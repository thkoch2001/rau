;;; rau-placement-tests.el --- Tiled placement regressions -*- lexical-binding: t; -*-

;;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'ert)
(require 'cl-lib)
(require 'rau-be)

(defmacro rau-placement--with-backend (&rest body)
  "Run BODY with protocol objects but no compositor, timers or frontend."
  (declare (indent 0))
  `(let* ((client (ewc-client-make :interfaces (rau--read-protocols)))
          (rau--state (rau--state-make :client client))
          (ewc--lgr rau--lgr)
          requests
          (dirty-count 0))
     (cl-letf (((symbol-function 'rau--request)
                (lambda (object request &optional args)
                  (push (list (ewc-object-id object) request args) requests)))
               ((symbol-function 'rau--mark-manage-dirty)
                (lambda () (cl-incf dirty-count))))
       ,@body)))

(defun rau-placement--object (interface id data &rest tags)
  (let* ((client (rau--state-client rau--state))
         (object (ewc-object-add client interface id)))
    (setf (ewc-object-data object) data)
    (dolist (tag tags) (ewc-object-tag client object tag))
    object))

(defun rau-placement--frame (window-id frame-id output-id)
  (rau-placement--object 'river-output-v1 output-id
                         (rau--output-make :frame-id frame-id
                                           :dimensions '(1000 . 800)))
  (rau-placement--object
   'river-window-v1 window-id
   (rau--window-make
    :frame-id frame-id
    :node-wl (rau-placement--object 'river-node-v1 (+ window-id 1000) nil))
   rau--tag-outputframe))

(defun rau-placement--external (id)
  (rau-placement--object
   'river-window-v1 id
   (rau--window-make
    :node-wl (rau-placement--object 'river-node-v1 (+ id 1000) nil))
   rau--tag-external rau--tag-tiled))

(defun rau-placement--render ()
  (rau--on-river-window-manager-v1-render-start
   (ewc-object-make :interface 'river-window-manager-v1 :id 1) nil))

(ert-deftest rau-placement-window-update-tracks-geometry-changes ()
  "Report frame/position/size changes, but enqueue resizing only for a new size."
  (dolist (case '((2 (15 . 43) (800 . 700) nil nil)
                  (3 (15 . 43) (800 . 700) t nil)
                  (2 (30 . 60) (800 . 700) t nil)
                  (2 (15 . 43) (640 . 480) t t)))
    (pcase-let ((`(,frame-id ,position ,dimensions ,changed ,resized) case))
      (ert-info ((format "Geometry update %S" case))
        (rau-placement--with-backend
          (let ((external (rau-placement--external 300)))
            (setf (rau--window-wl-frame-id external) 2
                  (rau--window-wl-position external) '(15 . 43)
                  (rau--window-wl-dimensions external) '(800 . 700))
            (should (eq changed
                        (rau--update-window-tiled external frame-id position dimensions)))
            (should (equal (list (rau--window-wl-frame-id external)
                                 (rau--window-wl-position external)
                                 (rau--window-wl-dimensions external))
                           (list frame-id position dimensions)))
            (should (equal (rau--state-manage-queue rau--state)
                           (and resized
                                `((,external propose-dimensions
                                             ((width . 640) (height . 480)))))))
            (should (equal (rau--state-render-queue rau--state)
                           (and resized
                                `((,external set-clip-box
                                             ((x . 0) (y . 0)
                                              (width . 640) (height . 480)))))))
            (should (= dirty-count (if resized 1 0)))
            (should-not requests)))))))

(ert-deftest rau-placement-frame-assignment-alone-schedules-management ()
  (rau-placement--with-backend
    (rau-placement--frame 101 nil 100)
    (rau--update-fe-ui-state '[([2 101 100]) nil])
    (should (> dirty-count 0))
    (setq dirty-count 0)
    (rau--update-fe-ui-state '[([2 101 100]) nil])
    (should (zerop dirty-count))))

(ert-deftest rau-placement-stale-frame-object-types-do-not-poison-associations ()
  (rau-placement--with-backend
    (let* ((current (rau-placement--frame 201 3 200))
           (reused (rau-placement--external 100))
           (old-frame (rau-placement--frame 101 2 102)))
      ;; Old output ID now names a window; old window ID now names an output.
      (rau--update-fe-ui-state '[([2 101 100] [9 102 nil] [3 201 200]) nil])
      (should (= 3 (rau--output-wl-frame-id (ewc-object-get client 200))))
      (should (= 3 (rau--window-wl-frame-id current)))
      (should (= 2 (rau--window-wl-frame-id old-frame)))
      (should (= 2 (rau--output-wl-frame-id (ewc-object-get client 102))))
      (should-not (rau--window-wl-frame-id reused)))))

(ert-deftest rau-placement-same-geometry-new-frame-restacks ()
  (rau-placement--with-backend
    (rau-placement--frame 101 2 100)
    (rau-placement--frame 201 3 200)
    (rau-placement--external 300)
    (rau--update-fe-ui-state
     '[([2 101 100] [3 201 200]) ([300 2 (15 . 43) (800 . 700)])])
    (rau-placement--render)
    (setq requests nil dirty-count 0)
    (rau--update-fe-ui-state
     '[([2 101 100] [3 201 200]) ([300 3 (15 . 43) (800 . 700)])])
    (should (> dirty-count 0))
    (rau-placement--render)
    (should (member '(1300 place-above ((other . 1201))) requests))
    (should-not (member '(300 show nil) requests))
    ;; Stable state must not send redundant placement requests.
    (setq requests nil)
    (rau-placement--render)
    (should-not (seq-find (lambda (r) (memq (cadr r) '(set-position place-above)))
                          requests))))

(ert-deftest rau-placement-same-geometry-new-output-repositions ()
  "Regress issue #11: transfer between outputs without changing local geometry."
  (rau-placement--with-backend
    (rau-placement--frame 101 2 100)
    (rau-placement--frame 201 3 200)
    (rau--on-river-output-v1-position (ewc-object-get client 100)
                                    '((x . -1000) (y . 0)))
    (rau--on-river-output-v1-position (ewc-object-get client 200)
                                    '((x . 0) (y . 0)))
    (let ((external (rau-placement--external 300)))
      (rau--update-fe-ui-state
       '[([2 101 100] [3 201 200]) ([300 2 (15 . 43) (800 . 700)])])
      (rau-placement--render)
      (should (member '(1300 set-position ((x . -985) (y . 43))) requests))
      (setq requests nil dirty-count 0)
      (rau--update-fe-ui-state
       '[([2 101 100] [3 201 200]) ([300 3 (15 . 43) (800 . 700)])])
      (should (> dirty-count 0))
      (rau-placement--render)
      (should (member '(1300 set-position ((x . 15) (y . 43))) requests))
      (should (member '(1300 place-above ((other . 1201))) requests))
      (should (equal '(15 . 43) (rau--window-wl-absolute-position external))))))

(ert-deftest rau-placement-output-move-repositions-without-frontend-change ()
  (rau-placement--with-backend
    (rau-placement--frame 101 2 100)
    (let ((external (rau-placement--external 300)))
      (rau--update-window-tiled external 2 '(15 . 43) '(800 . 700))
      (rau-placement--render)
      (setq requests nil)
      (rau--on-river-output-v1-position (ewc-object-get client 100)
                                      '((x . 1000) (y . 20)))
      (rau-placement--render)
      (should (member '(1300 set-position ((x . 1015) (y . 63))) requests))
      (should (equal '(1015 . 63) (rau--window-wl-absolute-position external))))))

(ert-deftest rau-placement-hiding-and-redisplaying-same-layout-shows-again ()
  (rau-placement--with-backend
    (let* ((frame (rau-placement--frame 101 2 100))
           (external (rau-placement--external 300))
           (state '[([2 101 100]) ([300 2 (15 . 43) (800 . 700)])]))
      (rau--update-fe-ui-state state)
      (rau-placement--render)
      (should (eq (rau--window-wl-node-wl frame)
                  (rau--window-wl-placed-frame-node-wl external)))
      (rau--update-fe-ui-state '[([2 101 100]) nil])
      ;; Retained coordinates alone must not suppress show on redisplay.
      (should-not (rau--window-wl-placed-frame-node-wl external))
      (should (equal '(15 . 43) (rau--window-wl-absolute-position external)))
      (rau-placement--render)
      (should (member '(300 hide nil) requests))
      (setq requests nil)
      (rau--update-fe-ui-state state)
      (should-not (rau--window-wl-placed-frame-node-wl external))
      (rau-placement--render)
      (should (member '(300 show nil) requests))
      (should (eq (rau--window-wl-node-wl frame)
                  (rau--window-wl-placed-frame-node-wl external))))))

(ert-deftest rau-placement-late-frame-link-schedules-render ()
  (rau-placement--with-backend
    (rau-placement--external 300)
    (rau-placement--object 'river-output-v1 100 (rau--output-make))
    (rau--update-fe-ui-state '[([2 nil 100]) ([300 2 (15 . 43) (800 . 700)])])
    (rau-placement--render)
    (should-not (member '(300 show nil) requests))
    (rau-placement--object
     'river-window-v1 101
     (rau--window-make :node-wl (rau-placement--object 'river-node-v1 1101 nil))
     rau--tag-outputframe)
    (setq requests nil dirty-count 0)
    (rau--update-fe-ui-state '[([2 101 100]) ([300 2 (15 . 43) (800 . 700)])])
    ;; A forced render alone would mask a missing manage-dirty request.
    (should (> dirty-count 0))
    (rau-placement--render)
    (should (member '(300 show nil) requests))
    (should (member '(1300 place-above ((other . 1101))) requests))))

(ert-deftest rau-placement-late-output-link-schedules-render ()
  (rau-placement--with-backend
    (rau-placement--external 300)
    (rau-placement--object
     'river-window-v1 101
     (rau--window-make
      :frame-id 2 :node-wl (rau-placement--object 'river-node-v1 1101 nil))
     rau--tag-outputframe)
    (rau--update-fe-ui-state '[([2 101 nil]) ([300 2 (15 . 43) (800 . 700)])])
    (rau-placement--render)
    (should-not (member '(300 show nil) requests))
    (rau-placement--object 'river-output-v1 100 (rau--output-make))
    ;; The compositor's output event may be rendered before the async link.
    (rau-placement--render)
    (setq requests nil dirty-count 0)
    (rau--update-fe-ui-state '[([2 101 100]) ([300 2 (15 . 43) (800 . 700)])])
    (should (> dirty-count 0))
    (rau-placement--render)
    (should (member '(300 show nil) requests))
    (should (member '(1300 place-above ((other . 1101))) requests))))

(ert-deftest rau-placement-uses-latest-output-at-render ()
  (rau-placement--with-backend
    (rau-placement--frame 101 2 100)
    (let ((external (rau-placement--external 300)))
      (rau--update-window-tiled external 2 '(15 . 43) '(800 . 700))
      (should-not requests)
      (rau--on-river-output-v1-position (ewc-object-get client 100)
                                      '((x . -1000) (y . -20)))
      (rau-placement--render)
      (should (member '(1300 set-position ((x . -985) (y . 23))) requests))
      (should (member '(1 render-finish nil) requests)))))

(ert-deftest rau-placement-hide-show-coalesced-before-render ()
  (rau-placement--with-backend
    (rau-placement--frame 101 2 100)
    (rau-placement--external 300)
    (let ((state '[([2 101 100]) ([300 2 (15 . 43) (800 . 700)])]))
      (rau--update-fe-ui-state state)
      (rau-placement--render)
      (setq requests nil)
      (rau--update-fe-ui-state '[([2 101 100]) nil])
      (rau--update-fe-ui-state state)
      (rau-placement--render)
      (let* ((ordered (reverse requests))
             (hide (cl-position '(300 hide nil) ordered :test #'equal))
             (show (cl-position '(300 show nil) ordered :test #'equal)))
        (should hide)
        (should show)
        (should (< hide show))
        (should (= 1 (cl-count '(300 show nil) ordered :test #'equal)))))))

(ert-deftest rau-placement-recycled-node-id-is-not-same-node ()
  (rau-placement--with-backend
    (let* ((frame (rau-placement--frame 101 2 100))
           (external (rau-placement--external 300))
           (old-node (rau--window-wl-node-wl frame)))
      (rau--update-window-tiled external 2 '(15 . 43) '(800 . 700))
      (rau-placement--render)
      (setq requests nil)
      (rau--remove old-node)
      (let ((new-node (rau-placement--object 'river-node-v1 1101 nil)))
        (setf (rau--window-wl-node-wl frame) new-node)
        (rau-placement--render)
        (should (member '(1300 place-above ((other . 1101))) requests))
        (should (eq new-node (rau--window-wl-placed-frame-node-wl external)))))))

(ert-deftest rau-placement-registered-windows-have-nodes ()
  (dolist (tag (list rau--tag-outputframe rau--tag-tiled))
    (ert-info ((format "Window tag %S" tag))
      (rau-placement--with-backend
        (let ((wm (ewc-object-add client 'river-window-manager-v1)))
          (cl-letf (((symbol-function 'rau--fe) #'ignore))
            (rau--on-river-window-manager-v1-window wm '((id . 300)))
            (let* ((window (ewc-object-get client 300))
                   (node (rau--window-wl-node-wl window))
                   (node-id (ewc-object-id node)))
              (should (eq (ewc-object-interface node) 'river-node-v1))
              (should (eq node (ewc-object-get client node-id)))
              (should (equal requests `((300 get-node ((id . ,node-id))))))
              (ewc-object-tag client window tag)
              (should (memq window (ewc-objects client tag)))
              (rau--on-river-window-v1-closed window nil)
              (should (member `(,node-id destroy nil) requests))
              (should (member '(300 destroy nil) requests))
              (should-not (ewc-object-get client 300))
              (should-not (ewc-objects client tag))
              (setq requests nil)
              (rau--on-river-window-manager-v1-render-start wm nil)
              (should (equal requests '((1 render-finish nil)))))))))))

(ert-deftest rau-placement-layer-shell-area-moves ()
  (rau-placement--with-backend
    (rau-placement--frame 101 2 100)
    (let* ((output (ewc-object-get client 100))
           (layer (rau-placement--object 'river-layer-shell-output-v1 500
                                        (rau--ls-output-make :output-wl output)))
           (external (rau-placement--external 300)))
      (setf (rau--output-wl-ls-output-wl output) layer)
      (rau--update-window-tiled external 2 '(15 . 43) '(800 . 700))
      (rau-placement--render)
      (setq requests nil)
      (rau--on-river-layer-shell-output-v1-non-exclusive-area
       layer '((x . 100) (y . 30) (width . 900) (height . 770)))
      (rau-placement--render)
      (should (member '(1300 set-position ((x . 115) (y . 73))) requests)))))

;;; rau-placement-tests.el ends here
