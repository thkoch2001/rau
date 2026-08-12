;;; reka-test.el --- Functional tests for reka -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'reka)

;;; Test debug helpers

(defun reka-test--print-captured ()
  (mapc (pcase-lambda ((seq id if rq args))
          (message "%d %s %s %S" id if rq args))
        reka-test-captured))

;;; Capture state (dynamically bound fresh in each test)

;; TODO define and use aliases reka-test-captured-id, -if, -rq, -args
(defvar reka-test-captured nil
  "List of captured outgoing requests: (OBJECT-ID INTERFACE REQUEST ARGS).")
(defvar reka-test-dirty-count 0
  "How many times `reka--mark-manage-dirty' was called.")
(defvar reka-test-handler-count 0
  "How many times the command handler was scheduled.")
(defvar reka-test-server-object-id nil
  "new_id for objects created by mocked river server.")
(defvar reka-test--protocols-cache nil)

(defun reka-test--protocols ()
  (or reka-test--protocols-cache
      (setq reka-test--protocols-cache (reka--read-protocols))))

(defun reka-test-make-state ()
  "Build a fresh `reka-state' with no live connection."
  (reka-state-make
   :connection nil
   :objects (ewc-objects-make :protocols (reka-test--protocols))))

(defun reka-test-server-object-id ()
  "Return and increment a server new_id."
  (cl-incf reka-test-server-object-id))

;;; Advice

(defun reka-test--adv-connect (_orig _objects &optional _socket)
  "Do not open a real Wayland socket."
  nil)

(defun reka-test--adv-request (_orig object request &optional arguments)
  "Capture outgoing requests instead of sending them."
  (push (list (ewc-object-id object) (ewc-object-interface object) request arguments)
        reka-test-captured))

(defun reka-test--adv-schedule-command-handler (_orig)
  (cl-incf reka-test-handler-count))

(defun reka-test--adv-mark-manage-dirty (_orig _state)
  (cl-incf reka-test-dirty-count))

(defun reka-test-install-advice ()
  (advice-add 'ewc-connect :around #'reka-test--adv-connect)
  (advice-add 'reka--request :around #'reka-test--adv-request)
  (advice-add 'reka--schedule-command-handler :around #'reka-test--adv-schedule-command-handler)
  (advice-add 'reka--mark-manage-dirty :around #'reka-test--adv-mark-manage-dirty))

(defun reka-test-remove-advice ()
  (advice-remove 'ewc-connect #'reka-test--adv-connect)
  (advice-remove 'reka--request #'reka-test--adv-request)
  (advice-remove 'reka--schedule-command-handler #'reka-test--adv-schedule-command-handler)
  (advice-remove 'reka--mark-manage-dirty #'reka-test--adv-mark-manage-dirty))

(defmacro reka-test-with-mock (&rest body)
  "Run BODY with reka's I/O intercepted and all globals isolated."
  (declare (indent 0) (debug t))
  `(let ((reka-test-captured nil)
         (reka-test-dirty-count 0)
         (reka-test-handler-count 0)
         (reka--state nil)
         (reka--manage-timer nil)
         (reka--command-timer nil)
         (reka--handling-commands nil)
         (reka--pending-handler nil)
         (reka--last-focused nil)
         (reka-test-server-object-id (- #xFF000000 1)))
     (unwind-protect
         (progn
           (reka-test-install-advice)
           ,@body)
       (reka-test-remove-advice))))

;;; Inbound-event simulation helpers

(defun reka-test-find-object (state interface)
  "Find first object of INTERFACE in STATE."
  (or (cl-find-if
               (lambda (o) (eq (ewc-object-interface o) interface))
               (hash-table-values (ewc-objects-table
                                   (reka-state-objects state))))
      (ert-fail (format "Did not find object of %s in state" interface))))

(defun reka-test-find-listener (state interface event)
  "Return cons (OBJECT . LISTENER) for listener of EVENT for first object
of interface INTERFACE in STATE."
  (if-let* ((object (reka-test-find-object state interface))
            (listener (ewc-listener object event)))
      (cons object listener)
    (ert-fail (format "Did not find listener for event %s in first object of interface %s." event interface))))

(defun reka-test-find-call-listener (state interface event &optional args)
  "Find EVENT listener of first object of INTERFACE in STATE and call
it with optional ARGS."
  (let* ((listener-cons (reka-test-find-listener state interface event))
         (object (car listener-cons))
         (listener (cdr listener-cons)))
    (funcall listener object args)))

(defun reka-test-call-listener (proxy event &optional args)
  "Call EVENT on PROXY with optional ARGS. PROXY can be an ewc-object or
its id in reka--state objects table."
  (let ((proxy-obj (if (integerp proxy)
                       (ewc-object-get proxy (reka-state-objects reka--state))
                     proxy)))
    (funcall (should (ewc-listener proxy-obj event)) proxy-obj args)))

;;; Request helpers

(defun reka-test-request-equal (captured interface request &optional args)
  (and (eq (cl-second captured) interface)
       (eq (cl-third captured) request)
       (if-let* ((args)
                 (captured-args (cl-fourth captured)))
           (seq-every-p
            (pcase-lambda (`(,k . ,v))
              (equal (cdr (assoc k captured-args)) v))
         args)
       t)
  ))

(defun reka-test-last-request-should (interface request &optional args)
  "Pop last request and compare with should."
  (let ((last (pop reka-test-captured)))
    (unless (reka-test-request-equal last interface request args)
      (ert-fail (format "Last request %S not equal %s %s (%S)." last interface request args)))))

(defun reka-test-some-request (interface request &optional args)
  (seq-some (lambda (captured) (reka-test-request-equal captured interface request args)) reka-test-captured))

;;; Tests

;; ---- The interception machinery itself
(ert-deftest reka-request-is-captured ()
  (reka-test-with-mock
    (let ((reka--state (reka-test-make-state)))
      (let ((obj (ewc-object-add
                  :objects (reka-state-objects reka--state)
                  :protocol 'river-window-management-v1
                  :interface 'river-window-v1)))
        (reka--request obj 'close)
        (should (length= reka-test-captured 1))
        (reka-test-last-request-should 'river-window-v1 'close)
        ;; No real connection was ever made.
        (should (null (reka-state-connection reka--state)))))))

(ert-deftest reka-manage-dirty-is-captured ()
  (reka-test-with-mock
    (let ((reka--state (reka-test-make-state)))
      (should (= reka-test-dirty-count 0))
      (reka--mark-manage-dirty reka--state)
      (reka--mark-manage-dirty reka--state)
      (should (= reka-test-dirty-count 2))
      ;; No real timer was scheduled.
      (should (null reka--manage-timer)))))

(ert-deftest reka-command-queue-drains-manually ()
  (reka-test-with-mock
    (let ((reka--state (reka-test-make-state))
          (ran nil))
      (reka--enqueue (lambda () (setq ran t)))
      ;; Enqueue asked for a handler, but it was intercepted.
      (should (= reka-test-handler-count 1))
      (should (null ran))
      ;; Drain it by hand (t = don't defer the focus update).
      (reka--handle-commands reka--state t)
      (should ran)
      (should (null (reka-state-command-queue reka--state))))))

;; ---- Startup
(ert-deftest reka-start-wm-requests-registry ()
  (reka-test-with-mock
    (reka--start-wm)
    (should reka--state)
    (reka-test-last-request-should 'wl-display 'get-registry)))

;; ---- A small end-to-end flow: bind WM global, then an output event
(ert-deftest reka-output-event-marks-manage-dirty ()
  (reka-test-with-mock
    (reka--start-wm)
    ;; Compositor advertises the window-manager global.
    (reka-test-find-call-listener reka--state
                             'wl-registry 'global
                             '((name . 1)
                               (interface . "river_window_manager_v1")
                               (version . 1)))

    (let ((wm (reka--global reka--state 'river-window-manager-v1)))
      (should wm)
      (should (= reka-test-dirty-count 0))
      ;; Compositor announces an output.
      (funcall (ewc-listener wm 'output) wm '((id . 1)))
      (should (= reka-test-dirty-count 0))
      (should (reka--output-by-id reka--state 1)))))

;; ---- Output
(ert-deftest reka-setup-one-output ()
  (reka-test-with-mock
   (reka--start-wm)
   (reka-test-find-call-listener reka--state
                             'wl-registry 'global
                             '((name . 1)
                               (interface . "river_window_manager_v1")
                               (version . 1)))
   (let* ((wm (reka--global reka--state 'river-window-manager-v1))
          (_ (reka-test-last-request-should 'wl-registry 'bind))
          (output-id (reka-test-server-object-id))
          (_ (reka-test-call-listener wm 'output `((id . ,output-id))))
          (output-struct (should (reka--output-by-id reka--state output-id)))
          (output-proxy (should (reka-output-proxy output-struct)))
          (_ (should (= reka-test-dirty-count 0)))
          (_ (should (length= (frame-list) 1)))
          (_ (reka-test-call-listener output-proxy 'position '((x . 42) (y . 43))))
          (_ (reka-test-call-listener output-proxy 'dimensions '((width . 44) (height . 45))))
          (_ (reka-test-call-listener wm 'manage-start))
          (_ (reka-test-last-request-should 'river-window-manager-v1 'manage-finish))
          (_ (reka-test-call-listener wm 'render-start))
          (_ (reka-test-last-request-should 'river-window-manager-v1 'render-finish))
          ;; emacs frame
          (win-id (reka-test-server-object-id))
          (_ (reka-test-call-listener wm 'window `((id . ,win-id))))
          (_ (reka-test-call-listener win-id 'unreliable-pid `((unreliable-pid . ,(emacs-pid)))))
          (_ (reka-test-last-request-should 'river-window-v1 'get-node))
          (_ (should (= (hash-table-count (reka-state-frames reka--state)) 1)))
          (frame-struct (should (gethash win-id (reka-state-frames reka--state))))
          (_ (reka-test-call-listener wm 'manage-start))
          (_ (reka-test-last-request-should 'river-window-manager-v1 'manage-finish))
          (_ (should (reka-test-some-request 'river-window-v1 'set-tiled)))
          (_ (should (reka-test-some-request 'river-window-v1 'inform-maximized)))
          (_ (should (reka-test-some-request 'river-window-v1 'propose-dimensions '((width . 44) (height . 45)))))
          (_ (reka-test-call-listener wm 'render-start))))))

;; ---- Reconciliation
(ert-deftest reka-reconcile-closes-killed-window ()
  (reka-test-with-mock
    (let* ((reka--state (reka-test-make-state))
           (proxy (ewc-object-add
                   :objects (reka-state-objects reka--state)
                   :protocol 'river-window-management-v1
                   :interface 'river-window-v1))
           (win (reka-window-make :proxy proxy :state 'killed)))
      (puthash (ewc-object-id proxy) win (reka-state-windows reka--state))
      (reka--reconcile-windows reka--state)
      (reka-test-last-request-should 'river-window-v1 'close))))

(ert-deftest reka-reconcile-proposes-dimensions ()
  (reka-test-with-mock
    (let* ((reka--state (reka-test-make-state))
           (proxy (ewc-object-add
                   :objects (reka-state-objects reka--state)
                   :protocol 'river-window-management-v1
                   :interface 'river-window-v1))
           (params (reka-window-parameters-make :x 0 :y 0 :w 800 :h 600))
           (win (reka-window-make :proxy proxy :state 'active :params params)))
      (puthash (ewc-object-id proxy) win (reka-state-windows reka--state))
      (reka--reconcile-windows reka--state)
      (reka-test-last-request-should 'river-window-v1 'propose-dimensions '((width . 800) (height . 600))))))

(provide 'reka-tests-functional.el)

