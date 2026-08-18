;;; rau-test.el --- Functional tests for rau -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'rau)

;;; Test debug helpers

(defun rau-test--print-captured ()
  (mapc (pcase-lambda ((seq id if rq args))
          (message "%d %s %s %S" id if rq args))
        rau-test-captured))

;;; Capture state (dynamically bound fresh in each test)

;; TODO define and use aliases rau-test-captured-id, -if, -rq, -args
(defvar rau-test-captured nil
  "List of captured outgoing requests: (OBJECT-ID INTERFACE REQUEST ARGS).")
(defvar rau-test-dirty-count 0
  "How many times `rau--mark-manage-dirty' was called.")
(defvar rau-test-handler-count 0
  "How many times the command handler was scheduled.")
(defvar rau-test-server-object-id nil
  "new_id for objects created by mocked river server.")
(defvar rau-test--interfaces-cache nil)

(defun rau-test--interfaces ()
  (or rau-test--interfaces-cache
      (setq rau-test--interfaces-cache (rau--read-protocols))))

(defun rau-test-make-state ()
  "Build a fresh `rau-state' with no live connection."
  (let ((client (ewc-client-make :interfaces (rau-test--interfaces))))
    (ewc-build-listeners client "rau-on-")
    (let ((display-wl (ewc-object-add client 'wl-display))
          (registry-wl (ewc-object-add client 'wl-registry)))

      )
    (rau-state-make :client client)))

(defun rau-test-server-object-id ()
  "Return and increment a server new_id."
  (cl-incf rau-test-server-object-id))

;;; Advice

(defun rau-test--adv-connect (_orig _client &optional _socket)
  "Do not open a real Wayland socket."
  nil)

(defun rau-test--adv-request (_orig object-wl request &optional arguments)
  "Capture outgoing requests instead of sending them."
  (push (list (ewc-object-id object-wl) (ewc-object-interface object-wl) request arguments)
        rau-test-captured))

(defun rau-test--adv-schedule-command-handler (_orig)
  (cl-incf rau-test-handler-count))

(defun rau-test--adv-mark-manage-dirty (_orig _state)
  (cl-incf rau-test-dirty-count))

(defun rau-test-install-advice ()
  (advice-add 'ewc-connect :around #'rau-test--adv-connect)
  (advice-add 'rau--request :around #'rau-test--adv-request)
  (advice-add 'rau--schedule-command-handler :around #'rau-test--adv-schedule-command-handler)
  (advice-add 'rau--mark-manage-dirty :around #'rau-test--adv-mark-manage-dirty))

(defun rau-test-remove-advice ()
  (advice-remove 'ewc-connect #'rau-test--adv-connect)
  (advice-remove 'rau--request #'rau-test--adv-request)
  (advice-remove 'rau--schedule-command-handler #'rau-test--adv-schedule-command-handler)
  (advice-remove 'rau--mark-manage-dirty #'rau-test--adv-mark-manage-dirty))

(defmacro rau-test-with-mock (&rest body)
  "Run BODY with rau's I/O intercepted and all globals isolated."
  (declare (indent 0) (debug t))
  `(let ((rau-test-captured nil)
         (rau-test-dirty-count 0)
         (rau-test-handler-count 0)
         (rau--state nil)
         (rau--manage-timer nil)
         (rau--command-timer nil)
         (rau--handling-commands nil)
         (rau--pending-handler nil)
         (rau--last-focused nil)
         (rau-test-server-object-id (- #xFF000000 1)))
     (unwind-protect
         (progn
           (rau-test-install-advice)
           ,@body)
       (rau-test-remove-advice))))

;;; Request helpers

(defun rau-test-request-equal (captured interface request &optional args)
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

(defun rau-test-last-request-should (interface request &optional args)
  "Pop last request and compare with should."
  (let ((last (pop rau-test-captured)))
    (unless (rau-test-request-equal last interface request args)
      (ert-fail (format "Last request %S not equal %s %s (%S)." last interface request args)))))

(defun rau-test-some-request (interface request &optional args)
  (seq-some (lambda (captured) (rau-test-request-equal captured interface request args)) rau-test-captured))

;;; Tests

;; ---- The interception machinery itself
(ert-deftest rau-request-is-captured ()
  (rau-test-with-mock
    (let ((rau--state (rau-test-make-state)))
      (let ((window-wl (ewc-object-add (rau-state-client rau--state) 'river-window-v1)))
        (rau--request window-wl 'close)
        (should (length= rau-test-captured 1))
        (rau-test-last-request-should 'river-window-v1 'close)
        ;; No real connection was ever made.
        (should (null (ewc-client-connection (rau-state-client rau--state))))))))

(ert-deftest rau-manage-dirty-is-captured ()
  (rau-test-with-mock
    (let ((rau--state (rau-test-make-state)))
      (should (= rau-test-dirty-count 0))
      (rau--mark-manage-dirty rau--state)
      (rau--mark-manage-dirty rau--state)
      (should (= rau-test-dirty-count 2))
      ;; No real timer was scheduled.
      (should (null rau--manage-timer)))))

(ert-deftest rau-command-queue-drains-manually ()
  (rau-test-with-mock
    (let ((rau--state (rau-test-make-state))
          (ran nil))
      (rau--enqueue (lambda () (setq ran t)))
      ;; Enqueue asked for a handler, but it was intercepted.
      (should (= rau-test-handler-count 1))
      (should (null ran))
      ;; Drain it by hand (t = don't defer the focus update).
      (rau--handle-commands rau--state t)
      (should ran)
      (should (null (rau-state-command-queue rau--state))))))

;; ---- Output
(ert-deftest rau-setup-one-output ()
  (rau-test-with-mock
   (let* ((rau--state (rau-test-make-state))
          (client (rau-state-client rau--state))
          (registry-wl (ewc-first-object client 'wl-registry))
          (_ (rau-on-wl-registry-global registry-wl
                                        '((name . 1)
                                          (interface . "river_window_manager_v1")
                                          (version . 1))))
          (wm-wl (ewc-first-object client 'river-window-manager-v1))
          (_ (rau-test-last-request-should 'wl-registry 'bind))
          (output-id (rau-test-server-object-id))
          (_ (rau-on-river-window-manager-v1-output wm-wl `((id . ,output-id))))
          (output-wl (should (car (ewc-objects client 'river-output-v1))))
          (_ (should (= rau-test-dirty-count 0)))
          (_ (should (length= (frame-list) 1)))
          (_ (rau-on-river-output-v1-position output-wl '((x . 42) (y . 43))))
          (_ (rau-on-river-output-v1-dimensions output-wl '((width . 44) (height . 45))))
          (_ (rau-on-river-window-manager-v1-manage-start wm-wl ()))
          (_ (rau-test-last-request-should 'river-window-manager-v1 'manage-finish))
          (_ (rau-on-river-window-manager-v1-render-start wm-wl ()))
          (_ (rau-test-last-request-should 'river-window-manager-v1 'render-finish))
          ;; emacs frame
          (win-id (rau-test-server-object-id))
          (_ (rau-on-river-window-manager-v1-window wm-wl `((id . ,win-id))))
          (window-wl (should (car (ewc-objects client 'river-window-v1))))
          (_ (rau-on-river-window-v1-unreliable-pid window-wl `((unreliable-pid . ,(emacs-pid)))))
          (_ (rau-test-last-request-should 'river-window-v1 'get-node))
          (_ (should (length= (ewc-objects client rau--tag-frame) 1)))
          (_ (should (ewc-object-get client win-id)))
          (_ (rau-on-river-window-manager-v1-manage-start wm-wl ()))
          (_ (rau-test-last-request-should 'river-window-manager-v1 'manage-finish))
          (_ (should (rau-test-some-request 'river-window-v1 'set-tiled)))
          (_ (should (rau-test-some-request 'river-window-v1 'inform-maximized)))
          (_ (should (rau-test-some-request 'river-window-v1 'propose-dimensions '((width . 44) (height . 45)))))
          (_ (rau-on-river-window-manager-v1-render-start wm-wl ()))))))

;; ---- Reconciliation
(ert-deftest rau-reconcile-closes-killed-window ()
  (rau-test-with-mock
   (let* ((rau--state (rau-test-make-state))
          (client (rau-state-client rau--state))
          (proxy (ewc-object-add (rau-state-client rau--state) 'river-window-v1))
          (win (rau-window-make :state 'killed)))
     (setf (ewc-object-data proxy) win)
     (ewc-object-tag client proxy rau--tag-window)
     (rau--reconcile-windows rau--state)
     (rau-test-last-request-should 'river-window-v1 'close))))

(ert-deftest rau-reconcile-proposes-dimensions ()
  (rau-test-with-mock
   (let* ((rau--state (rau-test-make-state))
          (client (rau-state-client rau--state))
          (window-wl (ewc-object-add (rau-state-client rau--state) 'river-window-v1))
          (params (rau-window-parameters-make :x 0 :y 0 :w 800 :h 600))
          (win (rau-window-make :state 'active :params params)))
     (setf (ewc-object-data window-wl) win)
     (ewc-object-tag client window-wl rau--tag-window)
     (rau--reconcile-windows rau--state)
     (rau-test-last-request-should 'river-window-v1 'propose-dimensions '((width . 800) (height . 600))))))

(provide 'rau-tests-functional.el)
