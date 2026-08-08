;;; reka-test.el --- Functional tests for reka -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'reka)

;;; Capture state (dynamically bound fresh in each test)

(defvar reka-test-captured nil
  "List of captured outgoing requests: (INTERFACE REQUEST ARGS).")
(defvar reka-test-dirty-count 0
  "How many times `reka--mark-manage-dirty' was called.")
(defvar reka-test-handler-count 0
  "How many times the command handler was scheduled.")
(defvar reka-test--protocols-cache nil)

(defun reka-test--protocols ()
  (or reka-test--protocols-cache
      (setq reka-test--protocols-cache (reka--read-protocols))))

(defun reka-test-make-state ()
  "Build a fresh `reka-state' with no live connection."
  (reka-state-make
   :connection nil
   :objects (ewc-objects-make :protocols (reka-test--protocols))))

;;; Advice

(defun reka-test--adv-connect (_orig _objects &optional _socket)
  "Do not open a real Wayland socket."
  nil)

(defun reka-test--adv-request (_orig object request &optional arguments)
  "Capture outgoing requests instead of sending them."
  (push (list (ewc-object-interface object) request arguments)
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
         (reka--last-focused nil))
     (unwind-protect
         (progn
           (reka-test-install-advice)
           ,@body)
       (reka-test-remove-advice))))

(defun reka-test-find-request (request &optional interface)
  "Return first captured request named REQUEST, optionally on INTERFACE."
  (cl-find-if (lambda (r)
                (and (eq (nth 1 r) request)
                     (or (null interface) (eq (nth 0 r) interface))))
              reka-test-captured))


;;; Inbound-event simulation helpers

(defun reka-test-simulate-global (state interface-name &optional version)
  "Fake a wl_registry.global event advertising INTERFACE-NAME."
  (let* ((reg (cl-find-if
               (lambda (o) (eq (ewc-object-interface o) 'wl-registry))
               (hash-table-values (ewc-objects-table
                                   (reka-state-objects state)))))
         (listener (ewc-listener reg 'global)))
    (funcall listener reg `((name . 1)
                            (interface . ,interface-name)
                            (version . ,(or version 1))))))


;;; Tests

;; ---- The interception machinery itself ----------------------------
(ert-deftest reka-request-is-captured ()
  (reka-test-with-mock
    (let ((reka--state (reka-test-make-state)))
      (let ((obj (ewc-object-add
                  :objects (reka-state-objects reka--state)
                  :protocol 'river-window-management-v1
                  :interface 'river-window-v1)))
        (reka--request obj 'close)
        (should (= (length reka-test-captured) 1))
        (should (reka-test-find-request 'close 'river-window-v1))
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
      (reka--enqueue reka--state 'test-cmd (lambda () (setq ran t)))
      ;; Enqueue asked for a handler, but it was intercepted.
      (should (= reka-test-handler-count 1))
      (should (null ran))
      ;; Drain it by hand (t = don't defer the focus update).
      (reka--handle-commands reka--state t)
      (should ran)
      (should (null (reka-state-command-queue reka--state))))))

;; ---- Startup ------------------------------------------------------
(ert-deftest reka-start-wm-requests-registry ()
  (reka-test-with-mock
    (reka--start-wm)
    (should reka--state)
    (should (reka-test-find-request 'get-registry 'wl-display))))

;; ---- A small end-to-end flow: bind WM global, then an output event
(ert-deftest reka-output-event-marks-manage-dirty ()
  (reka-test-with-mock
    (reka--start-wm)
    ;; Compositor advertises the window-manager global.
    (reka-test-simulate-global reka--state "river_window_manager_v1")
    (let ((wm (reka-global reka--state 'river-window-manager-v1)))
      (should wm)
      (should (= reka-test-dirty-count 0))
      ;; Compositor announces an output.
      (funcall (ewc-listener wm 'output) wm '((id . 1)))
      (should (= reka-test-dirty-count 1))
      (should (reka--output-by-id reka--state 1)))))

;; ---- Reconciliation ------------------------------------------------
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
      (should (reka-test-find-request 'close 'river-window-v1)))))

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
      (let ((req (reka-test-find-request 'propose-dimensions)))
        (should req)
        (should (= (alist-get 'width  (nth 2 req)) 800))
        (should (= (alist-get 'height (nth 2 req)) 600))))))

(provide 'reka-tests-functional.el)

