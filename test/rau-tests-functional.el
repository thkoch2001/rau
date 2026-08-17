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
(defvar rau-test--protocols-cache nil)

(defun rau-test--protocols ()
  (or rau-test--protocols-cache
      (setq rau-test--protocols-cache (rau--read-protocols))))

(defun rau-test-make-state ()
  "Build a fresh `rau-state' with no live connection."
  (let ((client (ewc-client-make :protocols (rau-test--protocols))))
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

;;; Inbound-event simulation helpers

(defun rau-test-find-object (state interface)
  "Find first object of INTERFACE in STATE."
  (or (cl-find-if
               (lambda (o) (eq (ewc-object-interface o) interface))
               (hash-table-values (ewc-client-table
                                   (rau-state-client state))))
      (ert-fail (format "Did not find object of %s in state" interface))))

(defun rau-test-find-listener (state interface event)
  "Return cons (OBJECT . LISTENER) for listener of EVENT for first object
of interface INTERFACE in STATE."
  (if-let* ((object (rau-test-find-object state interface))
            (listener (ewc-listener object event)))
      (cons object listener)
    (ert-fail (format "Did not find listener for event %s in first object of interface %s." event interface))))

(defun rau-test-find-call-listener (state interface event &optional args)
  "Find EVENT listener of first object of INTERFACE in STATE and call
it with optional ARGS."
  (let* ((listener-cons (rau-test-find-listener state interface event))
         (object (car listener-cons))
         (listener (cdr listener-cons)))
    (funcall listener object args)))

(defun rau-test-call-listener (proxy event &optional args)
  "Call EVENT on PROXY with optional ARGS. PROXY can be an ewc-object or
its id in rau--state client table."
  (let ((proxy-obj (if (integerp proxy)
                       (ewc-object-get (rau-state-client rau--state) proxy)
                     proxy)))
    (funcall (should (ewc-listener proxy-obj event)) proxy-obj args)))

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
          (_ (rau-test-find-call-listener rau--state
                                          'wl-registry 'global
                                          '((name . 1)
                                            (interface . "river_window_manager_v1")
                                            (version . 1))))
          (client (rau-state-client rau--state))
          (wm-wl (ewc-first-object client 'river-window-manager-v1))
          (_ (rau-test-last-request-should 'wl-registry 'bind))
          (output-id (rau-test-server-object-id))
          (_ (rau-test-call-listener wm-wl 'output `((id . ,output-id))))
          (output-wl (should (car (ewc-objects client 'river-output-v1))))
          (_ (should (= rau-test-dirty-count 0)))
          (_ (should (length= (frame-list) 1)))
          (_ (rau-test-call-listener output-wl 'position '((x . 42) (y . 43))))
          (_ (rau-test-call-listener output-wl 'dimensions '((width . 44) (height . 45))))
          (_ (rau-test-call-listener wm-wl 'manage-start))
          (_ (rau-test-last-request-should 'river-window-manager-v1 'manage-finish))
          (_ (rau-test-call-listener wm-wl 'render-start))
          (_ (rau-test-last-request-should 'river-window-manager-v1 'render-finish))
          ;; emacs frame
          (win-id (rau-test-server-object-id))
          (_ (rau-test-call-listener wm-wl 'window `((id . ,win-id))))
          (_ (rau-test-call-listener win-id 'unreliable-pid `((unreliable-pid . ,(emacs-pid)))))
          (_ (rau-test-last-request-should 'river-window-v1 'get-node))
          (_ (should (length= (ewc-objects client rau--tag-frame) 1)))
          (_ (should (ewc-object-get client win-id)))
          (_ (rau-test-call-listener wm-wl 'manage-start))
          (_ (rau-test-last-request-should 'river-window-manager-v1 'manage-finish))
          (_ (should (rau-test-some-request 'river-window-v1 'set-tiled)))
          (_ (should (rau-test-some-request 'river-window-v1 'inform-maximized)))
          (_ (should (rau-test-some-request 'river-window-v1 'propose-dimensions '((width . 44) (height . 45)))))
          (_ (rau-test-call-listener wm-wl 'render-start))))))

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
