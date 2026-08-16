;;; reka-tests.el -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'reka)
(require 'reka-test-helpers)

;;(setq ert-batch-backtrace-right-margin 240)

;; ---- fixtures -----------------------------------------------------------

(defun reka-test--fake-reka-buffer (win-obj)
  "Make a buffer that `reka--is-reka-buffer' accepts, without `reka-mode' side effects."
  (let ((buf (generate-new-buffer " *reka-test*")))
    (with-current-buffer buf
      ;; Set major-mode directly so we don't run reka-mode's body
      ;; (scroll-bar-mode etc.), which can misbehave in -batch.
      (setq major-mode 'reka-mode)
      (setq-local reka--window-wl win-obj))
    buf))

(defun reka-test--make-state (&key with-display)
  "Build a reka-state with one frame and one window.
If WITH-DISPLAY, the window has params pointing at that frame.
Return (STATE FRAME-OBJ WIN-OBJ WIN)."
  (let* ((client (ewc-client-make))
         (state (reka-state-make :client client))
         (frame-obj (ewc-object-make :id 100 :interface 'river-window-v1))
         (frame (reka-frame-make :title "reka-frame-test"))
         (win-obj (ewc-object-make :id 200 :interface 'river-window-v1))
         ;; TODO: consider storing ewc frame obj in reka-window-parameters and
         ;; just return it from reka--frame-displaying-win. That however
         ;; requires the title event to have happened before
         ;; reka--handle-commands.
         (emacs-frame (selected-frame))
         (params (when with-display
                   (reka-window-parameters-make
                    :emacs-frame emacs-frame
                    :x 0 :y 0 :w 800 :h 600)))
         (win (reka-window-make :params params)))
    (set-frame-parameter emacs-frame 'reka-frame-wl frame-obj)
    (setf (ewc-object-data frame-obj) frame)
    (setf (ewc-object-data win-obj) win)
    (ewc-object-tag client frame-obj reka--tag-frame)
    (ewc-object-tag client win-obj reka--tag-window)
    (list state frame-obj win-obj win)))

;; ---- Pure functions ---------------------------
(ert-deftest reka-resolve-keysym ()
  (should (= (reka--resolve-keysym "return") #xFF0D))
  (should (= (reka--resolve-keysym 'return)  #xFF0D))
  (should (= (reka--resolve-keysym "a")      ?a))
  (should (= (reka--resolve-keysym ?a)       ?a))
  (should (= (reka--resolve-keysym "nosuchkey") 0)))

(ert-deftest reka-key-to-xkb ()
  (pcase-let ((`(,_event ,key ,mods) (reka--key-to-xkb "C-x")))
    (should (= key ?x))
    (should (= mods 4)))                     ; control
  (pcase-let ((`(,_event ,key ,mods) (reka--key-to-xkb "M-x")))
    (should (= key ?x))
    (should (= mods 8)))                     ; meta
  (pcase-let ((`(,_event ,key ,mods) (reka--key-to-xkb "C-S-x")))
    (should (= key ?x))
    (should (= mods 5))))                    ; control(4) + shift(1)

(ert-deftest reka-make-buffer-name ()
  (should (equal (reka--make-buffer-name "firefox" "Mozilla")
                 "Mozilla - firefox"))
  (should (equal (reka--make-buffer-name nil "Mozilla") "Mozilla"))
  (should (string-suffix-p "…"
                           (reka--make-buffer-name nil (make-string 50 ?a)))))

;; ---- focus state machine (pure, no fixtures needed) ---------------------

(ert-deftest reka-focus-state-machine ()
  (let ((state (reka-state-make))
        (w (ewc-object-make :id 1))
        (f (ewc-object-make :id 2)))
    (should (reka--focus-window state w f))
    (should (eq 'window (reka-state-focus-state state)))
    (should (equal (cons w f) (reka--focus-current state)))
    (should-not (reka--focus-window state w f)) ; second call is a no-op
    (should (reka--focus-switch-to-frame state))
    (should (eq 'frame (reka-state-focus-state state)))
    (should (equal (cons f f) (reka--focus-current state)))
    (should-not (reka--focus-switch-to-frame state))))

(ert-deftest reka-focus-invalidate ()
  (let ((state (reka-state-make))
        (w (ewc-object-make :id 1))
        (f (ewc-object-make :id 2)))
    (reka--focus-window state w f)
    (reka--focus-invalidate state w)            ; focused window closed
    (should (eq 'frame (reka-state-focus-state state)))
    (should (null (reka-state-focused-window state)))
    (reka--focus-invalidate state f)            ; focused frame closed
    (should (eq 'lost (reka-state-focus-state state)))))

;; ---- reka--update-focus-for-window (the regression tests) --------------

(ert-deftest reka-focus-request-focuses-displayed-reka-buffer ()
  (pcase-let* ((`(,state ,_fobj ,win-obj ,_win) (reka-test--make-state :with-display t))
               (reka--state state)
               (reka--last-focused nil)
               (buf (reka-test--fake-reka-buffer win-obj)))
    (unwind-protect
        (progn
          (should (eq t (reka--update-focus-for-window state win-obj)))
          (should (eq 'window (reka-state-focus-state state)))
          (should (eq win-obj (reka-state-focused-window state))))
      (kill-buffer buf))))

(ert-deftest reka-focus-request-defers-when-not-displayed ()
  "when params aren't ready yet, do NOT set reka--last-focused,
so a later hook run can retry."
  (pcase-let* ((`(,state ,_fobj ,win-obj ,_win) (reka-test--make-state :with-display nil))
               (reka--state state)
               (reka--last-focused nil)
               (buf (reka-test--fake-reka-buffer win-obj)))
    (unwind-protect
        (progn
          (should (null (reka--update-focus-for-window state win-obj)))
          (should (eq 'lost (reka-state-focus-state state))))
      (kill-buffer buf))))

(ert-deftest reka-focus-request-returns-focus-to-frame-for-normal-buffer ()
  (pcase-let* ((`(,state ,frame-obj ,win-obj ,_win) (reka-test--make-state :with-display t))
               (reka--state state)
               (reka--last-focused nil)
               (buf (generate-new-buffer " *normal*"))
               (manage-calls nil))
    (setf (reka-state-focus-state state) 'window
          (reka-state-focused-window state) win-obj
          (reka-state-focused-frame state) frame-obj)
    (let ((prev (window-buffer (selected-window)))
          (reka--manage-timer nil))
      (unwind-protect
          (progn
            (set-window-buffer (selected-window) buf)
            (should (eq t (reka--update-focus-request)))
            (should (eq 'frame (reka-state-focus-state state))))
        (set-window-buffer (selected-window) prev)
        (kill-buffer buf)))))
