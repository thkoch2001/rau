;;; rau-tests.el -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'rau)
(require 'rau-test-helpers)

;;(setq ert-batch-backtrace-right-margin 240)

;; ---- fixtures -----------------------------------------------------------

(defun rau-test--fake-rau-buffer (window-wl)
  "Make a buffer that `rau--is-rau-buffer' accepts, without `rau-mode' side effects."
  (let ((buf (generate-new-buffer " *rau-test*")))
    (with-current-buffer buf
      ;; Set major-mode directly so we don't run rau-mode's body
      ;; (scroll-bar-mode etc.), which can misbehave in -batch.
      (setq major-mode 'rau-mode)
      (setq-local rau--window-wl window-wl))
    buf))

(defun rau-test--make-state (&key with-display)
  "Build a rau-state with one frame and one window.
If WITH-DISPLAY, the window has params pointing at that frame.
Return (STATE FRAME-WL WINDOW-WL WIN)."
  (let* ((client (ewc-client-make))
         (state (rau-state-make :client client))
         (frame-wl (ewc-object-make :id 100 :interface 'river-window-v1))
         (frame (rau-frame-make :title "rau-frame-test"))
         ;; TODO: consider storing ewc frame obj in rau-window-parameters and
         ;; just return it from rau--frame-displaying-win. That however
         ;; requires the title event to have happened before
         ;; rau--handle-commands.
         (window-wl (ewc-object-make :id 200 :interface 'river-window-v1))
         (emacs-frame (selected-frame))
         (params (when with-display
                   (rau-window-parameters-make
                    :emacs-frame emacs-frame
                    :x 0 :y 0 :w 800 :h 600)))
         (win (rau-window-make :params params)))
    (set-frame-parameter emacs-frame 'rau-frame-wl frame-wl)
    (setf (ewc-object-data frame-wl) frame)
    (setf (ewc-object-data window-wl) win)
    (ewc-object-tag client frame-wl rau--tag-frame)
    (ewc-object-tag client window-wl rau--tag-window)
    (list state frame-wl window-wl win)))

;; ---- Pure functions ---------------------------
(ert-deftest rau-resolve-keysym ()
  (should (= (rau--resolve-keysym "return") #xFF0D))
  (should (= (rau--resolve-keysym 'return)  #xFF0D))
  (should (= (rau--resolve-keysym "a")      ?a))
  (should (= (rau--resolve-keysym ?a)       ?a))
  (should (= (rau--resolve-keysym "nosuchkey") 0)))

(ert-deftest rau-key-to-xkb ()
  (pcase-let ((`(,_event ,key ,mods) (rau--key-to-xkb "C-x")))
    (should (= key ?x))
    (should (= mods 4)))                     ; control
  (pcase-let ((`(,_event ,key ,mods) (rau--key-to-xkb "M-x")))
    (should (= key ?x))
    (should (= mods 8)))                     ; meta
  (pcase-let ((`(,_event ,key ,mods) (rau--key-to-xkb "C-S-x")))
    (should (= key ?x))
    (should (= mods 5))))                    ; control(4) + shift(1)

(ert-deftest rau-make-buffer-name ()
  (should (equal (rau--make-buffer-name "firefox" "Mozilla")
                 "Mozilla - firefox"))
  (should (equal (rau--make-buffer-name nil "Mozilla") "Mozilla"))
  (should (string-suffix-p "…"
                           (rau--make-buffer-name nil (make-string 50 ?a)))))

;; ---- focus state machine (pure, no fixtures needed) ---------------------

(ert-deftest rau-focus-state-machine ()
  (let ((state (rau-state-make))
        (window-wl (ewc-object-make :id 1))
        (frame-wl (ewc-object-make :id 2)))
    (should (rau--focus-window state window-wl frame-wl))
    (should (eq 'window (rau-state-focus-state state)))
    (should (equal (cons window-wl frame-wl) (rau--focus-current state)))
    (should-not (rau--focus-window state window-wl frame-wl)) ; second call is a no-op
    (should (rau--focus-switch-to-frame state))
    (should (eq 'frame (rau-state-focus-state state)))
    (should (equal (cons frame-wl frame-wl) (rau--focus-current state)))
    (should-not (rau--focus-switch-to-frame state))))

(ert-deftest rau-focus-invalidate ()
  (let ((state (rau-state-make))
        (window-wl (ewc-object-make :id 1))
        (frame-wl (ewc-object-make :id 2)))
    (rau--focus-window state window-wl frame-wl)
    (rau--focus-invalidate state window-wl)    ; focused window closed
    (should (eq 'frame (rau-state-focus-state state)))
    (should (null (rau-state-focused-window state)))
    (rau--focus-invalidate state frame-wl)     ; focused frame closed
    (should (eq 'lost (rau-state-focus-state state)))))

;; ---- rau--update-focus-for-window (the regression tests) --------------

(ert-deftest rau-focus-request-focuses-displayed-rau-buffer ()
  (pcase-let* ((`(,state ,_frame-wl ,window-wl ,_win) (rau-test--make-state :with-display t))
               (rau--state state)
               (rau--last-focused nil)
               (buf (rau-test--fake-rau-buffer window-wl)))
    (unwind-protect
        (progn
          (should (eq t (rau--update-focus-for-window state window-wl)))
          (should (eq 'window (rau-state-focus-state state)))
          (should (eq window-wl (rau-state-focused-window state))))
      (kill-buffer buf))))

(ert-deftest rau-focus-request-defers-when-not-displayed ()
  "when params aren't ready yet, do NOT set rau--last-focused,
so a later hook run can retry."
  (pcase-let* ((`(,state ,_frame-wl ,window-wl ,_win) (rau-test--make-state :with-display nil))
               (rau--state state)
               (rau--last-focused nil)
               (buf (rau-test--fake-rau-buffer window-wl)))
    (unwind-protect
        (progn
          (should (null (rau--update-focus-for-window state window-wl)))
          (should (eq 'lost (rau-state-focus-state state))))
      (kill-buffer buf))))

(ert-deftest rau-focus-request-returns-focus-to-frame-for-normal-buffer ()
  (pcase-let* ((`(,state ,frame-wl ,window-wl ,_win) (rau-test--make-state :with-display t))
               (rau--state state)
               (rau--last-focused nil)
               (buf (generate-new-buffer " *normal*"))
               (manage-calls nil))
    (setf (rau-state-focus-state state) 'window
          (rau-state-focused-window state) window-wl
          (rau-state-focused-frame state) frame-wl)
    (let ((prev (window-buffer (selected-window)))
          (rau--manage-timer nil))
      (unwind-protect
          (progn
            (set-window-buffer (selected-window) buf)
            (should (eq t (rau--update-focus-request)))
            (should (eq 'frame (rau-state-focus-state state))))
        (set-window-buffer (selected-window) prev)
        (kill-buffer buf)))))
