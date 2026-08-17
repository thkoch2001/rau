;;; rau.el --- Wayland Window Manager based on River -*- lexical-binding: t -*-

;; Copyright (C) 2026 Thomas Koch

;; Author: Thomas Koch <thomas@koch.ro>
;; Version: 0.1
;; Keywords: frames
;; URL: https://github.com/thkoch2001/rau
;; Package-Requires: ((emacs "30.2"))

;;; Commentary:
;; Rau is a Wayland Window Manager based on Emacs and River in pure Elisp. The
;; heavy lifting is provided by River, written in Zig and thus fast while the
;; opinionated parts like Window placement, Keybindings and Focus management
;; are written in Elisp and are thus easy to customize.

;;; History:
;; This package is based on <https://codeberg.org/tazjin/reka>. The major
;; difference is that reka uses Rust code to bind to libwayland while rau uses
;; the ewc.el library of Michael Bauer to implement wayland communication.

;;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'cl-lib)
(require 'ewc)
(require 'map)          ; needed for `map' pcase pattern
(require 'pcase)
(require 'seq)
(require 'subr-x)

(defgroup rau nil
  "Rau - Emacs swimming in the river"
  :group 'environment
  :prefix "rau-")

(defcustom rau-enable-hook nil
  "Hook run at the end of `rau-enable'."
  :type 'hook)

(defvar rau-debug nil
  "When non-nil, enable verbose rau debugging messages.")

(defvar rau--state nil
  "Current global rau WM state.")


;;; State structs

(cl-defstruct (rau-fs (:constructor rau-fs) (:copier nil))
  "Fullscreen state machine for one output.
STATE is one of: `none', `requested', `fullscreen', `exiting'.
NEW and PREVIOUS are meaningful only when STATE is `requested'.
WINDOW is meaningful when STATE is `fullscreen' or `exiting'."
  (state    'none :type symbol :read-only t)
  (new      nil   :type ewc-object :read-only t)
  (previous nil   :type ewc-object :read-only t)
  (window   nil   :type ewc-object :read-only t))

(cl-defstruct (rau-window-parameters
               (:constructor rau-window-parameters-make))
  "Window parameters describing where an external surface should be placed."
  emacs-frame
  x
  y
  w
  h)

(cl-defstruct (rau-output (:constructor rau-output-make))
  "State for a River output."
  (ls-output-wl nil :type ewc-object)
  (x 0)
  (y 0)
  (width 0)
  (height 0)
  (fullscreen (rau-fs)))

(cl-defstruct (rau-surface (:constructor nil))
  "Common state shared by windows and frames.
Not instantiated directly; windows and frames include it."
  (node-wl nil :type ewc-object)
  title)

(cl-defstruct (rau-window (:constructor rau-window-make)
                           (:include rau-surface))
  "State for a regular external window."
  (state 'starting) ;; 'active 'killed
  params
  actual-width
  actual-height
  app-id)

(cl-defstruct (rau-frame (:constructor rau-frame-make)
                          (:include rau-surface))
  "State for an Emacs frame managed by rau."
  emacs-frame
  (output-wl nil :type ewc-object :documentation "displaying this frame")
  proposed-width
  proposed-height
  visible
  last-x
  last-y)

(cl-defstruct (rau-seat (:constructor rau-seat-make))
  "State for a River seat."
  (ls-seat-wl nil :type ewc-object))

(cl-defstruct (rau-binding (:constructor rau-binding-make))
  "State for one global XKB binding."
  keysym
  modifiers
  command
  event
  (state 'requested))

(cl-defstruct (rau-state (:constructor rau-state-make))
  "Holds the state of the rau Wayland client."
  (connection nil)
  (client nil :type ewc-client)
  (pid (emacs-pid))

  ;; XKB bindings: (keysym . modifiers) -> rau-binding.
  (bindings (make-hash-table :test 'equal))

  ;;; Focus tracking.
  ;; one of 'lost 'window 'frame
  (focus-state 'lost :type symbol)
  (focused-window nil :type ewc-object)
  (focused-frame nil :type ewc-object)
  ;; set only by window_interaction event
  (focus-dirty nil)

  ;; Frame request accounting.
  (pending-frames 0)

  ;; Lambda command queue: list of LAMBDA.
  (command-queue nil))


;;; Surface tags

(defconst rau--tag-frame :rau-frame
  "Tag for `river-window-v1' objects that are Emacs frames.")

(defconst rau--tag-window :rau-window
  "Tag for `river-window-v1' objects that are external windows.")

(defvar rau--command-timer nil
  "Timer used to drain the rau command queue.")

(defvar rau--manage-timer nil
  "Timer used to coalesce `manage-dirty' requests.")

(defvar rau--handling-commands nil
  "Non-nil while `rau--handle-commands' is running.")

(defvar rau--pending-handler nil
  "Non-nil if a command handler was requested while already handling commands.")

(cl-defmacro rau--do (tag (obj val state) &body body)
  "Iterate over the ewc objects in STATE tagged with TAG.
TAG is a form that evaluates to or is an ewc-object tag, for example
`rau--tag-window', `rau--tag-frame', or `'river-output-v1'.

OBJ is bound to the ewc-object and VAL to its data for each
element.  STATE is evaluated once and bound to that same name
within BODY."
  (declare (indent 1))
  (unless (symbolp state)
    (error "rau--do: STATE slot must be a symbol, got: %S" state))
  `(let ((,state ,state))
     (dolist (,obj (ewc-objects (rau-state-client ,state) ,tag))
       (let ((,val (ewc-object-data ,obj)))
         ,@body))))

(defun rau--set-frame-name (emacs-frame)
  (unless (string-prefix-p "rau-frame-"
                           (frame-parameter emacs-frame 'name))
    (set-frame-parameter emacs-frame 'name (make-temp-name "rau-frame-"))))

(defun rau--find-buffer-for-window (window-wl)
  (seq-find (lambda (buf)
              (eq (buffer-local-value 'rau--window-wl buf) window-wl))
            (buffer-list)))

(defun rau--make-buffer-name (app-id title)
  (let ((title-trunc (if (> (length title) 40)
                         (format "%s…" (substring title 0 40))
                       title)))
    (if app-id (concat title-trunc " - " app-id)
      title-trunc)))

(defun rau--handle-commands (state &optional no-focus-update)
  (unless rau--handling-commands
    (let ((rau--handling-commands t))
      ;; Update WM window parameters from the current Emacs window layout.
      (condition-case err
          ;; TODO consider adding back? Was lost in refactoring.
          ;; (setq rau--last-focused nil)
          (let ((params (make-hash-table :test 'eql))
                (changed nil))
            (dolist (emacs-frame (frame-list))
              (dolist (emacs-window (window-list emacs-frame))
                (when-let* ((buffer (window-buffer emacs-window))
                            ((rau--is-rau-buffer buffer))
                            (window-wl (buffer-local-value 'rau--window-wl buffer)))
                  (pcase-let ((`(,left ,top ,right ,bottom)
                               (window-inside-absolute-pixel-edges emacs-window)))
                    (puthash (ewc-object-id window-wl)
                             (rau-window-parameters-make
                              :emacs-frame emacs-frame
                              :x left
                              :y top
                              :w (- right left)
                              :h (- bottom top))
                             params)))))
            (rau--do rau--tag-window (window-wl win state)
              (let* ((id (ewc-object-id window-wl))
                     (new (gethash id params))
                    (old (rau-window-params win)))
                (unless (equal old new)
                  (setf (rau-window-params win) new)
                  (setq changed t))))
            (when changed
              (rau--mark-manage-dirty state)))
        (error
         (message "rau update window parameters error: %S" err)))
      ;; Drain the command queue.
      (while-let ((cmd (pop (rau-state-command-queue state))))
        (condition-case err
            (funcall cmd)
          (error (message "rau command failed: %S" err))))

      (when (or rau--pending-handler
                (rau-state-command-queue state))
        (setq rau--pending-handler nil)
        (rau--schedule-command-handler))
      (unless no-focus-update
        (run-at-time nil nil #'rau--update-focus-request)))))

;; Major mode for rau-managed buffers
(defvar-local rau--window-wl nil
  "Window object for this rau-mode buffer.")

(defun rau--buffer-killed ()
  "Request closing of the associated Wayland surface when a rau buffer is killed."
  (when-let* ((rau--window-wl)
              (data (ewc-object-data rau--window-wl)))
    (setf (rau-window-state data) 'killed)
    (rau--mark-manage-dirty rau--state)))

(define-derived-mode rau-mode special-mode "Rau"
  "Major mode for buffers representing windows managed by rau."
  :group 'rau
  (setq-local buffer-read-only t)
  (add-hook 'kill-buffer-hook #'rau--buffer-killed nil t)
  (scroll-bar-mode 0)
  (setq-local left-fringe-width 0
              right-fringe-width 0))

(defun rau--is-rau-buffer (buf)
  "Return non-nil if BUF is a rau buffer."
  (eq (buffer-local-value 'major-mode buf) 'rau-mode))

;; TODO: Consider showing something useful in rau buffers
;; (title/app-id/dimensions) instead of an empty read-only buffer — it makes
;; debugging focus/placement much easier.
;;
;; TODO: consider (set-window-dedicated-p win t) on the window showing a rau
;; buffer, so an accidental C-x b doesn't silently detach the external surface
;; (as it stands the surface just hides, which is confusing).
(defun rau--create-buffer (window-wl)
  "Create and display a rau buffer for Wayland WINDOW-WL."
  (or (rau--find-buffer-for-window window-wl)
      (let ((buffer (get-buffer-create (make-temp-name "rau-window-"))))
        (with-current-buffer buffer
          (rau-mode)
          (setq-local rau--window-wl window-wl))
        (display-buffer buffer)
        buffer)))

(defvar rau--last-focused nil
  "Last buffer for which a focus request was sent.")

(defun rau--focus-change-allowed-p ()
  "Non-nil when no interactive command or edit is in progress."
  (and (not this-command)
       (length= unread-command-events 0)
       (length= (this-single-command-keys) 0)
       (zerop (minibuffer-depth))
       (zerop (recursion-depth))))

(defun rau--update-focus-for-window (state window-wl)
  "Focus external window WINDOW-WL in STATE if it is displayed.
Return non-nil if the focus state changed.  Return nil without
changing state when the window has no parameters yet, so a later
hook run can retry."
  (if-let* ((win (ewc-object-data window-wl))
            (frame-wl (rau--frame-displaying-win win)))
      (rau--focus-window state window-wl frame-wl)
    (rau-log "rau: cannot focus window that is not displayed")
    nil))

(defun rau--update-focus-request (&rest _)
  "Reconcile Wayland focus with the selected window."
  (when-let* ((state rau--state))
    (let ((buf (window-buffer (selected-window)))
          changed)
      (unless (eq buf rau--last-focused)
        (setq changed
              (cond
               ((and (rau--is-rau-buffer buf) (rau--focus-change-allowed-p))
                (when-let* ((window-wl (buffer-local-value 'rau--window-wl buf)))
                  (rau--update-focus-for-window state window-wl)))
               (t (rau--focus-switch-to-frame state))))
        (when changed
          (setq rau--last-focused buf)
          (rau--mark-manage-dirty state)))
      changed)))

(defconst rau--modifier-bits
  '((shift   . 1)
    (control . 4)
    (meta    . 8)
    (super   . 64)
    (hyper   . 128))
  "Modifier bits as per river_seat_v1.modifiers / XKB")

(defun rau--key-to-xkb (key-string)
  "Decompose KEY-STRING into (EVENT KEY MODIFIERS)."
  (let* ((event (aref (kbd key-string) 0))
         (basic (event-basic-type event))
         (mods (seq-keep (lambda (mod)
                           (alist-get mod rau--modifier-bits))
                         (event-modifiers event)))
         (key (if (characterp basic) basic (symbol-name basic))))
    (list event key (apply #'logior mods))))

(defun rau-push-intercept-prefix (prefix &optional command)
  "Register PREFIX as an intercept key binding.
PREFIX is a key string suitable for `kbd'.
COMMAND may be `toggle-fullscreen'."
  (let* ((data (rau--key-to-xkb prefix))
         (event (nth 0 data))
         (key (nth 1 data))
         (modifiers (nth 2 data))
         (keysym (rau--resolve-keysym key))
         (cmd (if (eq command 'toggle-fullscreen)
                  'toggle-fullscreen
                event))
         (binding-key (cons keysym modifiers)))
    (if (= keysym 0)
        (message "rau: could not resolve XKB keysym for %S" key)
      (let ((existing (gethash binding-key (rau-state-bindings rau--state))))
        (if existing
            (setf (rau-binding-command existing) cmd
                  (rau-binding-event existing) event)
          (puthash binding-key
                   (rau-binding-make
                    :keysym keysym
                    :modifiers modifiers
                    :command cmd
                    :event event
                    :state 'requested)
                   (rau-state-bindings rau--state))))
      (rau--mark-manage-dirty rau--state))))

(defcustom rau-intercept-prefixes
  '("C-x" "C-u" "C-h" "M-x")
  "Prefix keys that should always go to Emacs."
  :type '(repeat key)
  :set (lambda (sym val)
         (set-default sym val)
         (when (and (boundp 'rau--state)
                    rau--state
                    (fboundp 'rau-push-intercept-prefixes))
           (rau-push-intercept-prefixes))))

(defun rau-push-intercept-prefixes () ;; TODO: remove, leave only one way?
  "Update the intercept prefixes defined in `rau-intercept-prefixes'."
  (dolist (prefix rau-intercept-prefixes)
    (rau-push-intercept-prefix prefix)))

(defun rau--suppress-focus-event (_orig-fn _event)
  "No-op for suppressing certain focus events in advice."
  (interactive "e")
  ;; okay, *almost* no-op ...
  (setq rau--last-focused nil))

(defun rau--buffer-predicate (buffer)
  "Buffer predicate to avoid accidentally showing the same rau buffer twice."
  (or (not (with-current-buffer buffer (derived-mode-p 'rau-mode)))
      (not (get-buffer-window buffer t))))

(defun rau--split-window-advice (new-window)
  "Advice window splits to always display another buffer, if a rau buffer
was split."
  (with-selected-window new-window
    (with-current-buffer (window-buffer)
      (when (derived-mode-p 'rau-mode)
        (switch-to-buffer (other-buffer)))))
  new-window)

(defun rau--set-window-buffer-advice (orig win buf &rest r)
  "Avoid double-display of rau buffers, by stealing them if they are
visible elsewhere. Note that displaying the same buffer in two different
tabs, for example, is completely valid."
  (with-current-buffer buf
    (when (derived-mode-p 'rau-mode)
      (dolist (other (get-buffer-window-list buf nil 'visible))
        (unless (eq (or win (selected-window)) other)
          (with-selected-window other
            (switch-to-buffer (other-buffer)))))))
  (apply orig win buf r))

(defun rau-log (&rest args)
  "Log ARGS with `message' when `rau-debug' is non-nil."
  (when rau-debug
    (apply #'message args)))

;;; Protocol loading

(eval-and-compile
  (defconst rau--protocol-basenames
    '(("wayland" wl-display wl-registry)
      "river-window-management-v1"
      "river-xkb-bindings-v1"
      "river-layer-shell-v1")
    "Wayland protocols used by rau.")

  (defun rau--protocol-dir ()
    "Return the protocol directory next to the rau.el source.
The location is resolved relative to rau.el itself, not the file that
happens to be loading, so `rau--read-protocols' expands correctly even
when used from other files (e.g. tests)."
    (let* ((rau-file
            (cond
             ;; rau.el is currently being loaded.
             ((and load-file-name
                   (member (file-name-nondirectory load-file-name)
                           '("rau.el" "rau.elc")))
              load-file-name)
             ;; rau.el is currently being byte-compiled.
             ((and (bound-and-true-p byte-compile-current-file)
                   (member (file-name-nondirectory byte-compile-current-file)
                           '("rau.el" "rau.elc")))
              byte-compile-current-file)
             ;; Expansion originates from some other file: find rau.el
             ;; on the load path.
             (t
              (or (locate-file "rau" load-path '(".el" ".elc"))
                  (error "rau: cannot locate rau.el; add its directory to `load-path'")))))
           (rau-dir (file-name-directory (expand-file-name rau-file))))
      (expand-file-name "protocol" rau-dir)))

  (defun rau--protocol-file (basename)
    "Return the absolute XML file path for protocol BASENAME."
    (expand-file-name (concat basename ".xml")
                      (rau--protocol-dir)))

  (defmacro rau--read-protocols ()
  "Expand to an `ewc-read' form with protocol paths and interface filters."
  `(ewc-read
    ,@(mapcar
       (lambda (spec)
         (let ((spec (ensure-list spec)))
           (cons (rau--protocol-file (car spec))
                 (cdr spec))))
       rau--protocol-basenames))))

(defconst rau--global-binds
  '("river_window_manager_v1"
    "river_xkb_bindings_v1"
    "river_layer_shell_v1")
  "Wayland globals that rau binds.")

(defconst rau--edges-all 15
  "River Edges::all() bitmask.")

;;; Basic helpers

(defun rau--request (object-wl request &optional arguments)
  "Send REQUEST on OBJECT using the current rau Wayland connection."
  (ewc-request (rau-state-connection rau--state)
               object-wl
               request
               arguments))

(defun rau--interface-version (client protocol interface)
  "Return XML-declared version of INTERFACE in PROTOCOL."
  (when-let* ((protocol-def (alist-get protocol
                                       (ewc-client-protocols client)))
              (interface-def (alist-get interface protocol-def)))
    ;; interface-def is: (version events requests)
    (car interface-def)))

;; TODO consider moving to ewc.el
(defun rau--decode-string (s)
  "Decode a Wayland string S as UTF-8.
Return nil if S is nil or empty."
  (when (and s (not (string-empty-p s)))
    (decode-coding-string s 'utf-8)))

(defun rau--frame-by-cond (state predicate)
  "Return the first frame ewc-object in STATE matching PREDICATE."
  (cl-loop for frame-wl in (ewc-objects (rau-state-client state) rau--tag-frame)
           for f = (ewc-object-data frame-wl)
           thereis (and f (funcall predicate f) frame-wl)))

(defun rau--frame-displaying-win (win)
  "Return the ewc frame object displaying window WIN."
  (when-let* ((p (rau-window-params win))
              (emacs-frame (rau-window-parameters-emacs-frame p))
              ((frame-live-p emacs-frame)))
    (frame-parameter emacs-frame 'rau-frame-wl)))

(defun rau--frame-for-output (state output-wl)
  "Return frame associated to OUTPUT or nil."
  (rau--frame-by-cond state (lambda (f) (eq (rau-frame-output-wl f) output-wl))))

(defun rau--frame-without-output (state)
  "Return a frame not associated to any output or nil."
  (rau--frame-by-cond state (lambda (f) (null (rau-frame-output-wl f)))))

(defun rau--frame-with-any-output (state)
  "Return any frame with an associated output or nil."
  (rau--frame-by-cond state #'rau-frame-output-wl))

;;; Command queue

(defun rau--schedule-command-handler ()
  "Schedule command processing if not already scheduled."
  (if rau--handling-commands
      (setq rau--pending-handler t)
    (unless rau--command-timer
      (setq rau--command-timer
            (run-at-time
             0 nil
             (lambda (state)
               (setq rau--command-timer nil)
               (condition-case err
                   (rau--handle-commands state)
                 (error
                  (message "rau command handler error: %S" err))))
             rau--state)))))

(defun rau--enqueue (fn)
  "Queue FN for execution by rau--command-handler and schedule the command
handler timer. Only to be used in event listeners."
  (setf (rau-state-command-queue rau--state)
        (append (rau-state-command-queue rau--state) (list fn)))
  (rau--schedule-command-handler))

;;; manage-dirty coalescing

(defun rau--mark-manage-dirty (state)
  "Mark that a new manage sequence is needed."
  (unless rau--manage-timer
    (setq rau--manage-timer
          (run-at-time
           0 nil
           (lambda (state)
             (setq rau--manage-timer nil)
             (condition-case err
                 (when-let* ((client (rau-state-client state))
                             (wm-wl (ewc-first-object client
                                                   'river-window-manager-v1)))
                   (rau--request wm-wl 'manage-dirty))
               (error
                (message "rau manage-dirty error: %S" err))))
           state))))

;;; Focus helpers

(defun rau--focus-frame (state frame-wl)
  "Focus FRAME-WL in STATE.
Return non-nil if the focus state changed."
  (let ((changed
         (not (and (eq (rau-state-focus-state state) 'frame)
                   (eq
                    (rau-state-focused-frame state)
                    frame-wl)))))
    (setf (rau-state-focus-state state) 'frame
          (rau-state-focused-frame state) frame-wl
          (rau-state-focused-window state) nil)
    changed))

(defun rau--focus-window (state window-wl frame-wl)
  "Focus WINDOW-WL on FRAME-WL in STATE.
Return non-nil if the focus state changed."
  (let ((changed
         (not (and (eq (rau-state-focus-state state) 'window)
                   (eq
                    (rau-state-focused-window state)
                    window-wl)
                   (eq
                    (rau-state-focused-frame state)
                    frame-wl)))))
    (setf (rau-state-focus-state state) 'window
          (rau-state-focused-window state) window-wl
          (rau-state-focused-frame state) frame-wl)
    changed))

(defun rau--focus-switch-to-frame (state)
  "Switch focus from an external window back to its Emacs frame.
Return non-nil if the focus state changed."
  (when (eq (rau-state-focus-state state) 'window)
    (if (rau-state-focused-frame state)
        (setf (rau-state-focus-state state) 'frame
              (rau-state-focused-window state) nil)
      (setf (rau-state-focus-state state) 'lost
            (rau-state-focused-window state) nil
            (rau-state-focused-frame state) nil))
    t) ;; TODO: Why this t here? Is it needed?
  )

(defun rau--focus-current (state)
  "Return current focus as (TARGET-WL . FRAME-WL), if any. TARGET-WL is
either a WINDOW-WL or FRAME-WL."
  (pcase (rau-state-focus-state state)
    ('window
     (when-let* ((window-wl (rau-state-focused-window state))
                (frame-wl (rau-state-focused-frame state)))
       (cons window-wl frame-wl)))
    ('frame
     (when-let* ((frame-wl (rau-state-focused-frame state)))
       (cons frame-wl frame-wl)))
    (_ nil)))

(defun rau--focus-invalidate (state target-wl)
  "Invalidate TARGET-WL in STATE's focus tracking, e.g. after close."
  (cond
   ((and (rau-state-focused-frame state)
         (eq (rau-state-focused-frame state) target-wl))
    (setf (rau-state-focus-state state) 'lost
          (rau-state-focused-frame state) nil
          (rau-state-focused-window state) nil))

   ((and (rau-state-focused-window state)
         (eq (rau-state-focused-window state) target-wl))
    (setf (rau-state-focus-state state) 'frame
          (rau-state-focused-window state) nil))))

;;; Fullscreen helpers

(defun rau--fs-window (fs)
  "Return the window involved in fullscreen state FS, if any."
  (pcase (rau-fs-state fs)
    ('requested (rau-fs-new fs))
    ((or 'fullscreen 'exiting) (rau-fs-window fs))
    (_ nil)))

(defun rau--select-buffer-for-window (window-wl)
  "Select the Emacs window displaying Wayland WINDOW-WL, if visible."
  (when-let* ((buf (rau--find-buffer-for-window window-wl))
              (emacs-window (get-buffer-window buf t)))
    (select-window emacs-window 'norecord)))

;;; XKB keysym resolution

(defvar rau--xkb-keysym-alist
  '(("return" . #xFF0D)
    ("escape" . #xFF1B)
    ("backspace" . #xFF08)
    ("tab" . #xFF09)
    ("space" . #x020)
    ("delete" . #xFFFF)
    ("home" . #xFF50)
    ("left" . #xFF51)
    ("up" . #xFF52)
    ("right" . #xFF53)
    ("down" . #xFF54)
    ("prior" . #xFF55)
    ("pageup" . #xFF55)
    ("next" . #xFF56)
    ("pagedown" . #xFF56)
    ("end" . #xFF57)
    ("insert" . #xFF63)
    ("begin" . #xFF58)
    ("select" . #xFF60)
    ("print" . #xFF61)
    ("execute" . #xFF62)
    ("pause" . #xFF13)
    ("scroll" . #xFF14)
    ("sysrq" . #xFF15)
    ("f1" . #xFFBE)
    ("f2" . #xFFBF)
    ("f3" . #xFFC0)
    ("f4" . #xFFC1)
    ("f5" . #xFFC2)
    ("f6" . #xFFC3)
    ("f7" . #xFFC4)
    ("f8" . #xFFC5)
    ("f9" . #xFFC6)
    ("f10" . #xFFC7)
    ("f11" . #xFFC8)
    ("f12" . #xFFC9)
    ("xf86audioraisevolume" . #x1008FF13)
    ("xf86audiolowervolume" . #x1008FF11)
    ("xf86audiomute" . #x1008FF12)
    ("xf86audioplay" . #x1008FF14)
    ("xf86audiostop" . #x1008FF15)
    ("xf86audiopause" . #x1008FF31)
    ("xf86audionext" . #x1008FF17)
    ("xf86audioprev" . #x1008FF16)
    ("xf86monbrightnessup" . #x1008FF02)
    ("xf86monbrightnessdown" . #x1008FF03))
  "Small fallback table mapping key names to XKB keysyms.")

(defun rau--utf32-to-keysym (cp)
  "Convert Unicode codepoint CP to an XKB keysym."
  (cond
   ((not (integerp cp)) 0)
   ((and (>= cp #x20) (<= cp #x7e)) cp)
   ((and (>= cp #xa0) (<= cp #xff)) cp)
   ((> cp 0) (+ #x01000000 cp))
   (t 0)))

(defun rau--resolve-keysym (key)
  "Resolve KEY to an XKB keysym.
KEY may be an integer codepoint, a symbol, or a string key name."
  (cond
   ((integerp key)
    (rau--utf32-to-keysym key))

   ((symbolp key)
    (rau--resolve-keysym (symbol-name key)))

   ((stringp key)
    (or (alist-get (downcase key)
                   rau--xkb-keysym-alist
                   nil nil #'equal)
        (and (= (length key) 1)
             (rau--utf32-to-keysym (aref key 0)))
        0))

   (t 0)))



;;; Layer shell attachment helpers

(defun rau--ensure-ls-output (state output-wl)
  "Create a layer-shell output object for OUTPUT-WL if possible."
  (when-let* ((out (ewc-object-data output-wl))
              ((null (rau-output-ls-output-wl out)))
              (client (rau-state-client state))
              (ls-wl (ewc-first-object client 'river-layer-shell-v1))
              (ls-output-id (cl-incf (ewc-client-new-id client)))
              (ls-output-wl
               (ewc-object-add client 'river-layer-shell-output-v1 ls-output-id)))
    (setf (rau-output-ls-output-wl out) ls-output-wl)
    (rau--request ls-wl 'get-output
                   `((id . ,ls-output-id)
                     (output . ,(ewc-object-id output-wl))))))

;; TODO: this ls-seat is never used ATM.
(defun rau--ensure-ls-seat (state)
  "Create a layer-shell seat object for the current seat if possible."
  (when-let* ((client (rau-state-client state))
              (seat-wl (ewc-first-object client 'river-seat-v1))
              (seat (ewc-object-data seat-wl))
              ((null (rau-seat-ls-seat-wl seat)))
              (ls-wl (ewc-first-object client 'river-layer-shell-v1))
              (ls-seat-id (cl-incf (ewc-client-new-id client)))
              (ls-seat-wl
               (ewc-object-add client
                               'river-layer-shell-seat-v1
                               ls-seat-id)))
    (setf (rau-seat-ls-seat-wl seat) ls-seat-wl)
    (rau--request ls-wl 'get-seat
                   `((id . ,ls-seat-id)
                     (seat . ,(ewc-object-id seat-wl))))))

;;; Listeners

;; Order event listeners by their order in the protocol definitions!

;;;; wayland protocol
;;;; wl-display listeners
;; TODO handle individual args and decode the message string with
;; rau--decode-string
(defun rau-on-wl-display-error (_display-wl args)
  (message "wl_display error: %S" args))

(defun rau-on-wl-display-delete-id (_display-wl args)
  (pcase-let (((map id) args))
    (when-let* ((client (rau-state-client rau--state))
                (table (ewc-client-table client))
                (object-wl (gethash id table)))
      (message "delete-id for object %d, interface=%s" id (ewc-object-interface object-wl))
      (ewc-object-remove client object-wl))))

;;;; wl-registry listeners
(defun rau-on-wl-registry-global (registry-wl args)
  (pcase-let (((map name interface version) args))
    (when-let* ((ifsym (intern (string-replace "_" "-" interface)))
                (client (rau-state-client rau--state))
                (protocols (ewc-client-protocols client))
                (protocol (ewc-find-protocol protocols ifsym))
                ((member interface rau--global-binds))
                (new-id (cl-incf (ewc-client-new-id client)))
                (xml-version (rau--interface-version client protocol ifsym))
                (bind-version
                 (if xml-version (min version xml-version) version)))
      (ewc-object-add client ifsym new-id)
      (rau-log "rau: binding global %s version %s" interface bind-version)
      (rau--request registry-wl 'bind
                     `((name . ,name)
                       (interface-len . ,(1+ (string-bytes interface)))
                       (interface . ,interface)
                       (version . ,bind-version)
                       (id . ,new-id)))
      (pcase ifsym
        ('river-layer-shell-v1
         ;; Attach layer-shell objects to existing outputs/seats in STATE."
         (rau--do 'river-output-v1 (output-wl _out rau--state)
                   (rau--ensure-ls-output rau--state output-wl))
         (rau--ensure-ls-seat rau--state))

        (_ (rau-log "rau: bound %s" ifsym))))))

;;;; river-window-management-v1 Protocol
;;;; river-window-manager-v1 listeners
(defun rau-on-river-window-manager-v1-unavailable (_wm-wl _)
  (message "rau: WM event unavailable"))

(defun rau-on-river-window-manager-v1-finished (_wm-wl _)
  (message "rau: WM event finished"))

(defun rau-on-river-window-manager-v1-manage-start (wm-wl _)
  (unwind-protect
      (condition-case err
          (rau--reconcile rau--state)
        (error
         (message "rau reconcile error: %S" err)))
    (rau--request wm-wl 'manage-finish)))

(defun rau-on-river-window-manager-v1-render-start (wm-wl _)
  (unwind-protect
      (condition-case err
          (progn
            (rau--render-frames rau--state)
            (rau--render-windows rau--state))
        (error
         (message "rau render error: %S" err)))
    (rau--request wm-wl 'render-finish)))

(defun rau-on-river-window-manager-v1-session-locked (_wm-wl _)
  (message "rau: WM event session-locked"))

(defun rau-on-river-window-manager-v1-session-unlocked (_wm-wl _)
  (message "rau: WM event session-unlocked"))

(defun rau-on-river-window-manager-v1-window (_wm-wl args)
  (pcase-let* (((map id) args))
    (ewc-object-add (rau-state-client rau--state)
                    'river-window-v1
                    id)))

(defun rau-on-river-window-manager-v1-output (_wm-wl args)
  (pcase-let* (((map id) args)
               (client (rau-state-client rau--state))
               (output-wl (ewc-object-add client 'river-output-v1 id)))
    (setf (ewc-object-data output-wl)
          (rau-output-make))
    (rau--ensure-ls-output rau--state output-wl)))

(defun rau-on-river-window-manager-v1-seat (_wm-wl args)
  (pcase-let (((map id) args)
              (client (rau-state-client rau--state)))
    (if (ewc-first-object client 'river-seat-v1)
        (message "rau does not support multi-seat")
      (let* ((seat-wl (ewc-object-add client 'river-seat-v1 id)))
        (setf (ewc-object-data seat-wl) (rau-seat-make))
        (rau--ensure-ls-seat rau--state)))))

;;;; river-window-v1 listeners
(defun rau-on-river-window-v1-closed (window-wl _)
  (let ((client (rau-state-client rau--state)))
    (when (ewc-object-tagged-p window-wl rau--tag-window)
      (rau--enqueue
       (lambda ()
         (when-let* ((buf (rau--find-buffer-for-window window-wl)))
           (kill-buffer buf)))))
    (rau--focus-invalidate rau--state window-wl)

    ;; Reset fullscreen on output if window was fullscreen.
    (rau--do 'river-output-v1 (output-wl out rau--state)
      (when (eq (rau--fs-window (rau-output-fullscreen out)) window-wl)
        (setf (rau-output-fullscreen out) (rau-fs))))

    (when-let* ((data (ewc-object-data window-wl))
                (node-wl (rau-surface-node-wl data)))
      (rau--request node-wl 'destroy)
      (ewc-object-remove client node-wl))

    (rau--request window-wl 'destroy)
    (ewc-object-remove client window-wl)))

(defun rau-on-river-window-v1-dimensions (window-wl args)
  (pcase-let (((map width height) args))
    (when-let* ((data (ewc-object-data window-wl))
                ((rau-window-p data)))
      (setf (rau-window-actual-width data) width
            (rau-window-actual-height data) height))))

(defun rau-on-river-window-v1-app-id (window-wl args)
  (pcase-let (((map app-id) args))
    (when-let* ((app-id (rau--decode-string app-id))
                (win (ewc-object-data window-wl))
                ((rau-window-p win)))
      (setf (rau-window-app-id win) app-id))))

(defun rau-on-river-window-v1-title (window-wl args)
  (pcase-let (((map title) args))
    (when-let* ((title (rau--decode-string title))
                (data (ewc-object-data window-wl)))
      (setf (rau-surface-title data) title)
      (when (rau-window-p data)
        (rau--enqueue
         (lambda ()
           (when-let* ((buf (rau--find-buffer-for-window window-wl)))
             (with-current-buffer buf
               (rename-buffer
                (rau--make-buffer-name
                 (rau-window-app-id data)
                 title)
                t))))))
      (when (rau-frame-p data)
        (when-let* ((emacs-frame
                     (cl-find title (frame-list)
                              :test #'equal
                              :key (lambda (f) (frame-parameter f 'name)))))
          (setf (rau-frame-emacs-frame data) emacs-frame)
          (rau--enqueue
           (lambda ()
             (when (frame-live-p emacs-frame)
               (set-frame-parameter emacs-frame 'rau-frame-wl window-wl)))))))))

(defun rau-on-river-window-v1-fullscreen-requested (window-wl args)
  (pcase-let* (((map output) args)
               (output-wl
                (or (and (integerp output)
                         (not (zerop output))
                         (ewc-object-get (rau-state-client rau--state) output))
                    (when-let* ((w (ewc-object-data window-wl))
                                ((rau-window-p w))
                                (frame-wl (rau--frame-displaying-win w))
                                (frame (ewc-object-data frame-wl)))
                      (rau-frame-output-wl frame))
                    (when-let* ((cur (rau--focus-current rau--state))
                                (frame-wl (cdr cur))
                                (frame (ewc-object-data frame-wl)))
                      (rau-frame-output-wl frame)))))
    (if (not output-wl)
        (message "Fullscreen requested, but no output found")
      (when-let* ((out (ewc-object-data output-wl)))
        (let* ((fs (rau-output-fullscreen out))
               (previous
                (pcase (rau-fs-state fs)
                  ((or 'fullscreen 'exiting)
                   (rau-fs-window fs))
                  (_ nil))))
          (setf (rau-output-fullscreen out)
                (rau-fs :state 'requested
                         :new window-wl
                         :previous previous)))))))

(defun rau-on-river-window-v1-exit-fullscreen-requested (window-wl _)
  (rau--do 'river-output-v1 (output-wl out rau--state)
    (let ((fs (rau-output-fullscreen out)))
      (when (and (member (rau-fs-state fs)
                         '(requested fullscreen))
                 (eq (rau--fs-window fs) window-wl))
        (setf (rau-output-fullscreen out)
              (rau-fs :state 'exiting
                       :window (rau--fs-window fs)))))))

(defun rau-on-river-window-v1-minimize-requested (window-wl _)
  (when-let* ((data (ewc-object-data window-wl))
              ((rau-window-p data)))
    (rau--enqueue
     (lambda ()
       (when-let* ((buf (rau--find-buffer-for-window window-wl)))
         (with-current-buffer buf
           (bury-buffer)))))))

(defun rau-on-river-window-v1-unreliable-pid (window-wl args)
  (pcase-let* (((map unreliable-pid) args)
               (client (rau-state-client rau--state))
               (node-wl (ewc-object-add client 'river-node-v1)))

    (rau--request window-wl 'get-node
                   `((id . ,(ewc-object-id node-wl))))

    (if (= unreliable-pid (rau-state-pid rau--state))
        (progn
          (rau-log "Discovered new Emacs frame")
          (let ((frame (rau-frame-make
                        :node-wl node-wl)))
            (setf (ewc-object-data window-wl) frame)
            (ewc-object-tag client window-wl rau--tag-frame))
          (if (> (rau-state-pending-frames rau--state) 0)
              (cl-decf (rau-state-pending-frames rau--state))
            (rau-log "New frame was not requested by WM")))

      (rau-log "Discovered new regular external window")
      (let ((win (rau-window-make
                  :node-wl node-wl)))
        (setf (ewc-object-data window-wl) win)
        (ewc-object-tag client window-wl rau--tag-window))
      (rau--enqueue
       (lambda ()
         ;; Confirm that the Emacs-side buffer for the window was created.
         (when-let* ((win (ewc-object-data window-wl)))
           (rau--create-buffer window-wl)
           (setf (rau-window-state win) 'active)
           (rau--mark-manage-dirty rau--state)))))))

;;;; river-output-v1 listeners
(defun rau-on-river-output-v1-removed (output-wl _)
  (let ((client (rau-state-client rau--state)))
    (rau--do rau--tag-frame (frame-wl frame rau--state)
      (when (eq (rau-frame-output-wl frame) output-wl)
        (setf (rau-frame-output-wl frame) nil)
        (rau--enqueue
         (lambda ()
           (when-let* ((emacs-frame (rau-frame-emacs-frame frame))
                       ((frame-live-p emacs-frame)))
             (delete-frame emacs-frame))))))
    (when-let* ((out (ewc-object-data output-wl))
                (ls-output-wl (rau-output-ls-output-wl out)))
      (rau--request ls-output-wl 'destroy)
      (ewc-object-remove client ls-output-wl))
    (rau--request output-wl 'destroy)
    (ewc-object-remove client output-wl)))

;; TODO: listener for wl_output, e.g. to get monitor names

(defun rau-on-river-output-v1-position (output-wl args)
  (pcase-let (((map x y) args))
    (when-let* ((out (ewc-object-data output-wl)))
      (setf (rau-output-x out) x
            (rau-output-y out) y))))

(defun rau-on-river-output-v1-dimensions (output-wl args)
  (pcase-let (((map width height) args))
    (when-let* ((out (ewc-object-data output-wl)))
      (setf (rau-output-width out) width
            (rau-output-height out) height))))

;;;; river-seat-v1 listener
(defun rau-on-river-seat-v1-window-interaction (_seat-wl args)
  (pcase-let* (((map window) args))
    (when-let* ((window-wl (ewc-object-get (rau-state-client rau--state) window)))
      (cond
       ((ewc-object-tagged-p window-wl rau--tag-frame)
        (when (rau--focus-frame rau--state window-wl)
          (setf (rau-state-focus-dirty rau--state) t)))
       ((ewc-object-tagged-p window-wl rau--tag-window)
        (if-let* ((w (ewc-object-data window-wl))
                  (frame-wl (rau--frame-displaying-win w)))
            (when (rau--focus-window rau--state window-wl frame-wl)
              (setf (rau-state-focus-dirty rau--state) t))
          (message "Window interaction for window without frame")))))))

;;;; river-xkb-bindings-v1 protocol
;;;; river-xkb-binding-v1 listeners
(defun rau-on-river-xkb-binding-v1-pressed (binding-wl _)
  (when-let* ((binding (ewc-object-data binding-wl))
              (command (rau-binding-command binding)))
    (if (eq command 'toggle-fullscreen)
        (rau--toggle-fullscreen rau--state)
      (rau--enqueue
       (lambda ()
         (push (cons t command) unread-command-events)))
      (rau--focus-switch-to-frame rau--state))))

;;;; river-layer-shell-v1 protocol
;;;; river-layer-shell-output-v1 listeners
(defun rau-on-river-layer-shell-output-v1-non-exclusive-area (ls-output-wl args)
  (pcase-let (((map x y width height) args))
    (when-let* ((out (cl-loop for output-wl in (ewc-objects
                                          (rau-state-client rau--state)
                                          'river-output-v1)
                              for data = (ewc-object-data output-wl)
                              thereis (and data
                                           (eq (rau-output-ls-output-wl data) ls-output-wl)
                                           data))))
      (setf (rau-output-x out) x
            (rau-output-y out) y
            (rau-output-width out) width
            (rau-output-height out) height))))

;;; Reconciliation

(defun rau--reconcile-frames (state)
  "Ensure each output gets one maximized Emacs frame."
  (let ((frame-requests 0))
    (rau--do 'river-output-v1 (output-wl out state)
              (if-let* ((frame-wl (rau--frame-for-output state output-wl))
                        (f (ewc-object-data frame-wl)))
                  ;; Frame already assigned: only re-propose if size changed.
                  (unless (and (eq (rau-frame-proposed-width f) (rau-output-width out))
                               (eq (rau-frame-proposed-height f) (rau-output-height out)))
                    (setf (rau-frame-proposed-width f) (rau-output-width out)
                          (rau-frame-proposed-height f) (rau-output-height out))
                    (rau--request frame-wl
                                   'propose-dimensions
                                   `((width . ,(rau-output-width out))
                                     (height . ,(rau-output-height out)))))

                (if-let* ((frame-wl (rau--frame-without-output state))
                          (f (ewc-object-data frame-wl)))
                    (progn
                      (setf (rau-frame-output-wl f) output-wl
                            (rau-frame-proposed-width f) (rau-output-width out)
                            (rau-frame-proposed-height f) (rau-output-height out))
                      (rau--request frame-wl
                                     'propose-dimensions
                                     `((width . ,(rau-output-width out))
                                       (height . ,(rau-output-height out))))
                      (rau--request frame-wl
                                     'inform-maximized)
                      (rau--request frame-wl
                                     'set-tiled
                                     `((edges . ,rau--edges-all))))
                  ;; No frame on this output yet: request one.
                  (cl-incf frame-requests))))
    (dotimes (_ (- frame-requests (rau-state-pending-frames state)))
      (rau--enqueue (lambda () (make-frame)))
      (cl-incf (rau-state-pending-frames state)))))

(defun rau--reconcile-windows (state)
  "Close killed windows and propose dimensions for active windows."
  (rau--do rau--tag-window (window-wl win state)
            (pcase (rau-window-state win)
              ;; nothing to do for window-state 'starting
              ('active
               (when-let* ((params (rau-window-params win)))
                 ;; TODO: This gets sent on every loop for all windows?
                 (rau--request window-wl
                                'set-tiled
                                `((edges . ,rau--edges-all)))

                 (rau--request window-wl
                                'propose-dimensions
                                `((width . ,(rau-window-parameters-w params))
                                  (height . ,(rau-window-parameters-h params))))))
              ('killed
               (rau--request window-wl 'close)))))

(defun rau--reconcile-bindings (state)
  "Create and enable XKB bindings."
  (when-let* ((client (rau-state-client state))
              (xkb-bindings-wl (ewc-first-object client 'river-xkb-bindings-v1))
              (seat-wl (ewc-first-object client 'river-seat-v1)))
    (maphash
     (lambda (_key binding)
       (when (eq (rau-binding-state binding) 'requested)
         (let* ((id (cl-incf (ewc-client-new-id client)))
                (binding-wl (ewc-object-add client 'river-xkb-binding-v1 id)))
           (rau--request xkb-bindings-wl 'get-xkb-binding
                          `((seat . ,(ewc-object-id seat-wl))
                            (keysym . ,(rau-binding-keysym binding))
                            (modifiers . ,(rau-binding-modifiers binding))
                            (id . ,id)))
           (setf (ewc-object-data binding-wl) binding
                 (rau-binding-state binding) 'registered)
           (rau--request binding-wl 'enable)
           (setf (rau-binding-state binding) 'enabled))))
     (rau-state-bindings state))))

(defun rau--reconcile-fullscreen (state)
  "Advance fullscreen state machines."
  (rau--do 'river-output-v1 (output-wl out state)
    (let ((fs (rau-output-fullscreen out)))
      (pcase (rau-fs-state fs)
        ('requested
         (let ((new-wl (rau-fs-new fs))
               (prev-wl (rau-fs-previous fs)))
           (when (and prev-wl (ewc-object-p prev-wl))
             (rau--request prev-wl 'inform-not-fullscreen)
             (rau--request prev-wl 'exit-fullscreen))
           (when (and new-wl (ewc-object-p new-wl))
             (rau--request new-wl 'inform-fullscreen)
             (rau--request new-wl 'fullscreen
                            `((output . ,(ewc-object-id output-wl)))))
           (setf (rau-output-fullscreen out)
                 (rau-fs :state 'fullscreen :window new-wl))))
        ('exiting
         (let ((window-wl (rau-fs-window fs)))
           (when (and window-wl (ewc-object-p window-wl))
             (rau--request window-wl 'inform-not-fullscreen)
             (rau--request window-wl 'exit-fullscreen))
           (setf (rau-output-fullscreen out) (rau-fs))))
        (_ nil)))))

(defun rau--reconcile-focus (state)
  "Update the seat focus based on STATE."
  (unless (rau--focus-current state)
    (setf (rau-state-focus-state state) 'lost))

  (when (eq (rau-state-focus-state state) 'lost)
    (when-let* ((frame-wl (rau--frame-with-any-output state)))
      (rau--focus-frame state frame-wl)))

  (when-let* ((client (rau-state-client state))
              (seat-wl (ewc-first-object client 'river-seat-v1))
              (cur (rau--focus-current state)))
    (let* ((target-wl (car cur))
           (frame-wl (cdr cur))
           (dirty (rau-state-focus-dirty state)))

      (when (and target-wl (ewc-object-p target-wl))
        (rau--request seat-wl
                       'focus-window
                       `((window . ,(ewc-object-id target-wl)))))

      (when dirty
        (when-let* ((frame-data (and frame-wl
                                    (ewc-object-data frame-wl)))
                    (output-wl (rau-frame-output-wl frame-data))
                    (out (ewc-object-data output-wl))
                    (ls-output-wl (rau-output-ls-output-wl out)))
          (rau--request ls-output-wl 'set-default))

        (unless (and frame-wl target-wl (eq target-wl frame-wl))
          (rau--enqueue
           (lambda ()
             (rau--select-buffer-for-window target-wl))))

        (setf (rau-state-focus-dirty state) nil)))))

(defun rau--reconcile (state)
  "Run the manage-sequence reconciliation for STATE."
  (rau--reconcile-frames state)
  (rau--reconcile-windows state)
  (rau--reconcile-bindings state)
  (rau--reconcile-fullscreen state)
  (rau--reconcile-focus state))

(defun rau--render-frames (state)
  "Run the render-sequence reconciliation for frames on STATE."
  (rau--do rau--tag-frame (frame-wl frame state)
            (let ((node-wl (rau-surface-node-wl frame))
                  (output-wl (rau-frame-output-wl frame)))
              (if (not output-wl)
                  (when (rau-frame-visible frame)
                    (setf (rau-frame-visible frame) nil)
                    (rau--request frame-wl 'hide))
                (unless (rau-frame-visible frame)
                  (setf (rau-frame-visible frame) t)
                  (rau--request frame-wl 'show))
                (when node-wl
                  (rau--request node-wl 'place-bottom))
                (when-let* ((out (ewc-object-data output-wl))
                            (node-wl)
                            ((not (and (eq (rau-frame-last-x frame) (rau-output-x out))
                                       (eq (rau-frame-last-y frame) (rau-output-y out))))))
                  (setf (rau-frame-last-x frame) (rau-output-x out)
                        (rau-frame-last-y frame) (rau-output-y out))
                  (rau--request node-wl 'set-position
                                 `((x . ,(rau-output-x out))
                                   (y . ,(rau-output-y out)))))))))

(defun rau--render-windows (state)
  "Run the render-sequence reconciliation for windows on STATE."
  (rau--do rau--tag-window (window-wl win state)
    (let ((node-wl (rau-surface-node-wl win)))
      (if (not (eq (rau-window-state win) 'active))
          (rau--request window-wl 'hide)

        (if-let* ((params (rau-window-params win))
                  (frame-wl (rau--frame-displaying-win win))
                  (frame (ewc-object-data frame-wl))
                  (output-wl (rau-frame-output-wl frame))
                  (out (ewc-object-data output-wl)))
            (progn
              (rau--request window-wl 'show)

              (when node-wl
                (rau--request node-wl 'set-position
                             `((x . ,(+ (rau-window-parameters-x params)
                                        (rau-output-x out)))
                               (y . ,(+ (rau-window-parameters-y params)
                                        (rau-output-y out)))))

                (rau--request node-wl 'place-top))

              (let ((clip-w (or (rau-window-actual-width win)
                                (rau-window-parameters-w params)))
                    (clip-h (or (rau-window-actual-height win)
                                (rau-window-parameters-h params))))
                (rau--request window-wl 'set-clip-box
                             `((x . 0)
                               (y . 0)
                               (width . ,clip-w)
                               (height . ,clip-h)))))

          (rau--request window-wl 'hide))))))

;;; Fullscreen toggle

(defun rau--toggle-fullscreen (state)
  "Toggle fullscreen for the currently focused external window."
  (if-let* ((cur (rau--focus-current state))
            (target-wl (car cur))
            (frame-wl (cdr cur)))
      (if (eq target-wl frame-wl)
          (rau--enqueue
           (lambda ()
             (message "rau: can not fullscreen Emacs even more!")))

        (if-let* ((frame-data (ewc-object-data frame-wl))
                  (output-wl (rau-frame-output-wl frame-data))
                  (out (ewc-object-data output-wl)))
            (let ((fs (rau-output-fullscreen out)))
              (pcase (rau-fs-state fs)
                ('none
                 (setf (rau-output-fullscreen out)
                       (rau-fs :state 'requested
                                :new target-wl))
                 (rau--mark-manage-dirty state))

                ('fullscreen
                 (setf (rau-output-fullscreen out)
                       (rau-fs :state 'exiting
                                :window (rau-fs-window fs)))
                 (rau--mark-manage-dirty state))

                (_
             (message "Invalid output state for fullscreen toggle"))))

          (message "Selected frame for fullscreen is not displayed"))))

  (message "Fullscreen requested, but nothing is focused"))

;;; Startup

(defun rau--start-wm ()
  "Connect to Wayland and initialize the rau state."
  (let ((client (ewc-client-make :protocols (rau--read-protocols))))
    (ewc-build-listeners client "rau-on-")
    (let ((connection (ewc-connect client))
          (display-wl (ewc-object-add client 'wl-display))
          (registry-wl (ewc-object-add client 'wl-registry)))

    (setq rau--state (rau-state-make :connection connection
                                       :client client))

    (rau--request display-wl 'get-registry
                   `((registry . ,(ewc-object-id registry-wl)))))))

;; NOTE: No need for rau-disable since this Emacs process is serving as a
;; Window Manager and disabling rau while keeping the Emacs process running
;; would result in an unresponsive user environment.
;;;###autoload
(defun rau-enable ()
  "Enable the rau window manager for river. Call this function once when
starting Emacs inside of river."
  (when rau--state
    (user-error "rau is already running"))

  (unless (eq window-system 'pgtk)
    (user-error "rau requires a pgtk Emacs on Wayland"))

  ;; TODO: this is a hack for lack of ability to figure out alignment ...
  (menu-bar-mode 0)
  (tool-bar-mode 0)

  ;; configure this and all future frames ..
  (let ((frame-params '((undecorated . t)
                        ;; avoid showing the same rau buffer twice
                        (buffer-predicate . rau--buffer-predicate))))
    (modify-all-frames-parameters frame-params))

  (advice-add 'split-window-below :filter-return #'rau--split-window-advice)
  (advice-add 'split-window-right :filter-return #'rau--split-window-advice)
  (advice-add 'set-window-buffer :around #'rau--set-window-buffer-advice)

  (message "Launching rau (pure Elisp) ...")

  ;; Ensure each existing frame has a unique title that rau can match.
  (cl-loop for emacs-frame being the frames
           do (rau--set-frame-name emacs-frame))
  (add-to-list 'after-make-frame-functions #'rau--set-frame-name)

  (rau--start-wm)
  (rau-push-intercept-prefixes)

  ;; Layout signals
  (add-hook 'window-configuration-change-hook #'rau--schedule-command-handler)

  ;; Focus signals
  (add-hook 'window-selection-change-functions #'rau--update-focus-request)
  (add-hook 'window-buffer-change-functions    #'rau--update-focus-request)
  (add-hook 'minibuffer-setup-hook             #'rau--update-focus-request)
  (add-hook 'minibuffer-exit-hook              #'rau--update-focus-request)
  (add-hook 'post-command-hook                 #'rau--update-focus-request)

  ;; Suppress pgtk focus feedback loop (as does EXWM)
  ;; TODO: figure out if/how this breaks multi-frame focus changes ...
  (advice-add 'handle-focus-in  :around #'rau--suppress-focus-event)
  (advice-add 'handle-focus-out :around #'rau--suppress-focus-event)

  (run-hooks 'rau-enable-hook))

(provide 'rau)
;;; rau.el ends here
