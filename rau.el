;;; rau.el --- Wayland Window Manager based on River -*- lexical-binding: t -*-

;; Copyright (C) 2026 Thomas Koch

;; Author: Thomas Koch <thomas@koch.ro>
;; Version: 0.1
;; Keywords: frames
;; URL: https://github.com/thkoch2001/rau
;; Package-Requires: ((emacs "30.2"))

;;; Commentary:
;; Rau is a Wayland Window Manager based on Emacs and River in pure Elisp.  The
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
  "Rau - Emacs swimming in the river."
  :group 'environment
  :prefix "rau-")

(defcustom rau-ready-hook nil
  "Hook run when rau is ready.  At the moment this means that all global
Wayland objects have been registered."
  :type 'hook)

(defvar rau-debug nil
  "When non-nil, enable verbose rau debugging messages.")

(defvar rau--state nil
  "Current global rau WM state.")

(defmacro rau--condition-case (location &rest body)
  "Wrap BODY in a condition-case unless `rau-debug' is non-nil.
LOCATION identifies where the error occurred.  If `rau-debug' is t, BODY
is executed directly so errors drop into the debugger."
  `(if rau-debug
       (progn ,@body)
     (condition-case err
         (progn ,@body)
       (error (message "Error at %s: %S" ,location err)))))

;;; State structs

(cl-defstruct (rau--fs (:constructor rau--fs) (:copier nil))
  "Fullscreen state machine for one output.
STATE is one of: `none', `requested', `fullscreen', `exiting'.
NEW and PREVIOUS are meaningful only when STATE is `requested'.
WINDOW is meaningful when STATE is `fullscreen' or `exiting'."
  (state    'none :type symbol :read-only t)
  (new      nil   :type ewc-object :read-only t)
  (previous nil   :type ewc-object :read-only t)
  (window   nil   :type ewc-object :read-only t))

(cl-defstruct (rau--output (:constructor rau--output-make))
  "State for a River output."
  (ls-output-wl nil :type ewc-object)
  (frame-wl nil :type ewc-object)
  (position '(0 . 0))
  (dimensions '(0 . 0))
  (fullscreen (rau--fs)))

(cl-defstruct (rau--ls-output (:constructor rau--ls-output-make))
  "State for a River output."
  (output-wl nil :type ewc-object)
  (non-excl-position '(0 . 0))
  (non-excl-dimensions '(0 . 0)))

(cl-defstruct (rau--window (:constructor rau--window-make))
  "Common state shared by windows and frames.
Not instantiated directly; windows and frames include it."
  (node-wl nil :type ewc-object)
  actual-dimensions
  app-id
  title
  pid
  role-data)

(cl-defstruct (rau--external (:constructor rau--external-make))
  "State for a regular external window."
  (state 'starting) ;; 'active 'killed
  buffer)

(cl-defstruct (rau--outputframe (:constructor rau--outputframe-make))
  "State for an Emacs frame managed by rau."
  emacs-frame
  (output-wl nil :type ewc-object :documentation "displaying this frame"))

(cl-defstruct (rau--seat (:constructor rau--seat-make))
  "State for a River seat."
  (ls-seat-wl nil :type ewc-object))

(cl-defstruct (rau--binding (:constructor rau--binding-make))
  "State for one global XKB binding."
  key
  keysym
  modifiers
  event
  (needs-focus t) ;; TODO remove when removing the old keybinding stuff
  locked-active
  layout
  (state 'requested))

(cl-defstruct (rau--state (:constructor rau--state-make))
  "Holds the state of the rau Wayland client."
  (client nil :type ewc-client)
  (pid (emacs-pid))
  session-locked

  ;; XKB bindings: (keysym . modifiers) -> rau--binding.
  (bindings (make-hash-table :test 'equal))

  ;;; Focus tracking.
  ;; id of ewc-object for which the last focus request was sent
  ;; This could be a focus_window or a focus_shell_surface request
  (focus-last-id -1)
  ;; window or surface id that should receive focus.
  ;; Events that lead to a focus change can be:
  ;; - window_interaction
  ;; - shell_surface_interaction
  ;; - pointer_enter (for focus follows mouse)
  ;; - pressed (giving temporary focus to emacs for one command)
  (focus-next-id -1)
  ;; Inhibit update focus due to any hooks. This is used in reconcile-focus
  ;; when it changes buffer itself in reaction to an event and this should not
  ;; trigger rau--update-focus-request
  (focus-inhibit-update nil)

  ;; Frame request accounting.
  (pending-frames 1)

  ;; task queue: list of TODO.
  task-queue
  task-queue-after-manage
  task-timer

  ;; manage queue: list of (ewc-object 'request args) for the next manage
  ;; cycle
  manage-queue)

(ewc-define-data-accessors rau--output)
(ewc-define-data-accessors rau--ls-output)
(ewc-define-data-accessors rau--window)
(ewc-define-data-accessors rau--seat)
(ewc-define-data-accessors rau--binding)

;;; Window tags

(defconst rau--tag-outputframe :rau-frame
  "Tag for `river-window-v1' objects that are Emacs frames.")

(defconst rau--tag-external :rau-external
  "Tag for `river-window-v1' objects that are external windows.")



(defvar rau--manage-timer nil
  "Timer used to coalesce `manage-dirty' requests.")

(cl-defmacro rau--do (tag obj state &body body)
  "Iterate over the ewc objects in STATE tagged with TAG.
TAG is a form that evaluates to or is an ewc-object tag, for example
`rau--tag-external', `rau--tag-outputframe', or `'river-output-v1'.

STATE is evaluated once and bound to that same name within BODY."
  (declare (indent 1))
  (unless (symbolp state)
    (error "rau--do: STATE slot must be a symbol, got: %S" state))
  `(let ((,state ,state))
     (dolist (,obj (ewc-objects (rau--state-client ,state) ,tag))
         ,@body)))

(defun rau--make-outputframe-parameters ()
  "Return alist of frame parameters with unique name as expected by title
event handler."
  `((name . ,(make-temp-name "rau-frame-"))
    (undecorated . t)
    ;; avoid showing the same rau buffer twice
    (buffer-predicate . rau--buffer-predicate)))

;; TODO: rename to external-wl
(defun rau--buffer-for-window-wl (window-wl)
  "Return Emacs buffer associated with WINDOW-WL."
  (when-let* ((role-data (rau--window-wl-role-data window-wl)))
    (rau--external-buffer role-data)))

(defun rau--window-wl-for-emacs-window (emacs-window)
  "Return window-wl for any EMACS-WINDOW rau-mode or not.
For a window with a rau-mode buffer return window-wl pointing to an
external window. For all other buffers return the window-wl of the emacs
frame."
  (if-let* ((buffer (window-buffer emacs-window))
            (window-wl (buffer-local-value 'rau--window-wl buffer)))
      window-wl
    (let ((emacs-frame (window-frame emacs-window)))
      (frame-parameter emacs-frame 'rau-frame-wl))))

(defun rau--emacs-window-for-window-wl (window-wl)
  "Return Emacs window associated with WINDOW-WL."
  (when-let* ((buffer (rau--buffer-for-window-wl window-wl)))
    (get-buffer-window buffer 'visible)))

(defun rau--make-buffer-name (app-id title)
  "Return a buffer name string using APP-ID and TITLE."
  (let ((title-trunc (if (> (length title) 40)
                         (format "%s…" (substring title 0 40))
                       title)))
    (if app-id (concat title-trunc " - " app-id)
      title-trunc)))

;; Major mode for rau-managed buffers
(defvar-local rau--window-wl nil
  "Window object for this `rau-mode' buffer.")


;; TODO: move to handler section
(defun rau--buffer-killed ()
  "Request closing of the associated Wayland window when a rau buffer is killed."
  (when-let* ((rau--window-wl)
              (role-data (rau--window-wl-role-data rau--window-wl))
              ;; avoid sending close request in response to closed event
              ((eq 'active (rau--external-state role-data))))
    (setf (rau--external-state role-data) 'killed)
    (rau--mark-manage-dirty rau--state)))

(define-derived-mode rau-mode special-mode "Rau"
  "Major mode for buffers representing windows managed by rau."
  :group 'rau
  (setq-local buffer-read-only t)
  (add-hook 'kill-buffer-hook #'rau--buffer-killed nil t)
  (scroll-bar-mode 0)
  (setq-local left-fringe-width 0
              right-fringe-width 0))

(defun rau--focus-change-allowed-p ()
  "Non-nil when no interactive command or edit is in progress."
  (and (not this-command)
       (length= unread-command-events 0)
       (length= (this-single-command-keys) 0)
       (zerop (minibuffer-depth))
       (zerop (recursion-depth))))

;; TODO move to hook handlers section
(defun rau--update-focus-request (&rest args)
  "Reconcile Wayland focus with the selected window."
  (rau--log "update-focus-request %S" args)
  (when-let* (((rau--focus-change-allowed-p))
              (state rau--state)
              ((null (rau--state-focus-inhibit-update state)))
              (emacs-window (selected-window))
              (target-wl (rau--window-wl-for-emacs-window emacs-window))
              (target-id (ewc-object-id target-wl))
              ((not (eq target-id (rau--state-focus-last-id state)))))

    (setf (rau--state-focus-next-id state) target-id)
    (rau--mark-manage-dirty state)))

(defconst rau--modifier-bits
  '((shift   . 1)
    (control . 4)
    (meta    . 8)
    (super   . 64)
    (hyper   . 128))
  "Modifier bits as per river_seat_v1.modifiers / XKB.")

(defun rau--key-to-xkb (key-string)
  "Decompose KEY-STRING into (EVENT KEY MODIFIERS)."
  (let* ((event (aref (kbd key-string) 0))
         (basic (event-basic-type event))
         (mods (seq-keep (lambda (mod)
                           (alist-get mod rau--modifier-bits))
                         (event-modifiers event)))
         (key (if (characterp basic) basic (symbol-name basic))))
    (list event key (apply #'logior mods))))

(defun rau-push-intercept-prefix (prefix)
  "Register PREFIX as an intercept key binding.
PREFIX is a key string suitable for `kbd'."
  (declare (obsolete 'rau-bind-keys "2026-09-04"))
  (display-warning 'rau
                   "Keybinding in rau has been refactored. Use rau-bind-keys."
                   :warning)

  (let* ((data (rau--key-to-xkb prefix))
         (event (nth 0 data))
         (key (nth 1 data))
         (modifiers (nth 2 data))
         (keysym (rau--resolve-keysym key))
         (binding-key (cons keysym modifiers)))
    (if (= keysym 0)
        (message "rau: could not resolve XKB keysym for %S" key)
      (let ((existing (gethash binding-key (rau--state-bindings rau--state))))
        (if existing
            (setf (rau--binding-event existing) event)
          (puthash binding-key
                   (rau--binding-make
                    :keysym keysym
                    :modifiers modifiers
                    :event event
                    :state 'requested)
                   (rau--state-bindings rau--state))))
      (rau--mark-manage-dirty rau--state))))

(defcustom rau-intercept-prefixes
  '("C-x" "C-u" "C-h" "M-x")
  "Obsolete. Use rau-bind-keys"
  :type '(repeat key)
  :set (lambda (sym val)
         (set-default sym val)
         (when (and (boundp 'rau--state)
                    rau--state
                    (fboundp 'rau-push-intercept-prefixes))
           (rau-push-intercept-prefixes))))

(defun rau-push-intercept-prefixes () ;; TODO: remove, leave only one way?
  "Update the intercept prefixes defined in `rau-intercept-prefixes'."
  (declare (obsolete 'rau-bind-keys "2026-09-04"))
  (dolist (prefix rau-intercept-prefixes)
    (rau-push-intercept-prefix prefix)))

(defun rau--buffer-predicate (buffer)
  "Buffer predicate to avoid accidentally showing the same rau BUFFER twice."
  (or (not (with-current-buffer buffer (derived-mode-p 'rau-mode)))
      (not (get-buffer-window buffer t))))

(defun rau--split-window-advice (new-window)
  "Advice window splits to always display another buffer."
  (with-selected-window new-window
    (with-current-buffer (window-buffer)
      (when (derived-mode-p 'rau-mode)
        (switch-to-buffer (other-buffer)))))
  new-window)

(defun rau--set-window-buffer-advice (orig win buf &rest r)
  "Avoid double-display of rau buffers, by stealing them.
Note that displaying the same buffer in two different tabs, for example,
is completely valid."
  (with-current-buffer buf
    (when (derived-mode-p 'rau-mode)
      (dolist (other (get-buffer-window-list buf nil 'visible))
        (unless (eq (or win (selected-window)) other)
          (with-selected-window other
            (switch-to-buffer (other-buffer)))))))
  (apply orig win buf r))

(defun rau--log (&rest args)
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
                  (error "Rau: cannot locate rau.el; add its directory to `load-path'")))))
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

(defun rau--remove (object-wl)
  "Remove object from ewc objects table.
See also `rau-on-wl-display-delete-id'."
  (ewc-object-remove (rau--state-client rau--state) object-wl))

(defun rau--request (object-wl request &optional arguments nocache)
  "Send REQUEST on OBJECT-WL using the current rau Wayland client."
  (ewc-request (rau--state-client rau--state)
               object-wl
               request
               arguments
               nocache))

(defun rau--frame-wl-by-cond (state predicate)
  "Return the first frame ewc-object in STATE matching PREDICATE."
  (cl-loop for frame-wl in (ewc-objects (rau--state-client state) rau--tag-outputframe)
           for f = (rau--window-wl-role-data frame-wl)
           thereis (and f (funcall predicate f) frame-wl)))

(defun rau--frame-wl-for-window-wl (window-wl)
  "Return the ewc frame object displaying WINDOW-WL."
  (when-let* ((emacs-window (rau--emacs-window-for-window-wl window-wl))
              (emacs-frame (window-frame emacs-window))
              ((frame-live-p emacs-frame)))
    (frame-parameter emacs-frame 'rau-frame-wl)))

(defun rau--frame-wl-without-output (state)
  "Return a frame not associated to any output or nil."
  (rau--frame-wl-by-cond state (lambda (f) (null (rau--outputframe-output-wl f)))))

(defun rau--dimensions-for-emacs-window (emacs-window)
  (pcase-let ((`(,left ,top ,right ,bottom)
               (window-inside-absolute-pixel-edges emacs-window)))
    `(,(- right left) .,(- bottom top))))

(defun rau--dimensions-for-outputframe (output-wl)
  "Get dimensions either from output-wl or its associated ls-output-wl non-exclusive-area."
  (if-let* ((ls-output-wl (rau--output-wl-ls-output-wl output-wl))
            (non-excl-dimensions (rau--ls-output-wl-non-excl-dimensions ls-output-wl))
            ((not (equal '(0 . 0) non-excl-dimensions))))
      non-excl-dimensions
    (rau--output-wl-dimensions output-wl)))

(defun rau--position-for-outputframe (output-wl)
  "Get position either from output-wl or its associated ls-output-wl non-exclusive-area."
  (if-let* ((ls-output-wl (rau--output-wl-ls-output-wl output-wl))
            (non-excl-position (rau--ls-output-wl-non-excl-position ls-output-wl))
            ((not (equal '(0 . 0) non-excl-position))))
      non-excl-position
    (rau--output-wl-position output-wl)))

;;; task queue
(defun rau--tasks-execute ()
  "Execute tasks enqueued by event listeners."
  (let ((tasks (nreverse (rau--state-task-queue rau--state))))
    (setf (rau--state-task-queue rau--state) nil)
    (while-let ((task (pop tasks)))
      (let ((fn (car task))
            (args (cdr task)))
        (rau--condition-case
         (format "task %s" fn)
         (apply fn args))))))

(defun rau--tasks-schedule-execution ()
  "Schedule task processing if not already scheduled."
  (unless (rau--state-task-timer rau--state)
    (setf (rau--state-task-timer rau--state)
          (run-at-time
           0 nil
           (lambda ()
             (setf (rau--state-task-timer rau--state) nil)
             (rau--tasks-execute))))))

(defun rau--tasks-enqueue (fn &rest args)
  "Queue FN with ARGS for execution by rau--tasks-execute.
Also schedule the task execution timer if not yet done so.  Only to be
used in event listeners."
  (push `(,fn . ,args) (rau--state-task-queue rau--state))
  (rau--tasks-schedule-execution))

(defun rau--tasks-enqueue-after-manage (fn &rest args)
  "Queue FN with ARGS to run after the next manage sequence."
  (push `(,fn . ,args) (rau--state-task-queue-after-manage rau--state)))


(defun rau--task-manage-start (wm-wl)
  (unwind-protect
      (rau--reconcile rau--state)
    (rau--request wm-wl 'manage-finish)
    (let ((after-manage (rau--state-task-queue-after-manage rau--state))
          (tasks (rau--state-task-queue rau--state)))
      (setf (rau--state-task-queue-after-manage rau--state) nil
            (rau--state-task-queue rau--state)
            (append after-manage tasks)))
    (when (rau--state-task-queue rau--state)
      (rau--tasks-schedule-execution))))

(defun rau--task-rename-buffer (window-wl)
  "Rename buffer for external window with data taken from
WINDOW-WL."
  (when-let* ((buffer (rau--buffer-for-window-wl window-wl))
              (app-id (rau--window-wl-app-id window-wl))
              (title (rau--window-wl-title window-wl))
              (name (rau--make-buffer-name app-id title)))
    (with-current-buffer buffer
      (rename-buffer name t))))

(defun rau--task-setup-new-external-window (window-wl)
  "Create Rau mode buffer for WINDOW-WL."
  (let ((role-data (rau--window-wl-role-data window-wl))
        (buffer (get-buffer-create (make-temp-name "rau-external-"))))
    (with-current-buffer buffer
      (rau-mode)
      (setq-local rau--window-wl window-wl)
      (unless (display-buffer buffer)
        (error "display-buffer failed for window-wl %S buffer %S." window-wl buffer)))
    (setf (rau--external-buffer role-data) buffer)

    (setf (rau--external-state role-data) 'active)))

(defun rau--task-consume-key-event (event needs-focus)
  "Forward EVENT to emacs and setup to recover focus if NEEDS-FOCUS."
  (push (cons t event) unread-command-events)
  (when needs-focus
    (add-hook 'post-command-hook #'rau--recover-focus-after-binding-pressed)))

;;; Manage requests queue
(defun rau--manage-enqueue (ewc-object request &optional args)
  (push `(,ewc-object ,request ,args) (rau--state-manage-queue rau--state))
  (rau--mark-manage-dirty rau--state))

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
                 (when-let* ((client (rau--state-client state))
                             (wm-wl (ewc-first-object client
                                                   'river-window-manager-v1)))
                   (rau--request wm-wl 'manage-dirty))
               (error
                (message "rau manage-dirty error: %S" err))))
           state))))

;;; Fullscreen helpers

(defun rau--fs-window (fs)
  "Return the window involved in fullscreen state FS, if any."
  (pcase (rau--fs-state fs)
    ('requested (rau--fs-new fs))
    ((or 'fullscreen 'exiting) (rau--fs-window fs))
    (_ nil)))

;;; Keybindings

(defun rau--parse-keys (keys)
  "Parse KEYS into a list of rau--binding structs. See `rau-bind-keys'."
  (let ((seen (make-hash-table :test #'equal))
        (result nil))
    (dolist (item keys)
      (let ((elements (if (stringp item) (list item) item))
            keys flags)
        ;; Walk the element list, separating keys from flags.
        ;; TODO use while-let
        (while-let ((elt (pop elements)))
          (cond
           ((stringp elt)
            (push elt keys))
           ((eq :layout elt)
            (unless elements
              (error ":layout requires a layout number"))
            (push (cons elt (pop elements)) flags))
           ((memq elt '(:needs-focus :locked-active))
            (push (cons elt t) flags))
           ((keywordp elt)
            (error "unknown flag %S" elt))
           (t
            (error "Unexpected element in key binding spec: %S" elt))))
        (dolist (key keys)
          (when (gethash key seen)
            (error "Duplicate key binding: %s" key))
          (puthash key t seen)
          (let ((xkb (rau--key-to-xkb key)))
            (push (rau--binding-make
                   :event (cl-first xkb)
                   :key key
                   :keysym (rau--resolve-keysym (cl-second xkb))
                   :modifiers (cl-third xkb)
                   :needs-focus   (cdr (assq :needs-focus flags))
                   :locked-active (cdr (assq :locked-active flags))
                   :layout        (cdr (assq :layout flags)))
                  result)))))
    (nreverse result)))

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

(defun rau--bind-parsed-keys (parsed-keys)
  "Actually bind PARSED-KEYS.  See `rau-bind-keys' for the public function."
  (when-let* ((client (rau--state-client rau--state))
              (xkb-bindings-wl (ewc-first-object client 'river-xkb-bindings-v1))
              (seat-wl (ewc-first-object client 'river-seat-v1))
              (seat-id (ewc-object-id seat-wl)))
    ;; TODO: check for duplicates in ewc-objects
    (dolist (binding parsed-keys)
      (let ((binding-wl (ewc-object-add client 'river-xkb-binding-v1)))
        (setf (ewc-object-data binding-wl) binding)
        (rau--request xkb-bindings-wl 'get-xkb-binding
                          `((seat . ,seat-id)
                            (keysym . ,(rau--binding-keysym binding))
                            (modifiers . ,(rau--binding-modifiers binding))
                            (id . ,(ewc-object-id binding-wl))))
        (when-let* ((layout (rau--binding-layout binding)))
          (rau--manage-enqueue
           binding-wl
           'set-layout-override
           `((layout . ,layout))))
        (rau--manage-enqueue
         binding-wl
         'enable)))))

(defun rau-bind-keys (keys)
  "Bind KEYS to always be sent to Emacs.  KEYS is a list either of strings
acceptable to `kbd' or of lists of strings and one or more of the
following keywords:

- :locked-active - keys are also active when the session is locked, e.g. Volume or Brighness control
- :needs-focus - focus should be temporarily given to Emacs
- :layout LAYOUT-NR - 0 indexed layout override

Example:

'(\"s-e\" \"s-f\"
  (\"s-d\" \"C-h\" :needs-focus)
  (:locked-active \"M-x\" :needs-focus)
  (:locked-active \"s-c\" \"s-z\" :layout 2))

This function should be run from the `rau-ready-hook'."
  (unless rau--state
    (error "Rau has not yet been started."))
  (let ((parsed-keys (rau--parse-keys keys)))
    (rau--bind-parsed-keys parsed-keys)))

;;; Layer shell attachment helpers

(defun rau--ensure-ls-output (state output-wl)
  "Create a layer-shell output object for OUTPUT-WL if possible."
  (when-let* ((out (ewc-object-data output-wl))
              ((null (rau--output-ls-output-wl out)))
              (client (rau--state-client state))
              (ls-wl (ewc-first-object client 'river-layer-shell-v1))
              (ls-output-wl
               (ewc-object-add client 'river-layer-shell-output-v1)))
    (setf (rau--output-ls-output-wl out) ls-output-wl
          (ewc-object-data ls-output-wl) (rau--ls-output-make :output-wl output-wl))
    (rau--request ls-wl 'get-output
                  `((id . ,(ewc-object-id ls-output-wl))
                    (output . ,(ewc-object-id output-wl))))))

;; TODO: this ls-seat is never used ATM.
(defun rau--ensure-ls-seat (state)
  "Create a layer-shell seat object for the current seat if possible."
  (when-let* ((client (rau--state-client state))
              (seat-wl (ewc-first-object client 'river-seat-v1))
              ((null (rau--seat-wl-ls-seat-wl seat-wl)))
              (ls-wl (ewc-first-object client 'river-layer-shell-v1))
              (ls-seat-id (cl-incf (ewc-client-new-id client)))
              (ls-seat-wl
               (ewc-object-add client
                               'river-layer-shell-seat-v1
                               ls-seat-id)))
    (setf (rau--seat-wl-ls-seat-wl seat-wl) ls-seat-wl)
    (rau--request ls-wl 'get-seat
                   `((id . ,ls-seat-id)
                     (seat . ,(ewc-object-id seat-wl))))
    (rau--tasks-enqueue #'run-hooks 'rau-ready-hook)))

;;; Emacs handler functions for hooks

(defun rau--recover-focus-after-binding-pressed ()
  "Give focus back to external window after it was given to Emacs to handle
a keybinding pressed event. This function is meant to be bound to
post-command-hook in the enqueued command of the pressed event handler."
  (rau--log "recover focus. not t-c=%S u-c-e=%d t-s-c-k=%d m-d=%d r-d=%d"
           (not this-command)
           (length unread-command-events)
           (length (this-single-command-keys))
           (minibuffer-depth)
           (recursion-depth))
  (when (and (length= unread-command-events 0)
             (zerop (minibuffer-depth))
             (zerop (recursion-depth)))
    (rau--log "recover focus. removing post-command-hook.")
    (remove-hook 'post-command-hook #'rau--recover-focus-after-binding-pressed)
    (when-let* ((window-wl (buffer-local-value 'rau--window-wl (current-buffer)))
                (window-id (ewc-object-id window-wl))
                ((/= window-id (rau--state-focus-last-id rau--state))))
      (rau--log "recover focus. focusing window-id=%d title=%s"
               window-id
               (rau--window-wl-title window-wl))
      (setf (rau--state-focus-next-id rau--state) window-id)
      (rau--mark-manage-dirty rau--state))))

(defun rau--window-configuration-change-handler ()
  "Schedule a river manage cycle and thus a reconciliation cycle.
This is necessary for external windows to resize when the minibuffer
expands.  This needs to be added to the global hook since local hooks
don't get called for windows that disappear.  Also
window-size-change-functions does not get called when minibuffer expands
and thus minibuffer ends up below external window."
  (rau--mark-manage-dirty rau--state))

;;; Listeners

;; Order event listeners by their order in the protocol definitions!

;;;; wayland protocol
;;;; wl-display listeners
(defun rau--on-wl-display-error (_display-wl args)
  (pcase-let (((map object-id code message) args))
    (message "wl_display error: object-id=%d code=%d message=%s"
             object-id
             code
             (ewc-to-utf8 message))
    (when-let* ((client (rau--state-client rau--state))
                (object (ewc-object-get client object-id)))
      (message "object id=%d interface=%s tags=%S"
               object-id
               (ewc-object-interface object)
               (ewc-object-tags object)))))

(defun rau--on-wl-display-delete-id (_display-wl args)
  "Server acknowledges deletion of object created by client.
Note that server does not send this event for objects created by
server (e.g. window, output). Thus object removal must be done at the
point where also the destroy request is sent."
  (pcase-let (((map id) args)
              (client (rau--state-client rau--state)))
    (ewc-object-remove-id client id)))

;;;; wl-registry listeners
(defun rau--on-wl-registry-global (registry-wl args)
  (pcase-let (((map name interface version) args))
    (when-let* ((ifsym (intern (string-replace "_" "-" interface)))
                (client (rau--state-client rau--state))
                ((member interface rau--global-binds))
                (new-id (cl-incf (ewc-client-new-id client)))
                (xml-version (ewc-interface-version client ifsym))
                (bind-version
                 (if xml-version (min version xml-version) version)))
      (ewc-object-add client ifsym new-id)
      (rau--log "rau: binding global %s version %s" interface bind-version)
      (rau--request registry-wl 'bind
                     `((name . ,name)
                       (interface-len . ,(1+ (string-bytes interface)))
                       (interface . ,interface)
                       (version . ,bind-version)
                       (id . ,new-id)))
      (pcase ifsym
        ('river-layer-shell-v1
         ;; Attach layer-shell objects to existing outputs/seats in STATE."
         (rau--do 'river-output-v1 output-wl rau--state
                   (rau--ensure-ls-output rau--state output-wl))
         (rau--ensure-ls-seat rau--state))

        (_ (rau--log "rau: bound %s" ifsym))))))

;;;; river-window-management-v1 Protocol
;;;; river-window-manager-v1 listeners
(defun rau--on-river-window-manager-v1-unavailable (_wm-wl _)
  (message "rau: WM event unavailable"))

(defun rau--on-river-window-manager-v1-finished (_wm-wl _)
  (message "rau: WM event finished"))

(defun rau--on-river-window-manager-v1-manage-start (wm-wl _)
  (rau--tasks-enqueue #'rau--task-manage-start wm-wl))

(defun rau--on-river-window-manager-v1-render-start (wm-wl _)
  (rau--tasks-enqueue #'rau--render-frames)
  (rau--tasks-enqueue #'rau--render-windows)
  (rau--tasks-enqueue #'rau--request wm-wl 'render-finish))

(defun rau--on-river-window-manager-v1-session-locked (_wm-wl _)
  (setf (rau--state-session-locked rau--state) t))

(defun rau--on-river-window-manager-v1-session-unlocked (_wm-wl _)
  (setf (rau--state-focus-next-id rau--state) (rau--state-focus-last-id rau--state)
        (rau--state-focus-last-id rau--state) -1
        (rau--state-session-locked rau--state) nil))

(defun rau--on-river-window-manager-v1-window (_wm-wl args)
  (pcase-let* (((map id) args)
               (client (rau--state-client rau--state))
               (window-wl (ewc-object-add client
                                          'river-window-v1
                                          id))
               (node-wl (ewc-object-add client 'river-node-v1))
               (window-data (rau--window-make :node-wl node-wl)))
    (setf (ewc-object-data window-wl) window-data)
    (rau--request window-wl 'get-node
                  `((id . ,(ewc-object-id node-wl))))))

(defun rau--on-river-window-manager-v1-output (_wm-wl args)
  (pcase-let* (((map id) args)
               (client (rau--state-client rau--state))
               (output-wl (ewc-object-add client 'river-output-v1 id)))
    (setf (ewc-object-data output-wl)
          (rau--output-make))
    (rau--ensure-ls-output rau--state output-wl)))

(defun rau--on-river-window-manager-v1-seat (_wm-wl args)
  (pcase-let (((map id) args)
              (client (rau--state-client rau--state)))
    (if (ewc-first-object client 'river-seat-v1)
        (message "rau does not support multi-seat")
      (let* ((seat-wl (ewc-object-add client 'river-seat-v1 id)))
        (setf (ewc-object-data seat-wl) (rau--seat-make))
        (rau--ensure-ls-seat rau--state)))))

;;;; river-window-v1 listeners
(defun rau--on-river-window-v1-closed (window-wl _)
  (when-let* (((ewc-object-tagged-p window-wl rau--tag-external))
              (buf (rau--buffer-for-window-wl window-wl)))
    (rau--tasks-enqueue #'kill-buffer buf))
  (when-let* ((node-wl (rau--window-wl-node-wl window-wl)))
    (rau--tasks-enqueue #'rau--request node-wl 'destroy))
  (rau--tasks-enqueue #'rau--request window-wl 'destroy)
  (rau--tasks-enqueue #'rau--remove window-wl)

  (when-let* (((ewc-object-tagged-p window-wl rau--tag-outputframe))
              (role-data (rau--window-wl-role-data window-wl))
              (output-wl (rau--outputframe-output-wl role-data)))
    (setf (rau--outputframe-output-wl role-data) nil
          (rau--output-wl-frame-wl output-wl) nil))

  ;; Reset fullscreen on output if window was fullscreen.
  (when (ewc-object-tagged-p window-wl rau--tag-external)
    (rau--do 'river-output-v1 output-wl rau--state
             (when (eq (rau--fs-window (rau--output-wl-fullscreen output-wl)) window-wl)
               (setf (rau--output-wl-fullscreen output-wl) (rau--fs))))))

(defun rau--on-river-window-v1-dimensions (window-wl args)
  (pcase-let (((map width height) args))
    (setf (rau--window-wl-actual-dimensions window-wl) `(,width . ,height))))

(defun rau--on-river-window-v1-app-id (window-wl args)
  (pcase-let (((map app-id) args))
    (when-let* ((app-id (ewc-to-utf8 app-id)))
      (setf (rau--window-wl-app-id window-wl) app-id))))

(defun rau--on-river-window-v1-title (window-wl args)
  "Handle title event for WINDOW-WL.
Also categorizes the window based on the title prefix into Emacs
outputframe or external window."
  (pcase-let* (((map title) args)
               (title (ewc-to-utf8 title))
               (client (rau--state-client rau--state)))
    (setf (rau--window-wl-title window-wl) title)

    (if (string-prefix-p "rau-frame-" title)
        ;; Categorize as Emacs output frame
        (unless (ewc-object-tagged-p window-wl rau--tag-outputframe)
          (rau--log "Discovered new Emacs frame by title: %s" title)
          (setf (rau--window-wl-role-data window-wl) (rau--outputframe-make))
          (ewc-object-tag client window-wl rau--tag-outputframe)
          (if (> (rau--state-pending-frames rau--state) 0)
              (cl-decf (rau--state-pending-frames rau--state))
            (rau--log "New frame was not requested by WM")))
      ;; Categorize as external window
      (unless (ewc-object-tagged-p window-wl rau--tag-external)
        (rau--log "Discovered new regular external window")
        (setf (rau--window-wl-role-data window-wl) (rau--external-make))
        (ewc-object-tag client window-wl rau--tag-external)
        (unless (rau--buffer-for-window-wl window-wl)
          (rau--tasks-enqueue #'rau--task-setup-new-external-window window-wl))))

    ;; Handle title updates for already categorized objects
    (when-let* (((ewc-object-tagged-p window-wl rau--tag-external)))
      (rau--tasks-enqueue #'rau--task-rename-buffer window-wl))

    (when-let* (((ewc-object-tagged-p window-wl rau--tag-outputframe))
                (role-data (rau--window-wl-role-data window-wl))
                ((not (rau--outputframe-emacs-frame role-data)))
                (emacs-frame
                 (cl-find title (frame-list)
                          :test #'equal
                          :key (lambda (f) (frame-parameter f 'name)))))
      (setf (rau--outputframe-emacs-frame role-data) emacs-frame)
      (rau--tasks-enqueue #'set-frame-parameter emacs-frame 'rau-frame-wl window-wl))))

(defun rau--on-river-window-v1-fullscreen-requested (window-wl args)
  (pcase-let* (((map output) args)
               (output-wl
                ;; Find output for fullscreen window
                ;; 1. optional event arg output
                ;; 2. output showing window-wl
                ;; 3. output currently having focusc
                (or (and (integerp output)
                         (not (zerop output))
                         (ewc-object-get (rau--state-client rau--state) output))
                    (when-let* (((ewc-object-tagged-p window-wl rau--tag-external))
                                (frame-wl (rau--frame-wl-for-window-wl window-wl))
                                (role-data (rau--window-wl-role-data frame-wl)))
                      (rau--outputframe-output-wl role-data))
                    (when-let* ((frame-wl (frame-parameter (selected-frame) 'rau-frame-wl))
                                (role-data (rau--window-wl-role-data frame-wl)))
                      (rau--outputframe-output-wl role-data)))))
    (if (not output-wl)
        (message "Fullscreen requested, but no output found")
      (let* ((fs (rau--output-wl-fullscreen output-wl))
             (previous
              (pcase (rau--fs-state fs)
                ((or 'fullscreen 'exiting)
                 (rau--fs-window fs))
                (_ nil))))
        (setf (rau--output-wl-fullscreen output-wl)
              (rau--fs :state 'requested
                       :new window-wl
                       :previous previous))))))

(defun rau--on-river-window-v1-exit-fullscreen-requested (window-wl _)
  (rau--do 'river-output-v1 output-wl rau--state
           (let ((fs (rau--output-wl-fullscreen output-wl)))
             (when (and (member (rau--fs-state fs)
                                '(requested fullscreen))
                        (eq (rau--fs-window fs) window-wl))
               (setf (rau--output-wl-fullscreen output-wl)
                     (rau--fs :state 'exiting
                             :window (rau--fs-window fs)))))))

(defun rau--on-river-window-v1-minimize-requested (window-wl _)
  (when-let* (((ewc-object-tagged-p window-wl rau--tag-external))
              (buffer (rau--buffer-for-window-wl window-wl)))
    (rau--tasks-enqueue #'bury-buffer buffer)))

(defun rau--on-river-window-v1-unreliable-pid (window-wl args)
  (pcase-let* (((map unreliable-pid) args))
    (setf (rau--window-wl-pid window-wl) unreliable-pid)))

;;;; river-output-v1 listeners
(defun rau--on-river-output-v1-removed (output-wl _)
  ;; TODO: check whether we lost focus
  (let ((client (rau--state-client rau--state)))
    (when-let* ((frame-wl (rau--output-wl-frame-wl output-wl))
                (role-data (rau--window-wl-role-data frame-wl)))
      (setf (rau--outputframe-output-wl role-data) nil
            (rau--output-wl-frame-wl output-wl) nil)
      (when-let* ((emacs-frame (rau--outputframe-emacs-frame role-data))
                  ((frame-live-p emacs-frame)))
        (rau--tasks-enqueue #'delete-frame emacs-frame)))
    ;; TODO: also enqueue the below three actions, look out for race conditions
    (when-let* ((ls-output-wl (rau--output-wl-ls-output-wl output-wl)))
      (rau--request ls-output-wl 'destroy))
    (rau--request output-wl 'destroy)
    (ewc-object-remove client output-wl)))

;; TODO: listener for wl_output, e.g. to get monitor names

(defun rau--on-river-output-v1-position (output-wl args)
  (pcase-let (((map x y) args))
    (setf (rau--output-wl-position output-wl) `(,x . ,y))))

(defun rau--on-river-output-v1-dimensions (output-wl args)
  (pcase-let (((map width height) args))
    (setf (rau--output-wl-dimensions output-wl) `(,width . ,height))))

;;;; river-seat-v1 listener
(defun rau--on-river-seat-v1-window-interaction (_seat-wl args)
  (pcase-let* (((map window) args))
    (rau--log "last focused window-wl id: %d" (rau--state-focus-last-id rau--state))
    (unless (equal window (rau--state-focus-last-id rau--state))
      (rau--log "window interaction with %d" window)
      (setf (rau--state-focus-next-id rau--state) window))))

;;;; river-xkb-bindings-v1 protocol
;;;; river-xkb-binding-v1 listeners
(defun rau--on-river-xkb-binding-v1-pressed (binding-wl _)
  (unless (and (rau--state-session-locked rau--state)
               (not (rau--binding-wl-locked-active binding-wl)))
    (let* ((event (rau--binding-wl-event binding-wl))
           (needs-focus (rau--binding-wl-needs-focus binding-wl)))
      (rau--tasks-enqueue-after-manage #'rau--task-consume-key-event event needs-focus)

      ;; If focus is with external window then switch to underlying emacs
      ;; frame such that following keypresses go to emacs
      (when-let* (needs-focus
                  (window-id (rau--state-focus-last-id rau--state))
                  (client (rau--state-client rau--state))
                  (window-wl (ewc-object-get client window-id))
                  ((ewc-object-tagged-p window-wl rau--tag-external))
                  (target-wl (rau--frame-wl-for-window-wl window-wl))
                  (target-id (ewc-object-id target-wl)))
        (rau--log "switch focus to emacs frame for key pressed.")
        (setf (rau--state-focus-next-id rau--state) target-id)))))

;;;; river-layer-shell-v1 protocol
;;;; river-layer-shell-output-v1 listeners
(defun rau--on-river-layer-shell-output-v1-non-exclusive-area (ls-output-wl args)
  (pcase-let (((map x y width height) args))
    (setf (rau--ls-output-wl-non-excl-position ls-output-wl) `(,x . ,y)
          (rau--ls-output-wl-non-excl-dimensions ls-output-wl) `(,width . ,height))))

;;;; river-layer-shell-seat-v1 listeners
(defun rau--on-river-layer-shell-seat-v1-focus-none (_ls-seat-wl _)
  "Give focus back to the last window that had it."
  (setf (rau--state-focus-next-id rau--state) (rau--state-focus-last-id rau--state)
        (rau--state-focus-last-id rau--state) -1))

;;; Reconciliation

(defun rau--reconcile-frames (state)
  "Ensure each output gets one maximized Emacs frame."
  (let ((frame-requests 0))
    (rau--do 'river-output-v1 output-wl state
             (rau--log "reconcile output: id=%d." (ewc-object-id output-wl))
             (if-let* ((frame-wl (rau--output-wl-frame-wl output-wl)))
                 (let ((dimensions (rau--dimensions-for-outputframe output-wl)))
                   (rau--log "frame found: id=%d." (ewc-object-id frame-wl))
                   (rau--request frame-wl
                                 'propose-dimensions
                                 `((width . ,(car dimensions))
                                   (height . ,(cdr dimensions)))))

               (rau--log "no frame found for output.")
               (if-let* ((frame-wl (rau--frame-wl-without-output state))
                         (role-data (rau--window-wl-role-data frame-wl)))
                   (let ((dimensions (rau--dimensions-for-outputframe output-wl)))
                      (setf (rau--outputframe-output-wl role-data) output-wl
                            (rau--output-wl-frame-wl output-wl) frame-wl)
                      (rau--request frame-wl
                                     'propose-dimensions
                                     `((width . ,(car dimensions))
                                       (height . ,(cdr dimensions))))
                      (rau--request frame-wl
                                     'inform-maximized)
                      (rau--request frame-wl
                                     'set-tiled
                                     `((edges . ,rau--edges-all))))
                  ;; No frame on this output yet: request one.
                  (rau--log "request frame.")
                  (cl-incf frame-requests))))
    (dotimes (_ (- frame-requests (rau--state-pending-frames state)))
      (rau--tasks-enqueue #'make-frame (rau--make-outputframe-parameters))
      (cl-incf (rau--state-pending-frames state)))))

(defun rau--reconcile-windows (state)
  "Close killed windows and propose dimensions for active windows."
  (rau--do rau--tag-external window-wl state
           (pcase (rau--external-state (rau--window-wl-role-data window-wl))
             ;; nothing to do for window-state 'starting
             ('active
              (when-let* ((emacs-window (rau--emacs-window-for-window-wl window-wl))
                          (dimensions (rau--dimensions-for-emacs-window emacs-window)))
                (rau--request window-wl
                              'set-tiled
                              `((edges . ,rau--edges-all)))
                (rau--request window-wl
                              'propose-dimensions
                              `((width . ,(car dimensions))
                                (height . ,(cdr dimensions))))))
             ('killed
              (rau--request window-wl 'close)))))

(defun rau--reconcile-bindings (state)
  "Create and enable XKB bindings."
  (when-let* ((client (rau--state-client state))
              (xkb-bindings-wl (ewc-first-object client 'river-xkb-bindings-v1))
              (seat-wl (ewc-first-object client 'river-seat-v1)))
    (maphash
     (lambda (_key binding)
       (when (eq (rau--binding-state binding) 'requested)
         (let* ((id (cl-incf (ewc-client-new-id client)))
                (binding-wl (ewc-object-add client 'river-xkb-binding-v1 id)))
           (rau--request xkb-bindings-wl 'get-xkb-binding
                          `((seat . ,(ewc-object-id seat-wl))
                            (keysym . ,(rau--binding-keysym binding))
                            (modifiers . ,(rau--binding-modifiers binding))
                            (id . ,id)))
           (setf (ewc-object-data binding-wl) binding
                 (rau--binding-state binding) 'registered)
           (rau--request binding-wl 'enable)
           (setf (rau--binding-state binding) 'enabled))))
     (rau--state-bindings state))))

(defun rau--reconcile-fullscreen (state)
  "Advance fullscreen state machines."
  (rau--do 'river-output-v1 output-wl state
    (let ((fs (rau--output-wl-fullscreen output-wl)))
      (rau--log "reconcile fs on output %d with fs state %s"
                (ewc-object-id output-wl)
                (rau--fs-state fs))
      (pcase (rau--fs-state fs)
        ('requested
         (let ((new-wl (rau--fs-new fs))
               (prev-wl (rau--fs-previous fs)))
           (when prev-wl
             (rau--request prev-wl 'inform-not-fullscreen)
             (rau--request prev-wl 'exit-fullscreen))
           (when new-wl
             (rau--request new-wl 'inform-fullscreen)
             (rau--request new-wl 'fullscreen
                           `((output . ,(ewc-object-id output-wl)))
                           t))
           (setf (rau--output-wl-fullscreen output-wl)
                 (rau--fs :state 'fullscreen :window new-wl))))
        ('exiting
         (let ((window-wl (rau--fs-window fs)))
           (when (and window-wl (ewc-object-p window-wl))
             (rau--request window-wl 'inform-not-fullscreen)
             (rau--request window-wl 'exit-fullscreen)
             (when-let* ((emacs-window (rau--emacs-window-for-window-wl window-wl))
                         (dimensions (rau--dimensions-for-emacs-window emacs-window)))
               (rau--request window-wl
                             'propose-dimensions
                             `((width . ,(car dimensions))
                               (height . ,(cdr dimensions)))
                             t)))
           (setf (rau--output-wl-fullscreen output-wl) (rau--fs))))
        (_ nil)))))

(defun rau--reconcile-focus (state)
  "Update focus based on either an event or a buffer change.
See also focus relevant slots in rau STATE."
  (when-let* (((/=
                  (rau--state-focus-last-id state)
                  (rau--state-focus-next-id state)))
              ((/= -1 (rau--state-focus-next-id state)))
              (client (rau--state-client state))
              (target-id (rau--state-focus-next-id state))
              (target-wl (ewc-object-get client target-id))
              (seat-wl (ewc-first-object client 'river-seat-v1)))

    (rau--log "request focus-window id=%d title=%s"
             target-id
             (rau--window-wl-title target-wl))
    (rau--request seat-wl
                  'focus-window
                  `((window . ,target-id))
                  t)
    (setf (rau--state-focus-last-id state) target-id
          (rau--state-focus-next-id state) -1)

    (when-let* ((frame-wl (if (ewc-object-tagged-p target-wl rau--tag-external)
                              (rau--frame-wl-for-window-wl target-wl)
                            target-wl))
                (role-data (rau--window-wl-role-data frame-wl))
                (output-wl (rau--outputframe-output-wl role-data))
                (ls-output-wl (rau--output-wl-ls-output-wl output-wl)))
      (rau--request ls-output-wl 'set-default))

    ;; Let Emacs select the underlying emacs-window for the external window
    (when-let* (((ewc-object-tagged-p target-wl rau--tag-external))
                (emacs-window (rau--emacs-window-for-window-wl target-wl)))
      (rau--log "select underlying window")
      (setf (rau--state-focus-inhibit-update state) t)
      (select-window emacs-window 'norecord))
      (setf (rau--state-focus-inhibit-update state) nil)))

(defun rau--reconcile (state)
  "Run the manage-sequence reconciliation for STATE."
  (rau--condition-case
   "reconcile-manage-requests"
   (let ((manage-requests (nreverse (rau--state-manage-queue state))))
     (setf (rau--state-manage-queue state) nil)
     (dolist (request manage-requests)
       (rau--request (cl-first request) (cl-second request) (cl-third request)))))
  (rau--condition-case "reconcile-frames" (rau--reconcile-frames state))
  (rau--condition-case "reconcile-windows" (rau--reconcile-windows state))
  (rau--condition-case "reconcile-bindings" (rau--reconcile-bindings state))
  (rau--condition-case "reconcile-fs" (rau--reconcile-fullscreen state))
  (rau--condition-case "reconcile-focus" (rau--reconcile-focus state)))

(defun rau--render-frames ()
  "Run the render-sequence reconciliation for frames."
  (rau--do rau--tag-outputframe frame-wl rau--state
           (when-let* ((node-wl (rau--window-wl-node-wl frame-wl))
                       (role-data (rau--window-wl-role-data frame-wl))
                       (output-wl (rau--outputframe-output-wl role-data))
                       (position (rau--position-for-outputframe output-wl)))
             (rau--log "render frame %d for output %d."
                      (ewc-object-id frame-wl)
                      (ewc-object-id output-wl))
             (rau--request node-wl 'place-bottom)
             (rau--request node-wl 'set-position
                           `((x . ,(car position))
                             (y . ,(cdr position)))))))

(defun rau--render-windows ()
  "Run the render-sequence reconciliation for windows."
  (rau--do rau--tag-external window-wl rau--state
    (when-let* ((node-wl (rau--window-wl-node-wl window-wl))
                (role-data (rau--window-wl-role-data window-wl))
                ((eq (rau--external-state role-data) 'active)))
      (if-let* ((frame-wl (rau--frame-wl-for-window-wl window-wl))
                (role-data (rau--window-wl-role-data frame-wl))
                (output-wl (rau--outputframe-output-wl role-data))
                (emacs-window (rau--emacs-window-for-window-wl window-wl)))
          (pcase-let* ((`(,left ,top ,right ,bottom)
                        (window-inside-absolute-pixel-edges emacs-window))
                       (position (rau--position-for-outputframe output-wl))
                       (dimensions (rau--window-wl-actual-dimensions window-wl))
                       (clip (or dimensions (rau--dimensions-for-emacs-window emacs-window))))
            (rau--request window-wl 'show)

            (rau--request node-wl 'set-position
                          `((x . ,(+ left (car position)))
                            (y . ,(+ top (cdr position)))))

            (rau--request node-wl 'place-top)

            (rau--request window-wl 'set-clip-box
                          `((x . 0)
                            (y . 0)
                            (width . ,(car clip))
                            (height . ,(cdr clip)))))

        (rau--request window-wl 'hide)))))

;;; Fullscreen toggle

(defun rau-toggle-fullscreen ()
  "Toggle fullscreen for the currently focused external window."
  (interactive)
  (if-let* ((window-wl (buffer-local-value 'rau--window-wl (current-buffer)))
            (frame-wl (rau--frame-wl-for-window-wl window-wl))
            (role-data (rau--window-wl-role-data frame-wl))
            (output-wl (rau--outputframe-output-wl role-data))
            (out (ewc-object-data output-wl))
            (fs (rau--output-fullscreen out)))
      (pcase (rau--fs-state fs)
        ('none
         (setf (rau--output-fullscreen out)
               (rau--fs :state 'requested
                       :new window-wl))
         (rau--mark-manage-dirty rau--state))

        ('fullscreen
         (setf (rau--output-fullscreen out)
               (rau--fs :state 'exiting
                       :window (rau--fs-window fs)))
         (rau--mark-manage-dirty rau--state))

        (_
         (message "Invalid output state for fullscreen toggle")))
    (message "Fullscreen requested, but nothing is focused")))

;;; Startup
;; NOTE: No need for rau-disable since this Emacs process is serving as a
;; Window Manager and disabling rau while keeping the Emacs process running
;; would result in an unresponsive user environment.
;;;###autoload
(defun rau-enable ()
  "Enable the rau window manager for river.
Call this function once when starting Emacs inside of river."
  (when rau--state
    (user-error "Rau is already running"))

  (unless (eq window-system 'pgtk)
    (user-error "Rau requires a pgtk Emacs on Wayland"))

  ;; TODO: this is a hack for lack of ability to figure out alignment ...
  (menu-bar-mode 0)
  (tool-bar-mode 0)

  (advice-add 'split-window-below :filter-return #'rau--split-window-advice)
  (advice-add 'split-window-right :filter-return #'rau--split-window-advice)
  (advice-add 'set-window-buffer :around #'rau--set-window-buffer-advice)

  (message "Launching rau (pure Elisp) ...")
  (unless (= 1 (length (frame-list)))
    (user-error "There should be exactly one frame when starting Rau."))

  (modify-frame-parameters nil (rau--make-outputframe-parameters))

  (let* ((interfaces (rau--read-protocols))
         (client (ewc-start interfaces "rau--on-")))
    (setq rau--state (rau--state-make :client client)))

  (rau-push-intercept-prefixes)

  ;; Layout signals
  (add-hook 'window-configuration-change-hook #'rau--window-configuration-change-handler)

  ;; Focus signals
  (add-hook 'window-selection-change-functions #'rau--update-focus-request)
  (add-hook 'window-buffer-change-functions    #'rau--update-focus-request)
  (add-hook 'minibuffer-setup-hook             #'rau--update-focus-request)
  (add-hook 'minibuffer-exit-hook              #'rau--update-focus-request))

;; TODO Hacks to avoid rau to freeze
(defun x-popup-menu(position menu)
  (message "x-popup-menu does not work with rau and is therefor overwritten.")
  nil)

(defun popup-menu(menu &optional position prefix from-menu-bar)
  (message "popup-menu does not work with rau and is therefor overwritten.")
  nil)

(defun x-popup-dialog(position contents &optional header)
  (message "x-popup-dialog does not work with rau and is therefor overwritten.")
  nil)

(defun display-popup-menus-p (&optional display)
  nil)

(provide 'rau)
;;; rau.el ends here
