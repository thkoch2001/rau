;;; -*- lexical-binding: t -*-
;;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'cl-lib)
(require 'ewc)
(require 'map)          ; needed for `map' pcase pattern
(require 'pcase)
(require 'seq)
(require 'subr-x)

(defgroup reka nil
  "Reka - Emacs swimming in the river"
  :group 'environment
  :prefix "reka-")

(defcustom reka-enable-hook nil
  "Hook run at the end of `reka-enable'."
  :type 'hook)

;;; State structs

(cl-defstruct (reka-window-parameters
               (:constructor reka-window-parameters-make))
  "Window parameters describing where an external surface should be placed."
  window
  frame-name
  x
  y
  w
  h)

(cl-defstruct (reka-output (:constructor reka-output-make))
  "State for a River output."
  proxy
  ls-output
  (x 0)
  (y 0)
  (width 0)
  (height 0)
  (fullscreen 'none))

(cl-defstruct (reka-surface (:constructor nil))
  "Common state shared by windows and frames.
Not instantiated directly; windows and frames include it."
  proxy
  node
  title)

(cl-defstruct (reka-window (:constructor reka-window-make)
                           (:include reka-surface))
  "State for a regular external window."
  (state 'starting) ;; 'active 'killed
  params
  actual-width
  actual-height
  app-id)

(cl-defstruct (reka-frame (:constructor reka-frame-make)
                          (:include reka-surface))
  "State for an Emacs frame managed by reka."
  displayed-on
  proposed-width
  proposed-height
  visible
  last-x
  last-y)

(cl-defstruct (reka-seat (:constructor reka-seat-make))
  "State for a River seat."
  id
  proxy
  ls-seat)

(cl-defstruct (reka-binding (:constructor reka-binding-make))
  "State for one global XKB binding."
  keysym
  modifiers
  command
  event
  (state 'requested)
  proxy)

(cl-defstruct (reka-state (:constructor reka-state-make))
  "Holds the state of the reka Wayland client."
  (connection nil)
  (objects nil :type ewc-objects)
  (pid (emacs-pid))

  ;; Bound globals: alist of interface symbol -> object id.
  (globals nil)

  ;; Current seat.
  (seat nil)

  ;; State tracking.  Hash tables are keyed by Wayland object id.
  (outputs (make-hash-table :test 'eql))
  (windows (make-hash-table :test 'eql))
  (frames (make-hash-table :test 'eql))

  ;; XKB bindings: (keysym . modifiers) -> reka-binding.
  (bindings (make-hash-table :test 'equal))

  ;;; Focus tracking.
  ;; one of 'lost 'window 'frame
  (focus-state 'lost)
  (focused-window nil)
  (focused-frame nil)
  ;; set only by window_interaction event
  (focus-dirty nil)

  ;; Frame request accounting.
  (pending-frames 0)

  ;; Lambda command queue: list of LAMBDA.
  (command-queue nil))

(cl-defmacro reka--do (with (key val state) &body body)
  "Iterate over a reka state hash table.
WITH selects the collection and must be one of:
  windows, outputs, frames, bindings
KEY and VAL are the lambda arguments for `maphash'.
STATE is evaluated once and bound to that same name within BODY,
so BODY may refer to the `reka-state' object as STATE.

Example:
  (reka--do windows (id win state)
    (message \"window id=%s\" id))"
  (declare (indent 1))
  (unless (symbolp state)
    (error "reka--do: STATE slot must be a symbol, got: %S" state))
  (let* ((accessor
          (pcase with
            ('windows  'reka-state-windows)
            ('outputs  'reka-state-outputs)
            ('frames   'reka-state-frames)
            ('bindings 'reka-state-bindings)
            (_ (error "Unknown reka--do collection: %S" with))))
         (table (cl-gensym "reka--do-table")))
    `(let* ((,state ,state)
            (,table (,accessor ,state)))
       (maphash (lambda (,key ,val)
                  ,@body)
                ,table))))

(defun reka--set-frame-name (frame)
  (unless (string-prefix-p "reka-frame-"
                           (frame-parameter frame 'name))
    (set-frame-parameter frame 'name (make-temp-name "reka-frame-"))))

(defun reka--find-buffer-for-window (window)
  (seq-find (lambda (buf)
              (eq (buffer-local-value 'reka--window buf) window))
            (buffer-list)))

(defun reka--make-buffer-name (app-id title)
  (let ((title-trunc (if (> (length title) 40)
                         (format "%s…" (substring title 0 40))
                       title)))
    (if app-id (concat title-trunc " - " app-id)
      title-trunc)))

(defun reka--handle-commands (state &optional no-focus-update)
  (unless reka--handling-commands
    (let ((reka--handling-commands t))
      ;; Update WM window parameters from the current Emacs window layout.
      (condition-case err
          ;; TODO consider adding back? Was lost in refactoring.
          ;; (setq reka--last-focused nil)
          (let ((params (make-hash-table :test 'eql))
                (changed nil))
            (dolist (frame (frame-list))
              (let ((frame-name (frame-parameter frame 'name)))
                (dolist (window (window-list frame))
                  (when-let* ((buffer (window-buffer window))
                              ((reka--is-reka-buffer buffer))
                              (wm-window (buffer-local-value 'reka--window buffer)))
                    (pcase-let ((`(,left ,top ,right ,bottom)
                                 (window-inside-absolute-pixel-edges window)))
                      (puthash (ewc-object-id wm-window)
                               (reka-window-parameters-make
                                :window wm-window
                                :frame-name frame-name
                                :x left
                                :y top
                                :w (- right left)
                                :h (- bottom top))
                               params))))))
            (reka--do windows (id win state)
              (let ((new (gethash id params))
                    (old (reka-window-params win)))
                (unless (reka--params-equal old new)
                  (setf (reka-window-params win) new)
                  (setq changed t))))
            (when changed
              (reka--mark-manage-dirty state)))
        (error
         (message "reka update window parameters error: %S" err)))
      ;; Drain the command queue.
      (while-let ((cmd (pop (reka-state-command-queue state))))
        (condition-case err
            (funcall cmd)
          (error (message "reka command failed: %S" err))))

      (when (or reka--pending-handler
                (reka-state-command-queue state))
        (setq reka--pending-handler nil)
        (reka--schedule-command-handler))
      (unless no-focus-update
        (run-at-time nil nil #'reka--update-focus-request)))))

;; Major mode for reka-managed buffers
(defvar-local reka--window nil
  "Window object for this reka-mode buffer.")

(defun reka--buffer-killed ()
  "Request closing of the associated Wayland surface when a reka buffer is killed."
  (when-let* ((reka--window)
              (win (reka--window-by-proxy reka--state reka--window)))
    (setf (reka-window-state win) 'killed)
    (reka--mark-manage-dirty reka--state)))

(define-derived-mode reka-mode special-mode "Reka"
  "Major mode for buffers representing windows managed by reka."
  :group 'reka
  (setq-local buffer-read-only t)
  (add-hook 'kill-buffer-hook #'reka--buffer-killed nil t)
  (scroll-bar-mode 0)
  (setq-local left-fringe-width 0
              right-fringe-width 0))

(defun reka--is-reka-buffer (buf)
  "Return non-nil if BUF is a reka buffer."
  (eq (buffer-local-value 'major-mode buf) 'reka-mode))

;; TODO: Consider showing something useful in reka buffers
;; (title/app-id/dimensions) instead of an empty read-only buffer — it makes
;; debugging focus/placement much easier.
;;
;; TODO: consider (set-window-dedicated-p win t) on the window showing a reka
;; buffer, so an accidental C-x b doesn't silently detach the external surface
;; (as it stands the surface just hides, which is confusing).
(defun reka--create-buffer (window)
  "Create and display a reka buffer for Wayland WINDOW."
  (or (reka--find-buffer-for-window window)
      (let ((buffer (get-buffer-create (make-temp-name "reka-window-"))))
        (with-current-buffer buffer
          (reka-mode)
          (setq-local reka--window window))
        (display-buffer buffer)
        buffer)))

(defvar reka--last-focused nil
  "Last buffer for which a focus request was sent.")

(defun reka--update-focus-request (&rest _)
  "Wrapper for hooks and to make the wrapped fun testable"
  (when-let* ((state reka--state)
              (buf (window-buffer (selected-window)))
              ((reka--update-focus-for-buffer state buf)))
    (setq reka--last-focused buf)
    (reka--mark-manage-dirty state)))

(defun reka--update-focus-for-buffer (state buf)
  "Send a focus request to the WM reflecting the current selected window."
  (unless (eq buf reka--last-focused)
    (cond
     ;; The selected buffer is a reka buffer and Emacs is in a
     ;; stable state: focus the external Wayland window.
     ((and (reka--is-reka-buffer buf)
           (not this-command) ;; not in an interactive command
           (length= unread-command-events 0) ;; no pending key injections
           (length= (this-single-command-keys) 0) ;; no unfinished command sequence
           (zerop (minibuffer-depth)) ;; no minibuffer editing active
           (zerop (recursion-depth))) ;; no recursive edit ongoing
      (if-let* ((wm-window (buffer-local-value 'reka--window buf))
                (win-state (reka--window-by-proxy state wm-window))
                (frame-found (reka--frame-displaying-win state win-state))
                (frame (cdr frame-found)))
          (reka--focus-window state
                              wm-window
                              (reka-surface-proxy frame))
        (reka-log "reka: cannot focus window that is not displayed")
        nil))
     ;; Otherwise: return focus to the Emacs frame.
     (t (reka--focus-switch-to-frame state)))))

(defconst reka--modifier-bits
  '((shift   . 1)
    (control . 4)
    (meta    . 8)
    (super   . 64)
    (hyper   . 128))
  "Modifier bits as per river_seat_v1.modifiers / XKB")

(defun reka--key-to-xkb (key-string)
  "Decompose KEY-STRING into (EVENT KEY MODIFIERS)."
  (let* ((event (aref (kbd key-string) 0))
         (basic (event-basic-type event))
         (mods (seq-keep (lambda (mod)
                           (alist-get mod reka--modifier-bits))
                         (event-modifiers event)))
         (key (if (characterp basic) basic (symbol-name basic))))
    (list event key (apply #'logior mods))))

(defun reka-push-intercept-prefix (prefix &optional command)
  "Register PREFIX as an intercept key binding.
PREFIX is a key string suitable for `kbd'.
COMMAND may be `toggle-fullscreen'."
  (let* ((data (reka--key-to-xkb prefix))
         (event (nth 0 data))
         (key (nth 1 data))
         (modifiers (nth 2 data))
         (keysym (reka--resolve-keysym key))
         (cmd (if (eq command 'toggle-fullscreen)
                  'toggle-fullscreen
                event))
         (binding-key (cons keysym modifiers)))
    (if (= keysym 0)
        (message "reka: could not resolve XKB keysym for %S" key)
      (let ((existing (gethash binding-key (reka-state-bindings reka--state))))
        (if existing
            (setf (reka-binding-command existing) cmd
                  (reka-binding-event existing) event)
          (puthash binding-key
                   (reka-binding-make
                    :keysym keysym
                    :modifiers modifiers
                    :command cmd
                    :event event
                    :state 'requested)
                   (reka-state-bindings reka--state))))
      (reka--mark-manage-dirty reka--state))))

(defun reka-push-intercept-prefixes () ;; TODO: remove, leave only one way?
  "Update the intercept prefixes defined in `reka-intercept-prefixes'."
  (dolist (prefix reka-intercept-prefixes)
    (reka-push-intercept-prefix prefix)))

(defcustom reka-intercept-prefixes
  '("C-x" "C-u" "C-h" "M-x")
  "Prefix keys that should always go to Emacs."
  :type '(repeat key)
  :set (lambda (sym val)
         (set-default sym val)
         (when (and (boundp 'reka--state)
                    reka--state
                    (fboundp 'reka-push-intercept-prefixes))
           (reka-push-intercept-prefixes))))

(defun reka--suppress-focus-event (_orig-fn _event)
  "No-op for suppressing certain focus events in advice."
  (interactive "e")
  ;; okay, *almost* no-op ...
  (setq reka--last-focused nil))

(defun reka--buffer-predicate (buffer)
  "Buffer predicate to avoid accidentally showing the same reka buffer twice."
  (or (not (with-current-buffer buffer (derived-mode-p 'reka-mode)))
      (not (get-buffer-window buffer t))))

(defun reka--split-window-advice (new-window)
  "Advice window splits to always display another buffer, if a reka buffer
was split."
  (with-selected-window new-window
    (with-current-buffer (window-buffer)
      (when (derived-mode-p 'reka-mode)
        (switch-to-buffer (other-buffer)))))
  new-window)

(defun reka--set-window-buffer-advice (orig win buf &rest r)
  "Avoid double-display of reka buffers, by stealing them if they are
visible elsewhere. Note that displaying the same buffer in two different
tabs, for example, is completely valid."
  (with-current-buffer buf
    (when (derived-mode-p 'reka-mode)
      (dolist (other (get-buffer-window-list buf nil 'visible))
        (unless (eq (or win (selected-window)) other)
          (with-selected-window other
            (switch-to-buffer (other-buffer)))))))
  (apply orig win buf r))

(defvar reka-debug nil
  "When non-nil, enable verbose reka debugging messages.")

(defvar reka--state nil
  "Current global reka WM state.")

(defvar reka--command-timer nil
  "Timer used to drain the reka command queue.")

(defvar reka--manage-timer nil
  "Timer used to coalesce `manage-dirty' requests.")

(defvar reka--handling-commands nil
  "Non-nil while `reka--handle-commands' is running.")

(defvar reka--pending-handler nil
  "Non-nil if a command handler was requested while already handling commands.")

(defun reka-log (&rest args)
  "Log ARGS with `message' when `reka-debug' is non-nil."
  (when reka-debug
    (apply #'message args)))

;;; Protocol loading

(eval-and-compile
  (defconst reka--protocol-basenames
    '(("wayland" wl-display wl-registry)
      "river-window-management-v1"
      "river-xkb-bindings-v1"
      "river-layer-shell-v1")
    "Wayland protocols used by reka.")

  (defun reka--protocol-dir ()
    "Return the protocol directory next to the reka.el source.
The location is resolved relative to reka.el itself, not the file that
happens to be loading, so `reka--read-protocols' expands correctly even
when used from other files (e.g. tests)."
    (let* ((reka-file
            (cond
             ;; reka.el is currently being loaded.
             ((and load-file-name
                   (member (file-name-nondirectory load-file-name)
                           '("reka.el" "reka.elc")))
              load-file-name)
             ;; reka.el is currently being byte-compiled.
             ((and (bound-and-true-p byte-compile-current-file)
                   (member (file-name-nondirectory byte-compile-current-file)
                           '("reka.el" "reka.elc")))
              byte-compile-current-file)
             ;; Expansion originates from some other file: find reka.el
             ;; on the load path.
             (t
              (or (locate-file "reka" load-path '(".el" ".elc"))
                  (error "reka: cannot locate reka.el; add its directory to `load-path'")))))
           (reka-dir (file-name-directory (expand-file-name reka-file))))
      (expand-file-name "protocol" reka-dir)))

  (defun reka--protocol-file (basename)
    "Return the absolute XML file path for protocol BASENAME."
    (expand-file-name (concat basename ".xml")
                      (reka--protocol-dir)))

  (defmacro reka--read-protocols ()
  "Expand to an `ewc-read' form with protocol paths and interface filters."
  `(ewc-read
    ,@(mapcar
       (lambda (spec)
         (let ((spec (ensure-list spec)))
           (cons (reka--protocol-file (car spec))
                 (cdr spec))))
       reka--protocol-basenames)))
)

(defconst reka--global-binds
  '("river_window_manager_v1"
    "river_xkb_bindings_v1"
    "river_layer_shell_v1")
  "Wayland globals that reka binds.")

(defconst reka--edges-all 15
  "River Edges::all() bitmask.")

;;; Basic helpers

(defun reka--request (object request &optional arguments)
  "Send REQUEST on OBJECT using the current reka Wayland connection."
  (ewc-request (reka-state-connection reka--state)
               object
               request
               arguments))

(defun reka--interface-version (objects protocol interface)
  "Return XML-declared version of INTERFACE in PROTOCOL."
  (when-let* ((protocol-def (alist-get protocol
                                       (ewc-objects-protocols objects)))
              (interface-def (alist-get interface protocol-def)))
    ;; interface-def is: (version events requests)
    (car interface-def)))

(defun reka--global (state interface)
  "Fetch the ewc-object for INTERFACE from STATE's globals."
  (when-let* ((id (alist-get interface (reka-state-globals state))))
    (ewc-object-get id (reka-state-objects state))))

;; TODO consider moving to ewc.el
(defun reka--decode-string (s)
  "Decode a Wayland string S as UTF-8.
Return nil if S is nil or empty."
  (when (and s (not (string-empty-p s)))
    (decode-coding-string s 'utf-8)))

(defun reka--params-equal (a b)
  "Compare two `reka-window-parameters' values without deep traversal."
  (or (eq a b)
      (and a b
           (let ((wa (reka-window-parameters-window a))
                 (wb (reka-window-parameters-window b)))
             (and (ewc-object-p wa)
                  (ewc-object-p wb)
                  (= (ewc-object-id wa)
                     (ewc-object-id wb))))
           (equal (reka-window-parameters-frame-name a)
                  (reka-window-parameters-frame-name b))
           (equal (reka-window-parameters-x a)
                  (reka-window-parameters-x b))
           (equal (reka-window-parameters-y a)
                  (reka-window-parameters-y b))
           (equal (reka-window-parameters-w a)
                  (reka-window-parameters-w b))
           (equal (reka-window-parameters-h a)
                  (reka-window-parameters-h b)))))

(defun reka--window-by-proxy (state proxy)
  "Return window state in STATE for ewc-object PROXY."
  (when (ewc-object-p proxy)
    (gethash (ewc-object-id proxy)
             (reka-state-windows state))))

(defun reka--frame-by-proxy (state proxy)
  "Return frame state in STATE for ewc-object PROXY."
  (when (ewc-object-p proxy)
    (gethash (ewc-object-id proxy)
             (reka-state-frames state))))

(defun reka--surface-by-id (state id)
  "Return either a window or a frame struct from STATE by ID."
  (or (gethash id (reka-state-frames state))
      (gethash id (reka-state-windows state))))

(defun reka--output-by-id (state id)
  "Return output state in STATE for output ID."
  (gethash id (reka-state-outputs state)))

(defun reka--frame-by-cond (state predicate)
  "Return (ID . FRAME) for the first frame in STATE matching PREDICATE."
  (cl-loop for id being the hash-keys
           of (reka-state-frames state)
           using (hash-value f)
           thereis (and (funcall predicate f) (cons id f))))

(defun reka--frame-displaying-win (state win)
  "Return (ID . FRAME) frame displaying window WIN in STATE, if any."
  (when-let* ((p (reka-window-params win))
              (name (reka-window-parameters-frame-name p)))
    (reka--frame-by-cond state
      (lambda (f) (equal (reka-surface-title f) name)))))

(defun reka--frame-for-output (state output-id)
  "Return (ID . FRAME) for frame associated to OUTPUT-ID or nil."
  (reka--frame-by-cond state (lambda (f) (eql (reka-frame-displayed-on f) output-id))))

(defun reka--frame-without-output (state)
  "Return (ID . FRAME) of a frame not associated to any output or nil."
  (reka--frame-by-cond state (lambda (f) (null (reka-frame-displayed-on f)))))

(defun reka--frame-with-any-output (state)
  "Return (ID . FRAME) of any frame with an associated output or nil."
  (reka--frame-by-cond state #'reka-frame-displayed-on))

;;; Command queue

(defun reka--schedule-command-handler ()
  "Schedule command processing if not already scheduled."
  (if reka--handling-commands
      (setq reka--pending-handler t)
    (unless reka--command-timer
      (setq reka--command-timer
            (run-at-time
             0 nil
             (lambda (state)
               (setq reka--command-timer nil)
               (condition-case err
                   (reka--handle-commands state)
                 (error
                  (message "reka command handler error: %S" err))))
             reka--state
             )))))

(defun reka--enqueue (fn)
  "Queue FN for execution by reka--command-handler and schedule the command
handler timer. Only to be used in event listeners."
  (setf (reka-state-command-queue reka--state)
        (append (reka-state-command-queue reka--state) (list fn)))
  (reka--schedule-command-handler))

;;; manage-dirty coalescing

(defun reka--mark-manage-dirty (state)
  "Mark that a new manage sequence is needed."
  (unless reka--manage-timer
    (setq reka--manage-timer
          (run-at-time
           0 nil
           (lambda (state)
             (setq reka--manage-timer nil)
             (condition-case err
                 (when-let* ((wm (reka--global state
                                              'river-window-manager-v1)))
                   (reka--request wm 'manage-dirty))
               (error
                (message "reka manage-dirty error: %S" err))))
           state))))

;;; Focus helpers

(defun reka--focus-frame (state frame)
  "Focus FRAME in STATE.
Return non-nil if the focus state changed."
  (let ((changed
         (not (and (eq (reka-state-focus-state state) 'frame)
                   (eq
                    (reka-state-focused-frame state)
                    frame)))))
    (setf (reka-state-focus-state state) 'frame
          (reka-state-focused-frame state) frame
          (reka-state-focused-window state) nil)
    changed))

(defun reka--focus-window (state window frame)
  "Focus WINDOW on FRAME in STATE.
Return non-nil if the focus state changed."
  (let ((changed
         (not (and (eq (reka-state-focus-state state) 'window)
                   (eq
                    (reka-state-focused-window state)
                    window)
                   (eq
                    (reka-state-focused-frame state)
                    frame)))))
    (setf (reka-state-focus-state state) 'window
          (reka-state-focused-window state) window
          (reka-state-focused-frame state) frame)
    changed))

(defun reka--focus-switch-to-frame (state)
  "Switch focus from an external window back to its Emacs frame.
Return non-nil if the focus state changed."
  (when (eq (reka-state-focus-state state) 'window)
    (if (reka-state-focused-frame state)
        (setf (reka-state-focus-state state) 'frame
              (reka-state-focused-window state) nil)
      (setf (reka-state-focus-state state) 'lost
            (reka-state-focused-window state) nil
            (reka-state-focused-frame state) nil))
    t) ;; TODO: Why this t here? Is it needed?
  )

(defun reka--focus-current (state)
  "Return current focus as (TARGET . FRAME), if any."
  (pcase (reka-state-focus-state state)
    ('window
     (when-let* ((w (reka-state-focused-window state))
                (f (reka-state-focused-frame state)))
       (cons w f)))
    ('frame
     (when-let* ((f (reka-state-focused-frame state)))
       (cons f f)))
    (_ nil)))

(defun reka--focus-invalidate (state target)
  "Invalidate TARGET in STATE's focus tracking, e.g. after close."
  (cond
   ((and (reka-state-focused-frame state)
         (eq (reka-state-focused-frame state) target))
    (setf (reka-state-focus-state state) 'lost
          (reka-state-focused-frame state) nil
          (reka-state-focused-window state) nil))

   ((and (reka-state-focused-window state)
         (eq (reka-state-focused-window state) target))
    (setf (reka-state-focus-state state) 'frame
          (reka-state-focused-window state) nil))))

;;; Fullscreen helpers

(defun reka--fs-state (fs)
  "Return the state symbol from fullscreen state FS."
  (if (listp fs)
      (or (plist-get fs :state) 'none)
    'none))

(defun reka--fs-window (fs)
  "Return the window involved in fullscreen state FS, if any."
  (pcase (reka--fs-state fs)
    ('requested (plist-get fs :new))
    ((or 'fullscreen 'exiting) (plist-get fs :window))
    (_ nil)))

(defun reka--select-buffer-for-window (window)
  "Select the Emacs window displaying Wayland WINDOW, if visible."
  (when-let* ((buf (reka--find-buffer-for-window window))
              (win (get-buffer-window buf t)))
    (select-window win 'norecord)))

;;; XKB keysym resolution

(defvar reka--xkb-keysym-alist
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

(defun reka--utf32-to-keysym (cp)
  "Convert Unicode codepoint CP to an XKB keysym."
  (cond
   ((not (integerp cp)) 0)
   ((and (>= cp #x20) (<= cp #x7e)) cp)
   ((and (>= cp #xa0) (<= cp #xff)) cp)
   ((> cp 0) (+ #x01000000 cp))
   (t 0)))

(defun reka--resolve-keysym (key)
  "Resolve KEY to an XKB keysym.
KEY may be an integer codepoint, a symbol, or a string key name."
  (cond
   ((integerp key)
    (reka--utf32-to-keysym key))

   ((symbolp key)
    (reka--resolve-keysym (symbol-name key)))

   ((stringp key)
    (or (alist-get (downcase key)
                   reka--xkb-keysym-alist
                   nil nil #'equal)
        (and (= (length key) 1)
             (reka--utf32-to-keysym (aref key 0)))
        0))

   (t 0)))



;;; Layer shell attachment helpers

(defun reka--setup-ls-output-listeners (ls-output-obj)
  "Setup listeners for a River layer-shell output object."
  (ewc-set-listener ls-output-obj 'non-exclusive-area
        (pcase-lambda (object (map x y width height))
          (when-let* ((out (cl-loop for out being the hash-values
                                    of (reka-state-outputs reka--state)
                                    thereis (and (eq (reka-output-ls-output out) object)
                                                 out))))
            (setf (reka-output-x out) x
                  (reka-output-y out) y
                  (reka-output-width out) width
                  (reka-output-height out) height)))))

(defun reka--ensure-ls-output (state output-id)
  "Create a layer-shell output object for OUTPUT-ID if possible."
  (when-let* ((out (reka--output-by-id state output-id))
              ((null (reka-output-ls-output out)))
              (ls-obj (reka--global state 'river-layer-shell-v1))
              (objects (reka-state-objects state))
              (ls-output-id (cl-incf (ewc-objects-new-id objects)))
              (ls-output-obj
               (ewc-object-add :objects objects
                               :protocol 'river-layer-shell-v1
                               :interface 'river-layer-shell-output-v1
                               :id ls-output-id)))
    (setf (reka-output-ls-output out) ls-output-obj)
    (reka--setup-ls-output-listeners ls-output-obj)
    (reka--request ls-obj 'get-output
                   `((id . ,ls-output-id)
                     (output . ,output-id)))))

;; TODO: this ls-seat is never used ATM.
(defun reka--ensure-ls-seat (state)
  "Create a layer-shell seat object for the current seat if possible."
  (when-let* ((seat (reka-state-seat state))
              ((null (reka-seat-ls-seat seat)))
              (ls-obj (reka--global state 'river-layer-shell-v1))
              (objects (reka-state-objects state))
              (ls-seat-id (cl-incf (ewc-objects-new-id objects)))
              (ls-seat-obj
               (ewc-object-add :objects objects
                               :protocol 'river-layer-shell-v1
                               :interface 'river-layer-shell-seat-v1
                               :id ls-seat-id)))
    (setf (reka-seat-ls-seat seat) ls-seat-obj)
    (reka--request ls-obj 'get-seat
                   `((id . ,ls-seat-id)
                     (seat . ,(reka-seat-id seat))))))

;;; Listeners

;; Order event listeners by their order in the protocol definitions!

;;;; wayland protocol
;;;; wl-display listeners
;; TODO handle individual args and decode the message string with
;; reka--decode-string
(defun reka-on-wl-display-error (_object args)
  (message "wl_display error: %S" args))

(defun reka-on-wl-display-delete-id (_object args)
  (pcase-let (((map id) args))
    (when-let* ((objects (reka-state-objects reka--state))
                (table (ewc-objects-table objects))
                (obj (gethash id table)))
      (message "delete-id for obj %d, interface=%s" id (ewc-object-interface obj))
      (remhash id table))))

;;;; wl-registry listeners
(defun reka-on-wl-registry-global (registry args)
  (pcase-let (((map name interface version) args))
    (when-let* ((ifsym (intern (string-replace "_" "-" interface)))
                (protocol (ewc-find-protocol (reka--read-protocols) ifsym))
                ((member interface reka--global-binds))
                (objects (reka-state-objects reka--state))
                (new-id (cl-incf (ewc-objects-new-id objects)))
                (xml-version (reka--interface-version objects protocol ifsym))
                (bind-version
                 (if xml-version (min version xml-version) version))
                (bound-object (ewc-object-add :objects objects
                                              :protocol protocol
                                              :interface ifsym
                                              :id new-id)))
      (reka-log "reka: binding global %s version %s" interface bind-version)
      (reka--request registry 'bind
                     `((name . ,name)
                       (interface-len . ,(1+ (string-bytes interface)))
                       (interface . ,interface)
                       (version . ,bind-version)
                       (id . ,new-id)))
      (push (cons ifsym new-id) (reka-state-globals reka--state))

      (pcase ifsym
        ('river-window-manager-v1
         (reka--setup-wm-listeners bound-object))

        ('river-layer-shell-v1
         ;; Attach layer-shell objects to existing outputs/seats in STATE."
         (reka--do outputs (id _out reka--state)
                   (reka--ensure-ls-output reka--state id))
         (reka--ensure-ls-seat reka--state))

        (_ (reka-log "reka: bound %s" ifsym))))))

;;;; river-window-management-v1 Protocol
;;;; river-window-v1 listeners
(defun reka-on-river-window-v1-closed (object _)
  (let ((win-id (ewc-object-id object)))
    (when (gethash win-id (reka-state-windows reka--state))
      (reka--enqueue
       (lambda ()
         (when-let* ((buf (reka--find-buffer-for-window object)))
           (kill-buffer buf)))))
    (reka--focus-invalidate reka--state object)
    ;; Reset fullscreen on output if window was fullscreen
    (reka--do outputs (_output-id out reka--state)
      (when (eq (reka--fs-window (reka-output-fullscreen out)) object)
        (setf (reka-output-fullscreen out) 'none)))
    (let* ((surface (reka--surface-by-id reka--state win-id))
           (node (reka-surface-node surface))
           (table (ewc-objects-table (reka-state-objects reka--state))))
      (reka--request node 'destroy)
      (reka--request object 'destroy)
      (remhash win-id table)
      (remhash (ewc-object-id node) table))
    (remhash win-id (reka-state-windows reka--state))
    (remhash win-id (reka-state-frames reka--state))))

(defun reka-on-river-window-v1-dimensions (object args)
  (pcase-let (((map width height) args))
    (when-let* ((win (reka--window-by-proxy reka--state object)))
      (setf (reka-window-actual-width win) width
            (reka-window-actual-height win) height))))

(defun reka-on-river-window-v1-app-id (object args)
  (pcase-let (((map app-id) args))
    (when-let* ((app-id (reka--decode-string app-id))
                (win (reka--window-by-proxy reka--state object)))
      (setf (reka-window-app-id win) app-id))))

(defun reka-on-river-window-v1-title (object args)
  (pcase-let (((map title) args))
    (when-let* ((title (reka--decode-string title))
                (win-id (ewc-object-id object))
                (surface (reka--surface-by-id reka--state win-id)))
      (setf (reka-surface-title surface) title)
      (when (reka-window-p surface)
        (reka--enqueue
         (lambda ()
           (when-let* ((buf (reka--find-buffer-for-window object)))
             (with-current-buffer buf
               (rename-buffer
                (reka--make-buffer-name
                 (reka-window-app-id surface)
                 title)
                t)))))))))

(defun reka-on-river-window-v1-fullscreen-requested (object args)
  (pcase-let* (((map output) args)
               (target
                (or (and (integerp output)
                         (not (zerop output))
                         output)
                    (when-let* ((w (reka--window-by-proxy reka--state object))
                                (found (reka--frame-displaying-win reka--state w))
                                (f (cdr found)))
                      (reka-frame-displayed-on f))
                    (when-let* ((cur (reka--focus-current reka--state))
                                (frame (cdr cur))
                                (f (reka--frame-by-proxy reka--state frame)))
                      (reka-frame-displayed-on f)))))
    (if (not target)
        (message "Fullscreen requested, but no output found")
      (when-let* ((out (reka--output-by-id reka--state target)))
        (let* ((fs (reka-output-fullscreen out))
               (previous
                (pcase (reka--fs-state fs)
                  ((or 'fullscreen 'exiting)
                   (plist-get fs :window))
                  (_ nil))))
          (setf (reka-output-fullscreen out)
                (list :state 'requested
                      :new object
                      :previous previous)))))))

(defun reka-on-river-window-v1-exit-fullscreen-requested (object _)
  (reka--do outputs (_output-id out reka--state)
    (let ((fs (reka-output-fullscreen out)))
      (when (and (member (reka--fs-state fs)
                         '(requested fullscreen))
                 (eq (reka--fs-window fs) object))
        (setf (reka-output-fullscreen out)
              (list :state 'exiting
                    :window (reka--fs-window fs)))))))

(defun reka-on-river-window-v1-minimize-requested (object _)
  (when (reka--window-by-proxy reka--state object)
    (reka--enqueue
     (lambda ()
       (when-let* ((buf (reka--find-buffer-for-window object)))
         (with-current-buffer buf
           (bury-buffer)))))))

(defun reka-on-river-window-v1-unreliable-pid (object args)
  (pcase-let* (((map unreliable-pid) args)
               (win-id (ewc-object-id object))
               (objects (reka-state-objects reka--state))
               (node-obj
                (ewc-object-add
                 :objects objects
                 :protocol 'river-window-management-v1
                 :interface 'river-node-v1
                 :id (cl-incf (ewc-objects-new-id objects)))))

    (reka--request object 'get-node
                   `((id . ,(ewc-object-id node-obj))))

    (if (= unreliable-pid (reka-state-pid reka--state))
        (progn
          (reka-log "Discovered new Emacs frame")

          (puthash win-id
                   (reka-frame-make
                    :proxy object
                    :node node-obj)
                   (reka-state-frames reka--state))

          (if (> (reka-state-pending-frames reka--state) 0)
              (cl-decf (reka-state-pending-frames reka--state))
            (reka-log "New frame was not requested by WM")))

      (reka-log "Discovered new regular external window")

      (puthash win-id
               (reka-window-make
                :proxy object
                :node node-obj)
               (reka-state-windows reka--state))

      (reka--enqueue
       (lambda ()
         ;; Confirm that the Emacs-side buffer for the window was created.
         (when-let* ((win (reka--window-by-proxy reka--state object)))
           (reka--create-buffer object)
           (setf (reka-window-state win) 'active)
           (reka--mark-manage-dirty reka--state)))))))

(defun reka--setup-wm-listeners (wm-obj)
  "Setup initial event listeners for the River window manager."
    (ewc-set-listener wm-obj 'output
          (pcase-lambda (_object (map id))
            (let* ((objects (reka-state-objects reka--state))
                   (output-obj
                    (ewc-object-add :objects objects
                                    :protocol 'river-window-management-v1
                                    :interface 'river-output-v1
                                    :id id)))

              (puthash id
                       (reka-output-make :proxy output-obj)
                       (reka-state-outputs reka--state))

              (ewc-set-listener output-obj 'removed 'reka-on-river-output-v1-removed)
              (ewc-set-listener output-obj 'position 'reka-on-river-output-v1-position)
              (ewc-set-listener output-obj 'dimensions 'reka-on-river-output-v1-dimensions)

              (reka--ensure-ls-output reka--state id))))

    (ewc-set-listener wm-obj 'seat
          (pcase-lambda (_object (map id))
            (if (reka-state-seat reka--state)
                (message "reka does not support multi-seat")
              (let* ((objects (reka-state-objects reka--state))
                     (seat-obj
                      (ewc-object-add :objects objects
                                      :protocol 'river-window-management-v1
                                      :interface 'river-seat-v1
                                      :id id)))

                (setf (reka-state-seat reka--state)
                      (reka-seat-make :id id
                                      :proxy seat-obj))

                (ewc-set-listener seat-obj 'window-interaction 'reka-on-river-seat-v1-window-interaction)
                (reka--ensure-ls-seat reka--state)))))

    (ewc-set-listener wm-obj 'window
          (pcase-lambda (_object (map id))
            (let ((win-obj
                   (ewc-object-add :objects (reka-state-objects reka--state)
                                   :protocol 'river-window-management-v1
                                   :interface 'river-window-v1
                                   :id id)))
              (ewc-set-listener win-obj 'unreliable-pid 'reka-on-river-window-v1-unreliable-pid)
              (ewc-set-listener win-obj 'app-id 'reka-on-river-window-v1-app-id)
              (ewc-set-listener win-obj 'title 'reka-on-river-window-v1-title)
              (ewc-set-listener win-obj 'dimensions 'reka-on-river-window-v1-dimensions)
              (ewc-set-listener win-obj 'closed 'reka-on-river-window-v1-closed)
              (ewc-set-listener win-obj 'minimize-requested 'reka-on-river-window-v1-minimize-requested)
              (ewc-set-listener win-obj 'fullscreen-requested 'reka-on-river-window-v1-fullscreen-requested)
              (ewc-set-listener win-obj 'exit-fullscreen-requested 'reka-on-river-window-v1-exit-fullscreen-requested))))

    (ewc-set-listener wm-obj 'manage-start
          (lambda (object _)
            (unwind-protect
                (condition-case err
                    (reka--reconcile reka--state)
                  (error
                   (message "reka reconcile error: %S" err)))
                (reka--request object 'manage-finish))))

    (ewc-set-listener wm-obj 'render-start
          (lambda (object _)
            (unwind-protect
                (condition-case err
                    (progn
                      (reka--render-frames reka--state)
                      (reka--render-windows reka--state))
                  (error
                   (message "reka render error: %S" err)))
                (reka--request object 'render-finish))))

    (dolist (evt '(unavailable finished session-locked session-unlocked))
      (let ((evt evt))
        (ewc-set-listener wm-obj evt
              (lambda (_object _)
                (message "reka: WM event %s" evt))))))

;;;; river-output-v1 listeners
(defun reka-on-river-output-v1-removed (object _)
  (let ((output-id (ewc-object-id object)))
    (reka--do frames (_fid f reka--state)
              (when-let* (((eql (reka-frame-displayed-on f) output-id))
                          (name (reka-surface-title f)))
                (reka--enqueue
                 (lambda ()
                   (when-let* ((frame (alist-get name (make-frame-names-alist) nil nil #'equal)))
                     (delete-frame frame))))))
    (let* ((out (gethash output-id (reka-state-outputs reka--state)))
           (ls-output (reka-output-ls-output out))
           (table (ewc-objects-table (reka-state-objects reka--state))))
      (when ls-output
        (reka--request ls-output 'destroy)
        (remhash (ewc-object-id ls-output) table))
      (reka--request object 'destroy)
      (remhash output-id (reka-state-outputs reka--state))
      (remhash output-id table))))

;; TODO: listener for wl_output, e.g. to get monitor names

(defun reka-on-river-output-v1-position (object args)
  (pcase-let (((map x y) args))
    (when-let* ((output-id (ewc-object-id object))
                (out (reka--output-by-id reka--state output-id)))
      (setf (reka-output-x out) x
            (reka-output-y out) y))))

(defun reka-on-river-output-v1-dimensions (object args)
  (pcase-let (((map width height) args))
    (when-let* ((output-id (ewc-object-id object))
                (out (reka--output-by-id reka--state output-id)))
      (setf (reka-output-width out) width
            (reka-output-height out) height))))

;;;; river-seat-v1 listener
(defun reka-on-river-seat-v1-window-interaction (_object args)
  (pcase-let* (((map window) args)
               (win-obj (ewc-object-get window
                                        (reka-state-objects reka--state))))
    (cond
     ((gethash window (reka-state-frames reka--state))
      (when (and win-obj
                 (reka--focus-frame reka--state win-obj))
        (setf (reka-state-focus-dirty reka--state) t)))
     ((gethash window (reka-state-windows reka--state))
      (if-let* ((w (gethash window (reka-state-windows reka--state)))
                (found (reka--frame-displaying-win reka--state w))
                (f (cdr found)))
          (when (and win-obj
                     (reka--focus-window reka--state
                                         win-obj
                                         (reka-surface-proxy f)))
            (setf (reka-state-focus-dirty reka--state) t))
        (message "Window interaction for window without frame"))))))

(defun reka--setup-binding-listeners (proxy)
  "Setup listeners for one XKB binding object."
  (ewc-set-listener proxy 'pressed
        (lambda (object _)
          (when-let* ((binding
                       (cl-loop for b being the hash-values
                                of (reka-state-bindings reka--state)
                                thereis (and (eq (reka-binding-proxy b) object) b)))
                      (command (reka-binding-command binding)))
            (if (eq command 'toggle-fullscreen)
                (reka--toggle-fullscreen reka--state)
              (reka--enqueue
               (lambda ()
                 (push (cons t command) unread-command-events)))
              (reka--focus-switch-to-frame reka--state))))))

;;; Reconciliation

(defun reka--reconcile-frames (state)
  "Ensure each output gets one maximized Emacs frame."
  (let ((frame-requests 0))
    (reka--do outputs (output-id out state)
              (if-let* ((found (reka--frame-for-output state output-id))
                        (f (cdr found)))
                  ;; Frame already assigned: only re-propose if size changed.
                  (unless (and (eq (reka-frame-proposed-width f) (reka-output-width out))
                               (eq (reka-frame-proposed-height f) (reka-output-height out)))
                    (setf (reka-frame-proposed-width f) (reka-output-width out)
                          (reka-frame-proposed-height f) (reka-output-height out))
                    (reka--request (reka-surface-proxy f)
                                   'propose-dimensions
                                   `((width . ,(reka-output-width out))
                                     (height . ,(reka-output-height out)))))

                (if-let* ((found (reka--frame-without-output state))
                          (f (cdr found))
                          (frame-proxy (reka-surface-proxy f)))
                    (progn
                      (setf (reka-frame-displayed-on f) output-id
                            (reka-frame-proposed-width f) (reka-output-width out)
                            (reka-frame-proposed-height f) (reka-output-height out))
                      (reka--request frame-proxy
                                     'propose-dimensions
                                     `((width . ,(reka-output-width out))
                                       (height . ,(reka-output-height out))))
                      (reka--request frame-proxy
                                     'inform-maximized)
                      (reka--request frame-proxy
                                     'set-tiled
                                     `((edges . ,reka--edges-all))))
                  ;; No frame on this output yet: request one.
                  (cl-incf frame-requests))))
    (dotimes (_ (- frame-requests (reka-state-pending-frames state)))
      (reka--enqueue (lambda () (make-frame)))
      (cl-incf (reka-state-pending-frames state)))))

(defun reka--reconcile-windows (state)
  "Close killed windows and propose dimensions for active windows."
  (reka--do windows (_id win state)
            (pcase (reka-window-state win)
              ;; nothing to do for window-state 'starting
              ('active
               (when-let* ((params (reka-window-params win)))
                 ;; TODO: This gets sent on every loop for all windows?
                 (reka--request (reka-surface-proxy win)
                                'set-tiled
                                `((edges . ,reka--edges-all)))

                 (reka--request (reka-surface-proxy win)
                                'propose-dimensions
                                `((width . ,(reka-window-parameters-w params))
                                  (height . ,(reka-window-parameters-h params))))))
              ('killed
               (reka--request (reka-surface-proxy win) 'close)))))

(defun reka--reconcile-bindings (state)
  "Create and enable XKB bindings."
  (when-let* ((xkb (reka--global state 'river-xkb-bindings-v1))
              (seat (reka-state-seat state))
              (seat-proxy (reka-seat-proxy seat)))

    ;; TODO: combine the two loops into one
    (reka--do bindings (key binding state)
      (when (eq (reka-binding-state binding) 'requested)
        (let* ((objects (reka-state-objects state))
               (id (cl-incf (ewc-objects-new-id objects)))
               (proxy
                (ewc-object-add :objects objects
                                :protocol 'river-xkb-bindings-v1
                                :interface 'river-xkb-binding-v1
                                :id id)))

          (reka--setup-binding-listeners proxy)

          (reka--request xkb 'get-xkb-binding
                       `((seat . ,(ewc-object-id seat-proxy))
                         (keysym . ,(reka-binding-keysym binding))
                         (modifiers . ,(reka-binding-modifiers binding))
                         (id . ,id)))

          (setf (reka-binding-proxy binding) proxy
                (reka-binding-state binding) 'registered))))

    (reka--do bindings (_key binding state)
      (when (eq (reka-binding-state binding) 'registered)
        (reka--request (reka-binding-proxy binding) 'enable)
        (setf (reka-binding-state binding) 'enabled)))))

(defun reka--reconcile-fullscreen (state)
  "Advance fullscreen state machines."
  (reka--do outputs (_output-id out state)
    (let ((fs (reka-output-fullscreen out)))
      (pcase (reka--fs-state fs)
        ('requested
         (let ((new (plist-get fs :new))
               (prev (plist-get fs :previous)))

           (when (and prev (ewc-object-p prev))
             (reka--request prev 'inform-not-fullscreen)
             (reka--request prev 'exit-fullscreen))

           (when (and new (ewc-object-p new))
             (reka--request new 'inform-fullscreen)
             (reka--request new 'fullscreen
                          `((output . ,(ewc-object-id
                                        (reka-output-proxy out))))))

           (setf (reka-output-fullscreen out)
                 (list :state 'fullscreen
                       :window new))))

        ('exiting
         (let ((win (plist-get fs :window)))
           (when (and win (ewc-object-p win))
             (reka--request win 'inform-not-fullscreen)
             (reka--request win 'exit-fullscreen))

           (setf (reka-output-fullscreen out) 'none)))

        (_ nil)))))

(defun reka--reconcile-focus (state)
  "Update the seat focus based on STATE."
  (unless (reka--focus-current state)
    (setf (reka-state-focus-state state) 'lost))

  (when (eq (reka-state-focus-state state) 'lost)
    (if-let* ((found (reka--frame-with-any-output state))
              (f (cdr found)))
        (reka--focus-frame state (reka-surface-proxy f))))

  (when-let* ((seat (reka-state-seat state))
              (seat-proxy (reka-seat-proxy seat))
              (cur (reka--focus-current state)))
    (let* ((target (car cur))
           (frame (cdr cur))
           (dirty (reka-state-focus-dirty state)))

      (when (and target (ewc-object-p target))
        (reka--request seat-proxy
                       'focus-window
                       `((window . ,(ewc-object-id target)))))

      (when dirty
        (when-let* ((frame-obj (and frame
                                    (reka--frame-by-proxy state frame)))
                    (out-id (reka-frame-displayed-on frame-obj))
                    (out (reka--output-by-id state out-id))
                    (ls (reka-output-ls-output out)))
          (reka--request ls 'set-default))

        (unless (and frame target (eq target frame))
          (reka--enqueue
           (lambda ()
             (reka--select-buffer-for-window target))))

        (setf (reka-state-focus-dirty state) nil)))))

(defun reka--reconcile (state)
  "Run the manage-sequence reconciliation for STATE."
  (reka--reconcile-frames state)
  (reka--reconcile-windows state)
  (reka--reconcile-bindings state)
  (reka--reconcile-fullscreen state)
  (reka--reconcile-focus state))

(defun reka--render-frames (state)
  "Run the render-sequence reconciliation for frames on STATE."
  (reka--do frames (_id frame state)
            (let ((proxy (reka-surface-proxy frame))
                  (node (reka-surface-node frame))
                  (out-id (reka-frame-displayed-on frame)))
              (if (not out-id)
                  (when (reka-frame-visible frame)
                    (setf (reka-frame-visible frame) nil)
                    (reka--request proxy 'hide))
                (unless (reka-frame-visible frame)
                  (setf (reka-frame-visible frame) t)
                  (reka--request proxy 'show))
                (when node
                  (reka--request node 'place-bottom))
                (when-let* ((out (reka--output-by-id state out-id))
                            (node)
                            ((not (and (eq (reka-frame-last-x frame) (reka-output-x out))
                                       (eq (reka-frame-last-y frame) (reka-output-y out))))))
                  (setf (reka-frame-last-x frame) (reka-output-x out)
                        (reka-frame-last-y frame) (reka-output-y out))
                  (reka--request node 'set-position
                                 `((x . ,(reka-output-x out))
                                   (y . ,(reka-output-y out)))))))))

(defun reka--render-windows (state)
  "Run the render-sequence reconciliation for windows on STATE."
  (reka--do windows (_id win state)
    (let ((proxy (reka-surface-proxy win))
          (node (reka-surface-node win)))
      (if (not (eq (reka-window-state win) 'active))
          (reka--request proxy 'hide)

        (if-let* ((params (reka-window-params win))
                  (frame-found (reka--frame-displaying-win state win))
                  (frame (cdr frame-found))
                  (out-id (reka-frame-displayed-on frame))
                  (out (reka--output-by-id state out-id)))
            (progn
              (reka--request proxy 'show)

              (when node
                (reka--request node 'set-position
                             `((x . ,(+ (reka-window-parameters-x params)
                                        (reka-output-x out)))
                               (y . ,(+ (reka-window-parameters-y params)
                                        (reka-output-y out)))))

                (reka--request node 'place-top))

              (let ((clip-w (or (reka-window-actual-width win)
                                (reka-window-parameters-w params)))
                    (clip-h (or (reka-window-actual-height win)
                                (reka-window-parameters-h params))))
                (reka--request proxy 'set-clip-box
                             `((x . 0)
                               (y . 0)
                               (width . ,clip-w)
                               (height . ,clip-h)))))

          (reka--request proxy 'hide))))))

;;; Fullscreen toggle

(defun reka--toggle-fullscreen (state)
  "Toggle fullscreen for the currently focused external window."
  (if-let* ((cur (reka--focus-current state))
            (focus (car cur))
            (frame (cdr cur)))
      (if (eq focus frame)
          (reka--enqueue
           (lambda ()
             (message "reka: can not fullscreen Emacs even more!")))

        (if-let* ((frame-obj (reka--frame-by-proxy state frame))
                  (out-id (reka-frame-displayed-on frame-obj))
                  (out (reka--output-by-id state out-id)))
            (let ((fs (reka-output-fullscreen out)))
              (pcase (reka--fs-state fs)
                ('none
                 (setf (reka-output-fullscreen out)
                       (list :state 'requested
                             :new focus
                             :previous nil))
                 (reka--mark-manage-dirty state))

                ('fullscreen
                 (setf (reka-output-fullscreen out)
                       (list :state 'exiting
                             :window (plist-get fs :window)))
                 (reka--mark-manage-dirty state))

                (_
             (message "Invalid output state for fullscreen toggle"))))

          (message "Selected frame for fullscreen is not displayed"))))

  (message "Fullscreen requested, but nothing is focused"))

;;; Startup

(defun reka--start-wm ()
  "Connect to Wayland and initialize the reka state."
  (let* ((objects (ewc-objects-make :protocols (reka--read-protocols)))
         (connection (ewc-connect objects))
         (display (ewc-object-add :objects objects
                                  :protocol 'wayland
                                  :interface 'wl-display))
         (registry (ewc-object-add :objects objects
                                   :protocol 'wayland
                                   :interface 'wl-registry))
         (state (reka-state-make :connection connection
                                 :objects objects)))

    (setq reka--state state)

    (ewc-set-listener display 'error 'reka-on-wl-display-error)
    (ewc-set-listener display 'delete-id 'reka-on-wl-display-delete-id)
    (ewc-set-listener registry 'global 'reka-on-wl-registry-global)

    (reka--request display 'get-registry
                   `((registry . ,(ewc-object-id registry))))))

;; NOTE: No need for reka-disable since this Emacs process is serving as a
;; Window Manager and disabling reka while keeping the Emacs process running
;; would result in an unresponsive user environment.
;;;###autoload
(defun reka-enable ()
  "Enable the reka window manager for river. Call this function once when
starting Emacs inside of river."
  (when reka--state
    (user-error "reka is already running"))

  (unless (eq window-system 'pgtk)
    (user-error "reka requires a pgtk Emacs on Wayland"))

  ;; TODO: this is a hack for lack of ability to figure out alignment ...
  (menu-bar-mode 0)
  (tool-bar-mode 0)

  ;; configure this and all future frames ..
  (let ((frame-params '((undecorated . t)
                        ;; avoid showing the same reka buffer twice
                        (buffer-predicate . reka--buffer-predicate))))
    (modify-all-frames-parameters frame-params))

  (advice-add 'split-window-below :filter-return #'reka--split-window-advice)
  (advice-add 'split-window-right :filter-return #'reka--split-window-advice)
  (advice-add 'set-window-buffer :around #'reka--set-window-buffer-advice)

  (message "Launching reka (pure Elisp) ...")

  ;; Ensure each existing frame has a unique title that reka can match.
  (cl-loop for frame being the frames
           do (reka--set-frame-name frame))
  (add-to-list 'after-make-frame-functions #'reka--set-frame-name)

  (reka--start-wm)
  (reka-push-intercept-prefixes)

  ;; Layout signals
  (add-hook 'window-configuration-change-hook #'reka--schedule-command-handler)

  ;; Focus signals
  (add-hook 'window-selection-change-functions #'reka--update-focus-request)
  (add-hook 'window-buffer-change-functions    #'reka--update-focus-request)
  (add-hook 'minibuffer-setup-hook             #'reka--update-focus-request)
  (add-hook 'minibuffer-exit-hook              #'reka--update-focus-request)
  (add-hook 'post-command-hook                 #'reka--update-focus-request)

  ;; Suppress pgtk focus feedback loop (as does EXWM)
  ;; TODO: figure out if/how this breaks multi-frame focus changes ...
  (advice-add 'handle-focus-in  :around #'reka--suppress-focus-event)
  (advice-add 'handle-focus-out :around #'reka--suppress-focus-event)

  (run-hooks 'reka-enable-hook))

(provide 'reka)
