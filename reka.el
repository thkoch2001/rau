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

(cl-defstruct (reka-fs (:constructor reka-fs) (:copier nil))
  "Fullscreen state machine for one output.
STATE is one of: `none', `requested', `fullscreen', `exiting'.
NEW and PREVIOUS are meaningful only when STATE is `requested'.
WINDOW is meaningful when STATE is `fullscreen' or `exiting'."
  (state    'none :type symbol :read-only t)
  (new      nil   :read-only t)
  (previous nil   :read-only t)
  (window   nil   :read-only t))

(cl-defstruct (reka-window-parameters
               (:constructor reka-window-parameters-make))
  "Window parameters describing where an external surface should be placed."
  emacs-frame
  x
  y
  w
  h)

(cl-defstruct (reka-output (:constructor reka-output-make))
  "State for a River output."
  ls-output-wl
  (x 0)
  (y 0)
  (width 0)
  (height 0)
  (fullscreen (reka-fs)))

(cl-defstruct (reka-surface (:constructor nil))
  "Common state shared by windows and frames.
Not instantiated directly; windows and frames include it."
  node-wl
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
  emacs-frame
  (output-wl nil :documentation "displaying this frame")
  proposed-width
  proposed-height
  visible
  last-x
  last-y)

(cl-defstruct (reka-seat (:constructor reka-seat-make))
  "State for a River seat."
  ls-seat-wl)

(cl-defstruct (reka-binding (:constructor reka-binding-make))
  "State for one global XKB binding."
  keysym
  modifiers
  command
  event
  (state 'requested))

(cl-defstruct (reka-state (:constructor reka-state-make))
  "Holds the state of the reka Wayland client."
  (connection nil)
  (client nil :type ewc-client)
  (pid (emacs-pid))

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


;;; Surface tags

(defconst reka--tag-frame :reka-frame
  "Tag for `river-window-v1' objects that are Emacs frames.")

(defconst reka--tag-window :reka-window
  "Tag for `river-window-v1' objects that are external windows.")

(cl-defmacro reka--do (tag (obj val state) &body body)
  "Iterate over the ewc objects in STATE tagged with TAG.
TAG is a form that evaluates to or is an ewc-object tag, for example
`reka--tag-window', `reka--tag-frame', or `'river-output-v1'.

OBJ is bound to the ewc-object and VAL to its data for each
element.  STATE is evaluated once and bound to that same name
within BODY."
  (declare (indent 1))
  (unless (symbolp state)
    (error "reka--do: STATE slot must be a symbol, got: %S" state))
  `(let ((,state ,state))
     (dolist (,obj (ewc-objects (reka-state-client ,state) ,tag))
       (let ((,val (ewc-object-data ,obj)))
         ,@body))))

(defun reka--set-frame-name (frame)
  (unless (string-prefix-p "reka-frame-"
                           (frame-parameter frame 'name))
    (set-frame-parameter frame 'name (make-temp-name "reka-frame-"))))

(defun reka--find-buffer-for-window (window-wl)
  (seq-find (lambda (buf)
              (eq (buffer-local-value 'reka--window buf) window-wl))
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
            (dolist (emacs-frame (frame-list))
              (dolist (emacs-window (window-list emacs-frame))
                (when-let* ((buffer (window-buffer emacs-window))
                            ((reka--is-reka-buffer buffer))
                            (window-wl (buffer-local-value 'reka--window buffer)))
                  (pcase-let ((`(,left ,top ,right ,bottom)
                               (window-inside-absolute-pixel-edges emacs-window)))
                    (puthash (ewc-object-id window-wl)
                             (reka-window-parameters-make
                              :emacs-frame emacs-frame
                              :x left
                              :y top
                              :w (- right left)
                              :h (- bottom top))
                             params)))))
            (reka--do reka--tag-window (window-wl win state)
              (let* ((id (ewc-object-id window-wl))
                     (new (gethash id params))
                    (old (reka-window-params win)))
                (unless (equal old new)
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
              (data (ewc-object-data reka--window)))
    (setf (reka-window-state data) 'killed)
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
(defun reka--create-buffer (window-wl)
  "Create and display a reka buffer for Wayland WINDOW-WL."
  (or (reka--find-buffer-for-window window-wl)
      (let ((buffer (get-buffer-create (make-temp-name "reka-window-"))))
        (with-current-buffer buffer
          (reka-mode)
          (setq-local reka--window window-wl))
        (display-buffer buffer)
        buffer)))

(defvar reka--last-focused nil
  "Last buffer for which a focus request was sent.")

(defun reka--focus-change-allowed-p ()
  "Non-nil when no interactive command or edit is in progress."
  (and (not this-command)
       (length= unread-command-events 0)
       (length= (this-single-command-keys) 0)
       (zerop (minibuffer-depth))
       (zerop (recursion-depth))))

(defun reka--update-focus-for-window (state window-wl)
  "Focus external window WINDOW-WL in STATE if it is displayed.
Return non-nil if the focus state changed.  Return nil without
changing state when the window has no parameters yet, so a later
hook run can retry."
  (if-let* ((win (ewc-object-data window-wl))
            (frame-wl (reka--frame-displaying-win win)))
      (reka--focus-window state window-wl frame-wl)
    (reka-log "reka: cannot focus window that is not displayed")
    nil))

(defun reka--update-focus-request (&rest _)
  "Reconcile Wayland focus with the selected window."
  (when-let* ((state reka--state))
    (let ((buf (window-buffer (selected-window)))
          changed)
      (unless (eq buf reka--last-focused)
        (setq changed
              (cond
               ((and (reka--is-reka-buffer buf) (reka--focus-change-allowed-p))
                (when-let* ((window-wl (buffer-local-value 'reka--window buf)))
                  (reka--update-focus-for-window state window-wl)))
               (t (reka--focus-switch-to-frame state))))
        (when changed
          (setq reka--last-focused buf)
          (reka--mark-manage-dirty state)))
      changed)))

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

(defun reka--request (object-wl request &optional arguments)
  "Send REQUEST on OBJECT using the current reka Wayland connection."
  (ewc-request (reka-state-connection reka--state)
               object-wl
               request
               arguments))

(defun reka--interface-version (client protocol interface)
  "Return XML-declared version of INTERFACE in PROTOCOL."
  (when-let* ((protocol-def (alist-get protocol
                                       (ewc-client-protocols client)))
              (interface-def (alist-get interface protocol-def)))
    ;; interface-def is: (version events requests)
    (car interface-def)))

;; TODO consider moving to ewc.el
(defun reka--decode-string (s)
  "Decode a Wayland string S as UTF-8.
Return nil if S is nil or empty."
  (when (and s (not (string-empty-p s)))
    (decode-coding-string s 'utf-8)))

(defun reka--frame-by-cond (state predicate)
  "Return the first frame ewc-object in STATE matching PREDICATE."
  (cl-loop for frame-wl in (ewc-objects (reka-state-client state) reka--tag-frame)
           for f = (ewc-object-data frame-wl)
           thereis (and f (funcall predicate f) frame-wl)))

(defun reka--frame-displaying-win (win)
  "Return the ewc frame object displaying window WIN."
  (when-let* ((p (reka-window-params win))
              (emacs-frame (reka-window-parameters-emacs-frame p))
              ((frame-live-p emacs-frame)))
    (frame-parameter emacs-frame 'reka-ewc-frame)))

(defun reka--frame-for-output (state output-wl)
  "Return frame associated to OUTPUT or nil."
  (reka--frame-by-cond state (lambda (f) (eq (reka-frame-output-wl f) output-wl))))

(defun reka--frame-without-output (state)
  "Return a frame not associated to any output or nil."
  (reka--frame-by-cond state (lambda (f) (null (reka-frame-output-wl f)))))

(defun reka--frame-with-any-output (state)
  "Return any frame with an associated output or nil."
  (reka--frame-by-cond state #'reka-frame-output-wl))

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
                 (when-let* ((client (reka-state-client state))
                             (wm-wl (ewc-first-object client
                                                   'river-window-manager-v1)))
                   (reka--request wm-wl 'manage-dirty))
               (error
                (message "reka manage-dirty error: %S" err))))
           state))))

;;; Focus helpers

(defun reka--focus-frame (state frame-wl)
  "Focus FRAME-WL in STATE.
Return non-nil if the focus state changed."
  (let ((changed
         (not (and (eq (reka-state-focus-state state) 'frame)
                   (eq
                    (reka-state-focused-frame state)
                    frame-wl)))))
    (setf (reka-state-focus-state state) 'frame
          (reka-state-focused-frame state) frame-wl
          (reka-state-focused-window state) nil)
    changed))

(defun reka--focus-window (state window-wl frame-wl)
  "Focus WINDOW-WL on FRAME-WL in STATE.
Return non-nil if the focus state changed."
  (let ((changed
         (not (and (eq (reka-state-focus-state state) 'window)
                   (eq
                    (reka-state-focused-window state)
                    window-wl)
                   (eq
                    (reka-state-focused-frame state)
                    frame-wl)))))
    (setf (reka-state-focus-state state) 'window
          (reka-state-focused-window state) window-wl
          (reka-state-focused-frame state) frame-wl)
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
  "Return current focus as (TARGET-WL . FRAME-WL), if any. TARGET-WL is
either a WINDOW-WL or FRAME-WL."
  (pcase (reka-state-focus-state state)
    ('window
     (when-let* ((window-wl (reka-state-focused-window state))
                (frame-wl (reka-state-focused-frame state)))
       (cons window-wl frame-wl)))
    ('frame
     (when-let* ((frame-wl (reka-state-focused-frame state)))
       (cons frame-wl frame-wl)))
    (_ nil)))

(defun reka--focus-invalidate (state target-wl)
  "Invalidate TARGET-WL in STATE's focus tracking, e.g. after close."
  (cond
   ((and (reka-state-focused-frame state)
         (eq (reka-state-focused-frame state) target-wl))
    (setf (reka-state-focus-state state) 'lost
          (reka-state-focused-frame state) nil
          (reka-state-focused-window state) nil))

   ((and (reka-state-focused-window state)
         (eq (reka-state-focused-window state) target-wl))
    (setf (reka-state-focus-state state) 'frame
          (reka-state-focused-window state) nil))))

;;; Fullscreen helpers

(defun reka--fs-window (fs)
  "Return the window involved in fullscreen state FS, if any."
  (pcase (reka-fs-state fs)
    ('requested (reka-fs-new fs))
    ((or 'fullscreen 'exiting) (reka-fs-window fs))
    (_ nil)))

(defun reka--select-buffer-for-window (window-wl)
  "Select the Emacs window displaying Wayland WINDOW-WL, if visible."
  (when-let* ((buf (reka--find-buffer-for-window window-wl))
              (emacs-window (get-buffer-window buf t)))
    (select-window emacs-window 'norecord)))

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

(defun reka--ensure-ls-output (state output-wl)
  "Create a layer-shell output object for OUTPUT-WL if possible."
  (when-let* ((out (ewc-object-data output-wl))
              ((null (reka-output-ls-output-wl out)))
              (client (reka-state-client state))
              (ls-wl (ewc-first-object client 'river-layer-shell-v1))
              (ls-output-id (cl-incf (ewc-client-new-id client)))
              (ls-output-wl
               (ewc-object-add client 'river-layer-shell-output-v1 ls-output-id)))
    (setf (reka-output-ls-output-wl out) ls-output-wl)
    (reka--request ls-wl 'get-output
                   `((id . ,ls-output-id)
                     (output . ,(ewc-object-id output-wl))))))

;; TODO: this ls-seat is never used ATM.
(defun reka--ensure-ls-seat (state)
  "Create a layer-shell seat object for the current seat if possible."
  (when-let* ((client (reka-state-client state))
              (seat-wl (ewc-first-object client 'river-seat-v1))
              (seat (ewc-object-data seat-wl))
              ((null (reka-seat-ls-seat-wl seat)))
              (ls-wl (ewc-first-object client 'river-layer-shell-v1))
              (ls-seat-id (cl-incf (ewc-client-new-id client)))
              (ls-seat-wl
               (ewc-object-add client
                               'river-layer-shell-seat-v1
                               ls-seat-id)))
    (setf (reka-seat-ls-seat-wl seat) ls-seat-wl)
    (reka--request ls-wl 'get-seat
                   `((id . ,ls-seat-id)
                     (seat . ,(ewc-object-id seat-wl))))))

;;; Listeners

;; Order event listeners by their order in the protocol definitions!

;;;; wayland protocol
;;;; wl-display listeners
;; TODO handle individual args and decode the message string with
;; reka--decode-string
(defun reka-on-wl-display-error (_display-wl args)
  (message "wl_display error: %S" args))

(defun reka-on-wl-display-delete-id (_display-wl args)
  (pcase-let (((map id) args))
    (when-let* ((client (reka-state-client reka--state))
                (table (ewc-client-table client))
                (object-wl (gethash id table)))
      (message "delete-id for object %d, interface=%s" id (ewc-object-interface object-wl))
      (ewc-object-remove client object-wl))))

;;;; wl-registry listeners
(defun reka-on-wl-registry-global (registry-wl args)
  (pcase-let (((map name interface version) args))
    (when-let* ((ifsym (intern (string-replace "_" "-" interface)))
                (client (reka-state-client reka--state))
                (protocols (ewc-client-protocols client))
                (protocol (ewc-find-protocol protocols ifsym))
                ((member interface reka--global-binds))
                (new-id (cl-incf (ewc-client-new-id client)))
                (xml-version (reka--interface-version client protocol ifsym))
                (bind-version
                 (if xml-version (min version xml-version) version)))
      (ewc-object-add client ifsym new-id)
      (reka-log "reka: binding global %s version %s" interface bind-version)
      (reka--request registry-wl 'bind
                     `((name . ,name)
                       (interface-len . ,(1+ (string-bytes interface)))
                       (interface . ,interface)
                       (version . ,bind-version)
                       (id . ,new-id)))
      (pcase ifsym
        ('river-layer-shell-v1
         ;; Attach layer-shell objects to existing outputs/seats in STATE."
         (reka--do 'river-output-v1 (output-wl _out reka--state)
                   (reka--ensure-ls-output reka--state output-wl))
         (reka--ensure-ls-seat reka--state))

        (_ (reka-log "reka: bound %s" ifsym))))))

;;;; river-window-management-v1 Protocol
;;;; river-window-manager-v1 listeners
(defun reka-on-river-window-manager-v1-unavailable (_wm-wl _)
  (message "reka: WM event unavailable"))

(defun reka-on-river-window-manager-v1-finished (_wm-wl _)
  (message "reka: WM event finished"))

(defun reka-on-river-window-manager-v1-manage-start (wm-wl _)
  (unwind-protect
      (condition-case err
          (reka--reconcile reka--state)
        (error
         (message "reka reconcile error: %S" err)))
    (reka--request wm-wl 'manage-finish)))

(defun reka-on-river-window-manager-v1-render-start (wm-wl _)
  (unwind-protect
      (condition-case err
          (progn
            (reka--render-frames reka--state)
            (reka--render-windows reka--state))
        (error
         (message "reka render error: %S" err)))
    (reka--request wm-wl 'render-finish)))

(defun reka-on-river-window-manager-v1-session-locked (_wm-wl _)
  (message "reka: WM event session-locked"))

(defun reka-on-river-window-manager-v1-session-unlocked (_wm-wl _)
  (message "reka: WM event session-unlocked"))

(defun reka-on-river-window-manager-v1-window (_wm-wl args)
  (pcase-let* (((map id) args))
    (ewc-object-add (reka-state-client reka--state)
                    'river-window-v1
                    id)))

(defun reka-on-river-window-manager-v1-output (_wm-wl args)
  (pcase-let* (((map id) args)
               (client (reka-state-client reka--state))
               (output-wl (ewc-object-add client 'river-output-v1 id)))
    (setf (ewc-object-data output-wl)
          (reka-output-make))
    (reka--ensure-ls-output reka--state output-wl)))

(defun reka-on-river-window-manager-v1-seat (_wm-wl args)
  (pcase-let (((map id) args)
              (client (reka-state-client reka--state)))
    (if (ewc-first-object client 'river-seat-v1)
        (message "reka does not support multi-seat")
      (let* ((seat-wl (ewc-object-add client 'river-seat-v1 id)))
        (setf (ewc-object-data seat-wl) (reka-seat-make))
        (reka--ensure-ls-seat reka--state)))))

;;;; river-window-v1 listeners
(defun reka-on-river-window-v1-closed (window-wl _)
  (let ((client (reka-state-client reka--state)))
    (when (ewc-object-tagged-p window-wl reka--tag-window)
      (reka--enqueue
       (lambda ()
         (when-let* ((buf (reka--find-buffer-for-window window-wl)))
           (kill-buffer buf)))))
    (reka--focus-invalidate reka--state window-wl)

    ;; Reset fullscreen on output if window was fullscreen.
    (reka--do 'river-output-v1 (_ out reka--state)
      (when (eq (reka--fs-window (reka-output-fullscreen out)) window-wl)
        (setf (reka-output-fullscreen out) (reka-fs))))

    (when-let* ((data (ewc-object-data window-wl))
                (node-wl (reka-surface-node-wl data)))
      (reka--request node-wl 'destroy)
      (ewc-object-remove client node-wl))

    (reka--request window-wl 'destroy)
    (ewc-object-remove client window-wl)))

(defun reka-on-river-window-v1-dimensions (window-wl args)
  (pcase-let (((map width height) args))
    (when-let* ((data (ewc-object-data window-wl))
                ((reka-window-p data)))
      (setf (reka-window-actual-width data) width
            (reka-window-actual-height data) height))))

(defun reka-on-river-window-v1-app-id (window-wl args)
  (pcase-let (((map app-id) args))
    (when-let* ((app-id (reka--decode-string app-id))
                (win (ewc-object-data window-wl))
                ((reka-window-p win)))
      (setf (reka-window-app-id win) app-id))))

(defun reka-on-river-window-v1-title (window-wl args)
  (pcase-let (((map title) args))
    (when-let* ((title (reka--decode-string title))
                (data (ewc-object-data window-wl)))
      (setf (reka-surface-title data) title)
      (when (reka-window-p data)
        (reka--enqueue
         (lambda ()
           (when-let* ((buf (reka--find-buffer-for-window window-wl)))
             (with-current-buffer buf
               (rename-buffer
                (reka--make-buffer-name
                 (reka-window-app-id data)
                 title)
                t))))))
      (when (reka-frame-p data)
        (when-let* ((emacs-frame
                     (cl-find title (frame-list)
                              :test #'equal
                              :key (lambda (f) (frame-parameter f 'name)))))
          (setf (reka-frame-emacs-frame data) emacs-frame)
          (reka--enqueue
           (lambda ()
             (when (frame-live-p emacs-frame)
               (set-frame-parameter emacs-frame 'reka-ewc-frame window-wl)))))))))

(defun reka-on-river-window-v1-fullscreen-requested (window-wl args)
  (pcase-let* (((map output) args)
               (output-wl
                (or (and (integerp output)
                         (not (zerop output))
                         (ewc-object-get (reka-state-client reka--state) output))
                    (when-let* ((w (ewc-object-data window-wl))
                                ((reka-window-p w))
                                (frame-wl (reka--frame-displaying-win w))
                                (frame (ewc-object-data frame-wl)))
                      (reka-frame-output-wl frame))
                    (when-let* ((cur (reka--focus-current reka--state))
                                (frame-wl (cdr cur))
                                (frame (ewc-object-data frame-wl)))
                      (reka-frame-output-wl frame)))))
    (if (not output-wl)
        (message "Fullscreen requested, but no output found")
      (when-let* ((out (ewc-object-data output-wl)))
        (let* ((fs (reka-output-fullscreen out))
               (previous
                (pcase (reka-fs-state fs)
                  ((or 'fullscreen 'exiting)
                   (reka-fs-window fs))
                  (_ nil))))
          (setf (reka-output-fullscreen out)
                (reka-fs :state 'requested
                         :new window-wl
                         :previous previous)))))))

(defun reka-on-river-window-v1-exit-fullscreen-requested (window-wl _)
  (reka--do 'river-output-v1 (_ out reka--state)
    (let ((fs (reka-output-fullscreen out)))
      (when (and (member (reka-fs-state fs)
                         '(requested fullscreen))
                 (eq (reka--fs-window fs) window-wl))
        (setf (reka-output-fullscreen out)
              (reka-fs :state 'exiting
                       :window (reka--fs-window fs)))))))

(defun reka-on-river-window-v1-minimize-requested (window-wl _)
  (when-let* ((data (ewc-object-data window-wl))
              ((reka-window-p data)))
    (reka--enqueue
     (lambda ()
       (when-let* ((buf (reka--find-buffer-for-window window-wl)))
         (with-current-buffer buf
           (bury-buffer)))))))

(defun reka-on-river-window-v1-unreliable-pid (window-wl args)
  (pcase-let* (((map unreliable-pid) args)
               (client (reka-state-client reka--state))
               (node-wl (ewc-object-add client 'river-node-v1)))

    (reka--request window-wl 'get-node
                   `((id . ,(ewc-object-id node-wl))))

    (if (= unreliable-pid (reka-state-pid reka--state))
        (progn
          (reka-log "Discovered new Emacs frame")
          (let ((frame (reka-frame-make
                        :node-wl node-wl)))
            (setf (ewc-object-data window-wl) frame)
            (ewc-object-tag client window-wl reka--tag-frame))
          (if (> (reka-state-pending-frames reka--state) 0)
              (cl-decf (reka-state-pending-frames reka--state))
            (reka-log "New frame was not requested by WM")))

      (reka-log "Discovered new regular external window")
      (let ((win (reka-window-make
                  :node-wl node-wl)))
        (setf (ewc-object-data window-wl) win)
        (ewc-object-tag client window-wl reka--tag-window))
      (reka--enqueue
       (lambda ()
         ;; Confirm that the Emacs-side buffer for the window was created.
         (when-let* ((win (ewc-object-data window-wl)))
           (reka--create-buffer window-wl)
           (setf (reka-window-state win) 'active)
           (reka--mark-manage-dirty reka--state)))))))

;;;; river-output-v1 listeners
(defun reka-on-river-output-v1-removed (output-wl _)
  (let ((client (reka-state-client reka--state)))
    (reka--do reka--tag-frame (_ f reka--state)
      (when (eq (reka-frame-output-wl f) output-wl)
        (setf (reka-frame-output-wl f) nil)
        (reka--enqueue
         (lambda ()
           (when-let* ((emacs-frame (reka-frame-emacs-frame f))
                       ((frame-live-p emacs-frame)))
             (delete-frame emacs-frame))))))
    (when-let* ((out (ewc-object-data output-wl))
                (ls-output-wl (reka-output-ls-output-wl out)))
      (reka--request ls-output-wl 'destroy)
      (ewc-object-remove client ls-output-wl))
    (reka--request output-wl 'destroy)
    (ewc-object-remove client output-wl)))

;; TODO: listener for wl_output, e.g. to get monitor names

(defun reka-on-river-output-v1-position (output-wl args)
  (pcase-let (((map x y) args))
    (when-let* ((out (ewc-object-data output-wl)))
      (setf (reka-output-x out) x
            (reka-output-y out) y))))

(defun reka-on-river-output-v1-dimensions (output-wl args)
  (pcase-let (((map width height) args))
    (when-let* ((out (ewc-object-data output-wl)))
      (setf (reka-output-width out) width
            (reka-output-height out) height))))

;;;; river-seat-v1 listener
(defun reka-on-river-seat-v1-window-interaction (_seat-wl args)
  (pcase-let* (((map window) args))
    (when-let* ((window-wl (ewc-object-get (reka-state-client reka--state) window)))
      (cond
       ((ewc-object-tagged-p window-wl reka--tag-frame)
        (when (reka--focus-frame reka--state window-wl)
          (setf (reka-state-focus-dirty reka--state) t)))
       ((ewc-object-tagged-p window-wl reka--tag-window)
        (if-let* ((w (ewc-object-data window-wl))
                  (frame-wl (reka--frame-displaying-win w)))
            (when (reka--focus-window reka--state window-wl frame-wl)
              (setf (reka-state-focus-dirty reka--state) t))
          (message "Window interaction for window without frame")))))))

;;;; river-xkb-bindings-v1 protocol
;;;; river-xkb-binding-v1 listeners
(defun reka-on-river-xkb-binding-v1-pressed (binding-wl _)
  (when-let* ((binding (ewc-object-data binding-wl))
              (command (reka-binding-command binding)))
    (if (eq command 'toggle-fullscreen)
        (reka--toggle-fullscreen reka--state)
      (reka--enqueue
       (lambda ()
         (push (cons t command) unread-command-events)))
      (reka--focus-switch-to-frame reka--state))))

;;;; river-layer-shell-v1 protocol
;;;; river-layer-shell-output-v1 listeners
(defun reka-on-river-layer-shell-output-v1-non-exclusive-area (ls-output-wl args)
  (pcase-let (((map x y width height) args))
    (when-let* ((out (cl-loop for output-wl in (ewc-objects
                                          (reka-state-client reka--state)
                                          'river-output-v1)
                              for data = (ewc-object-data output-wl)
                              thereis (and data
                                           (eq (reka-output-ls-output-wl data) ls-output-wl)
                                           data))))
      (setf (reka-output-x out) x
            (reka-output-y out) y
            (reka-output-width out) width
            (reka-output-height out) height))))

;;; Reconciliation

(defun reka--reconcile-frames (state)
  "Ensure each output gets one maximized Emacs frame."
  (let ((frame-requests 0))
    (reka--do 'river-output-v1 (output-wl out state)
              (if-let* ((frame-wl (reka--frame-for-output state output-wl))
                        (f (ewc-object-data frame-wl)))
                  ;; Frame already assigned: only re-propose if size changed.
                  (unless (and (eq (reka-frame-proposed-width f) (reka-output-width out))
                               (eq (reka-frame-proposed-height f) (reka-output-height out)))
                    (setf (reka-frame-proposed-width f) (reka-output-width out)
                          (reka-frame-proposed-height f) (reka-output-height out))
                    (reka--request frame-wl
                                   'propose-dimensions
                                   `((width . ,(reka-output-width out))
                                     (height . ,(reka-output-height out)))))

                (if-let* ((frame-wl (reka--frame-without-output state))
                          (f (ewc-object-data frame-wl)))
                    (progn
                      (setf (reka-frame-output-wl f) output-wl
                            (reka-frame-proposed-width f) (reka-output-width out)
                            (reka-frame-proposed-height f) (reka-output-height out))
                      (reka--request frame-wl
                                     'propose-dimensions
                                     `((width . ,(reka-output-width out))
                                       (height . ,(reka-output-height out))))
                      (reka--request frame-wl
                                     'inform-maximized)
                      (reka--request frame-wl
                                     'set-tiled
                                     `((edges . ,reka--edges-all))))
                  ;; No frame on this output yet: request one.
                  (cl-incf frame-requests))))
    (dotimes (_ (- frame-requests (reka-state-pending-frames state)))
      (reka--enqueue (lambda () (make-frame)))
      (cl-incf (reka-state-pending-frames state)))))

(defun reka--reconcile-windows (state)
  "Close killed windows and propose dimensions for active windows."
  (reka--do reka--tag-window (window-wl win state)
            (pcase (reka-window-state win)
              ;; nothing to do for window-state 'starting
              ('active
               (when-let* ((params (reka-window-params win)))
                 ;; TODO: This gets sent on every loop for all windows?
                 (reka--request window-wl
                                'set-tiled
                                `((edges . ,reka--edges-all)))

                 (reka--request window-wl
                                'propose-dimensions
                                `((width . ,(reka-window-parameters-w params))
                                  (height . ,(reka-window-parameters-h params))))))
              ('killed
               (reka--request window-wl 'close)))))

(defun reka--reconcile-bindings (state)
  "Create and enable XKB bindings."
  (when-let* ((client (reka-state-client state))
              (xkb-bindings-wl (ewc-first-object client 'river-xkb-bindings-v1))
              (seat-wl (ewc-first-object client 'river-seat-v1)))
    (maphash
     (lambda (_key binding)
       (when (eq (reka-binding-state binding) 'requested)
         (let* ((id (cl-incf (ewc-client-new-id client)))
                (binding-wl (ewc-object-add client 'river-xkb-binding-v1 id)))
           (reka--request xkb-bindings-wl 'get-xkb-binding
                          `((seat . ,(ewc-object-id seat-wl))
                            (keysym . ,(reka-binding-keysym binding))
                            (modifiers . ,(reka-binding-modifiers binding))
                            (id . ,id)))
           (setf (ewc-object-data binding-wl) binding
                 (reka-binding-state binding) 'registered)
           (reka--request binding-wl 'enable)
           (setf (reka-binding-state binding) 'enabled))))
     (reka-state-bindings state))))

(defun reka--reconcile-fullscreen (state)
  "Advance fullscreen state machines."
  (reka--do 'river-output-v1 (output-wl out state)
    (let ((fs (reka-output-fullscreen out)))
      (pcase (reka-fs-state fs)
        ('requested
         (let ((new-wl (reka-fs-new fs))
               (prev-wl (reka-fs-previous fs)))
           (when (and prev-wl (ewc-object-p prev-wl))
             (reka--request prev-wl 'inform-not-fullscreen)
             (reka--request prev-wl 'exit-fullscreen))
           (when (and new-wl (ewc-object-p new-wl))
             (reka--request new-wl 'inform-fullscreen)
             (reka--request new-wl 'fullscreen
                            `((output . ,(ewc-object-id output-wl)))))
           (setf (reka-output-fullscreen out)
                 (reka-fs :state 'fullscreen :window new-wl))))
        ('exiting
         (let ((window-wl (reka-fs-window fs)))
           (when (and window-wl (ewc-object-p window-wl))
             (reka--request window-wl 'inform-not-fullscreen)
             (reka--request window-wl 'exit-fullscreen))
           (setf (reka-output-fullscreen out) (reka-fs))))
        (_ nil)))))

(defun reka--reconcile-focus (state)
  "Update the seat focus based on STATE."
  (unless (reka--focus-current state)
    (setf (reka-state-focus-state state) 'lost))

  (when (eq (reka-state-focus-state state) 'lost)
    (when-let* ((frame-wl (reka--frame-with-any-output state)))
      (reka--focus-frame state frame-wl)))

  (when-let* ((client (reka-state-client state))
              (seat-wl (ewc-first-object client 'river-seat-v1))
              (cur (reka--focus-current state)))
    (let* ((target-wl (car cur))
           (frame-wl (cdr cur))
           (dirty (reka-state-focus-dirty state)))

      (when (and target-wl (ewc-object-p target-wl))
        (reka--request seat-wl
                       'focus-window
                       `((window . ,(ewc-object-id target-wl)))))

      (when dirty
        (when-let* ((frame-data (and frame-wl
                                    (ewc-object-data frame-wl)))
                    (output-wl (reka-frame-output-wl frame-data))
                    (out (ewc-object-data output-wl))
                    (ls-output-wl (reka-output-ls-output-wl out)))
          (reka--request ls-output-wl 'set-default))

        (unless (and frame-wl target-wl (eq target-wl frame-wl))
          (reka--enqueue
           (lambda ()
             (reka--select-buffer-for-window target-wl))))

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
  (reka--do reka--tag-frame (frame-wl frame state)
            (let ((node (reka-surface-node-wl frame))
                  (output-wl (reka-frame-output-wl frame)))
              (if (not output-wl)
                  (when (reka-frame-visible frame)
                    (setf (reka-frame-visible frame) nil)
                    (reka--request frame-wl 'hide))
                (unless (reka-frame-visible frame)
                  (setf (reka-frame-visible frame) t)
                  (reka--request frame-wl 'show))
                (when node
                  (reka--request node 'place-bottom))
                (when-let* ((out (ewc-object-data output-wl))
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
  (reka--do reka--tag-window (window-wl win state)
    (let ((node (reka-surface-node-wl win)))
      (if (not (eq (reka-window-state win) 'active))
          (reka--request window-wl 'hide)

        (if-let* ((params (reka-window-params win))
                  (frame-wl (reka--frame-displaying-win win))
                  (frame (ewc-object-data frame-wl))
                  (output-wl (reka-frame-output-wl frame))
                  (out (ewc-object-data output-wl)))
            (progn
              (reka--request window-wl 'show)

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
                (reka--request window-wl 'set-clip-box
                             `((x . 0)
                               (y . 0)
                               (width . ,clip-w)
                               (height . ,clip-h)))))

          (reka--request window-wl 'hide))))))

;;; Fullscreen toggle

(defun reka--toggle-fullscreen (state)
  "Toggle fullscreen for the currently focused external window."
  (if-let* ((cur (reka--focus-current state))
            (target-wl (car cur))
            (frame-wl (cdr cur)))
      (if (eq target-wl frame-wl)
          (reka--enqueue
           (lambda ()
             (message "reka: can not fullscreen Emacs even more!")))

        (if-let* ((frame-data (ewc-object-data frame-wl))
                  (output-wl (reka-frame-output-wl frame-data))
                  (out (ewc-object-data output-wl)))
            (let ((fs (reka-output-fullscreen out)))
              (pcase (reka-fs-state fs)
                ('none
                 (setf (reka-output-fullscreen out)
                       (reka-fs :state 'requested
                                :new target-wl))
                 (reka--mark-manage-dirty state))

                ('fullscreen
                 (setf (reka-output-fullscreen out)
                       (reka-fs :state 'exiting
                                :window (reka-fs-window fs)))
                 (reka--mark-manage-dirty state))

                (_
             (message "Invalid output state for fullscreen toggle"))))

          (message "Selected frame for fullscreen is not displayed"))))

  (message "Fullscreen requested, but nothing is focused"))

;;; Startup

(defun reka--start-wm ()
  "Connect to Wayland and initialize the reka state."
  (let ((client (ewc-client-make :protocols (reka--read-protocols))))
    (ewc-build-listeners client "reka-on-")
    (let ((connection (ewc-connect client))
          (display-wl (ewc-object-add client 'wl-display))
          (registry-wl (ewc-object-add client 'wl-registry)))

    (setq reka--state (reka-state-make :connection connection
                                       :client client))

    (reka--request display-wl 'get-registry
                   `((registry . ,(ewc-object-id registry-wl)))))))

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
