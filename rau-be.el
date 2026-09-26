;;; rau-be.el --- Backend for Rau Window Manager -*- lexical-binding: t -*-

;; Copyright (C) 2026 Thomas Koch

;; Author: Thomas Koch <thomas@koch.ro>
;; Version: 0.3
;; Keywords: frames
;; URL: https://github.com/thkoch2001/rau
;; Package-Requires: ((emacs "31.1")
;;                    (lgr "0.1.0"))

;;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'cl-lib)
(require 'ewc)
(require 'map)          ; needed for `map' pcase pattern
(require 'pcase)
(require 'rau-lib)
(require 'seq)
(require 'subr-x)

;;; Configuration and state

(defvar rau--state nil
  "Current global rau WM state.")

;;; State structs

(cl-defstruct (rau--output (:constructor rau--output-make))
  "State for a River output."
  (dimensions '(0 . 0))
  ;; emacs frame-id
  (frame-id nil)
  (fullscreen-window-wl nil :type (or null ewc-object))
  (ls-output-wl nil :type ewc-object)
  (position '(0 . 0)))

(cl-defstruct (rau--ls-output (:constructor rau--ls-output-make))
  "State for a River output."
  (output-wl nil :type ewc-object)
  (non-excl-position '(0 . 0))
  (non-excl-dimensions '(0 . 0)))

(defconst rau--tag-outputframe :rau--frame
  "Tag for `river-window-v1' objects that are Emacs frames.")

(defconst rau--tag-external :rau--external
  "Tag for `river-window-v1' objects that are external windows.")

(defconst rau--tag-floating :rau--floating
  "Tag for `river-window-v1' objects that are floating.")

(cl-defstruct (rau--window (:constructor rau--window-make))
  "State for a River window."
  actual-dimensions
  app-id
  (dimensions-hint-max '(0 . 0))
  (dimensions-hint-min '(0 . 0))
  ;; edges only for external, tiled windows
  edges
  ;; emacs frame-id, either the own for emacs output frame or of the
  ;; containing frame for tiled external window
  frame-id
  (node-wl nil :type ewc-object)
  parent-wl
  pid
  title)

(cl-defstruct (rau--seat (:constructor rau--seat-make))
  "State for a River seat."
  (ls-seat-wl nil :type ewc-object))

(cl-defstruct (rau--binding (:constructor rau--binding-make))
  "State for one global XKB binding."
  event
  key
  keysym
  modifiers
  needs-focus
  layout
  allow-when-locked)

(cl-defstruct (rau--state (:constructor rau--state-make))
  "Holds the state of the rau Wayland client."
  (client nil :type ewc-client)
  session-locked

  ;;; Focus tracking.
  ;; id of ewc-object for which the last focus request was sent
  ;; This could be a focus_window or a focus_shell_surface request
  (focus-last-id -1)

  manage-dirty-timer
  ;; manage queue: list of (ewc-object 'request args) for the next manage
  ;; cycle
  manage-queue)

(ewc-define-data-accessors rau--output)
(ewc-define-data-accessors rau--ls-output)
(ewc-define-data-accessors rau--window)
(ewc-define-data-accessors rau--seat)
(ewc-define-data-accessors rau--binding)

;;; Data access helpers

(defun rau--outframe-wl-output-wl (frame-wl)
  (when-let* ((frame-id (rau--window-wl-frame-id frame-wl))
              (client (rau--state-client rau--state)))
    (cl-find frame-id
             (ewc-objects client 'river-output-v1)
             :key #'rau--output-wl-frame-id)))

(defun rau--frame-wl-for-extwin-wl (window-wl)
  "Return the Emacs outputframe window-wl displaying external
WINDOW-WL."
  (when-let* ((client (rau--state-client rau--state))
              (outputframes (ewc-objects client rau--tag-outputframe))
              (frame-id (rau--window-wl-frame-id window-wl)))
    (cl-loop for frame-wl in outputframes
             if (eq frame-id (rau--window-wl-frame-id frame-wl))
             return frame-wl)))

(defun rau--dimensions-for-window-wl (window-wl)
  (pcase-let (((and edges `(,left ,top ,right ,bottom))
               (rau--window-wl-edges window-wl)))
    (when (and edges (all 'integerp edges))
      `(,(- right left) . ,(- bottom top)))))

(defun rau--dimensions-for-outputframe (output-wl)
  "Get dimensions from output-wl or its ls-output-wl non-exclusive-area."
  (if-let* ((ls-output-wl (rau--output-wl-ls-output-wl output-wl))
            (non-excl-dimensions (rau--ls-output-wl-non-excl-dimensions ls-output-wl))
            ((not (equal '(0 . 0) non-excl-dimensions))))
      non-excl-dimensions
    (rau--output-wl-dimensions output-wl)))

(defun rau--position-for-outputframe (output-wl)
  "Get position from output-wl or its ls-output-wl non-exclusive-area."
  (if-let* ((ls-output-wl (rau--output-wl-ls-output-wl output-wl))
            (non-excl-position (rau--ls-output-wl-non-excl-position ls-output-wl))
            ((not (equal '(0 . 0) non-excl-position))))
      non-excl-position
    (rau--output-wl-position output-wl)))


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

(defun rau--make-buffer-name (app-id title)
  "Return a buffer name string using APP-ID and TITLE."
  (let ((title-trunc (if (> (length title) 40)
                         (format "%s…" (substring title 0 40))
                       title)))
    (if app-id (concat title-trunc " - " app-id)
      title-trunc)))

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

(defun rau--remove (object-wl)
  "Remove object from ewc objects table.
See also `rau-on-wl-display-delete-id'."
  (ewc-object-remove (rau--state-client rau--state) object-wl))

(defun rau--request (object-wl request &optional arguments)
  "Send REQUEST on OBJECT-WL (or id) using the current rau Wayland client."
  (let* ((client (rau--state-client rau--state))
         (o
          (if (ewc-object-p object-wl)
              object-wl
            (ewc-object-get client object-wl))))
    (ewc-request client o request arguments)))

;;; Manage cycle preparation

(defun rau--mark-manage-dirty ()
  "Mark that a new manage sequence is needed."
  (unless (rau--state-manage-dirty-timer rau--state)
    (setf (rau--state-manage-dirty-timer rau--state)
          (run-at-time
           0 nil
           (lambda ()
             (setf (rau--state-manage-dirty-timer rau--state) nil)
             (rau--condition-case
              "request-manage-dirty"
              (when-let* ((client (rau--state-client rau--state))
                          (wm-wl (ewc-first-object client
                                                   'river-window-manager-v1)))
                (rau--request wm-wl 'manage-dirty))))))))

(defun rau--manage-enqueue (ewc-object request &optional args)
  "Enqueue REQUEST on EWC-OBJECT (or id) with ARGS for next manage cycle."
  (push `(,ewc-object ,request ,args) (rau--state-manage-queue rau--state))
  (rau--mark-manage-dirty))

;;; Focus

(defun rau--request-focus (target-wl &optional force)
  "Request focus for TARGET-WL.
Queues the focus-window request and updates Emacs state.  If FORCE is
non-nil, bypass the comparission with focus-last-id from rau--state for
situations where focus changed without us knowing (session-lock, layer surface)."
  (when-let* ((target-id (ewc-object-id target-wl))
              ((or force
                   (not (eq target-id (rau--state-focus-last-id rau--state)))))
              (client (rau--state-client rau--state))
              (seat-wl (ewc-first-object client 'river-seat-v1)))
    (lgr-debug rau--lgr "request focus-window id=%d title=%s"
               target-id
               (rau--window-wl-title target-wl))

    ;; Queue the actual Wayland focus request
    (rau--manage-enqueue seat-wl 'focus-window `((window . ,target-id)))
    (setf (rau--state-focus-last-id rau--state) target-id)

    ;; Queue the layer-shell default output update
    (when-let* ((frame-wl (if (ewc-object-tagged-p target-wl rau--tag-external)
                              (rau--frame-wl-for-extwin-wl target-wl)
                            target-wl))
                (output-wl (rau--outframe-wl-output-wl frame-wl))
                (ls-output-wl (rau--output-wl-ls-output-wl output-wl)))
      (rau--manage-enqueue ls-output-wl 'set-default))

    (when-let* (((ewc-object-tagged-p target-wl rau--tag-external)))
      (rau--fe #'rau--tasks-enqueue #'rau--task-select-window target-id))))

(defun rau--request-focus-by-id (&optional target-id force)
  "Request focus for the window with TARGET-ID or the last focused.
See `rau--request-focus' for details on FORCE."
  (let ((id (or target-id (rau--state-focus-last-id rau--state))))
    (when-let* (id
                ((not (equal -1 id)))
                (client (rau--state-client rau--state))
                (target-wl (ewc-object-get client id)))
      (rau--request-focus target-wl force))))

;;; Keybindings

(defun rau--bind-parsed-keys (parsed-keys)
  "Actually bind PARSED-KEYS.  See `rau-bind-keys' for the public function."
  (when-let* ((client (rau--state-client rau--state))
              (xkb-bindings-wl (ewc-first-object client 'river-xkb-bindings-v1))
              (seat-wl (ewc-first-object client 'river-seat-v1))
              (seat-id (ewc-object-id seat-wl)))
    (let ((bindings (mapcar (lambda (k) (apply #'rau--binding-make k)) parsed-keys))
          (existing-bindings
           (mapcar #'ewc-object-data (ewc-objects client 'river-xkb-binding-v1))))
      (dolist (binding bindings)
        (dolist (existing existing-bindings)
          (when (or (equal (rau--binding-key binding)
                           (rau--binding-key existing))
                    (and
                     (equal (rau--binding-keysym binding)
                            (rau--binding-keysym existing))
                     (equal (rau--binding-modifiers binding)
                            (rau--binding-modifiers existing))))
            (error "Key binding %s already exists: %s"
                   (rau--binding-key binding)
                   (rau--binding-key existing))))
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
           'enable))))))


;;; Fullscreen toggle

(defun rau--fullscreen-exit (window-wl output-wl)
  "Exit fullscreen for WINDOW-WL on OUTPUT-WL.
Also resets the fullscreen-window-wl slot of OUTPUT-WL to nil.  When
switching fullscreen between windows, call this before
`rau--fullscreen-enter'."
  (lgr-debug rau--lgr "fs exit window-id=%d output-id=%d"
             (ewc-object-id window-wl)
             (ewc-object-id output-wl))
  (rau--manage-enqueue window-wl 'inform-not-fullscreen)
  (rau--manage-enqueue window-wl 'exit-fullscreen)
  (when-let* ((dimensions (rau--dimensions-for-window-wl window-wl)))
    (rau--manage-enqueue window-wl 'propose-dimensions
                         `((width . ,(car dimensions))
                           (height . ,(cdr dimensions)))))
  (setf (rau--output-wl-fullscreen-window-wl output-wl) nil))

(defun rau--fullscreen-enter (window-wl output-wl)
  "Enter fullscreen for WINDOW-WL on OUTPUT-WL."
  (lgr-debug rau--lgr "fs enter window-id=%d output-id=%d"
             (ewc-object-id window-wl)
             (ewc-object-id output-wl))
  (rau--manage-enqueue window-wl 'inform-fullscreen)
  (rau--manage-enqueue window-wl 'fullscreen
                       `((output . ,(ewc-object-id output-wl))))
  (setf (rau--output-wl-fullscreen-window-wl output-wl) window-wl))

(defun rau--toggle-fullscreen-by-window-id (window-id)
  "Toggle fullscreen for WINDOW-ID."
  (lgr-debug rau--lgr "Toggle fs window-id=%S" window-id)
  (if-let* ((window-wl (ewc-object-get (rau--state-client rau--state) window-id))
            (frame-wl (rau--frame-wl-for-extwin-wl window-wl))
            (output-wl (rau--outframe-wl-output-wl frame-wl)))
      (let ((current-fs (rau--output-wl-fullscreen-window-wl output-wl)))
        (if current-fs
            (rau--fullscreen-exit current-fs output-wl)
          (rau--fullscreen-enter window-wl output-wl)))
    (lgr-error rau--lgr "toggle-fs: boundp window-wl=%S, frame-wl=%S, output-wl=%S"
               (boundp window-wl) (boundp frame-wl) (boundp output-wl))))

;;; Layer shell attachment helpers

(defun rau--ensure-ls-output (output-wl)
  "Create a layer-shell output object for OUTPUT-WL if possible."
  (when-let* (((null (rau--output-wl-ls-output-wl output-wl)))
              (client (rau--state-client rau--state))
              (ls-wl (ewc-first-object client 'river-layer-shell-v1))
              (ls-output-wl
               (ewc-object-add client 'river-layer-shell-output-v1)))
    (setf (rau--output-wl-ls-output-wl output-wl) ls-output-wl
          (ewc-object-data ls-output-wl) (rau--ls-output-make :output-wl output-wl))
    (rau--request ls-wl 'get-output
                  `((id . ,(ewc-object-id ls-output-wl))
                    (output . ,(ewc-object-id output-wl))))))

(defun rau--ensure-ls-seat (seat-wl)
  "Create a layer-shell seat object for the current seat if possible."
  (when-let* (((null (rau--seat-wl-ls-seat-wl seat-wl)))
              (client (rau--state-client rau--state))
              (ls-wl (ewc-first-object client 'river-layer-shell-v1))
              (ls-seat-wl
               (ewc-object-add client 'river-layer-shell-seat-v1)))
    (setf (rau--seat-wl-ls-seat-wl seat-wl) ls-seat-wl)
    (rau--request ls-wl 'get-seat
                   `((id . ,(ewc-object-id ls-seat-wl))
                     (seat . ,(ewc-object-id seat-wl))))
    (rau--fe #'rau--tasks-enqueue #'run-hooks 'rau-ready-hook)
    (rau--mark-manage-dirty)))

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
                (xml-version (ewc-interface-version client ifsym))
                (bind-version
                 (if xml-version (min version xml-version) version))
                (global (ewc-object-add client ifsym)))

      (lgr-debug rau--lgr "rau: binding global %s version %s" interface bind-version)
      (rau--request registry-wl 'bind
                     `((name . ,name)
                       (interface-len . ,(1+ (string-bytes interface)))
                       (interface . ,interface)
                       (version . ,bind-version)
                       (id . ,(ewc-object-id global))))
      (pcase ifsym
        ('river-layer-shell-v1
         ;; Attach layer-shell objects to existing outputs/seats in STATE."
         (rau--do 'river-output-v1 output-wl rau--state
                  (rau--ensure-ls-output output-wl))
         (rau--do 'river-seat-v1 seat-wl rau--state
                   (rau--ensure-ls-seat seat-wl)))))))

;;;; river-window-management-v1 Protocol
;;;; river-window-manager-v1 listeners
(defun rau--on-river-window-manager-v1-unavailable (_wm-wl _)
  (message "rau: WM event unavailable"))

(defun rau--on-river-window-manager-v1-finished (_wm-wl _)
  (message "rau: WM event finished"))

(defun rau--on-river-window-manager-v1-manage-start (wm-wl _)
  (rau--manage-queue-send)
  (rau--reconcile-frames)
  (rau--reconcile-windows)

  (rau--request wm-wl 'manage-finish))

(defun rau--on-river-window-manager-v1-render-start (wm-wl _)
  (rau--render-frames)
  (rau--render-windows)
  (rau--request wm-wl 'render-finish))

(defun rau--on-river-window-manager-v1-session-locked (_wm-wl _)
  (setf (rau--state-session-locked rau--state) t))

(defun rau--on-river-window-manager-v1-session-unlocked (_wm-wl _)
  (setf (rau--state-session-locked rau--state) nil)
  (rau--request-focus-by-id nil t))

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
    (setf (ewc-object-data output-wl) (rau--output-make))
    (rau--fe #'rau--tasks-enqueue #'rau--task-assign-emacs-frame id)
    (rau--ensure-ls-output output-wl)))

(defun rau--on-river-window-manager-v1-seat (_wm-wl args)
  (pcase-let (((map id) args)
              (client (rau--state-client rau--state)))
    (if (ewc-first-object client 'river-seat-v1)
        (message "rau does not support multi-seat")
      (let* ((seat-wl (ewc-object-add client 'river-seat-v1 id)))
        (setf (ewc-object-data seat-wl) (rau--seat-make))
        (rau--ensure-ls-seat seat-wl)))))

;;;; river-window-v1 listeners
(defun rau--on-river-window-v1-closed (window-wl _)
  (when-let* (((ewc-object-tagged-p window-wl rau--tag-external)))
    (rau--fe #'rau--tasks-enqueue #'rau--task-kill-buffer (ewc-object-id window-wl)))
  (when-let* ((node-wl (rau--window-wl-node-wl window-wl)))
    (rau--request node-wl 'destroy))
  (rau--request window-wl 'destroy)
  (rau--remove window-wl)

  ;; Reset fullscreen on output if window was fullscreen.
  (when (ewc-object-tagged-p window-wl rau--tag-external)
    (rau--do 'river-output-v1 output-wl rau--state
      (when (eq (rau--output-wl-fullscreen-window-wl output-wl) window-wl)
        (setf (rau--output-wl-fullscreen-window-wl output-wl) nil)))))

(defun rau--on-river-window-v1-dimensions-hint (window-wl args)
  (pcase-let (((map min-width min-height max-width max-height) args))
    (setf (rau--window-wl-dimensions-hint-min window-wl) `(,min-width . ,min-height)
          (rau--window-wl-dimensions-hint-max window-wl) `(,max-width . ,max-height))))

(defun rau--on-river-window-v1-dimensions (window-wl args)
  (pcase-let (((map width height) args))
    (setf (rau--window-wl-actual-dimensions window-wl) `(,width . ,height))))

(defun rau--on-river-window-v1-app-id (window-wl args)
  (pcase-let (((map app-id) args))
    (when-let* ((app-id (ewc-to-utf8 app-id)))
      (setf (rau--window-wl-app-id window-wl) app-id))))

(defun rau--maybe-new-outputframe-window (window-wl title)
  (unless (ewc-object-tagged-p window-wl rau--tag-outputframe)
    (lgr-debug rau--lgr "Discovered new Emacs frame by title: %s" title)
    (ewc-object-tag (rau--state-client rau--state)
                    window-wl rau--tag-outputframe)
    (rau--manage-enqueue window-wl 'inform-maximized)
    (rau--manage-enqueue window-wl 'set-tiled `((edges . ,rau--edges-all)))
    (let ((node-wl (rau--window-wl-node-wl window-wl)))
      (rau--manage-enqueue node-wl 'place-bottom))

    (rau--fe #'rau--tasks-enqueue #'rau--task-link-emacs-frame-to-window
                        title
                        (ewc-object-id window-wl))))

(defun rau--maybe-new-external-window (window-wl title)
  (unless (ewc-object-tagged-p window-wl rau--tag-external)
    (lgr-debug rau--lgr "Discovered new regular external window with title %s." title)
    (when (not (null (rau--window-wl-parent-wl window-wl)))
      (ewc-object-tag (rau--state-client rau--state)
                      window-wl rau--tag-floating))
    (ewc-object-tag (rau--state-client rau--state)
                    window-wl rau--tag-external)
    (rau--fe #'rau--tasks-enqueue #'rau--task-setup-new-external-window (ewc-object-id window-wl))))

(defun rau--on-river-window-v1-title (window-wl args)
  "Handle title event for WINDOW-WL.
Also categorizes the window based on the title prefix into Emacs
outputframe or external window."
  (pcase-let* (((map title) args)
               (title (ewc-to-utf8 title)))
    (setf (rau--window-wl-title window-wl) title)

    (if (string-prefix-p "rau-frame-" title)
        (rau--maybe-new-outputframe-window window-wl title)
      (rau--maybe-new-external-window window-wl title))

    ;; Handle title updates for already categorized objects
    (when-let* (((ewc-object-tagged-p window-wl rau--tag-external))
                (name (rau--make-buffer-name
                       (rau--window-wl-app-id window-wl)
                       title)))
      (rau--fe #'rau--tasks-enqueue #'rau--task-rename-buffer
                          (ewc-object-id window-wl)
                          name))))

(defun rau--on-river-window-v1-parent (window-wl args)
  (pcase-let* (((map object) args)
               (client (rau--state-client rau--state)))
    (when-let* ((parent-wl (ewc-object-get client object)))
      (setf (rau--window-parent-wl window-wl) parent-wl)
      (when-let* (((ewc-object-tagged-p window-wl rau--tag-external)))
        (ewc-object-tag client rau--tag-floating)))))

(defun rau--on-river-window-v1-fullscreen-requested (window-wl args)
  (when (ewc-object-tagged-p window-wl rau--tag-external)
    (pcase-let* (((map output) args)
                 (output-wl
                  ;; Find output for fullscreen window
                  ;; 1. optional event arg output
                  ;; 2. output showing window-wl
                  ;; 3. TODO: output currently having focus
                  (or (ewc-object-get (rau--state-client rau--state) output)
                      (when-let*
                          ((frame-wl (rau--frame-wl-for-extwin-wl window-wl)))
                        (rau--outframe-wl-output-wl frame-wl)))))
      (if (not output-wl)
          (message "Fullscreen requested, but no output found")
        (let ((current-fs (rau--output-wl-fullscreen-window-wl output-wl)))
          (when (and current-fs (not (eq current-fs window-wl)))
            (rau--fullscreen-exit current-fs output-wl)))
        (rau--fullscreen-enter window-wl output-wl)))))

(defun rau--on-river-window-v1-exit-fullscreen-requested (window-wl _)
  (rau--do 'river-output-v1 output-wl rau--state
    (when (eq (rau--output-wl-fullscreen-window-wl output-wl) window-wl)
      (rau--fullscreen-exit window-wl output-wl))))

(defun rau--on-river-window-v1-minimize-requested (window-wl _)
  (when-let* (((ewc-object-tagged-p window-wl rau--tag-external)))
    (rau--fe #'rau--tasks-enqueue #'rau--task-minimize-window (ewc-object-id window-wl))))

(defun rau--on-river-window-v1-unreliable-pid (window-wl args)
  (pcase-let* (((map unreliable-pid) args))
    (setf (rau--window-wl-pid window-wl) unreliable-pid)))

;;;; river-output-v1 listeners
(defun rau--on-river-output-v1-removed (output-wl _)
  ;; TODO: check whether we lost focus
  (let ((client (rau--state-client rau--state)))
    (when-let* ((frame-id (rau--output-wl-frame-id output-wl)))
      (rau--fe #'rau--tasks-enqueue #'rau--task-delete-frame frame-id))
    ;; TODO: also enqueue the below three actions, look out for race conditions
    (when-let* ((ls-output-wl (rau--output-wl-ls-output-wl output-wl)))
      (rau--request ls-output-wl 'destroy))
    (rau--request output-wl 'destroy)
    (rau--remove output-wl)))

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
    (lgr-debug rau--lgr "window interaction with %d" window)
    (rau--request-focus-by-id window)))

;;;; river-xkb-bindings-v1 protocol
;;;; river-xkb-binding-v1 listeners
(defun rau--on-river-xkb-binding-v1-pressed (binding-wl _)
  (unless (and (rau--state-session-locked rau--state)
               (not (rau--binding-wl-allow-when-locked binding-wl)))
    (let* ((event (rau--binding-wl-event binding-wl))
           (needs-focus (rau--binding-wl-needs-focus binding-wl)))
      (rau--fe #'rau--tasks-enqueue #'rau--task-consume-key-event event needs-focus)

      ;; If focus is with external window then switch to underlying emacs
      ;; frame such that following keypresses go to emacs
      (when-let* (needs-focus
                  (window-id (rau--state-focus-last-id rau--state))
                  (client (rau--state-client rau--state))
                  (window-wl (ewc-object-get client window-id))
                  ((ewc-object-tagged-p window-wl rau--tag-external))
                  (target-wl (rau--frame-wl-for-extwin-wl window-wl)))
        (lgr-debug rau--lgr "switch focus to emacs frame for key pressed.")
        (rau--request-focus target-wl)))))

;;;; river-layer-shell-v1 protocol
;;;; river-layer-shell-output-v1 listeners
(defun rau--on-river-layer-shell-output-v1-non-exclusive-area (ls-output-wl args)
  (pcase-let (((map x y width height) args))
    (setf (rau--ls-output-wl-non-excl-position ls-output-wl) `(,x . ,y)
          (rau--ls-output-wl-non-excl-dimensions ls-output-wl) `(,width . ,height))))

;;;; river-layer-shell-seat-v1 listeners
(defun rau--on-river-layer-shell-seat-v1-focus-none (_ls-seat-wl _)
  "Give focus back to the last window that had it."
  (rau--request-focus-by-id nil t))

;;; Manage cycle

(defun rau--reconcile-frames ()
  "Ensure each output gets one maximized Emacs frame."
  (let ((client (rau--state-client rau--state)))
    (rau--do 'river-output-v1 output-wl rau--state
      (lgr-trace rau--lgr "reconcile output: id=%d." (ewc-object-id output-wl))
      (if-let* ((frame-id (rau--output-wl-frame-id output-wl))
                (frame-wl (cl-find
                           frame-id
                           (ewc-objects client rau--tag-outputframe)
                           :key #'rau--window-wl-frame-id)))
          (let ((dimensions (rau--dimensions-for-outputframe output-wl)))
            (lgr-trace rau--lgr "frame found: id=%d." (ewc-object-id frame-wl))
            (rau--request frame-wl
                          'propose-dimensions
                          `((width . ,(car dimensions))
                            (height . ,(cdr dimensions)))))

        (lgr-debug rau--lgr "no frame found for output, waiting for client state.")))))

(defun rau--reconcile-window-floating (window-wl)
  (rau--request window-wl 'set-tiled '((edges . 0)))
  ;; propose 0, allow window do decide its own dimensions
  (rau--request window-wl 'propose-dimensions '((width . 0) (height . 0)))
  (rau--request window-wl 'use-csd))

(defun rau--reconcile-window-tiled (window-wl)
  (rau--request window-wl
                'set-tiled
                `((edges . ,rau--edges-all)))
  (when-let* ((dimensions (rau--dimensions-for-window-wl window-wl)))
    (rau--request window-wl
                  'propose-dimensions
                  `((width . ,(car dimensions))
                    (height . ,(cdr dimensions))))))

(defun rau--reconcile-windows ()
  "Close killed windows and propose dimensions for active windows."
  (rau--do rau--tag-external window-wl rau--state
    (if (ewc-object-tagged-p window-wl rau--tag-floating)
        (rau--reconcile-window-floating window-wl)
      (rau--reconcile-window-tiled window-wl))))

(defun rau--manage-queue-send ()
  "Run the manage-sequence reconciliation."
  (rau--condition-case
   "reconcile-manage-requests"
   (let ((manage-requests (nreverse (rau--state-manage-queue rau--state))))
     (setf (rau--state-manage-queue rau--state) nil)
     (dolist (request manage-requests)
       (rau--request (nth 0 request) (nth 1 request) (nth 2 request))))))

;;; Render cycle

(defun rau--render-frames ()
  "Run the render-sequence reconciliation for frames."
  (rau--do rau--tag-outputframe frame-wl rau--state
           (when-let* ((node-wl (rau--window-wl-node-wl frame-wl))
                       (output-wl (rau--outframe-wl-output-wl frame-wl))
                       (position (rau--position-for-outputframe output-wl)))
             (lgr-trace rau--lgr "render frame %d for output %d."
                      (ewc-object-id frame-wl)
                      (ewc-object-id output-wl))
             (rau--request node-wl 'set-position
                           `((x . ,(car position))
                             (y . ,(cdr position)))))))

(defun rau--render-window-floating (window-wl)
  (when-let* ((node-wl (rau--window-wl-node-wl window-wl)))
    (rau--request window-wl 'show)
    (rau--request node-wl 'place-top)))

(defun rau--render-window-tiled (window-wl)
  (when-let* ((node-wl (rau--window-wl-node-wl window-wl)))
    (if-let* ((frame-wl (rau--frame-wl-for-extwin-wl window-wl))
              (output-wl (rau--outframe-wl-output-wl frame-wl))
              (frame-node-wl (rau--window-wl-node-wl frame-wl))
              (frame-node-id (ewc-object-id frame-node-wl)))
        (pcase-let* ((`(,left ,top ,_right ,_bottom)
                      (rau--window-wl-edges window-wl))
                     (position (rau--position-for-outputframe output-wl))
                     (dimensions (rau--window-wl-actual-dimensions window-wl))
                     (clip (or dimensions (rau--dimensions-for-window-wl window-wl))))
          (rau--request window-wl 'show)

          (rau--request node-wl 'set-position
                        `((x . ,(+ left (car position)))
                          (y . ,(+ top (cdr position)))))

          ;; Place window above the Emacs outputframe window but not at the top
          ;; In theory this should leave the top for floating windows
          (rau--request node-wl 'place-above `((other . ,frame-node-id)))

          (rau--request window-wl 'set-clip-box
                        `((x . 0)
                          (y . 0)
                          (width . ,(car clip))
                          (height . ,(cdr clip)))))

      (rau--request window-wl 'hide))))

(defun rau--render-windows ()
  "Run the render-sequence reconciliation for windows."
  (rau--do rau--tag-external window-wl rau--state
    (if (ewc-object-tagged-p window-wl rau--tag-floating)
        (rau--render-window-floating window-wl)
      (rau--render-window-tiled window-wl))))

;;; Receive fe-ui-state

(defun rau--update-fe-ui-state (state)
  ;; TODO here we could build a visibility diff to send show and hide only when necessary
  (rau--do rau--tag-external window-wl rau--state
    (setf (rau--window-wl-edges window-wl) nil
          (rau--window-wl-frame-id window-wl) nil))
  (let ((extwins (alist-get 'extwins state))
        (outframes (alist-get 'outframes state))
        (client (rau--state-client rau--state)))
    (dolist (win extwins)
      (when-let* ((window-id (alist-get 'window-id win))
                  (window-wl (ewc-object-get client window-id)))
        (setf (rau--window-wl-edges window-wl) (alist-get 'edges win)
              (rau--window-wl-frame-id window-wl) (alist-get 'frame-id win))))
    (dolist (outframe outframes)
      (let* ((window-id (alist-get 'window-id outframe))
             (output-id (alist-get 'output-id outframe))
             (frame-id (alist-get 'frame-id outframe))
             (window-wl (ewc-object-get client window-id))
             (output-wl (ewc-object-get client output-id)))
        (when window-wl
          (setf (rau--window-wl-frame-id window-wl) frame-id))
        (when output-wl
          (setf (rau--output-wl-frame-id output-wl) frame-id))))))
  ;; TODO mark-manage-dirty?

(defun rau--connect-river ()
  (let* ((interfaces (rau--read-protocols))
         (client (ewc-start interfaces "rau--on-")))
    (setq rau--state (rau--state-make :client client))))

;;; Backend RPC State
(defvar rau--rpc-client-proc nil
  "Frontend client process.")
(defvar rau--rpc-read-marker nil)
(defvar rau--rpc-recv-buffer nil)

(defun rau--rpc-server-log (server connection msg)
  (setq rau--rpc-client-proc connection
        rau--rpc-recv-buffer (get-buffer-create "*rau--rpc-recv-buffer*"))
  (lgr-info rau--lgr "new connection: %S" msg)
  (rau--connect-river))

(defun rau-be-main ()
  "Entry point for the backend subprocess."

  ;; TODO add a way to load a config file for the backend
  (let ((appender
         (lgr-set-layout
          (lgr-appender-journald)
          (lgr-layout-format
           :format "%m"))))
    (let ((lgr (lgr-get-logger "ewc")))
      (lgr-add-appender lgr appender)
      (lgr-set-threshold lgr lgr-level-trace))
    (let ((lgr (lgr-get-logger "rau")))
      (lgr-add-appender lgr appender)
      (lgr-set-threshold lgr lgr-level-trace)
      (setf rau--lgr lgr)))

  (let ((sock-file (pop command-line-args-left)))
    (lgr-info rau--lgr "creating socket at %S" sock-file)
    (make-network-process
     :name "rau-be-rpc"
     :buffer " *rau-be-rpc*" ;; TODO remove?
     :family 'local
     :service sock-file
     ;; TODO try instead of family and service:
     ;; :local sock-file
     :server t
     :filter #'rau--be-rpc-filter
     :noquery t
     :log #'rau--rpc-server-log))

  (lgr-debug rau--lgr "looping")
  (while nil
    ;; TODO: sleep-for?
    ;; https://github.com/nicferrier/elnode/blob/master/elnode.el#L2491
    ;; https://github.com/skeeto/emacs-web-server/blob/master/simple-httpd.el#L360
    ;; Maybe don't run in batch mode?
    (accept-process-output nil 0.1)))

(defun rau--be-rpc-handle-message (proc msg)
  "Execute the requested function. Return values are ignored."
  (condition-case err
      (apply (car msg) (cdr msg))
    (error (message "Rau backend error handling %S: %S" msg err))))

(defun rau--be-rpc-filter (proc string)
  "Process filter for incoming frontend requests."
  (with-current-buffer rau--rpc-recv-buffer
    (goto-char (point-max))
    (save-excursion (insert string))
    (unless rau--rpc-read-marker
      (setq rau--rpc-read-marker (point-min-marker)))
    (while (search-forward "\n" nil t)
      (let ((line (buffer-substring-no-properties rau--rpc-read-marker (1- (point)))))
        (set-marker rau--rpc-read-marker (point))
        (when (string-match "^\"\\(.*\\)\"$" line)
          (let* ((b64 (match-string 1 line))
                 (decoded (ignore-errors (decode-coding-string
                                          (base64-decode-string b64)
                                          'utf-8-emacs-unix)))
                 (sexp (and decoded (ignore-errors (car (read-from-string decoded))))))
            (when sexp
              (lgr-trace rau--lgr "recv sexp: %S" sexp)
              (rau--be-rpc-handle-message proc sexp))))))))

(defun rau--be-rpc-send (proc sexp)
  "Send SEXP to the frontend using Base64 encoding."
  (if (process-live-p proc)
      (with-temp-buffer
        (message "sending sexp to fe: %S" sexp)
      (let (print-level print-length
            (print-escape-nonascii t)
            (print-circle t))
        (prin1 sexp (current-buffer))
        (encode-coding-region (point-min) (point-max) 'utf-8-emacs-unix)
        (base64-encode-region (point-min) (point-max) t)
        (goto-char (point-min)) (insert ?\")
        (goto-char (point-max)) (insert ?\" ?\n)
        (process-send-region proc (point-min) (point-max))))
    (error "process not live")))

(defun rau--fe (event &rest args)
  "Send an asynchronous function call/event to the frontend."
  (if rau--rpc-client-proc
      (rau--be-rpc-send rau--rpc-client-proc (cons event args))
    (error "rau--rpc-client-proc nil")))

(provide 'rau-be)
