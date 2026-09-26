;;; rau.el --- Wayland Window Manager based on River -*- lexical-binding: t -*-

;; Copyright (C) 2026 Thomas Koch

;; Author: Thomas Koch <thomas@koch.ro>
;; Version: 0.3
;; Keywords: frames
;; URL: https://github.com/thkoch2001/rau
;; Package-Requires: ((emacs "31.1")
;;                    (lgr "0.1.0"))

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
(require 'rau-lib)
;; TODO (require 'map)          ; needed for `map' pcase pattern
;; TODO (require 'pcase)
;; TODO (require 'seq)
;; TODO (require 'subr-x)

(defgroup rau nil
  "Rau - Emacs swimming in the river."
  :group 'environment
  :prefix "rau-")

(defcustom rau-ready-hook nil
  "Hook run when rau is ready.  At the moment this means that all global
Wayland objects have been registered."
  :type 'hook)

(defvar rau--fe-state nil
  "Current global rau WM front-end state.")

(cl-defstruct (rau--fe-state (:constructor rau--fe-state-make))
  last-send-state
  ;; task queue: list of (FN ARGS...).
  ;; TODO: move to rau--fe-state
  task-queue
  task-timer)

;;; Data access helpers

(defun rau--window-id-for-emacs-window (emacs-window)
  "Return window-id for any EMACS-WINDOW rau-mode or not.
For a window with a rau-mode buffer return window-id pointing to an
external window. For all other buffers return the window-id of the emacs
frame."
  (if-let* ((buffer (window-buffer emacs-window))
            (window-id (buffer-local-value 'rau--window-id buffer)))
      window-id
    (let ((emacs-frame (window-frame emacs-window)))
      (frame-parameter emacs-frame 'rau--window-id))))

(defun rau--buffer-for-window-id (window-id)
  (cl-find window-id
           (buffer-list)
           :key (lambda (b) (buffer-local-value 'rau--window-id b))))

(defun rau--emacs-window-for-window-id (window-id)
  "Return Emacs window associated with WINDOW-ID."
  (when-let* ((buffer (rau--buffer-for-window-id window-id)))
    (get-buffer-window buffer 'visible)))

;;; Emacs integration, interaction

(defun rau--make-outputframe-parameters ()
  "Return alist of frame parameters with unique name as expected by title
event handler."
  `((name . ,(make-temp-name "rau-frame-"))
    (undecorated . t)
    (window-system . pgtk)
    ;; avoid showing the same rau buffer twice
    (buffer-predicate . rau--buffer-predicate)))


;; Major mode for rau-managed buffers
(defvar-local rau--window-id nil
  "Wayland window id for this `rau-mode' buffer.")

(define-derived-mode rau-mode special-mode "Rau"
  "Major mode for buffers representing windows managed by rau."
  :group 'rau
  (setq-local buffer-read-only t)
  (add-hook 'kill-buffer-query-functions #'rau--buffer-killed nil t)
  (scroll-bar-mode 0)
  (setq-local left-fringe-width 0
              right-fringe-width 0))

;;; task queue
(defun rau--tasks-execute ()
  "Execute tasks enqueued by event listeners."
  (let ((tasks (nreverse (rau--fe-state-task-queue rau--fe-state))))
    (setf (rau--fe-state-task-queue rau--fe-state) nil)
    (while-let ((task (pop tasks)))
      (let ((fn (car task))
            (args (cdr task)))
        (rau--condition-case
         (format "task %s" fn)
         (apply fn args))))))

(defun rau--tasks-schedule-execution ()
  "Schedule task processing if not already scheduled."
  (unless (rau--fe-state-task-timer rau--fe-state)
    (setf (rau--fe-state-task-timer rau--fe-state)
          (run-at-time
           0 nil
           (lambda ()
             (setf (rau--fe-state-task-timer rau--fe-state) nil)
             (rau--tasks-execute))))))

(defun rau--tasks-enqueue (fn &rest args)
  "Queue FN with ARGS for execution by rau--tasks-execute.
Also schedule the task execution timer if not yet done so.  Only to be
used in event listeners."
  (push `(,fn . ,args) (rau--fe-state-task-queue rau--fe-state))
  (rau--tasks-schedule-execution))

(defun rau--task-assign-emacs-frame (output-id)
  "Find initial frame or create one and set OUTPUT-ID in frame-parameter."
  (let ((emacs-frame
         (or
          ;; search the initial frame
          (cl-loop for f in (frame-list)
                   for name = (frame-parameter f 'name)
                   for is_rau_frame = (string-prefix-p "rau-frame-" name)
                   for is_assigned = (frame-parameter f 'rau--output-id)
                   if (and is_rau_frame (not is_assigned)) return f)
          (make-frame (rau--make-outputframe-parameters)))))
    (set-frame-parameter emacs-frame 'rau--output-id output-id))
  (rau--send-fe-ui-state))

(defun rau--task-consume-key-event (event needs-focus)
  "Forward EVENT to emacs and setup to recover focus if NEEDS-FOCUS."
  (push (cons t event) unread-command-events)
  (when needs-focus
    (add-hook 'post-command-hook #'rau--recover-focus-after-binding-pressed)))

(defun rau--task-delete-frame (frame-id)
  "Delete frame by FRAME-ID."
  (lgr-info rau--lgr "task delete frame with frame-id %S" frame-id)
  (when-let* ((emacs-frame (frame-by-id frame-id)))
    (delete-frame emacs-frame)))

(defun rau--task-kill-buffer (window-id)
  "Remove kill inhibiting hook and kill."
  (when-let* ((buffer (rau--buffer-for-window-id window-id)))
    (with-current-buffer buffer
      (remove-hook 'kill-buffer-query-functions #'rau--buffer-killed t))
    (kill-buffer buffer)))

(defun rau--task-link-emacs-frame-to-window (title window-id)
  (let ((emacs-frame
         (cl-find title (frame-list)
                  :test #'equal
                  :key (lambda (f) (frame-parameter f 'name)))))
    (unless emacs-frame
      (error "No emacs frame found for wayland window. id=%d title=%s." window-id title))

    (set-frame-parameter emacs-frame 'rau--window-id window-id)
    (rau--send-fe-ui-state)))

(defun rau--task-minimize-window (window-id)
  (when-let* ((buffer (rau--buffer-for-window-id window-id)))
    (bury-buffer buffer)))

(defun rau--task-rename-buffer (window-id name)
  "Rename buffer for external window to NAME."
  (when-let* ((buffer (rau--buffer-for-window-id window-id)))
    (with-current-buffer buffer
      (rename-buffer name t))))

(defun rau--task-select-window (window-id)
  "Let Emacs select the underlying emacs-window for the external WINDOW-ID."
  (when-let* ((emacs-window (rau--emacs-window-for-window-id window-id)))
    (select-window emacs-window 'norecord)))

(defun rau--task-setup-new-external-window (window-id)
  "Create and display Rau mode buffer for WINDOW-ID."
  (let ((buffer (get-buffer-create (make-temp-name "rau-external-"))))
    (with-current-buffer buffer
      (rau-mode)
      (setq-local rau--window-id window-id)
      (unless (display-buffer buffer)
        (error "display-buffer failed for window %d buffer %S." window-id buffer)))))

;;; Keybindings

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

(defun rau--parse-keys (keys)
  "Parse KEYS into a list of rau--binding structs. See `rau-bind-keys'."
  (let ((seen (make-hash-table :test #'equal))
        (result nil))
    (dolist (item keys)
      (let ((elements (if (stringp item) (list item) item))
            keys flags)
        (while-let ((elt (pop elements)))
          (cond
           ((stringp elt)
            (push elt keys))
           ((eq :layout elt)
            (unless elements
              (error ":layout requires a layout number"))
            (push (cons elt (pop elements)) flags))
           ((memq elt '(:needs-focus :allow-when-locked))
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
            (push `(:event ,(cl-first xkb)
                   :key ,key
                   :keysym ,(rau--resolve-keysym (cl-second xkb))
                   :modifiers ,(cl-third xkb)
                   :needs-focus   ,(cdr (assq :needs-focus flags))
                   :allow-when-locked ,(cdr (assq :allow-when-locked flags))
                   :layout        ,(cdr (assq :layout flags)))
                  result)))))
    (nreverse result)))

(defvar rau--xkb-keysym-alist
  '(("iso-lefttab" . #xFE20)
    ("return" . #xFF0D)
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
    ("xf86monbrightnessup"   . #x1008FF02)
    ("xf86monbrightnessdown" . #x1008FF03)
    ("audioraisevolume" . #x1008FF13)
    ("audiolowervolume" . #x1008FF11)
    ("audiomute"        . #x1008FF12)
    ("audioplay"        . #x1008FF14)
    ("audiostop"        . #x1008FF15)
    ("audiopause"       . #x1008FF31)
    ("audionext"        . #x1008FF17)
    ("audioprev"        . #x1008FF16)
    ("homepage"  . #x1008FF18)
    ("mail"      . #x1008FF19)
    ("search"    . #x1008FF1B)
    ("favorites" . #x1008FF30)
    ("launcha"   . #x1008FF52)
    ("explorer"  . #x1008FF5D)
    ("display"   . #x1008FF85)
    ("tools"     . #x1008FF89)
    ("wlan"      . #x1008FF95)
    ("audiomicmute" . #x1008FFB2)
    )
  "Small fallback table mapping key names to XKB keysyms. See
X11/XF86keysym.h.")

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
    (or (and (length= key 1)
             (rau--utf32-to-keysym (aref key 0)))
        (alist-get (downcase key)
                   rau--xkb-keysym-alist
                   nil nil #'equal)
        (user-error "Could not resolve %s to an XKB keysym" key)))
   (t (error "Key should be an integer, symbol or string"))))

(defun rau-bind-keys (keys)
  "Bind KEYS to always be sent to Emacs.  KEYS is a list either of strings
acceptable to `kbd' or of lists of strings and one or more of the
following keywords:

- :allow-when-locked - keys are also active when the session is locked,
   e.g. Volume or Brightness control
- :needs-focus - focus should be temporarily given to Emacs
- :layout LAYOUT-NR - 0 indexed layout override

Example:

(\"s-e\" \"s-f\"
  (\"s-d\" \"C-h\" :needs-focus)
  (:allow-when-locked \"M-x\" :needs-focus)
  (:allow-when-locked \"s-c\" \"s-z\" :layout 2))

This function should be run from the `rau-ready-hook'."
  ;; TODO: other check needed after split
  ;; (unless rau--state
  ;;   (error "Rau has not yet been started."))
  (let ((parsed-keys (rau--parse-keys keys)))
    (message "parsed keys: %S" parsed-keys)
    (rau--be #'rau--bind-parsed-keys parsed-keys)))

(defun rau--global-unset-key-if (key command)
  "Unset KEY in the global map if it is currently bound to COMMAND."
  (when-let* ((kbd-key (kbd key))
              ((eq (lookup-key global-map kbd-key) command)))
    (global-unset-key kbd-key)))

;;; Emacs handler functions for hooks

(defun rau--focus-change-allowed-p ()
  "Non-nil when no interactive command or edit is in progress."
  (and (not this-command)
       (length= unread-command-events 0)
       (length= (this-single-command-keys) 0)
       (zerop (minibuffer-depth))
       (zerop (recursion-depth))))

(defun rau--update-focus-request (&rest args)
  "Reconcile Wayland focus with the selected window."
  (lgr-debug rau--lgr "update-focus-request %S" args)
  (when-let* (((rau--focus-change-allowed-p))
              (emacs-window (selected-window))
              (target-id (rau--window-id-for-emacs-window emacs-window)))
    (rau--be #'rau--request-focus-by-id target-id)))

(defun rau--recover-focus-after-binding-pressed ()
  "Give focus back to external window after it was given to Emacs to handle
a keybinding pressed event. This function is meant to be bound to
post-command-hook in the enqueued command of the pressed event handler."
  (lgr-debug rau--lgr "recover focus. not t-c=%S u-c-e=%d t-s-c-k=%d m-d=%d r-d=%d"
             (not this-command)
             (length unread-command-events)
             (length (this-single-command-keys))
             (minibuffer-depth)
             (recursion-depth))
  (when (and (length= unread-command-events 0)
             (zerop (minibuffer-depth))
             (zerop (recursion-depth)))
    (lgr-debug rau--lgr "recover focus. removing post-command-hook.")
    (remove-hook 'post-command-hook #'rau--recover-focus-after-binding-pressed)
    (when-let* ((window-id (buffer-local-value 'rau--window-id (current-buffer))))
      (rau--be #'rau--request-focus-by-id window-id))))

(defun rau--buffer-killed ()
  "Request closing of the associated Wayland window when a rau buffer is killed."
  (when rau--window-id
    (rau--be #'rau--manage-enqueue rau--window-id 'close)
    nil))

(defun rau--window-configuration-change-handler ()
  "Schedule a river manage cycle and thus a reconciliation cycle.
This is necessary for external windows to resize when the minibuffer
expands.  This needs to be added to the global hook since local hooks
don't get called for windows that disappear.  Also
window-size-change-functions does not get called when minibuffer expands
and thus minibuffer ends up below external window."
  (rau--be #'rau--mark-manage-dirty))

;;; Other Emacs config functions: advices, predicates

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

(defun rau-toggle-fullscreen ()
  "Toggle fullscreen for the currently focused external window."
  (interactive)
  (if-let* ((window-id (buffer-local-value 'rau--window-id (current-buffer))))
      (rau--be #'rau--toggle-fullscreen-by-window-id window-id)
    (message "Fullscreen requested, but nothing is focused")))

;;; fe-state

(defun rau--fe-ui-state ()
  (let (outframes extwins)
    (dolist (f (frame-list))
      (let ((frame-id (frame-id f))
            (window-id (frame-parameter f 'rau--window-id))
            (output-id (frame-parameter f 'rau--output-id)))
        (when (or window-id output-id)
          (push `((frame-id . ,frame-id)
                  (window-id . ,window-id)
                  (output-id . ,output-id))
                outframes))
        (dolist (w (window-list f))
          (when-let* ((buffer (window-buffer w))
                      (window-id (buffer-local-value 'rau--window-id buffer)))
            (push `((window-id . ,window-id)
                    (frame-id . ,frame-id)
                    ;; (name . ,(buffer-name buffer)) ; only for debugging
                    (edges . ,(window-inside-absolute-pixel-edges w)))
                  extwins)))))
    `((outframes . ,(sort outframes :key #'cdar))
      (extwins . ,(sort extwins :key #'cdar)))))

(defun rau--send-fe-ui-state ()
  (let ((new-state (rau--fe-ui-state))
        (last-state (rau--fe-state-last-send-state rau--fe-state)))
    (if (equal new-state last-state)
        (lgr-trace rau--lgr "new-state and last-state equal, not sending.")
      (rau--be #'rau--update-fe-ui-state new-state)
      (setf (rau--fe-state-last-send-state rau--fe-state) new-state))))

(defun rau--window-state-change-handler ()
  (rau--send-fe-ui-state)
  (rau--update-focus-request)

  ;; Schedule a river manage cycle and thus a reconciliation cycle.
  ;; This is necessary for external windows to resize when the minibuffer
  ;; expands.  This needs to be added to the global hook since local hooks
  ;; don't get called for windows that disappear.  Also
  ;; window-size-change-functions does not get called when minibuffer expands
  ;; and thus minibuffer ends up below external window.
  (rau--be #'rau--mark-manage-dirty))

;;; Startup



;; NOTE: No need for rau-disable since this Emacs process is serving as a
;; Window Manager and disabling rau while keeping the Emacs process running
;; would result in an unresponsive user environment.
;;;###autoload
(defun rau-enable ()
  "Enable the rau window manager for river.
Call this function once when starting Emacs inside of river."
  (unless (eq window-system 'pgtk)
    (user-error "Rau requires a pgtk Emacs on Wayland"))

  (when rau--fe-state
    (user-error "Rau is already running"))
  (setq rau--fe-state (rau--fe-state-make)
        rau--lgr (lgr-get-logger "rau"))

  (unless confirm-kill-emacs
    (setq confirm-kill-emacs #'yes-or-no-p))
  (rau--global-unset-key-if "C-x C-c" 'save-buffers-kill-terminal)
  (rau--global-unset-key-if "C-z" 'suspend-frame)
  (rau--global-unset-key-if "C-x C-z" 'suspend-frame)

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

  (rau--rpc-start-backend)

  (add-hook 'window-state-change-hook #'rau--window-state-change-handler))

;;; RPC State
(defvar rau--rpc-proc nil "Network process connected to the backend.")
(defvar rau--rpc-backend-proc nil "The backend subprocess itself.")
(defvar rau--rpc-read-marker nil "Marker for tracking complete lines in the RPC buffer.")

(defun rau--rpc-start-backend ()
  "Start the rau backend subprocess and connect via Unix socket."
  ;; TODO create socket in users run dir
  (let* ((sock-file (make-temp-name "/tmp/rau-be-"))
         (proc (make-process
                :name "rau-backend"
                :command (list (expand-file-name invocation-name invocation-directory)
                               "-Q" "--fg-daemon=rau-be"
                               "-l" (locate-library "ewc")
                               "-l" (locate-library "rau-lib")
                               "-l" (locate-library "rau-be")
                               "--eval=(rau-be-main)"
                               sock-file)
                :buffer " *rau-backend*"
                :noquery t
                :stderr " *rau-backend-err*"))
         (_ (sleep-for 1)) ;; TODO react on something from the child instead
         (conn (make-network-process
                :name "rau-rpc"
                :buffer " *rau-rpc*"
                :family 'local
                :service nil
                :remote sock-file
                :filter #'rau--rpc-filter
                :noquery t)))
    (setq rau--rpc-proc conn
          rau--rpc-backend-proc proc)
    (add-hook 'kill-emacs-hook (lambda () (ignore-errors (delete-file sock-file))))
    proc))

(defun rau--rpc-send (sexp)
  "Send SEXP to the backend using Base64 encoding."
  (when (and rau--rpc-proc (process-live-p rau--rpc-proc))
    (with-temp-buffer
      (let (print-level print-length
            (print-escape-nonascii t)
            (print-circle t))
        (prin1 sexp (current-buffer))
        (encode-coding-region (point-min) (point-max) 'utf-8-emacs-unix)
        (base64-encode-region (point-min) (point-max) t)
        (goto-char (point-min)) (insert ?\")
        (goto-char (point-max)) (insert ?\" ?\n)
        (process-send-region rau--rpc-proc (point-min) (point-max))))))

(defun rau--be (fn &rest args)
  "Call FN with ARGS on the backend. Return values are ignored."
  (rau--rpc-send (cons fn args)))

(defun rau--rpc-filter (proc string)
  "Process filter for backend RPC. Handles chunked reads safely."
  (with-current-buffer (process-buffer proc)
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
              (rau--rpc-handle-message sexp))))))))

(defun rau--rpc-handle-message (msg)
  "Execute incoming messages (function calls) from the backend."
  (condition-case err
      (apply (car msg) (cdr msg))
    (error (message "Rau frontend error handling %S: %S" msg err))))

(provide 'rau)
;;; rau.el ends here
