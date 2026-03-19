;;; -*- lexical-binding: t -*-
;;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'libreka)
(require 'cl-lib)
(require 'seq)

(defgroup reka nil
  "Reka - Emacs swimming in the river"
  :group 'environment
  :prefix "reka-")

(defvar reka-handle nil
  "Opaque handle for interacting with the WM")

(defcustom reka-enable-hook nil
  "Hook run at the end of `reka-enable'."
  :type 'hook)

(defun reka--set-frame-name (frame)
  (unless (string-prefix-p "reka-frame-"
                           (frame-parameter frame 'name))
    (set-frame-parameter frame 'name (make-temp-name "reka-frame-"))))

(defun reka--ensure-frame-names ()
  "Ensure each frame has a unique title that reka can match to its window."
  (cl-loop for frame being the frames
           do (reka--set-frame-name frame)))

(defun reka--find-buffer-for-window (window)
  (seq-find (lambda (buf)
              (when-let* ((w (buffer-local-value 'reka-window buf)))
                (reka-window-equal w window)))
            (reka--list-buffers)))

(defvar reka-processing-commands (make-mutex "reka-processor-lock")
  ;; Honestly, I'm not sure if this lock is ever contended. In theory there
  ;; should only ever be one linear thread doing stuff in Emacs. However, the
  ;; signal handler and its effects are a bit odd, so I'm opting for a defensive
  ;; approach here.
  "Internal lock to avoid double-processing of commands in signal handler.")

(defvar-local reka-app-id nil)

(defun reka--make-buffer-name (app-id title)
  (let ((title-trunc (if (> (length title) 40)
                         (format "%s…" (substring title 0 40))
                       title)))
    (if app-id (concat title-trunc " - " app-id)
      title-trunc)))

(defun reka--handle-commands ()
  (interactive)

  (with-mutex reka-processing-commands
    (setq reka--last-focused nil)
    (let ((params (reka--all-window-parameters)))
      (reka-update-window-parameters reka-handle params))

    (while-let ((cmd (reka-get-next-command reka-handle)))
      (pcase cmd
        (`(key-event . ,key)
         (push (cons t key) unread-command-events))

        (`(new-window . ,window)
         (reka--create-buffer window)
         (reka-notify-buffer-created reka-handle window))

        (`(window-closed . ,window)
         (when-let* ((buf (reka--find-buffer-for-window window)))
           (kill-buffer buf)))

        (`(minimize-requested . ,window)
         (when-let* ((buf (reka--find-buffer-for-window window)))
           (with-current-buffer buf
             (bury-buffer))))

        (`(focused . ,window)
         (when-let* ((buf (reka--find-buffer-for-window window))
                     (win (get-buffer-window buf t)))
           (select-window win 'norecord)))

        (`(app-id-change ,window ,app-id)
         (when-let* ((buf (reka--find-buffer-for-window window)))
           (with-current-buffer buf
             (setq reka-app-id app-id))))

        (`(title-change ,window ,title)
         (when-let* ((buf (reka--find-buffer-for-window window)))
           (with-current-buffer buf
             (rename-buffer (reka--make-buffer-name reka-app-id title) t))))

        ('frame-request (make-frame))

        (`(discard-frame . ,frame-name)
         (when-let* ((frame-names (make-frame-names-alist))
                     (frame (alist-get frame-name frame-names nil nil #'equal)))
           (delete-frame frame)))

        (`(message . ,msg)
         (message "reka: %s" msg))

        (_ (error "received unknown command from reka: %s" cmd))))

    (run-at-time nil nil #'reka--update-focus-request)))

(defun reka--handle-event (&rest _) ;; unused process filter args
  (when reka-handle
    (run-at-time 0 nil #'reka--handle-commands)))

;; Major mode for reka-managed buffers
(defvar-local reka-window nil
  "Window object for this reka-mode buffer.")

(defun reka--buffer-killed ()
  (when reka-window
    (reka-close-window reka-handle reka-window)))

(define-derived-mode reka-mode special-mode "Reka"
  "Major mode for buffers representing windows managed by reka."
  :group 'reka
  (setq-local buffer-read-only t)
  (add-hook 'kill-buffer-hook #'reka--buffer-killed nil t)
  (scroll-bar-mode 0)
  (setq-local left-fringe-width 0
              right-fringe-width 0))

(defun reka--is-reka-buffer (buf)
  (with-current-buffer buf
    (eq major-mode 'reka-mode)))

(defun reka--get-window (buffer)
  (buffer-local-value 'reka-window buffer))

(defun reka--list-buffers ()
  (apply #'vector (seq-filter #'reka--is-reka-buffer (buffer-list))))

(defun reka--window-parameters (frame window)
  (let ((edges (window-inside-absolute-pixel-edges window)))
    (reka-make-window-parameters
     (buffer-local-value 'reka-window (window-buffer window))
     frame
     (nth 0 edges)
     (nth 1 edges)
     (- (nth 2 edges) (nth 0 edges))
     (- (nth 3 edges) (nth 1 edges)))))

(defun reka--all-window-parameters ()
  (let ((windows-by-frame
         (cl-loop for frame being the frames
                  collect (cons (frame-parameter frame 'name)
                                (seq-filter (lambda (window)
                                              (reka--is-reka-buffer (window-buffer window)))
                                            (window-list frame))))))
    (apply #'vector
           (seq-map (lambda (elem)
                      (let ((frame-name (car elem))
                            (windows (cdr elem)))
                        (apply #'vector
                               (seq-map (lambda (w) (reka--window-parameters frame-name w))
                                        windows))))
                    windows-by-frame))))

(defun reka--create-buffer (window)
  (let ((buffer (get-buffer-create (make-temp-name "reka-window-"))))
    (with-current-buffer buffer
      (reka-mode)
      (setq-local reka-window window))
    (display-buffer buffer)))

(defvar reka--last-focused nil)

(defun reka--update-focus-request (&rest _) ;; TODO: take & forward frame for active output logic
  "Send a focus request to the WM reflecting the current selected-window."
  (when reka-handle
    (let* ((win (selected-window))
           (buf (window-buffer win))
           (can-focus-window (and (reka--is-reka-buffer buf)
                                  (= 0 (length unread-command-events))
                                  (= 0 (length (this-single-command-keys)))
                                  (= 0 (minibuffer-depth))
                                  (= 0 (recursion-depth)))))
      (unless (equal win reka--last-focused)
        (setq reka--last-focused buf)
        (reka-set-focus-request reka-handle
                                (when can-focus-window
                                  (buffer-local-value 'reka-window buf)))))))

(defconst reka--modifier-bits
  ;; TODO: can we handle this with XKB on the rust side, too?
  '((shift   . 1)
    (control . 4)
    (meta    . 8)
    (super   . 64)
    (hyper   . 128))
  "Modifier bits as per river_seat_v1.modifiers / XKB")

(defun reka--key-to-xkb (key-string)
  "Decomposes an Emacs key-string (e.g. `C-x') into (event key modifiers).
The Rust side resolves the keysyms using xkbcommon."
  (let* ((event (aref (kbd key-string) 0))
         (basic (event-basic-type event))
         (mods (seq-map (lambda (mod) (alist-get mod reka--modifier-bits))
                        (event-modifiers event)))
         (key (if (characterp basic) basic (symbol-name basic))))
    (list event key (apply #'logior mods))))

(defun reka-push-intercept-prefix (prefix &optional command)
  (let ((data (reka--key-to-xkb prefix)))
    (reka-register-xkb-prefix reka-handle (if (integerp (car data))
                                              (car data)
                                            (symbol-name (car data)))
                              (cadr data) (caddr data) command)))

(defun reka-push-intercept-prefixes ()
  (dolist (prefix reka-intercept-prefixes)
    (reka-push-intercept-prefix prefix)))

(defcustom reka-intercept-prefixes
  '("C-x" "C-u" "C-h" "M-x")
  "Prefix keys that should always go to Emacs."
  :type '(repeat key)
  :set (lambda (sym val)
         (set-default sym val)
         (when reka-handle (reka-push-intercept-prefixes))))

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

(defun reka-enable ()
  ;; TODO: this is a hack for lack of ability to figure out alignment ...
  (menu-bar-mode 0)
  (tool-bar-mode 0)

  ;; configure this and all future frames ..
  (let ((frame-params '((undecorated . t)
                        (buffer-predicate . reka--buffer-predicate))))
    (modify-all-frames-parameters frame-params))

  (advice-add 'split-window-below :filter-return #'reka--split-window-advice)
  (advice-add 'split-window-right :filter-return #'reka--split-window-advice)

  (message "Launching native module ...")
  (reka--ensure-frame-names)
  (add-to-list 'after-make-frame-functions #'reka--set-frame-name)
  (let ((pipe (make-pipe-process :name "reka-events"
                                 :buffer " *reka-events*"
                                 :filter #'reka--handle-event)))
    (setq reka-handle (reka-start-wm pipe)))
  (reka-push-intercept-prefixes)

  ;; Layout signals
  (add-hook 'window-configuration-change-hook #'reka--handle-event)

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
