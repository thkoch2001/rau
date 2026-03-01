;;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'libreka)
(require 'cl-lib)
(require 'seq)

(setq debug-on-error t) ;; TODO

(defgroup reka nil
  "Reka - Emacs swimming in the river"
  :group 'environment
  :prefix "reka-")

(defvar reka-handle nil
  "Opaque handle for interacting with the WM")

(defcustom reka-intercept-prefixes
  '("C-x" "C-u" "C-h" "M-x")
  "Prefix keys that should always go to Emacs.")

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
              (when-let ((w (buffer-local-value 'reka-window buf)))
                (reka-window-equal w window)))
            (reka--list-buffers)))

(defun reka-handle-sigusr1 ()
  (interactive)
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
       (when-let ((buf (reka--find-buffer-for-window window)))
           (kill-buffer buf)))

      (`(focused . ,window)
       (setq reka--last-focused-buffer nil)
       (when-let* ((buf (reka--find-buffer-for-window window))
                 (win (get-buffer-window buf t)))
           (select-window win 'norecord)))

      (`(title-change ,window ,title)
       (when-let* ((buf (reka--find-buffer-for-window window)))
         (with-current-buffer buf
           (rename-buffer title))))

      ('frame-request (make-frame))

      (_ (error "received unknown command from reka: %s" cmd)))))

(define-key special-event-map [sigusr1] #'reka-handle-sigusr1)

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

(defun reka--signal-wm-hook (&rest _)
  (when reka-handle
    (reka-handle-sigusr1)))

(defvar reka--last-focused-buffer nil
  "Last buffer for which a focus request was sent.")

(defun reka--update-focus-request (&rest _) ;; TODO: take & forward frame for active output logic
  "Send a focus request to the WM reflecting the current selected-window."
  (when reka-handle
    (let* ((buf (window-buffer (selected-window)))
           (is-reka (reka--is-reka-buffer buf))
           (key (if is-reka buf nil)))
      ;; avoid infinite loops of focus change <> focus update notification
      (unless (eq key reka--last-focused-buffer)
        (setq reka--last-focused-buffer key)
        (reka-set-focus-request reka-handle
                                (when is-reka
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

(defun reka-push-intercept-prefixes ()
  (dolist (prefix reka-intercept-prefixes)
    (let ((data (reka--key-to-xkb prefix)))
      (reka-register-xkb-prefix reka-handle (car data) (cadr data) (caddr data)))))

(defun reka-enable ()
  ;; TODO: this is a hack for lack of ability to figure out alignment ...
  (menu-bar-mode 0)
  (tool-bar-mode 0)
  (set-frame-parameter nil 'undecorated t)
  (add-to-list 'default-frame-alist '(undecorated . t))

  (message "Launching native module ...")
  (reka--ensure-frame-names)
  (add-to-list 'after-make-frame-functions #'reka--set-frame-name)
  (setq reka-handle (reka-start-wm))
  (reka-push-intercept-prefixes)

  ;; Layout signals
  (add-hook 'window-configuration-change-hook #'reka--signal-wm-hook)

  ;; Focus signals
  (add-hook 'window-selection-change-functions #'reka--update-focus-request)
  (add-hook 'window-buffer-change-functions    #'reka--update-focus-request)
  (add-hook 'minibuffer-setup-hook             #'reka--update-focus-request)
  (add-hook 'minibuffer-exit-hook              #'reka--update-focus-request)
  (add-hook 'post-command-hook                 #'reka--update-focus-request))

(defun rtest ()
  (async-shell-command "pavucontrol"))

(provide 'reka)
