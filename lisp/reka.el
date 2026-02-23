(require 'libreka)
(require 'cl-lib)
(require 'seq)

(setq debug-on-error t) ;; TODO

(defgroup reka nil
  "Reka - Emacs swimming in the river"
  :group 'environment
  :prefix "reka-")

(defcustom reka-handle nil
  "Opaque handle for interacting with the WM")

(defun reka--set-frame-name (frame)
  (unless (string-prefix-p "reka-frame-"
                           (frame-parameter frame 'name))
    (set-frame-parameter frame 'name (make-temp-name "reka-frame-"))))

(defun reka--ensure-frame-names ()
  "Ensure each frame has a unique title that reka can match to its xwindow."
  (cl-loop for frame being the frames
           do (reka--set-frame-name frame)))

(defun reka--select-focused-wayland-window ()
  "If an app window has Wayland focus, select its reka buffer's Emacs window."
  (let ((focused (reka-get-focused-window reka-handle)))
    (when focused
      (let ((buf (seq-find (lambda (buf)
                             (let ((w (buffer-local-value 'reka-window buf)))
                               (and w (reka-window-equal w focused))))
                           (reka--list-buffers))))
        (when buf
          (let ((win (get-buffer-window buf t)))
            (when win
              (select-window win 'norecord))))))))

(defun reka-handle-sigusr1 ()
  (interactive)
  (reka-reconcile-window-buffers reka-handle)
  (let ((params (reka--all-window-parameters)))
    (reka-update-window-parameters reka-handle params))
  (reka--select-focused-wayland-window))

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

(defun reka--window-parameters (window)
  (let ((edges (window-inside-absolute-pixel-edges window)))
    (reka-make-window-parameters
     (buffer-local-value 'reka-window (window-buffer window))
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
                      (cons (car elem)
                            (apply #'vector
                                   (seq-map #'reka--window-parameters (cdr elem)))))
                    windows-by-frame))))

(defun reka--create-buffer (window)
  (let ((buffer (get-buffer-create (make-temp-name "reka-window-"))))
    (with-current-buffer buffer
      (reka-mode)
      (setq-local reka-window window))
    (display-buffer buffer)))

(defun reka--signal-wm-hook (&rest _)
  (when reka-handle
    (reka-handle-sigusr1) ;; TODO: rename that?
    (reka-manage-dirty reka-handle)))

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

(defun reka-enable ()
  ;; TODO: this is a hack for lack of ability to figure out alignment ...
  (menu-bar-mode 0)
  (tool-bar-mode 0)
  (add-to-list 'default-frame-alist '(undecorated . t))

  (message "Launching native module ...")
  (reka--ensure-frame-names)
  (add-to-list 'after-make-frame-functions #'reka--set-frame-name)
  (setq reka-handle (reka-start-wm))

  ;; Layout signals
  (add-hook 'window-configuration-change-hook #'reka--signal-wm-hook)

  ;; Focus signals
  (add-hook 'window-selection-change-functions #'reka--update-focus-request)
  (add-hook 'window-buffer-change-functions    #'reka--update-focus-request)
  (add-hook 'minibuffer-setup-hook             #'reka--update-focus-request)
  (add-hook 'minibuffer-exit-hook              #'reka--update-focus-request))

(defun rtest ()
  (async-shell-command "pavucontrol"))

(provide 'reka)
