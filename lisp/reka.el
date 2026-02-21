(require 'libreka)
(require 'cl-lib)
(require 'seq)

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

(defun reka-enable ()
  (message "Launching native module ...")
  (reka--ensure-frame-names)
  (add-to-list 'after-make-frame-functions #'reka--set-frame-name)
  (setq reka-handle (reka-start-wm)))

(defun reka-handle-sigusr1 ()
  (interactive)
  (let ((cmd (reka-read-command reka-handle)))
    (message "Got command from WM: %s" cmd)
    (reka-send-command reka-handle "whatever")))

(define-key special-event-map [sigusr1] #'reka-handle-sigusr1)

;; Major mode for reka-managed buffers
(defvar-local reka-window-id nil
  "Window ID for this reka buffer.")

(define-derived-mode reka-mode special-mode "Reka"
  "Major mode for buffers representing windows managed by reka."
  :group 'reka
  (setq-local buffer-read-only t))

(defun reka--window-parameters (window)
  (let ((edges (window-inside-absolute-pixel-edges window)))
    (list :id (buffer-local-value 'reka-window-id (window-buffer window))
          :x (nth 0 edges)
          :y (nth 1 edges)
          :w (- (nth 2 edges) (nth 0 edges))
          :h (- (nth 3 edges) (nth 1 edges))
          :focused (eq window (selected-window)))))

(defun reka--all-window-parameters ()
  (let ((windows-by-frame
         (cl-loop for frame being the frames
                  collect (cons (frame-parameter frame 'name)
                                (seq-filter (lambda (window)
                                              (with-current-buffer (window-buffer window)
                                                (eq major-mode 'reka-mode)))
                                            (window-list frame))))))
    (seq-map (lambda (elem)
               (cons (car elem)
                     (seq-map #'reka--window-parameters (cdr elem))))
             windows-by-frame)))

(provide 'reka)
