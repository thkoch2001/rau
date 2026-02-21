(require 'libreka)
(require 'cl-lib)

(defgroup reka nil
  "Reka - Emacs swimming in the river"
  :group 'environment
  :prefix "reka-")

(defcustom reka-handle nil
  "Opaque handle for interacting with the WM")

(defun reka--ensure-frame-names ()
  "Ensure each frame has a unique title that reka can match to its xwindow."
  (cl-loop for frame being the frames
           do (unless (string-prefix-p "reka-frame-"
                                       (frame-parameter frame 'name))
                (set-frame-parameter frame 'name (make-temp-name "reka-frame-")))))

(defun reka-enable ()
  (message "Launching native module ...")
  (reka--ensure-frame-names)
  (setq reka-handle (reka-start-wm)))

(defun reka-handle-sigusr1 ()
  (interactive)
  (let ((cmd (reka-read-command reka-handle)))
    (message "Got command from WM: %s" cmd)
    (reka-send-command reka-handle "whatever")))

(define-key special-event-map [sigusr1] #'reka-handle-sigusr1)

(provide 'reka)
