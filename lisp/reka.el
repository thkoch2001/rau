(require 'libreka)

(defgroup reka nil
  "Reka - Emacs swimming in the river"
  :group 'environment
  :prefix "reka-")

(defcustom reka-handle nil
  "Opaque handle for interacting with the WM")

(defun reka-enable ()
  (message "Launching native module ...")
  (setq reka-handle (reka-start-wm)))

(defun reka-handle-sigusr1 ()
  (interactive)
  (let ((cmd (reka-read-command reka-handle)))
    (message "Got command from WM: %s" cmd)
    (reka-send-command reka-handle "whatever")))

(define-key special-event-map [sigusr1] #'reka-handle-sigusr1)

(defun reka-test ()
  (reka-send-command reka-handle "test"))

(provide 'reka)
