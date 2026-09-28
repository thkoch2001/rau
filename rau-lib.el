;; -*- lexical-binding: t; -*-

(require 'lgr)

(defvar rau--propagate-errors nil
  "See rau--condition-case.")

(defmacro rau--condition-case (location &rest body)
  "Wrap BODY in a condition-case unless `rau--propagate-errors' is non-nil.
LOCATION identifies where the error occurred."
  `(if rau--propagate-errors
       (progn ,@body)
     (condition-case err
         (progn ,@body)
       (error (message "Error at %s: %S" ,location err)))))

(defvar rau--lgr (lgr-get-logger "rau"))

;;; Remote Procedure Calls between front-end and back-end

(defvar rau--rpc-proc nil
  "Network process.")

(defvar rau--rpc-recv-buffer nil)

(defconst rau--rpc-prin1-overrides
  '(t
    (escape-newlines . t)
    (escape-control-characters . t)
    (float-format . nil)
    (integers-as-characters . nil)))

(defun rau--rpc-create-recv-buffer ()
  (setq rau--rpc-recv-buffer (get-buffer-create " *rau--rpc-recv-buffer*")))

(defun rau--rpc-send-sexp (process sexp)
  (let ((coding-system-for-write 'utf-8-emacs-unix)
        (msg (prin1-to-string sexp nil rau--rpc-prin1-overrides)))
    (process-send-string process (format "%s\n" msg))))

(defun rau--rpc-send (fn &rest args)
  "Call FN with ARGS on the backend. Return values are ignored."
  ;; (rau--rpc-send (cons fn args))
  (let ((sexp `(,fn . ,args)))
    (rau--rpc-send-sexp rau--rpc-proc sexp)))

(defalias 'rau--be #'rau--rpc-send)
(defalias 'rau--fe #'rau--rpc-send)

(defun rau--rpc-handle (fn args)
  "Execute the requested function. Return values are ignored."
  (condition-case err
      (apply fn args)
    (lgr-error rau--lgr "RPC error %S with %S: %S" fn args err)))

(defun rau--rpc-filter (proc string)
  "Process filter for incoming RPCs."
  (let ((coding-system-for-read 'utf-8-emacs-unix))
    (with-current-buffer rau--rpc-recv-buffer
      (goto-char (point-max))
      (save-excursion (insert string))
      (let ((end-pos (point-min-marker)))
        (while (search-forward "\n" nil t)
          (let ((line (buffer-substring-no-properties end-pos (1- (point)))))
            (setq end-pos (point))
            (lgr-trace rau--lgr "be recv: %s" line)
            (let* ((r (read-from-string line))
                   (sexp (car r))
                   (fn (car sexp))
                   (args (cdr sexp)))
              (rau--rpc-handle fn args))))
        (when (> end-pos (point-min))
          (delete-region (point-min) end-pos))))))

(provide 'rau-lib)
