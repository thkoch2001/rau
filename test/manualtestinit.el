(setq inhibit-message t)
(print "running init" #'external-debugging-output)

(setq message-log-max t)

(defun my-dump-buffer-to-fd (buffer-name fd-path)
  "Write the contents of BUFFER-NAME to FD-PATH (e.g., /dev/stdout).
Suppresses the 'Wrote file' message to keep the output clean."
  (when (get-buffer buffer-name)
    (with-current-buffer buffer-name
      ;; inhibit-message prevents write-region from printing "Wrote /dev/stdout"
      (let ((inhibit-message t))
        (write-region (point-min) (point-max) fd-path nil)))))

(defun my-dump-logs-on-exit ()
  "Dump *Messages* to stdout and *Warnings* to stderr when Emacs exits.
Only runs in GUI mode to prevent duplicate output in terminal/batch modes."
  ;; Guard: Only do this in GUI mode.
  ;; In terminal (-nw) or batch mode, messages already go to stdout natively.
  (when (display-graphic-p)
    ;; 1. Dump *Messages* to stdout
    (my-dump-buffer-to-fd "*Messages*" "/dev/stdout")

    ;; 2. Dump *Warnings* to stderr if the buffer exists
    (when (get-buffer "*Warnings*")
      (my-dump-buffer-to-fd "*Warnings*" "/dev/stderr"))))

;; Attach the function to the exit hook
(add-hook 'kill-emacs-hook #'my-dump-logs-on-exit)

(setq server-name "test")
(server-start)

(print "require rau" #'external-debugging-output)
(require 'rau)

(print "rau enable" #'external-debugging-output)
(setq rau-debug t)
;; (setq ewc-debug t)
(customize-set-variable 'rau-intercept-prefixes '("s-z"))
(keymap-global-set "s-z" #'rau-toggle-fullscreen)
(rau-enable)
(print "done with init" #'external-debugging-output)
(switch-to-buffer "*Messages*")

(easy-menu-define words-menu global-map
       "Menu for word navigation commands."
       '("Words"
          ["Forward word" forward-word]
          ["Backward word" backward-word]))

;; TODO: find a way to disable the below functions when rau is running since
;; they freeze Emacs. Advincing does not seem to work. Maybe redefining?
;(x-popup-menu t words-menu)
(define-advice display-popup-menus-p (:override (&rest _))
  "Always return nil to force text-based fallbacks instead of GUI popups."
  nil)
(define-advice x-popup-menu (:override (&rest _))
  "Always return nil to force text-based fallbacks instead of GUI popups."
  nil)
(define-advice x-popup-dialog (:override (&rest _))
  "Always return nil to force text-based fallbacks instead of GUI popups."
  nil)
(toggle-debug-on-error)

