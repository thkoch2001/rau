;; -*- lexical-binding: t; -*-

(use-package emacs
  :custom
  (async-shell-command-buffer 'new-buffer)
  (async-shell-command-display-buffer nil)
  (global-auto-revert-mode t)
  (scroll-bar-mode nil)
  (select-enable-clipboard t)
  (show-paren-mode t)
  (tab-bar-new-tab-choice "*scratch*")
  (tab-bar-show 1)
  (tab-bar-tab-hints 1)
  (tab-bar-select-tab-modifiers '(meta super))
  (tab-width 4)
  ;; see window-min-height
  (temp-buffer-max-height 10)
  (tool-bar-mode nil)
  ;; prevent frequent resizes of completions or other temp-buffer-resize-mode windows
  (window-min-height 7)

  (font-use-system-font t)
  (frame-resize-pixelwise t)
  (window-resize-pixelwise t)
  (mode-line-compact 'long)

;;;; Mouse-related options
  (context-menu-mode t)
  (save-interprogram-paste-before-kill t)
  (mouse-yank-at-point t)
  (pixel-scroll-mode t)
 ;; '(pixel-scroll-precision-mode t) ;; see bug#69972
  (mouse-drag-and-drop-region t)
  (mouse-drag-and-drop-region-cross-program t)
  (mouse-drag-mode-line-buffer t)
  (global-xref-mouse-mode t)

;;;; Frame- and window-related options
  (frame-inhibit-implied-resize t)
  (help-window-select t)
  (window-combination-resize t)
)

(setq inhibit-message t)
(setq multiple-terminals-merge-keyboards t)
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

(let ((appender
       (lgr-set-layout
        (lgr-appender-journald)
        (lgr-layout-format
         :format "%m"))))
  (let ((lgr (lgr-get-logger "ewc")))
    (lgr-add-appender lgr appender)
    (lgr-set-threshold lgr lgr-level-debug)
    )
  (let ((lgr (lgr-get-logger "rau")))
    (lgr-add-appender lgr appender)
    (lgr-set-threshold lgr lgr-level-trace)
    ))

(print "rau configure" #'external-debugging-output)

(defun rau-test-bind-keys ()
  (rau-bind-keys
   '("s-z"
     (:needs-focus "C-x" "C-u" "C-h" "M-x")
     )))

(add-to-list 'display-buffer-alist '((derived-mode . rau-mode)	display-buffer-full-frame))
(add-hook 'rau-ready-hook #'rau-test-bind-keys)
(setopt rau-backend-init-file
        (expand-file-name "rau-be-init.el" (file-name-directory load-file-name)))

(print "rau enable" #'external-debugging-output)

(keymap-global-set "s-z" #'rau-toggle-fullscreen)
(rau-enable)
(print "done with init" #'external-debugging-output)
(switch-to-buffer "*Messages*")

(toggle-debug-on-error)
