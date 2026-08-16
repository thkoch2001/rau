(defun reka-debug-test (test-name)
  "Run ERT test TEST-NAME and drop into the debugger on failure."
  (interactive "STest: ")
  (let ((print-level nil)
        (print-length nil)
        (print-circle t)
        (debug-on-error t))
    ;; Calling the body directly skips ERT's error-catching, so
    ;; debug-on-error gets the signal and opens a full *Backtrace*.
    (funcall (ert-test-body (ert-get-test test-name)))))

(provide 'rau-test-helpers)
