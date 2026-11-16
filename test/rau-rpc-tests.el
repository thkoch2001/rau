;;; rau-rpc-tests.el --- RPC error isolation tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'rau-lib)

(ert-deftest rau-rpc-error-does-not-replay-or-block-next-message ()
  (let ((rau--rpc-recv-buffer (generate-new-buffer " *rau-rpc-test*"))
        (debug-on-error nil)
        (attempts 0)
        (received 0))
    (unwind-protect
        (cl-letf (((symbol-function 'rau-test--rpc-broken-message)
                   (lambda () (cl-incf attempts) (error "RPC test failure")))
                  ((symbol-function 'rau-test--rpc-valid-message)
                   (lambda () (cl-incf received))))
          (rau--rpc-filter nil "(rau-test--rpc-broken-message)\n(rau-test--rpc-valid-message)\n")
          (should (= attempts 1))
          (should (= received 1))
          (should (zerop (buffer-size rau--rpc-recv-buffer)))
          (rau--rpc-filter nil "(rau-test--rpc-valid-message)\n")
          (should (= attempts 1))
          (should (= received 2))
          (should (zerop (buffer-size rau--rpc-recv-buffer))))
      (kill-buffer rau--rpc-recv-buffer))))

;;; rau-rpc-tests.el ends here
