;;; ewc-tests.el --- Tests for the ewc objects and tags API -*- lexical-binding: t; -*-

(require 'ert)
(require 'ewc)

(defun ewc-test-client ()
  "Return an `ewc-client' carrying one fake protocol/interface."
  (let ((ifs (make-hash-table)))
    (puthash 'test-iface '(1 (test-event) (test-request 0 8)) ifs)
    (ewc-client-make :interfaces ifs)))

(defun ewc-test-add (client)
  "Add one new `test-iface' object to CLIENT."
  (ewc-object-add client 'test-iface))

(ert-deftest ewc-object-add-tags-with-interface ()
  (let* ((client (ewc-test-client))
         (obj (ewc-test-add client)))
    ;; Protocol is derived from the interface by `ewc-find-protocol'.
    (should (eq (ewc-object-interface obj) 'test-iface))
    (should (memq 'test-iface (ewc-object-tags obj)))
    (should (equal (ewc-objects client 'test-iface) (list obj)))
    (should (eq obj (ewc-object-get client (ewc-object-id obj))))))

(ert-deftest ewc-object-add-unknown-interface-errors ()
  (let ((client (ewc-test-client)))
    (should-error (ewc-object-add client 'no-such-iface))))

(ert-deftest ewc-object-tag-does-not-duplicate ()
  (let* ((client (ewc-test-client))
         (obj (ewc-test-add client)))
    (ewc-object-tag client obj 'frame)
    (ewc-object-tag client obj 'frame)
    (should (equal (ewc-object-tags obj) '(frame test-iface)))
    (should (equal (ewc-objects client 'frame) (list obj)))))

(ert-deftest ewc-object-tag-rejects-non-symbol-tags ()
  (let* ((client (ewc-test-client))
         (obj (ewc-test-add client)))
    (should-error (ewc-object-tag client obj "frame"))
    (should-error (ewc-object-tag client obj 42))))

(ert-deftest ewc-object-untag ()
  (let* ((client (ewc-test-client))
         (obj (ewc-test-add client)))
    (ewc-object-tag client obj 'frame)
    (ewc-object-untag client obj 'frame)
    (ewc-object-untag client obj 'frame)          ; idempotent
    (should (equal (ewc-object-tags obj) '(test-iface)))
    (should (equal (ewc-objects client 'frame) nil))
    (should (ewc-object-tagged-p obj 'test-iface))
    (should-not (ewc-object-tagged-p obj 'frame))))

(ert-deftest ewc-objects-returns-a-copy ()
  (let* ((client (ewc-test-client))
         (obj (ewc-test-add client)))
    (let ((copy (ewc-objects client 'test-iface)))
      (setcar copy nil))
    (should (equal (ewc-objects client 'test-iface) (list obj)))))

(ert-deftest ewc-object-remove ()
  (let* ((client (ewc-test-client))
         (a (ewc-test-add client))
         (b (ewc-test-add client)))
    (ewc-object-tag client a 'frame)
    (ewc-object-tag client b 'window)
    (ewc-object-remove client a)
    (ewc-object-remove client a)                  ; idempotent
    (should-not (ewc-object-get client (ewc-object-id a)))
    (should (eq (ewc-object-get client (ewc-object-id b)) b))
    (should (equal (ewc-objects client 'frame) nil))
    (should (equal (ewc-objects client 'test-iface) (list b)))))

(ert-deftest ewc-object-remove-is-safe-during-iteration ()
  (let ((client (ewc-test-client)))
    (dotimes (_ 3) (ewc-test-add client))
    (dolist (obj (ewc-objects client 'test-iface))
      (ewc-object-remove client obj))
    (should (equal (ewc-objects client 'test-iface) nil))
    (should (zerop (hash-table-count (ewc-client-table client))))))

(provide 'ewc-tests)
;;; ewc-tests.el ends here
