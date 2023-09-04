;;; thread-buffer-test.el --- unit test -*- lexical-binding: t -*-

;;; Code:

(require 'thread-buffer)
(require 'cl-lib)

(ert-deftest test-thread-buffer-write ()
  (cl-defstruct test-thread-buffer-write-case
    (title nil :type 'string)
    (input nil :type 'string)
    (template nil :type 'string)
    (regex nil :type 'string)
    (append nil :type 'boolean)
    (no-switch nil :type 'boolean)
    (want-switch-buffer nil :type 'string)
    (want-buffer-name nil :type 'string)
    (want-buffer-string nil :type 'string))

  (let* ((template-1 "template-1-%d")
         (regex-1 "template-1-[0-9]+")
         (template-2 "template-2-%d")
         (regex-2 "template-2-[0-9]+")
         (input1 "input1")
         (input2 "input2")
         (testcases
          (list (make-test-thread-buffer-write-case
                 :title "create t1 1st buf"
                 :input input1
                 :template template-1
                 :regex regex-1
                 :want-switch-buffer (format template-1 1)
                 :want-buffer-name (format template-1 1)
                 :want-buffer-string input1)
                (make-test-thread-buffer-write-case
                 :title "overwrite t1 1st buf"
                 :input input2
                 :template template-1
                 :regex regex-1
                 :want-switch-buffer (format template-1 1)
                 :want-buffer-name (format template-1 1)
                 :want-buffer-string input2)
                (make-test-thread-buffer-write-case
                 :title "append t1 1st buf"
                 :input input1
                 :template template-1
                 :regex regex-1
                 :append t
                 :want-switch-buffer (format template-1 1)
                 :want-buffer-name (format template-1 1)
                 :want-buffer-string (concat input2 input1))
                (make-test-thread-buffer-write-case
                 :title "create t1 2nd buf without regex"
                 :input input1
                 :template template-1
                 :want-switch-buffer (format template-1 2)
                 :want-buffer-name (format template-1 2)
                 :want-buffer-string input1)
                (make-test-thread-buffer-write-case
                 :title "create t1 3rd buf without regex"
                 :input input2
                 :template template-1
                 :no-switch t
                 :want-switch-buffer (format template-1 2)
                 :want-buffer-name (format template-1 3)
                 :want-buffer-string input2)
                (make-test-thread-buffer-write-case
                 :title "create t2 1st buf without regex"
                 :input input1
                 :template template-2
                 :append t
                 :want-switch-buffer (format template-2 1)
                 :want-buffer-name (format template-2 1)
                 :want-buffer-string input1)
                (make-test-thread-buffer-write-case
                 :title "append t2 1st buf"
                 :input input2
                 :template template-2
                 :regex regex-2
                 :append t
                 :want-switch-buffer (format template-2 1)
                 :want-buffer-name (format template-2 1)
                 :want-buffer-string (concat input1 input2))
                (make-test-thread-buffer-write-case
                 :title "append t2 1st buf 2 times"
                 :input input2
                 :template template-2
                 :regex regex-2
                 :append t
                 :want-switch-buffer (format template-2 1)
                 :want-buffer-name (format template-2 1)
                 :want-buffer-string (concat input1 input2 input2)))))
    (while testcases
      (let ((c (car testcases)))
        (message "Test: %s" (test-thread-buffer-write-case-title c))
        (thread-buffer-write
         (test-thread-buffer-write-case-input c)
         (test-thread-buffer-write-case-template c)
         (test-thread-buffer-write-case-regex c)
         (test-thread-buffer-write-case-append c)
         (test-thread-buffer-write-case-no-switch c))
        (should (equal (buffer-name)
                       (test-thread-buffer-write-case-want-switch-buffer c)))
        (with-current-buffer (test-thread-buffer-write-case-want-buffer-name c)
          (should (equal (buffer-substring (point-min) (point-max))
                         (test-thread-buffer-write-case-want-buffer-string c)))))
      (setq testcases (cdr testcases)))))

(provide 'thread-buffer-test)
;;; thread-buffer-test.el ends here
