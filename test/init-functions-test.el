;;; init-functions-test.el --- Tests for init-functions.el -*- lexical-binding: t -*-

(require 'ert)

(ert-deftest test-use-package!-error-handling ()
  "use-package! should catch and log errors, not propagate them."
  (should (condition-case err
            (progn (use-package! nonexistent-fake-package
                     :ensure nil
                     :no-require t
                     :config (error "Test error"))
                   t)
            (error nil))))

(ert-deftest test-use-package!-success ()
  "use-package! should execute config normally on success."
  (let ((config-ran nil))
    (use-package! emacs
      :ensure nil
      :config (setq config-ran t))
    (should config-ran)))

(ert-deftest test-smarter-move-beginning-of-line-toggle-on-indent ()
  "First call goes to indentation, second call goes to column 0."
  (with-temp-buffer
    (insert "    indented line")
    (goto-char (point-max))
    (my-smarter-move-beginning-of-line nil)
    (should (= (current-column) 4))
    (my-smarter-move-beginning-of-line nil)
    (should (= (current-column) 0))))

(ert-deftest test-smarter-move-beginning-of-line-no-indent ()
  "On a line with no leading whitespace, stays at column 0."
  (with-temp-buffer
    (insert "no indent")
    (goto-char (point-max))
    (my-smarter-move-beginning-of-line nil)
    (should (= (current-column) 0))))

(ert-deftest test-smarter-move-beginning-of-line-prefix-arg ()
  "Prefix arg moves down first, then applies logic."
  (with-temp-buffer
    (insert "line 1\n    line 2")
    (goto-char (point-min))
    (my-smarter-move-beginning-of-line 2)
    (should (= (current-column) 4))))

(ert-deftest test-open-init-file-path ()
  "my-open-init-file should target the correct init.el path."
  (let ((file-arg nil))
    (cl-letf (((symbol-function 'find-file)
               (lambda (file) (setq file-arg file))))
      (my-open-init-file)
      (should (string-match "init\\.el$" file-arg)))))

(provide 'init-functions-test)
;;; init-functions-test.el ends here
