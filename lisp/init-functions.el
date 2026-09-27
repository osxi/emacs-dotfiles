;;; init-functions.el --- Custom functions and package loading utilities -*- lexical-binding: t -*-

;; use-package! macro: wraps use-package with error handling
(defmacro use-package! (&rest args)
  "Like `use-package', but safely catches install/require failures.
Failed packages are logged to *Messages* but never abort startup."
  `(with-demoted-errors "init.el package error: %S" (use-package ,@args)))

;; Smart C-a: toggle between indentation and true line start
(defun my-smarter-move-beginning-of-line (arg)
  "Move to indentation; if already there, move to true beginning.
Repeated calls toggle between the two positions."
  (interactive "^p")
  (setq arg (or arg 1))
  (when (/= arg 1)
    (let ((line-move-visual nil)) (forward-line (1- arg))))
  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; Open init.el
(defun my-open-init-file ()
  "Open init.el."
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))

(provide 'init-functions)
;;; init-functions.el ends here
