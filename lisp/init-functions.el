;;; init-functions.el --- Custom functions and package loading utilities -*- lexical-binding: t -*-

(defmacro use-package! (&rest args)
  "Like `use-package', but safely catches install/require failures.
Failed packages are logged to *Messages* but never abort startup."
  `(with-demoted-errors "init.el package error: %S" (use-package ,@args)))

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

(defun my-open-init-file ()
  "Open init.el."
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))

(defun my-configure-backup-locations ()
  "Centralize backup/autosave files outside working directories.
Prevents #lockfile# and backup clutter in projects. Stores backups
in ~/.config/emacs/var/{backup,autosave}/."
  (let ((backup-dir (expand-file-name "var/backup" user-emacs-directory))
        (autosave-dir (expand-file-name "var/autosave" user-emacs-directory)))
    (dolist (dir (list backup-dir autosave-dir))
      (make-directory dir t))
    (setq backup-directory-alist `(("." . ,backup-dir)))
    (setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))
    (setq backup-by-copying t)
    (setq backup-by-copying-when-linked t)
    (setq create-lockfiles nil)))

(defun my-suppress-package-compile-warnings ()
  "Suppress compilation warnings from packages on startup.
Only real errors are shown; this silences harmless noise from optional
functions referenced in package hooks."
  (setq warning-minimum-level :error)
  (setq native-comp-async-report-warnings-errors 'silent))

(provide 'init-functions)
;;; init-functions.el ends here
