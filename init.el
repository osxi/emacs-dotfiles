;;; init.el --- Emacs configuration -*- lexical-binding: t -*-

;; Redirect Customize to a separate file to keep init.el clean
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Initialize package management
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Add site-lisp directory for local packages
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))

;; use-package — declarative package management (bundled since Emacs 29)
(require 'use-package)
(setq use-package-always-ensure t)

(defmacro use-package! (&rest args)
  "Like `use-package', but safely catches install/require failures.
Failed packages are logged to *Messages* but never abort startup."
  `(with-demoted-errors "init.el package error: %S" (use-package ,@args)))

;; Theme
(use-package! zenburn-theme
  :config (load-theme 'zenburn t))

;; Git gutter (diff-hl)
(use-package! diff-hl
  :config (global-diff-hl-mode))

;; Undo tree
(use-package! undo-tree
  :config (global-undo-tree-mode))

;; Window management
(winner-mode 1) ; C-c C-<left>/<right> to undo/redo window-layout changes

;; chumpy-windows' window-jump.el — directional window navigation
;; Loaded from site-lisp if available (installed via 'git clone' or downloaded)
(with-demoted-errors "window-jump not available: %S"
  (require 'window-jump)
  (global-set-key (kbd "C-c b") #'window-jump-left)
  (global-set-key (kbd "C-c f") #'window-jump-right)
  (global-set-key (kbd "C-c p") #'window-jump-up)
  (global-set-key (kbd "C-c n") #'window-jump-down))

(global-set-key (kbd "C-c m") #'delete-other-windows) ; maximize current window
(global-set-key (kbd "C-c =") #'balance-windows)      ; equalize window sizes

;; Open init.el
(defun my-open-init-file ()
  "Open init.el."
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))
(global-set-key (kbd "C-c i") #'my-open-init-file)

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
(global-set-key [remap move-beginning-of-line] #'my-smarter-move-beginning-of-line)

;; Completion system (Vertico + Orderless + Marginalia)
(use-package! vertico
  :config (vertico-mode 1))

(use-package! orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package! marginalia
  :config (marginalia-mode 1))

;; Project file search
(use-package! projectile
  :config (projectile-mode 1)
  :bind (("C-c s" . projectile-find-file)))

(savehist-mode 1) ; persist minibuffer history across sessions

;; Mode line (Powerline-style)
(use-package! telephone-line
  :config (telephone-line-mode 1))

;; Sane defaults
(setq inhibit-startup-screen t)
(scroll-bar-mode -1)

;; Suppress non-error warnings on startup (compilation warnings from packages)
(setq warning-minimum-level :error)
(setq native-comp-async-report-warnings-errors 'silent)

;;; init.el ends here
