;;; init.el --- Emacs configuration -*- lexical-binding: t -*-

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))

(my-configure-backup-locations)

(require 'use-package)
(setq use-package-always-ensure t)

(load (expand-file-name "lisp/init-functions.el" user-emacs-directory))

(use-package! zenburn-theme
  :config (load-theme 'zenburn t))

(use-package! diff-hl
  :config (global-diff-hl-mode))

(use-package! undo-tree
  :config (global-undo-tree-mode))

(winner-mode 1)

(with-demoted-errors "window-jump not available: %S"
  (require 'window-jump))

(use-package! vertico
  :config (vertico-mode 1))

(use-package! orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package! marginalia
  :config (marginalia-mode 1))

(use-package! projectile
  :config (projectile-mode 1))

(savehist-mode 1)

(use-package! telephone-line
  :config (telephone-line-mode 1))

(load (expand-file-name "lisp/keybindings.el" user-emacs-directory))

(setq inhibit-startup-screen t)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)

(my-suppress-package-compile-warnings)

;;; init.el ends here
