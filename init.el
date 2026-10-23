;;; init.el --- Emacs configuration -*- lexical-binding: t -*-

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Startup performance: during init, skip garbage collection (512 MB heap
;; is plenty) and disable odd file-name handlers (Tramp etc., unused here).
;; Both are restored once init finishes, in `after-init-hook'.
(defvar my--gc-cons-threshold gc-cons-threshold)
(defvar my--file-name-handler-alist file-name-handler-alist)
(setq gc-cons-threshold (* 512 1024 1024)
      file-name-handler-alist nil)
(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold (* 8 1024 1024)
                  file-name-handler-alist my--file-name-handler-alist)
            (garbage-collect)))
;; Note: the 8 MB post-init threshold (down from 512 MB, up from the
;; 800 KB default) trades a little memory for fewer GCs while editing.
;; Set it back to `my--gc-cons-threshold' if you prefer stock behavior.

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))

(load (expand-file-name "lisp/init-functions.el" user-emacs-directory))

(my-configure-backup-locations)

(require 'use-package)
(setq use-package-always-ensure t)

(use-package! zenburn-theme
  :config (load-theme 'zenburn t))

(use-package! diff-hl
  :defer 1
  :config (global-diff-hl-mode))

(use-package! undo-tree
  :defer 1
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
  :defer t
  :commands (projectile-find-file)
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
