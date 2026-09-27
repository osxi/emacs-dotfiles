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

;; Centralize backup/autosave/swap files to avoid cluttering working directories
(let ((backup-dir (expand-file-name "var/backup" user-emacs-directory))
      (autosave-dir (expand-file-name "var/autosave" user-emacs-directory)))
  (dolist (dir (list backup-dir autosave-dir))
    (make-directory dir t))
  (setq backup-directory-alist `(("." . ,backup-dir)))
  (setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))
  (setq backup-by-copying t)
  (setq backup-by-copying-when-linked t)
  (setq create-lockfiles nil)) ; disable lock files (#filename#)

;; use-package — declarative package management (bundled since Emacs 29)
(require 'use-package)
(setq use-package-always-ensure t)

;; Load custom functions and macros
(load (expand-file-name "lisp/init-functions.el" user-emacs-directory))

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
  (require 'window-jump))

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
  :config (projectile-mode 1))

(savehist-mode 1) ; persist minibuffer history across sessions

;; Mode line (Powerline-style)
(use-package! telephone-line
  :config (telephone-line-mode 1))

;; Load keybindings
(load (expand-file-name "lisp/keybindings.el" user-emacs-directory))

;; Sane defaults
(setq inhibit-startup-screen t)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)

;; Suppress non-error warnings on startup (compilation warnings from packages)
(setq warning-minimum-level :error)
(setq native-comp-async-report-warnings-errors 'silent)

;;; init.el ends here
