;;; init.el --- Minimal vanilla Emacs configuration -*- lexical-binding: t -*-

;; Redirect Customize to a separate file to keep init.el clean
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Initialize package management
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Theme
(unless (package-installed-p 'zenburn-theme)
  (package-refresh-contents)
  (package-install 'zenburn-theme))
(load-theme 'zenburn t)

;; Git gutter (diff-hl)
(unless (package-installed-p 'diff-hl)
  (package-install 'diff-hl))
(global-diff-hl-mode)

;; Undo tree
(unless (package-installed-p 'undo-tree)
  (package-install 'undo-tree))
(global-undo-tree-mode)

;; Sane defaults
(setq inhibit-startup-screen t)
(scroll-bar-mode -1)

;;; init.el ends here
