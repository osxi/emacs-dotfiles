;;; init.el --- Minimal vanilla Emacs configuration -*- lexical-binding: t -*-

;; Redirect Customize to a separate file to keep init.el clean
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Initialize package management
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Sane defaults
(setq inhibit-startup-screen t)

;;; init.el ends here
