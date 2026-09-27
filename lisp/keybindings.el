;;; keybindings.el --- Keybinding configuration -*- lexical-binding: t -*-

;; Window management
(global-set-key (kbd "C-c b") #'window-jump-left)
(global-set-key (kbd "C-c f") #'window-jump-right)
(global-set-key (kbd "C-c p") #'window-jump-up)
(global-set-key (kbd "C-c n") #'window-jump-down)
(global-set-key (kbd "C-c m") #'maximize-window)

;; Arrow keys resize the current window in the direction of the arrow
(global-set-key (kbd "<up>") #'shrink-window)
(global-set-key (kbd "<down>") #'enlarge-window)
(global-set-key (kbd "<left>") #'shrink-window-horizontally)
(global-set-key (kbd "<right>") #'enlarge-window-horizontally)

;; Navigation
(global-set-key (kbd "C-c i") #'my-open-init-file)
(global-set-key [remap move-beginning-of-line] #'my-smarter-move-beginning-of-line)

;; Project search
(global-set-key (kbd "C-c s") #'projectile-find-file)

(provide 'keybindings)
;;; keybindings.el ends here
