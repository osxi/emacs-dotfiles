;;; keybindings.el --- Keybinding configuration -*- lexical-binding: t -*-

(global-set-key (kbd "C-c b") #'window-jump-left)
(global-set-key (kbd "C-c f") #'window-jump-right)
(global-set-key (kbd "C-c p") #'window-jump-up)
(global-set-key (kbd "C-c n") #'window-jump-down)
(global-set-key (kbd "C-c m") #'maximize-window)

(global-set-key (kbd "<up>") #'shrink-window)
(global-set-key (kbd "<down>") #'enlarge-window)
(global-set-key (kbd "<left>") #'shrink-window-horizontally)
(global-set-key (kbd "<right>") #'enlarge-window-horizontally)

(global-set-key (kbd "C-c i") #'my-open-init-file)
(global-set-key [remap move-beginning-of-line] #'my-smarter-move-beginning-of-line)

(global-set-key (kbd "C-c s") #'projectile-find-file)

(provide 'keybindings)
;;; keybindings.el ends here
