;;; keybindings-test.el --- Tests for keybindings.el -*- lexical-binding: t -*-

(require 'ert)

;; Helper to look up a binding
(defun test-get-binding (keys)
  "Return the function bound to KEYS (a key sequence string)."
  (key-binding (kbd keys)))

;; Window navigation
(ert-deftest test-keybinding-window-jump-left ()
  "C-c b should be bound to window-jump-left."
  (should (eq (test-get-binding "C-c b") 'window-jump-left)))

(ert-deftest test-keybinding-window-jump-right ()
  "C-c f should be bound to window-jump-right."
  (should (eq (test-get-binding "C-c f") 'window-jump-right)))

(ert-deftest test-keybinding-window-jump-up ()
  "C-c p should be bound to window-jump-up."
  (should (eq (test-get-binding "C-c p") 'window-jump-up)))

(ert-deftest test-keybinding-window-jump-down ()
  "C-c n should be bound to window-jump-down."
  (should (eq (test-get-binding "C-c n") 'window-jump-down)))

(ert-deftest test-keybinding-maximize-window ()
  "C-c m should be bound to maximize-window."
  (should (eq (test-get-binding "C-c m") 'maximize-window)))

;; Navigation
(ert-deftest test-keybinding-open-init-file ()
  "C-c i should be bound to my-open-init-file."
  (should (eq (test-get-binding "C-c i") 'my-open-init-file)))

(ert-deftest test-keybinding-smart-c-a ()
  "[remap move-beginning-of-line] should be bound to my-smarter-move-beginning-of-line."
  (should (eq (lookup-key global-map [remap move-beginning-of-line]) 'my-smarter-move-beginning-of-line)))

;; Project search
(ert-deftest test-keybinding-projectile-find-file ()
  "C-c s should be bound to projectile-find-file."
  (should (eq (test-get-binding "C-c s") 'projectile-find-file)))

;; Arrow key window resizing
(ert-deftest test-keybinding-arrow-up ()
  "<up> should be bound to shrink-window."
  (should (eq (test-get-binding "<up>") 'shrink-window)))

(ert-deftest test-keybinding-arrow-down ()
  "<down> should be bound to enlarge-window."
  (should (eq (test-get-binding "<down>") 'enlarge-window)))

(ert-deftest test-keybinding-arrow-left ()
  "<left> should be bound to shrink-window-horizontally."
  (should (eq (test-get-binding "<left>") 'shrink-window-horizontally)))

(ert-deftest test-keybinding-arrow-right ()
  "<right> should be bound to enlarge-window-horizontally."
  (should (eq (test-get-binding "<right>") 'enlarge-window-horizontally)))

(provide 'keybindings-test)
;;; keybindings-test.el ends here
