;;; init --- Emacs configuration

;;; Commentary:
;;;   by Zachary Ngo; Inspired by Prelude.

;;; Code:
;; (setq system-uses-terminfo)
;; start package dependencies
;; (set package-list '(2048-game 4clojure request ac-html auto-complete popup ack ack-menu mag-menu splitter ada-mode wisi afternoon-theme ample-zen-theme anti-zenburn-theme atom-dark-theme auto-complete popup bliss-theme boron-theme calmer-forest-theme colonoscopy-theme dark-krystal-theme darkburn-theme php-mode espresso-theme faces+ flappymacs flatland-black-theme flycheck let-alist pkg-info epl dash frame-fns git-gutter gntp gruber-darker-theme guide-key s popwin dash haml-mode handlebars-mode hexrgb highlight-indentation ido-ubiquitous ido-vertical-mode ir-black-theme let-alist mag-menu splitter magit git-rebase-mode git-commit-mode markdown-mode minimal-theme molokai-theme mu4e-maildirs-extension multi-term multiple-cursors naquadah-theme obsidian-theme org-trello dash s deferred request-deferred request deferred paredit pbcopy php-mode popup popwin projectile-rails rake dash f dash s f dash s inf-ruby inflections projectile pkg-info epl dash rake dash f dash s request-deferred request deferred restclient s screenshot scss-mode smart-mode-line rich-minority dash smex soft-charcoal-theme sokoban spacegray-theme splitter switch-window transpose-frame warm-night-theme web-mode wisi zenburn-theme))
; activate all the packages (in particular autoloads)
;; (package-initialize)
; fetch the list of packages available
;; (unless package-archive-contents
;; (package-refresh-contents))
; install the missing packages
;; (dolist (package package-list)
;;   (unless (package-installed-p package)
;;     (package-install package)))
;; end package dependencies
(setq c-basic-offset 2)
(setq create-lockfiles nil)
(setq scroll-conservatively 101)
(let ((default-directory "~/.emacs.d/lisp/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))
(require 'powerline)
(require 'undo-tree)
(require 'window-jump)
(require 'splitter)
(require 'mu4e)
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t))
(setq undo-tree-auto-save-history t)
(setq restclient-same-window-response t)
(menu-bar-mode -1)
; (scroll-bar-mode -1)
(ido-mode t)
(ido-ubiquitous-mode t)
(electric-indent-mode t)
(global-undo-tree-mode t)
(global-linum-mode t)
(global-git-gutter-mode t)
(electric-pair-mode t)
(electric-indent-mode t)
(global-auto-revert-mode t)
(setq dired-auto-revert-buffer t)
(turn-on-pbcopy)
(require 'mouse)
(xterm-mouse-mode t)
(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; (add-hook 'imenu-after-jump-hook)
(setq linum-disable-hooks
      '("magit" "elfeed" "rcirc" "eshell" "eww"))
(defun fun-for-hooks (hooks)
  "Execute a function on list of hooks."
  (interactive)
  (cl-maplist (lambda (hook-name)
                (add-hook
                 (make-symbol (concat hook-name "-mode-hook"))
                 (lambda ()
                   (linum-mode -1)
                   (message (concat "executed hook for " hook-name)))))
              linum-disable-hooks))
(add-hook 'magit-mode-hook
	  (lambda ()
	    (linum-mode -1)
	    (message ":D")))
(add-hook 'elfeed-mode-hook
          (lambda ()
            (linum-mode -1)
            (message ":D")))
(add-hook 'rcirc-mode-hook
	  (lambda ()
	    (linum-mode -1)
	    (message ":D")))
(add-hook 'eshell-mode-hook
	  (lambda ()
	    (linum-mode -1)
	    (message ":D")))
(add-hook 'eww-mode-hook
	  (lambda ()
	    (linum-mode -1)
	    (message ":D")))
(add-hook 'mu4e-index-updated-hook
	  (lambda ()
	    (message "You have mail")))
(add-hook 'handlebars-mode-hook
	  (lambda ()
	    (local-unset-key (kbd "C-c s"))))
(add-hook 'go-mode-hook
          (lambda ()
            (add-hook 'before-save-hook #'gofmt-before-save)))
(global-set-key
 (kbd "C-c i")
 (lambda ()
   (interactive)
   (find-file "~/.emacs.d/init.el")))
(global-set-key
 (kbd "C-c C-r")
 (lambda ()
   (interactive)
   (restclient-http-send-current-stay-in-window)))
(global-set-key
 (kbd "C-c s")
 (lambda ()
   (interactive)
   (projectile-find-file)))
(global-set-key
 (kbd "C-c b")
 (lambda ()
   (interactive)
   (window-jump-left)))
(global-set-key
 (kbd "C-c f")
 (lambda ()
   (interactive)
   (window-jump-right)))
(global-set-key
 (kbd "C-c p")
 (lambda ()
   (interactive)
   (window-jump-up)))
(global-set-key
 (kbd "C-c n")
 (lambda ()
   (interactive)
   (window-jump-down)))
(global-set-key
 (kbd "C-x C-b")
 (lambda ()
   (interactive)
   (buffer-menu)))
(global-set-key
 (kbd "C-a")
 (lambda ()
   (interactive)
   (move-beginning-of-line-smart)))
(defun up-slightly ()
  (interactive)
  "Scroll up a little bit."
  (scroll-up 5))
(defun down-slightly ()
  (interactive)
  "Scroll down a little bit."
  (scroll-down 5))
(global-set-key (kbd "<mouse-4>") 'down-slightly)
(global-set-key (kbd "<mouse-5>") 'up-slightly)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-q") 'ido-delete-backward-word-updir)
;; (global-set-key
;;  (kbd "M-x")
;;  (lambda ()
;;    (interactive)
;;    (call-interactively
;;     (intern
;;      (ido-completing-read
;;       "M-x "
;;       (all-completions "" obarray 'commandp))))))
(defun move-beginning-of-line-smart ()
  "Toggle point between beginning of code and beginning of line."
  (interactive)
  (let ((initial-point (point)))
    (beginning-of-line-text)
    (when (eq initial-point (point))
      (beginning-of-line))))
(defun eshell/clear ()
  "Clear eshell."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))
(defalias 'eshell/c 'eshell/clear)
(defun abbreviated-pwd ()
  "Return shortened PWD, i.e /h/z/code instead of /home/zach/code."
  (interactive)
  (append (mapcar (lambda (x) (downcase (substring x 0 1)))
		  (butlast (last (cdr (split-string (eshell/pwd) "/")) 3)))
	  (pwd-last)))
(defun pwd-last ()
  "Return the last segment of the PWD."
  (interactive)
  (last (split-string (eshell/pwd) "/")))
(defun join-list (list delim)
  "Join LIST into a string delimited by DELIM."
  (interactive)
  (mapconcat 'identity list delim))
(defun zach-eshell-prompt ()
  "Zach's eshell prompt."
  (interactive)
  (join-list (abbreviated-pwd) "/"))
(defun recenter-paragraph ()
  "(re-)center some text in a buffer."
  (interactive)
  (save-excursion
    (mark-paragraph)
    (let* ((start (region-beginning))
           (end (region-end))
           (count (count-lines start end))
           (min (/ (window-size) 2)))
      ;; pad buffer with empty lines if necessary
      (when (< (line-number-at-pos start) min)
        (goto-char start)
        (newline min))
      ;; go to paragraph midpoint and recenter
      (goto-char end)
      (previous-line (/ count 2))
      (recenter))))
;; guide-key
(require 'guide-key)
;; rcirc
 ;; (add-to-list 'rcirc-server-alist
 ;; 	     '("poetic.irc.slack.com"
 ;; 	       :user-name "zach"
 ;; 	       :pass "<password>"
 ;; 	       :encryption tls))
(setq rcirc-omit-responses '("JOIN" "PART" "QUIT" "NICK" "MODE"))
(setq rcirc-prompt "> ")
;; erc
(setq erc-server-reconnect-timeout 20)
(setq erc-input-line-position -1)
(setq erc-hide-list '("MODE"))
(require 'erc-networks)
;; (add-to-list 'erc-server-alist '("Slack" IRC-SLACK "poetic.irc.slack.com" 6667))
(add-to-list 'erc-networks-alist '(IRC-SLACK "poetic.irc.slack.com"))
(add-hook 'erc-after-connect
    	  '(lambda (SERVER NICK)
    	     (cond
    	      ((string-match "freenode\\.net" SERVER)
    	       (erc-message "PRIVMSG" "NickServ identify <password>")))))
(require 'erc-join)
(erc-autojoin-mode 1)
(setq erc-autojoin-channels-alist
      '(("freenode.net" "#emberjs" "#emacs" "#git" "#meteor" "#laravel")))
(require 'erc-match)
(setq erc-keywords '("osxi" "zach"))
(erc-match-mode)
(require 'erc-track)
(erc-track-mode t)
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
				"324" "329" "332" "333" "353" "477"))
(add-hook 'erc-mode-hook
          '(lambda ()
             (require 'erc-pcomplete)
             (pcomplete-erc-setup)
             (erc-completion-mode 1)))
(require 'erc-fill)
(erc-fill-mode t)
(require 'erc-ring)
(erc-ring-mode t)
(require 'erc-netsplit)
(erc-netsplit-mode t)
(erc-timestamp-mode t)
(setq erc-timestamp-format "[%R-%m/%d]")
(erc-button-mode nil) ;slow
(setq erc-max-buffer-size 20000)
(defvar erc-insert-post-hook)
(add-hook 'erc-insert-post-hook 'erc-truncate-buffer)
(setq erc-truncate-buffer-on-save t)
(defun irc-maybe ()
  "Connect to IRC."
  (interactive)
  (when (y-or-n-p "IRC? ")
    (erc :server "irc.freenode.net" :port 6667
	 :nick "osxi" :full-name "osxi")
    (erc-tls :server "poetic.irc.slack.com" :port 6667
	 :nick "zach" :full-name "zach" :password "<password>")))
;; mu4e config
(setq mu4e-mu-binary "/usr/local/bin/mu")
(setq mu4e-maildir (expand-file-name "~/.mail"))
(setq mu4e-drafts-folder "/[Gmail].Drafts")
(setq mu4e-sent-folder   "/[Gmail].Sent Mail")
(setq mu4e-trash-folder  "/[Gmail].Trash")
(setq mu4e-sent-messages-behavior 'delete)
(setq mu4e-maildir-shortcuts
      '(("/INBOX"             . ?i)
        ("/[Gmail].Sent Mail" . ?s)
        ("/[Gmail].Trash"     . ?t)))
(setq mu4e-get-mail-command "offlineimap")
(setq mu4e-update-interval 300)
(setq user-mail-address "zach@poeticsystems.com"
      user-full-name    "Zach Ngo"
      mu4e-compose-signature (concat "Zach"))
(require 'smtpmail)
(setq message-send-mail-function 'smtpmail-send-it
      starttls-use-gnutls t
      smtpmail-starttls-credentials
      '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials
      (expand-file-name "~/.authinfo.gpg")
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-debug-info t)
;; GUI PATH
(defun set-exec-path-from-shell-PATH ()
  "Set PATH to the path from shell"
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq eshell-path-env path-from-shell) ; for eshell users
    (setq exec-path (split-string path-from-shell path-separator))))
(when window-system (set-exec-path-from-shell-PATH))
;; ido-vertical
(require 'ido-vertical-mode)
(ido-vertical-mode 1)
(setq ido-vertical-show-count t)
;; switch-window
;; (require 'switch-window)
;; (global-set-key (kbd "C-x o") 'switch-window)
(global-set-key (kbd "<left>") 'shrink-window-horizontally)
(global-set-key (kbd "<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "<down>") 'shrink-window)
(global-set-key (kbd "<up>") 'enlarge-window)
;; elfeed
;; (elfeed-add-feed "http://rss.slashdot.org/slashdot/slashdotMain?format=xml")
(setq elfeed-feeds '("http://rss.slashdot.org/slashdot/slashdotMain?format=xml"))
(defun reset-erc-track-mode ()
  "Reset tracked channels in ERC."
  (interactive)
  (setq erc-modified-channels-alist nil)
  (erc-modified-channels-update))
(defun xml-format ()
  "Pretty print XML."
  (interactive)
  (save-excursion
    (shell-command-on-region (mark) (point) "xmllint --format -" (buffer-name) t)))
(defun parse-html (html)
  "Convert HTML into plain text."
  (interactive)
  ;; TODO: flag-based logic for skipping "<" that has no matching ">"
  ;; replace /\<.*\>/ig in `html' with empty string
  (replace-regexp-in-string "\<\\([A-Za-z]\\|\/[A-Za-z]\\)*\>" "" html))
(defun strip-css (html)
  "Strip CSS style tags from HTML text."
  (interactive)
  (replace-regexp-in-string "\<style.*\>.*\<\/style\>" "" html))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(ansi-term-color-vector
   [unspecified "black" "red" "green" "yellow" "blue" "brightmagenta" "cyan" "white"] t)
 '(cider-lein-command "/usr/local/bin/lein")
 '(coffee-tab-width 2)
 '(css-indent-offset 2)
 '(custom-enabled-themes (quote (zenburn)))
 '(custom-safe-themes
   (quote
    ("40f6a7af0dfad67c0d4df2a1dd86175436d79fc69ea61614d668a635c2cd94ab" "90feb3aa23dfadcb335260a82b4ca85eb3dfb540a21c1c5a12a61ffc42828eb7" "20e359ef1818a838aff271a72f0f689f5551a27704bf1c9469a5c2657b417e6c" "f024aea709fb96583cf4ced924139ac60ddca48d25c23a9d1cd657a2cf1e4728" "f5eb916f6bd4e743206913e6f28051249de8ccfd070eae47b5bde31ee813d55f" "55d31108a7dc4a268a1432cd60a7558824223684afecefa6fae327212c40f8d3" "11636897679ca534f0dec6f5e3cb12f28bf217a527755f6b9e744bd240ed47e1" "cb8d13429234ff2a8700da4db9bdf6b952c1b54b906a1aad2d0d98317c5b0224" "cedd3b4295ac0a41ef48376e16b4745c25fa8e7b4f706173083f16d5792bb379" "ba9be9caf9aa91eb34cf11ad9e8c61e54db68d2d474f99a52ba7e87097fa27f5" "a444b2e10bedc64e4c7f312a737271f9a2f2542c67caa13b04d525196562bf38" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "3a9249d4c34f75776e130efd7e02c4a0a7c90ad7723b50acc5806112394ec2dd" "51b8c4adab95ff23b8f5cf07ea0b9805c8662936fe0d877d61a0dd02b6adc5f6" "bfbe39eae84983ca5c3ad6c9ccccd23d6e324e124b825721c20e3062ebc663bd" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "d8f76414f8f2dcb045a37eb155bfaa2e1d17b6573ed43fb1d18b936febc7bbc2" "ac2b1fed9c0f0190045359327e963ddad250e131fbf332e80d371b2e1dbc1dc4" "95a6ac1b01dcaed4175946b581461e16e1b909d354ada79770c0821e491067c6" "8f2e60e25bd33a29f45867d99c49afd9d7f3f3ed8a60926d32d5a23c790de240" "b06aaf5cefc4043ba018ca497a9414141341cb5a2152db84a9a80020d35644d1" "8b51a9d5604680d5d533c9cae132f68bca1e02563b2b0943ff9d45eb9043605a" "d44939ef462b7efb9bb5739f2dd50b03ac9ecf98c4df6578edcf145d6a2d188d" "6a9606327ecca6e772fba6ef46137d129e6d1888dcfc65d0b9b27a7a00a4af20" "90d329edc17c6f4e43dbc67709067ccd6c0a3caa355f305de2041755986548f2" "d5de5ffdc352e765d4cdf02716941d932b9587dc2f768912e123cde24221b77e" "e80932ca56b0f109f8545576531d3fc79487ca35a9a9693b62bf30d6d08c9aaf" "efb148b9a120f417464713fe6cad47eb708dc45c7f2dbfeea4a7ec329214e63e" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "3d003561784526d83d1dd187aecf4799c72af27046bc3aa2f6d95c64e5ee4746" "456ac8176c7c01680384356cbdc568a7683d1bada9c83ae3d7294809ddda4014" "2affb26fb9a1b9325f05f4233d08ccbba7ec6e0c99c64681895219f964aac7af" "569dc84822fc0ac6025f50df56eeee0843bffdeceff2c1f1d3b87d4f7d9fa661" "e4bc8563d7651b2fed20402fe37b7ab7cb72869f92a3e705907aaecc706117b5" "c63bee436bd8a67f6d9cd96acd5720c4a949c169f09c6850c439751f14fd923e" "a655f17225ad0a7190c79602593563191b7640ddebbb8c8fbd80c9d82faff1c6" "9eb5269753c507a2b48d74228b32dcfbb3d1dbfd30c66c0efed8218d28b8f0dc" "f0ea6118d1414b24c2e4babdc8e252707727e7b4ff2e791129f240a2b3093e32" "0795e2c85394140788d72d34969be4acb305e4a54149e7237787d9df27832fbb" "53e29ea3d0251198924328fd943d6ead860e9f47af8d22f0b764d11168455a8e" "4a60f0178f5cfd5eafe73e0fc2699a03da90ddb79ac6dbc73042a591ae216f03" "0e121ff9bef6937edad8dfcff7d88ac9219b5b4f1570fd1702e546a80dba0832" "ef08e77c67344f23154fd8cb9f3b8b1b4bb1799c0bf9d05dfd4b792557e0e401" "d96416845141e99d05d45b5f99ecf46458bf97654be7d2e20184c5edcda1580a" "3c093ea152d7185cc78b61b05e52648c6d2fb0d8579c2119d775630fa459e0be" "fc2782b33667eb932e4ffe9dac475f898bf7c656f8ba60e2276704fabb7fa63b" "c3e567dedaa800e869d879c4df8478237d6ea31fd04464086fd674c864fe4d71" "d3ad220a181d1558f1fb2815d10182458115b365955bdaf67af52870be044eb4" "2a12e95e9ee6ed57592e7df12f3f028205575e9b3affdb5e6fa589421c618136" "8cf56691a70156f611ac86d0bbcbc7dee7673df195de5918f34bfdc6814ffd39" "1b946d5350c1b4ff2d3cdf42ef867520544b1ae55ddcd6f2e11020a236c87af4" "f8f37b678772edee27c1bf223b16f7fca0a20d18535eb59dff1679bea36e602d" "9453cdc7dcadbc86feccf22dcc0ab895b4199ce29e63ef140218202b4b98ce77" "cc0dbb53a10215b696d391a90de635ba1699072745bf653b53774706999208e3" "f6e959c888fb0b0e975ed702bae7c6bb9d775d6f18cfef73c8173fab85776e3f" "b9183de9666c3a16a7ffa7faaa8e9941b8d0ab50f9aaba1ca49f2f3aec7e3be9" default)))
 '(dired-listing-switches "-alh")
 '(electric-indent-mode t)
 '(erc-modules
   (quote
    (autojoin button completion fill irccontrols list match menu move-to-prompt netsplit networks noncommands readonly ring scrolltobottom stamp track)))
 '(eshell-banner-message "Welcome to Emacs hell

")
 '(eshell-prompt-function
   (lambda nil
     (concat
      (propertize
       (join-list
        (abbreviated-pwd)
        "/")
       (quote face)
       (\`
        (:foreground "white10")))
      (propertize " % "
                  (quote face)
                  (\`
                   (:foreground "white"))))))
 '(fci-rule-color "#383838" t)
 '(flycheck-lintr-caching nil)
 '(frame-brackground-mode (quote dark))
 '(gofmt-command "/usr/local/bin/gofmt")
 '(indent-tabs-mode nil)
 '(js-curly-indent-offset 0)
 '(js-expr-indent-offset 0)
 '(js-indent-level 2)
 '(js-switch-indent-offset 2)
 '(jsx-indent-level 2)
 '(lisp-tag-indentation 2)
 '(magit-status-buffer-switch-function (quote ignore))
 '(menu-bar-mode nil)
 '(mode-line-format
   (quote
    ("%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification mode-line-buffer-identification " " mode-line-position
     (vc-mode vc-mode)
     mode-line-modes mode-line-misc-info "[%l.%c/%i] " mode-line-end-spaces)))
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(package-selected-packages
   (quote
    (exec-path-from-shell elm-mode ztree haskell-mode flymake-jshint lua-mode minimap autumn-light-theme go-mode elnode coffee-mode hl-todo cider yaml-mode elixir-mode jsx-mode tronesque-theme firebelly-theme scala-mode clojure-mode alert bash-completion elfeed php-mode zenburn-theme web-mode warm-night-theme transpose-frame switch-window spacegray-theme sokoban soft-charcoal-theme smex smart-mode-line scss-mode screenshot restclient projectile-rails pbcopy paredit org-trello org-present obsidian-theme naquadah-theme multiple-cursors multi-term mu4e-maildirs-extension molokai-theme minimal-theme markdown-mode magit ir-black-theme ido-vertical-mode ido-ubiquitous highlight-indentation hexrgb handlebars-mode haml-mode guide-key gruber-darker-theme gntp git-gutter frame-fns flycheck flatland-black-theme flappymacs faces+ espresso-theme ember-mode darkburn-theme dark-krystal-theme column-marker colonoscopy-theme calmer-forest-theme boron-theme bliss-theme atom-dark-theme anti-zenburn-theme ample-zen-theme afternoon-theme ada-mode ack-menu ack ac-html 4clojure 2048-game)))
 '(restclient-same-buffer-response-name "*HTTP Response*")
 '(send-mail-function nil)
 '(sh-indentation 2)
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3")
 '(verilog-case-fold t nil nil "testing123")
 '(web-mode-attr-indent-offset 2)
 '(web-mode-code-indent-offset 2)
 '(web-mode-css-indent-offset 2)
 '(web-mode-markup-indent-offset 2)
 '(web-mode-sql-indent-offset 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :foreground "#c5c8c6" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 160 :width normal :foundry "nil" :family "Input Mono"))))
 '(dired-mark ((t (:inherit font-lock-constant-face))))
 '(mode-line-inactive ((t (:background "grey20" :foreground "grey50" :box (:line-width 1 :color "grey90" :style unspecified) :overline "grey90" :underline nil)))))

;; Powerline mode-line
;; (powerline-bad-ass-theme)

(provide 'init)
;;; init.el ends here
