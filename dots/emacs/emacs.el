;; package settings

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; window settings
(when (window-system)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
	(toggle-scroll-bar -1))
(display-battery-mode t)
(unless (display-graphic-p)
	(menu-bar-mode -1)
	(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
	)

;; marking and selection
(transient-mark-mode t)
(setq x-select-enable-clipboard t)
(setq x-select-enable-primary t)

;; electric pair mode
(electric-pair-mode)

;; setting font sizes
;; (set-frame-font "monaco-11")
(set-frame-font "Mono-10")

;; wrapping made better
(global-visual-line-mode t)

;;delete selection mode
(delete-selection-mode t)

;; blink cursor and show parenthesis
(blink-cursor-mode t)
(show-paren-mode t)

;; disable backup files and auto save stuffs
(setq make-backup-files nil)
(setq auto-save-default nil)

;;supress start message 
(setq inhibit-startup-message t)

;;default tabe width 
(setq-default tab-width 2)

;; expand region mode
(global-set-key (kbd "C-o") 'er/expand-region)

;; auto indentation
(electric-indent-mode t)

;;select and cut/copy selected region
(defun cut-line-or-region()
	"selecting and cut/copy of that region checking condition"
	(interactive)
	(if (region-active-p)
			(kill-region (region-beginning) (region-end))
		(kill-region (line-beginning-position) (line-beginning-position 2))))
(global-set-key [remap kill-region] 'cut-line-or-region)


;; flx ido
;; (setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
;; (defun ido-disable-line-truncation () (set (make-local-variable 'truncate-lines) nil))
;; (add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation)
;; (defun ido-define-keys () ;; C-n/p is more intuitive in vertical layout
;;   (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
;;   (define-key ido-completion-map (kbd "C-p") 'ido-prev-match))
;; (add-hook 'ido-setup-hook 'ido-define-keys)

;; basic key bindings
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-c C-k") 'compile)
(global-set-key (kbd "C-x g") 'magit-status)

;; reload emacs without closing
(defun reload-user-init-file()
	(interactive)
	(load-file user-init-file))

;;electric pair mode
(electric-pair-mode)

;; org babel for languages
(org-babel-do-load-languages
'org-babel-load-languages
'((python . t)
  (C . t)
	(css . t)
  (calc . t)
  (latex . t)
  (java . t)
  (ruby . t)
  (scheme . t)
  (shell . t)
  (sqlite . t)
  (js . t)
  (dot . t)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
	 [default default default italic underline success warning error])
 '(ansi-color-names-vector
	 ["#303030" "#ff4b4b" "#d7ff5f" "#fce94f" "#5fafd7" "#d18aff" "#afd7ff" "#c6c6c6"])
 '(ansi-term-color-vector
	 [unspecified "#32302f" "#fb4934" "#b8bb26" "#fabd2f" "#83a598" "#d3869b" "#83a598" "#d5c4a1"] t)
 '(beacon-color "#ffffff")
 '(company-quickhelp-color-background "#b0b0b0")
 '(company-quickhelp-color-foreground "#232333")
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#ffffff")
 '(cua-normal-cursor-color "#ffffff")
 '(cua-overwrite-cursor-color "#ffffff")
 '(cua-read-only-cursor-color "#ffffff")
 '(custom-enabled-themes (quote (doom-one)))
 '(custom-safe-themes
	 (quote
		("fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "174502267725776b47bdd2d220f035cae2c00c818765b138fea376b2cdc15eb6" "34c99997eaa73d64b1aaa95caca9f0d64229871c200c5254526d0062f8074693" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "68b847fac07094724e552eeaf96fa4c7e20824ed5f3f225cad871b8609d50ace" "1c10e946f9a22b28613196e4c02b6508970e3b34660282ec92d9a1c293ee81bb" "45482e7ddf47ab1f30fe05f75e5f2d2118635f5797687e88571842ff6f18b4d5" "2047464bf6781156ebdac9e38a17b97bd2594b39cfeaab561afffcbbe19314e2" "b5cff93c3c6ed12d09ce827231b0f5d4925cfda018c9dcf93a2517ce3739e7f1" "b8c5adfc0230bd8e8d73450c2cd4044ad7ba1d24458e37b6dec65607fc392980" "abe3405767afe98b35b6a2b212af1fbc34e4f4c455310d2b7f2ffd2ec81d387b" "be5b03913a1aaa3709d731e1fcfd4f162db6ca512df9196c8d4693538fa50b86" "a455366c5cdacebd8adaa99d50e37430b0170326e7640a688e9d9ad406e2edfd" "a0feb1322de9e26a4d209d1cfa236deaf64662bb604fa513cca6a057ddf0ef64" "7559ac0083d1f08a46f65920303f970898a3d80f05905d01e81d49bb4c7f9e39" "16dd114a84d0aeccc5ad6fd64752a11ea2e841e3853234f19dc02a7b91f5d661" "dd4628d6c2d1f84ad7908c859797b24cc6239dfe7d71b3363ccdd2b88963f336" "0f302165235625ca5a827ac2f963c102a635f27879637d9021c04d845a32c568" "ec0c9d1715065a594af90e19e596e737c7b2cdaa18eb1b71baf7ef696adbefb0" "cb39485fd94dabefc5f2b729b963cbd0bac9461000c57eae454131ed4954a8ac" "0ca71d3462db28ebdef0529995c2d0fdb90650c8e31631e92b9f02bd1bfc5f36" "96998f6f11ef9f551b427b8853d947a7857ea5a578c75aa9c4e7c73fe04d10b4" "5a2772c3ba633ab530cf1c648c5828d2e061ca7d454c2d67c88d044fdd848fa7" "ab98c7f7a58add58293ac67bec05ae163b5d3f35cddf18753b2b073c3fcd8841" "44bff5692b73759fa076b42ce37547d2ae98499d5c700bc8297973cb81212dcf" "abdb1863bc138f43c29ddb84f614b14e3819982936c43b974224641b0b6b8ba4" "e93b6d6a610a4de38b345eb5d57500ed115e12fd67bea1bb32720cd255bfd458" "88049c35e4a6cedd4437ff6b093230b687d8a1fb65408ef17bfcf9b7338734f6" "6daa09c8c2c68de3ff1b83694115231faa7e650fdbb668bc76275f0f2ce2a437" "1436d643b98844555d56c59c74004eb158dc85fc55d2e7205f8d9b8c860e177f" "5ed25f51c2ed06fc63ada02d3af8ed860d62707e96efc826f4a88fd511f45a1d" "de1f10725856538a8c373b3a314d41b450b8eba21d653c4a4498d52bb801ecd2" "c9b89349d269af4ac5d832759df2f142ae50b0bbbabcce9c0dd53f79008443c9" "2540689fd0bc5d74c4682764ff6c94057ba8061a98be5dd21116bf7bf301acfb" "158013ec40a6e2844dbda340dbabda6e179a53e0aea04a4d383d69c329fba6e6" "3fa07dd06f4aff80df2d820084db9ecbc007541ce7f15474f1d956c846a3238f" "b563a87aa29096e0b2e38889f7a5e3babde9982262181b65de9ce8b78e9324d5" "9399db70f2d5af9c6e82d4f5879b2354b28bc7b5e00cc8c9d568e5db598255c4" "9a3366202553fb2d2ad1a8fa3ac82175c4ec0ab1f49788dc7cfecadbcf1d6a81" "e3d6636d03c788a416157c9d34184672b500d63d82de0e2d9f36e9975fd63b9f" "d2868794b5951d57fb30bf223a7e46f3a18bf7124a1c288a87bd5701b53d775a" "1438a0656b1b25c0589edcb51229710d8c710ae86ddae4238c5e9226f58ab336" "811dabdae799fd679ab73ec15c987096ca7573afb43de3474c27405f032a7b9e" "add84a254d0176ffc2534cd05a17d57eea4a0b764146139656b4b7d446394a54" "d12a9fcbb4d9ca0eb77d9fc44c4cf409973aad4e467b89bd52c46f14e9886f6a" "14157dcd9c4e5669d89af65628f4eaf3247e24c2c0d134db48951afb2bdad421" "565aa482e486e2bdb9c3cf5bfb14d1a07c4a42cfc0dc9d6a14069e53b6435b56" "075351c6aeaddd2343155cbcd4168da14f54284453b2f1c11d051b2687d6dc48" "f0dc4ddca147f3c7b1c7397141b888562a48d9888f1595d69572db73be99a024" "d1b4990bd599f5e2186c3f75769a2c5334063e9e541e37514942c27975700370" "d2e9c7e31e574bf38f4b0fb927aaff20c1e5f92f72001102758005e53d77b8c9" "adf5275cc3264f0a938d97ded007c82913906fc6cd64458eaae6853f6be287ce" "cd736a63aa586be066d5a1f0e51179239fe70e16a9f18991f6f5d99732cabb32" "08ef1356470a9d3bf363ffab0705d90f8a492796e9db489936de4bde6a4fdb19" "b35a14c7d94c1f411890d45edfb9dc1bd61c5becd5c326790b51df6ebf60f402" "4697a2d4afca3f5ed4fdf5f715e36a6cac5c6154e105f3596b44a4874ae52c45" "93a0885d5f46d2aeac12bf6be1754faa7d5e28b27926b8aa812840fe7d0b7983" "b54826e5d9978d59f9e0a169bbd4739dd927eead3ef65f56786621b53c031a7c" "6b2636879127bf6124ce541b1b2824800afc49c6ccd65439d6eb987dbf200c36" "3a3de615f80a0e8706208f0a71bbcc7cc3816988f971b6d237223b6731f91605" default)))
 '(dap-mode t nil (dap-mode))
 '(dap-ui-mode t nil (dap-ui))
 '(diary-entry-marker (quote font-lock-variable-name-face))
 '(emms-mode-line-icon-image-cache
	 (quote
		(image :type xpm :ascent center :data "/* XPM */
static char *note[] = {
/* width height num_colors chars_per_pixel */
\"    10   11        2            1\",
/* colors */
\". c #358d8d\",
\"# c None s None\",
/* pixels */
\"###...####\",
\"###.#...##\",
\"###.###...\",
\"###.#####.\",
\"###.#####.\",
\"#...#####.\",
\"....#####.\",
\"#..######.\",
\"#######...\",
\"######....\",
\"#######..#\" };")))
 '(fci-rule-color "#c7c7c7")
 '(flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id))
 '(flycheck-disabled-checkers (quote (javascript-jshint javascript-jscs)))
 '(frame-background-mode (quote light))
 '(gnus-logo-colors (quote ("#259ea2" "#adadad")) t)
 '(gnus-mode-line-image-cache
	 (quote
		(image :type xpm :ascent center :data "/* XPM */
static char *gnus-pointer[] = {
/* width height num_colors chars_per_pixel */
\"    18    13        2            1\",
/* colors */
\". c #358d8d\",
\"# c None s None\",
/* pixels */
\"##################\",
\"######..##..######\",
\"#####........#####\",
\"#.##.##..##...####\",
\"#...####.###...##.\",
\"#..###.######.....\",
\"#####.########...#\",
\"###########.######\",
\"####.###.#..######\",
\"######..###.######\",
\"###....####.######\",
\"###..######.######\",
\"###########.######\" };")) t)
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
	 (--map
		(solarized-color-blend it "#fdf6e3" 0.25)
		(quote
		 ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors
	 (quote
		(("#eee8d5" . 0)
		 ("#B4C342" . 20)
		 ("#69CABF" . 30)
		 ("#69B7F0" . 50)
		 ("#DEB542" . 60)
		 ("#F2804F" . 70)
		 ("#F771AC" . 85)
		 ("#eee8d5" . 100))))
 '(hl-bg-colors
	 (quote
		("#DEB542" "#F2804F" "#FF6E64" "#F771AC" "#9EA0E5" "#69B7F0" "#69CABF" "#B4C342")))
 '(hl-fg-colors
	 (quote
		("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3")))
 '(hl-paren-colors
	 (quote
		("#B9F" "#B8D" "#B7B" "#B69" "#B57" "#B45" "#B33" "#B11")))
 '(hl-todo-keyword-faces
	 (quote
		(("TODO" . "#dc752f")
		 ("NEXT" . "#dc752f")
		 ("THEM" . "#2d9574")
		 ("PROG" . "#3a81c3")
		 ("OKAY" . "#3a81c3")
		 ("DONT" . "#f2241f")
		 ("FAIL" . "#f2241f")
		 ("DONE" . "#42ae2c")
		 ("NOTE" . "#b1951d")
		 ("KLUDGE" . "#b1951d")
		 ("HACK" . "#b1951d")
		 ("TEMP" . "#b1951d")
		 ("FIXME" . "#dc752f")
		 ("XXX" . "#dc752f")
		 ("XXXX" . "#dc752f")
		 ("???" . "#dc752f"))))
 '(jdee-db-active-breakpoint-face-colors (cons "#1B2229" "#fd971f"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1B2229" "#b6e63e"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1B2229" "#525254"))
 '(linum-format "%3i")
 '(magit-diff-use-overlays nil)
 '(notmuch-search-line-faces
	 (quote
		(("unread" :foreground "#aeee00")
		 ("flagged" :foreground "#0a9dff")
		 ("deleted" :foreground "#ff2c4b" :bold t))))
 '(nrepl-message-colors
	 (quote
		("#336c6c" "#205070" "#0f2050" "#806080" "#401440" "#6c1f1c" "#6b400c" "#23733c")))
 '(objed-cursor-color "#e74c3c")
 '(org-trello-current-prefix-keybinding "C-c o" nil (org-trello))
 '(package-selected-packages
	 (quote
		(terraform-mode nyx-theme git-timemachine ruby-test-mode systemd flatland-theme emamux prettier-js flycheck pretty-mode frog-jump-buffer nimbus-theme org-projectile react-snippets naysayer-theme helm-pass pass evil-visual-mark-mode molokai-theme paper-theme jazz-theme waher-theme darkburn-theme grandshell-theme moe-theme nova-theme lsp-mode dap-mode typescript-mode php-mode constant-theme cyberpunk-2019-theme cyberpunk-theme dark-krystal-theme display-theme django-theme busybee-theme caroline-theme challenger-deep-theme cherry-blossom-theme chyla-theme clues-theme colonoscopy-theme color-theme color-theme-approximate color-theme-buffer-local color-theme-modern color-theme-sanityinc-solarized color-theme-sanityinc-tomorrow color-theme-solarized color-theme-x commentary-theme arc-dark-theme sublime-themes helm-systemd helm-themes org-trello solarized-theme gruvbox-theme heroku-theme helm-youtube alect-themes ample-theme ample-zen-theme anti-zenburn-theme arjen-grey-theme atom-dark-theme autumn-light-theme avk-emacs-themes badger-theme badwolf-theme base16-theme basic-theme blackboard-theme bliss-theme borland-blue-theme boron-theme brutalist-theme bubbleberry-theme afternoon-theme ahungry-theme airline-themes abyss-theme sourcerer-theme spacegray-theme select-themes spacemacs-theme selectric-mode emmet-mode web-mode ace-window use-package shell-pop kaolin-themes dracula-theme sexy-monochrome-theme klere-theme dark-mint-theme calmer-forest-theme flucui-themes db-pg pg avy vimish-fold theme-changer atom-one-dark-theme go-projectile which-key go-playground go-complete go-eldoc go-autocomplete project-explorer helm-xref zerodark-theme shackle graphql-mode impatient-mode flymd nginx-mode dockerfile-mode yaml-mode neotree bundler gist try go-mode helm-ag helm-projectile doom-themes yasnippet-snippets dumb-jump magit multiple-cursors rjsx-mode ag projectile-rails projectile helm)))
 '(pdf-view-midnight-colors (quote ("#232333" . "#c7c7c7")))
 '(pos-tip-background-color "#eee8d5")
 '(pos-tip-foreground-color "#586e75")
 '(powerline-color1 "#3d3d68")
 '(powerline-color2 "#292945")
 '(projectile-rails-global-mode t)
 '(red "#ffffff")
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(tetris-x-colors
	 [[229 192 123]
		[97 175 239]
		[209 154 102]
		[224 108 117]
		[152 195 121]
		[198 120 221]
		[86 182 194]])
 '(vc-annotate-background "#d4d4d4")
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
	 (quote
		((20 . "#437c7c")
		 (40 . "#336c6c")
		 (60 . "#205070")
		 (80 . "#2f4070")
		 (100 . "#1f3060")
		 (120 . "#0f2050")
		 (140 . "#a080a0")
		 (160 . "#806080")
		 (180 . "#704d70")
		 (200 . "#603a60")
		 (220 . "#502750")
		 (240 . "#401440")
		 (260 . "#6c1f1c")
		 (280 . "#935f5c")
		 (300 . "#834744")
		 (320 . "#732f2c")
		 (340 . "#6b400c")
		 (360 . "#23733c"))))
 '(vc-annotate-very-old-color "#23733c")
 '(weechat-color-list
	 (quote
		(unspecified "#fdf6e3" "#eee8d5" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#657b83" "#839496")))
 '(which-key-mode t)
 '(winner-mode t)
 '(xterm-color-names
	 ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#073642"])
 '(xterm-color-names-bright
	 ["#fdf6e3" "#cb4b16" "#93a1a1" "#839496" "#657b83" "#6c71c4" "#586e75" "#002b36"])
 '(yas-global-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; multiple cursors configurations
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C-{") 'mc/mark-next-like-this)
(global-set-key (kbd "C-}") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-{") 'mc/mark-all-like-this)

;; dumb jump configurations
(global-set-key (kbd "M-g j") 'dumb-jump-go-other-window)
(global-set-key (kbd "M-g q") 'dumb-jump-quick-look)
(global-set-key (kbd "M-g p") 'dumb-jump-back)
;; (global-set-key (kbd "M-g r") 'dumb-jump-go-prompt)

;; js indent level
(setq js-indent-level 2)

;; projectile global mode
(projectile-global-mode)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(helm-projectile-on) ;; helm with projectile 
(projectile-rails-global-mode) ;; projectile rails global mode also enabled
(add-to-list 'projectile-globally-ignored-directories "node_modules") ;; Add to list projectile globally ignored directories

;; enable ido mode
(ido-mode 1)


;; javascript mode indentation fixes
(setq default-tab-width 2)
(setq js-indent-level 2)
(setq-default js2-basic-offset 2)
(setq javascript-indent-level 2)
(add-hook 'js2-mode-hook (lambda () (setq js2-basic-offset 2)))
(add-hook 'rjsx-mode-hook (lambda () (setq js2-basic-offset 2)))

;; set neotree theme to arrow
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

;; go mode
(require 'go-eldoc) ;; Don't need to require, if you install by package.el
;; (add-hook 'go-mode-hook 'go-eldoc-setup)
(add-to-list 'exec-path (expand-file-name "~/go/bin/godef") )
(defun my-go-mode-hook ()
  ; Call Gofmt before saving                                                    
  (add-hook 'before-save-hook 'gofmt-before-save)
  ; Godef jump key binding                                                      
  (local-set-key (kbd "M-.") 'godef-jump)
  (local-set-key (kbd "M-*") 'pop-tag-mark)
  )
(add-hook 'go-mode-hook 'my-go-mode-hook)

;; org-mode extended todos
(setq org-todo-keywords 
  '((sequence "TODO" "DOING" "BLOCKED" "REVIEW" "TESTING" "|" "DONE" "ARCHIVED" "COMPLETED")))

;; go mode hooks
(defun my-go-mode-hook ()
  ; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)
  ; Customize compile command to run go build
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet"))
  ; Godef jump key binding
  (local-set-key (kbd "M-.") 'godef-jump)
  (local-set-key (kbd "M-*") 'pop-tag-mark)
)
(add-hook 'go-mode-hook 'my-go-mode-hook)
;;; go flymake
(add-to-list 'load-path "~/go/src/github.com/dougm/goflymake")
(require 'go-flymake)

;;; go guruji
(add-to-list 'load-path "~/.emacs.d/go-guru")
(go-guru-hl-identifier-mode)
(require 'go-guru)

;; go remove unused imports
(add-hook 'go-mode-hook (lambda ()
                          (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)))

;; go goto imports
(add-hook 'go-mode-hook (lambda ()
                          (local-set-key (kbd "C-c i") 'go-goto-imports)))

;; which key mode like spacemacs
(which-key-setup-minibuffer)

;; emacs themes switcher
;; from https://github.com/thapakazi/.emacs.d/blob/master/ui.org#convenient-theme-functions

(defun switch-theme (theme)
	"Disables any currently active themes and loads THEME."
	;; This interactive call is taken from `load-theme'
	(interactive
	 (list
		(intern (completing-read "Load custom theme: "
														 (mapc 'symbol-name
																	 (custom-available-themes))))))
	(let ((enabled-themes custom-enabled-themes))
		(mapc #'disable-theme custom-enabled-themes)
		(load-theme theme t)))

;; (defun disable-active-themes ()
;;   "Disables any currently active themes listed in `custom-enabled-themes'."
;;   (interactive)
;;   (mapc #'disable-theme custom-enabled-themes))

;; (bind-key "s-<f12>" switch-theme)
;; (bind-key "s-<f11>" 'disable-active-themes)

;; set emacs straight line
(setq-default cursor-type 'box)

;; hightlight symbol mode
;; (defun highlight-symbol-my-binds ()
;;   (interactive)
;;   (el-get 'sync 'highlight-symbol)
;;   (require 'highlight-symbol)
;;   (local-set-key [(control f3)] 'highlight-symbol-at-point)
;;   (local-set-key [(shift f3)] 'highlight-symbol-next)
;;   (local-set-key [(shift f2)] 'highlight-symbol-prev)

;;   ;; by default its just for if tabbar config didn't run
;;   (local-unset-key (kbd "<C-down-mouse-1>"))

;;   (local-set-key (kbd "<C-down-mouse-1>") (lambda (event)
;;     (interactive "e")
;;     (save-excursion
;;       (goto-char (posn-point (event-start event)))
;;       (highlight-symbol-at-point)))))

;; (add-hook 'prog-mode-hook 'highlight-symbol-my-binds)

;; minimal shell pop inside emacs
(use-package shell-pop
  :bind (("C-t" . shell-pop))
  :config
  (setq shell-pop-shell-type (quote ("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell)))))
  (setq shell-pop-term-shell "/bin/zsh")
  ;; need to do this manually or not picked up by `shell-pop'
  (shell-pop--set-shell-type 'shell-pop-shell-type shell-pop-shell-type))

;; set C-x o for ace-window
(global-set-key (kbd "C-x o") 'ace-window)

;; load file gruvbox-theme.el
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

;; org-trello mode
(define-key org-trello-mode-map (kbd "C-c o i") 'org-trello/install-key-and-token)
(define-key org-trello-mode-map (kbd "C-c o I") 'org-trello/install-board-and-lists-ids)
(define-key org-trello-mode-map (kbd "C-c o d") 'org-trello/check-setup)
(define-key org-trello-mode-map (kbd "C-c o a") 'org-trello/assign-me)
(define-key org-trello-mode-map (kbd "C-c o u") 'org-trello/unassign-me)
(define-key org-trello-mode-map (kbd "C-c o x") 'org-trello/delete-setup)
(define-key org-trello-mode-map (kbd "C-c o b") 'org-trello/create-board)
(define-key org-trello-mode-map (kbd "C-c o S") 'org-trello/sync-from-trello)
(define-key org-trello-mode-map (kbd "C-c o c") 'org-trello/sync-entity)
(define-key org-trello-mode-map (kbd "C-c o C") 'org-trello/sync-full-entity)
(define-key org-trello-mode-map (kbd "C-c o k") 'org-trello/kill-entity)
(define-key org-trello-mode-map (kbd "C-c o K") 'org-trello/kill-all-entities)
(define-key org-trello-mode-map (kbd "C-c o s") 'org-trello/sync-to-trello)
(define-key org-trello-mode-map (kbd "C-c o h") 'org-trello/help-describing-bindings)
(define-key org-trello-mode-map (kbd "C-c o e") 'org-trello/describe-entry)

;; move line up and down functions
(defun move-line-up ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines -1))
    (move-to-column col)))

(defun move-line-down ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines 1))
    (forward-line)
    (move-to-column col)))

(global-set-key (kbd "<C-S-down>") 'move-line-down)
(global-set-key (kbd "<C-S-up>") 'move-line-up)

;; ;; for awesome-tray
;; (add-to-list 'load-path "~/.emacs.d/awesome-tray/")
;; (require 'awesome-tray)
;; (awesome-tray-mode 1)

;; rjsx-mode
(add-to-list 'auto-mode-alist '(".*\.js\'" . rjsx-mode))
(add-to-list 'auto-mode-alist '("components\/.*\.js\'" . rjsx-mode))
(add-to-list 'auto-mode-alist '("containers\/.*\.js\'" . rjsx-mode))

;; org cv for generating cv
(add-to-list 'load-path "~/.emacs.d/org-cv/")
(require 'ox-moderncv)

;; highlight full line mode
(global-hl-line-mode -1)

;; latex classes
(require 'ox-latex)
(with-eval-after-load 'ox-latex
   (add-to-list 'org-latex-classes
            '("moderncv"
              "\\documentclass{moderncv}"
              ("\\section{%s}" . "\\section*{%s}")
              ("\\subsection{%s}" . "\\subsection*{%s}"))))

;; eslintrc
(eval-after-load 'flycheck
  '(custom-set-variables
    '(flycheck-disabled-checkers '(javascript-jshint javascript-jscs))))
