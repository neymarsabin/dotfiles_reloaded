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
(set-frame-font "Mono-14")

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
	 ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-enabled-themes (quote (doom-one)))
 '(custom-safe-themes
	 (quote
		("f0dc4ddca147f3c7b1c7397141b888562a48d9888f1595d69572db73be99a024" "d1b4990bd599f5e2186c3f75769a2c5334063e9e541e37514942c27975700370" "d2e9c7e31e574bf38f4b0fb927aaff20c1e5f92f72001102758005e53d77b8c9" "adf5275cc3264f0a938d97ded007c82913906fc6cd64458eaae6853f6be287ce" "cd736a63aa586be066d5a1f0e51179239fe70e16a9f18991f6f5d99732cabb32" "08ef1356470a9d3bf363ffab0705d90f8a492796e9db489936de4bde6a4fdb19" "b35a14c7d94c1f411890d45edfb9dc1bd61c5becd5c326790b51df6ebf60f402" "4697a2d4afca3f5ed4fdf5f715e36a6cac5c6154e105f3596b44a4874ae52c45" "93a0885d5f46d2aeac12bf6be1754faa7d5e28b27926b8aa812840fe7d0b7983" "b54826e5d9978d59f9e0a169bbd4739dd927eead3ef65f56786621b53c031a7c" "6b2636879127bf6124ce541b1b2824800afc49c6ccd65439d6eb987dbf200c36" "3a3de615f80a0e8706208f0a71bbcc7cc3816988f971b6d237223b6731f91605" default)))
 '(fci-rule-color "#5B6268")
 '(jdee-db-active-breakpoint-face-colors (cons "#1B2229" "#51afef"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1B2229" "#98be65"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1B2229" "#3f444a"))
 '(package-selected-packages
	 (quote
		(dracula-theme sexy-monochrome-theme klere-theme dark-mint-theme calmer-forest-theme flucui-themes db-pg pg avy vimish-fold theme-changer atom-one-dark-theme go-projectile which-key go-playground go-complete go-eldoc go-autocomplete project-explorer helm-xref zerodark-theme shackle graphql-mode impatient-mode flymd nginx-mode dockerfile-mode yaml-mode neotree bundler gist try go-mode helm-ag helm-projectile doom-themes yasnippet-snippets dumb-jump magit multiple-cursors rjsx-mode ag projectile-rails projectile helm)))
 '(projectile-rails-global-mode t)
 '(tetris-x-colors
	 [[229 192 123]
		[97 175 239]
		[209 154 102]
		[224 108 117]
		[152 195 121]
		[198 120 221]
		[86 182 194]])
 '(vc-annotate-background "#282c34")
 '(vc-annotate-color-map
	 (list
		(cons 20 "#98be65")
		(cons 40 "#b4be6c")
		(cons 60 "#d0be73")
		(cons 80 "#ECBE7B")
		(cons 100 "#e6ab6a")
		(cons 120 "#e09859")
		(cons 140 "#da8548")
		(cons 160 "#d38079")
		(cons 180 "#cc7cab")
		(cons 200 "#c678dd")
		(cons 220 "#d974b7")
		(cons 240 "#ec7091")
		(cons 260 "#ff6c6b")
		(cons 280 "#cf6162")
		(cons 300 "#9f585a")
		(cons 320 "#6f4e52")
		(cons 340 "#5B6268")
		(cons 360 "#5B6268")))
 '(vc-annotate-very-old-color nil)
 '(which-key-mode t)
 '(winner-mode t)
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

;; enable ido mode
(ido-mode 1)


;; javascript mode indentation fixes
(setq default-tab-width 2)
(setq js-indent-level 2)
(setq-default js2-basic-offset 2)
(setq javascript-indent-level 2)

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
(setq-default cursor-type 'bar)

;; hightlight symbol mode
(defun highlight-symbol-my-binds ()
  (interactive)
  (el-get 'sync 'highlight-symbol)
  (require 'highlight-symbol)
  (local-set-key [(control f3)] 'highlight-symbol-at-point)
  (local-set-key [(shift f3)] 'highlight-symbol-next)
  (local-set-key [(shift f2)] 'highlight-symbol-prev)

  ;; by default its just for if tabbar config didn't run
  (local-unset-key (kbd "<C-down-mouse-1>"))

  (local-set-key (kbd "<C-down-mouse-1>") (lambda (event)
    (interactive "e")
    (save-excursion
      (goto-char (posn-point (event-start event)))
      (highlight-symbol-at-point)))))

(add-hook 'prog-mode-hook 'highlight-symbol-my-binds)
