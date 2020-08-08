;; setup cask for emacs project management
(require 'cask "~/.cask/cask.el")
(cask-initialize)

;; really hate the title bar, menu options, scrollbar in emacs
;; no use to me, removing these
(when (window-system)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1))

;; sync system clipboard with emacs <->
(transient-mark-mode t)
(setq x-select-enable-clipboard t)
(setq x-select-enable-primary t)

;; you would () want parenthesis at once, also hightlight parenthesis
(electric-pair-mode)
(show-paren-mode t)

;; better wrapping mode for each line
(global-visual-line-mode t)

;; no need to make backup files
(setq make-backup-files nil)

;; also disable auto save
(setq auto-save-default nil)

;; default tab width
(setq-default tab-width 2)

;; enable auto indentation
(electric-indent-mode t)

;; really important for editing, multiple-cursors
(require  'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C-{") 'mc/mark-next-like-this)
(global-set-key (kbd "C-}") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-{") 'mc/mark-all-like-this)

;; some important keybindings
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-c C-k") 'compile)
(global-set-key (kbd "C-x g") 'magit-status)

;; really like dumb-jump for function/definitition navigation
(global-set-key (kbd "M-g j") 'dumb-jump-go)
(global-set-key (kbd "M-g p") 'dumb-jump-back)
(global-set-key (kbd "M-g w") 'dumb-jump-go-other-window)

;; projectile mode is the best for projects navigation, works really well with rails with projectile-rails
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(projectile-rails-global-mode)
(define-key projectile-rails-mode-map (kbd "C-c r") 'projectile-rails-command-map)

;; using helm for projects/files/everything navigation easily
(require 'helm)
(helm-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(require 'helm-projectile)
(helm-projectile-on)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
	 (quote
		("f56eb33cd9f1e49c5df0080a3e8a292e83890a61a89bceeaa481a5f183e8e3ef" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; set default theme
(set-frame-font "Mono-16")

;; some javascript configurations
(setq js-indent-level 2)
(setq js2-basic-offset 2)
(setq javascript-indent-level 2)
(setq css-indent-offset 2)
(setq web-mode-markup-indent-offset 2)

;; org-agenda-mode
(global-set-key (kbd "C-c a") 'org-agenda-list)
