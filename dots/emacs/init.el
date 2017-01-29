;;cask
(require 'cask "~/.cask/cask.el")
(cask-initialize)

;;org mode initialize latest
(add-to-list 'load-path "~/.emacs.d/org/org-mode/lisp")


;;projectile mode global mode
(projectile-global-mode)
(projectile-rails-global-mode)
(require 'helm-config)
(require 'helm)
(helm-autoresize-mode t)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x k") 'ido-kill-buffer)
(global-set-key (kbd "C-c h g") 'helm-google-suggest)
(global-set-key (kbd "C-c h c") 'helm-calcul-expression)
(global-set-key (kbd "C-c h m") 'helm-man-woman)
(global-set-key (kbd "C-c h l") 'helm-locate)
(global-set-key (kbd "C-c h t") 'helm-top)
(global-set-key (kbd "C-c C-l") 'helm-minibuffer-history)
(global-set-key (kbd "C-s") 'helm-swoop)


;;yasnippet loading
(yas-global-mode t)

;;package require at first 
;;(require 'package)
;;(package-initialize)
;;(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t) 

;;minor mode custom
(defvar my-keys-minor-mode-map (make-keymap) "my keys")
(define-minor-mode my-keys-minor-mode
	"A minor mode for my custom keys"
	t " my-keys" 'my-keys-minor-mode-map)
(my-keys-minor-mode t)


;;changing the window unwanteds  
(when (window-system)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1))
(display-battery-mode t)

;;marking and selection
(transient-mark-mode t)
(setq x-select-enable-clipboard t)
(setq x-select-enable-primary t)

;;electric pair mode 
(electric-pair-mode)

;; setting font sizes 
(set-frame-font "monaco-14")

;; wrapping made better
(global-visual-line-mode t)

;;delete selection mode
(delete-selection-mode t)

;;blink cursor mode
(blink-cursor-mode t)
;;show paren mode 
(show-paren-mode t)

;;disable backup files
(setq make-backup-files nil)

;;disable auto save stuff 
(setq auto-save-default nil)

;;supress start message 
(setq inhibit-startup-message t)

;;default tabe width 
(setq-default tab-width 2)

;;aliases for yes and no 
(fset 'yes-or-no 'y-or-n)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
	 ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-enabled-themes (quote (wombat)))
 '(custom-safe-themes
	 (quote
		("c7a9a68bd07e38620a5508fef62ec079d274475c8f92d75ed0c33c45fbe306bc" default)))
 '(package-selected-packages
	 (quote
		(ace-jump-mode flx-ido browse-kill-ring expand-region))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;expand region mode
(global-set-key (kbd "C-o") 'er/expand-region)


;;activate ido mode
(ido-mode 1)
(ido-everywhere 1)

;;use flx mode with ido 
(flx-ido-mode 1)
(setq ido-use-faces nil)

;;ace jump mode
(define-key my-keys-minor-mode-map (kbd "C-c SPC") 'ace-jump-mode)

;;save place package(cursor place upon exit)
(require 'saveplace)
(setq save-place-file (concat user-emacs-directory "saveplace.el"))
(setq-default save-place t)

;;autoindentation
(electric-indent-mode t)

;;autocompletion
(require 'auto-complete-config)
(ac-config-default)

;;select the current line as a whole 
(defun select-current-line ()
	"Selects the current line"
	(interactive)
	(end-of-line)
	(push-mark (line-beginning-position) nil t))
(define-key my-keys-minor-mode-map (kbd "M-i") 'select-current-line)

;;select and cut/copy selected region
(defun cut-line-or-region()
	"selecting and cut/copy of that region checking condition"
	(interactive)
	(if (region-active-p)
			(kill-region (region-beginning) (region-end))
		(kill-region (line-beginning-position) (line-beginning-position 2))))
(global-set-key [remap kill-region] 'cut-line-or-region)

;;adding hook for org mode
;; (add-hook 'org-mode-hook (lambda()
;; 													 set (make-local-variable 'electric-indent-functions)
;; 													 (list (lambda(arg) 'no-indent))))
;; (setq org-src-fontify-natively t)



;; ;;FLX ido
(setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
(defun ido-disable-line-truncation () (set (make-local-variable 'truncate-lines) nil))
(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation)
(defun ido-define-keys () ;; C-n/p is more intuitive in vertical layout
  (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
  (define-key ido-completion-map (kbd "C-p") 'ido-prev-match))
(add-hook 'ido-setup-hook 'ido-define-keys)


;;multiple cursors
(require  'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C-{") 'mc/mark-next-like-this)
(global-set-key (kbd "C-}") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-{") 'mc/mark-all-like-this)

;;ERB hooks 
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.erb$" . web-mode))

;;emmet mode and hooks 
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
(add-hook 'web-mode-hook 'emmet-mode) ;; enable emmet's abbreviation in web mode as well
(add-hook 'php-mode-hook 'emmet-mode) ;; enable html mode and php mode alongside

;;Revealjs settings
(require 'ox-reveal)
(setq org-reveal-root "file:///mnt/hackit/codeds/github-repos/reveal.js/reveal.js")

;;org babel for languages 
(org-babel-do-load-languages
'org-babel-load-languages
'((python . t)
  (C . t)
  (calc . t)
  (latex . t)
  (java . t)
  (ruby . t)
  (scheme . t)
  (sh . t)
  (sqlite . t)
  (js . t)))

 (require 'ox-ioslide)

;;org agenda 
(define-key global-map "\C-ca" 'org-agenda)

;;custom org mode templates 
(add-to-list 'org-structure-template-alist
             '("C" "#+begin_html \n <div class=\"code-block\"> \n #+end_html \n #+begin_src ?\n\n #+end_src \n #+begin_html \n </div> \n #+end_html" ))


;; ;;org mode for blogging and stuffs
;; (setq org-publish-project-alist
;;       '(
;; 				("org-neymarsabin"
;;           ;; Path to your org files.
;;           :base-directory "/mnt/hackit/codeds/github-website/org-web-site/org/"
;;           :base-extension "org"

;;           ;; Path to your Jekyll project.
;;           :publishing-directory "/mnt/hackit/codeds/github-website/org-web-site/jekyll/"
;;           :recursive t
;; 					:publishing-function org-html-publish-to-html
;;           :headline-levels 4 
;;           :html-extension "html"
;; 					:body-only t
;; 					)


;; 				("org-static-sabin"
;;           :base-directory "/mnt/hackit/codeds/github-website/org-web-site/org/"
;;           :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|php"
;;           :publishing-directory "/mnt/hackit/codeds/github-website/org-web-site/"
;;           :recursive t
;;           :publishing-function org-publish-attachment)

;; 				("sabin" :components ("org-neymarsabin" "org-static-sabin"))
				
;; 				))

;;load theme 
(load-theme 'wombat t)

;;basic key bindings
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-c C-k") 'compile)
(global-set-key (kbd "C-x g") 'magit-status)

;;my blogging setup
(setq org-publish-project-alist
      '(("blog"
         :base-directory "/mnt/hackit/codeds/orgblog/posts"
         :html-extension "html"
         :base-extension "org"
         :publishing-directory "/mnt/hackit/codeds/orgblog/html"
         :publishing-function (org-html-publish-to-html)
         :html-preamble nil
         :html-postamble nil )))

;;reload emacs without closing 
(defun reload-user-init-file()
	(interactive)
	(load-file user-init-file))

;; adding mode-icons 
(add-to-list 'load-path "/mnt/hackit/codeds/github-repos/mode-icons/")
(require 'mode-icons)
(mode-icons-mode)
