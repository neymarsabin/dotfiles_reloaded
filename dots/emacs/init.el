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
(unless (display-graphic-p)
	(menu-bar-mode -1)
	(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
	)

;;marking and selection
(transient-mark-mode t)
(setq x-select-enable-clipboard t)
(setq x-select-enable-primary t)

;;electric pair mode 
(electric-pair-mode)

;; setting font sizes 
;; (set-frame-font "monaco-11")
(set-frame-font "Mono-14")

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

 (require 'ox-ioslide)

;;org agenda 
(define-key global-map "\C-ca" 'org-agenda)

;; ;;org mode for blogging and stuffs
(setq org-publish-project-alist
      '(
				("org-neymarsabin"
          ;; Path to your org files.
				 :base-directory "/mnt/hackit/codeds/orgblog/posts"
				 ;; :base-directory "/mnt/hackit/codeds/github-website/org-web-site/org/"
          :base-extension "org"
          ;; Path to your Jekyll project.
          :publishing-directory "/mnt/hackit/codeds/orgblog/_posts"
          :recursive t
					:publishing-function org-html-publish-to-html
          :headline-levels 4 
          :html-extension "html"
					:body-only t
					)
				;; ("org-static-sabin"
        ;;   :base-directory "/mnt/hackit/codeds/cloned/blogcopy/posts"
        ;;   :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|php"
        ;;   :publishing-directory "/mnt/hackit/codeds/cloned/console"
        ;;   :recursive t
        ;;   :publishing-function org-publish-attachment)
 				))

;;load theme 
(load-theme 'doom-molokai t)



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
         :html-postamble nil )
			("static"
				 :base-directory "/mnt/hackit/codeds/orgblog/"
				 :html-extension "html"
				 :base-extension "org"
				 :publishing-directory "/mnt/hackit/codeds/orgblog"
				 :publishing-function(org-html-publish-to-html)
				 :html-preamble nil
				 :html-postamble nil )
			("bootstrap-static"
			 :base-directory "/home/neymar/public_html/orgblog/"
			 :html-extension "html"
			 :base-extension "org"
			 :publishing-directory "/home/neymar/public_html/"
			 :publishing-function org-twbs-publish-to-html
			 :with-sub-superscript nil
			 )
			("bootstrap"
			 :base-directory "/home/neymar/public_html/orgblog/posts"
			 :html-extension "html"
			 :base-extension "org"
			 :publishing-directory "/home/neymar/public_html/html"
			 :publishing-function org-twbs-publish-to-html
			 :with-sub-superscript nil
			 )))

;;reload emacs without closing 
(defun reload-user-init-file()
	(interactive)
	(load-file user-init-file))

;; adding mode-icons 
;; (add-to-list 'load-path "/mnt/hackit/codeds/github-repos/mode-icons/")
;; (require 'mode-icons)
;; (mode-icons-mode)

;; org latex class
(with-eval-after-load 'ox-latex
(add-to-list 'org-latex-classes
        '("article"
          "\\documentclass[10pt,article,oneside]{memoir}"
          ("\\chapter{%s}" . "\\chapter*{%s}")
          ("\\section{%s}" . "\\section*{%s}")
          ("\\subsection{%s}" . "\\subsection*{%s}")       
          ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
          ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
        ))
(with-eval-after-load 'ox-latex
   (add-to-list 'org-latex-classes
                '("report"
                  "\\documentclass{report}"
                  ("\\chapter{%s}" . "\\chapter*{%s}")
                  ("\\section{%s}" . "\\section*{%s}")
                  ("\\subsection{%s}" . "\\subsection*{%s}")
                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))

;; find files faster with the recent files packages
;; so i copied this code from masteringemacs.org
;; (require 'recentf)

;; ;; get rid of `find-file-read-only' and replace it with something
;; ;; more useful.
;; (global-set-key (kbd "C-x C-r") 'ido-recentf-open)

;; ;; enable recent files mode.
;; (recentf-mode t)

;; ; 50 files ought to be enough.
;; (setq recentf-max-saved-items 50)

;; (defun ido-recentf-open ()
;;   "Use `ido-completing-read' to \\[find-file] a recent file"
;;   (interactive)
;;   (if (find-file (ido-completing-read "Find recent file: " recentf-list))
;;       (message "Opening file...")
;;     (message "Aborting")))

;;set global highlight line with cursor
;;(global-hl-line-mode nil)


;;enable powerline themes
(powerline-default-theme )

;;thanks to whattheemacsd.com ;; now i can move into buffers more easily
(global-set-key (kbd "C-S-n")
                (lambda ()
                  (interactive)
                  (ignore-errors (next-line 5))))

(global-set-key (kbd "C-S-p")
                (lambda ()
                  (interactive)
                  (ignore-errors (previous-line 5))))

(global-set-key (kbd "C-S-f")
                (lambda ()
                  (interactive)
                  (ignore-errors (forward-char 5))))

(global-set-key (kbd "C-S-b")
                (lambda ()
                  (interactive)
                  (ignore-errors (backward-char 5))))

;; again from whattheemacsd.com moving lines up and down
(defun move-line-down ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines 1))
    (forward-line)
    (move-to-column col)))

(defun move-line-up ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines -1))
    (move-to-column col)))
(global-set-key (kbd "<C-S-up>") 'move-line-up)
(global-set-key (kbd "<C-S-down>") 'move-line-down)

;; going evil on everything rite now
;;(evil-mode 1)

;;starting the server at the time of emacs start
(require 'server)
(unless (server-running-p)
	(server-start))


;; toggle split window
(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
	     (next-win-buffer (window-buffer (next-window)))
	     (this-win-edges (window-edges (selected-window)))
	     (next-win-edges (window-edges (next-window)))
	     (this-win-2nd (not (and (<= (car this-win-edges)
					 (car next-win-edges))
				     (<= (cadr this-win-edges)
					 (cadr next-win-edges)))))
	     (splitter
	      (if (= (car this-win-edges)
		     (car (window-edges (next-window))))
		  'split-window-horizontally
		'split-window-vertically)))
	(delete-other-windows)
	(let ((first-win (selected-window)))
	  (funcall splitter)
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  (if this-win-2nd (other-window 1))))))

(define-key ctl-x-4-map "t" 'toggle-window-split)


;; writing cover letters using emacs org mode and latex
(add-to-list 'load-path "/home/neymar/.emacs.d/koma")
(eval-after-load 'ox '(require 'ox-koma-letter))

;; for org mode latex
(add-to-list 'org-latex-classes
             '("bjmarticle"
               "\\documentclass{article}
\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
\\usepackage{graphicx}
\\usepackage{longtable}
\\usepackage{hyperref}
\\usepackage{natbib}
\\usepackage{amssymb}
\\usepackage{amsmath}
\\usepackage{geometry}
\\geometry{a4paper,left=14.32mm,top=19mm,right=14.32mm,bottom=43mm}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;; For disqus commenting system configuration 
;; (setq site-domain "neymarsabin.me")
;; ;; configure site-baseurl to "/directory/" if the site is inside a subdirectory.  Otherwise set it to  "/"
;; (setq site-baseurl "/html/" )
;; ;; your disqus name
;; ;;(setq disqus-shortname "snarvaez-com-ar-libertad")

;; (defun basename (path)
;;   (file-name-nondirectory (directory-file-name path)))


;; ;; function to simply replace a regular expression in the output
;; (defun my-final-filter(output backend info)
;;   ;; inside org exports,  file variable containts a string with the full path of the output file
;;   (setq page-url  (basename file) )
;;   (setq output  (replace-regexp-in-string  "{{my_site_domain}}" site-domain output ))
;;   (setq output  (replace-regexp-in-string  "{{my_site_baseurl}}" site-baseurl output ))
;;   (setq output  (replace-regexp-in-string  "{{my_page_url}}" page-url  output ))
;;   output
;; )

;; (setq org-export-filter-final-output-functions  '(my-final-filter) )


;; setup ace-window mode
;; dotfile from https://github.com/howardabrams/dot-files/blob/master/emacs.org#jumping-to-windows
(use-package ace-window
  :ensure t
  :init
    (setq aw-keys '(?a ?s ?d ?f ?j ?k ?l ?o))
    (global-set-key (kbd "C-x o") 'ace-window)
  :diminish ace-window-mode)

;; smart-comment features 
(use-package smart-comment
  :bind ("M-;" . smart-comment))

;; can't believe we are still using this awful data format // from howard abrahams
(setq nxml-slash-auto-complete-flag t)

;; tabbar mode
;; thank you for the gist (3demax) https://gist.github.com/3demax/1264635/91ccb6c423effd811dbdb1412b70c15e95fa700d
(require 'tabbar)
;; Tabbar settings
(set-face-attribute
 'tabbar-default nil
 :background "gray20"
 :foreground "gray20"
 :box '(:line-width 1 :color "gray20" :style nil))
(set-face-attribute
 'tabbar-unselected nil
 :background "gray30"
 :foreground "white"
 :box '(:line-width 5 :color "gray30" :style nil))
(set-face-attribute
 'tabbar-selected nil
 :background "gray75"
 :foreground "black"
 :box '(:line-width 5 :color "gray75" :style nil))
(set-face-attribute
 'tabbar-highlight nil
 :background "white"
 :foreground "black"
 :underline nil
 :box '(:line-width 5 :color "white" :style nil))
(set-face-attribute
 'tabbar-button nil
 :box '(:line-width 1 :color "gray20" :style nil))
(set-face-attribute
 'tabbar-separator nil
 :background "gray20"
 :height 0.6)

;; Change padding of the tabs
;; we also need to set separator to avoid overlapping tabs by highlighted tabs
(custom-set-variables
 '(tabbar-separator (quote (0.5))))
;; adding spaces
(defun tabbar-buffer-tab-label (tab)
  "Return a label for TAB.
That is, a string used to represent it on the tab bar."
  (let ((label  (if tabbar--buffer-show-groups
                    (format "[%s]  " (tabbar-tab-tabset tab))
                  (format "%s  " (tabbar-tab-value tab)))))
    ;; Unless the tab bar auto scrolls to keep the selected tab
    ;; visible, shorten the tab label to keep as many tabs as possible
    ;; in the visible area of the tab bar.
(if tabbar-auto-scroll-flag
        label
      (tabbar-shorten
       label (max 1 (/ (window-width)
                       (length (tabbar-view
                                (tabbar-current-tabset)))))))))

;;(tabbar-mode 1)

;; from rhoit dai's conf 
(define-key global-map [(control tab)] 'tabbar-forward)
(define-key global-map [(control next)] 'tabbar-forward)
(define-key global-map [(control prior)] 'tabbar-backward)
(define-key global-map (kbd "C-S-<iso-lefttab>") 'tabbar-backward)

;; enabling python development environment
(elpy-enable)

;; highlight cursor whenever window scrolls
;;(beacon-mode t)

;; some smtp configurations
;; '(send-mail-function (quote smtpmail-send-it))
;; '(smtpmail-smtp-server "smtp.googlemail.com")
;; '(smtpmail-smtp-service 25)

;; gnus configurations 
(setq user-mail-address "reddevil.sabin@gmail.com"
      user-full-name "neymarsabin")

(setq gnus-select-method
      '(nnimap "gmail"
	       (nnimap-address "imap.gmail.com")  ; it could also be imap.googlemail.com if that's your server.
	       (nnimap-server-port "imaps")
	       (nnimap-stream ssl)))

(setq smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

;; neotree modifications and key binding
(global-set-key [f8] 'neotree-toggle)
(setq neo-smart-open t)
(setq projectile-switch-project-action 'neotree-projectile-action)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))
