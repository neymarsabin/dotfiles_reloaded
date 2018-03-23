;;yasnippet loading
(yas-global-mode t)

;;expand region mode
(global-set-key (kbd "C-o") 'er/expand-region)


;;multiple cursors
(require  'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C-{") 'mc/mark-next-like-this)
(global-set-key (kbd "C-}") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-{") 'mc/mark-all-like-this)

;;ERB hooks 
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.erb$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))

;;emmet mode and hooks 
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
(add-hook 'web-mode-hook 'emmet-mode) ;; enable emmet's abbreviation in web mode as well

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
  (sh . t)
  (sqlite . t)
  (js . t)
  (dot . t)))

;; setup ace-window mode
;; dotfile from https://github.com/howardabrams/dot-files/blob/master/emacs.org#jumping-to-windows
(use-package ace-window
  :ensure t
  :init
    (setq aw-keys '(?a ?s ?d ?f ?j ?k ?l ?o))
    (global-set-key (kbd "C-x o") 'ace-window)
  :diminish ace-window-mode)

;; adding some variables settings for web-mode
(custom-set-variables
 '(web-mode-markup-indent-offset 2)
 '(web-mode-css-indent-offset 2)
 '(web-mode-code-indent-offset 2)
 '(web-mode-enable-auto-pairing t)
 '(web-mode-enable-auto-closing t)
 '(web-mode-enable-css-colorization t)
 '(web-mode-commet-style 2)
 '(web-mode-enable-current-column-highlight t)
 '(web-mode-enable-current-element-highlight t))
