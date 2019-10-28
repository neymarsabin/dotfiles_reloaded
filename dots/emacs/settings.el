;; personal window settings 
(when (window-system)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1))
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

;;default tab width
(setq-default tab-width 2)

;; expand region mode
(global-set-key (kbd "C-o") 'er/expand-region)

;; auto indentation
(electric-indent-mode t)

;;select the current line as a whole 
;; (defun select-current-line ()
;;	"Selects the current line"
;;	(interactive)
;;	(end-of-line)
;;	(push-mark (line-beginning-position) nil t))
;; (define-key my-keys-minor-mode-map (kbd "M-i") ;;  'select-current-line)

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

;;basic key bindings
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-c C-k") 'compile)
(global-set-key (kbd "C-x g") 'magit-status)

;;reload emacs without closing 
(defun reload-user-init-file()
	(interactive)
	(load-file user-init-file))

(global-set-key (kbd "C-M-]")
                (lambda ()
                  (interactive)
                  (ignore-errors (next-line 5))))

(global-set-key (kbd "C-M-[")
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

;; which key mode
;; (which-key-mode)
