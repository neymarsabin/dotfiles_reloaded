;; from rhoit dai's config highlight-symbol
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
