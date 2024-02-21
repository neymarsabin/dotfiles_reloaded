;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "neymarsabin"
      user-mail-address "reddevil.sabin@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
(setq doom-font (font-spec :family "Monaco" :size 12 :weight 'normal)
      doom-variable-pitch-font (font-spec :family "Monaco" :size 12))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme ')

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/projects/mine/myself/the-new-org/doom/capture")
(setq org-journal-dir "~/projects/mine/myself/the-new-org/doom/capture")
(setq org-hugo-base-dir "~/projects/mine/portfolio")

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; enable LSP for Javascript
;; (after! lsp-mode
;;   (lsp-register-client
;;    (make-lsp-client :new-connection (lsp-stdio-connection "typescript-language-server --stdio")
;;                     :major-modes '(js2-mode typescript-mode web-mode typescript-tsx-mode)
;;                     :server-id 'ts-ls)))

;; set org agenda files
(setq org-agenda-files (list "~/projects/mine/myself/the-new-org/doom/capture"))

;; set org agenda list
(setq org-agenda-span 7)

;; lsp mode setup for golang
;; (add-hook 'go-mode-hook #'lsp-deferred)
;; (defun lsp-go-install-save-hooks ()
;;   (add-hook 'before-save-hook #'lsp-format-buffer t t)
;;   (add-hook 'before-save-hook #'lsp-organize-imports t t))
;; (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;; disable title bar
(menu-bar-mode -1)
;; (add-to-list 'default-frame-alist '(undecorated . t))

;; duplicate current line
(defun duplicate-line ()
  (interactive)
  (save-mark-and-excursion
    (beginning-of-line)
    (insert (thing-at-point 'line t))))

(global-set-key (kbd "C-S-d") 'duplicate-line)

;; move a line up or down
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
    (forward-line -1)
    (move-to-column col)))

(global-set-key (kbd "C-S-j") 'move-line-down)
(global-set-key (kbd "C-S-k") 'move-line-up)

;; maximize the window upon startup
(setq initial-frame-alist '((top . 1) (left . 1) (width . 114) (height . 32)))

;; enable variable and visual line mode in Org mode by default
(add-hook! org-mode :append
           #'visual-line-mode
           #'variable-pitch-mode)

;; org-capture configurations
(defun zz/add-file-keybinding (key file &optional desc)
  (let ((key key)
        (file file)
        (desc desc))
    (map! :desc (or desc file)
          key
          (lambda () (interactive) (find-file file)))))

(zz/add-file-keybinding "C-c z i" "~/projects/mine/myself/the-new-org/doom/capture/ideas.org" "ideas.org")

;; disable global highlight line mode, irritating when using visual mode
;; TODO: maybe best to uninstall the whole package hl-line-mode
;; also for some reason, the config below does not work in Elisp files
(setq hl-line-mode nil)

;; wrap a word or region with something
;; TODO: find some shortcuts to use these
(defun insert-curves (&optional arg)
  "Inserts {  } curves to the selected region.
I wish I could say I wrote this function, copies everything of the function insert-parenthesis.
Rely on your LSP for indentation, couldn't write a single thing on indenting."
  (interactive "P")
  (insert-pair arg ?\{ ?\}))
(global-set-key (kbd "M-S-{") 'insert-curves)

;; (defun insert-bigboots (&optional arg)
;;   "Inserts [ ] curves to the selected region.
;; Same here :D I wish I could say I wrote this function, copies everything of the function insert-parenthesis.
;; Rely on your LSP for indentation, couldn't write a single thing on indenting."
;;   (interactive "P")
;;   (insert-pair arg ?\[ ?\]))
;; ;; (global-set-key (kbd "M-S-^") 'insert-bigboots)

;; ChatGPT Configuration
(defun get-openai-key-from-env ()
  (getenv "OPENAI_API_KEY"))
(setq chatgpt-shell-openai-key (get-openai-key-from-env))

;; Elfeed RSS list
(setq elfeed-feeds
      '("https://alexwlchan.net/atom.xml"
        "https://news.ycombinator.com/rss"
        "http://feeds.feedburner.com/AlexSexton"))

;; open my .zshrc file when I press these key strokes
;; use evil-define-key from evil mode || docs link: https://evil.readthedocs.io/en/latest/keymaps.html#leader-keys
(defun zshrc()
  (interactive)
  (find-file "~/.zshrc")
  (message "echo:: zshrc config file"))
(evil-define-key 'normal 'global (kbd "SPC fz") 'zshrc)

;; terraform lsp has issues with emacs30, does not allow other lsp's to work
;; found this fix from the issue mentioned below
(after! lsp-mode
  ;; https://github.com/emacs-lsp/lsp-mode/issues/3577#issuecomment-1709232622
  (delete 'lsp-terraform lsp-client-packages))

;; change cursor type in insert mode in terminal
(add-hook 'evil-insert-state-entry-hook (lambda () (send-string-to-terminal "\033[5 q")))
(add-hook 'evil-insert-state-exit-hook  (lambda () (send-string-to-terminal "\033[2 q")))

;; add new custom org-capture templates for readme
(setq org-capture-templates
      '(("r" "Readme" entry (file+headline "~/.org/notes.org" "Readme")
         "* TODO %?\n  %i\n DEADLINE: %t \n - [ ] url: \n %a")))

;; xclip will allow us to copy paste from emacs to outside
(xclip-mode 1)
