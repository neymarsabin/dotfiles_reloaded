;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Sabin Nepal"
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
(setq doom-font (font-spec :family "Monaco" :size 12 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "Monaco" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-zenburn)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


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

;; set go path in PATH env
(require 'lsp-mode)
(setenv "PATH"
        (concat
         "/Users/neymarsabin/go/bin" path-separator
         (getenv "PATH")))

;; tide node executable
(setq tide-node-executable "/Users/neymarsabin/.nvm/versions/node/v18.9.1/bin/node")

;; enable LSP for Javascript
(after! lsp-mode
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "typescript-language-server")
                    :major-modes '(js2-mode typescript-mode web-mode typescript-tsx-mode)
                    :server-id 'ts-ls)))

;; hook lsp mode for js files
;; (after! lsp-mode
;;   (add-to-list 'auto-mode-alist '("\\.tsx" . lsp))
;;   (add-to-list 'auto-mode-alist '("\\.ts" . lsp))
;;   )

;; set org agenda files
(setq org-agenda-files (list "~/org/index.org"))

;; add to list
(add-to-list 'load-path "~/.config/doom/ox-jekyll")

;; my org mode blogging setup
(after! org
  (setq org-jekyll-directory "/Users/neymarsabin/projects/mine/myself/neymarsabin.github.io/neymarsabin.github.io/")
  (setq org-jekyll-use-src-plugin t)
  (setq org-publish-project-alist
        `(("blog"
           :base-directory ,(concat org-jekyll-directory "_org")
           :base-extension "org"
           :publishing-directory ,(concat org-jekyll-directory "_posts")
           :publishing-function org-md-export-to-jekyll
           :recursive t
           :section-numbers nil
           :with-toc nil
           :html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"./css/styles.css\" />")
          ("static"
           :base-directory ,(concat org-jekyll-directory "_org")
           :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
           :publishing-directory ,(concat org-jekyll-directory "")
           :publishing-function org-publish-attachment)))
  (setq org-jekyll-preview-time 5))
(add-hook! 'org-mode-hook 'org-jekyll-enable)

;; set org agenda list
(setq org-agenda-span 20)

;; lsp mode setup for golang
(add-hook 'go-mode-hook #'lsp-deferred)
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

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


;; org mode designs
;; (set-face-attribute 'org-level-1 nil :height 1.0 :background nil)
