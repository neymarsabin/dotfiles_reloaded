;;; ox-jekyll.el --- Jekyll Markdown Back-End for Org Export Engine

;; Copyright (C) 2014 Lars Tveito

;; Author: Mickael Kerjean
;; Keywords: org, jekyll, markdown, github

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This library implements a Jekyll back-end (with github markdown flavor and front matter) for Org
;; exporter, based on the `md' back-end.
;; This backend was adapt and extend from the work already done on the gfm backend

;;; Code:

(require 'ox-md)
(require 'ox-html)
(require 'ox-publish)

;;; User-Configurable Variables

(defgroup org-export-jekyll nil
  "Options specific to Markdown export back-end."
  :tag "Jekyll Markdown with Front Matter"
  :group 'org-export
  :version "24.4"
  :package-version '(Org . "8.0"))

(defcustom org-jekyll-default-layout "layout"
  "The default layout"
  :group 'org-export-jekyll)

(defvar jekyll-date-export nil)
(defvar jekyll-default-layout "post")


;;; Define Back-End

(org-export-define-derived-backend 'jekyll 'md
  :filters-alist '((:filter-parse-tree . org-md-separate-elements))
  :options-alist '((:layout "LAYOUT" nil jekyll-default-layout t)
                   (:image "IMAGE" nil nil t)
                   (:title "TITLE" nil nil t)
                   (:tags "TAGS" nil nil t)
                   (:published "PUBLISHED" nil nil t)
                   (:category "CATEGORY" nil nil t)
                   (:categories "CATEGORIES" nil nil t)
                   (:date "DATE" nil nil t))
  :menu-entry
  '(?j "Export to Jekyll"
       ((?J "as Jekyll buffer" org-md-export-as-jekyll)
        (?j "as Jekyll file" org-md-export-to-jekyll)
        (?o "as Jekyll file and open"
            (lambda (a s v b)
              (if a (org-md-export-to-jekyll t s v)
                (org-open-file (org-md-export-to-jekyll nil s v)))))))
  :translate-alist '((paragraph . org-jekyll-paragraph)
                     (keyword . org-jekyll-keyword)
                     (strike-through . org-jekyll-strike-through)
                     (src-block . org-jekyll-src-block)
                     (link . org-jekyll-link)
                     (inner-template . org-jekyll-template)
                     (headline . org-jekyll-headline)
                     (table-cell . org-jekyll-table-cell)
                     (timestamp . org-jekyll-date)
                     (table-row . org-jekyll-table-row)
                     (table . org-jekyll-table)))


;;; Transcode Functions
(defun org-jekyll-template (contents info)
  (let (
        (layout (format "layout: %s\n" (org-export-data (plist-get info :layout) info)))
        (title (org-jekyll--format "title: %s\n" (org-export-data (plist-get info :title) info)))
        (date (org-jekyll--format "date: %s\n" (org-export-data (org-export-get-date info) info)))
        (image (org-jekyll--format "image: %s\n" (org-export-data (plist-get info :image) info)))
        (published (org-jekyll--format "published: %s\n" (org-export-data (plist-get info :published) info)))
        (tags (org-jekyll--format "tags: %s\n" (org-export-data (plist-get info :tags) info)))
        (categories (org-jekyll--format "categories: %s\n" (org-export-data (plist-get info :categories) info)))
        (category (org-jekyll--format "category: %s\n" (org-export-data (plist-get info :category) info))))
    (concat "---\n" layout title date image published tags category categories "---\n\n" contents)))


(defun org-jekyll--format (my-format value)
  "private function formatting a value if the value exists"
  (if (eq (length value) 0) nil (format my-format value)))


(defun org-jekyll-generate-header (contents info)
  "Return the Front Matter string.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (let* ((layout ((lambda (val) (val)) (plist-get info :jekyll-layout))))
    (concat "---\n" layout "---\n\n")))

(defun org-jekyll-headline (headline contents info)
  (let* ((new-info (plist-put info :with-toc nil)))
    (org-md-headline headline contents new-info)))

;;;; Links
(defun org-jekyll-link (link contents info)
  (if (and
       (string= "file" (org-element-property :type link))
       (not (org-export-inline-image-p link org-html-inline-image-rules)))
      (format "[%s]({%% post_url %s %%})" contents (file-name-base (org-element-property :path link)))
      (org-export-data-with-backend link 'md info)))


;;;; Timestamp
(defun org-jekyll-date (timestamp contents info)
  (format-time-string "%Y-%m-%d" (org-time-string-to-time (org-timestamp-translate timestamp))))

;;;; Keyword
(defun org-jekyll-keyword (keyword contents info)
  "Transcode a KEYWORD element from Org to Jekyll.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((key (org-element-property :key keyword))
        (value (org-element-property :value keyword)))
    (cond
     ((string= key "EXCERPT") ((lambda(val) (if (eq (length val) 0) "<!--more-->" val)) value))
     ((string= key "AMP_YOUTUBE") (format "<amp-youtube data-videoid=\"%s\" %s data-param-rel=\"0\" data-param-showinfo=\"0\" layout=\"responsive\" width=\"480\" height=\"270\"></amp-youtube>"
                                          (replace-regexp-in-string ",.*" "" value)
                                          (if (string-match "start=" value) (format "data-param-start=\"%s\"" (replace-regexp-in-string ".*start=" "" value)) "")
                                          ))
     ((string= key "AMP_IMG") (format "<amp-img src=\"%s\" width=\"%s\" height=\"%s\" layout="responsive"></amp-img>"
                                      (replace-regexp-in-string ",.*" "" value)
                                      (replace-regexp-in-string "x.*" "" (replace-regexp-in-string ".*size=" "" value))
                                      (replace-regexp-in-string ".*x" "" (replace-regexp-in-string ".*size=" "" value))))
     )))


;;;; Paragraph

(defun org-jekyll-paragraph (paragraph contents info)
  "Transcode PARAGRAPH element into Github Flavoured Markdown format.
CONTENTS is the paragraph contents.  INFO is a plist used as a
communication channel."
  (unless (plist-get info :preserve-breaks)
    (setq contents (concat (mapconcat 'identity (split-string contents) " ") "\n")))
  (let ((first-object (car (org-element-contents paragraph))))
    ;; If paragraph starts with a #, protect it.
    (if (and (stringp first-object) (string-match "\\`#" first-object))
        (replace-regexp-in-string "\\`#" "\\#" contents nil t)
      contents)))


;;;; Src Block

(defun org-jekyll-src-block (src-block contents info)
  "Transcode SRC-BLOCK element into Github Flavored Markdown
format. CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (let* ((lang (org-element-property :language src-block))
         (code (org-export-format-code-default src-block info))
         (prefix (concat "```" lang "\n"))
         (suffix "```"))
    (concat prefix code suffix)))


;;;; Strike-Through

(defun org-jekyll-strike-through (strike-through contents info)
  "Transcode STRIKE-THROUGH from Org to Markdown (JEKYLL).
CONTENTS is the text with strike-through markup.  INFO is a plist
holding contextual information."
  (format "~~%s~~" contents))


;;;; Table-Common

(defvar width-cookies nil)
(defvar width-cookies-table nil)

(defconst jekyll-table-left-border "|")
(defconst jekyll-table-right-border " |")
(defconst jekyll-table-separator " |")

(defun org-jekyll-table-col-width (table column info)
  "Return width of TABLE at given COLUMN. INFO is a plist used as
communication channel. Width of a column is determined either by
inquerying `width-cookies' in the column, or by the maximum cell with in
the column."
  (let ((cookie (when (hash-table-p width-cookies)
                  (gethash column width-cookies))))
    (if (and (eq table width-cookies-table)
             (not (eq nil cookie)))
        cookie
      (progn
        (unless (and (eq table width-cookies-table)
                     (hash-table-p width-cookies))
          (setq width-cookies (make-hash-table))
          (setq width-cookies-table table))
        (let ((max-width 0)
              (specialp (org-export-table-has-special-column-p table)))
          (org-element-map
              table
              'table-row
            (lambda (row)
              (setq max-width
                    (max (length
                          (org-export-data
                           (org-element-contents
                            (elt (if specialp (car (org-element-contents row))
                                   (org-element-contents row))
                                 column))
                           info))
                         max-width)))
            info)
          (puthash column max-width width-cookies))))))


(defun org-jekyll-make-hline-builder (table info char)
  "Return a function to build horizontal line in TABLE with given
CHAR. INFO is a plist used as a communication channel."
  `(lambda (col)
     (let ((max-width (max 3 (org-jekyll-table-col-width table col info))))
       (when (< max-width 1)
         (setq max-width 1))
       (make-string max-width ,char))))


;;;; Table-Cell

(defun org-jekyll-table-cell (table-cell contents info)
  "Transcode TABLE-CELL element from Org into JEKYLL. CONTENTS is content
of the cell. INFO is a plist used as a communication channel."
  (let* ((table (org-export-get-parent-table table-cell))
         (column (cdr (org-export-table-cell-address table-cell info)))
         (width (org-jekyll-table-col-width table column info))
         (left-border (if (org-export-table-cell-starts-colgroup-p table-cell info) "| " " "))
         (right-border " |")
         (data (or contents "")))
    (setq contents
          (concat data
                  (make-string (max 0 (- width (string-width data)))
                               ?\s)))
    (concat left-border contents right-border)))


;;;; Table-Row

(defun org-jekyll-table-row (table-row contents info)
  "Transcode TABLE-ROW element from Org into JEKYLL. CONTENTS is cell
contents of TABLE-ROW. INFO is a plist used as a communication
channel."
  (let ((table (org-export-get-parent-table table-row)))
    (when (and (eq 'rule (org-element-property :type table-row))
               ;; In JEKYLL, rule is valid only at second row.
               (eq 1 (cl-position
                      table-row
                      (org-element-map table 'table-row 'identity info))))
      (let* ((table (org-export-get-parent-table table-row))
             (header-p (org-export-table-row-starts-header-p table-row info))
             (build-rule (org-jekyll-make-hline-builder table info ?-))
             (cols (cdr (org-export-table-dimensions table info))))
        (setq contents
              (concat jekyll-table-left-border
                      (mapconcat (lambda (col) (funcall build-rule col))
                                 (number-sequence 0 (- cols 1))
                                 jekyll-table-separator)
                      jekyll-table-right-border))))
    contents))



;;;; Table

(defun org-jekyll-table (table contents info)
  "Transcode TABLE element into Github Flavored Markdown table.
CONTENTS is the contents of the table. INFO is a plist holding
contextual information."
  (let* ((rows (org-element-map table 'table-row 'identity info))
         (no-header (or (<= (length rows) 1)))
         (cols (cdr (org-export-table-dimensions table info)))
         (build-dummy-header
          (function
           (lambda ()
             (let ((build-empty-cell (org-jekyll-make-hline-builder table info ?\s))
                   (build-rule (org-jekyll-make-hline-builder table info ?-))
                   (columns (number-sequence 0 (- cols 1))))
               (concat jekyll-table-left-border
                       (mapconcat (lambda (col) (funcall build-empty-cell col))
                                  columns
                                  jekyll-table-separator)
                       jekyll-table-right-border "\n" jekyll-table-left-border
                       (mapconcat (lambda (col) (funcall build-rule col))
                                  columns
                                  jekyll-table-separator)
                       jekyll-table-right-border "\n"))))))
    (concat (when no-header (funcall build-dummy-header))
            (replace-regexp-in-string "\n\n" "\n" contents))))



;; (defun org-jekyll-filename (old-function &rest arguments)
;;   (error "advising in")
;;   (let ((filename (apply old-function arguments)))
;;     (concat (file-name-directory filename)
;;             jekyll-date-export "-"
;;             (file-name-nondirectory filename))))


;;; Interactive function

;;;###autoload
(defun org-md-export-as-jekyll (&optional async subtreep visible-only body-only ext-plist)
  (interactive)
  (org-export-to-buffer 'jekyll "*Org JEKYLL Export*"
    async subtreep visible-only nil nil (lambda () (text-mode))))


;;;###autoload
(defun org-md-convert-region-to-jekyll ()
  (interactive)
  (org-export-replace-region-by 'jekyll))


;;;###autoload
(defun org-md-export-to-jekyll (&optional async subtreep visible-only body-only ext-plist)
  (interactive)
  (let ((outfile (org-export-output-file-name ".md" subtreep)))
    (org-export-to-file 'jekyll outfile async subtreep visible-only)))

;;;###autoload
(defun org-md-publish-to-jekyll (plist filename pub-dir)
  (org-publish-org-to 'jekyll filename ".md" plist pub-dir))


(provide 'ox-jekyll)
