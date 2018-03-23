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
