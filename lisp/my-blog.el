(defalias 'my-complete-blog-post-url #'my-org-blog-complete)

;;;###autoload
(defun my-blog-posts (&optional start end filter-fn)
  (let ((json-object-type 'alist)
        (data (json-read-file "~/proj/static-blog/_site/blog/all/index.json")))
    (if (or start end)
        (seq-filter
         (lambda (o)
           (and (or (null start) (not (string< (alist-get 'date o) start)))
                (or (null end) (string< (alist-get 'date o) end))
                (or (null filter-fn) (funcall filter-fn o))))
         data)
      data)))

;;;###autoload
(defun my-blog-edit-html (url)
  (interactive (list (my-org-blog-complete)))
  (let ((base (replace-regexp-in-string
               (concat "^" (regexp-quote my-blog-base-url))
               ""
               url))
        (filename (my-11ty-html-filename url)))
    (if filename
        (find-file filename)
      (error "Could not find file."))))

;;;###autoload
(defun my-blog-edit-json (url)
  (interactive (list (my-org-blog-complete)))
  (let* ((filename (my-11ty-html-filename url))
         (json (and filename (concat (file-name-sans-extension filename) ".11tydata.json"))))
    (if json
        (find-file json)
      (error "Could not find file."))))

;;;###autoload
(defun my-view-blog-post-locally (url)
  (interactive (list (my-org-blog-complete)))
  (browse-url
   (replace-regexp-in-string
    (concat "^" (regexp-quote my-blog-base-url))
    "https://localhost:8080/"
    url)))

;;;###autoload
(defun my-insert-blog-post-url (url)
  (interactive (list (my-complete-blog-post-url)))
  (insert url))

;;;###autoload
(defun my-blog-title (url)
  (let ((base (replace-regexp-in-string
               (concat "^" (regexp-quote my-blog-base-url))
               "/"
               url)))
    (alist-get 'title
               (seq-find (lambda (o)
                           (string= (alist-get 'permalink o) base))
                         (my-blog-posts)))))

;;;###autoload
(defun my-insert-blog-post-link (url)
  (interactive (list (my-complete-blog-post-url)))
  (if (derived-mode-p 'org-mode)
      (insert (org-link-make-string
               url
               (my-blog-title url)))
    (insert url)))

;;;###autoload
(defun my-11ty-list-all-matching-blog-posts (match)
	(interactive "MMatch: ")
	(mapc (lambda (o)
					(when (or (string-match match (alist-get 'title o))
										(member match (alist-get 'categories o)))
						(insert "- "
										(org-link-make-string
										 (concat my-blog-base-url (alist-get 'permalink o))
										 (alist-get 'title o))
										"\n")))
				(my-blog-posts)))

;;;###autoload
(defun my-11ty-complete-blog-post ()
  (completing-read
   "Post: "
   (mapcar (lambda (o)
	     (file-name-directory (file-relative-name o my-11ty-base-dir)))
	   (directory-files-recursively (expand-file-name "blog" my-11ty-base-dir) "index\\.html" nil))))
;;;###autoload
(defun my-11ty-ripgrep ()
  (interactive)
  (consult-ripgrep (expand-file-name "blog" my-11ty-base-dir)))

;;;###autoload
(defun my-11ty-post-categories (file)
  (assoc-default 'categories
		 (let ((json-object-type 'alist)
		       (json-array-type 'list))
		   (json-read-file (my-11ty-json-filename file)))))

;;;###autoload
(defun my-11ty-add-category-tag (cat)
  (interactive (list (my-11ty-complete-category "Category: ")))
  (org-set-tags (cons cat (org-get-tags))))

;;;###autoload
(defun my-11ty-complete-category (prompt &optional categories)
  (let ((all-categories
	 (json-read-file
	  (expand-file-name "siteCategories.json"
			    (expand-file-name "_data" my-11ty-base-dir)))))
    (completing-read
     (if categories
	 (format  "%s(current: %s) "
		  prompt
		  (string-join categories ", "))
       prompt)
     (mapcar (lambda (c) (assoc-default 'slug c))
	     all-categories))))

;;;###autoload
(defun my-11ty-json-filename (path)
  (concat (file-name-sans-extension (my-11ty-html-filename path)) ".11tydata.json"))

;;;###autoload
(defun my-11ty-change-details (file modify-func)
  (let* ((json-object-type 'alist)
	 (json-array-type 'list)
	 (json-file (my-11ty-json-filename file))
	 (json (funcall modify-func (json-read-file json-file))))
    (when json
      (with-temp-file json-file
	(insert (json-encode json)))
      json-file)))

;;;###autoload
(defun my-11ty-add-category-to-post (file new-category)
  (interactive (list (buffer-file-name)
		     (my-11ty-complete-category "Add category: "
						(my-11ty-post-categories file))))
  (my-11ty-change-details
   file
   (lambda (json)
     (let ((categories (assoc-default 'categories json)))
       (if categories
	   (unless (member new-category categories)
	     (setcdr (assoc 'categories json)
		     (cons new-category categories)))
	 (setq json (cons (cons 'categories (cons new-category categories)) json)))
       json))))

;;;###autoload
(defun my-11ty-add-category-to-all-posts-in-region (category beg end)
	(interactive (list (my-11ty-complete-category "Category: ")
										 (min (point) (mark))
										 (max (point) (mark))))
	(goto-char beg)
	(while (re-search-forward org-link-bracket-re end t)
		(my-11ty-add-category-to-post
		 (my-11ty-html-filename (org-element-property :raw-link (org-element-context)))
		 category)))

(defvar my-11ty-base-dir "~/proj/static-blog/")

;;;###autoload
(defun my-blog-edit-org (info)
	(interactive (list (my-consult-blog-posts-by-title)))
  (unless (listp info) (setq info (my-blog-post-info-for-url info)))
  (org-goto-marker-or-bmk (my-blog-post--position info)))

;;;###autoload
(defun my-blog-find-html (url)
  "Go to the HTML file for URL."
	(interactive (list (my-complete-blog-post-url)))
  (setq url (my-org-link-as-url url))
	(when (string-match "https://sachachua\\.com/\\(blog/.*\\)" url)
    (find-file
     (expand-file-name
      "index.html"
      (expand-file-name (match-string 1 url)
                        my-11ty-base-dir)))))

;;;###autoload
(defalias 'my-blog-find-org #'my-blog-edit-org)


;;;###autoload
(defun my-blog-org-files-except-reviews (after-date)
  "Return a list of recent .org files except for Emacs News and weekly/monthly/yearly reviews.
AFTER-DATE is in the form yyyy, yyyy-mm, or yyyy-mm-dd."
  (setq after-date (or after-date "2020"))
  (let ((after-month (substring after-date 0 7))
        (posts (my-blog-posts)))
    (seq-keep
     (lambda (filename)
       (when (not (string-match "[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]-emacs-news" filename))
         (when (string-match "/blog/\\([0-9]+\\)/\\([0-9]+\\)/" filename)
           (let ((month (match-string 2 filename))
                 (year (match-string 1 filename)))
             (unless (string> after-month
                              (concat year "-" month))
               (let ((info (my-blog-post-info-for-url (replace-regexp-in-string "~/proj/static-blog\\|index\\.org$\\|\\.org$" "" filename) posts)))
                 (let-alist info

                   (when (and
                          info
                          (string> .date after-date)
                          (not (seq-intersection .categories
                                                 '("emacs-news" "weekly" "monthly" "yearly")
                                                 'string=)))
                     filename))))))))
     (sort
      (directory-files-recursively "~/proj/static-blog/blog" "\\.org$")
      :lessp #'string<
      :reverse t))))

;;;###autoload
(defun my-11ty-add-blog-comment (new-comment url)
	"Add COMMENT to URL.
COMMENT should be an alist with author, date (ISO8901 format), and message (HTML)."
	(let* ((filename (my-11ty-comment-file url))
				 (comments (my-11ty-comments url))
				 (comment-list (alist-get 'comments (alist-get 'disqus comments)))
				 (existing (and
										(alist-get 'postId new-comment)
										(seq-find (lambda (o)
																(string= (alist-get 'postId o)
																				 (alist-get 'postId new-comment)))
															comment-list))))
		(cond
		 (existing
			;; I think this is how you replace
			(setcar (member existing comment-list)
							new-comment))
		 (comment-list
			(push new-comment (alist-get 'comments (alist-get 'disqus comments)))
			(cl-incf (alist-get 'commentCount (alist-get 'disqus comments))))
		 (t
			(map-put! (alist-get 'disqus comments)
								'comments
								(list new-comment))
			(cl-incf (alist-get 'commentCount (alist-get 'disqus comments)))))
		(with-temp-file filename
			(insert
			 (json-encode comments))
			(json-pretty-print (point-min) (point-max)))
		filename))

;;;###autoload
(defun my-11ty-comment-file (url)
	(interactive (list (my-complete-blog-post-url)))
	(let* ((permalink (replace-regexp-in-string "^https?://[^/]+" "" url))
				 (post (seq-find (lambda (o)
														 (string= (alist-get 'permalink o)
																			permalink))
													 (json-read-file
														(expand-file-name "_site/blog/all/index.json" my-11ty-base-dir))))
				 (filename
					(if post
							(expand-file-name
							 (concat
								(file-name-sans-extension
								 (alist-get 'inputPath
														post
														))
								".json")
							 my-11ty-base-dir)
						(error "Could not find %s" permalink))))
		(when (called-interactively-p 'any)
			(find-file filename))
		filename))
;; (my-11ty-comment-file "https://sachachua.com/blog/2021/01/a-list-of-sharks-that-are-obligate-ram-ventilators/")

;;;###autoload
(defun my-11ty-comments (url)
  (let ((filename (my-11ty-comment-file url))
        (json-array-type 'list)
        (json-object-type 'alist))
    (if (file-exists-p filename)
        (json-read-file filename)
      `((disqus
         (path . ,(replace-regexp-in-string "^https?://[^/]+" "" url))
         (commentCount . 0)
         (comments . nil))))))

;;(let-alist (my-11ty-comments "/blog/2021/01/a-list-of-sharks-that-are-obligate-ram-ventilators/") .disqus)

;;;###autoload
(defun my-org-11ty-copy-just-this-post (&optional url)
	(interactive)
	(cond
   (url
    (let* ((relative-path (replace-regexp-in-string
                           (concat "^" (regexp-quote my-blog-base-url) "\\|^/")
                           ""
                           url))
           (local (expand-file-name relative-path (expand-file-name "_local" my-11ty-base-dir)))
           (remote (concat "web:/var/www/static-blog/" relative-path))
           (remote-tramp (concat "/ssh:" remote)))
      (if (file-directory-p local)
          (progn
            (call-process "chmod" nil nil nil "ugo+rX" "-R" local)
            (unless (file-directory-p (file-name-directory remote-tramp))
              (make-directory (file-name-directory remote-tramp) t))
            (call-process "rsync" nil (get-buffer-create "*rsync*") nil "--chmod=ugo=rX" "-avzpe" "ssh"
                          local
                          remote)
            (if (string-match "^https://" url)
                (browse-url url)
              (browse-url (concat my-blog-base-url url))))
        (error "Could not find %s" local))))
	 ((derived-mode-p 'org-mode)
		(let* ((subtreep (not (org-before-first-heading-p)))
					 (params (org-combine-plists
										(org-export--get-export-attributes '11ty subtreep nil)
										(org-export--get-buffer-attributes)
										(org-export-get-environment '11ty subtreep)))
					 (file (plist-get params :file-name))
					 (permalink (plist-get params :permalink))
					 (local (expand-file-name file (expand-file-name "_local" (plist-get params :base-dir))))
					 (remote (concat "web:/var/www/static-blog/" file))
					 (remote-tramp (concat "/ssh:" remote)))
			(if (and permalink file (file-directory-p local))
					(progn
						(call-process "chmod" nil nil nil "ugo+rX" "-R" local)
						(unless (file-directory-p (file-name-directory remote-tramp))
							(make-directory (file-name-directory remote-tramp) t))
						(call-process "rsync" nil (get-buffer-create "*rsync*") nil "--chmod=ugo=rX" "-avzpe" "ssh"
													local
													remote)
						(browse-url (concat (replace-regexp-in-string "/$" "" my-blog-base-url)
																permalink)))
				(error "Could not find %s" local))))
	 ((or (derived-mode-p 'html-mode)
        (derived-mode-p 'web-mode))
		(let* ((json-object-type 'alist)
					 (permalink
						(alist-get 'permalink (json-read-file (concat (file-name-sans-extension (buffer-file-name)) ".11tydata.json"))))
					 (local (expand-file-name (concat "." permalink) (expand-file-name "_local" my-11ty-base-dir)))
					 (remote (concat "web:/var/www/static-blog" permalink)))
			(call-process "rsync" nil (get-buffer-create "*rsync*") nil "--chmod=ugo=rX" "-avzpe" "ssh"
										local
										remote)
			(browse-url (concat (replace-regexp-in-string "/$" "" my-blog-base-url)
													permalink))))))

;;;###autoload
(defun my-11ty-post-plist ()
	(cond
	 ((derived-mode-p 'org-mode)
		(let* ((subtreep (not (org-before-first-heading-p)))
					 (combined (org-combine-plists
											(org-export--get-export-attributes '11ty subtreep nil)
											(org-export--get-buffer-attributes)
											(org-export-get-environment '11ty subtreep))))
			(when (listp (plist-get combined :title))
				(plist-put combined :title (car (plist-get combined :title))))
			(plist-put combined :tags (org-get-tags))
			combined))
	 ((or (derived-mode-p 'js-mode)
				(derived-mode-p 'jsonian-mode)) ; probably looking at .11tydata.json
		(json-parse-string (buffer-string)
											 :object-type 'plist
											 :array-type 'list))
	 ((and (derived-mode-p 'html-mode)
				 (file-exists-p (concat (file-name-base (buffer-file-name)) ".11tydata.json")))
		(let ((json-object-type 'plist)
					(json-array-type 'list))
			(json-read-file
			 (concat (file-name-base (buffer-file-name)) ".11tydata.json"))))
	 (t (error "Could not find info."))))

;;;###autoload
(defun my-11ty-permalink ()
	"Get permalink for current post."
	(plist-get (my-11ty-post-plist) :permalink))

;;;###autoload
(defun my-11ty-tags ()
	"Get tags for current post."
	(plist-get (my-11ty-post-plist) :tags))

;;;###autoload
(defun my-11ty-post-url ()
  (when (org-entry-get-with-inheritance "EXPORT_ELEVENTY_PERMALINK")
    (concat (replace-regexp-in-string "/$" "" my-blog-base-url)
            (org-entry-get-with-inheritance "EXPORT_ELEVENTY_PERMALINK"))))

;;;###autoload
(defun my-11ty-post-text ()
	(save-excursion
		(goto-char
     (cond
      ((org-entry-get-with-inheritance "EXPORT_ELEVENTY_PERMALINK")
       (org-find-property "EXPORT_ELEVENTY_PERMALINK"
												  (org-entry-get-with-inheritance "EXPORT_ELEVENTY_PERMALINK")))
      (t (org-back-to-heading-or-point-min))))
    (if (org-before-first-heading-p)
        (buffer-string)
		  (org-end-of-meta-data)
		  (buffer-substring (point) (org-end-of-subtree)))))

;;;###autoload
(defun my-strip-blog-share ()
  (interactive)
  (let (base)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              "<div class=\"sharedaddy sd-sharing-enabled\">.*?<div class=\"sharing-clear\"></div></div></div></div>" nil t)
        (replace-match "")))))
