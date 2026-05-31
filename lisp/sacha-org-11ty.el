;;; sacha-org-11ty.el ---  -*- lexical-binding: t -*-

;; Author: Sacha Chua <sacha@sachachua.com>
;; URL: https://sachachua.com/dotemacs

;;; License:
;;
;; This file is not part of GNU Emacs.
;;
;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Related Emacs config sections:
;;
;; - 11ty static site generation
;;   https://sachachua.com/dotemacs#11ty
;;
;; - Moving my Org post subtree to the 11ty directory
;;   https://sachachua.com/dotemacs#moving-sacha-org-post-subtree-to-the-11ty-directory
;;
;;; Code:



;; [[file:../Sacha.org::#11ty][11ty static site generation:2]]
(defvar sacha-org-11ty-serve-process nil)

;;;###autoload
(defun sacha-org-11ty-rewrite-tags (info)
	"Turn OneWordTags into one-word-tags."
	(require 's)
	(dolist (field '(:categories :tags))
		(when (plist-get info field)
			(plist-put info field
								 (mapcar (lambda (s)
													 (if (string-match "^_" s)
															 s
														 (s-dashed-words s)))
												 (plist-get info field)))))
	info)

(defvar sacha-11ty-remote-dir "/ssh:web:/var/www/static-blog")
(defvar sacha-11ty-base-dir "~/proj/static-blog")
(defvar sacha-11ty-site-dir (expand-file-name "_site" sacha-11ty-base-dir))
(defvar sacha-11ty-local-dir (expand-file-name "_local" sacha-11ty-base-dir))

;;;###autoload
(defun sacha-org-11ty-unpublish-file (file-name)
  "Remove file from local exports and web server."
  (interactive (list (read-file-name "File: ")))
	(when-let* ((base-dir
							 (or
								(and (not (org-before-first-heading-p))
										 (org-entry-get-with-inheritance "EXPORT_ELEVENTY_FILE_NAME"))
								(cdr
								 (assoc-string
									"ELEVENTY_FILE_NAME"
									(sacha-org-keywords))))))
		(dolist (dir (list sacha-11ty-site-dir sacha-11ty-local-dir sacha-11ty-remote-dir sacha-11ty-base-dir))
			(condition-case nil
					(when (and dir (file-directory-p (expand-file-name base-dir dir))
										 (file-exists-p (expand-file-name (file-name-nondirectory file-name)
																											(expand-file-name base-dir dir))))
						(delete-file (expand-file-name (file-name-nondirectory file-name)
																					 (expand-file-name base-dir dir))))
				(error nil)))))

;;;###autoload
(defun sacha-org-11ty-unpublish-current-post ()
	(interactive)
	(cond
	 ((derived-mode-p 'org-mode)
		(when (org-entry-get (point) "EXPORT_ELEVENTY_FILE_NAME")
			(let ((filename (org-entry-get (point) "EXPORT_ELEVENTY_FILE_NAME")))
				(dolist (dir (list sacha-11ty-site-dir sacha-11ty-local-dir sacha-11ty-remote-dir sacha-11ty-base-dir))
					(condition-case nil
							(when (and dir (file-directory-p (expand-file-name filename dir)))
								(delete-directory (expand-file-name filename dir) t))
						(error nil)))
				(org-delete-property "EXPORT_ELEVENTY_FILE_NAME")
				(org-delete-property "EXPORT_DATE")
				(org-delete-property "EXPORT_ELEVENTY_PERMALINK"))))
	 ((derived-mode-p '(html-mode web-mode))
		;; called from an index.html or page.html, maybe?
		(let* ((file (buffer-file-name))
					 (json-file (concat (file-name-sans-extension (buffer-file-name))
															".11tydata.json"))
					 (json-data (and (file-exists-p json-file)
													 (json-read-file json-file))))
			;; delete the published files
			(when (alist-get 'permalink json-data)
				(dolist (dir (list sacha-11ty-site-dir sacha-11ty-local-dir sacha-11ty-remote-dir))
					(condition-case nil
							(when (and dir (file-directory-p
															(expand-file-name (concat "." (alist-get 'permalink json-data)) dir)))
								(delete-directory (expand-file-name (concat "." (alist-get 'permalink json-data)) dir) t))
						(error nil))))
			;; delete the .json and the .html file
			(when (file-exists-p json-file)
				(delete-file json-file))
			(kill-buffer (current-buffer))
			(delete-file file)))))

(defalias 'sacha-org-11ty-delete-current-post #'sacha-org-11ty-unpublish-current-post)

;;;###autoload
(defun sacha-org-11ty-copy-permalink ()
	(interactive)
	(kill-new (concat "https://sachachua.com" (org-entry-get (point) "EXPORT_ELEVENTY_PERMALINK"))))

;;;###autoload
(defun sacha-org-11ty-browse-local ()
	(interactive)
	(unless (seq-find (lambda (o) (string-match "--serve" (assoc-default 'args (cdr o) nil "")))
										(proced-process-attributes))
		(let ((default-directory "~/proj/static-blog"))
			(setq sacha-org-11ty-serve-process (start-process "serve" nil "make" "serve"))))
	(browse-url "http://localhost:8080/blog"))

;;;###autoload
(defun sacha-org-11ty-serve-stop ()
	(interactive)
	(if (process-live-p sacha-org-11ty-serve-process)
			(stop-process sacha-org-11ty-serve-process)
		(when-let ((proc (seq-find (lambda (o) (string-match "--serve" (assoc-default 'args (cdr o) nil "")))
															 (proced-process-attributes))))
			(call-process "kill" nil nil nil (number-to-string) (car proc)))))


;;;###autoload
(defun sacha-org-11ty-prepare-subtree ()
  (interactive)
  (unless (or (org-entry-get (point) "EXPORT_DATE")
              (org-entry-get-with-inheritance "DATE"))
    (org-entry-put (point) "EXPORT_DATE" (format-time-string "%Y-%m-%dT%T%z")))
  (let ((path (concat "blog/" (format-time-string "%Y/%m/")
                      (sacha-make-slug (org-get-heading t t t t))
                      "/")))
    (unless (org-entry-get (point) "EXPORT_ELEVENTY_PERMALINK")
      (org-entry-put (point) "EXPORT_ELEVENTY_PERMALINK" (concat "/" path)))
    (unless (org-entry-get (point) "EXPORT_ELEVENTY_FILE_NAME")
      (org-entry-put (point) "EXPORT_ELEVENTY_FILE_NAME" path))))

(with-eval-after-load '11ty
	(advice-add
	 'org-11ty-export-to-11tydata-and-html
	 :before
	 (lambda (&optional _ subtreep &rest _)
		 (when (and subtreep (not (org-entry-get-with-inheritance "EXPORT_ELEVENTY_PERMALINK")))
			 (sacha-org-11ty-prepare-subtree)))))

;;;###autoload
(defun sacha-org-11ty-rename-subtree ()
	(interactive)
	(let ((new-path (concat "blog/" (format-time-string "%Y/%m/")
													(sacha-make-slug (org-get-heading t t t t))
													"/")))
		(when (not (string= new-path (org-entry-get (point) "EXPORT_ELEVENTY_FILE_NAME")))
			(when
					(file-exists-p (expand-file-name
													(org-entry-get (point) "EXPORT_ELEVENTY_FILE_NAME")
													sacha-11ty-base-dir))
				(rename-file (expand-file-name
											(org-entry-get (point) "EXPORT_ELEVENTY_FILE_NAME")
											sacha-11ty-base-dir)
										 (expand-file-name
											new-path
											sacha-11ty-base-dir)))
			(org-entry-put (point) "EXPORT_ELEVENTY_PERMALINK" (concat "/" path))
			(org-entry-put (point) "EXPORT_ELEVENTY_FILE_NAME" path))))
;;;###autoload
(defun sacha-11ty-convert-to-njk ()
  (interactive)
  (let* ((filename (buffer-file-name))
         (old-buffer (current-buffer))
         (new-name (concat (file-name-base filename) ".njk")))
    (save-buffer)
    (rename-file filename new-name)
    (find-file new-name)
    (kill-buffer old-buffer)))

;;;###autoload
(defun sacha-11ty-browse-page ()
  (interactive)
  (if (org-entry-get-with-inheritance "EXPORT_ELEVENTY_PERMALINK")
      (browse-url (concat "http://localhost:8080" (org-entry-get-with-inheritance "EXPORT_ELEVENTY_PERMALINK")))
    (let* ((json-object-type 'plist)
           (data (json-read-file (concat (file-name-base (buffer-file-name)) ".11tydata.json"))))
      (browse-url (concat "http://localhost:8080" (plist-get data :permalink))) )))

;;;###autoload
(defun sacha-org-11ty-pathname ()
	(if (derived-mode-p 'org-mode)
			(file-name-directory (org-entry-get-with-inheritance "EXPORT_ELEVENTY_FILE_NAME"))
		(let ((url (thing-at-point 'url)))
			(when url
				(url-file-directory (url-filename (url-generic-parse-url url)))))))

;;;###autoload
(defun sacha-org-11ty-find-post (url)
	(interactive (list (sacha-org-11ty-pathname)))
	;; check in posts.org
	(find-file "~/sync/orgzly/posts.org")
	(let ((pos (org-find-property "EXPORT_ELEVENTY_PERMALINK" url)))
		(when pos (goto-char pos))))

;;;###autoload
(defun sacha-org-11ty-find-file (file)
  (interactive
	 (list
		(completing-read
		 (if (sacha-org-11ty-pathname)
				 (format "Post (%s): " (concat "/" (sacha-org-11ty-pathname)))
			 "Post: ")
		 (mapcar (lambda (o) (replace-regexp-in-string "^~/proj/static-blog\\|index.html$" "" o))
						 (directory-files-recursively "~/proj/static-blog/blog" "index\\.html" nil))
		 nil nil nil nil (concat "/" (sacha-org-11ty-pathname)))))
  (find-file
	 (expand-file-name
		"index.html"
		(expand-file-name
		 (concat "." file)
		 "~/proj/static-blog"))))

;;;###autoload
(defun sacha-org-11ty-post-to-mastodon (&optional post-automatically)
  (interactive (list current-prefix-arg))
  (let ((message (concat (org-entry-get (point) "ITEM") " https://sachachua.com" (org-entry-get (point) "EXPORT_ELEVENTY_PERMALINK"))))
    (if post-automatically
        (sacha-mastodon-toot-public-string message)
      (mastodon-toot)
      (insert message))))

;; https://kitchingroup.cheme.cmu.edu/blog/2013/05/05/Getting-keyword-options-in-org-files/
;;;###autoload
(defun sacha-org-keywords ()
  "Parse the buffer and return a cons list of (property . value).
This is extracted from lines like:
#+PROPERTY: value"
  (org-element-map (org-element-parse-buffer 'element) 'keyword
    (lambda (keyword) (cons (org-element-property :key keyword)
                            (org-element-property :value keyword)))))

;;;###autoload
(defun sacha-11ty-copy-file-and-insert-into-org (filename caption)
  (interactive (list (read-file-name "File: ")
                     (read-string "Caption: ")))
	(let ((path (expand-file-name
							 (file-name-nondirectory filename)
							 (expand-file-name
								(org-entry-get-with-inheritance
								 "EXPORT_ELEVENTY_FILE_NAME")
								(assoc-default "ELEVENTY_BASE_DIR" (sacha-org-keywords)))
							 )))
		(copy-file filename path t)
		(insert "#+CAPTION: " caption "\n"
						(org-link-make-string (concat "file:" path)) "\n")))
;; 11ty static site generation:2 ends here

;; [[file:../Sacha.org::#moving-sacha-org-post-subtree-to-the-11ty-directory][Moving my Org post subtree to the 11ty directory:1]]
;;;###autoload
(defun sacha-org-11ty-copy-subtree (&optional do-cut subtreep)
	"Copy the subtree for the current post to the 11ty export directory.
With prefix arg, move the subtree."
	(interactive (list current-prefix-arg))
	(let* ((info (org-combine-plists
								(org-export--get-export-attributes '11ty subtreep)
								(org-export--get-buffer-attributes)
								(org-export-get-environment '11ty subtreep)))
				 (file-properties
					(seq-filter (lambda (entry)
												(string-match (regexp-opt
																			 '("ELEVENTY_COLLECTIONS"
                                         "ELEVENTY_BASE_DIR"
                                         "ELEVENTY_BASE_URL"
																				 "TITLE"
																				 "ELEVENTY_CATEGORIES"
																				 "ELEVENTY_LAYOUT"))
																			(car entry)))
					            (org-element-map (org-element-parse-buffer) 'keyword
						            (lambda (el) (cons (org-element-property :key el)
																           (org-element-property :value el))))))
				 (entry-properties (org-entry-properties))
				 (filename (expand-file-name
										"index.org"
										(expand-file-name
										 (plist-get info :file-name)
										 (plist-get info :base-dir))))
				 (parent-pos
					(and subtreep
							 (org-find-property
								"EXPORT_ELEVENTY_FILE_NAME"
								(org-entry-get-with-inheritance "EXPORT_ELEVENTY_FILE_NAME"))))
				 body)
		(unless (string= (buffer-file-name)
										 filename)
			(unless (file-directory-p (file-name-directory filename))
				(make-directory (file-name-directory filename) t))
			;; find the heading that sets the current EXPORT_ELEVENTY_FILE_NAME
			(if parent-pos
					(save-excursion
						(goto-char parent-pos)
						(org-copy-subtree 1 (if do-cut 'cut)))
				(setq body (buffer-string)))
			(with-temp-file filename
				(org-mode)
				(if subtreep
						(progn
							(insert
               (or
								(mapconcat
                 (lambda (o) (format "#+%s: %s" (car o) (cdr o)))
                 file-properties
                 "\n")
								"")
							 "\n")
							(org-yank))
					(insert body))))))
;; Moving my Org post subtree to the 11ty directory:1 ends here

;; [[file:../Sacha.org::#moving-sacha-org-post-subtree-to-the-11ty-directory][Moving my Org post subtree to the 11ty directory:4]]
(defvar sacha-org-11ty-export-and-copy nil "*Non-nil means copy to site after specified delay (ex: \"5s\").")
(defvar sacha-org-11ty-export-and-copy-browse nil "Non-nil means browse after copying.")

;;;###autoload
(defun sacha-org-11ty-export (&optional async subtreep visible-only body-only ext-plist)
	(when (and subtreep (not (org-entry-get-with-inheritance "EXPORT_ELEVENTY_PERMALINK")))
		(sacha-org-11ty-prepare-subtree))
  (let* ((info (org-11ty--get-info subtreep visible-only))
         (file (org-11ty--base-file-name subtreep visible-only))
				 (permalink-slug (sacha-make-slug (plist-get info :permalink)))
				 (org-html-footnotes-section
					(format
					 "<div id=\"%s-footnotes\">\n<h3 class=\"footnotes\">%%s</h3>\n<div id=\"%s-text-footnotes\">\n%%s\n</div>\n</div>"
					 permalink-slug
					 permalink-slug)))
		(unless (or (string= (plist-get info :input-file)
										     (expand-file-name
											    "index.org"
											    (expand-file-name
											     (plist-get info :file-name)
											     (plist-get info :base-dir))))
                (plist-get (org-11ty--front-matter info) :no_source))
			(save-window-excursion
				(sacha-org-11ty-copy-subtree nil subtreep)))
		(org-11ty-export-to-11tydata-and-html async subtreep visible-only body-only ext-plist)
    (when sacha-org-11ty-export-and-copy
      (message "%s" "Scheduling copy...")
      (run-at-time sacha-org-11ty-export-and-copy nil
                   (lambda (url)
                     (sacha-org-11ty-copy-just-this-post
                      url))
                   (plist-get info :permalink)))))

;;;###autoload
(defun sacha-org-11ty-export-and-copy (&rest args)
  "Export and copy to website."
  (let ((sacha-org-11ty-export-and-copy "10"))
    (apply #'sacha-org-11ty-export args)))

;;;###autoload
(defun sacha-org-11ty-export-copy-browse (&rest args)
  "Export and copy to website."
  (let ((sacha-org-11ty-export-and-copy "10")
				(sacha-org-11ty-export-and-copy-browse t))
    (apply #'sacha-org-11ty-export args)))
;; Moving my Org post subtree to the 11ty directory:4 ends here

;; [[file:../Sacha.org::#moving-sacha-org-post-subtree-to-the-11ty-directory][Moving my Org post subtree to the 11ty directory:6]]
;;;###autoload
(define-minor-mode sacha-org-11ty-auto-export-mode
  ""
	:lighter ""
	(if sacha-org-11ty-auto-export-mode
			(progn
				(setq-local sacha-org-11ty-export-and-copy "10")
				(add-hook 'after-save-hook #'sacha-org-11ty-export-and-copy nil t))
		(setq-local sacha-org-11ty-export-and-copy nil)
		(remove-hook 'after-save-hook #'sacha-org-11ty-export-and-copy t)))
;; Moving my Org post subtree to the 11ty directory:6 ends here

;; [[file:../Sacha.org::#moving-sacha-org-post-subtree-to-the-11ty-directory][Moving my Org post subtree to the 11ty directory:7]]
;;;###autoload
(defun sacha-org-11ty-update-modified ()
  "Update modified date."
  (interactive)
	(if (or (org-before-first-heading-p)
					(not (org-entry-get-with-inheritance "ELEVENTY_PERMALINK")))
			(sacha-org-set-file-property "MODIFIED" (format-time-string "%Y-%m-%d"))
		(save-excursion
			(goto-char (org-find-property "ELEVENTY_PERMALINK" (org-entry-get-with-inheritance "ELEVENTY_PERMALINK")))
			(org-entry-put (point) "MODIFIED" (format-time-string "%Y-%m-%d")))))
;; Moving my Org post subtree to the 11ty directory:7 ends here

(provide 'sacha-org-11ty)
;;; sacha-org-11ty.el ends here
