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
(defun sacha-org-11ty-unpublish-current-post ()
	(interactive)
	(cond
	 ((derived-mode-p 'org-mode)
		(when (org-entry-get (point) "EXPORT_ELEVENTY_FILE_NAME")
			(let ((filename (org-entry-get (point) "EXPORT_ELEVENTY_FILE_NAME")))
				(dolist (dir (list sacha-11ty-site-dir sacha-11ty-local-dir sacha-11ty-remote-dir sacha-11ty-base-dir))
					(when (and dir (file-directory-p (expand-file-name filename dir)))
						(delete-directory (expand-file-name filename dir) t)))
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
					(when (and dir (file-directory-p
													(expand-file-name (concat "." (alist-get 'permalink json-data)) dir)))
						(delete-directory (expand-file-name (concat "." (alist-get 'permalink json-data)) dir) t))))
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

(provide 'sacha-org-11ty)
;;; sacha-org-11ty.el ends here
