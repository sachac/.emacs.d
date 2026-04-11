;;; sacha-org-export.el ---  -*- lexical-binding: t -*-

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

;;; Code:



;; [[file:../Sacha.org::#org-mode-publishing-changing-org-mode-underlines-to-the-html-mark-element][Changing Org Mode underlines to the HTML mark element:2]]
;;;###autoload
(defun sacha-org-highlight-export (link desc format _)
	(pcase format
		((or '11ty 'html)
		 (format "<mark%s>%s</mark>"
						 (if link
								 (format " class=\"%s\"" link)
							 link)
						 desc))))
;; Changing Org Mode underlines to the HTML mark element:2 ends here

;; [[file:../Sacha.org::#org-inline-svg][Include inline SVGs in Org Mode HTML and Markdown exports:2]]
;;;###autoload
(defun sacha-ox-link-path (link _ info)
	(let* ((raw-path (org-element-property :path link)))
		(setq raw-path
					(org-export-file-uri
					 (org-publish-file-relative-name raw-path info)))
		;; Possibly append `:html-link-home' to relative file
		;; name.
		(let ((home (and (plist-get info :html-link-home)
										 (org-trim (plist-get info :html-link-home)))))
			(when (and home
								 (plist-get info :html-link-use-abs-url)
								 (not (file-name-absolute-p raw-path)))
				(setq raw-path (concat (file-name-as-directory home) raw-path))))
		raw-path))

;;;###autoload
(defun sacha-org-html-link (link desc info)
	(if (and
			 (string= (org-element-property :type link) "file")
			 (not (plist-get (org-export-read-attribute :attr_html (org-element-parent-element link))
											 :data-link))
			 (org-export-inline-image-p link (plist-get info :html-inline-image-rules)))
			(let ((path (org-element-property :path link))
						(attr (org-export-read-attribute :attr_html (org-element-parent-element link))))
				(if (string= (file-name-extension path) "svg")
						(with-temp-buffer
              (set-buffer-multibyte t)
							(insert-file-contents path)
							(if attr
									(replace-regexp-in-string
									 "<svg "
									 (concat
										"<svg "
										(org-html--make-attribute-string attr)
										" ")
									 (buffer-string))
								(buffer-string)))
					(org-html-link link desc info)))
		(org-html-link link desc info)))

;;;###autoload
(defun sacha-org-md-link (link desc info)
	(if (and (string= (org-element-property :type link) "file")
					 (not (plist-get (org-export-read-attribute :attr_html (org-element-parent-element link))
											 :data-link)))
			(let ((path (org-element-property :path link)))
				(if (string= (file-name-extension path) "svg")
						(with-temp-buffer
							(insert-file-contents-literally path)
							(buffer-string))
					(org-md-link link desc info)))
		(org-md-link link desc info)))

;;;###autoload
(defun sacha-org-11ty-link (link desc info)
	(if (and (string= (org-element-property :type link) "file")
					 (not (plist-get (org-export-read-attribute :attr_html (org-element-parent-element link))
													 :data-link))
					 (not desc))
			(let ((path (org-element-property :path link))
						(attr (org-export-read-attribute :attr_html (org-element-parent-element link))))
				(if (string= (file-name-extension path) "svg")
						(with-temp-buffer
              (set-buffer-multibyte t)
							(insert-file-contents path)
							(if attr
									(replace-regexp-in-string
									 "<svg "
									 (concat
										"<svg "
										(org-html--make-attribute-string attr)
										" ")
									 (buffer-string))
								(buffer-string)))
					(org-11ty-link link desc info)))
		(org-11ty-link link desc info)))
;; Include inline SVGs in Org Mode HTML and Markdown exports:2 ends here

;; [[file:../Sacha.org::#org-inline-svg][Include inline SVGs in Org Mode HTML and Markdown exports:3]]
(with-eval-after-load 'ox-html
	(setf
	 (alist-get 'link (org-export-backend-transcoders (org-export-get-backend 'html)))
	 'sacha-org-html-link))
(with-eval-after-load 'ox-md
	(setf
	 (alist-get 'link (org-export-backend-transcoders (org-export-get-backend 'md)))
	 'sacha-org-md-link))
(with-eval-after-load 'ox-11ty
	(setf
	 (alist-get 'link (org-export-backend-transcoders (org-export-get-backend '11ty)))
	 'sacha-org-11ty-link))
;; Include inline SVGs in Org Mode HTML and Markdown exports:3 ends here

;; [[file:../Sacha.org::#org-mode-publishing-html-export-html-copy-files-and-serve-via-simple-httpd][Org Mode: Export HTML, copy files, and serve the results via simple-httpd so that media files work:4]]
;;;###autoload
(defun sacha-org-serve-buffer (&optional async _subtreep visible-only body-only ext-plist)
  (sacha-org-export-and-serve nil))

;;;###autoload
(defun sacha-org-serve-subtree (&optional async _subtreep visible-only body-only ext-plist)
  (sacha-org-export-and-serve t))

;; Based on org-11ty--copy-files-and-replace-links
;; Might be a good idea to use something DOM-based instead
(defun sacha-html-copy-files-and-replace-links (info &optional destination-dir)
  (let ((file-regexp "\\(?:src\\|href\\|poster\\)=\"\\(\\(file:\\)?.*?\\)\"")
        (destination-dir (or destination-dir (file-name-directory (plist-get info :file-path))))
        file-all-urls file-name beg
				new-file file-re
				unescaped)
    (unless (file-directory-p destination-dir)
      (make-directory destination-dir t))
    (unless (file-directory-p destination-dir)
      (error "%s is not a directory." destination-dir))
    (save-excursion
			(goto-char (point-min))
      (while (re-search-forward file-regexp nil t)
        (setq file-name (or (match-string 1) (match-string 2)))
				(unless (or (string-match "^#" file-name)
                    (get-text-property 0 'changed file-name))
					(setq file-name
                (replace-regexp-in-string
                 "\\?.+" ""
                 (save-match-data (if (string-match "^file:" file-name)
																		  (substring file-name 7)
																	  file-name))))
          (setq unescaped
                (replace-regexp-in-string
								 "%23" "#"
								 file-name))
					(setq new-file (concat
													(if info (plist-get info :permalink) "")
													(file-name-nondirectory unescaped)))
					(unless (org-url-p file-name)
            (let ((new-file-name (expand-file-name (file-name-nondirectory unescaped)
                                                   destination-dir)))
						  (condition-case err
                  (when (or (not (file-exists-p new-file-name))
                            (file-newer-than-file-p unescaped new-file-name))
							      (copy-file unescaped new-file-name t))
							  (error nil))
						  (when (file-exists-p new-file-name)
							  (save-excursion
								  (goto-char (point-min))
								  (setq file-re (concat "\\(?: src=\"\\| href=\"\\| poster=\"\\)\\(\\(?:file://\\)?" (regexp-quote file-name) "\\)"))
								  (while (re-search-forward file-re nil t)
									  (replace-match
                     (propertize
                      (save-match-data (replace-regexp-in-string "#" "%23" new-file))
                      'changed t)
										 t t nil 1)))))))))))

;;;###autoload
(defun sacha-org-export-and-serve (&optional subtreep)
  "Export current org buffer (or subtree if SUBTREEP) to HTML and serve via simple-httpd."
  (interactive "P")
  (require 'simple-httpd)
  (httpd-stop)
  (unless httpd-root (error "Set `httpd-root'."))
  (unless (file-directory-p httpd-root)
    (make-directory httpd-root t))
  (unless (file-directory-p httpd-root)
    (error "%s is not a directory." httpd-root))
  (let* ((out-file (expand-file-name (concat (file-name-base (buffer-file-name)) ".html")
                                     httpd-root))
         (html-file (org-export-to-file 'sacha-html-served out-file nil subtreep)))
    ;; Copy all the files and rewrite all the links
    (with-temp-file out-file
      (insert-file-contents out-file)
      (sacha-html-copy-files-and-replace-links
       `(:permalink "/") httpd-root))
    (httpd-start)
    (browse-url (format "http://localhost:%d/%s"
                        httpd-port
                        (file-name-nondirectory html-file)))))
;; Org Mode: Export HTML, copy files, and serve the results via simple-httpd so that media files work:4 ends here

;; [[file:../Sacha.org::#moving-sacha-org-post-subtree-to-the-11ty-directory][Moving my Org post subtree to the 11ty directory:2]]
;;;###autoload
(defun sacha-org-export-filter-body-add-index-link (info)
  (when (and
				 (plist-get info :file-name)
				 (plist-get info :base-dir)
				 (file-exists-p (expand-file-name
												 "index.org"
												 (expand-file-name
													(plist-get info :file-name)
													(plist-get info :base-dir)))))
		(goto-char (point-max))
		(insert
		 (format "<div><a href=\"%sindex.org\">View Org source for this post</a></div>"
						 (plist-get info :permalink)))))
;; Moving my Org post subtree to the 11ty directory:2 ends here

;; [[file:../Sacha.org::#moving-sacha-org-post-subtree-to-the-11ty-directory][Moving my Org post subtree to the 11ty directory:4]]
(defvar sacha-org-11ty-export-and-copy nil "*Non-nil means copy to site after specified delay (ex: \"5s\").")

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
;; Moving my Org post subtree to the 11ty directory:4 ends here

;; [[file:../Sacha.org::#org-mode-publishing-remove-heading-from-toc][Remove heading from TOC:1]]
;;;###autoload
(defun sacha-org-html-toc (depth info &optional scope)
  "Build a table of contents.
DEPTH is an integer specifying the depth of the table.  INFO is
a plist used as a communication channel.  Optional argument SCOPE
is an element defining the scope of the table.  Return the table
of contents as a string, or nil if it is empty."
  (let ((toc-entries
	 (mapcar (lambda (headline)
		   (cons (org-html--format-toc-headline headline info)
			 (org-export-get-relative-level headline info)))
		 (org-export-collect-headlines info depth scope))))
    (when toc-entries
      (let* ((toc-id-counter (plist-get info :org-html--toc-counter))
             (toc (concat (format "<div class=\"text-table-of-contents toc-id%s\" role=\"doc-toc\">"
                                  (if toc-id-counter (format "-%d" toc-id-counter) ""))
			  (org-html--toc-text toc-entries)
			  "</div>\n")))
        (plist-put info :org-html--toc-counter (1+ (or toc-id-counter 0)))
	(if scope toc
	  (let ((outer-tag (if (org-html--html5-fancy-p info)
			       "nav"
			     "div")))
	    (concat (format "<%s class=\"table-of-contents toc-id%s\" role=\"doc-toc\">\n"
                            outer-tag
                            (if toc-id-counter (format "-%d" toc-id-counter) ""))
							;; (let ((top-level (plist-get info :html-toplevel-hlevel)))
							;; (format "<h%d>%s</h%d>\n"
							;;   top-level
							;;   (org-html--translate "Table of Contents" info)
							;;   top-level))
		    toc
		    (format "</%s>\n" outer-tag))))))))

;; (with-eval-after-load 'org
;;   (advice-add 'org-html-toc :override #'sacha-org-html-toc))
;; Remove heading from TOC:1 ends here

;; [[file:../Sacha.org::#config-footer][Add a note to the bottom of blog posts exported from my config file:1]]
;;;###autoload
(defun sacha-org-export-filter-body-add-emacs-configuration-link (string backend info)
  (when (and (plist-get info :input-file) (string-match "\\.emacs\\.d/Sacha\\.org\\|sync/emacs/Sacha\\.org" (plist-get info :input-file)))
    (concat string
            (let ((id (org-entry-get-with-inheritance "CUSTOM_ID")))
              (format
							 (if (eq backend 'md)
									 "\nThis is part of my [Emacs configuration](https://sachachua.com/dotemacs%s)\n"
								 "\n<div class=\"note\">This is part of my <a href=\"https://sachachua.com/dotemacs%s\">Emacs configuration.</a></div>")
               (if id (concat "#" id) ""))))))
;; Add a note to the bottom of blog posts exported from my config file:1 ends here

;; [[file:../Sacha.org::#cleaning-up-export][Cleaning up export:2]]
(defun sacha-org-11ty-publish-from-project (_ from-file _)
  (with-current-buffer (find-file-noselect from-file)
    (save-excursion
     (goto-char (point-min))
     (when (save-excursion (re-search-forward "ELEVENTY_PERMALINK" nil t))
       (sacha-org-11ty-export)))))
;(load "~/proj/dev/emacs-chats/build-site.el" t)
;(load "~/proj/dev/emacs-notes/build-site.el" t)
;; Cleaning up export:2 ends here

;; [[file:../Sacha.org::#cleaning-up-export][Cleaning up export:3]]
;;;###autoload
(defun sacha-org-publish-maybe ()
  (require 'ox-publish)
  (interactive)
  (save-excursion
    (if (org-publish-get-project-from-filename
         (buffer-file-name (buffer-base-buffer)) 'up)
        (org-publish-current-file t)
      (sacha-org-html-export-trustingly))))
;; Cleaning up export:3 ends here

;; [[file:../Sacha.org::#cleaning-up-export][Cleaning up export:4]]
;;;###autoload
(defun sacha-org-publish-and-browse ()
  (interactive)
  (save-buffer)
  (sacha-org-publish-maybe)
  (browse-url (org-export-output-file-name ".html" nil default-directory)))
;; Cleaning up export:4 ends here

;; [[file:../Sacha.org::#publish-without-prompting][Publish without prompting:1]]
;;;###autoload
(defun sacha-org-html-export-trustingly ()
  (interactive)
  (let ((org-confirm-babel-evaluate nil))
    (org-html-export-to-html)))

;;;###autoload
(defun sacha-org-html-publish-to-html-trustingly (plist filename pub-dir)
  (let ((org-confirm-babel-evaluate nil))
    (org-html-publish-to-html plist filename pub-dir)))
;; Publish without prompting:1 ends here

;; [[file:../Sacha.org::#org-mode-publishing-abbreviations][Abbreviations:1]]
;;;###autoload
(defun sacha-org-abbr-export (path desc backend info)
  "Export abbr links for Org mode.
PATH is the expansion/title.
DESC is the abbreviation text (optional).
BACKEND is the export backend.
INFO is a plist holding contextual information."
  (pcase backend
    ;; HTML export
    ((or 'html '11ty)
     (if desc
         (format "<abbr title=\"%s\" tabindex=\"0\">%s</abbr>"
                 (org-html-encode-plain-text path)
                 (org-html-encode-plain-text desc))
       (format "<abbr>%s</abbr>"
               (org-html-encode-plain-text path))))
    ('org
     (org-link-make-string (concat "abbr:" path) desc))
    ;; LaTeX export
    ('latex
     (if desc
         (format "\\abbr[%s]{%s}"
                 (org-latex-encode-plain-text path)
                 (org-latex-encode-plain-text desc))
       (format "\\abbr{%s}"
               (org-latex-encode-plain-text path))))

    ;; ASCII/plain text export
    ('ascii
     (if desc
         (format "%s (%s)" desc path)
       path))

    ;; Default for other backends
    (_
     (if desc
         (format "%s (%s)" desc path)
       path))))
;; Abbreviations:1 ends here

;; [[file:../Sacha.org::#org-mode-publishing-abbreviations][Abbreviations:2]]
(with-eval-after-load 'org
	(org-link-set-parameters "abbr"	:export #'sacha-org-abbr-export))
;; Abbreviations:2 ends here

;; [[file:../Sacha.org::#adding-a-custom-header-argument-to-org-mode-source-blocks-and-using-that-argument-during-export][Adding a custom header argument to Org Mode source blocks and using that argument during export:1]]
(eval-and-compile
  (require 'org-macs nil t))
;;;###autoload
(defun sacha-org-html-src-block (src-block _contents info)
	(let* ((result
					(org-html-src-block
					 src-block
					 ;; todo: apply filter functions
					 _contents
					 info))
				 (block-info
					(org-with-point-at (org-element-property :begin src-block)
						(org-babel-get-src-block-info)))
				 (summary (assoc-default :summary (elt block-info 2))))
		(if (member summary '("%summary" ""))
				result
			(format "<details><summary>%s</summary>%s</details>"
							summary
							result))))

;;;###autoload
(defun sacha-org-11ty-src-block (src-block _contents info)
	(let* ((result (org-11ty-src-block src-block _contents info))
				 (block-info
					(org-with-point-at (org-element-property :begin src-block)
						(org-babel-get-src-block-info)))
				 (summary (assoc-default :summary (elt block-info 2))))
		(if (member summary '("%summary" ""))
				result
			(format "<details><summary>%s</summary>%s</details>"
							summary
							result))))
;; Adding a custom header argument to Org Mode source blocks and using that argument during export:1 ends here

;; [[file:../Sacha.org::#org-async-export-and-tangle][Org Mode: Asynchronous export and tangle of a large file:3]]
(defmacro sacha-org-debounce-idle-timer (seconds var body &rest args)
  `(progn
     (defvar ,var nil "Timer.")
     (when (timerp ,var) (cancel-timer ,var))
     (setq ,var (run-with-idle-timer ,seconds nil ,body ,@args))))
(defvar sacha-unfocusing nil "Non-nil when I'm in the middle of unfocusing.")
;;;###autoload
(defun sacha-org-async-export-and-tangle (&optional filename)
  (async-start
   `(lambda ()
      ;; make async emacs aware of packages (for byte-compilation)
      (package-initialize)
      (setq package-enable-at-startup nil)
      (require 'org)
			(setq-default tab-width 8)
			(setq org-babel-default-header-args
			      '((:session . "none")
			        (:results . "drawer replace")
							(:comments . "link")  ;; add a link to the original source
			        (:exports . "both")
			        (:cache . "no")
			        (:eval . "never-export") ;; explicitly evaluate blocks instead of evaluating them during export
			        (:hlines . "no")
			        (:tangle . "no"))) ;; I have to explicitly set up blocks for tangling
      (org-babel-tangle-file ,(buffer-file-name))
      )
   (lambda (&rest results) (message "Tangled.")))
  (org-export-to-file 'html (or filename "index.html") t))
(defun sacha-org-export-and-tangle-if-saved-in-focus ()
	(interactive)
  (when (frame-focus-state)
    (message "Scheduling export...")
    (sacha-org-debounce-idle-timer
		 10
     sacha-export-org-config
     (lambda (buf)
       (with-current-buffer buf
         (sacha-org-async-export-and-tangle "index.html")))
     (current-buffer))))

;;;###autoload
(define-minor-mode sacha-org-export-and-tangle-when-saved-in-focus-mode
  "Toggle a mode for exporting and tangling when saved.
Interactively with no argument, this command toggles the mode.
A positive prefix argument enables the mode, any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state."
  :group 'my
  (if sacha-org-export-and-tangle-when-saved-in-focus-mode
      (add-hook 'after-save-hook #'sacha-org-export-and-tangle-if-saved-in-focus nil t)
    (remove-hook 'after-save-hook #'sacha-org-export-and-tangle-if-saved-in-focus t)))

;;;###autoload
(defun sacha-org-save-and-tangle-sacha-config ()
	(when (string= (buffer-file-name) (expand-file-name "~/sync/emacs/Sacha.org")) (sacha-org-export-and-tangle-when-saved-in-focus-mode 1)))

;;;###autoload
(defun sacha-export-dotemacs (&optional sync)
	(interactive)
	(with-current-buffer (find-file-noselect "~/sync/emacs/Sacha.org")
		(org-babel-tangle)
    (if sync
        (org-export-to-file 'html "index.html")
		  (async-start
		   `(lambda ()
				  ;; make async emacs aware of packages (for byte-compilation)
				  (package-initialize)
				  (setq sacha-exporting t)
				  (load-file "~/sync/emacs/Sacha.el")
				  (find-file "~/sync/emacs/Sacha.org")
				  (org-export-to-file 'html "index.html"))
		   (lambda (&rest results) (message "Tangled and exported."))))))
;(use-package org
;  :hook ((org-mode . sacha-org-save-and-tangle-sacha-config)))
;; Org Mode: Asynchronous export and tangle of a large file:3 ends here

;; [[file:../Sacha.org::#org-mode-publishing-plain-text][Plain text:1]]
;;;###autoload
(defun sacha-plain-text-link (link contents info)
  "Export LINK in 'description URL' format."
  (let* ((type (org-element-property :type link))
         (path (org-element-property :path link))
         (raw-link (org-element-property :raw-link link))
         (description (or contents
													(and (string= type "fuzzy") path)
													path))
         (url (cond
               ((member type '("http" "https"))
                (concat type ":" path))
               ((string= type "file") path)
               (t raw-link))))
		(cond
     ((org-export-custom-protocol-maybe link description 'sacha-plain-text info))
     (t
			(if description
					(format "%s %s" description url)
				url)))))

;;;###autoload
(defun sacha-plain-text-item (item contents info)
  "Transcode an ITEM element with 4-space indentation."
	(replace-regexp-in-string "^\\(  \\)+" "\\1\\1"
														(org-ascii-item item contents info)))

;;;###autoload
(defun sacha-plain-text-export-to-buffer (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to plain text buffer."
  (interactive)
  (org-export-to-buffer 'sacha-plain-text "*My Plain Text Export*"
    async subtreep visible-only body-only ext-plist))

;;;###autoload
(defun sacha-plain-text-export-to-file (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to plain text file."
  (interactive)
  (let ((file (org-export-output-file-name ".txt" subtreep)))
    (org-export-to-file 'sacha-plain-text file
      async subtreep visible-only body-only ext-plist)))
;; Plain text:1 ends here

;; [[file:../Sacha.org::org-elisp-link][org-elisp-link]]
;;;###autoload
(defun sacha-org-elisp-link-export (link description format &optional arg)
  (pcase format
   ('html (format "<span title=\"%s\">%s</span>" (replace-regexp-in-string "\"" "&quot;" link) description))
   ((or 'icalendar 'ascii) description)
   ))
;; org-elisp-link ends here

;; [[file:../Sacha.org::#irc][IRC:2]]
;;;###autoload
(defun sacha-org-ircs-export (link description format)
   "Export an ircs link.
See `org-link-parameters' for details about LINK, DESCRIPTION and
FORMAT."
   (let ((desc (or description link)))
     (pcase format
       (`html (format "<a href=\"ircs:%s\">%s</a>" link desc))
       (`md (format "[%s](ircs:%s)" desc link))
       (_ nil))))
;; IRC:2 ends here

;; [[file:../Sacha.org::#save-when-emacs-loses-focus][Save when Emacs loses focus:1]]
;;;###autoload
(defun sacha-org-save-all-org-buffers ()
  (unless sacha-unfocusing
    (let ((sacha-unfocusing t))
      (sacha-org-debounce-idle-timer 10
                                  sacha-org-save-all-org-buffers-timer
                                  'org-save-all-org-buffers))))
;; Save when Emacs loses focus:1 ends here

(provide 'sacha-org-export)
;;; sacha-org-export.el ends here
