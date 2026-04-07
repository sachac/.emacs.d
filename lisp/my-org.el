;;; my-org.el ---  -*- lexical-binding: t -*-

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
;; - Using Embark to insert files as Org INCLUDEs
;;   https://sachachua.com/dotemacs#using-embark-to-insert-files-as-org-includes
;;
;; - Renaming and storing
;;   https://sachachua.com/dotemacs#keybindings-embark-renaming-and-storing
;;
;; - Hydra keyboard shortcuts
;;   https://sachachua.com/dotemacs#hydras
;;
;; - consult-omni
;;   https://sachachua.com/dotemacs#consult-omni
;;
;; - Using web searches and bookmarks to quickly link placeholders in Org Mode
;;   https://sachachua.com/dotemacs#completion-consult-consult-omni-using-web-searches-and-bookmarks-to-quickly-link-placeholders-in-org-mode
;;
;; - Inserting code
;;   https://sachachua.com/dotemacs#inserting-code
;;
;; - 11ty static site generation
;;   https://sachachua.com/dotemacs#11ty
;;
;; - Linking to blog topics
;;   https://sachachua.com/dotemacs#org-mode-publishing-11ty-static-site-generation-linking-to-blog-topics
;;
;; - Linking to blog posts
;;   https://sachachua.com/dotemacs#linking-to-blog-posts
;;
;; - Moving my Org post subtree to the 11ty directory
;;   https://sachachua.com/dotemacs#moving-my-org-post-subtree-to-the-11ty-directory
;;
;; - Include Mastodon, HN, Reddit fields in front matter
;;   https://sachachua.com/dotemacs#org-mode-publishing-11ty-static-site-generation-include-mastodon-field-in-front-matter
;;
;; - Copy Tasker task
;;   https://sachachua.com/dotemacs#org-mode-publishing-copy-tasker-task
;;
;; - Counting words without blocks
;;   https://sachachua.com/dotemacs#org-mode-publishing-counting-words-without-blocks
;;
;; - Org Mode: Including portions of files between two regular expressions
;;   https://sachachua.com/dotemacs#org-mode-including-portions-of-files-between-two-regular-expressions
;;
;; - Copy linked file and change link
;;   https://sachachua.com/dotemacs#copy-linked-file-and-change-link
;;
;; - Org Mode: Create a quick timestamped note and capture a screenshot
;;   https://sachachua.com/dotemacs#org-mode-create-a-quick-timestamped-note-and-capture-a-screenshot
;;
;; - Special blocks
;;   https://sachachua.com/dotemacs#special-blocks
;;
;; - Copy region
;;   https://sachachua.com/dotemacs#copy-region
;;
;; - Convert an inline link into a side note/footnote
;;   https://sachachua.com/dotemacs#org-mode-links-convert-an-inline-link-into-a-side-note-footnote
;;
;; - Adding Org Mode link awesomeness elsewhere: my-org-insert-link-dwim
;;   https://sachachua.com/dotemacs#my-org-insert-link-dwim
;;
;; - Links to my config
;;   https://sachachua.com/dotemacs#links-to-my-config
;;
;; - YouTube
;;   https://sachachua.com/dotemacs#youtube
;;
;; - Copy web link
;;   https://sachachua.com/dotemacs#web-link
;;
;; - Linking to headings that match a tag
;;   https://sachachua.com/dotemacs#org-mode-links-linking-to-headings-that-match-a-tag
;;
;; - Dired
;;   https://sachachua.com/dotemacs#org-dired
;;
;; - Speed command for adding a custom ID to Org Mode posts
;;   https://sachachua.com/dotemacs#add-custom-id
;;
;; - Counting
;;   https://sachachua.com/dotemacs#counting
;;
;; - Spreadsheets
;;   https://sachachua.com/dotemacs#spreadsheets
;;
;; - Copying and sharing code
;;   https://sachachua.com/dotemacs#copying-and-sharing-code
;;
;; - Tables
;;   https://sachachua.com/dotemacs#tables
;;
;; - Invoices
;;   https://sachachua.com/dotemacs#invoices
;;
;; - Counting words
;;   https://sachachua.com/dotemacs#counting-words
;;
;; - Allow dashes in tags
;;   https://sachachua.com/dotemacs#allow-dashes-in-tags
;;
;; - Convert from Markdown
;;   https://sachachua.com/dotemacs#org-mode-convert-from-markdown
;;
;; - Copying information from my phone
;;   https://sachachua.com/dotemacs#copying-information-from-my-phone
;;
;; - Reddit
;;   https://sachachua.com/dotemacs#reddit
;;
;; - Sorting Org Mode lists using a sequence of regular expressions
;;   https://sachachua.com/dotemacs#sorting-org-mode-lists-using-a-sequence-of-regular-expressions
;;
;; - Clipboard
;;   https://sachachua.com/dotemacs#clipboard
;;
;; - Setting properties
;;   https://sachachua.com/dotemacs#setting-properties
;;
;; - Org - send things to the bottom of the list
;;   https://sachachua.com/dotemacs#org-send-things-to-the-bottom-of-the-list
;;
;; - Org Mode: Format Libby book highlights exported as JSON
;;   https://sachachua.com/dotemacs#org-mode-org-mode-format-libby-book-highlights-exported-as-json
;;
;; - Org Mode custom link: copy to clipboard
;;   https://sachachua.com/dotemacs#org-mode-copy
;;
;; - Digital index piles with Emacs
;;   https://sachachua.com/dotemacs#digital-index-piles-with-emacs
;;
;; - Simplify inserting audio links
;;   https://sachachua.com/dotemacs#multimedia-subtitles-with-subed-simplify-inserting-audio-links
;;
;; - Collecting Emacs News from Mastodon
;;   https://sachachua.com/dotemacs#mastodon-news
;;
;; - Quantified Awesome
;;   https://sachachua.com/dotemacs#clock-in
;;
;; - Compare times and effort estimates
;;   https://sachachua.com/dotemacs#compare-time
;;
;; - List upcoming tasks so that I can see if I'm overloaded
;;   https://sachachua.com/dotemacs#list-upcoming-tasks-so-that-i-can-see-if-i-m-overloaded
;;
;; - Send currently-clocked task title to file, include in stream
;;   https://sachachua.com/dotemacs#streaming-send-currently-clocked-task-title-to-file-include-in-stream
;;
;; - Show Emacs-related tasks
;;   https://sachachua.com/dotemacs#show-emacs-related-tasks
;;
;; - Comparison-shopping with Org Mode
;;   https://sachachua.com/dotemacs#shopping
;;
;;; Code:



;; [[file:../Sacha.org::#using-embark-to-insert-files-as-org-includes][Using Embark to insert files as Org INCLUDEs:1]]
;;;###autoload
(defun my-insert-file-as-org-include (file)
  (interactive "fFile: ")
  (set-text-properties 0 (length file) nil file)
  (let ((mode (assoc-default file auto-mode-alist 'string-match)))
    (insert
     (org-link-make-string (concat "file:" file) (concat "Download " (file-name-nondirectory file))) "\n"
     "#+begin_my_details " (file-name-nondirectory file) "\n"
     (format "#+INCLUDE: %s" (prin1-to-string file))
     (if mode
         (concat " src " (replace-regexp-in-string "-mode$" "" (symbol-name mode)))
       "")
     "\n"
     "#+end_my_details\n")))

;;;###autoload
(defun my-transform-org-link-to-include ()
  (interactive)
  (let ((link (org-element-lineage (org-element-context) '(link) t))
        (mode (assoc-default (org-element-property :path link) auto-mode-alist 'string-match)))
    (when link
      (delete-region (org-element-property :begin link)
                     (org-element-property :end link))
      (my-insert-file-as-org-include (org-element-property :path link)))))
;; Using Embark to insert files as Org INCLUDEs:1 ends here

;; [[file:../Sacha.org::#keybindings-embark-renaming-and-storing][Renaming and storing:2]]
;;;###autoload
(defun my-org-svg-copy-links (filename)
  (interactive (list (read-file-name "SVG: " nil
                                     nil
                                     (lambda (f)
                                       (or (string-match "\\.svg$" f)
                                           (file-directory-p f))))))
  (let ((dom (car (xml-parse-file filename))))
    (kill-new
     (mapconcat
      (lambda (elem)
        (concat "- " (org-link-make-string
                      (dom-attr elem 'href)
                      (or (dom-attr elem 'title)
                          (dom-text (dom-by-tag elem 'title))))))
      (dom-by-tag dom 'a)
      "\n"))))
;; Renaming and storing:2 ends here

;; [[file:../Sacha.org::#hydras][Hydra keyboard shortcuts:4]]
;;;###autoload
(defun my-org-update-link-description (description)
  "Update the current link's DESCRIPTION."
  (interactive "MDescription: ")
  (let (link)
    (save-excursion
      (cond
       ((org-in-regexp org-link-bracket-re 1)
        (setq link (org-link-unescape (match-string-no-properties 1)))
        (delete-region (match-beginning 0) (match-end 0))
        (insert (org-link-make-string link description))
        (sit-for 0))
       ((or (org-in-regexp org-link-angle-re)
            (org-in-regexp org-link-plain-re))
        (setq link (org-unbracket-string "<" ">" (match-string 0)))
        (delete-region (match-beginning 0) (match-end 0))
        (insert (org-link-make-string link description))
        (sit-for 0))))))
;; Hydra keyboard shortcuts:4 ends here

;; [[file:../Sacha.org::#hydras][Hydra keyboard shortcuts:7]]
;;;###autoload
(defun my-org-check-agenda ()
  "Peek at agenda."
  (interactive)
  (cond
   ((derived-mode-p 'org-agenda-mode)
    (if (window-parent) (delete-window) (bury-buffer)))
   ((get-buffer "*Org Agenda*")
    (switch-to-buffer-other-window "*Org Agenda*"))
   (t (org-agenda nil "a"))))
;; Hydra keyboard shortcuts:7 ends here

;; [[file:../Sacha.org::#consult-omni][consult-omni:1]]
  (defun my-insert-or-replace-link (url &optional title)
          "Insert a link, wrap the current region in a link, or replace the current link."
          (interactive (list (read-string "URL: ")))
          (cond
           ((derived-mode-p 'org-mode)
                  (cond
                   ((org-in-regexp org-link-bracket-re 1)
                          (when (match-end 2) (setq title (match-string-no-properties 2)))
                          (delete-region (match-beginning 0) (match-end 0)))
                   ((org-in-regexp org-link-any-re 1)
                          (delete-region (match-beginning 0) (match-end 0)))
                   ((region-active-p)
                          (setq title (buffer-substring-no-properties (region-beginning) (region-end)))
                          (delete-region (region-beginning) (region-end))))
                  ;; update link
                  (insert (org-link-make-string url title)))
           ((derived-mode-p 'org-mode)		 ; not in a link
                  (insert (org-link-make-string url title)))
           ((and (region-active-p) (derived-mode-p 'markdown-mode))
                  (setq title (buffer-substring-no-properties (region-beginning) (region-end)))
                  (delete-region (region-beginning) (region-end))
                  (insert (format "[%s](%s)" title url)))
           ((derived-mode-p 'markdown-mode)
                  (insert (format "[%s](%s)" title url)))
           ((and (region-active-p) (string-match (regexp-quote "*new toot*") (buffer-name)))
                  (setq title (buffer-substring-no-properties (region-beginning) (region-end)))
                  (delete-region (region-beginning) (region-end))
                  (insert (format "[%s](%s)" title url)))
           ((string-match (regexp-quote "*new toot*") (buffer-name))
                  (insert (format "[%s](%s)" (read-string "Title: " (my-page-title url))
                                                                                  url)))
           (t
                  (insert (format "%s (%s)" title url)))))

  ;; override the embark actions
  (defun my-consult-omni-embark-copy-url-as-kill (cand)
          "Don't add spaces."
          (when-let ((s (and (stringp cand) (get-text-property 0 :url cand))))
                  (kill-new (string-trim s))))

  (defun my-consult-omni-embark-insert-url (cand)
          "Don't add spaces."
          (when-let ((s (and (stringp cand) (get-text-property 0 :url cand))))
                  (insert (string-trim s))))

  (defun my-consult-omni-embark-copy-title-as-kill (cand)
          "Don't add spaces."
          (when-let ((s (and (stringp cand) (get-text-property 0 :title cand))))
                  (kill-new (string-trim s))))

  (defun my-consult-omni-embark-insert-title (cand)
          "Don't add spaces."
          (when-let ((s (and (stringp cand) (get-text-property 0 :title cand))))
                  (insert (string-trim s))))

  (defun my-consult-omni-embark-insert-link (cand)
          "Don't add spaces."
          (let ((url (and (stringp cand) (get-text-property 0 :url cand)))
                                  (title (and (stringp cand) (get-text-property 0 :title cand))))
                  (my-insert-or-replace-link url title)))

  (use-package consult-omni
          :defer t
          :commands consult-omni
          :load-path "~/vendor/consult-omni"
    :after (consult embark)
    :custom
    (consult-omni-show-preview t) ;;; show previews
    (consult-omni-preview-key "C-o") ;;; set the preview key to C-o
    :config
          (add-to-list 'load-path "~/vendor/consult-omni/sources")
    (require 'consult-omni-sources)
    (require 'consult-omni-embark)
    (setq consult-omni-sources-modules-to-load (list 'consult-omni-wikipedia 'consult-omni-google))
    (consult-omni-sources-load-modules)
          (setq consult-omni-dynamic-input-debounce 1.0)
          (setq consult-omni-dynamic-refresh-delay consult-omni-dynamic-input-debounce)
    (setq consult-omni-default-interactive-command #'consult-omni-multi)
          (setq consult-omni-multi-sources
                                  '(consult-omni--source-google
                                          consult-omni--source-my-org-bookmarks
                                          consult-omni--source-blog))
          :bind
          (("M-g w" . consult-omni)
           ("M-g f" . consult-omni-my-org-bookmarks)
           :map consult-omni-embark-general-actions-map
           ("i l" .  #'my-consult-omni-embark-insert-link)
           ("i u" .  #'my-consult-omni-embark-insert-url)
           ("i t" .  #'my-consult-omni-embark-insert-title)
           ("w u" . #'my-consult-omni-embark-copy-url-as-kill)
           ("w t" . #'my-consult-omni-embark-copy-title-as-kill)))
;; consult-omni:1 ends here

;; [[file:../Sacha.org::#completion-consult-consult-omni-using-web-searches-and-bookmarks-to-quickly-link-placeholders-in-org-mode][Using web searches and bookmarks to quickly link placeholders in Org Mode:1]]
;; we're in a bracketed link with no description and the target doesn't look like a link;
;; likely I've actually added the text for the description and now we need to include the link
(defun my-org-in-bracketed-text-link-p ()
  (when (and (derived-mode-p 'org-mode) org-link-bracket-re)
    (let* ((bracket-pos (org-in-regexp org-link-bracket-re))
           (bracket-target (and bracket-pos (match-string 1)))
           (bracket-desc (and bracket-pos (match-string 2))))
      (and bracket-pos bracket-target
           (null bracket-desc)
           ;; try to trigger only when the target is plain text and doesn't have a protocol
           (not (string-match ":" bracket-target))))))

;;;###autoload
(defun my-org-set-link-target-with-search ()
  "Replace the current link's target with a web search.
  Assume the target is actually supposed to be the description.  For
  example, if the link is [[some text]], do a web search for 'some text',
  prompt for the link to use as the target, and move 'some text' to the
  description."
  (interactive)
  (let* ((bracket-pos (org-in-regexp org-link-bracket-re))
         (bracket-target (and bracket-pos (match-string 1)))
         (bracket-desc (match-string 2))
         result)
    (when (my-org-in-bracketed-text-link-p)
      (let ((link (consult-omni bracket-target nil nil t)))
        (cond
         ((get-text-property 0 :url link)
          (setq result (org-link-make-string (get-text-property 0 :url link)
                                             bracket-target)))
         ((string-match ":" link) ; might be a URL
          (setq result (org-link-make-string link bracket-target))))
        (when result
          (delete-region (car bracket-pos) (cdr bracket-pos))
          (insert result)
          result)))))
;; Using web searches and bookmarks to quickly link placeholders in Org Mode:1 ends here

;; [[file:../Sacha.org::#inserting-code][Inserting code:1]]
;;;###autoload
(defun my-org-insert-defun (function)
  "Inserts an Org source block with the definition for FUNCTION."
  (interactive (find-function-read))
  (let* ((buffer-point (condition-case nil (find-definition-noselect function nil) (error nil)))
         (new-buf (car buffer-point))
         (new-point (cdr buffer-point))
         definition)
    (if (and buffer-point new-point)
        (with-current-buffer new-buf ;; Try to get original definition
          (save-excursion
            (goto-char new-point)
            (setq definition (buffer-substring-no-properties (point) (save-excursion (end-of-defun) (point))))))
      ;; Fallback: Print function definition
      (setq definition (concat (prin1-to-string (symbol-function function)) "\n")))
    (if (org-in-src-block-p)
        (insert definition)
      (insert "#+begin_src emacs-lisp\n" definition "#+end_src\n"))))

;;;###autoload
(defun my-org-insert-function-and-key (keys)
  (interactive (caar (help--read-key-sequence)))
  (insert (format "=%s= (=%s=)" (symbol-name (key-binding keys t))
                  (key-description keys))))
;; Inserting code:1 ends here

;; [[file:../Sacha.org::#11ty][11ty static site generation:3]]
;;;###autoload
(defun my-org-replace-with-permalink ()
	(interactive)
	(let* ((elem (org-element-context))
				 (path (org-element-property :path elem))
				 (description (org-element-property :description elem))
				 (type (org-element-property :type elem))
				 (permalink (org-entry-get (point) "EXPORT_ELEVENTY_PERMALINK" t))
				 (base-url my-blog-base-url))
		(when (member type '("file" "audio" "video"))
			(delete-region (org-element-begin elem) (org-element-end elem))
			(insert (org-link-make-string (concat
																		 (if (string= type "file") "" (concat type ":"))
																		 base-url permalink (file-name-nondirectory path))
																		description)))))
;; 11ty static site generation:3 ends here

;; [[file:../Sacha.org::#org-mode-publishing-11ty-static-site-generation-linking-to-blog-topics][Linking to blog topics:1]]
;;;###autoload
(defun my-org-topic-open (link &rest _)
	"Find the post."
	(if (string-match "\\(.*\\)#\\(.+\\)" link)
			(let ((file (match-string 1 link))
						(anchor (match-string 2 link)))
				(find-file (format "~/sync/topics/%s.org" file))
				(goto-char (org-find-property "CUSTOM_ID" anchor)))
		(find-file (format "~/sync/topics/%s.org" link))))

;;;###autoload
(defun my-org-topic-export (link desc format _)
	(let ((path (concat (if (eq format '11ty) "/" "https://sachachua.com/")
											"topic/"
											(replace-regexp-in-string
											 "\\.html$" "/"
											 (replace-regexp-in-string
												"^/\\|index.html$" ""
												(replace-regexp-in-string "^index" ""
																									link))))))
		(pcase format
			((or 'html '11ty) (format "<a href=\"%s\">%s</a>" path (or desc link)))
      ('latex (format "\\href{%s}{%s}" path desc))
      ('texinfo (format "@uref{%s,%s}" path desc))
      ('ascii (if desc (format "%s (%s)" desc path)
								path)))))

;;;###autoload
(defun my-org-topic-complete ()
	(format "%stopic/%s/"
					my-blog-base-url
					(completing-read
					 "Topic: "
					 (mapcar (lambda (o) (file-name-base o))
									 (directory-files "~/sync/topics" "\\.org" nil)))))

;;;###autoload
(defun my-org-topic-store ()
	(when (and (derived-mode-p 'org-mode)
						 (buffer-file-name)
						 (string-match "/home/sacha/sync/topics/"
													 (expand-file-name (buffer-file-name))))
		(let ((props (org-collect-keywords '("TITLE")))
					(id (org-entry-get-with-inheritance "CUSTOM_ID")))
			(org-link-store-props
			 :link
			 (concat
				"topic:" (file-name-base (buffer-file-name))
				(if id
						(concat "#" id)
					""))
			 :description
			 (save-excursion
				 (if id (progn
									(goto-char (org-find-property "CUSTOM_ID" id))
									(org-entry-get (point) "ITEM"))
					 (car (assoc-default "TITLE" props #'string=))))))))
;; Linking to blog topics:1 ends here

;; [[file:../Sacha.org::org-blog-link][org-blog-link]]
(defvar my-blog-base-url "https://sachachua.com/")

;;;###autoload
(defun my-org-blog-complete ()
  "Select a blog post and return its URL."
  (my-blog-url (my-consult-blog-posts-by-title)))

;;;###autoload
(defun my-org-blog-export (link desc format _)
	(let ((path (concat (if (eq format '11ty) "/" my-blog-base-url)
											(replace-regexp-in-string "\\.html$" "/"
																								(replace-regexp-in-string "^/\\|index.html$" ""
																																					link)))))
		(pcase format
			((or 'html '11ty) (format "<a href=\"%s\">%s</a>" path (or desc link)))
      ('latex (format "\\href{%s}{%s}" path desc))
      ('texinfo (format "@uref{%s,%s}" path desc))
      ('ascii (if desc (format "%s (%s)" desc path)
								path)))))

;;;###autoload
(defun my-11ty-html-filename (link)
	"Return the HTML file for LINK."
  (when (listp link) (setq link (assoc-default 'permalink link)))
	(setq link (replace-regexp-in-string (concat "^blog:\\|" (regexp-quote my-blog-base-url)) "" link))
	(when (string-match "^/" link) (setq link (concat "." link)))
  (if (file-exists-p link)
			link
		(or (catch 'found
					(dolist (f
									 (list
										(expand-file-name "index.html"
																			(expand-file-name
																			 link
																			 my-11ty-base-dir))
										(expand-file-name "index.html"
																			(expand-file-name
																			 link
																			 (expand-file-name "blog" my-11ty-base-dir)))
										(replace-regexp-in-string
										 "/$" ".html"
										 (expand-file-name
											link
											my-11ty-base-dir))))
						(if (and f (file-exists-p f))
								(throw 'found f))))
				(error "%s not found" link))))

;;;###autoload
(defun my-org-blog-open (link &rest _)
	"Find the post if it exists, or open the HTML."
	(with-current-buffer (find-file-noselect "~/sync/orgzly/posts.org")
		(let ((pos (org-find-property "EXPORT_ELEVENTY_PERMALINK" link)))
			(if pos
					(progn (goto-char pos) (switch-to-buffer (current-buffer)))
				(when-let ((filename (my-11ty-html-filename link)))
					(find-file filename))))))

;;;###autoload
(defun my-org-link-insert-description (link &optional description)
	(unless description
		(my-blog-title (my-org-link-as-url link))))

;;;###autoload
(defun my-org-blog-store ()
	(when (derived-mode-p 'org-mode)
		(let* ((props (org-collect-keywords '("ELEVENTY_PERMALINK"
																					"ELEVENTY_BASE_URL"
																					"TITLE")))
					 (permalink
						(or (org-entry-get-with-inheritance "EXPORT_ELEVENTY_PERMALINK")
								(car (assoc-default "ELEVENTY_PERMALINK" props #'string=)))))
			(when permalink
				(org-link-store-props
				 :link
				 (concat
					(replace-regexp-in-string "/$" "" my-blog-base-url) permalink
					(if (org-entry-get-with-inheritance "CUSTOM_ID")
							(concat "#" (org-entry-get-with-inheritance "CUSTOM_ID"))
						""))
				 :description
				 (save-excursion
					 (goto-char (or (org-find-property "EXPORT_ELEVENTY_PERMALINK" permalink)
													(point-min)))
					 (or (org-entry-get (point) "ITEM")
							 (car (assoc-default "TITLE" props #'string=)))))))))
;; org-blog-link ends here

;; [[file:../Sacha.org::#moving-my-org-post-subtree-to-the-11ty-directory][Moving my Org post subtree to the 11ty directory:1]]
;;;###autoload
(defun my-org-11ty-copy-subtree (&optional do-cut subtreep)
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
					(org-find-property
           "EXPORT_ELEVENTY_FILE_NAME"
					 (org-entry-get-with-inheritance "EXPORT_ELEVENTY_FILE_NAME")))
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

;; [[file:../Sacha.org::#org-mode-publishing-11ty-static-site-generation-include-mastodon-field-in-front-matter][Include Mastodon, HN, Reddit fields in front matter:1]]
;;;###autoload
(defun my-org-11ty-add-mastodon-to-front-matter (front-matter info)
	(plist-put front-matter :mastodon (plist-get info :mastodon))
	(plist-put front-matter :hn (plist-get info :hn))
	(plist-put front-matter :reddit (plist-get info :reddit)))
;; Include Mastodon, HN, Reddit fields in front matter:1 ends here

;; [[file:../Sacha.org::#org-mode-publishing-copy-tasker-task][Copy Tasker task:1]]
;;;###autoload
(defun my-tasker-org-insert (url)
	(interactive "MTaskernet URL: ")
	(let* ((parts (url-path-and-query (url-generic-parse-url url)))
				 (params (url-parse-query-string (cdr parts)))
				 (xml-url (format
							 "https://taskernet.com/_ah/api/datashare/v1/sharedata/%s/%s?a=0&xml=true"
							 (url-hexify-string (car (assoc-default "user" params 'string=)))
							 (url-hexify-string (replace-regexp-in-string
																	 "\\+" " "
																	 (car (assoc-default "id" params 'string=))))))
				 (json-object-type 'alist)
				 (data (plz 'get
								 xml-url
								 :as
								 #'json-read))
				 filename)
		(setq filename (expand-file-name (concat (alist-get 'fileName data)
																						 "."
																						 (alist-get 'extension data)
																						 ".xml")
																		 my-download-dir))
		(with-temp-file filename
			(insert (alist-get 'shareData data)))
		(my-insert-file-as-org-include filename)
		(insert (org-link-make-string url "Import via Taskernet"))))
;; Copy Tasker task:1 ends here

;; [[file:../Sacha.org::#org-mode-publishing-counting-words-without-blocks][Counting words without blocks:1]]
;;;###autoload
(defun my-org-simplify-text (text)
  "Don't include source blocks or links."
  (with-temp-buffer
			(insert text)
			(org-mode)
			(goto-char (point-min))
			(while (re-search-forward org-link-any-re nil t)
        (replace-match
         (if (save-match-data (string-match "audio\\|vtime\\|video" (match-string 0)))
             ""
           (or (match-string 3) "(link)"))) )
			(goto-char (point-min))
			(while (re-search-forward "^ *#\\+begin" nil t)
			 (let* ((block (org-element-context))
              (text
               (if (or (eq (org-element-type block) 'quote-block)
                       (string= (org-element-property :type block)
                                "media-post"))
                   (buffer-substring
                    (org-element-contents-begin block)
                    (org-element-contents-end block))
                 "(block)")))
         (delete-region (org-element-begin block)
													    (org-element-end block))
         (insert text "\n")))
			(while (re-search-forward "\n\n+" nil t)
				(replace-match "\n"))
			(string-trim
			 (buffer-string))))

;;;###autoload
(defun my-org-subtree-text-without-blocks ()
	"Don't include source blocks or links. "
	(let (list)
		(save-excursion
			(save-restriction
				(org-back-to-heading)
				(org-narrow-to-subtree)
        (org-map-entries
         (lambda ()
           (push (buffer-substring-no-properties
                  (line-beginning-position)
                  (line-end-position))
                 list)
           (org-end-of-meta-data t)
           (unless (looking-at org-heading-regexp)
             (push
              (buffer-substring
               (point)
               (save-excursion
                 (or (outline-next-heading)
                     (point-max))))
              list)))
         nil 'tree)))
    (my-org-simplify-text (string-join (nreverse list) "\n"))))

;;;###autoload
(defun my-org-subtree-count-words-without-blocks ()
	(interactive)
	(let ((text (my-org-subtree-text-without-blocks)))
		(with-temp-buffer
			(insert text)
			(message "%s" (count-words--buffer-format)))))

;;;###autoload
(defun my-org-subtree-copy-words-without-blocks ()
	(interactive)
	(kill-new (my-org-subtree-text-without-blocks)))
;; Counting words without blocks:1 ends here

;; [[file:../Sacha.org::#org-mode-including-portions-of-files-between-two-regular-expressions][Org Mode: Including portions of files between two regular expressions:2]]
;;;###autoload
(defun my-include-open (path &optional _)
	"Narrow to the region specified in PATH."
	(require 'org-protocol)
	(let (params start end)
		(if (string-match "^\\(.*+?\\)\\(?:::\\|\\?\\)\\(.*+\\)" path)
				(setq params (save-match-data (org-protocol-convert-query-to-plist (match-string 2 path)))
							path (match-string 1 path)))
		(find-file path)
		(if (plist-get params :name)
				(when (org-babel-find-named-block (plist-get params :name))
					(goto-char (org-babel-find-named-block (plist-get params :name)))
					(let ((block (org-element-context)))
						(narrow-to-region (org-element-begin block)
															(org-element-end block))))
			(setq start
						(or
						 (and
							(plist-get params :from-regexp)
							(progn
								(goto-char (point-min))
								(when (re-search-forward (url-unhex-string (plist-get params :from-regexp)))
									(line-beginning-position))))
						 (progn
							 (goto-char (point-min))
							 (point))))
			(setq end
						(or
						 (and
							(plist-get params :to-regexp)
							(progn
								(when (re-search-forward (url-unhex-string (plist-get params :to-regexp)))
									(line-end-position))))
						 (progn
							 (goto-char (point-max))
							 (point))))
			(when (or (not (= start (point-min)))
								(not (= end (point-max))))
				(narrow-to-region start end)))))

;;;###autoload
(defun my-include-store ()
  "Store a link to Org Babel named blocks."
  (when-let ((elem (and (derived-mode-p 'org-mode) (org-element-at-point))))
    (when (and (org-element-type-p elem 'src-block)
               (org-element-property :name elem))
      (org-link-store-props :type "my-include"
                            :link (concat "my-include:" (buffer-file-name) "?name="
                                          (org-element-property :name elem))
                            :text (org-entry-get (point) "ITEM")))))

;;;###autoload
(defun my-include-export (path _ format _)
	"Export PATH to FORMAT using the specified wrap parameter."
	(require 'org-protocol)
	(let (params body start end)
		(when (string-match "^\\(.*+?\\)\\(?:::\\|\\?\\)\\(.*+\\)" path)
			(setq params (save-match-data (org-protocol-convert-query-to-plist (match-string 2 path)))
						path (match-string 1 path)))
		(with-temp-buffer
			(insert-file-contents-literally path)
			(when (string-match "\\.org$" path)
				(org-mode))
			(if (plist-get params :name)
					(when (org-babel-find-named-block (plist-get params :name))
						(goto-char (org-babel-find-named-block (plist-get params :name)))
						(let ((block (org-element-context)))
							(setq start (org-element-begin block)
										end (org-element-end block))))
				(goto-char (point-min))
				(when (plist-get params :from-regexp)
					(re-search-forward (url-unhex-string (plist-get params :from-regexp)))
					(goto-char (match-beginning 0)))
				(setq start (point))
				(setq end (point-max))
				(when (plist-get params :to-regexp)
					(re-search-forward (url-unhex-string (plist-get params :to-regexp)))
					(setq end (match-beginning 0))))
			(setq body (buffer-substring start end)))
		(with-temp-buffer
			(when (plist-get params :wrap)
				(let* ((wrap (plist-get params :wrap))
							 block args)
					(when (string-match "\\<\\(\\S-+\\)\\( +.*\\)?" wrap)
						(setq block (match-string 1 wrap))
						(setq args (match-string 2 wrap))
						(setq body (format "#+BEGIN_%s%s\n%s\n#+END_%s\n"
															 block (or args "")
															 body
															 block)))))
			(when (plist-get params :summary)
				(setq body (format "#+begin_my_details %s\n%s\n#+end_my_details\n"
													 (plist-get params :summary)
													 body)))
			(insert body)
			(message "BODY: %s" body)
			(org-export-as format nil nil t))))

;;;###autoload
(defun my-include-complete ()
	"Include a section of a file from one line to another, specified with regexps."
	(interactive)
	(require 'consult)
	(let ((file (read-file-name "File: ")))
		(save-window-excursion
			(find-file file)
			(concat "my-include:"
							file
							"?from-regexp="
							(let ((curr-line (line-number-at-pos
																(point)
																consult-line-numbers-widen))
										(prompt "From line: "))
								(goto-char (point-min))
								(consult-line)
								(url-hexify-string
								 (regexp-quote (buffer-substring (line-beginning-position) (line-end-position)))))
							"&to-regexp="
							(let ((curr-line (line-number-at-pos
																(point)
																consult-line-numbers-widen))
										(prompt "To line: "))
								(goto-char (point-min))
								(consult-line
								 nil (point))
								(url-hexify-string
								 (regexp-quote (buffer-substring (line-beginning-position) (line-end-position)))))
							"&wrap=src " (replace-regexp-in-string "-mode$" "" (symbol-name major-mode))))))
;; Org Mode: Including portions of files between two regular expressions:2 ends here

;; [[file:../Sacha.org::#org-mode-including-portions-of-files-between-two-regular-expressions][Org Mode: Including portions of files between two regular expressions:3]]
;;;###autoload
(defun my-org-display-included-images (&optional include-linked refresh beg end)
	"Display inline images for my-include types."
	(interactive "P")
	(when (display-graphic-p)
		(when refresh
      (org-remove-inline-images beg end)
      (when (fboundp 'clear-image-cache) (clear-image-cache)))
    (let ((end (or end (point-max))))
      (org-with-point-at (or beg (point-min))))
		(let* ((case-fold-search t)
					 (file-extension-re "\\.svg")
					 (file-types-re (format "\\[\\[my-include:")))
			(while (re-search-forward file-types-re end t)
				(let* ((link (org-element-lineage (save-match-data (org-element-context)) 'link t))
							 (inner-start (match-beginning 1))
							 (path
								(cond
								 ((not link) nil)
								 ;; file link without a description
								 ((or (not (org-element-contents-begin link)) include-linked)
									(org-element-property :path link))
								 ((not inner-start) nil)
								 (t (org-with-point-at inner-start
											(and (looking-at
														(if (char-equal ?< (char-after inner-start))
																org-link-angle-re
															org-link-plain-re))
													 ;; File name must fill the whole
													 ;; description.
													 (= (org-element-contents-end link)
															(match-end 0))
													 (progn
                             (setq linktype (match-string 1))
                             (match-string 2))))))))
					(when (string-match "\\(.+\\)\\?" path)
						(setq path (match-string 1 path)))
					(when (and path (string-match-p file-extension-re path))
						(let ((file (expand-file-name path)))
              ;; Expand environment variables.
              (when file (setq file (substitute-in-file-name file)))
							(when (and file (file-exists-p file))
								(let ((width (org-display-inline-image--width link))
											(old (get-char-property-and-overlay
														(org-element-begin link)
														'org-image-overlay)))
									(if (and (car-safe old) refresh)
                      (image-flush (overlay-get (cdr old) 'display))
										(let ((image (org--create-inline-image file width)))
											(when image
												(let ((ov (make-overlay
																	 (org-element-begin link)
																	 (progn
																		 (goto-char
																			(org-element-end link))
																		 (skip-chars-backward " \t")
																		 (point)))))
                          ;; FIXME: See bug#59902.  We cannot rely
                          ;; on Emacs to update image if the file
                          ;; has changed.
                          (image-flush image)
                          (overlay-put ov 'evaporate t)
													(overlay-put ov 'display image)
													(overlay-put ov 'face 'default)
													(overlay-put ov 'org-image-overlay t)
													(overlay-put
													 ov 'modification-hooks
													 (list 'org-display-inline-remove-overlay))
													(when (boundp 'image-map)
														(overlay-put ov 'keymap image-map))
													(push ov org-inline-image-overlays))))))))))))))

;; Org Mode: Including portions of files between two regular expressions:3 ends here

;; [[file:../Sacha.org::#copy-linked-file-and-change-link][Copy linked file and change link:1]]
;;;###autoload
(defun my-org-copy-linked-file-and-change-link (destination)
	(interactive (list
								(cond
								 ((and (not current-prefix-arg) (file-directory-p "images/"))
									"images/")
								 ((org-entry-get-with-inheritance "EXPORT_ELEVENTY_FILE_NAME")
									(expand-file-name
									 (org-entry-get-with-inheritance "EXPORT_ELEVENTY_FILE_NAME")
									 (cadar (org-collect-keywords '("ELEVENTY_BASE_DIR")))))
								 (t
									(read-file-name (format "Copy %s to: "
																					(file-name-nondirectory (org-element-property :path (org-element-context)))))))))
	(let* ((elem (org-element-context))
				 (path (org-element-property :path elem))
				 (description (org-element-property :description elem)))
		(copy-file path destination t)
		(delete-region (org-element-begin elem) (org-element-end elem))
		(insert (org-link-make-string
						 (concat "file:"
										 (file-relative-name
											(if (file-directory-p destination)
													(expand-file-name (file-name-nondirectory path)
																						destination)
												destination)))
						 description))))

;;;###autoload
(defun my-embark-org-copy-linked-file-and-change-link (url destination)
	(interactive (list
								(read-string "Link: ")
								(read-file-name (format "Copy %s to: "
																				(file-name-nondirectory (org-element-property :path (org-element-context)))))))
	(my-org-copy-linked-file-and-change-link destination))

;;;###autoload
(defun my-org-copy-linked-files (destination beg end)
	(interactive (list
								(if (and (not current-prefix-arg) (file-directory-p "images/"))
										"images/"
									(read-file-name (format "Copy %s to: "
																					(file-name-nondirectory (org-element-property :path (org-element-context))))))
								(if (region-active-p) (region-beginning) (point-min))
								(if (region-active-p) (region-end) (point-max))))
	(goto-char beg)
	(while (re-search-forward "\\(file\\):" end t)
		(let* ((elem (org-element-context))
					 (path (org-element-property :path elem))
					 (description (org-element-property :description elem)))
			(when path
				(unless (string-match (concat "^" (regexp-quote (expand-file-name destination))) (expand-file-name path))
					(my-org-copy-linked-file-and-change-link destination))))))
;; Copy linked file and change link:1 ends here

;; [[file:../Sacha.org::#org-mode-create-a-quick-timestamped-note-and-capture-a-screenshot][Org Mode: Create a quick timestamped note and capture a screenshot:2]]
;;;###autoload
(defun my-org-capture-prefill-template (template &rest values)
  "Pre-fill TEMPLATE with VALUES."
  (setq template (or template (org-capture-get :template)))
  (with-temp-buffer
    (insert template)
    (goto-char (point-min))
    (while (re-search-forward
            (concat "%\\("
                    "\\[\\(.+\\)\\]\\|"
                    "<\\([^>\n]+\\)>\\|"
                    "\\([tTuUaliAcxkKInfF]\\)\\|"
                    "\\(:[-a-zA-Z]+\\)\\|"
                    "\\^\\({\\([^}]*\\)}\\)"
                    "?\\([gGtTuUCLp]\\)?\\|"
                    "%\\\\\\([1-9][0-9]*\\)"
                    "\\)") nil t)
      (if (car values)
          (replace-match (car values) nil t))
      (setq values (cdr values)))
    (buffer-string)))

;;;###autoload
(defun my-capture-timestamped-note (time note)
  "Disable Helm and capture a quick timestamped note."
  (interactive (list (current-time) (read-string "Note: ")))
  (let ((helm-completing-read-handlers-alist '((org-capture . nil)))
        (entry (org-capture-select-template "p")))
    (org-capture-set-plist entry)
    (org-capture-get-template)
    (org-capture-set-target-location)
    (org-capture-put
     :template (org-capture-fill-template
                (my-org-capture-prefill-template (org-capture-get :template)
                                                 (format-time-string "%H:%M:%S,%3N")
                                                 note)))
    (org-capture-place-template)
    (org-capture-finalize)))

;;;###autoload
(defun my-capture-timestamped-note-with-screenshot (time note)
  "Include a link to the latest screenshot."
  (interactive (list (current-time) (read-string "Note: ")))
  (kill-new (my-latest-screenshot))
  (my-capture-timestamped-note time note))
;; Org Mode: Create a quick timestamped note and capture a screenshot:2 ends here

;; [[file:../Sacha.org::#special-blocks][Special blocks:3]]
;;;###autoload
(defun my-org-convert-list-to-collapsible-details ()
	(interactive)
	(let ((list (org-list-to-lisp t)))
		(mapc (lambda (o)
						(when (stringp (car o))
							(insert
							 (format
								"#+begin_my_details %s :open t\n%s#+end_my_details\n"
								(car o)
								(mapconcat
								 (lambda (s)
									 (concat "- " (string-trim (org-ascii--indent-string (car s) 2)) "\n"))
								 (cdr (cadr o)))))))
					(cdr list))))
;; Special blocks:3 ends here

;; [[file:../Sacha.org::#copy-region][Copy region:1]]
;;;###autoload
(defun my-org-copy-region-as-html (beg end &optional level)
  "Make it easier to copy code for Wordpress posts and other things."
  (interactive "r\np")
  (let ((org-export-html-preamble nil)
        (org-html-toplevel-hlevel (or level 3)))
    (kill-new
     (org-export-string-as (buffer-substring beg end) 'html t))))
;; Copy region:1 ends here

;; [[file:../Sacha.org::#copy-region][Copy region:2]]
;;;###autoload
(defun my-org-copy-subtree-as-html ()
  (interactive)
  (my-org-copy-region-as-html
   (org-back-to-heading)
   (org-end-of-subtree)))
;; Copy region:2 ends here

;; [[file:../Sacha.org::#org-mode-links-convert-an-inline-link-into-a-side-note-footnote][Convert an inline link into a side note/footnote:1]]
;;;###autoload
(defun my-org-convert-link-to-footnote (label)
  "Convert the link at point into a footnote."
  (interactive (list (read-string "Footnote label: ")))
  ;; Get the Org link path at point
  (when (org-in-regexp org-link-bracket-re 1)
    (let ((url (match-string 1)))
      (replace-match (match-string 2) 0)
      (skip-syntax-forward ".")
      (my-org-footnote-add (org-link-make-string (org-link-unescape url)
                                                 (my-org-link-default-description url nil))))))

;;;###autoload
(defun my-org-footnote-add (label &optional text)
  (interactive "MLabel: \n")
  (insert (format "[fn:%s]" label))
  (goto-char (org-footnote-create-definition label))
  (goto-char (line-end-position))
  (when text
    (insert text)))
;; Convert an inline link into a side note/footnote:1 ends here

;; [[file:../Sacha.org::#my-org-insert-link-dwim][Adding Org Mode link awesomeness elsewhere: my-org-insert-link-dwim:1]]
;;;###autoload
(defun my-org-insert-link-dwim (&optional url title)
	"Like `org-insert-link' but with personal dwim preferences."
	(interactive)
	(let* ((point-in-link (and (derived-mode-p 'org-mode) (org-in-regexp org-link-any-re 1)))
				 (point-in-html-block (and (derived-mode-p 'org-mode)
																	 (let ((elem (org-element-context)))
																		 (and (eq (org-element-type elem) 'export-block)
																					(string= (org-element-property :type elem) "HTML")))))
				 (point-in-src-or-export-block
					(and (derived-mode-p 'org-mode)
							 (let ((elem (org-element-context)))
								 (and (member (org-element-type elem) '(src-block export-block))
											(not (string= (org-element-property :type elem) "Org"))
                      (>= (point) (org-element-property :post-affiliated elem))
                      (<= (point) (+ (length (org-element-property :value elem))
                                     (org-element-property :post-affiliated elem)))))))
				 (region-content (when (region-active-p)
													 (buffer-substring-no-properties (region-beginning)
																													 (region-end))))
         (bookmark-match (when region-content (my-org-bookmark-match region-content)))
         (url (cond
               (url url)
							 ((my-org-in-bracketed-text-link-p) nil)
               (bookmark-match bookmark-match)
							 ((not point-in-link)
                (my-org-read-link
								 ;; clipboard
								 (when (string-match-p "^http" (current-kill 0))
									 (current-kill 0))))))
				 (title (or title
                    region-content
										(when (or (string-match (regexp-quote "*new toot*") (buffer-name))
															(derived-mode-p '(markdown-mode web-mode oddmuse-mode))
															point-in-html-block
															point-in-src-or-export-block
															(not (and (derived-mode-p 'org-mode)
																				point-in-link)))
											(read-string "Title: "
																	 (or (my-org-link-default-description url nil)
																			 (my-page-title url)))))))
		;; resolve the links; see my-org-link-as-url in  https://sachachua.com/dotemacs#web-link
		(unless (and (derived-mode-p 'org-mode)
								 (not (or point-in-html-block point-in-src-or-export-block)))
			(setq url (my-org-link-as-url url)))
		(when (region-active-p) (delete-region (region-beginning) (region-end)))
		(cond
		 ((or (string-match (regexp-quote "*new toot*") (buffer-name))
					(derived-mode-p 'markdown-mode))
			(insert (format "[%s](%s)" title url)))
		 ((or (derived-mode-p '(web-mode html-mode)) point-in-html-block)
			(insert (format "<a href=\"%s\">%s</a>" url title)))
		 ((derived-mode-p 'oddmuse-mode)
			(insert (format "[%s %s]" url title)))
		 ((or point-in-src-or-export-block
					(not (derived-mode-p 'org-mode)))
			(insert title " " url))
     ((or (derived-mode-p 'message-mode)
          (derived-mode-p 'notmuch-message-mode))
      (insert title " " url))
		 ((and region-content url (not point-in-link))
			(insert (org-link-make-string url region-content)))
		 ((and url (not point-in-link))
			(insert (org-link-make-string
							 url
							 (or title
									 (read-string "Title: "
																(or (my-org-link-default-description url nil)
																		(my-page-title url)))))))
		 ;; bracketed [[plain text]]; see Using web searches and bookmarks to quickly link placeholders in Org Mode https://sachachua.com/dotemacs#completion-consult-consult-omni-using-web-searches-and-bookmarks-to-quickly-link-placeholders-in-org-mode
		 ((my-org-set-link-target-with-search))
		 ;; In Org Mode, edit the link
		 ((call-interactively 'org-insert-link)))))
;; Adding Org Mode link awesomeness elsewhere: my-org-insert-link-dwim:1 ends here

;; [[file:../Sacha.org::#my-org-insert-link-dwim][Adding Org Mode link awesomeness elsewhere: my-org-insert-link-dwim:4]]
(defun my-org-link-https-insert-description (link desc)
	"Default to the page title."
	(unless desc (my-page-title link)))
;; Adding Org Mode link awesomeness elsewhere: my-org-insert-link-dwim:4 ends here

;; [[file:../Sacha.org::#my-org-insert-link-dwim][Adding Org Mode link awesomeness elsewhere: my-org-insert-link-dwim:6]]
;;;###autoload
(defun my-org-link-default-description (link desc)
	"Return the default description for an Org Mode LINK.
This uses :insert-description if defined."
	(let* ((abbrevs org-link-abbrev-alist-local)
				 (all-prefixes (append (mapcar #'car abbrevs)
															 (mapcar #'car org-link-abbrev-alist)
															 (org-link-types)))
				 (type
          (cond
           ((and all-prefixes
                 (string-match (rx-to-string `(: string-start (submatch (or ,@all-prefixes)) ":")) link))
            (match-string 1 link))
           ((file-name-absolute-p link) "file")
           ((string-match "\\`\\.\\.?/" link) "file"))))
		(when (org-link-get-parameter type :insert-description)
			(let ((def (org-link-get-parameter type :insert-description)))
				(condition-case nil
						(cond
						 ((stringp def) def)
						 ((functionp def)
							(funcall def link desc)))
					(error
					 nil))))))
;; Adding Org Mode link awesomeness elsewhere: my-org-insert-link-dwim:6 ends here

;; [[file:../Sacha.org::#my-org-insert-link-dwim][Adding Org Mode link awesomeness elsewhere: my-org-insert-link-dwim:7]]
;;;###autoload
(defun my-org-read-link (&optional default)
	"Act like `org-insert-link'. Return link."
	(let* ((wcf (current-window-configuration))
				 (origbuf (current-buffer))
				 (abbrevs org-link-abbrev-alist-local)
				 (all-prefixes (append (mapcar #'car abbrevs)
															 (mapcar #'car org-link-abbrev-alist)
															 (org-link-types)))

				 link)
		(unwind-protect
				;; Fake a link history, containing the stored links.
				(let ((org-link--history
							 (append (mapcar #'car org-stored-links)
											 org-link--insert-history)))
					(setq link
								(org-completing-read
								 (org-format-prompt "Insert link" (or default (caar org-stored-links)))
								 (append
									(mapcar (lambda (x) (concat x ":")) all-prefixes)
									(mapcar #'car org-stored-links)
									;; Allow description completion.  Avoid "nil" option
									;; in the case of `completing-read-default' when
									;; some links have no description.
									(delq nil (mapcar 'cadr org-stored-links)))
								 nil nil nil
								 'org-link--history
								 (or default (caar org-stored-links))))
					(unless (org-string-nw-p link) (user-error "No link selected"))
					(dolist (l org-stored-links)
						(when (equal link (cadr l))
							(setq link (car l))))
					(when (or (member link all-prefixes)
										(and (equal ":" (substring link -1))
												 (member (substring link 0 -1) all-prefixes)
												 (setq link (substring link 0 -1))))
						(setq link (with-current-buffer origbuf
												 (org-link--try-special-completion link)))))
			(when-let* ((window (get-buffer-window "*Org Links*" t)))
				(quit-window 'kill window))
			(set-window-configuration wcf)
			(when (get-buffer "*Org Links*")
				(kill-buffer "*Org Links*")))
    (replace-regexp-in-string
     "^Link: " ""
		 (string-trim link))))
;; Adding Org Mode link awesomeness elsewhere: my-org-insert-link-dwim:7 ends here

;; [[file:../Sacha.org::org-dotemacs-link][org-dotemacs-link]]
;;;###autoload
(defvar my-emacs-config-url)
(defun my-org-dotemacs-export (path desc format _)
	"Export dotemacs link."
	(pcase format
   ((or 'html '11ty 'md)
	  (format "<a href=\"%s#%s\">%s</a>"
            my-emacs-config-url
            path (or desc path)))
	('ascii
   (if desc
	     (format "%s %s#%s"
               desc
               my-emacs-config-url
               path)
     (format "%s#%s"
               my-emacs-config-url
               path)))))

;;;###autoload
(defun my-org-dotemacs-complete ()
	"Prompt for dotemacs."
	(interactive)
	(with-current-buffer (find-file-noselect "~/sync/emacs/Sacha.org")
		(concat "dotemacs:" (org-read-property-value "CUSTOM_ID"))))

;;;###autoload
(defun my-org-dotemacs-insert-description (link &optional description)
	(unless description
		(with-current-buffer (find-file-noselect "~/sync/emacs/Sacha.org")
			(save-restriction
				(save-excursion
					(widen)
					(goto-char (org-find-property "CUSTOM_ID" (replace-regexp-in-string "^dotemacs:" "" link)))
					(org-entry-get (point) "ITEM"))))))

;;;###autoload
(defun my-org-dotemacs-open (path)
	(with-current-buffer (find-file-noselect "~/sync/emacs/Sacha.org")
		(when-let ((pos (org-find-property "CUSTOM_ID" (replace-regexp-in-string "^dotemacs:" "" path))))
			(switch-to-buffer (current-buffer))
			(goto-char pos))))

;;;###autoload
(defun my-org-dotemacs-store ()
	(when (and (string= (buffer-file-name)
											(expand-file-name "~/sync/emacs/Sacha.org"))
						 (org-entry-get (point) "CUSTOM_ID"))
		(org-link-store-props
		 :link (concat "dotemacs:" (org-entry-get (point) "CUSTOM_ID"))
		 :description (org-entry-get (point) "ITEM"))))
;; org-dotemacs-link ends here

;; [[file:../Sacha.org::#youtube][YouTube:2]]
;;;###autoload
(defun my-org-copy-region-as-plain-text (beg end)
	"Copy as plain text, removing links."
	(interactive "r")
	(save-restriction
		(narrow-to-region beg end)
		(kill-new (org-export-as 'ascii nil nil t))))

;; YouTube:2 ends here

;; [[file:../Sacha.org::#web-link][Copy web link:1]]
;;;###autoload
(defun my-copy-link (&optional filename skip-links)
	"Return the URL of this file.
If FILENAME is non-nil, use that instead.
If SKIP-LINKS is non-nil, skip custom links.
If we're in a Dired buffer, use the file at point."
	(interactive)
	(setq filename (or filename
										 (if (derived-mode-p 'dired-mode) (dired-get-filename))
										 (buffer-file-name)))
	(if-let*
			((project-re (concat "\\(" (regexp-opt (mapcar 'car my-project-web-base-list)) "\\)"
													 "\\(.*\\)"))
			 (url (cond
						 ((and (derived-mode-p 'org-mode)
									 (eq (org-element-type (org-element-context)) 'link)
									 (not skip-links))
							(pcase (org-element-property :type (org-element-context))
								((or "https" "http")
								 (org-element-property :raw-link (org-element-context)))
								("yt"
								 (org-element-property :path (org-element-context)))
								;; if it's a custom link, visit it and get the link
								(_
								 (save-window-excursion
									 (org-open-at-point)
									 (my-copy-link nil t)))))
						 ;; links to my config usually have a CUSTOM_ID property
						 ((string= (buffer-file-name) (expand-file-name "~/sync/emacs/Sacha.org"))
							(concat "https://sachachua.com/dotemacs#" (org-entry-get-with-inheritance "CUSTOM_ID")))
						 ;; blog post drafts have permalinks
						 ((and (derived-mode-p 'org-mode) (org-entry-get-with-inheritance "EXPORT_ELEVENTY_PERMALINK"))
							(concat "https://sachachua.com" (org-entry-get-with-inheritance "EXPORT_ELEVENTY_PERMALINK")))
						 ;; some projects have web repos
						 ((string-match
							 project-re filename)
							(concat (assoc-default (match-string 1 filename) my-project-web-base-list)
											(url-hexify-string (match-string 2 filename)))))))
			(progn
				(when (called-interactively-p 'any)
					(kill-new url)
					(message "%s" url))
				url)
		(error "Couldn't figure out URL.")))
;; Copy web link:1 ends here

;; [[file:../Sacha.org::#web-link][Copy web link:2]]
;;;###autoload
(defun my-org-link-as-url (link)
	"Return the final URL for LINK."
	(cond
	 ((string-match "^/" link)
		(concat my-blog-base-url (replace-regexp-in-string "^/" "" link)))
	 ((string-match "^\\(https://\\|file:\\)" link)
		link)
	 (t
		(dom-attr
		 (dom-by-tag
			(with-temp-buffer
				(insert (org-export-string-as link 'html t))
				(xml-parse-region (point-min) (point-max)))
			'a)
		 'href))))


;;;###autoload
(defun my-org-stored-link-as-url (&optional link insert)
	"Copy the stored link as a plain URL.
If LINK is specified, use that instead."
	(interactive (list nil current-prefix-arg))
	(setq link (or link (caar org-stored-links)))
	(let ((url (if link
								 (my-org-link-as-url link)
							 (error "No stored link"))))
		(when (called-interactively-p 'any)
			(if url
					(if insert (insert url) (kill-new url))
				(error "Could not find URL.")))
		url))

(ert-deftest my-org-stored-link-as-url ()
	(should
	 (string= (my-org-stored-link-as-url "[[dotemacs:web-link]]")
						"https://sachachua.com/dotemacs#web-link"))
	(should
	 (string= (my-org-stored-link-as-url "[[dotemacs:org-mode-sketch-links][my Org Mode sketch links]]")
						"https://sachachua.com/dotemacs#org-mode-sketch-links")))

;;;###autoload
(defun my-embark-org-copy-exported-url-as-wayback (link &rest _)
	(interactive "MLink: ")
	(let ((url	(my-embark-org-copy-exported-url link)))
		(when (not (string-match (regexp-quote "^https://web.archive.org") url))
			(setq url (concat "https://web.archive.org/web/" (format-time-string "%Y%m%d%H%M%S/")
												url)))
		(when (called-interactively-p 'any)
			(kill-new url)
			(message "Copied %s" url))
		url))

;;;###autoload
(defun my-embark-org-copy-exported-url (link &rest _)
	(interactive "MLink: \np")
	(let ((url (my-org-link-as-url link)))
		(when (and (derived-mode-p 'org-mode)
							 (org-entry-get-with-inheritance "EXPORT_ELEVENTY_PERMALINK")
							 (string-match "^/" url))
			;; local file links are copied to blog directories
			(setq url (concat "https://sachachua.com"
												(org-entry-get-with-inheritance "EXPORT_ELEVENTY_PERMALINK")
												(replace-regexp-in-string
												 "[\\?&].*"
												 ""
												 (file-name-nondirectory link)))))
		(when (called-interactively-p 'any)
			(kill-new url)
			(message "Copied %s" url))
		url))

;;;###autoload
(defun my-embark-replace-link-with-exported-url (link &rest _)
	(interactive (list (org-element-property :raw-link (org-element-context))))
	(my-insert-or-replace-link (my-org-link-as-url link)))
;; Copy web link:2 ends here

;; [[file:../Sacha.org::#web-link][Copy web link:3]]
(with-eval-after-load 'embark-org
	(mapc (lambda (map)
					(keymap-set map "u" #'my-embark-org-copy-exported-url)
					(keymap-set map "U" #'my-embark-org-copy-exported-url-as-wayback)
					(keymap-set map "r e" #'my-embark-replace-link-with-exported-url))
				(list embark-url-map embark-org-link-map embark-org-link-copy-map)))
;; Copy web link:3 ends here

;; [[file:../Sacha.org::#org-mode-links-linking-to-headings-that-match-a-tag][Linking to headings that match a tag:1]]
;;;###autoload
(defun my-org-insert-matching-heading-links (match)
	(interactive "MMatch: ")
	(let ((org-tags-exclude-from-inheritance (list match)))
		(insert
		 (string-join
			(org-map-entries
			 (lambda ()
				 (concat "- " (org-link-make-string
											 (car (org-link--file-link-to-here))
											 (org-entry-get (point) "ITEM"))
								 (if (org-entry-get (point) "EXPORT_DATE")
										 (format-time-string " (%Y)"
																				 (date-to-time (org-entry-get (point) "EXPORT_DATE")))
									 "")
								 "\n"))
			 match)))
		""))
;; Linking to headings that match a tag:1 ends here

;; [[file:../Sacha.org::#org-dired][Dired:2]]
;;;###autoload
(defun my-org-get-links-in-region (beg end)
  (save-excursion
    (let (results)
      (goto-char (min beg end))
      (while (re-search-forward org-any-link-re (max beg end) t)
        (add-to-list 'results (org-element-context)))
      results)))

;;;###autoload
(defun my-org-dired-file-links-in-region (beg end)
  "Display a Dired buffer for the file links in the selected region."
  (interactive "r")
  (let ((files
         (-map
          (lambda (x)
            (expand-file-name (org-link-unescape (plist-get (cadr x) :path))))
          (-filter
           (lambda (x)
             (string= (plist-get (cadr x) :type) "file"))
           (my-org-get-links-in-region beg end)))))
    (with-current-buffer (get-buffer-create "*Files*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (apply 'call-process "ls" nil t nil "-lR" files))
      (dired-virtual "/")
      (switch-to-buffer (current-buffer)))))
;; Dired:2 ends here

;; [[file:../Sacha.org::#add-custom-id][Speed command for adding a custom ID to Org Mode posts:1]]
;;;###autoload
(defun my-make-slug (s)
  (thread-last s
               (my-translate-unaccented)
               (downcase)
               (replace-regexp-in-string "[^a-z0-9]+" "-")
               (replace-regexp-in-string "^-\\|-$" "")))

;;;###autoload
(defun my-org-set-custom-id (id)
  "Set the CUSTOM_ID property to ID at point."
  (interactive (list
                (let ((default-custom-id (my-make-slug (string-join (org-get-outline-path t) " "))))
                  (read-string (format "ID (%s): " default-custom-id) nil nil default-custom-id))))
  (org-entry-put (point) "CUSTOM_ID" id))
;; Speed command for adding a custom ID to Org Mode posts:1 ends here

;; [[file:../Sacha.org::#counting][Counting:1]]
;;;###autoload
(defun my-org-summarize-task-status ()
  "Count number of tasks by status.
      Probably should make this a dblock someday."
  (interactive)
  (let (result)
    (org-map-entries
     (lambda ()
       (let ((todo (elt (org-heading-components) 2)))
         (if todo
             (if (assoc todo result)
                 (setcdr (assoc todo result)
                         (1+ (cdr (assoc todo result))))
               (setq result (cons (cons todo 1) result)))))))
    (message "%s" (mapconcat (lambda (x) (format "%s: %d" (car x) (cdr x)))
                             result "\n"))))
;; Counting:1 ends here

;; [[file:../Sacha.org::#spreadsheets][Spreadsheets:1]]
;;;###autoload
(defun my-org-days-between (start end)
  "Number of days between START and END (exclusive).
      This includes START but not END."
  (- (calendar-absolute-from-gregorian (org-date-to-gregorian end))
     (calendar-absolute-from-gregorian (org-date-to-gregorian start))))
;; Spreadsheets:1 ends here

;; [[file:../Sacha.org::#copying-and-sharing-code][Copying and sharing code:1]]
;;;###autoload
(defun my-copy-code-as-org-block-and-gist (beg end)
  (interactive "r")
  (let ((filename (or (file-name-base) ""))
        (mode (symbol-name major-mode))
        (contents
         (if (use-region-p) (buffer-substring beg end) (buffer-string)))
        (gist (if (use-region-p) (gist-region beg end) (gist-buffer))))
    (kill-new
     (format "\n%s\n#+begin_src %s\n%s\n#+end_src\n"
             (org-link-make-string (oref (oref gist :data) :html-url) filename)
             (replace-regexp-in-string "-mode$" "" mode)
             contents))))
;; Copying and sharing code:1 ends here

;; [[file:../Sacha.org::#tables][Tables:1]]
;;;###autoload
(defun my-org-table-as-alist (table)
  "Convert TABLE to an alist. Remember to set :colnames no."
  (let ((headers (seq-map 'intern (car table))))
    (cl-loop for x in (cdr table) collect (-zip headers x))))
;; Tables:1 ends here

;; [[file:../Sacha.org::#invoices][Invoices:2]]
;;;###autoload
(defun my-org-get-invoice-range-based-on-date (date)
  (let* ((invoice-date (org-date-to-gregorian date))
         (start (list (1- (car invoice-date)) 1 (elt invoice-date 2)))
         (end (list (car invoice-date) 1 (elt invoice-date 2))))
    (mapcar (lambda (date)
              (format-time-string "%F %H:%M" (encode-time 0 0 0 1 (elt date 0) (elt date 2))))
            (list start end))))

;;;###autoload
(defun my-org-quantified-get-hours-based-on-range (category start end)
  "Return the number of hours for the specified category."
  (/ (assoc-default category
                    (quantified-summarize-time start end)) 3600.0))

;; TODO: paginate
;;;###autoload
(defun my-org-quantified-get-detailed-hours-based-on-range (category start end)
  "Return a list of (date week-ending-date dow seconds) for CATEGORY from START to END."
  (let ((entries
         (quantified-parse-json
           (quantified-request (format "records.json?start=%s&end=%s&filter_string=%s&per_page=1000&split=split" start end (url-encode-url category))
                               nil "GET"))))
    (mapcar
     (lambda (entry)
       (let ((time (date-to-time (assoc-default 'timestamp entry))))
         (list
          (format-time-string "%F" time)
          (format-time-string "%F" (my-get-week-end-for-time time))
          (format-time-string "%a" time)
          (assoc-default 'duration entry))))
     entries)))

;;;###autoload
(defun my-get-week-end-for-time (time &optional week-ends-on-day)
  "WEEK-ENDS-ON-DAY: 0 is Sunday"
  (let* ((decoded (decode-time time))
         (dow (elt decoded 6))
         (end-week (or week-ends-on-day (% (+ 6 calendar-week-start-day) 7))))
    (encode-time
     (elt decoded 0)
     (elt decoded 1)
     (elt decoded 2)
     (+ (elt decoded 3)
        (% (+ 7 (- end-week dow)) 7))
     (elt decoded 4)
     (elt decoded 5))))

(ert-deftest my-org-get-week-ending-date ()
  (let ((calendar-week-start-day 6)
        (tests '(
                 ("2015-09-03" . "2015-09-04")
                 ("2015-12-01" . "2015-12-04")
                 ("2015-12-03" . "2015-12-04")
                 ("2015-12-04" . "2015-12-04")
                 ("2015-12-05" . "2015-12-11"))))
    (dolist (test tests)
      (should (string=
               (format-time-string
                "%F"
                (my-get-week-end-for-time (org-time-string-to-time (car test))))
               (cdr test)))
      (should (string=
               (format-time-string
                "%F"
                (my-get-week-end-for-time (org-time-string-to-time (car test)) 5))
               (cdr test))))))



;;;###autoload
(defun my-org-quantified-format-detailed-hours-as-table (list)
  "Return a table with rows for LIST.
        | Week ending ____ | Sat | Sun | Mon | Tue | Wed | Thu | Fri | Total |
        LIST elements should be in the form (date week-end-date dow seconds).
        See `my-org-quantified-get-detailed-hours-based-on-range'."
  ;; Group by week ending date
  (let ((days '("Sat" "Sun" "Mon" "Tue" "Wed" "Thu" "Fri")))
    (append
     (list (append '("Week ending") days '("Total")))
     (mapcar
      (lambda (row)
        (let ((day-values (-group-by (lambda (x) (elt x 2)) (cdr row)))
              (week-total 0))
          (append
           (list (format "Week ending %s" (format-time-string "%b %-e" (org-time-string-to-time (car row)))))
           (mapcar (lambda (day)
                     (if (assoc-default day day-values)
                         (format "%.1f"
                                 (apply '+
                                        (mapcar
                                         (lambda (day-val) (/ (elt day-val 3) 3600.0))
                                         (assoc-default day day-values))))
                       ""))
                   days)
           (list (format "%.1f"
                         (apply '+ (mapcar (lambda (day-val) (/ (elt day-val 3) 3600.0)) (cdr row)))))
           ))
        )
      (-sort (lambda (a b) (string< (car a) (car b))) (-group-by (lambda (x) (elt x 1)) list))))))


;;;###autoload
(defun my-org-quantified-hours-table ()
  (my-org-quantified-format-detailed-hours-as-table
   (apply 'my-org-quantified-get-detailed-hours-based-on-range
          (org-entry-get-with-inheritance "QUANTIFIED_CATEGORY")
          (my-org-get-invoice-range-based-on-date (org-entry-get-with-inheritance "INVOICE_DATE")))))

(ert-deftest my-org-get-invoice-range-based-on-date ()
  "Check if invoice range is sane."
  (should (equal (my-org-get-invoice-range-based-on-date "2015-12-05")
                 '("2015-11-01 00:00" "2015-12-01 00:00"))))
;; Invoices:2 ends here

;; [[file:../Sacha.org::counting-words-in-notes][counting-words-in-notes]]
(defvar my-org-note-words-target (* 140 20))
;;;###autoload
(defun my-org-collect-notes (&optional block-name)
	(let (results)
		(org-block-map
		 (lambda ()
			 (unless (org-in-commented-heading-p)
				 (let ((elem (org-element-at-point)))
					 (when (string= (downcase (org-element-property :type elem))
													(or block-name "notes"))
						 (push (string-trim
													(buffer-substring-no-properties
													 (org-element-property :contents-begin elem)
													 (org-element-property :contents-end elem)))
									 results))))))
		(reverse results)))

;;;###autoload
(defun my-org-count-words-in-notes (&optional target block-name)
	"Count words in #+begin_notes blocks.
If TARGET or `my-org-note-words-target' is specified, calculate percentage and words left.
If BLOCK-NAME is specified, use that block type instead."
	(interactive)
	(let ((notes (my-org-collect-notes)))
		(with-temp-buffer
			(insert (string-join notes "\n"))
			(let ((num (count-words-region (point-min) (point-max))))
				(if (or target my-org-note-words-target)
						(message "%d words (%.f%% of %d, %d to go)"
										 num
										 (/ (* 100.0 num) my-org-note-words-target)
										 my-org-note-words-target
										 (- my-org-note-words-target num))
					(message "%d words" num))))))

;;;###autoload
(defun my-org-create-notes-buffer ()
	(interactive)
	(let ((notes (my-org-collect-notes)))
		(with-current-buffer (get-buffer-create "*Notes*")
			(insert (string-join notes "\n\n"))
			(switch-to-buffer (current-buffer)))))
;; counting-words-in-notes ends here

;; [[file:../Sacha.org::#allow-dashes-in-tags][Allow dashes in tags:1]]
;;;###autoload
(defun my-org-add-dashes-to-tag-regexps ()
  (setq org-complex-heading-regexp
        (concat "^\\(\\*+\\)"
                "\\(?: +" org-todo-regexp "\\)?"
                "\\(?: +\\(\\[#.\\]\\)\\)?"
                "\\(?: +\\(.*?\\)\\)??"
                "\\(?:[ \t]+\\(:[-[:alnum:]_@#%:]+:\\)\\)?"
                "[ \t]*$")
        org-complex-heading-regexp-format
        (concat "^\\(\\*+\\)"
                "\\(?: +" org-todo-regexp "\\)?"
                "\\(?: +\\(\\[#.\\]\\)\\)?"
                "\\(?: +"
                ;; Stats cookies can be stuck to body.
                "\\(?:\\[[0-9%%/]+\\] *\\)*"
                "\\(%s\\)"
                "\\(?: *\\[[0-9%%/]+\\]\\)*"
                "\\)"
                "\\(?:[ \t]+\\(:[-[:alnum:]_@#%%:]+:\\)\\)?"
                "[ \t]*$")
        org-todo-line-tags-regexp
        (concat "^\\(\\*+\\)"
                "\\(?: +" org-todo-regexp "\\)?"
                "\\(?: +\\(.*?\\)\\)??"
                "\\(?:[ \t]+\\(:[-[:alnum:]:_@#%]+:\\)\\)?"
                "[ \t]*$")))
;; Allow dashes in tags:1 ends here

;; [[file:../Sacha.org::#org-mode-convert-from-markdown][Convert from Markdown:1]]
;;;###autoload
(defun my-org-convert-region-from-markdown (beg end)
	(interactive "r")
	(shell-command-on-region beg end "pandoc -t org" nil t))
;; Convert from Markdown:1 ends here

;; [[file:../Sacha.org::#copying-information-from-my-phone][Copying information from my phone:1]]
;;;###autoload
(defun my-read-phone-entries ()
  "Copy phone data to a summary Org file."
  (interactive)
  (mapc
   (lambda (filename)
     (let ((base (file-name-base filename)) contents timestamp category encoded-time date)
       (when (string-match "^[^ ]+ [^ ]+ \\([^ ]+\\) - \\(.*\\)" base)
         (setq time (seconds-to-time (/ (string-to-number (match-string 1 base)) 1000))
               encoded-time (decode-time time)
               date (list (elt encoded-time 4) (elt encoded-time 3) (elt encoded-time 5))
               category (match-string 2 base))
         (with-temp-buffer
           (insert-file-contents filename)
           (setq contents (s-trim (buffer-string))))
         (with-current-buffer
             (find-file "~/dropbox/tasker/summary.txt")
           (org-datetree-find-date-create date)
           (unless (save-excursion (re-search-forward (regexp-quote base) nil t))
             (goto-char (line-end-position))
             (insert "\n")
             (insert "**** " contents "  :" category ":\n" base "\n")
             (insert (format-time-string "[%Y-%m-%d %a %H:%M]\n" time))

             (if (member category '("Think" "Do"))
                 (save-excursion
                   (org-back-to-heading t)
                   (if (looking-at org-outline-regexp) (goto-char (1- (match-end 0))))
                   (unless (looking-at org-todo-regexp)
                     (org-todo "TODO"))))
             (if (string-match "^Energy \\([0-9]\\)" contents)
                 (org-set-property "ENERGY" (match-string 1 contents)))))
         (delete-file filename))))
   (directory-files "~/dropbox/tasker/data" t "\\.txt$")))
;; Copying information from my phone:1 ends here

;; [[file:../Sacha.org::#reddit][Reddit:1]]
;;;###autoload
(defun my-reddit-list-upvoted (date)
  (interactive (list (org-read-date)))
  (let ((threshold (org-read-date nil t (concat (substring date 0 (min (length date) 10)) " 0:00")))
        (url my-reddit-upvoted-json)
        results)
    (while url
      (with-current-buffer (url-retrieve-synchronously url)
        (goto-char (point-min))
        (re-search-forward "^$")
        (let* ((data (json-read))
               (items (assoc-default 'children (assoc-default 'data data)))
               (after (assoc-default 'after (assoc-default 'data data)))
               (result
                (mapconcat
                 (lambda (item)
                   (let* ((o (assoc-default 'data item))
                          (title (assoc-default 'title o))
                          (url (helm-html-decode-entities-string (assoc-default 'url o)))
                          (date (seconds-to-time (assoc-default 'created_utc o)))
                          (permalink (concat "https://reddit.com" (assoc-default 'permalink o)))
                          (num-comments (assoc-default 'num_comments o 'eq 0)))
                     (when (time-less-p threshold date)
                       (if (and (> num-comments 0) (not (string-match "reddit\\.com" url)))
                           (format "- %s (%s)\n"
                                   (org-link-make-string (url-unhex-string url) title)
                                   (org-link-make-string (url-unhex-string permalink) "Reddit"))
                         (format "- %s\n" (org-link-make-string (url-unhex-string url) title))))))
                 items "")))

          (setq results (concat result "\n" results))
          (setq url
                (if (and after (> (length result) 0))
                    (concat my-reddit-upvoted-json "&after=" after)
                  nil)))))
    results))
;;  (my-reddit-list-upvoted "-mon")
;; Reddit:1 ends here

;; [[file:../Sacha.org::#sorting-org-mode-lists-using-a-sequence-of-regular-expressions][Sorting Org Mode lists using a sequence of regular expressions:1]]
;;;###autoload
(defun my-org-sort-list-in-custom-order (order)
  "Sort the current Org list so that items are in the specified order.
       ORDER is a list of regexps."
  (org-sort-list
   nil ?f
   (lambda ()
     (let ((case-fold-search t)
           (item
            (when (looking-at "[ \t]*[-+*0-9.)]+\\([ \t]+\\[[- X]\\]\\)?[ \t]+")
              (org-sort-remove-invisible (buffer-substring (match-end 0) (point-at-eol))))))
       (or (cl-position item order :test (lambda (a b) (string-match b a))) (1+ (length order)))))
   '<))
;; Sorting Org Mode lists using a sequence of regular expressions:1 ends here

;; [[file:../Sacha.org::#clipboard][Clipboard:1]]
;;;###autoload
(defun my-org-insert-clipboard ()
  "Convert clipboard contents from HTML to Org and then paste (yank)."
  (interactive)
  (insert (shell-command-to-string "xclip -o -selection clipboard -t text/html | pandoc -f html -t json | pandoc -f json -t org")))

;;;###autoload
(defun my-org-insert-clipboard-without-data-images ()
  "Convert clipboard contents from HTML to Org and then insert, but replace images with placeholders"
  (interactive)
	(insert
	 (with-temp-buffer
		 (shell-command "xclip -o -selection clipboard -t text/html" (current-buffer))
		 (goto-char (point-min))
		 (while (re-search-forward "<img src='data:image[^>]*?'>" nil t)
			 (replace-match "{{{ image }}}"))
		 (buffer-string))))

;;;###autoload
(defun my-org-convert-clipboard-to-org-without-data-images ()
  "Convert clipboard contents from HTML to Org and then insert, but replace images with placeholders"
  (interactive)
	(kill-new
	 (with-temp-buffer
		 (shell-command "xclip -o -selection clipboard -t text/html" (current-buffer))
		 (goto-char (point-min))
		 (while (re-search-forward "<img src='data:image[^>]*?'>" nil t)
			 (replace-match "{{{ image }}}"))
		 (goto-char (point-min))
		 (while (re-search-forward "<\\(tr\\)>" nil t)
			 (replace-match "{{{ image }}}"))
		 (buffer-string))))
;; Clipboard:1 ends here

;; [[file:../Sacha.org::#setting-properties][Setting properties:1]]
;;;###autoload
(defun my-org-set-property (property value)
  "In the current entry, set PROPERTY to VALUE.
Use the region if active."
  (interactive
	 (list
		(org-read-property-name)
    (when (region-active-p)
			(replace-regexp-in-string
			 "[ \n\t]+" " "
			 (buffer-substring (point) (mark))))))
  (org-set-property property value))
;; Setting properties:1 ends here

;; [[file:../Sacha.org::#org-send-things-to-the-bottom-of-the-list][Org - send things to the bottom of the list:1]]
;;;###autoload
(defun my-org-send-to-bottom-of-list ()
  "Send the current line to the bottom of the list."
  (interactive)
  (beginning-of-line)
  (let ((kill-whole-line t))
    (save-excursion
      (kill-line 1)
      (org-end-of-item-list)
      (yank))))
;; Org - send things to the bottom of the list:1 ends here

;; [[file:../Sacha.org::#org-mode-org-mode-format-libby-book-highlights-exported-as-json][Org Mode: Format Libby book highlights exported as JSON:2]]
;;;###autoload
(defun my-org-insert-book-highlights-from-libby (url)
	(interactive "MURL: ")
	(let-alist (plz 'get url :as #'json-read)
		(insert
		 "* "
		 .readingJourney.title.text
		 " - "
		 .readingJourney.author
		 "\n")
		(org-set-property "ISBN" .readingJourney.isbn)
		(org-set-property "COVER" .readingJourney.cover.url)
		(org-set-property "TITLE" .readingJourney.title.text)
		(org-set-property "AUTHOR" .readingJourney.author)
		(insert (org-link-make-string .readingJourney.title.url .readingJourney.cover.url)
						"\n")
		;; sort the highlights by chapter
		(insert
		 (mapconcat
			(lambda (row)
				(concat "** " (replace-regexp-in-string " +" " " (car row)) "\n"
								(mapconcat (lambda (quote)
														 (concat "#+begin_quote\n"
																		 (alist-get 'quote quote)
																		 "\n#+end_quote\n\n"))
													 (cdr row)
													 "")
								"\n\n"))
			(seq-group-by
			 (lambda (o) (alist-get 'chapter o))
			 (sort .highlights
						 :key (lambda (o) (alist-get 'percent o))))))))
;; Org Mode: Format Libby book highlights exported as JSON:2 ends here

;; [[file:../Sacha.org::org-copy-link][org-copy-link]]
;;;###autoload
(defun my-org-copy-export (link desc format)
	(pcase format
		('org (org-link-make-string (concat "copy:" link) desc))
		(_ desc)))
;; org-copy-link ends here

;; [[file:../Sacha.org::#digital-index-piles-with-emacs][Digital index piles with Emacs:1]]
;;;###autoload
      (defun my-org-get-list-categories ()
        "Return a list of (category indent matching-regexp sample).
              List categories are items that don't contain links."
        (let ((list (org-list-struct)) last-category results)
          (save-excursion
            (mapc
             (lambda (x)
               (goto-char (car x))
               (let ((current-item
                      (buffer-substring-no-properties
                       (+ (point)
                          (elt x 1)
                          (length (elt x 2)))
                       (line-end-position))))
                 (if (string-match
                      org-link-bracket-re
                      (buffer-substring-no-properties
                       (point)
                       (line-end-position)))
                     ;; Link - update the last category
                     (when last-category
                       (if (< (elt x 1) (elt last-category 1))
                           (setq results
                                 (cons (append last-category
                                               (list
                                                (match-string-no-properties
                                                 3
                                                 (buffer-substring-no-properties
                                                  (point)
                                                  (line-end-position)))))
                                       (cdr results))))
                       (setq last-category nil))
                   ;; Category
                   (setq results
                         (cons
                          (setq last-category
                                (list
                                 current-item
                                 (elt x 1)
                                 (concat "^"
                                         (make-string (elt x 1) ?\ )
                                         (regexp-quote
                                          (concat (elt x 2)
                                                  current-item))
                                         "$")))
                          results)))))
             list))
          (append '(("x" 2 "^$" nil)) results)))
;; Digital index piles with Emacs:1 ends here

;; [[file:../Sacha.org::my-org-move-current-item-to-category][my-org-move-current-item-to-category]]
;;;###autoload
(defun my-org-move-current-item-to-category (category)
    "Move current list item under CATEGORY earlier in the list.
  CATEGORY can be a string or a list of the form (text indent regexp).
  Point should be on the next line to process, even if a new category
  has been inserted."
    (interactive (list (completing-read "Category: " (my-org-get-list-categories))))
    (when category
      (let* ((col (current-column))
             (item (point-at-bol))
             (struct (org-list-struct))
             (category-text (if (stringp category) category (elt category 0)))
             (category-indent (if (stringp category) 2 (+ 2 (elt category 1))))
             (category-regexp (if (stringp category) category (elt category 2)))
             (end (elt (car (last struct)) 6))
             (pos (point))
             s)
        (setq s (org-remove-indentation (buffer-substring-no-properties item (org-list-get-item-end item struct))))
        (save-excursion
          (if (string= category-text "x")
              (org-list-send-item item 'delete struct)
            (goto-char (caar struct))
            (if (re-search-forward (concat "^ *- +" category-regexp) end t)
                (progn
                  ;; needs a patch to ol.el to check if stringp
                  (org-list-send-item item (point-at-bol) struct)
                  (org-move-item-down)
                  (org-indent-item))
              (goto-char end)
              (org-list-insert-item
               (point-at-bol)
               struct (org-list-prevs-alist struct))
              (let ((old-struct (copy-tree struct)))
                (org-list-set-ind (point-at-bol) struct 0)
                (org-list-struct-fix-bul struct (org-list-prevs-alist struct))
                (org-list-struct-apply-struct struct old-struct))
              (goto-char (point-at-eol))
              (insert category-text)
              (org-list-send-item item 'end struct)
              (org-indent-item)
              (org-indent-item))
            (recenter))))))

;;;###autoload
(defun my-org-guess-list-category (&optional categories)
  (interactive)
  (require 'cl-lib)
  (unless categories
    (setq categories
          (my-helm-org-list-categories-init-candidates)))
  (let* ((beg (line-beginning-position))
         (end (line-end-position))
         (string (buffer-substring-no-properties beg end))
         (found
          (cl-member string
                     categories
                     :test
                     (lambda (string cat-entry)
                       (unless (string= (car cat-entry) "x")
                         (string-match (regexp-quote (downcase (car cat-entry)))
                                       string))))))
    (when (car found)
      (my-org-move-current-item-to-category
       (cdr (car found)))
      t)))
;; my-org-move-current-item-to-category ends here

;; [[file:../Sacha.org::#digital-index-piles-with-emacs][Digital index piles with Emacs:4]]
(defvar my-org-browse-link-while-categorizing 'eww-readable
  "Set to nil to skip browsing.")

;;;###autoload
(defun my-org-guess-uncategorized ()
  "Interactively move linked list items to categories from the list.
        Try to guess categories based on substring matches."
  (interactive)
                                        ;(my-helm-org-list-categories-init-candidates)
  (let ((categories (my-org-get-list-categories))
        category)
    (while (and (looking-at "^[-+] \\[\\[\\([^]]+\\)\\]\\[\\([^]]+*\\)")
                (not (string= "done" category)))
      (save-excursion
        ;; (when (eq my-org-browse-link-while-categorizing 'eww-readable)
        ;;   (save-excursion (save-match-data (my-eww-browse-readable (match-string 1)))))
        (setq category (completing-read (match-string 2) categories))
        (unless (string= category "done")
          (my-org-move-current-item-to-category category))))))

;; From https://emacs.stackexchange.com/questions/36284/how-to-open-eww-in-readable-mode/47757
;;;###autoload
(defun my-eww-readable-nonce ()
  "Once-off call to `eww-readable' after EWW is done rendering."
  (unwind-protect
      (eww-readable)
    (remove-hook 'eww-after-render-hook #'my-eww-readable-nonce)))

;;;###autoload
(defun my-eww-browse-readable (url)
  (when (looking-at "^[-+] \\[\\[\\([^]]+\\)")
    (add-hook 'eww-after-render-hook #'my-eww-readable-nonce)
    (eww (match-string 1))))

;; Digital index piles with Emacs:4 ends here

;; [[file:../Sacha.org::#digital-index-piles-with-emacs][Digital index piles with Emacs:5]]
;;;###autoload
(defun my-org-sort-list-by-regexp (regexp)
  (interactive "MRegexp: ")
  (let ((sort-func
         (lambda ()
           (let ((line (buffer-substring-no-properties (point) (line-end-position))))
             (if (string-match regexp line)
                 (if (string-match org-link-bracket-re line)
                     (match-string 2 line)
                   "ZZZ")
               "ZZZZZ")))))
    (funcall
     (cond
      ((org-at-table-p) 'org-table-sort-lines)
      ((org-at-item-p) 'org-sort-list)
      (t 'org-sort-entries))
     nil ?f sort-func (lambda (a b) (if (and (stringp a) (stringp b)) (string< a b) t)))))
;; Digital index piles with Emacs:5 ends here

;; [[file:../Sacha.org::#multimedia-subtitles-with-subed-simplify-inserting-audio-links][Simplify inserting audio links:2]]
;;;###autoload
(defun my-org-next-item-or-paragraph (&optional by-sentence)
  (cond
   ((org-in-item-p)
    (condition-case nil
        (progn
          (org-next-item)
          (when (looking-at org-list-full-item-re)
            (goto-char (match-end 0))))
      (error nil)))
   (by-sentence (forward-sentence))
   (t
    (forward-paragraph)
    (skip-syntax-forward " ")
    (when (looking-at org-list-full-item-re)
      (goto-char (match-end 0)))
    ; Move to start of a list item
    (when (looking-at org-heading-regexp)
      (org-end-of-meta-data t))
    (skip-syntax-forward " "))))
;; Simplify inserting audio links:2 ends here

;; [[file:../Sacha.org::#mastodon-news][Collecting Emacs News from Mastodon:4]]
;;;###autoload
(defun my-org-link-url-from-string (s)
	"Return the link URL from S."
	(if (string-match org-link-any-re s)
			(or
			 (match-string 7 s)
			 (match-string 2 s))))
;; Collecting Emacs News from Mastodon:4 ends here

;; [[file:../Sacha.org::#clock-in][Quantified Awesome:1]]
(require 'quantified nil t)

(defmacro my-org-with-current-task (&rest body)
  "Execute BODY with the point at the subtree of the current task."
  (declare (debug t))
  `(if (derived-mode-p 'org-agenda-mode)
       (save-window-excursion
         (org-agenda-switch-to)
         ,@body)
     ,@body))

;;;###autoload
(defun my-org-clock-in-and-track ()
  "Start the clock running. Clock into Quantified Awesome."
  (interactive)
  (my-org-with-current-task
   (org-clock-in)
   (call-interactively 'my-org-quantified-track)
   ;(when (websocket-openp obs-websocket)  (my-stream-message (org-get-heading t t t t)))
   (cond
    ((org-entry-get (point) "AUTO")
     (org-link-open-from-string (org-entry-get (point) "AUTO")))
    (t
     (save-restriction
       (org-narrow-to-subtree)
       (org-next-link)
       (when (looking-at org-link-any-re)
         (org-open-at-point)))))))

(defmacro my-with-org-task (&rest body)
  "Run BODY within the current agenda task, clocked task, or cursor task."
  `(cond
    ((derived-mode-p 'org-agenda-mode)
     (let* ((marker (org-get-at-bol 'org-marker))
            (buffer (marker-buffer marker))
            (pos (marker-position marker)))
       (with-current-buffer buffer
         (save-excursion
           (save-restriction
             (widen)
             (goto-char pos)
             ,@body)))))
    ((and (derived-mode-p 'org-mode) (org-at-heading-p)) (save-excursion ,@body))
    ((org-clocking-p) (save-excursion (org-clock-goto) ,@body))
    ((derived-mode-p 'org-mode) ,@body)))

(defvar my-org-quantified-regexps
	'(("emacsconf" . "Emacs | Emacsconf")
		("emacs" . "Emacs")
		("consulting" . "E1 Gen"))
	"Alist of regexp . category.")

;;;###autoload
(defun my-org-quantified-track (&optional category note)
  "Create a tracking record using CATEGORY and NOTE.
      Default to the current task in the agenda, the currently-clocked
      entry, or the current subtree in Org."
  (interactive (list nil nil))
  (unless (and category note)
    (my-with-org-task
     (setq category (or category
                        (org-entry-get-with-inheritance "QUANTIFIED")))
     (cond
      ((null category)
			 (let* ((heading (org-get-heading))
							(guess (seq-find (lambda (entry)
																 (string-match (car entry)
																							 heading))
															 my-org-quantified-regexps)))
				 (setq category (or (cdr guess) (read-string "Category: "))))
       (org-set-property "QUANTIFIED" category))
      ((string= category ' "ask")
       (setq category (read-string "Category: "))))
     (setq note
           (concat
            (if (string= (or (org-entry-get-with-inheritance "QUANTIFIEDQUIET") "") "t")
                "!private "
              "")
            (or note (elt (org-heading-components) 4) (read-string "Note: "))))))
  (quantified-track (concat category " | " note)))

;;;###autoload
(defun my-org-quick-clock-in-task (location jump)
  "Track and clock in on the specified task.
      If JUMP is non-nil or the function is called with the prefix argument, jump to that location afterwards."
  (interactive (list (save-excursion (my-org-refile-get-location "Location")) current-prefix-arg))
  (when location
    (if jump
        (progn (org-refile 4 nil location) (my-org-clock-in-and-track))
      (save-window-excursion
        (org-refile 4 nil location)
        (my-org-clock-in-and-track)))))

;; Quantified Awesome:1 ends here

;; [[file:../Sacha.org::#compare-time][Compare times and effort estimates:1]]
;;;###autoload
(defun my-compare-times (clocked estimated)
  (if (and (> (length clocked) 0) estimated)
      (format "%.2f"
              (/ (* 1.0 (org-hh:mm-string-to-minutes clocked))
                 (org-hh:mm-string-to-minutes estimated)))
    ""))
;; Compare times and effort estimates:1 ends here

;; [[file:../Sacha.org::#list-upcoming-tasks-so-that-i-can-see-if-i-m-overloaded][List upcoming tasks so that I can see if I'm overloaded:1]]
;;;###autoload
(defun my-org-summarize-upcoming-week ()
  "Summarize upcoming tasks as a list."
  (interactive)
  (org-agenda nil "w")
  (let ((string (buffer-string))
        business relationships life)
    (with-temp-buffer
      (insert string)
      (goto-char (point-min))
      (while (re-search-forward my-weekly-review-line-regexp nil t)
        (cond
         ((string= (match-string 1) "routines") nil) ; skip routine tasks
         ((string= (match-string 1) "business")
          (add-to-list 'business (concat "  - [ ] " (match-string 3))))
         ((string= (match-string 1) "people")
          (add-to-list 'relationships (concat "  - [ ] " (match-string 3))))
         (t (add-to-list 'life (concat "  - [ ] " (match-string 3)))))))
    (setq string
          (concat
           "*Plans for next week*\n"
           "- Business\n"
           (mapconcat 'identity business "\n")
           "\n- Relationships\n"
           (mapconcat 'identity relationships "\n")
           "\n- Life\n"
           (mapconcat 'identity life "\n")))
    (if (called-interactively-p 'any)
        (kill-new string)
      string)))
;; List upcoming tasks so that I can see if I'm overloaded:1 ends here

;; [[file:../Sacha.org::#list-upcoming-tasks-so-that-i-can-see-if-i-m-overloaded][List upcoming tasks so that I can see if I'm overloaded:2]]
;;;###autoload
(defun my-org-summarize-previous-week ()
  "Summarize previously-completed tasks as a list."
  (interactive)
  (save-window-excursion
    (org-agenda nil "w")
    (org-agenda-later -1)
    (org-agenda-log-mode 16)
    (let ((string (buffer-string))
          business relationships life)
      (with-temp-buffer
        (insert string)
        (goto-char (point-min))
        (while (re-search-forward my-weekly-review-line-regexp nil t)
          (cond
           ((string= (match-string 1) "routines") nil) ; skip routine tasks
           ((string= (match-string 1) "business")
            (add-to-list 'business (concat "  - " (match-string 2))))
           ((string= (match-string 1) "people")
            (add-to-list 'relationships (concat "  - " (match-string 2))))
           (t (add-to-list 'life (concat "  - " (match-string 2)))))))
      (setq string
            (concat
             "*Accomplished this week*\n\n"
             "- Business\n"
             (mapconcat 'identity business "\n")
             "\n- Relationships\n"
             (mapconcat 'identity relationships "\n")
             "\n- Life\n"
             (mapconcat 'identity life "\n")))
      (if (called-interactively-p 'any)
          (kill-new string)
        string))))

;; List upcoming tasks so that I can see if I'm overloaded:2 ends here

;; [[file:../Sacha.org::#streaming-send-currently-clocked-task-title-to-file-include-in-stream][Send currently-clocked task title to file, include in stream:1]]
;;;###autoload
(defun my-org-save-streaming-task-to-file ()
  (if (member "stream" (org-get-tags))
      (let ((title (org-entry-get (point) "ITEM")))
        (with-temp-file "~/proj/stream/current-task.txt"
          (insert title)))
    (with-temp-file "~/proj/stream/current-task.txt")))

;;;###autoload
(defun my-org-clear-streaming-task ()
  (with-temp-file "~/proj/stream/current-task.txt"))

;; Send currently-clocked task title to file, include in stream:1 ends here

;; [[file:../Sacha.org::#show-emacs-related-tasks][Show Emacs-related tasks:1]]
;;;###autoload
(defun my-show-emacs-tasks ()
  (interactive)
  (org-ql-search (org-agenda-files)
    '(and (todo)
          (parent (and (or (tags "stream")
                           (tags "project"))
                       (tags "emacs")
                       (not (tags "inactive")))))
    :title "Emacs-related project tasks"
    :sort '(date priority todo)
    :super-groups '((:auto-parent t))))
;; Show Emacs-related tasks:1 ends here

;; [[file:../Sacha.org::#shopping][Comparison-shopping with Org Mode:2]]
(defvar my-get-shopping-details-functions
	'(my-org-shopping-get-details-from-spookfox
		my-get-shopping-details-amazon
		my-get-shopping-details-uniqlo
		my-get-shopping-details-manually))

;;;###autoload
(defun my-get-shopping-details-manually (link)
	(when (string-match "theshoecompany\\|dsw" link)
		(browse-url link)
		(list
		 (cons 'url link)
		 (cons 'image (read-string "Image: "))
		 (cons 'price (read-string "Price: ")))))

;;;###autoload
(defun my-get-shopping-details-amazon (link)
	(when (string-match "amazon.ca" link)
		(with-current-buffer (url-retrieve-synchronously link)
			(goto-char (point-min))
			(re-search-forward "^$")
			(let ((doc (libxml-parse-html-region (point) (point-max))))
				(list (cons 'name (dom-text (dom-by-tag doc 'title)))
							(cons 'description (dom-texts (dom-by-id doc "productDescription")))
							(cons 'image (dom-attr (dom-by-tag (dom-by-id doc "imgTagWrapperId") 'img) 'src))
							(cons 'price
										(dom-texts (dom-by-id doc "priceblock_ourprice"))))))))

;;;###autoload
(defun my-shopping-reformat-ld-data (data)
  (let-alist data
    (list (cons 'name .name)
          (cons 'url (or .url .@id))
          (cons 'brand .brand.name)
          (cons 'description .description)
          (cons 'rating .aggregateRating.ratingValue)
          (cons 'ratingCount .aggregateRating.reviewCount)
          (cons 'image (cond
												((stringp .image) .image)
												((stringp .image.url) .image.url)
												(t (elt .image 0))))
          (cons 'price
                (assoc-default 'price (cond
																			 ((arrayp .offers)
																				(elt .offers 0))
																			 (t .offers)))))))

;;;###autoload
(defun my-get-shopping-details ()
  (goto-char (point-min))
  (let (data)
    (cond
     ((re-search-forward "  data-section-data
>" nil t)
      (setq data (json-read))
      (let-alist data
        (list (cons 'name .product.title)
              (cons 'brand .product.vendor)
              (cons 'description .product.description)
              (cons 'image (if (stringp .image) .image (concat "https:" .product.featured_image)))
              (cons 'price (/ .product.price 100.0)))))
     ((and (re-search-forward "<script type=\"application/ld\\+json\">" nil t)
           (null (re-search-forward "Fabric Fabric\\|angryballerinafabrics" nil t))) ; Carter's, Columbia?
      (setq data (json-read))
      (if (vectorp data) (setq data (elt data 0)))
      (if (assoc-default '@graph data)
          (setq data (assoc-default '@graph data)))
      (if (vectorp data) (setq data (elt data 0)))
			(my-shopping-reformat-ld-data data))
		 (t
			(goto-char (point-min))
      (re-search-forward "^$")
      (let* ((doc (libxml-parse-html-region (point) (point-max)))
						 (result
							`((name . ,(string-trim (dom-text (dom-by-tag doc "title"))))
								(description . ,(string-trim (dom-text (dom-by-tag doc "title")))))
							))
        (mapc (lambda (property)
                (let ((node
											 (dom-search
												doc
												(lambda (o)
													(delq nil
																(mapcar (lambda (p)
																					(or (string= (dom-attr o 'property) p)
																							(string-match p (or (dom-attr o 'class) ""))))
																				(cdr property)))))))
									(when node (add-to-list 'result (cons (car property)
																												(or (dom-attr node 'content)
																														(string-trim (dom-text node))))))))
              '((name "og:title" "pdp-product-title")
                (brand "og:brand")
                (url "og:url")
                (image "og:image")
                (description "og:description")
                (price "og:price:amount" "product:price:amount" "pdp-price-label")))
				result)
			))))
;;;###autoload
(defun my-org-insert-shopping-details ()
  (interactive)
	(save-excursion
		(org-insert-heading)
		(when (string-match "^https://" (car kill-ring))
			(save-excursion (yank)))
		(my-org-update-shopping-details)
		(when (org-entry-get (point) "NAME")
			(org-edit-headline (org-entry-get (point) "NAME")))))

;;;###autoload
(defun my-org-shopping-get-details-from-spookfox (&optional link)
	"Get shopping details from Spookfox for sites that use Javascript."
	(let* ((current-url
					(spookfox-js-injection-eval-in-active-tab "window.location.href" t))
				 (data
					(when (or (not link)
										(string= link current-url))
						(json-parse-string
						 (spookfox-js-injection-eval-in-active-tab
							"JSON.stringify({
name: window.rawData?.Product?.Name || document.querySelector('meta[property=\"og:title\"]')?.getAttribute('content'),
image: document.querySelector('meta[property=\"og:image\"]')?.getAttribute('content'),
description: window.rawData?.Product?.Description || document.querySelector('meta[property=\"og:description\"]')?.getAttribute('content'),
url: document.querySelector('link[rel=\"canonical\"]')?.getAttribute('href') || window.location.href,
price: window.rawData?.Product?.MinPrice || document.querySelector('.nl-price--total')?.textContent?.replace(/^\s*$/, '')
})"
							t)
						 :object-type 'alist))))
		(when (alist-get 'name data)
			data)))

;; (my-org-get-shopping-details-uniqlo "https://www.uniqlo.com/ca/en/products/E451023-000?colorCode=COL07&sizeCode=KSS020")
;;;###autoload
(defun my-org-update-shopping-details ()
  (interactive)
	(let (data)
		(if (and (< (point)
								(save-excursion (org-end-of-subtree)))
						 (re-search-forward org-link-any-re (save-excursion (org-end-of-subtree)) t))
				(let ((link (org-element-property :raw-link (org-element-context))))
					(setq
					 data
					 (or (run-hook-with-args-until-success 'my-get-shopping-details-functions link)
							 (with-current-buffer (url-retrieve-synchronously link)
								 (my-get-shopping-details)))))
			(setq data (my-org-shopping-get-details-from-spookfox)))
		(when data
			(let-alist data
				(org-entry-put (point) "NAME" \.name)
				(org-entry-put (point) "URL" \.url)
				(org-entry-put (point) "BRAND" \.brand)
				(org-entry-put (point) "DESCRIPTION" (replace-regexp-in-string "&#039;" "'" (replace-regexp-in-string "\n" " " (or \.description ""))))
				(org-entry-put (point) "IMAGE"
											 (if (string-match "https?" \.image)
													 \.image
												 (concat "https:" \.image)))
				(org-entry-put (point) "PRICE" (cond ((stringp \.price) \.price) ((numberp \.price) (format "%.2f" \.price)) (t "")))
				(if \.rating (org-entry-put (point) "RATING" (if (stringp \.rating) \.rating (format "%.1f" \.rating))))
				(if \.ratingCount (org-entry-put (point) "RATING_COUNT" (if (stringp \.ratingCount) \.ratingCount (number-to-string \.ratingCount))))))))

;;;###autoload
(defun my-org-format-shopping-subtree (&optional height large)
	(concat
	 "<style>body { max-width: 100% !important } #content { max-width: 100% !important } .item img { max-height: "
	 (or height "100px")
	 " } .item img:hover { max-height: " (or large "400px") " }</style><div style=\"display: flex; flex-wrap: wrap; align-items: flex-start\">"
	 (string-join
		(save-excursion
			(org-map-entries
			 (lambda ()
				 (if (org-entry-get (point) "URL")
						 (format
							"<div class=item style=\"width: %s\"><div><a href=\"%s\"><img src=\"%s\" height=100></a></div>
<div>%s</div>
<div><a href=\"%s\">%s</a></div>
<div>%s</div>
<div>%s</div></div>"
							(or height "200px")
							(org-entry-get (point) "URL")
							(org-entry-get (point) "IMAGE")
							(or  (org-entry-get (point) "PRICE") "")
							(org-entry-get (point) "URL")
							(url-domain (url-generic-parse-url (org-entry-get (point) "URL")))
							(or (org-entry-get (point) "NAME") "")
							(or (org-entry-get (point) "NOTES") ""))
					 ""))
			 nil
			 (if (org-before-first-heading-p) nil 'tree)))
		"")
	 "</div>"))

;;;###autoload
(defun my-get-shopping-details-uniqlo (link)
	(when (string-match "https://www.uniqlo.com/ca/en/products/\\([^?]+\\)\\(\\?\\(.*\\)\\)?" link)
		(let ((code (match-string 1 link))
					(params (org-protocol-convert-query-to-plist (match-string 3 link)))
					item)
			(setq item
						(car
						 (assoc-default
							'items
							(assoc-default
							 'result
							 (with-current-buffer
									 (url-retrieve-synchronously
										(concat "https://www.uniqlo.com/ca/api/commerce/v3/en/products/" code))
								 (goto-char (point-min))
								 (re-search-forward "^$")
								 (json-parse-buffer :object-type 'alist :array-type 'list :null-object nil))))))
			(list
			 (cons 'price
						 (or (assoc-default
									'value
									(or
									 (assoc-default 'promo (assoc-default 'prices item))
									 (assoc-default 'base (assoc-default 'prices item))))
								 ""))
			 (cons 'image
						 (assoc-default
							'url
							(seq-find
							 (lambda (entry)
								 (or (null (plist-get params :colorCode))
										 (string=
											(concat "COL" (or (assoc-default 'colorCode entry) ""))
											(plist-get params :colorCode))))
							 (assoc-default 'main (assoc-default 'images item)))))
			 (cons 'price
						 (or (assoc-default
									'value
									(assoc-default
									 'promo
									 (assoc-default 'prices item)))
								 (assoc-default 'base (assoc-default 'prices item))
								 ""))
			 (cons 'name
						 (assoc-default 'name item))
			 (cons 'description
						 (concat (assoc-default 'longDescription item) " - "
										 (assoc-default 'washingDescription item)))
			 (cons 'url link)))))
;; Comparison-shopping with Org Mode:2 ends here

(provide 'my-org)
;;; my-org.el ends here
