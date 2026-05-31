;;; sacha-embark.el ---  -*- lexical-binding: t -*-

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
;; - Using Embark to act on video
;;   https://sachachua.com/dotemacs#embark-video
;;
;; - Using Embark to act on audio
;;   https://sachachua.com/dotemacs#embark-audio
;;
;; - Using Embark to offer context-sensitive actions for Org elements
;;   https://sachachua.com/dotemacs#using-embark-to-offer-context-sensitive-actions-for-org-elements
;;
;; - Whichkey and Embark
;;   https://sachachua.com/dotemacs#whichkey-embark
;;
;; - Embark and images
;;   https://sachachua.com/dotemacs#embark-image
;;
;; - Embark and subed
;;   https://sachachua.com/dotemacs#embark-subed
;;
;; - Use Embark to copy the name of a defun or defvar
;;   https://sachachua.com/dotemacs#keybindings-embark-use-embark-to-copy-the-name-of-a-defun-or-defvar
;;
;; - Embark and erefactor-rename-symbol-in-buffer
;;   https://sachachua.com/dotemacs#keybindings-embark-embark-and-erefactor-rename-symbol-in-buffer
;;
;; - Quickly jump to positions
;;   https://sachachua.com/dotemacs#quickly-jump-to-positions
;;
;; - Time zones
;;   https://sachachua.com/dotemacs#org-mode-time-zones
;;
;; - Insert a link to an Org Mode heading from an org-refile prompt
;;   https://sachachua.com/dotemacs#org-refile-insert-link
;;
;; - Other speed commands
;;   https://sachachua.com/dotemacs#org-mode-keyboard-shortcuts-other-speed-commands
;;
;; - Making it easier to add a category to a blog post
;;   https://sachachua.com/dotemacs#org-mode-publishing-11ty-static-site-generation-linking-to-blog-posts-making-it-easier-to-add-a-category-to-a-blog-post
;;
;; - Tip from Omar: embark-around-action-hooks
;;   https://sachachua.com/dotemacs#org-mode-links-using-an-emacs-lisp-macro-to-define-quick-custom-org-mode-links-to-project-files-plus-urls-and-search-quickly-search-sacha-code-tip-from-omar-embark-around-action-hooks
;;
;; - Act on current message with Embark
;;   https://sachachua.com/dotemacs#act-on-current-message-with-embark
;;
;;; Code:



;; [[file:../Sacha.org::#embark-video][Using Embark to act on video:1]]
;;;###autoload
(defun sacha-embark-video ()
  "Match video."
  (let ((extensions "youtu\\.?be\\|\\(webm\\|mp4\\|flv\\)$"))
    (or (if-let* ((link (and (derived-mode-p 'org-mode)
														 (org-element-context))))
						(when (eq (org-element-type link) 'link)
							(cond
							 ((string-match extensions (org-element-property :path link))
								(cons 'video (org-element-property :raw-link link))))))
				(and (url-get-url-at-point)
						 (string-match extensions (url-get-url-at-point))
						 (cons 'video (url-get-url-at-point)))
				(and (derived-mode-p 'dired-mode)
						 (string-match extensions (dired-get-filename))
						 (cons 'video (dired-get-filename))))))

;;;###autoload
(defun sacha-embark-video-edit-youtube (url)
  "Go to the YouTube edit page for VIDEO."
  (interactive "MURL: ")
	(when (or (string-match "youtu\\.?be\\(?:\\.com\\)?/\\(?:live/\\|watch/\\)?\\([-A-Za-z0-9_]+\\)" url)
            (string-match "watch\\?v=\\([-A-Za-z0-9_]+\\)" url))
    (browse-url (format "https://studio.youtube.com/video/%s/edit" (match-string 1 url)))))
;; Using Embark to act on video:1 ends here

;; [[file:../Sacha.org::#embark-audio][Using Embark to act on audio:1]]
;;;###autoload
(defun sacha-embark-audio ()
  "Match audio."
  (let ((extensions "m4a\\|mp3\\|wav\\|ogg\\|opus"))
    (if-let ((link (and (derived-mode-p 'org-mode)
                        (org-element-context))))
        (when (eq (org-element-type link) 'link)
          (cond
           ((string-match extensions (org-element-property :path link))
            (cons 'audio (org-element-property :path link)))))
      (when (and (derived-mode-p 'dired-mode)
                 (string-match extensions (dired-get-filename)))
        (cons 'audio (dired-get-filename))))))

;;;###autoload
(defun sacha-audio-text (file &optional insert)
  "Get the text for FILE audio.
  If called interactively, copy to the kill ring."
  (interactive (list (read-file-name "Audio: ")))
  (let (text)
    (cond
     ((file-exists-p (concat (file-name-sans-extension file) ".txt"))
      (with-temp-buffer
        (insert-file-contents (concat (file-name-sans-extension file) ".txt"))
        (setq text (buffer-string))))
     ;; no txt yet, is there a vtt?
     ((file-exists-p (concat (file-name-sans-extension file) ".vtt"))
      (setq text (subed-subtitle-list-text
                  (subed-parse-file (concat (file-name-sans-extension file) ".vtt")))))
     ;; no VTT, let's recognize it
     (t
      (sacha-deepgram-recognize-audio file)
      (when (file-exists-p (concat (file-name-sans-extension file) ".vtt"))
        (setq text (subed-subtitle-list-text
                    (subed-parse-file (concat (file-name-sans-extension file) ".vtt")))))))
    (when text
      (when (called-interactively-p 'any)
        (if insert
            (insert text "\n")
          (kill-new text)))
      text)))

;;;###autoload
(defun sacha-open-in-audacity (file)
  (interactive "FFile: ")
  (start-process "audacity" nil "audacity" file))
;; Using Embark to act on audio:1 ends here

;; [[file:../Sacha.org::embark][embark]]
;;;###autoload
(defun sacha-embark-org-element ()
  "Target an Org Mode element at point."
  (save-window-excursion
    (save-excursion
      (save-restriction
        (when (derived-mode-p 'org-agenda-mode)
          (org-goto-marker-or-bmk (org-get-at-bol 'org-marker))
          (org-back-to-heading))
        (when (derived-mode-p 'org-mode)
          (let* ((context ;; Borrowed from org-open-at-point
                  ;; Only consider supported types, even if they are not the
                  ;; closest one.
                  (org-element-lineage (org-element-context)
                                       '(headline src-block link) t))
                 (type (org-element-type context))
                 (value (org-element-property :value context)))
            (cond ((eq type 'headline)
                   (cons 'org-heading (org-element-property :title context)))
                  ;; src-block and link can be handled by embark-org
                  )))))))

;;;###autoload
(defun sacha-embark-org-src-block-copy-noweb-reference (element)
  (kill-new (if (org-element-property element :parameters)
                (format "<<%s(%s)>>" (org-element-property element :name)
                        (org-element-property element :parameters))
              (format "<<%s>>" (org-element-property element :parameters)))))
;; embark ends here

;; [[file:../Sacha.org::#whichkey-embark][Whichkey and Embark:1]]
;;;###autoload
(defun embark-which-key-indicator ()
  "An embark indicator that displays keymaps using which-key.
  The which-key help message will show the type and value of the
  current target followed by an ellipsis if there are further
  targets."
  (lambda (&optional keymap targets prefix)
    (if (null keymap)
        (which-key--hide-popup-ignore-command)
      (which-key--show-keymap
       (if (eq (plist-get (car targets) :type) 'embark-become)
           "Become"
				 (propertize
					(format "Act on %s '%s'%s"
                 (plist-get (car targets) :type)
                 (embark--truncate-target (plist-get (car targets) :target))
                 (if (cdr targets) "…" ""))
					'face
					`(:height 300
										:background ,(modus-themes-get-color-value
															 'bg-term-blue))
					))
       (if prefix
           (pcase (lookup-key keymap prefix 'accept-default)
             ((and (pred keymapp) km) km)
             (_ (key-binding prefix 'accept-default)))
         keymap)
       nil nil t (lambda (binding)
                   (not (string-suffix-p "-argument" (cdr binding))))))))

;;;###autoload
(defun embark-hide-which-key-indicator (fn &rest args)
  "Hide the which-key indicator immediately when using the completing-read prompter."
  (which-key--hide-popup-ignore-command)
  (let ((embark-indicators
         (remq #'embark-which-key-indicator embark-indicators)))
    (apply fn args)))
;; Whichkey and Embark:1 ends here

;; [[file:../Sacha.org::#embark-image][Embark and images:2]]
;;;###autoload
(defun sacha-embark-image ()
  "Match images."
  (let ((extensions "\\(png\\|jpg\\|svg\\|gif\\|jpeg\\)\\'"))
    (cond
     ((derived-mode-p 'org-mode)
      (when-let* ((link (org-element-context)))
        (when (eq (org-element-type link) 'link)
          (cond
           ((string= "attachment" (org-element-property :type link))
            (cons 'image (expand-file-name (org-element-property :path link)
                                           (org-attach-dir))))
           ((string-match "sketch" (org-element-property :type link))
            (cons 'image (sacha-get-sketch-filename (org-element-property :path link))))
           ((string-match extensions (org-element-property :path link))
            (cons 'image (org-element-property :path link)))))))
     ((and (derived-mode-p 'dired-mode)
           (string-match extensions (dired-get-filename)))
      (cons 'image (dired-get-filename)))
     ((derived-mode-p 'subed-mode)
      (when-let* ((filename (thing-at-point 'filename)))
        (when (string-match (concat "file:\\(.+\\." extensions "\\)") filename)
          (cons 'image (match-string 1 filename)))))
     ((and (buffer-file-name)
           (string-match extensions (buffer-file-name)))
      (cons 'image (buffer-file-name))))))
  (with-eval-after-load 'embark
          (add-to-list 'embark-target-finders 'sacha-embark-image))
;; Embark and images:2 ends here

;; [[file:../Sacha.org::#embark-subed][Embark and subed:2]]
;;;###autoload
(defun sacha-embark-subed-timestamp ()
  (save-excursion
    (skip-chars-backward "0-9:,.")
    (when (looking-at "\\(\\([0-9]+\\):\\)?\\([0-9]+\\):\\([0-9]+\\)\\.\\([0-9]+\\)")
      (list 'subed-timestamp
            (propertize
             (match-string 0)
             'ms (compile-media-timestamp-to-msecs (match-string 0))
             'position (if (bolp) 'start 'stop))))))
;; Embark and subed:2 ends here

;; [[file:../Sacha.org::#keybindings-embark-use-embark-to-copy-the-name-of-a-defun-or-defvar][Use Embark to copy the name of a defun or defvar:1]]
(defun sacha-embark-copy-def-name (target)
  "Extract and copy the name from a TARGET."
  (let* ((name (symbol-name (cadr (read target)))))
    (kill-new name)
    (message "%s" name)))
;; Use Embark to copy the name of a defun or defvar:1 ends here

;; [[file:../Sacha.org::#keybindings-embark-embark-and-erefactor-rename-symbol-in-buffer][Embark and erefactor-rename-symbol-in-buffer:1]]
;;;###autoload
(defun sacha-embark-erefactor-rename-symbol-in-buffer (old-name new-name)
  (interactive (let* ((old-name (read-string "Symbol: "))
                      (new-name (read-string (format "%s -> New name: " old-name)
                                             old-name
                                             'erefactor--read-symbol-history)))
                 (list old-name new-name)))
  (erefactor-rename-symbol-in-buffer old-name new-name))
;; Embark and erefactor-rename-symbol-in-buffer:1 ends here

;; [[file:../Sacha.org::#quickly-jump-to-positions][Quickly jump to positions:1]]
;;;###autoload
    (defun avy-action-exchange (pt)
      "Exchange sexp at PT with the one at point."
      (set-mark pt)
      (transpose-sexps 0))
;;;###autoload
    (defun avy-action-embark (pt)
      (save-excursion
        (goto-char pt)
        (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0)))
      t)
;; Quickly jump to positions:1 ends here

;; [[file:../Sacha.org::#org-mode-time-zones][Time zones:1]]
(defvar sacha-time-zones nil "List of time-zones (ex: America/Toronto) to translate time to.")
;;;###autoload
(defun sacha-summarize-times (time &optional time-zones)
	"Return TIME in TIME-ZONES."
  (let (prev-day)
    (mapconcat
     (lambda (tz)
       (let ((cur-day (format-time-string "%a %b %-e" time tz))
             (cur-time (format-time-string "%H%MH %Z" time tz)))
         (if (equal prev-day cur-day)
             cur-time
           (setq prev-day cur-day)
           (concat cur-day " " cur-time))))
     (or time-zones sacha-time-zones)
     " / ")))

;;;###autoload
(defun sacha-org-timestamp-in-time-zones (&optional s)
	"Copy the current timestamp in `sacha-time-zones'."
	(interactive)
	(let* ((s (or s (org-element-property :raw-value (org-element-context))))
				 (time (org-timestamp-to-time (org-timestamp-from-string s)))
				 (result (format "%s (%s) = %s"
												 s
												 (format-time-string "%z")
												 (sacha-summarize-times time))))
		(when (called-interactively-p)
			(message "%s" result)
			(kill-new result))
		result))
;; Time zones:1 ends here

;; [[file:../Sacha.org::#org-refile-insert-link][Insert a link to an Org Mode heading from an org-refile prompt:1]]
;;;###autoload
(defun sacha-embark-org-insert-link-from-path (path)
	(interactive (list (car (org-refile-get-location))))
	(let* ((extra (if org-refile-use-outline-path "/" ""))
				 (tbl (mapcar
							 (lambda (x)
								 (if (and (not (member org-refile-use-outline-path
																			 '(file full-file-path title)))
													(not (equal filename (file-truename (nth 1 x)))))
										 (cons (concat (car x) extra " ("
																	 (file-name-nondirectory (nth 1 x)) ")")
													 (cdr x))
									 (cons (concat (car x) extra) (cdr x))))
							 org-refile-target-table))
				 link)
		(insert (save-window-excursion
							(save-excursion
								(org-goto-marker-or-bmk
								 (elt
									(org-refile--get-location path tbl)
									3))
								(org-store-link nil))))))
(defvar-keymap sacha-org-path-map
	:doc "Shortcuts for working with Org paths from `org-refile'."
	"i" #'sacha-embark-org-insert-link-from-path
	"L" #'sacha-embark-org-insert-link-from-path)
;; Insert a link to an Org Mode heading from an org-refile prompt:1 ends here

;; [[file:../Sacha.org::#org-mode-keyboard-shortcuts-other-speed-commands][Other speed commands:3]]
;;;###autoload
(defun sacha-embark-org-copy-subtree-text (target)
  "Copy the subtree text for TARGET."
	(if (and (stringp target)
					 (derived-mode-p 'org-mode))
			(sacha-org-copy-subtree-text)
		(embark-org--in-source-window
		 target
		 (lambda (marker)
			 (org-with-point-at marker
				 (sacha-org-copy-subtree-text))))))
;; Other speed commands:3 ends here

;; [[file:../Sacha.org::#org-mode-publishing-11ty-static-site-generation-linking-to-blog-posts-making-it-easier-to-add-a-category-to-a-blog-post][Making it easier to add a category to a blog post:2]]
;;;###autoload
(defun sacha-embark-org-blog-target ()
  "Identify when we're looking at a blog link."
  (or
	 (when-let* ((context (and (derived-mode-p 'org-mode) (org-element-context))))
  	 (and (org-element-type-p context 'link)
  				(cond
  				 ((string= (org-element-property :type context) "blog")
  					(cons 'sacha-blog (org-element-property :path (org-element-context))))
  				 ((string-match "//sachachua.com\\(.+\\)" (org-element-property :path context))
  					(cons 'sacha-blog (match-string 1 (org-element-property :path context)))))))
	 (when (and (derived-mode-p 'org-mode)
							(org-entry-get-with-inheritance "EXPORT_ELEVENTY_PERMALINK"))
		 (cons 'sacha-blog (org-entry-get-with-inheritance "EXPORT_ELEVENTY_PERMALINK")))))

;;;###autoload
(defun sacha-embark-org-blog-add-category (blog &optional category)
  (interactive (list (sacha-org-blog-complete)))
  (unless category
    (setq category
  				(sacha-11ty-complete-category
  				 "Add category: "
  				 (sacha-11ty-post-categories (sacha-11ty-html-filename blog)))))
  (sacha-11ty-add-category-to-post (sacha-11ty-html-filename blog) category))

;;;###autoload
(defun sacha-blog-url (path)
  (concat sacha-blog-base-url
          (replace-regexp-in-string (concat "^" (regexp-quote sacha-blog-base-url))
                                    ""
                                    (replace-regexp-in-string "^\\(blog:\\)?/" ""
                                                              (if (stringp path)
                                                                  path
                                                                (assoc-default 'permalink path))))))

;;;###autoload
(defun sacha-embark-org-blog-open-in-browser (path)
  (interactive (list (sacha-consult-blog-posts-by-title)))
  (browse-url (sacha-blog-url path)))

;;;###autoload
(defun sacha-embark-blog-insert-link (post)
  (interactive (list (sacha-consult-blog-posts-by-title)))
  (when (looking-back "\\]\\]")         ; end of a link, add a space
    (insert ", "))
  (sacha-org-insert-link-dwim (sacha-blog-url post)
															(and (listp post) (assoc-default 'title post))))

;;;###autoload
(defun sacha-11ty-exported-org-filename (info)
  "Return the exported .org file for INFO, or nil if there isn't any."
  (when (stringp info) (setq info (sacha-blog-post-info-for-url info)))
  (let-alist info
    (let* ((org (and .permalink (expand-file-name "index.org" (expand-file-name (concat "." .permalink) sacha-11ty-base-dir)))))
      (cond
       ((and org (file-exists-p org))
        org)
       (.inputPath
        (expand-file-name .inputPath sacha-11ty-base-dir))))))

(defvar sacha-topic-dir "~/sync/topics" "Directory with topics.")
;;;###autoload
(defun sacha-blog-post--position (info &optional find-file)
  "Return the file position marker for a blog post INFO.
  FIND-FILE is the file open function, defaulting to `consult--file-action'."
  (when (stringp info) (setq info (sacha-blog-post-info-for-url info)))
	(when info
		;; Handle topics
		(if (string-match "/topic/\\(.+?\\)/" (alist-get 'permalink info))
				(consult--marker-from-line-column
				 (funcall (or find-file #'consult--file-action)
									(expand-file-name (concat (match-string 1 (alist-get 'permalink info))
																						".org")
																		sacha-topic-dir))
				 1 0)
			(let (pos
						(files (delq nil
												 (list "~/sync/emacs/Sacha.org"
															 "~/sync/orgzly/posts.org"
															 (alist-get 'source_path info)
															 (sacha-11ty-exported-org-filename info))))
						(line-number (alist-get 'line_number info 0)))
				(when-let* ((source-path
										 (seq-find
											(lambda (filename)
												(with-current-buffer (find-file-noselect filename)
													(save-excursion
														(save-restriction
															(widen)
															(if (assoc-default 'anchor info)
																	(when-let* ((pos (org-find-property "CUSTOM_ID" (assoc-default 'anchor info))))
																		(goto-char pos)
																		(setq line-number (line-number-at-pos nil t))
																		(buffer-file-name))
																(setq pos (org-find-property "EXPORT_ELEVENTY_PERMALINK" (assoc-default 'permalink info)))
																(when pos
																	(progn
																		(goto-char pos)
																		(when line-number
																			(forward-line line-number))
																		(setq line-number (line-number-at-pos nil t))
																		(buffer-file-name))))))))
											files)))
					(consult--marker-from-line-column
					 (funcall (or find-file #'consult--file-action) (file-truename source-path))
					 line-number 0))))))

;;;###autoload
(defun sacha-blog-post-info-for-url (url &optional all-posts)
  "Return the alist for URL.
  The alist will have the following keys: permalink, date, title,
  categories, inputPath.  If URL has an anchor, add it as an anchor
  attribute.
  "
  (let (anchor entry)
    (if (listp url)
        url
      (when (string-match "#\\(.*\\)" url)
        (setq anchor (match-string 1 url))
        (setq url (replace-match "" nil nil url)))
      (setq url (replace-regexp-in-string
                 (concat "^" (regexp-quote sacha-11ty-base-dir)
                         "\\|^"
                         (regexp-quote (expand-file-name sacha-11ty-base-dir))
                         "\\|^"
                         (regexp-quote (file-truename sacha-11ty-base-dir))
                         "\\|index\\.org$\\|\\.org$") "" url))
      (when (string-match (regexp-quote sacha-blog-base-url) url)
        (setq url (substring url (match-end 0))))
      (when (string-match "\\?" url)
        (setq url (substring url 0 (match-beginning 0))))
      (unless (string-match "^/" url)
        (setq url (concat "/" url)))
      (unless (string-match "/$" url)
        (setq url (concat url "/")))
      (setq entry
            (seq-find (lambda (o) (string= (alist-get 'permalink o) url))
                      (or all-posts (sacha-blog-posts))))
      (if anchor
          (cons `(anchor . ,anchor) entry)
        entry))))

(ert-deftest sacha-blog-post-info-for-url--handle-hash ()
  "Tests `sacha-blog-post-info-for-url'."
  (should
   (equal
    (sacha-blog-post-info-for-url "/blog/2026/04/yayemacs-10-emacs-coaching-with-prot-packaging-emacs-lisp/#projects-experiment-with-learning-from-prot-yayemacs-10-emacs-coaching-with-prot-packaging-emacs-lisp-ideas-for-next-steps")
    '((anchor
       . "projects-experiment-with-learning-from-prot-yayemacs-10-emacs-coaching-with-prot-packaging-emacs-lisp-ideas-for-next-steps")
      (permalink
       . "/blog/2026/04/yayemacs-10-emacs-coaching-with-prot-packaging-emacs-lisp/")
      (date . "2026-04-04T02:23:03.000Z")
      (title
       . "#YayEmacs 10: Emacs coaching with Prot: Emacs workflows and streaming")
      (categories "emacs" "yay-emacs")
      (inputPath
       . "./blog/2026/04/yayemacs-10-emacs-coaching-with-prot-packaging-emacs-lisp/index.html")))))

(ert-deftest sacha-blog-post-info-for-url ()
  (should
   (equal
    (sacha-blog-post-info-for-url
     "https://sachachua.com/blog/2025/10/added-multiple-timezone-support-to-casual-timezone-planner/"
     )
    '((permalink . "/blog/2025/10/added-multiple-timezone-support-to-casual-timezone-planner/")
      (date . "2025-10-08T13:53:11.000Z")
      (title . "Added multiple timezone support to casual-timezone-planner")
      (categories "emacs")
      (inputPath . "./blog/2025/10/added-multiple-timezone-support-to-casual-timezone-planner/index.html"))))
  (should
   (equal
    (sacha-blog-post-info-for-url
     "/blog/2021/03/org2blog-add-a-note-to-the-bottom-of-blog-posts-exported-from-sacha-config-file/")
    '((permalink . "/blog/2021/03/org2blog-add-a-note-to-the-bottom-of-blog-posts-exported-from-sacha-config-file/") (date . "2021-03-25T00:00:00.000Z") (title . "Add a note to the bottom of blog posts exported from my config file") (categories "emacs" "org") (inputPath . "./blog/2021/03/org2blog-add-a-note-to-the-bottom-of-blog-posts-exported-from-sacha-config-file.html")))))

;;;###autoload
(defun sacha-blog-post--state ()
  "Blog post search state function, managing preview window and cleanup."
  ;; These functions are closures captured when the state is initialized by consult--read
  (let ((preview (consult--jump-preview))
        (open (consult--temporary-files))
        (jump (consult--jump-state)))
    ;; The returned lambda is the actual preview function called by Consult
    (lambda (action cand)
      (unless cand
        (funcall open))
      (funcall
			 preview action
       (and (or (eq action 'preview))
						(sacha-blog-post--position
						 (car (split-string cand " "))
						 (and (not (eq action 'return)) open)))))))

;;;###autoload
(defun sacha-11ty-current-post ()
  "Return the current blog post info if any."
  (cond
   ((derived-mode-p 'org-mode)
    (when (org-entry-get-with-inheritance "EXPORT_ELEVENTY_FILE_NAME")
      (let ((filename (org-entry-get-with-inheritance "EXPORT_ELEVENTY_FILE_NAME")))
        `((filename . ,filename)
          (permalink . ,(org-entry-get-with-inheritance "EXPORT_ELEVENTY_PERMALINK"))
          (title . ,(save-excursion
                      (save-restriction
                        (widen)
                        (goto-char (org-find-property "EXPORT_ELEVENTY_FILE_NAME" (org-entry-get-with-inheritance "EXPORT_ELEVENTY_FILE_NAME")))
                        (org-entry-get (point) "ITEM"))))
          (date . ,(org-entry-get-with-inheritance "EXPORT_DATE"))))))
   ((derived-mode-p '(html-mode web-mode))
    ;; called from an index.html or page.html, maybe?
    (let* ((file (buffer-file-name))
  				 (json-file (concat (file-name-sans-extension (buffer-file-name))
  														".11tydata.json"))
  				 (json-data (and (file-exists-p json-file)
  												 (json-read-file json-file))))
      (cons
       (cons 'filename file)
       json-data)))))

(defun sacha-consult--blog-posts ()
	(mapcar
	 #'sacha-blog-format-for-completion
	 (delq
		nil
		(append
		 (list (sacha-11ty-current-post))
		 ;;todo: make a function to get the current blog post context
		 (sort (sacha-blog-posts) :key (lambda (o) (alist-get 'date o)) :lessp #'string< :reverse t)
		 nil))))

;;;###autoload
(defun sacha-consult-blog-posts-by-title (&optional query)
  (interactive)
  (let* ((options (sacha-consult--blog-posts))
				 (val (consult--read
							 options
               :prompt "Search blog posts (exact): "
               :category 'sacha-blog
							 :sort nil
               :require-match nil
							 ;; Preview no longer working: text properties not included?
               ;; :state (sacha-blog-post--state)
               :initial query)
							))
    (or (and (stringp val) (string-match "^https://\\|^/" val)
             (sacha-blog-post-info-for-url val))
        (get-text-property 0 'consult--candidate val)
        (assoc-default val options #'string=))))

;;;###autoload
(defun sacha-blog-format-for-completion (result)
  (let-alist result
    (let* ((title (or .title "No Title"))
           (date (or .date "2000-01-01"))
           (year (substring date 0 4))
           (categories (string-join .categories ", "))
					 (permalink .permalink)
           (final-display
            (format "%-5s %s [%s]"
                    (propertize year 'face 'font-lock-comment-face) title categories)))
      (put-text-property 0 1 'consult--candidate result final-display)
			(cons final-display result))))

;;;###autoload
(defun sacha-embark-blog-copy-link (permalink)
  "Copy the URL to this blog post."
  (interactive "MPermalink: ")
	(kill-new
	 (if (string-match (regexp-quote sacha-blog-base-url) permalink)
			 permalink
		 (concat sacha-blog-base-url
						 (replace-regexp-in-string "^/" ""
																			 permalink)))))
;; Making it easier to add a category to a blog post:2 ends here

;; [[file:../Sacha.org::#org-mode-links-using-an-emacs-lisp-macro-to-define-quick-custom-org-mode-links-to-project-files-plus-urls-and-search-quickly-search-sacha-code-tip-from-omar-embark-around-action-hooks][Tip from Omar: embark-around-action-hooks:1]]
;;;###autoload
(cl-defun embark-consult--at-location (&rest args &key target type run &allow-other-keys)
	"RUN action at the target location."
	(save-window-excursion
		(save-excursion
			(save-restriction
				(pcase type
					('consult-location (consult--jump (consult--get-location target)))
					('org-heading (org-goto-marker-or-bmk (get-text-property 0 'org-marker target)))
					('consult-grep (consult--jump (consult--grep-position target)))
					('file (find-file target)))
				(apply run args)))))
;; Tip from Omar: embark-around-action-hooks:1 ends here

;; [[file:../Sacha.org::#act-on-current-message-with-embark][Act on current message with Embark:1]]
;;;###autoload
(defun sacha-embark-mail-finder ()
	"Identify when we're in a notmuch message."
	(cond ((derived-mode-p 'notmuch-show-mode)
				 `(mail . ,(plist-get (plist-get (notmuch-show-get-message-properties) :headers) :From)))))
;; Act on current message with Embark:1 ends here

(provide 'sacha-embark)
;;; sacha-embark.el ends here
