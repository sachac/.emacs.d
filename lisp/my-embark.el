;;; my-embark.el ---  -*- lexical-binding: t -*-

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
;; - Embark and erefactor-rename-symbol-in-buffer
;;   https://sachachua.com/dotemacs#keybindings-embark-embark-and-erefactor-rename-symbol-in-buffer
;;
;; - Making it easier to add a category to a blog post
;;   https://sachachua.com/dotemacs#org-mode-publishing-11ty-static-site-generation-linking-to-blog-posts-making-it-easier-to-add-a-category-to-a-blog-post
;;
;; - Tip from Omar: embark-around-action-hooks
;;   https://sachachua.com/dotemacs#org-mode-links-using-an-emacs-lisp-macro-to-define-quick-custom-org-mode-links-to-project-files-plus-urls-and-search-quickly-search-my-code-tip-from-omar-embark-around-action-hooks
;;
;; - Act on current message with Embark
;;   https://sachachua.com/dotemacs#act-on-current-message-with-embark
;;
;;; Code:



;; [[file:../Sacha.org::#embark-video][Using Embark to act on video:1]]
;;;###autoload
(defun my-embark-video ()
  "Match video."
  (let ((extensions "youtu\\.?be\\|\\(webm\\|mp4\\|flv\\)$"))
    (if-let ((link (and (derived-mode-p 'org-mode)
                        (org-element-context))))
        (when (eq (org-element-type link) 'link)
          (cond
           ((string-match extensions (org-element-property :path link))
            (cons 'video (org-element-property :path link)))))
      (when (and (derived-mode-p 'dired-mode)
                 (string-match extensions (dired-get-filename)))
        (cons 'video (dired-get-filename))))))
;; Using Embark to act on video:1 ends here

;; [[file:../Sacha.org::#embark-audio][Using Embark to act on audio:1]]
;;;###autoload
(defun my-embark-audio ()
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
(defun my-audio-text (file &optional insert)
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
      (my-deepgram-recognize-audio file)
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
(defun my-open-in-audacity (file)
  (interactive "FFile: ")
  (start-process "audacity" nil "audacity" file))
;; Using Embark to act on audio:1 ends here

;; [[file:../Sacha.org::embark][embark]]
;;;###autoload
(defun my-embark-org-element ()
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
(defun my-embark-org-src-block-copy-noweb-reference (element)
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
         (format "Act on %s '%s'%s"
                 (plist-get (car targets) :type)
                 (embark--truncate-target (plist-get (car targets) :target))
                 (if (cdr targets) "…" "")))
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
(defun my-embark-image ()
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
            (cons 'image (my-get-sketch-filename (org-element-property :path link))))
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
          (add-to-list 'embark-target-finders 'my-embark-image))
;; Embark and images:2 ends here

;; [[file:../Sacha.org::#embark-subed][Embark and subed:2]]
;;;###autoload
(defun my-embark-subed-timestamp ()
  (save-excursion
    (skip-chars-backward "0-9:,.")
    (when (looking-at "\\(\\([0-9]+\\):\\)?\\([0-9]+\\):\\([0-9]+\\)\\.\\([0-9]+\\)")
      (list 'subed-timestamp
            (propertize
             (match-string 0)
             'ms (compile-media-timestamp-to-msecs (match-string 0))
             'position (if (bolp) 'start 'stop))))))
;; Embark and subed:2 ends here

;; [[file:../Sacha.org::#keybindings-embark-embark-and-erefactor-rename-symbol-in-buffer][Embark and erefactor-rename-symbol-in-buffer:1]]
;;;###autoload
(defun my-embark-erefactor-rename-symbol-in-buffer (old-name new-name)
  (interactive (let* ((old-name (read-string "Symbol: "))
                      (new-name (read-string (format "%s -> New name: " old-name)
                                             old-name
                                             'erefactor--read-symbol-history)))
                 (list old-name new-name)))
  (erefactor-rename-symbol-in-buffer old-name new-name))
;; Embark and erefactor-rename-symbol-in-buffer:1 ends here

;; [[file:../Sacha.org::#org-mode-publishing-11ty-static-site-generation-linking-to-blog-posts-making-it-easier-to-add-a-category-to-a-blog-post][Making it easier to add a category to a blog post:2]]
;;;###autoload
(defun my-embark-org-blog-target ()
	"Identify when we're looking at a blog link."
	(cond
	 ((and (derived-mode-p 'org-mode)
				 (let ((context (org-element-context)))
					 (and (org-element-type-p context 'link)
								(cond
								 ((string= (org-element-property :type context) "blog")
									(cons 'my-blog (org-element-property :path (org-element-context))))
								 ((string-match "//sachachua.com\\(.+\\)" (org-element-property :path context))
									(cons 'my-blog (match-string 1 (org-element-property :path context))))))))
)))

;;;###autoload
(defun my-embark-org-blog-add-category (blog &optional category)
	(interactive (list (my-org-blog-complete)))
	(unless category
		(setq category
					(my-11ty-complete-category
					 "Add category: "
					 (my-11ty-post-categories (my-11ty-html-filename blog)))))
	(my-11ty-add-category-to-post (my-11ty-html-filename blog) category))

;;;###autoload
(defun my-blog-url (path)
  (concat my-blog-base-url
          (replace-regexp-in-string (concat "^" (regexp-quote my-blog-base-url))
                                    ""
                                    (replace-regexp-in-string "^\\(blog:\\)?/" ""
                                                              (if (stringp path)
                                                                  path
                                                                (assoc-default 'permalink path))))))

;;;###autoload
(defun my-embark-org-blog-open-in-browser (path)
	(interactive (list (my-consult-blog-posts-by-title)))
  (browse-url (my-blog-url path)))

;;;###autoload
(defun my-embark-blog-insert-link (post)
  (interactive (list (my-consult-blog-posts-by-title)))
  (when (looking-back "\\]\\]")         ; end of a link, add a space
    (insert ", "))
  (my-org-insert-link-dwim (my-blog-url post)
                           (and (listp post) (assoc-default 'title post))))

;;;###autoload
(defun my-11ty-exported-org-filename (info)
  "Return the exported .org file for INFO, or nil if there isn't any."
  (when (stringp info) (setq info (my-blog-post-info-for-url info)))
  (let-alist info
    (let* ((org (and .permalink (expand-file-name "index.org" (expand-file-name (concat "." .permalink) my-11ty-base-dir)))))
      (cond
       ((and org (file-exists-p org))
        org)
       (.inputPath
        (expand-file-name .inputPath my-11ty-base-dir))))))

;;;###autoload
(defun my-blog-post--position (info &optional find-file)
  "Return the file position marker for a blog post INFO.
FIND-FILE is the file open function, defaulting to `consult--file-action'."
  (when (stringp info) (setq info (my-blog-post-info-for-url info)))
  (let (pos
        (files '("~/sync/emacs/Sacha.org"
								 "~/sync/orgzly/posts.org"))
        (line-number (alist-get 'line_number info 0)))
    (when-let* ((source-path
                 (or
                  (while files
                    (with-current-buffer (find-file-noselect (car files))
                      (save-excursion
                        (save-restriction
                          (widen)
						              (setq pos (org-find-property "EXPORT_ELEVENTY_PERMALINK" (assoc-default 'permalink info)))
						              (if pos
                              (progn
                                (goto-char pos)
                                (when line-number
                                  (forward-line line-number))
                                (setq line-number (line-number-at-pos nil t))
                                (setq files nil)
							                  (buffer-file-name))
                            (setq files (cdr files)))))))
                  (alist-get 'source_path info)
                  (my-11ty-exported-org-filename info))))
      (consult--marker-from-line-column
       (funcall (or find-file #'consult--file-action) (file-truename source-path))
       line-number 0))))

;;;###autoload
(defun my-blog-post-info-for-url (url &optional all-posts)
  "Return the alist for URL."
  (if (listp url)
      url
    (setq url (replace-regexp-in-string
               (concat "^" (regexp-quote my-11ty-base-dir)
                       "\\|^"
                       (regexp-quote (expand-file-name my-11ty-base-dir))
                       "\\|^"
                       (regexp-quote (file-truename my-11ty-base-dir))
                       "\\|index\\.org$\\|\\.org$") "" url))
    (when (string-match (regexp-quote my-blog-base-url) url)
      (setq url (substring url (match-end 0))))
    (when (string-match "\\?" url)
      (setq url (substring url 0 (match-beginning 0))))
    (unless (string-match "^/" url)
      (setq url (concat "/" url)))
    (seq-find (lambda (o) (string= (alist-get 'permalink o) url))
              (or all-posts (my-blog-posts)))))

(ert-deftest my-blog-post-info-for-url ()
  (should
   (equal
    (my-blog-post-info-for-url
     "https://sachachua.com/blog/2025/10/added-multiple-timezone-support-to-casual-timezone-planner/"
     )
    '((permalink . "/blog/2025/10/added-multiple-timezone-support-to-casual-timezone-planner/")
      (date . "2025-10-08T13:53:11.000Z")
      (title . "Added multiple timezone support to casual-timezone-planner")
      (categories "emacs")
      (inputPath . "./blog/2025/10/added-multiple-timezone-support-to-casual-timezone-planner/index.html"))))
  (should
   (equal
    (my-blog-post-info-for-url
     "/blog/2021/03/org2blog-add-a-note-to-the-bottom-of-blog-posts-exported-from-my-config-file/")
    '((permalink . "/blog/2021/03/org2blog-add-a-note-to-the-bottom-of-blog-posts-exported-from-my-config-file/") (date . "2021-03-25T00:00:00.000Z") (title . "Add a note to the bottom of blog posts exported from my config file") (categories "emacs" "org") (inputPath . "./blog/2021/03/org2blog-add-a-note-to-the-bottom-of-blog-posts-exported-from-my-config-file.html")))))

;;;###autoload
(defun my-blog-post--state ()
  "Blog post RAG search state function, managing preview window and cleanup."
  ;; These functions are closures captured when the state is initialized by consult--read
  (let ((preview (consult--jump-preview))
        (open (consult--temporary-files))
        (jump (consult--jump-state)))
    ;; The returned lambda is the actual preview function called by Consult
    (lambda (action cand)
      (unless cand
        (funcall open))
      (funcall preview action
               (and (or (eq action 'preview))
                    (my-blog-post--position cand (and (not (eq action 'return)) open)))))))

;;;###autoload
(defun my-11ty-current-post ()
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

;;;###autoload
(defun my-consult-blog-posts-by-title (&optional query)
  (interactive)
  (consult--read
   (mapcar
    #'my-blog-format-for-completion
    (append
     (list (my-11ty-current-post))
                                        ;todo: make a function to get the current blog post context
     (sort (my-blog-posts) :key (lambda (o) (alist-get 'date o)) :lessp #'string< :reverse t)
     nil))
   :lookup #'consult--lookup-cdr
   :prompt "Search blog posts (exact): "
   :category 'my-blog
   :sort nil
   :require-match t
   :state (my-blog-post--state)
   :initial query))

;;;###autoload
(defun my-blog-format-for-completion (result)
  (let-alist result
    (let* ((title (or .title "No Title"))
           (date (or .date "2000-01-01"))
           (year (substring date 0 4))
           (categories (string-join .categories ", "))
           (final-display
            (format "%-5s %s [%s]"
                    (propertize year 'face 'font-lock-comment-face) title categories)))
      (put-text-property 0 1 'consult--candidate result final-display)
      (cons final-display result))))
;; Making it easier to add a category to a blog post:2 ends here

;; [[file:../Sacha.org::#org-mode-links-using-an-emacs-lisp-macro-to-define-quick-custom-org-mode-links-to-project-files-plus-urls-and-search-quickly-search-my-code-tip-from-omar-embark-around-action-hooks][Tip from Omar: embark-around-action-hooks:1]]
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
(defun mail-embark-finder ()
	"Identify when we're in a notmuch message."
	(cond ((derived-mode-p 'notmuch-show-mode)
				 `(mail . ,(plist-get (plist-get (notmuch-show-get-message-properties) :headers) :From)))))
;; Act on current message with Embark:1 ends here

(provide 'my-embark)
;;; my-embark.el ends here
