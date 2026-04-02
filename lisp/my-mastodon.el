;;; my-mastodon.el ---  -*- lexical-binding: t -*-

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
;; - Mastodon
;;   https://sachachua.com/dotemacs#mastodon
;;
;; - Adding Mastodon toots as comments in my 11ty static blog
;;   https://sachachua.com/dotemacs#mastodon-adding-mastodon-toots-as-comments-in-my-11ty-static-blog
;;
;; - mastodon.el: Copy toot content as Org Mode
;;   https://sachachua.com/dotemacs#mastodon-mastodon-el-copy-toot-content-as-org-mode
;;
;; - mastodon.el: Mention people based on regexp
;;   https://sachachua.com/dotemacs#mastodon-mastodon-el-mention-people-based-on-regexp
;;
;; - mastodon.el: Collect handles in clipboard (Emacs kill ring)
;;   https://sachachua.com/dotemacs#mastodon-mastodon-el-collect-handles-in-kill-ring
;;
;; - mastodon.el: Copy toot URL after posting; also, copying just this post with 11ty
;;   https://sachachua.com/dotemacs#mastodon-mastodon-el-copy-toot-url-after-posting-also-copying-just-this-post-with-11ty
;;
;; - Storing Mastodon links in Org mode
;;   https://sachachua.com/dotemacs#storing-mastodon-links-in-org-mode
;;
;; - Collecting Emacs News from Mastodon
;;   https://sachachua.com/dotemacs#mastodon-news
;;
;; - Copy Mastodon link for Emacs News
;;   https://sachachua.com/dotemacs#copy-mastodon-link-for-emacs-news
;;
;; - Combining Mastodon timelines using mastodon.el
;;   https://sachachua.com/dotemacs#mastodon-combined-timeline
;;
;; - Following people
;;   https://sachachua.com/dotemacs#following-people
;;
;; - Tooting a link to the current post
;;   https://sachachua.com/dotemacs#mastodon-tooting-a-link-to-the-current-post
;;
;; - Compose a Mastodon toot with the current Org subtree
;;   https://sachachua.com/dotemacs#mastodon-toot-subtree
;;
;; - Posting the latest screenshot with mastodon.el
;;   https://sachachua.com/dotemacs#posting-the-latest-screenshot-with-mastodon-el
;;
;; - Making it easier to toot my config
;;   https://sachachua.com/dotemacs#mastodon-toot-config
;;
;; - Capture
;;   https://sachachua.com/dotemacs#mastodon-org-contacts-capture
;;
;; - Completion
;;   https://sachachua.com/dotemacs#mastodon-org-contacts-complete
;;
;; - Copy Mastodon toot URL as author link
;;   https://sachachua.com/dotemacs#mastodon-copy-mastodon-toot-url-as-author-link
;;
;; - Collect my recent toots in an Org file so that I can refile them
;;   https://sachachua.com/dotemacs#mastodon-org-feed
;;
;; - Archive toots on my blog
;;   https://sachachua.com/dotemacs#mastodon-insert-statuses
;;
;;; Code:



;; [[file:../Sacha.org::#mastodon][Mastodon:2]]
;;;###autoload
(defun my-mastodon-clear-auth ()
	"Fix alist-get: Wrong type argument: listp, (error . \"The access token is invalid\") error. Then you can use `mastodon-auth--access-token'."
	(interactive)
	(setq mastodon-client--active-user-details-plist nil)
	(delete-file (concat user-emacs-directory "mastodon.plstore"))
	(setq mastodon-auth--token-alist nil))

;;;###autoload
(defun my-mastodon-toot-public-string (message)
  (interactive "sMessage: ")
	(mastodon-toot--compose-buffer
	 nil nil nil
	 message)
	(condition-case nil (mastodon-toot-send)
		(error nil)))

;;;###autoload
(defun my-mastodon-show-my-followers ()
  (interactive)
  (mastodon-profile--make-profile-buffer-for
   (mastodon-profile--lookup-account-in-status (mastodon-auth--get-account-name) nil)
   "followers"
   #'mastodon-profile--add-author-bylines))

;;;###autoload
(defun my-yank-mastodon-link ()
  (interactive)
  (let* ((url (current-kill 0))
         (url-parsed (url-generic-parse-url url))
         (user (file-name-base (url-filename url-parsed))))
    (cond
     ((derived-mode-p 'oddmuse-mode) (insert "[" url " " user
                                             "@" (url-host url-parsed) "]"))
     ((derived-mode-p 'org-mode) (insert "[[" url "][" user
                                         "@" (url-host url-parsed) "]]"))
     (t (insert url)))))

(declare-function 'mastodon-notifications-get-mentions "mastodon-notifications")
;; Mastodon:2 ends here

;; [[file:../Sacha.org::#mastodon][Mastodon:4]]
;;;###autoload
(defun my-mastodon-browse-url (url &rest _)
  "Open URL."
	(if (string-match "medium\\.com" url)
			(funcall browse-url-browser-function url)
		(mastodon-url-lookup url)))
;; Mastodon:4 ends here

;; [[file:../Sacha.org::#mastodon-adding-mastodon-toots-as-comments-in-my-11ty-static-blog][Adding Mastodon toots as comments in my 11ty static blog:3]]
;;;###autoload
(defun my-mastodon-toot-comment-json ()
	(let* ((toot (mastodon-toot--base-toot-or-item-json)))
		(unless (string= (alist-get 'visibility toot) "public")
			(error "Not a public toot."))
		`((parentPostId . ,(alist-get 'in_reply_to_id toot))
			(postId . ,(alist-get 'id toot))
			(author . ,(format "<a href=\"%s\">@%s</a>"
												 (alist-get 'url (alist-get 'account toot))
												 (alist-get 'acct (alist-get 'account toot))))
			(date . ,(alist-get 'created_at toot))
			(message . ,(format "<div class=\"mastodon-body\">%s</div><div class=\"mastodon-source\">From <a href=\"%s\">Mastodon</a></div>"
													(alist-get 'content toot)
													(alist-get 'url toot))))))
;; Adding Mastodon toots as comments in my 11ty static blog:3 ends here

;; [[file:../Sacha.org::#mastodon-adding-mastodon-toots-as-comments-in-my-11ty-static-blog][Adding Mastodon toots as comments in my 11ty static blog:4]]
;;;###autoload
(defun my-mastodon-toot-add-or-update-blog-comment (url)
	(interactive (list (my-complete-blog-post-url)))
	(find-file (my-11ty-add-blog-comment (my-mastodon-toot-comment-json))))
;; Adding Mastodon toots as comments in my 11ty static blog:4 ends here

;; [[file:../Sacha.org::#mastodon-mastodon-el-copy-toot-content-as-org-mode][mastodon.el: Copy toot content as Org Mode:1]]
;;;###autoload
(defun my-mastodon-toot-at-url (&optional url)
	"Return JSON toot object at URL.
If URL is nil, return JSON toot object at point."
	(if url
			(let* ((search (format "%s/api/v2/search" mastodon-instance-url))
						 (params `(("q" . ,url)
											 ("resolve" . "t"))) ; webfinger
						 (response (mastodon-http--get-json search params :silent)))
				(car (alist-get 'statuses response)))
		(mastodon-toot--base-toot-or-item-json)))

;;;###autoload
(defun my-mastodon-org-copy-toot-content (&optional url)
	"Copy the current toot's content as Org Mode.
Use pandoc to convert.

When called with \\[universal-argument], prompt for a URL."
	(interactive (list
								(when current-prefix-arg
									(read-string "URL: "))))

	(let ((toot (my-mastodon-toot-at-url url)))
		(with-temp-buffer
			(insert (alist-get 'content toot))
			(call-process-region nil nil "pandoc" t t nil "-f" "html" "-t" "org")
			(kill-new
			 (concat
				(org-link-make-string
				 (alist-get 'url toot)
				 (concat "@" (alist-get 'acct (alist-get 'account toot))))
				":\n\n#+begin_quote\n"
				(string-trim (buffer-string)) "\n#+end_quote\n"))
			(message "Copied."))))
;; mastodon.el: Copy toot content as Org Mode:1 ends here

;; [[file:../Sacha.org::#mastodon-mastodon-el-mention-people-based-on-regexp][mastodon.el: Mention people based on regexp:1]]
(defvar my-org-contacts-file "~/sync/orgzly/people.org")
;;;###autoload
(defun my-mastodon-insert-handle-from-contacts ()
	(interactive)
	(let ((collection
				 (with-temp-buffer
					 (insert-file-contents my-org-contacts-file)
					 (org-mode)
					 (goto-char (point-min))
					 (org-map-entries
						(lambda ()
							(let ((handle (org-entry-get (point) "MASTODON"))
										(name (org-entry-get (point) "ITEM")))
								(cons (format "%s (%s)" name handle)
											handle)))
						"MASTODON={.}"))))
		(insert (assoc-default (completing-read "Name: " collection)
													 collection #'string= ""))))
;; mastodon.el: Mention people based on regexp:1 ends here

;; [[file:../Sacha.org::#mastodon-mastodon-el-mention-people-based-on-regexp][mastodon.el: Mention people based on regexp:3]]
;;;###autoload
(defun my-mastodon-interested-handles (text)
  (seq-uniq
   (append
    (seq-keep
     (lambda (o)
       (assoc-default "MASTODON" o #'string=))
     (my-org-contacts-to-mention text))
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (cl-loop while (re-search-forward "@[\\.0-9A-Z_a-z-]+@[\\.0-9A-Z_a-z-]+" nil t)
               collect (match-string 0))))))

;;;###autoload
(defun my-mastodon-insert-interested-handles (text)
	(interactive (list (mastodon-toot--remove-docs)))
	(when-let* ((handles (my-mastodon-interested-handles text)))
		(save-excursion
			(unless (looking-at " ") (insert " "))
			(insert (string-join handles " ")))))
;; mastodon.el: Mention people based on regexp:3 ends here

;; [[file:../Sacha.org::#mastodon-mastodon-el-collect-handles-in-kill-ring][mastodon.el: Collect handles in clipboard (Emacs kill ring):1]]
(defvar my-mastodon-handle "@sacha@social.sachachua.com")
;;;###autoload
(defun my-mastodon-copy-handle (&optional start-new beg end)
	"Append Mastodon handles to the kill ring.

Use the handle at point or the author of the toot.  If called with a
region, collect all handles in the region.

Append to the current kill if it starts with @. If not, start a new
kill. Call with \\[universal-argument] to always start a new list.

Omit my own handle, as specified in `my-mastodon-handle'."
	(interactive (list current-prefix-arg
										 (when (region-active-p) (region-beginning))
										 (when (region-active-p) (region-end))))
	(let ((handle
				 (if (and beg end)
						 ;; collect handles in region
						 (save-excursion
               (goto-char beg)
               (let (list)
                 ;; Collect all handles from the specified region
                 (while (< (point) end)
                   (let ((mastodon-handle (get-text-property (point) 'mastodon-handle))
                         (button (get-text-property (point) 'button)))
                     (cond
                      (mastodon-handle
											 (when (and (string-match "@" mastodon-handle)
																	(or (null my-mastodon-handle)
																			(not (string= my-mastodon-handle mastodon-handle))))
												 (cl-pushnew
													(concat (if (string-match "^@" mastodon-handle) ""
																		"@")
																	mastodon-handle)
													list
													:test #'string=))
											 (goto-char (next-single-property-change (point) 'mastodon-handle nil end)))
                      ((and button (looking-at "@"))
                       (let ((text-start (point))
                             (text-end (or (next-single-property-change (point) 'button nil end) end)))
												 (dolist (h (split-string (buffer-substring-no-properties text-start text-end) ", \n\t"))
													 (unless (and my-mastodon-handle (string= my-mastodon-handle h))
														 (cl-pushnew h list :test #'string=)))
												 (goto-char text-end)))
											(t
											 ;; collect authors of toots too
											 (when-let*
													 ((toot (mastodon-toot--base-toot-or-item-json))
														(author (and toot
																				 (concat "@"
																								 (alist-get
																									'acct
																									(alist-get 'account (mastodon-toot--base-toot-or-item-json)))))))
												 (unless (and my-mastodon-handle (string= my-mastodon-handle author))
													 (cl-pushnew
														author
														list
														:test #'string=)))
											 (goto-char (next-property-change (point) nil end))))))
                 (setq handle (string-join (seq-uniq list #'string=) " "))))
					 (concat "@"
									 (or
										(get-text-property (point) 'mastodon-handle)
										(alist-get
										 'acct
										 (alist-get 'account (mastodon-toot--base-toot-or-item-json))))))))
		(if (or start-new (null kill-ring) (not (string-match "^@" (car kill-ring))))
				(kill-new handle)
			(dolist (h (split-string handle " "))
				(unless (member h (split-string " " (car kill-ring)))
					(setf (car kill-ring) (concat (car kill-ring) " " h)))))
		(message "%s" (car kill-ring))))
;; mastodon.el: Collect handles in clipboard (Emacs kill ring):1 ends here

;; [[file:../Sacha.org::#mastodon-mastodon-el-copy-toot-url-after-posting-also-copying-just-this-post-with-11ty][mastodon.el: Copy toot URL after posting; also, copying just this post with 11ty:1]]
(defvar my-mastodon-toot-posted-hook nil "Called with the item.")

;;;###autoload
(defun my-mastodon-copy-toot-url (toot)
	(interactive (list (my-mastodon-latest-toot)))
	(kill-new (alist-get 'url toot)))
(add-hook 'my-mastodon-toot-posted-hook #'my-mastodon-copy-toot-url)

;;;###autoload
(defun my-mastodon-latest-toot ()
	(interactive)
	(require 'mastodon-http)
	(let* ((json-array-type 'list)
				 (json-object-type 'alist))
		(car
		 (mastodon-http--get-json
			(mastodon-http--api
			 (format "accounts/%s/statuses?count=1&limit=1&exclude_reblogs=t"
							 (mastodon-auth--get-account-id)))
			nil :silent))))
;; mastodon.el: Copy toot URL after posting; also, copying just this post with 11ty:1 ends here

;; [[file:../Sacha.org::#mastodon-mastodon-el-copy-toot-url-after-posting-also-copying-just-this-post-with-11ty][mastodon.el: Copy toot URL after posting; also, copying just this post with 11ty:3]]
;;;###autoload
(defun my-mastodon-org-maybe-set-toot-url (toot)
	(cond
	 ((derived-mode-p 'org-mode)
		(let ((permalink (org-entry-get-with-inheritance "EXPORT_ELEVENTY_PERMALINK")))
			(when (and permalink
								 (string-match (regexp-quote permalink) (alist-get 'content toot))
								 (not (org-entry-get-with-inheritance "MASTODON")))
				(save-excursion
					(goto-char (org-find-property "EXPORT_ELEVENTY_PERMALINK"
																				permalink))
					(org-entry-put
					 (point)
					 "EXPORT_MASTODON"
					 (alist-get 'url toot))
					(message "Toot URL set: %s, republish if needed" toot)))))
	 (t
		(when (buffer-file-name)
			(let (filename data)
				(cond
				 ((string-match "\\.11tydata\\.json" (buffer-file-name))
					(setq data (json-parse-string (buffer-string) :object-type 'alist :array-type 'list)))
				 ((and (buffer-file-name)
							 (file-exists-p (concat (file-name-sans-extension (buffer-file-name)) ".11tydata.json")))
					(let ((json-object-type 'alist)
								(json-array-type 'list))
						(setq
						 filename (concat (file-name-sans-extension (buffer-file-name)) ".11tydata.json")
						 data (json-read-file filename)))))
				(when (and
							 data
							 (string-match (regexp-quote (alist-get 'permalink data)) (alist-get 'content toot))
							 (not (alist-get 'mastodon data)))
					(push (cons 'mastodon (alist-get 'url toot)) data)
					(if filename
							(with-temp-file filename
								(insert (json-encode data)))
						(erase-buffer)
						(insert (json-encode data)))))))))
(add-hook 'my-mastodon-toot-posted-hook #'my-mastodon-org-maybe-set-toot-url)
;; mastodon.el: Copy toot URL after posting; also, copying just this post with 11ty:3 ends here

;; [[file:../Sacha.org::my-mastodon-store-link][my-mastodon-store-link]]
;;;###autoload
(defun my-mastodon-store-link ()
  "Store links in Mastodon buffers."
  (when (derived-mode-p 'mastodon-mode)
    (let ((json (get-text-property (point) 'item-json)))
      (org-link-store-props
       :link (mastodon-toot--toot-url)
       :content (mastodon-tl--content json)
       :text
			 (concat
				(string-trim (mastodon-tl--render-text (mastodon-tl--content json)))
				(if (assoc-default 'media_attachments json)
						(concat "\n\n"
										(mapconcat
										 (lambda (attachment)
											 (org-link-make-string
												(assoc-default 'url attachment)
												(assoc-default 'description attachment)))
										 (assoc-default 'media_attachments json)
										 "\n"
										 )))
						"")
				))))
;; my-mastodon-store-link ends here

;; [[file:../Sacha.org::#mastodon-news][Collecting Emacs News from Mastodon:1]]
;;;###autoload
(defun my-mastodon-save-toot-for-emacs-news ()
	(interactive)
	;; store a link and capture the note
	(org-capture nil "📰")
	;; boost if not already boosted
	(unless (get-text-property
					 (car
						(mastodon-tl--find-property-range 'byline (point)))
					 'boosted-p)
		(mastodon-toot--toggle-boost-or-favourite 'boost)))
;; Collecting Emacs News from Mastodon:1 ends here

;; [[file:../Sacha.org::#mastodon-news][Collecting Emacs News from Mastodon:5]]
;;;###autoload
(defun my-mastodon-get-note-info ()
	"Return (:handle ... :url ... :links ... :text) for the current subtree."
	(let ((url (let ((title (org-entry-get (point) "ITEM")))
							 (if (string-match org-link-any-re title)
									 (or
										(match-string 7 title)
										(match-string 2 title)))))
				beg end
				handle)
		(save-excursion
			(org-back-to-heading)
			(org-end-of-meta-data)
			(setq beg (point))
			(setq end (org-end-of-subtree))
      (unless url
        (goto-char beg)
        (when (re-search-forward org-any-link-re end t)
          (setq url (org-element-property :raw-link (org-element-context)))))
			(cond
       ((string-match "\\[\\[https://bsky\\.app/.+?\\]\\[\\(.+\\)\\]\\]" url)
				(setq handle (match-string 1 url)))
			 ((string-match "https://\\(.+?\\)/\\(@.+?\\)/" url)
				(setq handle (concat
											(match-string 2 url) "@" (match-string 1 url))))
			 ((string-match "https://\\(.+?\\)/\\(.+?\\)/p/[0-9]+\\.[0-9]+" url)
				(setq handle (concat
											"@" (match-string 2 url) "@" (match-string 1 url)))))
			(list
			 :handle handle
			 :url (if (string-match org-link-bracket-re url) (match-string 1 url) url)
			 :links (reverse (mapcar (lambda (o) (org-element-property :raw-link o))
															 (my-org-get-links-in-region beg end)))
			 :text (string-trim (buffer-substring-no-properties beg end))))))

(ert-deftest my-mastodon-get-note-info ()
 (should
	(equal
	 (with-temp-buffer
		 (insert "** SOMEDAY https://mastodon.online/@jcastp/111762105597746747         :news:
:PROPERTIES:
:CREATED:  [2024-01-22 Mon 05:51]
:END:

jcastp@mastodon.online - I've shared my emacs config: https://codeberg.org/jcastp/emacs.d

After years of reading other's configs, copying really useful snippets, and tinkering a little bit myself, I wanted to give something back, although I'm still an amateur (and it shows, but I want to improve!)

If you can find there something you can use, then I'm happy to be useful to the community.

#emacs
")
		 (org-mode)
		 (my-mastodon-get-note-info))
	 '(:handle "@jcastp@mastodon.online"
						 :url
						 "https://mastodon.online/@jcastp/111762105597746747"
						 :links
						 ("https://codeberg.org/jcastp/emacs.d")
						 :text
						 "jcastp@mastodon.online - I've shared my emacs config: https://codeberg.org/jcastp/emacs.d\n\nAfter years of reading other's configs, copying really useful snippets, and tinkering a little bit myself, I wanted to give something back, although I'm still an amateur (and it shows, but I want to improve!)\n\nIf you can find there something you can use, then I'm happy to be useful to the community.\n\n#emacs"))))
;; Collecting Emacs News from Mastodon:5 ends here

;; [[file:../Sacha.org::#copy-mastodon-link-for-emacs-news][Copy Mastodon link for Emacs News:1]]
;;;###autoload
(defun my-mastodon-copy-link-dwim (prefix)
	(interactive "P")
	(if prefix
			(mastodon-toot--copy-toot-url)
		(my-mastodon-copy-toot-as-author-link)))

;;;###autoload
(defun my-emacs-news-copy-mastodon-link ()
	(interactive)
	(let ((url (org-entry-get (point) "ITEM")))
		(when (string-match "https://\\(.+?\\)/\\(@.+?\\)/" url)
			(kill-new (org-link-make-string url (concat (match-string 2 url) "@" (match-string 1 url)))))))

;;;###autoload
(defun my-emacs-news-copy-mastodon-item (&optional name-only)
	(interactive (list current-prefix-arg))
	(let (s)
		(with-current-buffer
				(if (string-match "emacs-news/index.org" (buffer-file-name))
						(save-window-excursion
							(other-window 1)
							(current-buffer))
					(current-buffer))
			(let ((url (or (thing-at-point 'url)
										 (progn
											 (save-restriction
												 (org-back-to-heading)
												 (org-narrow-to-subtree)
												 (org-end-of-meta-data)
												 (if (re-search-forward org-link-any-re nil t)
														 (thing-at-point 'url)
													 (setq name-only t)
													 (org-entry-get (point) "ITEM")
													 )))))
						(toot (org-entry-get (point) "ITEM"))
						attrib)
				(when (string-match org-link-bracket-re toot)
					(setq toot (match-string 1 toot)))
				(when (string-match "https://\\(.+?\\)/\\(@.+?\\)/" toot)
					(setq attrib (org-link-make-string toot
																						 (concat
																							(match-string 2 toot) "@" (match-string 1 toot)))))
				(setq s
							(if name-only
									(format " (%s)" attrib)
								(format "- %s (%s)\n"
												(org-link-make-string
												 url
												 (my-page-title url))
												attrib)))))
		(when (called-interactively-p 'any)
			(if (string-match "emacs-news/index.org" (buffer-file-name))
					(insert s)
				(kill-new s)))
		s))
;; Copy Mastodon link for Emacs News:1 ends here

;; [[file:../Sacha.org::#mastodon-combined-timeline][Combining Mastodon timelines using mastodon.el:1]]
;;;###autoload
(defun my-mastodon-fetch-posts-after (base-url after-date)
	"Page backwards through BASE-URL using max_id for all the posts after AFTER-DATE."
	(require 'plz)
	(require 'mastodon-http)
	(let ((results [])
				(url base-url)
				(use-mastodon-el (not (string-match "^http" base-url)))
				(json-array-type 'list)
				page filtered)
		(while url
			(setq page (if use-mastodon-el
										 (mastodon-http--get-json (mastodon-http--api url) nil :silent)
									 (seq-map (lambda (o)
															(cons (cons 'external t) o))
														(plz 'get url :as #'json-read)))
						filtered (seq-filter (lambda (o) (string< after-date (assoc-default 'created_at o)))
																 page))
			(if filtered
					(progn
						(setq results (seq-concatenate 'vector filtered results)
									url (concat base-url (if (string-match "\\?" base-url) "&" "?")
															"max_id=" (assoc-default 'id (elt (last page) 0))))
						(message "%s %s" (assoc-default 'created_at (elt (last page) 0)) url))
				(setq url nil)))
		results))

;;;###autoload
(defun my-mastodon-combined-tag-timeline (later-than tag &optional servers)
	"Display items after LATER-THAN about TAG from SERVERS and the current mastodon.el account."
	(interactive (list
								(org-read-date nil nil nil nil nil "-Mon")
								"#emacs"
								'("mastodon.social" "fosstodon.org")))
	(setq servers (or servers '("mastodon.social" "fosstodon.org")))
	(require 'mastodon)
	(require 'mastodon-tl)
	(require 'mastodon-toot)
	(if (stringp later-than)
			(setq later-than (org-read-date nil nil later-than)))
	(setq tag (replace-regexp-in-string "#" "" tag))
	(let* ((limit 40)
				 (sources (cons (format "timelines/tag/%s?limit=%d" tag limit)
												(mapcar (lambda (s)
																	(format "https://%s/api/v1/timelines/tag/%s?limit=%d" s tag limit))
																servers)))
				 (combined
					(seq-map
					 ;; remove edited_at
					 (lambda (o) (assoc-delete-all 'edited_at o))
					 (sort
						(seq-reduce (lambda (prev val)
													(seq-union prev
																		 (condition-case nil
																				 (my-mastodon-fetch-posts-after val later-than)
																			 (error nil))
																		 (lambda (a b) (string= (assoc-default 'uri a)
																														(assoc-default 'uri b)))))
												sources [])
						(lambda (a b)
							(string< (assoc-default 'created_at b)
											 (assoc-default 'created_at a)))))))
		(with-current-buffer (get-buffer-create "*Combined*")
			(let ((inhibit-read-only t))
				(erase-buffer)
				(mastodon-tl--timeline combined)
				(mastodon-mode))
			(setq mastodon-tl--buffer-spec `(account ,(cons mastodon-active-user mastodon-instance-url) buffer-name ,(buffer-name)))
			(display-buffer (current-buffer)))))
;; Combining Mastodon timelines using mastodon.el:1 ends here

;; [[file:../Sacha.org::#mastodon-combined-timeline][Combining Mastodon timelines using mastodon.el:5]]
;;;###autoload
(defun my-mastodon-update-external-item-id (&rest _)
	(when (mastodon-tl--field 'external (mastodon-tl--property 'item-json))
		;; ask the server to resolve it
		(let* ((response (mastodon-http--get-json (format "%s/api/v2/search" mastodon-instance-url)
																							`(("q" . ,(mastodon-toot--toot-url))
																								("resolve" . "t"))))
					 (id (alist-get 'id (seq-first (assoc-default 'statuses response))))
					 (inhibit-read-only t)
					 (json (get-text-property (point) 'item-json)))
			(when (and id json)
				(my-text-property-update-at-point (point) 'base-item-id id)
				(my-text-property-update-at-point (point) 'item-json
																					(progn
																						(setf (alist-get 'id json) id)
																						(setf (alist-get 'external json) nil)
																						json))))))
;; Combining Mastodon timelines using mastodon.el:5 ends here

;; [[file:../Sacha.org::#following-people][Following people:1]]
;;;###autoload
(defun my-mastodon-follow-user (user-handle)
	"Follow HANDLE."
	(interactive "MHandle: ")
	(require 'mastodon-profile)
	(when (string-match "https?://\\(.+?\\)/\\(@.+\\)" user-handle)
		(setq user-handle (concat (match-string user-handle) "@" (match-string 1 user-handle))))
	(let* ((account (mastodon-profile--search-account-by-handle
                   user-handle))
				 (user-id (alist-get 'id account))
				 (name (if (not (string-empty-p (alist-get 'display_name account)))
                   (alist-get 'display_name account)
								 (alist-get 'username account)))
				 (url (mastodon-http--api (format "accounts/%s/%s" user-id "follow"))))
		(if account
				(mastodon-tl--do-user-action-function url name user-handle "follow")
			(message "Cannot find a user with handle %S" user-handle))))
;; Following people:1 ends here

;; [[file:../Sacha.org::#mastodon-tooting-a-link-to-the-current-post][Tooting a link to the current post:2]]
;;;###autoload
(defun my-mastodon-11ty-toot-post ()
	"Compose a toot sharing this blog post on Mastodon."
	(interactive)
	(require 'mastodon)
	(require 'mastodon-toot)
	(let* ((info (my-11ty-post-plist))
				 (url (concat "https://sachachua.com" (plist-get info :permalink)))
				 (blog-text (my-11ty-post-text))
				 (title (plist-get info :title)))
		(mastodon-toot--compose-buffer
		 nil nil nil
		 (concat "[" (plist-get info :title) "]"
						 "(" url ") "
						 (mapconcat (lambda (tag) (concat "#" tag))
												(seq-remove (lambda (tag) (string-match "^_" tag))
																		(plist-get info :tags))
												" ")))
		(unless (string-match "^[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9] Emacs News" title)
			(my-mastodon-insert-interested-handles (concat title "\n" blog-text)))))
;; Tooting a link to the current post:2 ends here

;; [[file:../Sacha.org::#mastodon-toot-subtree][Compose a Mastodon toot with the current Org subtree:1]]
;;;###autoload
(defun my-mastodon-toot-subtree ()
	(interactive)
	(let ((body (org-export-as 'md t nil t))
				(link (org-entry-get (point) "EXPORT_ELEVENTY_PERMALINK")))
		(mastodon-toot)
		(save-excursion
			(insert body)
			(when link (insert "\n\nBlog post: <" my-blog-base-url link ">\n")))))
;; Compose a Mastodon toot with the current Org subtree:1 ends here

;; [[file:../Sacha.org::#posting-the-latest-screenshot-with-mastodon-el][Posting the latest screenshot with mastodon.el:1]]
;;;###autoload
(defun my-mastodon-toot-screenshot (&optional filename description)
	"Compose a buffer and attach the latest screenshot.
Prompt for a description and add that to the filename as well.
When called with a prefix argument, prompt for the file.
Use consult to provide a preview."
	(interactive
	 (let ((filename
					(if current-prefix-arg
							(consult--read
							 (my-combined-screenshots)
							 :sort nil
							 :require-match t
							 :category 'file
							 :state (lambda (candidate state)
												(when candidate
													(with-current-buffer (find-file-noselect
																								candidate)
														(display-buffer (current-buffer))))))
						(my-latest-screenshot))))
		 (list
			filename
			(when (string-match "^[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]_[0-9][0-9]-[0-9][0-9]-[0-9][0-9]$" (file-name-base filename))
				(display-buffer (find-file-noselect filename))
				(read-string "Description: ")))))
	(let ((new-filename (if (string= (or description "") "")
													nil
												(expand-file-name
												 (concat (file-name-base filename) " " description
																 (file-name-extension filename))
												 (file-name-directory filename)))))
		(if new-filename
				(rename-file filename new-filename))
		(unless (string-match "new toot" (buffer-name)) ; can't match off major mode yet
			(mastodon-toot))
		(mastodon-toot--attach-media
		 (or new-filename filename) "image/png"
		 (or description
				 (when (string-match "^[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]_[0-9][0-9]-[0-9][0-9]-[0-9][0-9] \\(.+\\)" (save-match-data (file-name-base filename)))
					 (match-string 1 (save-match-data (file-name-base filename))))))))
;; Posting the latest screenshot with mastodon.el:1 ends here

;; [[file:../Sacha.org::#mastodon-toot-config][Making it easier to toot my config:1]]
;;;###autoload
(defun my-mastodon-toot-config (&optional include-screenshot)
	"Toot this part of my config."
	(interactive (list current-prefix-arg))
	(let ((link (if (org-entry-get (point) "EXPORT_ELEVENTY_PERMALINK")
									(concat "https://sachachua.com" (org-entry-get (point) "EXPORT_ELEVENTY_PERMALINK"))
								(concat "https://sachachua.com/dotemacs/#" (org-entry-get (point) "CUSTOM_ID"))))
				text)
		(save-excursion
			(org-back-to-heading)
			(org-end-of-meta-data)
			(setq text (buffer-substring (point) (org-end-of-subtree))))
		(mastodon-toot)
		(insert text "\n\nLink: " link)))
;; Making it easier to toot my config:1 ends here

;; [[file:../Sacha.org::#mastodon-org-contacts-capture][Capture:1]]
;;;###autoload
(defun my-mastodon-org-contact-add ()
	"Add current toot author as a contact."
	(interactive)
	(let-alist (get-text-property (point) 'item-json)
		(with-current-buffer (find-file-noselect (car org-contacts-files))
			(if (org-find-property "MASTODON" .account.acct)
					(message "Already exists.")
				(org-insert-heading)
				(insert (format "%s\n:PROPERTIES:\n:NAME: %s\n:MASTODON: %s\n:ALIAS: %s\n:END:\n"
												.account.display_name
												.account.display_name
												.account.acct
												.account.username))
				(message "Added %s" .account.acct)))))
;; Capture:1 ends here

;; [[file:../Sacha.org::#mastodon-org-contacts-complete][Completion:1]]
;;;###autoload
(defun my-org-contacts-complete-mastodon (string)
	(let* ((completion-ignore-case org-contacts-completion-ignore-case)
				 (completion-list
					(cl-loop for contact in (org-contacts-filter)
									 ;; The contact name is always the car of the assoc-list
									 ;; returned by `org-contacts-filter'.
									 for contact-name = (car contact)
									 ;; Build the list of the Mastodon handles which have expired
									 for ignore-list = (org-contacts-split-property
																			(or (cdr (assoc-string org-contacts-ignore-property
																														 (nth 2 contact))) ""))
									 ;; Build the list of the user Mastodon handles.
									 for handle-list = (org-contacts-remove-ignored-property-values
																			ignore-list
																			(org-contacts-split-property
																			 (or (cdr (assoc-string "MASTODON"
																															(nth 2 contact))) "")))
									 nconc (cl-loop for handle in handle-list
																	collect (format "%s (%s)" contact-name handle))))
				 (completion-list (org-contacts-all-completions-prefix
													 string
													 (org-uniquify completion-list))))
		(when completion-list
			(org-contacts-make-collection-prefix completion-list))))

;;;###autoload
(defun my-mastodon-complete-contact ()
	"Suitable for adding to `completion-at-point-functions'."
	(interactive)
	(let ((beg
				 (save-excursion
					 (re-search-backward "\\(\\`\\|[\n:,]\\)[ \t]*")
					 (goto-char (match-end 0))
           (point)))
				(end (point)))
    (list beg
          end
          (completion-table-dynamic
           (lambda (string)
             (my-org-contacts-complete-mastodon string))))))
;; Completion:1 ends here

;; [[file:../Sacha.org::#mastodon-copy-mastodon-toot-url-as-author-link][Copy Mastodon toot URL as author link:1]]
;;;###autoload
(defun my-mastodon-copy-toot-as-author-link ()
	(interactive)
  (let* ((url (mastodon-toot--toot-url))
				 (handle (concat "@"
												 (let-alist (or (mastodon-tl--property 'base-toot)
																				(mastodon-tl--property 'item-json))
													 .account.acct))))
		;; figure out how to properly add to org-stored-links someday
		(kill-new (org-link-make-string url handle))
		(message "Link stored (%s, %s)." handle url)))
;; Copy Mastodon toot URL as author link:1 ends here

;; [[file:../Sacha.org::#mastodon-org-feed][Collect my recent toots in an Org file so that I can refile them:2]]
;;;###autoload
(defun my-mastodon-org-feed-formatter (entry)
	(concat "* " (pandoc-convert-stdio
								(dom-text (dom-by-tag
													 (with-temp-buffer
														 (insert "<item>"
																		 (plist-get entry :item-full-text)
																		 "</item>")
														 (xml-parse-region (point-min) (point-max)))
													 'description))
								"html" "org")
					"\n\n[" (format-time-string (cdr org-time-stamp-formats)
																			(date-to-time (plist-get entry :pubDate)))
"]\n" (plist-get entry :link)))

;;;###autoload
(defun my-org-feed-sort (pos entries)
	(save-excursion
    (goto-char pos)
    (when (looking-at org-complex-heading-regexp)
			(org-sort-entries nil ?T))))
;; Collect my recent toots in an Org file so that I can refile them:2 ends here

;; [[file:../Sacha.org::#mastodon-insert-statuses][Archive toots on my blog:1]]
;;;###autoload
(defun my-mastodon-format-my-toots-since (date)
	(require 'mastodon-auth)
	(format "#+begin_toot_archive\n%s\n#+end_toot_archive\n"
					 (mapconcat
						(lambda (o)
							(format "- %s\n%s\n\n"
											(org-link-make-string (assoc-default 'url o)
																						"(toot)"
																						;(assoc-default 'created_at o)
																						)
											(org-ascii--indent-string
											 (string-trim (pandoc-convert-stdio (assoc-default 'content o) "html" "org"))
											 2))
							;; (format "#+begin_quote\n#+begin_export html\n%s\n#+end_export\n#+end_quote\n\n%s\n\n"
							;; 				(assoc-default 'content o)
							;; 				(org-link-make-string (assoc-default 'url o) (assoc-default 'created_at o)))
							)
						(seq-filter
						 (lambda (o)
							 (string= (assoc-default 'visibility o) "public"))
						 (my-mastodon-fetch-posts-after
							(format "accounts/%s/statuses?count=40&exclude_reblogs=t" (mastodon-auth--get-account-id))
							date))
						"")))

;;;###autoload
(defun my-mastodon-insert-my-toots-since (date)
	(interactive (list (org-read-date nil nil nil "Since date: ")))
	(insert (my-mastodon-format-my-toots-since date)))

;;;###autoload
(defun my-mastodon-roundup (date)
	(interactive (list (org-read-date nil nil nil "Date of post: ")))
	(org-insert-heading)
	(let ((start (org-read-date nil nil "--wed" nil (date-to-time date))))
		(insert "Wednesday weblog: Toots ending " start " :review:weblog:\n\n")
		(my-mastodon-insert-my-toots-since start)))
;; Archive toots on my blog:1 ends here

(provide 'my-mastodon)
;;; my-mastodon.el ends here
