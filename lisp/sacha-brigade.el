;;; sacha-brigade.el ---  -*- lexical-binding: t -*-

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
;; - Automating buttons
;;   https://sachachua.com/dotemacs#areas-transforming-html-clipboard-contents-with-emacs-to-smooth-out-mailchimp-annoyances-dates-images-comments-colours-automating-buttons
;;
;; - Removing sections
;;   https://sachachua.com/dotemacs#areas-transforming-html-clipboard-contents-with-emacs-to-smooth-out-mailchimp-annoyances-dates-images-comments-colours-transforming-html-removing-sections
;;
;; - Formatting calls to action
;;   https://sachachua.com/dotemacs#areas-transforming-html-clipboard-contents-with-emacs-to-smooth-out-mailchimp-annoyances-dates-images-comments-colours-transforming-html-formatting-calls-to-action
;;
;; - Changing link colours
;;   https://sachachua.com/dotemacs#areas-transforming-html-clipboard-contents-with-emacs-to-smooth-out-mailchimp-annoyances-dates-images-comments-colours-transforming-html-changing-link-colours
;;
;; - Just the headings
;;   https://sachachua.com/dotemacs#collaboration-transforming-html-clipboard-contents-with-emacs-to-smooth-out-mailchimp-annoyances-dates-images-comments-colours-just-the-headings
;;
;; - Wrapping it up
;;   https://sachachua.com/dotemacs#areas-transforming-html-clipboard-contents-with-emacs-to-smooth-out-mailchimp-annoyances-dates-images-comments-colours-transforming-html-wrapping-it-up
;;
;; - Getting a Google Docs draft ready for Mailchimp via Emacs and Org Mode
;;   https://sachachua.com/dotemacs#collaboration-bike-brigade-extract-information-from-google-docs-export-as-zipped-html
;;
;; - Bike Brigade: working with Mailchimp images
;;   https://sachachua.com/dotemacs#collaboration-bike-brigade-working-with-mailchimp-images
;;
;; - Emacs: Updating a Mailchimp campaign using a template, sending test e-mails, and scheduling it
;;   https://sachachua.com/dotemacs#collaboration-bike-brigade-updating-mailchimp-directly
;;
;;; Code:



;; [[file:../Sacha.org::#areas-transforming-html-clipboard-contents-with-emacs-to-smooth-out-mailchimp-annoyances-dates-images-comments-colours-automating-buttons][Automating buttons:1]]
;;;###autoload
(defun sacha-brigade-copy-signup-block (date)
	(interactive (list (if current-prefix-arg (org-read-date nil t nil "Date: ")
											 (org-read-date nil t "+Sun"))))
	(when (stringp date) (setq date (date-to-time date)))
	(let* ((newsletter-date (format-time-string "%Y-%m-%d" date))
				 (current-week (org-read-date nil t "++Mon" nil date))
				 (current-week-end (org-read-date nil t "++Sun" nil date))
				 (next-week (org-read-date nil t "+2Mon" nil date))
				 (next-week-end (org-read-date nil t "+3Sun" nil date))
				 result)
		(setq result (format
			"<table class=\"sign-up\" style=\"background-color: #223f4d; text-align: center; margin: auto; margin-top: 24px; margin-bottom: 12px;\"><tbody><tr><td><a href=\"https://dispatch.bikebrigade.ca/campaigns/signup?current_week=%s\" target=\"_blank\" class=\"sign-up mceButtonLink\" style=\"background-color:#223f4d;border-radius:0;border:2px solid #223f4d;color:#ffffff;display:block;font-family:'Helvetica Neue', Helvetica, Arial, Verdana, sans-serif;font-size:16px;font-weight:normal;font-style:normal;padding:16px 28px;text-decoration:none;text-align:center;direction:ltr;letter-spacing:0px\" rel=\"noreferrer\">SIGN UP NOW TO DELIVER %s-%s</a></td></tr></table>
<p style=\"text-align: center; font-family: 'Helvetica Neue', Helvetica, Arial, Verdana\"><a href=\"https://dispatch.bikebrigade.ca/campaigns/signup?current_week=%s\" style=\"color: #476584; margin-top: 12px; margin-bottom: 12px;\" target=\"_blank\">You can also sign up early to deliver %s-%s</a></p>"
			(format-time-string "%Y-%m-%d" current-week)
			(upcase (format-time-string "%b %-e" current-week))
			(upcase (format-time-string
			 (if (string= (format-time-string "%m" current-week)
										(format-time-string "%m" current-week-end))
					 "%-e"
				 "%b %-e")
			 current-week-end))
			(format-time-string "%Y-%m-%d" next-week)
			(format-time-string "%b %-e" next-week)
			(format-time-string
			 (if (string= (format-time-string "%m" next-week)
										(format-time-string "%m" next-week-end))
					 "%-e"
				 "%b %-e")
			 next-week-end)))
		(when (called-interactively-p 'any)
			(kill-new result)
			(shell-command "xdotool search  --onlyvisible --all Chrome windowactivate windowfocus"))
		result))
;; Automating buttons:1 ends here

;; [[file:../Sacha.org::#areas-transforming-html-clipboard-contents-with-emacs-to-smooth-out-mailchimp-annoyances-dates-images-comments-colours-transforming-html-removing-sections][Removing sections:1]]
(defvar sacha-brigade-section nil)
;;;###autoload
(defun sacha-brigade-remove-meta-recursively (node &optional recursing)
	"Remove <h1>Meta</h1> headings in NODE and the elements that follow them.
Resume at the next h1 heading."
	(unless recursing (setq sacha-brigade-section nil))
	(cond
	 ((eq (dom-tag node) 'h1)
		(setq sacha-brigade-section (string-trim (dom-texts node)))
		(if (string= sacha-brigade-section "Meta")
				nil
			node))
	 ((string= sacha-brigade-section "Meta")
		nil)
	 (t
		(let ((processed
					 (seq-keep
						(lambda (child)
							(if (stringp child)
									(unless (string= sacha-brigade-section "Meta")
										child)
								(sacha-brigade-remove-meta-recursively child t)))
						(dom-children node))))
			`(,(dom-tag node) ,(dom-attributes node) ,@processed)))))
;; Removing sections:1 ends here

;; [[file:../Sacha.org::#areas-transforming-html-clipboard-contents-with-emacs-to-smooth-out-mailchimp-annoyances-dates-images-comments-colours-transforming-html-formatting-calls-to-action][Formatting calls to action:1]]
;;;###autoload
(defun sacha-brigade-format-buttons (dom)
	(dolist (node (dom-by-tag dom 'a))
		(let ((text (dom-texts node)))
			(if (string-match "\\[ *\\(.+?\\) *\\]" text)
					;; button, wrap in a table
					(with-temp-buffer
						(insert
						 (format "<table><tbody><tr><td style=\"padding: 12px 0 12px 0\"><div style=\"margin-top: 12px\"><table align=\"center\" border=\"0\" cellpadding=\"0\" cellspacing=\"0\" role=\"presentation\" class=\"mceButtonContainer\" style=\"padding-top: 24px; margin: auto; margin-top: 12px; text-align: center\"><tbody><tr class=\"mceStandardButton\"><td style=\"background-color:#000000;border-radius:0;margin-top:12px;text-align:center\" valign=\"top\" class=\"mceButton\"><a href=\"%s\" target=\"_blank\" class=\"mceButtonLink\" style=\"background-color:#000000;border-radius:0;border:2px solid #000000;color:#ffffff;display:block;font-family:'Helvetica Neue', Helvetica, Arial, Verdana, sans-serif;font-size:16px;font-weight:normal;font-style:normal;padding:16px 28px;text-decoration:none;text-align:center;direction:ltr;letter-spacing:0px\" rel=\"noreferrer\">%s</a></td></tr></tbody></table></td></tr></table>"
										 (dom-attr node 'href)
										 (match-string 1 text)))
						(let ((parent-paragraph (sacha-dom-closest dom node 'p)))
							(dom-add-child-before
							 (dom-parent dom parent-paragraph)
							 (car (dom-by-tag (libxml-parse-html-region (point-min) (point-max)) 'div))
							 parent-paragraph)
							(dom-remove-node dom parent-paragraph))))))
	dom)
;; Formatting calls to action:1 ends here

;; [[file:../Sacha.org::#areas-transforming-html-clipboard-contents-with-emacs-to-smooth-out-mailchimp-annoyances-dates-images-comments-colours-transforming-html-changing-link-colours][Changing link colours:1]]
(defvar sacha-brigade-community-text-style "color: #ffffff")
(defvar sacha-brigade-community-link-style "color: #aed9ef")
;;;###autoload
(defun sacha-brigade-recolor-recursively (node)
	"Change the colors of links and text in NODE.
Ignore links with the class mceButtonLink.
Uses `sacha-brigade-community-text-style' and `sacha-brigade-community-link-style'."
	(pcase (dom-tag node)
		('table node) ; pass through, don't recurse further
		('a						; change the colour
		 (unless (string= (or (dom-attr node 'class) "") "mceButtonLink")
			 (dom-set-attribute node 'style sacha-brigade-community-link-style))
		 node)
		(_
		 (let ((processed
						(seq-map
						 (lambda (child)
							 (if (stringp child)
									 (dom-node 'span `((style . ,sacha-brigade-community-text-style)) child)
								 (sacha-brigade-recolor-recursively child)))
						 (dom-children node))))
			 `(,(dom-tag node) ,(dom-attributes node) ,@processed)))))
;; Changing link colours:1 ends here

;; [[file:../Sacha.org::#collaboration-transforming-html-clipboard-contents-with-emacs-to-smooth-out-mailchimp-annoyances-dates-images-comments-colours-just-the-headings][Just the headings:1]]
;;;###autoload
(defun sacha-brigade-just-headings (dom)
	(let ((entries
				 (dom-node 'ul)))
		(dolist (tag (dom-by-tag dom 'h2))
			(let ((text (string-trim (dom-texts tag))))
				(unless (string= text "")
					(dom-append-child entries (dom-node 'li nil text)))))
		entries))
;; Just the headings:1 ends here

;; [[file:../Sacha.org::#areas-transforming-html-clipboard-contents-with-emacs-to-smooth-out-mailchimp-annoyances-dates-images-comments-colours-transforming-html-wrapping-it-up][Wrapping it up:1]]
;;;###autoload
(defun sacha-brigade-transform-html (&optional recolor file as-rich-text)
	(interactive (list nil (when current-prefix-arg (read-file-name "File: "))))
	(sacha-transform-html-clipboard
   "Chrome"
	 (append
		'(sacha-transform-html-remove-images
			sacha-transform-html-remove-italics
			sacha-brigade-remove-meta-recursively
			sacha-brigade-remove-styles
			sacha-brigade-format-buttons)
		(if recolor '(sacha-brigade-recolor-recursively)))
	 (when file
		 (with-temp-buffer (insert-file-contents file) (buffer-string)))
	 as-rich-text))

;;;###autoload
(defun sacha-brigade-transform-community-html (&optional file as-rich-text)
	(interactive (list (when current-prefix-arg (read-file-name "File: "))))
	(sacha-brigade-transform-html t file as-rich-text))

;;;###autoload
(defun sacha-brigade-transform-just-headings (&optional file as-rich-text)
	(interactive (list (when current-prefix-arg (read-file-name "File: "))))
	(sacha-transform-html-clipboard
   "Chrome"
	 '(sacha-brigade-just-headings)
	 (when file
		 (with-temp-buffer (insert-file-contents file) (buffer-string)))
	as-rich-text))

;; Wrapping it up:1 ends here

;; [[file:../Sacha.org::#collaboration-bike-brigade-extract-information-from-google-docs-export-as-zipped-html][Getting a Google Docs draft ready for Mailchimp via Emacs and Org Mode:1]]
;;;###autoload
(defun sacha-brigade-process-latest-newsletter-draft (date)
	"Create an Org file with the HTML for different blocks."
	(interactive (list (if current-prefix-arg (org-read-date nil t nil "Date: ")
											 (org-read-date nil t "+Sun"))))
	(when (stringp date) (setq date (date-to-time date)))
	(let ((default-directory "~/Downloads/newsletter")
				file
				dom
				sections)
		(call-process "unzip" nil nil nil "-o" (sacha-latest-file "~/Downloads" "\\.zip$"))
		(setq file (sacha-latest-file default-directory))
		(with-temp-buffer
			(insert-file-contents-literally file)
			(goto-char (point-min))
			(setq dom (sacha-brigade-simplify-html (libxml-parse-html-region (point-min) (point-max))))
			(sacha-brigade-save-newsletter-images dom)
			(setq sections
						(sacha-html-group-by-tag
						 'h1
						 (dom-children
							(dom-by-tag
							 dom 'body)))))
		(with-current-buffer (get-buffer-create "*newsletter*")
			(erase-buffer)
			(org-mode)
			(insert
			 (format-time-string "%B %-e, %Y" date) "\n"
			 "* In this e-mail\n#+begin_src html\n"
			 "<p>Hi Bike Brigaders! Here’s what's happening this week, with quick signup links. In this e-mail:</p>"
			 (replace-regexp-in-string
				"<li>" "\n<li>"
				(with-temp-buffer
					(svg-print
					 (apply 'dom-node
									'ul nil
									(append
									 (sacha-brigade-toc-items (assoc-default "Bike Brigade" sections 'string=))
									 (sacha-brigade-toc-items (assoc-default "In our community" sections 'string=)))))
					(buffer-string)))
			 "\n<br />\n"
			 (sacha-brigade-copy-signup-block date)
			 "\n#+end_src\n\n")
			(dolist (sec '("Bike Brigade" "In our community"))
				(insert "* " sec "\n"
								(mapconcat
								 (lambda (group)
									 (let* ((item (apply 'dom-node 'div nil
																			 (append
																				(list (dom-node 'h2 nil (car group)))
																				(cdr group))))
													(image (sacha-brigade-image (car group))))
										 (format "** %s\n\n%s\n%s\n\n#+begin_src html\n%s\n#+end_src\n\n"
														 (car group)
														 (if image (org-link-make-string (concat "copy:" image)) "")
														 (or (sacha-html-last-link-href item) "")
														 (sacha-transform-html
															(delq nil
																		(list
																		 'sacha-transform-html-remove-images
																		 'sacha-transform-html-remove-italics
																		 'sacha-brigade-format-buttons
																		 (when (string= sec "In our community")
																			 'sacha-brigade-recolor-recursively)))
															item))))
								 (sacha-html-group-by-tag 'h2 (cdr (assoc sec sections 'string=)))
								 "")))
			(insert "* Other updates\n"
							(format "#+begin_src html\n<h2>Other updates</h2>%s\n#+end_src\n\n"
											(sacha-transform-html
											 '(sacha-transform-html-remove-images
												 sacha-transform-html-remove-italics)
											 (car (cdr (assoc "Other updates" sections 'string=))))))
			(goto-char (point-min))
			(display-buffer (current-buffer)))))

;;;###autoload
(defun sacha-brigade-toc-items (section-children)
	"Return a list of <li /> nodes."
	(mapcar
	 (lambda (group)
		 (let* ((text (dom-texts (cadr group)))
						(regexp (format "^%s \\([A-Za-z]+ [0-9]+\\)"
														(regexp-opt '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))))
						(match (when (string-match regexp text) (match-string 1 text))))
			 (dom-node 'li nil
								 (org-html-encode-plain-text
									(if match
											(format "%s: %s" match (car group))
										(car group))))))
	 (sacha-html-group-by-tag 'h2 section-children)))

;;;###autoload
(defun sacha-html-group-by-tag (tag dom-list)
	"Use TAG to divide DOM-LIST into sections. Return an alist of (section . children)."
	(let (section-name current-section results)
		(dolist (node dom-list)
			(if (and (eq (dom-tag node) tag)
							 (not (string= (string-trim (dom-texts node)) "")))
					(progn
						(when current-section
							(push (cons section-name (nreverse current-section))  results)
							(setq current-section nil))
						(setq section-name (string-trim (dom-texts node))))
				(when section-name
					(push node current-section))))
		(when current-section
			(push (cons section-name (reverse current-section))  results)
			(setq current-section nil))
		(nreverse results)))

;;;###autoload
(defun sacha-html-last-link-href (node)
	"Return the last link HREF in NODE."
	(dom-attr (car (last (dom-by-tag node 'a))) 'href))

;;;###autoload
(defun sacha-brigade-image (heading)
	"Find the latest image related to HEADING."
	(car
	 (nreverse
		(directory-files sacha-brigade-newsletter-images-directory
												t (regexp-quote (sacha-brigade-newsletter-heading-to-image-file-name heading))))))

;; Getting a Google Docs draft ready for Mailchimp via Emacs and Org Mode:1 ends here

;; [[file:../Sacha.org::#collaboration-bike-brigade-working-with-mailchimp-images][Bike Brigade: working with Mailchimp images:2]]
;;;###autoload
(defun sacha-brigade-reuse-or-upload-images (images)
  "Return an updated list of (section . images)."
	(let* ((base-regexp "^.+?-news-\\|\\(\\.[0-9][0-9]\\)?\\.\\(jpg\\||png\\)$")
         (recent-files
          (mapcar
           (lambda (o)
             (cons
              (cons 'base
                    (replace-regexp-in-string
                     base-regexp ""
                     (file-name-base (assoc-default 'name o))))
              o))
           (assoc-default 'files (mailchimp-recent-files 100))))
         results)
		(mapcar
		 (lambda (section)
       (setcdr
        section
        (mapcar
         (lambda (image)
			     (let* ((base
                   (if (eq (alist-get 'type image) 'emoji)
                       (replace-regexp-in-string "^:\\|:$" "" (alist-get 'alt image))
                     (and (assoc-default 'filename image)
                          (replace-regexp-in-string
                           base-regexp ""
                           (file-name-base (alist-get 'filename image))))))
                  (existing (and base (seq-find (lambda (o)
                                                  (string= base (alist-get 'base o)))
                                                recent-files))))
             (when base
               (let ((url
                      (if existing
                          (assoc-default 'full_size_url existing)
                        (alist-get
                         'full_size_url
                         (mailchimp-upload-file
                          (alist-get 'filename image)
                          (if (eq (alist-get 'type image) 'emoji)
                              (concat
                               (replace-regexp-in-string "^:\\|:$" "" (alist-get 'alt image))
                               "."
                               (file-name-extension (alist-get 'filename image)))))))))
				         (push (cons 'url url) image)
                 (push (cons (file-name-base (alist-get 'filename image)) url) results)))
				     image))
         (cdr section)))
       section)
		 images)))

;;;###autoload
(defun sacha-brigade-toc (sections)
	(replace-regexp-in-string
	 "<li>" "\n<li>"
	 (with-temp-buffer
		 (svg-print
			(apply 'dom-node
						 'ul nil
						 (append
							(sacha-brigade-toc-items (assoc-default "Bike Brigade" sections 'string=))
							(sacha-brigade-toc-items (assoc-default "In our community" sections 'string=)))))
		 (buffer-string))))

;;;###autoload
(defun sacha-brigade-remove-section-images (item)
  ;; remove the last image, because that's the image for the next section
  (when-let* ((last-image (car (last (dom-by-tag item 'img)))))
    (dom-remove-node item last-image))
  item)

;;;###autoload
(defun sacha-brigade-format-section (section images &optional recolor)
	(mapconcat
	 (lambda (group)
		 (let* ((item (apply 'dom-node 'div nil
												 (append
													(list (dom-node 'h2 nil (car group)))
													(cdr group))))
						(images (assoc-default (car group) images 'string=))
            (main-image (seq-find (lambda (o) (eq (alist-get 'type o) 'main)) images))
            (extra-image (seq-find (lambda (o) (eq (alist-get 'type o) 'extra)) images))
						(call-to-action (dom-attr (dom-search item (lambda (o)
																												 (and (eq (dom-tag o) 'a)
																															(string-match "^\\[ .+ \\]" (dom-texts o)))))
																			'href)))
       (if main-image
			     (format "<table width=\"100%%\" border=\"0\" cellspacing=\"0\" cellpadding=\"0\" align=\"center\" style=\"margin-top: 12px; margin-bottom: 12px;\"><tbody><tr class=\"mceRow\"><td colspan=\"1\" rowspan=\"1\" style=\"background-position:center;background-repeat:no-repeat;background-size:cover\" valign=\"top\"><table width=\"100%%\" border=\"0\" cellspacing=\"0\" cellpadding=\"0\"><tbody><tr><td colspan=\"12\" rowspan=\"1\" valign=\"top\" width=\"100%%\" class=\"mceColumn\" id=\"mceColumnId--38\"><table width=\"100%%\" border=\"0\" cellspacing=\"0\" cellpadding=\"0\"><tbody><tr><td colspan=\"1\" rowspan=\"1\" style=\"border:0;border-radius:0\" valign=\"top\" id=\"b812\"><table width=\"100%%\" border=\"0\" cellspacing=\"0\" cellpadding=\"0\" align=\"center\"><tbody><tr class=\"mceRow\"><td colspan=\"1\" rowspan=\"1\" style=\"background-position:center;background-repeat:no-repeat;background-size:cover;padding-top:0px;padding-bottom:0px\" valign=\"top\"><table style=\"table-layout:fixed\" width=\"100%%\" border=\"0\" cellspacing=\"24\" cellpadding=\"0\"><tbody><tr><td colspan=\"6\" rowspan=\"1\" style=\"padding-top:0;padding-bottom:0\" valign=\"top\" width=\"50%%\" class=\"mceColumn\" id=\"mceColumnId-809\"><table width=\"100%%\" border=\"0\" cellspacing=\"0\" cellpadding=\"0\"><tbody><tr><td colspan=\"1\" rowspan=\"1\" style=\"background-color:transparent;border:0;border-radius:0\" valign=\"top\" class=\"mceImageBlockContainer\" id=\"b808\"><table style=\"border-collapse:separate;margin:0;vertical-align:top;max-width:100%%;width:100%%;height:auto\" width=\"100%%\" border=\"0\" cellspacing=\"0\" cellpadding=\"0\" align=\"center\"><tbody><tr><td colspan=\"1\" rowspan=\"1\" style=\"border:0;border-radius:0;margin:0\" valign=\"top\">%s</td></tr></tbody></table></td></tr></tbody></table></td><td colspan=\"6\" rowspan=\"1\" style=\"padding-top:0;padding-bottom:0\" valign=\"top\" width=\"50%%\" class=\"mceColumn\" id=\"mceColumnId-811\"><table width=\"100%%\" border=\"0\" cellspacing=\"0\" cellpadding=\"0\"><tbody><tr><td colspan=\"1\" rowspan=\"1\" style=\"padding:12px\" valign=\"top\" class=\"mceGutterContainer\"><table style=\"border-collapse:separate\" width=\"100%%\" border=\"0\" cellspacing=\"0\" cellpadding=\"0\"><tbody><tr><td colspan=\"1\" rowspan=\"1\" style=\"padding-top:0;padding-bottom:0;padding-right:0;padding-left:0;border:0;border-radius:0\" valign=\"top\" id=\"b810\"><table style=\"border:0;background-color:transparent;border-radius:0;border-collapse:separate\" width=\"100%%\"><tbody><tr><td colspan=\"1\" rowspan=\"1\" qqstyle=\"padding-left:24px;padding-right:24px;\" class=\"mceTextBlockContainer\">%s</td></tr></tbody></table></td></tr></tbody></table></td></tr></tbody></table></td></tr></tbody></table></td></tr></tbody></table></td></tr></tbody></table></td></tr></tbody></table></td></tr>%s</tbody></table>"
							     (if call-to-action
									     (format "<a href=\"%s\" tabindex=\"-1\" style=\"display: block;\"><span style=\"background-color: transparent\"><img src=\"%s\" alt=\"%s\" style=\"padding-top: 12px; display:block;max-width:100%%;height:auto;border-radius:0\" width=\"306\" height=\"auto\" class=\"imageDropZone mceImage\"></span></a>"
													     call-to-action
													     (assoc-default 'url main-image)
													     (if (not (string= (assoc-default 'alt main-image) ""))
															     (assoc-default 'alt main-image)
														     (car group)))
								     (format "<img src=\"%s\" alt=\"%s\" style=\"display:block; padding-top: 12px; width:100%%; max-width:100%%;height:auto;border-radius:0\" width=\"306\" height=\"auto\" class=\"imageDropZone mceImage\">"
												     (assoc-default 'url main-image)
												     (if (not (string= (assoc-default 'alt main-image) ""))
														     (assoc-default 'alt main-image)
													     (car group))))
							     (sacha-transform-html
								    (delq nil
											    (list
											     'sacha-transform-html-remove-italics
											     'sacha-brigade-format-buttons
											     (when recolor
												     'sacha-brigade-recolor-recursively)))
								    item)
                   ;; extra images?
                   (if extra-image
                       (format "<tr><td colspan=\"2\" style=\"padding-top: 12px\"><img src=\"%s\" style=\"width: 100%%; max-width: 100%%\" alt=\"%s\"></td></tr>" (assoc-default 'url extra-image) (assoc-default 'alt extra-image nil ""))
                     ""))
         ;; No images
         (format "<table width=\"100%%\" border=\"0\" cellspacing=\"0\" cellpadding=\"0\" align=\"center\" style=\"margin-top: 12px; margin-bottom: 12px;\"><tbody>%s%s</tbody></table>"
                 (format "<tr><td colspan=\"2\" style=\"padding-top: 12px\">%s</td></tr>"
                         (sacha-transform-html
								          (delq nil
											          (list
											           'sacha-transform-html-remove-italics
											           'sacha-brigade-format-buttons
											           (when recolor
												           'sacha-brigade-recolor-recursively)))
								          item))
                 ;; extra images?
                 (if extra-image
                     (format "<tr><td colspan=\"2\" style=\"padding-top: 12px\"><img src=\"%s\" alt=\"%s\" style=\"width: 100%%; max-width: 100%%\"></td></tr>" (assoc-default 'url extra-image) (assoc-default 'alt extra-image nil ""))
                   "")
                 ))
       ))
	 (sacha-html-group-by-tag 'h2 section)
	 ""))

;;;###autoload
(cl-defun sacha-brigade-block (text &key (bg "#223f4d")
																 (style "padding-left:24px;padding-right:24px;padding-top:12px;padding-bottom:12px"))
	(format
	 "<table border=\"0\" cellpadding=\"0\" cellspacing=\"0\" width=\"100%%\" style=\"border-collapse:collapse\" role=\"presentation\"><tbody><tr><td style= \"padding-top:0;padding-bottom:0;padding-right:0;padding-left:0;border:0;border-radius:0\" valign=\"top\"><table width=\"100%%\" style= \"border:0;background-color:%s;border-radius:0\"><tbody><tr><td style=\"%s\" class=\"mceTextBlockContainer\"><div data-block-id=\"738\" class=\"mceText\" style= \"width:100%%\">%s</div></td></tr></tbody></table></td></tr></tbody></table>"
	 bg
	 style
	 text))

;;;###autoload
(defun sacha-brigade-update-images (dom images)
  "Replace image references.
IMAGES is an alist of (filename . URL)."
  (let ((all-images (mapcan 'cdr images))) ; todo: hash or alist
    (mapcar
     (lambda (node)
       (when-let* ((base (file-name-base (dom-attr node 'src)))
                   (url
                    (alist-get 'url
                               (seq-find
                                (lambda (o)
                                  (string= base
                                           (file-name-base (alist-get 'filename o))))
                                all-images))))
         (dom-set-attribute node 'src url)))
     (dom-by-tag dom 'img)))
  dom)

;;;###autoload
(defun sacha-brigade-process-latest-newsletter-draft-with-images (date)
	"Create an Org file with the HTML for different blocks."
	(interactive (list (if current-prefix-arg (org-read-date nil t nil "Date: ")
											 (org-read-date nil t "+Sun"))))
	(when (stringp date) (setq date (date-to-time date)))
	(let ((default-directory "~/proj/bike-brigade/temp_newsletter/")
				file
				images
				dom
				sections
				html)
                                        ;(call-process "unzip" nil nil nil "-o" (sacha-latest-file "~/Downloads" "\\.zip$"))
		(setq file (sacha-latest-file default-directory ".html"))
		(with-temp-buffer
			(insert-file-contents-literally file)
			(goto-char (point-min))
			(setq dom (libxml-parse-html-region (point-min) (point-max)))
			(setq images (sacha-brigade-reuse-or-upload-images
                    (sacha-brigade-save-newsletter-images dom)))
			(setq dom (sacha-brigade-simplify-html dom))
      (setq dom (sacha-brigade-update-images dom images))
			(setq sections
						(sacha-html-group-by-tag
						 'h1
						 (dom-children
							(dom-by-tag
							 dom 'body)))))
		(setq html
					(replace-regexp-in-string "<p><span></span></p>" ""
					                          (concat
					                           "<table class=\"newsletter\" margin=0 cellpadding=0 cellspacing=0 style=\"border-collapse:collapse\"><tbody><tr><td>"
					                           (sacha-brigade-block (format "<table style=\"margin: auto\"><tbody><tr><td style=\"text-align: center; color: #f3f3f3\"><div style=\"text-align: center; color: #f3f3f3\">%s</div></td></tr></table>"
																		                           (format-time-string "%B %-e, %Y" date))
														                           :bg "#16232a"
														                           :style "padding: 0px 24px 12px 24px")
					                           "<base href=\"\"><style>table { border-collapse: collapse !important } table.newsletter { border-collapse: collapse} .mceStandardButton a, table.sign-up a { text-decoration: none }</style><table><tbody><tr><td style=\"padding: 12px 24px 12px 24px\"><p>Hi Bike Brigaders! Here’s what's happening this week, with quick signup links. In this e-mail:</p>"
					                           (sacha-brigade-toc sections)
					                           ""
					                           (sacha-brigade-copy-signup-block date)
					                           (sacha-brigade-format-section (assoc-default "Bike Brigade" sections #'string=) images)
					                           "</td></tr></tbody></table><table style=\"background-color:#223f4d;\"><tbody><tr><td style=\"padding-left: 24px; padding-right: 24px\">"
					                           (sacha-brigade-block "<h1 style=\"text-align: center;\"><span style= \"color:#ffffff;\">In our community</span></h1>")
					                           (sacha-brigade-format-section (assoc-default "In our community" sections #'string=) images t)
					                           "</td></tr></tbody></table>"
					                           (if (assoc-default "Other updates" sections #'string=)
							                           (format "<table><tbody><tr><td style=\"padding: 12px 24px 12px 24px\"><h2>Other updates</h2>%s</td></tr></tbody></table>"
											                           (sacha-transform-html
												                          nil
												                          (car (assoc-default "Other updates" sections #'string=))))
						                           "")
					                           "</td></tr></tbody></table>")))
		(when (called-interactively-p 'any)
			(kill-new html))
		html))

;; Bike Brigade: working with Mailchimp images:2 ends here

;; [[file:../Sacha.org::#collaboration-bike-brigade-updating-mailchimp-directly][Emacs: Updating a Mailchimp campaign using a template, sending test e-mails, and scheduling it:1]]
;;;###autoload
(defun sacha-brigade-next-campaign (&optional date)
  (setq date (or date (org-read-date nil nil "+Sun")))
  (seq-find
   (lambda (o)
     (string-match (concat "^" date)
                   (alist-get 'title (alist-get 'settings o))))
   (alist-get 'campaigns (mailchimp-campaigns 5))))

(defvar sacha-bike-brigade-output-file nil)

;;;###autoload
(defun sacha-brigade-download-newsletter-from-google-docs ()
  "Download the newsletter from Google Docs and puts it in ~/proj/bike-brigade/temp_newsletter/."
  (interactive)
  (let ((default-directory "~/proj/bike-brigade"))
    (delete-directory "~/proj/bike-brigade/temp_newsletter" t)
    (with-current-buffer (get-buffer-create "*Newsletter*")
      (erase-buffer)
      (display-buffer (current-buffer))
      (call-process "node" nil t t "convert-newsletter.js" "download"))))

;;;###autoload
(defun sacha-brigade-create-or-update-campaign (&optional use-local)
  (interactive (list current-prefix-arg))
  (let* ((date (org-read-date nil nil "+Sun"))
         (template-name "Bike Brigade weekly update")
         (list-name "Bike Brigade")
         (template-id
          (alist-get
           'id
           (seq-find
            (lambda (o)
              (string= template-name (alist-get 'name o)))
            (alist-get 'templates (mailchimp--request-json "templates")))))
         (list-id (seq-find
                   (lambda (o)
                     (string= list-name
                              (alist-get 'name o)))
                   (alist-get 'lists (mailchimp--request-json "lists"))))
         (campaign (sacha-brigade-next-campaign date))
         (body `((type . "regular")
                 (recipients (list_id . ,(alist-get 'id list-id)))
                 (settings
                  (title . ,date)
                  (subject_line . "Bike Brigade: Weekly update")
                  (from_name . "Bike Brigade")
                  (reply_to . "info@bikebrigade.ca")
                  (tracking
                   (opens . t)
                   (html_clicks . t))))))
    (unless campaign
      (setq campaign (mailchimp--request-json
                      "/campaigns"
                      :method "POST"
                      :body
                      body)))
    ;; Download the HTML
    (if use-local
        (progn
          (make-directory "~/proj/bike-brigade/temp_newsletter/" t)
          (let ((default-directory "~/proj/bike-brigade/temp_newsletter/"))
            (call-process "unzip" nil (get-buffer-create "*Newsletter*") nil
                          (sacha-latest-file sacha-download-dir ".zip")))
          (let ((default-directory "~/proj/bike-brigade/"))
            (call-process "node" nil (get-buffer-create "*Newsletter*") nil
                          "convert-newsletter.js"
                          "resize")))
      (sacha-brigade-download-newsletter-from-google-docs))
    ;; Upload to Mailchimp
    (mailchimp-campaign-update-from-template
     (alist-get 'id campaign)
     template-id
     (list
      (cons "main_content_area"
            (sacha-brigade-process-latest-newsletter-draft-with-images
             date))))
    (when sacha-bike-brigade-output-file
      (with-temp-file sacha-bike-brigade-output-file
        (insert (alist-get 'html (mailchimp--request-json (format "/campaigns/%s/content" (alist-get 'id campaign)))))))
    (browse-url (concat "https://sachachua.com/bike-brigade/" (file-name-nondirectory sacha-bike-brigade-output-file)))
    (message "%s" "Done!")))
;; Emacs: Updating a Mailchimp campaign using a template, sending test e-mails, and scheduling it:1 ends here

;; [[file:../Sacha.org::#collaboration-bike-brigade-updating-mailchimp-directly][Emacs: Updating a Mailchimp campaign using a template, sending test e-mails, and scheduling it:2]]
(defvar sacha-brigade-test-emails nil "Set to a list of e-mail addresses.")
;;;###autoload
(defun sacha-brigade-send-test-to-me ()
  (interactive)
  (mailchimp-campaign-send-test-email (sacha-brigade-next-campaign) user-mail-address))

;;;###autoload
(defun sacha-brigade-send-test ()
  (interactive)
  (if sacha-brigade-test-emails
      (mailchimp-campaign-send-test-email (sacha-brigade-next-campaign) sacha-brigade-test-emails)
    (error "Set `sacha-brigade-test-emails'.")))
;; Emacs: Updating a Mailchimp campaign using a template, sending test e-mails, and scheduling it:2 ends here

;; [[file:../Sacha.org::#collaboration-bike-brigade-updating-mailchimp-directly][Emacs: Updating a Mailchimp campaign using a template, sending test e-mails, and scheduling it:3]]
;;;###autoload
(defun sacha-brigade-schedule ()
  (interactive)
  (let* ((campaign (sacha-brigade-next-campaign))
         (sched (format-time-string "%FT%T%z" (org-read-date t t "+Sun 11:00") t)))
    (mailchimp-campaign-schedule campaign sched)
    (message "Scheduled %s" (alist-get 'title (alist-get 'settings campaign)))))
;; Emacs: Updating a Mailchimp campaign using a template, sending test e-mails, and scheduling it:3 ends here

(provide 'sacha-brigade)
;;; sacha-brigade.el ends here
