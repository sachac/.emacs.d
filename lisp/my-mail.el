;;;###autoload
(defun my-async-smtpmail-send-it ()
  (let ((to          (message-field-value "To"))
        (buf-content (buffer-substring-no-properties
                      (point-min) (point-max))))
    (message "Delivering message to %s..." to)
    (async-start
     `(lambda ()
        (require 'smtpmail)
        (with-temp-buffer
          (insert ,buf-content)
          (set-buffer-multibyte nil)
          ;; Pass in the variable environment for smtpmail
          ,(async-inject-variables
            "\\`\\(smtpmail\\|async-smtpmail\\|user-mail\\)-\\|auth-sources\\|epg\\|nsm"
            nil "\\`\\(mail-header-format-function\\|smtpmail-address-buffer\\|mail-mode-abbrev-table\\)")
          (smtpmail-send-it)))
     `(lambda (&optional _ignore)
				(message "Delivering message to %s...done" ,to)))))

;;;###autoload
(defun my-message-add-blog-comment (url)
	(interactive (list (my-complete-blog-post-url)))
	(save-excursion
		(goto-char (point-min))
		(let* ((author (when (re-search-forward "Name you want.+?: \\(.+\\)" nil t)
										 (match-string 1)))
					 (message (when (re-search-forward "Message: *\n?" nil t)
											(read-string "Message: "
																	 (buffer-substring (match-end 0)
																										 (if (re-search-forward "Can I share your comment" nil t)
																												 (match-beginning 0)
																											 (point-max))))))
					 (date (format-time-string "%FT%T%z" (date-to-time (message-field-value "Date"))))
					 (new-comment
						`((author . ,author)
							(date . ,date)
							(message . ,(format "<div class=\"email-body\">%s</div>"
																	(org-export-string-as message 'html t)
																	)))))
			(find-file (my-11ty-add-blog-comment new-comment url)))))

;;;###autoload
(defun my-mailman-approve ()
  "Approve this mailing list message."
  (interactive)
	(goto-char (point-min))
	(when (re-search-forward "From: \\(\\(.+\\)-request@.*?\\)\nSubject: \\(confirm [0-9a-f]+\\)" nil t)
		(let* ((id (match-string 2)))
			(compose-mail (match-string 1) (match-string 3)
										`(("Approved" . ,(string-trim (shell-command-to-string
																									 (concat "pass " (match-string 2)))))))
			(message-send-and-exit))))

;;;###autoload
(defun my-mailman-discard ()
	"Discard the current message."
	(interactive)
	(goto-char (point-min))
	(when (re-search-forward "From: \\(\\(.+\\)-request@.*?\\)\nSubject: \\(confirm [0-9a-f]+\\)" nil t)
		(compose-mail (match-string 1) (match-string 3))
		(message-send-and-exit)))

;;;###autoload
(defun my-mailman-web (&optional list-id)
	"Open the web admin interface."
	(interactive
	 (list
		(if (and (derived-mode-p 'notmuch-show-mode)
						 (re-search-forward "\\(https://.+?/mailman/admindb/\\(.+\\)\\)" nil t))
			 (match-string 2)
		 (completing-read "List: " '("emacsconf-org" "emacsconf-org-private" "emacs-tangents"
																 "emacsconf-submit" "emacsconf-discuss"
																 "info-gnu-emacs")))))
	(goto-char (point-min))
	(browse-url (concat "https://lists.gnu.org/mailman/admindb/" list-id "?adminpw="
												(url-hexify-string (string-trim (shell-command-to-string
																												 (concat "pass " list-id)))))))
