;;; sacha-org-contacts.el ---  -*- lexical-binding: t -*-

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



;; [[file:../Sacha.org::#templates][Templates:1]]
;;;###autoload
(defun sacha-org-contacts-template-email (&optional return-value)
  "Try to return the contact email for a template.
         If not found return RETURN-VALUE or something that would ask the user."
  (eval-when-compile (require 'gnus-art nil t))
  (eval-when-compile (require 'org-contacts nil t))
  (or (cadr (if (gnus-alive-p)
                (gnus-with-article-headers
                  (mail-extract-address-components
                   (or (mail-fetch-field "Reply-To") (mail-fetch-field "From") "")))))
      return-value
      (concat "%^{" org-contacts-email-property "}p")))
;; Templates:1 ends here

;; [[file:../Sacha.org::#org-contacts][Contacts:2]]
(defvar sacha-message-greet-contacts t "Non-nil means say hi.")

;;;###autoload
(defun sacha-message-greet-contacts-skip (fn &rest args)
	(let ((sacha-message-greet-contacts nil))
		(apply fn args)))

;;;###autoload
(defun sacha-message-greet-contacts ()
	(interactive)
	(when sacha-message-greet-contacts
		(let* ((emails
						(mapcar 'car
										(append
										 (mail-header-parse-addresses (message-fetch-field "To"))
										 (mail-header-parse-addresses (message-fetch-field "Cc")))))
					 (people
						(seq-keep
						 (lambda (email)
							 (cdr (assoc-string "NAME_SHORT"
																	(caddr (car (org-contacts-filter nil nil (cons "EMAIL" email)))))))
						 emails)))
			(when people
				(message-goto-body)
				(unless (re-search-forward "^Hi, " nil t)
					(insert "Hi, " (string-join people ",") "!\n\n"))))))

;; Contacts:2 ends here

;; [[file:../Sacha.org::#mastodon-mastodon-el-mention-people-based-on-regexp][mastodon.el: Mention people based on regexp:2]]
;;;###autoload
(defun sacha-org-contacts-all-alist ()
  "Return a list of all contacts in `org-contacts-files'.
Each element has the form (NAME . (FILE . POSITION))."
  (seq-mapcat
   (lambda (file)
     (unless (buffer-live-p (get-buffer (file-name-nondirectory file)))
       (find-file-noselect file))
     (with-current-buffer (find-file-noselect file)
       (org-map-entries
        (lambda ()
          (let* ((name (substring-no-properties (org-get-heading t t t t)))
                 (file (buffer-file-name))
                 (position (point))
                 (entry-properties (org-entry-properties position 'standard)))
            `(("NAME" . ,name)
              ("FILE" . ,file)
              ("POSITION" . ,position)
              ,@entry-properties))))))
   (org-contacts-files)))

;;;###autoload
(defun sacha-org-contacts-to-mention (text)
  (seq-filter
   (lambda (o) (and (assoc-default "MENTION_REGEXP" o #'string=)
                    (string-match (assoc-default "MENTION_REGEXP" o #'string=) text)))
   (sacha-org-contacts-all-alist)))
;; mastodon.el: Mention people based on regexp:2 ends here

;; [[file:../Sacha.org::#mastodon-mastodon-el-mention-people-based-on-regexp][mastodon.el: Mention people based on regexp:4]]
;;;###autoload
(defun sacha-org-contacts-best-contact (rec)
  (seq-find
   (lambda (type)
     (when (assoc-default type rec #'string=)
       type))
   '("MASTODON" "EMAIL" "X")))

;;;###autoload
(defun sacha-org-contacts-collect-context (contact text)
  "Return the list of links or keywords matching MENTION_REGEXP for CONTACT in TEXT."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (org-mode)
    (let ((regexp (assoc-default "MENTION_REGEXP" contact #'string=))
          results)
      (while (re-search-forward regexp nil t)
        (let ((elem (org-element-context)))
          (if (eq (org-element-type elem) 'link)
              (push (org-element-property :raw-link elem)
                    results)
            (push (sentence-at-point) results))))
      (nreverse results))))

;;;###autoload
(defun sacha-org-contacts-suggest-mentions (text url)
  (interactive (list (if (region-active-p) (buffer-substring (region-beginning) (region-end))
                       (sacha-11ty-post-text))
                     (sacha-11ty-post-url)))

  (when-let* ((people (seq-group-by #'sacha-org-contacts-best-contact (sacha-org-contacts-to-mention text))))
    (with-current-buffer (get-buffer-create "*mentions*")
      (erase-buffer)
      (org-mode)
      (dolist (type '("MASTODON" "EMAIL" "X"))
        (when (assoc-default type people #'string=)
          (insert "* " type "\n\n"
                  (mapconcat (lambda (o)
                               (assoc-default type o #'string=))
                             (assoc-default type people #'string=) ", ")
                  "\n"
                  (string-join
                   (seq-mapcat
                    (lambda (o)
                      (sacha-org-contacts-collect-context o text))
                    (assoc-default type people #'string=))
                   "\n")
                  (if url (concat " " url) "")
                  "\n\n")))
      (goto-char (point-min))
      ;; collect the links
      (pop-to-buffer (current-buffer)))))
;; mastodon.el: Mention people based on regexp:4 ends here

(provide 'sacha-org-contacts)
;;; sacha-org-contacts.el ends here
