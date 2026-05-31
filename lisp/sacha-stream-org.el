;;; sacha-stream-org.el ---  -*- lexical-binding: t -*-

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
;; - Create YouTube livestream broadcasts from Emacs Lisp
;;   https://sachachua.com/dotemacs#streaming-create-youtube-livestream-broadcasts-from-emacs-lisp
;;
;;; Code:



;; [[file:../Sacha.org::#streaming-create-youtube-livestream-broadcasts-from-emacs-lisp][Create YouTube livestream broadcasts from Emacs Lisp:2]]
;;;###autoload
(defun sacha-stream-org-schedule-livestream-for-entry-at-point ()
  "Schedule a livestream for the entry at point."
  (interactive)
	(let* ((time (save-excursion
								 (org-back-to-heading)
								 (org-end-of-meta-data t)
								 (when (re-search-forward
												org-element--timestamp-regexp
												(save-excursion (org-end-of-subtree)) t)
									 (org-timestamp-from-string (match-string 0)))))
				 (start-time (org-timestamp-to-time (org-timestamp-split-range time)))
				 (end-time (org-timestamp-to-time (org-timestamp-split-range time t)))
				 (response (sacha-stream-youtube-schedule-livestream
										:title (org-entry-get (point) "ITEM")
										:description
										(concat
										 (if (org-entry-get-with-inheritance "EXPORT_ELEVENTY_FILE_NAME")
												 (concat sacha-blog-base-url
																 (org-entry-get-with-inheritance "EXPORT_ELEVENTY_FILE_NAME")
																 "\n\n")
											 "")
										 (org-export-string-as (sacha-org-subtree-text) 'sacha-plain-text t))
										:time start-time
										:end-time end-time
										:privacy "public"
										:auto-start json-false
										:auto-stop json-false
										:thumbnail (or (org-entry-get (point) "THUMBNAIL") "~/proj/yay-emacs/thumbnail.png")))
				 (url (concat "https://youtube.com/live/" (alist-get 'id response))))
		(org-entry-put (point) "YOUTUBE_URL" url)
		(insert "yt:" url)
		(cl-pushnew
		 response
		 (alist-get 'items sacha-google-youtube-live-broadcasts))
		response))
;; Create YouTube livestream broadcasts from Emacs Lisp:2 ends here

(provide 'sacha-stream-org)
;;; sacha-stream-org.el ends here
