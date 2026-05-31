;;; sacha-ox-11ty.el ---  -*- lexical-binding: t -*-

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
;; - Making podcasts easier to publish
;;   https://sachachua.com/dotemacs#11ty
;;
;;; Code:



;; [[file:../Sacha.org::*Making podcasts easier to publish][Making podcasts easier to publish:1]]
;;;###autoload
(defun sacha-org-11ty-add-podcast-frontmatter (front-matter info)
  "Add podcast-related frontmatter."
  (interactive)
	(when (org-entry-get-with-inheritance "AUDIO_URL")
		(setq front-matter (plist-put front-matter :audioUrl (org-entry-get-with-inheritance "AUDIO_URL")))
		(setq front-matter (plist-put front-matter :audioType "audio/mpeg"))
		(when (org-entry-get-with-inheritance "AUDIO_LENGTH")
			(setq front-matter (plist-put front-matter :audioLength (org-entry-get-with-inheritance "AUDIO_LENGTH")))))
	(when (org-entry-get-with-inheritance "POST_CLASS")
		(setq front-matter (plist-put front-matter :postClass (org-entry-get-with-inheritance "POST_CLASS"))))
	front-matter)

(defvar sacha-podcast-file-directory "~/proj/yay-emacs" "Directory with podcast files.")

;;;###autoload
(defun sacha-stream-add-podcast-info ()
	(interactive)
	(let ((slug (sacha-make-slug (org-entry-get (point) "ITEM"))))
		(when (file-exists-p (expand-file-name (concat slug ".mp3") sacha-podcast-file-directory))
			(org-entry-put (point) "AUDIO_LENGTH"
										 (number-to-string (file-attribute-size (file-attributes (expand-file-name (concat slug ".mp3") sacha-podcast-file-directory)))))
			(org-entry-put (point) "AUDIO_URL"
										 (format "https://archive.org/details/%s/%s.mp3" slug slug)))))
;; Making podcasts easier to publish:1 ends here

(provide 'sacha-ox-11ty)
;;; sacha-ox-11ty.el ends here
