;;; sacha-lisp.el ---  -*- lexical-binding: t -*-

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



;; [[file:../Sacha.org::#streaming-create-youtube-livestream-broadcasts-from-emacs-lisp][Create YouTube livestream broadcasts from Emacs Lisp:3]]
(defun sacha-date-to-time (time)
	"Return Emacs time object for TIME."
	(when (stringp time)
		(if (and (stringp time)
						 (boundp 'org-ts-regexp)
						 (string-match org-ts-regexp time))
				(setq time (org-timestamp-to-time (org-timestamp-from-string (match-string 0 time))))
			(setq time (date-to-time time))))
	time)

;;;###autoload
(defun sacha-date-to-iso-utc (time)
	"Return ISO8601 for TIME."
	(interactive (list (org-read-date t t nil "Time: ")))
	(let ((result (format-time-string "%FT%TZ" (sacha-date-to-time time) t)))
		(when (called-interactively-p 'any)
			(kill-new result)
			(message "%s" result))
		result))
;; Create YouTube livestream broadcasts from Emacs Lisp:3 ends here

(provide 'sacha-lisp)
;;; sacha-lisp.el ends here
