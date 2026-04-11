;;; sacha-org-captions.el ---  -*- lexical-binding: t -*-

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



;; [[file:../Sacha.org::org-captions-link][org-captions-link]]
(org-link-set-parameters
 "captions"
 :export #'sacha-org-captions-export
 :follow #'find-file
 :complete #'sacha-org-captions-complete)
;; org-captions-link ends here

;; [[file:../Sacha.org::#org-captions][Captions:2]]
;;;###autoload
(defun sacha-org-captions-format (file &optional separator)
	(let ((cues (subed-parse-file file)))
		(mapconcat (lambda (cue)
                 (concat
                  (if (and (elt cue 4) (not (string= (elt cue 4) "")))
                      (format "<div class=\"transcript-heading\"><span class=\"audio-time\" data-start=\"%f\">%s</span> <strong>%s</strong></div>"
                              (floor (/ (elt cue 1) 1000))
                              (format-seconds "%02h:%02m:%02s" (floor (/ (elt cue 1) 1000)))
                              (elt cue 4))
                    "")
								  (format "<span class=\"audio-time caption\" data-start=\"%f\" data-stop=\"%f\" >%s</span>"
												  (/ (elt cue 1) 1000.0)
												  (/ (elt cue 2) 1000.0)
												  (elt cue 3))))
							 cues (or separator " "))))

;;;###autoload
(defun sacha-org-captions-export (link desc format _)
	"Export PATH to FORMAT using the specified wrap parameter."
	(if desc
			(org-export-string-as (org-link-make-string link desc) format)
		(pcase format
			((or 'html '11ty 'md) (sacha-org-captions-format (car (url-path-and-query (url-generic-parse-url link)))))
			(_ path))))

;;;###autoload
(defun sacha-org-captions-complete ()
	"Complete audio reference."
	(interactive)
	(concat "captions:" (read-file-name "Captions: ")))

;;;###autoload
(defun sacha-org-captions-insert-as-html-block (file)
	(interactive "FFile: ")
	(insert "#+begin_export html\n" (sacha-org-captions-format file "\n") "\n#+end_export html\n"))
;; Captions:2 ends here

(provide 'sacha-org-captions)
;;; sacha-org-captions.el ends here
