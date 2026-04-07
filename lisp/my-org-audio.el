;;; my-org-audio.el ---  -*- lexical-binding: t -*-

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
;; - Audio
;;   https://sachachua.com/dotemacs#audio
;;
;;; Code:



;; [[file:../Sacha.org::#audio][Audio:2]]

;;;###autoload
(defun my-org-audio-replace-with-permalink ()
	(interactive)
	(let* ((elem (org-element-context))
				 (path (org-element-property :path elem))
				 (description (org-element-property :description elem))
				 (permalink (org-entry-get (point) "EXPORT_ELEVENTY_PERMALINK" t)))
		(delete-region (org-element-begin elem) (org-element-end elem))
		(insert (org-link-make-string (concat "audio:https://sachachua.com" permalink (file-name-nondirectory path))
																	description))))

;;;###autoload
(defun my-org-audio-export (link desc format info)
	"Export PATH to FORMAT using the specified wrap parameter."
	(pcase format
		((or 'html '11ty 'md 'my-html-served)
		 (let* ((parsed-url (url-generic-parse-url link))
            (path-and-query (url-path-and-query parsed-url))
						(params (and (cdr path-and-query) (url-parse-query-string (cdr path-and-query))))
						(element (or (assoc-default "element" params #'string=) "audio"))
            (url (if (string-match "^https://" link)
                     (concat (url-type parsed-url) "://" (url-domain parsed-url) (car path-and-query))
                   (concat "file://"
                           (if (file-name-absolute-p (car path-and-query))
															 (expand-file-name (car path-and-query))
														 (car path-and-query)))))
            (is-icon (or (assoc-default "icon" params 'string=)
                         (string= desc "▶️"))))
       (if is-icon
           (format
				    "<a href=\"%s\" class=\"audio-icon\"%s%s>%s</a>"
				    (concat (car path-and-query)
                    (if (string= (or (assoc-default "nocache" params 'string= "0") "1") "1")
						            (concat "?" (format-time-string "%Y-%m-%d"))
					            ""))
            (if (not (string= "" (or (car (assoc-default "title" params 'string=)) "")))
                (format " title=\"%s\"" (htmlize-attr-escape (decode-coding-string (car (assoc-default "title" params 'string=)) 'utf-8)))
              "")
            (if (not (string= "" (or (car (assoc-default "style" params 'string=)) "")))
                (format " style=\"%s\"" (htmlize-attr-escape (car (assoc-default "style" params 'string=))))
              "")
            desc)
         (format
				  "<div class=\"audio\">%s<%s%s%s%s preload=\"metadata\" src=\"%s%s\" type=\"%s\"><a href=\"%s\">Download the audio</a>%s</%s>%s</div>"
          (if desc (concat desc " ") "")
				  element
				  (if (string= (or (assoc-default "controls" params 'string= "1") "1") "0")
						  ""
					  " controls=\"1\"")
				  (if (string= (or (assoc-default "autoplay" params 'string= "0") "0") "0")
						  ""
					  " autoplay=\"1\"")
				  (if (assoc-default "id" params)
						  (format " id=\"%s\"" (car (assoc-default "id" params)))
					  "")
				  (car path-and-query)
				  (if (string= (or (assoc-default "nocache" params 'string= "0") "1") "1")
						  (concat "?" (format-time-string "%Y-%m-%d"))
					  "")
				  (mailcap-file-name-to-mime-type (car path-and-query))
				  (car path-and-query)
				  (if (assoc-default "captions" params)
						  (format "<track kind=\"captions\" label=\"Captions\" src=\"%s\" srclang=\"en\" default></track>"
										  (if (string= (car (assoc-default "captions" params)) "t")
                          (concat (file-name-sans-extension url) ".vtt")
											  (expand-file-name (car (assoc-default "captions" params)))))
					  "")
				  element
				  (if (assoc-default "captions-below" params)
              "<div class=\"captions\" style=\"display: none\"></div>"
					  "")))))
    ('org
     (org-link-make-string (concat "audio:" link) desc))
		(_ path)))

(ert-deftest my-org-audio-export ()
  (should
	 (string-match
	  "<audio controls=\"1\" id=\"play-this\" src=\"test.opus\" type=\"audio/ogg\"><a href=\"test.opus\">Download the audio</a><track kind=\"subtitles\" label=\"Captions\" src=\"test.vtt\" srclang=\"en\" default></track></audio>"
	  (my-org-audio-export
		 "test.opus?id=play-this&captions=test.vtt"
		 nil
		 'html
		 nil
		 )
	  )))

;;;###autoload
(defun my-org-audio-complete ()
	"Complete audio reference."
	(interactive)
	(concat "audio:" (read-file-name "File: ") "?captions=t&captions-below=t"))

;;;###autoload
(defun my-org-audio-icon-complete ()
	"Complete audio reference."
	(interactive)
	(concat "audio:" (read-file-name "File: ") "?icon=t"))
;; Audio:2 ends here

(provide 'my-org-audio)
;;; my-org-audio.el ends here
