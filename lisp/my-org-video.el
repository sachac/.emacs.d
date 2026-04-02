;;; my-org-video.el ---  -*- lexical-binding: t -*-

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
;; - Videos
;;   https://sachachua.com/dotemacs#videos
;;
;;; Code:



;; [[file:../Sacha.org::org-video-link][org-video-link]]
;;;###autoload
(defun my-org-video-follow (path _)
	(cond
	 ((string-match "\\(https://.+\\):\\([0-9:]+\\)" path)
		(mpv-start (concat (match-string 1 path) "?t=" (my-org-yt-convert-time (match-string 2 path)))))
	 ((string-match "https:" path)
		(mpv-start path))
	 ((string-match "\\(.+?\\):\\([0-9:]+\\)" path)
		(mpv-start (expand-file-name (match-string 1 path))
							 (concat "--start=+" (match-string 2 path))))
	 (t (mpv-play (expand-file-name (replace-regexp-in-string "\\?.*" "" path))))))

;;;###autoload
(defun my-org-video-replace-with-permalink ()
	(interactive)
	(let* ((elem (org-element-context))
				 (path (org-element-property :path elem))
				 (description (org-element-property :description elem))
				 (permalink (org-entry-get (point) "EXPORT_ELEVENTY_PERMALINK" t)))
		(delete-region (org-element-begin elem) (org-element-end elem))
		(insert (org-link-make-string (concat "video:https://sachachua.com" permalink (file-name-nondirectory path))
																	description))))

;;;###autoload
(defun my-org-video-export (link desc format info)
	"Export PATH to FORMAT using the specified wrap parameter."
	(if desc
			(org-export-string-as (org-link-make-string link desc) format)
		(pcase format
			((or 'html '11ty 'md)
			 (let* ((parsed-url (url-generic-parse-url link))
              (path-and-query (url-path-and-query parsed-url))
							(url
               (if (string-match "^https://" link)
                   (concat (url-type parsed-url) "://" (url-domain parsed-url) (car path-and-query))
                 (concat "file://" (if (file-name-absolute-p (car path-and-query))
																		   (expand-file-name (car path-and-query))
																	   (car path-and-query)))))
							(params (and (cdr path-and-query) (url-parse-query-string (cdr path-and-query))))
							body)
				 (setq body
							 (format
								"<video%s%s src=\"%s\" %stype=\"%s\">%s%s<a href=\"%s\">Download the video</a></video>%s"
								(if (string= (or (car (assoc-default "controls" params 'string= '("1"))) "1") "0")
										""
									" controls=\"1\"")
								(if (string= (or (car (assoc-default "autoplay" params 'string= '("0"))) "0") "0")
										""
									" autoplay=\"1\"")
								url
								(if (assoc-default "thumbnail" params)
										(format "poster=\"%s\" "
														(car (assoc-default "thumbnail" params)))
									"")
								(mailcap-file-name-to-mime-type (car path-and-query))
								(if (assoc-default "captions" params)
 										(format "<track kind=\"subtitles\" label=\"Captions\" src=\"%s\" srclang=\"en\" default></track>"
                            (if (string= (car (assoc-default "captions" params)) "t")
                                (concat (file-name-sans-extension url) ".vtt")
														  (expand-file-name (car (assoc-default "captions" params)))))
									"")
								(if (assoc-default "thumbnail" params)
										(format "<div>Video not supported. Thumbnail:<br /><img src=\"%s\" alt=\"Thumbnail\" /></div>"
														(car (assoc-default "thumbnail" params)))
									"")
								(car path-and-query)
								(if (assoc-default "captions-below" params)
										"<div class=\"captions\" style=\"display: none\"></div>"
									"")
								))
				 (when (assoc-default "caption" params)
					 (setq body (format "<figure>%s<figcaption><div>%s</div></figcaption></figure>"
															body
															(car (assoc-default "caption" params)))))
				 body))
			(_ link))))

;;;###autoload
(defun my-org-video-complete ()
	"Complete video reference."
	(interactive)
	(concat "video:" (read-file-name "File: ")))
;; org-video-link ends here

(provide 'my-org-video)
;;; my-org-video.el ends here
