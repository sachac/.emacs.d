;;; sacha-youtube.el ---  -*- lexical-binding: t -*-

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
;; Related EmacsConfig sections:
;;
;; - Replace YouTube captions from Emacs Lisp
;;   https://sachachua.com/dotemacs#streaming-and-videos-replace-youtube-captions-from-emacs-lisp
;;
;;; Code:



;; [[file:../Sacha.org::#streaming-and-videos-replace-youtube-captions-from-emacs-lisp][Replace YouTube captions from Emacs Lisp:2]]
;;;###autoload
(defun sacha-youtube-replace-captions (url language vtt-file)
  "Replace captions for URL in LANGUAGE with VTT-FILE."
  (interactive
	 (let ((params (and (derived-mode-p 'subed-vtt-mode)
											(sacha-subed-record-youtube-params))))
		 (list
			(or (plist-get params :url)
					(read-string "YouTube URL: "))
			(or (plist-get params :language)
					(read-string "Language (ex: fr): "))
			(if (derived-mode-p 'subed-vtt-mode)
					(buffer-file-name)
				(read-file-name "VTT file: ")))))
  (let* ((video-id (sacha-org-yt-id url))
         (existing-captions (sacha-youtube-get-captions video-id))
         (boundary "---------------------------sacha_yt_caption_boundary"))
    (dolist (item existing-captions)
      (let ((snippet (alist-get 'snippet item)))
        (when (string= (alist-get 'language snippet) language)
          (sacha-youtube-delete-caption (alist-get 'id item)))))
    (let* ((metadata (json-encode `((snippet . ((videoId . ,video-id)
																								(language . ,language)
																								(name . ,language))))))
           (text (with-temp-buffer
									 (insert-file-contents vtt-file)
									 (buffer-string)))
           (body (concat "--" boundary "\r\n"
                         "Content-Type: application/json; charset=UTF-8\r\n\r\n"
                         metadata "\r\n"
                         "--" boundary "\r\n"
                         "Content-Type: text/vtt\r\n\r\n"
                         (encode-coding-string text 'utf-8) "\r\n"
                         "--" boundary "--\r\n"))
           (response (request-response-data
                      (request "https://www.googleapis.com/upload/youtube/v3/captions?uploadType=multipart&part=snippet"
                        :type "POST"
                        :headers `(("Authorization" . ,(format "Bearer %s" (sacha-google-access-token)))
                                   ("Content-Type" . ,(format "multipart/related; boundary=%s" boundary))
                                   ("Accept" . "application/json"))
                        :data body
                        :sync t
                        :parser #'json-read))))
			(prin1 response)
			(message "Uploaded.")
      response)))

(defun sacha-youtube-get-captions (video-id)
  "Return the list of existing caption tracks for VIDEO-ID."
  (let ((response (request-response-data
                   (request (format "https://www.googleapis.com/youtube/v3/captions?part=snippet&videoId=%s" video-id)
                            :headers `(("Authorization" . ,(format "Bearer %s" (sacha-google-access-token))))
                            :sync t
                            :parser #'json-read))))
    (alist-get 'items response)))

(defun sacha-youtube-delete-caption (caption-id)
  "Delete the caption track identified by CAPTION-ID."
  (request (format "https://www.googleapis.com/youtube/v3/captions?id=%s" caption-id)
           :type "DELETE"
           :headers `(("Authorization" . ,(format "Bearer %s" (sacha-google-access-token))))
           :sync t))

(defun sacha-subed-record-youtube-params ()
	"Return directives related to YouTube."
	(save-excursion
		(goto-char (point-min))
		(unless (subed-subtitle-msecs-start) (subed-forward-subtitle-start-pos))
		(list
		 :url (subed-record-get-directive "#+YOUTUBE_URL")
		 :language (or (subed-record-get-directive "#+LANGUAGE")
									 (progn (goto-char (point-min))
													(when (re-search-forward "^Language: \\(.+\\)\n" (or (save-excursion (re-search-forward "\n\n" nil t))
																																							 (point-max)))
														(match-string 1)))))))
;; Replace YouTube captions from Emacs Lisp:2 ends here

(provide 'sacha-youtube)
;;; sacha-youtube.el ends here
