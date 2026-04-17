;;; sacha-google.el ---  -*- lexical-binding: t -*-

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
;; - Emacs Lisp and NodeJS: Getting the bolded words from a section of a Google Document
;;   https://sachachua.com/dotemacs#writing-and-editing-learning-french-emacs-lisp-and-nodejs-getting-the-bolded-words-from-a-section-of-a-google-document
;;
;; - Calculate an Org timestamp's offset into a YouTube stream
;;   https://sachachua.com/dotemacs#streaming-make-chapter-markers-and-video-time-hyperlinks-easier-to-note-while-i-livestream-calculate-an-org-timestamp-s-offset-into-a-youtube-stream
;;
;;; Code:



;; [[file:../Sacha.org::#writing-and-editing-learning-french-emacs-lisp-and-nodejs-getting-the-bolded-words-from-a-section-of-a-google-document][Emacs Lisp and NodeJS: Getting the bolded words from a section of a Google Document:1]]
  (defvar sacha-google-doc-download-command
    (list "nodejs" (expand-file-name "~/bin/download-google-doc-html.cjs")))

;;;###autoload
  (defun sacha-google-doc-html (doc-id)
    (when (string-match "https://docs\\.google\\.com/document/d/\\(.+?\\)/" doc-id)
      (setq doc-id (match-string 1 doc-id)))
    (with-temp-buffer
      (apply #'call-process (car sacha-google-doc-download-command)
             nil t nil (append (cdr sacha-google-doc-download-command) (list doc-id)))
      (buffer-string)))

(require 'dom)
;;;###autoload
(defun sacha-google-doc-clean-html (html)
  "Remove links on spaces, replace Google links."
  (let ((dom (with-temp-buffer
               (insert html)
               (libxml-parse-html-region))))
    (dom-search
     dom
     (lambda (o)
       (when (eq (dom-tag o) 'a)
         (when (and (dom-attr o 'href)
                    (string-match "https://\\(www\\.\\)?google\\.com/url\\?q=" (dom-attr o 'href)))
           (let* ((parsed (url-path-and-query
                           (url-generic-parse-url (dom-attr o 'href))))
                  (params (url-parse-query-string (cdr parsed))))
             (dom-set-attribute o 'href (car (assoc-default "q" params #'string=)))))
         (let ((text (string= (string-trim (dom-text o)) "")))
           (when (string= text "")
             (setf (car o) 'span))))
       (when (and
              (string-match "font-weight:700" (or (dom-attr o 'style) ""))
              (not (string-match "font-style:normal" (or (dom-attr o 'style) ""))))
         (setf (car o) 'strong))
       (when (dom-attr o 'style)
         (dom-remove-attribute o 'style))))
    ;; bold text is actually represented as font-weight:700 instead
    (with-temp-buffer
      (svg-print dom)
      (buffer-string))))

;;;###autoload
  (defun sacha-google-doc-org (doc-id)
    "Return DOC-ID in Org Mode format."
    (pandoc-convert-stdio (sacha-google-doc-clean-html (sacha-google-doc-html doc-id)) "html" "org"))
;; Emacs Lisp and NodeJS: Getting the bolded words from a section of a Google Document:1 ends here

;; [[file:../Sacha.org::#streaming-make-chapter-markers-and-video-time-hyperlinks-easier-to-note-while-i-livestream-calculate-an-org-timestamp-s-offset-into-a-youtube-stream][Calculate an Org timestamp's offset into a YouTube stream:2]]
;;;###autoload
(defun sacha-google-youtube-stream-offset (time)
	"Return the offset from the start of the stream.
When called interactively, copy it."
	(interactive (list (sacha-org-time-at-point)))
	(when (and (stringp time)
						 (string-match org-element--timestamp-regexp time))
		(setq time (org-timestamp-to-time (org-timestamp-from-string (match-string 0 time)))))
	(let ((result
				 (emacstv-format-seconds (sacha-google-youtube-live-seconds-offset-from-start-of-stream
																	time))))
		(when (called-interactively-p 'any)
			(kill-new result)
			(message "%s" result))
		result))

(defvar sacha-google-access-token nil "Cached access token.")

;;;###autoload
(defun sacha-google-access-token ()
	"Return Google access token."
	(or sacha-google-access-token
			(setq sacha-google-access-token
						(string-trim (shell-command-to-string "gcloud auth application-default print-access-token")))))

(defvar sacha-google-youtube-live-broadcasts nil "Cache.")
(defvar sacha-google-youtube-stream-offset-seconds 10 "Number of seconds to offset.")

;;;###autoload
(defun sacha-google-youtube-live-broadcasts ()
	"Return the list of broadcasts."
	(or sacha-google-youtube-live-broadcasts
			(setq sacha-google-youtube-live-broadcasts
						(request-response-data
						 (request "https://www.googleapis.com/youtube/v3/liveBroadcasts?part=snippet&mine=true&maxResults=10"
							 :headers `(("Authorization" . ,(format "Bearer %s" (sacha-google-access-token))))
							 :sync t
							 :parser #'json-read)))))

(defun sacha-google-youtube-live-get-broadcast-at-time (time)
	"Return the broadcast encompassing TIME."
	(seq-find (lambda (o)
							(or
							 ;; actual
							 (and
								(alist-get 'actualStartTime (alist-get 'snippet o))
								(alist-get 'actualEndTime (alist-get 'snippet o))
								(not (time-less-p time (date-to-time (alist-get 'actualStartTime (alist-get 'snippet o)))))
								(time-less-p time (date-to-time (alist-get 'actualEndTime (alist-get 'snippet o)))))
							 ;; actual, not done yet
							 (and
								(alist-get 'actualStartTime (alist-get 'snippet o))
								(null (alist-get 'actualEndTime (alist-get 'snippet o)))
								(not (time-less-p time (date-to-time (alist-get 'actualStartTime (alist-get 'snippet o))))))
							 ;; scheduled
							 (and
								(null (alist-get 'actualStartTime (alist-get 'snippet o)))
								(null (alist-get 'actualEndTime (alist-get 'snippet o)))
								(not (time-less-p time (date-to-time (alist-get 'scheduledStartTime (alist-get 'snippet o))))))))
						(sort
						 (alist-get 'items (sacha-google-youtube-live-broadcasts))
						 :key (or
									 (alist-get 'actualStartTime (alist-get 'snippet o))
									 (alist-get 'scheduledStartTime (alist-get 'snippet o))))))

(defun sacha-google-youtube-live-seconds-offset-from-start-of-stream (wall-time)
	"Return number of seconds for WALL-TIME from the start of the stream that contains it.
Offset by `sacha-google-youtube-stream-offset-seconds'."
	(+ sacha-google-youtube-stream-offset-seconds
		 (time-to-seconds
			(time-subtract
			 wall-time
			 (date-to-time
				(alist-get 'actualStartTime
									 (alist-get 'snippet
															(sacha-google-youtube-live-get-broadcast-at-time wall-time))))))))

;;;###autoload
(defun sacha-google-clear-cache ()
	"Clear cached Google access tokens and data."
	(interactive)
	(setq sacha-google-access-token nil)
	(setq sacha-google-youtube-live-broadcasts nil))
;; Calculate an Org timestamp's offset into a YouTube stream:2 ends here

(provide 'sacha-google)
;;; sacha-google.el ends here
