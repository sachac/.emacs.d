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
;; Related Emacs config sections:
;;
;; - Captions
;;   https://sachachua.com/dotemacs#org-captions
;;
;;; Code:



;; [[file:../Sacha.org::#org-captions][Captions:2]]
;;;###autoload
(defun sacha-org-captions-format (file &optional separator format info)
	(let ((cues (subed-parse-file file))
				(dir (file-name-directory file))
				last-speaker
				first
				formatted-last-speaker)
		(when (plist-get info :skip)
			(setq cues (subed-record-filter-skips cues)))
		(pcase format
			((or 'html '11ty)
			 (concat "<div class=\"full-transcript\">"
							 (mapconcat
								(lambda (cue)
									(let ((text (elt cue 3))
												(without-directives (subed-record-remove-directives (elt cue 4)))
												(time (emacstv-format-seconds (floor (/ (elt cue 1) 1000))))
												break)
										(cond
										 ((string-match "^\\[\\(.+?\\)\\]: \\(.+\\)" text)
											(if (and (string= last-speaker (match-string 1 text))
															 (not without-directives))
													(setq text (replace-match "" nil nil text 0))
												(setq last-speaker (match-string 1 text))
												(setq formatted-last-speaker
															(format "<strong class=\"speaker-name\">%s:</strong> "
																			(save-match-data
																				(replace-regexp-in-string "^SPEAKER_" "" last-speaker))))
												(setq text
															(concat formatted-last-speaker " "
																			(save-match-data
																				(cond
																				 ((save-match-data (string-match "[~=*%]" (match-string 2 text)))
																					(match-string 2 text))
																				 ((save-match-data (string-match "`" (match-string 2 text)))
																					(replace-regexp-in-string
																					 "<p>\\|</p>\n" ""
																					 (pandoc-convert-stdio (match-string 2 text) "markdown" "html")))
																				 (t (match-string 2 text))))))
												(setq break t)))
										 ((and without-directives last-speaker)
											(setq text (concat formatted-last-speaker
																				 (save-match-data
																					 (cond
																						((string-match "[~=*%]" text)
																						 text)
																						((string-match "`" text)
																						 (replace-regexp-in-string
																							"<p>\\|</p>\n" ""
																							(pandoc-convert-stdio text "markdown" "html")))
																						(t text))))))
										 ((string-match "`" text)
											(setq text
														(replace-regexp-in-string
														 "<p>\\|</p>\n" ""
														 (pandoc-convert-stdio text "markdown" "html")))))
										(concat
										 (if (and without-directives (not (string= without-directives "")))
												 (format "<p></p><div class=\"transcript-heading\"><span class=\"audio-time\" data-start=\"%f\">%s</span> <strong>%s</strong></div>"
																 (floor (/ (elt cue 1) 1000))
																 time
																 without-directives)
											 "")
										 (if (and (null break) (null without-directives))
												 ""
											 "<p></p>"
											 ;; (format "<p></p><span class=\"audio-time caption just-time\" data-start=\"%f\" data-stop=\"%f\">%s</span> "
											 ;; 				(/ (elt cue 1) 1000.0)
											 ;; 				(/ (elt cue 2) 1000.0)
											 ;; 				time)
											 )
										 (if (subed-record-get-directive "#+SCREENSHOT" (elt cue 4))
												 (format "<a href=\"%s\"><img loading=\"lazy\" target=\"_blank\" src=\"%s\" alt=\"image from video %s\" data-time=\"%s\"/></a>"
																 (if (eq format '11ty)
																		 (expand-file-name
																			(subed-record-get-directive "#+SCREENSHOT" (elt cue 4))
																			dir)
																	 (subed-record-get-directive "#+SCREENSHOT" (elt cue 4)))
																 (if (eq format '11ty)
																		 (expand-file-name
																			(subed-record-get-directive "#+SCREENSHOT" (elt cue 4))
																			dir)
																	 (subed-record-get-directive "#+SCREENSHOT" (elt cue 4)))
																 (or (subed-record-get-directive "#+SCREENSHOT_TIME" (elt cue 4)) "")
																 (or (subed-record-get-directive "#+SCREENSHOT_TIME" (elt cue 4)) ""))
											 "")
										 (format "<span class=\"audio-time caption\" data-speaker=\"%s\" data-start=\"%f\" data-stop=\"%f\" >%s</span>"
														 (or last-speaker "")
														 (/ (elt cue 1) 1000.0)
														 (/ (elt cue 2) 1000.0)
														 text))))
								cues
								(or separator " "))
							 "</div>"))
			('latex
			 (format
				"\\newgeometry{left=2cm, right=5cm, marginparsep=3cm, marginparwidth=1.5cm}\n%s%s\n\\restoregeometry\n"
				(if (plist-get info :anchor)
						(format "\\label{%s}\n" anchor)
					"")
				(mapconcat
				 (lambda (group)
					 (setq first t)
					 (concat
						(if (car group)
								(format "\\subsection*{%s}\n\\addcontentsline{toc}{subsection}{%s}\n"
												(car group)
												(car group))
							"")
						(format "\\begin{transcript}\n%s\\end{transcript}\n"
										(string-trim
										 (mapconcat
											(lambda (cue)
												(let ((text (elt cue 3))
															(time (emacstv-format-seconds (floor (/ (elt cue 1) 1000))))
															break)
													(cond
													 ((string-match "^\\[\\(.+?\\)\\]: " text)
														(if (and (string= last-speaker (match-string 1 text))
																		 (not first))
																(setq text (replace-match "" nil nil text 0))
															(setq last-speaker (match-string 1 text))
															(setq formatted-last-speaker
																		(format "@@latex:\\item[\\textbf{%s:}] \\timestamp{%s}@@"
																						(save-match-data
																							(replace-regexp-in-string "^SPEAKER_" "" last-speaker))
																						(if (plist-get info :url)
																								(format "\\href{%s?start=%.3f}{%s}"
																												(plist-get info :url)
																												(/ (elt cue 1) 1000.0)
																												time)
																							time)))
															(setq text (replace-match formatted-last-speaker t t text))
															(setq break t)))
													 ((and first
																 formatted-last-speaker)
														(setq text (concat formatted-last-speaker text))))
													(setq first nil)
													(concat
													 (if (subed-record-get-directive "#+SCREENSHOT" (elt cue 4))
															 (format "\\item\\includegraphics[width=\\textwidth, height=\\textheight, keepaspectratio]{%s}\n"
																			 (expand-file-name
																				(subed-record-get-directive "#+SCREENSHOT" (elt cue 4))
																				dir))
														 "")
													 (cond
														((string-match "[~=*%]" text)
														 (org-export-string-as text format t info))
														((string-match "`" text)
														 (pandoc-convert-stdio text "markdown" "latex"))
														(t text)))))
											(cdr group)
											(or separator "\n"))))))
				 (subed-vtt-group-subtitles-by-chapter cues))))
			('odt
			 (replace-regexp-in-string
				"\\`\n*<text:p text:style-name=\\\"Text_20_body\\\">\\|</text:p>\n*\\'"
				""
				(org-export-string-as
				 (mapconcat
					(lambda (cue)
						(let ((text (elt cue 3))
									(without-directives (subed-record-remove-directives (elt cue 4)))
									(time (emacstv-format-seconds (floor (/ (elt cue 1) 1000))))
									break)
							(cond
							 ((string-match "^\\[\\(.+?\\)\\]: " text)
								(if (and (string= last-speaker (match-string 1 text))
												 (not without-directives))
										(setq text (replace-match "" nil nil text 0))
									(setq last-speaker (match-string 1 text))
									(setq formatted-last-speaker
												(format "*%s* (%s): "
																(save-match-data
																	(replace-regexp-in-string "^SPEAKER_" "" last-speaker))
																time))
									(setq text (replace-match formatted-last-speaker t t text))
									(setq break t)))
							 ((and without-directives last-speaker)
								(setq text (concat formatted-last-speaker text))))
							(concat
							 (if (and without-directives (not (string= without-directives "")))
									 (format "*** %s"
													 without-directives)
								 "")
							 (if (and (null break) (null without-directives))
									 ""
								 "\n\n"
								 ;; (format "<p></p><span class=\"audio-time caption just-time\" data-start=\"%f\" data-stop=\"%f\">%s</span> "
								 ;; 				(/ (elt cue 1) 1000.0)
								 ;; 				(/ (elt cue 2) 1000.0)
								 ;; 				time)
								 )
							 (if (subed-record-get-directive "#+SCREENSHOT" (elt cue 4))
									 (format "[[%s]]\n\n"
													 (expand-file-name
														(subed-record-get-directive "#+SCREENSHOT" (elt cue 4))
														dir))
								 "")
							 text)))
					cues
					(or separator "\n"))
				 format t info)))
			(_
			 (org-export-string-as
				(mapconcat
				 (lambda (cue)
					 (let ((text (elt cue 3))
								 (without-directives (subed-record-remove-directives (elt cue 4)))
								 (time (emacstv-format-seconds (floor (/ (elt cue 1) 1000))))
								 break)
						 (cond
							((string-match "^\\[\\(.+?\\)\\]: " text)
							 (if (and (string= last-speaker (match-string 1 text))
												(not without-directives))
									 (setq text (replace-match "" nil nil text 0))
								 (setq last-speaker (match-string 1 text))
								 (setq formatted-last-speaker
											 (format "*%s* (%s): "
															 (save-match-data
																 (replace-regexp-in-string "^SPEAKER_" "" last-speaker))
															 time))
								 (setq text (replace-match formatted-last-speaker t t text))
								 (setq break t)))
							((and without-directives last-speaker)
							 (setq text (concat formatted-last-speaker text))))
						 (concat
							(if (and without-directives (not (string= without-directives "")))
									(format "*** %s"
													without-directives)
								"")
							(if (and (null break) (null without-directives))
									""
								"\n\n"
								;; (format "<p></p><span class=\"audio-time caption just-time\" data-start=\"%f\" data-stop=\"%f\">%s</span> "
								;; 				(/ (elt cue 1) 1000.0)
								;; 				(/ (elt cue 2) 1000.0)
								;; 				time)
								)
							(if (subed-record-get-directive "#+SCREENSHOT" (elt cue 4))
									(format "[[%s]]\n\n"
													(expand-file-name
													 (subed-record-get-directive "#+SCREENSHOT" (elt cue 4))
													 dir))
								"")
							text)))
				 cues
				 (or separator "\n"))
				format t info)))))

;;;###autoload
(defun sacha-org-captions-export (link desc format info)
	"Export PATH to FORMAT using the specified wrap parameter."
	(if desc
			(org-export-string-as (org-link-make-string link desc) nil format info)
		(let* ((path-and-query (url-path-and-query (url-generic-parse-url link)))
					 (path (car path-and-query))
					 (params (url-parse-query-string (or (cdr path-and-query) ""))))
			(sacha-org-captions-format
			 path
			 nil
			 format
			 (append
				(and (assoc-string "skip" params)
						 (list :skip (assoc-string "skip" params)))
				(and (assoc-string "url" params)
						 (list :url (cadr (assoc-string "url" params))))
				info)))))


;;;###autoload
(defun sacha-org-captions-complete ()
	"Complete audio reference."
	(interactive)
	(concat "captions:" (read-file-name "Captions: ")))

;;;###autoload
(defun sacha-org-captions-insert-as-html-block (file)
	(interactive "FFile: ")
	(insert "#+begin_export html\n" (sacha-org-captions-format file "\n") "\n#+end_export html\n"))

;;;###autoload
(defun sacha-org-chapters-format (file &optional separator format)
	"Format chapters in FILE."
	(let ((cues (seq-filter (lambda (cue) (elt cue 4))
													(subed-record-remove-directives (subed-parse-file file)))))
		(concat
		 (if (and (elt (car cues) 4)
							(< (elt (car cues) 1) 1000))
				 ""
			 "- vtime:0:00 Intro\n")
		 (mapconcat
			(lambda (cue)
				 (if (member format '(html 11ty))
						 (format
							"- @@html:<span class=\"audio-time\" data-start=\"%.3f\" data-stop=\"%.3f\">%s</span>@@ %s"
							(/ (elt cue 1) 1000.0)
							(/ (elt cue 2) 1000.0)
							(emacstv-format-seconds (/ (elt cue 1) 1000.0))
							(elt cue 4))
					 (format
						"- vtime:%s %s"
						(emacstv-format-seconds (/ (elt cue 1) 1000.0))
						(elt cue 4))))
			cues
			(or separator "\n")))))

;;;###autoload
(defun sacha-org-chapters-export (link desc format info)
	"Export PATH to FORMAT using the specified wrap parameter."
	(if desc
			(org-export-string-as (org-link-make-string link desc) format)
		(org-export-string-as
		 (sacha-org-chapters-format (car (url-path-and-query (url-generic-parse-url link)))
																nil
																format)
		 format info)))

;; Captions:2 ends here

(provide 'sacha-org-captions)
;;; sacha-org-captions.el ends here
