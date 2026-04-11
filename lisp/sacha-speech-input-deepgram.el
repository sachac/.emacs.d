;;; sacha-speech-input-deepgram.el ---  -*- lexical-binding: t -*-

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



;; [[file:../Sacha.org::#using-emacs-lisp-to-send-audio-files-to-deepgram-and-format-vtts][TOBLOG Using Emacs Lisp to send audio files to Deepgram and format VTTs:1]]
(defvar sacha-deepgram-length-threshold 45 "Number of characters.")
(defvar sacha-deepgram-time-threshold 10 "Number of seconds since the first word.")

;;;###autoload
(defun sacha-deepgram-recognize-audio (audio-file &optional diarize)
	"Send AUDIO-FILE to Deepgram, save the JSON, and create a VTT.
If DIARIZE is non-nil, identify speakers."
	(require 'subed)
	(interactive (list (if (auth-info-password (car (auth-source-search :host "https://api.deepgram.com")))
												 (read-file-name "Audio file: ")
											 (error "Please put deepgram API key in auth sources."))))
	(with-current-buffer (get-buffer-create "*Deepgram*")
		(erase-buffer)
		(unless (string-match "\\(opus\\|wav\\|mp3\\)$" audio-file)
			(if (file-exists-p (concat (file-name-sans-extension audio-file) ".opus"))
					(setq audio-file (concat (file-name-sans-extension audio-file) ".opus"))
				(call-process "ffmpeg" nil t t "-i" (expand-file-name audio-file)
											"-ac" "1" "-y"
											(expand-file-name (concat (file-name-sans-extension audio-file) ".opus")))
				(setq audio-file (concat (file-name-sans-extension audio-file) ".opus"))))
		(unless (file-exists-p (expand-file-name (concat (file-name-sans-extension audio-file) ".json")))
			(call-process
			 "curl" nil t t "--request" "POST" "--header"
			 (concat "Authorization: Token " (auth-info-password (car (auth-source-search :host "https://api.deepgram.com"))))
			 "--header" (concat "Content-Type: " (mailcap-file-name-to-mime-type audio-file))
			 "--data-binary" (concat "@" (expand-file-name audio-file))
			 "--url"
			 (concat
				"https://api.deepgram.com/v1/listen?punctuate=true&model=whisper-large&smart_format=true&utterances=true"
				(if diarize
						"&diarize=true"
					""))
			 "-o"
			 (expand-file-name (concat (file-name-sans-extension audio-file) ".json"))))
		(sacha-deepgram-convert-json-to-vtt (concat (file-name-sans-extension audio-file) ".json")))
	(find-file (concat (file-name-sans-extension audio-file) ".vtt")))

;;;###autoload
(defun sacha-emacsconf-extract-deepgram-recognize-qa-for-talk (talk)
	"Send the QA (or main) Opus file for TALK to Deepgram.
Save the results as JSON and VTT."
	(interactive (list (emacsconf-complete-talk-info)))
	(setq talk (emacsconf-resolve-talk talk))
	(if (or (emacsconf-talk-file talk "--answers--original.json")
					(emacsconf-talk-file talk "--original.json"))
			(message "Files already exist for %s" (plist-get talk :slug))
			(if-let ((file
								(or (emacsconf-talk-file talk "--answers--original.opus")
										(emacsconf-talk-file talk "--original.opus"))))
					(sacha-deepgram-recognize-audio file)
				(error "No file to recognize for %s" (plist-get talk :slug)))))

;;;###autoload
(defun sacha-deepgram-parse (json-file)
	"Convert JSON-FILE into a list of subtitles."
	(let* ((json-object-type 'alist)
				 (json (json-read-file json-file))
				 (words
					(assoc-default
					 'words
					 (aref (assoc-default 'alternatives (aref (let-alist json .results.channels) 0)) 0)))
				 (halfway-length (/ sacha-deepgram-length-threshold 2))
				 subtitles
				 current
				 current-length
				 last-speaker
				 last-text
				 current-text)
		(dolist (speaker (seq-group-by (lambda (o) (assoc-default 'speaker o)) words))
			(setq current-length 0 current nil)
			(dolist (word (cdr speaker))
				(let-alist word
					;; determine whether we are adding to the existing one.
					;; start a new one if length > length-threshold
					;; or time > time-threshold
					(when (or (> (+ (length .punctuated_word)
													current-length)
											 sacha-deepgram-length-threshold)
										(and (car current)
												 (> .start (+ (assoc-default 'start (car current))
																			sacha-deepgram-time-threshold))))
						;; copy the previous subtitle
						(push current subtitles)
						(setq current nil current-length 0))
					(push word current)
					(setq current-length (+ (length .punctuated_word) current-length 1))
					(when (and (string-match "[,\\.?]" .punctuated_word)
										 (> current-length halfway-length))
						(push current subtitles)
						(setq current nil current-length 0))))
			(when current (push current subtitles)))
		(seq-keep
		 (lambda (entry)
			 (setq current-text
						 (mapconcat (lambda (w) (assoc-default 'punctuated_word w))
												(reverse entry) " "))
			 (when (not (string= (downcase current-text) (or last-text "")))
				 (setq last-text (downcase current-text))
				 (list nil
							 (* (assoc-default 'start (car (last entry)) nil 0) 1000)
							 (* (assoc-default 'end (car entry) nil 0) 1000)
							 ;; add speaker tag?
							 (concat
								(if (and (assoc-default 'speaker (car entry))
												 (or (null last-speaker)
														 (not (eq last-speaker (assoc-default 'speaker (car entry))))))
										(progn
											(setq last-speaker (assoc-default 'speaker (car entry)))
											(format "[Speaker %d]: " (assoc-default 'speaker (car entry))))
									"")
								current-text
								))))
		 (sort subtitles
					 (lambda (a b)
						 ;; sort by time
						 (< (assoc-default 'start (car a) nil 0)
								(assoc-default 'start (car b) nil 0)))))))

;;;###autoload
(defun sacha-deepgram-convert-json-to-vtt (json-file &optional force)
	(interactive (list (read-file-name "JSON: ") current-prefix-arg))
	"Convert JSON-FILE into a VTT."
	(subed-create-file
	 (concat (file-name-sans-extension json-file) ".vtt")
	 (sacha-deepgram-parse json-file)
	 force))

(defconst deepgram-whisper-large-per-min 0.0048)
;;;###autoload
(defun sacha-deepgram-cost (file)
	(interactive "FFile: ")
	(let* ((whisper-large-per-min deepgram-whisper-large-per-min)
				 (nova2-streaming-per-min 0.0059)
				 (duration (/ (ceiling (/ (compile-media-get-file-duration-ms file) 1000.0)) 60))
				 (msg (format "%.1f minutes: USD %.2f batch, USD %.2f streaming"
											duration
											(* duration whisper-large-per-min)
											(* duration nova2-streaming-per-min))))
		(when (called-interactively-p 'any)
			(message "%s" msg)
			(kill-new msg))
		(list
		 duration
		 (* duration whisper-large-per-min)
		 (* duration nova2-streaming-per-min))))
;; TOBLOG Using Emacs Lisp to send audio files to Deepgram and format VTTs:1 ends here

(provide 'sacha-speech-input-deepgram)
;;; sacha-speech-input-deepgram.el ends here
