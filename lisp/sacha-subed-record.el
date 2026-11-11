;;; sacha-subed-record.el ---  -*- lexical-binding: t -*-

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
;; - Add shadowing with tts to subed-record
;;   https://sachachua.com/dotemacs#writing-and-editing-learning-french-add-shadowing-with-tts-to-subed-record
;;
;; - Make it easy to add reference links
;;   https://sachachua.com/dotemacs#writing-and-editing-learning-french-make-it-easy-to-add-reference-links
;;
;; - Insert a clip of the corresponding video
;;   https://sachachua.com/dotemacs#multimedia-subtitles-with-subed-subed-record-insert-a-clip-of-the-corresponding-video
;;
;; - Transcribe a VTT of the latest recording for easier editing
;;   https://sachachua.com/dotemacs#multimedia-subtitles-with-subed-subed-record-transcribe-a-vtt-of-the-latest-recording-for-easier-editing
;;
;;; Code:



;; [[file:../Sacha.org::#writing-and-editing-learning-french-add-shadowing-with-tts-to-subed-record][Add shadowing with tts to subed-record:2]]
;;;###autoload
  (defun sacha-subed-record-normalize-current (file)
    (interactive (list (when (derived-mode-p 'subed-mode) (subed-media-file))))
		(when (derived-mode-p 'subed-mode)
			(let ((temp-file (make-temp-file file nil (concat "." (file-name-extension file)))))
				(make-process
				 :name "normalize"
				 :buffer (get-buffer-create "*normalize*")
				 :command (list
									 (expand-file-name "~/bin/normalize")
									 (expand-file-name file)
									 temp-file)
				 :sentinel
				 (lambda (process event)
					 (when (string-match "finished" event)
						 (rename-file temp-file file t)
						 (message "Normalized %s" file)))))))
;; Add shadowing with tts to subed-record:2 ends here

;; [[file:../Sacha.org::#writing-and-editing-learning-french-make-it-easy-to-add-reference-links][Make it easy to add reference links:1]]
(defvar-local sacha-subed-record-references nil)
;;;###autoload
(defun sacha-subed-record-load-references (file &optional skip-insert)
  "Load the references from FILE (media)."
  (interactive (list (read-file-name "Media file: ")
		     current-prefix-arg))
  (dolist (cue (subed-parse-file (concat (file-name-sans-extension file) ".vtt")))
    (push
     (list
      (learn-lang-subed-record-simplify (elt cue 3))
      file
      (elt cue 1)
      (elt cue 2))
     sacha-subed-record-references))
  (unless skip-insert
    (sacha-subed-insert-references)))

;;;###autoload
(defun sacha-subed-record-insert-reference ()
  (interactive)
  (when-let* ((rec (alist-get (learn-lang-subed-record-simplify (subed-subtitle-text))
			     sacha-subed-record-references
			     nil nil #'string=)))
    (subed-record-set-directive
     "#+REFERENCE"
     (format "%s %s --> %s"
	     (elt rec 0)
	     (subed-msecs-to-timestamp (elt rec 1))
	     (subed-msecs-to-timestamp (elt rec 2))))))

;;;###autoload
(defun sacha-subed-insert-references ()
  (interactive)
  (subed-for-each-subtitle (point-min) (point-max) t
    (sacha-subed-record-insert-reference)))
;; Make it easy to add reference links:1 ends here

;; [[file:../Sacha.org::#multimedia-subtitles-with-subed-subed-record-insert-a-clip-of-the-corresponding-video][Insert a clip of the corresponding video:1]]
(defun sacha-subed-record-find-video-for-clip ()
	"Return the video that probably has CLIP in it.
Return (clip-filename start-ms end-ms)."
	(let* ((audio (subed-record-media-file))
				 (timestamp
					(when (string-match "[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]-[0-9][0-9][0-9][0-9][0-9][0-9]" audio)
						(+ (sacha-filename-timestamp (match-string 0 audio))
							 (/ (subed-subtitle-msecs-start) 1000.0))))
				 (video-file (seq-find
											(lambda (f)
												;; starts before timestamp
												(and (not (file-directory-p f))
														 (< (sacha-filename-timestamp f) timestamp)))
											(nreverse (directory-files sacha-recordings-dir t "\\.mkv\\'"))))
				 (video-time (sacha-filename-timestamp video-file))
				 ;; it would be nice to have milliseconds, we might have to sync this up manually or swap it for the video's audio
				 (start-ms (round (* (- timestamp video-time) 1000.0)))
				 (end-ms (round (+ start-ms
													 (- (subed-subtitle-msecs-stop) (subed-subtitle-msecs-start))))))
		(when video-file
			(list video-file start-ms end-ms))))

;;;###autoload
(defun sacha-subed-record-add-video-for-clip ()
  "Find the video that corresponds to the current subtitle."
  (interactive)
	(let ((video-file (sacha-subed-record-find-video-for-clip)))
		(unless (string-match (regexp-quote (car video-file)) (subed-subtitle-comment))
			(subed-set-subtitle-comment
			 (concat (subed-subtitle-comment) "\n"
							 (org-link-make-string (car video-file)))))
		(subed-record-set-directive "#+AUDIO" (car video-file))
		(subed-set-subtitle-time-start (elt video-file 1))
		(subed-set-subtitle-time-stop (elt video-file 2))))

(defun sacha-subed-record-based-on-obs ()
	"Calculate the offset based on the current OBS recording."
	(interactive)
	(let* ((video-file (sacha-latest-file sacha-recordings-dir "\\.mkv\\'")))
		(setq subed-record-backend 'obs-current-recording)
		(subed-record-start-recording video-file (seconds-to-time (sacha-filename-timestamp video-file)))))
;; Insert a clip of the corresponding video:1 ends here

;; [[file:../Sacha.org::#multimedia-subtitles-with-subed-subed-record-transcribe-a-vtt-of-the-latest-recording-for-easier-editing][Transcribe a VTT of the latest recording for easier editing:1]]
(defun sacha-subed-record-prepare-recording (video-file &optional data)
	"Create a VTT file for the latest recording."
	(interactive (list (sacha-latest-file sacha-recordings-dir "\\.mkv\\'")))
	(let* ((results (or data (speech-input-transcribe-file-to-json video-file)))
				 (output-file (concat (file-name-sans-extension video-file) ".vtt"))
				 (subtitles (mapcar (lambda (seg)
															(let-alist seg
																(list nil
																			(* .start 1000)
																			(* .end 1000)
																			.text
																			nil)))
														(alist-get 'segments results))))
		(setf (elt (car subtitles) 4)
					(format "[[%s]]\n#+OUTPUT: %s-edited.%s\n#+OPTIONS: same-edits"
									video-file
									(file-name-sans-extension video-file)
									(file-name-extension video-file)))
		(subed-create-file
		 output-file
		 subtitles
		 t)
		(when (called-interactively-p 'any)
			(find-file output-file))
		output-file))
;; Transcribe a VTT of the latest recording for easier editing:1 ends here

(provide 'sacha-subed-record)
;;; sacha-subed-record.el ends here
