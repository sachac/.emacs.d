;;; sacha-vad-tutor.el ---  -*- lexical-binding: t -*-

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
;; - Voice activity detection and speech recognition
;;   https://sachachua.com/dotemacs#writing-and-editing-speech-recognition-using-emacs-to-visualize-speaking-and-target-words-phrases-in-real-ish-time-voice-activity-detection-and-speech-recognition
;;
;;; Code:



;; [[file:../Sacha.org::#writing-and-editing-speech-recognition-using-emacs-to-visualize-speaking-and-target-words-phrases-in-real-ish-time-voice-activity-detection-and-speech-recognition][Voice activity detection and speech recognition:1]]
;; Report on the balance between my tutor and me
(defvar sacha-vad-tutor-process nil)
(defvar sacha-vad-tutor-dir "~/vendor/orukeet")
(defvar sacha-vad-whisper-url "http://localhost:8003/v1/audio/transcriptions")
(defvar sacha-vad-tutor-command `(,(expand-file-name ".venv/bin/python" sacha-vad-tutor-dir)
																      "asr.py"
																			"--url"
																			,sacha-vad-whisper-url
																      "--spacy-model"
																      "fr_dep_news_trf"
																      "-s" "300" "-t"
																      "StereoInputMic.monitor=me"
																			"alsa_output.usb-AT_AT2040USB_202011110001-00.analog-stereo.monitor=other"))
(defvar sacha-vad-tutor-events nil "List of events as an list of alists (label start stop duration).")
(defvar sacha-vad-tutor-labels '("sacha" "other"))
(defvar sacha-vad-tutor-summary '(("sacha" . 0)
																      ("other" . 0)))
(defvar sacha-vad-tutor-session-length (* 60 60) "Number of seconds to use for scale.")

;;;###autoload
(defun sacha-vad-tutor-start (&optional length)
  "Start the process."
  (interactive
	 (list
		(- (time-to-seconds (org-read-date t t nil "End: "))
			 (time-to-seconds))))
	(when (stringp length)
		(setq length (- (time-to-seconds (org-read-date t t length "End: "))
										(time-to-seconds))))
	(setq sacha-vad-tutor-session-length length)
  (sacha-vad-tutor-ensure)
  (setq sacha-vad-tutor-summary '(("me" . 0)
																  ("other" . 0))
			  sacha-vad-tutor-events nil)
  (with-current-buffer (get-buffer-create "*Transcript*")
	  (erase-buffer)
	  (display-buffer (current-buffer)))
  (display-buffer (get-buffer-create "*tutor stats*")))

;;;###autoload
(defun sacha-vad-tutor-update-view ()
  "Visualize the last 3 minutes and the overview of the session.
Also keep track of target phrases."
  (interactive)
  (with-current-buffer (get-buffer-create "*tutor stats*")
	  (erase-buffer)
	  (let* ((total (apply '+ (mapcar 'cdr sacha-vad-tutor-summary)))
				   (height 30)
				   (width (window-width nil t))
				   (max-seconds
					  (if sacha-vad-tutor-events
							  (apply 'max (mapcar (lambda (o) (alist-get 'end o))
																	  sacha-vad-tutor-events))
						  0))
				   (threshold 180)							; last three minutes
				   (threshold-seconds (- max-seconds threshold))
				   (recent
					  (seq-keep
					   (lambda (o)
						   (when (>= (alist-get 'start o)
											   threshold-seconds)
							   (let-alist o
								   `((label . ,.label)
									   (start . ,(- .start threshold-seconds))
									   (end . ,(- .end threshold-seconds))))))
					   (copy-sequence sacha-vad-tutor-events)))
				   (recent-total (apply '+ (mapcar (lambda (o)
																					   (-
																						  (alist-get 'end o)
																						  (alist-get 'start o)))
																				   recent))))
		  (save-excursion
			  (insert
			   (format "Last %d minute%s:\n"
							   (/ threshold 60)
							   (if (> (/ threshold 60) 1)
									   "s"
								   ""))
			   (mapconcat
				  (lambda (o)
					  (format "%d%% %s\n"
									  (/ (* 100.0 (cdr o)) recent-total)
									  (car o)))
				  (seq-map
				   (lambda (group)
					   (cons (car group)
								   (apply '+ (mapcar (lambda (o)
																		   (-
																			  (alist-get 'end o)
																			  (alist-get 'start o)))
																	   (cdr group)))))
				   (seq-group-by (lambda (o) (alist-get 'label o)) recent)))
			   "\n")
			  (svg-insert-image
			   (sacha-vad-visualize
				  sacha-vad-tutor-labels
				  recent
				  t
				  width height))
			  (insert
			   "\nTotal:\n"
			   (mapconcat
				  (lambda (o)
					  (format "%d%% %s\n"
									  (/ (* 100.0 (cdr o)) total)
									  (car o)))
				  sacha-vad-tutor-summary))
			  (svg-insert-image
			   (sacha-vad-visualize sacha-vad-tutor-labels sacha-vad-tutor-events t
														  width height
														  sacha-vad-tutor-session-length))
			  (insert "\n\n"  (sacha-learn-lang-bingo-summary))))))

(defun sacha-vad-visualize-spans (data)
	(when (alist-get 'end data)
		(let ((event (list
									(cons 'label (alist-get 'label data))
									(cons 'start (- (alist-get 'timestamp data)
																	(alist-get 'duration data)
																	(or sacha-vad-tutor-start-seconds 0)))
									(cons 'end (- (alist-get 'timestamp data)
																(or sacha-vad-tutor-start-seconds 0))))))
			(push event sacha-vad-tutor-events)
			(let-alist data
				(incf (alist-get (alist-get 'label data) sacha-vad-tutor-summary 0 nil 'string=)
							.duration))
			(sacha-vad-tutor-update-view))))

(defvar sacha-vad-event-handler-functions
	'(sacha-vad-speech-process
		sacha-vad-add-to-transcript-buffer
		sacha-vad-add-to-transcript-file
		sacha-vad-update-last-speaker
		sacha-vad-notice-bingo
		sacha-vad-visualize-spans)
	"Functions to call with the VAD/transcription events.")

(defvar sacha-vad-process-my-speech nil "Non-nil means process my speech through the whisper setup.")
(defun sacha-vad-speech-process (data)
  "Process DATA through my whisper input process if it's my speech.
Add to `sacha-vad-event-handler-functions'."
  (when (and sacha-vad-process-my-speech
					   (string= (alist-get 'event data) "final")
					   (string= (alist-get 'label data) sacha-vad-my-label))
	  (with-current-buffer (get-buffer-create whisper--stdout-buffer-name)
		  (erase-buffer)
		  (insert (alist-get 'text data))
		  (goto-char (point-min)))
	  (whisper--handle-transcription-output nil (get-buffer-create whisper--stdout-buffer-name))))

(defun sacha-vad-tutor-filter (proc string)
	"Add and parse."
	(when (buffer-live-p (process-buffer proc))
		(let ((data (with-current-buffer (process-buffer proc)
									(goto-char (point-max))
									(save-excursion
										(insert string))
									(catch 'waiting
										(unless (or (eobp) (looking-at "\n*$"))
											(json-read))))))
			(when (alist-get 'text data)
				(setf (alist-get 'text data)
							(sacha-subed-fix-common-errors-in-text (alist-get 'text data))))
			(run-hook-with-args 'sacha-vad-event-handler-functions data))))

(defvar sacha-vad-last-speaker nil "Previous speaker.")

(defvar sacha-vad-transcript-buffer nil "If non-nil, add speech to this buffer.")

(defvar sacha-vad-my-label "me")

(defun sacha-vad-update-last-speaker (data)
	"Save the last speaker."
	(when (string= (alist-get 'event data) "final")
		(setq sacha-vad-last-speaker (alist-get 'label data))) )

(defvar sacha-vad-adjust 1500)

(defun sacha-vad-transcript-file ()
	"Return the file name of the possible transcript."
	(concat (file-name-sans-extension
								 (sacha-latest-file sacha-recordings-dir "\\.mkv$"))
					".vtt"))

(with-eval-after-load 'phony
	(phony-defun prepare-to-fast-forward ("prepare" "to" "fast" "forward")
		(find-file (sacha-vad-transcript-file))
		(subed-mpv-play-from-file (subed-media-file))))

(defun sacha-vad-add-to-transcript-file (data)
	"Add DATA to the VTT beside the current recording."
	(when (and (alist-get 'text data)
						 (alist-get 'label data)
						 (alist-get 'start_ts data)
						 (alist-get 'end_ts data)
						 (and (featurep 'obs-websocket)
									(obs-websocket-recording-p)))
		;; Find the transcript for the most recent recording.
		(let* ((rec (sacha-latest-file sacha-recordings-dir "\\.mkv$"))
					 (filename (concat (file-name-sans-extension rec) ".vtt"))
					 (file-start (* 1000 (sacha-filename-timestamp (file-name-base filename))))
					 (data-start (alist-get 'start_ts data))
					 (data-stop (alist-get 'end_ts data))
					 (text (alist-get 'text data)))
			;(unless (subed-media-file)
;				(subed-mpv-play-from-file rec))
			(with-current-buffer (find-file-noselect filename)
				(save-restriction
					(save-excursion
						(widen)
						(when (= (point-max) 1)
							(subed-auto-insert))
						(goto-char (point-max))
						(subed-append-subtitle
						 nil
						 (- data-start file-start sacha-vad-adjust)
						 (- data-stop file-start sacha-vad-adjust)
						 (if (string= sacha-vad-last-speaker (alist-get 'label data))
								 text
							 (format "[%s]: %s" (alist-get 'label data)
											 text)))))))))

(defun sacha-vad-add-to-transcript-buffer (data)
	"Add DATA to *Transcript*."
	(when (and (alist-get 'text data)
						 (alist-get 'label data))
		(when sacha-vad-transcript-buffer
			(with-current-buffer (get-buffer-create sacha-vad-transcript-buffer)
				(set (make-local-variable 'window-point-insertion-type) t)
				(save-excursion
					(goto-char (point-max))
					(unless (bolp) (insert "\n\n"))
					(insert
					 (concat
						(if (string= sacha-vad-last-speaker (alist-get 'label data))
								" "
							(concat (if (bobp) "" "\n\n") (alist-get 'label data) ": "))
						(alist-get 'text data))))))))

(defun sacha-vad-notice-bingo (data)
	(when (and (alist-get 'text data) (string= (alist-get 'label data) sacha-vad-my-label))
	  (sacha-learn-lang-notice-bingo-words (alist-get 'text data))))

(defun sacha-vad-tutor-ensure ()
	"Start the process if it's not already running."
	(interactive)
	(unless (process-live-p sacha-vad-tutor-process)
	  (setq sacha-vad-tutor-process-start-time (current-time))
	  (setq sacha-vad-tutor-start-seconds 0)
		(let ((default-directory sacha-vad-tutor-dir))
			(setq sacha-vad-tutor-process
						(make-process
						 :name "vad-tutor"
						 :command sacha-vad-tutor-command
						 :connection-type 'pipe
						 :buffer (get-buffer-create "*vad-tutor*")
						 :stderr (get-buffer-create "*vad-tutor-err*")
						 :filter #'sacha-vad-tutor-filter)))))

(defun sacha-vad-tutor-stop ()
      "Stop the process."
      (interactive)
      (interrupt-process sacha-vad-tutor-process))

;;;###autoload
(defun sacha-js-parse-jsonl-file (file)
"Parse jsonl as a list"
      (let (results)
	      (with-temp-buffer
		      (insert-file-contents file)
		      (goto-char (point-min))
		      (while (not (eobp))
			      (condition-case nil
					      (push (json-read) results)
				      (error (goto-char (point-max))))))
	      (nreverse results)))

(defvar sacha-vad-tutor-start-seconds 0 "Number of seconds to consider as the start.")
(defvar sacha-vad-tutor-process-start-time nil "Time that the process started.")

;;;###autoload
(defun sacha-vad-tutor-reset-clock ()
"Reset the start of the session."
(interactive)
      (setq sacha-vad-tutor-start-seconds
			      (time-to-seconds (time-subtract (current-time)
																			      sacha-vad-tutor-process-start-time)))
      (mapcar (lambda (o) (setcdr o 0)) sacha-vad-tutor-summary)
      (setq sacha-vad-tutor-events nil))

(defun sacha-vad-visualize (labels list &optional output-file width height max-seconds)
      (setq height (or height 25))
      (setq width (or width (window-width nil t)))
      (let* ((svg (svg-create width height))
			       (max-seconds (max
										       (or max-seconds 0)
										       (if list
												       (apply 'max (mapcar (lambda (o) (alist-get 'end o))
																						       list))
											       0)))
			       (pixels-per-second (/ (* 1.0 width) max-seconds))
			       (row-height (/ height (length labels))))
	      (dolist (o list)
		      (let-alist o
			      (let ((y (* row-height (or (seq-position labels .label 'string=) -1))))
				      (when (>= y 0)
					      (svg-rectangle
					       svg
					       (* .start pixels-per-second)
					       y
					       (* (- .end .start) pixels-per-second)
					       row-height :fill "green")))))
	      (cond
	       ((stringp output-file)
		      (with-temp-file output-file
			      (svg-print svg)))
	       (output-file
		      svg)
	       (t
		      (with-current-buffer (get-buffer-create "*vad*")
			      (goto-char (point-min))
			      (display-buffer (current-buffer))
			      (erase-buffer)
			      (svg-insert-image svg))))))

(defun sacha-vad-stats (labels list)
      "Return the balance of time talking as (label total percentage)."
      (let* ((max-seconds (apply '+ (mapcar
															       (lambda (o) (let-alist o (- .end .start)))
															       list)))
			       (durations (mapcar (lambda (o)
													      (cons
													       (car o)
													       (mapcar (lambda (e) (let-alist e (- .end .start)))
																	       (cdr o))))
												      (seq-group-by (lambda (o) (alist-get 'label o))
																			      list))))
	      (seq-map
	       (lambda (label)
		       (let* ((row (cdr (assoc label durations)))
						      (total (apply '+ row)))
			       `((label . ,label)
				       (total . ,total)
				       (count . ,(length row))
				       (percentage . ,(and total max-seconds (> max-seconds 0) (* 100.0 (/ total max-seconds)))))))
	       labels)))

;; (sacha-vad-stats '("sacha" "tutor") (seq-take (sacha-js-parse-jsonl-file "~/proj/speech/output2.log") 5))
;; (sacha-vad-stats '("sacha" "tutor") (sacha-js-parse-jsonl-file "~/proj/speech/output.log"))
;; (sacha-vad-visualize '("sacha" "tutor") (sacha-js-parse-jsonl-file "~/proj/speech/output.log"))
;; (sacha-vad-visualize '("sacha" "tutor") (seq-take (sacha-js-parse-jsonl-file "~/proj/speech/output2.log") 30))

(defun sacha-vad-tutor-group-output (files)
      (let ((grouped-files (seq-group-by
											      (lambda (o)
												      (substring (file-name-base o) 0 10))
											      files)))
	      (mapcar
	       (lambda (group)
		       (let ((start 0))
			       (cons (car (cdr group))
						       (seq-reduce
							      (lambda (prev file)
								      (append prev
												      (let ((max-seconds 0))
													      (prog1 (mapcar (lambda (event)
																					       (incf (alist-get 'start event) start)
																					       (incf (alist-get 'end event) start)
																					       (setq max-seconds (max (alist-get 'end event) max-seconds))
																					       event)
																				       (sacha-js-parse-jsonl-file file))
														      (setq start max-seconds)))))
							      (cdr group)
							      nil))))
	       grouped-files)))

(defun sacha-vad-tutor-make-table (files &optional width height)
      "Make a summary table."
      (let ((labels sacha-vad-tutor-labels)
			      (width (or width 500))
			      (height (or height 30)))
	      ;; combine data as needed
	      (append
	       '(("Date" "Tutor" "Minutes" "%" "Visualization") hline)
	       (seq-map
		      (lambda (group)
			      (let* ((list (cdr group))
						       (base (file-name-base (car group))))
				      (if list
						      (let-alist (car (sacha-vad-stats labels list))
							      (let ((svg-name
										       (expand-file-name
											      (concat
											       (format-time-string
												      "%Y-%m-%d-%H-%M-%S"
												      (seconds-to-time
												       (sacha-filename-timestamp (car group))))
											       "-" .label ".svg")
											      "~/proj/french/conversation")))
								      (sacha-vad-visualize
								       (list .label)
								       list
								       svg-name
								       width
								       height)
								      (list
								       (format-time-string "%Y-%m-%d"
																		       (seconds-to-time
																			      (sacha-filename-timestamp (car group))))
								       ;; (org-link-make-string (concat "file:" o) "file")
								       (if (string-match "-conversation-\\([^\\.]+\\)" base)
										       (s-capitalize (substring (match-string 1 base) 0 1))
									       "")

								       (round (/ .total 60))
								       (format "%d%%" .percentage)
								       (org-link-make-string (concat "file:" svg-name)
																			       (concat "file:" svg-name))
								       )))
					      (list "?"))))
		      (sacha-vad-tutor-group-output files))
	       nil)))
;; Voice activity detection and speech recognition:1 ends here

(provide 'sacha-vad-tutor)
;;; sacha-vad-tutor.el ends here
