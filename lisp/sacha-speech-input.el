;;; sacha-speech-input.el ---  -*- lexical-binding: t -*-

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
;; - Using whisper.el to convert speech to text and save it to the currently clocked task in Org Mode or elsewhere
;;   https://sachachua.com/dotemacs#multimedia-whisper
;;
;; - Emacs and whisper.el: Trying out different speech-to-text backends and models
;;   https://sachachua.com/dotemacs#writing-and-editing-speech-recognition-emacs-and-whisper-el-trying-out-different-speech-to-text-backends-and-models
;;
;; - Using Silero voice activity detection to automatically queue multiple transcriptions with natrys/whisper.el
;;   https://sachachua.com/dotemacs#writing-and-editing-speech-recognition-using-silero-voice-activity-detection-to-automatically-queue-multiple-transcriptions-with-natrys-whisper-el
;;
;; - Slowly building speech-based commands for Emacs
;;   https://sachachua.com/dotemacs#writing-and-editing-speech-recognition-slowly-building-speech-based-commands-for-emacs
;;
;; - Okay, track...
;;   https://sachachua.com/dotemacs#writing-and-editing-speech-recognition-okay-track
;;
;; - Keep track of files and stats
;;   https://sachachua.com/dotemacs#writing-and-editing-speech-recognition-keep-track-of-files-and-stats
;;
;; - Using speech recognition for on-the-fly translations in Emacs and faking in-buffer completion for the results
;;   https://sachachua.com/dotemacs#writing-and-editing-speech-recognition-using-speech-recognition-for-translations-in-emacs-and-faking-in-buffer-completion-for-the-results
;;
;; - Streaming speech recognition into Emacs using Google Chrome Web Speech API
;;   https://sachachua.com/dotemacs#writing-and-editing-speech-recognition-streaming-speech-recognition-into-emacs-using-google-chrome-web-speech-api
;;
;; - speech and subed-record
;;   https://sachachua.com/dotemacs#writing-and-editing-speech-recognition-streaming-speech-recognition-into-emacs-using-google-chrome-web-speech-api-speech-and-subed-record
;;
;;; Code:



;; [[file:../Sacha.org::#multimedia-whisper][Using whisper.el to convert speech to text and save it to the currently clocked task in Org Mode or elsewhere:2]]
(defvar sacha-whisper-dir "~/recordings/whisper/")
;;;###autoload
(defun sacha-whisper-set-temp-filename ()
  (setq whisper--temp-file (expand-file-name
                            (format-time-string "%Y-%m-%d-%H-%M-%S.wav")
                            sacha-whisper-dir)))

;; Using whisper.el to convert speech to text and save it to the currently clocked task in Org Mode or elsewhere:2 ends here

;; [[file:../Sacha.org::#multimedia-whisper][Using whisper.el to convert speech to text and save it to the currently clocked task in Org Mode or elsewhere:4]]
;;;###autoload
(defun sacha-whisper-replay (&optional file)
  "Replay the last temporary recording."
  (interactive (list
                (when current-prefix-arg
                  (read-file-name "File: " sacha-whisper-dir))))
  (setq whisper--temp-file (or file whisper--temp-file))
  (mpv-play whisper--temp-file))

;;;###autoload
(defun sacha-whisper-insert-retry (&optional file)
  (interactive (list
                (when current-prefix-arg
                  (read-file-name "File: " sacha-whisper-dir))))
  (whisper--cleanup-transcription)
  (setq whisper--marker (point-marker)
        whisper--temp-file (or file whisper--temp-file))
  (whisper--transcribe-audio))
;; Using whisper.el to convert speech to text and save it to the currently clocked task in Org Mode or elsewhere:4 ends here

;; [[file:../Sacha.org::#multimedia-whisper][Using whisper.el to convert speech to text and save it to the currently clocked task in Org Mode or elsewhere:5]]
;;;###autoload
(defun sacha-whisper-reset (text)
  (setq sacha-whisper-skip-annotation nil)
  (remove-hook 'whisper-insert-text-at-point #'sacha-whisper-org-save-to-clocked-task)
  text)
;; Using whisper.el to convert speech to text and save it to the currently clocked task in Org Mode or elsewhere:5 ends here

;; [[file:../Sacha.org::#multimedia-whisper][Using whisper.el to convert speech to text and save it to the currently clocked task in Org Mode or elsewhere:7]]
(defvar sacha-whisper-last-annotation nil "Last annotation so we can skip duplicates.")
(defvar sacha-whisper-skip-annotation nil)
(defvar sacha-whisper-target-markers nil "List of markers to send text to.")

;;;###autoload
(defun sacha-whisper-insert (text)
  (let ((markers
         (cond
          ((null sacha-whisper-target-markers)
           (list whisper--marker)) ; current point where whisper was started
          ((listp sacha-whisper-target-markers)
           sacha-whisper-target-markers)
          ((markerp sacha-whisper-target-markers)
           (list sacha-whisper-target-markers))))
        (orig-point (point))
        (orig-buffer (current-buffer)))
    (when text
      (mapcar (lambda (marker)
                (with-current-buffer (marker-buffer marker)
                  (save-restriction
                    (widen)
                    (when (markerp marker) (goto-char marker))
                    (when (and (derived-mode-p 'org-mode) (org-at-drawer-p))
                      (insert "\n"))
                    (whisper--insert-text
                     (concat
                      (if (looking-back "[ \t\n]\\|^")
                          ""
                        " ")
                      (string-trim text)))
                    ;; Move the marker forward here
                    (move-marker marker (point)))))
              markers)
      (when sacha-whisper-target-markers
        (goto-char orig-point))
      nil)))
;; Using whisper.el to convert speech to text and save it to the currently clocked task in Org Mode or elsewhere:7 ends here

;; [[file:../Sacha.org::sacha-whisper-maybe-type][sacha-whisper-maybe-type]]
;;;###autoload
(defun sacha-whisper-maybe-type (text)
  "If Emacs is not the focused app, simulate typing TEXT.
Add this function to `whisper-insert-text-at-point'."
  (when text
    (if (frame-focus-state)
        text
      (make-process :name "xdotool" :command
                    (list "xdotool" "type"
                          text))
      nil)))
;; sacha-whisper-maybe-type ends here

;; [[file:../Sacha.org::#multimedia-whisper][Using whisper.el to convert speech to text and save it to the currently clocked task in Org Mode or elsewhere:9]]
;;;###autoload
(defun sacha-whisper-clear-markers ()
  (interactive)
  (setq sacha-whisper-target-markers nil))

;;;###autoload
(defun sacha-whisper-use-current-point (&optional add)
  (interactive (list current-prefix-arg))
  (if add
      (push (point-marker) sacha-whisper-target-markers)
    (setq sacha-whisper-target-markers (list (point-marker)))))

;;;###autoload
(defun sacha-whisper-run-at-point (&optional add)
  (interactive (list current-prefix-arg))
  (sacha-whisper-clear-markers)
  (whisper-run))

;; Using whisper.el to convert speech to text and save it to the currently clocked task in Org Mode or elsewhere:9 ends here

;; [[file:../Sacha.org::#multimedia-whisper][Using whisper.el to convert speech to text and save it to the currently clocked task in Org Mode or elsewhere:11]]
;;;###autoload
(defun sacha-whisper-jump-to-marker ()
  (interactive)
  (with-current-buffer (marker-buffer (car sacha-whisper-target-markers))
    (goto-char (car sacha-whisper-target-markers))))

;;;###autoload
(defun sacha-whisper-use-currently-clocked-task (&optional add)
  (interactive (list current-prefix-arg))
  (save-window-excursion
    (save-restriction
      (save-excursion
        (org-clock-goto)
        (org-end-of-meta-data)
        (org-end-of-subtree)
        (if add
            (push (point-marker) sacha-whisper-target-markers)
          (setq sacha-whisper-target-markers (list (point-marker))))))))

;;;###autoload
(defun sacha-whisper-run (&optional skip-annotation)
  (interactive (list current-prefix-arg))
  (require 'whisper)
  (add-hook 'whisper-insert-text-at-point #'sacha-whisper-org-save-to-clocked-task -10)
  (whisper-run)
  (when skip-annotation
    (setq sacha-whisper-skip-annotation t)))

;;;###autoload
(defun sacha-whisper-save-text (text)
  "Save TEXT beside `whisper--temp-file'."
  (when text
    (let ((link (org-store-link nil)))
      (with-temp-file (concat (file-name-sans-extension whisper--temp-file) ".txt")
        (when link
          (insert link "\n"))
        (insert text)))
    text))

;;;###autoload
(defun sacha-whisper-org-save-to-clocked-task (text)
  (when text
    (save-window-excursion
      (with-current-buffer (if (markerp whisper--marker) (marker-buffer whisper--marker) (current-buffer))
        (when (markerp whisper--marker) (goto-char whisper--marker))
        ;; Take a screenshot maybe
        (let* ((link (and (not sacha-whisper-skip-annotation)
                          (org-store-link nil)))
               (region (and (region-active-p) (buffer-substring (region-beginning) (region-end))))
               (screenshot-filename
                (when (or
                       (null link)
                       (not (string= sacha-whisper-last-annotation link))
                       (not (frame-focus-state))) ; not in focus, take a screenshot
                  (sacha-screenshot-current-screen (concat (file-name-sans-extension whisper--temp-file) ".png")))))
          (if (org-clocking-p)
              (save-window-excursion
                (save-restriction
                  (save-excursion
                    (org-clock-goto)
                    (org-end-of-subtree)
                    (unless (bolp)
                      (insert "\n"))
                    (insert "\n")
                    (if (and link (not (string= sacha-whisper-last-annotation link)))
                        (insert
                         (if screenshot-filename
                             (concat "(" (org-link-make-string
                                          (concat "file:" screenshot-filename)
                                          "screenshot") ") ")
                           "")
                         link
                         "\n")
                      (when screenshot-filename
                        (insert (org-link-make-string
                                 (concat "file:" screenshot-filename)
                                 "screenshot")
                                "\n")))
                    (when region
                      (insert "#+begin_example\n" region "\n#+end_example\n"))
                    (insert text "\n")
                    (setq sacha-whisper-last-annotation link)))
                (run-at-time 0.5 nil (lambda (text) (message "Added clock note: %s" text)) text))
            ;; No clocked task, prompt for a place to capture it
            (kill-new text)
            (setq org-capture-initial text)
            (call-interactively 'org-capture)
            ;; Delay the window configuration
            (let ((config (current-window-configuration)))
              (run-at-time 0.5 nil
                           (lambda (text config)
                             (set-window-configuration config)
                             (message "Copied: %s" text))
                           text config))))))))

;; Using whisper.el to convert speech to text and save it to the currently clocked task in Org Mode or elsewhere:11 ends here

;; [[file:../Sacha.org::#multimedia-whisper][Using whisper.el to convert speech to text and save it to the currently clocked task in Org Mode or elsewhere:13]]
;;;###autoload
(defun sacha-whisper-org-clear-saved-annotation ()
  (setq sacha-whisper-org-last-annotation nil))
;; Using whisper.el to convert speech to text and save it to the currently clocked task in Org Mode or elsewhere:13 ends here

;; [[file:../Sacha.org::#multimedia-whisper][Using whisper.el to convert speech to text and save it to the currently clocked task in Org Mode or elsewhere:14]]
(defvar sacha-whisper-notes "~/sync/stream/narration.org")
;;;###autoload
(defun sacha-whisper-save-to-file (text)
  (when text
    (let ((link (org-store-link nil)))
      (with-current-buffer (find-file-noselect sacha-whisper-notes)
        (goto-char (point-max))
        (insert "\n\n" (format-time-string "%H:%M ") text "\n" (if link (concat link "\n") ""))
        (save-buffer)
        (run-at-time 0.5 nil (lambda (text) (message "Saved to file: %s" text)) text)))
    text))
;; Using whisper.el to convert speech to text and save it to the currently clocked task in Org Mode or elsewhere:14 ends here

;; [[file:../Sacha.org::#multimedia-whisper][Using whisper.el to convert speech to text and save it to the currently clocked task in Org Mode or elsewhere:15]]
;;;###autoload
(defun sacha-save-to-kill-ring-after-current (text)
	"Save TEXT to the kill ring, but not at the top spot."
	(when text
		(if kill-ring
				(let ((temp (pop kill-ring)))
					(kill-new text)
					(push temp kill-ring))
			(kill-new text)))
	text)
;; Using whisper.el to convert speech to text and save it to the currently clocked task in Org Mode or elsewhere:15 ends here

;; [[file:../Sacha.org::#multimedia-whisper][Using whisper.el to convert speech to text and save it to the currently clocked task in Org Mode or elsewhere:16]]
;;;###autoload
(defun sacha-whisper-redo ()
  (interactive)
  (setq whisper--marker (point-marker))
  (whisper--transcribe-audio))
;; Using whisper.el to convert speech to text and save it to the currently clocked task in Org Mode or elsewhere:16 ends here

;; [[file:../Sacha.org::#writing-and-editing-speech-recognition-emacs-and-whisper-el-trying-out-different-speech-to-text-backends-and-models][Emacs and whisper.el: Trying out different speech-to-text backends and models:1]]
(defvar sacha-whisper-url-format "http://%s:%d/transcribe")
;;;###autoload
(defun sacha-whisper--transcribe-via-local-server ()
  "Transcribe audio using the local whisper server."
  (message "[-] Transcribing via local server")
  (whisper--setup-mode-line :show 'transcribing)
  (whisper--ensure-server)
  (setq whisper--transcribing-process
        (whisper--process-curl-request
         (format sacha-whisper-url-format whisper-server-host whisper-server-port)
         (list "Content-Type: multipart/form-data")
         (list (concat "file=@" whisper--temp-file)
               "temperature=0.0"
               "temperature_inc=0.2"
               "response_format=json"
               (concat "model=" whisper-model)
               (concat "language=" whisper-language)))))
;;;###autoload
(defun sacha-whisper--check-model-consistency () t)
;; Emacs and whisper.el: Trying out different speech-to-text backends and models:1 ends here

;; [[file:../Sacha.org::#writing-and-editing-speech-recognition-emacs-and-whisper-el-trying-out-different-speech-to-text-backends-and-models][Emacs and whisper.el: Trying out different speech-to-text backends and models:6]]
(defvar sacha-speech-input-model-aliases
  '(("small" . "Systran/faster-whisper-small.en")
    ("medium" . "Systran/faster-whisper-medium.en")
    ("base" . "Systran/faster-whisper-base.en")
    ("tiny" . "Systran/faster-whisper-tiny.en")
    ("large" . "Systran/faster-whisper-large-v2")))

(defun sacha-speech-input-set-model (model-name)
  "Change the speech recognition model to MODEL-NAME.
Use `sacha-speech-input-model-aliases' for aliases."
  (interactive (list (speech-input-speaches-read-model-name)))
  (when (assoc-default model-name sacha-speech-input-model-aliases #'string=)
    (setq model-name (assoc-default model-name sacha-speech-input-model-aliases #'string=)))
  (setq whisper-model model-name)
  (setq speech-input-model model-name
				speech-input-transcribe-model model-name))
;; Emacs and whisper.el: Trying out different speech-to-text backends and models:6 ends here

;; [[file:../Sacha.org::#writing-and-editing-speech-recognition-using-silero-voice-activity-detection-to-automatically-queue-multiple-transcriptions-with-natrys-whisper-el][Using Silero voice activity detection to automatically queue multiple transcriptions with natrys/whisper.el:3]]
;;;###autoload
(defun sacha-whisper-maybe-continue ()
  (when (process-live-p whisper--recording-process)
    (sacha-whisper-continue)))
;; Using Silero voice activity detection to automatically queue multiple transcriptions with natrys/whisper.el:3 ends here

;; [[file:../Sacha.org::#writing-and-editing-speech-recognition-slowly-building-speech-based-commands-for-emacs][Slowly building speech-based commands for Emacs:1]]
(defvar sacha-number-words
	'(("zero" . 0) ("one" . 1) ("two" . 2) ("three" . 3)
    ("four" . 4) ("five" . 5) ("six" . 6) ("seven" . 7)
    ("eight" . 8) ("nine" . 9) ("ten" . 10)
    ("eleven" . 11) ("twelve" . 12) ("thirteen" . 13)
    ("fourteen" . 14) ("fifteen" . 15) ("sixteen" . 16)
    ("seventeen" . 17) ("eighteen" . 18) ("nineteen" . 19)
    ("twenty" . 20) ("thirty" . 30) ("forty" . 40)
    ("fifty" . 50) ("sixty" . 60) ("seventy" . 70)
    ("eighty" . 80) ("ninety" . 90)
		("hundred" . 100)
		("thousand" . 1000)
		("quatre[- ]vingt" . 80)  					; we need to recognize this before quatre
		("dix" . 10)
		("une" . 1)
		("un" . 1)
		("deux" . 2)
		("trois" . 3)
		("quatre" . 4)
		("cinq" . 5)
		("sept" . 7)
		("huit" . 8)
		("neuf" . 9)
		("onze" . 11)
		("douze" . 12)
		("treize" . 13)
		("quatorze" . 14)
		("quinze" . 15)
		("seize" . 16)
		("vingt et un" . 21)
		("treinte" . 30)
		("treinte et un" . 31)
		("quarante" . 40)
		("quarante et un" . 41)
		("cinquante" . 50)
		("soixante" . 60)
		("cent" . 100)
		("mille" . 1000))
	"Number words.")

(defun sacha-number-words-regexp ()
	"Return a regular expression that matches the number words."
	(format "\\(?:%s\\|[0-9]+\\)" (mapconcat 'car sacha-number-words "\\|")))

(defvar sacha-whisper-commands
  `(("insert \\(.+\\)" . sacha-whisper-always-insert-at-point)
		("task note,? \\(.+\\)" . sacha-whisper-always-insert-at-current-task)
		("comment,? \\(.+\\)" . sacha-whisper-insert-as-comment)
		("scroll up" . scroll-down-command)	; this is a test
    ("scrolling up" . scroll-down-command)
    ("page up" . scroll-down-command)
    ("scroll down" . scroll-up-command)
    ("scroll down" . scroll-up-command)
    ("page down" . scroll-up-command)
    ("next page" . scroll-up-command)
    ("go back" . winner-undo)
		("okay,? push" . magit-push-current-to-pushremote)
    ("close other windows" . delete-other-windows)
    ("run the buffer" . eval-buffer)
    ("mark buffer" . mark-whole-buffer)
    ("run buffer" . eval-buffer)
    ("run function" . eval-defun)
		("save buffer" . save-buffer)
    ("mark paragraph" . mark-paragraph)
    ("expand" . expand-region)
    ("start emacs news" . sacha-workflow-emacs-news-start)
    ("update emacs calendar" . sacha-workflow-emacs-calendar-update)
		("\\(?:new task\\|switch to\\|change gears\\|switch gears\\|okay, now I'm going to \\)\\(.+\\)" . sacha-whisper-switch-task-to)
		("task today" . sacha-whisper-task-today)
		("task someday" . sacha-whisper-task-someday)
		("remind me today\\(?: to\\)? \\(.+\\)" . sacha-whisper-task-today)
		("remind me someday\\(?: to\\)? \\(.+\\)" . sacha-whisper-task-someday)
		("remind me next week\\(?: to\\)? \\(.+\\)" . sacha-whisper-task-next-week)
		("remind me next month\\(?: to\\)? \\(.+\\)" . sacha-whisper-task-next-month)
		("remind me tomorrow\\(?: to\\)? \\(.+\\)" . sacha-whisper-task-tomorrow)
		("clip audio" . sacha-org-subed-record-audio-insert-link-and-replay)
		("remind me to" . sacha-whisper-task-someday)
		("remind me\\(?: that\\)?" . sacha-whisper-note)
		("\\(?:rappelle\\|rappellez\\)-moi aujourd'hui\\(?: de\\| que\\)? \\(.+\\)" . sacha-whisper-task-today)
		("\\(?:rappelle\\|rappellez\\)-moi un jour\\(?: de\\| que\\)? \\(.+\\)" . sacha-whisper-task-someday)
		("\\(?:rappelle\\|rappellez\\)-moi demain\\(?: de\\| que\\)? \\(.+\\)" . sacha-whisper-task-tomorrow)
		("\\(?:rappelle\\|rappellez\\)-moi la semaine prochaine\\(?: de\\| que\\)? \\(.+\\)" . sacha-whisper-task-next-week)
		("\\(?:rappelle\\|rappellez\\)-moi\\(?: que\\| de\\)? \\(.+\\)" . sacha-whisper-note)
		("remind me\\(?: to\\)? \\(.+\\)" . sacha-whisper-note)
		("new note\\(?: to\\)? \\(.+\\)" . sacha-whisper-note)
		("journal,? \\(.+\\)" . sacha-whisper-journal)
		("log \\(.+\\)" . sacha-whisper-note)
		("agenda" . sacha-org-check-agenda)
		("inbox" . sacha-whisper-inbox)
		("current task" . sacha-whisper-current-task)
		("what was I doing" . sacha-whisper-current-task)
		("what's \\(?:the\\|a\\) stack\\|where was I" . sacha-whisper-clocked-tasks)
		("what can I say" . sacha-whisper-what-can-i-say)
		("toot" . sacha-whisper-draft-toot)
		("open \\(.+\\)" . sacha-whisper-open-favorite)
		("jump \\(.+\\)" . sacha-whisper-avy-jump)
		("\\(?:row\\|line\\|ligne\\) \\(.+\\)" . sacha-whisper-jump-to-line)
		("\\(?:simple\\|symbol\\) \\(.+\\)" . sacha-whisper-avy-insert-symbol)
		("start recording" . sacha-whisper-start-recording)
		("stop recording" . sacha-whisper-stop-recording)
		("that's done" . sacha-org-mark-done)
		("go to refiled?,? \\(.+\\)" . sacha-whisper-org-goto-refiled)
		("let's get ready\\|prepare to s[tc]ream\\|prepare for takeoff" . sacha-whisper-prepare-to-stream)
		("\\(?:man,?\\|I command you to\\) \\(.+\\)" . sacha-whisper-execute-extended-command)
		("start streaming\\|start screaming\\|let's go live" . sacha-whisper-start-streaming)
		("define \\(?:a \\)?test for this function" . sacha-ert-deftest-from-function-at-point)
		("stop streaming\\|stop screaming\\|over and out" . sacha-whisper-stop-streaming)
		;; someday it would be nice to have a number parser
		(,(format "\\(?:trip\\|clip\\|click\\)? \\(?:the last \\)?\\(%s\\(?:[- ]%s\\)* \\(minute\\|second\\)s?\\)"
							(sacha-number-words-regexp)
							(sacha-number-words-regexp))
		 . sacha-whisper-clip)
		("rename clip,? \\(.+\\)" . ,(lambda (text) (message "%s" (file-name-base (sacha-clip-add-note-to-latest text))) ""))
		("instant replay" . sacha-clip-play-latest)
		("panic button" . sacha-obs-panic)
		,(sacha-whisper-track "Routines")
		,(sacha-whisper-track "Consulting" "E1 Gen")
		,(sacha-whisper-track "Childcare")
		,(sacha-whisper-track "Emacsconf" "Emacs | Emacsconf")
		,(sacha-whisper-track "track Emacs" "Emacs")
		,(sacha-whisper-track "Sleep")
		)
  "Commands for speech recognition.")

;;;###autoload
(defun sacha-whisper-clip (text)
	"Clip the last part of the recording and save as a different file."
	(let* ((multiplier (if (string-match "second" text) 1 60))
				 (n (car (sacha-parse-number-words (replace-regexp-in-string " \\(minute\\|second\\)s?" "" text)))))
		(sacha-clip-seconds (* n multiplier))
		(sacha-whisper-audio-feedback (format "Clipping %d" n)))
	"")

;;;###autoload
(defun sacha-whisper-track (text &optional category)
	"Track my time."
	(cons (concat "\\(?:" text "\\)")
				(lambda ()
					(message "Tracking %s" (or category text))
					(quantified-track (or category text)))))


;;;###autoload
(defun sacha-whisper-always-insert-at-current-task (text)
	"Insert TEXT at the end of the currently-clocked task."
	(save-window-excursion
		(save-excursion
			(org-clock-goto)
			(org-end-of-subtree)
			(unless (bolp) (insert "\n"))
			(insert text "\n")))
	"")

;;;###autoload
(defun sacha-whisper-always-insert-at-point (text)
	"Insert TEXT at point."
	(insert text)
	"")

;;;###autoload
(defun sacha-whisper-journal (text)
  "Save TEXT to my journal."
	(sacha-journal-post (s-capitalize text) :Category "Us")
	"")

;;;###autoload
(defun sacha-whisper-insert-as-comment (text)
	"Insert TEXT as a comment.
Based on `comment-dwim'."
	(if (save-excursion (beginning-of-line) (not (looking-at "\\s-*$")))
			(comment-indent)
		(if comment-insert-comment-function
				(funcall comment-insert-comment-function)
			(let ((add (comment-add 1)))
				;; Some modes insist on keeping column 0 comment in column 0
				;; so we need to move away from it before inserting the comment.
				(indent-according-to-mode)
				(insert (comment-padright comment-start add))
				(save-excursion
					(unless (string= "" comment-end)
						(insert (comment-padleft comment-end add)))
					(indent-according-to-mode)))))
	(insert (if (looking-back " " (1- (point))) "" " ") (s-capitalize text))
	"")

;;;###autoload
(defun sacha-whisper-avy-jump (text)
	"Jump to the specified word."
	(let ((candidates
				 (or
					(avy--regex-candidates text)
					(avy--regex-candidates (concat "\\<" (substring (downcase text) 0 1))))))
		(if candidates
				(avy-process candidates)
			(setq unread-command-events
						(listify-key-sequence
						 (substring text 0 1)))
			(call-interactively #'avy-goto-word-1-below)))
	"")

;;;###autoload
(defun sacha-whisper-open-favorite (text)
	"Open the favorite."
	(if-let* ((url (sacha-org-favorite-match text)))
			(progn
				(org-link-open-from-string url)
				"")
		text))

;;;###autoload
(defun sacha-whisper-avy-insert-symbol (text)
	"Look for a symbol that contains TEXT and insert it at point."
	(let ((candidates
				 (or
					(avy--regex-candidates text)
					(avy--regex-candidates (concat "\\<" (substring (downcase text) 0 1)))))
				(avy-action 'sacha-avy-action-insert-symbol)
				results)
		(when candidates
			;; check if the candidates all resolve to the same
			(progn
				(setq results
							(seq-uniq
							 (seq-map (lambda (c)
													(save-window-excursion
														(save-excursion
															(with-selected-window
																	(cdr c)
																(goto-char (caar c))
																(thing-at-point 'symbol)))))
												candidates)))
				(if (and (= (length results) 1)
								 (car results))
						(insert (car results))
					(avy-process candidates)))
			"")))

;;;###autoload
(defun sacha-whisper-jump-to-line (text)
  "Go to the visible line modulo 100."
	(let* ((number (if (string-match "[0-9]+" text)
										 (string-to-number text)
									 (car (sacha-parse-number-words text))))
				 (adjusted (+ (* (/ (line-number-at-pos (point)) 100) 100)
											number)))
		(cond
		 ((< adjusted (line-number-at-pos (window-start)))
			(incf adjusted 100))
		 ((> adjusted (line-number-at-pos (window-end)))
			(decf adjusted 100)))
		(goto-line adjusted)
		""))

(ert-deftest sacha-parse-number-words ()
  "Tests `sacha-parse-number-words'."
	(should
	 (equal
		(sacha-parse-number-words "five minutes")
		'(5 . "minutes")))
	(should
	 (equal
		(sacha-parse-number-words "twenty five")
		'(25 . "")))
	(should
	 (equal
		(sacha-parse-number-words "mille neuf cent quatre-vingt-dix-neuf test")
		'(1999 . "test"))))

(defun sacha-parse-number-words (str)
  "Parse number words in English or French.
Return (number . rest-of-string)."
	(if (string-match "^[0-9]+" (string-trim str))
			(string-to-number str)
		(let ((total 0)
					(current 0)
					found)
			(catch 'done
				(while (> (length str) 0)
					(setq found
								(cdr (seq-find (lambda (o) (string-match (concat "^" (car o)) str))
															 sacha-number-words)))
					(if found
							(progn
								(setq str
											(if (< (1+ (match-end 0)) (length str))
													(replace-regexp-in-string "^[- ]+" "" (substring str (1+ (match-end 0))))
												""))
								(cond
								 ((= found 100) (setq current (* (max current 1) 100)))
								 ((>= found 1000)
									(incf total (* (max current 1) found))
									(setq current 0))
								 (t (incf current found))))
						(throw 'done t)))
				(throw 'done t))
			(cons (+ total current)
						str))))

;;;###autoload
(defun sacha-whisper-inbox ()
  "Open my inbox file."
  (interactive)
	(jump-to-register ?i)
	"") ; TODO: remove the need for the register


;;;###autoload
(defun sacha-whisper-org-goto-refiled (text)
  "Go to a heading based on refile."
	(setq unread-command-events (listify-key-sequence (concat text (kbd "RET"))))
	(let ((current-prefix-arg '(4))
				(vertico-sort-function 'vertico-sort-length-alpha))
		(call-interactively #'org-refile))
	"")

;;;###autoload
(defun sacha-whisper-execute-extended-command (text)
  "Run Emacs command."
	(setq unread-command-events (listify-key-sequence (concat text (kbd "RET"))))
	(let ((current-prefix-arg '(4))
				(vertico-sort-function 'vertico-sort-length-alpha))
		(call-interactively #'execute-extended-command))
	"")

(defun sacha-whisper-audio-feedback (text)
	"Give me TEXT as basic audio feedback."
	(let ((learn-lang-language "en")
				(learn-lang-tts-function 'learn-lang-tts-gtts-say))
		(learn-lang-tts-say text)))

;;;###autoload
(defun sacha-whisper-start-streaming ()
  "Start streaming and let me know."
  (interactive)
	(obs-websocket-start-streaming)
	(obs-websocket-update-stream-status)
	(sit-for 1)
	(let ((learn-lang-language "en")
				(learn-lang-tts-function 'learn-lang-tts-gtts-say))
		(if obs-websocket-streaming-p
				(learn-lang-tts-say "Live")
			(learn-lang-tts-say "Weird"))))

;;;###autoload
(defun sacha-whisper-stop-streaming ()
  "Stop streaming and let me know."
  (interactive)
	(obs-websocket-stop-streaming)
	(obs-websocket-update-stream-status)
	(sacha-stream-or-video-global-mode -1)
	(sit-for 1)
	(sacha-whisper-audio-feedback
	 (if obs-websocket-streaming-p
			 "Weird"
		 "Off")))

;;;###autoload
(defun sacha-whisper-start-recording ()
  "Stop recording and let me know."
  (interactive)
	(obs-websocket-start-recording)
	(obs-websocket-update-recording-status)
	(sit-for 0.2)
	(if obs-websocket-recording-p
			(message "Recording")
		(sacha-whisper-audio-feedback "Weird")))

;;;###autoload
(defun sacha-whisper-stop-recording ()
  "Stop recording and let me know."
  (interactive)
	(obs-websocket-stop-recording)
	(obs-websocket-update-recording-status)
	(sit-for 1)
	(sacha-whisper-audio-feedback
	 (if obs-websocket-recording-p
			 "Weird"
		 "Stopped")))

;;;###autoload
(defun sacha-whisper-what-can-i-say ()
	(interactive)
	(pop-to-buffer (find-file-noselect "~/sync/orgzly/speech.org"))
	(view-mode)
	"")

;;;###autoload
(defun sacha-whisper-current-task ()
	(interactive)
	(org-clock-goto)
	"")

;;;###autoload
(defun sacha-whisper-clocked-tasks ()
	(interactive)
	(org-clock-goto t)
	"")

;;;###autoload
(defun sacha-whisper-draft-toot (text)
	(if (get-buffer "*new toot*")
			(switch-to-buffer (get-buffer "*new toot*"))
		(mastodon-toot))
	(goto-char (point-max))
	(unless (looking-back "^\\| " 5)
		(insert " ")
		(insert text))
	"")

;;;###autoload
(defun sacha-whisper-handle-commands (text)
	(interactive (list (if (region-active-p)
												 (buffer-substring (region-beginning) (region-end))
											 (buffer-substring (line-beginning-position) (line-end-position)))))
	(let (done entry func num-args)
		(while (and text (not (string= text "")) (not done))
			(setq entry
						(seq-find
						 (lambda (row)
							 (string-match (format "^\\(?:%s\\)\\>" (car row)) text))
						 sacha-whisper-commands))
			(if (null entry)
					(setq done t)
				(setq func (cdr entry))
				(setq num-args (and func (func-arity func)))
				(cond
				 ((commandp func)
					(setq text (replace-match "" nil nil text))
					(call-interactively func))
				 ((and (functionp func) (>= (cdr num-args) 1))
					(unless (match-string 1 text)
						(setq text (replace-match "" nil nil text)))
					(setq text (funcall func (or (match-string 1 text) text))))
				 ((functionp func)
					(setq text (replace-match "" nil nil text))
					(funcall func))))
			(when (string-match "^[ \\.?,]+\\(?:and \\)?" text)
				(setq text (replace-match "" nil nil text))))
		(when (string= text "") (setq text nil))
		text))

(defvar sacha-whisper-replacements
  '((" *\\<start \\(list\\|next\\) item\\>[\\.,] *" . "\n- ")
    (" *\\<start check ?box\\>[\\.,] *" . "\n- [ ] ")
    (" *start paragraph[\\.,]? *" . "\n\n")))

;;;###autoload
(defun sacha-whisper-process-replacements ()
  (goto-char (point-min))
  (when (looking-at " +") (replace-match ""))
  (let ((case-fold-search t))
    (cond
     ((re-search-forward  " *okay[,\\.]? stop recording" nil t)
      (when (process-live-p whisper--recording-process)
        (replace-match "")
        (message "Stopping.")
        (sacha-whisper-done)))))
  (dolist (rep sacha-whisper-replacements)
    (goto-char (point-min))
    (while (re-search-forward (car rep) nil t)
      (replace-match (cdr rep))))
  (goto-char (point-max))
  (insert " "))

;; Slowly building speech-based commands for Emacs:1 ends here

;; [[file:../Sacha.org::#writing-and-editing-speech-recognition-okay-track][Okay, track...:1]]
(defvar sacha-quantified-common-categories
  '(("Emacs" . "Discretionary - Productive - Emacs")
    ("Child care" . "Childcare")
    ("French" . "Discretionary - French")
    ("Brigade" . "Discretionary - Productive - Bike Brigade")
    ("Consulting" . "E1 Gen")))

;;;###autoload
(defun sacha-speech-input-quantified-track (text)
  "Start tracking time."
  (if (and text
           (string-match "^ok\\(?:ay\\)?[,\\.]? track \\(.+\\)" text))
      (let ((category
             (speech-input-match-in-list
              (match-string 1 text)
              (mapcar 'car sacha-quantified-common-categories))))
        (message "Tracking %s" category)
        (quantified-track
         (assoc-default category sacha-quantified-common-categories #'string=))
        nil)
    text))
;; Okay, track...:1 ends here

;; [[file:../Sacha.org::#writing-and-editing-speech-recognition-keep-track-of-files-and-stats][Keep track of files and stats:1]]
;;;###autoload
(defun sacha-whisper-add-text-properties ()
	"Calculate time elapsed and keep track of filename."
	(when (and whisper--temp-file whisper--time-started)
		(goto-char (point-min))
		(let* ((time-elapsed
						(- (float-time) whisper--time-started))
					 (duration
						(/ (compile-media-get-file-duration-ms whisper--temp-file) 1000.0)))
			(add-text-properties
			 (point-min) (point-max)
			 `(file ,whisper--temp-file
							time-elapsed ,time-elapsed
							orig-duration ,duration
							ratio ,(/ time-elapsed duration))))))

;;;###autoload
(defun sacha-whisper-replay-file-from-text ()
  "Replay the file at point."
  (interactive)
	(if (get-text-property (point) 'file)
			(let ((mpv-default-options
						 (append
							(list "--vid=no" "--no-video" "--window-minimized=yes" )
							(when (get-text-property (point) 'start)
								(list (format "--start=%f" (get-text-property (point) 'start))))
							(when (get-text-property (point) 'end)
								(list (format "--end=%f" (get-text-property (point) 'end))))
							mpv-default-options)))
				(mpv-play (get-text-property (point) 'file)))
		(message "No file data.")))
;; Keep track of files and stats:1 ends here

;; [[file:../Sacha.org::#writing-and-editing-speech-recognition-using-speech-recognition-for-translations-in-emacs-and-faking-in-buffer-completion-for-the-results][Using speech recognition for on-the-fly translations in Emacs and faking in-buffer completion for the results:2]]
;;;###autoload
(defun sacha-whisper-translate ()
  (goto-char (point-min))
  (let ((case-fold-search t))
    (when (re-search-forward "okay[,\\.]? translate[,\\.]? \\(.+\\)\\|okay[,\\.]? \\(.+?\\) in French" nil t)
      (let* ((s (or (match-string 1) (match-string 2)))
             (translation (save-match-data (sacha-learn-lang-en-to-fr s))))
        (replace-match
         (propertize translation
                     'type-hint translation
                     'type-original s
                     'help-echo s))))))

;; Using speech recognition for on-the-fly translations in Emacs and faking in-buffer completion for the results:2 ends here

;; [[file:../Sacha.org::#writing-and-editing-speech-recognition-using-speech-recognition-for-translations-in-emacs-and-faking-in-buffer-completion-for-the-results][Using speech recognition for on-the-fly translations in Emacs and faking in-buffer completion for the results:4]]
;;;###autoload
(defun sacha-whisper-maybe-type-with-hints (text)
  "Add this function to `whisper-insert-text-at-point'."
  (let* ((hint (and text (org-find-text-property-in-string 'type-hint text)))
         (original (and text (org-find-text-property-in-string 'type-original text))))
    (if hint
        (progn
          (learn-lang-type-with-hint hint original)
          nil)
      text)))
;; Using speech recognition for on-the-fly translations in Emacs and faking in-buffer completion for the results:4 ends here

;; [[file:../Sacha.org::#writing-and-editing-speech-recognition-streaming-speech-recognition-into-emacs-using-google-chrome-web-speech-api][Streaming speech recognition into Emacs using Google Chrome Web Speech API:3]]
;;;###autoload
(defun sacha-speech-sessions ()
  (seq-keep (lambda (o)
              (with-current-buffer o
                (when sacha-speech-session
                  (cons sacha-speech-session o))))
            (buffer-list)))

;;;###autoload
(defun sacha-speech-clear-all ()
  (interactive)
  (dolist (session (sacha-speech-sessions))
    (sacha-speech-clear session)))

;;;###autoload
(defun sacha-speech-clear (session)
  (interactive (list (sacha-speech-select-session)))
  (with-current-buffer (cdr session)
      (erase-buffer)
      (setq-local sacha-speech-previous-final nil)))

;;;###autoload
(defun sacha-speech-select-session (&optional prompt)
  (let ((sessions (sacha-speech-sessions)))
    (if (= (length sessions) 1)
        (car sessions)
      (assoc
       (completing-read
        (or prompt "Session: ")
        (mapcar 'car sessions))
       sessions))))

(defvar-local sacha-speech-input "VirtualMicSink:input")

;;;###autoload
(defun sacha-speech-rewire (&optional id input)
  "Unhook it from all input and reconnect it to `sacha-speech-input'.
Call with \\[universal-argument] to specify the input."
  (interactive (list (sacha-speech-select-session)
                     (if current-prefix-arg
                         (epwgraph-complete-logical-node-name)
                       sacha-speech-input)))
  (with-current-buffer (cdr id)
    (setq input (or input sacha-speech-input))
    (setq-local sacha-speech-input input)
    (let* ((node-name (concat (car id) ":input"))
           (session-ports (epwgraph-get-ports-with-logical-name
                           node-name))
           (new-ports (if (stringp input)
                          (epwgraph-get-ports-with-logical-name input)
                        input))
           (old-incoming (epwgraph-get-incoming-links session-ports)))
      (epwgraph-disconnect-all-inputs-for-logical-node session-ports)
      (epwgraph-connect-logical-nodes
       (epwgraph--map-channels new-ports session-ports)))))

;;;###autoload
(defun sacha-speech-get-text-and-clear (session)
  (let (text)
    (with-current-buffer (cdr session)
      (setq text (buffer-substring-no-properties (point-min) (point-max)))
      (erase-buffer)
      (setq-local sacha-speech-previous-final nil))
    text))

;;;###autoload
(defun sacha-speech-insert-at-point (session)
  (interactive (list (sacha-speech-select-session)))
  (insert (sacha-speech-get-text-and-clear session)))

;;;###autoload
(defun sacha-speech-save-to-clocked-task (session)
  (interactive (list (sacha-speech-select-session)))
  (save-window-excursion
    (let ((link (org-store-link nil)))
      (org-clock-goto)
      (org-end-of-subtree)
      (unless (bolp)
        (insert "\n"))
      (insert "\n")
      (when link (insert link "\n"))
      (insert (sacha-speech-get-text-and-clear session) "\n"))))
;; Streaming speech recognition into Emacs using Google Chrome Web Speech API:3 ends here

;; [[file:../Sacha.org::#writing-and-editing-speech-recognition-streaming-speech-recognition-into-emacs-using-google-chrome-web-speech-api][Streaming speech recognition into Emacs using Google Chrome Web Speech API:4]]
(defvar sacha-speech-etherpads nil "Alist of (session . pad-id)")
;; (setq sacha-speech-etherpads '(("chrome-VgjMhu" . "test")))

;;;###autoload
(defun sacha-speech-append-to-etherpad (info)
  (when (and info (string= (assoc-default 'type info) "FINAL"))
    (let-alist info
      (when-let* ((pad-id (assoc-default .session sacha-speech-etherpads #'string=)))
        (emacsconf-pad-append-text pad-id (concat "\n" .content)))))
  info)

;;;###autoload
(defun sacha-speech-link-etherpad (session pad-id)
  (interactive (list
                (sacha-speech-select-session)
                (read-string "Pad ID: ")))
  (add-to-list 'sacha-speech-etherpads
               (cons (concat "#" (car session))
                     pad-id)))

;;;###autoload
(defun sacha-speech-unlink-etherpad (pad-id)
  (interactive (list (completing-read "Pad: " (mapcar 'cdr sacha-speech-etherpads))))
  (setq sacha-speech-etherpads
        (seq-remove (lambda (o)
                      (string= (cdr o) pad-id))
                    sacha-speech-etherpads)))
;; Streaming speech recognition into Emacs using Google Chrome Web Speech API:4 ends here

;; [[file:../Sacha.org::#writing-and-editing-speech-recognition-streaming-speech-recognition-into-emacs-using-google-chrome-web-speech-api][Streaming speech recognition into Emacs using Google Chrome Web Speech API:6]]
(defvar sacha-speech-erc nil "Alist of (session . channel)")
;; (setq sacha-speech-erc '(("#chrome-HP7k8I" . "#emacsconf-test")))

;;;###autoload
(defun sacha-speech-send-to-erc (info)
  (when (and info (string= (assoc-default 'type info) "FINAL"))
    (let-alist info
      (when-let* ((channel (assoc-default .session sacha-speech-erc #'string=)))
        (emacsconf-erc-with-channels (list channel)
          (erc-send-message (string-trim .content))))))
  info)

;;;###autoload
(defun sacha-speech-link-erc (session channel)
  (interactive (list
                (sacha-speech-select-session)
                (read-string "Channel: ")))
  (add-to-list 'sacha-speech-erc
               (cons (concat "#" (car session))
                     channel)))

;;;###autoload
(defun sacha-speech-unlink-channel (channel)
  (interactive (list (completing-read "Channel: " (mapcar 'cdr sacha-speech-erc))))
  (setq sacha-speech-erc
        (seq-remove (lambda (o)
                      (string= (cdr o) channel))
                    sacha-speech-erc)))
;; Streaming speech recognition into Emacs using Google Chrome Web Speech API:6 ends here

;; [[file:../Sacha.org::#writing-and-editing-speech-recognition-streaming-speech-recognition-into-emacs-using-google-chrome-web-speech-api][Streaming speech recognition into Emacs using Google Chrome Web Speech API:8]]
;;;###autoload
(defun sacha-speech-fix-common-errors (info)
  (with-temp-buffer
    (insert (alist-get 'content info))
    (goto-char (point-min))
    (sacha-subed-fix-common-errors-from-start)
    (setf (alist-get 'content info) (buffer-string)))
  info)
;; Streaming speech recognition into Emacs using Google Chrome Web Speech API:8 ends here

;; [[file:../Sacha.org::#writing-and-editing-speech-recognition-streaming-speech-recognition-into-emacs-using-google-chrome-web-speech-api][Streaming speech recognition into Emacs using Google Chrome Web Speech API:10]]
;;;###autoload
(defun sacha-speech-insert-at-markers (info)
  (when (and sacha-whisper-target-markers info)
    (sacha-whisper-insert (alist-get 'content info))))
(add-hook 'sacha-speech-functions #'sacha-speech-insert-at-markers 100)

;; Streaming speech recognition into Emacs using Google Chrome Web Speech API:10 ends here

;; [[file:../Sacha.org::#writing-and-editing-speech-recognition-streaming-speech-recognition-into-emacs-using-google-chrome-web-speech-api-speech-and-subed-record][speech and subed-record:1]]
(defvar sacha-speech-timestamp-adjust-before 1000)
(defvar sacha-speech-timestamp-adjust-after 300)

;;;###autoload
(defun sacha-speech-subed-record-convert-timestamp (s)
  "Convert S into a relative number of milliseconds based on `subed-record-filename'."
  (floor (* (float-time (time-subtract (date-to-time s) subed-record-start-time)) 1000.0)))

;;;###autoload
(defun sacha-speech-subed-record-distance (s1 s2)
  (/
   (* 1.0
      (string-distance (downcase (replace-regexp-in-string "[^A-Za-z]"
                                                           ""
                                                           s1))
                       (downcase (replace-regexp-in-string "[^A-Za-z]"
                                                           ""
                                                           s2))))
   (max (length s1)
        (length s2))))

;;;###autoload
(defun sacha-speech-subed-record-close-enough (s1 s2)
  "Return t if it's close enough."
  (< (sacha-speech-subed-record-distance s1 s2) 0.3))

;;;###autoload
(defun sacha-speech-subed-record-update (info)
  (let ((start-ms (- (sacha-speech-subed-record-convert-timestamp
                      (alist-get 'start info))
                     sacha-speech-timestamp-adjust-before))
        (stop-ms (+ (sacha-speech-subed-record-convert-timestamp
                     (alist-get 'end info))
                    sacha-speech-timestamp-adjust-after)))
    (subed-set-subtitle-time-start start-ms)
    (subed-set-subtitle-time-stop stop-ms)
    (subed-set-subtitle-comment
	   (concat
		  (if (subed-subtitle-comment)
				  (concat (string-trim (replace-regexp-in-string
									              "#\\+AUDIO: .*\\(\n\\|$\\)?" ""
									              (subed-subtitle-comment)))
								  "\n")
			  "")
		  (format "#+AUDIO: %s" subed-record-filename)))
    (message "%.1f %s"
             (sacha-speech-subed-record-distance
              (alist-get 'content info)
              (subed-subtitle-text))
             (alist-get 'content info))))

(defvar sacha-speech-subed-ignore nil "Ignore the GTTS-CLI output.")
;;;###autoload
(defun sacha-speech-subed-record-process (info)
  (let ((text (alist-get 'content info))
        (current (subed-subtitle-text)))
    (cond
     ((sacha-speech-subed-record-close-enough text current)
      (sacha-speech-subed-record-update info)
      (subed-forward-subtitle-text)
      (sacha-learn-lang-say-current-subtitle
       (lambda ()
         (setq sacha-speech-subed-ignore nil))))
     ;; Check previous
     ((sacha-speech-subed-record-close-enough
       text
       (save-excursion
         (subed-backward-subtitle-text)
         (subed-subtitle-text)))
      (save-excursion
        (subed-backward-subtitle-text)
        (sacha-speech-subed-record-update info)))
     ;; Check next
     ((sacha-speech-subed-record-close-enough
       text
       (save-excursion
         (subed-forward-subtitle-text)
         (subed-subtitle-text)))
      (save-excursion
        (subed-forward-subtitle-text)
        (sacha-speech-subed-record-update info)))
     (t
      (sacha-speech-subed-record-update info)))))
;;;###autoload
(defun sacha-speech-subed-record (info)
  (when (and (string= (alist-get 'type info) "FINAL")
             (derived-mode-p 'subed-mode)
             (boundp 'subed-record-start-time)
             subed-record-start-time
             (not sacha-speech-subed-ignore))
    (sacha-speech-subed-record-process info))
  info)
;; speech and subed-record:1 ends here

(provide 'sacha-speech-input)
;;; sacha-speech-input.el ends here
