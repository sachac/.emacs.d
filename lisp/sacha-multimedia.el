;;; sacha-multimedia.el ---  -*- lexical-binding: t -*-

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



;; [[file:../Sacha.org::#writing-and-editing-learning-french-process-audio-files][Process audio files:1]]
;;;###autoload
  (defun sacha-audio-clip (source start-time end-time destination text)
    (interactive
     (let ((s (and (region-active-p) (buffer-substring (region-beginning) (region-end)))))
       (if (and s
                (string-match "\\(\\(?:\\(?:[0-9]+\\):\\)?\\(?:[0-9]+\\):\\(?:[0-9]+\\)\\(?:\\.\\(?:[0-9]+\\)\\)?\\)[ \n\t]+\\(\\(?:\\(?:[0-9]+\\):\\)?\\(?:[0-9]+\\):\\(?:[0-9]+\\)\\(?:\\.\\(?:[0-9]+\\)\\)?\\)" s))
           (let ((start (match-string 1 s))
                 (end (match-string 2 s)))
             (list
              (read-file-name "Source: " nil nil t)
              start
              end
              (read-file-name "Destination: ")
              (read-string "Text: ")))
         (list (read-file-name "Source: " nil nil t)
               (read-string "Start time: ")
               (read-string "End time: ")
               (read-file-name "Destination: ")
               (read-string "Text: ")))))
    (let ((result (call-process "ffmpeg" nil (get-buffer-create "*ffmpeg*") nil
                                "-y" ; Overwrite output file without asking
                                "-i" (expand-file-name source) ; Input file
                                "-ss" start-time ; Start time (e.g., 00:00:10)
                                "-to" end-time ; End time/Stop time
                                (expand-file-name destination))))
      (when result
        (when (region-active-p) (delete-region (region-beginning) (region-end)))
        (insert (org-link-make-string
                 (concat "audio:" (replace-regexp-in-string (getenv "HOME") "~" destination))
                 text)))))
;; Process audio files:1 ends here

;; [[file:../Sacha.org::#gif-screencast][gif-screencast:1]]
;;;###autoload
  (defun sacha-gif-screencast-start-or-stop-and-choose-thumbnail ()
          "Start a screencast or pause recording."
          (interactive)
          (if gif-screencast-mode
                          (progn
                                  (gif-screencast-toggle-pause)
                                  (dired gif-screencast-screenshot-directory)
                                  (revert-buffer)
                                  (dired gif-screencast-screenshot-directory)
                                  (image-dired gif-screencast-screenshot-directory))
                  (gif-screencast)))
;; gif-screencast:1 ends here

;; [[file:../Sacha.org::#gif-screencast][gif-screencast:2]]
;;;###autoload
  (defun sacha-gif-screencast-copy-image-to-first-frame (file)
          (interactive (list (dired-get-filename)))
    ;; Determine the timestamp of the first file in this directory
          (copy-file
           file
           (expand-file-name
                  (format-time-string
                   "screen-%F-%T-%3N.png"
                   (time-subtract
                          (sacha-gif-screencast-timestamp-from-filename
                           (car (directory-files gif-screencast-screenshot-directory nil ".png")))
                          (seconds-to-time 0.001)))
                  gif-screencast-screenshot-directory)))

;;;###autoload
  (defun sacha-gif-screencast-timestamp-from-filename (file)
          (setq file (replace-regexp-in-string "^screen-" "" (file-name-base file)))
          (time-add (date-to-time (format "%s %s" (substring file 0 10) (substring file 11 19)))
                                                  (float-time (/ (string-to-number (substring file 20 23)) 1000.0))))
  (cl-assert
   (string= (format-time-string "test-%F-%T-%3N" (sacha-gif-screencast-timestamp-from-filename "screen-2024-09-20-13:18:08-024.png"))
                                          "test-2024-09-20-13:18:08-024"))

;;;###autoload
  (defun sacha-gif-screencast-update-frames-from-directory ()
          (interactive)
          (let* ((files (directory-files gif-screencast-screenshot-directory nil ".png"))
                                   (start-time (sacha-gif-screencast-timestamp-from-filename (car files))))
                  (setq gif-screencast--frames
                                          (mapcar (lambda (o)
                                                                                  (make-gif-screencast-frame
                                                                                   :timestamp (sacha-gif-screencast-timestamp-from-filename o)
                                                                                   :filename o))
                                                                          files))
                  (gif-screencast-mode 0)
                  (gif-screencast--finish)))
;; gif-screencast:2 ends here

;; [[file:../Sacha.org::#transcripts-from-sacha-phone][Transcripts from my phone:1]]
  (defvar sacha-audio-braindump-dir "~/sync/Phone")
;;;###autoload
  (defun sacha-open-latest-braindump ()
    (interactive)
    (find-file (sacha-latest-file sacha-audio-braindump-dir "\\.txt"))
    (kill-new (buffer-string)))

;;;###autoload
  (defun sacha-insert-latest-braindump ()
    (interactive)
    (insert-file-contents (sacha-latest-file sacha-audio-braindump-dir "\\.txt")))
;;;###autoload
  (defun sacha-audio-braindump-dired ()
          (interactive)
          (dired sacha-audio-braindump-dir "-lt"))
  (defalias 'sacha-phone-dired #'sacha-audio-braindump-dired)
;; Transcripts from my phone:1 ends here

;; [[file:../Sacha.org::#svg-animating-paths-in-order][Animating paths in order:3]]
;;;###autoload
(defun sacha-ffmpeg-animate-images (files output-file &optional framerate)
	"Make an animated GIF or WEBM out of FILES.
Save it to OUTPUT-FILE.
If FRAMERATE is specified, use that instead of 30."
	(setq framerate (or framerate 30))
	(if (string-match "\\.webm$" output-file)
			(let ((compile-media-ffmpeg-arguments
						 (append compile-media-ffmpeg-arguments
										 (list "-r"
													 (number-to-string framerate)))))
				(compile-media `((video ,@(mapcar (lambda (o) (list :source o :duration-ms (/ 1000.0 framerate)
																														:before-input
																														(list "-width" compile-media-output-video-width)))
																					files)))
											 output-file))
		(with-current-buffer (get-buffer-create "*gif*")
			(erase-buffer)
			(let ((frame-input (seq-mapcat (lambda (o) (list "-i" o)) files))
						(palette (make-temp-file "palette" nil ".png")))
				(insert "ffmpeg "
								(string-join (append frame-input (list "-vf" "palettegen" "-y" palette)) " ")
								"\n")
				(apply #'call-process "ffmpeg" nil t t
							 (append frame-input (list "-vf" "palettegen" "-y" palette)))
				(insert "ffmpeg "
								(string-join (append (list "-i" palette "-lavfi" "paletteuse")
																		 (list "-framerate" (number-to-string framerate))
																		 frame-input
																		 (list "-loop" "-1" "-y" output-file)) " ")
								"\n")
				(apply #'call-process "ffmpeg" nil t t
							 (append (list "-i" palette "-lavfi" "paletteuse")
											 (list "-framerate" (number-to-string framerate))
											 frame-input
											 (list "-loop" "-1" "-y" output-file)))
				(delete-file palette))
			(display-buffer (current-buffer))))
	output-file)
;; Animating paths in order:3 ends here

;; [[file:../Sacha.org::#multimedia-ffmpeg][FFmpeg:1]]
;;;###autoload
(defun sacha-ffmpeg-save-last-frame-as-image (input-file output-image)
	(interactive "FInput: \nFOutput: ")
	(let ((args (list
							 "-sseof" "-2"
							 "-i"
							 (expand-file-name input-file)
							 "-update"
							 "1"
							 "-q:v"
							 "1"
							 "-y"
							 (expand-file-name output-image))))
		(with-current-buffer (get-buffer-create "*ffmpeg*")
			(insert "\nffmpeg "
							(mapconcat #'shell-quote-argument args " ") "\n")
			(apply 'call-process "ffmpeg" nil t nil args))))
;; FFmpeg:1 ends here

;; [[file:../Sacha.org::#transcript-editing][Transcript editing:2]]
;;;###autoload
(defun sacha-emms-player-mplayer-set-speed (speed)
  "Depends on mplayer's -slave mode"
  (interactive "MSpeed: ")
  (process-send-string emms-player-simple-process-name
                       (format "speed_set %s\n" speed)))

(defvar sacha-emms-player-mplayer-speed-increment 0.1)

;;;###autoload
(defun sacha-emms-player-mplayer-speed-up ()
  "Depends on mplayer's -slave mode"
  (interactive)
  (process-send-string emms-player-simple-process-name
                       (format "speed_incr %f\n" sacha-emms-player-mplayer-speed-increment)))
;;;###autoload
(defun sacha-emms-player-mplayer-slow-down ()
  "Depends on mplayer's -slave mode"
  (interactive)
  (process-send-string emms-player-simple-process-name
                       (format "speed_incr %f\n" (- 0 sacha-emms-player-mplayer-speed-increment))))


;; Transcript editing:2 ends here

;; [[file:../Sacha.org::#word-level][Using word-level timing information when editing subtitles or captions in Emacs:1]]
;;;###autoload
(defun sacha-caption-download-srv2 (id)
  (interactive "MID: ")
  (require 'subed-word-data)
  (when (string-match "v=\\([^&]+\\)" id) (setq id (match-string 1 id)))
  (let ((default-directory "/tmp"))
    (call-process "yt-dlp" nil nil nil "--write-auto-sub" "--write-sub" "--no-warnings" "--sub-lang" "en" "--skip-download" "--sub-format" "srv2"
                  (concat "https://youtu.be/" id))
    (subed-word-data-load-from-file (sacha-latest-file "/tmp" "\\.srv2\\'"))))
;; Using word-level timing information when editing subtitles or captions in Emacs:1 ends here

;; [[file:../Sacha.org::#word-level][Using word-level timing information when editing subtitles or captions in Emacs:2]]
;;;###autoload
(defun sacha-caption-fix-common-errors (data)
  (mapc (lambda (o)
          (mapc (lambda (e)
                  (when (string-match (concat "\\<" (regexp-opt (if (listp e) (seq-remove (lambda (s) (string= "" s)) e)
                                                                  (list e)))
                                              "\\>")
                                      (alist-get 'text o))
                    (map-put! o 'text (replace-match (car (if (listp e) e (list e))) t t (alist-get 'text o)))))
                sacha-subed-common-edits))
        data))
;; Using word-level timing information when editing subtitles or captions in Emacs:2 ends here

;; [[file:../Sacha.org::#word-level][Using word-level timing information when editing subtitles or captions in Emacs:5]]
(defvar sacha-caption-breaks
  '("the" "this" "we" "we're" "I" "finally" "but" "and" "when")
  "List of words to try to break at.")
;;;###autoload
(defun sacha-caption-make-groups (list &optional threshold)
  (let (result
        current-item
        done
        (current-length 0)
        (limit (or threshold 70))
        (lower-limit 30)
        (break-regexp (concat "\\<" (regexp-opt sacha-caption-breaks) "\\>")))
    (while list
      (cond
       ((null (car list)))
       ((string-match "^\n*$" (alist-get 'text (car list)))
        (push (cons '(text . " ") (car list)) current-item)
        (setq current-length (1+ current-length)))
       ((< (+ current-length (length (alist-get 'text (car list)))) limit)
        (setq current-item (cons (car list) current-item)
              current-length (+ current-length (length (alist-get 'text (car list))) 1)))
       (t (setq done nil)
          (while (not done)
          (cond
           ((< current-length lower-limit)
            (setq done t))
           ((and (string-match break-regexp (alist-get 'text (car current-item)))
                 (not (string-match break-regexp (alist-get 'text (cadr current-item)))))
            (setq current-length (- current-length (length (alist-get 'text (car current-item)))))
            (push (pop current-item) list)
            (setq done t))
           (t
            (setq current-length (- current-length (length (alist-get 'text (car current-item)))))
            (push (pop current-item) list))))
          (push nil list)
          (setq result (cons (reverse current-item) result) current-item nil current-length 0)))
      (setq list (cdr list)))
    (reverse result)))

;;;###autoload
(defun sacha-caption-format-as-subtitle (list &optional word-timing)
  "Turn a LIST of the form (((start . ms) (end . ms) (text . s)) ...) into VTT.
If WORD-TIMING is non-nil, include word-level timestamps."
  (format "%s --> %s\n%s\n\n"
          (subed-vtt--msecs-to-timestamp (alist-get 'start (car list)))
          (subed-vtt--msecs-to-timestamp (alist-get 'end (car (last list))))
          (s-trim (mapconcat (lambda (entry)
                               (if word-timing
                                   (format " <%s>%s"
                                           (subed-vtt--msecs-to-timestamp (alist-get 'start entry))
                                           (string-trim (alist-get 'text entry)))
                                 (alist-get 'text entry)))
                             list ""))))

;;;###autoload
(defun sacha-caption-to-vtt (&optional data)
  (interactive)
  (with-temp-file "captions.vtt"
    (insert "WEBVTT\n\n"
            (mapconcat
             (lambda (entry) (sacha-caption-format-as-subtitle entry))
             (sacha-caption-make-groups
              (or data (sacha-caption-fix-common-errors subed-word-data--cache)))
             ""))))
;; Using word-level timing information when editing subtitles or captions in Emacs:5 ends here

;; [[file:../Sacha.org::#showing-captions][Showing captions:1]]
;;;###autoload
(defun sacha-caption-show (url)
  (interactive (list
                (let ((link (and (derived-mode-p 'org-mode)
                                 (org-element-context))))
                  (if (and link
                           (eq (org-element-type link) 'link))
                      (read-string (format "URL (%s): " (org-element-property :raw-link link)) nil nil
                                   (org-element-property :raw-link link))
                    (read-string "URL: ")))))
  (when (and (listp url) (org-element-property :raw-link url)) (setq url (org-element-property :raw-link url)))
  (delete-other-windows)
  (split-window-right)
	(if (string-match "http" url)
			(with-current-buffer-window "*Captions*"
					'display-buffer-same-window
					nil
				(org-mode)
				(save-excursion
					(sacha-org-insert-youtube-video-with-transcript url)))
		(unless (file-exists-p (concat (file-name-sans-extension url) ".vtt"))
			(sacha-deepgram-recognize-audio url))
		(find-file (concat (file-name-sans-extension url) ".vtt"))))
;; Showing captions:1 ends here

;; [[file:../Sacha.org::#org-youtube-captions][Org Mode: Insert YouTube video with separate captions:1]]
;;;###autoload
(defun sacha-msecs-to-timestamp (msecs)
  "Convert MSECS to string in the format HH:MM:SS.MS."
  (concat (format-seconds "%02h:%02m:%02s" (/ msecs 1000))
          "." (format "%03d" (mod msecs 1000))))
;; Org Mode: Insert YouTube video with separate captions:1 ends here

;; [[file:../Sacha.org::#youtube-shorts][Preparing to record YouTube shorts:1]]
;;;###autoload
(defun sacha-youtube-prepare-for-shorts ()
	(interactive)
	(keycast-header-line-mode 1)
	(modus-themes-load-theme (car modus-themes-to-toggle))
	(setq compile-media-output-video-width 1080
				compile-media-output-video-height 1920
				compile-media-output-video-fps 30)
	(shell-command "wmctrl -r :ACTIVE: -e 0,300,0,554,984"))
;; Preparing to record YouTube shorts:1 ends here

;; [[file:../Sacha.org::#youtube-shorts][Preparing to record YouTube shorts:2]]
;;;###autoload
(defun sacha-prepare-for-landscape ()
	(let ((width 6) (height 9))
		(setq compile-media-output-video-width 1080
					compile-media-output-video-height 1920
					compile-media-output-video-fps 30)
	(shell-command "wmctrl -r :ACTIVE: -e 0,300,0,554,984")
	))
;; Preparing to record YouTube shorts:2 ends here

;; [[file:../Sacha.org::#simple-streaming][Simple streaming with FFmpeg:4]]
(defvar sacha-stream-process nil)
(defvar sacha-stream-type nil)
(defvar sacha-stream-offset-seconds 2 "Number of seconds to offset timestamps.")
(defvar sacha-stream-start-time nil)

;;;###autoload
(defun sacha-stream-toggle ()
	(interactive)
	(if (process-live-p sacha-stream-process)
			(sacha-stream-stop)
		(sacha-stream-start)))

;;;###autoload
(defun sacha-recording-toggle ()
	(interactive)
	(if (process-live-p sacha-stream-process)
			(sacha-recording-stop)
		(sacha-recording-start)))

;;;###autoload
(defun sacha-stream-start ()
	(interactive)
	(unless (process-live-p sacha-stream-process)
		(unless (getenv "YOUTUBE_KEY")
			(setenv "YOUTUBE_KEY" (auth-info-password (auth-source-search :host "https://studio.youtube.com"))))
		(setq sacha-stream-type 'stream)
		(setq sacha-stream-start-time (current-time))
		(setq sacha-stream-process (start-process "ffmpeg" (get-buffer-create "*stream-ffmpeg*")
																					 "bash" (expand-file-name "~/bin/stream-laptop")))
		(message "Streaming.")))

;;;###autoload
(defun sacha-recording-start ()
	(interactive)
	(unless (process-live-p sacha-stream-process)
		(setq sacha-stream-type 'record)
		(setq sacha-stream-start-time (current-time))
		(setq sacha-stream-process (start-process "ffmpeg" (get-buffer-create "*stream-ffmpeg*")
																					 "bash" (expand-file-name "~/bin/record-laptop")))
		(message "Recording.")))

;;;###autoload
(defun sacha-stream-stop ()
	(interactive)
	(when (process-live-p sacha-stream-process)
		(setq sacha-stream-type nil)
		(setq sacha-stream-start-time nil)
		(stop-process sacha-stream-process)
		(kill-process sacha-stream-process)))

(defalias 'sacha-recording-stop #'sacha-stream-stop)
;;;###autoload
(defun sacha-recordings-dired ()
	(interactive)
	(dired sacha-recordings-dir "-lt"))
;; Simple streaming with FFmpeg:4 ends here

;; [[file:../Sacha.org::#simple-streaming][Simple streaming with FFmpeg:5]]
;;;###autoload
(defun sacha-stream-insert-timestamp ()
	(interactive)
	(when sacha-stream-start-time
		(let ((time (format-seconds "%.2h:%z%.2m:%.2s"
																(- (time-to-seconds (current-time))
																	 (time-to-seconds sacha-stream-start-time)
																	 (if (eq sacha-stream-type 'stream) sacha-stream-offset-seconds 0)))))
			(insert (org-link-make-string
							 (concat "video:" (sacha-latest-file "~/recordings" "flv")
											 ":" time)
							 time)
							" "))))

;;;###autoload
(defun sacha-stream-set-recording-file ()
	(interactive)
	(org-entry-put (point) "RECORDING"
								 (sacha-latest-file "~/recordings" "flv")))
;; Simple streaming with FFmpeg:5 ends here

;; [[file:../Sacha.org::#controlling-sacha-stream-audio-from-emacs-background-music-typing-sounds-and-push-to-talk][Controlling my stream audio from Emacs: background music, typing sounds, and push to talk:2]]
(defvar sacha-background-music-process nil "Process for playing background music")
;;;###autoload
(defun sacha-stream-toggle-background-music (&optional enable)
  (interactive)
  (if (or sacha-background-music-process
          (and (numberp enable) (< enable 0)))
      (progn
        (when (process-live-p sacha-background-music-process)
          (kill-process sacha-background-music-process))
        (setq sacha-background-music-process nil))
    (let ((files (directory-files "~/proj/music" t "mid\\'")))
      (setq sacha-background-music-process
            (apply
             'start-process
             "*Music*"
             nil
             (append (list "timidity" "-idlr" "--volume=10") files))))))
;; Controlling my stream audio from Emacs: background music, typing sounds, and push to talk:2 ends here

;; [[file:../Sacha.org::#controlling-sacha-stream-audio-from-emacs-background-music-typing-sounds-and-push-to-talk][Controlling my stream audio from Emacs: background music, typing sounds, and push to talk:5]]
;;;###autoload
(defun sacha-pacmd-set-device (regexp status)
  (with-current-buffer (get-buffer-create "*pacmd*")
    (erase-buffer)
    (shell-command "pacmd list-sources" (current-buffer))
    (goto-char (point-max))
    (let (results)
      (while (re-search-backward regexp nil t)
        (when (re-search-backward "index: \\([[:digit:]]+\\)" nil t)
          (setq results (cons (match-string 1) results))
          (shell-command-to-string (format "pacmd set-source-mute %s %d"
                                           (match-string 1)
                                           (if (equal status 'on) 0 1)))))
      results)))

(defvar sacha-mic-p nil "Non-nil means microphone is on")

;;;###autoload
(defun sacha-mic-off ()
  (interactive)
  (sacha-pacmd-set-device "Yeti" 'off)
  (sacha-pacmd-set-device "Internal Microphone" 'off)
  (setq sacha-mic-p nil))
;;;###autoload
(defun sacha-mic-on ()
  (interactive)
  (sacha-pacmd-set-device "Yeti" 'on)
  (sacha-pacmd-set-device "Internal Microphone" 'on)
  (setq sacha-mic-p t))
;;;###autoload
(defun sacha-mic-toggle ()
  (interactive)
  (if sacha-mic-p (sacha-mic-off) (sacha-mic-on)))

(defvar sacha-push-to-talk-mute-timer nil "Timer to mute things again.")
(defvar sacha-push-to-talk-last-time nil "Last time sacha-push-to-talk was run")
(defvar sacha-push-to-talk-threshold 0.5 "Number of seconds")

;;;###autoload
(defun sacha-push-to-talk-mute ()
  (interactive)
  (message "Muting.")
  (sacha-mic-off)
  (force-mode-line-update)
  (when obs-websocket-recording-p (sacha-obs-websocket-add-caption "[Microphone off]")))

;;;###autoload
(defun sacha-push-to-talk ()
  "Tap to toggle microphone on and off, or repeat the command to make it push to talk."
  (interactive)
  (cond
   ((null sacha-mic-p) ;; It's off, so turn it on
    (when (timerp sacha-push-to-talk-mute-timer)
      (cancel-timer sacha-push-to-talk-mute-timer))
    (sacha-mic-on)
    (when obs-websocket-recording-p (sacha-obs-websocket-add-caption "[Microphone on]"))
    (setq sacha-push-to-talk-last-time (current-time)))
   ((timerp sacha-push-to-talk-mute-timer) ;; Push-to-talk mode
    (cancel-timer sacha-push-to-talk-mute-timer)
    (setq sacha-push-to-talk-mute-timer
          (run-at-time sacha-push-to-talk-threshold nil #'sacha-push-to-talk-mute)))
   ;; Might be push to talk, if we're within the key repeating time
   ((< (- (time-to-seconds (current-time)) (time-to-seconds sacha-push-to-talk-last-time))
       sacha-push-to-talk-threshold)
    (setq sacha-push-to-talk-mute-timer
          (run-at-time sacha-push-to-talk-threshold nil #'sacha-push-to-talk-mute)))
   ;; It's been a while since I turned the mic on.
   (t (sacha-push-to-talk-mute))))

;(global-set-key (kbd "<f12>") #'sacha-push-to-talk)
;; Controlling my stream audio from Emacs: background music, typing sounds, and push to talk:5 ends here

;; [[file:../Sacha.org::#more-background-music][More background music:1]]
;;;###autoload
(defun sacha-stream-emms-toggle-background ()
	(interactive)
	(unless (emms-playlist-buffer-list)
		(emms-play-directory "~/sync/Phone/music/freepd/"))
	(emms-pause)
	(emms-show))
;; More background music:1 ends here

;; [[file:../Sacha.org::#streaming-stream-message][Stream message:1]]
;;;###autoload
(defun sacha-stream-message (message)
	(interactive "MMessage: ")
	(with-temp-file "~/proj/stream/message.html"
		(insert "<style>body { font-size: large; color: white; font-family: sans-serif; padding: 10px; background-color: black }</style>"
						message))
	(shell-command "scp ~/proj/stream/message.html web:/var/www/yayemacs.com"))
;; Stream message:1 ends here

;; [[file:../Sacha.org::#playing-recordings][Playing recordings:2]]
(defvar sacha-recordings-dir "~/recordings/")
;;;###autoload
(defun sacha-delete-latest-recording ()
	(interactive)
	(delete-file (sacha-latest-file sacha-recordings-dir)))
;;;###autoload
(defun sacha-open-latest-recording ()
	(interactive)
	(find-file (sacha-latest-file sacha-recordings-dir)))
;;;###autoload
(defun sacha-play-latest-recording (&optional arg)
  (interactive "P")
  (let ((latest (sacha-latest-file sacha-recordings-dir)))
    (if (and arg (file-exists-p (sacha-obs-websocket-caption-file latest)))
        (with-current-buffer (find-file-noselect (sacha-obs-websocket-caption-file (sacha-latest-file sacha-recordings-dir)))
          (goto-char (point-min))
          (subed-mpv-find-video latest)
          (pop-to-buffer (current-buffer)))
      (mpv-play (sacha-latest-file sacha-recordings-dir)))))
;;;###autoload
(defun sacha-rename-last-recording ()
  (interactive)
  (let ((latest (sacha-latest-file sacha-recordings-dir))
				(new-name (read-string "New name: " (format-time-string "%Y-%m-%d-"))))
    (rename-file latest
                 (expand-file-name
                  (concat new-name
													(if (and (file-name-extension latest) (null (file-name-extension new-name)))
															(concat "." (file-name-extension latest))
														""))
                  sacha-recordings-dir))))

;;;###autoload
(defun sacha-upload-recording (recording tags)
  (interactive (list (let ((latest (sacha-latest-file sacha-recordings-dir "mkv\\|mp4\\|webm")))
                       (read-file-name "Recording: " sacha-recordings-dir latest t)
                       (read-string "Tags: " "emacs"))))
  (start-process "youtube-upload" nil "youtube-upload" recording "--privacy=unlisted" "--license=creativeCommon"
                 (format
                  "--tags=\"%s\""
                  tags)
                 "--open-link"
                 (format "--title=%s" (shell-quote-argument (file-name-base recording)))
                 (format "--client-secrets=%s" google-video-credentials)))
;; Playing recordings:2 ends here

;; [[file:../Sacha.org::#stream-notes][Stream notes:1]]
;;;###autoload
(defun sacha-org-save-and-tangle-stream-notes ()
  (when (and (buffer-file-name)
						 (string= (expand-file-name (buffer-file-name))
											(expand-file-name "~/proj/stream/index.org")))
		(add-hook 'after-save-hook #'sacha-stream-publish-and-sync-notes nil t)))
;;;###autoload
(defun sacha-stream-publish-and-sync-notes ()
	(interactive)
	(with-current-buffer (find-file "~/proj/stream/index.org")
		(org-html-export-to-html)
		(let ((org-icalendar-timezone "America/Toronto")
					(org-icalendar-date-time-format ":%Y%m%dT%H%M%SZ"))
			(org-icalendar-export-to-ics))
		(shell-command "rsync -aze ssh ./ web:/var/www/yayemacs.com")))
;; Stream notes:1 ends here

;; [[file:../Sacha.org::#streaming-chapters][Chapters:1]]
;;;###autoload
(defun sacha-youtube-copy-chapters ()
	"Call from a VTT file with NOTE comments."
	(interactive)
	(let ((subtitles (subed-subtitle-list)))
		(kill-new
		 (concat (if (elt (car subtitles) 4)
								 ""
							 "0:00 Intro\n")
						 (mapconcat (lambda (o)
													(if (elt o 4)
															(concat (format-seconds "%m:%.2s" (/ (elt o 2) 1000))
																			" "
																			(elt o 4)
																			"\n")
														""))
												subtitles
												"")))))
;; Chapters:1 ends here

;; [[file:../Sacha.org::#speech-to-text][CANCELLED Try continuous streaming and the Google Speech Recognition API:1]]
(defvar sacha-stream-captions-websocket nil)
(defvar sacha-stream-captions-history nil)
(defvar sacha-stream-captions-last-caption nil)
;;;###autoload
(defun sacha-stream-captions-insert () (interactive) (setq sacha-stream-captions-insert (not sacha-stream-captions-insert)))

(define-minor-mode sacha-stream-captions-minor-mode "Toggle the captions server."
  :lighter "CAP"
  :global t)

;;;###autoload
(defun sacha-get-last-n-chars (text limit)
  (if (< (length text) limit)
      text
    (substring text (- (length text) limit))))

;;;###autoload
(defun sacha-stream-captions-on-message (websocket frame)
  (let* ((payload (let ((json-object-type 'plist) (json-array-type 'list)) (json-read-from-string (websocket-frame-payload frame))))
         (type (plist-get payload :type))
         (caption (string-trim (plist-get (car (plist-get (car (plist-get (plist-get payload :stream) :results)) :alternatives)) :transcript))))

    (if (string= type "interim")
        (when (websocket-openp obs-websocket) (obs-websocket-send "SendCaptions" :text (sacha-get-last-n-chars caption 80)))
      (setq sacha-stream-captions-last-caption caption)
      (call-process "notify-send" nil nil nil caption)
      (sacha-obs-websocket-add-caption caption)
      (when sacha-stream-captions-insert (insert caption))
      (setq sacha-stream-captions-history (cons caption sacha-stream-captions-history)))))


;;;###autoload
(defun sacha-stream-captions-edit-last (caption)
  (interactive (list (read-string "Caption: " sacha-stream-captions-last-caption 'sacha-stream-captions-history sacha-stream-captions-last-caption)))
  (when (> (length caption) 0)
    (sacha-obs-websocket-add-caption caption)))
;; CANCELLED Try continuous streaming and the Google Speech Recognition API:1 ends here

;; [[file:../Sacha.org::#animation-for-emacs-chats][Animation for Emacs chats:1]]
;;;###autoload
(defun sacha-animate-emacs-chat ()
  (interactive)
  (text-scale-set 6)
  (erase-buffer)
  (sit-for 3)
  (let ((list '("Emacs Chat: Sacha Chua"
                "interviewed by Bastien Guerry"
                ""
                "July 24, 2013"
                "sachachua.com/emacs-chat"))
        (approx-width 41)
        (approx-height 16)
        row)
    (setq row (/ (- approx-height (length list)) 2))
    (mapcar
     (lambda (x)
       (animate-string x
                       row
                       (/ (- approx-width (length x)) 2))
       (setq row (1+ row)))
     list)))
;; Animation for Emacs chats:1 ends here

(provide 'sacha-multimedia)
;;; sacha-multimedia.el ends here
