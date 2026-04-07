;;; my-multimedia.el ---  -*- lexical-binding: t -*-

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
;; - FFmpeg
;;   https://sachachua.com/dotemacs#multimedia-ffmpeg
;;
;; - Transcript editing
;;   https://sachachua.com/dotemacs#transcript-editing
;;
;; - Using word-level timing information when editing subtitles or captions in Emacs
;;   https://sachachua.com/dotemacs#word-level
;;
;; - Showing captions
;;   https://sachachua.com/dotemacs#showing-captions
;;
;; - Org Mode: Insert YouTube video with separate captions
;;   https://sachachua.com/dotemacs#org-youtube-captions
;;
;; - Preparing to record YouTube shorts
;;   https://sachachua.com/dotemacs#youtube-shorts
;;
;; - Simple streaming with FFmpeg
;;   https://sachachua.com/dotemacs#simple-streaming
;;
;; - Controlling my stream audio from Emacs: background music, typing sounds, and push to talk
;;   https://sachachua.com/dotemacs#controlling-my-stream-audio-from-emacs-background-music-typing-sounds-and-push-to-talk
;;
;; - More background music
;;   https://sachachua.com/dotemacs#more-background-music
;;
;; - Stream message
;;   https://sachachua.com/dotemacs#streaming-stream-message
;;
;; - Playing recordings
;;   https://sachachua.com/dotemacs#playing-recordings
;;
;; - Stream notes
;;   https://sachachua.com/dotemacs#stream-notes
;;
;; - Chapters
;;   https://sachachua.com/dotemacs#streaming-chapters
;;
;; - Try continuous streaming and the Google Speech Recognition API
;;   https://sachachua.com/dotemacs#speech-to-text
;;
;;; Code:



;; [[file:../Sacha.org::#multimedia-ffmpeg][FFmpeg:1]]
;;;###autoload
(defun my-ffmpeg-save-last-frame-as-image (input-file output-image)
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
(defun my-emms-player-mplayer-set-speed (speed)
  "Depends on mplayer's -slave mode"
  (interactive "MSpeed: ")
  (process-send-string emms-player-simple-process-name
                       (format "speed_set %s\n" speed)))

(defvar my-emms-player-mplayer-speed-increment 0.1)

;;;###autoload
(defun my-emms-player-mplayer-speed-up ()
  "Depends on mplayer's -slave mode"
  (interactive)
  (process-send-string emms-player-simple-process-name
                       (format "speed_incr %f\n" my-emms-player-mplayer-speed-increment)))
;;;###autoload
(defun my-emms-player-mplayer-slow-down ()
  "Depends on mplayer's -slave mode"
  (interactive)
  (process-send-string emms-player-simple-process-name
                       (format "speed_incr %f\n" (- 0 my-emms-player-mplayer-speed-increment))))


;; Transcript editing:2 ends here

;; [[file:../Sacha.org::#word-level][Using word-level timing information when editing subtitles or captions in Emacs:1]]
;;;###autoload
(defun my-caption-download-srv2 (id)
  (interactive "MID: ")
  (require 'subed-word-data)
  (when (string-match "v=\\([^&]+\\)" id) (setq id (match-string 1 id)))
  (let ((default-directory "/tmp"))
    (call-process "yt-dlp" nil nil nil "--write-auto-sub" "--write-sub" "--no-warnings" "--sub-lang" "en" "--skip-download" "--sub-format" "srv2"
                  (concat "https://youtu.be/" id))
    (subed-word-data-load-from-file (my-latest-file "/tmp" "\\.srv2\\'"))))
;; Using word-level timing information when editing subtitles or captions in Emacs:1 ends here

;; [[file:../Sacha.org::#word-level][Using word-level timing information when editing subtitles or captions in Emacs:2]]
;;;###autoload
(defun my-caption-fix-common-errors (data)
  (mapc (lambda (o)
          (mapc (lambda (e)
                  (when (string-match (concat "\\<" (regexp-opt (if (listp e) (seq-remove (lambda (s) (string= "" s)) e)
                                                                  (list e)))
                                              "\\>")
                                      (alist-get 'text o))
                    (map-put! o 'text (replace-match (car (if (listp e) e (list e))) t t (alist-get 'text o)))))
                my-subed-common-edits))
        data))
;; Using word-level timing information when editing subtitles or captions in Emacs:2 ends here

;; [[file:../Sacha.org::#word-level][Using word-level timing information when editing subtitles or captions in Emacs:5]]
(defvar my-caption-breaks
  '("the" "this" "we" "we're" "I" "finally" "but" "and" "when")
  "List of words to try to break at.")
;;;###autoload
(defun my-caption-make-groups (list &optional threshold)
  (let (result
        current-item
        done
        (current-length 0)
        (limit (or threshold 70))
        (lower-limit 30)
        (break-regexp (concat "\\<" (regexp-opt my-caption-breaks) "\\>")))
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
(defun my-caption-format-as-subtitle (list &optional word-timing)
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
(defun my-caption-to-vtt (&optional data)
  (interactive)
  (with-temp-file "captions.vtt"
    (insert "WEBVTT\n\n"
            (mapconcat
             (lambda (entry) (my-caption-format-as-subtitle entry))
             (my-caption-make-groups
              (or data (my-caption-fix-common-errors subed-word-data--cache)))
             ""))))
;; Using word-level timing information when editing subtitles or captions in Emacs:5 ends here

;; [[file:../Sacha.org::#showing-captions][Showing captions:1]]
;;;###autoload
(defun my-caption-show (url)
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
					(my-org-insert-youtube-video-with-transcript url)))
		(unless (file-exists-p (concat (file-name-sans-extension url) ".vtt"))
			(my-deepgram-recognize-audio url))
		(find-file (concat (file-name-sans-extension url) ".vtt"))))
;; Showing captions:1 ends here

;; [[file:../Sacha.org::#org-youtube-captions][Org Mode: Insert YouTube video with separate captions:1]]
;;;###autoload
(defun my-msecs-to-timestamp (msecs)
  "Convert MSECS to string in the format HH:MM:SS.MS."
  (concat (format-seconds "%02h:%02m:%02s" (/ msecs 1000))
          "." (format "%03d" (mod msecs 1000))))
;; Org Mode: Insert YouTube video with separate captions:1 ends here

;; [[file:../Sacha.org::#youtube-shorts][Preparing to record YouTube shorts:1]]
;;;###autoload
(defun my-youtube-prepare-for-shorts ()
	(interactive)
	(keycast-header-line-mode 1)
	(modus-themes-load-theme (car modus-themes-to-toggle))
	(setq compile-media-output-video-width 1080
				compile-media-output-video-height 1920
				compile-media-output-video-fps 30)
	(shell-command "wmctrl -r :ACTIVE: -e 0,300,0,554,984"))
;; Preparing to record YouTube shorts:1 ends here

;; [[file:../Sacha.org::#simple-streaming][Simple streaming with FFmpeg:4]]
(defvar my-stream-process nil)
(defvar my-stream-type nil)
(defvar my-stream-offset-seconds 2 "Number of seconds to offset timestamps.")
(defvar my-stream-start-time nil)

;;;###autoload
(defun my-stream-toggle ()
	(interactive)
	(if (process-live-p my-stream-process)
			(my-stream-stop)
		(my-stream-start)))

;;;###autoload
(defun my-recording-toggle ()
	(interactive)
	(if (process-live-p my-stream-process)
			(my-recording-stop)
		(my-recording-start)))

;;;###autoload
(defun my-stream-start ()
	(interactive)
	(unless (process-live-p my-stream-process)
		(unless (getenv "YOUTUBE_KEY")
			(setenv "YOUTUBE_KEY" (auth-info-password (auth-source-search :host "https://studio.youtube.com"))))
		(setq my-stream-type 'stream)
		(setq my-stream-start-time (current-time))
		(setq my-stream-process (start-process "ffmpeg" (get-buffer-create "*stream-ffmpeg*")
																					 "bash" (expand-file-name "~/bin/stream-laptop")))
		(message "Streaming.")))

;;;###autoload
(defun my-recording-start ()
	(interactive)
	(unless (process-live-p my-stream-process)
		(setq my-stream-type 'record)
		(setq my-stream-start-time (current-time))
		(setq my-stream-process (start-process "ffmpeg" (get-buffer-create "*stream-ffmpeg*")
																					 "bash" (expand-file-name "~/bin/record-laptop")))
		(message "Recording.")))

;;;###autoload
(defun my-stream-stop ()
	(interactive)
	(when (process-live-p my-stream-process)
		(setq my-stream-type nil)
		(setq my-stream-start-time nil)
		(stop-process my-stream-process)
		(kill-process my-stream-process)))

(defalias 'my-recording-stop #'my-stream-stop)
;;;###autoload
(defun my-recordings-dired ()
	(interactive)
	(dired my-recordings-dir "-lt"))
;; Simple streaming with FFmpeg:4 ends here

;; [[file:../Sacha.org::#simple-streaming][Simple streaming with FFmpeg:5]]
;;;###autoload
(defun my-stream-insert-timestamp ()
	(interactive)
	(when my-stream-start-time
		(let ((time (format-seconds "%.2h:%z%.2m:%.2s"
																(- (time-to-seconds (current-time))
																	 (time-to-seconds my-stream-start-time)
																	 (if (eq my-stream-type 'stream) my-stream-offset-seconds 0)))))
			(insert (org-link-make-string
							 (concat "video:" (my-latest-file "~/recordings" "flv")
											 ":" time)
							 time)
							" "))))

;;;###autoload
(defun my-stream-set-recording-file ()
	(interactive)
	(org-entry-put (point) "RECORDING"
								 (my-latest-file "~/recordings" "flv")))
;; Simple streaming with FFmpeg:5 ends here

;; [[file:../Sacha.org::#controlling-my-stream-audio-from-emacs-background-music-typing-sounds-and-push-to-talk][Controlling my stream audio from Emacs: background music, typing sounds, and push to talk:2]]
(defvar my-background-music-process nil "Process for playing background music")
;;;###autoload
(defun my-stream-toggle-background-music (&optional enable)
  (interactive)
  (if (or my-background-music-process
          (and (numberp enable) (< enable 0)))
      (progn
        (when (process-live-p my-background-music-process)
          (kill-process my-background-music-process))
        (setq my-background-music-process nil))
    (let ((files (directory-files "~/proj/music" t "mid\\'")))
      (setq my-background-music-process
            (apply
             'start-process
             "*Music*"
             nil
             (append (list "timidity" "-idlr" "--volume=10") files))))))
;; Controlling my stream audio from Emacs: background music, typing sounds, and push to talk:2 ends here

;; [[file:../Sacha.org::#controlling-my-stream-audio-from-emacs-background-music-typing-sounds-and-push-to-talk][Controlling my stream audio from Emacs: background music, typing sounds, and push to talk:5]]
;;;###autoload
(defun my-pacmd-set-device (regexp status)
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

(defvar my-mic-p nil "Non-nil means microphone is on")

;;;###autoload
(defun my-mic-off ()
  (interactive)
  (my-pacmd-set-device "Yeti" 'off)
  (my-pacmd-set-device "Internal Microphone" 'off)
  (setq my-mic-p nil))
;;;###autoload
(defun my-mic-on ()
  (interactive)
  (my-pacmd-set-device "Yeti" 'on)
  (my-pacmd-set-device "Internal Microphone" 'on)
  (setq my-mic-p t))
;;;###autoload
(defun my-mic-toggle ()
  (interactive)
  (if my-mic-p (my-mic-off) (my-mic-on)))

(defvar my-push-to-talk-mute-timer nil "Timer to mute things again.")
(defvar my-push-to-talk-last-time nil "Last time my-push-to-talk was run")
(defvar my-push-to-talk-threshold 0.5 "Number of seconds")

;;;###autoload
(defun my-push-to-talk-mute ()
  (interactive)
  (message "Muting.")
  (my-mic-off)
  (force-mode-line-update)
  (when obs-websocket-recording-p (my-obs-websocket-add-caption "[Microphone off]")))

;;;###autoload
(defun my-push-to-talk ()
  "Tap to toggle microphone on and off, or repeat the command to make it push to talk."
  (interactive)
  (cond
   ((null my-mic-p) ;; It's off, so turn it on
    (when (timerp my-push-to-talk-mute-timer)
      (cancel-timer my-push-to-talk-mute-timer))
    (my-mic-on)
    (when obs-websocket-recording-p (my-obs-websocket-add-caption "[Microphone on]"))
    (setq my-push-to-talk-last-time (current-time)))
   ((timerp my-push-to-talk-mute-timer) ;; Push-to-talk mode
    (cancel-timer my-push-to-talk-mute-timer)
    (setq my-push-to-talk-mute-timer
          (run-at-time my-push-to-talk-threshold nil #'my-push-to-talk-mute)))
   ;; Might be push to talk, if we're within the key repeating time
   ((< (- (time-to-seconds (current-time)) (time-to-seconds my-push-to-talk-last-time))
       my-push-to-talk-threshold)
    (setq my-push-to-talk-mute-timer
          (run-at-time my-push-to-talk-threshold nil #'my-push-to-talk-mute)))
   ;; It's been a while since I turned the mic on.
   (t (my-push-to-talk-mute))))

;(global-set-key (kbd "<f12>") #'my-push-to-talk)
;; Controlling my stream audio from Emacs: background music, typing sounds, and push to talk:5 ends here

;; [[file:../Sacha.org::#more-background-music][More background music:1]]
;;;###autoload
(defun my-stream-emms-toggle-background ()
	(interactive)
	(unless (emms-playlist-buffer-list)
		(emms-play-directory "~/sync/Phone/music/freepd/"))
	(emms-pause)
	(emms-show))
;; More background music:1 ends here

;; [[file:../Sacha.org::#streaming-stream-message][Stream message:1]]
;;;###autoload
(defun my-stream-message (message)
	(interactive "MMessage: ")
	(with-temp-file "~/proj/stream/message.html"
		(insert "<style>body { font-size: large; color: white; font-family: sans-serif; padding: 10px; background-color: black }</style>"
						message))
	(shell-command "scp ~/proj/stream/message.html web:/var/www/yayemacs.com"))
;; Stream message:1 ends here

;; [[file:../Sacha.org::#playing-recordings][Playing recordings:2]]
(defvar my-recordings-dir "~/recordings/")
;;;###autoload
(defun my-delete-latest-recording ()
	(interactive)
	(delete-file (my-latest-file my-recordings-dir)))
;;;###autoload
(defun my-open-latest-recording ()
	(interactive)
	(find-file (my-latest-file my-recordings-dir)))
;;;###autoload
(defun my-play-latest-recording (&optional arg)
  (interactive "P")
  (let ((latest (my-latest-file my-recordings-dir)))
    (if (and arg (file-exists-p (my-obs-websocket-caption-file latest)))
        (with-current-buffer (find-file-noselect (my-obs-websocket-caption-file (my-latest-file my-recordings-dir)))
          (goto-char (point-min))
          (subed-mpv-find-video latest)
          (pop-to-buffer (current-buffer)))
      (mpv-play (my-latest-file my-recordings-dir)))))
;;;###autoload
(defun my-rename-last-recording ()
  (interactive)
  (let ((latest (my-latest-file my-recordings-dir))
				(new-name (read-string "New name: " (format-time-string "%Y-%m-%d-"))))
    (rename-file latest
                 (expand-file-name
                  (concat new-name
													(if (and (file-name-extension latest) (null (file-name-extension new-name)))
															(concat "." (file-name-extension latest))
														""))
                  my-recordings-dir))))

;;;###autoload
(defun my-upload-recording (recording tags)
  (interactive (list (let ((latest (my-latest-file my-recordings-dir "mkv\\|mp4\\|webm")))
                       (read-file-name "Recording: " my-recordings-dir latest t)
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
(defun my-org-save-and-tangle-stream-notes ()
  (when (and (buffer-file-name)
						 (string= (expand-file-name (buffer-file-name))
											(expand-file-name "~/proj/stream/index.org")))
		(add-hook 'after-save-hook #'my-stream-publish-and-sync-notes nil t)))
;;;###autoload
(defun my-stream-publish-and-sync-notes ()
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
(defun my-youtube-copy-chapters ()
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

;; [[file:../Sacha.org::#speech-to-text][Try continuous streaming and the Google Speech Recognition API:1]]
(defvar my-stream-captions-websocket nil)
(defvar my-stream-captions-history nil)
(defvar my-stream-captions-last-caption nil)
;;;###autoload
(defun my-stream-captions-insert () (interactive) (setq my-stream-captions-insert (not my-stream-captions-insert)))

(define-minor-mode my-stream-captions-minor-mode "Toggle the captions server."
  :lighter "CAP"
  :global t)

;;;###autoload
(defun my-get-last-n-chars (text limit)
  (if (< (length text) limit)
      text
    (substring text (- (length text) limit))))

;;;###autoload
(defun my-stream-captions-on-message (websocket frame)
  (let* ((payload (let ((json-object-type 'plist) (json-array-type 'list)) (json-read-from-string (websocket-frame-payload frame))))
         (type (plist-get payload :type))
         (caption (string-trim (plist-get (car (plist-get (car (plist-get (plist-get payload :stream) :results)) :alternatives)) :transcript))))

    (if (string= type "interim")
        (when (websocket-openp obs-websocket) (obs-websocket-send "SendCaptions" :text (my-get-last-n-chars caption 80)))
      (setq my-stream-captions-last-caption caption)
      (call-process "notify-send" nil nil nil caption)
      (my-obs-websocket-add-caption caption)
      (when my-stream-captions-insert (insert caption))
      (setq my-stream-captions-history (cons caption my-stream-captions-history)))))


;;;###autoload
(defun my-stream-captions-edit-last (caption)
  (interactive (list (read-string "Caption: " my-stream-captions-last-caption 'my-stream-captions-history my-stream-captions-last-caption)))
  (when (> (length caption) 0)
    (my-obs-websocket-add-caption caption)))
;; Try continuous streaming and the Google Speech Recognition API:1 ends here

(provide 'my-multimedia)
;;; my-multimedia.el ends here
