;;; my-speech-input.el ---  -*- lexical-binding: t -*-

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
;; - Using whisper.el to convert speech to text and save it to the currently clocked task in Org Mode or elsewhere
;;   https://sachachua.com/dotemacs#multimedia-whisper
;;
;; - Emacs and whisper.el: Trying out different speech-to-text backends and models
;;   https://sachachua.com/dotemacs#writing-and-editing-speech-recognition-emacs-and-whisper-el-trying-out-different-speech-to-text-backends-and-models
;;
;; - Queuing multiple transcriptions with whisper.el speech recognition
;;   https://sachachua.com/dotemacs#writing-and-editing-speech-recognition-queue-multiple-transcriptions-with-whisper-el-speech-recognition
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
;; - Using speech recognition for on-the-fly translations in Emacs and faking in-buffer completion for the results
;;   https://sachachua.com/dotemacs#writing-and-editing-speech-recognition-using-speech-recognition-for-translations-in-emacs-and-faking-in-buffer-completion-for-the-results
;;
;;; Code:



;; [[file:../Sacha.org::#multimedia-whisper][Using whisper.el to convert speech to text and save it to the currently clocked task in Org Mode or elsewhere:2]]
(defvar my-whisper-org-reminder-template "t")

;;;###autoload
(defun my-whisper-org-process-reminder ()
  (let ((text (buffer-string))
        reminder)
    (when (string-match "computer[,\.]? reminds? me to \\(.+\\)" text)
      (setq reminder (match-string 1 text))
      (save-window-excursion
        (with-current-buffer (if (markerp whisper--marker) (marker-buffer whisper--marker) (current-buffer))
          (when (markerp whisper--marker) (goto-char whisper--marker))
          (org-capture nil my-whisper-org-reminder-template)
          (insert reminder)
          (org-capture-finalize)))
      (erase-buffer))))

;; Using whisper.el to convert speech to text and save it to the currently clocked task in Org Mode or elsewhere:2 ends here

;; [[file:../Sacha.org::#multimedia-whisper][Using whisper.el to convert speech to text and save it to the currently clocked task in Org Mode or elsewhere:4]]
(defvar my-whisper-dir "~/recordings/whisper/")
;;;###autoload
(defun my-whisper-set-temp-filename ()
  (setq whisper--temp-file (expand-file-name
                            (format-time-string "%Y-%m-%d-%H-%M-%S.wav")
                            my-whisper-dir)))

;; Using whisper.el to convert speech to text and save it to the currently clocked task in Org Mode or elsewhere:4 ends here

;; [[file:../Sacha.org::#multimedia-whisper][Using whisper.el to convert speech to text and save it to the currently clocked task in Org Mode or elsewhere:6]]
;;;###autoload
(defun my-whisper-replay (&optional file)
  "Replay the last temporary recording."
  (interactive (list
                (when current-prefix-arg
                  (read-file-name "File: " my-whisper-dir))))
  (setq whisper--temp-file (or file whisper--temp-file))
  (mpv-play whisper--temp-file))

;;;###autoload
(defun my-whisper-insert-retry (&optional file)
  (interactive (list
                (when current-prefix-arg
                  (read-file-name "File: " my-whisper-dir))))
  (whisper--cleanup-transcription)
  (setq whisper--marker (point-marker)
        whisper--temp-file (or file whisper--temp-file))
  (whisper--transcribe-audio))
;; Using whisper.el to convert speech to text and save it to the currently clocked task in Org Mode or elsewhere:6 ends here

;; [[file:../Sacha.org::#multimedia-whisper][Using whisper.el to convert speech to text and save it to the currently clocked task in Org Mode or elsewhere:8]]
;;;###autoload
(defun my-whisper-reset (text)
  (setq my-whisper-skip-annotation nil)
  (remove-hook 'whisper-insert-text-at-point #'my-whisper-org-save-to-clocked-task)
  text)
;; Using whisper.el to convert speech to text and save it to the currently clocked task in Org Mode or elsewhere:8 ends here

;; [[file:../Sacha.org::#multimedia-whisper][Using whisper.el to convert speech to text and save it to the currently clocked task in Org Mode or elsewhere:10]]
(defvar my-whisper-last-annotation nil "Last annotation so we can skip duplicates.")
(defvar my-whisper-skip-annotation nil)
(defvar my-whisper-target-markers nil "List of markers to send text to.")

;;;###autoload
(defun my-whisper-insert (text)
  (let ((markers
         (cond
          ((null my-whisper-target-markers)
           (list whisper--marker)) ; current point where whisper was started
          ((listp my-whisper-target-markers)
           my-whisper-target-markers)
          ((markerp my-whisper-target-markers)
           (list my-whisper-target-markers))))
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
      (when my-whisper-target-markers
        (goto-char orig-point))
      nil)))
;; Using whisper.el to convert speech to text and save it to the currently clocked task in Org Mode or elsewhere:10 ends here

;; [[file:../Sacha.org::my-whisper-maybe-type][my-whisper-maybe-type]]
;;;###autoload
(defun my-whisper-maybe-type (text)
  "If Emacs is not the focused app, simulate typing TEXT.
Add this function to `whisper-insert-text-at-point'."
  (when text
    (if (frame-focus-state)
        text
      (make-process :name "xdotool" :command
                    (list "xdotool" "type"
                          text))
      nil)))
;; my-whisper-maybe-type ends here

;; [[file:../Sacha.org::#multimedia-whisper][Using whisper.el to convert speech to text and save it to the currently clocked task in Org Mode or elsewhere:12]]
;;;###autoload
(defun my-whisper-clear-markers ()
  (interactive)
  (setq my-whisper-target-markers nil))

;;;###autoload
(defun my-whisper-use-current-point (&optional add)
  (interactive (list current-prefix-arg))
  (if add
      (push (point-marker) my-whisper-target-markers)
    (setq my-whisper-target-markers (list (point-marker)))))

;;;###autoload
(defun my-whisper-run-at-point (&optional add)
  (interactive (list current-prefix-arg))
  (my-whisper-clear-markers)
  (whisper-run))

;; Using whisper.el to convert speech to text and save it to the currently clocked task in Org Mode or elsewhere:12 ends here

;; [[file:../Sacha.org::#multimedia-whisper][Using whisper.el to convert speech to text and save it to the currently clocked task in Org Mode or elsewhere:14]]
(defvar my-whisper-notes "~/sync/stream/narration.org")
;;;###autoload
(defun my-whisper-save-to-file (text)
  (when text
    (let ((link (org-store-link nil)))
      (with-current-buffer (find-file-noselect my-whisper-notes)
        (goto-char (point-max))
        (insert "\n\n" (format-time-string "%H:%M ") text "\n" (if link (concat link "\n") ""))
        (save-buffer)
        (run-at-time 0.5 nil (lambda (text) (message "Saved to file: %s" text)) text)))
    text))
;; Using whisper.el to convert speech to text and save it to the currently clocked task in Org Mode or elsewhere:14 ends here

;; [[file:../Sacha.org::#multimedia-whisper][Using whisper.el to convert speech to text and save it to the currently clocked task in Org Mode or elsewhere:15]]
;;;###autoload
(defun my-whisper-redo ()
  (interactive)
  (setq whisper--marker (point-marker))
  (whisper--transcribe-audio))
;; Using whisper.el to convert speech to text and save it to the currently clocked task in Org Mode or elsewhere:15 ends here

;; [[file:../Sacha.org::#writing-and-editing-speech-recognition-emacs-and-whisper-el-trying-out-different-speech-to-text-backends-and-models][Emacs and whisper.el: Trying out different speech-to-text backends and models:1]]
(defvar my-whisper-url-format "http://%s:%d/transcribe")
;;;###autoload
(defun my-whisper--transcribe-via-local-server ()
  "Transcribe audio using the local whisper server."
  (message "[-] Transcribing via local server")
  (whisper--setup-mode-line :show 'transcribing)
  (whisper--ensure-server)
  (setq whisper--transcribing-process
        (whisper--process-curl-request
         (format my-whisper-url-format whisper-server-host whisper-server-port)
         (list "Content-Type: multipart/form-data")
         (list (concat "file=@" whisper--temp-file)
               "temperature=0.0"
               "temperature_inc=0.2"
               "response_format=json"
               (concat "model=" whisper-model)
               (concat "language=" whisper-language)))))
;;;###autoload
(defun my-whisper--check-model-consistency () t)
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
  (setq speech-input-model model-name))
;; Emacs and whisper.el: Trying out different speech-to-text backends and models:6 ends here

;; [[file:../Sacha.org::#writing-and-editing-speech-recognition-queue-multiple-transcriptions-with-whisper-el-speech-recognition][Queuing multiple transcriptions with whisper.el speech recognition:1]]
(defvar my-whisper--queue nil)
;;;###autoload
(defun my-whisper-continue (&optional arg)
  "Send what we've got so far for transcription and then continue recording.
Call with \\[universal-argument] to signal that we can stop."
  (interactive "P")
  (require 'whisper)
  (if arg
      (my-whisper-done)
    (setq whisper--marker (point-marker) whisper--point-buffer (current-buffer))
    (when (process-live-p whisper--recording-process)
      ;; queue only if the last one is not asking for the same file
      (unless
          (string=
           (plist-get
            (car
             (last my-whisper--queue))
            :file)
           whisper--temp-file)
        (add-to-list
         'my-whisper--queue
         (list :file whisper--temp-file
               :buffer
               (format "*result: %s*" (file-name-base whisper--temp-file)))
         t))
      ;; Remove the sentinel; handle results ourselves
      (set-process-sentinel whisper--recording-process
                            (lambda (process event)
                              (my-whisper-process-queue)))
      (interrupt-process whisper--recording-process))
    (run-hooks 'whisper-before-transcription-hook)
    (whisper--setup-mode-line :show 'recording)
    (whisper--record-audio)))

;;;###autoload
(defun my-whisper-discard ()
 "Ignore the previous recording."
  (interactive)
  (when (process-live-p whisper--recording-process)
    ;; Remove the sentinel; handle results ourselves
    (set-process-sentinel whisper--recording-process
                          (lambda (process event)
                            (when (file-exists-p whisper--temp-file)
                              (delete-file whisper--temp-file))
                            (my-whisper-process-queue)))
    (interrupt-process whisper--recording-process)))

;;;###autoload
(defun my-whisper-discard-and-continue ()
 "Ignore the previous recording and continue."
  (interactive)
  (if (process-live-p whisper--recording-process)
      (progn
        ;; Remove the sentinel; handle results ourselves
        (set-process-sentinel whisper--recording-process
                              (lambda (process event)
                                (my-whisper-process-queue)
                                (my-whisper-continue)))
        (interrupt-process whisper--recording-process))
    (my-whisper-continue)))

;;;###autoload
(defun my-whisper-done ()
  (interactive)
  (when (process-live-p whisper--recording-process)
    (add-to-list
     'my-whisper--queue
     (list :file whisper--temp-file
           :buffer
           (format "*result: %s*" (file-name-base whisper--temp-file)))
     t)
    ;; Remove the sentinel; handle results ourselves
    (set-process-sentinel whisper--recording-process
                          (lambda (process event)
                            (my-whisper-process-queue)))
    (whisper--setup-mode-line :hide 'recording)
    (interrupt-process whisper--recording-process)))

;;;###autoload
(defun my-whisper-process-queue-result ()
  "Process the first part of the queue that already has results."
  (while (plist-get (car my-whisper--queue) :results)
    (let ((o (pop my-whisper--queue)))
      (unless my-whisper-target-markers
        (setq whisper--marker (point-marker)
              whisper--point-buffer (current-buffer)))
      (with-current-buffer (plist-get o :buffer)
        (erase-buffer)
        (insert (plist-get o :results)))
      ;; Only works with my fork: https://github.com/sachac/whisper.el/tree/whisper-insert-text-at-point-function
      (whisper--handle-transcription-output nil (plist-get o :buffer)))))

;;;###autoload
(defun my-whisper-process-queue ()
  (let (o)
    (while (setq o (seq-find (lambda (o) (and (plist-get o :file)
                                              (not (plist-get o :process))
                                              (not (plist-get o :results))))
                             my-whisper--queue))
      (let* ((headers (list "Content-Type: multipart/form-data"))
             (params (list (concat "file=@"
                                   (plist-get o :file))
                           "temperature=0.0"
                           "temperature_inc=0.2"
                           "response_format=json"
                           (concat "model=" whisper-model)
                           (concat "language=" whisper-language)))
             (url (format my-whisper-url-format whisper-server-host whisper-server-port))
             (command `("curl" "-s"
                        ,url
                        ,@(mapcan (lambda (h) (list "-H" h)) headers)
                        ,@(mapcan (lambda (p) (list "-F" p)) params))))
        (with-current-buffer (get-buffer-create (plist-get o :buffer))
          (erase-buffer))
        (plist-put
         o :process
         (make-process
          :name "whisper-curl"
          :command command
          :buffer (plist-get o :buffer)
          :coding 'utf-8
          :sentinel
          (lambda (process event)
            (with-current-buffer (process-buffer process)
              (let ((current my-whisper--queue-item))
                (when (and (get-buffer (plist-get current :buffer))
                           (string-equal "finished\n" event))
                  (with-current-buffer (plist-get current :buffer)
                    (goto-char (point-min))
                    (plist-put current :results
                               (or
                                (condition-case nil
                                    (gethash "text" (json-parse-buffer))
                                  (error ""))
                                "(error)"))))))
            (my-whisper-process-queue-result))))
        (plist-put o :command (string-join command " "))
        (with-current-buffer (process-buffer (plist-get o :process))
          (setq-local my-whisper--queue-item o))))))
(defvar-local my-whisper--queue-item nil)

;;;###autoload
(defun my-whisper-reprocess-queue ()
  (interactive)
  (setq whisper--marker (point-marker) whisper--point-buffer (current-buffer))
  (mapc (lambda (o)
          (when (process-live-p (plist-get o :process))
            (kill-process (plist-get o :process)))
          (when (get-buffer (plist-get o :buffer))
            (kill-buffer (plist-get o :buffer)))
          (plist-put o :process nil)
          (plist-put o :results nil))
        my-whisper--queue)
  (my-whisper-process-queue))

;;;###autoload
(defun my-whisper-clear-queue ()
  (interactive)
  (mapc (lambda (o)
          (when (process-live-p (plist-get o :process))
            (kill-process (plist-get o :process)))
          (when (get-buffer (plist-get o :buffer))
            (kill-buffer (plist-get o :buffer)))
          (plist-put o :process nil)
          (plist-put o :results nil))
        my-whisper--queue)
  (setq my-whisper--queue nil))

(defvar-keymap my-whisper-simulated-continuous-mode-map
  :doc "Keymap for my-minor-mode."
  "S-<f2>" #'my-whisper-continue
  )
(define-key my-whisper-simulated-continuous-mode-map [remap whisper-run] #'my-whisper-continue)

(define-minor-mode my-whisper-simulated-continuous-mode
  "Simulate continuous speech recognition by queuing."
  :lighter "W"
  (if my-whisper-simulated-continuous-mode
      (message "Start speaking...")
    (message "All done.")
    (my-whisper-done)))

;; Queuing multiple transcriptions with whisper.el speech recognition:1 ends here

;; [[file:../Sacha.org::#writing-and-editing-speech-recognition-using-silero-voice-activity-detection-to-automatically-queue-multiple-transcriptions-with-natrys-whisper-el][Using Silero voice activity detection to automatically queue multiple transcriptions with natrys/whisper.el:3]]
;;;###autoload
(defun my-whisper-maybe-continue ()
  (when (process-live-p whisper--recording-process)
    (my-whisper-continue)))
;; Using Silero voice activity detection to automatically queue multiple transcriptions with natrys/whisper.el:3 ends here

;; [[file:../Sacha.org::#writing-and-editing-speech-recognition-slowly-building-speech-based-commands-for-emacs][Slowly building speech-based commands for Emacs:1]]
(defvar my-whisper-commands
  '(("scroll up" . scroll-down-command)
    ("scrolling up" . scroll-down-command)
    ("page up" . scroll-down-command)
    ("scroll down" . scroll-up-command)
    ("scroll down" . scroll-up-command)
    ("page down" . scroll-up-command)
    ("next page" . scroll-up-command)
    ("close other windows" . delete-other-windows)
    ("run the buffer" . eval-buffer)
    ("mark buffer" . mark-whole-buffer)
    ("mark paragraph" . mark-paragraph)
    ("expand" . expand-region)
    ("start emacs news" . sacha-workflow-emacs-news-start)
    ("update emacs calendar" . sacha-workflow-emacs-calendar-update)
    )
  "Commands for speech recognition.")

;;;###autoload
(defun my-whisper-handle-commands (text)
  ;; Let's do commands at the beginning of a speech segment for now
  (if (string-match (concat "^" (regexp-opt (mapcar 'car my-whisper-commands)) "\\>")
                    text)
      (progn
        (while (string-match (concat "^\\(" (regexp-opt (mapcar 'car my-whisper-commands)) "\\)\\>[,\\.\\?]? *")
                             text)
          (let* ((match (match-string 1 text))
                 (func (assoc-default (downcase match) my-whisper-commands #'string=)))
            (when func
              (message "Command: %s" match)
              (setq text (replace-match "" nil nil text))
              (cond
               ((commandp func)
                (call-interactively func))
               ((functionp func)
                (funcall func))))))
        text)
    text))

(defvar my-whisper-replacements
  '((" *\\<start \\(list\\|next\\) item\\>[\\.,] *" . "\n- ")
    (" *\\<start check ?box\\>[\\.,] *" . "\n- [ ] ")
    (" *start paragraph[\\.,]? *" . "\n\n")))

;;;###autoload
(defun my-whisper-process-replacements ()
  (goto-char (point-min))
  (when (looking-at " +") (replace-match ""))
  (let ((case-fold-search t))
    (cond
     ((re-search-forward  " *okay[,\\.]? stop recording" nil t)
      (when (process-live-p whisper--recording-process)
        (replace-match "")
        (message "Stopping.")
        (my-whisper-done)))))
  (dolist (rep my-whisper-replacements)
    (goto-char (point-min))
    (while (re-search-forward (car rep) nil t)
      (replace-match (cdr rep))))
  (goto-char (point-max))
  (insert " "))

;; Slowly building speech-based commands for Emacs:1 ends here

;; [[file:../Sacha.org::#writing-and-editing-speech-recognition-okay-track][Okay, track...:1]]
(defvar my-quantified-common-categories
  '(("Emacs" . "Discretionary - Productive - Emacs")
    ("Child care" . "Childcare")
    ("French" . "Discretionary - French")
    ("Brigade" . "Discretionary - Productive - Bike Brigade")
    ("Consulting" . "E1 Gen")))

;;;###autoload
(defun my-speech-input-quantified-track (text)
  "Start tracking time."
  (if (and text
           (string-match "^ok\\(?:ay\\)?[,\\.]? track \\(.+\\)" text))
      (let ((category
             (speech-input-match-in-list
              (match-string 1 text)
              (mapcar 'car my-quantified-common-categories))))
        (message "Tracking %s" category)
        (quantified-track
         (assoc-default category my-quantified-common-categories #'string=))
        nil)
    text))
;; Okay, track...:1 ends here

;; [[file:../Sacha.org::#writing-and-editing-speech-recognition-using-speech-recognition-for-translations-in-emacs-and-faking-in-buffer-completion-for-the-results][Using speech recognition for on-the-fly translations in Emacs and faking in-buffer completion for the results:2]]
;;;###autoload
(defun my-whisper-translate ()
  (goto-char (point-min))
  (let ((case-fold-search t))
    (when (re-search-forward "okay[,\\.]? translate[,\\.]? \\(.+\\)\\|okay[,\\.]? \\(.+?\\) in French" nil t)
      (let* ((s (or (match-string 1) (match-string 2)))
             (translation (save-match-data (my-lang-en-to-fr s))))
        (replace-match
         (propertize translation
                     'type-hint translation
                     'type-original s
                     'help-echo s))))))

;; Using speech recognition for on-the-fly translations in Emacs and faking in-buffer completion for the results:2 ends here

;; [[file:../Sacha.org::#writing-and-editing-speech-recognition-using-speech-recognition-for-translations-in-emacs-and-faking-in-buffer-completion-for-the-results][Using speech recognition for on-the-fly translations in Emacs and faking in-buffer completion for the results:4]]
;;;###autoload
(defun my-whisper-maybe-type-with-hints (text)
  "Add this function to `whisper-insert-text-at-point'."
  (let* ((hint (and text (org-find-text-property-in-string 'type-hint text)))
         (original (and text (org-find-text-property-in-string 'type-original text))))
    (if hint
        (progn
          (learn-lang-type-with-hint hint original)
          nil)
      text)))
;; Using speech recognition for on-the-fly translations in Emacs and faking in-buffer completion for the results:4 ends here

(provide 'my-speech-input)
;;; my-speech-input.el ends here
