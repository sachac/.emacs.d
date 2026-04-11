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

;;; Code:



;; [[file:../Sacha.org::#multimedia-whisper][Using whisper.el to convert speech to text and save it to the currently clocked task in Org Mode or elsewhere:2]]
(defvar sacha-whisper-org-reminder-template "t")

;;;###autoload
(defun sacha-whisper-org-process-reminder ()
  (let ((text (buffer-string))
        reminder)
    (when (string-match "computer[,\.]? reminds? me to \\(.+\\)" text)
      (setq reminder (match-string 1 text))
      (save-window-excursion
        (with-current-buffer (if (markerp whisper--marker) (marker-buffer whisper--marker) (current-buffer))
          (when (markerp whisper--marker) (goto-char whisper--marker))
          (org-capture nil sacha-whisper-org-reminder-template)
          (insert reminder)
          (org-capture-finalize)))
      (erase-buffer))))

;; Using whisper.el to convert speech to text and save it to the currently clocked task in Org Mode or elsewhere:2 ends here

;; [[file:../Sacha.org::#multimedia-whisper][Using whisper.el to convert speech to text and save it to the currently clocked task in Org Mode or elsewhere:4]]
(defvar sacha-whisper-dir "~/recordings/whisper/")
;;;###autoload
(defun sacha-whisper-set-temp-filename ()
  (setq whisper--temp-file (expand-file-name
                            (format-time-string "%Y-%m-%d-%H-%M-%S.wav")
                            sacha-whisper-dir)))

;; Using whisper.el to convert speech to text and save it to the currently clocked task in Org Mode or elsewhere:4 ends here

;; [[file:../Sacha.org::#multimedia-whisper][Using whisper.el to convert speech to text and save it to the currently clocked task in Org Mode or elsewhere:6]]
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
;; Using whisper.el to convert speech to text and save it to the currently clocked task in Org Mode or elsewhere:6 ends here

;; [[file:../Sacha.org::#multimedia-whisper][Using whisper.el to convert speech to text and save it to the currently clocked task in Org Mode or elsewhere:7]]
;;;###autoload
(defun sacha-whisper-toggle-language ()
  "Set the language explicitly, since sometimes auto doesn't figure out the right one."
  (interactive)
  (setq whisper-language (if (string= whisper-language "en") "fr" "en"))
  ;; If using a server, we need to restart for the language
  (when (process-live-p whisper--server-process) (kill-process whisper--server-process))
  (message "%s" whisper-language))
;; Using whisper.el to convert speech to text and save it to the currently clocked task in Org Mode or elsewhere:7 ends here

;; [[file:../Sacha.org::#multimedia-whisper][Using whisper.el to convert speech to text and save it to the currently clocked task in Org Mode or elsewhere:8]]
;;;###autoload
(defun sacha-whisper-reset (text)
  (setq sacha-whisper-skip-annotation nil)
  (remove-hook 'whisper-insert-text-at-point #'sacha-whisper-org-save-to-clocked-task)
  text)
;; Using whisper.el to convert speech to text and save it to the currently clocked task in Org Mode or elsewhere:8 ends here

;; [[file:../Sacha.org::#multimedia-whisper][Using whisper.el to convert speech to text and save it to the currently clocked task in Org Mode or elsewhere:10]]
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
;; Using whisper.el to convert speech to text and save it to the currently clocked task in Org Mode or elsewhere:10 ends here

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

;; [[file:../Sacha.org::#multimedia-whisper][Using whisper.el to convert speech to text and save it to the currently clocked task in Org Mode or elsewhere:12]]
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

;; Using whisper.el to convert speech to text and save it to the currently clocked task in Org Mode or elsewhere:12 ends here

;; [[file:../Sacha.org::#multimedia-whisper][Using whisper.el to convert speech to text and save it to the currently clocked task in Org Mode or elsewhere:14]]
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

;; Using whisper.el to convert speech to text and save it to the currently clocked task in Org Mode or elsewhere:14 ends here

;; [[file:../Sacha.org::#multimedia-whisper][Using whisper.el to convert speech to text and save it to the currently clocked task in Org Mode or elsewhere:16]]
;;;###autoload
(defun sacha-whisper-org-clear-saved-annotation ()
  (setq sacha-whisper-org-last-annotation nil))
;; Using whisper.el to convert speech to text and save it to the currently clocked task in Org Mode or elsewhere:16 ends here

;; [[file:../Sacha.org::#multimedia-whisper][Using whisper.el to convert speech to text and save it to the currently clocked task in Org Mode or elsewhere:17]]
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
;; Using whisper.el to convert speech to text and save it to the currently clocked task in Org Mode or elsewhere:17 ends here

;; [[file:../Sacha.org::#multimedia-whisper][Using whisper.el to convert speech to text and save it to the currently clocked task in Org Mode or elsewhere:18]]
;;;###autoload
(defun sacha-whisper-redo ()
  (interactive)
  (setq whisper--marker (point-marker))
  (whisper--transcribe-audio))
;; Using whisper.el to convert speech to text and save it to the currently clocked task in Org Mode or elsewhere:18 ends here

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
  (setq speech-input-model model-name))
;; Emacs and whisper.el: Trying out different speech-to-text backends and models:6 ends here

;; [[file:../Sacha.org::#writing-and-editing-speech-recognition-queue-multiple-transcriptions-with-whisper-el-speech-recognition][Queuing multiple transcriptions with whisper.el speech recognition:1]]
(defvar sacha-whisper--queue nil)
;;;###autoload
(defun sacha-whisper-continue (&optional arg)
  "Send what we've got so far for transcription and then continue recording.
Call with \\[universal-argument] to signal that we can stop."
  (interactive "P")
  (require 'whisper)
  (if arg
      (sacha-whisper-done)
    (setq whisper--marker (point-marker) whisper--point-buffer (current-buffer))
    (when (process-live-p whisper--recording-process)
      ;; queue only if the last one is not asking for the same file
      (unless
          (string=
           (plist-get
            (car
             (last sacha-whisper--queue))
            :file)
           whisper--temp-file)
        (add-to-list
         'sacha-whisper--queue
         (list :file whisper--temp-file
               :buffer
               (format "*result: %s*" (file-name-base whisper--temp-file)))
         t))
      ;; Remove the sentinel; handle results ourselves
      (set-process-sentinel whisper--recording-process
                            (lambda (process event)
                              (sacha-whisper-process-queue)))
      (interrupt-process whisper--recording-process))
    (run-hooks 'whisper-before-transcription-hook)
    (whisper--setup-mode-line :show 'recording)
    (whisper--record-audio)))

;;;###autoload
(defun sacha-whisper-discard ()
 "Ignore the previous recording."
  (interactive)
  (when (process-live-p whisper--recording-process)
    ;; Remove the sentinel; handle results ourselves
    (set-process-sentinel whisper--recording-process
                          (lambda (process event)
                            (when (file-exists-p whisper--temp-file)
                              (delete-file whisper--temp-file))
                            (sacha-whisper-process-queue)))
    (interrupt-process whisper--recording-process)))

;;;###autoload
(defun sacha-whisper-discard-and-continue ()
 "Ignore the previous recording and continue."
  (interactive)
  (if (process-live-p whisper--recording-process)
      (progn
        ;; Remove the sentinel; handle results ourselves
        (set-process-sentinel whisper--recording-process
                              (lambda (process event)
                                (sacha-whisper-process-queue)
                                (sacha-whisper-continue)))
        (interrupt-process whisper--recording-process))
    (sacha-whisper-continue)))

;;;###autoload
(defun sacha-whisper-done ()
  (interactive)
  (when (process-live-p whisper--recording-process)
    (add-to-list
     'sacha-whisper--queue
     (list :file whisper--temp-file
           :buffer
           (format "*result: %s*" (file-name-base whisper--temp-file)))
     t)
    ;; Remove the sentinel; handle results ourselves
    (set-process-sentinel whisper--recording-process
                          (lambda (process event)
                            (sacha-whisper-process-queue)))
    (whisper--setup-mode-line :hide 'recording)
    (interrupt-process whisper--recording-process)))

;;;###autoload
(defun sacha-whisper-process-queue-result ()
  "Process the first part of the queue that already has results."
  (while (plist-get (car sacha-whisper--queue) :results)
    (let ((o (pop sacha-whisper--queue)))
      (unless sacha-whisper-target-markers
        (setq whisper--marker (point-marker)
              whisper--point-buffer (current-buffer)))
      (with-current-buffer (plist-get o :buffer)
        (erase-buffer)
        (insert (plist-get o :results)))
      ;; Only works with my fork: https://github.com/sachac/whisper.el/tree/whisper-insert-text-at-point-function
      (whisper--handle-transcription-output nil (plist-get o :buffer)))))

;;;###autoload
(defun sacha-whisper-process-queue ()
  (let (o)
    (while (setq o (seq-find (lambda (o) (and (plist-get o :file)
                                              (not (plist-get o :process))
                                              (not (plist-get o :results))))
                             sacha-whisper--queue))
      (let* ((headers (list "Content-Type: multipart/form-data"))
             (params (list (concat "file=@"
                                   (plist-get o :file))
                           "temperature=0.0"
                           "temperature_inc=0.2"
                           "response_format=json"
                           (concat "model=" whisper-model)
                           (concat "language=" whisper-language)))
             (url (format sacha-whisper-url-format whisper-server-host whisper-server-port))
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
              (let ((current sacha-whisper--queue-item))
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
            (sacha-whisper-process-queue-result))))
        (plist-put o :command (string-join command " "))
        (with-current-buffer (process-buffer (plist-get o :process))
          (setq-local sacha-whisper--queue-item o))))))
(defvar-local sacha-whisper--queue-item nil)

;;;###autoload
(defun sacha-whisper-reprocess-queue ()
  (interactive)
  (setq whisper--marker (point-marker) whisper--point-buffer (current-buffer))
  (mapc (lambda (o)
          (when (process-live-p (plist-get o :process))
            (kill-process (plist-get o :process)))
          (when (get-buffer (plist-get o :buffer))
            (kill-buffer (plist-get o :buffer)))
          (plist-put o :process nil)
          (plist-put o :results nil))
        sacha-whisper--queue)
  (sacha-whisper-process-queue))

;;;###autoload
(defun sacha-whisper-clear-queue ()
  (interactive)
  (mapc (lambda (o)
          (when (process-live-p (plist-get o :process))
            (kill-process (plist-get o :process)))
          (when (get-buffer (plist-get o :buffer))
            (kill-buffer (plist-get o :buffer)))
          (plist-put o :process nil)
          (plist-put o :results nil))
        sacha-whisper--queue)
  (setq sacha-whisper--queue nil))

(defvar-keymap sacha-whisper-simulated-continuous-mode-map
  :doc "Keymap for sacha-minor-mode."
  "S-<f2>" #'sacha-whisper-continue
  )
(define-key sacha-whisper-simulated-continuous-mode-map [remap whisper-run] #'sacha-whisper-continue)

(define-minor-mode sacha-whisper-simulated-continuous-mode
  "Simulate continuous speech recognition by queuing."
  :lighter "W"
  (if sacha-whisper-simulated-continuous-mode
      (message "Start speaking...")
    (message "All done.")
    (sacha-whisper-done)))

;; Queuing multiple transcriptions with whisper.el speech recognition:1 ends here

;; [[file:../Sacha.org::#writing-and-editing-speech-recognition-using-silero-voice-activity-detection-to-automatically-queue-multiple-transcriptions-with-natrys-whisper-el][Using Silero voice activity detection to automatically queue multiple transcriptions with natrys/whisper.el:3]]
;;;###autoload
(defun sacha-whisper-maybe-continue ()
  (when (process-live-p whisper--recording-process)
    (sacha-whisper-continue)))
;; Using Silero voice activity detection to automatically queue multiple transcriptions with natrys/whisper.el:3 ends here

;; [[file:../Sacha.org::#writing-and-editing-speech-recognition-slowly-building-speech-based-commands-for-emacs][Slowly building speech-based commands for Emacs:1]]
(defvar sacha-whisper-commands
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
(defun sacha-whisper-handle-commands (text)
  ;; Let's do commands at the beginning of a speech segment for now
  (if (string-match (concat "^" (regexp-opt (mapcar 'car sacha-whisper-commands)) "\\>")
                    text)
      (progn
        (while (string-match (concat "^\\(" (regexp-opt (mapcar 'car sacha-whisper-commands)) "\\)\\>[,\\.\\?]? *")
                             text)
          (let* ((match (match-string 1 text))
                 (func (assoc-default (downcase match) sacha-whisper-commands #'string=)))
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

(add-to-list 'sacha-speech-functions #'sacha-speech-append-to-etherpad)
;; Streaming speech recognition into Emacs using Google Chrome Web Speech API:4 ends here

;; [[file:../Sacha.org::#writing-and-editing-speech-recognition-streaming-speech-recognition-into-emacs-using-google-chrome-web-speech-api][Streaming speech recognition into Emacs using Google Chrome Web Speech API:5]]
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

(add-to-list 'sacha-speech-functions #'sacha-speech-send-to-erc)
;; Streaming speech recognition into Emacs using Google Chrome Web Speech API:5 ends here

;; [[file:../Sacha.org::#writing-and-editing-speech-recognition-streaming-speech-recognition-into-emacs-using-google-chrome-web-speech-api][Streaming speech recognition into Emacs using Google Chrome Web Speech API:6]]
;;;###autoload
(defun sacha-speech-fix-common-errors (info)
  (with-temp-buffer
    (insert (alist-get 'content info))
    (goto-char (point-min))
    (sacha-subed-fix-common-errors-from-start)
    (setf (alist-get 'content info) (buffer-string)))
  info)
(add-hook 'sacha-speech-functions #'sacha-speech-fix-common-errors -100)
;; Streaming speech recognition into Emacs using Google Chrome Web Speech API:6 ends here

;; [[file:../Sacha.org::#writing-and-editing-speech-recognition-streaming-speech-recognition-into-emacs-using-google-chrome-web-speech-api][Streaming speech recognition into Emacs using Google Chrome Web Speech API:7]]
;;;###autoload
(defun sacha-speech-insert-at-markers (info)
  (when (and sacha-whisper-target-markers info)
    (sacha-whisper-insert (alist-get 'content info))))
(add-hook 'sacha-speech-functions #'sacha-speech-insert-at-markers 100)

;; Streaming speech recognition into Emacs using Google Chrome Web Speech API:7 ends here

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

(add-to-list 'sacha-speech-functions #'sacha-speech-subed-record)
;; speech and subed-record:1 ends here

(provide 'sacha-speech-input)
;;; sacha-speech-input.el ends here
