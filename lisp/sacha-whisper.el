;;; sacha-whisper.el ---  -*- lexical-binding: t -*-

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
;; - Queuing multiple transcriptions with whisper.el speech recognition
;;   https://sachachua.com/dotemacs#writing-and-editing-speech-recognition-queue-multiple-transcriptions-with-whisper-el-speech-recognition
;;
;; - Switch task
;;   https://sachachua.com/dotemacs#writing-and-editing-speech-recognition-switch-task
;;
;; - Scratch that
;;   https://sachachua.com/dotemacs#writing-and-editing-speech-recognition-scratch-that
;;
;; - Expanding yasnippets by voice in Emacs and other applications
;;   https://sachachua.com/dotemacs#writing-and-editing-speech-recognition-expanding-yasnippet-by-voice
;;
;;; Code:



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
          (setq-local sacha-whisper--queue-item o)
					(setq-local whisper--temp-file (plist-get o :file))
					(whisper--store-process-info (current-buffer)))))))
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

(define-minor-mode sacha-whisper-simulated-continuous-mode
  "Simulate continuous speech recognition by queuing."
  :lighter "W"
  (if sacha-whisper-simulated-continuous-mode
      (message "Start speaking...")
    (message "All done.")
    (sacha-whisper-done)))

;; Queuing multiple transcriptions with whisper.el speech recognition:1 ends here

;; [[file:../Sacha.org::#writing-and-editing-speech-recognition-switch-task][Switch task:1]]
;;;###autoload
(defmacro sacha-whisper-org-capture (filename function docstring template message)
	"Register FUNCTION to be autoloaded from FILENAME to capture to TEMPLATE.
Display MESSAGE."
	`(defun ,function (text)
			 ,docstring
			 (let ((sacha-org-quick-task t)
						 (screenshot (sacha-screenshot-current-screen)))
				 ;; take a screenshot too
				 (org-capture-string
					(if (get-text-property 0 'file text)
							(format "%s %s %s"
											text
											(org-link-make-string (concat "audio:" (get-text-property 0'file text))
																						"▶️")
											(org-link-make-string (concat "file:" screenshot) "screenshot"))
						(format "%s %s"
										text
										(org-link-make-string (concat "file:" screenshot) "screenshot")))
					,template))
			 (message ,message text)
			 ""))

;;;###autoload
(sacha-whisper-org-capture
 "sacha-whisper"
 sacha-whisper-switch-task-to
 "Save the rest of this text to my inbox and clock into it."
 "wi"
 "Switched to: %s")

;;;###autoload
(sacha-whisper-org-capture
 "sacha-whisper"
 sacha-whisper-task-today
 "Make a task for today."
 "wT"
 "Today: %s")

;;;###autoload
(sacha-whisper-org-capture
 "sacha-whisper"
 sacha-whisper-task-someday
 "Make a task for someday."
 "wt"
 "Someday: %s")

;;;###autoload
(sacha-whisper-org-capture
 "sacha-whisper"
 sacha-whisper-note
 "Make a note."
 "wn"
 "Note: %s")

;;;###autoload
(sacha-whisper-org-capture
 "sacha-whisper"
 sacha-whisper-task-tomorrow
 "Make a task for tomorrow."
 "w>"
 "Tomorrow: %s")

;;;###autoload
(sacha-whisper-org-capture
 "sacha-whisper"
 sacha-whisper-task-next-week
 "Make a task for next-week."
 "ww"
 "Next week: %s")

;;;###autoload
(sacha-whisper-org-capture
 "sacha-whisper"
 sacha-whisper-task-next-month
 "Make a task for next month."
 "wm"
 "Next month: %s")
;; Switch task:1 ends here

;; [[file:../Sacha.org::#writing-and-editing-speech-recognition-scratch-that][Scratch that:1]]
;;;###autoload
(defun sacha-whisper-scratch-that (text)
  "Cancel the utterance if I end it with \"scratch that.\""
  (when (string-match "^\\(.*\\)scratch that\\.? *$" text)
		(message "Scratched: %s (%s)" (or (match-string 1 text) "")
						 whisper--temp-file)
		(setq text nil))
	text)
;; Scratch that:1 ends here

;; [[file:../Sacha.org::#writing-and-editing-speech-recognition-expanding-yasnippet-by-voice][Expanding yasnippets by voice in Emacs and other applications:1]]

(declare-function subed-word-data-compare-normalized-string-distance "subed-word-data")

;;;###autoload
(defun sacha-whisper-maybe-expand-snippet (text)
  "Add to `whisper-insert-text-at-point'."
  (if (and text
           (string-match
            "^ok\\(?:ay\\)?[,\\.]? \\(.+\\)" text))
    (let* ((name
            (downcase
             (string-trim
              (replace-regexp-in-string "[,\\.]" "" (match-string 1 text)))))
           (matching
            (seq-find (lambda (o)
                        (subed-word-data-compare-normalized-string-distance
                         name
                         (downcase (yas--template-name o))))
                      (yas--all-templates (yas--get-snippet-tables)))))
      (if matching
          (progn
            (if (frame-focus-state)
                (progn
                  (yas-expand-snippet matching)
                  nil)
              ;; In another application
              (with-temp-buffer
                (yas-minor-mode)
                (yas-expand-snippet matching)
                (buffer-string))))
        text))
    text))
;; Expanding yasnippets by voice in Emacs and other applications:1 ends here

(provide 'sacha-whisper)
;;; sacha-whisper.el ends here
