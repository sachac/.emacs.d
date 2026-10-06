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
;; Related Emacs config sections:
;;
;; - Switch task
;;   https://sachachua.com/dotemacs#speech-recognition
;;
;; - Scratch that
;;   https://sachachua.com/dotemacs#speech-recognition
;;
;; - Expanding yasnippets by voice in Emacs and other applications
;;   https://sachachua.com/dotemacs#writing-and-editing-speech-recognition-expanding-yasnippet-by-voice
;;
;;; Code:



;; [[file:../Sacha.org::*Switch task][Switch task:1]]
(defmacro sacha-whisper-org-capture (filename function docstring template message)
	"Register FUNCTION to be autoloaded from FILENAME to capture to TEMPLATE.
Display MESSAGE."
	`(progn
		 (autoload ',function ,filename ,docstring nil)
		 (defun ,function (text)
			 ,docstring
			 (let ((sacha-org-quick-task t))
				 (org-capture-string
					(if (get-text-property 0 'file text)
							(concat text " " (org-link-make-string (concat "audio:" (get-text-property 0'file text))
																										 "▶️"))
						text)
					,template))
			 (message ,message text)
			 "")))

(sacha-whisper-org-capture
 "sacha-whisper"
 sacha-whisper-switch-task-to
 "Save the rest of this text to my inbox and clock into it."
 "wi"
 "Switched to: %s")

(sacha-whisper-org-capture
 "sacha-whisper"
 sacha-whisper-task-today
 "Make a task for today."
 "wT"
 "Today: %s")

(sacha-whisper-org-capture
 "sacha-whisper"
 sacha-whisper-task-someday
 "Make a task for someday."
 "wt"
 "Someday: %s")

(sacha-whisper-org-capture
 "sacha-whisper"
 sacha-whisper-note
 "Make a note."
 "wn"
 "Note: %s")

(sacha-whisper-org-capture
 "sacha-whisper"
 sacha-whisper-task-tomorrow
 "Make a task for tomorrow."
 "w>"
 "Tomorrow: %s")


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
