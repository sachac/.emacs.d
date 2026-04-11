;;; sacha-speech-input-speaches-realtime.el ---  -*- lexical-binding: t -*-

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



;; [[file:../Sacha.org::#writing-and-editing-speech-recognition-try-speaches-with-realtime][Try speaches with realtime:1]]
(defvar sacha-speaches-process nil)
(defvar sacha-speaches-dir "~/vendor/speaches")
;;;###autoload
(defun sacha-speaches-start ()
  (interactive)
  (unless (process-live-p sacha-speaches-process)
    (let ((default-directory sacha-speaches-dir))
      (setq sacha-speaches-process
            (make-process
             :name "speaches-bridge"
             :buffer "*speaches-output*" ; Standard output buffer
             :command '("bash" "-c" "rec -q -t raw -r 16000 -c 1 -b 16 -e signed-integer - | uv run python3 stream.py")
             :filter #'sacha-speaches-filter
             :sentinel (lambda (proc event)
                         (when (memq (process-status proc) '(exit signal))
                           (message "Speaches process finished: %s" event)))
             :stderr "*speaches-stderr*" ; Separate buffer for Python errors/logs
             :noquery t)))
    (message "Speaches started.")))

;;;###autoload
(defun sacha-speaches-filter (proc string)
  "Accumulate STRING and call processor on complete JSON lines."
  (let ((moving-point (process-mark proc))
        results)
    (with-current-buffer (process-buffer proc)
      (save-excursion
        (goto-char moving-point)
        (insert string)
        (set-marker (process-mark proc) (point))
        (goto-char (point-min))
        (while (search-forward "\n" nil t)
          (let ((line (buffer-substring (point-min) (1- (point)))))
            (delete-region (point-min) (point))
            (unless (string-empty-p (string-trim line))
              (condition-case err
                  (let ((json-obj (json-parse-string line :object-type 'alist)))
                    (push json-obj results))
                (error (message "JSON parse error: %s" err))))))))
    (mapc #'sacha-speaches-process-logic
          (nreverse results))))

;;;###autoload
(defun sacha-speaches-process-logic (o)
  "Handle the parsed ALIST from the Speaches bridge."
  (let ((type (cdr (assoc 'type o))))
    (cond
     ((string= type "conversation.item.input_audio_transcription.completed")
      (let ((text (cdr (assoc 'transcript o))))
        (with-current-buffer (get-buffer-create "*speaches*")
          (goto-char (point-max))
          (unless (bolp) (insert "\n"))
          (insert text))))
     (t (prin1 o)))))
;; Try speaches with realtime:1 ends here

(provide 'sacha-speech-input-speaches-realtime)
;;; sacha-speech-input-speaches-realtime.el ends here
