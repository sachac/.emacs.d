;;; sacha-speech-input-live.el ---  -*- lexical-binding: t -*-

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



;; [[file:../Sacha.org::#general-code][General code:1]]
(defvar sacha-live-speech-buffer "*Speech*")
(defvar sacha-live-speech-process nil)
(defvar sacha-live-speech-output-buffer "*Speech JSON*")

(defvar sacha-live-speech-functions
	'(sacha-live-speech-display-in-speech-buffer
		sacha-live-speech-display-wpm
		sacha-live-speech-append-to-etherpad)
	"Functions to call with one argument, the recognition results.")

;;;###autoload
(defun sacha-live-speech-start ()
	"Turn on live captions."
	(interactive)
	(with-current-buffer (get-buffer-create sacha-live-speech-buffer)
		(unless (process-live-p sacha-live-speech-process)
			(let ((default-directory "~/proj/deepgram-live"))
				(message "%s" default-directory)
				(with-current-buffer (get-buffer-create sacha-live-speech-output-buffer)
					(erase-buffer))
				(setq sacha-live-speech-recent-words nil
							sacha-live-speech-wpm-string "READY ")
				(setq sacha-deepgram-process
							(make-process
							 :command '("bash" "run.sh")
							 :name "speech"
							 :filter 'sacha-live-speech-json-filter
							 :sentinel #'sacha-live-speech-process-sentinel
							 :buffer sacha-live-speech-output-buffer)))
			(org-mode))
    (display-buffer (current-buffer))))

;;;###autoload
(defun sacha-live-speech-stop ()
	(interactive)
	(if (process-live-p sacha-live-speech-process)
			(kill-process sacha-live-speech-process))
	(setq sacha-live-speech-wpm-string nil))

;; (define-minor-mode sacha-live-speech-mode
;; 	"Show live speech and display WPM.
;; Need to check how to reliably turn this on and off."
;; 	:global t :group 'sachac
;; 	(if sacha-live-speech-mode
;; 			(sacha-live-speech-start)
;; 		(sacha-live-speech-stop)
;; 		(setq sacha-live-speech-wpm-string nil)))

;; based on subed-mpv::client-filter
;;;###autoload
(defun sacha-live-speech-handle-json (line-object)
	"Process the JSON object in LINE."
	(run-hook-with-args 'sacha-live-speech-functions (json-parse-string line :object-type 'alist)))

;;;###autoload
(defun sacha-live-speech-process-sentinel (proc event)
	(when (string-match "finished" event)
		(sacha-live-speech-stop)
		;(sacha-live-speech-mode -1)
		))

;;;###autoload
(defun sacha-live-speech-json-filter (proc string)
	(when (buffer-live-p (process-buffer proc))
		(with-current-buffer (process-buffer proc)
			(let* ((proc-mark (process-mark proc))
						 (moving (= (point) proc-mark)))
				;;  insert the output
				(save-excursion
					(goto-char proc-mark)
					(insert string)
					(set-marker proc-mark (point)))
				(if moving (goto-char proc-mark))
				;; process and remove all complete lines of JSON (lines are complete if ending with \n)
				(let ((pos (point-min)))
					(while (progn (goto-char pos)
												(end-of-line)
												(equal (following-char) ?\n))
						(let* ((end (point))
									 (line (buffer-substring pos end)))
							(delete-region pos (+ end 1))
							(with-current-buffer (get-buffer sacha-live-speech-buffer)
								(sacha-live-speech-handle-json line)))))))))
;; General code:1 ends here

;; [[file:../Sacha.org::#display-in-speech-buffer][Display in speech buffer:1]]
;;;###autoload
(defun sacha-live-speech-display-in-speech-buffer (recognition-results)
	(with-current-buffer (get-buffer-create sacha-live-speech-buffer)
		(let-alist recognition-results
			(let* ((pos (point))
						 (at-end (eobp)))
				(goto-char (point-max))
				(unless (eolp) (insert "\n"))
				(when .msg
					(insert .msg "\n"))
				(when .transcript
					(insert .transcript "\n"))
				;; scroll to the bottom if being displayed
				(if at-end
						(when (get-buffer-window (current-buffer))
							(set-window-point (get-buffer-window (current-buffer)) (point)))
					(goto-char pos))))))

;;;###autoload
(defun sacha-live-speech-toggle-heading ()
	"Toggle a line as a heading."
	(interactive)
	(with-current-buffer (get-buffer sacha-live-speech-buffer)
		(display-buffer (current-buffer))
		(with-selected-window (get-buffer-window (get-buffer sacha-live-speech-buffer))
			(let ((avy-all-windows nil))
				(avy-goto-line 1))
			(org-toggle-heading 1))))
;;;###autoload
(defun sacha-live-speech-cycle-visibility ()
	"Get a quick overview."
	(interactive)
	(with-current-buffer (get-buffer sacha-live-speech-buffer)
		(display-buffer (current-buffer))
		(if (eq org-cycle-global-status 'contents)
				(progn
					(run-hook-with-args 'org-cycle-pre-hook 'all)
					(org-fold-show-all '(headings blocks))
					(setq org-cycle-global-status 'all)
					(run-hook-with-args 'org-cycle-hook 'all))
			(run-hook-with-args 'org-cycle-pre-hook 'contents)
			(org-cycle-content)
			(setq org-cycle-global-status 'contents)
			(run-hook-with-args 'org-cycle-hook 'contents))))
;; Display in speech buffer:1 ends here

;; [[file:../Sacha.org::#display-words-per-minute][Display words per minute:1]]
(defvar sacha-live-speech-wpm-window-seconds 15 "How many seconds to calculate WPM for.")
(defvar sacha-live-speech-recent-words nil "Words spoken in `sacha-live-speech-wpm-window-minutes'.")
(defvar sacha-live-speech-wpm nil "Current WPM.")
(defvar sacha-live-speech-wpm-colors  ; haven't figured out how to make these work yet
	'((180 :foreground "red")
		(170 :foreground "yellow")
		(160 :foreground "green")))
(defvar sacha-live-speech-wpm-string nil "Add this somewhere in `mode-line-format'.")
;;;###autoload
(defun sacha-live-speech-wpm-string ()
	(propertize
	 (format "%d WPM " sacha-live-speech-wpm)
	 'face
	 (cdr (seq-find (lambda (row) (> sacha-live-speech-wpm (car row))) sacha-live-speech-wpm-colors))))

;;;###autoload
(defun sacha-live-speech-display-wpm (recognition-results)
	(let-alist recognition-results
		(when .words
			;; calculate WPM
			(setq sacha-live-speech-recent-words
						(append sacha-live-speech-recent-words .words nil))
			(let ((threshold (- (assoc-default 'end (aref .words (1- (length .words))))
													sacha-live-speech-wpm-window-seconds)))
				(setq sacha-live-speech-recent-words
							(seq-filter
							 (lambda (o)
								 (>= (assoc-default 'start o)
										 threshold))
							 sacha-live-speech-recent-words))
				(setq sacha-live-speech-wpm
							(/
							 (length sacha-live-speech-recent-words)
							 (/ (- (assoc-default 'end (aref .words (1- (length .words))))
										 (assoc-default 'start (car sacha-live-speech-recent-words)))
									60.0)))
				(setq sacha-live-speech-wpm-string (sacha-live-speech-wpm-string))))))
;; Display words per minute:1 ends here

;; [[file:../Sacha.org::#append-to-emacsconf-etherpad][Append to EmacsConf Etherpad:1]]
(defvar sacha-live-speech-etherpad-id nil)
;;;###autoload
(defun sacha-live-speech-append-to-etherpad (recognition-results)
	(when sacha-live-speech-etherpad-id
		(emacsconf-pad-append-text sacha-live-speech-etherpad-id (concat " " (assoc-default 'transcript recognition-results)))))
;; Append to EmacsConf Etherpad:1 ends here

(provide 'sacha-speech-input-live)
;;; sacha-speech-input-live.el ends here
