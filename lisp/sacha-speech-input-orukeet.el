;;; sacha-speech-input-orukeet.el ---  -*- lexical-binding: t -*-

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
;; - Orukeet
;;   https://sachachua.com/dotemacs#writing-and-editing-speech-recognition-orukeet
;;
;;; Code:



;; [[file:../Sacha.org::#writing-and-editing-speech-recognition-orukeet][Orukeet:1]]
(defvar sacha-speech-input-orukeet-command
 	'("/home/sacha/vendor/orukeet/.venv/bin/python" "/home/sacha/vendor/orukeet/orukeet-local.py" "--installation" "/home/sacha/vendor/orukeet/installation.json"))

;;;###autoload
(defun sacha-speech-input-orukeet-json (filename &optional callback)
	(let ((buffer (generate-new-buffer " *orukeet*" t)))
		(if callback
				(make-process
				 :name "orukeet"
				 :buffer buffer
				 :command (append sacha-speech-input-orukeet-command (list (expand-file-name filename)))
				 :sentinel (lambda (proc event)
										 (let (data)
											 (with-current-buffer (process-buffer proc)
												 (goto-char (point-min))
												 (setq data (json-read))))
										 (kill-buffer (process-buffer proc))
										 (funcall callback data)))
			(let (data)
				(with-current-buffer buffer
					(apply #'call-process
								 (car sacha-speech-input-orukeet-command) nil t nil
								 (append (cdr sacha-speech-input-orukeet-command)
												 (list (expand-file-name filename))))
					(goto-char (point-min))
					(setq data (json-read)))
				(kill-buffer buffer)
				data))))
;; (setq speech-input-transcribe-url #'sacha-speech-input-orukeet-json)
;; Orukeet:1 ends here

(provide 'sacha-speech-input-orukeet)
;;; sacha-speech-input-orukeet.el ends here
