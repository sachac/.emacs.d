;;; sacha-ai.el ---  -*- lexical-binding: t -*-

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
;; - ChatGPT, AI, and large-language models
;;   https://sachachua.com/dotemacs#chatgpt-ai
;;
;; - Ask me questions based on the text so far
;;   https://sachachua.com/dotemacs#inactive-infrequent-things-chatgpt-ai-and-large-language-models-presets
;;
;; - agent-shell
;;   https://sachachua.com/dotemacs#inactive-infrequent-things-chatgpt-ai-and-large-language-models-agent-shell
;;
;;; Code:



;; [[file:../Sacha.org::#chatgpt-ai][ChatGPT, AI, and large-language models:2]]
;;;###autoload
(defun gptel-api-key-from-environment (&optional var)
  (lambda ()
    (getenv (or var                     ;provided key
                (thread-first           ;or fall back to <TYPE>_API_KEY
                  (type-of gptel-backend)
                  (symbol-name)
                  (substring 6)
                  (upcase)
                  (concat "_API_KEY"))))))
;; ChatGPT, AI, and large-language models:2 ends here

;; [[file:../Sacha.org::#chatgpt-ai][ChatGPT, AI, and large-language models:4]]
;;;###autoload
(defun sacha-gptel-set-model ()
  "Interactively set the gptel backend and model.
Generated with an LLM."
  (interactive)
  (require 'gptel)
  (let* ((models-alist
          (cl-loop for (name . backend) in gptel--known-backends
                   nconc (cl-loop for model in (gptel-backend-models backend)
                                  collect (list (concat name ":" (gptel--model-name model))
                                                backend model))))
         (current-cand (concat (gptel-backend-name gptel-backend) ":"
                              (gptel--model-name gptel-model)))
         (choice (completing-read "Model: " models-alist nil t nil nil current-cand))
         (match (cdr (assoc choice models-alist))))
    (when match
      (setq gptel-backend (nth 0 match)
            gptel-model   (nth 1 match)))))
;; ChatGPT, AI, and large-language models:4 ends here

;; [[file:../Sacha.org::#inactive-infrequent-things-chatgpt-ai-and-large-language-models-presets][Ask me questions based on the text so far:1]]
(defun sacha-gptel-check-if-too-large ()
	"Error if I might have forgotten to narrow the buffer."
	(when (and (> (- (point) (point-min)) 15000)
						 (not (y-or-n-p "Large buffer. Continue?")))
		(error "Too large.")))

(defun sacha-gptel-writing-context ()
	"Return the subtree or buffer, depending on the size."
	(if (> (- (point-max) (point-min)) 15000)
			(sacha-org-subtree-text)
		(buffer-string)))

;;;###autoload
(defun sacha-gptel-questions ()
  "Display follow-up questions in a buffer."
  (interactive)
	(sacha-whisper-audio-feedback "Thinking...")
	(gptel-request
			(concat
			 "Make a list of ten short follow-up questions based on this text. Assume a fairly knowledgeable reader or listener. Use Org Mode syntax. Do not include a preamble, just return the questions.
###
"
			 (sacha-gptel-writing-context))
		:callback
		(lambda (response info)
			(with-current-buffer (get-buffer-create "*gptel-feedback*")
				(erase-buffer)
				(insert (or (let-alist (car (alist-get 'choices info))
											.message.content)
										(prin1-to-string response)))
				(display-buffer (current-buffer))))))

;;;###autoload
(defun sacha-gptel-proofread ()
  "Proofread"
  (interactive)
	(sacha-whisper-audio-feedback "Thinking...")
	(gptel-request
			(concat
			 (plist-get (gptel-get-preset 'proofread) :system) "\n\n"
			 (sacha-gptel-writing-context))
		:callback
		(lambda (response info)
			(with-current-buffer (get-buffer-create "*gptel-feedback*")
				(erase-buffer)
				(insert (or (let-alist (car (alist-get 'choices info))
											.message.content)
										response))
				(display-buffer (current-buffer))))))

(eval-when-compile (require 'phony))
(with-eval-after-load 'phony
	(phony-defun proofread ("proofread")
		(sacha-gptel-proofread))
	(phony-defun proof-read ("proof" "read")
		(sacha-gptel-proofread)))

;;;###autoload
(defun sacha-gptel-feedback ()
  "Display writing feedback in a buffer."
  (interactive)
	(sacha-gptel-check-if-too-large)
	(gptel-request (concat "Make a list of three ideas for improving this text. Assume a fairly knowledgeable reader or listener. Use Org Mode syntax. Do not include a preamble, just return the feedback.
###
" (buffer-string))
		:callback
		(lambda (response info)
			(with-current-buffer (get-buffer-create "*gptel-feedback*")
				(erase-buffer)
				(insert (or (let-alist (car (alist-get 'choices info))
											.message.content)
										(prin1-to-string response)))
				(display-buffer (current-buffer))))))

(with-eval-after-load 'gptel
	(gptel-make-preset 'proofread :system "Proofread. Do not return the fixed text, just tell me what edits I need to make.")
	(gptel-make-preset 'questions :system "Make a list of three short follow-up questions based on this text. Assume a fairly knowledgeable reader or listener. Use Org Mode syntax. Do not include a preamble, just return the questions.")
	(gptel-make-preset 'french-conversation :system "Chat like French conversation partners. My level is B1. Reply in 1-3 simple sentences using words for my level. Input is unedited transcription, so there may be misrecognized words. Try to introduce new vocabulary words naturally. End every reply with one easy question to keep us talking." :model 'gemini-3.5-flash-lite))

;;;###autoload
(defun sacha-gptel-practice-french ()
	"Practice French conversation. "
	(interactive)
	(with-current-buffer (gptel "*French*")
		(gptel-preset 'french-conversation)
		(sacha-whisper-conversation-mode 1)
		(switch-to-buffer (current-buffer))))
(eval-when-compile (require 'phony))
(with-eval-after-load 'phony
	(phony-defun practice-french ("practice" "french")
		(sacha-gptel-practice-french)))
;; Ask me questions based on the text so far:1 ends here

;; [[file:../Sacha.org::#inactive-infrequent-things-chatgpt-ai-and-large-language-models-agent-shell][agent-shell:1]]
;;;###autoload
(defun sacha-agent-shell-dot-subdir (subdir)
  (let* ((cwd (string-remove-suffix "/" (agent-shell-cwd)))
         (sanitized (replace-regexp-in-string "/" "-" (string-remove-prefix "/" cwd))))
    (expand-file-name subdir (locate-user-emacs-file (concat "agent-shell/" sanitized)))))

;; agent-shell:1 ends here

(provide 'sacha-ai)
;;; sacha-ai.el ends here
