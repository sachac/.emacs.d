;;; sacha-phony.el ---  -*- lexical-binding: t -*-

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
;; - Emacslisten
;;   https://sachachua.com/dotemacs#writing-and-editing-speech-recognition-emacslisten
;;
;; - Create Org tasks
;;   https://sachachua.com/dotemacs#writing-and-editing-speech-recognition-emacslisten-create-org-tasks
;;
;; - Speech shortcuts with phony
;;   https://sachachua.com/dotemacs#writing-and-editing-speech-recognition-emacslisten-speech-shortcuts
;;
;; - Trackers
;;   https://sachachua.com/dotemacs#writing-and-editing-speech-recognition-emacslisten-trackers
;;
;; - gptel and phony
;;   https://sachachua.com/dotemacs#writing-and-editing-speech-recognition-emacslisten-gptel-and-phony
;;
;; - OBS voice commands
;;   https://sachachua.com/dotemacs#writing-and-editing-speech-recognition-emacslisten-obs-voice-commands
;;
;; - Numbers in French
;;   https://sachachua.com/dotemacs#writing-and-editing-speech-recognition-emacslisten-numbers-in-french
;;
;; - Laser pointer
;;   https://sachachua.com/dotemacs#streaming-and-videos-laser-pointer
;;
;;; Code:



;; [[file:../Sacha.org::#writing-and-editing-speech-recognition-emacslisten][Emacslisten:1]]
(require 'phony-parser)

(define-error 'sacha-phony-not-parsed "Sentence not parsed." 'error)

(defun sacha-phony-parser--sanitize (s)
	"Lowercase and remove punctuation."
	(string-trim (replace-regexp-in-string "[,\\.!?:]" "" (downcase s))))

;;;###autoload
(defun sacha-phony-compare-literal (a b)
  "Ignore case and ending punctuation."
  (string= (sacha-phony-parser--sanitize a)
				   (sacha-phony-parser--sanitize b)))

;;;###autoload
(defun sacha-phony-evaluate (sentence)
	"Evaluate SENTENCE.
Throw 'not-parsed if not parsed.
Based on `phony-test--evaluate'."
	(let ((parses (phony-parser--parse-utterance sentence)))
		(unless parses
			(signal 'sacha-phony-not-parsed sentence))
		(phony--evaluate-ast (caar parses))))

;;;###autoload
(defun sacha-whisper-phony (text)
  "Evaluate TEXT using phony.el definitions.
Lower-case the first letter of TEXT to match command definitions.
Useful in `speech-input-commands'."
  (sacha-phony-evaluate
	 (concat (downcase (substring text 0 1))
					 (if (> (length text) 1) (substring text 1) "")))
  "")

(phony-defun hello-world ("hello" "world")
	(sacha-whisper-audio-feedback "Hello too"))
;; Emacslisten:1 ends here

;; [[file:../Sacha.org::#writing-and-editing-speech-recognition-emacslisten-create-org-tasks][Create Org tasks:1]]
(phony-define-open-rule date)
(phony-defun in-minutes ("in" (number phony-stdlib--number-component) "minutes")
	:contributes-to date
	(format-time-string
	 "%Y-%m-%d %H:%M"
	 (seconds-to-time
		(+ (time-to-seconds)
			 (* number 60)))))
(phony-defun in-hours ("in" (number phony-stdlib--number-component) "hours")
	:contributes-to date
	(format-time-string
	 "%Y-%m-%d %H:%M"
	 (seconds-to-time
		(+ (time-to-seconds)
			 (* number 60 60)))))
(phony-defun in-minute ("in" (number phony-stdlib--number-component) "minute")
	:contributes-to date
	(format-time-string
	 "%Y-%m-%d %H:%M"
	 (seconds-to-time
		(+ (time-to-seconds)
			 (* number 60)))))
(phony-defun in-hour ("in" (number phony-stdlib--number-component) "hour")
	:contributes-to date
	(format-time-string
	 "%Y-%m-%d %H:%M"
	 (seconds-to-time
		(+ (time-to-seconds)
			 (* number 60 60)))))

(phony-defun today "today" :contributes-to date (org-read-date nil nil "."))
(phony-defun tomorrow "tomorrow" :contributes-to date (org-read-date nil nil "+1"))
(phony-defun monday "Monday" :contributes-to date (org-read-date nil nil "+mon"))
(phony-defun tuesday "Tuesday" :contributes-to date (org-read-date nil nil "+tue"))
(phony-defun wednesday "Wednesday" :contributes-to date (org-read-date nil nil "+wed"))
(phony-defun thursday "Thursday" :contributes-to date (org-read-date nil nil "+thu"))
(phony-defun friday "Friday" :contributes-to date (org-read-date nil nil "+fri"))
(phony-defun saturday "Saturday" :contributes-to date (org-read-date nil nil "+sat"))
(phony-defun sunday "Sunday" :contributes-to date (org-read-date nil nil "+sun"))
(phony-defun next-week ("next" "week") :contributes-to date (org-read-date nil nil "+7"))
(phony-defun next-month ("next" "month") :contributes-to date (org-read-date nil nil "+1m"))
(phony-defun aujourdhui "aujourd-hui" :contributes-to date (org-read-date nil nil "."))
(phony-defun demain "demain" :contributes-to date (org-read-date nil nil "+1"))
(phony-defun la-semaine-prochaine ("la" "semaine" "prochaine") :contributes-to date (org-read-date nil nil "+7"))

(defun sacha-org-capture-scheduled-task (text &optional date)
	"Create an inbox task for TEXT on DATE.
If DATE is t, clock in immediately."
	(let* ((org-capture-templates
					(list
					 (append
						(function sacha-org-goto-inbox-target)
							,(format
								"* TODO %%i\n%s:PROPERTIES:\n:CREATED: %%U\n:END:\n%s\n# %s%s\n"
								(if (stringp date)
										(format "SCHEDULED: <%s>\n"
														(format-time-string
														 (if (string-match ":" date)
																 (cdr org-timestamp-formats)
															 (car org-timestamp-formats))
														 (date-to-time date)))
									"")
								(replace-regexp-in-string "%" "%%" (sacha-org-all-links-from-context))
								(if (get-text-property 0 'file text)
										(concat
										 (org-link-make-string (concat "audio:" (get-text-property 0 'file text))
																					 "▶️") " ")
									"")
								(org-link-make-string (concat "file:" (sacha-screenshot-current-screen))
																			"screenshot"))
							:immediate-finish t
							:prepend t)
						(when (eq date t)
							(list :clock-in t
										:clock-keep t)))))
				 (sacha-org-quick-task t))
		(org-capture-string text "s")
		""))

(phony-defun remind-me ("remind" "me" (? (date date)) (? "to") (? "that") (text phony--builtin-phrase))
	"Capture a scheduled task."
	(sacha-org-capture-scheduled-task text date))
(phony-defun rappele-moi ("rappele-moi" (? (date date)) (? "de") (? "que") (text phony--builtin-phrase))
	"Capture a scheduled task."
	(sacha-org-capture-scheduled-task text date))
(phony-defun rappelez-moi ("rappelez-moi" (? (date date)) (? "de") (? "que") (text phony--builtin-phrase))
	"Capture a scheduled task."
	(sacha-org-capture-scheduled-task text date))
(phony-defun switch-to ("switch" "to" (text phony--builtin-phrase))
	"Switch to this task."
	(sacha-org-capture-scheduled-task text t))
;; Create Org tasks:1 ends here

;; [[file:../Sacha.org::#writing-and-editing-speech-recognition-emacslisten-create-org-tasks][Create Org tasks:2]]
(phony-defun rephrase ("rephrase" (text phony--builtin-phrase))
	"Delete this sentence and reinsert it."
	(save-excursion
		(when (looking-back sentence-end-base (1- (point))) (backward-char))
		(let ((bounds (bounds-of-thing-at-point 'sentence)))
			(delete-region (car bounds) (cdr bounds))
			(insert (upcase (substring text 0 1)) (substring text 1)))))

(phony-defun retake ("retake" (text phony--builtin-phrase))
	"Replace the recording for this sentence."
	(save-excursion
		(when (looking-back sentence-end-base (1- (point))) (backward-char))
		(let ((bounds (bounds-of-thing-at-point 'sentence)))
			(set-text-properties
			 (car bounds)
			 (cdr bounds)
			 `(file ,(get-text-property 0 'file text)
							time-elapsed ,(get-text-property 0 'time-elapsed text)
							orig-duration ,(get-text-property 0 'orig-duration text)
							ratio ,(get-text-property 0 'ratio text))))))

;; Create Org tasks:2 ends here

;; [[file:../Sacha.org::#writing-and-editing-speech-recognition-emacslisten-create-org-tasks][Create Org tasks:3]]
(defun sacha-doctor (input)
      "Get the doctor response to INPUT."
      (with-current-buffer (get-buffer-create "*doctor*")
	      (unless (derived-mode-p 'doctor-mode) (doctor-mode))
	      (insert input "\n\n")
	      (let ((pos (point)))
		      (doctor-read-print)
		      (string-trim (buffer-substring pos (point-max))))))

(defvar sacha-doctor-tts nil "Non-nil means say the response out loud.")
(phony-defun doctor ("doctor" (text phony--builtin-phrase))
      "Chat with Eliza."
      (let ((response (sacha-doctor text)))
	      (message "%s" response)
	      (when sacha-doctor-tts
		      (sacha-whisper-audio-feedback response))
	      response))

(defun sacha-whisper-send-after-transcription (text)
      "Send the response."
      (cond
       ((derived-mode-p 'doctor-mode)
	      (doctor-ret-or-read 1)
	      (doctor-ret-or-read 1))
       (gptel-mode
	      (gptel-send)))
      text)

(defun sacha-whisper-doctor-speak-output (fn &rest args)
      (let ((pos (point)))
	      (apply fn args)
	      (when sacha-whisper-conversation-mode
		      (sacha-whisper-audio-feedback (buffer-substring pos (point))))))

(defun sacha-whisper-gptel-speak-output (beg end)
"Speak the output.
Set `learn-lang-language' and `learn-lang-tts-function' as needed."
(learn-lang-tts-say (buffer-substring beg end)))

;;;###autoload
(define-minor-mode sacha-whisper-conversation-mode ()
      :init-value nil
      (cond
       ((derived-mode-p 'doctor-mode)
	      (if sacha-whisper-conversation-mode
			      (progn
				      (add-hook 'whisper-insert-text-at-point 'sacha-whisper-send-after-transcription 100 t)
				      (advice-add 'doctor-doc :around #'sacha-whisper-doctor-speak-output))
		      (remove-hook 'whisper-insert-text-at-point 'sacha-whisper-send-after-transcription 100 t)
		      (advice-remove 'doctor-doc #'sacha-whisper-doctor-speak-output)))
       (gptel-mode
	      (if sacha-whisper-conversation-mode
			      (progn
				      (add-hook 'whisper-insert-text-at-point 'sacha-whisper-send-after-transcription 100 t)
				      (add-hook 'gptel-post-response-functions 'sacha-whisper-gptel-speak-output 100 t))
		      (remove-hook 'whisper-insert-text-at-point 'sacha-whisper-send-after-transcription)
		      (remove-hook 'gptel-post-response-functions 'sacha-whisper-gptel-speak-output)))))

;; (phony--export-all)
;; (sacha-phony-evaluate "Remind me tomorrow to do the thing")
;; (sacha-phony-evaluate "rappele-moi demain à faire la chose")
;; (sacha-phony-evaluate "doctor I'm trying to get Emacs to talk to me.")
;; (speech-input-commands-process "Remind me next month to verify if this works")
;; Create Org tasks:3 ends here

;; [[file:../Sacha.org::#writing-and-editing-speech-recognition-emacslisten-speech-shortcuts][Speech shortcuts with phony:1]]
(require 'phony)
(eval-when-compile (require 'phony))

(phony-defun undo ("undo") (undo))
(phony-defun redo ("redo") (undo-redo))
(phony-defun winner ("winner") (winner-undo))

(phony-define-open-rule target)
(phony-defun region "region"
	:contributes-to target
	(car (region-beginning)
			 (region-end)))
(phony-defun function "function"
	:contributes-to target
	(cons 'function
				(bounds-of-thing-at-point 'defun)))
(phony-defun sentence "sentence"
	:contributes-to target
	(cons 'sentence
				(bounds-of-thing-at-point 'sentence)))
(phony-defun buffer "buffer"
	:contributes-to target
	(cons 'buffer
				(bounds-of-thing-at-point 'buffer)))
(phony-defun line "line"
	:contributes-to target
	(cons 'line
				(bounds-of-thing-at-point 'line)))
(phony-defun paragraph "paragraph"
	:contributes-to target
	(cons 'paragraph
				(bounds-of-thing-at-point 'paragraph)))
(phony-defun element "element"
	:contributes-to target
	(cons 'element
				(let ((elem (org-element-context)))
					(cons (org-element-begin elem)
								(org-element-end elem)))))

(phony-defun target-items "items"
	:contributes-to target
	(cons 'list
				(let ((list (org-list-struct)))
					(cons (caar list)
								(car (last (car (last list))))))))

(phony-defun subtree "subtree"
	:contributes-to target
	(save-excursion
		(org-back-to-heading)
		(cons 'subtree
					(cons (point)
								(org-end-of-subtree)))))
(phony-defun contents "contents"
	:contributes-to target
	(save-excursion
		(org-back-to-heading)
		(org-end-of-meta-data)
		(cons 'contents
					(cons (point)
								(org-end-of-subtree)))))

(phony-defun subtree ("sub" "tree")
	:contributes-to target
	(save-excursion
		(org-back-to-heading)
		(cons 'subtree
					(cons (point)
								(org-end-of-subtree)))))

(phony-defun eval-target ("run" (target target))
	"Run selected target."
	(eval-region
	 (car (cdr target))
	 (cdr (cdr target)))
	(sacha-whisper-audio-feedback "Run"))

(phony-defun debug-target ("debug" (target target))
	"Edebug selected target."
	(let ((edebug-all-forms t))
		(eval-region
		 (car (cdr target))
		 (cdr (cdr target)))))

(phony-defun pick-target ("pick" (target target))
	"Mark TARGET."
	(sacha-mark-bounds (cdr target)))

(defun sacha-mark-bounds (bounds)
	"Mark BOUNDS."
	(goto-char (car bounds))
	(push-mark)
	(goto-char (cdr bounds))
	(activate-mark))

(defun sacha-mark-list ()
	"Mark the current list."
	(let ((list (org-list-struct)))
		(sacha-mark-bounds (cons (caar list)
														 (car (last (car list)))))))

(phony-defun toggle-list ("toggle" "list")
	(unless (region-active-p)
		(if (org-list-struct)
				(sacha-mark-list)
			(mark-paragraph)))
	(org-toggle-item nil))

(phony-defun open-file ("open" "file")
  "Open file under cursor."
  (ffap))
(phony-defun save ("save")
  "Save buffer."
  (save-buffer))
(phony-defun close ("close")
  "Close current buffer."
  (kill-current-buffer))
(phony-defun switch ("switch" (? (text phony--builtin-phrase)))
  "Switch to buffer."
	(if text
			(setq unread-command-events (listify-key-sequence (concat text (kbd "RET")))))
	(let ((vertico-sort-function 'vertico-sort-length-alpha))
    (call-interactively #'consult-buffer)))

(phony-defun other ("other")
	"Switch to other window."
	(other-window 1))

(phony-defun delete-window ("delete" "window")
	"Delete window."
	(delete-window))
(phony-defun delete-other-window ("delete" "other" "windows")
	"Delete other window."
	(delete-other-windows))
(phony-defun transpose ("transpose")
	"Transpose windows."
	(transpose-windows))

(phony-defun delete-target ("delete" (target target))
	"Delete TARGET."
	(delete-region (cadr target) (cddr target)))
(phony-defun kill-target ("kill" (target target))
	"Kill TARGET."
	(kill-region (cadr target) (cddr target)))

(phony-defun copy-target ("copy" (target target))
	"Copy TARGET."
	(sacha-mark-bounds (cdr target))
	(kill-new (buffer-substring (cadr target) (cddr target))))

(phony-defun next-sentence ("next sentence")
  (forward-sentence))
(phony-defun previous-sentence ("previous sentence")
  (previous-sentence))

(phony-defun next-paragraph ("next paragraph")
  "Go to next paragraph."
  (forward-paragraph))

(phony-defun previous-paragraph ("previous paragraph")
  "Go to previous paragraph."
  (backward-paragraph))
(phony-defun next-heading ("previous heading")
	(org-previous-visible-heading))
(phony-defun previous-heading ("next heading")
	(org-next-visible-heading))
(phony-defun beginning-of-buffer ("top")
  (beginning-of-buffer))
(phony-defun end-of-buffer ("bottom")
  (end-of-buffer))

(phony-defun narrow ("narrow" (target target))
	(narrow-to-region (cadr target)
										(cddr target)))

(phony-defun widen ("widen")
	(widen))
(phony-defun zoom-out ("zoom out")
	(widen))

(phony-define-dictionary on-or-off
  "t or nil."
  `(("yes" . t)
    ("no" . nil)
    ("on" . t)
    ("off" . nil)))

(phony-defun detect-voice-on ("detect voice" (state on-or-off))
	(if state
			(speech-input-vad-start)
		(speech-input-vad-stop))
	(sacha-whisper-audio-feedback (if state "on" "off")))

;; org edit source block
(phony-defun sacha-phony-edit-block ("edit block")
  "Edit source block"
  (org-edit-special))

;; org exit source block
(phony-defun sacha-phony-exit-block ("exit block")
  "Exit source block"
  (org-edit-src-exit))

;; org next source block
(phony-defun sacha-phony-next-src-block ("next block")
  "Jump to next block"
  (org-babel-next-src-block))

;; org previous source block
(phony-defun sacha-phony-previous-src-block ("previous block")
  "Jump to prev block"
  (org-babel-previous-src-block))

(phony-defun sacha-phony-tangle-block ("tangle block")
  "Tangle babel block"
  (let ((current-prefix-arg '(4)))
    (call-interactively #'org-babel-tangle)))

;; org babel tangle file
(phony-defun sacha-phony-tangle-file ("tangle file")
  "Tangle current file"
  (when buffer-file-name
    (org-babel-tangle-file buffer-file-name)))

(phony-defun mark-done ("task done")
  "Change TODO state of an item."
  (org-todo "DONE"))
(phony-defun mark-waiting ("task waiting")
  "Change TODO state of an item."
  (org-todo "WAITING"))
(phony-defun mark-started ("task started")
  "Change TODO state of an item."
  (org-todo "STARTED"))


;; implement up/down five lines / sentences / paragraphs
;; Speech shortcuts with phony:1 ends here

;; [[file:../Sacha.org::#writing-and-editing-speech-recognition-emacslisten-trackers][Trackers:1]]
(require 'phony)
(eval-when-compile (require 'phony))
(phony-defun emacs-news ("emacs" "news")
	"Start tracking Emacs News."
	(quantified-track "Emacs | Emacs News")
	(find-file "~/sync/emacs-news/index.org"))
;; Trackers:1 ends here

;; [[file:../Sacha.org::#writing-and-editing-speech-recognition-emacslisten-gptel-and-phony][gptel and phony:1]]
(require 'phony)
(eval-when-compile (require 'phony))
(require 'gptel)
(require 'gptel-rewrite)

;;; Work in progress, doesn't quite work yet

(defun sacha-gptel-rewrite-region (text beg end)
  "Rewrite BEG and END using TEXT."
  (let ((gptel-rewrite-directive 'rewrite)
				(current-prefix-arg text))
		(save-restriction
			(narrow-to-region beg end)
			(gptel--suffix-rewrite (concat text "\n\n" (buffer-substring beg end))))))

(phony-defun rewrite-target ("rewrite" (target target) (text phony--builtin-phrase))
	"Rewrite selected target."
	(sacha-gptel-rewrite-region
	 text
	 (car (cdr target))
	 (cdr (cdr target))))

(phony-defun computer-implement ("computer implement this function")
	"Rewrite and implement this function."
	(let ((bounds (bounds-of-thing-at-point 'defun)))
		(sacha-gptel-rewrite-region "Implement this function."
																(car bounds)
																(cdr bounds))))

(phony-defun computer-insert ("computer" "insert" (text phony--builtin-phrase))
	"Do something with gptel and insert the results."
	(let ((tok
				 (pending-insert (point) "Calling gptel...")))
		(gptel-request (concat gptel-system-prompt "\n" text)
			:callback (lambda (response _info)
									(if (stringp response)
											(pending-finish tok response)
										(pending-cancel tok))))))

(phony-defun computer-continue ("computer" "continue" (text phony--builtin-phrase))
	"Do something with gptel and the context so far and insert the results."
	(let ((tok
				 (pending-insert (point) "Calling gptel...")))
		(gptel-request
				(if (> (- (point) (point-min)) 15000)
						(concat gptel-system-prompt "\n"
										text "\n### Context:\n\n"
										(buffer-string (point-min) (point)))
					text)
			:callback (lambda (response _info)
									(if (stringp response)
											(pending-finish tok response)
										(pending-cancel tok))))))

(phony-defun computer-tell-me ("computer" "tell" "me" (text phony--builtin-phrase))
	"Start or switch to a chat session."
	(with-current-buffer (gptel "*gptel*")
		(insert text)
		(gptel-send)
		(display-buffer (current-buffer))))
;; gptel and phony:1 ends here

;; [[file:../Sacha.org::#writing-and-editing-speech-recognition-emacslisten-obs-voice-commands][OBS voice commands:1]]
(phony-defun break-time ("stepping" "out")
	(obs-websocket-set-scene "Break")
	(sacha-whisper-audio-feedback "See you soon"))
(phony-defun break-time-2 ("break" "time")
	(obs-websocket-set-scene "Break")
	(sacha-whisper-audio-feedback "See you soon"))
(phony-defun im-back ("I'm" "back")
	(obs-websocket-set-scene "Current task and webcam")
	(sacha-whisper-audio-feedback "Welcome back"))
(phony-defun share-my-screen ("share" "my" "screen")
	(obs-websocket-set-scene "Current task and webcam")
	(sacha-whisper-audio-feedback "Shared"))
(phony-defun webcam ("webcam")
	(obs-websocket-set-scene "Just webcam"))
;; OBS voice commands:1 ends here

;; [[file:../Sacha.org::#writing-and-editing-speech-recognition-emacslisten-numbers-in-french][Numbers in French:1]]
(phony-define-dictionary sacha-phony-fr-digit
"A number between 0 and 9."
:provides digit
'(("zéro" . 0)
  ("un" . 1)
  ("une" . 1)
  ("deux" . 2)
  ("trois" . 3)
  ("quatre" . 4)
  ("cinq" . 5)
  ("six" . 6)
  ("sept" . 7)
  ("huit" . 8)
  ("neuf" . 9)))

(phony-define-dictionary sacha-phony-fr-teens
"A number between 10 and 19."
      '(("onze" . 11)
  ("douze" . 12)
  ("treize" . 13)
  ("quatorze" . 14)
  ("quinze" . 15)
  ("seize" . 16)
  ("dix sept" . 17)
  ("dix-sept" . 17)
  ("dix huit" . 18)
  ("dix-huit" . 18)
  ("dix neuf" . 19)
  ("dix-neuf" . 19)
	      ("dix" . 10)))

(phony-define-dictionary sacha-phony-fr-tens
"A number between 20 and 90 divisible by 10."
'(("vingt" . 20)
  ("trente" . 30)
  ("quarante" . 40)
  ("cinquante" . 50)
  ("soixante" . 60)
  ("soixante dix" . 70)
  ("quatre vingt" . 80)
  ("quatre vingt dix" . 90)))

(phony-define-dictionary sacha-phony-fr-numbers-scale
"A number 10^(3*n), or 100."
      '(("cent" . 100)
	      ("mille" . 1000)))

(phony-define-dictionary sacha-phony-fr-number-component
"A number that partakes in the utterance of a number."
(append
       (phony-dictionary-alist 'sacha-phony-fr-tens)
       (phony-dictionary-alist 'sacha-phony-fr-teens)
       (phony-dictionary-alist 'sacha-phony-fr-digit)
 (phony-dictionary-alist 'sacha-phony-fr-numbers-scale)))

(defun sacha-phony-fr-parse-number (parts)
"Parse PARTS, a number sequence, into a number.

PARTS corresponds to how the number would be uttered.  E.g.,

(2 1000 5 100 20 3)

parses into 2523."
(setq parts (phony-stdlib--collapse-tens parts))
(let ((scales (seq-map #'cdr (phony-dictionary-alist
			      'sacha-phony-fr-numbers-scale))))
  (seq-doseq (scale scales)
    (setq parts (phony-stdlib--collapse-scale parts scale))))
(string-to-number
 (string-join (seq-map #'number-to-string parts))))

(phony-defun sacha-phony-fr-number-string
  ((+ (? "et") (components sacha-phony-fr-number-component)))
"A string containing a number, parsed from COMPONENTS.

If COMPONENTS contains leading zeroes, the number string does as well."
:interactive nil
(let* ((parts components)
       (initial-zeroes (seq-take-while (apply-partially #'= 0) parts))
       (initial-zero-string (seq-into (seq-map (lambda (_) ?0) initial-zeroes) 'string)))
  (if (= (seq-length initial-zeroes) (seq-length parts))
      initial-zero-string
    (concat initial-zero-string
	    (number-to-string (phony-stdlib--parse-number parts))))))

(phony-defun sacha-phony-fr-number ((number-string sacha-phony-fr-number-string))
"A number.

NUMBER-STRING is parsed by `string-to-number'."
:interactive nil
:provides number
(string-to-number number-string))

;; (phony--export-all)
;; (sacha-phony-evaluate "dix neuf")
;(sacha-phony-evaluate "mille neuf cent quatre vingt dix neuf")
;; Numbers in French:1 ends here

;; [[file:../Sacha.org::#streaming-and-videos-laser-pointer][Laser pointer:2]]
;(with-eval-after-load 'phony
;	(phony-defun laser ("laser") (sacha-highlight-pointer-minor-mode)))
;; Laser pointer:2 ends here

(provide 'sacha-phony)
;;; sacha-phony.el ends here
