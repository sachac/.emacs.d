;;; my-plover.el ---  -*- lexical-binding: t -*-

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
;; - Looking things up
;;   https://sachachua.com/dotemacs#looking-things-up
;;
;;; Code:



;; [[file:../Sacha.org::#looking-things-up][Looking things up:1]]
;;;###autoload
  (defun my-plover-search-dictionary-for-strokes-jq (stroke-regexp)
    (json-parse-string
     (shell-command-to-string
      (format "cat ~/.config/plover/main.json | jq 'with_entries(if (.key|test(\"%s\")) then ( {key: .key, value: .value}) else empty end)'"
	      stroke-regexp))
     :object-type 'alist))
(defvar my-plover-main-dict
    (if (and my-laptop-p (file-exists-p "~/.config/plover/main.json"))
	      (mapcar (lambda (o) (cons (symbol-name (car o)) (cdr o)))
		(json-read-file "~/.config/plover/main.json"))))

;;;###autoload
  (defun my-plover-search-dictionary-for-strokes (stroke-regexp)
    (interactive "MStroke regexp: ")
    (let ((results (seq-filter (lambda (o) (string-match stroke-regexp (car o))) my-plover-main-dict)))
      (when (called-interactively-p 'any) (my-plover-display-dictionary-results results))
      results))
  (defvar my-plover-dict-cache nil "Alist of (filename . ((stroke . translation) ...))")
  (defvar my-plover-home "~/.config/plover")
;;;###autoload
  (defun my-plover-dict (&optional filename)
    (setq filename (expand-file-name (or filename "main.json") my-plover-home))
    (or (cdr (assoc-default filename my-plover-dict-cache))
	(let ((result (mapcar (lambda (o) (cons (symbol-name (car o)) (cdr o))) (json-read-file filename))))
	  (push (cons filename result) my-plover-dict-cache )
	  result)))

;;;###autoload
  (defun my-plover-search-dictionary-for-translation (translation &optional start file)
    (interactive "MTranslation: \nP")
    (let* ((regexp (concat "^" (regexp-quote translation) (unless start "$")))
	   (results (seq-filter (lambda (o) (string-match regexp (cdr o))) (my-plover-dict file))))
      (when (called-interactively-p 'any) (my-plover-display-dictionary-results results))
      results))

;;;###autoload
  (defun my-plover-display-dictionary-results (results)
    (with-current-buffer (get-buffer-create "*Plover*")
      (erase-buffer)
      (insert (format "%d entries\n" (length results))
	      (mapconcat (lambda (o) (format "%s\t%s" (car o) (cdr o))) results "\n"))
      (goto-char (point-min))
      (display-buffer (current-buffer))))

  (defmacro my-with-plover-fingerspelling (&rest body)
    `(progn
       (plover-websocket-send :translation "{PLOVER:SOLO_DICT:+commands.json,+fingerspelling.json}")
       (prog1 (progn ,@body)
	 (plover-websocket-send :translation "{PLOVER:END_SOLO_DICT}"))))

;;;###autoload
  (defun my-consult-plover-read-stroke-or-translation ()
    (interactive)
    (let ((dict (mapcar (lambda (o) (cons (format "%s: %s" (car o) (cdr o)) o))
			(my-plover-dict))))
      (my-with-plover-fingerspelling
       (consult--read
	dict
	:prompt "Strokes/translation: "
	:category 'plover-stroke))))

;;;###autoload
  (defun my-consult-plover-and-execute-strokes (choice)
    (interactive (list (my-consult-plover-read-stroke-or-translation)))
    (when (string-match "^\\([^ ]+\\): \\(.+\\)" choice)
      (plover-websocket-send :translation (match-string 2 choice) :force t :zero_last_stroke_length t)))

;;;###autoload
  (defun my-consult-plover-search-strokes (regexp solo-p)
    (interactive (list (with-plover-plain (read-string "Strokes: ")) current-prefix-arg))
    (consult--read
     (mapcar (lambda (o) (cons (format "%s: %s" (car o) (cdr o)) o))
	     (my-plover-search-dictionary-for-strokes (if solo-p (concat "^" regexp "\\(?:/\\|$\\)" ) (concat "^" regexp))))
     :prompt "Narrow: "))



  ;; (list
  ;;  (benchmark-run 2 (my-plover-search-dictionary-for-strokes-jq "^THER"))
  ;;  (benchmark-run 2 (my-plover-search-dictionary-for-translation "stenography" t "typey-type.json")
  ;; (benchmark-run 2 (my-plover-search-dictionary-for-translation "stenography" t))
  ;;  (benchmark-run 2 (my-plover-search-dictionary-for-strokes "^THER/")))

;; Looking things up:1 ends here

(provide 'my-plover)
;;; my-plover.el ends here
