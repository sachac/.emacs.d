;;; sacha-plover.el ---  -*- lexical-binding: t -*-

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



;; [[file:../Sacha.org::#looking-things-up][Looking things up:1]]
;;;###autoload
  (defun sacha-plover-search-dictionary-for-strokes-jq (stroke-regexp)
    (json-parse-string
     (shell-command-to-string
      (format "cat ~/.config/plover/main.json | jq 'with_entries(if (.key|test(\"%s\")) then ( {key: .key, value: .value}) else empty end)'"
	      stroke-regexp))
     :object-type 'alist))
(defvar sacha-plover-main-dict
    (if (and sacha-laptop-p (file-exists-p "~/.config/plover/main.json"))
	      (mapcar (lambda (o) (cons (symbol-name (car o)) (cdr o)))
		(json-read-file "~/.config/plover/main.json"))))

;;;###autoload
  (defun sacha-plover-search-dictionary-for-strokes (stroke-regexp)
    (interactive "MStroke regexp: ")
    (let ((results (seq-filter (lambda (o) (string-match stroke-regexp (car o))) sacha-plover-main-dict)))
      (when (called-interactively-p 'any) (sacha-plover-display-dictionary-results results))
      results))
  (defvar sacha-plover-dict-cache nil "Alist of (filename . ((stroke . translation) ...))")
  (defvar sacha-plover-home "~/.config/plover")
;;;###autoload
  (defun sacha-plover-dict (&optional filename)
    (setq filename (expand-file-name (or filename "main.json") sacha-plover-home))
    (or (cdr (assoc-default filename sacha-plover-dict-cache))
	(let ((result (mapcar (lambda (o) (cons (symbol-name (car o)) (cdr o))) (json-read-file filename))))
	  (push (cons filename result) sacha-plover-dict-cache )
	  result)))

;;;###autoload
  (defun sacha-plover-search-dictionary-for-translation (translation &optional start file)
    (interactive "MTranslation: \nP")
    (let* ((regexp (concat "^" (regexp-quote translation) (unless start "$")))
	   (results (seq-filter (lambda (o) (string-match regexp (cdr o))) (sacha-plover-dict file))))
      (when (called-interactively-p 'any) (sacha-plover-display-dictionary-results results))
      results))

;;;###autoload
  (defun sacha-plover-display-dictionary-results (results)
    (with-current-buffer (get-buffer-create "*Plover*")
      (erase-buffer)
      (insert (format "%d entries\n" (length results))
	      (mapconcat (lambda (o) (format "%s\t%s" (car o) (cdr o))) results "\n"))
      (goto-char (point-min))
      (display-buffer (current-buffer))))

  (defmacro sacha-with-plover-fingerspelling (&rest body)
    `(progn
       (plover-websocket-send :translation "{PLOVER:SOLO_DICT:+commands.json,+fingerspelling.json}")
       (prog1 (progn ,@body)
	 (plover-websocket-send :translation "{PLOVER:END_SOLO_DICT}"))))

;;;###autoload
  (defun sacha-consult-plover-read-stroke-or-translation ()
    (interactive)
    (let ((dict (mapcar (lambda (o) (cons (format "%s: %s" (car o) (cdr o)) o))
			(sacha-plover-dict))))
      (sacha-with-plover-fingerspelling
       (consult--read
	dict
	:prompt "Strokes/translation: "
	:category 'plover-stroke))))

;;;###autoload
  (defun sacha-consult-plover-and-execute-strokes (choice)
    (interactive (list (sacha-consult-plover-read-stroke-or-translation)))
    (when (string-match "^\\([^ ]+\\): \\(.+\\)" choice)
      (plover-websocket-send :translation (match-string 2 choice) :force t :zero_last_stroke_length t)))

;;;###autoload
  (defun sacha-consult-plover-search-strokes (regexp solo-p)
    (interactive (list (with-plover-plain (read-string "Strokes: ")) current-prefix-arg))
    (consult--read
     (mapcar (lambda (o) (cons (format "%s: %s" (car o) (cdr o)) o))
	     (sacha-plover-search-dictionary-for-strokes (if solo-p (concat "^" regexp "\\(?:/\\|$\\)" ) (concat "^" regexp))))
     :prompt "Narrow: "))



  ;; (list
  ;;  (benchmark-run 2 (sacha-plover-search-dictionary-for-strokes-jq "^THER"))
  ;;  (benchmark-run 2 (sacha-plover-search-dictionary-for-translation "stenography" t "typey-type.json")
  ;; (benchmark-run 2 (sacha-plover-search-dictionary-for-translation "stenography" t))
  ;;  (benchmark-run 2 (sacha-plover-search-dictionary-for-strokes "^THER/")))

;; Looking things up:1 ends here

;; [[file:../Sacha.org::#running-plover-drills-from-emacs][Running Plover drills from Emacs:1]]
(defvar sacha-plover-drills
  (append
   (mapcar (lambda (desc)
             (cons desc (concat "https://joshuagrams.github.io/steno-jig/learn-keyboard.html?drill=" (url-encode-url (replace-regexp-in-string "\\+" "%2B" desc)))))
           '("Left hand, bottom row"
             "Right hand, bottom row"
             "Left hand, top row"
             "Right hand, top row"
             "Right hand, full bottom row"
             "Right hand, full top row"
             "Vowels"
             "Left hand"
             "Right hand"
             "All keys"
             "Left + Right"
             "Left + Vowel"
             "Vowel + Right"
             "Left + Vowel + Right"
             "Columns: D, B, L, -N"
             "x"))
   (mapcar (lambda (desc)
             (cons desc (concat "https://joshuagrams.github.io/steno-jig/learn-plover.html?hints=yes&type=randomly&timeLimit=2&drill=" (url-encode-url (replace-regexp-in-string "\\+" "%2B" desc)))))
           '("One Syllable Words" "Consonant Clusters" "Where's the TRUFT?" "Dropping Unstressed Vowels" "Inversion" "The Fifth Vowel Key" "Long Vowel Chords" "Diphthong Chords" "Vowel Disambiguator Chords" "The Missing Keys" "The Remaining Missing Letters" "Review Through Missing Letters" "Digraphs" "Review Through Digraphs" "Common Compound Clusters" "Review Through Common Compound Clusters" "Common Briefs 1-20" "Common Briefs 21-40" "Common Briefs 41-60" "Common Briefs 61-80" "Common Briefs 81-100"))))

(defvar sacha-plover-drill-history nil "Previous drills")
(defvar sacha-plover-drill-file "~/proj/plover-notes/README.org")

;;;###autoload
(defun sacha-plover-stenojig-custom-drill (words)
  (interactive "MWords: ")
  (plover-websocket-resume-plover)
  (unwind-protect
    (progn
    (browse-url-chrome (concat "file:///home/sacha/vendor/steno-jig/from-url.html?go=true&type=randomly&timeLimit=2&name=test&hints=true&drillItems=" (url-encode-url words)))
    (read-string "Ignore this: "))
  (plover-websocket-suspend-plover)))

;;;###autoload
(defun sacha-plover-drill (drill)
  "Run a single Plover keyboard drill and capture stats in an Org table."
  (interactive (list (consult--read sacha-plover-drills :prompt "Drill: " :sort nil
                                    :history sacha-plover-drill-history
                                    :default (car sacha-plover-drill-history))))
  (unless (string= (downcase (string-trim drill)) "x")
    (let ((url (assoc-default drill sacha-plover-drills)))
      (plover-websocket-resume-plover)
      (when (string-match "learn-keyboard" url)
        (plover-websocket-send :translation "{PLOVER:TOGGLE_DICT:-main.json,-user.json}"))
      (switch-to-buffer (find-file sacha-plover-drill-file))
      (goto-char (point-min))
      (re-search-forward "#\\+NAME: drill\n")
      (insert (format "| %s | %s |  |\n"
                      (org-link-make-string url drill)
                      (format-time-string "[%Y-%m-%d %a %H:%M]")))
      (backward-char 3)
      (browse-url url)
      (read-string "Ignore this: ")
      (when (string-match "learn-keyboard" url)
        (plover-websocket-send :translation "{PLOVER:TOGGLE_DICT:+main.json,+user.json}"))
      (insert (read-string (format "Time (%s): " (string-join (reverse (sacha-plover-recent-stats drill)) ", "))))
      (end-of-line)
      (forward-char 1)
      t)))

;;;###autoload
(defun sacha-plover-recent-stats (drill-name)
  (mapcar
   (lambda (o) (substring-no-properties (elt o 2)))
   (seq-take
    (sort (seq-filter (lambda (o) (string-match (regexp-quote drill-name) (car o)))
	                    (org-with-wide-buffer
                       (save-excursion
                         (goto-char (point-min))
                         (if (re-search-forward "#\\+NAME: drill\n" nil t)
														(org-table-to-lisp)))))
          (lambda (a b) (string< (string-trim (elt b 1))
                                 (string-trim (elt a 1)))))
    3)))

;;;###autoload
 (defun sacha-plover-drilling-time ()
   "Keep drilling Plover.
Restore main dictionary and turn off Plover when done."
   (interactive)
   (quantified-track "Steno")
   (call-process "wmctrl" nil 0 nil "-i" "-a" (number-to-string (sacha-wmctl-get-id "emacs")))
   (while (sacha-plover-drill (consult--read sacha-plover-drills :prompt "Drill: " :sort nil
                                          :history 'sacha-plover-drill-history
                                          :default (car sacha-plover-drill-history)))))
;; Running Plover drills from Emacs:1 ends here

;; [[file:../Sacha.org::#making-it-easier-to-execute-commands][Making it easier to execute commands:3]]
;;;###autoload
(defun sacha-plover-process-inbox-entries ()
  (interactive)
  (catch 'exit
    (while t
      (plover-websocket-send :stroke '["K-" "P-" "A-" "*"])
      (sacha-read-command-string
       (lambda () (concat (org-get-heading t t t t) ": "))
       '(("replace and post"
          (lambda () (interactive)
            (call-interactively 'sacha-org-replace-heading)
            (call-interactively 'sacha-org-mark-done-and-add-to-journal)
            (org-forward-heading-same-level 1)))
         ("edit" sacha-org-replace-heading)
         ("post" sacha-org-mark-done-and-add-to-journal)
         ("refile" org-refile)
         ("to do" org-todo)
         ("next" org-forward-heading-same-level)
         ("open link" (lambda () (interactive)
                        (save-excursion
                          (when (re-search-forward org-link-any-re nil t)
                            (goto-char (match-beginning 0))
                            (org-open-at-point)))))
         ("yesterday" (lambda ()  (interactive)
                        (save-excursion
                          (re-search-forward org-element--timestamp-regexp)
                          (goto-char (match-beginning 0))
                          (org-timestamp-down-day))))
         ("previous" org-backward-heading-same-level)
         ("new journal" sacha-journal-post)
         ("practice" (lambda () (interactive) (quantified-track "steno") (browse-url "https://didoesdigital.com/typey-type/progress")))
         ("lowercase" downcase-word)
         ("capitalize" capitalize-dwim)
         ("clean" sacha-org-clean-up-inbox)
         ("replace heading" sacha-org-replace-heading)
         ("cut subtree" org-cut-subtree)
         ("export subtree to 11ty" (lambda () (interactive) (org-11ty-export-to-11ty t t)))
         ("exit" (throw 'exit nil)))
       (lambda (input)
         (sacha-org-replace-heading input)
         (call-interactively 'sacha-org-mark-done-and-add-to-journal)
         (org-forward-heading-same-level 1))
       t))))

(defmacro sacha-read-command-string (prompt commands default-fn &optional include-commands)
  (declare (debug t))
  `(let* ((command
           (consult--read
            (append ,commands
                    (if ,include-commands
                        (let (res)
                          (mapatoms
                           (lambda (o)
                             (when (commandp o) (push (symbol-name o) res))))
                          res)))
            :prompt (cond
                     ((functionp ,prompt) (funcall ,prompt))
                     ((stringp ,prompt) ,prompt)
                     (t "Command: "))
            :category 'function
            :sort nil))
          (entry (assoc-default command ,commands)))
     (cond
      ((and entry (listp (car entry)))
       (if (functionp (car entry))
           (funcall (car entry))
         (eval (car entry) t)))
      (entry (call-interactively (car entry)))
      ((commandp (intern command)) (call-interactively (intern command)))
      ((functionp ,default-fn) (funcall ,default-fn command)))))

;;;###autoload
(defun sacha-read-commands ()
  (interactive)
  (cond
   ((derived-mode-p 'org-mode)
    (sacha-plover-process-inbox-entries))
   ((derived-mode-p 'subed-mode)
    (sacha-plover/edit-subtitles))))

;; Making it easier to execute commands:3 ends here

;; [[file:../Sacha.org::#suggesting-briefs][Suggesting briefs:1]]
;;;###autoload
(defun sacha-plover-briefpedia (translation)
  (interactive "MTranslation: ")
  (with-current-buffer (url-retrieve-synchronously (concat "http://briefpedia.com/AjaxTables3.php?search=" (url-encode-url translation)))
    (goto-char (point-min))
    (re-search-forward "^$")
    (while (re-search-forward "</?\\(th\\)[ >]" nil t)
			(replace-match "td" nil nil nil 1))
		(goto-char (point-min))
    (re-search-forward "^$")
    (save-excursion
      (insert "<div>")
      (goto-char (point-max)) (insert "</div>"))
    (let* ((data (xml-parse-region (point-min) (point-max)))
           (entries (mapcar (lambda (o) (string-trim (dom-text o))) (dom-by-tag (dom-by-id data "divEnglishTable") 'a)))
           (conflicts (seq-group-by 'car
                                    (mapcar (lambda (row) (mapcar (lambda (cell) (string-trim (dom-texts cell))) (dom-by-tag row 'td)))
                                            (cdr (dom-by-tag (dom-by-id data "divCrossTable") 'tr)))))
					 (result
						(mapcar (lambda (entry) (cons entry (mapcar 'cadr (assoc-default entry conflicts)))) entries)))
			(when (called-interactively-p 'any)
				(message "%s"
								 (mapconcat (lambda (entry)
															(concat (car entry)
																			(if (cdr entry)
																					(concat " ("
																									(string-join (cdr entry) ", ")
																									")")
																				"")))
														result
														"; ")))
			result)))

;;;###autoload
(defun sacha-plover-read-outline-for-brief (base-prompt)
  (let* ((prompt (or base-prompt "Outline: "))
         new-brief
         (brief (with-plover-plain (read-string prompt)))
         (sacha-conflicts (sacha-plover-check-for-conflict brief)))
    (while sacha-conflicts
      (setq prompt (format "%s%s conflicts %s (alt: %s): "
                           (if base-prompt (concat base-prompt "\n") "")
                           brief (car sacha-conflicts) (string-join (cdr sacha-conflicts) ", ")))
      (setq new-brief (with-plover-plain (read-string prompt)))
      (if (string= new-brief "")
          (setq sacha-conflicts nil)
        (setq brief new-brief)
        (setq sacha-conflicts (sacha-plover-check-for-conflict brief))))
    brief))

;;;###autoload
(defun sacha-plover-brief-with-check (translation)
  (interactive "MTranslation: ")
  (setq translation (string-trim translation))
  (let ((brief (sacha-plover-read-outline-for-brief (format "Outline for %s: " translation))))
    (when brief
      (kill-new (format "| %s | %s |" brief translation))
      (plover-websocket-add-translation brief translation))))

;;;###autoload
(defun sacha-plover-briefpedia-suggest (translation)
  (interactive "MTranslation: ")
  (setq translation (string-trim translation))
  (let* ((entries (sacha-plover-briefpedia translation))
         (current (sacha-plover-search-dictionary-for-translation translation))
         (brief
          (sacha-plover-read-outline-for-brief
           (concat
            (if current (format "Current: %s\n" (mapconcat 'car current "; ")) "")
            (if entries
                (concat (mapconcat
                         (lambda (entry)
                           (let ((dict-conflict (sacha-plover-check-for-conflict (car entry))))
                             (cond
                              ((and (cdr entry) dict-conflict)
                               (format "%s - dict conflict: %s (%s)\nbrief conflict: %s"
                                       (car entry)
                                       (car dict-conflict)
                                       (string-join (cdr dict-conflict) "; ")
                                       (string-join (cdr entry) "; ")))
                              ((cdr entry)
                               (format "%s - brief conflict: %s"
                                       (car entry)
                                       (string-join (cdr entry) "; ")))
                              (t (car entry)))))
                         entries
                         "\n")
                        "\nOutline: ")
              "No suggestions. Outline: ")))))
    (when brief
      (kill-new (format "| %s | %s |" brief translation))
      (plover-websocket-add-translation brief translation))))

;;;###autoload
(defun sacha-plover-check-for-conflict (outline)
  (let* ((case-fold-search nil)
         (translation (cdar (sacha-plover-search-dictionary-for-strokes (concat "^" outline "$"))))
         (alternatives (and translation (sacha-plover-search-dictionary-for-translation translation))))
    (if translation (cons translation (mapcar 'car alternatives)))))
;; Suggesting briefs:1 ends here

;; [[file:../Sacha.org::#plover_clippy_buffer][Using inotify to add Plover Clippy suggestions into Emacs:1]]
(defvar sacha-clippy-recent-suggestions nil "Recent suggestions, limited by `sacha-clippy-recent-suggestions-limit`.")
(defvar sacha-clippy-recent-suggestions-limit nil "If non-nil, keep this many suggestions.")
(defvar sacha-clippy-extra-notes nil "Extra notes to add at the end.")
;;;###autoload
(defun sacha-clippy-last ()
  (let ((value (string-trim (shell-command-to-string "tail -1 ~/.config/plover/clippy.txt | cut -c 23-"))))
    (when (string-match "^\\(.*?\\)[ \t]+|| .*? -> \\(.+\\)" value)
      (cons (match-string 1 value) (match-string 2 value)))))

;;;###autoload
(defun sacha-clippy-show (&rest _)
  (interactive)
  (with-current-buffer (get-buffer-create "*Clippy*")
    (let ((last (sacha-clippy-last)))
      (if sacha-clippy-recent-suggestions-limit
          (progn
            (unless (equal last (car sacha-clippy-recent-suggestions))
              (setq sacha-clippy-recent-suggestions (seq-take (cons last sacha-clippy-recent-suggestions) sacha-clippy-recent-suggestions-limit)))
            (erase-buffer)
            (insert (mapconcat (lambda (o) (format "| %s | %s |\n"  (car o) (cdr o))) sacha-clippy-recent-suggestions "")))
        (unless (equal last (car sacha-clippy-recent-suggestions))
          (setq sacha-clippy-recent-suggestions (cons last sacha-clippy-recent-suggestions))
          (goto-char (point-min))
          (insert (format "| %s | %s |\n" (car last) (cdr last))))))
    (when (get-buffer-window (current-buffer))
      (set-window-point (get-buffer-window (current-buffer)) (point-min)))))

;;;###autoload
(defun sacha-insert-symbol (symbol-name)
  (interactive (list
                (let ((orig-buffer (current-buffer)))
                  (completing-read
                   "Insert symbol: "
                   #'help--symbol-completion-table
                   (lambda (vv)
                     ;; In case the variable only exists in the buffer
                     ;; the command we switch back to that buffer before
                     ;; we examine the variable.
                     (with-current-buffer orig-buffer
                       (or (get vv 'variable-documentation)
                           (functionp vv)
                           (and (boundp vv) (not (keywordp vv))))))))))
  (insert symbol-name))

;;;###autoload
(defun sacha-insert-variable-value (symbol-name)
  (interactive (list
                (let ((orig-buffer (current-buffer)))
                  (completing-read
                   "Insert variable: "
                   #'help--symbol-completion-table
                   (lambda (vv)
                     ;; In case the variable only exists in the buffer
                     ;; the command we switch back to that buffer before
                     ;; we examine the variable.
                     (with-current-buffer orig-buffer
                       (or (get vv 'variable-documentation)
                           (and (boundp vv) (not (keywordp vv))))))))))
  (insert (symbol-value (intern symbol-name))))

;;;###autoload
(defun sacha-insert-function (symbol-name)
	"Insert function name."
  (interactive (list
                (completing-read
                 "Insert function: "
                 #'help--symbol-completion-table
                 'functionp)))
  (insert symbol-name))

(defvar sacha-clippy-monitor nil)
;;;###autoload
(defun sacha-clippy-toggle-monitor ()
  (interactive)
  (if (inotify-valid-p sacha-clippy-monitor)
      (progn
        (message "Turning off")
        (inotify-rm-watch sacha-clippy-monitor))
    (message "Turning on")
    (setq sacha-clippy-monitor
          (inotify-add-watch
           (expand-file-name "~/.config/plover/clippy.txt") 'modify
           #'sacha-clippy-show))))
;; Using inotify to add Plover Clippy suggestions into Emacs:1 ends here

;; [[file:../Sacha.org::#stenoing-interface][Stenoing interface:1]]

(defvar sacha-plover-quick-notes "~/proj/plover-notes/scratch.org")
(defvar sacha-plover-current-stroke-buffer "*Current stroke*")
;;;###autoload
(defun sacha-plover-add-note (string)
  (interactive "MNote: ")
  (with-current-buffer (find-file-noselect sacha-plover-quick-notes)
    (goto-char (point-min))
    (insert string)
    (unless (bolp) (insert "\n"))))

;;;###autoload
(defun sacha-plover-add-last-clippy-to-notes ()
  (interactive)
  (sacha-plover-add-note (format "| %s | %s |\n" (caar sacha-clippy-recent-suggestions) (cdar sacha-clippy-recent-suggestions))))

;;;###autoload
(defun sacha-plover-scroll-notes ()
  (interactive)
  (message "Hello")
  (when (get-buffer-window (get-file-buffer sacha-plover-quick-notes))
    (with-selected-window (get-buffer-window (get-file-buffer sacha-plover-quick-notes))
      (scroll-up))))

;;;###autoload
(defun sacha-plover-scroll-notes-down ()
  (interactive)
  (message "World")
  (when (get-buffer-window (get-file-buffer sacha-plover-quick-notes))
    (with-selected-window (get-buffer-window (get-file-buffer sacha-plover-quick-notes))
      (scroll-down))))

;;;###autoload
(defun sacha-plover-spectra-last-clippy ()
  (interactive)
  (browse-url (format "http://localhost:8081/?outline=%s&translation=%s"
                      (car (split-string (cdar sacha-clippy-recent-suggestions) ", "))
                      (caar sacha-clippy-recent-suggestions))))

;;;###autoload
(defun sacha-plover-layout-windows ()
  "Organize my windows."
  (interactive)
  (delete-other-windows)
  (when plover-websocket-stroke-buffer-name
    (with-selected-window (split-window-below -4)
      (switch-to-buffer plover-websocket-stroke-buffer-name)))
  (with-selected-window (split-window-right 100)
    (switch-to-buffer (get-buffer-create "*Clippy*"))
    (when sacha-plover-quick-notes
      (with-selected-window (split-window-below 10)
        (switch-to-buffer (find-file sacha-plover-quick-notes))))))

;;;###autoload
(defun sacha-plover-clear-stroke-log ()
  (interactive)
  (with-current-buffer (get-buffer-create plover-websocket-stroke-buffer-name)
    (erase-buffer)))

;; Stenoing interface:1 ends here

;; [[file:../Sacha.org::#coding-with-plover][Coding with Plover:1]]
;;;###autoload
(defun sacha-plover-insert-defun ()
  "Prompt for parts of a function definition."
  (interactive)
  (insert "(defun ")
  (plover-websocket-send :translation "{MODE:LOWER}{MODE:SET_SPACE:-}")
  (insert (replace-regexp-in-string "-$" "" (read-string "Function name: ")))
  (insert " (")
  (plover-websocket-send :translation "{MODE:SET_SPACE: }")
  (let ((args (replace-regexp-in-string "\\<optional\\>" "&optional" (string-trim (read-string "Args: ")))))
    (insert args)
    (insert ")\n")
    (if (y-or-n-p "Interactive? ")
        (if (string= args "")
            (insert "(interactive)\n")
          (insert "(interactive (list))\n"))))
  (plover-websocket-send :translation "{MODE:RESET}{}{-|}")
  (insert (format "\"%s\"\n"
                  (replace-regexp-in-string "\"" "\\\"" (string-trim (read-string "Docstring: ")))))
  (save-excursion (insert ")") (lispy--normalize-1))
  (plover-websocket-send :translation "{MODE:LOWER}"))

;;;###autoload
(defun sacha-plover-insert-defvar ()
  (interactive)
  "Define a variable."
  (insert "(defvar ")
  (plover-websocket-send :translation "{MODE:LOWER}{MODE:SET_SPACE:-}")
  (insert (replace-regexp-in-string "-$" "" (read-string "Variable name: ")))
  (insert " ")
  (plover-websocket-send :translation "{MODE:RESET}{}{-|}")
  (insert (string-trim (read-string "Default value: ")))
  (insert (format " \"%s\")\n"
                  (replace-regexp-in-string "\"" "\\\"" (string-trim (read-string "Docstring: "))))))
;; Coding with Plover:1 ends here

(provide 'sacha-plover)
;;; sacha-plover.el ends here
