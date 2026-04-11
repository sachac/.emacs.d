;;; sacha-transcribe.el ---  -*- lexical-binding: t -*-

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
;; - Recognizing keyword phrases
;;   https://sachachua.com/dotemacs#recognizing-keyword-phrases
;;
;; - Splitting the lines based on keywords and oopses
;;   https://sachachua.com/dotemacs#splitting-the-lines-based-on-keywords-and-oopses
;;
;; - Preparing the VTT subtitles
;;   https://sachachua.com/dotemacs#preparing-the-vtt-subtitles
;;
;; - Formatting the subtitles into Org Mode subtrees
;;   https://sachachua.com/dotemacs#formatting-the-subtitles-into-org-mode-subtrees
;;
;; - Process a single transcript from the raw text file
;;   https://sachachua.com/dotemacs#process-a-single-transcript-from-the-raw-text-file
;;
;; - Process multiple files
;;   https://sachachua.com/dotemacs#process-multiple-files
;;
;; - Updating my audio braindump workflow to take advantage of WhisperX
;;   https://sachachua.com/dotemacs#writing-and-editing-updating-sacha-audio-braindump-workflow-to-take-advantage-of-whisperx
;;
;; - Rerecognize this audio and reprocess it
;;   https://sachachua.com/dotemacs#rerecognize
;;
;; - Remove filler words at the start and upcase the next word
;;   https://sachachua.com/dotemacs#filler-start
;;
;; - Split up oops better
;;   https://sachachua.com/dotemacs#split-up-oops-better
;;
;;; Code:



;; [[file:../Sacha.org::#recognizing-keyword-phrases][Recognizing keyword phrases:1]]
  (defvar sacha-audio-braindump-open-keywords '("start" "begin" "open"))
  (defvar sacha-audio-braindump-close-keywords '("stop" "end" "close"))
  (defvar sacha-audio-braindump-part-keywords '("summary" "chapter" "topic"
                                                                                                                                                          "section"
                                                                                                                                   "action" "idea" "journal" "reminder"
                                                                                                                                   "command" "interruption" "note"
                                                                                                                                   "next step" "next steps" "tags" "tag" "keywords" "keyword"))

  (defvar sacha-audio-braindump-part-keyword-distance-words 2 "Number of words to scan for part keyword.")
  (defvar sacha-audio-braindump-close-keyword-distance-words 50 "number of words to scan for stop keyword.
  Put the keywords on the same line if found.")
;;;###autoload
  (defun sacha-audio-braindump-scan-for-part-keyword (before-part &optional part-keywords within-distance before-distance)
          "Look for BEFORE-PART followed by PART-KEYWORDS.
  There might be WITHIN-DISTANCE words between BEFORE-PART and PART-KEYWORDS,
  and the pair might be within BEFORE-DISTANCE from point.
  Distances are in words.
  Return (start end before-part part) if found, nil otherwise."
          (setq before-part (pcase before-part
                                                                                          ('start sacha-audio-braindump-open-keywords)
                                                                                          ('stop sacha-audio-braindump-close-keywords)
                                                                                          ('nil (append sacha-audio-braindump-open-keywords sacha-audio-braindump-close-keywords))

                                                                                          (_ before-part)))
          (if (stringp before-part) (setq before-part (list before-part)))
          (setq part-keywords (or part-keywords sacha-audio-braindump-part-keywords))
          (when (stringp part-keywords) (setq part-keywords (list part-keywords)))
          (setq within-distance (or within-distance sacha-audio-braindump-part-keyword-distance-words))
          (setq before-distance (if (eq before-distance t)
                                                                                                                  (point-max)
                                                                                                          (or before-distance sacha-audio-braindump-close-keyword-distance-words)))
          (let (result
                                  start end
                                  (before-point (save-excursion (forward-word before-distance) (point)))
                                  before-word
                                  part-word)
                  (save-excursion
                          (when (looking-at (regexp-opt before-part))
                                  (setq before-word (match-string 0) start (match-beginning 0))
                                  (when (re-search-forward (regexp-opt part-keywords) (save-excursion (forward-word within-distance) (point)) t)
                                          (setq result (list start (match-end 0) before-word (match-string 0)))))
                          (while (and (not result)
                                                                          (re-search-forward (regexp-opt before-part) before-point t))
                                  (setq before-word (match-string 0) start (match-beginning 0))
                                  (when (re-search-forward (regexp-opt part-keywords) (save-excursion (forward-word within-distance) (point)) t)
                                          (setq result (list start (match-end 0) before-word (match-string 0)))))
                          (when result (goto-char (elt result 1)))
                          result)))

  (ert-deftest sacha-audio-braindump-scan-for-part-keyword ()
          (with-temp-buffer
                  (insert "some text start a reminder hello world stop there and do something stop reminder more text")
                  (goto-char (point-min))
                  (let ((result (sacha-audio-braindump-scan-for-part-keyword 'start nil)))
                          (expect (elt result 2) :to-equal "start")
                          (expect (elt result 3) :to-equal "reminder"))
                  (let ((result (sacha-audio-braindump-scan-for-part-keyword 'stop "reminder")))
                          (expect (elt result 2) :to-equal "stop")
                          (expect (elt result 3) :to-equal "reminder"))))
;; Recognizing keyword phrases:1 ends here

;; [[file:../Sacha.org::#splitting-the-lines-based-on-keywords-and-oopses][Splitting the lines based on keywords and oopses:1]]
;;;###autoload
  (defun sacha-audio-braindump-prepare-alignment-breaks ()
          "Split lines in preparation for forced alignment with aeneas.

  Split \"oops\" so that it's at the end of the line and the
  previous line starts with roughly the same words as the next
  line, for easier removal.

  Add a linebreak before \"begin/start\" followed by
  `sacha-audio-braindump-part-keywords'.

  Add a linebreak after \"stop\" followed by
  `sacha-audio-braindump-part-keywords'.

  Look for begin keyword ... stop keyword with at most
  `sacha-audio-braindump-part-keyword-distance-words' between them and put them on one
  line. If begin or stop has been misrecognized, try the best guess."
          (interactive)
          (let ((case-fold-search t) result close-result)
                  (sacha-split-oops)
                  ;; break "begin/start keyword"
                  (goto-char (point-min))
                  (while (setq result (sacha-audio-braindump-scan-for-part-keyword 'start nil nil t))
                          (goto-char (car result))
                          (delete-region (car result) (elt result 1))
                          (insert "\n" (upcase (concat (elt result 2) " " (elt result 3))) "\n"))
                  ;; break stop
                  (goto-char (point-min))
                  (while (setq result (sacha-audio-braindump-scan-for-part-keyword 'stop nil nil t))
                          (goto-char (car result))
                          (delete-region (car result) (elt result 1))
                          (insert (upcase (concat (elt result 2) " " (elt result 3))) "\n"))
                  ;; try to get start and end sections on one line
                  (goto-char (point-min))
                  (while (setq result (sacha-audio-braindump-scan-for-part-keyword 'start nil nil t))
                          (goto-char (elt result 1))
                          (setq stop-result (sacha-audio-braindump-scan-for-part-keyword 'stop (elt result 3)))
                          (if stop-result
                                          (progn
                                                  (goto-char (car stop-result))
                                                  (while (re-search-backward " *\n+ *" (car result) t)
                                                          (replace-match " ")))
                                  ;; no stop keyword; is the keyword around? maybe it was just misrecognized
                                  (if (re-search-forward (elt result 3)
                                                                                                                           (save-excursion
                                                                                                                                   (forward-word sacha-audio-braindump-close-keyword-distance-words)
                                                                                                                                   (point))
                                                                                                                           t)
                                                  (save-excursion
                                                          (goto-char (match-beginning 0))
                                                          (save-excursion
                                                                  (insert " STOP "))
                                                          (while (re-search-backward " *\n+ *" (car result) t)
                                                                  (replace-match " ")))
                                          (when (looking-at "\n+ *")
                                                  (replace-match " ")))))
                  ;; Check for stops without starts
                  (goto-char (point-min))
                  (while (setq result (sacha-audio-braindump-scan-for-part-keyword 'stop nil nil t))
                          (goto-char (car result))
                          (save-excursion
                                  (unless (re-search-backward (elt result 3) (line-beginning-position) t)
                                          (when (re-search-backward
                                                                   (elt result 3)
                                                                   (save-excursion (backward-word sacha-audio-braindump-close-keyword-distance-words)
                                                                                                                                   (point))
                                                                   t)
                                                  (replace-match (concat "\nSTART " (elt result 3))))))
                          (goto-char (cadr result)))
                  ;; remove empty lines
                  (goto-char (point-min))
                  (when (looking-at "\n+") (replace-match ""))
                  (while (re-search-forward "\n\n+" nil t)
                          (replace-match "\n"))
                  (goto-char (point-min))
                  (while (re-search-forward " *\n *" nil t)
                          (replace-match "\n"))))

  (ert-deftest sacha-audio-braindump-prepare-alignment-breaks ()
          (with-temp-buffer
                  (insert "some text start a reminder hello world stop there and do something stop reminder more text")
                  (goto-char (point-min))
                  (sacha-audio-braindump-prepare-alignment-breaks)
                  (expect (buffer-string) :to-equal
                                                  "some text
  START REMINDER hello world stop there and do something STOP REMINDER
  more text")))
;; Splitting the lines based on keywords and oopses:1 ends here

;; [[file:../Sacha.org::#preparing-the-vtt-subtitles][Preparing the VTT subtitles:1]]
;;;###autoload
  (defun sacha-audio-braindump-get-subtitle-note-based-on-keywords (sub-text)
          (let ((case-fold-search t))
                  (when (string-match (concat "^"
                                                                                                                                  (regexp-opt sacha-audio-braindump-open-keywords)
                                                                                                                                  " \\(" (regexp-opt sacha-audio-braindump-part-keywords) "\\) \\(.+?\\)\\( "
                                                                                                                                  (regexp-opt sacha-audio-braindump-close-keywords) " "
                                                                                                                                  (regexp-opt sacha-audio-braindump-part-keywords) "\\)?$")
                                                                                                  sub-text)
                          (concat (match-string 1 sub-text) ": " (match-string 2 sub-text)))))
  (ert-deftest sacha-audio-braindump-get-subtitle-note-based-on-keywords ()
          (expect (sacha-audio-braindump-get-subtitle-note-based-on-keywords "BEGIN NEXT STEPS . Think about how dictation helps me practice slower speed. CLOSE NEXT STEPS")
                                          :to-equal "NEXT STEPS: . Think about how dictation helps me practice slower speed.")
          (expect (sacha-audio-braindump-get-subtitle-note-based-on-keywords "START SUMMARY hello world STOP SUMMARY")
                                          :to-equal "SUMMARY: hello world")
          (expect (sacha-audio-braindump-get-subtitle-note-based-on-keywords "START CHAPTER hello world again")
                                          :to-equal "CHAPTER: hello world again")
          )
;; Preparing the VTT subtitles:1 ends here

;; [[file:../Sacha.org::#formatting-the-subtitles-into-org-mode-subtrees][Formatting the subtitles into Org Mode subtrees:1]]
;; todo: sort the completion? https://emacs.stackexchange.com/questions/55502/list-files-in-directory-in-reverse-order-of-date
;;
;;;###autoload
(defun sacha-audio-braindump-insert-subtitles-as-org-tree (vtt-filename)
  (interactive (list (read-file-name "VTT: " (expand-file-name "./" sacha-phone-recording-dir) nil t nil
                                     (lambda (s) (string-match "\\.vtt$" s)))))
  (let* ((subtitles
          (mapcar (lambda (sub)
                    (unless (elt sub 4)
                      (setf (elt sub 4)
                            (sacha-audio-braindump-get-subtitle-note-based-on-keywords (elt sub 3))))
                    sub)
                  (subed-parse-file vtt-filename)))
         (start-date (sacha-audio-braindump-get-file-start-time vtt-filename))
         chapters tags
         start-of-entry)
    (setq start-of-entry (point))
    (insert (format "* TODO Review braindump from %s  :braindump:\n\n" (file-name-base vtt-filename)))
    (org-entry-put (point) "CREATED"
                   (concat "[" (format-time-string
                                (cdr org-timestamp-formats)
                                (sacha-audio-braindump-get-file-start-time
                                 (file-name-nondirectory vtt-filename))) "]"))
    (insert
     (format "%s - %s - %s\n"
             (org-link-make-string (concat "file:" (file-name-sans-extension vtt-filename) ".vtt")
                                   "VTT")
             (org-link-make-string (concat "file:" (file-name-sans-extension vtt-filename) ".txt")
                                   "Text")
             (org-link-make-string (concat "file:" (file-name-sans-extension vtt-filename) ".m4a")
                                   "Audio")))
    (save-excursion
      (insert "** Transcript\n")
      ;; add each subtitle; add an ID in case we change the title
      (mapc
       (lambda (sub)
         (when (elt sub 4)
           (let ((note (sacha-audio-braindump-get-subtitle-note-based-on-keywords (elt sub 3))))
             (insert (concat "*** "
                             note " "
                             (org-link-make-string
                              (format "subed:%s::%s"
                                      vtt-filename
                                      (sacha-msecs-to-timestamp (elt sub 1)))
                              "VTT")
                             "\n\n"))
             (org-entry-put (point) "CREATED"
                            (concat "[" (format-time-string
                                         (cdr org-timestamp-formats)
                                         (time-add start-date
                                                   (seconds-to-time (/ (elt sub 1) 1000.0)))) "]"))
             (org-entry-put (point) "START" (sacha-msecs-to-timestamp (elt sub 2)))
             (when (elt sub 4)
               (when (string-match "command: .*recognize" (elt sub 4))
                 (save-excursion
                   ;; TODO: scope this to just the section someday
                   (goto-char start-of-entry)
                   (org-set-tags (append (list "recognize") (org-get-tags)))))
               (when (string-match "command: .*outline" (elt sub 4))
                 (save-excursion
                   (goto-char start-of-entry)
                   (org-set-tags (append (list "outline") (org-get-tags)))))
               (when (string-match "^time" (elt sub 4))
                 (insert "[" (org-format-time-string (cdr org-timestamp-formats)
                                                     (time-add start-date (seconds-to-time (/ (elt sub 1) 1000))))
                         "]\n"))
               (when (string-match "command: .+\\(high\\|low\\)" (elt sub 4))
                 (save-excursion
                   (goto-char start-of-entry)
                   (org-priority (if (string= (downcase (match-string 1)) "high") ?A ?C))))
               (when (string-match "\\(?:tags?\\|keywords?\\): \\(.+\\)" (elt sub 4))
                 (save-excursion
                   (goto-char start-of-entry)
                   (org-set-tags (append (split-string (match-string 1) " ") (org-get-tags))))))
             (add-to-list 'chapters
                          (format "- %s (%s)"
                                  (org-link-make-string (concat "id:" (org-id-get-create))
                                                        note)
                                  (org-link-make-string
                                   (format "subed:%s::%s"
                                           vtt-filename
                                           (sacha-msecs-to-timestamp (elt sub 1)))
                                   "VTT")))))
         (insert (elt sub 3) "\n"))
       subtitles))
    (when chapters
      (insert (string-join (nreverse chapters) "\n") "\n"))))
;; Formatting the subtitles into Org Mode subtrees:1 ends here

;; [[file:../Sacha.org::#formatting-the-subtitles-into-org-mode-subtrees][Formatting the subtitles into Org Mode subtrees:3]]
(defalias 'sacha-audio-braindump-get-file-start-time #'sacha-file-start-time)
;; Formatting the subtitles into Org Mode subtrees:3 ends here

;; [[file:../Sacha.org::#process-a-single-transcript-from-the-raw-text-file][Process a single transcript from the raw text file:1]]
  (defvar sacha-audio-braindump-file "~/sync/orgzly/braindump.org")

;;;###autoload
  (defun sacha-audio-braindump-make-todo (text-file &optional force)
          "Add TEXT-FILE as a TODO."
          (interactive (list (buffer-file-name) current-prefix-arg))
          ;; rename the files to use the timestamps
          (unless (string-match "^[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]"
                                                                                                  (file-name-base text-file))
                  (setq text-file (sacha-audio-braindump-rename-files-based-on-time text-file)))
          (let* ((recording (concat (file-name-sans-extension text-file) ".m4a"))
                                   (start (sacha-audio-braindump-get-file-start-time text-file))
                                   (vtt (concat (file-name-sans-extension text-file) ".vtt"))
                                   chapters
                                   (title (concat "Review braindump " text-file))
                                   existing)
                  ;; check if already exists
                  (with-current-buffer (find-file-noselect sacha-audio-braindump-file)
                          (save-excursion
                                  (goto-char (point-min))
                                  (setq existing (org-find-exact-headline-in-buffer title))))
                  (if (and existing (not force))
                                  (progn
                                          (message "Going to existing heading")
                                          (org-goto-marker-or-bmk existing))
                          (if (or (null sacha-audio-braindump-last-processed-time)
                                                          (time-less-p sacha-audio-braindump-last-processed-time start))
                                          (customize-save-variable 'sacha-audio-braindump-last-processed-time start))
                          (find-file text-file)
                          (sacha-audio-braindump-prepare-alignment-breaks)
                          (save-buffer)
                          (when (file-exists-p vtt) (delete-file vtt))
                          (when (get-file-buffer vtt) (kill-buffer (get-file-buffer vtt)))
                          (subed-align recording text-file "VTT")
                          (when (get-file-buffer vtt) (kill-buffer (get-file-buffer vtt)))
                          (find-file sacha-audio-braindump-file)
                          (goto-char (point-min))
                          (if existing
                                          (progn
                                                  (org-goto-marker-or-bmk existing)
                                                  (delete-region (point) (org-end-of-subtree)))
                                  (org-next-visible-heading 1))
                          (sacha-audio-braindump-insert-subtitles-as-org-tree vtt))))
;; Process a single transcript from the raw text file:1 ends here

;; [[file:../Sacha.org::#process-multiple-files][Process multiple files:1]]
;;;###autoload
  (defun sacha-audio-braindump-process (files &optional force)
          (interactive (list (cond
                                                                                          ((and (derived-mode-p 'dired-mode)
                                                                                                                  (dired-get-marked-files))
                                                                                           (dired-get-marked-files))
                                                                                          ((derived-mode-p 'dired-mode)
                                                                                           (list (dired-get-filename)))
                                                                                          ((string-match "\\.txt$" (buffer-file-name))
                                                                                           (list (buffer-file-name)))
                                                                                          (t (read-file-name "Transcript: ")))
                                                                                   current-prefix-arg))
          (mapc (lambda (f)
                                          (when (string-match "txt" f)
                                                  (sacha-audio-braindump-make-todo f force))) files))
;; Process multiple files:1 ends here

;; [[file:../Sacha.org::#process-multiple-files][Process multiple files:2]]
  (defcustom sacha-audio-braindump-last-processed-time nil
          "The timestamp of the last processed transcript."
          :group 'sacha
          :type '(repeat integer))

;;;###autoload
  (defun sacha-audio-braindump-process-since-last ()
          (interactive)
          (let ((files
                                   (seq-filter
                                          (lambda (f)
                                                  (or (null sacha-audio-braindump-last-processed-time)
                                                                  (time-less-p sacha-audio-braindump-last-processed-time
                                                                                                                   (sacha-audio-braindump-get-file-start-time f))))
                                          (directory-files sacha-phone-recording-dir 'full " at [0-9][0-9]-[0-9][0-9]\\.txt\\|^[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]T[0-9][0-9]\\.[0-9][0-9]\\.txt"))))
                  (mapc (lambda (f)
                                                  (sacha-audio-braindump-make-todo f)
                                                  (let ((start (sacha-audio-braindump-get-file-start-time f)))
                                                          (if (time-less-p sacha-audio-braindump-last-processed-time start)
                                                                          (setq sacha-audio-braindump-last-processed-time start))))
                                          files))
          (customize-save-variable 'sacha-audio-braindump-last-processed-time sacha-audio-braindump-last-processed-time))

;;;###autoload
  (defun sacha-audio-braindump-new-filename (text-file &optional base-date)
          (if (string-match "^[0-9][0-9][0-9][0-9]" text-file)
                          text-file			; no change, already uses date
                  (let* ((base (file-name-base text-file))
                                           (start (sacha-audio-braindump-get-file-start-time base base-date))
                                           (rest (if (string-match "^\\([-0-9T\\.]+\\|\\(?:.+? at [0-9][0-9]-[0-9][0-9]\\)\\)\\( .+\\)" base)
                                                                                   (match-string 2 base)
                                                                           ""))
                                           (new-base (format-time-string "%Y-%m-%dT%H.%M" start)))
                          (concat new-base rest "." (file-name-extension text-file)))))

  (ert-deftest sacha-audio-braindump-new-filename ()
   (should
          (equal (sacha-audio-braindump-new-filename "Wednesday at 18-58.txt" (date-to-time "2023-01-01"))
                                   "2022-12-28T18.58.txt"))
   (should
          (equal (sacha-audio-braindump-new-filename "Wednesday at 18-58 extra text.txt" (date-to-time "2023-01-01"))
                                   "2022-12-28T18.58 extra text.txt")))

;;;###autoload
  (defun sacha-audio-braindump-rename-files-based-on-time (text-file)
          "Rename TEXT-FILE based on date. Return the new text file."
          (interactive (list (if (derived-mode-p 'dired-mode) (dired-get-filename)
                                                                                           (buffer-file-name))))
          (if (string-match "^[0-9][0-9][0-9][0-9]" text-file)
                          text-file			; no change, already uses date
                  (let ((new-name (sacha-audio-braindump-new-filename (file-name-nondirectory text-file))))
                          (if (file-exists-p (expand-file-name new-name
                                                                                                                                                                           (file-name-directory text-file)))
                                          (error "%s already exists" new-base)
                                  (dolist (ext '(".txt" ".m4a" ".vtt"))
                                          (if (file-exists-p (concat (file-name-sans-extension text-file) ext))
                                                          (rename-file (concat (file-name-sans-extension text-file) ext)
                                                                                                           (expand-file-name (concat (file-name-sans-extension new-name) ext)
                                                                                                                                                                                   (file-name-directory text-file)))))
                                  (expand-file-name new-name
                                                                                                          (file-name-directory text-file))))))
;; Process multiple files:2 ends here

;; [[file:../Sacha.org::#writing-and-editing-updating-sacha-audio-braindump-workflow-to-take-advantage-of-whisperx][Updating my audio braindump workflow to take advantage of WhisperX:1]]
;;;###autoload
  (defun sacha-whisperx-word-list (file)
          (let* ((json-object-type 'alist)
                                   (jmson-array-type 'list))
                  (seq-mapcat (lambda (seg)
                                                                          (alist-get 'words seg))
                                                                  (alist-get 'segments (json-read-file file)))))

  ;; (seq-take (sacha-whisperx-word-list (sacha-latest-file "~/sync/recordings" "\\.json")) 10)
;;;###autoload
  (defun sacha-whisperx-insert-word-list (words)
          "Inserts WORDS with text properties."
          (require 'subed-word-data)
          (mapc (lambda (word)
                                                  (let ((start (point)))
                                                          (insert
                                                           (alist-get 'word word))
                                                          (subed-word-data--add-word-properties start (point) word)
                                                          (insert " ")))
                                  words))

;;;###autoload
  (defun sacha-audio-braindump-turn-sections-into-headings ()
          (interactive)
          (goto-char (point-min))
          (while (re-search-forward "START SECTION \\(.+?\\) STOP SECTION" nil t)
                  (replace-match
                   (save-match-data
                           (format
                                  "\n*** %s\n"
                                  (save-match-data (string-trim (replace-regexp-in-string "^[,\\.]\\|[,\\.]$" "" (match-string 1))))))
                   nil t)
                  (let ((prop-match (save-excursion (text-property-search-forward 'subed-word-data-start))))
                          (when prop-match
                                  (org-entry-put (point) "START" (format-seconds "%02h:%02m:%02s" (prop-match-value prop-match)))))))

;;;###autoload
  (defun sacha-audio-braindump-split-sentences ()
          (interactive)
          (goto-char (point-min))
          (while (re-search-forward "[a-z]\\. " nil t)
                  (replace-match (concat (string-trim (match-string 0)) "\n") )))

;;;###autoload
  (defun sacha-audio-braindump-restructure ()
          (interactive)
          (goto-char (point-min))
          (sacha-subed-fix-common-errors)
          (org-mode)
          (sacha-audio-braindump-prepare-alignment-breaks)
          (sacha-audio-braindump-turn-sections-into-headings)
          (sacha-audio-braindump-split-sentences)
          (goto-char (point-min))
          (sacha-remove-filler-words-at-start))

;;;###autoload
  (defun sacha-audio-braindump-from-whisperx-json (file)
          (interactive (list (read-file-name "JSON: " "~/sync/recordings/" nil nil nil (lambda (f) (string-match "\\.json\\'" f)))))
          ;; put them all into a buffer
          (with-current-buffer (get-buffer-create "*Words*")
                  (erase-buffer)
                  (fundamental-mode)
                  (sacha-whisperx-insert-word-list (sacha-whisperx-word-list file))
                  (sacha-audio-braindump-restructure)
                  (goto-char (point-min))
                  (switch-to-buffer (current-buffer))))

;;;###autoload
  (defun sacha-audio-braindump-process-text (file)
          (interactive (list (read-file-name "Text: " "~/sync/recordings/" nil nil nil (lambda (f) (string-match "\\.txt\\'" f)))))
          (with-current-buffer (find-file-noselect file)
                  (sacha-audio-braindump-restructure)
                  (save-buffer)))
  ;; (sacha-audio-braindump-from-whisperx-json (sacha-latest-file "~/sync/recordings" "\\.json"))
;; Updating my audio braindump workflow to take advantage of WhisperX:1 ends here

;; [[file:../Sacha.org::#rerecognize][Rerecognize this audio and reprocess it:1]]
;;;###autoload
(defun sacha-audio-braindump-reprocess (audio-file)
	(interactive
	 (list
		(let ((default (cond
										((derived-mode-p 'org-mode)
										 (save-excursion
											 (org-back-to-heading)
											 (when (re-search-forward "\\[Audio\\]" nil (save-excursion (org-end-of-subtree)))
												 (org-element-property :path (org-element-context)))))
										((file-exists-p (concat (file-name-sans-extension (buffer-file-name)) ".m4a"))
										 (concat (file-name-sans-extension (buffer-file-name)) ".m4a")))))
			(read-file-name (if default (format "Audio (%s): " default)
												"Audio: ")
											nil default))))
	(save-window-excursion
		(unless (file-exists-p (concat (file-name-sans-extension audio-file) ".json"))
			(sacha-deepgram-recognize-audio audio-file))
		(with-temp-file (concat (file-name-sans-extension audio-file) ".txt")
			(insert
			 (subed-subtitle-list-text
				(sacha-deepgram-parse (concat (file-name-sans-extension audio-file) ".json"))))
			(goto-char (point-min))
			(sacha-audio-braindump-prepare-alignment-breaks))
		(with-current-buffer (find-file-noselect (concat (file-name-sans-extension audio-file) ".txt"))
			(subed-align audio-file (concat (file-name-sans-extension audio-file) ".txt") "VTT")))
	(find-file sacha-audio-braindump-braindump-file)
	(goto-char (point-min))
	(sacha-audio-braindump-insert-subtitles-as-org-tree (concat (file-name-sans-extension audio-file) ".vtt")))
;; Rerecognize this audio and reprocess it:1 ends here

;; [[file:../Sacha.org::#filler-start][Remove filler words at the start and upcase the next word:1]]
(defvar sacha-filler-words-regexp "\\(\\. \\|^\\)\\(?:So?\\|And\\|You know\\|Uh\\)\\(?:,\\|\\.\\.\\.\\)? \\(.\\)")
;;;###autoload
(defun sacha-remove-filler-words-at-start ()
	(interactive)
	(save-excursion
		(let ((case-fold-search nil))
			(while (re-search-forward sacha-filler-words-regexp nil t)
				(if (and (called-interactively-p) (not current-prefix-arg))
						(let ((overlay (make-overlay (match-beginning 0)
																				 (match-end 0))))
							(overlay-put overlay 'common-edit t)
              (overlay-put overlay 'evaporate t)
							(overlay-put
							 overlay 'display
							 (propertize (concat (match-string 0) " -> "
																	 (match-string 1)
																	 (upcase (match-string 2)))
													 'face 'modus-themes-mark-sel))
							(unwind-protect
									(pcase (save-match-data (read-char-choice "Replace (y/n/!/q)? " "yn!q"))
										(?!
										 (replace-match (concat (match-string 1) (upcase (match-string 2))) t)
										 (while (re-search-forward sacha-filler-words-regexp nil t)
											 (replace-match (concat (match-string 1) (upcase (match-string 2))) t)))
										(?y
										 (replace-match (concat (match-string 1) (upcase (match-string 2))) t))
										(?n nil)
										(?q (goto-char (point-max))))
								(delete-overlay overlay)))
					(replace-match (concat (match-string 1) (upcase (match-string 2))) t))))))
;; Remove filler words at the start and upcase the next word:1 ends here

;; [[file:../Sacha.org::#split-up-oops-better][Split up oops better:1]]
;;;###autoload
(defun sacha-split-oops ()
	"Look for oops and make it easier to split."
	(interactive)
	(let ((scan-window 300))
		(while (re-search-forward "oops[,\.]?[ \n]+" nil t)
			(let ((start (min (line-beginning-position) (- (point) scan-window)))
						start-search
						found
						search-for)
				(if (bolp)
						(progn
							(backward-char)
							(setq start (min (line-beginning-position) (- (point) scan-window))))
					(insert "\n"))
				(save-excursion
					(setq start-search (point))
					;; look for 1..5 words back
					(goto-char
					 (or
						(cl-loop
						 for n downfrom 5 downto 1
						 do
						 (save-excursion
							 (dotimes (_ n) (forward-word))
							 (setq search-for (downcase (string-trim (buffer-substring start-search (point)))))
							 (goto-char start-search)
							 (when (re-search-backward (regexp-quote search-for) start t)
								 (goto-char (match-beginning 0))
								 (cl-return (point)))))
						(and (call-interactively 'isearch-backward) (point))))
					(insert "\n"))))))
;; Split up oops better:1 ends here

(provide 'sacha-transcribe)
;;; sacha-transcribe.el ends here
