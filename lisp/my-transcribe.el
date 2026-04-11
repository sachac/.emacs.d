  (defvar my-audio-braindump-open-keywords '("start" "begin" "open"))
  (defvar my-audio-braindump-close-keywords '("stop" "end" "close"))
  (defvar my-audio-braindump-part-keywords '("summary" "chapter" "topic"
                                                                                                                                                          "section"
                                                                                                                                   "action" "idea" "journal" "reminder"
                                                                                                                                   "command" "interruption" "note"
                                                                                                                                   "next step" "next steps" "tags" "tag" "keywords" "keyword"))

  (defvar my-audio-braindump-part-keyword-distance-words 2 "Number of words to scan for part keyword.")
  (defvar my-audio-braindump-close-keyword-distance-words 50 "number of words to scan for stop keyword.
  Put the keywords on the same line if found.")
;;;###autoload
  (defun my-audio-braindump-scan-for-part-keyword (before-part &optional part-keywords within-distance before-distance)
          "Look for BEFORE-PART followed by PART-KEYWORDS.
  There might be WITHIN-DISTANCE words between BEFORE-PART and PART-KEYWORDS,
  and the pair might be within BEFORE-DISTANCE from point.
  Distances are in words.
  Return (start end before-part part) if found, nil otherwise."
          (setq before-part (pcase before-part
                                                                                          ('start my-audio-braindump-open-keywords)
                                                                                          ('stop my-audio-braindump-close-keywords)
                                                                                          ('nil (append my-audio-braindump-open-keywords my-audio-braindump-close-keywords))

                                                                                          (_ before-part)))
          (if (stringp before-part) (setq before-part (list before-part)))
          (setq part-keywords (or part-keywords my-audio-braindump-part-keywords))
          (when (stringp part-keywords) (setq part-keywords (list part-keywords)))
          (setq within-distance (or within-distance my-audio-braindump-part-keyword-distance-words))
          (setq before-distance (if (eq before-distance t)
                                                                                                                  (point-max)
                                                                                                          (or before-distance my-audio-braindump-close-keyword-distance-words)))
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

  (ert-deftest my-audio-braindump-scan-for-part-keyword ()
          (with-temp-buffer
                  (insert "some text start a reminder hello world stop there and do something stop reminder more text")
                  (goto-char (point-min))
                  (let ((result (my-audio-braindump-scan-for-part-keyword 'start nil)))
                          (expect (elt result 2) :to-equal "start")
                          (expect (elt result 3) :to-equal "reminder"))
                  (let ((result (my-audio-braindump-scan-for-part-keyword 'stop "reminder")))
                          (expect (elt result 2) :to-equal "stop")
                          (expect (elt result 3) :to-equal "reminder"))))

;;;###autoload
  (defun my-audio-braindump-prepare-alignment-breaks ()
          "Split lines in preparation for forced alignment with aeneas.

  Split \"oops\" so that it's at the end of the line and the
  previous line starts with roughly the same words as the next
  line, for easier removal.

  Add a linebreak before \"begin/start\" followed by
  `my-audio-braindump-part-keywords'.

  Add a linebreak after \"stop\" followed by
  `my-audio-braindump-part-keywords'.

  Look for begin keyword ... stop keyword with at most
  `my-audio-braindump-part-keyword-distance-words' between them and put them on one
  line. If begin or stop has been misrecognized, try the best guess."
          (interactive)
          (let ((case-fold-search t) result close-result)
                  (my-split-oops)
                  ;; break "begin/start keyword"
                  (goto-char (point-min))
                  (while (setq result (my-audio-braindump-scan-for-part-keyword 'start nil nil t))
                          (goto-char (car result))
                          (delete-region (car result) (elt result 1))
                          (insert "\n" (upcase (concat (elt result 2) " " (elt result 3))) "\n"))
                  ;; break stop
                  (goto-char (point-min))
                  (while (setq result (my-audio-braindump-scan-for-part-keyword 'stop nil nil t))
                          (goto-char (car result))
                          (delete-region (car result) (elt result 1))
                          (insert (upcase (concat (elt result 2) " " (elt result 3))) "\n"))
                  ;; try to get start and end sections on one line
                  (goto-char (point-min))
                  (while (setq result (my-audio-braindump-scan-for-part-keyword 'start nil nil t))
                          (goto-char (elt result 1))
                          (setq stop-result (my-audio-braindump-scan-for-part-keyword 'stop (elt result 3)))
                          (if stop-result
                                          (progn
                                                  (goto-char (car stop-result))
                                                  (while (re-search-backward " *\n+ *" (car result) t)
                                                          (replace-match " ")))
                                  ;; no stop keyword; is the keyword around? maybe it was just misrecognized
                                  (if (re-search-forward (elt result 3)
                                                                                                                           (save-excursion
                                                                                                                                   (forward-word my-audio-braindump-close-keyword-distance-words)
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
                  (while (setq result (my-audio-braindump-scan-for-part-keyword 'stop nil nil t))
                          (goto-char (car result))
                          (save-excursion
                                  (unless (re-search-backward (elt result 3) (line-beginning-position) t)
                                          (when (re-search-backward
                                                                   (elt result 3)
                                                                   (save-excursion (backward-word my-audio-braindump-close-keyword-distance-words)
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

  (ert-deftest my-audio-braindump-prepare-alignment-breaks ()
          (with-temp-buffer
                  (insert "some text start a reminder hello world stop there and do something stop reminder more text")
                  (goto-char (point-min))
                  (my-audio-braindump-prepare-alignment-breaks)
                  (expect (buffer-string) :to-equal
                                                  "some text
  START REMINDER hello world stop there and do something STOP REMINDER
  more text")))

;;;###autoload
  (defun my-audio-braindump-get-subtitle-note-based-on-keywords (sub-text)
          (let ((case-fold-search t))
                  (when (string-match (concat "^"
                                                                                                                                  (regexp-opt my-audio-braindump-open-keywords)
                                                                                                                                  " \\(" (regexp-opt my-audio-braindump-part-keywords) "\\) \\(.+?\\)\\( "
                                                                                                                                  (regexp-opt my-audio-braindump-close-keywords) " "
                                                                                                                                  (regexp-opt my-audio-braindump-part-keywords) "\\)?$")
                                                                                                  sub-text)
                          (concat (match-string 1 sub-text) ": " (match-string 2 sub-text)))))
  (ert-deftest my-audio-braindump-get-subtitle-note-based-on-keywords ()
          (expect (my-audio-braindump-get-subtitle-note-based-on-keywords "BEGIN NEXT STEPS . Think about how dictation helps me practice slower speed. CLOSE NEXT STEPS")
                                          :to-equal "NEXT STEPS: . Think about how dictation helps me practice slower speed.")
          (expect (my-audio-braindump-get-subtitle-note-based-on-keywords "START SUMMARY hello world STOP SUMMARY")
                                          :to-equal "SUMMARY: hello world")
          (expect (my-audio-braindump-get-subtitle-note-based-on-keywords "START CHAPTER hello world again")
                                          :to-equal "CHAPTER: hello world again")
          )

;; todo: sort the completion? https://emacs.stackexchange.com/questions/55502/list-files-in-directory-in-reverse-order-of-date
;;
;;;###autoload
(defun my-audio-braindump-insert-subtitles-as-org-tree (vtt-filename)
  (interactive (list (read-file-name "VTT: " (expand-file-name "./" my-phone-recording-dir) nil t nil
                                     (lambda (s) (string-match "\\.vtt$" s)))))
  (let* ((subtitles
          (mapcar (lambda (sub)
                    (unless (elt sub 4)
                      (setf (elt sub 4)
                            (my-audio-braindump-get-subtitle-note-based-on-keywords (elt sub 3))))
                    sub)
                  (subed-parse-file vtt-filename)))
         (start-date (my-audio-braindump-get-file-start-time vtt-filename))
         chapters tags
         start-of-entry)
    (setq start-of-entry (point))
    (insert (format "* TODO Review braindump from %s  :braindump:\n\n" (file-name-base vtt-filename)))
    (org-entry-put (point) "CREATED"
                   (concat "[" (format-time-string
                                (cdr org-timestamp-formats)
                                (my-audio-braindump-get-file-start-time
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
           (let ((note (my-audio-braindump-get-subtitle-note-based-on-keywords (elt sub 3))))
             (insert (concat "*** "
                             note " "
                             (org-link-make-string
                              (format "subed:%s::%s"
                                      vtt-filename
                                      (my-msecs-to-timestamp (elt sub 1)))
                              "VTT")
                             "\n\n"))
             (org-entry-put (point) "CREATED"
                            (concat "[" (format-time-string
                                         (cdr org-timestamp-formats)
                                         (time-add start-date
                                                   (seconds-to-time (/ (elt sub 1) 1000.0)))) "]"))
             (org-entry-put (point) "START" (my-msecs-to-timestamp (elt sub 2)))
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
                                           (my-msecs-to-timestamp (elt sub 1)))
                                   "VTT")))))
         (insert (elt sub 3) "\n"))
       subtitles))
    (when chapters
      (insert (string-join (nreverse chapters) "\n") "\n"))))

(defalias 'my-audio-braindump-get-file-start-time #'my-file-start-time)

  (defvar my-audio-braindump-file "~/sync/orgzly/braindump.org")

;;;###autoload
  (defun my-audio-braindump-make-todo (text-file &optional force)
          "Add TEXT-FILE as a TODO."
          (interactive (list (buffer-file-name) current-prefix-arg))
          ;; rename the files to use the timestamps
          (unless (string-match "^[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]"
                                                                                                  (file-name-base text-file))
                  (setq text-file (my-audio-braindump-rename-files-based-on-time text-file)))
          (let* ((recording (concat (file-name-sans-extension text-file) ".m4a"))
                                   (start (my-audio-braindump-get-file-start-time text-file))
                                   (vtt (concat (file-name-sans-extension text-file) ".vtt"))
                                   chapters
                                   (title (concat "Review braindump " text-file))
                                   existing)
                  ;; check if already exists
                  (with-current-buffer (find-file-noselect my-audio-braindump-file)
                          (save-excursion
                                  (goto-char (point-min))
                                  (setq existing (org-find-exact-headline-in-buffer title))))
                  (if (and existing (not force))
                                  (progn
                                          (message "Going to existing heading")
                                          (org-goto-marker-or-bmk existing))
                          (if (or (null my-audio-braindump-last-processed-time)
                                                          (time-less-p my-audio-braindump-last-processed-time start))
                                          (customize-save-variable 'my-audio-braindump-last-processed-time start))
                          (find-file text-file)
                          (my-audio-braindump-prepare-alignment-breaks)
                          (save-buffer)
                          (when (file-exists-p vtt) (delete-file vtt))
                          (when (get-file-buffer vtt) (kill-buffer (get-file-buffer vtt)))
                          (subed-align recording text-file "VTT")
                          (when (get-file-buffer vtt) (kill-buffer (get-file-buffer vtt)))
                          (find-file my-audio-braindump-file)
                          (goto-char (point-min))
                          (if existing
                                          (progn
                                                  (org-goto-marker-or-bmk existing)
                                                  (delete-region (point) (org-end-of-subtree)))
                                  (org-next-visible-heading 1))
                          (my-audio-braindump-insert-subtitles-as-org-tree vtt))))

;;;###autoload
  (defun my-audio-braindump-process (files &optional force)
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
                                                  (my-audio-braindump-make-todo f force))) files))

  (defcustom my-audio-braindump-last-processed-time nil
          "The timestamp of the last processed transcript."
          :group 'sacha
          :type '(repeat integer))

;;;###autoload
  (defun my-audio-braindump-process-since-last ()
          (interactive)
          (let ((files
                                   (seq-filter
                                          (lambda (f)
                                                  (or (null my-audio-braindump-last-processed-time)
                                                                  (time-less-p my-audio-braindump-last-processed-time
                                                                                                                   (my-audio-braindump-get-file-start-time f))))
                                          (directory-files my-phone-recording-dir 'full " at [0-9][0-9]-[0-9][0-9]\\.txt\\|^[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]T[0-9][0-9]\\.[0-9][0-9]\\.txt"))))
                  (mapc (lambda (f)
                                                  (my-audio-braindump-make-todo f)
                                                  (let ((start (my-audio-braindump-get-file-start-time f)))
                                                          (if (time-less-p my-audio-braindump-last-processed-time start)
                                                                          (setq my-audio-braindump-last-processed-time start))))
                                          files))
          (customize-save-variable 'my-audio-braindump-last-processed-time my-audio-braindump-last-processed-time))

;;;###autoload
  (defun my-audio-braindump-new-filename (text-file &optional base-date)
          (if (string-match "^[0-9][0-9][0-9][0-9]" text-file)
                          text-file			; no change, already uses date
                  (let* ((base (file-name-base text-file))
                                           (start (my-audio-braindump-get-file-start-time base base-date))
                                           (rest (if (string-match "^\\([-0-9T\\.]+\\|\\(?:.+? at [0-9][0-9]-[0-9][0-9]\\)\\)\\( .+\\)" base)
                                                                                   (match-string 2 base)
                                                                           ""))
                                           (new-base (format-time-string "%Y-%m-%dT%H.%M" start)))
                          (concat new-base rest "." (file-name-extension text-file)))))

  (ert-deftest my-audio-braindump-new-filename ()
   (should
          (equal (my-audio-braindump-new-filename "Wednesday at 18-58.txt" (date-to-time "2023-01-01"))
                                   "2022-12-28T18.58.txt"))
   (should
          (equal (my-audio-braindump-new-filename "Wednesday at 18-58 extra text.txt" (date-to-time "2023-01-01"))
                                   "2022-12-28T18.58 extra text.txt")))

;;;###autoload
  (defun my-audio-braindump-rename-files-based-on-time (text-file)
          "Rename TEXT-FILE based on date. Return the new text file."
          (interactive (list (if (derived-mode-p 'dired-mode) (dired-get-filename)
                                                                                           (buffer-file-name))))
          (if (string-match "^[0-9][0-9][0-9][0-9]" text-file)
                          text-file			; no change, already uses date
                  (let ((new-name (my-audio-braindump-new-filename (file-name-nondirectory text-file))))
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

;;;###autoload
  (defun my-whisperx-word-list (file)
          (let* ((json-object-type 'alist)
                                   (jmson-array-type 'list))
                  (seq-mapcat (lambda (seg)
                                                                          (alist-get 'words seg))
                                                                  (alist-get 'segments (json-read-file file)))))

  ;; (seq-take (my-whisperx-word-list (my-latest-file "~/sync/recordings" "\\.json")) 10)
;;;###autoload
  (defun my-whisperx-insert-word-list (words)
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
  (defun my-audio-braindump-turn-sections-into-headings ()
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
  (defun my-audio-braindump-split-sentences ()
          (interactive)
          (goto-char (point-min))
          (while (re-search-forward "[a-z]\\. " nil t)
                  (replace-match (concat (string-trim (match-string 0)) "\n") )))

;;;###autoload
  (defun my-audio-braindump-restructure ()
          (interactive)
          (goto-char (point-min))
          (my-subed-fix-common-errors)
          (org-mode)
          (my-audio-braindump-prepare-alignment-breaks)
          (my-audio-braindump-turn-sections-into-headings)
          (my-audio-braindump-split-sentences)
          (goto-char (point-min))
          (my-remove-filler-words-at-start))

;;;###autoload
  (defun my-audio-braindump-from-whisperx-json (file)
          (interactive (list (read-file-name "JSON: " "~/sync/recordings/" nil nil nil (lambda (f) (string-match "\\.json\\'" f)))))
          ;; put them all into a buffer
          (with-current-buffer (get-buffer-create "*Words*")
                  (erase-buffer)
                  (fundamental-mode)
                  (my-whisperx-insert-word-list (my-whisperx-word-list file))
                  (my-audio-braindump-restructure)
                  (goto-char (point-min))
                  (switch-to-buffer (current-buffer))))

;;;###autoload
  (defun my-audio-braindump-process-text (file)
          (interactive (list (read-file-name "Text: " "~/sync/recordings/" nil nil nil (lambda (f) (string-match "\\.txt\\'" f)))))
          (with-current-buffer (find-file-noselect file)
                  (my-audio-braindump-restructure)
                  (save-buffer)))
  ;; (my-audio-braindump-from-whisperx-json (my-latest-file "~/sync/recordings" "\\.json"))

;;;###autoload
(defun my-audio-braindump-reprocess (audio-file)
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
			(my-deepgram-recognize-audio audio-file))
		(with-temp-file (concat (file-name-sans-extension audio-file) ".txt")
			(insert
			 (subed-subtitle-list-text
				(my-deepgram-parse (concat (file-name-sans-extension audio-file) ".json"))))
			(goto-char (point-min))
			(my-audio-braindump-prepare-alignment-breaks))
		(with-current-buffer (find-file-noselect (concat (file-name-sans-extension audio-file) ".txt"))
			(subed-align audio-file (concat (file-name-sans-extension audio-file) ".txt") "VTT")))
	(find-file my-audio-braindump-braindump-file)
	(goto-char (point-min))
	(my-audio-braindump-insert-subtitles-as-org-tree (concat (file-name-sans-extension audio-file) ".vtt")))

(defvar my-filler-words-regexp "\\(\\. \\|^\\)\\(?:So?\\|And\\|You know\\|Uh\\)\\(?:,\\|\\.\\.\\.\\)? \\(.\\)")
;;;###autoload
(defun my-remove-filler-words-at-start ()
	(interactive)
	(save-excursion
		(let ((case-fold-search nil))
			(while (re-search-forward my-filler-words-regexp nil t)
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
										 (while (re-search-forward my-filler-words-regexp nil t)
											 (replace-match (concat (match-string 1) (upcase (match-string 2))) t)))
										(?y
										 (replace-match (concat (match-string 1) (upcase (match-string 2))) t))
										(?n nil)
										(?q (goto-char (point-max))))
								(delete-overlay overlay)))
					(replace-match (concat (match-string 1) (upcase (match-string 2))) t))))))

;;;###autoload
(defun my-split-oops ()
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
