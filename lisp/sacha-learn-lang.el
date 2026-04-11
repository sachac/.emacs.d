;;; sacha-learn-lang.el ---  -*- lexical-binding: t -*-

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



;; [[file:../Sacha.org::#multimedia-learning-french][Learning French:3]]
;;;###autoload
  (defun sacha-learn-lang-chrome-speech-new-session ()
    (interactive)
    (sacha-speech-chrome-new-session "french" "fr-FR"))
;; Learning French:3 ends here

;; [[file:../Sacha.org::#writing-and-editing-learning-french-practice-pronunciation][Practice pronunciation:1]]
  (defvar sacha-learn-lang-practice-dir "~/proj/french/audio")
  (defvar sacha-learn-lang-practice-temp (expand-file-name "temp.wav" sacha-learn-lang-practice-dir))
;;;###autoload
  (defun sacha-learn-lang-practice-line ()
    (interactive)
    (sit-for 1)
    (let* ((date (format-time-string "%Y-%m-%d-%H-%M-%S"))
           (filename
            (expand-file-name
             (concat date " "
                     (string-trim
                      (buffer-substring (line-beginning-position)
                                        (line-end-position)))
                     ".opus")
             sacha-learn-lang-practice-dir))
           (process (start-process "arecord"
                                   (get-buffer-create "*record*")
                                   "arecord" "-D" "hw:2,0" "-t" "wav" "-f" "cd"
                                   sacha-learn-lang-practice-temp)))
      (read-key "Press a key")
      (kill-process process)
      (call-process "ffmpeg" nil (get-buffer-create "*record*") nil
                    "-y"
                    "-i"
                    sacha-learn-lang-practice-temp
                    "-af" "silenceremove=start_periods=1:start_duration=0:start_threshold=-60dB,areverse,silenceremove=start_periods=1:start_duration=0:start_threshold=-60dB,areverse,loudnorm=I=-16:LRA=11:TP=-1.5"
                    filename)
      (mpv-play filename)))

;;;###autoload
  (defun sacha-learn-lang-practice-replay ()
    (interactive)
    (mpv-play (sacha-latest-file sacha-learn-lang-practice-dir)))

;;;###autoload
  (defun sacha-learn-lang-practice-transcribe ()
    (interactive)
    (let* ((default-directory sacha-learn-lang-practice-dir)
           (file (sacha-latest-file sacha-learn-lang-practice-dir "\\.opus\\|\\.m4a\\|\\.webm")))
      ;; (call-process "/bin/bash" nil nil nil "/home/sacha/bin/whisperx" (expand-file-name file))
      (message "%s"
               (with-temp-buffer
                 (insert-file-contents (concat (file-name-sans-extension file) ".txt"))
                 (string-trim (buffer-string))))))

;;;###autoload
  (defun sacha-learn-lang-practice-play-current-reference (&optional beg end)
    "Play the current segment."
    (interactive)
    (when (derived-mode-p 'subed-mode)
      (let ((comment (subed-subtitle-comment)))
        (cond
         ((string-match "#\\+REFERENCE: \\(.+\\) +\\(.+?\\) +--> +\\([^ ]+\\)" comment)
          (let ((file (expand-file-name (match-string 1 comment)))
                (start (match-string 2 comment))
                (stop (match-string 3 comment)))
            (call-process "mpv" nil nil nil
                          file
                          (format "--start=%.3f" (/ (subed-timestamp-to-msecs start) 1000.0))
                          (format "--end=%.3f" (/ (subed-timestamp-to-msecs stop) 1000.0)))))
         ((string-match "#\\+REFERENCE: \\(.+\\)" comment)
          (call-process "mpv" nil nil nil (match-string 1 comment)))
        (t (let ((start
                 (if beg (save-excursion
                           (goto-char beg)
                           (subed-subtitle-msecs-start))
                   (subed-subtitle-msecs-start)))
                (stop
                 (if beg (save-excursion
                           (goto-char end)
                           (subed-subtitle-msecs-stop))
                   (subed-subtitle-msecs-stop))))
            (call-process "mpv" nil nil nil (subed-media-file)
                          (format "--start=%.3f" (/ start 1000.0))
                          (format "--end=%.3f" (/ stop 1000.0)))))))))

;;;###autoload
  (defun sacha-learn-lang-practice-record-loop (&optional extra)
    (interactive
     (list (cond
            ((and (derived-mode-p 'subed-mode)
                  (region-active-p))
             (subed-subtitle-list-text
              (subed-subtitle-list
               (region-beginning)
               (region-end))))
            ((region-active-p)
             (concat
              " "
              (string-trim
               (buffer-substring (region-beginning) (region-end)))))
            ((derived-mode-p 'subed-mode)
             (concat " " (subed-subtitle-text))))))
    (let* ((date (format-time-string "%Y-%m-%d-%H-%M-%S"))
           (key-prompt "%d (SPC to review, RET get feedback, q to quit, any other key to retry")
           (filename
            (expand-file-name
             (concat date
                     (if extra
                         (concat " "
                                 (string-trim (replace-regexp-in-string
                                               "[\\?]+" " "
                                               (car
                                                (split-string
                                                 extra "\n")))))
                       "")

                     ".opus")
             sacha-learn-lang-practice-dir))
           done
           char
           (count 0)
           process)
      (while (not done)
        (when (derived-mode-p 'subed-mode)
          (if (region-active-p)
              (sacha-learn-lang-practice-play-current-reference (region-beginning) (region-end))
            (sacha-learn-lang-practice-play-current-reference)))
        (setq count (1+ count))
        (setq process (start-process "ffmpeg"
                                     (get-buffer-create "*record*")
                                     "ffmpeg" "-y" "-f" "pulse" "-i" "alsa_input.usb-Blue_Microphones_Yeti_Stereo_Microphone_REV8-00.analog-stereo"
                                     sacha-learn-lang-practice-temp))
        (setq char
              (read-key (format key-prompt
                                count)))
        (while char
          (pcase char
            (?\  (when (process-live-p process)
                   (sit-for 1)
                   (kill-process process))
                 (sacha-learn-lang-practice-play-current-reference)
                 (call-process "mpv" nil nil nil sacha-learn-lang-practice-temp)
                 (setq char
                       (read-key (format "%d (SPC to review, RET get feedback, q to quit, any other key to retry"
                                         count))))
            (13 (when (process-live-p process)
                  (sit-for 1)
                  (kill-process process))
                (setq done 'feedback)
                (setq char nil))
            (?w            ; transcribe with Whisper
             (sacha-learn-lang-practice-transcribe))
            (?q
             (when (process-live-p process) (kill-process process))
             (setq done 'ignore)
             (setq char nil))
            (_
             (when (process-live-p process) (kill-process process))
             (setq char nil)))))
        (when (member done '(keep feedback))
          (call-process "ffmpeg" nil (get-buffer-create "*record*") nil
                        "-y"
                        "-i"
                        sacha-learn-lang-practice-temp
                        "-af" "loudnorm=I=-16:LRA=11:TP=-1.5"
                        ;; "-af" "silenceremove=start_periods=1:start_duration=0:start_threshold=-100dB,areverse,silenceremove=start_periods=1:start_duration=0:start_threshold=-100dB,areverse,loudnorm=I=-16:LRA=11:TP=-1.5"
                        filename)
          (when (eq done 'feedback)
            (sacha-learn-lang-get-audio-feedback filename extra t)))))
;; Practice pronunciation:1 ends here

;; [[file:../Sacha.org::#writing-and-editing-learning-french-start-the-process-for-transcribing-the-latest-recording][Start the process for transcribing the latest recording:1]]
;;;###autoload
  (defun sacha-learn-lang-process-latest-recording (annotation)
    (interactive "MAnnotation: ")
    (let* ((file (sacha-latest-file sacha-recordings-dir))
           (audio (expand-file-name
                   (concat (file-name-base file) "-" annotation ".opus")
                   "~/sync/recordings/")))
      (make-process :name "whisperx"
                    :buffer (get-buffer-create "*whisperx*")
                    :command (list
                              "bash"
                              "-c"
                              (format
                               "ffmpeg -y -i %s %s; cd ~/sync/recordings; ~/bin/whisperx %s"
                               (shell-quote-argument file)
                               (shell-quote-argument audio)
                               (shell-quote-argument audio))))
      (message "Started %s" audio)))
;; Start the process for transcribing the latest recording:1 ends here

;; [[file:../Sacha.org::#writing-and-editing-learning-french-emacs-lisp-and-nodejs-getting-the-bolded-words-from-a-section-of-a-google-document][Emacs Lisp and NodeJS: Getting the bolded words from a section of a Google Document:3]]
  (defvar sacha-learn-lang-words-for-review-context-function 'sentence-at-point)
  (defvar sacha-learn-lang-tutor-notes-url nil)
;;;###autoload
  (defun sacha-learn-lang-tutor-notes (section-name)
    (sacha-org-get-subtree-by-name
     (sacha-google-doc-org sacha-learn-lang-tutor-notes-url)
     section-name))

;;;###autoload
  (defun sacha-learn-lang-words-for-review (section)
    "List the bolded words for review in SECTION."
    (let* ((section (sacha-learn-lang-tutor-notes section))
           results)
      (with-temp-buffer
        (insert section)
        (org-mode)
        (goto-char (point-min))
        (org-map-entries
         (lambda ()
           (org-end-of-meta-data t)
           (unless (looking-at org-heading-regexp)
             (let ((end (save-excursion (org-end-of-subtree))))
               (while (re-search-forward "\\*[^* ].*?\\*" end t)
                 (cl-pushnew
                  (replace-regexp-in-string
                   "[ \n ]+" " "
                   (funcall sacha-learn-lang-words-for-review-context-function))
                  results
                  :test 'string=)))))))
      (nreverse results)))
;; Emacs Lisp and NodeJS: Getting the bolded words from a section of a Google Document:3 ends here

;; [[file:../Sacha.org::#writing-and-editing-learning-french-emacs-lisp-and-nodejs-getting-the-bolded-words-from-a-section-of-a-google-document][Emacs Lisp and NodeJS: Getting the bolded words from a section of a Google Document:6]]
;;;###autoload
  (defun sacha-learn-lang-words-for-review-phrase-context (&optional s)
    (setq s (replace-regexp-in-string " " " " (or s (sentence-at-point))))
    (string-join
     (seq-keep
      (lambda (s)
        (when (string-match "\\*" s)
          (replace-regexp-in-string "^, " "" s)))
      (sacha-split-string-keep-delimiters s ", \\| parce que \\| que \\| qui \\| qu'ils? \\| qu'elles? \\| qu'on \\| pour "))
     " ... "))

  (ert-deftest sacha-learn-lang-words-for-review-phrase-context ()
    (should
     (equal (sacha-learn-lang-words-for-review-phrase-context
             "Je peux consacrer une petite partie de mon *budget* à des essais, mais je ne veux pas travailler davantage pour rentabiliser une dépense plus importante.")
            "Je peux consacrer une petite partie de mon *budget* à des essais")))
;; Emacs Lisp and NodeJS: Getting the bolded words from a section of a Google Document:6 ends here

;; [[file:../Sacha.org::#writing-and-editing-learning-french-emacs-lisp-and-nodejs-getting-the-bolded-words-from-a-section-of-a-google-document][Emacs Lisp and NodeJS: Getting the bolded words from a section of a Google Document:8]]
;;;###autoload
  (defun sacha-learn-lang-tutor-notes-wdiff-org ()
    (interactive)
    (let ((section (org-entry-get (point) "ITEM")))
      (sacha-wdiff-strings
       (replace-regexp-in-string
        " " " "
        (sacha-org-subtree-text-without-blocks))
       (replace-regexp-in-string
        org-link-bracket-re
        "\\2"
        (replace-regexp-in-string
         " " " "
         (sacha-learn-lang-tutor-notes section))))))
;; Emacs Lisp and NodeJS: Getting the bolded words from a section of a Google Document:8 ends here

;; [[file:../Sacha.org::#writing-and-editing-learning-french-gtts-cli][gtts-cli:2]]

;; gtts-cli:2 ends here

;; [[file:../Sacha.org::#writing-and-editing-learning-french-word-timestamps][Word timestamps:1]]
;;;###autoload
  (defun sacha-learn-lang-word-timestamps ()
    "Add timestamps using the MFA French model."
    (interactive)
    (let ((subed-align-mfa-dictionary "french_mfa")
          (subed-align-mfa-acoustic-model "french_mfa"))
      (subed-align-mfa-set-word-data
       (subed-media-file)
       nil nil
       (lambda (&rest _)
         (subed-word-data-add-word-timestamps)))))
;; Word timestamps:1 ends here

;; [[file:../Sacha.org::#writing-and-editing-learning-french-ai-feedback][AI feedback:1]]
;;;###autoload
  (defun sacha-learn-lang-get-audio-feedback (filename &optional extra-text display)
    (interactive (list (if current-prefix-arg
                           (read-file-name "File: " sacha-learn-lang-practice-dir nil t nil
                                           (lambda (filename) (string-match "\\.m4a$" filename)))
                         (sacha-latest-file sacha-learn-lang-practice-dir "\\(\\.m4a\\|\\.webm\\)$"))
                       (if (region-active-p)
                           (buffer-substring (region-beginning) (region-end)))
                       t))
    (let* ((data
            (replace-regexp-in-string
             "\n" ""
             (shell-command-to-string (concat "base64 " (shell-quote-argument filename)))))
           (text
            (concat "Give me feedback in English on this recording of beginner French practice. I am a female A0/A1 speaker. Focus first on major mispronunciations, and provide English phonetic transcriptions for those words using italicized parenthetical notes.

  Return your response using Org Mode syntax using only list items, not headings. Do not put it in a code block, just return Org Mode text. For example, *bold*. Score it out of 10.

  Example output:

  - Pronunciation (7 /10)
    - travaillé /(trah vay yay)/
  "
                    (if extra-text
                        (concat "\n\n###\n\n" extra-text)
                      "")))
           (json-array-type 'vector)
           (json-object-type 'alist)
           (body (json-encode
                  `(("contents"
                     (("parts" .
                       ((("text" . ,text))
                        (("inline_data" .
                          (("mime_type" . "audio/mp4")
                           ("data" . ,data))))
                        )))))))
           result)
      (setq
       result
       (plz 'post "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-flash:generateContent"
         :headers
         `(("Content-Type" . "application/json")
           ("X-goog-api-key" . ,sacha-gemini-api-key))
         :as #'json-read
         :body body))
      (when display
        (with-current-buffer (get-buffer-create "*Feedback*")
          (org-mode)
          (goto-char (point-min))
          (let* ((text (map-nested-elt result '(candidates 0 content parts 0 text)))
                 (score (when (string-match "Pronunciation (\\(.+\\))" text) (match-string 1 text))))
            (insert "* " (org-link-make-string
                          (concat "audio:"
                                  (replace-regexp-in-string
                                   (rx line-start
                                       (literal (getenv "HOME")))
                                   "~"
                                   filename))
                          "me:") "  "
                          (or score "")
                          "\n"
                          text
                          "\n\n"))
          (display-buffer (current-buffer))
          (with-selected-window (get-buffer-window (current-buffer))
            (goto-char (point-min))
            (recenter-top-bottom 0))))
      (map-nested-elt result '(candidates 0 content parts 0 text))))
;; AI feedback:1 ends here

;; [[file:../Sacha.org::#writing-and-editing-learning-french-save-journal-entries-for-analysis][Save journal entries for analysis:1]]
;;;###autoload
  (defun sacha-learn-lang-write-journal-entries-for-subtree ()
    (interactive)
    (org-map-entries
     (lambda ()
       (when (org-entry-get (point) "DATE")
         (let ((text (replace-regexp-in-string "{.+?}" "" (sacha-org-subtree-text-without-blocks))))
           (with-temp-file (expand-file-name (concat (org-entry-get (point) "DATE") ".txt")
                                             "~/proj/french/journal")
             (insert text)))))
     nil 'tree))
;; Save journal entries for analysis:1 ends here

;; [[file:../Sacha.org::#writing-and-editing-learning-french-load-en-fr-dictionary][Load en-fr dictionary:1]]
(defvar sacha-learn-lang-en-fr-dictionary-file "~/proj/french/dic-en-fr.iso")
(defvar sacha-learn-lang-dictionary nil)
;;;###autoload
(defun sacha-learn-lang-load-dict ()
  (interactive)
  (with-temp-buffer
    (insert-file-contents sacha-learn-lang-en-fr-dictionary-file)
    (goto-char (point-min))
    (while (looking-at "^# ") (forward-line 1))
    (setq sacha-learn-lang-dictionary
          (seq-keep (lambda (s)
                      (when (string-match "^\\(.+?\\) {\\(.+?\\)}\\(?: /\\(.+?\\)/\\)?\\(?: (\\(.+?\\))\\)?\\( SEE: .+?\\)? ::\\(?: \\(.+?\\) *\\({.+}.*\\)?\\)?$" s)
                        (let ((headword (match-string 1 s))
                              (head-type (match-string 2 s))
                              (def (match-string 4 s))
                              (see (match-string 5 s))
                              (translation (match-string 6 s))
                              (types (match-string 7 s)))
                          (cons
                           (propertize (format "%s {%s} - %s - %s :: %s"
                                               headword
                                               head-type
                                               (or see translation)
                                               (or def "")
                                               (or types ""))
                                       'gender
                                       (cond
                                         ((null types) nil)
                                         ((string-match "^{m}" types) "m")
                                         ((string-match "^{f}" types) "f")))
                           translation))))
                    (split-string (buffer-substring (point) (point-max)) "\n")))
    (seq-take sacha-learn-lang-dictionary 10)))

;;;###autoload
(defun sacha-learn-lang-consult-en-fr ()
  (interactive)
  (unless sacha-learn-lang-dictionary (sacha-learn-lang-load-dict))
  (insert
   (consult--read
    (consult--dynamic-collection
        (lambda (input)
          (let (match-start
                match-any
                exact
                (search (regexp-quote input)))
            (seq-map (lambda (o)
                       (setf (car o)
                             (propertize
                              (car o)
                              'face
                              (list
                               :background
                               (pcase (get-text-property 0 'gender (car o))
                                 ('nil nil)
                                 ("m" (modus-themes-get-color-value 'bg-blue-subtle))
                                 ("f" (modus-themes-get-color-value 'bg-magenta-subtle))))))
                       (cond
                        ((string-match (concat "^" search " - ") (car o))
                         (push o exact))
                        ((string-match (concat "^" search) (car o))
                         (push o match-start))
                        ((string-match search (car o))
                         (push o match-any))))
                     sacha-learn-lang-dictionary)
            (append
             (nreverse exact)
             (nreverse match-start)
             (nreverse match-any)
             nil))))
    :sort nil
    :lookup #'consult--lookup-cdr)))
;; Load en-fr dictionary:1 ends here

;; [[file:../Sacha.org::#writing-and-editing-learning-french-conjugation][Conjugation:1]]
;;;###autoload
    (defun sacha-learn-lang-conjugate-grammalecte (input)
      "Query Grammalecte for INPUT."
      (interactive (list (sacha-learn-lang-lexique-complete-word)))
      (grammalecte-conjugate-verb input))

    (defvar sacha-learn-lang-verbe-db "~/vendor/verbe-conjugaison-academie-francaise/output/verbs.db")

;;;###autoload
    (defun sacha-learn-lang-conjugate (input &optional all-forms)
      (interactive (list (sacha-learn-lang-lexique-complete-word)
                         (null current-prefix-arg)))
      (let* ((db (sqlite-open sacha-learn-lang-verbe-db))
             (lemme (elt (car (sacha-learn-lang-lexique-lookup-db-exact input)) 1))
             (value
              (consult--read
               (if all-forms
                   (mapcar (lambda (row)
                             (cons (string-join
                                    (list
                                     (elt row 0)
                                     (elt row 1)
                                     (elt row 2)
                                     (elt row 4)
                                     (elt row 3))
                                    " - ")
                                   (elt row 0)))
                           (sqlite-select db
                                          "SELECT
        conjugaison, voix, mode, temps, personne
    FROM verbes v
    JOIN conjugaisons c
    ON v.id = c.verbe_id WHERE v.infinitif = ?
    ORDER BY temps, mode, personne" (list lemme)))
                 (mapcar (lambda (row)
                             (cons (string-join
                                    (list
                                     (elt row 0)
                                     (elt row 4)
                                     (elt row 3))
                                    " - ")
                                   (elt row 0)))

                           (sqlite-select db
                                          "SELECT
        conjugaison, voix, mode, temps, personne
    FROM verbes v
    JOIN conjugaisons c
    ON v.id = c.verbe_id WHERE v.infinitif = ?
    AND temps in (?, ?)
    AND mode=?
    ORDER BY temps, mode, personne"
                                          (list input "present" "passe_compose"  "indicatif"))))
               :prompt "Verbe: "
               :lookup 'consult--lookup-cdr)))
        (sqlite-close db)
        (when (called-interactively-p 'any)
          (when (word-at-point)
              (delete-region (save-excursion
                               (skip-syntax-backward "w")
                               (point))
                             (save-excursion
                               (skip-syntax-forward "w")
                               (point))))
          (insert value))
        value))
;; Conjugation:1 ends here

;; [[file:../Sacha.org::#writing-and-editing-learning-french-looking-up-words-via-wordreference][Looking up words via wordreference:1]]
  (defvar sacha-learn-lang-wordreference-cache nil)

;;;###autoload
  (defun sacha-learn-lang-wordreference-completing-read ()
    (interactive)
    (consult--read
     (consult--dynamic-collection
         (lambda (input)
           (message "input: %s" input)
           (with-current-buffer
               (url-retrieve-synchronously
                (concat "https://www.wordreference.com/autocomplete?dict=enfr&query="
                        (url-hexify-string input)))
                               (set-buffer-multibyte t)
             (goto-char (point-min))
             (re-search-forward "^$" nil t)
             (prog1
                 (mapcar (lambda (row)
                           (let ((fields (split-string row "\t")))
                             (propertize
                              (format "%s (%s)"
                                      (car fields)
                                      (cadr fields))
                              'consult--candidate
                              fields)))
                         (split-string
                          (string-trim (buffer-substring (point) (point-max))) "\n"))
               (kill-buffer (current-buffer))))))
     :sort nil
     :initial (symbol-name (symbol-at-point))
     :history 'sacha-learn-lang-wordreference-lookup-history
     :prompt "Word: "
     :category 'word))

  (defvar-keymap sacha-learn-lang-wordreference-keymap
    "v" #'sacha-spookfox-scroll-down
    "V" #'sacha-spookfox-scroll-up
    "c" #'sacha-learn-lang-conjugate-last-word
    "l" #'sacha-learn-lang-wordreference-lookup)

  (defvar sacha-learn-lang-wordreference-lookup-history nil)
;;;###autoload
  (defun sacha-learn-lang-wordreference-lookup (word)
    (interactive (list (sacha-learn-lang-wordreference-completing-read)))
    (let (language)
      (if (string-match " (\\(en\\|fr\\))" word)
          (setq language (match-string 1 word)
                word (replace-match "" nil t word 0)))
      (setq word (replace-regexp-in-string "^#" "" word))
      (browse-url
       (format "https://www.wordreference.com/%s/%s"
               (if (string= language "fr")
                   "fren"
                 "enfr")
               (url-hexify-string word))))
    (set-transient-map sacha-learn-lang-wordreference-keymap t))

;;;###autoload
  (defun sacha-learn-lang-conjugate-last-word ()
    (interactive)
    (grammalecte-conjugate-verb
     (replace-regexp-in-string " (.+)" ""
                               (car sacha-learn-lang-wordreference-lookup-history))))

;;;###autoload
  (defun sacha-learn-lang-wordreference-conjugate (word)
    (interactive (list (sacha-learn-lang-wordreference-completing-read)))
    (browse-url (concat "https://www.wordreference.com/conj/frverbs.aspx?v="
                        (url-hexify-string (if (listp word) (car word) word)))))

;;;###autoload
  (defun sacha-learn-lang-reverso (s)
    (interactive (list (if (region-active-p)
                           (buffer-substring (region-beginning) (region-end))
                         (word-at-point))))
    (browse-url (concat "https://www.reverso.net/text-translation#sl=fra&tl=eng&text="
                        (url-hexify-string s))))
;; Looking up words via wordreference:1 ends here

;; [[file:../Sacha.org::#writing-and-editing-learning-french-lexique][Lexique:1]]
  (defvar sacha-learn-lang-lexique-db "~/proj/french/lexique.db" "SQLite3 DB")
;; Lexique:1 ends here

;; [[file:../Sacha.org::#writing-and-editing-learning-french-lexique][Lexique:4]]
  (defvar sacha-learn-lang-csv-path "/home/sacha/proj/french/french_vocabulary_list.csv")
  (defvar sacha-learn-lang-known-lemmas nil)
;;;###autoload
  (defun sacha-learn-lang-lexique-lookup-db-exact (input)
    "Query the Lexique SQLite database for INPUT."
    (let ((db (sqlite-open sacha-learn-lang-lexique-db)))
      (prog1 (sqlite-select db
                            "SELECT ortho, lemme, genre, nombre, phon, syll, infover FROM lexique
                      WHERE ortho=? ORDER BY freqfilms2 DESC LIMIT 1"
                            (list input))
        (sqlite-close db))))

;;;###autoload
  (defun sacha-learn-lang-lexique-lookup-db-flat (input)
    "Query the Lexique SQLite database for INPUT."
    (let ((db (sqlite-open sacha-learn-lang-lexique-db)))
      (prog1 (sqlite-select db
                            "SELECT ortho, lemme, genre, nombre, phon, syll, infover FROM lexique
                      WHERE ortho_flat LIKE ? OR ortho LIKE ? ORDER BY freqfilms2 DESC LIMIT 50"
                            (list (concat (downcase input) "%")
                                  (concat (downcase input) "%")))
        (sqlite-close db))))

;;;###autoload
  (defun sacha-learn-lang-lexique-lookup-db-lemma (input)
    "Query the Lexique SQLite database for INPUT."
    (let ((db (sqlite-open sacha-learn-lang-lexique-db)))
      (prog1 (sqlite-select db
                            "SELECT ortho, lemme, genre, nombre, infover, phon, syll, ortho_flat FROM lexique
                      WHERE lemme LIKE ? ORDER BY freqfilms2 DESC LIMIT 50"
                            (list (concat input "%")))
        (sqlite-close db))))

;;;###autoload
  (defun sacha-learn-lang-lexique-complete-word ()
    (interactive)
    (let* ((selection
            (consult--read
             (consult--dynamic-collection
                 (lambda (input)
                   (modus-themes-with-colors
                     (mapcar (lambda (row)
                               (let ((word (nth 0 row))
                                     (gender (nth 2 row))
                                     (number (nth 3 row))
                                     (ipa (sacha-learn-lang-lexique-to-ipa (nth 5 row)))
                                     (infover (nth 6 row)))
                                 ;; Format the string for the completion buffer
                                 (cons
                                  (propertize
                                   (format "%-20s [%s] (%s)" word ipa
                                           (string-join
                                            (delq nil (list gender number infover))
                                            ", "))
                                   'consult--candidate word
                                   'face
                                   `(:background

                                     ,(pcase gender
                                        ("m" bg-blue-subtle)
                                        ("f" bg-magenta-subtle))))
                                  word)))
                             (sacha-learn-lang-lexique-lookup-db-flat input)))))
             :prompt "French word: "
             :initial (word-at-point)
             :sort nil
             :lookup #'consult--lookup-cdr
             :category 'french-word)))
      (when selection
        (when (called-interactively-p 'any)
          (when (word-at-point)
            (delete-region (save-excursion
                             (skip-syntax-backward "w")
                             (point))
                           (save-excursion
                             (skip-syntax-forward "w")
                             (point))))
          (insert selection))
        selection)))
;; Lexique:4 ends here

;; [[file:../Sacha.org::#writing-and-editing-learning-french-write-a-completion-at-point-function-for-french][STARTED Write a completion-at-point function for French:1]]
;;;###autoload
  (defun sacha-learn-lang-lexique-completion-at-point ()
    (let ((bounds (bounds-of-thing-at-point 'word)))
      (when bounds
        (list (car bounds)
              (cdr bounds)
              (mapcar (lambda (row)
                        (let ((word (nth 0 row))
                              (gender (nth 2 row))
                              (number (nth 3 row))
                              (ipa (sacha-learn-lang-lexique-to-ipa (nth 5 row)))
                              (infover (nth 6 row)))
                          word))
                      (sacha-learn-lang-lexique-lookup-db-flat
                       (buffer-substring-no-properties
                        (car bounds)
                        (cdr bounds))))
              :exclusive 'no))))
  (with-eval-after-load 'org
    (add-hook 'org-mode-hook
              (lambda ()
                (when (and (buffer-file-name) (string-match "journal-fr\\|french" (buffer-file-name)))
                  (add-hook 'completion-at-point-functions 'sacha-learn-lang-lexique-completion-at-point)))))
;; STARTED Write a completion-at-point function for French:1 ends here

;; [[file:../Sacha.org::#writing-and-editing-learning-french-highlight-and-count-new-words-in-journal-entries][Highlight and count new words in journal entries:1]]
;;;###autoload
  (defun sacha-learn-lang-load-known-lemmas ()
    "Parse the CSV and return a hash table of lemma -> info."
    (let* ((known (make-hash-table :test 'equal))
           (data (cdr (pcsv-parse-file sacha-learn-lang-csv-path))))
      (mapc
       (lambda (o)
         (unless (gethash (elt o 2) known)
           (puthash (elt o 2) o known))
         (unless (gethash (elt o 0) known)
           (puthash (elt o 0) o known)))
       data)
      (setq sacha-learn-lang-known-lemmas known)))

  (defvar sacha-learn-lang-ignore
    (with-temp-buffer
      (insert-file-contents "~/proj/french/ignored.txt")
      (split-string (string-trim (buffer-string)) "\n")))

;;;###autoload
  (defun sacha-learn-lang-lexique-to-ipa (s)
    (mapconcat (lambda (c)
                 (pcase c
                   (?O "ɔ")
                   (?E "ɛ")
                   (?° "ə")
                   (?2 "ø")
                   (?9 "œ")
                   (?S "ʃ")
                   (?5 "ɛ̃")
                   (?Z "ʒ")
                   (?@ "ɑ̃")
                   (?1 "œ̃")
                   (?§ "ɔ̃")
                   (?8 "ɥ")
                   (?R "ʁ")
                   (_ (char-to-string c))))
                 s ""))

  (defvar sacha-learn-lang-show-pronunciation t "*Non-nil means show pronunciation.")

;;;###autoload
  (defun sacha-learn-lang-highlight-new-words-in-subtree ()
    "Highlight words in the current subtree based on lexique.db and CSV data."
    (interactive)
    (save-excursion
      (sacha-learn-lang-remove-new-word-highlights)
      (if (org-entry-get-with-inheritance "DATE")
          (let* ((subtree-date (org-entry-get-with-inheritance "DATE"))
                 (known-lemmas (or sacha-learn-lang-known-lemmas (sacha-learn-lang-load-known-lemmas)))
                 (seen-so-far (make-hash-table :test 'equal))
                 (beg (save-excursion (org-back-to-heading t) (org-end-of-meta-data t) (point)))
                 (end (save-excursion (org-end-of-subtree t) (point)))
                 (count-new 0)
                 (count-words 0))
            (save-excursion
              (goto-char beg)
              (while (and (< (point) end) (re-search-forward "\\b[[:alpha:]-]+\\b" end t))
                (let* ((word (match-string 0))
                       (info (and (or
                                   (not (car (gethash (downcase word) known-lemmas)))
                                   (not (string>
                                         subtree-date
                                         (car (gethash (downcase word) known-lemmas)))))
                                   (car (sacha-learn-lang-lexique-lookup-db-exact (downcase word))))))
                  (setq count-words (1+ count-words))
                  (when info
                    (let* ((lemma (elt info 1))
                           (gender (elt info 2))
                           (syll (propertize (concat " (" (elt info 5) ")")
                                             'face
                                             'modus-themes-fg-cyan-faint
                                             'keymap
                                             sacha-learn-lang-overlay-map
                                             ))
                           (csv-date (car (gethash lemma known-lemmas)))
                           (is-new (and (or (null csv-date)
                                            (and subtree-date (not (string> subtree-date
                                                                            csv-date))))
                                        (not (gethash lemma seen-so-far))
                                        (not (member lemma sacha-learn-lang-ignore)))))
                      (when is-new
                        (puthash lemma (list subtree-date word lemma) seen-so-far)
                        (puthash lemma (list subtree-date word lemma) sacha-learn-lang-known-lemmas)
                        (puthash word (list subtree-date word lemma) sacha-learn-lang-known-lemmas)
                        (setq count-new (1+ count-new))
                        (let ((ov (make-overlay (match-beginning 0) (match-end 0)))
                              (face (cond
                                     ((string-equal gender "m") 'modus-themes-subtle-blue)
                                     ((string-equal gender "f") 'modus-themes-subtle-magenta)
                                     (t 'modus-themes-subtle-green))))
                          (overlay-put ov 'sacha-learn-lang-highlight t)
                          (overlay-put ov 'word word)
                          (overlay-put ov 'evaporate t)
                          (when sacha-learn-lang-show-pronunciation
                            (overlay-put ov 'after-string (sacha-learn-lang-lexique-to-ipa syll)))
                          (overlay-put ov 'face face)))))))
              (org-back-to-heading)
              (let ((ov (make-overlay (line-beginning-position) (line-end-position))))
                (overlay-put ov 'after-string (format " + %d = %d" count-new count-words))
                (overlay-put ov 'sacha-learn-lang-highlight t))
              (when (called-interactively-p 'any)
                (message "%d total words, %d new lemmas" count-words count-new))
              (cons count-new count-words)))
        (let ((data (org-map-entries #'sacha-learn-lang-highlight-new-words-in-subtree "DATE={.}" 'tree)))
          (org-back-to-heading)
          (let ((ov (make-overlay (line-beginning-position) (line-end-position))))
            (overlay-put ov 'evaporate t)
            (overlay-put ov 'after-string
                         (format " + %d = %d"
                                 (apply '+ (mapcar 'car data))
                                 (apply '+ (mapcar 'cdr data))))
            (overlay-put ov 'sacha-learn-lang-highlight t))))))

;;;###autoload
    (defun sacha-learn-lang-remove-new-word-highlights ()
      "Remove all word highlights created by `sacha-learn-lang-highlight-new-words-in-subtree'."
      (interactive)
      (let ((beg (save-excursion (org-back-to-heading t) (point)))
            (end (save-excursion (org-end-of-subtree t) (point))))
        (remove-overlays beg end 'sacha-learn-lang-highlight t)))
;; Highlight and count new words in journal entries:1 ends here

;; [[file:../Sacha.org::#writing-and-editing-learning-french-correct-encoding-errors][Correct encoding errors:1]]
;;;###autoload
  (defun sacha-learn-lang-repair-french-encoding-full ()
    "Repair double and single encoded UTF-8 sequences, including uppercase."
    (interactive)
    (save-excursion
      (let ((case-fold-search t)
      (pairs '(("ÃƒÂ©" . "é") ("ÃƒÂ‰" . "É")
               ("ÃƒÂ " . "à") ("ÃƒÂ€" . "À")
               ("Ã¨" . "è")
               ("Ãª" . "ê")
               ("Ã" . "Ç")
               ("Ã§" . "ç")
               ("Ã»" . "û")
               ("ÃƒÂ¨" . "è") ("ÃƒÂˆ" . "È")
               ("ÃƒÂ§" . "ç") ("ÃƒÂ‡" . "Ç")
               ("Ã©" . "é")    ("Ã‰" . "É")
               ("Ã " . "à")    ("Ã€" . "À")
               ("â" . "'"))))
        (dolist (pair pairs)
          (goto-char (point-min))
          ;; 'nil' for literal search, 't' for case-sensitivity
          (while (search-forward (car pair) nil t)
            (replace-match (cdr pair) t))))))
;; Correct encoding errors:1 ends here

;; [[file:../Sacha.org::#writing-and-editing-speech-recognition-map-lang-gptel-feedback-from-the-logbook-to-kwiziq-topics][Map lang-gptel feedback from the logbook to KwizIQ topics:1]]
;;;###autoload
(defun sacha-org-collect-logbook-contents ()
  "Collect contents of all LOGBOOK drawers in the current subtree.
Returns them concatenated as a string."
  (save-excursion
    (org-back-to-heading t)
    (let ((subtree-end (save-excursion (org-end-of-subtree t t)))
          contents
          elem)
      (while (re-search-forward "^[ \t]*:LOGBOOK:[ \t]*$" subtree-end t)
        (setq elem (org-element-at-point))
        (push (buffer-substring-no-properties
               (org-element-contents-begin elem)
               (org-element-contents-end elem))
              contents))
      (string-join (nreverse contents) "\n"))))

;;;###autoload
(defun sacha-org-get-subtree (link)
  (save-window-excursion
		(save-excursion
			(org-link-open-from-string link)
	    (buffer-substring-no-properties (point) (progn (org-end-of-subtree) (point))))))

;;;###autoload
(defun sacha-learn-lang-gptel-analyze-feedback ()
  (interactive)
  (with-current-buffer (get-buffer-create "*Feedback*")
    (erase-buffer)
    (org-mode))
  (gptel-request
      (json-encode
       `(("feedback on previous mistakes" . ,(sacha-org-collect-logbook-contents))
         ("topic links" . ,(sacha-org-get-subtree "[[file:~/sync/orgzly/organizer.org::#kwiziq-a2]]"))
         ("prompt" . "Analyze the feedback on previous mistakes. Map them to the different topics and create a frequency table where column A has a link to the topic and column B has the number of errors in that category. For anything that doesn't match, summarize them in a separate list called Other. Also create a 10-item quiz covering the most important points. Hide answers like this: [[answer:the answer goes here][___]] Use Org Mode syntax.")))
    :callback (lambda (response info)
                (with-current-buffer (get-buffer-create "*Feedback*")
                  (insert response)
                  (goto-char (point-min))
                  (pop-to-buffer (current-buffer))))))
;; Map lang-gptel feedback from the logbook to KwizIQ topics:1 ends here

;; [[file:../Sacha.org::#writing-and-editing-speech-recognition-using-speech-recognition-for-translations-in-emacs-and-faking-in-buffer-completion-for-the-results][Using speech recognition for on-the-fly translations in Emacs and faking in-buffer completion for the results:1]]
;;;###autoload
(defun sacha-learn-lang-en-to-fr (text &optional display-only)
  (interactive (list (read-string "Text: ") current-prefix-arg))
  (let* ((url "https://translation.googleapis.com/language/translate/v2")
         (params `(("key" . ,(getenv "GOOGLE_API_KEY"))
                   ("q" . ,text)
                   ("source" . "en")
                   ("target" . "fr")
                   ("format" . "text")))
         (query-string (mapconcat
                        (lambda (pair)
                          (format "%s=%s"
                                  (url-hexify-string (car pair))
                                  (url-hexify-string (cdr pair))))
                        params
                        "&"))
         (full-url (concat url "?" query-string)))
    (let* ((response (plz 'get full-url :as #'json-read))
           (data (alist-get 'data response))
           (translations (alist-get 'translations data))
           (first-translation (car translations))
           (translated-text (alist-get 'translatedText first-translation)))
      (when (called-interactively-p 'any)
        (if display-only
            (message "%s" translated-text)
          (insert translated-text)))
      translated-text)))
;; Using speech recognition for on-the-fly translations in Emacs and faking in-buffer completion for the results:1 ends here

(provide 'sacha-learn-lang)
;;; sacha-learn-lang.el ends here
