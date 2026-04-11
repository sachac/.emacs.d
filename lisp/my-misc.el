;;;###autoload
  (defun my-org-link-qr (url)
          "Display a QR code for URL in a buffer."
          (let ((buf (save-window-excursion (qrencode--encode-to-buffer (my-org-stored-link-as-url url)))))
                  (if (> (frame-width) 80)
                                  (display-buffer-in-side-window buf '((side . right)))
                          (display-buffer buf))))

;;;###autoload
  (defun my-store-action-key+cmd (cmd)
    (setq keycast--this-command-keys (this-single-command-keys) keycast--this-command cmd))
;;;###autoload
  (defun my-force-keycast-update (&rest _)
    (force-mode-line-update t))

  (defvar my-copy-append-string nil "String to append when copying.")
;;;###autoload
  (defun my-copy-and-append (beg end append)
          (interactive
           (list
                  (if (region-active-p) (region-beginning) (point-min))
                  (if (region-active-p) (region-end) (point-max))
                  (if current-prefix-arg (read-string "Append: ")
                          my-copy-append-string)))
          (when append
                  (setq my-copy-append-string append))
          (kill-new (concat (buffer-substring beg end) append)))

;;;###autoload
  (defun my-pdf-view-insert-current-page-text ()
          (interactive)
          (let (text)
                  (catch 'found
                          (walk-window-tree
                           (lambda (win)
                                   (with-selected-window win
                                           (when (derived-mode-p 'pdf-view-mode)
                                                   (setq text
                                                                           (pdf-info-gettext (pdf-view-current-page)
                                                                                                                                                   (list 0 0 1 1)))
                                                   (throw 'found text))))))
                  (when text (save-excursion (insert text)))))

;;;###autoload
  (defun my-replace-with-latest-download ()
    "Replace file contents with latest download."
    (interactive)
    (widen)
    (erase-buffer)
    (insert-file-contents (my-latest-file my-download-dir)))

;;;###autoload
  (defun prot/keyboard-quit-dwim ()
    "Do-What-I-Mean behaviour for a general `keyboard-quit'.

  The generic `keyboard-quit' does not do the expected thing when
  the minibuffer is open.  Whereas we want it to close the
  minibuffer, even without explicitly focusing it.

  The DWIM behaviour of this command is as follows:

  - When the region is active, disable it.
  - When a minibuffer is open, but not focused, close the minibuffer.
  - When the Completions buffer is selected, close it.
  - In every other case use the regular `keyboard-quit'."
    (interactive)
    (cond
     ((region-active-p)
      (keyboard-quit))
     ((derived-mode-p 'completion-list-mode)
      (delete-completion-window))
     ((> (minibuffer-depth) 0)
      (abort-recursive-edit))
     (t
      (keyboard-quit))))

  (define-key global-map (kbd "C-g") #'prot/keyboard-quit-dwim)

(defun my-capitalize-dwim ()
  "Capitalize the previous word if at the end of a word."
  (interactive)
  (if (region-active-p)
      (capitalize-region (region-beginning) (region-end))
    (when (and (not (bolp))
               (looking-back "\\w" 1)
               (not (eq last-command 'my-capitalize-dwim)))
      (backward-word))
    (capitalize-word 1)))

(defun my-copy-filename ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (cond
   ((derived-mode-p 'dired-mode) (dired-copy-filename-as-kill 0))
   (t (kill-new (buffer-file-name)))))

;;;###autoload
  (defun my-split-string-keep-delimiters (string delimiter)
    (when string
      (let (results pos)
        (with-temp-buffer
          (insert string)
          (goto-char (point-min))
          (setq pos (point-min))
          (while (re-search-forward delimiter nil t)
            (push (buffer-substring pos (match-beginning 0)) results)
            (setq pos (match-beginning 0)))
          (push (buffer-substring pos (point-max)) results)
          (nreverse results)))))

  (ert-deftest my-split-string-keep-delimiters ()
   (should
    (equal (my-split-string-keep-delimiters
            "Beaucoup de gens ont une réaction forte contre l'IA pour plusieurs raisons qui *incluent* le battage médiatique excessif dont elle fait l'objet, son utilisation à mauvais escient, et *l'inondation de banalité* qu'elle produit."
            ", \\| que \\| qui \\| qu'ils? \\| qu'elles? \\| qu'on "
            )
   )))

;;;###autoload
(defun my-file-start-time (filename &optional base-date)
  "Return the local time based on FILENAME."
  (setq filename (file-name-base filename))
  (cond
   ((string-match "^\\([0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]\\)[-T]\\([0-9][0-9][\\.-][0-9][0-9]\\(?:[\\.-][0-9][0-9]\\)?\\)" filename)
    (date-to-time (concat (match-string 1 filename) "T"
                          (replace-regexp-in-string "[\\.-]" ":" (match-string 2 filename)))))
   ((string-match "^\\(?:Copy of \\)?\\([^ ][^ ][^ ]\\)[^ ]+ at \\([0-9]+\\)-\\([0-9]+\\)" filename)
    (let* ((day (match-string 1 filename))
           (hour (match-string 2 filename))
           (min (match-string 3 filename))
           (changed-time (or base-date (file-attribute-modification-time
                                        (file-attributes filename))))
           (decoded-time (decode-time changed-time)))
      ;; get the day on or before changed-time
      (if (string= (format-time-string "%a" changed-time) day)
          (encode-time (append
                        (list
                         0
                         (string-to-number min)
                         (string-to-number hour))
                        (seq-drop decoded-time 3)))
        ;; synchronized maybe within the week after
        (let ((org-read-date-prefer-future nil))
          (org-read-date t t
                         (concat "--" day " " hour ":" min)
                         nil changed-time)))))))

(ert-deftest my-file-start-time ()
  (should
   (equal (format-time-string "%Y-%m-%d %H:%M:%S"
                              (my-file-start-time "2024-01-05-09-46-59.flv"))
          "2024-01-05 09:46:59"))
  (should
   (equal (format-time-string "%Y-%m-%d %H:%M:%S"
                              (my-file-start-time "2024-01-08T12.49.vtt"))
          "2024-01-08 12:49:00"))
  (should
   (equal (format-time-string "%Y-%m-%d %H:%M:%S"
                              (my-file-start-time "Sunday at 15-30.vtt"
                                                  (date-to-time "2023-01-12")))
          "2023-01-08 15:30:00"))
  (should
   (time-equal-p (my-file-start-time "Sunday at 12-49.txt")
                 (org-read-date t t "-sun 12:49"))))

(defvar my-copy-append "" "String to append.")
;;;###autoload
(defun my-copy-and-append (beg end string)
	(interactive (list (if (region-active-p) (region-beginning) (point-min))
										 (if (region-active-p) (region-end) (point-max))
										 (if current-prefix-arg
												 (read-string "Append: ")
											 my-copy-append)))
	(setq my-copy-append string)
	(kill-new (concat (buffer-substring beg end) string)))

;;;###autoload
(defun my-replace-buffer-with-clipboard ()
	(interactive)
	(erase-buffer)
	(insert (car kill-ring)))

;;;###autoload
(defun my-align-non-space (beg end)
  "Align non-space columns in region BEG END."
  (interactive "r")
  (align-regexp beg end "\\(\\s-*\\)\\S-+" 1 1 t))

;;;###autoload
(defun my-set-sentence-end-double-space ()
	(setq-local sentence-end-double-space t))

;;;###autoload
(defun my-weekly-average (count start end)
  "Report weekly average for COUNT from START to END."
  (/ (* 7.0 count) (days-between end start)))

;;;###autoload
(defun my-format-intent (intent &optional params)
  "Return a command string for sending INTENT with PARAMS.
      PARAMS is an alist of (\"key\" . \"value\") pairs."
  (format "am broadcast --user 0 -a %s %s"
          intent
          (mapconcat
           (lambda (o)
             (format
              "-e %s %s"
              (shell-quote-argument (car o))
              (shell-quote-argument (cdr o))))
           params
           " ")))

;;;###autoload
(defun my-send-intent (intent &optional params)
  "Send broadcast INTENT to my phone.
      PARAMS is a plist of :key value pairs."
  (let ((command (my-format-intent intent params)))
    (if my-phone-p
        (shell-command command)
      (shell-command (format "ssh phone %s" (shell-quote-argument command))))))
