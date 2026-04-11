(defvar my-wdiff-mode-font-lock-keywords
  `(("{\\+\\(.*?\\)\\+}" . 'diff-added)
    ("\\[\\-\\(.*?\\)\\-\\]" . 'diff-removed)))

(defconst my-wdiff-mode-font-lock-defaults
  '(my-wdiff-mode-font-lock-keywords t nil nil nil (font-lock-multiline . t)))

;;;###autoload
(define-derived-mode my-wdiff-mode fundamental-mode "Word diff" "Highlight word diffs."
	(setq-local font-lock-defaults my-wdiff-mode-font-lock-defaults))

;;;###autoload
(defun my-wdiff (old-file new-file)
	(interactive (list (read-file-name "Original: ")
										 (buffer-file-name)))
	(with-current-buffer (get-buffer-create "*wdiff*")
		(erase-buffer)
		(call-process "wdiff" nil t t (expand-file-name old-file)
									(expand-file-name new-file))
		(goto-char (point-min))
		(my-wdiff-mode)
		(switch-to-buffer (current-buffer))))

;;;###autoload
(defun my-wdiff-strings (original new)
  (let ((original-file (make-temp-file "wdiff"))
        (new-file (make-temp-file "wdiff")))
    (write-region original nil original-file)
    (write-region new nil new-file)
    (my-wdiff original-file new-file)
    (delete-file original-file)
    (delete-file new-file)))

;;;###autoload
(defun my-wdiff-org-text-with-clipboard ()
  (interactive)
  (my-wdiff-strings (my-org-subtree-text-without-blocks)
                    (car kill-ring)))

;;;###autoload
(defun my-wdiff-buffer-with-file ()
	(interactive)
	(let ((s (buffer-string))
				(temp-file (make-temp-file "temp")))
		(with-temp-file temp-file
			(insert s))
		(my-wdiff (buffer-file-name) temp-file)
		(delete-file temp-file)))

;;;###autoload
(defun my-wdiff-find-at-point ()
  (interactive)
  (unless (looking-at "\\[-")
    (re-search-backward "\\[-" nil t)
    (when (looking-at "\\[-\\(.+?\\)-\\] {\\+\\(.+?\\)\\+}")
      (let ((s (match-string 1))
            (rep (match-string 2)))
        (goto-char (match-end 0))
        (other-window 1)
        (if (re-search-forward (regexp-quote s) nil t)
            (progn
              (save-match-data (pulse-momentary-highlight-region (match-beginning 0)
                                                                 (match-end 0)))
              (when (save-match-data (y-or-n-p (format "Change %s to %s: " s rep)))
                (replace-match rep t t)
                t))
          (message "Could not find %s to change to %s" s rep)
          nil)))))

;;;###autoload
(defun my-wdiff-next ()
  (interactive)
  (other-window 1)
  (re-search-forward "{\\+\\(.+?\\)\\+}")
  (pulse-momentary-highlight-region (match-beginning 0) (match-end 0))
  (my-wdiff-find-at-point))

;;;###autoload
(defun my-wdiff-next-loop ()
  (interactive)
  (while (my-wdiff-next)))
