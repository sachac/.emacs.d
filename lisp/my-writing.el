  (defvar my-repeat-counter '()
    "How often `my-repeat-next' was called in a row using the same command.
  This is an alist of (cat count list) so we can use it for different functions.")

;;;###autoload
  (defun my-unfill-paragraph ()
    "Replace newline chars in current paragraph by single spaces.
  This command does the inverse of `fill-paragraph'."
    (interactive)
    (let ((fill-column most-positive-fixnum))
      (fill-paragraph)))

;;;###autoload
  (defun my-fill-paragraph-semlf-long ()
          (interactive)
          (let ((fill-column most-positive-fixnum))
                  (fill-paragraph-semlf)))

;;;###autoload
  (defun my-repeat-next (category &optional element-list reset)
          "Return the next element for CATEGORY.
  Initialize with ELEMENT-LIST if this is the first time."
          (let* ((counter
                                          (or (assoc category my-repeat-counter)
                                                          (progn
                                                                  (push (list category -1 element-list)
                                                                                          my-repeat-counter)
                                                                  (assoc category my-repeat-counter)))))
                  (setf (elt (cdr counter) 0)
                                          (mod
                                           (if reset 0 (1+ (elt (cdr counter) 0)))
                                           (length (elt (cdr counter) 1))))
                  (elt (elt (cdr counter) 1) (elt (cdr counter) 0))))

;;;###autoload
  (defun my-in-prefixed-comment-p ()
    (or (member 'font-lock-comment-delimiter-face (face-at-point nil t))
                          (member 'font-lock-comment-face (face-at-point nil t))
                          (save-excursion
                                  (beginning-of-line)
                                  (comment-search-forward (line-end-position) t))))

  ;; It might be nice to figure out what state we're
  ;; in and then cycle to the next one if we're just
  ;; working with a single paragraph. In the
  ;; meantime, just going by repeats is fine.
;;;###autoload
  (defun my-reformat-paragraph-or-region ()
    "Cycles the paragraph between three states: filled/unfilled/fill-sentences.
  If a region is selected, handle all paragraphs within that region."
    (interactive)
          (let ((func (my-repeat-next 'my-reformat-paragraph
                                                                                                                          '(my-fill-paragraph-semlf-long
                                  fill-paragraph-semlf
                                  fill-paragraph
                                  my-unfill-paragraph)
                                                                                                                          (not (eq this-command last-command))))
                                  (deactivate-mark nil))
                  (if (region-active-p)
                                  (save-restriction
                                          (save-excursion
                                                  (narrow-to-region (region-beginning) (region-end))
                                                  (goto-char (point-min))
                                                  (while (not (eobp))
                                                          (skip-syntax-forward " ")
                                                          (let ((elem (and (derived-mode-p 'org-mode)
                                                                                                                           (org-element-context))))
                                                                  (cond
                                                                   ((eq (org-element-type elem) 'headline)
                                                                          (org-forward-paragraph))
                                                                   ((member (org-element-type elem)
                                                                                                          '(src-block export-block headline property-drawer))
                                                                          (goto-char
                                                                           (org-element-end (org-element-context))))
                                                                   (t
                                                                          (funcall func)
                                                                          (if fill-forward-paragraph-function
                                                                                          (funcall fill-forward-paragraph-function)
                                                                                  (forward-paragraph))))))))
                          (save-excursion
                                  (move-to-left-margin)
                                  (funcall func)))))

  (keymap-global-set "M-q" #'my-reformat-paragraph-or-region)
