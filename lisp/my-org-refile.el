;;;###autoload
(defun my-org-refile-to-point (refloc)
	"Prompt for a heading and refile it to point."
	(interactive (list (org-refile-get-location "Heading: ")))
	(let* ((file (nth 1 refloc))
				 (pos (nth 3 refloc)))
		(save-excursion
			(with-current-buffer (find-file-noselect file 'nowarn)
				(save-excursion
					(save-restriction
						(widen)
						(goto-char pos)
						(org-copy-subtree 1 t))))
			(org-paste-subtree nil nil nil t))))

;;;###autoload
(defun my-org-bounce-to-file (file)
  "Toggle subtree between its home file and another file.
Limitations: Reinserts entry at bottom of subtree, uses kill ring."
  (interactive (list (read-file-name "File: ")))
  (if (string= (buffer-file-name) (expand-file-name file))
      ;; Return it
      (let ((location (org-entry-get (point) "BOUNCE")))
        (when location
          (setq location (read location))
          (org-cut-subtree)
          (save-buffer)
          (with-current-buffer (find-file (car location))
            (save-restriction
              (widen)
              (goto-char (org-find-olp location))
              (org-end-of-subtree)
              (unless (bolp) (insert "\n"))
              (org-paste-subtree (length location) nil nil t)
              (save-buffer)))))
    (org-entry-put (point) "BOUNCE" (prin1-to-string (cons (buffer-file-name) (org-get-outline-path))))
    (org-cut-subtree)
    (save-buffer)
    (with-current-buffer (find-file file)
      (save-restriction
        (widen)
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (org-yank)
        (save-buffer)))))

;; Example: (org-refile 4 nil (my-org-refile-get-location-by-substring "Other Emacs"))
;;;###autoload
(defun my-org-refile-get-location-by-substring (regexp &optional file)
  "Return the refile location identified by REGEXP."
  (let ((org-refile-targets org-refile-targets) tbl)
    (setq org-refile-target-table (org-refile-get-targets)))
  (unless org-refile-target-table
    (user-error "No refile targets"))
  (cl-find regexp org-refile-target-table
           :test
           (lambda (a b)
             (and
              (string-match a (car b))
              (or (null file)
                  (string-match file (elt b 1)))))))
;;;###autoload
(defun my-org-refile-subtree-to (name)
  (org-refile nil nil (my-org-refile-get-location-exact name)))

;;;###autoload
(defun my-org-refile-get-location-exact (name &optional file)
  "Return the refile location identified by NAME."
  (let ((org-refile-targets org-refile-targets) tbl)
    (setq org-refile-target-table (org-refile-get-targets)))
  (unless org-refile-target-table
    (user-error "No refile targets"))
  (cl-find name org-refile-target-table
           :test (lambda (a b)
                   (and (string-equal a (car b))
                        (or (null file)
                            (string-match file (elt b 1)))))))
;; Example: (my-org-clock-in-refile "Off my computer")
;;;###autoload
(defun my-org-clock-in-refile (location &optional file)
  "Clocks into LOCATION.
        LOCATION and FILE can also be regular expressions for `my-org-refile-get-location-by-substring'."
  (interactive (list (my-org-refile-get-location)))
  (save-window-excursion
    (save-excursion
      (if (stringp location) (setq location (my-org-refile-get-location-by-substring location file)))
      (org-refile 4 nil location)
      (org-clock-in))))

;;;###autoload
(defun my-org-finish-previous-task-and-clock-in-new-one (location &optional file)
  (interactive (list (my-org-refile-get-location)))
  (save-window-excursion
    (org-clock-goto)
    (org-todo 'done))
  (my-org-clock-in-and-track-by-name location file))

;;;###autoload
(defun my-org-clock-in-and-track-by-name (location &optional file)
  (interactive (list (my-org-refile-get-location)))
  (save-window-excursion
    (save-excursion
      (if (stringp location) (setq location (my-org-refile-get-location-exact location file)))
      (org-refile 4 nil location)
      (my-org-clock-in-and-track))))
;;;###autoload
(defun my-org-off-my-computer (category)
  (interactive "MCategory: ")
  (eval-when-compile (require 'quantified nil t))
  (my-org-clock-in-refile "Off my computer")
  (quantified-track category))

;;;###autoload
(defun my-org-jump ()
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively 'org-refile)))

;;;###autoload
(defun my-org-refile-to-subset (arg)
	"Refile to a smaller set of targets."
	(interactive "P")
	(let ((org-refile-targets '(("~/sync/orgzly/organizer.org" . (:tag . "inboxtarget"))
															("~/sync/orgzly/organizer.org" . (:maxlevel . 3))
															("~/sync/orgzly/resources.org" . (:maxlevel . 1))
															(nil . (:level . 1))
															("~/proj/stream/index.org" . (:maxlevel . 3))
															("~/sync/emacs/Inbox.org" . (:maxlevel . 1))
															("~/sync/emacs/Sacha.org" . (:maxlevel . 4))
															("~/sync/orgzly/people.org" . (:maxlevel . 2)))))
		(org-refile arg)))

;;;###autoload
(defun my-org-refile-to-target-or-subset (&optional arg)
	(interactive "P")
	(or (my-org-refile-current-entry-to-tag-target)
			(my-org-refile-to-subset arg)))


(defcustom my-org-refile-tag-targets nil
	"Searches and IDs."
	:group 'sacha
	:type '(repeat (cons string string string)))


(defvar my-org-tag-target-files
	nil
	"Files to check for tag targets.")
;;;###autoload
(defun my-org-update-tag-targets ()
	(interactive)
	(let ((org-agenda-files my-org-tag-target-files))
		(setq my-org-refile-tag-targets
					(let (list)
						(org-map-entries
						 (lambda ()
							 (list (concat "+" (org-entry-get (point) "TAG_TARGET"))
										 (org-id-get-create)
										 (org-entry-get (point) "ITEM")))
						 "TAG_TARGET={.}" 'agenda))))
	(customize-save-variable 'my-org-refile-tag-targets my-org-refile-tag-targets))

;;;###autoload
(defun my-org-add-tag-target (tag)
	(interactive "MTag: ")
	(org-entry-put (point) "TAG_TARGET" tag)
	(push (list (concat "+" tag)
							(org-id-get-create)
							(org-entry-get (point) "ITEM"))
				my-org-refile-tag-targets)
	(customize-save-variable 'my-org-refile-tag-targets my-org-refile-tag-targets))

;;;###autoload
(defun my-org-refile-current-entry-to-tag-target (&optional arg target-marker)
	(interactive (list current-prefix-arg (cadr (my-org-tag-target-for-entry-at-point))))
	(unless target-marker
		(setq target-marker (cadr (my-org-tag-target-for-entry-at-point))))
	(when (stringp target-marker)
		(setq target-marker (org-id-find target-marker t)))
	(when target-marker
		(org-refile
		 arg nil
		 (with-current-buffer (marker-buffer target-marker)
			 (goto-char target-marker)
			 (list (org-get-heading)
						 (buffer-file-name (marker-buffer target-marker))
						 nil
						 target-marker)))))

;; Based on https://emacs.stackexchange.com/questions/36360/recursively-refiling-all-subtrees-with-tag-to-a-destination-org-mode
;;;###autoload
(defun my-org-refile-matches-to-heading (match target-heading-id &optional scope copy)
  "Refile all headings within SCOPE (per `org-map-entries') to TARGET-HEADING-ID."
  (if-let (target-marker (org-id-find target-heading-id t))
      (let* ((target-rfloc (with-current-buffer (marker-buffer target-marker)
                             (goto-char target-marker)
                             (list (org-get-heading)
                                   (buffer-file-name (marker-buffer target-marker))
                                   nil
                                   target-marker)))
             (headings-to-copy (org-map-entries (lambda () (point-marker)) match scope)))
        (mapc
         (lambda (heading-marker)
           (with-current-buffer (marker-buffer heading-marker)
             (goto-char heading-marker)
             (org-refile nil nil target-rfloc (when copy "Copy"))))
         (nreverse headings-to-copy))
        (message "%s %d headings!"
                 (if copy "Copied" "Refiled")
                 (length headings-to-copy)))
    (warn "Could not find target heading %S" target-heading-id)))

;;;###autoload
(defun my-org-tag-target-for-entry-at-point ()
	"Return the `my-org-refile-tag-targets' entry that matches point."
	(let ((tags	(org-get-tags (point)))
				(level (org-current-level))
				(todo (org-get-todo-state))
				matcher)
		(catch 'found
			(dolist (target my-org-refile-tag-targets)
				(setq matcher (cdr (org-make-tags-matcher (car target))))
				(when (funcall matcher todo tags level)
					(throw 'found target))))))

;;;###autoload
(defun my-org-refile-to-tag-targets ()
	(interactive)
	(dolist (rule my-org-refile-tag-targets)
		(my-org-refile-matches-to-heading (car rule) (cadr rule))))

;;;###autoload
(defun my-org-refile-inbox-to-tag-targets ()
	(interactive)
	(with-current-buffer (find-file-noselect my-org-inbox-file)
		(dolist (rule my-org-refile-tag-targets)
			(my-org-refile-matches-to-heading (car rule) (cadr rule) 'file))))

;;;###autoload
(defun my-org-move-line-to-destination ()
  "Moves the current list item to DESTINATION in the current buffer.
If no DESTINATION is found, move it to the end of the list
and indent it one level."
  (interactive)
  (save-window-excursion
    (save-excursion
      (let ((string
             (buffer-substring-no-properties
              (line-beginning-position) (line-end-position)))
            (case-fold-search nil)
            found)
        (delete-region (line-beginning-position) (1+ (line-end-position)))
        (save-excursion
          (goto-char (point-min))
          (when (re-search-forward "DESTINATION" nil t)
            (insert "\n" (make-string (- (match-beginning 0) (line-beginning-position)) ?\ ) (s-trim string))
            (setq found t)))
        (unless found
          (org-end-of-item-list)
          (insert string "\n"))))))


;;;###autoload
(defun my-org-move-line-to-end-of-list ()
  "Move the current list item to the end of the list."
  (interactive)
  (save-excursion
    (let ((string (buffer-substring-no-properties (line-beginning-position)
                                                  (line-end-position))))
      (delete-region (line-beginning-position) (1+ (line-end-position)))
      (org-end-of-item-list)
      (insert string))))


;;;###autoload
(defun my-org-file-blog-index-entries ()
  "Keep filing until I press `C-g'."
  (interactive)
  (while t
    (my-org-file-blog-index-entry
     (line-beginning-position) (1+ (line-end-position))
     (let ((org-refile-targets
            '(("~/proj/sharing/blog.org" . (:maxlevel . 3)))))
       (save-excursion (org-refile-get-location "Location"))))))

;;;###autoload
(defun my-org-file-blog-index-entry (beg end location)
  "Copy entries into blog.org."
  (interactive
   (list
    (if (region-active-p) (point) (line-beginning-position))
    (if (region-active-p) (mark) (1+ (line-end-position)))
    (let ((org-refile-targets
           '(("~/proj/sharing/blog.org" . (:maxlevel . 3)))))
      (save-excursion (org-refile-get-location "Location")))))
  (let ((s
         (replace-regexp-in-string
          "^[ \t]*- \\(\\[X\\] \\)?"
          "- [X] "
          (buffer-substring-no-properties beg end))))
    ;; if we're already in blog.org, delete the previous entry
    (if (string= buffer-file-name (expand-file-name "~/proj/sharing/blog.org"))
        (delete-region beg end))
    (save-window-excursion
      (save-excursion
        (find-file (nth 1 location))
        (save-excursion
          (save-restriction
            (widen)
            (goto-char (nth 3 location))
            (re-search-forward org-list-full-item-re nil t)
            (goto-char (line-beginning-position))
            (insert s)
            (org-update-statistics-cookies nil)))))))

;;;###autoload
(defun my-org-refile-in-file (&optional prefix)
  "Refile to a target within the current file."
  (interactive)
	(let ((org-refile-targets (list (cons nil '(:maxlevel . 5)))))
		(call-interactively 'org-refile)))

;;;###autoload
(defun my-org-refile-to-previous ()
  "Refile subtree to last position from `my-org-refile-in-file'."
  (interactive)
  (save-selected-window
    (when (eq major-mode 'org-agenda-mode)
      (org-agenda-switch-to))
    (org-cut-subtree)
		(save-window-excursion
			(save-excursion
				(bookmark-jump (plist-get org-bookmark-names-plist :last-refile))
				(let ((level (org-current-level)))
					(org-end-of-subtree t t)
					(org-paste-subtree))))))
