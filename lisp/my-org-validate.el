(defvar my-org-validate-functions
	'(my-org-validate-no-blank-titles
		my-org-validate-unique-outline-paths
		my-org-validate-no-syncthing-conflicts))
;;;###autoload
(defun my-org-validate ()
	(interactive)
	(unless (string-match "_archive\\'" (buffer-file-name))
		(run-hooks 'my-org-validate-functions)))

;;;###autoload
(defun my-compare-org-headings (file)
	(interactive "FOther file: ")
	(let ((current (org-map-entries (lambda () (org-entry-get (point) "ITEM")) "LEVEL=1" 'file)))
		(find-file file)
		(goto-char
		 (catch 'done
			 (org-map-entries
				(lambda ()
					(when (member (org-entry-get (point) "ITEM") current)
						(throw 'done (point))))
				"LEVEL=1" 'file)))))


;;;###autoload
(defun my-org-validate-no-blank-titles ()
	(interactive)
	(let ((point (point)))
		(goto-char (point-min))
		(while (re-search-forward org-heading-regexp nil t)
			(unless (match-string 2)
				(error "Empty title")))
		(goto-char point)))

;;;###autoload
(defun my-org-validate-unique-outline-paths ()
	(interactive)
	(let ((point (point)))
		(goto-char (point-min))
		(let* ((paths (make-hash-table :test 'equal))
           (org-outline-path-cache nil)
					 (found (catch 'found
										(org-map-entries
										 (lambda ()
											 (let ((path (string-join (org-get-outline-path t t) "/")))
												 (if (gethash path paths)
														 (throw 'found (cons (point) path))
													 (puthash path (point) paths)))))
										nil)))
			(if found
					(progn
						(goto-char (car found))
						(error "Duplicate found: %s - previous %d" (cdr found) (gethash (cdr found) paths))
						found)
				(goto-char point)
				(when (called-interactively-p 'any) (message "No duplicates"))))))

;;;###autoload
(defun my-org-delete-duplicate-outline-paths-interactively ()
	(interactive)
	(let ((point (point))
				previous)
		(goto-char (point-min))
		(org-map-entries
		 (lambda ()
			 (let ((path (string-join (org-get-outline-path t t) "/")))
				 (when (assoc-default path previous #'string=)
					 (when (y-or-n-p "Delete this possible duplicate? ")
						 (org-cut-subtree)))
				 (push (cons path (point)) previous))))))

;;;###autoload
(defun my-org-validate-no-syncthing-conflicts ()
	(when (directory-files default-directory nil "sync-conflict.*\\.org")
		(message "Syncthing conflicts exist.")))
