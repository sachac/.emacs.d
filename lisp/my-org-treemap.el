(defvar my-org-treemap-temp-file "~/Downloads/treemap.html") ; Firefox inside Snap can't access /tmp
(defvar my-org-treemap-command "treemap" "Executable to generate a treemap.")

;;;###autoload
(defun my-org-treemap-include-p (node)
	(not (or (eq (org-element-property :todo-type node) 'done)
					 (member "notree" (org-element-property :tags node))
					 (org-element-property-inherited :archivedp node 'with-self))))

;;;###autoload
(defun my-org-treemap-data (node &optional path)
	"Output the size of headings underneath this one."
	(let ((sub
				 (apply
					'append
					(org-element-map
							(org-element-contents node)
							'(headline)
						(lambda (child)
							(if (my-org-treemap-include-p child)
									(my-org-treemap-data
									 child
									 (append path
													 (list
														(org-no-properties
														 (org-element-property :raw-value node)))))
								(list
								 (list
									(-
									 (org-element-end child)
									 (org-element-begin child))
									(string-join
									 (cdr
										(append path
														(list
														 (org-no-properties
															(org-element-property :raw-value node))
														 (org-no-properties
															(org-element-property :raw-value child)))))
									 "/")
									nil))))
						nil nil 'headline))))
		(append
		 (list
			(list
			 (-
				(org-element-end node)
				(org-element-begin node)
				(apply '+ (mapcar 'car sub))
				)
			 (string-join
				(cdr
				 (append path
								 (list
									(org-no-properties (org-element-property :raw-value node)))))
				"/")
			 (my-org-treemap-include-p node)))
		 sub)))

;;;###autoload
(defun my-org-treemap ()
	"Generate a treemap."
	(interactive)
	(save-excursion
		(goto-char (point-min))
		(let ((file (expand-file-name (expand-file-name my-org-treemap-temp-file)))
					(data (cdr (my-org-treemap-data (org-element-parse-buffer)))))
			(with-temp-file file
				(call-process-region
				 (mapconcat
					(lambda (entry)
						(if (elt entry 2)
								(format "%d %s\n" (car entry)
												(replace-regexp-in-string org-link-bracket-re "\\2" (cadr entry)))
							""))
					data
					"")
				 nil
				 my-org-treemap-command nil t t))
			(browse-url (concat "file://" (expand-file-name my-org-treemap-temp-file))))))
