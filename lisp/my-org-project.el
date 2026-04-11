(defvar my-project-web-base-list nil "Local path . web repo URLs for easy linking.")

;;;###autoload
(defmacro my-org-project-link (type file-path git-url)
  `(progn
		 (defun ,(intern (format "my-org-project-%s-complete" type)) ()
			 ,(format "Complete a file from %s." type)
			 (concat ,type ":" (completing-read "File: "
																					(projectile-project-files ,file-path))))
		 (defun ,(intern (format "my-org-project-%s-follow" type)) (link _)
			 ,(format "Open a file from %s." type)
			 (find-file
				(expand-file-name
				 link
				 ,file-path)))
		 (defun ,(intern (format "my-org-project-%s-export" type)) (link desc format _)
			 "Export link to file."
			 (setq desc (or desc link))
			 (when (and ,git-url link)
				 (setq link (concat ,git-url (replace-regexp-in-string "^/" "" link))))
			 (pcase format
				 ((or 'html '11ty) (format "<a href=\"%s\">%s</a>"
																	 link
																	 (or desc link)))
				 ('md (if desc (format "[%s](%s)" desc link)
								(format "<%s>" link)))
				 ('latex (format "\\href{%s}{%s}" link desc))
				 ('texinfo (format "@uref{%s,%s}" link desc))
				 ('ascii (format "%s (%s)" desc link))
				 (_ (format "%s (%s)" desc link))))
		 (org-link-set-parameters
				,type
				:complete (quote ,(intern (format "my-org-project-%s-complete" type)))
				:export (quote ,(intern (format "my-org-project-%s-export" type)))
				:follow (quote ,(intern (format "my-org-project-%s-follow" type))))
		 (cl-pushnew (cons (expand-file-name ,file-path) ,git-url)
								 my-project-web-base-list
								 :test 'equal)))
