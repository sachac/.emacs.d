;;;###autoload
(defun my-file-set (file)
	(let ((base (file-name-base file)))
		(seq-filter (lambda (o) (string= base (file-name-base o)))
								(directory-files (or (file-name-directory file) default-directory) t))))

;;;###autoload
(defun my-refresh-dired-buffers (dir)
	(dolist (buf (match-buffers '(derived-mode . dired-mode)))
		(when (string= default-directory dir)
			(revert-buffer))))

;;;###autoload
(defun my-delete-file-set (file)
	(interactive (list (read-file-name
											"File: "
											nil
											(if (derived-mode-p 'dired-mode)
													(dired-get-filename)
												(buffer-file-name)))))
	(mapc #'delete-file (my-file-set file))
	(when (and (buffer-file-name)
						 (string= (file-name-base (buffer-file-name))
											(file-name-base file)))
		(let ((buffer-modified-p nil))
			(kill-buffer)))
	(my-refresh-dired-buffers (file-name-directory file)))


;;;###autoload
(defun my-delete-current-file-set ()
	(interactive)
	(my-delete-file-set (if (derived-mode-p 'dired-mode)
													(dired-get-filename)
												(buffer-file-name))))

;;;###autoload
(defun my-rename-file-set (file new-prefix &optional force)
	(interactive (let ((file (read-file-name "File: ")))
								 (list
									file
									(read-file-name (format "New prefix (%s): "
																					(file-name-base file)))
									current-prefix-arg)))
	(unless force
		(dolist (file (my-file-set file))
			(let ((new-file (concat
											 new-prefix
											 "."
											 (file-name-extension file))))
				(when (and (not (string= file new-file))
									 (file-exists-p new-file))
					(error "%s already exists."
								 new-file)))))
	(dolist (file (my-file-set file))
		(let ((new-file (expand-file-name
										 (concat
											new-prefix
											"."
											(file-name-extension file)))))
			(when (not (string= file new-file))
				(rename-file file new-file t))))
	(my-refresh-dired-buffers (file-name-directory file))
	(concat
	 new-prefix
	 "."
	 (file-name-extension file)))

;;;###autoload
(defun my-rename-current-file-set (new-name)
	(interactive (list (read-string "New name: "
																	(file-name-base
																	 (if (derived-mode-p 'dired-mode)
																			 (dired-get-filename)
																		 (buffer-file-name))))))
	(my-rename-file-set (if (derived-mode-p 'dired-mode)
													(dired-get-filename)
												(buffer-file-name))
											new-name))

;;;###autoload
(defun my-move-current-file-set (new-dir)
  (interactive (list (read-file-name "Destination: " "~/proj/" nil t nil 'file-directory-p)))
	(dolist (file (my-file-set
	               (if (derived-mode-p 'dired-mode)
										 (dired-get-filename)
									 (buffer-file-name))))
    (rename-file file (expand-file-name (file-name-nondirectory file) new-dir))))
