(defvar my-supernote-export-dir "~/Dropbox/Supernote/EXPORT")
(defvar my-dropbox-sketches-dir "~/Dropbox/sketches")
;;;###autoload
(defun my-dropbox-sketches-dired () (interactive) (dired my-dropbox-sketches-dir))
;;;###autoload
(defun my-latest-sketch (&optional skip-download)
	(interactive "P")
	(let ((file
				 (or (condition-case nil
								 (and (not skip-download) (my-supernote-download-latest-exported-file))
							 (error nil))
						 (my-latest-file (list my-supernote-export-dir
																	 my-dropbox-sketches-dir)
														 "png\\|svg\\|jpe?g"))))
		(when (called-interactively-p 'any)
			(find-file file))
		file))

;;;###autoload
(defun my-supernote-process-latest (&optional skip-download)
  (interactive "P")
	(my-sketch-process (my-latest-sketch skip-download)))

;;;###autoload
(defun my-supernote-open-latest ()
	(interactive)
	(find-file-other-window
	 (or (my-supernote-download-latest-exported-file)
			 (my-latest-file (list my-supernote-export-dir
														 my-dropbox-sketches-dir)
											 "png\\|svg\\|jpe?g"))))

;;;###autoload
(defun my-supernote-export-dired ()
  (interactive)
  (dired my-supernote-export-dir "-tl"))

;;;###autoload
(defun my-open-latest-export ()
  (interactive)
  (find-file (my-latest-file "~/Dropbox/Supernote/EXPORT")))

;;;###autoload
(defun my-copy-latest-export-filename ()
  (interactive)
  (kill-new (my-latest-file "~/Dropbox/Supernote/EXPORT")))

;;;###autoload
(defun my-supernote-copy-latest-download ()
  (interactive)
  (call-process "sn" nil nil nil (my-latest-file "~/Downloads"))
	(message "%s" (my-latest-file "~/Downloads")))

(defvar my-supernote-inbox "~/Dropbox/Supernote/INBOX")
;;;###autoload
(defun my-save-manpage-to-supernote (path)
	(interactive (list (woman-file-name nil)))
	(unless (file-exists-p path) (setq path (woman-file-name path)))
	(let* ((base (file-name-base path))
				 (temp-html (make-temp-file base nil ".html")))
		(with-temp-buffer
			(insert-file-contents path)
			(call-process-region (point-min) (point-max) "man2html" t t)
			(when (re-search-backward "Invalid Man Page" nil t)
				(delete-file temp-html)
				(error "Could not convert."))
			(write-file temp-html))
		(call-process "ebook-convert" nil (get-buffer-create "*temp*") nil temp-html
									(expand-file-name (concat base ".epub") my-supernote-inbox))
		(delete-file temp-html)))

;;;###autoload
(defun my-supernote-save-info (path)
	(interactive (list (read-file-name "Texi: " nil nil
																		 (and Info-current-file
																					(file-exists-p (concat Info-current-file ".texi"))
																					(concat Info-current-file ".texi"))
																		 nil
																		 (lambda (f)
																			 (or
																				(string-match "\\.texi\\'" f)
																				(file-directory-p f))))))
	(call-process "texi2pdf" nil "*temp*" t (expand-file-name path)
								"-o"
								(expand-file-name (concat (file-name-base path) ".pdf")
																															my-supernote-inbox)))

(defvar my-supernote-css "~/proj/static-blog/assets/css/style.css")
;;;###autoload
(defun my-supernote-save ()
	(interactive)
	(cond
	 ((derived-mode-p 'Man-mode) (my-save-manpage-to-supernote Man-arguments))
	 ((derived-mode-p 'Info-mode)
		(my-supernote-save-info
		 (or (and Info-current-file
							(file-exists-p (concat Info-current-file ".texi"))
							(concat Info-current-file ".texi"))
				 (read-file-name
					"Texi: " nil nil nil nil
					(lambda (f)
						(or
						 (string-match "\\.texi\\'" f)
						 (file-directory-p f)))))))
	 ((derived-mode-p 'org-mode)
		(org-latex-export-to-pdf)
		(copy-file (concat (file-name-base (buffer-file-name)) ".pdf")
							 (expand-file-name (concat (file-name-base (buffer-file-name)) ".pdf")
																 my-supernote-inbox) t))
	 ((or (derived-mode-p 'html-mode)
				(derived-mode-p 'web-mode)
				(derived-mode-p 'markdown-mode))
		(call-process "pandoc" nil nil nil (buffer-file-name) "-t" "latex"
									"-o"
									(expand-file-name (concat (file-name-base (buffer-file-name)) ".pdf")
																		my-supernote-inbox)))
	 ((and (buffer-file-name) (string-match "\\.\\(pdf\\|epub\\)$" (buffer-file-name)))
		(copy-file (buffer-file-name)
							 (expand-file-name (file-name-nondirectory (buffer-file-name))
																 my-supernote-inbox)
							 t))
	 (t
		(let ((filename (expand-file-name
										 (concat (file-name-base (or (buffer-file-name)
																								 (format-time-string "%Y-%m-%d-%H-%M-%S")))
														 ".pdf")
										 my-supernote-inbox)))
			(with-current-buffer (htmlize-buffer)
				(call-process-region
				 (point-min) (point-max) "wkhtmltopdf" nil nil nil "--no-background" "-"
				 filename))))))


(defvar my-supernote-ip-address)
;;;###autoload
(defun my-supernote-upload (filename &optional supernote-path)
	(interactive "FFile: ")
	(setq supernote-path (or supernote-path "/INBOX"))
	(let* ((boundary (mml-compute-boundary '()))
				 (url-request-method "POST")
				 (url-request-extra-headers
					`(("Content-Type" . ,(format "multipart/form-data; boundary=%s" boundary))))
				 (url-request-data
					(mm-url-encode-multipart-form-data
					 `(("file" . (("name" . "file")
												("filename" . ,(file-name-nondirectory filename))
												("content-type" . "application/octet-stream")
												("filedata" . ,(with-temp-buffer
																				 (insert-file-contents-literally filename)
																				 (buffer-substring-no-properties (point-min) (point-max)))))))
					 boundary)))
		(condition-case nil
				(with-current-buffer
						(url-retrieve-synchronously
						 (format "http://%s:8089%s" my-supernote-ip-address supernote-path))
					(re-search-backward "^$")
					(prog1 (json-read)
						(kill-buffer)))
			(error
			 (copy-file filename (expand-file-name (file-name-nondirectory filename) my-supernote-inbox) t)
			 (message "Copied %s to %s, please sync" (file-name-nondirectory filename) my-supernote-inbox)))))

;;;###autoload
(defun my-supernote-org-upload-as-text (&optional async subtree visible-only body-only ext-plist)
	"Export Org format, but save it with a .txt extension."
	(interactive (list nil current-prefix-arg))
	(let ((filename (org-export-output-file-name ".txt" subtree))
				(text (org-export-as 'org subtree visible-only body-only ext-plist)))
		;; consider copying instead of exporting so that #+begin_export html etc. is preserved
		(with-temp-file filename
			(insert text))
		(my-supernote-upload filename)))

;;;###autoload
(defun my-supernote-org-upload-as-pdf (&optional async subtree visible-only body-only ext-plist)
	(interactive (list nil current-prefix-arg))
	(my-supernote-upload (org-latex-export-to-pdf async subtree visible-only body-only ext-plist)))

;;;###autoload
(defun my-supernote-org-upload-as-epub (&optional async subtree visible-only body-only ext-plist)
	(interactive (list nil current-prefix-arg))
	(my-supernote-upload (org-epub-export-to-epub async subtree visible-only ext-plist)))

(with-eval-after-load 'org
  (org-export-define-backend
		  'supernote nil
		  :menu-entry '(?S "Supernote"
										   ((?s "as PDF" my-supernote-org-upload-as-pdf)
											  (?e "as EPUB" my-supernote-org-upload-as-epub)
											  (?o "as Org" my-supernote-org-upload-as-text)))))

(defvar my-supernote-ip-address "192.168.1.221")
;;;###autoload
(defun my-supernote-get-exported-files ()
	(condition-case nil
			(let ((data (plz 'get (format "http://%s:8089/EXPORT" my-supernote-ip-address)))
						(list))
				(when (string-match "const json = '\\(.*\\)'" data)
					(sort
					 (alist-get 'fileList (json-parse-string (match-string 1 data) :object-type 'alist :array-type 'list))
					 :key (lambda (o) (alist-get 'date o))
					 :lessp 'string<
					 :reverse t)))
		(error nil)))

;;;###autoload
(defun my-supernote-download-latest-exported-file ()
	"Save exported file in downloads dir."
	(interactive)
	(let* ((info (car (my-supernote-get-exported-files)))
				 (dest-dir my-download-dir)
				 (new-file (and info (expand-file-name (file-name-nondirectory (alist-get 'name info)) dest-dir)))
				 renamed)
		(when info
			(copy-file
			 (plz 'get (format "http://%s:8089%s" my-supernote-ip-address
												 (alist-get 'uri info))
				 :as 'file)
			 new-file
			 t)
			new-file)))

;;;###autoload
(defun my-supernote-org-attach-latest-exported-file ()
	(interactive)
	;; save the file to the screenshot directory
	(let ((info (car (my-supernote-get-exported-files)))
				new-file
				renamed)
		;; delete matching files
		(setq new-file (expand-file-name
										(replace-regexp-in-string " " "%20" (alist-get 'name info) (org-attach-dir))))
		(when (file-exists-p new-file)
			(delete-file new-file))
		(org-attach-attach
		 (format "http://%s:8089%s" my-supernote-ip-address
						 (alist-get 'uri info))
		 nil
		 'url)
		(setq new-file (my-latest-file (org-attach-dir)))
		;; recolor
		(my-sketch-recolor-png new-file)
		;; autocrop that image
		(my-image-autocrop new-file)
		;; possibly rename
		(setq renamed (my-image-recognize-get-new-filename new-file))
		(when renamed
			(setq renamed (expand-file-name renamed (org-attach-dir)))
			(rename-file new-file renamed t)
			(my-image-store renamed) ; file it in my archive
			(setq new-file renamed))
		;; use a sketch link if it has an ID
		(if (string-match "^[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]-[0-9][0-9] "
											(file-name-base renamed))
				(org-insert-link nil (concat "sketchFull:" (file-name-base renamed)))
			;; insert the link
			(org-insert-link nil (concat "attachment:" (replace-regexp-in-string "#" "%23" (file-name-nondirectory new-file)))))
		(org-redisplay-inline-images)))

;;;###autoload
(defun my-supernote-org-insert-screenshot-from-mirror ()
	"Copy the current image from the SuperNote mirror."
	(interactive)
	(let ((filename (expand-file-name (format-time-string "%Y-%m-%d-%H-%M-%S.png") "~/recordings")))
		(shell-command-to-string (concat "NODE_PATH=/usr/lib/node_modules node ~/bin/supernote-screenshot.js " (shell-quote-argument filename)))
		;; trim it
		(call-process "mogrify" nil nil nil "-trim" "+repage" filename)
		(shell-command-to-string (concat "~/bin/recolor.py --colors c0c0c0,f6f396 " (shell-quote-argument filename)))
		(call-interactively 'my-org-insert-screenshot)))
