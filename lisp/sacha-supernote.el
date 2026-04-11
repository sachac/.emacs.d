;;; sacha-supernote.el ---  -*- lexical-binding: t -*-

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
;; Related Emacs config sections:
;;
;; - Supernote
;;   https://sachachua.com/dotemacs#supernote
;;
;; - Using Emacs Lisp to export TXT/EPUB/PDF from Org Mode to the Supernote via Browse and Access
;;   https://sachachua.com/dotemacs#supernote-org-upload
;;
;; - org-attaching the latest image from my Supernote via Browse and Access
;;   https://sachachua.com/dotemacs#supernote-browse
;;
;; - Using Puppeteer to grab an image from the SuperNote's screen mirror
;;   https://sachachua.com/dotemacs#using-puppeteer-to-grab-an-image-from-the-supernote-s-screen-mirror
;;
;;; Code:



;; [[file:../Sacha.org::#supernote][Supernote:1]]
(defvar sacha-supernote-export-dir "~/Dropbox/Supernote/EXPORT")
(defvar sacha-dropbox-sketches-dir "~/Dropbox/sketches")
;;;###autoload
(defun sacha-dropbox-sketches-dired () (interactive) (dired sacha-dropbox-sketches-dir))
;;;###autoload
(defun sacha-latest-sketch (&optional skip-download)
	(interactive "P")
	(let ((file
				 (or (condition-case nil
								 (and (not skip-download) (sacha-supernote-download-latest-exported-file))
							 (error nil))
						 (sacha-latest-file (list sacha-supernote-export-dir
																	 sacha-dropbox-sketches-dir)
														 "png\\|svg\\|jpe?g"))))
		(when (called-interactively-p 'any)
			(find-file file))
		file))

;;;###autoload
(defun sacha-supernote-process-latest (&optional skip-download)
  (interactive "P")
	(sacha-sketch-process (sacha-latest-sketch skip-download)))
;; Supernote:1 ends here

;; [[file:../Sacha.org::#supernote][Supernote:2]]
;;;###autoload
(defun sacha-supernote-open-latest ()
	(interactive)
	(find-file-other-window
	 (or (sacha-supernote-download-latest-exported-file)
			 (sacha-latest-file (list sacha-supernote-export-dir
														 sacha-dropbox-sketches-dir)
											 "png\\|svg\\|jpe?g"))))

;;;###autoload
(defun sacha-supernote-export-dired ()
  (interactive)
  (dired sacha-supernote-export-dir "-tl"))
;; Supernote:2 ends here

;; [[file:../Sacha.org::#supernote][Supernote:5]]
;;;###autoload
(defun sacha-open-latest-export ()
  (interactive)
  (find-file (sacha-latest-file "~/Dropbox/Supernote/EXPORT")))

;;;###autoload
(defun sacha-copy-latest-export-filename ()
  (interactive)
  (kill-new (sacha-latest-file "~/Dropbox/Supernote/EXPORT")))

;;;###autoload
(defun sacha-supernote-copy-latest-download ()
  (interactive)
  (call-process "sn" nil nil nil (sacha-latest-file "~/Downloads"))
	(message "%s" (sacha-latest-file "~/Downloads")))
;; Supernote:5 ends here

;; [[file:../Sacha.org::#supernote][Supernote:6]]
(defvar sacha-supernote-inbox "~/Dropbox/Supernote/INBOX")
;;;###autoload
(defun sacha-save-manpage-to-supernote (path)
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
									(expand-file-name (concat base ".epub") sacha-supernote-inbox))
		(delete-file temp-html)))
;; Supernote:6 ends here

;; [[file:../Sacha.org::#supernote][Supernote:7]]
;;;###autoload
(defun sacha-supernote-save-info (path)
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
																															sacha-supernote-inbox)))
;; Supernote:7 ends here

;; [[file:../Sacha.org::#supernote][Supernote:8]]
(defvar sacha-supernote-css "~/proj/static-blog/assets/css/style.css")
;;;###autoload
(defun sacha-supernote-save ()
	(interactive)
	(cond
	 ((derived-mode-p 'Man-mode) (sacha-save-manpage-to-supernote Man-arguments))
	 ((derived-mode-p 'Info-mode)
		(sacha-supernote-save-info
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
																 sacha-supernote-inbox) t))
	 ((or (derived-mode-p 'html-mode)
				(derived-mode-p 'web-mode)
				(derived-mode-p 'markdown-mode))
		(call-process "pandoc" nil nil nil (buffer-file-name) "-t" "latex"
									"-o"
									(expand-file-name (concat (file-name-base (buffer-file-name)) ".pdf")
																		sacha-supernote-inbox)))
	 ((and (buffer-file-name) (string-match "\\.\\(pdf\\|epub\\)$" (buffer-file-name)))
		(copy-file (buffer-file-name)
							 (expand-file-name (file-name-nondirectory (buffer-file-name))
																 sacha-supernote-inbox)
							 t))
	 (t
		(let ((filename (expand-file-name
										 (concat (file-name-base (or (buffer-file-name)
																								 (format-time-string "%Y-%m-%d-%H-%M-%S")))
														 ".pdf")
										 sacha-supernote-inbox)))
			(with-current-buffer (htmlize-buffer)
				(call-process-region
				 (point-min) (point-max) "wkhtmltopdf" nil nil nil "--no-background" "-"
				 filename))))))

;; Supernote:8 ends here

;; [[file:../Sacha.org::#supernote-org-upload][Using Emacs Lisp to export TXT/EPUB/PDF from Org Mode to the Supernote via Browse and Access:1]]
(defvar sacha-supernote-ip-address)
;;;###autoload
(defun sacha-supernote-upload (filename &optional supernote-path)
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
						 (format "http://%s:8089%s" sacha-supernote-ip-address supernote-path))
					(re-search-backward "^$")
					(prog1 (json-read)
						(kill-buffer)))
			(error
			 (copy-file filename (expand-file-name (file-name-nondirectory filename) sacha-supernote-inbox) t)
			 (message "Copied %s to %s, please sync" (file-name-nondirectory filename) sacha-supernote-inbox)))))
;; Using Emacs Lisp to export TXT/EPUB/PDF from Org Mode to the Supernote via Browse and Access:1 ends here

;; [[file:../Sacha.org::#supernote-org-upload][Using Emacs Lisp to export TXT/EPUB/PDF from Org Mode to the Supernote via Browse and Access:2]]
;;;###autoload
(defun sacha-supernote-org-upload-as-text (&optional async subtree visible-only body-only ext-plist)
	"Export Org format, but save it with a .txt extension."
	(interactive (list nil current-prefix-arg))
	(let ((filename (org-export-output-file-name ".txt" subtree))
				(text (org-export-as 'org subtree visible-only body-only ext-plist)))
		;; consider copying instead of exporting so that #+begin_export html etc. is preserved
		(with-temp-file filename
			(insert text))
		(sacha-supernote-upload filename)))

;;;###autoload
(defun sacha-supernote-org-upload-as-pdf (&optional async subtree visible-only body-only ext-plist)
	(interactive (list nil current-prefix-arg))
	(sacha-supernote-upload (org-latex-export-to-pdf async subtree visible-only body-only ext-plist)))

;;;###autoload
(defun sacha-supernote-org-upload-as-epub (&optional async subtree visible-only body-only ext-plist)
	(interactive (list nil current-prefix-arg))
	(sacha-supernote-upload (org-epub-export-to-epub async subtree visible-only ext-plist)))
;; Using Emacs Lisp to export TXT/EPUB/PDF from Org Mode to the Supernote via Browse and Access:2 ends here

;; [[file:../Sacha.org::#supernote-org-upload][Using Emacs Lisp to export TXT/EPUB/PDF from Org Mode to the Supernote via Browse and Access:3]]
(with-eval-after-load 'org
  (org-export-define-backend
		  'supernote nil
		  :menu-entry '(?S "Supernote"
										   ((?s "as PDF" sacha-supernote-org-upload-as-pdf)
											  (?e "as EPUB" sacha-supernote-org-upload-as-epub)
											  (?o "as Org" sacha-supernote-org-upload-as-text)))))
;; Using Emacs Lisp to export TXT/EPUB/PDF from Org Mode to the Supernote via Browse and Access:3 ends here

;; [[file:../Sacha.org::#supernote-browse][org-attaching the latest image from my Supernote via Browse and Access:2]]
(defvar sacha-supernote-ip-address "192.168.1.221")
;;;###autoload
(defun sacha-supernote-get-exported-files ()
	(condition-case nil
			(let ((data (plz 'get (format "http://%s:8089/EXPORT" sacha-supernote-ip-address)))
						(list))
				(when (string-match "const json = '\\(.*\\)'" data)
					(sort
					 (alist-get 'fileList (json-parse-string (match-string 1 data) :object-type 'alist :array-type 'list))
					 :key (lambda (o) (alist-get 'date o))
					 :lessp 'string<
					 :reverse t)))
		(error nil)))
;; org-attaching the latest image from my Supernote via Browse and Access:2 ends here

;; [[file:../Sacha.org::#supernote-browse][org-attaching the latest image from my Supernote via Browse and Access:4]]
;;;###autoload
(defun sacha-supernote-download-latest-exported-file ()
	"Save exported file in downloads dir."
	(interactive)
	(let* ((info (car (sacha-supernote-get-exported-files)))
				 (dest-dir sacha-download-dir)
				 (new-file (and info (expand-file-name (file-name-nondirectory (alist-get 'name info)) dest-dir)))
				 renamed)
		(when info
			(copy-file
			 (plz 'get (format "http://%s:8089%s" sacha-supernote-ip-address
												 (alist-get 'uri info))
				 :as 'file)
			 new-file
			 t)
			new-file)))
;; org-attaching the latest image from my Supernote via Browse and Access:4 ends here

;; [[file:../Sacha.org::#supernote-browse][org-attaching the latest image from my Supernote via Browse and Access:6]]
;;;###autoload
(defun sacha-supernote-org-attach-latest-exported-file ()
	(interactive)
	;; save the file to the screenshot directory
	(let ((info (car (sacha-supernote-get-exported-files)))
				new-file
				renamed)
		;; delete matching files
		(setq new-file (expand-file-name
										(replace-regexp-in-string " " "%20" (alist-get 'name info) (org-attach-dir))))
		(when (file-exists-p new-file)
			(delete-file new-file))
		(org-attach-attach
		 (format "http://%s:8089%s" sacha-supernote-ip-address
						 (alist-get 'uri info))
		 nil
		 'url)
		(setq new-file (sacha-latest-file (org-attach-dir)))
		;; recolor
		(sacha-sketch-recolor-png new-file)
		;; autocrop that image
		(sacha-image-autocrop new-file)
		;; possibly rename
		(setq renamed (sacha-image-recognize-get-new-filename new-file))
		(when renamed
			(setq renamed (expand-file-name renamed (org-attach-dir)))
			(rename-file new-file renamed t)
			(sacha-image-store renamed) ; file it in my archive
			(setq new-file renamed))
		;; use a sketch link if it has an ID
		(if (string-match "^[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]-[0-9][0-9] "
											(file-name-base renamed))
				(org-insert-link nil (concat "sketchFull:" (file-name-base renamed)))
			;; insert the link
			(org-insert-link nil (concat "attachment:" (replace-regexp-in-string "#" "%23" (file-name-nondirectory new-file)))))
		(org-redisplay-inline-images)))
;; org-attaching the latest image from my Supernote via Browse and Access:6 ends here

;; [[file:../Sacha.org::#using-puppeteer-to-grab-an-image-from-the-supernote-s-screen-mirror][Using Puppeteer to grab an image from the SuperNote's screen mirror:2]]
;;;###autoload
(defun sacha-supernote-org-insert-screenshot-from-mirror ()
	"Copy the current image from the SuperNote mirror."
	(interactive)
	(let ((filename (expand-file-name (format-time-string "%Y-%m-%d-%H-%M-%S.png") "~/recordings")))
		(shell-command-to-string (concat "NODE_PATH=/usr/lib/node_modules node ~/bin/supernote-screenshot.js " (shell-quote-argument filename)))
		;; trim it
		(call-process "mogrify" nil nil nil "-trim" "+repage" filename)
		(shell-command-to-string (concat "~/bin/recolor.py --colors c0c0c0,f6f396 " (shell-quote-argument filename)))
		(call-interactively 'sacha-org-insert-screenshot)))
;; Using Puppeteer to grab an image from the SuperNote's screen mirror:2 ends here

(provide 'sacha-supernote)
;;; sacha-supernote.el ends here
