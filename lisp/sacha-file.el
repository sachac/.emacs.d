;;; sacha-file.el ---  -*- lexical-binding: t -*-

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
;; - Working with or renaming a set of files
;;   https://sachachua.com/dotemacs#multimedia-working-with-or-renaming-a-set-of-files
;;
;;; Code:



;; [[file:../Sacha.org::#multimedia-working-with-or-renaming-a-set-of-files][Working with or renaming a set of files:1]]
;;;###autoload
(defun sacha-file-set (file)
	(let ((base (file-name-base file)))
		(seq-filter (lambda (o) (string= base (file-name-base o)))
								(directory-files (or (file-name-directory file) default-directory) t))))

;;;###autoload
(defun sacha-refresh-dired-buffers (dir)
	(dolist (buf (match-buffers '(derived-mode . dired-mode)))
		(when (string= default-directory dir)
			(revert-buffer))))

;;;###autoload
(defun sacha-delete-file-set (file)
	(interactive (list (read-file-name
											"File: "
											nil
											(if (derived-mode-p 'dired-mode)
													(dired-get-filename)
												(buffer-file-name)))))
	(mapc #'delete-file (sacha-file-set file))
	(when (and (buffer-file-name)
						 (string= (file-name-base (buffer-file-name))
											(file-name-base file)))
		(let ((buffer-modified-p nil))
			(kill-buffer)))
	(sacha-refresh-dired-buffers (file-name-directory file)))


;;;###autoload
(defun sacha-delete-current-file-set ()
	(interactive)
	(sacha-delete-file-set (if (derived-mode-p 'dired-mode)
													(dired-get-filename)
												(buffer-file-name))))

;;;###autoload
(defun sacha-rename-file-set (file new-prefix &optional force)
	(interactive (let ((file (read-file-name "File: ")))
								 (list
									file
									(read-file-name (format "New prefix (%s): "
																					(file-name-base file)))
									current-prefix-arg)))
	(unless force
		(dolist (file (sacha-file-set file))
			(let ((new-file (concat
											 new-prefix
											 "."
											 (file-name-extension file))))
				(when (and (not (string= file new-file))
									 (file-exists-p new-file))
					(error "%s already exists."
								 new-file)))))
	(dolist (file (sacha-file-set file))
		(let ((new-file (expand-file-name
										 (concat
											new-prefix
											"."
											(file-name-extension file)))))
			(when (not (string= file new-file))
				(rename-file file new-file t))))
	(sacha-refresh-dired-buffers (file-name-directory file))
	(concat
	 new-prefix
	 "."
	 (file-name-extension file)))

;;;###autoload
(defun sacha-rename-current-file-set (new-name)
	(interactive (list (read-string "New name: "
																	(file-name-base
																	 (if (derived-mode-p 'dired-mode)
																			 (dired-get-filename)
																		 (buffer-file-name))))))
	(sacha-rename-file-set (if (derived-mode-p 'dired-mode)
													(dired-get-filename)
												(buffer-file-name))
											new-name))

;;;###autoload
(defun sacha-move-current-file-set (new-dir)
  (interactive (list (read-file-name "Destination: " "~/proj/" nil t nil 'file-directory-p)))
	(dolist (file (sacha-file-set
	               (if (derived-mode-p 'dired-mode)
										 (dired-get-filename)
									 (buffer-file-name))))
    (rename-file file (expand-file-name (file-name-nondirectory file) new-dir))))
;; Working with or renaming a set of files:1 ends here

(provide 'sacha-file)
;;; sacha-file.el ends here
