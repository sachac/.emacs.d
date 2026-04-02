;;; my-org-validate.el ---  -*- lexical-binding: t -*-

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
;; - Validation
;;   https://sachachua.com/dotemacs#org-mode-validation
;;
;; - Keep only unique headings
;;   https://sachachua.com/dotemacs#org-mode-validation-keep-only-unique-headings
;;
;; - No blank titles, no duplicate paths
;;   https://sachachua.com/dotemacs#org-mode-validation-no-blank-titles-no-duplicate-paths
;;
;;; Code:



;; [[file:../Sacha.org::#org-mode-validation][Validation:1]]
(defvar my-org-validate-functions
	'(my-org-validate-no-blank-titles
		my-org-validate-unique-outline-paths
		my-org-validate-no-syncthing-conflicts))
;;;###autoload
(defun my-org-validate ()
	(interactive)
	(unless (string-match "_archive\\'" (buffer-file-name))
		(run-hooks 'my-org-validate-functions)))
;; Validation:1 ends here

;; [[file:../Sacha.org::#org-mode-validation-keep-only-unique-headings][Keep only unique headings:1]]
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
;; Keep only unique headings:1 ends here

;; [[file:../Sacha.org::#org-mode-validation-no-blank-titles-no-duplicate-paths][No blank titles, no duplicate paths:1]]
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
		(let* (paths
					 org-outline-path-cache
					 (found (catch 'found
										(org-map-entries
										 (lambda ()
											 (let ((path (string-join (org-get-outline-path t t) "/")))
												 (if (member path paths)
														 (throw 'found (cons (point) path))
													 (push path paths)))))
										nil)))
			(if found
					(progn
						(goto-char (car found))
						(error "Duplicate found: %s" (cdr found))
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
;; No blank titles, no duplicate paths:1 ends here

(provide 'my-org-validate)
;;; my-org-validate.el ends here
