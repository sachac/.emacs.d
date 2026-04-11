;;; sacha-org-treemap.el ---  -*- lexical-binding: t -*-

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
;; - Treemap visualization
;;   https://sachachua.com/dotemacs#org-mode-diagrams-and-graphics-treemap-visualization
;;
;;; Code:



;; [[file:../Sacha.org::#org-mode-diagrams-and-graphics-treemap-visualization][Treemap visualization:1]]
(defvar sacha-org-treemap-temp-file "~/Downloads/treemap.html") ; Firefox inside Snap can't access /tmp
(defvar sacha-org-treemap-command "treemap" "Executable to generate a treemap.")

;;;###autoload
(defun sacha-org-treemap-include-p (node)
	(not (or (eq (org-element-property :todo-type node) 'done)
					 (member "notree" (org-element-property :tags node))
					 (org-element-property-inherited :archivedp node 'with-self))))

;;;###autoload
(defun sacha-org-treemap-data (node &optional path)
	"Output the size of headings underneath this one."
	(let ((sub
				 (apply
					'append
					(org-element-map
							(org-element-contents node)
							'(headline)
						(lambda (child)
							(if (sacha-org-treemap-include-p child)
									(sacha-org-treemap-data
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
			 (sacha-org-treemap-include-p node)))
		 sub)))

;;;###autoload
(defun sacha-org-treemap ()
	"Generate a treemap."
	(interactive)
	(save-excursion
		(goto-char (point-min))
		(let ((file (expand-file-name (expand-file-name sacha-org-treemap-temp-file)))
					(data (cdr (sacha-org-treemap-data (org-element-parse-buffer)))))
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
				 sacha-org-treemap-command nil t t))
			(browse-url (concat "file://" (expand-file-name sacha-org-treemap-temp-file))))))
;; Treemap visualization:1 ends here

(provide 'sacha-org-treemap)
;;; sacha-org-treemap.el ends here
