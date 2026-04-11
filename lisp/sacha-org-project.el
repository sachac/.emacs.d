;;; sacha-org-project.el ---  -*- lexical-binding: t -*-

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

;;; Code:



;; [[file:../Sacha.org::org-project-link][org-project-link]]
(defvar sacha-project-web-base-list nil "Local path . web repo URLs for easy linking.")

;;;###autoload
(defmacro sacha-org-project-link (type file-path git-url)
  `(progn
		 (defun ,(intern (format "sacha-org-project-%s-complete" type)) ()
			 ,(format "Complete a file from %s." type)
			 (concat ,type ":" (completing-read "File: "
																					(projectile-project-files ,file-path))))
		 (defun ,(intern (format "sacha-org-project-%s-follow" type)) (link _)
			 ,(format "Open a file from %s." type)
			 (find-file
				(expand-file-name
				 link
				 ,file-path)))
		 (defun ,(intern (format "sacha-org-project-%s-export" type)) (link desc format _)
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
				:complete (quote ,(intern (format "sacha-org-project-%s-complete" type)))
				:export (quote ,(intern (format "sacha-org-project-%s-export" type)))
				:follow (quote ,(intern (format "sacha-org-project-%s-follow" type))))
		 (cl-pushnew (cons (expand-file-name ,file-path) ,git-url)
								 sacha-project-web-base-list
								 :test 'equal)))
;; org-project-link ends here

(provide 'sacha-org-project)
;;; sacha-org-project.el ends here
