;;; sacha-org-package.el ---  -*- lexical-binding: t -*-

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
;; - Package links
;;   https://sachachua.com/dotemacs#package-links
;;
;;; Code:



;; [[file:../Sacha.org::org-package-link][org-package-link]]
;;;###autoload
(defun sacha-org-package-open (package-name)
  (interactive "MPackage name: ")
  (describe-package (intern package-name)))

(ert-deftest sacha-org-package-export ()
  (should
   (string=
    (sacha-org-package-export "transcribe" "transcribe" 'html)
    "<a target=\"_blank\" href=\"https://elpa.gnu.org/packages/transcribe.html\">transcribe</a>"
    ))
  (should
   (string=
    (sacha-org-package-export "fireplace" "fireplace" 'html)
    "<a target=\"_blank\" href=\"http://melpa.org/#/fireplace\">fireplace</a>"
    )))
;;;###autoload
(defun sacha-org-package-export (link description format &optional arg)
  (let* ((package-info (car (assoc-default (intern link) package-archive-contents)))
         (package-source (and package-info (package-desc-archive package-info)))
         (path (format
                (cond
								 ((null package-source) link)
                 ((string= package-source "gnu") "https://elpa.gnu.org/packages/%s.html")
                 ((string= package-source "melpa") "https://melpa.org/#/%s")
                 ((string= package-source "nongnu") "https://elpa.nongnu.org/nongnu/%s.html")
                 (t (error 'unknown-source)))
                link))
         (desc (or description link)))
		(if package-source
				(cond
				 ((eq format '11ty) (format "<a target=\"_blank\" href=\"%s\">%s</a>" path desc))
				 ((eq format 'html) (format "<a target=\"_blank\" href=\"%s\">%s</a>" path desc))
				 ((eq format 'wp) (format "<a target=\"_blank\" href=\"%s\">%s</a>" path desc))
				 ((eq format 'latex) (format "\\href{%s}{%s}" path desc))
				 ((eq format 'texinfo) (format "@uref{%s,%s}" path desc))
				 ((eq format 'ascii) (format "%s <%s>" desc path))
				 ((eq format 'org) (org-link-make-string (concat "package:" link) description))
				 (t path))
			desc)))
;;;###autoload
(defun sacha-org-package-complete ()
	(require 'finder-inf nil t)
  (unless package--initialized
    (package-initialize t))
	(concat
	 "package:"
   ;; Load the package list if necessary (but don't activate them).
   (let ((packages (mapcar #'symbol-name (mapcar #'car package-archive-contents))))
		 (completing-read "Package: "
                      packages nil t nil nil))))

;;;###autoload
(defun sacha-org-package-link-description (link description)
	(unless description
		(when (string-match "package:\\(.+\\)" link)
			(match-string 1 link))))
;; org-package-link ends here

(provide 'sacha-org-package)
;;; sacha-org-package.el ends here
