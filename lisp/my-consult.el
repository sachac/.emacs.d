;;; my-consult.el ---  -*- lexical-binding: t -*-

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
;; - Quickly search my code
;;   https://sachachua.com/dotemacs#org-mode-links-using-an-emacs-lisp-macro-to-define-quick-custom-org-mode-links-to-project-files-plus-urls-and-search-quickly-search-my-code
;;
;;; Code:



;; [[file:../Sacha.org::#org-mode-links-using-an-emacs-lisp-macro-to-define-quick-custom-org-mode-links-to-project-files-plus-urls-and-search-quickly-search-my-code][Quickly search my code:1]]
;;;###autoload
(defun my-consult-ripgrep-code ()
  (interactive)
	(consult-ripgrep (mapcar 'car my-project-web-base-list)))
;; Quickly search my code:1 ends here

(provide 'my-consult)
;;; my-consult.el ends here
