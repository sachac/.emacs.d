;;; sacha-org-sketch.el ---  -*- lexical-binding: t -*-

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
;; - Save edited text for sketch post
;;   https://sachachua.com/dotemacs#multimedia-images-save-edited-text-for-sketch-post
;;
;;; Code:



;; [[file:../Sacha.org::#multimedia-images-save-edited-text-for-sketch-post][Save edited text for sketch post:1]]
;;;###autoload
(defun sacha-org-sketch-open-text-file (sketch)
  (interactive (list (sacha-complete-sketch-filename)))
	(find-file (concat (file-name-sans-extension sketch) ".txt"))
	(with-current-buffer (find-file-noselect sketch)
		(display-buffer-in-side-window
		 (current-buffer)
		 '((window-width . 0.5)
			 (side . right)))))
;; Save edited text for sketch post:1 ends here

(provide 'sacha-org-sketch)
;;; sacha-org-sketch.el ends here
