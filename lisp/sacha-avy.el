;;; sacha-avy.el ---  -*- lexical-binding: t -*-

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
;; - Quickly jump to positions
;;   https://sachachua.com/dotemacs#quickly-jump-to-positions
;;
;;; Code:



;; [[file:../Sacha.org::#quickly-jump-to-positions][Quickly jump to positions:2]]
;;;###autoload
(defun sacha-avy-action-copy-whole-line (pt)
	"From https://karthinks.com/software/avy-can-do-anything/"
  (save-excursion
    (goto-char pt)
    (cl-destructuring-bind (start . end)
        (bounds-of-thing-at-point 'line)
      (copy-region-as-kill start end)))
  (select-window
   (cdr
    (ring-ref avy-ring 0)))
  t)

;;;###autoload
(defun sacha-avy-action-yank-whole-line (pt)
	"From https://karthinks.com/software/avy-can-do-anything/"
  (sacha-avy-action-copy-whole-line pt)
  (save-excursion (yank))
  t)
;; Quickly jump to positions:2 ends here

(provide 'sacha-avy)
;;; sacha-avy.el ends here
