;;; sacha-org-hint.el ---  -*- lexical-binding: t -*-

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
;; - Custom Org link type for hints (and sound effects)
;;   https://sachachua.com/dotemacs#streaming
;;
;;; Code:



;; [[file:../Sacha.org::*Custom Org link type for hints (and sound effects)][Custom Org link type for hints (and sound effects):1]]
(defun sacha-org-hint-export (path desc format _)
	"Export hint."
	(pcase format
   ((or 'html '11ty 'md)
	  (format "<label class=\"hint\"><input type=\"checkbox\"> <span class=\"hint-desc\">%s</span><span class=\"hint-text\">%s</span></label>" desc path))
	('ascii
		desc)))

(defvar sacha-org-hint-functions nil
  "Functions to call with the hint as the argument.")

(defface sacha-org-hint-face
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for hints.")

(defun sacha-org-hint-open (path)
  "Display the hint at PATH."
  (let ((overlay (car (org-find-overlays 'sacha-org-hint)))
        (text (replace-regexp-in-string "^hint:" "" path))
        elem)
    (if overlay
        (delete-overlay overlay)
      (setq elem (org-element-context))
      (setq overlay (make-overlay (org-element-begin elem) (org-element-end elem)))
      (overlay-put overlay 'display text)
      (overlay-put overlay 'evaporate t)
      (overlay-put overlay 'sacha-org-hint text)
      (overlay-put overlay 'face 'sacha-org-hint-face)
      (run-hook-with-args 'sacha-org-hint-functions text))))

(defun sacha-org-hint-reset ()
  "Remove all hint overlays"
  (interactive)
  (remove-overlays (point-min) (point-max) 'sacha-org-hint))

(defun sacha-org-hint-play-sound (text)
  "Play sound for TEXT.
Match it against `sacha-org-hint-sound-alist'."
  (when-let* ((sound (assoc-default text sacha-org-hint-sound-alist #'string=)))
    (start-process "mpv" nil "mpv" (expand-file-name sound) "--no-video" "--force-window=no")))
;; Custom Org link type for hints (and sound effects):1 ends here

(provide 'sacha-org-hint)
;;; sacha-org-hint.el ends here
