;;; sacha-writing.el ---  -*- lexical-binding: t -*-

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
;; - Emacs: Cycle through different paragraph formats: all on one line, wrapped, max one sentence per line, one sentence per line
;;   https://sachachua.com/dotemacs#unfill-paragraph
;;
;;; Code:



;; [[file:../Sacha.org::#unfill-paragraph][Emacs: Cycle through different paragraph formats: all on one line, wrapped, max one sentence per line, one sentence per line:1]]
  (defvar sacha-repeat-counter '()
    "How often `sacha-repeat-next' was called in a row using the same command.
  This is an alist of (cat count list) so we can use it for different functions.")

;;;###autoload
  (defun sacha-unfill-paragraph ()
    "Replace newline chars in current paragraph by single spaces.
  This command does the inverse of `fill-paragraph'."
    (interactive)
    (let ((fill-column most-positive-fixnum))
      (fill-paragraph)))

;;;###autoload
  (defun sacha-fill-paragraph-semlf-long ()
          (interactive)
          (let ((fill-column most-positive-fixnum))
                  (fill-paragraph-semlf)))

;;;###autoload
  (defun sacha-repeat-next (category &optional element-list reset)
          "Return the next element for CATEGORY.
  Initialize with ELEMENT-LIST if this is the first time."
          (let* ((counter
                                          (or (assoc category sacha-repeat-counter)
                                                          (progn
                                                                  (push (list category -1 element-list)
                                                                                          sacha-repeat-counter)
                                                                  (assoc category sacha-repeat-counter)))))
                  (setf (elt (cdr counter) 0)
                                          (mod
                                           (if reset 0 (1+ (elt (cdr counter) 0)))
                                           (length (elt (cdr counter) 1))))
                  (elt (elt (cdr counter) 1) (elt (cdr counter) 0))))

;;;###autoload
  (defun sacha-in-prefixed-comment-p ()
    (or (member 'font-lock-comment-delimiter-face (face-at-point nil t))
                          (member 'font-lock-comment-face (face-at-point nil t))
                          (save-excursion
                                  (beginning-of-line)
                                  (comment-search-forward (line-end-position) t))))

  ;; It might be nice to figure out what state we're
  ;; in and then cycle to the next one if we're just
  ;; working with a single paragraph. In the
  ;; meantime, just going by repeats is fine.
;;;###autoload
  (defun sacha-reformat-paragraph-or-region ()
    "Cycles the paragraph between three states: filled/unfilled/fill-sentences.
  If a region is selected, handle all paragraphs within that region."
    (interactive)
          (let ((func (sacha-repeat-next 'sacha-reformat-paragraph
                                                                                                                          '(sacha-fill-paragraph-semlf-long
                                  fill-paragraph-semlf
                                  fill-paragraph
                                  sacha-unfill-paragraph)
                                                                                                                          (not (eq this-command last-command))))
                                  (deactivate-mark nil))
                  (if (region-active-p)
                                  (save-restriction
                                          (save-excursion
                                                  (narrow-to-region (region-beginning) (region-end))
                                                  (goto-char (point-min))
                                                  (while (not (eobp))
                                                          (skip-syntax-forward " ")
                                                          (let ((elem (and (derived-mode-p 'org-mode)
                                                                                                                           (org-element-context))))
                                                                  (cond
                                                                   ((eq (org-element-type elem) 'headline)
                                                                          (org-forward-paragraph))
                                                                   ((member (org-element-type elem)
                                                                                                          '(src-block export-block headline property-drawer))
                                                                          (goto-char
                                                                           (org-element-end (org-element-context))))
                                                                   (t
                                                                          (funcall func)
                                                                          (if fill-forward-paragraph-function
                                                                                          (funcall fill-forward-paragraph-function)
                                                                                  (forward-paragraph))))))))
                          (save-excursion
                                  (move-to-left-margin)
                                  (funcall func)))))

  (keymap-global-set "M-q" #'sacha-reformat-paragraph-or-region)
;; Emacs: Cycle through different paragraph formats: all on one line, wrapped, max one sentence per line, one sentence per line:1 ends here

(provide 'sacha-writing)
;;; sacha-writing.el ends here
