;;; sacha-wdiff.el ---  -*- lexical-binding: t -*-

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



;; [[file:../Sacha.org::#wdiff][Wdiff:1]]
(defvar sacha-wdiff-mode-font-lock-keywords
  `(("{\\+\\(.*?\\)\\+}" . 'diff-added)
    ("\\[\\-\\(.*?\\)\\-\\]" . 'diff-removed)))

(defconst sacha-wdiff-mode-font-lock-defaults
  '(sacha-wdiff-mode-font-lock-keywords t nil nil nil (font-lock-multiline . t)))

;;;###autoload
(define-derived-mode sacha-wdiff-mode fundamental-mode "Word diff" "Highlight word diffs."
	(setq-local font-lock-defaults sacha-wdiff-mode-font-lock-defaults))

;;;###autoload
(defun sacha-wdiff (old-file new-file)
	(interactive (list (read-file-name "Original: ")
										 (buffer-file-name)))
	(with-current-buffer (get-buffer-create "*wdiff*")
		(erase-buffer)
		(call-process "wdiff" nil t t (expand-file-name old-file)
									(expand-file-name new-file))
		(goto-char (point-min))
		(sacha-wdiff-mode)
		(switch-to-buffer (current-buffer))))

;;;###autoload
(defun sacha-wdiff-strings (original new)
  (let ((original-file (make-temp-file "wdiff"))
        (new-file (make-temp-file "wdiff")))
    (write-region original nil original-file)
    (write-region new nil new-file)
    (sacha-wdiff original-file new-file)
    (delete-file original-file)
    (delete-file new-file)))

;;;###autoload
(defun sacha-wdiff-org-text-with-clipboard ()
  (interactive)
  (sacha-wdiff-strings (sacha-org-subtree-text-without-blocks)
                    (car kill-ring)))

;;;###autoload
(defun sacha-wdiff-buffer-with-file ()
	(interactive)
	(let ((s (buffer-string))
				(temp-file (make-temp-file "temp")))
		(with-temp-file temp-file
			(insert s))
		(sacha-wdiff (buffer-file-name) temp-file)
		(delete-file temp-file)))

;;;###autoload
(defun sacha-wdiff-find-at-point ()
  (interactive)
  (unless (looking-at "\\[-")
    (re-search-backward "\\[-" nil t)
    (when (looking-at "\\[-\\(.+?\\)-\\] {\\+\\(.+?\\)\\+}")
      (let ((s (match-string 1))
            (rep (match-string 2)))
        (goto-char (match-end 0))
        (other-window 1)
        (if (re-search-forward (regexp-quote s) nil t)
            (progn
              (save-match-data (pulse-momentary-highlight-region (match-beginning 0)
                                                                 (match-end 0)))
              (when (save-match-data (y-or-n-p (format "Change %s to %s: " s rep)))
                (replace-match rep t t)
                t))
          (message "Could not find %s to change to %s" s rep)
          nil)))))

;;;###autoload
(defun sacha-wdiff-next ()
  (interactive)
  (other-window 1)
  (re-search-forward "{\\+\\(.+?\\)\\+}")
  (pulse-momentary-highlight-region (match-beginning 0) (match-end 0))
  (sacha-wdiff-find-at-point))

;;;###autoload
(defun sacha-wdiff-next-loop ()
  (interactive)
  (while (sacha-wdiff-next)))
;; Wdiff:1 ends here

(provide 'sacha-wdiff)
;;; sacha-wdiff.el ends here
