;;; my-transcribe.el ---  -*- lexical-binding: t -*-

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
;; - Remove filler words at the start and upcase the next word
;;   https://sachachua.com/dotemacs#filler-start
;;
;; - Split up oops better
;;   https://sachachua.com/dotemacs#split-up-oops-better
;;
;;; Code:



;; [[file:../Sacha.org::#filler-start][Remove filler words at the start and upcase the next word:1]]
(defvar my-filler-words-regexp "\\(\\. \\|^\\)\\(?:So?\\|And\\|You know\\|Uh\\)\\(?:,\\|\\.\\.\\.\\)? \\(.\\)")
;;;###autoload
(defun my-remove-filler-words-at-start ()
	(interactive)
	(save-excursion
		(let ((case-fold-search nil))
			(while (re-search-forward my-filler-words-regexp nil t)
				(if (and (called-interactively-p) (not current-prefix-arg))
						(let ((overlay (make-overlay (match-beginning 0)
																				 (match-end 0))))
							(overlay-put overlay 'common-edit t)
              (overlay-put overlay 'evaporate t)
							(overlay-put
							 overlay 'display
							 (propertize (concat (match-string 0) " -> "
																	 (match-string 1)
																	 (upcase (match-string 2)))
													 'face 'modus-themes-mark-sel))
							(unwind-protect
									(pcase (save-match-data (read-char-choice "Replace (y/n/!/q)? " "yn!q"))
										(?!
										 (replace-match (concat (match-string 1) (upcase (match-string 2))) t)
										 (while (re-search-forward my-filler-words-regexp nil t)
											 (replace-match (concat (match-string 1) (upcase (match-string 2))) t)))
										(?y
										 (replace-match (concat (match-string 1) (upcase (match-string 2))) t))
										(?n nil)
										(?q (goto-char (point-max))))
								(delete-overlay overlay)))
					(replace-match (concat (match-string 1) (upcase (match-string 2))) t))))))
;; Remove filler words at the start and upcase the next word:1 ends here

;; [[file:../Sacha.org::#split-up-oops-better][Split up oops better:1]]
;;;###autoload
(defun my-split-oops ()
	"Look for oops and make it easier to split."
	(interactive)
	(let ((scan-window 300))
		(while (re-search-forward "oops[,\.]?[ \n]+" nil t)
			(let ((start (min (line-beginning-position) (- (point) scan-window)))
						start-search
						found
						search-for)
				(if (bolp)
						(progn
							(backward-char)
							(setq start (min (line-beginning-position) (- (point) scan-window))))
					(insert "\n"))
				(save-excursion
					(setq start-search (point))
					;; look for 1..5 words back
					(goto-char
					 (or
						(cl-loop
						 for n downfrom 5 downto 1
						 do
						 (save-excursion
							 (dotimes (_ n) (forward-word))
							 (setq search-for (downcase (string-trim (buffer-substring start-search (point)))))
							 (goto-char start-search)
							 (when (re-search-backward (regexp-quote search-for) start t)
								 (goto-char (match-beginning 0))
								 (cl-return (point)))))
						(and (call-interactively 'isearch-backward) (point))))
					(insert "\n"))))))
;; Split up oops better:1 ends here

(provide 'my-transcribe)
;;; my-transcribe.el ends here
