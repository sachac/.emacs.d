;;; my-org-vtime.el ---  -*- lexical-binding: t -*-

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
;; - Linking to a specific time in a video
;;   https://sachachua.com/dotemacs#org-mode-links-linking-to-a-specific-time-in-a-video
;;
;; - Simplify inserting audio links
;;   https://sachachua.com/dotemacs#multimedia-subtitles-with-subed-simplify-inserting-audio-links
;;
;;; Code:



;; [[file:../Sacha.org::#org-mode-links-linking-to-a-specific-time-in-a-video][Linking to a specific time in a video:2]]
;;;###autoload
(defun my-org-video-time-follow (path _)
  ;; TODO: Look for the previous video and jump to the specified time
  (mpv-seek path)
  (mpv--enqueue '("set" "pause" "no") #'ignore))

;;;###autoload
(defun my-org-video-time-complete ()
  (interactive)
  (concat "vtime:" (format-seconds "%02h:%02m:%02s" (mpv-get-playback-position))))

;;;###autoload
(defun my-org-video-time-export (link desc format info)
	"Export PATH to FORMAT using the specified wrap parameter."
	(pcase format
		((or 'html '11ty 'md)
		 (when (string-match "\\([0-9]+:\\)?[0-9]+:[0-9]+" link)
			 (format "<span class=\"media-time\" data-start=\"%.3f\">%s</span>"
							 (save-match-data
								 (/ (compile-media-timestamp-to-msecs
										 (match-string 0 link)) 1000.0))
							 (match-string 0 link))))
		('org link)))
;; Linking to a specific time in a video:2 ends here

;; [[file:../Sacha.org::#org-mode-links-linking-to-a-specific-time-in-a-video][Linking to a specific time in a video:3]]
(defun my-org-vtime-item-p ()
  (save-excursion
    (org-list-at-regexp-after-bullet-p
     "\\[\\[\\(vtime:\\(?:[0-9]+:\\)?[0-9]+:[0-9]+\\)\\]\\]")))

;;;###autoload
(defun my-org-vtime-insert-item-advice (fn &rest args)
  (let ((itemp (org-in-item-p))
        (pos (point)))
    (unless (or (not itemp)
		            (save-excursion
		              (goto-char itemp)
		              (org-invisible-p)))
      (if (my-org-vtime-item-p)
          ;; Insert another vtime link
          (my-org-vtime-item)
        (apply fn args)))))

(defun my-org-vtime-item ()
  (let ((itemp (org-in-item-p)) (pos (point)))
    (cond
     ;; In a timer list, insert with `org-list-insert-item',
     ;; then fix the list.
     ((and itemp (goto-char itemp) (my-org-vtime-item-p))
      (let* ((struct (org-list-struct))
	           (prevs (org-list-prevs-alist struct))
	           (s (concat (org-link-make-string (my-org-video-time-complete)) " - ")))
	      (setq struct (org-list-insert-item pos struct prevs nil s))
	      (org-list-write-struct struct (org-list-parents-alist struct))
	      (looking-at org-list-full-item-re)
	      (goto-char (match-end 0))
        (line-end-position)))
     ;; In a list of another type, don't break anything: throw an error.
     (itemp (goto-char pos) (error "This is not a vtime list"))
     ;; Else, start a new list.
     (t
      (forward-line 0)
      (org-indent-line)
      (insert  "- "
               (concat (org-link-make-string (my-org-video-time-complete)) " - "))))))
;; Linking to a specific time in a video:3 ends here

;; [[file:../Sacha.org::#multimedia-subtitles-with-subed-simplify-inserting-audio-links][Simplify inserting audio links:3]]
;;;###autoload
(defun my-org-vtime-link (o &optional keep-hours)
  (concat "vtime:"
          (if (or keep-hours (>= (elt o 1) (* 60 60 1000)))
              (substring (car o) 0 8)
            (substring (car o) 3 8))))
;; Simplify inserting audio links:3 ends here

(provide 'my-org-vtime)
;;; my-org-vtime.el ends here
