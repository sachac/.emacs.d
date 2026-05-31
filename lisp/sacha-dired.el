;;; sacha-dired.el ---  -*- lexical-binding: t -*-

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
;; - Mark today's files or other recent files
;;   https://sachachua.com/dotemacs#navigation-dired-mark-today-s-files-or-other-recent-files
;;
;; - Open files externally
;;   https://sachachua.com/dotemacs#navigation
;;
;;; Code:



;; [[file:../Sacha.org::#navigation-dired-mark-today-s-files-or-other-recent-files][Mark today's files or other recent files:1]]
;;;###autoload
(defun sacha-dired-mark-recent (days)
  "Mark files last modified at most (abs(DAYS)-1) days ago.
This means files modified since midnight if DAYS=1.  Unmark if DAYS is
negative. If DAYS=0, mark files last modified within the last 60 minutes.

From https://mbork.pl/2026-05-18_Marking_today%e2%80%99s_files_in_Dired"
  (interactive "P" dired-mode)
  (let* ((n (prefix-numeric-value days))
         (absn (abs n))
         (msg (format "recent (last %s) file"
                      (if (zerop n)
                          "60 minutes"
                        (format "%s day%s"
                                absn
                                (if (= absn 1) "" "s")))))
         (dired-marker-char (if (minusp n) ?\s dired-marker-char))
         (cutoff (if (zerop n)
                     (time-add (current-time) -3600) ; now - 60 minutes
                   (let ((time (decode-time
                                (time-add (current-time)
                                          (* (1- absn)
                                             60 60 24 -1)))))
                     (setf (decoded-time-hour time) 0
                           (decoded-time-minute time) 0
                           (decoded-time-second time) 0)
                     (encode-time time)))))
    (dired-mark-if
     (and (time-less-p
           cutoff
           (file-attribute-modification-time
            (file-attributes (dired-get-filename t t))))
          (not (looking-at-p dired-re-dot)))
     msg)))
;; Mark today's files or other recent files:1 ends here

;; [[file:../Sacha.org::*Open files externally][Open files externally:1]]
;;;###autoload
(defun dired-open-externally (&optional arg)
  "Open marked or current file in operating system's default application.

From oantolin's config."
  (interactive "P")
  (dired-map-over-marks
   (embark-open-externally (dired-get-filename))
   arg))
;; Open files externally:1 ends here

(provide 'sacha-dired)
;;; sacha-dired.el ends here
