;;; my-bbb.el ---  -*- lexical-binding: t -*-

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
;; - Write a function to help with crontab entries
;;   https://sachachua.com/dotemacs#coding-write-a-function-to-help-with-crontab-entries
;;
;;; Code:



;; [[file:../Sacha.org::#coding-write-a-function-to-help-with-crontab-entries][Write a function to help with crontab entries:1]]
;;;###autoload
(defun my-bbb-insert-translated-crontab-entries (time-zone start-time end-time)
  "Insert crontab entries.
Convert START-TIME and END-TIME from TIME-ZONE.
(Assume END-TIME is after START-TIME.)
Create a pair of crontab entries for the start (converted to local time)
and end."
  (interactive
   (progn
     (require 'tzc)
     (let* ((time-zone (completing-read "Timezone: " tzc-time-zones))
            (start-time
             (org-read-date t t nil "Start date and time: "))
            (end-time
             (org-read-date t t nil "End time: " start-time)))
       (list time-zone start-time end-time))))
  (when (stringp start-time)
    (setq start-time (org-read-date t t start-time)))
  (when (stringp end-time)
    (setq end-time (org-read-date t t end-time nil start-time)))
  (let ((tz-offset (format-time-string "%z" start-time time-zone))
         text)
     (setq start-time (date-to-time (concat (format-time-string "%Y-%m-%dT%H:%M:%S.000" start-time) tz-offset)))
     (setq end-time (date-to-time (concat (format-time-string "%Y-%m-%dT%H:%M:%S.000" end-time) tz-offset)))
     (setq text (concat
                 (format-time-string "%-M %-H %-d %-m * \n" start-time)
                 (format-time-string "%-M %-H %-d %-m * " end-time)))
     (when (called-interactively-p 'any)
       (insert text))
     text))

(ert-deftest my-bbb-insert-translated-crontab-entries ()
  (should (string= (my-bbb-insert-translated-crontab-entries
                    "Europe/Berlin"
                    "2025-11-12T07:00:00"
                    "2025-11-12T10:30:00")
                   "0 1 12 11 *
30 4 12 11 * ")))
;; Write a function to help with crontab entries:1 ends here

(provide 'my-bbb)
;;; my-bbb.el ends here
