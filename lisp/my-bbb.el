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
