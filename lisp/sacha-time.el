;;; sacha-time.el ---  -*- lexical-binding: t -*-

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
;; - Timestamps
;;   https://sachachua.com/dotemacs#multimedia-timestamps
;;
;; - Using the calendar-date-echo-text variable to help plot a heatmap on a year-long calendar in Emacs
;;   https://sachachua.com/dotemacs#using-the-calendar-date-echo-text-variable-to-help-plot-a-heatmap-on-a-year-long-calendar-in-emacs
;;
;; - Workrave
;;   https://sachachua.com/dotemacs#workrave
;;
;;; Code:



;; [[file:../Sacha.org::#multimedia-timestamps][Timestamps:1]]
;;;###autoload
(defun sacha-filename-timestamp (file)
	(setq file (replace-regexp-in-string "^screen-" "" (file-name-base file)))
	(cond
	 ((string-match
		 "\\([0-9][0-9][0-9][0-9]\\)_?\\([0-9][0-9]\\)_?\\([0-9][0-9]\\)_\\([0-9][0-9]\\)_?\\([0-9][0-9]\\)_?\\([0-9][0-9]\\)"
		 file)
		(date-to-time (format "%s-%s-%s %s:%s:%s"
													(match-string 1 file)
													(match-string 2 file)
													(match-string 3 file)
													(match-string 4 file)
													(match-string 5 file)
													(match-string 6 file))))
	 (t
		(time-add (date-to-time (format "%s %s" (substring file 0 10) (substring file 11 19)))
							(float-time (/ (string-to-number (substring file 20 23)) 1000.0))))))

(cl-assert
 (string= (format-time-string "test-%F-%T-%3N" (sacha-filename-timestamp "screen-2024-09-20-13:18:08-024.png"))
					"test-2024-09-20-13:18:08-024")
 (string= (format-time-string "test-%F-%T-%3N" (sacha-filename-timestamp "screen-2024-09-20-13_1808-024.png"))
					"test-2024-09-20-13:18:08-024"))
;; Timestamps:1 ends here

;; [[file:../Sacha.org::#using-the-calendar-date-echo-text-variable-to-help-plot-a-heatmap-on-a-year-long-calendar-in-emacs][Using the calendar-date-echo-text variable to help plot a heatmap on a year-long calendar in Emacs:2]]
(defvar sacha-calendar-count-scaled)
;;;###autoload
(defun sacha-calendar-heat-map-using-echo-text (&rest _)
  (when sacha-calendar-count-scaled
		(save-excursion
			(goto-char (point-min))
			(while (not (eobp))
				(let* ((help (get-text-property (point) 'help-echo))
							 (next-change
								(or (next-single-property-change (point) 'help-echo)
										(point-max)))
							 (inhibit-read-only t)
							 (count-scaled (and help
																	(assoc-default
																	 help
																	 sacha-calendar-count-scaled))))
					(when (and help
										 (string-match "[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]" help)
										 count-scaled)
						(put-text-property
						 (point) (+ 2 (point))
						 'face (intern (format "calendar-scale-%d" count-scaled))))
					(goto-char next-change))))))
;; Using the calendar-date-echo-text variable to help plot a heatmap on a year-long calendar in Emacs:2 ends here

;; [[file:../Sacha.org::#using-the-calendar-date-echo-text-variable-to-help-plot-a-heatmap-on-a-year-long-calendar-in-emacs][Using the calendar-date-echo-text variable to help plot a heatmap on a year-long calendar in Emacs:4]]
(defface calendar-scale-1  '((((background light)) :foreground "black" :background "#eceff1")
                             (((background dark))  :foreground "white" :background "#263238")) "")
(defface calendar-scale-2  '((((background light)) :foreground "black" :background "#cfd8dc")
                             (((background dark))  :foreground "white" :background "#37474f")) "")
(defface calendar-scale-3  '((((background light)) :foreground "black" :background "#b0bec5")
                             (((background dark))  :foreground "white" :background "#455a64")) "")
(defface calendar-scale-4  '((((background light)) :foreground "black" :background "#90a4ae")
                             (((background dark))  :foreground "white" :background "#546e7a")) "")
(defface calendar-scale-5  '((((background light)) :foreground "black" :background "#78909c")
                             (((background dark))  :foreground "white" :background "#607d8b")) "")
(defface calendar-scale-6  '((((background light)) :foreground "white" :background "#607d8b")
                             (((background dark))  :foreground "black" :background "#78909c")) "")
(defface calendar-scale-7  '((((background light)) :foreground "white" :background "#546e7a")
                             (((background dark))  :foreground "black" :background "#90a4ae")) "")
(defface calendar-scale-8  '((((background light)) :foreground "white" :background "#455a64")
                             (((background dark))  :foreground "black" :background "#b0bec5")) "")
(defface calendar-scale-9  '((((background light)) :foreground "white" :background "#37474f")
                             (((background dark))  :foreground "black" :background "#cfd8dc")) "")
(defun sacha-count-calendar-entries (grouped-entries)
  (mapcar (lambda (entry) (cons (car entry) (length (cdr entry)))) grouped-entries))

(defface calendar-scale-10 '((((background light)) :foreground "white" :background "#263238")
                             (((background dark))  :foreground "black" :background "#eceff1")) "")

(defun sacha-scale-calendar-entries (grouped-entries &optional scale-max)
  (let* ((count (sacha-count-calendar-entries grouped-entries))
         (count-max (apply #'max (mapcar (lambda (o) (if (car o) (cdr o) 0)) count))))
    (mapcar (lambda (entry)
              (cons (car entry)
                    (/ (* 1.0 (or scale-max 1.0) (cdr entry)) count-max)))
            count)))

(defun sacha-scale-calendar-entries-logarithmically (grouped-entries &optional scale-max)
  (let* ((count (sacha-count-calendar-entries grouped-entries))
         (count-max (apply #'max (mapcar (lambda (o) (if (car o) (cdr o) 0)) count))))
    (mapcar (lambda (entry)
              (cons (car entry)
                    (/ (* 1.0 (or scale-max 1.0) (log (cdr entry))) (log count-max))))
            count)))

(defvar sacha-calendar-count-scaled nil "Values to display.")
;; Using the calendar-date-echo-text variable to help plot a heatmap on a year-long calendar in Emacs:4 ends here

;; [[file:../Sacha.org::#using-the-calendar-date-echo-text-variable-to-help-plot-a-heatmap-on-a-year-long-calendar-in-emacs][Using the calendar-date-echo-text variable to help plot a heatmap on a year-long calendar in Emacs:5]]
(defvar sacha-calendar-count-scaled)
;;;###autoload
(defun sacha-calendar-visualize (values)
  (setq sacha-calendar-count-scaled values)
	(let* ((date (calendar-current-date))
				 (month (calendar-extract-month date))
				 (year (calendar-extract-year date)))
		(year-calendar month (1- year))))
;; Using the calendar-date-echo-text variable to help plot a heatmap on a year-long calendar in Emacs:5 ends here

;; [[file:../Sacha.org::#using-the-calendar-date-echo-text-variable-to-help-plot-a-heatmap-on-a-year-long-calendar-in-emacs][Using the calendar-date-echo-text variable to help plot a heatmap on a year-long calendar in Emacs:6]]
;;;###autoload
(defun sacha-calendar-visualize-journal-entries ()
  (interactive)
  (sacha-calendar-visualize
   (mapcar
    (lambda (o)
      (cons
       (car o)
       (ceiling (+ 1 (* 7.0 (cdr o))))))
    (sacha-scale-calendar-entries
     (seq-group-by #'sacha-journal-date
                   (cdr (pcsv-parse-file "~/Downloads/entries.csv")))))))

;;;###autoload
(defun sacha-calendar-visualize-sketches ()
  (interactive)
  (let ((sacha-calendar-sketches
         (assoc-delete-all
          nil
          (seq-group-by
           (lambda (o)
             (when (string-match "^\\([0-9][0-9][0-9][0-9]\\)[-_]?\\([0-9][0-9]\\)[-_]?\\([0-9][0-9]\\)" o)
               (format "%s-%s-%s"
                       (match-string 1 o)
                       (match-string 2 o)
                       (match-string 3 o))))
           (append
            (directory-files "~/sync/sketches" nil "\\.\\(png\\|jpg\\)\\'")
            (directory-files "~/sync/private-sketches" nil "\\.\\(png\\|jpg\\)\\'"))))))
    (sacha-calendar-visualize
     (mapcar
      (lambda (o)
        (cons (car o)
              ;; many days have just 1 sketch, so I set the low end of the scale
              ;; to make them visible, and use a logarithmic scale for the rest
              (ceiling (+ 3 (* 7.0 (cdr o))))))
      (sacha-scale-calendar-entries-logarithmically sacha-calendar-sketches)))))

;;;###autoload
(defun sacha-calendar-visualize-tantrums ()
  (interactive)
  (sacha-calendar-visualize
   (mapcar
    (lambda (o)
      (cons
       (car o)
       (ceiling (* 10.0 (cdr o)))))
    (sacha-scale-calendar-entries
     (seq-group-by #'sacha-journal-date
                   (seq-filter (lambda (o) (string-match "tantrum\\|grump\\|angry\\|meltdown"
                                                           (sacha-journal-note o)))
                               (cdr (pcsv-parse-file "~/Downloads/entries.csv"))))))))

;; Using the calendar-date-echo-text variable to help plot a heatmap on a year-long calendar in Emacs:6 ends here

;; [[file:../Sacha.org::#using-the-calendar-date-echo-text-variable-to-help-plot-a-heatmap-on-a-year-long-calendar-in-emacs][Using the calendar-date-echo-text variable to help plot a heatmap on a year-long calendar in Emacs:8]]
;; https://stackoverflow.com/questions/9547912/emacs-calendar-show-more-than-3-months
(defmacro lawlist-calendar-for-loop (var from init to final do &rest body)
  "Execute a for loop.
Evaluate BODY with VAR bound to successive integers from INIT to FINAL,
inclusive.  The standard macro `dotimes' is preferable in most cases."
  `(let ((,var (1- ,init)))
    (while (>= ,final (setq ,var (1+ ,var)))
      ,@body)))

;;;###autoload
(defun year-calendar (&optional month year)
  "Generate a one (1) year calendar that can be scrolled by month in each direction.
This is a modification of:  http://homepage3.nifty.com/oatu/emacs/calendar.html
See also:  http://ivan.kanis.fr/caly.el"
	(interactive)
  (require 'calendar)
  (let* ((current-year (number-to-string (nth 5 (decode-time (current-time)))))
         (month (if month month
           (string-to-number
             (read-string "Please enter a month number (e.g., 1):  " nil nil "1"))))
         (year (if year year
           (string-to-number
             (read-string "Please enter a year (e.g., 2014):  "
               nil nil current-year)))))
    (switch-to-buffer (get-buffer-create calendar-buffer))
    (when (not (eq major-mode 'calendar-mode))
      (calendar-mode))
    (setq displayed-month month)
    (setq displayed-year year)
    (setq buffer-read-only nil)
    (erase-buffer)
    ;; horizontal rows
    (lawlist-calendar-for-loop j from 0 to 3 do
      ;; vertical columns
      (lawlist-calendar-for-loop i from 0 to 2 do
        (calendar-generate-month
          ;; month
          (cond
            ((> (+ (* j 3) i month) 12)
              (- (+ (* j 3) i month) 12))
            (t
              (+ (* j 3) i month)))
          ;; year
          (cond
            ((> (+ (* j 3) i month) 12)
             (+ year 1))
            (t
              year))
          ;; indentation / spacing between months
          (+ 5 (* 25 i))))
      (goto-char (point-max))
      (insert (make-string (- 10 (count-lines (point-min) (point-max))) ?\n))
      (widen)
      (goto-char (point-max))
      (narrow-to-region (point-max) (point-max)))
    (widen)
    (goto-char (point-min))
    (setq buffer-read-only t)))

;;;###autoload
(defun lawlist-scroll-year-calendar-forward (&optional arg event)
  "Scroll the yearly calendar by month in a forward direction."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     last-nonmenu-event))
  (unless arg (setq arg 1))
  (save-selected-window
    (if (setq event (event-start event)) (select-window (posn-window event)))
    (unless (zerop arg)
      (let ((month displayed-month)
            (year displayed-year))
        (calendar-increment-month month year arg)
        (year-calendar month year)))
    (goto-char (point-min))
    (run-hooks 'calendar-move-hook)))

;;;###autoload
(defun lawlist-scroll-year-calendar-backward (&optional arg event)
  "Scroll the yearly calendar by month in a backward direction."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     last-nonmenu-event))
  (lawlist-scroll-year-calendar-forward (- (or arg 1)) event))
;; Using the calendar-date-echo-text variable to help plot a heatmap on a year-long calendar in Emacs:8 ends here

;; [[file:../Sacha.org::#using-the-calendar-date-echo-text-variable-to-help-plot-a-heatmap-on-a-year-long-calendar-in-emacs][Using the calendar-date-echo-text variable to help plot a heatmap on a year-long calendar in Emacs:9]]
;;;###autoload
(defun sacha-scroll-year-calendar-forward-year (&optional arg event)
  "Scroll the yearly calendar by year in a forward direction."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     last-nonmenu-event))
  (unless arg (setq arg 1))
  (save-selected-window
    (if (setq event (event-start event)) (select-window (posn-window event)))
    (unless (zerop arg)
      (setq displayed-year (+ (or arg 1) displayed-year))
      (year-calendar displayed-month displayed-year))
    (goto-char (point-min))
    (run-hooks 'calendar-move-hook)))

;;;###autoload
(defun sacha-scroll-year-calendar-backward-year (&optional arg event)
  "Scroll the yearly calendar by month in a backward direction."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     last-nonmenu-event))
  (sacha-scroll-year-calendar-forward-year (- (or arg 1)) event))
(eval-after-load "calendar" '(progn
  (define-key calendar-mode-map "{" 'sacha-scroll-year-calendar-backward-year)
  (define-key calendar-mode-map "}" 'sacha-scroll-year-calendar-forward-year)))
;; Using the calendar-date-echo-text variable to help plot a heatmap on a year-long calendar in Emacs:9 ends here

;; [[file:../Sacha.org::#workrave][Workrave:1]]
(defvar sacha-workrave-file (expand-file-name ".\\Workrave\\historystats" (getenv "AppData")))

;;;###autoload
(defun sacha-workrave-transform-statistics (&optional file)
  (interactive (list sacha-workrave-file))
  (with-current-buffer (find-file-noselect file)
    ;; D day month-1 year hour min day month-1 year hour min
    (let ((result "Date\tStart\tEnd\tClicks\tKeystrokes\n"))
      (goto-char (point-min))
      (while (re-search-forward "^D \\(.*\\)" nil t)
        (let ((dates (split-string (match-string 1))))
          (if (re-search-forward "^m \\(.*\\)" nil t)
              (let ((info (split-string (match-string 1))))
                (setq result
                      (concat result
                              (format "%d-%d-%s\t%s:%02d\t%s:%02d\t%s\t%s\n"
                                      (+ 1900 (string-to-number (elt dates 2))) ; year
                                      (1+ (string-to-number (elt dates 1))) ; month
                                      (elt dates 0) ; day
                                      (elt dates 3) ; start hour
                                      (string-to-number (elt dates 4)) ; start min
                                      (elt dates 8) ; end hour
                                      (string-to-number (elt dates 9)) ; end min
                                      (elt info 5) ; clicks
                                      (elt info 6) ; keystrokes
                                      )))))))
      (if (interactive-p)
          (kill-new result)
        result))))
;; Workrave:1 ends here

(provide 'sacha-time)
;;; sacha-time.el ends here
