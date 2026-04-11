;;; sacha-quantified.el ---  -*- lexical-binding: t -*-

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
;; - Weekly review
;;   https://sachachua.com/dotemacs#weekly-review
;;
;; - Monthly reviews
;;   https://sachachua.com/dotemacs#monthly-reviews
;;
;; - Child time!
;;   https://sachachua.com/dotemacs#self-tracking-statistics-and-other-data-transformations-quantified-awesome-child-time
;;
;; - Make a tablist of my time entries
;;   https://sachachua.com/dotemacs#quantified-tablist
;;
;; - Time tracking, previous weekly review
;;   https://sachachua.com/dotemacs#time-tracking-previous-weekly-review
;;
;; - Compare time use
;;   https://sachachua.com/dotemacs#compare-time-use
;;
;;; Code:



;; [[file:../Sacha.org::#weekly-review][Weekly review:2]]
(defvar sacha-weekly-review-line-regexp
  "^  \\([^:]+\\): +\\(Sched[^:]+: +\\)?TODO \\(.*?\\)\\(?:[      ]+\\(:[[:alnum:]_@#%:]+:\\)\\)?[        ]*$"
  "Regular expression matching lines to include.")
(defvar sacha-weekly-done-line-regexp
  "^  \\([^:]+\\): +.*?\\(?:Clocked\\|Closed\\):.*?\\(TODO\\|DONE\\) \\(.*?\\)\\(?:[       ]+\\(:[[:alnum:]_@#%:]+:\\)\\)?[        ]*$"
  "Regular expression matching lines to include as completed tasks.")

;;;###autoload
(defun sacha-quantified-sum (start end cat)
	"Return the number of hours from START to END in CAT."
	(quantified-parse-json
   (quantified-request
    (concat "records.json?start=" (or start "") "&end=" (or end "")
						"&order=newest&display_type=time&split=keep&category=" (url-hexify-string cat))
    (list (cons 'auth_token (quantified-token))) "GET")))

;;;###autoload
(defun sacha-quantified-average-weekly (start end category &optional insert)
  "Calculate average hours per week from START to END for CATEGORY."
  (interactive (list (org-read-date nil nil nil "Start: ")
                     (org-read-date nil nil nil "End: ")
                     (sacha-quantified-read-category)
                     current-prefix-arg))
  (let ((hours
         (/ (* 7.0 (sacha-quantified-sum start end category))
            (days-between end start))))
    (when (called-interactively-p 'any)
      (if insert
          (insert "%.1f hours" hours)
        (message "%.1f hours" hours)))
    hours))

(defvar sacha-quantified-categories nil)
;;;###autoload
(defun sacha-quantified-read-category ()
	(setq sacha-quantified-categories
				(or sacha-quantified-categories
						(quantified-parse-json
						 (quantified-request "/record_categories.json?all=1"
																 (list (cons 'auth_token (quantified-token)))
																 "GET"))))
	(completing-read
	 "Category: "
	 (mapcar (lambda (o)
						 (cons
							(alist-get 'full_name o)
							o))
					 sacha-quantified-categories)))

;;;###autoload
(defun sacha-quantified-sum (start end cat)
	"Return the number of hours from START to END in CAT."
	(interactive (list (org-read-date nil nil nil "Start: ")
										 (org-read-date nil nil nil "End: ")
										 (sacha-quantified-read-category)))
	(let* ((records
					(quantified-parse-json
					 (quantified-request
						(concat "records.json?start=" (or start "") "&end=" (or end "")
										"&order=newest&display_type=time&filter_string=" (url-hexify-string cat))
						(list (cons 'auth_token (quantified-token))) "GET")))
				 (duration (apply '+ (delq nil (mapcar (lambda (o) (alist-get 'duration o 0)) records))))
				 (hours (/ duration 3600.0)))
    (when (called-interactively-p 'any)
		  (message "%s: %.1f hour(s) in %d entries" cat hours (length records)))
    hours))

;;;###autoload
(defun sacha-quantified-get-hours (category time-summary)
  "Return the number of hours based on the time summary."
  (if (stringp category)
      (if (assoc category time-summary) (/ (cdr (assoc category time-summary)) 3600.0) 0)
    (apply '+ (mapcar (lambda (x) (sacha-quantified-get-hours x time-summary)) category))))
;; Weekly review:2 ends here

;; [[file:../Sacha.org::#monthly-reviews][Monthly reviews:3]]
(defvar sacha-quantified-summary-categories '("Business" "Discretionary - Play" "Unpaid work" "A+" "Discretionary - Family" "Sleep" "Discretionary - Productive" "Personal"))
;;;###autoload
(defun sacha-quantified-summarize-time-table-month (month)
	"Insert or return the table summarizing the month's time, compared with the previous month."
	(interactive (list (org-read-date nil t)))
	(let* ((date (decode-time (if (stringp month) (date-to-time month) month)))
				 (month (elt date 4))
         (year (elt date 5))
				 start-date
				 end-date
				 previous-date
				 results)
		(calendar-increment-month month year -1)
		(setq start-date (format "%4d-%02d-01 0:00" year month)
          end-date (format "%4d-%02d-01 0:00" (elt date 5) (elt date 4)))
		(calendar-increment-month month year -1)
		(setq previous-date (format "%4d-%02d-01 0:00" year month))
		(setq results (orgtbl-to-orgtbl (sacha-quantified-compare previous-date start-date start-date end-date sacha-quantified-summary-categories "Previous month %" "This month %")
																		nil))
		(when (called-interactively-p 'any)
			(insert results))
		results))
;; Monthly reviews:3 ends here

;; [[file:../Sacha.org::#self-tracking-statistics-and-other-data-transformations-quantified-awesome-child-time][Child time!:1]]
;;;###autoload
(defun sacha-childcare ()
	(interactive)
	(unwind-protect
			(when (org-clocking-p)
				(org-clock-out)))
	(quantified-track "Childcare"))
;; Child time!:1 ends here

;; [[file:../Sacha.org::#quantified-tablist][Make a tablist of my time entries:1]]
(define-derived-mode sacha-quantified-list-mode tablist-mode "Time"
	"Major mode for time entries"
	(setq tabulated-list-format [("id" 5)
															 ("timestamp" 25)
															 ("duration" 5)
															 ("full_name" 60)
															 ("note" 20)])
	(tabulated-list-init-header)
	(tabulated-list-print t))

;;;###autoload
(defun sacha-quantified-list (start end filter)
	(interactive (list (org-read-date nil nil nil "Start: ") (org-read-date nil nil nil "End: ")
										 (read-string "Filter: ")))
	(switch-to-buffer (get-buffer-create "*quantified*"))
	(setq filter (and filter
										(not (string= filter ""))
										(split-string filter " ")))
	(let ((json-array-type 'list)
				(json-object-type 'alist))
		(setq tabulated-list-entries
					(seq-keep
					 (lambda (o)
						 (let-alist o
							 (when (or (not filter)
												 (not (seq-find
															 (lambda (term)
																 (not
																	(or
																	 (string-match term .full_name)
																	 (string-match term (or .data.note "")))))
															 filter)))
								 (list
									.id
									(vector
									 (number-to-string .id)
									 (format-time-string "%a %b %d %l:%M%p" (parse-iso8601-time-string .timestamp))
									 (propertize (if .duration (format-seconds "%h:%.2m" .duration) "")
															 'duration .duration)
									 .full_name
									 (or .data.note ""))))))
					 (quantified-parse-json
						(quantified-request
						 (format
							"/records.json?start=%s&end=%s&auth_token=%s"
							(or start "")
							(or end "")
							(quantified-token))
						 nil "GET")))))
	(sacha-quantified-list-mode))
;;;###autoload
(defun sacha-quantified-list-sum-marked-duration ()
	(interactive)
	(let ((seconds (apply '+
																	(mapcar
																	 (lambda (o)
																		 (get-text-property 0 'duration
																												(aref (cdr o) 2)))
																	 (tablist-get-marked-items)))))
		(message "%s (%.1f)"
						 (format-seconds "%d:%z%.2h:%.2m" seconds)
						 (/ seconds 3600.0))))
;; (sacha-quantified-list "2024-09-30" nil "E1")
;; Make a tablist of my time entries:1 ends here

;; [[file:../Sacha.org::#time-tracking-previous-weekly-review][Time tracking, previous weekly review:1]]
(defvar sacha-org-quantified-categories
  '(("Business"
     ("Earn" . "Business - Earn")
     ("E1" . "Business - Earn - Consulting - E1")
     ("Connect" . "Business - Connect")
     ("Build" . "Business - Build"))
    ("Discretionary"
     ("Social" . "Discretionary - Social")
     ("Productive" . "Discretionary - Productive")
     ("Sewing" . "Discretionary - Productive - Sewing")
     ("Writing" . "Discretionary - Productive - Writing")
     ("Emacs" . "Discretionary - Productive - Emacs")
     ("Play" . "Discretionary - Play"))
    ("Personal" ;("Biking" . "Personal - Bike")
     ("Routines" . "Personal - Routines"))
    ("Sleep" nil)
    ("Unpaid work"
     ("Commuting" . "Unpaid work - Subway")
     ("Cook" . "Unpaid work - Cook")
     ("Tidy" . "Unpaid work - Tidy up")))
  "Categories for time summary.")

;;;###autoload
(defun sacha-org-summarize-time-use (&optional start end)
  (interactive (list (org-read-date) (org-read-date)))
  (let ((time-summary (quantified-summarize-time start end))
        (categories sacha-org-quantified-categories)
        result)
    (setq result
          (mapconcat
           (lambda (a)
             (if (assoc (car a) time-summary)
                 (concat
                  (format "- %s: %.1f hours" (car a) (/ (cdr (assoc (car a) time-summary)) 3600.0))
                  (if (cdr a)
                      (let ((detail
                             (delq nil
                                   (mapcar (lambda (b)
                                             (if (assoc (cdr b) time-summary)
                                                 (format "%s: %.1f"
                                                         (car b)
                                                         (/ (cdr (assoc (cdr b) time-summary)) 3600.0))
                                               nil))
                                           (cdr a)))))
                        (if detail
                            (concat " (" (mapconcat 'identity detail ", ") ")")
                          ""))
                    "")
                  (if (string-equal (car a) "Sleep")
                      (format " - average of %.1f hours per day" (/ (cdr (assoc (car a) time-summary)) 3600.0 7.0))
                    "")
                  "\n")))
           categories ""))
    (if (called-interactively-p 'any)
        (insert result)
      result)))
;; Time tracking, previous weekly review:1 ends here

;; [[file:../Sacha.org::#compare-time-use][Compare time use:1]]
;;;###autoload
(defun sacha-quantified-compare (start1 end1 start2 end2 &optional categories label1 label2)
  "Return a table comparing the times for START1 - END1 and START2 - END2."
	(interactive (list
								(org-read-date "Start of period 1")
								(org-read-date "End of period 1")
								(org-read-date "Start of period 2")
								(org-read-date "End of period 2")
								'("Business" "Discretionary - Play" "Unpaid work"
									"A+" "Discretionary - Family" "Discretionary - Social" "Sleep"
									"Discretionary - Productive" "Personal")))
  (let* ((start2 (org-read-date nil nil (or start2 "-sat")))
         (end2 (org-read-date nil nil (or end2 "+1")))
         (start1 (org-read-date nil nil (or start1 "-4sat")))
         (end1 (org-read-date nil nil (or end1 "-sat")))
         (time2 (quantified-summarize-time start2 end2))
         (time1 (quantified-summarize-time start1 end1))
         (label1 (or label1 "Period 1 %"))
         (label2 (or label2 "Period 2 %"))
         (total2 (* 0.01 (- (org-time-string-to-seconds end2) (org-time-string-to-seconds start2))))
         (total1 (* 0.01 (- (org-time-string-to-seconds end1) (org-time-string-to-seconds start1))))
         (keys (or categories (-union (mapcar 'car time1) (mapcar 'car time2))))
				 result)
    ;; Build a list comparing the two
		(setq result
					(append
					 `(("Category" ,label1 ,label2 "Diff %" "h/wk" "Diff h/wk") hline)
					 (sort
						(mapcar (lambda (key)
											(list
											 key
											 (format "%.1f" (/ (or (assoc-default key time1) 0) total1))
											 (format "%.1f" (/ (or (assoc-default key time2) 0) total2))
											 (format "%.1f" (- (/ (or (assoc-default key time2) 0) total2)
																				 (/ (or (assoc-default key time1) 0) total1)))
											 (format "%.1f" (* (/ (or (assoc-default key time2) 0) total1) 1.68))
											 (format "%.1f"
															 (* (- (/ (or (assoc-default key time2) 0) total2)
																		 (/ (or (assoc-default key time1) 0) total1)) 1.68))
											 )) keys)
						(lambda (a b)
							(<
							 (string-to-number (car (last b)))
							 (string-to-number (car (last a))))))))
		(when (called-interactively-p 'any)
			(insert (orgtbl-to-orgtbl result nil)))
		result))
;; Compare time use:1 ends here

(provide 'sacha-quantified)
;;; sacha-quantified.el ends here
