;;; my-quantified.el ---  -*- lexical-binding: t -*-

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



;; [[file:../Sacha.org::#self-tracking-statistics-and-other-data-transformations-quantified-awesome-child-time][Child time!:1]]
;;;###autoload
(defun my-childcare ()
	(interactive)
	(unwind-protect
			(when (org-clocking-p)
				(org-clock-out)))
	(quantified-track "Childcare"))
;; Child time!:1 ends here

;; [[file:../Sacha.org::#quantified-tablist][Make a tablist of my time entries:1]]
(define-derived-mode my-quantified-list-mode tablist-mode "Time"
	"Major mode for time entries"
	(setq tabulated-list-format [("id" 5)
															 ("timestamp" 25)
															 ("duration" 5)
															 ("full_name" 60)
															 ("note" 20)])
	(tabulated-list-init-header)
	(tabulated-list-print t))

;;;###autoload
(defun my-quantified-list (start end filter)
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
	(my-quantified-list-mode))
;;;###autoload
(defun my-quantified-list-sum-marked-duration ()
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
;; (my-quantified-list "2024-09-30" nil "E1")
;; Make a tablist of my time entries:1 ends here

;; [[file:../Sacha.org::#time-tracking-previous-weekly-review][Time tracking, previous weekly review:1]]
(defvar my-org-quantified-categories
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
(defun my-org-summarize-time-use (&optional start end)
  (interactive (list (org-read-date) (org-read-date)))
  (let ((time-summary (quantified-summarize-time start end))
        (categories my-org-quantified-categories)
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
(defun my-quantified-compare (start1 end1 start2 end2 &optional categories label1 label2)
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

(provide 'my-quantified)
;;; my-quantified.el ends here
