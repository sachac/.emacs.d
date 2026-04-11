;;; sacha-journal.el ---  -*- lexical-binding: t -*-

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



;; [[file:../Sacha.org::#org-mode-keyboard-shortcuts-other-speed-commands][Other speed commands:3]]
;;;###autoload
(defun sacha-org-mark-done-and-add-to-journal (&optional note category)
  (interactive (list (if current-prefix-arg
                         (read-string (format "Note (%s): " (org-get-heading t t t t)))
                       (org-get-heading t t t t))
                     (or (org-entry-get (point) "JOURNAL_CAT") (sacha-journal-read-category (sacha-journal-guess-category)))))
  (sacha-org-with-current-task
   (org-todo "DONE")
   (org-entry-put (point) "JOURNAL_CAT" category)
   (let* ((title (or note (org-get-heading t t t t)))
          (zid (org-entry-get (point) "ZIDSTRING"))
          (other (if current-prefix-arg (substring-no-properties (sacha-org-subtree-text))))
          (date (unless zid
                  (format-time-string "%Y-%m-%d %H:%M"
                                      (let ((base-date (org-read-date nil t (org-entry-get (point) "CREATED"))))
                                        (if (string-match "Yesterday " title)
                                            (progn
                                              (setq title (replace-match "" nil nil title))
                                              (org-read-date nil t "--1" nil (org-time-string-to-time (org-entry-get (point) "CREATED"))))
                                          base-date))))))
     (if zid
         (sacha-journal-update (list :ZIDString zid :Note title :Category category :Other other))
       (org-entry-put (point) "ZIDSTRING"
                      (plist-get
                       (sacha-journal-post title
                                        :Category category
                                        :Other other
                                        :Date date)
                       :ZIDString)))
     (org-back-to-heading)
     (sacha-copy-observation))))

;; Other speed commands:3 ends here

;; [[file:../Sacha.org::#journal][Journal:1]]
(defvar sacha-journal-category-map
  '(("Gross" . "Gross motor")
    ("Fine" . "Fine motor")
    ("8 - Kaizen" . "Kaizen")
    ("9 - Us" . "Us")
    ("Self-care" . "Self-care and independence"))
  "Alist of string replacements for journal categories.")
(defvar sacha-journal-categories
  '("Kaizen" "Us" "Field trip" "Gross motor" "Fine motor"
    "Sensory" "Language" "Music" "Art"
    "Self-care and independence" "Eating" "Sleep" "Emotion"
    "Household" "Social" "Pretend" "Cognition" "World" "Other" "Oops" "Thoughts" "Consulting" "Track" "Uncategorized")
  "List of categories to display.
      Unknown categories will be added to the end.")

;;;###autoload
(defun sacha-journal-date (o) (elt o 3))
;;;###autoload
(defun sacha-journal-note (o) (car o))
;;;###autoload
(defun sacha-journal-week-highlight (o) (elt o 4))
;;;###autoload
(defun sacha-journal-category (o) (elt o 1))
;;;###autoload
(defun sacha-journal-pictures (o) (when (string> (elt o 2) "") (split-string (elt o 2) ",")))
;;;###autoload
(defun sacha-journal-id (o) (elt o 7))
;;;###autoload
(defun sacha-journal-status (o) (elt o 8))
;;;###autoload
(defun sacha-journal-other (o) (elt o 9))
;;;###autoload
(defun sacha-journal-zidstring (o) (elt o 11))
;;;###autoload
(defun sacha-org-group-journal-entries (filtered &optional category-map categories)
  (setq category-map (or category-map sacha-journal-category-map))
  (setq categories (or categories sacha-journal-categories))
  (let* ((grouped (-group-by 'sacha-journal-category filtered))
         (mapped-list
          (mapcar
           (lambda (o)
             (cons (or (assoc-default (car o) category-map) (car o))
                   (cdr o)))
           grouped))
         (sorted-list
          (delq nil
                (append
                 (mapcar (lambda (cat)
                           (when (assoc-default cat mapped-list)
                             (cons cat (assoc-default cat mapped-list))))
                         categories)
                 (-remove (lambda (o) (member (car o) categories)) mapped-list)))))
    sorted-list))

;;;###autoload
(defun sacha-org-date-to-string (date &optional base-date)
  "Return the Org date specified by DATE.
      This is relative to BASE-DATE if specified."
  (org-read-date nil nil date nil (when base-date (org-read-date nil t base-date))))

(ert-deftest sacha-org-date-to-string ()
  (should (string= (sacha-org-date-to-string "++1" "2018-08-01") "2018-08-02")))

;;;###autoload
(defun sacha-org-filter-journal-csv (filename &optional from to highlight base-date)
  "Return a list of matching entries."
  (setq from (and from (substring (sacha-org-date-to-string from base-date) 0 10))
        to (and to (substring (sacha-org-date-to-string to base-date) 0 10)))
  (let* ((data (pcsv-parse-file filename))
         (filtered
          (-filter
           (lambda (o)
             (let ((date (sacha-journal-date o)))
               (and (or (null from) (not (string< date from)))
                    (or (null to) (string< date to))
                    (and (not (string= (sacha-journal-status o) "Deleted")))
                    (not (string-match "^!" (sacha-journal-note o)))
                    (string-equal
                     "true"
                     (cond
                      ((null highlight) "true")
                      ((string-equal highlight "week") (sacha-journal-week-highlight o))
                      (t "true"))))))
           data)))
    filtered))

;;;###autoload
(defun sacha-journal-read-category (&optional initial)
  (consult--read sacha-journal-categories :sort nil :prompt "Category: " :initial initial))

;;;###autoload
(defun sacha-journal-guess-category ()
	(when (derived-mode-p 'org-mode)
		(save-excursion
      (org-back-to-heading)
		  (org-end-of-meta-data)
		  (let ((text (buffer-substring-no-properties (point) (org-end-of-subtree))))
			  (if (string-match "#gardening" text)
					  "Household")))))

;;;###autoload
(defun sacha-journal-post (note &rest plist)
  (interactive (list (read-string "Note: ")
                     :Date (concat (org-read-date "Date: ") " 23:00")
                     :Category (sacha-journal-read-category (condition-case nil (sacha-journal-guess-category) (error nil)))
                     :Other (read-string "Other: ")))
  (setq plist (append `(:Note ,note) plist))
  (let ((url-request-method "POST")
        (url-request-extra-headers `(("Content-Type" . "application/json")
																		("Authorization" . ,(concat "Basic "
																																(base64-encode-string
																																 (concat sacha-journal-user ":" sacha-journal-password))))))
        (json-object-type 'plist)
        (url-request-data (encode-coding-string (json-encode-plist plist) 'utf-8))
        data)
    (with-current-buffer (url-retrieve-synchronously (concat sacha-journal-url "/api/entries"))
      (goto-char (point-min))
      (re-search-forward "^$")
      (setq data (json-read))
      (message "%s" (plist-get data :ZIDString))
      data)))

;;;###autoload
(defun sacha-journal-get-by-zidstring (zidstring)
  (sacha-journal-get (concat "api/entries/" zidstring)))

;;;###autoload
(defun sacha-journal-insert-ref (zidstring)
  (interactive (list (sacha-journal-completing-read)))
  (insert (org-link-make-string (concat "ref:" (sacha-journal-id-from-string zidstring)))))

;;;###autoload
(defun sacha-journal-edit (zidstring)
  (interactive (list (sacha-journal-completing-read)))
  (let* ((id (sacha-journal-id-from-string zidstring))
         (entry (and id (sacha-journal-get-by-zidstring id))))
    (if (null id)
        (sacha-journal-post zidstring
                         :Category (sacha-journal-read-category (plist-get entry :Category))
                         :Other (read-string "Other: " (plist-get entry :Other)))
      (plist-put entry :Note (read-string (format "Note (%s): " (plist-get entry :Note))))
      (plist-put entry :Category (sacha-journal-read-category (plist-get entry :Category)))
      (plist-put entry :Other (read-string "Other: " (plist-get entry :Other)))
      (sacha-journal-update entry))))

;;;###autoload
(defun sacha-journal-update (plist)
  "Update journal entry using PLIST."
  (let ((url-request-method "PUT")
        (url-request-data (json-encode-plist plist)))
    (sacha-json-request (concat sacha-journal-url "/api/entries/" (plist-get plist :ZIDString)))))
;; (sacha-journal-post "Hello, world")

;;;###autoload
(defun sacha-journal-get-entries (&optional from to search)
  "Return parsed CSV of entries limited by FROM, TO, and SEARCH."
  (with-current-buffer
      (url-retrieve-synchronously (format "%s/api/entries.csv?from=%s&to=%s&regex=1&q=%s"
                                          sacha-journal-url
                                          (or from "")
                                          (or to "")
                                          (or search "")))
		(set-buffer-multibyte t)
    (goto-char (point-min))
    (delete-region (point-min) (search-forward "\n\n"))
    (cdr (pcsv-parse-buffer))))

;;;###autoload
(defun sacha-journal-get (url)
	(let ((url-request-extra-headers
				 `(("Authorization" . ,(concat "Basic "
																			 (base64-encode-string
																				(concat sacha-journal-user ":" sacha-journal-password)))))))
		(sacha-json-request (concat sacha-journal-url "/" url))))
;;;###autoload
(defun sacha-journal-get-entry (zid) (sacha-journal-get (format "api/entries/zid/%s" zid)))
;; Journal:1 ends here

;; [[file:../Sacha.org::helm-journal][helm-journal]]
(defun sacha-json-request (url)
  (let ((json-object-type 'plist)
        (url-request-extra-headers (cons '("Content-Type" . "application/json") url-request-extra-headers)))
    (with-current-buffer (url-retrieve-synchronously url)
      (set-buffer-multibyte t)
      (goto-char (point-min))
      (re-search-forward "^$" nil t)
      (json-read))))

(defvar sacha-journal-search-cache nil "List of search results.")
;;;###autoload
(defun sacha-journal-search-query (query-str)
  (let* ((url-request-method "GET")
         (json-response (sacha-journal-get (format "api/entries?q=%s&limit=50&sort=date&regex=1"
                                                 query-str))))
    (setq sacha-journal-search-cache (mapcar (lambda (o)
              (cons
               (format "%s %s"
                       (plist-get o :ZIDString)
                       (plist-get o :Note))
               o))
            json-response))))

;;;###autoload
(defun sacha-journal-search-query-async (query-str next)
  (let* ((url-request-method "GET")
         (url-request-extra-headers (cons '("Content-Type" . "application/json") url-request-extra-headers)))
    (url-retrieve
     (format "%s/api/entries?q=%s&limit=50&sort=date&regex=1"
             sacha-journal-url
       query-str)
     (lambda (status)
       (goto-char (point-min))
       (re-search-forward "^$" nil t)
       (setq sacha-journal-search-cache
             (mapcar (lambda (o)
                       (cons
                        (format "%s %s"
                                (plist-get o :ZIDString)
                                (plist-get o :Note))
                        o))
                     (let ((json-object-type 'plist))
                       (json-read))))
       (funcall next 'flush)
       (if sacha-journal-search-cache (funcall next sacha-journal-search-cache))))))

;;;###autoload
(defun sacha-journal--async-search (next)
  (lambda (action)
    (cond
     ((eq action 'setup)                ;; Should figure out how to start
      (sacha-journal-search-query-async "" next))
     ((and (stringp action) (not (string= action "")))
      (sacha-journal-search-query-async action next))
     (t (funcall next action)))))

;;;###autoload
(defun sacha-journal-completing-read ()
  (interactive)
  (consult--read
   (thread-first (consult--async-sink)
     (consult--async-refresh-immediate)
     (sacha-journal--async-search)
     (consult--async-throttle)
     (consult--async-split))
   :sort nil
   :prompt "Entry: "
   :category 'journal))

;;;###autoload
(defun sacha-journal-id-from-string (s)
  (when (string-match "^[-0-9]+" s) (match-string 0 s)))

;;;###autoload
(defun sacha-journal-view (s)
  (interactive (list (sacha-journal-completing-read)))
  (sacha-org-journal-open (sacha-journal-id-from-string s)))

;;;###autoload
(defun sacha-journal-sketch-large (zid)
  "Create a large sketch based on ZID."
  (interactive (list (sacha-journal-completing-read)))
  (let ((filename (expand-file-name (format "%s.psd"
                                             (sacha-journal-id-from-string zid))
                                    sacha-sketch-inbox-directory)))
    (unless (file-exists-p filename)
      (copy-file sacha-sketch-large-template-file filename))
    (sacha-org-sketch-open filename)))
;; helm-journal ends here

;; [[file:../Sacha.org::#journal][Journal:3]]
;;;###autoload
(defun sacha-journal-format-entry (type o)
  (cond
   ((eq type 'org-link-zid-only)
    (org-link-make-string (format "journal:%s" (cdr (assoc 'ZIDString o)))))
   ((eq type 'list-item-with-zid)
    (format "- %s (%s)\n"
            (assoc-default 'Note o)
            (org-link-make-string
             (format "journal:%s" (assoc-default 'ZIDString o)))))
   ((eq type 'list-item)
    (format "- %s\n" (assoc-default 'Note o)))
   ((eq type 'text)
    (assoc-default 'Note o))))

;;;###autoload
(defun sacha-journal-format-entries (type list)
  (mapconcat
   (lambda (o) (sacha-journal-format-entry type o))
   (reverse list)
   (cond
    ((eq type 'org-link-zid-only) ", ")
    ((eq type 'list-item-with-zid) "")
    ((eq type 'list-item) "")
    ((eq type 'text) " "))))
;; Journal:3 ends here

;; [[file:../Sacha.org::org-journal-link][org-journal-link]]
;;;###autoload
(defun sacha-org-journal-open (id &optional arg)
  (browse-url (format "%s/zid/%s" sacha-journal-url id)))

;;;###autoload
(defun sacha-org-journal-export (link description format &optional arg)
  (let* ((path (concat "%s/zid/" sacha-journal-url link))
         (image (concat "%s/zid/" sacha-journal-url link))
         (desc (or description link)))
    (cond
     ((or (eq format 'html) (eq format 'wp))
      (if description
          (format "<a target=\"_blank\" href=\"%s\">%s</a>" path desc)
        (format "<a target=\"_blank\" href=\"%s\"><img src=\"%s\"><br />%s</a>" path image desc)))
     ((eq format 'latex) (format "\\href{%s}{%s}" path desc))
     ((eq format 'texinfo) (format "@uref{%s,%s}" path desc))
     ((eq format 'ascii) (format "%s <%s>" desc path))
     (t path))))

;;;###autoload
(defun sacha-org-journal-complete (&optional prefix)
  (cdr (assoc 'ZIDString (helm-comp-read "Entry: " 'sacha-helm-journal-search :volatile t))))
;; org-journal-link ends here

;; [[file:../Sacha.org::#journal][Journal:6]]
;;;###autoload
(defun sacha-org-journal-summarize (from to &optional search category-map categories)
  (sacha-org-group-journal-entries (sacha-journal-get-entries from to search) category-map categories))

;;;###autoload
(defun sacha-org-journal-format-tree (groups &optional include)
  (mapconcat
   (lambda (o)
     (concat "- *" (car o) "*\n"
             (mapconcat
              (lambda (i)
                (concat "  - "
                        (if (member 'date include) (concat (sacha-journal-date i) " ") "")
                        (replace-regexp-in-string "\\\"" "\"" (sacha-journal-note i))
                        (if (member 'zid include) (concat " " (sacha-journal-zidstring i)) "")
                        ;; (if (string= "" (sacha-journal-category i))
                        ;;     ""
                        ;;   (format " (%s)" (sacha-journal-category i)))
                        "\n"))
              (reverse (cdr o)) "")))
   groups ""))

;;;###autoload
(defun sacha-org-summarize-journal-csv (from to &optional search category-map categories include)
  (interactive
   (list (org-read-date nil nil nil "From: ")
         (org-read-date nil nil nil "To: ")
         (read-string "Search: ")
         sacha-journal-category-map
         sacha-journal-categories
         nil))
  (let ((list (sacha-org-journal-format-tree
               (sacha-org-group-journal-entries
                (sacha-journal-get-entries from to search)
                category-map categories)
               include)))
    (if (called-interactively-p 'any) (insert list) list)))
;; Journal:6 ends here

;; [[file:../Sacha.org::#journal][Journal:7]]
;;;###autoload
(defun sacha-read-journal-category ()
  (completing-read "Category: " sacha-journal-categories))

;;;###autoload
(defun sacha-update-journal-entry (old-text new-text category)
  (interactive (list (read-string "Old: ")
                     (read-string "New: ")
                     (sacha-read-journal-category)))
  (sacha-send-intent "com.sachachua.journal.categorize"
                  (list (cons "text" old-text)
                        (cons "newtext" (or new-text old-text))
                        (cons "category" (or category "Uncategorized")))))

;;;###autoload
(defun sacha-create-journal-entry (new-text category)
  (interactive (list (read-string "Text: ")
                     (sacha-read-journal-category)))
  (sacha-update-journal-entry new-text new-text category))

;;;###autoload
(defun sacha-export-journal-entries ()
  "Trigger task to export. Phone must be unlocked."
  (interactive)
  (sacha-send-intent "com.sachachua.journal.export" '(("a" . "b"))))

;; Journal:7 ends here

;; [[file:../Sacha.org::#journal][Journal:9]]
;;;###autoload
(defun sacha-prompt-for-uncategorized-entries ()
  (interactive)
  (let ((key-list '("Note" "Date" "highlight week" "Category" "month" "Time" "Link" "ELECT"))
        x new-text category done)
    (while (and (not (eobp)) (not done))
      (forward-char 1)
      (setq x (csv--read-line key-list))
      (when (string= (assoc-default "Category" x nil "") "")
        (setq text (read-string "Text: " (assoc-default "Note" x nil "")))
        (setq category (completing-read "Category: " (cons "." sacha-journal-categories)))
        (if (string= category ".")
            (setq done t)
          (sacha-update-journal-entry (assoc-default "Note" x nil "") text category))))))
;; Journal:9 ends here

;; [[file:../Sacha.org::#working-with-journal-entries][Working with journal entries:1]]
;;;###autoload
(defun sacha-journal-insert-matching-entries (from to match)
  (interactive (list (org-read-date "From: ") (org-read-date "To: ") (read-string "Match: ")))
  (insert
  (mapconcat
   (lambda (o)
     (format "- %s %s" (sacha-journal-zidstring o) (sacha-journal-note o)))
   (seq-filter (lambda (o) (string-match match (sacha-journal-other o)))
    (sacha-journal-get-entries from to))
   "\n")))
;;;###autoload
(defun sacha-journal-convert-to-refs (beg end)
  (interactive "r")
  (save-restriction
    (goto-char beg)
    (narrow-to-region beg end)
    (while (re-search-forward "^- \\([0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]-[0-9][0-9]\\) .*?$" nil t)
      (replace-match "ref:\\1"))))
;;;###autoload
  (defun sacha-journal-get-refs-from-region (beg end)
    (interactive "r")
    (save-excursion
      (goto-char beg)
      (cl-loop for pos = (re-search-forward " \\([0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]-[0-9][0-9]\\) " end t)
               while pos
               collect (match-string 1))))

;;;###autoload
(defun sacha-journal-add-tag (tag beg end)
  (interactive "MTag: \nr")
  (let* ((url-request-method "POST")
         (url-request-extra-headers '(("Content-Type" . "application/json")))
         (zids (sacha-journal-get-refs-from-region beg end))
         (json-object-type 'plist)
         (url-request-data (json-encode-plist (list :zids zids :tags (split-string tag " ")))))
    (pp (sacha-journal-get "api/entries/tag/bulk"))))

;;;###autoload
(defun sacha-journal-remove-tag (tag beg end)
  (interactive "MTag: \nr")
  (let* ((url-request-method "DELETE")
         (url-request-extra-headers '(("Content-Type" . "application/json")))
         (zids (sacha-journal-get-refs-from-region beg end))
         (json-object-type 'plist)
         (url-request-data (json-encode-plist (list :zids zids :tags (split-string tag " ")))))
    (pp (sacha-journal-get "api/entries/tag/bulk"))))

;;;###autoload
(defun sacha-journal-post-with-refs (note date other beg end)
  (interactive (list
                (read-string "Note: ")
                (concat (org-read-date "Date: ") " 23:00")
                (read-string "Other: ")
                (min (point) (mark))
                (max (point) (mark))))
  (sacha-journal-post note :Date date :Other (concat other "\n"
                                                  (mapconcat (lambda (o) (concat "ref:" o))
                                                             (sacha-journal-get-refs-from-region beg end)
                                                             " "))))

;;;###autoload
(defun sacha-journal-browse-current-day ()
	(interactive)
	(browse-url
	 (format "https://journal.sachachua.com/day/%s"
					 (format-time-string "%Y-%m-%d"
															 (sacha-filename-timestamp (buffer-file-name))))))
;; Working with journal entries:1 ends here

;; [[file:../Sacha.org::#tagging-journal-entries][Tagging journal entries:1]]
;;;###autoload
(defun sacha-journal-list-toggle-monthly-highlight ()
	(interactive)
	(let ((entry (tabulated-list-get-entry)))
		(setf (elt entry 3) (if (string-match "#monthly-highlight" (elt entry 3))
														(replace-regexp-in-string " ?#monthly-highlight" "" (elt entry 3))
													(string-trim (concat (elt entry 3) " #monthly-highlight"))))
		(sacha-journal-update
		 (list :ZIDString (elt entry 0)
					 :Other (elt entry 3)))
		(tabulated-list-print t t)))

;;;###autoload
(defun sacha-journal-list-echo ()
	(interactive)
	(message "%s -- %s" (elt (tabulated-list-get-entry) 2) (elt (tabulated-list-get-entry) 3)))

(defvar-keymap sacha-journal-list-mode-map
	:parent tabulated-list-mode-map
	"t" #'sacha-journal-list-toggle-monthly-highlight
	"v" #'sacha-journal-list-echo)

(define-derived-mode sacha-journal-list-mode tabulated-list-mode "Journal"
	"Major mode for journal entries."
	(setq tabulated-list-format [("ZID" 14 t)
															 ("Category" 10 t)
															 ("Note" 80 nil)
															 ("Other" 30 nil)])
	(tabulated-list-init-header)
	(tabulated-list-print t))

;;;###autoload
(defun sacha-journal-list (start end filter)
	(interactive (list (org-read-date "Start: ") (org-read-date "End: ")
										 (read-string "Filter: ")))
	(switch-to-buffer (get-buffer-create "*journal*"))
	(setq tabulated-list-entries
				(mapcar
				 (lambda (row)
					 (list
						(sacha-journal-zidstring row)
						(vector
						 (sacha-journal-zidstring row)
						 (sacha-journal-category row)
						 (replace-regexp-in-string "\n" " " (sacha-journal-note row))
						 (replace-regexp-in-string "\n" " " (sacha-journal-other row)))))
				 (sacha-journal-get-entries start end filter)))
	(sacha-journal-list-mode))
;; Tagging journal entries:1 ends here

;; [[file:../Sacha.org::#org-mode-journal-moments][Moments:1]]
;;;###autoload
(defun sacha-journal-moments (date)
	(interactive (list (org-read-date "Start: ")))
	(sacha-journal-post (concat "Moments starting " date " #moment") :Date (concat date " 23:00") :Category "Thoughts"))
;; Moments:1 ends here

;; [[file:../Sacha.org::#org-mode-journal-slicing-and-dicing-the-journal-entries][Slicing and dicing the journal entries:1]]
;;;###autoload
(defun sacha-journal-filter-by-category (category list)
	(reverse (seq-filter (lambda (o) (string= (sacha-journal-category o) "Eating"))
											 list)))
;;;###autoload
(defun sacha-journal-group-by-month (list)
	(seq-group-by (lambda (o)
									(substring (sacha-journal-date o) 0 7))
								list))
;;;###autoload
(defun sacha-journal-filter-by-month (month-regexp list)
	(seq-filter (lambda (o)
								(string-match month-regexp
															(substring (sacha-journal-date o) 5 7)))
								list))
;;;###autoload
(defun sacha-journal-group-by-month-day (list)
	(seq-group-by (lambda (o)
									(substring (sacha-journal-date o) 5))
								list))
;;;###autoload
(defun sacha-journal-list-with-day (list)
	(mapconcat (lambda (o)
							 (concat "  - " (substring (sacha-journal-date o) 8) " "
											 (replace-regexp-in-string "#.*" "" (sacha-journal-note o))))
						 list
						 "\n"))
;;;###autoload
(defun sacha-journal-list-with-year (list)
	(mapconcat (lambda (o)
							 (concat "  - " (substring (sacha-journal-date o) 0 4) " "
											 (replace-regexp-in-string "#.*" "" (sacha-journal-note o))))
						 list
						 "\n"))
;;;###autoload
(defun sacha-journal-this-month-by-day (list)
	(mapconcat (lambda (group)
							 (format
								"- %s\n%s"
								(car group)
								(sacha-journal-list-with-year (cdr group))))
						 (cl-sort
							(sacha-journal-group-by-month-day
							 (sacha-journal-filter-by-month (format-time-string "%02m")
																					 list))
						'string<
						:key #'car)
					 "\n"))
;; Slicing and dicing the journal entries:1 ends here

;; [[file:../Sacha.org::#easily-backfill-sacha-journal][Easily backfill my journal:1]]
;;;###autoload
(defun sacha-draw-journal-entry (date)
  "Creates a blank journal entry for DATE and brings up the log."
  (interactive (list (org-read-date)))
  ;; Open the Quantified Awesome time log for that date
  (let ((filename (sacha-get-journal-entry date))
        (day (format-time-string "%A" (org-time-string-to-time date))))
    (if filename
        (sacha-org-sketch-open filename)
      ;; (browse-url (format "http://quantifiedawesome.com/records?start=%s&end=%s"
      ;;                     date
      ;;                     (format-time-string
      ;;                      "%Y-%m-%d"
      ;;                      (seconds-to-time
      ;;                       (+ (org-time-string-to-seconds date) 86400)))))
      (setq filename
            (sacha-prepare-index-card-template (concat day " #daily #journal") date))
      (sacha-org-sketch-open filename))))

;;;###autoload
(defun sacha-get-journal-entry (date)
  "Returns the filename for the journal sketch for DATE."
  (car
   (-filter (lambda (x) (not (string-match "weekly" x)))
            (sacha-get-sketch-filenames
             (format "%s.* .*#daily" date)
             t))))

;;;###autoload
(defun sacha-get-missing-journal-dates (start-date end-date)
  "Return a list of dates missing journal entries.
      Range is specified by START-DATE (inclusive) and END-DATE (exclusive)."
  (let* ((current-day (org-time-string-to-absolute end-date))
         (start-day (org-time-string-to-absolute start-date))
         current-date
         current-date-string
         missing-list)
    (while (>= current-day start-day)
      (setq current-date (calendar-gregorian-from-absolute current-day))
      (setq current-date-string (format "%04d-%02d-%02d" (elt current-date 2) (elt current-date 0) (elt current-date 1)))
      (unless (sacha-get-journal-entry current-date-string)
        (add-to-list 'missing-list current-date-string))
      (setq current-day (1- current-day)))
    missing-list))

;;;###autoload
(defun sacha-show-missing-journal-entries (since)
  (interactive (list (if current-prefix-arg (org-read-date) (org-read-date nil nil "-7"))))
  (let ((missing-dates (sacha-get-missing-journal-dates since (org-read-date nil nil "."))))
    (with-current-buffer (sacha-set-up-sketch-buffer)
      (mapc
       (lambda (date)
         (widget-create 'push-button
                        :date date
                        :notify (lambda (widget &rest ignore)
                                  (sacha-draw-journal-entry (plist-get (cdr widget) :date)))
                        date))
       missing-dates)
      (widget-setup)
      (widget-minor-mode))))
;; Easily backfill my journal:1 ends here

(provide 'sacha-journal)
;;; sacha-journal.el ends here
