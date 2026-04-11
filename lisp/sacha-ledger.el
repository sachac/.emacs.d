;;; sacha-ledger.el ---  -*- lexical-binding: t -*-

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



;; [[file:../Sacha.org::#ledger-personal-finance-in-sacha-config][Ledger:3]]
(defvar-local sacha-ledger-account-list-cache nil)
(defadvice ledger-accounts-list (around sacha activate)
  "Cache"
  (setq ad-return-value (or sacha-ledger-account-list-cache
                            (setq sacha-ledger-account-list-cache ad-do-it))))

;;;###autoload
(defun sacha-ledger-set-unknown-account (account point)
  (interactive (list (ledger-read-account-with-prompt "Account") (point)))
  (let ((extents (ledger-navigate-find-xact-extents point)))
    (save-excursion
      (goto-char (car extents))
      (if (re-search-forward "Expenses:\\(Unknown\\|Play\\)" (cadr extents) t)
          (replace-match account t t)
        (goto-char point)
        (beginning-of-line)
        (when (re-search-forward "\\([^ \t]+\\)  " (line-end-position) nil)
          (replace-match account t t nil 1))))))

;;;###autoload
(defun sacha-ledger-go-to-beginning-of-entry ()
  "Move to the beginning of the current entry."
  (while (and (not (bobp))
              (eq (ledger-context-line-type (ledger-context-at-point))
                  'acct-transaction))
    (forward-line -1)))

;;;###autoload
(defun sacha-ledger-entry-date ()
  "Returns the date of the entry containing point or nil."
  (save-excursion
    (sacha-ledger-go-to-beginning-of-entry)
    (let ((context-info (ledger-context-other-line 0)))
      (when (eq (ledger-context-line-type context-info) 'entry)
        (goto-char (line-beginning-position))
        (if (looking-at "\\([-0-9\\./]+\\)")
            (match-string-no-properties 1))))))

;;;###autoload
(defun sacha-ledger-guess-mbna ()
  "Adds a sub-account for the dates for my credit card transactions."
  (interactive)
  (save-excursion
    (sacha-ledger-go-to-beginning-of-entry)
    (forward-line 1)
    (let ((amount 0) (date (sacha-ledger-entry-date)) month)
      (if (string-match "[0-9]+[-\\.]\\([0-9]+\\)[-\\.]\\([0-9]+\\)" date)
          (setq month (string-to-number (match-string 1 date))))
      ;; Is this a payment or a charge?
      (save-excursion
        (while (and (eq (ledger-context-line-type (ledger-context-at-point))
                        'acct-transaction)
                    (not (eobp)))
          (let ((context (ledger-context-at-point)))
            (if (ledger-context-field-value context 'amount)
                (if (string-match "MBNA" (ledger-context-field-value context 'account))
                    (setq amount (string-to-number (ledger-context-field-value context 'amount)))
                  (setq amount (- (string-to-number (ledger-context-field-value context 'amount)))))))
          (forward-line 1)))
      (save-excursion
        (while (and (eq (ledger-context-line-type (ledger-context-at-point))
                        'acct-transaction)
                    (not (eobp)))
          (let ((context (ledger-context-at-point)))
            (if (string-match "MBNA" (ledger-context-field-value context 'account))
                (if (re-search-forward "\\(MBNA\\)[ \t]*[-$\.0-9]*[ \t]*$" (line-end-position) t)
                    (replace-match
                     (concat "MBNA:"
                             (elt
                              '("January" "February" "March" "April" "May" "June" "July" "August" "September" "October" "November" "December")
                              (% (+ (if (> amount 0) 10 11) month) 12)))
                     t t nil 1))))
          (forward-line 1))))))

;;;###autoload
(defun sacha-ledger-change-account (account)
  (interactive (list (ledger-read-account-with-prompt (concat (ledger-xact-payee) ": "))))
  (beginning-of-line)
  (re-search-forward ledger-account-name-or-directive-regex)
  (replace-match (concat "  " account "  ") t t))

;;;###autoload
(defun sacha-ledger-fix-unknown ()
  (interactive)
  (while (re-search-forward "Expenses:Unknown.*$ \\(.+\\)" nil t)
    (sacha-ledger-change-account (ledger-read-account-with-prompt
                               (format "%s %s: " (s-trim (save-match-data (ledger-xact-payee)))
                                       (match-string 1))))))

;; Ledger:3 ends here

(provide 'sacha-ledger)
;;; sacha-ledger.el ends here
