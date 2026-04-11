;;; sacha-org-def.el ---  -*- lexical-binding: t -*-

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
;; - Linking to and exporting function definitions in Org Mode
;;   https://sachachua.com/dotemacs#linking-to-and-exporting-function-definitions-in-org-mode
;;
;; - Still allow linking to the file
;;   https://sachachua.com/dotemacs#org-mode-linking-to-and-exporting-function-definitions-in-org-mode-still-allow-linking-to-the-file
;;
;; - Including variables
;;   https://sachachua.com/dotemacs#including-variables
;;
;;; Code:



;; [[file:../Sacha.org::org-defun-link][org-defun-link]]
;;;###autoload
(defun sacha-org-defun-complete ()
	"Return function definitions."
	(concat "defun:"
					(completing-read
					 "Function: "
					 #'help--symbol-completion-table
					 #'fboundp
					 'confirm
					 nil nil))) ; 	 (and fn (symbol-name fn)) ?

;;;###autoload
(defun sacha-org-defun-link-description (link description)
	"Add documentation string as part of the description"
	(unless description
		(when (string-match "defun:\\(.+\\)" link)
			(let ((symbol (intern (match-string 1 link))))
				(when (documentation symbol)
					(concat (symbol-name symbol) ": "
									(car (split-string (documentation symbol) "\n"))))))))

;;;###autoload
(defun sacha-org-defun-open-complete ()
	"Return function definitions."
	(concat "defun-open:"
					(completing-read
					 "Function: "
					 #'help--symbol-completion-table
					 #'fboundp
					 'confirm
					 nil nil)))

;;;###autoload
(defun sacha-org-defun-open-export (link description format _)
	(sacha-org-defun-export (concat link (if (string-match "\\?" link) "&open=1" "?open=1")) description format _))

;;;###autoload
(defun sacha-org-defun-export (link description format _)
	"Export the function."
	(let (symbol params path-and-query
        sacha-org-defun-open-special)
		(if (string-match "\\?" link)
				(setq path-and-query (url-path-and-query (url-generic-parse-url link))
							symbol (car path-and-query)
							params (url-parse-query-string (cdr path-and-query)))
			(setq symbol link))
		(save-window-excursion
			(sacha-org-defun-open symbol t)
			(let ((function-body
						 ;; get the whole babel block instead of the sexp at point
             (if (derived-mode-p 'org-mode)
						     (nth 2 (org-src--contents-area (org-element-at-point)))
               (thing-at-point 'defun)))
						body)
				(pcase format
					((or '11ty 'html)
					 (setq body
								 (if (assoc-default "bare" params 'string=)
										 (format "<div class=\"org-src-container\"><pre class=\"src src-emacs-lisp\">%s</pre></div>"
														 (org-html-do-format-code function-body "emacs-lisp" nil nil nil nil))
									 (format "<details%s><summary>%s</summary><div class=\"org-src-container\"><pre class=\"src src-emacs-lisp\">%s</pre></div></details>"
													 (if (assoc-default "open" params 'string=) " open"
														 "")
													 (or description
															 (and (documentation (intern symbol))
																		(concat
																		 symbol
																		 ": "
																		 (car (split-string (documentation (intern symbol)) "\n"))))
															 symbol)
													 (org-html-do-format-code function-body "emacs-lisp" nil nil nil nil))))
					 (when (assoc-default "link" params)
						 (setq body (format "%s<div><a href=\"%s\">Context</a></div>" body (sacha-copy-link))))
					 body)
					('latex
					 (org-latex-src-block `(test
																	(:language "emacs-lisp" :value ,function-body))
																function-body nil))
					('ascii function-body)
					(_ function-body))))))

;;;###autoload
(defun sacha-org-defun-store ()
	"Store a link to the function."
	(when (derived-mode-p 'emacs-lisp-mode)
		(org-link-store-props :type "defun"
													:link (concat "defun:" (lisp-current-defun-name)))))

(defvar sacha-org-defun-open-special t)

;;;###autoload
(defun sacha-org-defun-open (symbol &rest _)
	"Jump to the function definition.
If it's from a tangled file, follow the link."
  (interactive (list (symbol-at-point)))
  (when (symbolp symbol) (setq symbol (symbol-name symbol)))
  (cond
   ((fboundp (intern symbol)) (find-function (intern symbol)))
   ((boundp (intern symbol)) (find-variable (intern symbol))))
	(when (re-search-backward "^;; \\[\\[file:" nil t)
		(goto-char (match-end 0))
		(org-open-at-point-global)
		(when (re-search-forward (concat "( *\\(cl-\\)?defun +" (regexp-quote (replace-regexp-in-string "\\?.*$" "" symbol)))
														 nil t)
			(goto-char (match-beginning 0))
      (when (and sacha-org-defun-open-special
                 (org-in-src-block-p))
        (org-edit-special)
        (goto-char (point-min))
        (when (re-search-forward (concat "( *\\(cl-\\)?defun +" (regexp-quote (replace-regexp-in-string "\\?.*$" "" symbol)))
														     nil t)
          (goto-char (match-beginning 0)))))))

(org-link-set-parameters "defun" :follow #'sacha-org-defun-open
												 :export #'sacha-org-defun-export
												 :complete #'sacha-org-defun-complete
												 :insert-description #'sacha-org-defun-link-description
												 :store #'sacha-org-def-store)

(org-link-set-parameters "defun-open" :follow #'sacha-org-defun-open
												 :export #'sacha-org-defun-open-export
												 :complete #'sacha-org-defun-open-complete
												 :insert-description #'sacha-org-defun-link-description)
;; org-defun-link ends here

;; [[file:../Sacha.org::#org-mode-linking-to-and-exporting-function-definitions-in-org-mode-still-allow-linking-to-the-file][Still allow linking to the file:1]]
;;;###autoload
(defun sacha-org-defun-store-file-link ()
	"Store a link to the file itself."
	(when (derived-mode-p 'emacs-lisp-mode)
		(org-link-store-props :type "file"
													:link (concat "file:" (buffer-file-name)))))
;; Still allow linking to the file:1 ends here

;; [[file:../Sacha.org::org-defvar-link][org-defvar-link]]
;;;###autoload
(defun sacha-org-defvar-complete ()
	"Return variable definitions."
	(concat "defvar:"
					(completing-read
					 "Variable: "
					 #'help--symbol-completion-table
					 #'indirect-variable
					 'confirm
					 nil nil))) ; 	 (and fn (symbol-name fn)) ?
;;;###autoload
(defun sacha-org-defvar-link-description (link description)
	"Add documentation string as part of the description"
	(unless description
		(when (string-match "\\(?:defun\\|defvar\\):\\(.+\\)" link)
			(let* ((symbol (intern (match-string 1 link)))
						 (doc (documentation-property symbol 'variable-documentation symbol)))
				(when doc
					(concat (symbol-name symbol) ": "
									(car (split-string doc "\n"))))))))

;;;###autoload
(defun sacha-org-def-export (link description format _)
	"Export the variable-or-function."
	(let (symbol params path-and-query)
		(if (string-match "\\?" link)
				(setq path-and-query (url-path-and-query (url-generic-parse-url link))
							symbol (car path-and-query)
							params (url-parse-query-string (cdr path-and-query)))
			(setq symbol link))
		(save-window-excursion
			(if (functionp (intern symbol))
					(find-function (intern symbol))
				(find-variable (intern symbol)))
			(let ((body (buffer-substring (point)
																		(progn (forward-sexp) (point)))))
				(pcase format
					((or '11ty 'html)
					 (if (assoc-default "bare" params 'string= "")
							 (format "<div class=\"org-src-container\"><pre class=\"src src-emacs-lisp\">%s</pre></div>"
											 (org-html-do-format-code body "emacs-lisp" nil nil nil nil))

						 (format "<details%s><summary>%s</summary><div class=\"org-src-container\"><pre class=\"src src-emacs-lisp\">%s</pre></div></details>"
										 (if (assoc-default "open" params 'string=) " open"
											 "")
										 (or description
												 (and (functionp (intern symbol))
															(documentation (intern symbol))
															(concat
															 symbol
															 ": "
															 (car (split-string (documentation (intern symbol)) "\n"))))
												 symbol)
										 (org-html-do-format-code body "emacs-lisp" nil nil nil nil))
						 ))
					(`ascii body)
					(_ body))))))

;;;###autoload
(defun sacha-org-def-store ()
	"Store a link to the function."
	(when (derived-mode-p 'emacs-lisp-mode)
		(save-excursion
      (or (eobp) (forward-char 1))
      (condition-case nil
          (progn
            (beginning-of-defun)
			      (let ((data (read (current-buffer))))
				      (cond
               ((not (listp data)) nil)
               ((eq (car data) 'defun)
				        (org-link-store-props :type "defun"
															        :link (concat "defun:" (lisp-current-defun-name))))
               ((member (car data) '(defvar defcustom))
				        (org-link-store-props :type "defvar"
															        :link (format "defvar:%s" (cadr data)))))))
        (error nil)))))

;;;###autoload
(defun sacha-org-defvar-open (symbol _)
	"Jump to the function definition."
	(find-variable (intern (replace-regexp-in-string "\\?.*$" "" symbol))))

(org-link-set-parameters "defvar" :follow #'sacha-org-defvar-open
												 :export #'sacha-org-def-export
												 :complete #'sacha-org-defvar-complete
												 :insert-description #'sacha-org-defvar-link-description
												 ; :store #'sacha-org-def-store  ; already added by defun link
												 )
;; org-defvar-link ends here

(provide 'sacha-org-def)
;;; sacha-org-def.el ends here
