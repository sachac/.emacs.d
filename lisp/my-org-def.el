;;;###autoload
(defun my-org-defun-complete ()
	"Return function definitions."
	(concat "defun:"
					(completing-read
					 "Function: "
					 #'help--symbol-completion-table
					 #'fboundp
					 'confirm
					 nil nil))) ; 	 (and fn (symbol-name fn)) ?

;;;###autoload
(defun my-org-defun-link-description (link description)
	"Add documentation string as part of the description"
	(unless description
		(when (string-match "defun:\\(.+\\)" link)
			(let ((symbol (intern (match-string 1 link))))
				(when (documentation symbol)
					(concat (symbol-name symbol) ": "
									(car (split-string (documentation symbol) "\n"))))))))

;;;###autoload
(defun my-org-defun-open-complete ()
	"Return function definitions."
	(concat "defun-open:"
					(completing-read
					 "Function: "
					 #'help--symbol-completion-table
					 #'fboundp
					 'confirm
					 nil nil)))

;;;###autoload
(defun my-org-defun-open-export (link description format _)
	(my-org-defun-export (concat link (if (string-match "\\?" link) "&open=1" "?open=1")) description format _))

;;;###autoload
(defun my-org-defun-export (link description format _)
	"Export the function."
	(let (symbol params path-and-query
        my-org-defun-open-special)
		(if (string-match "\\?" link)
				(setq path-and-query (url-path-and-query (url-generic-parse-url link))
							symbol (car path-and-query)
							params (url-parse-query-string (cdr path-and-query)))
			(setq symbol link))
		(save-window-excursion
			(my-org-defun-open symbol t)
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
						 (setq body (format "%s<div><a href=\"%s\">Context</a></div>" body (my-copy-link))))
					 body)
					('latex
					 (org-latex-src-block `(test
																	(:language "emacs-lisp" :value ,function-body))
																function-body nil))
					('ascii function-body)
					(_ function-body))))))

;;;###autoload
(defun my-org-defun-store ()
	"Store a link to the function."
	(when (derived-mode-p 'emacs-lisp-mode)
		(org-link-store-props :type "defun"
													:link (concat "defun:" (lisp-current-defun-name)))))

(defvar my-org-defun-open-special t)

;;;###autoload
(defun my-org-defun-open (symbol &rest _)
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
      (when (and my-org-defun-open-special
                 (org-in-src-block-p))
        (org-edit-special)
        (goto-char (point-min))
        (when (re-search-forward (concat "( *\\(cl-\\)?defun +" (regexp-quote (replace-regexp-in-string "\\?.*$" "" symbol)))
														     nil t)
          (goto-char (match-beginning 0)))))))

(org-link-set-parameters "defun" :follow #'my-org-defun-open
												 :export #'my-org-defun-export
												 :complete #'my-org-defun-complete
												 :insert-description #'my-org-defun-link-description
												 :store #'my-org-def-store)

(org-link-set-parameters "defun-open" :follow #'my-org-defun-open
												 :export #'my-org-defun-open-export
												 :complete #'my-org-defun-open-complete
												 :insert-description #'my-org-defun-link-description)

;;;###autoload
(defun my-org-defun-store-file-link ()
	"Store a link to the file itself."
	(when (derived-mode-p 'emacs-lisp-mode)
		(org-link-store-props :type "file"
													:link (concat "file:" (buffer-file-name)))))

;;;###autoload
(defun my-org-defvar-complete ()
	"Return variable definitions."
	(concat "defvar:"
					(completing-read
					 "Variable: "
					 #'help--symbol-completion-table
					 #'indirect-variable
					 'confirm
					 nil nil))) ; 	 (and fn (symbol-name fn)) ?
;;;###autoload
(defun my-org-defvar-link-description (link description)
	"Add documentation string as part of the description"
	(unless description
		(when (string-match "\\(?:defun\\|defvar\\):\\(.+\\)" link)
			(let* ((symbol (intern (match-string 1 link)))
						 (doc (documentation-property symbol 'variable-documentation symbol)))
				(when doc
					(concat (symbol-name symbol) ": "
									(car (split-string doc "\n"))))))))

;;;###autoload
(defun my-org-def-export (link description format _)
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
(defun my-org-def-store ()
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
(defun my-org-defvar-open (symbol _)
	"Jump to the function definition."
	(find-variable (intern (replace-regexp-in-string "\\?.*$" "" symbol))))

(org-link-set-parameters "defvar" :follow #'my-org-defvar-open
												 :export #'my-org-def-export
												 :complete #'my-org-defvar-complete
												 :insert-description #'my-org-defvar-link-description
												 ; :store #'my-org-def-store  ; already added by defun link
												 )
