;;; my-elisp.el ---  -*- lexical-binding: t -*-

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
;; - Prefix for writing functions
;;   https://sachachua.com/dotemacs#coding-emacs-lisp-prefix-for-writing-functions
;;
;; - Easily override existing functions
;;   https://sachachua.com/dotemacs#easily-override-existing-functions
;;
;; - Edebug
;;   https://sachachua.com/dotemacs#edebug
;;
;; - ERT
;;   https://sachachua.com/dotemacs#ert
;;
;; - Buttercup
;;   https://sachachua.com/dotemacs#buttercup
;;
;; - Eldoc
;;   https://sachachua.com/dotemacs#eldoc
;;
;; - Sorting
;;   https://sachachua.com/dotemacs#sorting
;;
;; - Evaluation
;;   https://sachachua.com/dotemacs#evaluation
;;
;; - Stubbing
;;   https://sachachua.com/dotemacs#stubbing
;;
;; - Collecting Emacs News from Mastodon
;;   https://sachachua.com/dotemacs#mastodon-news
;;
;; - Combining Mastodon timelines using mastodon.el
;;   https://sachachua.com/dotemacs#mastodon-combined-timeline
;;
;; - Using Spookfox to scroll Firefox up and down from Emacs
;;   https://sachachua.com/dotemacs#spookfox-scroll
;;
;; - Ledger
;;   https://sachachua.com/dotemacs#ledger-personal-finance-in-my-config
;;
;;; Code:



;; [[file:../Sacha.org::#coding-emacs-lisp-prefix-for-writing-functions][Prefix for writing functions:1]]
;;;###autoload
(defun my-function-prefix ()
  (if (and (buffer-file-name) (string-match "\\.el\\'" (buffer-file-name)))
      (concat (file-name-base (buffer-file-name)) "-")
    "my-"))
;; Prefix for writing functions:1 ends here

;; [[file:../Sacha.org::#easily-override-existing-functions][Easily override existing functions:1]]
;;;###autoload
(defun my-override-function (symbol)
	(interactive (list (completing-read
											"Function: "
											#'help--symbol-completion-table
											#'fboundp
											'confirm nil nil)))
	(let (function-body function-name)
		(save-window-excursion
			(find-function (intern symbol))
			(setq function-name (lisp-current-defun-name))
			(setq function-body (buffer-substring (point)
																						(progn (forward-sexp) (point)))))
		(save-excursion
			(insert function-body (format "\n\n(advice-add '%s :override #'my-%s)\n" function-name function-name)))
		(save-excursion
			(forward-char 1)
			(forward-sexp 1)
			(skip-syntax-forward " ")
			(insert "my-")
			(forward-sexp 1)
			(skip-syntax-forward " ")
			(forward-char 1))))
;; Easily override existing functions:1 ends here

;; [[file:../Sacha.org::#edebug][Edebug:1]]
(require 'eros)
;;;###autoload
(defun adviced:edebug-previous-result (_ &rest r)
  "Adviced `edebug-previous-result'."
  (eros--make-result-overlay edebug-previous-result
    :where (point)
    :duration eros-eval-result-duration))

;;;###autoload
(defun adviced:edebug-compute-previous-result (_ &rest r)
  "Adviced `edebug-compute-previous-result'."
  (let ((previous-value (nth 0 r)))
    (if edebug-unwrap-results
        (setq previous-value
              (edebug-unwrap* previous-value)))
    (setq edebug-previous-result
          (edebug-safe-prin1-to-string previous-value))))
;; Edebug:1 ends here

;; [[file:../Sacha.org::#ert][ERT:1]]
;;;###autoload
(defun my-eval-buf-and-run-ert-test-at-point ()
  "Evaluate the current buffer and run the ERT test at point."
  (interactive)
  (save-excursion
    (beginning-of-defun)
    (unless (looking-at "(ert-deftest\\s-+")
      (user-error "Not at an ERT test"))
    (goto-char (match-end 0))
    (let ((test-name (thing-at-point 'symbol)))
      (unless test-name
        (user-error "Couldn't get ERT test name"))
      (eval-buffer)
      (ert-run-tests-interactively test-name))))

;;;###autoload
(defun my-ert-visit-test-file (&optional base)
  (let* ((base (or base (buffer-file-name)))
         (filename
          (and base
               (seq-find
                #'file-exists-p
                (list
                 (expand-file-name
                  (concat "tests/" (file-name-base) "-test.el")
                  (file-name-directory base))
                 (expand-file-name
                  (concat "../tests/" (file-name-base) "-test.el")
                  (file-name-directory base))
                 (concat (file-name-sans-extension base) "-test.el"))))))
    (when filename
      (find-file filename))))

;;;###autoload
(defun my-ert-find-insert-point (func-name)
  (goto-char (point-min))
  (cond
   ((re-search-forward (format "(ert-deftest %s"
                               (if (symbolp func-name)
                                   (regexp-quote (symbol-name func-name))
                                 func-name)) nil t)
    (goto-char (match-beginning 0)))
   ((re-search-forward ";;; Code" nil t)
    (forward-line))
   ((re-search-forward "^$" nil t))
   (t (goto-char (point-max)))))

(declare-function which-function "which-function")
;;;###autoload
(defun my-ert-test-from-function-at-point ()
  "Create an ERT test template for the function at point."
  (interactive)
  (let* ((func (or (and (functionp (symbol-at-point)) (symbol-at-point))
                   (which-function)))
         (test-def (and func
                        (format
                         "(ert-deftest %s ()
  \"Tests `%s'.\"
  (should (equal (%s) t)))\n\n"
                         func func func))))
    (if (not func)
        (error "No function at point")
      ;; Open the test file
      (my-ert-visit-test-file)
      (my-ert-find-insert-point func)
      (insert test-def)
      (backward-char 7))))
;; ERT:1 ends here

;; [[file:../Sacha.org::#buttercup][Buttercup:1]]
(defvar my-buttercup-source-buffer nil)
(defvar my-buttercup-tests nil)
;;;###autoload
(defun my-buttercup-track-source ()
	(interactive)
	(setq my-buttercup-source-buffer (current-buffer))
	(setq my-buttercup-tests (my-buttercup-tests-and-positions)))

;;;###autoload
(defun my-buttercup-run-dwim ()
	(interactive)
	(let ((lexical-binding t))
		(if buttercup-minor-mode
				(my-buttercup-run-closest-at-point)
			(buttercup-run))))

;; (advice-remove 'buttercup-run 'my-buttercup-track-source)
;;;###autoload
(defun my-buttercup-run-closest-at-point ()
  "Run the buttercup suite at point."
  (interactive)
  (let ((lexical-binding t)
				start)
		(setq buttercup-suites nil)
    (save-selected-window
			(save-excursion
				(save-restriction
					;; go up until we find a describe form
					(while (not (looking-at "([[:space:]]*describe[[:space:]]+"))
						(backward-up-list nil t))
					(setq start (point))
					(forward-sexp)
					(narrow-to-region start (point))
					(eval-last-sexp nil)
					(my-buttercup-track-source)))
      (buttercup-run))
    (message "Suite executed successfully")))

;;;###autoload
(defun my-buttercup-find-test ()
	(interactive)
	(if (re-search-backward (make-string 40 ?=) nil t)
			(progn
				(forward-line)
				(let ((pos (assoc-default (buffer-substring (line-beginning-position)
																										(line-end-position))
																	my-buttercup-tests)))
					(when pos
						(pop-to-buffer my-buttercup-source-buffer)
						(goto-char pos))))
		(let ((tests (my-buttercup-tests-and-positions)))
			(goto-char (assoc-default (completing-read "Test: "
																								 (my-presorted-completion-table tests))
																tests)))))

;;;###autoload
(defun my-buttercup-test-name ()
	(save-excursion
		(let (list)
			(condition-case err
					(progn
						(while (not (bobp))
							(let ((form (save-excursion
														(ignore-errors
															(read (current-buffer))))))
								(when (listp form) (and (member (car form) '(describe it)))
											(setq list (cons (cadr form) list)))
								(backward-up-list nil t)))
						(string-join list " "))
				(error
				 (string-join list " "))))))

;;;###autoload
(defun my-buttercup-tests-and-positions-lookup ()
	"Return a list of test names and points, for easier jumping."
	;; This is a very inefficient implementation. I wonder how to walk the tree...
	(goto-char (point-min))
	(cl-loop while (re-search-forward "([[:space:]]*it[[:space:]]+\"" nil t)
					 collect (cons (my-buttercup-test-name) (point))))

;;;###autoload
(defun my-buttercup-tests-as-tree ()
	"Return the tests as nested lists ending with (description . point).
Useful as `imenu-create-index-function'."
	(goto-char (point-min))
	(let (result)
		(condition-case _
				(progn
					(down-list)
					(while (not (eobp))
						(cond
						 ((looking-at "describe\\_>")
							(forward-sexp)
							(setq result (cons
														(cons (read (current-buffer))
																	(save-restriction
																		(narrow-to-region
																		 (point)
																		 (progn
																			 (up-list)
																			 (1- (point))))
																		(my-buttercup-tests-as-tree)))
														result)))
						 ((looking-at "it\\_>")
							(forward-sexp)
							(setq result (cons
														(cons (read (current-buffer)) (point))
														result))
							(up-list)
							(down-list))
						 (t
							;; todo, handle other things
							(up-list)
							(down-list)))))
			(scan-error
			 ;; can't go down or forward
			 (reverse result)))))

;;;###autoload
(defun my-buttercup-set-up-imenu ()
	(setq-local imenu-generic-expression nil)
	(setq-local imenu-create-index-function #'my-buttercup-tests-as-tree))

;;;###autoload
(defun my-buttercup-tests-and-positions ()
	"Return test names and points to jump to."
	(save-excursion
		(goto-char (point-min))
		(condition-case _
				(progn
					(down-list)
					(let (breadcrumbs sym result)
						(catch 'done
							(while (not (eobp))
								(condition-case _
										(cond
										 ((looking-at "describe[[:space:]]+")
											(forward-sexp)
											(setq breadcrumbs (cons (read (current-buffer)) breadcrumbs))
											;; ignore :var and :var*
											(when (looking-at "[\n[:space:]]+:var\\*?")
												(read (current-buffer))
												(read (current-buffer)))
											(down-list))
										 ((looking-at "it[[:space:]]+")
											(forward-sexp)
											(setq result (cons (cons
																					(string-join
																					 (reverse
																						(delq nil
																									(cons (read (current-buffer)) breadcrumbs)))
																					 " ")
																					(point))
																				 result))
											(up-list)
											(down-list))
										 (t
											;; might be something else that includes describe or it, so we explore it
											(setq breadcrumbs (cons nil breadcrumbs))
											(down-list)
											))
									(scan-error
									 ;; At the innermost thing, time to start going forward
									 (condition-case _
											 (progn
												 ;; Try to go down. If we can, continue
												 ;; processing. If we can't, go up until we
												 ;; can go down.
												 (while (condition-case _
																		(down-list)
																	(error t))
													 (up-list)
													 (setq breadcrumbs (cdr breadcrumbs))))
										 (scan-error
											(error (throw 'done (reverse result)))))))))
						(reverse result)))
			(error nil))))




(ert-deftest my-buttercup-tests-and-positions ()
	(with-temp-buffer
		(insert "(describe \"test\"
	:var ((test))
	(it \"1\")
	(it \"2\")
	(describe \"b\"
		(before-each \"do this\")
		(it \"3\")
		(it \"4\"))
	(describe \"c\"
		(it \"5\")
		(it \"6\")
		(it \"7\")
		(describe \"d\"
			(it \"8\")))
	(describe \"e\"
		(it \"5\")
		(it \"6\")
		(it \"7\")
		(describe \"f\"
			(it \"8\")))
	)")
		(let ((tests (my-buttercup-tests-and-positions)))
			(expect (assoc "test 1" tests))
			(expect (assoc "test 2" tests))
			(expect (assoc "test b 3" tests))
			(expect (assoc "test b 4" tests))
			(expect (assoc "test c 5" tests))
			(expect (assoc "test e f 8" tests)))))
;; Buttercup:1 ends here

;; [[file:../Sacha.org::#eldoc][Eldoc:3]]
;;;###autoload
(defun mp-flycheck-eldoc (callback &rest _ignored)
   "Print flycheck messages at point by calling CALLBACK."
   (when-let ((flycheck-errors (and flycheck-mode (flycheck-overlay-errors-at (point)))))
     (mapc
      (lambda (err)
        (funcall callback
           (format "%s: %s"
                   (let ((level (flycheck-error-level err)))
                     (pcase level
                       ('info (propertize "I" 'face 'flycheck-error-list-info))
                       ('error (propertize "E" 'face 'flycheck-error-list-error))
                       ('warning (propertize "W" 'face 'flycheck-error-list-warning))
                       (_ level)))
                   (flycheck-error-message err))
           :thing (or (flycheck-error-id err)
                      (flycheck-error-group err))
           :face 'font-lock-doc-face))
      flycheck-errors)))
;;;###autoload
  (defun mp-flycheck-prefer-eldoc ()
    (add-hook 'eldoc-documentation-functions #'mp-flycheck-eldoc nil t)
    (setq eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
    (setq flycheck-display-errors-function nil)
    (setq flycheck-help-echo-function nil))
;; Eldoc:3 ends here

;; [[file:../Sacha.org::#sorting][Sorting:1]]
;;;###autoload
(defun my-sort-sexps-in-region (beg end)
  "Can be handy for sorting out duplicates.
       Sorts the sexps from BEG to END. Leaves the point at where it
       couldn't figure things out (ex: syntax errors)."
  (interactive "r")
  (let ((input (buffer-substring beg end))
        list last-point form result)
    (save-restriction
      (save-excursion
        (narrow-to-region beg end)
        (goto-char (point-min))
        (setq last-point (point-min))
        (setq form t)
        (while (and form (not (eobp)))
          (setq form (ignore-errors (read (current-buffer))))
          (when form
            (add-to-list
             'list
             (cons
              (prin1-to-string form)
              (buffer-substring last-point (point))))
            (setq last-point (point))))
        (setq list (sort list (lambda (a b) (string< (car a) (car b)))))
        (delete-region (point-min) (point))
        (insert (mapconcat 'cdr list "\n"))))))
;; Sorting:1 ends here

;; [[file:../Sacha.org::#evaluation][Evaluation:1]]
;;;###autoload
(defun sanityinc/eval-last-sexp-or-region (prefix)
  "Eval region from BEG to END if active, otherwise the last sexp."
  (interactive "P")
  (if (and (mark) (use-region-p))
      (eval-region (min (point) (mark)) (max (point) (mark)))
    (pp-eval-last-sexp prefix)))
;; Evaluation:1 ends here

;; [[file:../Sacha.org::#stubbing][Stubbing:1]]
;;;###autoload
(defun my-stub-elisp-defun ()
  "Stub an elisp function from symbol at point."
  (interactive)
  (let* ((fun (thing-at-point 'list 'no-properties)))
    (when fun
      (let* ((fun-list (car (read-from-string fun)))
             (name (symbol-name (nth 0 fun-list)))
             (args (cdr fun-list)))
        (save-excursion
          (or (search-backward "(defun" nil 't) (goto-char (point-min)))
          (insert
           (s-concat
            "(defun "
            name
            " "
            (format "%s" (--map (s-concat "arg" (number-to-string it)) (number-sequence 1 (length args))))
            "\n  \"SomeDocs\"\n  nil)\n\n")))))))
;; Stubbing:1 ends here

;; [[file:../Sacha.org::#mastodon-news][Collecting Emacs News from Mastodon:3]]
;;;###autoload
(defun my-match-groups (&optional object)
	"Return the matching groups, good for debugging regexps."
	(seq-map-indexed (lambda (entry i)
										 (list i entry
													 (and (car entry)
																(if object
																		(substring object (car entry) (cadr entry))
																	(buffer-substring (car entry) (cadr entry))))))
									 (seq-partition
										(match-data t)
										2)))
;; Collecting Emacs News from Mastodon:3 ends here

;; [[file:../Sacha.org::#mastodon-combined-timeline][Combining Mastodon timelines using mastodon.el:4]]
;;;###autoload
(defun my-text-property-update-at-point (pos prop value)
	(let ((start (previous-single-property-change (or pos (point)) prop))
				(end (next-single-property-change (or pos (point)) prop)))
		(put-text-property (or start (point-min))
											 (or end (point-max))
											 prop value)))
;; Combining Mastodon timelines using mastodon.el:4 ends here

;; [[file:../Sacha.org::mastodon-comparison][mastodon-comparison]]
;;;###autoload
(defun my-three-way-comparison (seq1 seq2 seq3 &optional test-fn)
	`(("1" ,@(seq-difference seq1 (seq-union seq2 seq3 test-fn) test-fn))
		("2" ,@(seq-difference seq2 (seq-union seq1 seq3 test-fn) test-fn))
		("3" ,@(seq-difference seq3 (seq-union seq1 seq2 test-fn) test-fn))
		("1&2" ,@(seq-difference (seq-intersection seq1 seq2 test-fn) seq3 test-fn))
		("1&3" ,@(seq-difference (seq-intersection seq1 seq3 test-fn) seq2 test-fn))
		("2&3" ,@(seq-difference (seq-intersection seq2 seq3 test-fn) seq1 test-fn))
		("1&2&3" ,@(seq-intersection (seq-intersection seq2 seq3 test-fn) seq1 test-fn))))

;;;###autoload
(defun my-three-way-comparison-report (label1 seq1 label2 seq2 label3 seq3 &optional test-fn)
	(let ((list (my-three-way-comparison seq1 seq2 seq3)))
		`((,(format "%s only" label1) ,@(assoc-default "1" list #'string=))
			(,(format "%s only" label2) ,@(assoc-default "2" list #'string=))
			(,(format "%s only" label3) ,@(assoc-default "3" list #'string=))
			(,(format "%s & %s" label1 label2) ,@(assoc-default "1&2" list #'string=))
			(,(format "%s & %s" label1 label3) ,@(assoc-default "1&3" list #'string=))
			(,(format "%s & %s" label2 label3) ,@(assoc-default "2&3" list #'string=))
			("all" ,@(assoc-default "1&2&3" list #'string=)))))
;; mastodon-comparison ends here

;; [[file:../Sacha.org::#spookfox-scroll][Using Spookfox to scroll Firefox up and down from Emacs:5]]
;;https://emacs.stackexchange.com/questions/41801/how-to-stop-completing-read-ivy-completing-read-from-sorting
;;;###autoload
(defun my-presorted-completion-table (completions)
  (lambda (string pred action)
    (if (eq action 'metadata)
        '(metadata
					(cycle-sort-function . identity)
					(display-sort-function . identity))
      (complete-with-action action completions string pred))))
;; Using Spookfox to scroll Firefox up and down from Emacs:5 ends here

;; [[file:../Sacha.org::#ledger-personal-finance-in-my-config][Ledger:4]]
;;;###autoload
(defun my-latest-file (path &optional filter)
  "Return the newest file in PATH. Optionally filter by FILTER."
	(if (listp path)
			(car
			 (sort (mapcar (lambda (dir) (my-latest-file dir)) path)
						 #'file-newer-than-file-p))
		(car
		 (sort (seq-remove #'file-directory-p
											 (directory-files path 'full filter t))
					 #'file-newer-than-file-p))))
;; Ledger:4 ends here

(provide 'my-elisp)
;;; my-elisp.el ends here
