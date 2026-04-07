;; [[file:../Sacha.org::#org-mode-org-babel-fix-find-function-when-i-ve-evaluated-something-from-org-babel][YE11: Fix find-function for Emacs Lisp from org-babel or scratch:6]]
(ert-deftest sacha-elisp--find-function-search-for-symbol--in-buffer ()
  (let ((sym (make-temp-name "--test-fn"))
        buffer)
    (unwind-protect
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert (format ";; Comment\n(defun %s () (message \"Hello\"))" sym))
          (eval-last-sexp nil)
          (setq buffer (current-buffer))
          (with-temp-buffer
            (let ((pos (sacha-elisp-find-function-search-for-symbol nil (intern sym) nil nil)))
              (should (equal (car pos) buffer))
              (should (equal (cdr pos) 12)))))
      (fmakunbound (intern sym)))))

(ert-deftest sacha-elisp--find-function-search-for-symbol--in-file ()
  (let* ((sym (make-temp-name "--test-fn"))
         (temp-file (make-temp-file
                     "test-" nil ".org"
                     (format
                      "#+begin_src emacs-lisp\n;; Comment\n(defun %s () (message \"Hello\"))\n#+end_src"
                      sym)))
         (sacha-elisp-find-function-search-extra (list temp-file))
         buffer)
    (unwind-protect
        (with-temp-buffer
          (let ((pos (sacha-elisp-find-function-search-for-symbol nil (intern sym) nil nil)))
            (should (equal (buffer-file-name (car pos)) temp-file))
            (should (equal (cdr pos) 35))))
      (delete-file temp-file))))
;; YE11: Fix find-function for Emacs Lisp from org-babel or scratch:6 ends here
