all: Sacha.el index.html index.pdf

Sacha.el: Sacha.org
	emacs -Q --batch --eval '(progn (load-file "batch-tangle.el") (org-babel-tangle-file "Sacha.org"))'

index.html: Sacha.el
	emacs -Q --batch --eval "(progn (load-file \"batch-tangle.el\") (load-file \"Sacha.el\") (find-file \"Sacha.org\") (org-export-to-file 'html \"index.html\"))"

index.pdf: Sacha.el
	emacs -Q --batch --eval "(progn (load-file \"batch-tangle.el\") (load-file \"Sacha.el\") (find-file \"Sacha.org\") (org-export-to-file 'latex \"index.tex\" nil nil nil nil nil #'org-latex-compile) nil)"
