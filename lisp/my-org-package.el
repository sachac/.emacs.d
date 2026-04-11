;;;###autoload
(defun my-org-package-open (package-name)
  (interactive "MPackage name: ")
  (describe-package (intern package-name)))

(ert-deftest my-org-package-export ()
  (should
   (string=
    (my-org-package-export "transcribe" "transcribe" 'html)
    "<a target=\"_blank\" href=\"https://elpa.gnu.org/packages/transcribe.html\">transcribe</a>"
    ))
  (should
   (string=
    (my-org-package-export "fireplace" "fireplace" 'html)
    "<a target=\"_blank\" href=\"http://melpa.org/#/fireplace\">fireplace</a>"
    )))
;;;###autoload
(defun my-org-package-export (link description format &optional arg)
  (let* ((package-info (car (assoc-default (intern link) package-archive-contents)))
         (package-source (and package-info (package-desc-archive package-info)))
         (path (format
                (cond
								 ((null package-source) link)
                 ((string= package-source "gnu") "https://elpa.gnu.org/packages/%s.html")
                 ((string= package-source "melpa") "https://melpa.org/#/%s")
                 ((string= package-source "nongnu") "https://elpa.nongnu.org/nongnu/%s.html")
                 (t (error 'unknown-source)))
                link))
         (desc (or description link)))
		(if package-source
				(cond
				 ((eq format '11ty) (format "<a target=\"_blank\" href=\"%s\">%s</a>" path desc))
				 ((eq format 'html) (format "<a target=\"_blank\" href=\"%s\">%s</a>" path desc))
				 ((eq format 'wp) (format "<a target=\"_blank\" href=\"%s\">%s</a>" path desc))
				 ((eq format 'latex) (format "\\href{%s}{%s}" path desc))
				 ((eq format 'texinfo) (format "@uref{%s,%s}" path desc))
				 ((eq format 'ascii) (format "%s <%s>" desc path))
				 ((eq format 'org) (org-link-make-string (concat "package:" link) description))
				 (t path))
			desc)))
;;;###autoload
(defun my-org-package-complete ()
	(require 'finder-inf nil t)
  (unless package--initialized
    (package-initialize t))
	(concat
	 "package:"
   ;; Load the package list if necessary (but don't activate them).
   (let ((packages (mapcar #'symbol-name (mapcar #'car package-archive-contents))))
		 (completing-read "Package: "
                      packages nil t nil nil))))

;;;###autoload
(defun my-org-package-link-description (link description)
	(unless description
		(when (string-match "package:\\(.+\\)" link)
			(match-string 1 link))))
