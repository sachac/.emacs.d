  (defvar my-google-doc-download-command
    (list "nodejs" (expand-file-name "~/bin/download-google-doc-html.cjs")))

;;;###autoload
  (defun my-google-doc-html (doc-id)
    (when (string-match "https://docs\\.google\\.com/document/d/\\(.+?\\)/" doc-id)
      (setq doc-id (match-string 1 doc-id)))
    (with-temp-buffer
      (apply #'call-process (car my-google-doc-download-command)
             nil t nil (append (cdr my-google-doc-download-command) (list doc-id)))
      (buffer-string)))

(require 'dom)
;;;###autoload
(defun my-google-doc-clean-html (html)
  "Remove links on spaces, replace Google links."
  (let ((dom (with-temp-buffer
               (insert html)
               (libxml-parse-html-region))))
    (dom-search
     dom
     (lambda (o)
       (when (eq (dom-tag o) 'a)
         (when (and (dom-attr o 'href)
                    (string-match "https://\\(www\\.\\)?google\\.com/url\\?q=" (dom-attr o 'href)))
           (let* ((parsed (url-path-and-query
                           (url-generic-parse-url (dom-attr o 'href))))
                  (params (url-parse-query-string (cdr parsed))))
             (dom-set-attribute o 'href (car (assoc-default "q" params #'string=)))))
         (let ((text (string= (string-trim (dom-text o)) "")))
           (when (string= text "")
             (setf (car o) 'span))))
       (when (and
              (string-match "font-weight:700" (or (dom-attr o 'style) ""))
              (not (string-match "font-style:normal" (or (dom-attr o 'style) ""))))
         (setf (car o) 'strong))
       (when (dom-attr o 'style)
         (dom-remove-attribute o 'style))))
    ;; bold text is actually represented as font-weight:700 instead
    (with-temp-buffer
      (svg-print dom)
      (buffer-string))))

;;;###autoload
  (defun my-google-doc-org (doc-id)
    "Return DOC-ID in Org Mode format."
    (pandoc-convert-stdio (my-google-doc-clean-html (my-google-doc-html doc-id)) "html" "org"))
