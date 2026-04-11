
(declare-function 'subed-word-data-compare-normalized-string-distance "subed-word-data")

;;;###autoload
(defun my-whisper-maybe-expand-snippet (text)
  "Add to `whisper-insert-text-at-point'."
  (if (and text
           (string-match
            "^ok\\(?:ay\\)?[,\\.]? \\(.+\\)" text))
    (let* ((name
            (downcase
             (string-trim
              (replace-regexp-in-string "[,\\.]" "" (match-string 1 text)))))
           (matching
            (seq-find (lambda (o)
                        (subed-word-data-compare-normalized-string-distance
                         name
                         (downcase (yas--template-name o))))
                      (yas--all-templates (yas--get-snippet-tables)))))
      (if matching
          (progn
            (if (frame-focus-state)
                (progn
                  (yas-expand-snippet matching)
                  nil)
              ;; In another application
              (with-temp-buffer
                (yas-minor-mode)
                (yas-expand-snippet matching)
                (buffer-string))))
        text))
    text))
