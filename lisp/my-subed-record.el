;;;###autoload
  (defun my-subed-record-normalize-current (file)
    (interactive (list (subed-media-file)))
    (let ((temp-file (make-temp-file file nil (concat "." (file-name-extension file)))))
      (make-process
       :name "normalize"
       :buffer (get-buffer-create "*normalize*")
       :command (list
                 (expand-file-name "~/bin/normalize")
                 (expand-file-name file)
                 temp-file)
       :sentinel
       (lambda (process event)
         (when (string-match "finished" event)
           (rename-file temp-file file t)
           (message "Normalized %s" file))))))

  (defvar-local my-subed-record-references nil)
;;;###autoload
  (defun my-subed-record-load-references (file &optional skip-insert)
    "Load the references from FILE (media)."
    (interactive (list (read-file-name "Media file: ")
                       current-prefix-arg))
    (dolist (cue (subed-parse-file (concat (file-name-sans-extension file) ".vtt")))
      (push
       (list
        (learn-lang-subed-record-simplify (elt cue 3))
        file
        (elt cue 1)
        (elt cue 2))
       my-subed-record-references))
    (unless skip-insert
      (my-subed-insert-references)))

;;;###autoload
  (defun my-subed-record-insert-reference ()
    (interactive)
    (when-let* ((rec (alist-get (learn-lang-subed-record-simplify (subed-subtitle-text))
                               my-subed-record-references
                               nil nil #'string=)))
      (subed-record-set-directive
       "#+REFERENCE"
       (format "%s %s --> %s"
               (elt rec 0)
               (subed-msecs-to-timestamp (elt rec 1))
               (subed-msecs-to-timestamp (elt rec 2))))))

;;;###autoload
  (defun my-subed-insert-references ()
    (interactive)
    (subed-for-each-subtitle (point-min) (point-max) t
      (my-subed-record-insert-reference)))
