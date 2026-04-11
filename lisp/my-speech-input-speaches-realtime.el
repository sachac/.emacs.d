(defvar my-speaches-process nil)
(defvar my-speaches-dir "~/vendor/speaches")
;;;###autoload
(defun my-speaches-start ()
  (interactive)
  (unless (process-live-p my-speaches-process)
    (let ((default-directory my-speaches-dir))
      (setq my-speaches-process
            (make-process
             :name "speaches-bridge"
             :buffer "*speaches-output*" ; Standard output buffer
             :command '("bash" "-c" "rec -q -t raw -r 16000 -c 1 -b 16 -e signed-integer - | uv run python3 stream.py")
             :filter #'my-speaches-filter
             :sentinel (lambda (proc event)
                         (when (memq (process-status proc) '(exit signal))
                           (message "Speaches process finished: %s" event)))
             :stderr "*speaches-stderr*" ; Separate buffer for Python errors/logs
             :noquery t)))
    (message "Speaches started.")))

;;;###autoload
(defun my-speaches-filter (proc string)
  "Accumulate STRING and call processor on complete JSON lines."
  (let ((moving-point (process-mark proc))
        results)
    (with-current-buffer (process-buffer proc)
      (save-excursion
        (goto-char moving-point)
        (insert string)
        (set-marker (process-mark proc) (point))
        (goto-char (point-min))
        (while (search-forward "\n" nil t)
          (let ((line (buffer-substring (point-min) (1- (point)))))
            (delete-region (point-min) (point))
            (unless (string-empty-p (string-trim line))
              (condition-case err
                  (let ((json-obj (json-parse-string line :object-type 'alist)))
                    (push json-obj results))
                (error (message "JSON parse error: %s" err))))))))
    (mapc #'my-speaches-process-logic
          (nreverse results))))

;;;###autoload
(defun my-speaches-process-logic (o)
  "Handle the parsed ALIST from the Speaches bridge."
  (let ((type (cdr (assoc 'type o))))
    (cond
     ((string= type "conversation.item.input_audio_transcription.completed")
      (let ((text (cdr (assoc 'transcript o))))
        (with-current-buffer (get-buffer-create "*speaches*")
          (goto-char (point-max))
          (unless (bolp) (insert "\n"))
          (insert text))))
     (t (prin1 o)))))
