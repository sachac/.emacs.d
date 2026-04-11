;;;###autoload
(defun gptel-api-key-from-environment (&optional var)
  (lambda ()
    (getenv (or var                     ;provided key
                (thread-first           ;or fall back to <TYPE>_API_KEY
                  (type-of gptel-backend)
                  (symbol-name)
                  (substring 6)
                  (upcase)
                  (concat "_API_KEY"))))))

;;;###autoload
(defun my-gptel-set-model ()
  "Interactively set the gptel model."
  (interactive)
  (require 'gptel-transient)
  (let* ((infix (get 'gptel--infix-provider 'transient--suffix))
         (reader (oref infix reader))
         (result (funcall reader "Model: ")))
    (setq gptel-backend (car result))
    (setq gptel-model (cadr result))))

;;;###autoload
(defun my-agent-shell-dot-subdir (subdir)
  (let* ((cwd (string-remove-suffix "/" (agent-shell-cwd)))
         (sanitized (replace-regexp-in-string "/" "-" (string-remove-prefix "/" cwd))))
    (expand-file-name subdir (locate-user-emacs-file (concat "agent-shell/" sanitized)))))
