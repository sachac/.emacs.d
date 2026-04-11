(defvar hydra-stack nil)

(defun my-hydra-push (expr)
  (push `(lambda () ,expr) hydra-stack))

(defun my-hydra-pop ()
  (interactive)
  (let ((x (pop hydra-stack)))
    (when x (funcall x))))

(defun my-hydra-go-and-push (expr)
  (push hydra-curr-body-fn hydra-stack)
  (prin1 hydra-stack)
  (funcall expr))

;; example (progn (hydra-b/body) (hydra-push '(hydra-a/body)))
;; or   ("q" hydra-pop "exit")

(defun my-hydra-format-head (h)
  (let ((key-binding (elt h 0))
        (hint (elt h 2))
        (cmd (and (elt h 1) (prin1-to-string (elt h 1)))))
    (if cmd
        (format "%s (%s) - %s" hint key-binding cmd)
      (format "%s (%s)" hint key-binding))))

(defun my-hydra-heads-to-candidates (base)
  (mapcar (lambda (h)
            (cons (my-hydra-format-head h) (hydra--head-name h base)))
          (symbol-value (intern (concat (symbol-name base) "/heads")))))

;;;###autoload
(defun my-hydra-execute-extended (&optional _ hydra-base)
  (interactive (list current-prefix-arg nil))
  (hydra-keyboard-quit)
  (let* ((candidates (my-hydra-heads-to-candidates
                      (or hydra-base
                          (intern
                           (replace-regexp-in-string "/body$" ""
                                                     (symbol-name hydra-curr-body-fn))))))
         (command-name (completing-read "Cmd: " candidates))
         (bind (assoc-default command-name candidates 'string=)))
    (cond
     ((null bind) nil)
     ((hydra--callablep bind) (call-interactively bind)))))
