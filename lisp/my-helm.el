;;;###autoload
(defun my-helm-source-org-sketch-list ()
  (my-list-sketches "."))

;;;###autoload
(defun my-helm-org-insert-sketch-candidates (&optional candidates)
  (mapc (lambda (o)
          (org-insert-link nil (concat "sketch:" o))
          (insert "\n"))
        (helm-marked-candidates)))

;;;###autoload
(defun my-helm-open-sketches-in-krita (&optional candidates)
  (my-sketch-open-in-krita (helm-marked-candidates)))

;;;###autoload
(defun my-helm-open-sketches-in-gwenview (&optional candidates)
  (my-sketch-open-in-gwenview (helm-marked-candidates)))

;;;###autoload
(defun my-helm-open-sketches-in-feh (&optional candidates)
  (my-sketch-open-in-feh (helm-marked-candidates)))

(defvar my-helm-source-org-sketches
  '((name . "Sketches")
    (candidates . my-helm-source-org-sketch-list)
    (action . (("Insert" . my-helm-org-insert-sketch-candidates)
               ("Open in Krita" . my-helm-open-sketches-in-krita)
               ("Open in Gwenview" . my-helm-open-sketches-in-gwenview)
               ("Open as Feh slideshow" . my-helm-open-sketches-in-feh)))
    (persistent-action . my-helm-open-sketches-in-gwenview)))

;;;###autoload
(defun my-helm-org-sketches ()
  (interactive)
  (helm :sources '(my-helm-source-org-sketches)
        :buffer "*helm-org-sketches*"))

(defvar my-helm-org-list-candidates nil)
;;;###autoload
(defun my-helm-org-list-categories-init-candidates ()
  "Return a list of categories from this list in a form ready for Helm."
  (setq my-helm-org-list-candidates
        (mapcar (lambda (x)
                  (cons (if (elt x 3)
                            (format "%s - %s" (car x) (elt x 3))
                          (car x))
                        x))
                (my-org-get-list-categories))))
