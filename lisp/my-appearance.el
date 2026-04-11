;;;###autoload
  (defun sanityinc/adjust-opacity (frame incr)
    (let* ((oldalpha (or (frame-parameter frame 'alpha) 100))
           (newalpha (+ incr oldalpha)))
      (when (and (<= frame-alpha-lower-limit newalpha) (>= 100 newalpha))
        (modify-frame-parameters frame (list (cons 'alpha newalpha))))))

;;;###autoload
  (defun my-setup-color-theme ()
    (interactive)
    (when (display-graphic-p)
      (load-theme (car modus-themes-to-toggle) t)))

(defun my-hl-sexp-update-overlay ()
  (when (overlayp hl-sexp-overlay)
    (overlay-put
     hl-sexp-overlay
     'face
     `(:background
       ,(modus-themes-get-color-value 'bg-inactive)))))
(defun my-hl-sexp-update-all-overlays (&rest args)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when highlight-sexp-mode
        (my-hl-sexp-update-overlay)))))

;;;###autoload
  (defun my-update-active-mode-line-colors ()
          (set-face-attribute
           'mode-line nil
           :foreground (modus-themes-get-color-value 'fg-mode-line-active)
           :background (modus-themes-get-color-value 'bg-blue-subtle)))

;;;###autoload
  (defun my-add-face-text-property (start end attribute value)
          (interactive
           (let ((attribute (intern
                                                                                   (completing-read
                                                                                          "Attribute: "
                                                                                          (mapcar (lambda (o) (symbol-name (car o)))
                                                                                                                          face-attribute-name-alist)))))
                   (list (point)
                                           (mark)
                                           attribute
                                           (read-face-attribute '(()) attribute))))
          (add-face-text-property start end (list attribute value)))

;;;###autoload
  (defun my-face-text-larger (start end)
          (interactive "r")
          (add-face-text-property
           start end
           (list :height (floor (+ 50 (car (alist-get :height (get-text-property start 'face) '(100))))))))
;;;###autoload
  (defun my-face-text-smaller (start end)
          (interactive "r")
          (add-face-text-property
           start end
           (list :height (floor (- (car (alist-get :height (get-text-property start 'face) '(100))) 50)))))

(defun my-suggest-other-faces (func &rest args)
  "Disable `hl-line-mode' when choosing a face."
  (if hl-line-mode
      (progn
        (hl-line-mode -1)
        (prog1 (apply func args)
          (hl-line-mode 1)))
    (apply func args)))

;;;###autoload
(defun my-org-todo-set-keyword-faces ()
	(setq org-todo-keyword-faces
				`(("TODO" . (:foreground ,(modus-themes-get-color-value 'blue-warmer) :weight bold))
					("DONE" . (:foreground ,(modus-themes-get-color-value 'green-warmer) :weight bold))
					("WAITING" . (:foreground ,(modus-themes-get-color-value 'red-warmer) :weight bold))
					("SOMEDAY" . (:foreground ,(modus-themes-get-color-value 'fg-dim) :weight bold))))
	(when (derived-mode-p 'org-mode)
		(font-lock-fontify-buffer)))
