;;;###autoload
(defun my-org-sketch-open-text-file (sketch)
  (interactive (list (my-complete-sketch-filename)))
	(find-file (concat (file-name-sans-extension sketch) ".txt"))
	(with-current-buffer (find-file-noselect sketch)
		(display-buffer-in-side-window
		 (current-buffer)
		 '((window-width . 0.5)
			 (side . right)))))
