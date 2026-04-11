(defun sacha-org-hint-export (path desc format _)
	"Export hint."
	(pcase format
   ((or 'html '11ty 'md)
	  (format "<label class=\"hint\"><input type=\"checkbox\"> <span class=\"hint-desc\">%s</span><span class=\"hint-text\">%s</span></label>" desc path))
	('ascii
		desc)))

(defvar sacha-org-hint-functions nil
  "Functions to call with the hint as the argument.")

(defface sacha-org-hint-face
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for hints.")

(defun sacha-org-hint-open (path)
  "Display the hint at PATH."
  (let ((overlay (car (org-find-overlays 'sacha-org-hint)))
        (text (replace-regexp-in-string "^hint:" "" path))
        elem)
    (if overlay
        (delete-overlay overlay)
      (setq elem (org-element-context))
      (setq overlay (make-overlay (org-element-begin elem) (org-element-end elem)))
      (overlay-put overlay 'display text)
      (overlay-put overlay 'evaporate t)
      (overlay-put overlay 'sacha-org-hint text)
      (overlay-put overlay 'face 'sacha-org-hint-face)
      (run-hook-with-args 'sacha-org-hint-functions text))))

(defun sacha-org-hint-reset ()
  "Remove all hint overlays"
  (interactive)
  (remove-overlays (point-min) (point-max) 'sacha-org-hint))

(defun sacha-org-hint-play-sound (text)
  "Play sound for TEXT.
Match it against `sacha-org-hint-sound-alist'."
  (when-let* ((sound (assoc-default text sacha-org-hint-sound-alist #'string=)))
    (start-process "mpv" nil "mpv" (expand-file-name sound) "--no-video" "--force-window=no")))
