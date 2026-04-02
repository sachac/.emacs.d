;;; my-sketch.el ---  -*- lexical-binding: t -*-

;; Author: Sacha Chua <sacha@sachachua.com>
;; URL: https://sachachua.com/dotemacs

;;; License:
;;
;; This file is not part of GNU Emacs.
;;
;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Related Emacs config sections:
;;
;; - Embark and images
;;   https://sachachua.com/dotemacs#embark-image
;;
;; - Completing sketches
;;   https://sachachua.com/dotemacs#completing-sketches
;;
;; - Breaking up a PDF from Supernote
;;   https://sachachua.com/dotemacs#multimedia-images-svg-animating-svgs-breaking-up-a-pdf-from-supernote
;;
;; - Finding sketches
;;   https://sachachua.com/dotemacs#finding-sketches
;;
;; - Renaming and recoloring sketches
;;   https://sachachua.com/dotemacs#sketch-rename-recolor
;;
;; - Button-based interface
;;   https://sachachua.com/dotemacs#button-based-interface
;;
;; - Templates
;;   https://sachachua.com/dotemacs#templates
;;
;; - Get information for sketched books
;;   https://sachachua.com/dotemacs#get-information-for-sketched-books
;;
;; - Make it easy to follow up on a sketch
;;   https://sachachua.com/dotemacs#make-it-easy-to-follow-up-on-a-sketch
;;
;; - Digital index piles with Emacs
;;   https://sachachua.com/dotemacs#digital-index-piles-with-emacs
;;
;; - Sketched books
;;   https://sachachua.com/dotemacs#insert-point
;;
;; - Other sketches
;;   https://sachachua.com/dotemacs#other-sketches
;;
;; - Other sketch-related functions
;;   https://sachachua.com/dotemacs#other-sketch-related-functions
;;
;; - Write about half-page scans
;;   https://sachachua.com/dotemacs#write-about-half-page-scans
;;
;; - Supernote
;;   https://sachachua.com/dotemacs#supernote
;;
;; - org-attaching the latest image from my Supernote via Browse and Access
;;   https://sachachua.com/dotemacs#supernote-browse
;;
;;; Code:



;; [[file:../Sacha.org::#embark-image][Embark and images:1]]
;;;###autoload
(defun my-sketch-insert-file-as-link (f)
  (interactive (list (my-complete-sketch-filename)))
  (cond
   ((derived-mode-p 'org-mode)
    (insert (org-link-make-string (concat "sketch:" (file-name-nondirectory f))) "\n"))
   ((or (derived-mode-p 'html-mode) (derived-mode-p 'web-mode))
    (insert "{% sketchFull \"" (file-name-base f) "\" %}"))
   (t (insert f))))
;; Embark and images:1 ends here

;; [[file:../Sacha.org::my-image--state][my-image--state]]
(declare-function 'my-geeqie-view "Sacha.el")

(defvar my-sketch-preview 'text
  "*Preview sketches.
  'text means show the associated text.
  'geeqie means open image in Geeqie.
  t means open image in Emacs.")

(defun my-image--state ()
  "Manage preview window and cleanup."
  ;; These functions are closures captured when the state is initialized by consult--read
  (let ((preview (consult--buffer-preview))
        (open (consult--temporary-files)))
    ;; The returned lambda is the actual preview function called by Consult
    (lambda (action cand)
      (unless cand
        (funcall open))
      (when my-sketch-preview
        (let ((filename (cond
                         ((and (eq my-sketch-preview 'text)
                               (listp cand)
                               (alist-get 'source_path cand))
                          (alist-get 'source_path cand))
                         ((and (listp cand)
                               (alist-get 'source_path cand))
                          (my-image-filename (file-name-base (alist-get 'source_path cand))))
                         (t cand))))
          (when filename
            (pcase my-sketch-preview
              ('geeqie (my-geeqie-view (list filename)))
              (_ (funcall preview action
                          (and cand
                               (eq action 'preview)
                               (funcall open filename)))))))))))
;; my-image--state ends here

;; [[file:../Sacha.org::#completing-sketches][Completing sketches:2]]
;;;###autoload
(defun my-complete-sketch-filename (&optional filter)
  (interactive)
  (consult--read (my-sketches filter)
                 :sort nil
                 :state (my-image--state)
                 :prompt "Sketch: "
                 :category 'sketch))

(defun my-date-from-filename (filename)
  (let ((f (file-name-nondirectory filename)))
    (if (string-match "^[-0-9]+" f)
        (replace-regexp-in-string "[^0-9]" "" (match-string 0 f))
      nil)))

(defvar my-sketches nil "Cache for sketch filenames.")

;;;###autoload
(defun my-sketches (&optional filter)
  (interactive)
  (let (results)
    (setq results
          (apply 'append (mapcar (lambda (dir)
                                   (directory-files dir t "\\.\\(jpe?g\\|png\\|svg\\)$"))
                                 my-sketch-directories)))
    (when filter
      (setq results (seq-filter (lambda (o) (string-match filter o))
                                results)))
    (sort
     results
     (lambda (a b)
       (string< (concat (or (my-date-from-filename b) "0") (file-name-nondirectory b))
                (concat (or (my-date-from-filename a) "0") (file-name-nondirectory a)))))))

;;;###autoload
(defun my-find-sketch (file)
  (interactive (list (my-complete-sketch-filename)))
  (find-file file))

;;;###autoload
(defun my-sketch-prepare-post (file)
  (interactive (list (my-complete-sketch-filename)))
  (insert (org-link-make-string (concat "sketchFull:" (file-name-base file))))
  (let ((text (my-sketch-text file)))
    (when text
      (insert (format "\n\n#+begin_my_src \"Text from %s\"\n%s\n#")))))

(defun my-sketch-text (file)
  (cond
   ((file-exists-p (concat (file-name-sans-extension file) ".txt"))
    (with-temp-buffer
      (insert-file-contents (concat (file-name-sans-extension file) ".txt"))
      (buffer-string)))
   ((file-exists-p (concat (file-name-sans-extension file) ".json"))
    (let ((json-object-type 'alist))
      (assoc-default 'description (elt (assoc-default 'textAnnotations (json-read-file (concat (file-name-sans-extension file) ".json"))) 0))))
   (t (error "Can't find text."))))

;;;###autoload
(defun my-sketch-insert-text (file)
  (interactive "FFile: ")
  (let ((text (my-sketch-text file)))
    (insert (or text ""))))
;; Completing sketches:2 ends here

;; [[file:../Sacha.org::#multimedia-images-svg-animating-svgs-breaking-up-a-pdf-from-supernote][Breaking up a PDF from Supernote:1]]
(defvar my-debug-buffer (get-buffer-create "*temp*"))
;;;###autoload
(defun my-sketch-convert-pdf (pdf-file)
	"Returns the SVG filename."
	(interactive "FPDF: ")
	(if-let ((links (and (file-exists-p (concat (file-name-sans-extension pdf-file) ".svg"))
											 (dom-by-tag
												(car (xml-parse-file (concat (file-name-sans-extension pdf-file) ".svg")))
												'a))))
			;; copy links over
			(let ((temp-file (concat (make-temp-name "svg-conversion") ".svg"))
						new-file)
				(unwind-protect
						(progn
							(call-process "pdftocairo" nil my-debug-buffer nil "-svg" (expand-file-name pdf-file)
														temp-file)
							(setq new-file (car (xml-parse-file temp-file)))
							(dolist (link links)
								(dom-append-child new-file link))
							(with-temp-file (file-exists-p (concat (file-name-sans-extension pdf-file) ".svg"))
								(svg-print new-file)))
					(error
					 (delete-file temp-file))))
		(delete-file (concat (file-name-sans-extension pdf-file) ".svg"))
		(call-process "pdftocairo" nil my-debug-buffer nil "-svg" (expand-file-name pdf-file)
									(expand-file-name (concat (file-name-sans-extension pdf-file) ".svg"))))
	(concat (file-name-sans-extension pdf-file) ".svg"))

;;;###autoload
(defun my-sketch-change-fill-to-style (dom)
	"Inkscape handles these better when we split paths."
	(dolist (path (dom-by-tag dom 'path))
		(when (dom-attr path 'fill)
			(dom-set-attribute
			 path 'style
			 (if (dom-attr path 'style)
					 (concat (dom-attr path 'style) ";fill:" (dom-attr path 'fill))
				 (concat "fill:" (dom-attr path 'fill))))
			(dom-remove-attribute path 'fill)))
	dom)

;;;###autoload
(defun my-sketch-recolor (dom color-map &optional selector)
	"Colors are specified as ((\"#input\" . \"#output\") ...)."
	(if (symbolp color-map)
			(setq color-map
 						(assoc-default color-map my-sketch-color-map)))
	(let ((map-re (regexp-opt (mapcar 'car color-map))))
		(dolist (path (if selector (dom-search dom selector)
										(dom-by-tag dom 'path)))
			(dolist (attr '(style fill))
				(when (and (dom-attr path attr)
									 (string-match map-re (dom-attr path attr)))
					(dom-set-attribute
					 path attr
					 (replace-regexp-in-string
						map-re
						(lambda (match)
							(assoc-default match color-map))
						(or (dom-attr path attr) "")))))))
	dom)

;;;###autoload
(defun my-sketch-add-bg (dom)
	;; add background rectangle
	(unless (dom-search dom (lambda (elem) (and (dom-attr elem 'class) (string-match "\\<background\\>" (dom-attr elem 'class)))))
		(let* ((view-box (mapcar 'string-to-number (split-string (dom-attr dom 'viewBox))))
					 (bg-node (dom-node 'rect `((x . 0)
																			(y . 0)
																			(class . "background")
																			(width . ,(elt view-box 2))
																			(height . ,(elt view-box 3))
																			(fill . "#ffffff")))))
			(if (dom-by-id dom "surface1")
					(push bg-node (cddr (car (dom-by-id dom "surface1"))))
				(push bg-node (cddr (car dom))))))
	dom)

;;;###autoload
(defun my-sketch-clean (dom)
	"Remove USE and IMAGE tags."
	(dolist (use (dom-by-tag dom 'use))
		(dom-remove-node dom use))
	(dolist (use (dom-by-tag dom 'image))
		(dom-remove-node dom use))
	dom)

;;;###autoload
(defun my-sketch-rotate (dom)
	(let* ((old-width (dom-attr dom 'width))
				 (old-height (dom-attr dom 'height))
				 (view-box (mapcar 'string-to-number (split-string (dom-attr dom 'viewBox))))
				 (rotate (format "rotate(90) translate(0 %s)" (- (elt view-box 3)))))
		(dom-set-attribute dom 'width old-height)
		(dom-set-attribute dom 'height old-width)
		(dom-set-attribute dom 'viewBox (format "0 0 %d %d" (elt view-box 3) (elt view-box 2)))
		(dolist (g (dom-by-tag dom 'g))
			(dom-set-attribute g 'transform rotate)))
	dom)

;;;###autoload
(defun my-sketch-mix-blend-mode-darken (dom &optional selector)
	(dolist (p (if (functionp selector) (dom-search dom selector) (or selector (dom-by-tag dom 'path))))
		(when (and (dom-attr p 'style)
							 (not (string-match "mix-blend-mode" (dom-attr p 'style))))
			(dom-set-attribute
			 p 'style
			 (replace-regexp-in-string ";;\\|^;" ""
																 (concat
																	(or (dom-attr p 'style) "")
																	";mix-blend-mode:darken")))))
	dom)

;;;###autoload
(defun my-sketch-color-to-hex (dom &optional selector)
	(dolist (p (if (functionp selector) (dom-search dom selector)
							 (or selector (dom-search dom
																				(lambda (p) (or (dom-attr p 'style)
																												(dom-attr p 'fill)))))))
		(dolist (attr '(style fill))
			(when (dom-attr p attr)
				(dom-set-attribute
				 p attr
				 (replace-regexp-in-string
					"rgb(\\([0-9\\.]+\\)%, *\\([0-9\\.%]+\\)%, *\\([0-9\\.]+\\)%)"
					(lambda (s)
						(color-rgb-to-hex
						 (* 0.01 (string-to-number (match-string 1 s)))
						 (* 0.01 (string-to-number (match-string 2 s)))
						 (* 0.01 (string-to-number (match-string 3 s)))
						 2))
					(dom-attr p attr))))))
	dom)

;; default for now, but will support more colour schemes someday
(defvar my-sketch-color-map
	'((blue
		 ("#9d9d9d" . "#2b64a9")
		 ("#9c9c9c" . "#2b64a9")
		 ("#c9c9c9" . "#b3e3f1")
		 ("#c8c8c8" . "#b3e3f1")
		 ("#cacaca" . "#b3e3f1")
		 ("#a6d2ff" . "#ffffff"))
		(t
		 ("#9d9d9d" . "#888888")
		 ("#9c9c9c" . "#888888")
		 ("#cacaca" . "#f6f396")
		 ("#c8c8c8" . "#f6f396")
		 ("#a6d2ff" . "#ffffff")
		 ("#c9c9c9" . "#f6f396"))))

(cl-defun my-sketch-svg-prepare (file &key color-map color-scheme new-file)
	"Clean up SVG for publishing."
	(when (string= (file-name-extension file) "pdf")
		(setq file (my-sketch-convert-pdf file)))
	(let ((dom (xml-parse-file file)))
		(setq dom (my-sketch-clean dom))
		(setq dom (my-sketch-color-to-hex dom))
		(setq dom (my-sketch-add-bg dom))
		(setq dom (my-sketch-change-fill-to-style dom))
		(setq dom (my-sketch-recolor dom
																 (or color-map
																		 color-scheme
																		 t)))
		(with-temp-file (or new-file file) (svg-print (car dom)))
		(or new-file file)))
;; Breaking up a PDF from Supernote:1 ends here

;; [[file:../Sacha.org::#finding-sketches][Finding sketches:1]]
(defvar my-sketch-directories
  '("~/sync/sketches"
    "~/sync/private-sketches"))

;;;###autoload
(defun my-get-sketch-filenames-between-dates (start end &optional filter)
  "Returns index card filenames between START and END."
  (setq start (replace-regexp-in-string "[^0-9]" "" start))
  (setq end (replace-regexp-in-string "[^0-9]" "" end))
  (my-get-sketch-filenames
   (lambda (filename)
     (let ((f (replace-regexp-in-string "[^0-9]" "" (file-name-nondirectory filename))))
       (and (string> f start)
            (string> end f)
            (or (not filter) (string-match filter filename)))))))

;;;###autoload
(defun my-get-sketch-filenames (base &optional as-regexp)
  (my-get-image-filenames base as-regexp my-sketch-directories))
;;;###autoload
(defun my-get-image-filenames (base &optional as-regexp directories)
  "Check several directories for files matching BASE.
           Return the matching filenames, if any.
           If AS-REGEXP is non-nil, treat BASE as a regular expression.
           If BASE is a function, use that to filter."
	(when (and (stringp base) (string-match "^[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]-[0-9][0-9]" base))
		(setq base (match-string 0 base)))
  (let ((base-regexp (unless (functionp base)
                       (concat
                        "\\("
                        (if as-regexp base (regexp-quote base))
                        "\\)"
                        ".*\\(\\.\\(png\\|psd\\|tiff\\|jpe?g\\|svg\\)\\)$"))))
    (-filter
     (lambda (o) (not (string-match "\\.xmp" o)))
     (sort (-flatten
            (delq nil
                  (mapcar
                   (lambda (dir)
                     (and (file-directory-p dir)
                          (if (functionp base)
                              (-filter base (directory-files dir t ".*\\.\\(png\\|psd\\|tiff\\|jpe?g\\|svg\\)?$"))
                            (directory-files
                             dir t
                             base-regexp))))
                   (or directories my-image-directories))))
           'string<))))

;;;###autoload
(defun my-image-filename (base &optional as-regexp directories)
  "Check several directories for files matching BASE.
Return the first matching filename, if any.
If AS-REGEXP is non-nil, treat BASE as a regular expression."
  (when (and (listp base) (alist-get 'source_path base))
    (setq base (file-name-base (alist-get 'source_path base))))
  (if (file-exists-p base)
      base
    (car (my-get-image-filenames base as-regexp directories))))
;;;###autoload
(defun my-sketch-filename (base &optional as-regexp)
  (my-image-filename base as-regexp my-sketch-directories))
(defalias 'my-get-image-filename 'my-image-filename)
(defalias 'my-get-sketch-filename 'my-sketch-filename)

;;;###autoload
(defun my-list-sketches (regexp &optional full-filename directories)
  "Return a list of sketch filenames matching REGEXP."
  (interactive (list (read-string "Filter: ")))
  (let ((my-sketch-directories (or directories my-sketch-directories)))
    (funcall (if (called-interactively-p 'interactive)
                 (lambda (x) (insert (mapconcat (lambda (y) (concat "- " (org-link-make-string (concat "sketchLink:" y)))) x "\n"))) 'identity)
             (sort (-uniq
                    (mapcar (if full-filename 'identity
                              'file-name-nondirectory)
                            (my-get-sketch-filenames regexp t)))
                   'string>))))
;; Finding sketches:1 ends here

;; [[file:../Sacha.org::#sketch-rename-recolor][Renaming and recoloring sketches:1]]
;;;###autoload
(defun my-sketch-rename (file)
	(interactive "FFile: ")
	(my-image-rename-set
	 file
	 (my-image-recognize-get-new-filename file)))

;;;###autoload
(defun my-sketch-recolor-png (file &optional color-scheme)
	(interactive (list (read-file-name "File: ")
										 (completing-read "Scheme: " (mapcar (lambda (o) (symbol-name (car o)))
																												 my-sketch-color-map))))
	(setq color-scheme (or color-scheme 't))
	(call-process "/home/sacha/bin/recolor.py" nil nil nil
								"--colors"
								(mapconcat
								 (lambda (row)
									 (concat (car row) "," (cdr row)))
								 (assoc-default (if (stringp color-scheme)
																		(intern color-scheme)
																	color-scheme)
																my-sketch-color-map)
								 ",")
								(expand-file-name file))
	file)
(defalias 'my-image-recolor 'my-sketch-recolor-png)
;; Renaming and recoloring sketches:1 ends here

;; [[file:../Sacha.org::#button-based-interface][Button-based interface:1]]
;;;###autoload
(defun my-set-up-sketch-buffer ()
  "Populate a widget buffer with a few handy buttons."
  (interactive)
  (with-current-buffer (get-buffer-create "*Done*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (widget-create 'push-button
                     :notify (lambda (&rest ignore)
                               (my-org-clock-in-and-track-by-name "Draw"))
                     "Track: Draw")
      (widget-create 'push-button
                     :notify (lambda (&rest ignore)
                               (my-org-clock-in-and-track-by-name "Draw journal entries"))
                     "Track: Journal")
      (widget-create 'push-button
                     :notify (lambda (&rest ignore)
                               (my-org-sketch-open (my-prepare-index-card-template)))
                     "New")
      (widget-create 'push-button
                     :notify (lambda (&rest ignore)
                               (my-org-sketch-open (my-prepare-large-template)))
                     "New large")
      (widget-create 'push-button
                     :notify (lambda (&rest ignore)
                               (my-org-sketch-open (my-prepare-index-card-template nil (org-read-date))))
                     "Date")
      (widget-create 'push-button
                     :notify (lambda (&rest ignore) (shell-command "~/bin/rotate-screen")) "Rotate")
      (insert "\n")
      (widget-create 'push-button
                     :notify (lambda (&rest ignore)
                               (shell-command "~/bin/add-output-png"))
                     "Add output.png")
      (widget-create 'push-button
                     :notify (lambda (&rest ignore)
                               (my-rotate-screen 0)
                               (kill-buffer)
                               (my-rename-scanned-cards))
                     "Process")
      (widget-create 'push-button
                     :notify (lambda (&rest ignore)
                               (my-rotate-screen 0)
                               (delete-window)
                               (my-rename-scanned-cards))
                     "Rename")
      (widget-create 'push-button
                     :notify (lambda (&rest ignore)
                               (my-rotate-screen 0)
                               (delete-window)
                               (my-convert-and-upload-cards))
                     "Upload")
      (widget-create 'push-button
                     :notify (lambda (&rest ignore)
                               (my-rotate-screen 0)
                               (org-clock-out)
                               (kill-buffer))
                     "Quit")
      (text-scale-set 10)
      (widget-setup)
      (widget-minor-mode)
      (pop-to-buffer (current-buffer))
      (goto-char (point-min))
      (current-buffer))))

;;;###autoload
(defun my-prepare-index-cards (n)
  (interactive (list (or current-prefix-arg 5)))
  (let ((counter 1)
        (directory "~/Dropbox/Inbox")
        (template my-index-card-template-file)
        (date (substring (org-read-date nil nil ".") 0 10))
        temp-file)
    (quantified-track "Drawing")
    (dotimes (i 5) (my-org-sketch-open (my-prepare-index-card-template)))
    (my-rotate-screen 180)
    (my-set-up-sketch-buffer)))

(defvar my-index-card-file-name nil "Most recent index card file name.")
;;;###autoload
(defun my-rotate-screen (degrees)
  (cond
   ((eq system-type 'windows-nt)
    (shell-command (format "c:/sacha/Dropbox/bin/orient /rotate:%d" degrees)))
   ((eq system-type 'gnu/linux)
    (shell-command (format "~/bin/rotate-screen %s"
                           (cond
                            ((= degrees 0) "normal")
                            ((= degrees 180) "inverted")
                            ((= degrees 90) "left")
                            ((= degrees 270) "right")))))))
;; Button-based interface:1 ends here

;; [[file:../Sacha.org::#templates][Templates:1]]
;;;###autoload
(defun my-prepare-drawing-template (&optional name date template)
  "Create the image file for NAME. Return the new filename."
  (let* ((date (or date (substring (org-read-date nil nil ".") 0 10)))
         (data (my-journal-post (or name "sketch") :Date date)))
    (setq name (expand-file-name
                (concat (assoc-default 'ZIDString data)
                        (if name
                            (concat " "
                                    (my-convert-sketch-title-to-filename (or name "")))

                              "")
                            "." (file-name-extension template))
                    "~/Dropbox/Inbox"))
    (copy-file (or template my-index-card-template-file) name)
    name))

;;;###autoload
(defun my-org-insert-new-index-card-link ()
  (interactive)
  (let ((filename
         (my-prepare-index-card-template)))
    (insert "[[sketch:" filename "]]\n")
    (save-window-excursion
      (my-rotate-screen 180)
      (shell-command
       (concat (shell-quote-argument my-sketch-executable)
               " " (shell-quote-argument filename) " &")))))

;;;###autoload
(defun my-prepare-index-card-template (&optional name date)
  "Create the image file for NAME. Return the new filename."
  (my-prepare-drawing-template name date my-index-card-template-file))

;;;###autoload
(defun my-prepare-large-template (&optional name date)
  "Create the image file for NAME. Return the new filename."
  (my-prepare-drawing-template name date my-sketch-large-template-file))


;;;###autoload
(defun my-prepare-index-card (&optional name date)
  "Prepare the index card for NAME.
              Rotate the screen and show a button to un-rotate the screen."
  (interactive (list (read-string "Name: ")
                     (substring (if current-prefix-arg (org-read-date) (org-read-date nil nil ".")) 0 10)))
  (setq my-index-card-file-name (my-prepare-index-card-template name date))
  (save-window-excursion
    (my-rotate-screen 180)
    (shell-command
     (concat (shell-quote-argument my-sketch-executable)
             " " (shell-quote-argument my-index-card-file-name) " &")))
  (my-set-up-sketch-buffer))

;;;###autoload
(defun my-prepare-index-card-for-subtree ()
  "Create an index card template for the current subtree."
  (interactive)
  (let* ((heading (elt (org-heading-components) 4)))
    (unless (org-entry-get (point) "Effort") (org-set-property "Effort" "0:15"))
    (if (derived-mode-p 'org-agenda-mode) (org-agenda-clock-in) (org-clock-in))
    (my-org-quantified-track "Drawing")
    (if (org-at-heading-p) (forward-line 1))
    (my-prepare-index-card heading)))

;;;###autoload
(defun my-helm-org-prepare-index-card-for-subtree (candidate)
  (let ((location (org-refile--get-location candidate my-helm-org-refile-locations)))
    (save-window-excursion
      (save-excursion
        (org-refile 4 nil location)
        (my-prepare-index-card-for-subtree)) t)))
;; Templates:1 ends here

;; [[file:../Sacha.org::#get-information-for-sketched-books][Get information for sketched books:1]]
;;;###autoload
(defun my-prepare-sketchnote-file ()
  (interactive)
  (let* ((base-name (org-entry-get-with-inheritance  "BASENAME")))
    (unless base-name (error "Missing basename property"))
    (my-org-sketch-open (my-prepare-large-template base-name))))
;; Get information for sketched books:1 ends here

;; [[file:../Sacha.org::#make-it-easy-to-follow-up-on-a-sketch][Make it easy to follow up on a sketch:1]]
;;;###autoload
(defun my-follow-up-on-sketch (filename)
  "Prompt for FILENAME to follow up on.
      Create an index card with it as a layer, and add the ref to the filename."
  (interactive (list (helm-read-file-name "Image: " :initial-input "~/sketches/")))
  ;; Allow the specification of a short identifier
  (unless (file-exists-p filename)
    (setq filename (car (directory-files "~/sketches" t (concat "^" filename)))))
  (let ((async-shell-command-buffer 'new-buffer)
        (index-card (my-prepare-index-card-template
                     (format "-- index card ref %s"
                             (and (string-match "^[^ \\.]+" (file-name-nondirectory filename))
                                  (match-string 0 (file-name-nondirectory filename)))))))
    (shell-command (format "convert %s %s -colorspace cmyk %s"
                           (shell-quote-argument (expand-file-name my-index-card-template-file))
                           (shell-quote-argument (expand-file-name filename))
                           (shell-quote-argument (expand-file-name index-card))))
    (shell-command (format "%s %s &"
                           (shell-quote-argument my-sketch-executable)
                           (shell-quote-argument (expand-file-name index-card))))
    (my-rotate-screen 180)
    (my-set-up-sketch-buffer)))
;; Make it easy to follow up on a sketch:1 ends here

;; [[file:../Sacha.org::#digital-index-piles-with-emacs][Digital index piles with Emacs:6]]
;;;###autoload
(defun my-refile-sketches-to-questions ()
  (interactive)
  (while (looking-at "^  \\+ \\[\\[.*?\\]\\[\\(.*?\\) -- \\(.*?\\)\\]\\]\n")
    (let ((link (match-string 0))
          (title (match-string 1)))
      (save-excursion
        (if (save-match-data (search-forward (concat "* " title) nil t))
            (progn (forward-line) (insert (match-string 0)) (replace-match ""))
          (forward-line 1))))))
;; Digital index piles with Emacs:6 ends here

;; [[file:../Sacha.org::#insert-point][Sketched books:2]]
;;;###autoload
(defun my-convert-sketch-title-to-filename (text)
  (setq text (replace-regexp-in-string "[?!]$" "" text))
  (setq text (replace-regexp-in-string "[?!:] " " - " text)))
(ert-deftest my-convert-sketch-title-to-filename ()
  (should (string= (my-convert-sketch-title-to-filename "Test") "Test"))
  (should (string= (my-convert-sketch-title-to-filename "Another Test!") "Another Test"))
  (should (string= (my-convert-sketch-title-to-filename "Does this work? Yes") "Does this work - Yes"))
  (should (string= (my-convert-sketch-title-to-filename "Title: Subtitle") "Title - Subtitle"))
  )

;;;###autoload
(defun my-convert-sketched-book-to-png ()
  "Convert TIFF to PNG."
  (interactive)
  (let ((basename (org-entry-get-with-inheritance "BASENAME")))
    (shell-command (format "convert \"c:/sacha/dropbox/inbox/%s.tif\" \"c:/sacha/dropbox/inbox/%s.png\""
                           basename
                           basename))))

;;;###autoload
(defun my-index-sketched-book ()
  "Add entries to sketched books index."
  (interactive)
  (let* ((title (org-entry-get-with-inheritance "SHORT_TITLE"))
         (author (org-entry-get-with-inheritance "AUTHOR"))
         (basename (org-entry-get-with-inheritance "BASENAME"))
         (base-file (format "~/Dropbox/Inbox/%s.png" basename)))
    (when (file-exists-p base-file)
      (copy-file base-file
                 (format "~/Dropbox/Packaging/sketched-books/%s.png" basename) t t))
    (find-file "~/Dropbox/Packaging/sketched-books/index.org")
    (vc-git-register (list (format "%s.png" basename)))
    (goto-char (point-min))
    (re-search-forward "<<insert-point>>")
    (insert (format "\n- [[file:%s.png][%s - %s (sketched %s)]]\n  [[file:%s.png]]\n\n"
                    basename
                    title
                    author
                    (substring basename 0 10)
                    basename))
    (find-file "~/Dropbox/Packaging/sketched-books/ebook.org")
    (goto-char (point-min))
    (re-search-forward "<<insert-point>>")
    (insert (format "\n* %s - %s (sketched %s)\n\n[[file:%s.png]]\n\n"
                    title
                    author
                    (substring basename 0 10)
                    basename))))

;;;###autoload
(defun my-package-sketched-book ()
  "Add the latest sketch and package the collection."
  (interactive)
  (shell-command
   (format "plink -A vagrant@127.0.0.1 -P 2222 \"cd ~/Dropbox/Packaging/sketched-books; git add '%s.png'; git commit -m 'Added %s - %s' -a; git push; make all\" &"
           (org-entry-get-with-inheritance "BASENAME")
           (org-entry-get-with-inheritance "SHORT_TITLE")
           (org-entry-get-with-inheritance "AUTHOR"))))
;; Sketched books:2 ends here

;; [[file:../Sacha.org::#other-sketches][Other sketches:1]]
;;;###autoload
(defun my-get-tile-dimensions (num-items orig-width orig-height target-aspect-ratio)
  (let ((rows 1) (cols 1)
        (current-aspect (/ orig-width (float orig-height)))
        add-col-aspect
        add-row-aspect)
    (while (< (* rows cols) num-items)
      (setq add-col-aspect (/ (* (1+ cols) (float orig-width))
                              (* rows orig-height))
            add-row-aspect (/ (* cols (float orig-width))
                              (* (1+ rows) orig-height)))
      (if (<  (abs (- add-col-aspect target-aspect-ratio))
              (abs (- add-row-aspect target-aspect-ratio)))
          (setq cols (1+ cols))
        (setq rows (1+ rows))))
    (cons cols rows)))
(ert-deftest my-get-tile-dimensions ()
  (should (equal (my-get-tile-dimensions 2 2 1 1) (cons 1 2)))
  (should (equal (my-get-tile-dimensions 4 2 1 0.5) (cons 1 4)))
  (should (equal (my-get-tile-dimensions 12 1 1 (/ 4.0 3.0)) (cons 4 3)))
  (should (equal (my-get-tile-dimensions 11 1 1 (/ 4.0 3.0)) (cons 4 3)))
  (should (equal (my-get-tile-dimensions 13 1 1 (/ 4.0 3.0)) (cons 4 4))))

;;;###autoload
(defun my-extract-image-filenames (beg end)
  "Return the filenames from the links in this region."
  (let (files)
    (save-excursion
      (goto-char (min beg end))
      (while (re-search-forward "sketch:" (max beg end) t)
        (let ((link (org-element-context)))
          (add-to-list 'files (org-element-property :path link))))
      files)))

;;;###autoload
(defun my-create-sketch-montage (files &optional tiles)
  "Combine the sketches in the region."
  (interactive
   (list
    (if (derived-mode-p 'dired-mode)
        (dired-get-marked-files)
      (mapcar 'my-get-sketch-filename
              (my-extract-image-filenames (min (point) (mark)) (max (point) (mark)))))
    (if current-prefix-arg (read-string "Tiling: "))))
  ;; Extract the links
  (let ((output-file "~/Dropbox/Inbox/output.png"))
    (unless tiles
      (setq tiles
            (format "%dx"
                    (car (my-get-tile-dimensions (length files) 1500 900 (/ 4.0 3))))))
    (with-temp-buffer
      (cd "~/Dropbox/Inbox/To blog")
      (apply 'call-process
             "montage" nil nil nil
             (append
              files
              (list
               "-geometry" "1500x900>+0+0"
               "-tile" tiles
               (expand-file-name output-file)))))
    (if (called-interactively-p 'any) (find-file output-file))))

;;;###autoload
(defun my-create-week-montage (beg end)
  (interactive "r")
  (let* ((date (org-read-date nil nil (unless current-prefix-arg "-fri")))
         (filename (format "Week ending %s #journal #weekly" date))
         (full-filename (my-get-sketch-filename filename)))
    (if full-filename
        (my-org-sketch-open full-filename)
      (my-create-index-card-montage
       (mapcar 'my-get-sketch-filename
               (my-extract-image-filenames (min (point) (mark)) (max (point) (mark))))
       "2x"
       (my-prepare-index-card-template filename)))))

;;;###autoload
(defun my-create-index-card-montage (files &optional tiling filename)
  "Prepare an index card with a montage of the selected sketches as a layer."
  (interactive
   (list
    (if (derived-mode-p 'dired-mode)
        (dired-get-marked-files)
      (mapcar 'my-get-sketch-filename
              (my-extract-image-filenames (min (point) (mark)) (max (point) (mark)))))))
  (let ((async-shell-command-buffer 'new-buffer)
        (index-card (or filename (my-prepare-index-card-template))))
    (my-create-sketch-montage files tiling)
    (shell-command
     (format "convert %s \\( %s -resize 1500x900 \\) -colorspace cmyk %s"
             (shell-quote-argument (expand-file-name my-index-card-template-file))
             (shell-quote-argument (expand-file-name "~/Dropbox/Inbox/output.png"))
             (shell-quote-argument (expand-file-name index-card))))
    (shell-command (format "%s %s &"
                           (shell-quote-argument my-sketch-executable)
                           (shell-quote-argument (expand-file-name index-card))))
    (my-rotate-screen 180)
    (my-set-up-sketch-buffer)))
;; Other sketches:1 ends here

;; [[file:../Sacha.org::#other-sketch-related-functions][Other sketch-related functions:1]]
;;;###autoload
(defun my-show-sketches-as-slideshow (list &optional shuffle)
  "Display a quick slideshow of sketches in LIST.
          If LIST is a string, look up those sketch filenames in my Flickr copy."
  (interactive "MFilter: \nP")
  (apply 'call-process "feh" nil nil nil "-D" "1" "-F" (if shuffle "-z" """")
         (-filter (lambda (x) (string-match "photostream" x))
                  (if (stringp list)
                      (my-list-sketches list t)
                    list))))

(defvar my-org-index-card-source nil)
;;;###autoload
(defun my-org-prompt-index-cards ()
  "Display a buffer for easy selection of questions to work on."
  (interactive)
  (find-file "~/personal/questions.org")
  (let ((questions
         (cl-sort (org-map-entries 'org-heading-components "TODO=\"DRAW\"")
                  '< :key (lambda (x) (or (elt x 3) 100)))))
    (setq my-org-index-card-source (current-buffer))
    (my-rotate-screen 180)
    (my-set-up-sketch-buffer)
    (mapc (lambda (q)
            (widget-create 'push-button
                           :notify (lambda (widget &rest ignore)
                                     (my-org-sketch-open
                                      (my-prepare-index-card-template
                                       (widget-value widget)))
                                     (with-current-buffer my-org-index-card-source
                                       (save-excursion
                                         (goto-char (org-find-exact-headline-in-buffer (widget-value widget) my-org-index-card-source t))
                                         (org-set-property "Effort" "0:15")
                                         (org-clock-in)
                                         (org-todo "LINK")))
                                     (widget-delete widget))
                           (elt q 4))
            (insert "\n"))
          questions)
    (text-scale-set 5)
    (widget-setup)
    (widget-minor-mode)
    (goto-char (point-min))
    (when (functionp 'scroll-bar-mode) (scroll-bar-mode))
    (switch-to-buffer (current-buffer))))

;;;###autoload
(defun my-prepare-index-card-for-journal ()
  "Create an index card for my process journal."
  (interactive)
  (quantified-track "Drawing")
  (my-prepare-index-card "Journal"))
;; Other sketch-related functions:1 ends here

;; [[file:../Sacha.org::#write-about-half-page-scans][Write about half-page scans:1]]
;;;###autoload
(defun my-write-about-half-page-scan (filename)
  (interactive (list (read-file-name (format "Sketch (%s): "
                                             (file-name-base (my-latest-file my-scan-directory)))
                                     (expand-file-name my-scan-directory)
                                     (my-latest-file my-scan-directory)
                                     nil
                                     (expand-file-name my-scan-directory)
                                     (lambda (f) (string-match "\\.\\(jpg\\|png\\)$" f)))))
  (let (new-name)
    (shell-command (concat "~/bin/prepare-half-page " (shell-quote-argument filename)))
    (if (string-match "[0-9]+-[0-9]+-[0-9]+\\([a-z]\\|-[0-9]+\\)? .*" (file-name-base filename))
        (progn
          (rename-file filename (expand-file-name (file-name-nondirectory filename) my-sketches-directory) t)
          (setq new-name filename))
      (save-window-excursion
        (find-file filename)
        (setq new-name (expand-file-name (concat (read-string "New name: ") "." (file-name-extension filename))
                                         my-sketches-directory))
        (rename-file filename new-name)))
    (my-write-about-sketch new-name)))
;; Write about half-page scans:1 ends here

;; [[file:../Sacha.org::#supernote][Supernote:4]]
;;;###autoload
(defun my-sketch-process (file &optional do-crop)
  (interactive (list (read-file-name "File: ")))
	(condition-case nil
			(progn
				(my-image-recognize file)
				(setq file (my-sketch-rename file))

				(pcase (file-name-extension file)
					((or "svg" "pdf")
					 (setq file
								 (my-image-store
									(my-sketch-svg-prepare file))))
					((or "png" "jpg" "jpeg")
					 (setq file
								 (my-image-store
									(my-image-autorotate
									 (if do-crop
											 (my-image-autocrop file
										;; (my-sketch-recolor-png
										;;  file)
																					)
										 file)))))))
		(error nil))
	(find-file file)
	(find-file-other-window (concat (file-name-sans-extension file) ".txt"))
	file)
;; Supernote:4 ends here

;; [[file:../Sacha.org::#supernote-browse][org-attaching the latest image from my Supernote via Browse and Access:2]]
;;;###autoload
(defun my-sketch-insert-latest-doodle ()
	(interactive)
	(let* ((file (my-latest-sketch)))
		(insert
		 (format
			"#+begin_right-doodle
#+ATTR_HTML: :title
%s
#+end_right-doodle"
			(org-link-make-string (concat "file:" file))))))
;; org-attaching the latest image from my Supernote via Browse and Access:2 ends here

;; [[file:../Sacha.org::#supernote-browse][org-attaching the latest image from my Supernote via Browse and Access:4]]
;;;###autoload
(defun my-sketch-insert-latest ()
	(interactive)
	(let ((renamed (my-latest-sketch)))
		(when (and renamed (derived-mode-p 'org-mode))
			(if (string-match "^[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]-[0-9][0-9] "
												(file-name-base renamed))
					(org-insert-link nil (concat "sketchFull:" (file-name-base renamed)))
				;; insert the link
				(org-insert-link nil (concat "file:" renamed)))
			(org-redisplay-inline-images))))
;; org-attaching the latest image from my Supernote via Browse and Access:4 ends here

(provide 'my-sketch)
;;; my-sketch.el ends here
