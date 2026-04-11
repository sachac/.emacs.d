;;; sacha-svg.el ---  -*- lexical-binding: t -*-

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

;;; Code:



;; [[file:../Sacha.org::#svg-identifying-paths][Identifying paths:1]]
(defvar sacha-svg-auto-resize-timer nil)
;; based on image-mode
;;;###autoload
(defun sacha-svg-resize-with-window (window)
	(when (numberp image-auto-resize-on-window-resize)
    (when sacha-svg-auto-resize-timer
      (cancel-timer sacha-svg-auto-resize-timer))
    (setq sacha-svg-auto-resize-timer
          (run-with-idle-timer 1 nil
                               #'sacha-svg-fit-to-window window))))
;;;###autoload
(defun sacha-svg-fit-to-window (window)
	(when (window-live-p window)
    (with-current-buffer (window-buffer window)
      (let ((spec (get-text-property (point-min) 'display)))
        (when (eq (car-safe spec) 'image)
          (let* ((image-width  (plist-get (cdr spec) :max-width))
                 (image-height (plist-get (cdr spec) :max-height))
                 (edges (window-inside-pixel-edges window))
                 (window-width  (- (nth 2 edges) (nth 0 edges)))
                 (window-height (- (nth 3 edges) (nth 1 edges))))
            ;; If the size has been changed manually (with `+'/`-'),
            ;; then :max-width/:max-height is nil.  In that case, do
            ;; no automatic resizing.
            (when (and image-width image-height
                       ;; Don't do resizing if we have a manual
                       ;; rotation (from the `r' command), either.
                       (not (plist-get (cdr spec) :rotation))
                       (or (not (= image-width  window-width))
                           (not (= image-height window-height))))
              (unless image-fit-to-window-lock
                (unwind-protect
                    (progn
                      (setq-local image-fit-to-window-lock t)
                      (ignore-error remote-file-error
												(setcdr spec
																(plist-put
																 (plist-put (cdr spec) :max-width window-width)
																 :max-height window-height))
												(put-text-property (point-min) (1+ (point-min))
																					 'display spec)))
                  (setq image-fit-to-window-lock nil))))))))))

;;;###autoload
(defun sacha-svg-bounding-box (node)
	"Return (x1 y1 x2 y2) for NODE.

Note: Relative paths don't work very well yet, so it's probably
better to set Inkscape's Preferences - Input/Output - SVG output
- Path string format - Absolute."
	(require 's)
	(pcase (dom-tag node)
		('rect
		 (list
			(string-to-number (dom-attr node 'x))
			(string-to-number (dom-attr node 'y))
			(+ (string-to-number (dom-attr node 'x)) (string-to-number (dom-attr node 'width)))
			(+ (string-to-number (dom-attr node 'y)) (string-to-number (dom-attr node 'height)))))
		('path
		 (let ((path (dom-attr node 'path))
					 (x1 most-positive-fixnum)
					 (y1 most-positive-fixnum)
					 (x2 most-negative-fixnum)
					 (y2 most-negative-fixnum)
					 (x 0)
					 (y 0)
					 (i 0))
			 (dolist (seg (s-slice-at " *[MCmc] *" path))
				 (unless (string= (string-trim seg) "")
					 (setq seg (split-string seg "[ ,]") i 0)
					 (let ((points (mapcar 'string-to-number (cdr seg))))
						 (pcase (car seg)
							 ((or "m" "M")
								(if (or (eq (car seg) "M") (= i 0))
										;; starting points are always absolute
										(setq x (car points)
													y (cadr points))
									;; m, so relative movement
									(setq x (+ x (car points))
												y (+ y (cadr points))))
								(when (< x x1) (setq x1 x))
								(when (< y y1) (setq y1 y))
								(when (> x x2) (setq x2 x))
								(when (> y y2) (setq y2 y)))
							 ("c"
								(let ((old-x x) (old-y y))
									(dolist (set (seq-partition points 6))
										;; relative movement? still very fuzzy on how this should work
										(setq x (+ x (elt set 4))
													y (+ y (elt set 5)))
										(when (< x x1) (setq x1 x))
										(when (< y y1) (setq y1 y))
										(when (> x x2) (setq x2 x))
										(when (> y y2) (setq y2 y))))
								)
							 ("C"
								(dolist (set (seq-partition points 2))
									(setq x (elt set 0))
									(setq y (elt set 1))
									(when (and x y)
										(when (< x x1) (setq x1 x))
										(when (< y y1) (setq y1 y))
										(when (> x x2) (setq x2 x))
										(when (> y y2) (setq y2 y))))))
						 (cl-incf i))))
			 (list x1 y1 x2 y2)))
		)
	)
;; Identifying paths:1 ends here

;; [[file:../Sacha.org::#svg-identifying-paths][Identifying paths:3]]
;;;###autoload
(defun sacha-svg-display (buffer-name svg &optional highlight-id full-window)
	"HIGHLIGHT-ID is a string ID or a node."
	(with-current-buffer (get-buffer-create buffer-name)
		(when highlight-id
			;; make a copy
			(setq svg (with-temp-buffer (svg-print svg) (car (xml-parse-region (point-min) (point-max)))))
			(if-let* ((path (if (stringp highlight-id) (dom-by-id svg highlight-id) highlight-id))
								(view-box (split-string (dom-attr svg 'viewBox)))
								(box (sacha-svg-bounding-box path))
								(parent (car path)))
					(progn
						;; find parents for possible rotation
						(while (and parent (not (dom-attr parent 'transform)))
							(setq parent (dom-parent svg parent)))
						(dom-set-attribute path 'style
															 (concat (dom-attr path 'style) "; stroke: 1px red; fill: #ff0000 !important"))
						;; add a crosshair
						(dom-append-child
						 (or parent svg)
						 (dom-node 'path
											 `((d .
														,(format "M %f,0 V %s M %f,0 V %s M 0,%f H %s M 0,%f H %s"
																		 (elt box 0)
																		 (elt view-box 3)
																		 (elt box 2)
																		 (elt view-box 3)
																		 (elt box 1)
																		 (elt view-box 2)
																		 (elt box 3)
																		 (elt view-box 2)))
												 (stroke-dasharray . "5,5")
												 (style . "fill:none;stroke:gray;stroke-width:3px")))))
				(error "Could not find %s" highlight-id)))
		(let* ((inhibit-read-only t)
					 (image (svg-image svg))
					 (edges (window-inside-pixel-edges (get-buffer-window))))
			(erase-buffer)
			(if full-window
					(progn
						(delete-other-windows)
						(switch-to-buffer (current-buffer)))
				(display-buffer (current-buffer)))
			(insert-image (append image
														(list :max-width
																	(floor (* 0.8 (- (nth 2 edges) (nth 0 edges))))
																	:max-height
																	(floor (* 0.8 (- (nth 3 edges) (nth 1 edges)))) )))
			;; (sacha-svg-resize-with-window (selected-window))
			;; (add-hook 'window-state-change-functions #'sacha-svg-resize-with-window t)
			(current-buffer))))

;;;###autoload
(cl-defun sacha-svg-identify-paths (filename &key selector node-func dom)
	"Prompt for IDs for each path in FILENAME."
	(interactive (list (read-file-name "SVG: " nil nil
																		 (lambda (f)
																			 (or (string-match "\\.svg$" f)
																					 (file-directory-p f))))))
	(let* ((dom (or dom (car (xml-parse-file filename))))
				 (paths (if (functionp selector)
										(dom-search dom selector)
									(or selector
											(dom-by-tag dom 'path))))
				 (vertico-count 3)
				 (ids (seq-keep (lambda (path)
													(and (dom-attr path 'id)
															 (unless (string-match "\\(path\\|rect\\)[0-9]+"
																										 (or (dom-attr path 'id) "path0"))
																 (dom-attr path 'id))))
												paths))
				 (edges (window-inside-pixel-edges (get-buffer-window)))
				 id)
		(sacha-svg-display "*image*" dom nil t)
		(dolist (path paths)
			;; display the image with an outline
			(unwind-protect
					(progn
						(sacha-svg-display "*image*" dom (dom-attr path 'id) t)
						(if (functionp node-func)
								(funcall node-func path dom)
							(setq id (completing-read
												(format "ID (%s): " (dom-attr path 'id))
												ids))
							;; already exists, merge with existing element
							(if-let* ((old (dom-by-id dom id)))
									(progn
										(dom-set-attribute
										 old
										 'd
										 (concat (dom-attr (dom-by-id dom id) 'd)
														 " "
														 ;; change relative to absolute
														 (replace-regexp-in-string "^m" "M"
																											 (dom-attr path 'd))))
										(dom-remove-node dom path)
										(setq id nil))
								(dom-set-attribute path 'id id)
								(add-to-list 'ids id)))))
			;; save the image just in case we get interrupted halfway through
			(with-temp-file filename
				(svg-print dom)))))

;;;###autoload
(defun sacha-svg-identify-rects (filename)
	(interactive (list (read-file-name "SVG: " nil nil
																		 (lambda (f)
																			 (or (string-match "\\.svg$" f)
																					 (file-directory-p f))))))
	(sacha-svg-identify-paths
	 filename
	 :selector
	 (lambda (elem)
		 (and (eq (dom-tag elem) 'rect)
					(not (and (dom-attr elem 'class)
										(string-match "\\<background\\>" (dom-attr elem 'class))))))))

;;;###autoload
(defun sacha-org-links-from-file (filename)
	"Return a list of (description . link) of the Org links in FILENAME."
	(when (file-exists-p filename)
		(let (results)
			(with-temp-buffer
				(insert-file-contents filename)
				(goto-char (point-min))
				(while (re-search-forward org-link-any-re nil t)
					(push (cons (match-string-no-properties 3)
											(or (match-string-no-properties 2)
													(match-string-no-properties 0)))
								results)))
			(reverse results))))

;;;###autoload
(defun sacha-svg-linkify-rects (filename)
	(interactive (list (read-file-name "SVG: " nil nil
																		 (lambda (f)
																			 (or (string-match "\\.svg$" f)
																					 (file-directory-p f))))))
	(let ((dom (car (xml-parse-file filename)))
				(links-from-text (sacha-org-links-from-file (concat (file-name-sans-extension filename) ".txt"))))
		(sacha-svg-identify-paths
		 filename
		 :dom
		 dom
		 :selector
		 (append
			;; not yet linked
			(dom-search dom
									(lambda (elem)
										(and (eq (dom-tag elem) 'rect)
												 (not (and (dom-attr elem 'class)
																	 (string-match "\\<background\\|link-rect\\>" (dom-attr elem 'class)))))))
			;; linked
			(dom-search dom
									(lambda (elem)
										(and (eq (dom-tag elem) 'rect)
												 (string-match "\\<link-rect\\>" (or (dom-attr elem 'class) ""))))))

		 :node-func
		 (lambda (elem dom)
			 (let* ((current-link-node (sacha-dom-closest dom elem 'a))
							(current-title-node (or (dom-by-tag elem 'title)
																			(dom-by-tag current-link-node 'title)))
							(title (string-trim
											(completing-read
											"Title: "
											(mapcar 'car links-from-text)
											nil nil
											(dom-text current-title-node))))
							(link (string-trim
										 (read-string
											 "URL: "
											 (or (dom-attr current-link-node 'href)
													 (assoc-default title links-from-text 'string=)))
										 )))
				 (cond
					((and current-link-node (not (string= link "")))
					 (dom-set-attribute elem
															'style
															"stroke: blue; stroke-dasharray: 4; fill: #006fff; fill-opacity: 0.25")
					 (dom-set-attribute current-link-node 'href link))
					((and current-link-node (string= link ""))
					 (dom-add-child-before
						(dom-parent dom current-link-node)
						elem)
					 (dom-remove-node current-link-node))
					((and (null current-link-node) (not (string= link "")))
					 (setq current-link-node (dom-node
																		'a
																		`((href . ,link)
																			(class . "link"))))
					 (dom-add-child-before (dom-parent dom elem) current-link-node elem)
					 (dom-remove-node dom elem)
					 (dom-append-child current-link-node elem)
					 (dom-remove-attribute elem 'fill)
					 (dom-set-attribute elem
															'style
															"stroke: blue; stroke-dasharray: 4; fill: #006fff; fill-opacity: 0.25")
					 (dom-set-attribute
						elem
						'class
						(if (dom-attr elem 'class)
								(concat (dom-attr elem 'class) " link-rect")
							"link-rect"))))
				 (cond
					((and (string= title "") current-title-node)
					 (dom-remove-node current-title-node))
					((and (not (string= title "")) (not current-title-node))
					 (dom-append-child current-link-node (dom-node 'title nil title)))
					((and (not (string= title "")) current-title-node)
					 (setf (car (dom-children current-title-node))
								 title))))))))

;;;###autoload
(defun sacha-svg-update-links-from-text (filename)
	(interactive (list (read-file-name
											"SVG: " nil
											(if (file-exists-p (concat (file-name-sans-extension (buffer-file-name)) ".svg"))
													(concat (file-name-sans-extension (buffer-file-name)) ".svg")
												(cdr (sacha-embark-image)))
											(lambda (f)
												(or (string-match "\\.svg$" f)
														(file-directory-p f))))))
	(let ((dom (car (xml-parse-file filename)))
				(links-from-text (sacha-org-links-from-file (concat (file-name-sans-extension filename) ".txt"))))
		(dolist (link (dom-by-tag dom 'a))
			(when (and
						 (assoc-default (dom-text (dom-by-tag link 'title))
														links-from-text)
						 (not (string=
									 (dom-attr link 'href)
									 (assoc-default (dom-text (dom-by-tag link 'title))
																	links-from-text))))
				(dom-set-attribute
				 link
				 'href
				 (assoc-default (dom-text (dom-by-tag link 'title))
												links-from-text))))
		(with-temp-file filename
			(svg-print dom))))


;; Identifying paths:3 ends here

;; [[file:../Sacha.org::#multimedia-images-svg-animating-svgs-linking-paths][Linking paths:1]]
;;;###autoload
(defun sacha-dom-closest (dom node tag)
	(let ((current node))
		(while (and current (not (eq (dom-tag current) tag)))
			(setq current (dom-parent dom current)))
		current))

;;;###autoload
(defun sacha-svg-save-links (widget &rest ignore)
	(let ((inputs (widget-get widget :inputs))
				(dom (widget-get widget :dom)))
		(dolist (input inputs)
			(let ((link (xml-escape-string (string-trim (widget-value (cdr input)))))
						(node (widget-get (cdr input) :node))
						(link-node (widget-get (cdr input) :link-node))
						parent)
				(cond
				 ;; remove link if linked
				 ((string= link "")
					(when link-node
						(setq parent (dom-parent dom link-node))
						(dolist (child (dom-children link-node))
							;; move all the parent's children to the grandparent
							(dom-add-child-before parent child link-node))
						(dom-remove parent link-node)
						(widget-put (cdr input) :link-node nil)))
				 ;; update link
				 (link-node
					(dom-set-attribute link-node 'href link))
				 ;; add a link
				 (t
					(let ((new-link (dom-node 'a `((href . ,link)))))
						(setq parent (dom-parent dom node))
						(dom-add-child-before parent new-link node)
						(dom-remove-node parent node)
						(dom-append-child new-link node)
						(widget-put (cdr input) :link-node new-link))))))
		(with-temp-file (widget-get widget :file)
			(svg-print dom))
		(find-file (widget-get widget :file))))

(defvar-local original-styles nil)

;; (defun
;; 		sacha-svg-highlight-from-widget (widget)
;; 	(let ((new-dom (copy-tree (widget-get widget :dom)))
;; 				(image (widget-get widget :image)))
;; 		(dom-by-id new-dom (dom-attr (widget-get widget :node) 'id))

;; 		(put-text-property 0 (length image) 'display (svg-image new-dom))

;; 		;; highlight the widget's node
;; 		;; update the image being displayed

;; 		)
;; 	)


;;;###autoload
(defun sacha-svg-link-rects (filename)
	"Add links to rects in FILENAME.
Exclude the background rect."
	(interactive (list (read-file-name "SVG: " nil nil (lambda (f)
																											 (or (string-match "\\.svg$" f)
																													 (file-directory-p f))))))
	(let* ((dom (car (xml-parse-file filename)))
				 (rects (seq-remove (lambda (elem)
															(and (dom-attr elem 'class)
																	 (string-match "\\<background\\>" (dom-attr elem 'class))))
														(dom-by-tag dom 'rect)))
				 image
				 inputs)
		(with-current-buffer (get-buffer-create "*svg*")
			(erase-buffer)
			(setq image
						(propertize "x" 'display (svg-image dom)))
			(widget-insert image "\n")
			(seq-map-indexed
			 (lambda (node i)
				 ;; save the original style for including later
				 (push (cons (dom-attr node 'id) (dom-attr node 'style))
							 original-styles)
				 (let ((link (sacha-dom-closest dom node 'a)))
					 (widget-insert (format "Rect %d: ") i)
					 (push (list
									title
									(widget-create 'editable-field
																 :image
																 image
																 :dom
																 dom
																 :node
																 node
																 :link-node
																 link
																 :help-echo
																 #'sacha-svg-highlight-from-widget
																 :format
																 "Link: %v"
																 :value
																 (and link (dom-attr link 'href)))
									(widget-create 'editable-field
																 :image
																 image
																 :dom
																 dom
																 :node
																 node
																 :help-echo
																 #'sacha-svg-highlight-from-widget
																 :link-node
																 link
																 :format
																 "Title: %v"
																 :value
																 (dom-text (or (dom-by-tag node 'title)
																							 (and link (dom-by-tag link 'title))))))
								 inputs)))
			 rects)
			(widget-create 'push-button
										 :file filename
										 :inputs inputs
										 :dom dom
										 :notify #'sacha-svg-save-links
										 "Save")

			(widget-setup)
			(widget-minor-mode)
			(goto-char (point-min))
			(switch-to-buffer (current-buffer)))))

;;;###autoload
(defun sacha-svg-link-groups-with-titles (filename)
	"Add links to paths in FILENAME.
Paths should have the title attribute."
	(interactive (list (read-file-name "SVG: " nil nil (lambda (f)
																											 (or (string-match "\\.svg$" f)
																													 (file-directory-p f))))))
	(let* ((dom (car (xml-parse-file filename)))
				 (titles (dom-by-tag dom 'title))
				 inputs)
		(with-current-buffer (get-buffer-create "*svg*")
			(erase-buffer)
			(dolist (node titles)
				(let ((title (dom-text node))
							(group (sacha-dom-closest dom node 'g))
							(link (sacha-dom-closest dom node 'a)))
					(when (and group title (> (length title) 0))
						(widget-insert title ": ")
						(push (cons title
												(widget-create 'editable-field
																			 :node
																			 node
																			 :link-node
																			 link
																			 :title
																			 title
																			 :value
																			 (and link (dom-attr link 'href))))
									inputs))))
			(widget-create 'push-button
										 :file filename
										 :inputs inputs
										 :dom dom
										 :notify #'sacha-svg-save-links
										 "Save")
			(widget-setup)
			(widget-minor-mode)
			(goto-char (point-min))
			(switch-to-buffer (current-buffer)))))
;; (sacha-svg-link-paths (sacha-latest-file "~/sync/sketches"))
;; Linking paths:1 ends here

;; [[file:../Sacha.org::#svg-sorting-paths][Sorting paths:1]]
;;;###autoload
(defun sacha-svg-reorder-paths (filename &optional ids output-filename)
	"Sort paths in FILENAME."
	(interactive (list (read-file-name "SVG: " nil nil (lambda (f) (string-match "\\.svg$" f)))
										 nil (read-file-name "Output: ")))
	(let* ((dom (car (xml-parse-file filename)))
				 (paths (dom-by-tag dom 'path))
				 (parent (dom-parent dom (car paths)))
				 (ids-left
					(nreverse (seq-keep (lambda (path)
																(unless (string-match "path[0-9]+" (or (dom-attr path 'id) "path0"))
																	(dom-attr path 'id)))
															paths)))
				 list)
		(when (called-interactively-p)
			(while ids-left
				(sacha-svg-display "*image*" dom (car ids-left))
				(let ((current (completing-read
												(format "ID (%s): "
																(car ids-left))
												ids-left nil nil nil nil (car ids-left)))
							node)
					(add-to-list 'ids current)
					(setq ids-left (seq-remove (lambda (o) (string= o current)) ids-left)))))
		(if ids ;; reorganize under the first path's parent
				(progn
					(dolist (id ids)
						(if-let ((node (car (dom-by-id dom id))))
								(progn
									(dom-remove-node dom node)
									(dom-append-child parent node))
							(message "Could not find %s" id)))
					(with-temp-file (or output-filename filename)
						(svg-print dom))))
		(nreverse (seq-keep (lambda (path)
													(unless (string-match "path[0-9]+" (or (dom-attr path 'id) "path0"))
														(dom-attr path 'id)))
												(dom-by-tag dom 'path)))))
;; Sorting paths:1 ends here

;; [[file:../Sacha.org::#svg-animating-paths-in-order][Animating paths in order:1]]
;;;###autoload
(defun sacha-animate-svg-paths (filename output-dir)
	"Add one path at a time. Save the resulting SVGs to OUTPUT-DIR."
	(unless (file-directory-p output-dir)
		(make-directory output-dir t))
	(let* ((dom (xml-parse-file filename))
				 (paths (seq-filter (lambda (e) (dom-attr e 'style))
														(dom-by-tag dom 'path)))
				 (total (length paths))
				 (frame-num (length paths))
				 result)
		(dolist (elem paths)
			(dom-set-attribute elem 'style
												 (concat
													(dom-attr elem 'style)
													";mix-blend-mode:darken")))
		(with-temp-file (expand-file-name (format "frame-%03d.svg" (1+ frame-num)) output-dir)
			(xml-print dom))
		(dolist (elem paths)
			(dom-set-attribute elem 'style
												 (concat
													(dom-attr elem 'style)
													";fill-opacity:0")))
		(dolist (elem paths)
			(with-temp-file (expand-file-name
											 (format "frame-%03d.svg"
															 (- total frame-num))
											 output-dir)
				(message "%03d" frame-num)
				(dom-set-attribute elem 'style
													 (concat (dom-attr elem 'style)
																	 ";fill-opacity:1"))
				(push (list (format "frame-%03d.svg"
														(1+ (- total frame-num)))
										(dom-attr elem 'id))
							result)
				(setq frame-num (1- frame-num))
				(xml-print dom)))
		(reverse result)))
;; Animating paths in order:1 ends here

;; [[file:../Sacha.org::revealjs-css-animation-code][revealjs-css-animation-code]]
;;;###autoload
(defun sacha-reveal-svg-animation (slide)
	(string-join
	 (seq-map-indexed
		(lambda (step-ids i)
			(format "%s { fill: #f6f396; transition: fill %ds; transition-delay: %ds }"
							(mapconcat
							 (lambda (id) (format "#slide-%s.present #%s" (car slide) id))
							 (split-string step-ids ",")
							 ", ")
							highlight-duration
							(* i highlight-duration)))
		(split-string (elt slide 1) ";"))
	 "\n"))

;;;###autoload
(defun sacha-reveal-svg-highlight-different-colors (slide)
	(let* ((colors '("#f6f396" "#c6c6c6")) ; reverse
				 (steps (split-string (elt slide 1) ";"))
				 (step-length 0.5))
		(string-join
	 	 (seq-map-indexed
			(lambda (step-ids i)
				(format "%s { fill: %s; opacity: 1 !important; transition: fill %.1fs; transition-delay: %.1fs }"
								(mapconcat
								 (lambda (id) (format "#slide-%s.present #%s" (car slide) id))
								 (split-string step-ids ",")
								 ", ")
								(elt colors (- (length steps) i 1))
								step-length
								(* i 0.5)))
			steps))))

;;;###autoload
(defun sacha-reveal-svg-progression-css (map-progression &optional highlight-duration)
	"Make the CSS.
map-progression should be a list of lists with the following format:
((\"slide-id\" \"prev1,prev2;cur1\" \"id-to-add1,id-to-add2\") ...)."
	(setq highlight-duration (or highlight-duration 2))
	(let (full)
		(format
		 "<style>%s</style>"
		 (mapconcat
			(lambda (slide)
				(setq full (append (split-string (elt slide 2) ",") full))
				(format "#slide-%s.present path { opacity: 0.2 }
%s { opacity: 1 !important }
%s"
								(car slide)
								(mapconcat (lambda (id) (format "#slide-%s.present #%s" (car slide) id))
													 full
													 ", ")
								(sacha-reveal-svg-highlight-different-colors slide)))
			map-progression
			"\n"))))
;; revealjs-css-animation-code ends here

(provide 'sacha-svg)
;;; sacha-svg.el ends here
