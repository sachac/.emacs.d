;;; sacha-svg-dot-grid.el ---  -*- lexical-binding: t -*-

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
;; - Dot-grid box templates
;;   https://sachachua.com/dotemacs#multimedia-images-dot-grid-box-templates
;;
;;; Code:



;; [[file:../Sacha.org::sacha-dot-grid-boxes-template][sacha-dot-grid-boxes-template]]
(require 'svg)
(defvar sacha-dot-grid-boxes-params
	'(:num-rows 5
		:num-cols 7
		:dot-size 3
		:line-width 3
		:dot-spacing 60
		:grid-color "#a6d2ff"
		:row-size 6
		:col-size 6
		:text-size 50
		:margin-top 2))
;;;###autoload
(cl-defun sacha-dot-grid-boxes-template (&key (num-rows 5)
                                           (num-cols 7)
                                           (dot-size 3)
																					 (line-width 3)
                                           (dot-spacing 60)
                                           (grid-color "#a6d2ff")
                                           (row-size 6)
                                           (col-size 6)
																					 (text-size 50)
                                           (margin-top 2))
  "Prepare an SVG with a dot grid within a table with solid gridlines.
Each dot is a solid circle of DOT-SIZE filled with GRID-COLOR spaced DOT-SPACING apart.
The gridlines are also GRID-COLOR. They should divide the image into ROWS and COLUMNS, which are ROW-SIZE * DOT-SPACING and COL-SIZE * DOT-SPACING apart.
The table has a top margin with the dot grid, and this is MARGIN-TOP * DOT-SPACING tall.
All dots are centered on their x, y coordinates.
The rest of the image's background is white."
  (let* ((width (* num-cols col-size dot-spacing))
         (height (* dot-spacing (+ margin-top (* num-rows row-size))))
         (margin-top-height (* margin-top dot-spacing))
				 (svg (svg-create width height)))
		(dotimes (row (+ (* num-rows row-size) margin-top))
      (dotimes (col (1+ (* num-cols col-size)))
        (let ((x (* col dot-spacing))
              (y (* row dot-spacing)))
          (svg-circle svg x y dot-size
											:fill-color grid-color
											:stroke-width 0))))
		(when (> text-size 0)
			(dotimes (i (* num-rows num-cols))
        (let ((x (* (% i num-cols) col-size dot-spacing))
              (y (+ margin-top-height (* (/ i num-cols) row-size dot-spacing))))
          (svg-text svg
										(number-to-string (1+ i))
										:x x :y (+ y text-size)
										:fill-color grid-color
										:font-size text-size
										:stroke-width 0))))
    (dotimes (col (1+ num-cols))
      (let ((x (* col col-size dot-spacing)))
        (svg-line svg x margin-top-height x height
									:stroke-color grid-color
									:stroke-width line-width)))
    (dotimes (row (1+ num-rows))
      (let ((y (+ margin-top-height (* row row-size dot-spacing))))
        (svg-line svg 0 y width y
									:stroke-color grid-color
									:stroke-width line-width)))
		svg))
;; sacha-dot-grid-boxes-template ends here

;; [[file:../Sacha.org::sacha-dot-grid-boxes-extract][sacha-dot-grid-boxes-extract]]
;;;###autoload
(cl-defun sacha-dot-grid-boxes-list (&key (num-rows 5)
																			 (num-cols 7)
																			 (dot-spacing 60)
																			 (row-size 6)
																			 (col-size 6)
																			 (text-bottom 1)
																			 (margin-top 2)
																			 filename
																			 &allow-other-keys)
	"Return a list of boxes."
	(let* ((margin-top-height (* margin-top dot-spacing))
				 (max-image-size nil)
				 (size (image-size (create-image filename nil nil :scale 1) t))
				 (ratio (/ (car size) (* num-cols col-size dot-spacing 1.0)))
				 results)
		(message "Expected adjusted height %f actual height %f"
						 (* (+ margin-top (* num-rows row-size)) dot-spacing ratio)
						 (cdr size))
		(dotimes (i (* num-rows num-cols))
			(let* ((r (/ i num-cols))
						 (c (% i num-cols))
						 (y (* (+ margin-top-height (* r col-size dot-spacing)) ratio))
						 (x (* c row-size dot-spacing ratio))
						 (width (* col-size dot-spacing ratio))
						 (height (* (- row-size text-bottom) dot-spacing ratio)))
				(setq results (cons
											 `((r . ,r)
												 (c . ,c)
												 (i . ,i)
												 (x . ,(floor x))
												 (y . ,(floor y))
												 (w . ,(floor width))
												 (h . ,(floor height))
												 (x2 . ,(floor (+ x width)))
												 (y2 . ,(floor (+ y height))))
											 results))))
		(nreverse results)))

(defvar sacha-sketch-icon-directory "~/sync/sketches/icons")
;;;###autoload
(cl-defun sacha-dot-grid-boxes-extract (&rest args &key filename labels
																					 (output-dir sacha-sketch-icon-directory) force &allow-other-keys)
	(let* ((list (apply #'sacha-dot-grid-boxes-list args))
				 (base (file-name-base filename))
				 (ext (concat "." (file-name-extension filename)))
				 (id
					(if (string-match "^[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]-[0-9][0-9]" base)
							(match-string 0 base)
						""))
				 results
				 args)
		(dolist (icon list)
			(let-alist icon
				(let ((new-filename (expand-file-name
														 (concat (sacha-make-slug (elt labels .i)) "--"
																		 id
																		 (format "-%d-%d"
																						 .r .c)
																		 ext)
														 output-dir)))
					(push `((term . ,(elt labels .i))
									(icon . ,(file-name-nondirectory new-filename))
									(source . ,(file-name-nondirectory filename)))
								results)
					(when (or force (not (file-exists-p new-filename)))
						(setq args
									(list (expand-file-name filename)
												"-crop"
												(format "%dx%d+%d+%d" .w .h .x .y)
												"+repage"
												new-filename))
						(message "%s" (concat "convert " (mapconcat #'shell-quote-argument args " ")))
						(apply #'call-process "convert" nil nil nil args)))))
		(nreverse results)))

;;;###autoload
(defun sacha-dot-grid-boxes-labels (id)
	(with-temp-buffer
		(insert-file-contents (concat (file-name-sans-extension (sacha-get-sketch-filename id)) ".txt"))
		(goto-char (point-min))
		(re-search-forward "^ *$")
		(split-string (string-trim (buffer-substring (point) (point-max))) "\n")))
;; sacha-dot-grid-boxes-extract ends here

;; [[file:../Sacha.org::sacha-dot-grid-boxes-process][sacha-dot-grid-boxes-process]]
;;;###autoload
(defun sacha-sketch-icon-update-index (list)
	(let (data
				(index-file (expand-file-name "index.json" sacha-sketch-icon-directory)))
		(with-temp-file index-file
			(setq data
						(if (file-exists-p index-file)
								(json-read-file index-file)
							'()))
			(dolist (entry list)
				;; Remove current entry
				(setq data (seq-remove (lambda (o)
																 (and (string-match (regexp-quote (alist-get 'source o)) (alist-get 'source entry))
																			(string= (alist-get 'term o) (alist-get 'term entry))
                                      (string= (alist-get 'icon o) (alist-get 'icon entry))))
															 data))
				;; Add a new entry
				(push
				 `((term . ,(alist-get 'term entry))
					 (icon . ,(alist-get 'icon entry))
					 (source . ,(alist-get 'source entry)))
				 data))
			(insert (json-encode (sort data :key (lambda (o) (alist-get 'term o)) :lessp #'string<))))))

;;;###autoload
(defun sacha-dot-grid-boxes-process (id &optional force)
	(interactive
	 (list
		(sacha-complete-sketch-filename "drawing")
		current-prefix-arg))
	(let* ((labels (sacha-dot-grid-boxes-labels id))
				 list)
		(cl-assert (= (% (length labels) 7) 0))
		(cl-assert (> (length labels) 1))
		(setq list
					(sacha-dot-grid-boxes-extract :output-dir sacha-sketch-icon-directory
																		 :num-rows (/ (length labels) 7)
																		 :filename (sacha-get-sketch-filename id)
																		 :labels labels
																		 :force force))
		(sacha-sketch-icon-update-index list)))

;;;###autoload
(defun sacha-dot-grid-boxes-process-all-icons ()
	(interactive)
	(dolist (source (sacha-sketches "icons")) (sacha-dot-grid-boxes-process source)))
;; sacha-dot-grid-boxes-process ends here

(provide 'sacha-svg-dot-grid)
;;; sacha-svg-dot-grid.el ends here
