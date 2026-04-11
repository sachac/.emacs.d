;;; sacha-fun.el ---  -*- lexical-binding: t -*-

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



;; [[file:../Sacha.org::#building-a-today-i-learned-habit-and-displaying-the-documentation-for-random-emacs-commands][Building a today-I-learned habit, and displaying the documentation for random Emacs commands:1]]
;;;###autoload
(defun sacha-describe-random-interactive-function ()
  (interactive)
  "Show the documentation for a random interactive function.
       Consider only documented, non-obsolete functions."
  (let (result)
    (mapatoms
     (lambda (s)
       (when (and (commandp s)
                  (documentation s t)
                  (null (get s 'byte-obsolete-info)))
         (setq result (cons s result)))))
    (describe-function (elt result (random (length result))))))
;; Building a today-I-learned habit, and displaying the documentation for random Emacs commands:1 ends here

;; [[file:../Sacha.org::#shuffling-lines][Shuffling lines:1]]
;;;###autoload
(defun sacha-shuffle-lines-in-region (beg end)
  "Randomize the order of lines from BEG to END."
  (interactive "r")
  (let ((list (split-string (buffer-substring beg end) "[\r\n]+")))
    (delete-region beg end)
    (insert (string-join (seq-sort-by (lambda (_) (random)) #'<= list) "\n"))))
;; Shuffling lines:1 ends here

;; [[file:../Sacha.org::#controlling-sacha-stream-audio-from-emacs-background-music-typing-sounds-and-push-to-talk][Controlling my stream audio from Emacs: background music, typing sounds, and push to talk:3]]
;;;###autoload
(defun sacha-selectric-type-sound ()
  "Make the sound of typing."
  ;; Someday, randomize this or something
  (selectric-make-sound (expand-file-name "selectric-move.wav" selectric-files-path)))
;; Controlling my stream audio from Emacs: background music, typing sounds, and push to talk:3 ends here

;; [[file:../Sacha.org::#miscellaneous-stardew-valley][Stardew Valley:1]]
;;;###autoload
(defun sacha-stardew-install-mod (file)
	(interactive (list (read-file-name "Zip: " "~/Downloads/")))
	(call-process "unzip"

								nil (get-buffer-create "*mods*") nil
								"-uo"
								file
								"-d" (expand-file-name "/home/sacha/.local/share/Steam/steamapps/common/Stardew Valley/Mods/"))
	(message "Installed %s" (file-name-base file))
	)

;;;###autoload
(defun sacha-stardew-install-latest-mod ()
	(interactive)
	(sacha-stardew-install-mod (sacha-latest-file "~/Downloads")))
;; Stardew Valley:1 ends here

;; [[file:../Sacha.org::#diagrams][Diagrams:1]]
;; Start of cubing code
(defun sacha-cubing-pos (size n i)
	(list
	 (* (/ size n) (% i n))
	 (* (/ size n) (/ i n))))

;;;###autoload
(defun sacha-cubing-last-layer-arrows (arrows)
	"Draw ARROWS.
Arrows are defined as a list of lists of the form
((from to) (from to t) ...). Ex: '(sacha-cubing-last-layer-arrows '((3 1 t) (2 8 t)))
Cells are numbered from left to right, top to bottom, with the top left box being 0.
"
	(let* ((size 99)
				 (n 3)
				 (arrow-color "#000")
				 (svg (svg-create size size)))
		(svg--append
		 svg
		 (dom-node
			'defs
			nil
			(dom-node
			 'marker
			 '((id . "arrowhead")
				 (markerWidth . "10")
				 (markerHeight . "7")
				 (refX . "0")
				 (refY . "3.5")
				 (orient . "auto-start-reverse"))
			 (dom-node
				'polygon
				`((fill . ,arrow-color)
					(points . "0 0, 4 3.5, 0 7")))
			 )))
		(dotimes (i (* n n))
			(let ((pos (sacha-cubing-pos size n i)))
				(svg-rectangle
				 svg
				 (car pos)
				 (cadr pos)
				 (/ size n)
				 (/ size n)
				 :fill "#fff"
				 :stroke-width 1
				 :stroke "#666")))
		(dolist (arrow arrows)
			(let ((from (car arrow))
						(to (cadr arrow)))
				(apply 'svg-line
							 (append
								(list svg)
								(mapcar (lambda (o) (+ o (/ size (* 2 n))))
												(sacha-cubing-pos size n from))
								(mapcar (lambda (o) (+ o (/ size (* 2 n))))
												(sacha-cubing-pos size n to))
								(list
								 :stroke-width 2
								 :stroke arrow-color
								 :marker-start (if (elt arrow 2) "url(#arrowhead)")
								 :marker-end "url(#arrowhead)")))))
		(with-temp-buffer
			(svg-print svg)
			(buffer-string))))

(defvar sacha-cubing-colors '((?R  . "#ff0000")
													 (?G  . "#00ff00")
													 (?B  . "#0000ff")
													 (?O  . "#ed7117")
													 (?Y  . "#ffff00")
													 (?W  . "#ffffff")
													 (?\? . "#666666")))

;;;###autoload
(defun sacha-cubing-last-layer-with-sides (sides top arrows)
	"Draw a diagram of the top of the cube.
The style is similar to https://www.cubeskills.com/uploads/pdf/tutorials/pll-algorithms.pdf .
SIDES is a string specifying colors going clockwise from the back-left side.
TOP is a string specifying colors going from left to right, top to bottom.
Arrows are defined as a list of lists of the form ((from to) (from to t) ...).
Cells are numbered from left to right, top to bottom, with the top left box being 0.
Ex: (sacha-cubing-last-layer-with-sides \"ORRBOOGGGRBB\" \"YYYYYYYYY\" '((3 1 t) (2 8 t)))
"
	(let* ((size 99)
				 (n 3)
				 (side-size 10)
				 (cell-size (/ (- size (* 2 side-size)) n))
				 (arrow-color "#000")
				 (svg (svg-create size size)))
		(svg--append
		 svg
		 (dom-node
			'defs
			nil
			(dom-node
			 'marker
			 '((id . "arrowhead")
				 (markerWidth . "10")
				 (markerHeight . "7")
				 (refX . "0")
				 (refY . "3.5")
				 (orient . "auto-start-reverse"))
			 (dom-node
				'polygon
				`((fill . ,arrow-color)
					(points . "0 0, 4 3.5, 0 7"))))))
		;; Draw the sides. It's a string of colors going clockwise from back left
		(when sides
			(dotimes (i (* n 4))
				(apply 'svg-rectangle
							 (append
								(list svg)
								(pcase (/ i n)
									(0 (list (+ (* (% i n) cell-size) side-size)
													 0
													 cell-size
													 side-size))
									(1 (list (+ side-size (* n cell-size))
													 (+ (* (% i n) cell-size) side-size)
													 side-size
													 cell-size))
									(2 (list (+ (* (- n (% i n) 1) cell-size) side-size)
													 (+ (* n cell-size) side-size)
													 cell-size
													 side-size))
									(3 (list 0
													 (+ (* (- n (% i n) 1) cell-size) side-size)
													 side-size
													 cell-size)))
								(list
								 :stroke-width 1
								 :stroke "#666"
								 :fill (assoc-default (elt sides i)
																			sacha-cubing-colors
																			'eq
																			(assoc-default ?\? sacha-cubing-colors)))))))
		;; Draw the top face specified by a string of colors going from left to right, top to bottom
		(dotimes (i (* n n))
			(let ((pos (sacha-cubing-pos (* cell-size n) n i)))
				(svg-rectangle
				 svg
				 (+ side-size (car pos))
				 (+ side-size (cadr pos))
				 cell-size
				 cell-size
				 :fill (if top
									 (assoc-default (elt top i) sacha-cubing-colors
																	'eq
																	(assoc-default ?\? sacha-cubing-colors))
								 (assoc-default ?\? sacha-cubing-colors))
				 :stroke-width 1
				 :stroke "#666")))
		;; Draw the arrows
		(dolist (arrow arrows)
			(let ((from (car arrow))
						(to (cadr arrow)))
				(apply 'svg-line
							 (append
								(list svg)
								(mapcar (lambda (o) (+ side-size o (/ cell-size 2)))
												(sacha-cubing-pos (* n cell-size) n from))
								(mapcar (lambda (o) (+ side-size o (/ cell-size 2)))
												(sacha-cubing-pos (* n cell-size) n to))
								(list
								 :stroke-width 2
								 :stroke arrow-color
								 :opacity 0.5
								 :marker-start (if (elt arrow 2) "url(#arrowhead)")
								 :marker-end "url(#arrowhead)")))))
		(with-temp-buffer
			(svg-print svg)
			(buffer-string))))
;; end of cubing code
;; Diagrams:1 ends here

(provide 'sacha-fun)
;;; sacha-fun.el ends here
