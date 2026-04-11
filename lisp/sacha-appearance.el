;;; sacha-appearance.el ---  -*- lexical-binding: t -*-

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
;; - Display
;;   https://sachachua.com/dotemacs#display
;;
;; - Set up a color scheme
;;   https://sachachua.com/dotemacs#set-up-a-light-on-dark-color-scheme
;;
;; - Making highlight-sexp follow modus-themes-toggle
;;   https://sachachua.com/dotemacs#making-highlight-sexp-follow-modus-themes-toggle
;;
;; - Highlight the active modeline using colours from modus-themes
;;   https://sachachua.com/dotemacs#highlight-the-active-modeline-using-colours-from-modus-themes
;;
;; - Quickly adding face properties to regions
;;   https://sachachua.com/dotemacs#face-text
;;
;; - Highlight the current line while still being able to easily customize/describe underlying faces
;;   https://sachachua.com/dotemacs#highlight-line-mode
;;
;; - Change Org Mode TODO keyword color based on the state and the current Modus theme
;;   https://sachachua.com/dotemacs#sacha-org-todo-set-keyword-faces
;;
;;; Code:



;; [[file:../Sacha.org::#display][Display:1]]
;;;###autoload
  (defun sanityinc/adjust-opacity (frame incr)
    (let* ((oldalpha (or (frame-parameter frame 'alpha) 100))
           (newalpha (+ incr oldalpha)))
      (when (and (<= frame-alpha-lower-limit newalpha) (>= 100 newalpha))
        (modify-frame-parameters frame (list (cons 'alpha newalpha))))))
;; Display:1 ends here

;; [[file:../Sacha.org::#set-up-a-light-on-dark-color-scheme][Set up a color scheme:1]]
;;;###autoload
  (defun sacha-setup-color-theme ()
    (interactive)
    (when (display-graphic-p)
      (load-theme (car modus-themes-to-toggle) t)))
;; Set up a color scheme:1 ends here

;; [[file:../Sacha.org::#making-highlight-sexp-follow-modus-themes-toggle][Making highlight-sexp follow modus-themes-toggle:1]]
(defun sacha-hl-sexp-update-overlay ()
  (when (overlayp hl-sexp-overlay)
    (overlay-put
     hl-sexp-overlay
     'face
     `(:background
       ,(modus-themes-get-color-value 'bg-inactive)))))
(defun sacha-hl-sexp-update-all-overlays (&rest args)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when highlight-sexp-mode
        (sacha-hl-sexp-update-overlay)))))
;; Making highlight-sexp follow modus-themes-toggle:1 ends here

;; [[file:../Sacha.org::#highlight-the-active-modeline-using-colours-from-modus-themes][Highlight the active modeline using colours from modus-themes:1]]
;;;###autoload
  (defun sacha-update-active-mode-line-colors ()
          (set-face-attribute
           'mode-line nil
           :foreground (modus-themes-get-color-value 'fg-mode-line-active)
           :background (modus-themes-get-color-value 'bg-blue-subtle)))
;; Highlight the active modeline using colours from modus-themes:1 ends here

;; [[file:../Sacha.org::#face-text][Quickly adding face properties to regions:1]]
;;;###autoload
  (defun sacha-add-face-text-property (start end attribute value)
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
;; Quickly adding face properties to regions:1 ends here

;; [[file:../Sacha.org::#face-text][Quickly adding face properties to regions:2]]
;;;###autoload
  (defun sacha-face-text-larger (start end)
          (interactive "r")
          (add-face-text-property
           start end
           (list :height (floor (+ 50 (car (alist-get :height (get-text-property start 'face) '(100))))))))
;;;###autoload
  (defun sacha-face-text-smaller (start end)
          (interactive "r")
          (add-face-text-property
           start end
           (list :height (floor (- (car (alist-get :height (get-text-property start 'face) '(100))) 50)))))
;; Quickly adding face properties to regions:2 ends here

;; [[file:../Sacha.org::#highlight-line-mode][Highlight the current line while still being able to easily customize/describe underlying faces:2]]
(defun sacha-suggest-other-faces (func &rest args)
  "Disable `hl-line-mode' when choosing a face."
  (if hl-line-mode
      (progn
        (hl-line-mode -1)
        (prog1 (apply func args)
          (hl-line-mode 1)))
    (apply func args)))
;; Highlight the current line while still being able to easily customize/describe underlying faces:2 ends here

;; [[file:../Sacha.org::#sacha-org-todo-set-keyword-faces][Change Org Mode TODO keyword color based on the state and the current Modus theme:1]]
;;;###autoload
(defun sacha-org-todo-set-keyword-faces ()
	(setq org-todo-keyword-faces
				`(("TODO" . (:foreground ,(modus-themes-get-color-value 'blue-warmer) :weight bold))
					("DONE" . (:foreground ,(modus-themes-get-color-value 'green-warmer) :weight bold))
					("WAITING" . (:foreground ,(modus-themes-get-color-value 'red-warmer) :weight bold))
					("SOMEDAY" . (:foreground ,(modus-themes-get-color-value 'fg-dim) :weight bold))))
	(when (derived-mode-p 'org-mode)
		(font-lock-fontify-buffer)))
;; Change Org Mode TODO keyword color based on the state and the current Modus theme:1 ends here

(provide 'sacha-appearance)
;;; sacha-appearance.el ends here
