;;; my-appearance.el ---  -*- lexical-binding: t -*-

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
;; - Highlight the active modeline using colours from modus-themes
;;   https://sachachua.com/dotemacs#highlight-the-active-modeline-using-colours-from-modus-themes
;;
;; - Quickly adding face properties to regions
;;   https://sachachua.com/dotemacs#face-text
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
  (defun my-setup-color-theme ()
    (interactive)
    (when (display-graphic-p)
      (load-theme (car modus-themes-to-toggle) t)))
;; Set up a color scheme:1 ends here

;; [[file:../Sacha.org::#highlight-the-active-modeline-using-colours-from-modus-themes][Highlight the active modeline using colours from modus-themes:1]]
;;;###autoload
  (defun my-update-active-mode-line-colors ()
          (set-face-attribute
           'mode-line nil
           :foreground (modus-themes-get-color-value 'fg-mode-line-active)
           :background (modus-themes-get-color-value 'bg-blue-subtle)))
;; Highlight the active modeline using colours from modus-themes:1 ends here

;; [[file:../Sacha.org::#face-text][Quickly adding face properties to regions:1]]
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
;; Quickly adding face properties to regions:1 ends here

;; [[file:../Sacha.org::#face-text][Quickly adding face properties to regions:2]]
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
;; Quickly adding face properties to regions:2 ends here

(provide 'my-appearance)
;;; my-appearance.el ends here
