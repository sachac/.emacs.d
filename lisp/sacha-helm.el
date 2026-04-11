;;; sacha-helm.el ---  -*- lexical-binding: t -*-

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
;; - Helm completion with sacha-helm-org-sketches
;;   https://sachachua.com/dotemacs#helm-completion-with-sacha-helm-org-sketches
;;
;; - Digital index piles with Emacs
;;   https://sachachua.com/dotemacs#digital-index-piles-with-emacs
;;
;;; Code:



;; [[file:../Sacha.org::#helm-completion-with-sacha-helm-org-sketches][Helm completion with sacha-helm-org-sketches:1]]
;;;###autoload
(defun sacha-helm-source-org-sketch-list ()
  (sacha-list-sketches "."))

;;;###autoload
(defun sacha-helm-org-insert-sketch-candidates (&optional candidates)
  (mapc (lambda (o)
          (org-insert-link nil (concat "sketch:" o))
          (insert "\n"))
        (helm-marked-candidates)))

;;;###autoload
(defun sacha-helm-open-sketches-in-krita (&optional candidates)
  (sacha-sketch-open-in-krita (helm-marked-candidates)))

;;;###autoload
(defun sacha-helm-open-sketches-in-gwenview (&optional candidates)
  (sacha-sketch-open-in-gwenview (helm-marked-candidates)))

;;;###autoload
(defun sacha-helm-open-sketches-in-feh (&optional candidates)
  (sacha-sketch-open-in-feh (helm-marked-candidates)))

(defvar sacha-helm-source-org-sketches
  '((name . "Sketches")
    (candidates . sacha-helm-source-org-sketch-list)
    (action . (("Insert" . sacha-helm-org-insert-sketch-candidates)
               ("Open in Krita" . sacha-helm-open-sketches-in-krita)
               ("Open in Gwenview" . sacha-helm-open-sketches-in-gwenview)
               ("Open as Feh slideshow" . sacha-helm-open-sketches-in-feh)))
    (persistent-action . sacha-helm-open-sketches-in-gwenview)))

;;;###autoload
(defun sacha-helm-org-sketches ()
  (interactive)
  (helm :sources '(sacha-helm-source-org-sketches)
        :buffer "*helm-org-sketches*"))
;; Helm completion with sacha-helm-org-sketches:1 ends here

;; [[file:../Sacha.org::#digital-index-piles-with-emacs][Digital index piles with Emacs:2]]
(defvar sacha-helm-org-list-candidates nil)
;;;###autoload
(defun sacha-helm-org-list-categories-init-candidates ()
  "Return a list of categories from this list in a form ready for Helm."
  (setq sacha-helm-org-list-candidates
        (mapcar (lambda (x)
                  (cons (if (elt x 3)
                            (format "%s - %s" (car x) (elt x 3))
                          (car x))
                        x))
                (sacha-org-get-list-categories))))
;; Digital index piles with Emacs:2 ends here

(provide 'sacha-helm)
;;; sacha-helm.el ends here
