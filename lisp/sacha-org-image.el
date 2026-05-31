;;; sacha-org-image.el ---  -*- lexical-binding: t -*-

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
;; - Org Mode sketch: links
;;   https://sachachua.com/dotemacs#org-mode-sketch-links
;;
;;; Code:



;; [[file:../Sacha.org::org-sketch-link][org-sketch-link]]
;;;###autoload
(defun sacha-open-images-in-krita (files)
  (apply 'call-process "krita" nil 0 nil "--nosplash" files))
;;;###autoload
(defun sacha-open-images-in-gwenview (files)
  (apply 'call-process "gwenview" nil 0 nil "--slideshow" files))
;;;###autoload
(defun sacha-open-images-in-feh (files)
  (apply 'call-process "feh" nil nil nil "-D" "1" "-F" files))
;;;###autoload
(defun sacha-org-image-open (id &optional arg directories)
  "Open image named ID.
      If ARG is specified, prompt for application to open it in."
  (interactive (list
                (completing-read "Sketch ID: " (sacha-list-sketches "."))
                (current-prefix-arg)))
  (let* ((files (mapcar (lambda (o) (sacha-get-image-filename o (or sacha-image-directories))) (if (listp id) id (list id))))
         (input (if arg (read-char "(k)rita, (g)wenview, (f)eh: ") ?k)))
    (funcall
     (cond
      ((eq input ?g) 'sacha-open-images-in-gwenview)
      ((eq input ?f) 'sacha-open-images-in-feh)
      (t 'sacha-open-images-in-krita))
     files)))
;;;###autoload
(defun sacha-org-sketch-edit (id &optional arg)
  (sacha-org-image-open id arg sacha-sketch-directories))
;;;###autoload
(defun sacha-org-sketch-open (id &optional arg)
  (delete-other-windows)
  (with-selected-window (split-window-right)
    (find-file (sacha-get-image-filename
								id sacha-sketch-directories))))
;;;###autoload
(defun sacha-org-image-export (link description format info)
  (let* ((path (concat "https://sketches.sachachua.com/filename/" link))
         (image (concat "https://sketches.sachachua.com/static/" link))
         (backend (org-export-backend-name (plist-get info :back-end)))
         (desc (or description link)))
    (cond
     ((eq backend '11ty) (format "{%% sketchLink \"%s\", \"%s\" %%}" link desc))
     ((or (eq format 'html) (eq format 'wp))
      (if description
          (format "<a target=\"_blank\" href=\"%s\">%s</a>" path desc)
        (format "<div style=\"text-align: center\"><a target=\"_blank\" href=\"%s\"><img src=\"%s\" style=\"max-height: 90vw; height: auto; width: auto\"><br />%s</a></div>" path image desc)))
     ((eq format 'latex) (format "\\href{%s}{%s}" path desc))
     ((eq format 'texinfo) (format "@uref{%s,%s}" path desc))
     ((eq format 'md)
      (if (file-exists-p (expand-file-name link "~/sketches"))
          (format "{{<photo src=\"%s\">}}" image)
        (format "{{<photo nas=\"1\" src=\"%s\">}}" link)))
     ((eq format 'ascii) (format "%s <%s>" desc path))
     (t path))))

;;;###autoload
(defun sacha-org-image-export-link (link description format info)
  (let* ((backend (if (plist-get info :backend) (org-export-backend-name (plist-get info :back-end))
                    format))
         (desc (or description link)))
    (cond ((eq backend 'md)
           (format "[%s](%s)" desc link))
          ((eq backend '11ty)
           (format "{%% sketchLink \"%s\", \"%s\" %%}"
                   (replace-regexp-in-string "\"" "\\\"" (file-name-base link) nil t)
                   (replace-regexp-in-string "\"" "\\\"" desc nil t)))
          ((eq backend 'html)
           (format "<a href=\"https://sketches.sachachua.com/filename/%s\">%s</a>" (file-name-nondirectory link) desc))
          (t (format "[[%s][%s]]" link desc)))))

;;;###autoload
(defun sacha-org-image-export-thumb (link description format info)
  (let* ((path (concat "https://sketches.sachachua.com/filename/" link))
         (image (concat "https://sketches.sachachua.com/static/" link))
         (backend (org-export-backend-name (plist-get info :back-end)))
         (desc (replace-regexp-in-string "%23" "#" (or description link))))
    (cond
     ((eq backend '11ty) (format "{%% sketchThumb \"%s\", \"%s\" %%}" (file-name-base link) desc))
     ((or (eq format 'html) (eq format 'wp))
      (format "<div class=\"sketch-thumbnail\"><a target=\"_blank\" href=\"%s\"><img src=\"%s\"><br />%s</a></div>" path image desc))
     ((eq format 'latex) (format "\\href{%s}{%s}" path desc))
     ((eq format 'texinfo) (format "@uref{%s,%s}" path desc))
     ((eq format 'md)
      (if (file-exists-p (expand-file-name link "~/sketches"))
          (format "{{<photo src=\"%s\">}}" image)
        (format "{{<photo nas=\"1\" src=\"%s\">}}" link)))
     ((eq format 'ascii) (format "%s <%s>" desc path))
     (t path))))

;;;###autoload
(defun sacha-org-image-export-full (link description format info)
  (let* ((path (concat "https://sketches.sachachua.com/filename/" link))
         (image (concat "https://sketches.sachachua.com/static/" link))
         (backend (org-export-backend-name (plist-get info :back-end)))
         (desc (or description link)))
    (cond
     ((eq backend '11ty) (format "{%% sketchFull \"%s\", \"%s\" %%}" link desc))
     ((or (eq format 'html) (eq format 'wp))
      (if description
          (format "<figure><a target=\"_blank\" href=\"%s\"><img src=\"%s\" /><br /></a><figcaption>%s</figcaption></figure>" path image desc)
        (format "<figure><a target=\"_blank\" href=\"%s\"><img src=\"%s\" /><br /><figcaption>%s</figcaption></a></figure>" path image desc)))
     ((eq format 'latex)
			(format "\\href{\\includegraphics{%s}}{%s}"
							path desc))
     ((eq format 'texinfo) (format "@uref{%s,%s}" path desc))
     ((eq format 'md)
      (if (file-exists-p (expand-file-name link "~/sketches"))
          (format "{{<photo src=\"%s\">}}" image)
        (format "{{<photo nas=\"1\" src=\"%s\">}}" link)))
     ((eq format 'ascii) (format "%s <%s>" desc path))
     (t path))))

;;;###autoload
(defun sacha-org-sketch-complete (&optional prefix)
  (concat "sketch:" (file-name-nondirectory (sacha-complete-sketch-filename))))
;;;###autoload
(defun sacha-org-sketch-complete-full (&optional prefix)
  (concat "sketchFull:" (file-name-nondirectory (sacha-complete-sketch-filename))))
;;;###autoload
(defun sacha-org-image-complete (&optional prefix)
  (concat "image:"
          (completing-read "Image: " (sacha-list-sketches "." nil sacha-image-directories))))
;; Based on https://emacs.stackexchange.com/questions/38098/org-mode-custom-youtube-link-syntax
;;;###autoload
(defun sacha-org-sketch-preview (start end path bracketp)
  "Include overlays for sketches."
  (when (display-graphic-p)
    (let ((filename (sacha-get-sketch-filename path))
          (refresh nil)
          (link (save-excursion
                  (goto-char start)
                  (org-element-lineage
                   (save-match-data (org-element-context))
                   '(link) t)))) ;; set this someday
      (when (and (not (org-element-property :contents-begin link)) filename)
        (let ((width
               ;; Apply `org-image-actual-width' specifications.
               (cond
                ((not (image-type-available-p 'imagemagick)) nil)
                ((eq org-image-actual-width t) nil)
                ((numberp org-image-actual-width) org-image-actual-width)
                ;; Pick this up from the paragraph someday
                ))
              (old (get-char-property-and-overlay start 'org-image-overlay)))
          (if (and (car-safe old) refresh)
              (image-refresh (overlay-get (cdr old) 'display))
            (let ((image (create-image filename
                                       (and width 'imagemagick)
                                       nil
                                       :width width)))
              (when image
                (let* ((ov (make-overlay start end)))
                  (overlay-put ov 'display image)
                  (overlay-put ov 'face 'default)
                  (overlay-put ov 'org-image-overlay t)
                  (overlay-put ov 'evaporate t)
                  (overlay-put
                   ov 'modification-hooks
                   (list 'org-display-inline-remove-overlay))
                  (push ov org-inline-image-overlays))))))))))

;; org-sketch-link ends here

(provide 'sacha-org-image)
;;; sacha-org-image.el ends here
