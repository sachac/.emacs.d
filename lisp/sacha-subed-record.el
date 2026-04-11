;;; sacha-subed-record.el ---  -*- lexical-binding: t -*-

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
;; - Add shadowing with tts to subed-record
;;   https://sachachua.com/dotemacs#writing-and-editing-learning-french-add-shadowing-with-tts-to-subed-record
;;
;; - Make it easy to add reference links
;;   https://sachachua.com/dotemacs#writing-and-editing-learning-french-make-it-easy-to-add-reference-links
;;
;;; Code:



;; [[file:../Sacha.org::#writing-and-editing-learning-french-add-shadowing-with-tts-to-subed-record][Add shadowing with tts to subed-record:2]]
;;;###autoload
  (defun sacha-subed-record-normalize-current (file)
    (interactive (list (subed-media-file)))
    (let ((temp-file (make-temp-file file nil (concat "." (file-name-extension file)))))
      (make-process
       :name "normalize"
       :buffer (get-buffer-create "*normalize*")
       :command (list
                 (expand-file-name "~/bin/normalize")
                 (expand-file-name file)
                 temp-file)
       :sentinel
       (lambda (process event)
         (when (string-match "finished" event)
           (rename-file temp-file file t)
           (message "Normalized %s" file))))))
;; Add shadowing with tts to subed-record:2 ends here

;; [[file:../Sacha.org::#writing-and-editing-learning-french-make-it-easy-to-add-reference-links][Make it easy to add reference links:1]]
  (defvar-local sacha-subed-record-references nil)
;;;###autoload
  (defun sacha-subed-record-load-references (file &optional skip-insert)
    "Load the references from FILE (media)."
    (interactive (list (read-file-name "Media file: ")
                       current-prefix-arg))
    (dolist (cue (subed-parse-file (concat (file-name-sans-extension file) ".vtt")))
      (push
       (list
        (learn-lang-subed-record-simplify (elt cue 3))
        file
        (elt cue 1)
        (elt cue 2))
       sacha-subed-record-references))
    (unless skip-insert
      (sacha-subed-insert-references)))

;;;###autoload
  (defun sacha-subed-record-insert-reference ()
    (interactive)
    (when-let* ((rec (alist-get (learn-lang-subed-record-simplify (subed-subtitle-text))
                               sacha-subed-record-references
                               nil nil #'string=)))
      (subed-record-set-directive
       "#+REFERENCE"
       (format "%s %s --> %s"
               (elt rec 0)
               (subed-msecs-to-timestamp (elt rec 1))
               (subed-msecs-to-timestamp (elt rec 2))))))

;;;###autoload
  (defun sacha-subed-insert-references ()
    (interactive)
    (subed-for-each-subtitle (point-min) (point-max) t
      (sacha-subed-record-insert-reference)))
;; Make it easy to add reference links:1 ends here

(provide 'sacha-subed-record)
;;; sacha-subed-record.el ends here
