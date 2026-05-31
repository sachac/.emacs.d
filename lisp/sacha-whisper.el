;;; sacha-whisper.el ---  -*- lexical-binding: t -*-

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
;; - Expanding yasnippets by voice in Emacs and other applications
;;   https://sachachua.com/dotemacs#writing-and-editing-speech-recognition-expanding-yasnippet-by-voice
;;
;;; Code:



;; [[file:../Sacha.org::#writing-and-editing-speech-recognition-expanding-yasnippet-by-voice][Expanding yasnippets by voice in Emacs and other applications:1]]

(declare-function subed-word-data-compare-normalized-string-distance "subed-word-data")

;;;###autoload
(defun sacha-whisper-maybe-expand-snippet (text)
  "Add to `whisper-insert-text-at-point'."
  (if (and text
           (string-match
            "^ok\\(?:ay\\)?[,\\.]? \\(.+\\)" text))
    (let* ((name
            (downcase
             (string-trim
              (replace-regexp-in-string "[,\\.]" "" (match-string 1 text)))))
           (matching
            (seq-find (lambda (o)
                        (subed-word-data-compare-normalized-string-distance
                         name
                         (downcase (yas--template-name o))))
                      (yas--all-templates (yas--get-snippet-tables)))))
      (if matching
          (progn
            (if (frame-focus-state)
                (progn
                  (yas-expand-snippet matching)
                  nil)
              ;; In another application
              (with-temp-buffer
                (yas-minor-mode)
                (yas-expand-snippet matching)
                (buffer-string))))
        text))
    text))
;; Expanding yasnippets by voice in Emacs and other applications:1 ends here

(provide 'sacha-whisper)
;;; sacha-whisper.el ends here
