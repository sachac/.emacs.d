;;; sacha-google.el ---  -*- lexical-binding: t -*-

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
;; - Emacs Lisp and NodeJS: Getting the bolded words from a section of a Google Document
;;   https://sachachua.com/dotemacs#writing-and-editing-learning-french-emacs-lisp-and-nodejs-getting-the-bolded-words-from-a-section-of-a-google-document
;;
;;; Code:



;; [[file:../Sacha.org::#writing-and-editing-learning-french-emacs-lisp-and-nodejs-getting-the-bolded-words-from-a-section-of-a-google-document][Emacs Lisp and NodeJS: Getting the bolded words from a section of a Google Document:1]]
  (defvar sacha-google-doc-download-command
    (list "nodejs" (expand-file-name "~/bin/download-google-doc-html.cjs")))

;;;###autoload
  (defun sacha-google-doc-html (doc-id)
    (when (string-match "https://docs\\.google\\.com/document/d/\\(.+?\\)/" doc-id)
      (setq doc-id (match-string 1 doc-id)))
    (with-temp-buffer
      (apply #'call-process (car sacha-google-doc-download-command)
             nil t nil (append (cdr sacha-google-doc-download-command) (list doc-id)))
      (buffer-string)))

(require 'dom)
;;;###autoload
(defun sacha-google-doc-clean-html (html)
  "Remove links on spaces, replace Google links."
  (let ((dom (with-temp-buffer
               (insert html)
               (libxml-parse-html-region))))
    (dom-search
     dom
     (lambda (o)
       (when (eq (dom-tag o) 'a)
         (when (and (dom-attr o 'href)
                    (string-match "https://\\(www\\.\\)?google\\.com/url\\?q=" (dom-attr o 'href)))
           (let* ((parsed (url-path-and-query
                           (url-generic-parse-url (dom-attr o 'href))))
                  (params (url-parse-query-string (cdr parsed))))
             (dom-set-attribute o 'href (car (assoc-default "q" params #'string=)))))
         (let ((text (string= (string-trim (dom-text o)) "")))
           (when (string= text "")
             (setf (car o) 'span))))
       (when (and
              (string-match "font-weight:700" (or (dom-attr o 'style) ""))
              (not (string-match "font-style:normal" (or (dom-attr o 'style) ""))))
         (setf (car o) 'strong))
       (when (dom-attr o 'style)
         (dom-remove-attribute o 'style))))
    ;; bold text is actually represented as font-weight:700 instead
    (with-temp-buffer
      (svg-print dom)
      (buffer-string))))

;;;###autoload
  (defun sacha-google-doc-org (doc-id)
    "Return DOC-ID in Org Mode format."
    (pandoc-convert-stdio (sacha-google-doc-clean-html (sacha-google-doc-html doc-id)) "html" "org"))
;; Emacs Lisp and NodeJS: Getting the bolded words from a section of a Google Document:1 ends here

(provide 'sacha-google)
;;; sacha-google.el ends here
