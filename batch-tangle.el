;; [[file:Sacha.org::#org-mode-org-babel-tangling-sacha-emacs-config-snippets-to-different-files-and-adding-boilerplate][Org Mode: Tangle Emacs config snippets to different files and add boilerplate:5]]
(setq load-path (cl-remove-if (lambda (p) (string-match-p "lisp/org$" p)) load-path))
(add-to-list 'load-path "~/vendor/org-mode/lisp")
(add-to-list 'load-path "~/vendor/org-mode/contrib/lisp")
(load "~/vendor/org-mode/lisp/org-loaddefs.el" nil t)
(setq user-lisp-directory "~/sync/emacs/lisp")
(add-to-list 'load-path "~/sync/emacs/lisp")
(load "~/sync/emacs/lisp/.user-lisp-autoloads.el" nil t)
(package-initialize)
(setq safe-local-variable-directories '("/home/sacha/sync/orgzly/" "/home/sacha/sync/emacs/"))
(require 'ert)
(setq sacha-exporting t)
;;;###autoload
(defun sacha-dotemacs-link-for-section-at-point (&optional combined)
  "Return the link for the current section."
  (let* ((custom-id (org-entry-get-with-inheritance "CUSTOM_ID"))
         (title (org-entry-get (point) "ITEM"))
         (url (if custom-id
                  (concat "dotemacs:" custom-id)
                (concat sacha-emacs-config-url ":-:text=" (url-hexify-string title)))))
    (if combined
        (org-link-make-string
         url
         title)
      (cons url title))))

(eval-and-compile
  (require 'org-core nil t)
  (require 'org-macs nil t)
  (require 'org-src nil t))
(declare-function org-babel-tangle--compute-targets "ob-tangle")
(defun sacha-org-collect-links-for-tangled-files ()
  "Return a list of ((filename (link link link link)) ...)."
  (let* ((file (buffer-file-name))
         results)
    (org-babel-map-src-blocks (buffer-file-name)
      (let* ((info (org-babel-get-src-block-info))
             (link (sacha-dotemacs-link-for-section-at-point)))
        (mapc
         (lambda (target)
           (let ((list (assoc target results #'string=)))
             (if list
                 (cl-pushnew link (cdr list) :test 'equal)
               (push (list target link) results))))
         (org-babel-tangle--compute-targets file info))))
    ;; Put it back in source order
    (nreverse
     (mapcar (lambda (o)
               (cons (car o)
                     (nreverse (cdr o))))
             results))))
(defvar sacha-emacs-config-module-links nil "Cache for links from tangled files.")

;;;###autoload
(defun sacha-emacs-config-update-module-info ()
  "Update the list of links."
  (interactive)
  (setq sacha-emacs-config-module-links
        (seq-filter
         (lambda (o)
           (string-match "sacha-" (car o)))
         (sacha-org-collect-links-for-tangled-files)))
  (setq sacha-emacs-config-modules-info
        (mapcar (lambda (group)
                  `(,(file-name-base (car group))
                    (commentary
                     .
                     ,(replace-regexp-in-string
                       "^"
                       ";; "
                       (concat
                        "Related Emacs config sections:\n\n"
                        (org-export-string-as
                         (mapconcat
                          (lambda (link)
                            (concat "- " (cdr link) "\\\\\n  " (org-link-make-string (car link)) "\n"))
                          (cdr group)
                          "\n")
                         'ascii
                         t))))))
                sacha-emacs-config-module-links)))

;;;###autoload
(defun sacha-emacs-config-prepare-to-tangle ()
  "Update module info if tangling my config."
  (when (string-match "Sacha.org" (buffer-file-name))
    (sacha-emacs-config-update-module-info)))
(defvar sacha-emacs-config-modules-dir "~/sync/emacs/lisp/")
(defvar sacha-emacs-config-modules-info nil "Alist of module info.")
(defvar sacha-emacs-config-url "https://sachachua.com/dotemacs")

;;;###autoload
(defun sacha-org-babel-post-tangle-insert-boilerplate-for-sacha-lisp ()
  (when (file-in-directory-p (buffer-file-name) sacha-emacs-config-modules-dir)
    (goto-char (point-min))
    (let ((base (file-name-base (buffer-file-name))))
      (insert (format ";;; %s.el --- %s -*- lexical-binding: t -*-

;; Author: %s <%s>
;; URL: %s

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
%s
;;; Code:

\n\n"
                      base
                      (or
                       (assoc-default 'description
                                      (assoc-default base sacha-emacs-config-modules-info #'string=))
                       "")
                      user-full-name
                      user-mail-address
                      sacha-emacs-config-url
                      (or
                       (assoc-default 'commentary
                                      (assoc-default base sacha-emacs-config-modules-info #'string=))
                       "")))
      (goto-char (point-max))
      (insert (format "\n(provide '%s)\n;;; %s.el ends here\n"
                      base
                      base))
      (save-buffer))))
(setq sacha-emacs-config-url "https://sachachua.com/dotemacs")
(with-eval-after-load 'org
  (add-hook 'org-babel-pre-tangle-hook #'sacha-emacs-config-prepare-to-tangle)
  (add-hook 'org-babel-post-tangle-hook #'sacha-org-babel-post-tangle-insert-boilerplate-for-sacha-lisp))
(with-eval-after-load 'org
	(org-link-set-parameters
	 "dotemacs"
	 :complete #'sacha-org-dotemacs-complete
	 :store #'sacha-org-dotemacs-store
	 :insert-description #'sacha-org-dotemacs-insert-description
	 :export #'sacha-org-dotemacs-export
	 :follow #'sacha-org-dotemacs-open))
;; Org Mode: Tangle Emacs config snippets to different files and add boilerplate:5 ends here
