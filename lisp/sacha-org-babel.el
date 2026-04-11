;;; sacha-org-babel.el ---  -*- lexical-binding: t -*-

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



;; [[file:../Sacha.org::#org-mode-org-babel-tangling-sacha-emacs-config-snippets-to-different-files-and-adding-boilerplate][Tangle Emacs config snippets to different files and add boilerplate:3]]
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
;; Tangle Emacs config snippets to different files and add boilerplate:3 ends here

;; [[file:../Sacha.org::#org-mode-org-babel-detangle-just-the-current-block][Org Babel: Detangle just the current block:1]]
;;;###autoload
(defun sacha-org-babel-detangle-current-block ()
  "Detangle just the current block."
  (interactive)
  (save-restriction
    (sacha-org-babel-detangle-narrow-to-block)
    (org-babel-detangle)))

;;;###autoload
(defun sacha-org-babel-detangle-narrow-to-block ()
  "Narrow to just the current block."
  (interactive)
  (let ((start (if (save-excursion (re-search-backward (concat "^;; " org-link-bracket-re) nil t))
                   (match-beginning 0)
                 (point-min)))
        (end (if (save-excursion (re-search-forward (concat "^;; " org-link-bracket-re) nil t))
                 (match-end 0)
               (point-max))))
    (narrow-to-region start end)))
;; Org Babel: Detangle just the current block:1 ends here

;; [[file:../Sacha.org::#execute-subtree-by-custom-id][Run source blocks in an Org Mode subtree by custom ID:1]]
;;;###autoload
(defun sacha-org-execute-subtree-by-custom-id (id &optional filename)
	"Prompt for a CUSTOM_ID value and execute the subtree with that ID.
If called with \\[universal-argument], prompt for a file, and then prompt for the ID."
  (interactive (if current-prefix-arg
									 (let ((file (read-file-name "Filename: ")))
										 (list
											(with-current-buffer (find-file-noselect file)
												(completing-read
												 "Custom ID: "
												 (org-property-values "CUSTOM_ID")))
											file))
								 (list
									(completing-read "Custom ID: " (org-property-values "CUSTOM_ID")))))
	(with-current-buffer (if filename (find-file-noselect filename) (current-buffer))
		(let ((pos (org-find-property "CUSTOM_ID" id)))
			(if pos
					(org-babel-execute-subtree)
				(if filename(error "Could not find %s in %s" id filename)
					(error "Could not find %s" id))))))
;; Run source blocks in an Org Mode subtree by custom ID:1 ends here

;; [[file:../Sacha.org::test][test]]
;;;###autoload
(defun sacha-org-execute-src-block-by-name (name)
  (interactive (list (completing-read "Block: "(org-babel-src-block-names))))
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (format "^#\\+NAME:[ \t]+%s[ \t]*$" (regexp-quote name)) nil t)
      (org-babel-execute-src-block))))
;; test ends here

;; [[file:../Sacha.org::#json][JSON:1]]
;;;###autoload
(defun sacha-org-babel-execute:json (body params)
  (let ((jq (cdr (assoc :jq params)))
        (node (cdr (assoc :node params))))
    (cond
     (jq
      (with-temp-buffer
        ;; Insert the JSON into the temp buffer
        (insert body)
        ;; Run jq command on the whole buffer, and replace the buffer
        ;; contents with the result returned from jq
        (shell-command-on-region (point-min) (point-max) (format "jq -r \"%s\"" jq) nil 't)
        ;; Return the contents of the temp buffer as the result
        (buffer-string)))
     (node
      (with-temp-buffer
        (insert (format "const it = %s;" body))
        (insert node)
        (shell-command-on-region (point-min) (point-max) "node -p" nil 't)
        (buffer-string))))))
;; JSON:1 ends here

;; [[file:../Sacha.org::#org-block-indentation][Fix block indentation:1]]
;;;###autoload
(defun sacha-org-fix-block-indentation ()
	"Fix the indentation of the current src block."
	(interactive)
	(org-edit-special)
	(indent-region (point-min) (point-max))
	(org-edit-src-exit))
;; Fix block indentation:1 ends here

(provide 'sacha-org-babel)
;;; sacha-org-babel.el ends here
