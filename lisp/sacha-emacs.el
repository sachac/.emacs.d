;;; sacha-emacs.el ---  -*- lexical-binding: t -*-

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
;; - Reload
;;   https://sachachua.com/dotemacs#reload
;;
;; - Repeatable commands
;;   https://sachachua.com/dotemacs#repeatable-commands
;;
;; - which-key and which-key-posframe
;;   https://sachachua.com/dotemacs#which-key-and-which-key-posframe
;;
;; - Make it easier to split my literate config into files
;;   https://sachachua.com/dotemacs#org-mode-org-babel-make-it-easier-to-split-sacha-literate-config-into-files
;;
;; - Org Mode: Tangle Emacs config snippets to different files and add boilerplate
;;   https://sachachua.com/dotemacs#org-mode-org-babel-tangling-sacha-emacs-config-snippets-to-different-files-and-adding-boilerplate
;;
;; - Key chords
;;   https://sachachua.com/dotemacs#key-chord
;;
;;; Code:



;; [[file:../Sacha.org::#reload][Reload:1]]
;;;###autoload
(defun sacha-reload-emacs-configuration ()
  (interactive)
  (load-file "~/proj/.emacs.d/Sacha.el"))
;; Reload:1 ends here

;; [[file:../Sacha.org::#repeatable-commands][Repeatable commands:1]]
;;;###autoload
(defun sacha-def-rep-command (alist)
  "Return a lambda that calls the first function of ALIST.
        It sets the transient map to all functions of ALIST,
        allowing you to repeat those functions as needed."
  (let ((keymap (make-sparse-keymap))
        (func (cdar alist)))
    (mapc (lambda (x)
            (when x
              (define-key keymap (kbd (car x)) (cdr x))))
          alist)
    (lambda (arg)
      (interactive "p")
      (when func
        (funcall func arg))
      (set-transient-map keymap t))))
;; Repeatable commands:1 ends here

;; [[file:../Sacha.org::#which-key-and-which-key-posframe][which-key and which-key-posframe:2]]
;;;###autoload
(defun sacha-reset-transients ()
  (interactive)
  (setq overriding-terminal-local-map nil))
;; which-key and which-key-posframe:2 ends here

;; [[file:../Sacha.org::#org-mode-org-babel-make-it-easier-to-split-sacha-literate-config-into-files][Make it easier to split my literate config into files:1]]
;;;###autoload
(defun sacha-emacs-suggest-file ()
  (let ((elem (org-element-context)))
    (seq-find (lambda (o)
                (save-excursion
                  (goto-char (org-element-begin elem))
                  (re-search-forward
                   (concat "^ *(\\(cl-\\)?defun *"
                           (regexp-quote (file-name-base o)))
                   (org-element-end elem) t)))
              (directory-files "lisp/" t "\\.el"))))

;;;###autoload
(defun sacha-emacs-split-into-file (filename)
  "Prepare this block for splitting into FILENAME."
  (interactive (list
                (let* ((default (sacha-emacs-suggest-file))
                       (relative
                        (and default
                             (file-relative-name
                              default
                              (file-name-directory (buffer-file-name))))))
                  (if default
                      (read-file-name
                       (format "File (%s): " relative)
                       "lisp/"
                       relative)
                    (read-file-name "File: " "lisp/")))))
  (let ((elem (org-element-context)))
    (save-restriction
      (narrow-to-region (org-element-begin elem)
                        (org-element-end elem))
      (goto-char (point-min))
      (forward-line)
      (while (re-search-forward "^ *(\\(cl-defun\\|defun\\|define-minor-mode\\|define-derived-mode\\) " nil t)
        (unless (save-match-data
                  (save-excursion
                    (forward-line -1)
                    (looking-at ";;;###autoload\n")))
          (replace-match
           (concat ";;;###autoload\n"
                   (match-string 0)))))

      (goto-char (point-min))
      (forward-line)
      (when (re-search-forward "^(\\(setq\\|use-package\\|with-eval-after-load\\|bind-key\\|keymap-set\\|keymap-global-set\\)" nil t)
        (goto-char (match-beginning 0))
        (sacha-org-demarcate-block))
      ;; Add the tangle
      (goto-char (point-min))
      (unless (looking-at "#\\+begin_src")
        (re-search-forward "#\\+begin_src" nil t))
      (unless (save-excursion (re-search-forward ":tangle" nil (line-end-position)))
        (goto-char (line-end-position))
        (insert " :tangle " (file-relative-name filename (file-name-directory (buffer-file-name))))))))
;; Make it easier to split my literate config into files:1 ends here

;; [[file:../Sacha.org::#org-mode-org-babel-tangling-sacha-emacs-config-snippets-to-different-files-and-adding-boilerplate][Org Mode: Tangle Emacs config snippets to different files and add boilerplate:2]]
(defvar sacha-dotemacs-url "https://sachachua.com/dotemacs/")

;;;###autoload
(defun sacha-dotemacs-link-for-section-at-point (&optional combined)
  "Return the link for the current section."
  (let* ((custom-id (org-entry-get-with-inheritance "CUSTOM_ID"))
         (title (org-entry-get (point) "ITEM"))
         (url (if custom-id
                  (concat "dotemacs:" custom-id)
                (concat sacha-dotemacs-url ":-:text=" (url-hexify-string title)))))
    (if combined
        (org-link-make-string
         url
         title)
      (cons url title))))

(eval-and-compile
  (require 'org-core nil t)
  (require 'org-macs nil t)
  (require 'org-src nil t))
(declare-function 'org-babel-tangle--compute-targets "ob-tangle")
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
;; Org Mode: Tangle Emacs config snippets to different files and add boilerplate:2 ends here

;; [[file:../Sacha.org::#key-chord][Key chords:1]]
;;;###autoload
  (defun sacha-key-chord-define (keymap keys command)
    "Define in KEYMAP, a key-chord of two keys in KEYS starting a COMMAND.
        \nKEYS can be a string or a vector of two elements. Currently only elements
        that corresponds to ascii codes in the range 32 to 126 can be used.
        \nCOMMAND can be an interactive function, a string, or nil.
        If COMMAND is nil, the key-chord is removed.

        MODIFICATION: Do not define the transposed key chord.
        "
    (if (/= 2 (length keys))
        (error "Key-chord keys must have two elements"))
    ;; Exotic chars in a string are >255 but define-key wants 128..255 for those
    (let ((key1 (logand 255 (aref keys 0)))
          (key2 (logand 255 (aref keys 1))))
      (define-key keymap (vector 'key-chord key1 key2) command)))
;; Key chords:1 ends here

(provide 'sacha-emacs)
;;; sacha-emacs.el ends here
