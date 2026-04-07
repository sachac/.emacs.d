;;; my-navigate.el ---  -*- lexical-binding: t -*-

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
;; - Obscure Emacs package appreciation: backup-walker
;;   https://sachachua.com/dotemacs#about-this-file-backups-obscure-emacs-package-appreciation-backup-walker
;;
;; - Killing text
;;   https://sachachua.com/dotemacs#killing-text
;;
;; - Hydra keyboard shortcuts
;;   https://sachachua.com/dotemacs#hydras
;;
;; - Tag files
;;   https://sachachua.com/dotemacs#tag-files
;;
;;; Code:



;; [[file:../Sacha.org::#about-this-file-backups-obscure-emacs-package-appreciation-backup-walker][Obscure Emacs package appreciation: backup-walker:2]]
(defvar backup-walker-data-alist)
(declare-function diff-no-select "diff" (old new &optional switches noasync bufname))
(declare-function backup-walker-get-version "backup-walker")
;;;###autoload
(defun my-backup-walker-refresh ()
  (let* ((index (cdr (assq :index backup-walker-data-alist)))
         (suffixes (cdr (assq :backup-suffix-list backup-walker-data-alist)))
         (prefix (cdr (assq :backup-prefix backup-walker-data-alist)))
         (right-file (concat prefix (nth index suffixes)))
         (right-version (format "%i" (backup-walker-get-version right-file)))
         diff-buf left-file left-version)
    (if (eq index 0)
        (setq left-file (cdr (assq :original-file backup-walker-data-alist))
              left-version "orig")
      (setq left-file (concat prefix (nth (1- index) suffixes))
            left-version (format "%i" (backup-walker-get-version left-file))))
    ;; we change this to go the other way here
    (setq diff-buf (diff-no-select right-file left-file nil 'noasync))
    (setq buffer-read-only nil)
    (delete-region (point-min) (point-max))
    (insert-buffer-substring diff-buf)
    (set-buffer-modified-p nil)
    (setq buffer-read-only t)
    (force-mode-line-update)
    (setq header-line-format
          (concat (format "{{ ~%s~ → ~%s~ }} "
                          (propertize left-version 'face 'font-lock-variable-name-face)
                          (propertize right-version 'face 'font-lock-variable-name-face))
                  (if (nth (1+ index) suffixes)
                      (concat (propertize "<p>" 'face 'italic)
                              " ~"
                              (propertize (int-to-string
                                           (backup-walker-get-version (nth (1+ index) suffixes)))
                                          'face 'font-lock-keyword-face)
                              "~ ")
                    "")
                  (if (eq index 0)
                      ""
                    (concat (propertize "<n>" 'face 'italic)
                            " ~"
                            (propertize (int-to-string (backup-walker-get-version (nth (1- index) suffixes)))
                                        'face 'font-lock-keyword-face)
                            "~ "))
                  (propertize "<return>" 'face 'italic)
                  " open ~"
                  (propertize (propertize (int-to-string (backup-walker-get-version right-file))
                                          'face 'font-lock-keyword-face))
                  "~"))
    (kill-buffer diff-buf)))
;; Obscure Emacs package appreciation: backup-walker:2 ends here

;; [[file:../Sacha.org::#killing-text][Killing text:2]]
;;;###autoload
(defun my-kill-single-line-if-region-is-inactive (beg end &optional region)
  "Wrap around `kill-region' so that we kill a single line."
  (interactive (progn
                 (let ((beg (mark kill-region-dwim))
                       (end (point)))
                   (cond
                    ((and kill-region-dwim (not (use-region-p)))
                     (list beg end kill-region-dwim))
                    ((not (and beg end))
                     (user-error "The mark is not set now, so there is no region"))
                    ((list beg end 'region))))))
  (if (or (region-active-p)
          (derived-mode-p 'minibuffer-mode))
      (kill-region beg end region)
    (kill-region
     (line-beginning-position)
     (line-beginning-position 2))))

(ert-deftest my-kill-single-line-if-region-is-inactive ()
  "Tests `my-kill-single-line-if-region-is-inactive'."
  (should
   (equal
    (with-temp-buffer
      (insert "Hello there\nWorld\n")
      (goto-char (point-min))
      (my-kill-single-line-if-region-is-inactive nil nil)
      (setq text (buffer-string)))
    "World\n")))
;; Killing text:2 ends here

;; [[file:../Sacha.org::#killing-text][Killing text:3]]
;;;###autoload
(defun my-copy-symbol-if-region-is-inactive (beg end &optional region)
  "Wrap around `kill-ring-save' so that we kill a single line."
  (interactive (list (mark) (point) 'region))
  (if (region-active-p)
      (kill-ring-save beg end region)
    (let ((bounds (or (bounds-of-thing-at-point 'symbol)
                      (bounds-of-thing-at-point 'word))))
      (kill-new (filter-buffer-substring (car bounds) (cdr bounds))))))
;; Killing text:3 ends here

;; [[file:../Sacha.org::#hydras][Hydra keyboard shortcuts:6]]
;;;###autoload
(defun my-switch-to-previous-buffer ()
  "Switch to previously open buffer.
        Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))
;; Hydra keyboard shortcuts:6 ends here

;; [[file:../Sacha.org::#tag-files][Tag files:1]]
;;;###autoload
(defun my-recursive-find-file (file &optional directory)
  "Find the first FILE in DIRECTORY or its parents."
  (setq directory (or directory (file-name-directory (buffer-file-name)) (pwd)))
  (if (file-exists-p (expand-file-name file directory))
      (expand-file-name file directory)
    (unless (string= directory "/")
      (my-recursive-find-file file (expand-file-name ".." directory)))))

;;;###autoload
(defun my-find-tags ()
  "Set the TAGS file."
  (set (make-variable-buffer-local 'tags-table-list) nil)
  (set (make-variable-buffer-local 'tags-file-name)
       (my-recursive-find-file "TAGS")))
;; Tag files:1 ends here

(provide 'my-navigate)
;;; my-navigate.el ends here
