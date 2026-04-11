;;; sacha-misc.el ---  -*- lexical-binding: t -*-

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
;; - Using Embark and qrencode to show a QR code for the Org Mode link at point
;;   https://sachachua.com/dotemacs#embark-qr
;;
;; - Cargo-culted stuff
;;   https://sachachua.com/dotemacs#cargo-culted-stuff
;;
;; - Copy and append string
;;   https://sachachua.com/dotemacs#navigation-copy-and-append-string
;;
;; - Copy text from current PDFview page in other window
;;   https://sachachua.com/dotemacs#pdf-copy
;;
;; - Replace with latest download
;;   https://sachachua.com/dotemacs#navigation-downloaded-files-replace-with-latest-download
;;
;; - C-g improvement
;;   https://sachachua.com/dotemacs#navigation-c-g-improvement
;;
;; - Writing and editing
;;   https://sachachua.com/dotemacs#writing-and-editing
;;
;; - Emacs Lisp and NodeJS: Getting the bolded words from a section of a Google Document
;;   https://sachachua.com/dotemacs#writing-and-editing-learning-french-emacs-lisp-and-nodejs-getting-the-bolded-words-from-a-section-of-a-google-document
;;
;; - Formatting the subtitles into Org Mode subtrees
;;   https://sachachua.com/dotemacs#formatting-the-subtitles-into-org-mode-subtrees
;;
;; - Web development
;;   https://sachachua.com/dotemacs#web-development
;;
;; - Alignment
;;   https://sachachua.com/dotemacs#alignment
;;
;; - Emacs Lisp
;;   https://sachachua.com/dotemacs#emacs-lisp
;;
;; - Other useful functions
;;   https://sachachua.com/dotemacs#coding-emacs-lisp-other-useful-functions
;;
;; - Emacs and my phone
;;   https://sachachua.com/dotemacs#on-sacha-phone
;;
;;; Code:



;; [[file:../Sacha.org::#embark-qr][Using Embark and qrencode to show a QR code for the Org Mode link at point:1]]
;;;###autoload
  (defun sacha-org-link-qr (url)
          "Display a QR code for URL in a buffer."
          (let ((buf (save-window-excursion (qrencode--encode-to-buffer (sacha-org-stored-link-as-url url)))))
                  (if (> (frame-width) 80)
                                  (display-buffer-in-side-window buf '((side . right)))
                          (display-buffer buf))))
;; Using Embark and qrencode to show a QR code for the Org Mode link at point:1 ends here

;; [[file:../Sacha.org::#cargo-culted-stuff][Cargo-culted stuff:1]]
;;;###autoload
  (defun sacha-store-action-key+cmd (cmd)
    (setq keycast--this-command-keys (this-single-command-keys) keycast--this-command cmd))
;;;###autoload
  (defun sacha-force-keycast-update (&rest _)
    (force-mode-line-update t))
;; Cargo-culted stuff:1 ends here

;; [[file:../Sacha.org::#navigation-copy-and-append-string][Copy and append string:1]]
  (defvar sacha-copy-append-string nil "String to append when copying.")
;;;###autoload
  (defun sacha-copy-and-append (beg end append)
          (interactive
           (list
                  (if (region-active-p) (region-beginning) (point-min))
                  (if (region-active-p) (region-end) (point-max))
                  (if current-prefix-arg (read-string "Append: ")
                          sacha-copy-append-string)))
          (when append
                  (setq sacha-copy-append-string append))
          (kill-new (concat (buffer-substring beg end) append)))
;; Copy and append string:1 ends here

;; [[file:../Sacha.org::#pdf-copy][Copy text from current PDFview page in other window:1]]
;;;###autoload
  (defun sacha-pdf-view-insert-current-page-text ()
          (interactive)
          (let (text)
                  (catch 'found
                          (walk-window-tree
                           (lambda (win)
                                   (with-selected-window win
                                           (when (derived-mode-p 'pdf-view-mode)
                                                   (setq text
                                                                           (pdf-info-gettext (pdf-view-current-page)
                                                                                                                                                   (list 0 0 1 1)))
                                                   (throw 'found text))))))
                  (when text (save-excursion (insert text)))))
;; Copy text from current PDFview page in other window:1 ends here

;; [[file:../Sacha.org::#navigation-downloaded-files-replace-with-latest-download][Replace with latest download:1]]
;;;###autoload
  (defun sacha-replace-with-latest-download ()
    "Replace file contents with latest download."
    (interactive)
    (widen)
    (erase-buffer)
    (insert-file-contents (sacha-latest-file sacha-download-dir)))
;; Replace with latest download:1 ends here

;; [[file:../Sacha.org::#navigation-c-g-improvement][C-g improvement:1]]
;;;###autoload
  (defun prot/keyboard-quit-dwim ()
    "Do-What-I-Mean behaviour for a general `keyboard-quit'.

  The generic `keyboard-quit' does not do the expected thing when
  the minibuffer is open.  Whereas we want it to close the
  minibuffer, even without explicitly focusing it.

  The DWIM behaviour of this command is as follows:

  - When the region is active, disable it.
  - When a minibuffer is open, but not focused, close the minibuffer.
  - When the Completions buffer is selected, close it.
  - In every other case use the regular `keyboard-quit'."
    (interactive)
    (cond
     ((region-active-p)
      (keyboard-quit))
     ((derived-mode-p 'completion-list-mode)
      (delete-completion-window))
     ((> (minibuffer-depth) 0)
      (abort-recursive-edit))
     (t
      (keyboard-quit))))

  (define-key global-map (kbd "C-g") #'prot/keyboard-quit-dwim)
;; C-g improvement:1 ends here

;; [[file:../Sacha.org::#writing-and-editing][Writing and editing:2]]
  (defun sacha-capitalize-dwim ()
    "Capitalize the previous word if at the end of a word."
    (interactive)
    (if (region-active-p)
        (capitalize-region (region-beginning) (region-end))
      (when (and (not (bolp))
                 (looking-back "\\w" 1)
                 (not (eq last-command 'sacha-capitalize-dwim)))
        (backward-word))
      (capitalize-word 1)))

  (defun sacha-copy-filename ()
    "Copy the current buffer file name to the clipboard."
    (interactive)
    (cond
     ((derived-mode-p 'dired-mode) (dired-copy-filename-as-kill 0))
     (t (kill-new (buffer-file-name)))))
;; Writing and editing:2 ends here

;; [[file:../Sacha.org::#writing-and-editing-learning-french-emacs-lisp-and-nodejs-getting-the-bolded-words-from-a-section-of-a-google-document][Emacs Lisp and NodeJS: Getting the bolded words from a section of a Google Document:5]]
;;;###autoload
  (defun sacha-split-string-keep-delimiters (string delimiter)
    (when string
      (let (results pos)
        (with-temp-buffer
          (insert string)
          (goto-char (point-min))
          (setq pos (point-min))
          (while (re-search-forward delimiter nil t)
            (push (buffer-substring pos (match-beginning 0)) results)
            (setq pos (match-beginning 0)))
          (push (buffer-substring pos (point-max)) results)
          (nreverse results)))))

  (ert-deftest sacha-split-string-keep-delimiters ()
   (should
    (equal (sacha-split-string-keep-delimiters
            "Beaucoup de gens ont une réaction forte contre l'IA pour plusieurs raisons qui *incluent* le battage médiatique excessif dont elle fait l'objet, son utilisation à mauvais escient, et *l'inondation de banalité* qu'elle produit."
            ", \\| que \\| qui \\| qu'ils? \\| qu'elles? \\| qu'on "
            )
   )))
;; Emacs Lisp and NodeJS: Getting the bolded words from a section of a Google Document:5 ends here

;; [[file:../Sacha.org::#formatting-the-subtitles-into-org-mode-subtrees][Formatting the subtitles into Org Mode subtrees:2]]
;;;###autoload
(defun sacha-file-start-time (filename &optional base-date)
  "Return the local time based on FILENAME."
  (setq filename (file-name-base filename))
  (cond
   ((string-match "^\\([0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]\\)[-T]\\([0-9][0-9][\\.-][0-9][0-9]\\(?:[\\.-][0-9][0-9]\\)?\\)" filename)
    (date-to-time (concat (match-string 1 filename) "T"
                          (replace-regexp-in-string "[\\.-]" ":" (match-string 2 filename)))))
   ((string-match "^\\(?:Copy of \\)?\\([^ ][^ ][^ ]\\)[^ ]+ at \\([0-9]+\\)-\\([0-9]+\\)" filename)
    (let* ((day (match-string 1 filename))
           (hour (match-string 2 filename))
           (min (match-string 3 filename))
           (changed-time (or base-date (file-attribute-modification-time
                                        (file-attributes filename))))
           (decoded-time (decode-time changed-time)))
      ;; get the day on or before changed-time
      (if (string= (format-time-string "%a" changed-time) day)
          (encode-time (append
                        (list
                         0
                         (string-to-number min)
                         (string-to-number hour))
                        (seq-drop decoded-time 3)))
        ;; synchronized maybe within the week after
        (let ((org-read-date-prefer-future nil))
          (org-read-date t t
                         (concat "--" day " " hour ":" min)
                         nil changed-time)))))))

(ert-deftest sacha-file-start-time ()
  (should
   (equal (format-time-string "%Y-%m-%d %H:%M:%S"
                              (sacha-file-start-time "2024-01-05-09-46-59.flv"))
          "2024-01-05 09:46:59"))
  (should
   (equal (format-time-string "%Y-%m-%d %H:%M:%S"
                              (sacha-file-start-time "2024-01-08T12.49.vtt"))
          "2024-01-08 12:49:00"))
  (should
   (equal (format-time-string "%Y-%m-%d %H:%M:%S"
                              (sacha-file-start-time "Sunday at 15-30.vtt"
                                                  (date-to-time "2023-01-12")))
          "2023-01-08 15:30:00"))
  (should
   (time-equal-p (sacha-file-start-time "Sunday at 12-49.txt")
                 (org-read-date t t "-sun 12:49"))))
;; Formatting the subtitles into Org Mode subtrees:2 ends here

;; [[file:../Sacha.org::#web-development][Web development:3]]
(defvar sacha-copy-append "" "String to append.")
;;;###autoload
(defun sacha-copy-and-append (beg end string)
	(interactive (list (if (region-active-p) (region-beginning) (point-min))
										 (if (region-active-p) (region-end) (point-max))
										 (if current-prefix-arg
												 (read-string "Append: ")
											 sacha-copy-append)))
	(setq sacha-copy-append string)
	(kill-new (concat (buffer-substring beg end) string)))
;; Web development:3 ends here

;; [[file:../Sacha.org::#web-development][Web development:4]]
;;;###autoload
(defun sacha-replace-buffer-with-clipboard ()
	(interactive)
	(erase-buffer)
	(insert (car kill-ring)))
;; Web development:4 ends here

;; [[file:../Sacha.org::#alignment][Alignment:1]]
;;;###autoload
(defun sacha-align-non-space (beg end)
  "Align non-space columns in region BEG END."
  (interactive "r")
  (align-regexp beg end "\\(\\s-*\\)\\S-+" 1 1 t))
;; Alignment:1 ends here

;; [[file:../Sacha.org::#emacs-lisp][Emacs Lisp:2]]
;;;###autoload
(defun sacha-set-sentence-end-double-space ()
	(setq-local sentence-end-double-space t))
;; Emacs Lisp:2 ends here

;; [[file:../Sacha.org::#coding-emacs-lisp-other-useful-functions][Other useful functions:1]]
;;;###autoload
(defun sacha-weekly-average (count start end)
  "Report weekly average for COUNT from START to END."
  (/ (* 7.0 count) (days-between end start)))
;; Other useful functions:1 ends here

;; [[file:../Sacha.org::#on-sacha-phone][Emacs and my phone:2]]
;;;###autoload
(defun sacha-format-intent (intent &optional params)
  "Return a command string for sending INTENT with PARAMS.
      PARAMS is an alist of (\"key\" . \"value\") pairs."
  (format "am broadcast --user 0 -a %s %s"
          intent
          (mapconcat
           (lambda (o)
             (format
              "-e %s %s"
              (shell-quote-argument (car o))
              (shell-quote-argument (cdr o))))
           params
           " ")))

;;;###autoload
(defun sacha-send-intent (intent &optional params)
  "Send broadcast INTENT to my phone.
      PARAMS is a plist of :key value pairs."
  (let ((command (sacha-format-intent intent params)))
    (if sacha-phone-p
        (shell-command command)
      (shell-command (format "ssh phone %s" (shell-quote-argument command))))))

;; Emacs and my phone:2 ends here

(provide 'sacha-misc)
;;; sacha-misc.el ends here
