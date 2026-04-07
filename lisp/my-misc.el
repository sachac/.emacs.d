;;; my-misc.el ---  -*- lexical-binding: t -*-

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
;; - Web development
;;   https://sachachua.com/dotemacs#web-development
;;
;; - Alignment
;;   https://sachachua.com/dotemacs#alignment
;;
;; - Emacs and my phone
;;   https://sachachua.com/dotemacs#on-my-phone
;;
;;; Code:



;; [[file:../Sacha.org::#embark-qr][Using Embark and qrencode to show a QR code for the Org Mode link at point:1]]
;;;###autoload
  (defun my-org-link-qr (url)
          "Display a QR code for URL in a buffer."
          (let ((buf (save-window-excursion (qrencode--encode-to-buffer (my-org-stored-link-as-url url)))))
                  (if (> (frame-width) 80)
                                  (display-buffer-in-side-window buf '((side . right)))
                          (display-buffer buf))))
;; Using Embark and qrencode to show a QR code for the Org Mode link at point:1 ends here

;; [[file:../Sacha.org::#web-development][Web development:3]]
(defvar my-copy-append "" "String to append.")
;;;###autoload
(defun my-copy-and-append (beg end string)
	(interactive (list (if (region-active-p) (region-beginning) (point-min))
										 (if (region-active-p) (region-end) (point-max))
										 (if current-prefix-arg
												 (read-string "Append: ")
											 my-copy-append)))
	(setq my-copy-append string)
	(kill-new (concat (buffer-substring beg end) string)))
;; Web development:3 ends here

;; [[file:../Sacha.org::#web-development][Web development:4]]
;;;###autoload
(defun my-replace-buffer-with-clipboard ()
	(interactive)
	(erase-buffer)
	(insert (car kill-ring)))
;; Web development:4 ends here

;; [[file:../Sacha.org::#alignment][Alignment:1]]
;;;###autoload
(defun my-align-non-space (beg end)
  "Align non-space columns in region BEG END."
  (interactive "r")
  (align-regexp beg end "\\(\\s-*\\)\\S-+" 1 1 t))
;; Alignment:1 ends here

;; [[file:../Sacha.org::#on-my-phone][Emacs and my phone:2]]
;;;###autoload
(defun my-format-intent (intent &optional params)
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
(defun my-send-intent (intent &optional params)
  "Send broadcast INTENT to my phone.
      PARAMS is a plist of :key value pairs."
  (let ((command (my-format-intent intent params)))
    (if my-phone-p
        (shell-command command)
      (shell-command (format "ssh phone %s" (shell-quote-argument command))))))

;; Emacs and my phone:2 ends here

(provide 'my-misc)
;;; my-misc.el ends here
