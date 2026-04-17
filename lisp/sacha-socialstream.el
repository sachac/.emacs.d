;;; sacha-socialstream.el ---  -*- lexical-binding: t -*-

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
;; - Comment overlay via socialstream.ninja
;;   https://sachachua.com/dotemacs#streaming-comment-overlay-via-socialstream-ninja
;;
;;; Code:



;; [[file:../Sacha.org::#streaming-comment-overlay-via-socialstream-ninja][Comment overlay via socialstream.ninja:1]]
(defvar sacha-socialstream-session nil "Session code (not the full URL).")

;;;###autoload
(defun sacha-socialstream-send-message (text)
  "Send TEXT."
  (interactive "MText: ")
	(url-retrieve
	 (format "https://io.socialstream.ninja/%s/sendChat/null/%s"
					 sacha-socialstream-session
					 (url-hexify-string text))
	 #'ignore))

;; Comment overlay via socialstream.ninja:1 ends here

(provide 'sacha-socialstream)
;;; sacha-socialstream.el ends here
