;;; sacha-notmuch.el ---  -*- lexical-binding: t -*-

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
;; - Notmuch
;;   https://sachachua.com/dotemacs#notmuch
;;
;;; Code:



;; [[file:../Sacha.org::#notmuch][Notmuch:2]]
;;;###autoload
(defun sacha-notmuch-flagged ()
  (interactive)
  (notmuch-search "tag:flagged and not tag:trash"))
;;;###autoload
(defun sacha-notmuch-inbox ()
  (interactive)
  (notmuch-search "tag:inbox and not tag:trash"))
;;;###autoload
(defun sacha-notmuch-important-inbox ()
  (interactive)
  (notmuch-search "tag:primary and tag:inbox and not tag:trash"))
;;;###autoload
(defun sacha-notmuch-search-this-author ()
  (interactive)
  (notmuch-search (format "from:\"%s\""
                          (plist-get (get-text-property (point) 'notmuch-search-result) :authors))))
;; Notmuch:2 ends here

(provide 'sacha-notmuch)
;;; sacha-notmuch.el ends here
