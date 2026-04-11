;;; sacha-emacsconf.el ---  -*- lexical-binding: t -*-

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
;; - Prepare for EmacsConf screenshots or recordings
;;   https://sachachua.com/dotemacs#prepare-for-emacsconf-screenshots-or-recordings
;;
;; - EmacsConf
;;   https://sachachua.com/dotemacs#emacsconf
;;
;;; Code:



;; [[file:../Sacha.org::#prepare-for-emacsconf-screenshots-or-recordings][Prepare for EmacsConf screenshots or recordings:1]]
;;;###autoload
(defun sacha-emacsconf-prepare-for-screenshots ()
	(interactive)
	(shell-command "xrandr --output LVDS-1 --mode 1280x720")
	(modus-themes-load-theme 'modus-operandi-tinted)
	(sacha-hl-sexp-update-overlay)
	(set-face-attribute 'default nil :height 170)
	(keycast-header-line-mode))

;;;###autoload
(defun sacha-emacsconf-back-to-normal ()
	(interactive)
	(shell-command "xrandr --output LVDS-1 --mode 1366x768")
	(modus-themes-load-theme (car modus-themes-to-toggle))
	(sacha-hl-sexp-update-overlay)
	(set-face-attribute 'default nil :height 115)
	(keycast-header-line-mode -1))
;; Prepare for EmacsConf screenshots or recordings:1 ends here

;; [[file:../Sacha.org::#emacsconf][EmacsConf:1]]
;;;###autoload
(defun sacha-emacsconf-search-mail (talk)
	(interactive (list (emacsconf-complete-talk)))
	(emacsconf-with-talk-heading talk
		(notmuch-search (format "from:%s or to:%s" (org-entry-get (point) "EMAIL")
														(org-entry-get (point) "EMAIL")))))
;; EmacsConf:1 ends here

(provide 'sacha-emacsconf)
;;; sacha-emacsconf.el ends here
