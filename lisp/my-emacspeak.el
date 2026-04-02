;;; my-emacspeak.el ---  -*- lexical-binding: t -*-

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
;; - Emacspeak
;;   https://sachachua.com/dotemacs#emacspeak
;;
;;; Code:



;; [[file:../Sacha.org::#emacspeak][Emacspeak:2]]
;;;###autoload
  (defun my-emacspeak ()
    (interactive)
    (load-file "/home/sacha/vendor/emacspeak/lisp/emacspeak-setup.el")
    (setq emacspeak-use-auditory-icons t)
    (setq-default emacspeak-use-auditory-icons t)
    (setq-default dtk-quiet nil)
    (setq dtk-quiet nil))

;;;###autoload
  (defun my-emacspeak-quiet ()
    (interactive)
    (setq emacspeak-use-auditory-icons nil)
    (setq-default emacspeak-use-auditory-icons nil)
    (setq-default dtk-quiet t)
    (setq dtk-quiet t)
		(dtk-interp-sync)
		(ad-disable-regexp "emacspeak"))
;; Emacspeak:2 ends here

(provide 'my-emacspeak)
;;; my-emacspeak.el ends here
