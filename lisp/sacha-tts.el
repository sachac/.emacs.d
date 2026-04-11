;;; sacha-tts.el ---  -*- lexical-binding: t -*-

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



;; [[file:../Sacha.org::#keybindings-foot-pedal][Foot pedal:1]]
  (defun sacha-speechd-speak-sentence-and-advance ()
    "Speak the current sentence and move forward."
    (interactive)
    (call-interactively #'speechd-speak-read-sentence)
    (forward-sentence))

  (defun sacha-speechd-repeat-sentence ()
    "Speak the current sentence and move forward."
    (interactive)
    (backward-sentence)
    (call-interactively #'speechd-speak-read-sentence)
    (forward-sentence))
;; Foot pedal:1 ends here

;; [[file:../Sacha.org::#speech-synthesis-experimental][Speech synthesis (experimental):2]]
(defvar sacha-espeak-command "c:/program files (x86)/espeak/command_line/espeak.exe")
;;;###autoload
(defun sacha-say (string &optional speed)
  (interactive "MString: ")
  (setq speed (or speed 175))
  (call-process sacha-espeak-command nil nil nil string "-s" speed))
;; Speech synthesis (experimental):2 ends here

(provide 'sacha-tts)
;;; sacha-tts.el ends here
