;;; sacha-hydra.el ---  -*- lexical-binding: t -*-

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
;; - Hydra keyboard shortcuts
;;   https://sachachua.com/dotemacs#hydras
;;
;; - Emacs Hydra: Allow completion when I can't remember the command name
;;   https://sachachua.com/dotemacs#hydra-completion
;;
;;; Code:



;; [[file:../Sacha.org::#hydras][Hydra keyboard shortcuts:8]]
(defvar hydra-stack nil)

(defun sacha-hydra-push (expr)
  (push `(lambda () ,expr) hydra-stack))

(defun sacha-hydra-pop ()
  (interactive)
  (let ((x (pop hydra-stack)))
    (when x (funcall x))))

(defun sacha-hydra-go-and-push (expr)
  (push hydra-curr-body-fn hydra-stack)
  (prin1 hydra-stack)
  (funcall expr))

;; example (progn (hydra-b/body) (hydra-push '(hydra-a/body)))
;; or   ("q" hydra-pop "exit")
;; Hydra keyboard shortcuts:8 ends here

;; [[file:../Sacha.org::#hydra-completion][Emacs Hydra: Allow completion when I can't remember the command name:1]]
(defun sacha-hydra-format-head (h)
  (let ((key-binding (elt h 0))
        (hint (elt h 2))
        (cmd (and (elt h 1) (prin1-to-string (elt h 1)))))
    (if cmd
        (format "%s (%s) - %s" hint key-binding cmd)
      (format "%s (%s)" hint key-binding))))

(defun sacha-hydra-heads-to-candidates (base)
  (mapcar (lambda (h)
            (cons (sacha-hydra-format-head h) (hydra--head-name h base)))
          (symbol-value (intern (concat (symbol-name base) "/heads")))))

;;;###autoload
(defun sacha-hydra-execute-extended (&optional _ hydra-base)
  (interactive (list current-prefix-arg nil))
  (hydra-keyboard-quit)
  (let* ((candidates (sacha-hydra-heads-to-candidates
                      (or hydra-base
                          (intern
                           (replace-regexp-in-string "/body$" ""
                                                     (symbol-name hydra-curr-body-fn))))))
         (command-name (completing-read "Cmd: " candidates))
         (bind (assoc-default command-name candidates 'string=)))
    (cond
     ((null bind) nil)
     ((hydra--callablep bind) (call-interactively bind)))))
;; Emacs Hydra: Allow completion when I can't remember the command name:1 ends here

(provide 'sacha-hydra)
;;; sacha-hydra.el ends here
