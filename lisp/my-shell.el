;;; my-shell.el ---  -*- lexical-binding: t -*-

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
;; - Scan ~/bin and turn the scripts into interactive commands
;;   https://sachachua.com/dotemacs#scan-bin-and-turn-the-scripts-into-interactive-commands
;;
;; - dwim-shell-command
;;   https://sachachua.com/dotemacs#dwim-shell-command
;;
;; - Automation
;;   https://sachachua.com/dotemacs#automation
;;
;;; Code:



;; [[file:../Sacha.org::#scan-bin-and-turn-the-scripts-into-interactive-commands][Scan ~/bin and turn the scripts into interactive commands:1]]
(require 'dash)
(defmacro my-convert-shell-scripts-to-interactive-commands (directory)
  "Make the shell scripts in DIRECTORY available as interactive commands."
  (cons 'progn
        (-map
         (lambda (filename)
           (let ((function-name (intern (concat "my-shell/" (file-name-nondirectory filename)))))
             `(defun ,function-name (&rest args)
                (interactive)
                (cond
                 ((not (called-interactively-p 'any))
                  (shell-command-to-string (mapconcat 'shell-quote-argument (cons ,filename args) " ")))
                 ((region-active-p)
                  (apply 'call-process-region (point) (mark) ,filename nil (if current-prefix-arg t nil) t args))
                 (t
                  (apply 'call-process ,filename nil (if current-prefix-arg t nil) nil args))))))
         (-filter (-not #'file-directory-p)
                  (-filter #'file-executable-p (directory-files directory t))))))
;; Scan ~/bin and turn the scripts into interactive commands:1 ends here

;; [[file:../Sacha.org::#dwim-shell-command][dwim-shell-command:1]]
;;;###autoload
(defun my-dwim-shell-command (prefix)
  "Execute DWIM shell command asynchronously using noweb templates.

Which files

  `dwim-shell-command' attempts to guess which file(s) you may want
  the command to operate on.

  1. If visiting a `dired' buffer, draw the marked file(s).
  2. If visiting any other buffer with an associated file, use that.

Templates

  Operate on drawn files using either the following:

    <<f>> (file path,used by default)
    <<fne>> (file path without extension)
    <<e>> (extension)
    <<td>> (generate a temporary directory)
    <<*>> (all files joined)
    <<cb>> (clipboard)
    <<n>>, <<1n>>, or <<An>> (for current iteration)

  For example:

    With drawn files '(\"path/to/image1.png\" \"path/to/image2.png\")

   \"convert '<<f>>' '<<fne>>.jpg'\" expands to

     \"convert 'path/to/image1.png' 'path/to/image1.jpg'\"
     \"convert 'path/to/image2.png' 'path/to/image2.jpg'\"

   while \"ls -lh <<*>>\" expands to

     \"ls -lh path/to/image1.png path/to/image2.png\"

Focus

  `dwim-shell-command' creates a process buffer to capture command
  output, but doesn't display or focus on it by default.  Instead,
  it tries to guess what's more convenient to focus on.

  While the process is busy, show a spinner in the minibuffer.  No
  focus changes.

  After process is finished:

  1. If there were any files created in the `default-directory',
  jump to a `dired' buffer and move point to the new file (via
  `dired-jump').

  2. If no new files were created, automatically switch focus to the
  process buffer and display its output.

    Note: You can prevent this automatic focus by prepending your
    command with whitespace.

      |
      V
    \" convert '<<f>>' '<<fne>>.jpg'\"

  3. If the shell command caused any errors, offer to focus the
  process buffer and display its output.

Quick exit

  Process buffers are read-only and can be quickly closed by
  pressing `q'.

Prefix

  With PREFIX, execute command that number of times."
  (interactive "p")
	(require 'dwim-shell-command)
  (let ((script (read-shell-command dwim-shell-command-prompt)))
    (unless (string-match "<<" script) (setq script (concat script " <<f>>")))
    (dwim-shell-command-on-marked-files
     dwim-shell-command-buffer-name script
     :repeat prefix
     :shell-util dwim-shell-command-shell-util
     :shell-args dwim-shell-command-shell-args
     :silent-success (string-prefix-p " " script)
     :error-autofocus (not dwim-shell-command-prompt-on-error))))
;; dwim-shell-command:1 ends here

;; [[file:../Sacha.org::#automation][Automation:1]]
;;;###autoload
(defun my-insert-xdotool-click-as-shell-command ()
  (interactive)
  (insert
   (shell-command-to-string "xdotool getmouselocation | sed -E 's/x:([0-9]+) y:([0-9]+) .*/xdotool mousemove \\1 \\2 click 1/'")))
;; Automation:1 ends here

(provide 'my-shell)
;;; my-shell.el ends here
