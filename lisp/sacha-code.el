;;; sacha-code.el ---  -*- lexical-binding: t -*-

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
;; - Format source
;;   https://sachachua.com/dotemacs#format-source
;;
;; - Python
;;   https://sachachua.com/dotemacs#python
;;
;; - Web development
;;   https://sachachua.com/dotemacs#web-development
;;
;; - LSP
;;   https://sachachua.com/dotemacs#lsp
;;
;; - Javascript
;;   https://sachachua.com/dotemacs#javascript
;;
;; - HTML
;;   https://sachachua.com/dotemacs#html
;;
;; - Projects and projectile
;;   https://sachachua.com/dotemacs#projects-and-projectile
;;
;; - SQLite
;;   https://sachachua.com/dotemacs#coding-sqlite
;;
;;; Code:



;; [[file:../Sacha.org::#format-source][Format source:1]]
;;;###autoload
(defun sacha-format-all-advice ()
  (ignore-errors               ; in case there's no language support
    (format-all-buffer)))
;; Format source:1 ends here

;; [[file:../Sacha.org::#python][Python:2]]
;;;###autoload
(defun colorize-compilation-buffer ()
  (when (eq major-mode 'compilation-mode)
		(let ((inhibit-read-only t))
			(ansi-color-apply-on-region compilation-filter-start (point-max)))))
;; Python:2 ends here

;; [[file:../Sacha.org::#web-development][Web development:2]]
;;;###autoload
(defun themkat/activate-tide ()
  (interactive)
  (tide-setup)
  (eldoc-mode 1)
  (tide-hl-identifier-mode 1))

;;;###autoload
(defun themkat/complete-web-mode ()
  (interactive)
  (let ((current-scope (web-mode-language-at-pos (point))))
    (cond ((string-equal "javascript" current-scope)
	   (company-tide 'interactive))
	  ((string-equal "css" current-scope)
	   (company-css 'interactive))
	  (t
	   (company-dabbrev-code 'interactive)))))

;;;###autoload
(defun themkat/eldoc-web-mode ()
  (let ((current-scope (web-mode-language-at-pos (point))))
    (cond ((string-equal "javascript" current-scope)
	   (tide-eldoc-function))
	  ((string-equal "css" current-scope)
	   (css-eldoc-function))
	  (t
	   nil))))
;;;###autoload
(defun themkat/setup-web-mode-mixed ()
  (web-mode)
  (themkat/activate-tide)
  (setq-local eldoc-documentation-function #'themkat/eldoc-web-mode))
;; Web development:2 ends here

;; [[file:../Sacha.org::#web-development][Web development:5]]
;; from FAQ at http://web-mode.org/ for smartparens
;;;###autoload
(defun sacha-sp-web-mode-is-code-context (id action context)
  (when (and (eq action 'insert)
             (not (or (get-text-property (point) 'part-side)
                      (get-text-property (point) 'block-side))))
    t))
;; Web development:5 ends here

;; [[file:../Sacha.org::#lsp][LSP:1]]
;;;###autoload
(defun sacha-local-lsp ()
  (unless (file-remote-p default-directory)
    (lsp)))
;; LSP:1 ends here

;; [[file:../Sacha.org::#javascript][Javascript:4]]
(defvar sacha-javascript-test-regexp (concat (regexp-quote "/** Testing **/") "\\(.*\n\\)*")
  "Regular expression matching testing-related code to remove.
      See `sacha-copy-javascript-region-or-buffer'.")

;;;###autoload
(defun sacha-copy-javascript-region-or-buffer (beg end)
  "Copy the active region or the buffer, wrapping it in script tags.
      Add a comment with the current filename and skip test-related
      code. See `sacha-javascript-test-regexp' to change the way
      test-related code is detected."
  (interactive "r")
  (unless (region-active-p)
    (setq beg (point-min) end (point-max)))
  (kill-new
   (concat
    "<script type=\"text/javascript\">\n"
    (if (buffer-file-name) (concat "// " (file-name-nondirectory (buffer-file-name)) "\n") "")
    (replace-regexp-in-string
     sacha-javascript-test-regexp
     ""
     (buffer-substring (point-min) (point-max))
     nil)
    "\n</script>")))
;; Javascript:4 ends here

;; [[file:../Sacha.org::#javascript][Javascript:5]]
(defvar sacha-debug-counter 1)
;;;###autoload
(defun sacha-insert-or-flush-debug (&optional reset beg end)
  (interactive "pr")
  (cond
   ((= reset 4)
    (save-excursion
      (flush-lines "console.log('DEBUG: [0-9]+" (point-min) (point-max))
      (setq sacha-debug-counter 1)))
   ((region-active-p)
    (save-excursion
      (goto-char end)
      (insert ");\n")
      (goto-char beg)
      (insert (format "console.log('DEBUG: %d', " sacha-debug-counter))
      (setq sacha-debug-counter (1+ sacha-debug-counter))
      (js2-indent-line)))
   (t
    ;; Wrap the region in the debug
    (insert (format "console.log('DEBUG: %d');\n" sacha-debug-counter))
    (setq sacha-debug-counter (1+ sacha-debug-counter))
    (backward-char 3)
    (js2-indent-line))))
;; Javascript:5 ends here

;; [[file:../Sacha.org::#html][HTML:1]]
;;;###autoload
(defun sacha-clean-up-spans-in-region (beg end)
  (interactive "r")
  (save-excursion
    (let ((changed t))
      (while changed
        (setq changed nil)
        (goto-char beg)
        (while (re-search-forward "<span>\\([^<]*\\)</span>" end t)
          (replace-match "\\1")
          (setq changed t)))
      (setq changed t)
      (while changed
        (setq changed nil)
        (goto-char beg)
        (while (re-search-forward "<span>*\\(<a[^<]+>[^<]*</a>\\)</span>" end t)
          (replace-match "\\1")
          (setq changed t))))))

;;;###autoload
(defun sacha-clean-up-spans-in-string (string)
  (with-temp-buffer
    (insert string)
    (sacha-clean-up-spans-in-region (point-min) (point-max))
    (buffer-string)))

(ert-deftest sacha-clean-up-spans-in-string ()
  (should (string= (sacha-clean-up-spans-in-string "<span><span>Hello world</span></span>")
                   "Hello world"))
  (should (string= (sacha-clean-up-spans-in-string "<span><span><a href=\"http://example.com\">Hello another world</a></span></span>")
                   "<a href=\"http://example.com\">Hello another world</a>"))
  (should (string= (sacha-clean-up-spans-in-string "<span><h1>Leave alone</h1></span>") "<span><h1>Leave alone</h1></span>"))
  (should (string= (sacha-clean-up-spans-in-string "<span><a href=\"http://example.com\">Leave</a> alone</span>")
                   "<span><a href=\"http://example.com\">Leave</a> alone</span>")))

;; (ert "sacha-clean-up-spans-in-string")
;; HTML:1 ends here

;; [[file:../Sacha.org::#projects-and-projectile][Projects and projectile:1]]
;;;###autoload
(defun sacha-projectile-open-notes ()
	(interactive)
	(find-file-other-window (expand-file-name "notes.org" (projectile-project-root))))
;; Projects and projectile:1 ends here

;; [[file:../Sacha.org::#coding-sqlite][SQLite:1]]
;;;###autoload
(defun ct/sqlite-view-file-magically ()
    "Runs `sqlite-mode-open-file' on the file name visited by the
current buffer, killing it."
    (require 'sqlite-mode)
    (let ((file-name buffer-file-name))
      (kill-current-buffer)
      (sqlite-mode-open-file file-name)))
;; SQLite:1 ends here

(provide 'sacha-code)
;;; sacha-code.el ends here
