;;; sacha-spookfox.el ---  -*- lexical-binding: t -*-

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
;; - Link to current webpage from Spookfox
;;   https://sachachua.com/dotemacs#web-spookfox-link-to-current-webpage-from-spookfox
;;
;; - Link to currently-selected text using Spookfox
;;   https://sachachua.com/dotemacs#spookfox-fragment
;;
;; - Running the current Org Mode Babel Javascript block from Emacs using Spookfox
;;   https://sachachua.com/dotemacs#spookfox-babel
;;
;; - Using Spookfox to scroll Firefox up and down from Emacs
;;   https://sachachua.com/dotemacs#spookfox-scroll
;;
;; - Emacs and Spookfox: org-capture the current tab from Firefox or a link from the page
;;   https://sachachua.com/dotemacs#spookfox-insert-url
;;
;; - Interact with Google Gemini web interface through Spookfox
;;   https://sachachua.com/dotemacs#inactive-infrequent-things-chatgpt-ai-and-large-language-models-interact-with-google-gemini-web-interface-through-spookfox
;;
;;; Code:



;; [[file:../Sacha.org::#web-spookfox-link-to-current-webpage-from-spookfox][Link to current webpage from Spookfox:1]]
;;;###autoload
(defun sacha-org-spookfox-complete ()
	(spookfox-js-injection-eval-in-active-tab "window.location.href" t))
;; Link to current webpage from Spookfox:1 ends here

;; [[file:../Sacha.org::#spookfox-fragment][Link to currently-selected text using Spookfox:1]]
;;;###autoload
(defun sacha-spookfox-link-to-fragment ()
	(interactive)
	(let ((url
				 (spookfox-js-injection-eval-in-active-tab "window.location.href + '#:~:text=' + encodeURIComponent(window.getSelection().toString())" t)))
		(when (called-interactively-p 'any)
			(insert url))
		url))
;; Link to currently-selected text using Spookfox:1 ends here

;; [[file:../Sacha.org::#spookfox-babel][Running the current Org Mode Babel Javascript block from Emacs using Spookfox:4]]
;;;###autoload
(defun sacha-org-babel-execute:js-spookfox (old-fn body params)
	"Maybe execute Spookfox."
	(if (assq :spookfox params)
			(spookfox-js-injection-eval-in-active-tab
			 body t)
		(funcall old-fn body params)))
;; Running the current Org Mode Babel Javascript block from Emacs using Spookfox:4 ends here

;; [[file:../Sacha.org::#spookfox-babel][Running the current Org Mode Babel Javascript block from Emacs using Spookfox:6]]
;;;###autoload
(defun sacha-spookfox-eval-org-block ()
	(interactive)
	(let ((block (org-element-context)))
		(when (and (eq (org-element-type block) 'src-block)
							 (string= (org-element-property :language block) "js"))
			(spookfox-js-injection-eval-in-active-tab
			 (nth 2 (org-src--contents-area block))
			 t))))
;; Running the current Org Mode Babel Javascript block from Emacs using Spookfox:6 ends here

;; [[file:../Sacha.org::#spookfox-babel][Running the current Org Mode Babel Javascript block from Emacs using Spookfox:8]]
;;;###autoload
(defun sacha-spookfox-send-region (start end)
	(interactive "r")
	(spookfox-js-injection-eval-in-active-tab (buffer-substring start end) t))

;;;###autoload
(defun sacha-spookfox-send-buffer ()
	(interactive)
	(sacha-spookfox-send-region (point-min) (point-max)))

;;;###autoload
(defun sacha-spookfox-send-line ()
	(interactive)
	(sacha-spookfox-send-region (line-beginning-position) (line-end-position)))

;;;###autoload
(defun sacha-spookfox-send-last-expression ()
	(interactive)
	(sacha-spookfox-send-region (save-excursion (nodejs-repl--beginning-of-expression)) (point)))

(defvar-keymap sacha-js-spookfox-minor-mode-map
	:doc "Send parts of the buffer to Spookfox."
	"C-x C-e" 'sacha-spookfox-send-last-expression
	"C-c C-j" 'sacha-spookfox-send-line
	"C-c C-r" 'sacha-spookfox-send-region
	"C-c C-c" 'sacha-spookfox-send-buffer)

(define-minor-mode sacha-js-spookfox-minor-mode "Send code to Spookfox.")
;; Running the current Org Mode Babel Javascript block from Emacs using Spookfox:8 ends here

;; [[file:../Sacha.org::#spookfox-babel][Running the current Org Mode Babel Javascript block from Emacs using Spookfox:9]]
;;;###autoload
(defun org-babel-edit-prep:js (info)
	(when (assq :spookfox (nth 2 info))
		(sacha-js-spookfox-minor-mode 1)))
;; Running the current Org Mode Babel Javascript block from Emacs using Spookfox:9 ends here

;; [[file:../Sacha.org::sacha-spookfox-scroll][sacha-spookfox-scroll]]
;;;###autoload
(defun sacha-spookfox-scroll-down ()
	(interactive)
 	(spookfox-js-injection-eval-in-active-tab "window.scrollBy(0, document.documentElement.clientHeight);" t))

;;;###autoload
(defun sacha-spookfox-scroll-up ()
	(interactive)
 	(spookfox-js-injection-eval-in-active-tab "window.scrollBy(0, -document.documentElement.clientHeight);"))
;; sacha-spookfox-scroll ends here

;; [[file:../Sacha.org::sacha-spookfox-background-tab][sacha-spookfox-background-tab]]
;;;###autoload
(defun sacha-spookfox-background-tab (url &rest args)
	"Open URL as a background tab."
	(if spookfox--connected-clients
			(spookfox-tabs--request (cl-first spookfox--connected-clients) "OPEN_TAB" `(:url ,url))
		(browse-url url)))
;; sacha-spookfox-background-tab ends here

;; [[file:../Sacha.org::#spookfox-scroll][Using Spookfox to scroll Firefox up and down from Emacs:6]]
;;;###autoload
(defun sacha-spookfox-get-links ()
	(seq-uniq
 	 (spookfox-eval-js-in-active-tab "[...(document.querySelector('[data-testid=post-container]')?.parentElement || document).querySelectorAll('a')].map(a => a.href).filter(a => a && (!window.location.host.match(/reddit/) || !a.match(/redd\.?it/)) && !a.match(window.location.host))" t)))

;;;###autoload
(defun sacha-spookfox-complete-link (&optional prompt)
	(completing-read
	 (or prompt "Link: ")
	 (sacha-presorted-completion-table
		(sacha-spookfox-get-links))))

;;;###autoload
(defun sacha-spookfox-insert-link-from-page (link)
	(interactive (list (sacha-spookfox-complete-link)))
	(insert (org-link-make-string link (sacha-page-title link))))

;;;###autoload
(defun sacha-spookfox-open-link-from-page (link)
	(interactive (list (sacha-spookfox-complete-link)))
	(sacha-spookfox-background-tab link))

;;;###autoload
(defun sacha-spookfox-insert-link-to-tab ()
	(interactive)
	(let ((tab (spookfox-request-active-tab)))
		(insert (org-link-make-string
						 (plist-get tab :url)
						 (plist-get tab :title)))))
;; Using Spookfox to scroll Firefox up and down from Emacs:6 ends here

;; [[file:../Sacha.org::#spookfox-insert-url][Emacs and Spookfox: org-capture the current tab from Firefox or a link from the page:1]]
;;;###autoload
(defun sacha-spookfox-insert-url ()
	(interactive)
	(insert (spookfox-js-injection-eval-in-active-tab "window.location.href" t)))
;;;###autoload
(defun sacha-spookfox-insert-org-link ()
	(interactive)
	(insert (apply #'org-link-make-string
								 (append (spookfox-js-injection-eval-in-active-tab "[window.location.href, document.title]" t) nil))))
;; Emacs and Spookfox: org-capture the current tab from Firefox or a link from the page:1 ends here

;; [[file:../Sacha.org::#inactive-infrequent-things-chatgpt-ai-and-large-language-models-interact-with-google-gemini-web-interface-through-spookfox][Interact with Google Gemini web interface through Spookfox:1]]
;;;###autoload
(defun sacha-spookfox-ai-replace-with-code ()
  (interactive)
  (erase-buffer)
  (insert (learn-lang-spookfox-ai-get-latest-code)))

;;;###autoload
(defun sacha-spookfox-ai-ediff-with-code ()
  (interactive)
  (with-current-buffer (get-buffer-create "*ai*")
    (erase-buffer)
    (insert (learn-lang-spookfox-ai-get-latest-code)))
  (ediff-buffers (current-buffer) (get-buffer-create "*ai*")))
;; Interact with Google Gemini web interface through Spookfox:1 ends here

(provide 'sacha-spookfox)
;;; sacha-spookfox.el ends here
