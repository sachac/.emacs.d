;;; my-ai.el ---  -*- lexical-binding: t -*-

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
;; - ChatGPT, AI, and large-language models
;;   https://sachachua.com/dotemacs#chatgpt-ai
;;
;; - agent-shell
;;   https://sachachua.com/dotemacs#inactive-infrequent-things-chatgpt-ai-and-large-language-models-agent-shell
;;
;;; Code:



;; [[file:../Sacha.org::#chatgpt-ai][ChatGPT, AI, and large-language models:2]]
;;;###autoload
(defun gptel-api-key-from-environment (&optional var)
  (lambda ()
    (getenv (or var                     ;provided key
                (thread-first           ;or fall back to <TYPE>_API_KEY
                  (type-of gptel-backend)
                  (symbol-name)
                  (substring 6)
                  (upcase)
                  (concat "_API_KEY"))))))
;; ChatGPT, AI, and large-language models:2 ends here

;; [[file:../Sacha.org::#chatgpt-ai][ChatGPT, AI, and large-language models:4]]
;;;###autoload
(defun my-gptel-set-model ()
  "Interactively set the gptel model."
  (interactive)
  (require 'gptel-transient)
  (let* ((infix (get 'gptel--infix-provider 'transient--suffix))
         (reader (oref infix reader))
         (result (funcall reader "Model: ")))
    (setq gptel-backend (car result))
    (setq gptel-model (cadr result))))
;; ChatGPT, AI, and large-language models:4 ends here

;; [[file:../Sacha.org::#inactive-infrequent-things-chatgpt-ai-and-large-language-models-agent-shell][agent-shell:1]]
;;;###autoload
(defun my-agent-shell-dot-subdir (subdir)
  (let* ((cwd (string-remove-suffix "/" (agent-shell-cwd)))
         (sanitized (replace-regexp-in-string "/" "-" (string-remove-prefix "/" cwd))))
    (expand-file-name subdir (locate-user-emacs-file (concat "agent-shell/" sanitized)))))
;; agent-shell:1 ends here

(provide 'my-ai)
;;; my-ai.el ends here
