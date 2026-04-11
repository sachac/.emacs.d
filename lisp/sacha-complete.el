;;; sacha-complete.el ---  -*- lexical-binding: t -*-

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
;; - Extended command list
;;   https://sachachua.com/dotemacs#keybindings-extended-command-list
;;
;; - Emacs completion and handling accented characters with orderless
;;   https://sachachua.com/dotemacs#completion-emacs-completion-and-handling-accented-characters-with-orderless
;;
;; - Marginalia
;;   https://sachachua.com/dotemacs#marginalia
;;
;; - Marginalia and annotating journal entries
;;   https://sachachua.com/dotemacs#marginalia-and-annotating-journal-entries
;;
;; - Expand
;;   https://sachachua.com/dotemacs#expand
;;
;; - Snippets
;;   https://sachachua.com/dotemacs#snippets
;;
;;; Code:



;; [[file:../Sacha.org::#keybindings-extended-command-list][Extended command list:2]]
;;; Mostly the same as my/read-extended-command-from-list
;;;###autoload
(defun sacha-read-extended-command-from-list (list)
  "Read command name to invoke in `execute-extended-command'."
  (minibuffer-with-setup-hook
      (lambda ()
        (add-hook 'post-self-insert-hook
                  (lambda ()
                    (setq execute-extended-command--last-typed
                          (minibuffer-contents)))
                  nil 'local)
        (setq-local minibuffer-default-add-function
                    (lambda ()
                      ;; Get a command name at point in the original buffer
                      ;; to propose it after M-n.
                      (let ((def (with-current-buffer
                                     (window-buffer (minibuffer-selected-window))
                                   (and (commandp (function-called-at-point))
                                        (format "%S" (function-called-at-point)))))
                            (all (sort (minibuffer-default-add-completions)
                                       #'string<)))
                        (if def
                            (cons def (delete def all))
                          all)))))
    ;; Read a string, completing from and restricting to the set of
    ;; all defined commands.  Don't provide any initial input.
    ;; Save the command read on the extended-command history list.
    (completing-read
     (concat (cond
              ((eq current-prefix-arg '-) "- ")
              ((and (consp current-prefix-arg)
                    (eq (car current-prefix-arg) 4)) "C-u ")
              ((and (consp current-prefix-arg)
                    (integerp (car current-prefix-arg)))
               (format "%d " (car current-prefix-arg)))
              ((integerp current-prefix-arg)
               (format "%d " current-prefix-arg)))
             ;; This isn't strictly correct if `execute-extended-command'
             ;; is bound to anything else (e.g. [menu]).
             ;; It could use (key-description (this-single-command-keys)),
             ;; but actually a prompt other than "M-x" would be confusing,
             ;; because "M-x" is a well-known prompt to read a command
             ;; and it serves as a shorthand for "Extended command: ".
             "M-x ")
     (lambda (string pred action)
       (if (and suggest-key-bindings (eq action 'metadata))
           '(metadata
             (affixation-function . read-extended-command--affixation)
             (category . command))
         (complete-with-action action list string pred)))
     #'commandp t nil 'extended-command-history)))

;;; Mostly the same as execute-extended-command
;;;###autoload
(defun sacha-execute-extended-command-from-list (prefixarg &optional command-name typed)
  ;; Based on Fexecute_extended_command in keyboard.c of Emacs.
  ;; Aaron S. Hawley <aaron.s.hawley(at)gmail.com> 2009-08-24
  "Read a command name, then read the arguments and call the command.
   To pass a prefix argument to the command you are
   invoking, give a prefix argument to `execute-extended-command'."
  (declare (interactive-only command-execute))
  ;; FIXME: Remember the actual text typed by the user before completion,
  ;; so that we don't later on suggest the same shortening.
  (interactive
   (let ((execute-extended-command--last-typed nil))
     (list current-prefix-arg
           (if (and command-name (listp command-name))
               (sacha-read-extended-command-from-list command-name)
             (read-extended-command))
           execute-extended-command--last-typed)))
  ;; Emacs<24 calling-convention was with a single `prefixarg' argument.
  (when (listp command-name)
    (let ((current-prefix-arg prefixarg) ; for prompt
          (execute-extended-command--last-typed nil))
      (setq command-name
            (if command-name
                (sacha-read-extended-command-from-list command-name)
              (read-extended-command)))
      (setq typed execute-extended-command--last-typed)))
  (let* ((function (and (stringp command-name) (intern-soft command-name)))
         (binding (and suggest-key-bindings
                       (not executing-kbd-macro)
                       (where-is-internal function overriding-local-map t))))
    (unless (commandp function)
      (error "`%s' is not a valid command name" command-name))
    ;; Some features, such as novice.el, rely on this-command-keys
    ;; including M-x COMMAND-NAME RET.
    (set--this-command-keys (concat "\M-x" (symbol-name function) "\r"))
    (setq this-command function)
    ;; Normally `real-this-command' should never be changed, but here we really
    ;; want to pretend that M-x <cmd> RET is nothing more than a "key
    ;; binding" for <cmd>, so the command the user really wanted to run is
    ;; `function' and not `execute-extended-command'.  The difference is
    ;; visible in cases such as M-x <cmd> RET and then C-x z (bug#11506).
    (setq real-this-command function)
    (let ((prefix-arg prefixarg))
      (command-execute function 'record))
    ;; If enabled, show which key runs this command.
    ;; But first wait, and skip the message if there is input.
    (let* ((waited
            ;; If this command displayed something in the echo area;
            ;; wait a few seconds, then display our suggestion message.
            ;; FIXME: Wait *after* running post-command-hook!
            ;; FIXME: If execute-extended-command--shorter were
            ;; faster, we could compute the result here first too.
            (when (and suggest-key-bindings
                       (or binding
                           (and extended-command-suggest-shorter typed)))
              (sit-for (cond
                        ((zerop (length (current-message))) 0)
                        ((numberp suggest-key-bindings) suggest-key-bindings)
                        (t 2))))))
      (when (and waited (not (consp unread-command-events)))
        (unless (or (not extended-command-suggest-shorter)
                    binding executing-kbd-macro (not (symbolp function))
                    (<= (length (symbol-name function)) 2))
          ;; There's no binding for CMD.  Let's try and find the shortest
          ;; string to use in M-x.
          ;; FIXME: Can be slow.  Cache it maybe?
          (while-no-input
            (setq binding (execute-extended-command--shorter
                           (symbol-name function) typed))))
        (when binding
          (with-temp-message
              (format-message "You can run the command `%s' with %s"
                              function
                              (if (stringp binding)
                                  (concat "M-x " binding " RET")
                                (key-description binding)))
            (sit-for (if (numberp suggest-key-bindings)
                         suggest-key-bindings
                       2))))))))
;; Extended command list:2 ends here

;; [[file:../Sacha.org::#completion-emacs-completion-and-handling-accented-characters-with-orderless][Emacs completion and handling accented characters with orderless:1]]
(defvar sacha-orderless-accent-replacements
  '(("a" . "[aàáâãäå]")
    ("e" . "[eèéêë]")
    ("i" . "[iìíîï]")
    ("o" . "[oòóôõöœ]")
    ("u" . "[uùúûü]")
    ("c" . "[cç]")
    ("n" . "[nñ]"))) ; in case anyone needs ñ for Spanish

;;;###autoload
(defun sacha-orderless-accent-dispatch (pattern &rest _)
  (seq-reduce
   (lambda (prev val)
     (replace-regexp-in-string (car val) (cdr val) prev))
   sacha-orderless-accent-replacements
   pattern))
;; Emacs completion and handling accented characters with orderless:1 ends here

;; [[file:../Sacha.org::#marginalia][Marginalia:1]]
;;;###autoload
(defun sacha-marginalia-annotate-variable (cand)
  "Annotate variable CAND with its documentation string.
Omit values when streaming."
  (when-let* ((sym (intern-soft cand)))
    (marginalia--fields
     ((marginalia--symbol-class sym) :face 'marginalia-type)
     ((or (documentation-property sym 'variable-documentation)
          (marginalia--definition-prefix sym))
      :truncate 1.0 :face 'marginalia-documentation))))
;; Marginalia:1 ends here

;; [[file:../Sacha.org::#marginalia][Marginalia:3]]
;;;###autoload
  (defun marginalia-annotate-alias (cand)
    "Annotate CAND with the function it aliases."
    (when-let ((sym (intern-soft cand))
               (alias (car (last (function-alias-p sym))))
               (name (and (symbolp alias) (symbol-name alias))))
      (format " (%s)" name)))

;;;###autoload
  (defun marginalia-annotate-symbol-with-alias (cand)
    "Annotate symbol CAND with its documentation string.
      Similar to `marginalia-annotate-symbol'."
    (when-let (sym (intern-soft cand))
      (concat
       (marginalia-annotate-binding cand)
       (marginalia--fields
        ((marginalia-annotate-alias cand) :face 'marginalia-function)
        ((marginalia--symbol-class sym) :face 'marginalia-type)
        ((cond
          ((fboundp sym) (marginalia--function-doc sym))
          ((facep sym) (documentation-property sym 'face-documentation))
          (t (documentation-property sym 'variable-documentation)))
         :truncate 1.0 :face 'marginalia-documentation)))))
;; Marginalia:3 ends here

;; [[file:../Sacha.org::#marginalia-and-annotating-journal-entries][Marginalia and annotating journal entries:1]]
;;;###autoload
  (defun sacha-marginalia-annotate-journal (cand)
    (when-let ((o (cdr (assoc cand sacha-journal-search-cache))))
      (marginalia--fields
       ((plist-get o :Category)
        :face 'marginalia-documentation
        :truncate 13))))

  (use-package marginalia
    :config
    (add-to-list 'marginalia-annotators '(journal sacha-marginalia-annotate-journal builtin none)))
;; Marginalia and annotating journal entries:1 ends here

;; [[file:../Sacha.org::#expand][Expand:2]]
;;;###autoload
  (defun sanityinc/dabbrev-friend-buffer (other-buffer)
    (< (buffer-size other-buffer) (* 1 1024 1024)))
  (setq dabbrev-friend-buffer-function 'sanityinc/dabbrev-friend-buffer)
;; Expand:2 ends here

;; [[file:../Sacha.org::#snippets][Snippets:1]]
;;;###autoload
(defun sacha-use-yasnippet-capf () (add-to-list 'completion-at-point-functions #'yasnippet-capf))
;; Snippets:1 ends here

;; [[file:../Sacha.org::#snippets][Snippets:3]]
;;;###autoload
(defun shk-yas/helm-prompt (prompt choices &optional display-fn)
  "Use helm to select a snippet. Put this into `yas/prompt-functions.'"
  (interactive)
  (setq display-fn (or display-fn 'identity))
  (if (require 'helm-config)
      (let (tmpsource cands result rmap)
        (setq cands (mapcar (lambda (x) (funcall display-fn x)) choices))
        (setq rmap (mapcar (lambda (x) (cons (funcall display-fn x) x)) choices))
        (setq tmpsource
              (list
               (cons 'name prompt)
               (cons 'candidates cands)
               '(action . (("Expand" . (lambda (selection) selection))))
               ))
        (setq result (helm-other-buffer '(tmpsource) "*helm-select-yasnippet"))
        (if (null result)
            (signal 'quit "user quit!")
          (cdr (assoc result rmap))))
    nil))
;; Snippets:3 ends here

;; [[file:../Sacha.org::#snippets][Snippets:5]]
;; It will test whether it can expand, if yes, cursor color -> green.
;;;###autoload
(defun yasnippet-can-fire-p (&optional field)
  (interactive)
  (setq yas--condition-cache-timestamp (current-time))
  (let (templates-and-pos)
    (unless (and yas-expand-only-for-last-commands
                 (not (member last-command yas-expand-only-for-last-commands)))
      (setq templates-and-pos (if field
                                  (save-restriction
                                    (narrow-to-region (yas--field-start field)
                                                      (yas--field-end field))
                                    (yas--templates-for-key-at-point))
                                (yas--templates-for-key-at-point))))
    (and templates-and-pos (first templates-and-pos))))

;;;###autoload
(defun sacha-change-cursor-color-when-can-expand (&optional field)
  (interactive)
  (when (eq last-command 'self-insert-command)
    (set-cursor-color (if (sacha-can-expand)
                          yasnippet-can-fire-cursor-color
                        default-cursor-color))))

;;;###autoload
(defun sacha-can-expand ()
  "Return true if right after an expandable thing."
  (or (abbrev--before-point) (yasnippet-can-fire-p)))

;;;###autoload
(defun sacha-insert-space-or-expand ()
  "For binding to the SPC SPC keychord."
  (interactive)
  (condition-case nil (or (sacha-hippie-expand-maybe nil) (insert "  "))))
;; Snippets:5 ends here

;; [[file:../Sacha.org::#snippets][Snippets:7]]
;;;###autoload
(defun sacha-hippie-expand-maybe (arg)
  "Try to expand text before point, using multiple methods.
      The expansion functions in `hippie-expand-try-functions-list' are
      tried in order, until a possible expansion is found.  Repeated
      application of `hippie-expand' inserts successively possible
      expansions.
      With a positive numeric argument, jumps directly to the ARG next
      function in this list.  With a negative argument or just \\[universal-argument],
      undoes the expansion."
  (interactive "P")
  (require 'hippie-exp)
  (if (or (not arg)
          (and (integerp arg) (> arg 0)))
      (let ((first (or (= he-num -1)
                       (not (equal this-command last-command)))))
        (if first
            (progn
              (setq he-num -1)
              (setq he-tried-table nil)))
        (if arg
            (if (not first) (he-reset-string))
          (setq arg 0))
        (let ((i (max (+ he-num arg) 0)))
          (while (not (or (>= i (length hippie-expand-try-functions-list))
                          (apply (nth i hippie-expand-try-functions-list)
                                 (list (= he-num i)))))
            (setq i (1+ i)))
          (setq he-num i))
        (if (>= he-num (length hippie-expand-try-functions-list))
            (progn (setq he-num -1) nil)
          (if (and hippie-expand-verbose
                   (not (window-minibuffer-p)))
              (message "Using %s"
                       (nth he-num hippie-expand-try-functions-list)))))
    (if (and (>= he-num 0)
             (eq (marker-buffer he-string-beg) (current-buffer)))
        (progn
          (setq he-num -1)
          (he-reset-string)
          (if (and hippie-expand-verbose
                   (not (window-minibuffer-p)))
              (message "Undoing expansions"))))))

;; Snippets:7 ends here

(provide 'sacha-complete)
;;; sacha-complete.el ends here
