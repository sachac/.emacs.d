;;; sacha-navigate.el ---  -*- lexical-binding: t -*-

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
;; - Obscure Emacs package appreciation: backup-walker
;;   https://sachachua.com/dotemacs#about-this-file-backups-obscure-emacs-package-appreciation-backup-walker
;;
;; - Killing text
;;   https://sachachua.com/dotemacs#killing-text
;;
;; - Hydra keyboard shortcuts
;;   https://sachachua.com/dotemacs#hydras
;;
;; - Searching my blog, notes, and sketches with consult-ripgrep and consult-omni
;;   https://sachachua.com/dotemacs#searching-sacha-blog
;;
;; - Navigation
;;   https://sachachua.com/dotemacs#navigation
;;
;; - Focus on the current window
;;   https://sachachua.com/dotemacs#navigation-focus-on-the-current-window
;;
;; - Sort files in read-file-name
;;   https://sachachua.com/dotemacs#sort-read-file-name
;;
;; - Deleting things
;;   https://sachachua.com/dotemacs#deleting-things
;;
;; - Ediff
;;   https://sachachua.com/dotemacs#ediff
;;
;; - Hideshow
;;   https://sachachua.com/dotemacs#hideshow
;;
;; - Frequently-accessed files
;;   https://sachachua.com/dotemacs#frequently-accessed-files
;;
;; - Move to beginning of line
;;   https://sachachua.com/dotemacs#move-to-beginning-of-line
;;
;; - Open files externally
;;   https://sachachua.com/dotemacs#open-files-externally
;;
;; - Toggle
;;   https://sachachua.com/dotemacs#toggle
;;
;; - Randomness for serendipity
;;   https://sachachua.com/dotemacs#random
;;
;; - Touch gestures
;;   https://sachachua.com/dotemacs#touch
;;
;; - Link-related convenience functions
;;   https://sachachua.com/dotemacs#link-related-convenience-functions
;;
;; - Tag files
;;   https://sachachua.com/dotemacs#tag-files
;;
;;; Code:



;; [[file:../Sacha.org::#about-this-file-backups-obscure-emacs-package-appreciation-backup-walker][Obscure Emacs package appreciation: backup-walker:2]]
(defvar backup-walker-data-alist)
(declare-function diff-no-select "diff" (old new &optional switches noasync bufname))
(declare-function backup-walker-get-version "backup-walker")
;;;###autoload
(defun sacha-backup-walker-refresh ()
  (let* ((index (cdr (assq :index backup-walker-data-alist)))
         (suffixes (cdr (assq :backup-suffix-list backup-walker-data-alist)))
         (prefix (cdr (assq :backup-prefix backup-walker-data-alist)))
         (right-file (concat prefix (nth index suffixes)))
         (right-version (format "%i" (backup-walker-get-version right-file)))
         diff-buf left-file left-version)
    (if (eq index 0)
        (setq left-file (cdr (assq :original-file backup-walker-data-alist))
              left-version "orig")
      (setq left-file (concat prefix (nth (1- index) suffixes))
            left-version (format "%i" (backup-walker-get-version left-file))))
    ;; we change this to go the other way here
    (setq diff-buf (diff-no-select right-file left-file nil 'noasync))
    (setq buffer-read-only nil)
    (delete-region (point-min) (point-max))
    (insert-buffer-substring diff-buf)
    (set-buffer-modified-p nil)
    (setq buffer-read-only t)
    (force-mode-line-update)
    (setq header-line-format
          (concat (format "{{ ~%s~ → ~%s~ }} "
                          (propertize left-version 'face 'font-lock-variable-name-face)
                          (propertize right-version 'face 'font-lock-variable-name-face))
                  (if (nth (1+ index) suffixes)
                      (concat (propertize "<p>" 'face 'italic)
                              " ~"
                              (propertize (int-to-string
                                           (backup-walker-get-version (nth (1+ index) suffixes)))
                                          'face 'font-lock-keyword-face)
                              "~ ")
                    "")
                  (if (eq index 0)
                      ""
                    (concat (propertize "<n>" 'face 'italic)
                            " ~"
                            (propertize (int-to-string (backup-walker-get-version (nth (1- index) suffixes)))
                                        'face 'font-lock-keyword-face)
                            "~ "))
                  (propertize "<return>" 'face 'italic)
                  " open ~"
                  (propertize (propertize (int-to-string (backup-walker-get-version right-file))
                                          'face 'font-lock-keyword-face))
                  "~"))
    (kill-buffer diff-buf)))
;; Obscure Emacs package appreciation: backup-walker:2 ends here

;; [[file:../Sacha.org::#killing-text][Killing text:2]]
;;;###autoload
(defun sacha-kill-single-line-if-region-is-inactive (beg end &optional region)
  "Wrap around `kill-region' so that we kill a single line."
  (interactive (progn
                 (let ((beg (mark kill-region-dwim))
                       (end (point)))
                   (cond
                    ((and kill-region-dwim (not (use-region-p)))
                     (list beg end kill-region-dwim))
                    ((not (and beg end))
                     (user-error "The mark is not set now, so there is no region"))
                    ((list beg end 'region))))))
  (if (or (region-active-p)
          (derived-mode-p 'minibuffer-mode))
      (kill-region beg end region)
    (kill-region
     (line-beginning-position)
     (line-beginning-position 2))))

(ert-deftest sacha-kill-single-line-if-region-is-inactive ()
  "Tests `sacha-kill-single-line-if-region-is-inactive'."
  (should
   (equal
    (with-temp-buffer
      (insert "Hello there\nWorld\n")
      (goto-char (point-min))
      (sacha-kill-single-line-if-region-is-inactive nil nil)
      (setq text (buffer-string)))
    "World\n")))
;; Killing text:2 ends here

;; [[file:../Sacha.org::#killing-text][Killing text:3]]
;;;###autoload
(defun sacha-copy-symbol-if-region-is-inactive (beg end &optional region)
  "Wrap around `kill-ring-save' so that we kill a single line."
  (interactive (list (mark) (point) 'region))
  (if (region-active-p)
      (kill-ring-save beg end region)
    (let ((bounds (or (bounds-of-thing-at-point 'symbol)
                      (bounds-of-thing-at-point 'word))))
      (kill-new (filter-buffer-substring (car bounds) (cdr bounds))))))
;; Killing text:3 ends here

;; [[file:../Sacha.org::#hydras][Hydra keyboard shortcuts:6]]
;;;###autoload
(defun sacha-switch-to-previous-buffer ()
  "Switch to previously open buffer.
        Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))
;; Hydra keyboard shortcuts:6 ends here

;; [[file:../Sacha.org::#searching-sacha-blog][Searching my blog, notes, and sketches with consult-ripgrep and consult-omni:1]]
;;;###autoload
  (defun sacha-search-notes ()
          (interactive)
          (consult-ripgrep '("~/sync/orgzly" "~/sync/static-blog/blog" "~/sync/sketches" "~/sync/topics")))
;;;###autoload
  (defun sacha-search-public-notes ()
          (interactive)
          (consult-ripgrep '("~/sync/static-blog/blog" "~/sync/sketches" "~/sync/topics")))
;; Searching my blog, notes, and sketches with consult-ripgrep and consult-omni:1 ends here

;; [[file:../Sacha.org::#navigation][Navigation:2]]
(defun sacha-close-other-buffers ()
  (interactive)
  (mapc (lambda (buf)
          (unless (buffer-modified-p buf)
            (kill-buffer buf)))
        (delete (current-buffer)
                (buffer-list))))
;; Navigation:2 ends here

;; [[file:../Sacha.org::#navigation-focus-on-the-current-window][Focus on the current window:1]]
    (defvar prot/window-configuration nil
      "Current window configuration.
  Intended for use by `prot/window-monocle'.")

    (define-minor-mode prot/window-single-toggle
      "Toggle between multiple windows and single window.
  This is the equivalent of maximising a window.  Tiling window
  managers such as DWM, BSPWM refer to this state as 'monocle'."
      :lighter " [M]"
      :global nil
      (if (one-window-p)
          (when prot/window-configuration
            (set-window-configuration prot/window-configuration))
        (setq prot/window-configuration (current-window-configuration))
        (delete-other-windows)))

;;;###autoload
    (defun prot/kill-buffer-current (&optional arg)
      "Kill current buffer or abort recursion when in minibuffer."
      (interactive "P")
      (if (minibufferp)
          (abort-recursive-edit)
        (kill-buffer (current-buffer)))
      (when (and arg
                 (not (one-window-p)))
        (delete-window)))
;; Focus on the current window:1 ends here

;; [[file:../Sacha.org::#sort-read-file-name][Sort files in read-file-name:1]]
  (defcustom file-name-completions-sort-function #'files-sort-modification-time
    "Function for sorting the completion list of file names.
  The function takes the list of file names as argument
  and returns the sorted list."
    :type '(choice (function :tag "Sort Function") (const :tag "Natural Order" nil))
    :group 'files)

;;;###autoload
  (defun files-sort-access-time (files)
    "Sort FILES list with respect to access time."
    (sort
     files
     (lambda (fn1 fn2)
       (time-less-p
        (file-attribute-access-time (file-attributes fn2))
        (file-attribute-access-time (file-attributes fn1))))))

;;;###autoload
  (defun files-sort-modification-time (files)
    "Sort FILES list with respect to modification time."
    (sort
     files
           :key (lambda (f) (file-attribute-modification-time (file-attributes f)))
           :lessp #'time-less-p
           :reverse t))

;;;###autoload
  (defun ad-completion-file-name-table (fun string pred action)
    "Add 'display-sort-function' to metadata.
  If the completion action is metadata, add
  `file-name-completions-sort-function' as display-sort-function.
  Otherwise call FUN with STRING, PRED and ACTION as arguments."
    (if (and (functionp file-name-completions-sort-function)
                                           (eq action 'metadata))
        `(metadata
                                  (category . file)
                                  (cycle-sort-function . identity)
                                  (display-sort-function . ,file-name-completions-sort-function))
      (funcall fun string pred action)))
;; Sort files in read-file-name:1 ends here

;; [[file:../Sacha.org::#deleting-things][Deleting things:1]]
;;;###autoload
  (defun zap-to-isearch (rbeg rend)
    "Kill the region between the mark and the closest portion of
        the isearch match string. The behaviour is meant to be analogous
        to zap-to-char; let's call it zap-to-isearch. The deleted region
        does not include the isearch word. This is meant to be bound only
        in isearch mode.  The point of this function is that oftentimes
        you want to delete some portion of text, one end of which happens
        to be an active isearch word. The observation to make is that if
        you use isearch a lot to move the cursor around (as you should,
        it is much more efficient than using the arrows), it happens a
        lot that you could just delete the active region between the mark
        and the point, not include the isearch word."
    (interactive "r")
    (when (not mark-active)
      (error "Mark is not active"))
    (let* ((isearch-bounds (list isearch-other-end (point)))
           (ismin (apply 'min isearch-bounds))
           (ismax (apply 'max isearch-bounds))
           )
      (if (< (mark) ismin)
          (kill-region (mark) ismin)
        (if (> (mark) ismax)
            (kill-region ismax (mark))
          (error "Internal error in isearch kill function.")))
      (isearch-exit)
      ))

  (define-key isearch-mode-map [(meta z)] 'zap-to-isearch)
;; Deleting things:1 ends here

;; [[file:../Sacha.org::#ediff][Ediff:2]]
(defvar sacha-ediff-last-windows nil)

;;;###autoload
(defun sacha-store-pre-ediff-winconfig ()
  "Store `current-window-configuration' in variable `sacha-ediff-last-windows'."
  (setq sacha-ediff-last-windows (current-window-configuration)))

;;;###autoload
(defun sacha-restore-pre-ediff-winconfig ()
  "Restore window configuration to stored value in `sacha-ediff-last-windows'."
  (set-window-configuration sacha-ediff-last-windows))
;; Ediff:2 ends here

;; [[file:../Sacha.org::#hideshow][Hideshow:2]]
;;;###autoload
  (defun hs-cycle (&optional level)
    (interactive "p")
    (let (message-log-max
          (inhibit-message t))
      (if (= level 1)
          (pcase last-command
            ('hs-cycle
             (hs-hide-level 1)
             (setq this-command 'hs-cycle-children))
            ('hs-cycle-children
             ;; TODO: Fix this case. `hs-show-block' needs to be
             ;; called twice to open all folds of the parent
             ;; block.
             (save-excursion (hs-show-block))
             (hs-show-block)
             (setq this-command 'hs-cycle-subtree))
            ('hs-cycle-subtree
             (hs-hide-block))
            (_
             (if (not (hs-already-hidden-p))
                 (hs-hide-block)
               (hs-hide-level 1)
               (setq this-command 'hs-cycle-children))))
        (hs-hide-level level)
        (setq this-command 'hs-hide-level))))

;;;###autoload
  (defun hs-global-cycle ()
      (interactive)
      (pcase last-command
        ('hs-global-cycle
         (save-excursion (hs-show-all))
         (setq this-command 'hs-global-show))
        (_ (hs-hide-all))))
;; Hideshow:2 ends here

;; [[file:../Sacha.org::#frequently-accessed-files][Frequently-accessed files:2]]
(require 'bookmark)
(defvar sacha-org-refile-map (make-sparse-keymap))

(defmacro sacha-defshortcut (key file &optional label)
  `(progn
     (set-register ,(string-to-char key) (cons 'file ,file))
     (bookmark-store ,file (list (cons 'filename ,file)
                                 (cons 'position 1)
                                 (cons 'front-context-string "")) nil)
     (define-key sacha-org-refile-map
                 ,key
                 ,(if label
                      `(cons ,label
                             (lambda (prefix)
                               (interactive "p")
                               (let ((org-refile-targets '(((,file) :maxlevel . 6)))
                                     (current-prefix-arg (or current-prefix-arg '(4))))
                                 (call-interactively 'org-refile))))
                    `(lambda (prefix)
                       (interactive "p")
                       (let ((org-refile-targets '(((,file) :maxlevel . 6)))
                             (current-prefix-arg (or current-prefix-arg '(4))))
                         (call-interactively 'org-refile)))))))
(defmacro defshortcuts (name body &optional docstring &rest heads)
  (declare (indent defun) (doc-string 3))
  (cond ((stringp docstring))
        (t
         (setq heads (cons docstring heads))
         (setq docstring "")))
  ;; unwrap
  (when (and (= (length heads) 1) (stringp (car (car (car heads)))))
    (setq heads (car heads)))
  (list
   'progn

   (cons 'progn
         (mapcar (lambda (h) (list 'sacha-defshortcut (string-to-char (elt h 0)) (elt h 1)))
                 heads))))

(defmacro defshortcuts+ (name body &optional docstring &rest heads)
  (declare (indent defun) (doc-string 3))
  (cond ((stringp docstring))
        (t
         (setq heads (cons docstring heads))
         (setq docstring "")))
  (list
   'progn
   (append `(defhydra+ ,name (:exit t))
           (mapcar (lambda (h)
                     (list (elt h 0) (list 'find-file (elt h 1)) (elt h 2)))
                   heads))
   (cons 'progn
         (mapcar (lambda (h) (list 'sacha-defshortcut (string-to-char (elt h 0)) (elt h 1)))
                 heads))))

(defvar sacha-file-shortcuts nil
  "List of ((character filename label) ...).")

;;;###autoload
(defun sacha-navigate-set-up-file-shortcuts ()
  (interactive)
  (mapcar (lambda (o)
            (eval `(sacha-defshortcut ,(elt o 0) ,(elt o 1))))
          sacha-file-shortcuts))
;; Frequently-accessed files:2 ends here

;; [[file:../Sacha.org::#move-to-beginning-of-line][Move to beginning of line:1]]
(defun sacha-smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

        Move point to the first non-whitespace character on this line.
        If point is already there, move to the beginning of the line.
        Effectively toggle between the first non-whitespace character and
        the beginning of the line.

        If ARG is not nil or 1, move forward ARG - 1 lines first.  If
        point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))
;; Move to beginning of line:1 ends here

;; [[file:../Sacha.org::#open-files-externally][Open files externally:1]]
(defun sacha-open-with (arg)
  "Open visited file in default external program.

        With a prefix ARG always prompt for command to use."
  (interactive "P")
  (when buffer-file-name
    (shell-command (concat
                    (cond
                     ((and (not arg) (eq system-type 'darwin)) "open")
                     ((and (not arg) (member system-type '(gnu gnu/linux gnu/kfreebsd))) "xdg-open")
                     (t (read-shell-command "Open current file with: ")))
                    " "
                    (shell-quote-argument buffer-file-name)))))
;; Open files externally:1 ends here

;; [[file:../Sacha.org::#toggle][Toggle:1]]
;;;###autoload
(defun sacha-toggle-or-create (buffer-name &optional  buffer-create-fn switch-cont)
  "Raises or hides BUFFER-NAME.
If BUFFER-CREATE-FN"
  (interactive)
  (let ((target-buf
         (if (file-exists-p buffer-name)
             (find-file-noselect buffer-name)
           (get-buffer buffer-name))))
    (prin1 target-buf)
    (cond
     ((equal (current-buffer) target-buf)
      (bury-buffer))
     (target-buf
      (switch-to-buffer target-buf)
      (if switch-cont (funcall switch-cont)))
     (t (if buffer-create-fn
            (funcall buffer-create-fn)
          (switch-to-buffer
           (get-buffer-create buffer-name)))
        (if switch-cont (funcall switch-cont))))))

;;;###autoload
(defmacro sacha-make-toggle-buffer-function (function-name buffer-name &optional buffer-create-fn switch-cont)
  "Makes a toggle-function to have raise-or-create behaviour.

Creates a toggle-function that executes BUFFER-CREATE-FN if a
buffer named BUFFER-NAME doesn't exist, switches to the buffer
named BUFFER-NAME if it exists, and switches to the previous
buffer if we are currently visiting buffer BUFFER-NAME.

The SWITCH-CONT argument is a function which, if given, is called
after the buffer has been created or switched to.  This allows
running further actions that setup the state of the buffer or
modify it.

From https://www.reddit.com/r/emacs/comments/l4v1ux/one_of_the_most_useful_small_lisp_functions_in_my/"
  (declare (debug t))
  `(defun ,function-name ()
     ,(format "Toggle %s." buffer-name)
     (interactive)
     (sacha-toggle-or-create
      ,buffer-name
      ,buffer-create-fn
      ,switch-cont)))
;; Toggle:1 ends here

;; [[file:../Sacha.org::#random][Randomness for serendipity:1]]
;;;###autoload
(defun sacha-goto-random-char ()
  (interactive)
  (goto-char (random (point-max))))
;; Randomness for serendipity:1 ends here

;; [[file:../Sacha.org::#touch][Touch gestures:1]]
(defvar sacha-navigate-swipe-debounce t "Non-nil means allow swiping.")

;;;###autoload
(defun sacha-navigate-previous-buffer-debounced ()
  (interactive)
  (when sacha-navigate-swipe-debounce
    (previous-buffer)
    (setq sacha-navigate-swipe-debounce nil)
    (run-at-time "1 sec" nil (lambda () (setq sacha-navigate-swipe-debounce t)))))

;;;###autoload
(defun sacha-navigate-next-buffer-debounced ()
  (interactive)
  (when sacha-navigate-swipe-debounce
    (next-buffer)
    (setq sacha-navigate-swipe-debounce nil)
    (run-at-time "1 sec" nil (lambda ()
                               (setq sacha-navigate-swipe-debounce t)))))
;; Touch gestures:1 ends here

;; [[file:../Sacha.org::#link-related-convenience-functions][Link-related convenience functions:1]]
;;;###autoload
(defun kensanata/resolve-redirect (url)
  "Resolve shortened URL by launching `curl --head' and parsing the result."
  (let* ((curl (shell-command-to-string
                (format "curl --silent --head %s" url)))
         (location (when (and (string-match "^HTTP/1\.1 301" curl)
                              (string-match "^Location: \\(.*\\)" curl))
                     (match-string 1 curl))))
    (or location url)))

;;;###autoload
(defun sacha-resolve-urls-in-region (beg end)
  "Expand URLs between BEG and END."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (re-search-forward org-link-bracket-re nil t)
        (replace-match (save-match-data (kensanata/resolve-redirect
                                         (match-string 1))) t t nil 1))
      (goto-char (point-min))
      (while (re-search-forward org-link-re-with-space nil t)
        (replace-match (save-match-data (kensanata/resolve-redirect
                                         (match-string 0))) t t nil)))))

;;;###autoload
(defun sacha-open-urls-in-region (beg end)
  "Open URLs between BEG and END.
        TODO: Get better at detecting and opening all URLs"
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (re-search-forward org-link-any-re nil t)
        (save-excursion
          (backward-char)
					(let ((url (match-string 0)))
						(unless (string-match "permalink.gmane.org" url)
							(browse-url url))))))))
;; Link-related convenience functions:1 ends here

;; [[file:../Sacha.org::#link-related-convenience-functions][Link-related convenience functions:2]]
(add-to-list 'browse-url-handlers '("https?://yhetil.org/.*/raw" . sacha-browse-yhetil))
;;;###autoload
(defun sacha-browse-yhetil (url &rest _)
  (when (string-match "\\(https?://yhetil.org/.*\\)/raw$" url)
    (funcall browse-url-browser-function (match-string 1 url))))
;; Link-related convenience functions:2 ends here

;; [[file:../Sacha.org::#tag-files][Tag files:1]]
;;;###autoload
(defun sacha-recursive-find-file (file &optional directory)
  "Find the first FILE in DIRECTORY or its parents."
  (setq directory (or directory (file-name-directory (buffer-file-name)) (pwd)))
  (if (file-exists-p (expand-file-name file directory))
      (expand-file-name file directory)
    (unless (string= directory "/")
      (sacha-recursive-find-file file (expand-file-name ".." directory)))))

;;;###autoload
(defun sacha-find-tags ()
  "Set the TAGS file."
  (set (make-variable-buffer-local 'tags-table-list) nil)
  (set (make-variable-buffer-local 'tags-file-name)
       (sacha-recursive-find-file "TAGS")))
;; Tag files:1 ends here

(provide 'sacha-navigate)
;;; sacha-navigate.el ends here
