;;; my-git.el ---  -*- lexical-binding: t -*-

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
;; - Magit - nice git interface
;;   https://sachachua.com/dotemacs#magit
;;
;; - Finding repos with uncommitted changes
;;   https://sachachua.com/dotemacs#coding-magit-nice-git-interface-finding-repos-with-uncommitted-changes
;;
;; - Use difftastic
;;   https://sachachua.com/dotemacs#coding-magit-nice-git-interface-use-difftastic
;;
;; - Checking things out
;;   https://sachachua.com/dotemacs#checking-things-out
;;
;;; Code:



;; [[file:../Sacha.org::#magit][Magit - nice git interface:1]]
(defvar my-magit-limit-to-directory nil "Limit magit status to a specific directory.")
;;;###autoload
(defun my-magit-stage-all-and-commit (message)
  (interactive (list (progn (magit-diff-unstaged) (read-string "Commit Message: "))))
  (magit-stage-modified)
  (magit-commit-create (list "-m" message))
  (call-interactively #'magit-push-current-to-pushremote))
;;;###autoload
(defun my-magit-status-in-directory (directory)
  "Displays magit status limited to DIRECTORY.
Uses the current `default-directory', or prompts for a directory
if called with a prefix argument. Sets `my-magit-limit-to-directory'
so that it's still active even after you stage a change. Very experimental."
  (interactive (list (expand-file-name
                        (if current-prefix-arg
                            (read-directory-name "Directory: ")
                          default-directory))))
    (setq my-magit-limit-to-directory directory)
    (magit-status directory))
;; Magit - nice git interface:1 ends here

;; [[file:../Sacha.org::#coding-magit-nice-git-interface-finding-repos-with-uncommitted-changes][Finding repos with uncommitted changes:1]]
;;;###autoload
(defun my-git-find-unclean-repo (root-dir)
  "Find repo with modified files."
  ;; (interactive)
  (setq out nil)
  (dolist (dir (directory-files-recursively root-dir "\\.git$" t))
    (message "checking repo %s" dir)
    (let* ((git-dir (file-name-parent-directory dir))
           (default-directory git-dir))
      (unless (string= "" (shell-command-to-string "git status --untracked=no --porcelain"))
        (push git-dir out))))
  out)

;;;###autoload
(defun my-list-uncommitted-projects ()
	(interactive)
	(let ((s (string-join
						(seq-keep
						 (lambda (root)
							 (when-let ((repo (my-git-find-unclean-repo root)))
								 (concat "- "
												 (org-link-make-string
													(format "elisp:(magit-status \"%s\")"
																	(car repo))
													(file-name-nondirectory (replace-regexp-in-string "/$" "" root))))))
						 (seq-uniq
							(mapcar (lambda (row)
												(or (projectile-project-root
														 (car row))
														(car row)))
											(cons '("~/sync/emacs") my-project-web-base-list))))
						"\n")))
		(when (called-interactively-p 'any)
			(switch-to-buffer (get-buffer-create "*Uncommitted*"))
			(erase-buffer)
			(insert s)
			(org-mode))
		s))
;; Finding repos with uncommitted changes:1 ends here

;; [[file:../Sacha.org::#coding-magit-nice-git-interface-use-difftastic][Use difftastic:1]]
;;;###autoload
(defun th/magit--with-difftastic (buffer command)
  "Run COMMAND with GIT_EXTERNAL_DIFF=difftastic then show result in BUFFER."
  (let ((process-environment
         (cons (concat "TMPDIR=~/tmp GIT_EXTERNAL_DIFF=difftastic --width="
                       (number-to-string (frame-width)))
               process-environment)))
    ;; Clear the result buffer (we might regenerate a diff, e.g., for
    ;; the current changes in our working directory).
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer))
    ;; Now spawn a process calling the git COMMAND.
    (make-process
     :name (buffer-name buffer)
     :buffer buffer
     :command command
     ;; Don't query for running processes when emacs is quit.
     :noquery t
     ;; Show the result buffer once the process has finished.
     :sentinel (lambda (proc event)
                 (when (eq (process-status proc) 'exit)
                   (with-current-buffer (process-buffer proc)
                     (goto-char (point-min))
                     (ansi-color-apply-on-region (point-min) (point-max))
                     (setq buffer-read-only t)
                     (view-mode)
                     (end-of-line)
                     ;; difftastic diffs are usually 2-column side-by-side,
                     ;; so ensure our window is wide enough.
                     (let ((width (current-column)))
                       (while (zerop (forward-line 1))
                         (end-of-line)
                         (setq width (max (current-column) width)))
                       ;; Add column size of fringes
                       (setq width (+ width
                                      (fringe-columns 'left)
                                      (fringe-columns 'right)))
                       (goto-char (point-min))
                       (pop-to-buffer
                        (current-buffer)
                        `(;; If the buffer is that wide that splitting the frame in
                          ;; two side-by-side windows would result in less than
                          ;; 80 columns left, ensure it's shown at the bottom.
                          ,(when (> 80 (- (frame-width) width))
                             #'display-buffer-at-bottom)
                          (window-width
                           . ,(min width (frame-width))))))))))))
;;;###autoload
(defun th/magit-show-with-difftastic (rev)
  "Show the result of \"git show REV\" with GIT_EXTERNAL_DIFF=difft."
  (interactive
   (list (or
          ;; If REV is given, just use it.
          (when (boundp 'rev) rev)
          ;; If not invoked with prefix arg, try to guess the REV from
          ;; point's position.
          (and (not current-prefix-arg)
               (or (magit-thing-at-point 'git-revision t)
                   (magit-branch-or-commit-at-point)))
          ;; Otherwise, query the user.
          (magit-read-branch-or-commit "Revision"))))
  (if (not rev)
      (error "No revision specified")
    (th/magit--with-difftastic
     (get-buffer-create (concat "*git show difftastic " rev "*"))
     (list "git" "--no-pager" "show" "--ext-diff" rev))))
;;;###autoload
(defun th/magit-diff-with-difftastic (arg)
  "Show the result of \"git diff ARG\" with GIT_EXTERNAL_DIFF=difft."
  (interactive
   (list (or
          ;; If RANGE is given, just use it.
          (when (boundp 'range) range)
          ;; If prefix arg is given, query the user.
          (and current-prefix-arg
               (magit-diff-read-range-or-commit "Range"))
          ;; Otherwise, auto-guess based on position of point, e.g., based on
          ;; if we are in the Staged or Unstaged section.
          (pcase (magit-diff--dwim)
            ('unmerged (error "unmerged is not yet implemented"))
            ('unstaged nil)
            ('staged "--cached")
            (`(stash . ,value) (error "stash is not yet implemented"))
            (`(commit . ,value) (format "%s^..%s" value value))
            ((and range (pred stringp)) range)
            (_ (magit-diff-read-range-or-commit "Range/Commit"))))))
  (let ((name (concat "*git diff difftastic"
                      (if arg (concat " " arg) "")
                      "*")))
    (th/magit--with-difftastic
     (get-buffer-create name)
     `("git" "--no-pager" "diff" "--ext-diff" ,@(when arg (list arg))))))
(transient-define-prefix th/magit-aux-commands ()
  "My personal auxiliary magit commands."
  ["Auxiliary commands"
   ("d" "Difftastic Diff (dwim)" th/magit-diff-with-difftastic)
	 ("s" "Difftastic Show" th/magit-show-with-difftastic)])
;; Use difftastic:1 ends here

;; [[file:../Sacha.org::#checking-things-out][Checking things out:1]]
(defvar my-git-clone-destination "~/vendor")
;;;###autoload
(defun my-git-clone-clipboard-url ()
  "Clone git URL in clipboard asynchronously and open in dired when finished."
  (interactive)
  (cl-assert (string-match-p "^\\(http\\|https\\|ssh\\)://" (current-kill 0)) nil "No URL in clipboard")
  (let* ((url (current-kill 0))
         (download-dir (expand-file-name my-git-clone-destination))
         (project-dir (concat (file-name-as-directory download-dir)
                              (file-name-base url)))
         (default-directory download-dir)
         (command (format "git clone %s" url))
         (buffer (generate-new-buffer (format "*%s*" command)))
         (proc))
    (when (file-exists-p project-dir)
      (if (y-or-n-p (format "%s exists. delete?" (file-name-base url)))
          (delete-directory project-dir t)
        (user-error "Bailed")))
    (switch-to-buffer buffer)
    (setq proc (start-process-shell-command (nth 0 (split-string command)) buffer command))
    (with-current-buffer buffer
      (setq default-directory download-dir)
      (shell-command-save-pos-or-erase)
      (require 'shell)
      (shell-mode)
      (view-mode +1))
    (set-process-sentinel proc (lambda (process state)
                                 (let ((output (with-current-buffer (process-buffer process)
                                                 (buffer-string))))
                                   (kill-buffer (process-buffer process))
                                   (if (= (process-exit-status process) 0)
                                       (progn
                                         (message "finished: %s" command)
                                         (dired project-dir))
                                     (user-error (format "%s\n%s" command output))))))
    (set-process-filter proc #'comint-output-filter)))
;; Checking things out:1 ends here

(provide 'my-git)
;;; my-git.el ends here
