;;; sacha-eshell.el ---  -*- lexical-binding: t -*-

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



;; [[file:../Sacha.org::#consult-directory-navigation][Consult directory navigation:2]]
;; https://karthinks.com/software/jumping-directories-in-eshell/
;;;###autoload
(defun eshell/z (&optional regexp)
  "Navigate to a previously visited directory in eshell, or to
       any directory proferred by `consult-dir'."
  (let ((eshell-dirs (delete-dups
                      (mapcar 'abbreviate-file-name
                              (ring-elements eshell-last-dir-ring)))))
    (cond
     ((and (not regexp) (featurep 'consult-dir))
      (let* ((consult-dir--source-eshell `(:name "Eshell"
                                                 :narrow ?e
                                                 :category file
                                                 :face consult-file
                                                 :items ,eshell-dirs))
             (consult-dir-sources (cons consult-dir--source-eshell
                                        consult-dir-sources)))
        (eshell/cd (substring-no-properties
                    (consult-dir--pick "Switch directory: ")))))
     (t (eshell/cd (if regexp (eshell-find-previous-directory regexp)
                     (completing-read "cd: " eshell-dirs)))))))
;; Consult directory navigation:2 ends here

;; [[file:../Sacha.org::#correctly-complete-commands-in-subdirectories][Correctly complete commands in subdirectories:1]]
;;;###autoload
(defun eshell--complete-commands-list ()
		"Generate list of applicable, visible commands."
		(let ((filename (pcomplete-arg)) glob-name)
			(if (file-name-directory filename)
					(if eshell-force-execution
							(pcomplete-dirs-or-entries nil #'file-readable-p)
						(pcomplete-executables))
				(if (and (> (length filename) 0)
								 (eq (aref filename 0) eshell-explicit-command-char))
						(setq filename (substring filename 1)
									pcomplete-stub filename
									glob-name t))
				(let* ((paths (eshell-get-path))
							 (cwd (file-name-as-directory
										 (expand-file-name default-directory)))
							 (path "") (comps-in-path ())
							 (file "") (filepath "") (completions ()))
					;; Go thru each path in the search path, finding completions.
					(while paths
						(setq path (file-name-as-directory
												(expand-file-name (or (car paths) ".")))
									comps-in-path
									(and (file-accessible-directory-p path)
											 (file-name-all-completions filename path)))
						;; Go thru each completion found, to see whether it should
						;; be used.
						(while comps-in-path
							(setq file (car comps-in-path)
										filepath (concat path file))
							(if (and (not (member file completions)) ;
											 (or (string-equal path cwd)
													 (not (file-directory-p filepath)))
											 (if eshell-force-execution
													 (file-readable-p filepath)
												 (file-executable-p filepath)))
									(setq completions (cons file completions)))
							(setq comps-in-path (cdr comps-in-path)))
						(setq paths (cdr paths)))
					;; Add aliases which are currently visible, and Lisp functions.
					(pcomplete-uniquify-list
					 (if glob-name
							 completions
						 (setq completions
									 (append (if (fboundp 'eshell-alias-completions)
															 (eshell-alias-completions filename))
													 (eshell-winnow-list
														(mapcar
														 (lambda (name)
															 (substring name 7))
														 (all-completions (concat "eshell/" filename)
																							obarray #'functionp))
														nil '(eshell-find-alias-function))
													 completions))
						 (append (and (or eshell-show-lisp-completions
															(and eshell-show-lisp-alternatives
																	 (null completions)))
													(all-completions filename obarray #'functionp))
										 completions)))))))
;; Correctly complete commands in subdirectories:1 ends here

(provide 'sacha-eshell)
;;; sacha-eshell.el ends here
