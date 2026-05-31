;;; sacha-org-favorites.el ---  -*- lexical-binding: t -*-

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
;; - Favorites
;;   https://sachachua.com/dotemacs#org-bookmarks
;;
;; - Copy a link from my resources.org to the kill ring
;;   https://sachachua.com/dotemacs#org-mode-favorites-copy-a-link-from-my-resources-org-to-the-kill-ring
;;
;;; Code:



;; [[file:../Sacha.org::#org-bookmarks][Favorites:1]]
(defvar sacha-org-favorite-file "~/sync/orgzly/resources.org")

;;;###autoload
(defun sacha-org-favorites ()
	"Returns a list of plists like this: ((:title ... :url ...) ...).
Uses the info from `sacha-org-favorite-file'."
	(delq nil
        (with-current-buffer (find-file-noselect sacha-org-favorite-file)
          (org-map-entries
           (lambda ()
             (let ((title (org-entry-get (point) "ITEM"))
                   (url (or (org-entry-get (point) "URL")
                            (progn
                              (let ((end (save-excursion (org-end-of-subtree))))
                                (when (and (< (point) end)
                                           (re-search-forward org-link-any-re end t))
                                  (org-element-property
                                   :raw-link
                                   (org-element-context))))))))
               (when (string-match org-link-bracket-re title)
                 (setq title (match-string 2 title)))
               (when url
                 (list :title title :url url))))))))

;;;###autoload
(defun sacha-org-favorite-match (s)
  "Return the first favorite that matches S."
  (setq s (downcase s))
  (plist-get (seq-find (lambda (favorite)
                         (string= (downcase (plist-get favorite :title))
                                  s))
                       (sacha-org-favorites))
             :url))

(defun sacha-org-favorites-for-completion ()
	(mapcar
	 (lambda (o)
		 (cons (propertize (format "%s - %s"
															 (plist-get o :title)
															 (plist-get o :url))
											 :title (plist-get o :title)
											 :url (plist-get o :url))
					 (propertize
						(plist-get o :url)
						:title (plist-get o :title)
						:url (plist-get o :url)
						'title (plist-get o :title)
						'url (plist-get o :url))))
	 (sacha-org-favorites)))

;;;###autoload
(defun sacha-org-favorite-complete (&optional initial-text)
	"Complete a favorite."
  (when (region-active-p)
    (setq initial-text (or initial-text (buffer-substring (region-beginning)
                                                          (region-end)))))
	(let ((favorites (sacha-org-favorites-for-completion)))
		(assoc-default
		 (completing-read
			"Favorite: " favorites nil nil initial-text)
		 favorites
		 #'string=)))

;;;###autoload
(defun sacha-org-favorite-insert-description (link &optional description)
	"Provide a default description."
	(or description
			(get-text-property 0 'title link)
			(sacha-page-title link)))

;;;###autoload
(defun sacha-org-favorite-save-link (link title)
  "Save the current link to my resources file."
  (interactive
   (or
    (and (derived-mode-p 'org-mode)
         (let ((elem (org-element-context)))
           (when (eq (org-element-type elem) 'link)
             (list (org-element-property :raw-link elem)
                   (buffer-substring (org-element-contents-begin elem)
                                     (org-element-contents-end elem))))))
    (let* ((url (read-string "URL: "))
           (title (sacha-page-title url)))
      (list url (read-string "Title: " title)))))
  (with-current-buffer (find-file-noselect sacha-org-favorite-file)
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (insert "** " title "\n" link "\n")
    (save-buffer)))
;; Favorites:1 ends here

;; [[file:../Sacha.org::#org-mode-favorites-copy-a-link-from-my-resources-org-to-the-kill-ring][Copy a link from my resources.org to the kill ring:1]]
;;;###autoload
(defun sacha-org-copy-favorite-url (favorite)
  "Copy the URL for the favorite."
  (interactive (list (sacha-org-favorite-complete)))
	(kill-new favorite)
	(message "%s" favorite))
;; Copy a link from my resources.org to the kill ring:1 ends here

(provide 'sacha-org-favorites)
;;; sacha-org-favorites.el ends here
