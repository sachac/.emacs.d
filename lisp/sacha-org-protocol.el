;;; sacha-org-protocol.el ---  -*- lexical-binding: t -*-

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
;; - Links from org-protocol
;;   https://sachachua.com/dotemacs#links-from-org-protocol
;;
;; - Org protocol: following Org links from outside Emacs
;;   https://sachachua.com/dotemacs#org-protocol-open
;;
;;; Code:



;; [[file:../Sacha.org::#links-from-org-protocol][Links from org-protocol:1]]
;;;###autoload
(defun sacha-org-protocol-insert-link (info)
  "Store and insert the link at point based on INFO."
  (org-protocol-store-link info)
  (with-current-buffer (window-buffer (selected-window))
    (insert "- ")
    (org-insert-last-stored-link 1)
    (insert "\n")))
(eval-after-load 'org-protocol
  '(add-to-list 'org-protocol-protocol-alist
                '("insert-link" :protocol "insert-link" :function sacha-org-protocol-insert-link)))

;; javascript:location.href = 'org-protocol://copy-thumbnail?thumbnail=' + encodeURIComponent(document.querySelector('meta[property=\"og:image\"]') ? document.querySelector('meta[property=\"og:image\"]').getAttribute('content') : '') + '&title=' + encodeURIComponent(document.title) + '&url=' + encodeURIComponent(location.href) + '&videoId=' + ((typeof(videoId) !== 'undefined' ? videoId : (document.querySelector('meta[itemprop=\"videoId\"]') ? document.querySelector('meta[itemprop=\"videoId\"]').getAttribute('content') : '')) || '')


;;;###autoload
(defun sacha-get-youtube-info (url)
  (with-current-buffer (url-retrieve-synchronously url)
    (goto-char (point-min))
    (prog1
        (list
         :url
         url
         :title
         (when (re-search-forward "<title>\\(.*?\\)</title>" nil t)
           (match-string 1))
         :duration
         (when (re-search-forward "approxDurationMs\":\"\\([0-9]+\\)\"" nil t)
           (format-seconds "%.2h:%.2m:%.2s%z" (/ (string-to-number (match-string 1)) 1000))))
      (kill-buffer))))

;;;###autoload
(defun sacha-link-video (list)
  (when (stringp list) (setq list (list :url list)))
  (with-current-buffer (url-retrieve-synchronously (concat "https://video.link/bookmarklet?url=" (url-encode-url (plist-get list :url))))
    (save-excursion
      (if (re-search-forward "<input type=\"text\" id=\"safeURL\" readonly=\"readonly\" value=\"\\(.*?\\)\"" nil t)
          (plist-put list :url (match-string-no-properties 1))
        (plist-put list :url (replace-regexp-in-string "watch" "watch_popup" (plist-get list :url)))))
    (when (string= (or (plist-get list :thumbnail) "") "")
      (save-excursion
        (when (re-search-forward "<img id=\"videoThumb\" src=\"\\(.*?\\)\"" nil t)
          (plist-put list :thumbnail (match-string-no-properties 1)))))
    list))

;;;###autoload
(defun sacha-org-protocol-copy-thumbnail (info)
  "Store and insert the link at point based on INFO."
  (interactive "MURL: ")
  (when (stringp info) (setq info (list :url info)))
  (when (string-match "youtube\\.com" (plist-get info :url))
    (setq info (sacha-link-video info)))
  (let ((date (format-time-string "%Y-%m-%d")))
    (kill-new
     (if (string= (plist-get info :videoId) "")
         (format "{{<thumbnail image=\"%s\" title=\"%s\" link=\"%s\" date=\"%s\">}}\n"
                 (plist-get info :thumbnail)
                 (plist-get info :title)
                 (plist-get info :url)
                 date
                 )
       (format "{{<youtube id=\"%s\" title=\"%s\" link=\"%s\" date=\"%s\">}}\n"
               (plist-get info :videoId)
               (plist-get info :title)
               (plist-get info :url)
               date))))
  nil)
;; Links from org-protocol:1 ends here

;; [[file:../Sacha.org::#links-from-org-protocol][Links from org-protocol:2]]
(eval-after-load 'org-protocol
  '(add-to-list 'org-protocol-protocol-alist
                '("copy-thumbnail" :protocol "copy-thumbnail" :function sacha-org-protocol-copy-thumbnail)))

;; Links from org-protocol:2 ends here

;; [[file:../Sacha.org::org-protocol-link][org-protocol-link]]
;;;###autoload
(defun org-protocol-open-link (info)
	"Process an org-protocol://open style url with INFO."
	(org-link-open (car (org-element-parse-secondary-string (plist-get info :link) '(link)))))
;;;###autoload
(defun org-protocol-copy-open-link (arg)
	(interactive "P")
	(kill-new (concat "org-protocol://open?link=" (url-hexify-string (org-store-link arg)))))

;; org-protocol-link ends here

;; [[file:../Sacha.org::#org-protocol-open][Org protocol: following Org links from outside Emacs:3]]
(defun org-protocol-follow (path &rest _)
	"Follow the org-protocol link for PATH."
	(org-protocol-check-filename-for-protocol (concat "org-protocol:" path) nil nil))

(defun org-protocol-export (path desc format info)
	"Export an org-protocol link."
	(setq path (concat "org-protocol:" path))
	(setq desc (or desc path))
	(pcase format
    (`html (format "<a href=\"%s\">%s</a>" path desc))
		(`11ty (format "<a href=\"%s\">%s</a>" path desc))
    (`latex (org-latex-link path desc info))
    (`ascii (org-ascii-link path desc info))
		(`md (org-md-link path desc info))
    (_ path)))
;; Org protocol: following Org links from outside Emacs:3 ends here

;; [[file:../Sacha.org::#org-protocol-open][Org protocol: following Org links from outside Emacs:4]]
(with-eval-after-load 'org
	(org-link-set-parameters "org-protocol"
													 :follow #'org-protocol-follow
													 :export #'org-protocol-export))
;; Org protocol: following Org links from outside Emacs:4 ends here

(provide 'sacha-org-protocol)
;;; sacha-org-protocol.el ends here
