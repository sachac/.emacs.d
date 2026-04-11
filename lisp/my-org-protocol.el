;;;###autoload
(defun my-org-protocol-insert-link (info)
  "Store and insert the link at point based on INFO."
  (org-protocol-store-link info)
  (with-current-buffer (window-buffer (selected-window))
    (insert "- ")
    (org-insert-last-stored-link 1)
    (insert "\n")))
(eval-after-load 'org-protocol
  '(add-to-list 'org-protocol-protocol-alist
                '("insert-link" :protocol "insert-link" :function my-org-protocol-insert-link)))

;; javascript:location.href = 'org-protocol://copy-thumbnail?thumbnail=' + encodeURIComponent(document.querySelector('meta[property=\"og:image\"]') ? document.querySelector('meta[property=\"og:image\"]').getAttribute('content') : '') + '&title=' + encodeURIComponent(document.title) + '&url=' + encodeURIComponent(location.href) + '&videoId=' + ((typeof(videoId) !== 'undefined' ? videoId : (document.querySelector('meta[itemprop=\"videoId\"]') ? document.querySelector('meta[itemprop=\"videoId\"]').getAttribute('content') : '')) || '')


;;;###autoload
(defun my-get-youtube-info (url)
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
(defun my-link-video (list)
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
(defun my-org-protocol-copy-thumbnail (info)
  "Store and insert the link at point based on INFO."
  (interactive "MURL: ")
  (when (stringp info) (setq info (list :url info)))
  (when (string-match "youtube\\.com" (plist-get info :url))
    (setq info (my-link-video info)))
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

(eval-after-load 'org-protocol
  '(add-to-list 'org-protocol-protocol-alist
                '("copy-thumbnail" :protocol "copy-thumbnail" :function my-org-protocol-copy-thumbnail)))


;;;###autoload
(defun org-protocol-open-link (info)
	"Process an org-protocol://open style url with INFO."
	(org-link-open (car (org-element-parse-secondary-string (plist-get info :link) '(link)))))
;;;###autoload
(defun org-protocol-copy-open-link (arg)
	(interactive "P")
	(kill-new (concat "org-protocol://open?link=" (url-hexify-string (org-store-link arg)))))


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

(with-eval-after-load 'org
	(org-link-set-parameters "org-protocol"
													 :follow #'org-protocol-follow
													 :export #'org-protocol-export))
