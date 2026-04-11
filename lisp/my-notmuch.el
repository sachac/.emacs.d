;;;###autoload
(defun my-notmuch-flagged ()
  (interactive)
  (notmuch-search "tag:flagged and not tag:trash"))
;;;###autoload
(defun my-notmuch-inbox ()
  (interactive)
  (notmuch-search "tag:inbox and not tag:trash"))
;;;###autoload
(defun my-notmuch-important-inbox ()
  (interactive)
  (notmuch-search "tag:primary and tag:inbox and not tag:trash"))
;;;###autoload
(defun my-notmuch-search-this-author ()
  (interactive)
  (notmuch-search (format "from:\"%s\""
                          (plist-get (get-text-property (point) 'notmuch-search-result) :authors))))
