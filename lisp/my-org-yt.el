;;;###autoload
(defun my-org-link-youtube-time (url beg end)
  "Link times of the form h:mm to YouTube video at URL.
       Works on region defined by BEG and END."
  (interactive (list (read-string "URL: " (org-entry-get-with-inheritance "YOUTUBE")) (point) (mark)))
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ((char (if (string-match "\\?" url) "&" "?")))
        (while (re-search-forward "\\(\\([0-9]+\\):\\([0-9]+\\)\\(:\\([0-9]+\\)\\)?\\) ::" nil t)
          (replace-match
           (format "[[%s%st=%sh%sm%ss][%s]] "
                   url
                   char
                   (match-string 2)
                   (match-string 3)
                   (or (match-string 5) "0")
                   (match-string 1)) nil t))))))

;;;###autoload
(defun my-clean-up-google-hangout-chat ()
  (interactive)
  (save-excursion
    (while (re-search-forward "<hr.*?div class=\"Kc-Ma-m\".*?>" nil t)
      (replace-match "\n| ")))
  (save-excursion
    (while (re-search-forward "</div><div class=\"Kc-yi-m\">" nil t)
      (replace-match " | ")))
  (save-excursion
    (while (re-search-forward "</div></div><div class=\"Kc-ib\">" nil t)
      (replace-match " | ")))
  (save-excursion
    (while (re-search-forward "<a rel=\"nofollow\" target=\"_blank\" href=\"\\(.*?\\)\">\\(.*?\\)</a>" nil t)
      (replace-match "[[\\1][\\2]]")))
  (save-excursion
    (while (re-search-forward "</div></div></div></div>" nil t)
      (replace-match " |")))
  (save-excursion
    (while (re-search-forward "&nbsp;" nil t)
      (replace-match " ")))
  (save-excursion
    (while (re-search-forward "</div><div class=\"Kc-ib\">" nil t)
      (replace-match " ")))
  (save-excursion
    (while (re-search-forward "<img.*?>" nil t)
      (replace-match "")))
  (save-excursion
    (while (re-search-forward "<wbr>" nil t)
      (replace-match "")))
  )

(defvar my-org-yt-iframe-format
  (concat "<div class=\"yt-video\"><iframe width=\"456\""
          " height=\"315\""
					" title=""YouTube video player\""
          " src=\"https://www.youtube-nocookie.com/embed/%s?enablejsapi=1\""
          " frameborder=\"0\""
          " allowfullscreen>%s</iframe><a href=\"%s\">Watch on YouTube</a></div>"))


;;;###autoload
(defun my-org-yt-id (path)
	(cond
	 ((string-match "\\(?:v=\\|tu\\.be/\\|live/\\)\\([^&]+\\)" path)
		(match-string 1 path))
	 ((string-match "\\(live_stream\\?channel.*\\)" path)
		(match-string 1 path))
	 (t path)))

;;;###autoload
(defun my-org-yt-export (path desc format _)
	"Export time link or embed."
	(pcase format
		((or 'html '11ty 'md)
		 (cond
			(desc (format "<a href=\"%s\">%s</a>" path (or desc path)))
			(t
			 (let* ((path-and-query (url-path-and-query (url-generic-parse-url path)))
							(url (car path-and-query))
							(params (and (cdr path-and-query) (url-parse-query-string (cdr path-and-query))))
							(id (cond
									 ((string-match "\\(?:v=\\|tu\\.be/\\|live/\\)\\([^&]+\\)" path)
										(match-string 1 path))
									 ((string-match "\\(live_stream\\?channel.*\\)" path)
										(match-string 1 path))
									 (t path)))
							(width (or (car (assoc-default "width" params 'string=)) "456"))
							(height (or (car (assoc-default "height" params 'string=)) "315"))
							(time (assoc-default "t" params 'string=)))
				 (if time
						 (format "<a href=\"%s\">%s</a>" path (or desc path))
					 (format "<div class=\"yt-video\"><iframe width=\"%s\" height=\"%s\" title=\"YouTube video player\" src=\"https://www.youtube-nocookie.com/embed/%s?enablejsapi=1\" frameborder=\"0\" allowfullscreen>%s</iframe><a href=\"%s\">Watch on YouTube</a></div>"
									 width height id desc path))))))
		('ascii
		 desc)))

;;;###autoload
(defun my-org-yt-convert-time (time)
	(let ((split-time (reverse (split-string time ":"))))
		(format "%sh%sm%ss"
						(or (elt split-time 2) "0")
						(or (elt split-time 1) "0")
						(or (elt split-time 0) "0"))))
(ert-deftest my-org-yt-convert-time ()
	(should
	 (string=
		(my-org-yt-convert-time "1:02")
		"0h1m02s")))

;;;###autoload
(defun my-org-yt-complete ()
	"Prompt for a timestamp and link to a video."
	(interactive)
	(let* ((url (read-string "URL: " (when (derived-mode-p 'org-mode)
																		 (org-entry-get (point) "YOUTUBE"))))
				 (time (read-string "Time: "))
				 (split-time (reverse (split-string time ":"))))
		(concat "yt:"
						url
						(if (string= time "")
								""
							(concat
							 (if (string-match "\\?" url) "&t=" "?t=")
							 (format "%sh%sm%ss"
											 (or (elt split-time 2) "0")
											 (or (elt split-time 1) "0")
											 (or (elt split-time 0) "0")))))))

;;;###autoload
(defun my-org-yt-insert-description (link &optional description)
	(unless description
		(when (string-match "t=\\([0-9hms]+\\)" link)
			(let ((split-time (cdr (reverse (split-string (match-string 1 link) "[hms]")))))
				(concat
				 (if (and (elt split-time 2) (not (string= (elt split-time 2) "0")))
						 (concat (elt split-time 2) ":")
					 "")
				 (if (elt split-time 1)
						 (concat (if (and (and (elt split-time 2) (not (string= (elt split-time 2) "0")))
															(< (length (elt split-time 1)) 2))
												 "0" "")
										 (elt split-time 1) ":")
					 "")
				 (concat (if (and (elt split-time 1) (< (length (elt split-time 0)) 2)) "0" "")
								 (elt split-time 0)))))))
(ert-deftest my-org-yt-insert-description ()
	(should
	 (string=
		(my-org-yt-insert-description "yt:somevideo?t=0h1m2s")
		"1:02"))
	(should
	 (string=
		(my-org-yt-insert-description "yt:somevideo?t=1h2m3s")
		"1:02:03")))

;;;###autoload
(defun my-org-yt-open (path)
	(browse-url path))

(require 'dash)

;;;###autoload
(defun my-org-insert-youtube-video-with-transcript (url)
  (interactive "MURL: ")
  (let* ((id (if (string-match "\\(?:v=\\|youtu\\.be/\\)\\([^&]+\\)" url) (match-string 1 url) url))
         (temp-file (make-temp-name "org-youtube-"))
         (temp-file-name (concat temp-file ".en.srv1"))
         data)
    (when (and (call-process "yt-dlp" nil nil nil
                             "--write-sub" "--write-auto-sub"  "--no-warnings" "--sub-lang" "en" "--skip-download" "--sub-format" "srv1"
                             "-o" temp-file
                             (format "https://youtube.com/watch?v=%s" id))
               (file-exists-p temp-file-name))
      (insert
       (format "#+begin_export html
<iframe width=\"560\" height=\"315\" src=\"https://www.youtube.com/embed/%s\" title=\"YouTube video player\" frameborder=\"0\" allow=\"accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture\" allowfullscreen></iframe>\n#+end_export\n" id)
       "\n"
       (mapconcat (lambda (o)
                    (format "| [[https://youtube.com/watch?v=%s&t=%ss][%s]] | %s |\n"
                            id
                            (dom-attr o 'start)
                            (my-msecs-to-timestamp (* 1000 (string-to-number (dom-attr o 'start))))
                            (->> (dom-text o)
                                 (replace-regexp-in-string "[ \n]+" " ")
                                 (replace-regexp-in-string "&#39;" "'")
                                 (replace-regexp-in-string "&quot;" "\""))))
                  (dom-by-tag (xml-parse-file temp-file-name) 'text)
                  ""))
      (delete-file temp-file-name))))
