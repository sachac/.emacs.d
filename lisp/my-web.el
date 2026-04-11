;;;###autoload
(defun my-simple-httpd-remove-temporary-root ()
  "Remove `httpd-root' only if it's a temporary directory."
  (when (file-in-directory-p httpd-root temporary-file-directory)
    (delete-directory httpd-root t)))

;;;###autoload
(defun my-page-title (url)
	"Get the page title for URL. Simplify some titles."
	(condition-case nil
			(pcase url
				((rx "reddit.com") "Reddit")
				((rx "news.ycombinator.com") "HN")
				((rx "lobste.rs") "lobste.rs")
				(_
				 (with-current-buffer (url-retrieve-synchronously url)
					 (string-trim
						(replace-regexp-in-string
						 "[ \n]+" " "
						 (replace-regexp-in-string
							"\\(^Github - \\|:: Sacha Chua\\)" ""
							(or
							 (dom-texts (car
													 (dom-by-tag (libxml-parse-html-region
																				(point-min)
																				(point-max))
																			 'title)))
							 "")))))))
		(error nil)))

(defcustom my-search-web-handler "https://duckduckgo.com/html/?q="
	"How to search. Could be a string that accepts the search query at the end (URL-encoded)
or a function that accepts the text (unencoded)."
	:type '(choice (string :tag "Prefix URL to search engine.")
								 (function :tag "Handler function.")))

;;;###autoload
(defun my-open-url-or-search-web (&optional text-or-url)
	(interactive (list (if (region-active-p)
												 (buffer-substring (region-beginning) (region-end))
											 (or
												(and (derived-mode-p 'org-mode)
														 (let ((elem (org-element-context)))
															 (and (eq (org-element-type elem) 'link)
																		(buffer-substring-no-properties
																		 (org-element-begin elem)
																		 (org-element-end elem)))))
												(thing-at-point 'url)
												(thing-at-point 'email)
												(thing-at-point 'filename)
												(thing-at-point 'word)))))
		(catch 'done
			(let (links)
				(with-temp-buffer
					(insert text-or-url)
					(org-mode)
					(goto-char (point-min))
					;; We add all the links to a list first because following them may change the point
					(while (re-search-forward org-any-link-re nil t)
						(cl-pushnew (match-string-no-properties 0) links))
					(when links
						(dolist (link links)
							(org-link-open-from-string link))
						(throw 'done links))
					;; Try emails
					(while (re-search-forward thing-at-point-email-regexp nil t)
						(cl-pushnew (match-string-no-properties 0) links))
					(when links
						(compose-mail (string-join links ", "))
						(throw 'done links)))
				;; Open filename if specified, or do a web search
				(cond
				 ((ffap-guesser) (find-file-at-point))
				 ((functionp my-search-web-handler)
					(funcall my-search-web-handler text-or-url))
				 ((stringp my-search-web-handler)
					(browse-url (concat my-search-web-handler (url-hexify-string text-or-url))))))))

(defvar my-check-dead-links-skip
  (regexp-opt
   '("news.ycombinator"
     "meejah.ca"
     "reddit"))
  "Some sites block automated link checking.")
(defvar my-check-dead-links-user-agent "Emacs link checker"
  "Some sites may let certain user agents through. Change this as desired.")

;;;###autoload
(defun my-check-dead-links ()
  "Look for dead links."
  (interactive)
  (let (pos)
    (setq pos
          (catch 'done
            (while (re-search-forward ffap-url-regexp nil t)
              (let* ((start (match-beginning 0))
                     (end (match-end 0))
                     (url (or (and (derived-mode-p 'org-mode)
                                   (org-element-property :raw-link (org-element-context)))
                              (ffap-url-at-point)))
                     (browse-url-handlers nil)
                     (url-user-agent my-check-dead-links-user-agent))
                (message "Checking %s" url)
                (unless (or (string-match my-check-dead-links-skip url)
                            (save-match-data
                              (condition-case nil
                                  (url-file-exists-p url)
                                (error nil))))
                  (goto-char start)
                  (browse-url url)
                  (if (y-or-n-p "Continue?")
                      (goto-char end)
                    (throw 'done (cons url (point)))))))))
    (goto-char (cdr pos))))

;;;###autoload
(defun my-rss-get-entries (url)
	"Return a list of the form ((:title ... :url ... :date ...) ...)."
	(with-current-buffer (url-retrieve-synchronously url)
		(set-buffer-multibyte t)
    (goto-char (point-min))
		(when (re-search-forward "<\\?xml\\|<rss" nil t)
			(goto-char (match-beginning 0))
			(sort
			 (let* ((feed (xml-parse-region (point) (point-max)))
							(is-rss (> (length (xml-get-children (car feed) 'entry)) 0)))
				 (if is-rss
						 (mapcar
							(lambda (entry)
								(list
								 :url
								 (or
									(xml-get-attribute
									 (car
										(or
										 (seq-filter (lambda (x) (string= (xml-get-attribute x 'rel) "alternate"))
																 (xml-get-children entry 'link))
										 (xml-get-children entry 'link)))
									 'href)
									(dom-text (dom-by-tag entry 'guid)))
								 :title
								 (elt (car (xml-get-children entry 'title)) 2)
								 :date
								 (date-to-time (elt (car (xml-get-children entry 'updated)) 2))))
							(xml-get-children (car feed) 'entry))
					 (mapcar (lambda (entry)
										 (list
											:url
											(or (caddr (car (xml-get-children entry 'link)))
													(dom-text (dom-by-tag entry 'guid)))
											:title
											(caddr (car (xml-get-children entry 'title)))
											:date
											(date-to-time (elt (car (xml-get-children entry 'pubDate)) 2))))
									 (xml-get-children (car (xml-get-children (car feed) 'channel)) 'item))))
			 :key (lambda (o) (plist-get o :date))
			 :lessp #'time-less-p
			 :reverse t))))

;;;###autoload
(defun my-opml-table (xml)
	(sort
	 (mapcar
		(lambda (o)
			(let ((latest (car (condition-case nil (my-rss-get-entries (dom-attr o 'xmlUrl))
													 (error nil)))))
				(list
				 (if latest
						 (format-time-string "%Y-%m-%d" (plist-get latest :date))
					 "")
				 (org-link-make-string
					(or (dom-attr o 'htmlUrl)
							(dom-attr o 'xmlUrl))
					(replace-regexp-in-string " *|" "" (dom-attr o 'text)))
				 (if latest
						 (org-link-make-string
							(plist-get latest :url)
							(or (plist-get latest :title) "(untitled)"))
					 ""))))
		(dom-search
		 xml
		 (lambda (o)
			 (and
				(eq (dom-tag o) 'outline)
				(dom-attr o 'xmlUrl)
				(dom-attr o 'text)))))
	 :key #'car
	 :reverse t))

;;;###autoload
(defun my-transform-html (functions text)
	"Apply FUNCTIONS to TEXT, which is parsed as HTML.
Each function is called with the DOM and should return a DOM.
Return the resulting HTML as a string."
	(with-temp-buffer
		(when (stringp text)
				(insert (concat "<div>"
												text
												"</div>")))
		(let ((dom (if (stringp text) (libxml-parse-html-region (point-min) (point-max))
								 text))) ; might already be a DOM
			(erase-buffer)
			(svg-print (seq-reduce
									(lambda (prev val)
										(funcall val prev))
									(or functions my-transform-html-clipboard-functions)
									dom))
			(buffer-string))))

(defvar my-transform-html-clipboard-functions nil "List of functions to call with the clipboard contents.
Each function should take a DOM node and return the resulting DOM node.")
;; Rich text can sometimes be finicky to paste, so maybe I'll default to working with plain text
;; if there's a code view I can use to paste in the HTML.
(defvar my-transform-html-clipboard-rich-text nil
	"Non-nil means copy as rich text instead of plain HTML.")
;;;###autoload
(defun my-transform-html-clipboard (&optional activate-app-afterwards functions text
																							as-rich-text)
	"Parse clipboard contents and transform it.
This calls FUNCTIONS, defaulting to `my-transform-html-clipboard-functions'.
If ACTIVATE-APP-AFTERWARDS is non-nil, use xdotool to try to activate that app's window."
	(when (region-active-p) (setq text (buffer-substring (region-beginning) (region-end))))
	(unless text
		(setq text (shell-command-to-string "unbuffer -p xclip -o -selection clipboard -t text/html 2>& /dev/null")))
	(when (string= text "") (error "Clipboard does not contain HTML."))
	(with-temp-buffer
		(insert (my-transform-html functions text))
		(if (or as-rich-text my-transform-html-clipboard-rich-text)
				(shell-command-on-region
					 (point-min) (point-max)
					 "xclip -i -selection clipboard -t text/html -filter 2>& /dev/null")
			(kill-new (buffer-substring-no-properties (point-min) (point-max)))))
	(when activate-app-afterwards
		(call-process "xdotool" nil nil nil "search" "--onlyvisible" "--all" activate-app-afterwards "windowactivate" "windowfocus")))

;; Hmm, now I need to modify it to handle emojis in the text.
;; Emojis have :text: in the alt. I need to upload them.

;;;###autoload
(defun my-transform-html-save-images (dom dir &optional file-prefix transform-fn)
  "Returns a list of (section list).
list is a list of alists with the following keys:
- type (main, extra, emoji)
- alt
- filename"
	(let (last-image last-image-filename last-image-alt results last-image-node)
		(dom-search
     dom
		 (lambda (node)
			 (pcase (dom-tag node)
				 ('img
					(let* ((data (dom-attr node 'src))
                 (jpg (concat (file-name-sans-extension data)
                             ".jpg")))
            (when (file-exists-p jpg)
              (dom-set-attribute node 'src jpg)
              (setq data jpg))
            (when (or last-image last-image-filename)
              (cond
               ;; this image is after another image, so the previous image must be an extra one
							 (last-image
								(setq last-image-filename
											(expand-file-name
											 (format "%s%s-extra.%s"
															 (or file-prefix "")
															 (if transform-fn
																	 (funcall transform-fn (caar results))
																 (caar results))
															 (car last-image))
											 dir))
								(with-temp-file last-image-filename
									(set-buffer-file-coding-system 'binary)
									(insert (base64-decode-string (cdr last-image)))))
							 ;; this image is after another image, so the previous image must be an extra one
							 (last-image-filename
								(let ((new-filename
											 (expand-file-name
												(format "%s%s-extra.%s"
																(or file-prefix "")
																(if transform-fn
																		(funcall transform-fn (caar results))
																	(caar results))
																"jpg")
												dir)))
									(call-process "convert" nil nil nil last-image-filename
																new-filename)
									(setq last-image-filename new-filename))))
              (push
               (list
                (cons 'type 'extra)
                (cons 'filename last-image-filename)
                (cons 'alt last-image-alt))
               (cdr (car results)))
              (dom-remove-node dom node)
              (setq last-image-node nil)
              (setq last-image nil last-image-filename nil))
						(cond
             ((string-match "^:.+?:" (or (dom-attr node 'alt) ""))
              (setq last-image-node nil)
              (push
               (list
                (cons 'type 'emoji)
                (cons 'filename (dom-attr node 'src))
                (cons 'alt (dom-attr node 'alt)))
               (cdar results)))
						 ((string-match "^images/" data)
							(setq last-image nil
										last-image-filename data
										last-image-alt (dom-attr node 'alt)
                    last-image-node node))
						 ((string-match "^data:image/" data)
							(with-temp-buffer
								(insert data)
								(goto-char (point-min))
								(when (looking-at "data:image/\\([^;]+?\\);base64,")
									(setq last-image (cons (match-string 1)
																				 (buffer-substring (match-end 0) (point-max)))
												last-image-filename nil
												last-image-alt (dom-attr node 'alt)
                        last-image-node node)))))))
				 ('h2
					(when (not (string= (string-trim (dom-texts node)) ""))
						(cond
						 (last-image
							(setq last-image-filename
										(expand-file-name
										 (format "%s%s.%s"
														 (or file-prefix "")
														 (if transform-fn
																 (funcall transform-fn (dom-texts node))
															 (dom-texts node))
														 (car last-image))
										 dir))
							(with-temp-file last-image-filename
								(set-buffer-file-coding-system 'binary)
								(insert (base64-decode-string (cdr last-image)))))
						 (last-image-filename
							(let ((new-filename
										 (expand-file-name
											(format "%s%s.%s"
															(or file-prefix "")
															(if transform-fn
																	(funcall transform-fn (dom-texts node))
																(dom-texts node))
															(file-name-extension last-image-filename))
											dir)))
                (copy-file last-image-filename new-filename t)
								(setq last-image-filename new-filename))))
						(push (cons (string-trim (dom-texts node))
												`(((filename . ,last-image-filename)
					                 (alt . ,last-image-alt)
                           (type . main))))
									results)
            (when last-image-node
              (dom-remove-node dom last-image-node))
						(setq last-image nil
                  last-image-node nil
									last-image-filename nil))))))
		(nreverse results)))


;;;###autoload
(defun my-transform-html-slugify (s)
	(downcase
	 (replace-regexp-in-string
		"^-+\\|-$" ""
		(replace-regexp-in-string
		 "[^A-Za-z0-9]+" "-"
		 (string-trim s)))))
(defvar my-brigade-newsletter-images-directory "~/proj/bike-brigade/newsletter/images")
;;;###autoload
(defun my-brigade-newsletter-heading-to-image-file-name (heading)
	(replace-regexp-in-string
	 "[^-a-z0-9]" ""
	 (replace-regexp-in-string
		" +"
		"-"
		(string-trim (downcase heading)))))
;;;###autoload
(defun my-brigade-save-newsletter-images (dom)
	(my-transform-html-save-images
	 dom
	 my-brigade-newsletter-images-directory
	 (concat (substring (org-read-date nil nil "+Sun") 0 10)
					 "-news-")
	 #'my-transform-html-slugify))

;;;###autoload
(defun my-transform-html-remove-images (dom)
	(dolist (img (dom-by-tag dom 'img))
		(dom-remove-node dom img))
	dom)

;;;###autoload
(defun my-transform-html-remove-italics (dom)
	(dolist (node (dom-by-tag dom 'i))
		(dom-remove-node dom node))
	dom)

;;;###autoload
(defun my-html-extract-css-rules (dom)
  "Extract CSS rules and return a hash table mapping class names to properties."
  (let* ((css-rules (make-hash-table :test 'equal))
				 (css-content (dom-texts (car (dom-by-tag dom 'style))))
				 (len (length css-content))
				 (start 0))
    (while (and (< start len)
								(string-match "\\([^{]+\\){\\([^}]+\\)}" css-content start))
      (let ((selector (match-string 1 css-content))
            (properties (match-string 2 css-content)))
        (puthash selector properties css-rules)
        (setq start (match-end 0))))
    css-rules))

;;;###autoload
(defun my-brigade-convert-span-style (node rules)
	(when (and (dom-attr node 'class)
						 (not (string= (string-trim (dom-texts node)) "")))
		(let ((styles
					 (when (dom-attr node 'class)
						 (string-join
							(seq-keep (lambda (class-name)
													(let ((prop (gethash (concat "." class-name) rules)))
														(when (and prop
																			 (string-match "font-weight:700\\|font-style:italic" prop))
															prop)))
												(split-string (dom-attr node 'class)))
							";"))))
			(unless (or (null styles) (string= styles ""))
				(dom-set-attribute node 'style styles)
				(dom-remove-attribute node 'class)
				node))))

;;;###autoload
(defun my-brigade-simplify-html (dom)
	(let ((css-rules (my-html-extract-css-rules dom)))
		(dolist (tag '(li b ul span p a h2 div))
			(dolist (node (dom-by-tag dom tag))
				(or (my-brigade-convert-span-style node css-rules)
						(dolist (attr '(style class id))
							(when (dom-attr node attr)
								(dom-remove-attribute node attr))))))
		;; remove comments
    (mapc (lambda (node) (dom-remove-node (dom-parent dom node) node))
          (dom-by-tag dom 'sup))
		;; remove blank paragraphs
		(dom-search
		 dom
		 (lambda (node)
			 (when (eq (dom-tag node) 'p))
			 (when (and (string= (string-trim (dom-texts node)) "")
									(not (dom-by-tag node 'img)))
				 (dom-remove-node (dom-parent dom node) node))))
		;; fix links
		(dolist (node (dom-by-tag dom 'a))
			(when (string-match "https://www\\.google\\.com\\/url" (dom-attr node 'href))
				(let ((args (url-parse-query-string
										 (cdr (url-path-and-query (url-generic-parse-url (dom-attr node 'href)))))))
					(dom-set-attribute node 'href (car (assoc-default "q" args 'string=))))))
		dom))

;; based on https://www.reddit.com/r/emacs/comments/57nps0/comment/d8umsr4/?context=3
;;;###autoload
(defun my-imp-htmlize-filter (buffer)
  "Alternate htmlization of BUFFER before sending to clients."
  ;; leave the result in the current-buffer
  (let ((noninteractive t)
        (org-export-use-babel nil)
        (m (with-current-buffer buffer major-mode)))
    (case m
      (org-mode
       (insert
        (with-current-buffer buffer
          (org-export-as 'html))))
      (t
       (let ((html-buffer (save-match-data (htmlize-buffer buffer))))
         (insert-buffer-substring html-buffer)
         (kill-buffer html-buffer))))))

;;;###autoload
(defun my-impatient-org-export-as-html-filter (buffer)
  (let ((output-buffer (current-buffer))
        (log-message-max nil))
    (with-current-buffer buffer
      (let ((output (org-export-as 'html)))
				(with-current-buffer output-buffer (insert output))))))
