;;;###autoload
  (defun my-consult-omni-bookmarks-builder (input &rest args &key callback &allow-other-keys)
          (let* ((quoted (when input (regexp-quote input)))
                                   (list (my-org-bookmarks))
                                   (candidates
                                          (mapcar
                                           (lambda (o)
                                                   (propertize
                                                          (concat (plist-get o :title) "\s"
                                                                                          (plist-get o :url))
                                                          :source "Bookmarks"
                                                          :on-callback 'my-consult-org-bookmark-visit
                                                          :title (plist-get o :title)
                                                          :url (plist-get o :url)))
                                           (if quoted
                                                           (seq-filter
                                                                  (lambda (o)
                                                                          (string-match quoted (concat (plist-get o :title) " - " (plist-get o :title))))
                                                                  list)
                                                   list))))
                  (when callback (funcall callback candidates))
                  candidates))

;;;###autoload
  (defun my-consult-org-bookmark-visit (o)
          (browse-url (get-text-property 0 :url o)))

  ;; (consult--multi (list my-consult--source-org-bookmark))
  (with-eval-after-load 'consult-omni
    (consult-omni-define-source
           "My Org bookmarks"
     :narrow-char ?b
     :type 'sync
           :request #'my-consult-omni-bookmarks-builder
           :on-return 'my-consult-org-bookmark-visit
           :group #'consult-omni--group-function
     :min-input 1
     :require-match t))

;;;###autoload
  (defun my-consult-omni-blog-data ()
          (let ((base (replace-regexp-in-string "/$" "" my-blog-base-url))
                                  (json-object-type 'alist)
                                  (json-array-type 'list))
                  (mapcar
                   (lambda (o)
                           (list :url (concat base (alist-get 'permalink o))
                                                   :title (alist-get 'title o)
                                                   :date (alist-get 'date o)))
                   (sort (json-read-file "~/sync/static-blog/_site/blog/all/index.json")
                                           (lambda (a b)
                                                   (string< (or (alist-get 'date b) "")
                                                                                          (or (alist-get 'date a) "")))))))
  (unless (get 'my-consult-omni-blog-data :memoize-original-function)
          (memoize #'my-consult-omni-blog-data "5 minutes"))

;;;###autoload
  (defun my-consult-omni-blog-titles-builder (input &rest args &key callback &allow-other-keys)
          (let* ((quoted (when input (regexp-quote input)))
                                   (list
                                          (if quoted
                                                          (seq-filter
                                                           (lambda (o)
                                                                   ;; TODO: Someday figure out orderless?
                                                                   (string-match quoted (concat (plist-get o :title) " - " (plist-get o :title))))
                                                           (my-consult-omni-blog-data))
                                                  (my-consult-omni-blog-data)))
                                   (candidates
                                          (mapcar
                                           (lambda (o)
                                                   (propertize
                                                          (concat (plist-get o :title))
                                                          :source "Blog"
                                                          :date (plist-get o :date)
                                                          :title (plist-get o :title)
                                                          :url (plist-get o :url)))
                                           (if quoted (seq-take list 3) list))))
                  (when callback (funcall callback candidates))
                  candidates))

;;;###autoload
  (defun my-consult-omni-blog-annotation (s)
          (format " (%s)"
                                          (propertize (substring (or (get-text-property 0 :date s) "") 0 4)
                                                                                          'face 'completions-annotations)))

  (with-eval-after-load 'consult-omni
    (consult-omni-define-source
           "Blog"
           :narrow-char ?b
     :type 'sync
           :request #'my-consult-omni-blog-titles-builder
           :on-return 'my-consult-org-bookmark-visit
     :group #'consult-omni--group-function
           :annotate #'my-consult-omni-blog-annotation
     :min-input 3
           :sort nil
     :require-match t))

;;;###autoload
  (cl-defun my-consult-omni--google-blog-fetch-results (input &rest args &key callback &allow-other-keys)
    "Fetches search results for INPUT from “Google Custom Search” service.
  Narrows to `my-blog-base-url'.

  Refer to URL `https://programmablesearchengine.google.com/about/' and
  URL `https://developers.google.com/custom-search/' for more info."
    (pcase-let* ((`(,query . ,opts)
                  (consult-omni--split-command input (seq-difference args (list :callback callback))))
                 (opts (car-safe opts))
                 (count (plist-get opts :count))
                 (page (plist-get opts :page))
                 (filter (plist-get opts :filter))
                 (count (or (and count (integerp (read count)) (string-to-number count))
                            consult-omni-default-count))
                 (page (or (and page (integerp (read page)) (string-to-number page))
                           consult-omni-default-page))
                 (filter (or (and (integerp filter) filter)
                             (and filter (string-to-number (format "%s" filter)))
                             1))
                 (filter (if (member filter '(0 1)) filter 1))
                 (count (min count 10))
                 (page (+ (* page count) 1))
                 (page (min page (- 100 count)))
                 (params `(("q" . ,(format "site:%s+%s"
                                           (url-encode-url my-blog-base-url)
                                           (replace-regexp-in-string " " "+" query)))
                           ("key" . ,(consult-omni-expand-variable-function consult-omni-google-customsearch-key))
                           ("cx" . ,(consult-omni-expand-variable-function consult-omni-google-customsearch-cx))
                           ("gl" . "en")
                           ("filter" . ,(format "%s" filter))
                           ("num" . ,(format "%s" count))
                           ("start" . ,(format "%s" page))))
                 (headers '(("Accept" . "application/json")
                            ("Accept-Encoding" . "gzip")
                            ("User-Agent" . "consult-omni (gzip)"))))
      (consult-omni--fetch-url
       consult-omni-google-customsearch-api-url consult-omni-http-retrieve-backend
       :encoding 'utf-8
       :params params
       :headers headers
       :parser #'consult-omni--json-parse-buffer
       :callback
       (lambda (attrs)
         (let* ((raw-results (gethash "items" attrs))
                (annotated-results
                 (mapcar (lambda (item)
                           (let*
                               ((source "Google")
                                (url (format "%s" (gethash "link" item)))
                                (title (format "%s" (gethash "title" item)))
                                (snippet (string-trim (format "%s" (gethash "snippet" item))))
                                (search-url (consult-omni--make-url-string consult-omni-google-search-url params '("key" "cx" "gl")))
                                (decorated (funcall consult-omni-default-format-candidate :source source :query query :url url :search-url search-url :title title :snippet snippet)))
                             (propertize decorated
                                         :source source
                                         :title title
                                         :url url
                                         :search-url search-url
                                         :query query
                                         :snippet snippet)))
                         raw-results)))
           (when (and annotated-results (functionp callback))
             (funcall callback annotated-results))
           annotated-results)))))

  (use-package consult-omni
          :load-path "~/vendor/consult-omni"
          :after (consult embark)
          :config
    (consult-omni-define-source
     "Google my blog"
     :narrow-char ?b
     :type 'dynamic
     :require-match nil
     :face 'consult-omni-engine-title-face
     :request #'my-consult-omni--google-blog-fetch-results
     :on-new (apply-partially #'consult-omni-external-search-with-engine "Google")
     :preview-key consult-omni-preview-key
     :search-hist 'consult-omni--search-history
     :select-hist 'consult-omni--selection-history
     :enabled (lambda () (bound-and-true-p consult-omni-google-customsearch-key))
     :group #'consult-omni--group-function
     :sort t
     :interactive consult-omni-intereactive-commands-type
     :annotate nil))

;;;###autoload
(defun my-consult-ripgrep-code ()
  (interactive)
	(consult-ripgrep (mapcar 'car my-project-web-base-list)))

;;;###autoload
(defun my-consult-flymake-project ()
  (interactive)
  (consult-flymake t))
