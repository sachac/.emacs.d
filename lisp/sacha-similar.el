;;; sacha-similar.el ---  -*- lexical-binding: t -*-

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



;; [[file:../Sacha.org::#org-mode-vector-search][Vector search:2]]
;;;###autoload
(defun sacha-org-db-v3-to-emacs-rag-search (query &optional limit filename-pattern)
  "Search org-db-v3 and transform the data to look like emacs-rag-search's output."
  (org-db-v3-ensure-server)
  (setq limit (or limit 100))
  (mapcar (lambda (o)
            `((source_path . ,(assoc-default 'filename o))
              (line_number . ,(assoc-default 'begin_line o))
              ,@o))
          (sort
           (assoc-default 'results
                          (plz 'post (concat (org-db-v3-server-url) "/api/search/semantic")
                            :headers '(("Content-Type" . "application/json"))
                            :body (json-encode `((query . ,query)
                                                 (limit . ,limit)
                                                 (filename_pattern . ,filename-pattern)))
                            :as #'json-read))
           :key (lambda (o) (alist-get 'similarity_score o))
           :reverse t)))
;; Vector search:2 ends here

;; [[file:../Sacha.org::#org-mode-vector-search-indexing-related-code][Indexing-related code:3]]
;; Based on org-db-v3-reindex-database
;;;###autoload
(defun sacha-org-db-v3-remove-missing-files ()
  "Remove missing files currently in the database.
Fetches the list of files from the server and reindexes each one.
Also removes files that no longer exist from the database.
Uses non-blocking queue processing to keep Emacs responsive.
Skips remote Tramp files."
  (interactive)
  (org-db-v3-ensure-server)
  (plz 'get (concat (org-db-v3-server-url) "/api/files")
    :as #'json-read
    :then (lambda (response)
            (let* ((files (alist-get 'files response))
                   (count (length files))
                   (missing-files nil)
                   (existing-files nil)
                   (remote-files nil))

              ;; Classify files as existing, missing, or remote
              (dotimes (i count)
                (let ((filename (alist-get 'filename (elt files i))))
                  (cond
                   ((file-remote-p filename)
                    (push filename remote-files))
                   ((file-exists-p filename)
                    (push filename existing-files))
                   (t
                    (push filename missing-files)))))
              (if (zerop count)
                  (message "No files found in database")
                (when missing-files
                  (message "Removing %d missing file%s from database..."
                           (length missing-files)
                           (if (= (length missing-files) 1) "" "s"))
                  (dolist (filename missing-files)
                    (org-db-v3-delete-file-async filename)))
                )))
    :else (lambda (error)
            (message "Error fetching file list: %s" (plz-error-message error)))))
;; Indexing-related code:3 ends here

;; [[file:../Sacha.org::sacha-blog-similar-link][sacha-blog-similar-link]]
;;;###autoload
(defun sacha-blog-similar-link (link)
  "Vector-search blog posts using `emacs-rag-search' and insert a link.
If called with \\[universal-argument\], use the current post's text.
If a region is selected, use that as the default QUERY.
HIDE-INITIAL means hide the initial query, which is handy if the query is very long."
  (interactive (list
                (if embark--command
                    (read-string "Link: ")
                  (sacha-blog-similar
                   (cond
                    (current-prefix-arg (sacha-11ty-post-text))
                    ((region-active-p)
                     (buffer-substring (region-beginning)
                                       (region-end))))
                   current-prefix-arg))))
  (sacha-embark-blog-insert-link link))

;;;###autoload
(defun sacha-embark-blog--inject-target-url (&rest args)
  "Replace the completion text with the URL."
  (delete-minibuffer-contents)
  (insert (sacha-blog-url (get-text-property 0 'consult--candidate (plist-get args :target)))))

;;;###autoload
(defun sacha-11ty-interactive-context (use-post)
  "Returns (query hide-initial) for use in interactive arguments.
If USE-POST is non-nil, query is the current post text and hide-initial is t.
If the region is active, returns that as the query."
  (list (cond
         (embark--command (read-string "Input: "))
         (use-post (sacha-11ty-post-text))
         ((region-active-p)
          (buffer-substring (region-beginning)
                            (region-end))))
        use-post))

;;;###autoload
(defun sacha-blog-similar (&optional query hide-initial)
  "Vector-search blog posts using org-db-v3 and present results via Consult.
If called with \\[universal-argument\], use the current post's text.
If a region is selected, use that as the default QUERY.
HIDE-INITIAL means hide the initial query, which is handy if the query is very long."
  (interactive (sacha-11ty-interactive-context current-prefix-arg))
  (consult--read
   (if hide-initial
       (sacha-org-db-v3-blog-post--collection query)
     (consult--dynamic-collection
         #'sacha-org-db-v3-blog-post--collection
       :min-input 3 :debounce 1))
   :lookup #'consult--lookup-cdr
   :prompt "Search blog posts (approx): "
   :category 'sacha-blog
   :sort nil
   :require-match t
   :state (sacha-blog-post--state)
   :initial (unless hide-initial query)))

(defvar sacha-blog-semantic-search-source 'org-db-v3)
;;;###autoload
(defun sacha-org-db-v3-blog-post--collection (input)
  "Perform the RAG search and format the results for Consult.
Returns a list of cons cells (DISPLAY-STRING . PLIST)."
  (let ((posts (sacha-blog-posts)))
    (mapcar (lambda (o)
              (sacha-blog-format-for-completion
               (append o
                       (sacha-blog-post-info-for-url (alist-get 'source_path o)
                                                  posts))))
            (seq-uniq
               (sacha-org-db-v3-to-emacs-rag-search input 100 "%static-blog%")
               (lambda (a b) (string= (alist-get 'source_path a)
                                      (alist-get 'source_path b)))))))

;; sacha-blog-similar-link ends here

;; [[file:../Sacha.org::sacha-org-db-v3-index-recent-sketches][sacha-org-db-v3-index-recent-sketches]]
;;;###autoload
(defun sacha-org-db-v3-index-recent-sketches (after)
  (interactive (list
                (when current-prefix-arg
                  (org-read-date nil nil nil "After: " nil "-2w"))))
  (setq after (or after (org-read-date nil nil "-2w")))
  (mapcar #'org-db-v3-index-file-async
          (seq-remove
           (lambda (o) (string> after (file-name-base o)))
           (directory-files "~/sync/sketches" t "\\.txt$"))))
;; sacha-org-db-v3-index-recent-sketches ends here

;; [[file:../Sacha.org::org-db-v3-sketches][org-db-v3-sketches]]
;;;###autoload
(defun sacha-org-db-v3-sketch--collection (input)
  "Perform the RAG search and format the results for Consult.
Returns a list of cons cells (DISPLAY-STRING . PLIST)."
  (mapcar
   (lambda (o)
     (cons (file-name-base (alist-get 'source_path o)) o))
   (seq-uniq
    (sacha-org-db-v3-to-emacs-rag-search input 100 "%sync/sketches%")
    (lambda (a b) (string= (alist-get 'source_path a)
                           (alist-get 'source_path b))))))

;;;###autoload
(defun sacha-sketch-similar (&optional query hide-initial)
  "Vector-search blog posts using `emacs-rag-search' and present results via Consult.
If called with \\[universal-argument\], use the current post's text.
If a region is selected, use that as the default QUERY.
HIDE-INITIAL means hide the initial query, which is handy if the query is very long."
  (interactive (sacha-11ty-interactive-context current-prefix-arg))
  (consult--read
   (if hide-initial
       (sacha-org-db-v3-sketch--collection query)
     (consult--dynamic-collection
         #'sacha-org-db-v3-sketch--collection
       :min-input 3 :debounce 1))
   :lookup #'consult--lookup-cdr
   :prompt "Search sketches (approx): "
   :category 'sketch
   :sort nil
   :require-match t
   :state (sacha-image--state)
   :initial (unless hide-initial query)))

;;;###autoload
(defun sacha-sketch-similar-insert (link)
  "Vector-search sketches and insert a link.
If called with \\[universal-argument\], use the current post's text.
If a region is selected, use that as the default QUERY.
HIDE-INITIAL means hide the initial query, which is handy if the query is very long."
  (interactive (list
                (if embark--command
                    (read-string "Sketch: ")
                  (apply #'sacha-sketch-similar
                         (sacha-11ty-interactive-context current-prefix-arg)))))
  (sacha-insert-sketch-and-text link))

;;;###autoload
(defun sacha-sketch-similar-link (link)
  "Vector-search sketches and insert a link.
If called with \\[universal-argument\], use the current post's text.
If a region is selected, use that as the default QUERY.
HIDE-INITIAL means hide the initial query, which is handy if the query is very long."
  (interactive (list
                (if embark--command
                    (read-string "Sketch: ")
                  (apply #'sacha-sketch-similar
                         (sacha-11ty-interactive-context current-prefix-arg)))))
  (when (and (listp link) (alist-get 'source_path link))
    (setq link (sacha-image-filename (file-name-base link))))
  (insert (org-link-make-string (concat "sketchLink:" link) (file-name-base link))))
;; org-db-v3-sketches ends here

;; [[file:../Sacha.org::sacha-consult-similar][sacha-consult-similar]]
(defvar sacha-consult-source-similar-sketches
    (list :name "Sketches"
          :narrow ?s
          :category 'sketch
          :async (consult--dynamic-collection
                     (lambda (input)
                       (seq-take (sacha-org-db-v3-sketch--collection input) 5)))
          :state #'sacha-image--state
          :action #'sacha-insert-sketch-and-text)))

;;;###autoload
(defun sacha-consult-similar (query hide-initial)
  (interactive (sacha-11ty-interactive-context current-prefix-arg))
  (require 'consult)
  (if hide-initial
      (let ((new-sources
             (list
              (append
               (copy-sequence sacha-consult-source-similar-blog-posts)
               (list :items (seq-take (sacha-org-db-v3-blog-post--collection query) 5)))
              (append
               (copy-sequence sacha-consult-source-similar-sketches)
               (list :items (seq-take (sacha-org-db-v3-sketch--collection query) 5))))))
        (dolist (source new-sources)
          (cl-remf source :async))
        (consult--multi new-sources))
    (consult--multi '(sacha-consult-source-similar-blog-posts
                      sacha-consult-source-similar-sketches)
                    :initial query)))

;;;###autoload
(defun sacha-org-db-v3-index-recent-public (after)
  (interactive (list
                (when current-prefix-arg
                  (org-read-date nil nil nil "After: " nil "-2w"))))
  (setq after (or after (org-read-date nil nil "-2w")))
  (mapc #'org-db-v3-index-file-async
        (sacha-blog-org-files-except-reviews after))
  (sacha-org-db-v3-index-recent-sketches after))
;; sacha-consult-similar ends here

;; [[file:../Sacha.org::#org-mode-vector-search-emacs-rag-search][emacs-rag-search?:2]]
;;;###autoload
(defun sacha-emacs-rag-search (query limit)
  (assoc-default 'results
                 (emacs-rag--request "GET" "/search/vector" nil
                                     `((query . ,query)
                                       (limit . ,limit)
                                       (rerank . ,emacs-rag-search-enable-rerank)))))

;;;###autoload
(defun sacha-emacs-rag-search-blog-posts-hybrid (query &optional vector-weight limit)
  "Use hybrid search to search for QUERY.
Tune with VECTOR-WEIGHT
Start with LIMIT results and keep only unique blog posts."
  (interactive (list (if (region-active-p)
                         (buffer-substring (region-beginning)
                                           (region-end))
                       (read-string "Search: "))))
  (setq limit (or limit 100))
  (setq vector-weight (or vector-weight 0.5))
  (let* ((blog-posts (sacha-blog-posts))
         (results
          (seq-uniq
           (assoc-default 'results
                          (emacs-rag--request "GET" "/search/hybrid" nil
                                              `((query . ,query)
                                                (limit . ,limit)
                                                (vector_weight . ,vector-weight)
                                                (rerank . ,emacs-rag-search-enable-rerank))))
           (lambda (a b)
             (string= (assoc-default 'source_path a nil "")
                      (assoc-default 'source_path b nil ""))))))
    (seq-keep
     (lambda (o)
       (when (string-match "/blog/" (alist-get 'source_path o))
         (append
          o
          (sacha-blog-post-info-for-url (alist-get 'source_path o) blog-posts)
          nil)))
     results)))
;; emacs-rag-search?:2 ends here

(provide 'sacha-similar)
;;; sacha-similar.el ends here
