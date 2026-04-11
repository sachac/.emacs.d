;;;###autoload
(defun my-insert-file-as-org-include (file)
  (interactive "fFile: ")
  (set-text-properties 0 (length file) nil file)
  (let ((mode (assoc-default file auto-mode-alist 'string-match)))
    (insert
     (org-link-make-string (concat "file:" file) (concat "Download " (file-name-nondirectory file))) "\n"
     "#+begin_my_details " (file-name-nondirectory file) "\n"
     (format "#+INCLUDE: %s" (prin1-to-string file))
     (if mode
         (concat " src " (replace-regexp-in-string "-mode$" "" (symbol-name mode)))
       "")
     "\n"
     "#+end_my_details\n")))

;;;###autoload
(defun my-transform-org-link-to-include ()
  (interactive)
  (let ((link (org-element-lineage (org-element-context) '(link) t))
        (mode (assoc-default (org-element-property :path link) auto-mode-alist 'string-match)))
    (when link
      (delete-region (org-element-property :begin link)
                     (org-element-property :end link))
      (my-insert-file-as-org-include (org-element-property :path link)))))

;;;###autoload
(defun my-org-svg-copy-links (filename)
  (interactive (list (read-file-name "SVG: " nil
                                     nil
                                     (lambda (f)
                                       (or (string-match "\\.svg$" f)
                                           (file-directory-p f))))))
  (let ((dom (car (xml-parse-file filename))))
    (kill-new
     (mapconcat
      (lambda (elem)
        (concat "- " (org-link-make-string
                      (dom-attr elem 'href)
                      (or (dom-attr elem 'title)
                          (dom-text (dom-by-tag elem 'title))))))
      (dom-by-tag dom 'a)
      "\n"))))

;;;###autoload
(defun my-org-update-link-description (description)
  "Update the current link's DESCRIPTION."
  (interactive "MDescription: ")
  (let (link)
    (save-excursion
      (cond
       ((org-in-regexp org-link-bracket-re 1)
        (setq link (org-link-unescape (match-string-no-properties 1)))
        (delete-region (match-beginning 0) (match-end 0))
        (insert (org-link-make-string link description))
        (sit-for 0))
       ((or (org-in-regexp org-link-angle-re)
            (org-in-regexp org-link-plain-re))
        (setq link (org-unbracket-string "<" ">" (match-string 0)))
        (delete-region (match-beginning 0) (match-end 0))
        (insert (org-link-make-string link description))
        (sit-for 0))))))

;;;###autoload
(defun my-org-check-agenda ()
  "Peek at agenda."
  (interactive)
  (cond
   ((derived-mode-p 'org-agenda-mode)
    (if (window-parent) (delete-window) (bury-buffer)))
   ((get-buffer "*Org Agenda*")
    (switch-to-buffer-other-window "*Org Agenda*"))
   (t (org-agenda nil "a"))))

(defun my-insert-or-replace-link (url &optional title)
        "Insert a link, wrap the current region in a link, or replace the current link."
        (interactive (list (read-string "URL: ")))
        (cond
         ((derived-mode-p 'org-mode)
                (cond
                 ((org-in-regexp org-link-bracket-re 1)
                        (when (match-end 2) (setq title (match-string-no-properties 2)))
                        (delete-region (match-beginning 0) (match-end 0)))
                 ((org-in-regexp org-link-any-re 1)
                        (delete-region (match-beginning 0) (match-end 0)))
                 ((region-active-p)
                        (setq title (buffer-substring-no-properties (region-beginning) (region-end)))
                        (delete-region (region-beginning) (region-end))))
                ;; update link
                (insert (org-link-make-string url title)))
         ((derived-mode-p 'org-mode)		 ; not in a link
                (insert (org-link-make-string url title)))
         ((and (region-active-p) (derived-mode-p 'markdown-mode))
                (setq title (buffer-substring-no-properties (region-beginning) (region-end)))
                (delete-region (region-beginning) (region-end))
                (insert (format "[%s](%s)" title url)))
         ((derived-mode-p 'markdown-mode)
                (insert (format "[%s](%s)" title url)))
         ((and (region-active-p) (string-match (regexp-quote "*new toot*") (buffer-name)))
                (setq title (buffer-substring-no-properties (region-beginning) (region-end)))
                (delete-region (region-beginning) (region-end))
                (insert (format "[%s](%s)" title url)))
         ((string-match (regexp-quote "*new toot*") (buffer-name))
                (insert (format "[%s](%s)" (read-string "Title: " (my-page-title url))
                                                                                url)))
         (t
                (insert (format "%s (%s)" title url)))))

;; override the embark actions
(defun my-consult-omni-embark-copy-url-as-kill (cand)
        "Don't add spaces."
        (when-let ((s (and (stringp cand) (get-text-property 0 :url cand))))
                (kill-new (string-trim s))))

(defun my-consult-omni-embark-insert-url (cand)
        "Don't add spaces."
        (when-let ((s (and (stringp cand) (get-text-property 0 :url cand))))
                (insert (string-trim s))))

(defun my-consult-omni-embark-copy-title-as-kill (cand)
        "Don't add spaces."
        (when-let ((s (and (stringp cand) (get-text-property 0 :title cand))))
                (kill-new (string-trim s))))

(defun my-consult-omni-embark-insert-title (cand)
        "Don't add spaces."
        (when-let ((s (and (stringp cand) (get-text-property 0 :title cand))))
                (insert (string-trim s))))

(defun my-consult-omni-embark-insert-link (cand)
        "Don't add spaces."
        (let ((url (and (stringp cand) (get-text-property 0 :url cand)))
                                (title (and (stringp cand) (get-text-property 0 :title cand))))
                (my-insert-or-replace-link url title)))

(use-package consult-omni
        :defer t
        :commands consult-omni
        :load-path "~/vendor/consult-omni"
  :after (consult embark)
  :custom
  (consult-omni-show-preview t) ;;; show previews
  (consult-omni-preview-key "C-o") ;;; set the preview key to C-o
  :config
        (add-to-list 'load-path "~/vendor/consult-omni/sources")
  (require 'consult-omni-sources)
  (require 'consult-omni-embark)
  (setq consult-omni-sources-modules-to-load (list 'consult-omni-wikipedia 'consult-omni-google))
  (consult-omni-sources-load-modules)
        (setq consult-omni-dynamic-input-debounce 1.0)
        (setq consult-omni-dynamic-refresh-delay consult-omni-dynamic-input-debounce)
  (setq consult-omni-default-interactive-command #'consult-omni-multi)
        (setq consult-omni-multi-sources
                                '(consult-omni--source-google
                                        consult-omni--source-my-org-bookmarks
                                        consult-omni--source-blog))
        :bind
        (("M-g w" . consult-omni)
         ("M-g f" . consult-omni-my-org-bookmarks)
         :map consult-omni-embark-general-actions-map
         ("i l" .  #'my-consult-omni-embark-insert-link)
         ("i u" .  #'my-consult-omni-embark-insert-url)
         ("i t" .  #'my-consult-omni-embark-insert-title)
         ("w u" . #'my-consult-omni-embark-copy-url-as-kill)
         ("w t" . #'my-consult-omni-embark-copy-title-as-kill)))

;; we're in a bracketed link with no description and the target doesn't look like a link;
;; likely I've actually added the text for the description and now we need to include the link
(defun my-org-in-bracketed-text-link-p ()
  (when (and (derived-mode-p 'org-mode) org-link-bracket-re)
    (let* ((bracket-pos (org-in-regexp org-link-bracket-re))
           (bracket-target (and bracket-pos (match-string 1)))
           (bracket-desc (and bracket-pos (match-string 2))))
      (and bracket-pos bracket-target
           (null bracket-desc)
           ;; try to trigger only when the target is plain text and doesn't have a protocol
           (not (string-match ":" bracket-target))))))

;;;###autoload
(defun my-org-set-link-target-with-search ()
  "Replace the current link's target with a web search.
  Assume the target is actually supposed to be the description.  For
  example, if the link is [[some text]], do a web search for 'some text',
  prompt for the link to use as the target, and move 'some text' to the
  description."
  (interactive)
  (let* ((bracket-pos (org-in-regexp org-link-bracket-re))
         (bracket-target (and bracket-pos (match-string 1)))
         (bracket-desc (match-string 2))
         result)
    (when (my-org-in-bracketed-text-link-p)
      (let ((link (consult-omni bracket-target nil nil t)))
        (cond
         ((get-text-property 0 :url link)
          (setq result (org-link-make-string (get-text-property 0 :url link)
                                             bracket-target)))
         ((string-match ":" link) ; might be a URL
          (setq result (org-link-make-string link bracket-target))))
        (when result
          (delete-region (car bracket-pos) (cdr bracket-pos))
          (insert result)
          result)))))

;;;###autoload
  (defun my-org-set-link-target-with-org-completion ()
          "Replace the current link's target with `org-insert-link' completion.
  Assume the target is actually supposed to be the description.  For
  example, if the link is [[some text]], do a web search for 'some text',
  prompt for the link to use as the target, and move 'some text' to the
  description."
          (interactive)
          (let* ((bracket-pos (org-in-regexp org-link-bracket-re))
                                   (bracket-target (match-string 1))
                                   (bracket-desc (match-string 2))
                                   result)
                  (when (and bracket-pos bracket-target
                                                           (null bracket-desc)
                                                           ;; try to trigger only when the target is plain text and doesn't have a protocol
                                                           (not (string-match ":" bracket-target))
                                                           (org-element-lineage (org-element-context) '(link) t)) ; ignore text in code blocks, etc.
                          ;; we're in a bracketed link with no description and the target doesn't look like a link;
                          ;; likely I've actually added the text for the description and now we need to include the link.
                          ;; This is a hack so that we don't have to delete the link until the new link has been inserted
                          ;; since org-insert-link doesn' tbreak out the link prompting code into a smaller function.
                          (let ((org-link-bracket-re "{{{}}}"))
                                  (goto-char (cdr bracket-pos))
                                  (org-insert-link nil nil bracket-target))
                          (delete-region (car bracket-pos) (cdr bracket-pos)))))

;;;###autoload
  (defun my-org-set-link-target-dwim ()
          (interactive)
          (or (my-org-set-link-target-with-search)
                          (my-org-set-link-target-with-org-completion)))

;;;###autoload
  (defun my-org-scan-for-untargeted-links ()
          "Look for [[some text]] and prompt for the actual targets."
          (interactive)
          (while (re-search-forward org-link-bracket-re nil t)
                  (when (and
                                           (not (match-string 2))
                                           (and (match-string 1) (not (string-match ":" (match-string 1))))
                                           (org-element-lineage (org-element-context) '(link) t)) ; ignore text in code blocks, etc.
                          (undo-boundary)
                          (my-org-set-link-target-dwim))))

  (defvar my-download-dir "~/Downloads")
;;;###autoload
  (defun my-open-latest-download ()
    (interactive)
    (find-file (my-latest-file my-download-dir)))

;;;###autoload
  (defun my-attach-and-link-latest-download ()
    (interactive)
    (org-attach-attach (my-latest-file my-download-dir) nil 'cp)
    (org-insert-link nil (caar org-stored-links)))

;;;###autoload
  (defun my-link-latest-download ()
    (interactive)
    (org-insert-link nil (concat "file:" (my-latest-file my-download-dir))
                                                                           (file-name-nondirectory (my-latest-file my-download-dir))))

;;;###autoload
  (defun my-include-latest-download ()
          (interactive)
          (my-insert-file-as-org-include (my-latest-file my-download-dir)))

;;;###autoload
  (defun my-copy-latest-download (dest &optional force)
    (interactive "FDestination: ")
    (copy-file (my-latest-file my-download-dir) dest force))
;;;###autoload
  (defun my-download-dired ()
          (interactive)
          (dired my-download-dir "-lt"))

;;;###autoload
  (defun my-helm-org-rifle-org-directory ()
    (interactive)
    (helm-org-rifle-directories (list org-directory) t))
;;;###autoload
  (defun my-consult-recoll-without-emacs-news ()
    (interactive)
    (consult-recoll--open (consult-recoll--search "-\"Emacs News\" ")))

;;;###autoload
(defun my-org-image-dired-store-link ()
  (when (and (derived-mode-p 'image-dired-thumbnail-mode)
             (get-text-property (point) 'original-file-name))
    (org-link-store-props
     :link (concat "file:" (get-text-property (point) 'original-file-name)))))

(with-eval-after-load 'org
  (org-link-set-parameters
   "image-dired"
   :store #'my-org-image-dired-store-link))

;;;###autoload
(defun my-org-yank-file-links-from-kill-ring ()
  (interactive)
  (dolist (file (read (concat "(" (current-kill 0) ")")))
    (insert (org-link-make-string (concat "file:" file)) "\n")))

;;;###autoload
  (defun my-org-get-subtree-by-name (org-text heading-name)
    "Return ORG-TEXT subtree for HEADING-NAME."
    (with-temp-buffer
      (insert org-text)
      (org-mode)
      (goto-char (point-min))
      (while (re-search-forward " " nil t)
        (replace-match " "))
      (goto-char (point-min))
      (let ((org-trust-scanner-tags t))
        (car (delq nil
                   (org-map-entries
                    (lambda ()
                      (when (string= (org-entry-get (point) "ITEM") heading-name)
                        (buffer-substring (point) (org-end-of-subtree))))))))))

;;;###autoload
  (defun my-org-copy-clean-version (text)
    "Copy BEG to END without strike-throughs."
    (interactive (list (if (region-active-p) (buffer-substring (region-beginning) (region-end))
                         (sentence-at-point))))
    (let ((result
           (mapconcat (lambda (o)
                               (if (stringp o)
                                   o
                                 (car (org-element-contents o))))
                             (seq-remove (lambda (o)
                                           (and (listp o)
                                                (member (org-element-type o)
                                                        '(strike-through macro))))
                                         (org-element-parse-secondary-string
                                          text '(paragraph bold strike-through macro)))
                             " ")))
             (setq result
               (replace-regexp-in-string
                "  +" " "
                (replace-regexp-in-string " [\\.,]" "\\1" result)))
      (when (called-interactively-p 'any)
        (message "%s" result)
        (kill-new result))
      result))

  ;; hmm, doesn't quite work for looking things up yet. I basically want a programmatic where-is for a specific keymap
  (defvar my-keybinding-maps '(subed-mode-map subed-waveform-minor-mode-map subed-waveform-svg-map))
;;;###autoload
  (defun my-copy-keybinding (symbol)
          (interactive (list (find-function-read)))
          (when (listp symbol)
                  (setq symbol (car symbol)))
          (let (result keys)
                  (map-keymap
                   (lambda (event def)
                           (cond ((and (symbolp def))
                                                          (push (list def event) result))
                                                   ((and (listp def) (eq 'keymap (car def)))
                                                          (apply 'append
                                                                                   (map-keymap
                                                                                          (lambda (event def)
                                                                                                  (when (and (symbolp def))
                                                                                                          (push (list def event) result)))
                                                                                          def)))))
                   subed-mode-map)
                  (setq keys (assoc-default symbol result))
                  (when keys
                          (kill-new (key-description keys))
                          (message "%s" (key-description keys)))))

;;;###autoload
(defun sacha-org-autolist-allow-newlines ()
  "Insert newline conditionally."
  (when (and current-prefix-arg (org-in-item-p) org-autolist-mode)
    (insert "\n")
    t))

;;;###autoload
(defun my-org-find-first-common-heading (other-buffer)
	"Go to the first top-level heading in common with OTHER-BUFFER.
This is helpful when resolving sync conflicts."
	(interactive (list (read-buffer "Other buffer: ")))
	(let ((other-headings (with-current-buffer (get-buffer other-buffer)
													(org-map-entries (lambda () (org-entry-get (point) "ITEM")) "LEVEL=1"))))
		(goto-char
		 (catch 'done
			 (org-map-entries
				(lambda ()
					(when (member (org-entry-get (point) "ITEM") other-headings)
						(throw 'done (point))))
				"LEVEL=1")))
		))

;;;###autoload
(defun my-org-delete-open-clocks ()
	(interactive)
	(flush-lines
	 (rx
		line-start
		(zero-or-more space)
		"CLOCK:"
		(one-or-more space)
		(regexp org-ts-regexp-inactive)
		(zero-or-more space)
		line-end)))

;;;###autoload
(defun my-org-move-properties-to-parent ()
  "Move entry properties to parent."
	(interactive)
	(let ((properties (org-entry-properties (point) 'standard)))
		;; delete properties from the current entry
		(mapc (lambda (prop)
						(unless (string= (car prop) "CATEGORY") (org-entry-delete (point) (car prop))))
					properties)
		;; add properties
		(outline-up-heading 1)
		(mapc (lambda (prop)
						(org-entry-put (point) (car prop) (cdr prop)))
					properties)))

;;;###autoload
(defun my-org-use-speed-commands-for-headings-and-lists ()
  "Activate speed commands on list items too."
  (or (and (looking-at org-outline-regexp) (looking-back "^\**" nil))
      (save-excursion (and (looking-at (org-item-re)) (looking-back "^[ \t]*" nil)))))

(defvar my-org-cut-subtree-file-regexp
  (regexp-opt '("Inbox.org" "posts.org" "news.org" "ipad.org"))
  "Only cut subtrees in files that match this regular expression.
See `my-org-cut-subtree-or-list-item'.")

;;;###autoload
(defun my-org-cut-subtree-or-list-item (&optional n)
	"Cut current subtree or list item."
	(cond
                                        ; limit this to certain files
	 ((and my-org-cut-subtree-file-regexp
         (not
          (string-match my-org-cut-subtree-file-regexp
									      (or (buffer-file-name) ""))))
		(message "Ignoring files not matching `my-org-cut-subtree-file-regexp'")) ; do nothing
	 ((and (looking-at org-outline-regexp) (looking-back "^\**" nil))
		(org-cut-subtree n))
	 ((looking-at (org-item-re))
		(kill-region (org-beginning-of-item) (org-end-of-item)))))

;;;###autoload
(defun my-org-goto-text-start ()
  "Go to the start of the text after properties and drawers."
  (if (org-before-first-heading-p)
      (goto-char (point-min))
    (org-back-to-heading)
    (org-end-of-meta-data t)))

;;;###autoload
(defun my-org-subtree-text ()
  (if (derived-mode-p 'org-mode)
      (if (org-before-first-heading-p)
          (buffer-substring (point-min)
                            (save-excursion
                              (org-next-visible-heading)
                              (line-beginning-position)))
        (save-excursion
          (buffer-substring (save-excursion (org-end-of-meta-data t) (point))
                            (org-end-of-subtree))))
    (buffer-string)))

;;;###autoload
(defun my-org-copy-subtree-text ()
  (interactive)
  (kill-new (my-org-subtree-text)))

;;;###autoload
(defun my-org-mark-done ()
  (interactive)
  (my-org-with-current-task (org-todo "DONE")))

;;;###autoload
(defun my-org-follow-entry-link ()
  "Follow the defined link for this entry."
  (interactive)
  (if (org-entry-get (point) "LINK")
      (org-open-link-from-string (org-entry-get (point) "LINK"))
    (org-open-at-point)))

;;;###autoload
(defun my-org-link-projects (location)
  "Add link properties between the current subtree and the one specified by LOCATION."
  (interactive
   (list (let ((org-refile-use-cache nil))
           (org-refile-get-location "Location"))))
  (let ((link1 (org-store-link nil)) link2)
    (save-window-excursion
      (org-refile 4 nil location)
      (setq link2 (org-store-link nil))
      (org-set-property "LINK" link1))
    (org-set-property "LINK" link2)))

;;;###autoload
(defun my-org-show-row-and-column (point)
  (interactive "d")
  (save-excursion
    (goto-char point)
    (let ((row (s-trim (org-table-get nil 1)))
          (col (s-trim (org-table-get 1 nil)))
          (message-log-max nil))
      (message "%s - %s" row col))))

;;;###autoload
(defun my-org-insert-heading-for-next-day ()
  "Insert a same-level heading for the following day."
  (interactive)
  (let ((new-date
         (seconds-to-time
          (+ 86400.0
             (float-time
              (org-read-date nil 'to-time (elt (org-heading-components) 4)))))))
    (org-insert-heading-after-current)
    (insert (format-time-string "%Y-%m-%d\n\n" new-date))))

;;;###autoload
(defun my-org-refile-and-jump ()
  (interactive)
  (if (derived-mode-p 'org-capture-mode)
      (org-capture-refile)
    (call-interactively 'org-refile))
  (org-refile-goto-last-stored))

;;;###autoload
(defun my-org-capture-region-contents-with-metadata (start end parg)
  "Write selected text between START and END to currently clocked `org-mode' entry.

With PARG, kill the content instead.
If there is no clocked task, create it as a new note in my inbox instead.

From https://takeonrules.com/2022/10/16/adding-another-function-to-my-workflow/, modified slightly so that it creates a new entry if we are not currently clocked in."
  (interactive "r\nP")
  (let ((text (my-org-region-contents-get-with-metadata start end)))
    (if (car parg)
	      (kill-new text)
      (org-capture-string (concat "-----\n" text)
                          (if (org-clocking-p) "c"
                            "r")))))

;;;###autoload
(defun my-org-region-contents-get-with-metadata (start end)
  "Get the region contents between START and END and return an `org-mode' formatted string.

From https://takeonrules.com/2022/10/16/adding-another-function-to-my-workflow/"
  (require 'magit)
  (require 'git-link)
  (let* ((file-name (buffer-file-name (current-buffer)))
	       (org-src-mode (replace-regexp-in-string
			                  "-mode"
			                  ""
			                  (format "%s" major-mode)))
	       (func-name (which-function))
	       (type (if (derived-mode-p 'prog-mode) "SRC" "EXAMPLE"))
	       (code-snippet (buffer-substring-no-properties start end))
	       (file-base (file-name-nondirectory file-name))
	       (line-number (line-number-at-pos (region-beginning)))
	       (remote-link (when (magit-list-remotes)
			                  (progn
			                    (call-interactively 'git-link)
			                    (car kill-ring))))
	       (initial-txt (if (null func-name)
			                    (format "From [[file:%s::%s][%s]]:"
				                          file-name
				                          line-number
				                          file-base)
			                  (format "From ~%s~ (in [[file:%s::%s][%s]]):"
				                        func-name
				                        file-name
				                        line-number
				                        file-base))))
	  (format (concat "\n- Local :: %s"
			              (when remote-link (format "\n- Remote :: %s" remote-link))
			              "\n\n#+BEGIN_%s %s"
			              "\n%s"
			              "\n#+END_%s\n")
		        initial-txt
		        type
		        org-src-mode
		        code-snippet
		        type)))

(require 'org-clock)
;;;###autoload
(defun my-org-entry-wpm ()
  (interactive)
  (save-restriction
    (save-excursion
      (org-narrow-to-subtree)
      (goto-char (point-min))
      (let* ((words (count-words-region (point-min) (point-max)))
             (minutes (org-clock-sum-current-item))
             (wpm (/ words minutes)))
        (message "WPM: %d (words: %d, minutes: %d)" wpm words minutes)
        (kill-new (number-to-string wpm))))))

;;;###autoload
(defun my-org-log-note (note)
  "Add NOTE to the current entry's logbook."
  (interactive "MNote: ")
  (setq org-log-note-window-configuration (current-window-configuration))
  (move-marker org-log-note-return-to (point))
  (move-marker org-log-note-marker (point))
  (setq org-log-note-purpose 'note)
  (with-temp-buffer
    (insert note)
    (org-store-log-note)))

;;;###autoload
(defun my-org-agenda-for-subtree ()
  (interactive)
  (when (derived-mode-p 'org-agenda-mode) (org-agenda-switch-to))
  (my-org-with-current-task
   (let ((org-agenda-view-columns-initially t))
     (org-agenda nil "t" 'subtree))))


;;;###autoload
(defun my-org-mode-ask-effort ()
  "Ask for an effort estimate when clocking in."
  (unless (org-entry-get (point) "Effort")
    (let ((effort
           (completing-read
            "Effort: "
            (org-entry-get-multivalued-property (point) "Effort"))))
      (unless (equal effort "")
        (org-set-property "Effort" effort)))))

;;;###autoload
(defun my-org-clean-up-inbox ()
  "Archive all DONE tasks and sort the remainder by TODO order."
  (interactive)
  (with-current-buffer (find-file my-org-inbox-file)
    (my-org-archive-done-tasks 'file)
    (goto-char (point-min))
    (if (org-at-heading-p) (save-excursion (insert "\n")))
    (org-sort-entries nil ?p)
    (goto-char (point-min))
    (org-sort-entries nil ?o)
    (save-buffer)))

;;;###autoload
(defun my-org-archive-done-tasks (&optional scope)
  "Archive finished or cancelled tasks.
       SCOPE can be 'file or 'tree."
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (outline-previous-heading)))
   "TODO=\"DONE\"|TODO=\"CANCELLED\"" (or scope (if (org-before-first-heading-p) 'file 'tree))))

;;;###autoload
(defun my-org-html-quote2 (block backend info)
  (when (org-export-derived-backend-p backend 'html)
    (when (string-match "\\`<div class=\"quote2\">" block)
      (setq block (replace-match "<blockquote>" t nil block))
      (string-match "</div>\n\\'" block)
      (setq block (replace-match "</blockquote>\n" t nil block))
      block)))

;;;###autoload
(defun modi/org-split-block ()
  "Sensibly split the current Org block at point."
  (interactive)
  (if (modi/org-in-any-block-p)
      (save-match-data
        (save-restriction
          (widen)
          (let ((case-fold-search t)
                (at-bol (bolp))
                block-start
                block-end)
            (save-excursion
              (re-search-backward "^\\(?1:[[:blank:]]*#\\+begin_.+?\\)\\(?: .*\\)*$" nil nil 1)
              (setq block-start (match-string-no-properties 0))
              (setq block-end (replace-regexp-in-string
                               "begin_" "end_" ;Replaces "begin_" with "end_", "BEGIN_" with "END_"
                               (match-string-no-properties 1))))
            ;; Go to the end of current line, if not at the BOL
            (unless at-bol
              (end-of-line 1))
            (insert (concat (if at-bol "" "\n")
                            block-end
                            "\n\n"
                            block-start
                            (if at-bol "\n" "")))
            ;; Go to the line before the inserted "#+begin_ .." line
            (beginning-of-line (if at-bol -1 0)))))
    (message "Point is not in an Org block")))
(defalias 'my-org-demarcate-block #'modi/org-split-block)
(defalias 'my-org-split-block #'modi/org-split-block)


;;;###autoload
(defun modi/org-in-any-block-p ()
  "Return non-nil if the point is in any Org block.

The Org block can be *any*: src, example, verse, etc., even any
Org Special block.

This function is heavily adapted from `org-between-regexps-p'."
  (save-match-data
    (let ((pos (point))
          (case-fold-search t)
          (block-begin-re "^[[:blank:]]*#\\+begin_\\(?1:.+?\\)\\(?: .*\\)*$")
          (limit-up (save-excursion (outline-previous-heading)))
          (limit-down (save-excursion (outline-next-heading)))
          beg end)
      (save-excursion
        ;; Point is on a block when on BLOCK-BEGIN-RE or if
        ;; BLOCK-BEGIN-RE can be found before it...
        (and (or (org-in-regexp block-begin-re)
                 (re-search-backward block-begin-re limit-up :noerror))
             (setq beg (match-beginning 0))
             ;; ... and BLOCK-END-RE after it...
             (let ((block-end-re (concat "^[[:blank:]]*#\\+end_"
                                         (match-string-no-properties 1)
                                         "\\( .*\\)*$")))
               (goto-char (match-end 0))
               (re-search-forward block-end-re limit-down :noerror))
             (> (setq end (match-end 0)) pos)
             ;; ... without another BLOCK-BEGIN-RE in-between.
             (goto-char (match-beginning 0))
             (not (re-search-backward block-begin-re (1+ beg) :noerror))
             ;; Return value.
             (cons beg end))))))

;;;###autoload
(defun my-org-agenda-project-agenda ()
  "Return the project headline and up to `org-agenda-max-entries' tasks."
  (save-excursion
    (let* ((marker (org-agenda-new-marker))
           (heading
            (org-agenda-format-item "" (org-get-heading) (org-get-category) nil))
           (org-agenda-restrict t)
           (org-agenda-restrict-begin (point))
           (org-agenda-restrict-end (org-end-of-subtree 'invisible))
           ;; Find the TODO items in this subtree
           (list (org-agenda-get-day-entries (buffer-file-name) (calendar-current-date) :todo)))
      (org-add-props heading
          (list 'face 'defaults
                'done-face 'org-agenda-done
                'undone-face 'default
                'mouse-face 'highlight
                'org-not-done-regexp org-not-done-regexp
                'org-todo-regexp org-todo-regexp
                'org-complex-heading-regexp org-complex-heading-regexp
                'help-echo
                (format "mouse-2 or RET jump to org file %s"
                        (abbreviate-file-name
                         (or (buffer-file-name (buffer-base-buffer))
                             (buffer-name (buffer-base-buffer))))))
        'org-marker marker
        'org-hd-marker marker
        'org-category (org-get-category)
        'type "tagsmatch")
      (concat heading "\n"
              (org-agenda-finalize-entries list)))))

;;;###autoload
(defun my-org-agenda-projects-and-tasks (match)
  "Show TODOs for all `org-agenda-files' headlines matching MATCH."
  (interactive "MString: ")
  (let ((todo-only nil))
    (if org-agenda-overriding-arguments
        (setq todo-only (car org-agenda-overriding-arguments)
              match (nth 1 org-agenda-overriding-arguments)))
    (let* ((org-tags-match-list-sublevels
            org-tags-match-list-sublevels)
           (completion-ignore-case t)
           rtn rtnall files file pos matcher
           buffer)
      (when (and (stringp match) (not (string-match "\\S-" match)))
        (setq match nil))
      (when match
        (setq matcher (org-make-tags-matcher match)
              match (car matcher) matcher (cdr matcher)))
      (catch 'exit
        (if org-agenda-sticky
            (setq org-agenda-buffer-name
                  (if (stringp match)
                      (format "*Org Agenda(%s:%s)*"
                              (or org-keys (or (and todo-only "M") "m")) match)
                    (format "*Org Agenda(%s)*" (or (and todo-only "M") "m")))))
        (org-agenda-prepare (concat "TAGS " match))
        (org-compile-prefix-format 'tags)
        (org-set-sorting-strategy 'tags)
        (setq org-agenda-query-string match)
        (setq org-agenda-redo-command
              (list 'org-tags-view `(quote ,todo-only)
                    (list 'if 'current-prefix-arg nil `(quote ,org-agenda-query-string))))
        (setq files (org-agenda-files nil 'ifmode)
              rtnall nil)
        (while (setq file (pop files))
          (catch 'nextfile
            (org-check-agenda-file file)
            (setq buffer (if (file-exists-p file)
                             (org-get-agenda-file-buffer file)
                           (error "No such file %s" file)))
            (if (not buffer)
                ;; If file does not exist, error message to agenda
                (setq rtn (list
                           (format "ORG-AGENDA-ERROR: No such org-file %s" file))
                      rtnall (append rtnall rtn))
              (with-current-buffer buffer
                (unless (derived-mode-p 'org-mode)
                  (error "Agenda file %s is not in `org-mode'" file))
                (save-excursion
                  (save-restriction
                    (if org-agenda-restrict
                        (narrow-to-region org-agenda-restrict-begin
                                          org-agenda-restrict-end)
                      (widen))
                    (setq rtn (org-scan-tags 'my-org-agenda-project-agenda matcher todo-only))
                    (setq rtnall (append rtnall rtn))))))))
        (if org-agenda-overriding-header
            (insert (org-add-props (copy-sequence org-agenda-overriding-header)
                        nil 'face 'org-agenda-structure) "\n")
          (insert "Headlines with TAGS match: ")
          (add-text-properties (point-min) (1- (point))
                               (list 'face 'org-agenda-structure
                                     'short-heading
                                     (concat "Match: " match)))
          (setq pos (point))
          (insert match "\n")
          (add-text-properties pos (1- (point)) (list 'face 'org-warning))
          (setq pos (point))
          (unless org-agenda-multi
            (insert "Press `C-u r' to search again with new search string\n"))
          (add-text-properties pos (1- (point)) (list 'face 'org-agenda-structure)))
        (org-agenda-mark-header-line (point-min))
        (when rtnall
          (insert (mapconcat 'identity rtnall "\n") ""))
        (goto-char (point-min))
        (or org-agenda-multi (org-agenda-fit-window-to-buffer))
        (add-text-properties (point-min) (point-max)
                             `(org-agenda-type tags
                                               org-last-args (,todo-only ,match)
                                               org-redo-cmd ,org-agenda-redo-command
                                               org-series-cmd ,org-cmd))
        (org-agenda-finalize)
        (setq buffer-read-only t)))))

(defvar my-org-agenda-contexts
  '((tags-todo "phone")
    (tags-todo "work")
    (tags-todo "drawing")
    (tags-todo "coding")
    (tags-todo "writing")
    (tags-todo "computer")
    (tags-todo "home")
    (tags-todo "errands"))
  "Usual list of contexts.")
;;;###autoload
(defun my-org-agenda-skip-scheduled ()
  (org-agenda-skip-entry-if 'scheduled 'deadline 'regexp "\n]+>"))

;;;###autoload
(defun my-org-projects ()
  (interactive)
(org-ql-search (org-agenda-files)
  '(and (todo "TODO" "WAITING") (ancestors (tags "project")))
  :super-groups '((:auto-parent t))))

;;;###autoload
(defun my-org-ql-shuffle-todo ()
	(interactive)
	(org-ql-search (org-agenda-files)
		'(and
			(todo "TODO" "STARTED")
			(not (done))
			(not (scheduled))
			(not (deadline))
			(not (ts-active))
			(not (tags "cooking")))
		:sort 'random))

;;;###autoload
(defun my-org-ql-shuffle-someday ()
	(interactive)
	(org-ql-search (org-agenda-files)
		'(and
			(todo "SOMEDAY")
			(not (done))
			(not (scheduled))
			(not (deadline))
			(not (ts-active))
			(not (tags "cooking")))
		:sort 'random))

;;;###autoload
(defun my-org-agenda-done (&optional arg)
  "Mark current TODO as done.
       This changes the line at point, all other lines in the agenda referring to
       the same tree node, and the headline of the tree node in the Org-mode file."
  (interactive "P")
  (org-agenda-todo "DONE"))

;; Override the key definition for org-exit
(define-key org-agenda-mode-map "x" 'my-org-agenda-done)

;;;###autoload
(defun my-org-agenda-mark-done-and-add-followup ()
  "Mark the current TODO as done and add another task after it.
       Creates it at the same level as the previous task, so it's better to use
       this with to-do items than with projects or headings."
  (interactive)
  (org-agenda-todo "DONE")
  (org-agenda-switch-to)
  (org-capture 0 "t"))

;;;###autoload
(defun my-org-agenda-new ()
  "Create a new note or task at the current agenda item.
       Creates it at the same level as the previous task, so it's better to use
       this with to-do items than with projects or headings."
  (interactive)
  (org-agenda-switch-to)
  (org-capture 0))

(require 'cl)
;;;###autoload
(defun my-org-get-context (txt)
  "Find the context."
  (car (member-if
        (lambda (item) (string-match "@" item))
        (get-text-property 1 'tags txt))))

;;;###autoload
(defun my-org-compare-dates (a b)
  "Return 1 if A should go after B, -1 if B should go after A, or 0 if a = b."
  (cond
   ((and (= a 0) (= b 0)) nil)
   ((= a 0) 1)
   ((= b 0) -1)
   ((> a b) 1)
   ((< a b) -1)
   (t nil)))

;;;###autoload
(defun my-org-complete-cmp (a b)
  (let* ((state-a (or (get-text-property 1 'todo-state a) ""))
         (state-b (or (get-text-property 1 'todo-state b) "")))
    (or
     (if (member state-a org-done-keywords-for-agenda) 1)
     (if (member state-b org-done-keywords-for-agenda) -1))))

;;;###autoload
(defun my-org-date-cmp (a b)
  (let* ((sched-a (or (get-text-property 1 'org-scheduled a) 0))
         (sched-b (or (get-text-property 1 'org-scheduled b) 0))
         (deadline-a (or (get-text-property 1 'org-deadline a) 0))
         (deadline-b (or (get-text-property 1 'org-deadline b) 0)))
    (or
     (my-org-compare-dates
      (my-org-min-date sched-a deadline-a)
      (my-org-min-date sched-b deadline-b)))))

;;;###autoload
(defun my-org-min-date (a b)
  "Return the smaller of A or B, except for 0."
  (funcall (if (and (> a 0) (> b 0)) 'min 'max) a b))

;;;###autoload
(defun my-org-sort-agenda-items-user-defined (a b)
  ;; compare by deadline, then scheduled date; done tasks are listed at the very bottom
  (or
   (my-org-complete-cmp a b)
   (my-org-date-cmp a b)))

;;;###autoload
(defun my-org-context-cmp (a b)
  "Compare CONTEXT-A and CONTEXT-B."
  (let ((context-a (my-org-get-context a))
        (context-b (my-org-get-context b)))
    (cond
     ((null context-a) +1)
     ((null context-b) -1)
     ((string< context-a context-b) -1)
     ((string< context-b context-a) +1)
     (t nil))))

;;;###autoload
(defun my-org-sort-agenda-items-todo (a b)
  (or
   (org-cmp-time a b)
   (my-org-complete-cmp a b)
   (my-org-context-cmp a b)
   (my-org-date-cmp a b)
   (org-cmp-todo-state a b)
   (org-cmp-priority a b)
   (org-cmp-effort a b)))

;;;###autoload
(defun my-org-agenda-list-unscheduled (&rest ignore)
  "Create agenda view for tasks that are unscheduled and not done."
  (let* ((org-agenda-todo-ignore-with-date t)
         (org-agenda-overriding-header "List of unscheduled tasks: "))
    (org-agenda-get-todos)))

;;;###autoload
(defun my-org-show-active-projects ()
  "Show my current projects."
  (interactive)
  (org-tags-view nil "project-inactive-someday"))

;;;###autoload
(defun my-extract-tasks-from-agenda (string matchers prefix line-re)
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (while (re-search-forward line-re nil t)
      (let ((temp-list matchers))
        (while temp-list
          (if (save-match-data
                (string-match (car (car temp-list)) (match-string 1)))
              (progn
                (add-to-list (cdr (car temp-list)) (concat prefix (match-string 3)) t)
                (setq temp-list nil)))
          (setq temp-list (cdr temp-list)))))))

(ert-deftest my-extract-tasks-from-agenda ()
  (let (list-a list-b (line-re "\\([^:]+\\):\\( \\)\\(.*\\)"))
    (my-extract-tasks-from-agenda
     "listA: Task 1\nother: Task 2\nlistA: Task 3"
     '(("listA" . list-a)
       ("." . list-b))
     "- [ ] "
     line-re)
    (should (equal list-a '("- [ ] Task 1" "- [ ] Task 3")))
    (should (equal list-b '("- [ ] Task 2")))))

;;;###autoload
(defun my-get-upcoming-tasks ()
  (save-window-excursion
    (org-agenda nil "W")
    (my-extract-tasks-from-agenda (buffer-string)
                                   '(("routines" . ignore)
                                     ("business" . business-next)
                                     ("people" . relationships-next)
                                     ("tasks" . emacs-next)
                                     ("." . life-next))
                                   "  - [ ] "
                                   my-weekly-review-line-regexp)))
;;;###autoload
(defun my-get-previous-tasks ()
  (let (string)
    (save-window-excursion
      (org-agenda nil "W")
      (org-agenda-later -1)
      (org-agenda-log-mode 16)
      (setq string (buffer-string))
      ;; Get any completed tasks from the current week as well
      (org-agenda-later 1)
      (org-agenda-log-mode 16)
      (setq string (concat string "\n" (buffer-string)))
      (my-extract-tasks-from-agenda string
                                     '(("routines" . ignore)
                                       ("business" . business)
                                       ("people" . relationships)
                                       ("tasks" . emacs)
                                       ("." . life))
                                     "  - [X] "
                                     my-weekly-done-line-regexp))))

;;;###autoload
(defun my-org-summarize-focus-areas (date)
  "Summarize previous and upcoming tasks as a list."
  (interactive (list (org-read-date-analyze (if current-prefix-arg (org-read-date) "-fri") nil '(0 0 0))))
  (let (business relationships life business-next relationships-next life-next string emacs emacs-next
                 start end time-summary biz-time ignore base-date)
    (setq base-date (apply 'encode-time date))
    (setq start (format-time-string "%Y-%m-%d" (days-to-time (- (time-to-number-of-days base-date) 6))))
    (setq end (format-time-string "%Y-%m-%d" (days-to-time (1+ (time-to-number-of-days base-date)))))
    (setq time-summary (quantified-summarize-time start end))
    (setq biz-time (my-quantified-get-hours "Business" time-summary))
    (my-get-upcoming-tasks)
    (my-get-previous-tasks)
    (setq string
          (concat
           (format "- *A- (Childcare)* (%.1fh - %d%% of total)\n"
                   (my-quantified-get-hours '("A-") time-summary)
                   (/ (my-quantified-get-hours '("A-") time-summary) 1.68))
           (format "- *Business* (%.1fh - %d%%)\n" biz-time (/ biz-time 1.68))
           (mapconcat 'identity business "\n") "\n"
           (mapconcat 'identity business-next "\n")
           "\n"
           (format "  - *Earn* (%.1fh - %d%% of Business)\n"
                   (my-quantified-get-hours "Business - Earn" time-summary)
                   (/ (my-quantified-get-hours "Business - Earn" time-summary) (* 0.01 biz-time)))
           (format "  - *Build* (%.1fh - %d%% of Business)\n"
                   (my-quantified-get-hours "Business - Build" time-summary)
                   (/ (my-quantified-get-hours "Business - Build" time-summary) (* 0.01 biz-time)))
           (format "  - *Connect* (%.1fh - %d%% of Business)\n"
                   (my-quantified-get-hours "Business - Connect" time-summary)
                   (/ (my-quantified-get-hours "Business - Connect" time-summary) (* 0.01 biz-time)))
           (format "- *Relationships* (%.1fh - %d%%)\n"
                   (my-quantified-get-hours '("Discretionary - Social"
                                              "Discretionary - Family") time-summary)
                   (/ (my-quantified-get-hours '("Discretionary - Social"
                                                 "Discretionary - Family") time-summary) 1.68))
           (mapconcat 'identity relationships "\n") "\n"
           (mapconcat 'identity relationships-next "\n") "\n"
           "\n"
           (format "- *Discretionary - Productive* (%.1fh - %d%%)\n"
                   (my-quantified-get-hours "Discretionary - Productive" time-summary)
                   (/ (my-quantified-get-hours "Discretionary - Productive" time-summary) 1.68))
           (format "  - *Drawing* (%.1fh)\n"
                   (my-quantified-get-hours '("Discretionary - Productive - Drawing")  time-summary))
           (format "  - *Emacs* (%.1fh)\n"
                   (my-quantified-get-hours "Discretionary - Productive - Emacs" time-summary))
           (mapconcat 'identity emacs "\n") "\n"
           (mapconcat 'identity emacs-next "\n") "\n"
           (format "  - *Coding* (%.1fh)\n"
                   (my-quantified-get-hours "Discretionary - Productive - Coding" time-summary))
           (mapconcat 'identity life "\n") "\n"
           (mapconcat 'identity life-next "\n") "\n"
           (format "  - *Sewing* (%.1fh)\n"
                   (my-quantified-get-hours "Discretionary - Productive - Sewing" time-summary))
           (format "  - *Writing* (%.1fh)\n"
                   (my-quantified-get-hours "Discretionary - Productive - Writing" time-summary))
           (format "- *Discretionary - Play* (%.1fh - %d%%)\n"
                   (my-quantified-get-hours "Discretionary - Play" time-summary)
                   (/ (my-quantified-get-hours "Discretionary - Play" time-summary) 1.68))
           (format "- *Personal routines* (%.1fh - %d%%)\n"
                   (my-quantified-get-hours "Personal" time-summary)
                   (/ (my-quantified-get-hours "Personal" time-summary) 1.68))
           (format "- *Unpaid work* (%.1fh - %d%%)\n"
                   (my-quantified-get-hours "Unpaid work" time-summary)
                   (/ (my-quantified-get-hours "Unpaid work" time-summary) 1.68))
           (format "- *Sleep* (%.1fh - %d%% - average of %.1f per day)\n"
                   (my-quantified-get-hours "Sleep" time-summary)
                   (/ (my-quantified-get-hours "Sleep" time-summary) 1.68)
                   (/ (my-quantified-get-hours "Sleep" time-summary) 7)
                   )))
    (if (called-interactively-p 'any)
        (insert string)
      string)))

;;;###autoload
(defun my-org-add-line-item-task (task)
  (interactive "MTask: ")
  (org-insert-heading)
  (insert "[ ] " task)
  (let ((org-capture-entry '("t" "Tasks" entry
                             (file+headline "~/sync/orgzly/organizer.org" "Tasks")
                             "")))
    (org-capture nil "t")
    (insert "TODO " task "\nSCHEDULED: <" (org-read-date) ">")))
                                        ;(define-key org-mode-map (kbd "C-c t") 'my-org-add-line-item-task)

;;;###autoload
(defun my-org-list-from-rss (url from-date &optional to-date)
  "Convert URL to an Org list"
  (with-current-buffer (url-retrieve-synchronously url)
    (goto-char (point-min))
    (re-search-forward "<\\?xml")
    (goto-char (match-beginning 0))
    (let* ((feed (xml-parse-region (point) (point-max)))
           (is-rss (> (length (xml-get-children (car feed) 'entry)) 0)))
      (mapconcat (lambda (link)
                   (format "- %s\n"
                           (org-link-make-string (car link) (cdr link))))
                 (if is-rss
                     (mapcar
                      (lambda (entry)
                        (cons
                         (xml-get-attribute (car
                                             (or
                                              (seq-filter (lambda (x) (string= (xml-get-attribute x 'rel) "alternate"))
                                                          (xml-get-children entry 'link))
                                              (xml-get-children entry 'link))) 'href)
                         (elt (car (xml-get-children entry 'title)) 2)))
                      (-filter (lambda (entry)
                                 (let ((entry-date (elt (car (xml-get-children entry 'updated)) 2)))
                                   (and
                                    (org-string<= from-date entry-date)
                                    (or (null to-date) (string< entry-date to-date)))))
                               (xml-get-children (car feed) 'entry)))
                   (mapcar (lambda (entry)
                             (cons
                              (caddr (car (xml-get-children entry 'link)))
                              (caddr (car (xml-get-children entry 'title)))))
                           (-filter (lambda (entry)
                                      (let ((entry-time (format-time-string "%Y-%m-%d"
                                                                            (date-to-time (elt (car (xml-get-children entry 'pubDate)) 2))
                                                                            t)))
                                        (and
                                         (not (string< entry-time from-date))
                                         (or (null to-date) (string< entry-time to-date)))))
                                    (xml-get-children (car (xml-get-children (car feed) 'channel)) 'item))))
                 ""))))


;;;###autoload
  (defun my-org-prepare-weekly-review (&optional date skip-urls)
    "Prepare weekly review template."
    (interactive (list (org-read-date nil nil nil "Ending on Sun: " nil "-sun")))
    (let* ((post-date (current-time))
	   (base-date (apply 'encode-time (org-read-date-analyze date nil '(0 0 0))))
	   start end links prev
	   (title (format-time-string "Weekly review: Week ending %B %e, %Y" base-date))
	   (post-location (concat (format-time-string "%Y/%m/" post-date) (my-make-slug title))))
      (setq start (format-time-string "%Y-%m-%d 0:00" (days-to-time (- (time-to-number-of-days base-date) 6)) (current-time-zone)))
      (setq end (format-time-string "%Y-%m-%d 0:00" (days-to-time (1+ (time-to-number-of-days base-date))) (current-time-zone)))
      (setq prev (format-time-string "%Y-%m-%d 0:00" (days-to-time (- (time-to-number-of-days base-date) 7 6)) (current-time-zone)))
      (outline-next-heading)
      (insert
       "** " title "  :weekly:\n"
       (format
        ":PROPERTIES:
  :EXPORT_DATE: %s
  :EXPORT_ELEVENTY_PERMALINK: %s
  :EXPORT_ELEVENTY_FILE_NAME: %s
  :END:\n"
        (format-time-string "%Y-%m-%dT%T%z")
        (concat "/blog/" post-location "/")
        (concat "blog/" post-location))
       (my-org-summarize-journal-csv start end nil my-journal-category-map my-journal-categories)
       "\n\n*Blog posts*\n\n"
       (my-org-list-from-rss "https://sachachua.com/blog/feed" start end)
       "\n\n*Sketches*\n\n"
       (my-sketches-export-and-extract start end) "\n"
			 "\n\n*Toots*\n\n"
			 (my-mastodon-format-my-toots-since start)
       "\n\n#+begin_my_details Time\n"
			 (format "#+begin_src emacs-lisp :results table :exports results
(my-quantified-compare \"%s\" \"%s\" \"%s\" \"%s\" my-quantified-summary-categories \"The other week %%\" \"Last week %%\")
#+end_src

:results:\n"  prev start start end)
       (orgtbl-to-orgtbl
        (my-quantified-compare prev start start end my-quantified-summary-categories "The other week %" "Last week %")
        nil)
			 ":end:\"\""
			 (format "\n#+begin_src emacs-lisp :exports results :results file :file time-graph.svg :output-dir /tmp\n(quantified-svg-to-text (quantified-svg-days \"%s\" \"%s\"))\n#+end_src\n\n" start end)
       "\n#+end_my_details\n\n")))

;;;###autoload
  (defun my-prepare-missing-weekly-reviews ()
    "Prepare missing weekly reviews based on LAST_REVIEW property."
    (interactive)
    (let ((today (substring (org-read-date nil nil ".") 0 10))
	  (date (org-entry-get (point) "LAST_REVIEW")))
      (while (string< date today)
	(setq date (substring (org-read-date nil nil "++1w" nil (org-time-string-to-time date)) 0 10))
	(unless (string< today date)
	  (save-excursion
	    (my-org-prepare-weekly-review date))
	  (org-entry-put (point) "LAST_REVIEW" date)))))

;;;###autoload
(defun my-org-review-month (start-date)
  "Review the month's clocked tasks and time."
  (interactive (list (org-read-date)))
  ;; Set to the beginning of the month
  (setq start-date (concat (substring start-date 0 8) "01"))
  (let ((org-agenda-show-log t)
        (org-agenda-start-with-log-mode t)
        (org-agenda-start-with-clockreport-mode t)
        (org-agenda-clockreport-parameter-plist '(:link t :maxlevel 3)))
    (org-agenda-list nil start-date 'month)))

;;;###autoload
(defun my-list-blog-posts (start-date end-date)
  (seq-filter (lambda (o)
                (and (or (null start-date) (string< start-date (plist-get o :date)))
                     (or (null end-date) (string< (plist-get o :date) end-date))))
              (let ((json-object-type 'plist))
                (json-read-file "~/proj/static-blog/_site/blog/all/index.json"))))

;;;###autoload
(defun my-org-get-last-week ()
  "Return dates for filtering last week."
  (if (string= (format-time-string "%u") "6") ;; my week starts on Saturday
      (cons (org-read-date nil nil "-1w") (org-read-date nil nil "."))
    (cons (org-read-date nil nil "-2sat") (org-read-date nil nil "-sat"))))
;;;###autoload
(defun my-org-get-month (&optional date-string)
  "Return start of month containing DATE and start of following month.
       Result is (START . NEXT)."
  (let* ((date (decode-time (if (stringp date-string) (org-read-date nil t date-string) date-string)))
         (month (elt date 4))
         (year (elt date 5))
         start-date
         end-date)
    (calendar-increment-month month year 1)
    (cons
     (format "%4d-%02d-01" (elt date 5) (elt date 4))
     (format "%4d-%02d-01" year month))))

;;;###autoload
(defun my-org-prepare-monthly-review (time)
  (interactive (list (org-read-date nil t)))
  (let* ((date (decode-time time))
         (month (elt date 4))
         (year (elt date 5))
         (post-date (current-time))
         post-location
         title
         start-date
         end-date
         previous-date
         posts
         sketches
				 time-comparison
         org-date)
    (calendar-increment-month month year -1)
		(setq start-date (format "%4d-%02d-01 0:00" year month)
          end-date (format "%4d-%02d-01 0:00" (elt date 5) (elt date 4))
          title (format-time-string "Monthly review: %B %Y" (encode-time 0 0 0 1 month year))
          post-location (concat (format-time-string "%Y/%m/" post-date) (my-make-slug title))
          posts (mapconcat (lambda (o) (concat "- " (org-link-make-string (concat "https://sachachua.com" (plist-get o :permalink))
                                                                          (plist-get o :title))))
                           (my-list-blog-posts
                            (substring start-date 0 10)
                            (substring end-date 0 10))
                           "\n")
          sketches (my-sketches-export-and-extract (substring start-date 0 10) (substring end-date 0 10) nil t))
    (calendar-increment-month month year -1)
    (setq previous-date (format "%4d-%02d-01 0:00" year month))
    (setq time-comparison (my-quantified-compare previous-date start-date start-date end-date my-quantified-summary-categories "Previous month %" "This month %"))
    (goto-char (line-end-position))
    (insert
     "\n\n** " title "  :monthly:review:\n"
     "*Blog posts*\n"
     posts "\n\n"
     "*Sketches*\n\n"
     sketches
     (format "*Time*\n\n#+begin_src emacs-lisp :results table :exports results\n(my-quantified-compare \"%s\" \"%s\" \"%s\" \"%s\" my-quantified-summary-categories \"Previous month %%\" \"This month %%\")\n#+end_src\n\n"
						 previous-date start-date start-date end-date)
     (orgtbl-to-orgtbl time-comparison nil)
		 (format "\n#+begin_src emacs-lisp :exports results :results file :file monthly-%s.svg :output-dir /tmp\n(quantified-svg-to-text (quantified-svg-days \"%s\" \"%s\" 'horizontal))\n#+end_src\n\n"
						 start-date
						 start-date end-date))
    (my-org-11ty-prepare-subtree)))

;;;###autoload
(defun my-org-prepare-yearly-review (year-end)
	(interactive (list (org-read-date nil t nil "Year end (exclusive): ")))
  (let* ((date (decode-time year-end))
         (month (elt date 4))
         (year (elt date 5))
				 (end-date (format-time-string "%Y-%m-%d" year-end))
				 (start-date (progn
											 (setf (elt date 5) (1- (elt date 5)))
											 (format-time-string "%Y-%m-%d" (encode-time date))))
				 (previous-date (progn
													(setf (elt date 5) (1- (elt date 5)))
													(format-time-string "%Y-%m-%d" (encode-time date))))
				 (posts (mapconcat (lambda (o)
														 (concat "- " (org-link-make-string
																					 (concat my-blog-base-url (plist-get o :permalink))
																					 (plist-get o :title))))
													 (my-list-blog-posts
														(substring start-date 0 10)
														(substring end-date 0 10))
													 "\n"))
				 (sketches (my-sketches-export-and-extract
										(substring start-date 0 10) (substring end-date 0 10) nil t))
				 (time (my-quantified-compare
								previous-date start-date start-date end-date my-quantified-summary-categories
								"The other year %"
								"Last year %")))
    (insert
     "*Blog posts*\n\n" posts "\n\n"
     "*Sketches*\n\n" sketches
     "*Time*\n\n"
		 (format "#+begin_src emacs-lisp :results table :exports results
(my-quantified-compare \"%s\" \"%s\" \"%s\" \"%s\" my-quantified-summary-categories \"The other year %%\" \"Last year %%\")
#+end_src

:results:\n"  previous-date start-date start-date end-date)
		 (orgtbl-to-orgtbl time nil)
		 (format "\n#+begin_src emacs-lisp :exports results :results file :file time-graph.svg :output-dir /tmp\n(quantified-svg-to-text (quantified-svg-days \"%s\" \"%s\" 'horizontal))\n#+end_src\n\n" start-date end-date))))

;;;###autoload
(defun my-org-emoji-summary (&optional label)
	(let (results)
		(save-excursion
			(goto-char (org-find-property "EXPORT_ELEVENTY_PERMALINK" (org-entry-get-with-inheritance "EXPORT_ELEVENTY_PERMALINK")))
			(let ((end (save-excursion (org-end-of-subtree))))
				(while (re-search-forward "^\\([0-9]+\\)\\. \\([^A-Za-z0-9]+\\) \\(.+?\\)\\(- weekly highlight\\)?\n" end t)
					(let ((day (match-string 1))
								(icon (match-string 2))
								(text (match-string 3)))

						(push
						 (if (string-match org-link-bracket-re text)
								 (format "<a href=\"%s\" title=\"%s - %s\">%s</a>"
												 (match-string 1 text)
												 (match-string 2 text)
												 day
												 icon)
							 (format "<span title=\"%s - %s\">%s</span>"
											 text
											 day
											 icon))
						 results)))))
		(format "<div class=\"emoji-summary\">%s%s</div>"
						(if label (concat label ": ") "")
						(string-join (nreverse results) ""))))

;;;###autoload
(defun my-org-insert-defun (function)
  "Inserts an Org source block with the definition for FUNCTION."
  (interactive (find-function-read))
  (let* ((buffer-point (condition-case nil (find-definition-noselect function nil) (error nil)))
         (new-buf (car buffer-point))
         (new-point (cdr buffer-point))
         definition)
    (if (and buffer-point new-point)
        (with-current-buffer new-buf ;; Try to get original definition
          (save-excursion
            (goto-char new-point)
            (setq definition (buffer-substring-no-properties (point) (save-excursion (end-of-defun) (point))))))
      ;; Fallback: Print function definition
      (setq definition (concat (prin1-to-string (symbol-function function)) "\n")))
    (if (org-in-src-block-p)
        (insert definition)
      (insert "#+begin_src emacs-lisp\n" definition "#+end_src\n"))))

;;;###autoload
(defun my-org-insert-function-and-key (keys)
  (interactive (caar (help--read-key-sequence)))
  (insert (format "=%s= (=%s=)" (symbol-name (key-binding keys t))
                  (key-description keys))))

;;;###autoload
(defun my-org-replace-with-permalink ()
	(interactive)
	(let* ((elem (org-element-context))
				 (path (org-element-property :path elem))
				 (description (org-element-property :description elem))
				 (type (org-element-property :type elem))
				 (permalink (org-entry-get (point) "EXPORT_ELEVENTY_PERMALINK" t))
				 (base-url my-blog-base-url))
		(when (member type '("file" "audio" "video"))
			(delete-region (org-element-begin elem) (org-element-end elem))
			(insert (org-link-make-string (concat
																		 (if (string= type "file") "" (concat type ":"))
																		 base-url permalink (file-name-nondirectory path))
																		description)))))

;;;###autoload
(defun my-org-topic-open (link &rest _)
	"Find the post."
	(if (string-match "\\(.*\\)#\\(.+\\)" link)
			(let ((file (match-string 1 link))
						(anchor (match-string 2 link)))
				(find-file (format "~/sync/topics/%s.org" file))
				(goto-char (org-find-property "CUSTOM_ID" anchor)))
		(find-file (format "~/sync/topics/%s.org" link))))

;;;###autoload
(defun my-org-topic-export (link desc format _)
	(let ((path (concat (if (eq format '11ty) "/" "https://sachachua.com/")
											"topic/"
											(replace-regexp-in-string
											 "\\.html$" "/"
											 (replace-regexp-in-string
												"^/\\|index.html$" ""
												(replace-regexp-in-string "^index" ""
																									link))))))
		(pcase format
			((or 'html '11ty) (format "<a href=\"%s\">%s</a>" path (or desc link)))
      ('latex (format "\\href{%s}{%s}" path desc))
      ('texinfo (format "@uref{%s,%s}" path desc))
      ('ascii (if desc (format "%s (%s)" desc path)
								path)))))

;;;###autoload
(defun my-org-topic-complete ()
	(format "%stopic/%s/"
					my-blog-base-url
					(completing-read
					 "Topic: "
					 (mapcar (lambda (o) (file-name-base o))
									 (directory-files "~/sync/topics" "\\.org" nil)))))

;;;###autoload
(defun my-org-topic-store ()
	(when (and (derived-mode-p 'org-mode)
						 (buffer-file-name)
						 (string-match "/home/sacha/sync/topics/"
													 (expand-file-name (buffer-file-name))))
		(let ((props (org-collect-keywords '("TITLE")))
					(id (org-entry-get-with-inheritance "CUSTOM_ID")))
			(org-link-store-props
			 :link
			 (concat
				"topic:" (file-name-base (buffer-file-name))
				(if id
						(concat "#" id)
					""))
			 :description
			 (save-excursion
				 (if id (progn
									(goto-char (org-find-property "CUSTOM_ID" id))
									(org-entry-get (point) "ITEM"))
					 (car (assoc-default "TITLE" props #'string=))))))))

(defvar my-blog-base-url "https://sachachua.com/")

;;;###autoload
(defun my-org-blog-complete ()
  "Select a blog post and return its URL."
  (my-blog-url (my-consult-blog-posts-by-title)))

;;;###autoload
(defun my-org-blog-export (link desc format _)
	(let ((path (concat (if (eq format '11ty) "/" my-blog-base-url)
											(replace-regexp-in-string "\\.html$" "/"
																								(replace-regexp-in-string "^/\\|index.html$" ""
																																					link)))))
		(pcase format
			((or 'html '11ty) (format "<a href=\"%s\">%s</a>" path (or desc link)))
      ('latex (format "\\href{%s}{%s}" path desc))
      ('texinfo (format "@uref{%s,%s}" path desc))
      ('ascii (if desc (format "%s (%s)" desc path)
								path)))))

;;;###autoload
(defun my-11ty-html-filename (link)
	"Return the HTML file for LINK."
  (when (listp link) (setq link (assoc-default 'permalink link)))
	(setq link (replace-regexp-in-string (concat "^blog:\\|" (regexp-quote my-blog-base-url)) "" link))
	(when (string-match "^/" link) (setq link (concat "." link)))
  (if (file-exists-p link)
			link
		(or (catch 'found
					(dolist (f
									 (list
										(expand-file-name "index.html"
																			(expand-file-name
																			 link
																			 my-11ty-base-dir))
										(expand-file-name "index.html"
																			(expand-file-name
																			 link
																			 (expand-file-name "blog" my-11ty-base-dir)))
										(replace-regexp-in-string
										 "/$" ".html"
										 (expand-file-name
											link
											my-11ty-base-dir))))
						(if (and f (file-exists-p f))
								(throw 'found f))))
				(error "%s not found" link))))

;;;###autoload
(defun my-org-blog-open (link &rest _)
	"Find the post if it exists, or open the HTML."
	(with-current-buffer (find-file-noselect "~/sync/orgzly/posts.org")
		(let ((pos (org-find-property "EXPORT_ELEVENTY_PERMALINK" link)))
			(if pos
					(progn (goto-char pos) (switch-to-buffer (current-buffer)))
				(when-let ((filename (my-11ty-html-filename link)))
					(find-file filename))))))

;;;###autoload
(defun my-org-link-insert-description (link &optional description)
	(unless description
		(my-blog-title (my-org-link-as-url link))))

;;;###autoload
(defun my-org-blog-store ()
	(when (derived-mode-p 'org-mode)
		(let* ((props (org-collect-keywords '("ELEVENTY_PERMALINK"
																					"ELEVENTY_BASE_URL"
																					"TITLE")))
					 (permalink
						(or (org-entry-get-with-inheritance "EXPORT_ELEVENTY_PERMALINK")
								(car (assoc-default "ELEVENTY_PERMALINK" props #'string=)))))
			(when permalink
				(org-link-store-props
				 :link
				 (concat
					(replace-regexp-in-string "/$" "" my-blog-base-url) permalink
					(if (org-entry-get-with-inheritance "CUSTOM_ID")
							(concat "#" (org-entry-get-with-inheritance "CUSTOM_ID"))
						""))
				 :description
				 (save-excursion
					 (goto-char (or (org-find-property "EXPORT_ELEVENTY_PERMALINK" permalink)
													(point-min)))
					 (or (org-entry-get (point) "ITEM")
							 (car (assoc-default "TITLE" props #'string=)))))))))

;;;###autoload
(defun my-org-11ty-copy-subtree (&optional do-cut subtreep)
	"Copy the subtree for the current post to the 11ty export directory.
With prefix arg, move the subtree."
	(interactive (list current-prefix-arg))
	(let* ((info (org-combine-plists
								(org-export--get-export-attributes '11ty subtreep)
								(org-export--get-buffer-attributes)
								(org-export-get-environment '11ty subtreep)))
				 (file-properties
					(seq-filter (lambda (entry)
												(string-match (regexp-opt
																			 '("ELEVENTY_COLLECTIONS"
                                         "ELEVENTY_BASE_DIR"
                                         "ELEVENTY_BASE_URL"
																				 "TITLE"
																				 "ELEVENTY_CATEGORIES"
																				 "ELEVENTY_LAYOUT"))
																			(car entry)))
					            (org-element-map (org-element-parse-buffer) 'keyword
						            (lambda (el) (cons (org-element-property :key el)
																           (org-element-property :value el))))))
				 (entry-properties (org-entry-properties))
				 (filename (expand-file-name
										"index.org"
										(expand-file-name
										 (plist-get info :file-name)
										 (plist-get info :base-dir))))
				 (parent-pos
					(org-find-property
           "EXPORT_ELEVENTY_FILE_NAME"
					 (org-entry-get-with-inheritance "EXPORT_ELEVENTY_FILE_NAME")))
				 body)
		(unless (string= (buffer-file-name)
										 filename)
			(unless (file-directory-p (file-name-directory filename))
				(make-directory (file-name-directory filename) t))
			;; find the heading that sets the current EXPORT_ELEVENTY_FILE_NAME
			(if parent-pos
					(save-excursion
						(goto-char parent-pos)
						(org-copy-subtree 1 (if do-cut 'cut)))
				(setq body (buffer-string)))
			(with-temp-file filename
				(org-mode)
				(if subtreep
						(progn
							(insert
               (or
								(mapconcat
                 (lambda (o) (format "#+%s: %s" (car o) (cdr o)))
                 file-properties
                 "\n")
								"")
							 "\n")
							(org-yank))
					(insert body))))))

;;;###autoload
(defun my-org-11ty-add-mastodon-to-front-matter (front-matter info)
	(plist-put front-matter :mastodon (plist-get info :mastodon))
	(plist-put front-matter :hn (plist-get info :hn))
	(plist-put front-matter :reddit (plist-get info :reddit)))

;;;###autoload
(defun my-tasker-org-insert (url)
	(interactive "MTaskernet URL: ")
	(let* ((parts (url-path-and-query (url-generic-parse-url url)))
				 (params (url-parse-query-string (cdr parts)))
				 (xml-url (format
							 "https://taskernet.com/_ah/api/datashare/v1/sharedata/%s/%s?a=0&xml=true"
							 (url-hexify-string (car (assoc-default "user" params 'string=)))
							 (url-hexify-string (replace-regexp-in-string
																	 "\\+" " "
																	 (car (assoc-default "id" params 'string=))))))
				 (json-object-type 'alist)
				 (data (plz 'get
								 xml-url
								 :as
								 #'json-read))
				 filename)
		(setq filename (expand-file-name (concat (alist-get 'fileName data)
																						 "."
																						 (alist-get 'extension data)
																						 ".xml")
																		 my-download-dir))
		(with-temp-file filename
			(insert (alist-get 'shareData data)))
		(my-insert-file-as-org-include filename)
		(insert (org-link-make-string url "Import via Taskernet"))))

;;;###autoload
(defun my-org-simplify-text (text)
  "Don't include source blocks or links."
  (with-temp-buffer
			(insert text)
			(org-mode)
			(goto-char (point-min))
			(while (re-search-forward org-link-any-re nil t)
        (replace-match
         (if (save-match-data (string-match "audio\\|vtime\\|video" (match-string 0)))
             ""
           (or (match-string 3) "(link)"))) )
			(goto-char (point-min))
			(while (re-search-forward "^ *#\\+begin" nil t)
			 (let* ((block (org-element-context))
              (text
               (if (or (eq (org-element-type block) 'quote-block)
                       (string= (org-element-property :type block)
                                "media-post"))
                   (buffer-substring
                    (org-element-contents-begin block)
                    (org-element-contents-end block))
                 "(block)")))
         (delete-region (org-element-begin block)
													    (org-element-end block))
         (insert text "\n")))
			(while (re-search-forward "\n\n+" nil t)
				(replace-match "\n"))
			(string-trim
			 (buffer-string))))

;;;###autoload
(defun my-org-subtree-text-without-blocks ()
	"Don't include source blocks or links. "
	(let (list)
		(save-excursion
			(save-restriction
				(org-back-to-heading)
				(org-narrow-to-subtree)
        (org-map-entries
         (lambda ()
           (push (buffer-substring-no-properties
                  (line-beginning-position)
                  (line-end-position))
                 list)
           (org-end-of-meta-data t)
           (unless (looking-at org-heading-regexp)
             (push
              (buffer-substring
               (point)
               (save-excursion
                 (or (outline-next-heading)
                     (point-max))))
              list)))
         nil 'tree)))
    (my-org-simplify-text (string-join (nreverse list) "\n"))))

;;;###autoload
(defun my-org-subtree-count-words-without-blocks ()
	(interactive)
	(let ((text (my-org-subtree-text-without-blocks)))
		(with-temp-buffer
			(insert text)
			(message "%s" (count-words--buffer-format)))))

;;;###autoload
(defun my-org-subtree-copy-words-without-blocks ()
	(interactive)
	(kill-new (my-org-subtree-text-without-blocks)))

;;;###autoload
(defun my-include-open (path &optional _)
	"Narrow to the region specified in PATH."
	(require 'org-protocol)
	(let (params start end)
		(if (string-match "^\\(.*+?\\)\\(?:::\\|\\?\\)\\(.*+\\)" path)
				(setq params (save-match-data (org-protocol-convert-query-to-plist (match-string 2 path)))
							path (match-string 1 path)))
		(find-file path)
		(if (plist-get params :name)
				(when (org-babel-find-named-block (plist-get params :name))
					(goto-char (org-babel-find-named-block (plist-get params :name)))
					(let ((block (org-element-context)))
						(narrow-to-region (org-element-begin block)
															(org-element-end block))))
			(setq start
						(or
						 (and
							(plist-get params :from-regexp)
							(progn
								(goto-char (point-min))
								(when (re-search-forward (url-unhex-string (plist-get params :from-regexp)))
									(line-beginning-position))))
						 (progn
							 (goto-char (point-min))
							 (point))))
			(setq end
						(or
						 (and
							(plist-get params :to-regexp)
							(progn
								(when (re-search-forward (url-unhex-string (plist-get params :to-regexp)))
									(line-end-position))))
						 (progn
							 (goto-char (point-max))
							 (point))))
			(when (or (not (= start (point-min)))
								(not (= end (point-max))))
				(narrow-to-region start end)))))

;;;###autoload
(defun my-include-store ()
  "Store a link to Org Babel named blocks."
  (when-let ((elem (and (derived-mode-p 'org-mode) (org-element-at-point))))
    (when (and (org-element-type-p elem 'src-block)
               (org-element-property :name elem))
      (org-link-store-props :type "my-include"
                            :link (concat "my-include:" (buffer-file-name) "?name="
                                          (org-element-property :name elem))
                            :text (org-entry-get (point) "ITEM")))))

;;;###autoload
(defun my-include-export (path _ format _)
	"Export PATH to FORMAT using the specified wrap parameter."
	(require 'org-protocol)
	(let (params body start end)
		(when (string-match "^\\(.*+?\\)\\(?:::\\|\\?\\)\\(.*+\\)" path)
			(setq params (save-match-data (org-protocol-convert-query-to-plist (match-string 2 path)))
						path (match-string 1 path)))
		(with-temp-buffer
			(insert-file-contents-literally path)
			(when (string-match "\\.org$" path)
				(org-mode))
			(if (plist-get params :name)
					(when (org-babel-find-named-block (plist-get params :name))
						(goto-char (org-babel-find-named-block (plist-get params :name)))
						(let ((block (org-element-context)))
							(setq start (org-element-begin block)
										end (org-element-end block))))
				(goto-char (point-min))
				(when (plist-get params :from-regexp)
					(re-search-forward (url-unhex-string (plist-get params :from-regexp)))
					(goto-char (match-beginning 0)))
				(setq start (point))
				(setq end (point-max))
				(when (plist-get params :to-regexp)
					(re-search-forward (url-unhex-string (plist-get params :to-regexp)))
					(setq end (match-beginning 0))))
			(setq body (buffer-substring start end)))
		(with-temp-buffer
			(when (plist-get params :wrap)
				(let* ((wrap (plist-get params :wrap))
							 block args)
					(when (string-match "\\<\\(\\S-+\\)\\( +.*\\)?" wrap)
						(setq block (match-string 1 wrap))
						(setq args (match-string 2 wrap))
						(setq body (format "#+BEGIN_%s%s\n%s\n#+END_%s\n"
															 block (or args "")
															 body
															 block)))))
			(when (plist-get params :summary)
				(setq body (format "#+begin_my_details %s\n%s\n#+end_my_details\n"
													 (plist-get params :summary)
													 body)))
			(insert body)
			(message "BODY: %s" body)
			(org-export-as format nil nil t))))

;;;###autoload
(defun my-include-complete ()
	"Include a section of a file from one line to another, specified with regexps."
	(interactive)
	(require 'consult)
	(let ((file (read-file-name "File: ")))
		(save-window-excursion
			(find-file file)
			(concat "my-include:"
							file
							"?from-regexp="
							(let ((curr-line (line-number-at-pos
																(point)
																consult-line-numbers-widen))
										(prompt "From line: "))
								(goto-char (point-min))
								(consult-line)
								(url-hexify-string
								 (regexp-quote (buffer-substring (line-beginning-position) (line-end-position)))))
							"&to-regexp="
							(let ((curr-line (line-number-at-pos
																(point)
																consult-line-numbers-widen))
										(prompt "To line: "))
								(goto-char (point-min))
								(consult-line
								 nil (point))
								(url-hexify-string
								 (regexp-quote (buffer-substring (line-beginning-position) (line-end-position)))))
							"&wrap=src " (replace-regexp-in-string "-mode$" "" (symbol-name major-mode))))))

(eval-and-compile
  (require 'org-macs nil t))

;;;###autoload
(defun my-org-display-included-images (&optional include-linked refresh beg end)
	"Display inline images for my-include types."
	(interactive "P")
	(when (display-graphic-p)
		(when refresh
      (org-remove-inline-images beg end)
      (when (fboundp 'clear-image-cache) (clear-image-cache)))
    (let ((end (or end (point-max))))
      (org-with-point-at (or beg (point-min))))
		(let* ((case-fold-search t)
					 (file-extension-re "\\.svg")
					 (file-types-re (format "\\[\\[my-include:")))
			(while (re-search-forward file-types-re end t)
				(let* ((link (org-element-lineage (save-match-data (org-element-context)) 'link t))
							 (inner-start (match-beginning 1))
							 (path
								(cond
								 ((not link) nil)
								 ;; file link without a description
								 ((or (not (org-element-contents-begin link)) include-linked)
									(org-element-property :path link))
								 ((not inner-start) nil)
								 (t (org-with-point-at inner-start
											(and (looking-at
														(if (char-equal ?< (char-after inner-start))
																org-link-angle-re
															org-link-plain-re))
													 ;; File name must fill the whole
													 ;; description.
													 (= (org-element-contents-end link)
															(match-end 0))
													 (progn
                             (setq linktype (match-string 1))
                             (match-string 2))))))))
					(when (string-match "\\(.+\\)\\?" path)
						(setq path (match-string 1 path)))
					(when (and path (string-match-p file-extension-re path))
						(let ((file (expand-file-name path)))
              ;; Expand environment variables.
              (when file (setq file (substitute-in-file-name file)))
							(when (and file (file-exists-p file))
								(let ((width (org-display-inline-image--width link))
											(old (get-char-property-and-overlay
														(org-element-begin link)
														'org-image-overlay)))
									(if (and (car-safe old) refresh)
                      (image-flush (overlay-get (cdr old) 'display))
										(let ((image (org--create-inline-image file width)))
											(when image
												(let ((ov (make-overlay
																	 (org-element-begin link)
																	 (progn
																		 (goto-char
																			(org-element-end link))
																		 (skip-chars-backward " \t")
																		 (point)))))
                          ;; FIXME: See bug#59902.  We cannot rely
                          ;; on Emacs to update image if the file
                          ;; has changed.
                          (image-flush image)
                          (overlay-put ov 'evaporate t)
													(overlay-put ov 'display image)
													(overlay-put ov 'face 'default)
													(overlay-put ov 'org-image-overlay t)
													(overlay-put
													 ov 'modification-hooks
													 (list 'org-display-inline-remove-overlay))
													(when (boundp 'image-map)
														(overlay-put ov 'keymap image-map))
													(push ov org-inline-image-overlays))))))))))))))


;;;###autoload
(defun my-org-copy-linked-file-and-change-link (destination)
	(interactive (list
								(cond
								 ((and (not current-prefix-arg) (file-directory-p "images/"))
									"images/")
								 ((org-entry-get-with-inheritance "EXPORT_ELEVENTY_FILE_NAME")
									(expand-file-name
									 (org-entry-get-with-inheritance "EXPORT_ELEVENTY_FILE_NAME")
									 (cadar (org-collect-keywords '("ELEVENTY_BASE_DIR")))))
								 (t
									(read-file-name (format "Copy %s to: "
																					(file-name-nondirectory (org-element-property :path (org-element-context)))))))))
	(let* ((elem (org-element-context))
				 (path (org-element-property :path elem))
				 (description (org-element-property :description elem)))
		(copy-file path destination t)
		(delete-region (org-element-begin elem) (org-element-end elem))
		(insert (org-link-make-string
						 (concat "file:"
										 (file-relative-name
											(if (file-directory-p destination)
													(expand-file-name (file-name-nondirectory path)
																						destination)
												destination)))
						 description))))

;;;###autoload
(defun my-embark-org-copy-linked-file-and-change-link (url destination)
	(interactive (list
								(read-string "Link: ")
								(read-file-name (format "Copy %s to: "
																				(file-name-nondirectory (org-element-property :path (org-element-context)))))))
	(my-org-copy-linked-file-and-change-link destination))

;;;###autoload
(defun my-org-copy-linked-files (destination beg end)
	(interactive (list
								(if (and (not current-prefix-arg) (file-directory-p "images/"))
										"images/"
									(read-file-name (format "Copy %s to: "
																					(file-name-nondirectory (org-element-property :path (org-element-context))))))
								(if (region-active-p) (region-beginning) (point-min))
								(if (region-active-p) (region-end) (point-max))))
	(goto-char beg)
	(while (re-search-forward "\\(file\\):" end t)
		(let* ((elem (org-element-context))
					 (path (org-element-property :path elem))
					 (description (org-element-property :description elem)))
			(when path
				(unless (string-match (concat "^" (regexp-quote (expand-file-name destination))) (expand-file-name path))
					(my-org-copy-linked-file-and-change-link destination))))))

;;;###autoload
(defun my-org-capture-prefill-template (template &rest values)
  "Pre-fill TEMPLATE with VALUES."
  (setq template (or template (org-capture-get :template)))
  (with-temp-buffer
    (insert template)
    (goto-char (point-min))
    (while (re-search-forward
            (concat "%\\("
                    "\\[\\(.+\\)\\]\\|"
                    "<\\([^>\n]+\\)>\\|"
                    "\\([tTuUaliAcxkKInfF]\\)\\|"
                    "\\(:[-a-zA-Z]+\\)\\|"
                    "\\^\\({\\([^}]*\\)}\\)"
                    "?\\([gGtTuUCLp]\\)?\\|"
                    "%\\\\\\([1-9][0-9]*\\)"
                    "\\)") nil t)
      (if (car values)
          (replace-match (car values) nil t))
      (setq values (cdr values)))
    (buffer-string)))

;;;###autoload
(defun my-capture-timestamped-note (time note)
  "Disable Helm and capture a quick timestamped note."
  (interactive (list (current-time) (read-string "Note: ")))
  (let ((helm-completing-read-handlers-alist '((org-capture . nil)))
        (entry (org-capture-select-template "p")))
    (org-capture-set-plist entry)
    (org-capture-get-template)
    (org-capture-set-target-location)
    (org-capture-put
     :template (org-capture-fill-template
                (my-org-capture-prefill-template (org-capture-get :template)
                                                 (format-time-string "%H:%M:%S,%3N")
                                                 note)))
    (org-capture-place-template)
    (org-capture-finalize)))

;;;###autoload
(defun my-capture-timestamped-note-with-screenshot (time note)
  "Include a link to the latest screenshot."
  (interactive (list (current-time) (read-string "Note: ")))
  (kill-new (my-latest-screenshot))
  (my-capture-timestamped-note time note))

;;;###autoload
(defun my-org-convert-list-to-collapsible-details ()
	(interactive)
	(let ((list (org-list-to-lisp t)))
		(mapc (lambda (o)
						(when (stringp (car o))
							(insert
							 (format
								"#+begin_my_details %s :open t\n%s#+end_my_details\n"
								(car o)
								(mapconcat
								 (lambda (s)
									 (concat "- " (string-trim (org-ascii--indent-string (car s) 2)) "\n"))
								 (cdr (cadr o)))))))
					(cdr list))))

;;;###autoload
(defun my-org-copy-region-as-html (beg end &optional level)
  "Make it easier to copy code for Wordpress posts and other things."
  (interactive "r\np")
  (let ((org-export-html-preamble nil)
        (org-html-toplevel-hlevel (or level 3)))
    (kill-new
     (org-export-string-as (buffer-substring beg end) 'html t))))

;;;###autoload
(defun my-org-copy-subtree-as-html ()
  (interactive)
  (my-org-copy-region-as-html
   (org-back-to-heading)
   (org-end-of-subtree)))

;;;###autoload
(defun my-org-convert-link-to-footnote (label)
  "Convert the link at point into a footnote."
  (interactive (list (read-string "Footnote label: ")))
  ;; Get the Org link path at point
  (when (org-in-regexp org-link-bracket-re 1)
    (let ((url (match-string 1)))
      (replace-match (match-string 2) 0)
      (skip-syntax-forward ".")
      (my-org-footnote-add (org-link-make-string (org-link-unescape url)
                                                 (my-org-link-default-description url nil))))))

;;;###autoload
(defun my-org-footnote-add (label &optional text)
  (interactive "MLabel: \n")
  (insert (format "[fn:%s]" label))
  (goto-char (org-footnote-create-definition label))
  (goto-char (line-end-position))
  (when text
    (insert text)))

;;;###autoload
(defun my-org-insert-link-dwim (&optional url title)
	"Like `org-insert-link' but with personal dwim preferences."
	(interactive)
	(let* ((point-in-link (and (derived-mode-p 'org-mode) (org-in-regexp org-link-any-re 1)))
				 (point-in-html-block (and (derived-mode-p 'org-mode)
																	 (let ((elem (org-element-context)))
																		 (and (eq (org-element-type elem) 'export-block)
																					(string= (org-element-property :type elem) "HTML")))))
				 (point-in-src-or-export-block
					(and (derived-mode-p 'org-mode)
							 (let ((elem (org-element-context)))
								 (and (member (org-element-type elem) '(src-block export-block))
											(not (string= (org-element-property :type elem) "Org"))
                      (>= (point) (org-element-property :post-affiliated elem))
                      (<= (point) (+ (length (org-element-property :value elem))
                                     (org-element-property :post-affiliated elem)))))))
				 (region-content (when (region-active-p)
													 (buffer-substring-no-properties (region-beginning)
																													 (region-end))))
         (bookmark-match (when region-content (my-org-bookmark-match region-content)))
         (url (cond
               (url url)
							 ((my-org-in-bracketed-text-link-p) nil)
               (bookmark-match bookmark-match)
							 ((not point-in-link)
                (my-org-read-link
								 ;; clipboard
								 (when (string-match-p "^http" (current-kill 0))
									 (current-kill 0))))))
				 (title (or title
                    region-content
										(when (or (string-match (regexp-quote "*new toot*") (buffer-name))
															(derived-mode-p '(markdown-mode web-mode oddmuse-mode))
															point-in-html-block
															point-in-src-or-export-block
															(not (and (derived-mode-p 'org-mode)
																				point-in-link)))
											(read-string "Title: "
																	 (or (my-org-link-default-description url nil)
																			 (my-page-title url)))))))
		;; resolve the links; see my-org-link-as-url in  https://sachachua.com/dotemacs#web-link
		(unless (and (derived-mode-p 'org-mode)
								 (not (or point-in-html-block point-in-src-or-export-block)))
			(setq url (my-org-link-as-url url)))
		(when (region-active-p) (delete-region (region-beginning) (region-end)))
		(cond
		 ((or (string-match (regexp-quote "*new toot*") (buffer-name))
					(derived-mode-p 'markdown-mode))
			(insert (format "[%s](%s)" title url)))
		 ((or (derived-mode-p '(web-mode html-mode)) point-in-html-block)
			(insert (format "<a href=\"%s\">%s</a>" url title)))
		 ((derived-mode-p 'oddmuse-mode)
			(insert (format "[%s %s]" url title)))
		 ((or point-in-src-or-export-block
					(not (derived-mode-p 'org-mode)))
			(insert title " " url))
     ((or (derived-mode-p 'message-mode)
          (derived-mode-p 'notmuch-message-mode))
      (insert title " " url))
		 ((and region-content url (not point-in-link))
			(insert (org-link-make-string url region-content)))
		 ((and url (not point-in-link))
			(insert (org-link-make-string
							 url
							 (or title
									 (read-string "Title: "
																(or (my-org-link-default-description url nil)
																		(my-page-title url)))))))
		 ;; bracketed [[plain text]]; see Using web searches and bookmarks to quickly link placeholders in Org Mode https://sachachua.com/dotemacs#completion-consult-consult-omni-using-web-searches-and-bookmarks-to-quickly-link-placeholders-in-org-mode
		 ((my-org-set-link-target-with-search))
		 ;; In Org Mode, edit the link
		 ((call-interactively 'org-insert-link)))))

(defun my-org-link-https-insert-description (link desc)
	"Default to the page title."
	(unless desc (my-page-title link)))

;;;###autoload
(defun my-org-link-default-description (link desc)
	"Return the default description for an Org Mode LINK.
This uses :insert-description if defined."
	(let* ((abbrevs org-link-abbrev-alist-local)
				 (all-prefixes (append (mapcar #'car abbrevs)
															 (mapcar #'car org-link-abbrev-alist)
															 (org-link-types)))
				 (type
          (cond
           ((and all-prefixes
                 (string-match (rx-to-string `(: string-start (submatch (or ,@all-prefixes)) ":")) link))
            (match-string 1 link))
           ((file-name-absolute-p link) "file")
           ((string-match "\\`\\.\\.?/" link) "file"))))
		(when (org-link-get-parameter type :insert-description)
			(let ((def (org-link-get-parameter type :insert-description)))
				(condition-case nil
						(cond
						 ((stringp def) def)
						 ((functionp def)
							(funcall def link desc)))
					(error
					 nil))))))

;;;###autoload
(defun my-org-read-link (&optional default)
	"Act like `org-insert-link'. Return link."
	(let* ((wcf (current-window-configuration))
				 (origbuf (current-buffer))
				 (abbrevs org-link-abbrev-alist-local)
				 (all-prefixes (append (mapcar #'car abbrevs)
															 (mapcar #'car org-link-abbrev-alist)
															 (org-link-types)))

				 link)
		(unwind-protect
				;; Fake a link history, containing the stored links.
				(let ((org-link--history
							 (append (mapcar #'car org-stored-links)
											 org-link--insert-history)))
					(setq link
								(org-completing-read
								 (org-format-prompt "Insert link" (or default (caar org-stored-links)))
								 (append
									(mapcar (lambda (x) (concat x ":")) all-prefixes)
									(mapcar #'car org-stored-links)
									;; Allow description completion.  Avoid "nil" option
									;; in the case of `completing-read-default' when
									;; some links have no description.
									(delq nil (mapcar 'cadr org-stored-links)))
								 nil nil nil
								 'org-link--history
								 (or default (caar org-stored-links))))
					(unless (org-string-nw-p link) (user-error "No link selected"))
					(dolist (l org-stored-links)
						(when (equal link (cadr l))
							(setq link (car l))))
					(when (or (member link all-prefixes)
										(and (equal ":" (substring link -1))
												 (member (substring link 0 -1) all-prefixes)
												 (setq link (substring link 0 -1))))
						(setq link (with-current-buffer origbuf
												 (org-link--try-special-completion link)))))
			(when-let* ((window (get-buffer-window "*Org Links*" t)))
				(quit-window 'kill window))
			(set-window-configuration wcf)
			(when (get-buffer "*Org Links*")
				(kill-buffer "*Org Links*")))
    (replace-regexp-in-string
     "^Link: " ""
		 (string-trim link))))

;;;###autoload
(defvar my-emacs-config-url)
(defun my-org-dotemacs-export (path desc format _)
	"Export dotemacs link."
	(pcase format
   ((or 'html '11ty 'md)
	  (format "<a href=\"%s#%s\">%s</a>"
            my-emacs-config-url
            path (or desc path)))
	('ascii
   (if desc
	     (format "%s %s#%s"
               desc
               my-emacs-config-url
               path)
     (format "%s#%s"
               my-emacs-config-url
               path)))))

;;;###autoload
(defun my-org-dotemacs-complete ()
	"Prompt for dotemacs."
	(interactive)
	(with-current-buffer (find-file-noselect "~/sync/emacs/Sacha.org")
		(concat "dotemacs:" (org-read-property-value "CUSTOM_ID"))))

;;;###autoload
(defun my-org-dotemacs-insert-description (link &optional description)
	(unless description
		(with-current-buffer (find-file-noselect "~/sync/emacs/Sacha.org")
			(save-restriction
				(save-excursion
					(widen)
					(goto-char (org-find-property "CUSTOM_ID" (replace-regexp-in-string "^dotemacs:" "" link)))
					(org-entry-get (point) "ITEM"))))))

;;;###autoload
(defun my-org-dotemacs-open (path)
	(with-current-buffer (find-file-noselect "~/sync/emacs/Sacha.org")
		(when-let ((pos (org-find-property "CUSTOM_ID" (replace-regexp-in-string "^dotemacs:" "" path))))
			(switch-to-buffer (current-buffer))
			(goto-char pos))))

;;;###autoload
(defun my-org-dotemacs-store ()
	(when (and (string= (buffer-file-name)
											(expand-file-name "~/sync/emacs/Sacha.org"))
						 (org-entry-get (point) "CUSTOM_ID"))
		(org-link-store-props
		 :link (concat "dotemacs:" (org-entry-get (point) "CUSTOM_ID"))
		 :description (org-entry-get (point) "ITEM"))))

;;;###autoload
(defun my-org-copy-region-as-plain-text (beg end)
	"Copy as plain text, removing links."
	(interactive "r")
	(save-restriction
		(narrow-to-region beg end)
		(kill-new (org-export-as 'ascii nil nil t))))


;;;###autoload
(defun my-copy-link (&optional filename skip-links)
	"Return the URL of this file.
If FILENAME is non-nil, use that instead.
If SKIP-LINKS is non-nil, skip custom links.
If we're in a Dired buffer, use the file at point."
	(interactive)
	(setq filename (or filename
										 (if (derived-mode-p 'dired-mode) (dired-get-filename))
										 (buffer-file-name)))
	(if-let*
			((project-re (concat "\\(" (regexp-opt (mapcar 'car my-project-web-base-list)) "\\)"
													 "\\(.*\\)"))
			 (url (cond
						 ((and (derived-mode-p 'org-mode)
									 (eq (org-element-type (org-element-context)) 'link)
									 (not skip-links))
							(pcase (org-element-property :type (org-element-context))
								((or "https" "http")
								 (org-element-property :raw-link (org-element-context)))
								("yt"
								 (org-element-property :path (org-element-context)))
								;; if it's a custom link, visit it and get the link
								(_
								 (save-window-excursion
									 (org-open-at-point)
									 (my-copy-link nil t)))))
						 ;; links to my config usually have a CUSTOM_ID property
						 ((string= (buffer-file-name) (expand-file-name "~/sync/emacs/Sacha.org"))
							(concat "https://sachachua.com/dotemacs#" (org-entry-get-with-inheritance "CUSTOM_ID")))
						 ;; blog post drafts have permalinks
						 ((and (derived-mode-p 'org-mode) (org-entry-get-with-inheritance "EXPORT_ELEVENTY_PERMALINK"))
							(concat "https://sachachua.com" (org-entry-get-with-inheritance "EXPORT_ELEVENTY_PERMALINK")))
						 ;; some projects have web repos
						 ((string-match
							 project-re filename)
							(concat (assoc-default (match-string 1 filename) my-project-web-base-list)
											(url-hexify-string (match-string 2 filename)))))))
			(progn
				(when (called-interactively-p 'any)
					(kill-new url)
					(message "%s" url))
				url)
		(error "Couldn't figure out URL.")))

;;;###autoload
(defun my-org-link-as-url (link)
	"Return the final URL for LINK."
	(cond
	 ((string-match "^/" link)
		(concat my-blog-base-url (replace-regexp-in-string "^/" "" link)))
	 ((string-match "^\\(https://\\|file:\\)" link)
		link)
	 (t
		(dom-attr
		 (dom-by-tag
			(with-temp-buffer
				(insert (org-export-string-as link 'html t))
				(xml-parse-region (point-min) (point-max)))
			'a)
		 'href))))


;;;###autoload
(defun my-org-stored-link-as-url (&optional link insert)
	"Copy the stored link as a plain URL.
If LINK is specified, use that instead."
	(interactive (list nil current-prefix-arg))
	(setq link (or link (caar org-stored-links)))
	(let ((url (if link
								 (my-org-link-as-url link)
							 (error "No stored link"))))
		(when (called-interactively-p 'any)
			(if url
					(if insert (insert url) (kill-new url))
				(error "Could not find URL.")))
		url))

(ert-deftest my-org-stored-link-as-url ()
	(should
	 (string= (my-org-stored-link-as-url "[[dotemacs:web-link]]")
						"https://sachachua.com/dotemacs#web-link"))
	(should
	 (string= (my-org-stored-link-as-url "[[dotemacs:org-mode-sketch-links][my Org Mode sketch links]]")
						"https://sachachua.com/dotemacs#org-mode-sketch-links")))

;;;###autoload
(defun my-embark-org-copy-exported-url-as-wayback (link &rest _)
	(interactive "MLink: ")
	(let ((url	(my-embark-org-copy-exported-url link)))
		(when (not (string-match (regexp-quote "^https://web.archive.org") url))
			(setq url (concat "https://web.archive.org/web/" (format-time-string "%Y%m%d%H%M%S/")
												url)))
		(when (called-interactively-p 'any)
			(kill-new url)
			(message "Copied %s" url))
		url))

;;;###autoload
(defun my-embark-org-copy-exported-url (link &rest _)
	(interactive "MLink: \np")
	(let ((url (my-org-link-as-url link)))
		(when (and (derived-mode-p 'org-mode)
							 (org-entry-get-with-inheritance "EXPORT_ELEVENTY_PERMALINK")
							 (string-match "^/" url))
			;; local file links are copied to blog directories
			(setq url (concat "https://sachachua.com"
												(org-entry-get-with-inheritance "EXPORT_ELEVENTY_PERMALINK")
												(replace-regexp-in-string
												 "[\\?&].*"
												 ""
												 (file-name-nondirectory link)))))
		(when (called-interactively-p 'any)
			(kill-new url)
			(message "Copied %s" url))
		url))

;;;###autoload
(defun my-embark-replace-link-with-exported-url (link &rest _)
	(interactive (list (org-element-property :raw-link (org-element-context))))
	(my-insert-or-replace-link (my-org-link-as-url link)))

(with-eval-after-load 'embark-org
	(mapc (lambda (map)
					(keymap-set map "u" #'my-embark-org-copy-exported-url)
					(keymap-set map "U" #'my-embark-org-copy-exported-url-as-wayback)
					(keymap-set map "r e" #'my-embark-replace-link-with-exported-url))
				(list embark-url-map embark-org-link-map embark-org-link-copy-map)))

;;;###autoload
(defun my-org-insert-matching-heading-links (match)
	(interactive "MMatch: ")
	(let ((org-tags-exclude-from-inheritance (list match)))
		(insert
		 (string-join
			(org-map-entries
			 (lambda ()
				 (concat "- " (org-link-make-string
											 (car (org-link--file-link-to-here))
											 (org-entry-get (point) "ITEM"))
								 (if (org-entry-get (point) "EXPORT_DATE")
										 (format-time-string " (%Y)"
																				 (date-to-time (org-entry-get (point) "EXPORT_DATE")))
									 "")
								 "\n"))
			 match)))
		""))

;;;###autoload
(defun my-org-get-links-in-region (beg end)
  (save-excursion
    (let (results)
      (goto-char (min beg end))
      (while (re-search-forward org-any-link-re (max beg end) t)
        (add-to-list 'results (org-element-context)))
      results)))

;;;###autoload
(defun my-org-dired-file-links-in-region (beg end)
  "Display a Dired buffer for the file links in the selected region."
  (interactive "r")
  (let ((files
         (-map
          (lambda (x)
            (expand-file-name (org-link-unescape (plist-get (cadr x) :path))))
          (-filter
           (lambda (x)
             (string= (plist-get (cadr x) :type) "file"))
           (my-org-get-links-in-region beg end)))))
    (with-current-buffer (get-buffer-create "*Files*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (apply 'call-process "ls" nil t nil "-lR" files))
      (dired-virtual "/")
      (switch-to-buffer (current-buffer)))))

;;;###autoload
(defun my-make-slug (s)
  (thread-last
    s
    (learn-lang-replace-accents)
    (downcase)
    (replace-regexp-in-string "[^a-z0-9]+" "-")
    (replace-regexp-in-string "^-\\|-$" "")))

;;;###autoload
(defun my-org-set-custom-id (id)
  "Set the CUSTOM_ID property to ID at point."
  (interactive (list
                (let ((default-custom-id (my-make-slug (string-join (org-get-outline-path t) " "))))
                  (read-string (format "ID (%s): " default-custom-id) nil nil default-custom-id))))
  (org-entry-put (point) "CUSTOM_ID" id))

(defun my-org-assign-custom-ids ()
	(interactive)
	(let ((custom-ids
				 (org-map-entries (lambda () (org-entry-get (point) "CUSTOM_ID")) "CUSTOM_ID={.}")))
		(org-map-entries
		 (lambda ()
			 (let ((slug
							(replace-regexp-in-string
							 "^-\\|-$" ""
							 (replace-regexp-in-string "[^A-Za-z0-9]+" "-"
																				 (downcase (string-join (org-get-outline-path t) " "))))))
				 (while (member slug custom-ids)
					 (setq slug (read-string "Manually set custom ID: ")))
				 (org-entry-put (point) "CUSTOM_ID" slug)))
		 "-CUSTOM_ID={.}")))

;;;###autoload
(defun my-org-summarize-task-status ()
  "Count number of tasks by status.
      Probably should make this a dblock someday."
  (interactive)
  (let (result)
    (org-map-entries
     (lambda ()
       (let ((todo (elt (org-heading-components) 2)))
         (if todo
             (if (assoc todo result)
                 (setcdr (assoc todo result)
                         (1+ (cdr (assoc todo result))))
               (setq result (cons (cons todo 1) result)))))))
    (message "%s" (mapconcat (lambda (x) (format "%s: %d" (car x) (cdr x)))
                             result "\n"))))

;;;###autoload
(defun my-org-days-between (start end)
  "Number of days between START and END (exclusive).
      This includes START but not END."
  (- (calendar-absolute-from-gregorian (org-date-to-gregorian end))
     (calendar-absolute-from-gregorian (org-date-to-gregorian start))))

;;;###autoload
(defun my-copy-code-as-org-block-and-gist (beg end)
  (interactive "r")
  (let ((filename (or (file-name-base) ""))
        (mode (symbol-name major-mode))
        (contents
         (if (use-region-p) (buffer-substring beg end) (buffer-string)))
        (gist (if (use-region-p) (gist-region beg end) (gist-buffer))))
    (kill-new
     (format "\n%s\n#+begin_src %s\n%s\n#+end_src\n"
             (org-link-make-string (oref (oref gist :data) :html-url) filename)
             (replace-regexp-in-string "-mode$" "" mode)
             contents))))

;;;###autoload
(defun my-org-table-as-alist (table)
  "Convert TABLE to an alist. Remember to set :colnames no."
  (let ((headers (seq-map 'intern (car table))))
    (cl-loop for x in (cdr table) collect (-zip headers x))))

;;;###autoload
(defun my-org-get-invoice-range-based-on-date (date)
  (let* ((invoice-date (org-date-to-gregorian date))
         (start (list (1- (car invoice-date)) 1 (elt invoice-date 2)))
         (end (list (car invoice-date) 1 (elt invoice-date 2))))
    (mapcar (lambda (date)
              (format-time-string "%F %H:%M" (encode-time 0 0 0 1 (elt date 0) (elt date 2))))
            (list start end))))

;;;###autoload
(defun my-org-quantified-get-hours-based-on-range (category start end)
  "Return the number of hours for the specified category."
  (/ (assoc-default category
                    (quantified-summarize-time start end)) 3600.0))

;; TODO: paginate
;;;###autoload
(defun my-org-quantified-get-detailed-hours-based-on-range (category start end)
  "Return a list of (date week-ending-date dow seconds) for CATEGORY from START to END."
  (let ((entries
         (quantified-parse-json
           (quantified-request (format "records.json?start=%s&end=%s&filter_string=%s&per_page=1000&split=split" start end (url-encode-url category))
                               nil "GET"))))
    (mapcar
     (lambda (entry)
       (let ((time (date-to-time (assoc-default 'timestamp entry))))
         (list
          (format-time-string "%F" time)
          (format-time-string "%F" (my-get-week-end-for-time time))
          (format-time-string "%a" time)
          (assoc-default 'duration entry))))
     entries)))

;;;###autoload
(defun my-get-week-end-for-time (time &optional week-ends-on-day)
  "WEEK-ENDS-ON-DAY: 0 is Sunday"
  (let* ((decoded (decode-time time))
         (dow (elt decoded 6))
         (end-week (or week-ends-on-day (% (+ 6 calendar-week-start-day) 7))))
    (encode-time
     (elt decoded 0)
     (elt decoded 1)
     (elt decoded 2)
     (+ (elt decoded 3)
        (% (+ 7 (- end-week dow)) 7))
     (elt decoded 4)
     (elt decoded 5))))

(ert-deftest my-org-get-week-ending-date ()
  (let ((calendar-week-start-day 6)
        (tests '(
                 ("2015-09-03" . "2015-09-04")
                 ("2015-12-01" . "2015-12-04")
                 ("2015-12-03" . "2015-12-04")
                 ("2015-12-04" . "2015-12-04")
                 ("2015-12-05" . "2015-12-11"))))
    (dolist (test tests)
      (should (string=
               (format-time-string
                "%F"
                (my-get-week-end-for-time (org-time-string-to-time (car test))))
               (cdr test)))
      (should (string=
               (format-time-string
                "%F"
                (my-get-week-end-for-time (org-time-string-to-time (car test)) 5))
               (cdr test))))))



;;;###autoload
(defun my-org-quantified-format-detailed-hours-as-table (list)
  "Return a table with rows for LIST.
        | Week ending ____ | Sat | Sun | Mon | Tue | Wed | Thu | Fri | Total |
        LIST elements should be in the form (date week-end-date dow seconds).
        See `my-org-quantified-get-detailed-hours-based-on-range'."
  ;; Group by week ending date
  (let ((days '("Sat" "Sun" "Mon" "Tue" "Wed" "Thu" "Fri")))
    (append
     (list (append '("Week ending") days '("Total")))
     (mapcar
      (lambda (row)
        (let ((day-values (-group-by (lambda (x) (elt x 2)) (cdr row)))
              (week-total 0))
          (append
           (list (format "Week ending %s" (format-time-string "%b %-e" (org-time-string-to-time (car row)))))
           (mapcar (lambda (day)
                     (if (assoc-default day day-values)
                         (format "%.1f"
                                 (apply '+
                                        (mapcar
                                         (lambda (day-val) (/ (elt day-val 3) 3600.0))
                                         (assoc-default day day-values))))
                       ""))
                   days)
           (list (format "%.1f"
                         (apply '+ (mapcar (lambda (day-val) (/ (elt day-val 3) 3600.0)) (cdr row)))))
           ))
        )
      (-sort (lambda (a b) (string< (car a) (car b))) (-group-by (lambda (x) (elt x 1)) list))))))


;;;###autoload
(defun my-org-quantified-hours-table ()
  (my-org-quantified-format-detailed-hours-as-table
   (apply 'my-org-quantified-get-detailed-hours-based-on-range
          (org-entry-get-with-inheritance "QUANTIFIED_CATEGORY")
          (my-org-get-invoice-range-based-on-date (org-entry-get-with-inheritance "INVOICE_DATE")))))

(ert-deftest my-org-get-invoice-range-based-on-date ()
  "Check if invoice range is sane."
  (should (equal (my-org-get-invoice-range-based-on-date "2015-12-05")
                 '("2015-11-01 00:00" "2015-12-01 00:00"))))

(defvar my-org-note-words-target (* 140 20))
;;;###autoload
(defun my-org-collect-notes (&optional block-name)
	(let (results)
		(org-block-map
		 (lambda ()
			 (unless (org-in-commented-heading-p)
				 (let ((elem (org-element-at-point)))
					 (when (string= (downcase (org-element-property :type elem))
													(or block-name "notes"))
						 (push (string-trim
													(buffer-substring-no-properties
													 (org-element-property :contents-begin elem)
													 (org-element-property :contents-end elem)))
									 results))))))
		(reverse results)))

;;;###autoload
(defun my-org-count-words-in-notes (&optional target block-name)
	"Count words in #+begin_notes blocks.
If TARGET or `my-org-note-words-target' is specified, calculate percentage and words left.
If BLOCK-NAME is specified, use that block type instead."
	(interactive)
	(let ((notes (my-org-collect-notes)))
		(with-temp-buffer
			(insert (string-join notes "\n"))
			(let ((num (count-words-region (point-min) (point-max))))
				(if (or target my-org-note-words-target)
						(message "%d words (%.f%% of %d, %d to go)"
										 num
										 (/ (* 100.0 num) my-org-note-words-target)
										 my-org-note-words-target
										 (- my-org-note-words-target num))
					(message "%d words" num))))))

;;;###autoload
(defun my-org-create-notes-buffer ()
	(interactive)
	(let ((notes (my-org-collect-notes)))
		(with-current-buffer (get-buffer-create "*Notes*")
			(insert (string-join notes "\n\n"))
			(switch-to-buffer (current-buffer)))))

;;;###autoload
(defun my-org-add-dashes-to-tag-regexps ()
  (setq org-complex-heading-regexp
        (concat "^\\(\\*+\\)"
                "\\(?: +" org-todo-regexp "\\)?"
                "\\(?: +\\(\\[#.\\]\\)\\)?"
                "\\(?: +\\(.*?\\)\\)??"
                "\\(?:[ \t]+\\(:[-[:alnum:]_@#%:]+:\\)\\)?"
                "[ \t]*$")
        org-complex-heading-regexp-format
        (concat "^\\(\\*+\\)"
                "\\(?: +" org-todo-regexp "\\)?"
                "\\(?: +\\(\\[#.\\]\\)\\)?"
                "\\(?: +"
                ;; Stats cookies can be stuck to body.
                "\\(?:\\[[0-9%%/]+\\] *\\)*"
                "\\(%s\\)"
                "\\(?: *\\[[0-9%%/]+\\]\\)*"
                "\\)"
                "\\(?:[ \t]+\\(:[-[:alnum:]_@#%%:]+:\\)\\)?"
                "[ \t]*$")
        org-todo-line-tags-regexp
        (concat "^\\(\\*+\\)"
                "\\(?: +" org-todo-regexp "\\)?"
                "\\(?: +\\(.*?\\)\\)??"
                "\\(?:[ \t]+\\(:[-[:alnum:]:_@#%]+:\\)\\)?"
                "[ \t]*$")))

;;;###autoload
(defun my-org-convert-region-from-markdown (beg end)
	(interactive "r")
	(shell-command-on-region beg end "pandoc -t org" nil t))

;;;###autoload
(defun my-read-phone-entries ()
  "Copy phone data to a summary Org file."
  (interactive)
  (mapc
   (lambda (filename)
     (let ((base (file-name-base filename)) contents timestamp category encoded-time date)
       (when (string-match "^[^ ]+ [^ ]+ \\([^ ]+\\) - \\(.*\\)" base)
         (setq time (seconds-to-time (/ (string-to-number (match-string 1 base)) 1000))
               encoded-time (decode-time time)
               date (list (elt encoded-time 4) (elt encoded-time 3) (elt encoded-time 5))
               category (match-string 2 base))
         (with-temp-buffer
           (insert-file-contents filename)
           (setq contents (s-trim (buffer-string))))
         (with-current-buffer
             (find-file "~/dropbox/tasker/summary.txt")
           (org-datetree-find-date-create date)
           (unless (save-excursion (re-search-forward (regexp-quote base) nil t))
             (goto-char (line-end-position))
             (insert "\n")
             (insert "**** " contents "  :" category ":\n" base "\n")
             (insert (format-time-string "[%Y-%m-%d %a %H:%M]\n" time))

             (if (member category '("Think" "Do"))
                 (save-excursion
                   (org-back-to-heading t)
                   (if (looking-at org-outline-regexp) (goto-char (1- (match-end 0))))
                   (unless (looking-at org-todo-regexp)
                     (org-todo "TODO"))))
             (if (string-match "^Energy \\([0-9]\\)" contents)
                 (org-set-property "ENERGY" (match-string 1 contents)))))
         (delete-file filename))))
   (directory-files "~/dropbox/tasker/data" t "\\.txt$")))

;;;###autoload
(defun my-reddit-list-upvoted (date)
  (interactive (list (org-read-date)))
  (let ((threshold (org-read-date nil t (concat (substring date 0 (min (length date) 10)) " 0:00")))
        (url my-reddit-upvoted-json)
        results)
    (while url
      (with-current-buffer (url-retrieve-synchronously url)
        (goto-char (point-min))
        (re-search-forward "^$")
        (let* ((data (json-read))
               (items (assoc-default 'children (assoc-default 'data data)))
               (after (assoc-default 'after (assoc-default 'data data)))
               (result
                (mapconcat
                 (lambda (item)
                   (let* ((o (assoc-default 'data item))
                          (title (assoc-default 'title o))
                          (url (helm-html-decode-entities-string (assoc-default 'url o)))
                          (date (seconds-to-time (assoc-default 'created_utc o)))
                          (permalink (concat "https://reddit.com" (assoc-default 'permalink o)))
                          (num-comments (assoc-default 'num_comments o 'eq 0)))
                     (when (time-less-p threshold date)
                       (if (and (> num-comments 0) (not (string-match "reddit\\.com" url)))
                           (format "- %s (%s)\n"
                                   (org-link-make-string (url-unhex-string url) title)
                                   (org-link-make-string (url-unhex-string permalink) "Reddit"))
                         (format "- %s\n" (org-link-make-string (url-unhex-string url) title))))))
                 items "")))

          (setq results (concat result "\n" results))
          (setq url
                (if (and after (> (length result) 0))
                    (concat my-reddit-upvoted-json "&after=" after)
                  nil)))))
    results))
;;  (my-reddit-list-upvoted "-mon")

;;;###autoload
(defun my-org-sort-list-in-custom-order (order)
  "Sort the current Org list so that items are in the specified order.
       ORDER is a list of regexps."
  (org-sort-list
   nil ?f
   (lambda ()
     (let ((case-fold-search t)
           (item
            (when (looking-at "[ \t]*[-+*0-9.)]+\\([ \t]+\\[[- X]\\]\\)?[ \t]+")
              (org-sort-remove-invisible (buffer-substring (match-end 0) (point-at-eol))))))
       (or (cl-position item order :test (lambda (a b) (string-match b a))) (1+ (length order)))))
   '<))

;;;###autoload
(defun my-org-insert-clipboard ()
  "Convert clipboard contents from HTML to Org and then paste (yank)."
  (interactive)
  (insert (shell-command-to-string "xclip -o -selection clipboard -t text/html | pandoc -f html -t json | pandoc -f json -t org")))

;;;###autoload
(defun my-org-insert-clipboard-without-data-images ()
  "Convert clipboard contents from HTML to Org and then insert, but replace images with placeholders"
  (interactive)
	(insert
	 (with-temp-buffer
		 (shell-command "xclip -o -selection clipboard -t text/html" (current-buffer))
		 (goto-char (point-min))
		 (while (re-search-forward "<img src='data:image[^>]*?'>" nil t)
			 (replace-match "{{{ image }}}"))
		 (buffer-string))))

;;;###autoload
(defun my-org-convert-clipboard-to-org-without-data-images ()
  "Convert clipboard contents from HTML to Org and then insert, but replace images with placeholders"
  (interactive)
	(kill-new
	 (with-temp-buffer
		 (shell-command "xclip -o -selection clipboard -t text/html" (current-buffer))
		 (goto-char (point-min))
		 (while (re-search-forward "<img src='data:image[^>]*?'>" nil t)
			 (replace-match "{{{ image }}}"))
		 (goto-char (point-min))
		 (while (re-search-forward "<\\(tr\\)>" nil t)
			 (replace-match "{{{ image }}}"))
		 (buffer-string))))

;;;###autoload
(defun my-org-set-property (property value)
  "In the current entry, set PROPERTY to VALUE.
Use the region if active."
  (interactive
	 (list
		(org-read-property-name)
    (when (region-active-p)
			(replace-regexp-in-string
			 "[ \n\t]+" " "
			 (buffer-substring (point) (mark))))))
  (org-set-property property value))

;;;###autoload
(defun my-org-send-to-bottom-of-list ()
  "Send the current line to the bottom of the list."
  (interactive)
  (beginning-of-line)
  (let ((kill-whole-line t))
    (save-excursion
      (kill-line 1)
      (org-end-of-item-list)
      (yank))))

;;;###autoload
(defun my-org-insert-book-highlights-from-libby (url)
	(interactive "MURL: ")
	(let-alist (plz 'get url :as #'json-read)
		(insert
		 "* "
		 .readingJourney.title.text
		 " - "
		 .readingJourney.author
		 "\n")
		(org-set-property "ISBN" .readingJourney.isbn)
		(org-set-property "COVER" .readingJourney.cover.url)
		(org-set-property "TITLE" .readingJourney.title.text)
		(org-set-property "AUTHOR" .readingJourney.author)
		(insert (org-link-make-string .readingJourney.title.url .readingJourney.cover.url)
						"\n")
		;; sort the highlights by chapter
		(insert
		 (mapconcat
			(lambda (row)
				(concat "** " (replace-regexp-in-string " +" " " (car row)) "\n"
								(mapconcat (lambda (quote)
														 (concat "#+begin_quote\n"
																		 (alist-get 'quote quote)
																		 "\n#+end_quote\n\n"))
													 (cdr row)
													 "")
								"\n\n"))
			(seq-group-by
			 (lambda (o) (alist-get 'chapter o))
			 (sort .highlights
						 :key (lambda (o) (alist-get 'percent o))))))))

;;;###autoload
(defun my-org-copy-export (link desc format)
	(pcase format
		('org (org-link-make-string (concat "copy:" link) desc))
		(_ desc)))

;;;###autoload
      (defun my-org-get-list-categories ()
        "Return a list of (category indent matching-regexp sample).
              List categories are items that don't contain links."
        (let ((list (org-list-struct)) last-category results)
          (save-excursion
            (mapc
             (lambda (x)
               (goto-char (car x))
               (let ((current-item
                      (buffer-substring-no-properties
                       (+ (point)
                          (elt x 1)
                          (length (elt x 2)))
                       (line-end-position))))
                 (if (string-match
                      org-link-bracket-re
                      (buffer-substring-no-properties
                       (point)
                       (line-end-position)))
                     ;; Link - update the last category
                     (when last-category
                       (if (< (elt x 1) (elt last-category 1))
                           (setq results
                                 (cons (append last-category
                                               (list
                                                (match-string-no-properties
                                                 3
                                                 (buffer-substring-no-properties
                                                  (point)
                                                  (line-end-position)))))
                                       (cdr results))))
                       (setq last-category nil))
                   ;; Category
                   (setq results
                         (cons
                          (setq last-category
                                (list
                                 current-item
                                 (elt x 1)
                                 (concat "^"
                                         (make-string (elt x 1) ?\ )
                                         (regexp-quote
                                          (concat (elt x 2)
                                                  current-item))
                                         "$")))
                          results)))))
             list))
          (append '(("x" 2 "^$" nil)) results)))

;;;###autoload
(defun my-org-move-current-item-to-category (category)
    "Move current list item under CATEGORY earlier in the list.
  CATEGORY can be a string or a list of the form (text indent regexp).
  Point should be on the next line to process, even if a new category
  has been inserted."
    (interactive (list (completing-read "Category: " (my-org-get-list-categories))))
    (when category
      (let* ((col (current-column))
             (item (point-at-bol))
             (struct (org-list-struct))
             (category-text (if (stringp category) category (elt category 0)))
             (category-indent (if (stringp category) 2 (+ 2 (elt category 1))))
             (category-regexp (if (stringp category) category (elt category 2)))
             (end (elt (car (last struct)) 6))
             (pos (point))
             s)
        (setq s (org-remove-indentation (buffer-substring-no-properties item (org-list-get-item-end item struct))))
        (save-excursion
          (if (string= category-text "x")
              (org-list-send-item item 'delete struct)
            (goto-char (caar struct))
            (if (re-search-forward (concat "^ *- +" category-regexp) end t)
                (progn
                  ;; needs a patch to ol.el to check if stringp
                  (org-list-send-item item (point-at-bol) struct)
                  (org-move-item-down)
                  (org-indent-item))
              (goto-char end)
              (org-list-insert-item
               (point-at-bol)
               struct (org-list-prevs-alist struct))
              (let ((old-struct (copy-tree struct)))
                (org-list-set-ind (point-at-bol) struct 0)
                (org-list-struct-fix-bul struct (org-list-prevs-alist struct))
                (org-list-struct-apply-struct struct old-struct))
              (goto-char (point-at-eol))
              (insert category-text)
              (org-list-send-item item 'end struct)
              (org-indent-item)
              (org-indent-item))
            (recenter))))))

;;;###autoload
(defun my-org-guess-list-category (&optional categories)
  (interactive)
  (require 'cl-lib)
  (unless categories
    (setq categories
          (my-helm-org-list-categories-init-candidates)))
  (let* ((beg (line-beginning-position))
         (end (line-end-position))
         (string (buffer-substring-no-properties beg end))
         (found
          (cl-member string
                     categories
                     :test
                     (lambda (string cat-entry)
                       (unless (string= (car cat-entry) "x")
                         (string-match (regexp-quote (downcase (car cat-entry)))
                                       string))))))
    (when (car found)
      (my-org-move-current-item-to-category
       (cdr (car found)))
      t)))

(defvar my-org-browse-link-while-categorizing 'eww-readable
  "Set to nil to skip browsing.")

;;;###autoload
(defun my-org-guess-uncategorized ()
  "Interactively move linked list items to categories from the list.
        Try to guess categories based on substring matches."
  (interactive)
                                        ;(my-helm-org-list-categories-init-candidates)
  (let ((categories (my-org-get-list-categories))
        category)
    (while (and (looking-at "^[-+] \\[\\[\\([^]]+\\)\\]\\[\\([^]]+*\\)")
                (not (string= "done" category)))
      (save-excursion
        ;; (when (eq my-org-browse-link-while-categorizing 'eww-readable)
        ;;   (save-excursion (save-match-data (my-eww-browse-readable (match-string 1)))))
        (setq category (completing-read (match-string 2) categories))
        (unless (string= category "done")
          (my-org-move-current-item-to-category category))))))

;; From https://emacs.stackexchange.com/questions/36284/how-to-open-eww-in-readable-mode/47757
;;;###autoload
(defun my-eww-readable-nonce ()
  "Once-off call to `eww-readable' after EWW is done rendering."
  (unwind-protect
      (eww-readable)
    (remove-hook 'eww-after-render-hook #'my-eww-readable-nonce)))

;;;###autoload
(defun my-eww-browse-readable (url)
  (when (looking-at "^[-+] \\[\\[\\([^]]+\\)")
    (add-hook 'eww-after-render-hook #'my-eww-readable-nonce)
    (eww (match-string 1))))


;;;###autoload
(defun my-org-sort-list-by-regexp (regexp)
  (interactive "MRegexp: ")
  (let ((sort-func
         (lambda ()
           (let ((line (buffer-substring-no-properties (point) (line-end-position))))
             (if (string-match regexp line)
                 (if (string-match org-link-bracket-re line)
                     (match-string 2 line)
                   "ZZZ")
               "ZZZZZ")))))
    (funcall
     (cond
      ((org-at-table-p) 'org-table-sort-lines)
      ((org-at-item-p) 'org-sort-list)
      (t 'org-sort-entries))
     nil ?f sort-func (lambda (a b) (if (and (stringp a) (stringp b)) (string< a b) t)))))

;;;###autoload
(defun my-org-copy-as-doodle ()
	(interactive)
	(cond
	 ((derived-mode-p 'dired-mode)
		(kill-new
		 (mapconcat
			(lambda (s)
				(format
				 "#+begin_center-doodle\n#+ATTR_HTML: :style max-height:100px :alt \n[[file:%s]]\n#+end_center-doodle"
				 s))
			(dired-get-marked-files) "\n\n")))
	 ((derived-mode-p 'image-mode)
		(kill-new
		 (format
				 "#+begin_center-doodle\n#+ATTR_HTML: :style max-height:100px :alt \n%s\n#+end_center-doodle"
				 (org-link-make-string (concat "file:" (buffer-file-name))))))
	))

;;;###autoload
(defun my-org-next-item-or-paragraph (&optional by-sentence)
  (cond
   ((org-in-item-p)
    (condition-case nil
        (progn
          (org-next-item)
          (when (looking-at org-list-full-item-re)
            (goto-char (match-end 0))))
      (error nil)))
   (by-sentence (forward-sentence))
   (t
    (forward-paragraph)
    (skip-syntax-forward " ")
    (when (looking-at org-list-full-item-re)
      (goto-char (match-end 0)))
    ; Move to start of a list item
    (when (looking-at org-heading-regexp)
      (org-end-of-meta-data t))
    (skip-syntax-forward " "))))

;;;###autoload
(defun my-org-link-url-from-string (s)
	"Return the link URL from S."
	(if (string-match org-link-any-re s)
			(or
			 (match-string 7 s)
			 (match-string 2 s))))

(require 'quantified nil t)

(defmacro my-org-with-current-task (&rest body)
  "Execute BODY with the point at the subtree of the current task."
  (declare (debug t))
  `(if (derived-mode-p 'org-agenda-mode)
       (save-window-excursion
         (org-agenda-switch-to)
         ,@body)
     ,@body))

;;;###autoload
(defun my-org-clock-in-and-track ()
  "Start the clock running. Clock into Quantified Awesome."
  (interactive)
  (my-org-with-current-task
   (org-clock-in)
   (call-interactively 'my-org-quantified-track)
   ;(when (websocket-openp obs-websocket)  (my-stream-message (org-get-heading t t t t)))
   (cond
    ((org-entry-get (point) "AUTO")
     (org-link-open-from-string (org-entry-get (point) "AUTO")))
    (t
     (save-restriction
       (org-narrow-to-subtree)
       (org-next-link)
       (when (looking-at org-link-any-re)
         (org-open-at-point)))))))

(defmacro my-with-org-task (&rest body)
  "Run BODY within the current agenda task, clocked task, or cursor task."
  `(cond
    ((derived-mode-p 'org-agenda-mode)
     (let* ((marker (org-get-at-bol 'org-marker))
            (buffer (marker-buffer marker))
            (pos (marker-position marker)))
       (with-current-buffer buffer
         (save-excursion
           (save-restriction
             (widen)
             (goto-char pos)
             ,@body)))))
    ((and (derived-mode-p 'org-mode) (org-at-heading-p)) (save-excursion ,@body))
    ((org-clocking-p) (save-excursion (org-clock-goto) ,@body))
    ((derived-mode-p 'org-mode) ,@body)))

(defvar my-org-quantified-regexps
	'(("emacsconf" . "Emacs | Emacsconf")
		("emacs" . "Emacs")
		("consulting" . "E1 Gen"))
	"Alist of regexp . category.")

;;;###autoload
(defun my-org-quantified-track (&optional category note)
  "Create a tracking record using CATEGORY and NOTE.
      Default to the current task in the agenda, the currently-clocked
      entry, or the current subtree in Org."
  (interactive (list nil nil))
  (unless (and category note)
    (my-with-org-task
     (setq category (or category
                        (org-entry-get-with-inheritance "QUANTIFIED")))
     (cond
      ((null category)
			 (let* ((heading (org-get-heading))
							(guess (seq-find (lambda (entry)
																 (string-match (car entry)
																							 heading))
															 my-org-quantified-regexps)))
				 (setq category (or (cdr guess) (read-string "Category: "))))
       (org-set-property "QUANTIFIED" category))
      ((string= category ' "ask")
       (setq category (read-string "Category: "))))
     (setq note
           (concat
            (if (string= (or (org-entry-get-with-inheritance "QUANTIFIEDQUIET") "") "t")
                "!private "
              "")
            (or note (elt (org-heading-components) 4) (read-string "Note: "))))))
  (quantified-track (concat category " | " note)))

;;;###autoload
(defun my-org-quick-clock-in-task (location jump)
  "Track and clock in on the specified task.
      If JUMP is non-nil or the function is called with the prefix argument, jump to that location afterwards."
  (interactive (list (save-excursion (my-org-refile-get-location "Location")) current-prefix-arg))
  (when location
    (if jump
        (progn (org-refile 4 nil location) (my-org-clock-in-and-track))
      (save-window-excursion
        (org-refile 4 nil location)
        (my-org-clock-in-and-track)))))


;;;###autoload
(defun my-compare-times (clocked estimated)
  (if (and (> (length clocked) 0) estimated)
      (format "%.2f"
              (/ (* 1.0 (org-hh:mm-string-to-minutes clocked))
                 (org-hh:mm-string-to-minutes estimated)))
    ""))

;;;###autoload
(defun my-org-summarize-upcoming-week ()
  "Summarize upcoming tasks as a list."
  (interactive)
  (org-agenda nil "w")
  (let ((string (buffer-string))
        business relationships life)
    (with-temp-buffer
      (insert string)
      (goto-char (point-min))
      (while (re-search-forward my-weekly-review-line-regexp nil t)
        (cond
         ((string= (match-string 1) "routines") nil) ; skip routine tasks
         ((string= (match-string 1) "business")
          (add-to-list 'business (concat "  - [ ] " (match-string 3))))
         ((string= (match-string 1) "people")
          (add-to-list 'relationships (concat "  - [ ] " (match-string 3))))
         (t (add-to-list 'life (concat "  - [ ] " (match-string 3)))))))
    (setq string
          (concat
           "*Plans for next week*\n"
           "- Business\n"
           (mapconcat 'identity business "\n")
           "\n- Relationships\n"
           (mapconcat 'identity relationships "\n")
           "\n- Life\n"
           (mapconcat 'identity life "\n")))
    (if (called-interactively-p 'any)
        (kill-new string)
      string)))

;;;###autoload
(defun my-org-summarize-previous-week ()
  "Summarize previously-completed tasks as a list."
  (interactive)
  (save-window-excursion
    (org-agenda nil "w")
    (org-agenda-later -1)
    (org-agenda-log-mode 16)
    (let ((string (buffer-string))
          business relationships life)
      (with-temp-buffer
        (insert string)
        (goto-char (point-min))
        (while (re-search-forward my-weekly-review-line-regexp nil t)
          (cond
           ((string= (match-string 1) "routines") nil) ; skip routine tasks
           ((string= (match-string 1) "business")
            (add-to-list 'business (concat "  - " (match-string 2))))
           ((string= (match-string 1) "people")
            (add-to-list 'relationships (concat "  - " (match-string 2))))
           (t (add-to-list 'life (concat "  - " (match-string 2)))))))
      (setq string
            (concat
             "*Accomplished this week*\n\n"
             "- Business\n"
             (mapconcat 'identity business "\n")
             "\n- Relationships\n"
             (mapconcat 'identity relationships "\n")
             "\n- Life\n"
             (mapconcat 'identity life "\n")))
      (if (called-interactively-p 'any)
          (kill-new string)
        string))))


;;;###autoload
(defun my-show-emacs-tasks ()
  (interactive)
  (org-ql-search (org-agenda-files)
    '(and (todo)
          (parent (and (or (tags "stream")
                           (tags "project"))
                       (tags "emacs")
                       (not (tags "inactive")))))
    :title "Emacs-related project tasks"
    :sort '(date priority todo)
    :super-groups '((:auto-parent t))))

;;;###autoload
(defun my-org-replace-heading (new-text)
  (interactive (list (read-string (concat (org-get-heading t t t t) ": "))))
  (org-back-to-heading)
  (when (looking-at org-complex-heading-regexp)
    (replace-match new-text t t nil 4)))


;;;###autoload
(defun my-org-edit-special-dwim ()
  (interactive)
  (cond
    ((org-src-edit-buffer-p) (org-edit-src-exit))
    ((org-in-src-block-p) (org-edit-special))
    ((derived-mode-p 'org-mode)
     (org-insert-structure-template "src emacs-lisp")
     (org-edit-special))))
;;;###autoload
(defun my-org-execute-special-dwim ()
  (interactive)
  (cond
    ((org-src-edit-buffer-p) (eval-buffer))
    ((org-in-src-block-p) (org-babel-execute-src-block))
    (t (eval-buffer))))

(defvar my-get-shopping-details-functions
	'(my-org-shopping-get-details-from-spookfox
		my-get-shopping-details-amazon
		my-get-shopping-details-uniqlo
		my-get-shopping-details-manually))

;;;###autoload
(defun my-get-shopping-details-manually (link)
	(when (string-match "theshoecompany\\|dsw" link)
		(browse-url link)
		(list
		 (cons 'url link)
		 (cons 'image (read-string "Image: "))
		 (cons 'price (read-string "Price: ")))))

;;;###autoload
(defun my-get-shopping-details-amazon (link)
	(when (string-match "amazon.ca" link)
		(with-current-buffer (url-retrieve-synchronously link)
			(goto-char (point-min))
			(re-search-forward "^$")
			(let ((doc (libxml-parse-html-region (point) (point-max))))
				(list (cons 'name (dom-text (dom-by-tag doc 'title)))
							(cons 'description (dom-texts (dom-by-id doc "productDescription")))
							(cons 'image (dom-attr (dom-by-tag (dom-by-id doc "imgTagWrapperId") 'img) 'src))
							(cons 'price
										(dom-texts (dom-by-id doc "priceblock_ourprice"))))))))

;;;###autoload
(defun my-shopping-reformat-ld-data (data)
  (let-alist data
    (list (cons 'name .name)
          (cons 'url (or .url .@id))
          (cons 'brand .brand.name)
          (cons 'description .description)
          (cons 'rating .aggregateRating.ratingValue)
          (cons 'ratingCount .aggregateRating.reviewCount)
          (cons 'image (cond
												((stringp .image) .image)
												((stringp .image.url) .image.url)
												(t (elt .image 0))))
          (cons 'price
                (assoc-default 'price (cond
																			 ((arrayp .offers)
																				(elt .offers 0))
																			 (t .offers)))))))

;;;###autoload
(defun my-get-shopping-details ()
  (goto-char (point-min))
  (let (data)
    (cond
     ((re-search-forward "  data-section-data
>" nil t)
      (setq data (json-read))
      (let-alist data
        (list (cons 'name .product.title)
              (cons 'brand .product.vendor)
              (cons 'description .product.description)
              (cons 'image (if (stringp .image) .image (concat "https:" .product.featured_image)))
              (cons 'price (/ .product.price 100.0)))))
     ((and (re-search-forward "<script type=\"application/ld\\+json\">" nil t)
           (null (re-search-forward "Fabric Fabric\\|angryballerinafabrics" nil t))) ; Carter's, Columbia?
      (setq data (json-read))
      (if (vectorp data) (setq data (elt data 0)))
      (if (assoc-default '@graph data)
          (setq data (assoc-default '@graph data)))
      (if (vectorp data) (setq data (elt data 0)))
			(my-shopping-reformat-ld-data data))
		 (t
			(goto-char (point-min))
      (re-search-forward "^$")
      (let* ((doc (libxml-parse-html-region (point) (point-max)))
						 (result
							`((name . ,(string-trim (dom-text (dom-by-tag doc "title"))))
								(description . ,(string-trim (dom-text (dom-by-tag doc "title")))))
							))
        (mapc (lambda (property)
                (let ((node
											 (dom-search
												doc
												(lambda (o)
													(delq nil
																(mapcar (lambda (p)
																					(or (string= (dom-attr o 'property) p)
																							(string-match p (or (dom-attr o 'class) ""))))
																				(cdr property)))))))
									(when node (add-to-list 'result (cons (car property)
																												(or (dom-attr node 'content)
																														(string-trim (dom-text node))))))))
              '((name "og:title" "pdp-product-title")
                (brand "og:brand")
                (url "og:url")
                (image "og:image")
                (description "og:description")
                (price "og:price:amount" "product:price:amount" "pdp-price-label")))
				result)
			))))
;;;###autoload
(defun my-org-insert-shopping-details ()
  (interactive)
	(save-excursion
		(org-insert-heading)
		(when (string-match "^https://" (car kill-ring))
			(save-excursion (yank)))
		(my-org-update-shopping-details)
		(when (org-entry-get (point) "NAME")
			(org-edit-headline (org-entry-get (point) "NAME")))))

;;;###autoload
(defun my-org-shopping-get-details-from-spookfox (&optional link)
	"Get shopping details from Spookfox for sites that use Javascript."
	(let* ((current-url
					(spookfox-js-injection-eval-in-active-tab "window.location.href" t))
				 (data
					(when (or (not link)
										(string= link current-url))
						(json-parse-string
						 (spookfox-js-injection-eval-in-active-tab
							"JSON.stringify({
name: window.rawData?.Product?.Name || document.querySelector('meta[property=\"og:title\"]')?.getAttribute('content'),
image: document.querySelector('meta[property=\"og:image\"]')?.getAttribute('content'),
description: window.rawData?.Product?.Description || document.querySelector('meta[property=\"og:description\"]')?.getAttribute('content'),
url: document.querySelector('link[rel=\"canonical\"]')?.getAttribute('href') || window.location.href,
price: window.rawData?.Product?.MinPrice || document.querySelector('.nl-price--total')?.textContent?.replace(/^\s*$/, '')
})"
							t)
						 :object-type 'alist))))
		(when (alist-get 'name data)
			data)))

;; (my-org-get-shopping-details-uniqlo "https://www.uniqlo.com/ca/en/products/E451023-000?colorCode=COL07&sizeCode=KSS020")
;;;###autoload
(defun my-org-update-shopping-details ()
  (interactive)
	(let (data)
		(if (and (< (point)
								(save-excursion (org-end-of-subtree)))
						 (re-search-forward org-link-any-re (save-excursion (org-end-of-subtree)) t))
				(let ((link (org-element-property :raw-link (org-element-context))))
					(setq
					 data
					 (or (run-hook-with-args-until-success 'my-get-shopping-details-functions link)
							 (with-current-buffer (url-retrieve-synchronously link)
								 (my-get-shopping-details)))))
			(setq data (my-org-shopping-get-details-from-spookfox)))
		(when data
			(let-alist data
				(org-entry-put (point) "NAME" \.name)
				(org-entry-put (point) "URL" \.url)
				(org-entry-put (point) "BRAND" \.brand)
				(org-entry-put (point) "DESCRIPTION" (replace-regexp-in-string "&#039;" "'" (replace-regexp-in-string "\n" " " (or \.description ""))))
				(org-entry-put (point) "IMAGE"
											 (if (string-match "https?" \.image)
													 \.image
												 (concat "https:" \.image)))
				(org-entry-put (point) "PRICE" (cond ((stringp \.price) \.price) ((numberp \.price) (format "%.2f" \.price)) (t "")))
				(if \.rating (org-entry-put (point) "RATING" (if (stringp \.rating) \.rating (format "%.1f" \.rating))))
				(if \.ratingCount (org-entry-put (point) "RATING_COUNT" (if (stringp \.ratingCount) \.ratingCount (number-to-string \.ratingCount))))))))

;;;###autoload
(defun my-org-format-shopping-subtree (&optional height large)
	(concat
	 "<style>body { max-width: 100% !important } #content { max-width: 100% !important } .item img { max-height: "
	 (or height "100px")
	 " } .item img:hover { max-height: " (or large "400px") " }</style><div style=\"display: flex; flex-wrap: wrap; align-items: flex-start\">"
	 (string-join
		(save-excursion
			(org-map-entries
			 (lambda ()
				 (if (org-entry-get (point) "URL")
						 (format
							"<div class=item style=\"width: %s\"><div><a href=\"%s\"><img src=\"%s\" height=100></a></div>
<div>%s</div>
<div><a href=\"%s\">%s</a></div>
<div>%s</div>
<div>%s</div></div>"
							(or height "200px")
							(org-entry-get (point) "URL")
							(org-entry-get (point) "IMAGE")
							(or  (org-entry-get (point) "PRICE") "")
							(org-entry-get (point) "URL")
							(url-domain (url-generic-parse-url (org-entry-get (point) "URL")))
							(or (org-entry-get (point) "NAME") "")
							(or (org-entry-get (point) "NOTES") ""))
					 ""))
			 nil
			 (if (org-before-first-heading-p) nil 'tree)))
		"")
	 "</div>"))

;;;###autoload
(defun my-get-shopping-details-uniqlo (link)
	(when (string-match "https://www.uniqlo.com/ca/en/products/\\([^?]+\\)\\(\\?\\(.*\\)\\)?" link)
		(let ((code (match-string 1 link))
					(params (org-protocol-convert-query-to-plist (match-string 3 link)))
					item)
			(setq item
						(car
						 (assoc-default
							'items
							(assoc-default
							 'result
							 (with-current-buffer
									 (url-retrieve-synchronously
										(concat "https://www.uniqlo.com/ca/api/commerce/v3/en/products/" code))
								 (goto-char (point-min))
								 (re-search-forward "^$")
								 (json-parse-buffer :object-type 'alist :array-type 'list :null-object nil))))))
			(list
			 (cons 'price
						 (or (assoc-default
									'value
									(or
									 (assoc-default 'promo (assoc-default 'prices item))
									 (assoc-default 'base (assoc-default 'prices item))))
								 ""))
			 (cons 'image
						 (assoc-default
							'url
							(seq-find
							 (lambda (entry)
								 (or (null (plist-get params :colorCode))
										 (string=
											(concat "COL" (or (assoc-default 'colorCode entry) ""))
											(plist-get params :colorCode))))
							 (assoc-default 'main (assoc-default 'images item)))))
			 (cons 'price
						 (or (assoc-default
									'value
									(assoc-default
									 'promo
									 (assoc-default 'prices item)))
								 (assoc-default 'base (assoc-default 'prices item))
								 ""))
			 (cons 'name
						 (assoc-default 'name item))
			 (cons 'description
						 (concat (assoc-default 'longDescription item) " - "
										 (assoc-default 'washingDescription item)))
			 (cons 'url link)))))
