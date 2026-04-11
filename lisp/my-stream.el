;;;###autoload
  (defun my-redact (s)
          "Replace S with x characters."
          (make-string (length s) ?x))

;;;###autoload
  (defun my-redact-region (beg end &optional func)
          "Redact from BEG to END."
          (interactive "r")
          (let ((overlay (make-overlay beg end)))
                  (overlay-put overlay 'redact t)
      (overlay-put overlay 'evaporate t)
                  (overlay-put overlay 'display
                                                                   (cond
                                                                          ((functionp func)
                                                                           (funcall func))
                                                                          ((stringp func)
                                                                           func)
                                                                          (t (make-string (- end beg) ?x))))))

;;;###autoload
  (defun my-redact-regexp-replacement (regexp replacement &optional beg end)
    "Redact buffer content matching regexp."
    (interactive (list (read-regexp "Redact regexp: " 'regexp-history-last)
                                                                                   (read-string "Replacement (ex: \\1 \\,(my-redact \\2)): ")))
          (setq beg (or beg (point-min)))
          (setq end (or end (point-max)))
          (when (stringp replacement)
                  (setq replacement (query-replace-compile-replacement replacement t)))
          (save-excursion
      (goto-char beg)
      (while (re-search-forward regexp end t)
                          (my-redact-region
                           (match-beginning 0) (match-end 0)
                           (with-temp-buffer
                                   (insert (match-string 0))
                                   (goto-char (point-min))

                                   )
                           (replace-regexp-in-string regexp replacement (match-string 0))))))

;;;###autoload
  (defun my-redact-regexp (regexp &optional beg end func)
    "Redact buffer content matching regexp."
    (interactive (list (string-trim (read-regexp "Redact regexp: " 'regexp-history-last))))
          (save-excursion
      (goto-char (or beg (point-min)))
      (while (re-search-forward regexp (or end (point-max)) t)
                          (my-redact-region (match-beginning 0) (match-end 0) func))))

;;;###autoload
  (defun my-unredact ()
          (interactive)
          (mapc 'delete-overlay
                                  (seq-filter (lambda (overlay) (overlay-get overlay 'redact))
                                                                                  (overlays-in (point-min) (point-max)))))

;;;###autoload
  (defun my-redact-email-string (s)
          (replace-regexp-in-string
           "\\([-+_~a-zA-Z0-9][-+_.~:a-zA-Z0-9]*\\)@\\([-a-zA-Z0-9]+[-.a-zA-Z0-9]*\\)"
           (lambda (sub)
                   (concat
                          (make-string (length (match-string 1 sub)) ?x)
                          "@"
                          (make-string (length (match-string 2 sub)) ?x)))
           s))

;;;###autoload
  (defun my-redact-emails (&rest _)
          (interactive)
          (my-redact-regexp
           "\\([-+_~a-zA-Z0-9][-+_.~:a-zA-Z0-9]*\\)@\\([-a-zA-Z0-9]+[-.a-zA-Z0-9]*\\)"
           nil nil
           (lambda () (my-redact-email-string (match-string 0)))))

;;;###autoload
  (defun my-redact-emacsconf-org ()
          (interactive)
          (my-redact-regexp-replacement
           "\\(^:EMAIL:[ \t]+\\)\\(.+\\)"
           "\\1 \\,(my-redact \\2)"
           ))
;;;###autoload
  (defun my-redact-tabulated-list-in-rectangle (regexp beg end)
          ;; tabulated columns use substrings with display properties
          ;; so we should skip any characters that have text-property-any 'display
          (interactive (list (read-regexp "Redact regexp: " 'regexp-history-last)
                                                                                   (min (point) (mark))
                                                                                   (max (point) (mark))))
          (apply-on-rectangle
           (lambda (start-col end-col)
                   (let ((start-pos (and (move-to-column start-col) (point)))
                                           (end-pos (and (move-to-column end-col) (point)))
                                           display-prop)
                           (save-restriction
                                   (narrow-to-region start-pos end-pos)
                                   (goto-char start-pos)
                                   (setq display-prop (text-property-search-forward 'display))
                                   (if display-prop
                                                   (while display-prop
                                                           (my-redact-regexp regexp start-pos (prop-match-beginning display-prop))
                                                           (setq start-pos (prop-match-end display-prop))
                                                           (setq display-prop (text-property-search-forward 'display)))
                                           (my-redact-regexp regexp start-pos end-pos)))))
           beg end))

;;;###autoload
  (defun my-redact-regexp-in-rectangle (regexp beg end)
    (interactive (list (read-regexp "Redact regexp: " 'regexp-history-last)
                                                                                   (min (point) (mark))
                                                                                   (max (point) (mark))))
          (apply-on-rectangle (lambda (start-col end-col)
                                                                                                  (my-redact-regexp regexp
                                                                                                                                                                          (and (move-to-column start-col) (point))
                                                                                                                                                                          (and (move-to-column end-col) (point))))
                                                                                          beg end))

(defvar sacha-stream-old-variable-values nil "Alist of values to save.")
(defvar sacha-stream-inbox-file nil "File to save new items to.")
(defvar sacha-stream-inbox-target
  `(file+headline ,sacha-stream-inbox-file "Current / notes for next time"))
(defvar sacha-stream-variables-to-override
  `((org-agenda-files . ("~/sync/stream/index.org" "~/sync/topics/live.org" "~/sync/stream/inbox.org"))
    (org-refile-targets
     .
     ((("~/sync/stream/index.org"
        "~/sync/stream/inbox.org"
        "~/sync/topics/live.org"
        "~/sync/emacs/Sacha.org"
        "~/sync/orgzly/news.org") . t)))
    (my-org-inbox-file . ,sacha-stream-inbox-file)
    (my-file-shortcuts .
     (("C" "~/proj/emacs-calendar/README.org" "Emacs calendar")
      ("e" "~/sync/emacs/Sacha.org" "Config")
      ("E" "~/sync/emacs-news/index.org" "Emacs News")
      ("f" "~/sync/orgzly/journal-fr.org" "French journal")
      ("F" "~/sync/orgzly/french.org" "French")
      ("i" "~/sync/topics/live.org" "Live")
      ("s" "~/proj/stream/index.org" "Yay Emacs")
      ("p" "~/sync/orgzly/posts.org" "Posts")
      ("n" "~/sync/topics/now.org" "Now")
      ("w" "~/sync/topics/workflows.org" "Workflows")))
    (org-capture-templates
     .
     (("r" "Note" entry
       ,sacha-stream-inbox-target
       "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n%i\n\n- %a\n%U"
       :prepend t)
      ("u" "Update" item
       (file+headline ,sacha-stream-inbox-file "Updates")
       "- %U %?"
       :prepend t)
      ("F" "Firefox link" entry
       ,sacha-stream-inbox-target
       "* %^{Note}\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n%(org-link-make-string\n(my-spookfox-complete-link))")
      ("f" "Firefox" entry
       ,sacha-stream-inbox-target
       "* %^{Note}\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n%(apply #'org-link-make-string\n								 (append (spookfox-js-injection-eval-in-active-tab \"[window.location.href, document.title]\" t) nil))")
      ("📰" "Emacs News" entry
       (file+headline "~/sync/orgzly/news.org"
                      "Collect Emacs News")
       "* %a  :news:\n\n#+begin_quote\n%:text\n#+end_quote\n\n"
       :prepend t :immediate-finish t)
      ("m" "Mastodon" entry
       ,sacha-stream-inbox-target
       "* %?\n\n#+begin_quote\n%:text\n#+end_quote\n\n%a"
       :prepend t)

      ("t" "Task with annotation" entry
       ,sacha-stream-inbox-target
       "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n%a\n"
       :prepend t)
      ("i" "Interrupting task" entry
       ,sacha-stream-inbox-target
       "* STARTED %^{Task}\n:PROPERTIES:\n:CREATED: %U\n:END:\n%a\n"
       :clock-in :clock-resume :prepend t)
      ("T" "Task without annotation" entry
       ,sacha-stream-inbox-target
       "* TODO %^{Task}\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n"
       :prepend t)
      ("c" "Contents to current clocked task" plain
       (clock) "%i%?\n%a" :empty-lines 1)
      ("." "Today" entry
       ,sacha-stream-inbox-target
       "* TODO %^{Task}\nSCHEDULED: %t\n:PROPERTIES:\n:CREATED: %U\n:END:\n"
       :immediate-finish t)
      ("v" "Video" entry
       ,sacha-stream-inbox-target
       "* TODO %^{Task}  :video:\nSCHEDULED: %t\n"
       :immediate-finish t)
      ("e" "Errand" entry
       ,sacha-stream-inbox-target
       "* TODO %^{Task}  :errands:\n:PROPERTIES:\n:CREATED: %U\n:END:\n"
       :immediate-finish t)
      ("n" "Note" entry
       ,sacha-stream-inbox-target
       "* %^{Note}\n:PROPERTIES:\n:CREATED: %U\n:END:\n"
       :immediate-finish t)
      ("N" "Note" entry
       ,sacha-stream-inbox-target
       "* %^{Note}\n:PROPERTIES:\n:CREATED: %U\n:END:\n"
       :prepend t)
      ("s" "Selection from browser" entry
       ,sacha-stream-inbox-target
       "* %a :website:\n:PROPERTIES:\n:CREATED: %U\n:END:\n#+begin_quote\n%i\n#+end_quote\n\n%?\n"
       :prepend t)
      ("S" "Screenshot" entry
       ,sacha-stream-inbox-target
       "* %^{Note}\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n[[file:%(my-latest-screenshot)]]\n"
       :prepend t)
      ("q" "Quick note" item
       ,sacha-stream-inbox-target)
      ("w" "Web" entry (file ,sacha-stream-inbox-file)
       "* %a\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n%i\n")
      ("W" "Web bookmark" entry
       (file "~/sync/orgzly/resources.org")
       "* %a\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n%i\n"
       :prepend t)
      ("y" "Yay Emacs" entry
       (file+headline "~/proj/yayemacs/index.org"
                      "Notes for this session")
       "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n\n%i\n\n%a\n")))))

;;;###autoload
(defun sacha-stream-override-variables ()
  "Save and override variables for streaming."
  (interactive)
  (if sacha-stream-old-variable-values
      (message "Values already saved? Not overriding.")
    (setq sacha-stream-old-variable-values
          (mapcar
           (lambda (var)
             (prog1 (cons (car var) (symbol-value (car var)))
               (set (car var)
                    (cdr var))))
           sacha-stream-variables-to-override)))
  (when (featurep 'org)
    (org-refile-cache-clear)))

;;;###autoload
(defun sacha-stream-restore-variables ()
  "Restore values saved in `sacha-stream-old-variable-values'."
  (interactive)
  (mapc
   (lambda (var) (set (car var) (cdr var)))
   sacha-stream-old-variable-values)
  (setq sacha-stream-old-variable-values nil)
  (when (featurep 'org)
    (org-refile-cache-clear)))

;;;###autoload
(defun sacha-stream-refresh-variables ()
  "Restore and override `sacha-stream-variables-to-override'."
  (interactive)
  (sacha-stream-restore-variables)
  (sacha-stream-override-variables))

(defvar sacha-stream-sensitive-files-regexps
  '("\\.gpg"
    "Inbox.org"
    "organizer.org"
    "business.org")
  "List of regexps matching files that would ideally not end up on stream.")

;;;###autoload
(defun sacha-stream-clean-up-buffers ()
  "Clean up buffers that might be sensitive."
  (interactive)
  (org-save-all-org-buffers)
  (let (remaining cleaned)
    (mapc
     (lambda (buf)
       (with-current-buffer buf
         (when (and (buffer-file-name)
                    (seq-find (lambda (o) (string-match o (buffer-file-name)))
                              sacha-stream-sensitive-files-regexps))
           (if (buffer-modified-p buf)
               (push (buffer-name buf) remaining)
             (push (buffer-name buf) cleaned)
             (kill-buffer buf)))))
     (buffer-list))
    (message "%s remaining, %s cleaned" remaining cleaned)))

;;;###autoload
(define-minor-mode sacha-stream-or-video-global-mode
  "On air or doing a video."
  :init-val nil
  :lighter "🎥"
  (if sacha-stream-or-video-global-mode
      (progn
        (sacha-stream-clean-up-buffers)
        (global-display-line-numbers-mode 1)
        (load-theme 'modus-vivendi t)
        (fontaine-set-preset 'presentation)
        (keycast-header-line-mode 1)
        (sacha-stream-override-variables)
        (cl-pushnew
         'sacha-marginalia-annotate-variable
         (alist-get 'variable marginalia-annotators)))
    (global-display-line-numbers-mode -1)
    ;; TODO: Pick this based on the time? We'll assume light background since I
    ;; probably won't be doing too much coding late at night anyway.
    (load-theme 'modus-operandi-tinted t)
    (fontaine-set-preset 'regular)
    (keycast-header-line-mode -1)
    (setf
     (alist-get 'variable marginalia-annotators)
     (remove
      'sacha-marginalia-annotate-variable
      (alist-get 'variable marginalia-annotators)))
    (sacha-stream-restore-variables))
  (my-navigate-set-up-file-shortcuts))

(defvar my-stream-chat-process nil)
(defvar my-stream-chat-command `("npx" "masterchat-cli" "stream" "-n" "-t" "all"))
(defvar my-stream-chat-buffer "*stream-chat*")
;;;###autoload
(defun my-stream-chat-start (&optional url)
  "Start the process if it's not already running."
  (interactive (if (process-live-p my-stream-chat-process)
                   nil
                 (list (read-string "URL: "))))
  (if url
      (unless (process-live-p my-stream-chat-process)
        (with-current-buffer (get-buffer-create my-stream-chat-buffer)
          (erase-buffer)
          (setq my-stream-chat-process
                (make-process
                 :name "stream-chat"
                 :command (append my-stream-chat-command (list url))
                 :buffer (get-buffer-create my-stream-chat-buffer)
                 :stderr (get-buffer-create "*stream-chat-err*"))))
        (display-buffer (current-buffer)))
    (if (string= (buffer-name) my-stream-chat-buffer)
        (bury-buffer)
      (switch-to-buffer my-stream-chat-buffer))))

;;;###autoload
(defun my-stream-chat-stop ()
  (interactive)
  (kill-process my-stream-chat-process))

(defvar sacha-stream-display-file nil)

;;;###autoload
(defun sacha-stream-obs-display-text (text)
  "Display TEXT in the current task area in my OBS."
  (interactive (list (read-string "Text: ")))
  (when sacha-stream-display-file
    (write-region (string-join (org-wrap text 100) "\n")
                  nil sacha-stream-display-file)))

;;;###autoload
(defun my-stream-obs-org-display-current-task ()
  "Display the current task on OBS."
  (interactive)
  (sacha-stream-obs-display-text
   (if (member "stream" (org-get-tags))
       (org-entry-get (point) "ITEM")
     "")))

;;;###autoload
(defun my-org-clear-streaming-task ()
  "Clear the text."
  (sacha-stream-obs-display-text ""))


;;;###autoload
(defun my-stream-agenda ()
  (interactive)
  (org-agenda nil "s"))

;;;###autoload
(defun my-stream-captions-on-close (&rest args)
  (message "Captions websocket closed.")
  (my-stream-captions-minor-mode 0)
  (setq my-stream-captions-websocket nil))

;;;###autoload
(defun my-stream-captions-websocket-connect ()
  (interactive)
  (setq my-stream-captions-history nil)
  (my-stream-captions-minor-mode 1)
  (setq my-stream-captions-websocket (websocket-open "ws://localhost:8085"
                                                     :on-message #'my-stream-captions-on-message
                                                     :on-close #'my-stream-captions-on-close)))

(defvar my-stream-captions-process nil)
;;;###autoload
(defun my-stream-captions-start ()
  (interactive)
  (let ((default-directory "~/proj/speech"))
    (setq my-stream-captions-process (start-process "Stream captions" (get-buffer-create "*stream captions*") "node" "test.js"))
    (sleep-for 2)
    (my-stream-captions-websocket-connect)))

;;;###autoload
(defun my-stream-captions-sentinel (process event)
  (let ((status (process-status my-stream-captions-process)))
    (if (member status '(stop exit signal))
        (my-stream-captions-minor-mode -1))))
;;;###autoload
(defun my-stream-captions-stop ()
  (interactive)
  (stop-process my-stream-captions-process))
