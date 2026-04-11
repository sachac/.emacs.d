(defvar my-whisper-org-reminder-template "t")

;;;###autoload
(defun my-whisper-org-process-reminder ()
  (let ((text (buffer-string))
        reminder)
    (when (string-match "computer[,\.]? reminds? me to \\(.+\\)" text)
      (setq reminder (match-string 1 text))
      (save-window-excursion
        (with-current-buffer (if (markerp whisper--marker) (marker-buffer whisper--marker) (current-buffer))
          (when (markerp whisper--marker) (goto-char whisper--marker))
          (org-capture nil my-whisper-org-reminder-template)
          (insert reminder)
          (org-capture-finalize)))
      (erase-buffer))))


(defvar my-whisper-dir "~/recordings/whisper/")
;;;###autoload
(defun my-whisper-set-temp-filename ()
  (setq whisper--temp-file (expand-file-name
                            (format-time-string "%Y-%m-%d-%H-%M-%S.wav")
                            my-whisper-dir)))


;;;###autoload
(defun my-whisper-replay (&optional file)
  "Replay the last temporary recording."
  (interactive (list
                (when current-prefix-arg
                  (read-file-name "File: " my-whisper-dir))))
  (setq whisper--temp-file (or file whisper--temp-file))
  (mpv-play whisper--temp-file))

;;;###autoload
(defun my-whisper-insert-retry (&optional file)
  (interactive (list
                (when current-prefix-arg
                  (read-file-name "File: " my-whisper-dir))))
  (whisper--cleanup-transcription)
  (setq whisper--marker (point-marker)
        whisper--temp-file (or file whisper--temp-file))
  (whisper--transcribe-audio))

;;;###autoload
(defun my-whisper-toggle-language ()
  "Set the language explicitly, since sometimes auto doesn't figure out the right one."
  (interactive)
  (setq whisper-language (if (string= whisper-language "en") "fr" "en"))
  ;; If using a server, we need to restart for the language
  (when (process-live-p whisper--server-process) (kill-process whisper--server-process))
  (message "%s" whisper-language))

;;;###autoload
(defun my-whisper-reset (text)
  (setq my-whisper-skip-annotation nil)
  (remove-hook 'whisper-insert-text-at-point #'my-whisper-org-save-to-clocked-task)
  text)

(defvar my-whisper-last-annotation nil "Last annotation so we can skip duplicates.")
(defvar my-whisper-skip-annotation nil)
(defvar my-whisper-target-markers nil "List of markers to send text to.")

;;;###autoload
(defun my-whisper-insert (text)
  (let ((markers
         (cond
          ((null my-whisper-target-markers)
           (list whisper--marker)) ; current point where whisper was started
          ((listp my-whisper-target-markers)
           my-whisper-target-markers)
          ((markerp my-whisper-target-markers)
           (list my-whisper-target-markers))))
        (orig-point (point))
        (orig-buffer (current-buffer)))
    (when text
      (mapcar (lambda (marker)
                (with-current-buffer (marker-buffer marker)
                  (save-restriction
                    (widen)
                    (when (markerp marker) (goto-char marker))
                    (when (and (derived-mode-p 'org-mode) (org-at-drawer-p))
                      (insert "\n"))
                    (whisper--insert-text
                     (concat
                      (if (looking-back "[ \t\n]\\|^")
                          ""
                        " ")
                      (string-trim text)))
                    ;; Move the marker forward here
                    (move-marker marker (point)))))
              markers)
      (when my-whisper-target-markers
        (goto-char orig-point))
      nil)))

;;;###autoload
(defun my-whisper-maybe-type (text)
  "If Emacs is not the focused app, simulate typing TEXT.
Add this function to `whisper-insert-text-at-point'."
  (when text
    (if (frame-focus-state)
        text
      (make-process :name "xdotool" :command
                    (list "xdotool" "type"
                          text))
      nil)))

;;;###autoload
(defun my-whisper-clear-markers ()
  (interactive)
  (setq my-whisper-target-markers nil))

;;;###autoload
(defun my-whisper-use-current-point (&optional add)
  (interactive (list current-prefix-arg))
  (if add
      (push (point-marker) my-whisper-target-markers)
    (setq my-whisper-target-markers (list (point-marker)))))

;;;###autoload
(defun my-whisper-run-at-point (&optional add)
  (interactive (list current-prefix-arg))
  (my-whisper-clear-markers)
  (whisper-run))


;;;###autoload
(defun my-whisper-jump-to-marker ()
  (interactive)
  (with-current-buffer (marker-buffer (car my-whisper-target-markers))
    (goto-char (car my-whisper-target-markers))))

;;;###autoload
(defun my-whisper-use-currently-clocked-task (&optional add)
  (interactive (list current-prefix-arg))
  (save-window-excursion
    (save-restriction
      (save-excursion
        (org-clock-goto)
        (org-end-of-meta-data)
        (org-end-of-subtree)
        (if add
            (push (point-marker) my-whisper-target-markers)
          (setq my-whisper-target-markers (list (point-marker))))))))

;;;###autoload
(defun my-whisper-run (&optional skip-annotation)
  (interactive (list current-prefix-arg))
  (require 'whisper)
  (add-hook 'whisper-insert-text-at-point #'my-whisper-org-save-to-clocked-task -10)
  (whisper-run)
  (when skip-annotation
    (setq my-whisper-skip-annotation t)))

;;;###autoload
(defun my-whisper-save-text (text)
  "Save TEXT beside `whisper--temp-file'."
  (when text
    (let ((link (org-store-link nil)))
      (with-temp-file (concat (file-name-sans-extension whisper--temp-file) ".txt")
        (when link
          (insert link "\n"))
        (insert text)))
    text))

;;;###autoload
(defun my-whisper-org-save-to-clocked-task (text)
  (when text
    (save-window-excursion
      (with-current-buffer (if (markerp whisper--marker) (marker-buffer whisper--marker) (current-buffer))
        (when (markerp whisper--marker) (goto-char whisper--marker))
        ;; Take a screenshot maybe
        (let* ((link (and (not my-whisper-skip-annotation)
                          (org-store-link nil)))
               (region (and (region-active-p) (buffer-substring (region-beginning) (region-end))))
               (screenshot-filename
                (when (or
                       (null link)
                       (not (string= my-whisper-last-annotation link))
                       (not (frame-focus-state))) ; not in focus, take a screenshot
                  (my-screenshot-current-screen (concat (file-name-sans-extension whisper--temp-file) ".png")))))
          (if (org-clocking-p)
              (save-window-excursion
                (save-restriction
                  (save-excursion
                    (org-clock-goto)
                    (org-end-of-subtree)
                    (unless (bolp)
                      (insert "\n"))
                    (insert "\n")
                    (if (and link (not (string= my-whisper-last-annotation link)))
                        (insert
                         (if screenshot-filename
                             (concat "(" (org-link-make-string
                                          (concat "file:" screenshot-filename)
                                          "screenshot") ") ")
                           "")
                         link
                         "\n")
                      (when screenshot-filename
                        (insert (org-link-make-string
                                 (concat "file:" screenshot-filename)
                                 "screenshot")
                                "\n")))
                    (when region
                      (insert "#+begin_example\n" region "\n#+end_example\n"))
                    (insert text "\n")
                    (setq my-whisper-last-annotation link)))
                (run-at-time 0.5 nil (lambda (text) (message "Added clock note: %s" text)) text))
            ;; No clocked task, prompt for a place to capture it
            (kill-new text)
            (setq org-capture-initial text)
            (call-interactively 'org-capture)
            ;; Delay the window configuration
            (let ((config (current-window-configuration)))
              (run-at-time 0.5 nil
                           (lambda (text config)
                             (set-window-configuration config)
                             (message "Copied: %s" text))
                           text config))))))))


;;;###autoload
(defun my-whisper-org-clear-saved-annotation ()
  (setq my-whisper-org-last-annotation nil))

(defvar my-whisper-notes "~/sync/stream/narration.org")
;;;###autoload
(defun my-whisper-save-to-file (text)
  (when text
    (let ((link (org-store-link nil)))
      (with-current-buffer (find-file-noselect my-whisper-notes)
        (goto-char (point-max))
        (insert "\n\n" (format-time-string "%H:%M ") text "\n" (if link (concat link "\n") ""))
        (save-buffer)
        (run-at-time 0.5 nil (lambda (text) (message "Saved to file: %s" text)) text)))
    text))

;;;###autoload
(defun my-whisper-redo ()
  (interactive)
  (setq whisper--marker (point-marker))
  (whisper--transcribe-audio))

(defvar my-whisper-url-format "http://%s:%d/transcribe")
;;;###autoload
(defun my-whisper--transcribe-via-local-server ()
  "Transcribe audio using the local whisper server."
  (message "[-] Transcribing via local server")
  (whisper--setup-mode-line :show 'transcribing)
  (whisper--ensure-server)
  (setq whisper--transcribing-process
        (whisper--process-curl-request
         (format my-whisper-url-format whisper-server-host whisper-server-port)
         (list "Content-Type: multipart/form-data")
         (list (concat "file=@" whisper--temp-file)
               "temperature=0.0"
               "temperature_inc=0.2"
               "response_format=json"
               (concat "model=" whisper-model)
               (concat "language=" whisper-language)))))
;;;###autoload
(defun my-whisper--check-model-consistency () t)

(defvar sacha-speech-input-model-aliases
  '(("small" . "Systran/faster-whisper-small.en")
    ("medium" . "Systran/faster-whisper-medium.en")
    ("base" . "Systran/faster-whisper-base.en")
    ("tiny" . "Systran/faster-whisper-tiny.en")
    ("large" . "Systran/faster-whisper-large-v2")))

(defun sacha-speech-input-set-model (model-name)
  "Change the speech recognition model to MODEL-NAME.
Use `sacha-speech-input-model-aliases' for aliases."
  (interactive (list (speech-input-speaches-read-model-name)))
  (when (assoc-default model-name sacha-speech-input-model-aliases #'string=)
    (setq model-name (assoc-default model-name sacha-speech-input-model-aliases #'string=)))
  (setq whisper-model model-name)
  (setq speech-input-model model-name))

(defvar my-whisper--queue nil)
;;;###autoload
(defun my-whisper-continue (&optional arg)
  "Send what we've got so far for transcription and then continue recording.
Call with \\[universal-argument] to signal that we can stop."
  (interactive "P")
  (require 'whisper)
  (if arg
      (my-whisper-done)
    (setq whisper--marker (point-marker) whisper--point-buffer (current-buffer))
    (when (process-live-p whisper--recording-process)
      ;; queue only if the last one is not asking for the same file
      (unless
          (string=
           (plist-get
            (car
             (last my-whisper--queue))
            :file)
           whisper--temp-file)
        (add-to-list
         'my-whisper--queue
         (list :file whisper--temp-file
               :buffer
               (format "*result: %s*" (file-name-base whisper--temp-file)))
         t))
      ;; Remove the sentinel; handle results ourselves
      (set-process-sentinel whisper--recording-process
                            (lambda (process event)
                              (my-whisper-process-queue)))
      (interrupt-process whisper--recording-process))
    (run-hooks 'whisper-before-transcription-hook)
    (whisper--setup-mode-line :show 'recording)
    (whisper--record-audio)))

;;;###autoload
(defun my-whisper-discard ()
 "Ignore the previous recording."
  (interactive)
  (when (process-live-p whisper--recording-process)
    ;; Remove the sentinel; handle results ourselves
    (set-process-sentinel whisper--recording-process
                          (lambda (process event)
                            (when (file-exists-p whisper--temp-file)
                              (delete-file whisper--temp-file))
                            (my-whisper-process-queue)))
    (interrupt-process whisper--recording-process)))

;;;###autoload
(defun my-whisper-discard-and-continue ()
 "Ignore the previous recording and continue."
  (interactive)
  (if (process-live-p whisper--recording-process)
      (progn
        ;; Remove the sentinel; handle results ourselves
        (set-process-sentinel whisper--recording-process
                              (lambda (process event)
                                (my-whisper-process-queue)
                                (my-whisper-continue)))
        (interrupt-process whisper--recording-process))
    (my-whisper-continue)))

;;;###autoload
(defun my-whisper-done ()
  (interactive)
  (when (process-live-p whisper--recording-process)
    (add-to-list
     'my-whisper--queue
     (list :file whisper--temp-file
           :buffer
           (format "*result: %s*" (file-name-base whisper--temp-file)))
     t)
    ;; Remove the sentinel; handle results ourselves
    (set-process-sentinel whisper--recording-process
                          (lambda (process event)
                            (my-whisper-process-queue)))
    (whisper--setup-mode-line :hide 'recording)
    (interrupt-process whisper--recording-process)))

;;;###autoload
(defun my-whisper-process-queue-result ()
  "Process the first part of the queue that already has results."
  (while (plist-get (car my-whisper--queue) :results)
    (let ((o (pop my-whisper--queue)))
      (unless my-whisper-target-markers
        (setq whisper--marker (point-marker)
              whisper--point-buffer (current-buffer)))
      (with-current-buffer (plist-get o :buffer)
        (erase-buffer)
        (insert (plist-get o :results)))
      ;; Only works with my fork: https://github.com/sachac/whisper.el/tree/whisper-insert-text-at-point-function
      (whisper--handle-transcription-output nil (plist-get o :buffer)))))

;;;###autoload
(defun my-whisper-process-queue ()
  (let (o)
    (while (setq o (seq-find (lambda (o) (and (plist-get o :file)
                                              (not (plist-get o :process))
                                              (not (plist-get o :results))))
                             my-whisper--queue))
      (let* ((headers (list "Content-Type: multipart/form-data"))
             (params (list (concat "file=@"
                                   (plist-get o :file))
                           "temperature=0.0"
                           "temperature_inc=0.2"
                           "response_format=json"
                           (concat "model=" whisper-model)
                           (concat "language=" whisper-language)))
             (url (format my-whisper-url-format whisper-server-host whisper-server-port))
             (command `("curl" "-s"
                        ,url
                        ,@(mapcan (lambda (h) (list "-H" h)) headers)
                        ,@(mapcan (lambda (p) (list "-F" p)) params))))
        (with-current-buffer (get-buffer-create (plist-get o :buffer))
          (erase-buffer))
        (plist-put
         o :process
         (make-process
          :name "whisper-curl"
          :command command
          :buffer (plist-get o :buffer)
          :coding 'utf-8
          :sentinel
          (lambda (process event)
            (with-current-buffer (process-buffer process)
              (let ((current my-whisper--queue-item))
                (when (and (get-buffer (plist-get current :buffer))
                           (string-equal "finished\n" event))
                  (with-current-buffer (plist-get current :buffer)
                    (goto-char (point-min))
                    (plist-put current :results
                               (or
                                (condition-case nil
                                    (gethash "text" (json-parse-buffer))
                                  (error ""))
                                "(error)"))))))
            (my-whisper-process-queue-result))))
        (plist-put o :command (string-join command " "))
        (with-current-buffer (process-buffer (plist-get o :process))
          (setq-local my-whisper--queue-item o))))))
(defvar-local my-whisper--queue-item nil)

;;;###autoload
(defun my-whisper-reprocess-queue ()
  (interactive)
  (setq whisper--marker (point-marker) whisper--point-buffer (current-buffer))
  (mapc (lambda (o)
          (when (process-live-p (plist-get o :process))
            (kill-process (plist-get o :process)))
          (when (get-buffer (plist-get o :buffer))
            (kill-buffer (plist-get o :buffer)))
          (plist-put o :process nil)
          (plist-put o :results nil))
        my-whisper--queue)
  (my-whisper-process-queue))

;;;###autoload
(defun my-whisper-clear-queue ()
  (interactive)
  (mapc (lambda (o)
          (when (process-live-p (plist-get o :process))
            (kill-process (plist-get o :process)))
          (when (get-buffer (plist-get o :buffer))
            (kill-buffer (plist-get o :buffer)))
          (plist-put o :process nil)
          (plist-put o :results nil))
        my-whisper--queue)
  (setq my-whisper--queue nil))

(defvar-keymap my-whisper-simulated-continuous-mode-map
  :doc "Keymap for my-minor-mode."
  "S-<f2>" #'my-whisper-continue
  )
(define-key my-whisper-simulated-continuous-mode-map [remap whisper-run] #'my-whisper-continue)

(define-minor-mode my-whisper-simulated-continuous-mode
  "Simulate continuous speech recognition by queuing."
  :lighter "W"
  (if my-whisper-simulated-continuous-mode
      (message "Start speaking...")
    (message "All done.")
    (my-whisper-done)))


;;;###autoload
(defun my-whisper-maybe-continue ()
  (when (process-live-p whisper--recording-process)
    (my-whisper-continue)))

(defvar my-whisper-commands
  '(("scroll up" . scroll-down-command)
    ("scrolling up" . scroll-down-command)
    ("page up" . scroll-down-command)
    ("scroll down" . scroll-up-command)
    ("scroll down" . scroll-up-command)
    ("page down" . scroll-up-command)
    ("next page" . scroll-up-command)
    ("close other windows" . delete-other-windows)
    ("run the buffer" . eval-buffer)
    ("mark buffer" . mark-whole-buffer)
    ("mark paragraph" . mark-paragraph)
    ("expand" . expand-region)
    ("start emacs news" . sacha-workflow-emacs-news-start)
    ("update emacs calendar" . sacha-workflow-emacs-calendar-update)
    )
  "Commands for speech recognition.")

;;;###autoload
(defun my-whisper-handle-commands (text)
  ;; Let's do commands at the beginning of a speech segment for now
  (if (string-match (concat "^" (regexp-opt (mapcar 'car my-whisper-commands)) "\\>")
                    text)
      (progn
        (while (string-match (concat "^\\(" (regexp-opt (mapcar 'car my-whisper-commands)) "\\)\\>[,\\.\\?]? *")
                             text)
          (let* ((match (match-string 1 text))
                 (func (assoc-default (downcase match) my-whisper-commands #'string=)))
            (when func
              (message "Command: %s" match)
              (setq text (replace-match "" nil nil text))
              (cond
               ((commandp func)
                (call-interactively func))
               ((functionp func)
                (funcall func))))))
        text)
    text))

(defvar my-whisper-replacements
  '((" *\\<start \\(list\\|next\\) item\\>[\\.,] *" . "\n- ")
    (" *\\<start check ?box\\>[\\.,] *" . "\n- [ ] ")
    (" *start paragraph[\\.,]? *" . "\n\n")))

;;;###autoload
(defun my-whisper-process-replacements ()
  (goto-char (point-min))
  (when (looking-at " +") (replace-match ""))
  (let ((case-fold-search t))
    (cond
     ((re-search-forward  " *okay[,\\.]? stop recording" nil t)
      (when (process-live-p whisper--recording-process)
        (replace-match "")
        (message "Stopping.")
        (my-whisper-done)))))
  (dolist (rep my-whisper-replacements)
    (goto-char (point-min))
    (while (re-search-forward (car rep) nil t)
      (replace-match (cdr rep))))
  (goto-char (point-max))
  (insert " "))


(defvar my-quantified-common-categories
  '(("Emacs" . "Discretionary - Productive - Emacs")
    ("Child care" . "Childcare")
    ("French" . "Discretionary - French")
    ("Brigade" . "Discretionary - Productive - Bike Brigade")
    ("Consulting" . "E1 Gen")))

;;;###autoload
(defun my-speech-input-quantified-track (text)
  "Start tracking time."
  (if (and text
           (string-match "^ok\\(?:ay\\)?[,\\.]? track \\(.+\\)" text))
      (let ((category
             (speech-input-match-in-list
              (match-string 1 text)
              (mapcar 'car my-quantified-common-categories))))
        (message "Tracking %s" category)
        (quantified-track
         (assoc-default category my-quantified-common-categories #'string=))
        nil)
    text))

;;;###autoload
(defun my-whisper-translate ()
  (goto-char (point-min))
  (let ((case-fold-search t))
    (when (re-search-forward "okay[,\\.]? translate[,\\.]? \\(.+\\)\\|okay[,\\.]? \\(.+?\\) in French" nil t)
      (let* ((s (or (match-string 1) (match-string 2)))
             (translation (save-match-data (my-learn-lang-en-to-fr s))))
        (replace-match
         (propertize translation
                     'type-hint translation
                     'type-original s
                     'help-echo s))))))


;;;###autoload
(defun my-whisper-maybe-type-with-hints (text)
  "Add this function to `whisper-insert-text-at-point'."
  (let* ((hint (and text (org-find-text-property-in-string 'type-hint text)))
         (original (and text (org-find-text-property-in-string 'type-original text))))
    (if hint
        (progn
          (learn-lang-type-with-hint hint original)
          nil)
      text)))

;;;###autoload
(defun my-speech-sessions ()
  (seq-keep (lambda (o)
              (with-current-buffer o
                (when my-speech-session
                  (cons my-speech-session o))))
            (buffer-list)))

;;;###autoload
(defun my-speech-clear-all ()
  (interactive)
  (dolist (session (my-speech-sessions))
    (my-speech-clear session)))

;;;###autoload
(defun my-speech-clear (session)
  (interactive (list (my-speech-select-session)))
  (with-current-buffer (cdr session)
      (erase-buffer)
      (setq-local my-speech-previous-final nil)))

;;;###autoload
(defun my-speech-select-session (&optional prompt)
  (let ((sessions (my-speech-sessions)))
    (if (= (length sessions) 1)
        (car sessions)
      (assoc
       (completing-read
        (or prompt "Session: ")
        (mapcar 'car sessions))
       sessions))))

(defvar-local my-speech-input "VirtualMicSink:input")

;;;###autoload
(defun my-speech-rewire (&optional id input)
  "Unhook it from all input and reconnect it to `my-speech-input'.
Call with \\[universal-argument] to specify the input."
  (interactive (list (my-speech-select-session)
                     (if current-prefix-arg
                         (epwgraph-complete-logical-node-name)
                       my-speech-input)))
  (with-current-buffer (cdr id)
    (setq input (or input my-speech-input))
    (setq-local my-speech-input input)
    (let* ((node-name (concat (car id) ":input"))
           (session-ports (epwgraph-get-ports-with-logical-name
                           node-name))
           (new-ports (if (stringp input)
                          (epwgraph-get-ports-with-logical-name input)
                        input))
           (old-incoming (epwgraph-get-incoming-links session-ports)))
      (epwgraph-disconnect-all-inputs-for-logical-node session-ports)
      (epwgraph-connect-logical-nodes
       (epwgraph--map-channels new-ports session-ports)))))

;;;###autoload
(defun my-speech-get-text-and-clear (session)
  (let (text)
    (with-current-buffer (cdr session)
      (setq text (buffer-substring-no-properties (point-min) (point-max)))
      (erase-buffer)
      (setq-local my-speech-previous-final nil))
    text))

;;;###autoload
(defun my-speech-insert-at-point (session)
  (interactive (list (my-speech-select-session)))
  (insert (my-speech-get-text-and-clear session)))

;;;###autoload
(defun my-speech-save-to-clocked-task (session)
  (interactive (list (my-speech-select-session)))
  (save-window-excursion
    (let ((link (org-store-link nil)))
      (org-clock-goto)
      (org-end-of-subtree)
      (unless (bolp)
        (insert "\n"))
      (insert "\n")
      (when link (insert link "\n"))
      (insert (my-speech-get-text-and-clear session) "\n"))))

(defvar my-speech-etherpads nil "Alist of (session . pad-id)")
;; (setq my-speech-etherpads '(("chrome-VgjMhu" . "test")))

;;;###autoload
(defun my-speech-append-to-etherpad (info)
  (when (and info (string= (assoc-default 'type info) "FINAL"))
    (let-alist info
      (when-let* ((pad-id (assoc-default .session my-speech-etherpads #'string=)))
        (emacsconf-pad-append-text pad-id (concat "\n" .content)))))
  info)

;;;###autoload
(defun my-speech-link-etherpad (session pad-id)
  (interactive (list
                (my-speech-select-session)
                (read-string "Pad ID: ")))
  (add-to-list 'my-speech-etherpads
               (cons (concat "#" (car session))
                     pad-id)))

;;;###autoload
(defun my-speech-unlink-etherpad (pad-id)
  (interactive (list (completing-read "Pad: " (mapcar 'cdr my-speech-etherpads))))
  (setq my-speech-etherpads
        (seq-remove (lambda (o)
                      (string= (cdr o) pad-id))
                    my-speech-etherpads)))

(add-to-list 'my-speech-functions #'my-speech-append-to-etherpad)

(defvar my-speech-erc nil "Alist of (session . channel)")
;; (setq my-speech-erc '(("#chrome-HP7k8I" . "#emacsconf-test")))

;;;###autoload
(defun my-speech-send-to-erc (info)
  (when (and info (string= (assoc-default 'type info) "FINAL"))
    (let-alist info
      (when-let* ((channel (assoc-default .session my-speech-erc #'string=)))
        (emacsconf-erc-with-channels (list channel)
          (erc-send-message (string-trim .content))))))
  info)

;;;###autoload
(defun my-speech-link-erc (session channel)
  (interactive (list
                (my-speech-select-session)
                (read-string "Channel: ")))
  (add-to-list 'my-speech-erc
               (cons (concat "#" (car session))
                     channel)))

;;;###autoload
(defun my-speech-unlink-channel (channel)
  (interactive (list (completing-read "Channel: " (mapcar 'cdr my-speech-erc))))
  (setq my-speech-erc
        (seq-remove (lambda (o)
                      (string= (cdr o) channel))
                    my-speech-erc)))

(add-to-list 'my-speech-functions #'my-speech-send-to-erc)

;;;###autoload
(defun my-speech-fix-common-errors (info)
  (with-temp-buffer
    (insert (alist-get 'content info))
    (goto-char (point-min))
    (my-subed-fix-common-errors-from-start)
    (setf (alist-get 'content info) (buffer-string)))
  info)
(add-hook 'my-speech-functions #'my-speech-fix-common-errors -100)

;;;###autoload
(defun my-speech-insert-at-markers (info)
  (when (and my-whisper-target-markers info)
    (my-whisper-insert (alist-get 'content info))))
(add-hook 'my-speech-functions #'my-speech-insert-at-markers 100)


(defvar my-speech-timestamp-adjust-before 1000)
(defvar my-speech-timestamp-adjust-after 300)

;;;###autoload
(defun my-speech-subed-record-convert-timestamp (s)
  "Convert S into a relative number of milliseconds based on `subed-record-filename'."
  (floor (* (float-time (time-subtract (date-to-time s) subed-record-start-time)) 1000.0)))

;;;###autoload
(defun my-speech-subed-record-distance (s1 s2)
  (/
   (* 1.0
      (string-distance (downcase (replace-regexp-in-string "[^A-Za-z]"
                                                           ""
                                                           s1))
                       (downcase (replace-regexp-in-string "[^A-Za-z]"
                                                           ""
                                                           s2))))
   (max (length s1)
        (length s2))))

;;;###autoload
(defun my-speech-subed-record-close-enough (s1 s2)
  "Return t if it's close enough."
  (< (my-speech-subed-record-distance s1 s2) 0.3))

;;;###autoload
(defun my-speech-subed-record-update (info)
  (let ((start-ms (- (my-speech-subed-record-convert-timestamp
                      (alist-get 'start info))
                     my-speech-timestamp-adjust-before))
        (stop-ms (+ (my-speech-subed-record-convert-timestamp
                     (alist-get 'end info))
                    my-speech-timestamp-adjust-after)))
    (subed-set-subtitle-time-start start-ms)
    (subed-set-subtitle-time-stop stop-ms)
    (subed-set-subtitle-comment
	   (concat
		  (if (subed-subtitle-comment)
				  (concat (string-trim (replace-regexp-in-string
									              "#\\+AUDIO: .*\\(\n\\|$\\)?" ""
									              (subed-subtitle-comment)))
								  "\n")
			  "")
		  (format "#+AUDIO: %s" subed-record-filename)))
    (message "%.1f %s"
             (my-speech-subed-record-distance
              (alist-get 'content info)
              (subed-subtitle-text))
             (alist-get 'content info))))

(defvar my-speech-subed-ignore nil "Ignore the GTTS-CLI output.")
;;;###autoload
(defun my-speech-subed-record-process (info)
  (let ((text (alist-get 'content info))
        (current (subed-subtitle-text)))
    (cond
     ((my-speech-subed-record-close-enough text current)
      (my-speech-subed-record-update info)
      (subed-forward-subtitle-text)
      (my-learn-lang-say-current-subtitle
       (lambda ()
         (setq my-speech-subed-ignore nil))))
     ;; Check previous
     ((my-speech-subed-record-close-enough
       text
       (save-excursion
         (subed-backward-subtitle-text)
         (subed-subtitle-text)))
      (save-excursion
        (subed-backward-subtitle-text)
        (my-speech-subed-record-update info)))
     ;; Check next
     ((my-speech-subed-record-close-enough
       text
       (save-excursion
         (subed-forward-subtitle-text)
         (subed-subtitle-text)))
      (save-excursion
        (subed-forward-subtitle-text)
        (my-speech-subed-record-update info)))
     (t
      (my-speech-subed-record-update info)))))
;;;###autoload
(defun my-speech-subed-record (info)
  (when (and (string= (alist-get 'type info) "FINAL")
             (derived-mode-p 'subed-mode)
             (boundp 'subed-record-start-time)
             subed-record-start-time
             (not my-speech-subed-ignore))
    (my-speech-subed-record-process info))
  info)

(add-to-list 'my-speech-functions #'my-speech-subed-record)
