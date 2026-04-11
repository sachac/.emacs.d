(defvar my-speech-chrome-ws nil "Websocket for connecting.")
(defvar my-speech-chrome-url "ws://127.0.0.1:8000/ws" "Websocket URL to connect to for captions.")
;;;###autoload
(defun my-speech-chrome-connect ()
  (interactive)
  (unless (websocket-p my-speech-chrome-ws)
    (setq my-speech-chrome-ws
          (websocket-open
           my-speech-chrome-url
           :on-message #'my-speech-chrome-handle))))
;;;###autoload
(defun my-speech-chrome-reconnect ()
  (interactive)
  (my-speech-chrome-disconnect)
  (my-speech-chrome-connect))

(defvar my-speech-functions '(my-speech-display)
  "Functions to run with the info as an argument.
The info is an alist with 'type and 'content.
The functions are called in sequence, with the first function getting the info
from the websocket message and the other functions getting the results of the
previous functions.")

(defvar-local my-speech-previous-final nil)

(defface my-chrome-caption-current
  '((t :height 200))
  "Display current caption.")

;;;###autoload
(defun my-speech-display (info)
  (let-alist info
    (with-current-buffer (get-buffer-create
                          (format "*Captions - %s*"
                                  .session))
      (when (and (string= .type "TEMP")
                 my-speech-previous-final)
        (goto-char (point-max))
        (delete-region
         (line-beginning-position)
         (line-end-position))
        (insert (propertize my-speech-previous-final
                            'face `(:foreground ,(modus-themes-get-color-value 'fg-dim)))
                "\n")
        (setq my-speech-previous-final nil))
      (when (string= .type "TEMP")
        (goto-char (point-max))
        (delete-region (line-beginning-position) (line-end-position))
        (insert .type (propertize .content 'face 'my-chrome-caption-current)))
      (when (string= .type "FINAL")
        (unless (string= my-speech-previous-final .content)
          (goto-char (point-max))
          (set-text-properties
           (line-beginning-position)
           (line-end-position)
           (list
            'face `(:foreground ,(modus-themes-get-color-value 'fg-dim))))
          (insert "\n"))
        (setq my-speech-previous-final .content)
        (goto-char (point-max))
        (delete-region (line-beginning-position) (line-end-position))
        (insert (propertize .content 'face 'my-chrome-caption-current)))))
  info)

;;;###autoload
(defun my-speech-chrome-handle (_ frame)
  (let* ((info (json-parse-string (websocket-frame-text frame)
                                  :object-type 'alist)))
    (seq-reduce (lambda (prev cur)
                  (funcall cur prev))
                my-speech-functions info)))

;;;###autoload
(defun my-speech-chrome-disconnect ()
  (interactive)
  (websocket-close my-speech-chrome-ws)
  (setq my-speech-chrome-ws nil))

(defvar my-speech-chrome-recognition-server-process nil)
(defvar my-speech-chrome-dir "~/proj/emacs-web-speech")
;;;###autoload
(defun my-chrome-ensure-speech-recognition-server ()
  (interactive)
  (unless (process-live-p my-speech-chrome-recognition-server-process)
    (let ((default-directory my-speech-chrome-dir))
      (setq my-speech-chrome-recognition-server-process
            (make-process
             :name "live-captioning"
             :buffer "*live-captioning*"
             :command (list (expand-file-name ".venv/bin/python3") "app.py")))
      (sit-for 1))))

;;;###autoload
(defun my-chrome-stop-speech-recognition-server ()
  (interactive)
  (when (process-live-p my-speech-chrome-recognition-server-process)
    (kill-process my-speech-chrome-recognition-server-process)))

(defvar-local my-speech-chrome-session nil)
(defvar-local my-speech-chrome-user-dir nil)
(defvar-local my-speech-chrome-lang "en-US")
;;;###autoload
(defun my-speech-chrome-new-session (&optional id lang local-only)
  (interactive (list
                (file-name-base
                 (make-temp-name
                  (expand-file-name "chrome-"
                                    (temporary-file-directory))))
                my-speech-chrome-lang
                current-prefix-arg))
  (my-chrome-ensure-speech-recognition-server)
  (let* ((base-id (file-name-base id))
         (user-dir
          (if (file-exists-p
               (expand-file-name
                id
                (temporary-file-directory)))
              (make-temp-file "chrome-" t)
            ;; small race condition, but this is fine
            (expand-file-name
             id
             (temporary-file-directory))))
         process
         (process-environment
          (append
           (list
            (format
             "PULSE_SOURCE=%s"
             my-speech-input)
            (format
             "PULSE_PROP=node.description='%s' media.name='%s' node.name='%s'"
             base-id base-id base-id))
           process-environment)))
    ;; Hook it up to my-speech-input by default
    (setq process
          (make-process
           :name "chrome"
           :buffer "*chrome*"
           :command (list
                     "google-chrome"
                     "--disable-fre"
                     "--no-default-browser-check"
                     "--no-first-run"
                     (concat "--user-data-dir=" (shell-quote-argument user-dir))
                     (format "http://127.0.0.1:8000/?session=%s&lang=%s&local=%s"
                             (url-hexify-string base-id)
                             lang
                             (if local-only "1" ""))
                     (concat "--class=" (shell-quote-argument base-id)))
           :sentinel
           (lambda (process event)
             ;; Clean up afterwards
             (cond
              ((string-match "finished\\|deleted\\|exited\\|failed\\|core dumped" event)
               (with-current-buffer (process-buffer process)
                 (when my-speech-chrome-user-dir
                   (delete-directory my-speech-chrome-user-dir t))))))))
    (with-current-buffer (process-buffer process)
      (setq-local my-speech-chrome-user-dir user-dir))
    (switch-to-buffer (format "*Captions - %s*" base-id))
    (setq-local my-speech-chrome-session base-id)
    (my-speech-chrome-connect)))
