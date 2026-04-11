;;; sacha-speech-chrome.el ---  -*- lexical-binding: t -*-

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
;; Related Emacs config sections:
;;
;; - Streaming speech recognition into Emacs using Google Chrome Web Speech API
;;   https://sachachua.com/dotemacs#writing-and-editing-speech-recognition-streaming-speech-recognition-into-emacs-using-google-chrome-web-speech-api
;;
;;; Code:



;; [[file:../Sacha.org::#writing-and-editing-speech-recognition-streaming-speech-recognition-into-emacs-using-google-chrome-web-speech-api][Streaming speech recognition into Emacs using Google Chrome Web Speech API:1]]
(defvar sacha-speech-chrome-ws nil "Websocket for connecting.")
(defvar sacha-speech-chrome-url "ws://127.0.0.1:8000/ws" "Websocket URL to connect to for captions.")
;;;###autoload
(defun sacha-speech-chrome-connect ()
  (interactive)
  (unless (websocket-p sacha-speech-chrome-ws)
    (setq sacha-speech-chrome-ws
          (websocket-open
           sacha-speech-chrome-url
           :on-message #'sacha-speech-chrome-handle))))
;;;###autoload
(defun sacha-speech-chrome-reconnect ()
  (interactive)
  (sacha-speech-chrome-disconnect)
  (sacha-speech-chrome-connect))

(defvar sacha-speech-functions '(sacha-speech-display)
  "Functions to run with the info as an argument.
The info is an alist with 'type and 'content.
The functions are called in sequence, with the first function getting the info
from the websocket message and the other functions getting the results of the
previous functions.")

(defvar-local sacha-speech-previous-final nil)

(defface sacha-chrome-caption-current
  '((t :height 200))
  "Display current caption.")

;;;###autoload
(defun sacha-speech-display (info)
  (let-alist info
    (with-current-buffer (get-buffer-create
                          (format "*Captions - %s*"
                                  .session))
      (when (and (string= .type "TEMP")
                 sacha-speech-previous-final)
        (goto-char (point-max))
        (delete-region
         (line-beginning-position)
         (line-end-position))
        (insert (propertize sacha-speech-previous-final
                            'face `(:foreground ,(modus-themes-get-color-value 'fg-dim)))
                "\n")
        (setq sacha-speech-previous-final nil))
      (when (string= .type "TEMP")
        (goto-char (point-max))
        (delete-region (line-beginning-position) (line-end-position))
        (insert .type (propertize .content 'face 'sacha-chrome-caption-current)))
      (when (string= .type "FINAL")
        (unless (string= sacha-speech-previous-final .content)
          (goto-char (point-max))
          (set-text-properties
           (line-beginning-position)
           (line-end-position)
           (list
            'face `(:foreground ,(modus-themes-get-color-value 'fg-dim))))
          (insert "\n"))
        (setq sacha-speech-previous-final .content)
        (goto-char (point-max))
        (delete-region (line-beginning-position) (line-end-position))
        (insert (propertize .content 'face 'sacha-chrome-caption-current)))))
  info)
;; Streaming speech recognition into Emacs using Google Chrome Web Speech API:1 ends here

;; [[file:../Sacha.org::#writing-and-editing-speech-recognition-streaming-speech-recognition-into-emacs-using-google-chrome-web-speech-api][Streaming speech recognition into Emacs using Google Chrome Web Speech API:2]]
;;;###autoload
(defun sacha-speech-chrome-handle (_ frame)
  (let* ((info (json-parse-string (websocket-frame-text frame)
                                  :object-type 'alist)))
    (seq-reduce (lambda (prev cur)
                  (funcall cur prev))
                sacha-speech-functions info)))

;;;###autoload
(defun sacha-speech-chrome-disconnect ()
  (interactive)
  (websocket-close sacha-speech-chrome-ws)
  (setq sacha-speech-chrome-ws nil))

(defvar sacha-speech-chrome-recognition-server-process nil)
(defvar sacha-speech-chrome-dir "~/proj/emacs-web-speech")
;;;###autoload
(defun sacha-chrome-ensure-speech-recognition-server ()
  (interactive)
  (unless (process-live-p sacha-speech-chrome-recognition-server-process)
    (let ((default-directory sacha-speech-chrome-dir))
      (setq sacha-speech-chrome-recognition-server-process
            (make-process
             :name "live-captioning"
             :buffer "*live-captioning*"
             :command (list (expand-file-name ".venv/bin/python3") "app.py")))
      (sit-for 1))))

;;;###autoload
(defun sacha-chrome-stop-speech-recognition-server ()
  (interactive)
  (when (process-live-p sacha-speech-chrome-recognition-server-process)
    (kill-process sacha-speech-chrome-recognition-server-process)))

(defvar-local sacha-speech-chrome-session nil)
(defvar-local sacha-speech-chrome-user-dir nil)
(defvar-local sacha-speech-chrome-lang "en-US")
;;;###autoload
(defun sacha-speech-chrome-new-session (&optional id lang local-only)
  (interactive (list
                (file-name-base
                 (make-temp-name
                  (expand-file-name "chrome-"
                                    (temporary-file-directory))))
                sacha-speech-chrome-lang
                current-prefix-arg))
  (sacha-chrome-ensure-speech-recognition-server)
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
             sacha-speech-input)
            (format
             "PULSE_PROP=node.description='%s' media.name='%s' node.name='%s'"
             base-id base-id base-id))
           process-environment)))
    ;; Hook it up to sacha-speech-input by default
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
                 (when sacha-speech-chrome-user-dir
                   (delete-directory sacha-speech-chrome-user-dir t))))))))
    (with-current-buffer (process-buffer process)
      (setq-local sacha-speech-chrome-user-dir user-dir))
    (switch-to-buffer (format "*Captions - %s*" base-id))
    (setq-local sacha-speech-chrome-session base-id)
    (sacha-speech-chrome-connect)))
;; Streaming speech recognition into Emacs using Google Chrome Web Speech API:2 ends here

(provide 'sacha-speech-chrome)
;;; sacha-speech-chrome.el ends here
