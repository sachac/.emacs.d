(defvar my-live-speech-buffer "*Speech*")
(defvar my-live-speech-process nil)
(defvar my-live-speech-output-buffer "*Speech JSON*")

(defvar my-live-speech-functions
	'(my-live-speech-display-in-speech-buffer
		my-live-speech-display-wpm
		my-live-speech-append-to-etherpad)
	"Functions to call with one argument, the recognition results.")

;;;###autoload
(defun my-live-speech-start ()
	"Turn on live captions."
	(interactive)
	(with-current-buffer (get-buffer-create my-live-speech-buffer)
		(unless (process-live-p my-live-speech-process)
			(let ((default-directory "~/proj/deepgram-live"))
				(message "%s" default-directory)
				(with-current-buffer (get-buffer-create my-live-speech-output-buffer)
					(erase-buffer))
				(setq my-live-speech-recent-words nil
							my-live-speech-wpm-string "READY ")
				(setq my-deepgram-process
							(make-process
							 :command '("bash" "run.sh")
							 :name "speech"
							 :filter 'my-live-speech-json-filter
							 :sentinel #'my-live-speech-process-sentinel
							 :buffer my-live-speech-output-buffer)))
			(org-mode))
    (display-buffer (current-buffer))))

;;;###autoload
(defun my-live-speech-stop ()
	(interactive)
	(if (process-live-p my-live-speech-process)
			(kill-process my-live-speech-process))
	(setq my-live-speech-wpm-string nil))

;; (define-minor-mode my-live-speech-mode
;; 	"Show live speech and display WPM.
;; Need to check how to reliably turn this on and off."
;; 	:global t :group 'sachac
;; 	(if my-live-speech-mode
;; 			(my-live-speech-start)
;; 		(my-live-speech-stop)
;; 		(setq my-live-speech-wpm-string nil)))

;; based on subed-mpv::client-filter
;;;###autoload
(defun my-live-speech-handle-json (line-object)
	"Process the JSON object in LINE."
	(run-hook-with-args 'my-live-speech-functions (json-parse-string line :object-type 'alist)))

;;;###autoload
(defun my-live-speech-process-sentinel (proc event)
	(when (string-match "finished" event)
		(my-live-speech-stop)
		;(my-live-speech-mode -1)
		))

;;;###autoload
(defun my-live-speech-json-filter (proc string)
	(when (buffer-live-p (process-buffer proc))
		(with-current-buffer (process-buffer proc)
			(let* ((proc-mark (process-mark proc))
						 (moving (= (point) proc-mark)))
				;;  insert the output
				(save-excursion
					(goto-char proc-mark)
					(insert string)
					(set-marker proc-mark (point)))
				(if moving (goto-char proc-mark))
				;; process and remove all complete lines of JSON (lines are complete if ending with \n)
				(let ((pos (point-min)))
					(while (progn (goto-char pos)
												(end-of-line)
												(equal (following-char) ?\n))
						(let* ((end (point))
									 (line (buffer-substring pos end)))
							(delete-region pos (+ end 1))
							(with-current-buffer (get-buffer my-live-speech-buffer)
								(my-live-speech-handle-json line)))))))))

;;;###autoload
(defun my-live-speech-display-in-speech-buffer (recognition-results)
	(with-current-buffer (get-buffer-create my-live-speech-buffer)
		(let-alist recognition-results
			(let* ((pos (point))
						 (at-end (eobp)))
				(goto-char (point-max))
				(unless (eolp) (insert "\n"))
				(when .msg
					(insert .msg "\n"))
				(when .transcript
					(insert .transcript "\n"))
				;; scroll to the bottom if being displayed
				(if at-end
						(when (get-buffer-window (current-buffer))
							(set-window-point (get-buffer-window (current-buffer)) (point)))
					(goto-char pos))))))

;;;###autoload
(defun my-live-speech-toggle-heading ()
	"Toggle a line as a heading."
	(interactive)
	(with-current-buffer (get-buffer my-live-speech-buffer)
		(display-buffer (current-buffer))
		(with-selected-window (get-buffer-window (get-buffer my-live-speech-buffer))
			(let ((avy-all-windows nil))
				(avy-goto-line 1))
			(org-toggle-heading 1))))
;;;###autoload
(defun my-live-speech-cycle-visibility ()
	"Get a quick overview."
	(interactive)
	(with-current-buffer (get-buffer my-live-speech-buffer)
		(display-buffer (current-buffer))
		(if (eq org-cycle-global-status 'contents)
				(progn
					(run-hook-with-args 'org-cycle-pre-hook 'all)
					(org-fold-show-all '(headings blocks))
					(setq org-cycle-global-status 'all)
					(run-hook-with-args 'org-cycle-hook 'all))
			(run-hook-with-args 'org-cycle-pre-hook 'contents)
			(org-cycle-content)
			(setq org-cycle-global-status 'contents)
			(run-hook-with-args 'org-cycle-hook 'contents))))

(defvar my-live-speech-wpm-window-seconds 15 "How many seconds to calculate WPM for.")
(defvar my-live-speech-recent-words nil "Words spoken in `my-live-speech-wpm-window-minutes'.")
(defvar my-live-speech-wpm nil "Current WPM.")
(defvar my-live-speech-wpm-colors  ; haven't figured out how to make these work yet
	'((180 :foreground "red")
		(170 :foreground "yellow")
		(160 :foreground "green")))
(defvar my-live-speech-wpm-string nil "Add this somewhere in `mode-line-format'.")
;;;###autoload
(defun my-live-speech-wpm-string ()
	(propertize
	 (format "%d WPM " my-live-speech-wpm)
	 'face
	 (cdr (seq-find (lambda (row) (> my-live-speech-wpm (car row))) my-live-speech-wpm-colors))))

;;;###autoload
(defun my-live-speech-display-wpm (recognition-results)
	(let-alist recognition-results
		(when .words
			;; calculate WPM
			(setq my-live-speech-recent-words
						(append my-live-speech-recent-words .words nil))
			(let ((threshold (- (assoc-default 'end (aref .words (1- (length .words))))
													my-live-speech-wpm-window-seconds)))
				(setq my-live-speech-recent-words
							(seq-filter
							 (lambda (o)
								 (>= (assoc-default 'start o)
										 threshold))
							 my-live-speech-recent-words))
				(setq my-live-speech-wpm
							(/
							 (length my-live-speech-recent-words)
							 (/ (- (assoc-default 'end (aref .words (1- (length .words))))
										 (assoc-default 'start (car my-live-speech-recent-words)))
									60.0)))
				(setq my-live-speech-wpm-string (my-live-speech-wpm-string))))))

(defvar my-live-speech-etherpad-id nil)
;;;###autoload
(defun my-live-speech-append-to-etherpad (recognition-results)
	(when my-live-speech-etherpad-id
		(emacsconf-pad-append-text my-live-speech-etherpad-id (concat " " (assoc-default 'transcript recognition-results)))))
