;;;###autoload
(defun my-subed-set-timestamp-to-mpv-position (&optional rest)
  (interactive)
  (skip-chars-backward "0-9:,.")
  (when (looking-at "\\(\\([0-9]+\\):\\)?\\([0-9]+\\):\\([0-9]+\\)\\.\\([0-9]+\\)")
    (replace-match (save-match-data (subed-msecs-to-timestamp subed-mpv-playback-position)) t t)))

;;;###autoload
(defun my-subed-adjust-timestamp (offset)
  (interactive (list -100))
  (save-excursion
    (skip-chars-backward "0-9:,.")
    (when (looking-at subed-vtt--regexp-timestamp)
      (let ((new-ts (+ (subed-vtt--timestamp-to-msecs (match-string 0)) offset)))
        (replace-match (save-match-data
                         (subed-vtt--msecs-to-timestamp new-ts)))
        (my-waveform-subed-show-after-time)
        new-ts))))

;;;###autoload
(defun my-subed-adjust-timestamp-up (offset)
  (interactive (list 100))
  (subed-mpv-jump (my-subed-adjust-timestamp (- offset))))

;;;###autoload
(defun my-subed-adjust-timestamp-down (offset)
  (interactive (list -100))
  (subed-mpv-jump (my-subed-adjust-timestamp (- offset))))

;;;###autoload
(defun my-subed-copy-timestamp-from-previous ()
  (interactive)
  (let ((ms (save-excursion (subed-backward-subtitle-time-stop) (subed-subtitle-msecs-stop))))
    (subed-set-subtitle-time-start ms)))
;;;###autoload
(defun my-subed-copy-timestamp-to-next ()
  (interactive)
  (let ((ms (subed-subtitle-msecs-stop)))
    (save-excursion
      (subed-forward-subtitle-time-stop) (subed-set-subtitle-time-start ms))))
;;;###autoload
(defun my-subed-copy-timestamp-dwim ()
  (interactive)
  (save-excursion
    (skip-chars-backward "0-9:,.")
    (if (bolp)
        (my-subed-copy-timestamp-from-previous)
      (my-subed-copy-timestamp-to-next))))

;;;###autoload
(defun my-subed-format-second-speaker (speaker-name)
  "Italicize and shift the subtitles for SPEAKER-NAME"
  (interactive "MSpeaker name: ")
  (goto-char (point-min))
  (let ((line-pos "80%")
        (first t))
    (while (re-search-forward
            (format "\n<v %s>\\(.+\\)</v>" (regexp-quote speaker-name))
            nil t)
      (replace-match
       (format " line:%s\n<v %s><i>%s</i></v>"
               line-pos
               speaker-name
               (if first
                   (concat speaker-name ": ")
                 (match-string 1))))
      (setq first nil))))

;;;###autoload
(defun my-subed-interleave-image-links (dir &optional offset-ms)
	(interactive (list (read-file-name "Directory: ")
										 (if current-prefix-arg (read-number "Offset (ms): "))))
	(setq offset-ms (or offset-ms 0))
	(let* ((start-of-recording (my-filename-timestamp (buffer-file-name)))
				 (subtitles (subed-subtitle-list))
				 (end-of-recording
					(time-add start-of-recording
										(seconds-to-time
										 (/
											(elt (car (last subtitles)) 2)
											1000.0))))
				 (files
					(sort
					 (seq-keep
						(lambda (f)
							(let ((time (my-filename-timestamp f)))
								(when (and
											 (not (time-less-p time start-of-recording))
											 (not (time-less-p end-of-recording time)))
									(cons
									 ;; ms
									 (* 1000 (float-time (time-subtract time start-of-recording)))
									 f))))
						(directory-files dir t "20250123.*\\.\\(jpg\\|png\\|svg\\|webm\\)"))
					 :key 'car)))
		;; Now I have the cues and the file timestamps.
		;; Do I want to add the comments to the current file or go streak to breaking it out?
		;; Let's break it out into a different buffer.
		(save-excursion
			(goto-char (point-min))
			(unless (subed-subtitle-msecs-start) (subed-forward-subtitle-time-start))
			(dolist (cue subtitles)
				(when (and files (>= (+ offset-ms (elt cue 1)) (caar files)))
					(let ((link (org-link-make-string (concat "file:" (cdar files)))))
						(setf (elt cue 4)
									(if (elt cue 4)
											(concat (elt cue 4) "\n" link)
										link))
						(subed-set-subtitle-comment (elt cue 4)))
					(pop files))
				(subed-forward-subtitle-time-start)))
		(with-current-buffer (get-buffer-create "*interleaved*")
			(erase-buffer)
			(org-mode)
			(insert (subed-subtitle-list-text subtitles t))
			(goto-char (point-min))
			(switch-to-buffer (current-buffer)))))

;;;###autoload
(defun my-subed-interleave-calculate-offset (filename)
	(interactive "FFile: ")
	(let ((start-of-recording (my-filename-timestamp (buffer-file-name)))
				(file-timestamp (my-filename-timestamp filename)))
		(message "%d"
						 (- (* 1000.0
									 (time-to-seconds (time-subtract file-timestamp start-of-recording)))
								(subed-subtitle-msecs-start)))))


;;;###autoload
(defun my-split-at-words ()
	(interactive)
	(while (not (eobp))
		(recenter)
		(remove-overlays (point-min) (point-max) 'my-split t)
		(save-excursion
			(forward-word 3)
			(dotimes (n 10)
				(let* ((word-start (point))
							 (word-end (progn (skip-syntax-forward "^ ") (point)))
							 (overlay (make-overlay word-start word-end)))
					(overlay-put overlay 'my-split t)
					(overlay-put overlay 'split-num n)
          (overlay-put overlay 'evaporate t)
					(overlay-put overlay 'before-string (propertize (format "%s" n)
																													'face '(:foreground "white"
																																							:background "blue")))

					(skip-syntax-forward " ")
					)))
		(let* ((input (read-char "Split: "))
					 (num (unless (= input 13) 		; enter
									(string-to-number (char-to-string input))))
					 (match (when num (seq-find (lambda (ov)
																				(and (overlay-get ov 'split-num)
																						 (= (overlay-get ov 'split-num) num)))
																			(overlays-in (point-min) (point-max))))))
			(if match
					(progn
						(goto-char (overlay-start match))
						(skip-syntax-backward " ")
						(delete-region (point) (overlay-start match))
						(insert "\n"))
				(forward-word 7)))))
;;;###autoload
(defun my-split-clear-overlays ()
	(interactive)
	(remove-overlays (point-min) (point-max) 'my-split t))

;;;###autoload
(defun my-split-sentence-and-capitalize ()
  (interactive)
  (delete-char 1)
  (insert ".")
  (capitalize-word 1))
;;;###autoload
(defun my-split-sentence-delete-word-and-capitalize ()
  (interactive)
  (delete-char 1)
  (insert ".")
  (kill-word 1)
  (capitalize-word 1))
;;;###autoload
(defun my-delete-word-and-capitalize ()
  (interactive)
  (skip-syntax-backward "w")
  (kill-word 1)
  (capitalize-word 1))

;;;###autoload
(defun my-subed-remove-whisperx-underlines ()
  "Remove underlines from the transcript.
If you called whisperx with --highlight_words, this function can remove the underlines."
	(interactive)
	(let (results)
		(dolist (cue (subed-subtitle-list))
			(let ((text (replace-regexp-in-string "</?u>" "" (elt cue 3))))
				(if (and results (string= text (elt (car results) 3)))
						(setf (elt (car results) 2) (elt cue 2))
					(setf (elt cue 3) text)
					(push cue results))))
		(goto-char (point-min))
		(subed-forward-subtitle-start-pos)
		(delete-region (point) (point-max))
		(subed-append-subtitle-list (reverse results))))

;;;###autoload
(defun my-subed-move-succeeding-subtitles-based-on-mpv ()
  "Move current and succeeding subtitles so that current starts at MPV playing position."
	(interactive)
	(if subed-mpv-playback-position
			(subed-move-subtitles
			 (- subed-mpv-playback-position (subed-subtitle-msecs-start))
			 (point) (point-max))
		(error "Need playback position.")))

;;;###autoload
(defun my-subed-check-random ()
	(interactive)
	(let* ((list (subed-subtitle-list))
				 (pos (random (length list))))
		(subed-jump-to-subtitle-id
		 (subed-msecs-to-timestamp (elt (elt list pos) 1)))
		(subed-mpv-jump-to-current-subtitle)
		(subed-mpv-unpause)))

;;;###autoload
(defun my-subed-get-region-start-stop (beg end)
  (interactive "r")
  (cons (save-excursion
          (goto-char (min beg end))
          (subed-subtitle-msecs-start))
        (save-excursion
          (goto-char (max beg end))
          (subed-subtitle-msecs-stop))))

;;;###autoload
(defun my-extend-file-name (original name &optional extension)
  "Add NAME to the end of ORIGINAL, before the file extension."
  (concat (file-name-sans-extension original) " " name "."
          (or extension (file-name-extension original))))

;;;###autoload
(defun my-adjust-subtitles (offset)
  "Change all of the start and end times by OFFSET."
  (interactive (list (subed--string-to-msecs (read-string "Time: "))))
  (subed-for-each-subtitle (point-min) (point-max) nil
    (subed-adjust-subtitle-time-start offset t t)
    (subed-adjust-subtitle-time-stop offset t t))
  (subed-regenerate-ids))

;;;###autoload
(defun my-subed-write-adjusted-subtitles (source-file start-msecs end-msecs dest-file)
  (let ((s (with-current-buffer (find-file-noselect source-file)
             (buffer-substring-no-properties
              (subed-jump-to-subtitle-id-at-msecs start-msecs)
              (progn (subed-jump-to-subtitle-id-at-msecs end-msecs) (subed-jump-to-subtitle-end)))))
        (offset (- start-msecs)))
    (with-current-buffer (find-file-noselect dest-file)
      (erase-buffer)
      (insert s)
      (my-adjust-subtitles offset)
      (save-buffer)
      (buffer-file-name))))

;;;###autoload
(defun my-msecs-to-timestamp (msecs)
  "Convert MSECS to string in the format HH:MM:SS.MS."
  (concat (format-seconds "%02h:%02m:%02s" (/ msecs 1000))
          "." (format "%03d" (mod msecs 1000))))

;;;###autoload
(defun my-subed-make-animated-gif (beg end name)
  (interactive "r\nMName: ")
  (let* ((video-file (subed-guess-video-file))
         (msecs (my-subed-get-region-start-stop beg end))
         (new-file (my-extend-file-name video-file name "gif"))
         cmd)
    (when (> (length name) 0)
      (setq cmd
            (format "ffmpeg -y -i %s -ss %s -t %s -vf subtitles=%s -r 10 -c:a copy -shortest -async 1 %s"
                    (shell-quote-argument video-file)
                    (my-msecs-to-timestamp (car msecs))
                    (my-msecs-to-timestamp (- (cdr msecs) (car msecs)))
                    (shell-quote-argument (my-subed-write-adjusted-subtitles beg end name))
                    (shell-quote-argument new-file)))
      (message "%s" cmd)
      (kill-new cmd)
      (shell-command cmd))))

;;;###autoload
(defun my-subed-ffmpeg-make-mute-filter (segments)
  (mapconcat
   (lambda (s)
     (format "volume=enable='between(t,%.3f,%.3f)':volume=0"
             (/ (car s) 1000.0)
             (/ (cdr s) 1000.0)))
   segments ", "))

;;;###autoload
(defun my-subed-cut-video (beg end name video-file caption-file &optional kill-only)
  (interactive
   (append
    (if (use-region-p)
        (list (point) (mark))
      (list (save-excursion (subed-jump-to-subtitle-id))
            (save-excursion (subed-jump-to-subtitle-end))))
    (list
     (expand-file-name (read-file-name "New video filename: "))
     (if (derived-mode-p 'subed-mode) (expand-file-name (subed-media-file))
			 (read-file-name "Video: "))
     (if (derived-mode-p 'subed-mode) (expand-file-name (buffer-file-name))
			 (read-file-name "Captions: ")))))
  (let*
      ((msecs (my-subed-get-region-start-stop beg end))
       (new-file name)
       cmd)
    (when (> (length name) 0)
      (setq cmd
            (format "ffmpeg -y -i %s -i %s -ss %s -t %s -shortest -async 1 %s"
                    (shell-quote-argument caption-file)
                    (shell-quote-argument video-file)
                    (my-msecs-to-timestamp
                     (car msecs))
                    (my-msecs-to-timestamp
                     (-
                      (cdr msecs)
                      (car msecs)))
                    (shell-quote-argument new-file)))
      (message "%s" cmd)
      (if kill-only (kill-new cmd)
				(shell-command cmd)))))

(define-minor-mode my-subed-hide-nontext-minor-mode
  "Minor mode for hiding non-text stuff.")
;;;###autoload
(defun my-subed-hide-nontext-overlay (start end)
  (let ((new-overlay (make-overlay start end)))
    (overlay-put new-overlay 'invisible t)
    (overlay-put new-overlay 'intangible t)
    (overlay-put new-overlay 'evaporate t)
    (overlay-put new-overlay 'read-only t)
    (overlay-put new-overlay 'hide-non-text t)
    (with-silent-modifications
      (add-text-properties start end '(read-only t)))
    new-overlay))

;;;###autoload
(defun my-subed-hide-nontext ()
  (interactive)
  (remove-overlays (point-min) (point-max) 'invisible t)
  (when my-subed-hide-nontext-minor-mode
    (save-excursion
      (goto-char (point-min))
      (subed-jump-to-subtitle-id)
      (my-subed-hide-nontext-overlay (point-min) (subed-jump-to-subtitle-text))
      (let (next)
        (while (setq next (save-excursion (subed-forward-subtitle-text)))
          (subed-jump-to-subtitle-end)
          (my-subed-hide-nontext-overlay (1+ (point)) (1- next))
          (subed-forward-subtitle-text))))))

;;;###autoload
(defun my-subed-show-all ()
  (interactive)
  (let ((inhibit-read-only t))
    (with-silent-modifications
      (remove-text-properties (point-min) (point-max) '(read-only t))
      (remove-overlays (point-min) (point-max) 'invisible t))))

;;;###autoload
(defun my-ignore-read-only (f &rest args)
  (let ((inhibit-read-only t))
    (apply f args)
    (my-subed-hide-nontext)))

(advice-add 'subed-split-and-merge-dwim :around #'my-ignore-read-only)
(advice-add 'subed-split-subtitle :around #'my-ignore-read-only)
(advice-add 'subed-merge-with-next :around #'my-ignore-read-only)
(advice-add 'subed-merge-with-previous :around #'my-ignore-read-only)
(advice-add 'subed-regenerate-ids :around #'my-ignore-read-only)
(advice-add 'subed-kill-subtitle :around #'my-ignore-read-only)

;;;###autoload
(defun my-subed-forward-word (&optional arg)
  "Skip timestamps."
  (interactive "^p")
  (setq arg (or arg 1))
  (let ((end (or (save-excursion (subed-jump-to-subtitle-end)) (point))))
    (loop while (> arg 0)
          do
          (forward-word 1)
          (skip-syntax-forward "^\s")
          (setq arg (1- arg))
          (when (> (point) end)
            (subed-jump-to-subtitle-text)
            (forward-word 1)
            (skip-syntax-forward "^\s")
            (setq end (or (save-excursion (subed-jump-to-subtitle-end)) (point)))))))

;;;###autoload
(defun my-subed-backward-word (&optional arg)
  "Skip timestamps."
  (interactive "^p")
  (setq arg (or arg 1))
  (let ((end (or (save-excursion (subed-jump-to-subtitle-text)) (point))))
    (loop while (> arg 0)
          do
          (backward-word 1)
          (setq arg (1- arg))
          (when (< (point) end)
            (subed-backward-subtitle-text)
            (setq end (point))
            (subed-jump-to-subtitle-end)
            (backward-word 1)))))

(defvar my-subed-audio-link-list nil)
;;;###autoload
(defun my-subed-remove-audio-links (beg end)
  (interactive (if (region-active-p)
                   (list (region-beginning)
                         (region-end))
                 (save-excursion
                   (org-back-to-heading)
                   (org-end-of-meta-data t)
                   (list (point)
                         (save-excursion (org-end-of-subtree)
                                         (point))))))
  (save-excursion
    (goto-char beg)
    (while (re-search-forward "vtime:[0-9:]+ +" nil t)
      (replace-match ""))))

;;;###autoload
(defun my-subed-load-audio-links (&optional op)
  (interactive (list
                (cond
                 ((null current-prefix-arg) 'insert)
                 ((equal current-prefix-arg '(4)) 'list)
                 ((equal current-prefix-arg '(16)) 'skip))))
  (cond
   ((derived-mode-p 'subed-mode)
    (setq my-subed-audio-link-list (subed-subtitle-list)))
   ((derived-mode-p 'org-mode)
    (save-excursion
      (unless (eq 'link (org-element-type (org-element-context)))
        (re-search-backward "\\(audio\\|video\\):"))
      (let ((filename
             (car
              (url-path-and-query (url-generic-parse-url (org-element-property :path (org-element-context)))))))
        (setq my-subed-audio-link-list
              (seq-filter
               (lambda (o)
                 (and (elt o 3) (not (string= (elt o 3) ""))))
               (subed-parse-file
                (concat (file-name-sans-extension filename)
                        ".vtt"))))))
    (pcase op
      ('insert (call-interactively #'my-subed-insert-audio-links))
      ('list (my-subed-insert-audio-links-as-list)))
    my-subed-audio-link-list)))

;;;###autoload
(defun my-subed-remove-audio-links (beg end)
  "Remove audio links from region."
  (interactive (cond
                ((region-active-p)
                 (list (region-beginning)
                       (region-end)))
                ((org-in-block-p '("media-post"))
                 (let ((block (org-element-lineage (org-element-context) 'special-block)))
                   (list
                    (org-element-begin block)
                    (org-element-end block))))
                (t
                 (list
                  (point-min) (point-max)))))
  (save-excursion
    (goto-char beg)
    (while (re-search-forward "vtime:[0-9:]+ " end t)
      (replace-match ""))))

;;;###autoload
(defun my-subed-insert-next-audio-link (&optional by-sentence)
  (interactive (list current-prefix-arg))
  (let* ((candidates
          (seq-keep (lambda (o)
                      (unless (string= (string-trim (elt o 3)) "")
                        (cons (replace-regexp-in-string "<.+?>" "" (elt o 3))
                              (car o))))
                    my-subed-audio-link-list))
         (sentence
          (my-org-simplify-text
           (replace-regexp-in-string
            "\\*" ""
            (replace-regexp-in-string
             " *{.+?}" ""
             (let ((sentence (sentence-at-point))
                   (subs (buffer-substring (point) (line-end-position))))
               (if (and sentence (< (length sentence) (length subs)))
                   sentence
                 subs))))))
         (choice
          (or
           (cdr
            (seq-find
             (lambda (o)
               (subed-word-data-compare-normalized-string-distance
                sentence
                (replace-regexp-in-string
                 "\\*" ""
                 (replace-regexp-in-string " *{.+?}" "" (car o)))))
             candidates))
           (consult--read
            candidates
            :lookup 'consult--lookup-cdr
            :sort nil))))
    (save-excursion
      (insert "vtime:" (replace-regexp-in-string "\\.[0-9]+" "" choice) " "))
    (my-org-next-item-or-paragraph by-sentence)
    (setq my-subed-audio-link-list
          (seq-remove
           (lambda (o) (string= (car o) choice))
           my-subed-audio-link-list))))

;;;###autoload
(defun my-subed-insert-audio-links (&optional beg end do-load)
  (interactive (cond
                ((region-active-p)
                 (list (region-beginning)
                       (region-end)
                       current-prefix-arg))
                ((org-in-block-p '("media-post"))
                 (let ((block (org-element-lineage (org-element-context) 'special-block)))
                   (list
                    (org-element-begin block)
                    (org-element-end block)
                    current-prefix-arg)))
                (t
                 (list
                  (point-min) (point-max)
                  current-prefix-arg))))
  (setq beg (or beg (point)))
  (setq end (or end (point-max)))
  (when (or do-load (null my-subed-audio-link-list))
    (save-excursion
      (unless (eq 'link (org-element-type (org-element-context)))
        (re-search-backward "audio:" nil t))
      (let ((elem (org-element-context)))
        (when (and (eq 'link (org-element-type elem))
                   (string= "audio" (org-element-property :type elem)))
          (my-subed-load-audio-links)
          (my-org-next-item-or-paragraph)))))
  (save-restriction
    (narrow-to-region beg end)
    (while (and my-subed-audio-link-list
                (not (eobp)))
      (my-subed-insert-next-audio-link))))

;;;###autoload
(defun my-subed-insert-audio-links-as-list ()
  (interactive)
  (dolist (cue my-subed-audio-link-list)
    (insert "- " (my-org-vtime-link cue) " " (elt cue 3) "\n")))

;;;###autoload
(defun my-subed-fix-timestamps ()
  "Change all ending timestamps to the start of the next subtitle."
  (interactive)
  (goto-char (point-max))
  (let ((timestamp (subed-subtitle-msecs-start)))
    (while (subed-backward-subtitle-time-start)
      (subed-set-subtitle-time-stop timestamp)
      (setq timestamp (subed-subtitle-msecs-start)))))

;;;###autoload
(defun subed-avy-set-up-actions ()
  (interactive)
  (make-local-variable 'avy-dispatch-alist)
  (add-to-list
   'avy-dispatch-alist
   (cons ?, 'subed-split-subtitle)))

;;;###autoload
(defun my-subed-maybe-save-place ()
  (when buffer-file-name (save-place-local-mode 1)))

;;;###autoload
(defun my-subed-word-tsv-from-whisperx-json (file)
	(interactive "FJSON: ")
	(let* ((json-array-type 'list)
				 (json-object-type 'alist)
				 (data (json-read-file file))
				 (filename (concat (file-name-sans-extension file) ".tsv"))
				 (base (seq-mapcat
								(lambda (segment)
									(seq-map (lambda (word)
														 (let-alist word
															 (list nil
																		 (and .start (* 1000 .start))
																		 (and .end (* 1000 .end))
																		 .word)))
													 (alist-get 'words segment)))
								(alist-get 'segments data)))
				 (current base)
				 (last-end 0))
		 ;; numbers at the end of a sentence sometimes don't end up with times
		 ;; so we need to fix them
		(while current
			(unless (elt (car current) 1)						; start
				(setf (elt (car current) 1) (1+ last-end)))
			(unless (elt (car current) 2)
				(setf (elt (car current) 2) (1- (elt (cadr current) 1))))
			(setq
			 last-end (elt (car current) 2)
			 current (cdr current)))
		(subed-create-file
		 filename
		 base
		 t
		 'subed-tsv-mode)
		(find-file filename)))

;;;###autoload
(defun my-subed-load-word-data-from-whisperx-highlights (file)
	"Return a list of word cues from FILE.
FILE should be a VTT or SRT file produced by whisperx with the
--highlight_words True option."
	(seq-keep (lambda (sub)
							(when (string-match "<u>\\(.+?\\)</u>" (elt sub 3))
								(setf (elt sub 3) (match-string 1 (elt sub 3)))
								sub))
						(subed-parse-file file)))

;;;###autoload
(defun my-subed-word-tsv-from-whisperx-highlights (file)
	(interactive "FVTT: ")
	(with-current-buffer (find-file-noselect (concat (file-name-nondirectory file) ".tsv"))
		(erase-buffer)
		(subed-tsv-mode)
		(subed-auto-insert)
    (mapc (lambda (sub) (apply #'subed-append-subtitle nil (cdr sub)))
					(my-subed-load-word-data-from-whisperx-highlights file))
		(switch-to-buffer (current-buffer))))

(defvar my-subed-merge-close-subtitles-threshold 500)
;;;###autoload
(defun my-subed-merge-close-subtitles (threshold)
	"Merge subtitles with the following one if there is less than THRESHOLD msecs gap between them."
	(interactive (list (read-number "Threshold in msecs: " my-subed-merge-close-subtitles-threshold)))
	(goto-char (point-min))
	(while (not (eobp))
		(let ((end (subed-subtitle-msecs-stop))
					(next-start (save-excursion
												(and (subed-forward-subtitle-time-start)
														 (subed-subtitle-msecs-stop)))))
			(if (and end next-start (< (- next-start end) threshold))
					(subed-merge-with-next)
				(or (subed-forward-subtitle-end) (goto-char (point-max)))))))

(defvar my-subed-skim-msecs 1000 "Number of milliseconds to play when skimming.")
;;;###autoload
(defun my-subed-skim-starts ()
	(interactive)
	(subed-mpv-unpause)
	(subed-disable-loop-over-current-subtitle)
	(catch 'done
		(while (not (eobp))
			(subed-mpv-jump-to-current-subtitle)
			(let ((ch
						 (read-char "(q)uit? " nil (/ my-subed-skim-msecs 1000.0))))
				(when ch
					(throw 'done t)))
			(subed-forward-subtitle-text)
			(when (and subed-waveform-minor-mode
								 (not subed-waveform-show-all))
				(subed-waveform-refresh))
			(recenter)))
	(subed-mpv-pause))

;; Use the saved version of this instead of forcing the reevaluation
(defcustom my-subed-common-edits
	'("I"
    "I've"
    "I'm"
    "Mendeley"
    "JavaScript"
    "RSS"
		("stop section" "subsection")
		("EmacsConf" "EmacsCon" "emacs conf" "imaxconf")
    ("going to" "gonna")
    ("want to" "wanna")
    ("transient" "transit")
    ("C-c" "control c" "Ctrl+C")
    ("C-x" "control x" "Ctrl+X")
    ("C-f" "control f")
    ("" "uh" "um")
    ("Magit" "maggot")
    ("Emacs" "e-max" "emex" "emax" "bmx" "imax")
    ("Emacs News" "emacs news")
		("Emacs Lisp" "emacs list")
    ("ivy" "iv")
    ("UI" "ui")
    ("TECO" "tico")
    ("org-roam" "orgrim" "orgrom" "Org Rome")
    ("non-nil" "non-nail")
    ("commits" "comets")
    "SQL"
    "arXiv"
    "Montessori"
    "SVG"
    "YouTube" "GitHub" "GitLab" "OmegaT" "Linux" "SourceForge"
    "LaTeX"
    "Lisp"
    "Org"
    "IRC"
    "Reddit"
    "PowerPoint"
    "SQLite"
    "SQL"
    "I'll"
    ("<f9>" "F-9" "f9")
    "I'd"
    "PDFs"
    "PDF"
    "ASCII"
    ("Spacemacs" "spacemax")
    "Elisp"
		"Reddit"
		"TextMate"
		"macOS"
		"API"
		"IntelliSense"
    ("EXWM" "axwm")
    ("Emacs's" "emax's")
    ("BIDI" "bd")
    ("Perso-Arabic" "personal arabic")
    "Persian"
    "URL"
    "HTML"
		("vdo.ninja" "Video Ninja"))
	"Commonly-misrecognized words or words that need special capitalization."
	:group 'sachac
	:type '(repeat (choice string
												 (repeat string))))

;;;###autoload
(defun my-subed-add-common-edit (beg end replacement)
	"Add this word to the misrecognized words."
	(interactive
	 (let ((beg (if (region-active-p) (min (point) (mark))
								(skip-syntax-backward "w")
								(point)))
				 (end (if (region-active-p) (max (point) (mark))
								(save-excursion (forward-word 1) (point)))))
		 (list beg end
					 (completing-read
						(format "Replacement (%s): " (buffer-substring beg end))
						(mapcar (lambda (o) (if (stringp o) o (car o))) my-subed-common-edits)))))
	(customize-set-variable
	 'my-subed-common-edits
	 (cond
		((member replacement my-subed-common-edits)
		 (cons (list replacement (buffer-substring-no-properties beg end))
					 (delete replacement my-subed-common-edits)))
		((assoc replacement my-subed-common-edits)
		 (setcdr (assoc replacement my-subed-common-edits)
						 (append (list replacement) (cdr (assoc replacement my-subed-common-edits))))
		 my-subed-common-edits)
		(t
		 (push (list replacement (buffer-substring-no-properties beg end))
					 my-subed-common-edits))))
	(delete-region beg end)
	(insert replacement))

;;;###autoload
(defun my-subed-find-next-fix-point ()
  (when (re-search-forward
         (format "\\<%s\\>"
                 (downcase
                  (regexp-opt (seq-mapcat
                               (lambda (o)
                                 (if (listp o)
                                     (if (string= (car o) "") (cdr o) o)
                                   (list o)))
                               my-subed-common-edits))))
         nil t)
    (goto-char (match-beginning 0))
    (seq-find (lambda (o)
                (if (listp o)
                    (seq-find (lambda (s) (string= (downcase s) (downcase (match-string 0)))) o)
                  (string= (downcase o) (downcase (match-string 0)))))
              my-subed-common-edits)))

;;;###autoload
(defun my-subed-fix-common-error ()
  (interactive)
  (let ((entry (my-subed-find-next-fix-point)))
    (replace-match (if (listp entry) (car entry) entry) t t)))

;;;###autoload
(defun my-subed-fix-common-errors ()
  (interactive)
  (let (done entry correction)
    (while (and
            (not done)
            (setq entry (my-subed-find-next-fix-point)))
      (setq correction (if (listp entry) (car entry) entry))
			(if (called-interactively-p 'any)
					(let* ((c (read-char (format "%s (yn.): " correction))))
						(cond
						 ((= c ?y) (replace-match correction t t))
						 ((= c ?n) (goto-char (match-end 0)))
						 ((= c ?j) (subed-mpv-jump-to-current-subtitle))
						 ((= c ?.) (setq done t))))
				(replace-match correction t t)))))

;;;###autoload
(defun my-subed-fix-common-errors-from-start ()
  (goto-char (point-min))
  (my-subed-fix-common-errors))

;;;###autoload
(defun my-subed-group-sections (subtitles)
	"Return a list of ((:comment ... :start-ms ... :stop-ms ... :subtitles ...) ...)."
	(reverse
	 (seq-reduce (lambda (prev val)
								 (if (elt val 4)
										 (cons
											(list :comment (elt val 4)
														:start-ms (elt val 1)
														:stop-ms (elt val 2)
														:subtitles (list val))
											prev)
									 (when (> (elt val 2) (plist-get (car prev) :stop-ms))
										 (setcar prev (plist-put (car prev) :stop-ms (elt val 2))))
									 (setcar
										prev
										(plist-put (car prev) :subtitles (nconc (plist-get (car prev) :subtitles)
																														(list val))))
									 prev))
							 (cdr subtitles)
							 (list
								(list :comment (elt (car subtitles) 4)
											:start-ms (elt (car subtitles) 1)
											:stop-ms (elt (car subtitles) 2)
											:subtitles (list (car subtitles)))))))

(ert-deftest my-subed-group-sections ()
 (should
	(equal (my-subed-group-sections '((nil 0 99 "Test" "Intro")
																		(nil 100 199 "A")
																		(nil 200 299 "B" "Conclusion")
																		(nil 300 399 "C")
																		(nil 400 499 "D")))
				 '((:comment "Intro" :start-ms 0 :stop-ms 199
										 :subtitles
										 ((nil 0 99 "Test" "Intro")
											(nil 100 199 "A")))
					 (:comment "Conclusion" :start-ms 200 :stop-ms 499
										 :subtitles
										 ((nil 200 299 "B" "Conclusion")
											(nil 300 399 "C") (nil 400 499 "D")))))))

;;;###autoload
(defun my-subed-mark-section ()
	"Return the start and end of the current section.
The current section is defined by NOTE comments."
	(interactive)
	(let* ((start
					(save-excursion
						(if (subed-subtitle-comment)
								(progn (subed-jump-to-subtitle-comment) (point))
							;; keep going backwards
							(while (and (not (bobp))
													(if (subed-backward-subtitle-start-pos)
															(not (subed-subtitle-comment))
														(goto-char (point-min)))))
							(subed-jump-to-subtitle-comment)
							(point))))
				 (end
					(save-excursion
						;; keep going backwards
						(while (and (not (eobp))
												(if (subed-forward-subtitle-start-pos)
														(not (subed-jump-to-subtitle-comment))
													(goto-char (point-max)))))
						(subed-jump-to-subtitle-comment))))
		(when (and start end)
			(push-mark start)
			(goto-char end)
			(activate-mark))))

;;;###autoload
(defun my-subed-delete-oops (&optional skip-only)
	(interactive (list current-prefix-arg))
	(atomic-change-group
		(subed-for-each-subtitle (point-min) (point-max) t
			(when (string-match "\\boops\\b" (subed-subtitle-text))
				(if skip-only
						(subed-set-subtitle-comment "#+SKIP")
					(subed-kill-subtitle))))))

(ert-deftest my-subed-delete-oops ()
	(let ((test '((nil 0 99 "Hello")
								(nil 100 199 "Hello oops")
								(nil 200 299 "Hello world")
								(nil 299 300 "Hello again oops"))))
		(should
		 (equal
			(with-temp-buffer
				(subed-vtt-mode)
				(subed-append-subtitle-list test)
				(my-subed-delete-oops)
				(subed-subtitle-list-text (subed-subtitle-list) t))
			"Hello\nHello world\n"))
		(should
		 (equal
			(with-temp-buffer
				(subed-vtt-mode)
				(subed-append-subtitle-list test)
				(my-subed-delete-oops t)
				(subed-subtitle-list-text (subed-subtitle-list) t))
			"Hello\n\n#+SKIP\n\nHello oops\nHello world\n\n#+SKIP\n\nHello again oops\n"))))

;;;###autoload
(defun my-subed-skip-oops ()
	(interactive)
	(my-subed-delete-oops t))

;;;###autoload
(defun my-subed-record-wpm ()
	(interactive)
	(let ((wpm (subed-wpm
							(seq-remove (lambda (o) (and (elt o 4) (string-match "skip" (elt o 4))))
													(subed-subtitle-list)))))
		(apply 'message
					  "%d wpm (%d words / %.1f minutes)" wpm)))

;;;###autoload
(defun my-subed-prepare-for-cleaning ()
	(interactive)
	(my-subed-delete-oops)
	(goto-char (point-min))
	(subed-forward-subtitle-id)
	(subed-set-subtitle-comment (concat "#+OUTPUT: " (file-name-sans-extension (buffer-file-name)) "-cleaned.opus")))

(defvar my-phone-recording-dir "~/sync/Phone")
;;;###autoload
(defun my-subed-copy-recording (filename destination)
	(interactive
	 (list
		(buffer-file-name)
		(file-name-directory
		 (read-file-name (format "Copy %s to: "
														 (file-name-base (buffer-file-name)))
										 nil nil nil nil #'file-directory-p))))
	(dolist (ext '("m4a" "txt" "json" "vtt"))
		(when (file-exists-p (concat (file-name-sans-extension filename) "." ext))
			(copy-file (concat (file-name-sans-extension filename) "." ext)
								 destination t)))
	(when (get-file-buffer filename)
		(kill-buffer (get-file-buffer filename))
		(dired destination)))

;;;###autoload
(defun my-subed-copy-latest-phone-recording (destination)
	"Copy the latest recording transcript and audio to DESTINATION."
	(interactive
	 (list
		(file-name-directory
		 (read-file-name (format "Move %s to: "
														 (file-name-base (my-latest-file my-phone-recording-dir ".txt")))
										 nil nil nil nil #'file-directory-p))))
	(let ((base (file-name-base (my-latest-file my-phone-recording-dir ".txt"))))
		(rename-file (expand-file-name (concat base ".txt") my-phone-recording-dir)
								 destination)
		(rename-file (expand-file-name (concat base ".m4a") my-phone-recording-dir)
								 destination)
		(find-file (expand-file-name (concat base ".txt") destination))
		(save-excursion (my-split-oops))
		(goto-char (point-min))
		(flush-lines "^$")
		(goto-char (point-min))
		(subed-forward-subtitle-id)
		(subed-set-subtitle-comment
		 (concat "#+OUTPUT: "
						 (file-name-base (buffer-file-name))
						 "-cleaned.opus"))))




(ert-deftest my-subed-org-format-by-speaker ()
  "Tests `my-subed-org-format-by-speaker'."
  (should
   (string=
    (my-subed-org-format-by-speaker
     '((nil 0 10 "<v Sacha>This is a test</v>")
       (nil 20 30 "<v Sacha>This is another</v>")
       (nil 40 50 "<v Guest>Next sentence</v>")))
    "Sacha: This is a test\nThis is another\n\nGuest: Next sentence")))


(defun my-subed-org-format-by-speaker (subtitles)
  "Return a string of

Speaker: text ...

Speaker: text ...
"
  (let (last-speaker)
    (replace-regexp-in-string
     "</?i>" ""
     (string-trim
      (mapconcat (lambda (sub)
                   (let ((text (elt sub 3)))
                     (if (string-match "<v \\([^>]+\\)>\\(.+\\)</v>" text)
                         (if (not (string= last-speaker (match-string 1 text)))
                             (progn
                               (setq last-speaker (match-string 1 text))
                               (format "\n%s: %s" last-speaker
                                       (replace-regexp-in-string
                                        (save-match-data (concat "^" (regexp-quote last-speaker) ": ")) ""
                                        (match-string 2 text))))
                           (match-string 2 text))
                       text)))
                 subtitles
                 "\n")))))


;;;###autoload
(cl-defun my-subed-as-org-list-with-times (file &key from to)
	(interactive "FVTT: ")
  (when (stringp from) (setq from (compile-media-timestamp-to-msecs from)))
  (when (stringp to) (setq to (compile-media-timestamp-to-msecs to)))
	(let ((s (mapconcat
						(lambda (o)
              (let ((text (org-ascii--indent-string
                           (my-subed-org-format-by-speaker
                            (plist-get o :subtitles))
                           2)))
							  (format "- @@html:<span class=\"audio-time\" data-start=\"%.3f\" data-stop=\"%.3f\">%s</span>@@: *%s*:\n%s\n\n"
  										  (/ (plist-get o :start-ms) 1000.0)
  										  (/ (plist-get o :stop-ms) 1000.0)
  										  (replace-regexp-in-string "^00:0?\\|\\.[0-9]+$" "" (my-msecs-to-timestamp (plist-get o :start-ms)))
  										  (plist-get o :comment)
                        text)))
						(my-subed-group-sections
						 (seq-filter (lambda (sub)
  												 (and (or (not from) (>= (elt sub 1) from))
  															(or (not to) (< (elt sub 2) to))))
  											 (subed-parse-file file)))
						"")))
		(if (called-interactively-p 'any)
				(insert s)
			s)))

;;;###autoload
(defun my-subed-remove-gaps (&optional threshold)
	"Remove gaps between cues below threshold.
If threshold is 0, remove all gaps."
	(interactive "NThreshold: ")
	(goto-char (point-min))
	(unless (subed-jump-to-subtitle-time-start)
		(subed-forward-subtitle-time-start))
	(subed-set-subtitle-time-start 0)
	(let (last-start)
		(subed-for-each-subtitle (point) (point-max) t
			(if (and last-start (< (- last-start (subed-subtitle-msecs-stop)) threshold))
					(subed-set-subtitle-time-stop (1- last-start)))
			(setq last-start (subed-subtitle-msecs-start)))))
;;;###autoload
(defun my-subed-merge-to-min-length (threshold)
	"Merge cues until the duration is at least THRESHOLD."
	(interactive "NThreshold (msecs): ")
	(goto-char (point-min))
	(while (not (eobp))
		(subed-jump-to-subtitle-text)
		(while (not (eobp))
			(let ((duration (- (subed-subtitle-msecs-stop)
												 (subed-subtitle-msecs-start)))
						(next-duration (save-excursion
														 (when (subed-forward-subtitle-start-pos)
															 (- (subed-subtitle-msecs-stop)
																	(subed-subtitle-msecs-start))))))
				(while (and next-duration (< (+ duration next-duration) threshold))
					(subed-merge-with-next)
					(setq duration (- (subed-subtitle-msecs-stop)
														(subed-subtitle-msecs-start))
								next-duration (save-excursion
																(when (subed-forward-subtitle-start-pos)
																	(- (subed-subtitle-msecs-stop)
																		 (subed-subtitle-msecs-start)))))))
			(unless (subed-forward-subtitle-start-pos)
				(goto-char (point-max))))))


;;;###autoload
(defun my-subed-subtitle-set-text (text)
  (interactive "MNew text: ")
  (subed-jump-to-subtitle-text)
  (delete-region (point) (or (subed-jump-to-subtitle-end) (point)))
  (insert text))

;;;###autoload
(defun my-plover/edit-find-target (input)
  (or (looking-at (concat "\\b" (regexp-quote input) "\\b"))
      (re-search-forward (concat "\\b" (regexp-quote input) "\\b")
                         nil t)))
;;;###autoload
(defun my-plover/edit-subtitles ()
  (interactive)
  (catch 'exit
    (while t
      (my-read-command-string
       "Command: "
       '(("toggle" subed-mpv-toggle-pause)
         ("jump" (lambda () (interactive) (subed-mpv-jump-to-current-subtitle)))
         ("split [text before split]" subed-split-subtitle)
         ("center" recenter-top-bottom)
         (" previous" (lambda () (interactive) (subed-merge-with-previous) (fill-paragraph)))
         ("merge next" (lambda () (interactive) (subed-merge-with-next) (fill-paragraph)))
         ("slow" (lambda () (interactive) (subed-mpv-playback-speed 0.5)))
         ("fast" (lambda () (interactive) (subed-mpv-playback-speed 2)))
         ("scroll" scroll-up-command)
         ("fill" fill-paragraph)
         ("next [text]" search-forward)
         ("replace <text>")
         ("previous [text]" search-backward)
         ("cap [text]" capitalize-word)
         ("delete [text]" kill-word)
         (", [text]" (lambda () (interactive) (insert ",")))
         ("end [text] - adds period and capitalizes next word" (lambda () (interactive) (insert ".") (capitalize-word 1)))
         ("oops" 'undo)
         ("exit" (throw 'exit nil)))
       (lambda (input)
         (cond
          ((string-match "^split \\(.+\\) *$" input)
           (when (my-plover/edit-find-target (match-string 1 input))
             (goto-char (match-end 0))
             (subed-split-subtitle)
             (fill-paragraph)))
          ((string-match "^delete \\(.+\\) *$" input)
           (when (my-plover/edit-find-target (match-string 1 input))
             (replace-match "")))
          ((string-match "^, \\(.+\\) *$" input)
           (when (my-plover/edit-find-target (match-string 1 input))
             (goto-char (match-end 0))
             (insert ",")))
          ((string-match "^end \\(.+\\) *$" input)
           (when (my-plover/edit-find-target (match-string 1 input))
             (goto-char (match-end 0))
             (insert ".")
             (unless (save-excursion (subed-jump-to-subtitle-end))
               (subed-forward-subtitle-text))
             (capitalize-word 1)))
          ((string-match "^zap \\(.+\\)$" input)
           (delete-region (point)
                          (my-plover/edit-find-target (match-string 1 input))))
          ((string-match "^replace \\(.+\\)$" input)
           (kill-word 1)
           (insert (match-string 1 input)))
          ((string-match "^cap \\(.+\\) *$" input)
           (when (my-plover/edit-find-target (match-string 1 input))
             (replace-match (capitalize (match-string 0)) t t)))
          ((string-match "^... \\(.+\\) *$" input)
           (when (my-plover/edit-find-target (match-string 1 input))
             (insert "...")))
          ((string-match "^next \\(.+\\) *$" input)
           (my-plover/edit-find-target (match-string 1 input)))
          ((string-match "^previous \\(.+\\) *$" input)
           (re-search-backward (concat "\\b" (regexp-quote (match-string 1 input)) "\\b") nil t)
           (goto-char (match-end 0)))
          (t (re-search-forward (concat "\\b" (regexp-quote input) "\\b")))
          ;; (t (my-subed-subtitle-set-text input))
          ))
       nil))))
