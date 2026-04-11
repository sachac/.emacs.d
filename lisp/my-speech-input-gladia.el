;;;###autoload
(defun my-gladia-parse (json-file)
	"Convert JSON-FILE into a list of subtitles."
	(let* ((json-object-type 'alist)
				 (json (json-read-file json-file))
				 (words
					(seq-mapcat (lambda (pred) (seq-map (lambda (w)
																								(append
																								 (list
																									(cons 'speaker (when (not (string= "speaker_not_activated" (assoc-default 'speaker pred)))
																																	 (assoc-default 'speaker pred)))
																									(cons 'start (assoc-default 'time_begin pred))
																									(cons 'end (assoc-default 'time_end pred))
																									(cons 'punctuated_word (string-trim (assoc-default 'word w))))
																								 w))
																							(assoc-default 'words pred)))
											(assoc-default 'prediction json)))
				 (halfway-length (/ my-deepgram-length-threshold 2))
				 subtitles
				 current
				 current-length
				 last-speaker
				 last-text
				 current-text)
		(dolist (speaker (seq-group-by (lambda (o) (assoc-default 'speaker o)) words))
			(setq current-length 0 current nil)
			(dolist (word (cdr speaker))
				(let-alist word
					;; determine whether we are adding to the existing one.
					;; start a new one if length > length-threshold
					;; or time > time-threshold
					(when (or (> (+ (length .punctuated_word)
													current-length)
											 my-deepgram-length-threshold)
										(and (car current)
												 (> .start (+ (assoc-default 'start (car current))
																			my-deepgram-time-threshold))))
						;; copy the previous subtitle
						(push current subtitles)
						(setq current nil current-length 0))
					(push word current)
					(setq current-length (+ (length .punctuated_word) current-length 1))
					(when (and (string-match "[,\\.?]" .punctuated_word)
										 (> current-length halfway-length))
						(push current subtitles)
						(setq current nil current-length 0))))
			(when current (push current subtitles)))
		(seq-keep
		 (lambda (entry)
			 (setq current-text
						 (mapconcat (lambda (w) (assoc-default 'punctuated_word w))
												(nreverse entry) " "))
			 (when (not (string= (downcase current-text) (or last-text "")))
				 (setq last-text (downcase current-text))
				 (list nil
							 (* (assoc-default 'start (car entry) nil 0) 1000)
							 (* (assoc-default 'end (car (last entry)) nil 0) 1000)
							 ;; add speaker tag?
							 (concat
								(if (and (assoc-default 'speaker (car entry))
												 (or (null last-speaker)
														 (not (eq last-speaker (assoc-default 'speaker (car entry))))))
										(progn
											(setq last-speaker (assoc-default 'speaker (car entry)))
											(format "[Speaker %s]: " (assoc-default 'speaker (car entry))))
									"")
								current-text
								))))
		 (sort subtitles
					 (lambda (a b)
						 ;; sort by time
						 (< (assoc-default 'start (car a) nil 0)
								(assoc-default 'start (car b) nil 0)))))))

;;;###autoload
(defun my-gladia-recognize-audio (audio-file &optional diarize other-options)
	"Send AUDIO-FILE to Gladia, save the JSON, and create a VTT.
If DIARIZE is non-nil, identify speakers."
	(interactive (list (if (getenv "GLADIA_API_KEY")
												 (read-file-name "Audio file: ")
											 (error "Please specify GLADIA_API_KEY."))))
	(with-current-buffer (get-buffer-create "*recognition*")
		(erase-buffer)
		(call-process
		 "curl" nil t t "--request" "POST" "--header"
		 (concat "x-gladia-key: " (getenv "GLADIA_API_KEY"))
		 "--header" (concat "Content-Type: multipart/form-data" )
		 "--header" (concat "Accept: application/json")
		 "-F" (concat "audio=@" (expand-file-name audio-file) ";type=" (mailcap-file-name-to-mime-type audio-file))
		 "-F" (concat "toggle_noise_reduction=true&output_format=json" (or other-options "") (if diarize "&toggle_diarization=true" ""))
		 "--url" "https://api.gladia.io/audio/text/audio-transcription?toggle_noise_reduction=true&output_format=json"
		 "-o"
		 (expand-file-name (concat (file-name-sans-extension audio-file) ".json")))
		(subed-create-file
		 (concat (file-name-sans-extension audio-file) ".vtt")
		 (my-gladia-parse (concat (file-name-sans-extension audio-file) ".json"))))
	(find-file (concat (file-name-sans-extension audio-file) ".vtt")))
