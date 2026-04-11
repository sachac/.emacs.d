;;; sacha-image.el ---  -*- lexical-binding: t -*-

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



;; [[file:../Sacha.org::#embark-image][Embark and images:3]]
;;;###autoload
(defun sacha-image-open-in-annotator (file)
  (interactive "FImage: ")
  (start-process "annotator" nil "com.github.phase1geo.annotator" (expand-file-name file)))

;;;###autoload
(defun sacha-image-open-in-krita (file)
  (interactive "FImage: ")
  (start-process "krita" nil "krita" "--nosplash" (expand-file-name file)))

;;;###autoload
(defun sacha-image-open-in-inkscape (file)
  (interactive "FImage: ")
  (start-process "inkscape" nil "inkscape" (expand-file-name file)))

;;;###autoload
(defun sacha-image-open-in-gimp (file)
  (interactive "FImage: ")
  (start-process "gimp" nil "gimp" (expand-file-name file)))

;;;###autoload
(defun sacha-open-in-firefox (file)
  (interactive "FItem: ")
  (start-process "firefox" nil "firefox" (if (string-match "^http" file) file (expand-file-name file))))

(defvar sacha-image-autocrop-border 10)

;;;###autoload
(defun sacha-image-autocrop (filename &optional border)
  (interactive "FFile: ")
  (setq border (or border sacha-image-autocrop-border))
  (let ((args (append '("-trim")
                      (if border `("-bordercolor" "#FFFFFF" "-border" ,(number-to-string border)))
                      (list "+repage" (expand-file-name filename)))))
    (apply #'call-process "mogrify" nil sacha-debug-buffer nil args)
    filename))
;; Embark and images:3 ends here

;; [[file:../Sacha.org::#keybindings-embark-converting-handwriting-to-text][Converting handwriting to text:1]]
;;;###autoload
(defun sacha-image-recognize (file)
  "Returns the text."
  (interactive "FFile: ")
  (if (file-exists-p (concat (file-name-sans-extension file) ".txt"))
      (with-temp-buffer
        (insert-file-contents (concat (file-name-sans-extension file) ".txt"))
        (buffer-string))
    (let* ((data
            (json-parse-string
             (if (file-exists-p (concat (file-name-sans-extension file) ".json"))
                 (with-temp-buffer
                   (insert-file-contents (concat (file-name-sans-extension file) ".json"))
                   (buffer-string))
               (when (string= (file-name-extension file) "pdf")
                 (setq file (sacha-sketch-convert-pdf file)))
               (when (string= (file-name-extension file) "svg")
                 (call-process "inkscape" nil sacha-debug-buffer nil "--export-type=png" "--export-dpi=96" "--export-background-opacity=1" "--pdf-poppler" (expand-file-name file)))
               (catch 'done
                 (dolist (ext '(".png" ".jpg" ".jpeg"))
                   (when (file-exists-p (concat (file-name-sans-extension file) ext))
                     (with-temp-file (concat (file-name-sans-extension file) ".json")
                       (call-process "gcloud" nil t nil "ml" "vision" "detect-document"
                                     (expand-file-name (concat (file-name-sans-extension file) ext)))
                       (throw 'done (buffer-string)))))))
             :object-type 'alist))
           (text
            (if (assoc-default 'responses data)
                (assoc-default 'text (assoc-default 'fullTextAnnotation (elt (assoc-default 'responses data) 0)))
              (assoc-default 'description (elt (assoc-default 'textAnnotations data) 0)))))
      (with-temp-file (concat (file-name-sans-extension file) ".txt")
        (insert text))
      text)))
;; Converting handwriting to text:1 ends here

;; [[file:../Sacha.org::#keybindings-embark-renaming-and-storing][Renaming and storing:1]]
;;;###autoload
(defun sacha-image-rename-current-image-based-on-id (id)
  (interactive
   (let ((filename (if (derived-mode-p 'image-mode)
                       (buffer-file-name)
                     (dired-get-filename))))
     (list
      (if (string-match "[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]-[0-9][0-9]" filename)
          (match-string 0 filename)
        (read-string "ID: ")))))
  (let* ((data (sacha-journal-get-by-zidstring id))
         (old-file (if (derived-mode-p 'image-mode)
                       (buffer-file-name)
                     (dired-get-filename)))
         (ext (file-name-extension old-file))
         (new-prefix (concat id " " (plist-get data :Note)))
         (text (plist-get data :Other))
         new-file)
    (when (and text (not (string= (string-trim text) "")))
      (with-temp-file (concat (file-name-sans-extension old-file) ".txt")
        (insert text)))
    (when (derived-mode-p 'image-mode)
      (kill-buffer))
    (setq new-file
          (sacha-image-store (sacha-rename-file-set old-file
                                              new-prefix t)
                          t))
    (when (derived-mode-p 'image-mode)
      (find-file new-file))
    (find-file (concat (file-name-sans-extension new-file) ".txt"))))

;;;###autoload
(defun sacha-image-recognize-get-new-filename (file)
  (interactive "FFile: ")
  (if-let* ((text (sacha-image-recognize file))
            (id (and (string-match "[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]-[0-9][0-9]" text)
                     (match-string 0 text)))
            (data (and id (sacha-journal-get-by-zidstring id))))
      (expand-file-name
       (concat id " " (plist-get data :Note) "." (file-name-extension file))
       (file-name-directory file))
    file))

;;;###autoload
(defun sacha-image-recognize-and-rename (file)
  (interactive "FFile: ")
  (let ((new-name (expand-file-name (sacha-image-recognize-get-new-filename file)
                                    (file-name-directory file))))
    (rename-file file new-name t)
    new-name))

;;;###autoload
(defun sacha-image-tags (file)
  (setq file (file-name-base file))
  (cond
   ((string-match "#\\([^ ]+\\)" file)
    (let ((start-pos 0))
      (cl-loop for match-pos = (string-match "#\\([^ \\.]+\\)" file start-pos)
               while match-pos
               collect (match-string 1 file)
               do (setf start-pos (1+ match-pos)))))
   ((string-match " -- \\(.+\\)" file)
    (split-string (match-string 1 file) " "))))

;;;###autoload
(defun sacha-image-name-without-tags (file)
  (replace-regexp-in-string " -- \\([^\\.]+\\)" (replace-regexp-in-string " #\\([^\\.]+\\)" "" file)))

;;;###autoload
(defun sacha-image-rename-set (old-name new-name &optional tags do-copy)
  (when (or (not (string= old-name new-name)) tags)
    (when tags
      (setq new-name (concat (sacha-image-name-without-tags new-name)
                             " -- "
                             (string-join tags " ")
                             (file-name-extension new-name))))
    (dolist (file (sacha-file-set old-name))
      (funcall
       (if do-copy
           'copy-file
         'rename-file)
       file
       (concat (file-name-sans-extension new-name) "." (file-name-extension file))
       t)))
  new-name)

;;;###autoload
(defun sacha-image-store (file &optional do-move)
  "Copy or move this image into public or private sketches as needed."
  (interactive (list (if (derived-mode-p 'image-mode)
                         (buffer-file-name)
                       (dired-get-filename))
                     t))
  (unless (string-match "^[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]-[0-9][0-9] " (file-name-nondirectory file))
    (setq file (sacha-image-recognize-and-rename file)))
  (if (string-match "^[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]-[0-9][0-9] " (file-name-nondirectory file))
      (let ((private (member "private" (sacha-image-tags file))))
        (sacha-image-rename-set
         file
         (expand-file-name
          (file-name-nondirectory file)
          (if private
              sacha-private-sketches-directory
            (car sacha-sketch-directories)))))
    file))

;;;###autoload
(defun sacha-image-copy-text (file)
  (interactive "FImage: ")
  (kill-new (sacha-image-recognize file)))

;;;###autoload
(defun sacha-image-insert-text-as-details (file)
  (interactive "FImage: ")
  (when (and (derived-mode-p 'org-mode)
             (eq (org-element-type (org-element-context)) 'link))
    (goto-char (org-element-end (org-element-context))))
  (insert "\n#+begin_my_details\n" (sacha-image-recognize file) "\n#+end_my_details\n"))

;;;###autoload
(defun sacha-image-thumbnail (path)
  (interactive "FImage: ")
  (let* ((filename (expand-file-name (concat "thumb-" (file-name-nondirectory path))
                                     (file-name-directory path))))
    (call-process "convert" nil nil nil path "-thumbnail" "500x" filename)
    (kill-new filename)
    filename))
;; Renaming and storing:1 ends here

;; [[file:../Sacha.org::screenshots][screenshots]]
  (defvar sacha-screenshot-dirs
          '("~/recordings"
                  "~/.var/app/org.prismlauncher.PrismLauncher/data/PrismLauncher/instances/"
                  "~/sync/gdlauncher-instances/"
                  ))
  (defvar sacha-recent-screenshot-limit 50)

;;;###autoload
  (defun sacha-combined-screenshots (&optional limit)
          (seq-take
           (sort
                  (seq-mapcat (lambda (dir)
                                                                          (directory-files-recursively dir "[0-9][0-9][0-9][0-9]-.*\\.\\(png\\|webm\\|gif\\|svg\\|mkv\\)"))
                                                                  sacha-screenshot-dirs)
                  :key (lambda (o) (file-attribute-modification-time (file-attributes o)))
                  :reverse t)
           (or limit sacha-recent-screenshot-limit)))

;;;###autoload
  (defun sacha-latest-screenshot ()
          (car (sacha-combined-screenshots)))

;;;###autoload
  (defun sacha-show-combined-screenshots (&optional limit)
          "Show thumbnails for combined screenshots."
          (interactive (list (when current-prefix-arg (read-number "Limit: "))))
          (condition-case nil
                          ;; ignore errors from image-dired trying to set default-directory
                          (image-dired-show-all-from-dir
                           (cons (car sacha-screenshot-dirs) (sacha-combined-screenshots limit)))
                  (error nil)))
;; screenshots ends here

;; [[file:../Sacha.org::#saving-photos][Saving photos:1]]
;;;###autoload
(defun sacha-save-photo (name)
  (interactive "MName: ")
  (let* ((file (dired-get-filename))
         new-name)
    (cond
     ((string-match "CameraZOOM-\\([0-9][0-9][0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9][0-9]\\)" file)
      (setq new-name
            (format "%s-%s-%s %s.%s.%s.%s %s.jpg"
                    (match-string 1 file)
                    (match-string 2 file)
                    (match-string 3 file)
                    (match-string 4 file)
                    (match-string 5 file)
                    (match-string 6 file)
                    (match-string 7 file)
                    name)))
     ((string-match "\\([0-9][0-9][0-9][0-9]\\)[\\.-]\\([0-9][0-9]\\)[\\.-]\\([0-9][0-9]\\)[\\.- ]\\([0-9][0-9]\\)\\.\\([0-9][0-9]\\)\\.\\([0-9][0-9]\\)" file)
      (setq new-name
            (format "%s-%s-%s %s.%s.%s %s.jpg"
                    (match-string 1 file)
                    (match-string 2 file)
                    (match-string 3 file)
                    (match-string 4 file)
                    (match-string 5 file)
                    (match-string 6 file)
                    name)))
     (t (setq new-name (concat (file-name-sans-extension (file-name-nondirectory file)) " " name ".jpg"))))
    (when (string-match "A-" name)
      (copy-file file (expand-file-name new-name sacha-kid-photo-directory)))
    (rename-file file (expand-file-name new-name "~/archives/2016/photos/selected/"))))
;;;###autoload
(defun sacha-backup-media ()
  (interactive)
  (mapcar (lambda (file)
            (rename-file
             file
             (expand-file-name
              (file-name-nondirectory file)
              (cond
               ((string-match "mp4" file) "~/archives/2016/videos/")
               ((string-match "mp3\\|wav" file) "~/archives/2016/audio/")
               (t "~/archives/2016/photos/backup/")))))
          (dired-get-marked-files)))
;; Saving photos:1 ends here

;; [[file:../Sacha.org::#screenshot][Screenshot:1]]
;;;###autoload
(defun sacha-screenshot (&optional filename current-screen caption)
  "Save a screenshot of the current frame as an SVG image.
Saves to a temp file and puts the filename in the kill ring.
Prompt for a caption afterwards."
  (interactive (list nil current-prefix-arg))
  (if current-screen
      (setq filename (sacha-screenshot-current-screen filename))
    (let* ((filename
            (or filename
                (expand-file-name
                 (format-time-string "%Y-%m-%d-%H-%M-%S.svg")
                 sacha-recordings-dir)))
           (data (x-export-frames nil 'svg)))
      (with-temp-file filename
        (insert data))))
  (when (called-interactively-p 'any)
    (unless caption
      (save-window-excursion
        (with-current-buffer (find-file-noselect filename) (display-buffer (current-buffer)))
        (setq caption (read-string "Caption: "))))
    (when (and caption (not (string= caption "")))
      (let ((new-filename (concat
                           (file-name-sans-extension filename)
                           " " caption
                           "." (file-name-extension filename))))
        (rename-file filename new-filename t)
        (setq filename new-filename)))
    (kill-new filename)
    (message filename))
  filename)

;;;###autoload
(defun sacha-screenshot-current-screen (&optional filename)
  (interactive)
  (let ((new-file
         (or filename
             (expand-file-name
              (format-time-string "%Y-%m-%d-%H-%M-%S.png")
              sacha-recordings-dir))))
    (make-process
     :name "spectacle"
     :command
     (list "spectacle" "-b" "-m" "-n" "-o" new-file))
    new-file))

(keymap-global-set "C-c s" #'sacha-screenshot)
(keymap-global-set "s-s" #'sacha-screenshot)
;; Screenshot:1 ends here

;; [[file:../Sacha.org::sacha-org-insert-screenshot][sacha-org-insert-screenshot]]
;;;###autoload
(defun sacha-org-insert-screenshot (file &optional note)
  (interactive (list
                (if current-prefix-arg
                    (consult--read
                     (sacha-combined-screenshots)
                     :sort nil
                     :require-match t
                     :category 'file)
									(sacha-latest-screenshot))))
	(cond
	 ((derived-mode-p 'mastodon-toot-mode)
		(mastodon-toot--attach-media file (or note (read-string "Caption: "))))
	 ((derived-mode-p 'subed-mode)
		(insert "NOTE\n" (org-link-make-string (concat "file:" file)) "\n"))
	 (t
		(save-window-excursion
			(if (string-match "webm" file)
					(progn
						(mpv-play file)
						(insert "#+begin_media-post\n"
                    (org-link-make-string (concat "video:" file "?caption=" (or note (read-string "Caption: ")))) "\n"
                    "#+end_media-post\n"))
				(with-current-buffer (find-file-noselect file) (display-buffer (current-buffer)))
				(insert "#+CAPTION: " (or note (read-string "Caption: ")) "\n"
								(org-link-make-string (concat "file:" file)
                                      (concat "file:" file)
                                      )))))))

;;;###autoload
(defun sacha-copy-last-screenshot-to-file (new-filename)
  (interactive (list (read-file-name (format "Copy %s to: " (file-name-nondirectory (sacha-latest-screenshot))))))
  (copy-file (sacha-latest-screenshot) new-filename))

;;;###autoload
(defun sacha-copy-last-screenshot-and-insert-into-org (new-filename caption)
  (interactive (list (read-file-name (format "Copy %s to: " (file-name-nondirectory (sacha-latest-screenshot))))
                     (read-string "Caption: ")))
  (copy-file (sacha-latest-screenshot) new-filename t)
  (insert "#+CAPTION: " caption "\n"
          (org-link-make-string (concat "file:" (file-relative-name new-filename))) "\n"))

;;;###autoload
(defun sacha-convert-latest-recording ()
  (interactive)
  (let* ((latest (expand-file-name (sacha-latest-screenshot))))
    (pcase (file-name-extension latest)
      ("mkv"
       (message "Converting %s..." latest)
       (let ((new-file (new-file (concat (file-name-sans-extension latest) ".webm"))))
         (make-process :name "ffmpeg"
                       :buffer "*ffmpeg*"
                       :command
                       (list "ffmpeg" "-i" latest "-y" new-file)
                       :sentinel
                       (lambda (proc status)
                         (when (string-match "finished" status)
                           (message "%s done." new-file))))))
      ("svg"
       (let ((new-file (concat (file-name-sans-extension latest) ".png")))
         (call-process "convert" nil nil nil latest new-file)
         (kill-new new-file)
         (message "Converted %s to %s" latest new-file))))))
;; sacha-org-insert-screenshot ends here

;; [[file:../Sacha.org::#photos][Photos:1]]
;;;###autoload
(defun sacha-get-image-caption (file)
  (let ((caption (shell-command-to-string (format "exiftool -s -s -s -ImageDescription %s" (shell-quote-argument file)))))
    (when (> (length caption) 0) (format "#+CAPTION: %s" caption))))

;;;###autoload
(defun sacha-insert-image-link-with-caption (file)
  (let ((caption (sacha-get-image-caption file)))
    (insert (or caption "") (org-link-make-string file) "\n")))

;;;###autoload
(defun sacha-caption-current-image ()
  (interactive)
  (let ((link (org-element-link-parser)) caption)
    (when (and link (org-element-property :path link))
      (setq caption (sacha-get-image-caption (org-element-property :path link)))
      (when caption (insert caption)))))

;;;###autoload
(defun sacha-set-image-caption (file caption)
  (interactive (list (if (derived-mode-p 'dired-mode) (dired-get-filename) (buffer-file-name))
                     (read-string "Caption: ")))
  (shell-command (format "exiftool -ImageDescription=\"%s\" %s" (shell-quote-argument caption) (shell-quote-argument file))))
;; Photos:1 ends here

;; [[file:../Sacha.org::#photos][Photos:2]]
(defvar sacha-photo-directory "/mnt/nfs/photos/inbox")
;;;###autoload
(defun sacha-get-photo-rating (file)
  (let ((rating (shell-command-to-string (concat "exiftool -s -s -s -Rating " (shell-quote-argument file)))))
    (string-to-number rating)))

;;;###autoload
(defun sacha-make-photo-list (start end &optional rating require-description)
  (interactive (list (org-read-date "Start: ") (org-read-date "End: ")))
  (-filter
   (lambda (filename)
     (and (string> (file-name-nondirectory filename) start)
          (string> end (file-name-nondirectory filename))
          (if rating (>= (sacha-get-photo-rating filename) rating) t)
          (if require-description (sacha-get-image-caption filename) t)))
   (directory-files sacha-photo-directory t ".*\\.jpg$")))

;;;###autoload
(defun sacha-org-get-photo (id)
  "Open the photo identified by ID."
  (car (directory-files sacha-photo-directory t (concat id ".*\\.jpg"))))

;;;###autoload
(defun sacha-org-open-photo (id)
  (find-file (sacha-org-get-photo id)))

                                        ;(sacha-make-photo-list "2018-06-10" "2018-06-15" nil t)
                                        ;(sacha-get-photo-rating  (sacha-org-get-photo "2018-06-10-18-16-31"))

;;;###autoload
(defun sacha-org-significant-moments (start end &optional rating)
  (interactive (list (org-read-date "Start: ") (org-read-date "End: ") 3))
  (let ((result
         (mapconcat (lambda (file)
                      (let ((caption (sacha-get-image-caption file)))
                        (if caption
                            (concat caption (org-link-make-string file) "\n")
                          (concat (org-link-make-string file) "\n"))))
                    (sacha-make-photo-list start end 3)
                    "\n")))
    (if (called-interactively-p 'any) (insert result) result)))
;; Photos:2 ends here

;; [[file:../Sacha.org::#multimedia-images-imagemagick-rotate-clockwise-or-counterclockwise][Rotate clockwise or counterclockwise:1]]
;;;###autoload
(defun sacha-image-rotate-counterclockwise (image)
	(interactive "FImage: ")
	(call-process "mogrify" nil nil nil "-rotate" "270" image))
;;;###autoload
(defun sacha-image-rotate-clockwise (image)
	(interactive "FImage: ")
	(call-process "mogrify" nil nil nil "-rotate" "90" image))
;; Rotate clockwise or counterclockwise:1 ends here

;; [[file:../Sacha.org::#sacha-image-write-region][TOBLOG Emacs: Extract part of an image to another file:1]]
;; Based on image-crop.
;;;###autoload
(defun sacha-image-select-rect (op)
	"Select a region of the current buffer's image.
OP should be a string describing the operation (ex: \"cut\").

`q':   Exit without changing anything.
`RET': Select this region.
`m':   Make mouse movements move the rectangle instead of altering the
       rectangle shape.
`s':   Same as `m', but make the rectangle into a square first."
	(unless (image-type-available-p 'svg)
    (error "SVG support is needed to crop and cut images"))
	(let ((image (image--get-image)))
    (unless (imagep image)
      (user-error "No image under point"))
    (when (overlays-at (point))
      (user-error "Can't edit images that have overlays"))
    ;; We replace the image under point with an SVG image that looks
    ;; just like that image.  That allows us to draw lines over it.
    ;; At the end, we replace that SVG with a cropped version of the
    ;; original image.
    (let* ((data (cl-getf (cdr image) :data))
					 (type (cond
									((cl-getf (cdr image) :format)
									 (format "%s" (cl-getf (cdr image) :format)))
									(data
									 (image-crop--content-type data))))
					 (image-scaling-factor 1)
           (orig-point (point))
					 (size (image-size image t))
					 (svg (svg-create (car size) (cdr size)
														:xmlns:xlink "http://www.w3.org/1999/xlink"
														:stroke-width 5))
           ;; We want to get the original text that's covered by the
           ;; image so that we can restore it.
           (image-start
            (save-excursion
              (let ((match (text-property-search-backward 'display image)))
                (if match
                    (prop-match-end match)
                  (point-min)))))
           (image-end
            (save-excursion
              (let ((match (text-property-search-forward 'display image)))
                (if match
                    (prop-match-beginning match)
                  (point-max)))))
					 (text (buffer-substring image-start image-end))
					 (inhibit-read-only t)
           orig-data svg-end)
      (with-temp-buffer
				(set-buffer-multibyte nil)
				(if (null data)
						(insert-file-contents-literally (cl-getf (cdr image) :file))
					(insert data))
				(let ((image-crop-exif-rotate nil))
					(image-crop--possibly-rotate-buffer image))
				(setq orig-data (buffer-string))
				(setq type (image-crop--content-type orig-data))
        (image-crop--process image-crop-resize-command
                             `((?w . 600)
                               (?f . ,(cadr (split-string type "/")))))
				(setq data (buffer-string)))
      (svg-embed svg data type t
								 :width (car size)
								 :height (cdr size))
			(with-temp-buffer
				(svg-insert-image svg)
				(switch-to-buffer (current-buffer))
        (setq svg-end (point))
				;; Area
				(let ((area
							 (condition-case _
									 (save-excursion
										 (forward-line 1)
										 (image-crop--crop-image-1
											svg op))
								 (quit nil))))
					(when area
						;;  scale to original
						(let* ((image-scaling-factor 1)
									 (osize (image-size (create-image orig-data nil t) t))
									 (factor (/ (float (car osize)) (car size)))
									 ;; width x height + left + top
									 (width (abs (truncate (* factor (- (cl-getf area :right)
																											(cl-getf area :left))))))
									 (height (abs (truncate (* factor (- (cl-getf area :bottom)
																											 (cl-getf area :top))))))
									 (left (truncate (* factor (min (cl-getf area :left)
																									(cl-getf area :right)))))
									 (top (truncate (* factor (min (cl-getf area :top)
																								 (cl-getf area :bottom))))))
							(list :left left :top top
										:width width :height height
										:right (+ left width)
										:bottom (+ top height)))))))))
;; TOBLOG Emacs: Extract part of an image to another file:1 ends here

;; [[file:../Sacha.org::#sacha-image-write-region][TOBLOG Emacs: Extract part of an image to another file:2]]
;;;###autoload
(defun sacha-image-write-region ()
  "Copy a section of the image under point to a different file.
This command presents the image with a rectangular area superimposed
on it, and allows moving and resizing the area to define which
part of it to crop.

While moving/resizing the cropping area, the following key bindings
are available:

`q':   Exit without changing anything.
`RET': Save the image.
`m':   Make mouse movements move the rectangle instead of altering the
       rectangle shape.
`s':   Same as `m', but make the rectangle into a square first."
  (interactive)
	(goto-char (point-min))
	(when-let* ((orig-data (buffer-string))
							(area (sacha-image-select-rect "write"))
							(inhibit-read-only t)
							(type (image-crop--content-type orig-data))
							(left (plist-get area :left))
							(top (plist-get area :top))
							(width (plist-get area :width))
							(height (plist-get area :height)))
		(with-temp-file (read-file-name "File: ")
			(set-buffer-multibyte nil)
			(insert orig-data)
			(image-crop--process image-crop-crop-command
													 `((?l . ,left)
                             (?t . ,top)
                             (?w . ,width)
                             (?h . ,height)
                             (?f . ,(cadr (split-string type "/"))))))))
;; TOBLOG Emacs: Extract part of an image to another file:2 ends here

;; [[file:../Sacha.org::#multimedia-images-imagemagick-make-an-image-square][Make an image square:1]]
;;;###autoload
(defun sacha-image-square (filename &optional output-filename)
	(interactive)
	(let* ((size (image-size (create-image filename) t))
				(args
				 (delq nil (list
										"-background"
										"white"
										"-gravity"
										"center"
										"-resize"
										(format "%sx%s"
														(max (car size) (cdr size))
														(max (car size) (cdr size)))
										"-extent"
										(format "%sx%s"
														(max (car size) (cdr size))
														(max (car size) (cdr size)))))))
		(apply 'call-process (if output-filename "convert" "mogrify") nil nil nil
					 (if output-filename
							 (append
								(list filename)
								args
								(list output-filename))
						 (append args (list filename)))
					 args)))
;; Make an image square:1 ends here

;; [[file:../Sacha.org::#multimedia-images-imagemagick-animate-highlighting-part-of-an-image][Animate highlighting part of an image:1]]
;;;###autoload
(defun sacha-image-get-coordinates ()
	(interactive)
	(when-let*
			((area (sacha-image-select-rect "select"))
			 (x1y1x2y2
				(format "%d,%d,%d,%d"
								(plist-get area :left)
								(plist-get area :top)
								(+ (plist-get area :left)
									 (plist-get area :width))
								(+ (plist-get area :top)
									 (plist-get area :height)))))
		(when (called-interactively-p 'any)
			(kill-new x1y1x2y2))
		x1y1x2y2))
;; Animate highlighting part of an image:1 ends here

;; [[file:../Sacha.org::#artrage][Artrage:1]]
;;;###autoload
(defun sacha-artrage-export-png (directory &optional prefix)
  "Change an Artrage script file (arscript) to export images to DIRECTORY.
          If PREFIX is specified, use that instead of image-."
  (interactive "MPath: ")
  (unless (file-directory-p directory)
    (make-directory directory t))
  (while (re-search-forward "[0-9\\.]+s" nil t)
    (replace-match "0.000s"))
  (goto-char (point-min))
  (while (search-forward "<StrokeEvent>" nil t)
    (replace-match (concat
                    "EvType: Command    CommandID: ExportLayer    Idx: -1    Channels: NO    Path: \""
                    directory
                    "/" (or prefix "image-")
                    ".png\"
      <StrokeEvent>") t t)))

;; Artrage:1 ends here

;; [[file:../Sacha.org::#interactively-recolor][Interactively recolor a sketch:1]]
(defvar sacha-recolor-command "/home/sacha/bin/recolor.py")

;;;###autoload
(defun sacha-image-colors-by-frequency (file)
	"Return the colors in FILE."
	(with-temp-buffer
		(call-process sacha-recolor-command nil t nil (expand-file-name file))
		(goto-char (point-min))
		(delete-region (point-min) (1+ (line-end-position)))
		(mapcar (lambda (o) (concat "#" (car (split-string o "[ \t]"))))
						(split-string (string-trim (buffer-string)) "\n"))))

;;;###autoload
(defun sacha-completing-read-color (prompt list)
	"Display PROMPT and select a color from LIST."
	(completing-read
	 (or prompt "Color: ")
	 (mapcar (lambda (o)
						 (faces--string-with-color o o))
					 list)))

;;;###autoload
(defun sacha-image-recolor-interactively (file)
	(interactive (list (read-file-name "File: " (concat sacha-sketches-directory "/") nil t
																		 nil
																		 (lambda (file) (string-match "\\.png\\'" file)))))
	(save-window-excursion
		(find-file file)

		;; Identify the colors by frequency
		(let (choice done)
			(while (not done)
				(let* ((by-freq (sacha-image-colors-by-frequency file))
							 (old-color (sacha-completing-read-color "Old color: " by-freq))
							 (new-color (read-color "New color: " t))
							 (temp-file (make-temp-file "recolor" nil (concat "." (file-name-extension file))))
							 color-map)
					(when (string-match "#\\(..\\)..\\(..\\)..\\(..\\).." new-color)
						(setq new-color (concat (match-string 1 new-color)
																		(match-string 2 new-color)
																		(match-string 3 new-color))))
					(setq color-map (replace-regexp-in-string "#" "" (concat old-color "," new-color)))
					(call-process sacha-recolor-command nil nil nil
												(expand-file-name file)
												"--colors"
												color-map
												"--output" temp-file)
					(find-file temp-file)
					(pcase (read-char-choice "(y)es, (m)ore, (r)edo, (c)ancel: " "yrc")
						(?y
						 (kill-buffer)
						 (rename-file temp-file file t)
						 (setq done t))
						(?m
						 (kill-buffer)
						 (rename-file temp-file file t))
						(?r
						 (kill-buffer)
						 (delete-file temp-file))
						(?c
						 (kill-buffer)
						 (delete-file temp-file)
						 (setq done t))))))))
;; Interactively recolor a sketch:1 ends here

;; [[file:../Sacha.org::#rename-scanned-index-cards][Rename scanned index cards:2]]
;;;###autoload
(defun sacha-process-tiff (files)
  "Convert, display, rename, and upload FILES."
  (interactive (list (dired-get-marked-files)))
  (unless (listp files) (setq files (list files)))
  (save-window-excursion
    (apply 'call-process "mogrify" nil nil nil (append (list "-format" "png" "-quality" "1") files))
    (delete-other-windows)
    (setq files
          (mapcar
           (lambda (filename)
             (find-file (setq filename (s-append ".png" (s-chop-suffix ".tif" filename))))
             (let ((new-name
                    (read-string "New name: "
                                 (concat
                                  (if (string-match "/\\(\\([0-9]+-[0-9]+-[0-9]+\\)\\( ?.*\\)?\\)\\.png" filename)
                                      (match-string 1 filename)
                                    filename)
                                  " "))))
               (rename-file filename (concat new-name ".png"))
               (setq filename (expand-file-name (concat new-name ".png") (file-name-directory filename)))))
           files)))
  (find-file "~/Dropbox/Public/sharing/index.org")
  (goto-char (point-min))
  (when (re-search-forward (regexp-quote "#+ORGLST: sketchinbox"))
    (forward-line 1)
    (org-end-of-item-list)
    (apply 'call-process "up" nil t nil files)))

;;;###autoload
(defun sacha-convert-index-card-to-png (o)
  (lambda (o)
    (call-process "krita" nil nil nil o "--export" "--export-filename"
                  (concat (file-name-sans-extension o) ".png"))
    (rename-file o "~/Dropbox/Inbox/backup/" t)))

;;;###autoload
(defun sacha-convert-index-card-tiffs-to-pngs ()
  (interactive)
  (let ((pattern "^\\(IMG\\|[0-9]+-[0-9]+-[0-9]+\\).*.\\(tif\\|psd\\)$"))
    (when (directory-files "~/Dropbox/Inbox/" t pattern)
      ;; Convert the TIFFs first
      (mapc 'sacha-convert-index-card-to-png
            (directory-files "~/Dropbox/Inbox/" t pattern)))))

;;;###autoload
(defun sacha-convert-and-upload-cards ()
  "Trust in existing filenames, upload without modification."
  (interactive)
  (sacha-convert-index-card-tiffs-to-pngs)
  (sacha-upload-scanned-cards))

;;;###autoload
(defun sacha-rename-scanned-card (filename)
  (find-file filename)
  (delete-other-windows)
  (let ((base (file-name-sans-extension filename))
        notes)
    (when (string-match "/IMG.*\\|\\(\\([0-9]+-[0-9]+-[0-9]+\\)\\( ?.*\\)?\\)" base)
      (let ((kill-buffer-query-functions nil)
            old-name
            (new-name (read-string "New name: "
                                   (if (match-string 1 base)
                                       (concat (match-string 1 base))
                                     ""))))
        (while (and (string-match "^[0-9]+-[0-9]+-[0-9]+[a-z]" new-name)
                    (setq old-name (sacha-get-sketch-filename (match-string 0 new-name)))
                    (and old-name
                         (not (string= old-name filename))
                         (not (string= (file-name-nondirectory old-name)
                                       (concat (s-trim new-name) "." (file-name-extension filename))))))
          (setq new-name
                (read-string (format "Already exists (%s) - new name: " old-name)
                             new-name)))
        (when (string-match new-name "^\\(.*?\\) *| *\\(.*\\)")
          (with-current-buffer (find-file "~/sync/orgzly/Inbox.org")
            (goto-char (point-max))
            (insert "\n* " (match-string 1 new-name) "\n" (match-string 2 new-name))
            (save-buffer))
          (setq new-name (match-string 1 new-name)))
        (when (> (length new-name) 0)
          (revert-buffer t t)
          (rename-file filename (concat (s-trim new-name) "." (file-name-extension filename)) t)
          (kill-buffer))))))

;;;###autoload
(defun sacha-rename-scanned-cards ()
  "Display and rename the scanned or saved files."
  (interactive)
  (sacha-convert-index-card-tiffs-to-pngs)
  (mapc (lambda (o)
          (when (string= (file-name-extension o) "psd")
            (sacha-convert-index-card-to-png o)
            (setq o (concat (file-name-sans-extension o) ".png")))
          (sacha-rename-scanned-card o))
        (reverse (directory-files "~/Dropbox/Inbox/" t "^\\(IMG\\|[0-9]+-[0-9]+-[0-9]+\\).*.\\(psd\\|png\\|jpg\\)")))
  (sacha-upload-scanned-cards))

;;;###autoload
(defun sacha-clean-index-card-directory ()
  "Remove files marked for deletion and move private files."
  (shell-command "mv ~/Dropbox/Inbox/*delete* ~/Dropbox/Inbox/backup")
  (shell-command "mv ~/Dropbox/Inbox/*private* ~/cloud/private-sketches/"))

;;;###autoload
(defun sacha-upload-scanned-cards ()
  (interactive)
  (sacha-clean-index-card-directory)
  (with-current-buffer (get-buffer-create "*Files to be uploaded*")
    (erase-buffer)
    (insert (mapconcat 'identity (directory-files "~/Dropbox/Inbox" nil "^[0-9]+-[0-9]+-[0-9]+[^ ]? .*.\\(png\\|jpg\\)") "\n"))
    (goto-char (point-min))
    (switch-to-buffer (current-buffer))
    (delete-other-windows))
  (shell-command "~/bin/copy-sketches"))
;; Rename scanned index cards:2 ends here

;; [[file:../Sacha.org::#supernote][Supernote:3]]
;;;###autoload
(defun sacha-image-autorotate (file)
	(let ((tags (sacha-image-tags file)))
		(cond
		 ((member "ccw" tags)
			(call-process "mogrify" nil nil nil "-rotate" "270" file)
			(sacha-image-rename-set file file (delete "ccw" tags)))
		 ((member "ccw" tags)
			(call-process "mogrify" nil nil nil "-rotate" "90" file)
			(sacha-image-rename-set file file (delete "cw" tags)))
		 (t file))))
;; Supernote:3 ends here

;; [[file:../Sacha.org::#manage-photos-with-geeqie][TOBLOG Manage photos with geeqie:1]]
(defvar sacha-scan-directory "~/sync/scans/")
(defvar sacha-ipad-directory "~/sync/ipad")
(defvar sacha-portfolio-directory "~/sync/portfolio")
(defvar sacha-camera-directory "~/sync/camera")
(defvar sacha-private-sketches-directory "~/sync/private-sketches")
(defvar sacha-sketches-directory "~/sync/sketches")
;;;###autoload
(defun sacha-scans-dired () (interactive) (dired sacha-scan-directory "-lt"))
;;;###autoload
(defun sacha-geeqie-next ()
  (interactive)
  (shell-command "geeqie --remote -n"))
;;;###autoload
(defun sacha-geeqie-previous ()
  (interactive)
  (shell-command "geeqie --remote -b"))
;;;###autoload
(defun sacha-geeqie-filename ()
  (string-trim (shell-command-to-string "geeqie --remote --tell")))
;;;###autoload
(defun sacha-geeqie-insert-file-link ()
  (interactive)
  (insert (org-link-make-string (concat "file:" (string-trim (shell-command-to-string "geeqie --remote --tell"))))))
;; TOBLOG Manage photos with geeqie:1 ends here

;; [[file:../Sacha.org::sacha-geeqie-view][sacha-geeqie-view]]
;;;###autoload
(defun sacha-geeqie-view (filenames)
  (interactive "f")
  (start-process-shell-command
   "geeqie" nil
	 (concat
    "geeqie --remote "
    (mapconcat
     (lambda (f)
       (concat "file:" (shell-quote-argument f)))
     (cond
      ((listp filenames) filenames)
      ((file-directory-p filenames)
       (list (car (seq-filter #'file-regular-p (directory-files filenames t)))))
      (t (list filenames)))
     " "))))
;; sacha-geeqie-view ends here

;; [[file:../Sacha.org::#manage-photos-with-geeqie][TOBLOG Manage photos with geeqie:3]]

(defvar sacha-rotate-jpeg-using-exiftran nil)

;;;###autoload
(defun sacha-rotate-image-clockwise (filename)
  (if (and sacha-rotate-jpeg-using-exiftran
					 (string-match "jpe?g" (file-name-extension filename)))
			(call-process "exiftran" nil nil nil "-i" "-9" filename)
		(call-process "mogrify" nil nil nil "-rotate" "90" filename)))

;;;###autoload
(defun sacha-rotate-image-counterclockwise (filename)
  (if (and sacha-rotate-jpeg-using-exiftran
					 (string-match "jpe?g" (file-name-extension filename)))
			(call-process "exiftran" nil nil nil "-i" "-2" filename)
		(call-process "mogrify" nil nil nil "-rotate" "270" filename)))

;;;###autoload
(defun sacha-geeqie-rotate-clockwise ()
  (interactive)
  (sacha-rotate-image-clockwise (sacha-geeqie-filename))
  (sacha-geeqie-view (sacha-geeqie-filename)))

;;;###autoload
(defun sacha-geeqie-rotate-counterclockwise ()
  (interactive)
  (sacha-rotate-image-counterclockwise (sacha-geeqie-filename))
  (sacha-geeqie-view (sacha-geeqie-filename)))

;;;###autoload
(defun sacha-rename-file-based-on-modification-time (filename)
  "Rename files to their modification time."
  (rename-file filename
							 (expand-file-name
								(concat
								 (format-time-string "%Y-%m-%d_%H%M%S"
																		 (file-attribute-modification-time (file-attributes filename)))
								 "."
								 (file-name-extension filename))
								(file-name-directory filename))))

;;;###autoload
(defun sacha-geeqie-change-date (filename new-time)
  (interactive (list (sacha-geeqie-filename)
										 (let ((org-read-date-prefer-future nil))
											 (org-read-date nil t))))
  (let ((new-file (expand-file-name
									 (replace-regexp-in-string
										"^[0-9]*"
										(format-time-string
										 "%Y%m%d"
										 new-time)
										(file-name-nondirectory filename))
									 (file-name-directory filename))))
		(rename-file filename new-file)
		(sacha-geeqie-view new-file)))

;;;###autoload
(defun sacha-geeqie-rename-current (old-filename new-filename)
  (interactive
   (list (sacha-geeqie-filename)
				 (read-string "Filename: " (concat (file-name-base (sacha-geeqie-filename)) " "))))
  (rename-file old-filename
							 (expand-file-name
								(concat new-filename "." (file-name-extension old-filename))
								(file-name-directory old-filename))))

;;;###autoload
(defun sacha-geeqie-crop-to-rectangle ()
  (interactive)
  (call-process
   "mogrify" nil nil nil "-crop"
   (string-trim (shell-command-to-string "geeqie --remote --get-rectangle"))
   (sacha-geeqie-filename))
  (sacha-geeqie-view (sacha-geeqie-filename)))

;;;###autoload
(defun sacha-geeqie-scans ()
  "Rename files and open the first one."
  (interactive)
  (mapc 'sacha-rename-file-based-on-modification-time (directory-files sacha-scan-directory t "^scan"))
  (call-process "geeqie" nil nil nil "--remote" (concat "file:" (shell-quote-argument (seq-find 'file-regular-p (directory-files "~/sync/scans" t "^[0-9].*\\(gif\\|png\\|jpg\\)"))))))

;;;###autoload
(defun sacha-geeqie-delete-and-next ()
  (interactive)
  (let ((file (sacha-geeqie-filename)))
		(sacha-geeqie-next)
		(delete-file file t)))

;; TOBLOG Manage photos with geeqie:3 ends here

;; [[file:../Sacha.org::#manage-photos-with-geeqie][TOBLOG Manage photos with geeqie:5]]
;;;###autoload
(defun sacha-geeqie-setup ()
  (interactive)
  (shell-command "wmctrl -r :ACTIVE: -b remove,maximized_vert,maximized_horz; xdotool getactivewindow windowsize 50% 100%")
  (shell-command "geeqie &"))
;; TOBLOG Manage photos with geeqie:5 ends here

;; [[file:../Sacha.org::#manage-photos-with-geeqie][TOBLOG Manage photos with geeqie:7]]
;;;###autoload
(defun sacha-move-portfolio-files ()
  (interactive)
  (mapc (lambda (f)
					(let ((new-dir
								 (cond
									((string-match "#private" f) sacha-private-sketches-directory)
									((string-match "#me\\>" f) sacha-sketches-directory)
									(t sacha-portfolio-directory))))
						(when new-dir (rename-file f (expand-file-name (file-name-nondirectory f) new-dir)))))
				(seq-filter
				 'file-regular-p
				 (directory-files sacha-scan-directory t "^[0-9]+.*#")))
  (shell-command-to-string "make-sketch-thumbnails"))
;; TOBLOG Manage photos with geeqie:7 ends here

;; [[file:../Sacha.org::#tools-for-organizing][Tools for organizing:1]]
;;;###autoload
(defun sacha-rename-bank-statements ()
  (interactive)
  (let ((months '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")))
    (cl-loop for i from 1 to 12 do
             (message "%d" i)
             (goto-char (point-min))
             (while (re-search-forward (elt months (1- i)) nil t)
               (ignore-errors
                 (replace-match (format "%02d" i))
                 )))))

;;;###autoload
(defun sacha-rename-scanned-receipts ()
  "Display and rename the scanned or saved files."
  (interactive)
  (delete-other-windows)
  (mapc (lambda (o)
          (find-file o)
          (let ((new-name (concat (read-string "New filename: ") ".jpg")))
            (kill-buffer)
            (unless (string= new-name ".jpg")
              (rename-file o new-name))))
        (or (if (derived-mode-p 'dired-mode)
                (dired-get-marked-files))
            (directory-files default-directory t "^[-_0-9]+\\.jpg"))))
;; Tools for organizing:1 ends here

(provide 'sacha-image)
;;; sacha-image.el ends here
