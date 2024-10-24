;; Starting up
;; :PROPERTIES:
;; :CUSTOM_ID: starting-up
;; :END:

;; Here's how we start:

;; #+NAME: startup

;; [[file:Sacha.org::startup][startup]]
;; -*- lexical-binding: t -*-
;; This sets up the load path so that we can override it
(setq warning-suppress-log-types '((package reinitialization)))  (package-initialize)
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp")
(add-to-list 'load-path "~/vendor/org-mode/lisp")
(add-to-list 'load-path "~/vendor/org-mode/contrib/lisp")
(setq custom-file "~/.config/emacs/custom-settings.el")
(setq use-package-always-ensure t)
(load custom-file t)
;; startup ends here

;; Add package sources
;; :PROPERTIES:
;; :CUSTOM_ID: add-package-sources
;; :END:


;; [[file:Sacha.org::#add-package-sources][Add package sources:1]]
(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))
(unless (assoc-default "nongnu" package-archives)
  (add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t))
;; Add package sources:1 ends here

;; Add my elisp directory and other files
;; :PROPERTIES:
;; :CUSTOM_ID: add-my-elisp-directory-and-other-files
;; :END:

;; Sometimes I load files outside the package system. As long as they're
;; in a directory in my =load-path=, Emacs can find them.

;; #+NAME: package-setup

;; [[file:Sacha.org::package-setup][package-setup]]
(add-to-list 'load-path "~/elisp")
(setq use-package-verbose t)
(setq use-package-always-ensure t)
(require 'use-package)
(use-package quelpa)
(use-package quelpa-use-package)
(quelpa-use-package-activate-advice)
(setq load-prefer-newer t)
;; package-setup ends here

;; Personal information
;; :PROPERTIES:
;; :CUSTOM_ID: personal-information
;; :END:


;; [[file:Sacha.org::#personal-information][Personal information:1]]
(setq user-full-name "Sacha Chua"
      user-mail-address "sacha@sachachua.com")
;; Personal information:1 ends here

;; System information
;; :PROPERTIES:
;; :CUSTOM_ID: system-information
;; :END:

;; #+NAME: system-info

;; [[file:Sacha.org::system-info][system-info]]
(defvar my-laptop-p (or (equal (system-name) "sacha-x230") (equal (system-name) "sacha-p52")))
(defvar my-server-p (and (equal (system-name) "localhost") (equal user-login-name "sacha")))
(defvar my-phone-p (not (null (getenv "ANDROID_ROOT")))
  "If non-nil, GNU Emacs is running on Termux.")
(when my-phone-p (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))
(global-auto-revert-mode)  ; simplifies syncing
;; system-info ends here

;; Reload
;; :PROPERTIES:
;; :CUSTOM_ID: reload
;; :END:


;; [[file:Sacha.org::#reload][Reload:1]]
(defun my-reload-emacs-configuration ()
  (interactive)
  (load-file "~/proj/.emacs.d/Sacha.el"))
;; Reload:1 ends here

;; Backups
;; :PROPERTIES:
;; :CUSTOM_ID: backups
;; :END:

;; This is one of the things people usually want to change right away. By default, Emacs saves backup files in the current directory. These are the files ending in =~= that are cluttering up your directory lists. The following code stashes them all in =~/.config/emacs/backups=, where I can find them with =C-x C-f= (=find-file=) if I really need to.


;; [[file:Sacha.org::#backups][Backups:1]]
(setq backup-directory-alist '(("." . "~/.config/emacs/backups")))
(with-eval-after-load 'tramp
	(add-to-list 'tramp-backup-directory-alist
							 (cons tramp-file-name-regexp nil)))
;; Backups:1 ends here



;; Disk space is cheap. Save lots.


;; [[file:Sacha.org::#backups][Backups:2]]
(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.config/emacs/auto-save-list/" t)))
;; Backups:2 ends here

;; History
;; :PROPERTIES:
;; :CUSTOM_ID: history
;; :END:

;; From http://www.wisdomandwonder.com/wp-content/uploads/2014/03/C3F.html:

;; [[file:Sacha.org::#history][History:1]]
(setq savehist-file "~/.config/emacs/savehist")
(savehist-mode 1)
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring))
;; History:1 ends here

;; Disabling the toolbar
;; :PROPERTIES:
;; :ID:       440c0b9a-9068-450b-89a3-a20c8ec1f447
;; :DRILL_LAST_INTERVAL: 3.86
;; :DRILL_REPEATS_SINCE_FAIL: 2
;; :DRILL_TOTAL_REPEATS: 1
;; :DRILL_FAILURE_COUNT: 0
;; :DRILL_AVERAGE_QUALITY: 3.0
;; :DRILL_EASE: 2.36
;; :DRILL_LAST_QUALITY: 3
;; :DRILL_LAST_REVIEWED: [2013-02-27 Wed 23:14]
;; :CUSTOM_ID: windows-configuration
;; :END:

;; When you're starting out, the tool bar can be very helpful. [[http://sachachua.com/blog/2014/03/emacs-basics-using-mouse/][(Emacs Basics: Using the Mouse]]). Eventually, you may want to reclaim that extra little bit of screenspace. The following code turns that thing off. (Although I changed my mind about the menu - I want that again.)


;; [[file:Sacha.org::#windows-configuration][Disabling the toolbar:1]]
(tool-bar-mode -1)
;; Disabling the toolbar:1 ends here

;; Change "yes or no" to "y or n"
;; :PROPERTIES:
;; :CUSTOM_ID: change-yes-or-no-to-y-or-n
;; :END:

;; Lazy people like me never want to type "yes" when "y" will suffice.


;; [[file:Sacha.org::#change-yes-or-no-to-y-or-n][Change "yes or no" to "y or n":1]]
(fset 'yes-or-no-p 'y-or-n-p)
;; Change "yes or no" to "y or n":1 ends here

;; Minibuffer editing - more space!
;; :PROPERTIES:
;; :CUSTOM_ID: minibuffer-editing-more-space
;; :END:

;; Sometimes you want to be able to do fancy things with the text
;; that you're entering into the minibuffer. Sometimes you just want
;; to be able to read it, especially when it comes to lots of text.
;; This binds =C-M-e= in a minibuffer) so that you can edit the
;; contents of the minibuffer before submitting it.


;; [[file:Sacha.org::#minibuffer-editing-more-space][Minibuffer editing - more space!:1]]
(use-package miniedit
  :commands minibuffer-edit
  :init (miniedit-install))
;; Minibuffer editing - more space!:1 ends here

;; Killing text
;; :PROPERTIES:
;; :CUSTOM_ID: killing-text
;; :END:


;; [[file:Sacha.org::#killing-text][Killing text:1]]
(setq kill-ring-max 1000)
;; Killing text:1 ends here



;; From https://github.com/itsjeyd/emacs-config/blob/emacs24/init.el


;; [[file:Sacha.org::#killing-text][Killing text:2]]
(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))
;; Killing text:2 ends here

;; Keybindings
;; :PROPERTIES:
;; :CUSTOM_ID: keybindings
;; :END:


;; [[file:Sacha.org::#keybindings][Keybindings:1]]
(repeat-mode 1)
;; Keybindings:1 ends here

;; Embark                                                           :embark:
;; :PROPERTIES:
;; :CUSTOM_ID: embark
;; :END:

;; [[file:Sacha.org::#embark][Embark:1]]
(use-package embark
  :after org
  :load-path "~/vendor/embark"
					; :quelpa (embark :fetcher github :repo "oantolin/embark")
  :config
	(setq embark-prompter 'embark-keymap-prompter)
	(add-to-list 'embark-target-finders 'my-embark-org-element)
	(add-to-list 'embark-target-finders 'my-embark-subed-timestamp)
	(add-to-list 'embark-target-injection-hooks '(my-journal-post embark--allow-edit))
	(with-eval-after-load 'subed
	  (defvar-keymap embark-subed-timestamp-actions
	    :doc "Subed timestamp actions"
	    :parent subed-mode-map
	    "." #'my-subed-set-timestamp-to-mpv-position
	    "c" #'my-subed-copy-timestamp-dwim
	    "<up>" #'my-subed-adjust-timestamp/my-subed-adjust-timestamp-up
	    "w" #'my-waveform-subed-show-after-time
	    "<down>" #'my-subed-adjust-timestamp/my-subed-adjust-timestamp-down))
	(defvar-keymap embark-sketch-actions
	  :doc "Org Mode sketch-related actions"
	  :parent org-mode-map
	  "o" #'my-sketch-insert-file-as-link
	  "v" #'my-geeqie-view)
	(defvar-keymap embark-journal-actions
	  :doc "Journal"
	  "e" #'my-journal-edit)
	(add-to-list 'embark-keymap-alist '(sketch . embark-sketch-actions))
	(add-to-list 'embark-keymap-alist '(subed-timestamp . embark-subed-timestamp-actions))
	(add-to-list 'embark-keymap-alist '(journal . embark-journal-actions))
	:bind
	(("C-." . embark-act)
	 ("C-;" . embark-act)
	 :map vertico-map
	 (("M-e" . embark-export))
	 :map minibuffer-local-map
	 (("C-c e" . embark-act)
		("M-e" . embark-export)
	  ("C-;" . embark-act)
		("C-<tab>" . embark-select)
		("C-SPC" . (lambda () (interactive) (embark-select) (vertico-next))))
	 :map embark-collect-mode-map
	 (("C-c e" . embark-act)
	  ("C-;" . embark-act)
		("C-<tab>" . embark-select))
	 :map embark-general-map
	 (("j" . my-journal-post)
	  ("m" . my-stream-message)
	  ("M-w" . (lambda (s) (interactive "MString: ") (kill-new s))))
	 :map embark-symbol-map
	 ("r" . erefactor-rename-symbol-in-buffer)
	 :map embark-url-map
	 ("c" . my-caption-show)
	 ))
(with-eval-after-load 'embark-org
  (define-key embark-org-src-block-map
	 "i" #'my-org-fix-block-indentation))
;; Embark:1 ends here

;; Using Embark and qrencode to show a QR code for the Org Mode link at point :emacs:org:
;; :PROPERTIES:
;; :CUSTOM_ID: embark-qr
;; :EXPORT_DATE: 2024-01-10T15:46:11-0500
;; :EXPORT_ELEVENTY_PERMALINK: /blog/2024/01/using-embark-and-qrencode-to-show-a-qr-code-for-the-org-mode-link-at-point/
;; :EXPORT_ELEVENTY_FILE_NAME: blog/2024/01/using-embark-and-qrencode-to-show-a-qr-code-for-the-org-mode-link-at-point/
;; :EXPORT_MODIFIED: 2024-01-12T07:31:44-0500
;; :END:

;; #+begin_update
;; [2024-01-12]: Added some code to display the QR code on the right side.
;; #+end_update

;; John Kitchin includes [[https://www.youtube.com/watch?v=rGGAr1AWkTc][little QR codes in his videos]]. I
;; thought that was a neat touch that makes it easier for
;; people to jump to a link while they're watching. I'd like to
;; make it easier to show QR codes too. The following code lets
;; me show a QR code for the Org link at point. Since many of
;; my links use custom Org link types that aren't that useful
;; for people to scan, the code reuses the link resolution code
;; from [[dotemacs:web-link]] so that I can get the regular
;; ~https:~ link.


;; [[file:Sacha.org::#embark-qr][Using Embark and qrencode to show a QR code for the Org Mode link at point:1]]
(defun my-org-link-qr (url)
	"Display a QR code for URL in a buffer."
	(let ((buf (save-window-excursion (qrencode--encode-to-buffer (my-org-stored-link-as-url url)))))
		(if (> (frame-width) 80)
				(display-buffer-in-side-window buf '((side . right)))
			(display-buffer buf))))

(use-package qrencode
	:config
	(with-eval-after-load 'embark-org
		(define-key embark-org-link-map (kbd "q") #'my-org-link-qr)))
;; Using Embark and qrencode to show a QR code for the Org Mode link at point:1 ends here

;; TODO Using Embark to act on video
;; :PROPERTIES:
;; :CUSTOM_ID: embark-video
;; :END:


;; [[file:Sacha.org::#embark-video][Using Embark to act on video:1]]
(defun my-embark-video ()
	"Match video."
	(let ((extensions "youtu\\.?be\\|\\(webm\\|mp4\\|flv\\)$"))
		(if-let ((link (and (derived-mode-p 'org-mode)
												(org-element-context))))
				(when (eq (org-element-type link) 'link)
					(cond
					 ((string-match extensions (org-element-property :path link))
						(cons 'video (org-element-property :path link)))))
			(when (and (derived-mode-p 'dired-mode)
								 (string-match extensions (dired-get-filename)))
				(cons 'video (dired-get-filename))))))

(with-eval-after-load 'embark
	(add-to-list 'embark-target-finders 'my-embark-video)
	(defvar-keymap my-embark-video-actions
		:doc "video"
		"d" #'my-deepgram-recognize-audio
		"$" #'my-deepgram-cost
		"m" #'mpv-play
		"c" #'my-caption-show
		"w" #'my-audio-text
		"W" #'waveform-show)
	(add-to-list 'embark-keymap-alist '(video . my-embark-video-actions)))
;; Using Embark to act on video:1 ends here

;; Using Embark to act on audio
;; :PROPERTIES:
;; :CUSTOM_ID: embark-audio
;; :END:


;; [[file:Sacha.org::#embark-audio][Using Embark to act on audio:1]]
(defun my-embark-audio ()
	"Match audio."
	(let ((extensions "m4a\\|mp3\\|wav\\|ogg\\|opus"))
		(if-let ((link (and (derived-mode-p 'org-mode)
												(org-element-context))))
				(when (eq (org-element-type link) 'link)
					(cond
					 ((string-match extensions (org-element-property :path link))
						(cons 'audio (org-element-property :path link)))))
			(when (and (derived-mode-p 'dired-mode)
								 (string-match extensions (dired-get-filename)))
				(cons 'audio (dired-get-filename))))))

(defun my-audio-text (file &optional insert)
	"Get the text for FILE audio.
If called interactively, copy to the kill ring."
	(interactive (list (read-file-name "Audio: ")))
	(let (text)
		(cond
		 ((file-exists-p (concat (file-name-sans-extension file) ".txt"))
			(with-temp-buffer
				(insert-file-contents (concat (file-name-sans-extension file) ".txt"))
				(setq text (buffer-string))))
		 ;; no txt yet, is there a vtt?
		 ((file-exists-p (concat (file-name-sans-extension file) ".vtt"))
			(setq text (subed-subtitle-list-text
									(subed-parse-file (concat (file-name-sans-extension file) ".vtt")))))
		 ;; no VTT, let's recognize it
		 (t
			(my-deepgram-recognize-audio file)
			(when (file-exists-p (concat (file-name-sans-extension file) ".vtt"))
				(setq text (subed-subtitle-list-text
										(subed-parse-file (concat (file-name-sans-extension file) ".vtt")))))))
		(when text
			(when (called-interactively-p 'any)
				(if insert
						(insert text "\n")
					(kill-new text)))
			text)))

(defun my-open-in-audacity (file)
	(interactive "FFile: ")
	(start-process "audacity" nil "audacity" file))

(with-eval-after-load 'embark
	(add-to-list 'embark-target-finders 'my-embark-audio)
	(defvar-keymap my-embark-audio-actions
		:doc "audio"
		"a" #'my-open-in-audacity
		"d" #'my-deepgram-recognize-audio
		"$" #'my-deepgram-cost
		"D" #'my-audio-braindump-reprocess
		"m" #'mpv-play
		"w" #'my-audio-text
		"W" #'waveform-show)
	(add-to-list 'embark-keymap-alist '(audio . my-embark-audio-actions)))
;; Using Embark to act on audio:1 ends here

;; Using Embark to insert files as Org INCLUDEs
;; :PROPERTIES:
;; :CUSTOM_ID: using-embark-to-insert-files-as-org-includes
;; :END:


;; [[file:Sacha.org::#using-embark-to-insert-files-as-org-includes][Using Embark to insert files as Org INCLUDEs:1]]
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

(defun my-transform-org-link-to-include ()
	(interactive)
	(let ((link (org-element-lineage (org-element-context) '(link) t))
				(mode (assoc-default (org-element-property :path link) auto-mode-alist 'string-match)))
		(when link
			(delete-region (org-element-property :begin link)
										 (org-element-property :end link))
			(my-insert-file-as-org-include (org-element-property :path link)))))


(with-eval-after-load 'embark
	(define-key embark-file-map "O" #'my-insert-file-as-org-include))
;; Using Embark to insert files as Org INCLUDEs:1 ends here

;; Using Embark to offer context-sensitive actions for Org elements
;; :PROPERTIES:
;; :CUSTOM_ID: using-embark-to-offer-context-sensitive-actions-for-org-elements
;; :END:

;; #+NAME: embark

;; [[file:Sacha.org::embark][embark]]
(let ((foo '"bar"))
(defun my-embark-org-element ()
  "Target an Org Mode element at point."
  (save-window-excursion
    (save-excursion
      (save-restriction
        (when (derived-mode-p 'org-agenda-mode)
          (org-goto-marker-or-bmk (org-get-at-bol 'org-marker))
          (org-back-to-heading))
        (when (derived-mode-p 'org-mode)
          (let* ((context ;; Borrowed from org-open-at-point
	                ;; Only consider supported types, even if they are not the
	                ;; closest one.
	                (org-element-lineage (org-element-context)
                                       '(headline src-block link) t))
                 (type (org-element-type context))
                 (value (org-element-property :value context)))
            (cond ((eq type 'headline)
                   (cons 'org-heading (org-element-property :title context)))
                  ((eq type 'src-block)
                   (cons 'org-src-block (org-element-property :name context)))
                  ((eq type 'link)
                   (cons 'url (org-element-property :raw-link context))))))))))

(defun my-embark-org-src-block-copy-noweb-reference (element)
  (kill-new (if (org-element-property element :parameters)
                (format "<<%s(%s)>>" (org-element-property element :name)
                        (org-element-property element :parameters))
              (format "<<%s>>" (org-element-property element :parameters)))))
)
;; embark ends here

;; Whichkey and Embark

;; From https://github.com/oantolin/embark/wiki/Additional-Configuration#use-which-key-like-a-key-menu-prompt


;; [[file:Sacha.org::*Whichkey and Embark][Whichkey and Embark:1]]
(defun embark-which-key-indicator ()
  "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
  (lambda (&optional keymap targets prefix)
    (if (null keymap)
        (which-key--hide-popup-ignore-command)
      (which-key--show-keymap
       (if (eq (plist-get (car targets) :type) 'embark-become)
           "Become"
         (format "Act on %s '%s'%s"
                 (plist-get (car targets) :type)
                 (embark--truncate-target (plist-get (car targets) :target))
                 (if (cdr targets) "…" "")))
       (if prefix
           (pcase (lookup-key keymap prefix 'accept-default)
             ((and (pred keymapp) km) km)
             (_ (key-binding prefix 'accept-default)))
         keymap)
       nil nil t (lambda (binding)
                   (not (string-suffix-p "-argument" (cdr binding))))))))

(setq embark-indicators
  '(embark-which-key-indicator
    embark-highlight-indicator
    embark-isearch-highlight-indicator))

(defun embark-hide-which-key-indicator (fn &rest args)
  "Hide the which-key indicator immediately when using the completing-read prompter."
  (which-key--hide-popup-ignore-command)
  (let ((embark-indicators
         (remq #'embark-which-key-indicator embark-indicators)))
      (apply fn args)))

(with-eval-after-load 'embark
	(advice-add #'embark-completing-read-prompter
							:around #'embark-hide-which-key-indicator))
;; Whichkey and Embark:1 ends here

;; Embark and images   :image:
;; :PROPERTIES:
;; :CATEGORY: Sacha
;; :EXPORT_MODIFIED: 2024-09-25T14:31:49-0400
;; :CUSTOM_ID: embark-image
;; :END:

;; #+begin_update
;; [2024-09-25]: added attachment handling
;; #+end_update


;; [[file:Sacha.org::#embark-image][Embark and images:1]]
(defun my-sketch-insert-file-as-link (f)
	(interactive "fSketch: ")
	(insert (org-link-make-string (concat "sketch:" (file-name-nondirectory f))) "\n"))
;; Embark and images:1 ends here

;; [[file:Sacha.org::#embark-image][Embark and images:2]]
(defun my-embark-image ()
	"Match images."
	(let ((extensions "\\(png\\|jpg\\|svg\\|gif\\)\\'"))
		(if-let ((link (and (derived-mode-p 'org-mode)
												(org-element-context))))
				(when (eq (org-element-type link) 'link)
					(cond
					 ((string= "attachment" (org-element-property :type link))
						(cons 'image (expand-file-name (org-element-property :path link)
																					 (org-attach-dir))))
					 ((string-match "sketch" (org-element-property :type link))
						(cons 'image (my-get-sketch-filename (org-element-property :path link))))
					 ((string-match extensions (org-element-property :path link))
						(cons 'image (org-element-property :path link)))))
			(when (and (derived-mode-p 'dired-mode)
								 (string-match extensions (dired-get-filename)))
				(cons 'image (dired-get-filename))))))
(with-eval-after-load 'embark
	(add-to-list 'embark-target-finders 'my-embark-image))
;; Embark and images:2 ends here



;; I want to:

;; - open images in an annotation program, maybe [[https://github.com/phase1geo/Annotator][com.github.phase1geo.annotator]]
;; - open images in [[https://krita.org/en/][Krita]]
;; - replace with latest screenshot
;; - copy text to kill ring
;; - insert text as details block


;; [[file:Sacha.org::#embark-image][Embark and images:3]]
(defun my-image-open-in-annotator (file)
	(interactive "FImage: ")
	(start-process "annotator" nil "com.github.phase1geo.annotator" (expand-file-name file)))

(defun my-image-open-in-krita (file)
	(interactive "FImage: ")
	(start-process "krita" nil "krita" "--nosplash" (expand-file-name file)))

(defun my-image-open-in-inkscape (file)
	(interactive "FImage: ")
	(start-process "inkscape" nil "inkscape" (expand-file-name file)))

(defun my-image-open-in-gimp (file)
	(interactive "FImage: ")
	(start-process "gimp" nil "gimp" (expand-file-name file)))

(defun my-open-in-firefox (file)
	(interactive "FItem: ")
	(start-process "firefox" nil "firefox" (if (string-match "^http" file) file (expand-file-name file))))

(defvar my-image-autocrop-border 10)
(defun my-image-autocrop (filename &optional border)
	(interactive "FFile: ")
	(setq border (or border my-image-autocrop-border))
	(let ((args (append '("-trim")
											(if border `("-bordercolor" "#FFFFFF" "-border" ,(number-to-string border)))
											(list "+repage" filename))))
		(apply #'call-process "mogrify" nil nil nil args)))

(defun my-image-recognize (file)
	(interactive "FFile: ")
	(let ((data (json-parse-string
							 (if (file-exists-p (concat (file-name-sans-extension file) ".json"))
									 (with-temp-buffer
										 (insert-file-contents (concat (file-name-sans-extension file) ".json"))
										 (buffer-string))
								 (with-temp-file (concat (file-name-sans-extension file) ".json")
									 (call-process "gcloud" nil t nil "ml" "vision" "detect-document" (expand-file-name file))
									 (buffer-string)))
							 :object-type 'alist)))
		(if (assoc-default 'responses data)
				(assoc-default 'text (assoc-default 'fullTextAnnotation (elt (assoc-default 'responses data) 0)))
			(assoc-default 'description (elt (assoc-default 'textAnnotations data) 0)))))

(defun my-image-recognize-get-new-filename (file)
	(interactive "FFile: ")
	(if-let* ((text (my-image-recognize file))
						(id (and (string-match "[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]-[0-9][0-9]" text)
										 (match-string 0 text)))
						(data (and id (my-journal-get-by-zidstring id))))
			(concat id " " (plist-get data :Note) "." (file-name-extension file))))

(defun my-image-recognize-and-rename (file)
	(interactive "FFile: ")
	(let ((new-name (expand-file-name (my-image-recognize-get-new-filename file)
																		(file-name-directory file))))
		(rename-file file new-name t)
		new-name))

(defun my-image-store (file &optional do-move)
	"Copy or move this image into public or private sketches as needed."
	(unless (string-match "^[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]-[0-9][0-9] " (file-name-nondirectory file))
		(setq file (my-image-recognize-and-rename file)))
	(when (string-match "^[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]-[0-9][0-9] " (file-name-nondirectory file))
		(funcall
		 (if do-move
				 'rename-file
			 'copy-file)
		 file
		 (expand-file-name
			(file-name-nondirectory file)
			(if (string-match "#private" (file-name-nondirectory file))
					my-private-sketches-directory
				(car my-sketch-directories)))
		 t)))

(defun my-image-copy-text (file)
	(interactive "FImage: ")
	(kill-new (my-image-recognize file)))

(defun my-image-insert-text-as-details (file)
	(interactive "FImage: ")
	(when (and (derived-mode-p 'org-mode)
						 (eq (org-element-type (org-element-context)) 'link))
		(goto-char (org-element-end (org-element-context))))
	(insert "\n#+begin_my_details\n" (my-image-recognize file) "\n#+end_my_details\n"))

(with-eval-after-load 'embark
	(defvar-keymap my-embark-image-actions
		:doc "Images"
		"k" #'my-image-open-in-krita
		"a" #'my-image-open-in-annotator
		"i" #'my-image-open-in-inkscape
		"w" #'my-image-copy-text
		"c" #'my-image-autocrop
		"g" #'my-image-open-in-gimp
		"f" #'my-open-in-firefox
		"s" #'my-image-store
		"r" #'my-image-recognize-and-rename
		"C" #'my-image-recolor
		"d" #'my-image-insert-text-as-details)
	(add-to-list 'embark-keymap-alist '(image . my-embark-image-actions)))
;; Embark and images:3 ends here

;; Embark and subed
;; :PROPERTIES:
;; :CUSTOM_ID: embark-subed
;; :END:

;; [[file:Sacha.org::#embark-subed][Embark and subed:1]]
(defun my-subed-set-timestamp-to-mpv-position (&optional rest)
	(interactive)
	(skip-chars-backward "0-9:,.")
	(when (looking-at "\\(\\([0-9]+\\):\\)?\\([0-9]+\\):\\([0-9]+\\)\\.\\([0-9]+\\)")
	  (replace-match (save-match-data (subed-msecs-to-timestamp subed-mpv-playback-position)) t t)))
(defun my-embark-subed-timestamp ()
	(save-excursion
	  (skip-chars-backward "0-9:,.")
	  (when (looking-at "\\(\\([0-9]+\\):\\)?\\([0-9]+\\):\\([0-9]+\\)\\.\\([0-9]+\\)")
	    (list 'subed-timestamp
						(propertize
						 (match-string 0)
						 'ms (compile-media-timestamp-to-msecs (match-string 0))
						 'position (if (bolp) 'start 'stop))))))
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

(defun my-subed-adjust-timestamp-up (offset)
	(interactive (list 100))
	(subed-mpv-jump (my-subed-adjust-timestamp (- offset))))

(defun my-subed-adjust-timestamp-down (offset)
	(interactive (list -100))
	(subed-mpv-jump (my-subed-adjust-timestamp (- offset))))

(defhydra my-subed-adjust-timestamp ()
	("<up>" my-subed-adjust-timestamp-up "Up" :exit nil)
	("<down>" my-subed-adjust-timestamp-down "Down" :exit nil))

(defun my-subed-copy-timestamp-from-previous ()
	(interactive)
	(let ((ms (save-excursion (subed-backward-subtitle-time-stop) (subed-subtitle-msecs-stop))))
	  (subed-set-subtitle-time-start ms)))
(defun my-subed-copy-timestamp-to-next ()
	(interactive)
	(let ((ms (subed-subtitle-msecs-stop)))
	  (save-excursion
	    (subed-forward-subtitle-time-stop) (subed-set-subtitle-time-start ms))))
(defun my-subed-copy-timestamp-dwim ()
	(interactive)
	(save-excursion
	  (skip-chars-backward "0-9:,.")
	  (if (bolp)
	      (my-subed-copy-timestamp-from-previous)
	    (my-subed-copy-timestamp-to-next))))
;; Embark and subed:1 ends here

;; Embark, symbols, and casual-symbol-overlay
;; :PROPERTIES:
;; :CUSTOM_ID: casual-symbol-overlay
;; :END:

;; Link: http://yummymelon.com/devnull/announcing-casual-symbol-overlay.html


;; [[file:Sacha.org::#casual-symbol-overlay][Embark, symbols, and casual-symbol-overlay:1]]
(use-package casual-symbol-overlay
	:if my-laptop-p
	:after embark
	:init
	(with-eval-after-load 'embark
		(keymap-set embark-symbol-map "z" #'casual-symbol-overlay-tmenu)))
;; Embark, symbols, and casual-symbol-overlay:1 ends here

;; [[file:Sacha.org::*Extended command list][Extended command list:2]]
;;; Mostly the same as my/read-extended-command-from-list
(defun my-read-extended-command-from-list (list)
  "Read command name to invoke in `execute-extended-command'."
  (minibuffer-with-setup-hook
      (lambda ()
        (add-hook 'post-self-insert-hook
                  (lambda ()
                    (setq execute-extended-command--last-typed
                          (minibuffer-contents)))
                  nil 'local)
        (setq-local minibuffer-default-add-function
	                  (lambda ()
	                    ;; Get a command name at point in the original buffer
	                    ;; to propose it after M-n.
	                    (let ((def (with-current-buffer
			                               (window-buffer (minibuffer-selected-window))
			                             (and (commandp (function-called-at-point))
				                                (format "%S" (function-called-at-point)))))
		                        (all (sort (minibuffer-default-add-completions)
                                       #'string<)))
		                    (if def
		                        (cons def (delete def all))
		                      all)))))
    ;; Read a string, completing from and restricting to the set of
    ;; all defined commands.  Don't provide any initial input.
    ;; Save the command read on the extended-command history list.
    (completing-read
     (concat (cond
	            ((eq current-prefix-arg '-) "- ")
	            ((and (consp current-prefix-arg)
		                (eq (car current-prefix-arg) 4)) "C-u ")
	            ((and (consp current-prefix-arg)
		                (integerp (car current-prefix-arg)))
	             (format "%d " (car current-prefix-arg)))
	            ((integerp current-prefix-arg)
	             (format "%d " current-prefix-arg)))
	           ;; This isn't strictly correct if `execute-extended-command'
	           ;; is bound to anything else (e.g. [menu]).
	           ;; It could use (key-description (this-single-command-keys)),
	           ;; but actually a prompt other than "M-x" would be confusing,
	           ;; because "M-x" is a well-known prompt to read a command
	           ;; and it serves as a shorthand for "Extended command: ".
	           "M-x ")
     (lambda (string pred action)
       (if (and suggest-key-bindings (eq action 'metadata))
	         '(metadata
	           (affixation-function . read-extended-command--affixation)
	           (category . command))
         (complete-with-action action list string pred)))
     #'commandp t nil 'extended-command-history)))

;;; Mostly the same as execute-extended-command
(defun my-execute-extended-command-from-list (prefixarg &optional command-name typed)
  ;; Based on Fexecute_extended_command in keyboard.c of Emacs.
  ;; Aaron S. Hawley <aaron.s.hawley(at)gmail.com> 2009-08-24
  "Read a command name, then read the arguments and call the command.
To pass a prefix argument to the command you are
invoking, give a prefix argument to `execute-extended-command'."
  (declare (interactive-only command-execute))
  ;; FIXME: Remember the actual text typed by the user before completion,
  ;; so that we don't later on suggest the same shortening.
  (interactive
   (let ((execute-extended-command--last-typed nil))
     (list current-prefix-arg
           (if (and command-name (listp command-name))
               (my-read-extended-command-from-list command-name)
             (read-extended-command))
           execute-extended-command--last-typed)))
  ;; Emacs<24 calling-convention was with a single `prefixarg' argument.
  (when (listp command-name)
    (let ((current-prefix-arg prefixarg) ; for prompt
          (execute-extended-command--last-typed nil))
      (setq command-name
            (if command-name
                (my/read-extended-command-from-list command-name)
              (read-extended-command)))
      (setq typed execute-extended-command--last-typed)))
  (let* ((function (and (stringp command-name) (intern-soft command-name)))
         (binding (and suggest-key-bindings
		                   (not executing-kbd-macro)
		                   (where-is-internal function overriding-local-map t))))
    (unless (commandp function)
      (error "`%s' is not a valid command name" command-name))
    ;; Some features, such as novice.el, rely on this-command-keys
    ;; including M-x COMMAND-NAME RET.
    (set--this-command-keys (concat "\M-x" (symbol-name function) "\r"))
    (setq this-command function)
    ;; Normally `real-this-command' should never be changed, but here we really
    ;; want to pretend that M-x <cmd> RET is nothing more than a "key
    ;; binding" for <cmd>, so the command the user really wanted to run is
    ;; `function' and not `execute-extended-command'.  The difference is
    ;; visible in cases such as M-x <cmd> RET and then C-x z (bug#11506).
    (setq real-this-command function)
    (let ((prefix-arg prefixarg))
      (command-execute function 'record))
    ;; If enabled, show which key runs this command.
    ;; But first wait, and skip the message if there is input.
    (let* ((waited
            ;; If this command displayed something in the echo area;
            ;; wait a few seconds, then display our suggestion message.
            ;; FIXME: Wait *after* running post-command-hook!
            ;; FIXME: If execute-extended-command--shorter were
            ;; faster, we could compute the result here first too.
            (when (and suggest-key-bindings
                       (or binding
                           (and extended-command-suggest-shorter typed)))
              (sit-for (cond
                        ((zerop (length (current-message))) 0)
                        ((numberp suggest-key-bindings) suggest-key-bindings)
                        (t 2))))))
      (when (and waited (not (consp unread-command-events)))
        (unless (or (not extended-command-suggest-shorter)
                    binding executing-kbd-macro (not (symbolp function))
                    (<= (length (symbol-name function)) 2))
          ;; There's no binding for CMD.  Let's try and find the shortest
          ;; string to use in M-x.
          ;; FIXME: Can be slow.  Cache it maybe?
          (while-no-input
            (setq binding (execute-extended-command--shorter
                           (symbol-name function) typed))))
        (when binding
          (with-temp-message
              (format-message "You can run the command `%s' with %s"
                              function
                              (if (stringp binding)
                                  (concat "M-x " binding " RET")
                                (key-description binding)))
            (sit-for (if (numberp suggest-key-bindings)
                         suggest-key-bindings
                       2))))))))
;; Extended command list:2 ends here

;; Menus
;; :PROPERTIES:
;; :CUSTOM_ID: menus
;; :END:

;; Handy when I'm in tablet mode.


;; [[file:Sacha.org::#menus][Menus:1]]
(define-key-after global-map [menu-bar my-menu] (cons "Shortcuts" (make-sparse-keymap "Custom shortcuts")) 'tools)
(define-key global-map [menu-bar my-menu journal] '("Show journal entries" . my-show-missing-journal-entries))
(define-key global-map [menu-bar my-menu agenda] '("Org agenda" . (lambda () (interactive) (org-agenda nil "a"))))
(define-key global-map [menu-bar my-menu audio] '("Process audio" . (lambda () (interactive) (shell-command "~/bin/process-audio &"))))
(define-key global-map [menu-bar my-menu new-index-card] '("New index card" . (lambda () (interactive)
                                                                                (my-org-sketch-edit (my-prepare-index-card-template)))))
;; Menus:1 ends here

;; Context menus
;; :PROPERTIES:
;; :CUSTOM_ID: context-menus
;; :END:


;; [[file:Sacha.org::#context-menus][Context menus:1]]
(add-hook 'text-mode-hook 'context-menu-mode)
(with-eval-after-load 'dired
	(add-hook 'dired-mode-hook 'context-menu-mode))
(add-hook 'shell-mode-hook 'context-menu-mode)
;; Context menus:1 ends here

;; Repeatable commands
;; :PROPERTIES:
;; :CUSTOM_ID: repeatable-commands
;; :END:

;; Based on http://oremacs.com/2015/01/14/repeatable-commands/ . Modified to
;; accept =nil= as the first value if you don't want the keymap to run a
;; command by default, and to use =kbd= for the keybinding definitions.


;; [[file:Sacha.org::#repeatable-commands][Repeatable commands:1]]
(defun my-def-rep-command (alist)
  "Return a lambda that calls the first function of ALIST.
      It sets the transient map to all functions of ALIST,
      allowing you to repeat those functions as needed."
  (let ((keymap (make-sparse-keymap))
        (func (cdar alist)))
    (mapc (lambda (x)
            (when x
              (define-key keymap (kbd (car x)) (cdr x))))
          alist)
    (lambda (arg)
      (interactive "p")
      (when func
        (funcall func arg))
      (set-transient-map keymap t))))
;; Repeatable commands:1 ends here

;; Hydra keyboard shortcuts
;; :PROPERTIES:
;; :CUSTOM_ID: hydras
;; :END:

;; package:hydra offers customizable shortcuts. package:transient is another option.


;; [[file:Sacha.org::#hydras][Hydra keyboard shortcuts:1]]
(use-package hydra :commands defhydra)
(use-package use-package-hydra)
(if my-laptop-p
    (use-package hydra-posframe
			:if my-laptop-p :after hydra
			:vc (:url "https://github.com/Ladicle/hydra-posframe")
			))
;; Hydra keyboard shortcuts:1 ends here

;; [[file:Sacha.org::#hydras][Hydra keyboard shortcuts:2]]
(with-eval-after-load 'hydra
  (defhydra my-window-movement ()
    ("<left>" windmove-left)
    ("<right>" windmove-right)
    ("<down>" windmove-down)
    ("<up>" windmove-up)
    ("y" other-window "other")
    ("h" switch-window "switch-window")
    ("b" consult-buffer "buffer")
    ("f" find-file "file")
    ("F" find-file-other-window "other file")
    ("v" (progn (split-window-right) (windmove-right)))
    ("o" delete-other-windows :color blue)
    ("a" ace-window)
    ("s" ace-swap-window)
    ("d" delete-window "delete")
    ("D" ace-delete-window "ace delete")
    ("i" ace-maximize-window "maximize")
     ("q" nil)))
;; Hydra keyboard shortcuts:2 ends here

;; [[file:Sacha.org::#hydras][Hydra keyboard shortcuts:3]]
(with-eval-after-load 'hydra
  (defhydra my-shortcuts (:exit t)
    ("j" my-helm-journal "Journal")
    ("C" my-resolve-orgzly-syncthing "Conflicts")
    ("n" my-capture-timestamped-note "Note")
    ("c" my-org-categorize-emacs-news/body "Categorize")
    ("d" my-emacs-news-check-duplicates "Dupe")
    ("s" save-buffer "Save")
    ("f" my-file-shortcuts/body "File shortcut")
    ("+" text-scale-increase "Increase")
    ("-" text-scale-decrease "Decrease")
		("G" gif-screencast-start-or-stop "GIF screencast")
    ("g" my-geeqie/body "Geeqie")
    ("r" my-record-ffmpeg-toggle-recording "Record screen")
    ("l" (my-toggle-or-create "*scratch*" (lambda () (switch-to-buffer (startup--get-buffer-create-scratch)))) "Lisp")
    ("e" eshell-toggle "Eshell")
    ("w" my-engine-dmode-hydra/body "Search web")
    ("E" my-emacs-news/body "Emacs News"))
	(keymap-global-set "<f5>" #'my-shortcuts/body)
  (defhydra my-emacs-news (:exit t)
    "Emacs News"
    ("f" (find-file "~/sync/emacs-news/index.org") "News")
    ("C" (find-file "~/proj/emacs-calendar/README.org") "Calendar")
    ("C" (find-file "/ssh:web:/var/www/emacslife.com/calendar/README.org" "Calendar on server"))
    ("d" my-emacs-news-check-duplicates "Dupe")
    ("c" my-org-categorize-emacs-news/body "Categorize")
    ("h" (my-org-update-link-description "HN") "Link HN")
    ("i" (my-org-update-link-description "Irreal") "Link Irreal")
    ("m" my-share-emacs-news "Mail")
    ("t" (browse-url "https://tweetdeck.twitter.com") "Twitter")))

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

(defun my-org-insert-link ()
  (interactive)
  (when (org-in-regexp org-link-bracket-re 1)
    (goto-char (match-end 0))
    (insert "\n"))
  (call-interactively 'org-insert-link))
;; Hydra keyboard shortcuts:3 ends here

;; [[file:Sacha.org::#hydras][Hydra keyboard shortcuts:4]]
(defun my-switch-to-previous-buffer ()
  "Switch to previously open buffer.
      Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun my-org-check-agenda ()
  "Peek at agenda."
  (interactive)
  (cond
   ((derived-mode-p 'org-agenda-mode)
    (if (window-parent) (delete-window) (bury-buffer)))
   ((get-buffer "*Org Agenda*")
    (switch-to-buffer-other-window "*Org Agenda*"))
   (t (org-agenda nil "a"))))
;; Hydra keyboard shortcuts:4 ends here



;; From https://github.com/abo-abo/hydra/wiki/Nesting-Hydras :

;; [[file:Sacha.org::#hydras][Hydra keyboard shortcuts:5]]
(defvar hydra-stack nil)

(defun my-hydra-push (expr)
  (push `(lambda () ,expr) hydra-stack))

(defun my-hydra-pop ()
  (interactive)
  (let ((x (pop hydra-stack)))
    (when x (funcall x))))

(defun my-hydra-go-and-push (expr)
  (push hydra-curr-body-fn hydra-stack)
  (prin1 hydra-stack)
  (funcall expr))

;; example (progn (hydra-b/body) (hydra-push '(hydra-a/body)))
;; or   ("q" hydra-pop "exit")
;; Hydra keyboard shortcuts:5 ends here

;; Emacs Hydra: Allow completion when I can't remember the command name
;;      :PROPERTIES:
;;      :CREATED:  [2021-04-25 Sun 21:45]
;;      :Effort:   0:30
;;      :QUANTIFIED: Emacs
;;      :EXPORT_DATE: 2021-04-25
;;      :EXPORT_MODIFIED: 2021-04-29
;;      :EXPORT_ELEVENTY_PERMALINK: /blog/2021/04/emacs-hydra-allow-completion-when-i-can-t-remember-the-command-name/
;;      :EXPORT_ELEVENTY_FILE_NAME: blog/2021/04/emacs-hydra-allow-completion-when-i-can-t-remember-the-command-name/
;;      :CUSTOM_ID: hydra-completion
;;      :END:
;;    :LOGBOOK:
;;    CLOCK: [2021-04-25 Sun 22:32]
;;    :END:
;; 2021-04-29: Added the ability to complete using an arbitrary Hydra.

;;    So it turns out that I'm pretty much zonked after a day with the
;;    kiddo and have a hard time remembering keystrokes or speed-reading
;;    [[dotemacs:hydra-lispy][my Hydra cheat sheets]]. I want to be able to use M-x-like completion
;;    in my Hydra so that I can type a few characters and then maybe see
;;    the shortcuts there. Here's what it looks like:

;; #+CAPTION: Hydra completion
;; [[https://sachachua.com/blog/2021/04/emacs-hydra-allow-completion-when-i-can-t-remember-the-command-name/Screenshot_20210425_232535.png]]


;; [[file:Sacha.org::#hydra-completion][Emacs Hydra: Allow completion when I can't remember the command name:1]]
(defun my-hydra-format-head (h)
  (let ((key-binding (elt h 0))
        (hint (elt h 2))
        (cmd (and (elt h 1) (prin1-to-string (elt h 1)))))
    (if cmd
        (format "%s (%s) - %s" hint key-binding cmd)
      (format "%s (%s)" hint key-binding))))

(defun my-hydra-heads-to-candidates (base)
  (mapcar (lambda (h)
            (cons (my-hydra-format-head h) (hydra--head-name h base)))
          (symbol-value (intern (concat (symbol-name base) "/heads")))))

(defun my-hydra-execute-extended (&optional _ hydra-base)
  (interactive (list current-prefix-arg nil))
  (hydra-keyboard-quit)
  (let* ((candidates (my-hydra-heads-to-candidates
                      (or hydra-base
                          (intern
                           (replace-regexp-in-string "/body$" ""
                                                     (symbol-name hydra-curr-body-fn))))))
         (command-name (completing-read "Cmd: " candidates))
         (bind (assoc-default command-name candidates 'string=)))
    (cond
     ((null bind) nil)
     ((hydra--callablep bind) (call-interactively bind)))))
;; Emacs Hydra: Allow completion when I can't remember the command name:1 ends here



;; This is how I add it to all my hydras:


;; [[file:Sacha.org::#hydra-completion][Emacs Hydra: Allow completion when I can't remember the command name:2]]
(with-eval-after-load 'hydra
  (define-key hydra-base-map (kbd "<tab>") #'my-hydra-execute-extended))
;; Emacs Hydra: Allow completion when I can't remember the command name:2 ends here

;; which-key and which-key-posframe
;; :PROPERTIES:
;; :CUSTOM_ID: which-key-and-which-key-posframe
;; :END:

;; It's hard to remember keyboard shortcuts.


;; [[file:Sacha.org::#which-key-and-which-key-posframe][which-key and which-key-posframe:1]]
(use-package which-key :init (which-key-mode 1))
(use-package which-key-posframe :if my-laptop-p :init (which-key-posframe-mode 1))
;; which-key and which-key-posframe:1 ends here



;; Sometimes ~C-h~ gets weird and calls ~which-key-C-h-dispatch~, probably from a transient map that got confused.


;; [[file:Sacha.org::#which-key-and-which-key-posframe][which-key and which-key-posframe:2]]
(defun my-reset-transients ()
	(interactive)
	(setq overriding-terminal-local-map nil))
;; which-key and which-key-posframe:2 ends here

;; Key chords
;; :PROPERTIES:
;; :CUSTOM_ID: key-chord
;; :END:

;; I'm on a Dvorak keyboard, so these might not work
;; for you. Experimenting with this. =key-chord= lets
;; you define keyboard shortcuts that use ordinary
;; keys typed in quick succession. I haven't been
;; using this lately, though...

;; Some code from http://emacsredux.com/blog/2013/04/28/switch-to-previous-buffer/

;; [[file:Sacha.org::#key-chord][Key chords:1]]
(defun my-key-chord-define (keymap keys command)
  "Define in KEYMAP, a key-chord of two keys in KEYS starting a COMMAND.
      \nKEYS can be a string or a vector of two elements. Currently only elements
      that corresponds to ascii codes in the range 32 to 126 can be used.
      \nCOMMAND can be an interactive function, a string, or nil.
      If COMMAND is nil, the key-chord is removed.

      MODIFICATION: Do not define the transposed key chord.
      "
  (if (/= 2 (length keys))
      (error "Key-chord keys must have two elements"))
  ;; Exotic chars in a string are >255 but define-key wants 128..255 for those
  (let ((key1 (logand 255 (aref keys 0)))
        (key2 (logand 255 (aref keys 1))))
    (define-key keymap (vector 'key-chord key1 key2) command)))
(fset 'key-chord-define 'my-key-chord-define)
;; Key chords:1 ends here



;; Now let's set up the actual keychords.


;; [[file:Sacha.org::#key-chord][Key chords:2]]
(use-package key-chord
  :if my-laptop-p
  :hydra (my-key-chord-commands
          ()
          "Main"
          ("k" kill-sexp)
          ("h" my-org-jump :color blue)
          ("x" my-org-finish-previous-task-and-clock-in-new-one "Finish and clock in" :color blue)
          ("b" helm-buffers-list :color blue)
          ("f" find-file :color blue)
          ("a" my-org-check-agenda :color blue)
          ("c" (call-interactively 'org-capture) "capture" :color blue)
          ("t" (org-capture nil "T") "Capture task")
          ("." repeat)
          ("C-t" transpose-chars)
          ("o" my-org-off-my-computer :color blue)
          ("w" my-engine-mode-hydra/body "web" :exit t)
          ("m" imenu :color blue)
          ("i" my-capture-timestamped-note-with-screenshot :exit t)
          ("n" my-capture-timestamped-note "Timestamped note" :exit t)
          ("q" quantified-track :color blue)
          ("r" my-describe-random-interactive-function)
          ("l" org-insert-last-stored-link)
          ("L" my-org-insert-link))
  :init
  (setq key-chord-one-key-delay 0.16)
  (setq key-chord-two-keys-delay 0.002)
  (key-chord-define-global "uu" 'undo)
  (key-chord-define-global "jr" 'my-goto-random-char-hydra/my-goto-random-char)
  (key-chord-define-global "kk" 'kill-whole-line)
  (key-chord-define-global "et" 'my-stream-message)
  (key-chord-define-global "em" 'embark-act)
  (key-chord-define-global ".t" 'my-stream/body)
  (key-chord-define-global "jj" 'avy-goto-word-1)
  (key-chord-define-global "yy" 'my-window-movement/body)
  (key-chord-define-global "jw" 'switch-window)
  (key-chord-define-global "jl" 'avy-goto-line)
  (key-chord-define-global "j." 'join-lines/body)
  (key-chord-define-global "FF" 'find-file)
  (key-chord-define-global "qq" 'my-quantified-hydra/body)
  (key-chord-define-global "hh" 'my-key-chord-commands/body)
  (key-chord-define-global "xx" 'er/expand-region)
  (key-chord-define-global "  " 'my-insert-space-or-expand)
  (key-chord-define-global "vv" 'god-mode-all)
  (key-chord-define-global "JJ" 'my-switch-to-previous-buffer)
  (key-chord-mode -1)) ;; disable for now
;; Key chords:2 ends here



;; Hmm, good point about =C-t= being more useful as a Hydra than as =transpose-char=. It turns out I actually do use =C-t= a fair bit, but I can always add it back as an option.


;; [[file:Sacha.org::#key-chord][Key chords:3]]
(bind-key "C-t" 'my-key-chord-commands/body)
;; Key chords:3 ends here

;; Completion
;; :PROPERTIES:
;; :CUSTOM_ID: completion
;; :END:


;; [[file:Sacha.org::#completion][Completion:1]]
(use-package vertico
	:config
	(vertico-mode +1)
	(vertico-multiform-mode)
	(with-eval-after-load 'vertico-multiform
		(add-to-list 'vertico-multiform-categories '(embark-keybinding grid))))
(use-package orderless
	:custom
	(completion-styles '(orderless basic))
	(completion-category-overrides '((file (styles basic partial-completion)))))
(use-package prescient :config (prescient-persist-mode +1))
(use-package company-prescient :init (company-prescient-mode +1))
;; Completion:1 ends here

;; Consult
;; :PROPERTIES:
;; :CUSTOM_ID: consult
;; :END:

;; [[file:Sacha.org::#consult][Consult:1]]
(use-package consult
	:load-path "~/vendor/consult"
	;:quelpa (consult :fetcher github :repo "minad/consult")
	:after projectile
	:bind (("C-x r x" . consult-register)
				 ("C-x r b" . consult-bookmark)
				 ("C-c k" . consult-kmacro)
				 ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complet-command
				 ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
				 ("C-x 5 b" . consult-buffer-other-frame)
				 ("M-#" . consult-register-load)
				 ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
				 ("C-M-#" . consult-register)
				 ("M-g o" . consult-outline)
				 ("M-g h" . consult-org-heading)
				 ("M-g a" . consult-org-agenda)
				 ("M-g m" . consult-mark)
				 ("C-x b" . consult-buffer)
				 ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
				 ("M-g o" . consult-outline)
				 ("M-g m" . consult-mark)
				 ("M-g k" . consult-global-mark)
				 ("M-g i" . consult-imenu)
				 ("M-g I" . consult-project-imenu)
				 ("M-g e" . consult-error)
				 ;; M-s bindings (search-map)
				 ("M-s f" . consult-find)
				 ("M-s i" . consult-info)
				 ("M-s L" . consult-locate)
				 ("M-s g" . consult-grep)
				 ("M-s G" . consult-git-grep)
				 ("M-s r" . consult-ripgrep)
				 ("M-s l" . consult-line)
				 ("M-s m" . consult-multi-occur)
				 ("M-s k" . consult-keep-lines)
				 ("M-s u" . consult-focus-lines)
				 ;; Isearch integration
				 ("M-s e" . consult-isearch)
				 ("M-g l" . consult-line)
				 ("M-s m" . consult-multi-occur)
				 ("C-x c o" . consult-multi-occur)
				 ("C-x c SPC" . consult-mark)
				 :map isearch-mode-map
				 ("M-e" . consult-isearch)                 ;; orig. isearch-edit-string
				 ("M-s e" . consult-isearch)               ;; orig. isearch-edit-string
				 ("M-s l" . consult-line))
	:init
	(setq register-preview-delay 0
	      register-preview-function #'consult-register-format)
	:custom
	consult-preview-key '(:debounce 0.2 any)
	consult-narrow-key "<"
	:config
	(setq consult-project-root-function #'projectile-project-root))
;; Consult:1 ends here

;; Completing blog posts
;; :PROPERTIES:
;; :CUSTOM_ID: completing-blog-posts
;; :END:


;; [[file:Sacha.org::#completing-blog-posts][Completing blog posts:1]]
(defalias 'my-complete-blog-post-url #'my-org-blog-complete)

(defun my-blog-posts ()
	(let ((json-object-type 'alist))
    (json-read-file "~/proj/static-blog/_site/blog/all/index.json")))

(defun my-blog-edit-html (url)
  (interactive (list (my-org-blog-complete)))
	(let ((base (replace-regexp-in-string
							 (concat "^" (regexp-quote my-blog-base-url))
							 ""
							 url)))
		(catch 'found
			(dolist (f (list (expand-file-name "index.html" (expand-file-name base my-11ty-base-dir))
											 (expand-file-name (replace-regexp-in-string "/?$" ".html" base) my-11ty-base-dir)))
				(when (file-exists-p f)
					(find-file f)
					(throw 'found f))))))

(defun my-view-blog-post-locally (url)
  (interactive (list (my-org-blog-complete)))
  (browse-url
	 (replace-regexp-in-string
		(concat "^" (regexp-quote my-blog-base-url))
		"https://localhost:8080/"
		url)))

(defun my-insert-blog-post-url (url)
  (interactive (list (my-complete-blog-post-url)))
  (insert url))

(defun my-insert-blog-post-link (url)
  (interactive (list (my-complete-blog-post-url)))
	(if (derived-mode-p 'org-mode)
			(let ((base )))
			(insert (org-link-make-string url

																		))
		(insert url)))
;; Completing blog posts:1 ends here

;; Completing sketches  :image:
;; :PROPERTIES:
;; :CUSTOM_ID: completing-sketches
;; :END:


;; [[file:Sacha.org::#completing-sketches][Completing sketches:1]]
(declare-function 'my-geeqie-view "Sacha.el")
(defun my-preview-image (candidate state)
  (when (and my-sketch-preview candidate) (my-geeqie-view (list candidate)))
  nil)

(defun my-complete-sketch-filename ()
  (interactive)
  (consult--read (my-sketches)
  		 :sort nil
  		 :state 'my-preview-image
  		 :prompt "Sketch: "
  		 :category 'sketch))

(defun my-date-from-filename (filename)
  (let ((f (file-name-nondirectory filename)))
    (if (string-match "^[-0-9]+" f)
        (replace-regexp-in-string "[^0-9]" "" (match-string 0 f))
      nil)))

(defvar my-sketches nil "Cache for sketch filenames.")
(defun my-sketches ()
  (interactive)
  (sort
   (apply 'append (mapcar (lambda (dir)
                            (directory-files dir t "\\.\\(jpe?g\\|png\\|svg\\)$"))
                          my-sketch-directories))
   (lambda (a b)
     (string< (concat (or (my-date-from-filename b) "0") (file-name-nondirectory b))
              (concat (or (my-date-from-filename a) "0") (file-name-nondirectory a))))))

(defvar my-sketch-preview nil "Non-nil means preview images.")
(defun my-find-sketch (file)
  (interactive (list (my-complete-sketch-filename)))
  (find-file file))

(defun my-sketch-prepare-post (file)
  (interactive (list (my-complete-sketch-filename)))
  (insert (org-link-make-string (concat "sketchFull:" (file-name-base file))))
  (let ((text (my-sketch-text file)))
    (when text
      (insert (format "\n\n#+begin_my_src \"Text from %s\"\n%s\n#")))))

(defun my-sketch-text (file)
  (setq file
       	(if (string-match ".json" file) file
       		(concat (file-name-sans-extension file) ".json")))
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (let ((json-object-type 'alist))
       	(assoc-default 'description (elt (assoc-default 'textAnnotations (json-read)) 0))))))

(defun my-sketch-insert-text-from-json (file)
  (interactive "FJSON: ")
  (let ((text (my-sketch-text file)))
    (insert (or text ""))))
;; Completing sketches:1 ends here

;; Consult directory navigation
;; :PROPERTIES:
;; :CUSTOM_ID: consult-directory-navigation
;; :END:


;; [[file:Sacha.org::#consult-directory-navigation][Consult directory navigation:1]]
(use-package consult-dir
       :ensure t
       :bind (("C-x C-d" . consult-dir)
              :map minibuffer-local-completion-map
              ("C-x C-d" . consult-dir)
              ("C-x C-j" . consult-dir-jump-file)))

;; https://karthinks.com/software/jumping-directories-in-eshell/
(defun eshell/z (&optional regexp)
  "Navigate to a previously visited directory in eshell, or to
any directory proferred by `consult-dir'."
  (let ((eshell-dirs (delete-dups
                      (mapcar 'abbreviate-file-name
                              (ring-elements eshell-last-dir-ring)))))
    (cond
     ((and (not regexp) (featurep 'consult-dir))
      (let* ((consult-dir--source-eshell `(:name "Eshell"
                                                 :narrow ?e
                                                 :category file
                                                 :face consult-file
                                                 :items ,eshell-dirs))
             (consult-dir-sources (cons consult-dir--source-eshell
                                        consult-dir-sources)))
        (eshell/cd (substring-no-properties
                    (consult-dir--pick "Switch directory: ")))))
     (t (eshell/cd (if regexp (eshell-find-previous-directory regexp)
                     (completing-read "cd: " eshell-dirs)))))))
;; Consult directory navigation:1 ends here

;; Using projects as a source for consult-buffer
;; :PROPERTIES:
;; :CUSTOM_ID: using-projects-as-a-source-for-consult-buffer
;; :END:


;; [[file:Sacha.org::#using-projects-as-a-source-for-consult-buffer][Using projects as a source for consult-buffer:1]]
(use-package consult
  :after projectile
  :defines consult-buffer-sources
  :config
  (projectile-load-known-projects)
  (setq my-consult-source-projectile-projects
        `(:name "Projectile projects"
                :narrow   ?P
                :category project
                :action   ,#'projectile-switch-project-by-name
                :items    ,projectile-known-projects))
  (add-to-list 'consult-buffer-sources my-consult-source-projectile-projects 'append))
;; Using projects as a source for consult-buffer:1 ends here

;; consult-omni  :embark:
;; :PROPERTIES:
;; :CUSTOM_ID: consult-omni
;; :END:

;; For some reason, installing [[https://github.com/armindarvish/consult-omni][consult-omni]] using the ~:vc~ keyword was giving me problems, so I checked it out from Github instead.

;; I also needed to create a Google custom search JSON API key at https://developers.google.com/custom-search/v1/introduction .


;; [[file:Sacha.org::#consult-omni][consult-omni:1]]
(defun my-insert-or-replace-link (url &optional title)
	"Insert a link, wrap the current region in a link, or replace the current link."
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
	(let ((url (and (stringp cand) (get-text-property 0 :url cand )))
				(title (and (stringp cand) (get-text-property 0 :title cand))))
		(my-insert-or-replace-link url title)))

(use-package consult-omni
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
  (setq consult-omni-default-interactive-command #'consult-omni-google)
	:bind
	(("M-g w" . consult-omni)
	 :map consult-omni-embark-general-actions-map
	 ("i l" .  #'my-consult-omni-embark-insert-link)
	 ("i u" .  #'my-consult-omni-embark-insert-url)
	 ("i t" .  #'my-consult-omni-embark-insert-title)
	 ("w u" . #'my-consult-omni-embark-copy-url-as-kill)
	 ("w t" . #'my-consult-omni-embark-copy-title-as-kill)))
;; consult-omni:1 ends here

;; Marginalia
;; :PROPERTIES:
;; :CUSTOM_ID: marginalia
;; :END:
;; [[https://www.reddit.com/r/emacs/comments/196pvtx/comment/khxa8ip/?share_id=-4IBSwNFQR_-Gd744ZcrH&utm_content=2&utm_medium=android_app&utm_name=androidcss&utm_source=share&utm_term=2][Marginalia - add function name for aliases]]


;; [[file:Sacha.org::#marginalia][Marginalia:1]]
(use-package marginalia :quelpa (marginalia :fetcher github :repo "minad/marginalia")
	:init
	(marginalia-mode)
	:bind (:map minibuffer-local-completion-map
							("M-m" . marginalia-cycle))
	:config
	(add-to-list 'marginalia-prompt-categories '("sketch" . sketch))
	(add-to-list 'marginalia-censor-variables "-api-key")
	(cl-pushnew #'marginalia-annotate-symbol-with-alias
		    (alist-get 'command marginalia-annotator-registry))
	(cl-pushnew #'marginalia-annotate-symbol-with-alias
		    (alist-get 'function marginalia-annotator-registry))
	(cl-pushnew #'marginalia-annotate-symbol-with-alias
		    (alist-get 'symbol marginalia-annotator-registry)))

(defun marginalia-annotate-alias (cand)
  "Annotate CAND with the function it aliases."
  (when-let ((sym (intern-soft cand))
             (alias (car (last (function-alias-p sym))))
             (name (and (symbolp alias) (symbol-name alias))))
    (format " (%s)" name)))

(defun marginalia-annotate-symbol-with-alias (cand)
  "Annotate symbol CAND with its documentation string.
    Similar to `marginalia-annotate-symbol'."
  (when-let (sym (intern-soft cand))
    (concat
     (marginalia-annotate-binding cand)
     (marginalia--fields
      ((marginalia-annotate-alias cand) :face 'marginalia-function)
      ((marginalia--symbol-class sym) :face 'marginalia-type)
      ((cond
        ((fboundp sym) (marginalia--function-doc sym))
        ((facep sym) (documentation-property sym 'face-documentation))
        (t (documentation-property sym 'variable-documentation)))
       :truncate 1.0 :face 'marginalia-documentation)))))
;; Marginalia:1 ends here

;; Marginalia and annotating journal entries
;; :PROPERTIES:
;; :CUSTOM_ID: marginalia-and-annotating-journal-entries
;; :END:

;; The following code annotates journal entries with their categories.


;; [[file:Sacha.org::#marginalia-and-annotating-journal-entries][Marginalia and annotating journal entries:1]]
(defun my-marginalia-annotate-journal (cand)
  (when-let ((o (cdr (assoc cand my-journal-search-cache))))
    (marginalia--fields
     ((plist-get o :Category)
:face 'marginalia-documentation
:truncate 13))))

(use-package marginalia
  :config
  (add-to-list 'marginalia-annotator-registry '(journal my-marginalia-annotate-journal builtin none)))
;; Marginalia and annotating journal entries:1 ends here

;; Cargo-culted stuff
;; :PROPERTIES:
;; :CUSTOM_ID: cargo-culted-stuff
;; :END:


;; [[file:Sacha.org::#cargo-culted-stuff][Cargo-culted stuff:1]]
(defun my-store-action-key+cmd (cmd)
  (setq keycast--this-command-keys (this-single-command-keys) keycast--this-command cmd))
(defun my-force-keycast-update (&rest _)
  (force-mode-line-update t))
(use-package keycast
  :if my-laptop-p
  :after embark
  :config (dolist (cmd '(embark-act embark-act-noexit embark-become))
            (advice-add cmd
                        :before #'my-force-keycast-update)))

(use-package
  embark
  :config
                                        ;(setq embark-prompter 'embark-completing-read-prompter)
  (advice-add 'embark-keymap-prompter :filter-return #'my-store-action-key+cmd)
  (add-to-list 'embark-target-injection-hooks '(my-stream-message embark--allow-edit)))
;; Cargo-culted stuff:1 ends here

;; color-theme sometimes comes across lists. Odd!
;; :PROPERTIES:
;; :CUSTOM_ID: color-theme-sometimes-comes-across-lists-odd
;; :END:


;; [[file:Sacha.org::#color-theme-sometimes-comes-across-lists-odd][color-theme sometimes comes across lists. Odd!:1]]
(defadvice face-attribute (around sacha activate)
  (if (symbolp (ad-get-arg 0))
      ad-do-it))
;; color-theme sometimes comes across lists. Odd!:1 ends here

;; Display
;; :PROPERTIES:
;; :CUSTOM_ID: display
;; :END:


;; [[file:Sacha.org::#display][Display:1]]
(defun sanityinc/adjust-opacity (frame incr)
  (let* ((oldalpha (or (frame-parameter frame 'alpha) 100))
         (newalpha (+ incr oldalpha)))
    (when (and (<= frame-alpha-lower-limit newalpha) (>= 100 newalpha))
      (modify-frame-parameters frame (list (cons 'alpha newalpha))))))
(keymap-global-set "C-M-8" (lambda () (interactive) (sanityinc/adjust-opacity nil -2)))
(keymap-global-set "C-M-9" (lambda () (interactive) (sanityinc/adjust-opacity nil 2)))
(keymap-global-set "C-M-0" (lambda () (interactive) (modify-frame-parameters nil `((alpha . 100)))))
;; Display:1 ends here

;; Set up a light-on-dark color scheme
;; :PROPERTIES:
;; :CUSTOM_ID: set-up-a-light-on-dark-color-scheme
;; :END:


;; [[file:Sacha.org::#set-up-a-light-on-dark-color-scheme][Set up a light-on-dark color scheme:1]]
(defun my-setup-color-theme ()
  (interactive)
  (when (display-graphic-p)
    (load-theme (car modus-themes-to-toggle))))
(use-package modus-themes
	:quelpa (modus-themes :fetcher github :repo "protesilaos/modus-themes")
	:init (setq modus-themes-to-toggle '(modus-vivendi-tinted modus-operandi-tinted))
	:config (my-setup-color-theme))
;; Set up a light-on-dark color scheme:1 ends here

;; Making highlight-sexp follow modus-themes-toggle           :elisp:emacs:
;; :PROPERTIES:
;; :EXPORT_DATE: 2023-01-26T10:25:38-0500
;; :EXPORT_ELEVENTY_PERMALINK: /blog/2023/01/making-highlight-sexp-follow-modus-themes-toggle/
;; :EXPORT_ELEVENTY_FILE_NAME: blog/2023/01/making-highlight-sexp-follow-modus-themes-toggle/
;; :CUSTOM_ID: making-highlight-sexp-follow-modus-themes-toggle
;; :END:

;; #+begin_update
;; [2023-01-27 Fri] Prot just added a [[https://github.com/protesilaos/modus-themes/commit/0ca79257ef941ff5f9ec34f5d76eed2ff35d7752][modus-themes-get-color-value]]
;; function. Yay! Also, it turns out that I need to update the overlay in
;; all the buffers.
;; #+end_update

;; I'm experimenting with using the ~highlight-sexp~ minor mode to
;; highlight my current s-expression, since I sometimes get confused
;; about what I'm modifying with smartparens. The highlight-sexp
;; background colour is hardcoded in the variable
;; ~hl-sexp-background-color~, and will probably look terrible if you use
;; a light background. I wanted it to adapt when I use
;; ~modus-themes-toggle~. Here's how that works:

;; #+CAPTION: highlight-sexp demonstration

;; [[file:Sacha.org::#making-highlight-sexp-follow-modus-themes-toggle][Making highlight-sexp follow modus-themes-toggle:1]]
(use-package highlight-sexp
  :quelpa
  (highlight-sexp :repo "daimrod/highlight-sexp" :fetcher github :version original)
	:after modus-themes
  :hook
  ((emacs-lisp-mode . highlight-sexp-mode)
	 (modus-themes-after-load-theme . my-hl-sexp-update-all-overlays))
	:config
	(defun my-hl-sexp-update-overlay ()
		(when (overlayp hl-sexp-overlay)
			(overlay-put
			 hl-sexp-overlay
			 'face
			 `(:background
				 ,(modus-themes-get-color-value 'bg-inactive)))))
	(defun my-hl-sexp-update-all-overlays (&rest args)
		(dolist (buf (buffer-list))
			(with-current-buffer buf
				(when highlight-sexp-mode
					(my-hl-sexp-update-overlay)))))
	(advice-add 'hl-sexp-create-overlay :after 'my-hl-sexp-update-overlay))
;; Making highlight-sexp follow modus-themes-toggle:1 ends here

;; Time in the modeline
;; :PROPERTIES:
;; :CUSTOM_ID: time-in-the-modeline
;; :END:

;; I like having the clock.


;; [[file:Sacha.org::#time-in-the-modeline][Time in the modeline:1]]
(display-time-mode 1)
;; Time in the modeline:1 ends here

;; Diminish mode names in modeline
;; :PROPERTIES:
;; :CUSTOM_ID: diminish
;; :END:

;; [[file:Sacha.org::#diminish][Diminish mode names in modeline:1]]
(use-package diminish :ensure t)
;; Diminish mode names in modeline:1 ends here

;; Highlight the active modeline using colours from modus-themes    :emacs:
;; :PROPERTIES:
;; :EXPORT_DATE: 2024-01-01T08:15:01-0500
;; :EXPORT_ELEVENTY_PERMALINK: /blog/2024/01/highlight-the-active-modeline-using-colours-from-modus-themes/
;; :EXPORT_ELEVENTY_FILE_NAME: blog/2024/01/highlight-the-active-modeline-using-colours-from-modus-themes/
;; :CUSTOM_ID: highlight-the-active-modeline-using-colours-from-modus-themes
;; :END:

;; I wanted to experiment with [[ https://irreal.org/blog/?p=11867#comment-6354017310][Ignacio Paz Posse's snippet]] for colouring the mode line of the active window ever so slightly different to make it easier to see where the active window is. I usually have ~global-hl-line-mode~ turned on, so that highlight is another indicator, but let's see how this tweak feels. I modified the code so that it uses the theme colours from the currently-selected Modus themes, since I trust Prot's colour choices more than I trust mine. Thanks to Irreal for sharing Ignacio's comment!


;; [[file:Sacha.org::#highlight-the-active-modeline-using-colours-from-modus-themes][Highlight the active modeline using colours from modus-themes:1]]
(defun my-update-active-mode-line-colors ()
	(set-face-attribute
	 'mode-line nil
	 :foreground (modus-themes-get-color-value 'fg-mode-line-active)
	 :background (modus-themes-get-color-value 'bg-blue-subtle)))
(use-package modus-themes
	:hook
	(modus-themes-after-load-theme . my-update-active-mode-line-colors))
;; Highlight the active modeline using colours from modus-themes:1 ends here

;; Quickly adding face properties to regions                         :emacs:
;; :PROPERTIES:
;; :CUSTOM_ID: face-text
;; :EXPORT_DATE: 2024-09-19T20:25:35-0400
;; :EXPORT_ELEVENTY_PERMALINK: /blog/2024/09/quickly-adding-face-properties-to-regions/
;; :EXPORT_ELEVENTY_FILE_NAME: blog/2024/09/quickly-adding-face-properties-to-regions/
;; :EXPORT_MODIFIED: 2024-09-20T07:38:17-0400
;; :END:

;; #+begin_update
;; - [2024-09-20 Fri]: Set the first frame of the animated GIF to a reasonable backup image.
;; - [2024-09-20]: Add ~:init-value nil~ to the mode.
;; #+end_update

;; #+CAPTION: Screencast of modifying face properties
;; [[file:/home/sacha/recordings/output-2024-09-20-13:09:17.gif]]

;; Sometimes I just want to make some text look a
;; little fancier in the buffer so that I can make a
;; thumbnail or display a message. This
;; ~my-add-face-text-property~ function lets me
;; select a region and temporarily change its height,
;; make it bold, or do other things. It will work in
;; ~text-mode~ or ~enriched-mode~ buffers (not Org
;; Mode or programming buffers like ~*scratch*~, as
;; those do a lot of font-locking).


;; [[file:Sacha.org::#face-text][Quickly adding face properties to regions:1]]
(defun my-add-face-text-property (start end attribute value)
	(interactive
	 (let ((attribute (intern
										 (completing-read
											"Attribute: "
											(mapcar (lambda (o) (symbol-name (car o)))
															face-attribute-name-alist)))))
		 (list (point)
					 (mark)
					 attribute
					 (read-face-attribute '(()) attribute))))
	(add-face-text-property start end (list attribute value)))
;; Quickly adding face properties to regions:1 ends here



;; ~enriched-mode~ has some keyboard shortcuts for
;; face attributes (~M-o b~ for bold, ~M-o i~ for
;; italic). I can add some keyboard shortcuts for
;; other properties even if they can't be saved in
;; ~text/enriched~ format.


;; [[file:Sacha.org::#face-text][Quickly adding face properties to regions:2]]
(defun my-face-text-larger (start end)
	(interactive "r")
	(add-face-text-property
	 start end
	 (list :height (floor (+ 50 (car (alist-get :height (get-text-property start 'face) '(100))))))))
(defun my-face-text-smaller (start end)
	(interactive "r")
	(add-face-text-property
	 start end
	 (list :height (floor (- (car (alist-get :height (get-text-property start 'face) '(100))) 50)))))
;; Quickly adding face properties to regions:2 ends here



;; What's an easy way to make this keyboard shortcut
;; available during the rare times I want it? I know,
;; maybe I'll make a quick minor mode so I don't
;; have to dedicate those keyboard shortcuts all the
;; time. ~repeat-mode~ lets me change the size by
;; repeating just the last keystroke.


;; [[file:Sacha.org::#face-text][Quickly adding face properties to regions:3]]
(defvar-keymap my-face-text-property-mode-map
	"M-o p" #'my-add-face-text-property
  "M-o +" #'my-face-text-larger
	"M-o -" #'my-face-text-smaller)
(define-minor-mode my-face-text-property-mode
  "Make it easy to modify face properties."
  :init-value nil
  (repeat-mode 1))
(defvar-keymap my-face-text-property-mode-repeat-map
	:repeat t
	"+" #'my-face-text-larger
	"-" #'my-face-text-smaller)
(dolist (cmd '(my-face-text-larger my-face-text-smaller))
  (put cmd 'repeat-map 'my-face-text-property-mode-repeat-map))
;; Quickly adding face properties to regions:3 ends here

;; Navigation
;; :PROPERTIES:
;; :CUSTOM_ID: navigation
;; :END:

;; [[file:Sacha.org::#navigation][Navigation:1]]
(transient-mark-mode 1)
(defun my-close-other-buffers ()
  (interactive)
  (mapc (lambda (buf)
          (unless (buffer-modified-p buf)
            (kill-buffer buf)))
        (delete (current-buffer)
                (buffer-list))))
;; Navigation:1 ends here

;; TODO Get the hang of using vundo
;;     :PROPERTIES:
;;     :CREATED:  [2024-10-14 Mon 10:40]
;;     :END:


;; [[file:Sacha.org::*Get the hang of using vundo][Get the hang of using vundo:1]]
(use-package vundo)
;; Get the hang of using vundo:1 ends here

;; Get scroll-other-window to work with PDFs

;; Thanks to [[https://www.reddit.com/r/emacs/comments/10pkhko/how_to_navigate_to_previousnext_pdf_page_in/][Reddit: How to navigate to previous/next PDF page in DocView from another window split without moving into it?]]


;; [[file:Sacha.org::*Get scroll-other-window to work with PDFs][Get scroll-other-window to work with PDFs:1]]
(use-package scroll-other-window
	:vc (:url "https://gist.github.com/politza/3f46785742e6e12ba0d1a849f853d0b9")
	:commands sow-mode
	:init (sow-mode 1))
;; Get scroll-other-window to work with PDFs:1 ends here

;; Quickly jump to positions  :embark:
;; :PROPERTIES:
;; :ID:       56f173e7-d2a2-4589-84d7-c6b435c8a5f8
;; :DRILL_LAST_INTERVAL: 0.0
;; :DRILL_REPEATS_SINCE_FAIL: 1
;; :DRILL_TOTAL_REPEATS: 3
;; :DRILL_FAILURE_COUNT: 2
;; :DRILL_AVERAGE_QUALITY: 1.667
;; :DRILL_EASE: 2.36
;; :DRILL_LAST_QUALITY: 0
;; :DRILL_LAST_REVIEWED: [2013-03-13 Wed 09:50]
;; :CUSTOM_ID: quickly-jump-to-positions
;; :END:

;; Quickly jump to a position in the current view.

;; - https://karthinks.com/software/avy-can-do-anything/
;; - https://www.reddit.com/r/emacs/comments/r6px3r/avy_can_do_anything_youre_using_avy_wrong/


;; [[file:Sacha.org::#quickly-jump-to-positions][Quickly jump to positions:1]]
(use-package avy
  :if my-laptop-p
  :config
  (defun avy-action-exchange (pt)
    "Exchange sexp at PT with the one at point."
    (set-mark pt)
    (transpose-sexps 0))

  (add-to-list 'avy-dispatch-alist '(?e . avy-action-exchange))

  (defun avy-action-embark (pt)
    (save-excursion
      (goto-char pt)
      (embark-act))
    (select-window
     (cdr (ring-ref avy-ring 0)))
    t)
  (setf (alist-get ?. avy-dispatch-alist) 'avy-action-embark)
  :bind
  ("M-j" . avy-goto-char-timer)
  )

(use-package avy-zap
  :if my-laptop-p
  :config
  (setq avy-zap-forward-only t)
  (setq avy-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s))
  :bind
  (("M-z" . avy-zap-up-to-char-dwim)
   ("M-Z" . avy-zap-to-char-dwim)))
;; Quickly jump to positions:1 ends here

;; Winner mode - undo and redo window configuration
;; :PROPERTIES:
;; :CUSTOM_ID: winner-mode-undo-and-redo-window-configuration
;; :END:

;; =winner-mode= lets you use =C-c <left>= and =C-c <right>= to switch between window configurations. This is handy when something has popped up a buffer that you want to look at briefly before returning to whatever you were working on. When you're done, press =C-c <left>=.


;; [[file:Sacha.org::#winner-mode-undo-and-redo-window-configuration][Winner mode - undo and redo window configuration:1]]
(use-package winner
  :defer t)
;; Winner mode - undo and redo window configuration:1 ends here

;; TODO Sort files in read-file-name
;; :PROPERTIES:
;; :CUSTOM_ID: sort-read-file-name
;; :END:

;; https://emacs.stackexchange.com/questions/55502/list-files-in-directory-in-reverse-order-of-date


;; [[file:Sacha.org::#sort-read-file-name][Sort files in read-file-name:1]]
(defcustom file-name-completions-sort-function #'files-sort-access-time
  "Function for sorting the completion list of file names.
The function takes the list of file names as argument
and returns the sorted list."
  :type '(choice (function :tag "Sort Function") (const :tag "Natural Order" nil))
  :group 'files)

(defun files-sort-access-time (files)
  "Sort FILES list with respect to access time."
  (sort
   files
   (lambda (fn1 fn2)
     (time-less-p
      (file-attribute-access-time (file-attributes fn2))
      (file-attribute-access-time (file-attributes fn1))))))

(defun ad-completion-file-name-table (fun string pred action)
  "Add 'display-sort-function' to metadata.
If the completion action is metadata, add
`file-name-completions-sort-function' as display-sort-function.
Otherwise call FUN with STRING, PRED and ACTION as arguments."
  (if (and (functionp file-name-completions-sort-function)
       (eq action 'metadata))
      (list 'metadata
        '(category . file)
        (cons 'display-sort-function file-name-completions-sort-function))
    (funcall fun string pred action)))

(advice-add 'completion-file-name-table :around #'ad-completion-file-name-table)
;; Sort files in read-file-name:1 ends here

;; Downloaded files


;; [[file:Sacha.org::*Downloaded files][Downloaded files:1]]
(defvar my-download-dir "~/Downloads")
(defun my-open-latest-download ()
  (interactive)
  (find-file (my-latest-file my-download-dir)))
(defun my-attach-and-link-latest-download ()
  (interactive)
  (org-attach-attach (my-latest-file my-download-dir) nil 'cp)
  (org-insert-link nil (caar org-stored-links)))
(defun my-copy-latest-download (dest &optional force)
  (interactive "FDestination: ")
  (copy-file (my-latest-file my-download-dir) dest force))
(defun my-download-dired ()
	(interactive)
	(dired my-download-dir "-lt"))
;; Downloaded files:1 ends here

;; Searching
;; :PROPERTIES:
;; :CUSTOM_ID: searching
;; :END:

;; I should get the hang of using =helm-org-rifle= and =ripgrep=.


;; [[file:Sacha.org::#searching][Searching:1]]
(defun my-helm-org-rifle-org-directory ()
  (interactive)
  (helm-org-rifle-directories (list org-directory) t))
(use-package helm-org-rifle
  :bind
  ("M-g r r" . helm-org-rifle)
  ("M-g r a" . helm-org-rifle-org-agenda-files)
  ("M-g r o" . helm-org-rifle-org-directory)
  )
(defun my-consult-recoll-without-emacs-news ()
  (interactive)
  (consult-recoll--open (consult-recoll--search "-\"Emacs News\" ")))
(use-package consult-recoll
  :config
  (setq consult-recoll-search-flags nil)
  :bind
  ("M-s S" . consult-recoll))
;; Searching:1 ends here

;; Deleting things
;; :PROPERTIES:
;; :CUSTOM_ID: deleting-things
;; :END:

;; From Steve Purcell, who linked to http://www.emacswiki.org/emacs/ZapToISearch

;; [[file:Sacha.org::#deleting-things][Deleting things:1]]
(defun zap-to-isearch (rbeg rend)
  "Kill the region between the mark and the closest portion of
      the isearch match string. The behaviour is meant to be analogous
      to zap-to-char; let's call it zap-to-isearch. The deleted region
      does not include the isearch word. This is meant to be bound only
      in isearch mode.  The point of this function is that oftentimes
      you want to delete some portion of text, one end of which happens
      to be an active isearch word. The observation to make is that if
      you use isearch a lot to move the cursor around (as you should,
      it is much more efficient than using the arrows), it happens a
      lot that you could just delete the active region between the mark
      and the point, not include the isearch word."
  (interactive "r")
  (when (not mark-active)
    (error "Mark is not active"))
  (let* ((isearch-bounds (list isearch-other-end (point)))
         (ismin (apply 'min isearch-bounds))
         (ismax (apply 'max isearch-bounds))
         )
    (if (< (mark) ismin)
        (kill-region (mark) ismin)
      (if (> (mark) ismax)
          (kill-region ismax (mark))
        (error "Internal error in isearch kill function.")))
    (isearch-exit)
    ))

(define-key isearch-mode-map [(meta z)] 'zap-to-isearch)
;; Deleting things:1 ends here

;; Transient for isearch

;; From https://github.com/kickingvegas/cclisp/blob/fae13b5adb6cb667af23070d000f9bd91b6ba3d8/cc-isearch-menu.el#L96


;; [[file:Sacha.org::*Transient for isearch][Transient for isearch:1]]
(require 'transient)
(transient-define-prefix cc/isearch-menu ()
  "isearch Menu"
  [["Edit Search String"
    ("e"
     "Edit the search string (recursive)"
     isearch-edit-string
     :transient nil)
    ("w"
     "Pull next word or character word from buffer"
     isearch-yank-word-or-char
     :transient nil)
    ("s"
     "Pull next symbol or character from buffer"
     isearch-yank-symbol-or-char
     :transient nil)
    ("l"
     "Pull rest of line from buffer"
     isearch-yank-line
     :transient nil)
    ("y"
     "Pull string from kill ring"
     isearch-yank-kill
     :transient nil)
    ("t"
     "Pull thing from buffer"
     isearch-forward-thing-at-point
     :transient nil)]

   ["Replace"
    ("q"
     "Start ‘query-replace’"
     isearch-query-replace
     :if-nil buffer-read-only
     :transient nil)
    ("x"
     "Start ‘query-replace-regexp’"
     isearch-query-replace-regexp
     :if-nil buffer-read-only
     :transient nil)]]

  [["Toggle"
    ("X"
     "Regexp searching"
     isearch-toggle-regexp
     :transient nil)
    ("S"
     "Symbol searching"
     isearch-toggle-symbol
     :transient nil)
    ("W"
     "Word searching"
     isearch-toggle-word
     :transient nil)
    ("F"
     "Case fold"
     isearch-toggle-case-fold
     :transient nil)
    ("L"
     "Lax whitespace"
     isearch-toggle-lax-whitespace
     :transient nil)]

   ["Misc"
    ("o"
     "occur"
     isearch-occur
     :transient nil)
    ("h"
     "highlight"
     isearch-highlight-regexp
     :transient nil)
    ("H"
     "highlight lines"
     isearch-highlight-lines-matching-regexp
     :transient nil)]])

(define-key isearch-mode-map (kbd "M-S") 'cc/isearch-menu)
;; Transient for isearch:1 ends here

;; Search invisible text


;; [[file:Sacha.org::*Search invisible text][Search invisible text:1]]
(setq isearch-invisible t
			search-invisible t)
;; Search invisible text:1 ends here

;; Occur

;; From https://emacs.ch/@bram85/111724372485640053:

;; [[file:Sacha.org::*Occur][Occur:1]]
(with-eval-after-load 'occur
	(keymap-set occur-mode-map "C-x C-q" #'occur-edit-mode))
;; Occur:1 ends here

;; Ediff
;; :PROPERTIES:
;; :CUSTOM_ID: ediff
;; :END:

;; http://yummymelon.com/devnull/surprise-and-emacs-defaults.html

;; [[file:Sacha.org::#ediff][Ediff:1]]
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(defvar my-ediff-last-windows nil)

(defun my-store-pre-ediff-winconfig ()
"Store `current-window-configuration' in variable `my-ediff-last-windows'."
(setq my-ediff-last-windows (current-window-configuration)))

(defun my-restore-pre-ediff-winconfig ()
"Restore window configuration to stored value in `my-ediff-last-windows'."
(set-window-configuration my-ediff-last-windows))

(add-hook 'ediff-before-setup-hook #'my-store-pre-ediff-winconfig)
(add-hook 'ediff-quit-hook #'my-restore-pre-ediff-winconfig)
;; Ediff:1 ends here

;; Hideshow
;; :PROPERTIES:
;; :CUSTOM_ID: hideshow
;; :END:

;; From https://karthinks.com/software/simple-folding-with-hideshow/ :


;; [[file:Sacha.org::#hideshow][Hideshow:1]]
(use-package hideshow
  :hook
  (prog-mode . hs-minor-mode)
  :bind
  ("C-<tab>" . hs-cycle)
  ("C-<iso-lefttab>" . hs-global-cycle)
  ("C-S-<tab>" . hs-global-cycle))
(defun hs-cycle (&optional level)
  (interactive "p")
  (let (message-log-max
        (inhibit-message t))
    (if (= level 1)
        (pcase last-command
          ('hs-cycle
           (hs-hide-level 1)
           (setq this-command 'hs-cycle-children))
          ('hs-cycle-children
           ;; TODO: Fix this case. `hs-show-block' needs to be
           ;; called twice to open all folds of the parent
           ;; block.
           (save-excursion (hs-show-block))
           (hs-show-block)
           (setq this-command 'hs-cycle-subtree))
          ('hs-cycle-subtree
           (hs-hide-block))
          (_
           (if (not (hs-already-hidden-p))
               (hs-hide-block)
             (hs-hide-level 1)
             (setq this-command 'hs-cycle-children))))
      (hs-hide-level level)
      (setq this-command 'hs-hide-level))))

(defun hs-global-cycle ()
    (interactive)
    (pcase last-command
      ('hs-global-cycle
       (save-excursion (hs-show-all))
       (setq this-command 'hs-global-show))
      (_ (hs-hide-all))))
;; Hideshow:1 ends here

;; Pop to mark
;; :PROPERTIES:
;; :CUSTOM_ID: pop-to-mark
;; :END:

;; Handy way of getting back to previous places.


;; [[file:Sacha.org::#pop-to-mark][Pop to mark:1]]
(bind-key "C-x p" 'pop-to-mark-command)
(setq set-mark-command-repeat-pop t)
;; Pop to mark:1 ends here

;; Helm-swoop - quickly finding lines
;; :PROPERTIES:
;; :CUSTOM_ID: helm-swoop-quickly-finding-lines
;; :END:

;; This promises to be a fast way to find things. Let's bind it to =Ctrl-Shift-S= to see if I can get used to that...


;; [[file:Sacha.org::#helm-swoop-quickly-finding-lines][Helm-swoop - quickly finding lines:1]]
(use-package helm-swoop
  :if my-laptop-p
  :bind
  (("C-S-s" . helm-swoop)
   ("M-i" . helm-swoop)
   ("M-s M-s" . helm-swoop)
   ("M-I" . helm-swoop-back-to-last-point)
   ("C-c M-i" . helm-multi-swoop)
   ("C-x M-i" . helm-multi-swoop-all)
   )
  :config
  (progn
    (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
    (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop))
  )
;; Helm-swoop - quickly finding lines:1 ends here

;; Highlight the current line while still being able to easily customize/describe underlying faces
;; :PROPERTIES:
;; :CUSTOM_ID: highlight-line-mode
;; :EXPORT_DATE: 2024-09-17T12:14:19-0400
;; :EXPORT_ELEVENTY_PERMALINK: /blog/2024/09/highlight-the-current-line-while-still-being-able-to-easily-customize-describe-underlying-faces/
;; :EXPORT_ELEVENTY_FILE_NAME: blog/2024/09/highlight-the-current-line-while-still-being-able-to-easily-customize-describe-underlying-faces/
;; :END:

;; I use ~global-hl-line-mode~ to highlight the current line.


;; [[file:Sacha.org::#highlight-line-mode][Highlight the current line while still being able to easily customize/describe underlying faces:1]]
(global-hl-line-mode 1)
;; Highlight the current line while still being able to easily customize/describe underlying faces:1 ends here



;; However, I don't want ~hl-line~ to interfere with the default face suggested by ~customize-face~, which is returned by ~face-at-point~.


;; [[file:Sacha.org::#highlight-line-mode][Highlight the current line while still being able to easily customize/describe underlying faces:2]]
(defun my-suggest-other-faces (func &rest args)
	(if global-hl-line-mode
			(progn
				(global-hl-line-mode -1)
				(prog1 (apply func args)
					(global-hl-line-mode 1)))
		(apply func args)))
(advice-add #'face-at-point :around #'my-suggest-other-faces)
;; Highlight the current line while still being able to easily customize/describe underlying faces:2 ends here

;; Windmove - switching between windows
;; :PROPERTIES:
;; :CUSTOM_ID: windmove-switching-between-windows
;; :END:

;; Windmove lets you move between windows with something more natural than cycling through =C-x o= (=other-window=).
;; Windmove doesn't behave well with Org, so we need to use different keybindings.


;; [[file:Sacha.org::#windmove-switching-between-windows][Windmove - switching between windows:1]]
(use-package windmove
  :bind
  (("<f2> <right>" . windmove-right)
   ("<f2> <left>" . windmove-left)
   ("<f2> <up>" . windmove-up)
   ("<f2> <down>" . windmove-down)
   ))
;; Windmove - switching between windows:1 ends here

;; Frequently-accessed files
;; :PROPERTIES:
;; :CUSTOM_ID: frequently-accessed-files
;; :END:
;; Registers allow you to jump to a file or other location quickly. To
;; jump to a register, use =C-x r j= followed by the letter of the
;; register. Using registers for all these file shortcuts is probably a bit of a waste since I can easily define my own keymap, but since I rarely go beyond register A anyway. Also, I might as well add shortcuts for refiling.


;; [[file:Sacha.org::#frequently-accessed-files][Frequently-accessed files:1]]
(setq bookmark-watch-bookmark-file 'silent)
(defvar my-refile-map (make-sparse-keymap))
(require 'bookmark)
(defmacro my-defshortcut (key file)
  `(progn
     (set-register ,key (cons 'file ,file))
     (bookmark-store ,file (list (cons 'filename ,file)
                                 (cons 'position 1)
                                 (cons 'front-context-string "")) nil)
     (define-key my-refile-map
       (char-to-string ,key)
       (lambda (prefix)
         (interactive "p")
         (let ((org-refile-targets '(((,file) :maxlevel . 6)))
               (current-prefix-arg (or current-prefix-arg '(4))))
           (call-interactively 'org-refile))))))


(define-key my-refile-map "," 'my-org-refile-to-previous-in-file)

(defmacro defshortcuts (name body &optional docstring &rest heads)
  (declare (indent defun) (doc-string 3))
  (cond ((stringp docstring))
        (t
         (setq heads (cons docstring heads))
         (setq docstring "")))
  (list
   'progn
   (append `(defhydra ,name (:exit t))
           (mapcar (lambda (h)
                     (list (elt h 0) (list 'find-file (elt h 1)) (elt h 2)))
                   heads))
   (cons 'progn
         (mapcar (lambda (h) (list 'my-defshortcut (string-to-char (elt h 0)) (elt h 1)))
                 heads))))

(defmacro defshortcuts+ (name body &optional docstring &rest heads)
  (declare (indent defun) (doc-string 3))
  (cond ((stringp docstring))
        (t
         (setq heads (cons docstring heads))
         (setq docstring "")))
  (list
   'progn
   (append `(defhydra+ ,name (:exit t))
           (mapcar (lambda (h)
                     (list (elt h 0) (list 'find-file (elt h 1)) (elt h 2)))
                   heads))
   (cons 'progn
         (mapcar (lambda (h) (list 'my-defshortcut (string-to-char (elt h 0)) (elt h 1)))
                 heads))))

(use-package hydra
  :config
  (defshortcuts my-file-shortcuts ()
    ("C" "~/proj/emacs-calendar/README.org" "Emacs calendar")
    ("e" "~/sync/emacs/Sacha.org" "Config")
    ("E" "~/sync/emacs-news/index.org" "Emacs News")
    ("f" "~/proj/font/README.org" "Font")
    ("I" "~/sync/orgzly/computer-inbox.org" "Computer inbox")
    ("i" "~/sync/orgzly/Inbox.org" "Phone inbox")
    ("o" "~/sync/orgzly/organizer.org" "Main org file")
    ("s" "~/proj/stream/index.org" "Yay Emacs")
    ("b" "~/sync/orgzly/business.org" "Business")
    ("P" "/scp:web:/mnt/prev/home/sacha/planet/en.ini" "Planet Emacsen")
    ("p" "~/sync/orgzly/posts.org" "Posts")
    ("m" "~/sync/web/beginner-map.org" "Map")
    ("n" "/ssh:web|sudo::/etc/nginx/sites-available" "Nginx sites")
    ("w" "~/Dropbox/public/sharing/index.org" "Sharing index")
    ("W" "~/Dropbox/public/sharing/blog.org" "Blog index")
    ("1" "~/proj/static-blog/" "Static blog")
    ("r" "~/sync/orgzly/reference.org" "Reference")
    ("R" "~/personal/reviews.org" "Reviews")
    ("g" "~/proj/sachac.github.io/evil-plans/index.org" "Evil plans"))
  :bind
  ("C-c f" . #'my-file-shortcuts/body))
;; Frequently-accessed files:1 ends here

;; Smartscan
;; :PROPERTIES:
;; :CUSTOM_ID: smartscan
;; :END:

;; From https://github.com/itsjeyd/emacs-config/blob/emacs24/init.el, this makes =M-n= and =M-p= look for the symbol at point.

;; [[file:Sacha.org::#smartscan][Smartscan:1]]
(use-package smartscan
  :if my-laptop-p
  :defer t
  :config (global-smartscan-mode t))
;; Smartscan:1 ends here

;; Dired
;; :PROPERTIES:
;; :CUSTOM_ID: dired
;; :END:


;; [[file:Sacha.org::#dired][Dired:1]]
(setq dired-listing-switches "-altr")
(setq dired-dwim-target 'dired-dwim-target-next)
;; Dired:1 ends here



;; From http://www.masteringemacs.org/articles/2011/03/25/working-multiple-files-dired/


;; [[file:Sacha.org::#dired][Dired:2]]
(require 'find-dired)
(setq find-ls-option '("-print0 | xargs -0 ls -ld" . "-ld"))
;; Dired:2 ends here

;; Saving photos
;; :PROPERTIES:
;; :CUSTOM_ID: saving-photos
;; :END:


;; [[file:Sacha.org::#saving-photos][Saving photos:1]]
(defun my-save-photo (name)
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
      (copy-file file (expand-file-name new-name my-kid-photo-directory)))
    (rename-file file (expand-file-name new-name "~/archives/2016/photos/selected/"))))
(defun my-backup-media ()
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
(bind-key "b" 'my-save-photo dired-mode-map)
(bind-key "r" 'my-backup-media dired-mode-map)
;; Saving photos:1 ends here

;; Move to beginning of line
;; :PROPERTIES:
;; :CUSTOM_ID: move-to-beginning-of-line
;; :END:
;; Copied from http://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/


;; [[file:Sacha.org::#move-to-beginning-of-line][Move to beginning of line:1]]
(defun my-smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

      Move point to the first non-whitespace character on this line.
      If point is already there, move to the beginning of the line.
      Effectively toggle between the first non-whitespace character and
      the beginning of the line.

      If ARG is not nil or 1, move forward ARG - 1 lines first.  If
      point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'my-smarter-move-beginning-of-line)
;; Move to beginning of line:1 ends here

;; Recent files
;; :PROPERTIES:
;; :CUSTOM_ID: recent-files
;; :END:


;; [[file:Sacha.org::#recent-files][Recent files:1]]
(require 'recentf)
(setq recentf-max-saved-items 200
      recentf-max-menu-items 15)
(recentf-mode)
;; Recent files:1 ends here

;; Copy filename to clipboard
;; :PROPERTIES:
;; :CUSTOM_ID: copy-filename-to-clipboard
;; :END:

;; http://emacsredux.com/blog/2013/03/27/copy-filename-to-the-clipboard/
;; https://github.com/bbatsov/prelude


;; [[file:Sacha.org::#copy-filename-to-clipboard][Copy filename to clipboard:1]]
(defun prelude-copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))
;; Copy filename to clipboard:1 ends here

;; Open files externally
;; :PROPERTIES:
;; :CUSTOM_ID: open-files-externally
;; :END:

;; Copied from Prelude: http://emacsredux.com/blog/2013/03/27/open-file-in-external-program/


;; [[file:Sacha.org::#open-files-externally][Open files externally:1]]
(defun prelude-open-with (arg)
  "Open visited file in default external program.

      With a prefix ARG always prompt for command to use."
  (interactive "P")
  (when buffer-file-name
    (shell-command (concat
                    (cond
                     ((and (not arg) (eq system-type 'darwin)) "open")
                     ((and (not arg) (member system-type '(gnu gnu/linux gnu/kfreebsd))) "xdg-open")
                     (t (read-shell-command "Open current file with: ")))
                    " "
                    (shell-quote-argument buffer-file-name)))))
;; Open files externally:1 ends here

;; Toggle
;; :PROPERTIES:
;; :CUSTOM_ID: toggle
;; :END:

;;     Based on https://www.reddit.com/r/emacs/comments/l4v1ux/one_of_the_most_useful_small_lisp_functions_in_my-


;; [[file:Sacha.org::#toggle][Toggle:1]]
(defun my-toggle-or-create (buffer-name buffer-create-fn &optional switch-cont)
  (interactive)
  (let ((target-buf (get-buffer buffer-name)))
    (prin1 target-buf)
    (cond
     ((equal (current-buffer) target-buf) (switch-to-buffer nil))
     (target-buf
      (switch-to-buffer target-buf)
      (if switch-cont (funcall switch-cont)))
     (t (funcall buffer-create-fn)
        (if switch-cont (funcall switch-cont))))))
;; Toggle:1 ends here

;; link-hint
;; :PROPERTIES:
;; :CUSTOM_ID: link-hint
;; :END:

;; This should make it easier to jump to a link.


;; [[file:Sacha.org::#link-hint][link-hint:1]]
(use-package link-hint
  :bind
  ("M-g u" . link-hint-open-link)
  ("M-g U" . link-hint-open-multiple-links))
;; link-hint:1 ends here

;; Bookmarks
;; :PROPERTIES:
;; :CUSTOM_ID: bookmarks
;; :END:
;; http://yummymelon.com/devnull/using-bookmarks-in-emacs-like-you-do-in-web-browsers.html


;; [[file:Sacha.org::#bookmarks][Bookmarks:1]]
(easy-menu-define cc/bookmarks-menu nil
  "Keymap for CC Bookmarks Menu"
  '("Bookmarks"
    ["Edit Bookmarks" list-bookmarks
     :help "Display a list of existing bookmarks."]
    ["--" nil]
    ["Add Bookmark…" bookmark-set-no-overwrite
     :help "Set a bookmark named NAME at the current location."]
    ["---" nil]
    ["Jump to Bookmark…" bookmark-jump
     :help "Jump to bookmark"]))
(easy-menu-add-item global-map '(menu-bar)
                    cc/bookmarks-menu
                    "Tools")
(defhydra+ my-shortcuts (:exit t)
	("b" bookmark-jump "Jump to bookmark")
	("B" bookmark-set-no-overwrite "Set bookmark"))
;; Bookmarks:1 ends here

;; Dogears
;; :PROPERTIES:
;; :CUSTOM_ID: dogears
;; :END:

;;     https://github.com/alphapapa/dogears.el


;; [[file:Sacha.org::#dogears][Dogears:1]]
;; Install and load `quelpa-use-package'.
(use-package dogears
  ;; :quelpa (dogears :fetcher github :repo "alphapapa/dogears.el")

  ;; These bindings are optional, of course:
  :bind (:map global-map
              ("M-g d" . dogears-go)
              ("M-g M-b" . dogears-back)
              ("M-g M-f" . dogears-forward)
              ("M-g M-d" . dogears-list)
              ("M-g M-D" . dogears-sidebar)))
;; Dogears:1 ends here

;; Randomness for serendipity
;; :PROPERTIES:
;; :CUSTOM_ID: random
;; :END:


;; [[file:Sacha.org::#random][Randomness for serendipity:1]]
(defun my-goto-random-char ()
  (interactive)
  (goto-char (random (point-max))))
;; Randomness for serendipity:1 ends here

;; Building a today-I-learned habit, and displaying the documentation for random Emacs commands :emacs:
;; :PROPERTIES:
;; :ID:       o2b:f3c021e8-8b7a-4bd2-a035-3de1eaa206a2
;; :POST_DATE: [2016-02-19 Fri 17:11]
;; :POSTID:   28623
;; :BLOG:     sacha
;; :CUSTOM_ID: building-a-today-i-learned-habit-and-displaying-the-documentation-for-random-emacs-commands
;; :END:

;; I'd like to build a habit of regularly learning one small thing each
;; day in one of three domains: tech, life, and learning. My measurable
;; output would probably be in the form of index cards, tweets, blog
;; posts, and notes (in org-capture, Dropbox, or Evernote). I can get
;; input from various sources like blog posts, videos, books, webpages,
;; and so on.

;; A little bit of randomness might be useful for learning more about
;; Emacs. Emacswiki has a [[http://www.emacswiki.org/emacs?action=random][random page]] function, but the chunks are often
;; a little large or irrelevant. On the other hand, displaying a random
;; command from the packages that I already have loaded into my Emacs -
;; that might be a good way to discover interesting things.

;; I started by looking at =apropos-command=, which led me to
;; =apropos-internal=, which is a C function that referred to =obarray=.
;; Using =obarray= by itself didn't work (suspiciously few elements, so I
;; often ended up looking at emms-related functions). I eventually found
;; [[http://www.gnu.org/software/emacs/manual/html_node/elisp/Creating-Symbols.html][mapatoms]], which seems to do a better job at listing an appreciable
;; number of interactive functions. I filtered the list to include only
;; documented functions that had not been marked as obsolete: 8,415 in
;; my current Emacs, which should be plenty to go through. =)


;; [[file:Sacha.org::#building-a-today-i-learned-habit-and-displaying-the-documentation-for-random-emacs-commands][Building a today-I-learned habit, and displaying the documentation for random Emacs commands:1]]
(defun my-describe-random-interactive-function ()
  (interactive)
  "Show the documentation for a random interactive function.
     Consider only documented, non-obsolete functions."
  (let (result)
    (mapatoms
     (lambda (s)
       (when (and (commandp s)
                  (documentation s t)
                  (null (get s 'byte-obsolete-info)))
         (setq result (cons s result)))))
    (describe-function (elt result (random (length result))))))
;; Building a today-I-learned habit, and displaying the documentation for random Emacs commands:1 ends here

;; Shuffling lines
;; :PROPERTIES:
;; :CUSTOM_ID: shuffling-lines
;; :END:


;; [[file:Sacha.org::#shuffling-lines][Shuffling lines:1]]
(defun my-shuffle-lines-in-region (beg end)
  (interactive "r")
  (let ((list (split-string (buffer-substring beg end) "[\r\n]+")))
    (delete-region beg end)
    (insert (string-join (seq-sort-by (lambda (_) (random)) #'<= list) "\n"))))
;; Shuffling lines:1 ends here

;; Network: TRAMP and editing files over SSH
;; :PROPERTIES:
;; :CUSTOM_ID: network-tramp-and-editing-files-over-ssh
;; :END:
;; Emacs lets you edit files on remote servers, which is pretty darn
;; cool. On Windows, these things help a little.


;; [[file:Sacha.org::#network-tramp-and-editing-files-over-ssh][Network: TRAMP and editing files over SSH:1]]
(when (eq system-type 'windows-nt)
  (setq tramp-default-method "plink")
  (setq tramp-auto-save-directory "c:\\sacha\\tmp"))
;; Network: TRAMP and editing files over SSH:1 ends here

;; Touch gestures
;; :PROPERTIES:
;; :CUSTOM_ID: touch
;; :END:
;; [2024-02-01 Thu]
;; From https://kitchingroup.cheme.cmu.edu/blog/2014/08/31/Using-Mac-gestures-in-Emacs/

;; [[file:Sacha.org::#touch][Touch gestures:1]]
(defvar *my-previous-buffer* t
  "can we switch?")

(defun my-previous-buffer ()
  (interactive)
  (message "custom prev: *my-previous-buffer*=%s" *my-previous-buffer*)
  (when *my-previous-buffer*
    (previous-buffer)
    (setq *my-previous-buffer* nil)
    (run-at-time "1 sec" nil (lambda ()
                               (setq *my-previous-buffer* t)))))

(defvar *my-next-buffer* t
  "can we switch?")

(defun my-next-buffer ()
  (interactive)
  (message "custom prev: *my-next-buffer*=%s" *my-next-buffer*)
  (when *my-next-buffer*
    (next-buffer)
    (setq *my-next-buffer* nil)
    (run-at-time "1 sec" nil (lambda ()
                               (setq *my-next-buffer* t)))))

(keymap-global-set "<triple-wheel-right>" 'my-previous-buffer)
(keymap-global-set "<triple-wheel-left>" 'my-next-buffer)
;; Touch gestures:1 ends here

;; Reading
;; :PROPERTIES:
;; :CUSTOM_ID: reading
;; :END:

;; https://github.com/xahlee/xah_emacs_init/blob/master/xah_emacs_font.el
;; From Xah Lee:


;; [[file:Sacha.org::#reading][Reading:1]]
(defun xah-toggle-margin-right ()
  "Toggle the right margin between `fill-column' or window width.
     This command is convenient when reading novel, documentation."
  (interactive)
  (if (eq (cdr (window-margins)) nil)
      (set-window-margins nil 0 (- (window-body-width) fill-column))
    (set-window-margins nil 0 0)))
;; Reading:1 ends here

;; [[file:Sacha.org::#reading][Reading:2]]
(use-package pdf-tools
  :if my-laptop-p
  :config
  (pdf-tools-install)
  (setq pdf-view-resize-factor 1.1)
  (setq-default pdf-view-display-size 'fit-page)
  )
;; Reading:2 ends here

;; Writing and editing
;; :PROPERTIES:
;; :CUSTOM_ID: writing-and-editing
;; :END:


;; [[file:Sacha.org::#writing-and-editing][Writing and editing:1]]
(keymap-global-set "M-c" #'capitalize-dwim)
(setq-default fill-column 50)
;; Writing and editing:1 ends here

;; gif-screencast
;; :PROPERTIES:
;; :CUSTOM_ID: gif-screencast
;; :END:

;; Animated GIFs make it easy to demonstrate things,
;; and [[package:gif-screencast][gif-screencast]] makes it easy to record a
;; screencast with each command getting its own GIF
;; frame. In my config, I have ~s-s~ (super-s, or the
;; Windows key + s) bound to
;; ~gif-screencast-start-or-stop~. I like adding
;; animated GIFs to my blog posts and videos.

;; Not all interfaces can display animated GIFs. For
;; example, some people read my blog in [[package:elfeed][elfeed]] or in
;; a console Emacs and can see only the first frame
;; of a GIF. It makes sense to select the a frame to
;; use as the fallback for the animated GIF, much
;; like videos can have thumbnails.

;; When I stop gif-screencast, I want to pick a frame
;; and use it as the first frame. Let's see if this works...


;; [[file:Sacha.org::#gif-screencast][gif-screencast:1]]
(defun my-gif-screencast-start-or-stop-and-choose-thumbnail ()
	"Start a screencast or pause recording."
	(interactive)
	(if gif-screencast-mode
			(progn
				(gif-screencast-toggle-pause)
				(dired gif-screencast-screenshot-directory)
				(revert-buffer)
				(dired gif-screencast-screenshot-directory)
				(image-dired gif-screencast-screenshot-directory))
		(gif-screencast)))
;; gif-screencast:1 ends here



;; Actually, what I want to do is a more general
;; case: be able to delete frames, copy frames to the
;; beginning, eventually move frames around, etc.,
;; and then generate the GIF based on the contents of
;; that directory.


;; [[file:Sacha.org::#gif-screencast][gif-screencast:2]]
(defun my-gif-screencast-copy-image-to-first-frame (file)
	(interactive (list (dired-get-filename)))
  ;; Determine the timestamp of the first file in this directory
	(copy-file
	 file
	 (expand-file-name
		(format-time-string
		 "screen-%F-%T-%3N.png"
		 (time-subtract
			(my-gif-screencast-timestamp-from-filename
			 (car (directory-files gif-screencast-screenshot-directory nil ".png")))
			(seconds-to-time 0.001)))
		gif-screencast-screenshot-directory)))

(defun my-gif-screencast-timestamp-from-filename (file)
	(setq file (replace-regexp-in-string "^screen-" "" (file-name-base file)))
	(time-add (date-to-time (format "%s %s" (substring file 0 10) (substring file 11 19)))
						(float-time (/ (string-to-number (substring file 20 23)) 1000.0))))
(cl-assert
 (string= (format-time-string "test-%F-%T-%3N" (my-gif-screencast-timestamp-from-filename "screen-2024-09-20-13:18:08-024.png"))
					"test-2024-09-20-13:18:08-024"))

(defun my-gif-screencast-update-frames-from-directory ()
	(interactive)
	(let* ((files (directory-files gif-screencast-screenshot-directory nil ".png"))
				 (start-time (my-gif-screencast-timestamp-from-filename (car files))))
		(setq gif-screencast--frames
					(mapcar (lambda (o)
										(make-gif-screencast-frame
										 :timestamp (my-gif-screencast-timestamp-from-filename o)
										 :filename o))
									files))
		(gif-screencast-mode 0)
		(gif-screencast--finish)))
;; gif-screencast:2 ends here



;; Someday it could be pretty cool to have an SVG where I can see the frames on a timeline and then drag them around. In the meantime, let's see how this works out.


;; [[file:Sacha.org::#gif-screencast][gif-screencast:3]]
(use-package gif-screencast
	:bind
	("s-s" . my-gif-screencast-start-or-stop-and-choose-thumbnail)
	:config
	(setq gif-screencast-output-directory my-recordings-dir))

(use-package giffy
	:quelpa (giffy :fetcher github :repo "larsmagne/giffy"))
;; gif-screencast:3 ends here

;; Sentences end with a single space
;; :PROPERTIES:
;; :CUSTOM_ID: sentences-end-with-a-single-space
;; :END:

;; In my world, sentences end with a single space. This makes
;; sentence navigation commands work for me.


;; [[file:Sacha.org::#sentences-end-with-a-single-space][Sentences end with a single space:1]]
(setq sentence-end-double-space nil)
;; Sentences end with a single space:1 ends here

;; Writeroom
;; :PROPERTIES:
;; :CUSTOM_ID: writeroom
;; :END:


;; [[file:Sacha.org::#writeroom][Writeroom:1]]
(use-package writeroom-mode
	:config
	(setq writeroom-global-effects (remove 'writeroom-set-fullscreen
																				 writeroom-global-effects)))
;; Writeroom:1 ends here

;; Try redacting                                              :emacs:config:
;; :PROPERTIES:
;; :CUSTOM_ID: try-redacting
;; :END:


;; [[file:Sacha.org::#try-redacting][Try redacting:1]]
(defun my-redact (s)
	"Replace S with x characters."
	(make-string (length s) ?x))

(defun my-redact-region (beg end &optional func)
	"Redact from BEG to END."
	(interactive "r")
	(let ((overlay (make-overlay beg end)))
		(overlay-put overlay 'redact t)
		(overlay-put overlay 'display
								 (cond
									((functionp func)
									 (funcall func))
									((stringp func)
									 func)
									(t (make-string (- end beg) ?x))))))

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

(defun my-redact-regexp (regexp &optional beg end func)
  "Redact buffer content matching regexp."
  (interactive (list (string-trim (read-regexp "Redact regexp: " 'regexp-history-last))))
	(save-excursion
    (goto-char (or beg (point-min)))
    (while (re-search-forward regexp (or end (point-max)) t)
			(my-redact-region (match-beginning 0) (match-end 0) func))))

(defun my-unredact ()
	(interactive)
	(mapc 'delete-overlay
				(seq-filter (lambda (overlay) (overlay-get overlay 'redact))
										(overlays-in (point-min) (point-max)))))

(defun my-redact-email-string (s)
	(replace-regexp-in-string
	 "\\([-+_~a-zA-Z0-9][-+_.~:a-zA-Z0-9]*\\)@\\([-a-zA-Z0-9]+[-.a-zA-Z0-9]*\\)"
	 (lambda (sub)
		 (concat
			(make-string (length (match-string 1 sub)) ?x)
			"@"
			(make-string (length (match-string 2 sub)) ?x)))
	 s))

(defun my-redact-emails (&rest _)
	(interactive)
	(my-redact-regexp
	 "\\([-+_~a-zA-Z0-9][-+_.~:a-zA-Z0-9]*\\)@\\([-a-zA-Z0-9]+[-.a-zA-Z0-9]*\\)"
	 nil nil
	 (lambda () (my-redact-email-string (match-string 0)))))

(defun my-redact-emacsconf-org ()
	(interactive)
	(my-redact-regexp-replacement
	 "\\(^:EMAIL:[ \t]+\\)\\(.+\\)"
	 "\\1 \\,(my-redact \\2)"
	 ))
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

(defun my-redact-regexp-in-rectangle (regexp beg end)
  (interactive (list (read-regexp "Redact regexp: " 'regexp-history-last)
										 (min (point) (mark))
										 (max (point) (mark))))
	(apply-on-rectangle (lambda (start-col end-col)
												(my-redact-regexp regexp
																					(and (move-to-column start-col) (point))
																					(and (move-to-column end-col) (point))))
											beg end))

(advice-add
 #'notmuch-show
 :after #'my-redact-emails)
;; Try redacting:1 ends here

;; Recognizing keyword phrases
;; :PROPERTIES:
;; :CUSTOM_ID: recognizing-keyword-phrases
;; :END:

;; There are several things I want to do while dictating.

;; - I want to mark different topics so that it's easy to find the section where I was talking about something.
;; - I might want to set tags or priorities, or even schedule something (today, tomorrow, next week, next month).
;; - I can also use commands to trigger different things, like sending the section to a better speech recognition engine.

;; By analyzing the text, I might be able to make my own command system.

;; So far, for starting keywords, I can use "start", "begin", or "open".
;; I pair that with one of these part keywords:

;; - "section", "chapter", "topic", "summary": I use these pretty interchangeably at the moment. I want them to make a new Org heading.
;; - "next steps": could be handy for being able to quickly see what to do next
;; - "reminder":
;; - "interruption": don't know what I'll use this for yet, but it might be useful to note this.
;; - "tag", "keyword": maybe use this to add tags to the current section?

;; Then the code can extract the text until the matching "stop/close/end
;; <part>", assuming it happens within 50 words or so.
;; (~my-audio-braindump-close-keyword-distance-words~)

;; Sometimes keywords get misrecognized. "Begin summary" sometimes
;; becomes "again summary" or "the game summary". I could try "open" and
;; "close". Commercial dictation programs like Dragon NaturallySpeaking
;; use "open" and "close" for punctuation, so that would probably work
;; fine. "Start" works well, but "end" doesn't because it can confused
;; with "and".

;; Sometimes an extra word sneaks in, either because I say it or because
;; the speech recognition tries too hard to guess. "Begin reminder" ends
;; up as "Begin a reminder." I changed from using regular expressions
;; that searched for just start-keyword + part-keyword to one that looked
;; for the start of the keyword phrase and then looked for the next
;; keyword within the next X words. (~my-audio-braindump-scan-for-part-keyword~)


;; [[file:Sacha.org::#recognizing-keyword-phrases][Recognizing keyword phrases:1]]
(defvar my-audio-braindump-open-keywords '("start" "begin" "open"))
(defvar my-audio-braindump-close-keywords '("stop" "end" "close"))
(defvar my-audio-braindump-part-keywords '("summary" "chapter" "topic"
																			"section"
																 "action" "idea" "journal" "reminder"
																 "command" "interruption" "note"
																 "next step" "next steps" "tags" "tag" "keywords" "keyword"))

(defvar my-audio-braindump-part-keyword-distance-words 2 "Number of words to scan for part keyword.")
(defvar my-audio-braindump-close-keyword-distance-words 50 "number of words to scan for stop keyword.
Put the keywords on the same line if found.")
(defun my-audio-braindump-scan-for-part-keyword (before-part &optional part-keywords within-distance before-distance)
	"Look for BEFORE-PART followed by PART-KEYWORDS.
There might be WITHIN-DISTANCE words between BEFORE-PART and PART-KEYWORDS,
and the pair might be within BEFORE-DISTANCE from point.
Distances are in words.
Return (start end before-part part) if found, nil otherwise."
	(setq before-part (pcase before-part
											('start my-audio-braindump-open-keywords)
											('stop my-audio-braindump-close-keywords)
											('nil (append my-audio-braindump-open-keywords my-audio-braindump-close-keywords))
											(_ before-part)))
	(setq part-keywords (or part-keywords my-audio-braindump-part-keywords))
	(when (stringp part-keywords) (setq part-keywords (list part-keywords)))
	(setq within-distance (or within-distance my-audio-braindump-part-keyword-distance-words))
	(setq before-distance (if (eq before-distance t)
														(point-max)
													(or before-distance my-audio-braindump-close-keyword-distance-words)))
	(let (result
				start end
				(before-point (save-excursion (forward-word before-distance) (point)))
				before-word
				part-word)
		(save-excursion
			(when (looking-at (regexp-opt before-part))
				(setq before-word (match-string 0) start (match-beginning 0))
				(when (re-search-forward (regexp-opt part-keywords) (save-excursion (forward-word within-distance) (point)) t)
					(setq result (list start (match-end 0) before-word (match-string 0)))))
			(while (and (not result)
									(re-search-forward (regexp-opt before-part) before-point t))
				(setq before-word (match-string 0) start (match-beginning 0))
				(when (re-search-forward (regexp-opt part-keywords) (save-excursion (forward-word within-distance) (point)) t)
					(setq result (list start (match-end 0) before-word (match-string 0)))))
			(when result (goto-char (elt result 1)))
			result)))

(ert-deftest my-audio-braindump-scan-for-part-keyword ()
	(with-temp-buffer
		(insert "some text start a reminder hello world stop there and do something stop reminder more text")
		(goto-char (point-min))
		(let ((result (my-audio-braindump-scan-for-part-keyword 'start nil)))
			(expect (elt result 2) :to-equal "start")
			(expect (elt result 3) :to-equal "reminder"))
		(let ((result (my-audio-braindump-scan-for-part-keyword 'stop "reminder")))
			(expect (elt result 2) :to-equal "stop")
			(expect (elt result 3) :to-equal "reminder"))))
;; Recognizing keyword phrases:1 ends here

;; Splitting the lines based on keywords and oopses
;; :PROPERTIES:
;; :CUSTOM_ID: splitting-the-lines-based-on-keywords-and-oopses
;; :END:

;; Now I can use that to scan through the text. I want to put commands on
;; their own lines so that ~subed-align~ will get the timestamp for that
;; segment and so that the commands are easier to parse.

;; I also want to detect "oops" and split things up so that the start of
;; that line matches my correction after the "oops". I use
;; [[https://sachachua.com/dotemacs/index.html#split-up-oops-better][my-subed-split-oops]] for that, which I should write about in another
;; post. By putting the oops fragment on its own line, I can use
;; ~subed-align~ to get a timestamp for just that segment. Then I can
;; either use ~flush-lines~ to get rid of anything with "oops" in it. I
;; can even remove the subtitle and use ~subed-record-compile-media~ to
;; compile audio/video without that segment, if I want to use the audio
;; without rerecording it.

;; #+begin_example
;; And the way I can help is by jotting words down in a mind map,
;; typing her sentences. Oops
;; typing, her sentences And generating, follow-up questions.
;; #+end_example

;; I also all-caps the keyword phrases so that they're easier to see when
;; skimming the text file.


;; [[file:Sacha.org::#splitting-the-lines-based-on-keywords-and-oopses][Splitting the lines based on keywords and oopses:1]]
(defun my-audio-braindump-prepare-alignment-breaks ()
	"Split lines in preparation for forced alignment with aeneas.

Split \"oops\" so that it's at the end of the line and the
previous line starts with roughly the same words as the next
line, for easier removal.

Add a linebreak before \"begin/start\" followed by
`my-audio-braindump-part-keywords'.

Add a linebreak after \"stop\" followed by
`my-audio-braindump-part-keywords'.

Look for begin keyword ... stop keyword with at most
`my-audio-braindump-part-keyword-distance-words' between them and put them on one
line."
	(interactive)
	(let ((case-fold-search t) result close-result)
		(my-split-oops)
		;; break "begin/start keyword"
		(goto-char (point-min))
		(while (setq result (my-audio-braindump-scan-for-part-keyword 'start nil nil t))
			(goto-char (car result))
			(delete-region (car result) (elt result 1))
			(insert "\n" (upcase (concat (elt result 2) " " (elt result 3))) "\n"))
		;; break stop
		(goto-char (point-min))
		(while (setq result (my-audio-braindump-scan-for-part-keyword 'stop nil nil t))
			(goto-char (car result))
			(delete-region (car result) (elt result 1))
			(insert (upcase (concat (elt result 2) " " (elt result 3))) "\n"))
		;; try to get start and end sections on one line
		(goto-char (point-min))
		(while (setq result (my-audio-braindump-scan-for-part-keyword 'start nil nil t))
			(goto-char (elt result 1))
			(setq stop-result (my-audio-braindump-scan-for-part-keyword 'stop (elt result 3)))
			(if stop-result
					(progn
						(goto-char (car stop-result))
						(while (re-search-backward " *\n+ *" (car result) t)
							(replace-match " ")))
				;; no stop keyword; are we on an empty line? If so, just merge it with the next one
				(when (looking-at "\n+ *")
					(replace-match " "))))
		;; remove empty lines
		(goto-char (point-min))
		(when (looking-at "\n+") (replace-match ""))
		(while (re-search-forward "\n\n+" nil t)
			(replace-match "\n"))
		(goto-char (point-min))
		(while (re-search-forward " *\n *" nil t)
			(replace-match "\n"))))

(ert-deftest my-audio-braindump-prepare-alignment-breaks ()
	(with-temp-buffer
		(insert "some text start a reminder hello world stop there and do something stop reminder more text")
		(goto-char (point-min))
		(my-audio-braindump-prepare-alignment-breaks)
		(expect (buffer-string) :to-equal
						"some text
START REMINDER hello world stop there and do something STOP REMINDER
more text")))
;; Splitting the lines based on keywords and oopses:1 ends here

;; Preparing the VTT subtitles
;; :PROPERTIES:
;; :CUSTOM_ID: preparing-the-vtt-subtitles
;; :END:

;; ~subed-align~ gives me a VTT subtitle file with timestamps and text. I
;; add NOTE comments with the keywords and make ~subed:~ links to the
;; timestamps using the ~ol-subed.el~ that I just added.


;; [[file:Sacha.org::#preparing-the-vtt-subtitles][Preparing the VTT subtitles:1]]
(defun my-audio-braindump-get-subtitle-note-based-on-keywords (sub-text)
	(let ((case-fold-search t))
		(when (string-match (concat "^"
																(regexp-opt my-audio-braindump-open-keywords)
																" \\(" (regexp-opt my-audio-braindump-part-keywords) "\\) \\(.+?\\)\\( "
																(regexp-opt my-audio-braindump-close-keywords) " "
																(regexp-opt my-audio-braindump-part-keywords) "\\)?$")
												sub-text)
			(concat (match-string 1 sub-text) ": " (match-string 2 sub-text)))))
(ert-deftest my-audio-braindump-get-subtitle-note-based-on-keywords ()
	(expect (my-audio-braindump-get-subtitle-note-based-on-keywords "BEGIN NEXT STEPS . Think about how dictation helps me practice slower speed. CLOSE NEXT STEPS")
					:to-equal "NEXT STEPS: . Think about how dictation helps me practice slower speed.")
	(expect (my-audio-braindump-get-subtitle-note-based-on-keywords "START SUMMARY hello world STOP SUMMARY")
					:to-equal "SUMMARY: hello world")
	(expect (my-audio-braindump-get-subtitle-note-based-on-keywords "START CHAPTER hello world again")
					:to-equal "CHAPTER: hello world again")
	)
;; Preparing the VTT subtitles:1 ends here

;; Formatting the subtitles into Org Mode subtrees
;; :PROPERTIES:
;; :CUSTOM_ID: formatting-the-subtitles-into-org-mode-subtrees
;; :END:

;; The last step is to take the list of subtitles and format it into the subtree.


;; [[file:Sacha.org::#formatting-the-subtitles-into-org-mode-subtrees][Formatting the subtitles into Org Mode subtrees:1]]
;; todo: sort the completion? https://emacs.stackexchange.com/questions/55502/list-files-in-directory-in-reverse-order-of-date
;;
(defun my-audio-braindump-insert-subtitles-as-org-tree (vtt-filename)
	(interactive (list (read-file-name "VTT: " (expand-file-name "./" my-phone-recording-dir) nil t nil
																		 (lambda (s) (string-match "\\.vtt$" s)))))
	(let* ((subtitles
					(mapcar (lambda (sub)
										(unless (elt sub 4)
											(setf (elt sub 4)
														(my-audio-braindump-get-subtitle-note-based-on-keywords (elt sub 3))))
										sub)
									(subed-parse-file vtt-filename)))
				 (start-date (my-audio-braindump-get-file-start-time vtt-filename))
				 chapters tags
				 start-of-entry)
		(setq start-of-entry (point))
		(insert (format "* TODO Review braindump from %s  :braindump:\n\n" (file-name-base vtt-filename)))
		(org-entry-put (point) "CREATED"
									 (concat "[" (format-time-string
																(cdr org-timestamp-formats)
																(my-audio-braindump-get-file-start-time
																 (file-name-nondirectory vtt-filename))) "]"))
		(insert
		 (format "%s - %s - %s\n"
						 (org-link-make-string (concat "file:" (file-name-sans-extension vtt-filename) ".vtt")
																	 "VTT")
						 (org-link-make-string (concat "file:" (file-name-sans-extension vtt-filename) ".txt")
																	 "Text")
						 (org-link-make-string (concat "file:" (file-name-sans-extension vtt-filename) ".m4a")
																	 "Audio")))
		(save-excursion
			(insert "** Transcript\n")
			;; add each subtitle; add an ID in case we change the title
			(mapc
			 (lambda (sub)
				 (when (elt sub 4)
					 (let ((note (my-audio-braindump-get-subtitle-note-based-on-keywords (elt sub 3))))
						 (insert (concat "*** "
														 note " "
														 (org-link-make-string
															(format "subed:%s::%s"
																			vtt-filename
																			(my-msecs-to-timestamp (elt sub 1)))
															"VTT")
														 "\n\n"))
						 (org-entry-put (point) "CREATED"
									 (concat "[" (format-time-string
																(cdr org-timestamp-formats)
																(time-add start-date
																					(seconds-to-time (/ (elt sub 1) 1000.0)))) "]"))
						 (org-entry-put (point) "START" (my-msecs-to-timestamp (elt sub 2)))
						 (when (elt sub 4)
							 (when (string-match "command: .*recognize" (elt sub 4))
								 (save-excursion
									 ;; TODO: scope this to just the section someday
									 (goto-char start-of-entry)
									 (org-set-tags (append (list "recognize") (org-get-tags)))))
							 (when (string-match "command: .*outline" (elt sub 4))
								 (save-excursion
									 (goto-char start-of-entry)
									 (org-set-tags (append (list "outline") (org-get-tags)))))
							 (when (string-match "^time" (elt sub 4))
								 (insert "[" (org-format-time-string (cdr org-timestamp-formats)
																										 (time-add start-date (seconds-to-time (/ (elt sub 1) 1000))))
												 "]\n"))
							 (when (string-match "command: .+\\(high\\|low\\)" (elt sub 4))
								 (save-excursion
									 (goto-char start-of-entry)
									 (org-priority (if (string= (downcase (match-string 1)) "high") ?A ?C))))
							 (when (string-match "\\(?:tags?\\|keywords?\\): \\(.+\\)" (elt sub 4))
								 (save-excursion
									 (goto-char start-of-entry)
									 (org-set-tags (append (split-string (match-string 1) " ") (org-get-tags))))))
						 (add-to-list 'chapters
													(format "- %s (%s)"
																	(org-link-make-string (concat "id:" (org-id-get-create))
																												note)
																	(org-link-make-string
																	 (format "subed:%s::%s"
																					 vtt-filename
																					 (my-msecs-to-timestamp (elt sub 1)))
																	 "VTT")))))
				 (insert (elt sub 3) "\n"))
			 subtitles))
		(when chapters
			(insert (string-join (nreverse chapters) "\n") "\n"))))
;; Formatting the subtitles into Org Mode subtrees:1 ends here

;; [[file:Sacha.org::#formatting-the-subtitles-into-org-mode-subtrees][Formatting the subtitles into Org Mode subtrees:2]]
(defun my-file-start-time (filename &optional base-date)
	"Return the local time based on FILENAME."
	(setq filename (file-name-base filename))
	(cond
	 ((string-match "^\\([0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]\\)[-T]\\([0-9][0-9][\\.-][0-9][0-9]\\(?:[\\.-][0-9][0-9]\\)?\\)" filename)
		(date-to-time (concat (match-string 1 filename) "T"
													(replace-regexp-in-string "[\\.-]" ":" (match-string 2 filename)))))
	 ((string-match "^\\(?:Copy of \\)?\\([^ ][^ ][^ ]\\)[^ ]+ at \\([0-9]+\\)-\\([0-9]+\\)" filename)
		(let* ((day (match-string 1 filename))
					 (hour (match-string 2 filename))
					 (min (match-string 3 filename))
					 (changed-time (or base-date (file-attribute-modification-time
																				(file-attributes filename))))
					 (decoded-time (decode-time changed-time)))
			;; get the day on or before changed-time
			(if (string= (format-time-string "%a" changed-time) day)
					(encode-time (append
												(list
												 0
												 (string-to-number min)
												 (string-to-number hour))
												(seq-drop decoded-time 3)))
				;; synchronized maybe within the week after
				(let ((org-read-date-prefer-future nil))
					(org-read-date t t
												 (concat "--" day " " hour ":" min)
												 nil changed-time)))))))

(ert-deftest my-file-start-time ()
	(should
	 (equal (format-time-string "%Y-%m-%d %H:%M:%S"
															(my-file-start-time "2024-01-05-09-46-59.flv"))
					"2024-01-05 09:46:59"))
	(should
	 (equal (format-time-string "%Y-%m-%d %H:%M:%S"
															(my-file-start-time "2024-01-08T12.49.vtt"))
					"2024-01-08 12:49:00"))
	(should
	 (equal (format-time-string "%Y-%m-%d %H:%M:%S"
															(my-file-start-time "Sunday at 15-30.vtt"
																									(date-to-time "2023-01-12")))
					"2023-01-08 15:30:00"))
	(should
	 (time-equal-p (my-file-start-time "Sunday at 12-49.txt")
								 (org-read-date t t "-sun 12:49"))))

(defalias 'my-audio-braindump-get-file-start-time #'my-file-start-time)
;; Formatting the subtitles into Org Mode subtrees:2 ends here

;; Process a single transcript from the raw text file
;; :PROPERTIES:
;; :CUSTOM_ID: process-a-single-transcript-from-the-raw-text-file
;; :END:

;; So now we put that all together: rename the file using the calculated
;; start time, prepare the alignment breaks, align the file to get the
;; timestamps, and add the subtree to an Org file.


;; [[file:Sacha.org::#process-a-single-transcript-from-the-raw-text-file][Process a single transcript from the raw text file:1]]
(defvar my-audio-braindump-file "~/sync/orgzly/braindump.org")

(defun my-audio-braindump-make-todo (text-file &optional force)
	"Add TEXT-FILE as a TODO."
	(interactive (list (buffer-file-name) current-prefix-arg))
	;; rename the files to use the timestamps
	(unless (string-match "^[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]"
												(file-name-base text-file))
		(setq text-file (my-audio-braindump-rename-files-based-on-time text-file)))
	(let* ((recording (concat (file-name-sans-extension text-file) ".m4a"))
				 (start (my-audio-braindump-get-file-start-time text-file))
				 (vtt (concat (file-name-sans-extension text-file) ".vtt"))
				 chapters
				 (title (concat "Review braindump " text-file))
				 existing)
		;; check if already exists
		(with-current-buffer (find-file-noselect my-audio-braindump-file)
			(save-excursion
				(goto-char (point-min))
				(setq existing (org-find-exact-headline-in-buffer title))))
		(if (and existing (not force))
				(progn
					(message "Going to existing heading")
					(org-goto-marker-or-bmk existing))
			(if (or (null my-audio-braindump-last-processed-time)
							(time-less-p my-audio-braindump-last-processed-time start))
					(customize-save-variable 'my-audio-braindump-last-processed-time start))
			(find-file text-file)
			(my-audio-braindump-prepare-alignment-breaks)
			(save-buffer)
	 		(when (file-exists-p vtt) (delete-file vtt))
			(when (get-file-buffer vtt) (kill-buffer (get-file-buffer vtt)))
			(subed-align recording text-file "VTT")
			(when (get-file-buffer vtt) (kill-buffer (get-file-buffer vtt)))
			(find-file my-audio-braindump-file)
			(goto-char (point-min))
			(if existing
					(progn
						(org-goto-marker-or-bmk existing)
						(delete-region (point) (org-end-of-subtree)))
				(org-next-visible-heading 1))
			(my-audio-braindump-insert-subtitles-as-org-tree vtt))))
;; Process a single transcript from the raw text file:1 ends here

;; Process multiple files
;; :PROPERTIES:
;; :CUSTOM_ID: process-multiple-files
;; :END:

;; I want to process multiple files in one batch.


;; [[file:Sacha.org::#process-multiple-files][Process multiple files:1]]
(defun my-audio-braindump-process (files &optional force)
	(interactive (list (cond
											((and (derived-mode-p 'dired-mode)
											 			(dired-get-marked-files))
											 (dired-get-marked-files))
											((derived-mode-p 'dired-mode)
											 (list (dired-get-filename)))
											((string-match "\\.txt$" (buffer-file-name))
											 (list (buffer-file-name)))
											(t (read-file-name "Transcript: ")))
										 current-prefix-arg))
	(mapc (lambda (f)
					(when (string-match "txt" f)
						(my-audio-braindump-make-todo f force))) files))
;; Process multiple files:1 ends here



;; It would be nice to have it automatically keep track of the latest one
;; that's been processed, maybe via ~customize-save-variable~. This still
;; needs some tinkering with.


;; [[file:Sacha.org::#process-multiple-files][Process multiple files:2]]
(defcustom my-audio-braindump-last-processed-time nil
	"The timestamp of the last processed transcript."
	:group 'sacha
	:type '(repeat integer))

(defun my-audio-braindump-process-since-last ()
	(interactive)
	(let ((files
				 (seq-filter
					(lambda (f)
						(or (null my-audio-braindump-last-processed-time)
								(time-less-p my-audio-braindump-last-processed-time
														 (my-audio-braindump-get-file-start-time f))))
					(directory-files my-phone-recording-dir 'full " at [0-9][0-9]-[0-9][0-9]\\.txt\\|^[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]T[0-9][0-9]\\.[0-9][0-9]\\.txt"))))
		(mapc (lambda (f)
						(my-audio-braindump-make-todo f)
						(let ((start (my-audio-braindump-get-file-start-time f)))
							(if (time-less-p my-audio-braindump-last-processed-time start)
									(setq my-audio-braindump-last-processed-time start))))
					files))
	(customize-save-variable 'my-audio-braindump-last-processed-time my-audio-braindump-last-processed-time))

(defun my-audio-braindump-new-filename (text-file &optional base-date)
	(if (string-match "^[0-9][0-9][0-9][0-9]" text-file)
			text-file			; no change, already uses date
		(let* ((base (file-name-base text-file))
					 (start (my-audio-braindump-get-file-start-time base base-date))
					 (rest (if (string-match "^\\([-0-9T\\.]+\\|\\(?:.+? at [0-9][0-9]-[0-9][0-9]\\)\\)\\( .+\\)" base)
										 (match-string 2 base)
									 ""))
					 (new-base (format-time-string "%Y-%m-%dT%H.%M" start)))
			(concat new-base rest "." (file-name-extension text-file)))))

(ert-deftest my-audio-braindump-new-filename ()
 (should
	(equal (my-audio-braindump-new-filename "Wednesday at 18-58.txt" (date-to-time "2023-01-01"))
				 "2022-12-28T18.58.txt"))
 (should
	(equal (my-audio-braindump-new-filename "Wednesday at 18-58 extra text.txt" (date-to-time "2023-01-01"))
				 "2022-12-28T18.58 extra text.txt")))

(defun my-audio-braindump-rename-files-based-on-time (text-file)
	"Rename TEXT-FILE based on date. Return the new text file."
	(interactive (list (if (derived-mode-p 'dired-mode) (dired-get-filename)
											 (buffer-file-name))))
	(if (string-match "^[0-9][0-9][0-9][0-9]" text-file)
			text-file			; no change, already uses date
		(let ((new-name (my-audio-braindump-new-filename (file-name-nondirectory text-file))))
			(if (file-exists-p (expand-file-name new-name
																					 (file-name-directory text-file)))
					(error "%s already exists" new-base)
				(dolist (ext '(".txt" ".m4a" ".vtt"))
					(if (file-exists-p (concat (file-name-sans-extension text-file) ext))
							(rename-file (concat (file-name-sans-extension text-file) ext)
													 (expand-file-name (concat (file-name-sans-extension new-name) ext)
																						 (file-name-directory text-file)))))
				(expand-file-name new-name
													(file-name-directory text-file))))))
;; Process multiple files:2 ends here

;; Markdown
;; :PROPERTIES:
;; :CUSTOM_ID: markdown
;; :END:

;; [[file:Sacha.org::#markdown][Markdown:1]]
(use-package markdown-mode
  :if my-laptop-p
  :mode ("\\.\\(njk\\|md\\)\\'" . markdown-mode))
;; Markdown:1 ends here

;; Screenshot
;; :PROPERTIES:
;; :CUSTOM_ID: screenshot
;; :END:

;; Based on https://www.reddit.com/r/emacs/comments/idz35e/emacs_27_can_take_svg_screenshots_of_itself/

;; [[file:Sacha.org::#screenshot][Screenshot:1]]
(defun screenshot-svg ()
  "Save a screenshot of the current frame as an SVG image.
Saves to a temp file and puts the filename in the kill ring."
  (interactive)
  (let* ((filename
					(expand-file-name
					 (format-time-string "%Y-%m-%d-%H-%M-%S.svg")
					 my-recordings-dir))
         (data (x-export-frames nil 'svg)))
    (with-temp-file filename
      (insert data))
    (kill-new filename)
    (message filename)))
(keymap-global-set "C-c s" #'screenshot-svg)
;; Screenshot:1 ends here

;; Avoiding weasel words
;; :PROPERTIES:
;; :CUSTOM_ID: avoiding-weasel-words
;; :END:

;; [[file:Sacha.org::#avoiding-weasel-words][Avoiding weasel words:1]]
(use-package artbollocks-mode
  :if my-laptop-p
  :defer t
  :load-path  "~/elisp/artbollocks-mode"
  :config
  (progn
    (setq artbollocks-weasel-words-regex
          (concat "\\b" (regexp-opt
                         '("one of the"
                           "should"
                           "just"
                           "sort of"
                           "a lot"
                           "probably"
                           "maybe"
                           "perhaps"
                           "I think"
                           "really"
                           "pretty"
                           "nice"
                           "action"
                           "utilize"
                           "leverage") t) "\\b"))
    ;; Don't show the art critic words, or at least until I figure
    ;; out my own jargon
    (setq artbollocks-jargon nil)))
;; Avoiding weasel words:1 ends here

;; Unfill paragraph
;; :PROPERTIES:
;; :CUSTOM_ID: unfill-paragraph
;; :END:

;; I unfill paragraphs a lot because Wordpress likes adding extra =<br>= tags if I don't. (I should probably just tweak my Wordpress installation.)


;; [[file:Sacha.org::#unfill-paragraph][Unfill paragraph:1]]
(defun my-unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn
                 (barf-if-buffer-read-only)
                 (list t)))
  (let ((fill-column (point-max)))
    (fill-paragraph nil region)))
(bind-key "M-Q" 'my-unfill-paragraph)
;; Unfill paragraph:1 ends here



;; I never actually justify text, so I might as well change the way
;; =fill-paragraph= works. With the code below, =M-q= will fill the
;; paragraph normally, and =C-u M-q= will unfill it.


;; [[file:Sacha.org::#unfill-paragraph][Unfill paragraph:2]]
(defun my-fill-or-unfill-paragraph (&optional unfill region)
  "Fill paragraph (or REGION).
        With the prefix argument UNFILL, unfill it instead."
  (interactive (progn
                 (barf-if-buffer-read-only)
                 (list (if current-prefix-arg 'unfill) t)))
  (let ((fill-column (if unfill (point-max) fill-column)))
    (fill-paragraph nil region)))
(bind-key "M-q" 'my-fill-or-unfill-paragraph)
;; Unfill paragraph:2 ends here



;; Also, =visual-line-mode= is so much better than =auto-fill-mode=. It doesn't actually break the text into multiple lines - it only looks that way.


;; [[file:Sacha.org::#unfill-paragraph][Unfill paragraph:3]]
(remove-hook 'text-mode-hook #'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
;; Unfill paragraph:3 ends here

;; Unicode
;; :PROPERTIES:
;; :CUSTOM_ID: unicode
;; :END:


;; [[file:Sacha.org::#unicode][Unicode:1]]
(defmacro my-insert-unicode (unicode-name)
  `(lambda () (interactive)
     (insert-char (cdr (assoc-string ,unicode-name (ucs-names))))))
(bind-key "C-x 8 s" (my-insert-unicode "ZERO WIDTH SPACE"))
(bind-key "C-x 8 S" (my-insert-unicode "SNOWMAN"))
;; Unicode:1 ends here

;; Clean up spaces
;; :PROPERTIES:
;; :CUSTOM_ID: clean-up-spaces
;; :END:


;; [[file:Sacha.org::#clean-up-spaces][Clean up spaces:1]]
(bind-key "M-SPC" 'cycle-spacing)
;; Clean up spaces:1 ends here

;; Expand
;; :PROPERTIES:
;; :CUSTOM_ID: expand
;; :END:


;; [[file:Sacha.org::#expand][Expand:1]]
(setq save-abbrevs 'silently)
(bind-key "M-/" 'hippie-expand)
;; Expand:1 ends here



;; From https://github.com/purcell/emacs.d/blob/master/lisp/init-auto-complete.el - Exclude very large buffers from dabbrev

;; [[file:Sacha.org::#expand][Expand:2]]
(defun sanityinc/dabbrev-friend-buffer (other-buffer)
  (< (buffer-size other-buffer) (* 1 1024 1024)))
(setq dabbrev-friend-buffer-function 'sanityinc/dabbrev-friend-buffer)
;; Expand:2 ends here

;; [[file:Sacha.org::#expand][Expand:3]]
(setq hippie-expand-try-functions-list
      '(yas-hippie-try-expand
        try-expand-all-abbrevs
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-dabbrev
        try-expand-dabbrev-from-kill
        try-expand-dabbrev-all-buffers
        try-expand-list
        try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))
;; Expand:3 ends here

;; Write about keybindings
;; :PROPERTIES:
;; :CUSTOM_ID: write-about-keybindings
;; :END:


;; [[file:Sacha.org::#write-about-keybindings][Write about keybindings:1]]
;; hmm, doesn't quite work for looking things up yet. I basically want a programmatic where-is for a specific keymap
(defvar my-keybinding-maps '(subed-mode-map subed-waveform-minor-mode-map subed-waveform-svg-map))
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
;; Write about keybindings:1 ends here

;; Transcripts from my phone  :audio:editing:
;; :PROPERTIES:
;; :CUSTOM_ID: transcripts-from-my-phone
;; :END:


;; [[file:Sacha.org::#transcripts-from-my-phone][Transcripts from my phone:1]]
(defvar my-audio-braindump-dir "~/sync/Phone")
(defun my-open-latest-braindump ()
  (interactive)
  (find-file (my-latest-file my-audio-braindump-dir "\\.txt"))
  (kill-new (buffer-string)))

(defun my-insert-latest-braindump ()
  (interactive)
  (insert-file-contents (my-latest-file my-audio-braindump-dir "\\.txt")))
(defun my-audio-braindump-dired ()
	(interactive)
	(dired my-audio-braindump-dir "-lt"))
(defalias 'my-phone-dired #'my-audio-braindump-dired)
;; Transcripts from my phone:1 ends here

;; TOBLOG Using Emacs Lisp to send audio files to Deepgram and format VTTs :emacs:speech:
;; :PROPERTIES:
;; :CUSTOM_ID: using-emacs-lisp-to-send-audio-files-to-deepgram-and-format-vtts
;; :END:

;; I've been experimenting with Deepgram's API for speech
;; recognition because it can handle larger files than OpenAI
;; Whisper's API, so I don't have to worry about chunking my
;; files into 15-minute segments. It also supports diarization,
;; which means identifying different speakers. That's handy for
;; things like the EmacsConf Q&A sessions, which involve
;; multiple people.

;; I think the built-in VTT formatter doesn't handle speaker
;; identification, so I wrote some Emacs Lisp to send an audio
;; file for recognition, save the JSON, and format the results
;; as a VTT subtitle file. I also split the captions a little
;; closer to the way I like to do them, starting a new subtitle
;; if the line exceeds ~my-deepgram-length-threshold~ or
;; ~my-deepgram-time-threshold~, or if we're after a punctuated
;; word and the current subtitle is more than halfway to the
;; length threshold. Someday I'll figure out how to get it to
;; split on prepositions.


;; [[file:Sacha.org::#using-emacs-lisp-to-send-audio-files-to-deepgram-and-format-vtts][TOBLOG Using Emacs Lisp to send audio files to Deepgram and format VTTs:1]]
(defvar my-deepgram-length-threshold 45 "Number of characters.")
(defvar my-deepgram-time-threshold 10 "Number of seconds since the first word.")

(defun my-deepgram-recognize-audio (audio-file &optional diarize)
	"Send AUDIO-FILE to Deepgram, save the JSON, and create a VTT.
If DIARIZE is non-nil, identify speakers."
	(require 'subed)
	(interactive (list (if (auth-info-password (car (auth-source-search :host "https://api.deepgram.com")))
												 (read-file-name "Audio file: ")
											 (error "Please put deepgram API key in auth sources."))))
	(with-current-buffer (get-buffer-create "*Deepgram*")
		(erase-buffer)
		(unless (string-match "\\(opus\\|wav\\|mp3\\)$" audio-file)
			(if (file-exists-p (concat (file-name-sans-extension audio-file) ".opus"))
					(setq audio-file (concat (file-name-sans-extension audio-file) ".opus"))
				(call-process "ffmpeg" nil t t "-i" (expand-file-name audio-file)
											"-ac" "1" "-y"
											(expand-file-name (concat (file-name-sans-extension audio-file) ".opus")))
				(setq audio-file (concat (file-name-sans-extension audio-file) ".opus"))))
		(unless (file-exists-p (expand-file-name (concat (file-name-sans-extension audio-file) ".json")))
			(call-process
			 "curl" nil t t "--request" "POST" "--header"
			 (concat "Authorization: Token " (auth-info-password (car (auth-source-search :host "https://api.deepgram.com"))))
			 "--header" (concat "Content-Type: " (mailcap-file-name-to-mime-type audio-file))
			 "--data-binary" (concat "@" (expand-file-name audio-file))
			 "--url"
			 (concat
				"https://api.deepgram.com/v1/listen?punctuate=true&model=whisper-large&smart_format=true&utterances=true"
				(if diarize
						"&diarize=true"
					""))
			 "-o"
			 (expand-file-name (concat (file-name-sans-extension audio-file) ".json"))))
		(my-deepgram-convert-json-to-vtt (concat (file-name-sans-extension audio-file) ".json")))
	(find-file (concat (file-name-sans-extension audio-file) ".vtt")))

(defun my-emacsconf-extract-deepgram-recognize-qa-for-talk (talk)
	"Send the QA (or main) Opus file for TALK to Deepgram.
Save the results as JSON and VTT."
	(interactive (list (emacsconf-complete-talk-info)))
	(setq talk (emacsconf-resolve-talk talk))
	(if (or (emacsconf-talk-file talk "--answers--original.json")
					(emacsconf-talk-file talk "--original.json"))
			(message "Files already exist for %s" (plist-get talk :slug))
			(if-let ((file
								(or (emacsconf-talk-file talk "--answers--original.opus")
										(emacsconf-talk-file talk "--original.opus"))))
					(my-deepgram-recognize-audio file)
				(error "No file to recognize for %s" (plist-get talk :slug)))))

(defun my-deepgram-parse (json-file)
	"Convert JSON-FILE into a list of subtitles."
	(let* ((json-object-type 'alist)
				 (json (json-read-file json-file))
				 (words
					(assoc-default
					 'words
					 (aref (assoc-default 'alternatives (aref (let-alist json .results.channels) 0)) 0)))
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
												(reverse entry) " "))
			 (when (not (string= (downcase current-text) (or last-text "")))
				 (setq last-text (downcase current-text))
				 (list nil
							 (* (assoc-default 'start (car (last entry)) nil 0) 1000)
							 (* (assoc-default 'end (car entry) nil 0) 1000)
							 ;; add speaker tag?
							 (concat
								(if (and (assoc-default 'speaker (car entry))
												 (or (null last-speaker)
														 (not (eq last-speaker (assoc-default 'speaker (car entry))))))
										(progn
											(setq last-speaker (assoc-default 'speaker (car entry)))
											(format "[Speaker %d]: " (assoc-default 'speaker (car entry))))
									"")
								current-text
								))))
		 (sort subtitles
					 (lambda (a b)
						 ;; sort by time
						 (< (assoc-default 'start (car a) nil 0)
								(assoc-default 'start (car b) nil 0)))))))

(defun my-deepgram-convert-json-to-vtt (json-file &optional force)
	(interactive (list (read-file-name "JSON: ") current-prefix-arg))
	"Convert JSON-FILE into a VTT."
	(subed-create-file
	 (concat (file-name-sans-extension json-file) ".vtt")
	 (my-deepgram-parse json-file)
	 force))

(defconst deepgram-whisper-large-per-min 0.0048)
(defun my-deepgram-cost (file)
	(interactive "FFile: ")
	(let* ((whisper-large-per-min deepgram-whisper-large-per-min)
				 (nova2-streaming-per-min 0.0059)
				 (duration (/ (ceiling (/ (compile-media-get-file-duration-ms file) 1000.0)) 60))
				 (msg (format "%.1f minutes: USD %.2f batch, USD %.2f streaming"
											duration
											(* duration whisper-large-per-min)
											(* duration nova2-streaming-per-min))))
		(when (called-interactively-p 'any)
			(message "%s" msg)
			(kill-new msg))
		(list
		 duration
		 (* duration whisper-large-per-min)
		 (* duration nova2-streaming-per-min))))
;; TOBLOG Using Emacs Lisp to send audio files to Deepgram and format VTTs:1 ends here

;; TOBLOG Rerecognize this audio and reprocess it
;; :PROPERTIES:
;; :CUSTOM_ID: rerecognize
;; :END:


;; [[file:Sacha.org::#rerecognize][TOBLOG Rerecognize this audio and reprocess it:1]]
(defun my-audio-braindump-reprocess (audio-file)
	(interactive
	 (list
		(let ((default (cond
										((derived-mode-p 'org-mode)
										 (save-excursion
											 (org-back-to-heading)
											 (when (re-search-forward "\\[Audio\\]" nil (save-excursion (org-end-of-subtree)))
												 (org-element-property :path (org-element-context)))))
										((file-exists-p (concat (file-name-sans-extension (buffer-file-name)) ".m4a"))
										 (concat (file-name-sans-extension (buffer-file-name)) ".m4a")))))
			(read-file-name (if default (format "Audio (%s): " default)
												"Audio: ")
											nil default))))
	(save-window-excursion
		(unless (file-exists-p (concat (file-name-sans-extension audio-file) ".json"))
			(my-deepgram-recognize-audio audio-file))
		(with-temp-file (concat (file-name-sans-extension audio-file) ".txt")
			(insert
			 (subed-subtitle-list-text
				(my-deepgram-parse (concat (file-name-sans-extension audio-file) ".json"))))
			(goto-char (point-min))
			(my-audio-braindump-prepare-alignment-breaks))
		(with-current-buffer (find-file-noselect (concat (file-name-sans-extension audio-file) ".txt"))
			(subed-align audio-file (concat (file-name-sans-extension audio-file) ".txt") "VTT")))
	(find-file my-audio-braindump-braindump-file)
	(goto-char (point-min))
	(my-audio-braindump-insert-subtitles-as-org-tree (concat (file-name-sans-extension audio-file) ".vtt")))
;; TOBLOG Rerecognize this audio and reprocess it:1 ends here

;; Gladia
;; :PROPERTIES:
;; :CUSTOM_ID: gladia
;; :END:

;; [[file:Sacha.org::#gladia][Gladia:1]]
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
;; Gladia:1 ends here

;; General code
;; :PROPERTIES:
;; :CUSTOM_ID: general-code
;; :END:

;; [[file:Sacha.org::#general-code][General code:1]]
(defvar my-live-speech-buffer "*Speech*")
(defvar my-live-speech-process nil)
(defvar my-live-speech-output-buffer "*Speech JSON*")

(defvar my-live-speech-functions
	'(my-live-speech-display-in-speech-buffer
		my-live-speech-display-wpm
		my-live-speech-append-to-etherpad)
	"Functions to call with one argument, the recognition results.")

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
(defun my-live-speech-handle-json (line-object)
	"Process the JSON object in LINE."
	(run-hook-with-args 'my-live-speech-functions (json-parse-string line :object-type 'alist)))

(defun my-live-speech-process-sentinel (proc event)
	(when (string-match "finished" event)
		(my-live-speech-stop)
		;(my-live-speech-mode -1)
		))

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
;; General code:1 ends here

;; Display in speech buffer
;; :PROPERTIES:
;; :CUSTOM_ID: display-in-speech-buffer
;; :END:


;; [[file:Sacha.org::#display-in-speech-buffer][Display in speech buffer:1]]
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

(defun my-live-speech-toggle-heading ()
	"Toggle a line as a heading."
	(interactive)
	(with-current-buffer (get-buffer my-live-speech-buffer)
		(display-buffer (current-buffer))
		(with-selected-window (get-buffer-window (get-buffer my-live-speech-buffer))
			(let ((avy-all-windows nil))
				(avy-goto-line 1))
			(org-toggle-heading 1))))
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
;; Display in speech buffer:1 ends here

;; Display words per minute
;; :PROPERTIES:
;; :CUSTOM_ID: display-words-per-minute
;; :END:

;; [[file:Sacha.org::#display-words-per-minute][Display words per minute:1]]
(defvar my-live-speech-wpm-window-seconds 15 "How many seconds to calculate WPM for.")
(defvar my-live-speech-recent-words nil "Words spoken in `my-live-speech-wpm-window-minutes'.")
(defvar my-live-speech-wpm nil "Current WPM.")
(defvar my-live-speech-wpm-colors  ; haven't figured out how to make these work yet
	'((180 :foreground "red")
		(170 :foreground "yellow")
		(160 :foreground "green")))
(defvar my-live-speech-wpm-string nil "Add this somewhere in `mode-line-format'.")
(defun my-live-speech-wpm-string ()
	(propertize
	 (format "%d WPM " my-live-speech-wpm)
	 'face
	 (cdr (seq-find (lambda (row) (> my-live-speech-wpm (car row))) my-live-speech-wpm-colors))))

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
;; Display words per minute:1 ends here

;; Append to EmacsConf Etherpad
;; :PROPERTIES:
;; :CUSTOM_ID: append-to-emacsconf-etherpad
;; :END:


;; [[file:Sacha.org::#append-to-emacsconf-etherpad][Append to EmacsConf Etherpad:1]]
(defvar my-live-speech-etherpad-id nil)
(defun my-live-speech-append-to-etherpad (recognition-results)
	(when my-live-speech-etherpad-id
		(emacsconf-pad-append-text my-live-speech-etherpad-id (concat " " (assoc-default 'transcript recognition-results)))))
;; Append to EmacsConf Etherpad:1 ends here

;; UTF-8
;; :PROPERTIES:
;; :CUSTOM_ID: utf-8
;; :END:

;; From http://www.wisdomandwonder.com/wordpress/wp-content/uploads/2014/03/C3F.html


;; [[file:Sacha.org::#utf-8][UTF-8:1]]
(prefer-coding-system 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))
;; UTF-8:1 ends here

;; Wdiff
;; :PROPERTIES:
;; :CUSTOM_ID: wdiff
;; :EXPORT_MODIFIED: 2024-10-20T08:22:22-0400
;; :END:

;; #+begin_update
;; [2024-10-20]: I switched to using proper font-locking, yay.
;; #+end_update

;; This uses the [[https://www.gnu.org/software/wdiff/][wdiff]] tool for word-based diffs.


;; [[file:Sacha.org::#wdiff][Wdiff:1]]
(defvar my-wdiff-mode-font-lock-keywords
  `(("{\\+\\(.*?\\)\\+}" . 'diff-added)
    ("\\[\\-\\(.*?\\)\\-\\]" . 'diff-removed)))

(defconst my-wdiff-mode-font-lock-defaults
  '(my-wdiff-mode-font-lock-keywords t nil nil nil (font-lock-multiline . t)))

(define-derived-mode my-wdiff-mode fundamental-mode "Word diff" "Highlight word diffs."
	(setq-local font-lock-defaults my-wdiff-mode-font-lock-defaults))

(defun my-wdiff (old-file new-file)
	(interactive (list (read-file-name "Original: ")
										 (buffer-file-name)))
	(with-current-buffer (get-buffer-create "*wdiff*")
		(erase-buffer)
		(call-process "wdiff" nil t t (expand-file-name old-file)
									(expand-file-name new-file))
		(goto-char (point-min))
		(my-wdiff-mode)
		(switch-to-buffer (current-buffer))))

(defun my-wdiff-buffer-with-file ()
	(interactive)
	(let ((s (buffer-string))
				(temp-file (make-temp-file "temp")))
		(with-temp-file temp-file
			(insert s))
		(my-wdiff (buffer-file-name) temp-file)
		(delete-file temp-file)))
;; Wdiff:1 ends here

;; Org Mode                                                              :org:
;; :PROPERTIES:
;; :CUSTOM_ID: org
;; :END:

;; I use [[http://www.orgmode.org][Org Mode]] to take notes, publish my blog, and do all sorts of
;; stuff.

;; #+NAME: org-package-setup

;; [[file:Sacha.org::org-package-setup][org-package-setup]]
(defvar my-org-inbox-file "~/sync/orgzly/Inbox.org")
(use-package org
  :load-path ("~/vendor/org-mode/lisp" "~/vendor/org-mode/contrib/lisp")
  :bind
  (:map org-mode-map
        ("C-M-<return>" . org-insert-subheading))
	:custom
	(org-export-with-sub-superscripts nil)
	(org-fold-catch-invisible-edits 'smart))
;; org-package-setup ends here

;; Move Org Mode properties from subtree to parent
;; :PROPERTIES:
;; :CREATED:  [2024-09-16 Mon 14:04]
;; :CUSTOM_ID: my-org-move-properties-to-parent
;; :END:

;; Sometimes I set Org properties on a subtree when I meant to set them on the parent heading.
;; This function moves all the properties up one level.


;; [[file:Sacha.org::#my-org-move-properties-to-parent][Move Org Mode properties from subtree to parent:1]]
(defun my-org-move-properties-to-parent ()
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
;; Move Org Mode properties from subtree to parent:1 ends here

;; Modules
;; :PROPERTIES:
;; :CUSTOM_ID: modules
;; :END:
;; Org has a whole bunch of optional modules. These are the ones I'm
;; currently experimenting with.

;; [[file:Sacha.org::#modules][Modules:1]]
(setq org-modules '(org-habit
                    org-mouse
                    org-protocol
                    org-annotate-file
                    org-eval
                    org-expiry
                    org-interactive-query
                    org-collector
                    org-panel
                    org-screen
                    org-toc))
(eval-after-load 'org
  '(org-load-modules-maybe t))
;; Prepare stuff for org-export-backends
(setq org-export-backends '(org latex icalendar html ascii))
;; Modules:1 ends here

;; Keyboard shortcuts
;; :PROPERTIES:
;; :CUSTOM_ID: keyboard-shortcuts
;; :END:


;; [[file:Sacha.org::#keyboard-shortcuts][Keyboard shortcuts:1]]
(bind-key "C-c r" 'org-capture)
(bind-key "C-c a" 'org-agenda)
(bind-key "C-c l" 'org-store-link)
(bind-key "C-c L" 'org-insert-link-global)
(bind-key "C-c O" 'org-open-at-point-global)
;; Keyboard shortcuts:1 ends here



;; =append-next-kill= is more useful to me than =org-table-copy-region=.


;; [[file:Sacha.org::#keyboard-shortcuts][Keyboard shortcuts:2]]
(with-eval-after-load 'org
  (bind-key "C-M-w" 'append-next-kill org-mode-map)
  (bind-key "C-TAB" 'org-cycle org-mode-map)
  (bind-key "C-c v" 'org-show-todo-tree org-mode-map)
  (bind-key "C-c C-r" 'org-refile org-mode-map)
  (bind-key "C-c R" 'org-reveal org-mode-map)
  (bind-key "C-c o" 'my-org-follow-entry-link org-mode-map)
  (bind-key "C-c d" 'my-org-move-line-to-destination org-mode-map)
  (bind-key "C-c t s"  'my-split-sentence-and-capitalize org-mode-map)
  (bind-key "C-c t -"  'my-split-sentence-delete-word-and-capitalize org-mode-map)
  (bind-key "C-c t d"  'my-delete-word-and-capitalize org-mode-map)

  (bind-key "C-c C-p C-p" 'my-org-publish-maybe org-mode-map)
  (bind-key "C-c C-r" 'my-org-refile-and-jump org-mode-map))
;; Keyboard shortcuts:2 ends here



;; I don't use the diary, but I do use the clock a lot.


;; [[file:Sacha.org::#keyboard-shortcuts][Keyboard shortcuts:3]]
(with-eval-after-load 'org-agenda
  (bind-key "i" 'org-agenda-clock-in org-agenda-mode-map))
;; Keyboard shortcuts:3 ends here

;; Speed commands
;; :PROPERTIES:
;; :CUSTOM_ID: speed-commands
;; :END:

;; These are great for quickly acting on tasks.

;; - hello
;;   - world
;;   - this
;; - world here




;; [[file:Sacha.org::#speed-commands][Speed commands:1]]
(setq org-use-effective-time t)

(defun my-org-use-speed-commands-for-headings-and-lists ()
  "Activate speed commands on list items too."
  (or (and (looking-at org-outline-regexp) (looking-back "^\**" nil))
      (save-excursion (and (looking-at (org-item-re)) (looking-back "^[ \t]*" nil)))))
(setq org-use-speed-commands 'my-org-use-speed-commands-for-headings-and-lists)

(defun my-org-subtree-text ()
  (save-excursion
    (buffer-substring (save-excursion (org-end-of-meta-data t) (point)) (org-end-of-subtree))))

(defun my-org-mark-done ()
  (interactive)
  (my-org-with-current-task (org-todo "DONE")))
(defun my-org-mark-done-and-add-to-journal (&optional note category)
  (interactive (list (if current-prefix-arg
                         (read-string (format "Note (%s): " (org-get-heading t t t t)))
                       (org-get-heading t t t t))
                     (or (org-entry-get (point) "JOURNAL_CAT") (my-journal-read-category (my-journal-guess-category)))))
  (my-org-with-current-task
   (org-todo "DONE")
   (org-entry-put (point) "JOURNAL_CAT" category)
   (let* ((title (or note (org-get-heading t t t t)))
          (zid (org-entry-get (point) "ZIDSTRING"))
          (other (if current-prefix-arg (substring-no-properties (my-org-subtree-text))))
          (date (unless zid
                         (format-time-string "%Y-%m-%d %H:%M"
                                             (let ((base-date (org-read-date nil t (org-entry-get (point) "CREATED"))))
                                               (if (string-match "Yesterday " title)
                                                   (progn
                                                     (setq title (replace-match "" nil nil title))
                                                     (org-read-date nil t "--1" nil (org-time-string-to-time (org-entry-get (point) "CREATED"))))
                                                 base-date))))))
     (if zid
         (my-journal-update (list :ZIDString zid :Note title :Category category :Other other))
       (org-entry-put (point) "ZIDSTRING"
                      (plist-get
                       (my-journal-post title
                                        :Category category
                                        :Other other
                                        :Date date)
                       :ZIDString)))
     (org-back-to-heading)
     (my-copy-observation))))

(with-eval-after-load 'org
  (let ((listvar (if (boundp 'org-speed-commands) 'org-speed-commands
                   'org-speed-commands-user)))
    (add-to-list listvar '("A" org-archive-subtree-default))
    (add-to-list listvar '("x" org-todo "DONE"))
    (add-to-list listvar '("X" call-interactively 'my-org-mark-done-and-add-to-journal))
    (add-to-list listvar '("y" org-todo-yesterday "DONE"))
    (add-to-list listvar '("!" my-org-clock-in-and-track))
    (add-to-list listvar '("s" call-interactively 'org-schedule))
    (add-to-list listvar '("d" my-org-move-line-to-destination))
    (add-to-list listvar '("i" call-interactively 'org-clock-in))
    (add-to-list listvar '("o" call-interactively 'org-clock-out))
    (add-to-list listvar '("$" call-interactively 'org-archive-subtree)))
  (bind-key "!" 'my-org-clock-in-and-track org-agenda-mode-map))
;; Speed commands:1 ends here

;; Org navigation
;; :PROPERTIES:
;; :CUSTOM_ID: org-navigation
;; :END:

;; From http://stackoverflow.com/questions/15011703/is-there-an-emacs-org-mode-command-to-jump-to-an-org-heading

;; [[file:Sacha.org::#org-navigation][Org navigation:1]]
(setq org-goto-interface 'outline-path-completion
      org-goto-max-level 10)
(require 'imenu)
(setq org-startup-folded nil)
(bind-key "C-c j" 'org-clock-goto) ;; jump to current task from anywhere
(bind-key "C-c C-w" 'org-refile)
(setq org-cycle-include-plain-lists 'integrate)
(setq org-catch-invisible-edits 'show-and-error)
;; Org navigation:1 ends here

;; Link Org subtrees and navigate between them
;; :PROPERTIES:
;; :CUSTOM_ID: link-org-subtrees-and-navigate-between-them
;; :END:
;; The following code makes it easier for me to link trees with entries, as in http://sachachua.com/evil-plans


;; [[file:Sacha.org::#link-org-subtrees-and-navigate-between-them][Link Org subtrees and navigate between them:1]]
(defun my-org-follow-entry-link ()
  "Follow the defined link for this entry."
  (interactive)
  (if (org-entry-get (point) "LINK")
      (org-open-link-from-string (org-entry-get (point) "LINK"))
    (org-open-at-point)))

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
;; Link Org subtrees and navigate between them:1 ends here

;; Viewing, navigating, and editing the Org tree
;; :PROPERTIES:
;; :CUSTOM_ID: viewing-navigating-and-editing-the-org-tree
;; :END:

;; I often cut and paste subtrees. This makes it easier to cut
;; something and paste it elsewhere in the hierarchy.

;; [[file:Sacha.org::#viewing-navigating-and-editing-the-org-tree][Viewing, navigating, and editing the Org tree:1]]
(with-eval-after-load 'org
  (bind-key "C-c k" 'org-cut-subtree org-mode-map)
  (setq org-yank-adjusted-subtrees t))
;; Viewing, navigating, and editing the Org tree:1 ends here

;; Finding my place on a small mobile screen with org-back-to-heading
;; :PROPERTIES:
;; :CUSTOM_ID: finding-my-place-on-a-small-mobile-screen-with-org-back-to-heading
;; :END:

;; There's probably a better way to do this. I'm surprised
;; org-back-to-heading isn't interactive yet. It's useful.


;; [[file:Sacha.org::#finding-my-place-on-a-small-mobile-screen-with-org-back-to-heading][Finding my place on a small mobile screen with org-back-to-heading:1]]
(defun my-org-back-to-heading ()
  (interactive)
  (org-back-to-heading))

(use-package org
  :bind (:map org-mode-map
              ("C-c b" . my-org-back-to-heading)
              ("C-c p" . org-display-outline-path)))
;; Finding my place on a small mobile screen with org-back-to-heading:1 ends here

;; Dealing with big tables
;; :PROPERTIES:
;; :CUSTOM_ID: dealing-with-big-tables
;; :END:

;; Sometimes I forget where I am in a big table. This would be nice to turn into a minor mode someday.


;; [[file:Sacha.org::#dealing-with-big-tables][Dealing with big tables:1]]
(defun my-org-show-row-and-column (point)
  (interactive "d")
  (save-excursion
    (goto-char point)
    (let ((row (s-trim (org-table-get nil 1)))
          (col (s-trim (org-table-get 1 nil)))
          (message-log-max nil))
      (message "%s - %s" row col))))
;; Dealing with big tables:1 ends here

;; Taking notes
;; :PROPERTIES:
;; :CUSTOM_ID: taking-notes
;; :END:


;; [[file:Sacha.org::#taking-notes][Taking notes:1]]
(setq org-directory "~/sync/orgzly/")
(setq org-default-notes-file "~/sync/orgzly/organizer.org")
;; Taking notes:1 ends here

;; Date trees
;; :PROPERTIES:
;; :CUSTOM_ID: date-trees
;; :END:

;; This quickly adds a same-level heading for the succeeding day.

;; [[file:Sacha.org::#date-trees][Date trees:1]]
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
;; Date trees:1 ends here

;; Templates
;; :PROPERTIES:
;; :CUSTOM_ID: templates
;; :END:

;; I use =org-capture= templates to quickly jot down tasks, ledger
;; entries, notes, and other semi-structured pieces of information.

;; [[file:Sacha.org::#templates][Templates:1]]
(defun my-org-contacts-template-email (&optional return-value)
  "Try to return the contact email for a template.
         If not found return RETURN-VALUE or something that would ask the user."
  (eval-when-compile (require 'gnus-art nil t))
  (eval-when-compile (require 'org-contacts nil t))
  (or (cadr (if (gnus-alive-p)
                (gnus-with-article-headers
                  (mail-extract-address-components
                   (or (mail-fetch-field "Reply-To") (mail-fetch-field "From") "")))))
      return-value
      (concat "%^{" org-contacts-email-property "}p")))

(defvar my-org-basic-task-template "* TODO %^{Task}
         :PROPERTIES:
         :Effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}
         :END:
         Captured %<%Y-%m-%d %H:%M>
         %?

         %i
         " "Basic task data")
(defvar my-ledger-file "~/cloud/ledger/current.ledger")
(with-eval-after-load 'org-capture
	(setq org-capture-templates
				(seq-uniq
				 (append

      `(("r" "Note" entry
         (file ,my-org-inbox-file)
         "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n%i\n\n- %a"
         :prepend t)
				("t" "Task with annotation" entry
         (file ,my-org-inbox-file)
         "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n%a\n"
         :prepend t)
        ("i" "Interrupting task" entry
         (file ,my-org-inbox-file)
         "* STARTED %^{Task}\n:PROPERTIES:\n:CREATED: %U\n:END:\n%a\n"
         :clock-in :clock-resume
         :prepend t)
				("T" "Task without annotation" entry
         (file ,my-org-inbox-file)
         "* TODO %^{Task}\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n"
         :prepend t)
        ;; From https://takeonrules.com/2022/10/16/adding-another-function-to-my-workflow/
        ("c" "Contents to current clocked task"
	       plain (clock)
	       "%i%?"
	       :empty-lines 1)
        ;; ("p" "Podcast log - timestamped" item
        ;;  (file+olp+datetree "~/sync/orgzly/timestamped.org")
        ;;  "%<%H:%M:%S,%3N> %^{Note}"
        ;;  :immediate-finish t)
        ;; ("b" "Plover note" table-line
        ;;  (file+headline "~/proj/plover-notes/README.org" "Brief notes")
        ;;  "| %^{Stroke} | %^{Translation} | %^{Note} |"
        ;;  :immediate-finish t)
        ;; ("c" "Plover review from clippy" table-line
        ;;  (file+headline "~/proj/plover-notes/README.org" "For review")
        ;;  "%(let ((last (my-clippy-last))) (format \"| %s | %s |\" (car last) (cdr last)))"
        ;;  :immediate-finish t)

        ("." "Today" entry
         (file ,my-org-inbox-file)
         "* TODO %^{Task}\nSCHEDULED: %t\n:PROPERTIES:\n:CREATED: %U\n:END:\n"
         :immediate-finish t)
        ("v" "Video" entry
         (file ,my-org-inbox-file)
         "* TODO %^{Task}  :video:\nSCHEDULED: %t\n"
         :immediate-finish t)
        ("e" "Errand" entry
         (file ,my-org-inbox-file)
         "* TODO %^{Task}  :errands:\n:PROPERTIES:\n:CREATED: %U\n:END:\n"
         :immediate-finish t)
        ("n" "Note" entry
         (file ,my-org-inbox-file)
         "* %^{Note}\n:PROPERTIES:\n:CREATED: %U\n:END:\n"
         :immediate-finish t)
        ("N" "Note" entry
         (file ,my-org-inbox-file)
         "* %^{Note}\n:PROPERTIES:\n:CREATED: %U\n:END:\n"
         :prepend t)
				("s" "Screenshot" entry
				 (file ,my-org-inbox-file)
				 "* %^{Note}\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n[[file:%(my-latest-file my-screenshot-directory)]]\n"
				 :prepend t)
        ("b" "Business task" entry
         (file+headline "~/personal/business.org" "Tasks")
         ,my-org-basic-task-template)
        ("j" "Journal entry" plain
         (file+olp+datetree "~/sync/orgzly/journal.org")
         "%K - %a\n%i\n%?\n"
         :unnarrowed t)
        ("db" "Done - Business" entry
         (file+headline "~/personal/business.org" "Tasks")
         "* DONE %^{Task}\nSCHEDULED: %^t\n%?")
        ("dp" "Done - People" entry
         (file+headline "~/personal/people.org" "Tasks")
         "* DONE %^{Task}\nSCHEDULED: %^t\n%?")
        ("dt" "Done - Task" entry
         (file+headline "~/sync/orgzly/organizer.org" "Inbox")
         "* DONE %^{Task}\nSCHEDULED: %^t\n%?")
        ("q" "Quick note" item
         (file+headline "~/sync/orgzly/organizer.org" "Quick notes"))
        ("l" "Ledger")
        ("lc" "Cash expense" plain
         (file ,my-ledger-file)
         "%(ledger-read-date \"Date: \") * %^{Payee}
             Expenses:Cash
             Expenses:%^{Account}  %^{Amount}
           ")
        ("lb" "BDO CAD" plain
         (file ,my-ledger-file)
         "%(ledger-read-date \"Date: \") * %^{Payee}
             Expenses:Play    $ %^{Amount}
             Assets:BDO
           ")
        ("lp" "BDO PHP" plain
         (file ,my-ledger-file)
         "%(ledger-read-date \"Date: \") * %^{Payee}
             Expenses:Play    PHP %^{Amount}
             Assets:BDO
           ")
        ("B" "Book" entry
         (file+datetree "~/personal/books.org" "Inbox")
         "* %^{Title}  %^g
           %i
           *Author(s):* %^{Author} \\\\
           *ISBN:* %^{ISBN}

           %?

           *Review on:* %^t \\
           %a
           %U"
         :clock-in :clock-resume)
        ("C" "Contact" entry (file "~/sync/orgzly/people.org")
         "* %(org-contacts-template-name)
:PROPERTIES:
:EMAIL: %(my-org-contacts-template-email)
:END:")
				("y" "Yay Emacs" entry (file+headline "~/proj/stream/index.org" "Notes for this session")
				 "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n

%i

%a
"))
			org-capture-templates))))
(bind-key "C-M-r" 'org-capture)



;;(bind-key (kbd "<f5>") 'org-capture)
;; Templates:1 ends here

;; Allow refiling in the middle(ish) of a capture
;; :PROPERTIES:
;; :CUSTOM_ID: allow-refiling-in-the-middle-ish-of-a-capture
;; :END:

;; This lets me use =C-c C-r= to refile a capture and then jump to the
;; new location. I wanted to be able to file tasks under projects so that
;; they could inherit the QUANTIFIED property that I use to track time
;; (and any Beeminder-related properties too), but I also wanted to be
;; able to clock in on them.


;; [[file:Sacha.org::#allow-refiling-in-the-middle-ish-of-a-capture][Allow refiling in the middle(ish) of a capture:1]]
(defun my-org-refile-and-jump ()
  (interactive)
  (if (derived-mode-p 'org-capture-mode)
      (org-capture-refile)
    (call-interactively 'org-refile))
  (org-refile-goto-last-stored))
(eval-after-load 'org-capture
  '(bind-key "C-c C-r" 'my-org-refile-and-jump org-capture-mode-map))
;; Allow refiling in the middle(ish) of a capture:1 ends here

;; Try out this capture command
;; :PROPERTIES:
;; :CUSTOM_ID: try-out-this-capture-command
;; :END:

;; From https://takeonrules.com/2022/10/16/adding-another-function-to-my-workflow/


;; [[file:Sacha.org::#try-out-this-capture-command][Try out this capture command:1]]
(use-package git-link)
(bind-key "C-c c" 'jf/capture-region-contents-with-metadata)
(defun jf/capture-region-contents-with-metadata (start end parg)
  "Write selected text between START and END to currently clocked `org-mode' entry.

With PARG kill the content instead."
  (interactive "r\nP")
  (let ((text (jf/region-contents-get-with-metadata start end)))
    (if (car parg)
	(kill-new text)
      (org-capture-string (concat "-----\n" text) "c"))))
(defun jf/region-contents-get-with-metadata (start end)
      "Get the region contents between START and END and return an `org-mode' formatted string."
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
;; Try out this capture command:1 ends here

;; Estimating WPM
;; :PROPERTIES:
;; :CUSTOM_ID: estimating-wpm
;; :END:

;; I'm curious about how fast I type some things.

;; [[file:Sacha.org::#estimating-wpm][Estimating WPM:1]]
(require 'org-clock)
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
;; Estimating WPM:1 ends here

;; Logbook
;; :PROPERTIES:
;; :CUSTOM_ID: logbook
;; :END:

;; [[file:Sacha.org::#logbook][Logbook:1]]
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
;; Logbook:1 ends here

;; Track TODO state
;; :PROPERTIES:
;; :CUSTOM_ID: todo-keywords
;; :END:
;; <<todo-keywords>>

;; The parentheses indicate keyboard shortcuts that I can use to set the
;; task state. =@= and =!= toggle logging. =@= prompts you for a note,
;; and =!= automatically logs the timestamp of the state change.


;; [[file:Sacha.org::#todo-keywords][Track TODO state:1]]
(setq org-todo-keywords
      '((sequence
         "STARTED(s)"
         "TODO(t)"  ; next action
         "TOBLOG(b)"  ; next action
         "WAITING(w@/!)"
         "SOMEDAY(.)" "BLOCKED(k@/!)" "|" "DONE(x!)" "CANCELLED(c)")
        (sequence "PROJECT" "|" "DONE(x)")
        (sequence "LEARN" "TRY" "TEACH" "|" "COMPLETE(x)")
        (sequence "TOSKETCH" "SKETCHED" "|" "POSTED")
        (sequence "TOBUY" "TOSHRINK" "TOCUT"  "TOSEW" "|" "DONE(x)")
        (sequence "TODELEGATE(-)" "DELEGATED(d)" "|" "COMPLETE(x)")))
;; Track TODO state:1 ends here

;; [[file:Sacha.org::#todo-keywords][Track TODO state:2]]
(setq org-log-done 'time)
;; Track TODO state:2 ends here

;; TODO Change Org Mode TODO keyword color based on the state and the current Modus theme
;; :PROPERTIES:
;; :CUSTOM_ID: my-org-todo-set-keyword-faces
;; :EXPORT_DATE: 2024-10-14T17:02:45-0400
;; :EXPORT_ELEVENTY_PERMALINK: /blog/2024/10/change-org-mode-todo-keyword-color-based-on-the-state-and-the-current-modus-theme/
;; :EXPORT_ELEVENTY_FILE_NAME: blog/2024/10/change-org-mode-todo-keyword-color-based-on-the-state-and-the-current-modus-theme/
;; :END:

;; I use ~modus-theme-toggle~ to switch between
;; ~modus-vivendi-tinted~ and ~modus-operandi-tinted~
;; depending on whether I want a dark background or a
;; light one. I also customize my
;; ~org-todo-keyword-faces~ to visually distinguish
;; TODO, DONE, WAITING, and SOMEDAY. This is how to
;; colour them based on the current Modus theme.


;; [[file:Sacha.org::#my-org-todo-set-keyword-faces][Change Org Mode TODO keyword color based on the state and the current Modus theme:1]]
(defun my-org-todo-set-keyword-faces ()
	(setq org-todo-keyword-faces
				`(("TODO" . (:foreground ,(modus-themes-get-color-value 'blue-warmer) :weight bold))
					("DONE" . (:foreground ,(modus-themes-get-color-value 'green-warmer) :weight bold))
					("WAITING" . (:foreground ,(modus-themes-get-color-value 'red-warmer) :weight bold))
					("SOMEDAY" . (:foreground ,(modus-themes-get-color-value 'fg-dim) :weight bold))))
	(when (derived-mode-p 'org-mode)
		(font-lock-fontify-buffer)))
(with-eval-after-load 'modus-themes
	(add-hook 'modus-themes-after-load-theme-hook #'my-org-todo-set-keyword-faces))
;; Change Org Mode TODO keyword color based on the state and the current Modus theme:1 ends here

;; Projects
;; :PROPERTIES:
;; :CUSTOM_ID: projects
;; :END:

;; Projects are headings with the =:project:= tag, so we generally don't
;; want that tag inherited, except when we display unscheduled tasks that
;; don't belong to any projects.


;; [[file:Sacha.org::#projects][Projects:1]]
(setq org-tags-exclude-from-inheritance '("project" "inboxtarget"))
;; Projects:1 ends here



;; This code makes it easy for me to focus on one project and its tasks.


;; [[file:Sacha.org::#projects][Projects:2]]
(with-eval-after-load 'org
  (let ((listvar (if (boundp 'org-speed-commands) 'org-speed-commands
                   'org-speed-commands-user)))
    (add-to-list listvar '("N" org-narrow-to-subtree))
    (add-to-list listvar '("W" widen))
    (add-to-list listvar '("T" my-org-agenda-for-subtree))
    (add-to-list listvar '("b" my-org-bounce-to-file))))

(defun my-org-agenda-for-subtree ()
  (interactive)
  (when (derived-mode-p 'org-agenda-mode) (org-agenda-switch-to))
  (my-org-with-current-task
   (let ((org-agenda-view-columns-initially t))
     (org-agenda nil "t" 'subtree))))
;; Projects:2 ends here



;; There's probably a proper way to do this, maybe with =<=. Oh, that would work nicely. =< C-c a t= too.

;; And sorting:


;; [[file:Sacha.org::#projects][Projects:3]]
(with-eval-after-load 'org
  (let ((listvar (if (boundp 'org-speed-commands) 'org-speed-commands
                   'org-speed-commands-user)))
    (add-to-list listvar '("S" call-interactively 'org-sort))))
;; Projects:3 ends here

;; Tag tasks with GTD-ish contexts
;; :PROPERTIES:
;; :CUSTOM_ID: tag-tasks-with-gtd-ish-contexts
;; :END:

;; This defines keyboard shortcuts for those, too.


;; [[file:Sacha.org::#tag-tasks-with-gtd-ish-contexts][Tag tasks with GTD-ish contexts:1]]
(setq org-tag-alist '(("work" . ?b)
                      ("home" . ?h)
                      ("writing" . ?w)
                      ("errands" . ?e)
                      ("drawing" . ?d)
                      ("coding" . ?c)
                      ("video" . ?v)
                      ("kaizen" . ?k)
                      ("phone" . ?p)
                      ("learning" . ?a)
                      ("reading" . ?r)
                      ("computer" . ?l)
                      ("quantified" . ?q)
                      ("shopping" .?s)
                      ("focus" . ?f)))
;; Tag tasks with GTD-ish contexts:1 ends here

;; Enable filtering by effort estimates
;; :PROPERTIES:
;; :CUSTOM_ID: enable-filtering-by-effort-estimates
;; :END:

;; That way, it's easy to see short tasks that I can finish.


;; [[file:Sacha.org::#enable-filtering-by-effort-estimates][Enable filtering by effort estimates:1]]
(add-to-list 'org-global-properties
             '("Effort_ALL". "0:05 0:15 0:30 1:00 2:00 3:00 4:00"))
;; Enable filtering by effort estimates:1 ends here

;; Track time
;; :PROPERTIES:
;; :CUSTOM_ID: track-time
;; :END:


;; [[file:Sacha.org::#track-time][Track time:1]]
(use-package org
  :init
  (progn
    (setq org-expiry-inactive-timestamps t)
    (setq org-clock-idle-time nil)
    (setq org-log-done 'time)
    (setq org-clock-auto-clock-resolution nil)
    (setq org-clock-continuously nil)
    (setq org-clock-persist t)
    (setq org-clock-in-switch-to-state "STARTED")
    (setq org-clock-in-resume nil)
    (setq org-show-notification-handler 'message)
    (setq org-clock-report-include-clocking-task t))
  :config
  (org-clock-persistence-insinuate))
;; Track time:1 ends here



;; Too many clock entries clutter up a heading.


;; [[file:Sacha.org::#track-time][Track time:2]]
(setq org-log-into-drawer "LOGBOOK")
(setq org-clock-into-drawer 1)
;; Track time:2 ends here

;; Habits
;; :PROPERTIES:
;; :CUSTOM_ID: habits
;; :END:

;; I like using org-habits to track consistency. My task names tend
;; to be a bit long, though, so I've configured the graph column to
;; show a little bit more to the right.


;; [[file:Sacha.org::#habits][Habits:1]]
(setq org-habit-graph-column 80)
(setq org-habit-show-habits-only-for-today nil)
;; Habits:1 ends here

;; Estimating tasks
;; :PROPERTIES:
;; :CUSTOM_ID: subset
;; :END:

;; From "Add an effort estimate on the fly when clocking in" on the
;; [[http://orgmode.org/worg/org-hacks.html][Org Hacks]] page:


;; [[file:Sacha.org::#subset][Estimating tasks:1]]
(add-hook 'org-clock-in-prepare-hook
          'my-org-mode-ask-effort)

(defun my-org-mode-ask-effort ()
  "Ask for an effort estimate when clocking in."
  (unless (org-entry-get (point) "Effort")
    (let ((effort
           (completing-read
            "Effort: "
            (org-entry-get-multivalued-property (point) "Effort"))))
      (unless (equal effort "")
        (org-set-property "Effort" effort)))))
;; Estimating tasks:1 ends here

;; Flexible scheduling of tasks
;; :PROPERTIES:
;; :CUSTOM_ID: flexible-scheduling-of-tasks
;; :END:

;; I (theoretically) want to be able to schedule tasks for dates like the first Saturday
;; of every month. Fortunately, [[http://stackoverflow.com/questions/13555385/org-mode-how-to-schedule-repeating-tasks-for-the-first-saturday-of-every-month][someone else has figured that out!]]


;; [[file:Sacha.org::#flexible-scheduling-of-tasks][Flexible scheduling of tasks:1]]
;; Get this from https://raw.github.com/chenfengyuan/elisp/master/next-spec-day.el
(load "~/elisp/next-spec-day.el" t)
;; Flexible scheduling of tasks:1 ends here

;; Task dependencies
;; :PROPERTIES:
;; :CUSTOM_ID: task-dependencies
;; :END:


;; [[file:Sacha.org::#task-dependencies][Task dependencies:1]]
(setq org-enforce-todo-dependencies t)
(setq org-track-ordered-property-with-tag t)
(setq org-agenda-dim-blocked-tasks t)
;; Task dependencies:1 ends here

;; Quick way to archive all DONE from inbox                :emacs:computer:
;; :PROPERTIES:
;; :CUSTOM_ID: quick-way-to-archive-all-done-from-inbox
;; :END:


;; [[file:Sacha.org::#quick-way-to-archive-all-done-from-inbox][Quick way to archive all DONE from inbox:1]]
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

(defun my-org-archive-done-tasks (&optional scope)
  "Archive finished or cancelled tasks.
       SCOPE can be 'file or 'tree."
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (outline-previous-heading)))
   "TODO=\"DONE\"|TODO=\"CANCELLED\"" (or scope (if (org-before-first-heading-p) 'file 'tree))))
;; Quick way to archive all DONE from inbox:1 ends here

;; Structure templates
;; :PROPERTIES:
;; :CUSTOM_ID: structure-templates
;; :END:

;; Org makes it easy to insert blocks by typing =<s[TAB]=, etc.
;; I hardly ever use LaTeX, but I insert a lot of Emacs Lisp blocks, so I
;; redefine =<l= to insert a Lisp block instead.


;; [[file:Sacha.org::#structure-templates][Structure templates:1]]
(setq org-structure-template-alist
      '(("a" . "export ascii")
        ("C" . "center")
        ("c" . "comment")
				("d" . "my_details")
        ("e" . "example")
        ("E" . "export")
        ("m" . "export md")
        ("h" . "export html")
        ("j" . "src js :spookfox t")
        ("l" . "src emacs-lisp")
        ("p" . "src python")
        ("n" . "notes")
        ("q" . "quote")
        ("s" . "src")
        ("S" . "src sh")
        ("u" . "update")
        ("v" . "verse")))
;; Structure templates:1 ends here



;; This lets me nest quotes. http://emacs.stackexchange.com/questions/2404/exporting-org-mode-nested-blocks-to-html


;; [[file:Sacha.org::#structure-templates][Structure templates:2]]
(defun my-org-html-quote2 (block backend info)
  (when (org-export-derived-backend-p backend 'html)
    (when (string-match "\\`<div class=\"quote2\">" block)
      (setq block (replace-match "<blockquote>" t nil block))
      (string-match "</div>\n\\'" block)
      (setq block (replace-match "</blockquote>\n" t nil block))
      block)))
(eval-after-load 'ox
  '(add-to-list 'org-export-filter-special-block-functions 'my-org-html-quote2))
;; Structure templates:2 ends here

;; Demarcate, but for all blocks                            :emacs:config:
;; :PROPERTIES:
;; :CUSTOM_ID: demarcate-but-for-begin-notes
;; :END:

;; I often want to split an Org Mode block so that I can add stuff in between. This code is based on
;; https://scripter.co/splitting-an-org-block-into-two/ .


;; [[file:Sacha.org::#demarcate-but-for-begin-notes][Demarcate, but for all blocks:1]]
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
;; Demarcate, but for all blocks:1 ends here

;; Emacs chats, Emacs hangouts
;; :PROPERTIES:
;; :CUSTOM_ID: emacs-chats-emacs-hangouts
;; :END:



;; [[file:Sacha.org::#emacs-chats-emacs-hangouts][Emacs chats, Emacs hangouts:1]]
(defun my-org-link-youtube-time (url beg end)
  "Link times of the form h:mm to YouTube video at URL.
       Works on region defined by BEG and END."
  (interactive (list (read-string "URL: " (org-entry-get-with-inheritance "YOUTUBE")) (point) (mark)))
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ((char (if (string-match "\\?" url) "&" "?")))
        (while (re-search-forward "\\(\\([0-9]+\\):\\([0-9]+\\)\\(:\\([0-9]+\\)\\)?\\) ::" nil t)
          (replace-match
           (format "[[%s%st=%sh%sm%ss][%s]] "
                   url
                   char
                   (match-string 2)
                   (match-string 3)
                   (or (match-string 5) "0")
                   (match-string 1)) nil t))))))

(defun my-clean-up-google-hangout-chat ()
  (interactive)
  (save-excursion
    (while (re-search-forward "<hr.*?div class=\"Kc-Ma-m\".*?>" nil t)
      (replace-match "\n| ")))
  (save-excursion
    (while (re-search-forward "</div><div class=\"Kc-yi-m\">" nil t)
      (replace-match " | ")))
  (save-excursion
    (while (re-search-forward "</div></div><div class=\"Kc-ib\">" nil t)
      (replace-match " | ")))
  (save-excursion
    (while (re-search-forward "<a rel=\"nofollow\" target=\"_blank\" href=\"\\(.*?\\)\">\\(.*?\\)</a>" nil t)
      (replace-match "[[\\1][\\2]]")))
  (save-excursion
    (while (re-search-forward "</div></div></div></div>" nil t)
      (replace-match " |")))
  (save-excursion
    (while (re-search-forward "&nbsp;" nil t)
      (replace-match " ")))
  (save-excursion
    (while (re-search-forward "</div><div class=\"Kc-ib\">" nil t)
      (replace-match " ")))
  (save-excursion
    (while (re-search-forward "<img.*?>" nil t)
      (replace-match "")))
  (save-excursion
    (while (re-search-forward "<wbr>" nil t)
      (replace-match "")))
  )
;; Emacs chats, Emacs hangouts:1 ends here

;; Basic configuration
;; :PROPERTIES:
;; :CUSTOM_ID: project_subtasks
;; :END:
;; I have quite a few Org files, but I keep my agenda items and TODOs in
;; only a few of them them for faster scanning.


;; [[file:Sacha.org::#project_subtasks][Basic configuration:1]]
(defvar my-kid-org-file nil "Defined in secrets")
(setq org-agenda-files
      (delq nil
            (mapcar (lambda (x) (and x (file-exists-p x) x))
                    `("~/sync/orgzly/organizer.org"
                      "~/sync/orgzly/Inbox.org"
                      "~/sync/orgzly/garden.org"
                      "~/sync/orgzly/decisions.org"
                      "~/sync/orgzly/computer-inbox.org"
                      "~/sync/orgzly/crafts.org"
                      "~/sync/emacs/Sacha.org"
                      "~/proj/stream/index.org"
                      "~/proj/plover-notes/README.org"
                      "~/personal/sewing.org"
                      "~/sync/orgzly/people.org"
                      "~/sync/orgzly/business.org"
                      "~/Dropbox/wsmef/trip.txt"
                      ,my-kid-org-file
                      "~/personal/orgzly.org"
                      "~/personal/calendar.org"
                      "~/Dropbox/tasker/summary.txt"
                      "~/Dropbox/public/sharing/index.org"
                      "~/dropbox/public/sharing/learning.org"
                      "~/proj/emacs-notes/tasks.org"
                      "~/proj/sachac.github.io/evil-plans/index.org"
                      "~/sync/orgzly/cooking.org"
                      "~/sync/orgzly/routines.org"))))
(setq org-agenda-dim-blocked-tasks nil)
(add-to-list 'auto-mode-alist '("\\.txt$" . org-mode))
;; Basic configuration:1 ends here




;; I like looking at two days at a time when I plan using the Org
;; agenda. I want to see my log entries, but I don't want to see
;; scheduled items that I've finished. I like seeing a time grid so that
;; I can get a sense of how appointments are spread out.


;; [[file:Sacha.org::#project_subtasks][Basic configuration:2]]
(setq org-agenda-span 2)
(setq org-agenda-tags-column -100) ; take advantage of the screen width
(setq org-agenda-sticky nil)
(setq org-agenda-inhibit-startup t)
(setq org-agenda-use-tag-inheritance t)
(setq org-agenda-show-log t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled)
(setq org-agenda-time-grid
      '((daily today require-timed)
        (800 1000 1200 1400 1600 1800 2000)
        "......" "----------------"))
(setq org-columns-default-format "%14SCHEDULED %Effort{:} %1PRIORITY %TODO %50ITEM %TAGS")
;; Basic configuration:2 ends here



;; Some other keyboard shortcuts:


;; [[file:Sacha.org::#project_subtasks][Basic configuration:3]]
(bind-key "Y" 'org-agenda-todo-yesterday org-agenda-mode-map)
;; Basic configuration:3 ends here

;; Starting my weeks on Saturday
;; :PROPERTIES:
;; :CUSTOM_ID: starting-my-weeks-on-saturday
;; :END:

;; I like looking at weekends as [[http://sachachua.com/blog/2010/11/week-beginnings/][week beginnings]] instead, so I want the
;; Org agenda to start on Saturdays.


;; [[file:Sacha.org::#starting-my-weeks-on-saturday][Starting my weeks on Saturday:1]]
(setq org-agenda-start-on-weekday 6)
;; Starting my weeks on Saturday:1 ends here

;; Display projects with associated subtasks
;; :PROPERTIES:
;; :CUSTOM_ID: agenda_commands
;; :END:

;; I wanted a view that showed projects with a few subtasks underneath
;; them. Here's a sample of the output:

;; #+begin_example
;;      Headlines with TAGS match: +PROJECT
;;      Press `C-u r' to search again with new search string
;;        organizer:  Set up communication processes for Awesome Foundation Toronto
;;        organizer:  TODO Announce the next pitch night
;;        organizer:  TODO Follow up with the winner of the previous pitch night for any news to include in the updates

;;        organizer:  Tidy up the house so that I can find things quickly
;;        organizer:  TODO Inventory all the things in closets and boxes         :@home:
;;        organizer:  TODO Drop things off for donation                       :@errands:

;;        organizer:  Learn how to develop for Android devices
;; #+end_example


;; [[file:Sacha.org::#agenda_commands][Display projects with associated subtasks:1]]
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
;; Display projects with associated subtasks:1 ends here

;; Org agenda custom commands
;; :PROPERTIES:
;; :CUSTOM_ID: org-agenda-custom-commands
;; :END:

;; There are quite a few custom commands here, and I often forget to use
;; them. =) But it's good to define them, and over time, I'll get the
;; hang of using these more!

;; | Key         | Description                                                                                    |
;; | .           | What am I waiting for?                                                                         |
;; | T           | Not really an agenda command - shows the to-do tree in the current file                        |
;; | b           | Shows business-related tasks                                                                   |
;; | o           | Shows personal tasks and miscellaneous tasks (o: organizer)                                    |
;; | w           | Show all tasks for the upcoming week                                                           |
;; | W           | Show all tasks for the upcoming week, aside from the routine ones                              |
;; | g ...       | Show tasks by context: b - business; c - coding; w - writing; p - phone; d - drawing, h - home |
;; | 0           | Show common contexts with up to 3 tasks each, so that I can choose what I feel like working on |
;; | ) (shift-0) | Show common contexts with all the tasks associated with them                                   |
;; | 9           | Show common contexts with up to 3 unscheduled tasks each                                       |
;; | ( (shift-9) | Show common contexts with all the unscheduled tasks associated with them                       |
;; | d           | Timeline for today (agenda, clock summary)                                                     |
;; | u           | Unscheduled tasks to do if I have free time                                                    |
;; | U           | Unscheduled tasks that are not part of projects                                                |
;; | P           | Tasks by priority                                                                              |
;; | p           | My projects                                                                                    |
;; | 2           | Projects with tasks                                                                            |


;; [[file:Sacha.org::#org-agenda-custom-commands][Org agenda custom commands:1]]
(bind-key "<apps> a" 'org-agenda)
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
(defun my-org-agenda-skip-scheduled ()
  (org-agenda-skip-entry-if 'scheduled 'deadline 'regexp "\n]+>"))

(use-package org-super-agenda
	:init
	(org-super-agenda-mode 1))
(use-package org-ql)
(defun my-org-projects ()
  (interactive)
(org-ql-search (org-agenda-files)
  '(and (todo "TODO" "WAITING") (ancestors (tags "project")))
  :super-groups '((:auto-parent t))))

(setq org-agenda-custom-commands
      `(("a" "Agenda"
         ((agenda "" ((org-agenda-span 2)))
          ;; (alltodo
          ;;  ""
          ;;  ((org-agenda-overriding-header "")
          ;;   (org-super-agenda-groups
          ;;    '((:name "Inbox, unscheduled"
          ;;             :and (:scheduled nil
					;; 														 :file-path "Inbox.org"
					;; 														 )
          ;;             :order 1)
          ;;      (:name "Important, unscheduled"
          ;;             :and (:priority "A"
          ;;                             :scheduled nil)
          ;;             :order 2)

          ;;      (:name "Project-related, unscheduled"
          ;;             :and (:tag "project" :date nil :todo ("STARTED" "WAITING" "TODO"))
          ;;             :order 3)
          ;;      (:name "Waiting"
          ;;             :and (:todo "WAITING"
          ;;                         :scheduled nil)
          ;;             :order 4)
          ;;      (:discard (:todo "SOMEDAY"
          ;;                       :category "cooking"
          ;;                       :date t))
          ;;      (:name "Unscheduled"
          ;;             :scheduled nil
          ;;             :order 5)
          ;;      (:discard (:anything t))
          ;;      )
          ;;    )))
          ;; (tags-todo "TODO=\"TODO\"-project-cooking-routine-errands-shopping-video-evilplans"
          ;;            ((org-agenda-skip-function 'my-org-agenda-skip-scheduled)
          ;;             (org-agenda-prefix-format "%-6e ")
          ;;             (org-agenda-overriding-header "Unscheduled TODO entries: ")
          ;;             (org-agenda-sorting-strategy '(priority-down effort-up tag-up category-keep))))
          ))
        ("e" "Emacs" tags "emacs")
				("n" "Emacs News" tags "news" ((org-agenda-files '("~/sync/orgzly/Inbox.org"
																													 "~/sync/orgzly/news.org"))))
        ("E" "Emacsconf" tags-todo "emacsconf"
				 ((org-agenda-sorting-strategy '(priority-down effort-up category-keep)))
				 )
        ("i" "Inbox" alltodo ""
         ((org-agenda-files '("~/sync/orgzly/Inbox.org" "~/sync/orgzly/computer-inbox.org"))))
        ("t" tags-todo "-cooking"
         ((org-agenda-sorting-strategy '(todo-state-up priority-down effort-up))))
        ("T" tags-todo "TODO=\"TODO\"-goal-routine-cooking-SCHEDULED={.+}" nil "~/cloud/agenda/nonroutine.html")
        ("f" tags-todo "focus-TODO=\"DONE\"-TODO=\"CANCELLED\"")
        ("b" todo ""
         ((org-agenda-files '("~/sync/orgzly/business.org"))))
        ("B" todo ""
         ((org-agenda-files '("~/Dropbox/books"))))
        ("x" "Column view" todo ""			; Column view
         ((org-agenda-prefix-format "")
          (org-agenda-cmp-user-defined 'my-org-sort-agenda-items-todo)
          (org-agenda-view-columns-initially t)
          ))
        ;; Weekly review
        ("w" "Weekly review" agenda ""
         ((org-agenda-span 7)
          (org-agenda-log-mode 1)) "~/cloud/agenda/this-week.html")
        ("W" "Weekly review sans routines" agenda ""
         ((org-agenda-span 7)
          (org-agenda-log-mode 1)
          (org-agenda-tag-filter-preset '("-routine"))) "~/cloud/agenda/this-week-nonroutine.html")
        ("2" "Bi-weekly review" agenda "" ((org-agenda-span 14) (org-agenda-log-mode 1)))
        ("5" "Quick tasks" tags-todo "EFFORT>=\"0:05\"&EFFORT<=\"0:15\"")
        ("0" "Unestimated tasks" tags-todo "EFFORT=\"\"")
        ("gb" "Business" todo ""
         ((org-agenda-files '("~/sync/orgzly/business.org"))
          (org-agenda-view-columns-initially t)))
        ("gc" "Coding" tags-todo "@coding"
         ((org-agenda-view-columns-initially t)))
        ("gw" "Writing" tags-todo "@writing"
         ((org-agenda-view-columns-initially t)))
        ("gp" "Phone" tags-todo "@phone"
         ((org-agenda-view-columns-initially t)))
        ("gd" "Drawing" tags-todo "@drawing"
         ((org-agenda-view-columns-initially t)))
        ("gh" "Home" tags-todo "@home"
         ((org-agenda-view-columns-initially t)))
        ("gk" "Kaizen" tags-todo "kaizen"
         ((org-agenda-view-columns-initially t))
         ("~/cloud/agenda/kaizen.html"))
        ("ge" "Errands" tags-todo "errands"
         ((org-agenda-view-columns-initially t))
         ("~/cloud/agenda/errands.html"))
        ("c" "Top 3 by context"
         ,my-org-agenda-contexts
         ((org-agenda-sorting-strategy '(priority-up effort-down))
          (org-agenda-max-entries 3)))
        ("C" "All by context"
         ,my-org-agenda-contexts
         ((org-agenda-sorting-strategy '(priority-down effort-down))
          (org-agenda-max-entries nil)))
        ("9" "Unscheduled top 3 by context"
         ,my-org-agenda-contexts
         ((org-agenda-skip-function 'my-org-agenda-skip-scheduled)
          (org-agenda-sorting-strategy '(priority-down effort-down))
          (org-agenda-max-entries 3)))
        ("(" "All unscheduled by context"
         ,my-org-agenda-contexts
         ((org-agenda-skip-function 'my-org-agenda-skip-scheduled)
          (org-agenda-sorting-strategy '(priority-down effort-down))
          ))
        ("d" "Timeline for today" ((agenda "" ))
         ((org-agenda-ndays 1)
          (org-agenda-show-log t)
          (org-agenda-log-mode-items '(clock closed))
          (org-agenda-clockreport-mode t)
          (org-agenda-entry-types '())))
        ("." "Waiting for" todo "WAITING")
        ("u" "Unscheduled tasks" tags-todo "-someday-TODO=\"SOMEDAY\"-TODO=\"DELEGATED\"-TODO=\"WAITING\"-project-cooking-routine"
         ((org-agenda-skip-function 'my-org-agenda-skip-scheduled)
          (org-agenda-view-columns-initially nil)
          (org-tags-exclude-from-inheritance '("project"))
          (org-agenda-overriding-header "Unscheduled TODO entries: ")
          (org-columns-default-format "%50ITEM %TODO %3PRIORITY %Effort{:} %TAGS")
          (org-agenda-sorting-strategy '(todo-state-up priority-down effort-up tag-up category-keep))))
        ("!" "Someday" tags-todo "TODO=\"SOMEDAY\""
         ((org-agenda-skip-function 'my-org-agenda-skip-scheduled)
          (org-agenda-view-columns-initially nil)
          (org-tags-exclude-from-inheritance '("project"))
          (org-agenda-overriding-header "Someday: ")
          (org-columns-default-format "%50ITEM %TODO %3PRIORITY %Effort{:} %TAGS")
          (org-agenda-sorting-strategy '(todo-state-up priority-down effort-up tag-up category-keep))))
        ("U" "Unscheduled tasks outside projects" tags-todo "-project-cooking-routine"
         ((org-agenda-skip-function 'my-org-agenda-skip-scheduled)
          (org-tags-exclude-from-inheritance nil)
          (org-agenda-view-columns-initially nil)
          (org-agenda-overriding-header "Unscheduled TODO entries outside projects: ")
          (org-agenda-sorting-strategy '(todo-state-up priority-down tag-up category-keep effort-down))))
        ("P" "By priority"
         ((tags-todo "+PRIORITY=\"A\"")
          (tags-todo "+PRIORITY=\"B\"")
          (tags-todo "+PRIORITY=\"\"")
          (tags-todo "+PRIORITY=\"C\""))
         ((org-agenda-prefix-format "%-10c %-10T %e ")
          (org-agenda-sorting-strategy '(priority-down tag-up category-keep effort-down))))
        ("pp" tags "+project-someday-TODO=\"DONE\"-TODO=\"SOMEDAY\"-inactive"
         ((org-tags-exclude-from-inheritance '("project"))
          (org-agenda-sorting-strategy '(priority-down tag-up category-keep effort-down))))
        ("p." tags "+project-TODO=\"DONE\""
         ((org-tags-exclude-from-inheritance '("project"))
          (org-agenda-sorting-strategy '(priority-down tag-up category-keep effort-down))))
        ("S" tags-todo "TODO=\"STARTED\"")
        ("C" "Cooking"
         ((tags "vegetables")
          (tags "chicken")
          (tags "beef")
          (tags "pork")
          (tags "other"))
         ((org-agenda-files '("~/sync/orgzly/cooking.org"))
          (org-agenda-view-columns-initially t)
          (org-agenda-sorting-strategy '(scheduled-up time-down todo-state-up)))
         )
        ("8" "List projects with tasks" my-org-agenda-projects-and-tasks
         "+PROJECT"
         ((org-agenda-max-entries 3)))))
;; Org agenda custom commands:1 ends here

;; Shuffling my Org Mode unscheduled tasks  :emacs:org:
;; :PROPERTIES:
;; :EXPORT_DATE: 2024-10-20T19:00:22-0400
;; :EXPORT_ELEVENTY_PERMALINK: /blog/2024/10/shuffling-my-org-mode-unscheduled-tasks/
;; :EXPORT_ELEVENTY_FILE_NAME: blog/2024/10/shuffling-my-org-mode-unscheduled-tasks/
;; :END:

;; I enjoyed listening to [[https://www.artofmanliness.com/character/advice/podcast-1029-treat-your-to-do-list-like-a-river-and-other-mindset-shifts-for-making-better-use-of-your-time/][Podcast #1,029: Treat Your To-Do List Like a River, and Other Mindset Shifts for Making Better Use of Your Time | The Art of Manliness]] (thanks [[https://mastodon.social/@ctietze/113300034618623731][@ctietze@mastodon.social]] for the recommendation) and checking out the [[https://www.artofmanliness.com/character/behavior/autofocus-the-productivity-system-that-treats-your-to-do-list-like-a-river/][Autofocus method]] ([[http://markforster.squarespace.com/autofocus-system/][main website with translations]]) for reviewing your TODO list without worrying too much about prioritization. I also had an opportunity to reflect on similar topics in a conversation with John Wiegley and Adam Porter about personal information management (which I'll be blogging about once John has a chance to review the draft).

;; This nudged me to experiment with randomizing my
;; unscheduled task list. I'm not trying to finish
;; everything on my list, I'm just mixing it up so
;; that I enjoy keeping things on my radar and
;; picking something to do. [[https://github.com/alphapapa/org-ql][org-ql]] lets me create
;; randomly-sorted views, so I wrote some code to
;; show me a shuffled list of my unscheduled TODO
;; tasks and SOMEDAY tasks.

;; #+begin_comment
;; [[elisp:my-org-ql-shuffle-todo][TODO]] - [[elisp:my-org-ql-shuffle-someday][SOMEDAY]]
;; #+end_comment


;; [[file:Sacha.org::*Shuffling my Org Mode unscheduled tasks][Shuffling my Org Mode unscheduled tasks:1]]
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
;; Shuffling my Org Mode unscheduled tasks:1 ends here

;; Making it easier to tag inbox items
;; :PROPERTIES:
;; :CUSTOM_ID: making-it-easier-to-tag-inbox-items
;; :END:


;; [[file:Sacha.org::#making-it-easier-to-tag-inbox-items][Making it easier to tag inbox items:1]]
(setq org-complete-tags-always-offer-all-agenda-tags t)
(setq org-use-fast-tag-selection nil)
;; Making it easier to tag inbox items:1 ends here

;; Make it easy to mark a task as done
;; :PROPERTIES:
;; :CUSTOM_ID: make-it-easy-to-mark-a-task-as-done
;; :END:

;; Great for quickly going through the to-do list. Gets rid of one
;; extra keystroke. ;)


;; [[file:Sacha.org::#make-it-easy-to-mark-a-task-as-done][Make it easy to mark a task as done:1]]
(defun my-org-agenda-done (&optional arg)
  "Mark current TODO as done.
       This changes the line at point, all other lines in the agenda referring to
       the same tree node, and the headline of the tree node in the Org-mode file."
  (interactive "P")
  (org-agenda-todo "DONE"))
;; Override the key definition for org-exit
(define-key org-agenda-mode-map "x" 'my-org-agenda-done)
;; Make it easy to mark a task as done:1 ends here

;; Make it easy to mark a task as done and create a follow-up task
;; :PROPERTIES:
;; :CUSTOM_ID: make-it-easy-to-mark-a-task-as-done-and-create-a-follow-up-task
;; :END:


;; [[file:Sacha.org::#make-it-easy-to-mark-a-task-as-done-and-create-a-follow-up-task][Make it easy to mark a task as done and create a follow-up task:1]]
(defun my-org-agenda-mark-done-and-add-followup ()
  "Mark the current TODO as done and add another task after it.
       Creates it at the same level as the previous task, so it's better to use
       this with to-do items than with projects or headings."
  (interactive)
  (org-agenda-todo "DONE")
  (org-agenda-switch-to)
  (org-capture 0 "t"))
;; Override the key definition
(define-key org-agenda-mode-map "F" 'my-org-agenda-mark-done-and-add-followup)
;; Make it easy to mark a task as done and create a follow-up task:1 ends here

;; Capture something based on the agenda
;; :PROPERTIES:
;; :CUSTOM_ID: capture-something-based-on-the-agenda
;; :END:


;; [[file:Sacha.org::#capture-something-based-on-the-agenda][Capture something based on the agenda:1]]
(defun my-org-agenda-new ()
  "Create a new note or task at the current agenda item.
       Creates it at the same level as the previous task, so it's better to use
       this with to-do items than with projects or headings."
  (interactive)
  (org-agenda-switch-to)
  (org-capture 0))
;; New key assignment
(define-key org-agenda-mode-map "N" 'my-org-agenda-new)
;; Capture something based on the agenda:1 ends here

;; Sorting by date and priority
;; :PROPERTIES:
;; :CUSTOM_ID: sorting-by-date-and-priority
;; :END:


;; [[file:Sacha.org::#sorting-by-date-and-priority][Sorting by date and priority:1]]
(setq org-agenda-sorting-strategy
      '((agenda time-up priority-down tag-up category-keep)
        ;; (todo user-defined-up todo-state-up priority-down effort-up)
        (todo todo-state-up priority-down effort-up)
;        (tags user-defined-up)
        (search category-keep)))
(setq org-agenda-cmp-user-defined 'my-org-sort-agenda-items-user-defined)
(require 'cl)
(defun my-org-get-context (txt)
  "Find the context."
  (car (member-if
        (lambda (item) (string-match "@" item))
        (get-text-property 1 'tags txt))))

(defun my-org-compare-dates (a b)
  "Return 1 if A should go after B, -1 if B should go after A, or 0 if a = b."
  (cond
   ((and (= a 0) (= b 0)) nil)
   ((= a 0) 1)
   ((= b 0) -1)
   ((> a b) 1)
   ((< a b) -1)
   (t nil)))

(defun my-org-complete-cmp (a b)
  (let* ((state-a (or (get-text-property 1 'todo-state a) ""))
         (state-b (or (get-text-property 1 'todo-state b) "")))
    (or
     (if (member state-a org-done-keywords-for-agenda) 1)
     (if (member state-b org-done-keywords-for-agenda) -1))))

(defun my-org-date-cmp (a b)
  (let* ((sched-a (or (get-text-property 1 'org-scheduled a) 0))
         (sched-b (or (get-text-property 1 'org-scheduled b) 0))
         (deadline-a (or (get-text-property 1 'org-deadline a) 0))
         (deadline-b (or (get-text-property 1 'org-deadline b) 0)))
    (or
     (my-org-compare-dates
      (my-org-min-date sched-a deadline-a)
      (my-org-min-date sched-b deadline-b)))))

(defun my-org-min-date (a b)
  "Return the smaller of A or B, except for 0."
  (funcall (if (and (> a 0) (> b 0)) 'min 'max) a b))

(defun my-org-sort-agenda-items-user-defined (a b)
  ;; compare by deadline, then scheduled date; done tasks are listed at the very bottom
  (or
   (my-org-complete-cmp a b)
   (my-org-date-cmp a b)))

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

(defun my-org-sort-agenda-items-todo (a b)
  (or
   (org-cmp-time a b)
   (my-org-complete-cmp a b)
   (my-org-context-cmp a b)
   (my-org-date-cmp a b)
   (org-cmp-todo-state a b)
   (org-cmp-priority a b)
   (org-cmp-effort a b)))
;; Sorting by date and priority:1 ends here

;; Preventing things from falling through the cracks
;; :PROPERTIES:
;; :CUSTOM_ID: preventing-things-from-falling-through-the-cracks
;; :END:
;; This helps me keep track of unscheduled tasks, because I sometimes
;; forget to assign tasks a date. I also want to keep track of stuck projects.

;; [[file:Sacha.org::#preventing-things-from-falling-through-the-cracks][Preventing things from falling through the cracks:1]]
(defun my-org-agenda-list-unscheduled (&rest ignore)
  "Create agenda view for tasks that are unscheduled and not done."
  (let* ((org-agenda-todo-ignore-with-date t)
         (org-agenda-overriding-header "List of unscheduled tasks: "))
    (org-agenda-get-todos)))
(setq org-stuck-projects
      '("+PROJECT-MAYBE-DONE"
        ("TODO")
        nil
        "\\<IGNORE\\>"))
;; Preventing things from falling through the cracks:1 ends here

;; Projects
;; :PROPERTIES:
;; :CUSTOM_ID: projects
;; :END:


;; [[file:Sacha.org::#projects][Projects:1]]
(defun my-org-show-active-projects ()
  "Show my current projects."
  (interactive)
  (org-tags-view nil "project-inactive-someday"))
;; Projects:1 ends here

;; Weekly review
;; :PROPERTIES:
;; :CUSTOM_ID: weekly-review
;; :END:

;; <<weekly-review>>

;; I regularly post [[http://sachachua.com/blog/category/weekly][weekly reviews]] to keep track of what I'm done,
;; remind me to plan for the upcoming week, and list blog posts,
;; sketches, and links. I want to try out grouping tasks by topic first,
;; then breaking it down into previous/next week.


;; [[file:Sacha.org::#weekly-review][Weekly review:1]]
(use-package quantified :ensure nil :load-path "~/sync/cloud/elisp" :unless my-phone-p)
(defvar my-weekly-review-line-regexp
  "^  \\([^:]+\\): +\\(Sched[^:]+: +\\)?TODO \\(.*?\\)\\(?:[      ]+\\(:[[:alnum:]_@#%:]+:\\)\\)?[        ]*$"
  "Regular expression matching lines to include.")
(defvar my-weekly-done-line-regexp
  "^  \\([^:]+\\): +.*?\\(?:Clocked\\|Closed\\):.*?\\(TODO\\|DONE\\) \\(.*?\\)\\(?:[       ]+\\(:[[:alnum:]_@#%:]+:\\)\\)?[        ]*$"
  "Regular expression matching lines to include as completed tasks.")

(defun my-quantified-get-hours (category time-summary)
  "Return the number of hours based on the time summary."
  (if (stringp category)
      (if (assoc category time-summary) (/ (cdr (assoc category time-summary)) 3600.0) 0)
    (apply '+ (mapcar (lambda (x) (my-quantified-get-hours x time-summary)) category))))

(defun _my-extract-tasks-from-agenda (string matchers prefix line-re)
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

(ert-deftest _my-extract-tasks-from-agenda ()
  (let (list-a list-b (line-re "\\([^:]+\\):\\( \\)\\(.*\\)"))
    (_my-extract-tasks-from-agenda
     "listA: Task 1\nother: Task 2\nlistA: Task 3"
     '(("listA" . list-a)
       ("." . list-b))
     "- [ ] "
     line-re)
    (should (equal list-a '("- [ ] Task 1" "- [ ] Task 3")))
    (should (equal list-b '("- [ ] Task 2")))))

(defun _my-get-upcoming-tasks ()
  (save-window-excursion
    (org-agenda nil "W")
    (_my-extract-tasks-from-agenda (buffer-string)
                                   '(("routines" . ignore)
                                     ("business" . business-next)
                                     ("people" . relationships-next)
                                     ("tasks" . emacs-next)
                                     ("." . life-next))
                                   "  - [ ] "
                                   my-weekly-review-line-regexp)))
(defun _my-get-previous-tasks ()
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
      (_my-extract-tasks-from-agenda string
                                     '(("routines" . ignore)
                                       ("business" . business)
                                       ("people" . relationships)
                                       ("tasks" . emacs)
                                       ("." . life))
                                     "  - [X] "
                                     my-weekly-done-line-regexp))))

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
    (_my-get-upcoming-tasks)
    (_my-get-previous-tasks)
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
;; Weekly review:1 ends here



;; I use this to put together a quick summary of how I spent my time.

;; The following code makes it easy to add a line:


;; [[file:Sacha.org::#weekly-review][Weekly review:2]]
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
;; Weekly review:2 ends here



;; Now we put it all together...


;; [[file:Sacha.org::#weekly-review][Weekly review:3]]
(defun my-org-prepare-weekly-review (&optional date skip-urls)
  "Prepare weekly review template."
  (interactive (list (org-read-date nil nil nil "Ending on Fri: " nil "-fri")))
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
     "\n\n#+begin_my_details Time\n"
     (orgtbl-to-orgtbl
      (my-quantified-compare prev start start end
                             '("A-"
                               "Business"
                               "Discretionary - Play"
                               "Unpaid work"
                               "Discretionary - Social"
                               "Discretionary - Family"
                               "Sleep"
                               "Discretionary - Productive"
                               "Personal")
                             "The other week %" "Last week %")
      nil)
     "\n#+end_my_details\n\n")))

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
;; Weekly review:3 ends here

;; Flickr extract
;; :PROPERTIES:
;; :CUSTOM_ID: flickr-extract
;; :END:


;; [[file:Sacha.org::#flickr-extract][Flickr extract:1]]
(defun _my-clean-up-flickr-list (list)
  (setq list
        (replace-regexp-in-string "\\[\"" "[" list))
  (setq list
        (replace-regexp-in-string "<a href=\"\"\\([^\"]+\\).*?>.*?</a>"
                                  "[[\\1][\\2]]" list))
  (setq list
        (replace-regexp-in-string "\"
        " "" (replace-regexp-in-string "\"\\]" "]" list))))

(defun _my-format-flickr-link-for-org (x)
  (let ((title (assoc-default "FileName" x)))
    (format
     "- %s %s"
     (org-link-make-string
      (assoc-default "URL" x)
      title)
     (if (string= (assoc-default "Description" x) "")
         ""
       (concat "- "
               (replace-regexp-in-string
                "<a href=\"\"\\(.*?\\)\"\".*?>\\(.*?\\)</a>"
                (lambda (string)
                  (org-link-make-string
                   (match-string 1 string)
                   (match-string 2 string)))
                (assoc-default "Description" x)))))))


(defun _my-parse-and-filter-flickr-csv-buffer (start end)
  (sort
   (delq nil
         (mapcar (lambda (x)
                   (if (and (string< (assoc-default "FileName" x) end)
                            (org-string<= start (assoc-default "FileName" x)))
                       x))
                 (csv-parse-buffer t)))
   (lambda (a b)
     (string< (assoc-default "FileName" a)
              (assoc-default "FileName" b)))))


(defun my-sketches-export-and-extract (start end &optional do-insert update-db filter)
  "Create a list of links to sketches."
  (interactive (list (org-read-date) (org-read-date) t current-prefix-arg (read-string "Filter: ")))
  (let ((value
         (mapconcat
          (lambda (filename)
            (let ((base (file-name-nondirectory filename)))
              (format "- %s\n"
                      (org-link-make-string
                       (replace-regexp-in-string "#" "%23"
                                                 (concat "sketch:" base))
                       base))))
          (let ((my-sketch-directories '("~/sync/sketches"))) (my-get-sketch-filenames-between-dates start end filter))
          "")))
    (if do-insert
        (insert value)
      value)))
;; Flickr extract:1 ends here

;; Link-related convenience functions
;; :PROPERTIES:
;; :CUSTOM_ID: link-related-convenience-functions
;; :END:


;; [[file:Sacha.org::#link-related-convenience-functions][Link-related convenience functions:1]]
(defun kensanata/resolve-redirect (url)
  "Resolve shortened URL by launching `curl --head' and parsing the result."
  (let* ((curl (shell-command-to-string
                (format "curl --silent --head %s" url)))
         (location (when (and (string-match "^HTTP/1\.1 301" curl)
                              (string-match "^Location: \\(.*\\)" curl))
                     (match-string 1 curl))))
    (or location url)))

(defun my-resolve-urls-in-region (beg end)
  "Expand URLs between BEG and END."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (re-search-forward org-link-bracket-re nil t)
        (replace-match (save-match-data (kensanata/resolve-redirect
                                         (match-string 1))) t t nil 1))
      (goto-char (point-min))
      (while (re-search-forward org-link-re-with-space nil t)
        (replace-match (save-match-data (kensanata/resolve-redirect
                                         (match-string 0))) t t nil)))))

(defun my-open-urls-in-region (beg end)
  "Open URLs between BEG and END.
        TODO: Get better at detecting and opening all URLs"
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (re-search-forward org-link-any-re nil t)
        (save-excursion
          (backward-char)
					(browse-url (match-string 0)))))))
;; Link-related convenience functions:1 ends here

;; Monthly reviews
;; :PROPERTIES:
;; :CUSTOM_ID: monthly-reviews
;; :END:

;; <<monthly-reviews>>

;; I want to be able to see what I worked on in a month so that I can write my [[http://sachachua.com/blog/category/monthly][monthly reviews]]. This code makes it easy to display a month's clocked tasks and time. I haven't been particularly thorough in tracking time before, but now that I have a shortcut that logs in Quantified Awesome as well as in Org, I should end up clocking more.


;; [[file:Sacha.org::#monthly-reviews][Monthly reviews:1]]
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
;; Monthly reviews:1 ends here



;; Here's a function like =my-org-prepare-weekly-review=:


;; [[file:Sacha.org::#monthly-reviews][Monthly reviews:2]]
(defun my-list-blog-posts (start-date end-date)
  (seq-filter (lambda (o)
                (and (or (null start-date) (string< start-date (plist-get o :date)))
                     (or (null end-date) (string< (plist-get o :date) end-date))))
              (let ((json-object-type 'plist))
                (json-read-file "~/proj/static-blog/_site/blog/all/index.json"))))

(defun my-org-get-last-week ()
  "Return dates for filtering last week."
  (if (string= (format-time-string "%u") "6") ;; my week starts on Saturday
      (cons (org-read-date nil nil "-1w") (org-read-date nil nil "."))
    (cons (org-read-date nil nil "-2sat") (org-read-date nil nil "-sat"))))
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
         org-date
         time)
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
    (setq time (my-quantified-compare previous-date start-date start-date end-date '("Business" "Discretionary - Play" "Unpaid work" "A-" "Discretionary - Family" "Discretionary - Social" "Sleep" "Discretionary - Productive" "Personal") "Previous month %" "This month %"))
    (goto-char (line-end-position))
    (insert
     "\n\n** " title "  :monthly:review:\n"
     (my-org-summarize-journal-csv start-date end-date "monthly-highlight" my-journal-category-map my-journal-categories) "\n\n"
     "*Blog posts*\n"
     posts "\n\n"
     "*Sketches*\n\n"
     sketches
     "*Time*\n\n"
     (orgtbl-to-orgtbl time nil))
    (my-org-11ty-prepare-subtree)))

(defun my-org-prepare-yearly-review (previous-date start-date end-date)
  (let* ((posts (mapconcat (lambda (o) (concat "- " (org-link-make-string (concat "https://sachachua.com" (plist-get o :permalink))
                                                                          (plist-get o :title))))
                           (my-list-blog-posts
                            (substring start-date 0 10)
                            (substring end-date 0 10))
                           "\n")
                )
         (sketches (my-sketches-export-and-extract (substring start-date 0 10) (substring end-date 0 10) nil t))
         (time (my-quantified-compare previous-date start-date start-date end-date '("Business" "Discretionary - Play" "Unpaid work" "A-" "Discretionary - Family" "Discretionary - Social" "Sleep" "Discretionary - Productive" "Personal") "2020-2021 %" "2021-2022 %"))
         )
    (insert
     "*Blog posts*\n\n" posts "\n\n"
     "*Sketches*\n\n" sketches
     "*Time*\n\n" (orgtbl-to-orgtbl time nil))))
;; Monthly reviews:2 ends here

;; TODO Bounce to another file                             :computer:phone:
;; :PROPERTIES:
;; :CUSTOM_ID: bounce-to-another-file
;; :END:

;; On my phone, Emacs in Termux is nice for scripting, and Orgzly is nice
;; for editing long text. Let's see if this function lets me quickly
;; bounce things around from one place to another.


;; [[file:Sacha.org::#bounce-to-another-file][Bounce to another file:1]]
(defun my-org-bounce-to-file (file)
  "Toggle subtree between its home file and another file.
Limitations: Reinserts entry at bottom of subtree, uses kill ring."
  (interactive (list (read-file-name "File: ")))
  (if (string= (buffer-file-name) (expand-file-name file))
      ;; Return it
      (let ((location (org-entry-get (point) "BOUNCE")))
        (when location
          (setq location (read location))
          (org-cut-subtree)
          (save-buffer)
          (with-current-buffer (find-file (car location))
            (save-restriction
              (widen)
              (goto-char (org-find-olp location))
              (org-end-of-subtree)
              (unless (bolp) (insert "\n"))
              (org-paste-subtree (length location) nil nil t)
              (save-buffer)))))
    (org-entry-put (point) "BOUNCE" (prin1-to-string (cons (buffer-file-name) (org-get-outline-path))))
    (org-cut-subtree)
    (save-buffer)
    (with-current-buffer (find-file file)
      (save-restriction
        (widen)
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (org-yank)
        (save-buffer)))))
;; Bounce to another file:1 ends here

;; Basic refiling configuration
;; :PROPERTIES:
;; :CUSTOM_ID: refiling
;; :END:

;; =org-refile= lets you organize notes by typing in the headline to file them under.


;; [[file:Sacha.org::#refiling][Basic refiling configuration:1]]
(setq org-reverse-note-order t) ; I want new notes prepended
(setq org-refile-use-outline-path 'title)  ; distinguish between files named the same
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)
(setq org-refile-use-cache t)
(setq org-blank-before-new-entry nil)

(setq org-refile-targets
			'((("~/sync/orgzly/organizer.org"
					"~/sync/orgzly/routines.org"
					"~/sync/orgzly/business.org"
					"~/sync/orgzly/reference.org"
					"~/sync/orgzly/garden.org"
					"~/sync/orgzly/decisions.org"
					"~/sync/emacs/Sacha.org"
					"~/sync/orgzly/posts.org"
					"~/sync/orgzly/people.org"
					"~/sync/orgzly/resources.org"
					"~/sync/orgzly/Inbox.org"
					"~/proj/emacsconf/wiki/2023/organizers-notebook/index.org")
				 . (:maxlevel . 7))
				(("~/proj/quantified/notes.org"
					"~/proj/sketches/notes.org"
					"~/sync/static-blog/notes.org"
					"~/proj/journal/notes.org"
					"~/sync/orgzly/crafts.org"
					"~/sync/orgzly/misc.org")
				 . (:maxlevel . 2))
				(("~/sync/orgzly/news.org")
				 . (:maxlevel . 1))
				))
;; Basic refiling configuration:1 ends here

;; TEACH Jump to Org location by substring
;; :PROPERTIES:
;; :Effort:   1:00
;; :QUANTIFIED: Emacs
;; :CUSTOM_ID: jump-to-org-location-by-substring
;; :END:
;; :LOGBOOK:
;; CLOCK: [2015-02-05 Thu 19:48]--[2015-02-05 Thu 20:03] =>  0:15
;; :END:


;; [[file:Sacha.org::#jump-to-org-location-by-substring][TEACH Jump to Org location by substring:1]]
;; Example: (org-refile 4 nil (my-org-refile-get-location-by-substring "Other Emacs"))
(defun my-org-refile-get-location-by-substring (regexp &optional file)
  "Return the refile location identified by REGEXP."
  (let ((org-refile-targets org-refile-targets) tbl)
    (setq org-refile-target-table (org-refile-get-targets)))
  (unless org-refile-target-table
    (user-error "No refile targets"))
  (cl-find regexp org-refile-target-table
           :test
           (lambda (a b)
             (and
              (string-match a (car b))
              (or (null file)
                  (string-match file (elt b 1)))))))
(defun my-org-refile-subtree-to (name)
  (org-refile nil nil (my-org-refile-get-location-exact name)))

(defun my-org-refile-get-location-exact (name &optional file)
  "Return the refile location identified by NAME."
  (let ((org-refile-targets org-refile-targets) tbl)
    (setq org-refile-target-table (org-refile-get-targets)))
  (unless org-refile-target-table
    (user-error "No refile targets"))
  (cl-find name org-refile-target-table
           :test (lambda (a b)
                   (and (string-equal a (car b))
                        (or (null file)
                            (string-match file (elt b 1)))))))
;; Example: (my-org-clock-in-refile "Off my computer")
(defun my-org-clock-in-refile (location &optional file)
  "Clocks into LOCATION.
        LOCATION and FILE can also be regular expressions for `my-org-refile-get-location-by-substring'."
  (interactive (list (my-org-refile-get-location)))
  (save-window-excursion
    (save-excursion
      (if (stringp location) (setq location (my-org-refile-get-location-by-substring location file)))
      (org-refile 4 nil location)
      (org-clock-in))))

(defun my-org-finish-previous-task-and-clock-in-new-one (location &optional file)
  (interactive (list (my-org-refile-get-location)))
  (save-window-excursion
    (org-clock-goto)
    (org-todo 'done))
  (my-org-clock-in-and-track-by-name location file))

(defun my-org-clock-in-and-track-by-name (location &optional file)
  (interactive (list (my-org-refile-get-location)))
  (save-window-excursion
    (save-excursion
      (if (stringp location) (setq location (my-org-refile-get-location-exact location file)))
      (org-refile 4 nil location)
      (my-org-clock-in-and-track))))
(defun my-org-off-my-computer (category)
  (interactive "MCategory: ")
  (eval-when-compile (require 'quantified nil t))
  (my-org-clock-in-refile "Off my computer")
  (quantified-track category))
;; TEACH Jump to Org location by substring:1 ends here

;; Quick way to jump
;; :PROPERTIES:
;; :CUSTOM_ID: quick-way-to-jump
;; :END:


;; [[file:Sacha.org::#quick-way-to-jump][Quick way to jump:1]]
(defun my-org-jump ()
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively 'org-refile)))
;; Quick way to jump:1 ends here

;; TODO Refile inbox entries to a smaller set of org-refile-targets :dotemacs:
;; :PROPERTIES:
;; :CUSTOM_ID: refile-inbox
;; :END:

;; When I'm filing things from my inbox, I want a faster refile that
;; considers a smaller set of entries.


;; [[file:Sacha.org::#refile-inbox][Refile inbox entries to a smaller set of org-refile-targets:1]]
(defun my-org-refile-to-subset (arg)
	"Refile to a smaller set of targets."
	(interactive "P")
	(let ((org-refile-targets '(("~/sync/orgzly/organizer.org" . (:tag . "inboxtarget"))
															("~/sync/orgzly/organizer.org" . (:maxlevel . 3))
															("~/sync/orgzly/resources.org" . (:maxlevel . 1))
															(nil . (:level . 1))
															("~/proj/stream/index.org" . (:maxlevel . 3))
															("~/sync/emacs/Inbox.org" . (:maxlevel . 1))
															("~/sync/emacs/Sacha.org" . (:maxlevel . 4))
															("~/sync/orgzly/people.org" . (:maxlevel . 2)))))
		(org-refile arg)))

(defun my-org-refile-to-target-or-subset (&optional arg)
	(interactive "P")
	(or (my-org-refile-current-entry-to-tag-target)
			(my-org-refile-to-subset arg)))

(keymap-global-set "C-c w" 'my-org-refile-to-target-or-subset)
;; Refile inbox entries to a smaller set of org-refile-targets:1 ends here

;; DONE Automatically refiling Org Mode headings based on tags  :org:emacs:
;; CLOSED: [2023-12-28 Thu 15:57]
;; :PROPERTIES:
;; :CREATED:  [2023-12-11 Mon 21:29]
;; :CUSTOM_ID: refile-tags
;; :EXPORT_DATE: 2023-12-28T15:53:13-0500
;; :EXPORT_ELEVENTY_PERMALINK: /blog/2023/12/automatically-refiling-org-mode-headings-based-on-tags/
;; :EXPORT_ELEVENTY_FILE_NAME: blog/2023/12/automatically-refiling-org-mode-headings-based-on-tags/
;; :END:
;; :LOGBOOK:
;; - State "DONE"       from "TOBLOG"     [2023-12-28 Thu 15:57]
;; - State "TODO"       from "BLOCKED"    [2023-12-25 Mon 21:18]
;; :END:

;; I have lots of different things in my Org Mode inbox. Following the [[https://fortelabs.com/blog/para/][PARA]] method, I want to file them under projects, areas, resources, or archive so that I can find related things later. Actually, no, I don't /want/ to refile them. I do want to be able to:

;; - find all the pieces related to something when I'm ready to start working on a task
;; - find useful links again, especially if I can use my own words

;; Refiling is annoying on my phone, so I tend to wait until I'm back at
;; my computer. But even with ~org-refile-use-outline-path~ set to ~file~
;; and the ability to specify substrings, there's still a bit of
;; friction.

;; Tagging is a little easier to do on my phone. I can add a few tags
;; when I share a webpage or create a task.

;; I thought it would be nice to have something that automatically
;; refiles my inbox headings tagged with various tags to other subtrees
;; where I've set a ~:TAG_TARGET:~ property or something like that. For
;; example, I can set the ~TAG_TARGET~ property to ~emacsconf~ to mean
;; that anything tagged with ~:emacsconf:~ should get filed under there.

;; https://emacs.stackexchange.com/questions/36360/recursively-refiling-all-subtrees-with-tag-to-a-destination-org-mode ...


;; [[file:Sacha.org::#refile-tags][Automatically refiling Org Mode headings based on tags:1]]
(defcustom my-org-refile-tag-targets nil
	"Searches and IDs."
	:group 'sacha
	:type '(repeat (cons string string string)))

(with-eval-after-load 'org
	(defvar my-org-tag-target-files
		(append '("~/sync/orgzly/news.org"
							"~/sync/orgzly/resources.org"
							"~/proj/stream/index.org")
						org-agenda-files)
		"Files to check for tag targets."))

(defun my-org-update-tag-targets ()
	(interactive)
	(let ((org-agenda-files my-org-tag-target-files))
		(setq my-org-refile-tag-targets
					(let (list)
						(org-map-entries
						 (lambda ()
							 (list (concat "+" (org-entry-get (point) "TAG_TARGET"))
										 (org-id-get-create)
										 (org-entry-get (point) "ITEM")))
						 "TAG_TARGET={.}" 'agenda))))
	(customize-save-variable 'my-org-refile-tag-targets my-org-refile-tag-targets))

(defun my-org-add-tag-target (tag)
	(interactive "MTag: ")
	(org-entry-put (point) "TAG_TARGET" tag)
	(push (list (concat "+" tag)
							(org-id-get-create)
							(org-entry-get (point) "ITEM"))
				my-org-refile-tag-targets)
	(customize-save-variable 'my-org-refile-tag-targets my-org-refile-tag-targets))

(defun my-org-refile-current-entry-to-tag-target (&optional arg target-marker)
	(interactive (list current-prefix-arg (cadr (my-org-tag-target-for-entry-at-point))))
	(unless target-marker
		(setq target-marker (cadr (my-org-tag-target-for-entry-at-point))))
	(when (stringp target-marker)
		(setq target-marker (org-id-find target-marker t)))
	(when target-marker
		(org-refile
		 arg nil
		 (with-current-buffer (marker-buffer target-marker)
			 (goto-char target-marker)
			 (list (org-get-heading)
						 (buffer-file-name (marker-buffer target-marker))
						 nil
						 target-marker)))))

;; Based on https://emacs.stackexchange.com/questions/36360/recursively-refiling-all-subtrees-with-tag-to-a-destination-org-mode
(defun my-org-refile-matches-to-heading (match target-heading-id &optional scope copy)
  "Refile all headings within SCOPE (per `org-map-entries') to TARGET-HEADING-ID."
  (if-let (target-marker (org-id-find target-heading-id t))
      (let* ((target-rfloc (with-current-buffer (marker-buffer target-marker)
                             (goto-char target-marker)
                             (list (org-get-heading)
                                   (buffer-file-name (marker-buffer target-marker))
                                   nil
                                   target-marker)))
             (headings-to-copy (org-map-entries (lambda () (point-marker)) match scope)))
        (mapc
         (lambda (heading-marker)
           (with-current-buffer (marker-buffer heading-marker)
             (goto-char heading-marker)
             (org-refile nil nil target-rfloc (when copy "Copy"))))
         (nreverse headings-to-copy))
        (message "%s %d headings!"
                 (if copy "Copied" "Refiled")
                 (length headings-to-copy)))
    (warn "Could not find target heading %S" target-heading-id)))

(defun my-org-tag-target-for-entry-at-point ()
	"Return the `my-org-refile-tag-targets' entry that matches point."
	(let ((tags	(org-get-tags (point)))
				(level (org-current-level))
				(todo (org-get-todo-state))
				matcher)
		(catch 'found
			(dolist (target my-org-refile-tag-targets)
				(setq matcher (cdr (org-make-tags-matcher (car target))))
				(when (funcall matcher todo tags level)
					(throw 'found target))))))

(defun my-org-refile-to-tag-targets ()
	(interactive)
	(dolist (rule my-org-refile-tag-targets)
		(my-org-refile-matches-to-heading (car rule) (cadr rule))))

(defun my-org-refile-inbox-to-tag-targets ()
	(interactive)
	(with-current-buffer (find-file-noselect my-org-inbox-file)
		(dolist (rule my-org-refile-tag-targets)
			(my-org-refile-matches-to-heading (car rule) (cadr rule) 'file))))
;; Automatically refiling Org Mode headings based on tags:1 ends here

;; Moving lines around
;; :PROPERTIES:
;; :CUSTOM_ID: destination
;; :END:

;; This makes it easier to reorganize lines in my weekly review.


;; [[file:Sacha.org::#destination][Moving lines around:1]]
(defun my-org-move-line-to-destination ()
  "Moves the current list item to DESTINATION in the current buffer.
If no DESTINATION is found, move it to the end of the list
and indent it one level."
  (interactive)
  (save-window-excursion
    (save-excursion
      (let ((string
             (buffer-substring-no-properties
              (line-beginning-position) (line-end-position)))
            (case-fold-search nil)
            found)
        (delete-region (line-beginning-position) (1+ (line-end-position)))
        (save-excursion
          (goto-char (point-min))
          (when (re-search-forward "DESTINATION" nil t)
            (insert "\n" (make-string (- (match-beginning 0) (line-beginning-position)) ?\ ) (s-trim string))
            (setq found t)))
        (unless found
          (org-end-of-item-list)
          (insert string "\n"))))))
;; Moving lines around:1 ends here

;; [[file:Sacha.org::#destination][Moving lines around:2]]
(defun my-org-move-line-to-end-of-list ()
  "Move the current list item to the end of the list."
  (interactive)
  (save-excursion
    (let ((string (buffer-substring-no-properties (line-beginning-position)
                                                  (line-end-position))))
      (delete-region (line-beginning-position) (1+ (line-end-position)))
      (org-end-of-item-list)
      (insert string))))
;; Moving lines around:2 ends here

;; Organizing my blog index
;; :PROPERTIES:
;; :CUSTOM_ID: organizing-my-blog-index
;; :END:


;; [[file:Sacha.org::#organizing-my-blog-index][Organizing my blog index:1]]
(defun my-org-file-blog-index-entries ()
  "Keep filing until I press `C-g'."
  (interactive)
  (while t
    (my-org-file-blog-index-entry
     (line-beginning-position) (1+ (line-end-position))
     (let ((org-refile-targets
            '(("~/proj/sharing/blog.org" . (:maxlevel . 3)))))
       (save-excursion (org-refile-get-location "Location"))))))

(defun my-org-file-blog-index-entry (beg end location)
  "Copy entries into blog.org."
  (interactive
   (list
    (if (region-active-p) (point) (line-beginning-position))
    (if (region-active-p) (mark) (1+ (line-end-position)))
    (let ((org-refile-targets
           '(("~/proj/sharing/blog.org" . (:maxlevel . 3)))))
      (save-excursion (org-refile-get-location "Location")))))
  (let ((s
         (replace-regexp-in-string
          "^[ \t]*- \\(\\[X\\] \\)?"
          "- [X] "
          (buffer-substring-no-properties beg end))))
    ;; if we're already in blog.org, delete the previous entry
    (if (string= buffer-file-name (expand-file-name "~/proj/sharing/blog.org"))
        (delete-region beg end))
    (save-window-excursion
      (save-excursion
        (find-file (nth 1 location))
        (save-excursion
          (save-restriction
            (widen)
            (goto-char (nth 3 location))
            (re-search-forward org-list-full-item-re nil t)
            (goto-char (line-beginning-position))
            (insert s)
            (org-update-statistics-cookies nil)))))))
;; Organizing my blog index:1 ends here

;; Refiling Org Mode notes to headings in the same file  :org:emacs:
;; :PROPERTIES:
;; :CUSTOM_ID: quickly-refiling-org-mode-notes-to-headings-in-the-same-file
;; :END:

;; I spent some time tidying up my [[https://sachachua.com/dotemacs][Emacs
;; configuration]] . I used ~org-babel-demarcate-block~
;; to split up some long ~#+begin_src...#+end_src~
;; blocks and refiled sections to group them
;; together. I also promoted more sections to
;; top-level headings in order to make the most of
;; the side navigation provided by the [[https://github.com/fniessen/org-html-themes][Read the Org
;; setup file]] based on [[https://docs.readthedocs.io/en/latest/][Read the Docs]]. These functions
;; were helpful:

;; #+NAME: my-org-refile-in-file

;; [[file:Sacha.org::my-org-refile-in-file][my-org-refile-in-file]]
(defun my-org-refile-in-file (&optional prefix)
  "Refile to a target within the current file."
  (interactive)
	(let ((org-refile-targets (list (cons nil '(:maxlevel . 5)))))
		(call-interactively 'org-refile)))

(defun my-org-refile-to-previous ()
  "Refile subtree to last position from `my-org-refile-in-file'."
  (interactive)
  (save-selected-window
    (when (eq major-mode 'org-agenda-mode)
      (org-agenda-switch-to))
    (org-cut-subtree)
		(save-window-excursion
			(save-excursion
				(bookmark-jump (plist-get org-bookmark-names-plist :last-refile))
				(let ((level (org-current-level)))
					(org-end-of-subtree t t)
					(org-paste-subtree))))))

(with-eval-after-load 'org
  (push '("w" call-interactively 'org-refile) org-speed-commands)
  (push '("W" call-interactively 'my-org-refile-in-file) org-speed-commands)
  (push '("." call-interactively 'my-org-refile-to-previous) org-speed-commands))
;; my-org-refile-in-file ends here

;; Contacts
;; :PROPERTIES:
;; :CUSTOM_ID: org-contacts
;; :END:

;; [[file:Sacha.org::#org-contacts][Contacts:1]]
(use-package org-contacts
	:config
	(setq org-contacts-files '("~/sync/orgzly/people.org")))
;; Contacts:1 ends here

;; Inserting code
;; :PROPERTIES:
;; :CUSTOM_ID: inserting-code
;; :END:


;; [[file:Sacha.org::#inserting-code][Inserting code:1]]
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
(defun my-org-insert-function-and-key (keys)
  (interactive (caar (help--read-key-sequence)))
  (insert (format "=%s= (=%s=)" (symbol-name (key-binding keys t))
                  (key-description keys))))
;; Inserting code:1 ends here

;; [[file:Sacha.org::#inserting-code][Inserting code:2]]
(use-package org
  :hook (org-mode . org-indent-mode)
  :config
  (setq org-indent-indentation-per-level 2)
  (setq org-edit-src-content-indentation 0)
  (setq org-src-preserve-indentation t))
;; Inserting code:2 ends here

;; Org Babel
;; :PROPERTIES:
;; :CUSTOM_ID: org-babel
;; :END:

;; #+NAME: org-babel-default-header-args

;; [[file:Sacha.org::org-babel-default-header-args][org-babel-default-header-args]]
(setq org-babel-default-header-args
      '((:session . "none")
        (:results . "drawer replace")
				(:comments . "both")
        (:exports . "code")
        (:cache . "no")
        (:eval . "never-export")
        (:hlines . "no")
        (:tangle . "no")))
(setq org-edit-src-auto-save-idle-delay 5)
;; org-babel-default-header-args ends here

;; Format source
;; :PROPERTIES:
;; :CUSTOM_ID: format-source
;; :END:

;; [[https://apps.bram85.nl/git/bram/gists/src/commit/118c5a579a231862f4d1a548afe071e450af4e03/gists/format-org-mode-source-blocks.el][gists/format-org-mode-source-blocks.el at 118c5a579a231862f4d1a548afe071e450af4e03 - gists - Forgejo]]


;; [[file:Sacha.org::#format-source][Format source:1]]
(use-package format-all :if my-laptop-p)

(use-package org
  :config
  (defun my/format-all-advice ()
    (ignore-errors               ; in case there's no language support
      (format-all-buffer)))
  (advice-add #'org-edit-src-exit :before #'my/format-all-advice))
;; Format source:1 ends here

;; Execute named babel block
;; :PROPERTIES:
;; :CUSTOM_ID: execute-named-babel-block
;; :END:

;;   #+NAME: test

;; [[file:Sacha.org::test][test]]
(defun my-org-execute-src-block-by-name (name)
  (interactive (list (completing-read "Block: "(org-babel-src-block-names))))
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (format "^#\\+NAME:[ \t]+%s[ \t]*$" (regexp-quote name)) nil t)
      (org-babel-execute-src-block))))
;; test ends here

;; JSON
;; :PROPERTIES:
;; :CUSTOM_ID: json
;; :END:

;; From https://isamert.net/2022/01/04/dealing-with-apis-jsons-and-databases-in-org-mode.html


;; [[file:Sacha.org::#json][JSON:1]]
(defun org-babel-execute:json (body params)
  (let ((jq (cdr (assoc :jq params)))
        (node (cdr (assoc :node params))))
    (cond
     (jq
      (with-temp-buffer
        ;; Insert the JSON into the temp buffer
        (insert body)
        ;; Run jq command on the whole buffer, and replace the buffer
        ;; contents with the result returned from jq
        (shell-command-on-region (point-min) (point-max) (format "jq -r \"%s\"" jq) nil 't)
        ;; Return the contents of the temp buffer as the result
        (buffer-string)))
     (node
      (with-temp-buffer
        (insert (format "const it = %s;" body))
        (insert node)
        (shell-command-on-region (point-min) (point-max) "node -p" nil 't)
        (buffer-string))))))
;; JSON:1 ends here

;; JQ
;; :PROPERTIES:
;; :CUSTOM_ID: jq
;; :END:


;; [[file:Sacha.org::#jq][JQ:1]]
(use-package jq-mode
	:vc (:url "https://github.com/ljos/jq-mode")
	:config
	(org-babel-do-load-languages 'org-babel-load-languages
															 '((jq . t))))
;; JQ:1 ends here

;; Fix block indentation
;; :PROPERTIES:
;; :CUSTOM_ID: org-block-indentation
;; :END:

;; [[file:Sacha.org::#org-block-indentation][Fix block indentation:1]]
(defun my-org-fix-block-indentation ()
	"Fix the indentation of the current src block."
	(interactive)
	(org-edit-special)
	(indent-region (point-min) (point-max))
	(org-edit-src-exit))
;; Fix block indentation:1 ends here

;; Let's try literate-elisp
;; :PROPERTIES:
;; :CUSTOM_ID: let-s-try-literate-elisp
;; :END:


;; [[file:Sacha.org::#let-s-try-literate-elisp][Let's try literate-elisp:1]]
(use-package literate-elisp :if my-laptop-p)
;; Let's try literate-elisp:1 ends here



;; #+RESULTS:
;; :results:
;; [[file:inline.svg]]
;; :end:

;; The following code overrides HTML and Markdown exports
;; to include SVGs.


;; [[file:Sacha.org::#org-inline-svg][Include inline SVGs in Org Mode HTML and Markdown exports:2]]
(defun my-ox-link-path (link _ info)
	(let* ((raw-path (org-element-property :path link)))
		(setq raw-path
					(org-export-file-uri
					 (org-publish-file-relative-name raw-path info)))
		;; Possibly append `:html-link-home' to relative file
		;; name.
		(let ((home (and (plist-get info :html-link-home)
										 (org-trim (plist-get info :html-link-home)))))
			(when (and home
								 (plist-get info :html-link-use-abs-url)
								 (not (file-name-absolute-p raw-path)))
				(setq raw-path (concat (file-name-as-directory home) raw-path))))
		raw-path))

(defun my-org-html-link (link desc info)
	(if (and
			 (string= (org-element-property :type link) "file")
			 (not (plist-get (org-export-read-attribute :attr_html (org-element-parent-element link))
											 :data-link))
			 (org-export-inline-image-p link (plist-get info :html-inline-image-rules)))
			(let ((path (org-element-property :path link)))
				(if (string= (file-name-extension path) "svg")
						(with-temp-buffer
							(insert-file-contents-literally path)
							(buffer-string))
					(org-html-link link desc info)))
		(org-html-link link desc info)))

(defun my-org-md-link (link desc info)
	(if (and (string= (org-element-property :type link) "file")
					 (not (plist-get (org-export-read-attribute :attr_html (org-element-parent-element link))
											 :data-link)))
			(let ((path (org-element-property :path link)))
				(if (string= (file-name-extension path) "svg")
						(with-temp-buffer
							(insert-file-contents-literally path)
							(buffer-string))
					(org-md-link link desc info)))
		(org-md-link link desc info)))

(defun my-org-11ty-link (link desc info)
	(if (and (string= (org-element-property :type link) "file")
					 (not (plist-get (org-export-read-attribute :attr_html (org-element-parent-element link))
											 :data-link)))
			(let ((path (org-element-property :path link)))
				(if (string= (file-name-extension path) "svg")
						(with-temp-buffer
							(insert-file-contents-literally path)
							(buffer-string))
					(org-11ty-link link desc info)))
		(org-11ty-link link desc info)))

(with-eval-after-load 'ox-html
	(setf
	 (alist-get 'link (org-export-backend-transcoders (org-export-get-backend 'html)))
	 'my-org-html-link))
(with-eval-after-load 'ox-md
	(setf
	 (alist-get 'link (org-export-backend-transcoders (org-export-get-backend 'md)))
	 'my-org-md-link))
(with-eval-after-load 'ox-11ty
	(setf
	 (alist-get 'link (org-export-backend-transcoders (org-export-get-backend '11ty)))
	 'my-org-11ty-link))
;; Include inline SVGs in Org Mode HTML and Markdown exports:2 ends here

;; Counting words without blocks


;; [[file:Sacha.org::*Counting words without blocks][Counting words without blocks:1]]
(defun my-org-subtree-text-without-blocks ()
	"Don't include source blocks or links."
	(let ((text ""))
		(save-excursion
			(save-restriction
				(org-back-to-heading)
				(org-narrow-to-subtree)
				(org-end-of-meta-data)
				(setq text (buffer-substring (point) (point-max)))))
		(with-temp-buffer
			(insert text)
			(org-mode)
			(goto-char (point-min))
			(while (re-search-forward org-link-any-re nil t)
				(replace-match (or (match-string 3) "(link)")))
			(goto-char (point-min))
			(while (re-search-forward "^ *#\\+begin" nil t)
			 (let ((block (org-element-context)))
				 (unless (eq (org-element-type block) 'quote-block)
					 (delete-region (org-element-begin block)
													(org-element-end block)))))
			(while (re-search-forward "\n\n+" nil t)
				(replace-match "\n"))
			(string-trim
			 (buffer-string)))))

(defun my-org-subtree-count-words-without-blocks ()
	(interactive)
	(let ((text (my-org-subtree-text-without-blocks)))
		(with-temp-buffer
			(insert text)
			(message "%s" (count-words--buffer-format)))))

(defun my-org-subtree-copy-words-without-blocks ()
	(interactive)
	(kill-new (my-org-subtree-text-without-blocks)))
;; Counting words without blocks:1 ends here

;; Org Mode: Including portions of files between two regular expressions :org:emacs:
;; :PROPERTIES:
;; :EXPORT_DATE: 2023-01-08T08:29:01-0500
;; :EXPORT_ELEVENTY_PERMALINK: /blog/2023/01/org-mode-including-portions-of-files-between-two-regular-expressions/
;; :EXPORT_ELEVENTY_FILE_NAME: blog/2023/01/org-mode-including-portions-of-files-between-two-regular-expressions/
;; :CUSTOM_ID: org-mode-including-portions-of-files-between-two-regular-expressions
;; :END:

;; #+begin_update
;; - 2023-10-11 Wed: Include images inline.
;; - 2023-09-10: Use =consult-line= instead of =consult--line=.
;; #+end_update

;; I'd like to refer to snippets of code, but lines are too fragile to
;; use as references for code and posts that I want to easily update. I'd
;; like to specify a ~from-regexp~ and a ~to-regexp~ instead in order to
;; collect the lines between those regexps (including the ones with the
;; regexps themselves). ~org-export-expand-include-keyword~ looked a bit
;; hairy to extend since it uses regular expressions to match parameter
;; values. For this quick experiment, I decided to make a custom link
;; type instead. This allows me to refer to parts of code with a link like this:

;; ~[[my-include:~/proj/static-blog/assets/css/style.css::from-regexp=Start of copy code&to-regexp=End of copy code&wrap=src js]]~

;; which will turn into this snippet from my stylesheet:
;; [[my-include:~/proj/static-blog/assets/css/style.css::from-regexp=Start of copy code&to-regexp=End of copy code&wrap=src js]]

;; Here's the Emacs Lisp code to do that. ~my-include-complete~ function
;; reuses ~my-include-open~ to narrow to the file, and
;; ~my-include-complete~ uses ~consult-line~ so that we can specify the
;; prompt.

;; #+NAME: org-my-include-link

;; [[file:Sacha.org::org-my-include-link][org-my-include-link]]
(org-link-set-parameters
 "my-include"
 :follow #'my-include-open
 :export #'my-include-export
 :complete #'my-include-complete)

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
			(org-export-as format nil nil t))))

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
;; org-my-include-link ends here



;; #+RESULTS:
;; :results:
;; my-include-complete
;; :end:

;; This code displays the images inline.


;; [[file:Sacha.org::#org-mode-including-portions-of-files-between-two-regular-expressions][Org Mode: Including portions of files between two regular expressions:2]]
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
													(overlay-put ov 'display image)
													(overlay-put ov 'face 'default)
													(overlay-put ov 'org-image-overlay t)
													(overlay-put
													 ov 'modification-hooks
													 (list 'org-display-inline-remove-overlay))
													(when (boundp 'image-map)
														(overlay-put ov 'keymap image-map))
													(push ov org-inline-image-overlays))))))))))))))
;; Org Mode: Including portions of files between two regular expressions:2 ends here

;; ox-epub
;; :PROPERTIES:
;; :CUSTOM_ID: ox-epub
;; :END:


;; [[file:Sacha.org::#ox-epub][ox-epub:1]]
(use-package ox-epub
  :if my-laptop-p
  :config
	(setq org-epub-style-default (concat org-epub-style-default "\n  p.my-verse { white-space: pre }\n")))
;; ox-epub:1 ends here

;; DONE Add a note to the bottom of blog posts exported from my config file
;;      CLOSED: [2021-03-25 Thu 23:43]
;;      :PROPERTIES:
;;      :ID:       o2b:a2b0a30e-aece-45fd-a42d-44f9afd397c2
;;      :POST_DATE: [2021-03-25 Thu 23:37]
;;      :BLOG:     sacha
;;      :POSTID:   29700
;;      :EXPORT_MODIFIED: 2021-04-18
;;      :EXPORT_DATE: 2021-03-25
;;      :EXPORT_ELEVENTY_PERMALINK: /blog/2021/03/org2blog-add-a-note-to-the-bottom-of-blog-posts-exported-from-my-config-file/
;;      :EXPORT_ELEVENTY_FILE_NAME: downloaded/2021/03/org2blog-add-a-note-to-the-bottom-of-blog-posts-exported-from-my-config-file
;;      :EXPORT_ELEVENTY_CATEGORIES: emacs org
;;      :CUSTOM_ID: config-footer
;;      :END:
;;      :LOGBOOK:
;;      - State "DONE"       from "TODO"       [2021-03-25 Thu 23:43]
;;      :END:

;;      Update: 2021-04-18: Tweaked the code so that I could add it to
;;      the main =org-export-filter-body-functions= list now that I'm
;;      using Eleventy and ox-11ty.el instead of Wordpress and org2blog.

;;      I occasionally post snippets from my Emacs configuration file,
;;      drafting the notes directly in my literate config. I figured it
;;      might be a good idea to include a link to my config at the end of
;;      the posts, but I didn't want to scatter redundant links in my
;;      config file itself. Wouldn't it be cool if the link could be
;;      automatically added whenever I post a subtree from my config
;;      file? I think the code below accomplishes that.


;; [[file:Sacha.org::#config-footer][Add a note to the bottom of blog posts exported from my config file:1]]
(defun my-org-export-filter-body-add-emacs-configuration-link (string backend info)
  (when (and (plist-get info :input-file) (string-match "\\.emacs\\.d/Sacha\\.org\\|sync/emacs/Sacha\\.org" (plist-get info :input-file)))
    (concat string
            (let ((id (org-entry-get-with-inheritance "CUSTOM_ID")))
              (format
							 (if (eq backend 'md)
									 "\nThis is part of my [Emacs configuration](https://sachachua.com/dotemacs%s)\n"
								 "\n<div class=\"note\">This is part of my <a href=\"https://sachachua.com/dotemacs%s\">Emacs configuration.</a></div>")
               (if id (concat "#" id) ""))))))

(use-package org
  :config
  (with-eval-after-load 'ox
    (add-to-list 'org-export-filter-body-functions #'my-org-export-filter-body-add-emacs-configuration-link)))
;; Add a note to the bottom of blog posts exported from my config file:1 ends here

;; Copy linked file and change link
;; :PROPERTIES:
;; :CUSTOM_ID: copy-linked-file-and-change-link
;; :END:


;; [[file:Sacha.org::#copy-linked-file-and-change-link][Copy linked file and change link:1]]
(defun my-org-copy-linked-file-and-change-link (destination)
	(interactive (list (read-file-name (format "Copy %s to: "
																						 (file-name-nondirectory (org-element-property :path (org-element-context)))))))
	(let* ((elem (org-element-context))
				 (path (org-element-property :path elem))
				 (description (org-element-property :description elem)))
		(copy-file path destination t)
		(delete-region (org-element-begin elem) (org-element-end elem))
		(insert (org-link-make-string (concat "file:" (file-relative-name (if (file-directory-p destination)
																																					(expand-file-name (file-name-nondirectory path)
																																														destination)
																																				destination)))
																	description))))
(defun my-org-copy-linked-files (destination)
	(interactive (list (read-file-name (format "Copy %s to: "
																						 (file-name-nondirectory (org-element-property :path (org-element-context)))))))
	(while (re-search-forward "file:" nil t)
		(let* ((elem (org-element-context))
					 (path (org-element-property :path elem))
					 (description (org-element-property :description elem)))
			(unless (string-match (concat "^" (regexp-quote (expand-file-name destination))) (expand-file-name path))
				(my-org-copy-linked-file-and-change-link destination)))))
;; Copy linked file and change link:1 ends here

;; Org Mode: Create a quick timestamped note and capture a screenshot :emacs:org:
;; :PROPERTIES:
;; :ID:       o2b:95dacc89-9c51-4d02-a7c9-4e28bf7f961b
;; :POST_DATE: [2020-12-12 Sat 23:58]
;; :BLOG:     sacha
;; :POSTID:   29649
;; :CUSTOM_ID: org-mode-create-a-quick-timestamped-note-and-capture-a-screenshot
;; :END:

;; I wanted to be able to quickly create timestamped notes and possibly
;; capture a screenshot. Prompting for a value inside an
;; =org-capture-template= disrupts my screen a little, so maybe this will
;; make it as easy as possible. I could probably do this without going
;; through org-capture-templates, but I wanted to take advantage of the
;; fact that Org Mode will deal with the date tree and finding the right
;; position itself.


;; [[file:Sacha.org::#org-mode-create-a-quick-timestamped-note-and-capture-a-screenshot][Org Mode: Create a quick timestamped note and capture a screenshot:1]]
(defvar my-screenshot-directory "~/recordings")
(defun my-org-insert-screenshot (file &optional note)
  (interactive (list
                (if current-prefix-arg
                    (expand-file-name
                     (consult--read
                      (reverse (directory-files my-screenshot-directory nil "\\.png$"))
                      :sort nil
                      :require-match t
                      :category 'file
                      :state (lambda (candidate state)
                               (when candidate
                                 (with-current-buffer (find-file-noselect (expand-file-name candidate my-screenshot-directory))
                                   (display-buffer (current-buffer))))))
                     my-screenshot-directory)
                  (my-latest-file my-screenshot-directory))))
	(if (derived-mode-p 'mastodon-toot-mode)
			(mastodon-toot--attach-media file (or note (read-string "Caption: ")))
		(save-window-excursion
			(if (string-match "webm" file)
					(progn
						(mpv-play file)
						(insert (org-link-make-string (concat "video:" file "?caption=" (or note (read-string "Caption: ")))) "\n"))
				(with-current-buffer (find-file-noselect file) (display-buffer (current-buffer)))
				(insert "#+CAPTION: " (or note (read-string "Caption: ")) "\n"
								(org-link-make-string (concat "file:" file)))))))

(defun my-copy-last-screenshot-to-file (new-filename)
  (interactive (list (read-file-name (format "Copy %s to: " (file-name-nondirectory (my-latest-file my-screenshot-directory))))))
  (copy-file (my-latest-file my-screenshot-directory) new-filename))

(defun my-copy-last-screenshot-and-insert-into-org (new-filename caption)
  (interactive (list (read-file-name (format "Copy %s to: " (file-name-nondirectory (my-latest-file my-screenshot-directory))))
                     (read-string "Caption: ")))
  (copy-file (my-latest-file my-screenshot-directory) new-filename t)
  (insert "#+CAPTION: " caption "\n"
          (org-link-make-string (concat "file:" (file-relative-name new-filename))) "\n"))
;; Org Mode: Create a quick timestamped note and capture a screenshot:1 ends here

;; [[file:Sacha.org::#org-mode-create-a-quick-timestamped-note-and-capture-a-screenshot][Org Mode: Create a quick timestamped note and capture a screenshot:2]]
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

(defun my-capture-timestamped-note-with-screenshot (time note)
  "Include a link to the latest screenshot."
  (interactive (list (current-time) (read-string "Note: ")))
  (kill-new (my-latest-file my-screenshot-directory))
  (my-capture-timestamped-note time note))
;; Org Mode: Create a quick timestamped note and capture a screenshot:2 ends here

;; 11ty static site generation
;; :PROPERTIES:
;; :CUSTOM_ID: 11ty
;; :END:


;; [[file:Sacha.org::#11ty][11ty static site generation:1]]
(use-package ox-11ty
  :if my-laptop-p
  :load-path "~/proj/ox-11ty"
	:config
	(advice-add 'org-11ty--front-matter :filter-return #'my-org-11ty-rewrite-tags))

(defvar my-org-11ty-serve-process nil)

(defun my-org-11ty-rewrite-tags (info)
	"Turn OneWordTags into one-word-tags."
	(require 's)
	(dolist (field '(:categories :tags))
		(when (plist-get info field)
			(plist-put info field
								 (mapcar (lambda (s)
													 (if (string-match "^_" s)
															 s
														 (s-dashed-words s)))
												 (plist-get info field)))))
	info)

(defun my-org-11ty-unpublish-current-post ()
	(interactive)
	(when (org-entry-get (point) "EXPORT_ELEVENTY_FILE_NAME")
		(delete-directory (expand-file-name (org-entry-get (point) "EXPORT_ELEVENTY_FILE_NAME")
																				my-11ty-base-dir)
											t)
		(delete-directory (expand-file-name (org-entry-get (point) "EXPORT_ELEVENTY_FILE_NAME")
																				(expand-file-name "_site" my-11ty-base-dir))
											t)))
(defalias 'my-org-11ty-delete-current-post #'my-org-11ty-unpublish-current-post)

(defun my-org-11ty-copy-permalink ()
	(interactive)
	(kill-new (concat "https://sachachua.com" (org-entry-get (point) "EXPORT_ELEVENTY_PERMALINK"))))

(defun my-org-11ty-browse-local ()
	(interactive)
	(unless (seq-find (lambda (o) (string-match "--serve" (assoc-default 'args (cdr o) nil "")))
										(proced-process-attributes))
		(let ((default-directory "~/proj/static-blog"))
			(setq my-org-11ty-serve-process (start-process "serve" nil "make" "serve"))))
	(browse-url "http://localhost:8080/blog"))

(defun my-org-11ty-serve-stop ()
	(interactive)
	(if (process-live-p my-org-11ty-serve-process)
			(stop-process my-org-11ty-serve-process)
		(when-let ((proc (seq-find (lambda (o) (string-match "--serve" (assoc-default 'args (cdr o) nil "")))
															 (proced-process-attributes))))
			(call-process "kill" nil nil nil (number-to-string) (car proc)))))


(defun my-org-11ty-prepare-subtree ()
  (interactive)
  (unless (or (org-entry-get (point) "EXPORT_DATE")
              (org-entry-get-with-inheritance "DATE"))
    (org-entry-put (point) "EXPORT_DATE" (format-time-string "%Y-%m-%dT%T%z")))
  (let ((path (concat "blog/" (format-time-string "%Y/%m/")
                      (my-make-slug (org-get-heading t t t t))
                      "/")))
    (unless (org-entry-get (point) "EXPORT_ELEVENTY_PERMALINK")
      (org-entry-put (point) "EXPORT_ELEVENTY_PERMALINK" (concat "/" path)))
    (unless (org-entry-get (point) "EXPORT_ELEVENTY_FILE_NAME")
      (org-entry-put (point) "EXPORT_ELEVENTY_FILE_NAME" path))))

(defun my-org-11ty-rename-subtree ()
	(interactive)
	(let ((new-path (concat "blog/" (format-time-string "%Y/%m/")
													(my-make-slug (org-get-heading t t t t))
													"/")))
		(when (not (string= new-path (org-entry-get (point) "EXPORT_ELEVENTY_FILE_NAME")))
			(when
					(file-exists-p (expand-file-name
													(org-entry-get (point) "EXPORT_ELEVENTY_FILE_NAME")
													my-11ty-base-dir))
				(rename-file (expand-file-name
											(org-entry-get (point) "EXPORT_ELEVENTY_FILE_NAME")
											my-11ty-base-dir)
										 (expand-file-name
											new-path
											my-11ty-base-dir)))
			(org-entry-put (point) "EXPORT_ELEVENTY_PERMALINK" (concat "/" path))
			(org-entry-put (point) "EXPORT_ELEVENTY_FILE_NAME" path))))
(defun my-11ty-convert-to-njk ()
  (interactive)
  (let* ((filename (buffer-file-name))
         (old-buffer (current-buffer))
         (new-name (concat (file-name-base filename) ".njk")))
    (save-buffer)
    (rename-file filename new-name)
    (find-file new-name)
    (kill-buffer old-buffer)))

(defun my-11ty-browse-page ()
  (interactive)
  (if (org-entry-get-with-inheritance "EXPORT_ELEVENTY_PERMALINK")
      (browse-url (concat "http://localhost:8080" (org-entry-get-with-inheritance "EXPORT_ELEVENTY_PERMALINK")))
    (let* ((json-object-type 'plist)
           (data (json-read-file (concat (file-name-base (buffer-file-name)) ".11tydata.json"))))
      (browse-url (concat "http://localhost:8080" (plist-get data :permalink))) )))

(defun my-org-11ty-pathname ()
	(if (derived-mode-p 'org-mode)
			(file-name-directory (org-entry-get-with-inheritance "EXPORT_ELEVENTY_FILE_NAME"))
		(let ((url (thing-at-point 'url)))
			(when url
				(url-file-directory (url-filename (url-generic-parse-url url)))))))

(defun my-org-11ty-find-post (url)
	(interactive (list (my-org-11ty-pathname)))
	;; check in posts.org
	(find-file "~/sync/orgzly/posts.org")
	(let ((pos (org-find-property "EXPORT_ELEVENTY_PERMALINK" url)))
		(when pos (goto-char pos))))

(defun my-org-11ty-find-file (file)
  (interactive
	 (list
		(completing-read
		 (if (my-org-11ty-pathname)
				 (format "Post (%s): " (concat "/" (my-org-11ty-pathname)))
			 "Post: ")
		 (mapcar (lambda (o) (replace-regexp-in-string "^~/proj/static-blog\\|index.html$" "" o))
						 (directory-files-recursively "~/proj/static-blog/blog" "index\\.html" nil))
		 nil nil nil nil (concat "/" (my-org-11ty-pathname)))))
  (find-file
	 (expand-file-name
		"index.html"
		(expand-file-name
		 (concat "." file)
		 "~/proj/static-blog"))))

(defun my-org-11ty-post-to-mastodon (&optional post-automatically)
  (interactive (list current-prefix-arg))
  (let ((message (concat (org-entry-get (point) "ITEM") " https://sachachua.com" (org-entry-get (point) "EXPORT_ELEVENTY_PERMALINK"))))
    (if post-automatically
        (my-mastodon-toot-public-string message)
      (mastodon-toot)
      (insert message))))

;; https://kitchingroup.cheme.cmu.edu/blog/2013/05/05/Getting-keyword-options-in-org-files/
(defun my-org-keywords ()
  "Parse the buffer and return a cons list of (property . value).
This is extracted from lines like:
#+PROPERTY: value"
  (org-element-map (org-element-parse-buffer 'element) 'keyword
    (lambda (keyword) (cons (org-element-property :key keyword)
                            (org-element-property :value keyword)))))

(defun my-11ty-copy-file-and-insert-into-org (filename caption)
  (interactive (list (read-file-name "File: ")
                     (read-string "Caption: ")))
	(let ((path (expand-file-name
							 (file-name-nondirectory filename)
							 (expand-file-name
								(org-entry-get-with-inheritance
								 "EXPORT_ELEVENTY_FILE_NAME")
								(assoc-default "ELEVENTY_BASE_DIR" (my-org-keywords)))
							 )))
		(copy-file filename path t)
		(insert "#+CAPTION: " caption "\n"
						(org-link-make-string (concat "file:" path)) "\n")))
;; 11ty static site generation:1 ends here

;; [[file:Sacha.org::#11ty][11ty static site generation:2]]
(defun my-org-replace-with-permalink ()
	(interactive)
	(let* ((elem (org-element-context))
				 (path (org-element-property :path elem))
				 (description (org-element-property :description elem))
				 (type (org-element-property :type elem))
				 (permalink (org-entry-get (point) "EXPORT_ELEVENTY_PERMALINK" t))
				 (base-url "https://sachachua.com"))
		(when (member type '("file" "audio" "video"))
			(delete-region (org-element-begin elem) (org-element-end elem))
			(insert (org-link-make-string (concat
																		 (if (string= type "file") "" (concat type ":"))
																		 base-url permalink (file-name-nondirectory path)
																		 (if (string-match "\\?.+" path)
																				 (match-string 0 path)
																			 ""))
																		description)))))
;; 11ty static site generation:2 ends here

;; DONE Linking to blog topics
;; CLOSED: [2024-10-18 Fri 17:55]
;; :PROPERTIES:
;; :Effort:   0:30
;; :QUANTIFIED: Emacs
;; :END:
;; :LOGBOOK:
;; - State "DONE"       from "STARTED"    [2024-10-18 Fri 17:55]
;; CLOCK: [2024-10-18 Fri 17:43]--[2024-10-18 Fri 17:51] =>  0:08
;; :END:


;; [[file:Sacha.org::*Linking to blog topics][Linking to blog topics:1]]
(defun my-org-topic-open (link &rest _)
	"Find the post."
	(find-file (format "~/sync/topic/%s.org" link)))

(defun my-org-topic-export (link desc format _)
	(let ((path (concat (if (eq format '11ty) "/" "https://sachachua.com/")
											"topic/"
											(replace-regexp-in-string "\\.html$" "/"
																								(replace-regexp-in-string "^/\\|index.html$" ""
																																					link)))))
		(pcase format
			((or 'html '11ty) (format "<a href=\"%s\">%s</a>" path (or desc link)))
      ('latex (format "\\href{%s}{%s}" path desc))
      ('texinfo (format "@uref{%s,%s}" path desc))
      ('ascii (if desc (format "%s (%s)" desc path)
								path)))))

(defun my-org-topic-complete ()
	(concat "topic:"
					(completing-read
					 "Topic: "
					 (mapcar (lambda (o) (replace-regexp-in-string ".org$" "" o))
									 (directory-files "~/sync/topic" "\\.org" nil)))))

(org-link-set-parameters
	 "topic"
	 :follow #'my-org-topic-open
	 :insert-description #'my-org-link-insert-description
	 :export #'my-org-topic-export
	 :complete #'my-org-topic-complete)
;; Linking to blog topics:1 ends here

;; Linking to blog posts
;; :PROPERTIES:
;; :CUSTOM_ID: linking-to-blog-posts
;; :END:

;; #+begin_update
;; - [2024-01-19 Fri]: added link description
;; #+end_update

;; #+NAME: org-blog-link

;; [[file:Sacha.org::org-blog-link][org-blog-link]]
(defvar my-blog-base-url "https://sachachua.com/")

(defun my-org-blog-complete ()
	(let ((base (replace-regexp-in-string "/$" "" my-blog-base-url)))
		(completing-read
		 "Post: "
		 (mapcar
			(lambda (o)
				(concat base (alist-get 'permalink o)))
			(let* ((json-object-type 'alist)
						 (json-array-type 'list))
				(json-read-file "~/sync/static-blog/_site/blog/all/index.json"))))))

(defun my-org-blog-export (link desc format _)
	(let ((path (concat (if (eq format '11ty) "/" "https://sachachua.com/")
											(replace-regexp-in-string "\\.html$" "/"
																								(replace-regexp-in-string "^/\\|index.html$" ""
																																					link)))))
		(pcase format
			((or 'html '11ty) (format "<a href=\"%s\">%s</a>" path (or desc link)))
      ('latex (format "\\href{%s}{%s}" path desc))
      ('texinfo (format "@uref{%s,%s}" path desc))
      ('ascii (if desc (format "%s (%s)" desc path)
								path)))))

(defun my-11ty-html-filename (link)
	"Return the HTML file for LINK."
	(setq link (replace-regexp-in-string "^blog:" "" link))
	(if (file-exists-p link)
			link
		(catch 'found
			(dolist (f
							 (list
								(expand-file-name "index.html"
																	(expand-file-name
																	 (if (string-match "^/" link)
																			 (concat "." link)
																		 link)
																	 my-11ty-base-dir))
								(expand-file-name "index.html"
																	(expand-file-name
																	 (if (string-match "^/" link)
																			 (concat "." link)
																		 link)
																	 (expand-file-name "blog" my-11ty-base-dir)))
								(replace-regexp-in-string
								 "/$" ".html"
								 (expand-file-name
									(if (string-match "^/" link)
											(concat "." link)
										link)
									my-11ty-base-dir))
								(replace-regexp-in-string
								 "/$" ".html"
								 (expand-file-name
									(if (string-match "^/" link)
											(concat "." link)
										link)
									(expand-file-name "blog" my-11ty-base-dir)))))
				(when (file-exists-p f)
					(throw 'found f))))))

(defun my-org-blog-open (link &rest _)
	"Find the post if it exists, or open the HTML."
	(with-current-buffer (find-file-noselect "~/sync/orgzly/posts.org")
		(let ((pos (org-find-property "EXPORT_ELEVENTY_PERMALINK" link)))
			(if pos
					(progn (goto-char pos) (switch-to-buffer (current-buffer)))
				(when-let ((filename (my-11ty-html-filename link)))
					(find-file filename))))))

(defun my-org-link-insert-description (link &optional description)
	(unless description
		(my-page-title (my-org-link-as-url link))))

(use-package org
	:config
	(org-link-set-parameters
	 "blog"
	 :follow #'my-org-blog-open
	 :insert-description #'my-org-link-insert-description
	 :export #'my-org-blog-export
	 :complete #'my-org-blog-complete))
;; org-blog-link ends here

;; Making it easier to add a category to a blog post  :embark:
;; :PROPERTIES:
;; :Effort:   0:30
;; :QUANTIFIED: Coding
;; :END:
;; :LOGBOOK:
;; CLOCK: [2024-10-16 Wed 10:07]--[2024-10-16 Wed 10:22] =>  0:15
;; :END:

;; I want to be able to quickly add a category to a post when I'm looking at the file or when I'm working with a link.


;; [[file:Sacha.org::*Making it easier to add a category to a blog post][Making it easier to add a category to a blog post:1]]
(defun my-11ty-complete-blog-post ()
	(completing-read
	 "Post: "
	 (mapcar (lambda (o)
						 (file-name-directory (file-relative-name o my-11ty-base-dir)))
					 (directory-files-recursively (expand-file-name "blog" my-11ty-base-dir) "index\\.html" nil))))
(defun my-11ty-ripgrep ()
	(interactive)
	(consult-ripgrep (expand-file-name "blog" my-11ty-base-dir)))

(defun my-11ty-post-categories (file)
	(assoc-default 'categories
								 (let ((json-object-type 'alist)
											 (json-array-type 'list))
									 (json-read-file (my-11ty-json-filename file)))))

(defun my-11ty-complete-category (prompt &optional categories)
	(let ((all-categories
				 (json-read-file
					(expand-file-name "siteCategories.json"
														(expand-file-name "_data" my-11ty-base-dir)))))
		(completing-read
		 (if categories
				 (format  "%s(current: %s) "
									prompt
									(string-join categories ", "))
			 prompt)
		 (mapcar (lambda (c) (assoc-default 'slug c))
						 all-categories))))

(defun my-11ty-json-filename (path)
	(concat (file-name-sans-extension (my-11ty-html-filename path)) ".11tydata.json"))

(defun my-11ty-add-category-to-post (file new-category)
	(interactive (list (buffer-file-name)
										 (my-11ty-complete-category "Add category: "
																								(my-11ty-post-categories file))))
	(let* ((json-object-type 'alist)
				 (json-array-type 'list)
				 (json-file (my-11ty-json-filename file))
				 (json (json-read-file json-file))
				 (categories (assoc-default 'categories json)))
		(unless (member new-category categories)
			(if categories
					(setcdr (assoc 'categories json)
									(cons new-category categories))
				(setq json (cons (cons 'categories (cons new-category categories)) json)))
			(with-temp-file json-file
				(insert (json-encode json))))))
;; Making it easier to add a category to a blog post:1 ends here



;; Then it makes sense to be able to work with blog URLs from an Embark action:


;; [[file:Sacha.org::*Making it easier to add a category to a blog post][Making it easier to add a category to a blog post:2]]
(defun my-embark-org-blog-target ()
	"Identify when we're looking at a blog: link."
	(cond
	 ((and (derived-mode-p 'org-mode)
				 (let ((context (org-element-context)))
					 (and (org-element-type-p context 'link)
								(string= (org-element-property :type context) "blog"))))
		(cons 'my-blog (org-element-property :path (org-element-context))))))

(defun my-embark-org-blog-add-category (blog &optional category)
	(interactive (list (my-org-blog-complete)))
	(unless category
		(setq category
					(my-11ty-complete-category
					 "Add category: "
					 (my-11ty-post-categories (my-11ty-html-filename blog)))))
	(my-11ty-add-category-to-post (my-11ty-html-filename blog) category))

(defun my-embark-org-blog-open-in-browser (path)
	(interactive (list (my-org-blog-complete)))
	(browse-url (concat "https://sachachua.com/"
											(replace-regexp-in-string "^\\(blog:\\)?/" "" path))))

(with-eval-after-load 'embark
	(add-to-list 'embark-target-finders #'my-embark-org-blog-target)
	(defvar-keymap embark-my-blog-actions
		:doc "Shortcuts for my blog"
		"c" #'my-embark-org-blog-add-category
		"b" #'my-embark-org-blog-open-in-browser)
	(add-to-list 'embark-keymap-alist '(my-blog . embark-my-blog-actions)))
;; Making it easier to add a category to a blog post:2 ends here

;; embark-11ty                                     :11ty:org:emacs:embark:
;; :PROPERTIES:
;; :CUSTOM_ID: embark-11ty
;; :END:


;; [[file:Sacha.org::#embark-11ty][embark-11ty:1]]
(defvar my-11ty-base-dir "~/proj/static-blog/")
(defun my-embark-11ty-find-org (url)
	(interactive (list (my-complete-blog-post-url)))
	(setq url (or (my-org-link-as-url url) url))
	(let ((path (replace-regexp-in-string (concat "^" (regexp-quote my-blog-base-url))
																				""
																				url))
				pos)
		;; check my config
		(catch 'found
			(dolist (file '("~/sync/emacs/Sacha.org"
											"~/sync/orgzly/posts.org"))
				(with-current-buffer (find-file-noselect file)
					(setq pos (org-find-property "EXPORT_ELEVENTY_PERMALINK" path))
					(when pos
						(switch-to-buffer (current-buffer))
						(goto-char pos)
						(throw 'found (buffer-file-name)))))
			(when (file-exists-p
						 (expand-file-name "index.org"
															 (concat my-11ty-base-dir path)))
				(find-file
				 (expand-file-name "index.org" (concat my-11ty-base-dir path)))
				(throw 'found (buffer-file-name))))))
(defun my-embark-11ty-find-html (url)
	(interactive (list (my-complete-blog-post-url)))
	(when (string-match "https://sachachua\\.com\\(/blog/.*\\)" (my-org-link-as-url url))
		(let ((path (match-string 1 url))
					pos)
			;; check my config
			(catch 'found
				(dolist (file '("~/sync/emacs/Sacha.org"
												"~/sync/orgzly/posts.org"))
					(with-current-buffer (find-file-noselect file)
						(setq pos (org-find-property "EXPORT_ELEVENTY_PERMALINK" path))
						(when pos
							(switch-to-buffer (current-buffer))
							(goto-char pos)
							(throw 'found (buffer-file-name)))))
				(when (file-exists-p
							 (expand-file-name "index.org"
																 (concat my-11ty-base-dir path)))
					(find-file
					 (expand-file-name "index.org" (concat my-11ty-base-dir path)))
					(throw 'found (buffer-file-name)))))))
(with-eval-after-load 'embark
	(define-key embark-url-map "v" #'my-embark-11ty-find-org)
	(define-key embark-org-link-map "v" #'my-embark-11ty-find-org))
;; embark-11ty:1 ends here

;; Moving my Org post subtree to the 11ty directory :11ty:org:emacs:blogging:
;; :PROPERTIES:
;; :EXPORT_DATE: 2023-01-09T11:07:23-0500
;; :EXPORT_ELEVENTY_PERMALINK: /blog/2023/01/moving-my-org-post-subtree-to-the-11ty-directory/
;; :EXPORT_ELEVENTY_FILE_NAME: blog/2023/01/moving-my-org-post-subtree-to-the-11ty-directory/
;; :CUSTOM_ID: moving-my-org-post-subtree-to-the-11ty-directory
;; :END:

;; I sometimes want to move the Org source for my blog posts to the same
;; directory as the 11ty-exported HTML. This should make it easier to
;; update and reexport blog posts in the future. The following code
;; copies or moves the subtree to the 11ty export directory.


;; [[file:Sacha.org::#moving-my-org-post-subtree-to-the-11ty-directory][Moving my Org post subtree to the 11ty directory:1]]
(defun my-org-11ty-copy-subtree (&optional do-cut subtreep)
	"Copy the subtree for the current post to the 11ty export directory.
With prefix arg, move the subtree."
	(interactive (list current-prefix-arg))
	(let* ((info (org-combine-plists
								(org-export--get-export-attributes '11ty subtreep)
								(org-export--get-buffer-attributes)
								(org-export-get-environment '11ty subtreep)))
				 (file-properties
					(org-element-map (org-element-parse-buffer) 'keyword
						(lambda (el) (cons (org-element-property :key el)
															 (org-element-property :value el)))))
				 (entry-properties (org-entry-properties))
				 (filename (expand-file-name
										"index.org"
										(expand-file-name
										 (plist-get info :file-name)
										 (plist-get info :base-dir))))
				 (parent-pos
					(org-find-property "EXPORT_ELEVENTY_FILE_NAME"
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
							(insert (or
											 (mapconcat (lambda (o) (format "#+%s: %s" (car o) (cdr o))) file-properties "\n")
											 "")
											"\n")
							(org-yank))
					(insert body))))))
;; Moving my Org post subtree to the 11ty directory:1 ends here



;; Then this adds a link to it:


;; [[file:Sacha.org::#moving-my-org-post-subtree-to-the-11ty-directory][Moving my Org post subtree to the 11ty directory:2]]
(defun my-org-export-filter-body-add-index-link (info)
  (when (and
				 (plist-get info :file-name)
				 (plist-get info :base-dir)
				 (file-exists-p (expand-file-name
												 "index.org"
												 (expand-file-name
													(plist-get info :file-name)
													(plist-get info :base-dir)))))
		(goto-char (point-max))
		(insert
		 (format "<div><a href=\"%sindex.org\">View org source for this post</a></div>"
						 (plist-get info :permalink)))))

(with-eval-after-load 'ox-11ty
  (add-to-list 'org-11ty-process-export-functions #'my-org-export-filter-body-add-index-link))
;; Moving my Org post subtree to the 11ty directory:2 ends here



;; Then I want to wrap the whole thing up in an export function:


;; [[file:Sacha.org::#moving-my-org-post-subtree-to-the-11ty-directory][Moving my Org post subtree to the 11ty directory:3]]
(defun my-org-11ty-export (&optional async subtreep visible-only body-only ext-plist)
  (let* ((info (org-11ty--get-info subtreep visible-only))
         (file (org-11ty--base-file-name subtreep visible-only)))
		(unless (string= (plist-get info :input-file)
										 (expand-file-name
											"index.org"
											(expand-file-name
											 (plist-get info :file-name)
											 (plist-get info :base-dir))))
			(save-window-excursion
				(my-org-11ty-copy-subtree nil subtreep)))
		(org-11ty-export-to-11tydata-and-html async subtreep visible-only body-only ext-plist)
		;(my-org-11ty-find-file)
		))
;; Moving my Org post subtree to the 11ty directory:3 ends here



;; Now to figure out how to override the export menu. Totally messy hack!


;; [[file:Sacha.org::#moving-my-org-post-subtree-to-the-11ty-directory][Moving my Org post subtree to the 11ty directory:4]]
(with-eval-after-load 'ox-11ty
	(map-put (caddr (org-export-backend-menu (org-export-get-backend '11ty)))
					 ?1 (list "To Org, 11tydata.json, HTML" 'my-org-11ty-export)))
;; Moving my Org post subtree to the 11ty directory:4 ends here

;; Cleaning up export
;; :PROPERTIES:
;; :CUSTOM_ID: cleaning-up-export
;; :END:
;; Timestamps and section numbers make my published files look more
;; complicated than they are. Let's turn them off by default, and let's use fancy HTML5.

;; #+NAME: org-clean-up-export

;; [[file:Sacha.org::org-clean-up-export][org-clean-up-export]]
(setq org-html-doctype "html5")
(setq org-html-html5-fancy t)
(setq org-export-with-section-numbers nil)
(setq org-html-include-timestamps nil)
(setq org-export-with-sub-superscripts nil)
(setq org-export-with-toc nil)
(setq org-html-toplevel-hlevel 2)
(setq org-export-htmlize-output-type 'css)
(setq org-export-with-broken-links t)
(setq org-ascii-text-width 10000)
(setq-default tab-width 2)
;; org-clean-up-export ends here



;; This makes it easier to publish my files:


;; [[file:Sacha.org::#cleaning-up-export][Cleaning up export:2]]
(setq org-publish-project-alist
      '(("stream"
         :base-directory "~/proj/stream"
         )
        ("emacs-config"
         :base-directory "~/.config/emacs"
         :publishing-directory "~/.config/emacs"
         :publishing-function my-org-html-publish-to-html-trustingly
         )
        ("book-notes"
         :base-directory "c:/sacha/Dropbox/books"
         :publishing-directory "c:/sacha/Dropbox/books/html"
         :publishing-function my-org-html-publish-to-html-trustingly
         :makeindex t)))
;(load "~/proj/dev/emacs-chats/build-site.el" t)
;(load "~/proj/dev/emacs-notes/build-site.el" t)
;; Cleaning up export:2 ends here



;; If a file is in a publishing project, publish it.


;; [[file:Sacha.org::#cleaning-up-export][Cleaning up export:3]]
(defun my-org-publish-maybe ()
  (require 'ox-publish)
  (interactive)
  (save-excursion
    (if (org-publish-get-project-from-filename
         (buffer-file-name (buffer-base-buffer)) 'up)
        (org-publish-current-file t)
      (my-org-html-export-trustingly))))
;; Cleaning up export:3 ends here



;; Make it easy to publish and browse a file.


;; [[file:Sacha.org::#cleaning-up-export][Cleaning up export:4]]
(defun my-org-publish-and-browse ()
  (interactive)
  (save-buffer)
  (my-org-publish-maybe)
  (browse-url (org-export-output-file-name ".html" nil default-directory)))
(bind-key "<apps> b" 'my-org-publish-and-browse)
;; Cleaning up export:4 ends here

;; Publish without prompting
;; :PROPERTIES:
;; :CUSTOM_ID: publish-without-prompting
;; :END:

;; I want to be able to export without having to say yes to code blocks all the time.


;; [[file:Sacha.org::#publish-without-prompting][Publish without prompting:1]]
(defun my-org-html-export-trustingly ()
  (interactive)
  (let ((org-confirm-babel-evaluate nil))
    (org-html-export-to-html)))

(defun my-org-html-publish-to-html-trustingly (plist filename pub-dir)
  (let ((org-confirm-babel-evaluate nil))
    (org-html-publish-to-html plist filename pub-dir)))
;; Publish without prompting:1 ends here

;; Special blocks
;; :PROPERTIES:
;; :CUSTOM_ID: special-blocks
;; :END:
;; #+NAME: org-special-blocks

;; [[file:Sacha.org::org-special-blocks][org-special-blocks]]
(use-package org-special-block-extras
  :if my-laptop-p
  :hook (org-mode . org-special-block-extras-mode)
	:init (setq org-special-block-add-html-extra nil)
  :config
  ;; Use short names like ‘defblock’ instead of the fully qualified name
  ;; ‘org-special-block-extras--defblock’
	(setcdr org-special-block-extras-mode-map nil)
	(org-defblock
	 my_details (title "Details" title-color "Green" open "")
	 "Top level (HTML & 11ty)OSPE-RESPECT-NEWLINES? Enclose contents in a folded up box."
	 (message "my_details %s %s %s" title title-color open)
   (cond
    ((eq backend '11ty)
     (format
      "{%% details \"%s\" %s%%}\n%s\n{%% enddetails %%}"
      title (if (string= open "") "" ", \"open\"") contents))
    ((eq backend 'html)
     (format
      "<details class=\"code-details\"
                 style =\"padding: 1em;
                          border-radius: 15px;
                          font-size: 0.9em;
                          box-shadow: 0.05em 0.1em 5px 0.01em  #00000057;\"%s>
                  <summary>
                    <strong>
                      <font face=\"Courier\" size=\"3\" color=\"%s\">
                         %s
                      </font>
                    </strong>
                  </summary>
                  %s
               </details>"
      (if (string= open "") "" " open") title-color title contents))))

  (org-defblock columns nil nil
								"Top level (HTML & wp & 11ty)OSPE-RESPECT-NEWLINES? Split into columns using Foundation."
								(format "<div class=\"row\">%s</div>" contents))
  (org-defblock column50 nil nil
								"Top level (HTML & wp & 11ty)OSPE-RESPECT-NEWLINES? Split into columns."
								(format "<div class=\"columns small-12 medium-6 large-6\">%s</div>" contents))
	(org-defblock short (yt nil video nil audio nil)
							"Top level (HTML & 11ty)OSPE-RESPECT-NEWLINES? Mark up a YouTube short."
							(let ((yt-link (and yt (format "<a href=\"https://youtube.com/watch?v=%s\">watch this on YouTube</a>"
																						 (my-org-yt-id yt))))
										(video-link (and video
																		 (format "<a href=\"%s\">download the video</a>"
																						 (org-export-file-uri video))))
										(audio-link (and audio
																		 (format "<a href=\"%s\">download the audio</a>"
																						 (org-export-file-uri audio)))))
								(concat
								 "<div class=\"row\"><div class=\"columns\"><div style=\"width: 400px\">"
								 (if video
										 (my-org-video-export (concat "video:" (expand-file-name video)) nil backend nil)
									 (my-org-yt-export yt nil backend nil))
								 "</div></div><div class=\"columns\">"
								 contents
								 "<p>You can "
								 (cond
									((and yt-link video-link audio-link) (format "%s, %s, or %s." yt-link video-link audio-link))
									((and yt-link video-link) (format "%s or %s." yt-link video-link))
									((and yt-link audio-link) (format "%s or %s." yt-link audio-link))
									((and video-link audio-link) (format "%s or %s." video-link audio-link))
									(video-link (format "%s." video-link))
									(audio-link (format "%s." audio-link)))
								 "</p></div></div>")))
	)
;; org-special-blocks ends here



;; And here's a little thing to convert a two-level list into my collapsible sections:


;; [[file:Sacha.org::#special-blocks][Special blocks:2]]
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
;; Special blocks:2 ends here

;; Adding a custom header argument to Org Mode source blocks and using that argument during export :org:emacs:
;; :PROPERTIES:
;; :EXPORT_DATE: 2023-01-27T10:11:01-0500
;; :EXPORT_ELEVENTY_PERMALINK: /blog/2023/01/adding-a-custom-header-argument-to-org-mode-source-blocks-and-using-that-argument-during-export/
;; :EXPORT_ELEVENTY_FILE_NAME: blog/2023/01/adding-a-custom-header-argument-to-org-mode-source-blocks-and-using-that-argument-during-export/
;; :CUSTOM_ID: adding-a-custom-header-argument-to-org-mode-source-blocks-and-using-that-argument-during-export
;; :END:

;; I sometimes want to put long source blocks in a
;; ~<details><summary>...</summary>...</details>~ block when I export to
;; HTML, so that they're tucked away in a collapsible block. I tried
;; using ~https://github.com/alhassy/org-special-block-extras~ to define
;; my own ~#+begin_my_details "summary text" ... #+end_my_details~ block,
;; but source blocks inside ~my_details~ doesn't get fontlocked properly
;; while in the Org file. I wanted to add a ~:summary~ attribute to the
;; regular src blocks, and to change the HTML export to wrap the code in
;; ~details~ if the summary was specified.


;; [[file:Sacha.org::#adding-a-custom-header-argument-to-org-mode-source-blocks-and-using-that-argument-during-export][Adding a custom header argument to Org Mode source blocks and using that argument during export:1]]
(setq org-babel-exp-code-template "#+begin_src %lang%switches%flags :summary %summary\n%body\n#+end_src")
(defun my-org-html-src-block (src-block _contents info)
	(let* ((result (org-html-src-block src-block _contents info))
				 (block-info
					(org-with-point-at (org-element-property :begin src-block)
						(org-babel-get-src-block-info)))
				 (summary (assoc-default :summary (elt block-info 2))))
		(if (member summary '("%summary" ""))
				result
			(format "<details><summary>%s</summary>%s</details>"
							summary
							result))))

(defun my-org-11ty-src-block (src-block _contents info)
	(let* ((result (org-11ty-src-block src-block _contents info))
				 (block-info
					(org-with-point-at (org-element-property :begin src-block)
						(org-babel-get-src-block-info)))
				 (summary (assoc-default :summary (elt block-info 2))))
		(if (member summary '("%summary" ""))
				result
			(format "<details><summary>%s</summary>%s</details>"
							summary
							result))))

(with-eval-after-load 'ox-html
	(map-put!
	 (org-export-backend-transcoders (org-export-get-backend 'html))
	 'src-block 'my-org-html-src-block))
(with-eval-after-load 'ox-11ty
	(map-put!
	 (org-export-backend-transcoders (org-export-get-backend '11ty))
	 'src-block 'my-org-11ty-src-block))
;; Adding a custom header argument to Org Mode source blocks and using that argument during export:1 ends here

;; Stylesheet / header
;; :PROPERTIES:
;; :CUSTOM_ID: stylesheet-header
;; :END:
;; Might as well take advantage of my stylesheet:

;; #+NAME: org-styles

;; [[file:Sacha.org::org-styles][org-styles]]
(setq org-html-head "
       <link rel=\"stylesheet\" type=\"text/css\" href=\"https://sachachua.com/assets/css/style.css\"></link>
       <link rel=\"stylesheet\" type=\"text/css\" href=\"https://sachachua.com/assets/css/org-export.css\"></link>
       <script src=\"https://ajax.googleapis.com/ajax/libs/jquery/1.11.0/jquery.min.js\"></script>
       <script src=\"https://sachachua.com/assets/js/misc.js\"></script>")
(setq org-html-htmlize-output-type 'css)
(setq org-src-fontify-natively t)
;; org-styles ends here

;; Footer
;; :PROPERTIES:
;; :CUSTOM_ID: footer
;; :END:

;; Make it easy to scroll to the top:


;; [[file:Sacha.org::#footer][Footer:1]]
(setq org-html-preamble "<a name=\"top\" id=\"top\"></a>")
(setq org-html-postamble "
       <style type=\"text/css\">
       .back-to-top {
           position: fixed;
           bottom: 2em;
           right: 0px;
           text-decoration: none;
           color: #000000;
           background-color: rgba(235, 235, 235, 0.80);
           font-size: 12px;
           padding: 1em;
           display: none;
       }

       .back-to-top:hover {
           background-color: rgba(135, 135, 135, 0.50);
       }
       </style>

       <div class=\"back-to-top\">
       <a href=\"#top\">Back to top</a> | <a href=\"mailto:sacha@sachachua.com\">E-mail me</a>
       </div>

       <script type=\"text/javascript\">
           var offset = 220;
           var duration = 500;
           jQuery(window).scroll(function() {
               if (jQuery(this).scrollTop() > offset) {
                   jQuery('.back-to-top').fadeIn(duration);
               } else {
                   jQuery('.back-to-top').fadeOut(duration);
               }
           });
       </script>")
;; Footer:1 ends here

;; Copy region
;; :PROPERTIES:
;; :CUSTOM_ID: copy-region
;; :END:
;; Sometimes I want a region's HTML in my kill-ring/clipboard without any of the extra fluff:


;; [[file:Sacha.org::#copy-region][Copy region:1]]
(defun my-org-copy-region-as-html (beg end &optional level)
  "Make it easier to copy code for Wordpress posts and other things."
  (interactive "r\np")
  (let ((org-export-html-preamble nil)
        (org-html-toplevel-hlevel (or level 3)))
    (kill-new
     (org-export-string-as (buffer-substring beg end) 'html t))))
;; Copy region:1 ends here



;; Sometimes I want a subtree:


;; [[file:Sacha.org::#copy-region][Copy region:2]]
(defun my-org-copy-subtree-as-html ()
  (interactive)
  (my-org-copy-region-as-html
   (org-back-to-heading)
   (org-end-of-subtree)))
;; Copy region:2 ends here

;; UTF-8 checkboxes
;; :PROPERTIES:
;; :CUSTOM_ID: utf-8-checkboxes
;; :END:

;; This snippet turns =- [X]= into ☑ and =- [ ]= into ☐, but leaves =[-]= alone.

;; [[file:Sacha.org::#utf-8-checkboxes][UTF-8 checkboxes:1]]
(setq org-html-checkbox-type 'unicode)
(setq org-html-checkbox-types
      '((unicode (on . "<span class=\"task-done\">&#x2611;</span>")
                 (off . "<span class=\"task-todo\">&#x2610;</span>")
                 (trans . "<span class=\"task-in-progress\">[-]</span>"))))
;; UTF-8 checkboxes:1 ends here

;; Share my Emacs configuration
;; :PROPERTIES:
;; :CUSTOM_ID: share-my-emacs-configuration
;; :END:

;; This code gets around the fact that my config is called Sacha.org, but
;; I want it to export as sacha-emacs.org in my Dropbox's public
;; directory. Although now that I'm shifting to Github Pages, maybe I
;; don't need this any more...


;; [[file:Sacha.org::#share-my-emacs-configuration][Share my Emacs configuration:1]]
(defun my-org-share-emacs ()
  "Share my Emacs configuration."
  (interactive)
  (let* ((destination-dir "~/Dropbox/Public/")
         (destination-filename "sacha-emacs.org"))
    (my-save-new-packages)
    (with-current-buffer (find-file "~/.config/emacs/Sacha.org")
      (save-restriction
        (save-excursion
          (widen)
          (write-region (point-min) (point-max)
                        (expand-file-name destination-filename destination-dir))
          (with-current-buffer (find-file-noselect (expand-file-name
                                                    destination-filename destination-dir))
            (org-babel-tangle-file buffer-file-name
                                   (expand-file-name
                                    "sacha-emacs.el" destination-dir) "emacs-lisp")
            (org-html-export-to-html)))))))
;; Share my Emacs configuration:1 ends here

;; Remembering to set custom IDs for this file
;; :PROPERTIES:
;; :CUSTOM_ID: remembering-to-set-custom-ids-for-this-file
;; :END:


;; [[file:Sacha.org::#remembering-to-set-custom-ids-for-this-file][Remembering to set custom IDs for this file:1]]
(defun my-assign-custom-ids ()
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
;; Remembering to set custom IDs for this file:1 ends here

;; Beamer
;; :PROPERTIES:
;; :CUSTOM_ID: beamer
;; :END:

;; [[file:Sacha.org::#beamer][Beamer:1]]
(with-eval-after-load 'org
  (require 'ox-latex)
  (add-to-list 'org-latex-classes
               '("beamer"
                 "\\documentclass\[presentation\]\{beamer\}"
                 ("\\section\{%s\}" . "\\section*\{%s\}")
                 ("\\subsection\{%s\}" . "\\subsection*\{%s\}")
                 ("\\subsubsection\{%s\}" . "\\subsubsection*\{%s\}")))
  (add-to-list 'org-latex-classes
               '("memoir"
                 "\\documentclass\{memoir\}"
                 ("\\section\{%s\}" . "\\section*\{%s\}")
                 ("\\subsection\{%s\}" . "\\subsection*\{%s\}")
                 ("\\subsubsection\{%s\}" . "\\subsubsection*\{%s\}"))))
;; Beamer:1 ends here

;; PlantUML
;; :PROPERTIES:
;; :CUSTOM_ID: plantuml
;; :END:


;; [[file:Sacha.org::#plantuml][PlantUML:1]]
(setq org-plantuml-jar-path (expand-file-name "/usr/share/plantuml/plantuml.jar"))
(add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
;; PlantUML:1 ends here

;; ox-hugo
;; :PROPERTIES:
;; :CUSTOM_ID: ox-hugo
;; :END:


;; [[file:Sacha.org::#ox-hugo][ox-hugo:1]]
(use-package ox-hugo
  :ensure t            ;Auto-install the package from Melpa (optional)
  :after ox)
;; ox-hugo:1 ends here

;; Org Mode: Asynchronous export and tangle of a large file           :org:
;;      :PROPERTIES:
;;      :CUSTOM_ID: org-async-export-and-tangle
;;      :END:

;; I have a pretty large [[https://sachachua.com/dotemacs][Emacs configuration file]]. It's annoying to wait
;; 11 seconds for it to export to HTML or 12 seconds to tangle.
;; Fortunately, Org Mode allows me to export asynchronously. I tried it
;; out from =org-export-dispatch= (=C-c C-e=) by using the =C-a= option.
;; It worked pretty well, but it was a bit slow because it loaded my full
;; configuration. Fortunately, there's a way to use a smaller
;; configuration that focuses on just the packages needed.

;; #+NAME: org-async-variables

;; [[file:Sacha.org::org-async-variables][org-async-variables]]
(setq org-export-async-init-file "~/.config/emacs/org-async-export-config.el")
(setq org-export-async-debug t)
;; org-async-variables ends here



;; I want my config file to be tangled and exported
;; to HTML regularly so that I don't forget to do so.
;; The following code exports my config, but only if
;; I saved it myself instead of when I auto-save it
;; by focusing away from Emacs.


;; [[file:Sacha.org::#org-async-export-and-tangle][Org Mode: Asynchronous export and tangle of a large file:3]]
(defmacro my-org-debounce-idle-timer (seconds var body &rest args)
  `(progn
     (defvar ,var nil "Timer.")
     (when (timerp ,var) (cancel-timer ,var))
     (setq ,var (run-with-idle-timer ,seconds nil ,body ,@args))))
(defvar my-unfocusing nil "Non-nil when I'm in the middle of unfocusing.")
(defun my-org-async-export-and-tangle (&optional filename)
  (async-start
   `(lambda ()
      ;; make async emacs aware of packages (for byte-compilation)
      (package-initialize)
      (setq package-enable-at-startup nil)
      (require 'org)
			(setq-default tab-width 2)
			(setq org-babel-default-header-args
			      '((:session . "none")
			        (:results . "drawer replace")
							(:comments . "both")
			        (:exports . "code")
			        (:cache . "no")
			        (:eval . "never-export")
			        (:hlines . "no")
			        (:tangle . "no")))
			(setq org-edit-src-auto-save-idle-delay 5)
      (org-babel-tangle-file ,(buffer-file-name))
      )
   (lambda (&rest results) (message "Tangled.")))
  (org-export-to-file 'html (or filename "index.html") t))
(defun my-org-export-and-tangle-if-saved-in-focus ()
	(interactive)
  (when (frame-focus-state)
    (message "Scheduling export...")
    (my-org-debounce-idle-timer
		 10
     my-export-org-config
     (lambda (buf)
       (with-current-buffer buf
         (my-org-async-export-and-tangle "index.html")))
     (current-buffer))))
(define-minor-mode my-org-export-and-tangle-when-saved-in-focus-mode
  "Toggle a mode for exporting and tangling when saved.
Interactively with no argument, this command toggles the mode.
A positive prefix argument enables the mode, any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state."
  :group 'my
  (if my-org-export-and-tangle-when-saved-in-focus-mode
      (add-hook 'after-save-hook #'my-org-export-and-tangle-if-saved-in-focus nil t)
    (remove-hook 'after-save-hook #'my-org-export-and-tangle-if-saved-in-focus t)))

(defun my-org-save-and-tangle-my-config ()
	(when (string= (buffer-file-name) (expand-file-name "~/sync/emacs/Sacha.org")) (my-org-export-and-tangle-when-saved-in-focus-mode 1)))

(use-package org
  :hook ((org-mode . my-org-save-and-tangle-my-config)))
;; Org Mode: Asynchronous export and tangle of a large file:3 ends here

;; PDF
;; :PROPERTIES:
;; :CUSTOM_ID: pdf
;; :END:

;; https://so.nwalsh.com/2020/01/05-latex , but I use letter paper instead of A4.


;; [[file:Sacha.org::#pdf][PDF:1]]
(setq org-latex-compiler "xelatex")
(setq org-latex-pdf-process
      (list (concat "latexmk -"
                    org-latex-compiler
                    " -recorder -synctex=1 -bibtex-cond %b")))
(setq org-latex-default-packages-alist
      '(("" "graphicx" t)
        ("" "grffile" t)
        ("" "longtable" nil)
        ("" "wrapfig" nil)
        ("" "rotating" nil)
        ("normalem" "ulem" t)
        ("" "amsmath" t)
        ("" "textcomp" t)
        ("" "amssymb" t)
        ("" "capt-of" nil)
        ("" "hyperref" nil)))
(setq org-latex-classes
'(("article"
"\\RequirePackage{fix-cm}
\\PassOptionsToPackage{svgnames}{xcolor}
\\documentclass[11pt]{article}
\\usepackage{fontspec}
\\setmainfont{Noto Sans}
\\setsansfont[Scale=MatchLowercase]{Noto Sans}
\\setmonofont[Scale=MatchLowercase]{Hack}
\\usepackage{sectsty}
\\allsectionsfont{\\sffamily}
\\usepackage{enumitem}
\\setlist[description]{style=unboxed,font=\\sffamily\\bfseries}
\\usepackage{listings}
\\lstset{frame=single,aboveskip=1em,
	framesep=.5em,backgroundcolor=\\color{AliceBlue},
	rulecolor=\\color{LightSteelBlue},framerule=1pt}
\\usepackage{xcolor}
\\newcommand\\basicdefault[1]{\\scriptsize\\color{Black}\\ttfamily#1}
\\lstset{basicstyle=\\basicdefault{\\spaceskip1em}}
\\lstset{literate=
	    {§}{{\\S}}1
	    {©}{{\\raisebox{.125ex}{\\copyright}\\enspace}}1
	    {«}{{\\guillemotleft}}1
	    {»}{{\\guillemotright}}1
	    {Á}{{\\'A}}1
	    {Ä}{{\\\"A}}1
	    {É}{{\\'E}}1
	    {Í}{{\\'I}}1
	    {Ó}{{\\'O}}1
	    {Ö}{{\\\"O}}1
	    {Ú}{{\\'U}}1
	    {Ü}{{\\\"U}}1
	    {ß}{{\\ss}}2
	    {à}{{\\`a}}1
	    {á}{{\\'a}}1
	    {ä}{{\\\"a}}1
	    {é}{{\\'e}}1
	    {í}{{\\'i}}1
	    {ó}{{\\'o}}1
	    {ö}{{\\\"o}}1
	    {ú}{{\\'u}}1
	    {ü}{{\\\"u}}1
	    {¹}{{\\textsuperscript1}}1
            {²}{{\\textsuperscript2}}1
            {³}{{\\textsuperscript3}}1
	    {ı}{{\\i}}1
	    {—}{{---}}1
	    {’}{{'}}1
	    {…}{{\\dots}}1
            {⮠}{{$\\hookleftarrow$}}1
	    {␣}{{\\textvisiblespace}}1,
	    keywordstyle=\\color{DarkGreen}\\bfseries,
	    identifierstyle=\\color{DarkRed},
	    commentstyle=\\color{Gray}\\upshape,
	    stringstyle=\\color{DarkBlue}\\upshape,
	    emphstyle=\\color{Chocolate}\\upshape,
	    showstringspaces=false,
	    columns=fullflexible,
	    keepspaces=true}
\\usepackage[margin=1in,left=1.5in]{geometry}
\\usepackage{parskip}
\\makeatletter
\\renewcommand{\\maketitle}{%
  \\begingroup\\parindent0pt
  \\sffamily
  \\Huge{\\bfseries\\@title}\\par\\bigskip
  \\LARGE{\\bfseries\\@author}\\par\\medskip
  \\normalsize\\@date\\par\\bigskip
  \\endgroup\\@afterindentfalse\\@afterheading}
\\makeatother
[DEFAULT-PACKAGES]
\\hypersetup{linkcolor=Blue,urlcolor=DarkBlue,
  citecolor=DarkRed,colorlinks=true}
\\AtBeginDocument{\\renewcommand{\\UrlFont}{\\ttfamily}}
[PACKAGES]
[EXTRA]"
("\\section{%s}" . "\\section*{%s}")
("\\subsection{%s}" . "\\subsection*{%s}")
("\\subsubsection{%s}" . "\\subsubsection*{%s}")
("\\paragraph{%s}" . "\\paragraph*{%s}")
("\\subparagraph{%s}" . "\\subparagraph*{%s}"))

("report" "\\documentclass[11pt]{report}"
("\\part{%s}" . "\\part*{%s}")
("\\chapter{%s}" . "\\chapter*{%s}")
("\\section{%s}" . "\\section*{%s}")
("\\subsection{%s}" . "\\subsection*{%s}")
("\\subsubsection{%s}" . "\\subsubsection*{%s}"))

("book" "\\documentclass[11pt]{book}"
("\\part{%s}" . "\\part*{%s}")
("\\chapter{%s}" . "\\chapter*{%s}")
("\\section{%s}" . "\\section*{%s}")
("\\subsection{%s}" . "\\subsection*{%s}")
("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))
;; PDF:1 ends here

;; IDs
;; :PROPERTIES:
;; :CUSTOM_ID: ids
;; :END:

;; [[file:Sacha.org::#ids][IDs:1]]
(setq org-id-method 'ts)
(setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
;; IDs:1 ends here

;; Quick links
;; :PROPERTIES:
;; :CUSTOM_ID: quick-links
;; :END:

;; [[file:Sacha.org::#quick-links][Quick links:1]]
(setq org-link-abbrev-alist
      '(("google" . "http://www.google.com/search?q=")
        ("gmap" . "http://maps.google.com/maps?q=%s")
        ))
;; Quick links:1 ends here

;; Links to my config
;; :PROPERTIES:
;; :CUSTOM_ID: links-to-my-config
;; :END:

;; #+NAME: org-dotemacs-link

;; [[file:Sacha.org::org-dotemacs-link][org-dotemacs-link]]
(defun my-org-dotemacs-export (path desc format _)
	"Export dotemacs link."
	(pcase format
   ((or 'html '11ty 'md)
	  (format "<a href=\"https://sachachua.com/dotemacs#%s\">%s</a>" path (or desc path)))
	('ascii
		desc)))

(defun my-org-dotemacs-complete ()
	"Prompt for dotemacs."
	(interactive)
	(with-current-buffer (find-file-noselect "~/sync/emacs/Sacha.org")
		(concat "dotemacs:" (org-read-property-value "CUSTOM_ID"))))

(defun my-org-dotemacs-insert-description (link &optional description)
	(unless description))

(defun my-org-dotemacs-open (path)
	(with-current-buffer (find-file-noselect "~/sync/emacs/Sacha.org")
		(when-let ((pos (org-find-property "CUSTOM_ID" path)))
			(switch-to-buffer (current-buffer))
			(goto-char pos))))

(defun my-org-dotemacs-store ()
	(when (string= (buffer-file-name)
								 (expand-file-name "~/sync/emacs/Sacha.org"))
		(org-link-store-props
		 :link (concat "dotemacs:" (org-entry-get (point) "CUSTOM_ID"))
		 :content (org-entry-get (point) "ITEM"))))

(org-link-set-parameters
 "dotemacs"
 :complete #'my-org-dotemacs-complete
 :store #'my-org-dotemacs-store
 :insert-description #'my-org-dotemacs-insert-description
 :export #'my-org-dotemacs-export
 :follow #'my-org-dotemacs-open)
;; org-dotemacs-link ends here

;; YouTube
;; :PROPERTIES:
;; :CUSTOM_ID: youtube
;; :END:

;; #+NAME: org-yt-link

;; [[file:Sacha.org::org-yt-link][org-yt-link]]
(defvar my-org-yt-iframe-format
  (concat "<div class=\"yt-video\"><iframe width=\"456\""
          " height=\"315\""
					" title=""YouTube video player\""
          " src=\"https://www.youtube-nocookie.com/embed/%s?enablejsapi=1\""
          " frameborder=\"0\""
          " allowfullscreen>%s</iframe><a href=\"%s\">Watch on YouTube</a></div>"))
(defun my-org-yt-id (path)
	(cond
	 ((string-match "\\(?:v=\\|tu\\.be/\\|live/\\)\\([^&]+\\)" path)
		(match-string 1 path))
	 ((string-match "\\(live_stream\\?channel.*\\)" path)
		(match-string 1 path))
	 (t path)))

(defun my-org-yt-export (path desc format _)
	"Export time link or embed."
	(pcase format
		((or 'html '11ty 'md)
		 (cond
			(desc (format "<a href=\"%s\">%s</a>" path (or desc path)))
			(t
			 (let* ((path-and-query (url-path-and-query (url-generic-parse-url path)))
							(url (car path-and-query))
							(params (url-parse-query-string (cdr path-and-query)))
							(id (cond
									 ((string-match "\\(?:v=\\|tu\\.be/\\|live/\\)\\([^&]+\\)" path)
										(match-string 1 path))
									 ((string-match "\\(live_stream\\?channel.*\\)" path)
										(match-string 1 path))
									 (t path)))
							(width (or (car (assoc-default "width" params 'string=)) "456"))
							(height (or (car (assoc-default "height" params 'string=)) "315"))
							(time (assoc-default "t" params 'string=)))
				 (if time
						 (format "<a href=\"%s\">%s</a>" path (or desc path))
					 (format "<div class=\"yt-video\"><iframe width=\"%s\" height=\"%s\" title=\"YouTube video player\" src=\"https://www.youtube-nocookie.com/embed/%s?enablejsapi=1\" frameborder=\"0\" allowfullscreen>%s</iframe><a href=\"%s\">Watch on YouTube</a></div>"
									 width height id desc path))))))
		('ascii
		 desc)))

(defun my-org-yt-convert-time (time)
	(let ((split-time (reverse (split-string time ":"))))
		(format "%sh%sm%ss"
						(or (elt split-time 2) "0")
						(or (elt split-time 1) "0")
						(or (elt split-time 0) "0"))))
(ert-deftest my-org-yt-convert-time ()
	(should
	 (string=
		(my-org-yt-convert-time "1:02")
		"0h1m02s")))

(defun my-org-yt-complete ()
	"Prompt for a timestamp and link to a video."
	(interactive)
	(let* ((url (read-string "URL: " (when (derived-mode-p 'org-mode)
																		 (org-entry-get (point) "YOUTUBE"))))
				 (time (read-string "Time: "))
				 (split-time (reverse (split-string time ":"))))
		(concat "yt:"
						url
						(if (string= time "")
								""
							(concat
							 (if (string-match "\\?" url) "&t=" "?t=")
							 (format "%sh%sm%ss"
											 (or (elt split-time 2) "0")
											 (or (elt split-time 1) "0")
											 (or (elt split-time 0) "0")))))))

(defun my-org-yt-insert-description (link &optional description)
	(unless description
		(when (string-match "t=\\([0-9hms]+\\)" link)
			(let ((split-time (cdr (reverse (split-string (match-string 1 link) "[hms]")))))
				(concat
				 (if (and (elt split-time 2) (not (string= (elt split-time 2) "0")))
						 (concat (elt split-time 2) ":")
					 "")
				 (if (elt split-time 1)
						 (concat (if (and (and (elt split-time 2) (not (string= (elt split-time 2) "0")))
															(< (length (elt split-time 1)) 2))
												 "0" "")
										 (elt split-time 1) ":")
					 "")
				 (concat (if (and (elt split-time 1) (< (length (elt split-time 0)) 2)) "0" "")
								 (elt split-time 0)))))))
(ert-deftest my-org-yt-insert-description ()
	(should
	 (string=
		(my-org-yt-insert-description "yt:somevideo?t=0h1m2s")
		"1:02"))
	(should
	 (string=
		(my-org-yt-insert-description "yt:somevideo?t=1h2m3s")
		"1:02:03")))

(defun my-org-yt-open (path)
	(browse-url path))
(org-link-set-parameters "yt" :complete #'my-org-yt-complete
												 :insert-description #'my-org-yt-insert-description
												 :export #'my-org-yt-export
												 :follow #'my-org-yt-open)

(defun my-org-copy-region-as-plain-text (beg end)
	"Copy as plain text, removing links."
	(interactive "r")
	(save-restriction
		(narrow-to-region beg end)
		(kill-new (org-export-as 'ascii nil nil t))))
;; org-yt-link ends here

;; Videos
;; :PROPERTIES:
;; :CUSTOM_ID: videos
;; :END:

;; #+NAME: org-video-link

;; [[file:Sacha.org::org-video-link][org-video-link]]
(org-link-set-parameters
 "video"
 :export #'my-org-video-export
 :follow #'my-org-video-follow
 :complete #'my-org-video-complete)
(defun my-org-video-follow (path _)
	(cond
	 ((string-match "\\(https://.+\\):\\([0-9:]+\\)" path)
		(mpv-start (concat (match-string 1 path) "?t=" (my-org-yt-convert-time (match-string 2 path)))))
	 ((string-match "https:" path)
		(mpv-start path))
	 ((string-match "\\(.+?\\):\\([0-9:]+\\)" path)
		(mpv-start (expand-file-name (match-string 1 path))
							 (concat "--start=+" (match-string 2 path))))
	 (t (mpv-play (expand-file-name (replace-regexp-in-string "\\?.*" "" path))))))

(defun my-org-video-replace-with-permalink ()
	(interactive)
	(let* ((elem (org-element-context))
				 (path (org-element-property :path elem))
				 (description (org-element-property :description elem))
				 (permalink (org-entry-get (point) "EXPORT_ELEVENTY_PERMALINK" t)))
		(delete-region (org-element-begin elem) (org-element-end elem))
		(insert (org-link-make-string (concat "video:https://sachachua.com" permalink (file-name-nondirectory path))
																	description))))

(defun my-org-video-export (link desc format info)
	"Export PATH to FORMAT using the specified wrap parameter."
	(if desc
			(org-export-string-as (org-link-make-string link desc) format)
		(pcase format
			((or 'html '11ty 'md)
			 (let* ((path-and-query (url-path-and-query (url-generic-parse-url link)))
							(params (and (cdr path-and-query) (url-parse-query-string (cdr path-and-query))))
							body)
				 (setq body
							 (format
								"<video%s%s src=\"%s\" type=\"%s\">%s<a href=\"%s\">Download the video</a>%s</video>"
								(if (string= (or (car (assoc-default "controls" params 'string= '("1"))) "1") "0")
										""
									" controls=\"1\"")
								(if (string= (or (car (assoc-default "autoplay" params 'string= '("0"))) "0") "0")
										""
									" autoplay=\"1\"")
								(car path-and-query)
								(mailcap-file-name-to-mime-type (car path-and-query))
								(if (assoc-default "captions" params)
										(format "<track kind=\"subtitles\" label=\"Captions\" src=\"%s\" srclang=\"en\" default></track>"
														(expand-file-name (car (assoc-default "captions" params))))
									"")
								(car path-and-query)
								(if (assoc-default "captions-below" params)
										"<div class=\"captions\" style=\"display: none\"></div>"
									"")
								))
				 (when (assoc-default "caption" params)
					 (setq body (format "<figure>%s<figcaption><div>%s</div></figcaption></figure>"
															body
															(car (assoc-default "caption" params)))))
				 body))
			(_ link))))

(defun my-org-video-complete ()
	"Complete video reference."
	(interactive)
	(concat "video:" (read-file-name "File: ")))
;; org-video-link ends here

;; Audio
;; :PROPERTIES:
;; :CUSTOM_ID: audio
;; :END:

;; #+NAME: org-audio-link

;; [[file:Sacha.org::org-audio-link][org-audio-link]]
(org-link-set-parameters
 "audio"
 :export #'my-org-audio-export
 :complete #'my-org-audio-complete)

(defun my-org-audio-replace-with-permalink ()
	(interactive)
	(let* ((elem (org-element-context))
				 (path (org-element-property :path elem))
				 (description (org-element-property :description elem))
				 (permalink (org-entry-get (point) "EXPORT_ELEVENTY_PERMALINK" t)))
		(delete-region (org-element-begin elem) (org-element-end elem))
		(insert (org-link-make-string (concat "audio:https://sachachua.com" permalink (file-name-nondirectory path))
																	description))))

(defun my-org-audio-export (link desc format _)
	"Export PATH to FORMAT using the specified wrap parameter."
	(if desc
			(org-export-string-as (org-link-make-string link desc) format)
		(pcase format
			((or 'html '11ty 'md)
			 (let* ((path-and-query (url-path-and-query (url-generic-parse-url link)))
							(params (and (cdr path-and-query) (url-parse-query-string (cdr path-and-query))))
							(element (or (assoc-default "element" params #'string=) "audio")))
				 (format
					"<div class=\"audio\"><%s%s%s%s preload=\"metadata\" src=\"%s\" type=\"%s\"><a href=\"%s\">Download the audio</a>%s</%s>%s</div>"
					element
					(if (string= (or (assoc-default "controls" params 'string= "1") "1") "0")
							""
						" controls=\"1\"")
					(if (string= (or (assoc-default "autoplay" params 'string= "0") "0") "0")
							""
						" autoplay=\"1\"")
					(if (assoc-default "id" params)
							(format " id=\"%s\"" (car (assoc-default "id" params)))
						"")
					(car path-and-query)
					(mailcap-file-name-to-mime-type (car path-and-query))
					(car path-and-query)
					(if (assoc-default "captions" params)
							(format "<track kind=\"captions\" label=\"Captions\" src=\"%s\" srclang=\"en\" default></track>"
											(car (assoc-default "captions" params)))
						"")
					element
					(if (and (assoc-default "captions" params)
									 (assoc-default "id" params))
							(format  "<div class=\"captions\" style=\"display: none\"></div>"
											 (car (assoc-default "id" params)))
						"")
					)))
			(_ path))))
(ert-deftest my-org-audio-export ()
 (should
	(string-match
	 "<audio controls=\"1\" id=\"play-this\" src=\"test.opus\" type=\"audio/ogg\"><a href=\"test.opus\">Download the audio</a><track kind=\"subtitles\" label=\"Captions\" src=\"test.vtt\" srclang=\"en\" default></track></audio>"
	 (my-org-audio-export
		"test.opus?id=play-this&captions=test.vtt"
		nil
		'html
		nil
		)
	 )))
(defun my-org-audio-complete ()
	"Complete audio reference."
	(interactive)
	(concat "audio:" (read-file-name "File: ")))
;; org-audio-link ends here

;; Captions
;; :PROPERTIES:
;; :CUSTOM_ID: org-captions
;; :END:

;; #+NAME: org-captions-link

;; [[file:Sacha.org::org-captions-link][org-captions-link]]
(org-link-set-parameters
 "captions"
 :export #'my-org-captions-export
 :complete #'my-org-captions-complete)

(defun my-org-captions-format (file &optional separator)
	(let ((cues (subed-parse-file file)))
		(mapconcat (lambda (cue)
								 (format "<span class=\"audio-time\" data-start=\"%f\" data-stop=\"%f\">%s</span>"
												 (/ (elt cue 1) 1000.0)
												 (/ (elt cue 2) 1000.0)
												 (elt cue 3)))
							 cues (or separator " "))))

(defun my-org-captions-export (link desc format _)
	"Export PATH to FORMAT using the specified wrap parameter."
	(if desc
			(org-export-string-as (org-link-make-string link desc) format)
		(pcase format
			((or 'html '11ty 'md) (my-org-captions-format (car (url-path-and-query (url-generic-parse-url link)))))
			(_ path))))

(defun my-org-captions-complete ()
	"Complete audio reference."
	(interactive)
	(concat "captions:" (read-ffile-name "Captions: ")))

(defun my-org-captions-insert-as-html-block (file)
	(interactive "FFile: ")
	(insert "#+begin_export html\n" (my-org-captions-format file "\n") "\n#+end_export html\n"))
;; org-captions-link ends here

;; Using an Emacs Lisp macro to define quick custom Org Mode links to project files; plus URLs and search :org:emacs:coding:embark:
;; :PROPERTIES:
;; :CUSTOM_ID: git-projects
;; :EXPORT_DATE: 2024-01-07T08:07:09-0500
;; :EXPORT_ELEVENTY_PERMALINK: /blog/2024/01/using-an-emacs-lisp-macro-to-define-quick-custom-org-mode-links-to-project-files/
;; :EXPORT_ELEVENTY_FILE_NAME: blog/2024/01/using-an-emacs-lisp-macro-to-define-quick-custom-org-mode-links-to-project-files/
;; :EXPORT_ELEVENTY_WORDS: 885
;; :END:

;; #+begin_update
;; - [2024-09-19 Thu]: Added function for replacing current link, bound to ~C-. r~ (~my-embark-replace-link-with-exported-url~)
;; - [2024-01-12 Fri] Added embark action to copy the exported link URL.
;; - [2024-01-11 Thu] Switched to using Github links since Codeberg's down.
;; - [2024-01-11 Thu] Updated my-copy-link to just return the link if called from Emacs Lisp. Fix getting the properties.
;; - [2024-01-08 Mon] Add tip from Omar about ~embark-around-action-hooks~
;; - [2024-01-08 Mon] Simplify code by using ~consult--grep-position~
;; #+end_update

;; #+begin_summary
;; Summary (882 words): Emacs macros make it easy to define sets of related functions for custom Org links. This makes it easier to link to projects and export or copy the links to the files in the web-based repos. You can also use that information to consult-ripgrep across lots of projects.
;; #+end_summary

;; I'd like to get better at writing notes while coding and at turning
;; those notes into blog posts and videos. I want to be able to link to
;; files in projects easily with the ability to complete, follow, and
;; export links. For example, ~[[subed:subed.el]]~ should become
;; [[subed:subed.el]], which opens the file if I'm in Emacs and exports a
;; link if I'm publishing a post. I've been making custom link types
;; using ~org-link-set-parameters~. I think it's time to make a macro
;; that defines that set of functions for me. Emacs Lisp macros are a
;; great way to write code to write code.

;; #+NAME: org-project-link

;; [[file:Sacha.org::org-project-link][org-project-link]]
(defvar my-project-web-base-list nil "Local path . web repo URLs for easy linking.")

(defmacro my-org-project-link (type file-path git-url)
  `(progn
		 (defun ,(intern (format "my-org-%s-complete" type)) ()
			 ,(format "Complete a file from %s." type)
			 (concat ,type ":" (completing-read "File: "
																					(projectile-project-files ,file-path))))
		 (defun ,(intern (format "my-org-%s-follow" type)) (link _)
			 ,(format "Open a file from %s." type)
			 (find-file
				(expand-file-name
				 link
				 ,file-path)))
		 (defun ,(intern (format "my-org-%s-export" type)) (link desc format _)
			 "Export link to file."
			 (setq desc (or desc link))
			 (when ,git-url
				 (setq link (concat ,git-url (replace-regexp-in-string "^/" "" link))))
			 (pcase format
				 ((or 'html '11ty) (format "<a href=\"%s\">%s</a>"
																	 link
																	 (or desc link)))
				 ('md (if desc (format "[%s](%s)" desc link)
								(format "<%s>" link)))
				 ('latex (format "\\href{%s}{%s}" link desc))
				 ('texinfo (format "@uref{%s,%s}" link desc))
				 ('ascii (format "%s (%s)" desc link))
				 (_ (format "%s (%s)" desc link))))
		 (with-eval-after-load 'org
			 (org-link-set-parameters
				,type
				:complete (quote ,(intern (format "my-org-%s-complete" type)))
				:export (quote ,(intern (format "my-org-%s-export" type)))
				:follow (quote ,(intern (format "my-org-%s-follow" type))))
			 (cl-pushnew (cons (expand-file-name ,file-path) ,git-url)
									 my-project-web-base-list
									 :test 'equal))))
;; org-project-link ends here



;; Then I can define projects this way:


;; [[file:Sacha.org::#git-projects][Using an Emacs Lisp macro to define quick custom Org Mode links to project files; plus URLs and search:2]]
(my-org-project-link "subed"
										 "~/proj/subed/subed/"
										 "https://github.com/sachac/subed/blob/main/subed/"
										 ;; "https://codeberg.org/sachac/subed/src/branch/main/subed/"
										 )
(my-org-project-link "emacsconf-el"
										 "~/proj/emacsconf/lisp/"
										 "https://git.emacsconf.org/emacsconf-el/tree/")
(my-org-project-link "subed-record"
										 "~/proj/subed-record/"
										 "https://github.com/sachac/subed-record/blob/main/"
										 ;; "https://codeberg.org/sachac/subed-record/src/branch/main/"
										 )
(my-org-project-link "compile-media"
										 "~/proj/compile-media/"
										 "https://github.com/sachac/compile-media/blob/main/"
										 ;; "https://codeberg.org/sachac/compile-media/src/branch/main/"
										 )
(my-org-project-link "ox-11ty"
										 "~/proj/ox-11ty/"
										 "https://github.com/sachac/ox-11ty/blob/master/")
;; Using an Emacs Lisp macro to define quick custom Org Mode links to project files; plus URLs and search:2 ends here



;; #+RESULTS:
;; :results:
;; ((/home/sacha/proj/ox-11ty/ . https://github.com/sachac/ox-11ty/blob/master/) (/home/sacha/proj/compile-media/ . https://codeberg.org/sachac/compile-media/src/branch/main/) (/home/sacha/proj/subed-record/ . https://codeberg.org/sachac/subed-record/src/branch/main/) (/home/sacha/proj/emacsconf/lisp/ . https://git.emacsconf.org/emacsconf-el/tree/) (/home/sacha/proj/subed/subed/ . https://codeberg.org/sachac/subed/src/branch/main/subed/))
;; :end:

;; And I can complete them with the usual ~C-c C-l~ (~org-insert-link~) process:

;; #+BEGIN_COMMENT
;; Demonstrate completion to subed-vtt
;; #+END_COMMENT

;; #+CAPTION: Completing a custom link with ~org-insert-link~
;; [[file:images/completing-custom-links.gif]]

;; Sketches are handled by [[dotemacs:org-mode-sketch-links][my Org Mode sketch links]], but we can add them anyway.


;; [[file:Sacha.org::#git-projects][Using an Emacs Lisp macro to define quick custom Org Mode links to project files; plus URLs and search:3]]
(cl-pushnew (cons (expand-file-name "~/sync/sketches/") "https://sketches.sachachua.com/filename/")
						my-project-web-base-list
						:test 'equal)
;; Using an Emacs Lisp macro to define quick custom Org Mode links to project files; plus URLs and search:3 ends here

;; Copy web link
;; :PROPERTIES:
;; :CUSTOM_ID: web-link
;; :EXPORT_MODIFIED: 2024-01-20T07:44:20-0500
;; :END:

;; #+begin_update
;; - [2024-01-20]: Fix Wayback link handling.
;; - [2024-01-19]: Add Wayback machine.
;; #+end_update

;; Keeping a list of projects and their web versions also makes it easier
;; for me to get the URL for something. I try to post as much as possible
;; on the Web so that it's easier for me to find things again and so that
;; other people can pick up ideas from my notes. Things are a bit
;; scattered: [[https://sachachua.com][my blog]], repositories on [[https://github.com/sachac/][Github]] and [[https://codeberg.org/sachac/][Codeberg]], [[https://sketches.sachachua.com][my
;; sketches]]... I don't want to think about /where/ the code has ended
;; up, I just want to grab the URL. If I'm going to put the link into an
;; Org Mode document, that's super easy. I just take advantage of the
;; things I've added to ~org-store-link~. If I'm going to put it into an
;; e-mail or a toot or wherever else, I just want the bare URL.

;; I can think of two ways to approach this. One is a command that copies
;; just the URL by figuring it out from the buffer filename, which allows
;; me to special-case a bunch of things:


;; [[file:Sacha.org::#web-link][Copy web link:1]]
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
;; Copy web link:1 ends here



;; Another approach is to hitch a ride on the Org Mode link storage and
;; export functions and just grab the URL from whatever link I've stored
;; with ~org-store-link~, which I've bound to ~C-c l~. I almost always
;; have an HTML version of the exported link. We can even use XML parsing
;; instead of regular expressions.


;; [[file:Sacha.org::#web-link][Copy web link:2]]
(defun my-org-link-as-url (link)
	"Return the final URL for LINK."
	(dom-attr
	 (dom-by-tag
		(with-temp-buffer
			(insert (org-export-string-as link 'html t))
			(xml-parse-region (point-min) (point-max)))
		'a)
	 'href))

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

(defun my-embark-replace-link-with-exported-url (link &rest _)
	(interactive (list (org-element-property :raw-link (org-element-context))))
	(my-insert-or-replace-link (my-org-link-as-url link)))

(with-eval-after-load 'embark-org
	(mapc (lambda (map)
					(keymap-set map "u" #'my-embark-org-copy-exported-url)
					(keymap-set map "U" #'my-embark-org-copy-exported-url-as-wayback)
					(keymap-set map "r" #'my-embark-replace-link-with-exported-url))
				(list embark-url-map embark-org-link-map embark-org-link-copy-map)))
;; Copy web link:2 ends here

;; Quickly search my code

;; Since ~my-project-web-base-list~ is a list of projects I often think
;; about or write about, I can also make something that searches through
;; them. That way, I don't have to care about where my code is.


;; [[file:Sacha.org::*Quickly search my code][Quickly search my code:1]]
(defun my-consult-ripgrep-code ()
  (interactive)
	(consult-ripgrep (mapcar 'car my-project-web-base-list)))
;; Quickly search my code:1 ends here



;; I can add ~.rgignore~ files in directories to tell ripgrep to ignore
;; things like ~node_modules~ or ~*.json~.

;; I also want to search my Emacs configuration at the same time,
;; although links to my config are handled by [[dotemacs:links-to-my-config][my dotemacs link type]] so
;; I'll leave the URL as nil. This is also the way I can handle other
;; unpublished directories.


;; [[file:Sacha.org::*Quickly search my code][Quickly search my code:2]]
(cl-pushnew (cons (expand-file-name "~/sync/emacs/Sacha.org") nil)
						my-project-web-base-list
						:test 'equal)
(cl-pushnew (cons (expand-file-name "~/proj/static-blog/_includes") nil)
						my-project-web-base-list
						:test 'equal)
(cl-pushnew (cons (expand-file-name "~/bin") nil)
						my-project-web-base-list
						:test 'equal)
;; Quickly search my code:2 ends here



;; Actually, let's throw my blog posts and Org files in there as well,
;; since I often have code snippets. If it gets to be too much, I can
;; always have different commands search different things.


;; [[file:Sacha.org::*Quickly search my code][Quickly search my code:3]]
(cl-pushnew (cons (expand-file-name "~/proj/static-blog/blog/") "https://sachachua.com/blog/")
						my-project-web-base-list
						:test 'equal)
(cl-pushnew (cons (expand-file-name "~/sync/orgzly") nil)
						my-project-web-base-list
						:test 'equal)
;; Quickly search my code:3 ends here



;; #+BEGIN_COMMENT
;; Demonstrate [[elisp:my-consult-ripgrep-code]] for defun file duration
;; #+END_COMMENT

;; #+CAPTION: Using my-consult-ripgrep-code
;; [[file:images/ripgrep-code.gif]]

;; I don't have anything bound to ~M-s c~ (code) yet, so let's try that.


;; [[file:Sacha.org::*Quickly search my code][Quickly search my code:4]]
(keymap-global-set "M-s c" #'my-consult-ripgrep-code)
;; Quickly search my code:4 ends here

;; Tip from Omar: embark-around-action-hooks

;; [2024-01-07 Sun] I modified oantolin's suggestion from the comments to work with ~consult-ripgrep~, since ~consult-ripgrep~ gives me ~consult-grep~ targets instead of ~consult-location~:


;; [[file:Sacha.org::*Tip from Omar: embark-around-action-hooks][Tip from Omar: embark-around-action-hooks:1]]
(cl-defun embark-consult--at-location (&rest args &key target type run &allow-other-keys)
	"RUN action at the target location."
	(save-window-excursion
		(save-excursion
			(save-restriction
				(pcase type
					('consult-location (consult--jump (consult--get-location target)))
					('org-heading (org-goto-marker-or-bmk (get-text-property 0 'org-marker target)))
					('consult-grep (consult--jump (consult--grep-position target)))
					('file (find-file target)))
				(apply run args)))))

(cl-pushnew #'embark-consult--at-location (alist-get 'org-store-link embark-around-action-hooks))
;; Tip from Omar: embark-around-action-hooks:1 ends here

;; Linking to headings that match a tag

;; [[file:Sacha.org::*Linking to headings that match a tag][Linking to headings that match a tag:1]]
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
;; Linking to headings that match a tag:1 ends here

;; Links from org-protocol
;; :PROPERTIES:
;; :CUSTOM_ID: links-from-org-protocol
;; :END:

;; So that I can easily add links at point. Formatted as an Org list for now.


;; [[file:Sacha.org::#links-from-org-protocol][Links from org-protocol:1]]
(defun my-org-protocol-insert-link (info)
  "Store and insert the link at point based on INFO."
  (org-protocol-store-link info)
  (with-current-buffer (window-buffer (selected-window))
    (insert "- ")
    (org-insert-last-stored-link 1)
    (insert "\n")))
(eval-after-load 'org-protocol
  '(add-to-list 'org-protocol-protocol-alist
                '("insert-link" :protocol "insert-link" :function my-org-protocol-insert-link)))

;; javascript:location.href = 'org-protocol://copy-thumbnail?thumbnail=' + encodeURIComponent(document.querySelector('meta[property=\"og:image\"]') ? document.querySelector('meta[property=\"og:image\"]').getAttribute('content') : '') + '&title=' + encodeURIComponent(document.title) + '&url=' + encodeURIComponent(location.href) + '&videoId=' + ((typeof(videoId) !== 'undefined' ? videoId : (document.querySelector('meta[itemprop=\"videoId\"]') ? document.querySelector('meta[itemprop=\"videoId\"]').getAttribute('content') : '')) || '')

(defun my-get-youtube-info (url)
  (with-current-buffer (url-retrieve-synchronously url)
    (goto-char (point-min))
    (prog1
        (list
         :url
         url
         :title
         (when (re-search-forward "<title>\\(.*?\\)</title>" nil t)
           (match-string 1))
         :duration
         (when (re-search-forward "approxDurationMs\":\"\\([0-9]+\\)\"" nil t)
           (format-seconds "%.2h:%.2m:%.2s%z" (/ (string-to-number (match-string 1)) 1000))))
      (kill-buffer))))

(defun my-link-video (list)
  (when (stringp list) (setq list (list :url list)))
  (with-current-buffer (url-retrieve-synchronously (concat "https://video.link/bookmarklet?url=" (url-encode-url (plist-get list :url))))
    (save-excursion
      (if (re-search-forward "<input type=\"text\" id=\"safeURL\" readonly=\"readonly\" value=\"\\(.*?\\)\"" nil t)
          (plist-put list :url (match-string-no-properties 1))
        (plist-put list :url (replace-regexp-in-string "watch" "watch_popup" (plist-get list :url)))))
    (when (string= (or (plist-get list :thumbnail) "") "")
      (save-excursion
        (when (re-search-forward "<img id=\"videoThumb\" src=\"\\(.*?\\)\"" nil t)
          (plist-put list :thumbnail (match-string-no-properties 1)))))
    list))

(defun my-org-protocol-copy-thumbnail (info)
  "Store and insert the link at point based on INFO."
  (interactive "MURL: ")
  (when (stringp info) (setq info (list :url info)))
  (when (string-match "youtube\\.com" (plist-get info :url))
    (setq info (my-link-video info)))
  (let ((date (format-time-string "%Y-%m-%d")))
    (kill-new
     (if (string= (plist-get info :videoId) "")
         (format "{{<thumbnail image=\"%s\" title=\"%s\" link=\"%s\" date=\"%s\">}}\n"
                 (plist-get info :thumbnail)
                 (plist-get info :title)
                 (plist-get info :url)
                 date
                 )
       (format "{{<youtube id=\"%s\" title=\"%s\" link=\"%s\" date=\"%s\">}}\n"
               (plist-get info :videoId)
               (plist-get info :title)
               (plist-get info :url)
               date))))
  nil)
(eval-after-load 'org-protocol
  '(add-to-list 'org-protocol-protocol-alist
                '("copy-thumbnail" :protocol "copy-thumbnail" :function my-org-protocol-copy-thumbnail)))
;; Links from org-protocol:1 ends here

;; Fix elisp links
;; :PROPERTIES:
;; :CUSTOM_ID: fix-elisp-links
;; :END:

;; #+NAME: org-elisp-link

;; [[file:Sacha.org::org-elisp-link][org-elisp-link]]
(defun my-org-elisp-link-export (link description format &optional arg)
  (pcase format
   ('html (format "<span title=\"%s\">%s</span>" (replace-regexp-in-string "\"" "&quot;" link) description))
   ((or 'icalendar 'ascii) description)
   ))
(org-link-set-parameters
 "elisp"
 :export 'my-org-elisp-link-export)
;; org-elisp-link ends here

;; IRC
;; :PROPERTIES:
;; :CUSTOM_ID: irc
;; :END:

;; #+NAME: org-irc-link

;; [[file:Sacha.org::org-irc-link][org-irc-link]]
(org-link-set-parameters
 "ircs"
 :export
 (lambda (link description format)
   "Export an ircs link.
See `org-link-parameters' for details about LINK, DESCRIPTION and
FORMAT."
   (let ((desc (or description link)))
     (pcase format
       (`html (format "<a href=\"ircs:%s\">%s</a>" link desc))
       (`md (format "[%s](ircs:%s)" desc link))
       (_ nil)))))
;; org-irc-link ends here

;; Dired
;; :PROPERTIES:
;; :CUSTOM_ID: org-dired
;; :END:


;; [[file:Sacha.org::#org-dired][Dired:1]]
(setq dired-dwim-target t)

(defun my-org-get-links-in-region (beg end)
  (save-excursion
    (let (results)
      (goto-char (min beg end))
      (while (re-search-forward org-any-link-re (max beg end) t)
        (add-to-list 'results (org-element-context)))
      results)))

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
;; Dired:1 ends here

;; Org protocol: following Org links from outside Emacs          :org:emacs:
;; :PROPERTIES:
;; :CUSTOM_ID: org-protocol-open
;; :EXPORT_DATE: 2023-09-26T09:42:45-0400
;; :EXPORT_ELEVENTY_PERMALINK: /blog/2023/09/org-protocol-following-org-links-from-outside-emacs/
;; :EXPORT_ELEVENTY_FILE_NAME: blog/2023/09/org-protocol-following-org-links-from-outside-emacs/
;; :END:

;; =_xor= had an interesting idea: can we use =org-protocol= to link to
;; things inside Emacs, so that we can have a webpage with bookmarks into
;; our Org files? Here's a quick hack that reuses =org-store-link= and
;; =org-link-open=.

;; #+NAME: org-protocol-link

;; [[file:Sacha.org::org-protocol-link][org-protocol-link]]
(defun org-protocol-open-link (info)
	"Process an org-protocol://open style url with INFO."
	(org-link-open (car (org-element-parse-secondary-string (plist-get info :link) '(link)))))

(defun org-protocol-copy-open-link (arg)
	(interactive "P")
	(kill-new (concat "org-protocol://open?link=" (url-hexify-string (org-store-link arg)))))

(with-eval-after-load 'org-protocol
	(add-to-list 'org-protocol-protocol-alist
							 '("org-open" :protocol "open" :function org-protocol-open-link)))
;; org-protocol-link ends here



;; To make exporting and following easier, we also need a little code to
;; handle =org-protocol= links inside Org.


;; [[file:Sacha.org::#org-protocol-open][Org protocol: following Org links from outside Emacs:2]]
(defun org-protocol-follow (path &rest _)
	"Follow the org-protocol link for PATH."
	(org-protocol-check-filename-for-protocol (concat "org-protocol:" path) nil nil))

(defun org-protocol-export (path desc format info)
	"Export an org-protocol link."
	(setq path (concat "org-protocol:" path))
	(setq desc (or desc path))
	(pcase format
    (`html (format "<a href=\"%s\">%s</a>" path desc))
		(`11ty (format "<a href=\"%s\">%s</a>" path desc))
    (`latex (org-latex-link path desc info))
    (`ascii (org-ascii-link path desc info))
		(`md (org-md-link path desc info))
    (_ path)))

(with-eval-after-load 'org
	(org-link-set-parameters "org-protocol"
													 :follow #'org-protocol-follow
													 :export #'org-protocol-export))
;; Org protocol: following Org links from outside Emacs:2 ends here

;; TODO Speed command for adding a custom ID to Org Mode posts
;;     :PROPERTIES:
;;     :CREATED:  [2021-04-13 Tue 19:28]
;;     :CUSTOM_ID: add-custom-id
;;     :ID:       o2b:e80c2eef-fed6-4658-8172-2d8d7cdd2588
;;     :BLOG:     sacha
;;     :POSTID:   29719
;;     :END:

;;   Nudged by [[https://amitp.blogspot.com/2021/04/automatically-generate-ids-for-emacs.html][Amit's post about adding custom IDs to Org headings]], I
;;   decided to write a speed command to add a custom ID with a reasonable
;;   default, and to make it happen whenever I post something from my Emacs
;;   config (like this one). I'm running out of brainspace for speed
;;   commands, so I'm going to try sticking it into a hydra so that I can
;;   add future things to the hydra instead. I'll probably figure out some
;;   kind of [[https://sachachua.com/blog/2021/04/emacs-making-a-hydra-cheatsheet-for-lispy/][cheat sheet thing]] for speed commands too.


;; [[file:Sacha.org::#add-custom-id][Speed command for adding a custom ID to Org Mode posts:1]]
(defun my-make-slug (s)
  (thread-last s
    (downcase)
    (replace-regexp-in-string "[^a-z0-9]+" "-")
    (replace-regexp-in-string "^-\\|-$" "")))
(defun my-org-set-custom-id (id)
  "Set the CUSTOM_ID property to ID at point."
  (interactive (list
                (let ((default-custom-id (my-make-slug (string-join (org-get-outline-path t) " "))))
                  (read-string (format "ID (%s): " default-custom-id) nil nil default-custom-id))))
  (org-entry-put (point) "CUSTOM_ID" id))

(with-eval-after-load 'hydra
  (define-key hydra-base-map (kbd "<down>") 'my-hydra-pop)
  (define-key hydra-base-map (kbd "<up>") (lambda () (interactive) (my-hydra-go-and-push 'my-shortcuts/body)))


  (defhydra my-hydra/org-speed-commands ()
    ("i" my-org-set-custom-id "CUSTOM_ID" :exit t)
    ("<up>" my-hydra/org-mode/body :exit t)
    ("u" (my-hydra-go-and-push 'my-hydra/org-mode/body) :exit t :hint nil))
  (defhydra my-hydra/org-mode (:foreign-keys run)
    ("b" my-org-back-to-heading "Heading")
    ("n" org-forward-heading-same-level "Next")
    ("p" org-backward-heading-same-level "Previous")
    ("a" org-archive-subtree-default "Archive")
    ("j" my-org-mark-done-and-add-to-journal "Journal" :exit t)
    ("k" org-cut-subtree "Kill")
    ("<up>" (my-hydra-go-and-push 'my-shortcuts/body) :exit t hint nil)
    ("u" (my-hydra-go-and-push 'my-shortcuts/body) :exit t :hint nil)
    ("<f14>" nil "Exit" :exit t))
  (defhydra my-hydra/org-link ()
    ("RET" org-open-at-point "Open")
    ("e" org-insert-link "Edit")
    ("c" my-caption-show "Captions")
    ("w" my-org-link-element-copy-link "Copy link")
    ("u" (my-hydra-go-and-push 'my-hydra/org-mode/body) :exit t :hint nil)
    ("<up>" (my-hydra-go-and-push 'my-hydra/org-mode/body) :exit t :hint nil))
  (defhydra my-hydra/org-src ()
    ("e" org-babel-execute-src-block "Exec")
    ("E" my-org-execute-src-block-by-name "Exec by name")
    ("i" org-edit-special "Edit")
    ("d" org-babel-demarcate-block "Demarcate")
    ("g" org-babel-goto-named-src-block "Goto")
    ("r" org-babel-open-src-block-result "Result")
    ("x" org-babel-expand-src-block "Expand")
    ("t" (org-babel-tangle '(4)) "Tangle at point")
    ("T" (org-babel-tangle '(16)) "Tangle target file")
    ("u" (my-hydra-go-and-push 'my-hydra/org-mode/body) :exit t :hint nil)
    ("<up>" (my-hydra-go-and-push 'my-hydra/org-mode/body) :exit t :hint nil)
    )
  (defun my-hydra/dwim ()
    (interactive)
    (if (derived-mode-p 'org-mode)
        (let ((context (org-element-context)))
          (cond
           ((and (bolp) (looking-at org-outline-regexp))
            (my-hydra/org-speed-commands/body))
           ((org-in-src-block-p) (my-hydra/org-src/body))
           ((eq (org-element-type context) 'link) (my-hydra/org-link/body))
           (t (my-hydra/org-mode/body))))
      (my-shortcuts/body)))
  (define-key org-mode-map (kbd "<f14>") 'my-hydra/dwim)
  (keymap-global-set  "<f14>" 'my-hydra/dwim))
;; Speed command for adding a custom ID to Org Mode posts:1 ends here

;; Journal
;; :PROPERTIES:
;; :CUSTOM_ID: journal
;; :END:


;; [[file:Sacha.org::#journal][Journal:1]]
(defvar my-journal-category-map
  '(("Gross" . "Gross motor")
    ("Fine" . "Fine motor")
    ("8 - Kaizen" . "Kaizen")
    ("9 - Us" . "Us")
    ("Self-care" . "Self-care and independence"))
  "Alist of string replacements for journal categories.")
(defvar my-journal-categories
  '("Kaizen" "Us" "Field trip" "Gross motor" "Fine motor"
    "Sensory" "Language" "Music" "Art"
    "Self-care and independence" "Eating" "Sleep" "Emotion"
    "Household" "Social" "Pretend" "Cognition" "World" "Other" "Oops" "Thoughts" "Consulting" "Track" "Uncategorized")
  "List of categories to display.
      Unknown categories will be added to the end.")

(defun my-journal-date (o) (elt o 3))
(defun my-journal-note (o) (car o))
(defun my-journal-week-highlight (o) (elt o 4))
(defun my-journal-category (o) (elt o 1))
(defun my-journal-pictures (o) (when (string> (elt o 2) "") (split-string (elt o 2) ",")))
(defun my-journal-id (o) (elt o 7))
(defun my-journal-status (o) (elt o 8))
(defun my-journal-other (o) (elt o 9))
(defun my-journal-zidstring (o) (elt o 11))
(defun my-org-group-journal-entries (filtered &optional category-map categories)
  (setq category-map (or category-map (my-journal-category-map)))
  (setq categories (or categories (my-journal-categories)))
  (let* ((grouped (-group-by 'my-journal-category filtered))
         (mapped-list
          (mapcar
           (lambda (o)
             (cons (or (assoc-default (car o) category-map) (car o))
                   (cdr o)))
           grouped))
         (sorted-list
          (delq nil
                (append
                 (mapcar (lambda (cat)
                           (when (assoc-default cat mapped-list)
                             (cons cat (assoc-default cat mapped-list))))
                         categories)
                 (-remove (lambda (o) (member (car o) categories)) mapped-list)))))
    sorted-list))

(defun my-org-date-to-string (date &optional base-date)
  "Return the Org date specified by DATE.
      This is relative to BASE-DATE if specified."
  (org-read-date nil nil date nil (when base-date (org-read-date nil t base-date))))

(ert-deftest my-org-date-to-string ()
  (should (string= (my-org-date-to-string "++1" "2018-08-01") "2018-08-02")))

(defun my-org-filter-journal-csv (filename &optional from to highlight base-date)
  "Return a list of matching entries."
  (setq from (and from (substring (my-org-date-to-string from base-date) 0 10))
        to (and to (substring (my-org-date-to-string to base-date) 0 10)))
  (let* ((data (pcsv-parse-file filename))
         (filtered
          (-filter
           (lambda (o)
             (let ((date (my-journal-date o)))
               (and (or (null from) (not (string< date from)))
                    (or (null to) (string< date to))
                    (and (not (string= (my-journal-status o) "Deleted")))
                    (not (string-match "^!" (my-journal-note o)))
                    (string-equal
                     "true"
                     (cond
                      ((null highlight) "true")
                      ((string-equal highlight "week") (my-journal-week-highlight o))
                      (t "true"))))))
           data)))
    filtered))

(defun my-journal-read-category (&optional initial)
  (consult--read my-journal-categories :sort nil :prompt "Category: " :initial initial))

(defun my-journal-guess-category ()
	(save-excursion
		(org-back-to-heading)
		(org-end-of-meta-data)
		(let ((text (buffer-substring-no-properties (point) (org-end-of-subtree))))
			(if (string-match "#gardening" text)
					"Household"))))

(defun my-journal-post (note &rest plist)
  (interactive (list (read-string "Note: ")
                     :Date (concat (org-read-date "Date: ") " 23:00")
                     :Category (my-journal-read-category (condition-case nil (my-journal-guess-category) (error nil)))
                     :Other (read-string "Other: ")))
  (setq plist (append `(:Note ,note) plist))
  (let ((url-request-method "POST")
        (url-request-extra-headers `(("Content-Type" . "application/json")
																		("Authorization" . ,(concat "Basic "
																																(base64-encode-string
																																 (concat my-journal-user ":" my-journal-password))))))
        (json-object-type 'plist)
        (url-request-data (encode-coding-string (json-encode-plist plist) 'utf-8))
        data)
    (with-current-buffer (url-retrieve-synchronously (concat my-journal-url "/api/entries"))
      (goto-char (point-min))
      (re-search-forward "^$")
      (setq data (json-read))
      (message "%s" (plist-get data :ZIDString))
      data)))

(defun my-journal-get-by-zidstring (zidstring)
  (my-journal-get (concat "api/entries/" zidstring)))

(defun my-journal-insert-ref (zidstring)
  (interactive (list (my-journal-completing-read)))
  (insert (org-link-make-string (concat "ref:" (my-journal-id-from-string zidstring)))))

(defun my-journal-edit (zidstring)
  (interactive (list (my-journal-completing-read)))
  (let* ((id (my-journal-id-from-string zidstring))
         (entry (and id (my-journal-get-by-zidstring id))))
    (if (null id)
        (my-journal-post zidstring
                         :Category (my-journal-read-category (plist-get entry :Category))
                         :Other (read-string "Other: " (plist-get entry :Other)))
      (plist-put entry :Note (read-string (format "Note (%s): " (plist-get entry :Note))))
      (plist-put entry :Category (my-journal-read-category (plist-get entry :Category)))
      (plist-put entry :Other (read-string "Other: " (plist-get entry :Other)))
      (my-journal-update entry))))

(defun my-journal-update (plist)
  "Update journal entry using PLIST."
  (let ((url-request-method "PUT")
        (url-request-data (json-encode-plist plist)))
    (my-json-request (concat my-journal-url "/api/entries/" (plist-get plist :ZIDString)))))
;; (my-journal-post "Hello, world")

(defun my-journal-get-entries (&optional from to search)
  "Return parsed CSV of entries limited by FROM, TO, and SEARCH."
  (with-current-buffer
      (url-retrieve-synchronously (format "%s/api/entries.csv?from=%s&to=%s&regex=1&q=%s"
                                          my-journal-url
                                          (or from "")
                                          (or to "")
                                          (or search "")))
    (goto-char (point-min))
    (delete-region (point-min) (search-forward "\n\n"))
    (cdr (pcsv-parse-buffer))))

(defun my-journal-get (url)
	(let ((url-request-extra-headers
				 `(("Authorization" . ,(concat "Basic "
																			 (base64-encode-string
																				(concat my-journal-user ":" my-journal-password)))))))
		(my-json-request (concat my-journal-url "/" url))))
(defun my-journal-get-entry (zid) (my-journal-get (format "api/entries/zid/%s" zid)))
;; Journal:1 ends here



;; The following code lets me complete journal entries and get their ZIDs.

;; #+NAME: helm-journal

;; [[file:Sacha.org::helm-journal][helm-journal]]
(defun my-json-request (url)
  (let ((json-object-type 'plist)
        (url-request-extra-headers (cons '("Content-Type" . "application/json") url-request-extra-headers)))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (re-search-forward "^$" nil t)
      (json-read))))

(defvar my-journal-search-cache nil "List of search results.")
(defun my-journal-search-query (query-str)
  (let* ((url-request-method "GET")
         (json-response (my-journal-get (format "api/entries?q=%s&limit=50&sort=date&regex=1"
                                                 query-str))))
    (setq my-journal-search-cache (mapcar (lambda (o)
              (cons
               (format "%s %s"
                       (plist-get o :ZIDString)
                       (plist-get o :Note))
               o))
            json-response))))

(defun my-journal-search-query-async (query-str next)
  (let* ((url-request-method "GET")
         (url-request-extra-headers (cons '("Content-Type" . "application/json") url-request-extra-headers)))
    (url-retrieve
     (format "%s/api/entries?q=%s&limit=50&sort=date&regex=1"
             my-journal-url
       query-str)
     (lambda (status)
       (goto-char (point-min))
       (re-search-forward "^$" nil t)
       (setq my-journal-search-cache
             (mapcar (lambda (o)
                       (cons
                        (format "%s %s"
                                (plist-get o :ZIDString)
                                (plist-get o :Note))
                        o))
                     (let ((json-object-type 'plist))
                       (json-read))))
       (funcall next 'flush)
       (if my-journal-search-cache (funcall next my-journal-search-cache))))))

(defun my-journal--async-search (next)
  (lambda (action)
    (cond
     ((eq action 'setup)                ;; Should figure out how to start
      (my-journal-search-query-async "" next))
     ((and (stringp action) (not (string= action "")))
      (my-journal-search-query-async action next))
     (t (funcall next action)))))

(defun my-journal-completing-read ()
  (interactive)
  (consult--read
   (thread-first (consult--async-sink)
     (consult--async-refresh-immediate)
     (my-journal--async-search)
     (consult--async-throttle)
     (consult--async-split))
   :sort nil
   :prompt "Entry: "
   :category 'journal))

(defun my-journal-id-from-string (s)
  (when (string-match "^[-0-9]+" s) (match-string 0 s)))

(defun my-journal-view (s)
  (interactive (list (my-journal-completing-read)))
  (my-org-journal-open (my-journal-id-from-string s)))

(defun my-journal-sketch-large (zid)
  "Create a large sketch based on ZID."
  (interactive (list (my-journal-completing-read)))
  (let ((filename (expand-file-name (format "%s.psd"
                                             (my-journal-id-from-string zid))
                                    my-sketch-inbox-directory)))
    (unless (file-exists-p filename)
      (copy-file my-sketch-large-template-file filename))
    (my-org-sketch-open filename)))
;; helm-journal ends here



;; I should probably figure out how to switch this over to my Consult-based workflow:


;; [[file:Sacha.org::#journal][Journal:3]]
(defun my-journal-format-entry (type o)
  (cond
   ((eq type 'org-link-zid-only)
    (org-link-make-string (format "journal:%s" (cdr (assoc 'ZIDString o)))))
   ((eq type 'list-item-with-zid)
    (format "- %s (%s)\n"
            (assoc-default 'Note o)
            (org-link-make-string
             (format "journal:%s" (assoc-default 'ZIDString o)))))
   ((eq type 'list-item)
    (format "- %s\n" (assoc-default 'Note o)))
   ((eq type 'text)
    (assoc-default 'Note o))))

(defun my-journal-format-entries (type list)
  (mapconcat
   (lambda (o) (my-journal-format-entry type o))
   (reverse list)
   (cond
    ((eq type 'org-link-zid-only) ", ")
    ((eq type 'list-item-with-zid) "")
    ((eq type 'list-item) "")
    ((eq type 'text) " "))))
;; Journal:3 ends here



;; This lets me define a custom link type.

;; #+NAME: org-journal-link

;; [[file:Sacha.org::org-journal-link][org-journal-link]]
(defun my-org-journal-open (id &optional arg)
  (browse-url (format "%s/zid/%s" my-journal-url id)))

(defun my-org-journal-export (link description format &optional arg)
  (let* ((path (concat "%s/zid/" my-journal-url link))
         (image (concat "%s/zid/" my-journal-url link))
         (desc (or description link)))
    (cond
     ((or (eq format 'html) (eq format 'wp))
      (if description
          (format "<a target=\"_blank\" href=\"%s\">%s</a>" path desc)
        (format "<a target=\"_blank\" href=\"%s\"><img src=\"%s\"><br />%s</a>" path image desc)))
     ((eq format 'latex) (format "\\href{%s}{%s}" path desc))
     ((eq format 'texinfo) (format "@uref{%s,%s}" path desc))
     ((eq format 'ascii) (format "%s <%s>" desc path))
     (t path))))

(defun my-org-journal-complete (&optional prefix)
  (cdr (assoc 'ZIDString (helm-comp-read "Entry: " 'my-helm-journal-search :volatile t))))

(use-package org
  :config
  (org-link-set-parameters
   "journal"
   :follow 'my-org-journal-open
   :export 'my-org-journal-export
   :complete 'my-org-journal-complete))
;; org-journal-link ends here

;; [[file:Sacha.org::#journal][Journal:5]]
(defun my-org-journal-summarize (from to &optional search category-map categories)
  (my-org-group-journal-entries (my-journal-get-entries from to search) category-map categories))

(defun my-org-journal-format-tree (groups &optional include)
  (mapconcat
   (lambda (o)
     (concat "- *" (car o) "*\n"
             (mapconcat
              (lambda (i)
                (concat "  - "
                        (if (member 'date include) (concat (my-journal-date i) " ") "")
                        (replace-regexp-in-string "\\\"" "\"" (my-journal-note i))
                        (if (member 'zid include) (concat " " (my-journal-zidstring i)) "")
                        ;; (if (string= "" (my-journal-category i))
                        ;;     ""
                        ;;   (format " (%s)" (my-journal-category i)))
                        "\n"))
              (reverse (cdr o)) "")))
   groups ""))

(defun my-org-summarize-journal-csv (from to &optional search category-map categories include)
  (interactive
   (list (org-read-date nil nil nil "From: ")
         (org-read-date nil nil nil "To: ")
         (read-string "Search: ")
         my-journal-category-map
         my-journal-categories
         nil))
  (let ((list (my-org-journal-format-tree
               (my-org-group-journal-entries
                (my-journal-get-entries from to search)
                category-map categories)
               include)))
    (if (called-interactively-p 'any) (insert list) list)))
;; Journal:5 ends here

;; [[file:Sacha.org::#journal][Journal:6]]
(defun my-read-journal-category ()
  (completing-read "Category: " my-journal-categories))

(defun my-update-journal-entry (old-text new-text category)
  (interactive (list (read-string "Old: ")
                     (read-string "New: ")
                     (my-read-journal-category)))
  (my-send-intent "com.sachachua.journal.categorize"
                  (list (cons "text" old-text)
                        (cons "newtext" (or new-text old-text))
                        (cons "category" (or category "Uncategorized")))))

(defun my-create-journal-entry (new-text category)
  (interactive (list (read-string "Text: ")
                     (my-read-journal-category)))
  (my-update-journal-entry new-text new-text category))

(defun my-export-journal-entries ()
  "Trigger task to export. Phone must be unlocked."
  (interactive)
  (my-send-intent "com.sachachua.journal.export" '(("a" . "b"))))

(use-package csv
  :commands csv--read-line)
(defun my-prompt-for-uncategorized-entries ()
  (interactive)
  (let ((key-list '("Note" "Date" "highlight week" "Category" "month" "Time" "Link" "ELECT"))
        x new-text category done)
    (while (and (not (eobp)) (not done))
      (forward-char 1)
      (setq x (csv--read-line key-list))
      (when (string= (assoc-default "Category" x nil "") "")
        (setq text (read-string "Text: " (assoc-default "Note" x nil "")))
        (setq category (completing-read "Category: " (cons "." my-journal-categories)))
        (if (string= category ".")
            (setq done t)
          (my-update-journal-entry (assoc-default "Note" x nil "") text category))))))
;; Journal:6 ends here

;; Working with journal entries
;; :PROPERTIES:
;; :CUSTOM_ID: working-with-journal-entries
;; :END:


;; [[file:Sacha.org::#working-with-journal-entries][Working with journal entries:1]]
(defun my-journal-insert-matching-entries (from to match)
  (interactive (list (org-read-date "From: ") (org-read-date "To: ") (read-string "Match: ")))
  (insert
  (mapconcat
   (lambda (o)
     (format "- %s %s" (my-journal-zidstring o) (my-journal-note o)))
   (seq-filter (lambda (o) (string-match match (my-journal-other o)))
    (my-journal-get-entries from to))
   "\n")))
(defun my-journal-convert-to-refs (beg end)
  (interactive "r")
  (save-restriction
    (goto-char beg)
    (narrow-to-region beg end)
    (while (re-search-forward "^- \\([0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]-[0-9][0-9]\\) .*?$" nil t)
      (replace-match "ref:\\1"))))
  (defun my-journal-get-refs-from-region (beg end)
    (interactive "r")
    (save-excursion
      (goto-char beg)
      (cl-loop for pos = (re-search-forward " \\([0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]-[0-9][0-9]\\) " end t)
               while pos
               collect (match-string 1))))

(defun my-journal-add-tag (tag beg end)
  (interactive "MTag: \nr")
  (let* ((url-request-method "POST")
         (url-request-extra-headers '(("Content-Type" . "application/json")))
         (zids (my-journal-get-refs-from-region beg end))
         (json-object-type 'plist)
         (url-request-data (json-encode-plist (list :zids zids :tags (split-string tag " ")))))
    (pp (my-journal-get "api/entries/tag/bulk"))))

(defun my-journal-remove-tag (tag beg end)
  (interactive "MTag: \nr")
  (let* ((url-request-method "DELETE")
         (url-request-extra-headers '(("Content-Type" . "application/json")))
         (zids (my-journal-get-refs-from-region beg end))
         (json-object-type 'plist)
         (url-request-data (json-encode-plist (list :zids zids :tags (split-string tag " ")))))
    (pp (my-journal-get "api/entries/tag/bulk"))))

(defun my-journal-post-with-refs (note date other beg end)
  (interactive (list
                (read-string "Note: ")
                (concat (org-read-date "Date: ") " 23:00")
                (read-string "Other: ")
                (min (point) (mark))
                (max (point) (mark))))
  (my-journal-post note :Date date :Other (concat other "\n"
                                                  (mapconcat (lambda (o) (concat "ref:" o))
                                                             (my-journal-get-refs-from-region beg end)
                                                             " "))))
;; Working with journal entries:1 ends here

;; Tagging journal entries
;; :PROPERTIES:
;; :CUSTOM_ID: tagging-journal-entries
;; :END:


;; [[file:Sacha.org::#tagging-journal-entries][Tagging journal entries:1]]
(defun my-journal-list-toggle-monthly-highlight ()
	(interactive)
	(let ((entry (tabulated-list-get-entry)))
		(setf (elt entry 3) (if (string-match "#monthly-highlight" (elt entry 3))
														(replace-regexp-in-string " ?#monthly-highlight" "" (elt entry 3))
													(string-trim (concat (elt entry 3) " #monthly-highlight"))))
		(my-journal-update
		 (list :ZIDString (elt entry 0)
					 :Other (elt entry 3)))
		(tabulated-list-print t t)))

(defun my-journal-list-echo ()
	(interactive)
	(message "%s -- %s" (elt (tabulated-list-get-entry) 2) (elt (tabulated-list-get-entry) 3)))

(defvar-keymap my-journal-list-mode-map
	:parent tabulated-list-mode-map
	"t" #'my-journal-list-toggle-monthly-highlight
	"v" #'my-journal-list-echo)

(define-derived-mode my-journal-list-mode tabulated-list-mode "Journal"
	"Major mode for journal entries."
	(setq tabulated-list-format [("ZID" 14 t)
															 ("Category" 10 t)
															 ("Note" 80 nil)
															 ("Other" 30 nil)])
	(tabulated-list-init-header)
	(tabulated-list-print t))

(defun my-journal-list (start end filter)
	(interactive (list (org-read-date "Start: ") (org-read-date "End: ")
										 (read-string "Filter: ")))
	(switch-to-buffer (get-buffer-create "*journal*"))
	(setq tabulated-list-entries
				(mapcar
				 (lambda (row)
					 (list
						(my-journal-zidstring row)
						(vector
						 (my-journal-zidstring row)
						 (my-journal-category row)
						 (replace-regexp-in-string "\n" " " (my-journal-note row))
						 (replace-regexp-in-string "\n" " " (my-journal-other row)))))
				 (my-journal-get-entries start end filter)))
	(my-journal-list-mode))
;; Tagging journal entries:1 ends here

;; Photos  :images:
;; :PROPERTIES:
;; :CUSTOM_ID: photos
;; :END:

;; [[file:Sacha.org::#photos][Photos:1]]
(defun my-get-image-caption (file)
  (let ((caption (shell-command-to-string (format "exiftool -s -s -s -ImageDescription %s" (shell-quote-argument file)))))
    (when (> (length caption) 0) (format "#+CAPTION: %s" caption))))

(defun my-insert-image-link-with-caption (file)
  (let ((caption (my-get-image-caption file)))
    (insert (or caption "") (org-link-make-string file) "\n")))

(defun my-caption-current-image ()
  (interactive)
  (let ((link (org-element-link-parser)) caption)
    (when (and link (org-element-property :path link))
      (setq caption (my-get-image-caption (org-element-property :path link)))
      (when caption (insert caption)))))

(defun my-set-image-caption (file caption)
  (interactive (list (if (derived-mode-p 'dired-mode) (dired-get-filename) (buffer-file-name))
                     (read-string "Caption: ")))
  (shell-command (format "exiftool -ImageDescription=\"%s\" %s" (shell-quote-argument caption) (shell-quote-argument file))))
;; Photos:1 ends here

;; [[file:Sacha.org::#photos][Photos:2]]
(defvar my-photo-directory "/mnt/nfs/photos/inbox")
(defun my-get-photo-rating (file)
  (let ((rating (shell-command-to-string (concat "exiftool -s -s -s -Rating " (shell-quote-argument file)))))
    (string-to-number rating)))

(defun my-make-photo-list (start end &optional rating require-description)
  (interactive (list (org-read-date "Start: ") (org-read-date "End: ")))
  (-filter
   (lambda (filename)
     (and (string> (file-name-nondirectory filename) start)
          (string> end (file-name-nondirectory filename))
          (if rating (>= (my-get-photo-rating filename) rating) t)
          (if require-description (my-get-image-caption filename) t)))
   (directory-files my-photo-directory t ".*\\.jpg$")))

(defun my-org-get-photo (id)
  "Open the photo identified by ID."
  (car (directory-files my-photo-directory t (concat id ".*\\.jpg"))))

(defun my-org-open-photo (id)
  (find-file (my-org-get-photo id)))

                                        ;(my-make-photo-list "2018-06-10" "2018-06-15" nil t)
                                        ;(my-get-photo-rating  (my-org-get-photo "2018-06-10-18-16-31"))

(defun my-org-significant-moments (start end &optional rating)
  (interactive (list (org-read-date "Start: ") (org-read-date "End: ") 3))
  (let ((result
         (mapconcat (lambda (file)
                      (let ((caption (my-get-image-caption file)))
                        (if caption
                            (concat caption (org-link-make-string file) "\n")
                          (concat (org-link-make-string file) "\n"))))
                    (my-make-photo-list start end 3)
                    "\n")))
    (if (called-interactively-p 'any) (insert result) result)))
;; Photos:2 ends here

;; Moments


;; [[file:Sacha.org::*Moments][Moments:1]]
(defun my-journal-moments (date)
	(interactive (list (org-read-date "Start: ")))
	(my-journal-post (concat "Moments starting " date " #moment") :Date (concat date " 23:00") :Category "Thoughts"))
;; Moments:1 ends here

;; Slicing and dicing the journal entries

;; [[file:Sacha.org::*Slicing and dicing the journal entries][Slicing and dicing the journal entries:1]]
(defun my-journal-filter-by-category (category list)
	(reverse (seq-filter (lambda (o) (string= (my-journal-category o) "Eating"))
											 list)))
(defun my-journal-group-by-month (list)
	(seq-group-by (lambda (o)
									(substring (my-journal-date o) 0 7))
								list))
(defun my-journal-filter-by-month (month-regexp list)
	(seq-filter (lambda (o)
								(string-match month-regexp
															(substring (my-journal-date o) 5 7)))
								list))
(defun my-journal-group-by-month-day (list)
	(seq-group-by (lambda (o)
									(substring (my-journal-date o) 5))
								list))
(defun my-journal-list-with-day (list)
	(mapconcat (lambda (o)
							 (concat "  - " (substring (my-journal-date o) 8) " "
											 (replace-regexp-in-string "#.*" "" (my-journal-note o))))
						 list
						 "\n"))
(defun my-journal-list-with-year (list)
	(mapconcat (lambda (o)
							 (concat "  - " (substring (my-journal-date o) 0 4) " "
											 (replace-regexp-in-string "#.*" "" (my-journal-note o))))
						 list
						 "\n"))
(defun my-journal-this-month-by-day (list)
	(mapconcat (lambda (group)
							 (format
								"- %s\n%s"
								(car group)
								(my-journal-list-with-year (cdr group))))
						 (cl-sort
							(my-journal-group-by-month-day
							 (my-journal-filter-by-month (format-time-string "%02m")
																					 list))
						'string<
						:key #'car)
					 "\n"))
;; Slicing and dicing the journal entries:1 ends here

;; Attachments
;; :PROPERTIES:
;; :CUSTOM_ID: attachments
;; :END:

;; Org lets you attach files to an Org file. Haven't gotten the hang of this yet, but looks interesting.


;; [[file:Sacha.org::#attachments][Attachments:1]]
(use-package org-attach
  :ensure nil
  :config
  (setq org-attach-store-link-p 'attached)
  (setq org-attach-auto-tag nil))
;; Attachments:1 ends here

;; HTTP
;; :PROPERTIES:
;; :CUSTOM_ID: http
;; :END:


;; [[file:Sacha.org::#http][HTTP:1]]
(use-package ob-http)
;; HTTP:1 ends here

;; Lilypond
;; :PROPERTIES:
;; :CUSTOM_ID: lilypond
;; :END:

;; [[file:Sacha.org::#lilypond][Lilypond:1]]
(use-package lilypond-init
  :if my-laptop-p
  :load-path "~/vendor/lilypond/elisp"
  :config
  (setq org-babel-lilypond-arrange-mode t
        org-babel-lilypond-commands '("lilypond" "timidity" "timidity")
        org-babel-lilypond-gen-pdf nil
        org-babel-lilypond-display-pdf-post-tangle nil)
  :mode ("\\.ly\\'" . LilyPond-mode))
;; Lilypond:1 ends here

;; Diagrams and graphics
;; :PROPERTIES:
;; :CUSTOM_ID: diagrams-and-graphics
;; :END:

;; Ooooh. Graphviz and Ditaa make it easier to create diagrams from Emacs. See [[http://sachachua.com/evil-plans]] for examples and source.

;; The ~pikchr-cli~ package in Ubuntu 24 is version 0.1.2 and does not have the ~--svg-only~ argument that ~pikchr-mode~ uses, so I followed the instructions at https://pikchr.org/home/doc/trunk/doc/download.md to download and compile pikchr.c.


;; [[file:Sacha.org::#diagrams-and-graphics][Diagrams and graphics:1]]
;also includes Org Babel support
(use-package pikchr-mode
	:config
	(setq pikchr-executable "/home/sacha/vendor/pikchr/pikchr"))
;; Diagrams and graphics:1 ends here

;; [[file:Sacha.org::#diagrams-and-graphics][Diagrams and graphics:2]]
(use-package ob-mermaid)
(setq org-ditaa-jar-path "c:/sacha/Dropbox/bin/ditaa.jar")
(setq org-startup-with-inline-images t)
(use-package org-contrib)
(use-package org
  :config
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
  (setq org-confirm-babel-evaluate nil)
  (setq org-link-elisp-confirm-function
        (lambda (prompt)
          (if (and (buffer-file-name) (string-match "vendor" (buffer-file-name)))
              (y-or-n-p prompt)
            t)))
  (require 'ob-ledger)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((dot . t)
     (ditaa . t)
		 (pikchr . t)
		 (gnuplot . t)
		 (mermaid . t)
     (emacs-lisp . t)
     (plantuml . t)
     (lilypond . t)
     (python . t)
     (ruby . t)
     (shell . t)
     (calc . t)
     (js . t)
     (sqlite . t)
     (http . t)
		 (org . t)
     (ledger . t)
     (shell . t)
     (R . t)))
  (setq org-babel-python-command "python3")
  (setq python-shell-interpreter "python3")
	(add-to-list 'org-src-lang-modes '("html" . web))
  (add-to-list 'org-src-lang-modes '("dot" . graphviz-dot)))
;; Diagrams and graphics:2 ends here

;; Counting
;; :PROPERTIES:
;; :CUSTOM_ID: counting
;; :END:

;; Good way to remind myself that I have lots of STARTED tasks.


;; [[file:Sacha.org::#counting][Counting:1]]
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
;; Counting:1 ends here

;; Spreadsheets
;; :PROPERTIES:
;; :CUSTOM_ID: spreadsheets
;; :END:

;; [[file:Sacha.org::#spreadsheets][Spreadsheets:1]]
(defun my-org-days-between (start end)
  "Number of days between START and END (exclusive).
      This includes START but not END."
  (- (calendar-absolute-from-gregorian (org-date-to-gregorian end))
     (calendar-absolute-from-gregorian (org-date-to-gregorian start))))
;; Spreadsheets:1 ends here

;; Editing source code
;; :PROPERTIES:
;; :CUSTOM_ID: editing-source-code
;; :END:
;; I don't want to get distracted by the same code in the other window, so I want org src to use the current window.


;; [[file:Sacha.org::#editing-source-code][Editing source code:1]]
(setq org-src-window-setup 'current-window)
;; Editing source code:1 ends here

;; Copying and sharing code
;; :PROPERTIES:
;; :CUSTOM_ID: copying-and-sharing-code
;; :END:


;; [[file:Sacha.org::#copying-and-sharing-code][Copying and sharing code:1]]
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
;; Copying and sharing code:1 ends here

;; Tables
;; :PROPERTIES:
;; :CUSTOM_ID: tables
;; :END:

;;      Requires dash.


;; [[file:Sacha.org::#tables][Tables:1]]
(defun my-org-table-as-alist (table)
  "Convert TABLE to an alist. Remember to set :colnames no."
  (let ((headers (seq-map 'intern (car table))))
    (cl-loop for x in (cdr table) collect (-zip headers x))))
;; Tables:1 ends here

;; Invoices
;; :PROPERTIES:
;; :CUSTOM_ID: invoices
;; :END:


;; [[file:Sacha.org::#invoices][Invoices:1]]
(setq calendar-week-start-day 6) ;; My weeks start on Saturday

(defun my-org-get-invoice-range-based-on-date (date)
  (let* ((invoice-date (org-date-to-gregorian date))
         (start (list (1- (car invoice-date)) 1 (elt invoice-date 2)))
         (end (list (car invoice-date) 1 (elt invoice-date 2))))
    (mapcar (lambda (date)
              (format-time-string "%F %H:%M" (encode-time 0 0 0 1 (elt date 0) (elt date 2))))
            (list start end))))

(defun my-org-quantified-get-hours-based-on-range (category start end)
  "Return the number of hours for the specified category."
  (/ (assoc-default category
                    (quantified-summarize-time start end)) 3600.0))

;; TODO: paginate
(defun my-org-quantified-get-detailed-hours-based-on-range (category start end)
  "Return a list of (date week-ending-date dow seconds) for CATEGORY from START to END."
  (let ((entries
         (gethash "entries"
                  (quantified-parse-json
                   (quantified-request (format "records.json?start=%s&end=%s&filter_string=%s&per_page=1000&split=split" start end (url-encode-url category))
                                       nil "GET")))))
    (mapcar
     (lambda (entry)
       (let ((time (date-to-time (gethash "timestamp" entry))))
         (list
          (format-time-string "%F" time)
          (format-time-string "%F" (my-get-week-end-for-time time))
          (format-time-string "%a" time)
          (gethash "duration" entry))))
     entries)))

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


(defun my-org-quantified-hours-table ()
  (my-org-quantified-format-detailed-hours-as-table
   (apply 'my-org-quantified-get-detailed-hours-based-on-range
          (org-entry-get-with-inheritance "QUANTIFIED_CATEGORY")
          (my-org-get-invoice-range-based-on-date (org-entry-get-with-inheritance "INVOICE_DATE")))))

(ert-deftest my-org-get-invoice-range-based-on-date ()
  "Check if invoice range is sane."
  (should (equal (my-org-get-invoice-range-based-on-date "2015-12-05")
                 '("2015-11-01 00:00" "2015-12-01 00:00"))))
;; Invoices:1 ends here

;; Presentations
;; :PROPERTIES:
;; :CUSTOM_ID: presentations
;; :END:


;; [[file:Sacha.org::#presentations][Presentations:1]]
(use-package org-re-reveal
	:config
	(setq org-re-reveal-revealjs-version "4")
	(setq org-re-reveal-history t))
(use-package oer-reveal
	:config
	(setq oer-reveal-plugin-4-config
				"audioslideshow RevealAudioSlideshow plugin/audio-slideshow/plugin.js
anything RevealAnything https://cdn.jsdelivr.net/npm/reveal.js-plugins@latest/anything/plugin.js"))
;; Presentations:1 ends here

;; Counting words
;; :PROPERTIES:
;; :CUSTOM_ID: counting-words
;; :END:

;; #+NAME: counting-words-in-notes

;; [[file:Sacha.org::counting-words-in-notes][counting-words-in-notes]]
(defvar my-org-note-words-target (* 140 20))
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

(defun my-org-create-notes-buffer ()
	(interactive)
	(let ((notes (my-org-collect-notes)))
		(with-current-buffer (get-buffer-create "*Notes*")
			(insert (string-join notes "\n\n"))
			(switch-to-buffer (current-buffer)))))
;; counting-words-in-notes ends here

;; Allow dashes in tags
;; :PROPERTIES:
;; :CUSTOM_ID: allow-dashes-in-tags
;; :END:


;; [[file:Sacha.org::#allow-dashes-in-tags][Allow dashes in tags:1]]
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
(use-package org :hook (org-mode . my-org-add-dashes-to-tag-regexps))
;; Allow dashes in tags:1 ends here

;; Convert from Markdown

;; ChatGPT likes to output Markdown. I like to think in Org Mode.


;; [[file:Sacha.org::*Convert from Markdown][Convert from Markdown:1]]
(defun my-org-convert-region-from-markdown (beg end)
	(interactive "r")
	(shell-command-on-region beg end "pandoc -t org" nil t))
;; Convert from Markdown:1 ends here

;; Copying information from my phone
;; :PROPERTIES:
;; :CUSTOM_ID: copying-information-from-my-phone
;; :END:

;; I have a tiny Tasker script that makes it easy to log timestamped
;; entries as files in a directory that I synchronize with Dropbox. This
;; code pulls that information into my ~/Dropbox/tasker/


;; [[file:Sacha.org::#copying-information-from-my-phone][Copying information from my phone:1]]
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
;; Copying information from my phone:1 ends here

;; ASCII export
;; :PROPERTIES:
;; :CUSTOM_ID: ascii-export
;; :END:

;; This setting puts Org ASCII export links right after the text instead of in a separate section:


;; [[file:Sacha.org::#ascii-export][ASCII export:1]]
(setq org-ascii-links-to-notes nil)
;; ASCII export:1 ends here

;; Reddit
;; :PROPERTIES:
;; :CUSTOM_ID: reddit
;; :END:

;; This one exports links from my secret =my-reddit-upvoted-json=. You
;; can get your Reddit upvoted JSON URL at
;; https://www.reddit.com/prefs/feeds/ .


;; [[file:Sacha.org::#reddit][Reddit:1]]
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
;; Reddit:1 ends here

;; Sorting Org Mode lists using a sequence of regular expressions :emacs:org:
;; :PROPERTIES:
;; :ID:       o2b:ab84dc77-bea4-4e71-ae7f-e91fb34bfa28
;; :POST_DATE: [2017-12-21 Thu 12:08]
;; :POSTID:   29132
;; :BLOG:     sacha
;; :CUSTOM_ID: sorting-org-mode-lists-using-a-sequence-of-regular-expressions
;; :END:

;; I manually categorize Emacs News links into an Org unordered list, and
;; then I reorganize the list by using M-S-up (org-shiftmetaup) and
;; M-S-down (org-shiftmetadown). I decide to combine or split categories
;; depending on the number of links. I have a pretty consistent order.
;; John Wiegley suggested promoting Emacs Lisp and Emacs development
;; links at the top of the list. I like to sort the rest of the list
;; roughly by interest: general links first, then Org, then coding, then
;; other links at the bottom.

;; Here's some code that sorts Org lists in a custom sequence, with
;; unknown items at the bottom for easy re-ordering. It will take a list like:

;; #+begin_example
;;      - Other:
;;        - Link A
;;        - Link B
;;      - Emacs development:
;;        - Link A
;;        - Link B
;;      - Emacs Lisp:
;;        - Link A
;;        - Link B
;; #+end_example

;; and turn it into:

;; #+begin_example
;;      - Emacs Lisp:
;;        - Link A
;;        - Link B
;;      - Emacs development:
;;        - Link A
;;        - Link B
;;      - Other:
;;        - Link A
;;        - Link B
;; #+end_example


;; [[file:Sacha.org::#sorting-org-mode-lists-using-a-sequence-of-regular-expressions][Sorting Org Mode lists using a sequence of regular expressions:1]]
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
;; Sorting Org Mode lists using a sequence of regular expressions:1 ends here

;; Package links
;; :PROPERTIES:
;; :CUSTOM_ID: package-links
;; :END:

;; #+NAME: org-package-link

;; [[file:Sacha.org::org-package-link][org-package-link]]
(defun my-org-package-open (package-name)
  (interactive "MPackage name: ")
  (describe-package (intern package-name)))

(ert-deftest my-org-package-export ()
  (should
   (string=
    (my-org-package-export "transcribe" "transcribe" 'html)
    "<a target=\"_blank\" href=\"https://elpa.gnu.org/packages/transcribe.html\">transcribe</a>"
    ))
  (should
   (string=
    (my-org-package-export "fireplace" "fireplace" 'html)
    "<a target=\"_blank\" href=\"http://melpa.org/#/fireplace\">fireplace</a>"
    )))
(defun my-org-package-export (link description format &optional arg)
  (let* ((package-info (car (assoc-default (intern link) package-archive-contents)))
         (package-source (and package-info (package-desc-archive package-info)))
         (path (format
                (cond
								 ((null package-source) link)
                 ((string= package-source "gnu") "https://elpa.gnu.org/packages/%s.html")
                 ((string= package-source "melpa") "http://melpa.org/#/%s")
                 ((string= package-source "nongnu") "https://elpa.nongnu.org/nongnu/%s.html")
                 (t (error 'unknown-source)))
                link))
         (desc (or description link)))
		(if package-source
				(cond
				 ((eq format '11ty) (format "<a target=\"_blank\" href=\"%s\">%s</a>" path desc))
				 ((eq format 'html) (format "<a target=\"_blank\" href=\"%s\">%s</a>" path desc))
				 ((eq format 'wp) (format "<a target=\"_blank\" href=\"%s\">%s</a>" path desc))
				 ((eq format 'latex) (format "\\href{%s}{%s}" path desc))
				 ((eq format 'texinfo) (format "@uref{%s,%s}" path desc))
				 ((eq format 'ascii) (format "%s <%s>" desc path))
				 (t path))
			desc)))
(defun my-org-package-complete ()
	(require 'finder-inf nil t)
  (unless package--initialized
    (package-initialize t))
	(concat
	 "package:"
   ;; Load the package list if necessary (but don't activate them).
   (let ((packages (mapcar #'symbol-name (mapcar #'car package-archive-contents))))
		 (completing-read "Package: "
                      packages nil t nil nil))))

(defun my-org-package-link-description (link description)
	(unless description
		(when (string-match "package:\\(.+\\)" link)
			(match-string 1 link))))

(org-link-set-parameters
 "package"
 :follow 'my-org-package-open :export 'my-org-package-export :complete 'my-org-package-complete
 :insert-description #'my-org-package-link-description)
;; org-package-link ends here

;; Save when Emacs loses focus
;; :PROPERTIES:
;; :CUSTOM_ID: save-when-emacs-loses-focus
;; :END:


;; [[file:Sacha.org::#save-when-emacs-loses-focus][Save when Emacs loses focus:1]]
(defun my-org-save-all-org-buffers ()
  (unless my-unfocusing
    (let ((my-unfocusing t))
      (my-org-debounce-idle-timer 10
                                  my-org-save-all-org-buffers-timer
                                  'org-save-all-org-buffers))))
(use-package org
  :config
  (add-function :after after-focus-change-function 'my-org-save-all-org-buffers))
;; Save when Emacs loses focus:1 ends here

;; Org links
;; :PROPERTIES:
;; :CUSTOM_ID: org-links
;; :END:

;; Based on https://xenodium.com/emacs-dwim-do-what-i-mean/

;; [[file:Sacha.org::#org-links][Org links:1]]
(defun my-page-title (url)
	(with-current-buffer (url-retrieve-synchronously url)
		(string-trim
		 (replace-regexp-in-string
			"[ \n]+" " "
			(replace-regexp-in-string
			 "\\(^Github - \\|:: Sacha Chua\\)" ""
			 (or
				(dom-text (car
									 (dom-by-tag (libxml-parse-html-region
																(point-min)
																(point-max))
															 'title)))
				""))))))

(defun ar/org-insert-link-dwim (use-clipboard)
  "Like `org-insert-link' but with personal dwim preferences."
  (interactive (list (equal current-prefix-arg '(4))))
  (let* ((point-in-link (org-in-regexp org-link-any-re 1))
         (clipboard-url (and use-clipboard
                          (when (string-match-p "^http" (current-kill 0))
                            (current-kill 0))))
         (region-content (when (region-active-p)
                           (buffer-substring-no-properties (region-beginning)
                                                           (region-end)))))
    (cond
		 ((and (derived-mode-p 'markdown-mode) region-content clipboard-url)
			(delete-region (region-beginning) (region-end))
      (insert (format "[%s](%s)" region-content clipboard-url)))
		 ((and (derived-mode-p 'markdown-mode) clipboard-url)
      (insert (format "[%s](%s)" (my-page-title clipboard-url) clipboard-url)))
		 ((derived-mode-p 'markdown-mode)
      (insert (format "[%s](%s)" (read-string "Text: ") (read-string "Link: "))))
		 ((and region-content clipboard-url (not point-in-link))
      (delete-region (region-beginning) (region-end))
      (insert (org-link-make-string clipboard-url region-content)))
     ((and clipboard-url (not point-in-link))
      (insert (org-link-make-string
               clipboard-url
               (read-string "title: "
														(my-page-title clipboard-url)))))
     (t
      (call-interactively 'org-insert-link)))))
(use-package org :bind (:map org-mode-map ("C-c C-l" . ar/org-insert-link-dwim)))
(with-eval-after-load 'markdown-mode
	(define-key markdown-mode-map (kbd "C-c C-l") #'ar/org-insert-link-dwim))
;; Org links:1 ends here

;; Clipboard
;; :PROPERTIES:
;; :CUSTOM_ID: clipboard
;; :END:

;; [[file:Sacha.org::#clipboard][Clipboard:1]]
(defun my-org-insert-clipboard ()
  "Convert clipboard contents from HTML to Org and then paste (yank)."
  (interactive)
  (insert (shell-command-to-string "xclip -o -selection clipboard -t text/html | pandoc -f html -t json | pandoc -f json -t org")))
;; Clipboard:1 ends here

;; Setting properties
;; :PROPERTIES:
;; :CUSTOM_ID: setting-properties
;; :END:


;; [[file:Sacha.org::#setting-properties][Setting properties:1]]
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
(use-package org
  :bind (:map org-mode-map
              ("C-c C-x p" . my-org-set-property)))
;; Setting properties:1 ends here

;; Linking to and exporting function definitions in Org Mode     :emacs:org:
;; :PROPERTIES:
;; :EXPORT_DATE: 2023-01-02T21:34:25-0500
;; :EXPORT_ELEVENTY_PERMALINK: /blog/2023/01/linking-to-and-exporting-function-definitions-in-org-mode/
;; :EXPORT_ELEVENTY_FILE_NAME: blog/2023/01/linking-to-and-exporting-function-definitions-in-org-mode/
;; :CUSTOM_ID: linking-to-and-exporting-function-definitions-in-org-mode
;; :END:

;; #+begin_update
;; - [2024-01-11 Thu]: Added ?link=1 to copy the context link
;; - 2023-09-12: added a way to force the defun to start open with ?open=1
;; - 2023-09-05: fixed the completion to include =defun:=
;; #+end_update

;; I'd like to write more blog posts about little Emacs hacks, and I'd
;; like to do it with less effort. Including source code is handy even
;; when it's missing some context from other functions defined in the
;; same file, since sometimes people pick up ideas and having the source
;; code right there means less flipping between links. When I'm working
;; inside my config file or other literate programming documents, I can
;; just write my blog post around the function definitions. When I'm
;; talking about Emacs Lisp functions defined elsewhere, though, it's a
;; little more annoying to copy the function definition and put it in a
;; source block, especially if there are updates.

;; The following code creates a ~defun~ link type that exports the function
;; definition. It works for functions that can be located with
;; find-function, so only functions loaded from .el files, but that does
;; what I need for now. Probably once I post this, someone will mention a
;; much more elegant way to do things. Anyway, it makes it easier to use
;; ~org-store-link~ to capture a link to the function, insert it into a
;; blog post, navigate back to the function, and export HTML.

;; #+NAME: org-defun-link

;; [[file:Sacha.org::org-defun-link][org-defun-link]]
(defun my-org-defun-complete ()
	"Return function definitions."
	(concat "defun:"
					(completing-read
					 "Function: "
					 #'help--symbol-completion-table
					 #'fboundp
					 'confirm
					 nil nil))) ; 	 (and fn (symbol-name fn)) ?

(defun my-org-defun-link-description (link description)
	"Add documentation string as part of the description"
	(unless description
		(when (string-match "defun:\\(.+\\)" link)
			(let ((symbol (intern (match-string 1 link))))
				(when (documentation symbol)
					(concat (symbol-name symbol) ": "
									(car (split-string (documentation symbol) "\n"))))))))

(defun my-org-defun-open-complete ()
	"Return function definitions."
	(concat "defun-open:"
					(completing-read
					 "Function: "
					 #'help--symbol-completion-table
					 #'fboundp
					 'confirm
					 nil nil)))

(defun my-org-defun-open-export (link description format _)
	(my-org-defun-export (concat link (if (string-match "\\?" link) "&open=1" "?open=1")) description format _))

(defun my-org-defun-export (link description format _)
	"Export the function."
	(let (symbol params path-and-query)
		(if (string-match "\\?" link)
				(setq path-and-query (url-path-and-query (url-generic-parse-url link))
							symbol (car path-and-query)
							params (url-parse-query-string (cdr path-and-query)))
			(setq symbol link))
		(save-window-excursion
			(my-org-defun-open symbol)
			(let ((function-body (buffer-substring (point)
																						 (progn (forward-sexp) (point))))
						body)
				(pcase format
					((or '11ty 'html)
					 (setq body
								 (if (assoc-default "bare" params 'string=)
										 (format "<div class=\"org-src-container\"><pre class=\"src src-emacs-lisp\">%s</pre></div>"
														 (org-html-do-format-code function-body "emacs-lisp" nil nil nil nil))
									 (format "<details%s><summary>%s</summary><div class=\"org-src-container\"><pre class=\"src src-emacs-lisp\">%s</pre></div></details>"
													 (if (assoc-default "open" params 'string=) " open"
														 "")
													 (or description
															 (and (documentation (intern symbol))
																		(concat
																		 symbol
																		 ": "
																		 (car (split-string (documentation (intern symbol)) "\n"))))
															 symbol)
													 (org-html-do-format-code function-body "emacs-lisp" nil nil nil nil))))
					 (when (assoc-default "link" params)
						 (setq body (format "%s<div><a href=\"%s\">Context</a></div>" body (my-copy-link))))
					 body)
					('latex
					 (org-latex-src-block `(test
																	(:language "emacs-lisp" :value ,function-body))
																function-body nil))
					('ascii function-body)
					(_ function-body))))))

(defun my-org-defun-store ()
	"Store a link to the function."
	(when (derived-mode-p 'emacs-lisp-mode)
		(org-link-store-props :type "defun"
													:link (concat "defun:" (lisp-current-defun-name)))))

(defun my-org-defun-open (symbol &rest _)
	"Jump to the function definition.
If it's from a tangled file, follow the link."
	(find-function (intern (replace-regexp-in-string "\\?.*$" "" symbol)))
	(when (re-search-backward "^;; \\[\\[file:" nil t)
		(goto-char (match-end 0))
		(org-open-at-point-global)
		(when (re-search-forward (concat "( *defun +" (regexp-quote (replace-regexp-in-string "\\?.*$" "" symbol)))
														 nil t)
			(goto-char (match-beginning 0)))))

(org-link-set-parameters "defun" :follow #'my-org-defun-open
												 :export #'my-org-defun-export
												 :complete #'my-org-defun-complete
												 :insert-description #'my-org-defun-link-description
												 :store #'my-org-def-store)

(org-link-set-parameters "defun-open" :follow #'my-org-defun-open
												 :export #'my-org-defun-open-export
												 :complete #'my-org-defun-open-complete
												 :insert-description #'my-org-defun-link-description)
;; org-defun-link ends here

;; Still allow linking to the file
;; :PROPERTIES:
;; :ID:       20240108T074407.456930
;; :END:

;; Sometimes I want to link to a defun and sometimes I want to link to
;; the file itself. Maybe I can have a file link with the same kind of
;; scoping so that it kicks in only when ~defun:~ would also kick in.


;; [[file:Sacha.org::*Still allow linking to the file][Still allow linking to the file:1]]
(defun my-org-defun-store-file-link ()
	"Store a link to the file itself."
	(when (derived-mode-p 'emacs-lisp-mode)
		(org-link-store-props :type "file"
													:link (concat "file:" (buffer-file-name)))))
(with-eval-after-load 'org
	(org-link-set-parameters "_file" :store #'my-org-defun-store-file-link))
;; Still allow linking to the file:1 ends here

;; Including variables
;; :PROPERTIES:
;; :CUSTOM_ID: including-variables
;; :EXPORT_MODIFIED: 2024-01-20T15:49:07-0500
;; :END:
;; #+begin_update
;; [2024-01-20]: Fixed org-def-store thanks to oantolin's comment.
;; #+end_update

;; #+NAME: org-defvar-link

;; [[file:Sacha.org::org-defvar-link][org-defvar-link]]
(defun my-org-defvar-complete ()
	"Return variable definitions."
	(concat "defvar:"
					(completing-read
					 "Variable: "
					 #'help--symbol-completion-table
					 #'indirect-variable
					 'confirm
					 nil nil))) ; 	 (and fn (symbol-name fn)) ?
(defun my-org-defvar-link-description (link description)
	"Add documentation string as part of the description"
	(unless description
		(when (string-match "\\(?:defun\\|defvar\\):\\(.+\\)" link)
			(let* ((symbol (intern (match-string 1 link)))
						 (doc (documentation-property symbol 'variable-documentation symbol)))
				(when doc
					(concat (symbol-name symbol) ": "
									(car (split-string doc "\n"))))))))

(defun my-org-def-export (link description format _)
	"Export the variable-or-function."
	(let (symbol params path-and-query)
		(if (string-match "\\?" link)
				(setq path-and-query (url-path-and-query (url-generic-parse-url link))
							symbol (car path-and-query)
							params (url-parse-query-string (cdr path-and-query)))
			(setq symbol link))
		(save-window-excursion
			(if (functionp (intern symbol))
					(find-function (intern symbol))
				(find-variable (intern symbol)))
			(let ((body (buffer-substring (point)
																		(progn (forward-sexp) (point)))))
				(pcase format
					((or '11ty 'html)
					 (if (assoc-default "bare" params 'string= "")
							 (format "<div class=\"org-src-container\"><pre class=\"src src-emacs-lisp\">%s</pre></div>"
											 (org-html-do-format-code body "emacs-lisp" nil nil nil nil))

						 (format "<details%s><summary>%s</summary><div class=\"org-src-container\"><pre class=\"src src-emacs-lisp\">%s</pre></div></details>"
										 (if (assoc-default "open" params 'string=) " open"
											 "")
										 (or description
												 (and (functionp (intern symbol))
															(documentation (intern symbol))
															(concat
															 symbol
															 ": "
															 (car (split-string (documentation (intern symbol)) "\n"))))
												 symbol)
										 (org-html-do-format-code body "emacs-lisp" nil nil nil nil))
						 ))
					(`ascii body)
					(_ body))))))

(defun my-org-def-store ()
	"Store a link to the function."
	(when (derived-mode-p 'emacs-lisp-mode)
		(save-excursion
      (or (eobp) (forward-char 1))
      (beginning-of-defun)
			(let ((data (read (current-buffer))))
				(if (eq (car data) 'defun)
						(org-link-store-props :type "defun"
																	:link (concat "defun:" (lisp-current-defun-name)))
					(org-link-store-props :type "defvar"
																:link (format "defvar:%s" (cadr data))))))))

(defun my-org-defvar-open (symbol _)
	"Jump to the function definition."
	(find-variable (intern (replace-regexp-in-string "\\?.*$" "" symbol))))

(org-link-set-parameters "defvar" :follow #'my-org-defvar-open
												 :export #'my-org-def-export
												 :complete #'my-org-defvar-complete
												 :insert-description #'my-org-defvar-link-description
												 ; :store #'my-org-def-store  ; already added by defun link
												 )
;; org-defvar-link ends here

;; Imagemagick
;; :PROPERTIES:
;; :CUSTOM_ID: imagemagick
;; :END:

;; https://xenodium.com/emacs-viewing-webp-images/

;; [[file:Sacha.org::#imagemagick][Imagemagick:1]]
(setq image-use-external-converter t)
;; Imagemagick:1 ends here

;; Artrage
;; :PROPERTIES:
;; :CUSTOM_ID: artrage
;; :END:


;; [[file:Sacha.org::#artrage][Artrage:1]]
(defun my-artrage-export-png (directory &optional prefix)
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

;; Tablet clicks count as drags
;; :PROPERTIES:
;; :CUSTOM_ID: tablet-clicks-count-as-drags
;; :END:


;; [[file:Sacha.org::#tablet-clicks-count-as-drags][Tablet clicks count as drags:1]]
(defun widget-button-click (event)
  "Invoke the button that the mouse is pointing at."
  (interactive "e")
  (if (widget-event-point event)
      (let* ((oevent event)
             (mouse-1 (memq (event-basic-type event) '(mouse-1 down-mouse-1)))
             (pos (widget-event-point event))
             (start (event-start event))
             (button (get-char-property
                      pos 'button (and (windowp (posn-window start))
                                       (window-buffer (posn-window start)))))
             newpoint)
        (when (or (null button)
                  (catch 'button-press-cancelled
                    ;; Mouse click on a widget button.  Do the following
                    ;; in a save-excursion so that the click on the button
                    ;; doesn't change point.
                    (save-selected-window
                      (select-window (posn-window (event-start event)))
                      (save-excursion
                        (goto-char (posn-point (event-start event)))
                        (let* ((overlay (widget-get button :button-overlay))
                               (pressed-face (or (widget-get button :pressed-face)
                                                 widget-button-pressed-face))
                               (face (overlay-get overlay 'face))
                               (mouse-face (overlay-get overlay 'mouse-face)))
                          (unwind-protect
                              ;; Read events, including mouse-movement
                              ;; events, waiting for a release event.  If we
                              ;; began with a mouse-1 event and receive a
                              ;; movement event, that means the user wants
                              ;; to perform drag-selection, so cancel the
                              ;; button press and do the default mouse-1
                              ;; action.  For mouse-2, just highlight/
                              ;; unhighlight the button the mouse was
                              ;; initially on when we move over it.
                              (save-excursion
                                (when face	; avoid changing around image
                                  (overlay-put overlay 'face pressed-face)
                                  (overlay-put overlay 'mouse-face pressed-face))
                                (unless (widget-apply button :mouse-down-action event)
                                  (let ((track-mouse t))
                                    (while (not (widget-button-release-event-p event))
                                      (setq event (read-event))

                                      ;; Sacha: Commented this section out so that my stylus
                                      ;; clicks don't get reported as mouse movement

                                      ;; (when (and mouse-1 (mouse-movement-p event))
                                      ;;   (push event unread-command-events)
                                      ;;   (setq event oevent)
                                      ;;   (throw 'button-press-cancelled t))
                                      (unless (or (integerp event)
                                                  (memq (car event) '(switch-frame select-window))
                                                  (eq (car event) 'scroll-bar-movement))
                                        (setq pos (widget-event-point event))
                                        (if (and pos
                                                 (eq (get-char-property pos 'button)
                                                     button))
                                            (when face
                                              (overlay-put overlay 'face pressed-face)
                                              (overlay-put overlay 'mouse-face pressed-face))
                                          (overlay-put overlay 'face face)
                                          (overlay-put overlay 'mouse-face mouse-face))))))

                                ;; When mouse is released over the button, run
                                ;; its action function.
                                (when (and pos (eq (get-char-property pos 'button) button))
                                  (goto-char pos)
                                  (widget-apply-action button event)
                                  (if widget-button-click-moves-point
                                      (setq newpoint (point)))))
                            (overlay-put overlay 'face face)
                            (overlay-put overlay 'mouse-face mouse-face))))

                      (if newpoint (goto-char newpoint))
                      ;; This loses if the widget action switches windows. -- cyd
                      ;; (unless (pos-visible-in-window-p (widget-event-point event))
                      ;;   (mouse-set-point event)
                      ;;   (beginning-of-line)
                      ;;   (recenter))
                      )
                    nil))
          (let ((up t) command)
            ;; Mouse click not on a widget button.  Find the global
            ;; command to run, and check whether it is bound to an
            ;; up event.
            (if mouse-1
                (cond ((setq command	;down event
                             (lookup-key widget-global-map [down-mouse-1]))
                       (setq up nil))
                      ((setq command	;up event
                             (lookup-key widget-global-map [mouse-1]))))
              (cond ((setq command	;down event
                           (lookup-key widget-global-map [down-mouse-2]))
                     (setq up nil))
                    ((setq command	;up event
                           (lookup-key widget-global-map [mouse-2])))))
            (when up
              ;; Don't execute up events twice.
              (while (not (widget-button-release-event-p event))
                (setq event (read-event))))
            (when command
              (call-interactively command)))))
    (message "You clicked somewhere weird.")))
;; Tablet clicks count as drags:1 ends here

;; SVG                                                              :image:
;; :PROPERTIES:
;; :CUSTOM_ID: svg
;; :END:

;; [[file:Sacha.org::#svg][SVG:1]]
(auto-image-file-mode -1)
;; SVG:1 ends here

;; Breaking up a PDF from Supernote

;; [[file:Sacha.org::*Breaking up a PDF from Supernote][Breaking up a PDF from Supernote:1]]
(defun my-sketch-convert-pdf (pdf-file)
	"Returns the SVG filename."
	(interactive "FPDF: ")
	(unless (file-exists-p (concat (file-name-sans-extension pdf-file) ".svg"))
		(call-process "pdftocairo" nil nil nil "-svg" (expand-file-name pdf-file)
									(expand-file-name (concat (file-name-sans-extension pdf-file) ".svg"))))
	(expand-file-name (concat (file-name-sans-extension pdf-file) ".svg")))

(defun my-sketch-recolor (dom color-map &optional selector)
	"Colors are specified as ((\"#input\" . \"#output\") ...)."
	(if (symbolp color-map)
			(setq color-map
 						(assoc-default color-map my-sketch-color-map)))
	(let ((map-re (regexp-opt (mapcar 'car color-map))))
		(dolist (path (if selector (dom-search dom selector)
										(dom-by-tag dom 'path)))
			(when (and (dom-attr path 'style)
								 (string-match map-re (dom-attr path 'style)))
				(dom-set-attribute
				 path 'style
				 (replace-regexp-in-string
					map-re
					(lambda (match)
						(assoc-default match color-map))
					(or (dom-attr path 'style) ""))))))
	dom)


(defun my-sketch-regroup (dom groups)
	"Move matching paths to their own group.
GROUPS is specified as ((id . (lambda (elem) ..)))."
	(dolist (group groups)
		(when-let* ((matches (dom-search dom (cdr group)))
								(node (dom-node 'g '((id . "highlights")))))
			(dolist (p matches)
				(dom-remove-node dom p)
				(dom-append-child node p))
			(dom-append-child dom node)))
	dom)

(defun my-sketch-break-apart (dom selector)
	"Break paths apart.
SELECTOR can be a function that takes the node as an argument and returns non-nil,
or a list of nodes."
	(dolist (path (if (functionp selector) (dom-search dom selector) selector))
		(let ((parent (dom-parent dom path)))
			;; break apart
			(when (dom-attr path 'd)
				(dolist (part (split-string (dom-attr path 'd) "M " t " +"))
					(dom-append-child
					 parent
					 (dom-node 'path `((style . ,(dom-attr path 'style))
														 (d . ,(concat "M " part))))))
				(dom-remove-node dom path))))
	dom)

(defun my-sketch-add-bg (dom)
	;; add background rectangle
	(let ((view-box (mapcar 'string-to-number (split-string (dom-attr dom 'viewBox)))))
		(push (dom-node 'rect `((x . 0)
														(y . 0)
														(width . ,(elt view-box 2))
														(height . ,(elt view-box 3))
														(fill . "#ffffff")))
					(cddr (car (dom-by-id dom "surface1")))))
	dom)

(defun my-sketch-clean (dom)
	"Remove USE and IMAGE tags."
	(dolist (use (dom-by-tag dom 'use))
		(dom-remove-node dom use))
	(dolist (use (dom-by-tag dom 'image))
		(dom-remove-node dom use))
	dom)

(defun my-sketch-rotate (dom)
	(let* ((old-width (dom-attr dom 'width))
				 (old-height (dom-attr dom 'height))
				 (view-box (mapcar 'string-to-number (split-string (dom-attr dom 'viewBox))))
				 (rotate (format "rotate(90) translate(0 %s)" (- (elt view-box 3)))))
		(dom-set-attribute dom 'width old-height)
		(dom-set-attribute dom 'height old-width)
		(dom-set-attribute dom 'viewBox (format "0 0 %d %d" (elt view-box 3) (elt view-box 2)))
		(dolist (g (dom-by-tag dom 'g))
			(dom-set-attribute g 'transform rotate)))
	dom)

(defun my-sketch-mix-blend-mode-darken (dom &optional selector)
	(dolist (p (if (functionp selector) (dom-search dom selector) (or selector (dom-by-tag dom 'path))))
		(when (dom-attr p 'style)
			(dom-set-attribute
			 p 'style
			 (replace-regexp-in-string ";;\\|^;" ""
																 (concat
																	(dom-attr p 'style)
																	";mix-blend-mode:darken")))))
	dom)

(defun my-sketch-color-to-hex (dom &optional selector)
	(dolist (p (if (functionp selector) (dom-search dom selector)
							 (or selector (dom-search dom
																				(lambda (p) (dom-attr p 'style))))))
		(when (dom-attr p 'style)
			(dom-set-attribute
			 p 'style
			 (replace-regexp-in-string
				"rgb(\\([0-9\\.]+\\)%,\\([0-9\\.%]+\\)%,\\([0-9\\.]+\\)%)"
				(lambda (s)
					(color-rgb-to-hex
					 (* 0.01 (string-to-number (match-string 1 s)))
					 (* 0.01 (string-to-number (match-string 2 s)))
					 (* 0.01 (string-to-number (match-string 3 s)))
					 2))
				(dom-attr p 'style)))))
	dom)

;; default for now, but will support more colour schemes someday
(defvar my-sketch-color-map
	'((blue
		 ("#9d9d9d" . "#2b64a9")
		 ("#9c9c9c" . "#2b64a9")
		 ("#c9c9c9" . "#b3e3f1")
		 ("#c8c8c8" . "#b3e3f1")
		 ("#cacaca" . "#b3e3f1")
		 ("#a6d2ff" . "#ffffff"))
		(t
		 ("#9d9d9d" . "#884636")
		 ("#9c9c9c" . "#884636")
		 ("#cacaca" . "#f6f396")
		 ("#c8c8c8" . "#f6f396")
		 ("#a6d2ff" . "#ffffff")
		 ("#c9c9c9" . "#f6f396"))))

(cl-defun my-sketch-convert-pdf-and-break-up-paths (pdf-file &key rotate color-map color-scheme selector)
	"Convert PDF to SVG and break up paths."
	(interactive (list (read-file-name
											(format "PDF (%s): "
															(my-latest-file "~/Dropbox/Supernote/EXPORT/" "pdf"))
											"~/Dropbox/Supernote/EXPORT/"
											(my-latest-file "~/Dropbox/Supernote/EXPORT/" "pdf")
											t
											nil
											(lambda (s) (string-match "pdf" s)))))
	(let ((dom (xml-parse-file (my-sketch-convert-pdf pdf-file)))
				(new-file (expand-file-name (concat (file-name-sans-extension pdf-file) "-split.svg"))))
		(setq dom (my-sketch-clean dom))
		(setq dom (my-sketch-color-to-hex dom))
		(setq dom (my-sketch-add-bg dom))
		(setq dom (my-sketch-regroup
							 dom
							 '(("highlights" . (lambda (elem)
																	 (and (dom-attr elem 'style)
																				(string-match "#c"
																											(dom-attr elem 'style))))))))
		(setq dom (my-sketch-mix-blend-mode-darken dom))
		(when rotate (setq dom (my-sketch-rotate dom)))
		(setq dom (my-sketch-break-apart dom (or selector
																						 (dom-by-tag dom 'path))))
		(setq dom (my-sketch-recolor dom
																 (or color-map
																		 color-scheme)))
		(with-temp-file new-file
			(svg-print (car dom)))
		new-file))
;; Breaking up a PDF from Supernote:1 ends here

;; Identifying paths
;; :PROPERTIES:
;; :CUSTOM_ID: svg-identifying-paths
;; :END:


;; [[file:Sacha.org::#svg-identifying-paths][Identifying paths:1]]
(defvar my-svg-auto-resize-timer nil)
;; based on image-mode
(defun my-svg-resize-with-window (window)
	(when (numberp image-auto-resize-on-window-resize)
    (when my-svg-auto-resize-timer
      (cancel-timer my-svg-auto-resize-timer))
    (setq my-svg-auto-resize-timer
          (run-with-idle-timer 1 nil
                               #'my-svg-fit-to-window window))))
(defun my-svg-fit-to-window (window)
	(when (window-live-p window)
    (with-current-buffer (window-buffer window)
      (let ((spec (get-text-property (point-min) 'display)))
        (when (eq (car-safe spec) 'image)
          (let* ((image-width  (plist-get (cdr spec) :max-width))
                 (image-height (plist-get (cdr spec) :max-height))
                 (edges (window-inside-pixel-edges window))
                 (window-width  (- (nth 2 edges) (nth 0 edges)))
                 (window-height (- (nth 3 edges) (nth 1 edges))))
            ;; If the size has been changed manually (with `+'/`-'),
            ;; then :max-width/:max-height is nil.  In that case, do
            ;; no automatic resizing.
            (when (and image-width image-height
                       ;; Don't do resizing if we have a manual
                       ;; rotation (from the `r' command), either.
                       (not (plist-get (cdr spec) :rotation))
                       (or (not (= image-width  window-width))
                           (not (= image-height window-height))))
              (unless image-fit-to-window-lock
                (unwind-protect
                    (progn
                      (setq-local image-fit-to-window-lock t)
                      (ignore-error remote-file-error
												(setcdr spec
																(plist-put
																 (plist-put (cdr spec) :max-width window-width)
																 :max-height window-height))
												(put-text-property (point-min) (1+ (point-min))
																					 'display spec)))
                  (setq image-fit-to-window-lock nil))))))))))

(defun my-svg-bounding-box (path)
	"Note: Relative paths don't work very well yet, so it's probably
better to set Inkscape's Preferences - Input/Output - SVG output
- Path string format - Absolute."
	(require 's)
	(let ((x1 most-positive-fixnum)
				(y1 most-positive-fixnum)
				(x2 most-negative-fixnum)
				(y2 most-negative-fixnum)
				(x 0)
				(y 0)
				(i 0))
		(dolist (seg (s-slice-at " *[MCmc] *" path))
			(unless (string= (string-trim seg) "")
				(setq seg (split-string seg "[ ,]") i 0)
				(let ((points (mapcar 'string-to-number (cdr seg))))
					(pcase (car seg)
						((or "m" "M")
						 (if (or (eq (car seg) "M") (= i 0))
								 ;; starting points are always absolute
								 (setq x (car points)
											 y (cadr points))
							 ;; m, so relative movement
							 (setq x (+ x (car points))
										 y (+ y (cadr points))))
						 (when (< x x1) (setq x1 x))
						 (when (< y y1) (setq y1 y))
						 (when (> x x2) (setq x2 x))
						 (when (> y y2) (setq y2 y)))
						("c"
						 (let ((old-x x) (old-y y))
							 (dolist (set (seq-partition points 6))
								 ;; relative movement? still very fuzzy on how this should work
								 (setq x (+ x (elt set 4))
											 y (+ y (elt set 5)))
								 (when (< x x1) (setq x1 x))
								 (when (< y y1) (setq y1 y))
								 (when (> x x2) (setq x2 x))
								 (when (> y y2) (setq y2 y))))
						 )
						("C"
						 (dolist (set (seq-partition points 2))
							 (setq x (elt set 0))
							 (setq y (elt set 1))
							 (when (and x y)
								 (when (< x x1) (setq x1 x))
								 (when (< y y1) (setq y1 y))
								 (when (> x x2) (setq x2 x))
								 (when (> y y2) (setq y2 y))))))
					(cl-incf i))))
		(list x1 y1 x2 y2)))
;; Identifying paths:1 ends here

;; [[file:Sacha.org::#svg-identifying-paths][Identifying paths:2]]
(ert-deftest my-svg-bounding-box ()
	(should (equal (my-svg-bounding-box "M 15.838959,27.678234 C 15.838959,27.678234 50.667557,45.01362 62.948412,30.731177 75.229269,16.448732 98.309577,20.617771 102.23147,26.236269")))
	(should
	 (equal (my-svg-bounding-box "M 1025.609375 852.070312 C 1025.660156 853.179688 1026.097656 854.332031 1026.914062 854.871094 C 1028.179688 855.707031 1033.238281 855.589844 1033.761719 854.746094 C 1034.320312 853.839844 1032.726562 851.054688 1031.199219 850.105469 C 1030.3125 849.554688 1029.003906 849.210938 1027.953125 849.207031 C 1027.144531 849.207031 1026.625 849.296875 1026.109375 849.976562 C 1025.710938 850.496094 1025.574219 851.332031 1025.609375 852.070312")
					'(1025.609375 849.207031 1033.761719 854.871094)))
	(should
	 (equal (my-svg-bounding-box "m 1160.0156,382.75391 c 0.3867,4.04296 1.2696,9.02343 1.1719,12.88281 -1.6953,1.875 -5.8711,0.25781 -8.3906,1.05469 -0.6055,0.26171 -0.9063,0.65234 -0.9063,1.28906 0,0.64844 0.2969,0.98047 0.8907,1.21094 2.5664,0.20703 5.1289,0.41406 7.6953,0.62109 1.3672,1 0.9218,4.21484 3.4453,4.29297 0.7344,0.0273 1.0742,-0.29688 1.2109,-0.88281 0.035,-1.375 -0.625,-2.5 0.457,-3.56641 2.9375,-1.20313 5.8711,-2.41016 8.8086,-3.61328 0.9727,-0.47656 1.793,-1.08203 1.7539,-2.0625 -0.035,-0.99219 -0.8789,-1.27344 -1.871,-1.17969 -2.9336,0.74219 -5.8672,1.48047 -8.7969,2.22266 -1.8281,-2.50782 -1.6758,-7.36328 -2.1953,-11.23828 -0.2813,-0.95704 -1.1446,-1.80469 -2.1875,-1.86719 -0.7305,-0.043 -0.9922,0.26953 -1.086,0.83594 m 11.9219,24.23828 c 0.7188,2.97656 1.4375,5.94922 2.1563,8.92187 -0.027,1.29297 -1.125,3.60156 -2.3438,4.05078 -1.1836,0.44141 -3.1602,-0.78515 -4.4961,-1.76172 -1.5625,-1.13671 -2.7851,-2.75781 -4.0351,-4.40234 -2.625,-2.01953 0.1328,-5.14844 -1.3594,-6.60156 -0.9766,-0.60938 -2.9571,0.32812 -3.1133,1.64844 -0.5391,1.83984 -0.3594,4.5 0.7695,6.35546 1.9532,2.94532 5.1953,6.72266 8.3203,7.9336 1.6993,0.57422 4.7149,0.65625 6.3125,0.19531 1.0039,-0.28906 1.4297,-0.96094 1.8633,-2.05078 0.8008,-1.99609 1.5196,-4.24609 1.375,-6.26953 -0.8554,-2.90625 -0.9883,-6.82031 -3.4179,-8.94922 -1.0157,-0.50781 -1.875,-0.0508 -2.0313,0.92969 m -13.3789,26.9375 c -0.078,1.33593 -0.1328,2.92187 0.293,4.17968 0.9453,1.51172 1.8867,3.02344 2.8281,4.53907 -0.6524,0.73828 -1.3086,1.47656 -1.9609,2.21484 -0.7305,2.76172 -0.875,9.38672 0.1484,12.29297 1.0859,2.86719 4.3516,4.23047 7.0312,5.91016 1.9375,0.79296 4.3946,0.40234 6.3516,-0.21485 0.6641,-0.21094 1.2969,-0.46875 1.6484,-0.96484 0.5274,-0.7461 0.5274,-2.09766 -0.027,-2.64844 -1.9102,0.008 -3.8203,0.0156 -5.7305,0.0273 -1.7773,-0.49218 -4.207,-1.9414 -5.6484,-3.60156 -1.8672,-2.39453 -0.8125,-5.0625 -0.9766,-7.5625 0.1758,-1.18359 0.8164,-2.70703 1.8867,-3.11328 2.5977,0.14844 5.1915,0.29688 7.7891,0.44531 1.0625,-0.0664 1.918,-0.27734 2.8945,-1.19531 1.2657,-1.19531 2.086,-2.81641 2.3008,-4.16406 0.3164,-2 0.1094,-4.34375 -0.5312,-6.33203 -0.2149,-0.66016 -0.4805,-1.29297 -1.0157,-1.63282 -0.4882,-0.30859 -1.1914,-0.30078 -1.6093,0.0156 -1.4844,1.51562 0.1953,4.54687 -0.2383,6.68359 -0.2969,0.9375 -1.3047,1.9961 -2.2344,2.72266 -0.9765,0.76562 -1.7734,1.05469 -2.7187,0.95703 -1.461,-0.14844 -3.1953,-1.41797 -4.5274,-2.86328 -1.2578,-1.37109 -2.5078,-3.19922 -2.7187,-4.59375 -0.1289,-0.86719 0.2734,-1.10938 1.1289,-0.38672 1.3867,1.78125 2.7695,3.55859 4.1562,5.33594 0.586,0.28515 1.2813,0.2539 1.7071,-0.125 0.6796,-0.60547 0.6523,-1.85156 0.25,-2.94922 -0.6368,-1.73828 -2.043,-3.77734 -3.1602,-5.26953 -0.7656,-1.01953 -1.668,-1.77344 -2.8086,-1.94922 -0.6992,-0.10938 -1.5234,0.004 -2.2461,0.37891 -1.6445,0.85937 -2.1758,2.46093 -2.2617,3.86328 m -44.8516,12.89843 c -0.1562,7.03125 -0.1875,14.48047 0.1016,21.36719 0.2305,0.60938 0.5703,0.91016 1.1914,0.91406 0.625,0 0.9648,-0.30078 1.1953,-0.89843 0.6914,-3.53125 -0.582,-10 0.8906,-11.95313 4.9375,6.73438 15.668,16.79688 20.3321,24.84766 -1.0469,9.58203 -3.8399,19.17187 -6.2578,28.75 -1.8321,3.38672 -3.668,6.77344 -5.5039,10.16015 -0.1485,1.13672 0.3281,2.05469 1.3789,2.11329 1.0625,0.0586 2.0625,-0.78516 2.8046,-1.76954 1.8125,-2.41406 3.2461,-5.60937 4.129,-8.1914 2.9101,-11.14063 5.621,-21.85156 7.3515,-33.25781 -3.9726,-6.83594 -13.1719,-14.88672 -17.6406,-20.35938 -1.8203,-2.29297 -6.4102,-8.75 -6.3594,-9.76953 0.035,-0.78906 2.4805,-1.89844 3.8164,-2.04688 1.668,0.19141 3.3321,0.38672 5,0.57813 0.875,-0.26563 1.3047,-1.26953 0.7383,-2.34766 -0.3984,-0.7539 -1.0117,-1.07031 -1.7031,-1.26562 -2.0547,-0.57031 -5.2188,-0.38281 -7.2813,-0.0703 -1.6797,0.16015 -3.9687,1.58203 -4.1836,3.19921 m 35.4766,21.35547 c -0.668,0.67188 -0.7461,2.96485 0.039,3.65625 0.6523,0.56641 1.9531,0.3086 2.9531,-0.67578 0.9961,-0.98437 1.2695,-2.28515 0.6836,-2.9414 -0.7071,-0.79297 -3.0117,-0.70313 -3.6758,-0.0391 m 25.8633,-0.39062 c -2.7031,1.03906 -5.4024,2.07812 -8.1055,3.11719 -1.3398,-0.0742 -2.6836,-0.14844 -4.0234,-0.22266 -0.9102,0.23047 -1.3477,1.27734 -0.7813,2.34766 0.3946,0.75 1.0274,1.08203 1.7227,1.26953 1.3515,0.36328 2.9023,0.0469 4.2109,-0.27344 2.4883,-0.60547 6.1172,-1.4375 8.1797,-2.63281 0.7969,-0.46094 1.2578,-1.35938 1,-2.41016 -0.2578,-1.05469 -1.0547,-1.3125 -2.2031,-1.19531 m 0.2304,28.30078 c 0.4258,1.11719 -0.2382,2.55078 -1.375,2.75781 -1.871,-0.043 -4.7148,-3.05078 -6.0546,-5.01562 -0.4727,-0.92188 -0.4532,-1.77344 -0.012,-2.64063 0.4454,-0.87109 1.3633,-1.84765 2.0664,-1.92187 1.8711,0.53906 4.0547,4.24218 5.375,6.82031 m 3.0899,-2.16406 c -1.0859,-2.19141 -2.168,-4.38282 -3.25,-6.57422 1.2812,-0.79688 2.5586,-1.59375 3.8398,-2.39063 0.6172,-0.96093 0.6602,-3.09765 -0.1601,-3.80468 -2.2735,-1.32813 -4.2344,3.59765 -6.8633,3.10546 -3.6523,-0.54296 -7.3047,-1.08203 -10.957,-1.625 -2.8828,0.15625 -6.6953,-0.55468 -8.8477,0.5586 -0.6953,0.88281 -0.4726,2.82031 0.6484,3.00781 3.2657,0.89844 7.7657,1.15234 10.7071,1.50391 0.6289,0.41797 0.2226,6.12109 1.4258,8.48437 1.0195,1.99219 2.8632,3.76563 4.8945,5.17969 1.4844,1.03516 2.7617,1.15234 4.2695,1.03516 1.3711,-0.10547 3.086,-0.37891 3.8164,-1.3711 0.9766,-1.32812 0.7188,-5.28125 0.4766,-7.10937 M 1167,513.47266 1167.5273,514 1167,514.52734 1166.4727,514 1167,513.47266 m 10.8203,-7.64844 c 0.3906,2.33594 0.7774,4.66797 1.1641,7.0039 -0.4024,1.29297 -2.8242,3.76172 -4.0078,4.0625 -0.8868,0.22657 -1.586,-0.41796 -2.3125,-1.30468 -1.5469,-2.1836 -3.0938,-4.3711 -4.6407,-6.55469 -0.875,-0.5 -2.0898,-0.54297 -3.1992,0.0352 -1.1719,0.60937 -1.8789,1.70703 -2.1406,2.83203 -0.8633,2.57812 1.2852,4.94922 2.1484,7.125 -0.4062,1.29687 -0.8086,2.59375 -1.2148,3.89062 -0.3281,2.24219 -0.2422,4.94922 0.3203,7.21875 0.4297,1.72656 1.2578,3.50391 2.5195,5.2461 0.7696,1.0625 1.4141,1.71875 2.4258,1.92187 2.5938,0.52344 7.75,-0.74609 10.3945,-1.55078 1.0547,-0.32422 1.7735,-0.68359 1.9766,-1.78516 0.1992,-1.08984 -0.2422,-1.89843 -1.0703,-2.01953 -2.9961,0.375 -5.9961,0.75391 -8.9961,1.12891 -2.207,-1.27735 -4.4453,-4.15235 -4.6523,-6.15235 -0.086,-1.98828 0.4921,-4.85937 1.9531,-5.94531 2.5547,0.0547 5.1133,0.10938 7.6719,0.16406 1.5898,-0.55468 3.7968,-2.25 4.9414,-3.92187 1.125,-1.64063 1.375,-3.51953 1.2812,-5.1875 -0.3476,-2.22266 -0.8398,-5.41016 -2.5117,-6.94922 -0.9102,-0.53125 -1.8203,-0.11328 -2.0508,0.74219")
					nil 													; don't know what
					)))

(defun my-svg-display (buffer-name svg &optional highlight-id full-window)
	"HIGHLIGHT-ID is a string ID or a node."
	(with-current-buffer (get-buffer-create buffer-name)
		(when highlight-id
			;; make a copy
			(setq svg (with-temp-buffer (svg-print svg) (car (xml-parse-region (point-min) (point-max)))))
			(if-let* ((path (if (stringp highlight-id) (dom-by-id svg highlight-id) highlight-id))
								(view-box (split-string (dom-attr svg 'viewBox)))
								(box (my-svg-bounding-box (dom-attr path 'd)))
								(parent (car path)))
					(progn
						;; find parents for possible rotation
						(while (and parent (not (dom-attr parent 'transform)))
							(setq parent (dom-parent svg parent)))
						(dom-set-attribute path 'style
															 (concat (dom-attr path 'style) "; stroke: 1px red; fill: #ff0000 !important"))
						;; add a crosshair
						(dom-append-child
						 (or parent svg)
						 (dom-node 'path
											 `((d .
														,(format "M %f,0 V %s M %f,0 V %s M 0,%f H %s M 0,%f H %s"
																		 (elt box 0)
																		 (elt view-box 3)
																		 (elt box 2)
																		 (elt view-box 3)
																		 (elt box 1)
																		 (elt view-box 2)
																		 (elt box 3)
																		 (elt view-box 2)))
												 (stroke-dasharray . "5,5")
												 (style . "fill:none;stroke:gray;stroke-width:3px")))))
				(error "Could not find %s" highlight-id)))
		(let* ((inhibit-read-only t)
					 (image (svg-image svg))
					 (edges (window-inside-pixel-edges (get-buffer-window))))
			(erase-buffer)
			(if full-window
					(progn
						(delete-other-windows)
						(switch-to-buffer (current-buffer)))
				(display-buffer (current-buffer)))
			(insert-image (append image
														(list :max-width
																	(floor (* 0.8 (- (nth 2 edges) (nth 0 edges))))
																	:max-height
																	(floor (* 0.8 (- (nth 3 edges) (nth 1 edges)))) )))
			;; (my-svg-resize-with-window (selected-window))
			;; (add-hook 'window-state-change-functions #'my-svg-resize-with-window t)
			(current-buffer))))

(defun my-svg-identify-paths (filename &optional selector)
	"Prompt for IDs for each path in FILENAME."
	(interactive (list (read-file-name "SVG: " nil nil
																		 (lambda (f) (string-match "\\.svg$" f)))))
	(let* ((dom (car (xml-parse-file filename)))
				 (paths (if (functionp selector) (dom-search dom selector)
									(or selector
											(dom-by-tag dom 'path))))
				 (vertico-count 3)
				 (ids (seq-keep (lambda (path)
													(and (dom-attr path 'id)
															 (unless (string-match "path[0-9]+" (or (dom-attr path 'id) "path0"))
																 (dom-attr path 'id))))
												paths))
				 (edges (window-inside-pixel-edges (get-buffer-window)))
				 id)
		(my-svg-display "*image*" dom nil t)
		(dolist (path paths)
			(when (string-match "path[0-9]+" (or (dom-attr path 'id) "path0"))
				;; display the image with an outline
				(unwind-protect
						(progn
							(my-svg-display "*image*" dom path t)
							(setq id (completing-read
												(format "ID (%s): " (dom-attr path 'id))
												ids))
							;; already exists, merge with existing element
							(if-let ((old (dom-by-id dom id)))
									(progn
										(dom-set-attribute
										 old
										 'd
										 (concat (dom-attr (dom-by-id dom id) 'd)
														 " "
														 ;; change relative to absolute
														 (replace-regexp-in-string "^m" "M"
																											 (dom-attr path 'd))))
										(dom-remove-node dom path)
										(setq id nil))
								(dom-set-attribute path 'id id)
								(add-to-list 'ids id))))
				;; save the image just in case we get interrupted halfway through
				(with-temp-file filename
					(svg-print dom))))))
;; Identifying paths:2 ends here

;; Sorting paths
;; :PROPERTIES:
;; :CUSTOM_ID: svg-sorting-paths
;; :END:


;; [[file:Sacha.org::#svg-sorting-paths][Sorting paths:1]]
(defun my-svg-reorder-paths (filename &optional ids output-filename)
	"Sort paths in FILENAME."
	(interactive (list (read-file-name "SVG: " nil nil (lambda (f) (string-match "\\.svg$" f)))
										 nil (read-file-name "Output: ")))
	(let* ((dom (car (xml-parse-file filename)))
				 (paths (dom-by-tag dom 'path))
				 (parent (dom-parent dom (car paths)))
				 (ids-left
					(nreverse (seq-keep (lambda (path)
																(unless (string-match "path[0-9]+" (or (dom-attr path 'id) "path0"))
																	(dom-attr path 'id)))
															paths)))
				 list)
		(when (called-interactively-p)
			(while ids-left
				(my-svg-display "*image*" dom (car ids-left))
				(let ((current (completing-read
												(format "ID (%s): "
																(car ids-left))
												ids-left nil nil nil nil (car ids-left)))
							node)
					(add-to-list 'ids current)
					(setq ids-left (seq-remove (lambda (o) (string= o current)) ids-left)))))
		(if ids ;; reorganize under the first path's parent
				(progn
					(dolist (id ids)
						(if-let ((node (car (dom-by-id dom id))))
								(progn
									(dom-remove-node dom node)
									(dom-append-child parent node))
							(message "Could not find %s" id)))
					(with-temp-file (or output-filename filename)
						(svg-print dom))))
		(nreverse (seq-keep (lambda (path)
													(unless (string-match "path[0-9]+" (or (dom-attr path 'id) "path0"))
														(dom-attr path 'id)))
												(dom-by-tag dom 'path)))))
;; Sorting paths:1 ends here

;; Animating paths in order
;; :PROPERTIES:
;; :CUSTOM_ID: svg-animating-paths-in-order
;; :END:


;; [[file:Sacha.org::#svg-animating-paths-in-order][Animating paths in order:1]]
(defun my-animate-svg-paths (filename output-dir)
	"Add one path at a time. Save the resulting SVGs to OUTPUT-DIR."
	(unless (file-directory-p output-dir)
		(make-directory output-dir t))
	(let* ((dom (xml-parse-file filename))
				 (paths (seq-filter (lambda (e) (dom-attr e 'style))
														(dom-by-tag dom 'path)))
				 (total (length paths))
				 (frame-num (length paths))
				 result)
		(dolist (elem paths)
			(dom-set-attribute elem 'style
												 (concat
													(dom-attr elem 'style)
													";mix-blend-mode:darken")))
		(with-temp-file (expand-file-name (format "frame-%03d.svg" (1+ frame-num)) output-dir)
			(xml-print dom))
		(dolist (elem paths)
			(dom-set-attribute elem 'style
												 (concat
													(dom-attr elem 'style)
													";fill-opacity:0")))
		(dolist (elem paths)
			(with-temp-file (expand-file-name
											 (format "frame-%03d.svg"
															 (- total frame-num))
											 output-dir)
				(message "%03d" frame-num)
				(dom-set-attribute elem 'style
													 (concat (dom-attr elem 'style)
																	 ";fill-opacity:1"))
				(push (list (format "frame-%03d.svg"
														(1+ (- total frame-num)))
										(dom-attr elem 'id))
							result)
				(setq frame-num (1- frame-num))
				(xml-print dom)))
		(reverse result)))
;; Animating paths in order:1 ends here



;; for FILE in *.svg; do inkscape --export-type=png --export-dpi=96 --export-background-opacity=1 $FILE; done

;; One image per second
;; - ffmpeg -i frame-%03d.svg.png -vf palettegen palette.png
;; - ffmpeg -f image2 -framerate 1 -i frame-%03d.svg.png -loop -1 animation.gif
;; - ffmpeg -framerate 1 -i frame-%03d.svg.png -i palette.png -lavfi "paletteuse" -loop -1 animation.gif


;; [[file:Sacha.org::#svg-animating-paths-in-order][Animating paths in order:3]]
(defun my-ffmpeg-animate-images (files output-file &optional framerate)
	"Make an animated GIF or WEBM out of FILES.
Save it to OUTPUT-FILE.
If FRAMERATE is specified, use that instead of 30."
	(setq framerate (or framerate 30))
	(if (string-match "\\.webm$" output-file)
			(let ((compile-media-ffmpeg-arguments
						 (append compile-media-ffmpeg-arguments
										 (list "-r"
													 (number-to-string framerate)))))
				(compile-media `((video ,@(mapcar (lambda (o) (list :source o :duration-ms (/ 1000.0 framerate)
																														:before-input
																														(list "-width" compile-media-output-video-width)))
																					files)))
											 output-file))
		(with-current-buffer (get-buffer-create "*gif*")
			(erase-buffer)
			(let ((frame-input (seq-mapcat (lambda (o) (list "-i" o)) files))
						(palette (make-temp-file "palette" nil ".png")))
				(insert "ffmpeg "
								(string-join (append frame-input (list "-vf" "palettegen" "-y" palette)) " ")
								"\n")
				(apply #'call-process "ffmpeg" nil t t
							 (append frame-input (list "-vf" "palettegen" "-y" palette)))
				(insert "ffmpeg "
								(string-join (append (list "-i" palette "-lavfi" "paletteuse")
																		 (list "-framerate" (number-to-string framerate))
																		 frame-input
																		 (list "-loop" "-1" "-y" output-file)) " ")
								"\n")
				(apply #'call-process "ffmpeg" nil t t
							 (append (list "-i" palette "-lavfi" "paletteuse")
											 (list "-framerate" (number-to-string framerate))
											 frame-input
											 (list "-loop" "-1" "-y" output-file)))
				(delete-file palette))
			(display-buffer (current-buffer))))
	output-file)
;; Animating paths in order:3 ends here

;; RevealJS CSS animation of sketches
;; :PROPERTIES:
;; :CUSTOM_ID: reveal-js-sketch-animation
;; :END:

;; #+NAME: revealjs-css-animation-code

;; [[file:Sacha.org::revealjs-css-animation-code][revealjs-css-animation-code]]
(defun my-reveal-svg-animation (slide)
	(string-join
	 (seq-map-indexed
		(lambda (step-ids i)
			(format "%s { fill: #f6f396; transition: fill %ds; transition-delay: %ds }"
							(mapconcat
							 (lambda (id) (format "#slide-%s.present #%s" (car slide) id))
							 (split-string step-ids ",")
							 ", ")
							highlight-duration
							(* i highlight-duration)))
		(split-string (elt slide 1) ";"))
	 "\n"))

(defun my-reveal-svg-highlight-different-colors (slide)
	(let* ((colors '("#f6f396" "#c6c6c6")) ; reverse
				 (steps (split-string (elt slide 1) ";"))
				 (step-length 0.5))
		(string-join
	 	 (seq-map-indexed
			(lambda (step-ids i)
				(format "%s { fill: %s; opacity: 1 !important; transition: fill %.1fs; transition-delay: %.1fs }"
								(mapconcat
								 (lambda (id) (format "#slide-%s.present #%s" (car slide) id))
								 (split-string step-ids ",")
								 ", ")
								(elt colors (- (length steps) i 1))
								step-length
								(* i 0.5)))
			steps))))

(defun my-reveal-svg-progression-css (map-progression &optional highlight-duration)
	"Make the CSS.
map-progression should be a list of lists with the following format:
((\"slide-id\" \"prev1,prev2;cur1\" \"id-to-add1,id-to-add2\") ...)."
	(setq highlight-duration (or highlight-duration 2))
	(let (full)
		(format
		 "<style>%s</style>"
		 (mapconcat
			(lambda (slide)
				(setq full (append (split-string (elt slide 2) ",") full))
				(format "#slide-%s.present path { opacity: 0.2 }
%s { opacity: 1 !important }
%s"
								(car slide)
								(mapconcat (lambda (id) (format "#slide-%s.present #%s" (car slide) id))
													 full
													 ", ")
								(my-reveal-svg-highlight-different-colors slide)))
			map-progression
			"\n"))))
;; revealjs-css-animation-code ends here

;; Finding sketches                                                 :image:
;; :PROPERTIES:
;; :CUSTOM_ID: finding-sketches
;; :END:


;; [[file:Sacha.org::#finding-sketches][Finding sketches:1]]
(defvar my-sketch-directories
  '("~/sync/sketches"
    "~/sync/private-sketches"))

(defun my-get-sketch-filenames-between-dates (start end filter)
  "Returns index card filenames between START and END."
  (setq start (replace-regexp-in-string "[^0-9]" "" start))
  (setq end (replace-regexp-in-string "[^0-9]" "" end))
  (my-get-sketch-filenames
   (lambda (filename)
     (let ((f (replace-regexp-in-string "[^0-9]" "" (file-name-nondirectory filename))))
       (and (string> f start)
            (string> end f)
            (or (not filter) (string-match filter filename)))))))

(defun my-get-sketch-filenames (base &optional as-regexp)
  (my-get-image-filenames base as-regexp my-sketch-directories))
(defun my-get-image-filenames (base &optional as-regexp directories)
  "Check several directories for files matching BASE.
           Return the matching filenames, if any.
           If AS-REGEXP is non-nil, treat BASE as a regular expression.
           If BASE is a function, use that to filter."
	(when (and (stringp base) (string-match "^[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]-[0-9][0-9]" base))
		(setq base (match-string 0 base)))
  (let ((base-regexp (unless (functionp base)
                       (concat
                        "\\("
                        (if as-regexp base (regexp-quote base))
                        "\\)"
                        ".*\\(\\.\\(png\\|psd\\|tiff\\|jpg\\|svg\\)\\)$"))))
    (-filter
     (lambda (o) (not (string-match "\\.xmp" o)))
     (sort (-flatten
            (delq nil
                  (mapcar
                   (lambda (dir)
                     (and (file-directory-p dir)
                          (if (functionp base)
                              (-filter base (directory-files dir t ".*\\.\\(png\\|psd\\|tiff\\|jpg\\|svg\\)?$"))
                            (directory-files
                             dir t
                             base-regexp))))
                   (or directories my-image-directories))))
           'string<))))

(defun my-get-image-filename (base &optional as-regexp directories)
  "Check several directories for files matching BASE.
Return the first matching filename, if any.
If AS-REGEXP is non-nil, treat BASE as a regular expression."
  (if (file-exists-p base)
      base
    (car (my-get-image-filenames base as-regexp directories))))
(defun my-get-sketch-filename (base &optional as-regexp)
  (my-get-image-filename base as-regexp my-sketch-directories))

(defun my-list-sketches (regexp &optional full-filename directories)
  "Return a list of sketch filenames matching REGEXP."
  (interactive (list (read-string "Filter: ")))
  (let ((my-sketch-directories (or directories my-sketch-directories)))
    (funcall (if (called-interactively-p 'interactive)
                 (lambda (x) (insert (mapconcat (lambda (y) (concat "- " (org-link-make-string (concat "sketchLink:" y)))) x "\n"))) 'identity)
             (sort (-uniq
                    (mapcar (if full-filename 'identity
                              'file-name-nondirectory)
                            (my-get-sketch-filenames regexp t)))
                   'string>))))
;; Finding sketches:1 ends here

;; Renaming and recoloring sketches                                 :image:
;; :PROPERTIES:
;; :CUSTOM_ID: sketch-rename-recolor
;; :END:
;; [2024-02-07 Wed]


;; [[file:Sacha.org::#sketch-rename-recolor][Renaming and recoloring sketches:1]]
(defun my-sketch-rename (file)
	(interactive "FFile: ")
	(let ((data (my-image-recognize file))
				new-id)
		(when (string-match "[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]-[0-9][0-9]" data)
			(setq new-id (match-string 0 data))
			(rename-file file
									 (expand-file-name
										(concat new-id "." (file-name-extension file))
										(file-name-directory file)))
			(when (file-exists-p (concat (file-name-sans-extension file) ".json"))
				(rename-file (concat (file-name-sans-extension file) ".json")
										 (expand-file-name
											(concat new-id ".json")
											(file-name-directory file)))))))
(defun my-sketch-recolor-png (file &optional color-scheme)
	(interactive (list (read-file-name "File: ")
										 (completing-read "Scheme: " (mapcar (lambda (o) (symbol-name (car o)))
																												 my-sketch-color-map))))
	(setq color-scheme (or color-scheme 't))
	(call-process "/home/sacha/bin/recolor.py" nil nil nil
								"--colors"
								(mapconcat
								 (lambda (row)
									 (concat (car row) "," (cdr row)))
								 (assoc-default (if (stringp color-scheme)
																		(intern color-scheme)
																	color-scheme)
																my-sketch-color-map)
								 ",")
								(expand-file-name file)))
(defalias 'my-image-recolor 'my-sketch-recolor-png)
;; Renaming and recoloring sketches:1 ends here

;; Org Mode sketch: links                                           :image:
;; :PROPERTIES:
;; :CUSTOM_ID: org-mode-sketch-links
;; :END:

;; #+NAME: org-sketch-link

;; [[file:Sacha.org::org-sketch-link][org-sketch-link]]
(defun my-open-images-in-krita (files)
  (apply 'call-process "krita" nil 0 nil "--nosplash" files))
(defun my-open-images-in-gwenview (files)
  (apply 'call-process "gwenview" nil 0 nil "--slideshow" files))
(defun my-open-images-in-feh (files)
  (apply 'call-process "feh" nil nil nil "-D" "1" "-F" files))
(defun my-org-image-open (id &optional arg directories)
  "Open image named ID.
      If ARG is specified, prompt for application to open it in."
  (interactive (list
                (completing-read "Sketch ID: " (my-list-sketches "."))
                (current-prefix-arg)))
  (let* ((files (mapcar (lambda (o) (my-get-image-filename o (or my-image-directories))) (if (listp id) id (list id))))
         (input (if arg (read-char "(k)rita, (g)wenview, (f)eh: ") ?k)))
    (funcall
     (cond
      ((eq input ?g) 'my-open-images-in-gwenview)
      ((eq input ?f) 'my-open-images-in-feh)
      (t 'my-open-images-in-krita))
     files)))
(defun my-org-sketch-edit (id &optional arg)
  (my-org-image-open id arg my-sketch-directories))
(defun my-org-sketch-open (id &optional arg)
  (delete-other-windows)
  (with-selected-window (split-window-right)
    (find-file (my-get-image-filename
								id my-sketch-directories))))
(defun my-org-image-export (link description format info)
  (let* ((path (concat "https://sketches.sachachua.com/filename/" link))
         (image (concat "https://sketches.sachachua.com/static/" link))
         (backend (org-export-backend-name (plist-get info :back-end)))
         (desc (or description link)))
    (cond
     ((eq backend '11ty) (format "{%% sketchLink \"%s\", \"%s\" %%}" link desc))
     ((or (eq format 'html) (eq format 'wp))
      (if description
          (format "<a target=\"_blank\" href=\"%s\">%s</a>" path desc)
        (format "<div style=\"text-align: center\"><a target=\"_blank\" href=\"%s\"><img src=\"%s\" style=\"max-height: 90vw; height: auto; width: auto\"><br />%s</a></div>" path image desc)))
     ((eq format 'latex) (format "\\href{%s}{%s}" path desc))
     ((eq format 'texinfo) (format "@uref{%s,%s}" path desc))
     ((eq format 'md)
      (if (file-exists-p (expand-file-name link "~/sketches"))
          (format "{{<photo src=\"%s\">}}" image)
        (format "{{<photo nas=\"1\" src=\"%s\">}}" link)))
     ((eq format 'ascii) (format "%s <%s>" desc path))
     (t path))))

(defun my-org-image-export-link (link description format info)
  (let* ((backend (if (plist-get info :backend) (org-export-backend-name (plist-get info :back-end))
                    format))
         (desc (or description link)))
    (cond ((eq backend 'md)
           (format "[%s](%s)" desc link))
          ((eq backend '11ty)
           (format "{%% sketchLink \"%s\", \"%s\" %%}" (file-name-base link) desc))
          ((eq backend 'html)
           (format "<a href=\"https://sketches.sachachua.com/filename/%s\">%s</a>" (file-name-nondirectory link) desc))
          (t (format "[[%s][%s]]" link desc)))))

(defun my-org-image-export-thumb (link description format info)
  (let* ((path (concat "https://sketches.sachachua.com/filename/" link))
         (image (concat "https://sketches.sachachua.com/static/" link))
         (backend (org-export-backend-name (plist-get info :back-end)))
         (desc (replace-regexp-in-string "%23" "#" (or description link))))
    (cond
     ((eq backend '11ty) (format "{%% sketchThumb \"%s\", \"%s\" %%}" (file-name-base link) desc))
     ((or (eq format 'html) (eq format 'wp))
      (if description
          (format "<a target=\"_blank\" href=\"%s\">%s</a>" path desc)
        (format "<div class=\"sketch-thumbnail\"><a target=\"_blank\" href=\"%s\"><img src=\"%s\"><br />%s</a></div>" path image desc)))
     ((eq format 'latex) (format "\\href{%s}{%s}" path desc))
     ((eq format 'texinfo) (format "@uref{%s,%s}" path desc))
     ((eq format 'md)
      (if (file-exists-p (expand-file-name link "~/sketches"))
          (format "{{<photo src=\"%s\">}}" image)
        (format "{{<photo nas=\"1\" src=\"%s\">}}" link)))
     ((eq format 'ascii) (format "%s <%s>" desc path))
     (t path))))

(defun my-org-image-export-full (link description format info)
  (let* ((path (concat "https://sketches.sachachua.com/filename/" link))
         (image (concat "https://sketches.sachachua.com/static/" link))
         (backend (org-export-backend-name (plist-get info :back-end)))
         (desc (or description link)))
    (cond
     ((eq backend '11ty) (format "{%% sketchFull \"%s\", \"%s\" %%}" link desc))
     ((or (eq format 'html) (eq format 'wp))
      (if description
          (format "<figure><a target=\"_blank\" href=\"%s\"><img src=\"%s\" /><br /></a><figcaption>%s</figcaption></figure>" path image desc)
        (format "<figure><a target=\"_blank\" href=\"%s\"><img src=\"%s\" /><br /><figcaption>%s</figcaption></a></figure>" path image desc)))
     ((eq format 'latex) (format "\\href{%s}{%s}" path desc))
     ((eq format 'texinfo) (format "@uref{%s,%s}" path desc))
     ((eq format 'md)
      (if (file-exists-p (expand-file-name link "~/sketches"))
          (format "{{<photo src=\"%s\">}}" image)
        (format "{{<photo nas=\"1\" src=\"%s\">}}" link)))
     ((eq format 'ascii) (format "%s <%s>" desc path))
     (t path))))

(defun my-org-sketch-complete (&optional prefix)
  (concat "sketch:" (file-name-nondirectory (my-complete-sketch-filename))))
(defun my-org-sketch-complete-full (&optional prefix)
  (concat "sketchFull:" (file-name-nondirectory (my-complete-sketch-filename))))
(defun my-org-image-complete (&optional prefix)
  (concat "image:"
          (completing-read "Image: " (my-list-sketches "." nil my-image-directories))))
;; Based on https://emacs.stackexchange.com/questions/38098/org-mode-custom-youtube-link-syntax
(defun my-org-sketch-preview (start end path bracketp)
  "Include overlays for sketches."
  (when (display-graphic-p)
    (let ((filename (my-get-sketch-filename path))
          (refresh nil)
          (link (save-excursion
                  (goto-char start)
                  (org-element-lineage
                   (save-match-data (org-element-context))
                   '(link) t)))) ;; set this someday
      (when (and (not (org-element-property :contents-begin link)) filename)
        (let ((width
               ;; Apply `org-image-actual-width' specifications.
               (cond
                ((not (image-type-available-p 'imagemagick)) nil)
                ((eq org-image-actual-width t) nil)
                ((numberp org-image-actual-width) org-image-actual-width)
                ;; Pick this up from the paragraph someday
                ))
              (old (get-char-property-and-overlay start 'org-image-overlay)))
          (if (and (car-safe old) refresh)
              (image-refresh (overlay-get (cdr old) 'display))
            (let ((image (create-image filename
                                       (and width 'imagemagick)
                                       nil
                                       :width width)))
              (when image
                (let* ((ov (make-overlay start end)))
                  (overlay-put ov 'display image)
                  (overlay-put ov 'face 'default)
                  (overlay-put ov 'org-image-overlay t)
                  (overlay-put
                   ov 'modification-hooks
                   (list 'org-display-inline-remove-overlay))
                  (push ov org-inline-image-overlays))))))))))

(use-package org
  :config
  (setq org-image-actual-width 600)
  (org-link-set-parameters
   "sketch"
   :follow 'my-org-sketch-open
   :export 'my-org-image-export-link
   :complete 'my-org-sketch-complete
   :activate-func nil)
  (org-link-set-parameters
   "sketchLink"
   :follow 'my-org-sketch-open
   :export 'my-org-image-export-link
   :complete 'my-org-sketch-complete
   :activate-func nil)
  (org-link-set-parameters
   "sketchThumb"
   :follow 'my-org-sketch-open
   :export 'my-org-image-export-thumb
   :complete 'my-org-sketch-complete
   :activate-func nil)
  (org-link-set-parameters
   "sketchFull"
   :follow 'my-org-sketch-open
   :export 'my-org-image-export-full
   :complete 'my-org-sketch-complete-full
   :activate-func nil))

(use-package org
  :config
  (setq org-image-actual-width 600)
  (org-link-set-parameters
   "image"
   :follow 'my-org-image-open
   :export 'my-org-image-export
   :complete 'my-org-image-complete))
;; org-sketch-link ends here

;; Org Mode custom link: copy to clipboard                      :emacs:org:
;; :PROPERTIES:
;; :CUSTOM_ID: org-mode-copy
;; :EXPORT_DATE: 2024-01-16T07:57:50-0500
;; :EXPORT_ELEVENTY_PERMALINK: /blog/2024/01/org-mode-custom-link-copy-to-clipboard/
;; :EXPORT_ELEVENTY_FILE_NAME: blog/2024/01/org-mode-custom-link-copy-to-clipboard/
;; :END:

;; I have a tiny corporation for my consulting. I do
;; all of my own paperwork. I have lots of notes in
;; Org Mode for infrequent tasks like the tax-related
;; paperwork I do once a year. My notes include
;; checklists, links, and Org Babel blocks for
;; calculations. I often need to copy standard text
;; (ex: the name of the company) or parts of the
;; output of my Org Babel blocks (ex: tax collected)
;; so that I can fill in web forms on the Canada
;; Revenue Agency website.

;; This little snippet makes it easy to copy text for
;; pasting. It defines a custom Org link that starts
;; with ~copy:~. When I follow the link by clicking
;; on it or using ~C-c C-o~ (~org-open-at-point~), it
;; copies the text to the kill ring (which is what
;; Emacs calls the clipboard) so that I can paste it
;; anywhere. For example, ~[[copy:Hello world]]~
;; becomes a link to copy "Hello world". Copying
;; means never having to worry about typos or
;; accidentally selecting only part of the text.

;; #+NAME: org-copy-link

;; [[file:Sacha.org::org-copy-link][org-copy-link]]
(use-package org
  :config
  (org-link-set-parameters
   "copy"
   :follow (lambda (link) (kill-new link))
	 :export (lambda (_ desc &rest _) desc)))
;; org-copy-link ends here



;; I can use these links as part of my checklist so
;; that I can quickly fill in things like my business
;; name and other details. I can put sensitive
;; information like my social insurance number in a
;; GPG-encrypted file. (Just set up your GPG keys and
;; end a filename with ~.gpg~, and Emacs will take
;; care of transparently encrypting and decrypting
;; the file.)

;; I can also export those links as part of my Org
;; Babel output. For example, the following code
;; calculates the numbers I need to fill in a T5 form
;; for the other-than-eligible dividends that I issue
;; myself according to the [[https://www.canada.ca/en/revenue-agency/services/tax/businesses/topics/completing-slips-summaries/financial-slips-summaries/return-investment-income-t5/t5-slip/completing-t5-slip.html][T5 instructions from the CRA]].


;; [[file:Sacha.org::#org-mode-copy][Org Mode custom link: copy to clipboard:2]]
(let* ((box-10 1234) ; fake number for demo
       (box-11 (* 1.15 box-10))
       (box-12 (* 0.090301 box-11)))
  `((box-10 ,(format "[[copy:%.2f][%.2f]]" box-10 box-10))
    (box-11 ,(format "[[copy:%.2f][%.2f]]" box-11 box-11))
    (box-12 ,(format "[[copy:%.2f][%.2f]]" box-12 box-12))))
;; Org Mode custom link: copy to clipboard:2 ends here

;; Config
;; :PROPERTIES:
;; :CUSTOM_ID: config
;; :END:

;; #+NAME: org-config-link

;; [[file:Sacha.org::org-config-link][org-config-link]]
(use-package org
  :config
  (org-link-set-parameters
   "config"
   :follow (lambda (id) (org-open-link-from-string (format "[[~/sync/emacs/Sacha.org::%s]]" id)))
   :export (lambda (link description format)
             (format "<a href=\"https://sachachua.com/dotemacs#%s\">%s</a>" link description))))
;; org-config-link ends here

;; Helm completion with my-helm-org-sketches
;; :PROPERTIES:
;; :CUSTOM_ID: helm-completion-with-my-helm-org-sketches
;; :END:


;; [[file:Sacha.org::#helm-completion-with-my-helm-org-sketches][Helm completion with my-helm-org-sketches:1]]
(defun my-helm-source-org-sketch-list ()
  (my-list-sketches "."))

(defun my-helm-org-insert-sketch-candidates (&optional candidates)
  (mapc (lambda (o)
          (org-insert-link nil (concat "sketch:" o))
          (insert "\n"))
        (helm-marked-candidates)))

(defun my-helm-open-sketches-in-krita (&optional candidates)
  (my-sketch-open-in-krita (helm-marked-candidates)))

(defun my-helm-open-sketches-in-gwenview (&optional candidates)
  (my-sketch-open-in-gwenview (helm-marked-candidates)))

(defun my-helm-open-sketches-in-feh (&optional candidates)
  (my-sketch-open-in-feh (helm-marked-candidates)))

(defvar my-helm-source-org-sketches
  '((name . "Sketches")
    (candidates . my-helm-source-org-sketch-list)
    (action . (("Insert" . my-helm-org-insert-sketch-candidates)
               ("Open in Krita" . my-helm-open-sketches-in-krita)
               ("Open in Gwenview" . my-helm-open-sketches-in-gwenview)
               ("Open as Feh slideshow" . my-helm-open-sketches-in-feh)))
    (persistent-action . my-helm-open-sketches-in-gwenview)))

(defun my-helm-org-sketches ()
  (interactive)
  (helm :sources '(my-helm-source-org-sketches)
        :buffer "*helm-org-sketches*"))
;; Helm completion with my-helm-org-sketches:1 ends here

;; Button-based interface
;; :PROPERTIES:
;; :CUSTOM_ID: button-based-interface
;; :END:

;; This makes a buffer with big buttons so that I can easily tap them with my stylus.


;; [[file:Sacha.org::#button-based-interface][Button-based interface:1]]
(defun my-set-up-sketch-buffer ()
  "Populate a widget buffer with a few handy buttons."
  (interactive)
  (with-current-buffer (get-buffer-create "*Done*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (widget-create 'push-button
                     :notify (lambda (&rest ignore)
                               (my-org-clock-in-and-track-by-name "Draw"))
                     "Track: Draw")
      (widget-create 'push-button
                     :notify (lambda (&rest ignore)
                               (my-org-clock-in-and-track-by-name "Draw journal entries"))
                     "Track: Journal")
      (widget-create 'push-button
                     :notify (lambda (&rest ignore)
                               (my-org-sketch-open (my-prepare-index-card-template)))
                     "New")
      (widget-create 'push-button
                     :notify (lambda (&rest ignore)
                               (my-org-sketch-open (my-prepare-large-template)))
                     "New large")
      (widget-create 'push-button
                     :notify (lambda (&rest ignore)
                               (my-org-sketch-open (my-prepare-index-card-template nil (org-read-date))))
                     "Date")
      (widget-create 'push-button
                     :notify (lambda (&rest ignore) (shell-command "~/bin/rotate-screen")) "Rotate")
      (insert "\n")
      (widget-create 'push-button
                     :notify (lambda (&rest ignore)
                               (shell-command "~/bin/add-output-png"))
                     "Add output.png")
      (widget-create 'push-button
                     :notify (lambda (&rest ignore)
                               (my-rotate-screen 0)
                               (kill-buffer)
                               (my-rename-scanned-cards))
                     "Process")
      (widget-create 'push-button
                     :notify (lambda (&rest ignore)
                               (my-rotate-screen 0)
                               (delete-window)
                               (my-rename-scanned-cards))
                     "Rename")
      (widget-create 'push-button
                     :notify (lambda (&rest ignore)
                               (my-rotate-screen 0)
                               (delete-window)
                               (my-convert-and-upload-cards))
                     "Upload")
      (widget-create 'push-button
                     :notify (lambda (&rest ignore)
                               (my-rotate-screen 0)
                               (org-clock-out)
                               (kill-buffer))
                     "Quit")
      (text-scale-set 10)
      (widget-setup)
      (widget-minor-mode)
      (pop-to-buffer (current-buffer))
      (goto-char (point-min))
      (current-buffer))))

(setq my-sketch-executable "krita"
      my-sketch-inbox-directory "~/Dropbox/Inbox"
      my-index-card-template-file "~/Dropbox/drawings/templates/0 - index.psd"
      my-sketch-large-template-file "/home/sacha/Dropbox/drawings/templates/0 - base.psd")
(defun my-prepare-index-cards (n)
  (interactive (list (or current-prefix-arg 5)))
  (let ((counter 1)
        (directory "~/Dropbox/Inbox")
        (template my-index-card-template-file)
        (date (substring (org-read-date nil nil ".") 0 10))
        temp-file)
    (quantified-track "Drawing")
    (dotimes (i 5) (my-org-sketch-open (my-prepare-index-card-template)))
    (my-rotate-screen 180)
    (my-set-up-sketch-buffer)))

(defvar my-index-card-file-name nil "Most recent index card file name.")
(defun my-rotate-screen (degrees)
  (cond
   ((eq system-type 'windows-nt)
    (shell-command (format "c:/sacha/Dropbox/bin/orient /rotate:%d" degrees)))
   ((eq system-type 'gnu/linux)
    (shell-command (format "~/bin/rotate-screen %s"
                           (cond
                            ((= degrees 0) "normal")
                            ((= degrees 180) "inverted")
                            ((= degrees 90) "left")
                            ((= degrees 270) "right")))))))
;; Button-based interface:1 ends here

;; Templates                                                        :image:
;; :PROPERTIES:
;; :CUSTOM_ID: templates
;; :END:

;; [[file:Sacha.org::#templates][Templates:1]]
(defun my-prepare-drawing-template (&optional name date template)
  "Create the image file for NAME. Return the new filename."
  (let* ((date (or date (substring (org-read-date nil nil ".") 0 10)))
         (data (my-journal-post (or name "sketch") :Date date)))
    (setq name (expand-file-name
                (concat (assoc-default 'ZIDString data)
                        (if name
                            (concat " "
                                    (my-convert-sketch-title-to-filename (or name "")))

                              "")
                            "." (file-name-extension template))
                    "~/Dropbox/Inbox"))
    (copy-file (or template my-index-card-template-file) name)
    name))

(defun my-org-insert-new-index-card-link ()
  (interactive)
  (let ((filename
         (my-prepare-index-card-template)))
    (insert "[[sketch:" filename "]]\n")
    (save-window-excursion
      (my-rotate-screen 180)
      (shell-command
       (concat (shell-quote-argument my-sketch-executable)
               " " (shell-quote-argument filename) " &")))))

(defun my-prepare-index-card-template (&optional name date)
  "Create the image file for NAME. Return the new filename."
  (my-prepare-drawing-template name date my-index-card-template-file))

(defun my-prepare-large-template (&optional name date)
  "Create the image file for NAME. Return the new filename."
  (my-prepare-drawing-template name date my-sketch-large-template-file))


(defun my-prepare-index-card (&optional name date)
  "Prepare the index card for NAME.
              Rotate the screen and show a button to un-rotate the screen."
  (interactive (list (read-string "Name: ")
                     (substring (if current-prefix-arg (org-read-date) (org-read-date nil nil ".")) 0 10)))
  (setq my-index-card-file-name (my-prepare-index-card-template name date))
  (save-window-excursion
    (my-rotate-screen 180)
    (shell-command
     (concat (shell-quote-argument my-sketch-executable)
             " " (shell-quote-argument my-index-card-file-name) " &")))
  (my-set-up-sketch-buffer))

(defun my-prepare-index-card-for-subtree ()
  "Create an index card template for the current subtree."
  (interactive)
  (let* ((heading (elt (org-heading-components) 4)))
    (unless (org-entry-get (point) "Effort") (org-set-property "Effort" "0:15"))
    (if (derived-mode-p 'org-agenda-mode) (org-agenda-clock-in) (org-clock-in))
    (my-org-quantified-track "Drawing")
    (if (org-at-heading-p) (forward-line 1))
    (my-prepare-index-card heading)))

(defun my-helm-org-prepare-index-card-for-subtree (candidate)
  (let ((location (org-refile--get-location candidate my-helm-org-refile-locations)))
    (save-window-excursion
      (save-excursion
        (org-refile 4 nil location)
        (my-prepare-index-card-for-subtree)) t)))
;; Templates:1 ends here

;; Easily backfill my journal
;; CLOSED: [2015-07-19 Sun 11:53]
;; :PROPERTIES:
;; :Effort:   0:30
;; :QUANTIFIED: Emacs
;; :CUSTOM_ID: easily-backfill-my-journal
;; :END:
;; :LOGBOOK:
;; - State "DONE"       from "STARTED"    [2015-07-19 Sun 11:53]
;;   CLOCK: [2015-07-19 Sun 11:18]--[2015-07-19 Sun 11:53] =>  0:35
;; :END:


;; [[file:Sacha.org::#easily-backfill-my-journal][Easily backfill my journal:1]]
(defun my-draw-journal-entry (date)
  "Creates a blank journal entry for DATE and brings up the log."
  (interactive (list (org-read-date)))
  ;; Open the Quantified Awesome time log for that date
  (let ((filename (my-get-journal-entry date))
        (day (format-time-string "%A" (org-time-string-to-time date))))
    (if filename
        (my-org-sketch-open filename)
      ;; (browse-url (format "http://quantifiedawesome.com/records?start=%s&end=%s"
      ;;                     date
      ;;                     (format-time-string
      ;;                      "%Y-%m-%d"
      ;;                      (seconds-to-time
      ;;                       (+ (org-time-string-to-seconds date) 86400)))))
      (setq filename
            (my-prepare-index-card-template (concat day " #daily #journal") date))
      (my-org-sketch-open filename))))

(defun my-get-journal-entry (date)
  "Returns the filename for the journal sketch for DATE."
  (car
   (-filter (lambda (x) (not (string-match "weekly" x)))
            (my-get-sketch-filenames
             (format "%s.* .*#daily" date)
             t))))

(defun my-get-missing-journal-dates (start-date end-date)
  "Return a list of dates missing journal entries.
      Range is specified by START-DATE (inclusive) and END-DATE (exclusive)."
  (let* ((current-day (org-time-string-to-absolute end-date))
         (start-day (org-time-string-to-absolute start-date))
         current-date
         current-date-string
         missing-list)
    (while (>= current-day start-day)
      (setq current-date (calendar-gregorian-from-absolute current-day))
      (setq current-date-string (format "%04d-%02d-%02d" (elt current-date 2) (elt current-date 0) (elt current-date 1)))
      (unless (my-get-journal-entry current-date-string)
        (add-to-list 'missing-list current-date-string))
      (setq current-day (1- current-day)))
    missing-list))

(defun my-show-missing-journal-entries (since)
  (interactive (list (if current-prefix-arg (org-read-date) (org-read-date nil nil "-7"))))
  (let ((missing-dates (my-get-missing-journal-dates since (org-read-date nil nil "."))))
    (with-current-buffer (my-set-up-sketch-buffer)
      (mapc
       (lambda (date)
         (widget-create 'push-button
                        :date date
                        :notify (lambda (widget &rest ignore)
                                  (my-draw-journal-entry (plist-get (cdr widget) :date)))
                        date))
       missing-dates)
      (widget-setup)
      (widget-minor-mode))))
;; Easily backfill my journal:1 ends here

;; Rename scanned index cards
;; :PROPERTIES:
;; :CUSTOM_ID: rename-scanned-index-cards
;; :END:


;; [[file:Sacha.org::#rename-scanned-index-cards][Rename scanned index cards:1]]
(use-package s)
(defun my-process-tiff (files)
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

(defun my-convert-index-card-to-png (o)
  (lambda (o)
    (call-process "krita" nil nil nil o "--export" "--export-filename"
                  (concat (file-name-sans-extension o) ".png"))
    (rename-file o "~/Dropbox/Inbox/backup/" t)))

(defun my-convert-index-card-tiffs-to-pngs ()
  (interactive)
  (let ((pattern "^\\(IMG\\|[0-9]+-[0-9]+-[0-9]+\\).*.\\(tif\\|psd\\)$"))
    (when (directory-files "~/Dropbox/Inbox/" t pattern)
      ;; Convert the TIFFs first
      (mapc 'my-convert-index-card-to-png
            (directory-files "~/Dropbox/Inbox/" t pattern)))))

(defun my-convert-and-upload-cards ()
  "Trust in existing filenames, upload without modification."
  (interactive)
  (my-convert-index-card-tiffs-to-pngs)
  (my-upload-scanned-cards))

(defun my-rename-scanned-card (filename)
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
                    (setq old-name (my-get-sketch-filename (match-string 0 new-name)))
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

(defun my-rename-scanned-cards ()
  "Display and rename the scanned or saved files."
  (interactive)
  (my-convert-index-card-tiffs-to-pngs)
  (mapc (lambda (o)
          (when (string= (file-name-extension o) "psd")
            (my-convert-index-card-to-png o)
            (setq o (concat (file-name-sans-extension o) ".png")))
          (my-rename-scanned-card o))
        (reverse (directory-files "~/Dropbox/Inbox/" t "^\\(IMG\\|[0-9]+-[0-9]+-[0-9]+\\).*.\\(psd\\|png\\|jpg\\)")))
  (my-upload-scanned-cards))

(defun my-clean-index-card-directory ()
  "Remove files marked for deletion and move private files."
  (shell-command "mv ~/Dropbox/Inbox/*delete* ~/Dropbox/Inbox/backup")
  (shell-command "mv ~/Dropbox/Inbox/*private* ~/cloud/private-sketches/"))

(defun my-upload-scanned-cards ()
  (interactive)
  (my-clean-index-card-directory)
  (with-current-buffer (get-buffer-create "*Files to be uploaded*")
    (erase-buffer)
    (insert (mapconcat 'identity (directory-files "~/Dropbox/Inbox" nil "^[0-9]+-[0-9]+-[0-9]+[^ ]? .*.\\(png\\|jpg\\)") "\n"))
    (goto-char (point-min))
    (switch-to-buffer (current-buffer))
    (delete-other-windows))
  (shell-command "~/bin/copy-sketches"))
;; Rename scanned index cards:1 ends here

;; Automatically resize images
;; :PROPERTIES:
;; :CUSTOM_ID: automatically-resize-images
;; :END:

;; The =image+= package is handy for displaying the images so
;; that they're scaled to the window size.


;; [[file:Sacha.org::#automatically-resize-images][Automatically resize images:1]]
(use-package image+
  :if my-laptop-p
  ;;    :load-path "~/elisp/Emacs-imagex"
  :commands (imagex-global-sticky-mode imagex-auto-adjust-mode)
  :init (progn (imagex-global-sticky-mode) (imagex-auto-adjust-mode)))
;; Automatically resize images:1 ends here

;; Get information for sketched books
;; :PROPERTIES:
;; :CUSTOM_ID: get-information-for-sketched-books
;; :END:

;; For sketchnotes of books, I set up the filename based on properties in
;; my Org Mode tree for that book.


;; [[file:Sacha.org::#get-information-for-sketched-books][Get information for sketched books:1]]
(defun my-prepare-sketchnote-file ()
  (interactive)
  (let* ((base-name (org-entry-get-with-inheritance  "BASENAME")))
    (unless base-name (error "Missing basename property"))
    (my-org-sketch-open (my-prepare-large-template base-name))))
;; Get information for sketched books:1 ends here

;; Make it easy to follow up on a sketch
;; :PROPERTIES:
;; :CUSTOM_ID: make-it-easy-to-follow-up-on-a-sketch
;; :END:


;; [[file:Sacha.org::#make-it-easy-to-follow-up-on-a-sketch][Make it easy to follow up on a sketch:1]]
(defun my-follow-up-on-sketch (filename)
  "Prompt for FILENAME to follow up on.
      Create an index card with it as a layer, and add the ref to the filename."
  (interactive (list (helm-read-file-name "Image: " :initial-input "~/sketches/")))
  ;; Allow the specification of a short identifier
  (unless (file-exists-p filename)
    (setq filename (car (directory-files "~/sketches" t (concat "^" filename)))))
  (let ((async-shell-command-buffer 'new-buffer)
        (index-card (my-prepare-index-card-template
                     (format "-- index card ref %s"
                             (and (string-match "^[^ \\.]+" (file-name-nondirectory filename))
                                  (match-string 0 (file-name-nondirectory filename)))))))
    (shell-command (format "convert %s %s -colorspace cmyk %s"
                           (shell-quote-argument (expand-file-name my-index-card-template-file))
                           (shell-quote-argument (expand-file-name filename))
                           (shell-quote-argument (expand-file-name index-card))))
    (shell-command (format "%s %s &"
                           (shell-quote-argument my-sketch-executable)
                           (shell-quote-argument (expand-file-name index-card))))
    (my-rotate-screen 180)
    (my-set-up-sketch-buffer)))
;; Make it easy to follow up on a sketch:1 ends here

;; Digital index piles with Emacs
;; CLOSED: [2015-02-01 Sun 18:26]
;; :PROPERTIES:
;; :Effort:   2:00
;; :QUANTIFIED: Emacs
;; :ID:       o2b:243ed83f-244f-417d-b251-53a3fef813aa
;; :POSTID:   27923
;; :BLOG:     sacha
;; :ARCHIVE_TIME: 2015-05-07 Thu 22:17
;; :ARCHIVE_FILE: ~/.config/emacs/Sacha.org
;; :ARCHIVE_OLPATH: Inactive/infrequent things/Drawing
;; :ARCHIVE_CATEGORY: Sacha
;; :ARCHIVE_TODO: DONE
;; :CUSTOM_ID: digital-index-piles-with-emacs
;; :END:
;; :LOGBOOK:
;; - State "DONE"       from "STARTED"    [2015-02-01 Sun 18:26]
;;   CLOCK: [2015-02-01 Sun 17:30]--[2015-02-01 Sun 18:26] =>  0:56
;;   - State "DONE"       from "STARTED"    [2015-02-01 Sun 17:24]
;;   CLOCK: [2015-02-01 Sun 13:30]--[2015-02-01 Sun 15:26] =>  1:56
;; :END:

;; Somewhat daunted by the prospect of categorizing more than a hundred
;; sketches and blog posts for my monthly review, I spent some time
;; figuring out how to create the digital equivalent of sorting index
;; cards into various piles.

;; [[https://www.flickr.com/photos/sachac/16234413499/][2015-02-01 Digital piles of index cards -- index card #indexing #organization #pkm]]

;; In fact, wouldn't it be super-cool if the items could automatically
;; guess which category they should probably go in, prompting me only if
;; it wasn't clear?

;; I wanted to write a function that could take a list structured like this:

;;   - Keyword A
;;     - Previous links
;;   - Keyword B
;;     - Previous links
;;   - Link 1 with Keyword A
;;   - Link 2 with Keyword B
;;   - Link 3 with Keyword A
;;   - Link 4

;;     It should file Link 1 and 3 under Keyword A, Link 2 under Keyword B,
;;     and prompt me for the category for Link 4. At that prompt, I should be
;;     able to select Keyword A or Keyword B, or specify a new category.

;;     Inspired by John Kitchin's recent post on [[http://kitchingroup.cheme.cmu.edu/blog/2015/01/24/Anatomy-of-a-helm-source/][defining a Helm source]], I
;;     wanted to get it to work with Helm.

;;     First step: I needed to figure out the structure of the list, maybe
;;     including a sample from the category to make it clearer what's
;;     included. =org-list.el= seemed to have useful functions for this.
;;     =org-list-struct= gave me the structure of the current list. Let's say
;;     that a category is anything whose text does not match
;;     =org-link-bracket-re=.


;; [[file:Sacha.org::#digital-index-piles-with-emacs][Digital index piles with Emacs:1]]
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
;; Digital index piles with Emacs:1 ends here



;; The next step was to write a function that guessed the list category
;; based on the item text, and moved the item there.


;; [[file:Sacha.org::#digital-index-piles-with-emacs][Digital index piles with Emacs:2]]
(defvar my-helm-org-list-candidates nil)
(defun my-helm-org-list-categories-init-candidates ()
  "Return a list of categories from this list in a form ready for Helm."
  (setq my-helm-org-list-candidates
        (mapcar (lambda (x)
                  (cons (if (elt x 3)
                            (format "%s - %s" (car x) (elt x 3))
                          (car x))
                        x))
                (my-org-get-list-categories))))

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
             (pos (point))
             s)
        (setq s (org-remove-indentation (buffer-substring-no-properties item (org-list-get-item-end item struct))))
        (save-excursion
          (if (string= category-text "x")
              (org-list-send-item item 'delete struct)
            (goto-char (caar struct))
            (if (re-search-forward category-regexp nil t)
                (progn
                  ;; needs a patch to ol.el to check if stringp
                  (org-list-send-item item (point-at-bol) struct)
                  (org-move-item-down)
                  (org-indent-item))
              (goto-char (car (last (car (last struct)))))
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
;; Digital index piles with Emacs:2 ends here



;; After that, I wrote a function that used Helm to prompt me for a
;; category in case it couldn't guess the category. It took me a while to
;; figure out that I needed to use =:init= instead of =:candidates=
;; because I wanted to read information from the buffer before Helm
;; kicked in.


;; [[file:Sacha.org::#digital-index-piles-with-emacs][Digital index piles with Emacs:3]]
(defvar my-org-browse-link-while-categorizing 'eww-readable
  "Set to nil to skip browsing.")

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
(defun my-eww-readable-nonce ()
  "Once-off call to `eww-readable' after EWW is done rendering."
  (unwind-protect
      (eww-readable)
    (remove-hook 'eww-after-render-hook #'my-eww-readable-nonce)))

(defun my-eww-browse-readable (url)
  (when (looking-at "^[-+] \\[\\[\\([^]]+\\)")
    (add-hook 'eww-after-render-hook #'my-eww-readable-nonce)
    (eww (match-string 1))))
;; Digital index piles with Emacs:3 ends here



;; Actually, it might be helpful to be able to sort lists by a keyword.


;; [[file:Sacha.org::#digital-index-piles-with-emacs][Digital index piles with Emacs:4]]
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
;; Digital index piles with Emacs:4 ends here



;; This one files sketches into the headings I've started using in questions.org.


;; [[file:Sacha.org::#digital-index-piles-with-emacs][Digital index piles with Emacs:5]]
(defun my-refile-sketches-to-questions ()
  (interactive)
  (while (looking-at "^  \\+ \\[\\[.*?\\]\\[\\(.*?\\) -- \\(.*?\\)\\]\\]\n")
    (let ((link (match-string 0))
          (title (match-string 1)))
      (save-excursion
        (if (save-match-data (search-forward (concat "* " title) nil t))
            (progn (forward-line) (insert (match-string 0)) (replace-match ""))
          (forward-line 1))))))
;; Digital index piles with Emacs:5 ends here

;; Xournalpp and Krita
;; :PROPERTIES:
;; :CUSTOM_ID: xournalpp-and-krita
;; :END:

;; Let's try xournal++.



;; [[file:Sacha.org::#xournalpp-and-krita][Xournalpp and Krita:1]]
(use-package org-krita
  :ensure t
  :quelpa (org-krita :fetcher github :repo "lepisma/org-krita" :files ("*.el" "resources"))
  :hook (org-mode . org-krita-mode))
(use-package org-xournalpp
  :disabled t
  :quelpa (org-xournalpp :fetcher gitlab :repo "vherrmann/org-xournalpp" :files ("*.el" "resources"))
  :hook (org-mode . org-xournalpp-mode))
;; Xournalpp and Krita:1 ends here

;; Sketched books
;; :PROPERTIES:
;; :CUSTOM_ID: insert-point
;; :END:

;; Convenience functions to make my life easier when sketchnoting books.


;; [[file:Sacha.org::#insert-point][Sketched books:1]]
(setq yas-indent-line 'fixed)
(defun my-convert-sketch-title-to-filename (text)
  (setq text (replace-regexp-in-string "[?!]$" "" text))
  (setq text (replace-regexp-in-string "[?!:] " " - " text)))
(ert-deftest my-convert-sketch-title-to-filename ()
  (should (string= (my-convert-sketch-title-to-filename "Test") "Test"))
  (should (string= (my-convert-sketch-title-to-filename "Another Test!") "Another Test"))
  (should (string= (my-convert-sketch-title-to-filename "Does this work? Yes") "Does this work - Yes"))
  (should (string= (my-convert-sketch-title-to-filename "Title: Subtitle") "Title - Subtitle"))
  )

(defun my-convert-sketched-book-to-png ()
  "Convert TIFF to PNG."
  (interactive)
  (let ((basename (org-entry-get-with-inheritance "BASENAME")))
    (shell-command (format "convert \"c:/sacha/dropbox/inbox/%s.tif\" \"c:/sacha/dropbox/inbox/%s.png\""
                           basename
                           basename))))

(defun my-index-sketched-book ()
  "Add entries to sketched books index."
  (interactive)
  (let* ((title (org-entry-get-with-inheritance "SHORT_TITLE"))
         (author (org-entry-get-with-inheritance "AUTHOR"))
         (basename (org-entry-get-with-inheritance "BASENAME"))
         (base-file (format "~/Dropbox/Inbox/%s.png" basename)))
    (when (file-exists-p base-file)
      (copy-file base-file
                 (format "~/Dropbox/Packaging/sketched-books/%s.png" basename) t t))
    (find-file "~/Dropbox/Packaging/sketched-books/index.org")
    (vc-git-register (list (format "%s.png" basename)))
    (goto-char (point-min))
    (re-search-forward "<<insert-point>>")
    (insert (format "\n- [[file:%s.png][%s - %s (sketched %s)]]\n  [[file:%s.png]]\n\n"
                    basename
                    title
                    author
                    (substring basename 0 10)
                    basename))
    (find-file "~/Dropbox/Packaging/sketched-books/ebook.org")
    (goto-char (point-min))
    (re-search-forward "<<insert-point>>")
    (insert (format "\n* %s - %s (sketched %s)\n\n[[file:%s.png]]\n\n"
                    title
                    author
                    (substring basename 0 10)
                    basename))))

(defun my-package-sketched-book ()
  "Add the latest sketch and package the collection."
  (interactive)
  (shell-command
   (format "plink -A vagrant@127.0.0.1 -P 2222 \"cd ~/Dropbox/Packaging/sketched-books; git add '%s.png'; git commit -m 'Added %s - %s' -a; git push; make all\" &"
           (org-entry-get-with-inheritance "BASENAME")
           (org-entry-get-with-inheritance "SHORT_TITLE")
           (org-entry-get-with-inheritance "AUTHOR"))))
;; Sketched books:1 ends here

;; Other sketches
;; :PROPERTIES:
;; :CUSTOM_ID: other-sketches
;; :END:

;; Based on [[http://williamedwardscoder.tumblr.com/post/84505278488/making-image-mosaics]]
;; Aspect ratio is width / height


;; [[file:Sacha.org::#other-sketches][Other sketches:1]]
(defun my-get-tile-dimensions (num-items orig-width orig-height target-aspect-ratio)
  (let ((rows 1) (cols 1)
        (current-aspect (/ orig-width (float orig-height)))
        add-col-aspect
        add-row-aspect)
    (while (< (* rows cols) num-items)
      (setq add-col-aspect (/ (* (1+ cols) (float orig-width))
                              (* rows orig-height))
            add-row-aspect (/ (* cols (float orig-width))
                              (* (1+ rows) orig-height)))
      (if (<  (abs (- add-col-aspect target-aspect-ratio))
              (abs (- add-row-aspect target-aspect-ratio)))
          (setq cols (1+ cols))
        (setq rows (1+ rows))))
    (cons cols rows)))
(ert-deftest my-get-tile-dimensions ()
  (should (equal (my-get-tile-dimensions 2 2 1 1) (cons 1 2)))
  (should (equal (my-get-tile-dimensions 4 2 1 0.5) (cons 1 4)))
  (should (equal (my-get-tile-dimensions 12 1 1 (/ 4.0 3.0)) (cons 4 3)))
  (should (equal (my-get-tile-dimensions 11 1 1 (/ 4.0 3.0)) (cons 4 3)))
  (should (equal (my-get-tile-dimensions 13 1 1 (/ 4.0 3.0)) (cons 4 4))))

(defun my-extract-image-filenames (beg end)
  "Return the filenames from the links in this region."
  (let (files)
    (save-excursion
      (goto-char (min beg end))
      (while (re-search-forward "sketch:" (max beg end) t)
        (let ((link (org-element-context)))
          (add-to-list 'files (org-element-property :path link))))
      files)))

(defun my-create-sketch-montage (files &optional tiles)
  "Combine the sketches in the region."
  (interactive
   (list
    (if (derived-mode-p 'dired-mode)
        (dired-get-marked-files)
      (mapcar 'my-get-sketch-filename
              (my-extract-image-filenames (min (point) (mark)) (max (point) (mark)))))
    (if current-prefix-arg (read-string "Tiling: "))))
  ;; Extract the links
  (let ((output-file "~/Dropbox/Inbox/output.png"))
    (unless tiles
      (setq tiles
            (format "%dx"
                    (car (my-get-tile-dimensions (length files) 1500 900 (/ 4.0 3))))))
    (with-temp-buffer
      (cd "~/Dropbox/Inbox/To blog")
      (apply 'call-process
             "montage" nil nil nil
             (append
              files
              (list
               "-geometry" "1500x900>+0+0"
               "-tile" tiles
               (expand-file-name output-file)))))
    (if (called-interactively-p 'any) (find-file output-file))))

(defun my-create-week-montage (beg end)
  (interactive "r")
  (let* ((date (org-read-date nil nil (unless current-prefix-arg "-fri")))
         (filename (format "Week ending %s #journal #weekly" date))
         (full-filename (my-get-sketch-filename filename)))
    (if full-filename
        (my-org-sketch-open full-filename)
      (my-create-index-card-montage
       (mapcar 'my-get-sketch-filename
               (my-extract-image-filenames (min (point) (mark)) (max (point) (mark))))
       "2x"
       (my-prepare-index-card-template filename)))))

(defun my-create-index-card-montage (files &optional tiling filename)
  "Prepare an index card with a montage of the selected sketches as a layer."
  (interactive
   (list
    (if (derived-mode-p 'dired-mode)
        (dired-get-marked-files)
      (mapcar 'my-get-sketch-filename
              (my-extract-image-filenames (min (point) (mark)) (max (point) (mark)))))))
  (let ((async-shell-command-buffer 'new-buffer)
        (index-card (or filename (my-prepare-index-card-template))))
    (my-create-sketch-montage files tiling)
    (shell-command
     (format "convert %s \\( %s -resize 1500x900 \\) -colorspace cmyk %s"
             (shell-quote-argument (expand-file-name my-index-card-template-file))
             (shell-quote-argument (expand-file-name "~/Dropbox/Inbox/output.png"))
             (shell-quote-argument (expand-file-name index-card))))
    (shell-command (format "%s %s &"
                           (shell-quote-argument my-sketch-executable)
                           (shell-quote-argument (expand-file-name index-card))))
    (my-rotate-screen 180)
    (my-set-up-sketch-buffer)))
;; Other sketches:1 ends here

;; Other sketch-related functions
;; :PROPERTIES:
;; :CUSTOM_ID: other-sketch-related-functions
;; :END:

;; [[file:Sacha.org::#other-sketch-related-functions][Other sketch-related functions:1]]
(defun my-show-sketches-as-slideshow (list &optional shuffle)
  "Display a quick slideshow of sketches in LIST.
          If LIST is a string, look up those sketch filenames in my Flickr copy."
  (interactive "MFilter: \nP")
  (apply 'call-process "feh" nil nil nil "-D" "1" "-F" (if shuffle "-z" """")
         (-filter (lambda (x) (string-match "photostream" x))
                  (if (stringp list)
                      (my-list-sketches list t)
                    list))))

(defvar my-org-index-card-source nil)
(defun my-org-prompt-index-cards ()
  "Display a buffer for easy selection of questions to work on."
  (interactive)
  (find-file "~/personal/questions.org")
  (let ((questions
         (cl-sort (org-map-entries 'org-heading-components "TODO=\"DRAW\"")
                  '< :key (lambda (x) (or (elt x 3) 100)))))
    (setq my-org-index-card-source (current-buffer))
    (my-rotate-screen 180)
    (my-set-up-sketch-buffer)
    (mapc (lambda (q)
            (widget-create 'push-button
                           :notify (lambda (widget &rest ignore)
                                     (my-org-sketch-open
                                      (my-prepare-index-card-template
                                       (widget-value widget)))
                                     (with-current-buffer my-org-index-card-source
                                       (save-excursion
                                         (goto-char (org-find-exact-headline-in-buffer (widget-value widget) my-org-index-card-source t))
                                         (org-set-property "Effort" "0:15")
                                         (org-clock-in)
                                         (org-todo "LINK")))
                                     (widget-delete widget))
                           (elt q 4))
            (insert "\n"))
          questions)
    (text-scale-set 5)
    (widget-setup)
    (widget-minor-mode)
    (goto-char (point-min))
    (when (functionp 'scroll-bar-mode) (scroll-bar-mode))
    (switch-to-buffer (current-buffer))))

(defun my-prepare-index-card-for-journal ()
  "Create an index card for my process journal."
  (interactive)
  (quantified-track "Drawing")
  (my-prepare-index-card "Journal"))

(with-eval-after-load 'org
  (let ((listvar (if (boundp 'org-speed-commands) 'org-speed-commands
                   'org-speed-commands-user)))
    (add-to-list listvar '("d" call-interactively 'my-prepare-index-card-for-subtree))))
;; Other sketch-related functions:1 ends here

;; SOMEDAY Write about half-page scans
;; :PROPERTIES:
;; :CREATED:  [2021-12-19 Sun 21:16]
;; :CUSTOM_ID: write-about-half-page-scans
;; :END:


;; [[file:Sacha.org::#write-about-half-page-scans][SOMEDAY Write about half-page scans:1]]
(defun my-insert-sketch-and-text (sketch)
	(interactive (list (my-complete-sketch-filename)))
	(insert (file-name-base sketch)
					(format "\n\n[[sketchFull:%s][%s]]\n\n" (file-name-nondirectory sketch) (file-name-base sketch)))
	(insert "#+begin_my_details Text from sketch\n")
	(my-sketch-insert-text-from-json sketch)
	(insert "\n#+end_my_details"))
(defun my-write-about-sketch (sketch)
  (interactive (list (my-complete-sketch-filename)))
  (shell-command "make-sketch-thumbnails")
  (find-file "~/sync/orgzly/posts.org")
  (goto-char (point-max))
  (org-insert-heading nil nil t)
	(my-insert-sketch-and-text sketch)
  (my-org-11ty-prepare-subtree)
  (delete-other-windows)
  (save-excursion
    (with-selected-window (split-window-horizontally)
      (find-file sketch))))

(defun my-write-about-half-page-scan (filename)
  (interactive (list (read-file-name (format "Sketch (%s): "
                                             (file-name-base (my-latest-file my-scan-directory)))
                                     (expand-file-name my-scan-directory)
                                     (my-latest-file my-scan-directory)
                                     nil
                                     (expand-file-name my-scan-directory)
                                     (lambda (f) (string-match "\\.\\(jpg\\|png\\)$" f)))))
  (let (new-name)
    (shell-command (concat "~/bin/prepare-half-page " (shell-quote-argument filename)))
    (if (string-match "[0-9]+-[0-9]+-[0-9]+\\([a-z]\\|-[0-9]+\\)? .*" (file-name-base filename))
        (progn
          (rename-file filename (expand-file-name (file-name-nondirectory filename) my-sketches-directory) t)
          (setq new-name filename))
      (save-window-excursion
        (find-file filename)
        (setq new-name (expand-file-name (concat (read-string "New name: ") "." (file-name-extension filename))
                                         my-sketches-directory))
        (rename-file filename new-name)))
    (my-write-about-sketch new-name)))
;; SOMEDAY Write about half-page scans:1 ends here

;; Supernote                                                    :supernote:
;; :PROPERTIES:
;; :CUSTOM_ID: supernote
;; :END:

;; [[file:Sacha.org::#supernote][Supernote:1]]
(defvar my-supernote-export-dir "~/Dropbox/Supernote/EXPORT")
(defun my-supernote-process-latest ()
  (interactive)
  (find-file (my-latest-file my-supernote-export-dir))
  (my-supernote-process-sketch (read-string (format "New name for %s: " (file-name-base (buffer-file-name))))))
(defun my-supernote-export-dired ()
  (interactive)
  (dired my-supernote-export-dir "-tl"))
(defun my-supernote-process-sketch (new-name)
  (interactive (list
								(completing-read "New name: " (my-sketches))))
  (unless (member (file-name-extension new-name) '("png" "jpg"))
    (setq new-name (concat new-name "." (file-name-extension (buffer-file-name)))))
	(let ((dest (if (string-match "#private" new-name)
									"~/sync/private-sketches"
								"~/sync/sketches")))
		(when (string-match " #ccw" new-name)
			(setq new-name (replace-match "" t t new-name))
			(call-process "mogrify" nil nil nil "-rotate" "270" (buffer-file-name)))
		(when (string-match " #cw" new-name)
			(setq new-name (replace-match "" t t new-name))
			(call-process "mogrify" nil nil nil "-rotate" "90" (buffer-file-name)))
		(when (file-exists-p (expand-file-name (file-name-nondirectory new-name) dest))
			(delete-file (expand-file-name (file-name-nondirectory new-name) dest)))
		(rename-visited-file
		 (expand-file-name (file-name-nondirectory new-name) dest))
		(kill-buffer)))
(defun my-open-latest-export ()
  (interactive)
  (find-file (my-latest-file "~/Dropbox/Supernote/EXPORT")))

(defun my-copy-latest-export-filename ()
  (interactive)
  (kill-new (my-latest-file "~/Dropbox/Supernote/EXPORT")))

(defun my-supernote-copy-latest-download ()
  (interactive)
  (call-process "sn" nil nil nil (my-latest-file "~/Downloads"))
	(message "%s" (my-latest-file "~/Downloads")))
;; Supernote:1 ends here

;; [[file:Sacha.org::#supernote][Supernote:2]]
(defvar my-supernote-inbox "~/Dropbox/Supernote/INBOX")
(defun my-save-manpage-to-supernote (path)
	(interactive (list (woman-file-name nil)))
	(unless (file-exists-p path) (setq path (woman-file-name path)))
	(let* ((base (file-name-base path))
				 (temp-html (make-temp-file base nil ".html")))
		(with-temp-buffer
			(insert-file-contents path)
			(call-process-region (point-min) (point-max) "man2html" t t)
			(when (re-search-backward "Invalid Man Page" nil t)
				(delete-file temp-html)
				(error "Could not convert."))
			(write-file temp-html))
		(call-process "ebook-convert" nil (get-buffer-create "*temp*") nil temp-html
									(expand-file-name (concat base ".epub") my-supernote-inbox))
		(delete-file temp-html)))
;; Supernote:2 ends here



;; Info file:


;; [[file:Sacha.org::#supernote][Supernote:3]]
(defun my-save-info-to-supernote (path)
	(interactive (list (read-file-name "Texi: " nil nil
																		 (and Info-current-file
																					(file-exists-p (concat Info-current-file ".texi"))
																					(concat Info-current-file ".texi"))
																		 nil
																		 (lambda (f)
																			 (or
																				(string-match "\\.texi\\'" f)
																				(file-directory-p f))))))
	(call-process "texi2pdf" nil "*temp*" t (expand-file-name path)
								"-o"
								(expand-file-name (concat (file-name-base path) ".pdf")
																															my-supernote-inbox)))
;; Supernote:3 ends here



;; And in general:


;; [[file:Sacha.org::#supernote][Supernote:4]]
(defvar my-supernote-css "~/proj/static-blog/assets/css/style.css")
(defun my-save-to-supernote ()
	(interactive)
	(cond
	 ((derived-mode-p 'Man-mode) (my-save-manpage-to-supernote Man-arguments))
	 ((derived-mode-p 'Info-mode)
		(my-save-info-to-supernote
		 (or (and Info-current-file
							(file-exists-p (concat Info-current-file ".texi"))
							(concat Info-current-file ".texi"))
				 (read-file-name
					"Texi: " nil nil nil nil
					(lambda (f)
						(or
						 (string-match "\\.texi\\'" f)
						 (file-directory-p f)))))))
	 ((derived-mode-p 'org-mode)
		(org-latex-export-to-pdf)
		(copy-file (concat (file-name-base (buffer-file-name)) ".pdf")
							 (expand-file-name (concat (file-name-base (buffer-file-name)) ".pdf")
																 my-supernote-inbox) t))
	 ((or (derived-mode-p 'html-mode)
				(derived-mode-p 'web-mode)
				(derived-mode-p 'markdown-mode))
		(call-process "pandoc" nil nil nil (buffer-file-name) "-t" "latex"
									"-o"
									(expand-file-name (concat (file-name-base (buffer-file-name)) ".pdf")
																		my-supernote-inbox)))
	 ((and (buffer-file-name) (string-match "\\.\\(pdf\\|epub\\)$" (buffer-file-name)))
		(copy-file (buffer-file-name)
							 (expand-file-name (file-name-nondirectory (buffer-file-name))
																 my-supernote-inbox)
							 t))
	 (t
		(let ((filename (expand-file-name
										 (concat (file-name-base (or (buffer-file-name)
																								 (format-time-string "%Y-%m-%d-%H-%M-%S")))
														 ".pdf")
										 my-supernote-inbox)))
			(with-current-buffer (htmlize-buffer)
				(call-process-region
				 (point-min) (point-max) "wkhtmltopdf" nil nil nil "--no-background" "-"
				 filename))))))

(setq htmlize-css-name-prefix "org-")
(setq htmlize-head-tags "<link rel=\"stylesheet\" href=\"https://sachachua.com/assets/css/style.css\" />")
;; Supernote:4 ends here

;; Using Emacs Lisp to export TXT/EPUB/PDF from Org Mode to the Supernote via Browse and Access :supernote:org:emacs:
;; :PROPERTIES:
;; :CREATED:  [2024-09-29 Sun 07:58]
;; :EXPORT_LATEX_HEADER: \usepackage{setspace}
;; :EXPORT_DATE: 2024-09-29T08:27:21-0400
;; :EXPORT_ELEVENTY_PERMALINK: /blog/2024/09/using-emacs-lisp-to-export-txt-epub-pdf-from-org-mode-to-the-supernote-via-browse-and-access/
;; :EXPORT_ELEVENTY_FILE_NAME: blog/2024/09/using-emacs-lisp-to-export-txt-epub-pdf-from-org-mode-to-the-supernote-via-browse-and-access/
;; :CUSTOM_ID: supernote-org-upload
;; :END:

;; #+begin_export latex
;; \spacing{1.5}
;; #+end_export

;; I've been experimenting with the Supernote's
;; Browse and Access feature because I want to be
;; able to upload files quickly instead of waiting
;; for Dropbox to synchronize. First, I want to store
;; the IP address in a variable:

;; [[defvar:my-supernote-ip-address?open=1]]

;; Here's how to upload:


;; [[file:Sacha.org::#supernote-org-upload][Using Emacs Lisp to export TXT/EPUB/PDF from Org Mode to the Supernote via Browse and Access:1]]
(defun my-supernote-upload (filename &optional supernote-path)
	(interactive "FFile: ")
	(setq supernote-path (or supernote-path "/INBOX"))
	(let* ((boundary (mml-compute-boundary '()))
				 (url-request-method "POST")
				 (url-request-extra-headers
					`(("Content-Type" . ,(format "multipart/form-data; boundary=%s" boundary))))
				 (url-request-data
					(mm-url-encode-multipart-form-data
					 `(("file" . (("name" . "file")
												("filename" . ,(file-name-nondirectory filename))
												("content-type" . "application/octet-stream")
												("filedata" . ,(with-temp-buffer
																				 (insert-file-contents-literally filename)
																				 (buffer-substring-no-properties (point-min) (point-max)))))))
					 boundary)))
		(with-current-buffer
				(url-retrieve-synchronously
				 (format "http://%s:8089%s" my-supernote-ip-address supernote-path))
			(re-search-backward "^$")
			(prog1 (json-read)
				(kill-buffer)))))
;; Using Emacs Lisp to export TXT/EPUB/PDF from Org Mode to the Supernote via Browse and Access:1 ends here



;; HTML isn't supported. Text works, but it doesn't support annotation. PDF or EPUB could work.
;; It would make sense to register this as an export backend so that I can call it as part of the usual export process.


;; [[file:Sacha.org::#supernote-org-upload][Using Emacs Lisp to export TXT/EPUB/PDF from Org Mode to the Supernote via Browse and Access:2]]
(defun my-supernote-org-upload-as-text (&optional async subtree visible-only body-only ext-plist)
	"Export Org format, but save it with a .txt extension."
	(interactive (list nil current-prefix-arg))
	(let ((filename (org-export-output-file-name ".txt" subtree))
				(text (org-export-as 'org subtree visible-only body-only ext-plist)))
		;; consider copying instead of exporting so that #+begin_export html etc. is preserved
		(with-temp-file filename
			(insert text))
		(my-supernote-upload filename)))

(defun my-supernote-org-upload-as-pdf (&optional async subtree visible-only body-only ext-plist)
	(interactive (list nil current-prefix-arg))
	(my-supernote-upload (org-latex-export-to-pdf async subtree visible-only body-only ext-plist)))

(defun my-supernote-org-upload-as-epub (&optional async subtree visible-only body-only ext-plist)
	(interactive (list nil current-prefix-arg))
	(my-supernote-upload (org-epub-export-to-epub async subtree visible-only ext-plist)))

(org-export-define-backend
		'supernote nil
		:menu-entry '(?s "Supernote"
										 ((?s "as PDF" my-supernote-org-upload-as-pdf)
											(?e "as EPUB" my-supernote-org-upload-as-epub)
											(?o "as Org" my-supernote-org-upload-as-text))))
;; Using Emacs Lisp to export TXT/EPUB/PDF from Org Mode to the Supernote via Browse and Access:2 ends here

;; org-attaching the latest image from my Supernote via Browse and Access :supernote:org:
;; :PROPERTIES:
;; :CUSTOM_ID: supernote-browse
;; :ID:       20240926T080807.815726
;; :EXPORT_DATE: 2024-09-26T08:25:54-0400
;; :EXPORT_ELEVENTY_PERMALINK: /blog/2024/09/org-attaching-the-latest-image-from-my-supernote-via-browse-and-access/
;; :EXPORT_ELEVENTY_FILE_NAME: blog/2024/09/org-attaching-the-latest-image-from-my-supernote-via-browse-and-access/
;; :EXPORT_MODIFIED: 2024-09-29T20:32:58-0400
;; :END:

;; #+begin_update
;; [2024-09-29]: Use sketch links when possible. Recolor before cropping so that the grid is removed.
;; #+end_update

;; #+CAPTION: Diagram of different ways to get drawings off my Supernote A5X
;; [[attachment:2024-09-26-01 Supernote A5X Browse and Access %23supernote.png]]

;; #+begin_my_details Text from sketch
;; Supernote A5X
;; - Screen mirroring (pixelated) -> Puppeteer screenshot (or maybe .mjpeg?)
;; - Browse & Access (HTTP) -> latest file: recognize text, recolor, crop, upload?
;; - Dropbox/Google Drive (slow) -> batch process: recognize text, recolor, upload

;; Bonus: Autocropping encourages me to just get stuff out there even if I haven't filled a page

;; ideas: remove template automatically? I wonder if I can use another color...

;; 2024-09-26-01
;; #+end_my_details

;; I want to quickly get drawings from my Supernote A5X into Emacs so that I can include them in blog posts. Dropbox/Google Drive sync is slow because it synchronizes all the files. The Supernote can mirror its screen as an .mjpeg stream. I couldn't figure out how to grab a frame from that, but I did find out how to [[dotemacs:using-puppeteer-to-grab-an-image-from-the-supernote-s-screen-mirror][use Puppeteer to take an screenshot of the Supernote's screen mirror]]. Still, the resulting image is a little pixelated. If I turn on Browse and Access, the Supernote can serve directories and files as webpages. This lets me grab the latest file and process it. I don't often have time to fill a full A5 page with thoughts, so [[dotemacs:embark-image][autocropping]] the image encourages me to get stuff out there instead of holding on to things.


;; [[file:Sacha.org::#supernote-browse][org-attaching the latest image from my Supernote via Browse and Access:1]]
(defvar my-supernote-ip-address "192.168.1.221")
(defun my-supernote-get-exported-files ()
	(let ((data (plz 'get (format "http://%s:8089/EXPORT" my-supernote-ip-address)))
				(list))
		(when (string-match "const json = '\\(.*\\)'" data)
			(sort
			 (alist-get 'fileList (json-parse-string (match-string 1 data) :object-type 'alist :array-type 'list))
			 :key (lambda (o) (alist-get 'date o))
			 :lessp 'string<
			 :reverse t))))

(defun my-supernote-org-attach-latest-exported-file ()
	(interactive)
	;; save the file to the screenshot directory
	(let ((info (car (my-supernote-get-exported-files)))
				new-file
				renamed)
		;; delete matching files
		(setq new-file (expand-file-name
										(replace-regexp-in-string " " "%20" (alist-get 'name info) (org-attach-dir))))
		(when (file-exists-p new-file)
			(delete-file new-file))
		(org-attach-attach
		 (format "http://%s:8089%s" my-supernote-ip-address
						 (alist-get 'uri info))
		 nil
		 'url)
		(setq new-file (my-latest-file (org-attach-dir)))
		;; recolor
		(my-sketch-recolor-png new-file)
		;; autocrop that image
		(my-image-autocrop new-file)
		;; possibly rename
		(setq renamed (my-image-recognize-get-new-filename new-file))
		(when renamed
			(setq renamed (expand-file-name renamed (org-attach-dir)))
			(rename-file new-file renamed t)
			(my-image-store renamed) ; file it in my archive
			(setq new-file renamed))
		;; use a sketch link if it has an ID
		(if (string-match "^[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]-[0-9][0-9] "
											(file-name-base renamed))
				(org-insert-link nil (concat "sketchFull:" (file-name-base renamed)))
			;; insert the link
			(org-insert-link nil (concat "attachment:" (replace-regexp-in-string "#" "%23" (file-name-nondirectory new-file)))))
		(org-redisplay-inline-images)))
;; org-attaching the latest image from my Supernote via Browse and Access:1 ends here



;; Then I can call that from Emacs Lisp and run it through my usual [[dotemacs:org-mode-create-a-quick-timestamped-note-and-capture-a-screenshot][screenshot insertion process]]:


;; [[file:Sacha.org::#using-puppeteer-to-grab-an-image-from-the-supernote-s-screen-mirror][Using Puppeteer to grab an image from the SuperNote's screen mirror:2]]
(defun my-org-insert-supernote-screenshot-from-mirror ()
	"Copy the current image from the SuperNote mirror."
	(interactive)
	(let ((filename (expand-file-name (format-time-string "%Y-%m-%d-%H-%M-%S.png") "~/recordings")))
		(shell-command-to-string (concat "NODE_PATH=/usr/lib/node_modules node ~/bin/supernote-screenshot.js " (shell-quote-argument filename)))
		;; trim it
		(call-process "mogrify" nil nil nil "-trim" "+repage" filename)
		(shell-command-to-string (concat "~/bin/recolor.py --colors c0c0c0,f6f396 " (shell-quote-argument filename)))
		(call-interactively 'my-org-insert-screenshot)))
;; Using Puppeteer to grab an image from the SuperNote's screen mirror:2 ends here

;; TOBLOG Split a transcript into phrases for subtitles
;; :PROPERTIES:
;; :Effort:   0:30
;; :QUANTIFIED: Emacs
;; :END:
;; :LOGBOOK:
;; CLOCK: [2024-10-21 Mon 12:13]--[2024-10-21 Mon 13:12] =>  0:59
;; :END:


;; [[file:Sacha.org::*TOBLOG Split a transcript into phrases for subtitles][TOBLOG Split a transcript into phrases for subtitles:1]]
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
(defun my-split-clear-overlays ()
	(interactive)
	(remove-overlays (point-min) (point-max) 'my-split t))
;; TOBLOG Split a transcript into phrases for subtitles:1 ends here

;; Transcript editing
;; :PROPERTIES:
;; :CUSTOM_ID: transcript-editing
;; :END:


;; [[file:Sacha.org::#transcript-editing][Transcript editing:1]]
(defun my-split-sentence-and-capitalize ()
  (interactive)
  (delete-char 1)
  (insert ".")
  (capitalize-word 1))
(defun my-split-sentence-delete-word-and-capitalize ()
  (interactive)
  (delete-char 1)
  (insert ".")
  (kill-word 1)
  (capitalize-word 1))
(defun my-delete-word-and-capitalize ()
  (interactive)
  (skip-syntax-backward "w")
  (kill-word 1)
  (capitalize-word 1))

(defun my-emms-player-mplayer-set-speed (speed)
  "Depends on mplayer's -slave mode"
  (interactive "MSpeed: ")
  (process-send-string emms-player-simple-process-name
                       (format "speed_set %s\n" speed)))

(defvar my-emms-player-mplayer-speed-increment 0.1)

(defun my-emms-player-mplayer-speed-up ()
  "Depends on mplayer's -slave mode"
  (interactive)
  (process-send-string emms-player-simple-process-name
                       (format "speed_incr %f\n" my-emms-player-mplayer-speed-increment)))
(defun my-emms-player-mplayer-slow-down ()
  "Depends on mplayer's -slave mode"
  (interactive)
  (process-send-string emms-player-simple-process-name
                       (format "speed_incr %f\n" (- 0 my-emms-player-mplayer-speed-increment))))
;; Transcript editing:1 ends here

;; Remove underlining from WhisperX VTT
;; :PROPERTIES:
;; :CREATED:  [2024-10-16 Wed 20:43]
;; :END:
;; :LOGBOOK:
;; CLOCK: [2024-10-16 Wed 20:43]--[2024-10-16 Wed 20:50] =>  0:07
;; :END:


;; [[file:Sacha.org::*Remove underlining from WhisperX VTT][Remove underlining from WhisperX VTT:1]]
(defun my-subed-remove-whisperx-underlines ()
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
;; Remove underlining from WhisperX VTT:1 ends here

;; Adjust subtitles
;; :PROPERTIES:
;; :CUSTOM_ID: adjust-subtitles
;; :END:


;; [[file:Sacha.org::#adjust-subtitles][Adjust subtitles:1]]
(defun my-subed-move-succeeding-subtitles-based-on-mpv ()
  "Move current and succeeding subtitles so that current starts at MPV playing position."
	(interactive)
	(if subed-mpv-playback-position
			(subed-move-subtitles
			 (- subed-mpv-playback-position (subed-subtitle-msecs-start))
			 (point) (point-max))
		(error "Need playback position.")))

(defun my-subed-check-random ()
	(interactive)
	(let* ((list (subed-subtitle-list))
				 (pos (random (length list))))
		(subed-jump-to-subtitle-id
		 (subed-msecs-to-timestamp (elt (elt list pos) 1)))
		(subed-mpv-jump-to-current-subtitle)
		(subed-mpv-unpause)))
;; Adjust subtitles:1 ends here

;; Extract part of a video
;; :PROPERTIES:
;; :CUSTOM_ID: extract-part-of-a-video
;; :END:


;; [[file:Sacha.org::#extract-part-of-a-video][Extract part of a video:1]]
(defun my-subed-get-region-start-stop (beg end)
  (interactive "r")
  (cons (save-excursion
          (goto-char (min beg end))
          (subed-subtitle-msecs-start))
        (save-excursion
          (goto-char (max beg end))
          (subed-subtitle-msecs-stop))))

(defun my-extend-file-name (original name &optional extension)
  "Add NAME to the end of ORIGINAL, before the file extension."
  (concat (file-name-sans-extension original) " " name "."
          (or extension (file-name-extension original))))

(defun my-adjust-subtitles (offset)
  "Change all of the start and end times by OFFSET."
  (interactive (list (subed--string-to-msecs (read-string "Time: "))))
  (subed-for-each-subtitle (point-min) (point-max) nil
    (subed-adjust-subtitle-time-start offset t t)
    (subed-adjust-subtitle-time-stop offset t t))
  (subed-regenerate-ids))

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

(defun my-msecs-to-timestamp (msecs)
  "Convert MSECS to string in the format HH:MM:SS.MS."
  (concat (format-seconds "%02h:%02m:%02s" (/ msecs 1000))
          "." (format "%03d" (mod msecs 1000))))

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

(defun my-subed-ffmpeg-make-mute-filter (segments)
  (mapconcat
   (lambda (s)
     (format "volume=enable='between(t,%.3f,%.3f)':volume=0"
             (/ (car s) 1000.0)
             (/ (cdr s) 1000.0)))
   segments ", "))

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
;; Extract part of a video:1 ends here

;; Hide IDs and times
;; :PROPERTIES:
;; :CUSTOM_ID: hide-ids-and-times
;; :END:

;; [[file:Sacha.org::#hide-ids-and-times][Hide IDs and times:1]]
(define-minor-mode my-subed-hide-nontext-minor-mode
  "Minor mode for hiding non-text stuff.")
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

(defun my-subed-show-all ()
  (interactive)
  (let ((inhibit-read-only t))
    (with-silent-modifications
      (remove-text-properties (point-min) (point-max) '(read-only t))
      (remove-overlays (point-min) (point-max) 'invisible t))))

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
;; Hide IDs and times:1 ends here

;; Other subtitle code
;; :PROPERTIES:
;; :CUSTOM_ID: other-subtitle-code
;; :END:

;; [[file:Sacha.org::#other-subtitle-code][Other subtitle code:1]]
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

(defhydra my-subed ()
  "Make it easier to split and merge"
  ("e" subed-jump-to-subtitle-end "End")
  ("s" subed-jump-to-subtitle-text "Start")
  ("f" my-subed-forward-word "Forward word")
  ("b" my-subed-backward-word "Backward word")
  ("w" avy-goto-word-1-below "Jump to word")
  ("n" subed-forward-subtitle-text "Forward subtitle")
  ("p" subed-backward-subtitle-text "Backward subtitle")
  (".p" (subed-split-and-merge-dwim 'prev) "Split and merge with previous")
  (".n" (subed-split-and-merge-dwim 'next) "Split and merge with next")
  ("mp" subed-merge-with-previous "Merge previous")
  ("mn" subed-merge-with-next "Merge next")
  ("j" subed-mpv-jump-to-current-subtitle "MPV current")
  ("1" (subed-mpv-playback-speed 1.0) "1x speed")
  ("2" (subed-mpv-playback-speed 0.7) "0.7x speed")
  ("3" (subed-mpv-playback-speed 0.5) "0.5x speed")
  (" " subed-mpv-pause "Pause")
  ("[" (subed-mpv-seek -1000) "-1s")
  ("]" (subed-mpv-seek 1000) "-1s")
  (";" (re-search-forward "[,\\.;]") "Search for break")
  ("uu" (subed-split-and-merge-dwim 'prev) "Split and merge with previous")
  ("hh" (subed-split-and-merge-dwim 'next) "Split and merge with next")
  ("hu" subed-merge-with-previous "Merge with previous")
  ("uh" subed-merge-with-next "Merge with next")
  ("lf" subed-mpv-find-video "Find video file")
  ("lu" subed-mpv-play-url "Find video at URL")
  ("x" kill-word "Kill word")
  ("S" save-buffer "Save")
  ("o" (insert "\n") (let ((fill-column (point-max))) (fill-paragraph))))
(use-package subed
  :if my-laptop-p
  ;; :quelpa (subed :fetcher github :repo "rndusr/subed" :files (:defaults "subed/*.el"))
  :load-path "~/vendor/subed/subed"
  :config
  (setq subed-subtitle-spacing 1)
  (key-chord-define subed-mode-map "hu" 'my-subed/body)
  (key-chord-define subed-mode-map "ht" 'my-subed/body)
	(setq subed-loop-seconds-before 0 subed-loop-seconds-after 0)
	(setq subed-align-command
				'("/home/sacha/vendor/aeneas/venv/bin/python3" "-m" "aeneas.tools.execute_task"))
  :bind
  (:map subed-mode-map
        ("M-j" . avy-goto-char-timer)
        ("M-j" . subed-mpv-jump-to-current-subtitle)
        ("M-!" . subed-mpv-seek)))
(use-package subed-record
	:load-path "~/proj/subed-record"
  :config
  (remove-hook 'subed-sanitize-functions 'subed-sort)
	(setq subed-record-ffmpeg-args (split-string "-y -f pulse -i alsa_input.usb-Blue_Microphones_Yeti_Stereo_Microphone_REV8-00.analog-stereo"))
  :bind
  (:map subed-mode-map ("C-c C-c" . subed-record-compile-video)))
;; Other subtitle code:1 ends here

;; Using Emacs to fix automatically generated subtitle timestamps   :emacs:
;; :PROPERTIES:
;; :ID:       o2b:6bd48025-ccdc-4a2a-8a19-fbf7727cb8e5
;; :POST_DATE: [2021-01-10 Sun 00:59]
;; :BLOG:     sacha
;; :POSTID:   29659
;; :CUSTOM_ID: using-emacs-to-fix-automatically-generated-subtitle-timestamps
;; :END:

;; I like how people are making more and more Emacs-related videos. I
;; think subtitles, transcripts, and show notes would go a long way to
;; helping people quickly search, skim, and squeeze these videos into
;; their day.

;; Youtube's automatically-generated subtitles overlap. I think some
;; players scroll the subtitles, but the ones I use just display them
;; in alternating positions. I like to have non-overlapping subtitles,
;; so here's some code that works with [[https://github.com/rndusr/subed][subed.el]] to fix the timestamps.


;; [[file:Sacha.org::#using-emacs-to-fix-automatically-generated-subtitle-timestamps][Using Emacs to fix automatically generated subtitle timestamps:1]]
(defun my-subed-fix-timestamps ()
  "Change all ending timestamps to the start of the next subtitle."
  (interactive)
  (goto-char (point-max))
  (let ((timestamp (subed-subtitle-msecs-start)))
    (while (subed-backward-subtitle-time-start)
      (subed-set-subtitle-time-stop timestamp)
      (setq timestamp (subed-subtitle-msecs-start)))))
;; Using Emacs to fix automatically generated subtitle timestamps:1 ends here

;; Using word-level timing information when editing subtitles or captions in Emacs :emacs:
;;      :PROPERTIES:
;;      :EXPORT_DATE: 2021-03-18T16:30:30-0400
;;      :EXPORT_ELEVENTY_PERMALINK: /blog/2021/03/using-word-level-timing-information-when-editing-subtitles-or-captions-in-emacs/
;;      :EXPORT_ELEVENTY_FILE_NAME: blog/2021/03/using-word-level-timing-information-when-editing-subtitles-or-captions-in-emacs/
;;      :ID:       o2b:a3c2434a-c127-439f-9c66-a70a25baa78f
;;      :POST_DATE: [2021-03-18 Thu 16:30]
;;      :BLOG:     sacha
;;      :POSTID:   29685
;;      :CUSTOM_ID:  word-level
;;      :END:

;; #+begin_update
;; 2022-10-26: [[/blog/2022/10/subed-el-word-level-timing-improvements/][Merged word-level timing support into subed.el]], so I don't need my old caption functions.
;; #+end_update

;; #+begin_update
;; 2022-04-18: Switched to using yt-dlp.
;; #+end_update

;; I like to split captions at logical points, such as at the end of a
;; phrase or sentence. At first, I used subed.el to play the video for
;; the caption, pausing it at the appropriate point and then calling
;; =subed-split-subtitle= to split at the playback position. Then I
;; modified =subed-split-subtitle= to split at the video position that's
;; proportional to the text position, so that it's roughly in the right
;; spot even if I'm not currently listening. That got me most of the way
;; to being able to quickly edit subtitles.

;; It turns out that word-level timing is actually available from YouTube
;; if I download the autogenerated SRV2 file using yt-dlp, which I
;; can do with the following function:


;; [[file:Sacha.org::#word-level][Using word-level timing information when editing subtitles or captions in Emacs:1]]
(defun my-caption-download-srv2 (id)
  (interactive "MID: ")
  (require 'subed-word-data)
  (when (string-match "v=\\([^&]+\\)" id) (setq id (match-string 1 id)))
  (let ((default-directory "/tmp"))
    (call-process "yt-dlp" nil nil nil "--write-auto-sub" "--write-sub" "--no-warnings" "--sub-lang" "en" "--skip-download" "--sub-format" "srv2"
                  (concat "https://youtu.be/" id))
    (subed-word-data-load-from-file (my-latest-file "/tmp" "\\.srv2\\'"))))
;; Using word-level timing information when editing subtitles or captions in Emacs:1 ends here



;; #+begin_update
;; 2022-10-26: I can also generate a SRV2-ish file using
;; torchaudio, which I can then load with
;; ~subed-word-data-load-from-file~.
;; #+end_update


;; [[file:Sacha.org::#word-level][Using word-level timing information when editing subtitles or captions in Emacs:2]]
(defun my-caption-fix-common-errors (data)
  (mapc (lambda (o)
          (mapc (lambda (e)
                  (when (string-match (concat "\\<" (regexp-opt (if (listp e) (seq-remove (lambda (s) (string= "" s)) e)
                                                                  (list e)))
                                              "\\>")
                                      (alist-get 'text o))
                    (map-put! o 'text (replace-match (car (if (listp e) e (list e))) t t (alist-get 'text o)))))
                my-subed-common-edits))
        data))
;; Using word-level timing information when editing subtitles or captions in Emacs:2 ends here



;; Assuming I start editing from the beginning of the file, then the part
;; of the captions file after point is mostly unedited. That means I can
;; match the remainder of the current caption with the word-level timing
;; to try to figure out the time to use when splitting the subtitle,
;; falling back to the proportional method if the data is not available.


;; [[file:Sacha.org::#word-level][Using word-level timing information when editing subtitles or captions in Emacs:3]]
(defun subed-avy-set-up-actions ()
  (interactive)
  (make-local-variable 'avy-dispatch-alist)
  (add-to-list
   'avy-dispatch-alist
   (cons ?, 'subed-split-subtitle)))

(use-package subed
  :if my-laptop-p
  :load-path "~/proj/subed/subed"
  :mode
  (("\\.vtt\\'" . subed-vtt-mode)
   ("\\.srt\\'" . subed-srt-mode)
   ("\\.ass\\'" . subed-ass-mode))
  :init
  (autoload 'subed-vtt-mode "subed-vtt" nil t)
  (autoload 'subed-srt-mode "subed-srt" nil t)
  (autoload 'subed-ass-mode "subed-ass" nil t)
  (autoload 'subed-txt-mode "subed-txt" nil t)
	(require 'subed-autoloads)
  :hook
  (subed-mode . display-fill-column-indicator-mode)
  (subed-mode . subed-avy-set-up-actions)
  :bind
  (:map subed-mode-map
        ("M-," . subed-split-subtitle)
        ("M-." . subed-merge-dwim))
	:config
	;; Remember cursor position between sessions
	(add-hook 'subed-mode-hook 'save-place-local-mode)
	;; Some reasonable defaults
	;; Replay subtitles as you adjust their start or stop time with M-[, M-], M-{, or M-}
	(add-hook 'subed-mode-hook 'subed-enable-replay-adjusted-subtitle)
	;; Loop over subtitles
	(add-hook 'subed-mode-hook 'subed-enable-loop-over-current-subtitle)
	;; Show characters per second
	(add-hook 'subed-mode-hook 'subed-enable-show-cps)
  (add-hook 'subed-mode-hook (lambda () (remove-hook 'before-save-hook 'subed-sort t)))
	(with-eval-after-load 'consult
		(advice-add 'consult-buffer :around
								(lambda (f &rest r)
									(let ((subed-auto-play-media nil))
										(apply f r)))))

	)
;; Using word-level timing information when editing subtitles or captions in Emacs:3 ends here



;; That way, I can use the word-level timing information for most of the
;; reformatting, but I can easily replay segments of the video if I'm
;; unsure about a word that needs to be changed.

;; If I want to generate a VTT based on the caption data, breaking it at
;; certain words, these functions help:


;; [[file:Sacha.org::#word-level][Using word-level timing information when editing subtitles or captions in Emacs:4]]
(defvar my-caption-breaks
  '("the" "this" "we" "we're" "I" "finally" "but" "and" "when")
  "List of words to try to break at.")
(defun my-caption-make-groups (list &optional threshold)
  (let (result
        current-item
        done
        (current-length 0)
        (limit (or threshold 70))
        (lower-limit 30)
        (break-regexp (concat "\\<" (regexp-opt my-caption-breaks) "\\>")))
    (while list
      (cond
       ((null (car list)))
       ((string-match "^\n*$" (alist-get 'text (car list)))
        (push (cons '(text . " ") (car list)) current-item)
        (setq current-length (1+ current-length)))
       ((< (+ current-length (length (alist-get 'text (car list)))) limit)
        (setq current-item (cons (car list) current-item)
              current-length (+ current-length (length (alist-get 'text (car list))) 1)))
       (t (setq done nil)
          (while (not done)
          (cond
           ((< current-length lower-limit)
            (setq done t))
           ((and (string-match break-regexp (alist-get 'text (car current-item)))
                 (not (string-match break-regexp (alist-get 'text (cadr current-item)))))
            (setq current-length (- current-length (length (alist-get 'text (car current-item)))))
            (push (pop current-item) list)
            (setq done t))
           (t
            (setq current-length (- current-length (length (alist-get 'text (car current-item)))))
            (push (pop current-item) list))))
          (push nil list)
          (setq result (cons (reverse current-item) result) current-item nil current-length 0)))
      (setq list (cdr list)))
    (reverse result)))

(defun my-caption-format-as-subtitle (list &optional word-timing)
  "Turn a LIST of the form (((start . ms) (end . ms) (text . s)) ...) into VTT.
If WORD-TIMING is non-nil, include word-level timestamps."
  (format "%s --> %s\n%s\n\n"
          (subed-vtt--msecs-to-timestamp (alist-get 'start (car list)))
          (subed-vtt--msecs-to-timestamp (alist-get 'end (car (last list))))
          (s-trim (mapconcat (lambda (entry)
                               (if word-timing
                                   (format " <%s>%s"
                                           (subed-vtt--msecs-to-timestamp (alist-get 'start entry))
                                           (string-trim (alist-get 'text entry)))
                                 (alist-get 'text entry)))
                             list ""))))

(defun my-caption-to-vtt (&optional data)
  (interactive)
  (with-temp-file "captions.vtt"
    (insert "WEBVTT\n\n"
            (mapconcat
             (lambda (entry) (my-caption-format-as-subtitle entry))
             (my-caption-make-groups
              (or data (my-caption-fix-common-errors subed-word-data--cache)))
             ""))))
;; Using word-level timing information when editing subtitles or captions in Emacs:4 ends here



;; Sometimes I just want the text so that I can use [[dotemacs:transcript-keywords][an audio braindump as the starting point for a blog post or for notes]]. WhisperX is way more accurate than Google Recorder, so that will probably be easier once I update my workflow for that.

;; Sometimes I want to make an edited audio file that sounds smooth so that I can use it in a podcast, a video, or some audio notes. For that, I'd like word-level timing data so that I can cut out words or sections. Aeneas didn't give me word-level timestamps, but WhisperX does, so I can get the time information before I start editing. I can extract the word timestamps from the JSON like this:


;; [[file:Sacha.org::#whisperx][Using WhisperX to get word-level timestamps for audio editing with Emacs and subed-record:3]]
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
;; Using WhisperX to get word-level timestamps for audio editing with Emacs and subed-record:3 ends here



;; Here's my old code for parsing the highlighted VTT or SRT files that underline each word:


;; [[file:Sacha.org::#whisperx][Using WhisperX to get word-level timestamps for audio editing with Emacs and subed-record:4]]
(defun my-subed-load-word-data-from-whisperx-highlights (file)
	"Return a list of word cues from FILE.
FILE should be a VTT or SRT file produced by whisperx with the
--highlight_words True option."
	(seq-keep (lambda (sub)
							(when (string-match "<u>\\(.+?\\)</u>" (elt sub 3))
								(setf (elt sub 3) (match-string 1 (elt sub 3)))
								sub))
						(subed-parse-file file)))

(defun my-subed-word-tsv-from-whisperx-highlights (file)
	(interactive "FVTT: ")
	(with-current-buffer (find-file-noselect (concat (file-name-nondirectory file) ".tsv"))
		(erase-buffer)
		(subed-tsv-mode)
		(subed-auto-insert)
    (mapc (lambda (sub) (apply #'subed-append-subtitle nil (cdr sub)))
					(my-subed-load-word-data-from-whisperx-highlights file))
		(switch-to-buffer (current-buffer))))
;; Using WhisperX to get word-level timestamps for audio editing with Emacs and subed-record:4 ends here



;; I like to use the TSV format for this one because
;; it's easy to scan down the right side.
;; Incidentally, this format is compatible with
;; [[https://www.audacityteam.org/][Audacity]] labels, so I could import that there if I
;; wanted. I like Emacs much more, though. I'm used
;; to having all my keyboard shortcuts at hand.

;; #+begin_example
;; 0.427000	0.507000	I
;; 0.587000	0.887000	often
;; 0.987000	1.227000	need
;; 1.267000	1.508000	to...
;; 4.329000	4.429000	I
;; 4.469000	4.869000	sometimes
;; 4.950000	5.170000	need
;; 5.210000	5.410000	to
;; 5.530000	6.090000	replace
;; #+end_example

;; Once I've deleted the words I don't want to
;; include, I can merge subtitles for phrases so that
;; I can keep the pauses between words. A quick
;; heuristic is to merge subtitles if they don't have
;; much of a pause between them.


;; [[file:Sacha.org::#whisperx][Using WhisperX to get word-level timestamps for audio editing with Emacs and subed-record:5]]
(defvar my-subed-merge-close-subtitles-threshold 500)
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
;; Using WhisperX to get word-level timestamps for audio editing with Emacs and subed-record:5 ends here

;; Showing captions
;; :PROPERTIES:
;; :CUSTOM_ID: showing-captions
;; :END:
;; This tidbit displays a buffer with the text of the subtitles so that I can quickly skim it.


;; [[file:Sacha.org::#showing-captions][Showing captions:1]]
(defun my-caption-show (url)
  (interactive (list
                (let ((link (and (derived-mode-p 'org-mode)
                                 (org-element-context))))
                  (if (and link
                           (eq (org-element-type link) 'link))
                      (read-string (format "URL (%s): " (org-element-property :raw-link link)) nil nil
                                   (org-element-property :raw-link link))
                    (read-string "URL: ")))))
  (when (and (listp url) (org-element-property :raw-link url)) (setq url (org-element-property :raw-link url)))
  (delete-other-windows)
  (split-window-right)
	(if (string-match "http" url)
			(with-current-buffer-window "*Captions*"
					'display-buffer-same-window
					nil
				(org-mode)
				(save-excursion
					(my-org-insert-youtube-video-with-transcript url)))
		(unless (file-exists-p (concat (file-name-sans-extension url) ".vtt"))
			(my-deepgram-recognize-audio url))
		(find-file (concat (file-name-sans-extension url) ".vtt"))))
;; Showing captions:1 ends here

;; Edit text
;; :PROPERTIES:
;; :CUSTOM_ID: edit-text
;; :END:


;; [[file:Sacha.org::#edit-text][Edit text:1]]
(defcustom my-subed-common-edits
	'("I"
    "I've"
    "I'm"
    "Mendeley"
    "JavaScript"
    "RSS"
    ("going to" "gonna")
    ("want to" "wanna")
    ("transient" "transit")
    ("" "uh" "um")
    ("Magit" "maggot")
    ("Emacs" "e-max" "emex" "emax" "bmx" "imax")
    ("Emacs News" "emacs news")
		("Emacs Lisp" "emacs list")
    ("EmacsConf" "emacs conf" "imaxconf")
    ("ivy" "iv")
    ("UI" "ui")
    ("TECO" "tico")
    ("org-roam" "orgrim" "orgrom")
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
;; Edit text:1 ends here

;; [[file:Sacha.org::#edit-text][Edit text:2]]
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

(defun my-subed-fix-common-error ()
  (interactive)
  (let ((entry (my-subed-find-next-fix-point)))
    (replace-match (if (listp entry) (car entry) entry) t t)))

(defun my-subed-fix-common-errors ()
  (interactive)
  (let (done entry correction)
    (while (and
            (not done)
            (setq entry (my-subed-find-next-fix-point)))
      (setq correction (if (listp entry) (car entry) entry))
      (let* ((c (read-char (format "%s (yn.): " correction))))
        (cond
         ((= c ?y) (replace-match correction t t))
         ((= c ?n) (goto-char (match-end 0)))
         ((= c ?j) (subed-mpv-jump-to-current-subtitle))
         ((= c ?.) (setq done t)))
        ))))
;; Edit text:2 ends here

;; Working with media
;; :PROPERTIES:
;; :CUSTOM_ID: working-with-media
;; :END:

;; You can get these from https://github.com/sachac

;; [[file:Sacha.org::#working-with-media][Working with media:1]]
(use-package waveform :load-path "~/proj/waveform-el" :if my-laptop-p)
(use-package compile-media :load-path "~/proj/compile-media" :if my-laptop-p
)
;; Working with media:1 ends here

;; TODO Working with sections defined by NOTE comments
;; SCHEDULED: <2024-10-28 Mon>
;; :PROPERTIES:
;; :CREATED:  [024-05-31 Fri 15:1]
;; :END:


;; [[file:Sacha.org::*Working with sections defined by NOTE comments][Working with sections defined by NOTE comments:1]]
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
;; Working with sections defined by NOTE comments:1 ends here

;; Split up oops better
;; :PROPERTIES:
;; :CUSTOM_ID: split-up-oops-better
;; :END:


;; [[file:Sacha.org::#split-up-oops-better][Split up oops better:1]]
(defun my-split-oops ()
	"Look for oops and make it easier to split."
	(interactive)
	(let ((scan-window 300))
		(while (re-search-forward "oops[,\.]?[ \n]+" nil t)
			(let ((start (min (line-beginning-position) (- (point) scan-window)))
						start-search
						found
						search-for)
				(if (bolp)
						(progn
							(backward-char)
							(setq start (min (line-beginning-position) (- (point) scan-window))))
					(insert "\n"))
				(save-excursion
					(setq start-search (point))
					;; look for 1..5 words back
					(goto-char
					 (or
						(cl-loop
						 for n downfrom 5 downto 1
						 do
						 (save-excursion
							 (dotimes (_ n) (forward-word))
							 (setq search-for (downcase (string-trim (buffer-substring start-search (point)))))
							 (goto-char start-search)
							 (when (re-search-backward (regexp-quote search-for) start t)
								 (goto-char (match-beginning 0))
								 (cl-return (point)))))
						(and (call-interactively 'isearch-backward) (point))))
					(insert "\n"))))))
;; Split up oops better:1 ends here

;; [[file:Sacha.org::#split-up-oops-better][Split up oops better:2]]
(setq subed-align-options "task_adjust_boundary_offset_percent=0.5")
;; Split up oops better:2 ends here

;; [[file:Sacha.org::#split-up-oops-better][Split up oops better:3]]
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

(defun my-subed-skip-oops ()
	(interactive)
	(my-subed-delete-oops t))

(defun my-subed-record-wpm ()
	(interactive)
	(let ((wpm (subed-wpm
							(seq-remove (lambda (o) (and (elt o 4) (string-match "skip" (elt o 4))))
													(subed-subtitle-list)))))
		(apply 'message
					  "%d wpm (%d words / %.1f minutes)" wpm)))

(defun my-subed-prepare-for-cleaning ()
	(interactive)
	(my-subed-delete-oops)
	(goto-char (point-min))
	(subed-forward-subtitle-id)
	(subed-set-subtitle-comment (concat "#+OUTPUT: " (file-name-sans-extension (buffer-file-name)) "-cleaned.opus")))

(defvar my-phone-recording-dir "~/sync/Phone")
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
;; Split up oops better:3 ends here

;; TODO Org Mode: Insert YouTube video with separate captions       :emacs:
;;      :PROPERTIES:
;;      :ID:       o2b:60850240-1608-46ce-8e36-75f9ffaa5dc5
;;      :POST_DATE: [2021-04-01 Thu 23:43]
;;      :BLOG:     sacha
;;      :POSTID:   29703
;;      :CUSTOM_ID: org-youtube-captions
;;      :END:

;;    I'm playing around with some ideas for making it easier to post a
;;    video with its captions on a webpage or in an Org file so that it's
;;    easier to skim or search.

;;    This requires the =yt-dlp= command. I'm also learning how to use
;;    =dash.el='s threading macro, so you'll need to install that as well if
;;    you want to run it.


;; [[file:Sacha.org::#org-youtube-captions][Org Mode: Insert YouTube video with separate captions:1]]
(require 'dash)

(defun my-msecs-to-timestamp (msecs)
  "Convert MSECS to string in the format HH:MM:SS.MS."
  (concat (format-seconds "%02h:%02m:%02s" (/ msecs 1000))
          "." (format "%03d" (mod msecs 1000))))

(defun my-org-insert-youtube-video-with-transcript (url)
  (interactive "MURL: ")
  (let* ((id (if (string-match "\\(?:v=\\|youtu\\.be/\\)\\([^&]+\\)" url) (match-string 1 url) url))
         (temp-file (make-temp-name "org-youtube-"))
         (temp-file-name (concat temp-file ".en.srv1"))
         data)
    (when (and (call-process "yt-dlp" nil nil nil
                             "--write-sub" "--write-auto-sub"  "--no-warnings" "--sub-lang" "en" "--skip-download" "--sub-format" "srv1"
                             "-o" temp-file
                             (format "https://youtube.com/watch?v=%s" id))
               (file-exists-p temp-file-name))
      (insert
       (format "#+begin_export html
<iframe width=\"560\" height=\"315\" src=\"https://www.youtube.com/embed/%s\" title=\"YouTube video player\" frameborder=\"0\" allow=\"accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture\" allowfullscreen></iframe>\n#+end_export\n" id)
       "\n"
       (mapconcat (lambda (o)
                    (format "| [[https://youtube.com/watch?v=%s&t=%ss][%s]] | %s |\n"
                            id
                            (dom-attr o 'start)
                            (my-msecs-to-timestamp (* 1000 (string-to-number (dom-attr o 'start))))
                            (->> (dom-text o)
                                 (replace-regexp-in-string "[ \n]+" " ")
                                 (replace-regexp-in-string "&#39;" "'")
                                 (replace-regexp-in-string "&quot;" "\""))))
                  (dom-by-tag (xml-parse-file temp-file-name) 'text)
                  ""))
      (delete-file temp-file-name))))
;; Org Mode: Insert YouTube video with separate captions:1 ends here

;; Export transcript as list


;; [[file:Sacha.org::*Export transcript as list][Export transcript as list:1]]
(cl-defun my-subed-as-org-list-with-times (file &key from to)
	(interactive "FVTT: ")
  (when (stringp from) (setq from (compile-media-timestamp-to-msecs from)))
  (when (stringp to) (setq to (compile-media-timestamp-to-msecs to)))
	(let ((s (mapconcat
						(lambda (o)
							(format "- @@html:<span class=\"audio-time\" data-start=\"%.3f\" data-stop=\"%.3f\">%s</span>@@: *%s*:\n  %s\n\n"
											(/ (plist-get o :start-ms) 1000.0)
											(/ (plist-get o :stop-ms) 1000.0)
											(replace-regexp-in-string "^00:0?\\|\\.[0-9]+$" "" (my-msecs-to-timestamp (plist-get o :start-ms)))
											(plist-get o :comment)
											(string-trim (replace-regexp-in-string
																		"[ \n]+" " "
																		(subed-subtitle-list-text (plist-get o :subtitles))))))
						(my-subed-group-sections
						 (seq-filter (lambda (sub)
													 (and (or (not from) (>= (elt sub 1) from))
																(or (not to) (< (elt sub 2) to))))
												 (subed-parse-file file)))
						"")))
		(if (called-interactively-p 'any)
				(insert s)
			s)))
;; Export transcript as list:1 ends here

;; Removing gaps and merging subtitles
;; :PROPERTIES:
;; :CUSTOM_ID: subed-gaps
;; :END:


;; [[file:Sacha.org::#subed-gaps][Removing gaps and merging subtitles:1]]
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
;; Removing gaps and merging subtitles:1 ends here

;; Prepare for EmacsConf screenshots or recordings
;; :PROPERTIES:
;; :CUSTOM_ID: prepare-for-emacsconf-screenshots-or-recordings
;; :END:


;; [[file:Sacha.org::#prepare-for-emacsconf-screenshots-or-recordings][Prepare for EmacsConf screenshots or recordings:1]]
(defun my-emacsconf-prepare-for-screenshots ()
	(interactive)
	(shell-command "xrandr --output LVDS-1 --mode 1280x720")
	(modus-themes-load-theme 'modus-operandi)
	(my-hl-sexp-update-overlay)
	(set-face-attribute 'default nil :height 170)
	(keycast-mode))

(defun my-emacsconf-back-to-normal ()
	(interactive)
	(shell-command "xrandr --output LVDS-1 --mode 1366x768")
	(modus-themes-load-theme 'modus-vivendi)
	(my-hl-sexp-update-overlay)
	(set-face-attribute 'default nil :height 115)
	(keycast-mode -1))
;; Prepare for EmacsConf screenshots or recordings:1 ends here

;; Preparing to record YouTube shorts
;; :PROPERTIES:
;; :CUSTOM_ID: youtube-shorts
;; :END:


;; [[file:Sacha.org::#youtube-shorts][Preparing to record YouTube shorts:1]]
(defun my-youtube-prepare-for-shorts ()
	(interactive)
	(setq compile-media-output-video-width 1080
				compile-media-output-video-height 1920
				compile-media-output-video-fps 30)
	(shell-command "wmctrl -r :ACTIVE: -e 0,300,0,554,984"))
;; Preparing to record YouTube shorts:1 ends here

;; [[file:Sacha.org::#youtube-shorts][Preparing to record YouTube shorts:2]]
(defun my-prepare-for-landscape ()
	(let ((width 6) (height 9))
		(setq compile-media-output-video-width 1080
					compile-media-output-video-height 1920
					compile-media-output-video-fps 30)
	(shell-command "wmctrl -r :ACTIVE: -e 0,300,0,554,984")
	))
;; Preparing to record YouTube shorts:2 ends here

;; Renaming a set of files


;; [[file:Sacha.org::*Renaming a set of files][Renaming a set of files:1]]
(defun my-rename-fileset (new-prefix files &optional force)
	(interactive (list
								(read-file-name
								 (format "New prefix (%s): "
												 (file-name-base (car (dired-get-marked-files)))))
								(dired-get-marked-files)
								current-prefix-arg))
	(unless force
		(dolist (file files)
			(let ((new-file (concat
											 new-prefix
											 "."
											 (file-name-extension file))))
				(when (file-exists-p new-file)
					(error "%s already exists."
								 new-file)))))
	(dolist (file files)
		(let ((new-file (expand-file-name
										 (concat
											new-prefix
											"."
											(file-name-extension file)))))
			(rename-file file new-file t)))
	(when (derived-mode-p 'dired-mode) (revert-buffer)))
;; Renaming a set of files:1 ends here

;; Elfeed

;; [[file:Sacha.org::*Elfeed][Elfeed:1]]
(use-package elfeed-tube
  :quelpa (elfeed-tube :fetcher github :repo "karthink/elfeed-tube")
  :after elfeed
  :demand t
  :commands
  (elfeed-tube-fetch)
  :config
  ;; (setq elfeed-tube-auto-save-p nil) ;; t is auto-save (not default)
  ;; (setq elfeed-tube-auto-fetch-p t) ;;  t is auto-fetch (default)
  (elfeed-tube-setup)
  :bind (:map elfeed-show-mode-map
              ("F" . elfeed-tube-fetch)
              ([remap save-buffer] . elfeed-tube-save)
              :map elfeed-search-mode-map
              ("F" . elfeed-tube-fetch)
              ([remap save-buffer] . elfeed-tube-save)))
(use-package elfeed-tube-mpv
  :quelpa (elfeed-tube-mpv :fetcher github :repo "karthink/elfeed-tube")
  :bind (:map elfeed-show-mode-map
              ("C-c C-f" . elfeed-tube-mpv-follow-mode)
              ("C-c C-w" . elfeed-tube-mpv-where)))
;; Elfeed:1 ends here

;; [[file:Sacha.org::*Elfeed][Elfeed:2]]
(use-package emms
	:config
	(require 'emms-player-simple)
  (require 'emms-source-file)
  (require 'emms-source-playlist)
	(require 'emms-player-mpv)
	(require 'emms-info-native)
	(require 'emms-info-exiftool)
	(emms-all)
  (add-to-list 'emms-info-functions 'emms-info-native)
  (add-to-list 'emms-info-functions 'emms-info-exiftool)

  (setq emms-player-list '(emms-player-mpv)))
;; Elfeed:2 ends here

;; DONE Scan ~/bin and turn the scripts into interactive commands
;; CLOSED: [2015-12-14 Mon 21:24]
;; :PROPERTIES:
;; :Effort:   0:30
;; :ID:       o2b:39fb2260-d161-4a78-929c-5443f551a5fe
;; :POST_DATE: [2015-12-14 Mon 21:22]
;; :POSTID:   28517
;; :BLOG:     sacha
;; :CUSTOM_ID: scan-bin-and-turn-the-scripts-into-interactive-commands
;; :END:
;; :LOGBOOK:
;; - State "DONE"       from              [2015-12-14 Mon 21:24]
;;   CLOCK: [2015-12-14 Mon 20:51]--[2015-12-14 Mon 21:40] =>  0:49
;; :END:

;; I want to automate little things on my computer so that I don't have
;; to look up command lines or stitch together different applications.
;; Many of these things make sense to turn into shell scripts. That way,
;; I can call them from other programs and assign keyboard shortcuts to
;; them. Still, I spend most of my computer time in Emacs, and I don't
;; want to think about whether I've defined a command in Emacs Lisp or in
;; a shell script. Besides, I like the way [[http://sachachua.com/blog/2014/03/emacs-basics-call-commands-name-m-x-tips-better-completion-using-ido-helm/][Helm]] lets me type parts of
;; commands in order to select and call them.

;; Emacs Lisp allows you to define a macro that results in Emacs Lisp
;; code. In this case, I want to define interactive functions so I can
;; call them with =M-x=. In case I decide to call them from Emacs Lisp,
;; such as =(my-shell/rotate-screen "left")=, I want to be able to pass
;; arguments. I'm also using [[https://github.com/magnars/dash.el][dash.el]] to provide functions like =-filter=
;; and =-not=, although I could rewrite this to just use the standard
;; Emacs Lisp functions.

;; Here's the code that scans a given directory for executable files and
;; creates interactive functions, and some code that calls it for my [[https://github.com/sachac/scripts][~/bin]] directory.


;; [[file:Sacha.org::#scan-bin-and-turn-the-scripts-into-interactive-commands][Scan ~/bin and turn the scripts into interactive commands:1]]
(use-package dash
  :init
  (defmacro my-convert-shell-scripts-to-interactive-commands (directory)
    "Make the shell scripts in DIRECTORY available as interactive commands."
    (cons 'progn
          (-map
           (lambda (filename)
             (let ((function-name (intern (concat "my-shell/" (file-name-nondirectory filename)))))
               `(defun ,function-name (&rest args)
                  (interactive)
                  (cond
                   ((not (called-interactively-p 'any))
                    (shell-command-to-string (mapconcat 'shell-quote-argument (cons ,filename args) " ")))
                   ((region-active-p)
                    (apply 'call-process-region (point) (mark) ,filename nil (if current-prefix-arg t nil) t args))
                   (t
                    (apply 'call-process ,filename nil (if current-prefix-arg t nil) nil args))))))
           (-filter (-not #'file-directory-p)
                    (-filter #'file-executable-p (directory-files directory t))))))
  (my-convert-shell-scripts-to-interactive-commands "~/bin"))
;; Scan ~/bin and turn the scripts into interactive commands:1 ends here

;; CSVs
;; :PROPERTIES:
;; :CUSTOM_ID: csvs
;; :END:


;; [[file:Sacha.org::#csvs][CSVs:1]]
(use-package pcsv)
;; CSVs:1 ends here

;; Whitespace
;; :PROPERTIES:
;; :CUSTOM_ID: whitespace
;; :END:


;; [[file:Sacha.org::#whitespace][Whitespace:1]]
(use-package ws-butler
	:config (ws-butler-global-mode))
;; Whitespace:1 ends here

;; Python
;; :PROPERTIES:
;; :CUSTOM_ID: python
;; :END:

;; [[file:Sacha.org::#python][Python:1]]
(use-package elpy
	:config
	(elpy-enable)
	(setq python-shell-interpreter "ipython3"
				python-shell-interpreter-args "-i --simple-prompt")
	(setq python-indent-offset 4)
	(add-hook 'python-mode-hook
      (lambda ()
        (setq-local tab-width 4)
				(setq-local python-flymake-command '("flake8" "--append-config" "/home/sacha/.config/flake8" "-"))
				(setq-local python-check-command "flake8 --append-config /home/sacha/.config/flake8"))
			70)
	)
;; (use-package lsp-pyright
;;   :ensure t
;;   :hook (python-mode . (lambda ()
;;                           (require 'lsp-pyright)
;;                           (lsp))))
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (when (eq major-mode 'compilation-mode)
		(let ((inhibit-read-only t))
			(ansi-color-apply-on-region compilation-filter-start (point-max)))))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
;; Python:1 ends here

;; Web development
;; :PROPERTIES:
;; :CUSTOM_ID: web-development
;; :END:

;; From [[https://themkat.net/2022/10/04/emacs_web_mode_mixed.html][Mixed content HTML files in Emacs web-mode with context-aware completion and documentation]]:

;; [[file:Sacha.org::#web-development][Web development:1]]
(use-package tide)
(use-package css-eldoc)
(defun themkat/activate-tide ()
  (interactive)
  (tide-setup)
  (eldoc-mode 1)
  (tide-hl-identifier-mode 1))

(defun themkat/complete-web-mode ()
  (interactive)
  (let ((current-scope (web-mode-language-at-pos (point))))
    (cond ((string-equal "javascript" current-scope)
	   (company-tide 'interactive))
	  ((string-equal "css" current-scope)
	   (company-css 'interactive))
	  (t
	   (company-dabbrev-code 'interactive)))))

(defun themkat/eldoc-web-mode ()
  (let ((current-scope (web-mode-language-at-pos (point))))
    (cond ((string-equal "javascript" current-scope)
	   (tide-eldoc-function))
	  ((string-equal "css" current-scope)
	   (css-eldoc-function))
	  (t
	   nil))))
(defun themkat/setup-web-mode-mixed ()
  (web-mode)
  (themkat/activate-tide)
  (setq-local eldoc-documentation-function #'themkat/eldoc-web-mode))
;; Web development:1 ends here

;; [[file:Sacha.org::#web-development][Web development:2]]
;; from FAQ at http://web-mode.org/ for smartparens

;; Avoid lockfiles because they mess up React projects
(when my-laptop-p
  (setq create-lockfiles nil))

(defun my-sp-web-mode-is-code-context (id action context)
  (when (and (eq action 'insert)
             (not (or (get-text-property (point) 'part-side)
                      (get-text-property (point) 'block-side))))
    t))

(use-package web-mode
  :if my-laptop-p
  :mode "\\(\\.html?\\|\\.njk\\)\\'"
	:custom
  (web-mode-enable-current-element-highlight t)
	(web-mode-markup-indent-offset 2)
  (web-mode-code-indent-offset 2)
	(web-mode-enable-auto-pairing nil)
  (web-mode-ac-sources-alist
   '(("css" . (ac-source-css-property))
     ("html" . (ac-source-words-in-buffer ac-source-abbrev))))
	:mode
  (("\\.html?$" . themkat/setup-web-mode-mixed))
	:bind
  ([(control return)] . themkat/complete-web-mode))
;; Web development:2 ends here

;; LSP
;; :PROPERTIES:
;; :CUSTOM_ID: lsp
;; :END:
;; https://emacs-lsp.github.io/lsp-mode/tutorials/reactjs-tutorial/
;; https://www.mattduck.com/lsp-python-getting-started.html


;; [[file:Sacha.org::#lsp][LSP:1]]
(use-package lsp-mode
  :if my-laptop-p
  :config
  (setq lsp-headerline-breadcrumb-enable t
        gc-cons-threshold (* 100 1024 1024)
        read-process-output-max (* 1024 1024)
        company-idle-delay 0.5
        company-minimum-prefix-length 1
        create-lockfiles nil ;; lock files will kill `npm start'
        )
  (lsp-register-custom-settings
   '(("pyls.plugins.pyls_mypy.enabled" t t)
     ("pyls.plugins.pyls_mypy.live_mode" nil t)
     ("pyls.plugins.pyls_black.enabled" t t)
     ("pyls.plugins.pyls_isort.enabled" t t)))
	(add-to-list 'lsp-file-watch-ignored-directories "/blog\\'")
	(add-to-list 'lsp-file-watch-ignored-directories "/_site\\'")
	(add-to-list 'lsp-file-watch-ignored-directories "/_local\\'")
  :hook ((js-mode . lsp)
         (python-mode . lsp)
         (lsp-mode-hook . lsp-enable-which-key-integration)))
(use-package lsp-ui
  :if my-laptop-p
  :commands lsp-ui-mode
  :after lsp-mode)
(use-package dap-mode
  :if my-laptop-p
  :after lsp-mode)
;; LSP:1 ends here

;; Turbo log
;; :PROPERTIES:
;; :CUSTOM_ID: turbo-log
;; :END:


;; [[file:Sacha.org::#turbo-log][Turbo log:1]]
(use-package tree-sitter-langs
  :ensure t
  :defer t)

(use-package tree-sitter
  :ensure t
  :after tree-sitter-langs
  :config
  (global-tree-sitter-mode))

(use-package turbo-log
  :quelpa (turbo-log :fetcher github :repo "Artawower/turbo-log")
  :bind (("C-s-l" . turbo-log-print)
         ("C-s-i" . turbo-log-print-immediately)
         ("C-s-h" . turbo-log-comment-all-logs)
         ("C-s-s" . turbo-log-uncomment-all-logs)
         ("C-s-[" . turbo-log-paste-as-logger)
         ("C-s-]" . turbo-log-paste-as-logger-immediately)
         ("C-s-d" . turbo-log-delete-all-logs))
  :config
  (setq turbo-log-msg-format-template "\"🚀: %s\"")
  (setq turbo-log-allow-insert-without-tree-sitter-p t))
;; Turbo log:1 ends here

;; Tab width of 2 is compact and readable
;; :PROPERTIES:
;; :CUSTOM_ID: tab-width-of-2-is-compact-and-readable
;; :END:

;; [[file:Sacha.org::#tab-width-of-2-is-compact-and-readable][Tab width of 2 is compact and readable:1]]
(setq-default tab-width 2)
;; Tab width of 2 is compact and readable:1 ends here

;; More indentation things
;; :PROPERTIES:
;; :CUSTOM_ID: more-indentation-things
;; :END:

;; From https://github.com/purcell/emacs.d/blob/master/lisp/init-editing-utils.el


;; [[file:Sacha.org::#more-indentation-things][More indentation things:1]]
(defun sanityinc/kill-back-to-indentation ()
  "Kill from point back to the first non-whitespace character on the line."
  (interactive)
  (let ((prev-pos (point)))
    (back-to-indentation)
    (kill-region (point) prev-pos)))
(bind-key "C-M-<backspace>" 'sanityinc/kill-back-to-indentation)
;; More indentation things:1 ends here

;; Alignment
;; :PROPERTIES:
;; :CUSTOM_ID: alignment
;; :END:
;;     From https://blog.lambda.cx/posts/emacs-align-columns/

;; [[file:Sacha.org::#alignment][Alignment:1]]
(defun my-align-non-space (BEG END)
  "Align non-space columns in region BEG END."
  (interactive "r")
  (align-regexp BEG END "\\(\\s-*\\)\\S-+" 1 1 t))
;; Alignment:1 ends here

;; YAML
;; :PROPERTIES:
;; :CUSTOM_ID: yaml
;; :END:


;; [[file:Sacha.org::#yaml][YAML:1]]
(use-package yaml-mode
  :if my-laptop-p
  :mode "\\.yml\\'")
;; YAML:1 ends here

;; Expand region with expreg
;; :PROPERTIES:
;; :CUSTOM_ID: expreg
;; :END:

;; This is something I have to get the hang of too. It gradually expands the selection. Handy for Emacs Lisp.


;; [[file:Sacha.org::#expreg][Expand region with expreg:1]]
(use-package expreg
  :defer t
  :bind
	("C-=" . expreg-expand)
	("C-+" . expreg-contract)
  ("C-<prior>" . expreg-expand)
  ("C-<next>" . expreg-contract))
;; Expand region with expreg:1 ends here

;; Compilation
;; :PROPERTIES:
;; :CUSTOM_ID: compilation
;; :END:


;; [[file:Sacha.org::#compilation][Compilation:1]]
(eval-after-load 'python-mode
  '(bind-key "C-c C-c" 'compile python-mode-map))
;; Compilation:1 ends here

;; Emacs Lisp
;; :PROPERTIES:
;; :CUSTOM_ID: emacs-lisp
;; :END:

;; Autocompile, but don't interrupt me with native compilation warnings.


;; [[file:Sacha.org::#emacs-lisp][Emacs Lisp:1]]
(use-package auto-compile
  :if my-laptop-p
  :config (auto-compile-on-load-mode))
(setq native-comp-async-report-warnings-errors nil)
;; Emacs Lisp:1 ends here



;; Memoize is handy for improving the performance when I use slow functions multiple times.

;; [[file:Sacha.org::#emacs-lisp][Emacs Lisp:2]]
(use-package memoize)
;; Emacs Lisp:2 ends here

;; [[file:Sacha.org::#emacs-lisp][Emacs Lisp:3]]
(setq eval-expression-print-length nil)
(setq print-length nil)
(setq edebug-print-length nil)
(defun my-set-sentence-end-double-space ()
	(setq-local sentence-end-double-space t))
(add-hook 'emacs-lisp-mode-hook
					'my-set-sentence-end-double-space)
;; Emacs Lisp:3 ends here

;; Easily override existing functions
;; :PROPERTIES:
;; :CUSTOM_ID: easily-override-existing-functions
;; :END:


;; [[file:Sacha.org::#easily-override-existing-functions][Easily override existing functions:1]]
(defun my-override-function (symbol)
	(interactive (list (completing-read
											"Function: "
											#'help--symbol-completion-table
											#'fboundp
											'confirm nil nil)))
	(let (function-body function-name)
		(save-window-excursion
			(find-function (intern symbol))
			(setq function-name (lisp-current-defun-name))
			(setq function-body (buffer-substring (point)
																						(progn (forward-sexp) (point)))))
		(save-excursion
			(insert function-body (format "\n\n(advice-add '%s :around 'my-%s)\n" function-name function-name)))
		(save-excursion
			(forward-char 1)
			(forward-sexp 1)
			(skip-syntax-forward " ")
			(insert "my-")
			(forward-sexp 1)
			(skip-syntax-forward " ")
			(forward-char 1)
			(insert "_ "))))
;; Easily override existing functions:1 ends here

;; Lispy
;; :PROPERTIES:
;; :CUSTOM_ID: lispy
;; :END:

;; [[file:Sacha.org::#lispy][Lispy:1]]
(use-package lispy :hook (emacs-lisp-mode . lispy-mode))
;; Lispy:1 ends here

;; Emacs: Making a hydra cheatsheet for Lispy                      :emacs:
;;      :PROPERTIES:
;;      :ID:       o2b:912426e0-a60e-4a60-adac-c7781a0fa8eb
;;      :POST_DATE: [2021-04-13 Tue 00:57]
;;      :BLOG:     sacha
;;      :POSTID:   29718
;;      :CUSTOM_ID: hydra-lispy
;;      :END:

;;    I wanted to get the hang of Lispy thanks to Leo Vivier's presentation
;;    at EmacsSF, but there are [[https://oremacs.com/lispy/][a lot of keyboard shortcuts to explore]].
;;    In [[https://karl-voit.at/2021/04/10/GLT21-emacs-org-features/][Karl Voit's demo of Org Mode at GLT21]], he showed how he uses
;;    Hydra to make cheat sheets. That makes perfect sense, of course, as
;;    Hydra can display text and allow you to run commands while the text
;;    is displayed. I wanted to make a Hydra that would show me
;;    categorized commands to make it easier to look up and eventually
;;    remember them. I also wanted to skip the commands that I already knew or
;;    that I didn't want to focus on just yet.

;;    Fortunately, the function reference had a link to [[https://raw.githubusercontent.com/abo-abo/lispy/gh-pages/index.org][the Org file used to generate it]].
;;    I copied the tables, merged them together, named them
;;    with =#+NAME: bindings=, replaced the links with plain text, and added
;;    a third column with the category I wanted to put commands into.

;;    #+begin_my_details :summary Bindings
;;    #+NAME: bindings
;;    | key | function                      | column   |
;;    |-----+-------------------------------+----------|
;;    | <   | lispy-barf                    |          |
;;    | A   | lispy-beginning-of-defun      |          |
;;    | j   | lispy-down                    |          |
;;    | Z   | lispy-edebug-stop             |          |
;;    | B   | lispy-ediff-regions           |          |
;;    | G   | lispy-goto-local              |          |
;;    | h   | lispy-left                    |          |
;;    | N   | lispy-narrow                  |          |
;;    | y   | lispy-occur                   |          |
;;    | o   | lispy-other-mode              |          |
;;    | J   | lispy-outline-next            |          |
;;    | K   | lispy-outline-prev            |          |
;;    | P   | lispy-paste                   |          |
;;    | l   | lispy-right                   |          |
;;    | I   | lispy-shifttab                |          |
;;    | >   | lispy-slurp                   |          |
;;    | SPC | lispy-space                   |          |
;;    | xB  | lispy-store-region-and-buffer |          |
;;    | u   | lispy-undo                    |          |
;;    | k   | lispy-up                      |          |
;;    | v   | lispy-view                    |          |
;;    | V   | lispy-visit                   |          |
;;    | W   | lispy-widen                   |          |
;;    | D   | pop-tag-mark                  |          |
;;    | x   | see                           |          |
;;    | L   | unbound                       |          |
;;    | U   | unbound                       |          |
;;    | X   | unbound                       |          |
;;    | Y   | unbound                       |          |
;;    | H   | lispy-ace-symbol-replace      | Edit     |
;;    | c   | lispy-clone                   | Edit     |
;;    | C   | lispy-convolute               | Edit     |
;;    | n   | lispy-new-copy                | Edit     |
;;    | O   | lispy-oneline                 | Edit     |
;;    | r   | lispy-raise                   | Edit     |
;;    | R   | lispy-raise-some              | Edit     |
;;    | \   | lispy-splice                  | Edit     |
;;    | S   | lispy-stringify               | Edit     |
;;    | i   | lispy-tab                     | Edit     |
;;    | xj  | lispy-debug-step-in           | Eval     |
;;    | xe  | lispy-edebug                  | Eval     |
;;    | xT  | lispy-ert                     | Eval     |
;;    | e   | lispy-eval                    | Eval     |
;;    | E   | lispy-eval-and-insert         | Eval     |
;;    | xr  | lispy-eval-and-replace        | Eval     |
;;    | p   | lispy-eval-other-window       | Eval     |
;;    | q   | lispy-ace-paren               | Move     |
;;    | z   | lispy-knight                  | Move     |
;;    | s   | lispy-move-down               | Move     |
;;    | w   | lispy-move-up                 | Move     |
;;    | t   | lispy-teleport                | Move     |
;;    | Q   | lispy-ace-char                | Nav      |
;;    | -   | lispy-ace-subword             | Nav      |
;;    | a   | lispy-ace-symbol              | Nav      |
;;    | b   | lispy-back                    | Nav      |
;;    | d   | lispy-different               | Nav      |
;;    | f   | lispy-flow                    | Nav      |
;;    | F   | lispy-follow                  | Nav      |
;;    | g   | lispy-goto                    | Nav      |
;;    | xb  | lispy-bind-variable           | Refactor |
;;    | xf  | lispy-flatten                 | Refactor |
;;    | xc  | lispy-to-cond                 | Refactor |
;;    | xd  | lispy-to-defun                | Refactor |
;;    | xi  | lispy-to-ifs                  | Refactor |
;;    | xl  | lispy-to-lambda               | Refactor |
;;    | xu  | lispy-unbind-variable         | Refactor |
;;    | M   | lispy-multiline               | Other    |
;;    | xh  | lispy-describe                | Other    |
;;    | m   | lispy-mark-list               | Other    |
;;    #+end_my_details


;;    I wrote this Emacs Lisp code with the header arguments =#+begin_src emacs-lisp :var bindings=bindings :colnames yes=:


;; [[file:Sacha.org::#hydra-lispy][Emacs: Making a hydra cheatsheet for Lispy:1]]
(let ((bindings '(("<" "lispy-barf" "") ("A" "lispy-beginning-of-defun" "") ("j" "lispy-down" "") ("Z" "lispy-edebug-stop" "") ("B" "lispy-ediff-regions" "") ("G" "lispy-goto-local" "") ("h" "lispy-left" "") ("N" "lispy-narrow" "") ("y" "lispy-occur" "") ("o" "lispy-other-mode" "") ("J" "lispy-outline-next" "") ("K" "lispy-outline-prev" "") ("P" "lispy-paste" "") ("l" "lispy-right" "") ("I" "lispy-shifttab" "") (">" "lispy-slurp" "") ("SPC" "lispy-space" "") ("xB" "lispy-store-region-and-buffer" "") ("u" "lispy-undo" "") ("k" "lispy-up" "") ("v" "lispy-view" "") ("V" "lispy-visit" "") ("W" "lispy-widen" "") ("D" "pop-tag-mark" "") ("x" "see" "") ("L" "unbound" "") ("U" "unbound" "") ("X" "unbound" "") ("Y" "unbound" "") ("H" "lispy-ace-symbol-replace" "Edit") ("c" "lispy-clone" "Edit") ("C" "lispy-convolute" "Edit") ("n" "lispy-new-copy" "Edit") ("O" "lispy-oneline" "Edit") ("r" "lispy-raise" "Edit") ("R" "lispy-raise-some" "Edit") ("\\" "lispy-splice" "Edit") ("S" "lispy-stringify" "Edit") ("i" "lispy-tab" "Edit") ("xj" "lispy-debug-step-in" "Eval") ("xe" "lispy-edebug" "Eval") ("xT" "lispy-ert" "Eval") ("e" "lispy-eval" "Eval") ("E" "lispy-eval-and-insert" "Eval") ("xr" "lispy-eval-and-replace" "Eval") ("p" "lispy-eval-other-window" "Eval") ("q" "lispy-ace-paren" "Move") ("z" "lispy-knight" "Move") ("s" "lispy-move-down" "Move") ("w" "lispy-move-up" "Move") ("t" "lispy-teleport" "Move") ("Q" "lispy-ace-char" "Nav") ("-" "lispy-ace-subword" "Nav") ("a" "lispy-ace-symbol" "Nav") ("b" "lispy-back" "Nav") ("d" "lispy-different" "Nav") ("f" "lispy-flow" "Nav") ("F" "lispy-follow" "Nav") ("g" "lispy-goto" "Nav") ("xb" "lispy-bind-variable" "Refactor") ("xf" "lispy-flatten" "Refactor") ("xc" "lispy-to-cond" "Refactor") ("xd" "lispy-to-defun" "Refactor") ("xi" "lispy-to-ifs" "Refactor") ("xl" "lispy-to-lambda" "Refactor") ("xu" "lispy-unbind-variable" "Refactor") ("M" "lispy-multiline" "Other") ("xh" "lispy-describe" "Other") ("m" "lispy-mark-list" "Other"))))
(eval
 (append
  '(defhydra my-lispy-cheat-sheet (:hint nil :foreign-keys run)
     ("<f14>" nil "Exit" :exit t))
  (cl-loop for x in bindings
           unless (string= "" (elt x 2))
           collect
           (list (car x)
                 (intern (elt x 1))
                 (when (string-match "lispy-\\(?:eval-\\)?\\(.+\\)"
                                     (elt x 1))
                   (match-string 1 (elt x 1)))
                 :column
                 (elt x 2)))))
(with-eval-after-load "lispy"
  (define-key lispy-mode-map (kbd "<f14>") 'my-lispy-cheat-sheet/body)
  (define-key lispy-mode-map (kbd "C-?") 'my-lispy-cheat-sheet/body))
(with-eval-after-load 'evil-lispy
  (evil-define-key nil evil-lispy-mode-map (kbd "<f14>") 'my-lispy-cheat-sheet/body))
)
;; Emacs: Making a hydra cheatsheet for Lispy:1 ends here

;; Smartparens mode                                                 :drill:
;; :PROPERTIES:
;; :CUSTOM_ID: smartparens-mode
;; :END:


;; [[file:Sacha.org::#smartparens-mode][Smartparens mode:1]]
(use-package smartparens
  :if my-laptop-p
  :config
  (progn
    ;(require 'smartparens-config)
    ;(add-hook 'emacs-lisp-mode-hook 'smartparens-mode)
    ;(add-hook 'emacs-lisp-mode-hook 'show-smartparens-mode)

      ;;;;;;;;;;;;;;;;;;;;;;;;
    ;; keybinding management

    (define-key sp-keymap (kbd "C-c s r n") 'sp-narrow-to-sexp)
    (define-key sp-keymap (kbd "C-M-f") 'sp-forward-sexp)
    (define-key sp-keymap (kbd "C-M-b") 'sp-backward-sexp)
    (define-key sp-keymap (kbd "C-M-d") 'sp-down-sexp)
    (define-key sp-keymap (kbd "C-M-a") 'sp-backward-down-sexp)
    (define-key sp-keymap (kbd "C-S-a") 'sp-beginning-of-sexp)
    (define-key sp-keymap (kbd "C-S-d") 'sp-end-of-sexp)

    (define-key sp-keymap (kbd "C-M-e") 'sp-up-sexp)
    (define-key emacs-lisp-mode-map (kbd ")") 'sp-up-sexp)
    (define-key sp-keymap (kbd "C-M-u") 'sp-backward-up-sexp)
    (define-key sp-keymap (kbd "C-M-t") 'sp-transpose-sexp)

    (define-key sp-keymap (kbd "C-M-n") 'sp-next-sexp)
    (define-key sp-keymap (kbd "C-M-p") 'sp-previous-sexp)

    (define-key sp-keymap (kbd "C-M-k") 'sp-kill-sexp)
    (define-key sp-keymap (kbd "C-M-w") 'sp-copy-sexp)

    (define-key sp-keymap (kbd "M-<delete>") 'sp-unwrap-sexp)
    (define-key sp-keymap (kbd "M-<backspace>") 'sp-backward-unwrap-sexp)

    (define-key sp-keymap (kbd "C-<right>") 'sp-forward-slurp-sexp)
    (define-key sp-keymap (kbd "C-<left>") 'sp-forward-barf-sexp)
    (define-key sp-keymap (kbd "C-M-<left>") 'sp-backward-slurp-sexp)
    (define-key sp-keymap (kbd "C-M-<right>") 'sp-backward-barf-sexp)

    (define-key sp-keymap (kbd "M-D") 'sp-splice-sexp)
    (define-key sp-keymap (kbd "C-M-<delete>") 'sp-splice-sexp-killing-forward)
    (define-key sp-keymap (kbd "C-M-<backspace>") 'sp-splice-sexp-killing-backward)
    (define-key sp-keymap (kbd "C-S-<backspace>") 'sp-splice-sexp-killing-around)

    (define-key sp-keymap (kbd "C-]") 'sp-select-next-thing-exchange)
    (define-key sp-keymap (kbd "C-<left_bracket>") 'sp-select-previous-thing)
    (define-key sp-keymap (kbd "C-M-]") 'sp-select-next-thing)

    (define-key sp-keymap (kbd "M-F") 'sp-forward-symbol)
    (define-key sp-keymap (kbd "M-B") 'sp-backward-symbol)

    (define-key sp-keymap (kbd "C-c s t") 'sp-prefix-tag-object)
    (define-key sp-keymap (kbd "C-c s p") 'sp-prefix-pair-object)
    (define-key sp-keymap (kbd "C-c s c") 'sp-convolute-sexp)
    (define-key sp-keymap (kbd "C-c s a") 'sp-absorb-sexp)
    (define-key sp-keymap (kbd "C-c s e") 'sp-emit-sexp)
    (define-key sp-keymap (kbd "C-c s p") 'sp-add-to-previous-sexp)
    (define-key sp-keymap (kbd "C-c s n") 'sp-add-to-next-sexp)
    (define-key sp-keymap (kbd "C-c s j") 'sp-join-sexp)
    (define-key sp-keymap (kbd "C-c s s") 'sp-split-sexp)

      ;;;;;;;;;;;;;;;;;;
    ;; pair management

    (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
    (sp-local-pair 'web-mode "<" nil :when '(my-sp-web-mode-is-code-context))

      ;;; markdown-mode
    (sp-with-modes '(markdown-mode gfm-mode rst-mode)
      (sp-local-pair "*" "*" :bind "C-*")
      (sp-local-tag "2" "**" "**")
      (sp-local-tag "s" "```scheme" "```")
      (sp-local-tag "<"  "<_>" "</_>" :transform 'sp-match-sgml-tags))

      ;;; tex-mode latex-mode
    (sp-with-modes '(tex-mode plain-tex-mode latex-mode)
      (sp-local-tag "i" "1d5f8e69396c521f645375107197ea4dfbc7b792quot;<" "1d5f8e69396c521f645375107197ea4dfbc7b792quot;>"))

      ;;; html-mode
    (sp-with-modes '(html-mode sgml-mode web-mode)
      (sp-local-pair "<" ">"))

      ;;; lisp modes
    (sp-with-modes sp--lisp-modes
      (sp-local-pair "(" nil :bind "C-("))))
;; Smartparens mode:1 ends here

;; Edit list                                                        :drill:embark:
;; :PROPERTIES:
;; :ID:       e9147cb0-bad0-421c-9396-4f9045d6ebbb
;; :DRILL_LAST_INTERVAL: 3.86
;; :DRILL_REPEATS_SINCE_FAIL: 2
;; :DRILL_TOTAL_REPEATS: 3
;; :DRILL_FAILURE_COUNT: 2
;; :DRILL_AVERAGE_QUALITY: 2.333
;; :DRILL_EASE: 2.36
;; :DRILL_LAST_QUALITY: 3
;; :DRILL_LAST_REVIEWED: [2013-02-27 Wed 21:18]
;; :CUSTOM_ID: edit-list
;; :END:

;; M-x edit-list makes it easier to edit an Emacs Lisp list.


;; [[file:Sacha.org::#edit-list][Edit list:1]]
(use-package edit-list
	:commands edit-list
	:config
	(with-eval-after-load 'embark
	  (define-key embark-variable-map "l" 'edit-list)))
;; Edit list:1 ends here

;; General-purpose Emacs Lisp libraries
;; :PROPERTIES:
;; :CUSTOM_ID: libraries
;; :END:


;; [[file:Sacha.org::#libraries][General-purpose Emacs Lisp libraries:1]]
(use-package dash :ensure t)
(use-package s :ensure t)
;; General-purpose Emacs Lisp libraries:1 ends here

;; Let's try this setup
;; :PROPERTIES:
;; :CUSTOM_ID: let-s-try-this-setup
;; :END:

;; Copied from https://www.reddit.com/r/emacs/comments/1051bfu/comment/j38ymkn/?utm_source=reddit&utm_medium=web2x&context=3


;; [[file:Sacha.org::#let-s-try-this-setup][Let's try this setup:1]]
(with-eval-after-load 'elisp-mode
	(define-key emacs-lisp-mode-map (kbd "C-c C-d C-d") 'describe-function)
	(define-key emacs-lisp-mode-map (kbd "C-c C-d d") 'describe-function)
	(define-key emacs-lisp-mode-map (kbd "C-c C-k") 'eval-buffer))

(use-package highlight-quoted
  :ensure t
  :hook
  (emacs-lisp-mode . highlight-quoted-mode))

(use-package eros
  :ensure t
  :hook
  (emacs-lisp-mode . eros-mode))

(use-package suggest
  :ensure t
  :defer t)

(use-package ipretty
  :defer t
  :ensure t
  :config
  (ipretty-mode 1))

;; Hide package namespaces
(use-package nameless
  :ensure t
  :hook
  (emacs-lisp-mode .  nameless-mode)
  :custom
  (nameless-global-aliases '())
  (nameless-private-prefix t))

(use-package erefactor
  :ensure t
  :defer t)

(use-package flycheck-package
  :ensure t
  :hook
  (emacs-lisp-mode . flycheck-package-setup))

;; Emacs Lisp Static Analyzer
(use-package elsa
  :defer t
  :ensure t)

(use-package flycheck-elsa
  :ensure t
  :hook
  (emacs-lisp-mode . flycheck-elsa-setup))
;; Let's try this setup:1 ends here

;; Edebug
;; :PROPERTIES:
;; :CUSTOM_ID: edebug
;; :END:

;; From https://xenodium.com/inline-previous-result-and-why-you-should-edebug/


;; [[file:Sacha.org::#edebug][Edebug:1]]
(require 'eros)
(defun adviced:edebug-previous-result (_ &rest r)
  "Adviced `edebug-previous-result'."
  (eros--make-result-overlay edebug-previous-result
    :where (point)
    :duration eros-eval-result-duration))

(advice-add #'edebug-previous-result
            :around
            #'adviced:edebug-previous-result)

(defun adviced:edebug-compute-previous-result (_ &rest r)
  "Adviced `edebug-compute-previous-result'."
  (let ((previous-value (nth 0 r)))
    (if edebug-unwrap-results
        (setq previous-value
              (edebug-unwrap* previous-value)))
    (setq edebug-previous-result
          (edebug-safe-prin1-to-string previous-value))))

(advice-add #'edebug-compute-previous-result
            :around
            #'adviced:edebug-compute-previous-result)
;; Edebug:1 ends here

;; Testing
;; :PROPERTIES:
;; :CUSTOM_ID: testing
;; :END:

;; [[file:Sacha.org::#testing][Testing:1]]
(use-package buttercup
	:hook '(buttercup-minor-mode . my-buttercup-set-up-imenu))

(use-package package-lint)
;; Testing:1 ends here

;; ERT
;; :PROPERTIES:
;; :CUSTOM_ID: ert
;; :END:

;; From https://www.reddit.com/r/emacs/comments/1fub66z/comment/lpyv8no/:

;; [[file:Sacha.org::#ert][ERT:1]]
(defun my-eval-buf-and-run-ert-test-at-point ()
  "Evaluate the current buffer and run the ERT test at point."
  (interactive)
  (save-excursion
    (beginning-of-defun)
    (unless (looking-at "(ert-deftest\\s-+")
      (user-error "Not at an ERT test"))
    (goto-char (match-end 0))
    (let ((test-name (thing-at-point 'symbol)))
      (unless test-name
        (user-error "Couldn't get ERT test name"))
      (eval-buffer)
      (ert-run-tests-interactively test-name))))
;; ERT:1 ends here

;; [[file:Sacha.org::#ert][ERT:2]]
(use-package ert
	:config
	;; handle truncated lists
	(advice-add 'ert--pp-with-indentation-and-newline
							:around (lambda (oldfunc &rest args) (condition-case nil (apply oldfunc args) (error nil))))
	:bind
	(:map
	 emacs-lisp-mode-map ("C-c C-t" . #'my-eval-buf-and-run-ert-test-at-point)))
;; ERT:2 ends here

;; Buttercup
;; :PROPERTIES:
;; :CUSTOM_ID: buttercup
;; :END:

;; [[file:Sacha.org::#buttercup][Buttercup:1]]
(defvar my-buttercup-source-buffer nil)
(defvar my-buttercup-tests nil)
(defun my-buttercup-track-source ()
	(interactive)
	(setq my-buttercup-source-buffer (current-buffer))
	(setq my-buttercup-tests (my-buttercup-tests-and-positions)))

(defun my-buttercup-run-dwim ()
	(interactive)
	(let ((lexical-binding t))
		(if buttercup-minor-mode
				(my-buttercup-run-closest-at-point)
			(buttercup-run))))

;; (advice-remove 'buttercup-run 'my-buttercup-track-source)
(defun my-buttercup-run-closest-at-point ()
  "Run the buttercup suite at point."
  (interactive)
  (let ((lexical-binding t)
				start)
		(setq buttercup-suites nil)
    (save-selected-window
			(save-excursion
				(save-restriction
					;; go up until we find a describe form
					(while (not (looking-at "([[:space:]]*describe[[:space:]]+"))
						(backward-up-list nil t))
					(setq start (point))
					(forward-sexp)
					(narrow-to-region start (point))
					(eval-last-sexp nil)
					(my-buttercup-track-source)))
      (buttercup-run))
    (message "Suite executed successfully")))

(defun my-buttercup-find-test ()
	(interactive)
	(if (re-search-backward (make-string 40 ?=) nil t)
			(progn
				(forward-line)
				(let ((pos (assoc-default (buffer-substring (line-beginning-position)
																										(line-end-position))
																	my-buttercup-tests)))
					(when pos
						(pop-to-buffer my-buttercup-source-buffer)
						(goto-char pos))))
		(let ((tests (my-buttercup-tests-and-positions)))
			(goto-char (assoc-default (completing-read "Test: " tests) tests)))))

(defun my-buttercup-test-name ()
	(save-excursion
		(let (list)
			(condition-case err
					(progn
						(while (not (bobp))
							(let ((form (save-excursion
														(ignore-errors
															(read (current-buffer))))))
								(when (listp form) (and (member (car form) '(describe it)))
											(setq list (cons (cadr form) list)))
								(backward-up-list nil t)))
						(string-join list " "))
				(error
				 (string-join list " "))))))

(defun my-buttercup-tests-and-positions-lookup ()
	"Return a list of test names and points, for easier jumping."
	;; This is a very inefficient implementation. I wonder how to walk the tree...
	(goto-char (point-min))
	(cl-loop while (re-search-forward "([[:space:]]*it[[:space:]]+\"" nil t)
					 collect (cons (my-buttercup-test-name) (point))))

(defun my-buttercup-tests-as-tree ()
	"Return the tests as nested lists ending with (description . point).
Useful as `imenu-create-index-function'."
	(goto-char (point-min))
	(let (result)
		(condition-case _
				(progn
					(down-list)
					(while (not (eobp))
						(cond
						 ((looking-at "describe\\_>")
							(forward-sexp)
							(setq result (cons
														(cons (read (current-buffer))
																	(save-restriction
																		(narrow-to-region
																		 (point)
																		 (progn
																			 (up-list)
																			 (1- (point))))
																		(my-buttercup-tests-as-tree)))
														result)))
						 ((looking-at "it\\_>")
							(forward-sexp)
							(setq result (cons
														(cons (read (current-buffer)) (point))
														result))
							(up-list)
							(down-list))
						 (t
							;; todo, handle other things
							(up-list)
							(down-list)))))
			(scan-error
			 ;; can't go down or forward
			 (reverse result)))))

(defun my-buttercup-set-up-imenu ()
	(setq-local imenu-generic-expression nil)
	(setq-local imenu-create-index-function #'my-buttercup-tests-as-tree))

(defun my-buttercup-tests-and-positions ()
	"Return test names and points to jump to."
	(save-excursion
		(goto-char (point-min))
		(condition-case _
				(progn
					(down-list)
					(let (breadcrumbs sym result)
						(catch 'done
							(while (not (eobp))
								(condition-case _
										(cond
										 ((looking-at "describe[[:space:]]+")
											(forward-sexp)
											(setq breadcrumbs (cons (read (current-buffer)) breadcrumbs))
											;; ignore :var and :var*
											(when (looking-at "[\n[:space:]]+:var\\*?")
												(read (current-buffer))
												(read (current-buffer)))
											(down-list))
										 ((looking-at "it[[:space:]]+")
											(forward-sexp)
											(setq result (cons (cons
																					(string-join
																					 (reverse
																						(delq nil
																									(cons (read (current-buffer)) breadcrumbs)))
																					 " ")
																					(point))
																				 result))
											(up-list)
											(down-list))
										 (t
											;; might be something else that includes describe or it, so we explore it
											(setq breadcrumbs (cons nil breadcrumbs))
											(down-list)
											))
									(scan-error
									 ;; At the innermost thing, time to start going forward
									 (condition-case _
											 (progn
												 ;; Try to go down. If we can, continue
												 ;; processing. If we can't, go up until we
												 ;; can go down.
												 (while (condition-case _
																		(down-list)
																	(error t))
													 (up-list)
													 (setq breadcrumbs (cdr breadcrumbs))))
										 (scan-error
											(error (throw 'done (reverse result)))))))))
						(reverse result)))
			(error nil))))




(ert-deftest my-buttercup-tests-and-positions ()
	(with-temp-buffer
		(insert "(describe \"test\"
	:var ((test))
	(it \"1\")
	(it \"2\")
	(describe \"b\"
		(before-each \"do this\")
		(it \"3\")
		(it \"4\"))
	(describe \"c\"
		(it \"5\")
		(it \"6\")
		(it \"7\")
		(describe \"d\"
			(it \"8\")))
	(describe \"e\"
		(it \"5\")
		(it \"6\")
		(it \"7\")
		(describe \"f\"
			(it \"8\")))
	)")
		(let ((tests (my-buttercup-tests-and-positions)))
			(expect (assoc "test 1" tests))
			(expect (assoc "test 2" tests))
			(expect (assoc "test b 3" tests))
			(expect (assoc "test b 4" tests))
			(expect (assoc "test c 5" tests))
			(expect (assoc "test e f 8" tests)))))
;; Buttercup:1 ends here

;; Undercover
;; :PROPERTIES:
;; :CUSTOM_ID: undercover
;; :END:


;; [[file:Sacha.org::#undercover][Undercover:1]]
(use-package undercover
	:quelpa (undercover :fetcher github :repo "undercover-el/undercover.el")
	)
(use-package coverage)
;; Undercover:1 ends here

;; Eldoc
;; :PROPERTIES:
;; :CUSTOM_ID: eldoc
;; :END:
;; Eldoc provides minibuffer hints when working with Emacs Lisp.

;; [[file:Sacha.org::#eldoc][Eldoc:1]]
(use-package eldoc
  :if my-laptop-p
  :diminish eldoc-mode
  :commands turn-on-eldoc-mode
  :defer t
  :init
  (progn
    (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
    (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
    (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode))
	:config
	(eldoc-add-command-completions "paredit-")
	(eldoc-add-command-completions "lispy-"))
;; Eldoc:1 ends here



;; Related:
;; - [[https://www.masteringemacs.org/article/seamlessly-merge-multiple-documentation-sources-eldoc][Seamlessly Merge Multiple Documentation Sources with Eldoc - Mastering Emacs]]


;; [[file:Sacha.org::#eldoc][Eldoc:2]]
(use-package flycheck
	:if my-laptop-p
	:preface
(defun mp-flycheck-eldoc (callback &rest _ignored)
   "Print flycheck messages at point by calling CALLBACK."
   (when-let ((flycheck-errors (and flycheck-mode (flycheck-overlay-errors-at (point)))))
     (mapc
      (lambda (err)
        (funcall callback
           (format "%s: %s"
                   (let ((level (flycheck-error-level err)))
                     (pcase level
                       ('info (propertize "I" 'face 'flycheck-error-list-info))
                       ('error (propertize "E" 'face 'flycheck-error-list-error))
                       ('warning (propertize "W" 'face 'flycheck-error-list-warning))
                       (_ level)))
                   (flycheck-error-message err))
           :thing (or (flycheck-error-id err)
                      (flycheck-error-group err))
           :face 'font-lock-doc-face))
      flycheck-errors)))
  (defun mp-flycheck-prefer-eldoc ()
    (add-hook 'eldoc-documentation-functions #'mp-flycheck-eldoc nil t)
    (setq eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
    (setq flycheck-display-errors-function nil)
    (setq flycheck-help-echo-function nil))
  :hook ((flycheck-mode . mp-flycheck-prefer-eldoc)))
(use-package eglot
	:if my-laptop-p
  :preface
  (defun mp-eglot-eldoc ()
    (setq eldoc-documentation-strategy
            'eldoc-documentation-compose-eagerly))
  :hook ((eglot-managed-mode . mp-eglot-eldoc)))
;; Eldoc:2 ends here

;; Refactoring                                                      :drill:
;; :PROPERTIES:
;; :ID:       99ac7ddb-08ef-46c4-8fa8-8a45164f9ef4
;; :DRILL_LAST_INTERVAL: 3.86
;; :DRILL_REPEATS_SINCE_FAIL: 2
;; :DRILL_TOTAL_REPEATS: 2
;; :DRILL_FAILURE_COUNT: 1
;; :DRILL_AVERAGE_QUALITY: 2.5
;; :DRILL_EASE: 2.36
;; :DRILL_LAST_QUALITY: 3
;; :DRILL_LAST_REVIEWED: [2013-02-27 Wed 21:18]
;; :CUSTOM_ID: refactoring
;; :END:

;; More things that I need to get used to...


;; [[file:Sacha.org::#refactoring][Refactoring:1]]
;; C-c C-v l : elint current buffer in clean environment.
;; C-c C-v L : elint current buffer by multiple emacs binaries.
;;             See `erefactor-lint-emacsen'
;; C-c C-v r : Rename symbol in current buffer.
;;             Resolve `let' binding as long as i can.
;; C-c C-v R : Rename symbol in requiring modules and current buffer.
;; C-c C-v h : Highlight current symbol in this buffer
;;             and suppress `erefacthr-highlight-mode'.
;; C-c C-v d : Dehighlight all by above command.
;; C-c C-v c : Switch prefix bunch of symbols.
;;             ex: '(hoge-var hoge-func) -> '(foo-var foo-func)
;; C-c C-v ? : Display flymake elint warnings/errors

(use-package erefactor
  :if my-laptop-p
  :defer t
  :bind (:map emacs-lisp-mode-map ("C-c C-v" . erefactor-map)))

(use-package redshank
  :if my-laptop-p
  :disabled t
  :defer t
  :init (add-hook 'emacs-lisp-mode-hook 'redshank-mode))
;; Refactoring:1 ends here

;; Jumping to code
;; :PROPERTIES:
;; :CUSTOM_ID: jumping-to-code
;; :END:


;; [[file:Sacha.org::#jumping-to-code][Jumping to code:1]]
(define-key emacs-lisp-mode-map (kbd "C-c .") 'find-function-at-point)
;; Jumping to code:1 ends here

;; Sorting
;; :PROPERTIES:
;; :CUSTOM_ID: sorting
;; :END:


;; [[file:Sacha.org::#sorting][Sorting:1]]
(defun my-sort-sexps-in-region (beg end)
  "Can be handy for sorting out duplicates.
       Sorts the sexps from BEG to END. Leaves the point at where it
       couldn't figure things out (ex: syntax errors)."
  (interactive "r")
  (let ((input (buffer-substring beg end))
        list last-point form result)
    (save-restriction
      (save-excursion
        (narrow-to-region beg end)
        (goto-char (point-min))
        (setq last-point (point-min))
        (setq form t)
        (while (and form (not (eobp)))
          (setq form (ignore-errors (read (current-buffer))))
          (when form
            (add-to-list
             'list
             (cons
              (prin1-to-string form)
              (buffer-substring last-point (point))))
            (setq last-point (point))))
        (setq list (sort list (lambda (a b) (string< (car a) (car b)))))
        (delete-region (point-min) (point))
        (insert (mapconcat 'cdr list "\n"))))))
;; Sorting:1 ends here

;; Evaluation
;; :PROPERTIES:
;; :CUSTOM_ID: evaluation
;; :END:

;; Borrowed from Steve Purcell's config. This pretty-prints the results.


;; [[file:Sacha.org::#evaluation][Evaluation:1]]
(bind-key "M-:" 'pp-eval-expression)

(defun sanityinc/eval-last-sexp-or-region (prefix)
  "Eval region from BEG to END if active, otherwise the last sexp."
  (interactive "P")
  (if (and (mark) (use-region-p))
      (eval-region (min (point) (mark)) (max (point) (mark)))
    (pp-eval-last-sexp prefix)))

(bind-key "C-x C-e" 'sanityinc/eval-last-sexp-or-region emacs-lisp-mode-map)
;; Evaluation:1 ends here

;; Auto insert
;; :PROPERTIES:
;; :CUSTOM_ID: auto-insert
;; :END:

;; [[file:Sacha.org::#auto-insert][Auto insert:1]]
(with-eval-after-load 'auto-insert
	(add-to-list 'auto-insert-alist
							 '(("\\.el\\'" . "Emacs Lisp header")
								 "Short description: "
								 ";;; " (file-name-nondirectory (buffer-file-name)) " --- " str
     (make-string (max 2 (- 80 (current-column) 27)) ?\s)
     "-*- lexical-binding: t; -*-" '(setq lexical-binding t)
     "

;; Copyright (C) " (format-time-string "%Y") "  "
 (getenv "ORGANIZATION") | (progn user-full-name) "

;; Author: " (user-full-name)
'(if (search-backward "&" (line-beginning-position) t)
     (replace-match (capitalize (user-login-name)) t t))
'(end-of-line 1) " <" (progn user-mail-address) ">
"
;; Keywords and completing-read with a require-match don't give me a way to break out
;; ;; Keywords: "
;;  '(require 'finder)
;;  ;;'(setq v1 (apply 'vector (mapcar 'car finder-known-keywords)))
;;  '(setq v1 (mapcar (lambda (x) (list (symbol-name (car x))))
;; 		   finder-known-keywords)
;; 	v2 (mapconcat (lambda (x) (format "%12s:  %s" (car x) (cdr x)))
;; 	   finder-known-keywords
;; 	   "\n"))
;;  ((let ((minibuffer-help-form v2))
;;     (completing-read "Keyword, C-h: " v1 nil t))
;;     str ", ")
 ;; & -2
 "

\;; This program is free software; you can redistribute it and/or modify
\;; it under the terms of the GNU General Public License as published by
\;; the Free Software Foundation, either version 3 of the License, or
\;; (at your option) any later version.

\;; This program is distributed in the hope that it will be useful,
\;; but WITHOUT ANY WARRANTY; without even the implied warranty of
\;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
\;; GNU General Public License for more details.

\;; You should have received a copy of the GNU General Public License
\;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

\;;; Commentary:

\;; " _ "

\;;; Code:



\(provide '"
       (file-name-base (buffer-file-name))
       ")
\;;; " (file-name-nondirectory (buffer-file-name)) " ends here\n")))
;; Auto insert:1 ends here

;; Stubbing
;; :PROPERTIES:
;; :CUSTOM_ID: stubbing
;; :END:

;; From https://ag91.github.io/blog/2020/12/31/top-down-elisping-a-simple-snippet-to-stub-a-function-while-your-are-designing-your-code/


;; [[file:Sacha.org::#stubbing][Stubbing:1]]
(defun my-stub-elisp-defun ()
  "Stub an elisp function from symbol at point."
  (interactive)
  (let* ((fun (thing-at-point 'list 'no-properties)))
    (when fun
      (let* ((fun-list (car (read-from-string fun)))
             (name (symbol-name (nth 0 fun-list)))
             (args (cdr fun-list)))
        (save-excursion
          (or (search-backward "(defun" nil 't) (goto-char (point-min)))
          (insert
           (s-concat
            "(defun "
            name
            " "
            (format "%s" (--map (s-concat "arg" (number-to-string it)) (number-sequence 1 (length args))))
            "\n  \"SomeDocs\"\n  nil)\n\n")))))))

(bind-key "C-:" #'my-stub-elisp-defun emacs-lisp-mode-map)
;; Stubbing:1 ends here

;; Helpful
;; :PROPERTIES:
;; :CUSTOM_ID: helpful
;; :END:

;; [[file:Sacha.org::#helpful][Helpful:1]]
(use-package helpful
  :bind
  ([remap describe-key] . helpful-key)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-function] . helpful-callable))
;; Helpful:1 ends here

;; elisp-demos
;; :PROPERTIES:
;; :CUSTOM_ID: elisp-demos
;; :END:

;; elisp-demos lets you add text to a symbol's help documentation from
;; entries in an Org file. The Org file at
;; https://github.com/xuchunyang/elisp-demos has many examples. I've
;; modified my version to allow me to have personal note files and a
;; button to add more examples. My diff: https://github.com/xuchunyang/elisp-demos/compare/master...sachac:elisp-demos:user-files


;; [[file:Sacha.org::#elisp-demos][elisp-demos:1]]
(use-package elisp-demos
	:load-path "~/vendor/elisp-demos"
	:commands
	elisp-demos-advice-helpful-update
	elisp-demos-add-demo
	:init
	(with-eval-after-load 'helpful
		(advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))
	:custom
	elisp-demos-user-files '("~/sync/orgzly/elisp-demos.org"))
;; elisp-demos:1 ends here

;; Useful libraries

;; [[file:Sacha.org::*Useful libraries][Useful libraries:1]]
(use-package plz)
(use-package tzc)
;; Useful libraries:1 ends here

;; Snippets
;; :PROPERTIES:
;; :CUSTOM_ID: snippets
;; :END:

;; [[file:Sacha.org::#snippets][Snippets:1]]
(use-package yasnippet
  :diminish yas-minor-mode
  :init (yas-global-mode)
  :config
	(push '(yasnippet backquote-change) warning-suppress-types)
	(yas-global-mode)
  (add-hook 'hippie-expand-try-functions-list 'yas-hippie-try-expand)
  (setq yas-key-syntaxes '("w_" "w_." "^ "))
  (setq yas-installed-snippets-dir "~/elisp/yasnippet-snippets")
  (setq yas-expand-only-for-last-commands nil)
  (yas-global-mode 1)
  (bind-key "\t" 'hippie-expand yas-minor-mode-map)
)
;;        (global-set-key (kbd "C-c y") (lambda () (interactive)
;;                                         (yas/load-directory "~/elisp/snippets")))
;; Snippets:1 ends here



;; From http://emacswiki.org/emacs/Yasnippet

;; [[file:Sacha.org::#snippets][Snippets:2]]
(defun shk-yas/helm-prompt (prompt choices &optional display-fn)
  "Use helm to select a snippet. Put this into `yas/prompt-functions.'"
  (interactive)
  (setq display-fn (or display-fn 'identity))
  (if (require 'helm-config)
      (let (tmpsource cands result rmap)
        (setq cands (mapcar (lambda (x) (funcall display-fn x)) choices))
        (setq rmap (mapcar (lambda (x) (cons (funcall display-fn x) x)) choices))
        (setq tmpsource
              (list
               (cons 'name prompt)
               (cons 'candidates cands)
               '(action . (("Expand" . (lambda (selection) selection))))
               ))
        (setq result (helm-other-buffer '(tmpsource) "*helm-select-yasnippet"))
        (if (null result)
            (signal 'quit "user quit!")
          (cdr (assoc result rmap))))
    nil))
;; Snippets:2 ends here



;; From https://github.com/pcmantz/elisp/blob/master/my-bindings.el

;; [[file:Sacha.org::#snippets][Snippets:3]]
(setq default-cursor-color "gray")
(setq yasnippet-can-fire-cursor-color "purple")

;; It will test whether it can expand, if yes, cursor color -> green.
(defun yasnippet-can-fire-p (&optional field)
  (interactive)
  (setq yas--condition-cache-timestamp (current-time))
  (let (templates-and-pos)
    (unless (and yas-expand-only-for-last-commands
                 (not (member last-command yas-expand-only-for-last-commands)))
      (setq templates-and-pos (if field
                                  (save-restriction
                                    (narrow-to-region (yas--field-start field)
                                                      (yas--field-end field))
                                    (yas--templates-for-key-at-point))
                                (yas--templates-for-key-at-point))))
    (and templates-and-pos (first templates-and-pos))))

(defun my-change-cursor-color-when-can-expand (&optional field)
  (interactive)
  (when (eq last-command 'self-insert-command)
    (set-cursor-color (if (my-can-expand)
                          yasnippet-can-fire-cursor-color
                        default-cursor-color))))

(defun my-can-expand ()
  "Return true if right after an expandable thing."
  (or (abbrev--before-point) (yasnippet-can-fire-p)))

                                        ; As pointed out by Dmitri, this will make sure it will update color when needed.
(remove-hook 'post-command-hook 'my-change-cursor-color-when-can-expand)

(defun my-insert-space-or-expand ()
  "For binding to the SPC SPC keychord."
  (interactive)
  (condition-case nil (or (my-hippie-expand-maybe nil) (insert "  "))))
;; Snippets:3 ends here



;; This requires me to modify the behaviour of hippie-expand so that it doesn't ding so much.

;; [[file:Sacha.org::#snippets][Snippets:4]]
(defun my-hippie-expand-maybe (arg)
  "Try to expand text before point, using multiple methods.
      The expansion functions in `hippie-expand-try-functions-list' are
      tried in order, until a possible expansion is found.  Repeated
      application of `hippie-expand' inserts successively possible
      expansions.
      With a positive numeric argument, jumps directly to the ARG next
      function in this list.  With a negative argument or just \\[universal-argument],
      undoes the expansion."
  (interactive "P")
  (require 'hippie-exp)
  (if (or (not arg)
          (and (integerp arg) (> arg 0)))
      (let ((first (or (= he-num -1)
                       (not (equal this-command last-command)))))
        (if first
            (progn
              (setq he-num -1)
              (setq he-tried-table nil)))
        (if arg
            (if (not first) (he-reset-string))
          (setq arg 0))
        (let ((i (max (+ he-num arg) 0)))
          (while (not (or (>= i (length hippie-expand-try-functions-list))
                          (apply (nth i hippie-expand-try-functions-list)
                                 (list (= he-num i)))))
            (setq i (1+ i)))
          (setq he-num i))
        (if (>= he-num (length hippie-expand-try-functions-list))
            (progn (setq he-num -1) nil)
          (if (and hippie-expand-verbose
                   (not (window-minibuffer-p)))
              (message "Using %s"
                       (nth he-num hippie-expand-try-functions-list)))))
    (if (and (>= he-num 0)
             (eq (marker-buffer he-string-beg) (current-buffer)))
        (progn
          (setq he-num -1)
          (he-reset-string)
          (if (and hippie-expand-verbose
                   (not (window-minibuffer-p)))
              (message "Undoing expansions"))))))
;; Snippets:4 ends here

;; Show column number
;; :PROPERTIES:
;; :CUSTOM_ID: show-column-number
;; :END:

;; I sometimes need to know where I am in a line.

;; [[file:Sacha.org::#show-column-number][Show column number:1]]
(column-number-mode 1)
;; Show column number:1 ends here

;; Don't show whitespace in diff, but show context
;; :PROPERTIES:
;; :CUSTOM_ID: don-t-show-whitespace-in-diff-but-show-context
;; :END:

;; [[file:Sacha.org::#don-t-show-whitespace-in-diff-but-show-context][Don't show whitespace in diff, but show context:1]]
(setq vc-diff-switches '("-b" "-B" "-u"))
(setq vc-git-diff-switches nil)
;; Don't show whitespace in diff, but show context:1 ends here

;; Javascript
;; :PROPERTIES:
;; :CUSTOM_ID: javascript
;; :END:

;; Handy shortcuts:

;; [[file:Sacha.org::#javascript][Javascript:1]]
(use-package js2-mode
	:mode "\\.c?js\\'"
  :if my-laptop-p
  :bind (:map js2-mode-map ("C-c C-c" . projectile-compile-project)))
;; Javascript:1 ends here

;; [[file:Sacha.org::#javascript][Javascript:2]]
(use-package coffee-mode
  :if my-laptop-p
  :mode "\\.coffee\\'"
  :bind (:map coffee-mode-map ("C-c C-c" . compile)))
;; Javascript:2 ends here

;; [[file:Sacha.org::#javascript][Javascript:3]]
(use-package jasminejs-mode
  :if my-laptop-p
  :after js2-mode
  :hook ((js2-mode . jasminejs-mode)
         (jasminejs-mode-hook . jasminejs-add-snippets-to-yas-snippet-dirs)))
;; Javascript:3 ends here



;; This makes script blocks easier to copy:


;; [[file:Sacha.org::#javascript][Javascript:4]]
(defvar my-javascript-test-regexp (concat (regexp-quote "/** Testing **/") "\\(.*\n\\)*")
  "Regular expression matching testing-related code to remove.
      See `my-copy-javascript-region-or-buffer'.")

(defun my-copy-javascript-region-or-buffer (beg end)
  "Copy the active region or the buffer, wrapping it in script tags.
      Add a comment with the current filename and skip test-related
      code. See `my-javascript-test-regexp' to change the way
      test-related code is detected."
  (interactive "r")
  (unless (region-active-p)
    (setq beg (point-min) end (point-max)))
  (kill-new
   (concat
    "<script type=\"text/javascript\">\n"
    (if (buffer-file-name) (concat "// " (file-name-nondirectory (buffer-file-name)) "\n") "")
    (replace-regexp-in-string
     my-javascript-test-regexp
     ""
     (buffer-substring (point-min) (point-max))
     nil)
    "\n</script>")))
;; Javascript:4 ends here



;; This makes it easier to debug:


;; [[file:Sacha.org::#javascript][Javascript:5]]
(defvar my-debug-counter 1)
(defun my-insert-or-flush-debug (&optional reset beg end)
  (interactive "pr")
  (cond
   ((= reset 4)
    (save-excursion
      (flush-lines "console.log('DEBUG: [0-9]+" (point-min) (point-max))
      (setq my-debug-counter 1)))
   ((region-active-p)
    (save-excursion
      (goto-char end)
      (insert ");\n")
      (goto-char beg)
      (insert (format "console.log('DEBUG: %d', " my-debug-counter))
      (setq my-debug-counter (1+ my-debug-counter))
      (js2-indent-line)))
   (t
    ;; Wrap the region in the debug
    (insert (format "console.log('DEBUG: %d');\n" my-debug-counter))
    (setq my-debug-counter (1+ my-debug-counter))
    (backward-char 3)
    (js2-indent-line))))
;; Javascript:5 ends here



;; And the rest of the js2 config:


;; [[file:Sacha.org::#javascript][Javascript:6]]
(use-package js2-mode
  :if my-laptop-p
  :commands js2-mode
  :defer t
  :interpreter "node"
  :init (setq js-indent-level 2)
  :bind (:map js2-mode-map
              ("C-x C-e" . js-send-last-sexp)
              ("C-M-x" . js-send-last-sexp-and-go)
              ("C-c d" . my-insert-or-flush-debug)
              ("C-c C-b" . js-send-buffer-and-go)
              ("C-c w" . my-copy-javascript-region-or-buffer))
  :config (js2-imenu-extras-setup))
;; Javascript:6 ends here

;; [[file:Sacha.org::#javascript][Javascript:7]]
(use-package coffee-mode
  :if my-laptop-p
  :defer t
  :config (setq-default coffee-js-mode 'js2-mode coffee-tab-width 2))
;; Javascript:7 ends here

;; Indium
;; :PROPERTIES:
;; :CUSTOM_ID: indium
;; :END:


;; [[file:Sacha.org::#indium][Indium:1]]
(use-package indium
:hook ((js2-mode . indium-interaction-mode)))
;; Indium:1 ends here

;; React
;; :PROPERTIES:
;; :CUSTOM_ID: react
;; :END:


;; [[file:Sacha.org::#react][React:1]]
(use-package rjsx-mode
  :if my-laptop-p)
;; React:1 ends here

;; Typescript


;; [[file:Sacha.org::*Typescript][Typescript:1]]
(use-package typescript-mode
	:mode "\\.ts\\'")
;; Typescript:1 ends here

;; HTML
;; :PROPERTIES:
;; :CUSTOM_ID: html
;; :END:

;; Convenience function for getting rid of annoying spans
;; offby1 says there's (setq nxml-sexp-element-flag t)

;; <span><span>Hello world</span></span>


;; [[file:Sacha.org::#html][HTML:1]]
(defun my-clean-up-spans-in-region (beg end)
  (interactive "r")
  (save-excursion
    (let ((changed t))
      (while changed
        (setq changed nil)
        (goto-char beg)
        (while (re-search-forward "<span>\\([^<]*\\)</span>" end t)
          (replace-match "\\1")
          (setq changed t)))
      (setq changed t)
      (while changed
        (setq changed nil)
        (goto-char beg)
        (while (re-search-forward "<span>*\\(<a[^<]+>[^<]*</a>\\)</span>" end t)
          (replace-match "\\1")
          (setq changed t))))))

(defun my-clean-up-spans-in-string (string)
  (with-temp-buffer
    (insert string)
    (my-clean-up-spans-in-region (point-min) (point-max))
    (buffer-string)))

(ert-deftest my-clean-up-spans-in-string ()
  (should (string= (my-clean-up-spans-in-string "<span><span>Hello world</span></span>")
                   "Hello world"))
  (should (string= (my-clean-up-spans-in-string "<span><span><a href=\"http://example.com\">Hello another world</a></span></span>")
                   "<a href=\"http://example.com\">Hello another world</a>"))
  (should (string= (my-clean-up-spans-in-string "<span><h1>Leave alone</h1></span>") "<span><h1>Leave alone</h1></span>"))
  (should (string= (my-clean-up-spans-in-string "<span><a href=\"http://example.com\">Leave</a> alone</span>")
                   "<span><a href=\"http://example.com\">Leave</a> alone</span>")))

;; (ert "my-clean-up-spans-in-string")
;; HTML:1 ends here

;; Shell
;; :PROPERTIES:
;; :CUSTOM_ID: shell
;; :END:

;; Make files executable if the first file has a shebang (ex: =#!/bin/bash#=)

;; [[file:Sacha.org::#shell][Shell:1]]
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)
;; Shell:1 ends here

;; Shellcheck
;; :PROPERTIES:
;; :CUSTOM_ID: shellcheck
;; :END:
;; https://amitp.blogspot.com/2023/10/emacs-and-shellcheck.html


;; [[file:Sacha.org::#shellcheck][Shellcheck:1]]
(use-package flymake
  :bind (("S-e" . flymake-show-project-diagnostics)))

(use-package sh-script
  :hook (sh-mode . flymake-mode))

(use-package flymake-shellcheck)
(use-package flymake
  :bind (("S-e" . my-consult-flymake-project))
  :preface
  (defun my/consult-flymake-project ()
    (interactive)
    (consult-flymake t))
  :custom
  (flymake-suppress-zero-counters t)
  :config
  (defface my-flymake-modeline-error-echo
    '((t :inherit 'flymake-error-echo :background "red"))
    "Mode line flymake errors")
  (put 'flymake-error 'mode-line-face 'my-flymake-modeline-error-echo)
  (defface my-flymake-modeline-warning-echo
    '((t :inherit 'flymake-warning-echo :background "orange"))
    "Mode line flymake warnings")
  (put 'flymake-warning 'mode-line-face 'my-flymake-modeline-warning-echo))
;; Shellcheck:1 ends here

;; dwim-shell-command
;; :PROPERTIES:
;; :CUSTOM_ID: dwim-shell-command
;; :END:

;; [[file:Sacha.org::#dwim-shell-command][dwim-shell-command:1]]
(defun my-dwim-shell-command (prefix)
  "Execute DWIM shell command asynchronously using noweb templates.

Which files

  `dwim-shell-command' attempts to guess which file(s) you may want
  the command to operate on.

  1. If visiting a `dired' buffer, draw the marked file(s).
  2. If visiting any other buffer with an associated file, use that.

Templates

  Operate on drawn files using either the following:

    <<f>> (file path,used by default)
    <<fne>> (file path without extension)
    <<e>> (extension)
    <<td>> (generate a temporary directory)
    <<*>> (all files joined)
    <<cb>> (clipboard)
    <<n>>, <<1n>>, or <<An>> (for current iteration)

  For example:

    With drawn files '(\"path/to/image1.png\" \"path/to/image2.png\")

   \"convert '<<f>>' '<<fne>>.jpg'\" expands to

     \"convert 'path/to/image1.png' 'path/to/image1.jpg'\"
     \"convert 'path/to/image2.png' 'path/to/image2.jpg'\"

   while \"ls -lh <<*>>\" expands to

     \"ls -lh path/to/image1.png path/to/image2.png\"

Focus

  `dwim-shell-command' creates a process buffer to capture command
  output, but doesn't display or focus on it by default.  Instead,
  it tries to guess what's more convenient to focus on.

  While the process is busy, show a spinner in the minibuffer.  No
  focus changes.

  After process is finished:

  1. If there were any files created in the `default-directory',
  jump to a `dired' buffer and move point to the new file (via
  `dired-jump').

  2. If no new files were created, automatically switch focus to the
  process buffer and display its output.

    Note: You can prevent this automatic focus by prepending your
    command with whitespace.

      |
      V
    \" convert '<<f>>' '<<fne>>.jpg'\"

  3. If the shell command caused any errors, offer to focus the
  process buffer and display its output.

Quick exit

  Process buffers are read-only and can be quickly closed by
  pressing `q'.

Prefix

  With PREFIX, execute command that number of times."
  (interactive "p")
  (let ((script (read-shell-command dwim-shell-command-prompt)))
    (unless (string-match "<<" script) (setq script (concat script " <<f>>")))
    (dwim-shell-command-on-marked-files
     dwim-shell-command-buffer-name script
     :repeat prefix
     :shell-util dwim-shell-command-shell-util
     :shell-args dwim-shell-command-shell-args
     :silent-success (string-prefix-p " " script)
     :error-autofocus (not dwim-shell-command-prompt-on-error))))

(use-package dwim-shell-command
  :if my-laptop-p
  :bind (([remap shell-command] . my-dwim-shell-command)
         :map dired-mode-map
         ([remap dired-do-async-shell-command] . my-dwim-shell-command)
         ([remap dired-do-shell-command] . my-dwim-shell-command)
         ([remap dired-smart-shell-command] . my-dwim-shell-command))
  )
;; dwim-shell-command:1 ends here

;; Magit - nice git interface
;; :PROPERTIES:
;; :ID:       o2b:9a42a292-7b75-4c7f-8da2-7a0d8c22d0c6
;; :POST_DATE: [2014-10-31 Fri 23:26]
;; :POSTID:   27579
;; :BLOG:     sacha
;; :CUSTOM_ID: magit
;; :END:

;; <<magit>>

;; Thanks to sheijk for hints on tweaking magit to limit it to the current directory!


;; [[file:Sacha.org::#magit][Magit - nice git interface:1]]
(defun my-magit-stage-all-and-commit (message)
  (interactive (list (progn (magit-diff-unstaged) (read-string "Commit Message: "))))
  (magit-stage-modified)
  (magit-commit-create (list "-m" message))
  (call-interactively #'magit-push-current-to-pushremote))
(defvar my-magit-limit-to-directory nil "Limit magit status to a specific directory.")
(defun my-magit-status-in-directory (directory)
  "Displays magit status limited to DIRECTORY.
Uses the current `default-directory', or prompts for a directory
if called with a prefix argument. Sets `my-magit-limit-to-directory'
so that it's still active even after you stage a change. Very experimental."
  (interactive (list (expand-file-name
                        (if current-prefix-arg
                            (read-directory-name "Directory: ")
                          default-directory))))
    (setq my-magit-limit-to-directory directory)
    (magit-status directory))
(use-package magit
  :config
  (setq magit-diff-options '("-b")) ; ignore whitespace
  (defadvice magit-insert-untracked-files (around sacha activate)
    (if my-magit-limit-to-directory
        (magit-with-section (section untracked 'untracked "Untracked files:" t)
                            (let ((files (cl-mapcan
                                          (lambda (f)
                                            (when (eq (aref f 0) ??) (list f)))
                                          (magit-git-lines
                                           "status" "--porcelain" "--" my-magit-limit-to-directory))))
                              (if (not files)
                                  (setq section nil)
                                (dolist (file files)
                                  (setq file (magit-decode-git-path (substring file 3)))
                                  (magit-with-section (section file file)
                                                      (insert "\t" file "\n")))
                                (insert "\n"))))
      ad-do-it))

  (defadvice magit-insert-unstaged-changes (around sacha activate)
    (if my-magit-limit-to-directory
        (let ((magit-current-diff-range (cons 'index 'working))
              (magit-diff-options (copy-sequence magit-diff-options)))
          (magit-git-insert-section (unstaged "Unstaged changes:")
                                    #'magit-wash-raw-diffs
                                    "diff-files"
                                    "--" my-magit-limit-to-directory
                                    ))
      ad-do-it))

  (defadvice magit-insert-staged-changes (around sacha activate)
    "Limit to `my-magit-limit-to-directory' if specified."
    (if my-magit-limit-to-directory
        (let ((no-commit (not (magit-git-success "log" "-1" "HEAD"))))
          (when (or no-commit (magit-anything-staged-p))
            (let ((magit-current-diff-range (cons "HEAD" 'index))
                  (base (if no-commit
                            (magit-git-string "mktree")
                          "HEAD"))
                  (magit-diff-options (append '("--cached") magit-diff-options)))
              (magit-git-insert-section (staged "Staged changes:")
                                        (apply-partially #'magit-wash-raw-diffs t)
                                        "diff-index" "--cached" base "--" my-magit-limit-to-directory))))
      ad-do-it))
  :bind (("C-x v C-d" . my-magit-status-in-directory)
         ("C-c g" . magit-file-dispatch)
         ("C-x g" . magit-status)
         ("C-x v p" . magit-push)
         ("C-x v c" . my-magit-stage-all-and-commit)))

;; ;; From http://endlessparentheses.com/merging-github-pull-requests-from-emacs.html
;; (defun endless/load-gh-pulls-mode ()
;;   "Start `magit-gh-pulls-mode' only after a manual request."
;;   (interactive)
;;   (require 'magit-gh-pulls)
;;   (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)
;;   (magit-gh-pulls-mode 1)
;;   (magit-gh-pulls-reload))

;; (use-package magit-gh-pulls)
;; Magit - nice git interface:1 ends here

;; Finding repos with uncommitted changes

;; Based on http://yitang.uk/2024/01/14/atomic-habit-in-emacs-keep-git-repos-clean/


;; [[file:Sacha.org::*Finding repos with uncommitted changes][Finding repos with uncommitted changes:1]]
(defun my-git-find-unclean-repo (root-dir)
  "Find repo with modified files."
  ;; (interactive)
  (setq out nil)
  (dolist (dir (directory-files-recursively root-dir "\\.git$" t))
    (message "checking repo %s" dir)
    (let* ((git-dir (file-name-parent-directory dir))
           (default-directory git-dir))
      (unless (string= "" (shell-command-to-string "git status --untracked=no --porcelain"))
        (push git-dir out))))
  out)

(defun my-list-uncommitted-projects ()
	(interactive)
	(let ((s (string-join
						(seq-keep
						 (lambda (root)
							 (when-let ((repo (my-git-find-unclean-repo root)))
								 (concat "- "
												 (org-link-make-string
													(format "elisp:(magit-status \"%s\")"
																	(car repo))
													(file-name-nondirectory (replace-regexp-in-string "/$" "" root))))))
						 (seq-uniq
							(mapcar (lambda (row)
												(or (projectile-project-root
														 (car row))
														(car row)))
											(cons '("~/sync/emacs") my-project-web-base-list))))
						"\n")))
		(when (called-interactively-p 'any)
			(switch-to-buffer (get-buffer-create "*Uncommitted*"))
			(erase-buffer)
			(insert s)
			(org-mode))
		s))
;; Finding repos with uncommitted changes:1 ends here

;; Checking things out
;; :PROPERTIES:
;; :CUSTOM_ID: checking-things-out
;; :END:

;; Based on http://xenodium.com/emacs-clone-git-repo-from-clipboard/ :


;; [[file:Sacha.org::#checking-things-out][Checking things out:1]]
(defvar my-git-clone-destination "~/vendor")
(defun my-git-clone-clipboard-url ()
  "Clone git URL in clipboard asynchronously and open in dired when finished."
  (interactive)
  (cl-assert (string-match-p "^\\(http\\|https\\|ssh\\)://" (current-kill 0)) nil "No URL in clipboard")
  (let* ((url (current-kill 0))
         (download-dir (expand-file-name my-git-clone-destination))
         (project-dir (concat (file-name-as-directory download-dir)
                              (file-name-base url)))
         (default-directory download-dir)
         (command (format "git clone %s" url))
         (buffer (generate-new-buffer (format "*%s*" command)))
         (proc))
    (when (file-exists-p project-dir)
      (if (y-or-n-p (format "%s exists. delete?" (file-name-base url)))
          (delete-directory project-dir t)
        (user-error "Bailed")))
    (switch-to-buffer buffer)
    (setq proc (start-process-shell-command (nth 0 (split-string command)) buffer command))
    (with-current-buffer buffer
      (setq default-directory download-dir)
      (shell-command-save-pos-or-erase)
      (require 'shell)
      (shell-mode)
      (view-mode +1))
    (set-process-sentinel proc (lambda (process state)
                                 (let ((output (with-current-buffer (process-buffer process)
                                                 (buffer-string))))
                                   (kill-buffer (process-buffer process))
                                   (if (= (process-exit-status process) 0)
                                       (progn
                                         (message "finished: %s" command)
                                         (dired project-dir))
                                     (user-error (format "%s\n%s" command output))))))
    (set-process-filter proc #'comint-output-filter)))
;; Checking things out:1 ends here

;; git-messenger - shows commit message
;; :PROPERTIES:
;; :CUSTOM_ID: git-messenger-shows-commit-message
;; :END:


;; [[file:Sacha.org::#git-messenger-shows-commit-message][git-messenger - shows commit message:1]]
(use-package git-messenger
  :bind (("C-x v m" . git-messenger:popup-message)))
;; git-messenger - shows commit message:1 ends here

;; Tag files
;; :PROPERTIES:
;; :CUSTOM_ID: tag-files
;; :END:

;; I don't often use a TAGS file, but when I do, I don't want to have
;; to set my tags file per project. I search for it in the directory
;; tree instead.


;; [[file:Sacha.org::#tag-files][Tag files:1]]
(defun my-recursive-find-file (file &optional directory)
  "Find the first FILE in DIRECTORY or its parents."
  (setq directory (or directory (file-name-directory (buffer-file-name)) (pwd)))
  (if (file-exists-p (expand-file-name file directory))
      (expand-file-name file directory)
    (unless (string= directory "/")
      (my-recursive-find-file file (expand-file-name ".." directory)))))

(defun my-find-tags ()
  "Set the TAGS file."
  (set (make-variable-buffer-local 'tags-table-list) nil)
  (set (make-variable-buffer-local 'tags-file-name)
       (my-recursive-find-file "TAGS")))

(eval-after-load 'drupal-mode
  '(progn
     (add-hook 'drupal-mode-hook 'my-find-tags)))
;; Tag files:1 ends here

;; Projects and projectile
;; :PROPERTIES:
;; :CUSTOM_ID: projects-and-projectile
;; :END:


;; [[file:Sacha.org::#projects-and-projectile][Projects and projectile:1]]
(use-package projectile
  :diminish projectile-mode
  :config
	(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1)
  (setq projectile-completion-system 'default)
  (setq projectile-enable-caching t)
  (setq projectile-indexing-method 'alien)
  (add-to-list 'projectile-globally-ignored-files "node_modules")
  (add-to-list 'projectile-globally-ignored-files ".cache")
  (add-to-list 'projectile-globally-ignored-files "_cache")
	(add-to-list 'projectile-globally-ignored-files "~")
	(add-to-list 'projectile-globally-ignored-files "#"))
;; Call with C-c p m m
(use-package makefile-executor
  :if my-laptop-p
  :config
  (add-hook 'makefile-mode-hook 'makefile-executor-mode))
(defun my-projectile-open-notes ()
	(interactive)
	(find-file-other-window (expand-file-name "notes.org" (projectile-project-root))))
;; Projects and projectile:1 ends here

;; Capturing notes to per-project files

;; Based on [[https://shreyas.ragavan.co/post/8f702ce2-8bb7-40a3-b44b-a47222c02909/][Juggling multiple projects and leveraging org-projectile | Shreyas Ragavan]]


;; [[file:Sacha.org::*Capturing notes to per-project files][Capturing notes to per-project files:1]]
(use-package org-project-capture)
(use-package org-projectile
	:after org-project-capture
	:config
	(setq org-projectile-per-project-filepath "notes.org")
	(org-projectile-per-project)
	(org-project-capture-per-project)
	(push (org-projectile-project-todo-entry) org-capture-templates)
	;; I have some remote files I don't want included.
	;; (setq org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
	:bind
	(("C-c p n" . org-projectile-capture-for-current-project)))
;; Capturing notes to per-project files:1 ends here

;; Exploring MELPA recipes
;; :PROPERTIES:
;; :CUSTOM_ID: exploring-melpa-recipes
;; :END:


;; [[file:Sacha.org::#exploring-melpa-recipes][Exploring MELPA recipes:1]]

;; Exploring MELPA recipes:1 ends here

;; [[file:Sacha.org::#ruby][Ruby:4]]
(use-package inf-ruby
	:config
	(setq inf-ruby-prompt-format
			(concat
			 (mapconcat
				#'identity
				'("\\(^%s> *\\)"					; Simple
					"\\(^(rdb:1) *\\)"			; Debugger
					"\\(^(rdbg[^)]*) *\\)"	; Ruby Debug Gem
					"\\(^(byebug) *\\)"			; byebug
					"\\(^\\(irb([^)]+)"			; IRB default
					"\\([[0-9]+] \\)?[Pp]ry ?([^)]+)"	; Pry
					"\\(^[^%s]+\\)"			 ; new rails console with project name and environment
					"\\(jruby-\\|JRUBY-\\)?[1-9]\\.[0-9]\\(\\.[0-9]+\\)*\\(-?p?[0-9]+\\)?" ; RVM
					"^rbx-head\\)")					 ; RVM continued
				"\\|")
			 ;; Statement and nesting counters, common to the last four.
			 " ?[0-9:]* ?%s *\\)")
			inf-ruby-first-prompt-pattern
			(format inf-ruby-prompt-format ">" ">" ">")
			inf-ruby-prompt-pattern
			(format inf-ruby-prompt-format "[?>]" "*>" "[\]>*\"'/`]")))
;; Ruby:4 ends here

;; Skewer
;; :PROPERTIES:
;; :CUSTOM_ID: skewer
;; :END:

;; This lets you send HTML, CSS, and Javascript fragments to Google
;; Chrome. You may need to start Chrome with =chrome
;; --allow-running-insecure-content=, if you're using the user script
;; with HTTPS sites.


;; [[file:Sacha.org::#skewer][Skewer:1]]
(use-package skewer-mode
  :if my-laptop-p
  :hook
  ((js2-mode-hook . skewer-mode)
   (css-mode-hook . skewer-css-mode)
   (html-mode-hook . skewer-html-mode)))
;; Skewer:1 ends here

;; Autocomplete
;; :PROPERTIES:
;; :CUSTOM_ID: autocomplete
;; :END:


;; [[file:Sacha.org::#autocomplete][Autocomplete:1]]
(with-eval-after-load 'company
	(define-key company-mode-map (kbd "<tab>") 'company-indent-or-complete-common))
(use-package company
  :if my-laptop-p
  :init (add-hook 'prog-mode-hook 'company-mode))
(use-package company-posframe :if my-laptop-p :init (company-posframe-mode 1) :diminish)
;; Autocomplete:1 ends here

;; Tern - for Javascript
;; :PROPERTIES:
;; :CUSTOM_ID: tern-for-javascript
;; :END:


;; [[file:Sacha.org::#tern-for-javascript][Tern - for Javascript:1]]
(use-package tern
  :if my-laptop-p
  :bind (:map tern-mode-keymap ("C-c C-c" . compile))
  :hook (js2-mode-hook . tern-mode)
  :config
  (when (eq system-type 'windows-nt) (setq tern-command '("cmd" "/c" "tern"))))
;; Tern - for Javascript:1 ends here

;; Docker
;; :PROPERTIES:
;; :CUSTOM_ID: docker
;; :END:


;; [[file:Sacha.org::#docker][Docker:1]]
(use-package dockerfile-mode
  :mode ("Dockerfile\\'" . dockerfile-mode))
;; Docker:1 ends here

;; Multiple cursors mode                                            :drill:
;; :PROPERTIES:
;; :ID:       o2b:61b0ffae-669b-4360-98fd-a6f0ea6f018e
;; :DRILL_LAST_INTERVAL: 3.86
;; :DRILL_REPEATS_SINCE_FAIL: 2
;; :DRILL_TOTAL_REPEATS: 2
;; :DRILL_FAILURE_COUNT: 1
;; :DRILL_AVERAGE_QUALITY: 2.5
;; :DRILL_EASE: 2.36
;; :DRILL_LAST_QUALITY: 3
;; :DRILL_LAST_REVIEWED: [2013-02-27 Wed 21:18]
;; :CUSTOM_ID: multiple-cursors-mode
;; :END:

;; I often define keyboard macros to process multiple lines in a region.
;; Maybe =multiple-cursors= will be an even better way. Looks promising!
;; [[http://emacsrocks.com/e13.html][See Emacs Rocks episode 13 (multiple-cursors) for a great demo]].


;; [[file:Sacha.org::#multiple-cursors-mode][Multiple cursors mode:1]]
(use-package multiple-cursors
  :bind
  (("C-c m t" . mc/mark-all-like-this)
   ("C-c m m" . mc/mark-all-like-this-dwim)
   ("C-c m l" . mc/edit-lines)
   ("C-c m e" . mc/edit-ends-of-lines)
   ("C-c m a" . mc/edit-beginnings-of-lines)
   ("C-c m n" . mc/mark-next-like-this)
   ("C-c m p" . mc/mark-previous-like-this)
   ("C-c m s" . mc/mark-sgml-tag-pair)
   ("C-c m d" . mc/mark-all-like-this-in-defun)))
(use-package phi-search)
(use-package phi-search-mc :config (phi-search-mc/setup-keys))
(use-package mc-extras :config (define-key mc/keymap (kbd "C-. =") 'mc/compare-chars))
;; Multiple cursors mode:1 ends here

;; iedit
;; https://www.reddit.com/r/emacs/comments/19ec8v5/comment/kjhuyrq/?utm_source=reddit&utm_medium=web2x&context=3


;; [[file:Sacha.org::*iedit][iedit:1]]
(use-package iedit
  :bind
  (("C-;"  . iedit-mode) ; also note: C-' toggles focus of matches
   :map iedit-mode-keymap
   ("C-g" . iedit-mode)) ; so I can exit iedit with C-g
  :config
  (advice-add #'iedit--get-scope    ; switch default to function scope
							:filter-args
							(defun my-iedit-defun-by-default (arg)
								(cond ((eq (car arg) nil) '(0))
											((eq (car arg) 0) '(nil))
											(t arg)))))
;; iedit:1 ends here

;; Eshell
;; :PROPERTIES:
;; :CUSTOM_ID: eshell
;; :END:
;; https://www.reddit.com/r/emacs/comments/b6n3t8/what_would_it_take_to_get_terminal_colors_in/

;; [[file:Sacha.org::#eshell][Eshell:1]]
(use-package xterm-color
  :commands (xterm-color-filter))
(use-package eshell
  :after xterm-color
  :config
	(setq eshell-scroll-to-bottom-on-input t)
	(define-key eshell-mode-map (kbd "<tab>") #'company-complete)
  (define-key eshell-hist-mode-map (kbd "M-r") #'consult-history)
  (add-hook 'eshell-mode-hook
            (lambda ()
              (setenv "TERM" "xterm-256color")))
  (add-hook 'eshell-before-prompt-hook (setq xterm-color-preserve-properties t))
  (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
  (setq eshell-output-filter-functions
        (remove 'eshell-handle-ansi-color eshell-output-filter-functions)))
;; Eshell:1 ends here

;; Eshell completion

;; [[https://www.emacs.dyerdwelling.family/emacs/20240827210257-emacs--enhancing-eshell-to-be-more-fishy/][Source]]


;; [[file:Sacha.org::*Eshell completion][Eshell completion:1]]
(use-package capf-autosuggest
   :hook
   (eshell-mode . capf-autosuggest-mode))
;; Eshell completion:1 ends here

;; Correctly complete commands in subdirectories
;; :PROPERTIES:
;; :CUSTOM_ID: correctly-complete-commands-in-subdirectories
;; :END:

;; From https://www.n16f.net/blog/eshell-key-bindings-and-completion/

;; [[file:Sacha.org::#correctly-complete-commands-in-subdirectories][Correctly complete commands in subdirectories:1]]
(with-eval-after-load 'eshell
	(defun eshell--complete-commands-list ()
		"Generate list of applicable, visible commands."
		(let ((filename (pcomplete-arg)) glob-name)
			(if (file-name-directory filename)
					(if eshell-force-execution
							(pcomplete-dirs-or-entries nil #'file-readable-p)
						(pcomplete-executables))
				(if (and (> (length filename) 0)
								 (eq (aref filename 0) eshell-explicit-command-char))
						(setq filename (substring filename 1)
									pcomplete-stub filename
									glob-name t))
				(let* ((paths (eshell-get-path))
							 (cwd (file-name-as-directory
										 (expand-file-name default-directory)))
							 (path "") (comps-in-path ())
							 (file "") (filepath "") (completions ()))
					;; Go thru each path in the search path, finding completions.
					(while paths
						(setq path (file-name-as-directory
												(expand-file-name (or (car paths) ".")))
									comps-in-path
									(and (file-accessible-directory-p path)
											 (file-name-all-completions filename path)))
						;; Go thru each completion found, to see whether it should
						;; be used.
						(while comps-in-path
							(setq file (car comps-in-path)
										filepath (concat path file))
							(if (and (not (member file completions)) ;
											 (or (string-equal path cwd)
													 (not (file-directory-p filepath)))
											 (if eshell-force-execution
													 (file-readable-p filepath)
												 (file-executable-p filepath)))
									(setq completions (cons file completions)))
							(setq comps-in-path (cdr comps-in-path)))
						(setq paths (cdr paths)))
					;; Add aliases which are currently visible, and Lisp functions.
					(pcomplete-uniquify-list
					 (if glob-name
							 completions
						 (setq completions
									 (append (if (fboundp 'eshell-alias-completions)
															 (eshell-alias-completions filename))
													 (eshell-winnow-list
														(mapcar
														 (lambda (name)
															 (substring name 7))
														 (all-completions (concat "eshell/" filename)
																							obarray #'functionp))
														nil '(eshell-find-alias-function))
													 completions))
						 (append (and (or eshell-show-lisp-completions
															(and eshell-show-lisp-alternatives
																	 (null completions)))
													(all-completions filename obarray #'functionp))
										 completions))))))))
;; Correctly complete commands in subdirectories:1 ends here

;; SQLite

;; From https://christiantietze.de/posts/2024/01/emacs-sqlite-mode-open-sqlite-files-automatically/

;; [[file:Sacha.org::*SQLite][SQLite:1]]
(use-package sqlite-mode
	:commands sqlite-mode-open-file
  :config
  (defun ct/sqlite-view-file-magically ()
    "Runs `sqlite-mode-open-file' on the file name visited by the
current buffer, killing it."
    (require 'sqlite-mode)
    (let ((file-name buffer-file-name))
      (kill-current-buffer)
      (sqlite-mode-open-file file-name)))
  (add-to-list 'magic-mode-alist '("SQLite format 3\x00" . ct/sqlite-view-file-magically)))
;; SQLite:1 ends here

;; Internet Relay Chat
;; :PROPERTIES:
;; :CUSTOM_ID: internet-relay-chat
;; :END:

;; IRC is a great way to hang out with other Emacs geeks.

;; [[file:Sacha.org::#internet-relay-chat][Internet Relay Chat:1]]
(use-package erc
  :if my-laptop-p
  :config
  (setq erc-track-remove-disconnected-buffers t)
  (setq erc-hide-list '("PART" "QUIT" "JOIN"))
  (setq erc-autojoin-channels-alist '(("freenode.net"
                                       "#org-mode"
                                       "#emacs"
                                       "#emacs-beginners"
                                       "#emacs-ops")
                                      ("irc.chat.twitch.tv"
                                       "#sachachua")
																			("irc.tilde.chat"
																			 "#emacs.ch"))
        erc-server "irc.freenode.net"
        erc-nick "sachac"
        erc-track '("NICK" "333" "353" "JOIN" "PART" "AWAY"))
  (defun erc-cmd-OPME ()
    "Request chanserv to op me."
    (erc-message "PRIVMSG"
                 (format "chanserv op %s %s"
                         (erc-default-target)
                         (erc-current-nick)) nil))

  (defun erc-cmd-DEOPME ()
    "Deop myself from current channel."
    (erc-cmd-DEOP (format "%s" (erc-current-nick))))
  (defun erc-cmd-BAN (nick)
    (let* ((chan (erc-default-target))
           (who (erc-get-server-user nick))
           (host (erc-server-user-host who))
           (user (erc-server-user-login who)))
      (erc-server-send (format "MODE %s +b *!%s@%s" chan user host))))

  (defun erc-cmd-KICKBAN (nick &rest reason)
    (setq reason (mapconcat #'identity reason " "))
    (and (string= reason "")
         (setq reason nil))
    (erc-cmd-BAN nick)
    (erc-server-send (format "KICK %s %s %s"
                             (erc-default-target)
                             nick
                             (or reason
                                 "Kicked (kickban)"))))
  (defun my-erc-clean-up ()
    "Clean up dead ERC buffers."
    (interactive)
    (mapc #'kill-buffer (erc-buffer-list (lambda () (null (erc-server-process-alive)))))
    (erc-update-mode-line))
  )
;; Internet Relay Chat:1 ends here

;; Search logs
;; :PROPERTIES:
;; :CUSTOM_ID: search-logs
;; :END:


;; [[file:Sacha.org::#search-logs][Search logs:1]]
(defun my-search-irc-logs ()
  (interactive)
  (let ((default-directory "~/backups/server/home/.znc/users/sachac/moddata/log"))
    (call-interactively 'consult-ripgrep)))
;; Search logs:1 ends here

;; Mastodon  :mastodon:
;; :PROPERTIES:
;; :CUSTOM_ID: mastodon
;; :END:


;; [[file:Sacha.org::#mastodon][Mastodon:1]]
(use-package mastodon
  :if my-laptop-p
	:quelpa
	(mastodon :fetcher git :url "https://codeberg.org/martianh/mastodon.el.git" :branch "develop")
	;:load-path "~/vendor/mastodon.el/lisp"
  :bind
  (:map mastodon-mode-map
        ("g" . mastodon-tl--update)
        ;; see org-capture-templates addition
        ("o" . (lambda () (interactive) (org-capture nil "m"))))
  :commands (mastodon-http--api mastodon-http--post mastodon-mode mastodon-http--get-search-json
																mastodon-tl--get-local-timeline)
  :config
  (setq mastodon-instance-url "https://social.sachachua.com"
        mastodon-active-user "sacha"))

(defun my-mastodon-clear-auth ()
	"Fix alist-get: Wrong type argument: listp, (error . \"The access token is invalid\") error. Then you can use `mastodon-auth--access-token'."
	(interactive)
	(setq mastodon-client--active-user-details-plist nil)
	(delete-file (concat user-emacs-directory "mastodon.plstore"))
	(setq mastodon-auth--token-alist nil))

(defun my-mastodon-toot-public-string (message)
  (interactive "sMessage: ")
  (let* ((endpoint (mastodon-http--api "statuses"))
         (args `(("status" . ,message)
                 ("visibility" . "public"))))
    (mastodon-http--post endpoint args nil)))

(defun my-mastodon-show-my-followers ()
  (interactive)
  (mastodon-profile--make-profile-buffer-for
   (mastodon-profile--lookup-account-in-status (mastodon-auth--get-account-name) nil)
   "followers"
   #'mastodon-profile--add-author-bylines))

(defun my-yank-mastodon-link ()
  (interactive)
  (let* ((url (current-kill 0))
         (url-parsed (url-generic-parse-url url))
         (user (file-name-base (url-filename url-parsed))))
    (cond
     ((derived-mode-p 'oddmuse-mode) (insert "[" url " " user
                                             "@" (url-host url-parsed) "]"))
     ((derived-mode-p 'org-mode) (insert "[[" url "][" user
                                         "@" (url-host url-parsed) "]]"))
     (t (insert url)))))

(autoload 'mastodon-notifications--get-mentions "mastodon-notifications" nil t)
;; Mastodon:1 ends here



;; I use [[https://tusky.app/][Tusky]] on my Android phone in order to share post content with
;; [[https://github.com/orgzly/orgzly-android][Orgzly]] (synchronized via [[https://syncthing.net/][Syncthing]]) so that I can add TODOs or notes
;; to my Org Mode files. The following code makes it easy to open links
;; to things that look like Mastodon URLs by using mastodon.el.


;; [[file:Sacha.org::#mastodon][Mastodon:2]]
(autoload 'mastodon-url-lookup "mastodon")
(add-to-list 'browse-url-handlers '("https?://[^/]+/@[^/]+/.*" . my-mastodon-browse-url))
(defun my-mastodon-browse-url (url &rest _)
  "Open URL."
	(if (string-match "medium\\.com" url)
			(funcall browse-url-browser-function url)
		(mastodon-url-lookup url)))
;; Mastodon:2 ends here

;; Storing Mastodon links in Org mode
;; :PROPERTIES:
;; :CUSTOM_ID: storing-mastodon-links-in-org-mode
;; :END:

;; This snippet makes it easier to store links to posts with
;; ~org-store-link~ and to use them as automatic annotations in
;; ~org-capture~. (2022-12-11: Now it links to media attachments, too!)

;; #+NAME: my-mastodon-store-link

;; [[file:Sacha.org::my-mastodon-store-link][my-mastodon-store-link]]
(defun my-mastodon-store-link ()
  "Store links in Mastodon buffers."
  (when (derived-mode-p 'mastodon-mode)
    (let ((json (get-text-property (point) 'item-json)))
      (org-link-store-props
       :link (mastodon-toot--toot-url)
       :content (mastodon-tl--content json)
       :text
			 (concat
				(string-trim (mastodon-tl--render-text (mastodon-tl--content json)))
				(if (assoc-default 'media_attachments json)
						(concat "\n\n"
										(mapconcat
										 (lambda (attachment)
											 (org-link-make-string
												(assoc-default 'url attachment)
												(assoc-default 'description attachment)))
										 (assoc-default 'media_attachments json)
										 "\n"
										 )))
						"")
				))))

(use-package org
  :config
  (org-link-set-parameters
   "mastodon"
   :store 'my-mastodon-store-link)
	(with-eval-after-load 'org-capture
		(add-to-list 'org-capture-templates
								 `("m" "Mastodon" entry (file ,my-org-inbox-file)
									 "* %?\n\n#+begin_quote\n%:text\n#+end_quote\n\n%a"
									 :prepend t))))
;; my-mastodon-store-link ends here

;; Collecting Emacs News from Mastodon  :emacs:mastodon:
;; :PROPERTIES:
;; :CUSTOM_ID: mastodon-news
;; :EXPORT_DATE: 2024-09-16T13:07:16-0400
;; :EXPORT_ELEVENTY_PERMALINK: /blog/2024/09/collecting-emacs-news-from-mastodon/
;; :EXPORT_ELEVENTY_FILE_NAME: blog/2024/09/collecting-emacs-news-from-mastodon/
;; :END:

;; One of the things I like about browsing Mastodon in Emacs using
;; [[https://codeberg.org/martianh/mastodon.el][mastodon.el]] is that I can modify my workflow to make things easier.
;; For example, I often come across links that I'd like to save for Emacs
;; News. I want to boost the post and save it to an Org file, and I can
;; do that with a single keystroke. It uses the ~my-mastodon-store-link~ function
;; defined elsewhere in [[https://sachachua.com/dotemacs/#mastodon][my config]].


;; [[file:Sacha.org::#mastodon-news][Collecting Emacs News from Mastodon:1]]
(use-package org
	:config
	(add-to-list
	 'org-capture-templates
   '("w" "Emacs News" entry (file+headline "~/sync/orgzly/news.org" "Collect Emacs News")
     "* %a  :news:

#+begin_quote
%:text
#+end_quote

"
     :prepend t :immediate-finish t)))
(defun my-mastodon-save-toot-for-emacs-news ()
	(interactive)
	;; boost if not already boosted
	(unless (get-text-property
					 (car
						(mastodon-tl--find-property-range 'byline (point)))
					 'boosted-p)
		(mastodon-toot--toggle-boost-or-favourite 'boost))
	;; store a link and capture the note
	(org-capture nil "w"))

(use-package mastodon
	:bind (:map mastodon-mode-map ("w" . my-mastodon-save-toot-for-emacs-news)))
;; Collecting Emacs News from Mastodon:1 ends here



;; This puts a bunch of notes in my
;; =~/sync/orgzly/news.org= file. Then I can use
;; =my-emacs-news-summarize-mastodon-items= to
;; summarize a bunch of items I've captured from
;; Mastodon, taking the title from the first link and
;; including a link to the toot using the author's
;; handle. This is what it looks like:

;; #+CAPTION: Quick screencast summarizing Mastodon toots
;; [[file:/home/sacha/recordings/output-2024-09-16-13:14:38.gif]]

;; Here's the code that makes that happen:


;; [[file:Sacha.org::#mastodon-news][Collecting Emacs News from Mastodon:2]]
(defun my-emacs-news-summarize-mastodon-items ()
	(interactive)
	(while (not (eobp))
		(let* ((info (my-mastodon-get-note-info))
					 (title (when (car (plist-get info :links))
										(my-page-title (car (plist-get info :links)))))
					 (summary (read-string
										 (if title
												 (format "Summary (%s): " title)
											 "Summary: ")
										 title)))
			(org-cut-subtree)
			(unless (string= summary "")
				(insert "- " (org-link-make-string
											(or (car (plist-get info :links))
													(plist-get info :url))
											summary)
								(if (and (car (plist-get info :links))
												 (plist-get info :handle))
										(concat " (" (org-link-make-string (plist-get info :url)
																											 (plist-get info :handle))
														")")
									"")
								"\n")))))
(defun my-mastodon-get-note-info ()
	"Return (:handle ... :url ... :links ... :text) for the current subtree."
	(let ((url (org-entry-get (point) "ITEM"))
				beg end
				handle)
		(save-excursion
			(org-back-to-heading)
			(org-end-of-meta-data)
			(setq beg (point))
			(setq end (org-end-of-subtree))
			(when (string-match "https://\\(.+?\\)/\\(@.+?\\)/" url)
				(setq handle (concat
											(match-string 2 url) "@" (match-string 1 url))))

			(list
			 :handle handle
			 :url (if (string-match org-link-bracket-re url) (match-string 1 url) url)
			 :links (mapcar (lambda (o) (org-element-property :raw-link o))
											(my-org-get-links-in-region beg end))
			 :text (string-trim (buffer-substring-no-properties beg end))))))

(ert-deftest my-mastodon-get-note-info ()
 (should
	(equal
	 (with-temp-buffer
		 (insert "** SOMEDAY https://mastodon.online/@jcastp/111762105597746747         :news:
:PROPERTIES:
:CREATED:  [2024-01-22 Mon 05:51]
:END:

jcastp@mastodon.online - I've shared my emacs config: https://codeberg.org/jcastp/emacs.d

After years of reading other's configs, copying really useful snippets, and tinkering a little bit myself, I wanted to give something back, although I'm still an amateur (and it shows, but I want to improve!)

If you can find there something you can use, then I'm happy to be useful to the community.

#emacs
")
		 (org-mode)
		 (my-mastodon-get-note-info))
	 '(:handle "@jcastp@mastodon.online"
						 :url
						 "https://mastodon.online/@jcastp/111762105597746747"
						 :links
						 ("https://codeberg.org/jcastp/emacs.d")
						 :text
						 "jcastp@mastodon.online - I've shared my emacs config: https://codeberg.org/jcastp/emacs.d\n\nAfter years of reading other's configs, copying really useful snippets, and tinkering a little bit myself, I wanted to give something back, although I'm still an amateur (and it shows, but I want to improve!)\n\nIf you can find there something you can use, then I'm happy to be useful to the community.\n\n#emacs"))))
;; Collecting Emacs News from Mastodon:2 ends here

;; Copy Mastodon link for Emacs News
;; :PROPERTIES:
;; :CUSTOM_ID: copy-mastodon-link-for-emacs-news
;; :END:


;; [[file:Sacha.org::#copy-mastodon-link-for-emacs-news][Copy Mastodon link for Emacs News:1]]
(defun my-mastodon-copy-link-dwim (prefix)
	(interactive "P")
	(if prefix
			(mastodon-toot--copy-toot-url)
		(my-mastodon-copy-toot-as-author-link)))

(defun my-emacs-news-copy-mastodon-link ()
	(interactive)
	(let ((url (org-entry-get (point) "ITEM")))
		(when (string-match "https://\\(.+?\\)/\\(@.+?\\)/" url)
			(kill-new (org-link-make-string url (concat (match-string 2 url) "@" (match-string 1 url)))))))

(defun my-emacs-news-copy-mastodon-item (&optional name-only)
	(interactive (list current-prefix-arg))
	(let (s)
		(with-current-buffer
				(if (string-match "emacs-news/index.org" (buffer-file-name))
						(save-window-excursion
							(other-window 1)
							(current-buffer))
					(current-buffer))
			(let ((url (or (thing-at-point 'url)
										 (progn
											 (save-restriction
												 (org-back-to-heading)
												 (org-narrow-to-subtree)
												 (org-end-of-meta-data)
												 (if (re-search-forward org-link-any-re nil t)
														 (thing-at-point 'url)
													 (setq name-only t)
													 (org-entry-get (point) "ITEM")
													 )))))
						(toot (org-entry-get (point) "ITEM"))
						attrib)
				(when (string-match org-link-bracket-re toot)
					(setq toot (match-string 1 toot)))
				(when (string-match "https://\\(.+?\\)/\\(@.+?\\)/" toot)
					(setq attrib (org-link-make-string toot
																						 (concat
																							(match-string 2 toot) "@" (match-string 1 toot)))))
				(setq s
							(if name-only
									(format " (%s)" attrib)
								(format "- %s (%s)\n"
												(org-link-make-string
												 url
												 (my-page-title url))
												attrib)))))
		(when (called-interactively-p 'any)
			(if (string-match "emacs-news/index.org" (buffer-file-name))
					(insert s)
				(kill-new s)))
		s))
;; Copy Mastodon link for Emacs News:1 ends here

;; Combining Mastodon timelines using mastodon.el                    :emacs:mastodon:
;; :PROPERTIES:
;; :EXPORT_DATE: 2024-09-13T12:55:25-0400
;; :EXPORT_ELEVENTY_PERMALINK: /blog/2024/09/combining-mastodon-timelines-using-mastodon-el/
;; :EXPORT_ELEVENTY_FILE_NAME: blog/2024/09/combining-mastodon-timelines-using-mastodon-el/
;; :EXPORT_MODIFIED: 2024-10-07T10:37:51-0400
;; :header-args:emacs-lisp+: :results silent :exports code :eval never-export
;; :CUSTOM_ID: mastodon-combined-timeline
;; :END:
;; :LOGBOOK:
;; - State "DONE"       from              [2024-09-13 Fri 14:09]
;; :END:

;; #+begin_update
;; - [2024-10-07]: Added screenshot.
;; - [2024-09-16]: Read JSON arrays as lists to be compatible with the latest mastodon.el.
;; #+end_update

;; #+CAPTION: Screenshot of combined timeline in mastodon.el
;; #+ATTR_HTML: :data-link t
;; [[file:/home/sacha/recordings/2024-10-07-10-37-39.svg]]

;; I like checking out the #emacs hashtag when I put together Emacs News. In the past, I usually browsed the hashtag timeline on emacs.ch, which also picked up updates from other people that emacs.ch was following. Now that [[https://sachachua.com/blog/2024/09/moving-from-sachac-emacs-ch-to-sacha-social-sachachua-com/][I've moved to @sacha@social.sachachua.com]] and [[https://emacs.ch/@emacs/113087752901949391][emacs.ch is winding down]], I wanted to see if there was a way for me to see a combined view using [[https://mastodon.social/api/v1/timelines/tag/emacs?count=40][mastodon.social's API feed]] (paging by
;; =max_id= as needed). I haven't enabled public
;; timeline feeds on my server, so I also need to reuse the OAuth mechanics from mastodon.el.

;; First, let's start by making a unified timeline. By digging around in =mastodon-tl.el=, I found that I could easily create a timeline view by passing it a vector of toot JSONs.


;; [[file:Sacha.org::#mastodon-combined-timeline][Combining Mastodon timelines using mastodon.el:1]]
(defun my-mastodon-fetch-posts-after (base-url after-date)
	"Page backwards through BASE-URL using max_id for all the posts after AFTER-DATE."
	(require 'plz)
	(require 'mastodon-http)
	(let ((results [])
				(url base-url)
				(use-mastodon-el (not (string-match "^http" base-url)))
				(json-array-type 'list)
				page filtered)
		(while url
			(setq page (if use-mastodon-el
										 (mastodon-http--get-json (mastodon-http--api url) nil :silent)
									 (seq-map (lambda (o)
															(cons (cons 'external t) o))
														(plz 'get url :as #'json-read)))
						filtered (seq-filter (lambda (o) (string< after-date (assoc-default 'created_at o)))
																 page))
			(if filtered
					(progn
						(setq results (seq-concatenate 'vector filtered results)
									url (concat base-url (if (string-match "\\?" base-url) "&" "?")
															"max_id=" (number-to-string (1- (string-to-number (assoc-default 'id (elt (last page) 0)))))))
						(message "%s %s" (assoc-default 'created_at (elt (last page) 0)) url))
				(setq url nil)))
		results))

(defun my-mastodon-combined-tag-timeline (later-than tag servers)
	"Display items after LATER-THAN about TAG from SERVERS and the current mastodon.el account."
	(interactive (list
								(org-read-date nil nil nil nil nil "-Mon")
								"#emacs"
								'("mastodon.social" "emacs.ch" "fosstodon.org")))
	(require 'mastodon)
	(require 'mastodon-tl)
	(require 'mastodon-toot)
	(let* ((limit 40)
				 (sources (cons (format "timelines/tag/emacs?count=%d" limit)
												(mapcar (lambda (s)
																	(format "https://%s/api/v1/timelines/tag/emacs?count=%d" s limit))
																servers)))
				 (combined
					(sort
					 (seq-reduce (lambda (prev val)
												 (seq-union prev
																		(my-mastodon-fetch-posts-after val later-than)
																		(lambda (a b) (string= (assoc-default 'uri a)
																													 (assoc-default 'uri b)))))
											 sources [])
					 (lambda (a b)
						 (string< (assoc-default 'created_at b)
											(assoc-default 'created_at a))))))
		(with-current-buffer (get-buffer-create "*Combined*")
			(let ((inhibit-read-only t))
				(erase-buffer)
				(mastodon-tl--timeline combined)
				(mastodon-mode))
			(setq mastodon-tl--buffer-spec `(account ,(cons mastodon-active-user mastodon-instance-url) buffer-name ,(buffer-name)))
			(display-buffer (current-buffer)))))
;; Combining Mastodon timelines using mastodon.el:1 ends here



;; The tricky thing is that boosting and replying in
;; mastodon.el both use the toot IDs instead of the
;; toot URLs, so they only work for toots that came
;; in via my current mastodon.el account. Toots from
;; other timelines might not have been fetched by my
;; server yet. Adding an =external= property lets me
;; find that in the =item_json= text property in the
;; timeline buffer. For those toots, I can use
;; =(mastodon-url-lookup (mastodon-toot--toot-url))=
;; to open the toot in a new buffer that does allow
;; boosting or replying, which is probably enough for
;; my purposes.


;; [[file:Sacha.org::#mastodon-combined-timeline][Combining Mastodon timelines using mastodon.el:2]]
(defun my-mastodon-lookup-toot ()
  (interactive)
  (mastodon-url-lookup (mastodon-toot--toot-url)))
;; Combining Mastodon timelines using mastodon.el:2 ends here



;; When I go through Emacs News, I have a shortcut
;; that boosts a post and saves it to as an Org Mode
;; capture with a link to the toot. I sometimes want
;; to reply, too. So I just need to intervene before
;; boosting and replying. Boosting and favoriting
;; both use =mastodon-toot--action=, which looks up
;; the =base-item-id= text property. Replying looks
;; up the =item-json= property and gets the =id= from
;; it.


;; [[file:Sacha.org::#mastodon-combined-timeline][Combining Mastodon timelines using mastodon.el:3]]
(defun my-text-property-update-at-point (pos prop value)
	(let ((start (previous-single-property-change (or pos (point)) prop))
				(end (next-single-property-change (or pos (point)) prop)))
		(put-text-property (or start (point-min))
											 (or end (point-max))
											 prop value)))

(defun my-mastodon-update-external-item-id (&rest _)
	(when (mastodon-tl--field 'external (mastodon-tl--property 'item-json))
		;; ask the server to resolve it
		(let* ((response (mastodon-http--get-json (format "%s/api/v2/search" mastodon-instance-url)
																							`(("q" . ,(mastodon-toot--toot-url))
																								("resolve" . "t"))))
					 (id (alist-get 'id (seq-first (assoc-default 'statuses response))))
					 (inhibit-read-only t)
					 (json (get-text-property (point) 'item-json)))
			(when (and id json)
				(my-text-property-update-at-point (point) 'base-item-id id)
				(my-text-property-update-at-point (point) 'item-json
																					(progn
																						(setf (alist-get 'id json) id)
																						(setf (alist-get 'external json) nil)
																						json))))))
;; Combining Mastodon timelines using mastodon.el:3 ends here



;; So now all I need to do is make sure that this is called before the relevant mastodon.el functions if I'm looking at an external toot.


;; [[file:Sacha.org::#mastodon-combined-timeline][Combining Mastodon timelines using mastodon.el:4]]
(with-eval-after-load 'mastodon-tl
	(advice-add #'mastodon-toot--action :before #'my-mastodon-update-external-item-id)
	(advice-add #'mastodon-toot--reply :before #'my-mastodon-update-external-item-id)
	(advice-add #'mastodon-tl--thread :before #'my-mastodon-update-external-item-id))
;; Combining Mastodon timelines using mastodon.el:4 ends here

;; Following people
;; :PROPERTIES:
;; :CUSTOM_ID: following-people
;; :END:
;; I want to be able to follow people if I specify their ID.


;; [[file:Sacha.org::#following-people][Following people:1]]
(defun my-mastodon-follow-user (user-handle)
	"Follow HANDLE."
	(interactive "MHandle: ")
	(when (string-match "https?://\\(.+?\\)/\\(@.+\\)" user-handle)
		(setq user-handle (concat (match-string 2 user-handle) "@" (match-string 1 user-handle))))
	(let* ((account (mastodon-profile--search-account-by-handle
                   user-handle))
				 (user-id (mastodon-profile--account-field account 'id))
				 (name (if (not (string-empty-p (mastodon-profile--account-field account 'display_name)))
                   (mastodon-profile--account-field account 'display_name)
								 (mastodon-profile--account-field account 'username)))
				 (url (mastodon-http--api (format "accounts/%s/%s" user-id "follow"))))
		(if account
				(mastodon-tl--do-user-action-function url name user-handle "follow")
			(message "Cannot find a user with handle %S" user-handle))))
;; Following people:1 ends here

;; Compose a Mastodon toot with the current Org subtree
;; :PROPERTIES:
;; :CUSTOM_ID: mastodon-toot-subtree
;; :END:

;; I want to make it easier to microblog the current Org subtree.


;; [[file:Sacha.org::#mastodon-toot-subtree][Compose a Mastodon toot with the current Org subtree:1]]
(defun my-mastodon-toot-subtree ()
	"Compose a buffer and include the current subtree."
	(interactive)
	(let ((text (org-export-as 'md t nil t)))
		(mastodon-toot)
		(insert text)))
;; Compose a Mastodon toot with the current Org subtree:1 ends here

;; Posting the latest screenshot with mastodon.el
;; :PROPERTIES:
;; :CUSTOM_ID: posting-the-latest-screenshot-with-mastodon-el
;; :END:

;; I want to make it easier to microblog the latest screenshot, or a
;; recent screenshot if I need to pick a different one. It might also be
;; a good time to add some text to the filename to make it easier to find
;; later on. I can use that text as alt-text for the image, too.


;; [[file:Sacha.org::#posting-the-latest-screenshot-with-mastodon-el][Posting the latest screenshot with mastodon.el:1]]
(defun my-mastodon-toot-screenshot (&optional filename description)
	"Compose a buffer and attach the latest screenshot.
Prompt for a description and add that to the filename as well.
When called with a prefix argument, prompt for the file.
Use consult to provide a preview."
	(interactive
	 (let ((filename
					(if current-prefix-arg
							(expand-file-name
							 (consult--read
								(reverse (directory-files my-screenshot-directory nil "\\.png$"))
								:sort nil
								:require-match t
								:category 'file
								:state (lambda (candidate state)
												 (when candidate
													 (with-current-buffer (find-file-noselect
																								 (expand-file-name candidate my-screenshot-directory))
														 (display-buffer (current-buffer))))))
							 my-screenshot-directory)
						(my-latest-file my-screenshot-directory))))
		 (list
			filename
			(when (string-match "^[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]_[0-9][0-9]-[0-9][0-9]-[0-9][0-9]$" (file-name-base filename))
				(display-buffer (find-file-noselect filename))
				(read-string "Description: ")))))
	(let ((new-filename (if (string= (or description "") "")
													nil
												(expand-file-name
												 (concat (file-name-base filename) " " description
																 (file-name-extension filename))
												 (file-name-directory filename)))))
		(if new-filename
				(rename-file filename new-filename))
		(unless (string-match "new toot" (buffer-name)) ; can't match off major mode yet
			(mastodon-toot))
		(mastodon-toot--attach-media
		 (or new-filename filename) "image/png"
		 (or description
				 (when (string-match "^[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]_[0-9][0-9]-[0-9][0-9]-[0-9][0-9] \\(.+\\)" (save-match-data (file-name-base filename)))
					 (match-string 1 (save-match-data (file-name-base filename))))))))
;; Posting the latest screenshot with mastodon.el:1 ends here

;; Mastodon keyboard shortcuts via Hydra
;; :PROPERTIES:
;; :CUSTOM_ID: mastodon-keyboard-shortcuts-via-hydra
;; :END:

;; Based on https://github.com/holgerschurig/emacs-doom-config/blob/master/config.el#L2397


;; [[file:Sacha.org::#mastodon-keyboard-shortcuts-via-hydra][Mastodon keyboard shortcuts via Hydra:1]]
;; Not in the following hydra, but mentioned in "M-x describe-mode". Also, the README.org
  ;; contains several functions that aren't in my hydra.
  ;;
  ;; TAB                     mastodon-tl--next-tab-item
  ;; D                       mastodon-toot--delete-and-redraft-toot
  ;; C-S-b                   mastodon-tl--unblock-user
  ;; S-TAB                   mastodon-tl--previous-tab-item
  ;; S-RET                   mastodon-tl--unmute-user
  ;; C-S-w                   mastodon-tl--unfollow-user
  ;; S-SPC                   scroll-down-command
  ;; <backtab>               mastodon-tl--previous-tab-item
  ;; C-M-i                   mastodon-tl--previous-tab-item
  ;; M-n                     mastodon-tl--next-tab-item
  ;; M-p                     mastodon-tl--previous-tab-item

  (defhydra my-mastodon-help (:color blue :hint nil)
    "
Timelines^^   Toots^^^^           Own Toots^^   Profiles^^      Users/Follows^^  Misc^^
^^-----------------^^^^--------------------^^----------^^-------------------^^------^^-----
_h_ome        _n_ext _p_rev       _r_eply       _A_uthors       follo_W_         _X_ lists
_l_ocal       _T_hread of toot^^  wri_t_e       user _P_rofile  _N_otifications  f_I_lter
_F_ederated   (un) _b_oost^^      _e_dit        ^^              _R_equests       _C_opy URL
fa_V_orites   (un) _f_avorite^^   _d_elete      _O_wn           su_G_estions     _S_earch
_#_ tagged    (un) p_i_n^^        ^^            _U_pdate own    _M_ute user      _H_elp
_@_ mentions  (un) boo_k_mark^^   show _E_dits  ^^              _B_lock user
boo_K_marks   _v_ote^^
trendin_g_
_u_pdate      _w_rite Emacs news  _o_rg  _s_creenshot
"
		;; my custom stuff
		("s" my-mastodon-toot-screenshot)
		("w" my-mastodon-save-toot-for-emacs-news)
		("o" (org-capture nil "m"))
		;; more general things
    ("h" (progn (require 'mastodon) mastodon-tl--get-home-timeline))

    ("l" mastodon-tl--get-local-timeline)
    ("F" mastodon-tl--get-federated-timeline)
    ("V" mastodon-profile--view-favourites)
    ("#" mastodon-tl--get-tag-timeline)
    ("@" (progn (require 'mastodon) (mastodon-notifications--get-mentions)))
    ("K" mastodon-profile--view-bookmarks)
    ("g" mastodon-search--trending-tags)
    ("u" mastodon-tl--update :exit nil)

    ("n" mastodon-tl--goto-next-toot)
    ("p" mastodon-tl--goto-prev-toot)
    ("T" mastodon-tl--thread)
    ("b" mastodon-toot--toggle-boost :exit nil)
    ("f" mastodon-toot--toggle-favourite :exit nil)
    ("i" mastodon-toot--pin-toot-toggle :exit nil)
    ("k" mastodon-toot--bookmark-toot-toggle :exit nil)
    ("c" mastodon-tl--toggle-spoiler-text-in-toot)
    ("v" mastodon-tl--poll-vote)

    ("A" mastodon-profile--get-toot-author)
    ("P" mastodon-profile--show-user)
    ("O" mastodon-profile--my-profile)
    ("U" mastodon-profile--update-user-profile-note)

    ("W" mastodon-tl--follow-user)
    ("N" mastodon-notifications-get)
    ("R" mastodon-profile--view-follow-requests)
    ("G" mastodon-tl--get-follow-suggestions)
    ("M" mastodon-tl--mute-user)
    ("B" mastodon-tl--block-user)

    ("r" mastodon-toot--reply)
    ("t" mastodon-toot)
    ("e" mastodon-toot--edit-toot-at-point)
    ("d" mastodon-toot--delete-toot)
    ("E" mastodon-toot--view-toot-edits)

    ("I" mastodon-tl--view-filters)
    ("X" mastodon-tl--view-lists)
    ("C" mastodon-toot--copy-toot-url)
    ("S" mastodon-search--search-query)
    ("H" describe-mode)

    ("q" nil :exit t)
  )
(use-package mastodon
 :bind ("s-m" . my-mastodon-help/body))
;; Mastodon keyboard shortcuts via Hydra:1 ends here

;; Making it easier to toot my config
;; :PROPERTIES:
;; :CUSTOM_ID: mastodon-toot-config
;; :END:

;; The following snippet composes a toot buffer with a link to the
;; relevant section of my configuration file, or to the relevant blog
;; post if specified.


;; [[file:Sacha.org::#mastodon-toot-config][Making it easier to toot my config:1]]
(defun my-mastodon-toot-config (&optional include-screenshot)
	"Toot this part of my config."
	(interactive (list current-prefix-arg))
	(let ((link (if (org-entry-get (point) "EXPORT_ELEVENTY_PERMALINK")
									(concat "https://sachachua.com" (org-entry-get (point) "EXPORT_ELEVENTY_PERMALINK"))
								(concat "https://sachachua.com/dotemacs/#" (org-entry-get (point) "CUSTOM_ID"))))
				text)
		(save-excursion
			(org-back-to-heading)
			(org-end-of-meta-data)
			(setq text (buffer-substring (point) (org-end-of-subtree))))
		(mastodon-toot)
		(insert text "\n\nLink: " link)))
;; Making it easier to toot my config:1 ends here

;; Capture
;; :PROPERTIES:
;; :CUSTOM_ID: mastodon-org-contacts-capture
;; :END:


;; [[file:Sacha.org::#mastodon-org-contacts-capture][Capture:1]]
(defun my-mastodon-org-contact-add ()
	"Add current toot author as a contact."
	(interactive)
	(let-alist (get-text-property (point) 'item-json)
		(with-current-buffer (find-file-noselect (car org-contacts-files))
			(if (org-find-property "MASTODON" .account.acct)
					(message "Already exists.")
				(org-insert-heading)
				(insert (format "%s\n:PROPERTIES:\n:NAME: %s\n:MASTODON: %s\n:ALIAS: %s\n:END:\n"
												.account.display_name
												.account.display_name
												.account.acct
												.account.username))
				(message "Added %s" .account.acct)))))
;; Capture:1 ends here

;; Completion
;; :PROPERTIES:
;; :CUSTOM_ID: mastodon-org-contacts-complete
;; :END:


;; [[file:Sacha.org::#mastodon-org-contacts-complete][Completion:1]]
(defun my-org-contacts-complete-mastodon (string)
	(let* ((completion-ignore-case org-contacts-completion-ignore-case)
				 (completion-list
					(cl-loop for contact in (org-contacts-filter)
									 ;; The contact name is always the car of the assoc-list
									 ;; returned by `org-contacts-filter'.
									 for contact-name = (car contact)
									 ;; Build the list of the Mastodon handles which have expired
									 for ignore-list = (org-contacts-split-property
																			(or (cdr (assoc-string org-contacts-ignore-property
																														 (nth 2 contact))) ""))
									 ;; Build the list of the user Mastodon handles.
									 for handle-list = (org-contacts-remove-ignored-property-values
																			ignore-list
																			(org-contacts-split-property
																			 (or (cdr (assoc-string "MASTODON"
																															(nth 2 contact))) "")))
									 nconc (cl-loop for handle in handle-list
																	collect (format "%s (%s)" contact-name handle))))
				 (completion-list (org-contacts-all-completions-prefix
													 string
													 (org-uniquify completion-list))))
		(when completion-list
			(org-contacts-make-collection-prefix completion-list))))

(defun my-mastodon-complete-contact ()
	"Suitable for adding to `completion-at-point-functions'."
	(interactive)
	(let ((beg
				 (save-excursion
					 (re-search-backward "\\(\\`\\|[\n:,]\\)[ \t]*")
					 (goto-char (match-end 0))
           (point)))
				(end (point)))
    (list beg
          end
          (completion-table-dynamic
           (lambda (string)
             (my-org-contacts-complete-mastodon string))))))
(with-eval-after-load 'mastodon-toot
	(with-eval-after-load 'org-contacts
		(add-hook 'mastodon-toot-mode-hook
							(lambda ()
								(add-hook 'completion-at-point-functions
													#'my-mastodon-complete-contact nil t)))))
;; Completion:1 ends here

;; Copy Mastodon toot URL as author link


;; [[file:Sacha.org::*Copy Mastodon toot URL as author link][Copy Mastodon toot URL as author link:1]]
(defun my-mastodon-copy-toot-as-author-link ()
	(interactive)
  (let* ((url (mastodon-toot--toot-url))
				 (handle (concat "@"
												 (let-alist (or (mastodon-tl--property 'base-toot)
																				(mastodon-tl--property 'item-json))
													 .account.acct))))
		;; figure out how to properly add to org-stored-links someday
		(kill-new (org-link-make-string url handle))
		(message "Link stored (%s, %s)." handle url)))
;; Copy Mastodon toot URL as author link:1 ends here

;; Collect my recent toots in an Org file so that I can refile them
;; :PROPERTIES:
;; :CREATED:  [2022-12-13 Tue 15:55]
;; :CUSTOM_ID: mastodon-org-feed
;; :EXPORT_DATE: 2022-12-19T12:53:34-0500
;; :EXPORT_ELEVENTY_PERMALINK: /blog/2022/12/collect-my-recent-toots-in-an-org-file-so-that-i-can-refile-them/
;; :EXPORT_ELEVENTY_FILE_NAME: blog/2022/12/collect-my-recent-toots-in-an-org-file-so-that-i-can-refile-them/
;; :EXPORT_ELEVENTY_CATEGORIES: emacs mastodon org
;; :END:

;; I want to use my microblog posts on Mastodon as building blocks for
;; longer posts on my blog. Getting them into an Org file makes it easier
;; to link to them or refile them to other parts of my Org files so that
;; I can build up my notes.


;; [[file:Sacha.org::#mastodon-org-feed][Collect my recent toots in an Org file so that I can refile them:1]]
(use-package pandoc)
(defun my-mastodon-org-feed-formatter (entry)
	(concat "* " (pandoc-convert-stdio
								(dom-text (dom-by-tag
													 (with-temp-buffer
														 (insert "<item>"
																		 (plist-get entry :item-full-text)
																		 "</item>")
														 (xml-parse-region (point-min) (point-max)))
													 'description))
								"html" "org")
					"\n\n[" (format-time-string (cdr org-time-stamp-formats)
																			(date-to-time (plist-get entry :pubDate)))
"]\n" (plist-get entry :link)))
(setq org-feed-alist '(("Mastodon" "https://emacs.ch/@sachac/with_replies.rss"
												"~/sync/orgzly/toots.org" "Toots"
												:formatter my-mastodon-org-feed-formatter)))
(defun my-org-feed-sort (pos entries)
	(save-excursion
    (goto-char pos)
    (when (looking-at org-complex-heading-regexp)
			(org-sort-entries nil ?T))))
(advice-add #'org-feed-add-items :after #'my-org-feed-sort)
;; Collect my recent toots in an Org file so that I can refile them:1 ends here

;; Archive toots on my blog
;; :PROPERTIES:
;; :CUSTOM_ID: mastodon-insert-statuses
;; :END:

;; I want to compile my public microblog posts into
;; weekly posts so that they're archived on my blog.
;; It might make sense to make them list items or
;; subtrees so that I can move them around easily.


;; [[file:Sacha.org::#mastodon-insert-statuses][Archive toots on my blog:1]]
(defun my-mastodon-insert-my-toots-since (date)
	(interactive (list (org-read-date "Since date: ")))
	(require 'mastodon-auth)
	(insert
	 (format "#+begin_toot_archive\n%s\n#+end_toot_archive\n"
					 (mapconcat
						(lambda (o)
							(format "- %s\n  #+begin_quote\n  #+begin_export html\n%s\n  #+end_export\n  #+end_quote\n\n"
											(org-link-make-string (assoc-default 'url o) (assoc-default 'created_at o))
											(org-ascii--indent-string (assoc-default 'content o) 2))
							;; (format "#+begin_quote\n#+begin_export html\n%s\n#+end_export\n#+end_quote\n\n%s\n\n"
							;; 				(assoc-default 'content o)
							;; 				(org-link-make-string (assoc-default 'url o) (assoc-default 'created_at o)))
							)
						(seq-filter
						 (lambda (o)
							 (string= (assoc-default 'visibility o) "public"))
						 (my-mastodon-fetch-posts-after
							(format "accounts/%s/statuses?count=40&exclude_reblogs=t&exclude_replies=t" (mastodon-auth--get-account-id))
							date))
						""))))
;; Archive toots on my blog:1 ends here

;; Checking URLs
;; :PROPERTIES:
;; :CUSTOM_ID: checking-urls
;; :END:


;; [[file:Sacha.org::#checking-urls][Checking URLs:1]]
(defun my-test-urls (urls)
  "Given a list of URLs, return a list of any URLS that don't result in an OK value."
  (delq nil
        (mapcar (lambda (url)
                  (let ((url-request-method "HEAD"))
                    (with-current-buffer (url-retrieve-synchronously url)
                      (goto-char (point-min))
                      (unless (looking-at "HTTP/1.1 200 OK") url))))
                urls)))
;; Checking URLs:1 ends here

;; Search
;; :PROPERTIES:
;; :CUSTOM_ID: search
;; :END:


;; [[file:Sacha.org::#search][Search:1]]
(use-package engine-mode
  :config
  (defengine my-blog "https://www.google.ca/search?q=site:sachachua.com+%s" :keybinding "b")
  (defengine mail "https://mail.google.com/mail/u/0/#search/%s" :keybinding "m")
  (defengine google "https://google.com/search?q=%s" :keybinding "g")
  (defengine emacswiki "https://google.com/search?q=site:emacswiki.org+%s" :keybinding "e")
  (engine-mode)
  :hydra
  (my-engine-mode-hydra
   (:color blue)
   "Engine mode"
   ("b" engine/search-my-blog "blog")
   ("m" engine/search-mail "mail")
   ("g" engine/search-google "google")
   ("e" engine/search-emacswiki "emacswiki")))
;; Search:1 ends here



;; #+RESULTS:
;; :results:
;; :end:

;; [[video:/home/sacha/recordings/2024-01-19_08.16.31.webm?caption=Evaluating a Javascript block with :spookfox t]]

;; To do this, we wrap some advice around the ~org-babel-execute:js~ function that's called by ~org-babel-execute-src-block~.


;; [[file:Sacha.org::#spookfox-babel][Running the current Org Mode Babel Javascript block from Emacs using Spookfox:4]]
(defun my-org-babel-execute:js-spookfox (old-fn body params)
	"Maybe execute Spookfox."
	(if (assq :spookfox params)
			(spookfox-js-injection-eval-in-active-tab
			 body t)
		(funcall old-fn body params)))
(with-eval-after-load 'ob-js
	(advice-add 'org-babel-execute:js :around #'my-org-babel-execute:js-spookfox))
;; Running the current Org Mode Babel Javascript block from Emacs using Spookfox:4 ends here



;; I can also run the block in Spookfox without adding the parameter if I make an interactive function:


;; [[file:Sacha.org::#spookfox-babel][Running the current Org Mode Babel Javascript block from Emacs using Spookfox:5]]
(defun my-spookfox-eval-org-block ()
	(interactive)
	(let ((block (org-element-context)))
		(when (and (eq (org-element-type block) 'src-block)
							 (string= (org-element-property :language block) "js"))
			(spookfox-js-injection-eval-in-active-tab
			 (nth 2 (org-src--contents-area block))
			 t))))
;; Running the current Org Mode Babel Javascript block from Emacs using Spookfox:5 ends here



;; I can add that as an Embark context action:


;; [[file:Sacha.org::#spookfox-babel][Running the current Org Mode Babel Javascript block from Emacs using Spookfox:6]]
(with-eval-after-load 'embark-org
	(define-key embark-org-src-block-map "f" #'my-spookfox-eval-org-block))
;; Running the current Org Mode Babel Javascript block from Emacs using Spookfox:6 ends here



;; In Javascript buffers, I want the ability to send the current line, region, or buffer too, just like package:nodejs-repl does.


;; [[file:Sacha.org::#spookfox-babel][Running the current Org Mode Babel Javascript block from Emacs using Spookfox:7]]
(defun my-spookfox-send-region (start end)
	(interactive "r")
	(spookfox-js-injection-eval-in-active-tab (buffer-substring start end) t))

(defun my-spookfox-send-buffer ()
	(interactive)
	(my-spookfox-send-region (point-min) (point-max)))

(defun my-spookfox-send-line ()
	(interactive)
	(my-spookfox-send-region (line-beginning-position) (line-end-position)))

(defun my-spookfox-send-last-expression ()
	(interactive)
	(my-spookfox-send-region (save-excursion (nodejs-repl--beginning-of-expression)) (point)))

(defvar-keymap my-js-spookfox-minor-mode-map
	:doc "Send parts of the buffer to Spookfox."
	"C-x C-e" 'my-spookfox-send-last-expression
	"C-c C-j" 'my-spookfox-send-line
	"C-c C-r" 'my-spookfox-send-region
	"C-c C-c" 'my-spookfox-send-buffer)

(define-minor-mode my-js-spookfox-minor-mode "Send code to Spookfox.")
;; Running the current Org Mode Babel Javascript block from Emacs using Spookfox:7 ends here



;; I usually edit Javascript files with package:js2-mode, so I can use ~my-js-spookfox-minor-mode~ in addition to that.

;; I can turn the minor mode on automatically for ~:spookfox t~ source blocks. There's no ~org-babel-edit-prep:js~ yet, I think, so we need to define it instead of advising it.


;; [[file:Sacha.org::#spookfox-babel][Running the current Org Mode Babel Javascript block from Emacs using Spookfox:8]]
(defun org-babel-edit-prep:js (info)
	(when (assq :spookfox (nth 2 info))
		(my-js-spookfox-minor-mode 1)))
;; Running the current Org Mode Babel Javascript block from Emacs using Spookfox:8 ends here

;; Using Spookfox to scroll Firefox up and down from Emacs      :web:emacs:
;; :PROPERTIES:
;; :EXPORT_DATE: 2023-01-30T11:02:57-0500
;; :EXPORT_ELEVENTY_PERMALINK: /blog/2023/01/using-spookfox-to-scroll-firefox-up-and-down-from-emacs/
;; :EXPORT_ELEVENTY_FILE_NAME: blog/2023/01/using-spookfox-to-scroll-firefox-up-and-down-from-emacs/
;; :CUSTOM_ID: spookfox
;; :END:

;; I open lots of pages in the process of making Emacs News. I like to
;; open the pages in Mozilla Firefox, but I want the keyboard focus to
;; stay with Emacs so that I can quickly categorize the links. I also
;; sometimes want to scroll the page up or down. While reading the
;; [[https://bitspook.in/blog/reading-and-not-forgetting/][Reading, and not forgetting]] post, I came across [[https://github.com/bitspook/spookfox][Spookfox]], which
;; bridges Emacs and Firefox using an Firefox add-on and websockets.
;; After I started spookfox and connected to it by clicking on the
;; extension in Firefox, I was able to interact with it from Emacs Lisp.
;; I feel a little nervous about it security-wise, but at least it's only
;; listening on the local port. There might be another way to do it with
;; the Marionette support in Firefox, but I haven't looked into it yet.


;; [[file:Sacha.org::#spookfox][Using Spookfox to scroll Firefox up and down from Emacs:1]]
(use-package spookfox
	;:quelpa (spookfox :fetcher github :repo "bitspook/spookfox"
  ; :files ("lisp/*.el" "lisp/apps/*.el"))
	:load-path ("~/vendor/spookfox/lisp" "~/vendor/spookfox/lisp/apps")
	:when my-laptop-p
	:config
	(require 'spookfox-tabs)
	;(require 'spookfox-org-tabs)
	(require 'spookfox-js-injection)
	(add-to-list 'spookfox-enabled-apps 'spookfox-tabs)
	(with-eval-after-load 'spookfox-org-tabs (add-to-list 'spookfox-enabled-apps 'spookfox-org-tabs))
	(add-to-list 'spookfox-enabled-apps 'spookfox-js-injection)
	;; (spookfox-init) ; don't automatically enable it; run (spookfox-init) to manually enable
	)
;; Using Spookfox to scroll Firefox up and down from Emacs:1 ends here



;; Anyway, this code seems to do the job of scrolling my Firefox window:


;; [[file:Sacha.org::#spookfox][Using Spookfox to scroll Firefox up and down from Emacs:2]]
(defun my-spookfox-scroll-down ()
	(interactive)
 	(spookfox-js-injection-eval-in-active-tab "window.scrollBy(0, document.documentElement.clientHeight);" t))

(defun my-spookfox-scroll-up ()
	(interactive)
 	(spookfox-js-injection-eval-in-active-tab "window.scrollBy(0, -document.documentElement.clientHeight);"))

(keymap-global-set "C-s-v" 'my-spookfox-scroll-down)
(keymap-global-set "S-s-v" 'my-spookfox-scroll-up)
;; Using Spookfox to scroll Firefox up and down from Emacs:2 ends here



;; This code opens a tab without switching keyboard focus away from Emacs:


;; [[file:Sacha.org::#spookfox][Using Spookfox to scroll Firefox up and down from Emacs:3]]
(defun my-spookfox-background-tab (url &rest args)
	"Open URL as a background tab."
	(if spookfox--connected-clients
			(spookfox-tabs--request (cl-first spookfox--connected-clients) "OPEN_TAB" `(:url ,url))
		(browse-url url)))
;; Using Spookfox to scroll Firefox up and down from Emacs:3 ends here



;; My Emacs News code for processing my upvoted Reddit posts can
;; automatically grab the links from Reddit link posts, but sometimes
;; people post Reddit text or image posts and then include the link to
;; the actual project in the post body or a comment instead.


;; [[file:Sacha.org::#spookfox][Using Spookfox to scroll Firefox up and down from Emacs:4]]
(defun my-spookfox-get-links ()
	(seq-uniq
 	 (spookfox-eval-js-in-active-tab "[...(document.querySelector('[data-testid=post-container]')?.parentElement || document).querySelectorAll('a')].map(a => a.href).filter(a => a && (!window.location.host.match(/reddit/) || !a.match(/redd\.?it/)) && !a.match(window.location.host))" t)))
;;https://emacs.stackexchange.com/questions/41801/how-to-stop-completing-read-ivy-completing-read-from-sorting
(defun my-presorted-completion-table (completions)
  (lambda (string pred action)
    (if (eq action 'metadata)
        '(metadata
					(cycle-sort-function . identity)
					(display-sort-function . identity))
      (complete-with-action action completions string pred))))

(defun my-spookfox-complete-link (&optional prompt)
	(completing-read
	 (or prompt "Link: ")
	 (my-presorted-completion-table
		(my-spookfox-get-links))))

(defun my-spookfox-insert-link-from-page (link)
	(interactive (list (my-spookfox-complete-link)))
	(insert (org-link-make-string link (my-page-title link))))

(defun my-spookfox-open-link-from-page (link)
	(interactive (list (my-spookfox-complete-link)))
	(my-spookfox-background-tab link))

(defun my-spookfox-insert-link-to-tab ()
	(interactive)
	(let ((tab (spookfox-request-active-tab)))
		(insert (org-link-make-string
						 (plist-get tab :url)
						 (plist-get tab :title)))))
;; Using Spookfox to scroll Firefox up and down from Emacs:4 ends here

;; Emacs and Spookfox: org-capture the current tab from Firefox or a link from the page
;; :PROPERTIES:
;; :CUSTOM_ID: spookfox-insert-url
;; :END:

;; I want to quickly capture notes based on the current tab in Firefox or
;; a link from the page's main body. I have the [[https://addons.mozilla.org/en-CA/firefox/addon/org-capture/][Org Capture Firefox
;; extension]] and ~Ctrl-Shift-L~ seems to be the keyboard shortcut for
;; capturing with it, so I probably just have to get the hang of using
;; it.

;; I also want to make it easier to add notes even when I've already
;; switched back to Emacs. I could use ~s-2~ to shift to Firefox (I have
;; some Autokey shortcuts for focusing specific applications; ~s-1~ is
;; Emacs), but sometimes I just want to add a link at point.


;; [[file:Sacha.org::#spookfox-insert-url][Emacs and Spookfox: org-capture the current tab from Firefox or a link from the page:1]]
(defun my-spookfox-insert-url ()
	(interactive)
	(insert (spookfox-js-injection-eval-in-active-tab "window.location.href" t)))
(defun my-spookfox-insert-org-link ()
	(interactive)
	(insert (apply #'org-link-make-string
								 (append (spookfox-js-injection-eval-in-active-tab "[window.location.href, document.title]" t) nil))))
(with-eval-after-load 'org
	(cl-pushnew
	 `("f" "Firefox" entry
			(file ,my-org-inbox-file)
			"* %^{Note}\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n%(apply #'org-link-make-string
								 (append (spookfox-js-injection-eval-in-active-tab \"[window.location.href, document.title]\" t) nil))")
	 org-capture-templates)
	(cl-pushnew
	 `("F" "Firefox link" entry
			(file ,my-org-inbox-file)
			"* %^{Note}\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n%(org-link-make-string
(my-spookfox-complete-link))")
	 org-capture-templates))
;; Emacs and Spookfox: org-capture the current tab from Firefox or a link from the page:1 ends here

;; Quantified Awesome
;; :PROPERTIES:
;; :CUSTOM_ID: clock-in
;; :END:
;; <<clock-in>>


;; [[file:Sacha.org::#clock-in][Quantified Awesome:1]]
(defmacro my-org-with-current-task (&rest body)
  "Execute BODY with the point at the subtree of the current task."
  (declare (debug t))
  `(if (derived-mode-p 'org-agenda-mode)
       (save-window-excursion
         (org-agenda-switch-to)
         ,@body)
     ,@body))

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
(bind-key "!" 'my-org-clock-in-and-track org-agenda-mode-map)

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
       (setq category (read-string "Category: "))
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
(bind-key "C-c q" 'my-org-quick-clock-in-task)

(require 'quantified nil t)
;; Quantified Awesome:1 ends here

;; Child time!


;; [[file:Sacha.org::*Child time!][Child time!:1]]
(defun my-childcare ()
	(interactive)
	(when (org-clocking-p)
		(org-clock-out))
	(quantified-track "Childcare"))
;; Child time!:1 ends here

;; Make a tablist of my time entries
;; :PROPERTIES:
;; :CUSTOM_ID: quantified-tablist
;; :END:


;; [[file:Sacha.org::#quantified-tablist][Make a tablist of my time entries:1]]
(define-derived-mode my-quantified-list-mode tablist-mode "Time"
	"Major mode for time entries"
	(setq tabulated-list-format [("id" 5)
															 ("timestamp" 25)
															 ("duration" 5)
															 ("full_name" 60)
															 ("note" 20)])
	(tabulated-list-init-header)
	(tabulated-list-print t))

(defun my-quantified-list (start end filter)
	(interactive (list (org-read-date nil nil nil "Start: ") (org-read-date nil nil nil "End: ")
										 (read-string "Filter: ")))
	(switch-to-buffer (get-buffer-create "*quantified*"))
	(let ((json-array-type 'list)
				(json-object-type 'alist))
		(setq tabulated-list-entries
					(seq-keep
					 (lambda (o)
						 (let-alist o
							 (when (or (not filter) (string= filter "") (string-match filter .full_name)
												 (string-match filter (or .data.note "")))
								 (list
									.id
									(vector
									 (number-to-string .id)
									 (format-time-string "%a %b %d %l:%M%p" (parse-iso8601-time-string .timestamp))
									 (propertize (if .duration (format-seconds "%h:%.2m" .duration) "")
															 'duration .duration)
									 .full_name
									 (or .data.note ""))))))
					 (quantified-parse-json
											 (quantified-request
												(format
												 "/records.json?start=%s&end=%s&auth_token=%s"
												 (or start "")
												 (or end "")
												 (quantified-token))
												nil "GET")))))
	(my-quantified-list-mode))
(defun my-quantified-list-sum-marked-duration ()
	(interactive)
	(let ((seconds (apply '+
																	(mapcar
																	 (lambda (o)
																		 (get-text-property 0 'duration
																												(aref (cdr o) 2)))
																	 (tablist-get-marked-items)))))
		(message "%s (%.1f)"
						 (format-seconds "%d:%z%.2h:%.2m" seconds)
						 (/ seconds 3600.0))))
;; (my-quantified-list "2024-09-30" nil "E1")
;; Make a tablist of my time entries:1 ends here

;; Compare times and effort estimates
;; :PROPERTIES:
;; :CUSTOM_ID: compare-time
;; :END:
;; <<compare-time>>

;; This is for comparing times in column view and in tables.


;; [[file:Sacha.org::#compare-time][Compare times and effort estimates:1]]
(defun my-compare-times (clocked estimated)
  (if (and (> (length clocked) 0) estimated)
      (format "%.2f"
              (/ (* 1.0 (org-hh:mm-string-to-minutes clocked))
                 (org-hh:mm-string-to-minutes estimated)))
    ""))
;; Compare times and effort estimates:1 ends here

;; Using the calendar-date-echo-text variable to help plot a heatmap on a year-long calendar in Emacs :emacs:
;; :PROPERTIES:
;; :EXPORT_DATE: 2023-01-06T10:37:58-0500
;; :EXPORT_ELEVENTY_PERMALINK: /blog/2023/01/using-the-calendar-date-echo-text-variable-to-help-plot-a-heatmap-on-a-year-long-calendar-in-emacs/
;; :EXPORT_ELEVENTY_FILE_NAME: blog/2023/01/using-the-calendar-date-echo-text-variable-to-help-plot-a-heatmap-on-a-year-long-calendar-in-emacs/
;; :CUSTOM_ID: using-the-calendar-date-echo-text-variable-to-help-plot-a-heatmap-on-a-year-long-calendar-in-emacs
;; :END:

;; #+CAPTION: Sketch heatmap from 2008-2023
;; [[file:~/recordings/output-2023-01-06-10:26:49.gif]]

;; Building on [[https://sachachua.com/blog/2023/01/display-a-calendar-heat-map-using-emacs-lisp/][Display a calendar heat map using Emacs Lisp]],
;; I figured out how to use ~calendar-date-echo-text~ to store
;; the date so that I can pick it up when plotting the heatmap:


;; [[file:Sacha.org::#using-the-calendar-date-echo-text-variable-to-help-plot-a-heatmap-on-a-year-long-calendar-in-emacs][Using the calendar-date-echo-text variable to help plot a heatmap on a year-long calendar in Emacs:1]]
;; This seems to be the only way we can hack the date in for now
(setq calendar-date-echo-text '(apply #'format (list "%04d-%02d-%02d" year month day)))

(defun my-calendar-heat-map-using-echo-text (&rest _)
  (when my-calendar-count-scaled
		(save-excursion
			(goto-char (point-min))
			(while (not (eobp))
				(let* ((help (get-text-property (point) 'help-echo))
							 (next-change
								(or (next-single-property-change (point) 'help-echo)
										(point-max)))
							 (inhibit-read-only t)
							 (count-scaled (and help
																	(assoc-default
																	 help
																	 my-calendar-count-scaled))))
					(when (and help
										 (string-match "[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]" help)
										 count-scaled)
						(put-text-property
						 (point) (+ 2 (point))
						 'face (intern (format "calendar-scale-%d" count-scaled))))
					(goto-char next-change))))))

(advice-add #'calendar :after #'my-calendar-heat-map-using-echo-text)
(advice-add #'calendar-redraw :after #'my-calendar-heat-map-using-echo-text)
(advice-add #'year-calendar :after #'my-calendar-heat-map-using-echo-text)
;; Using the calendar-date-echo-text variable to help plot a heatmap on a year-long calendar in Emacs:1 ends here



;; So now I don't need the advice around ~calendar-generate-month~, just
;; the code that sets up the faces, loads the values, and figures out the
;; data.

;; #+begin_my_details Previous source code (tweaked foreground colours)

;; [[file:Sacha.org::#using-the-calendar-date-echo-text-variable-to-help-plot-a-heatmap-on-a-year-long-calendar-in-emacs][Using the calendar-date-echo-text variable to help plot a heatmap on a year-long calendar in Emacs:2]]
(defface calendar-scale-1  '((((background light)) :foreground "black" :background "#eceff1")
                             (((background dark))  :foreground "white" :background "#263238")) "")
(defface calendar-scale-2  '((((background light)) :foreground "black" :background "#cfd8dc")
                             (((background dark))  :foreground "white" :background "#37474f")) "")
(defface calendar-scale-3  '((((background light)) :foreground "black" :background "#b0bec5")
                             (((background dark))  :foreground "white" :background "#455a64")) "")
(defface calendar-scale-4  '((((background light)) :foreground "black" :background "#90a4ae")
                             (((background dark))  :foreground "white" :background "#546e7a")) "")
(defface calendar-scale-5  '((((background light)) :foreground "black" :background "#78909c")
                             (((background dark))  :foreground "white" :background "#607d8b")) "")
(defface calendar-scale-6  '((((background light)) :foreground "white" :background "#607d8b")
                             (((background dark))  :foreground "black" :background "#78909c")) "")
(defface calendar-scale-7  '((((background light)) :foreground "white" :background "#546e7a")
                             (((background dark))  :foreground "black" :background "#90a4ae")) "")
(defface calendar-scale-8  '((((background light)) :foreground "white" :background "#455a64")
                             (((background dark))  :foreground "black" :background "#b0bec5")) "")
(defface calendar-scale-9  '((((background light)) :foreground "white" :background "#37474f")
                             (((background dark))  :foreground "black" :background "#cfd8dc")) "")
(defun my-count-calendar-entries (grouped-entries)
  (mapcar (lambda (entry) (cons (car entry) (length (cdr entry)))) grouped-entries))

(defface calendar-scale-10 '((((background light)) :foreground "white" :background "#263238")
                             (((background dark))  :foreground "black" :background "#eceff1")) "")

(defun my-scale-calendar-entries (grouped-entries &optional scale-max)
  (let* ((count (my-count-calendar-entries grouped-entries))
         (count-max (apply #'max (mapcar (lambda (o) (if (car o) (cdr o) 0)) count))))
    (mapcar (lambda (entry)
              (cons (car entry)
                    (/ (* 1.0 (or scale-max 1.0) (cdr entry)) count-max)))
            count)))

(defun my-scale-calendar-entries-logarithmically (grouped-entries &optional scale-max)
  (let* ((count (my-count-calendar-entries grouped-entries))
         (count-max (apply #'max (mapcar (lambda (o) (if (car o) (cdr o) 0)) count))))
    (mapcar (lambda (entry)
              (cons (car entry)
                    (/ (* 1.0 (or scale-max 1.0) (log (cdr entry))) (log count-max))))
            count)))

(defvar my-calendar-count-scaled nil "Values to display.")
;; Using the calendar-date-echo-text variable to help plot a heatmap on a year-long calendar in Emacs:2 ends here


;; #+end_my_details

;; Now I can have it display the last year of data or so.


;; [[file:Sacha.org::#using-the-calendar-date-echo-text-variable-to-help-plot-a-heatmap-on-a-year-long-calendar-in-emacs][Using the calendar-date-echo-text variable to help plot a heatmap on a year-long calendar in Emacs:3]]
(defun my-calendar-visualize (values)
  (setq my-calendar-count-scaled values)
	(let* ((date (calendar-current-date))
				 (month (calendar-extract-month date))
				 (year (calendar-extract-year date)))
		(year-calendar month (1- year))))
;; Using the calendar-date-echo-text variable to help plot a heatmap on a year-long calendar in Emacs:3 ends here



;; The code to load the data stays the same.

;; #+begin_my_details Loading the data

;; [[file:Sacha.org::#using-the-calendar-date-echo-text-variable-to-help-plot-a-heatmap-on-a-year-long-calendar-in-emacs][Using the calendar-date-echo-text variable to help plot a heatmap on a year-long calendar in Emacs:4]]
(defun my-calendar-visualize-journal-entries ()
  (interactive)
  (my-calendar-visualize
   (mapcar
    (lambda (o)
      (cons
       (car o)
       (ceiling (+ 1 (* 7.0 (cdr o))))))
    (my-scale-calendar-entries
     (seq-group-by #'my-journal-date
                   (cdr (pcsv-parse-file "~/Downloads/entries.csv")))))))

(defun my-calendar-visualize-sketches ()
  (interactive)
  (let ((my-calendar-sketches
         (assoc-delete-all
          nil
          (seq-group-by
           (lambda (o)
             (when (string-match "^\\([0-9][0-9][0-9][0-9]\\)[-_]?\\([0-9][0-9]\\)[-_]?\\([0-9][0-9]\\)" o)
               (format "%s-%s-%s"
                       (match-string 1 o)
                       (match-string 2 o)
                       (match-string 3 o))))
           (append
            (directory-files "~/sync/sketches" nil "\\.\\(png\\|jpg\\)\\'")
            (directory-files "~/sync/private-sketches" nil "\\.\\(png\\|jpg\\)\\'"))))))
    (my-calendar-visualize
     (mapcar
      (lambda (o)
        (cons (car o)
              ;; many days have just 1 sketch, so I set the low end of the scale
              ;; to make them visible, and use a logarithmic scale for the rest
              (ceiling (+ 3 (* 7.0 (cdr o))))))
      (my-scale-calendar-entries-logarithmically my-calendar-sketches)))))

(defun my-calendar-visualize-tantrums ()
  (interactive)
  (my-calendar-visualize
   (mapcar
    (lambda (o)
      (cons
       (car o)
       (ceiling (* 10.0 (cdr o)))))
    (my-scale-calendar-entries
     (seq-group-by #'my-journal-date
                   (seq-filter (lambda (o) (string-match "tantrum\\|grump\\|angry\\|meltdown"
                                                           (my-journal-note o)))
                               (cdr (pcsv-parse-file "~/Downloads/entries.csv"))))))))
;; Using the calendar-date-echo-text variable to help plot a heatmap on a year-long calendar in Emacs:4 ends here


;; #+end_my_details

;; Here's the code from lawlist's StackOverflow answer that [[https://stackoverflow.com/questions/9547912/emacs-calendar-show-more-than-3-months][displays the Emacs calendar for a year]]:

;; #+begin_my_details Source code for showing an Emacs calendar year

;; [[file:Sacha.org::#using-the-calendar-date-echo-text-variable-to-help-plot-a-heatmap-on-a-year-long-calendar-in-emacs][Using the calendar-date-echo-text variable to help plot a heatmap on a year-long calendar in Emacs:5]]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                            ;;;
;;; Scroll a yearly calendar by month -- in a forwards or backwards direction. ;;;
;;;                                                                            ;;;
;;; To try out this example, evaluate the entire code snippet and type:        ;;;
;;;                                                                            ;;;
;;;     M-x year-calendar                                                      ;;;
;;;                                                                            ;;;
;;; To scroll forward by month, type the key:  >                               ;;;
;;;                                                                            ;;;
;;; To scroll backward by month, type the key:  <                              ;;;
;;;                                                                            ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-after-load "calendar" '(progn
  (define-key calendar-mode-map "<" 'lawlist-scroll-year-calendar-backward)
  (define-key calendar-mode-map ">" 'lawlist-scroll-year-calendar-forward) ))

(defmacro lawlist-calendar-for-loop (var from init to final do &rest body)
  "Execute a for loop.
Evaluate BODY with VAR bound to successive integers from INIT to FINAL,
inclusive.  The standard macro `dotimes' is preferable in most cases."
  `(let ((,var (1- ,init)))
    (while (>= ,final (setq ,var (1+ ,var)))
      ,@body)))

(defun year-calendar (&optional month year)
  "Generate a one (1) year calendar that can be scrolled by month in each direction.
This is a modification of:  http://homepage3.nifty.com/oatu/emacs/calendar.html
See also:  http://ivan.kanis.fr/caly.el"
	(interactive)
  (require 'calendar)
  (let* ((current-year (number-to-string (nth 5 (decode-time (current-time)))))
         (month (if month month
           (string-to-number
             (read-string "Please enter a month number (e.g., 1):  " nil nil "1"))))
         (year (if year year
           (string-to-number
             (read-string "Please enter a year (e.g., 2014):  "
               nil nil current-year)))))
    (switch-to-buffer (get-buffer-create calendar-buffer))
    (when (not (eq major-mode 'calendar-mode))
      (calendar-mode))
    (setq displayed-month month)
    (setq displayed-year year)
    (setq buffer-read-only nil)
    (erase-buffer)
    ;; horizontal rows
    (lawlist-calendar-for-loop j from 0 to 3 do
      ;; vertical columns
      (lawlist-calendar-for-loop i from 0 to 2 do
        (calendar-generate-month
          ;; month
          (cond
            ((> (+ (* j 3) i month) 12)
              (- (+ (* j 3) i month) 12))
            (t
              (+ (* j 3) i month)))
          ;; year
          (cond
            ((> (+ (* j 3) i month) 12)
             (+ year 1))
            (t
              year))
          ;; indentation / spacing between months
          (+ 5 (* 25 i))))
      (goto-char (point-max))
      (insert (make-string (- 10 (count-lines (point-min) (point-max))) ?\n))
      (widen)
      (goto-char (point-max))
      (narrow-to-region (point-max) (point-max)))
    (widen)
    (goto-char (point-min))
    (setq buffer-read-only t)))

(defun lawlist-scroll-year-calendar-forward (&optional arg event)
  "Scroll the yearly calendar by month in a forward direction."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     last-nonmenu-event))
  (unless arg (setq arg 1))
  (save-selected-window
    (if (setq event (event-start event)) (select-window (posn-window event)))
    (unless (zerop arg)
      (let ((month displayed-month)
            (year displayed-year))
        (calendar-increment-month month year arg)
        (year-calendar month year)))
    (goto-char (point-min))
    (run-hooks 'calendar-move-hook)))

(defun lawlist-scroll-year-calendar-backward (&optional arg event)
  "Scroll the yearly calendar by month in a backward direction."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     last-nonmenu-event))
  (lawlist-scroll-year-calendar-forward (- (or arg 1)) event))
;; Using the calendar-date-echo-text variable to help plot a heatmap on a year-long calendar in Emacs:5 ends here


;; #+end_my_details

;; It might be fun to scroll by year:


;; [[file:Sacha.org::#using-the-calendar-date-echo-text-variable-to-help-plot-a-heatmap-on-a-year-long-calendar-in-emacs][Using the calendar-date-echo-text variable to help plot a heatmap on a year-long calendar in Emacs:6]]
(defun my-scroll-year-calendar-forward-year (&optional arg event)
  "Scroll the yearly calendar by year in a forward direction."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     last-nonmenu-event))
  (unless arg (setq arg 1))
  (save-selected-window
    (if (setq event (event-start event)) (select-window (posn-window event)))
    (unless (zerop arg)
      (setq displayed-year (+ (or arg 1) displayed-year))
      (year-calendar displayed-month displayed-year))
    (goto-char (point-min))
    (run-hooks 'calendar-move-hook)))

(defun my-scroll-year-calendar-backward-year (&optional arg event)
  "Scroll the yearly calendar by month in a backward direction."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     last-nonmenu-event))
  (my-scroll-year-calendar-forward-year (- (or arg 1)) event))
(eval-after-load "calendar" '(progn
  (define-key calendar-mode-map "{" 'my-scroll-year-calendar-backward-year)
  (define-key calendar-mode-map "}" 'my-scroll-year-calendar-forward-year)))
;; Using the calendar-date-echo-text variable to help plot a heatmap on a year-long calendar in Emacs:6 ends here

;; Workrave
;; :PROPERTIES:
;; :CUSTOM_ID: workrave
;; :END:

;; [[file:Sacha.org::#workrave][Workrave:1]]
(defvar my-workrave-file (expand-file-name ".\\Workrave\\historystats" (getenv "AppData")))

(defun my-workrave-transform-statistics (&optional file)
  (interactive (list my-workrave-file))
  (with-current-buffer (find-file-noselect file)
    ;; D day month-1 year hour min day month-1 year hour min
    (let ((result "Date\tStart\tEnd\tClicks\tKeystrokes\n"))
      (goto-char (point-min))
      (while (re-search-forward "^D \\(.*\\)" nil t)
        (let ((dates (split-string (match-string 1))))
          (if (re-search-forward "^m \\(.*\\)" nil t)
              (let ((info (split-string (match-string 1))))
                (setq result
                      (concat result
                              (format "%d-%d-%s\t%s:%02d\t%s:%02d\t%s\t%s\n"
                                      (+ 1900 (string-to-number (elt dates 2))) ; year
                                      (1+ (string-to-number (elt dates 1))) ; month
                                      (elt dates 0) ; day
                                      (elt dates 3) ; start hour
                                      (string-to-number (elt dates 4)) ; start min
                                      (elt dates 8) ; end hour
                                      (string-to-number (elt dates 9)) ; end min
                                      (elt info 5) ; clicks
                                      (elt info 6) ; keystrokes
                                      )))))))
      (if (interactive-p)
          (kill-new result)
        result))))
;; Workrave:1 ends here

;; Blog
;; :PROPERTIES:
;; :CUSTOM_ID: blog
;; :END:

;; [[file:Sacha.org::#blog][Blog:1]]
(defun my-strip-blog-share ()
  (interactive)
  (let (base)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              "<div class=\"sharedaddy sd-sharing-enabled\">.*?<div class=\"sharing-clear\"></div></div></div></div>" nil t)
        (replace-match "")))))
;; Blog:1 ends here

;; Time tracking, previous weekly review
;; :PROPERTIES:
;; :CUSTOM_ID: time-tracking-previous-weekly-review
;; :END:

;; [[file:Sacha.org::#time-tracking-previous-weekly-review][Time tracking, previous weekly review:1]]
(defvar my-org-quantified-categories
  '(("Business"
     ("Earn" . "Business - Earn")
     ("E1" . "Business - Earn - Consulting - E1")
     ("Connect" . "Business - Connect")
     ("Build" . "Business - Build"))
    ("Discretionary"
     ("Social" . "Discretionary - Social")
     ("Productive" . "Discretionary - Productive")
     ("Sewing" . "Discretionary - Productive - Sewing")
     ("Writing" . "Discretionary - Productive - Writing")
     ("Emacs" . "Discretionary - Productive - Emacs")
     ("Play" . "Discretionary - Play"))
    ("Personal" ;("Biking" . "Personal - Bike")
     ("Routines" . "Personal - Routines"))
    ("Sleep" nil)
    ("Unpaid work"
     ("Commuting" . "Unpaid work - Subway")
     ("Cook" . "Unpaid work - Cook")
     ("Tidy" . "Unpaid work - Tidy up")))
  "Categories for time summary.")

(defun my-org-summarize-time-use (&optional start end)
  (interactive (list (org-read-date) (org-read-date)))
  (let ((time-summary (quantified-summarize-time start end))
        (categories my-org-quantified-categories)
        result)
    (setq result
          (mapconcat
           (lambda (a)
             (if (assoc (car a) time-summary)
                 (concat
                  (format "- %s: %.1f hours" (car a) (/ (cdr (assoc (car a) time-summary)) 3600.0))
                  (if (cdr a)
                      (let ((detail
                             (delq nil
                                   (mapcar (lambda (b)
                                             (if (assoc (cdr b) time-summary)
                                                 (format "%s: %.1f"
                                                         (car b)
                                                         (/ (cdr (assoc (cdr b) time-summary)) 3600.0))
                                               nil))
                                           (cdr a)))))
                        (if detail
                            (concat " (" (mapconcat 'identity detail ", ") ")")
                          ""))
                    "")
                  (if (string-equal (car a) "Sleep")
                      (format " - average of %.1f hours per day" (/ (cdr (assoc (car a) time-summary)) 3600.0 7.0))
                    "")
                  "\n")))
           categories ""))
    (if (called-interactively-p 'any)
        (insert result)
      result)))
;; Time tracking, previous weekly review:1 ends here

;; List upcoming tasks so that I can see if I'm overloaded
;; :PROPERTIES:
;; :CUSTOM_ID: list-upcoming-tasks-so-that-i-can-see-if-i-m-overloaded
;; :END:


;; [[file:Sacha.org::#list-upcoming-tasks-so-that-i-can-see-if-i-m-overloaded][List upcoming tasks so that I can see if I'm overloaded:1]]
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
;; List upcoming tasks so that I can see if I'm overloaded:1 ends here



;; This uses Org Agenda's log mode to summarize the tasks that I checked
;; off. I still need to match it up with the plans for the previous week
;; to see which items I'd planned ahead, and which ones were new tasks.
;; (Hmm, is it important to track those separately? I might just skip it.)


;; [[file:Sacha.org::#list-upcoming-tasks-so-that-i-can-see-if-i-m-overloaded][List upcoming tasks so that I can see if I'm overloaded:2]]
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
;; List upcoming tasks so that I can see if I'm overloaded:2 ends here

;; Compare time use
;; :PROPERTIES:
;; :CUSTOM_ID: compare-time-use
;; :END:

;; [[file:Sacha.org::#compare-time-use][Compare time use:1]]
(defun my-quantified-compare (start1 end1 start2 end2 &optional categories label1 label2)
  "Return a table comparing the times for START1 - END1 and START2 - END2."
  (let* ((start2 (org-read-date nil nil (or start2 "-sat")))
         (end2 (org-read-date nil nil (or end2 "+1")))
         (start1 (org-read-date nil nil (or start1 "-4sat")))
         (end1 (org-read-date nil nil (or end1 "-sat")))
         (time2 (quantified-summarize-time start2 end2))
         (time1 (quantified-summarize-time start1 end1))
         (label1 (or label1 "Period 1 %"))
         (label2 (or label2 "Period 2 %"))
         (total2 (* 0.01 (- (org-time-string-to-seconds end2) (org-time-string-to-seconds start2))))
         (total1 (* 0.01 (- (org-time-string-to-seconds end1) (org-time-string-to-seconds start1))))
         (keys (or categories (-union (mapcar 'car time1) (mapcar 'car time2)))))
    ;; Build a list comparing the two
    (append
     `(("Category" ,label1 ,label2 "Diff %" "h/wk" "Diff h/wk") hline)
     (sort
      (mapcar (lambda (key)
                (list
                 key
                 (format "%.1f" (/ (or (assoc-default key time1) 0) total1))
                 (format "%.1f" (/ (or (assoc-default key time2) 0) total2))
                 (format "%.1f" (- (/ (or (assoc-default key time2) 0) total2)
                                   (/ (or (assoc-default key time1) 0) total1)))
                 (format "%.1f" (* (/ (or (assoc-default key time2) 0) total1) 1.68))
                 (format "%.1f"
                         (* (- (/ (or (assoc-default key time2) 0) total2)
                               (/ (or (assoc-default key time1) 0) total1)) 1.68))
                 )) keys)
      (lambda (a b)
        (<
         (string-to-number (car (last b)))
         (string-to-number (car (last a)))))))))
;; Compare time use:1 ends here

;; Emacs and my phone
;; :PROPERTIES:
;; :CUSTOM_ID: on-my-phone
;; :END:

;; I use Orgzly Revived on an Android phone, synchronizing my files with
;; Syncthing. (See =my-resolve-orgzly-syncthing= elsewhere in this
;; config.) Sometimes I use Termux, too.


;; [[file:Sacha.org::#on-my-phone][Emacs and my phone:1]]
(setq browse-url-browser-function 'browse-url-firefox)
(unless window-system
  (xterm-mouse-mode 1)
  (global-set-key [mouse-4] (lambda ()
                              (interactive)
                              (scroll-down 1)))
  (global-set-key [mouse-5] (lambda ()
                              (interactive)
                              (scroll-up 1))))
(use-package org
  :config
  (when my-phone-p
    (add-to-list 'org-file-apps '("\\.png\\'" . default))
    (add-to-list 'org-file-apps '("\\.jpg\\'" . default))
    (add-to-list 'org-file-apps '("\\.jpeg\\'" . default)))
  )

(defun my-format-intent (intent &optional params)
  "Return a command string for sending INTENT with PARAMS.
      PARAMS is an alist of (\"key\" . \"value\") pairs."
  (format "am broadcast --user 0 -a %s %s"
          intent
          (mapconcat
           (lambda (o)
             (format
              "-e %s %s"
              (shell-quote-argument (car o))
              (shell-quote-argument (cdr o))))
           params
           " ")))

(defun my-send-intent (intent &optional params)
  "Send broadcast INTENT to my phone.
      PARAMS is a plist of :key value pairs."
  (let ((command (my-format-intent intent params)))
    (if my-phone-p
        (shell-command command)
      (shell-command (format "ssh phone %s" (shell-quote-argument command))))))
;; Emacs and my phone:1 ends here

;; Syncthing
;; :PROPERTIES:
;; :CUSTOM_ID: syncthing
;; :END:

;; From https://www.reddit.com/r/emacs/comments/bqqqra/quickly_find_syncthing_conflicts_and_resolve_them/
;; In termux, you also need to =pkg install diffutils=.


;; [[file:Sacha.org::#syncthing][Syncthing:1]]
(setq ediff-toggle-skip-similar t
      ediff-diff-options "-w"
      ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-split-window-function 'split-window-horizontally)
(defun my-resolve-orgzly-syncthing ()
  (interactive)
  (ibizaman/syncthing-resolve-conflicts "~/sync/orgzly"))

(defun ibizaman/syncthing-resolve-conflicts (directory)
  "Resolve all conflicts under given DIRECTORY."
  (interactive "D")
  (let* ((all (ibizaman/syncthing--get-sync-conflicts directory))
         (chosen (ibizaman/syncthing--pick-a-conflict all)))
    (ibizaman/syncthing-resolve-conflict chosen)))


(defun ibizaman/syncthing-show-conflicts-dired (directory)
  "Open dired buffer at DIRECTORY showing all syncthing conflicts."
  (interactive "D")
  (find-name-dired directory "*.sync-conflict-*org"))

(defun ibizaman/syncthing-resolve-conflict-dired (&optional arg)
  "Resolve conflict of first marked file in dired or close to point with ARG."
  (interactive "P")
  (let ((chosen (car (dired-get-marked-files nil arg))))
    (ibizaman/syncthing-resolve-conflict chosen)))

(defun ibizaman/syncthing-resolve-conflict (conflict)
  "Resolve CONFLICT file using ediff."
  (let* ((normal (ibizaman/syncthing--get-normal-filename conflict)))
    (ibizaman/ediff-files
     (list conflict normal)
     `(lambda ()
        (when (y-or-n-p "Delete conflict file? ")
          (kill-buffer (get-file-buffer ,conflict))
          (delete-file ,conflict))))))



(defun ibizaman/syncthing--get-sync-conflicts (directory)
  "Return a list of all sync conflict files in a DIRECTORY."
  (seq-filter (lambda (o) (not (string-match "\\.stversions" o))) (directory-files-recursively directory "\\.sync-conflict-.*org$")))


(defvar ibizaman/syncthing--conflict-history nil
  "Completion conflict history")

(defun ibizaman/syncthing--pick-a-conflict (conflicts)
  "Let user choose the next conflict from CONFLICTS to investigate."
  (completing-read "Choose the conflict to investigate: " conflicts
                   nil t nil ibizaman/syncthing--conflict-history))


(defun ibizaman/syncthing--get-normal-filename (conflict)
  "Get non-conflict filename matching the given CONFLICT."
  (replace-regexp-in-string "\\.sync-conflict-.*\\(\\..*\\)$" "\\1" conflict))


(defun ibizaman/ediff-files (&optional files quit-hook)
  (interactive)
  (lexical-let ((files (or files (dired-get-marked-files)))
                (quit-hook quit-hook)
                (wnd (current-window-configuration)))
    (if (<= (length files) 2)
        (let ((file1 (car files))
              (file2 (if (cdr files)
                         (cadr files)
                       (read-file-name
                        "file: "
                        (dired-dwim-target-directory)))))
          (if (file-newer-than-file-p file1 file2)
              (ediff-files file2 file1)
            (ediff-files file1 file2))
          (add-hook 'ediff-after-quit-hook-internal
                    (lambda ()
                      (setq ediff-after-quit-hook-internal nil)
                      (when quit-hook (funcall quit-hook))
                      (set-window-configuration wnd))))
      (error "no more than 2 files should be marked"))))
;; Syncthing:1 ends here

;; Clipboard
;; :PROPERTIES:
;; :CUSTOM_ID: clipboard
;; :END:


;; [[file:Sacha.org::#clipboard][Clipboard:1]]
(use-package clipmon
  :disabled t
  :init (progn (setq clipmon-action 'kill-new clipmon-timeout nil clipmon-sound nil clipmon-cursor-color nil clipmon-suffix nil) (clipmon-mode)))
;; Clipboard:1 ends here



;; On my phone:


;; [[file:Sacha.org::#clipboard][Clipboard:2]]
(use-package xclip :if my-phone-p) ; Turn on with xclip-mode
;; Clipboard:2 ends here

;; TOBLOG Send mail asynchronously
;; :PROPERTIES:
;; :CREATED: [2024-10-15 Tue 11:01]
;; :Effort:   0:15
;; :QUANTIFIED: Emacs
;; :CUSTOM_ID: async-smtpmail
;; :END:
;; :LOGBOOK:
;; - State "DONE"       from "STARTED"    [2024-10-15 Tue 11:17]
;; CLOCK: [2024-10-15 Tue 11:01]--[2024-10-15 Tue 11:17] =>  0:16
;; :END:

;; Based on [[https://github.com/jwiegley/emacs-async/blob/master/smtpmail-async.el][smtpmail-async]], but with the list of variables tweaked because ~mail-extr-all-top-level-domains~ was an ~#<obarray n=344>~ that couldn't get passed.

;; [[file:Sacha.org::#async-smtpmail][TOBLOG Send mail asynchronously:1]]
(defun my-async-smtpmail-send-it ()
  (let ((to          (message-field-value "To"))
        (buf-content (buffer-substring-no-properties
                      (point-min) (point-max))))
    (message "Delivering message to %s..." to)
    (async-start
     `(lambda ()
        (require 'smtpmail)
        (with-temp-buffer
          (insert ,buf-content)
          (set-buffer-multibyte nil)
          ;; Pass in the variable environment for smtpmail
          ,(async-inject-variables
            "\\`\\(smtpmail\\|async-smtpmail\\|user-mail\\)-\\|auth-sources\\|epg\\|nsm"
            nil "\\`\\(mail-header-format-function\\|smtpmail-address-buffer\\|mail-mode-abbrev-table\\)")
          (smtpmail-send-it)))
     `(lambda (&optional _ignore)
				(message "Delivering message to %s...done" ,to)))))
(setq send-mail-function 'my-async-smtpmail-send-it
      message-send-mail-function 'my-async-smtpmail-send-it)
;; TOBLOG Send mail asynchronously:1 ends here

;; Notmuch
;; :PROPERTIES:
;; :CUSTOM_ID: notmuch
;; :END:

;; I use Notmuch with [[https://github.com/gauteh/lieer][Lieer]] to fetch my mail from Gmail.


;; [[file:Sacha.org::#notmuch][Notmuch:1]]
(setq notmuch-message-headers '("Subject" "To" "Cc" "Date" "Reply-To"))
(use-package notmuch
  :if my-laptop-p
  :config (setq-default notmuch-search-oldest-first nil)
  (setq notmuch-fcc-dirs nil)
  (setq notmuch-archive-tags '("-inbox" "-flagged" "-unread" "-new")))
(use-package ol-notmuch
  :if my-laptop-p)
(defun my-notmuch-flagged ()
  (interactive)
  (notmuch-search "tag:flagged and not tag:trash"))
(defun my-notmuch-inbox ()
  (interactive)
  (notmuch-search "tag:inbox and not tag:trash"))
(defun my-notmuch-important-inbox ()
  (interactive)
  (notmuch-search "tag:primary and tag:inbox and not tag:trash"))
(defun my-notmuch-search-this-author ()
  (interactive)
  (notmuch-search (format "from:\"%s\""
                          (plist-get (get-text-property (point) 'notmuch-search-result) :authors))))
;; Notmuch:1 ends here

;; Act on current message with Embark  :embark:
;; :PROPERTIES:
;; :CUSTOM_ID: act-on-current-message-with-embark
;; :END:


;; [[file:Sacha.org::#act-on-current-message-with-embark][Act on current message with Embark:1]]
(defun mail-embark-finder ()
	"Identify when we're in a notmuch message."
	(cond ((derived-mode-p 'notmuch-show-mode)
				 `(mail . ,(plist-get (plist-get (notmuch-show-get-message-properties) :headers) :From)))))
(with-eval-after-load 'embark
	(add-to-list 'embark-target-finders 'mail-embark-finder)
	)
;; Act on current message with Embark:1 ends here

;; Gnus
;; :PROPERTIES:
;; :ID:       o2b:c696259a-146e-4f47-8828-e7ca45cc2215
;; :POST_DATE: [2015-11-20 Fri 12:36]
;; :POSTID:   28485
;; :BLOG:     sacha
;; :CUSTOM_ID: gnus
;; :END:


;; I still use Gnus so that I can use [[http://gmane.org][Gmane]] to read mailing lists.

;;     I used to have my config in in =~/.gnus=, but people might find it
;;     handy, so I've added it to my public [[http://sachacuha.com/dotemacs][Emacs configuration]].


;; [[file:Sacha.org::#gnus][Gnus:1]]
(setq gnus-select-method '(nnnil ""))
(setq gnus-secondary-select-methods
      '((nntp "news.gmane.io")
        ;; (nnmaildir "mail"
        ;;            (directory "~/Maildir/account.gmail")
        ;;            (directory-files nnheader-directory-files-safe)
        ;;           (get-new-mail nil))
        ;; (nnimap "imap.googlemail.com"
        ;;         (nnimap-address "imap.googlemail.com")
        ;;         (nnimap-server-port 993)
        ;;         (nnimap-stream ssl)
        ;; (nnimap-authenticator login))
        (nnimap "localhost"
          (nnimap-address "localhost")
          (nnimap-stream network)
          (nnimap-user "sacha")
          (nnimap-authenticator login)
          (nnimap-authinfo-file "~/.authinfo.gpg"))
        ))
(setq smtpmail-smtp-server "smtp.googlemail.com"
      smtpmail-smtp-service 587
      smtpmail-auth-credentials "~/.authinfo.gpg"
      send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
      gnus-check-new-newsgroups nil
      gnus-activate-level 2
      gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")
;; Gnus:1 ends here



;; I now use Dovecot with OfflineIMAP for local IMAP access to my mail
;; and synchronization with Gmail, but you can see the commented-out
;; information for Gmail in case you prefer that. I have two-factor
;; authentication enabled for Gmail, so I set up an app-specific password
;; for Gnus. I have GPG set up for encryption, and an =~/.authinfo.gpg=
;; file set up with something like:

;; #+begin_example
;;   machine imap.gmail.com login sacha@sachachua.com password mysecretapppassword
;;   machine imap.gmail.com login sacha@sachachua.com password mysecretapppassword port 993
;;   machine smtp.gmail.com login sacha@sachachua.com password mysecretapppassword port 587
;;   machine localhost login sacha password mysecretlocalpassword port 993
;;   machine localhost login sacha password mysecretlocalpassword port 143
;; #+end_example

;; If you don't have GPG set up and you don't mind saving your passwords
;; in the clear, you can set up an =~/.authinfo= file instead.


;; [[file:Sacha.org::#gnus][Gnus:2]]
(use-package gnus
  :config
  (require 'mm-decode)
  (setq mm-discouraged-alternatives
        '("text/html" "text/richtext")
        mm-automatic-display
        (-difference mm-automatic-display '("text/html" "text/enriched" "text/richtext"))))
;; Gnus:2 ends here



;; Hide quoted text.


;; [[file:Sacha.org::#gnus][Gnus:3]]
(setq gnus-treat-hide-citation t)
;; Gnus:3 ends here



;; Get smarter about filtering depending on what I reed or mark. I use =!= (tick) for marking threads as something that interests me.


;; [[file:Sacha.org::#gnus][Gnus:4]]
(setq gnus-use-adaptive-scoring t)
(setq gnus-default-adaptive-score-alist
      '((gnus-unread-mark)
        (gnus-ticked-mark (subject 10))
        (gnus-killed-mark (subject -5))
        (gnus-catchup-mark (subject -1))))
;; Gnus:4 ends here

;; Approve or discard Mailman messages
;; :PROPERTIES:
;; :CUSTOM_ID: approve-or-discard-mailman-messages
;; :END:

;; The mailing lists for [[https://lists.gnu.org/mailman/listinfo/emacsconf-org][emacsconf-org]], [[https://lists.gnu.org/mailman/listinfo/emacsconf-org-private][emacsconf-org-private]],
;; [[https://lists.gnu.org/mailman/listinfo/emacsconf-submit][emacsconf-submit]], and [[https://lists.gnu.org/mailman/listinfo/emacs-tangents][emacs-tangents]] are all handled by the Mailman
;; program. We usually set mailing lists to moderated so that


;; [[file:Sacha.org::#approve-or-discard-mailman-messages][Approve or discard Mailman messages:1]]
(defun my-mailman-approve ()
  "Approve this mailing list message."
  (interactive)
	(goto-char (point-min))
	(when (re-search-forward "From: \\(\\(.+\\)-request@.*?\\)\nSubject: \\(confirm [0-9a-f]+\\)" nil t)
		(let* ((id (match-string 2)))
			(compose-mail (match-string 1) (match-string 3)
										`(("Approved" . ,(string-trim (shell-command-to-string
																									 (concat "pass " (match-string 2)))))))
			(message-send-and-exit))))

(defun my-mailman-discard ()
	"Discard the current message."
	(interactive)
	(goto-char (point-min))
	(when (re-search-forward "From: \\(\\(.+\\)-request@.*?\\)\nSubject: \\(confirm [0-9a-f]+\\)" nil t)
		(compose-mail (match-string 1) (match-string 3))
		(message-send-and-exit)))

(defun my-mailman-web (&optional list-id)
	"Open the web admin interface."
	(interactive
	 (list
		(if (and (derived-mode-p 'notmuch-show-mode)
						 (re-search-forward "\\(https://.+?/mailman/admindb/\\(.+\\)\\)" nil t))
			 (match-string 2)
		 (completing-read "List: " '("emacsconf-org" "emacsconf-org-private" "emacs-tangents" "emacsconf-submit")))))
	(goto-char (point-min))
	(browse-url (concat "https://lists.gnu.org/mailman/admindb/" list-id "?adminpw="
												(url-hexify-string (string-trim (shell-command-to-string
																												 (concat "pass " list-id)))))))
;; Approve or discard Mailman messages:1 ends here

;; Emacs server
;; :PROPERTIES:
;; :CUSTOM_ID: emacs-server
;; :END:

;; =(server-start)= permits the use of =emacsclient=, =emacsclientw=, and
;; =org-protocol=. I used to start a server as part of my config. Now I'm
;; switching to using =emacs --daemon=, which starts a server
;; automatically. Anyway, with =--daemon=, Emacs doesn't start off in a
;; graphical environment, so the frames that =emacsclient -c= creates
;; don't get the theme applied. This fixes that:


;; [[file:Sacha.org::#emacs-server][Emacs server:1]]
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (select-frame frame)
            (my-setup-color-theme)))
;; Emacs server:1 ends here

;; Collaboration
;; :PROPERTIES:
;; :CUSTOM_ID: collaboration
;; :END:


;; [[file:Sacha.org::#collaboration][Collaboration:1]]
(use-package crdt
  :quelpa (crdt :fetcher github :repo "zaeph/crdt.el")
  :commands (crdt-share-buffer crdt-connect)
  :load-path "~/vendor/crdt.el"
  :if my-laptop-p)
;; Collaboration:1 ends here



;; #+RESULTS:
;; :results:
;; :end:

;; Some code to start and stop the stream:


;; [[file:Sacha.org::#simple-streaming][Simple streaming with FFmpeg:4]]
(defvar my-stream-process nil)
(defvar my-stream-type nil)
(defvar my-stream-offset-seconds 2 "Number of seconds to offset timestamps.")
(defvar my-stream-start-time nil)

(defun my-stream-toggle ()
	(interactive)
	(if (process-live-p my-stream-process)
			(my-stream-stop)
		(my-stream-start)))

(defun my-recording-toggle ()
	(interactive)
	(if (process-live-p my-stream-process)
			(my-recording-stop)
		(my-recording-start)))

(defun my-stream-start ()
	(interactive)
	(unless (process-live-p my-stream-process)
		(unless (getenv "YOUTUBE_KEY")
			(setenv "YOUTUBE_KEY" (auth-info-password (auth-source-search :host "https://studio.youtube.com"))))
		(setq my-stream-type 'stream)
		(setq my-stream-start-time (current-time))
		(setq my-stream-process (start-process "ffmpeg" (get-buffer-create "*stream-ffmpeg*")
																					 "bash" (expand-file-name "~/bin/stream-laptop")))
		(message "Streaming.")))

(defun my-recording-start ()
	(interactive)
	(unless (process-live-p my-stream-process)
		(setq my-stream-type 'record)
		(setq my-stream-start-time (current-time))
		(setq my-stream-process (start-process "ffmpeg" (get-buffer-create "*stream-ffmpeg*")
																					 "bash" (expand-file-name "~/bin/record-laptop")))
		(message "Recording.")))

(defun my-stream-stop ()
	(interactive)
	(when (process-live-p my-stream-process)
		(setq my-stream-type nil)
		(setq my-stream-start-time nil)
		(stop-process my-stream-process)
		(kill-process my-stream-process)))

(defalias 'my-recording-stop #'my-stream-stop)
(defun my-recordings-dired ()
	(interactive)
	(dired my-recordings-dir "-lt"))
;; Simple streaming with FFmpeg:4 ends here



;; Let's have relative timestamps:


;; [[file:Sacha.org::#simple-streaming][Simple streaming with FFmpeg:5]]
(defun my-stream-insert-timestamp ()
	(interactive)
	(when my-stream-start-time
		(let ((time (format-seconds "%.2h:%z%.2m:%.2s"
																(- (time-to-seconds (current-time))
																	 (time-to-seconds my-stream-start-time)
																	 (if (eq my-stream-type 'stream) my-stream-offset-seconds 0)))))
			(insert (org-link-make-string
							 (concat "video:" (my-latest-file "~/recordings" "flv")
											 ":" time)
							 time)
							" "))))

(defun my-stream-set-recording-file ()
	(interactive)
	(org-entry-put (point) "RECORDING"
								 (my-latest-file "~/recordings" "flv")))
;; Simple streaming with FFmpeg:5 ends here



;; Then I wrote this Emacs Lisp function to turn it on and off.


;; [[file:Sacha.org::#controlling-my-stream-audio-from-emacs-background-music-typing-sounds-and-push-to-talk][Controlling my stream audio from Emacs: background music, typing sounds, and push to talk:2]]
(defvar my-background-music-process nil "Process for playing background music")
(defun my-stream-toggle-background-music (&optional enable)
  (interactive)
  (if (or my-background-music-process
          (and (numberp enable) (< enable 0)))
      (progn
        (when (process-live-p my-background-music-process)
          (kill-process my-background-music-process))
        (setq my-background-music-process nil))
    (let ((files (directory-files "~/proj/music" t "mid\\'")))
      (setq my-background-music-process
            (apply
             'start-process
             "*Music*"
             nil
             (append (list "timidity" "-idlr" "--volume=10") files))))))
;; Controlling my stream audio from Emacs: background music, typing sounds, and push to talk:2 ends here



;; People also suggested typing sounds. I guess that's a good way to get
;; a sense of activity. The default selectric sound was a little too loud
;; for me, so we'll use the move sound for now. It would be nice to make
;; this more random-sounding someday.


;; [[file:Sacha.org::#controlling-my-stream-audio-from-emacs-background-music-typing-sounds-and-push-to-talk][Controlling my stream audio from Emacs: background music, typing sounds, and push to talk:3]]
(defun my-selectric-type-sound ()
  "Make the sound of typing."
  ;; Someday, randomize this or something
  (selectric-make-sound (expand-file-name "selectric-move.wav" selectric-files-path)))

(use-package selectric-mode
  :if my-laptop-p
  :diminish ""
  :config
  (fset #'selectric-type-sound #'my-selectric-type-sound))
;; Controlling my stream audio from Emacs: background music, typing sounds, and push to talk:3 ends here



;; #+begin_update
;; [2024-01-10]: I'm using the Blue Yeti microphone now, so I can use the
;; hardware mute button instead of push to talk.
;; #+end_update

;; I was having a hard time remembering to go back on mute during
;; meetings, since the LED on the mute button wasn't working at the time
;; and the system tray icon was a little hard to notice. The LED has
;; mysteriously decided to start working again, but push-to-talk is handy
;; anyway. I want to be able to tap a key to toggle my microphone on and
;; off, and hold it down in order to make it push-to-talk. It looks like
;; my key repeat is less than 0.5 seconds, so I can set a timer that will
;; turn things off after a little while. This code doesn't pick up any
;; changes that happen outside Emacs, but it'll do for now. I used =pacmd list-sources= to list the sources and get the IDs.


;; [[file:Sacha.org::#controlling-my-stream-audio-from-emacs-background-music-typing-sounds-and-push-to-talk][Controlling my stream audio from Emacs: background music, typing sounds, and push to talk:4]]
(defun my-pacmd-set-device (regexp status)
  (with-current-buffer (get-buffer-create "*pacmd*")
    (erase-buffer)
    (shell-command "pacmd list-sources" (current-buffer))
    (goto-char (point-max))
    (let (results)
      (while (re-search-backward regexp nil t)
        (when (re-search-backward "index: \\([[:digit:]]+\\)" nil t)
          (setq results (cons (match-string 1) results))
          (shell-command-to-string (format "pacmd set-source-mute %s %d"
                                           (match-string 1)
                                           (if (equal status 'on) 0 1)))))
      results)))

(defvar my-mic-p nil "Non-nil means microphone is on")
(add-to-list 'mode-line-front-space '(:eval (if my-mic-p "*MIC*" "")))

(defun my-mic-off ()
  (interactive)
  (my-pacmd-set-device "Yeti" 'off)
  (my-pacmd-set-device "Internal Microphone" 'off)
  (setq my-mic-p nil))
(defun my-mic-on ()
  (interactive)
  (my-pacmd-set-device "Yeti" 'on)
  (my-pacmd-set-device "Internal Microphone" 'on)
  (setq my-mic-p t))
(defun my-mic-toggle ()
  (interactive)
  (if my-mic-p (my-mic-off) (my-mic-on)))

(defvar my-push-to-talk-mute-timer nil "Timer to mute things again.")
(defvar my-push-to-talk-last-time nil "Last time my-push-to-talk was run")
(defvar my-push-to-talk-threshold 0.5 "Number of seconds")

(defun my-push-to-talk-mute ()
  (interactive)
  (message "Muting.")
  (my-mic-off)
  (force-mode-line-update)
  (when obs-websocket-recording-p (my-obs-websocket-add-caption "[Microphone off]")))

(defun my-push-to-talk ()
  "Tap to toggle microphone on and off, or repeat the command to make it push to talk."
  (interactive)
  (cond
   ((null my-mic-p) ;; It's off, so turn it on
    (when (timerp my-push-to-talk-mute-timer)
      (cancel-timer my-push-to-talk-mute-timer))
    (my-mic-on)
    (when obs-websocket-recording-p (my-obs-websocket-add-caption "[Microphone on]"))
    (setq my-push-to-talk-last-time (current-time)))
   ((timerp my-push-to-talk-mute-timer) ;; Push-to-talk mode
    (cancel-timer my-push-to-talk-mute-timer)
    (setq my-push-to-talk-mute-timer
          (run-at-time my-push-to-talk-threshold nil #'my-push-to-talk-mute)))
   ;; Might be push to talk, if we're within the key repeating time
   ((< (- (time-to-seconds (current-time)) (time-to-seconds my-push-to-talk-last-time))
       my-push-to-talk-threshold)
    (setq my-push-to-talk-mute-timer
          (run-at-time my-push-to-talk-threshold nil #'my-push-to-talk-mute)))
   ;; It's been a while since I turned the mic on.
   (t (my-push-to-talk-mute))))

;(global-set-key (kbd "<f12>") #'my-push-to-talk)
;; Controlling my stream audio from Emacs: background music, typing sounds, and push to talk:4 ends here

;; More background music
;; :PROPERTIES:
;; :CUSTOM_ID: more-background-music
;; :END:


;; [[file:Sacha.org::#more-background-music][More background music:1]]
(defun my-stream-emms-toggle-background ()
	(interactive)
	(unless (emms-playlist-buffer-list)
		(emms-play-directory "~/sync/Phone/music/freepd/"))
	(emms-pause)
	(emms-show))
;; More background music:1 ends here

;; Show Emacs-related tasks
;; :PROPERTIES:
;; :CUSTOM_ID: show-emacs-related-tasks
;; :END:


;; [[file:Sacha.org::#show-emacs-related-tasks][Show Emacs-related tasks:1]]
(defun my-show-emacs-tasks ()
  (interactive)
  (org-ql-search (org-agenda-files)
    '(and (todo)
          (parent (and (tags "project") (tags "emacs") (not (tags "inactive")))))
    :title "Emacs-related project tasks"
    :sort '(date priority todo)
    :super-groups '((:auto-parent t))))
;; Show Emacs-related tasks:1 ends here

;; General streaming configuration
;; :PROPERTIES:
;; :CUSTOM_ID: general-streaming-configuration
;; :END:

;; [[file:Sacha.org::#general-streaming-configuration][General streaming configuration:1]]
(defvar my-stream-captions-insert nil "Non-nil means insert into the current buffer.")
(defhydra my-stream ()
	("w" (org-open-link-from-string "[[file:~/proj/stream/index.org::#streaming-workflow][Streaming]]") "Workflow" :column "Setup")
  ;("a" my-show-emacs-tasks "Agenda")
	;("t" my-stream-insert-timestamp "Timestamp" :exit t)
  ;("bt" selectric-mode "Typing sounds")
  ;("bm" my-stream-toggle-background-music "Background music")
  ("y" (browse-url "https://studio.youtube.com/channel/UClT2UAbC6j7TqOWurVhkuHQ/livestreaming/dashboard") "Youtube")
	("ts" (browse-url "https://twitch.tv/sachachua") "View stream")
  ("tv" (browse-url "https://dashboard.twitch.tv/u/sachachua/stream-manager") "View manager")
  ;; ("s" my-stream-toggle
	 ;; 	(format "Streaming [%s]"
	 ;; 					(if (eq my-stream-type 'stream) "X" " "))
	 ;; 	:exit t
	 ;; 	:column "Streaming/recording")
  ("r" my-recording-toggle
		(format "Recording [%s]"
						(if (eq my-stream-type 'record) "X" " "))
		:exit t)
  ("r" (org-capture nil "y") "Capture" :column "During")
	("o" (org-open-link-from-string "[[file:~/proj/stream/index.org::#plans]]")
	 "Notes"
	 :exit t)
	("m" my-stream-message "Message" :exit t)
	("p" my-stream-publish-and-sync-notes "Publish" :exit t)
  ("v" (my-play-latest-recording) "Play last" :exit t))
(keymap-global-set "<f8>" #'my-stream/body)
(keymap-global-set "s-r" #'my-stream/body)
(keymap-global-set "s-v" #'my-stream/body)
(keymap-global-set "s-SPC" #'my-stream/body)
;; General streaming configuration:1 ends here

;; Stream message


;; [[file:Sacha.org::*Stream message][Stream message:1]]
(defun my-stream-message (message)
	(interactive "MMessage: ")
	(with-temp-file "~/proj/stream/message.html"
		(insert "<style>body { font-size: large; color: white; font-family: sans-serif; padding: 10px; background-color: black }</style>"
						message))
	(shell-command "scp ~/proj/stream/message.html web:/var/www/yayemacs.com"))
;; Stream message:1 ends here

;; Playing recordings
;; :PROPERTIES:
;; :CUSTOM_ID: playing-recordings
;; :END:


;; [[file:Sacha.org::#playing-recordings][Playing recordings:1]]
(use-package mpv :if my-laptop-p)
(defvar my-recordings-dir "~/recordings/")
(defun my-delete-latest-recording ()
	(interactive)
	(delete-file (my-latest-file my-recordings-dir)))
(defun my-open-latest-recording ()
	(interactive)
	(find-file (my-latest-file my-recordings-dir)))
(defun my-play-latest-recording (&optional arg)
  (interactive "P")
  (let ((latest (my-latest-file my-recordings-dir)))
    (if (and arg (file-exists-p (my-obs-websocket-caption-file latest)))
        (with-current-buffer (find-file-noselect (my-obs-websocket-caption-file (my-latest-file my-recordings-dir)))
          (goto-char (point-min))
          (subed-mpv-find-video latest)
          (pop-to-buffer (current-buffer)))
      (mpv-play (my-latest-file my-recordings-dir)))))
(defun my-rename-last-recording ()
  (interactive)
  (let ((latest (my-latest-file my-recordings-dir))
				(new-name (read-string "New name: " (format-time-string "%Y-%m-%d-"))))
    (rename-file latest
                 (expand-file-name
                  (concat new-name
													(if (and (file-name-extension latest) (null (file-name-extension new-name)))
															(concat "." (file-name-extension latest))
														""))
                  my-recordings-dir))))

(defun my-upload-recording (recording tags)
  (interactive (list (let ((latest (my-latest-file my-recordings-dir "mkv\\|mp4\\|webm")))
                       (read-file-name "Recording: " my-recordings-dir latest t)
                       (read-string "Tags: " "emacs"))))
  (start-process "youtube-upload" nil "youtube-upload" recording "--privacy=unlisted" "--license=creativeCommon"
                 (format
                  "--tags=\"%s\""
                  tags)
                 "--open-link"
                 (format "--title=%s" (shell-quote-argument (file-name-base recording)))
                 (format "--client-secrets=%s" google-video-credentials)))
;; Playing recordings:1 ends here

;; Stream notes
;; :PROPERTIES:
;; :CUSTOM_ID: stream-notes
;; :END:


;; [[file:Sacha.org::#stream-notes][Stream notes:1]]
(defun my-org-save-and-tangle-stream-notes ()
  (when (and (buffer-file-name)
						 (string= (expand-file-name (buffer-file-name))
											(expand-file-name "~/proj/stream/index.org")))
		(add-hook 'after-save-hook #'my-stream-publish-and-sync-notes nil t)))
(defun my-stream-publish-and-sync-notes ()
	(interactive)
	(with-current-buffer (find-file "~/proj/stream/index.org")
		(org-html-export-to-html)
		(let ((org-icalendar-timezone "America/Toronto")
					(org-icalendar-date-time-format ":%Y%m%dT%H%M%SZ"))
			(org-icalendar-export-to-ics))
		(shell-command "rsync -aze ssh ./ web:/var/www/yayemacs.com")))
(with-eval-after-load 'org
	(add-hook 'org-mode-hook 'my-org-save-and-tangle-stream-notes))
;; Stream notes:1 ends here

;; [[file:Sacha.org::#stream-notes][Stream notes:2]]
;; based on https://www.reddit.com/r/emacs/comments/57nps0/comment/d8umsr4/?context=3
(setq imp-default-user-filters '((org-mode . my-impatient-org-export-as-html-filter)
                                 (mhtml-mode . nil)
                                 (html-mode . nil)
                                 (web-mode  . nil)))
(defun my-imp-htmlize-filter (buffer)
  "Alternate htmlization of BUFFER before sending to clients."
  ;; leave the result in the current-buffer
  (let ((noninteractive t)
        (org-export-use-babel nil)
        (m (with-current-buffer buffer major-mode)))
    (case m
      (org-mode
       (insert
        (with-current-buffer buffer
          (org-export-as 'html))))
      (t
       (let ((html-buffer (save-match-data (htmlize-buffer buffer))))
         (insert-buffer-substring html-buffer)
         (kill-buffer html-buffer))))))

(defun my-impatient-org-export-as-html-filter (buffer)
  (let ((output-buffer (current-buffer))
        (log-message-max nil))
    (with-current-buffer buffer
      (let ((output (org-export-as 'html)))
				(with-current-buffer output-buffer (insert output))))))
(use-package impatient-mode
  :config
	(setq impatient-mode-delay 1)
	(setq httpd-port 8085)
	(imp-set-user-filter 'my/impatient-org-export-as-html-filter))
;; Stream notes:2 ends here

;; Chapters
;; :PROPERTIES:
;; :CUSTOM_ID: streaming-chapters
;; :END:


;; [[file:Sacha.org::#streaming-chapters][Chapters:1]]
(defun my-youtube-copy-chapters ()
	"Call from a VTT file with NOTE comments."
	(interactive)
	(let ((subtitles (subed-subtitle-list)))
		(kill-new
		 (concat (if (elt (car subtitles) 4)
								 ""
							 "0:00 Intro\n")
						 (mapconcat (lambda (o)
													(if (elt o 4)
															(concat (format-seconds "%m:%.2s" (/ (elt o 2) 1000))
																			" "
																			(elt o 4)
																			"\n")
														""))
												subtitles
												"")))))
;; Chapters:1 ends here

;; CANCELLED Try continuous streaming and the Google Speech Recognition API
;; CLOSED: [2023-03-23 Thu 22:27]
;;     :PROPERTIES:
;;     :CUSTOM_ID: speech-to-text
;;     :END:

;; With data logging $0.004 USD / 15 seconds


;; [[file:Sacha.org::#speech-to-text][CANCELLED Try continuous streaming and the Google Speech Recognition API:1]]
(defvar my-stream-captions-websocket nil)
(defvar my-stream-captions-history nil)
(defvar my-stream-captions-last-caption nil)
(defun my-stream-captions-insert () (interactive) (setq my-stream-captions-insert (not my-stream-captions-insert)))

(define-minor-mode my-stream-captions-minor-mode "Toggle the captions server."
  :lighter "CAP"
  :global t)

(defun my-get-last-n-chars (text limit)
  (if (< (length text) limit)
      text
    (substring text (- (length text) limit))))

(defun my-stream-captions-on-message (websocket frame)
  (let* ((payload (let ((json-object-type 'plist) (json-array-type 'list)) (json-read-from-string (websocket-frame-payload frame))))
         (type (plist-get payload :type))
         (caption (string-trim (plist-get (car (plist-get (car (plist-get (plist-get payload :stream) :results)) :alternatives)) :transcript))))

    (if (string= type "interim")
        (when (websocket-openp obs-websocket) (obs-websocket-send "SendCaptions" :text (my-get-last-n-chars caption 80)))
      (setq my-stream-captions-last-caption caption)
      (call-process "notify-send" nil nil nil caption)
      (my-obs-websocket-add-caption caption)
      (when my-stream-captions-insert (insert caption))
      (setq my-stream-captions-history (cons caption my-stream-captions-history)))))


(defun my-stream-captions-edit-last (caption)
  (interactive (list (read-string "Caption: " my-stream-captions-last-caption 'my-stream-captions-history my-stream-captions-last-caption)))
  (when (> (length caption) 0)
    (my-obs-websocket-add-caption caption)))
(keymap-global-set  "<f11>" 'my-stream-captions-edit-last)

(defun my-stream-captions-on-close (&rest args)
  (message "Captions websocket closed.")
  (my-stream-captions-minor-mode 0)
  (setq my-stream-captions-websocket nil))

(defun my-stream-captions-websocket-connect ()
  (interactive)
  (setq my-stream-captions-history nil)
  (my-stream-captions-minor-mode 1)
  (setq my-stream-captions-websocket (websocket-open "ws://localhost:8085"
                                                     :on-message #'my-stream-captions-on-message
                                                     :on-close #'my-stream-captions-on-close)))

(defvar my-stream-captions-process nil)
(defun my-stream-captions-start ()
  (interactive)
  (let ((default-directory "~/proj/speech"))
    (setq my-stream-captions-process (start-process "Stream captions" (get-buffer-create "*stream captions*") "node" "test.js"))
    (sleep-for 2)
    (my-stream-captions-websocket-connect)))

(defun my-stream-captions-sentinel (process event)
  (let ((status (process-status my-stream-captions-process)))
    (if (member status '(stop exit signal))
        (my-stream-captions-minor-mode -1))))
(defun my-stream-captions-stop ()
  (interactive)
  (stop-process my-stream-captions-process))
;; CANCELLED Try continuous streaming and the Google Speech Recognition API:1 ends here

;; Ledger
;; :PROPERTIES:
;; :CUSTOM_ID: ledger-personal-finance-in-my-config
;; :END:

;; Make it easier to review my credit card transactions


;; [[file:Sacha.org::#ledger-personal-finance-in-my-config][Ledger:1]]
(use-package ledger-mode
  :mode "\\.ledger$"
  :bind (:map ledger-mode-map
              ("C-c C-n" . my-ledger-change-account)
              ("C-c a" . my-ledger-set-unknown-account)
              ("C-c f" . (lambda () (interactive) (find-file (my-latest-file "~/Downloads"))))))
;; Ledger:1 ends here

;; [[file:Sacha.org::#ledger-personal-finance-in-my-config][Ledger:2]]
(use-package flycheck-ledger
  :after (flycheck ledger-mode)
	:hook (ledger-mode . flycheck-mode)
  :demand t)
;; Ledger:2 ends here

;; [[file:Sacha.org::#ledger-personal-finance-in-my-config][Ledger:3]]
(defvar my-ledger-account-list-cache nil)
(make-variable-buffer-local 'my-ledger-account-list-cache)
(defadvice ledger-accounts-list (around sacha activate)
  "Cache"
  (setq ad-return-value (or my-ledger-account-list-cache
                            (setq my-ledger-account-list-cache ad-do-it))))

(defun my-ledger-set-unknown-account (account point)
  (interactive (list (ledger-read-account-with-prompt "Account") (point)))
  (let ((extents (ledger-navigate-find-xact-extents point)))
    (save-excursion
      (goto-char (car extents))
      (if (re-search-forward "Expenses:\\(Unknown\\|Play\\)" (cadr extents) t)
          (replace-match account t t)
        (goto-char point)
        (beginning-of-line)
        (when (re-search-forward "\\([^ \t]+\\)  " (line-end-position) nil)
          (replace-match account t t nil 1))))))

(defun my-ledger-go-to-beginning-of-entry ()
  "Move to the beginning of the current entry."
  (while (and (not (bobp))
              (eq (ledger-context-line-type (ledger-context-at-point))
                  'acct-transaction))
    (forward-line -1)))

(defun my-ledger-entry-date ()
  "Returns the date of the entry containing point or nil."
  (save-excursion
    (my-ledger-go-to-beginning-of-entry)
    (let ((context-info (ledger-context-other-line 0)))
      (when (eq (ledger-context-line-type context-info) 'entry)
        (goto-char (line-beginning-position))
        (if (looking-at "\\([-0-9\\./]+\\)")
            (match-string-no-properties 1))))))

(defun my-ledger-guess-mbna ()
  "Adds a sub-account for the dates for my credit card transactions."
  (interactive)
  (save-excursion
    (my-ledger-go-to-beginning-of-entry)
    (forward-line 1)
    (let ((amount 0) (date (my-ledger-entry-date)) month)
      (if (string-match "[0-9]+[-\\.]\\([0-9]+\\)[-\\.]\\([0-9]+\\)" date)
          (setq month (string-to-number (match-string 1 date))))
      ;; Is this a payment or a charge?
      (save-excursion
        (while (and (eq (ledger-context-line-type (ledger-context-at-point))
                        'acct-transaction)
                    (not (eobp)))
          (let ((context (ledger-context-at-point)))
            (if (ledger-context-field-value context 'amount)
                (if (string-match "MBNA" (ledger-context-field-value context 'account))
                    (setq amount (string-to-number (ledger-context-field-value context 'amount)))
                  (setq amount (- (string-to-number (ledger-context-field-value context 'amount)))))))
          (forward-line 1)))
      (save-excursion
        (while (and (eq (ledger-context-line-type (ledger-context-at-point))
                        'acct-transaction)
                    (not (eobp)))
          (let ((context (ledger-context-at-point)))
            (if (string-match "MBNA" (ledger-context-field-value context 'account))
                (if (re-search-forward "\\(MBNA\\)[ \t]*[-$\.0-9]*[ \t]*$" (line-end-position) t)
                    (replace-match
                     (concat "MBNA:"
                             (elt
                              '("January" "February" "March" "April" "May" "June" "July" "August" "September" "October" "November" "December")
                              (% (+ (if (> amount 0) 10 11) month) 12)))
                     t t nil 1))))
          (forward-line 1))))))

(defun my-latest-file (path &optional filter)
  "Return the newest file in PATH. Optionally filter by FILTER."
  (car (sort (seq-remove #'file-directory-p (directory-files path 'full filter t)) #'file-newer-than-file-p)))
(defun my-ledger-change-account (account)
  (interactive (list (ledger-read-account-with-prompt (concat (ledger-xact-payee) ": "))))
  (beginning-of-line)
  (re-search-forward ledger-account-name-or-directive-regex)
  (replace-match (concat "  " account "  ") t t))

(defun my-ledger-fix-unknown ()
  (interactive)
  (while (re-search-forward "Expenses:Unknown.*$ \\(.+\\)" nil t)
    (my-ledger-change-account (ledger-read-account-with-prompt
                               (format "%s %s: " (s-trim (save-match-data (ledger-xact-payee)))
                                       (match-string 1))))))
;; Ledger:3 ends here

;; SSH and --daemon
;; :PROPERTIES:
;; :CUSTOM_ID: ssh-and-daemon
;; :END:

;; From https://github.com/nhoffman/.emacs.d/blob/master/init.org


;; [[file:Sacha.org::#ssh-and-daemon][SSH and --daemon:1]]
(defun my-ssh-refresh ()
  "Reset the environment variable SSH_AUTH_SOCK"
  (interactive)
  (let (ssh-auth-sock-old (getenv "SSH_AUTH_SOCK"))
    (setenv "SSH_AUTH_SOCK"
            (car (split-string
                  (shell-command-to-string
                   "ls -t $(find /tmp/ssh-* -user $USER -name 'agent.*' 2> /dev/null)"))))
    (message
     (format "SSH_AUTH_SOCK %s --> %s"
             ssh-auth-sock-old (getenv "SSH_AUTH_SOCK")))))
(my-ssh-refresh)
;; SSH and --daemon:1 ends here

;; Encryption
;; :PROPERTIES:
;; :CUSTOM_ID: encryption
;; :END:


;; [[file:Sacha.org::#encryption][Encryption:1]]
(setq epa-file-encrypt-to '("sacha@sachachua.com"))
(setq epa-pinentry-mode 'loopback)
(setq epg-pinentry-mode 'loopback)
;; Encryption:1 ends here

;; Emacspeak
;; :PROPERTIES:
;; :CUSTOM_ID: emacspeak
;; :END:


;; [[file:Sacha.org::#emacspeak][Emacspeak:1]]
;(setq emacspeak-prefix "\C-E")
  (defun my-emacspeak ()
    (interactive)
    (load-file "/home/sacha/vendor/emacspeak/lisp/emacspeak-setup.el")
		(keymap-global-set "s-e" 'emacspeak-prefix-command)
		(keymap-global-set "C-e" 'end-of-line)
    (setq emacspeak-use-auditory-icons t)
    (setq-default emacspeak-use-auditory-icons t)
    (setq-default dtk-quiet nil)
    (setq dtk-quiet nil))

  (defun my-emacspeak-quiet ()
    (interactive)
    (setq emacspeak-use-auditory-icons nil)
    (setq-default emacspeak-use-auditory-icons nil)
    (setq-default dtk-quiet t)
    (setq dtk-quiet t)
		(dtk-interp-sync)
		(ad-disable-regexp "emacspeak"))
;; Emacspeak:1 ends here

;; TOBLOG Manage photos with geeqie  :image:
;; :PROPERTIES:
;; :CUSTOM_ID: manage-photos-with-geeqie
;; :END:

;; Opening images directly in Emacs seems a little slow. Geeqie is pretty
;; fast (after generating thumbnails) and can be remotely controlled via
;; the command-line. I wrote a few functions to help me flip through
;; images, add extra stuff to filenames, change dates, and insert
;; references.


;; [[file:Sacha.org::#manage-photos-with-geeqie][TOBLOG Manage photos with geeqie:1]]
(defvar my-scan-directory "~/sync/scans/")
(defvar my-ipad-directory "~/sync/ipad")
(defvar my-portfolio-directory "~/sync/portfolio")
(defvar my-camera-directory "~/sync/camera")
(defvar my-private-sketches-directory "~/sync/private-sketches")
(defvar my-sketches-directory "~/sync/sketches")
(defun my-scans-dired () (interactive) (dired my-scan-directory "-lt"))
(defun my-geeqie-next ()
  (interactive)
  (shell-command "geeqie --remote -n"))
(defun my-geeqie-previous ()
  (interactive)
  (shell-command "geeqie --remote -b"))
(defun my-geeqie-filename ()
  (string-trim (shell-command-to-string "geeqie --remote --tell")))
(defun my-geeqie-insert-file-link ()
  (interactive)
  (insert (org-link-make-string (concat "file:" (string-trim (shell-command-to-string "geeqie --remote --tell"))))))
(use-package org :config (require 'org-attach))


(defun my-geeqie-view (filenames)
  (interactive "f")
  (start-process-shell-command "geeqie" nil
															 (concat "geeqie --remote "
																			 (mapconcat (lambda (f)
																										(concat "file:" (shell-quote-argument f)))
																									(cond
																									 ((listp filenames) filenames)
																									 ((file-directory-p filenames)
																										(list (car (seq-filter #'file-regular-p (directory-files filenames t)))))
																									 (t (list filenames)))
																									" "))))

(defvar my-rotate-jpeg-using-exiftran nil)

(defun my-rotate-image-clockwise (filename)
  (if (and my-rotate-jpeg-using-exiftran
					 (string-match "jpe?g" (file-name-extension filename)))
			(call-process "exiftran" nil nil nil "-i" "-9" filename)
		(call-process "mogrify" nil nil nil "-rotate" "90" filename)))

(defun my-rotate-image-counterclockwise (filename)
  (if (and my-rotate-jpeg-using-exiftran
					 (string-match "jpe?g" (file-name-extension filename)))
			(call-process "exiftran" nil nil nil "-i" "-2" filename)
		(call-process "mogrify" nil nil nil "-rotate" "270" filename)))

(defun my-geeqie-rotate-clockwise ()
  (interactive)
  (my-rotate-image-clockwise (my-geeqie-filename))
  (my-geeqie-view (my-geeqie-filename)))

(defun my-geeqie-rotate-counterclockwise ()
  (interactive)
  (my-rotate-image-counterclockwise (my-geeqie-filename))
  (my-geeqie-view (my-geeqie-filename)))

(defun my-rename-file-based-on-modification-time (filename)
  "Rename files to their modification time."
  (rename-file filename
							 (expand-file-name
								(concat
								 (format-time-string "%Y-%m-%d_%H%M%S"
																		 (file-attribute-modification-time (file-attributes filename)))
								 "."
								 (file-name-extension filename))
								(file-name-directory filename))))

(defun my-geeqie-change-date (filename new-time)
  (interactive (list (my-geeqie-filename)
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
		(my-geeqie-view new-file)))

(defun my-geeqie-rename-current (old-filename new-filename)
  (interactive
   (list (my-geeqie-filename)
				 (read-string "Filename: " (concat (file-name-base (my-geeqie-filename)) " "))))
  (rename-file old-filename
							 (expand-file-name
								(concat new-filename "." (file-name-extension old-filename))
								(file-name-directory old-filename))))

(defun my-geeqie-crop-to-rectangle ()
  (interactive)
  (call-process
   "mogrify" nil nil nil "-crop"
   (string-trim (shell-command-to-string "geeqie --remote --get-rectangle"))
   (my-geeqie-filename))
  (my-geeqie-view (my-geeqie-filename)))

(defun my-geeqie-scans ()
  "Rename files and open the first one."
  (interactive)
  (mapc 'my-rename-file-based-on-modification-time (directory-files my-scan-directory t "^scan"))
  (call-process "geeqie" nil nil nil "--remote" (concat "file:" (shell-quote-argument (seq-find 'file-regular-p (directory-files "~/sync/scans" t "^[0-9].*\\(gif\\|png\\|jpg\\)"))))))

(defun my-geeqie-delete-and-next ()
  (interactive)
  (let ((file (my-geeqie-filename)))
		(my-geeqie-next)
		(delete-file file t)))

(use-package ewmctrl)
(defun my-geeqie-setup ()
  (interactive)
  (shell-command "wmctrl -r :ACTIVE: -b remove,maximized_vert,maximized_horz; xdotool getactivewindow windowsize 50% 100%")
  (shell-command "geeqie &"))
(use-package pretty-hydra
  :config
  (pretty-hydra-define my-geeqie ()
    ("Open"
     (("oo" my-geeqie-setup "Setup")
			("op" (my-geeqie-view my-portfolio-directory) "Portfolio")
			("oc" (my-geeqie-view my-camera-directory) "Camera")
			("oi" (my-geeqie-view my-ipad-directory) "iPad")
			("ox" (my-geeqie-view "~/screenshots") "Screenshots")
			("os" my-geeqie-scans "Scans"))
     "Modify"
     (("[" my-geeqie-rotate-counterclockwise "CCW")
			("]" my-geeqie-rotate-clockwise "CW")
			("r" my-geeqie-rename-current "Rename")
			("d" my-geeqie-change-date "Change date")
			("c" my-geeqie-crop-to-rectangle "Crop")
			("k" (start-process "krita" nil "krita" (my-geeqie-filename)) "krita")
			("O" (shell-command (format "mogrify -auto-orient %s" (shell-quote-argument (my-geeqie-filename)))) "Rotate based on EXIF")
			("g" (start-process "gimp" nil "gimp" (my-geeqie-filename)) "gimp"))
     "Navigate"
     (("n" my-geeqie-next "Next")
			("p" my-geeqie-previous "Previous")
			("x" my-geeqie-delete-and-next "Delete"))
     "Save"
     (("p" (rename-file (my-geeqie-filename)
												(expand-file-name (file-name-nondirectory (my-geeqie-filename)) my-sketches-directory))
			 "Portfolio")
			("s" (rename-file (my-geeqie-filename)
												(expand-file-name (file-name-nondirectory (my-geeqie-filename)) my-sketches-directory))
			 "Sketch"))
     "Other"
     (("<up>" (forward-line -1) :hint nil)
			("<down>" forward-line :hint nil)

			("im" (insert (format "{{<photo nas=\"1\" src=\"%s\">}}" (my-geeqie-filename))))
			("if" (insert (my-geeqie-filename) "\n")
			 "Insert filename")
			("v" (my-geeqie-view (string-trim (thing-at-point 'line))) "View")
			("il" (insert "- " (my-geeqie-filename) "\n") "Insert filename as list item")))))

(defun my-move-portfolio-files ()
  (interactive)
  (mapc (lambda (f)
					(let ((new-dir
								 (cond
									((string-match "#private" f) my-private-sketches-directory)
									((string-match "#me\\>" f) my-sketches-directory)
									(t my-portfolio-directory))))
						(when new-dir (rename-file f (expand-file-name (file-name-nondirectory f) new-dir)))))
				(seq-filter
				 'file-regular-p
				 (directory-files my-scan-directory t "^[0-9]+.*#")))
  (shell-command-to-string "make-sketch-thumbnails"))
;; TOBLOG Manage photos with geeqie:1 ends here

;; Plover
;;    :PROPERTIES:
;;    :CUSTOM_ID: plover
;;    :END:

;; https://github.com/sachac/plover-websocket-el


;; [[file:Sacha.org::#plover][Plover:1]]
(use-package plover-websocket
  :load-path "~/proj/plover-websocket-el"
  :after websocket
  :if my-laptop-p
  :config (setq plover-websocket-plover-command "cd ~/vendor/plover; tox -e launch")
  :hydra
  (my-plover (:exit t)
             ("<f1>" plover-websocket-connect "Open websocket")
             ("<f2>" plover-websocket-add-translation "Add translation")
             ("<f3>" plover-websocket-lookup "Lookup")
             ("<f4>" plover-websocket-configure "Configure")
             ("<f5>" plover-websocket-focus "Focus")
             ("<f6>" plover-websocket-toggle-plover "Toggle Plover")
             ("<f7>" plover-websocket-quit "Quit")
             ("<f8>" my-plover-drilling-time "Drill"))
  :bind
  ("<f6>" . #'my-plover/body))
;; Plover:1 ends here

;; Looking things up
;; :PROPERTIES:
;; :CUSTOM_ID: looking-things-up
;; :END:


;; [[file:Sacha.org::#looking-things-up][Looking things up:1]]
(defun my-plover-search-dictionary-for-strokes-jq (stroke-regexp)
  (json-parse-string
   (shell-command-to-string
    (format "cat ~/.config/plover/main.json | jq 'with_entries(if (.key|test(\"%s\")) then ( {key: .key, value: .value}) else empty end)'"
      stroke-regexp))
   :object-type 'alist))
(defvar my-plover-main-dict
  (if (and my-laptop-p (file-exists-p "~/.config/plover/main.json"))
(mapcar (lambda (o) (cons (symbol-name (car o)) (cdr o)))
	(json-read-file "~/.config/plover/main.json"))))
(defun my-plover-search-dictionary-for-strokes (stroke-regexp)
  (interactive "MStroke regexp: ")
  (let ((results (seq-filter (lambda (o) (string-match stroke-regexp (car o))) my-plover-main-dict)))
    (when (called-interactively-p 'any) (my-plover-display-dictionary-results results))
    results))
(defvar my-plover-dict-cache nil "Alist of (filename . ((stroke . translation) ...))")
(defvar my-plover-home "~/.config/plover")
(defun my-plover-dict (&optional filename)
  (setq filename (expand-file-name (or filename "main.json") my-plover-home))
  (or (cdr (assoc-default filename my-plover-dict-cache))
(let ((result (mapcar (lambda (o) (cons (symbol-name (car o)) (cdr o))) (json-read-file filename))))
  (push (cons filename result) my-plover-dict-cache )
  result)))

(defun my-plover-search-dictionary-for-translation (translation &optional start file)
  (interactive "MTranslation: \nP")
  (let* ((regexp (concat "^" (regexp-quote translation) (unless start "$")))
   (results (seq-filter (lambda (o) (string-match regexp (cdr o))) (my-plover-dict file))))
    (when (called-interactively-p 'any) (my-plover-display-dictionary-results results))
    results))

(defun my-plover-display-dictionary-results (results)
  (with-current-buffer (get-buffer-create "*Plover*")
    (erase-buffer)
    (insert (format "%d entries\n" (length results))
      (mapconcat (lambda (o) (format "%s\t%s" (car o) (cdr o))) results "\n"))
    (goto-char (point-min))
    (display-buffer (current-buffer))))

(defmacro my-with-plover-fingerspelling (&rest body)
  `(progn
     (plover-websocket-send :translation "{PLOVER:SOLO_DICT:+commands.json,+fingerspelling.json}")
     (prog1 (progn ,@body)
 (plover-websocket-send :translation "{PLOVER:END_SOLO_DICT}"))))

(defun my-consult-plover-read-stroke-or-translation ()
  (interactive)
  (let ((dict (mapcar (lambda (o) (cons (format "%s: %s" (car o) (cdr o)) o))
		(my-plover-dict))))
    (my-with-plover-fingerspelling
     (consult--read
dict
:prompt "Strokes/translation: "
:category 'plover-stroke))))

(defun my-consult-plover-and-execute-strokes (choice)
  (interactive (list (my-consult-plover-read-stroke-or-translation)))
  (when (string-match "^\\([^ ]+\\): \\(.+\\)" choice)
    (plover-websocket-send :translation (match-string 2 choice) :force t :zero_last_stroke_length t)))

(defun my-consult-plover-search-strokes (regexp solo-p)
  (interactive (list (with-plover-plain (read-string "Strokes: ")) current-prefix-arg))
  (consult--read
   (mapcar (lambda (o) (cons (format "%s: %s" (car o) (cdr o)) o))
     (my-plover-search-dictionary-for-strokes (if solo-p (concat "^" regexp "\\(?:/\\|$\\)" ) (concat "^" regexp))))
   :prompt "Narrow: "))



;; (list
;;  (benchmark-run 2 (my-plover-search-dictionary-for-strokes-jq "^THER"))
;;  (benchmark-run 2 (my-plover-search-dictionary-for-translation "stenography" t "typey-type.json")
;; (benchmark-run 2 (my-plover-search-dictionary-for-translation "stenography" t))
;;  (benchmark-run 2 (my-plover-search-dictionary-for-strokes "^THER/")))
;; Looking things up:1 ends here

;; Adding steno hints as I type                                :steno:emacs:
;; :PROPERTIES:
;; :EXPORT_DATE: 2023-01-26T14:15:20-0500
;; :EXPORT_ELEVENTY_PERMALINK: /blog/2023/01/adding-steno-hints-as-i-type/
;; :EXPORT_ELEVENTY_FILE_NAME: blog/2023/01/adding-steno-hints-as-i-type/
;; :CUSTOM_ID: adding-steno-hints-as-i-type
;; :END:

;; When I type with steno, I want to see little hints. I borrowed some
;; code from company-posframe to display hints based on the last few
;; words, even ones I ended up fingerspelling or typing on my keyboard.
;; This makes it easier to learn new words if I have to spell them out.
;; There's probably a better way to do it, but this is a good start.

;; #+CAPTION: This is how the hint appears
;; [[file:images/steno.png]]


;; [[file:Sacha.org::#adding-steno-hints-as-i-type][Adding steno hints as I type:1]]
(defvar my-steno-hint-dict nil)
(defvar my-steno-hint-dictionaries
	'("~/.config/plover/user.json"
		"~/vendor/steno-dictionaries/dictionaries/dict.json"))
(defvar my-steno-hint-buffer " *steno hint*")

(defun my-steno-hint-load-dictionary ()
	(interactive)
	(setq my-steno-hint-dict
				(seq-mapcat
				 (lambda (filename)
					 (with-temp-buffer
						 (insert-file-contents filename)
						 (goto-char (point-min))
						 (json-parse-buffer :object-type 'alist)))
				 my-steno-hint-dictionaries)))

(defun my-steno-hint-lookup (search)
	(let ((search-list (list search (downcase search))))
		(seq-group-by
		 'cdr
		 (seq-filter
			(lambda (entry)
				(member (cdr entry) search-list))
			my-steno-hint-dict))))

(defun my-steno-hint-find (&optional buffer)
	"Return a steno hint for the last 1-4 words, if any."
	(setq buffer (or buffer (current-buffer)))
	(when (buffer-live-p buffer)
		(with-current-buffer buffer
			(let ((pos (point)) result hint)
				(save-excursion
					(dotimes (i 4)
						(backward-word)
						(setq result
									(cons
									 (my-steno-hint-lookup
										(string-trim (buffer-substring-no-properties (point) pos)))
									 result)))
					(delq nil result))))))

(defvar my-steno-hint-display-functions '(my-steno-hint-show-posframe))

(defun my-steno-hint-show-posframe (result &optional command)
	(if (and result (or (null command)
											(member command '(self-insert-command org-self-insert-command))))
			(progn
				(with-current-buffer (get-buffer-create my-steno-hint-buffer)
					(erase-buffer)
					(insert
					 (propertize
						(mapconcat
						 (lambda (entries)
							 (mapconcat
								(lambda (entry)
									(concat
									 (car entry) ": "
									 (mapconcat (lambda (stroke)
																(symbol-name (car stroke)))
															(cdr entry) ", ")))
								entries "\n"))
						 result "\n")
						'face 'lispy-face-hint)
					 "\n"
					 (mapconcat 'my-steno-hint-propertized-layout
											 (split-string (symbol-name (car (cadar (car result)))) "/")
											 "\n\n")))
				(posframe-show my-steno-hint-buffer :position (point) :border-width 1))
		(posframe-hide my-steno-hint-buffer)))

(defvar my-steno-hint--timer nil)

(defun my-steno-hint-recent-when-idle ()
	(interactive)
	(when (timerp my-steno-hint--timer)
		(cancel-timer my-steno-hint--timer))
	(setq my-steno-hint--timer
				(run-with-idle-timer 0.1 nil #'my-steno-hint-recent (current-buffer) this-command)))

(defun my-steno-hint-recent (buffer command)
	(interactive)
	(setq my-steno-hint--timer nil)
	(run-hook-with-args 'my-steno-hint-display-functions (my-steno-hint-find buffer) command))

(defun my-steno-split-keys (s)
	"Return a list of individual steno keys for RTFCRE."
	(when (string-match "\\([STKPWHR]*\\)\\(-\\|\\([AOEU*]+\\)\\)\\([FRPBLGTSDZ]*\\)" s)
		(append
		 (mapcar (lambda (ch) (format "%s-" (char-to-string ch))) (match-string 1 s))
		 (mapcar 'char-to-string (match-string 3 s))
		 (mapcar (lambda (ch) (format "-%s" (char-to-string ch))) (match-string 4 s)))))
;; (my-steno-split-keys "HR-")
;; (my-steno-split-keys "HRAEUT")
;; (my-steno-split-keys "HR*T")

(defun my-steno-hint-propertized-layout (s)
	(let ((keys (my-steno-split-keys s))
				(steno-layout "STPH*FPLTD\nSKWR*RBGSZ\n  AO EU")
				after-mid)
		(mapconcat
		 (lambda (ch)
			 (setq ch (char-to-string ch))
			 (pcase ch
				 ("\n" (setq after-mid nil) "\n")
				 (" " "  ")
				 (_
					(let (found)
						(if (string-match "[AEOU*]" ch)
								(setq after-mid t
											found (member ch keys))
							(setq found
										(member
										 (if after-mid (concat "-" ch)
											 (concat ch "-"))
										 keys)))
						(if found
							(concat (propertize ch 'face '(:inverse-video t)) " ")
							(concat ch " "))))))
		 steno-layout
		 "")))

(defun my-steno-hint-window-change ()
	(when (posframe-workable-p)
		(unless (string= (buffer-name)
										 my-steno-hint-buffer)
			(when (and my-steno-hint-buffer
								 (get-buffer my-steno-hint-buffer))
				(posframe-hide my-steno-hint-buffer)))))

(define-minor-mode my-steno-hint-minor-mode
	"Show hints for recent words."
	:init-value nil
	:lighter "Hint"
	(if my-steno-hint-minor-mode
			(progn
				(unless my-steno-hint-dict (my-steno-hint-load-dictionary))
				(add-hook 'post-command-hook #'my-steno-hint-recent-when-idle nil t)
				(add-hook 'window-configuration-change-hook #'my-steno-hint-window-change))

		(remove-hook 'post-command-hook #'my-steno-hint-recent-when-idle t)
		(remove-hook 'window-configuration-change-hook #'my-steno-hint-window-change)
		(when (timerp my-steno-hint--timer)
			(cancel-timer my-steno-hint--timer))
		(when (and my-steno-hint-buffer
							 (get-buffer my-steno-hint-buffer))
			(posframe-delete my-steno-hint-buffer))))
;; Adding steno hints as I type:1 ends here

;; Running Plover drills from Emacs
;; :PROPERTIES:
;; :CUSTOM_ID: running-plover-drills-from-emacs
;; :END:

;; I'm learning stenography because I deal with a lot of text, and it
;; seems interesting. I'd like to someday be able to live-caption
;; EmacsConf, meetups, and other technical things. I've got a lot of
;; muscle memory to pick up, which means drills drills drills drills.


;; [[file:Sacha.org::#running-plover-drills-from-emacs][Running Plover drills from Emacs:1]]
(defvar my-plover-drills
  (append
   (mapcar (lambda (desc)
             (cons desc (concat "https://joshuagrams.github.io/steno-jig/learn-keyboard.html?drill=" (url-encode-url (replace-regexp-in-string "\\+" "%2B" desc)))))
           '("Left hand, bottom row"
             "Right hand, bottom row"
             "Left hand, top row"
             "Right hand, top row"
             "Right hand, full bottom row"
             "Right hand, full top row"
             "Vowels"
             "Left hand"
             "Right hand"
             "All keys"
             "Left + Right"
             "Left + Vowel"
             "Vowel + Right"
             "Left + Vowel + Right"
             "Columns: D, B, L, -N"
             "x"))
   (mapcar (lambda (desc)
             (cons desc (concat "https://joshuagrams.github.io/steno-jig/learn-plover.html?hints=yes&type=randomly&timeLimit=2&drill=" (url-encode-url (replace-regexp-in-string "\\+" "%2B" desc)))))
           '("One Syllable Words" "Consonant Clusters" "Where's the TRUFT?" "Dropping Unstressed Vowels" "Inversion" "The Fifth Vowel Key" "Long Vowel Chords" "Diphthong Chords" "Vowel Disambiguator Chords" "The Missing Keys" "The Remaining Missing Letters" "Review Through Missing Letters" "Digraphs" "Review Through Digraphs" "Common Compound Clusters" "Review Through Common Compound Clusters" "Common Briefs 1-20" "Common Briefs 21-40" "Common Briefs 41-60" "Common Briefs 61-80" "Common Briefs 81-100"))))

(defvar my-plover-drill-history nil "Previous drills")
(defvar my-plover-drill-file "~/proj/plover-notes/README.org")

(defun my-plover-stenojig-custom-drill (words)
  (interactive "MWords: ")
  (plover-websocket-resume-plover)
  (unwind-protect
    (progn
    (browse-url-chrome (concat "file:///home/sacha/vendor/steno-jig/from-url.html?go=true&type=randomly&timeLimit=2&name=test&hints=true&drillItems=" (url-encode-url words)))
    (read-string "Ignore this: "))
  (plover-websocket-suspend-plover)))

(defun my-plover-drill (drill)
  "Run a single Plover keyboard drill and capture stats in an Org table."
  (interactive (list (consult--read my-plover-drills :prompt "Drill: " :sort nil
                                    :history my-plover-drill-history
                                    :default (car my-plover-drill-history))))
  (unless (string= (downcase (string-trim drill)) "x")
    (let ((url (assoc-default drill my-plover-drills)))
      (plover-websocket-resume-plover)
      (when (string-match "learn-keyboard" url)
        (plover-websocket-send :translation "{PLOVER:TOGGLE_DICT:-main.json,-user.json}"))
      (switch-to-buffer (find-file my-plover-drill-file))
      (goto-char (point-min))
      (re-search-forward "#\\+NAME: drill\n")
      (insert (format "| %s | %s |  |\n"
                      (org-link-make-string url drill)
                      (format-time-string "[%Y-%m-%d %a %H:%M]")))
      (backward-char 3)
      (browse-url url)
      (read-string "Ignore this: ")
      (when (string-match "learn-keyboard" url)
        (plover-websocket-send :translation "{PLOVER:TOGGLE_DICT:+main.json,+user.json}"))
      (insert (read-string (format "Time (%s): " (string-join (reverse (my-plover-recent-stats drill)) ", "))))
      (end-of-line)
      (forward-char 1)
      t)))

(defun my-plover-recent-stats (drill-name)
  (mapcar
   (lambda (o) (substring-no-properties (elt o 2)))
   (seq-take
    (sort (seq-filter (lambda (o) (string-match (regexp-quote drill-name) (car o)))
	                    (org-with-wide-buffer
                       (save-excursion
                         (goto-char (point-min))
                         (if (re-search-forward "#\\+NAME: drill\n" nil t)
														(org-table-to-lisp)))))
          (lambda (a b) (string< (string-trim (elt b 1))
                                 (string-trim (elt a 1)))))
    3)))

 (defun my-plover-drilling-time ()
   "Keep drilling Plover.
Restore main dictionary and turn off Plover when done."
   (interactive)
   (quantified-track "Steno")
   (call-process "wmctrl" nil 0 nil "-i" "-a" (number-to-string (my-wmctl-get-id "emacs")))
   (while (my-plover-drill (consult--read my-plover-drills :prompt "Drill: " :sort nil
                                          :history 'my-plover-drill-history
                                          :default (car my-plover-drill-history)))))
;; Running Plover drills from Emacs:1 ends here

;; Making it easier to execute commands
;; :PROPERTIES:
;; :CUSTOM_ID: making-it-easier-to-execute-commands
;; :END:


;; [[file:Sacha.org::#making-it-easier-to-execute-commands][Making it easier to execute commands:1]]
(setq enable-recursive-minibuffers t)
(defun my-replace-heading (new-text)
  (interactive (list (read-string (concat (org-get-heading t t t t) ": "))))
  (org-back-to-heading)
  (when (looking-at org-complex-heading-regexp)
    (replace-match new-text t t nil 4)))

(defun my-process-inbox-entries ()
  (interactive)
  (catch 'exit
    (while t
      (plover-websocket-send :stroke '["K-" "P-" "A-" "*"])
      (my-read-command-string
       (lambda () (concat (org-get-heading t t t t) ": "))
       '(("replace and post"
          (lambda () (interactive)
            (call-interactively 'my-replace-heading)
            (call-interactively 'my-org-mark-done-and-add-to-journal)
            (org-forward-heading-same-level 1)))
         ("edit" my-replace-heading)
         ("post" my-org-mark-done-and-add-to-journal)
         ("refile" org-refile)
         ("to do" org-todo)
         ("next" org-forward-heading-same-level)
         ("open link" (lambda () (interactive)
                        (save-excursion
                          (when (re-search-forward org-link-any-re nil t)
                            (goto-char (match-beginning 0))
                            (org-open-at-point)))))
         ("yesterday" (lambda ()  (interactive)
                        (save-excursion
                          (re-search-forward org-element--timestamp-regexp)
                          (goto-char (match-beginning 0))
                          (org-timestamp-down-day))))
         ("previous" org-backward-heading-same-level)
         ("new journal" my-journal-post)
         ("practice" (lambda () (interactive) (quantified-track "steno") (browse-url "https://didoesdigital.com/typey-type/progress")))
         ("lowercase" downcase-word)
         ("capitalize" capitalize-dwim)
         ("clean" my-org-clean-up-inbox)
         ("replace heading" my-replace-heading)
         ("cut subtree" org-cut-subtree)
         ("export subtree to 11ty" (lambda () (interactive) (org-11ty-export-to-11ty t t)))
         ("exit" (throw 'exit nil)))
       (lambda (input)
         (my-replace-heading input)
         (call-interactively 'my-org-mark-done-and-add-to-journal)
         (org-forward-heading-same-level 1))
       t))))

(defmacro my-read-command-string (prompt commands default-fn &optional include-commands)
  (declare (debug t))
  `(let* ((command
           (consult--read
            (append ,commands
                    (if ,include-commands
                        (let (res)
                          (mapatoms
                           (lambda (o)
                             (when (commandp o) (push (symbol-name o) res))))
                          res)))
            :prompt (cond
                     ((functionp ,prompt) (funcall ,prompt))
                     ((stringp ,prompt) ,prompt)
                     (t "Command: "))
            :category 'function
            :sort nil))
          (entry (assoc-default command ,commands)))
     (cond
      ((and entry (listp (car entry)))
       (if (functionp (car entry))
           (funcall (car entry))
         (eval (car entry) t)))
      (entry (call-interactively (car entry)))
      ((commandp (intern command)) (call-interactively (intern command)))
      ((functionp ,default-fn) (funcall ,default-fn command)))))

(defun my-read-commands ()
  (interactive)
  (cond
   ((derived-mode-p 'org-mode)
    (my-process-inbox-entries))
   ((derived-mode-p 'subed-mode)
    (my-plover/edit-subtitles))))
;; Making it easier to execute commands:1 ends here

;; Suggesting briefs
;; :PROPERTIES:
;; :CUSTOM_ID: suggesting-briefs
;; :END:

;; Only checks one dictionary for now, but probably good enough


;; [[file:Sacha.org::#suggesting-briefs][Suggesting briefs:1]]
(defun my-plover-briefpedia (translation)
  (interactive "MTranslation: ")
  (with-current-buffer (url-retrieve-synchronously (concat "http://briefpedia.com/AjaxTables3.php?search=" (url-encode-url translation)))
    (goto-char (point-min))
    (re-search-forward "^$")
    (while (re-search-forward "</?\\(th\\)[ >]" nil t)
			(replace-match "td" nil nil nil 1))
		(goto-char (point-min))
    (re-search-forward "^$")
    (save-excursion
      (insert "<div>")
      (goto-char (point-max)) (insert "</div>"))
    (let* ((data (xml-parse-region (point-min) (point-max)))
           (entries (mapcar (lambda (o) (string-trim (dom-text o))) (dom-by-tag (dom-by-id data "divEnglishTable") 'a)))
           (conflicts (seq-group-by 'car
                                    (mapcar (lambda (row) (mapcar (lambda (cell) (string-trim (dom-texts cell))) (dom-by-tag row 'td)))
                                            (cdr (dom-by-tag (dom-by-id data "divCrossTable") 'tr)))))
					 (result
						(mapcar (lambda (entry) (cons entry (mapcar 'cadr (assoc-default entry conflicts)))) entries)))
			(when (called-interactively-p 'any)
				(message "%s"
								 (mapconcat (lambda (entry)
															(concat (car entry)
																			(if (cdr entry)
																					(concat " ("
																									(string-join (cdr entry) ", ")
																									")")
																				"")))
														result
														"; ")))
			result)))

(defun my-plover-read-outline-for-brief (base-prompt)
  (let* ((prompt (or base-prompt "Outline: "))
         new-brief
         (brief (with-plover-plain (read-string prompt)))
         (my-conflicts (my-plover-check-for-conflict brief)))
    (while my-conflicts
      (setq prompt (format "%s%s conflicts %s (alt: %s): "
                           (if base-prompt (concat base-prompt "\n") "")
                           brief (car my-conflicts) (string-join (cdr my-conflicts) ", ")))
      (setq new-brief (with-plover-plain (read-string prompt)))
      (if (string= new-brief "")
          (setq my-conflicts nil)
        (setq brief new-brief)
        (setq my-conflicts (my-plover-check-for-conflict brief))))
    brief))

(defun my-plover-brief-with-check (translation)
  (interactive "MTranslation: ")
  (setq translation (string-trim translation))
  (let ((brief (my-plover-read-outline-for-brief (format "Outline for %s: " translation))))
    (when brief
      (kill-new (format "| %s | %s |" brief translation))
      (plover-websocket-add-translation brief translation))))

(defun my-plover-briefpedia-suggest (translation)
  (interactive "MTranslation: ")
  (setq translation (string-trim translation))
  (let* ((entries (my-plover-briefpedia translation))
         (current (my-plover-search-dictionary-for-translation translation))
         (brief
          (my-plover-read-outline-for-brief
           (concat
            (if current (format "Current: %s\n" (mapconcat 'car current "; ")) "")
            (if entries
                (concat (mapconcat
                         (lambda (entry)
                           (let ((dict-conflict (my-plover-check-for-conflict (car entry))))
                             (cond
                              ((and (cdr entry) dict-conflict)
                               (format "%s - dict conflict: %s (%s)\nbrief conflict: %s"
                                       (car entry)
                                       (car dict-conflict)
                                       (string-join (cdr dict-conflict) "; ")
                                       (string-join (cdr entry) "; ")))
                              ((cdr entry)
                               (format "%s - brief conflict: %s"
                                       (car entry)
                                       (string-join (cdr entry) "; ")))
                              (t (car entry)))))
                         entries
                         "\n")
                        "\nOutline: ")
              "No suggestions. Outline: ")))))
    (when brief
      (kill-new (format "| %s | %s |" brief translation))
      (plover-websocket-add-translation brief translation))))

(defun my-plover-check-for-conflict (outline)
  (let* ((case-fold-search nil)
         (translation (cdar (my-plover-search-dictionary-for-strokes (concat "^" outline "$"))))
         (alternatives (and translation (my-plover-search-dictionary-for-translation translation))))
    (if translation (cons translation (mapcar 'car alternatives)))))
;; Suggesting briefs:1 ends here

;; Practising within Emacs
;; :PROPERTIES:
;; :CUSTOM_ID: practising-within-emacs
;; :END:
;;    Main function: =M-x my-practise-steno=, called in an Org table of =| translation | outline |=


;; [[file:Sacha.org::#practising-within-emacs][Practising within Emacs:1]]
(defun my-practise-steno-interleave (base item)
  "Interleave BASE words with item."
  (cons item
        (-interleave base (make-list (length base) item))))
;; Copied from elfeed--shuffle
(defun my-practise-steno-shuffle (seq)
  "Destructively shuffle SEQ."
  (let ((n (length seq)))
    (prog1 seq
      (dotimes (i n)
        (cl-rotatef (elt seq i) (elt seq (+ i (cl-random (- n i)))))))))
(defun my-practise-steno-repeat (seq times)
  (funcall 'append (make-list times seq)))
(defface my-practise-steno-correct '((t :foreground "green")) "Correct.")
(defface my-practise-steno-wrong '((t :foreground "red")) "Wrong.")
(defface my-practise-steno-highlight '((t :background "white" :foreground "black")) "Focus.")
(defface my-practise-steno-base '((t :height 150)) "Base.")
(defvar my-practise-steno-items nil)
(defvar my-practise-steno-index 0)
(defvar my-practise-steno-buffer-name "*Steno practice*")
(defvar my-practise-steno-start-of-input nil)
(defvar my-practise-steno-current-overlay nil)
(defvar my-practise-steno-previous-overlay nil)
(defvar my-practise-steno-highlight-overlay nil)
(defvar my-practise-steno-stroke-buffer nil)
(defvar my-practise-steno-for-review nil)

;; From https://stackoverflow.com/questions/1249497/command-to-center-screen-horizontally-around-cursor-on-emacs
(defun my-horizontal-recenter ()
  "Make the point horizontally centered in the window."
  (interactive)
  (let ((mid (/ (window-width) 2))
        (pixel-pos (car (window-absolute-pixel-position)))
        (pixel-mid (/ (window-pixel-width) 2))
        (line-len (save-excursion (end-of-line) (current-column)))
        (cur (current-column)))
    (while (< pixel-mid pixel-pos)
      (set-window-hscroll (selected-window)
                          (1+ (window-hscroll)))
      (setq pixel-pos (car (window-absolute-pixel-position))))))

(defun my-practise-steno--handle-correct ()
  (if my-practise-steno-previous-overlay
      (move-overlay my-practise-steno-previous-overlay (overlay-start my-practise-steno-previous-overlay)
                    (+ (overlay-end my-practise-steno-previous-overlay) (match-end 0)))
    (setq my-practise-steno-previous-overlay
          (make-overlay (overlay-end my-practise-steno-previous-overlay)
                        (+ (overlay-end my-practise-steno-previous-overlay) (match-end 0))))
    (overlay-put my-practise-steno-previous-overlay 'face 'my-practise-steno-correct)))

(defun my-practise-steno--mark-incorrect-and-fixed ()
  (overlay-put (make-overlay (overlay-end my-practise-steno-previous-overlay)
                             (+ (overlay-end my-practise-steno-previous-overlay) (match-beginning 0)))
               'face 'my-practise-steno-wrong)
  ;; make a new overlay
  (setq my-practise-steno-previous-overlay (copy-overlay my-practise-steno-previous-overlay))
  (move-overlay my-practise-steno-previous-overlay
                (+ (overlay-end my-practise-steno-previous-overlay) (match-beginning 0))
                (+ (overlay-end my-practise-steno-previous-overlay) (match-end 0)))
  (setq my-practise-steno-for-review (append my-practise-steno-for-review (list (elt my-practise-steno-items my-practise-steno-index))))
  ;; highlight the sample as incorrect, too
  (let ((incorrect-sample (copy-overlay my-practise-steno-highlight-overlay)))
    (overlay-put incorrect-sample 'face 'my-practise-steno-wrong)
    (save-excursion
      (goto-char (overlay-start my-practise-steno-highlight-overlay))
      (insert (make-string
               (+
                (if (bolp) 1 0)
                (match-beginning 0))
               ?\ )))))

(defun my-practise-steno--move-to-next-item ()
  (setq my-practise-steno-stroke-buffer nil)
  (setq my-practise-steno-index (1+ my-practise-steno-index))
  (move-overlay my-practise-steno-current-overlay (overlay-end my-practise-steno-previous-overlay) (point))
  (if (elt my-practise-steno-items my-practise-steno-index)
      (move-overlay my-practise-steno-highlight-overlay
                    (1+ (overlay-end my-practise-steno-highlight-overlay))
                    (+ (overlay-end my-practise-steno-highlight-overlay)
                       1 (length (car (elt my-practise-steno-items my-practise-steno-index)))))
    (when my-practise-steno-for-review
      (goto-char (point-max))
      (kill-new (mapconcat 'car my-practise-steno-for-review " "))
      (insert "\nFor review: " (mapconcat 'car my-practise-steno-for-review " ")))))

(defun my-practise-steno--handle-completed-item ()
  ;; extend the feedback overlay to the current point
  (if (= (match-beginning 0) 0)
      (my-practise-steno--handle-correct)
    ;; mark incorrect area
    (my-practise-steno--mark-incorrect-and-fixed))
  (my-practise-steno--move-to-next-item))

(defun my-practise-steno-check (&rest _)
  (interactive)
  (let* ((sample (car (elt my-practise-steno-items my-practise-steno-index)))
         (input (and (< (overlay-end my-practise-steno-previous-overlay) (point))
                     (buffer-substring-no-properties (overlay-end my-practise-steno-previous-overlay) (point)))))
    (when (and sample input)
      (if (string-match (concat " *" (regexp-quote sample) " *") input)
          (my-practise-steno--handle-completed-item)
        ;; still in progress
        (move-overlay my-practise-steno-current-overlay
                      (overlay-start my-practise-steno-current-overlay)
                      (1+ (point))))
      (my-horizontal-recenter))))

(defun my-practise-steno-store-strokes (payload)
  (when (and (plist-get payload :stroked) (string= (buffer-name) my-practise-steno-buffer-name))
    (let ((current-item (elt my-practise-steno-items my-practise-steno-index))
          (rtfcre (plist-get (plist-get payload :stroked) :rtfcre)))
      (save-excursion
        (goto-char (point-max))
        (insert (if (bolp) "" " ") rtfcre))
      (when (and (cadr current-item)
               (> (- (overlay-end my-practise-steno-current-overlay)
                     (overlay-start my-practise-steno-current-overlay))
                  (length (car current-item))))
        (setq my-practise-steno-stroke-buffer (append my-practise-steno-stroke-buffer (list rtfcre)))
        (momentary-string-display (format " (%s -> %s)"
                                          (string-join my-practise-steno-stroke-buffer " ")
                                          (cadr current-item))
                                  (point)
                                  ?\0
                                  "")))))

(defun my-practise-steno (items)
  "Display ITEMS for practicing.
ITEMS should be a list like ((word) (word) (word))."
  (interactive (list (let ((table (org-table-to-lisp)))
                       (if table
                           (if current-prefix-arg
                               (subseq table
                                       (1- (org-table-current-line))
                                       (min (length table) (+ (org-table-current-line) current-prefix-arg -1)))
                             table)
                         my-practise-steno-items))))
  (with-current-buffer (get-buffer-create my-practise-steno-buffer-name)
    (erase-buffer)
    (insert "\n" (mapconcat 'car items " ") "\n")
    (save-excursion (insert "\n\n"))
    (toggle-truncate-lines 1)
    (setq my-practise-steno-items items
          my-practise-steno-index 0
          my-practise-steno-start-of-input (point)
          my-practise-steno-for-review nil
          my-practise-steno-current-overlay (make-overlay (point) (1+ (point)))
          my-practise-steno-previous-overlay (make-overlay (point) (point))
          my-practise-steno-stroke-buffer nil
          my-practise-steno-highlight-overlay (make-overlay (1+ (point-min)) (+ 1 (point-min) (length (car (car items))))))
    (buffer-face-set "my-practise-steno-base")
    (overlay-put my-practise-steno-previous-overlay 'face 'my-practise-steno-correct)
    (overlay-put my-practise-steno-highlight-overlay 'face 'my-practise-steno-highlight)
    (overlay-put my-practise-steno-current-overlay 'modification-hooks '(my-practise-steno-check))
    (overlay-put my-practise-steno-current-overlay 'insert-in-front-hooks '(my-practise-steno-check))
    (overlay-put my-practise-steno-current-overlay 'face 'my-practise-steno-wrong)
    ;; (add-hook 'after-change-functions 'my-practise-steno-check nil t)
    (add-hook 'plover-websocket-on-message-payload-functions 'my-practise-steno-store-strokes)
    (switch-to-buffer (current-buffer))))

(defun my-practise-steno-word-list (words)
  (interactive (list (mapcar 'list (split-string (read-string "Words: ")))))
  (my-practise-steno words))


;; (call-interactively 'my-practise-steno)
;; Practising within Emacs:1 ends here

;; Editing subtitles
;; :PROPERTIES:
;; :CUSTOM_ID: editing-subtitles
;; :END:


;; [[file:Sacha.org::#editing-subtitles][Editing subtitles:1]]
(defun my-subed-subtitle-set-text (text)
  (interactive "MNew text: ")
  (subed-jump-to-subtitle-text)
  (delete-region (point) (or (subed-jump-to-subtitle-end) (point)))
  (insert text))

(defun my-plover/edit-find-target (input)
  (or (looking-at (concat "\\b" (regexp-quote input) "\\b"))
      (re-search-forward (concat "\\b" (regexp-quote input) "\\b")
                         nil t)))
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
;; Editing subtitles:1 ends here

;; Using inotify to add Plover Clippy suggestions into Emacs
;;     :PROPERTIES:
;;     :EXPORT_DATE: 2021-06-18
;;     :EXPORT_ELEVENTY_PERMALINK: /blog/2021/06/using-inotify-to-add-plover-clippy-suggestions-into-emacs/
;;     :EXPORT_ELEVENTY_FILE_NAME: blog/2021/06/using-inotify-to-add-plover-clippy-suggestions-into-emacs/
;;     :CUSTOM_ID: plover_clippy_buffer
;;     :END:

;; Update 2021-06-19: Changed to a vertical layout, added extra notes, simplified

;; I don't have a lot of screen space on my laptop, so I don't usually
;; have the [[https://github.com/openstenoproject/plover][Plover]] suggestion window open as I type. I came up with a
;; [[https://github.com/sachac/plover-sacha-plugin/blob/main/plover_sacha_plugin/commands.py][Plover plugin]] to let me flash the last [[https://github.com/tckmn/plover_clippy][Plover Clippy]] suggestion as a
;; temporary notification. It went by too quickly, though, so I wrote
;; something that uses inotify to monitor the clippy.txt log and put it
;; an Emacs buffer instead. It results in text like this:

;; #+begin_example
;; Clippy
;; KHREUP PEU
;; added
;; ATD
;; #+end_example


;; [[file:Sacha.org::#plover_clippy_buffer][Using inotify to add Plover Clippy suggestions into Emacs:1]]
(defvar my-clippy-recent-suggestions nil "Recent suggestions, limited by `my-clippy-recent-suggestions-limit`.")
(defvar my-clippy-recent-suggestions-limit nil "If non-nil, keep this many suggestions.")
(defvar my-clippy-extra-notes nil "Extra notes to add at the end.")
(defun my-clippy-last ()
  (let ((value (string-trim (shell-command-to-string "tail -1 ~/.config/plover/clippy.txt | cut -c 23-"))))
    (when (string-match "^\\(.*?\\)[ \t]+|| .*? -> \\(.+\\)" value)
      (cons (match-string 1 value) (match-string 2 value)))))

(defun my-clippy-show (&rest _)
  (interactive)
  (with-current-buffer (get-buffer-create "*Clippy*")
    (let ((last (my-clippy-last)))
      (if my-clippy-recent-suggestions-limit
          (progn
            (unless (equal last (car my-clippy-recent-suggestions))
              (setq my-clippy-recent-suggestions (seq-take (cons last my-clippy-recent-suggestions) my-clippy-recent-suggestions-limit)))
            (erase-buffer)
            (insert (mapconcat (lambda (o) (format "| %s | %s |\n"  (car o) (cdr o))) my-clippy-recent-suggestions "")))
        (unless (equal last (car my-clippy-recent-suggestions))
          (setq my-clippy-recent-suggestions (cons last my-clippy-recent-suggestions))
          (goto-char (point-min))
          (insert (format "| %s | %s |\n" (car last) (cdr last))))))
    (when (get-buffer-window (current-buffer))
      (set-window-point (get-buffer-window (current-buffer)) (point-min)))))

(defun my-insert-symbol (symbol-name)
  (interactive (list
                (let ((orig-buffer (current-buffer)))
                  (completing-read
                   "Insert symbol: "
                   #'help--symbol-completion-table
                   (lambda (vv)
                     ;; In case the variable only exists in the buffer
                     ;; the command we switch back to that buffer before
                     ;; we examine the variable.
                     (with-current-buffer orig-buffer
                       (or (get vv 'variable-documentation)
                           (functionp vv)
                           (and (boundp vv) (not (keywordp vv))))))))))
  (insert symbol-name))

(defun my-insert-variable-value (symbol-name)
  (interactive (list
                (let ((orig-buffer (current-buffer)))
                  (completing-read
                   "Insert variable: "
                   #'help--symbol-completion-table
                   (lambda (vv)
                     ;; In case the variable only exists in the buffer
                     ;; the command we switch back to that buffer before
                     ;; we examine the variable.
                     (with-current-buffer orig-buffer
                       (or (get vv 'variable-documentation)
                           (and (boundp vv) (not (keywordp vv))))))))))
  (insert (symbol-value (intern symbol-name))))

(defun my-insert-function (symbol-name)
  (interactive (list
                (completing-read
                 "Insert function: "
                 #'help--symbol-completion-table
                 'functionp)))
  (insert symbol-name))

(defvar my-clippy-monitor nil)
(defun my-clippy-toggle-monitor ()
  (interactive)
  (if (inotify-valid-p my-clippy-monitor)
      (progn
        (message "Turning off")
        (inotify-rm-watch my-clippy-monitor))
    (message "Turning on")
    (setq my-clippy-monitor
          (inotify-add-watch
           (expand-file-name "~/.config/plover/clippy.txt") 'modify
           #'my-clippy-show))))
;; Using inotify to add Plover Clippy suggestions into Emacs:1 ends here

;; Stenoing interface
;; :PROPERTIES:
;; :CUSTOM_ID: stenoing-interface
;; :END:


;; [[file:Sacha.org::#stenoing-interface][Stenoing interface:1]]
(defvar my-plover-quick-notes "~/proj/plover-notes/scratch.org")
(defvar my-plover-current-stroke-buffer "*Current stroke*")
(defun my-plover-add-note (string)
  (interactive "MNote: ")
  (with-current-buffer (find-file-noselect my-plover-quick-notes)
    (goto-char (point-min))
    (insert string)
    (unless (bolp) (insert "\n"))))

(defun my-plover-add-last-clippy-to-notes ()
  (interactive)
  (my-plover-add-note (format "| %s | %s |\n" (caar my-clippy-recent-suggestions) (cdar my-clippy-recent-suggestions))))

(defun my-plover-scroll-notes ()
  (interactive)
  (message "Hello")
  (when (get-buffer-window (get-file-buffer my-plover-quick-notes))
    (with-selected-window (get-buffer-window (get-file-buffer my-plover-quick-notes))
      (scroll-up))))

(defun my-plover-scroll-notes-down ()
  (interactive)
  (message "World")
  (when (get-buffer-window (get-file-buffer my-plover-quick-notes))
    (with-selected-window (get-buffer-window (get-file-buffer my-plover-quick-notes))
      (scroll-down))))

(defun my-plover-spectra-last-clippy ()
  (interactive)
  (browse-url (format "http://localhost:8081/?outline=%s&translation=%s"
                      (car (split-string (cdar my-clippy-recent-suggestions) ", "))
                      (caar my-clippy-recent-suggestions))))

(defun my-plover-layout-windows ()
  "Organize my windows."
  (interactive)
  (delete-other-windows)
  (when plover-websocket-stroke-buffer-name
    (with-selected-window (split-window-below -4)
      (switch-to-buffer plover-websocket-stroke-buffer-name)))
  (with-selected-window (split-window-right 100)
    (switch-to-buffer (get-buffer-create "*Clippy*"))
    (when my-plover-quick-notes
      (with-selected-window (split-window-below 10)
        (switch-to-buffer (find-file my-plover-quick-notes))))))

(defun my-plover-clear-stroke-log ()
  (interactive)
  (with-current-buffer (get-buffer-create plover-websocket-stroke-buffer-name)
    (erase-buffer)))

(setq plover-websocket-stroke-buffer-name "*Stroke log*")
;; Stenoing interface:1 ends here

;; Cheat sheets
;;     :PROPERTIES:
;;     :CREATED:  [2021-05-26 Wed 15:38]
;;     :CUSTOM_ID: cheat-sheets
;;     :END:


;; [[file:Sacha.org::#cheat-sheets][Cheat sheets:1]]
(defun my-steno-quick-help ()
	(interactive)
	(with-selected-window
			(display-buffer-at-bottom
			 (find-file-noselect "~/proj/plover-notes/cheat-sheet.txt")'())
    ;; ... mark it as dedicated to prevent focus from being stolen
    (set-window-dedicated-p (selected-window) t)
    ;; ... and shrink it immediately.
    (fit-window-to-buffer)))

(defhydra my-hydra/cheatsheet/plover ()
  "SKHW- symbols -LTZ modifiers TWR- journal phrases
newparSKWRAURBGS bsPW-FP capKPA !space!capTK-LS cap!spaceKPA rmspcTK-FPS*
number: dupeD, revEU, 00/#OD, 00Z, $DZ, timeK- or -BG
`KH-FG  ^KR-RT ~T*LD <AEPBGT =QA*LS >A*EPBGT |PAO*EUP \\_R*UND
-H-N --TK-RB ,KW-BG ;SKWR*RBGS :capSTPH-FPLT :KL-N !SKHRAPL
?H-F /OI .nspP-P ...SKWR-RBGS 'A*E,AE \"KW-GS,KR-GS
(PREN,* [PWR-BGT,* {TPR-BGT,* @KWRAT $TK-PL *STA*R
\\SPWHRAERB \\&SP-PBD #HAERB percPERS +PHR*US
retro KA*PD cap last *UPD cap all HRO*ERD lowered #* star AFPS add space TK-FPS del space
next HRO*ER lower KPA*L cap all
mode SPH-: RL lower R reset T Title -FPLT _RBGS")

(defhydra my-hydra/cheatsheet/jade-plover-phrasing ()
  "S: SWR I, KPWR you, KWHR he, SKWHR she, TWH they, TWR we, KPWH it, STKPWHR nothing
M: OE don't (AOE really don't OEU don't really)
AU didn't, E doesn't, O can't, A or U really, AOEU don't even
E: PB know, P want, RPL remember, BL believe, FG forget, R are
BG can, BGD could, BGT can't, BLG like, BLGT like to, BLGTS likes to
BLT believe that, BS said, BT be the, BTS be said to, BTZ say to
D had, F have, FGT forgot, FLG feel like, FLGT felt like, FLT felt
FPLT must, FR ever, FRB wish, FRBT wish to, FS was, FT have to, FTS has to, FZ has, GT get, L will, LG love, PBD need, PBG think, PBL mean,
PLD mind, PLG imagine, PLT might
"
  )

(defhydra my-hydra/cheatsheet/emily-symbols ()
  "SKHW+ A (spc before) O (spc after) * (cap)
        v   E         U     EU
FG ws   Tab Backspace Del   Esc
RPBG    Up  Left      Right Down
FPBL    ↑   ←         →     ↓
FRPBG   PgU Home      End   PgD
blank   ''  {*!}      {*?}  spc
FPL     (   [         <     {
RBG     )   ]         >     }
'F *L +G &FBG \"FP #FRLG $RPBL percFRPB
,B -PL .R /RP :LG ;RB =PBLG @FRPBLG \\FB \\^RPG
_BG `P |PB ~FPBG
-S 2x -T 3x -ST 4x"
  )
(defhydra my-hydra/cheatsheet/emily-modifiers ()
  "-LTZ F (C-) R (S-) P(s-) B(M-)
Z is STKPW
AO makes SKWR binary 0-9
Symbols with *, AO variants
TR tab delete backspace esc
KPWR up left down right
KPWHR pgup end home pgdown
blank esc tab return spc
TPH ( < [ {
KWR ) > ] }
P `
H '
!HR \"PH #TKHR $KPWH percPWHR &SKP *T +K ,W -TP .R /WH :TK ;WR
=TKPW ?TPW @TKPWHR \\PR ^KPR |PW ~TPWR")
;; Cheat sheets:1 ends here

;; Coding with Plover
;; :PROPERTIES:
;; :CUSTOM_ID: coding-with-plover
;; :END:


;; [[file:Sacha.org::#coding-with-plover][Coding with Plover:1]]
(defun my-plover-insert-defun ()
  "Prompt for parts of a function definition."
  (interactive)
  (insert "(defun ")
  (plover-websocket-send :translation "{MODE:LOWER}{MODE:SET_SPACE:-}")
  (insert (replace-regexp-in-string "-$" "" (read-string "Function name: ")))
  (insert " (")
  (plover-websocket-send :translation "{MODE:SET_SPACE: }")
  (let ((args (replace-regexp-in-string "\\<optional\\>" "&optional" (string-trim (read-string "Args: ")))))
    (insert args)
    (insert ")\n")
    (if (y-or-n-p "Interactive? ")
        (if (string= args "")
            (insert "(interactive)\n")
          (insert "(interactive (list))\n"))))
  (plover-websocket-send :translation "{MODE:RESET}{}{-|}")
  (insert (format "\"%s\"\n"
                  (replace-regexp-in-string "\"" "\\\"" (string-trim (read-string "Docstring: ")))))
  (save-excursion (insert ")") (lispy--normalize-1))
  (plover-websocket-send :translation "{MODE:LOWER}"))

(defun my-plover-insert-defvar ()
  (interactive)
  "Define a variable."
  (insert "(defvar ")
  (plover-websocket-send :translation "{MODE:LOWER}{MODE:SET_SPACE:-}")
  (insert (replace-regexp-in-string "-$" "" (read-string "Variable name: ")))
  (insert " ")
  (plover-websocket-send :translation "{MODE:RESET}{}{-|}")
  (insert (string-trim (read-string "Default value: ")))
  (insert (format " \"%s\")\n"
                  (replace-regexp-in-string "\"" "\\\"" (string-trim (read-string "Docstring: "))))))
;; Coding with Plover:1 ends here

;; [[file:Sacha.org::#coding-with-plover][Coding with Plover:2]]
(defun my-org-edit-special-dwim ()
  (interactive)
  (cond
    ((org-src-edit-buffer-p) (org-edit-src-exit))
    ((org-in-src-block-p) (org-edit-special))
    ((derived-mode-p 'org-mode)
     (org-insert-structure-template "src emacs-lisp")
     (org-edit-special))))
(defun my-org-execute-special-dwim ()
  (interactive)
  (cond
    ((org-src-edit-buffer-p) (eval-buffer))
    ((org-in-src-block-p) (org-babel-execute-src-block))
    (t (eval-buffer))))
;; Coding with Plover:2 ends here

;; Displaying frequency-sorted completions with stroke hints
;; :PROPERTIES:
;; :CUSTOM_ID: displaying-frequency-sorted-completions-with-stroke-hints
;; :END:


;; [[file:Sacha.org::#displaying-frequency-sorted-completions-with-stroke-hints][Displaying frequency-sorted completions with stroke hints:1]]
(defvar my-company-strokedict--grep-executable "grep")

(defun my-company-strokedict--candidates (prefix)
  "Fetches the candidates matching PREFIX."
  (mapcar (lambda (o)
            (let ((data (split-string o "\t")))
              (propertize (car data) 'meta (cadr data))))
          (split-string
           (shell-command-to-string (concat
                                     my-company-strokedict--grep-executable
                                     " -i "
                                     (shell-quote-argument (concat "^" prefix))
                                     " "
                                     "~/.config/plover/annotated.txt -m 10"))
           "\n")))

(defun my-company-strokedict--annotation (candidate)
  (let ((stroke (get-text-property 0 'meta candidate)))
    (if stroke
        (format " (%s)" stroke)
      "")))

(defun my-company-strokedict (command &optional arg &rest ignored)
  "`company-mode' backend for user-provided dictionaries. Dictionary files are lazy
loaded."
  (interactive (list 'interactive))
  (cl-case command
    (interactive     (company-begin-backend 'my-company-strokedict))
    (candidates      (my-company-strokedict--candidates arg))
    (prefix  (when-let ((prefix (company-grab-word))) (substring-no-properties prefix)))
    (annotation (my-company-strokedict--annotation arg))
    (sorted          t)
    (duplicates      t)
    (no-cache        t)))

(use-package company
  :hook
	(prog-mode . company-mode)
  ;(add-to-list 'company-backends 'my-company-strokedict)
  )
;; Displaying frequency-sorted completions with stroke hints:1 ends here

;; MineClone
;; :PROPERTIES:
;; :CUSTOM_ID: mineclone
;; :END:


;; [[file:Sacha.org::#mineclone][MineClone:1]]
(defun my-mineclone-ripgrep ()
		(interactive)
		(let ((default-directory "~/vendor/MineClone2"))
			(call-interactively 'consult-ripgrep)))
(keymap-global-set "s-M" 'my-mineclone-ripgrep)
;; MineClone:1 ends here

;; EmacsConf
;; :PROPERTIES:
;; :CUSTOM_ID: emacsconf
;; :END:


;; [[file:Sacha.org::#emacsconf][EmacsConf:1]]
(autoload 'emacsconf-mail-prepare "emacsconf-mail")
(defun my-emacsconf-search-mail (talk)
	(interactive (list (emacsconf-complete-talk)))
	(emacsconf-with-talk-heading talk
		(notmuch-search (format "from:%s or to:%s" (org-entry-get (point) "EMAIL")
														(org-entry-get (point) "EMAIL")))))
(use-package emacsconf
  :after hydra
  :bind (("C-c e" . emacsconf/body)
         ("M-g t" . emacsconf-go-to-talk))
	:init
	(require 'emacsconf-autoloads)
	:hook
	(message-send . emacsconf-mail-check-for-zzz-before-sending)
	:config
	(defhydra emacsconf
		(:exit t)
		("t" emacsconf-go-to-talk "talk")
		("n" emacsconf-mail-notmuch-search-for-talk "notmuch search")
		("f" emacsconf-cache-find-file "file")
		("c" (find-file emacsconf-org-file) "conf.org")

		("C" (let ((default-directory (file-name-directory emacsconf-org-file)))
					 (call-interactively #'projectile-find-file)) "org dir")
		("w" (let ((default-directory emacsconf-directory))
					 (call-interactively #'projectile-find-file)))
		("o"
		 (progn
			 (find-file (expand-file-name "2024/organizers-notebook/index.org" emacsconf-directory))
			 (call-interactively #'consult-org-heading))
		 "org notes")
		("a" (let ((default-directory emacsconf-ansible-directory))
					 (call-interactively #'projectile-find-file)) "ansible")
		("A" emacsconf-prep-agenda "agenda")
		("I" emacsconf-extract-irc/body "IRC extract")
		("ie" emacsconf-insert-talk-email "email")
		("it" emacsconf-insert-talk-title "title")
		("O" (switch-to-buffer (erc-get-buffer "#emacsconf-org")))
		("l" (let ((default-directory "~/proj/emacsconf/lisp"))
					 (call-interactively #'projectile-find-file)))
		("b" emacsconf-backstage-dired "backstage")
		("u" emacsconf-upload-dired "upload")
		("vie" emacsconf-volunteer-insert-email "volunteer email")
		("U" emacsconf-res-upload-dired "upload"))
  :load-path "~/proj/emacsconf/lisp")
(keymap-global-set "M-g t" 'emacsconf-go-to-talk)
;; EmacsConf:1 ends here

;; ChatGPT, AI, and large-language models
;; :PROPERTIES:
;; :CUSTOM_ID: chatgpt-ai
;; :END:


;; [[file:Sacha.org::#chatgpt-ai][ChatGPT, AI, and large-language models:1]]
(use-package chat
	:quelpa (chat :fetcher github :repo "iwahbe/chat.el"))
(use-package org-ai
	:quelpa (org-ai :fetcher github :repo "rksm/org-ai"))
(use-package khoj
  :after org
	:disabled t
  :quelpa (khoj :fetcher github :repo "debanjum/khoj" :files (:defaults "src/interface/emacs/khoj.el"))
  :bind ("C-c s" . 'khoj))
;; ChatGPT, AI, and large-language models:1 ends here

;; [[file:Sacha.org::#chatgpt-ai][ChatGPT, AI, and large-language models:2]]
(use-package gptel
	:commands (gptel gptel-send gptel-set-topic gptel-menu)
	:hook
	(gptel-post-stream . gptel-auto-scroll)
	(gptel-post-response . gptel-end-of-response))
;; ChatGPT, AI, and large-language models:2 ends here

;; Paint
;; :PROPERTIES:
;; :CUSTOM_ID: paint
;; :END:


;; [[file:Sacha.org::#paint][Paint:1]]
(use-package paint
  :disabled t
  :if my-laptop-p
  :load-path "~/sync/cloud/elisp"
  :init
  (progn
    (setq paint-foreground-color "white" paint-background-color "black")
    (defun my-paint () (interactive) (delete-other-windows) (paint 1600 900 nil))))
;; Paint:1 ends here

;; Oddmuse
;; :PROPERTIES:
;; :CUSTOM_ID: oddmuse
;; :END:


;; [[file:Sacha.org::#oddmuse][Oddmuse:1]]
(use-package oddmuse
  :if my-laptop-p
  :load-path "~/vendor/oddmuse-el"
  :ensure nil
  :config (oddmuse-mode-initialize)
  :hook (oddmuse-mode-hook .
                           (lambda ()
                             (unless (string-match "question" oddmuse-post)
                               (when (string-match "EmacsWiki" oddmuse-wiki)
                                 (setq oddmuse-post (concat "uihnscuskc=1;" oddmuse-post)))
                               (when (string-match "OddmuseWiki" oddmuse-wiki)
                                 (setq oddmuse-post (concat "ham=1;" oddmuse-post)))))))
;; Oddmuse:1 ends here

;; Animation for Emacs chats  :video:animation:
;; :PROPERTIES:
;; :CUSTOM_ID: animation-for-emacs-chats
;; :END:


;; [[file:Sacha.org::#animation-for-emacs-chats][Animation for Emacs chats:1]]
(defun my-animate-emacs-chat ()
  (interactive)
  (text-scale-set 6)
  (erase-buffer)
  (sit-for 3)
  (let ((list '("Emacs Chat: Sacha Chua"
                "interviewed by Bastien Guerry"
                ""
                "July 24, 2013"
                "sachachua.com/emacs-chat"))
        (approx-width 41)
        (approx-height 16)
        row)
    (setq row (/ (- approx-height (length list)) 2))
    (mapcar
     (lambda (x)
       (animate-string x
                       row
                       (/ (- approx-width (length x)) 2))
       (setq row (1+ row)))
     list)))
;; Animation for Emacs chats:1 ends here

;; Completion at point?
;; :PROPERTIES:
;; :CUSTOM_ID: completion-at-point
;; :END:


;; [[file:Sacha.org::#completion-at-point][Completion at point?:1]]
(use-package corfu :init (global-corfu-mode))
(use-package cape
	:bind (("M-/" . completion-at-point))
	:init
	(add-to-list 'completion-at-point-functions #'cape-dabbrev)
	(add-to-list 'completion-at-point-functions #'cape-file)
	(add-to-list 'completion-at-point-functions #'cape-elisp-block)
	(add-to-list 'completion-at-point-functions #'cape-abbrev)
	(add-to-list 'completion-at-point-functions #'cape-dict)
	(add-to-list 'completion-at-point-functions #'cape-line)

	)
;; Completion at point?:1 ends here

;; Tools for organizing
;; :PROPERTIES:
;; :CUSTOM_ID: tools-for-organizing
;; :END:


;; [[file:Sacha.org::#tools-for-organizing][Tools for organizing:1]]
(defun my-rename-bank-statements ()
  (interactive)
  (let ((months '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")))
    (cl-loop for i from 1 to 12 do
             (message "%d" i)
             (goto-char (point-min))
             (while (re-search-forward (elt months (1- i)) nil t)
               (ignore-errors
                 (replace-match (format "%02d" i))
                 )))))

(defun my-rename-scanned-receipts ()
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

;; TODO Make memes from Emacs
;; :PROPERTIES:
;; :CREATED:  [2024-01-16 Tue 17:48]
;; :END:


;; [[file:Sacha.org::*Make memes from Emacs][Make memes from Emacs:1]]
(use-package meme
	;:quelpa (meme :fetcher github :repo "larsmagne/meme")
	:load-path "~/vendor/meme"
	:init (provide 'imgur)  ; fake this
	:config
	(setq meme-dir "~/vendor/meme/images")
	(setq meme-font "Roboto"))
;; Make memes from Emacs:1 ends here

;; Rubik's Cube
;; :PROPERTIES:
;; :CUSTOM_ID: rubik-s-cube
;; :END:

;; [[file:Sacha.org::#rubik-s-cube][Rubik's Cube:1]]
(use-package eagle
	:quelpa (eagle :fetcher git
								 :url "https://codeberg.org/akib/emacs-eagle.git"))
(use-package cube
	:quelpa (cube :fetcher git
								:url "https://codeberg.org/akib/emacs-cube.git"))
;; Rubik's Cube:1 ends here

;; Diagrams
;; :PROPERTIES:
;; :CUSTOM_ID: diagrams
;; :END:


;; [[file:Sacha.org::#diagrams][Diagrams:1]]
;; Start of cubing code
(defun my-cubing-pos (size n i)
	(list
	 (* (/ size n) (% i n))
	 (* (/ size n) (/ i n))))

(defun my-cubing-last-layer-arrows (arrows)
	"Draw ARROWS.
Arrows are defined as a list of lists of the form
((from to) (from to t) ...). Ex: '(my-cubing-last-layer-arrows '((3 1 t) (2 8 t)))
Cells are numbered from left to right, top to bottom, with the top left box being 0.
"
	(let* ((size 99)
				 (n 3)
				 (arrow-color "#000")
				 (svg (svg-create size size)))
		(svg--append
		 svg
		 (dom-node
			'defs
			nil
			(dom-node
			 'marker
			 '((id . "arrowhead")
				 (markerWidth . "10")
				 (markerHeight . "7")
				 (refX . "0")
				 (refY . "3.5")
				 (orient . "auto-start-reverse"))
			 (dom-node
				'polygon
				`((fill . ,arrow-color)
					(points . "0 0, 4 3.5, 0 7")))
			 )))
		(dotimes (i (* n n))
			(let ((pos (my-cubing-pos size n i)))
				(svg-rectangle
				 svg
				 (car pos)
				 (cadr pos)
				 (/ size n)
				 (/ size n)
				 :fill "#fff"
				 :stroke-width 1
				 :stroke "#666")))
		(dolist (arrow arrows)
			(let ((from (car arrow))
						(to (cadr arrow)))
				(apply 'svg-line
							 (append
								(list svg)
								(mapcar (lambda (o) (+ o (/ size (* 2 n))))
												(my-cubing-pos size n from))
								(mapcar (lambda (o) (+ o (/ size (* 2 n))))
												(my-cubing-pos size n to))
								(list
								 :stroke-width 2
								 :stroke arrow-color
								 :marker-start (if (elt arrow 2) "url(#arrowhead)")
								 :marker-end "url(#arrowhead)")))))
		(with-temp-buffer
			(svg-print svg)
			(buffer-string))))

(defvar my-cubing-colors '((?R  . "#ff0000")
													 (?G  . "#00ff00")
													 (?B  . "#0000ff")
													 (?O  . "#ed7117")
													 (?Y  . "#ffff00")
													 (?W  . "#ffffff")
													 (?\? . "#666666")))

(defun my-cubing-last-layer-with-sides (sides top arrows)
	"Draw a diagram of the top of the cube.
The style is similar to https://www.cubeskills.com/uploads/pdf/tutorials/pll-algorithms.pdf .
SIDES is a string specifying colors going clockwise from the back-left side.
TOP is a string specifying colors going from left to right, top to bottom.
Arrows are defined as a list of lists of the form ((from to) (from to t) ...).
Cells are numbered from left to right, top to bottom, with the top left box being 0.
Ex: (my-cubing-last-layer-with-sides \"ORRBOOGGGRBB\" \"YYYYYYYYY\" '((3 1 t) (2 8 t)))
"
	(let* ((size 99)
				 (n 3)
				 (side-size 10)
				 (cell-size (/ (- size (* 2 side-size)) n))
				 (arrow-color "#000")
				 (svg (svg-create size size)))
		(svg--append
		 svg
		 (dom-node
			'defs
			nil
			(dom-node
			 'marker
			 '((id . "arrowhead")
				 (markerWidth . "10")
				 (markerHeight . "7")
				 (refX . "0")
				 (refY . "3.5")
				 (orient . "auto-start-reverse"))
			 (dom-node
				'polygon
				`((fill . ,arrow-color)
					(points . "0 0, 4 3.5, 0 7"))))))
		;; Draw the sides. It's a string of colors going clockwise from back left
		(when sides
			(dotimes (i (* n 4))
				(apply 'svg-rectangle
							 (append
								(list svg)
								(pcase (/ i n)
									(0 (list (+ (* (% i n) cell-size) side-size)
													 0
													 cell-size
													 side-size))
									(1 (list (+ side-size (* n cell-size))
													 (+ (* (% i n) cell-size) side-size)
													 side-size
													 cell-size))
									(2 (list (+ (* (- n (% i n) 1) cell-size) side-size)
													 (+ (* n cell-size) side-size)
													 cell-size
													 side-size))
									(3 (list 0
													 (+ (* (- n (% i n) 1) cell-size) side-size)
													 side-size
													 cell-size)))
								(list
								 :stroke-width 1
								 :stroke "#666"
								 :fill (assoc-default (elt sides i)
																			my-cubing-colors
																			'eq
																			(assoc-default ?\? my-cubing-colors)))))))
		;; Draw the top face specified by a string of colors going from left to right, top to bottom
		(dotimes (i (* n n))
			(let ((pos (my-cubing-pos (* cell-size n) n i)))
				(svg-rectangle
				 svg
				 (+ side-size (car pos))
				 (+ side-size (cadr pos))
				 cell-size
				 cell-size
				 :fill (if top
									 (assoc-default (elt top i) my-cubing-colors
																	'eq
																	(assoc-default ?\? my-cubing-colors))
								 (assoc-default ?\? my-cubing-colors))
				 :stroke-width 1
				 :stroke "#666")))
		;; Draw the arrows
		(dolist (arrow arrows)
			(let ((from (car arrow))
						(to (cadr arrow)))
				(apply 'svg-line
							 (append
								(list svg)
								(mapcar (lambda (o) (+ side-size o (/ cell-size 2)))
												(my-cubing-pos (* n cell-size) n from))
								(mapcar (lambda (o) (+ side-size o (/ cell-size 2)))
												(my-cubing-pos (* n cell-size) n to))
								(list
								 :stroke-width 2
								 :stroke arrow-color
								 :opacity 0.5
								 :marker-start (if (elt arrow 2) "url(#arrowhead)")
								 :marker-end "url(#arrowhead)")))))
		(with-temp-buffer
			(svg-print svg)
			(buffer-string))))
;; end of cubing code
;; Diagrams:1 ends here

;; Minecraft
;; :PROPERTIES:
;; :CUSTOM_ID: minecraft
;; :END:

;; https://github.com/rasensuihei/mcf


;; [[file:Sacha.org::#minecraft][Minecraft:1]]
(use-package mcf
	;:quelpa (mcf :fetcher github :repo "sachac/mcf")
	:load-path "~/vendor/mcf"
	:mode ("\\.mcfunction\\'" . mcf-mode)
	;; rcon settings are in my .emacs.secrets file
	:commands (mcf-rcon mcf-mode)
	)
;; Minecraft:1 ends here

;; Speech synthesis (experimental)
;; :PROPERTIES:
;; :CUSTOM_ID: speech-synthesis-experimental
;; :END:


;; [[file:Sacha.org::#speech-synthesis-experimental][Speech synthesis (experimental):1]]
(defvar my-espeak-command "c:/program files (x86)/espeak/command_line/espeak.exe")
(defun my-say (string &optional speed)
  (interactive "MString: ")
  (setq speed (or speed 175))
  (call-process my-espeak-command nil nil nil string "-s" speed))
;; Speech synthesis (experimental):1 ends here



;; and I can view the table by exporting the subtree with HTML using
;; ~org-export-dispatch~ (~C-c C-e C-s h o~). When I add new items, I can
;; use ~C-u C-c C-e~ to reexport the subtree without navigating up to the
;; root.

;; Here's the very rough code I use for that:


;; [[file:Sacha.org::#shopping][Comparison-shopping with Org Mode:2]]
(defvar my-get-shopping-details-functions '(my-get-shopping-details-amazon my-get-shopping-details-uniqlo my-get-shopping-details-manually))

(defun my-get-shopping-details-manually (link)
	(when (string-match "theshoecompany\\|dsw" link)
		(browse-url link)
		(list
		 (cons 'url link)
		 (cons 'image (read-string "Image: "))
		 (cons 'price (read-string "Price: ")))))

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
              (cons 'image (concat "https:" .product.featured_image))
              (cons 'price (/ .product.price 100.0)))))
     ((and (re-search-forward "<script type=\"application/ld\\+json\">" nil t)
           (null (re-search-forward "Fabric Fabric" nil t))) ; Carter's, Columbia?
      (setq data (json-read))
      (if (vectorp data) (setq data (elt data 0)))
      (if (assoc-default '@graph data)
          (setq data (assoc-default '@graph data)))
      (if (vectorp data) (setq data (elt data 0)))
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
(defun my-org-insert-shopping-details ()
  (interactive)
	(save-excursion
		(org-insert-heading)
		(save-excursion (yank))
		(my-org-update-shopping-details)
		(when (org-entry-get (point) "NAME")
			(org-edit-headline (org-entry-get (point) "NAME")))))


;; (my-org-get-shopping-details-uniqlo "https://www.uniqlo.com/ca/en/products/E451023-000?colorCode=COL07&sizeCode=KSS020")
(defun my-org-update-shopping-details ()
  (interactive)
  (when (re-search-forward org-link-any-re (save-excursion (org-end-of-subtree)) t)
    (let* ((link (org-element-property :raw-link (org-element-context)))
           data)
			(setq
			 data
			 (or (run-hook-with-args-until-success 'my-get-shopping-details-functions link)
					 (with-current-buffer (url-retrieve-synchronously link)
						 (my-get-shopping-details))))
			(when data
				(let-alist data
					(org-entry-put (point) "NAME" .name)
					(org-entry-put (point) "URL" link)
					(org-entry-put (point) "BRAND" .brand)
					(org-entry-put (point) "DESCRIPTION" (replace-regexp-in-string "&#039;" "'" (replace-regexp-in-string "\n" " " (or .description ""))))
					(org-entry-put (point) "IMAGE"
												 (if (string-match "https?" .image)
														 .image
													 (concat "https:" .image)))
					(org-entry-put (point) "PRICE" (cond ((stringp .price) .price) ((numberp .price) (format "%.2f" .price)) (t "")))
					(if .rating (org-entry-put (point) "RATING" (if (stringp .rating) .rating (format "%.1f" .rating))))
					(if .ratingCount (org-entry-put (point) "RATING_COUNT" (if (stringp .ratingCount) .ratingCount (number-to-string .ratingCount))))
					)))))

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
;; Comparison-shopping with Org Mode:2 ends here
