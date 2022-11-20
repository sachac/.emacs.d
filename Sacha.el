;; This sets up the load path so that we can override it
(setq warning-suppress-log-types '((package reinitialization)))  (package-initialize)
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp")
(add-to-list 'load-path "~/vendor/org-mode/lisp")
(add-to-list 'load-path "~/vendor/org-mode/contrib/lisp")
(setq custom-file "~/.config/emacs/custom-settings.el")
(setq use-package-always-ensure t)
(load custom-file t)

(defvar my-laptop-p (equal (system-name) "sacha-x220"))
(defvar my-server-p (and (equal (system-name) "localhost") (equal user-login-name "sacha")))
(defvar my-phone-p (not (null (getenv "ANDROID_ROOT")))
  "If non-nil, GNU Emacs is running on Termux.")
(when my-phone-p (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))
(global-auto-revert-mode)  ; simplifies syncing

(setq user-full-name "Sacha Chua"
      user-mail-address "sacha@sachachua.com")

(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))
(unless (assoc-default "nongnu" package-archives)
  (add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t))

(add-to-list 'load-path "~/elisp")
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(setq use-package-verbose t)
(setq use-package-always-ensure t)
(require 'use-package)
(use-package quelpa)
(use-package quelpa-use-package)
(quelpa-use-package-activate-advice)
(use-package auto-compile
  :if my-laptop-p
  :config (auto-compile-on-load-mode))
(setq load-prefer-newer t)

(repeat-mode 1)

(use-package hydra :commands defhydra)
(use-package use-package-hydra)
(if my-laptop-p
        (use-package hydra-posframe :if my-laptop-p  :quelpa (hydra-posframe :fetcher github :repo "Ladicle/hydra-posframe") :after hydra))

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
    ("g" my-geeqie/body "Geeqie")
    ("r" my-record-ffmpeg-toggle-recording "Record screen")
    ("l" (my-toggle-or-create "*scratch*" (lambda () (switch-to-buffer (startup--get-buffer-create-scratch)))) "Lisp")
    ("e" eshell-toggle "Eshell")
    ("w" my-engine-dmode-hydra/body "Search web")
    ("E" my-emacs-news/body "Emacs News"))
  (global-set-key (kbd "<f5>") #'my-shortcuts/body)
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
  (when (org-in-regexp org-bracket-link-regexp 1)
    (goto-char (match-end 0))
    (insert "\n"))
  (call-interactively 'org-insert-link))

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

(defun my-goto-random-char ()
  (interactive)
  (goto-char (random (point-max))))

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

(defun my-hydra-execute-extended (&optional prefixarg hydra-base)
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

(with-eval-after-load 'hydra
  (define-key hydra-base-map (kbd "<tab>") #'my-hydra-execute-extended))

(defun my-reload-emacs-configuration ()
  (interactive)
  (load-file "~/proj/.emacs.d/Sacha.el"))

(use-package dash :ensure t)
(use-package diminish :ensure t)

(setq backup-directory-alist '(("." . "~/.config/emacs/backups")))
(with-eval-after-load 'tramp
(add-to-list 'tramp-backup-directory-alist
             (cons tramp-file-name-regexp nil)))

(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.config/emacs/auto-save-list/" t)))

(setq savehist-file "~/.config/emacs/savehist")
(savehist-mode 1)
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring))

(tool-bar-mode -1)

(display-time-mode 1)

(use-package winner
  :defer t)

(setq sentence-end-double-space nil)

(use-package vertico :config (vertico-mode +1))
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))
(use-package prescient :config (prescient-persist-mode +1))
(use-package company-prescient :init (company-prescient-mode +1))
(use-package consult :quelpa (consult :fetcher github :repo "minad/consult")
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
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-project-imenu)
         ("M-g e" . consult-error)
         ;; M-s bindings (search-map)
         ("M-s f" . consult-find)
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
  :config
  (setq consult-project-root-function #'projectile-project-root)
  (setq consult-narrow-key "<"))
(use-package marginalia :quelpa (marginalia :fetcher github :repo "minad/marginalia")
  :init
  (marginalia-mode)
  :bind (:map minibuffer-local-completion-map
              ("M-A" . marginalia-cycle)
              ("C-i" . marginalia-cycle-annotators)))
(defun my-sketch-insert-file-as-link (f)
  (interactive "fSketch: ")
  (insert (org-link-make-string (concat "sketch:" (file-name-nondirectory f))) "\n"))
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

(use-package embark
  :quelpa (embark :fetcher github :repo "oantolin/embark")
  :config
  (load-library "embark-org")
  (setq embark-prompter 'embark-keymap-prompter)
  (add-to-list 'embark-target-finders 'my-embark-org-element)
  (add-to-list 'embark-target-finders 'my-embark-subed-timestamp)
  (add-to-list 'embark-target-injection-hooks '(my-journal-post embark--allow-edit))
  (embark-define-keymap embark-subed-timestamp-actions
    "Subed timestamp actions"
    ("." my-subed-set-timestamp-to-mpv-position)
    ("c" my-subed-copy-timestamp-dwim)
    ("<up>" my-subed-adjust-timestamp/my-subed-adjust-timestamp-up)
    ("w" my-waveform-subed-show-after-time)
    ("<down>" my-subed-adjust-timestamp/my-subed-adjust-timestamp-down))
  (embark-define-keymap embark-sketch-actions
    "Org Mode sketch-related actions"
    ("o" my-sketch-insert-file-as-link)
    ("v" my-geeqie-view))
  (embark-define-keymap embark-journal-actions
    "Journal"
    ("e" my-journal-edit))
  (add-to-list 'embark-keymap-alist '(sketch . embark-sketch-actions))
  (add-to-list 'embark-keymap-alist '(subed-timestamp . embark-subed-timestamp-actions))
  (add-to-list 'embark-keymap-alist '(journal . embark-journal-actions))
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-act)
   :map minibuffer-local-map
   (("C-c e" . embark-act)
    ("C-;" . embark-act))
   :map embark-collect-mode-map
   (("C-c e" . embark-act)
    ("C-;" . embark-act))
   :map embark-general-map
   (("j" . my-journal-post)
    ("m" . my-stream-message)
    ("M-w" . (lambda (s) (interactive "MString: ") (kill-new s))))
   :map embark-symbol-map
   ("r" . erefactor-rename-symbol-in-buffer)
   :map embark-variable-map
   ("l" . edit-list)
   :map embark-url-map
   ("c" . my-caption-show)))


(use-package
  embark-consult
  :after (embark consult)
  :demand t                ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook (embark-collect-mode . embark-consult-preview-minor-mode))

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

(defun my-date-from-filename (filename)
  (let ((f (file-name-nondirectory filename)))
    (if (string-match "^[-0-9]+" f)
        (replace-regexp-in-string "[^0-9]" "" (match-string 0 f))
      nil)))

(defvar my-sketches nil "Cache for sketch filenames.")
(defun my-update-sketch-cache ()
  (interactive)
  (setq my-sketches (sort
                          (apply 'append (mapcar (lambda (dir)
                                                   (directory-files dir t "\\.\\(jpe?g\\|png\\|svg\\)$"))
                                                 my-sketch-directories))
                          (lambda (a b)
                            (string< (concat (or (my-date-from-filename b) "0") (file-name-nondirectory b))
                                     (concat (or (my-date-from-filename a) "0") (file-name-nondirectory a)) )))))

(defun my-preview-image (candidate state)
  (when (and my-sketch-preview candidate) (my-geeqie-view (list candidate)))
  nil)

(defvar my-sketch-preview nil "Non-nil means preview images.")
(defun my-complete-sketch-filename ()
  (interactive)
  (consult--read (or my-sketches (my-update-sketch-cache))
   :sort nil
   :state 'my-preview-image
   :prompt "Sketch: "
   :category 'sketch))

(use-package marginalia
  :config
  (add-to-list 'marginalia-prompt-categories '("sketch" . sketch)))

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

(defun my-marginalia-annotate-journal (cand)
  (when-let ((o (cdr (assoc cand my-journal-search-cache))))
    (marginalia--fields
     ((plist-get o :Category)
      :face 'marginalia-documentation
      :truncate 13))))

(use-package marginalia
  :after elisp-mode
  :config
  (add-to-list 'marginalia-annotator-registry '(journal my-marginalia-annotate-journal builtin none)))

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

(defun my-complete-blog-post-url ()
  (concat "https://sachachua.com/"
          (replace-regexp-in-string
           "index\\.html$" ""
           (let ((default-directory "~/proj/static-blog/_site"))
             (consult--find "Post: " #'consult--find-builder ".html#")))))

(defun my-edit-blog-post ()
  (interactive)
  (consult-find "~/proj/static-blog/blog/" ".html#"))

(defun my-view-blog-post-locally ()
  (interactive)
  (browse-url
   (concat "http://localhost:8080/"
           (replace-regexp-in-string
            "index\\.html$" ""
            (let ((default-directory "~/proj/static-blog/_site"))
              (consult--find "Post: " #'consult--find-builder ".html#"))))))

(defun my-insert-blog-post-url (url)
  (interactive (list (my-complete-blog-post-url)))
  (insert url))

(defun my-insert-blog-post-link (url)
  (interactive (list (my-complete-blog-post-url)))
  (insert (org-link-make-string url
                                (replace-regexp-in-string
                                 " :: Sacha Chua" ""
                                 (with-current-buffer (url-retrieve-synchronously url)
                                   (dom-text (car
                                              (dom-by-tag (libxml-parse-html-region
                                                           (point-min)
                                                           (point-max))
                                                          'title))))))))

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

(defun my-cmap-org-link-element-target ()
  "Org-mode link target. Returns the element."
  (when (derived-mode-p 'org-mode)
    (let ((context (org-element-context)))
      (when (eq (org-element-type context) 'link)
        (cons 'my-cmap-org-link-element-map context)))))

(defun my-cmap-org-block-target ()
	(when (and (derived-mode-p 'org-mode)
		         (org-in-src-block-p))
	  (cons 'my-cmap-org-block-map 'cmap-no-arg)))
(defun my-org-indent-block ()
	(interactive)
	(save-excursion
	  (unless (looking-at "^[ \t]*#\\+begin")
	    (re-search-backward "^[ \t]*#\\+begin" nil t))
	  (org-indent-block)))
(defun my-org-copy-block-contents ()
	(interactive)
	(kill-new (org-element-property :value (org-element-context))))
(defun my-org-link-element-copy-link (element)
  (interactive (list (org-element-context)))
  (kill-new (org-element-property :raw-link element)))

(use-package cmap :quelpa (cmap :fetcher github :repo "jyp/cmap")
	:config
	(add-to-list 'cmap-targets #'my-cmap-org-block-target)
  (add-to-list 'cmap-targets #'my-cmap-org-link-element-target)
  (defvar my-cmap-org-link-element-map
    (cmap-keymap
      ("w" . my-org-link-element-copy-link)
      ("c" . my-caption-show)))
  (add-to-list 'which-key-replacement-alist '((nil . "^my-org-link-element-") . (nil . "")))
	(defvar my-cmap-org-block-map
	  (cmap-keymap
	    ("w" . my-org-copy-block-contents)
	    ("i" . my-org-indent-block)))
	:bind (("C-c E" . cmap-cmap)))

(use-package helm
  :diminish helm-mode
  :if my-laptop-p
  :config
  (progn
    (require 'helm-config)
    (require 'helm-for-files)
    (setq helm-candidate-number-limit 100)
    (setq helm-completing-read-handlers-alist
          '((describe-function)
            (consult-bookmark)
            (org-refile-get-location)
            (consult-outline)
            (consult-line)
            (org-olpath-completing-read)
            (consult-mark)
            (org-refile)
            (consult-multi-occur)
            (describe-variable)
            (execute-extended-command)
            (consult-yank)))
    ;; From https://gist.github.com/antifuchs/9238468
    (setq helm-idle-delay 0.0 ; update fast sources immediately (doesn't).
          helm-input-idle-delay 0.01  ; this actually updates things
                                        ; reeeelatively quickly.
          helm-yas-display-key-on-candidate t
          helm-quick-update t
          helm-M-x-requires-pattern nil
          helm-ff-skip-boring-files t))
  (defadvice helm-files-insert-as-org-links (around sacha activate)
    (insert (mapconcat (lambda (candidate)
                         (org-link-make-string candidate))
                       (helm-marked-candidates)
                       "\n")))
  :bind (("C-c h" . helm-mini)
         ("C-h a" . helm-apropos)
         ("C-x C-b" . helm-buffers-list)
         ("C-x c o" . helm-occur)
         ("C-x c s" . helm-swoop)
         ("C-x c y" . helm-yas-complete)
         ("C-x c Y" . helm-yas-create-snippet-on-region)
         ("C-x c SPC" . helm-all-mark-rings)))

(defvar my-book-notes-directory "~/Dropbox/books")
(defun my-helm-do-grep-book-notes ()
  "Search my book notes."
  (interactive)
  (helm-do-grep-1 (list my-book-notes-directory)))

(ert-deftest my-org-capture-prefill-template ()
  (should
   ;; It should fill things in one field at ia time
   (string=
    (my-org-capture-prefill-template
     "* TODO %^{Task}\nSCHEDULED: %^t\n:PROPERTIES:\n:Effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}\n:END:\n%?\n"
     "Hello World")
    "* TODO Hello World\nSCHEDULED: %^t\n:PROPERTIES:\n:Effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}\n:END:\n%?\n"
    ))
  (should
   (string=
    (my-org-capture-prefill-template
     "* TODO %^{Task}\nSCHEDULED: %^t\n:PROPERTIES:\n:Effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}\n:END:\n%?\n"
     "Hello World" "<2015-01-01>")
    "* TODO Hello World\nSCHEDULED: <2015-01-01>\n:PROPERTIES:\n:Effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}\n:END:\n%?\n"))
  (should
   (string=
    (my-org-capture-prefill-template
     "* TODO %^{Task}\nSCHEDULED: %^t\n:PROPERTIES:\n:Effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}\n:END:\n%?\n"
     "Hello World" "<2015-01-01>" "0:05")
    "* TODO Hello World\nSCHEDULED: <2015-01-01>\n:PROPERTIES:\n:Effort: 0:05\n:END:\n%?\n")))

(declare-function org-capture-get "org-capture")
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

(defun my-org-get-current-refile-location ()
  "Return the current entry as a location understood by org-refile."
  (interactive)
  (list (elt (org-heading-components) 4)
        (or buffer-file-name
            (with-current-buffer (buffer-base-buffer (current-buffer))
              buffer-file-name))
        nil
        (point)))

(defun my-helm-org-create-task (candidate)
  "Creates the task and returns the location."
  (let ((entry (org-capture-select-template "T")))
    (org-capture-set-plist entry)
    (org-capture-get-template)
    (org-capture-set-target-location)
    (condition-case error
        (progn
          (org-capture-put
           :template
           (org-capture-fill-template
            (my-org-capture-prefill-template (org-capture-get :template)
                                             candidate)))
          (org-capture-place-template
           (equal (car (org-capture-get :target)) 'function))
          (setq org-refile-target-table (org-refile-get-targets))
          ;; Return the new location
          (my-org-get-current-refile-location))
      ((error quit)
       (if (get-buffer "*Capture*") (kill-buffer "*Capture*"))
       (error "Capture abort: %s" error)))))

;; (my-org-refile-get-location-by-substring "Try again")

(defvar my-helm-org-refile-locations nil)
(defvar my-org-refile-last-location nil)

(defun my-helm-org-clock-in-and-track-from-refile (candidate)
  (let ((location (org-refile--get-location candidate my-helm-org-refile-locations)))
    (save-window-excursion
      (org-refile 4 nil location)
      (my-org-clock-in-and-track)
      t)))

(defun my-org-get-todays-items-as-refile-candidates ()
  "Return items scheduled for today, ready for choosing during refiling."
  (delq
   nil
   (mapcar
    (lambda (s)
      (if (get-text-property 0 'org-marker s)
          (list
           s
           (buffer-file-name (marker-buffer (get-text-property 0 'org-marker s)))
           nil
           (marker-position (get-text-property 0 'org-marker s)))))
    (save-window-excursion (my-org-get-entries-fn (calendar-current-date) (calendar-current-date))))))

;; Based on http://emacs.stackexchange.com/questions/4063/how-to-get-the-raw-data-for-an-org-mode-agenda-without-an-agenda-view
(defun my-org-get-entries-fn (begin end)
  "Return org schedule items between BEGIN and END.
         USAGE:  (org-get-entries-fn '(6 1 2015) '(6 30 2015))"
  (require 'calendar)
  (require 'org)
  (require 'org-agenda)
  (require 'cl)
  (unless
      (and
       (calendar-date-is-valid-p begin)
       (calendar-date-is-valid-p end))
    (let ((debug-on-quit nil))
      (signal 'quit `("One or both of your gregorian dates are invalid."))))
  (let* (
         result
         (org-agenda-entry-types '(:scheduled))
         (date-after
          (lambda (date num)
            "Return the date after NUM days from DATE."
            (calendar-gregorian-from-absolute
             (+ (calendar-absolute-from-gregorian date) num))))
         (enumerate-days
          (lambda (begin end)
            "Enumerate date objects between BEGIN and END."
            (when (> (calendar-absolute-from-gregorian begin)
                     (calendar-absolute-from-gregorian end))
              (error "Invalid period : %S - %S" begin end))
            (let ((d begin) ret (cont t))
              (while cont
                (push (copy-sequence d) ret)
                (setq cont (not (equal d end)))
                (setq d (funcall date-after d 1)))
              (nreverse ret)))) )
    (org-agenda-reset-markers)
    (setq org-agenda-buffer
          (when (buffer-live-p org-agenda-buffer)
            org-agenda-buffer))
    (org-compile-prefix-format nil)
    (setq result
          (loop for date in (funcall enumerate-days begin end) append
                (loop for file in (org-agenda-files nil 'ifmode)
                      append
                      (progn
                        (org-check-agenda-file file)
                        (apply 'org-agenda-get-day-entries file date org-agenda-entry-types)))))
    (unless (buffer-live-p (get-buffer org-agenda-buffer-name))
      (get-buffer-create org-agenda-buffer-name))
    (with-current-buffer (get-buffer org-agenda-buffer-name)
      (org-agenda-mode)
      (setq buffer-read-only t)
      (let ((inhibit-read-only t))
        (erase-buffer))
      (mapc
       (lambda (x)
         (let ((inhibit-read-only t))
           (insert (format "%s" x) "\n")))
       result))
    ;;    (display-buffer org-agenda-buffer-name t)
    result))

(defun my-helm-org-refile-read-location (tbl)
  (setq my-helm-org-refile-locations tbl)
  (helm
   (list
    ;; (helm-build-sync-source "Today's tasks"
    ;;   :candidates (mapcar (lambda (a) (cons (car a) a))
    ;;                       (my-org-get-todays-items-as-refile-candidates))
    ;;   :action '(("Select" . identity)
    ;;             ("Clock in and track" . my-helm-org-clock-in-and-track-from-refile)
    ;;             ("Draw index card" . my-helm-org-prepare-index-card-for-subtree))
    ;;   :history 'org-refile-history)
    (helm-build-sync-source "Refile targets"
      :candidates (mapcar (lambda (a) (cons (car a) a)) tbl)
      :action '(("Select" . identity)
                ("Clock in and track" . my-helm-org-clock-in-and-track-from-refile)
                ("Draw index card" . my-helm-org-prepare-index-card-for-subtree))
      :history 'org-refile-history)
    (helm-build-dummy-source "Create task"
      :action (helm-make-actions
               "Create task"
               'my-helm-org-create-task)))))

(defun my-org-refile-get-location (&optional prompt default-buffer new-nodes no-exclude)
  "Prompt the user for a refile location, using PROMPT.
           PROMPT should not be suffixed with a colon and a space, because
           this function appends the default value from
           `org-refile-history' automatically, if that is not empty."
  (let ((org-refile-targets org-refile-targets)
        (org-refile-use-outline-path org-refile-use-outline-path))
    (setq org-refile-target-table
          (org-refile-get-targets default-buffer))
    (unless org-refile-target-table
      (user-error "No refile targets"))
    (let* ((cbuf (current-buffer))
           (partial-completion-mode nil)
           (cfn (buffer-file-name (buffer-base-buffer cbuf)))
           (cfunc (if (and org-refile-use-outline-path
                           org-outline-path-complete-in-steps)
                      'org-olpath-completing-read
                    'org-icompleting-read))
           (extra (if org-refile-use-outline-path "/" ""))
           (cbnex (concat (buffer-name) extra))
           (filename (and cfn (expand-file-name cfn)))
           (tbl (mapcar
                 (lambda (x)
                   (if (and (not (member org-refile-use-outline-path
                                         '(file full-file-path)))
                            (not (equal filename (nth 1 x))))
                       (cons (concat (car x) extra " ("
                                     (file-name-nondirectory (nth 1 x)) ")")
                             (cdr x))
                     (cons (concat (car x) extra) (cdr x))))
                 org-refile-target-table))
           (completion-ignore-case t)
           cdef
           (prompt (concat prompt
                           (or (and (car org-refile-history)
                                    (concat " (default " (car org-refile-history) ")"))
                               (and (assoc cbnex tbl) (setq cdef cbnex)
                                    (concat " (default " cbnex ")"))) ": "))
           pa answ parent-target child parent old-hist)
      (setq old-hist org-refile-history)
      ;; Use Helm's sources instead
      (setq answ (my-helm-org-refile-read-location tbl))
      (cond
       ((and (stringp answ)
             (setq pa (org-refile--get-location answ tbl)))
        (org-refile-check-position pa)
        (when (or (not org-refile-history)
                  (not (eq old-hist org-refile-history))
                  (not (equal (car pa) (car org-refile-history))))
          (setq org-refile-history
                (cons (car pa) (if (assoc (car org-refile-history) tbl)
                                   org-refile-history
                                 (cdr org-refile-history))))
          (if (equal (car org-refile-history) (nth 1 org-refile-history))
              (pop org-refile-history)))
        (setq my-org-refile-last-location pa)
        pa)
       ((and (stringp answ) (string-match "\\`\\(.*\\)/\\([^/]+\\)\\'" answ))
        (setq parent (match-string 1 answ)
              child (match-string 2 answ))
        (setq parent-target (org-refile--get-location parent tbl))
        (when (and parent-target
                   (or (eq new-nodes t)
                       (and (eq new-nodes 'confirm)
                            (y-or-n-p (format "Create new node \"%s\"? "
                                              child)))))
          (org-refile-new-child parent-target child)))
       ((listp answ) answ) ;; Sacha: Helm returned a refile location
       ((not (equal answ t))
        (user-error "Invalid target location"))))))

;(fset 'org-refile-get-location 'my-org-refile-get-location)

(defvar my-screenshot-directory "~/screenshots")
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
  (save-window-excursion
    (with-current-buffer (find-file-noselect file) (display-buffer (current-buffer)))
    (insert "#+CAPTION: " (or note (read-string "Caption: "))))
  (save-excursion (insert "\n" (org-link-make-string (concat "file:" file)) "\n")))
(defun my-copy-last-screenshot-to-file (new-filename)
  (interactive (list (read-file-name (format "Copy %s to: " (file-name-nondirectory (my-latest-file my-screenshot-directory))))))
  (copy-file (my-latest-file my-screenshot-directory) new-filename))

(defun my-copy-last-screenshot-and-insert-into-org (new-filename caption)
  (interactive (list (read-file-name (format "Copy %s to: " (file-name-nondirectory (my-latest-file my-screenshot-directory))))
                     (read-string "Caption: ")))
  (copy-file (my-latest-file my-screenshot-directory) new-filename t)
  (insert "#+CAPTION: " caption "\n"
          (org-link-make-string (concat "file:" (file-relative-name new-filename))) "\n"))

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

(use-package recomplete
  :if my-laptop-p
  :quelpa (recomplete :fetcher gitlab :repo "ideasman42/emacs-recomplete")
  :bind ("M-/" . recomplete-dabbrev))

(fset 'yes-or-no-p 'y-or-n-p)

(use-package miniedit
  :commands minibuffer-edit
  :init (miniedit-install))

(defun my-setup-color-theme ()
  (interactive)
  (when (display-graphic-p)
    (modus-themes-load-vivendi)))
(use-package modus-themes :config (my-setup-color-theme))

(when window-system
  (custom-set-faces
   '(erc-input-face ((t (:foreground "antique white"))))
   '(helm-selection ((t (:background "ForestGreen" :foreground "black"))))
   '(org-agenda-clocking ((t (:inherit secondary-selection :foreground "black"))) t)
   '(org-agenda-done ((t (:foreground "dim gray" :strike-through nil))))
   '(org-done ((t (:foreground "PaleGreen" :weight normal :strike-through t))))
   '(org-clock-overlay ((t (:background "SkyBlue4" :foreground "black"))))
   '(org-headline-done ((((class color) (min-colors 16) (background dark)) (:foreground "LightSalmon" :strike-through t))))
   '(outline-1 ((t (:inherit font-lock-function-name-face :foreground "cornflower blue"))))))

(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)
    (setq undo-tree-history-directory-alist '(("." . "~/.config/emacs/backups/undo-tree")))))

(prefer-coding-system 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

(setq kill-ring-max 1000)

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

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

(use-package helm-descbinds
  :defer t
  :if my-laptop-p
  :bind (("C-h b" . helm-descbinds)
         ("C-h w" . helm-descbinds)))

(use-package sortie :if my-laptop-p :load-path "~/elisp")
(use-package keysee :after sortie :if my-laptop-p :load-path "~/elisp" :commands kc-mode :init (kc-mode))

(use-package which-key :init (which-key-mode 1))
(use-package which-key-posframe :if my-laptop-p :init (which-key-posframe-mode 1))

(defun my-close-other-buffers ()
  (interactive)
  (mapc (lambda (buf)
          (unless (buffer-modified-p buf)
            (kill-buffer buf)))
        (delete (current-buffer)
                (buffer-list))))

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
  ("M-s s" . consult-recoll))

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

(bind-key "C-x p" 'pop-to-mark-command)
(setq set-mark-command-repeat-pop t)

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

(global-hl-line-mode 1)

(use-package windmove
  :bind
  (("<f2> <right>" . windmove-right)
   ("<f2> <left>" . windmove-left)
   ("<f2> <up>" . windmove-up)
   ("<f2> <down>" . windmove-down)
   ))

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
    ("i" "~/sync/orgzly/computer-inbox.org" "Computer inbox")
    ("I" "~/sync/orgzly/Inbox.org" "Phone inbox")
    ("o" "~/sync/orgzly/organizer.org" "Main org file")
    ("s" "~/proj/stream/notes.org" "Public Emacs notes")
    ("b" "~/sync/orgzly/business.org" "Business")
    ("p" "/scp:web:/mnt/prev/home/sacha/planet/en.ini" "Planet Emacsen")
    ("P" "~/proj/static-blog/blog/2022/posts.org" "Posts")
;    ("B" "/ssh:web|sudo::/etc/nginx/sites-available" "Nginx sites")
    ("w" "~/Dropbox/public/sharing/index.org" "Sharing index")
    ("W" "~/Dropbox/public/sharing/blog.org" "Blog index")
    ("1" "~/proj/static-blog/" "Static blog")
    ("r" "~/personal/reviews.org" "Reviews")
    ("g" "~/proj/sachac.github.io/evil-plans/index.org" "Evil plans"))
  :bind
  ("C-c f" . #'my-file-shortcuts/body))

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

(bind-key "C-t" 'my-key-chord-commands/body)

(use-package smartscan
  :if my-laptop-p
  :defer t
  :config (global-smartscan-mode t))

(setq dired-listing-switches "-altr")

(require 'find-dired)
(setq find-ls-option '("-print0 | xargs -0 ls -ld" . "-ld"))

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

(require 'recentf)
(setq recentf-max-saved-items 200
      recentf-max-menu-items 15)
(recentf-mode)

(defun prelude-copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

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

(use-package link-hint
  :bind
  ("M-g u" . link-hint-open-link)
  ("M-g U" . link-hint-open-multiple-links))

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

(defun xah-toggle-margin-right ()
  "Toggle the right margin between `fill-column' or window width.
     This command is convenient when reading novel, documentation."
  (interactive)
  (if (eq (cdr (window-margins)) nil)
      (set-window-margins nil 0 (- (window-body-width) fill-column))
    (set-window-margins nil 0 0)))

(use-package pdf-tools
  :if my-laptop-p
  :config
  (pdf-tools-install)
  (setq pdf-view-resize-factor 1.1)
  (setq-default pdf-view-display-size 'fit-page)
  )

(defun my-shuffle-lines-in-region (beg end)
  (interactive "r")
  (let ((list (split-string (buffer-substring beg end) "[\r\n]+")))
    (delete-region beg end)
    (insert (mapconcat 'identity (shuffle-list list) "\n"))))

(global-set-key (kbd "M-c") #'capitalize-dwim)

(use-package markdown-mode
  :if my-laptop-p
  :mode ("\\.\\(njk\\|md\\)\\'" . markdown-mode))

(defun screenshot-svg ()
  "Save a screenshot of the current frame as an SVG image.
Saves to a temp file and puts the filename in the kill ring."
  (interactive)
  (let* ((filename (format-time-string "~/screenshots/%Y-%m-%d-%H-%M-%S.svg"))
         (data (x-export-frames nil 'svg)))
    (with-temp-file filename
      (insert data))
    (kill-new filename)
    (message filename)))
(global-set-key (kbd "C-c s") #'screenshot-svg)

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

(defun my-unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn
                 (barf-if-buffer-read-only)
                 (list t)))
  (let ((fill-column (point-max)))
    (fill-paragraph nil region)))
(bind-key "M-Q" 'my-unfill-paragraph)

(defun my-fill-or-unfill-paragraph (&optional unfill region)
  "Fill paragraph (or REGION).
        With the prefix argument UNFILL, unfill it instead."
  (interactive (progn
                 (barf-if-buffer-read-only)
                 (list (if current-prefix-arg 'unfill) t)))
  (let ((fill-column (if unfill (point-max) fill-column)))
    (fill-paragraph nil region)))
(bind-key "M-q" 'my-fill-or-unfill-paragraph)

(remove-hook 'text-mode-hook #'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

(defmacro my-insert-unicode (unicode-name)
  `(lambda () (interactive)
     (insert-char (cdr (assoc-string ,unicode-name (ucs-names))))))
(bind-key "C-x 8 s" (my-insert-unicode "ZERO WIDTH SPACE"))
(bind-key "C-x 8 S" (my-insert-unicode "SNOWMAN"))

(bind-key "M-SPC" 'cycle-spacing)

(setq save-abbrevs 'silently)
(bind-key "M-/" 'hippie-expand)

(defun sanityinc/dabbrev-friend-buffer (other-buffer)
  (< (buffer-size other-buffer) (* 1 1024 1024)))
(setq dabbrev-friend-buffer-function 'sanityinc/dabbrev-friend-buffer)

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







(defun my-subed-cut-video (beg end name video-file caption-file)
  (interactive
   (append
    (if (use-region-p)
        (list (point) (mark))
      (list (save-excursion (subed-jump-to-subtitle-id))
            (save-excursion (subed-jump-to-subtitle-end))))
    (list
     (read-string "Name: ")
     (read-file-name "Video: ")
     (read-file-name "Captions: "))))
  (let*
      ((msecs (my-subed-get-region-start-stop beg end))
       (new-file name)
       (mute (my-subed-get-mute-segments))
       cmd)
    (when (> (length name) 0)
      (setq cmd
            (format "ffmpeg -y -i %s -i %s -ss %s -t %s %s -c:v copy -c:s copy -shortest -async 1 %s"
                    (shell-quote-argument caption-file)
                    (shell-quote-argument video-file)
                    (my-msecs-to-timestamp
                     (car msecs))
                    (my-msecs-to-timestamp
                     (-
                      (cdr msecs)
                      (car msecs)))
                    (if mute
                        (format "-af %s"
                                (shell-quote-argument
                                 (my-subed-ffmpeg-make-mute-filter mute)))
                      "-c:a copy")
                    (shell-quote-argument new-file)))
      (message "%s" cmd)
      (kill-new cmd))))

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
  :bind
  (:map subed-mode-map
        ("M-j" . avy-goto-char-timer)
        ("M-j" . subed-mpv-jump-to-current-subtitle)
        ("M-[" . subed-mpv-seek))
  :hook
  ((subed-mode . subed-disable-sync-point-to-player)
   (subed-mode . subed-disable-sync-player-to-point)
   (subed-mode . subed-disable-loop-over-current-subtitle)
   (subed-mode . save-place-local-mode)
   (subed-mode . turn-on-auto-fill)
   (subed-mode . (lambda () (setq-local fill-column 40)))
   (subed-mode . (lambda () (remove-hook 'before-save-hook 'subed-sort t)))))
(use-package subed-record :load-path "~/proj/subed-record"
  :bind
  (:map subed-mode-map ("C-c C-c" . subed-record-compile-try-flow)))

(defun my-subed-fix-timestamps ()
  "Change all ending timestamps to the start of the next subtitle."
  (interactive)
  (goto-char (point-max))
  (let ((timestamp (subed-subtitle-msecs-start)))
    (while (subed-backward-subtitle-time-start)
      (subed-set-subtitle-time-stop timestamp)
      (setq timestamp (subed-subtitle-msecs-start)))))

(defun my-caption-download-srv2 (id)
  (interactive "MID: ")
  (require 'subed-word-data)
  (when (string-match "v=\\([^&]+\\)" id) (setq id (match-string 1 id)))
  (let ((default-directory "/tmp"))
    (call-process "yt-dlp" nil nil nil "--write-auto-sub" "--write-sub" "--no-warnings" "--sub-lang" "en" "--skip-download" "--sub-format" "srv2"
                  (concat "https://youtu.be/" id))
    (subed-word-data-load-from-file (my-latest-file "/tmp" "\\.srv2\\'"))))

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
  (autoload 'subed-align "subed-align" nil t)
  :hook
  (subed-mode . display-fill-column-indicator-mode)
  (subed-mode . subed-avy-set-up-actions)
  :bind
  (:map subed-mode-map
        ("M-," . subed-split-subtitle)
        ("M-." . subed-merge-dwim)))

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
  (with-current-buffer-window "*Captions*"
      'display-buffer-same-window
      nil
    (org-mode)
    (save-excursion
      (my-org-insert-youtube-video-with-transcript url))))

(defvar my-subed-common-edits '("I"
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
                                "Elisp" "Reddit" "TextMate" "macOS" "API" "IntelliSense"
                                ("EXWM" "axwm")
                                ("Emacs's" "emax's")

                                ("BIDI" "bd")
                                ("Perso-Arabic" "personal arabic")
                                "Persian"
                                "URL"
                                "HTML")
  "List of words and replacements.")

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

(use-package waveform :load-path "~/proj/waveform-el")
(use-package subed-waveform :load-path "~/proj/subed-waveform")
(use-package compile-media :load-path "~/proj/compile-media")

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

(defvar my-transcript-dir "~/sync/Phone")
(defun my-open-latest-transcript ()
  (interactive)
  (find-file (my-latest-file my-transcript-dir "\\.txt"))
  (kill-new (buffer-string)))

(defun my-insert-latest-transcript ()
  (interactive)
  (insert-file-contents (my-latest-file my-transcript-dir "\\.txt")))

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

(defun my-plover-briefpedia (translation)
  (interactive "MTranslation: ")
  (with-current-buffer (url-retrieve-synchronously (concat "http://briefpedia.com/AjaxTables.php?search=" (url-encode-url translation)))
    (goto-char (point-min))
    (re-search-forward "^$")
    (save-excursion
      (insert "<div>")
      (goto-char (point-max)) (insert "</div>"))
    (let* ((data (xml-parse-region (point) (point-max)))
           (entries (mapcar (lambda (o) (string-trim (dom-text o))) (dom-by-tag (dom-by-id data "divEnglishTable") 'a)))
           (conflicts (seq-group-by 'car
                                     (mapcar (lambda (row) (mapcar (lambda (cell) (string-trim (dom-texts cell))) (dom-by-tag row 'td)))
                                             (cdr (dom-by-tag (dom-by-id data "divCrossTable") 'tr))))))
      (mapcar (lambda (entry) (cons entry (mapcar 'cadr (assoc-default entry conflicts)))) entries))))

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

(defun my-insert-variable (symbol-name)
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
  (insert symbol-name))

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

(use-package typing-speed :if my-laptop-p :load-path "~/elisp"
  :config (setq typing-speed-window 120))

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
  :config
  ;(add-to-list 'company-backends 'my-company-strokedict)
  )

(defvar my-scan-directory "~/sync/scans/")
  (defvar my-ipad-directory "~/sync/ipad")
  (defvar my-portfolio-directory "~/sync/portfolio")
  (defvar my-camera-directory "~/sync/camera")
  (defvar my-private-sketches-directory "~/cloud/private-sketches")
  (defvar my-sketches-directory "~/sync/sketches")

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
  (defun my-copy-and-link-latest-download ()
    (interactive)
    (org-attach-attach (my-latest-file "~/Downloads") nil 'cp)
    (org-insert-link nil (caar org-stored-links)))



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

(setq org-export-with-sub-superscripts nil)
(setq org-fold-catch-invisible-edits 'smart)

(use-package org
  :load-path ("~/vendor/org-mode/lisp" "~/vendor/org-mode/contrib/lisp")
  :bind
  (:map org-mode-map
        ("C-M-<return>" . org-insert-subheading))
  :config
  (require 'oc-basic)                   ; started needing this
  (unless (functionp 'org-link-make-string)
    (fset 'org-link-make-string 'org-make-link-string))
  )

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

(bind-key "C-c r" 'org-capture)
(bind-key "C-c a" 'org-agenda)
(bind-key "C-c l" 'org-store-link)
(bind-key "C-c L" 'org-insert-link-global)
(bind-key "C-c O" 'org-open-at-point-global)

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

(with-eval-after-load 'org-agenda
  (bind-key "i" 'org-agenda-clock-in org-agenda-mode-map))

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
                     (or (org-entry-get (point) "JOURNAL_CAT") (my-journal-read-category))))
  (my-org-with-current-task
   (org-todo "DONE")
   (org-entry-put (point) "JOURNAL_CAT" category)
   (let* ((title (or note (org-get-heading t t t t)))
          (zid (org-entry-get (point) "ZIDSTRING"))
          (other (substring-no-properties (my-org-subtree-text)))
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

(setq org-goto-interface 'outline
      org-goto-max-level 10)
(require 'imenu)
(setq org-startup-folded nil)
(bind-key "C-c j" 'org-clock-goto) ;; jump to current task from anywhere
(bind-key "C-c C-w" 'org-refile)
(setq org-cycle-include-plain-lists 'integrate)
(setq org-catch-invisible-edits 'show-and-error)

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

(with-eval-after-load 'org
  (bind-key "C-c k" 'org-cut-subtree org-mode-map)
  (setq org-yank-adjusted-subtrees t))

(defun my-org-back-to-heading ()
  (interactive)
  (org-back-to-heading))

(use-package org
  :bind (:map org-mode-map
              ("C-c b" . my-org-back-to-heading)
              ("C-c p" . org-display-outline-path)))

(defun my-org-show-row-and-column (point)
  (interactive "d")
  (save-excursion
    (goto-char point)
    (let ((row (s-trim (org-table-get nil 1)))
          (col (s-trim (org-table-get 1 nil)))
          (message-log-max nil))
      (message "%s - %s" row col))))

(setq org-directory "~/sync/orgzly/")
(setq org-default-notes-file "~/sync/orgzly/organizer.org")

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
(defvar my-org-inbox-file "~/sync/orgzly/Inbox.org")
(defvar my-ledger-file "~/cloud/ledger/current.ledger")
(setq org-capture-templates
      `(("t" "Quick task" entry
         (file ,my-org-inbox-file)
         "* TODO %^{Task}\n:PROPERTIES:\n:CREATED: %U\n:END:\n%a\n"
         :immediate-finish t :prepend t)
        ;; From https://takeonrules.com/2022/10/16/adding-another-function-to-my-workflow/
        ("c" "Contents to current clocked task"
	       plain (clock)
	       "%i%?"
	       :empty-lines 1)
        ;; ("p" "Podcast log - timestamped" item
        ;;  (file+olp+datetree "~/sync/orgzly/timestamped.org")
        ;;  "%<%H:%M:%S,%3N> %^{Note}"
        ;;  :immediate-finish t)
        ("b" "Plover note" table-line
         (file+headline "~/proj/plover-notes/README.org" "Brief notes")
         "| %^{Stroke} | %^{Translation} | %^{Note} |"
         :immediate-finish t)
        ;; ("c" "Plover review from clippy" table-line
        ;;  (file+headline "~/proj/plover-notes/README.org" "For review")
        ;;  "%(let ((last (my-clippy-last))) (format \"| %s | %s |\" (car last) (cdr last)))"
        ;;  :immediate-finish t)
        ("T" "Task" entry
         (file ,my-org-inbox-file)
         "* TODO %^{Task}\n:PROPERTIES:\n:CREATED: %U\n:END:\n")
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
        ("r" "Note" entry
         (file ,my-org-inbox-file)
         "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n - %a"
         :prepend t)
        ("N" "Note" entry
         (file ,my-org-inbox-file)
         "* %^{Note}\n:PROPERTIES:\n:CREATED: %U\n:END:\n"
         :prepend t)
        ("i" "Interrupting task" entry
         (file ,my-org-inbox-file)
         "* STARTED %^{Task}\n:PROPERTIES:\n:CREATED: %U\n:END:\n"
         :clock-in :clock-resume
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
:END:")))
(bind-key "C-M-r" 'org-capture)

(setq org-contacts-files '("~/sync/orgzly/people.org"))

;;(bind-key (kbd "<f5>") 'org-capture)

(defun my-org-refile-and-jump ()
  (interactive)
  (if (derived-mode-p 'org-capture-mode)
      (org-capture-refile)
    (call-interactively 'org-refile))
  (org-refile-goto-last-stored))
(eval-after-load 'org-capture
  '(bind-key "C-c C-r" 'my-org-refile-and-jump org-capture-mode-map))

(setq org-reverse-note-order nil)
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)
(setq org-refile-use-cache t)
(setq org-refile-targets '((("~/sync/orgzly/organizer.org"
                             "~/sync/orgzly/routines.org"
                             "~/sync/orgzly/business.org"
                             "~/sync/orgzly/decisions.org"
                             "~/proj/static-blog/blog/2022/posts.org"
                             "~/sync/orgzly/people.org"
                             "~/sync/orgzly/Inbox.org"
                             "~/proj/emacsconf/wiki/2022/organizers-notebook/index.org") . (:maxlevel . 5))))
(setq org-blank-before-new-entry nil)

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

(defun my-org-jump ()
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively 'org-refile)))

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

(setq org-todo-keyword-faces
      '(("TODO" . (:foreground "green" :weight bold))
        ("DONE" . (:foreground "cyan" :weight bold))
        ("WAITING" . (:foreground "red" :weight bold))
        ("SOMEDAY" . (:foreground "gray" :weight bold))))

(setq org-log-done 'time)

(setq org-tags-exclude-from-inheritance '("project"))

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

(with-eval-after-load 'org
  (let ((listvar (if (boundp 'org-speed-commands) 'org-speed-commands
                   'org-speed-commands-user)))
    (add-to-list listvar '("S" call-interactively 'org-sort))))

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

(add-to-list 'org-global-properties
             '("Effort_ALL". "0:05 0:15 0:30 1:00 2:00 3:00 4:00"))

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

(setq org-log-into-drawer "LOGBOOK")
(setq org-clock-into-drawer 1)

(setq org-habit-graph-column 80)
(setq org-habit-show-habits-only-for-today nil)

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

;; Get this from https://raw.github.com/chenfengyuan/elisp/master/next-spec-day.el
(load "~/elisp/next-spec-day.el" t)

(setq org-enforce-todo-dependencies t)
(setq org-track-ordered-property-with-tag t)
(setq org-agenda-dim-blocked-tasks t)

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

(setq org-structure-template-alist
      '(("a" . "export ascii")
        ("c" . "center")
        ("C" . "comment")
        ("e" . "example")
        ("E" . "export")
        ("m" . "export md")
        ("h" . "export html")
        ("l" . "src emacs-lisp")
        ("p" . "src python")
        ("q" . "quote")
        ("s" . "src")
        ("v" . "verse")))

(defun my-org-html-quote2 (block backend info)
  (when (org-export-derived-backend-p backend 'html)
    (when (string-match "\\`<div class=\"quote2\">" block)
      (setq block (replace-match "<blockquote>" t nil block))
      (string-match "</div>\n\\'" block)
      (setq block (replace-match "</blockquote>\n" t nil block))
      block)))
(eval-after-load 'ox
  '(add-to-list 'org-export-filter-special-block-functions 'my-org-html-quote2))

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

(defvar my-kid-org-file nil "Defined in secrets")
(setq org-agenda-files
      (delq nil
            (mapcar (lambda (x) (and x (file-exists-p x) x))
                    `("~/sync/orgzly/organizer.org"
                      "~/sync/orgzly/Inbox.org"
                      "~/sync/orgzly/decisions.org"
                      "~/sync/orgzly/computer-inbox.org"
                      "~/proj/stream/index.org"
                      "~/proj/stream/notes.org"
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

(bind-key "Y" 'org-agenda-todo-yesterday org-agenda-mode-map)

(setq org-agenda-start-on-weekday 6)

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

(use-package org-super-agenda)
(use-package org-ql)
(defun my-org-projects ()
  (interactive)
(org-ql-search (org-agenda-files)
  '(and (todo "TODO" "WAITING") (ancestors (tags "project")))
  :super-groups '((:auto-parent t))))

(setq org-agenda-custom-commands
      `(("a" "Agenda"
         ((agenda "" ((org-agenda-span 2)))
          (alltodo
           ""
           ((org-agenda-overriding-header "")
            (org-super-agenda-groups
             '((:name "Inbox, unscheduled"
                      :and (:scheduled nil
                                  :file-path "Inbox.org"
                                  )
                      :order 1)
               (:name "Important, unscheduled"
                      :and (:priority "A"
                                      :scheduled nil)
                      :order 2)

               (:name "Project-related, unscheduled"
                      :and (:tag "project" :date nil :todo ("STARTED" "WAITING" "TODO"))
                      :order 3)
               (:name "Waiting"
                      :and (:todo "WAITING"
                                  :scheduled nil)
                      :order 4)
               (:discard (:todo "SOMEDAY"
                                :category "cooking"
                                :date t))
               (:name "Unscheduled"
                      :scheduled nil
                      :order 5)
               (:discard (:anything t))
               )
             )))
          ;; (tags-todo "TODO=\"TODO\"-project-cooking-routine-errands-shopping-video-evilplans"
          ;;            ((org-agenda-skip-function 'my-org-agenda-skip-scheduled)
          ;;             (org-agenda-prefix-format "%-6e ")
          ;;             (org-agenda-overriding-header "Unscheduled TODO entries: ")
          ;;             (org-agenda-sorting-strategy '(priority-down effort-up tag-up category-keep))))
          ))
        ("e" "Emacs" (tags "emacs"))
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
        ("x" "Column view" todo ""  ; Column view
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
        ("r" "Unscheduled, untagged tasks" tags-todo "-someday-TODO=\"SOMEDAY\"-TODO=\"DELEGATED\"-TODO=\"WAITING\"-project-cooking-routine-evilplans-computer-writing-phone-sewing-home-errands-shopping"
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

(setq org-complete-tags-always-offer-all-agenda-tags t)
(setq org-use-fast-tag-selection nil)

(defun my-org-agenda-done (&optional arg)
  "Mark current TODO as done.
       This changes the line at point, all other lines in the agenda referring to
       the same tree node, and the headline of the tree node in the Org-mode file."
  (interactive "P")
  (org-agenda-todo "DONE"))
;; Override the key definition for org-exit
(define-key org-agenda-mode-map "x" 'my-org-agenda-done)

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

(defun my-org-agenda-new ()
  "Create a new note or task at the current agenda item.
       Creates it at the same level as the previous task, so it's better to use
       this with to-do items than with projects or headings."
  (interactive)
  (org-agenda-switch-to)
  (org-capture 0))
;; New key assignment
(define-key org-agenda-mode-map "N" 'my-org-agenda-new)

(setq org-agenda-sorting-strategy
      '((agenda time-up priority-down tag-up category-keep)
        ;; (todo user-defined-up todo-state-up priority-down effort-up)
        (todo todo-state-up priority-down effort-up)
        (tags user-defined-up)
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

(defun my-org-show-active-projects ()
  "Show my current projects."
  (interactive)
  (org-tags-view nil "project-inactive-someday"))

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
      (while (re-search-forward org-bracket-link-regexp nil t)
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
      (while (re-search-forward org-any-link-re nil t)
        (save-excursion
          (backward-char)
          (org-open-at-point))))))

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

(defun my-org-move-line-to-end-of-list ()
  "Move the current list item to the end of the list."
  (interactive)
  (save-excursion
    (let ((string (buffer-substring-no-properties (line-beginning-position)
                                                  (line-end-position))))
      (delete-region (line-beginning-position) (1+ (line-end-position)))
      (org-end-of-item-list)
      (insert string))))

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

(defvar my-org-last-refile-marker nil "Marker for last refile")
(defun my-org-refile-in-file (&optional prefix)
  "Refile to a target within the current file."
  (require 'helm-org)
  (interactive)
  (let ((helm-org-headings-actions
         '(("Refile to this heading" . helm-org--refile-heading-to))))
    (save-excursion
      (helm-org-in-buffer-headings)
      (org-end-of-subtree t)
      (setq my-org-last-refile-marker (point-marker)))))

(defun my-org-refile-to-previous ()
  "Refile subtree to last position from `my-org-refile-in-file'."
  (interactive)
  (save-selected-window
    (when (eq major-mode 'org-agenda-mode)
      (org-agenda-switch-to))
    (org-cut-subtree)
    (save-excursion
      (let* ((marker my-org-last-refile-marker)
             (target-level
              (with-current-buffer (marker-buffer marker)
                (goto-char (marker-position marker))
                (org-current-level))))
        (helm-org-goto-marker marker)
        (org-end-of-subtree t t)
        (org-paste-subtree target-level)))))

(with-eval-after-load 'org
  (let ((listvar (if (boundp 'org-speed-commands) 'org-speed-commands
                   'org-speed-commands-user)))
    (add-to-list listvar '("w" call-interactively 'org-refile))
    (add-to-list listvar '("W" call-interactively 'my-org-refile-in-file))
    (add-to-list listvar '("." call-interactively 'my-org-refile-to-previous))))

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

(use-package org
  :hook (org-mode . org-indent-mode)
  :config
  (setq org-indent-indentation-per-level 2)
  (setq org-edit-src-content-indentation 0)
  (setq org-src-preserve-indentation t))

(setq org-babel-default-header-args
      '((:session . "none")
        (:results . "drawer replace")
        (:exports . "code")
        (:cache . "no")
        (:eval . "never-export")
        (:hlines . "no")
        (:tangle . "no")))
(setq org-edit-src-auto-save-idle-delay 5)

(defun my-org-execute-src-block-by-name (name)
  (interactive (list (completing-read "Block: "(org-babel-src-block-names))))
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (format "^#\\+NAME:[ \t]+%s[ \t]*$" (regexp-quote name)) nil t)
      (org-babel-execute-src-block))))

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

(use-package literate-elisp :if my-laptop-p)

(use-package ox-epub
  :if my-laptop-p
  :config
  (setq org-epub-style-default (concat org-epub-style-default "\n  p.my-verse { white-space: pre }\n")))

(defun my-org-export-filter-body-add-emacs-configuration-link (string backend info)
  (when (and (plist-get info :input-file) (string-match "\\.emacs\\.d/Sacha\\.org" (plist-get info :input-file)))
    (concat string
            (let ((id (org-entry-get-with-inheritance "CUSTOM_ID")))
              (format
               "\n<div class=\"note\">This is part of my <a href=\"https://sachachua.com/dotemacs%s\">Emacs configuration.</a></div>"
               (if id (concat "#" id) ""))))))

(use-package org
  :config
  (with-eval-after-load 'ox
    (add-to-list 'org-export-filter-body-functions #'my-org-export-filter-body-add-emacs-configuration-link)))

(use-package ox-11ty
  :if my-laptop-p
  :load-path "~/proj/ox-11ty")
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
      (browse-url (concat "http://localhost:8080" (plist-get data :permalink))))))

(defun my-org-11ty-find-file ()
  (interactive)
  (find-file (expand-file-name
              (concat (org-entry-get-with-inheritance "EXPORT_ELEVENTY_FILE_NAME")
                      (if (string-match "/$" (org-entry-get-with-inheritance "EXPORT_ELEVENTY_FILE_NAME"))
                          "index" "")
                      ".html")
              "~/proj/static-blog")))

(defun my-org-11ty-post-to-mastodon (&optional post-automatically)
  (interactive (list current-prefix-arg))
  (let ((message (concat (org-entry-get (point) "ITEM") " https://sachachua.com" (org-entry-get (point) "EXPORT_ELEVENTY_PERMALINK"))))
    (if post-automatically
        (my-mastodon-toot-public-string message)
      (mastodon-toot)
      (insert message))))

(setq org-export-with-section-numbers nil)
(setq org-html-include-timestamps nil)
(setq org-export-with-sub-superscripts nil)
(setq org-export-with-toc nil)
(setq org-html-toplevel-hlevel 2)
(setq org-export-htmlize-output-type 'css)
(setq org-export-with-broken-links t)
(setq org-ascii-text-width 10000)

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

(defun my-org-publish-maybe ()
  (require 'ox-publish)
  (interactive)
  (save-excursion
    (if (org-publish-get-project-from-filename
         (buffer-file-name (buffer-base-buffer)) 'up)
        (org-publish-current-file t)
      (my-org-html-export-trustingly))))

(defun my-org-publish-and-browse ()
  (interactive)
  (save-buffer)
  (my-org-publish-maybe)
  (browse-url (org-export-output-file-name ".html" nil default-directory)))
(bind-key "<apps> b" 'my-org-publish-and-browse)

(defun my-org-html-export-trustingly ()
  (interactive)
  (let ((org-confirm-babel-evaluate nil))
    (org-html-export-to-html)))

(defun my-org-html-publish-to-html-trustingly (plist filename pub-dir)
  (let ((org-confirm-babel-evaluate nil))
    (org-html-publish-to-html plist filename pub-dir)))

(use-package org-special-block-extras
  :if my-laptop-p
  :hook (org-mode . org-special-block-extras-mode)
  :config
  ;; Use short names like ‘defblock’ instead of the fully qualified name
  ;; ‘org-special-block-extras--defblock’
  (o-defblock my_details (title "Details") (title-color "Green")
              "Top level (HTML & 11ty)OSPE-RESPECT-NEWLINES? Enclose contents in a folded up box."
              (cond
               ((eq backend '11ty)
                (format
                 "{%% details \"%s\"%%}\n%s\n{%% enddetails %%}"
                 title contents))
               ((eq backend 'html)
                (format
                 "<details class=\"code-details\"
                 style =\"padding: 1em;
                          border-radius: 15px;
                          font-size: 0.9em;
                          box-shadow: 0.05em 0.1em 5px 0.01em  #00000057;\">
                  <summary>
                    <strong>
                      <font face=\"Courier\" size=\"3\" color=\"%s\">
                         %s
                      </font>
                    </strong>
                  </summary>
                  %s
               </details>"
                 title-color title contents))))

  (o-defblock columns nil nil
              "Top level (HTML & wp & 11ty)OSPE-RESPECT-NEWLINES? Split into columns using Foundation."
              (format "<div class=\"row\">%s</div>" contents))
  (o-defblock column50 nil nil
              "Top level (HTML & wp & 11ty)OSPE-RESPECT-NEWLINES? Split into columns."
              (format "<div class=\"columns small-12 medium-6 large-6\">%s</div>" contents))
)

(setq org-html-head "
       <link rel=\"stylesheet\" type=\"text/css\" href=\"https://sachachua.com/assets/css/style.css\"></link>
       <link rel=\"stylesheet\" type=\"text/css\" href=\"https://sachachua.com/assets/css/org-export.css\"></link>
       <script src=\"https://ajax.googleapis.com/ajax/libs/jquery/1.11.0/jquery.min.js\"></script>
       <script src=\"https://sachachua.com/assets/js/misc.js\"></script>")
(setq org-html-htmlize-output-type 'css)
(setq org-src-fontify-natively t)

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

(defun my-org-copy-region-as-html (beg end &optional level)
  "Make it easier to copy code for Wordpress posts and other things."
  (interactive "r\np")
  (let ((org-export-html-preamble nil)
        (org-html-toplevel-hlevel (or level 3)))
    (kill-new
     (org-export-string-as (buffer-substring beg end) 'html t))))

(defun my-org-copy-subtree-as-html ()
  (interactive)
  (my-org-copy-region-as-html
   (org-back-to-heading)
   (org-end-of-subtree)))

(setq org-html-checkbox-type 'unicode)
(setq org-html-checkbox-types
      '((unicode (on . "<span class=\"task-done\">&#x2611;</span>")
                 (off . "<span class=\"task-todo\">&#x2610;</span>")
                 (trans . "<span class=\"task-in-progress\">[-]</span>"))))

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

(setq org-plantuml-jar-path (expand-file-name "/usr/share/plantuml/plantuml.jar"))
(add-to-list 'org-src-lang-modes '("plantuml" . plantuml))

(use-package ox-hugo
  :ensure t            ;Auto-install the package from Melpa (optional)
  :after ox)

(setq org-export-async-init-file "~/.config/emacs/org-async-export-config.el")
(setq org-export-async-debug t)

(defmacro my-org-debounce-idle-timer (seconds var body &rest args)
  `(progn
     (defvar ,var nil "Timer.")
     (when (timerp ,var) (cancel-timer ,var))
     (setq ,var (run-with-idle-timer ,seconds nil ,body ,@args))))
(defvar my-unfocusing nil "Non-nil when I'm in the middle of unfocusing.")
(defun my-org-async-export-and-tangle ()
  (async-start
   `(lambda ()
      ;; make async emacs aware of packages (for byte-compilation)
      (package-initialize)
      (setq package-enable-at-startup nil)
      (require 'org)
      (org-babel-tangle-file ,(buffer-file-name))
      )
   (lambda (&rest results) (message "Tangled.")))
  (org-html-export-to-html t))
(defun my-org-export-and-tangle-if-saved-in-focus ()
  (when (frame-focus-state)
    (message "Scheduling export...")
    (my-org-debounce-idle-timer 10
                                my-export-org-config
                                (lambda (buf)
                                  (with-current-buffer buf
                                    (my-org-async-export-and-tangle)))
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

(use-package org
  :hook ((org-mode .
                   (lambda () (when (string= (buffer-file-name) (expand-file-name "~/sync/emacs/Sacha.org")) (my-org-export-and-tangle-when-saved-in-focus-mode 1))))))

(defun org-repair-export-blocks ()
  "Repair export blocks and INCLUDE keywords in current buffer."
  (when (eq major-mode 'org-mode)
    (let ((case-fold-search t)
          (back-end-re (regexp-opt
                        '("HTML" "ASCII" "LATEX" "ODT" "MARKDOWN" "MD" "ORG"
                          "MAN" "BEAMER" "TEXINFO" "GROFF" "KOMA-LETTER")
                        t)))
      (org-with-wide-buffer
       (goto-char (point-min))
       (let ((block-re (concat "^[ \t]*#\\+BEGIN_" back-end-re)))
         (save-excursion
           (while (re-search-forward block-re nil t)
             (let ((element (save-match-data (org-element-at-point))))
               (when (eq (org-element-type element) 'special-block)
                 (save-excursion
                   (goto-char (org-element-property :end element))
                   (save-match-data (search-backward "_"))
                   (forward-char)
                   (insert "EXPORT")
                   (delete-region (point) (line-end-position)))
                 (replace-match "EXPORT \\1" nil nil nil 1))))))
       (let ((include-re
              (format "^[ \t]*#\\+INCLUDE: .*?%s[ \t]*$" back-end-re)))
         (while (re-search-forward include-re nil t)
           (let ((element (save-match-data (org-element-at-point))))
             (when (and (eq (org-element-type element) 'keyword)
                        (string= (org-element-property :key element) "INCLUDE"))
               (replace-match "EXPORT \\1" nil nil nil 1)))))))))
(with-eval-after-load 'org
  (add-to-list 'org-mode-hook 'org-repair-export-blocks))

(setq org-link-abbrev-alist
      '(("google" . "http://www.google.com/search?q=")
        ("gmap" . "http://maps.google.com/maps?q=%s")
        ("blog" . "http://sachachua.com/blog/p/")))

(defvar yt-iframe-format
  ;; You may want to change your width and height.
  (concat "<iframe width=\"440\""
          " height=\"335\""
          " src=\"https://www.youtube.com/embed/%s\""
          " frameborder=\"0\""
          " allowfullscreen>%s</iframe>"))

(org-add-link-type
 "yt"
 (lambda (handle)
   (browse-url
    (concat "https://www.youtube.com/embed/"
            handle)))
 (lambda (path desc backend)
   (cl-case backend
     (html (format yt-iframe-format
                   path (or desc "")))
     (latex (format "\href{%s}{%s}"
                    path (or desc "video"))))))

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

(defun my-org-elisp-link-export (link description format &optional arg)
  (cond
   ((eq format 'html) (format "<span title=\"%s\">%s</span>" (replace-regexp-in-string "\"" "&quot;" link) description))
   ((eq format 'text) description)
   ))
(org-link-set-parameters
 "elisp"
 :export 'my-org-elisp-link-export)

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

(defun my-make-slug (s)
  (thread-last s
    (downcase)
    (replace-regexp-in-string "[^a-z0-9]+" "-")
    (replace-regexp-in-string "^-\\|-$" "")))
(defun my-org-set-custom-id (id)
  "Set the CUSTOM_ID property to ID at point."
  (interactive (list
                (let ((default-custom-id (my-make-slug (elt (org-heading-components) 4))))
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
  (global-set-key (kbd "<f14>") 'my-hydra/dwim))

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

(defun my-journal-post (note &rest plist)
  (interactive (list (read-string "Note: ")
                           :Date (concat (org-read-date "Date: ") " 23:00")
                     :Category (my-journal-read-category)
                     :Other (read-string "Other: ")))
  (setq plist (append `(:Note ,note) plist))
  (let ((url-request-method "POST")
        (url-request-extra-headers '(("Content-Type" . "application/json")))
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

(defun my-journal-get-entries (from to &optional search)
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

(defun my-journal-get (url) (my-json-request (concat my-journal-url "/" url)))
(defun my-journal-get-entry (zid) (my-journal-get (format "api/entries/zid/%s" zid)))

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

(use-package org-attach
  :ensure nil
  :config
  (setq org-attach-store-link-p 'attached)
  (setq org-attach-auto-tag nil))

(use-package ob-http)

(use-package lilypond-init
  :if my-laptop-p
  :load-path "~/vendor/lilypond/elisp"
  :config
  (setq org-babel-lilypond-arrange-mode t
        org-babel-lilypond-commands '("lilypond" "timidity" "timidity")
        org-babel-lilypond-gen-pdf nil
        org-babel-lilypond-display-pdf-post-tangle nil)
  :mode ("\\.ly\\'" . LilyPond-mode))

(setq org-ditaa-jar-path "c:/sacha/Dropbox/bin/ditaa.jar")
(setq org-startup-with-inline-images t)
(use-package org-contrib)
(use-package org
  :config
  (progn
    (add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
    (setq org-confirm-babel-evaluate nil)
    (setq org-link-elisp-confirm-function
          (lambda (prompt)
            (if (string-match "vendor" (buffer-file-name))
                (y-or-n-p prompt)
              t)))
    (require 'ob-ledger)
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((dot . t)
       (ditaa . t)
       (emacs-lisp . t)
       (plantuml . t)
       (lilypond . t)
       (python . t)
       (shell . t)
       (calc . t)
       (js . t)
       (sqlite . t)
       (http . t)
       (ledger . t)
       (shell . t)
       (R . t)))
    (setq org-babel-python-command "python3")
    (setq python-shell-interpreter "python3")
    (add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))))

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

(defun my-org-days-between (start end)
  "Number of days between START and END (exclusive).
      This includes START but not END."
  (- (calendar-absolute-from-gregorian (org-date-to-gregorian end))
     (calendar-absolute-from-gregorian (org-date-to-gregorian start))))

(setq org-src-window-setup 'current-window)

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

(defun my-org-table-as-alist (table)
  "Convert TABLE to an alist. Remember to set :colnames no."
  (let ((headers (seq-map 'intern (car table))))
    (cl-loop for x in (cdr table) collect (-zip headers x))))

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

(use-package ox-reveal :disabled t)

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
         (package-source (package-desc-archive package-info))
         (path (format
                (cond
                 ((string= package-source "gnu") "https://elpa.gnu.org/packages/%s.html")
                 ((string= package-source "melpa") "http://melpa.org/#/%s")
                 (t (throw 'unknown-source)))
                link))
         (desc (or description link)))
    (cond
     ((eq format '11ty) (format "<a target=\"_blank\" href=\"%s\">%s</a>" path desc))
     ((eq format 'html) (format "<a target=\"_blank\" href=\"%s\">%s</a>" path desc))
     ((eq format 'wp) (format "<a target=\"_blank\" href=\"%s\">%s</a>" path desc))
     ((eq format 'latex) (format "\\href{%s}{%s}" path desc))
     ((eq format 'texinfo) (format "@uref{%s,%s}" path desc))
     ((eq format 'ascii) (format "%s <%s>" desc path))
     (t path))))

(org-link-set-parameters "package" :follow 'my-org-package-open :export 'my-org-package-export)

(setq org-ascii-links-to-notes nil)

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

(defun my-org-save-all-org-buffers ()
  (unless my-unfocusing
    (let ((my-unfocusing t))
      (my-org-debounce-idle-timer 10
                                  my-org-save-all-org-buffers-timer
                                  'org-save-all-org-buffers))))
(use-package org
  :config
  (add-function :after after-focus-change-function 'my-org-save-all-org-buffers))

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
    (cond ((and region-content clipboard-url (not point-in-link))
           (delete-region (region-beginning) (region-end))
           (insert (org-link-make-string clipboard-url region-content)))
          ((and clipboard-url (not point-in-link))
           (insert (org-link-make-string
                    clipboard-url
                    (read-string "title: "
                                 (with-current-buffer (url-retrieve-synchronously clipboard-url)
                                   (dom-text (car
                                              (dom-by-tag (libxml-parse-html-region
                                                           (point-min)
                                                           (point-max))
                                                          'title))))))))
          (t
           (call-interactively 'org-insert-link)))))
(use-package org :bind (:map org-mode-map ("C-c C-l" . ar/org-insert-link-dwim)))

(defun my-org-insert-clipboard ()
  "Convert clipboard contents from HTML to Org and then paste (yank)."
  (interactive)
  (insert (shell-command-to-string "xclip -o -selection clipboard -t text/html | pandoc -f html -t json | pandoc -f json -t org")))

(defun my-org-set-property (property value)
  "In the current entry, set PROPERTY to VALUE.
Use the region if active."
  (interactive (list (org-read-property-name)
                     (when (region-active-p) (buffer-substring (point) (mark)))))
  (org-set-property property value))
(use-package org
  :bind (:map org-mode-map
              ("C-c C-x p" . my-org-set-property)))

;; from FAQ at http://web-mode.org/ for smartparens

;; Avoid lockfiles because they mess up React projects
(when my-laptop-p
  (setq create-lockfiles nil))

(defun my-web-mode-hook ()
  (setq web-mode-enable-auto-pairing nil))

(defun my-sp-web-mode-is-code-context (id action context)
  (when (and (eq action 'insert)
             (not (or (get-text-property (point) 'part-side)
                      (get-text-property (point) 'block-side))))
    t))

(use-package web-mode
  :if my-laptop-p
  :mode "\\(\\.html?\\|\\.njk\\)\\'"
  :config
  (progn
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-enable-current-element-highlight t)
    (setq web-mode-ac-sources-alist
          '(("css" . (ac-source-css-property))
            ("html" . (ac-source-words-in-buffer ac-source-abbrev)))
          )))

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
  :hook ((prog-mode-hook . lsp)
         (python-mode . lsp)
         (lsp-mode-hook . lsp-enable-which-key-integration)))
(use-package lsp-ui
  :if my-laptop-p
  :commands lsp-ui-mode
  :after lsp-mode)
(use-package dap-mode
  :if my-laptop-p
  :after lsp-mode)

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

(setq-default tab-width 2)

(defun sanityinc/kill-back-to-indentation ()
  "Kill from point back to the first non-whitespace character on the line."
  (interactive)
  (let ((prev-pos (point)))
    (back-to-indentation)
    (kill-region (point) prev-pos)))
(bind-key "C-M-<backspace>" 'sanityinc/kill-back-to-indentation)

(defun my-align-non-space (BEG END)
  "Align non-space columns in region BEG END."
  (interactive "r")
  (align-regexp BEG END "\\(\\s-*\\)\\S-+" 1 1 t))

(use-package yaml-mode
  :if my-laptop-p
  :mode "\\.yml\\'")

(use-package expand-region
  :defer t
  :bind ("C-=" . er/expand-region)
  ("C-<prior>" . er/expand-region)
  ("C-<next>" . er/contract-region))

(eval-after-load 'python-mode
  '(bind-key "C-c C-c" 'compile python-mode-map))

(use-package edit-list :commands edit-list)

(setq eval-expression-print-length nil)
(setq print-length nil)
(setq edebug-print-length nil)

(use-package buttercup)
(use-package package-lint)

(use-package "eldoc"
  :if my-laptop-p
  :diminish eldoc-mode
  :commands turn-on-eldoc-mode
  :defer t
  :init
  (progn
    (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
    (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
    (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)))

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

(define-key emacs-lisp-mode-map (kbd "C-c .") 'find-function-at-point)

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

(bind-key "M-:" 'pp-eval-expression)

(defun sanityinc/eval-last-sexp-or-region (prefix)
  "Eval region from BEG to END if active, otherwise the last sexp."
  (interactive "P")
  (if (and (mark) (use-region-p))
      (eval-region (min (point) (mark)) (max (point) (mark)))
    (pp-eval-last-sexp prefix)))

(bind-key "C-x C-e" 'sanityinc/eval-last-sexp-or-region emacs-lisp-mode-map)

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

(use-package helpful
  :bind
  ([remap describe-key] . helpful-key)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-function] . helpful-callable))

(use-package yasnippet
  :diminish yas-minor-mode
  :init (yas-global-mode)
  :config
  (progn
    (yas-global-mode)
    (add-hook 'hippie-expand-try-functions-list 'yas-hippie-try-expand)
    (setq yas-key-syntaxes '("w_" "w_." "^ "))
    (setq yas-installed-snippets-dir "~/elisp/yasnippet-snippets")
    (setq yas-expand-only-for-last-commands nil)
    (yas-global-mode 1)
    (bind-key "\t" 'hippie-expand yas-minor-mode-map)
    (add-to-list 'yas-prompt-functions 'shk-yas/helm-prompt)))
;;        (global-set-key (kbd "C-c y") (lambda () (interactive)
;;                                         (yas/load-directory "~/elisp/snippets")))

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

(column-number-mode 1)

(setq vc-diff-switches '("-b" "-B" "-u"))
(setq vc-git-diff-switches nil)

(add-to-list 'auto-mode-alist '("\\.js\\'\\|\\.json\\'" . js2-mode))

(use-package js2-mode
  :if my-laptop-p
  :mode "\\.js\\'"
  :bind (:map js2-mode-map ("C-c C-c" . projectile-compile-project)))

(use-package coffee-mode
  :if my-laptop-p
  :mode "\\.coffee\\'"
  :bind (:map coffee-mode-map ("C-c C-c" . compile)))

(use-package jasminejs-mode
  :if my-laptop-p
  :after js2-mode
  :hook ((js2-mode . jasminejs-mode)
         (jasminejs-mode-hook . jasminejs-add-snippets-to-yas-snippet-dirs)))

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

(use-package coffee-mode
  :if my-laptop-p
  :defer t
  :config (setq-default coffee-js-mode 'js2-mode coffee-tab-width 2))

(use-package indium
:hook ((js2-mode . indium-interaction-mode)))

(use-package rjsx-mode
  :if my-laptop-p)

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

(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

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

(use-package forge
  :after magit)

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

(use-package git-messenger
  :bind (("C-x v m" . git-messenger:popup-message)))

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

(use-package projectile
  :diminish projectile-mode
  :config
  (progn
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
    (projectile-mode +1)
    (setq projectile-completion-system 'default)
    (setq projectile-enable-caching t)
    (setq projectile-indexing-method 'alien)
    (add-to-list 'projectile-globally-ignored-files "node_modules")
    (add-to-list 'projectile-globally-ignored-files ".cache")
    (add-to-list 'projectile-globally-ignored-files "_cache")
    ))
;; Call with C-c p m m
(use-package makefile-executor
  :if my-laptop-p
  :config
  (add-hook 'makefile-mode-hook 'makefile-executor-mode))



(use-package rinari :if my-laptop-p)
(use-package bundler :if my-laptop-p)
(use-package robe
  :if my-laptop-p
  :hook
  ((ruby-mode-hook . robe-mode)
   (robe-mode-hook . ac-robe-setup)
   (ruby-mode-hook . auto-complete-mode)))
(use-package haml-mode
  :if my-laptop-p
  :mode "\\.haml\\'")

(defun my-rspec-verify-single ()
  "Runs the specified example at the point of the current buffer."
  (interactive)
  (rspec-run-single-file
   (concat
    (rspec-spec-file-for (buffer-file-name))
    ":"
    (save-restriction
      (widen)
      (number-to-string (line-number-at-pos))))
   (rspec-core-options)))

(use-package rspec-mode
  :if my-laptop-p
  :config
  (progn
    (setq rspec-command-options "--fail-fast --format documentation")
    (bind-key "C-c , ," 'rspec-rerun rspec-mode-map)
    (fset 'rspec-verify-single 'my-rspec-verify-single)))

(use-package sass-mode
  :if my-laptop-p
  :hook (sass-mode-hook . (lambda () (setq indent-tabs-mode nil))))
(setq-default indent-tabs-mode nil)

(use-package skewer-mode
  :if my-laptop-p
  :hook
  ((js2-mode-hook . skewer-mode)
   (css-mode-hook . skewer-css-mode)
   (html-mode-hook . skewer-html-mode)))

(use-package company
  :if my-laptop-p
  :bind (:map company-mode
              ("TAB" . #'company-indent-or-complete-common))
  :config (add-hook 'prog-mode-hook 'company-mode))
(use-package company-posframe :if my-laptop-p :init (company-posframe-mode 1) :diminish)

(use-package tern
  :if my-laptop-p
  :bind (:map tern-mode-keymap ("C-c C-c" . compile))
  :hook (js2-mode-hook . tern-mode)
  :config
  (when (eq system-type 'windows-nt) (setq tern-command '("cmd" "/c" "tern"))))

(use-package dockerfile-mode
  :mode ("Dockerfile\\'" . dockerfile-mode))

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
                                       "#sachachua"))
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

(use-package mastodon
  :if my-laptop-p
  :quelpa (mastodon :fetcher git :url "https://codeberg.org/martianh/mastodon.el.git" :branch "develop")
  :bind
  (:map mastodon-mode-map
        ("g" . mastodon-tl--update)
        ;; see org-capture-templates addition
        ("o" . (lambda () (interactive) (org-capture nil "m")))
        ("@" . my-mastodon-mentions))
  :commands (mastodon-http--api mastodon-http--post mastodon-mode mastodon-http--get-search-json)
  :config
  (setq mastodon-instance-url "https://emacs.ch"
        mastodon-active-user "sachac"))

(autoload 'mastodon-url-lookup "mastodon")

(defun my-mastodon-open-at-point ()
  "Open the URL at point, or prompt if a URL is not found."
  (interactive)
  (mastodon-url-lookup (or (thing-at-point 'url) (read-string "URL: "))))

(defun my-mastodon-org-open-at-point ()
  "Open things that look like Mastodon links using mastodon.el.
I'm guessing https://server/@user/id URLs are Mastodon links."
  (let ((context (org-element-lineage (org-element-context) '(link) t)))
    (when (and (org-element-property :raw-link context)
               ;; mastodon--masto-url-p might be related,
               ;; but it matches a bit too much for me
               (string-match "https?://[^/]+/@[^/]+/.*"
                             (org-element-property :raw-link context)))
      (my-mastodon-open-url (org-element-property :raw-link context)))))

(with-eval-after-load "org"
  (add-to-list 'org-open-at-point-functions #'my-mastodon-org-open-at-point))

(defun my-mastodon-store-link ()
  "Store links in Mastodon buffers."
  (when (derived-mode-p 'mastodon-mode)
    (let ((json (get-text-property (point) 'toot-json)))
      (org-link-store-props
       :link (assoc-default 'url json)
       :content (assoc-default 'content json)
       :text (string-trim (mastodon-tl--render-text (assoc-default 'content json)))))))

(use-package org
  :config
  (org-link-set-parameters
   "mastodon"
   :store 'my-mastodon-store-link)
  (add-to-list 'org-capture-templates
               `("m" "Mastodon" entry (file ,my-org-inbox-file)
                 "* %?

#+begin_quote
%:text
#+end_quote

%a"
                 :prepend t)))

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
   (when (websocket-openp obs-websocket)  (my-stream-message (org-get-heading t t t t)))
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
      ((string= category "ask")
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

(defun my-compare-times (clocked estimated)
  (if (and (> (length clocked) 0) estimated)
      (format "%.2f"
              (/ (* 1.0 (org-hh:mm-string-to-minutes clocked))
                 (org-hh:mm-string-to-minutes estimated)))
    ""))

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

(defun my-strip-blog-share ()
  (interactive)
  (let (base)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              "<div class=\"sharedaddy sd-sharing-enabled\">.*?<div class=\"sharing-clear\"></div></div></div></div>" nil t)
        (replace-match "")))))

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

(setq gnutls-trustfiles '("c:/sacha/cacert.pem.txt"))

(defadvice face-attribute (around sacha activate)
  (if (symbolp (ad-get-arg 0))
      ad-do-it))

(defadvice ido-sort-mtime (around sacha activate)
  (setq ido-temp-list
        (sort ido-temp-list
              (lambda (a b)
                (let ((ta (or (nth 5 (file-attributes (concat ido-current-directory a))) '(0 0)))
                      (tb (or (nth 5 (file-attributes (concat ido-current-directory b))) '(0 0))))
                  (if (= (nth 0 ta) (nth 0 tb))
                      (> (nth 1 ta) (nth 1 tb))
                    (> (nth 0 ta) (nth 0 tb)))))))
  (setq ad-return-value
        (ido-to-end  ;; move . files to end (again)
         (delq nil (mapcar
                    (lambda (x) (if (string-equal (substring x 0 1) ".") x))
                    ido-temp-list)))))

;(setq eimp-mogrify-program "c:/Program Files/ImageMagick-6.8.3-Q16/mogrify.exe")

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

(defun sanityinc/adjust-opacity (frame incr)
  (let* ((oldalpha (or (frame-parameter frame 'alpha) 100))
         (newalpha (+ incr oldalpha)))
    (when (and (<= frame-alpha-lower-limit newalpha) (>= 100 newalpha))
      (modify-frame-parameters frame (list (cons 'alpha newalpha))))))
(global-set-key (kbd "M-C-8") (lambda () (interactive) (sanityinc/adjust-opacity nil -2)))
(global-set-key (kbd "M-C-9") (lambda () (interactive) (sanityinc/adjust-opacity nil 2)))
(global-set-key (kbd "M-C-0") (lambda () (interactive) (modify-frame-parameters nil `((alpha . 100)))))

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

(use-package clipmon
  :disabled t
  :init (progn (setq clipmon-action 'kill-new clipmon-timeout nil clipmon-sound nil clipmon-cursor-color nil clipmon-suffix nil) (clipmon-mode)))

(use-package xclip :if my-phone-p) ; Turn on with xclip-mode

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

(use-package gnus
  :config
  (require 'mm-decode)
  (setq mm-discouraged-alternatives
        '("text/html" "text/richtext")
        mm-automatic-display
        (-difference mm-automatic-display '("text/html" "text/enriched" "text/richtext"))))

(setq gnus-treat-hide-citation t)

(setq gnus-use-adaptive-scoring t)
(setq gnus-default-adaptive-score-alist
      '((gnus-unread-mark)
        (gnus-ticked-mark (subject 10))
        (gnus-killed-mark (subject -5))
        (gnus-catchup-mark (subject -1))))

(setq notmuch-message-headers '("Subject" "To" "Cc" "Date" "Reply-To"))
(use-package notmuch
  :if my-laptop-p
  :config (setq-default notmuch-search-oldest-first nil)
  (setq notmuch-fcc-dirs nil)
  (setq notmuch-archive-tags '("-inbox" "-flagged" )))
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

(use-package ledger-mode
  :load-path "~/vendor/ledger-mode"
  :mode "\\.ledger$"
  :bind (:map ledger-mode-map
              ("C-c C-n" . my-ledger-change-account)
              ("C-c a" . my-ledger-set-unknown-account)
              ("C-c f" . (lambda () (interactive) (find-file (my-latest-file "~/Downloads"))))))
(defun my-open-latest-download ()
  (interactive)
  (find-file (my-latest-file "~/Downloads")))

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

(add-hook 'after-make-frame-functions
          (lambda (frame)
            (select-frame frame)
            (my-setup-color-theme)))

(use-package crdt
  :quelpa (crdt :fetcher github :repo "zaeph/crdt.el")
  :commands (crdt-share-buffer crdt-connect)
  :load-path "~/vendor/crdt.el"
  :if my-laptop-p)

(define-key-after global-map [menu-bar my-menu] (cons "Shortcuts" (make-sparse-keymap "Custom shortcuts")) 'tools)
(define-key global-map [menu-bar my-menu journal] '("Show journal entries" . my-show-missing-journal-entries))
(define-key global-map [menu-bar my-menu agenda] '("Org agenda" . (lambda () (interactive) (org-agenda nil "a"))))
(define-key global-map [menu-bar my-menu audio] '("Process audio" . (lambda () (interactive) (shell-command "~/bin/process-audio &"))))
(define-key global-map [menu-bar my-menu new-index-card] '("New index card" . (lambda () (interactive)
                                                                                (my-org-sketch-edit (my-prepare-index-card-template)))))

(use-package pcsv)

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

(use-package edit-list :commands edit-list)

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

(when (eq system-type 'windows-nt)
  (setq tramp-default-method "plink")
  (setq tramp-auto-save-directory "c:\\sacha\\tmp"))

(defun my-test-urls (urls)
  "Given a list of URLs, return a list of any URLS that don't result in an OK value."
  (delq nil
        (mapcar (lambda (url)
                  (let ((url-request-method "HEAD"))
                    (with-current-buffer (url-retrieve-synchronously url)
                      (goto-char (point-min))
                      (unless (looking-at "HTTP/1.1 200 OK") url))))
                urls)))

;; https://emacs.stackexchange.com/questions/19035/finding-frames-by-name
(defun my-get-frame-by-name (fname)
  "If there is a frame named FNAME, return it, else nil."
  (seq-find (lambda (frame)
              (when (equal fname (frame-parameter frame 'name))
             frame))
            (frame-list)))
;; (obs-websocket-send "GetSourceSettings" :sourceName "Command log" :callback (lambda  (frame payload) (prin1 payload)))
(defun my-wmctl-get-id (window-name)
  (string-to-number (replace-regexp-in-string "^0x\\|\n" "" (shell-command-to-string (format "wmctrl -l | grep %s | head -1 | awk '{print $1}'" (shell-quote-argument window-name)))) 16))

(defvar my-stream-ffmpeg-multicast nil "Process for multicasting the stream")
(defun my-stream-ffmpeg-multicast ()
  (interactive)
  (unless (process-live-p my-stream-ffmpeg-multicast)
    (setq my-stream-ffmpeg-multicast (start-process "FFmpeg multicast" "*ffmpeg multicast*" "~/bin/ffmpeg-multicast"))))

(defun my-stream-fix-sources ()
  (interactive)
  (obs-websocket-send "SetVolume" :source "Mic/Aux" :volume 1)
  (mapc (lambda (buf)
          (when (and (buffer-file-name buf) (string-match "secret" (buffer-file-name buf)))
            (kill-buffer-ask buf)))
        (buffer-list))
  (obs-websocket-send "SetSourceSettings" :sourceName "Command log"
                      :sourceSettings
                      `(:capture_window
                        ,(format "%d\n%s\n%s"
                                 (my-wmctl-get-id "command-log")
                                 " *command-log*"
                                 "emacs"))))

(use-package command-log-mode
  :if my-laptop-p
  :commands
  command-log-mode
  clm/open-command-log-buffer
  global-command-log-mode
  :defines
  clm/command-log-buffer
  )
(defun my-stream-set-up-frames ()
  (interactive)
  (global-command-log-mode 1)
  (unless (my-get-frame-by-name (buffer-name clm/command-log-buffer))
    (switch-to-buffer-other-frame clm/command-log-buffer))
  (clm/with-command-log-buffer
    (text-scale-set 3))
  (call-process "wmctrl" nil 0 nil "-r" (number-to-string (my-wmctl-get-id "command-log")) "-e" "0,0,100,1366,100"))

(defun my-stream-set-up ()
  (interactive)
  (my-stream-ffmpeg-multicast)
  (obs-websocket-connect)
  (my-stream-toggle-background-music)
  (selectric-mode 1)
  (my-stream-set-up-frames)
  (my-stream-fix-sources)
  (obs-websocket-minor-mode 1)
  (unless (and (erc-get-buffer "#sachachua")
               (with-current-buffer (erc-get-buffer "#sachachua")
                 (erc-server-process-alive)))
    (my-twitch-irc)))

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

(defun my-selectric-type-sound ()
  "Make the sound of typing."
  ;; Someday, randomize this or something
  (selectric-make-sound (expand-file-name "selectric-move.wav" selectric-files-path)))

(use-package selectric-mode
  :if my-laptop-p
  :diminish ""
  :config
  (fset #'selectric-type-sound #'my-selectric-type-sound))

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

(global-set-key (kbd "<f12>") #'my-push-to-talk)

(defun my-stream-message (text)
  (interactive "MText: ")
  (obs-websocket-send "SetSourceSettings" :sourceName "OBSMessage" :sourceSettings
                      (list :text
                            (concat (mapconcat 'identity (org-wrap text 80) "\n"))))
  (my-obs-websocket-add-caption text)
  (when obs-websocket-streaming-p
    (with-current-buffer (find-file-noselect "~/proj/stream/index.org")
      (org-link-search "Timestamps")
      (forward-line 1)
      (insert (format "- (%s) %s\n"
                      (format-seconds "%h:%.2m:%.2s%z" (floor (my-obs-websocket-stream-time-secs)))
                      text))))
  (when (erc-get-buffer "#sachachua")
    (with-current-buffer (erc-get-buffer "#sachachua")
      (erc-send-message text))))

(defvar my-obs-websocket-last-stream-timecode nil "(timecode-string . system-time)")
(defvar my-obs-websocket-last-recording-timecode nil "(timecode-string . system-time)")

(defun my-obs-websocket-message-handler (payload)
  "Save the current streaming timecode."
  (pcase (plist-get payload :update-type)
    ("RecordingStarted" (my-obs-websocket-check-recording-timecode))
    ("StreamStatus"
     (setq my-obs-websocket-last-stream-timecode (cons (plist-get payload :stream-timecode) (current-time))))))

(defun my-obs-websocket-check-recording-timecode ()
  (obs-websocket-send "GetRecordingStatus"
                      :callback
                      (lambda (frame payload)
                        (setq my-obs-websocket-last-recording-timecode
                              (cons (plist-get payload :recordTimecode) (current-time))))))

(defun my-obs-websocket-timecode-to-msecs (time-string)
  "Find HH:MM:SS.MS pattern in TIME-CODE and convert it to milliseconds.
Return nil if TIME-CODE doesn't match the pattern."
  (save-match-data
    (when (and time-string (string-match "\\([0-9]+\\):\\([0-9]+\\):\\([0-9]+\\)\\.\\([0-9]+\\)" time-string))
      (let ((hours (string-to-number (match-string 1 time-string)))
            (mins  (string-to-number (match-string 2 time-string)))
            (secs  (string-to-number (match-string 3 time-string)))
            (msecs (string-to-number (match-string 4 time-string))))
        (+ (* (truncate hours) 3600000)
           (* (truncate mins) 60000)
           (* (truncate secs) 1000)
           (truncate msecs))))))

(defun my-obs-websocket-adjust-timecode (timecode-time)
  "Returns the current adjusted time in milliseconds based on TIMECODE-TIME.
TIMECODE-TIME is an alist of (timecode-string . elisp-time)."
  (when timecode-time
    (+
     (my-obs-websocket-timecode-to-msecs (car timecode-time))
     (* 1000.0
        (- (time-to-seconds (current-time))
           (time-to-seconds (cdr timecode-time)))))))

(defun my-obs-websocket-stream-time-secs ()
  "Return current stream time in seconds."
  (/ (my-obs-websocket-adjust-timecode my-obs-websocket-last-stream-timecode) 1000.0))

(defun my-obs-websocket-stream-time-msecs ()
  "Return current stream time in milliseconds."
  (my-obs-websocket-adjust-timecode my-obs-websocket-last-stream-timecode))

(defun my-obs-websocket-recording-time-secs ()
  "Return current recording time in seconds."
  (/ (my-obs-websocket-adjust-timecode my-obs-websocket-last-recording-timecode) 1000.0))

(defun my-obs-websocket-recording-time-msecs ()
  "Return current recording time in milliseconds."
  (my-obs-websocket-adjust-timecode my-obs-websocket-last-recording-timecode))

(defun my-obs-websocket-caption-file (&optional filename)
  "Return the caption file for the current video."
  (setq filename (or filename obs-websocket-recording-filename))
  (when filename
    (expand-file-name (concat (file-name-sans-extension filename) ".vtt")
                      (file-name-directory filename))))

(defun my-obs-websocket-add-caption (text &optional ms)
  (interactive (list (read-string "Text: ")))
 (when (websocket-openp obs-websocket) (obs-websocket-send "SendCaptions" :text text))
  (setq ms (or ms (my-obs-websocket-recording-time-msecs)))
  (when obs-websocket-recording-filename
    (with-current-buffer (find-file-noselect (my-obs-websocket-caption-file))
      (goto-char (point-max))
      (when (bobp) (insert "WEBVTT\n\n"))
      (subed-append-subtitle nil ms nil text)
      (save-excursion
        (when (subed-backward-subtitle-text)
          (subed-set-subtitle-time-stop ms)))
      (save-buffer))))

(defun my-stream-intermission (text)
  "Start an intermission and prompt me for a message."
  (interactive "MText: ")
  (set-background-color "#330000")
  (obs-websocket-send "SetCurrentScene" :scene-name "Intermission")
  (my-stream-message text))

(defun my-show-emacs-tasks ()
  (interactive)
  (org-ql-search (org-agenda-files)
    '(and (todo)
          (parent (and (tags "project") (tags "emacs") (not (tags "inactive")))))
    :title "Emacs-related project tasks"
    :sort '(date priority todo)
    :super-groups '((:auto-parent t))))

(defvar my-stream-captions-insert nil "Non-nil means insert into the current buffer.")
(use-package websocket)
(use-package obs-websocket
  :after websocket
  :if my-laptop-p
  :config
  (add-to-list 'obs-websocket-on-message-payload-functions #'my-obs-websocket-message-handler)
  :load-path "~/proj/obs-websocket-el" :ensure nil)
(with-eval-after-load 'obs-websocket
  (defun my-stream-toggle-streaming () (interactive) (obs-websocket-send "StartStopStreaming"))
  (defun my-stream-toggle-recording () (interactive) (obs-websocket-send "StartStopRecording"))
  (defvaralias 'my-stream-toggle-streaming 'obs-websocket-streaming-p)
  (defvaralias 'my-stream-toggle-recording 'obs-websocket-recording-p)
  (defhydra my-stream-recording (:exit t) "Recording"
    ("b" (obs-websocket-send "StartRecording") "Begin")
    ("r" (obs-websocket-send "StartStopRecording") "Toggle")
    (" " (obs-websocket-send "PauseRecording") "Pause")
    ("p" (my-play-latest-recording) "Play last")
    ("c" (obs-websocket-send "ResumeRecording") "Continue")
    ("e" (obs-websocket-send "StopRecording") "End"))
  (defhydra my-stream (:quit-key "q")
   ("w" (org-open-link-from-string "[[file:~/proj/stream/notes.org::#streaming-workflow][Streaming]]") "Workflow" :column "Setup")
   ("o" (org-open-link-from-string "[[file:~/proj/stream/index.org::#plans]]") "Notes")
   ("a" my-show-emacs-tasks "Agenda")
   ("bt" selectric-mode "Typing sounds")
   ("bm" my-stream-toggle-background-music "Background music")
   ("I" my-stream-captions-insert (format "Insert caption [%s]" (if my-stream-captions-insert "X" " ")))
   ("us" (browse-url "https://twitch.tv/sachachua") "View stream")
   ("uv" (browse-url "https://dashboard.twitch.tv/u/sachachua/stream-manager") "View manager")
   ("uy" (browse-url "https://studio.youtube.com/channel/UClT2UAbC6j7TqOWurVhkuHQ/livestreaming/dashboard") "Youtube")
   ("m" my-mic-toggle (if my-mic-p "MIC IS ON [X]" "Mic [ ]"))
   ("s" my-stream-toggle-streaming (format "Streaming [%s]" (if (and (boundp 'obs-websocket-streaming-p) obs-websocket-streaming-p) "X" " ")) :exit t :column "Streaming/recording")
   ("r" my-stream-toggle-recording (format "Recording [%s]" (if (and (boundp 'obs-websocket-recording-p) obs-websocket-recording-p) "X" " ")) :exit t)
   ("v" (my-play-latest-recording) "Play last")
   ("d" (progn (set-background-color "black") (obs-websocket-send "SetCurrentScene" :scene-name "Desktop")) "Desktop" :exit t :column "Scenes")
   ("e" (obs-websocket-send "SetCurrentScene" :scene-name "Emacs") "Emacs" :exit t)
   ("i" my-stream-intermission "Intermission" :exit t)
   ("n" my-obs-websocket-add-caption "Add caption" :exit t :column "Captions")
   ("c" (find-file (my-obs-websocket-caption-file)) "View captions" :exit t)
   ("t" my-stream-message "Message" :hint nil :exit t)
   ("<f8>" my-stream-message "Message" :hint nil :exit t))
  (global-set-key (kbd "<f8>") #'my-stream/body))

(use-package mpv :if my-laptop-p)
(defvar my-recordings-dir "~/recordings/")
(defun my-play-latest-recording (&optional arg)
  (interactive "P")
  (let ((latest (my-latest-file my-recordings-dir)))
    (if (and arg (file-exists-p (my-obs-websocket-caption-file latest)))
        (with-current-buffer (find-file-noselect (my-obs-websocket-caption-file (my-latest-file my-recordings-dir)))
          (goto-char (point-min))
          (subed-mpv-find-video latest)
          (pop-to-buffer (current-buffer)))
      (mpv-play (my-latest-file my-recordings-dir "mkv")))))
(defun my-rename-last-recording ()
  (interactive)
  (let ((latest (my-latest-file my-recordings-dir "mkv")))
    (rename-file latest
                 (expand-file-name
                  (concat (read-string "New name: " (format-time-string "%Y-%m-%d ")) "." (file-name-extension latest))
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

(setq imp-default-user-filters '((org-mode . my-imp-htmlize-filter)
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
(use-package impatient-mode
  :config (setq impatient-mode-delay 1))

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
(global-set-key (kbd "<f11>") 'my-stream-captions-edit-last)

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

(use-package smartparens
  :if my-laptop-p
  :config
  (progn
    (require 'smartparens-config)
    (add-hook 'emacs-lisp-mode-hook 'smartparens-mode)
    (add-hook 'emacs-lisp-mode-hook 'show-smartparens-mode)

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

(use-package lispy :hook (emacs-lisp-mode . lispy-mode))

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

(setq epa-file-encrypt-to '("sacha@sachachua.com"))
(setq epa-pinentry-mode 'loopback)
(setq epg-pinentry-mode 'loopback)

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
  (find-name-dired directory "*.sync-conflict-*"))

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
  (seq-filter (lambda (o) (not (string-match "\\.stversions" o))) (directory-files-recursively directory "\\.sync-conflict-")))


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

(defun my-search-irc-logs ()
  (interactive)
  (let ((helm-rg-default-directory "~/backups/server/home/.znc/users/sachac/moddata/log"))
    (call-interactively 'helm-rg)))

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

(use-package emacsconf
  :after hydra
  :bind (("C-c e" . hydra-emacsconf/body)
         ("M-g t" . emacsconf-go-to-talk))
  :commands
  (emacsconf-go-to-talk emacsconf-cache-find-file emacsconf-res-dired emacsconf-upload-dired)
  :hydra (hydra-emacsconf
          ()
          ("t" emacsconf-go-to-talk "talk")
          ("f" emacsconf-cache-find-file "file")
          ("c" (find-file emacsconf-org-file) "conf.org")
          ("C" (let ((default-directory (file-name-directory emacsconf-org-file)))
                 (call-interactively #'projectile-find-file)) "org dir")
          ("w" (let ((default-directory emacsconf-directory))
                 (call-interactively #'projectile-find-file)))
          ("o"
           (progn
             (find-file (expand-file-name "2022/organizers-notebook/index.org" emacsconf-directory))
             (call-interactively #'consult-org-heading))
           "org notes")
          ("a" (let ((default-directory emacsconf-ansible-directory))
                 (call-interactively #'projectile-find-file)) "ansible")
          ("i" (switch-to-buffer (erc-get-buffer "#emacsconf-org")))
          ("l" (let ((default-directory "~/proj/emacsconf/lisp"))
                 (call-interactively #'projectile-find-file)))
          ("b" emacsconf-backstage-dired "backstage")
          ("u" emacsconf-upload-dired "upload"))
  :load-path "~/proj/emacsconf/lisp")
(global-set-key (kbd "M-g t") 'emacsconf-go-to-talk)

(use-package paint
  :disabled t
  :if my-laptop-p
  :load-path "~/cloud/elisp"
  :init
  (progn
    (setq paint-foreground-color "white" paint-background-color "black")
    (defun my-paint () (interactive) (delete-other-windows) (paint 1600 900 nil))))

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

(use-package rainbow-delimiters :disabled t)

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

(auto-image-file-mode -1)

(defvar my-sketch-directories
  '("~/sync/sketches"
    "~/cloud/private-sketches"
    "~/Dropbox/Inbox"
    "~/Dropbox/Inbox/To blog"))

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
  (let ((base-regexp (unless (functionp base)
                       (concat
                        "\\("
                        (if as-regexp base (regexp-quote base))
                        "\\)"
                        ".*\\(\\.\\(png\\|psd\\|tiff\\|jpg\\|svg\\)\\)?$"))))
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
    (find-file (my-get-image-filename id my-sketch-directories))))
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
        (format "<a target=\"_blank\" href=\"%s\"><img src=\"%s\"><br />%s</a>" path image desc)))
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
  (let* ((backend (org-export-backend-name (plist-get info :back-end)))
         (desc (or description link)))
    (format "{%% sketchThumb \"%s\", \"%s\" %%}" (file-name-base link) desc)))

(defun my-org-image-export-full (link description format info)
  (let* ((path (concat "https://sketches.sachachua.com/filename/" link))
         (image (concat "https://sketches.sachachua.com/static/" link))
         (backend (org-export-backend-name (plist-get info :back-end)))
         (desc (or description link)))
    (cond
     ((eq backend '11ty) (format "{%% sketchFull \"%s\", \"%s\" %%}" link desc))
     ((or (eq format 'html) (eq format 'wp))
      (if description
          (format "<a target=\"_blank\" href=\"%s\"><img src=\"%s\" /><br />%s</a>" path image desc)
        (format "<a target=\"_blank\" href=\"%s\"><img src=\"%s\"><br />%s</a>" path image desc)))
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

(use-package org
  :config
  (org-link-set-parameters
   "copy"
   :follow (lambda (link) (kill-new link))
))

(use-package org
  :config
  (org-link-set-parameters
   "config"
   :follow (lambda (id) (org-open-link-from-string (format "[[~/sync/emacs/Sacha.org::%s]]" id)))
   :export (lambda (link description format)
             (format "<a href=\"https://sachachua.com/dotemacs#%s\">%s</a>" link description))))

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

(use-package image+
  :if my-laptop-p
  ;;    :load-path "~/elisp/Emacs-imagex"
  :commands (imagex-global-sticky-mode imagex-auto-adjust-mode)
  :init (progn (imagex-global-sticky-mode) (imagex-auto-adjust-mode)))

(defun my-prepare-sketchnote-file ()
  (interactive)
  (let* ((base-name (org-entry-get-with-inheritance  "BASENAME")))
    (unless base-name (error "Missing basename property"))
    (my-org-sketch-open (my-prepare-large-template base-name))))

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
                org-bracket-link-regexp
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

(defun my-org-sort-list-by-regexp (regexp)
  (interactive "MRegexp: ")
  (let ((sort-func
         (lambda ()
           (let ((line (buffer-substring-no-properties (point) (line-end-position))))
             (if (string-match regexp line)
                 (if (string-match org-bracket-link-regexp line)
                     (match-string 2 line)
                   "ZZZ")
               "ZZZZZ")))))
    (funcall
     (cond
      ((org-at-table-p) 'org-table-sort-lines)
      ((org-at-item-p) 'org-sort-list)
      (t 'org-sort-entries))
     nil ?f sort-func (lambda (a b) (if (and (stringp a) (stringp b)) (string< a b) t)))))

(defun my-refile-sketches-to-questions ()
  (interactive)
  (while (looking-at "^  \\+ \\[\\[.*?\\]\\[\\(.*?\\) -- \\(.*?\\)\\]\\]\n")
    (let ((link (match-string 0))
          (title (match-string 1)))
      (save-excursion
        (if (save-match-data (search-forward (concat "* " title) nil t))
            (progn (forward-line) (insert (match-string 0)) (replace-match ""))
          (forward-line 1))))))

(use-package org-krita
  :ensure t
  :quelpa (org-krita :fetcher github :repo "lepisma/org-krita" :files ("*.el" "resources"))
  :hook (org-mode . org-krita-mode))
(use-package org-xournalpp
  :quelpa (org-xournalpp :fetcher gitlab :repo "vherrmann/org-xournalpp" :files ("*.el" "resources"))
  :hook (org-mode . org-xournalpp-mode))

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

(defun my-write-about-sketch (sketch)
  (interactive (list (and (my-update-sketch-cache) (my-complete-sketch-filename))))
  (shell-command "make-sketch-thumbnails")
  (let ((post-dir (format-time-string "~/proj/static-blog/blog/%Y/%m/")))
    (unless (file-directory-p post-dir) (mkdir post-dir))
    (find-file (expand-file-name "posts.org" post-dir)))
  (goto-char (point-max))
  (org-insert-heading nil nil t)
  (insert (read-string "Title: ") (format "\n\n[[sketchFull:%s][%s]]\n\n" (file-name-nondirectory sketch) (file-name-base sketch)))
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

(defvar my-supernote-export-dir "~/Dropbox/Supernote/EXPORT")
(defun my-supernote-process-latest ()
  (interactive)
  (find-file (my-latest-file my-supernote-export-dir))
  (my-supernote-process-sketch (read-string (format "New name for %s: " (file-name-base (buffer-file-name))))))
(defun my-supernote-export-dired ()
  (interactive)
  (dired my-supernote-export-dir "-tl"))
(defun my-supernote-process-sketch (new-name)
  (interactive "MNew name: ")
  (unless (member (file-name-extension new-name) '("png" "jpg"))
    (setq new-name (concat new-name "." (file-name-extension (buffer-file-name)))))
  (cond
   ((string-match " #ccw" new-name)
    (setq new-name (replace-match "" t t new-name))
    (call-process "convert" nil nil nil (buffer-file-name) "-rotate" "270" (expand-file-name new-name "~/sync/sketches")))
   ((string-match " #cw" new-name)
    (setq new-name (replace-match "" t t new-name))
    (call-process "convert" nil nil nil (buffer-file-name) "-rotate" "90" (expand-file-name new-name "~/sync/sketches"))
    )
   (t
    (copy-file
     (buffer-file-name)
     (expand-file-name new-name "~/sync/sketches"))))
  (when (file-exists-p (expand-file-name new-name "~/sync/sketches"))
    (delete-file (buffer-file-name))
    (kill-buffer)))

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

(defvar my-espeak-command "c:/program files (x86)/espeak/command_line/espeak.exe")
(defun my-say (string &optional speed)
  (interactive "MString: ")
  (setq speed (or speed 175))
  (call-process my-espeak-command nil nil nil string "-s" speed))

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
              (cons 'image (if (stringp .image) .image (elt .image 0)))
              (cons 'price
                    (assoc-default 'price (if (arrayp .offers)
                                              (elt .offers 0)
                                            .offers))))))
     ((re-search-forward "amazon.ca" nil t)
      (goto-char (point-min))
      (re-search-forward "^$")
      (let ((doc (libxml-parse-html-region (point) (point-max))))
        (list (cons 'name (dom-text (dom-by-tag doc 'title)))
              (cons 'description (dom-texts (dom-by-id doc "productDescription")))
              (cons 'image (dom-attr (dom-by-tag (dom-by-id doc "imgTagWrapperId") 'img) 'src))
              (cons 'price
                    (dom-texts (dom-by-id doc "priceblock_ourprice"))))))
     (t
      (goto-char (point-min))
      (re-search-forward "^$")
      (let ((doc (libxml-parse-html-region (point) (point-max))))
        (mapcar (lambda (property)
                  (let ((node (dom-search
                               doc
                               (lambda (o)
                                 (delq nil
                                       (mapcar (lambda (p) (string= (dom-attr o 'property) p)) (cdr property)))))))
                    (cons
                     (car property)
                     (dom-attr
                      node
                      'content))))
                '((name "og:title")
                  (brand "og:brand")
                  (url "og:url")
                  (image "og:image")
                  (description "og:description")
                  (price "og:price:amount" "product:price:amount"))))))))
(defun my-org-insert-shopping-details ()
  (interactive)
  (org-insert-heading)
  (save-excursion (yank))
  (my-org-update-shopping-details)
  (when (org-entry-get (point) "NAME")
    (org-edit-headline (org-entry-get (point) "NAME")))
  (org-end-of-subtree))
(defun my-org-update-shopping-details ()
  (interactive)
  (when (re-search-forward org-any-link-re (save-excursion (org-end-of-subtree)) t)
    (let* ((link (org-element-property :raw-link (org-element-context)))
           data)
      (if (string-match "theshoecompany\\|dsw" link)
          (progn
            (browse-url link)
            (org-entry-put (point) "URL" link)
            (unless (org-entry-get (point) "IMAGE")
              (org-entry-put (point) "IMAGE" (read-string "Image: ")))
            (unless (org-entry-get (point) "PRICE")
              (org-entry-put (point) "PRICE" (read-string "Price: "))))
        (setq data (with-current-buffer (url-retrieve-synchronously link)
                     (my-get-shopping-details)))
        (when data
          (let-alist data
            (org-entry-put (point) "NAME" .name)
            (org-entry-put (point) "URL" link)
            (org-entry-put (point) "BRAND" .brand)
            (org-entry-put (point) "DESCRIPTION" (replace-regexp-in-string "\n" " " .description))
            (org-entry-put (point) "IMAGE" .image)
            (if .price (org-entry-put (point) "PRICE" (if (stringp .price) .price (format "%.2f" .price))))
            (if .rating (org-entry-put (point) "RATING" (if (stringp .rating) .rating (format "%.1f" .rating))))
            (if .ratingCount (org-entry-put (point) "RATING_COUNT" (if (stringp .ratingCount) .ratingCount (number-to-string .ratingCount))))
            ))))))
(defun my-org-format-shopping-subtree ()
  (concat "<style>body { max-width: 100% !important } #content { max-width: 100% !important }</style><div style=\"display: flex; flex-wrap: wrap; align-items: flex-end\">"
          (string-join
           (save-excursion (org-map-entries
                            (lambda ()
                              (if (org-entry-get (point) "URL")
                                  (format "<div class=item style=\"width: 200px\"><div><a href=\"%s\"><img src=\"%s\" height=100></a></div>
<div><a href=\"%s\">%s</a></div>
<div>%s</div>
<div>%s</div>
<div>%s</div></div>"
                                          (org-entry-get (point) "URL")
                                          (org-entry-get (point) "IMAGE")
                                          (org-entry-get (point) "URL")
                                          (url-domain (url-generic-parse-url (org-entry-get (point) "URL")))
                                          (org-entry-get (point) "NAME")
                                          (org-entry-get (point) "PRICE")
                                          (or (org-entry-get (point) "NOTES") ""))
                                "")
                              ) nil
                                (if (org-before-first-heading-p) nil 'tree)))
           "")
          "</div>"))

(when (eq system-type 'windows-nt)
  (setenv "PATH" (concat "\"c:/program files/postgresql/9.3/bin;\"" (getenv "PATH"))))
