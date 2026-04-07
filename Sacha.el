;; -*- lexical-binding: t -*-
;; This sets up the load path so that we can override it
(setq warning-suppress-log-types '((package reinitialization)))  (package-initialize)
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp")
(add-to-list 'load-path "~/vendor/org-mode/lisp")
(add-to-list 'load-path "~/vendor/org-mode/contrib/lisp")
(setq custom-file "~/.config/emacs/custom-settings.el")
(setq use-package-always-ensure t)

;; [[file:Sacha.org::#starting-up][Starting up:2]]
(use-package memoize)
;; Starting up:2 ends here

;; [[file:Sacha.org::#add-package-sources][Add package sources:1]]
(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))
(unless (assoc-default "nongnu" package-archives)
  (add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t))
;; Add package sources:1 ends here

;; [[file:Sacha.org::#about-this-file-emacs-initialization-review-packages-when-upgrading][Review packages when upgrading:1]]
  (setq package-review-policy t
        package-review-diff-command '("git" "diff" "--no-index" "--color=never" "--diff-filter=d"))
  (add-to-list 'display-buffer-alist
               '("\\`\\*Package Review Diff:"
                 (display-buffer-full-frame)))
;; Review packages when upgrading:1 ends here

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

;; [[file:Sacha.org::#personal-information][Personal information:1]]
(setq user-full-name "Sacha Chua"
      user-mail-address "sacha@sachachua.com")
;; Personal information:1 ends here

;; [[file:Sacha.org::system-info][system-info]]
  (defvar my-laptop-p (or (equal (system-name) "sacha-x230") (equal (system-name) "sacha-p52")))
  (defvar my-server-p (and (equal (system-name) "localhost") (equal user-login-name "sacha")))
  (defvar my-phone-p (not (null (getenv "ANDROID_ROOT")))
    "If non-nil, GNU Emacs is running on Termux.")
  (when my-phone-p (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))
  (global-auto-revert-mode)  ; simplifies syncing
;; system-info ends here

;; [[file:Sacha.org::#backups][Backups:1]]
  (setq backup-directory-alist '(("\\.env$" . nil)
                                                                                                                           ("." . "~/.config/emacs/backups")))
  (with-eval-after-load 'tramp
          (setq tramp-backup-directory-alist nil))
;; Backups:1 ends here

;; [[file:Sacha.org::#backups][Backups:2]]
  (setq delete-old-versions -1)
  (setq version-control t)
  (setq vc-make-backup-files t)
  (setq auto-save-file-name-transforms '((".*" "~/.config/emacs/auto-save-list/" t)))
;; Backups:2 ends here

;; [[file:Sacha.org::#about-this-file-backups-obscure-emacs-package-appreciation-backup-walker][Obscure Emacs package appreciation: backup-walker:1]]
  (setq backup-directory-alist '(("\\.env$" . nil)
                                                                                                                           ("." . "~/.config/emacs/backups")))
  (with-eval-after-load 'tramp
          (setq tramp-backup-directory-alist nil))
  (setq delete-old-versions -1)
  (setq version-control t)
  (setq auto-save-file-name-transforms '((".*" "~/.config/emacs/auto-save-list/" t)))
;; Obscure Emacs package appreciation: backup-walker:1 ends here

;; [[file:Sacha.org::#about-this-file-backups-obscure-emacs-package-appreciation-backup-walker][Obscure Emacs package appreciation: backup-walker:3]]
  (with-eval-after-load 'backup-walker
    (advice-add 'backup-walker-refresh :override #'my-backup-walker-refresh))
;; Obscure Emacs package appreciation: backup-walker:3 ends here

;; [[file:Sacha.org::#about-this-file-backups-obscure-emacs-package-appreciation-backup-walker][Obscure Emacs package appreciation: backup-walker:4]]
  (use-package backup-walker
          :vc (:url "https://github.com/lewang/backup-walker")
          :commands backup-walker-start
          :init
          (defalias 'string-to-int 'string-to-number)  ; removed in 26.1
          (defalias 'display-buffer-other-window 'display-buffer))
;; Obscure Emacs package appreciation: backup-walker:4 ends here

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

;; [[file:Sacha.org::#windows-configuration][Disabling the toolbar:1]]
  (tool-bar-mode -1)
;; Disabling the toolbar:1 ends here

;; [[file:Sacha.org::#change-yes-or-no-to-y-or-n][Change "yes or no" to "y or n":1]]
  (setopt use-short-answers t)
;; Change "yes or no" to "y or n":1 ends here

;; [[file:Sacha.org::#minibuffer-editing-more-space][Minibuffer editing - more space!:1]]
  (use-package miniedit
    :commands minibuffer-edit
    :init (miniedit-install))
;; Minibuffer editing - more space!:1 ends here

;; [[file:Sacha.org::#killing-text][Killing text:1]]
      (setq kill-ring-max 1000)
;; Killing text:1 ends here

;; [[file:Sacha.org::#killing-text][Killing text:2]]
  (defadvice kill-region (before slick-cut activate compile)
    "When called interactively with no active region, kill a single line instead."
    (interactive
     (if mark-active (list (region-beginning) (region-end))
       (list (line-beginning-position)
             (line-beginning-position 2)))))
;; Killing text:2 ends here

;; [[file:Sacha.org::#keybindings][Keybindings:1]]
  (repeat-mode 1)
;; Keybindings:1 ends here

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
              "w" #'my-subed-copy-timestamp-dwim
              "<up>" #'my-subed-adjust-timestamp/my-subed-adjust-timestamp-up
              "f" #'my-waveform-subed-show-after-time
              "<down>" #'my-subed-adjust-timestamp/my-subed-adjust-timestamp-down))
          (defvar-keymap embark-sketch-actions
            :doc "Org Mode sketch-related actions"
            :parent org-mode-map
            "o" #'my-sketch-insert-file-as-link
                  "i" #'my-sketch-insert-file-as-link
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

;; [[file:Sacha.org::#embark][Embark:2]]
  (setq prefix-help-command 'embark-prefix-help-command)
;; Embark:2 ends here

;; [[file:Sacha.org::#embark-qr][Using Embark and qrencode to show a QR code for the Org Mode link at point:2]]
  (use-package qrencode
          :defer t
          :commands qrencode--encode-to-buffer
          :config
          (with-eval-after-load 'embark-org
                  (define-key embark-org-link-map (kbd "q") #'my-org-link-qr)))
;; Using Embark and qrencode to show a QR code for the Org Mode link at point:2 ends here

;; [[file:Sacha.org::#embark-video][Using Embark to act on video:2]]
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
;; Using Embark to act on video:2 ends here

;; [[file:Sacha.org::#embark-audio][Using Embark to act on audio:2]]
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
;; Using Embark to act on audio:2 ends here

;; [[file:Sacha.org::#using-embark-to-insert-files-as-org-includes][Using Embark to insert files as Org INCLUDEs:2]]
(with-eval-after-load 'embark
  (define-key embark-file-map "O" #'my-insert-file-as-org-include))
;; Using Embark to insert files as Org INCLUDEs:2 ends here

;; [[file:Sacha.org::#using-embark-to-offer-context-sensitive-actions-for-org-elements][Using Embark to offer context-sensitive actions for Org elements:2]]
(with-eval-after-load 'embark-org
  (keymap-set embark-org-src-block-map "N" #'my-embark-org-src-block-copy-noweb-reference))
;; Using Embark to offer context-sensitive actions for Org elements:2 ends here

;; [[file:Sacha.org::#whichkey-embark][Whichkey and Embark:2]]
(setq embark-indicators
      '(embark-which-key-indicator
        embark-highlight-indicator
        embark-isearch-highlight-indicator))

(with-eval-after-load 'embark
  (advice-add #'embark-completing-read-prompter
              :around #'embark-hide-which-key-indicator))
;; Whichkey and Embark:2 ends here

;; [[file:Sacha.org::#which-key-replacements][Changing the which-key labels for shortcuts:1]]
  (with-eval-after-load 'which-key
          (setopt which-key-allow-multiple-replacements t
                                  which-key-sort-order 'which-key-description-order
                                  which-key-replacement-alist
                                  (seq-map
                                   (lambda (rep)
                                           `((nil . ,(elt rep 0))
                                                   . (nil . ,(elt rep 1))))
                                   '(("my-subed-set-timestamp-to-mpv-position" "set to MPV")
                                           ("my-embark-org-copy-exported-url" "⭐🗐🔗 copy exported URL")
                                           ("my-subed-copy-timestamp-dwim" "copy")
                                           ("my-sketch-insert-file-as-link" "insert")
                                           ("my-geeqie-view" "geeqie")
                                           ("my-journal-edit" "edit")
                                           ("my-org-link-qr" "qr")
                                           ("my-image-open-in-" "")
                                           ("org-babel-" "ob-")
                                           ("next" "🠆")
                                           ("previous" "🠄")
                                           ("my-image-" "")
                                           ("my-embark-org-blog-" "")
                                           ("embark-collect" "⇶ collect")
                                           ("my-embark-org-" "")
                                           ("my-" "")
                                           ("embark-" "")
                                           ("embark-act-all" "all")
                                           ("embark-become" "become")
                                           ("embark-collect" "collect")
                                           ("-" " ")))))
;; Changing the which-key labels for shortcuts:1 ends here

;; [[file:Sacha.org::#keybindings-embark-renaming-and-storing][Renaming and storing:3]]
(with-eval-after-load 'embark
  (defvar-keymap my-embark-image-actions
    :doc "Images"
    "k" #'my-image-open-in-krita
    "a" #'my-image-open-in-annotator
    "i" #'my-image-open-in-inkscape
    "w" #'my-image-copy-text
    "c" #'my-image-autocrop
    "]" #'my-image-rotate-clockwise
    "[" #'my-image-rotate-counterclockwise
    "g" #'my-image-open-in-gimp
    "f" #'my-open-in-firefox
    "s" #'my-image-store
    "r" #'my-image-recognize-and-rename
    "t" #'my-org-sketch-open-text-file
    "T" #'my-image-thumbnail
    "L" #'my-org-svg-copy-links
    "C" #'my-image-recolor
    "d" #'my-image-insert-text-as-details)
  (add-to-list 'embark-keymap-alist '(image . my-embark-image-actions)))
;; Renaming and storing:3 ends here

;; [[file:Sacha.org::#embark-subed][Embark and subed:3]]
(defhydra my-subed-adjust-timestamp ()
  ("<up>" my-subed-adjust-timestamp-up "Up" :exit nil)
  ("<down>" my-subed-adjust-timestamp-down "Down" :exit nil))
;; Embark and subed:3 ends here

;; [[file:Sacha.org::#casual-symbol-overlay][Embark, symbols, and casual-symbol-overlay:1]]
  (use-package casual-symbol-overlay
          :if my-laptop-p
          :after embark
          :init
          (with-eval-after-load 'embark
                  (keymap-set embark-symbol-map "z" #'casual-symbol-overlay-tmenu)))
;; Embark, symbols, and casual-symbol-overlay:1 ends here

;; [[file:Sacha.org::#keybindings-embark-embark-and-erefactor-rename-symbol-in-buffer][Embark and erefactor-rename-symbol-in-buffer:2]]
(with-eval-after-load 'embark
  (keymap-set embark-command-map "r" #'my-embark-erefactor-rename-symbol-in-buffer)
  (keymap-set embark-symbol-map "r" #'my-embark-erefactor-rename-symbol-in-buffer))
;; Embark and erefactor-rename-symbol-in-buffer:2 ends here

;; [[file:Sacha.org::#menus][Menus:1]]
  (define-key-after global-map [menu-bar my-menu] (cons "Shortcuts" (make-sparse-keymap "Custom shortcuts")) 'tools)
  (define-key global-map [menu-bar my-menu journal] '("Show journal entries" . my-show-missing-journal-entries))
  (define-key global-map [menu-bar my-menu agenda] '("Org agenda" . (lambda () (interactive) (org-agenda nil "a"))))
  (define-key global-map [menu-bar my-menu audio] '("Process audio" . (lambda () (interactive) (shell-command "~/bin/process-audio &"))))
  (define-key global-map [menu-bar my-menu new-index-card] '("New index card" . (lambda () (interactive)
                                                                                  (my-org-sketch-edit (my-prepare-index-card-template)))))
;; Menus:1 ends here

;; [[file:Sacha.org::#context-menus][Context menus:1]]
  (add-hook 'text-mode-hook 'context-menu-mode)
  (with-eval-after-load 'dired
          (add-hook 'dired-mode-hook 'context-menu-mode))
  (add-hook 'shell-mode-hook 'context-menu-mode)
;; Context menus:1 ends here

;; [[file:Sacha.org::#hydras][Hydra keyboard shortcuts:1]]
  (use-package hydra :commands defhydra)
  (use-package use-package-hydra)
  (if my-laptop-p
      (use-package hydra-posframe
                          :defer t
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
;; Hydra keyboard shortcuts:3 ends here

;; [[file:Sacha.org::#hydras][Hydra keyboard shortcuts:5]]
(defalias 'my-org-insert-link 'my-org-insert-link-dwim)
;; Hydra keyboard shortcuts:5 ends here

;; [[file:Sacha.org::#hydra-completion][Emacs Hydra: Allow completion when I can't remember the command name:2]]
     (with-eval-after-load 'hydra
       (define-key hydra-base-map (kbd "<tab>") #'my-hydra-execute-extended))
;; Emacs Hydra: Allow completion when I can't remember the command name:2 ends here

;; [[file:Sacha.org::#which-key-and-which-key-posframe][which-key and which-key-posframe:1]]
  (use-package which-key :init (which-key-mode 1))
  (use-package which-key-posframe :if my-laptop-p :init (which-key-posframe-mode 1))
;; which-key and which-key-posframe:1 ends here

;; [[file:Sacha.org::#keybindings-casual][Casual:1]]
  (use-package casual
          :load-path "~/vendor/casual/lisp")
;; Casual:1 ends here

;; [[file:Sacha.org::#keybindings-foot-pedal][Foot pedal:2]]
  ;(keymap-global-set "S-<f1>" #'my-speechd-repeat-sentence)
  ;(keymap-global-set "S-<f3>" #'my-speechd-speak-sentence-and-advance)
;; Foot pedal:2 ends here

;; [[file:Sacha.org::#completion][Completion:1]]
  (global-completion-preview-mode 1)

  (use-package vertico
          :config
          (vertico-mode +1)
          (vertico-multiform-mode)
          (with-eval-after-load 'vertico-multiform
                  (add-to-list 'vertico-multiform-categories '(embark-keybinding grid))))
;; Completion:1 ends here

;; [[file:Sacha.org::#completion][Completion:2]]
  (use-package prescient :config (prescient-persist-mode +1))
                                          ;(use-package company-prescient :init (company-prescient-mode +1))
;; Completion:2 ends here

;; [[file:Sacha.org::#completion-emacs-completion-and-handling-accented-characters-with-orderless][Emacs completion and handling accented characters with orderless:2]]
  (use-package orderless
          :custom
          (completion-styles '(orderless basic))
          (completion-category-overrides '((file (styles basic partial-completion))))
    (orderless-style-dispatchers '(my-orderless-accent-dispatch orderless-affix-dispatch)))
;; Emacs completion and handling accented characters with orderless:2 ends here

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
                                   ("M-g L" . ace-link)
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

;; [[file:Sacha.org::#consult-directory-navigation][Consult directory navigation:1]]
       (use-package consult-dir
              :ensure t
              :bind (("C-x C-d" . consult-dir)
                     :map minibuffer-local-completion-map
                     ("C-x C-d" . consult-dir)
                     ("C-x C-j" . consult-dir-jump-file)))
;; Consult directory navigation:1 ends here

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
    (add-to-list 'consult-buffer-sources 'my-consult-source-projectile-projects 'append))
;; Using projects as a source for consult-buffer:1 ends here

;; [[file:Sacha.org::#completion-consult-consult-omni-using-web-searches-and-bookmarks-to-quickly-link-placeholders-in-org-mode][Using web searches and bookmarks to quickly link placeholders in Org Mode:2]]
  (defun my-org-set-link-target-with-org-completion ()
          "Replace the current link's target with `org-insert-link' completion.
  Assume the target is actually supposed to be the description.  For
  example, if the link is [[some text]], do a web search for 'some text',
  prompt for the link to use as the target, and move 'some text' to the
  description."
          (interactive)
          (let* ((bracket-pos (org-in-regexp org-link-bracket-re))
                                   (bracket-target (match-string 1))
                                   (bracket-desc (match-string 2))
                                   result)
                  (when (and bracket-pos bracket-target
                                                           (null bracket-desc)
                                                           ;; try to trigger only when the target is plain text and doesn't have a protocol
                                                           (not (string-match ":" bracket-target))
                                                           (org-element-lineage (org-element-context) '(link) t)) ; ignore text in code blocks, etc.
                          ;; we're in a bracketed link with no description and the target doesn't look like a link;
                          ;; likely I've actually added the text for the description and now we need to include the link.
                          ;; This is a hack so that we don't have to delete the link until the new link has been inserted
                          ;; since org-insert-link doesn' tbreak out the link prompting code into a smaller function.
                          (let ((org-link-bracket-re "{{{}}}"))
                                  (goto-char (cdr bracket-pos))
                                  (org-insert-link nil nil bracket-target))
                          (delete-region (car bracket-pos) (cdr bracket-pos)))))
;; Using web searches and bookmarks to quickly link placeholders in Org Mode:2 ends here

;; [[file:Sacha.org::#completion-consult-consult-omni-using-web-searches-and-bookmarks-to-quickly-link-placeholders-in-org-mode][Using web searches and bookmarks to quickly link placeholders in Org Mode:3]]
  (defun my-org-set-link-target-dwim ()
          (interactive)
          (or (my-org-set-link-target-with-search)
                          (my-org-set-link-target-with-org-completion)))
;; Using web searches and bookmarks to quickly link placeholders in Org Mode:3 ends here

;; [[file:Sacha.org::#completion-consult-consult-omni-using-web-searches-and-bookmarks-to-quickly-link-placeholders-in-org-mode][Using web searches and bookmarks to quickly link placeholders in Org Mode:4]]
  (defun my-org-scan-for-untargeted-links ()
          "Look for [[some text]] and prompt for the actual targets."
          (interactive)
          (while (re-search-forward org-link-bracket-re nil t)
                  (when (and
                                           (not (match-string 2))
                                           (and (match-string 1) (not (string-match ":" (match-string 1))))
                                           (org-element-lineage (org-element-context) '(link) t)) ; ignore text in code blocks, etc.
                          (undo-boundary)
                          (my-org-set-link-target-dwim))))
;; Using web searches and bookmarks to quickly link placeholders in Org Mode:4 ends here

;; [[file:Sacha.org::#completion-consult-consult-omni-bookmarks][Bookmarks:1]]
  (defun my-consult-omni-bookmarks-builder (input &rest args &key callback &allow-other-keys)
          (let* ((quoted (when input (regexp-quote input)))
                                   (list (my-org-bookmarks))
                                   (candidates
                                          (mapcar
                                           (lambda (o)
                                                   (propertize
                                                          (concat (plist-get o :title) "\s"
                                                                                          (plist-get o :url))
                                                          :source "Bookmarks"
                                                          :on-callback 'my-consult-org-bookmark-visit
                                                          :title (plist-get o :title)
                                                          :url (plist-get o :url)))
                                           (if quoted
                                                           (seq-filter
                                                                  (lambda (o)
                                                                          (string-match quoted (concat (plist-get o :title) " - " (plist-get o :title))))
                                                                  list)
                                                   list))))
                  (when callback (funcall callback candidates))
                  candidates))

  (defun my-consult-org-bookmark-visit (o)
          (browse-url (get-text-property 0 :url o)))

  ;; (consult--multi (list my-consult--source-org-bookmark))
  (with-eval-after-load 'consult-omni
    (consult-omni-define-source
           "My Org bookmarks"
     :narrow-char ?b
     :type 'sync
           :request #'my-consult-omni-bookmarks-builder
           :on-return 'my-consult-org-bookmark-visit
           :group #'consult-omni--group-function
     :min-input 1
     :require-match t))
;; Bookmarks:1 ends here

;; [[file:Sacha.org::#completion-consult-consult-omni-blog-posts][Finding my blog posts with consult-omni:1]]
  (defun my-consult-omni-blog-data ()
          (let ((base (replace-regexp-in-string "/$" "" my-blog-base-url))
                                  (json-object-type 'alist)
                                  (json-array-type 'list))
                  (mapcar
                   (lambda (o)
                           (list :url (concat base (alist-get 'permalink o))
                                                   :title (alist-get 'title o)
                                                   :date (alist-get 'date o)))
                   (sort (json-read-file "~/sync/static-blog/_site/blog/all/index.json")
                                           (lambda (a b)
                                                   (string< (or (alist-get 'date b) "")
                                                                                          (or (alist-get 'date a) "")))))))
  (unless (get 'my-consult-omni-blog-data :memoize-original-function)
          (memoize #'my-consult-omni-blog-data "5 minutes"))

  (defun my-consult-omni-blog-titles-builder (input &rest args &key callback &allow-other-keys)
          (let* ((quoted (when input (regexp-quote input)))
                                   (list
                                          (if quoted
                                                          (seq-filter
                                                           (lambda (o)
                                                                   ;; TODO: Someday figure out orderless?
                                                                   (string-match quoted (concat (plist-get o :title) " - " (plist-get o :title))))
                                                           (my-consult-omni-blog-data))
                                                  (my-consult-omni-blog-data)))
                                   (candidates
                                          (mapcar
                                           (lambda (o)
                                                   (propertize
                                                          (concat (plist-get o :title))
                                                          :source "Blog"
                                                          :date (plist-get o :date)
                                                          :title (plist-get o :title)
                                                          :url (plist-get o :url)))
                                           (if quoted (seq-take list 3) list))))
                  (when callback (funcall callback candidates))
                  candidates))

  (defun my-consult-omni-blog-annotation (s)
          (format " (%s)"
                                          (propertize (substring (or (get-text-property 0 :date s) "") 0 4)
                                                                                          'face 'completions-annotations)))

  (with-eval-after-load 'consult-omni
    (consult-omni-define-source
           "Blog"
           :narrow-char ?b
     :type 'sync
           :request #'my-consult-omni-blog-titles-builder
           :on-return 'my-consult-org-bookmark-visit
     :group #'consult-omni--group-function
           :annotate #'my-consult-omni-blog-annotation
     :min-input 3
           :sort nil
     :require-match t))
;; Finding my blog posts with consult-omni:1 ends here

;; [[file:Sacha.org::#searching-my-blog][Searching my blog, notes, and sketches with consult-ripgrep and consult-omni:1]]
  (defun my-search-notes ()
          (interactive)
          (consult-ripgrep '("~/sync/orgzly" "~/sync/static-blog/blog" "~/sync/sketches" "~/sync/topics")))
  (defun my-search-public-notes ()
          (interactive)
          (consult-ripgrep '("~/sync/static-blog/blog" "~/sync/sketches" "~/sync/topics")))
;; Searching my blog, notes, and sketches with consult-ripgrep and consult-omni:1 ends here

;; [[file:Sacha.org::#searching-my-blog][Searching my blog, notes, and sketches with consult-ripgrep and consult-omni:2]]
  (cl-defun my-consult-omni--google-blog-fetch-results (input &rest args &key callback &allow-other-keys)
    "Fetches search results for INPUT from “Google Custom Search” service.
  Narrows to `my-blog-base-url'.

  Refer to URL `https://programmablesearchengine.google.com/about/' and
  URL `https://developers.google.com/custom-search/' for more info."
    (pcase-let* ((`(,query . ,opts)
                  (consult-omni--split-command input (seq-difference args (list :callback callback))))
                 (opts (car-safe opts))
                 (count (plist-get opts :count))
                 (page (plist-get opts :page))
                 (filter (plist-get opts :filter))
                 (count (or (and count (integerp (read count)) (string-to-number count))
                            consult-omni-default-count))
                 (page (or (and page (integerp (read page)) (string-to-number page))
                           consult-omni-default-page))
                 (filter (or (and (integerp filter) filter)
                             (and filter (string-to-number (format "%s" filter)))
                             1))
                 (filter (if (member filter '(0 1)) filter 1))
                 (count (min count 10))
                 (page (+ (* page count) 1))
                 (page (min page (- 100 count)))
                 (params `(("q" . ,(format "site:%s+%s"
                                           (url-encode-url my-blog-base-url)
                                           (replace-regexp-in-string " " "+" query)))
                           ("key" . ,(consult-omni-expand-variable-function consult-omni-google-customsearch-key))
                           ("cx" . ,(consult-omni-expand-variable-function consult-omni-google-customsearch-cx))
                           ("gl" . "en")
                           ("filter" . ,(format "%s" filter))
                           ("num" . ,(format "%s" count))
                           ("start" . ,(format "%s" page))))
                 (headers '(("Accept" . "application/json")
                            ("Accept-Encoding" . "gzip")
                            ("User-Agent" . "consult-omni (gzip)"))))
      (consult-omni--fetch-url
       consult-omni-google-customsearch-api-url consult-omni-http-retrieve-backend
       :encoding 'utf-8
       :params params
       :headers headers
       :parser #'consult-omni--json-parse-buffer
       :callback
       (lambda (attrs)
         (let* ((raw-results (gethash "items" attrs))
                (annotated-results
                 (mapcar (lambda (item)
                           (let*
                               ((source "Google")
                                (url (format "%s" (gethash "link" item)))
                                (title (format "%s" (gethash "title" item)))
                                (snippet (string-trim (format "%s" (gethash "snippet" item))))
                                (search-url (consult-omni--make-url-string consult-omni-google-search-url params '("key" "cx" "gl")))
                                (decorated (funcall consult-omni-default-format-candidate :source source :query query :url url :search-url search-url :title title :snippet snippet)))
                             (propertize decorated
                                         :source source
                                         :title title
                                         :url url
                                         :search-url search-url
                                         :query query
                                         :snippet snippet)))
                         raw-results)))
           (when (and annotated-results (functionp callback))
             (funcall callback annotated-results))
           annotated-results)))))

  (use-package consult-omni
          :load-path "~/vendor/consult-omni"
          :after (consult embark)
          :config
    (consult-omni-define-source
     "Google my blog"
     :narrow-char ?b
     :type 'dynamic
     :require-match nil
     :face 'consult-omni-engine-title-face
     :request #'my-consult-omni--google-blog-fetch-results
     :on-new (apply-partially #'consult-omni-external-search-with-engine "Google")
     :preview-key consult-omni-preview-key
     :search-hist 'consult-omni--search-history
     :select-hist 'consult-omni--selection-history
     :enabled (lambda () (bound-and-true-p consult-omni-google-customsearch-key))
     :group #'consult-omni--group-function
     :sort t
     :interactive consult-omni-intereactive-commands-type
     :annotate nil))
;; Searching my blog, notes, and sketches with consult-ripgrep and consult-omni:2 ends here

;; [[file:Sacha.org::#searching-my-blog][Searching my blog, notes, and sketches with consult-ripgrep and consult-omni:3]]
  (keymap-global-set "M-g b" #'my-search-public-notes)
  (keymap-global-set "M-g N" #'my-search-notes)
  (keymap-global-set "M-g B" #'consult-omni-google-my-blog)
;; Searching my blog, notes, and sketches with consult-ripgrep and consult-omni:3 ends here

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

;; [[file:Sacha.org::#cargo-culted-stuff][Cargo-culted stuff:1]]
  (defun my-store-action-key+cmd (cmd)
    (setq keycast--this-command-keys (this-single-command-keys) keycast--this-command cmd))
  (defun my-force-keycast-update (&rest _)
    (force-mode-line-update t))
  (use-package keycast
    :if my-laptop-p
    :after embark
          :defer t
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

;; [[file:Sacha.org::#color-theme-sometimes-comes-across-lists-odd][color-theme sometimes comes across lists. Odd!:1]]
  (defadvice face-attribute (around sacha activate)
    (if (symbolp (ad-get-arg 0))
        ad-do-it))
;; color-theme sometimes comes across lists. Odd!:1 ends here

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

;; [[file:Sacha.org::#display][Display:2]]
  (add-to-list 'display-buffer-alist
               '("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
                 (display-buffer-no-window)
                 (allow-no-window . t)))
;; Display:2 ends here

;; [[file:Sacha.org::#set-up-a-light-on-dark-color-scheme][Set up a color scheme:1]]
  (defun my-setup-color-theme ()
    (interactive)
    (when (display-graphic-p)
      (load-theme (car modus-themes-to-toggle))))
  (use-package modus-themes
          :quelpa (modus-themes :fetcher github :repo "protesilaos/modus-themes")
          :init (setq modus-themes-to-toggle '(modus-operandi-tinted modus-vivendi-tinted))
          :config (my-setup-color-theme))
;; Set up a color scheme:1 ends here

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

;; [[file:Sacha.org::#time-in-the-modeline][Time in the modeline:1]]
  (display-time-mode 1)
;; Time in the modeline:1 ends here

;; [[file:Sacha.org::#diminish][Diminish mode names in modeline:1]]
  (use-package diminish :ensure t)
;; Diminish mode names in modeline:1 ends here

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

;; [[file:Sacha.org::#navigation-expand-region][Expand region:1]]
  (use-package expand-region
    :bind ("C-=" . er/expand-region)
          )
;; Expand region:1 ends here

;; [[file:Sacha.org::selected-config][selected-config]]
  (use-package ctrlf
    ;:init (ctrlf-mode +1)   ; I'm also getting used to isearch-lazy-count, though.
    )
  (use-package selected
    :init (selected-global-mode 1)
    :bind (:map selected-keymap
                            ("q" . selected-off)
                            ("u" . upcase-dwim)
                            ("d" . downcase-dwim)
                            ("w" . kill-ring-save)
                            ("n" . next-line)
                            ("p" . previous-line)
                            ("f" . forward-char)
                            ("b" . backward-char)
                            ("F" . forward-word)
                            ("B" . backward-word)
                            ("a" . beginning-of-visual-line)
                            ("e" . end-of-visual-line)
                            ("{" . backward-paragraph)
                            ("}" . forward-paragraph)
                            ("(" . backward-sentence)
                            (")" . forward-sentence)
                            ("r" . ctrlf-backward-fuzzy)
                            ("[" . scroll-down-line)
                            ("]" . scroll-up-line)
                            ("M" . rectangle-mark-mode)
                            ("R" . replace-rectangle)
                            ("x" . exchange-point-and-mark)))
;; selected-config ends here

;; [[file:Sacha.org::#navigation-copy-and-append-string][Copy and append string:1]]
  (defvar my-copy-append-string nil "String to append when copying.")
  (defun my-copy-and-append (beg end append)
          (interactive
           (list
                  (if (region-active-p) (region-beginning) (point-min))
                  (if (region-active-p) (region-end) (point-max))
                  (if current-prefix-arg (read-string "Append: ")
                          my-copy-append-string)))
          (when append
                  (setq my-copy-append-string append))
          (kill-new (concat (buffer-substring beg end) append)))
;; Copy and append string:1 ends here

;; [[file:Sacha.org::#pdf-copy][Copy text from current PDFview page in other window:1]]
  (defun my-pdf-view-insert-current-page-text ()
          (interactive)
          (let (text)
                  (catch 'found
                          (walk-window-tree
                           (lambda (win)
                                   (with-selected-window win
                                           (when (derived-mode-p 'pdf-view-mode)
                                                   (setq text
                                                                           (pdf-info-gettext (pdf-view-current-page)
                                                                                                                                                   (list 0 0 1 1)))
                                                   (throw 'found text))))))
                  (when text (save-excursion (insert text)))))
;; Copy text from current PDFview page in other window:1 ends here

;; [[file:Sacha.org::#navigation-links][Links:1]]
  (use-package ace-link)
;; Links:1 ends here

;; [[file:Sacha.org::#navigation-jumping-between-windows][Jumping between windows:1]]
  (use-package ace-window
          :config
          (setq aw-keys '(?a ?o ?e ?u ?h ?t ?n ?s))
          :bind
          ("M-o" . 'ace-window)
          ("C-x o" . 'ace-window)
          )
;; Jumping between windows:1 ends here

;; [[file:Sacha.org::#navigation-get-the-hang-of-using-vundo][Get the hang of using vundo:1]]
  (use-package vundo)
;; Get the hang of using vundo:1 ends here

;; [[file:Sacha.org::#navigation-focus-on-the-current-window][Focus on the current window:1]]
  ;; `prot/window-single-toggle' is based on `windower' by Pierre
  ;; Neidhardt (ambrevar on GitLab)
  (use-package emacs
    :config
    (defvar prot/window-configuration nil
      "Current window configuration.
  Intended for use by `prot/window-monocle'.")

    (define-minor-mode prot/window-single-toggle
      "Toggle between multiple windows and single window.
  This is the equivalent of maximising a window.  Tiling window
  managers such as DWM, BSPWM refer to this state as 'monocle'."
      :lighter " [M]"
      :global nil
      (if (one-window-p)
          (when prot/window-configuration
            (set-window-configuration prot/window-configuration))
        (setq prot/window-configuration (current-window-configuration))
        (delete-other-windows)))

    (defun prot/kill-buffer-current (&optional arg)
      "Kill current buffer or abort recursion when in minibuffer."
      (interactive "P")
      (if (minibufferp)
          (abort-recursive-edit)
        (kill-buffer (current-buffer)))
      (when (and arg
                 (not (one-window-p)))
        (delete-window)))
    :bind (("s-m" . prot/window-single-toggle)
           ("s-k" . prot/kill-buffer-current)))
;; Focus on the current window:1 ends here

;; [[file:Sacha.org::#navigation-focus-on-the-current-window][Focus on the current window:2]]
  (defun my-maybe-restore-other-windows (orig-fun &rest args)
          (when (called-interactively-p 'any)
                  (if (frame-root-window-p (selected-window))
                                  (call-interactively 'winner-undo)
                          (let ((ignore-window-parameters t))
                                  (apply orig-fun args)))))
  (advice-add 'delete-other-windows
                                                  :around #'my-maybe-restore-other-windows)
;; Focus on the current window:2 ends here

;; [[file:Sacha.org::#navigation-get-scroll-other-window-to-work-with-pdfs][Get scroll-other-window to work with PDFs:1]]
  (use-package scroll-other-window
          :vc (:url "https://gist.github.com/politza/3f46785742e6e12ba0d1a849f853d0b9")
          :commands sow-mode
          :init (sow-mode 1))
;; Get scroll-other-window to work with PDFs:1 ends here

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

;; [[file:Sacha.org::#winner-mode-undo-and-redo-window-configuration][Winner mode - undo and redo window configuration:1]]
  (use-package winner
    :init
          (winner-mode 1))
;; Winner mode - undo and redo window configuration:1 ends here

;; [[file:Sacha.org::#sort-read-file-name][Sort files in read-file-name:1]]
  (defcustom file-name-completions-sort-function #'files-sort-modification-time
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

  (defun files-sort-modification-time (files)
    "Sort FILES list with respect to modification time."
    (sort
     files
           :key (lambda (f) (file-attribute-modification-time (file-attributes f)))
           :lessp #'time-less-p
           :reverse t))

  (defun ad-completion-file-name-table (fun string pred action)
    "Add 'display-sort-function' to metadata.
  If the completion action is metadata, add
  `file-name-completions-sort-function' as display-sort-function.
  Otherwise call FUN with STRING, PRED and ACTION as arguments."
    (if (and (functionp file-name-completions-sort-function)
                                           (eq action 'metadata))
        `(metadata
                                  (category . file)
                                  (cycle-sort-function . identity)
                                  (display-sort-function . ,file-name-completions-sort-function))
      (funcall fun string pred action)))

  (advice-add 'completion-file-name-table :around #'ad-completion-file-name-table)
;; Sort files in read-file-name:1 ends here

;; [[file:Sacha.org::#navigation-downloaded-files][Downloaded files:1]]
  (defvar my-download-dir "~/Downloads")
  (defun my-open-latest-download ()
    (interactive)
    (find-file (my-latest-file my-download-dir)))

  (defun my-attach-and-link-latest-download ()
    (interactive)
    (org-attach-attach (my-latest-file my-download-dir) nil 'cp)
    (org-insert-link nil (caar org-stored-links)))

  (defun my-link-latest-download ()
    (interactive)
    (org-insert-link nil (concat "file:" (my-latest-file my-download-dir))
                                                                           (file-name-nondirectory (my-latest-file my-download-dir))))

  (defun my-include-latest-download ()
          (interactive)
          (my-insert-file-as-org-include (my-latest-file my-download-dir)))

  (defun my-copy-latest-download (dest &optional force)
    (interactive "FDestination: ")
    (copy-file (my-latest-file my-download-dir) dest force))
  (defun my-download-dired ()
          (interactive)
          (dired my-download-dir "-lt"))
;; Downloaded files:1 ends here

;; [[file:Sacha.org::#navigation-downloaded-files-replace-with-latest-download][Replace with latest download:1]]
  (defun my-replace-with-latest-download ()
    "Replace file contents with latest download."
    (interactive)
    (widen)
    (erase-buffer)
    (insert-file-contents (my-latest-file my-download-dir)))
;; Replace with latest download:1 ends here

;; [[file:Sacha.org::#searching][Searching:1]]
  (setopt isearch-lazy-count t)
;; Searching:1 ends here

;; [[file:Sacha.org::#searching][Searching:2]]
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
;; Searching:2 ends here

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

;; [[file:Sacha.org::#navigation-searching-transient-for-isearch][Transient for isearch:1]]
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

;; [[file:Sacha.org::#navigation-searching-search-invisible-text][Search invisible text:1]]
  (setq isearch-invisible t
                          search-invisible t)
;; Search invisible text:1 ends here

;; [[file:Sacha.org::#navigation-searching-occur][Occur:1]]
  (with-eval-after-load 'occur
          (keymap-set occur-mode-map "C-x C-q" #'occur-edit-mode))
;; Occur:1 ends here

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

;; [[file:Sacha.org::#pop-to-mark][Pop to mark:1]]
  (bind-key "C-x p" 'pop-to-mark-command)
  (setq set-mark-command-repeat-pop t)
;; Pop to mark:1 ends here

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

;; [[file:Sacha.org::#highlight-line-mode][Highlight the current line while still being able to easily customize/describe underlying faces:1]]
  (add-hook 'prog-mode-hook 'hl-line-mode)
  (add-hook 'text-mode-hook 'hl-line-mode)
;; Highlight the current line while still being able to easily customize/describe underlying faces:1 ends here

;; [[file:Sacha.org::#highlight-line-mode][Highlight the current line while still being able to easily customize/describe underlying faces:2]]
  (defun my-suggest-other-faces (func &rest args)
          (if hl-line-mode
                          (progn
                                  (hl-line-mode -1)
                                  (prog1 (apply func args)
                                          (hl-line-mode 1)))
                  (apply func args)))
  (advice-add #'face-at-point :around #'my-suggest-other-faces)
;; Highlight the current line while still being able to easily customize/describe underlying faces:2 ends here

;; [[file:Sacha.org::#windmove-switching-between-windows][Windmove - switching between windows:1]]
  (use-package windmove
    :bind
    (("<f2> <right>" . windmove-right)
     ("<f2> <left>" . windmove-left)
     ("<f2> <up>" . windmove-up)
     ("<f2> <down>" . windmove-down)
     ))
;; Windmove - switching between windows:1 ends here

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
      ("f" "~/sync/orgzly/journal-fr.org" "French journal")
      ("F" "~/sync/orgzly/french.org" "French")
      ("I" "~/sync/orgzly/computer-inbox.org" "Computer inbox")
      ("i" "~/sync/orgzly/Inbox.org" "Phone inbox")
      ("o" "~/sync/orgzly/organizer.org" "Main org file")
      ("s" "~/proj/stream/index.org" "Yay Emacs")
      ("b" "~/sync/orgzly/business.org" "Business")
      ("P" "/scp:web:/mnt/prev/home/sacha/planet/data/feeds.json" "Planet Emacsen")
      ("p" "~/sync/orgzly/posts.org" "Posts")
      ("m" "~/sync/web/beginner-map.org" "Map")
      ("n" "/ssh:web|sudo::/etc/nginx/sites-available" "Nginx sites")
      ("w" "~/Dropbox/public/sharing/index.org" "Sharing index")
      ("W" "~/Dropbox/public/sharing/blog.org" "Blog index")
      ("1" "~/proj/static-blog/" "Static blog")
      ("r" "~/sync/orgzly/reference.org" "Reference")
      ("R" "~/personal/reviews.org" "Reviews")
      ("v" "~/proj/emacstv.github.io/videos.org" "Videos")
      ("g" "~/proj/sachac.github.io/evil-plans/index.org" "Evil plans"))
    :bind
    ("C-c f" . #'my-file-shortcuts/body))
;; Frequently-accessed files:1 ends here

;; [[file:Sacha.org::#navigation-c-g-improvement][C-g improvement:1]]
  (defun prot/keyboard-quit-dwim ()
    "Do-What-I-Mean behaviour for a general `keyboard-quit'.

  The generic `keyboard-quit' does not do the expected thing when
  the minibuffer is open.  Whereas we want it to close the
  minibuffer, even without explicitly focusing it.

  The DWIM behaviour of this command is as follows:

  - When the region is active, disable it.
  - When a minibuffer is open, but not focused, close the minibuffer.
  - When the Completions buffer is selected, close it.
  - In every other case use the regular `keyboard-quit'."
    (interactive)
    (cond
     ((region-active-p)
      (keyboard-quit))
     ((derived-mode-p 'completion-list-mode)
      (delete-completion-window))
     ((> (minibuffer-depth) 0)
      (abort-recursive-edit))
     (t
      (keyboard-quit))))

  (define-key global-map (kbd "C-g") #'prot/keyboard-quit-dwim)
;; C-g improvement:1 ends here

;; [[file:Sacha.org::#smartscan][Smartscan:1]]
  (use-package smartscan
    :if my-laptop-p
    :defer t
    :config (global-smartscan-mode t))
;; Smartscan:1 ends here

;; [[file:Sacha.org::#dired][Dired:1]]
  (setq dired-listing-switches "-altr")
  (setq dired-dwim-target 'dired-dwim-target-next)
;; Dired:1 ends here

;; [[file:Sacha.org::#dired][Dired:2]]
  (require 'find-dired)
  (setq find-ls-option '("-print0 | xargs -0 ls -ld" . "-ld"))
;; Dired:2 ends here

;; [[file:Sacha.org::#dired][Dired:3]]
  (use-package dired-subtree
    :ensure t
    :after dired
    :bind
    ( :map dired-mode-map
      ("<tab>" . dired-subtree-toggle)
      ("TAB" . dired-subtree-toggle)
      ("<backtab>" . dired-subtree-remove)
      ("S-TAB" . dired-subtree-remove))
    :config
    (setq dired-subtree-use-backgrounds nil))
;; Dired:3 ends here

;; [[file:Sacha.org::screenshots][screenshots]]
  (defvar my-screenshot-dirs
          '("~/recordings"
                  "~/.var/app/org.prismlauncher.PrismLauncher/data/PrismLauncher/instances/"
                  "~/sync/gdlauncher-instances/"
                  ))
  (defvar my-recent-screenshot-limit 50)

  (defun my-combined-screenshots (&optional limit)
          (seq-take
           (sort
                  (seq-mapcat (lambda (dir)
                                                                          (directory-files-recursively dir "[0-9][0-9][0-9][0-9]-.*\\.\\(png\\|webm\\|gif\\|svg\\|mkv\\)"))
                                                                  my-screenshot-dirs)
                  :key (lambda (o) (file-attribute-modification-time (file-attributes o)))
                  :reverse t)
           (or limit my-recent-screenshot-limit)))

  (defun my-latest-screenshot ()
          (car (my-combined-screenshots)))

  (defun my-show-combined-screenshots (&optional limit)
          "Show thumbnails for combined screenshots."
          (interactive (list (when current-prefix-arg (read-number "Limit: "))))
          (condition-case nil
                          ;; ignore errors from image-dired trying to set default-directory
                          (image-dired-show-all-from-dir
                           (cons (car my-screenshot-dirs) (my-combined-screenshots limit)))
                  (error nil)))
;; screenshots ends here

;; [[file:Sacha.org::my-org-image-dired-store-link][my-org-image-dired-store-link]]
  (defun my-org-image-dired-store-link ()
          (when (and (derived-mode-p 'image-dired-thumbnail-mode)
                                                   (get-text-property (point) 'original-file-name))
                  (org-link-store-props
                   :link (concat "file:" (get-text-property (point) 'original-file-name)))))

  (with-eval-after-load 'org
          (org-link-set-parameters
           "image-dired"
           :store #'my-org-image-dired-store-link))
;; my-org-image-dired-store-link ends here

;; [[file:Sacha.org::#image-dired-screenshots][Using image-dired to browse the latest screenshots from multiple directories:3]]
  (defun my-org-yank-file-links-from-kill-ring ()
          (interactive)
          (dolist (file (read (concat "(" (current-kill 0) ")")))
                          (insert (org-link-make-string (concat "file:" file)) "\n")))
;; Using image-dired to browse the latest screenshots from multiple directories:3 ends here

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

;; [[file:Sacha.org::#recent-files][Recent files:1]]
  (require 'recentf)
  (setq recentf-max-saved-items 200
        recentf-max-menu-items 15)
  (recentf-mode)
;; Recent files:1 ends here

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

;; [[file:Sacha.org::#link-hint][link-hint:1]]
  (use-package link-hint
    :bind
    ("M-g u" . link-hint-open-link)
    ("M-g U" . link-hint-open-multiple-links))
;; link-hint:1 ends here

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

;; [[file:Sacha.org::#random][Randomness for serendipity:1]]
  (defun my-goto-random-char ()
    (interactive)
    (goto-char (random (point-max))))
;; Randomness for serendipity:1 ends here

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

;; [[file:Sacha.org::#shuffling-lines][Shuffling lines:1]]
  (defun my-shuffle-lines-in-region (beg end)
    (interactive "r")
    (let ((list (split-string (buffer-substring beg end) "[\r\n]+")))
      (delete-region beg end)
      (insert (string-join (seq-sort-by (lambda (_) (random)) #'<= list) "\n"))))
;; Shuffling lines:1 ends here

;; [[file:Sacha.org::#network-tramp-and-editing-files-over-ssh][Network: TRAMP and editing files over SSH:1]]
  (when (eq system-type 'windows-nt)
    (setq tramp-default-method "plink")
    (setq tramp-auto-save-directory "c:\\sacha\\tmp"))
;; Network: TRAMP and editing files over SSH:1 ends here

;; [[file:Sacha.org::#network-tramp-and-editing-files-over-ssh][Network: TRAMP and editing files over SSH:2]]
  (use-package tramp-hlo
      :ensure t
      :config
      (tramp-hlo-setup))
;; Network: TRAMP and editing files over SSH:2 ends here

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
          :defer t
    )
;; Reading:2 ends here

;; [[file:Sacha.org::#writing-and-editing][Writing and editing:1]]
  (keymap-global-set "M-c" #'my-capitalize-dwim)
  (setq-default fill-column 50)
  (keymap-global-set "M-o" #'join-line)
  (keymap-global-set "M-T" #'transpose-sentences)  ; https://www.matem.unam.mx/~omar/apropos-emacs.html#writing-experience

  (defun my-capitalize-dwim ()
    "Capitalize the previous word if at the end of a word."
    (interactive)
    (if (region-active-p)
        (capitalize-region (region-beginning) (region-end))
      (when (and (not (bolp))
                 (looking-back "\\w" 1)
                 (not (eq last-command 'my-capitalize-dwim)))
        (backward-word))
      (capitalize-word 1)))

  ;; Bind it to the original M-c key
  (global-set-key (kbd "M-c") 'my-capitalize-dwim)

  (defun my-copy-filename ()
    (interactive)
    (cond
     ((derived-mode-p 'dired-mode) (dired-copy-filename-as-kill 0))
     (t (kill-new (buffer-file-name)))))
;; Writing and editing:1 ends here

;; [[file:Sacha.org::#multimedia-learning-french][Learning French:1]]
  (use-package learn-lang :load-path "~/proj/learn-lang"
    :preface (load "~/proj/learn-lang/learn-lang-autoloads.el" nil t)
    :config
    (setq learn-lang-language "fr")
    (setq learn-lang-tatoeba-files
          '(("fr" . "~/proj/french/tatoeba-fr-en.tsv")))
    )
;; Learning French:1 ends here

;; [[file:Sacha.org::#multimedia-learning-french][Learning French:2]]
  (setq search-default-mode 'char-fold-to-regexp)
  ;; https://emacs.stackexchange.com/questions/75521/convert-accented-characters-to-non-accented-counterparts-character-folding
  (defun my-translate-unaccented (s)
    (mapconcat
     (lambda (c)
       (char-to-string
        (car (get-char-code-property c 'decomposition))))
     s ""))

  (defvar-keymap my-lang-map
    "l" (cons "lookup" #'my-lang-lexique-complete-word)
    "w" (cons "wordref" #'my-lang-wordreference-lookup)
    "c" (cons "conj" #'my-lang-conjugate)
    "f" (cons "→ fr" #'my-lang-consult-en-fr)
    "s" (cons "say" #'my-lang-say-word-at-point)
    "t" (cons "→ en" #'my-lang-translate-dwim))
  (fset 'my-lang-map my-lang-map)

  (with-eval-after-load 'org
    (keymap-set org-mode-map "C-," my-lang-map)
    (keymap-set org-mode-map "C-c u" my-lang-map))

  (with-eval-after-load 'org
    (keymap-set message-mode-map "C-," my-lang-map)
    )

  (with-eval-after-load 'flyspell
    (keymap-set flyspell-mode-map "C-," my-lang-map))


  (use-package wiktionary-bro
    :config
    (setq wiktionary-bro-language "fr")
    )

  (defun my-lang-chrome-speech-new-session ()
    (interactive)
    (my-chrome-speech-new-session "french" "fr-FR"))

  (use-package flycheck-grammalecte
    :config
    (setq flycheck-grammalecte-report-apos nil)
    (setq flycheck-grammalecte-report-nbsp nil)
    (setq flycheck-grammalecte-report-esp nil)
    (with-eval-after-load 'flycheck
      (flycheck-grammalecte-setup)))
;; Learning French:2 ends here

;; [[file:Sacha.org::#writing-and-editing-learning-french-practice-pronunciation][Practice pronunciation:1]]
  (defvar my-practice-dir "~/proj/french/audio")
  (defvar my-practice-temp (expand-file-name "temp.wav" my-practice-dir))
  (defun my-practice-line ()
    (interactive)
    (sit-for 1)
    (let* ((date (format-time-string "%Y-%m-%d-%H-%M-%S"))
           (filename
            (expand-file-name
             (concat date " "
                     (string-trim
                      (buffer-substring (line-beginning-position)
                                        (line-end-position)))
                     ".opus")
             my-practice-dir))
           (process (start-process "arecord"
                                   (get-buffer-create "*record*")
                                   "arecord" "-D" "hw:2,0" "-t" "wav" "-f" "cd"
                                   my-practice-temp)))
      (read-key "Press a key")
      (kill-process process)
      (call-process "ffmpeg" nil (get-buffer-create "*record*") nil
                    "-y"
                    "-i"
                    my-practice-temp
                    "-af" "silenceremove=start_periods=1:start_duration=0:start_threshold=-60dB,areverse,silenceremove=start_periods=1:start_duration=0:start_threshold=-60dB,areverse,loudnorm=I=-16:LRA=11:TP=-1.5"
                    filename)
      (mpv-play filename)))

  (defun my-practice-replay ()
    (interactive)
    (mpv-play (my-latest-file my-practice-dir)))

  (defun my-practice-transcribe ()
    (interactive)
    (let* ((default-directory my-practice-dir)
           (file (my-latest-file my-practice-dir "\\.opus\\|\\.m4a\\|\\.webm")))
      ;; (call-process "/bin/bash" nil nil nil "/home/sacha/bin/whisperx" (expand-file-name file))
      (message "%s"
               (with-temp-buffer
                 (insert-file-contents (concat (file-name-sans-extension file) ".txt"))
                 (string-trim (buffer-string))))))

  (defun my-practice-play-current-reference (&optional beg end)
    "Play the current segment."
    (interactive)
    (when (derived-mode-p 'subed-mode)
      (let ((comment (subed-subtitle-comment)))
        (cond
         ((string-match "#\\+REFERENCE: \\(.+\\) +\\(.+?\\) +--> +\\([^ ]+\\)" comment)
          (let ((file (expand-file-name (match-string 1 comment)))
                (start (match-string 2 comment))
                (stop (match-string 3 comment)))
            (call-process "mpv" nil nil nil
                          file
                          (format "--start=%.3f" (/ (subed-timestamp-to-msecs start) 1000.0))
                          (format "--end=%.3f" (/ (subed-timestamp-to-msecs stop) 1000.0)))))
         ((string-match "#\\+REFERENCE: \\(.+\\)" comment)
          (call-process "mpv" nil nil nil (match-string 1 comment)))
        (t (let ((start
                 (if beg (save-excursion
                           (goto-char beg)
                           (subed-subtitle-msecs-start))
                   (subed-subtitle-msecs-start)))
                (stop
                 (if beg (save-excursion
                           (goto-char end)
                           (subed-subtitle-msecs-stop))
                   (subed-subtitle-msecs-stop))))
            (call-process "mpv" nil nil nil (subed-media-file)
                          (format "--start=%.3f" (/ start 1000.0))
                          (format "--end=%.3f" (/ stop 1000.0)))))))))

  (defun my-practice-record-loop (&optional extra)
    (interactive
     (list (cond
            ((and (derived-mode-p 'subed-mode)
                  (region-active-p))
             (subed-subtitle-list-text
              (subed-subtitle-list
               (region-beginning)
               (region-end))))
            ((region-active-p)
             (concat
              " "
              (string-trim
               (buffer-substring (region-beginning) (region-end)))))
            ((derived-mode-p 'subed-mode)
             (concat " " (subed-subtitle-text))))))
    (let* ((date (format-time-string "%Y-%m-%d-%H-%M-%S"))
           (key-prompt "%d (SPC to review, RET get feedback, q to quit, any other key to retry")
           (filename
            (expand-file-name
             (concat date
                     (if extra
                         (concat " "
                                 (string-trim (replace-regexp-in-string
                                               "[\\?]+" " "
                                               (car
                                                (split-string
                                                 extra "\n")))))
                       "")

                     ".opus")
             my-practice-dir))
           done
           char
           (count 0)
           process)
      (while (not done)
        (when (derived-mode-p 'subed-mode)
          (if (region-active-p)
              (my-practice-play-current-reference (region-beginning) (region-end))
            (my-practice-play-current-reference)))
        (setq count (1+ count))
        (setq process (start-process "ffmpeg"
                                     (get-buffer-create "*record*")
                                     "ffmpeg" "-y" "-f" "pulse" "-i" "alsa_input.usb-Blue_Microphones_Yeti_Stereo_Microphone_REV8-00.analog-stereo"
                                     my-practice-temp))
        (setq char
              (read-key (format key-prompt
                                count)))
        (while char
          (pcase char
            (?\  (when (process-live-p process)
                   (sit-for 1)
                   (kill-process process))
                 (my-practice-play-current-reference)
                 (call-process "mpv" nil nil nil my-practice-temp)
                 (setq char
                       (read-key (format "%d (SPC to review, RET get feedback, q to quit, any other key to retry"
                                         count))))
            (13 (when (process-live-p process)
                  (sit-for 1)
                  (kill-process process))
                (setq done 'feedback)
                (setq char nil))
            (?w            ; transcribe with Whisper
             (my-practice-transcribe))
            (?q
             (when (process-live-p process) (kill-process process))
             (setq done 'ignore)
             (setq char nil))
            (_
             (when (process-live-p process) (kill-process process))
             (setq char nil)))))
        (when (member done '(keep feedback))
          (call-process "ffmpeg" nil (get-buffer-create "*record*") nil
                        "-y"
                        "-i"
                        my-practice-temp
                        "-af" "loudnorm=I=-16:LRA=11:TP=-1.5"
                        ;; "-af" "silenceremove=start_periods=1:start_duration=0:start_threshold=-100dB,areverse,silenceremove=start_periods=1:start_duration=0:start_threshold=-100dB,areverse,loudnorm=I=-16:LRA=11:TP=-1.5"
                        filename)
          (when (eq done 'feedback)
            (my-lang-get-audio-feedback filename extra t)))))
;; Practice pronunciation:1 ends here

;; [[file:Sacha.org::#writing-and-editing-learning-french-start-the-process-for-transcribing-the-latest-recording][Start the process for transcribing the latest recording:1]]
  (defun my-lang-process-latest-recording (annotation)
    (interactive "MAnnotation: ")
    (let* ((file (my-latest-file my-recordings-dir))
           (audio (expand-file-name
                   (concat (file-name-base file) "-" annotation ".opus")
                   "~/sync/recordings/")))
      (make-process :name "whisperx"
                    :buffer (get-buffer-create "*whisperx*")
                    :command (list
                              "bash"
                              "-c"
                              (format
                               "ffmpeg -y -i %s %s; cd ~/sync/recordings; ~/bin/whisperx %s"
                               (shell-quote-argument file)
                               (shell-quote-argument audio)
                               (shell-quote-argument audio))))
      (message "Started %s" audio)))
;; Start the process for transcribing the latest recording:1 ends here

;; [[file:Sacha.org::#writing-and-editing-learning-french-emacs-lisp-and-nodejs-getting-the-bolded-words-from-a-section-of-a-google-document][Emacs Lisp and NodeJS: Getting the bolded words from a section of a Google Document:1]]
  (defvar my-google-doc-download-command
    (list "nodejs" (expand-file-name "~/bin/download-google-doc-html.cjs")))

  (defun my-google-doc-html (doc-id)
    (when (string-match "https://docs\\.google\\.com/document/d/\\(.+?\\)/" doc-id)
      (setq doc-id (match-string 1 doc-id)))
    (with-temp-buffer
      (apply #'call-process (car my-google-doc-download-command)
             nil t nil (append (cdr my-google-doc-download-command) (list doc-id)))
      (buffer-string)))

  (defun my-google-doc-clean-html (html)
    "Remove links on spaces, replace Google links."
    (let ((dom (with-temp-buffer
                 (insert html)
                 (libxml-parse-html-region))))
      (dom-search
       dom
       (lambda (o)
         (when (eq (dom-tag o) 'a)
           (when (and (dom-attr o 'href)
                      (string-match "https://\\(www\\.\\)?google\\.com/url\\?q=" (dom-attr o 'href)))
             (let* ((parsed (url-path-and-query
                             (url-generic-parse-url (dom-attr o 'href))))
                    (params (url-parse-query-string (cdr parsed))))
               (dom-set-attribute o 'href (car (assoc-default "q" params #'string=)))))
           (let ((text (string= (string-trim (dom-text o)) "")))
             (when (string= text "")
               (setf (car o) 'span))))
         (when (and
                (string-match "font-weight:700" (or (dom-attr o 'style) ""))
                (not (string-match "font-style:normal" (or (dom-attr o 'style) ""))))
           (setf (car o) 'strong))
         (when (dom-attr o 'style)
           (dom-remove-attribute o 'style))))
      ;; bold text is actually represented as font-weight:700 instead
      (with-temp-buffer
        (svg-print dom)
        (buffer-string))))

  (defun my-google-doc-org (doc-id)
    "Return DOC-ID in Org Mode format."
    (pandoc-convert-stdio (my-google-doc-clean-html (my-google-doc-html doc-id)) "html" "org"))
;; Emacs Lisp and NodeJS: Getting the bolded words from a section of a Google Document:1 ends here

;; [[file:Sacha.org::#writing-and-editing-learning-french-emacs-lisp-and-nodejs-getting-the-bolded-words-from-a-section-of-a-google-document][Emacs Lisp and NodeJS: Getting the bolded words from a section of a Google Document:2]]
  (defun my-org-get-subtree-by-name (org-text heading-name)
    "Return ORG-TEXT subtree for HEADING-NAME."
    (with-temp-buffer
      (insert org-text)
      (org-mode)
      (goto-char (point-min))
      (while (re-search-forward " " nil t)
        (replace-match " "))
      (goto-char (point-min))
      (let ((org-trust-scanner-tags t))
        (car (delq nil
                   (org-map-entries
                    (lambda ()
                      (when (string= (org-entry-get (point) "ITEM") heading-name)
                        (buffer-substring (point) (org-end-of-subtree))))))))))
;; Emacs Lisp and NodeJS: Getting the bolded words from a section of a Google Document:2 ends here

;; [[file:Sacha.org::#writing-and-editing-learning-french-emacs-lisp-and-nodejs-getting-the-bolded-words-from-a-section-of-a-google-document][Emacs Lisp and NodeJS: Getting the bolded words from a section of a Google Document:3]]
  (defvar my-lang-words-for-review-context-function 'sentence-at-point)
  (defvar my-lang-tutor-notes-url nil)
  (defun my-lang-tutor-notes (section-name)
    (my-org-get-subtree-by-name
     (my-google-doc-org my-lang-tutor-notes-url)
     section-name))

  (defun my-lang-words-for-review (section)
    "List the bolded words for review in SECTION."
    (let* ((section (my-lang-tutor-notes section))
           results)
      (with-temp-buffer
        (insert section)
        (org-mode)
        (goto-char (point-min))
        (org-map-entries
         (lambda ()
           (org-end-of-meta-data t)
           (unless (looking-at org-heading-regexp)
             (let ((end (save-excursion (org-end-of-subtree))))
               (while (re-search-forward "\\*[^* ].*?\\*" end t)
                 (cl-pushnew
                  (replace-regexp-in-string
                   "[ \n ]+" " "
                   (funcall my-lang-words-for-review-context-function))
                  results
                  :test 'string=)))))))
      (nreverse results)))
;; Emacs Lisp and NodeJS: Getting the bolded words from a section of a Google Document:3 ends here

;; [[file:Sacha.org::#writing-and-editing-learning-french-emacs-lisp-and-nodejs-getting-the-bolded-words-from-a-section-of-a-google-document][Emacs Lisp and NodeJS: Getting the bolded words from a section of a Google Document:5]]
  (defun my-split-string-keep-delimiters (string delimiter)
    (when string
      (let (results pos)
        (with-temp-buffer
          (insert string)
          (goto-char (point-min))
          (setq pos (point-min))
          (while (re-search-forward delimiter nil t)
            (push (buffer-substring pos (match-beginning 0)) results)
            (setq pos (match-beginning 0)))
          (push (buffer-substring pos (point-max)) results)
          (nreverse results)))))

  (ert-deftest my-split-string-keep-delimiters ()
   (should
    (equal (my-split-string-keep-delimiters
            "Beaucoup de gens ont une réaction forte contre l'IA pour plusieurs raisons qui *incluent* le battage médiatique excessif dont elle fait l'objet, son utilisation à mauvais escient, et *l'inondation de banalité* qu'elle produit."
            ", \\| que \\| qui \\| qu'ils? \\| qu'elles? \\| qu'on "
            )
   )))

  (defun my-lang-words-for-review-phrase-context (&optional s)
    (setq s (replace-regexp-in-string " " " " (or s (sentence-at-point))))
    (string-join
     (seq-keep
      (lambda (s)
        (when (string-match "\\*" s)
          (replace-regexp-in-string "^, " "" s)))
      (my-split-string-keep-delimiters s ", \\| parce que \\| que \\| qui \\| qu'ils? \\| qu'elles? \\| qu'on \\| pour "))
     " ... "))

  (ert-deftest my-lang-words-for-review-phrase-context ()
    (should
     (equal (my-lang-words-for-review-phrase-context
             "Je peux consacrer une petite partie de mon *budget* à des essais, mais je ne veux pas travailler davantage pour rentabiliser une dépense plus importante.")
            "Je peux consacrer une petite partie de mon *budget* à des essais")))
;; Emacs Lisp and NodeJS: Getting the bolded words from a section of a Google Document:5 ends here

;; [[file:Sacha.org::#writing-and-editing-learning-french-emacs-lisp-and-nodejs-getting-the-bolded-words-from-a-section-of-a-google-document][Emacs Lisp and NodeJS: Getting the bolded words from a section of a Google Document:7]]
  (defun my-lang-tutor-notes-wdiff-org ()
    (interactive)
    (let ((section (org-entry-get (point) "ITEM")))
      (my-wdiff-strings
       (replace-regexp-in-string
        " " " "
        (my-org-subtree-text-without-blocks))
       (replace-regexp-in-string
        " " " "
        (my-lang-tutor-notes section)))))
;; Emacs Lisp and NodeJS: Getting the bolded words from a section of a Google Document:7 ends here

;; [[file:Sacha.org::#writing-and-editing-learning-french-emacs-and-french-focus-flycheck-grammalecte-on-the-narrowed-part-of-the-buffer][Emacs and French: Focus flycheck-grammalecte on the narrowed part of the buffer:1]]
  (defun my-flycheck-grammalecte-buffer (checker callback)
    (let* ((temp-file-name (make-temp-file "grammalecte"))
           (output-buffer (get-buffer-create temp-file-name))
           (buffer (current-buffer))
           (cmdline (delq nil `("python3"
                                ,(expand-file-name "flycheck_grammalecte.py"
                                                   grammalecte--site-directory)
                                ,(unless flycheck-grammalecte-report-spellcheck "-S")
                                ,(unless flycheck-grammalecte-report-grammar "-G")
                                ,(unless flycheck-grammalecte-report-apos "-A")
                                ,(unless flycheck-grammalecte-report-nbsp "-N")
                                ,(unless flycheck-grammalecte-report-esp "-W")
                                ,(unless flycheck-grammalecte-report-typo "-T")
                                (option-list "-f" flycheck-grammalecte-filters)
                                (eval (flycheck-grammalecte--prepare-arg-list
                                       "-f" flycheck-grammalecte-filters-by-mode))
                                (eval (flycheck-grammalecte--prepare-arg-list
                                       "-b" flycheck-grammalecte-borders-by-mode))
                                ,temp-file-name)))
           (args (mapcan (lambda (arg) (flycheck-substitute-argument arg checker)) cmdline))
           (command (flycheck--wrap-command (car args) (cdr args))))
      (write-region (buffer-string) nil temp-file-name)
      (make-process :name "grammalecte"
                    :buffer output-buffer
                    :command command
                    :sentinel
                    (lambda (process status)
                      (let ((errors (with-current-buffer (process-buffer process)
                                      (flycheck-parse-with-patterns
                                       (buffer-string)
                                       checker
                                       (current-buffer)))))
                        (delete-file temp-file-name)
                        (kill-buffer output-buffer)
                        ;; offset
                        (funcall
                         callback
                         'finished
                         (let ((offset (save-excursion (goto-char (point-min))
                                                       (line-number-at-pos nil t))))
                           (mapcar
                            (lambda (err)
                              (let ((new-err (copy-flycheck-error err)))
                                (setf (cl-struct-slot-value 'flycheck-error 'buffer new-err)
                                      buffer)
                                (setf (cl-struct-slot-value 'flycheck-error 'line new-err)
                                      (+ (flycheck-error-line new-err)
                                         offset -1))
                                (setf (cl-struct-slot-value 'flycheck-error '-end-line new-err)
                                      (+ (flycheck-error-end-line new-err)
                                         offset -1))
                                new-err))
                            errors))))))))

  (defun my-flycheck-grammalecte-setup ()
    "Build the flycheck checker, matching your taste."
    (interactive)
    (flycheck-mode 1)
    (unless (grammalecte--version)
      (advice-add 'grammalecte-download-grammalecte :after-while
                  #'flycheck-grammalecte--retry-setup))
    (grammalecte--augment-pythonpath-if-needed)
    (flycheck-define-generic-checker 'my-grammalecte-narrowed
      "Report Grammalecte errors, but only for the narrowed section."
      :start #'my-flycheck-grammalecte-buffer
      :modes flycheck-grammalecte-enabled-modes
      :predicate (lambda ()
                   (if (functionp flycheck-grammalecte-predicate)
                       (funcall flycheck-grammalecte-predicate)
                     t))
      :enabled #'grammalecte--version
      :verify #'flycheck-grammalecte--verify-setup)
    (setf (flycheck-checker-get 'my-grammalecte-narrowed 'error-patterns)
          (seq-map (lambda (p)
                     (cons (flycheck-rx-to-string `(and ,@(cdr p))
                                                  'no-group)
                           (car p)))
                   flycheck-grammalecte--error-patterns))
    (add-to-list 'flycheck-checkers 'my-grammalecte-narrowed)
    (flycheck-select-checker 'my-grammalecte-narrowed)
    (flycheck-grammalecte--patch-flycheck-mode-map))
;; Emacs and French: Focus flycheck-grammalecte on the narrowed part of the buffer:1 ends here

;; [[file:Sacha.org::#writing-and-editing-learning-french-gtts-cli][gtts-cli:1]]
  (use-package learn-lang-tts :load-path "~/proj/learn-lang"
    :config
    (setq learn-lang-tts-kokoro-cli-executable "~/.local/bin/kokoro-tts --model /home/sacha/vendor/kokoro-onnx/kokoro-v1.0.onnx --voices /home/sacha/vendor/kokoro-onnx/voices-v1.0.bin"))
;; gtts-cli:1 ends here

;; [[file:Sacha.org::#writing-and-editing-learning-french-gtts-cli][gtts-cli:2]]
  (defun my-lang-say-next-new-word ()
    (interactive)
    (when-let ((word (my-lang-find-next-new-word)))
      (learn-lang-tts-say word)))

  (defun my-lang-find-next-new-word ()
    (interactive)
    (let ((pos (point))
          (found nil))
      (while (and (not found)
                  (< pos (point-max)))
        (setq pos (next-overlay-change pos))
        (dolist (ov (overlays-at pos))
          (when (overlay-get ov 'my-lang-highlight)
            (setq found (overlay-get ov 'word))
            (goto-char (overlay-start ov)))))
      (if found
          found
        (message "No more highlights found.")
        nil)))

  (defun my-lang-say-overlay ()
    (interactive)
    (message "hello %s" (word-at-point))
    )

  (defvar-keymap my-lang-overlay-map
    "<down-mouse-1>" #'my-lang-say-overlay
    "<mouse-1>" #'my-lang-say-overlay)

  (defvar-keymap my-lang-shadow-map
    "w" #'my-lang-say
    "n" #'my-lang-say-next-new-word
    "'" #'my-lang-say-sentence-at-point
    "<left>" #'my-lang-say-sentence-at-point
    "<up>" #'my-lang-say-sentence-at-point
    "," #'my-lang-say-forward-sentence
    "<right>" #'my-lang-say-forward-sentence
    "<down>" #'my-lang-say-forward-sentence
    "q" #'my-lang-tts-stop)
  (defun my-lang-shadow ()
    (interactive)
    (set-transient-map my-lang-shadow-map t))
;; gtts-cli:2 ends here

;; [[file:Sacha.org::#writing-and-editing-learning-french-add-shadowing-with-tts-to-subed-record][Add shadowing with tts to subed-record:1]]
  (setq learn-lang-subed-record-reference-dir "~/proj/french/reference/")



  (defun my-lang-say-current-subtitle-and-reset-subed-record ()
    (interactive)
    (my-lang-say-current-subtitle
     (lambda (&rest _)
       (subed-record-retry))))

  (defun my-subed-say-and-review-current-subtitle ()
    (interactive)
    (my-lang-say-current-subtitle
     (lambda (&rest _)
       (my-subed-play-current-subtitle))))

  (defvar-keymap my-subed-review-map
    :repeat (:exit '(subed-record))
    "<right>" (lambda () (interactive)
                (subed-forward-subtitle-text)
                (my-subed-say-and-review-current-subtitle))
    "<left>" #'my-subed-say-and-review-current-subtitle
    "<up>" #'my-lang-say-current-subtitle
    "<down>" #'my-subed-play-current-subtitle
    "n" #'my-subed-review-next-subtitle
    "r" #'subed-record)

  (defun my-subed-review-next-subtitle ()
    "Play the next subtitle."
    (interactive)
    (subed-forward-subtitle-text)
    (my-subed-play-current-subtitle))

  (defun my-subed-review ()
    (interactive)
    (my-subed-say-and-review-current-subtitle)
    (set-transient-map my-subed-review-map t))

  (with-eval-after-load 'subed-record
    (add-hook 'subed-record-finished-hook 'my-subed-record-normalize-current))

  (defun my-subed-record-normalize-current (file)
    (interactive (list (subed-media-file)))
    (let ((temp-file (make-temp-file file nil (concat "." (file-name-extension file)))))
      (make-process
       :name "normalize"
       :buffer (get-buffer-create "*normalize*")
       :command (list
                 (expand-file-name "~/bin/normalize")
                 (expand-file-name file)
                 temp-file)
       :sentinel
       (lambda (process event)
         (when (string-match "finished" event)
           (rename-file temp-file file t)
           (message "Normalized %s" file))))))
;; Add shadowing with tts to subed-record:1 ends here

;; [[file:Sacha.org::#writing-and-editing-learning-french-make-it-easy-to-add-reference-links][Make it easy to add reference links:1]]
  (defvar-local my-subed-record-references nil)
  (defun my-subed-record-load-references (file &optional skip-insert)
    "Load the references from FILE (media)."
    (interactive (list (read-file-name "Media file: ")
                       current-prefix-arg))
    (dolist (cue (subed-parse-file (concat (file-name-sans-extension file) ".vtt")))
      (push
       (list
        (learn-lang-subed-record-simplify (elt cue 3))
        file
        (elt cue 1)
        (elt cue 2))
       my-subed-record-references))
    (unless skip-insert
      (my-subed-insert-references)))

  (defun my-subed-insert-reference ()
    (interactive)
    (when-let* ((rec (alist-get (learn-lang-subed-record-simplify (subed-subtitle-text))
                               my-subed-record-references
                               nil nil #'string=)))
      (subed-record-set-directive
       "#+REFERENCE"
       (format "%s %s --> %s"
               (elt rec 0)
               (subed-msecs-to-timestamp (elt rec 1))
               (subed-msecs-to-timestamp (elt rec 2))))))

  (defun my-subed-insert-references ()
    (interactive)
    (subed-for-each-subtitle (point-min) (point-max) t
      (my-subed-insert-reference)))
;; Make it easy to add reference links:1 ends here

;; [[file:Sacha.org::#writing-and-editing-learning-french-word-timestamps][Word timestamps:1]]
  (defun my-lang-word-timestamps ()
    "Add timestamps using the MFA French model."
    (interactive)
    (let ((subed-align-mfa-dictionary "french_mfa")
          (subed-align-mfa-acoustic-model "french_mfa"))
      (subed-align-mfa-set-word-data
       (subed-media-file)
       nil nil
       (lambda (&rest _)
         (subed-word-data-add-word-timestamps)))))
;; Word timestamps:1 ends here

;; [[file:Sacha.org::#writing-and-editing-learning-french-ai-feedback][AI feedback:1]]
  (defun my-lang-get-audio-feedback (filename &optional extra-text display)
    (interactive (list (if current-prefix-arg
                           (read-file-name "File: " my-practice-dir nil t nil
                                           (lambda (filename) (string-match "\\.m4a$" filename)))
                         (my-latest-file my-practice-dir "\\(\\.m4a\\|\\.webm\\)$"))
                       (if (region-active-p)
                           (buffer-substring (region-beginning) (region-end)))
                       t))
    (let* ((data
            (replace-regexp-in-string
             "\n" ""
             (shell-command-to-string (concat "base64 " (shell-quote-argument filename)))))
           (text
            (concat "Give me feedback in English on this recording of beginner French practice. I am a female A0/A1 speaker. Focus first on major mispronunciations, and provide English phonetic transcriptions for those words using italicized parenthetical notes.

  Return your response using Org Mode syntax using only list items, not headings. Do not put it in a code block, just return Org Mode text. For example, *bold*. Score it out of 10.

  Example output:

  - Pronunciation (7 /10)
    - travaillé /(trah vay yay)/
  "
                    (if extra-text
                        (concat "\n\n###\n\n" extra-text)
                      "")))
           (json-array-type 'vector)
           (json-object-type 'alist)
           (body (json-encode
                  `(("contents"
                     (("parts" .
                       ((("text" . ,text))
                        (("inline_data" .
                          (("mime_type" . "audio/mp4")
                           ("data" . ,data))))
                        )))))))
           result)
      (setq
       result
       (plz 'post "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-flash:generateContent"
         :headers
         `(("Content-Type" . "application/json")
           ("X-goog-api-key" . ,my-gemini-api-key))
         :as #'json-read
         :body body))
      (when display
        (with-current-buffer (get-buffer-create "*Feedback*")
          (org-mode)
          (goto-char (point-min))
          (let* ((text (map-nested-elt result '(candidates 0 content parts 0 text)))
                 (score (when (string-match "Pronunciation (\\(.+\\))" text) (match-string 1 text))))
            (insert "* " (org-link-make-string
                          (concat "audio:"
                                  (replace-regexp-in-string
                                   (rx line-start
                                       (literal (getenv "HOME")))
                                   "~"
                                   filename))
                          "me:") "  "
                          (or score "")
                          "\n"
                          text
                          "\n\n"))
          (display-buffer (current-buffer))
          (with-selected-window (get-buffer-window (current-buffer))
            (goto-char (point-min))
            (recenter-top-bottom 0))))
      (map-nested-elt result '(candidates 0 content parts 0 text))))
;; AI feedback:1 ends here

;; [[file:Sacha.org::my-org-copy-clean-version][my-org-copy-clean-version]]
  (defun my-org-copy-clean-version (text)
    "Copy BEG to END without strike-throughs."
    (interactive (list (if (region-active-p) (buffer-substring (region-beginning) (region-end))
                         (sentence-at-point))))
    (let ((result
           (mapconcat (lambda (o)
                               (if (stringp o)
                                   o
                                 (car (org-element-contents o))))
                             (seq-remove (lambda (o)
                                           (and (listp o)
                                                (member (org-element-type o)
                                                        '(strike-through macro))))
                                         (org-element-parse-secondary-string
                                          text '(paragraph bold strike-through macro)))
                             " ")))
             (setq result
               (replace-regexp-in-string
                "  +" " "
                (replace-regexp-in-string " [\\.,]" "\\1" result)))
      (when (called-interactively-p 'any)
        (message "%s" result)
        (kill-new result))
      result))
;; my-org-copy-clean-version ends here

;; [[file:Sacha.org::#writing-and-editing-learning-french-ai-feedback][AI feedback:3]]
  (defun my-lang-send-subtree-to-web-gemini ()
    (interactive)
    (my-spookfox-send-subtree-to-gemini-textarea)
    (with-current-buffer "*gemini*"
      (goto-char (point-min))
      (re-search-forward "{")
      (goto-char (match-beginning 0))
      (setq my-lang-feedback (json-read)))
    (my-lang-find-next-correction))
;; AI feedback:3 ends here

;; [[file:Sacha.org::#writing-and-editing-learning-french-using-flycheck-to-display-ai-grammar-feedback-from-gptel-as-i-learn-french][Using flycheck to display AI grammar feedback from gptel as I learn French:1]]
  (defvar my-gptel-use-paid t)
  (defvar my-lang-gptel-level nil)
  (defun my-lang-gptel-cycle (&optional reset)
    (interactive (list current-prefix-arg))
    (let ((levels '(grammalecte groq gemini-free gemini-paid)))
      (setq my-lang-gptel-level
            (cond
             ((and (numberp reset)
                   (< reset 0))
              (elt
               levels
               (%
                (+ (seq-position levels my-lang-gptel-level)
                   (1- (length levels)))
                (length levels))))
             ((or reset
                  (null my-lang-gptel-level)
                  (null
                   (seq-position levels my-lang-gptel-level)))
              (car levels))
             (t
              (elt
               levels
               (%
                (1+ (seq-position levels my-lang-gptel-level))
                (length levels)))))))
    (message "Level: %s" (symbol-name my-lang-gptel-level))
    (pcase my-lang-gptel-level
      ('grammalecte
       (flycheck-select-checker 'my-grammalecte-narrowed)
       (setq my-lang-gptel-current-errors nil
             my-lang-gptel-previous-version nil))
      ('groq
       (flycheck-select-checker 'lang-gptel-cached)
       (setq gptel-backend my-gptel-groq)
       (setq gptel-model 'openai/gpt-oss-120b)
       (my-lang-gptel-flycheck-setup))
      ('gemini-free
       (setq gptel-backend my-gptel-gemini)
       (setq gptel-model 'gemini-3-flash-preview)
       (my-lang-gptel-flycheck-setup))
      ('gemini-paid
       (setq gptel-backend my-gptel-gemini-paid)
       (setq gptel-model 'gemini-3-flash-preview)
       (my-lang-gptel-flycheck-setup))))
;; Using flycheck to display AI grammar feedback from gptel as I learn French:1 ends here

;; [[file:Sacha.org::#writing-and-editing-learning-french-process-audio-files][Process audio files:1]]
  (defun my-audio-clip (source start-time end-time destination text)
    (interactive
     (let ((s (and (region-active-p) (buffer-substring (region-beginning) (region-end)))))
       (if (and s
                (string-match "\\(\\(?:\\(?:[0-9]+\\):\\)?\\(?:[0-9]+\\):\\(?:[0-9]+\\)\\(?:\\.\\(?:[0-9]+\\)\\)?\\)[ \n\t]+\\(\\(?:\\(?:[0-9]+\\):\\)?\\(?:[0-9]+\\):\\(?:[0-9]+\\)\\(?:\\.\\(?:[0-9]+\\)\\)?\\)" s))
           (let ((start (match-string 1 s))
                 (end (match-string 2 s)))
             (list
              (read-file-name "Source: " nil nil t)
              start
              end
              (read-file-name "Destination: ")
              (read-string "Text: ")))
         (list (read-file-name "Source: " nil nil t)
               (read-string "Start time: ")
               (read-string "End time: ")
               (read-file-name "Destination: ")
               (read-string "Text: ")))))
    (let ((result (call-process "ffmpeg" nil (get-buffer-create "*ffmpeg*") nil
                                "-y" ; Overwrite output file without asking
                                "-i" (expand-file-name source) ; Input file
                                "-ss" start-time ; Start time (e.g., 00:00:10)
                                "-to" end-time ; End time/Stop time
                                (expand-file-name destination))))
      (when result
        (when (region-active-p) (delete-region (region-beginning) (region-end)))
        (insert (org-link-make-string
                 (concat "audio:" (replace-regexp-in-string (getenv "HOME") "~" destination))
                 text)))))
;; Process audio files:1 ends here

;; [[file:Sacha.org::#writing-and-editing-learning-french-save-journal-entries-for-analysis][Save journal entries for analysis:1]]
  (defun my-lang-write-journal-entries-for-subtree ()
    (interactive)
    (org-map-entries
     (lambda ()
       (when (org-entry-get (point) "DATE")
         (let ((text (replace-regexp-in-string "{.+?}" "" (my-org-subtree-text-without-blocks))))
           (with-temp-file (expand-file-name (concat (org-entry-get (point) "DATE") ".txt")
                                             "~/proj/french/journal")
             (insert text)))))
     nil 'tree))
;; Save journal entries for analysis:1 ends here

;; [[file:Sacha.org::#writing-and-editing-learning-french-load-en-fr-dictionary][Load en-fr dictionary:1]]
  (defvar my-lang-en-fr-dictionary-file "~/proj/french/dic-en-fr.iso")
  (defvar my-lang-dictionary nil)
  (defun my-lang-load-dict ()
    (interactive)
    (with-temp-buffer
      (insert-file-contents my-lang-en-fr-dictionary-file)
      (goto-char (point-min))
      (while (looking-at "^# ") (forward-line 1))
      (setq my-lang-dictionary
            (seq-keep (lambda (s)
                        (when (string-match "^\\(.+?\\) {\\(.+?\\)}\\(?: /\\(.+?\\)/\\)?\\(?: (\\(.+?\\))\\)?\\( SEE: .+?\\)? ::\\(?: \\(.+?\\) *\\({.+}.*\\)?\\)?$" s)
                          (let ((headword (match-string 1 s))
                                (head-type (match-string 2 s))
                                (def (match-string 4 s))
                                (see (match-string 5 s))
                                (translation (match-string 6 s))
                                (types (match-string 7 s)))
                            (cons
                             (propertize (format "%s {%s} - %s - %s :: %s"
                                                 headword
                                                 head-type
                                                 (or see translation)
                                                 (or def "")
                                                 (or types ""))
                                         'face
                                         (cond
                                          ((null types) nil)
                                          ((string-match "^{m}" types) 'modus-themes-subtle-blue)
                                          ((string-match "^{f}" types) 'modus-themes-subtle-magenta)))
                             translation))))
                      (split-string (buffer-substring (point) (point-max)) "\n")))
      (seq-take my-lang-dictionary 10)))

  (defun my-lang-consult-en-fr ()
    (interactive)
    (unless my-lang-dictionary (my-lang-load-dict))
    (insert
     (consult--read
      (consult--dynamic-collection
          (lambda (input)
            (let (match-start
                  match-any
                  exact
                  (search (regexp-quote input)))
              (seq-map (lambda (o)
                         (cond
                          ((string-match (concat "^" search " - ") (car o))
                           (push o exact))
                          ((string-match (concat "^" search) (car o))
                           (push o match-start))
                          ((string-match search (car o))
                           (push o match-any))))
                       my-lang-dictionary)
              (append
               (nreverse exact)
               (nreverse match-start)
               (nreverse match-any)
               nil))))
      :sort nil
      :lookup #'consult--lookup-cdr)))
;; Load en-fr dictionary:1 ends here

;; [[file:Sacha.org::#writing-and-editing-learning-french-conjugation][Conjugation:1]]
    (defun my-lang-conjugate-grammalecte (input)
      "Query Grammalecte for INPUT."
      (interactive (list (my-lang-lexique-complete-word)))
      (grammalecte-conjugate-verb input))

    (defvar my-lang-verbe-db "~/vendor/verbe-conjugaison-academie-francaise/output/verbs.db")

    (defun my-lang-conjugate (input &optional all-forms)
      (interactive (list (my-lang-lexique-complete-word)
                         (null current-prefix-arg)))
      (let* ((db (sqlite-open my-lang-verbe-db))
             (lemme (elt (car (my-lang-lexique-lookup-db-exact input)) 1))
             (value
              (consult--read
               (if all-forms
                   (mapcar (lambda (row)
                             (cons (string-join
                                    (list
                                     (elt row 0)
                                     (elt row 1)
                                     (elt row 2)
                                     (elt row 4)
                                     (elt row 3))
                                    " - ")
                                   (elt row 0)))
                           (sqlite-select db
                                          "SELECT
        conjugaison, voix, mode, temps, personne
    FROM verbes v
    JOIN conjugaisons c
    ON v.id = c.verbe_id WHERE v.infinitif = ?
    ORDER BY temps, mode, personne" (list lemme)))
                 (mapcar (lambda (row)
                             (cons (string-join
                                    (list
                                     (elt row 0)
                                     (elt row 4)
                                     (elt row 3))
                                    " - ")
                                   (elt row 0)))

                           (sqlite-select db
                                          "SELECT
        conjugaison, voix, mode, temps, personne
    FROM verbes v
    JOIN conjugaisons c
    ON v.id = c.verbe_id WHERE v.infinitif = ?
    AND temps in (?, ?)
    AND mode=?
    ORDER BY temps, mode, personne"
                                          (list input "present" "passe_compose"  "indicatif"))))
               :prompt "Verbe: "
               :lookup 'consult--lookup-cdr)))
        (sqlite-close db)
        (when (called-interactively-p 'any)
          (when (word-at-point)
              (delete-region (save-excursion
                               (skip-syntax-backward "w")
                               (point))
                             (save-excursion
                               (skip-syntax-forward "w")
                               (point))))
          (insert value))
        value))
;; Conjugation:1 ends here

;; [[file:Sacha.org::#writing-and-editing-learning-french-looking-up-words-via-wordreference][Looking up words via wordreference:1]]
  (defvar my-lang-wordreference-cache nil)

  (defun my-lang-wordreference-completing-read ()
    (interactive)
    (consult--read
     (consult--dynamic-collection
         (lambda (input)
           (message "input: %s" input)
           (with-current-buffer
               (url-retrieve-synchronously
                (concat "https://www.wordreference.com/autocomplete?dict=enfr&query="
                        (url-hexify-string input)))
                               (set-buffer-multibyte t)
             (goto-char (point-min))
             (re-search-forward "^$" nil t)
             (prog1
                 (mapcar (lambda (row)
                           (let ((fields (split-string row "\t")))
                             (propertize
                              (format "%s (%s)"
                                      (car fields)
                                      (cadr fields))
                              'consult--candidate
                              fields)))
                         (split-string
                          (string-trim (buffer-substring (point) (point-max))) "\n"))
               (kill-buffer (current-buffer))))))
     :sort nil
     :initial (symbol-name (symbol-at-point))
     :history 'my-lang-wordreference-lookup-history
     :prompt "Word: "
     :category 'word))

  (defvar-keymap my-lang-wordreference-keymap
    "v" #'my-spookfox-scroll-down
    "V" #'my-spookfox-scroll-up
    "c" #'my-lang-conjugate-last-word
    "l" #'my-lang-wordreference-lookup)

  (defvar my-lang-wordreference-lookup-history nil)
  (defun my-lang-wordreference-lookup (word)
    (interactive (list (my-lang-wordreference-completing-read)))
    (let (language)
      (if (string-match " (\\(en\\|fr\\))" word)
          (setq language (match-string 1 word)
                word (replace-match "" nil t word 0)))
      (setq word (replace-regexp-in-string "^#" "" word))
      (browse-url
       (format "https://www.wordreference.com/%s/%s"
               (if (string= language "fr")
                   "fren"
                 "enfr")
               (url-hexify-string word))))
    (set-transient-map my-lang-wordreference-keymap t))

  (defun my-lang-conjugate-last-word ()
    (interactive)
    (grammalecte-conjugate-verb
     (replace-regexp-in-string " (.+)" ""
                               (car my-lang-wordreference-lookup-history))))

  (defun my-lang-wordreference-conjugate (word)
    (interactive (list (my-lang-wordreference-completing-read)))
    (browse-url (concat "https://www.wordreference.com/conj/frverbs.aspx?v="
                        (url-hexify-string (if (listp word) (car word) word)))))

  (defun my-lang-reverso (s)
    (interactive (list (if (region-active-p)
                           (buffer-substring (region-beginning) (region-end))
                         (word-at-point))))
    (browse-url (concat "https://www.reverso.net/text-translation#sl=fra&tl=eng&text="
                        (url-hexify-string s))))
;; Looking up words via wordreference:1 ends here

;; [[file:Sacha.org::#writing-and-editing-learning-french-lexique][Lexique:1]]
  (defvar my-lang-lexique-db "~/proj/french/lexique.db" "SQLite3 DB")
;; Lexique:1 ends here

;; [[file:Sacha.org::#writing-and-editing-learning-french-lexique][Lexique:4]]
  (defvar my-lang-csv-path "/home/sacha/proj/french/french_vocabulary_list.csv")
  (defvar my-lang-known-lemmas nil)
  (defun my-lang-lexique-lookup-db-exact (input)
    "Query the Lexique SQLite database for INPUT."
    (let ((db (sqlite-open my-lang-lexique-db)))
      (prog1 (sqlite-select db
                            "SELECT ortho, lemme, genre, nombre, phon, syll, infover FROM lexique
                      WHERE ortho=? ORDER BY freqfilms2 DESC LIMIT 1"
                            (list input))
        (sqlite-close db))))

  (defun my-lang-lexique-lookup-db-flat (input)
    "Query the Lexique SQLite database for INPUT."
    (let ((db (sqlite-open my-lang-lexique-db)))
      (prog1 (sqlite-select db
                            "SELECT ortho, lemme, genre, nombre, phon, syll, infover FROM lexique
                      WHERE ortho_flat LIKE ? OR ortho LIKE ? ORDER BY freqfilms2 DESC LIMIT 50"
                            (list (concat (downcase input) "%")
                                  (concat (downcase input) "%")))
        (sqlite-close db))))

  (defun my-lang-lexique-lookup-db-lemma (input)
    "Query the Lexique SQLite database for INPUT."
    (let ((db (sqlite-open my-lang-lexique-db)))
      (prog1 (sqlite-select db
                            "SELECT ortho, lemme, genre, nombre, infover, phon, syll, ortho_flat FROM lexique
                      WHERE lemme LIKE ? ORDER BY freqfilms2 DESC LIMIT 50"
                            (list (concat input "%")))
        (sqlite-close db))))

  (defun my-lang-lexique-complete-word ()
    (interactive)
    (let* ((selection
            (consult--read
             (consult--dynamic-collection
                 (lambda (input)
                   (mapcar (lambda (row)
                             (let ((word (nth 0 row))
                                   (gender (nth 2 row))
                                   (number (nth 3 row))
                                   (ipa (my-lang-lexique-to-ipa (nth 5 row)))
                                   (infover (nth 6 row)))
                               ;; Format the string for the completion buffer
                               (cons
                                (propertize
                                 (format "%-20s [%s] (%s)" word ipa
                                         (string-join
                                          (delq nil (list gender number infover))
                                          ", "))
                                 'consult--candidate word
                                 'face
                                 (pcase gender
                                   ("m" 'modus-themes-subtle-blue)
                                   ("f" 'modus-themes-subtle-magenta)))
                                word)))
                           (my-lang-lexique-lookup-db-flat input))))
             :prompt "French word: "
             :initial (word-at-point)
             :sort nil
             :lookup #'consult--lookup-cdr
             :category 'french-word)))
      (when selection
        (when (called-interactively-p 'any)
          (when (word-at-point)
            (delete-region (save-excursion
                             (skip-syntax-backward "w")
                             (point))
                           (save-excursion
                             (skip-syntax-forward "w")
                             (point))))
          (insert selection))
        selection)))
;; Lexique:4 ends here

;; [[file:Sacha.org::#writing-and-editing-learning-french-write-a-completion-at-point-function-for-french][Write a completion-at-point function for French:1]]
  (defun my-lang-lexique-completion-at-point ()
    (let ((bounds (bounds-of-thing-at-point 'word)))
      (when bounds
        (list (car bounds)
              (cdr bounds)
              (mapcar (lambda (row)
                        (let ((word (nth 0 row))
                              (gender (nth 2 row))
                              (number (nth 3 row))
                              (ipa (my-lang-lexique-to-ipa (nth 5 row)))
                              (infover (nth 6 row)))
                          word))
                      (my-lang-lexique-lookup-db-flat
                       (buffer-substring-no-properties
                        (car bounds)
                        (cdr bounds))))
              :exclusive 'no))))
  (with-eval-after-load 'org
    (add-hook 'org-mode-hook
              (lambda ()
                (when (and (buffer-file-name) (string-match "journal-fr\\|french" (buffer-file-name)))
                  (add-hook 'completion-at-point-functions 'my-lang-lexique-completion-at-point)))))
;; Write a completion-at-point function for French:1 ends here

;; [[file:Sacha.org::#writing-and-editing-learning-french-highlight-and-count-new-words-in-journal-entries][Highlight and count new words in journal entries:1]]
  (defun my-lang-load-known-lemmas ()
    "Parse the CSV and return a hash table of lemma -> info."
    (let* ((known (make-hash-table :test 'equal))
           (data (cdr (pcsv-parse-file my-lang-csv-path))))
      (mapc
       (lambda (o)
         (unless (gethash (elt o 2) known)
           (puthash (elt o 2) o known))
         (unless (gethash (elt o 0) known)
           (puthash (elt o 0) o known)))
       data)
      (setq my-lang-known-lemmas known)))

  (defvar my-lang-ignore
    (with-temp-buffer
      (insert-file-contents "~/proj/french/ignored.txt")
      (split-string (string-trim (buffer-string)) "\n")))

  (defun my-lang-lexique-to-ipa (s)
    (mapconcat (lambda (c)
                 (pcase c
                   (?O "ɔ")
                   (?E "ɛ")
                   (?° "ə")
                   (?2 "ø")
                   (?9 "œ")
                   (?S "ʃ")
                   (?5 "ɛ̃")
                   (?Z "ʒ")
                   (?@ "ɑ̃")
                   (?1 "œ̃")
                   (?§ "ɔ̃")
                   (?8 "ɥ")
                   (?R "ʁ")
                   (_ (char-to-string c))))
                 s ""))

  (defvar my-lang-show-pronunciation t "*Non-nil means show pronunciation.")

  (defun my-lang-highlight-new-words-in-subtree ()
    "Highlight words in the current subtree based on lexique.db and CSV data."
    (interactive)
    (save-excursion
      (my-lang-remove-new-word-highlights)
      (if (org-entry-get-with-inheritance "DATE")
          (let* ((subtree-date (org-entry-get-with-inheritance "DATE"))
                 (known-lemmas (or my-lang-known-lemmas (my-lang-load-known-lemmas)))
                 (seen-so-far (make-hash-table :test 'equal))
                 (beg (save-excursion (org-back-to-heading t) (org-end-of-meta-data t) (point)))
                 (end (save-excursion (org-end-of-subtree t) (point)))
                 (count-new 0)
                 (count-words 0))
            (save-excursion
              (goto-char beg)
              (while (and (< (point) end) (re-search-forward "\\b[[:alpha:]-]+\\b" end t))
                (let* ((word (match-string 0))
                       (info (and (or
                                   (not (car (gethash (downcase word) known-lemmas)))
                                   (not (string>
                                         subtree-date
                                         (car (gethash (downcase word) known-lemmas)))))
                                   (car (my-lang-lexique-lookup-db-exact (downcase word))))))
                  (setq count-words (1+ count-words))
                  (when info
                    (let* ((lemma (elt info 1))
                           (gender (elt info 2))
                           (syll (propertize (concat " (" (elt info 5) ")")
                                             'face
                                             'modus-themes-fg-cyan-faint
                                             'keymap
                                             my-lang-overlay-map
                                             ))
                           (csv-date (car (gethash lemma known-lemmas)))
                           (is-new (and (or (null csv-date)
                                            (and subtree-date (not (string> subtree-date
                                                                            csv-date))))
                                        (not (gethash lemma seen-so-far))
                                        (not (member lemma my-lang-ignore)))))
                      (when is-new
                        (puthash lemma (list subtree-date word lemma) seen-so-far)
                        (puthash lemma (list subtree-date word lemma) my-lang-known-lemmas)
                        (puthash word (list subtree-date word lemma) my-lang-known-lemmas)
                        (setq count-new (1+ count-new))
                        (let ((ov (make-overlay (match-beginning 0) (match-end 0)))
                              (face (cond
                                     ((string-equal gender "m") 'modus-themes-subtle-blue)
                                     ((string-equal gender "f") 'modus-themes-subtle-magenta)
                                     (t 'modus-themes-subtle-green))))
                          (overlay-put ov 'my-lang-highlight t)
                          (overlay-put ov 'word word)
                          (overlay-put ov 'evaporate t)
                          (when my-lang-show-pronunciation
                            (overlay-put ov 'after-string (my-lang-lexique-to-ipa syll)))
                          (overlay-put ov 'face face)))))))
              (org-back-to-heading)
              (let ((ov (make-overlay (line-beginning-position) (line-end-position))))
                (overlay-put ov 'after-string (format " + %d = %d" count-new count-words))
                (overlay-put ov 'my-lang-highlight t))
              (when (called-interactively-p 'any)
                (message "%d total words, %d new lemmas" count-words count-new))
              (cons count-new count-words)))
        (let ((data (org-map-entries #'my-lang-highlight-new-words-in-subtree "DATE={.}" 'tree)))
          (org-back-to-heading)
          (let ((ov (make-overlay (line-beginning-position) (line-end-position))))
            (overlay-put ov 'evaporate t)
            (overlay-put ov 'after-string
                         (format " + %d = %d"
                                 (apply '+ (mapcar 'car data))
                                 (apply '+ (mapcar 'cdr data))))
            (overlay-put ov 'my-lang-highlight t))))))

    (defun my-lang-remove-new-word-highlights ()
      "Remove all word highlights created by `my-lang-highlight-new-words-in-subtree'."
      (interactive)
      (let ((beg (save-excursion (org-back-to-heading t) (point)))
            (end (save-excursion (org-end-of-subtree t) (point))))
        (remove-overlays beg end 'my-lang-highlight t)))
;; Highlight and count new words in journal entries:1 ends here

;; [[file:Sacha.org::#writing-and-editing-learning-french-doublecheck-with-google-translate][Doublecheck with Google Translate:1]]
  (defun my-lang-translate-dwim (text)
    "Translate TEXT via Google Translate."
    (interactive (list
                  (cond
                   ((region-active-p)
                    (buffer-substring (region-beginning)
                                      (region-end)))
                   (current-prefix-arg
                    (thing-at-point 'paragraph))
                   (t (thing-at-point 'sentence)))))
    (let* ((url "https://translation.googleapis.com/language/translate/v2")
           (params `(("key" . ,(getenv "GOOGLE_API_KEY"))
                     ("q" . ,text)
                     ("source" . "fr")
                     ("target" . "en")
                     ("format" . "text")))
           (query-string (mapconcat
                          (lambda (pair)
                            (format "%s=%s"
                                    (url-hexify-string (car pair))
                                    (url-hexify-string (cdr pair))))
                          params
                          "&"))
           (full-url (concat url "?" query-string)))
      (let* ((response (plz 'get full-url :as #'json-read))
             (data (alist-get 'data response))
             (translations (alist-get 'translations data))
             (first-translation (car translations))
             (translated-text (alist-get 'translatedText first-translation)))
        (when (called-interactively-p 'any)
          (message "%s" translated-text))
        translated-text)))
;; Doublecheck with Google Translate:1 ends here

;; [[file:Sacha.org::#writing-and-editing-learning-french-correct-encoding-errors][Correct encoding errors:1]]
  (defun my-repair-french-encoding-full ()
    "Repair double and single encoded UTF-8 sequences, including uppercase."
    (interactive)
    (save-excursion
      (let ((case-fold-search t)
      (pairs '(("ÃƒÂ©" . "é") ("ÃƒÂ‰" . "É")
               ("ÃƒÂ " . "à") ("ÃƒÂ€" . "À")
               ("Ã¨" . "è")
               ("Ãª" . "ê")
               ("Ã" . "Ç")
               ("Ã§" . "ç")
               ("Ã»" . "û")
               ("ÃƒÂ¨" . "è") ("ÃƒÂˆ" . "È")
               ("ÃƒÂ§" . "ç") ("ÃƒÂ‡" . "Ç")
               ("Ã©" . "é")    ("Ã‰" . "É")
               ("Ã " . "à")    ("Ã€" . "À")
               ("â" . "'"))))
        (dolist (pair pairs)
          (goto-char (point-min))
          ;; 'nil' for literal search, 't' for case-sensitivity
          (while (search-forward (car pair) nil t)
            (replace-match (cdr pair) t))))))
;; Correct encoding errors:1 ends here

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

;; [[file:Sacha.org::#gif-screencast][gif-screencast:3]]
  (use-package gif-screencast
          :bind
          ("s-s" . my-gif-screencast-start-or-stop-and-choose-thumbnail)
          :config
          (setq gif-screencast-output-directory my-recordings-dir))

  (use-package giffy
          :quelpa (giffy :fetcher github :repo "larsmagne/giffy")
          :defer t)
;; gif-screencast:3 ends here

;; [[file:Sacha.org::#sentences-end-with-a-single-space][Sentences end with a single space:1]]
  (setq sentence-end-double-space nil)
;; Sentences end with a single space:1 ends here

;; [[file:Sacha.org::#writeroom][Writeroom:1]]
  (use-package writeroom-mode
          :defer t
          :commands writeroom-mode
          :config
          (setq writeroom-global-effects (remove 'writeroom-set-fullscreen
                                                                                                                                                                   writeroom-global-effects)))
;; Writeroom:1 ends here

;; [[file:Sacha.org::#try-redacting][Try redacting:1]]
  (defun my-redact (s)
          "Replace S with x characters."
          (make-string (length s) ?x))

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
          (if (stringp before-part) (setq before-part (list before-part)))
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
  line. If begin or stop has been misrecognized, try the best guess."
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
                                  ;; no stop keyword; is the keyword around? maybe it was just misrecognized
                                  (if (re-search-forward (elt result 3)
                                                                                                                           (save-excursion
                                                                                                                                   (forward-word my-audio-braindump-close-keyword-distance-words)
                                                                                                                                   (point))
                                                                                                                           t)
                                                  (save-excursion
                                                          (goto-char (match-beginning 0))
                                                          (save-excursion
                                                                  (insert " STOP "))
                                                          (while (re-search-backward " *\n+ *" (car result) t)
                                                                  (replace-match " ")))
                                          (when (looking-at "\n+ *")
                                                  (replace-match " ")))))
                  ;; Check for stops without starts
                  (goto-char (point-min))
                  (while (setq result (my-audio-braindump-scan-for-part-keyword 'stop nil nil t))
                          (goto-char (car result))
                          (save-excursion
                                  (unless (re-search-backward (elt result 3) (line-beginning-position) t)
                                          (when (re-search-backward
                                                                   (elt result 3)
                                                                   (save-excursion (backward-word my-audio-braindump-close-keyword-distance-words)
                                                                                                                                   (point))
                                                                   t)
                                                  (replace-match (concat "\nSTART " (elt result 3))))))
                          (goto-char (cadr result)))
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

;; [[file:Sacha.org::#writing-and-editing-updating-my-audio-braindump-workflow-to-take-advantage-of-whisperx][Updating my audio braindump workflow to take advantage of WhisperX:1]]
  (defun my-whisperx-word-list (file)
          (let* ((json-object-type 'alist)
                                   (jmson-array-type 'list))
                  (seq-mapcat (lambda (seg)
                                                                          (alist-get 'words seg))
                                                                  (alist-get 'segments (json-read-file file)))))

  ;; (seq-take (my-whisperx-word-list (my-latest-file "~/sync/recordings" "\\.json")) 10)
  (defun my-whisperx-insert-word-list (words)
          "Inserts WORDS with text properties."
          (require 'subed-word-data)
          (mapc (lambda (word)
                                                  (let ((start (point)))
                                                          (insert
                                                           (alist-get 'word word))
                                                          (subed-word-data--add-word-properties start (point) word)
                                                          (insert " ")))
                                  words))

  (defun my-audio-braindump-turn-sections-into-headings ()
          (interactive)
          (goto-char (point-min))
          (while (re-search-forward "START SECTION \\(.+?\\) STOP SECTION" nil t)
                  (replace-match
                   (save-match-data
                           (format
                                  "\n*** %s\n"
                                  (save-match-data (string-trim (replace-regexp-in-string "^[,\\.]\\|[,\\.]$" "" (match-string 1))))))
                   nil t)
                  (let ((prop-match (save-excursion (text-property-search-forward 'subed-word-data-start))))
                          (when prop-match
                                  (org-entry-put (point) "START" (format-seconds "%02h:%02m:%02s" (prop-match-value prop-match)))))))

  (defun my-audio-braindump-split-sentences ()
          (interactive)
          (goto-char (point-min))
          (while (re-search-forward "[a-z]\\. " nil t)
                  (replace-match (concat (string-trim (match-string 0)) "\n") )))

  (defun my-audio-braindump-restructure ()
          (interactive)
          (goto-char (point-min))
          (my-subed-fix-common-errors)
          (org-mode)
          (my-audio-braindump-prepare-alignment-breaks)
          (my-audio-braindump-turn-sections-into-headings)
          (my-audio-braindump-split-sentences)
          (goto-char (point-min))
          (my-remove-filler-words-at-start))

  (defun my-audio-braindump-from-whisperx-json (file)
          (interactive (list (read-file-name "JSON: " "~/sync/recordings/" nil nil nil (lambda (f) (string-match "\\.json\\'" f)))))
          ;; put them all into a buffer
          (with-current-buffer (get-buffer-create "*Words*")
                  (erase-buffer)
                  (fundamental-mode)
                  (my-whisperx-insert-word-list (my-whisperx-word-list file))
                  (my-audio-braindump-restructure)
                  (goto-char (point-min))
                  (switch-to-buffer (current-buffer))))

  (defun my-audio-braindump-process-text (file)
          (interactive (list (read-file-name "Text: " "~/sync/recordings/" nil nil nil (lambda (f) (string-match "\\.txt\\'" f)))))
          (with-current-buffer (find-file-noselect file)
                  (my-audio-braindump-restructure)
                  (save-buffer)))
  ;; (my-audio-braindump-from-whisperx-json (my-latest-file "~/sync/recordings" "\\.json"))
;; Updating my audio braindump workflow to take advantage of WhisperX:1 ends here

;; [[file:Sacha.org::#markdown][Markdown:1]]
  (use-package markdown-mode
    :if my-laptop-p
    :mode ("\\.\\(njk\\|md\\)\\'" . markdown-mode))
;; Markdown:1 ends here

;; [[file:Sacha.org::#screenshot][Screenshot:1]]
  (defun my-screenshot-svg (&optional filename)
    "Save a screenshot of the current frame as an SVG image.
  Saves to a temp file and puts the filename in the kill ring."
    (interactive)
    (let* ((filename
            (or filename
                                              (expand-file-name
                                               (format-time-string "%Y-%m-%d-%H-%M-%S.svg")
                                               my-recordings-dir)))
           (data (x-export-frames nil 'svg)))
      (with-temp-file filename
        (insert data))
      (when (called-interactively-p 'any)
        (kill-new filename)
        (message filename))
      filename))

  (defun my-screenshot-current-screen (&optional filename)
    (interactive)
    (let ((new-file
           (or filename
               (expand-file-name
                (format-time-string "%Y-%m-%d-%H-%M-%S.png")
                my-recordings-dir))))
      (make-process
       :name "spectacle"
       :command
       (list "spectacle" "-b" "-m" "-n" "-o" new-file))
      new-file))

  (keymap-global-set "C-c s" #'screenshot-svg)
;; Screenshot:1 ends here

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

;; [[file:Sacha.org::#unfill-paragraph][Emacs: Cycle through different paragraph formats: all on one line, wrapped, max one sentence per line, one sentence per line:1]]
  (defvar my-repeat-counter '()
    "How often `my-repeat-next' was called in a row using the same command.
  This is an alist of (cat count list) so we can use it for different functions.")

  (defun my-unfill-paragraph ()
    "Replace newline chars in current paragraph by single spaces.
  This command does the inverse of `fill-paragraph'."
    (interactive)
    (let ((fill-column most-positive-fixnum))
      (fill-paragraph)))

  (defun my-fill-paragraph-semlf-long ()
          (interactive)
          (let ((fill-column most-positive-fixnum))
                  (fill-paragraph-semlf)))

  (defun my-repeat-next (category &optional element-list reset)
          "Return the next element for CATEGORY.
  Initialize with ELEMENT-LIST if this is the first time."
          (let* ((counter
                                          (or (assoc category my-repeat-counter)
                                                          (progn
                                                                  (push (list category -1 element-list)
                                                                                          my-repeat-counter)
                                                                  (assoc category my-repeat-counter)))))
                  (setf (elt (cdr counter) 0)
                                          (mod
                                           (if reset 0 (1+ (elt (cdr counter) 0)))
                                           (length (elt (cdr counter) 1))))
                  (elt (elt (cdr counter) 1) (elt (cdr counter) 0))))

  (defun my-in-prefixed-comment-p ()
    (or (member 'font-lock-comment-delimiter-face (face-at-point nil t))
                          (member 'font-lock-comment-face (face-at-point nil t))
                          (save-excursion
                                  (beginning-of-line)
                                  (comment-search-forward (line-end-position) t))))

  ;; It might be nice to figure out what state we're
  ;; in and then cycle to the next one if we're just
  ;; working with a single paragraph. In the
  ;; meantime, just going by repeats is fine.
  (defun my-reformat-paragraph-or-region ()
    "Cycles the paragraph between three states: filled/unfilled/fill-sentences.
  If a region is selected, handle all paragraphs within that region."
    (interactive)
          (let ((func (my-repeat-next 'my-reformat-paragraph
                                                                                                                          '(my-fill-paragraph-semlf-long
                                  fill-paragraph-semlf
                                  fill-paragraph
                                  my-unfill-paragraph)
                                                                                                                          (not (eq this-command last-command))))
                                  (deactivate-mark nil))
                  (if (region-active-p)
                                  (save-restriction
                                          (save-excursion
                                                  (narrow-to-region (region-beginning) (region-end))
                                                  (goto-char (point-min))
                                                  (while (not (eobp))
                                                          (skip-syntax-forward " ")
                                                          (let ((elem (and (derived-mode-p 'org-mode)
                                                                                                                           (org-element-context))))
                                                                  (cond
                                                                   ((eq (org-element-type elem) 'headline)
                                                                          (org-forward-paragraph))
                                                                   ((member (org-element-type elem)
                                                                                                          '(src-block export-block headline property-drawer))
                                                                          (goto-char
                                                                           (org-element-end (org-element-context))))
                                                                   (t
                                                                          (funcall func)
                                                                          (if fill-forward-paragraph-function
                                                                                          (funcall fill-forward-paragraph-function)
                                                                                  (forward-paragraph))))))))
                          (save-excursion
                                  (move-to-left-margin)
                                  (funcall func)))))

  (keymap-global-set "M-q" #'my-reformat-paragraph-or-region)
;; Emacs: Cycle through different paragraph formats: all on one line, wrapped, max one sentence per line, one sentence per line:1 ends here

;; [[file:Sacha.org::#writing-and-editing-visual-line][Visual line:1]]
  (global-visual-line-mode)
  (add-hook 'minibuffer-mode-hook (lambda () (visual-line-mode -1)))
;; Visual line:1 ends here

;; [[file:Sacha.org::#unicode][Unicode:1]]
  (defmacro my-insert-unicode (unicode-name)
    `(lambda () (interactive)
       (insert-char (cdr (assoc-string ,unicode-name (ucs-names))))))
  (bind-key "C-x 8 s" (my-insert-unicode "ZERO WIDTH SPACE"))
  (bind-key "C-x 8 S" (my-insert-unicode "SNOWMAN"))
;; Unicode:1 ends here

;; [[file:Sacha.org::#clean-up-spaces][Clean up spaces:1]]
  (bind-key "M-SPC" 'cycle-spacing)
;; Clean up spaces:1 ends here

;; [[file:Sacha.org::#expand][Expand:1]]
  (setq save-abbrevs 'silently)
  (bind-key "M-/" 'hippie-expand)
;; Expand:1 ends here

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

;; [[file:Sacha.org::#speech-recognition][Speech recognition:1]]
  (use-package caser
    :bind
    ("M-D" . caser-dashcase-dwim))
;; Speech recognition:1 ends here

;; [[file:Sacha.org::#writing-and-editing-speech-recognition-map-lang-gptel-feedback-from-the-logbook-to-kwiziq-topics][Map lang-gptel feedback from the logbook to KwizIQ topics:1]]
(defun my-org-collect-logbook-contents ()
  "Collect contents of all LOGBOOK drawers in the current subtree.
Returns them concatenated as a string."
  (save-excursion
    (org-back-to-heading t)
    (let ((subtree-end (save-excursion (org-end-of-subtree t t)))
          contents
          elem)
      (while (re-search-forward "^[ \t]*:LOGBOOK:[ \t]*$" subtree-end t)
        (setq elem (org-element-at-point))
        (push (buffer-substring-no-properties
               (org-element-contents-begin elem)
               (org-element-contents-end elem))
              contents))
      (string-join (nreverse contents) "\n"))))

(defun my-org-get-subtree (link)
  (save-window-excursion
		(save-excursion
			(org-link-open-from-string link)
	    (buffer-substring-no-properties (point) (progn (org-end-of-subtree) (point))))))

(defun my-lang-gptel-analyze-feedback ()
  (interactive)
  (with-current-buffer (get-buffer-create "*Feedback*")
    (erase-buffer)
    (org-mode))
  (gptel-request
      (json-encode
       `(("feedback on previous mistakes" . ,(my-org-collect-logbook-contents))
         ("topic links" . ,(my-org-get-subtree "[[file:~/sync/orgzly/organizer.org::#kwiziq-a2]]"))
         ("prompt" . "Analyze the feedback on previous mistakes. Map them to the different topics and create a frequency table where column A has a link to the topic and column B has the number of errors in that category. For anything that doesn't match, summarize them in a separate list called Other. Also create a 10-item quiz covering the most important points. Hide answers like this: [[answer:the answer goes here][___]] Use Org Mode syntax.")))
    :callback (lambda (response info)
                (with-current-buffer (get-buffer-create "*Feedback*")
                  (insert response)
                  (goto-char (point-min))
                  (pop-to-buffer (current-buffer))))))
;; Map lang-gptel feedback from the logbook to KwizIQ topics:1 ends here

;; [[file:Sacha.org::#multimedia-whisper][Using whisper.el to convert speech to text and save it to the currently clocked task in Org Mode or elsewhere:1]]
(use-package whisper
  :vc (:url "https://github.com/natrys/whisper.el")
  :load-path "~/vendor/whisper.el"
  :config
  (setq whisper--mode-line-recording-indicator "⏺")
  (setq whisper-quantize "q4_0")
  (setq whisper-install-directory "~/vendor")
  (setq whisper--install-path (concat
     (expand-file-name (file-name-as-directory whisper-install-directory))
     "whisper.cpp/"))
  ;; Get it running with whisper-server-mode set to nil first before you switch to 'local.
  ;; If you change models,
  ;; (whisper-install-whispercpp (whisper--check-install-and-run nil "whisper-start"))
  (setq whisper-server-mode 'local)
  (setq whisper-return-cursor-to-start nil)
  ;(setq whisper--ffmpeg-input-device "alsa_input.usb-Blue_Microphones_Yeti_Stereo_Microphone_REV8-00.analog-stereo")
  (setq whisper--ffmpeg-input-device "VirtualMicSink.monitor")
  (setq whisper-language "en")
  (setq whisper-recording-timeout 3000)
  (setq whisper-before-transcription-hook nil)
  (setq whisper-use-threads (1- (num-processors)))
  (setq whisper-transcription-buffer-name-function 'whisper--simple-transcription-buffer-name)
  (add-hook 'whisper-after-transcription-hook 'my-subed-fix-common-errors-from-start -100)
  :bind
  (("<f9>" . whisper-run)
   ("C-<f9>" . my-whisper-run)
   ("S-<f2>" . whisper-run)
   ("S-<f9>" . my-whisper-replay)
   ("M-<f9>" . my-whisper-toggle-language)))
;; Using whisper.el to convert speech to text and save it to the currently clocked task in Org Mode or elsewhere:1 ends here

;; [[file:Sacha.org::#multimedia-whisper][Using whisper.el to convert speech to text and save it to the currently clocked task in Org Mode or elsewhere:2]]
(defvar my-whisper-org-reminder-template "t")

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

(with-eval-after-load 'whisper
  (add-hook 'whisper-after-transcription-hook 'my-whisper-org-process-reminder 50))
;; Using whisper.el to convert speech to text and save it to the currently clocked task in Org Mode or elsewhere:2 ends here

;; [[file:Sacha.org::#multimedia-whisper][Using whisper.el to convert speech to text and save it to the currently clocked task in Org Mode or elsewhere:3]]
(defvar my-whisper-dir "~/recordings/whisper/")
(defun my-whisper-set-temp-filename ()
  (setq whisper--temp-file (expand-file-name
                            (format-time-string "%Y-%m-%d-%H-%M-%S.wav")
                            my-whisper-dir)))

(with-eval-after-load 'whisper
  (add-hook 'whisper-before-transcription-hook #'my-whisper-set-temp-filename))
;; Using whisper.el to convert speech to text and save it to the currently clocked task in Org Mode or elsewhere:3 ends here

;; [[file:Sacha.org::#multimedia-whisper][Using whisper.el to convert speech to text and save it to the currently clocked task in Org Mode or elsewhere:4]]
(defun my-whisper-replay (&optional file)
  "Replay the last temporary recording."
  (interactive (list
                (when current-prefix-arg
                  (read-file-name "File: " my-whisper-dir))))
  (setq whisper--temp-file (or file whisper--temp-file))
  (mpv-play whisper--temp-file))

(defun my-whisper-insert-retry (&optional file)
  (interactive (list
                (when current-prefix-arg
                  (read-file-name "File: " my-whisper-dir))))
  (whisper--cleanup-transcription)
  (setq whisper--marker (point-marker)
        whisper--temp-file (or file whisper--temp-file))
  (whisper--transcribe-audio))
;; Using whisper.el to convert speech to text and save it to the currently clocked task in Org Mode or elsewhere:4 ends here

;; [[file:Sacha.org::#multimedia-whisper][Using whisper.el to convert speech to text and save it to the currently clocked task in Org Mode or elsewhere:5]]
(defun my-whisper-toggle-language ()
  "Set the language explicitly, since sometimes auto doesn't figure out the right one."
  (interactive)
  (setq whisper-language (if (string= whisper-language "en") "fr" "en"))
  ;; If using a server, we need to restart for the language
  (when (process-live-p whisper--server-process) (kill-process whisper--server-process))
  (message "%s" whisper-language))
;; Using whisper.el to convert speech to text and save it to the currently clocked task in Org Mode or elsewhere:5 ends here

;; [[file:Sacha.org::#multimedia-whisper][Using whisper.el to convert speech to text and save it to the currently clocked task in Org Mode or elsewhere:6]]
(defun my-whisper-reset (text)
  (setq my-whisper-skip-annotation nil)
  (remove-hook 'whisper-insert-text-at-point #'my-whisper-org-save-to-clocked-task)
  text)
;; Using whisper.el to convert speech to text and save it to the currently clocked task in Org Mode or elsewhere:6 ends here

;; [[file:Sacha.org::whisper-insert-text-at-point-functions][whisper-insert-text-at-point-functions]]
;; Only works with my tweaks to whisper.el
;; https://github.com/sachac/whisper.el/tree/whisper-insert-text-at-point-function
(with-eval-after-load 'whisper
  (setq whisper-insert-text-at-point
        '(my-whisper-handle-commands
          my-whisper-save-text
          my-whisper-save-to-file
          my-whisper-maybe-expand-snippet
          my-speech-input-quantified-track
          my-whisper-maybe-type
          my-whisper-maybe-type-with-hints
          my-whisper-insert
          my-whisper-reset)))
;; whisper-insert-text-at-point-functions ends here

;; [[file:Sacha.org::#multimedia-whisper][Using whisper.el to convert speech to text and save it to the currently clocked task in Org Mode or elsewhere:8]]
(defvar my-whisper-last-annotation nil "Last annotation so we can skip duplicates.")
(defvar my-whisper-skip-annotation nil)
(defvar my-whisper-target-markers nil "List of markers to send text to.")

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
;; Using whisper.el to convert speech to text and save it to the currently clocked task in Org Mode or elsewhere:8 ends here

;; [[file:Sacha.org::my-whisper-maybe-type][my-whisper-maybe-type]]
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
;; my-whisper-maybe-type ends here

;; [[file:Sacha.org::#multimedia-whisper][Using whisper.el to convert speech to text and save it to the currently clocked task in Org Mode or elsewhere:10]]
(defun my-whisper-clear-markers ()
  (interactive)
  (setq my-whisper-target-markers nil))

(defun my-whisper-use-current-point (&optional add)
  (interactive (list current-prefix-arg))
  (if add
      (push (point-marker) my-whisper-target-markers)
    (setq my-whisper-target-markers (list (point-marker)))))

(defun my-whisper-run-at-point (&optional add)
  (interactive (list current-prefix-arg))
  (my-whisper-clear-markers)
  (whisper-run))

(keymap-global-set "<f9>" #'my-whisper-run-at-point)
(keymap-global-set "<kp-1>" #'whisper-run)

(defun my-whisper-jump-to-marker ()
  (interactive)
  (with-current-buffer (marker-buffer (car my-whisper-target-markers))
    (goto-char (car my-whisper-target-markers))))

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

(defun my-whisper-run (&optional skip-annotation)
  (interactive (list current-prefix-arg))
  (require 'whisper)
  (add-hook 'whisper-insert-text-at-point #'my-whisper-org-save-to-clocked-task -10)
  (whisper-run)
  (when skip-annotation
    (setq my-whisper-skip-annotation t)))

(defun my-whisper-save-text (text)
  "Save TEXT beside `whisper--temp-file'."
  (when text
    (let ((link (org-store-link nil)))
      (with-temp-file (concat (file-name-sans-extension whisper--temp-file) ".txt")
        (when link
          (insert link "\n"))
        (insert text)))
    text))

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

(with-eval-after-load 'org
  (add-hook 'org-clock-in-hook #'my-whisper-org-clear-saved-annotation))

(defun my-whisper-org-clear-saved-annotation ()
  (setq my-whisper-org-last-annotation nil))
;; Using whisper.el to convert speech to text and save it to the currently clocked task in Org Mode or elsewhere:10 ends here

;; [[file:Sacha.org::#multimedia-whisper][Using whisper.el to convert speech to text and save it to the currently clocked task in Org Mode or elsewhere:11]]
(defvar my-whisper-notes "~/sync/stream/narration.org")
(defun my-whisper-save-to-file (text)
  (when text
    (let ((link (org-store-link nil)))
      (with-current-buffer (find-file-noselect my-whisper-notes)
        (goto-char (point-max))
        (insert "\n\n" (format-time-string "%H:%M ") text "\n" (if link (concat link "\n") ""))
        (save-buffer)
        (run-at-time 0.5 nil (lambda (text) (message "Saved to file: %s" text)) text)))
    text))
;; Using whisper.el to convert speech to text and save it to the currently clocked task in Org Mode or elsewhere:11 ends here

;; [[file:Sacha.org::#multimedia-whisper][Using whisper.el to convert speech to text and save it to the currently clocked task in Org Mode or elsewhere:12]]
(defun my-whisper-redo ()
  (interactive)
  (setq whisper--marker (point-marker))
  (whisper--transcribe-audio))
;; Using whisper.el to convert speech to text and save it to the currently clocked task in Org Mode or elsewhere:12 ends here

;; [[file:Sacha.org::#writing-and-editing-speech-recognition-emacs-and-whisper-el-trying-out-different-speech-to-text-backends-and-models][Emacs and whisper.el: Trying out different speech-to-text backends and models:1]]
(defvar my-whisper-url-format "http://%s:%d/transcribe")
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
(defun my-whisper--check-model-consistency () t)
(with-eval-after-load 'whisper
  (advice-add 'whisper--transcribe-via-local-server :override #'my-whisper--transcribe-via-local-server)
  (advice-add 'whisper--check-model-consistency :override #'my-whisper--check-model-consistency)
  (advice-add 'whisper--ensure-server :override #'my-whisper--ensure-server)
  )
;; Emacs and whisper.el: Trying out different speech-to-text backends and models:1 ends here

;; [[file:Sacha.org::#writing-and-editing-speech-recognition-emacs-and-whisper-el-trying-out-different-speech-to-text-backends-and-models][Emacs and whisper.el: Trying out different speech-to-text backends and models:2]]
(defun my-test-whisper-api (url &optional args)
  (with-temp-buffer
    (apply #'call-process "curl" nil t nil "-s"
           url
         (append (mapcan
                  (lambda (h) (list "-H" h))
                  (list "Content-Type: multipart/form-data"))
                 (mapcan
                  (lambda (h) (list "-F" h))
                  (list (concat "file=@" whisper--temp-file)
                        "temperature=0.0"
                        "temperature_inc=0.2"
                        "response_format=verbose_json"
                        (concat "language=" whisper-language)))
                 args))
    (message "%s %s" (buffer-string) url)))
;; Emacs and whisper.el: Trying out different speech-to-text backends and models:2 ends here

;; [[file:Sacha.org::#writing-and-editing-speech-recognition-emacs-and-whisper-el-trying-out-different-speech-to-text-backends-and-models][Emacs and whisper.el: Trying out different speech-to-text backends and models:4]]
(setq whisper-server-port 8001 whisper-model "Systran/faster-whisper-base.en"
      my-whisper-url-format "http://%s:%d/v1/audio/transcriptions")
;; Emacs and whisper.el: Trying out different speech-to-text backends and models:4 ends here

;; [[file:Sacha.org::#writing-and-editing-speech-recognition-emacs-and-whisper-el-trying-out-different-speech-to-text-backends-and-models][Emacs and whisper.el: Trying out different speech-to-text backends and models:5]]
(defvar my-speaches-process nil)
(defvar my-speaches-dir "~/vendor/speaches")
(defvar my-speaches-command `(,(expand-file-name ".venv/bin/uvicorn" my-speaches-dir)
                              "--factory"
                              "--host"
                              "0.0.0.0"
                              "--port"
                              ,(number-to-string whisper-server-port)
                              "speaches.main:create_app"))

(defun my-whisper--ensure-server ()
  "Start the process if it's not already running."
  (interactive)
  (unless (process-live-p my-speaches-process)
    (let ((default-directory my-speaches-dir))
      (setq my-speaches-process
            (make-process
             :name "speaches"
             :command my-speaches-command
             :buffer (get-buffer-create "*speaches*")
             :stderr (get-buffer-create "*speaches-err*"))))))
;; Emacs and whisper.el: Trying out different speech-to-text backends and models:5 ends here

;; [[file:Sacha.org::#writing-and-editing-speech-recognition-queue-multiple-transcriptions-with-whisper-el-speech-recognition][Queuing multiple transcriptions with whisper.el speech recognition:1]]
(defvar my-whisper--queue nil)
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

(keymap-global-set "<kp-9>" #'my-whisper-continue)
(keymap-global-set "<kp-8>" #'my-whisper-discard-and-continue)
(keymap-global-set "C-<kp-9>" #'my-whisper-done)
;; Queuing multiple transcriptions with whisper.el speech recognition:1 ends here

;; [[file:Sacha.org::#writing-and-editing-speech-recognition-using-silero-voice-activity-detection-to-automatically-queue-multiple-transcriptions-with-natrys-whisper-el][Using Silero voice activity detection to automatically queue multiple transcriptions with natrys/whisper.el:1]]
(use-package speech-input :load-path "~/proj/speech-input/")
;; Using Silero voice activity detection to automatically queue multiple transcriptions with natrys/whisper.el:1 ends here

;; [[file:Sacha.org::#writing-and-editing-speech-recognition-using-silero-voice-activity-detection-to-automatically-queue-multiple-transcriptions-with-natrys-whisper-el][Using Silero voice activity detection to automatically queue multiple transcriptions with natrys/whisper.el:2]]
(with-eval-after-load 'speech-input-vad
  (add-hook 'speech-input-vad-on-end-functions #'my-whisper-maybe-continue))

(defun my-whisper-maybe-continue ()
  (when (process-live-p whisper--recording-process)
    (my-whisper-continue)))
;; Using Silero voice activity detection to automatically queue multiple transcriptions with natrys/whisper.el:2 ends here

;; [[file:Sacha.org::#writing-and-editing-speech-recognition-slowly-building-speech-based-commands-for-emacs][Slowly building speech-based commands for Emacs:1]]
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
    ("expand" . expand-region))
  "Commands for speech recognition.")

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

(with-eval-after-load 'whisper
  (add-hook 'whisper-after-transcription-hook 'my-whisper-process-replacements 70))
;; Slowly building speech-based commands for Emacs:1 ends here

;; [[file:Sacha.org::#writing-and-editing-speech-recognition-okay-track][Okay, track...:1]]
(defvar my-quantified-common-categories
  '(("Emacs" . "Discretionary - Productive - Emacs")
    ("Child care" . "Childcare")
    ("French" . "Discretionary - French")
    ("Brigade" . "Discretionary - Productive - Bike Brigade")
    ("Consulting" . "E1 Gen")))

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
;; Okay, track...:1 ends here

;; [[file:Sacha.org::#writing-and-editing-speech-recognition-using-speech-recognition-for-translations-in-emacs-and-faking-in-buffer-completion-for-the-results][Using speech recognition for on-the-fly translations in Emacs and faking in-buffer completion for the results:1]]
(defun my-lang-en-to-fr (text &optional display-only)
  (interactive (list (read-string "Text: ") current-prefix-arg))
  (let* ((url "https://translation.googleapis.com/language/translate/v2")
         (params `(("key" . ,(getenv "GOOGLE_API_KEY"))
                   ("q" . ,text)
                   ("source" . "en")
                   ("target" . "fr")
                   ("format" . "text")))
         (query-string (mapconcat
                        (lambda (pair)
                          (format "%s=%s"
                                  (url-hexify-string (car pair))
                                  (url-hexify-string (cdr pair))))
                        params
                        "&"))
         (full-url (concat url "?" query-string)))
    (let* ((response (plz 'get full-url :as #'json-read))
           (data (alist-get 'data response))
           (translations (alist-get 'translations data))
           (first-translation (car translations))
           (translated-text (alist-get 'translatedText first-translation)))
      (when (called-interactively-p 'any)
        (if display-only
            (message "%s" translated-text)
          (insert translated-text)))
      translated-text)))
;; Using speech recognition for on-the-fly translations in Emacs and faking in-buffer completion for the results:1 ends here

;; [[file:Sacha.org::#writing-and-editing-speech-recognition-using-speech-recognition-for-translations-in-emacs-and-faking-in-buffer-completion-for-the-results][Using speech recognition for on-the-fly translations in Emacs and faking in-buffer completion for the results:2]]
(defun my-whisper-translate ()
  (goto-char (point-min))
  (let ((case-fold-search t))
    (when (re-search-forward "okay[,\\.]? translate[,\\.]? \\(.+\\)\\|okay[,\\.]? \\(.+?\\) in French" nil t)
      (let* ((s (or (match-string 1) (match-string 2)))
             (translation (save-match-data (my-lang-en-to-fr s))))
        (replace-match
         (propertize translation
                     'type-hint translation
                     'type-original s
                     'help-echo s))))))

(with-eval-after-load 'whisper
  (add-hook 'whisper-after-transcription-hook 'my-whisper-translate 70))
;; Using speech recognition for on-the-fly translations in Emacs and faking in-buffer completion for the results:2 ends here

;; [[file:Sacha.org::#writing-and-editing-speech-recognition-using-speech-recognition-for-translations-in-emacs-and-faking-in-buffer-completion-for-the-results][Using speech recognition for on-the-fly translations in Emacs and faking in-buffer completion for the results:3]]
(defun my-whisper-maybe-type-with-hints (text)
  "Add this function to `whisper-insert-text-at-point'."
  (let* ((hint (and text (org-find-text-property-in-string 'type-hint text)))
         (original (and text (org-find-text-property-in-string 'type-original text))))
    (if hint
        (progn
          (learn-lang-type-with-hint hint original)
          nil)
      text)))
;; Using speech recognition for on-the-fly translations in Emacs and faking in-buffer completion for the results:3 ends here

;; [[file:Sacha.org::#writing-and-editing-speech-recognition-expanding-yasnippet-by-voice][Expanding yasnippets by voice in Emacs and other applications:1]]
(defun my-whisper-maybe-expand-snippet (text)
  "Add to `whisper-insert-text-at-point'."
  (if (and text
           (string-match
            "^ok\\(?:ay\\)?[,\\.]? \\(.+\\)" text))
    (let* ((name
            (downcase
             (string-trim
              (replace-regexp-in-string "[,\\.]" "" (match-string 1 text)))))
           (matching
            (seq-find (lambda (o)
                        (subed-word-data-compare-normalized-string-distance
                         name
                         (downcase (yas--template-name o))))
                      (yas--all-templates (yas--get-snippet-tables)))))
      (if matching
          (progn
            (if (frame-focus-state)
                (progn
                  (yas-expand-snippet matching)
                  nil)
              ;; In another application
              (with-temp-buffer
                (yas-minor-mode)
                (yas-expand-snippet matching)
                (buffer-string))))
        text))
    text))
;; Expanding yasnippets by voice in Emacs and other applications:1 ends here

;; [[file:Sacha.org::#writing-and-editing-speech-recognition-streaming-speech-recognition-into-emacs-using-google-chrome-web-speech-api][Streaming speech recognition into Emacs using Google Chrome Web Speech API:1]]
(defvar my-chrome-speech-ws nil "Websocket for connecting.")
(defvar my-chrome-speech-url "ws://127.0.0.1:8000/ws" "Websocket URL to connect to for captions.")
(defun my-chrome-speech-connect ()
  (interactive)
  (unless (websocket-p my-chrome-speech-ws)
    (setq my-chrome-speech-ws
          (websocket-open
           my-chrome-speech-url
           :on-message #'my-chrome-speech-handle))))
(defun my-chrome-speech-reconnect ()
  (interactive)
  (my-chrome-speech-disconnect)
  (my-chrome-speech-connect))

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
;; Streaming speech recognition into Emacs using Google Chrome Web Speech API:1 ends here

;; [[file:Sacha.org::#writing-and-editing-speech-recognition-streaming-speech-recognition-into-emacs-using-google-chrome-web-speech-api][Streaming speech recognition into Emacs using Google Chrome Web Speech API:2]]
(defun my-chrome-speech-handle (_ frame)
  (let* ((info (json-parse-string (websocket-frame-text frame)
                                  :object-type 'alist)))
    (seq-reduce (lambda (prev cur)
                  (funcall cur prev))
                my-speech-functions info)))

(defun my-chrome-speech-disconnect ()
  (interactive)
  (websocket-close my-chrome-speech-ws)
  (setq my-chrome-speech-ws nil))

(defvar my-chrome-speech-recognition-server-process nil)
(defvar my-chrome-speech-dir "~/proj/emacs-web-speech")
(defun my-chrome-ensure-speech-recognition-server ()
  (interactive)
  (unless (process-live-p my-chrome-speech-recognition-server-process)
    (let ((default-directory my-chrome-speech-dir))
      (setq my-chrome-speech-recognition-server-process
            (make-process
             :name "live-captioning"
             :buffer "*live-captioning*"
             :command (list (expand-file-name ".venv/bin/python3") "app.py")))
      (sit-for 1))))

(defun my-chrome-stop-speech-recognition-server ()
  (interactive)
  (when (process-live-p my-chrome-speech-recognition-server-process)
    (kill-process my-chrome-speech-recognition-server-process)))

(defvar-local my-chrome-speech-session nil)
(defvar-local my-chrome-speech-user-dir nil)
(defvar-local my-chrome-speech-lang "en-US")
(defun my-chrome-speech-new-session (&optional id lang local-only)
  (interactive (list
                (file-name-base
                 (make-temp-name
                  (expand-file-name "chrome-"
                                    (temporary-file-directory))))
                my-chrome-speech-lang
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
                 (when my-chrome-speech-user-dir
                   (delete-directory my-chrome-speech-user-dir t))))))))
    (with-current-buffer (process-buffer process)
      (setq-local my-chrome-speech-user-dir user-dir))
    (switch-to-buffer (format "*Captions - %s*" base-id))
    (setq-local my-chrome-speech-session base-id)
    (my-chrome-speech-connect)))
;; Streaming speech recognition into Emacs using Google Chrome Web Speech API:2 ends here

;; [[file:Sacha.org::#writing-and-editing-speech-recognition-streaming-speech-recognition-into-emacs-using-google-chrome-web-speech-api][Streaming speech recognition into Emacs using Google Chrome Web Speech API:3]]
(defun my-speech-sessions ()
  (seq-keep (lambda (o)
              (with-current-buffer o
                (when my-speech-session
                  (cons my-speech-session o))))
            (buffer-list)))

(defun my-speech-clear-all ()
  (interactive)
  (dolist (session (my-speech-sessions))
    (my-speech-clear session)))

(defun my-speech-clear (session)
  (interactive (list (my-speech-select-session)))
  (with-current-buffer (cdr session)
      (erase-buffer)
      (setq-local my-speech-previous-final nil)))

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

(defun my-speech-get-text-and-clear (session)
  (let (text)
    (with-current-buffer (cdr session)
      (setq text (buffer-substring-no-properties (point-min) (point-max)))
      (erase-buffer)
      (setq-local my-speech-previous-final nil))
    text))

(defun my-speech-insert-at-point (session)
  (interactive (list (my-speech-select-session)))
  (insert (my-speech-get-text-and-clear session)))

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
;; Streaming speech recognition into Emacs using Google Chrome Web Speech API:3 ends here

;; [[file:Sacha.org::#writing-and-editing-speech-recognition-streaming-speech-recognition-into-emacs-using-google-chrome-web-speech-api][Streaming speech recognition into Emacs using Google Chrome Web Speech API:4]]
(defvar my-speech-etherpads nil "Alist of (session . pad-id)")
;; (setq my-speech-etherpads '(("chrome-VgjMhu" . "test")))

(defun my-speech-append-to-etherpad (info)
  (when (and info (string= (assoc-default 'type info) "FINAL"))
    (let-alist info
      (when-let* ((pad-id (assoc-default .session my-speech-etherpads #'string=)))
        (emacsconf-pad-append-text pad-id (concat "\n" .content)))))
  info)

(defun my-speech-link-etherpad (session pad-id)
  (interactive (list
                (my-speech-select-session)
                (read-string "Pad ID: ")))
  (add-to-list 'my-speech-etherpads
               (cons (concat "#" (car session))
                     pad-id)))

(defun my-speech-unlink-etherpad (pad-id)
  (interactive (list (completing-read "Pad: " (mapcar 'cdr my-speech-etherpads))))
  (setq my-speech-etherpads
        (seq-remove (lambda (o)
                      (string= (cdr o) pad-id))
                    my-speech-etherpads)))

(add-to-list 'my-speech-functions #'my-speech-append-to-etherpad)
;; Streaming speech recognition into Emacs using Google Chrome Web Speech API:4 ends here

;; [[file:Sacha.org::#writing-and-editing-speech-recognition-streaming-speech-recognition-into-emacs-using-google-chrome-web-speech-api][Streaming speech recognition into Emacs using Google Chrome Web Speech API:5]]
(defvar my-speech-erc nil "Alist of (session . channel)")
;; (setq my-speech-erc '(("#chrome-HP7k8I" . "#emacsconf-test")))

(defun my-speech-send-to-erc (info)
  (when (and info (string= (assoc-default 'type info) "FINAL"))
    (let-alist info
      (when-let* ((channel (assoc-default .session my-speech-erc #'string=)))
        (emacsconf-erc-with-channels (list channel)
          (erc-send-message (string-trim .content))))))
  info)

(defun my-speech-link-erc (session channel)
  (interactive (list
                (my-speech-select-session)
                (read-string "Channel: ")))
  (add-to-list 'my-speech-erc
               (cons (concat "#" (car session))
                     channel)))

(defun my-speech-unlink-channel (channel)
  (interactive (list (completing-read "Channel: " (mapcar 'cdr my-speech-erc))))
  (setq my-speech-erc
        (seq-remove (lambda (o)
                      (string= (cdr o) channel))
                    my-speech-erc)))

(add-to-list 'my-speech-functions #'my-speech-send-to-erc)
;; Streaming speech recognition into Emacs using Google Chrome Web Speech API:5 ends here

;; [[file:Sacha.org::#writing-and-editing-speech-recognition-streaming-speech-recognition-into-emacs-using-google-chrome-web-speech-api][Streaming speech recognition into Emacs using Google Chrome Web Speech API:6]]
(defun my-speech-fix-common-errors (info)
  (with-temp-buffer
    (insert (alist-get 'content info))
    (goto-char (point-min))
    (my-subed-fix-common-errors-from-start)
    (setf (alist-get 'content info) (buffer-string)))
  info)
(add-hook 'my-speech-functions #'my-speech-fix-common-errors -100)
;; Streaming speech recognition into Emacs using Google Chrome Web Speech API:6 ends here

;; [[file:Sacha.org::#writing-and-editing-speech-recognition-streaming-speech-recognition-into-emacs-using-google-chrome-web-speech-api][Streaming speech recognition into Emacs using Google Chrome Web Speech API:7]]
(defun my-speech-insert-at-markers (info)
  (when (and my-whisper-target-markers info)
    (my-whisper-insert (alist-get 'content info))))
(add-hook 'my-speech-functions #'my-speech-insert-at-markers 100)
;; Streaming speech recognition into Emacs using Google Chrome Web Speech API:7 ends here

;; [[file:Sacha.org::#writing-and-editing-speech-recognition-streaming-speech-recognition-into-emacs-using-google-chrome-web-speech-api-speech-and-subed-record][speech and subed-record:1]]
(defvar my-speech-timestamp-adjust-before 1000)
(defvar my-speech-timestamp-adjust-after 300)

(defun my-speech-subed-record-convert-timestamp (s)
  "Convert S into a relative number of milliseconds based on `subed-record-filename'."
  (floor (* (float-time (time-subtract (date-to-time s) subed-record-start-time)) 1000.0)))

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

(defun my-speech-subed-record-close-enough (s1 s2)
  "Return t if it's close enough."
  (< (my-speech-subed-record-distance s1 s2) 0.3))

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
(defun my-speech-subed-record-process (info)
  (let ((text (alist-get 'content info))
        (current (subed-subtitle-text)))
    (cond
     ((my-speech-subed-record-close-enough text current)
      (my-speech-subed-record-update info)
      (subed-forward-subtitle-text)
      (my-lang-say-current-subtitle
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
(defun my-speech-subed-record (info)
  (when (and (string= (alist-get 'type info) "FINAL")
             (derived-mode-p 'subed-mode)
             (boundp 'subed-record-start-time)
             subed-record-start-time
             (not my-speech-subed-ignore))
    (my-speech-subed-record-process info))
  info)

(add-to-list 'my-speech-functions #'my-speech-subed-record)
;; speech and subed-record:1 ends here

;; [[file:Sacha.org::#writing-and-editing-speech-recognition-try-speaches-with-realtime][Try speaches with realtime:1]]
(defvar my-speaches-process nil)
(defvar my-speaches-dir "~/vendor/speaches")
(defun my-speaches-start ()
  (interactive)
  (unless (process-live-p my-speaches-process)
    (let ((default-directory my-speaches-dir))
      (setq my-speaches-process
            (make-process
             :name "speaches-bridge"
             :buffer "*speaches-output*" ; Standard output buffer
             :command '("bash" "-c" "rec -q -t raw -r 16000 -c 1 -b 16 -e signed-integer - | uv run python3 stream.py")
             :filter #'my-speaches-filter
             :sentinel (lambda (proc event)
                         (when (memq (process-status proc) '(exit signal))
                           (message "Speaches process finished: %s" event)))
             :stderr "*speaches-stderr*" ; Separate buffer for Python errors/logs
             :noquery t)))
    (message "Speaches started.")))

(defun my-speaches-filter (proc string)
  "Accumulate STRING and call processor on complete JSON lines."
  (let ((moving-point (process-mark proc))
        results)
    (with-current-buffer (process-buffer proc)
      (save-excursion
        (goto-char moving-point)
        (insert string)
        (set-marker (process-mark proc) (point))
        (goto-char (point-min))
        (while (search-forward "\n" nil t)
          (let ((line (buffer-substring (point-min) (1- (point)))))
            (delete-region (point-min) (point))
            (unless (string-empty-p (string-trim line))
              (condition-case err
                  (let ((json-obj (json-parse-string line :object-type 'alist)))
                    (push json-obj results))
                (error (message "JSON parse error: %s" err))))))))
    (mapc #'my-speaches-process-logic
          (nreverse results))))

(defun my-speaches-process-logic (o)
  "Handle the parsed ALIST from the Speaches bridge."
  (let ((type (cdr (assoc 'type o))))
    (cond
     ((string= type "conversation.item.input_audio_transcription.completed")
      (let ((text (cdr (assoc 'transcript o))))
        (with-current-buffer (get-buffer-create "*speaches*")
          (goto-char (point-max))
          (unless (bolp) (insert "\n"))
          (insert text))))
     (t (prin1 o)))))
;; Try speaches with realtime:1 ends here

;; [[file:Sacha.org::#using-emacs-lisp-to-send-audio-files-to-deepgram-and-format-vtts][Using Emacs Lisp to send audio files to Deepgram and format VTTs:1]]
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
;; Using Emacs Lisp to send audio files to Deepgram and format VTTs:1 ends here

;; [[file:Sacha.org::#rerecognize][Rerecognize this audio and reprocess it:1]]
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
;; Rerecognize this audio and reprocess it:1 ends here

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

;; [[file:Sacha.org::#append-to-emacsconf-etherpad][Append to EmacsConf Etherpad:1]]
(defvar my-live-speech-etherpad-id nil)
(defun my-live-speech-append-to-etherpad (recognition-results)
	(when my-live-speech-etherpad-id
		(emacsconf-pad-append-text my-live-speech-etherpad-id (concat " " (assoc-default 'transcript recognition-results)))))
;; Append to EmacsConf Etherpad:1 ends here

;; [[file:Sacha.org::#utf-8][UTF-8:1]]
(prefer-coding-system 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))
;; UTF-8:1 ends here

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

(defun my-wdiff-strings (original new)
  (let ((original-file (make-temp-file "wdiff"))
        (new-file (make-temp-file "wdiff")))
    (write-region original nil original-file)
    (write-region new nil new-file)
    (my-wdiff original-file new-file)
    (delete-file original-file)
    (delete-file new-file)))

(defun my-wdiff-org-text-with-clipboard ()
  (interactive)
  (my-wdiff-strings (my-org-subtree-text-without-blocks)
                    (car kill-ring)))

(defun my-wdiff-buffer-with-file ()
	(interactive)
	(let ((s (buffer-string))
				(temp-file (make-temp-file "temp")))
		(with-temp-file temp-file
			(insert s))
		(my-wdiff (buffer-file-name) temp-file)
		(delete-file temp-file)))

(defun my-wdiff-find-at-point ()
  (interactive)
  (unless (looking-at "\\[-")
    (re-search-backward "\\[-" nil t)
    (when (looking-at "\\[-\\(.+?\\)-\\] {\\+\\(.+?\\)\\+}")
      (let ((s (match-string 1))
            (rep (match-string 2)))
        (goto-char (match-end 0))
        (other-window 1)
        (if (re-search-forward (regexp-quote s) nil t)
            (progn
              (save-match-data (pulse-momentary-highlight-region (match-beginning 0)
                                                                 (match-end 0)))
              (when (save-match-data (y-or-n-p (format "Change %s to %s: " s rep)))
                (replace-match rep t t)
                t))
          (message "Could not find %s to change to %s" s rep)
          nil)))))

(defun my-wdiff-next ()
  (interactive)
  (other-window 1)
  (re-search-forward "{\\+\\(.+?\\)\\+}")
  (pulse-momentary-highlight-region (match-beginning 0) (match-end 0))
  (my-wdiff-find-at-point))

(defun my-wdiff-next-loop ()
  (interactive)
  (while (my-wdiff-next)))
;; Wdiff:1 ends here

;; [[file:Sacha.org::#writing-and-editing-denote][Denote:1]]
(use-package denote
	:config
	(setopt denote-directory "~/sync/Notes")
)
;; Denote:1 ends here

;; [[file:Sacha.org::org-package-setup][org-package-setup]]
(defvar my-org-inbox-file "~/sync/orgzly/Inbox.org")
(use-package org
  :load-path ("~/vendor/org-mode/lisp" "~/vendor/org-mode/contrib/lisp")
  :preface (load "~/vendor/org-mode/lisp/org-loaddefs.el" nil t)
  :bind
  (:map org-mode-map
        ("C-M-<return>" . org-insert-subheading)
        ("M-." . my-org-defun-open))
	:custom
	(org-export-with-sub-superscripts nil)
	(org-footnote-section nil)
	(org-fold-catch-invisible-edits 'smart))
;; org-package-setup ends here

;; [[file:Sacha.org::#org-mode-after-i-jump-to-a-task-from-org-clock-goto-narrow-to-it-automatically][After I jump to a task from org-clock-goto, narrow to it automatically:1]]
(with-eval-after-load 'org
  (add-hook 'org-clock-goto-hook #'org-narrow-to-subtree)
  (add-hook 'org-agenda-after-show-hook #'org-narrow-to-subtree))
;; After I jump to a task from org-clock-goto, narrow to it automatically:1 ends here

;; [[file:Sacha.org::#org-mode-find-first-common-org-mode-heading][Find first common Org Mode heading:1]]
(defun my-org-find-first-common-heading (other-buffer)
	"Go to the first top-level heading in common with OTHER-BUFFER.
This is helpful when resolving sync conflicts."
	(interactive (list (read-buffer "Other buffer: ")))
	(let ((other-headings (with-current-buffer (get-buffer other-buffer)
													(org-map-entries (lambda () (org-entry-get (point) "ITEM")) "LEVEL=1"))))
		(goto-char
		 (catch 'done
			 (org-map-entries
				(lambda ()
					(when (member (org-entry-get (point) "ITEM") other-headings)
						(throw 'done (point))))
				"LEVEL=1")))
		))
;; Find first common Org Mode heading:1 ends here

;; [[file:Sacha.org::#org-mode-writing-about-sketches-and-including-their-text][Writing about sketches and including their text:1]]
(defun my-insert-sketch-and-text (sketch)
	(interactive (list (my-complete-sketch-filename)))
  (when (and (listp sketch) (alist-get 'source_path sketch))
    (setq sketch (my-get-image-filename (file-name-base (alist-get 'source_path sketch)))))
	(insert
	 (if (string= (file-name-extension sketch) "svg")
			 (format
				"#+begin_panzoom\n%s\n#+end_panzoom\n\n"
				(org-link-make-string (concat "file:" sketch)))
		 (concat (org-link-make-string (concat "sketchFull:" (file-name-base sketch))) "\n\n")))
	(let ((links (my-org-links-from-file (concat (file-name-sans-extension sketch) ".txt")))
				(subheading-level (1+ (org-current-level))))
		(insert (if links
								"#+begin_my_details Text and links from sketch\n"
							"#+begin_my_details Text from sketch\n"))
		(my-sketch-insert-text sketch)
		(unless (bolp) (insert "\n"))
		(insert "#+end_my_details")
		(dolist (section (seq-filter (lambda (entry) (string-match "^#" (cdr entry)))
																 links))
			(org-end-of-subtree)
			(insert "\n\n")
			(org-insert-heading nil nil subheading-level)
			(insert (car section))
			(org-entry-put (point) "CUSTOM_ID" (substring (cdr section) 1)))))

(defun my-write-about-sketch (sketch)
  (interactive (list (my-complete-sketch-filename)))
  ;(shell-command "make-sketch-thumbnails")
  (find-file "~/sync/orgzly/posts.org")
  (goto-char (point-min))
	(unless (org-at-heading-p) (outline-next-heading))
  (org-insert-heading nil nil t)
	(insert (string-trim (replace-regexp-in-string "^[-0-9]+ *" "" (file-name-base sketch))) "\n\n")
	(my-insert-sketch-and-text sketch)
	(insert "\n/Feel free to use this sketch under the [[https://creativecommons.org/licenses/by/4.0/][Creative Commons Attribution License]]./\n")
  (delete-other-windows)
  (save-excursion
    (with-selected-window (split-window-horizontally)
      (find-file sketch))))
;; Writing about sketches and including their text:1 ends here

;; [[file:Sacha.org::#org-mode-remove-open-org-mode-clock-entries][Remove open Org Mode clock entries:1]]
(defun my-org-delete-open-clocks ()
	(interactive)
	(flush-lines
	 (rx
		line-start
		(zero-or-more space)
		"CLOCK:"
		(one-or-more space)
		(regexp org-ts-regexp-inactive)
		(zero-or-more space)
		line-end)))
;; Remove open Org Mode clock entries:1 ends here

;; [[file:Sacha.org::#org-refile-insert-link][Insert a link to an Org Mode heading from an org-refile prompt:1]]
(defun my-embark-org-insert-link-from-path (path)
	(interactive (list (car (org-refile-get-location))))
	(let* ((extra (if org-refile-use-outline-path "/" ""))
				 (tbl (mapcar
							 (lambda (x)
								 (if (and (not (member org-refile-use-outline-path
																			 '(file full-file-path title)))
													(not (equal filename (file-truename (nth 1 x)))))
										 (cons (concat (car x) extra " ("
																	 (file-name-nondirectory (nth 1 x)) ")")
													 (cdr x))
									 (cons (concat (car x) extra) (cdr x))))
							 org-refile-target-table))
				 link)
		(insert (save-window-excursion
							(save-excursion
								(org-goto-marker-or-bmk
								 (elt
									(org-refile--get-location path tbl)
									3))
								(org-store-link nil))))))
(defvar-keymap my-org-path-map
	:doc "Shortcuts for working with Org paths from `org-refile'."
	"i" #'my-embark-org-insert-link-from-path
	"L" #'my-embark-org-insert-link-from-path)
(with-eval-after-load 'marginalia
	(add-to-list 'marginalia-prompt-categories '("Goto\\|Refile" . my-org-path)))
(with-eval-after-load 'embark
	(add-to-list 'embark-keymap-alist '(my-org-path . my-org-path-map)))
;; Insert a link to an Org Mode heading from an org-refile prompt:1 ends here

;; [[file:Sacha.org::#org-refile-insert-link][Insert a link to an Org Mode heading from an org-refile prompt:2]]
(with-eval-after-load 'consult-org
	(keymap-set embark-org-heading-map "L" #'embark-org-insert-link-to))
;; Insert a link to an Org Mode heading from an org-refile prompt:2 ends here

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

;; [[file:Sacha.org::#modules][Modules:1]]
(setq org-modules '(org-habit
                    org-mouse
                    org-protocol
                    org-annotate-file
                    ol-info
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

;; [[file:Sacha.org::#keyboard-shortcuts][Keyboard shortcuts:1]]
(bind-key "C-c r" 'org-capture)
(bind-key "C-c a" 'org-agenda)
(bind-key "C-c l" 'org-store-link)
(bind-key "C-c L" 'org-insert-link-global)
(bind-key "C-c O" 'org-open-at-point-global)
;; Keyboard shortcuts:1 ends here

;; [[file:Sacha.org::#keyboard-shortcuts][Keyboard shortcuts:2]]
(with-eval-after-load 'org
  (bind-key "C-M-w" 'append-next-kill org-mode-map)
  (bind-key "C-TAB" 'org-cycle org-mode-map)
  (bind-key "C-c v" 'org-show-todo-tree org-mode-map)
  (bind-key "C-c C-r" 'org-refile org-mode-map)
  (bind-key "C-c R" 'org-reveal org-mode-map)
  (bind-key "C-c d" 'my-org-move-line-to-destination org-mode-map)
  (bind-key "C-c t s"  'my-split-sentence-and-capitalize org-mode-map)
  (bind-key "C-c t -"  'my-split-sentence-delete-word-and-capitalize org-mode-map)
  (bind-key "C-c t d"  'my-delete-word-and-capitalize org-mode-map)

  (bind-key "C-c C-p C-p" 'my-org-publish-maybe org-mode-map)
  (bind-key "C-c C-r" 'my-org-refile-and-jump org-mode-map))
;; Keyboard shortcuts:2 ends here

;; [[file:Sacha.org::#keyboard-shortcuts][Keyboard shortcuts:3]]
(with-eval-after-load 'org-agenda
  (bind-key "i" 'org-agenda-clock-in org-agenda-mode-map))
;; Keyboard shortcuts:3 ends here

;; [[file:Sacha.org::#org-mode-keyboard-shortcuts-speed-commands-org-mode-cutting-the-current-list-item-including-nested-lists-with-a-speed-command][Org Mode: Cutting the current list item (including nested lists) with a speed command:1]]
(defun my-org-use-speed-commands-for-headings-and-lists ()
  "Activate speed commands on list items too."
  (or (and (looking-at org-outline-regexp) (looking-back "^\**" nil))
      (save-excursion (and (looking-at (org-item-re)) (looking-back "^[ \t]*" nil)))))
(setq org-use-speed-commands 'my-org-use-speed-commands-for-headings-and-lists)
;; Org Mode: Cutting the current list item (including nested lists) with a speed command:1 ends here

;; [[file:Sacha.org::#org-mode-keyboard-shortcuts-speed-commands-org-mode-cutting-the-current-list-item-including-nested-lists-with-a-speed-command][Org Mode: Cutting the current list item (including nested lists) with a speed command:2]]
(defun my-org-cut-subtree-or-list-item (&optional n)
	"Cut current subtree or list item."
	(cond
	 ; limit this to certain files
	 ((not (string-match (regexp-opt '("Inbox.org" "posts.org" "news.org" "ipad.org"))
											 (or (buffer-file-name) "")))
		(message "Let's only cut things in inboxes")) ; do nothing
	 ((and (looking-at org-outline-regexp) (looking-back "^\**" nil))
		(org-cut-subtree n))
	 ((looking-at (org-item-re))
		(kill-region (org-beginning-of-item) (org-end-of-item)))))
(with-eval-after-load 'org
	(setf (alist-get "k" org-speed-commands nil nil #'string=)
				#'my-org-cut-subtree-or-list-item))
;; Org Mode: Cutting the current list item (including nested lists) with a speed command:2 ends here

;; [[file:Sacha.org::#org-mode-keyboard-shortcuts-other-speed-commands][Other speed commands:1]]
(setq org-use-effective-time t)

(defun my-org-goto-text-start ()
  (if (org-before-first-heading-p)
      (goto-char (point-min))
    (org-back-to-heading)
    (org-end-of-meta-data t)))

(defun my-org-subtree-text ()
  (if (derived-mode-p 'org-mode)
      (if (org-before-first-heading-p)
          (buffer-substring (point-min)
                            (save-excursion
                              (org-next-visible-heading)
                              (line-beginning-position)))
        (save-excursion
          (buffer-substring (save-excursion (org-end-of-meta-data t) (point))
                            (org-end-of-subtree))))
    (buffer-string)))

(defun my-org-copy-subtree-text ()
  (interactive)
  (kill-new (my-org-subtree-text)))

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
;; Other speed commands:1 ends here

;; [[file:Sacha.org::#org-navigation][Org navigation:1]]
(setq org-goto-interface 'outline-path-completion
      org-goto-max-level 10)
(require 'imenu)
(setq org-startup-folded nil)
(setq org-startup-with-inline-images nil)
(setq org-startup-with-link-previews nil)
(bind-key "C-c j" 'org-clock-goto) ;; jump to current task from anywhere
(bind-key "C-c C-w" 'org-refile)
(setq org-cycle-include-plain-lists 'integrate)
(setq org-catch-invisible-edits 'show-and-error)
;; Org navigation:1 ends here

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

;; [[file:Sacha.org::#viewing-navigating-and-editing-the-org-tree][Viewing, navigating, and editing the Org tree:1]]
(with-eval-after-load 'org
  (bind-key "C-c k" 'org-cut-subtree org-mode-map)
  (setq org-yank-adjusted-subtrees t))
;; Viewing, navigating, and editing the Org tree:1 ends here

;; [[file:Sacha.org::#finding-my-place-on-a-small-mobile-screen-with-org-back-to-heading][Finding my place on a small mobile screen with org-back-to-heading:1]]
(defun my-org-back-to-heading ()
  (interactive)
  (org-back-to-heading))

(use-package org
  :bind (:map org-mode-map
              ("C-c b" . my-org-back-to-heading)
              ("C-c p" . org-display-outline-path)))
;; Finding my place on a small mobile screen with org-back-to-heading:1 ends here

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

;; [[file:Sacha.org::#taking-notes][Taking notes:1]]
(setq org-directory "~/sync/orgzly/")
(setq org-default-notes-file "~/sync/orgzly/organizer.org")
;; Taking notes:1 ends here

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
	       "%i%?\n%a"
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
				("s" "Selection from browser" entry
				 (file ,my-org-inbox-file)
				 "* %a :website:\n:PROPERTIES:\n:CREATED: %U\n:END:\n#+begin_quote\n%i\n#+end_quote\n\n%?\n"
				 :prepend t)
				("S" "Screenshot" entry
				 (file ,my-org-inbox-file)
				 "* %^{Note}\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n[[file:%(my-latest-screenshot)]]\n"
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
         (file+olp+datetree "~/personal/books.org" "Inbox")
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
				("w" "Web" entry (file "~/sync/orgzly/Inbox.org")
				 "* %a
:PROPERTIES:
:CREATED: %U
:END:

%i
")
				("W" "Web bookmark" entry (file "~/sync/orgzly/resources.org")
				 "* %a
:PROPERTIES:
:CREATED: %U
:END:

%i
"
				 :prepend t)
				("y" "Yay Emacs" entry (file+headline "~/proj/stream/index.org" "Notes for this session")
				 "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n

%i

%a
"))
			org-capture-templates))))
(bind-key "C-M-r" 'org-capture)



;;(bind-key (kbd "<f5>") 'org-capture)
;; Templates:1 ends here

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

;; [[file:Sacha.org::#try-out-this-capture-command][Try out this capture command:1]]
(use-package git-link :defer t)
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

;; [[file:Sacha.org::#org-mode-tasks-managing-tasks-get-things-to-be-set-to-todo-when-they-repeat][Get things to be set to TODO when they repeat:1]]
(setq org-todo-repeat-to-state "TODO")
;; Get things to be set to TODO when they repeat:1 ends here

;; [[file:Sacha.org::#todo-keywords][Track TODO state:1]]
(setq org-todo-keywords
      '((sequence
         "STARTED(s)"
         "TODO(t)"  ; next action
         "TOBLOG(b)"
         "WAITING(w@/!)"
         "READY(r)"
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

;; [[file:Sacha.org::#projects][Projects:1]]
(setq org-tags-exclude-from-inheritance '("project" "inboxtarget"))
;; Projects:1 ends here

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

;; [[file:Sacha.org::#projects][Projects:3]]
(with-eval-after-load 'org
  (let ((listvar (if (boundp 'org-speed-commands) 'org-speed-commands
                   'org-speed-commands-user)))
    (add-to-list listvar '("S" call-interactively 'org-sort))))
;; Projects:3 ends here

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

;; [[file:Sacha.org::#enable-filtering-by-effort-estimates][Enable filtering by effort estimates:1]]
(add-to-list 'org-global-properties
             '("Effort_ALL". "0:05 0:15 0:30 1:00 2:00 3:00 4:00"))
;; Enable filtering by effort estimates:1 ends here

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

;; [[file:Sacha.org::#track-time][Track time:2]]
(setq org-log-into-drawer "LOGBOOK")
(setq org-clock-into-drawer 1)
;; Track time:2 ends here

;; [[file:Sacha.org::#habits][Habits:1]]
(setq org-habit-graph-column 80)
(setq org-habit-show-habits-only-for-today nil)
;; Habits:1 ends here

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

;; [[file:Sacha.org::#flexible-scheduling-of-tasks][Flexible scheduling of tasks:1]]
;; Get this from https://raw.github.com/chenfengyuan/elisp/master/next-spec-day.el
(load "~/elisp/next-spec-day.el" t)
;; Flexible scheduling of tasks:1 ends here

;; [[file:Sacha.org::#task-dependencies][Task dependencies:1]]
(setq org-enforce-todo-dependencies t)
(setq org-track-ordered-property-with-tag t)
(setq org-agenda-dim-blocked-tasks t)
;; Task dependencies:1 ends here

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

;; [[file:Sacha.org::#org-mode-tasks-checklists][Checklists:1]]
(with-eval-after-load 'org
	(require 'org-checklist))
;; Checklists:1 ends here

;; [[file:Sacha.org::#structure-templates][Structure templates:1]]
(setq org-structure-template-alist
      '(("a" . "export ascii")
        ("C" . "center")
        ("c" . "comment")
				("d" . "my_details")
        ("e" . "example")
        ("E" . "export")
        ("m" . "export md")
        ("M" . "media-post")
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

;; [[file:Sacha.org::#project_subtasks][Basic configuration:1]]
(defvar my-kid-org-file nil "Defined in secrets")
(setq org-agenda-files
      (delq nil
            (mapcar (lambda (x) (and x (file-exists-p x) x))
                    `("~/sync/orgzly/organizer.org"
											"~/sync/orgzly/ipad.org"
                      "~/sync/orgzly/Inbox.org"
                      "~/sync/orgzly/garden.org"
                      "~/sync/orgzly/decisions.org"
                      "~/sync/orgzly/computer-inbox.org"
                      "~/sync/orgzly/posts.org"
                      "~/sync/orgzly/crafts.org"
                      "~/sync/emacs/Sacha.org"
                      "~/proj/emacsconf/wiki/2025/organizers-notebook/index.org"
                      "~/proj/emacsconf/wiki/organizers-notebook/index.org"
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

;; [[file:Sacha.org::#project_subtasks][Basic configuration:3]]
(bind-key "Y" 'org-agenda-todo-yesterday org-agenda-mode-map)
;; Basic configuration:3 ends here

;; [[file:Sacha.org::#starting-my-weeks-on-saturday][Starting my weeks on Saturday:1]]
(setq org-agenda-start-on-weekday 6)
;; Starting my weeks on Saturday:1 ends here

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
        ("s" tags-todo "stream"
         ((org-agenda-sorting-strategy '(todo-state-up priority-down effort-up))))
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

;; [[file:Sacha.org::#org-mode-org-agenda-shuffling-my-org-mode-unscheduled-tasks][Shuffling my Org Mode unscheduled tasks:1]]
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

;; [[file:Sacha.org::#making-it-easier-to-tag-inbox-items][Making it easier to tag inbox items:1]]
(setq org-complete-tags-always-offer-all-agenda-tags t)
(setq org-use-fast-tag-selection nil)
;; Making it easier to tag inbox items:1 ends here

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

;; [[file:Sacha.org::#projects][Projects:1]]
(defun my-org-show-active-projects ()
  "Show my current projects."
  (interactive)
  (org-tags-view nil "project-inactive-someday"))
;; Projects:1 ends here

;; [[file:Sacha.org::#weekly-review][Weekly review:1]]
(use-package quantified :ensure nil :load-path "~/proj/quantified/lisp" :unless my-phone-p)
(defvar my-weekly-review-line-regexp
  "^  \\([^:]+\\): +\\(Sched[^:]+: +\\)?TODO \\(.*?\\)\\(?:[      ]+\\(:[[:alnum:]_@#%:]+:\\)\\)?[        ]*$"
  "Regular expression matching lines to include.")
(defvar my-weekly-done-line-regexp
  "^  \\([^:]+\\): +.*?\\(?:Clocked\\|Closed\\):.*?\\(TODO\\|DONE\\) \\(.*?\\)\\(?:[       ]+\\(:[[:alnum:]_@#%:]+:\\)\\)?[        ]*$"
  "Regular expression matching lines to include as completed tasks.")

(defun my-quantified-sum (start end cat)
	"Return the number of hours from START to END in CAT."
	(quantified-parse-json
   (quantified-request
    (concat "records.json?start=" (or start "") "&end=" (or end "")
						"&order=newest&display_type=time&split=keep&category=" (url-hexify-string cat))
    (list (cons 'auth_token (quantified-token))) "GET")))

(defun my-quantified-average-weekly (start end category &optional insert)
  "Calculate average hours per week from START to END for CATEGORY."
  (interactive (list (org-read-date nil nil nil "Start: ")
                     (org-read-date nil nil nil "End: ")
                     (my-quantified-read-category)
                     current-prefix-arg))
  (let ((hours
         (/ (* 7.0 (my-quantified-sum start end category))
            (days-between end start))))
    (when (called-interactively-p 'any)
      (if insert
          (insert "%.1f hours" hours)
        (message "%.1f hours" hours)))
    hours))

(defvar my-quantified-categories nil)
(defun my-quantified-read-category ()
	(setq my-quantified-categories
				(or my-quantified-categories
						(quantified-parse-json
						 (quantified-request "/record_categories.json?all=1"
																 (list (cons 'auth_token (quantified-token)))
																 "GET"))))
	(completing-read
	 "Category: "
	 (mapcar (lambda (o)
						 (cons
							(alist-get 'full_name o)
							o))
					 my-quantified-categories)))

(defun my-quantified-sum (start end cat)
	"Return the number of hours from START to END in CAT."
	(interactive (list (org-read-date nil nil nil "Start: ")
										 (org-read-date nil nil nil "End: ")
										 (my-quantified-read-category)))
	(let* ((records
					(quantified-parse-json
					 (quantified-request
						(concat "records.json?start=" (or start "") "&end=" (or end "")
										"&order=newest&display_type=time&filter_string=" (url-hexify-string cat))
						(list (cons 'auth_token (quantified-token))) "GET")))
				 (duration (apply '+ (delq nil (mapcar (lambda (o) (alist-get 'duration o 0)) records))))
				 (hours (/ duration 3600.0)))
    (when (called-interactively-p 'any)
		  (message "%s: %.1f hour(s) in %d entries" cat hours (length records)))
    hours))

(defun my-quantified-get-hours (category time-summary)
  "Return the number of hours based on the time summary."
  (if (stringp category)
      (if (assoc category time-summary) (/ (cdr (assoc category time-summary)) 3600.0) 0)
    (apply '+ (mapcar (lambda (x) (my-quantified-get-hours x time-summary)) category))))

(defun my-extract-tasks-from-agenda (string matchers prefix line-re)
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

(ert-deftest my-extract-tasks-from-agenda ()
  (let (list-a list-b (line-re "\\([^:]+\\):\\( \\)\\(.*\\)"))
    (my-extract-tasks-from-agenda
     "listA: Task 1\nother: Task 2\nlistA: Task 3"
     '(("listA" . list-a)
       ("." . list-b))
     "- [ ] "
     line-re)
    (should (equal list-a '("- [ ] Task 1" "- [ ] Task 3")))
    (should (equal list-b '("- [ ] Task 2")))))

(defun my-get-upcoming-tasks ()
  (save-window-excursion
    (org-agenda nil "W")
    (my-extract-tasks-from-agenda (buffer-string)
                                   '(("routines" . ignore)
                                     ("business" . business-next)
                                     ("people" . relationships-next)
                                     ("tasks" . emacs-next)
                                     ("." . life-next))
                                   "  - [ ] "
                                   my-weekly-review-line-regexp)))
(defun my-get-previous-tasks ()
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
      (my-extract-tasks-from-agenda string
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
    (my-get-upcoming-tasks)
    (my-get-previous-tasks)
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

;; [[file:Sacha.org::#weekly-review][Weekly review:3]]
  (defun my-org-prepare-weekly-review (&optional date skip-urls)
    "Prepare weekly review template."
    (interactive (list (org-read-date nil nil nil "Ending on Sun: " nil "-sun")))
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
			 "\n\n*Toots*\n\n"
			 (my-mastodon-format-my-toots-since start)
       "\n\n#+begin_my_details Time\n"
			 (format "#+begin_src emacs-lisp :results table :exports results
(my-quantified-compare \"%s\" \"%s\" \"%s\" \"%s\" my-quantified-summary-categories \"The other week %%\" \"Last week %%\")
#+end_src

:results:\n"  prev start start end)
       (orgtbl-to-orgtbl
        (my-quantified-compare prev start start end my-quantified-summary-categories "The other week %" "Last week %")
        nil)
			 ":end:\"\""
			 (format "\n#+begin_src emacs-lisp :exports results :results file :file time-graph.svg :output-dir /tmp\n(quantified-svg-to-text (quantified-svg-days \"%s\" \"%s\"))\n#+end_src\n\n" start end)
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

;; [[file:Sacha.org::#flickr-extract][Flickr extract:1]]
(defun my-clean-up-flickr-list (list)
  (setq list
        (replace-regexp-in-string "\\[\"" "[" list))
  (setq list
        (replace-regexp-in-string "<a href=\"\"\\([^\"]+\\).*?>.*?</a>"
                                  "[[\\1][\\2]]" list))
  (setq list
        (replace-regexp-in-string "\"
        " "" (replace-regexp-in-string "\"\\]" "]" list))))

(defun my-format-flickr-link-for-org (x)
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


(defun my-parse-and-filter-flickr-csv-buffer (start end)
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
					(let ((url (match-string 0)))
						(unless (string-match "permalink.gmane.org" url)
							(browse-url url))))))))

(add-to-list 'browse-url-handlers '("https?://yhetil.org/.*/raw" . my-browse-yhetil))
(defun my-browse-yhetil (url &rest _)
  (when (string-match "\\(https?://yhetil.org/.*\\)/raw$" url)
    (funcall browse-url-browser-function (match-string 1 url))))
;; Link-related convenience functions:1 ends here

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

(defvar my-quantified-summary-categories '("Business" "Discretionary - Play" "Unpaid work" "A+" "Discretionary - Family" "Sleep" "Discretionary - Productive" "Personal"))
(defun my-quantified-summarize-time-table-month (month)
	"Insert or return the table summarizing the month's time, compared with the previous month."
	(interactive (list (org-read-date nil t)))
	(let* ((date (decode-time (if (stringp month) (date-to-time month) month)))
				 (month (elt date 4))
         (year (elt date 5))
				 start-date
				 end-date
				 previous-date
				 results)
		(calendar-increment-month month year -1)
		(setq start-date (format "%4d-%02d-01 0:00" year month)
          end-date (format "%4d-%02d-01 0:00" (elt date 5) (elt date 4)))
		(calendar-increment-month month year -1)
		(setq previous-date (format "%4d-%02d-01 0:00" year month))
		(setq results (orgtbl-to-orgtbl (my-quantified-compare previous-date start-date start-date end-date my-quantified-summary-categories "Previous month %" "This month %")
																		nil))
		(when (called-interactively-p 'any)
			(insert results))
		results))

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
				 time-comparison
         org-date)
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
    (setq time-comparison (my-quantified-compare previous-date start-date start-date end-date my-quantified-summary-categories "Previous month %" "This month %"))
    (goto-char (line-end-position))
    (insert
     "\n\n** " title "  :monthly:review:\n"
     "*Blog posts*\n"
     posts "\n\n"
     "*Sketches*\n\n"
     sketches
     (format "*Time*\n\n#+begin_src emacs-lisp :results table :exports results\n(my-quantified-compare \"%s\" \"%s\" \"%s\" \"%s\" my-quantified-summary-categories \"Previous month %%\" \"This month %%\")\n#+end_src\n\n"
						 previous-date start-date start-date end-date)
     (orgtbl-to-orgtbl time-comparison nil)
		 (format "\n#+begin_src emacs-lisp :exports results :results file :file monthly-%s.svg :output-dir /tmp\n(quantified-svg-to-text (quantified-svg-days \"%s\" \"%s\" 'horizontal))\n#+end_src\n\n"
						 start-date
						 start-date end-date))
    (my-org-11ty-prepare-subtree)))

(defun my-org-prepare-yearly-review (year-end)
	(interactive (list (org-read-date nil t nil "Year end (exclusive): ")))
  (let* ((date (decode-time year-end))
         (month (elt date 4))
         (year (elt date 5))
				 (end-date (format-time-string "%Y-%m-%d" year-end))
				 (start-date (progn
											 (setf (elt date 5) (1- (elt date 5)))
											 (format-time-string "%Y-%m-%d" (encode-time date))))
				 (previous-date (progn
													(setf (elt date 5) (1- (elt date 5)))
													(format-time-string "%Y-%m-%d" (encode-time date))))
				 (posts (mapconcat (lambda (o)
														 (concat "- " (org-link-make-string
																					 (concat my-blog-base-url (plist-get o :permalink))
																					 (plist-get o :title))))
													 (my-list-blog-posts
														(substring start-date 0 10)
														(substring end-date 0 10))
													 "\n"))
				 (sketches (my-sketches-export-and-extract
										(substring start-date 0 10) (substring end-date 0 10) nil t))
				 (time (my-quantified-compare
								previous-date start-date start-date end-date my-quantified-summary-categories
								"The other year %"
								"Last year %")))
    (insert
     "*Blog posts*\n\n" posts "\n\n"
     "*Sketches*\n\n" sketches
     "*Time*\n\n"
		 (format "#+begin_src emacs-lisp :results table :exports results
(my-quantified-compare \"%s\" \"%s\" \"%s\" \"%s\" my-quantified-summary-categories \"The other year %%\" \"Last year %%\")
#+end_src

:results:\n"  previous-date start-date start-date end-date)
		 (orgtbl-to-orgtbl time nil)
		 (format "\n#+begin_src emacs-lisp :exports results :results file :file time-graph.svg :output-dir /tmp\n(quantified-svg-to-text (quantified-svg-days \"%s\" \"%s\" 'horizontal))\n#+end_src\n\n" start-date end-date))))
;; Monthly reviews:2 ends here

;; [[file:Sacha.org::#org-mode-reviews-emoji-summaries][Emoji summaries:1]]
(defun my-org-emoji-summary (&optional label)
	(let (results)
		(save-excursion
			(goto-char (org-find-property "EXPORT_ELEVENTY_PERMALINK" (org-entry-get-with-inheritance "EXPORT_ELEVENTY_PERMALINK")))
			(let ((end (save-excursion (org-end-of-subtree))))
				(while (re-search-forward "^\\([0-9]+\\)\\. \\([^A-Za-z0-9]+\\) \\(.+?\\)\\(- weekly highlight\\)?\n" end t)
					(let ((day (match-string 1))
								(icon (match-string 2))
								(text (match-string 3)))

						(push
						 (if (string-match org-link-bracket-re text)
								 (format "<a href=\"%s\" title=\"%s - %s\">%s</a>"
												 (match-string 1 text)
												 (match-string 2 text)
												 day
												 icon)
							 (format "<span title=\"%s - %s\">%s</span>"
											 text
											 day
											 icon))
						 results)))))
		(format "<div class=\"emoji-summary\">%s%s</div>"
						(if label (concat label ": ") "")
						(string-join (nreverse results) ""))))
;; Emoji summaries:1 ends here

;; [[file:Sacha.org::#org-mode-filing-org-mode-prompt-for-a-heading-and-then-refile-it-to-point][Org Mode: Prompt for a heading and then refile it to point:1]]
(defun my-org-refile-to-point (refloc)
	"Prompt for a heading and refile it to point."
	(interactive (list (org-refile-get-location "Heading: ")))
	(let* ((file (nth 1 refloc))
				 (pos (nth 3 refloc)))
		(save-excursion
			(with-current-buffer (find-file-noselect file 'nowarn)
				(save-excursion
					(save-restriction
						(widen)
						(goto-char pos)
						(org-copy-subtree 1 t))))
			(org-paste-subtree nil nil nil t))))
;; Org Mode: Prompt for a heading and then refile it to point:1 ends here

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

;; [[file:Sacha.org::#jump-to-org-location-by-substring][Jump to Org location by substring:1]]
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
;; Jump to Org location by substring:1 ends here

;; [[file:Sacha.org::#quick-way-to-jump][Quick way to jump:1]]
(defun my-org-jump ()
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively 'org-refile)))
;; Quick way to jump:1 ends here

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

;; [[file:Sacha.org::#org-contacts][Contacts:1]]
(use-package org-contacts
	:commands org-contacts-filter
	:config
	(setq org-contacts-files '("~/sync/orgzly/people.org" "~/proj/emacsconf/2025/private/conf.org"))
	:hook
	(message-setup . my-message-greet-contacts))

(defvar my-message-greet-contacts t "Non-nil means say hi.")

(defun my-message-greet-contacts-skip (fn &rest args)
	(let ((my-message-greet-contacts nil))
		(apply fn args)))

(with-eval-after-load 'emacsconf-mail
	(advice-add #'emacsconf-mail-prepare :around #'my-message-greet-contacts-skip))

(defun my-message-greet-contacts ()
	(interactive)
	(when my-message-greet-contacts
		(let* ((emails
						(mapcar 'car
										(append
										 (mail-header-parse-addresses (message-fetch-field "To"))
										 (mail-header-parse-addresses (message-fetch-field "Cc")))))
					 (people
						(seq-keep
						 (lambda (email)
							 (cdr (assoc-string "NAME_SHORT"
																	(caddr (car (org-contacts-filter nil nil (cons "EMAIL" email)))))))
						 emails)))
			(when people
				(message-goto-body)
				(unless (re-search-forward "^Hi, " nil t)
					(insert "Hi, " (string-join people ",") "!\n\n"))))))
;; Contacts:1 ends here

;; [[file:Sacha.org::#inserting-code][Inserting code:2]]
(use-package org
  :hook (org-mode . org-indent-mode)
  :config
  (setq org-indent-indentation-per-level 2)
  (setq org-edit-src-content-indentation 0)
  (setq org-src-preserve-indentation t))
;; Inserting code:2 ends here

;; [[file:Sacha.org::#org-bookmarks][Bookmarks:2]]
(with-eval-after-load
 'embark
 (keymap-set embark-org-link-map "s" #'my-org-bookmark-save-link))

(with-eval-after-load 'org
	(org-link-set-parameters
	 "bookmark"
	 :complete #'my-org-bookmark-complete
	 :insert-description #'my-org-link-insert-description))
;; Bookmarks:2 ends here

;; [[file:Sacha.org::#org-babel][Org Babel:1]]
(setq org-edit-src-auto-save-idle-delay 5)
;; Org Babel:1 ends here

;; [[file:Sacha.org::*Make it easier to split my literate config into files][Make it easier to split my literate config into files:1]]
(defun my-emacs-suggest-file ()
  (let ((elem (org-element-context)))
    (seq-find (lambda (o)
                (save-excursion
                  (goto-char (org-element-begin elem))
                  (re-search-forward
                   (concat "^ *(\\(cl-\\)?defun *"
                           (regexp-quote (file-name-base o)))
                   (org-element-end elem) t)))
              (directory-files "lisp/" t "\\.el"))))

(defun my-emacs-split-into-file (filename)
  "Prepare this block for splitting into FILENAME."
  (interactive (list
                (let* ((default (my-emacs-suggest-file))
                       (relative
                        (and default
                             (file-relative-name
                              default
                              (file-name-directory (buffer-file-name))))))
                  (if default
                      (read-file-name
                       (format "File (%s): " relative)
                       "lisp/"
                       relative)
                    (read-file-name "File: " "lisp/")))))
  (let ((elem (org-element-context)))
    (save-restriction
      (narrow-to-region (org-element-begin elem)
                        (org-element-end elem))
      (goto-char (point-min))
      (forward-line)
      (while (re-search-forward "^ *(\\(cl-\\)?defun " nil t)
        (unless (save-match-data
                  (save-excursion
                    (forward-line -1)
                    (looking-at ";;;###autoload\n")))
          (replace-match
           (concat ";;;###autoload\n"
                   (match-string 0)))))

      (goto-char (point-min))
      (forward-line)
      (when (re-search-forward "^(\\(setq\\|use-package\\|with-eval-after-load\\|bind-key\\|keymap-set\\|keymap-global-set\\)" nil t)
        (goto-char (match-beginning 0))
        (my-org-demarcate-block))
      ;; Add the tangle
      (goto-char (point-min))
      (unless (looking-at "#\\+begin_src")
        (re-search-forward "#\\+begin_src" nil t))
      (unless (save-excursion (re-search-forward ":tangle" nil (line-end-position)))
        (goto-char (line-end-position))
        (insert " :tangle " (file-relative-name filename (file-name-directory (buffer-file-name))))))))
;; Make it easier to split my literate config into files:1 ends here

;; [[file:Sacha.org::org-babel-default-header-args][org-babel-default-header-args]]
(setq org-babel-default-header-args
      '((:session . "none")
        (:results . "drawer replace")
				(:comments . "link")  ;; add a link to the original source
        (:exports . "both")
        (:cache . "no")
        (:eval . "never-export") ;; explicitly evaluate blocks instead of evaluating them during export
        (:hlines . "no")
        (:tangle . "no"))) ;; I have to explicitly set up blocks for tangling
;; org-babel-default-header-args ends here

;; [[file:Sacha.org::#org-babel-comments][Linking to Org Babel source in a comment, and making that always use file links:2]]
(advice-add #'org-babel-tangle--unbracketed-link
						:around (lambda (old-fun &rest args)
											(let (org-link-parameters)
												(apply old-fun args))))
;; Linking to Org Babel source in a comment, and making that always use file links:2 ends here

;; [[file:Sacha.org::#org-mode-org-babel-tangling-my-emacs-config-snippets-to-different-files-and-adding-boilerplate][Tangle Emacs config snippets to different files and add boilerplate:4]]
(setq my-emacs-config-url "https://sachachua.com/dotemacs")
(with-eval-after-load 'org
  (add-hook 'org-babel-pre-tangle-hook #'my-emacs-config-prepare-to-tangle)
  (add-hook 'org-babel-post-tangle-hook #'my-org-babel-post-tangle-insert-boilerplate-for-my-lisp))
;; Tangle Emacs config snippets to different files and add boilerplate:4 ends here

;; [[file:Sacha.org::#format-source][Format source:2]]
(use-package format-all :if my-laptop-p :defer t)
(with-eval-after-load 'org
  (advice-add #'org-edit-src-exit :before #'my-format-all-advice))
;; Format source:2 ends here

;; [[file:Sacha.org::#json][JSON:2]]
(defalias 'org-babel-execute:json #'my-org-babel-execute:json)
;; JSON:2 ends here

;; [[file:Sacha.org::#jq][JQ:1]]
(use-package jq-mode
	:vc (:url "https://github.com/ljos/jq-mode")
	:defer t
	:config
	(org-babel-do-load-languages 'org-babel-load-languages
															 '((jq . t))))
;; JQ:1 ends here

;; [[file:Sacha.org::#let-s-try-literate-elisp][Let's try literate-elisp:1]]
(use-package literate-elisp :if my-laptop-p :defer t)
;; Let's try literate-elisp:1 ends here

;; [[file:Sacha.org::#org-mode-publishing-changing-org-mode-underlines-to-the-html-mark-element][Changing Org Mode underlines to the HTML mark element:1]]
(with-eval-after-load 'ox-html
	(setf (alist-get 'underline org-html-text-markup-alist)
				"<mark>%s</mark>"))
;; Changing Org Mode underlines to the HTML mark element:1 ends here

;; [[file:Sacha.org::#org-mode-publishing-changing-org-mode-underlines-to-the-html-mark-element][Changing Org Mode underlines to the HTML mark element:3]]
(with-eval-after-load 'org
	(org-link-set-parameters "hl" :export 'my-org-highlight-export))
;; Changing Org Mode underlines to the HTML mark element:3 ends here

;; [[file:Sacha.org::#org-mode-publishing-html-export-html-copy-files-and-serve-via-simple-httpd][Org Mode: Export HTML, copy files, and serve the results via simple-httpd so that media files work:1]]
(use-package simple-httpd
  :config
  (setq httpd-root (make-temp-file "httpd" t))
  :hook
  (httpd-stop . my-simple-httpd-remove-temporary-root)
  (kill-emacs . httpd-stop))
;; Org Mode: Export HTML, copy files, and serve the results via simple-httpd so that media files work:1 ends here

;; [[file:Sacha.org::#org-mode-publishing-html-export-html-copy-files-and-serve-via-simple-httpd][Org Mode: Export HTML, copy files, and serve the results via simple-httpd so that media files work:3]]
(with-eval-after-load 'ox
  (org-export-define-derived-backend 'my-html-served 'html
    :menu-entry
    '(?s "Export to HTML and Serve"
         ((?b "Buffer"  my-org-serve-buffer)
          (?s "Subtree" my-org-serve-subtree)))))
;; Org Mode: Export HTML, copy files, and serve the results via simple-httpd so that media files work:3 ends here

;; [[file:Sacha.org::#11ty][11ty static site generation:1]]
(use-package ox-11ty
  :if my-laptop-p
  :load-path "~/proj/ox-11ty"
	:config
	(setq org-html-toplevel-hlevel 3)
	(advice-add 'org-11ty--front-matter :filter-return #'my-org-11ty-rewrite-tags))
;; 11ty static site generation:1 ends here

;; [[file:Sacha.org::#org-mode-publishing-11ty-static-site-generation-linking-to-blog-topics][Linking to blog topics:2]]
(with-eval-after-load 'org
  (org-link-set-parameters
	 "topic"
	 :follow #'my-org-topic-open
	 :store #'my-org-topic-store
	 :insert-description #'my-org-link-insert-description
	 :export #'my-org-topic-export
	 :complete #'my-org-topic-complete))
;; Linking to blog topics:2 ends here

;; [[file:Sacha.org::#linking-to-blog-posts][Linking to blog posts:2]]
(with-eval-after-load 'org
	(org-link-set-parameters
	 "blog"
	 :follow #'my-org-blog-open
	 :store #'my-org-blog-store
	 :insert-description #'my-org-link-insert-description
	 :export #'my-org-blog-export
	 :complete #'my-org-blog-complete))
;; Linking to blog posts:2 ends here

;; [[file:Sacha.org::#org-mode-publishing-11ty-static-site-generation-linking-to-blog-posts-making-it-easier-to-add-a-category-to-a-blog-post][Making it easier to add a category to a blog post:3]]
(with-eval-after-load 'embark
	(add-to-list 'embark-target-finders #'my-embark-org-blog-target)
	(defvar-keymap embark-my-blog-actions
    :parent embark-general-map
		:doc "Shortcuts for my blog"
		"h" #'my-blog-edit-html
		"j" #'my-blog-edit-json
		"e" #'my-blog-find-org
		"c" #'my-embark-org-blog-add-category
    "i" #'my-embark-blog-insert-link
		"b" #'my-embark-org-blog-open-in-browser)
	(add-to-list 'embark-keymap-alist '(my-blog . embark-my-blog-actions)))
;; Making it easier to add a category to a blog post:3 ends here

;; [[file:Sacha.org::#embark-11ty][embark-11ty:2]]
(with-eval-after-load 'embark
	(define-key embark-url-map "v" #'my-blog-find-org)
	(define-key embark-org-link-map "v" #'my-blog-find-org))
;; embark-11ty:2 ends here

;; [[file:Sacha.org::#moving-my-org-post-subtree-to-the-11ty-directory][Moving my Org post subtree to the 11ty directory:3]]
(with-eval-after-load 'ox-11ty
  (add-to-list 'org-11ty-process-export-functions #'my-org-export-filter-body-add-index-link))
;; Moving my Org post subtree to the 11ty directory:3 ends here

;; [[file:Sacha.org::#moving-my-org-post-subtree-to-the-11ty-directory][Moving my Org post subtree to the 11ty directory:5]]
(with-eval-after-load 'ox-11ty
	(map-put (caddr (org-export-backend-menu (org-export-get-backend '11ty)))
					 ?1 (list "To Org, 11tydata.json, HTML" 'my-org-11ty-export)))
;; Moving my Org post subtree to the 11ty directory:5 ends here

;; [[file:Sacha.org::#org-mode-publishing-11ty-static-site-generation-include-mastodon-field-in-front-matter][Include Mastodon, HN, Reddit fields in front matter:2]]
(with-eval-after-load 'ox-11ty
	(pushnew
	 '(:mastodon "MASTODON" nil nil)
	 (org-export-backend-options (org-export-get-backend '11ty)))
	(pushnew
	 '(:hn "HN" nil nil)
	 (org-export-backend-options (org-export-get-backend '11ty)))
	(pushnew
	 '(:reddit "REDDIT" nil nil)
	 (org-export-backend-options (org-export-get-backend '11ty)))
	(add-hook 'org-11ty-front-matter-functions #'my-org-11ty-add-mastodon-to-front-matter))
;; Include Mastodon, HN, Reddit fields in front matter:2 ends here

;; [[file:Sacha.org::org-my-include-link][org-my-include-link]]
(org-link-set-parameters
 "my-include"
 :follow #'my-include-open
 :store #'my-include-store
 :export #'my-include-export
 :complete #'my-include-complete)
;; org-my-include-link ends here

;; [[file:Sacha.org::#ox-epub][ox-epub:1]]
(use-package ox-epub
  :if my-laptop-p
	:defer t
  :config
	(setq org-epub-style-default
        (concat org-epub-style-default "\n  p.my-verse { white-space: pre }\n")))
;; ox-epub:1 ends here

;; [[file:Sacha.org::#config-footer][Add a note to the bottom of blog posts exported from my config file:2]]
(with-eval-after-load 'ox
  (add-to-list 'org-export-filter-body-functions #'my-org-export-filter-body-add-emacs-configuration-link))
;; Add a note to the bottom of blog posts exported from my config file:2 ends here

;; [[file:Sacha.org::#copy-linked-file-and-change-link][Copy linked file and change link:2]]
(with-eval-after-load 'embark-org
	(keymap-set embark-org-link-map "r l" #'my-embark-org-copy-linked-file-and-change-link))
;; Copy linked file and change link:2 ends here

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
         :makeindex t)
				("topics"
				 :base-directory "~/sync/topics"
				 :publishing-directory "/tmp/topics"
				 :publishing-function my-org-11ty-publish-from-project)))
;; org-clean-up-export ends here

;; [[file:Sacha.org::#cleaning-up-export][Cleaning up export:5]]
(bind-key "<apps> b" 'my-org-publish-and-browse)
;; Cleaning up export:5 ends here

;; [[file:Sacha.org::org-special-blocks][org-special-blocks]]
(use-package org-special-block-extras
  :if my-laptop-p
  :hook (org-mode . org-special-block-extras-mode)
	:init (setq org-special-block-add-html-extra nil)
  :config
  ;; Use short names like ‘defblock’ instead of the fully qualified name
  ;; ‘org-special-block-extras--defblock’
	(setcdr org-special-block-extras-mode-map nil)
	(org-defblock my_details (title "Details" title-color "Green" open "")
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
	(defalias 'org-block/details #'org-block/my_details)

  (org-defblock columns nil nil
								"Top level (HTML & wp & 11ty)OSPE-RESPECT-NEWLINES? Split into columns using Foundation."
								(format "<div class=\"row\">%s</div>" contents))
  (org-defblock column50 nil nil
								"Top level (HTML & wp & 11ty)OSPE-RESPECT-NEWLINES? Split into columns."
								(format "<div class=\"columns small-12 medium-6 large-6\">%s</div>" contents))
	(org-defblock short (yt nil video nil audio nil thumbnail nil)
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
											 (my-org-video-export (concat "video:" (expand-file-name video) "?thumbnail=" (or thumbnail ""))
																						nil backend nil)
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
	(org-defblock visual_book_note (title nil post nil image nil)
								"Top level (HTML & 11ty)OSPE-RESPECT-NEWLINES? Mark up a visual book note thumbnail."
								(format
								 "<figure class=\"book\">
<a href=\"%s\">
<div><img src=\"%s\" alt=\"%s\" /></div>
<figcaption>%s</figcaption>
</a>
</figure>"
								 post image title title))
	(org-defblock gallerylist ()
								"Top level (HTML & 11ty)OSPE-RESPECT-NEWLINES? Mark up a visual book note thumbnail."
								(if (eq backend '11ty)
										(concat "{% gallerylist %}\n" contents "\n{% endgallerylist %}\n")
									(concat "<div class=\"gallerylist\">" contents "</div>"))))
;; org-special-blocks ends here

;; [[file:Sacha.org::#adding-a-custom-header-argument-to-org-mode-source-blocks-and-using-that-argument-during-export][Adding a custom header argument to Org Mode source blocks and using that argument during export:2]]
(setq org-babel-exp-code-template "#+begin_src %lang%switches%flags :summary %summary\n%body\n#+end_src")
(with-eval-after-load 'ox-html
	(map-put!
	 (org-export-backend-transcoders (org-export-get-backend 'html))
	 'src-block 'my-org-html-src-block))
(with-eval-after-load 'ox-11ty
	(map-put!
	 (org-export-backend-transcoders (org-export-get-backend '11ty))
	 'src-block 'my-org-11ty-src-block))
;; Adding a custom header argument to Org Mode source blocks and using that argument during export:2 ends here

;; [[file:Sacha.org::org-styles][org-styles]]
(setq org-html-head "
       <link rel=\"stylesheet\" type=\"text/css\" href=\"https://sachachua.com/assets/css/style.css\"></link>
       <link rel=\"stylesheet\" type=\"text/css\" href=\"https://sachachua.com/assets/css/org-export.css\"></link>
       <script src=\"https://ajax.googleapis.com/ajax/libs/jquery/1.11.0/jquery.min.js\"></script>")
(setq org-html-htmlize-output-type 'css)
(setq org-src-fontify-natively t)
;; org-styles ends here

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
       </script>
       <script src=\"https://sachachua.com/assets/js/misc.js\"></script>")
;; Footer:1 ends here

;; [[file:Sacha.org::#utf-8-checkboxes][UTF-8 checkboxes:1]]
(setq org-html-checkbox-type 'unicode)
(setq org-html-checkbox-types
      '((unicode (on . "<span class=\"task-done\">&#x2611;</span>")
                 (off . "<span class=\"task-todo\">&#x2610;</span>")
                 (trans . "<span class=\"task-in-progress\">[-]</span>"))))
;; UTF-8 checkboxes:1 ends here

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

;; [[file:Sacha.org::#plantuml][PlantUML:1]]
     (setq org-plantuml-jar-path (expand-file-name "/usr/share/plantuml/plantuml.jar"))
(add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
;; PlantUML:1 ends here

;; [[file:Sacha.org::#ox-hugo][ox-hugo:1]]
(use-package ox-hugo
  :ensure t            ;Auto-install the package from Melpa (optional)
	:defer t
  :after ox)
;; ox-hugo:1 ends here

;; [[file:Sacha.org::org-async-variables][org-async-variables]]
(setq org-export-async-init-file "~/.config/emacs/org-async-export-config.el")
(setq org-export-async-debug t)
;; org-async-variables ends here

;; [[file:Sacha.org::#org-mode-publishing-plain-text][Plain text:2]]
(with-eval-after-load 'org
  (org-export-define-derived-backend 'my-plain-text 'ascii
    :translate-alist '((link . my-plain-text-link)
                       (item . my-plain-text-item))
    :menu-entry '(?p "Export to custom plain text"
                     ((?p "As plain text buffer" my-plain-text-export-to-buffer)
                      (?P "As plain text file" my-plain-text-export-to-file))))
  (add-to-list 'org-export-backends 'my-plain-text)
  (provide 'ox-my-plain-text))
;; Plain text:2 ends here

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

;; [[file:Sacha.org::#my-org-insert-link-dwim][Adding Org Mode link awesomeness elsewhere: my-org-insert-link-dwim:2]]
(dolist (group '((org . org-mode-map)
								 (markdown-mode . markdown-mode-map)
								 (mastodon-toot . mastodon-toot-mode-map)
								 (web-mode . web-mode-map)
								 (oddmuse-mode . oddmuse-mode-map)
                 (notmuch . notmuch-message-mode-map)
								 (text-mode . text-mode-map)
								 (html-mode . html-mode-map)))
	(with-eval-after-load (car group)
		(keymap-set (symbol-value (cdr group))  "C-c C-l" #'my-org-insert-link-dwim)))
;; Adding Org Mode link awesomeness elsewhere: my-org-insert-link-dwim:2 ends here

;; [[file:Sacha.org::#my-org-insert-link-dwim][Adding Org Mode link awesomeness elsewhere: my-org-insert-link-dwim:5]]
(with-eval-after-load 'org
	(org-link-set-parameters "https" :insert-description #'my-org-link-https-insert-description))
;; Adding Org Mode link awesomeness elsewhere: my-org-insert-link-dwim:5 ends here

;; [[file:Sacha.org::#ids][IDs:1]]
(setq org-id-method 'ts)
(setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
;; IDs:1 ends here

;; [[file:Sacha.org::#quick-links][Quick links:1]]
(setq org-link-abbrev-alist
      '(("google" . "http://www.google.com/search?q=")
        ("gmap" . "http://maps.google.com/maps?q=%s")
        ))
;; Quick links:1 ends here

;; [[file:Sacha.org::#links-to-my-config][Links to my config:2]]
(org-link-set-parameters
 "dotemacs"
 :complete #'my-org-dotemacs-complete
 :store #'my-org-dotemacs-store
 :insert-description #'my-org-dotemacs-insert-description
 :export #'my-org-dotemacs-export
 :follow #'my-org-dotemacs-open)
;; Links to my config:2 ends here

;; [[file:Sacha.org::org-config-link][org-config-link]]
(use-package org
  :config
  (org-link-set-parameters
   "config"
   :follow (lambda (id) (org-open-link-from-string (format "[[~/sync/emacs/Sacha.org::%s]]" id)))
   :export (lambda (link description format)
             (format "<a href=\"https://sachachua.com/dotemacs#%s\">%s</a>" link description))))
;; org-config-link ends here

;; [[file:Sacha.org::#youtube][YouTube:3]]
(org-link-set-parameters "yt" :complete #'my-org-yt-complete
												 :insert-description #'my-org-yt-insert-description
												 :export #'my-org-yt-export
												 :follow #'my-org-yt-open)
;; YouTube:3 ends here

;; [[file:Sacha.org::#videos][Videos:1]]
(org-link-set-parameters
 "video"
 :export #'my-org-video-export
 :follow #'my-org-video-follow
 :complete #'my-org-video-complete)
;; Videos:1 ends here

;; [[file:Sacha.org::#org-mode-links-linking-to-a-specific-time-in-a-video][Linking to a specific time in a video:1]]
(org-link-set-parameters
 "vtime"
 :export #'my-org-video-time-export
 :complete #'my-org-video-time-complete
 :follow #'my-org-video-time-follow)
;; Linking to a specific time in a video:1 ends here

;; [[file:Sacha.org::#org-mode-links-linking-to-a-specific-time-in-a-video][Linking to a specific time in a video:4]]
(with-eval-after-load 'org
  (advice-add 'org-insert-item :around 'my-org-vtime-insert-item-advice))
;; Linking to a specific time in a video:4 ends here

;; [[file:Sacha.org::org-audio-link][org-audio-link]]
(org-link-set-parameters
 "audio"
 :export #'my-org-audio-export
 :follow #'my-org-video-follow
 :complete #'my-org-audio-complete)

(org-link-set-parameters
 "audioi"
 :export #'my-org-audio-export
 :follow #'my-org-video-follow
 :complete #'my-org-audio-icon-complete)
;; org-audio-link ends here

;; [[file:Sacha.org::#git-projects][Using an Emacs Lisp macro to define quick custom Org Mode links to project files; plus URLs and search:2]]
(with-eval-after-load 'org
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
  (my-org-project-link "11ty"
										   "~/proj/static-blog/"
										   "https://github.com/sachac/eleventy-blog-setup/blob/master/")
  (my-org-project-link "emacstv"
										   "~/proj/emacstv.github.io/"
										   "https://github.com/emacstv/emacstv.github.io/blob/master/")
  (my-org-project-link "quantified"
										   "~/proj/quantified/"
										   "https://github.com/sachac/quantified/blob/master/")
  (my-org-project-link "emacs-news"
										   "~/sync/emacs-news/"
										   "https://github.com/sachac/emacs-news/blob/master/")
  (my-org-project-link "speech-input"
										   "~/proj/speech-input/"
										   "https://codeberg.org/sachac/speech-input/src/branch/main/")
  (my-org-project-link "learn-lang"
										   "~/proj/learn-lang/"
										   "https://codeberg.org/sachac/learn-lang/src/branch/main/"))
;; Using an Emacs Lisp macro to define quick custom Org Mode links to project files; plus URLs and search:2 ends here

;; [[file:Sacha.org::#git-projects][Using an Emacs Lisp macro to define quick custom Org Mode links to project files; plus URLs and search:3]]
(cl-pushnew (cons (expand-file-name "~/sync/sketches/") "https://sketches.sachachua.com/filename/")
						my-project-web-base-list
						:test 'equal)
;; Using an Emacs Lisp macro to define quick custom Org Mode links to project files; plus URLs and search:3 ends here

;; [[file:Sacha.org::#org-mode-links-using-an-emacs-lisp-macro-to-define-quick-custom-org-mode-links-to-project-files-plus-urls-and-search-quickly-search-my-code][Quickly search my code:2]]
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

;; [[file:Sacha.org::#org-mode-links-using-an-emacs-lisp-macro-to-define-quick-custom-org-mode-links-to-project-files-plus-urls-and-search-quickly-search-my-code][Quickly search my code:3]]
(cl-pushnew (cons (expand-file-name "~/proj/static-blog/blog/") "https://sachachua.com/blog/")
						my-project-web-base-list
						:test 'equal)
(cl-pushnew (cons (expand-file-name "~/sync/orgzly") nil)
						my-project-web-base-list
						:test 'equal)
;; Quickly search my code:3 ends here

;; [[file:Sacha.org::#org-mode-links-using-an-emacs-lisp-macro-to-define-quick-custom-org-mode-links-to-project-files-plus-urls-and-search-quickly-search-my-code][Quickly search my code:4]]
(keymap-global-set "M-s c" #'my-consult-ripgrep-code)
;; Quickly search my code:4 ends here

;; [[file:Sacha.org::#org-mode-links-using-an-emacs-lisp-macro-to-define-quick-custom-org-mode-links-to-project-files-plus-urls-and-search-quickly-search-my-code-tip-from-omar-embark-around-action-hooks][Tip from Omar: embark-around-action-hooks:2]]
(cl-pushnew #'embark-consult--at-location (alist-get 'org-store-link embark-around-action-hooks))
;; Tip from Omar: embark-around-action-hooks:2 ends here

;; [[file:Sacha.org::#links-from-org-protocol][Links from org-protocol:3]]
(use-package org-protocol-capture-html
	:vc (:url "https://github.com/alphapapa/org-protocol-capture-html"))
;; Links from org-protocol:3 ends here

;; [[file:Sacha.org::#fix-elisp-links][Fix elisp links:2]]
(org-link-set-parameters
 "elisp"
 :export 'my-org-elisp-link-export)
;; Fix elisp links:2 ends here

;; [[file:Sacha.org::org-irc-link][org-irc-link]]
(org-link-set-parameters
 "ircs"
 :export #'my-org-irc-export)
;; org-irc-link ends here

;; [[file:Sacha.org::#irc][IRC:2]]
;;;###autoload
(defun my-org-ircs-export (link description format)
   "Export an ircs link.
See `org-link-parameters' for details about LINK, DESCRIPTION and
FORMAT."
   (let ((desc (or description link)))
     (pcase format
       (`html (format "<a href=\"ircs:%s\">%s</a>" link desc))
       (`md (format "[%s](ircs:%s)" desc link))
       (_ nil))))
;; IRC:2 ends here

;; [[file:Sacha.org::#org-dired][Dired:1]]
(setq dired-dwim-target t)
;; Dired:1 ends here

;; [[file:Sacha.org::#org-protocol-open][Org protocol: following Org links from outside Emacs:2]]
(with-eval-after-load 'org-protocol
	(add-to-list 'org-protocol-protocol-alist
							 '("org-open" :protocol "open" :function org-protocol-open-link)))
;; Org protocol: following Org links from outside Emacs:2 ends here

;; [[file:Sacha.org::#add-custom-id][Speed command for adding a custom ID to Org Mode posts:2]]
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
;; Speed command for adding a custom ID to Org Mode posts:2 ends here

;; [[file:Sacha.org::#journal][Journal:5]]
(with-eval-after-load 'org
  (org-link-set-parameters
   "journal"
   :follow 'my-org-journal-open
   :export 'my-org-journal-export
   :complete 'my-org-journal-complete))
;; Journal:5 ends here

;; [[file:Sacha.org::#journal][Journal:8]]
(use-package csv
  :commands csv--read-line)
;; Journal:8 ends here

;; [[file:Sacha.org::#attachments][Attachments:1]]
(use-package org-attach
  :ensure nil
  :config
  (setq org-attach-store-link-p 'attached)
  (setq org-attach-auto-tag nil))
;; Attachments:1 ends here

;; [[file:Sacha.org::#http][HTTP:1]]
(use-package ob-http :defer t)
;; HTTP:1 ends here

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

;; [[file:Sacha.org::#diagrams-and-graphics][Diagrams and graphics:1]]
;also includes Org Babel support
(use-package pikchr-mode
	:defer t
	:config
	(setq pikchr-executable "/home/sacha/vendor/pikchr/pikchr"))
;; Diagrams and graphics:1 ends here

;; [[file:Sacha.org::#diagrams-and-graphics][Diagrams and graphics:2]]
(setq org-ditaa-jar-path "c:/sacha/Dropbox/bin/ditaa.jar")
(use-package org-contrib)
(use-package org
  :config
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
  (setq org-confirm-babel-evaluate (lambda (lang body)
																		 (pcase (or (buffer-file-name) "")
																			 ((rx (or "vendor" "emacstv")) t)
																			 ((rx (or "proj" "orgzly" "sync")) nil)
																			 (_ t))))
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
     (forth . t)
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

;; [[file:Sacha.org::#mermaid][Org Babel, Mermaid JS, and fixing "Failed to launch the browser process" on Ubuntu 24:1]]
(use-package ob-mermaid
	:load-path "~/vendor/ob-mermaid")
;; I need to override this so that the executable isn't quoted
(setq ob-mermaid-cli-path "aa-exec --profile chrome mmdc -c ~/.config/mermaid/config.json")
;; Org Babel, Mermaid JS, and fixing "Failed to launch the browser process" on Ubuntu 24:1 ends here

;; [[file:Sacha.org::#editing-source-code][Editing source code:1]]
(setq org-src-window-setup 'current-window)
;; Editing source code:1 ends here

;; [[file:Sacha.org::#invoices][Invoices:1]]
(setq calendar-week-start-day 6) ;; My weeks start on Saturday
;; Invoices:1 ends here

;; [[file:Sacha.org::#presentations][Presentations:1]]
(use-package org-re-reveal
	:config
	(setq org-re-reveal-revealjs-version "4")
	(setq org-re-reveal-history t)
	:defer t)
(use-package oer-reveal
	:defer t
	:config
	(setq oer-reveal-plugin-4-config
				"audioslideshow RevealAudioSlideshow plugin/audio-slideshow/plugin.js
anything RevealAnything https://cdn.jsdelivr.net/npm/reveal.js-plugins@latest/anything/plugin.js"))
;; Presentations:1 ends here

;; [[file:Sacha.org::#allow-dashes-in-tags][Allow dashes in tags:2]]
(use-package org :hook (org-mode . my-org-add-dashes-to-tag-regexps))
;; Allow dashes in tags:2 ends here

;; [[file:Sacha.org::#ascii-export][ASCII export:1]]
(setq org-ascii-links-to-notes nil)
;; ASCII export:1 ends here

;; [[file:Sacha.org::#reddit][Reddit:2]]
(use-package reddigg :vc (:url "https://github.com/thanhvg/emacs-reddigg") :commands reddigg)
;; Reddit:2 ends here

;; [[file:Sacha.org::#package-links][Package links:2]]
(with-eval-after-load 'org
  (org-link-set-parameters
   "package"
   :follow 'my-org-package-open :export 'my-org-package-export :complete 'my-org-package-complete
   :insert-description #'my-org-package-link-description))
;; Package links:2 ends here

;; [[file:Sacha.org::#save-when-emacs-loses-focus][Save when Emacs loses focus:2]]
(use-package org
  :config
  (add-function :after after-focus-change-function 'my-org-save-all-org-buffers))
;; Save when Emacs loses focus:2 ends here

;; [[file:Sacha.org::#setting-properties][Setting properties:2]]
(use-package org
  :bind (:map org-mode-map
              ("C-c C-x p" . my-org-set-property)))
;; Setting properties:2 ends here

;; [[file:Sacha.org::#org-mode-linking-to-and-exporting-function-definitions-in-org-mode-still-allow-linking-to-the-file][Still allow linking to the file:2]]
(with-eval-after-load 'org
	(org-link-set-parameters "_file" :store #'my-org-defun-store-file-link))
;; Still allow linking to the file:2 ends here

;; [[file:Sacha.org::#org-mode-sorting-completion-candidates-such-as-sorting-org-headings-by-level][Sorting completion candidates, such as sorting Org headings by level:1]]
(with-eval-after-load 'consult-org
  (advice-add
   #'consult-org--headings
   :filter-return
   (lambda (candidates)
     (sort candidates
           :key (lambda (o) (car (get-text-property 0 'consult-org--heading o)))))))
;; Sorting completion candidates, such as sorting Org headings by level:1 ends here

;; [[file:Sacha.org::#org-mode-sorting-completion-candidates-such-as-sorting-org-headings-by-level][Sorting completion candidates, such as sorting Org headings by level:3]]
(with-eval-after-load 'org
  (advice-add
   'org-refile-get-location
   :around
   (lambda (fn &rest args)
     (let ((completion-extra-properties
            '(:display-sort-function
              (lambda (candidates)
                (sort candidates
                      :key (lambda (s) (length (split-string s "/"))))))))
       (apply fn args)))))
;; Sorting completion candidates, such as sorting Org headings by level:3 ends here

;; [[file:Sacha.org::org-db-v3][org-db-v3]]
(use-package org-db-v3
  :load-path "~/vendor/org-db-v3/elisp"
  :init
  (setq org-db-v3-auto-enable nil))
;; org-db-v3 ends here

;; [[file:Sacha.org::#org-mode-vector-search-consult-based-interface-for-searching-blog-posts][Consult-based interface for searching blog posts:2]]
(with-eval-after-load 'embark
  (add-to-list 'embark-target-injection-hooks '(my-blog-similar-link my-embark-blog--inject-target-url)))
;; Consult-based interface for searching blog posts:2 ends here

;; [[file:Sacha.org::#org-mode-vector-search-consult-based-interface-for-searching-blog-posts-multiple-sources][Multiple sources:1]]
(with-eval-after-load 'consult
  (defvar my-consult-source-similar-blog-posts
    (list :name "Blog posts"
          :narrow ?b
          :category 'my-blog
          :state #'my-blog-post--state
          :async (consult--dynamic-collection
                     (lambda (input)
                       (seq-take
                        (my-org-db-v3-blog-post--collection input)
                        5)))
          :action #'my-embark-blog-insert-link))
;; Multiple sources:1 ends here

;; [[file:Sacha.org::emacs-rag-search][emacs-rag-search]]
(use-package emacs-rag
  :load-path "~/vendor/emacs-rag-libsql/emacs-rag"
  :commands (emacs-rag-menu emacs-rag--request)
  :config
  (setq emacs-rag-server-working-directory "~/vendor/emacs-rag-libsql/emacs-rag-server")
  (setq emacs-rag-server-command '("~/.local/bin/uv" "run" "emacs-rag-server" "serve"))
  (setq emacs-rag-indexed-extensions '("org" "txt" "md"))
  (setq emacs-rag-auto-index-on-save nil))
;; emacs-rag-search ends here

;; [[file:Sacha.org::#multimedia][Multimedia:1]]
(setq visible-bell t)
(use-package epwgraph :load-path "~/proj/epwgraph")
;; Multimedia:1 ends here

;; [[file:Sacha.org::#multimedia-emacs-tv][Emacs.tv:1]]
(use-package emacstv
	:load-path "~/proj/emacstv.github.io")
;; Emacs.tv:1 ends here

;; [[file:Sacha.org::#imagemagick][Imagemagick:1]]
(setq image-use-external-converter t)
;; Imagemagick:1 ends here

;; [[file:Sacha.org::#my-image-write-region][Emacs: Extract part of an image to another file:3]]
(with-eval-after-load 'image
	(keymap-set image-map "i w" #'my-image-write-region))
;; Emacs: Extract part of an image to another file:3 ends here

;; [[file:Sacha.org::#svg][SVG:1]]
(auto-image-file-mode -1)
;; SVG:1 ends here

;; [[file:Sacha.org::#multimedia-images-svg-animating-svgs-breaking-up-a-pdf-from-supernote][Breaking up a PDF from Supernote:2]]
(defun my-sketch-regroup (dom groups)
	"Move matching paths to their own group.
GROUPS is specified as ((id . (lambda (elem) ..)))."
	(dolist (group groups)
		(when-let* ((matches (dom-search dom
																		 (lambda (elem)
																			 (funcall (cdr group) elem))))
								(node (dom-node 'g `((id . ,(car group))))))
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
					(dom-add-child-before
					 parent
					 (dom-node 'path `((style . ,(or (dom-attr path 'style) ""))
														 (fill . ,(or (dom-attr path 'fill) ""))
														 (d . ,(concat "M " part))))
					 path))
				(dom-remove-node dom path))))
	dom)

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

	(let (dom
				(new-file (expand-file-name (concat (file-name-sans-extension pdf-file) "-split.svg"))))
		(my-sketch-svg-prepare
		 file :color-map color-map :color-scheme color-scheme :new-file new-file)
		(setq dom (xml-parse-file new-file))
		(when rotate (setq dom (my-sketch-rotate dom)))
		(setq dom (my-sketch-break-apart dom (or selector
																						 (dom-by-tag dom 'path))))
		(with-temp-file new-file
			(svg-print (car dom)))
		new-file))
;; Breaking up a PDF from Supernote:2 ends here

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

;; [[file:Sacha.org::#org-mode-sketch-links][Org Mode sketch: links:2]]
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
  (org-link-set-parameters
   "image"
   :follow 'my-org-image-open
   :export 'my-org-image-export
   :complete 'my-org-image-complete))
;; Org Mode sketch: links:2 ends here

;; [[file:Sacha.org::#org-mode-copy][Org Mode custom link: copy to clipboard:2]]
(use-package org
  :config
  (org-link-set-parameters
   "copy"
   :follow (lambda (link) (kill-new link))
	 :export #'my-org-copy-export))
;; Org Mode custom link: copy to clipboard:2 ends here

;; [[file:Sacha.org::#org-mode-copy][Org Mode custom link: copy to clipboard:3]]
(let* ((box-10 1234) ; fake number for demo
       (box-11 (* 1.15 box-10))
       (box-12 (* 0.090301 box-11)))
  `((box-10 ,(format "[[copy:%.2f][%.2f]]" box-10 box-10))
    (box-11 ,(format "[[copy:%.2f][%.2f]]" box-11 box-11))
    (box-12 ,(format "[[copy:%.2f][%.2f]]" box-12 box-12))))
;; Org Mode custom link: copy to clipboard:3 ends here

;; [[file:Sacha.org::#button-based-interface][Button-based interface:2]]
(setq my-sketch-executable "krita"
      my-sketch-inbox-directory "~/Dropbox/Inbox"
      my-index-card-template-file "~/Dropbox/drawings/templates/0 - index.psd"
      my-sketch-large-template-file "/home/sacha/Dropbox/drawings/templates/0 - base.psd")
;; Button-based interface:2 ends here

;; [[file:Sacha.org::#rename-scanned-index-cards][Rename scanned index cards:1]]
(use-package s)
;; Rename scanned index cards:1 ends here

;; [[file:Sacha.org::#automatically-resize-images][Automatically resize images:1]]
(use-package image+
  :if my-laptop-p
  ;;    :load-path "~/elisp/Emacs-imagex"
  :commands (imagex-global-sticky-mode imagex-auto-adjust-mode)
  :init (progn (imagex-global-sticky-mode) (imagex-auto-adjust-mode)))
;; Automatically resize images:1 ends here

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

;; [[file:Sacha.org::#insert-point][Sketched books:1]]
(setq yas-indent-line 'fixed)
;; Sketched books:1 ends here

;; [[file:Sacha.org::#other-sketch-related-functions][Other sketch-related functions:2]]
(with-eval-after-load 'org
  (let ((listvar (if (boundp 'org-speed-commands) 'org-speed-commands
                   'org-speed-commands-user)))
    (add-to-list listvar '("d" call-interactively 'my-prepare-index-card-for-subtree))))
;; Other sketch-related functions:2 ends here

;; [[file:Sacha.org::#multimedia-images-doodles][Doodles:1]]
(defun my-org-copy-as-doodle ()
	(interactive)
	(cond
	 ((derived-mode-p 'dired-mode)
		(kill-new
		 (mapconcat
			(lambda (s)
				(format
				 "#+begin_center-doodle\n#+ATTR_HTML: :style max-height:100px :alt \n[[file:%s]]\n#+end_center-doodle"
				 s))
			(dired-get-marked-files) "\n\n")))
	 ((derived-mode-p 'image-mode)
		(kill-new
		 (format
				 "#+begin_center-doodle\n#+ATTR_HTML: :style max-height:100px :alt \n%s\n#+end_center-doodle"
				 (org-link-make-string (concat "file:" (buffer-file-name))))))
	))
;; Doodles:1 ends here

;; [[file:Sacha.org::#supernote][Supernote:7]]
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
;; Supernote:7 ends here

;; [[file:Sacha.org::#supernote][Supernote:9]]
(setq htmlize-css-name-prefix "org-")
(setq htmlize-head-tags "<link rel=\"stylesheet\" href=\"https://sachachua.com/assets/css/style.css\" />")
;; Supernote:9 ends here

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

;; [[file:Sacha.org::#remove-whisperx-underline][Remove underlining from WhisperX VTT:1]]
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

;; [[file:Sacha.org::#other-subtitle-code][Other subtitle code:2]]
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
;; Other subtitle code:2 ends here

;; [[file:Sacha.org::#other-subtitle-code][Other subtitle code:3]]
(use-package subed
  :if my-laptop-p
  ;; :quelpa (subed :fetcher github :repo "rndusr/subed" :files (:defaults "subed/*.el"))
  :preface (load "~/proj/subed/subed-autoloads.el" nil t)
  :load-path "~/proj/subed/subed"
  :config
  (setq subed-subtitle-spacing 1)
  (setq subed-align-mfa-conda-env "/home/sacha/vendor/miniconda3/envs/aligner")
  (key-chord-define subed-mode-map "hu" 'my-subed/body)
  (key-chord-define subed-mode-map "ht" 'my-subed/body)
	(setq subed-loop-seconds-before 0 subed-loop-seconds-after 0)
  (setq subed-align-mfa-command '("mfa" "align"))
  (setq subed-align-mfa-conda-env "/home/sacha/vendor/miniconda3/envs/aligner")
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
	(setq subed-record-ffmpeg-args (split-string "-y -f pulse -i VirtualMicSink.monitor -r 48000"))
  :bind
  (:map subed-mode-map ("C-c C-c" . subed-record-compile-video)))
;; Other subtitle code:3 ends here

;; [[file:Sacha.org::#word-level][Using word-level timing information when editing subtitles or captions in Emacs:4]]
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
	(add-hook 'subed-mode-hook 'my-subed-maybe-save-place)
	;; Some reasonable defaults
	;; Replay subtitles as you adjust their start or stop time with M-[, M-], M-{, or M-}
	(add-hook 'subed-mode-hook 'subed-enable-replay-adjusted-subtitle)
	;; Loop over subtitles
	(add-hook 'subed-mode-hook 'subed-enable-loop-over-current-subtitle)
	;; Show characters per second
	(add-hook 'subed-mode-hook 'subed-enable-show-cps)
	(with-eval-after-load 'consult
		(advice-add 'consult-buffer :around
								(lambda (f &rest r)
									(let ((subed-auto-play-media nil))
										(apply f r)))))

	)
;; Using word-level timing information when editing subtitles or captions in Emacs:4 ends here

;; [[file:Sacha.org::#working-with-media][Working with media:1]]
(use-package waveform :load-path "~/proj/waveform-el" :if my-laptop-p :defer t)
(use-package compile-media :load-path "~/proj/compile-media" :if my-laptop-p :defer t
	:autoload compile-media-timestamp-to-msecs
	)
;; Working with media:1 ends here

;; [[file:Sacha.org::#split-up-oops-better][Split up oops better:2]]
(setq subed-align-options "task_adjust_boundary_offset_percent=0.5")
;; Split up oops better:2 ends here

;; [[file:Sacha.org::#multimedia-subtitles-with-subed-using-scripts-to-correct-transcripts][Using scripts to correct transcripts:1]]
;;  (my-combine-script-and-transcript '("I have a script" "that's broken up" "into phrases.") (split-string "I have, oops, I have a script oops. I have a script that's broken up in to faces." " ") "\\<oops\\>")
;;  (my-combine-script-and-transcript '("I already talk quickly," "so I'm not going to speed that up" "into phrases.") (split-string "I already talk pretty quickly. Oops. I already talk quickly, so I'm not going to speed that up, but I can trim the pauses in between phrases,"))
;; (subed-word-data-find-approximate-match "I already talk quickly" (split-string "I already talk pretty quickly oops I already talk quickly" " "))
;; Using scripts to correct transcripts:1 ends here

;; [[file:Sacha.org::#youtube-shorts][Preparing to record YouTube shorts:2]]
(defun my-prepare-for-landscape ()
	(let ((width 6) (height 9))
		(setq compile-media-output-video-width 1080
					compile-media-output-video-height 1920
					compile-media-output-video-fps 30)
	(shell-command "wmctrl -r :ACTIVE: -e 0,300,0,554,984")
	))
;; Preparing to record YouTube shorts:2 ends here

;; [[file:Sacha.org::#multimedia-elfeed][Elfeed:1]]
(use-package elfeed :defer t)
(use-package elfeed-protocol
	:after elfeed
	:defer t
	:custom
	(elfeed-use-curl nil)
	(elfeed-curl-extra-arguments '("--insecure"))
	(elfeed-protocol-enabled-protocols '(fever newsblur owncloud ttrss))
	(elfeed-protocol-fever-update-unread-only nil)
	(elfeed-protocol-fever-fetch-category-as-tag t)
	(elfeed-protocol-fever-maxsize 5)
	(elfeed-protocol-log-trace t)
	(elfeed-log-level 'debug)
	:config
	(elfeed-protocol-enable))
;; Elfeed:1 ends here

;; [[file:Sacha.org::#multimedia-elfeed][Elfeed:2]]
(use-package elfeed-tube
	:defer t
  :quelpa (elfeed-tube :fetcher github :repo "karthink/elfeed-tube")
  :after elfeed
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
;; Elfeed:2 ends here

;; [[file:Sacha.org::#multimedia-elfeed][Elfeed:3]]
(use-package emms
	:defer t
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
;; Elfeed:3 ends here

;; [[file:Sacha.org::#coding][Coding:1]]
(editorconfig-mode 1)
(add-to-list 'exec-path "~/.local/bin")
;; Coding:1 ends here

;; [[file:Sacha.org::#scan-bin-and-turn-the-scripts-into-interactive-commands][Scan ~/bin and turn the scripts into interactive commands:2]]
(use-package dash
  (my-convert-shell-scripts-to-interactive-commands "~/bin"))
;; Scan ~/bin and turn the scripts into interactive commands:2 ends here

;; [[file:Sacha.org::#csvs][CSVs:1]]
(use-package pcsv :defer t)
;; CSVs:1 ends here

;; [[file:Sacha.org::#whitespace][Whitespace:1]]
(use-package ws-butler
	:config (ws-butler-global-mode))
;; Whitespace:1 ends here

;; [[file:Sacha.org::#python][Python:1]]
(use-package elpy
	:defer t
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
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
;; Python:1 ends here

;; [[file:Sacha.org::#web-development][Web development:1]]
(use-package tide :defer t)
(use-package css-eldoc :defer t)
;; Web development:1 ends here

;; [[file:Sacha.org::#web-development][Web development:6]]
;; Avoid lockfiles because they mess up React projects
(when my-laptop-p
  (setq create-lockfiles nil))


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
  ("C-c RET" . themkat/complete-web-mode)
	("C-c C-r" . my-copy-and-append))
;; Web development:6 ends here

;; [[file:Sacha.org::#lsp][LSP:2]]
(use-package lsp-mode
  :if my-laptop-p
  :config
  (setq lsp-headerline-breadcrumb-enable t
        gc-cons-threshold (* 100 1024 1024)
        read-process-output-max (* 1024 1024)
        company-idle-delay 0.5
        company-minimum-prefix-length 1
        create-lockfiles nil ;; lock files will kill `npm start'
				lsp-enable-file-watchers nil
				lsp-auto-register-remote-clients nil
        )
  (lsp-register-custom-settings
   '(("pyls.plugins.pyls_mypy.enabled" t t)
     ("pyls.plugins.pyls_mypy.live_mode" nil t)
     ("pyls.plugins.pyls_black.enabled" t t)
     ("pyls.plugins.pyls_isort.enabled" t t)))
	(add-to-list 'lsp-file-watch-ignored-directories "/blog\\'")
	(add-to-list 'lsp-file-watch-ignored-directories "/_site\\'")
	(add-to-list 'lsp-file-watch-ignored-directories "/_local\\'")
  :hook ((js-mode . my-local-lsp)
         (python-mode . my-local-lsp)
         (lsp-mode-hook . lsp-enable-which-key-integration)))
(use-package lsp-ui
  :if my-laptop-p
  :commands lsp-ui-mode
  :after lsp-mode)
(use-package dap-mode
  :if my-laptop-p
  :after lsp-mode)
;; LSP:2 ends here

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

;; [[file:Sacha.org::#tab-width-of-2-is-compact-and-readable][Tab width of 2 is compact and readable:1]]
(setq-default tab-width 2)
;; Tab width of 2 is compact and readable:1 ends here

;; [[file:Sacha.org::#more-indentation-things][More indentation things:1]]
(defun sanityinc/kill-back-to-indentation ()
  "Kill from point back to the first non-whitespace character on the line."
  (interactive)
  (let ((prev-pos (point)))
    (back-to-indentation)
    (kill-region (point) prev-pos)))
(bind-key "C-M-<backspace>" 'sanityinc/kill-back-to-indentation)
;; More indentation things:1 ends here

;; [[file:Sacha.org::#yaml][YAML:1]]
(use-package yaml-mode
  :if my-laptop-p
  :mode "\\.yml\\'")
;; YAML:1 ends here

;; [[file:Sacha.org::#expreg][Expand region with expreg:1]]
(use-package expreg
  :defer t
  :bind
	("C-=" . expreg-expand)
	("C-+" . expreg-contract)
  ("C-<prior>" . expreg-expand)
  ("C-<next>" . expreg-contract))
;; Expand region with expreg:1 ends here

;; [[file:Sacha.org::#compilation][Compilation:1]]
(eval-after-load 'python-mode
  '(bind-key "C-c C-c" 'compile python-mode-map))
;; Compilation:1 ends here

;; [[file:Sacha.org::#emacs-lisp][Emacs Lisp:1]]
  (use-package auto-compile
    :if my-laptop-p
    :config (auto-compile-on-load-mode)
		:defer t)
  (setq native-comp-async-report-warnings-errors nil)
;; Emacs Lisp:1 ends here

;; [[file:Sacha.org::#emacs-lisp][Emacs Lisp:2]]
(setq eval-expression-print-length nil)
(setq print-length nil)
(setq edebug-print-length nil)
(defun my-set-sentence-end-double-space ()
	(setq-local sentence-end-double-space t))
(add-hook 'emacs-lisp-mode-hook
					'my-set-sentence-end-double-space)
;; Emacs Lisp:2 ends here

;; [[file:Sacha.org::#emacs-lisp][Emacs Lisp:3]]
(use-package let-completion :vc (:url "https://github.com/gggion/let-completion.el")
  :hook (emacs-lisp-mode . let-completion-mode))
;; Emacs Lisp:3 ends here

;; [[file:Sacha.org::#lispy][Lispy:1]]
(use-package lispy :hook (emacs-lisp-mode . lispy-mode))
;; Lispy:1 ends here

;; [[file:Sacha.org::#lispy][Lispy:2]]
(with-eval-after-load 'lispy
  (advice-add
   'lispy-tab
   :around
   (lambda (fn &rest args)
     (let ((print-length nil)
           (print-level nil))
       (apply fn args)))))
;; Lispy:2 ends here

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

;; [[file:Sacha.org::#smartparens-mode][Smartparens mode:1]]
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
;; Smartparens mode:1 ends here

;; [[file:Sacha.org::#edit-list][Edit list:1]]
(use-package edit-list
	:commands edit-list
	:config
	(with-eval-after-load 'embark
	  (define-key embark-variable-map "l" 'edit-list)))
;; Edit list:1 ends here

;; [[file:Sacha.org::#libraries][General-purpose Emacs Lisp libraries:1]]
(use-package dash :ensure t)
(use-package s :ensure t)
;; General-purpose Emacs Lisp libraries:1 ends here

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

;; Emacs Lisp Static Analyzer
(use-package elsa
  :defer t
  :ensure t)
;; Let's try this setup:1 ends here

;; [[file:Sacha.org::#edebug][Edebug:2]]
(advice-add #'edebug-previous-result
            :around
            #'adviced:edebug-previous-result)
(advice-add #'edebug-compute-previous-result
            :around
            #'adviced:edebug-compute-previous-result)
;; Edebug:2 ends here

;; [[file:Sacha.org::#testing][Testing:1]]
(use-package buttercup
	:hook '(buttercup-minor-mode . my-buttercup-set-up-imenu))

(use-package package-lint :defer t)
;; Testing:1 ends here

;; [[file:Sacha.org::#ert][ERT:2]]
(use-package ert
	:defer t
	:commands ert
	:config
	;; handle truncated lists
	(advice-add 'ert--pp-with-indentation-and-newline
							:around (lambda (oldfunc &rest args) (condition-case nil (apply oldfunc args) (error nil))))
	:bind
	(:map
	 emacs-lisp-mode-map ("C-c C-t" . #'my-eval-buf-and-run-ert-test-at-point)))
;; ERT:2 ends here

;; [[file:Sacha.org::#undercover][Undercover:1]]
(use-package undercover
	:quelpa (undercover :fetcher github :repo "undercover-el/undercover.el")
	:defer t
	)
(use-package coverage :defer t)
;; Undercover:1 ends here

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

;; [[file:Sacha.org::#eldoc][Eldoc:2]]
(add-to-list 'display-buffer-alist
             `(,(rx bos "*Flycheck errors*" eos)
               (display-buffer-in-side-window)
               (side . bottom)
               (reusable-frames . visible)
               (window-height . 0.33)))
;; Eldoc:2 ends here

;; [[file:Sacha.org::#eldoc][Eldoc:4]]
(use-package flycheck
	:if my-laptop-p
  :hook (flycheck-mode . mp-flycheck-prefer-eldoc)
  :bind (:map flycheck-mode-map
              ("s-n" . flycheck-next-error))
  )
(use-package eglot
	:if my-laptop-p
  :preface
;;;###autoload
  (defun mp-eglot-eldoc ()
    (setq eldoc-documentation-strategy
            'eldoc-documentation-compose-eagerly))
  :hook ((eglot-managed-mode . mp-eglot-eldoc)))
;; Eldoc:4 ends here

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

;; [[file:Sacha.org::#jumping-to-code][Jumping to code:1]]
(define-key emacs-lisp-mode-map (kbd "C-c .") 'find-function-at-point)
;; Jumping to code:1 ends here

;; [[file:Sacha.org::#evaluation][Evaluation:2]]
(bind-key "M-:" 'pp-eval-expression)
(bind-key "C-x C-e" 'sanityinc/eval-last-sexp-or-region emacs-lisp-mode-map)
;; Evaluation:2 ends here

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

;; [[file:Sacha.org::#stubbing][Stubbing:2]]
(bind-key "C-:" #'my-stub-elisp-defun emacs-lisp-mode-map)
;; Stubbing:2 ends here

;; [[file:Sacha.org::#helpful][Helpful:1]]
(use-package helpful
  :bind
  ([remap describe-key] . helpful-key)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-function] . helpful-callable))
;; Helpful:1 ends here

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

;; [[file:Sacha.org::#coding-emacs-lisp-democratize][Democratize:1]]
(use-package xht
	:vc (:url "https://git.sr.ht/~flandrew/xht"))
(use-package democratize
	:vc (:url "https://git.sr.ht/~flandrew/democratize")
  :config
  (democratize-enable-examples-in-helpful)
  (democratize-enable-examples-in-help))
;; Democratize:1 ends here

;; [[file:Sacha.org::#coding-emacs-lisp-json][JSON:1]]
(setq json-object-type 'alist
			json-array-type 'list)
;; JSON:1 ends here

;; [[file:Sacha.org::#coding-emacs-lisp-useful-libraries][Useful libraries:1]]
(use-package plz)
(use-package tzc)
;; Useful libraries:1 ends here

;; [[file:Sacha.org::#coding-emacs-lisp-other-useful-functions][Other useful functions:1]]
(defun my-weekly-average (count start end)
  (/ (* 7.0 count) (days-between end start)))
;; Other useful functions:1 ends here

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

(defun my-use-yasnippet-capf () (add-to-list 'completion-at-point-functions #'yasnippet-capf))

(use-package yasnippet-capf
  :after cape
  :config
  (add-to-list 'completion-at-point-functions #'yasnippet-capf)
  :hook
  (emacs-lisp-mode . my-use-yasnippet-capf)
  (lisp-interaction-mode . my-use-yasnippet-capf)
  (org-mode . my-use-yasnippet-capf)
  (js2-mode . my-use-yasnippet-capf)
  )
;;        (global-set-key (kbd "C-c y") (lambda () (interactive)
;;                                         (yas/load-directory "~/elisp/snippets")))
;; Snippets:1 ends here

;; [[file:Sacha.org::#snippets][Snippets:3]]
(setq default-cursor-color "gray")
(setq yasnippet-can-fire-cursor-color "purple")
;; Snippets:3 ends here

;; [[file:Sacha.org::#snippets][Snippets:5]]
;; As pointed out by Dmitri, this will make sure it will update color when needed.
(remove-hook 'post-command-hook 'my-change-cursor-color-when-can-expand)
;; Snippets:5 ends here

;; [[file:Sacha.org::#show-column-number][Show column number:1]]
(column-number-mode 1)
;; Show column number:1 ends here

;; [[file:Sacha.org::#don-t-show-whitespace-in-diff-but-show-context][Don't show whitespace in diff, but show context:1]]
(setq vc-diff-switches '("-b" "-B" "-u"))
(setq vc-git-diff-switches nil)
;; Don't show whitespace in diff, but show context:1 ends here

;; [[file:Sacha.org::#javascript][Javascript:1]]
(add-to-list 'auto-mode-alist '("\\.c?js\\'" . js-mode))
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

;; [[file:Sacha.org::#javascript][Javascript:6]]
(use-package js2-mode
  :if my-laptop-p
  :commands js2-mode
  :defer t
  :interpreter "node"
  :init (setq js-indent-level 2)
	:mode "\\.[mc]?js\\'"
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

;; [[file:Sacha.org::#coding-javascript-node][Node:1]]
(with-eval-after-load 'compile
  (add-to-list 'compilation-error-regexp-alist 'node)
  (add-to-list 'compilation-error-regexp-alist-alist
               '(node "^[[:blank:]]*at \\(?:.* (\\|\\)\\(.+?\\):\\([0-9]+\\):\\([0-9]+\\))?$" 1 2 3)))
;; Node:1 ends here

;; [[file:Sacha.org::#indium][Indium:1]]
(use-package indium
:hook ((js2-mode . indium-interaction-mode)))
;; Indium:1 ends here

;; [[file:Sacha.org::#react][React:1]]
(use-package rjsx-mode
	:defer t
  :if my-laptop-p)
;; React:1 ends here

;; [[file:Sacha.org::#coding-typescript][Typescript:1]]
(use-package typescript-mode
	:mode "\\.ts\\'")
;; Typescript:1 ends here

;; [[file:Sacha.org::#shell][Shell:1]]
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)
;; Shell:1 ends here

;; [[file:Sacha.org::#shellcheck][Shellcheck:1]]
(use-package flymake
  :bind (("S-e" . flymake-show-project-diagnostics)))

(use-package sh-script
  :hook (sh-mode . flymake-mode))

(use-package flymake-shellcheck :defer t)
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

;; [[file:Sacha.org::#dwim-shell-command][dwim-shell-command:2]]
(use-package dwim-shell-command
  :if my-laptop-p
  :bind (([remap shell-command] . my-dwim-shell-command)
         :map dired-mode-map
         ([remap dired-do-async-shell-command] . my-dwim-shell-command)
         ([remap dired-do-shell-command] . my-dwim-shell-command)
         ([remap dired-smart-shell-command] . my-dwim-shell-command))
  )
;; dwim-shell-command:2 ends here

;; [[file:Sacha.org::#coding-shell-exec-path-from-shell][Exec path from shell:1]]
(use-package exec-path-from-shell
  :init
  (exec-path-from-shell-initialize)
  :custom
  (exec-path-from-shell-variables
   '("PATH" "MANPATH"
     "GOOGLE_API_KEY"
     "AZURE_SPEECH_KEY"
     "AZURE_SPEECH_REGION"
     "GEMINI_API_KEY"
     "GEMINI_PAID_API_KEY"
     "MISTRAL_API_KEY")
  ))
;; Exec path from shell:1 ends here

;; [[file:Sacha.org::#magit][Magit - nice git interface:2]]
(defvar my-magit-limit-to-directory)
(use-package magit
  :config
  (setq magit-diff-options '("-b")) ; ignore whitespace
  (setq my-magit-limit-to-directory nil)
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
;; Magit - nice git interface:2 ends here

;; [[file:Sacha.org::#coding-magit-nice-git-interface-use-difftastic][Use difftastic:2]]
(with-eval-after-load 'magit
	(transient-append-suffix 'magit-dispatch "!"
		'("#" "My Magit Cmds" th/magit-aux-commands))
	(define-key magit-status-mode-map (kbd "#") #'th/magit-aux-commands))
;; Use difftastic:2 ends here

;; [[file:Sacha.org::#git-messenger-shows-commit-message][git-messenger - shows commit message:1]]
(use-package git-messenger
  :bind (("C-x v m" . git-messenger:popup-message)))
;; git-messenger - shows commit message:1 ends here

;; [[file:Sacha.org::#tag-files][Tag files:2]]
(with-eval-after-load 'drupal-mode
  (add-hook 'drupal-mode-hook 'my-find-tags))
;; Tag files:2 ends here

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
  :defer t
  :config
  (add-hook 'makefile-mode-hook 'makefile-executor-mode))
(defun my-projectile-open-notes ()
	(interactive)
	(find-file-other-window (expand-file-name "notes.org" (projectile-project-root))))
;; Projects and projectile:1 ends here

;; [[file:Sacha.org::#coding-projects-and-projectile-capturing-notes-to-per-project-files][Capturing notes to per-project files:1]]
(use-package org-project-capture :defer t)
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

;; [[file:Sacha.org::#exploring-melpa-recipes][Exploring MELPA recipes:1]]

;; Exploring MELPA recipes:1 ends here

;; [[file:Sacha.org::#ruby][Ruby:5]]
(use-package inf-ruby
	:defer t
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
;; Ruby:5 ends here

;; [[file:Sacha.org::#skewer][Skewer:1]]
(use-package skewer-mode
  :if my-laptop-p
  :hook
  ((js2-mode-hook . skewer-mode)
   (css-mode-hook . skewer-css-mode)
   (html-mode-hook . skewer-html-mode)))
;; Skewer:1 ends here

;; [[file:Sacha.org::#autocomplete][Autocomplete:1]]
(with-eval-after-load 'company
	(define-key company-mode-map (kbd "<tab>") 'company-indent-or-complete-common))
(use-package company
  :if my-laptop-p
  ;:init (add-hook 'prog-mode-hook 'company-mode)
  )
(use-package company-posframe :if my-laptop-p :init (company-posframe-mode 1) :diminish)
;; Autocomplete:1 ends here

;; [[file:Sacha.org::#tern-for-javascript][Tern - for Javascript:1]]
(use-package tern
  :if my-laptop-p
  :bind (:map tern-mode-keymap ("C-c C-c" . compile))
  :hook (js2-mode-hook . tern-mode)
  :config
  (when (eq system-type 'windows-nt) (setq tern-command '("cmd" "/c" "tern"))))
;; Tern - for Javascript:1 ends here

;; [[file:Sacha.org::#docker][Docker:1]]
(use-package dockerfile-mode
  :mode ("Dockerfile\\'" . dockerfile-mode))
;; Docker:1 ends here

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
(use-package phi-search :defer t)
(use-package phi-search-mc :config (phi-search-mc/setup-keys) :defer t)
(use-package mc-extras :config (define-key mc/keymap (kbd "C-. =") 'mc/compare-chars) :defer t)
;; Multiple cursors mode:1 ends here

;; [[file:Sacha.org::#coding-automation-iedit][iedit:1]]
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

;; [[file:Sacha.org::#coding-eshell-eshell-completion][Eshell completion:1]]
(use-package capf-autosuggest
   :hook
   (eshell-mode . capf-autosuggest-mode))
;; Eshell completion:1 ends here

;; [[file:Sacha.org::#coding-sqlite][SQLite:2]]
(use-package sqlite-mode
	:commands sqlite-mode-open-file
  :config
  (add-to-list 'magic-mode-alist '("SQLite format 3\x00" . ct/sqlite-view-file-magically)))
;; SQLite:2 ends here

;; [[file:Sacha.org::#internet-relay-chat][Internet Relay Chat:1]]
(use-package erc
	:defer t
	:commands erc-select
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
        erc-track '("NICK" "333" "353" "JOIN" "PART" "AWAY")))
;; Internet Relay Chat:1 ends here

;; [[file:Sacha.org::#mastodon][Mastodon:1]]
(use-package tp
	:vc (:url "https://codeberg.org/martianh/tp.el")
	)
(use-package mastodon
  :if my-laptop-p
	:load-path "~/vendor/mastodon.el/lisp"
  :config
  (require 'mastodon-tl)
  :bind
  (:map mastodon-mode-map
        ("g" . mastodon-tl-update)
        ;; see org-capture-templates addition
        ("o" . (lambda () (interactive) (org-capture nil "m")))
				:map mastodon-toot-mode-map)
  :commands (mastodon-http--api
						 mastodon-http--post
						 mastodon-mode
						 mastodon-http--get-search-json
						 mastodon-tl-get-local-timeline)
	:custom
  (mastodon-tl--display-media-p nil)
	(mastodon-instance-url "https://social.sachachua.com")
  (mastodon-active-user "sacha")
	(mastodon-group-notifications nil))
;; Mastodon:1 ends here

;; [[file:Sacha.org::#mastodon][Mastodon:3]]
(autoload 'mastodon-url-lookup "mastodon")
(add-to-list 'browse-url-handlers '("https?://[^/]+/@[^/]+/.*" . my-mastodon-browse-url))
;; Mastodon:3 ends here

;; [[file:Sacha.org::#mastodon-mastodon-el-copy-toot-url-after-posting-also-copying-just-this-post-with-11ty][mastodon.el: Copy toot URL after posting; also, copying just this post with 11ty:2]]
(with-eval-after-load 'mastodon-toot
	(when (functionp 'mastodon-toot-send)
		(advice-add
		 #'mastodon-toot-send
		 :after
		 (lambda (&rest _)
			 (run-hook-with-args 'my-mastodon-toot-posted-hook (my-mastodon-latest-toot)))))
	(when (functionp 'mastodon-toot--send)
		(advice-add
		 #'mastodon-toot--send
		 :after
		 (lambda (&rest _)
			 (run-hook-with-args 'my-mastodon-toot-posted-hook (my-mastodon-latest-toot))))))
;; mastodon.el: Copy toot URL after posting; also, copying just this post with 11ty:2 ends here

;; [[file:Sacha.org::#storing-mastodon-links-in-org-mode][Storing Mastodon links in Org mode:2]]
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
;; Storing Mastodon links in Org mode:2 ends here

;; [[file:Sacha.org::#mastodon-news][Collecting Emacs News from Mastodon:2]]
(use-package org
	:config
	(add-to-list
	 'org-capture-templates
   '("📰" "Emacs News" entry (file+headline "~/sync/orgzly/news.org" "Collect Emacs News")
     "* %a  :news:

#+begin_quote
%:text
#+end_quote

"
     :prepend t :immediate-finish t)))

(use-package mastodon
	:bind (:map mastodon-mode-map ("w" . my-mastodon-save-toot-for-emacs-news)))
;; Collecting Emacs News from Mastodon:2 ends here

;; [[file:Sacha.org::#mastodon-combined-timeline][Combining Mastodon timelines using mastodon.el:2]]
(when (functionp 'memoize)
	(unless (get #'my-mastodon-fetch-posts-after :memoize-original-function)
		(memoize #'my-mastodon-fetch-posts-after)))
;; Combining Mastodon timelines using mastodon.el:2 ends here

;; [[file:Sacha.org::#mastodon-combined-timeline][Combining Mastodon timelines using mastodon.el:3]]
(defun my-mastodon-lookup-toot ()
  (interactive)
  (mastodon-url-lookup (mastodon-toot--toot-url)))
;; Combining Mastodon timelines using mastodon.el:3 ends here

;; [[file:Sacha.org::#mastodon-combined-timeline][Combining Mastodon timelines using mastodon.el:6]]
(with-eval-after-load 'mastodon-tl
	(advice-add #'mastodon-toot--action :before #'my-mastodon-update-external-item-id)
	(advice-add #'mastodon-toot--reply :before #'my-mastodon-update-external-item-id)
	(advice-add #'mastodon-tl--thread :before #'my-mastodon-update-external-item-id))
;; Combining Mastodon timelines using mastodon.el:6 ends here

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
    ("@" (progn (require 'mastodon) (mastodon-notifications-get-mentions)))
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
    ("O" mastodon-profile-my-profile)
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

;; [[file:Sacha.org::#mastodon-org-contacts-complete][Completion:2]]
(with-eval-after-load 'mastodon-toot
	(with-eval-after-load 'org-contacts
		(add-hook 'mastodon-toot-mode-hook
							(lambda ()
								(add-hook 'completion-at-point-functions
													#'my-mastodon-complete-contact nil t)))))
;; Completion:2 ends here

;; [[file:Sacha.org::#mastodon-org-feed][Collect my recent toots in an Org file so that I can refile them:1]]
(use-package pandoc :defer t)
(advice-add #'org-feed-add-items :after #'my-org-feed-sort)
(setq org-feed-alist '(("Mastodon" "https://emacs.ch/@sachac/with_replies.rss"
												"~/sync/orgzly/toots.org" "Toots"
												:formatter my-mastodon-org-feed-formatter)))
;; Collect my recent toots in an Org file so that I can refile them:1 ends here

;; [[file:Sacha.org::#web-emacs-open-urls-or-search-the-web-plus-browse-url-handlers][Emacs: Open URLs or search the web, plus browse-url-handlers:2]]
(setopt my-search-web-handler #'consult-omni)
;; Emacs: Open URLs or search the web, plus browse-url-handlers:2 ends here

;; [[file:Sacha.org::#web-emacs-open-urls-or-search-the-web-plus-browse-url-handlers][Emacs: Open URLs or search the web, plus browse-url-handlers:3]]
(keymap-global-set "C-c o" #'my-open-url-or-search-web)
;; Emacs: Open URLs or search the web, plus browse-url-handlers:3 ends here

;; [[file:Sacha.org::#web-emacs-open-urls-or-search-the-web-plus-browse-url-handlers][Emacs: Open URLs or search the web, plus browse-url-handlers:4]]
(setq browse-url-handlers
      (seq-union
       browse-url-handlers
			 '(("https?://?medium\\.com" . ignore)
				 ("https?://[^/]+/@[^/]+/.*" . mastodon-url-lookup)
				 ("https?://mailchimp\\.com" . browse-url-chrome)
				 ("https?://bbb\\.emacsverse\\.org" . browse-url-chrome)
				 ("https?://emacswiki.org" . eww))))
(setopt browse-url-browser-function 'browse-url-firefox)
;; Emacs: Open URLs or search the web, plus browse-url-handlers:4 ends here

;; [[file:Sacha.org::#search][Search:1]]
(use-package engine-mode
	:defer t
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

;; [[file:Sacha.org::#web-spookfox-link-to-current-webpage-from-spookfox][Link to current webpage from Spookfox:2]]
(with-eval-after-load 'org
	(org-link-set-parameters
	 "spookfox"
	 :complete #'my-org-spookfox-complete
	 :insert-description #'my-org-link-insert-description))
;; Link to current webpage from Spookfox:2 ends here

;; [[file:Sacha.org::#spookfox-babel][Running the current Org Mode Babel Javascript block from Emacs using Spookfox:5]]
(with-eval-after-load 'ob-js
	(advice-add 'org-babel-execute:js :around #'my-org-babel-execute:js-spookfox))
;; Running the current Org Mode Babel Javascript block from Emacs using Spookfox:5 ends here

;; [[file:Sacha.org::#spookfox-babel][Running the current Org Mode Babel Javascript block from Emacs using Spookfox:7]]
(with-eval-after-load 'embark-org
	(define-key embark-org-src-block-map "f" #'my-spookfox-eval-org-block))
;; Running the current Org Mode Babel Javascript block from Emacs using Spookfox:7 ends here

;; [[file:Sacha.org::#spookfox-scroll][Using Spookfox to scroll Firefox up and down from Emacs:1]]
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

;; [[file:Sacha.org::#spookfox-scroll][Using Spookfox to scroll Firefox up and down from Emacs:3]]
(keymap-global-set "C-s-v" 'my-spookfox-scroll-down)
(keymap-global-set "S-s-v" 'my-spookfox-scroll-up)
;; Using Spookfox to scroll Firefox up and down from Emacs:3 ends here

;; [[file:Sacha.org::#spookfox-insert-url][Emacs and Spookfox: org-capture the current tab from Firefox or a link from the page:2]]
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
;; Emacs and Spookfox: org-capture the current tab from Firefox or a link from the page:2 ends here

;; [[file:Sacha.org::#clock-in][Quantified Awesome:2]]
(bind-key "C-c q" 'my-org-quick-clock-in-task)
(bind-key "!" 'my-org-clock-in-and-track org-agenda-mode-map)
;; Quantified Awesome:2 ends here

;; [[file:Sacha.org::#using-the-calendar-date-echo-text-variable-to-help-plot-a-heatmap-on-a-year-long-calendar-in-emacs][Using the calendar-date-echo-text variable to help plot a heatmap on a year-long calendar in Emacs:1]]
;; This seems to be the only way we can hack the date in for now
(setq calendar-date-echo-text '(apply #'format (list "%04d-%02d-%02d" year month day)))
;; Using the calendar-date-echo-text variable to help plot a heatmap on a year-long calendar in Emacs:1 ends here

;; [[file:Sacha.org::#using-the-calendar-date-echo-text-variable-to-help-plot-a-heatmap-on-a-year-long-calendar-in-emacs][Using the calendar-date-echo-text variable to help plot a heatmap on a year-long calendar in Emacs:3]]
(advice-add #'calendar :after #'my-calendar-heat-map-using-echo-text)
(advice-add #'calendar-redraw :after #'my-calendar-heat-map-using-echo-text)
(advice-add #'year-calendar :after #'my-calendar-heat-map-using-echo-text)
;; Using the calendar-date-echo-text variable to help plot a heatmap on a year-long calendar in Emacs:3 ends here

;; [[file:Sacha.org::#using-the-calendar-date-echo-text-variable-to-help-plot-a-heatmap-on-a-year-long-calendar-in-emacs][Using the calendar-date-echo-text variable to help plot a heatmap on a year-long calendar in Emacs:7]]
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
;; Using the calendar-date-echo-text variable to help plot a heatmap on a year-long calendar in Emacs:7 ends here

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
;; Emacs and my phone:1 ends here

;; [[file:Sacha.org::#syncthing][Syncthing:1]]
(setq ediff-toggle-skip-similar t
      ediff-diff-options "-w"
      ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-split-window-function 'split-window-horizontally)
;; Syncthing:1 ends here

;; [[file:Sacha.org::#clipboard][Clipboard:1]]
(use-package clipmon
  :disabled t
  :init (progn (setq clipmon-action 'kill-new clipmon-timeout nil clipmon-sound nil clipmon-cursor-color nil clipmon-suffix nil) (clipmon-mode)))
;; Clipboard:1 ends here

;; [[file:Sacha.org::#clipboard][Clipboard:2]]
(use-package xclip :if my-phone-p) ; Turn on with xclip-mode
;; Clipboard:2 ends here

;; [[file:Sacha.org::#async-smtpmail][Send mail asynchronously:2]]
(setq send-mail-function 'my-async-smtpmail-send-it
      message-send-mail-function 'my-async-smtpmail-send-it)
;; Send mail asynchronously:2 ends here

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

;; [[file:Sacha.org::#act-on-current-message-with-embark][Act on current message with Embark:2]]
(with-eval-after-load 'embark
	(add-to-list 'embark-target-finders 'mail-embark-finder)
	)
;; Act on current message with Embark:2 ends here

;; [[file:Sacha.org::#gnus][Gnus:1]]
(setq mml-secure-openpgp-encrypt-to-self t)
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

;; [[file:Sacha.org::#gnus][Gnus:2]]
(use-package gnus
	:defer t
	:commands gnus
  :config
  (require 'mm-decode)
  (setq mm-discouraged-alternatives
        '("text/html" "text/richtext")
        mm-automatic-display
        (-difference mm-automatic-display '("text/html" "text/enriched" "text/richtext"))))
;; Gnus:2 ends here

;; [[file:Sacha.org::#gnus][Gnus:3]]
(setq gnus-treat-hide-citation t)
;; Gnus:3 ends here

;; [[file:Sacha.org::#gnus][Gnus:4]]
(setq gnus-use-adaptive-scoring t)
(setq gnus-default-adaptive-score-alist
      '((gnus-unread-mark)
        (gnus-ticked-mark (subject 10))
        (gnus-killed-mark (subject -5))
        (gnus-catchup-mark (subject -1))))
;; Gnus:4 ends here

;; [[file:Sacha.org::#emacs-server][Emacs server:1]]
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (select-frame frame)
            (my-setup-color-theme)))
;; Emacs server:1 ends here

;; [[file:Sacha.org::#collaboration][Collaboration:1]]
(use-package crdt
  :quelpa (crdt :fetcher github :repo "zaeph/crdt.el")
  :commands (crdt-share-buffer crdt-connect)
  :load-path "~/vendor/crdt.el"
  :if my-laptop-p)
;; Collaboration:1 ends here

;; [[file:Sacha.org::#collaboration-bike-brigade-working-with-mailchimp-images][Bike Brigade: working with Mailchimp images:1]]
(use-package mailchimp :load-path "~/proj/mailchimp-el" :vc (:url "https://github.com/sachac/mailchimp-el"))
;; Bike Brigade: working with Mailchimp images:1 ends here

;; [[file:Sacha.org::#streaming-send-currently-clocked-task-title-to-file-include-in-stream][Send currently-clocked task title to file, include in stream:2]]
(with-eval-after-load 'org-clock
  (add-hook 'org-clock-in-hook #'my-org-save-streaming-task-to-file)
  (add-hook 'org-clock-out-hook #'my-org-clear-streaming-task))
;; Send currently-clocked task title to file, include in stream:2 ends here

;; [[file:Sacha.org::#streaming-stream-agenda][Stream agenda:1]]
(defun my-stream-agenda ()
  (interactive)
  (org-agenda nil "s")
  )
;; Stream agenda:1 ends here

;; [[file:Sacha.org::#controlling-my-stream-audio-from-emacs-background-music-typing-sounds-and-push-to-talk][Controlling my stream audio from Emacs: background music, typing sounds, and push to talk:4]]
(use-package selectric-mode
  :if my-laptop-p
  :diminish ""
	:defer t
	:commands selectric-mode
  :config
  (fset #'selectric-type-sound #'my-selectric-type-sound))
;; Controlling my stream audio from Emacs: background music, typing sounds, and push to talk:4 ends here

;; [[file:Sacha.org::#controlling-my-stream-audio-from-emacs-background-music-typing-sounds-and-push-to-talk][Controlling my stream audio from Emacs: background music, typing sounds, and push to talk:6]]
(add-to-list 'mode-line-front-space '(:eval (if my-mic-p "*MIC*" "")))
;; Controlling my stream audio from Emacs: background music, typing sounds, and push to talk:6 ends here

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
(keymap-global-set "s-R" #'ignore)
(keymap-global-set "s-v" #'my-stream/body)
(keymap-global-set "s-SPC" #'my-stream/body)
;; General streaming configuration:1 ends here

;; [[file:Sacha.org::#playing-recordings][Playing recordings:1]]
(use-package mpv :if my-laptop-p :defer t :commands mpv)
;; Playing recordings:1 ends here

;; [[file:Sacha.org::#stream-notes][Stream notes:2]]
(with-eval-after-load 'org
	(add-hook 'org-mode-hook 'my-org-save-and-tangle-stream-notes))
;; based on https://www.reddit.com/r/emacs/comments/57nps0/comment/d8umsr4/?context=3
(setq imp-default-user-filters '((org-mode . my-impatient-org-export-as-html-filter)
                                 (mhtml-mode . nil)
                                 (html-mode . nil)
                                 (web-mode  . nil)))
;; Stream notes:2 ends here

;; [[file:Sacha.org::#stream-notes][Stream notes:4]]
(use-package impatient-mode
  :config
	(setq impatient-mode-delay 1)
	(setq httpd-port 8085)
	(imp-set-user-filter 'my/impatient-org-export-as-html-filter))
;; Stream notes:4 ends here

;; [[file:Sacha.org::#speech-to-text][Try continuous streaming and the Google Speech Recognition API:2]]
(keymap-global-set  "<f11>" 'my-stream-captions-edit-last)

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
;; Try continuous streaming and the Google Speech Recognition API:2 ends here

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

;; [[file:Sacha.org::#ssh-and-daemon][SSH and --daemon:2]]
(my-ssh-refresh)
;; SSH and --daemon:2 ends here

;; [[file:Sacha.org::#encryption][Encryption:1]]
(setq epa-file-encrypt-to '("sacha@sachachua.com"))
(setq epa-pinentry-mode 'loopback)
(setq epg-pinentry-mode 'loopback)
;; Encryption:1 ends here

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

;; [[file:Sacha.org::#oddmuse][Oddmuse:1]]
(use-package oddmuse
  :if my-laptop-p
  :load-path "~/vendor/oddmuse-el"
  :ensure nil
  :config (oddmuse-mode-initialize)
	:commands oddmuse-edit
  :hook (oddmuse-mode-hook .
                           (lambda ()
                             (unless (string-match "question" oddmuse-post)
                               (when (string-match "EmacsWiki" oddmuse-wiki)
                                 (setq oddmuse-post (concat "uihnscuskc=1;" oddmuse-post)))
                               (when (string-match "OddmuseWiki" oddmuse-wiki)
                                 (setq oddmuse-post (concat "ham=1;" oddmuse-post)))))))
;; Oddmuse:1 ends here

;; [[file:Sacha.org::#mineclone][MineClone:1]]
(defun my-mineclone-ripgrep ()
		(interactive)
		(let ((default-directory "~/vendor/MineClone2"))
			(call-interactively 'consult-ripgrep)))
(keymap-global-set "s-M" 'my-mineclone-ripgrep)
;; MineClone:1 ends here

;; [[file:Sacha.org::#plover][Plover:1]]
(use-package plover-websocket
  :load-path "~/proj/plover-websocket-el"
  :after websocket
  :if my-laptop-p
	:defer t
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
    (overlay-put my-practise-steno-previous-overlay 'evaporate t)
    (overlay-put my-practise-steno-previous-overlay 'face 'my-practise-steno-correct)))

(defun my-practise-steno--mark-incorrect-and-fixed ()
  (let ((ov (make-overlay (overlay-end my-practise-steno-previous-overlay)
                          (+ (overlay-end my-practise-steno-previous-overlay) (match-beginning 0)))))
    (overlay-put ov 'face 'my-practise-steno-wrong)
    (overlay-put ov 'evaporate t))
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
    (overlay-put my-practise-steno-previous-overlay 'evaporate t)
    (overlay-put my-practise-steno-highlight-overlay 'face 'my-practise-steno-highlight)
    (overlay-put my-practise-steno-highlight-overlay 'evaporate t)
    (overlay-put my-practise-steno-current-overlay 'modification-hooks '(my-practise-steno-check))
    (overlay-put my-practise-steno-current-overlay 'insert-in-front-hooks '(my-practise-steno-check))
    (overlay-put my-practise-steno-current-overlay 'face 'my-practise-steno-wrong)
    (overlay-put my-practise-steno-current-overlay 'evaporate t)
    ;; (add-hook 'after-change-functions 'my-practise-steno-check nil t)
    (add-hook 'plover-websocket-on-message-payload-functions 'my-practise-steno-store-strokes)
    (switch-to-buffer (current-buffer))))

(defun my-practise-steno-word-list (words)
  (interactive (list (mapcar 'list (split-string (read-string "Words: ")))))
  (my-practise-steno words))


;; (call-interactively 'my-practise-steno)
;; Practising within Emacs:1 ends here

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
	"Insert function name."
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
;; Displaying frequency-sorted completions with stroke hints:1 ends here

;; [[file:Sacha.org::#key-chord][Key chords:2]]
  (fset 'key-chord-define 'my-key-chord-define)
;; Key chords:2 ends here

;; [[file:Sacha.org::#key-chord][Key chords:3]]
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
;; Key chords:3 ends here

;; [[file:Sacha.org::#key-chord][Key chords:4]]
  (bind-key "C-t" 'my-key-chord-commands/body)
;; Key chords:4 ends here

;; [[file:Sacha.org::#emacspeak][Emacspeak:1]]
(setq emacspeak-prefix (kbd "s-e"))
;; Emacspeak:1 ends here

;; [[file:Sacha.org::#manage-photos-with-geeqie][Manage photos with geeqie:4]]
(use-package ewmctrl :defer t)
;; Manage photos with geeqie:4 ends here

;; [[file:Sacha.org::#manage-photos-with-geeqie][Manage photos with geeqie:6]]
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
;; Manage photos with geeqie:6 ends here

;; [[file:Sacha.org::#emacsconf][EmacsConf:2]]
(use-package emacsconf
  :after hydra
  :bind (("C-c e" . emacsconf/body)
         ("M-g t" . emacsconf-go-to-talk))
	:init
	(require 'emacsconf-autoloads)
	:hook
	(message-send . emacsconf-mail-check-for-zzz-before-sending)
	:config
  (setq emacsconf-refresh-schedule-from-org t)
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
		("o" emacsconf-main-org-notebook-heading "org notes")
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
;; EmacsConf:2 ends here

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

;; [[file:Sacha.org::#chatgpt-ai][ChatGPT, AI, and large-language models:3]]
(use-package gptel
	:commands (gptel gptel-send gptel-set-topic gptel-menu)
	:defer t
	:config
  (setq my-gptel-groq
        (gptel-make-openai "Groq"
          :host "api.groq.com"
          :endpoint "/openai/v1/chat/completions"
          :stream t
          :key (gptel-api-key-from-environment "GROQ_API_KEY")
          :models '(llama-3.3-70b-versatile
                    llama-3.1-8b-instant
                    openai/gpt-oss-20b
                    openai/gpt-oss-120b)))
  (setq my-gptel-gemini
        (gptel-make-gemini "Gemini"
          :key (gptel-api-key-from-environment "GEMINI_API_KEY")
          :stream t
          :models '(gemini-3-flash-preview
                    gemini-2.5-flash
                    gemini-2.5-pro
                    gemini-2.5-flash-preview-09-2025
                    gemini-2.5-flash-lite)))
  (setq my-gptel-gemini-paid
        (gptel-make-gemini "Gemini - paid"
          :key (gptel-api-key-from-environment "GEMINI_PAID_API_KEY")
          :stream t
          :models '(gemini-3-flash-preview
                    gemini-2.5-flash
                    gemini-2.5-flash-preview-09-2025
                    gemini-2.5-flash-lite)))
  (setq my-gptel-mistral
        (gptel-make-openai "Mistral"
          :key (gptel-api-key-from-environment "MISTRAL_API_KEY")
          :host "api.mistral.ai"
          :endpoint "/v1/chat/completions"
          :stream t
          :models '(mistral-medium
                    mistral-large-2411)))
	(setq gptel-model 'gemini-3-flash-preview
				gptel-backend my-gptel-gemini
        gptel-log-level 'info)
	:hook
	(gptel-post-stream . gptel-auto-scroll)
	(gptel-post-response . gptel-end-of-response)
	)
;; ChatGPT, AI, and large-language models:3 ends here

;; [[file:Sacha.org::#inactive-infrequent-things-chatgpt-ai-and-large-language-models-agent-shell][agent-shell:2]]
(use-package agent-shell
  :config
  (setopt agent-shell-preferred-agent-config (agent-shell-anthropic-make-claude-code-config))
  (setopt agent-shell-session-strategy 'prompt)
  (setopt agent-shell-dot-subdir-function #'my-agent-shell-dot-subdir)
)
;; agent-shell:2 ends here

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

;; [[file:Sacha.org::#completion-at-point][Completion at point?:1]]
(use-package corfu :init (global-corfu-mode)
  :bind (:map corfu-map ("<tab>" . corfu-complete))
  :config
  (setq tab-always-indent 'complete)
  (setq corfu-preview-current nil)
  (setq corfu-min-width 20)

  (setq corfu-popupinfo-delay '(1.25 . 0.5))
  (corfu-popupinfo-mode 1) ; shows documentation after `corfu-popupinfo-delay'

  ;; Sort by input history (no need to modify `corfu-sort-function').
  (with-eval-after-load 'savehist
    (corfu-history-mode 1)
    (add-to-list 'savehist-additional-variables 'corfu-history))
	)
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

;; [[file:Sacha.org::#inactive-infrequent-things-fun-and-games-make-memes-from-emacs][Make memes from Emacs:1]]
(use-package meme
	;:quelpa (meme :fetcher github :repo "larsmagne/meme")
	:load-path "~/vendor/meme"
	:init (provide 'imgur)  ; fake this
	:defer t
	:commands meme
	:config
	(setq meme-dir "~/vendor/meme/images")
	(setq meme-font "Roboto"))
;; Make memes from Emacs:1 ends here

;; [[file:Sacha.org::#rubik-s-cube][Rubik's Cube:1]]
(use-package eagle
	:quelpa (eagle :fetcher git
								 :url "https://codeberg.org/akib/emacs-eagle.git")
	:defer t)
(use-package cube
	:quelpa (cube :fetcher git
								:url "https://codeberg.org/akib/emacs-cube.git")
	:defer t)
;; Rubik's Cube:1 ends here

;; [[file:Sacha.org::#minecraft][Minecraft:1]]
(use-package mcf
	;:quelpa (mcf :fetcher github :repo "sachac/mcf")
	:load-path "~/vendor/mcf"
	:mode ("\\.mcfunction\\'" . mcf-mode)
	;; rcon settings are in my .emacs.secrets file
	:commands (mcf-rcon mcf-mode)
	)
;; Minecraft:1 ends here

;; [[file:Sacha.org::#speech-synthesis-experimental][Speech synthesis (experimental):1]]
(use-package speechd-el)
(with-eval-after-load 'speechd-speak
  (setq speechd-speak-ignore-command-keys
        (append
         '(lispy-delete-backward
           lispy-delete
           python-indent-dedent-line-backspace)
         speechd-speak-ignore-command-keys)))
;; Speech synthesis (experimental):1 ends here
