;; -*- lexical-binding: t -*-
;; This sets up the load path so that we can override it
(setq warning-suppress-log-types '((package reinitialization)))  (package-initialize)
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp")
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
  (defvar sacha-laptop-p (or (equal (system-name) "sacha-x230") (equal (system-name) "sacha-p52")))
  (defvar sacha-server-p (and (equal (system-name) "localhost") (equal user-login-name "sacha")))
  (defvar sacha-phone-p (not (null (getenv "ANDROID_ROOT")))
    "If non-nil, GNU Emacs is running on Termux.")
  (when sacha-phone-p (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))
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
    (advice-add 'backup-walker-refresh :override #'sacha-backup-walker-refresh))
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
          sacha-stream-number
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

;; [[file:Sacha.org::#killing-text][Killing text:4]]
(keymap-global-set "M-w" #'sacha-copy-symbol-if-region-is-inactive)
;; Killing text:4 ends here

;; [[file:Sacha.org::#keybindings][Keybindings:1]]
  (repeat-mode 1)
;; Keybindings:1 ends here

;; [[file:Sacha.org::#embark][Embark:1]]
  (use-package embark
    :after org
    :load-path "~/vendor/embark"
    :config
          (setq embark-prompter 'embark-keymap-prompter)
          (add-to-list 'embark-target-finders 'sacha-embark-org-element)
          (add-to-list 'embark-target-finders 'sacha-embark-subed-timestamp)
          (add-to-list 'embark-target-injection-hooks '(sacha-journal-post embark--allow-edit))
          (with-eval-after-load 'subed
            (defvar-keymap embark-subed-timestamp-actions
              :doc "Subed timestamp actions"
              :parent subed-mode-map
              "." #'sacha-subed-set-timestamp-to-mpv-position
              "w" #'sacha-subed-copy-timestamp-dwim
              "<up>" #'sacha-subed-adjust-timestamp/sacha-subed-adjust-timestamp-up
              "f" #'sacha-waveform-subed-show-after-time
              "<down>" #'sacha-subed-adjust-timestamp/sacha-subed-adjust-timestamp-down))
          (defvar-keymap embark-sketch-actions
            :doc "Org Mode sketch-related actions"
            :parent org-mode-map
            "o" #'sacha-sketch-insert-file-as-link
                  "i" #'sacha-sketch-insert-file-as-link
            "v" #'sacha-geeqie-view)
          (defvar-keymap embark-journal-actions
            :doc "Journal"
            "e" #'sacha-journal-edit)
          (add-to-list 'embark-keymap-alist '(sketch . embark-sketch-actions))
          (add-to-list 'embark-keymap-alist '(subed-timestamp . embark-subed-timestamp-actions))
          (add-to-list 'embark-keymap-alist '(journal . embark-journal-actions))
          :bind
          (("C-." . embark-act)
           :map vertico-map
           (("M-e" . embark-export))
           :map minibuffer-local-map
           (("C-c e" . embark-act)
                  ("M-e" . embark-export)
            ("C-;" . embark-act)
                  ("C-<tab>" . embark-select)
                  ("C-S-<tab>" . (lambda () (interactive) (embark-select) (vertico-next))))
           :map embark-collect-mode-map
           (("C-c e" . embark-act)
            ("C-;" . embark-act)
                  ("C-<tab>" . embark-select))
           :map embark-general-map
           (("j" . sacha-journal-post)
            ("m" . sacha-stream-message)
            ("M-w" . (lambda (s) (interactive "MString: ") (kill-new s))))
           :map embark-symbol-map
           ("r" . erefactor-rename-symbol-in-buffer)
           :map embark-url-map
           ("c" . sacha-caption-show)
           ))
  (with-eval-after-load 'embark-org
    (define-key embark-org-src-block-map
           "i" #'sacha-org-fix-block-indentation))
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
                  (define-key embark-org-link-map (kbd "q") #'sacha-org-link-qr)))
;; Using Embark and qrencode to show a QR code for the Org Mode link at point:2 ends here

;; [[file:Sacha.org::#embark-video][Using Embark to act on video:2]]
  (with-eval-after-load 'embark
          (add-to-list 'embark-target-finders 'sacha-embark-video)
          (defvar-keymap sacha-embark-video-actions
                  :doc "video"
                  "d" #'sacha-deepgram-recognize-audio
                  "$" #'sacha-deepgram-cost
                  "m" #'mpv-play
                  "c" #'sacha-caption-show
                  "w" #'sacha-audio-text
                  "W" #'waveform-show)
          (add-to-list 'embark-keymap-alist '(video . sacha-embark-video-actions)))
;; Using Embark to act on video:2 ends here

;; [[file:Sacha.org::#embark-audio][Using Embark to act on audio:2]]
(with-eval-after-load 'embark
  (add-to-list 'embark-target-finders 'sacha-embark-audio)
  (defvar-keymap sacha-embark-audio-actions
    :doc "audio"
    "a" #'sacha-open-in-audacity
    "d" #'sacha-deepgram-recognize-audio
    "$" #'sacha-deepgram-cost
    "D" #'sacha-audio-braindump-reprocess
    "m" #'mpv-play
    "w" #'sacha-audio-text
    "W" #'waveform-show)
  (add-to-list 'embark-keymap-alist '(audio . sacha-embark-audio-actions)))
;; Using Embark to act on audio:2 ends here

;; [[file:Sacha.org::#using-embark-to-insert-files-as-org-includes][Using Embark to insert files as Org INCLUDEs:2]]
(with-eval-after-load 'embark
  (define-key embark-file-map "O" #'sacha-insert-file-as-org-include))
;; Using Embark to insert files as Org INCLUDEs:2 ends here

;; [[file:Sacha.org::#using-embark-to-offer-context-sensitive-actions-for-org-elements][Using Embark to offer context-sensitive actions for Org elements:2]]
(with-eval-after-load 'embark-org
  (keymap-set embark-org-src-block-map "N" #'sacha-embark-org-src-block-copy-noweb-reference))
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
                                   '(("sacha-subed-set-timestamp-to-mpv-position" "set to MPV")
                                           ("sacha-embark-org-copy-exported-url" "⭐🗐🔗 copy exported URL")
                                           ("sacha-subed-copy-timestamp-dwim" "copy")
                                           ("sacha-sketch-insert-file-as-link" "insert")
                                           ("sacha-geeqie-view" "geeqie")
                                           ("sacha-journal-edit" "edit")
                                           ("sacha-org-link-qr" "qr")
                                           ("sacha-image-open-in-" "")
                                           ("org-babel-" "ob-")
                                           ("next" "🠆")
                                           ("previous" "🠄")
                                           ("sacha-image-" "")
                                           ("sacha-embark-org-blog-" "")
                                           ("embark-collect" "⇶ collect")
                                           ("sacha-embark-org-" "")
                                           ("sacha-" "")
                                           ("embark-" "")
                                           ("embark-act-all" "all")
                                           ("embark-become" "become")
                                           ("embark-collect" "collect")
                                           ("-" " ")))))
;; Changing the which-key labels for shortcuts:1 ends here

;; [[file:Sacha.org::#keybindings-embark-renaming-and-storing][Renaming and storing:3]]
(with-eval-after-load 'embark
  (defvar-keymap sacha-embark-image-actions
    :doc "Images"
    "k" #'sacha-image-open-in-krita
    "a" #'sacha-image-open-in-annotator
    "i" #'sacha-image-open-in-inkscape
    "w" #'sacha-image-copy-text
    "c" #'sacha-image-autocrop
    "]" #'sacha-image-rotate-clockwise
    "[" #'sacha-image-rotate-counterclockwise
    "g" #'sacha-image-open-in-gimp
    "f" #'sacha-open-in-firefox
    "s" #'sacha-image-store
    "r" #'sacha-image-recognize-and-rename
    "t" #'sacha-org-sketch-open-text-file
    "T" #'sacha-image-thumbnail
    "L" #'sacha-org-svg-copy-links
    "C" #'sacha-image-recolor
    "d" #'sacha-image-insert-text-as-details)
  (add-to-list 'embark-keymap-alist '(image . sacha-embark-image-actions)))
;; Renaming and storing:3 ends here

;; [[file:Sacha.org::#embark-subed][Embark and subed:3]]
(defhydra sacha-subed-adjust-timestamp ()
  ("<up>" sacha-subed-adjust-timestamp-up "Up" :exit nil)
  ("<down>" sacha-subed-adjust-timestamp-down "Down" :exit nil))
;; Embark and subed:3 ends here

;; [[file:Sacha.org::#casual-symbol-overlay][Embark, symbols, and casual-symbol-overlay:1]]
  (use-package casual-symbol-overlay
          :if sacha-laptop-p
          :after embark
          :init
          (with-eval-after-load 'embark
                  (keymap-set embark-symbol-map "z" #'casual-symbol-overlay-tmenu)))
;; Embark, symbols, and casual-symbol-overlay:1 ends here

;; [[file:Sacha.org::#keybindings-embark-embark-and-erefactor-rename-symbol-in-buffer][Embark and erefactor-rename-symbol-in-buffer:2]]
(with-eval-after-load 'embark
  (keymap-set embark-command-map "r" #'sacha-embark-erefactor-rename-symbol-in-buffer)
  (keymap-set embark-symbol-map "r" #'sacha-embark-erefactor-rename-symbol-in-buffer))
;; Embark and erefactor-rename-symbol-in-buffer:2 ends here

;; [[file:Sacha.org::#menus][Menus:1]]
  (define-key-after global-map [menu-bar sacha-menu] (cons "Shortcuts" (make-sparse-keymap "Custom shortcuts")) 'tools)
  (define-key global-map [menu-bar sacha-menu journal] '("Show journal entries" . sacha-show-missing-journal-entries))
  (define-key global-map [menu-bar sacha-menu agenda] '("Org agenda" . (lambda () (interactive) (org-agenda nil "a"))))
  (define-key global-map [menu-bar sacha-menu audio] '("Process audio" . (lambda () (interactive) (shell-command "~/bin/process-audio &"))))
  (define-key global-map [menu-bar sacha-menu new-index-card] '("New index card" . (lambda () (interactive)
                                                                                  (sacha-org-sketch-edit (sacha-prepare-index-card-template)))))
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
  (if sacha-laptop-p
      (use-package hydra-posframe
                          :defer t
                          :if sacha-laptop-p :after hydra
                          :vc (:url "https://github.com/Ladicle/hydra-posframe")
                          ))
;; Hydra keyboard shortcuts:1 ends here

;; [[file:Sacha.org::#hydras][Hydra keyboard shortcuts:2]]
  (with-eval-after-load 'hydra
    (defhydra sacha-window-movement ()
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
      (defhydra sacha-shortcuts (:exit t)
        ("j" sacha-helm-journal "Journal")
        ("C" sacha-resolve-orgzly-syncthing "Conflicts")
        ("n" sacha-capture-timestamped-note "Note")
        ("c" sacha-org-categorize-emacs-news/body "Categorize")
        ("d" sacha-emacs-news-check-duplicates "Dupe")
        ("s" save-buffer "Save")
        ("f" sacha-file-shortcuts/body "File shortcut")
        ("+" text-scale-increase "Increase")
        ("-" text-scale-decrease "Decrease")
                    ("G" gif-screencast-start-or-stop "GIF screencast")
        ("g" sacha-geeqie/body "Geeqie")
        ("r" sacha-record-ffmpeg-toggle-recording "Record screen")
        ("l" (sacha-toggle-or-create "*scratch*" (lambda () (switch-to-buffer (startup--get-buffer-create-scratch)))) "Lisp")
        ("e" eshell-toggle "Eshell")
        ("w" sacha-engine-dmode-hydra/body "Search web")
        ("E" sacha-emacs-news/body "Emacs News"))
            (keymap-global-set "<f5>" #'sacha-shortcuts/body)
      (defhydra sacha-emacs-news (:exit t)
        "Emacs News"
        ("f" (find-file "~/sync/emacs-news/index.org") "News")
        ("C" (find-file "~/proj/emacs-calendar/README.org") "Calendar")
        ("C" (find-file "/ssh:web:/var/www/emacslife.com/calendar/README.org" "Calendar on server"))
        ("d" sacha-emacs-news-check-duplicates "Dupe")
        ("c" sacha-org-categorize-emacs-news/body "Categorize")
        ("h" (sacha-org-update-link-description "HN") "Link HN")
        ("i" (sacha-org-update-link-description "Irreal") "Link Irreal")
        ("m" sacha-share-emacs-news "Mail")
        ("t" (browse-url "https://tweetdeck.twitter.com") "Twitter")))
;; Hydra keyboard shortcuts:3 ends here

;; [[file:Sacha.org::#hydras][Hydra keyboard shortcuts:5]]
(defalias 'sacha-org-insert-link 'sacha-org-insert-link-dwim)
;; Hydra keyboard shortcuts:5 ends here

;; [[file:Sacha.org::#hydra-completion][Emacs Hydra: Allow completion when I can't remember the command name:2]]
     (with-eval-after-load 'hydra
       (define-key hydra-base-map (kbd "<tab>") #'sacha-hydra-execute-extended))
;; Emacs Hydra: Allow completion when I can't remember the command name:2 ends here

;; [[file:Sacha.org::#which-key-and-which-key-posframe][which-key and which-key-posframe:1]]
  (use-package which-key
    :init (which-key-mode 1)
    :config
    (setq which-key-show-prefix 'top))
  (use-package which-key-posframe :if sacha-laptop-p :init (which-key-posframe-mode 1))
;; which-key and which-key-posframe:1 ends here

;; [[file:Sacha.org::#keybindings-casual][Casual:1]]
  (use-package casual
          :load-path "~/vendor/casual/lisp")
;; Casual:1 ends here

;; [[file:Sacha.org::#keybindings-foot-pedal][Foot pedal:2]]
  ;(keymap-global-set "S-<f1>" #'sacha-speechd-repeat-sentence)
  ;(keymap-global-set "S-<f3>" #'sacha-speechd-speak-sentence-and-advance)
;; Foot pedal:2 ends here

;; [[file:Sacha.org::#completion][Completion:1]]
(setq read-extended-command-predicate
      #'command-completion-default-include-p)
;; Completion:1 ends here

;; [[file:Sacha.org::#completion][Completion:2]]
  (global-completion-preview-mode 1)

  (use-package vertico
          :config
          (vertico-mode +1)
          (vertico-multiform-mode)
          (with-eval-after-load 'vertico-multiform
                  (add-to-list 'vertico-multiform-categories '(embark-keybinding grid))))
;; Completion:2 ends here

;; [[file:Sacha.org::#completion][Completion:3]]
  (use-package prescient :config (prescient-persist-mode +1))
                                          ;(use-package company-prescient :init (company-prescient-mode +1))
;; Completion:3 ends here

;; [[file:Sacha.org::#completion-emacs-completion-and-handling-accented-characters-with-orderless][Emacs completion and handling accented characters with orderless:2]]
  (use-package orderless
          :custom
          (completion-styles '(orderless basic))
          (completion-category-overrides '((file (styles basic partial-completion))))
    (orderless-style-dispatchers '(sacha-orderless-accent-dispatch orderless-affix-dispatch)))
;; Emacs completion and handling accented characters with orderless:2 ends here

;; [[file:Sacha.org::#consult][Consult:1]]
  (use-package consult
    :load-path "~/vendor/consult"
    :after projectile
    :bind (("C-x r x" . consult-register)
           ("C-x r b" . consult-bookmark)
           ("C-c k" . consult-kmacro)
           ("C-x M-:" . consult-complex-command) ;; orig. repeat-complet-command
           ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
           ("C-x 5 b" . consult-buffer-other-frame)
           ("M-#" . consult-register-load)
           ("M-'" . consult-register-store) ;; orig. abbrev-prefix-mark (unrelated)
           ("C-M-#" . consult-register)
           ("M-g o" . consult-outline)
           ("M-g h" . consult-org-heading)
           ("M-g a" . consult-org-agenda)
           ("M-g m" . consult-mark)
           ("C-x b" . consult-buffer)
           ("M-g M-g" . consult-goto-line) ;; orig. goto-line
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
           ("M-e" . consult-isearch) ;; orig. isearch-edit-string
           ("M-s e" . consult-isearch) ;; orig. isearch-edit-string
           ("M-s l" . consult-line))
    :init
    (setq register-preview-delay 0
          register-preview-function #'consult-register-format)
    :custom
    consult-preview-key '(:debounce 0.2 any)
    consult-narrow-key "<"
    consult-preview-excluded-files '("\\`/[^/|:]+:"
                                     "\\.gpg\\'")
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
    (setq sacha-consult-source-projectile-projects
          `(:name "Projectile projects"
                  :narrow   ?P
                  :category project
                  :action   ,#'projectile-switch-project-by-name
                  :items    ,projectile-known-projects))
    (add-to-list 'consult-buffer-sources 'sacha-consult-source-projectile-projects 'append))
;; Using projects as a source for consult-buffer:1 ends here

;; [[file:Sacha.org::#searching-sacha-blog][Searching my blog, notes, and sketches with consult-ripgrep and consult-omni:3]]
  (keymap-global-set "M-g b" #'sacha-search-public-notes)
  (keymap-global-set "M-g N" #'sacha-search-notes)
  (keymap-global-set "M-g B" #'consult-omni-google-sacha-blog)
;; Searching my blog, notes, and sketches with consult-ripgrep and consult-omni:3 ends here

;; [[file:Sacha.org::#marginalia][Marginalia:2]]
(use-package marginalia
  :vc (:url "https://github.com/minad/marginalia")
  :init
  (marginalia-mode)
  :bind (:map minibuffer-local-completion-map
              ("M-m" . marginalia-cycle))
  :config
  (add-to-list 'marginalia-prompt-categories '("sketch" . sketch))
  (add-to-list 'marginalia-censor-variables "-api-key")
  (add-to-list 'marginalia-censor-variables "-private")
  (cl-pushnew #'marginalia-annotate-symbol-with-alias
              (alist-get 'command marginalia-annotator-registry))
  (cl-pushnew #'marginalia-annotate-symbol-with-alias
              (alist-get 'function marginalia-annotator-registry))
  (cl-pushnew #'marginalia-annotate-symbol-with-alias
              (alist-get 'symbol marginalia-annotator-registry)))
;; Marginalia:2 ends here

;; [[file:Sacha.org::#cargo-culted-stuff][Cargo-culted stuff:2]]
  (use-package keycast
    :if sacha-laptop-p
    :after embark
          :defer t
    :config (dolist (cmd '(embark-act embark-act-noexit embark-become))
              (advice-add cmd
                          :before #'sacha-force-keycast-update)))

  (use-package
    embark
    :config
                                          ;(setq embark-prompter 'embark-completing-read-prompter)
    (advice-add 'embark-keymap-prompter :filter-return #'sacha-store-action-key+cmd)
    (add-to-list 'embark-target-injection-hooks '(sacha-stream-message embark--allow-edit)))
;; Cargo-culted stuff:2 ends here

;; [[file:Sacha.org::#color-theme-sometimes-comes-across-lists-odd][color-theme sometimes comes across lists. Odd!:1]]
  (defadvice face-attribute (around sacha activate)
    (if (symbolp (ad-get-arg 0))
        ad-do-it))
;; color-theme sometimes comes across lists. Odd!:1 ends here

;; [[file:Sacha.org::#display][Display:2]]
  (keymap-global-set "C-M-8" (lambda () (interactive) (sanityinc/adjust-opacity nil -2)))
  (keymap-global-set "C-M-9" (lambda () (interactive) (sanityinc/adjust-opacity nil 2)))
  (keymap-global-set "C-M-0" (lambda () (interactive) (modify-frame-parameters nil `((alpha . 100)))))
;; Display:2 ends here

;; [[file:Sacha.org::#display][Display:3]]
  (add-to-list 'display-buffer-alist
               '("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
                 (display-buffer-no-window)
                 (allow-no-window . t)))
;; Display:3 ends here

;; [[file:Sacha.org::#set-up-a-light-on-dark-color-scheme][Set up a color scheme:2]]
  (use-package modus-themes
          :vc (:url "https://github.com/protesilaos/modus-themes")
          :init (setq modus-themes-to-toggle '(modus-operandi-tinted modus-vivendi-tinted))
          :config (sacha-setup-color-theme))
;; Set up a color scheme:2 ends here

;; [[file:Sacha.org::#making-highlight-sexp-follow-modus-themes-toggle][Making highlight-sexp follow modus-themes-toggle:2]]
  (use-package highlight-sexp
    :vc (:url "https://github.com/daimrod/highlight-sexp")
    :after modus-themes
    :hook
    ((emacs-lisp-mode . highlight-sexp-mode)
           (modus-themes-after-load-theme . sacha-hl-sexp-update-all-overlays))
          :config
          (advice-add 'hl-sexp-create-overlay :after 'sacha-hl-sexp-update-overlay))
;; Making highlight-sexp follow modus-themes-toggle:2 ends here

;; [[file:Sacha.org::#time-in-the-modeline][Time in the modeline:1]]
  (display-time-mode 1)
;; Time in the modeline:1 ends here

;; [[file:Sacha.org::#diminish][Diminish mode names in modeline:1]]
  (use-package diminish :ensure t)
;; Diminish mode names in modeline:1 ends here

;; [[file:Sacha.org::#highlight-the-active-modeline-using-colours-from-modus-themes][Highlight the active modeline using colours from modus-themes:2]]
  (use-package modus-themes
          :hook
          (modus-themes-after-load-theme . sacha-update-active-mode-line-colors))
;; Highlight the active modeline using colours from modus-themes:2 ends here

;; [[file:Sacha.org::#face-text][Quickly adding face properties to regions:3]]
  (defvar-keymap sacha-face-text-property-mode-map
          "M-o p" #'sacha-add-face-text-property
    "M-o +" #'sacha-face-text-larger
          "M-o -" #'sacha-face-text-smaller)
  (define-minor-mode sacha-face-text-property-mode
    "Make it easy to modify face properties."
    :init-value nil
    (repeat-mode 1))
  (defvar-keymap sacha-face-text-property-mode-repeat-map
          :repeat t
          "+" #'sacha-face-text-larger
          "-" #'sacha-face-text-smaller)
  (dolist (cmd '(sacha-face-text-larger sacha-face-text-smaller))
    (put cmd 'repeat-map 'sacha-face-text-property-mode-repeat-map))
;; Quickly adding face properties to regions:3 ends here

;; [[file:Sacha.org::#navigation][Navigation:1]]
(transient-mark-mode 1)
(keymap-global-set "C-x !" #'delete-other-windows-vertically)
;; Navigation:1 ends here

;; [[file:Sacha.org::*Substitution][Substitution:1]]
(use-package substitute
  :bind ("C-;" . substitute-target-in-buffer))
;; Substitution:1 ends here

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

;; [[file:Sacha.org::#navigation-focus-on-the-current-window][Focus on the current window:2]]
  ;; `prot/window-single-toggle' is based on `windower' by Pierre
  ;; Neidhardt (ambrevar on GitLab)
  (use-package emacs
    :bind (("C-x 1" . prot/window-single-toggle)
           ("s-k" . prot/kill-buffer-current)))
;; Focus on the current window:2 ends here

;; [[file:Sacha.org::#navigation-get-scroll-other-window-to-work-with-pdfs][Get scroll-other-window to work with PDFs:1]]
  (use-package scroll-other-window
          :vc (:url "https://gist.github.com/politza/3f46785742e6e12ba0d1a849f853d0b9")
          :commands sow-mode
          :init (sow-mode 1))
;; Get scroll-other-window to work with PDFs:1 ends here

;; [[file:Sacha.org::#quickly-jump-to-positions][Quickly jump to positions:2]]
(use-package avy
  :if sacha-laptop-p
  :config
  (add-to-list 'avy-dispatch-alist '(?e . avy-action-exchange))
  (setf (alist-get ?. avy-dispatch-alist) 'avy-action-embark)
  :bind
  ("M-j" . avy-goto-char-timer))

(use-package avy-zap
  :if sacha-laptop-p
  :config
  (setq avy-zap-forward-only t)
  (setq avy-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s))
  :bind
  (("M-z" . avy-zap-up-to-char-dwim)
   ("M-Z" . avy-zap-to-char-dwim)))
;; Quickly jump to positions:2 ends here

;; [[file:Sacha.org::#winner-mode-undo-and-redo-window-configuration][Winner mode - undo and redo window configuration:1]]
(use-package winner
  :init
  (winner-mode 1))
;; Winner mode - undo and redo window configuration:1 ends here

;; [[file:Sacha.org::#sort-read-file-name][Sort files in read-file-name:2]]
  (advice-add 'completion-file-name-table :around #'ad-completion-file-name-table)
;; Sort files in read-file-name:2 ends here

;; [[file:Sacha.org::#searching][Searching:1]]
  (setopt isearch-lazy-count t)
;; Searching:1 ends here

;; [[file:Sacha.org::#searching][Searching:3]]
  (use-package helm-org-rifle
    :bind
    ("M-g r r" . helm-org-rifle)
    ("M-g r a" . helm-org-rifle-org-agenda-files)
    ("M-g r o" . helm-org-rifle-org-directory)
    )

  (use-package consult-recoll
    :config
    (setq consult-recoll-search-flags nil)
    :bind
    ("M-s S" . consult-recoll))
;; Searching:3 ends here

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
(add-hook 'ediff-before-setup-hook #'sacha-store-pre-ediff-winconfig)
(add-hook 'ediff-quit-hook #'sacha-restore-pre-ediff-winconfig)
;; Ediff:1 ends here

;; [[file:Sacha.org::#hideshow][Hideshow:1]]
  (use-package hideshow
    :hook
    (prog-mode . hs-minor-mode)
    :bind
    ("C-<tab>" . hs-cycle)
    ("C-<iso-lefttab>" . hs-global-cycle)
    ("C-S-<tab>" . hs-global-cycle))
;; Hideshow:1 ends here

;; [[file:Sacha.org::#pop-to-mark][Pop to mark:1]]
  (bind-key "C-x p" 'pop-to-mark-command)
  (setq set-mark-command-repeat-pop t)
;; Pop to mark:1 ends here

;; [[file:Sacha.org::#helm-swoop-quickly-finding-lines][Helm-swoop - quickly finding lines:1]]
  (use-package helm-swoop
    :if sacha-laptop-p
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

;; [[file:Sacha.org::#highlight-line-mode][Highlight the current line while still being able to easily customize/describe underlying faces:3]]
  (advice-add #'face-at-point :around #'sacha-suggest-other-faces)
;; Highlight the current line while still being able to easily customize/describe underlying faces:3 ends here

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
(setq sacha-file-shortcuts
      '(("C" "~/proj/emacs-calendar/README.org" "Emacs calendar")
        ("e" "~/sync/emacs/Sacha.org" "Config")
        ("E" "~/sync/emacs-news/index.org" "Emacs News")
        ("f" "~/sync/orgzly/journal-fr.org" "French journal")
        ("F" "~/sync/orgzly/french.org" "French")
        ("I" "~/sync/orgzly/computer-inbox.org" "Computer inbox")
        ("i" "~/sync/orgzly/Inbox.org" "Phone inbox")
        ("o" "~/sync/orgzly/organizer.org" "Main org file")
        ("s" "~/proj/stream/index.org" "Yay Emacs")
        ("b" "~/sync/orgzly/business.org" "Business")
        ("P" "/ssh:web:/mnt/prev/home/sacha/planet/data/feeds.json" "Planet Emacsen")
        ("p" "~/sync/orgzly/posts.org" "Posts")
        ("m" "~/sync/web/beginner-map.org" "Map")
        ("n" "~/sync/topics/now.org" "Now")
        ("N" "/ssh:web|sudo::/etc/nginx/sites-available" "Nginx sites")
        ("w" "~/sync/topics/workflows.org" "Workflows")
        ("W" "~/Dropbox/public/sharing/blog.org" "Blog index")
        ("1" "~/proj/static-blog/" "Static blog")
        ("r" "~/sync/orgzly/reference.org" "Reference")
        ("R" "~/personal/reviews.org" "Reviews")
        ("v" "~/proj/emacstv.github.io/videos.org" "Videos")
        ("g" "~/proj/sachac.github.io/evil-plans/index.org" "Evil plans")))

(sacha-navigate-set-up-file-shortcuts)
;; Frequently-accessed files:1 ends here

;; [[file:Sacha.org::#smartscan][Smartscan:1]]
  (use-package smartscan
    :if sacha-laptop-p
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

;; [[file:Sacha.org::#saving-photos][Saving photos:2]]
(bind-key "b" 'sacha-save-photo dired-mode-map)
(bind-key "r" 'sacha-backup-media dired-mode-map)
;; Saving photos:2 ends here

;; [[file:Sacha.org::#move-to-beginning-of-line][Move to beginning of line:2]]
  ;; remap C-a to `smarter-move-beginning-of-line'
  (global-set-key [remap move-beginning-of-line]
                  'sacha-smarter-move-beginning-of-line)
;; Move to beginning of line:2 ends here

;; [[file:Sacha.org::#recent-files][Recent files:1]]
  (require 'recentf)
  (setq recentf-max-saved-items 200
        recentf-max-menu-items 15)
  (recentf-mode)
;; Recent files:1 ends here

;; [[file:Sacha.org::#open-files-externally][Open files externally:2]]
(with-eval-after-load 'org
  (add-to-list 'org-file-apps '("pdf" . "evince %s")))
;; Open files externally:2 ends here

;; [[file:Sacha.org::#toggle][Toggle:2]]
(keymap-global-set
 "C-z"
 (sacha-make-toggle-buffer-function
  sacha-toggle-live
  "~/sync/topics/live.org"))
(keymap-global-set
 "C-S-z"
 (sacha-make-toggle-buffer-function
  sacha-toggle-now
  "~/sync/topics/now.org"))
;; Toggle:2 ends here

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
  (defhydra+ sacha-shortcuts (:exit t)
          ("b" bookmark-jump "Jump to bookmark")
          ("B" bookmark-set-no-overwrite "Set bookmark"))
;; Bookmarks:1 ends here

;; [[file:Sacha.org::#dogears][Dogears:1]]
      (use-package dogears
        ;; These bindings are optional, of course:
        :bind (:map global-map
                    ("M-g d" . dogears-go)
                    ("M-g M-b" . dogears-back)
                    ("M-g M-f" . dogears-forward)
                    ("M-g M-d" . dogears-list)
                    ("M-g M-D" . dogears-sidebar)))
;; Dogears:1 ends here

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

;; [[file:Sacha.org::#reading][Reading:1]]
  (use-package pdf-tools
    :if sacha-laptop-p
    :config
    (pdf-tools-install)
    (setq pdf-view-resize-factor 1.1)
    (setq-default pdf-view-display-size 'fit-page)
          :defer t
    )
;; Reading:1 ends here

;; [[file:Sacha.org::#writing-and-editing][Writing and editing:1]]
  (keymap-global-set "M-c" #'sacha-capitalize-dwim)
  (setq-default fill-column 50)
  (keymap-global-set "M-o" #'join-line)
  (keymap-global-set "M-T" #'transpose-sentences)  ; https://www.matem.unam.mx/~omar/apropos-emacs.html#writing-experience
;; Writing and editing:1 ends here

;; [[file:Sacha.org::#writing-and-editing][Writing and editing:3]]
  ;; Bind it to the original M-c key
  (global-set-key (kbd "M-c") 'sacha-capitalize-dwim)
;; Writing and editing:3 ends here

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

  (defvar-keymap sacha-learn-lang-map
    :prefix t
    "l" (cons "lookup" #'sacha-learn-lang-lexique-complete-word)
    "w" (cons "wordref" #'sacha-learn-lang-wordreference-lookup)
    "c" (cons "conj" #'sacha-learn-lang-conjugate)
    "f" (cons "→ fr" #'sacha-learn-lang-consult-en-fr)
    "s" (cons "say" #'sacha-learn-lang-say-word-at-point)
    "x" (cons "example" #'learn-lang-tatoeba-consult)
    "t" (cons "→ en" #'sacha-learn-lang-translate-dwim))

  (with-eval-after-load 'org
    (keymap-set org-mode-map "C-," 'sacha-learn-lang-map)
    (keymap-set org-mode-map "C-c u" 'sacha-learn-lang-map))

  (with-eval-after-load 'message
    (keymap-set message-mode-map "C-," 'sacha-learn-lang-map)
    )

  (with-eval-after-load 'flyspell
    (keymap-set flyspell-mode-map "C-," 'sacha-learn-lang-map))


  ;; (use-package wiktionary-bro
  ;;   :config
  ;;   (setq wiktionary-bro-language "fr")
  ;;   )

  (use-package flycheck-grammalecte
    :config
    (setq flycheck-grammalecte-report-apos nil)
    (setq flycheck-grammalecte-report-nbsp nil)
    (setq flycheck-grammalecte-report-esp nil)
    (with-eval-after-load 'flycheck
      (flycheck-grammalecte-setup)))
;; Learning French:2 ends here

;; [[file:Sacha.org::#writing-and-editing-learning-french-emacs-and-french-focus-flycheck-grammalecte-on-the-narrowed-part-of-the-buffer][Emacs and French: Focus flycheck-grammalecte on the narrowed part of the buffer:1]]

;; Emacs and French: Focus flycheck-grammalecte on the narrowed part of the buffer:1 ends here

;; [[file:Sacha.org::#writing-and-editing-learning-french-gtts-cli][gtts-cli:1]]
  (use-package learn-lang-tts :load-path "~/proj/learn-lang"
    :config
    (setq learn-lang-tts-kokoro-cli-executable "~/.local/bin/kokoro-tts --model /home/sacha/vendor/kokoro-onnx/kokoro-v1.0.onnx --voices /home/sacha/vendor/kokoro-onnx/voices-v1.0.bin"))
;; gtts-cli:1 ends here

;; [[file:Sacha.org::#writing-and-editing-learning-french-add-shadowing-with-tts-to-subed-record][Add shadowing with tts to subed-record:1]]
  (setq learn-lang-subed-record-reference-dir "~/proj/french/reference/")

  (with-eval-after-load 'subed-record
    (add-hook 'subed-record-finished-hook 'sacha-subed-record-normalize-current))
;; Add shadowing with tts to subed-record:1 ends here

;; [[file:Sacha.org::#gif-screencast][gif-screencast:3]]
  (use-package gif-screencast
          :bind
          ("s-S" . sacha-gif-screencast-start-or-stop-and-choose-thumbnail)
          :config
          (setq gif-screencast-output-directory sacha-recordings-dir))

  (use-package giffy
          :vc (:url "https://github.com/larsmagne/giffy")
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

;; [[file:Sacha.org::#try-redacting][Try redacting:2]]
(with-eval-after-load 'notmuch
  (advice-add
   #'notmuch-show
   :after #'sacha-redact-emails))
;; Try redacting:2 ends here

;; [[file:Sacha.org::#markdown][Markdown:1]]
  (use-package markdown-mode
    :if sacha-laptop-p
    :mode ("\\.\\(njk\\|md\\)\\'" . markdown-mode))
;; Markdown:1 ends here

;; [[file:Sacha.org::#avoiding-weasel-words][Avoiding weasel words:1]]
  (use-package artbollocks-mode
    :if sacha-laptop-p
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

;; [[file:Sacha.org::#writing-and-editing-visual-line][Visual line:1]]
  (global-visual-line-mode)
  (add-hook 'minibuffer-mode-hook (lambda () (visual-line-mode -1)))
;; Visual line:1 ends here

;; [[file:Sacha.org::#unicode][Unicode:1]]
  (defmacro sacha-insert-unicode (unicode-name)
    `(lambda () (interactive)
       (insert-char (cdr (assoc-string ,unicode-name (ucs-names))))))
  (bind-key "C-x 8 s" (sacha-insert-unicode "ZERO WIDTH SPACE"))
  (bind-key "C-x 8 S" (sacha-insert-unicode "SNOWMAN"))
;; Unicode:1 ends here

;; [[file:Sacha.org::#clean-up-spaces][Clean up spaces:1]]
  (bind-key "M-SPC" 'cycle-spacing)
;; Clean up spaces:1 ends here

;; [[file:Sacha.org::#expand][Expand:1]]
  (setq save-abbrevs 'silently)
  (bind-key "M-/" 'hippie-expand)
;; Expand:1 ends here

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

;; [[file:Sacha.org::#speech-recognition][Speech recognition:1]]
  (use-package caser
    :bind
    ("M-D" . caser-dashcase-dwim))
;; Speech recognition:1 ends here

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
  (add-hook 'whisper-after-transcription-hook 'sacha-subed-fix-common-errors-from-start -100)
  :bind
  (("<f9>" . whisper-run)
   ("C-<f9>" . sacha-whisper-run)
   ("S-<f2>" . whisper-run)
   ("S-<f9>" . sacha-whisper-replay)
   ("M-<f9>" . sacha-whisper-toggle-language)))
;; Using whisper.el to convert speech to text and save it to the currently clocked task in Org Mode or elsewhere:1 ends here

;; [[file:Sacha.org::#multimedia-whisper][Using whisper.el to convert speech to text and save it to the currently clocked task in Org Mode or elsewhere:3]]
(with-eval-after-load 'whisper
  (add-hook 'whisper-after-transcription-hook 'sacha-whisper-org-process-reminder 50))
;; Using whisper.el to convert speech to text and save it to the currently clocked task in Org Mode or elsewhere:3 ends here

;; [[file:Sacha.org::#multimedia-whisper][Using whisper.el to convert speech to text and save it to the currently clocked task in Org Mode or elsewhere:5]]
(with-eval-after-load 'whisper
  (add-hook 'whisper-before-transcription-hook #'sacha-whisper-set-temp-filename))
;; Using whisper.el to convert speech to text and save it to the currently clocked task in Org Mode or elsewhere:5 ends here

;; [[file:Sacha.org::whisper-insert-text-at-point-functions][whisper-insert-text-at-point-functions]]
;; Only works with my tweaks to whisper.el
;; https://github.com/sachac/whisper.el/tree/whisper-insert-text-at-point-function
(with-eval-after-load 'whisper
  (setq whisper-insert-text-at-point
        '(sacha-whisper-handle-commands
          sacha-whisper-save-text
          sacha-whisper-save-to-file
          sacha-whisper-maybe-expand-snippet
          sacha-speech-input-quantified-track
          sacha-whisper-maybe-type
          sacha-whisper-maybe-type-with-hints
          sacha-whisper-insert
          sacha-whisper-reset)))
;; whisper-insert-text-at-point-functions ends here

;; [[file:Sacha.org::#multimedia-whisper][Using whisper.el to convert speech to text and save it to the currently clocked task in Org Mode or elsewhere:13]]
(keymap-global-set "<f9>" #'sacha-whisper-run-at-point)
(keymap-global-set "<kp-1>" #'whisper-run)
;; Using whisper.el to convert speech to text and save it to the currently clocked task in Org Mode or elsewhere:13 ends here

;; [[file:Sacha.org::#multimedia-whisper][Using whisper.el to convert speech to text and save it to the currently clocked task in Org Mode or elsewhere:15]]
(with-eval-after-load 'org
  (add-hook 'org-clock-in-hook #'sacha-whisper-org-clear-saved-annotation))
;; Using whisper.el to convert speech to text and save it to the currently clocked task in Org Mode or elsewhere:15 ends here

;; [[file:Sacha.org::#writing-and-editing-speech-recognition-emacs-and-whisper-el-trying-out-different-speech-to-text-backends-and-models][Emacs and whisper.el: Trying out different speech-to-text backends and models:2]]
(with-eval-after-load 'whisper
  (advice-add 'whisper--transcribe-via-local-server :override #'sacha-whisper--transcribe-via-local-server)
  (advice-add 'whisper--check-model-consistency :override #'sacha-whisper--check-model-consistency)
  (advice-add 'whisper--ensure-server :override #'speech-input-speaches-server-start)
  )
;; Emacs and whisper.el: Trying out different speech-to-text backends and models:2 ends here

;; [[file:Sacha.org::#writing-and-editing-speech-recognition-emacs-and-whisper-el-trying-out-different-speech-to-text-backends-and-models][Emacs and whisper.el: Trying out different speech-to-text backends and models:7]]
(setq whisper-server-port 8000
      whisper-model "Systran/faster-whisper-small.en"
      sacha-whisper-url-format "http://%s:%d/v1/audio/transcriptions")
;; Emacs and whisper.el: Trying out different speech-to-text backends and models:7 ends here

;; [[file:Sacha.org::#writing-and-editing-speech-recognition-queue-multiple-transcriptions-with-whisper-el-speech-recognition][Queuing multiple transcriptions with whisper.el speech recognition:2]]
(keymap-global-set "<kp-9>" #'sacha-whisper-continue)
(keymap-global-set "<kp-8>" #'sacha-whisper-discard-and-continue)
(keymap-global-set "C-<kp-9>" #'sacha-whisper-done)
;; Queuing multiple transcriptions with whisper.el speech recognition:2 ends here

;; [[file:Sacha.org::#writing-and-editing-speech-recognition-using-silero-voice-activity-detection-to-automatically-queue-multiple-transcriptions-with-natrys-whisper-el][Using Silero voice activity detection to automatically queue multiple transcriptions with natrys/whisper.el:1]]
(use-package speech-input
  :load-path "~/proj/speech-input/"
  :preface (load "~/proj/speech-input/speech-input-autoloads.el" nil t)
  )
;; Using Silero voice activity detection to automatically queue multiple transcriptions with natrys/whisper.el:1 ends here

;; [[file:Sacha.org::#writing-and-editing-speech-recognition-using-silero-voice-activity-detection-to-automatically-queue-multiple-transcriptions-with-natrys-whisper-el][Using Silero voice activity detection to automatically queue multiple transcriptions with natrys/whisper.el:2]]
(with-eval-after-load 'speech-input-vad
  (add-hook 'speech-input-vad-on-end-functions #'sacha-whisper-maybe-continue))
;; Using Silero voice activity detection to automatically queue multiple transcriptions with natrys/whisper.el:2 ends here

;; [[file:Sacha.org::#writing-and-editing-speech-recognition-slowly-building-speech-based-commands-for-emacs][Slowly building speech-based commands for Emacs:2]]
(with-eval-after-load 'whisper
  (add-hook 'whisper-after-transcription-hook 'sacha-whisper-process-replacements 70))
;; Slowly building speech-based commands for Emacs:2 ends here

;; [[file:Sacha.org::#writing-and-editing-speech-recognition-using-speech-recognition-for-translations-in-emacs-and-faking-in-buffer-completion-for-the-results][Using speech recognition for on-the-fly translations in Emacs and faking in-buffer completion for the results:3]]
(with-eval-after-load 'whisper
  (add-hook 'whisper-after-transcription-hook 'sacha-whisper-translate 70))
;; Using speech recognition for on-the-fly translations in Emacs and faking in-buffer completion for the results:3 ends here

;; [[file:Sacha.org::#utf-8][UTF-8:1]]
(prefer-coding-system 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))
;; UTF-8:1 ends here

;; [[file:Sacha.org::#writing-and-editing-denote][Denote:1]]
(use-package denote
	:config
	(setopt denote-directory "~/sync/Notes")
)
;; Denote:1 ends here

;; [[file:Sacha.org::org-package-setup][org-package-setup]]
(defvar sacha-org-inbox-file "~/sync/orgzly/Inbox.org")
(use-package org
  :load-path ("~/vendor/org-mode/lisp" "~/vendor/org-mode/contrib/lisp")
  :preface (load "~/vendor/org-mode/lisp/org-loaddefs.el" nil t)
  :bind
  (:map org-mode-map
        ("C-M-<return>" . org-insert-subheading)
        ("M-." . sacha-org-defun-open))
	:custom
	(org-export-with-sub-superscripts nil)
	(org-footnote-section nil)
	(org-fold-catch-invisible-edits 'smart))
;; org-package-setup ends here

;; [[file:Sacha.org::#org-mode-automatically-continue-lists][Automatically continue lists:2]]
(use-package org-autolist
  :hook
  ((org-mode . org-autolist-mode)
   (org-metareturn . sacha-org-autolist-allow-newlines)))
;; Automatically continue lists:2 ends here

;; [[file:Sacha.org::#org-mode-pdfs][PDFs:1]]
(use-package org-pdftools
  :hook (org-mode . org-pdftools-setup-link))
;; PDFs:1 ends here

;; [[file:Sacha.org::#org-mode-after-i-jump-to-a-task-from-org-clock-goto-narrow-to-it-automatically][After I jump to a task from org-clock-goto, narrow to it automatically:1]]
(with-eval-after-load 'org
  (add-hook 'org-clock-goto-hook #'org-narrow-to-subtree)
  (add-hook 'org-agenda-after-show-hook #'org-narrow-to-subtree))
;; After I jump to a task from org-clock-goto, narrow to it automatically:1 ends here

;; [[file:Sacha.org::#org-refile-insert-link][Insert a link to an Org Mode heading from an org-refile prompt:2]]
(with-eval-after-load 'marginalia
	(add-to-list 'marginalia-prompt-categories '("Goto\\|Refile" . sacha-org-path)))
(with-eval-after-load 'embark
	(add-to-list 'embark-keymap-alist '(sacha-org-path . sacha-org-path-map)))
;; Insert a link to an Org Mode heading from an org-refile prompt:2 ends here

;; [[file:Sacha.org::#org-refile-insert-link][Insert a link to an Org Mode heading from an org-refile prompt:3]]
(with-eval-after-load 'consult-org
	(keymap-set embark-org-heading-map "L" #'embark-org-insert-link-to))
;; Insert a link to an Org Mode heading from an org-refile prompt:3 ends here

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
  (bind-key "C-c d" 'sacha-org-move-line-to-destination org-mode-map)
  (bind-key "C-c t s"  'sacha-split-sentence-and-capitalize org-mode-map)
  (bind-key "C-c t -"  'sacha-split-sentence-delete-word-and-capitalize org-mode-map)
  (bind-key "C-c t d"  'sacha-delete-word-and-capitalize org-mode-map)

  (bind-key "C-c C-p C-p" 'sacha-org-publish-maybe org-mode-map)
  (bind-key "C-c C-r" 'sacha-org-refile-and-jump org-mode-map))
;; Keyboard shortcuts:2 ends here

;; [[file:Sacha.org::#keyboard-shortcuts][Keyboard shortcuts:3]]
(with-eval-after-load 'org-agenda
  (bind-key "i" 'org-agenda-clock-in org-agenda-mode-map))
;; Keyboard shortcuts:3 ends here

;; [[file:Sacha.org::#org-mode-keyboard-shortcuts-speed-commands-org-mode-cutting-the-current-list-item-including-nested-lists-with-a-speed-command][Org Mode: Cutting the current list item (including nested lists) with a speed command:2]]
(setq org-use-speed-commands 'sacha-org-use-speed-commands-for-headings-and-lists)
;; Org Mode: Cutting the current list item (including nested lists) with a speed command:2 ends here

;; [[file:Sacha.org::#org-mode-keyboard-shortcuts-speed-commands-org-mode-cutting-the-current-list-item-including-nested-lists-with-a-speed-command][Org Mode: Cutting the current list item (including nested lists) with a speed command:4]]
(with-eval-after-load 'org
	(setf (alist-get "k" org-speed-commands nil nil #'string=)
				#'sacha-org-cut-subtree-or-list-item))
;; Org Mode: Cutting the current list item (including nested lists) with a speed command:4 ends here

;; [[file:Sacha.org::#org-mode-keyboard-shortcuts-other-speed-commands][Other speed commands:1]]
(setq org-use-effective-time t)
;; Other speed commands:1 ends here

;; [[file:Sacha.org::#org-mode-keyboard-shortcuts-other-speed-commands][Other speed commands:4]]
(with-eval-after-load 'org
  (let ((listvar (if (boundp 'org-speed-commands) 'org-speed-commands
                   'org-speed-commands-user)))
    (add-to-list listvar '("A" org-archive-subtree-default))
    (add-to-list listvar '("x" org-todo "DONE"))
    (add-to-list listvar '("X" call-interactively 'sacha-org-mark-done-and-add-to-journal))
    (add-to-list listvar '("y" org-todo-yesterday "DONE"))
    (add-to-list listvar '("!" sacha-org-clock-in-and-track))
    (add-to-list listvar '("s" call-interactively 'org-schedule))
    (add-to-list listvar '("d" sacha-org-move-line-to-destination))
    (add-to-list listvar '("i" call-interactively 'org-clock-in))
    (add-to-list listvar '("o" call-interactively 'org-clock-out))
    (add-to-list listvar '("$" call-interactively 'org-archive-subtree)))
  (bind-key "!" 'sacha-org-clock-in-and-track org-agenda-mode-map))
;; Other speed commands:4 ends here

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

;; [[file:Sacha.org::#viewing-navigating-and-editing-the-org-tree][Viewing, navigating, and editing the Org tree:1]]
(with-eval-after-load 'org
  (bind-key "C-c k" 'org-cut-subtree org-mode-map)
  (setq org-yank-adjusted-subtrees t))
;; Viewing, navigating, and editing the Org tree:1 ends here

;; [[file:Sacha.org::#finding-sacha-place-on-a-small-mobile-screen-with-org-back-to-heading][Finding my place on a small mobile screen with org-back-to-heading:1]]
(use-package org
  :bind (:map org-mode-map
              ("C-c b" . outline-previous-heading)
              ("C-c p" . org-display-outline-path)))
;; Finding my place on a small mobile screen with org-back-to-heading:1 ends here

;; [[file:Sacha.org::#taking-notes][Taking notes:1]]
(setq org-directory "~/sync/orgzly/")
(setq org-default-notes-file "~/sync/orgzly/organizer.org")
;; Taking notes:1 ends here

;; [[file:Sacha.org::#templates][Templates:2]]
(defvar sacha-org-basic-task-template "* TODO %^{Task}
         :PROPERTIES:
         :Effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}
         :END:
         Captured %<%Y-%m-%d %H:%M>
         %?

         %i
         " "Basic task data")
(defvar sacha-ledger-file "~/cloud/ledger/current.ledger")
(with-eval-after-load 'org-capture
	(setq org-capture-templates
				(seq-uniq
				 (append

      `(("r" "Note" entry
         (file ,sacha-org-inbox-file)
         "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n%i\n\n- %a"
         :prepend t)
				("t" "Task with annotation" entry
         (file ,sacha-org-inbox-file)
         "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n%a\n"
         :prepend t)
        ("i" "Interrupting task" entry
         (file ,sacha-org-inbox-file)
         "* STARTED %^{Task}\n:PROPERTIES:\n:CREATED: %U\n:END:\n%a\n"
         :clock-in :clock-resume
         :prepend t)
				("T" "Task without annotation" entry
         (file ,sacha-org-inbox-file)
         "* TODO %^{Task}\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n"
         :prepend t)
        ;; From https://takeonrules.com/2022/10/16/adding-another-function-to-sacha-workflow/
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
        ;;  "%(let ((last (sacha-clippy-last))) (format \"| %s | %s |\" (car last) (cdr last)))"
        ;;  :immediate-finish t)

        ("." "Today" entry
         (file ,sacha-org-inbox-file)
         "* TODO %^{Task}\nSCHEDULED: %t\n:PROPERTIES:\n:CREATED: %U\n:END:\n"
         :immediate-finish t)
        ("v" "Video" entry
         (file ,sacha-org-inbox-file)
         "* TODO %^{Task}  :video:\nSCHEDULED: %t\n"
         :immediate-finish t)
        ("e" "Errand" entry
         (file ,sacha-org-inbox-file)
         "* TODO %^{Task}  :errands:\n:PROPERTIES:\n:CREATED: %U\n:END:\n"
         :immediate-finish t)
        ("n" "Note" entry
         (file ,sacha-org-inbox-file)
         "* %^{Note}\n:PROPERTIES:\n:CREATED: %U\n:END:\n"
         :immediate-finish t)
        ("N" "Note" entry
         (file ,sacha-org-inbox-file)
         "* %^{Note}\n:PROPERTIES:\n:CREATED: %U\n:END:\n"
         :prepend t)
				("s" "Selection from browser" entry
				 (file ,sacha-org-inbox-file)
				 "* %a :website:\n:PROPERTIES:\n:CREATED: %U\n:END:\n#+begin_quote\n%i\n#+end_quote\n\n%?\n"
				 :prepend t)
				("S" "Screenshot" entry
				 (file ,sacha-org-inbox-file)
				 "* %^{Note}\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n#+CAPTION: %(file-name-nondirectory (sacha-latest-screenshot))\n[[file:%(sacha-latest-screenshot)]]\n"
				 :prepend t)
        ("b" "Business task" entry
         (file+headline "~/personal/business.org" "Tasks")
         ,sacha-org-basic-task-template)
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
         (file ,sacha-ledger-file)
         "%(ledger-read-date \"Date: \") * %^{Payee}
             Expenses:Cash
             Expenses:%^{Account}  %^{Amount}
           ")
        ("lb" "BDO CAD" plain
         (file ,sacha-ledger-file)
         "%(ledger-read-date \"Date: \") * %^{Payee}
             Expenses:Play    $ %^{Amount}
             Assets:BDO
           ")
        ("lp" "BDO PHP" plain
         (file ,sacha-ledger-file)
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
:EMAIL: %(sacha-org-contacts-template-email)
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
;; Templates:2 ends here

;; [[file:Sacha.org::#allow-refiling-in-the-middle-ish-of-a-capture][Allow refiling in the middle(ish) of a capture:2]]
(eval-after-load 'org-capture
  '(bind-key "C-c C-r" 'sacha-org-refile-and-jump org-capture-mode-map))
;; Allow refiling in the middle(ish) of a capture:2 ends here

;; [[file:Sacha.org::#try-out-this-capture-command][Try out this capture command:1]]
(use-package git-link :defer t)
(bind-key "C-c c" 'jf/capture-region-contents-with-metadata)
;; Try out this capture command:1 ends here

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

;; [[file:Sacha.org::#sacha-org-todo-set-keyword-faces][Change Org Mode TODO keyword color based on the state and the current Modus theme:2]]
(with-eval-after-load 'modus-themes
	(add-hook 'modus-themes-after-load-theme-hook #'sacha-org-todo-set-keyword-faces))
;; Change Org Mode TODO keyword color based on the state and the current Modus theme:2 ends here

;; [[file:Sacha.org::#projects][Projects:1]]
(setq org-tags-exclude-from-inheritance '("project" "inboxtarget"))
;; Projects:1 ends here

;; [[file:Sacha.org::#projects][Projects:2]]
(with-eval-after-load 'org
  (let ((listvar (if (boundp 'org-speed-commands) 'org-speed-commands
                   'org-speed-commands-user)))
    (add-to-list listvar '("N" org-narrow-to-subtree))
    (add-to-list listvar '("W" widen))
    (add-to-list listvar '("T" sacha-org-agenda-for-subtree))
    (add-to-list listvar '("b" sacha-org-bounce-to-file))))

;; Projects:2 ends here

;; [[file:Sacha.org::#projects][Projects:4]]
(with-eval-after-load 'org
  (let ((listvar (if (boundp 'org-speed-commands) 'org-speed-commands
                   'org-speed-commands-user)))
    (add-to-list listvar '("S" call-interactively 'org-sort))))
;; Projects:4 ends here

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
          'sacha-org-mode-ask-effort)
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

;; [[file:Sacha.org::#structure-templates][Structure templates:3]]
(eval-after-load 'ox
  '(add-to-list 'org-export-filter-special-block-functions 'sacha-org-html-quote2))
;; Structure templates:3 ends here

;; [[file:Sacha.org::#project_subtasks][Basic configuration:1]]
(defvar sacha-kid-org-file nil "Defined in secrets")
(setq sacha-org-agenda-files
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
                      ,sacha-kid-org-file
                      "~/personal/orgzly.org"
                      "~/personal/calendar.org"
                      "~/Dropbox/tasker/summary.txt"
                      "~/Dropbox/public/sharing/index.org"
                      "~/dropbox/public/sharing/learning.org"
                      "~/proj/emacs-notes/tasks.org"
                      "~/proj/sachac.github.io/evil-plans/index.org"
                      "~/sync/orgzly/cooking.org"
                      "~/sync/orgzly/routines.org"))))
(setq org-agenda-files sacha-org-agenda-files)
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

;; [[file:Sacha.org::#starting-sacha-weeks-on-saturday][Starting my weeks on Saturday:1]]
(setq org-agenda-start-on-weekday 6)
;; Starting my weeks on Saturday:1 ends here

;; [[file:Sacha.org::#org-agenda-custom-commands][Org agenda custom commands:1]]
  (bind-key "<apps> a" 'org-agenda)
  (setq sacha-org-agenda-contexts
        '((tags-todo "phone")
  	(tags-todo "work")
  	(tags-todo "drawing")
  	(tags-todo "coding")
  	(tags-todo "writing")
  	(tags-todo "computer")
  	(tags-todo "home")
  	(tags-todo "errands")))
;; Org agenda custom commands:1 ends here

;; [[file:Sacha.org::#org-agenda-custom-commands][Org agenda custom commands:3]]
(use-package org-super-agenda
	:init
	(org-super-agenda-mode 1))
(use-package org-ql)

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
          ;;            ((org-agenda-skip-function 'sacha-org-agenda-skip-scheduled)
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
          (org-agenda-cmp-user-defined 'sacha-org-sort-agenda-items-todo)
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
         ,sacha-org-agenda-contexts
         ((org-agenda-sorting-strategy '(priority-up effort-down))
          (org-agenda-max-entries 3)))
        ("C" "All by context"
         ,sacha-org-agenda-contexts
         ((org-agenda-sorting-strategy '(priority-down effort-down))
          (org-agenda-max-entries nil)))
        ("9" "Unscheduled top 3 by context"
         ,sacha-org-agenda-contexts
         ((org-agenda-skip-function 'sacha-org-agenda-skip-scheduled)
          (org-agenda-sorting-strategy '(priority-down effort-down))
          (org-agenda-max-entries 3)))
        ("(" "All unscheduled by context"
         ,sacha-org-agenda-contexts
         ((org-agenda-skip-function 'sacha-org-agenda-skip-scheduled)
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
         ((org-agenda-skip-function 'sacha-org-agenda-skip-scheduled)
          (org-agenda-view-columns-initially nil)
          (org-tags-exclude-from-inheritance '("project"))
          (org-agenda-overriding-header "Unscheduled TODO entries: ")
          (org-columns-default-format "%50ITEM %TODO %3PRIORITY %Effort{:} %TAGS")
          (org-agenda-sorting-strategy '(todo-state-up priority-down effort-up tag-up category-keep))))
        ("!" "Someday" tags-todo "TODO=\"SOMEDAY\""
         ((org-agenda-skip-function 'sacha-org-agenda-skip-scheduled)
          (org-agenda-view-columns-initially nil)
          (org-tags-exclude-from-inheritance '("project"))
          (org-agenda-overriding-header "Someday: ")
          (org-columns-default-format "%50ITEM %TODO %3PRIORITY %Effort{:} %TAGS")
          (org-agenda-sorting-strategy '(todo-state-up priority-down effort-up tag-up category-keep))))
        ("U" "Unscheduled tasks outside projects" tags-todo "-project-cooking-routine"
         ((org-agenda-skip-function 'sacha-org-agenda-skip-scheduled)
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
        ("8" "List projects with tasks" sacha-org-agenda-projects-and-tasks
         "+PROJECT"
         ((org-agenda-max-entries 3)))))
;; Org agenda custom commands:3 ends here

;; [[file:Sacha.org::#making-it-easier-to-tag-inbox-items][Making it easier to tag inbox items:1]]
(setq org-complete-tags-always-offer-all-agenda-tags t)
(setq org-use-fast-tag-selection nil)
;; Making it easier to tag inbox items:1 ends here

;; [[file:Sacha.org::#make-it-easy-to-mark-a-task-as-done-and-create-a-follow-up-task][Make it easy to mark a task as done and create a follow-up task:2]]
;; Override the key definition
(define-key org-agenda-mode-map "F" 'sacha-org-agenda-mark-done-and-add-followup)
;; Make it easy to mark a task as done and create a follow-up task:2 ends here

;; [[file:Sacha.org::#capture-something-based-on-the-agenda][Capture something based on the agenda:2]]

;; New key assignment
(define-key org-agenda-mode-map "N" 'sacha-org-agenda-new)
;; Capture something based on the agenda:2 ends here

;; [[file:Sacha.org::#sorting-by-date-and-priority][Sorting by date and priority:1]]
(setq org-agenda-sorting-strategy
      '((agenda time-up priority-down tag-up category-keep)
        ;; (todo user-defined-up todo-state-up priority-down effort-up)
        (todo todo-state-up priority-down effort-up)
;        (tags user-defined-up)
        (search category-keep)))
(setq org-agenda-cmp-user-defined 'sacha-org-sort-agenda-items-user-defined)
;; Sorting by date and priority:1 ends here

;; [[file:Sacha.org::#preventing-things-from-falling-through-the-cracks][Preventing things from falling through the cracks:2]]
(setq org-stuck-projects
      '("+PROJECT-MAYBE-DONE"
        ("TODO")
        nil
        "\\<IGNORE\\>"))
;; Preventing things from falling through the cracks:2 ends here

;; [[file:Sacha.org::#weekly-review][Weekly review:1]]
(use-package quantified :ensure nil :load-path "~/proj/quantified/lisp" :unless sacha-phone-p)
;; Weekly review:1 ends here

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

;; [[file:Sacha.org::#refile-inbox][Refile inbox entries to a smaller set of org-refile-targets:2]]
(keymap-global-set "C-c w" 'sacha-org-refile-to-target-or-subset)
;; Refile inbox entries to a smaller set of org-refile-targets:2 ends here

;; [[file:Sacha.org::#refile-tags][Automatically refiling Org Mode headings based on tags:2]]
(setq sacha-org-tag-target-files
		  (append '("~/sync/orgzly/news.org"
							  "~/sync/orgzly/resources.org"
							  "~/proj/stream/index.org")
						  org-agenda-files))
;; Automatically refiling Org Mode headings based on tags:2 ends here

;; [[file:Sacha.org::#quickly-refiling-org-mode-notes-to-headings-in-the-same-file][Refiling Org Mode notes to headings in the same file:2]]
(with-eval-after-load 'org
  (push '("w" call-interactively 'org-refile) org-speed-commands)
  (push '("W" call-interactively 'sacha-org-refile-in-file) org-speed-commands)
  (push '("." call-interactively 'sacha-org-refile-to-previous) org-speed-commands))
;; Refiling Org Mode notes to headings in the same file:2 ends here

;; [[file:Sacha.org::#org-contacts][Contacts:1]]
(use-package org-contacts
	:commands org-contacts-filter
	:config
	(setq org-contacts-files '("~/sync/orgzly/people.org" "~/proj/emacsconf/2025/private/conf.org"))
	:hook
	(message-setup . sacha-message-greet-contacts))
(with-eval-after-load 'emacsconf-mail
	(advice-add #'emacsconf-mail-prepare :around #'sacha-message-greet-contacts-skip))
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
 (keymap-set embark-org-link-map "s" #'sacha-org-bookmark-save-link))

(with-eval-after-load 'org
	(org-link-set-parameters
	 "bookmark"
	 :complete #'sacha-org-bookmark-complete
	 :insert-description #'sacha-org-link-insert-description))
;; Bookmarks:2 ends here

;; [[file:Sacha.org::#org-babel][Org Babel:1]]
(setq org-edit-src-auto-save-idle-delay 5)
;; Org Babel:1 ends here

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

;; [[file:Sacha.org::#org-mode-org-babel-tangling-sacha-emacs-config-snippets-to-different-files-and-adding-boilerplate][Tangle Emacs config snippets to different files and add boilerplate:4]]
(setq sacha-emacs-config-url "https://sachachua.com/dotemacs")
(with-eval-after-load 'org
  (add-hook 'org-babel-pre-tangle-hook #'sacha-emacs-config-prepare-to-tangle)
  (add-hook 'org-babel-post-tangle-hook #'sacha-org-babel-post-tangle-insert-boilerplate-for-sacha-lisp))
;; Tangle Emacs config snippets to different files and add boilerplate:4 ends here

;; [[file:Sacha.org::#format-source][Format source:2]]
(use-package format-all :if sacha-laptop-p :defer t)
(with-eval-after-load 'org
  (advice-add #'org-edit-src-exit :before #'sacha-format-all-advice))
;; Format source:2 ends here

;; [[file:Sacha.org::#json][JSON:2]]
(defalias 'org-babel-execute:json #'sacha-org-babel-execute:json)
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
(use-package literate-elisp :if sacha-laptop-p :defer t)
;; Let's try literate-elisp:1 ends here

;; [[file:Sacha.org::#org-mode-publishing-changing-org-mode-underlines-to-the-html-mark-element][Changing Org Mode underlines to the HTML mark element:1]]
(with-eval-after-load 'ox-html
	(setf (alist-get 'underline org-html-text-markup-alist)
				"<mark>%s</mark>"))
;; Changing Org Mode underlines to the HTML mark element:1 ends here

;; [[file:Sacha.org::#org-mode-publishing-changing-org-mode-underlines-to-the-html-mark-element][Changing Org Mode underlines to the HTML mark element:3]]
(with-eval-after-load 'org
	(org-link-set-parameters "hl" :export 'sacha-org-highlight-export))
;; Changing Org Mode underlines to the HTML mark element:3 ends here

;; [[file:Sacha.org::#org-mode-publishing-html-export-html-copy-files-and-serve-via-simple-httpd][Org Mode: Export HTML, copy files, and serve the results via simple-httpd so that media files work:1]]
(use-package simple-httpd
  :config
  (setq httpd-root (make-temp-file "httpd" t))
  :hook
  (httpd-stop . sacha-simple-httpd-remove-temporary-root)
  (kill-emacs . httpd-stop))
;; Org Mode: Export HTML, copy files, and serve the results via simple-httpd so that media files work:1 ends here

;; [[file:Sacha.org::#org-mode-publishing-html-export-html-copy-files-and-serve-via-simple-httpd][Org Mode: Export HTML, copy files, and serve the results via simple-httpd so that media files work:3]]
(with-eval-after-load 'ox
  (org-export-define-derived-backend 'sacha-html-served 'html
    :menu-entry
    '(?s "Export to HTML and Serve"
         ((?b "Buffer"  sacha-org-serve-buffer)
          (?s "Subtree" sacha-org-serve-subtree)))))
;; Org Mode: Export HTML, copy files, and serve the results via simple-httpd so that media files work:3 ends here

;; [[file:Sacha.org::#11ty][11ty static site generation:1]]
(use-package ox-11ty
  :if sacha-laptop-p
  :load-path "~/proj/ox-11ty"
	:config
	(setq org-html-toplevel-hlevel 3)
	(advice-add 'org-11ty--front-matter :filter-return #'sacha-org-11ty-rewrite-tags))
;; 11ty static site generation:1 ends here

;; [[file:Sacha.org::#org-mode-publishing-11ty-static-site-generation-linking-to-blog-topics][Linking to blog topics:2]]
(with-eval-after-load 'org
  (org-link-set-parameters
	 "topic"
	 :follow #'sacha-org-topic-open
	 :store #'sacha-org-topic-store
	 :insert-description #'sacha-org-link-insert-description
	 :export #'sacha-org-topic-export
	 :complete #'sacha-org-topic-complete))
;; Linking to blog topics:2 ends here

;; [[file:Sacha.org::#linking-to-blog-posts][Linking to blog posts:2]]
(with-eval-after-load 'org
	(org-link-set-parameters
	 "blog"
	 :follow #'sacha-org-blog-open
	 :store #'sacha-org-blog-store
	 :insert-description #'sacha-org-link-insert-description
	 :export #'sacha-org-blog-export
	 :complete #'sacha-org-blog-complete))
;; Linking to blog posts:2 ends here

;; [[file:Sacha.org::#org-mode-publishing-11ty-static-site-generation-linking-to-blog-posts-making-it-easier-to-add-a-category-to-a-blog-post][Making it easier to add a category to a blog post:3]]
  (with-eval-after-load 'embark
    (add-to-list 'embark-target-finders #'sacha-embark-org-blog-target)
    (defvar-keymap embark-sacha-blog-actions
      :parent embark-general-map
  		:doc "Shortcuts for my blog"
  		"h" #'sacha-blog-edit-html
  		"j" #'sacha-blog-edit-json
  		"e" #'sacha-blog-find-org
  		"c" #'sacha-embark-org-blog-add-category
      "i" #'sacha-embark-blog-insert-link
  		"b" #'sacha-embark-org-blog-open-in-browser)
    (add-to-list 'embark-keymap-alist '(sacha-blog . embark-sacha-blog-actions)))
;; Making it easier to add a category to a blog post:3 ends here

;; [[file:Sacha.org::#embark-11ty][embark-11ty:2]]
(with-eval-after-load 'embark
	(define-key embark-url-map "v" #'sacha-blog-find-org)
	(define-key embark-org-link-map "v" #'sacha-blog-find-org))
;; embark-11ty:2 ends here

;; [[file:Sacha.org::#moving-sacha-org-post-subtree-to-the-11ty-directory][Moving my Org post subtree to the 11ty directory:3]]
(with-eval-after-load 'ox-11ty
  (add-to-list 'org-11ty-process-export-functions #'sacha-org-export-filter-body-add-index-link))
;; Moving my Org post subtree to the 11ty directory:3 ends here

;; [[file:Sacha.org::#moving-sacha-org-post-subtree-to-the-11ty-directory][Moving my Org post subtree to the 11ty directory:5]]
(with-eval-after-load 'ox-11ty
  ;; Only on my computer
	(map-put (caddr (org-export-backend-menu (org-export-get-backend '11ty)))
					 ?c (list "To Org, 11tydata.json, HTML" 'sacha-org-11ty-export))
  (map-put (caddr (org-export-backend-menu (org-export-get-backend '11ty)))
					 ?1 (list "...and copy to site" 'sacha-org-11ty-export-and-copy))
  )
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
	(add-hook 'org-11ty-front-matter-functions #'sacha-org-11ty-add-mastodon-to-front-matter))
;; Include Mastodon, HN, Reddit fields in front matter:2 ends here

;; [[file:Sacha.org::org-sacha-include-link][org-sacha-include-link]]
(org-link-set-parameters
 "sacha-include"
 :follow #'sacha-include-open
 :store #'sacha-include-store
 :export #'sacha-include-export
 :complete #'sacha-include-complete)
;; org-sacha-include-link ends here

;; [[file:Sacha.org::#ox-epub][ox-epub:1]]
(use-package ox-epub
  :if sacha-laptop-p
	:defer t
  :config
	(setq org-epub-style-default
        (concat org-epub-style-default "\n  p.sacha-verse { white-space: pre }\n")))

;; ox-epub:1 ends here

;; [[file:Sacha.org::#config-footer][Add a note to the bottom of blog posts exported from my config file:2]]
(with-eval-after-load 'ox
  (add-to-list 'org-export-filter-body-functions #'sacha-org-export-filter-body-add-emacs-configuration-link))
;; Add a note to the bottom of blog posts exported from my config file:2 ends here

;; [[file:Sacha.org::#copy-linked-file-and-change-link][Copy linked file and change link:2]]
(with-eval-after-load 'embark-org
	(keymap-set embark-org-link-map "r l" #'sacha-embark-org-copy-linked-file-and-change-link))
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
         :publishing-function sacha-org-html-publish-to-html-trustingly
         )
        ("book-notes"
         :base-directory "c:/sacha/Dropbox/books"
         :publishing-directory "c:/sacha/Dropbox/books/html"
         :publishing-function sacha-org-html-publish-to-html-trustingly
         :makeindex t)
				("topics"
				 :base-directory "~/sync/topics"
				 :publishing-directory "/tmp/topics"
				 :publishing-function sacha-org-11ty-publish-from-project)))
;; org-clean-up-export ends here

;; [[file:Sacha.org::#cleaning-up-export][Cleaning up export:5]]
(bind-key "<apps> b" 'sacha-org-publish-and-browse)
;; Cleaning up export:5 ends here

;; [[file:Sacha.org::org-special-blocks][org-special-blocks]]
(use-package org-special-block-extras
  :if sacha-laptop-p
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
																							 (sacha-org-yt-id yt))))
											(video-link (and video
																			 (format "<a href=\"%s\">download the video</a>"
																							 (org-export-file-uri video))))
											(audio-link (and audio
																			 (format "<a href=\"%s\">download the audio</a>"
																							 (org-export-file-uri audio)))))
									(concat
									 "<div class=\"row\"><div class=\"columns\"><div style=\"width: 400px\">"
									 (if video
											 (sacha-org-video-export (concat "video:" (expand-file-name video) "?thumbnail=" (or thumbnail ""))
																						nil backend nil)
										 (sacha-org-yt-export yt nil backend nil))
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
	 'src-block 'sacha-org-html-src-block))
(with-eval-after-load 'ox-11ty
	(map-put!
	 (org-export-backend-transcoders (org-export-get-backend '11ty))
	 'src-block 'sacha-org-11ty-src-block))
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
  (org-export-define-derived-backend 'sacha-plain-text 'ascii
    :translate-alist '((link . sacha-plain-text-link)
                       (item . sacha-plain-text-item))
    :menu-entry '(?p "Export to custom plain text"
                     ((?p "As plain text buffer" sacha-plain-text-export-to-buffer)
                      (?P "As plain text file" sacha-plain-text-export-to-file))))
  (add-to-list 'org-export-backends 'sacha-plain-text)
  (provide 'ox-sacha-plain-text))
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

;; [[file:Sacha.org::#sacha-org-insert-link-dwim][Adding Org Mode link awesomeness elsewhere: sacha-org-insert-link-dwim:2]]
(dolist (group '((org . org-mode-map)
								 (markdown-mode . markdown-mode-map)
								 (mastodon-toot . mastodon-toot-mode-map)
								 (web-mode . web-mode-map)
								 (oddmuse-mode . oddmuse-mode-map)
                 (notmuch . notmuch-message-mode-map)
								 (text-mode . text-mode-map)
								 (html-mode . html-mode-map)))
	(with-eval-after-load (car group)
		(keymap-set (symbol-value (cdr group))  "C-c C-l" #'sacha-org-insert-link-dwim)))
;; Adding Org Mode link awesomeness elsewhere: sacha-org-insert-link-dwim:2 ends here

;; [[file:Sacha.org::#sacha-org-insert-link-dwim][Adding Org Mode link awesomeness elsewhere: sacha-org-insert-link-dwim:5]]
(with-eval-after-load 'org
	(org-link-set-parameters "https" :insert-description #'sacha-org-link-https-insert-description))
;; Adding Org Mode link awesomeness elsewhere: sacha-org-insert-link-dwim:5 ends here

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

;; [[file:Sacha.org::#links-to-sacha-config][Links to my config:2]]
(org-link-set-parameters
 "dotemacs"
 :complete #'sacha-org-dotemacs-complete
 :store #'sacha-org-dotemacs-store
 :insert-description #'sacha-org-dotemacs-insert-description
 :export #'sacha-org-dotemacs-export
 :follow #'sacha-org-dotemacs-open)
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
(org-link-set-parameters "yt" :complete #'sacha-org-yt-complete
												 :insert-description #'sacha-org-yt-insert-description
												 :export #'sacha-org-yt-export
												 :follow #'sacha-org-yt-open)
;; YouTube:3 ends here

;; [[file:Sacha.org::#videos][Videos:1]]
(org-link-set-parameters
 "video"
 :export #'sacha-org-video-export
 :follow #'sacha-org-video-follow
 :complete #'sacha-org-video-complete)
;; Videos:1 ends here

;; [[file:Sacha.org::#org-mode-links-linking-to-a-specific-time-in-a-video][Linking to a specific time in a video:1]]
(org-link-set-parameters
 "vtime"
 :export #'sacha-org-video-time-export
 :complete #'sacha-org-video-time-complete
 :follow #'sacha-org-video-time-follow)
;; Linking to a specific time in a video:1 ends here

;; [[file:Sacha.org::#org-mode-links-linking-to-a-specific-time-in-a-video][Linking to a specific time in a video:4]]
(with-eval-after-load 'org
  (advice-add 'org-insert-item :around 'sacha-org-vtime-insert-item-advice))
;; Linking to a specific time in a video:4 ends here

;; [[file:Sacha.org::org-audio-link][org-audio-link]]
(org-link-set-parameters
 "audio"
 :export #'sacha-org-audio-export
 :follow #'sacha-org-video-follow
 :complete #'sacha-org-audio-complete)

(org-link-set-parameters
 "audioi"
 :export #'sacha-org-audio-export
 :follow #'sacha-org-video-follow
 :complete #'sacha-org-audio-icon-complete)
;; org-audio-link ends here

;; [[file:Sacha.org::#git-projects][Using an Emacs Lisp macro to define quick custom Org Mode links to project files; plus URLs and search:2]]
(with-eval-after-load 'org
  (sacha-org-project-link "subed"
										   "~/proj/subed/subed/"
										   "https://github.com/sachac/subed/blob/main/subed/"
										   ;; "https://codeberg.org/sachac/subed/src/branch/main/subed/"
										   )
  (sacha-org-project-link "emacsconf-el"
										   "~/proj/emacsconf/lisp/"
										   "https://git.emacsconf.org/emacsconf-el/tree/")
  (sacha-org-project-link "subed-record"
										   "~/proj/subed-record/"
										   "https://github.com/sachac/subed-record/blob/main/"
										   ;; "https://codeberg.org/sachac/subed-record/src/branch/main/"
										   )
  (sacha-org-project-link "compile-media"
										   "~/proj/compile-media/"
										   "https://github.com/sachac/compile-media/blob/main/"
										   ;; "https://codeberg.org/sachac/compile-media/src/branch/main/"
										   )
  (sacha-org-project-link "ox-11ty"
										   "~/proj/ox-11ty/"
										   "https://github.com/sachac/ox-11ty/blob/master/")
  (sacha-org-project-link "11ty"
										   "~/proj/static-blog/"
										   "https://github.com/sachac/eleventy-blog-setup/blob/master/")
  (sacha-org-project-link "emacstv"
										   "~/proj/emacstv.github.io/"
										   "https://github.com/emacstv/emacstv.github.io/blob/master/")
  (sacha-org-project-link "quantified"
										   "~/proj/quantified/"
										   "https://github.com/sachac/quantified/blob/master/")
  (sacha-org-project-link "emacs-news"
										   "~/sync/emacs-news/"
										   "https://github.com/sachac/emacs-news/blob/master/")
  (sacha-org-project-link "speech-input"
										   "~/proj/speech-input/"
										   "https://codeberg.org/sachac/speech-input/src/branch/main/")
  (sacha-org-project-link "learn-lang"
										   "~/proj/learn-lang/"
										   "https://codeberg.org/sachac/learn-lang/src/branch/main/"))
;; Using an Emacs Lisp macro to define quick custom Org Mode links to project files; plus URLs and search:2 ends here

;; [[file:Sacha.org::#git-projects][Using an Emacs Lisp macro to define quick custom Org Mode links to project files; plus URLs and search:3]]
(cl-pushnew (cons (expand-file-name "~/sync/sketches/") "https://sketches.sachachua.com/filename/")
						sacha-project-web-base-list
						:test 'equal)
;; Using an Emacs Lisp macro to define quick custom Org Mode links to project files; plus URLs and search:3 ends here

;; [[file:Sacha.org::#org-mode-links-using-an-emacs-lisp-macro-to-define-quick-custom-org-mode-links-to-project-files-plus-urls-and-search-quickly-search-sacha-code][Quickly search my code:2]]
(cl-pushnew (cons (expand-file-name "~/sync/emacs/Sacha.org") nil)
						sacha-project-web-base-list
						:test 'equal)
(cl-pushnew (cons (expand-file-name "~/proj/static-blog/_includes") nil)
						sacha-project-web-base-list
						:test 'equal)
(cl-pushnew (cons (expand-file-name "~/bin") nil)
						sacha-project-web-base-list
						:test 'equal)
;; Quickly search my code:2 ends here

;; [[file:Sacha.org::#org-mode-links-using-an-emacs-lisp-macro-to-define-quick-custom-org-mode-links-to-project-files-plus-urls-and-search-quickly-search-sacha-code][Quickly search my code:3]]
(cl-pushnew (cons (expand-file-name "~/proj/static-blog/blog/") "https://sachachua.com/blog/")
						sacha-project-web-base-list
						:test 'equal)
(cl-pushnew (cons (expand-file-name "~/sync/orgzly") nil)
						sacha-project-web-base-list
						:test 'equal)
;; Quickly search my code:3 ends here

;; [[file:Sacha.org::#org-mode-links-using-an-emacs-lisp-macro-to-define-quick-custom-org-mode-links-to-project-files-plus-urls-and-search-quickly-search-sacha-code][Quickly search my code:4]]
(keymap-global-set "M-s c" #'sacha-consult-ripgrep-code)
;; Quickly search my code:4 ends here

;; [[file:Sacha.org::#org-mode-links-using-an-emacs-lisp-macro-to-define-quick-custom-org-mode-links-to-project-files-plus-urls-and-search-quickly-search-sacha-code-tip-from-omar-embark-around-action-hooks][Tip from Omar: embark-around-action-hooks:2]]
(cl-pushnew #'embark-consult--at-location (alist-get 'org-store-link embark-around-action-hooks))
;; Tip from Omar: embark-around-action-hooks:2 ends here

;; [[file:Sacha.org::#links-from-org-protocol][Links from org-protocol:3]]
(use-package org-protocol-capture-html
	:vc (:url "https://github.com/alphapapa/org-protocol-capture-html"))

;; Links from org-protocol:3 ends here

;; [[file:Sacha.org::#fix-elisp-links][Fix elisp links:2]]
(org-link-set-parameters
 "elisp"
 :export 'sacha-org-elisp-link-export)
;; Fix elisp links:2 ends here

;; [[file:Sacha.org::org-irc-link][org-irc-link]]
(org-link-set-parameters
 "ircs"
 :export #'sacha-org-irc-export)
;; org-irc-link ends here

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
  (define-key hydra-base-map (kbd "<down>") 'sacha-hydra-pop)
  (define-key hydra-base-map (kbd "<up>") (lambda () (interactive) (sacha-hydra-go-and-push 'sacha-shortcuts/body)))


  (defhydra sacha-hydra/org-speed-commands ()
    ("i" sacha-org-set-custom-id "CUSTOM_ID" :exit t)
    ("<up>" sacha-hydra/org-mode/body :exit t)
    ("u" (sacha-hydra-go-and-push 'sacha-hydra/org-mode/body) :exit t :hint nil))
  (defhydra sacha-hydra/org-mode (:foreign-keys run)
    ("b" sacha-org-back-to-heading "Heading")
    ("n" org-forward-heading-same-level "Next")
    ("p" org-backward-heading-same-level "Previous")
    ("a" org-archive-subtree-default "Archive")
    ("j" sacha-org-mark-done-and-add-to-journal "Journal" :exit t)
    ("k" org-cut-subtree "Kill")
    ("<up>" (sacha-hydra-go-and-push 'sacha-shortcuts/body) :exit t hint nil)
    ("u" (sacha-hydra-go-and-push 'sacha-shortcuts/body) :exit t :hint nil)
    ("<f14>" nil "Exit" :exit t))
  (defhydra sacha-hydra/org-link ()
    ("RET" org-open-at-point "Open")
    ("e" org-insert-link "Edit")
    ("c" sacha-caption-show "Captions")
    ("w" sacha-org-link-element-copy-link "Copy link")
    ("u" (sacha-hydra-go-and-push 'sacha-hydra/org-mode/body) :exit t :hint nil)
    ("<up>" (sacha-hydra-go-and-push 'sacha-hydra/org-mode/body) :exit t :hint nil))
  (defhydra sacha-hydra/org-src ()
    ("e" org-babel-execute-src-block "Exec")
    ("E" sacha-org-execute-src-block-by-name "Exec by name")
    ("i" org-edit-special "Edit")
    ("d" org-babel-demarcate-block "Demarcate")
    ("g" org-babel-goto-named-src-block "Goto")
    ("r" org-babel-open-src-block-result "Result")
    ("x" org-babel-expand-src-block "Expand")
    ("t" (org-babel-tangle '(4)) "Tangle at point")
    ("T" (org-babel-tangle '(16)) "Tangle target file")
    ("u" (sacha-hydra-go-and-push 'sacha-hydra/org-mode/body) :exit t :hint nil)
    ("<up>" (sacha-hydra-go-and-push 'sacha-hydra/org-mode/body) :exit t :hint nil)
    )
  ;; Not in a lisp/ file because it's very idiosyncratic
  (defun sacha-hydra/dwim ()
    (interactive)
    (if (derived-mode-p 'org-mode)
        (let ((context (org-element-context)))
          (cond
           ((and (bolp) (looking-at org-outline-regexp))
            (sacha-hydra/org-speed-commands/body))
           ((org-in-src-block-p) (sacha-hydra/org-src/body))
           ((eq (org-element-type context) 'link) (sacha-hydra/org-link/body))
           (t (sacha-hydra/org-mode/body))))
      (sacha-shortcuts/body)))
  (define-key org-mode-map (kbd "<f14>") 'sacha-hydra/dwim)
  (keymap-global-set  "<f14>" 'sacha-hydra/dwim))
;; Speed command for adding a custom ID to Org Mode posts:2 ends here

;; [[file:Sacha.org::#journal][Journal:5]]
(with-eval-after-load 'org
  (org-link-set-parameters
   "journal"
   :follow 'sacha-org-journal-open
   :export 'sacha-org-journal-export
   :complete 'sacha-org-journal-complete))
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
  :if sacha-laptop-p
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
  (add-hook 'org-babel-after-execute-hook 'org-link-preview)
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
(use-package org :hook (org-mode . sacha-org-add-dashes-to-tag-regexps))
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
   :follow 'sacha-org-package-open :export 'sacha-org-package-export :complete 'sacha-org-package-complete
   :insert-description #'sacha-org-package-link-description))
;; Package links:2 ends here

;; [[file:Sacha.org::#save-when-emacs-loses-focus][Save when Emacs loses focus:2]]
(use-package org
  :config
  (add-function :after after-focus-change-function 'sacha-org-save-all-org-buffers))
;; Save when Emacs loses focus:2 ends here

;; [[file:Sacha.org::#setting-properties][Setting properties:2]]
(use-package org
  :bind (:map org-mode-map
              ("C-c C-x p" . sacha-org-set-property)))
;; Setting properties:2 ends here

;; [[file:Sacha.org::#org-mode-linking-to-and-exporting-function-definitions-in-org-mode-still-allow-linking-to-the-file][Still allow linking to the file:2]]
(with-eval-after-load 'org
	(org-link-set-parameters "_file" :store #'sacha-org-defun-store-file-link))
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
  (add-to-list 'embark-target-injection-hooks '(sacha-blog-similar-link sacha-embark-blog--inject-target-url)))
;; Consult-based interface for searching blog posts:2 ends here

;; [[file:Sacha.org::#org-mode-vector-search-consult-based-interface-for-searching-blog-posts-multiple-sources][Multiple sources:1]]
(with-eval-after-load 'consult
  (defvar sacha-consult-source-similar-blog-posts
    (list :name "Blog posts"
          :narrow ?b
          :category 'sacha-blog
          :state #'sacha-blog-post--state
          :async (consult--dynamic-collection
                     (lambda (input)
                       (seq-take
                        (sacha-org-db-v3-blog-post--collection input)
                        5)))
          :action #'sacha-embark-blog-insert-link))
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

;; [[file:Sacha.org::#sacha-image-write-region][Emacs: Extract part of an image to another file:3]]
(with-eval-after-load 'image
	(keymap-set image-map "i w" #'sacha-image-write-region))
;; Emacs: Extract part of an image to another file:3 ends here

;; [[file:Sacha.org::#svg][SVG:1]]
(auto-image-file-mode -1)
;; SVG:1 ends here

;; [[file:Sacha.org::#org-mode-sketch-links][Org Mode sketch: links:2]]
(use-package org
  :config
  (setq org-image-actual-width 600)
  (org-link-set-parameters
   "sketch"
   :follow 'sacha-org-sketch-open
   :export 'sacha-org-image-export-link
   :complete 'sacha-org-sketch-complete
   :activate-func nil)
  (org-link-set-parameters
   "sketchLink"
   :follow 'sacha-org-sketch-open
   :export 'sacha-org-image-export-link
   :complete 'sacha-org-sketch-complete
   :activate-func nil)
  (org-link-set-parameters
   "sketchThumb"
   :follow 'sacha-org-sketch-open
   :export 'sacha-org-image-export-thumb
   :complete 'sacha-org-sketch-complete
   :activate-func nil)
  (org-link-set-parameters
   "sketchFull"
   :follow 'sacha-org-sketch-open
   :export 'sacha-org-image-export-full
   :complete 'sacha-org-sketch-complete-full
   :activate-func nil))
  (org-link-set-parameters
   "image"
   :follow 'sacha-org-image-open
   :export 'sacha-org-image-export
   :complete 'sacha-org-image-complete))
;; Org Mode sketch: links:2 ends here

;; [[file:Sacha.org::#org-mode-copy][Org Mode custom link: copy to clipboard:2]]
(use-package org
  :config
  (org-link-set-parameters
   "copy"
   :follow (lambda (link) (kill-new link))
	 :export #'sacha-org-copy-export))
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
(setq sacha-sketch-executable "krita"
      sacha-sketch-inbox-directory "~/Dropbox/Inbox"
      sacha-index-card-template-file "~/Dropbox/drawings/templates/0 - index.psd"
      sacha-sketch-large-template-file "/home/sacha/Dropbox/drawings/templates/0 - base.psd")
;; Button-based interface:2 ends here

;; [[file:Sacha.org::#rename-scanned-index-cards][Rename scanned index cards:1]]
(use-package s)
;; Rename scanned index cards:1 ends here

;; [[file:Sacha.org::#automatically-resize-images][Automatically resize images:1]]
(use-package image+
  :if sacha-laptop-p
  ;;    :load-path "~/elisp/Emacs-imagex"
  :commands (imagex-global-sticky-mode imagex-auto-adjust-mode)
  :init (progn (imagex-global-sticky-mode) (imagex-auto-adjust-mode)))
;; Automatically resize images:1 ends here

;; [[file:Sacha.org::#xournalpp-and-krita][Xournalpp and Krita:1]]
(use-package org-krita
  :ensure t
  :vc (:url "https://github.com/lepisma/org-krita" :files ("*.el" "resources"))
  :hook (org-mode . org-krita-mode))
(use-package org-xournalpp
  :disabled t
  :vc (:url "https://github.com/vherrmann/org-xournalpp" :files ("*.el" "resources"))
  :hook (org-mode . org-xournalpp-mode))
;; Xournalpp and Krita:1 ends here

;; [[file:Sacha.org::#insert-point][Sketched books:1]]
(setq yas-indent-line 'fixed)
;; Sketched books:1 ends here

;; [[file:Sacha.org::#other-sketch-related-functions][Other sketch-related functions:2]]
(with-eval-after-load 'org
  (let ((listvar (if (boundp 'org-speed-commands) 'org-speed-commands
                   'org-speed-commands-user)))
    (add-to-list listvar '("d" call-interactively 'sacha-prepare-index-card-for-subtree))))
;; Other sketch-related functions:2 ends here

;; [[file:Sacha.org::#supernote][Supernote:9]]
(setq htmlize-css-name-prefix "org-")
(setq htmlize-head-tags "<link rel=\"stylesheet\" href=\"https://sachachua.com/assets/css/style.css\" />")
;; Supernote:9 ends here

;; [[file:Sacha.org::#supernote-browse][org-attaching the latest image from my Supernote via Browse and Access:1]]
(setq sacha-supernote-ip-address "192.168.1.221")
;; org-attaching the latest image from my Supernote via Browse and Access:1 ends here

;; [[file:Sacha.org::#other-subtitle-code][Other subtitle code:2]]
(defhydra sacha-subed ()
  "Make it easier to split and merge"
  ("e" subed-jump-to-subtitle-end "End")
  ("s" subed-jump-to-subtitle-text "Start")
  ("f" sacha-subed-forward-word "Forward word")
  ("b" sacha-subed-backward-word "Backward word")
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
  :if sacha-laptop-p
  :preface (load "~/proj/subed/subed-autoloads.el" nil t)
  :load-path "~/proj/subed/subed"
  :config
  (setq subed-subtitle-spacing 1)
  (setq subed-align-mfa-conda-env "/home/sacha/vendor/miniconda3/envs/aligner")
  (key-chord-define subed-mode-map "hu" 'sacha-subed/body)
  (key-chord-define subed-mode-map "ht" 'sacha-subed/body)
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
  :if sacha-laptop-p
  :load-path "~/proj/subed"
  :preface (load "~/proj/subed/subed-autoloads.el" nil t)
  :mode
  (("\\.vtt\\'" . subed-vtt-mode)
   ("\\.srt\\'" . subed-srt-mode)
   ("\\.ass\\'" . subed-ass-mode))
  :init
  (autoload 'subed-vtt-mode "subed-vtt" nil t)
  (autoload 'subed-srt-mode "subed-srt" nil t)
  (autoload 'subed-ass-mode "subed-ass" nil t)
  (autoload 'subed-txt-mode "subed-txt" nil t)
  :hook
  (subed-mode . display-fill-column-indicator-mode)
  (subed-mode . subed-avy-set-up-actions)
  :bind
  (:map subed-mode-map
        ("M-," . subed-split-subtitle)
        ("M-." . subed-merge-dwim))
	:config
	;; Remember cursor position between sessions
	(add-hook 'subed-mode-hook 'sacha-subed-maybe-save-place)
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
(use-package waveform :load-path "~/proj/waveform-el" :if sacha-laptop-p :defer t)
(use-package compile-media :load-path "~/proj/compile-media" :if sacha-laptop-p :defer t
	:autoload compile-media-timestamp-to-msecs
	)
;; Working with media:1 ends here

;; [[file:Sacha.org::#split-up-oops-better][Split up oops better:2]]
(setq subed-align-options "task_adjust_boundary_offset_percent=0.5")
;; Split up oops better:2 ends here

;; [[file:Sacha.org::#multimedia-subtitles-with-subed-using-scripts-to-correct-transcripts][Using scripts to correct transcripts:1]]



;;  (sacha-combine-script-and-transcript '("I have a script" "that's broken up" "into phrases.") (split-string "I have, oops, I have a script oops. I have a script that's broken up in to faces." " ") "\\<oops\\>")
;;  (sacha-combine-script-and-transcript '("I already talk quickly," "so I'm not going to speed that up" "into phrases.") (split-string "I already talk pretty quickly. Oops. I already talk quickly, so I'm not going to speed that up, but I can trim the pauses in between phrases,"))
;; (subed-word-data-find-approximate-match "I already talk quickly" (split-string "I already talk pretty quickly oops I already talk quickly" " "))
;; Using scripts to correct transcripts:1 ends here

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
  :vc (:url "https://github.com/karthink/elfeed-tube")
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
  :vc (:url "https://github.com/karthink/elfeed-tube")
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
	:config
  (sacha-convert-shell-scripts-to-interactive-commands "~/bin"))
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
(when sacha-laptop-p
  (setq create-lockfiles nil))


(use-package web-mode
  :if sacha-laptop-p
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
	("C-c C-r" . sacha-copy-and-append))
;; Web development:6 ends here

;; [[file:Sacha.org::#lsp][LSP:2]]
(use-package lsp-mode
  :if sacha-laptop-p
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
  :hook ((js-mode . sacha-local-lsp)
         (python-mode . sacha-local-lsp)
         (lsp-mode-hook . lsp-enable-which-key-integration)))
(use-package lsp-ui
  :if sacha-laptop-p
  :commands lsp-ui-mode
  :after lsp-mode)
(use-package dap-mode
  :if sacha-laptop-p
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
  :vc (:url "https://github.com/Artawower/turbo-log")
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

;; [[file:Sacha.org::#more-indentation-things][More indentation things:2]]
(bind-key "C-M-<backspace>" 'sanityinc/kill-back-to-indentation)
;; More indentation things:2 ends here

;; [[file:Sacha.org::#yaml][YAML:1]]
(use-package yaml-mode
  :if sacha-laptop-p
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
    :if sacha-laptop-p
    :config (auto-compile-on-load-mode)
		:defer t)
  (setq native-comp-async-report-warnings-errors nil)
;; Emacs Lisp:1 ends here

;; [[file:Sacha.org::#emacs-lisp][Emacs Lisp:3]]
(setq eval-expression-print-length nil)
(setq print-length nil)
(setq edebug-print-length nil)
(add-hook 'emacs-lisp-mode-hook
					'sacha-set-sentence-end-double-space)
;; Emacs Lisp:3 ends here

;; [[file:Sacha.org::#emacs-lisp][Emacs Lisp:4]]
(use-package which-func)
;; Emacs Lisp:4 ends here

;; [[file:Sacha.org::#emacs-lisp][Emacs Lisp:5]]
(use-package let-completion :vc (:url "https://github.com/gggion/let-completion.el")
  :hook (emacs-lisp-mode . let-completion-mode))
;; Emacs Lisp:5 ends here

;; [[file:Sacha.org::#coding-emacs-lisp-prefix-for-writing-functions][Prefix for writing functions:2]]
(setq sacha-function-prefix "sacha-")
;; Prefix for writing functions:2 ends here

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
  '(defhydra sacha-lispy-cheat-sheet (:hint nil :foreign-keys run)
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
(with-eval-after-load 'lispy
  (define-key lispy-mode-map (kbd "<f14>") 'sacha-lispy-cheat-sheet/body)
  (define-key lispy-mode-map (kbd "C-?") 'sacha-lispy-cheat-sheet/body))
(with-eval-after-load 'evil-lispy
  (evil-define-key nil evil-lispy-mode-map (kbd "<f14>") 'sacha-lispy-cheat-sheet/body))
)
;; Emacs: Making a hydra cheatsheet for Lispy:1 ends here

;; [[file:Sacha.org::#smartparens-mode][Smartparens mode:1]]
(use-package smartparens
  :if sacha-laptop-p
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
    (sp-local-pair 'web-mode "<" nil :when '(sacha-sp-web-mode-is-code-context))

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
  :after 'lispy
  :hook
  (emacs-lisp-mode .  nameless-mode)
  :bind
  (:map emacs-lisp-mode-map
        ("C-c -" . nameless-insert-name)
        ("_" . nameless-insert-name-or-self-insert)
   :map lispy-mode-map
        ("_" . nameless-insert-name-or-self-insert))
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
	:hook '(buttercup-minor-mode . sacha-buttercup-set-up-imenu))
(use-package bug-hunter
  :load-path "~/vendor/elisp-bug-hunter")
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
	 emacs-lisp-mode-map ("C-c C-t" . #'sacha-eval-buf-and-run-ert-test-at-point)))
;; ERT:2 ends here

;; [[file:Sacha.org::#undercover][Undercover:1]]
(use-package undercover
	:vc (:url "https://github.com/undercover-el/undercover.el")
	:defer t
	)
(use-package coverage :defer t)
;; Undercover:1 ends here

;; [[file:Sacha.org::#eldoc][Eldoc:1]]
(use-package eldoc
  :if sacha-laptop-p
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
	:if sacha-laptop-p
  :hook (flycheck-mode . mp-flycheck-prefer-eldoc)
  :bind (:map flycheck-mode-map
              ("s-n" . flycheck-next-error))
  )
(use-package eglot
	:if sacha-laptop-p
  :preface
;;;###autoload
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
  :if sacha-laptop-p
  :defer t
  :bind (:map emacs-lisp-mode-map ("C-c C-v" . erefactor-map)))

(use-package redshank
  :if sacha-laptop-p
  :disabled t
  :defer t
  :init (add-hook 'emacs-lisp-mode-hook 'redshank-mode))

;; Refactoring:1 ends here

;; [[file:Sacha.org::#jumping-to-code][Jumping to code:1]]
(define-key emacs-lisp-mode-map (kbd "C-c .") 'find-function-at-point)
;; Jumping to code:1 ends here

;; [[file:Sacha.org::#org-mode-org-babel-fix-find-function-when-i-ve-evaluated-something-from-org-babel][YE11: Fix find-function for Emacs Lisp from org-babel or scratch:3]]
(setq sacha-elisp-find-function-search-extra '("~/sync/emacs/Sacha.org"))
(advice-add 'find-function-search-for-symbol :around #'sacha-elisp-find-function-search-for-symbol)
;; YE11: Fix find-function for Emacs Lisp from org-babel or scratch:3 ends here

;; [[file:Sacha.org::#evaluation][Evaluation:2]]
(bind-key "M-:" 'pp-eval-expression)
(bind-key "C-x C-e" 'sanityinc/eval-last-sexp-or-region emacs-lisp-mode-map)
;; Evaluation:2 ends here

;; [[file:Sacha.org::#auto-insert][Auto insert:1]]
(auto-insert-mode)
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
(bind-key "C-:" #'sacha-stub-elisp-defun emacs-lisp-mode-map)
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

;; [[file:Sacha.org::#snippets][Snippets:2]]

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
  (bind-key "\t" 'hippie-expand yas-minor-mode-map))

(use-package yasnippet-capf
  :after cape
  :config
  (add-to-list 'completion-at-point-functions #'yasnippet-capf)
  :hook
  (emacs-lisp-mode . sacha-use-yasnippet-capf)
  (lisp-interaction-mode . sacha-use-yasnippet-capf)
  (org-mode . sacha-use-yasnippet-capf)
  (js2-mode . sacha-use-yasnippet-capf)
  )
;;        (global-set-key (kbd "C-c y") (lambda () (interactive)
;;                                         (yas/load-directory "~/elisp/snippets")))
;; Snippets:2 ends here

;; [[file:Sacha.org::#snippets][Snippets:4]]
(setq default-cursor-color "gray")
(setq yasnippet-can-fire-cursor-color "purple")
;; Snippets:4 ends here

;; [[file:Sacha.org::#snippets][Snippets:6]]
;; As pointed out by Dmitri, this will make sure it will update color when needed.
(remove-hook 'post-command-hook 'sacha-change-cursor-color-when-can-expand)
;; Snippets:6 ends here

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
  :if sacha-laptop-p
  :mode "\\.coffee\\'"
  :bind (:map coffee-mode-map ("C-c C-c" . compile)))
;; Javascript:2 ends here

;; [[file:Sacha.org::#javascript][Javascript:3]]
(use-package jasminejs-mode
  :if sacha-laptop-p
  :after js2-mode
  :hook ((js2-mode . jasminejs-mode)
         (jasminejs-mode-hook . jasminejs-add-snippets-to-yas-snippet-dirs)))
;; Javascript:3 ends here

;; [[file:Sacha.org::#javascript][Javascript:6]]
(use-package js2-mode
  :if sacha-laptop-p
  :commands js2-mode
  :defer t
  :interpreter "node"
  :init (setq js-indent-level 2)
	:mode "\\.[mc]?js\\'"
  :bind (:map js2-mode-map
              ("C-x C-e" . js-send-last-sexp)
              ("C-M-x" . js-send-last-sexp-and-go)
              ("C-c d" . sacha-insert-or-flush-debug)
              ("C-c C-b" . js-send-buffer-and-go)
              ("C-c w" . sacha-copy-javascript-region-or-buffer))
  :config (js2-imenu-extras-setup))
;; Javascript:6 ends here

;; [[file:Sacha.org::#javascript][Javascript:7]]
(use-package coffee-mode
  :if sacha-laptop-p
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
  :if sacha-laptop-p)
;; React:1 ends here

;; [[file:Sacha.org::#coding-typescript][Typescript:1]]
(use-package typescript-mode
	:mode "\\.ts\\'")
;; Typescript:1 ends here

;; [[file:Sacha.org::#shell][Shell:1]]
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)
;; Shell:1 ends here

;; [[file:Sacha.org::#shellcheck][Shellcheck:2]]
(use-package flymake
  :bind (("S-e" . flymake-show-project-diagnostics)))

(use-package sh-script
  :hook (sh-mode . flymake-mode))

(use-package flymake-shellcheck :defer t)
(use-package flymake
  :bind (("S-e" . sacha-consult-flymake-project))
  :custom
  (flymake-suppress-zero-counters t)
  :config
  (defface sacha-flymake-modeline-error-echo
    '((t :inherit 'flymake-error-echo :background "red"))
    "Mode line flymake errors")
  (defface sacha-flymake-modeline-warning-echo
    '((t :inherit 'flymake-warning-echo :background "orange"))
    "Mode line flymake warnings")
  (put 'flymake-error 'mode-line-face 'sacha-flymake-modeline-error-echo)
  (put 'flymake-warning 'mode-line-face 'sacha-flymake-modeline-warning-echo))
;; Shellcheck:2 ends here

;; [[file:Sacha.org::#dwim-shell-command][dwim-shell-command:2]]
(use-package dwim-shell-command
  :if sacha-laptop-p
  :bind (([remap shell-command] . sacha-dwim-shell-command)
         :map dired-mode-map
         ([remap dired-do-async-shell-command] . sacha-dwim-shell-command)
         ([remap dired-do-shell-command] . sacha-dwim-shell-command)
         ([remap dired-smart-shell-command] . sacha-dwim-shell-command))
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
(defvar sacha-magit-limit-to-directory)
(use-package magit
  :config
  (setq magit-diff-options '("-b")) ; ignore whitespace
  (setq sacha-magit-limit-to-directory nil)
  (defadvice magit-insert-untracked-files (around sacha activate)
    (if sacha-magit-limit-to-directory
        (magit-with-section (section untracked 'untracked "Untracked files:" t)
                            (let ((files (cl-mapcan
                                          (lambda (f)
                                            (when (eq (aref f 0) ??) (list f)))
                                          (magit-git-lines
                                           "status" "--porcelain" "--" sacha-magit-limit-to-directory))))
                              (if (not files)
                                  (setq section nil)
                                (dolist (file files)
                                  (setq file (magit-decode-git-path (substring file 3)))
                                  (magit-with-section (section file file)
                                                      (insert "\t" file "\n")))
                                (insert "\n"))))
      ad-do-it))

  (defadvice magit-insert-unstaged-changes (around sacha activate)
    (if sacha-magit-limit-to-directory
        (let ((magit-current-diff-range (cons 'index 'working))
              (magit-diff-options (copy-sequence magit-diff-options)))
          (magit-git-insert-section (unstaged "Unstaged changes:")
                                    #'magit-wash-raw-diffs
                                    "diff-files"
                                    "--" sacha-magit-limit-to-directory
                                    ))
      ad-do-it))

  (defadvice magit-insert-staged-changes (around sacha activate)
    "Limit to `sacha-magit-limit-to-directory' if specified."
    (if sacha-magit-limit-to-directory
        (let ((no-commit (not (magit-git-success "log" "-1" "HEAD"))))
          (when (or no-commit (magit-anything-staged-p))
            (let ((magit-current-diff-range (cons "HEAD" 'index))
                  (base (if no-commit
                            (magit-git-string "mktree")
                          "HEAD"))
                  (magit-diff-options (append '("--cached") magit-diff-options)))
              (magit-git-insert-section (staged "Staged changes:")
                                        (apply-partially #'magit-wash-raw-diffs t)
                                        "diff-index" "--cached" base "--" sacha-magit-limit-to-directory))))
      ad-do-it))
  :bind (("C-x v C-d" . sacha-magit-status-in-directory)
         ("C-c g" . magit-file-dispatch)
         ("C-x g" . magit-status)
         ("C-x v p" . magit-push)
         ("C-x v c" . sacha-magit-stage-all-and-commit)))
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
  (add-hook 'drupal-mode-hook 'sacha-find-tags))
;; Tag files:2 ends here

;; [[file:Sacha.org::#projects-and-projectile][Projects and projectile:2]]
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
  :if sacha-laptop-p
  :defer t
  :config
  (add-hook 'makefile-mode-hook 'makefile-executor-mode))
;; Projects and projectile:2 ends here

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
  :if sacha-laptop-p
  :hook
  ((js2-mode-hook . skewer-mode)
   (css-mode-hook . skewer-css-mode)
   (html-mode-hook . skewer-html-mode)))
;; Skewer:1 ends here

;; [[file:Sacha.org::#autocomplete][Autocomplete:1]]
(with-eval-after-load 'company
	(define-key company-mode-map (kbd "<tab>") 'company-indent-or-complete-common))
(use-package company
  :if sacha-laptop-p
  ;:init (add-hook 'prog-mode-hook 'company-mode)
  )
(use-package company-posframe :if sacha-laptop-p :init (company-posframe-mode 1) :diminish)
;; Autocomplete:1 ends here

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
  :if sacha-laptop-p
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
  :if sacha-laptop-p
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
(add-to-list 'browse-url-handlers '("https?://[^/]+/@[^/]+/.*" . sacha-mastodon-browse-url))
;; Mastodon:3 ends here

;; [[file:Sacha.org::#mastodon-mastodon-el-copy-toot-url-after-posting-also-copying-just-this-post-with-11ty][mastodon.el: Copy toot URL after posting; also, copying just this post with 11ty:2]]
(with-eval-after-load 'mastodon-toot
	(when (functionp 'mastodon-toot-send)
		(advice-add
		 #'mastodon-toot-send
		 :after
		 (lambda (&rest _)
			 (run-hook-with-args 'sacha-mastodon-toot-posted-hook (sacha-mastodon-latest-toot)))))
	(when (functionp 'mastodon-toot--send)
		(advice-add
		 #'mastodon-toot--send
		 :after
		 (lambda (&rest _)
			 (run-hook-with-args 'sacha-mastodon-toot-posted-hook (sacha-mastodon-latest-toot))))))
;; mastodon.el: Copy toot URL after posting; also, copying just this post with 11ty:2 ends here

;; [[file:Sacha.org::#storing-mastodon-links-in-org-mode][Storing Mastodon links in Org mode:2]]
(use-package org
  :config
  (org-link-set-parameters
   "mastodon"
   :store 'sacha-mastodon-store-link)
	(with-eval-after-load 'org-capture
		(add-to-list 'org-capture-templates
								 `("m" "Mastodon" entry (file ,sacha-org-inbox-file)
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
	:bind (:map mastodon-mode-map ("w" . sacha-mastodon-save-toot-for-emacs-news)))
;; Collecting Emacs News from Mastodon:2 ends here

;; [[file:Sacha.org::#mastodon-combined-timeline][Combining Mastodon timelines using mastodon.el:2]]
(when (functionp 'memoize)
	(unless (get #'sacha-mastodon-fetch-posts-after :memoize-original-function)
		(memoize #'sacha-mastodon-fetch-posts-after)))
;; Combining Mastodon timelines using mastodon.el:2 ends here

;; [[file:Sacha.org::#mastodon-combined-timeline][Combining Mastodon timelines using mastodon.el:6]]
(with-eval-after-load 'mastodon-tl
	(advice-add #'mastodon-toot--action :before #'sacha-mastodon-update-external-item-id)
	(advice-add #'mastodon-toot--reply :before #'sacha-mastodon-update-external-item-id)
	(advice-add #'mastodon-tl--thread :before #'sacha-mastodon-update-external-item-id))
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

  (defhydra sacha-mastodon-help (:color blue :hint nil)
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
		("s" sacha-mastodon-toot-screenshot)
		("w" sacha-mastodon-save-toot-for-emacs-news)
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
    ("O" mastodon-profile-sacha-profile)
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
 :bind ("s-m" . sacha-mastodon-help/body))
;; Mastodon keyboard shortcuts via Hydra:1 ends here

;; [[file:Sacha.org::#mastodon-org-contacts-complete][Completion:2]]
(with-eval-after-load 'mastodon-toot
	(with-eval-after-load 'org-contacts
		(add-hook 'mastodon-toot-mode-hook
							(lambda ()
								(add-hook 'completion-at-point-functions
													#'sacha-mastodon-complete-contact nil t)))))
;; Completion:2 ends here

;; [[file:Sacha.org::#mastodon-org-feed][Collect my recent toots in an Org file so that I can refile them:1]]
(use-package pandoc :defer t)
(advice-add #'org-feed-add-items :after #'sacha-org-feed-sort)
(setq org-feed-alist '(("Mastodon" "https://emacs.ch/@sachac/with_replies.rss"
												"~/sync/orgzly/toots.org" "Toots"
												:formatter sacha-mastodon-org-feed-formatter)))
;; Collect my recent toots in an Org file so that I can refile them:1 ends here

;; [[file:Sacha.org::#web][Web:1]]
(setq browse-url-firefox-program "firefox")
;; Web:1 ends here

;; [[file:Sacha.org::#web-emacs-open-urls-or-search-the-web-plus-browse-url-handlers][Emacs: Open URLs or search the web, plus browse-url-handlers:2]]
(setopt sacha-search-web-handler #'consult-omni)
;; Emacs: Open URLs or search the web, plus browse-url-handlers:2 ends here

;; [[file:Sacha.org::#web-emacs-open-urls-or-search-the-web-plus-browse-url-handlers][Emacs: Open URLs or search the web, plus browse-url-handlers:3]]
(keymap-global-set "C-c o" #'sacha-open-url-or-search-web)
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
  (defengine sacha-blog "https://www.google.ca/search?q=site:sachachua.com+%s" :keybinding "b")
  (defengine mail "https://mail.google.com/mail/u/0/#search/%s" :keybinding "m")
  (defengine google "https://google.com/search?q=%s" :keybinding "g")
  (defengine emacswiki "https://google.com/search?q=site:emacswiki.org+%s" :keybinding "e")
  (engine-mode)
  :hydra
  (sacha-engine-mode-hydra
   (:color blue)
   "Engine mode"
   ("b" engine/search-sacha-blog "blog")
   ("m" engine/search-mail "mail")
   ("g" engine/search-google "google")
   ("e" engine/search-emacswiki "emacswiki")))
;; Search:1 ends here

;; [[file:Sacha.org::#web-spookfox-link-to-current-webpage-from-spookfox][Link to current webpage from Spookfox:2]]
(with-eval-after-load 'org
	(org-link-set-parameters
	 "spookfox"
	 :complete #'sacha-org-spookfox-complete
	 :insert-description #'sacha-org-link-insert-description))

;; Link to current webpage from Spookfox:2 ends here

;; [[file:Sacha.org::#spookfox-babel][Running the current Org Mode Babel Javascript block from Emacs using Spookfox:5]]
(with-eval-after-load 'ob-js
	(advice-add 'org-babel-execute:js :around #'sacha-org-babel-execute:js-spookfox))
;; Running the current Org Mode Babel Javascript block from Emacs using Spookfox:5 ends here

;; [[file:Sacha.org::#spookfox-babel][Running the current Org Mode Babel Javascript block from Emacs using Spookfox:7]]
(with-eval-after-load 'embark-org
	(define-key embark-org-src-block-map "f" #'sacha-spookfox-eval-org-block))
;; Running the current Org Mode Babel Javascript block from Emacs using Spookfox:7 ends here

;; [[file:Sacha.org::#spookfox-scroll][Using Spookfox to scroll Firefox up and down from Emacs:1]]
(use-package spookfox
  ; :files ("lisp/*.el" "lisp/apps/*.el"))
	:load-path ("~/vendor/spookfox/lisp" "~/vendor/spookfox/lisp/apps")
	:when sacha-laptop-p
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
(keymap-global-set "C-s-v" 'sacha-spookfox-scroll-down)
(keymap-global-set "S-s-v" 'sacha-spookfox-scroll-up)
;; Using Spookfox to scroll Firefox up and down from Emacs:3 ends here

;; [[file:Sacha.org::#spookfox-insert-url][Emacs and Spookfox: org-capture the current tab from Firefox or a link from the page:2]]
(with-eval-after-load 'org
	(cl-pushnew
	 `("f" "Firefox" entry
			(file ,sacha-org-inbox-file)
			"* %^{Note}\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n%(apply #'org-link-make-string
								 (append (spookfox-js-injection-eval-in-active-tab \"[window.location.href, document.title]\" t) nil))")
	 org-capture-templates)
	(cl-pushnew
	 `("F" "Firefox link" entry
			(file ,sacha-org-inbox-file)
			"* %^{Note}\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n%(org-link-make-string
(sacha-spookfox-complete-link))")
	 org-capture-templates))
;; Emacs and Spookfox: org-capture the current tab from Firefox or a link from the page:2 ends here

;; [[file:Sacha.org::#clock-in][Quantified Awesome:2]]
(bind-key "C-c q" 'sacha-org-quick-clock-in-task)
(bind-key "!" 'sacha-org-clock-in-and-track org-agenda-mode-map)
;; Quantified Awesome:2 ends here

;; [[file:Sacha.org::#using-the-calendar-date-echo-text-variable-to-help-plot-a-heatmap-on-a-year-long-calendar-in-emacs][Using the calendar-date-echo-text variable to help plot a heatmap on a year-long calendar in Emacs:1]]
;; This seems to be the only way we can hack the date in for now
(setq calendar-date-echo-text '(apply #'format (list "%04d-%02d-%02d" year month day)))
;; Using the calendar-date-echo-text variable to help plot a heatmap on a year-long calendar in Emacs:1 ends here

;; [[file:Sacha.org::#using-the-calendar-date-echo-text-variable-to-help-plot-a-heatmap-on-a-year-long-calendar-in-emacs][Using the calendar-date-echo-text variable to help plot a heatmap on a year-long calendar in Emacs:3]]
(advice-add #'calendar :after #'sacha-calendar-heat-map-using-echo-text)
(advice-add #'calendar-redraw :after #'sacha-calendar-heat-map-using-echo-text)
(advice-add #'year-calendar :after #'sacha-calendar-heat-map-using-echo-text)
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

;; [[file:Sacha.org::#on-sacha-phone][Emacs and my phone:1]]
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
  (when sacha-phone-p
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
(use-package xclip :if sacha-phone-p) ; Turn on with xclip-mode
;; Clipboard:2 ends here

;; [[file:Sacha.org::#async-smtpmail][Send mail asynchronously:2]]
(setq send-mail-function 'sacha-async-smtpmail-send-it
      message-send-mail-function 'sacha-async-smtpmail-send-it)
;; Send mail asynchronously:2 ends here

;; [[file:Sacha.org::#notmuch][Notmuch:1]]
(setq notmuch-message-headers '("Subject" "To" "Cc" "Date" "Reply-To"))
(use-package notmuch
  :if sacha-laptop-p
  :config (setq-default notmuch-search-oldest-first nil)
  (setq notmuch-fcc-dirs nil)
  (setq notmuch-archive-tags '("-inbox" "-flagged" "-unread" "-new")))
(use-package ol-notmuch
  :if sacha-laptop-p)
;; Notmuch:1 ends here

;; [[file:Sacha.org::#act-on-current-message-with-embark][Act on current message with Embark:2]]
(with-eval-after-load 'embark
	(add-to-list 'embark-target-finders 'sacha-embark-mail-finder)
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

;; [[file:Sacha.org::#collaboration][Collaboration:1]]
(use-package crdt
  :vc (:url "https://github.com/zaeph/crdt.el")
  :commands (crdt-share-buffer crdt-connect)
  :load-path "~/vendor/crdt.el"
  :if sacha-laptop-p)

;; Collaboration:1 ends here

;; [[file:Sacha.org::#collaboration-bike-brigade-working-with-mailchimp-images][Bike Brigade: working with Mailchimp images:1]]
(use-package mailchimp :load-path "~/proj/mailchimp-el" :vc (:url "https://github.com/sachac/mailchimp-el"))
;; Bike Brigade: working with Mailchimp images:1 ends here

;; [[file:Sacha.org::#streaming-mode-for-streaming][Mode for streaming:1]]
  (setq sacha-stream-inbox-file "~/sync/topics/live.org")
  (with-eval-after-load 'org
    (add-to-list 'org-capture-templates
  	       `("u" "Update" item  ; Update for the livestream
  		 (file+headline ,sacha-stream-inbox-file "Updates")
  		 "- %U %?")))

;; Mode for streaming:1 ends here

;; [[file:Sacha.org::#streaming-mode-for-streaming][Mode for streaming:3]]
(use-package fontaine
  :config
  (setq fontaine-presets
        '((regular :default-height 100)
          (presentation :default-height 180))))
;; Mode for streaming:3 ends here

;; [[file:Sacha.org::*Custom Org link type for hints (and sound effects)][Custom Org link type for hints (and sound effects):2]]
(with-eval-after-load 'org
  (setq sacha-org-hint-sound-alist
        '(("yup" . "~/proj/stream/correct.mp3")
          ("nope" . "~/proj/stream/wrong.mp3")))
  (setq sacha-org-hint-functions '(sacha-org-hint-play-sound))
  (org-link-set-parameters "hint"
												 :export #'sacha-org-hint-export
												 :follow #'sacha-org-hint-open))
;; Custom Org link type for hints (and sound effects):2 ends here

;; [[file:Sacha.org::#streaming-chat][Chat:2]]
(keymap-global-set "s-c" #'sacha-stream-chat-start)
;; Chat:2 ends here

;; [[file:Sacha.org::#streaming-send-currently-clocked-task-title-to-file-include-in-stream][Send currently-clocked task title to file, include in stream:2]]
(setq sacha-stream-display-file "~/proj/stream/current-task.txt")
(with-eval-after-load 'org-clock
  (add-hook 'org-clock-in-hook #'sacha-stream-obs-org-display-current-task)
  (add-hook 'org-clock-out-hook #'sacha-org-clear-streaming-task))
;; Send currently-clocked task title to file, include in stream:2 ends here

;; [[file:Sacha.org::#controlling-sacha-stream-audio-from-emacs-background-music-typing-sounds-and-push-to-talk][Controlling my stream audio from Emacs: background music, typing sounds, and push to talk:4]]
(use-package selectric-mode
  :if sacha-laptop-p
  :diminish ""
	:defer t
	:commands selectric-mode
  :config
  (fset #'selectric-type-sound #'sacha-selectric-type-sound))
;; Controlling my stream audio from Emacs: background music, typing sounds, and push to talk:4 ends here

;; [[file:Sacha.org::#controlling-sacha-stream-audio-from-emacs-background-music-typing-sounds-and-push-to-talk][Controlling my stream audio from Emacs: background music, typing sounds, and push to talk:6]]
(defvar sacha-mic-p)
(add-to-list 'mode-line-front-space '(:eval (if sacha-mic-p "*MIC*" "")))
;; Controlling my stream audio from Emacs: background music, typing sounds, and push to talk:6 ends here

;; [[file:Sacha.org::#general-streaming-configuration][General streaming configuration:1]]
(defvar sacha-stream-captions-insert nil "Non-nil means insert into the current buffer.")
(defhydra sacha-stream ()
	("w" (org-open-link-from-string "[[file:~/proj/stream/index.org::#streaming-workflow][Streaming]]") "Workflow" :column "Setup")
  ;("a" sacha-show-emacs-tasks "Agenda")
	;("t" sacha-stream-insert-timestamp "Timestamp" :exit t)
  ;("bt" selectric-mode "Typing sounds")
  ;("bm" sacha-stream-toggle-background-music "Background music")
  ("y" (browse-url "https://studio.youtube.com/channel/UClT2UAbC6j7TqOWurVhkuHQ/livestreaming/dashboard") "Youtube")
	("ts" (browse-url "https://twitch.tv/sachachua") "View stream")
  ("tv" (browse-url "https://dashboard.twitch.tv/u/sachachua/stream-manager") "View manager")
  ;; ("s" sacha-stream-toggle
	 ;; 	(format "Streaming [%s]"
	 ;; 					(if (eq sacha-stream-type 'stream) "X" " "))
	 ;; 	:exit t
	 ;; 	:column "Streaming/recording")
  ("r" sacha-recording-toggle
		(format "Recording [%s]"
						(if (eq sacha-stream-type 'record) "X" " "))
		:exit t)
  ("r" (org-capture nil "y") "Capture" :column "During")
	("o" (org-open-link-from-string "[[file:~/proj/stream/index.org::#plans]]")
	 "Notes"
	 :exit t)
	("m" sacha-stream-message "Message" :exit t)
	("p" sacha-stream-publish-and-sync-notes "Publish" :exit t)
  ("v" (sacha-play-latest-recording) "Play last" :exit t))
(keymap-global-set "<f8>" #'sacha-stream/body)
(keymap-global-set "s-r" #'sacha-stream/body)
(keymap-global-set "s-R" #'ignore)
(keymap-global-set "s-v" #'sacha-stream/body)
(keymap-global-set "s-SPC" #'sacha-stream/body)
;; General streaming configuration:1 ends here

;; [[file:Sacha.org::#playing-recordings][Playing recordings:1]]
(use-package mpv :if sacha-laptop-p :defer t :commands mpv)
;; Playing recordings:1 ends here

;; [[file:Sacha.org::#stream-notes][Stream notes:2]]
(with-eval-after-load 'org
	(add-hook 'org-mode-hook 'sacha-org-save-and-tangle-stream-notes))
;; based on https://www.reddit.com/r/emacs/comments/57nps0/comment/d8umsr4/?context=3
(setq imp-default-user-filters '((org-mode . sacha-impatient-org-export-as-html-filter)
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
(keymap-global-set  "<f11>" 'sacha-stream-captions-edit-last)
;; Try continuous streaming and the Google Speech Recognition API:2 ends here

;; [[file:Sacha.org::#ledger-personal-finance-in-sacha-config][Ledger:1]]
(use-package ledger-mode
  :mode "\\.ledger$"
  :bind (:map ledger-mode-map
              ("C-c C-n" . sacha-ledger-change-account)
              ("C-c a" . sacha-ledger-set-unknown-account)
              ("C-c f" . (lambda () (interactive) (find-file (sacha-latest-file "~/Downloads"))))))
;; Ledger:1 ends here

;; [[file:Sacha.org::#ledger-personal-finance-in-sacha-config][Ledger:2]]
(use-package flycheck-ledger
  :after (flycheck ledger-mode)
	:hook (ledger-mode . flycheck-mode)
  :demand t)
;; Ledger:2 ends here

;; [[file:Sacha.org::#ssh-and-daemon][SSH and --daemon:2]]
(sacha-ssh-refresh)
;; SSH and --daemon:2 ends here

;; [[file:Sacha.org::#encryption][Encryption:1]]
(setq epa-file-encrypt-to '("sacha@sachachua.com"))
(setq epa-pinentry-mode 'loopback)
(setq epg-pinentry-mode 'loopback)
;; Encryption:1 ends here

;; [[file:Sacha.org::#oddmuse][Oddmuse:1]]
(use-package oddmuse
  :if sacha-laptop-p
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

;; [[file:Sacha.org::#plover][Plover:1]]
(use-package plover-websocket
  :load-path "~/proj/plover-websocket-el"
  :after websocket
  :if sacha-laptop-p
	:defer t
  :config (setq plover-websocket-plover-command "cd ~/vendor/plover; tox -e launch")
  :hydra
  (sacha-plover (:exit t)
             ("<f1>" plover-websocket-connect "Open websocket")
             ("<f2>" plover-websocket-add-translation "Add translation")
             ("<f3>" plover-websocket-lookup "Lookup")
             ("<f4>" plover-websocket-configure "Configure")
             ("<f5>" plover-websocket-focus "Focus")
             ("<f6>" plover-websocket-toggle-plover "Toggle Plover")
             ("<f7>" plover-websocket-quit "Quit")
             ("<f8>" sacha-plover-drilling-time "Drill"))
  :bind
  ("<f6>" . #'sacha-plover/body))

;; Plover:1 ends here

;; [[file:Sacha.org::#making-it-easier-to-execute-commands][Making it easier to execute commands:1]]
(setq enable-recursive-minibuffers t)
;; Making it easier to execute commands:1 ends here

;; [[file:Sacha.org::#stenoing-interface][Stenoing interface:2]]
(setq plover-websocket-stroke-buffer-name "*Stroke log*")
;; Stenoing interface:2 ends here

;; [[file:Sacha.org::#key-chord][Key chords:2]]
  (fset 'key-chord-define 'sacha-key-chord-define)
;; Key chords:2 ends here

;; [[file:Sacha.org::#key-chord][Key chords:3]]
  (use-package key-chord
    :if sacha-laptop-p
    :hydra (sacha-key-chord-commands
            ()
            "Main"
            ("k" kill-sexp)
            ("h" sacha-org-jump :color blue)
            ("x" sacha-org-finish-previous-task-and-clock-in-new-one "Finish and clock in" :color blue)
            ("b" helm-buffers-list :color blue)
            ("f" find-file :color blue)
            ("a" sacha-org-check-agenda :color blue)
            ("c" (call-interactively 'org-capture) "capture" :color blue)
            ("t" (org-capture nil "T") "Capture task")
            ("." repeat)
            ("C-t" transpose-chars)
            ("o" sacha-org-off-sacha-computer :color blue)
            ("w" sacha-engine-mode-hydra/body "web" :exit t)
            ("m" imenu :color blue)
            ("i" sacha-capture-timestamped-note-with-screenshot :exit t)
            ("n" sacha-capture-timestamped-note "Timestamped note" :exit t)
            ("q" quantified-track :color blue)
            ("r" sacha-describe-random-interactive-function)
            ("l" org-insert-last-stored-link)
            ("L" sacha-org-insert-link))
    :init
    (setq key-chord-one-key-delay 0.16)
    (setq key-chord-two-keys-delay 0.002)
    (key-chord-define-global "uu" 'undo)
    (key-chord-define-global "jr" 'sacha-goto-random-char-hydra/sacha-goto-random-char)
    (key-chord-define-global "kk" 'kill-whole-line)
    (key-chord-define-global "et" 'sacha-stream-message)
    (key-chord-define-global "em" 'embark-act)
    (key-chord-define-global ".t" 'sacha-stream/body)
    (key-chord-define-global "jj" 'avy-goto-word-1)
    (key-chord-define-global "yy" 'sacha-window-movement/body)
    (key-chord-define-global "jw" 'switch-window)
    (key-chord-define-global "jl" 'avy-goto-line)
    (key-chord-define-global "j." 'join-lines/body)
    (key-chord-define-global "FF" 'find-file)
    (key-chord-define-global "qq" 'sacha-quantified-hydra/body)
    (key-chord-define-global "hh" 'sacha-key-chord-commands/body)
    (key-chord-define-global "xx" 'er/expand-region)
    (key-chord-define-global "  " 'sacha-insert-space-or-expand)
    (key-chord-define-global "vv" 'god-mode-all)
    (key-chord-define-global "JJ" 'sacha-switch-to-previous-buffer)
    (key-chord-mode -1)) ;; disable for now
;; Key chords:3 ends here

;; [[file:Sacha.org::#key-chord][Key chords:4]]
  (bind-key "C-t" 'sacha-key-chord-commands/body)
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
  (pretty-hydra-define sacha-geeqie ()
    ("Open"
     (("oo" sacha-geeqie-setup "Setup")
			("op" (sacha-geeqie-view sacha-portfolio-directory) "Portfolio")
			("oc" (sacha-geeqie-view sacha-camera-directory) "Camera")
			("oi" (sacha-geeqie-view sacha-ipad-directory) "iPad")
			("ox" (sacha-geeqie-view "~/screenshots") "Screenshots")
			("os" sacha-geeqie-scans "Scans"))
     "Modify"
     (("[" sacha-geeqie-rotate-counterclockwise "CCW")
			("]" sacha-geeqie-rotate-clockwise "CW")
			("r" sacha-geeqie-rename-current "Rename")
			("d" sacha-geeqie-change-date "Change date")
			("c" sacha-geeqie-crop-to-rectangle "Crop")
			("k" (start-process "krita" nil "krita" (sacha-geeqie-filename)) "krita")
			("O" (shell-command (format "mogrify -auto-orient %s" (shell-quote-argument (sacha-geeqie-filename)))) "Rotate based on EXIF")
			("g" (start-process "gimp" nil "gimp" (sacha-geeqie-filename)) "gimp"))
     "Navigate"
     (("n" sacha-geeqie-next "Next")
			("p" sacha-geeqie-previous "Previous")
			("x" sacha-geeqie-delete-and-next "Delete"))
     "Save"
     (("p" (rename-file (sacha-geeqie-filename)
												(expand-file-name (file-name-nondirectory (sacha-geeqie-filename)) sacha-sketches-directory))
			 "Portfolio")
			("s" (rename-file (sacha-geeqie-filename)
												(expand-file-name (file-name-nondirectory (sacha-geeqie-filename)) sacha-sketches-directory))
			 "Sketch"))
     "Other"
     (("<up>" (forward-line -1) :hint nil)
			("<down>" forward-line :hint nil)

			("im" (insert (format "{{<photo nas=\"1\" src=\"%s\">}}" (sacha-geeqie-filename))))
			("if" (insert (sacha-geeqie-filename) "\n")
			 "Insert filename")
			("v" (sacha-geeqie-view (string-trim (thing-at-point 'line))) "View")
			("il" (insert "- " (sacha-geeqie-filename) "\n") "Insert filename as list item")))))
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
  :disabled t
	:vc (:url "https://github.com/iwahbe/chat.el"))
(use-package org-ai
  :disabled t
	:vc (:url "https://github.com/rksm/org-ai"))
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
  (setq sacha-gptel-groq
        (gptel-make-openai "Groq"
          :host "api.groq.com"
          :endpoint "/openai/v1/chat/completions"
          :stream t
          :key (gptel-api-key-from-environment "GROQ_API_KEY")
          :models '(llama-3.3-70b-versatile
                    llama-3.1-8b-instant
                    openai/gpt-oss-20b
                    openai/gpt-oss-120b)))
  (setq sacha-gptel-gemini
        (gptel-make-gemini "Gemini"
          :key (gptel-api-key-from-environment "GEMINI_API_KEY")
          :stream t
          :models '(gemini-3-flash-preview
                    gemini-2.5-flash
                    gemini-2.5-pro
                    gemini-2.5-flash-preview-09-2025
                    gemini-2.5-flash-lite)))
  (setq sacha-gptel-gemini-paid
        (gptel-make-gemini "Gemini - paid"
          :key (gptel-api-key-from-environment "GEMINI_PAID_API_KEY")
          :stream t
          :models '(gemini-3-flash-preview
                    gemini-2.5-flash
                    gemini-2.5-flash-preview-09-2025
                    gemini-2.5-flash-lite)))
  (setq sacha-gptel-mistral
        (gptel-make-openai "Mistral"
          :key (gptel-api-key-from-environment "MISTRAL_API_KEY")
          :host "api.mistral.ai"
          :endpoint "/v1/chat/completions"
          :stream t
          :models '(mistral-medium
                    mistral-large-2411)))
	(setq gptel-model 'gemini-3-flash-preview
				gptel-backend sacha-gptel-gemini
        gptel-log-level 'info)
	:hook
	(gptel-post-stream . gptel-auto-scroll)
	(gptel-post-response . gptel-end-of-response))

;; ChatGPT, AI, and large-language models:3 ends here

;; [[file:Sacha.org::#inactive-infrequent-things-chatgpt-ai-and-large-language-models-agent-shell][agent-shell:2]]
(use-package agent-shell
  :config
  (setopt agent-shell-preferred-agent-config (agent-shell-anthropic-make-claude-code-config))
  (setopt agent-shell-session-strategy 'prompt)
  (setopt agent-shell-dot-subdir-function #'sacha-agent-shell-dot-subdir)
)
;; agent-shell:2 ends here

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
  :disabled t
	:vc (:url "https://codeberg.org/akib/emacs-eagle.git")
	:defer t)
(use-package cube
	:vc (:url "https://codeberg.org/akib/emacs-cube.git")
	:defer t)
;; Rubik's Cube:1 ends here

;; [[file:Sacha.org::#minecraft][Minecraft:1]]
(use-package mcf
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
