;; This sets up the load path so that we can override it
(package-initialize)
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp")
(add-to-list 'load-path "~/vendor/org-mode/lisp")
(add-to-list 'load-path "~/vendor/org-mode/contrib/lisp")
(setq custom-file "~/.config/emacs/custom-settings.el")
(setq use-package-always-ensure t)
(load custom-file t)

(defvar my/laptop-p (equal (system-name) "sacha-kubuntu"))
(defvar my/server-p (and (equal (system-name) "localhost") (equal user-login-name "sacha")))
(defvar my/phone-p (not (null (getenv "ANDROID_ROOT")))
  "If non-nil, GNU Emacs is running on Termux.")
(when my/phone-p (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))
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
(use-package auto-compile
  :if my/laptop-p
  :config (auto-compile-on-load-mode))
(setq load-prefer-newer t)

(use-package hydra :commands defhydra)
(use-package use-package-hydra)
(use-package hydra-posframe :if my/laptop-p  :quelpa (hydra-posframe :fetcher github :repo "Ladicle/hydra-posframe") :after hydra)

(with-eval-after-load 'hydra
  (defhydra my/window-movement ()
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
  (defhydra my/shortcuts (:exit t)
    ("j" my/helm-journal "Journal")
    ("C" my/resolve-orgzly-syncthing "Conflicts")
    ("n" my/capture-timestamped-note "Note")
    ("c" my/org-categorize-emacs-news/body "Categorize")
    ("d" my/emacs-news-check-duplicates "Dupe")
    ("s" save-buffer "Save")
    ("f" my/file-shortcuts/body "File shortcut")
    ("+" text-scale-increase "Increase")  
    ("-" text-scale-decrease "Decrease")
    ("g" my/geeqie/body "Geeqie")
    ("l" (my/toggle-or-create "*scratch*" (lambda () (switch-to-buffer (startup--get-buffer-create-scratch)))) "Lisp")
    ("e" eshell-toggle "Eshell")
    ("w" my/engine-mode-hydra/body "Search web")
    ("E" my/emacs-news/body "Emacs News"))
  (global-set-key (kbd "<f5>") #'my/shortcuts/body)
  (defhydra my/emacs-news (:exit t)
    "Emacs News"
    ("f" (find-file "~/sync/emacs-news/index.org") "News")
    ("C" (find-file "~/code/emacs-calendar/README.org") "Calendar")
    ("C" (find-file "/ssh:web:/var/www/emacslife.com/calendar/README.org" "Calendar on server"))
    ("d" my/emacs-news-check-duplicates "Dupe")
    ("c" my/org-categorize-emacs-news/body "Categorize")
    ("h" (my/org-update-link-description "HN") "Link HN")
    ("i" (my/org-update-link-description "Irreal") "Link Irreal")
    ("m" my/share-emacs-news "Mail")
    ("t" (browse-url "https://tweetdeck.twitter.com") "Twitter")))

(defun my/org-update-link-description (description)
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

(defun my/org-insert-link ()
  (interactive)
  (when (org-in-regexp org-bracket-link-regexp 1)
    (goto-char (match-end 0))
    (insert "\n"))
  (call-interactively 'org-insert-link))

(defun my/switch-to-previous-buffer ()
  "Switch to previously open buffer.
      Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun my/org-check-agenda ()
  "Peek at agenda."
  (interactive)
  (cond
   ((derived-mode-p 'org-agenda-mode)
    (if (window-parent) (delete-window) (bury-buffer)))
   ((get-buffer "*Org Agenda*")
    (switch-to-buffer-other-window "*Org Agenda*"))
   (t (org-agenda nil "a"))))

(defun my/goto-random-char ()
  (interactive)
  (goto-char (random (point-max))))

(defvar hydra-stack nil)

(defun hydra-push (expr)
  (push `(lambda () ,expr) hydra-stack))

(defun hydra-pop ()
  (interactive)
  (let ((x (pop hydra-stack)))
    (when x (funcall x))))

(defun hydra-go-and-push (expr)
  (push hydra-curr-body-fn hydra-stack)
  (prin1 hydra-stack)
  (funcall expr))

;; example (progn (hydra-b/body) (hydra-push '(hydra-a/body)))
;; or   ("q" hydra-pop "exit")

(defun my/hydra-format-head (h)
  (let ((key-binding (elt h 0))
        (hint (elt h 2))
        (cmd (and (elt h 1) (prin1-to-string (elt h 1)))))
    (if cmd
        (format "%s (%s) - %s" hint key-binding cmd)
      (format "%s (%s)" hint key-binding))))

(defun my/hydra-heads-to-candidates (base)
  (mapcar (lambda (h)
            (cons (my/hydra-format-head h) (hydra--head-name h base)))
          (symbol-value (intern (concat (symbol-name base) "/heads")))))

(defun my/hydra-execute-extended (&optional prefixarg hydra-base)
  (declare (command-execute))
  (interactive (list current-prefix-arg nil))
  (hydra-keyboard-quit)
  (let* ((candidates (my/hydra-heads-to-candidates
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
  (define-key hydra-base-map (kbd "<tab>") #'my/hydra-execute-extended))

(defun my/reload-emacs-configuration ()
  (interactive)
  (load-file "~/code/.emacs.d/Sacha.el"))

(use-package dash :ensure t)
(use-package diminish :ensure t)

(setq backup-directory-alist '(("." . "~/.config/emacs/backups")))

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

(use-package selectrum :quelpa (selectrum :fetcher github :repo "raxod502/selectrum") :init (selectrum-mode +1)) 
(use-package prescient :config (prescient-persist-mode +1))
(use-package selectrum-prescient :init (selectrum-prescient-mode +1) :after selectrum)
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
         ("M-y" . consult-yank-pop)
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
  :config
  (setq marginalia-annotators (if my/laptop-p
                                  '(marginalia-annotators-heavy marginalia-annotators-light)
                                '(marginalia-annotators-light)))
  (advice-add #'marginalia-cycle :after
              (lambda () (when (bound-and-true-p selectrum-mode) (selectrum-exhibit))))
  :bind (:map minibuffer-local-completion-map
              ("M-A" . marginalia-cycle)
              ("C-i" . marginalia-cycle-annotators)))
(defun my/sketch-insert-file-as-link (f)
  (interactive "fSketch: ")
  (insert (org-link-make-string (concat "sketch:" (file-name-nondirectory f))) "\n"))
(use-package embark 
  :after selectrum 
  :config
  (setq embark-prompter 'embark-keymap-prompter) 
  (add-to-list 'embark-target-finders 'my/embark-org-element) 
  (add-to-list 'embark-allow-edit-commands #'my/stream-message)
  (add-to-list 'embark-allow-edit-commands #'my/journal-post)
  (embark-define-keymap embark-sketch-actions
    "Org Mode sketch-related actions"
    ("o" my/sketch-insert-file-as-link)
    ("v" my/geeqie-view))
  (embark-define-keymap embark-journal-actions
    "Journal"
    ("e" my/journal-edit))
  (add-to-list 'embark-keymap-alist '(sketch . embark-sketch-actions))
  (add-to-list 'embark-keymap-alist '(journal . embark-journal-actions))
  :bind
  (:map minibuffer-local-map
        (("C-c e" . embark-act)
         ("C-;" . embark-act))
        :map embark-collect-mode-map
        (("C-c e" . embark-act)
         ("C-;" . embark-act))
        :map embark-general-map
        (("j" . my/journal-post)
         ("m" . my/stream-message))
        :map embark-variable-map
        ("l" . edit-list)))

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
  (setq my/consult-source-projectile-projects
        `(:name "Projectile projects"
                :narrow   ?P
                :category project
                :action   ,#'projectile-switch-project-by-name
                :items    ,projectile-known-projects))
  (add-to-list 'consult-buffer-sources my/consult-source-projectile-projects 'append))

(defun my/date-from-filename (filename)
  (let ((f (file-name-nondirectory filename)))
    (if (string-match "^[-0-9]+" f)
        (replace-regexp-in-string "[^0-9]" "" (match-string 0 f))
      nil)))

(defvar my/sketches nil "Cache for sketch filenames.")
(defun my/update-sketch-cache ()
  (interactive)
  (setq my/sketches (sort
                          (apply 'append (mapcar (lambda (dir)
                                                   (directory-files dir t "\\.\\(jpe?g\\|png\\|svg\\)$"))
                                                 my/sketch-directories))
                          (lambda (a b)
                            (string< (concat (or (my/date-from-filename b) "0") (file-name-nondirectory b))
                                     (concat (or (my/date-from-filename a) "0") (file-name-nondirectory a)) )))))

(defun my/preview-image (candidate state)
  (when candidate (my/geeqie-view (list candidate)))
  nil)

(defun my/complete-sketch-filename ()
  (interactive)
  (consult--read (or my/sketches (my/update-sketch-cache))
   :sort nil
   :state 'my/preview-image
   :prompt "Sketch: "
   :category 'sketch))

(use-package marginalia
  :config
  (add-to-list 'marginalia-prompt-categories '("sketch" . sketch)))

(defun my/marginalia-annotate-variable (cand)
  "Annotate variable CAND with its documentation string."
  (when-let (sym (intern-soft cand))
    (marginalia--fields
     ((marginalia--symbol-class sym) :face 'marginalia-modified)
     ((let ((print-escape-newlines t)
            (print-escape-control-characters t)
            (print-escape-multibyte t))
        (prin1-to-string
         (cond
          ((string-match "pass" cand) "*******")
          ((boundp sym) (symbol-value sym))
          (t 'unbound))))
      :truncate (/ marginalia-truncate-width 3) :face 'marginalia-variable)
     ((documentation-property sym 'variable-documentation)
      :truncate marginalia-truncate-width :face 'marginalia-documentation))))

(use-package marginalia
  :config
  (setcdr (assoc 'variable marginalia-annotators-heavy) #'my/marginalia-annotate-variable))

(require 'elisp-mode)

(defun my/elisp-get-function-args (sym)
  "Return a string with the function arguments for SYM.
Based on `elisp-get-fnsym-args-string.'"
  (cond
	 ((not (and sym (symbolp sym) (fboundp sym))) nil)
	 ((and (eq sym (aref elisp--eldoc-last-data 0))
		     (eq 'function (aref elisp--eldoc-last-data 2)))
	  (aref elisp--eldoc-last-data 1))
	 (t
	  (let* ((advertised (gethash (indirect-function sym)
                                advertised-signature-table t))
           doc
		       (args
		        (cond
		         ((listp advertised) advertised)
		         ((setq doc (help-split-fundoc
				                 (condition-case nil (documentation sym t)
				                   (invalid-function nil))
				                 sym))
		          (substitute-command-keys (car doc)))
		         (t (help-function-arglist sym)))))
      ;; Stringify, and store before highlighting, downcasing, etc.
	    (elisp-function-argstring args)))))
(defun my/marginalia-annotate-journal (cand)
  (when-let ((o (cdr (assoc cand my/journal-search-cache))))
    (marginalia--fields
     ((plist-get o :Category)
      :face 'marginalia-documentation
      :truncate 13))))

(defun my/marginalia-annotate-function-with-args (cand)
  "Annotate symbol CAND with its arguments and documentation string."
  (when-let (sym (intern-soft cand))
    (let ((symbol-class-width 5))
      (marginalia--fields
       ((marginalia--symbol-class sym) :face 'marginalia-modified
        :truncate symbol-class-width)
       ((my/elisp-get-function-args sym) 
        :truncate (/ (- marginalia-truncate-width symbol-class-width) 3)
        :face 'my/marginalia-arguments)
       ((marginalia--function-doc sym)
        :truncate marginalia-truncate-width
        :face 'marginalia-documentation)))))
(defvar my/marginalia-function-width 30 "Width of variable value annotation string.")
(defface my/marginalia-arguments '((t :inherit marginalia-key))
  "Face used to highlight function arguments in `marginalia-mode'."
  :group 'marginalia)
(use-package marginalia
  :after elisp-mode
  :config
  (add-to-list 'marginalia-annotators-heavy (cons 'journal #'my/marginalia-annotate-journal))
  (add-to-list 'marginalia-annotators-heavy (cons 'function #'my/marginalia-annotate-function-with-args))
  (add-to-list 'marginalia-prompt-categories (cons "\\<function\\>" 'function)))

(let ((foo '"bar"))
(defun my/embark-org-element () 
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

(defun my/embark-org-src-block-copy-noweb-reference (element) 
  (kill-new (if (org-element-property element :parameters) 
                (format "<<%s(%s)>>" (org-element-property element :name) 
                        (org-element-property element :parameters)) 
              (format "<<%s>>" (org-element-property element :parameters)))))
)

(defun my/refresh-selectrum () 
  (setq selectrum--previous-input-string nil))
(defun my/store-action-key+cmd (cmd) 
  (setq keycast--this-command-keys (this-single-command-keys) keycast--this-command cmd))
(defun my/force-keycast-update (&rest _) 
  (force-mode-line-update t))
(use-package keycast
  :if my/laptop-p
  :after embark 
  :config (dolist (cmd '(embark-act embark-act-noexit embark-become)) 
            (advice-add cmd 
                        :before #'my/force-keycast-update)))

(defun my/shrink-selectrum () 
  (when (eq embark-collect--kind :live) 
    (with-selected-window (active-minibuffer-window) 
      (setq-local selectrum-num-candidates-displayed 1) 
      (setq-local selectrum-display-style '(horizontal :before-candidates "[" 
                                                       :after-candidates "]" 
                                                       :more-candidates "" 
                                                       :candidates-separator "")))))
(use-package 
  embark 
  :config
                                        ;(setq embark-prompter 'embark-completing-read-prompter)
  (advice-add 'embark-keymap-prompter :filter-return #'my/store-action-key+cmd) 
  (add-to-list 'embark-allow-edit-commands #'my/stream-message) 
  (add-hook 'embark-collect-mode-hook #'my/shrink-selectrum) 
  (add-hook 'embark-pre-action-hook #'my/refresh-selectrum))

(defun my/cmap-org-link-element-target ()
  "Org-mode link target. Returns the element."
  (when (derived-mode-p 'org-mode)
    (let ((context (org-element-context)))
      (when (eq (org-element-type context) 'link)
        (cons 'my/cmap-org-link-element-map context)))))
    
(defun my/cmap-org-block-target ()
	(when (and (derived-mode-p 'org-mode)
		         (org-in-src-block-p))
	  (cons 'my/cmap-org-block-map 'cmap-no-arg)))
(defun my/org-indent-block ()
	(interactive)
	(save-excursion
	  (unless (looking-at "^[ \t]*#\\+begin")
	    (re-search-backward "^[ \t]*#\\+begin" nil t))
	  (org-indent-block)))
(defun my/org-copy-block-contents ()
	(interactive)
	(kill-new (org-element-property :value (org-element-context))))
(defun my/org-link-element-copy-link (element)
  (interactive (list (org-element-context)))
  (kill-new (org-element-property :raw-link element)))

(use-package cmap :quelpa (cmap :fetcher github :repo "jyp/cmap")
	:config
	(add-to-list 'cmap-targets #'my/cmap-org-block-target)
  (add-to-list 'cmap-targets #'my/cmap-org-link-element-target)
  (defvar my/cmap-org-link-element-map
    (cmap-keymap
      ("w" . my/org-link-element-copy-link)
      ("c" . my/caption-show)))
  (add-to-list 'which-key-replacement-alist '((nil . "^my/org-link-element-") . (nil . "")))
	(defvar my/cmap-org-block-map
	  (cmap-keymap
	    ("w" . my/org-copy-block-contents)
	    ("i" . my/org-indent-block)))
	:bind (("C-c e" . cmap-cmap)))

(use-package helm
  :diminish helm-mode
  :if my/laptop-p
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
(use-package helm-ls-git
  :if my/laptop-p)

(use-package helm-descbinds
  :defer t
  :if my/laptop-p
  :bind (("C-h b" . helm-descbinds)
         ("C-h w" . helm-descbinds)))

(defvar my/book-notes-directory "~/Dropbox/books")
(defun my/helm-do-grep-book-notes ()
  "Search my book notes."
  (interactive)
  (helm-do-grep-1 (list my/book-notes-directory)))

(ert-deftest my/org-capture-prefill-template ()
  (should
   ;; It should fill things in one field at ia time
   (string=
    (my/org-capture-prefill-template
     "* TODO %^{Task}\nSCHEDULED: %^t\n:PROPERTIES:\n:Effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}\n:END:\n%?\n"
     "Hello World")
    "* TODO Hello World\nSCHEDULED: %^t\n:PROPERTIES:\n:Effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}\n:END:\n%?\n"
    ))
  (should
   (string=
    (my/org-capture-prefill-template
     "* TODO %^{Task}\nSCHEDULED: %^t\n:PROPERTIES:\n:Effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}\n:END:\n%?\n"
     "Hello World" "<2015-01-01>")
    "* TODO Hello World\nSCHEDULED: <2015-01-01>\n:PROPERTIES:\n:Effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}\n:END:\n%?\n"))
  (should
   (string=
    (my/org-capture-prefill-template
     "* TODO %^{Task}\nSCHEDULED: %^t\n:PROPERTIES:\n:Effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}\n:END:\n%?\n"
     "Hello World" "<2015-01-01>" "0:05")
    "* TODO Hello World\nSCHEDULED: <2015-01-01>\n:PROPERTIES:\n:Effort: 0:05\n:END:\n%?\n")))

(declare-function org-capture-get "org-capture")
(defun my/org-capture-prefill-template (template &rest values)
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

(defun my/org-get-current-refile-location ()
  "Return the current entry as a location understood by org-refile."
  (interactive)
  (list (elt (org-heading-components) 4)
        (or buffer-file-name
            (with-current-buffer (buffer-base-buffer (current-buffer))
              buffer-file-name))
        nil
        (point)))

(defun my/helm-org-create-task (candidate)
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
            (my/org-capture-prefill-template (org-capture-get :template)
                                             candidate)))
          (org-capture-place-template
           (equal (car (org-capture-get :target)) 'function))
          (setq org-refile-target-table (org-refile-get-targets))
          ;; Return the new location
          (my/org-get-current-refile-location))
      ((error quit)
       (if (get-buffer "*Capture*") (kill-buffer "*Capture*"))
       (error "Capture abort: %s" error)))))

;; (my/org-refile-get-location-by-substring "Try again")

(defvar my/helm-org-refile-locations nil)
(defvar my/org-refile-last-location nil)

(defun my/helm-org-clock-in-and-track-from-refile (candidate)
  (let ((location (org-refile--get-location candidate my/helm-org-refile-locations)))
    (save-window-excursion
      (org-refile 4 nil location)
      (my/org-clock-in-and-track)
      t)))

(defun my/org-get-todays-items-as-refile-candidates ()
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
    (save-window-excursion (my/org-get-entries-fn (calendar-current-date) (calendar-current-date))))))

;; Based on http://emacs.stackexchange.com/questions/4063/how-to-get-the-raw-data-for-an-org-mode-agenda-without-an-agenda-view
(defun my/org-get-entries-fn (begin end)
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

(defun my/helm-org-refile-read-location (tbl)
  (setq my/helm-org-refile-locations tbl)
  (helm
   (list
    ;; (helm-build-sync-source "Today's tasks"
    ;;   :candidates (mapcar (lambda (a) (cons (car a) a))
    ;;                       (my/org-get-todays-items-as-refile-candidates))
    ;;   :action '(("Select" . identity)
    ;;             ("Clock in and track" . my/helm-org-clock-in-and-track-from-refile)
    ;;             ("Draw index card" . my/helm-org-prepare-index-card-for-subtree))
    ;;   :history 'org-refile-history)
    (helm-build-sync-source "Refile targets"
      :candidates (mapcar (lambda (a) (cons (car a) a)) tbl)
      :action '(("Select" . identity)
                ("Clock in and track" . my/helm-org-clock-in-and-track-from-refile)
                ("Draw index card" . my/helm-org-prepare-index-card-for-subtree))
      :history 'org-refile-history)
    (helm-build-dummy-source "Create task"
      :action (helm-make-actions
               "Create task"
               'my/helm-org-create-task)))))

(defun my/org-refile-get-location (&optional prompt default-buffer new-nodes no-exclude)
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
      (setq answ (my/helm-org-refile-read-location tbl))
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
        (setq my/org-refile-last-location pa)
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

;(fset 'org-refile-get-location 'my/org-refile-get-location)

(defvar my/screenshot-directory "~/screenshots")
(defun my/org-insert-screenshot (file &optional note)
  (interactive (list
                (if current-prefix-arg
                    (expand-file-name
                     (consult--read
                      (reverse (directory-files my/screenshot-directory nil "\\.png$"))
                      :sort nil
                      :require-match t
                      :category 'file
                      :state (lambda (candidate state)
                               (when candidate
                                 (with-current-buffer (find-file-noselect (expand-file-name candidate my/screenshot-directory))
                                   (display-buffer (current-buffer))))))
                     my/screenshot-directory)
                  (my/latest-file my/screenshot-directory))))
  (save-window-excursion
    (with-current-buffer (find-file-noselect file) (display-buffer (current-buffer)))  
    (insert "#+CAPTION: " (or note (read-string "Caption: "))))
  (save-excursion (insert "\n" (org-link-make-string (concat "file:" file)) "\n")))

(defun my/org-capture-prefill-template (template &rest values)
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

(defun my/capture-timestamped-note (time note)
  "Disable Helm and capture a quick timestamped note."
  (interactive (list (current-time) (read-string "Note: ")))
  (let ((helm-completing-read-handlers-alist '((org-capture . nil)))
        (entry (org-capture-select-template "p")))
    (org-capture-set-plist entry)
    (org-capture-get-template)
    (org-capture-set-target-location)
    (org-capture-put
     :template (org-capture-fill-template
                (my/org-capture-prefill-template (org-capture-get :template)
                                                 (format-time-string "%H:%M:%S,%3N")
                                                 note)))
    (org-capture-place-template)
    (org-capture-finalize)))

(defun my/capture-timestamped-note-with-screenshot (time note)
  "Include a link to the latest screenshot."
  (interactive (list (current-time) (read-string "Note: ")))
  (kill-new (my/latest-file my/screenshot-directory))
  (my/capture-timestamped-note time note))

(use-package recomplete
  :if my/laptop-p
  :quelpa (recomplete :fetcher gitlab :repo "ideasman42/emacs-recomplete")
  :bind ("M-/" . recomplete-dabbrev))

(use-package smart-mode-line)

(fset 'yes-or-no-p 'y-or-n-p)

(use-package miniedit
  :commands minibuffer-edit
  :init (miniedit-install))

(defun my/setup-color-theme ()
  (interactive)
  (when (display-graphic-p) 
    (color-theme-sanityinc-solarized-dark))
  (set-background-color "black")
  (set-face-foreground 'secondary-selection "darkblue")
  (set-face-background 'secondary-selection "lightblue")
  (set-face-background 'font-lock-doc-face "black")
  (set-face-foreground 'font-lock-doc-face "wheat")
  (set-face-background 'font-lock-string-face "black"))
(use-package color-theme-sanityinc-solarized :config (my/setup-color-theme))

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
    (setq undo-tree-visualizer-diff t)))

(use-package which-key :init (which-key-mode 1))
(use-package which-key-posframe :if my/laptop-p :init (which-key-posframe-mode 1))

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

(defun my/def-rep-command (alist)
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

(bind-key "C-x p" 'pop-to-mark-command)
(setq set-mark-command-repeat-pop t)

(use-package helm-swoop
  :if my/laptop-p
  :bind
  (("C-S-s" . helm-swoop)
   ("M-i" . helm-swoop)
   ("M-s s" . helm-swoop)
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
(defvar my/refile-map (make-sparse-keymap))
(require 'bookmark)
(defmacro my/defshortcut (key file)
  `(progn
     (set-register ,key (cons 'file ,file))
     (bookmark-store ,file (list (cons 'filename ,file)
                                 (cons 'position 1)
                                 (cons 'front-context-string "")) nil)
     (define-key my/refile-map
       (char-to-string ,key)
       (lambda (prefix)
         (interactive "p")
         (let ((org-refile-targets '(((,file) :maxlevel . 6)))
               (current-prefix-arg (or current-prefix-arg '(4))))
           (call-interactively 'org-refile))))))


(define-key my/refile-map "," 'my/org-refile-to-previous-in-file)

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
         (mapcar (lambda (h) (list 'my/defshortcut (string-to-char (elt h 0)) (elt h 1)))
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
         (mapcar (lambda (h) (list 'my/defshortcut (string-to-char (elt h 0)) (elt h 1)))
                 heads))))

(use-package hydra
  :config
  (defshortcuts my/file-shortcuts ()
    ("C" "~/code/emacs-calendar/README.org" "Emacs calendar")
    ("e" "~/code/.emacs.d/Sacha.org" "Config")
    ("E" "~/sync/emacs-news/index.org" "Emacs News")
    ("f" "~/code/font/README.org" "Font")
    ("i" "~/orgzly/computer-inbox.org" "Computer inbox")
    ("I" "~/orgzly/Inbox.org" "Phone inbox")
    ("o" "~/orgzly/organizer.org" "Main org file")
    ("s" "~/code/stream/notes.org" "Public Emacs notes")
    ("b" "~/personal/business.org" "Business")
    ("p" "/ssh:web:/mnt/prev/home/sacha/planet/en.ini" "Planet Emacsen")
    ("B" "/ssh:web|sudo::/etc/nginx/sites-available" "Nginx sites")
    ("w" "~/Dropbox/public/sharing/index.org" "Sharing index")
    ("W" "~/Dropbox/public/sharing/blog.org" "Blog index")
    ("1" "~/code/static-blog/" "Static blog")
    ("r" "~/personal/reviews.org" "Reviews")
    ("g" "~/code/sachac.github.io/evil-plans/index.org" "Evil plans"))
  :bind
  ("C-c f" . #'my/file-shortcuts/body))

(defun my/key-chord-define (keymap keys command)
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
(fset 'key-chord-define 'my/key-chord-define)

(use-package key-chord
  :if my/laptop-p
  :hydra (my/key-chord-commands
          ()
          "Main"
          ("k" kill-sexp)
          ("h" my/org-jump :color blue)
          ("x" my/org-finish-previous-task-and-clock-in-new-one "Finish and clock in" :color blue)
          ("b" helm-buffers-list :color blue)
          ("f" find-file :color blue)
          ("a" my/org-check-agenda :color blue)
          ("c" (call-interactively 'org-capture) "capture" :color blue)
          ("t" (org-capture nil "T") "Capture task")
          ("." repeat)
          ("C-t" transpose-chars)
          ("o" my/org-off-my-computer :color blue)
          ("w" my/engine-mode-hydra/body "web" :exit t)
          ("m" imenu :color blue)
          ("i" my/capture-timestamped-note-with-screenshot :exit t)
          ("n" my/capture-timestamped-note "Timestamped note" :exit t)
          ("q" quantified-track :color blue)
          ("r" my/describe-random-interactive-function)
          ("l" org-insert-last-stored-link)
          ("L" my/org-insert-link))
  :init
  (setq key-chord-one-key-delay 0.16)
  (setq key-chord-two-keys-delay 0.002)
  (key-chord-define-global "uu" 'undo)
  (key-chord-define-global "jr" 'my/goto-random-char-hydra/my/goto-random-char)
  (key-chord-define-global "kk" 'kill-whole-line)
  (key-chord-define-global "et" 'my/stream-message)
  (key-chord-define-global "em" 'embark-act)
  (key-chord-define-global ".t" 'my/stream/body)
  (key-chord-define-global "jj" 'avy-goto-word-1)
  (key-chord-define-global "yy" 'my/window-movement/body)
  (key-chord-define-global "jw" 'switch-window)
  (key-chord-define-global "jl" 'avy-goto-line)
  (key-chord-define-global "j." 'join-lines/body)
  (key-chord-define-global "FF" 'find-file)
  (key-chord-define-global "qq" 'my/quantified-hydra/body)
  (key-chord-define-global "hh" 'my/key-chord-commands/body)
  (key-chord-define-global "xx" 'er/expand-region)
  (key-chord-define-global "  " 'my/insert-space-or-expand)
  (key-chord-define-global "vv" 'god-mode-all)
  (key-chord-define-global "JJ" 'my/switch-to-previous-buffer)
  (key-chord-mode -1)) ;; disable for now

(bind-key "C-t" 'my/key-chord-commands/body)

(use-package smartscan
  :if my/laptop-p
  :defer t
  :config (global-smartscan-mode t))

(require 'find-dired)
(setq find-ls-option '("-print0 | xargs -0 ls -ld" . "-ld"))

(defun my/save-photo (name)
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
      (copy-file file (expand-file-name new-name my/kid-photo-directory)))
    (rename-file file (expand-file-name new-name "~/archives/2016/photos/selected/"))))
(defun my/backup-media ()
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
(bind-key "b" 'my/save-photo dired-mode-map)
(bind-key "r" 'my/backup-media dired-mode-map)

(defun my/smarter-move-beginning-of-line (arg)
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
                'my/smarter-move-beginning-of-line)

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

(defun my/toggle-or-create (buffer-name buffer-create-fn &optional switch-cont)
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

(defun xah-toggle-margin-right ()
  "Toggle the right margin between `fill-column' or window width.
     This command is convenient when reading novel, documentation."
  (interactive)
  (if (eq (cdr (window-margins)) nil)
      (set-window-margins nil 0 (- (window-body-width) fill-column))
    (set-window-margins nil 0 0)))

(use-package pdf-tools
  :if my/laptop-p
  :config
  (pdf-tools-install)
  (setq pdf-view-resize-factor 1.1)
  (setq-default pdf-view-display-size 'fit-page)
  )

(defun my/shuffle-lines-in-region (beg end)
  (interactive "r")
  (let ((list (split-string (buffer-substring beg end) "[\r\n]+")))
    (delete-region beg end)
    (insert (mapconcat 'identity (shuffle-list list) "\n"))))

(use-package markdown-mode
  :if my/laptop-p
  :mode ("\\.\\(njk\\|md\\)\\'" . markdown-mode))

(use-package artbollocks-mode
  :if my/laptop-p
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

(defun my/unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn
                 (barf-if-buffer-read-only)
                 (list t)))
  (let ((fill-column (point-max)))
    (fill-paragraph nil region)))
(bind-key "M-Q" 'my/unfill-paragraph)

(defun my/fill-or-unfill-paragraph (&optional unfill region)
  "Fill paragraph (or REGION).
        With the prefix argument UNFILL, unfill it instead."
  (interactive (progn
                 (barf-if-buffer-read-only)
                 (list (if current-prefix-arg 'unfill) t)))
  (let ((fill-column (if unfill (point-max) fill-column)))
    (fill-paragraph nil region)))
(bind-key "M-q" 'my/fill-or-unfill-paragraph)

(remove-hook 'text-mode-hook #'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

(defmacro my/insert-unicode (unicode-name)
  `(lambda () (interactive)
     (insert-char (cdr (assoc-string ,unicode-name (ucs-names))))))
(bind-key "C-x 8 s" (my/insert-unicode "ZERO WIDTH SPACE"))
(bind-key "C-x 8 S" (my/insert-unicode "SNOWMAN"))

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

(defun my/subed-get-region-start-stop (beg end)
  (interactive "r")
  (cons (save-excursion
          (goto-char (min beg end))
          (subed-subtitle-msecs-start))
        (save-excursion
          (goto-char (max beg end))
          (subed-subtitle-msecs-stop))))

(defun my/extend-file-name (original name &optional extension)
  "Add NAME to the end of ORIGINAL, before the file extension."
  (concat (file-name-sans-extension original) " " name "."
          (or extension (file-name-extension original))))

(defun my/adjust-subtitles (offset)
  "Change all of the start and end times by OFFSET."
  (interactive (list (subed--string-to-msecs (read-string "Time: "))))
  (subed-for-each-subtitle (point-min) (point-max) nil
    (subed-adjust-subtitle-time-start offset t t)
    (subed-adjust-subtitle-time-stop offset t t))
  (subed-regenerate-ids))

(defun my/subed-write-adjusted-subtitles (source-file start-msecs end-msecs dest-file)
  (let ((s (with-current-buffer (find-file-noselect source-file)
             (buffer-substring-no-properties
              (subed-jump-to-subtitle-id-at-msecs start-msecs)
              (progn (subed-jump-to-subtitle-id-at-msecs end-msecs) (subed-jump-to-subtitle-end)))))
        (offset (- start-msecs)))
    (with-current-buffer (find-file-noselect dest-file)
      (erase-buffer)
      (insert s)
      (my/adjust-subtitles offset)
      (save-buffer)
      (buffer-file-name))))

(defun my/msecs-to-timestamp (msecs)
  "Convert MSECS to string in the format HH:MM:SS.MS."
  (concat (format-seconds "%02h:%02m:%02s" (/ msecs 1000))
          "." (format "%03d" (mod msecs 1000))))

(defun my/subed-make-animated-gif (beg end name)
  (interactive "r\nMName: ")
  (let* ((video-file (subed-guess-video-file))
         (msecs (my/subed-get-region-start-stop beg end))
         (new-file (my/extend-file-name video-file name "gif"))
         cmd)
    (when (> (length name) 0)
      (setq cmd
            (format "ffmpeg -y -i %s -ss %s -t %s -vf subtitles=%s -r 10 -c:a copy -shortest -async 1 %s"
                    (shell-quote-argument video-file)
                    (my/msecs-to-timestamp (car msecs))
                    (my/msecs-to-timestamp (- (cdr msecs) (car msecs)))
                    (shell-quote-argument (my/subed-write-adjusted-subtitles beg end name))                
                    (shell-quote-argument new-file)))
      (message "%s" cmd)
      (kill-new cmd)
      (shell-command cmd))))

(defun my/subed-ffmpeg-make-mute-filter (segments)
  (mapconcat
   (lambda (s)
     (format "volume=enable='between(t,%.3f,%.3f)':volume=0"
             (/ (car s) 1000.0)
             (/ (cdr s) 1000.0)))
   segments ", "))







(defun my/subed-cut-video (beg end name video-file caption-file)
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
      ((msecs (my/subed-get-region-start-stop beg end))
       (new-file name)
       (mute (my/subed-get-mute-segments))
       cmd)
    (when (> (length name) 0)
      (setq cmd
            (format "ffmpeg -y -i %s -i %s -ss %s -t %s %s -c:v copy -c:s copy -shortest -async 1 %s"
                    (shell-quote-argument caption-file)
                    (shell-quote-argument video-file)
                    (my/msecs-to-timestamp
                     (car msecs))
                    (my/msecs-to-timestamp
                     (-
                      (cdr msecs)
                      (car msecs)))
                    (if mute
                        (format "-af %s"
                                (shell-quote-argument
                                 (my/subed-ffmpeg-make-mute-filter mute)))
                      "-c:a copy")
                    (shell-quote-argument new-file)))
      (message "%s" cmd)
      (kill-new cmd))))

(define-minor-mode my/subed-hide-nontext-minor-mode
  "Minor mode for hiding non-text stuff.")
(defun my/subed-hide-nontext-overlay (start end)
  (let ((new-overlay (make-overlay start end)))
    (overlay-put new-overlay 'invisible t)
    (overlay-put new-overlay 'intangible t)
    (overlay-put new-overlay 'evaporate t)
    (overlay-put new-overlay 'read-only t)
    (overlay-put new-overlay 'hide-non-text t)
    (with-silent-modifications
      (add-text-properties start end '(read-only t)))
    new-overlay))

(defun my/subed-hide-nontext ()
  (interactive)
  (remove-overlays (point-min) (point-max) 'invisible t)
  (when my/subed-hide-nontext-minor-mode
    (save-excursion
      (goto-char (point-min))
      (subed-jump-to-subtitle-id)
      (my/subed-hide-nontext-overlay (point-min) (subed-jump-to-subtitle-text))
      (let (next)
        (while (setq next (save-excursion (subed-forward-subtitle-text)))
          (subed-jump-to-subtitle-end)
          (my/subed-hide-nontext-overlay (1+ (point)) (1- next))
          (subed-forward-subtitle-text))))))

(defun my/subed-show-all ()
  (interactive)
  (let ((inhibit-read-only t))
    (with-silent-modifications
      (remove-text-properties (point-min) (point-max) '(read-only t))
      (remove-overlays (point-min) (point-max) 'invisible t))))

(defun my/ignore-read-only (f &rest args)
  (let ((inhibit-read-only t))
    (apply f args)
    (my/subed-hide-nontext)))

(advice-add 'subed-split-and-merge-dwim :around #'my/ignore-read-only)
(advice-add 'subed-split-subtitle :around #'my/ignore-read-only)
(advice-add 'subed-merge-with-next :around #'my/ignore-read-only)
(advice-add 'subed-merge-with-previous :around #'my/ignore-read-only)
(advice-add 'subed-regenerate-ids :around #'my/ignore-read-only)
(advice-add 'subed-kill-subtitle :around #'my/ignore-read-only)

(defun my/subed-forward-word (&optional arg)
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

(defun my/subed-backward-word (&optional arg)
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

(defhydra my/subed ()
  "Make it easier to split and merge"
  ("e" subed-jump-to-subtitle-end "End")
  ("s" subed-jump-to-subtitle-text "Start")
  ("f" my/subed-forward-word "Forward word")
  ("b" my/subed-backward-word "Backward word")
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
  :if my/laptop-p
  :load-path "~/vendor/subed/subed"
  :mode ("\\.\\(vtt\\|srt\\)\\'" . subed-mode)
  :config
  (setq subed-subtitle-spacing 0)
  (key-chord-define subed-mode-map "hu" 'my/subed/body)
  (key-chord-define subed-mode-map "ht" 'my/subed/body)
  :bind
  (:map subed-mode-map
        ("M-j" . subed-mpv-jump-to-current-subtitle)
        ("M-[" . subed-mpv-seek))
  :hook 
  ((subed-mode . subed-disable-sync-point-to-player)
   (subed-mode . subed-disable-sync-player-to-point)
   (subed-mode . subed-disable-loop-over-current-subtitle)
   (subed-mode . save-place-local-mode)
   (subed-mode . turn-on-auto-fill)
   (subed-mode . (lambda () (setq-local fill-column 40)))))

(defun my/subed-fix-timestamps ()
  "Change all ending timestamps to the start of the next subtitle."
  (goto-char (point-max))
  (let ((timestamp (subed-subtitle-msecs-start)))
    (while (subed-backward-subtitle-time-start)
      (subed-set-subtitle-time-stop timestamp)
      (setq timestamp (subed-subtitle-msecs-start)))))

(defun my/caption-download-srv2 (id)
  (interactive "MID: ")
  (when (string-match "v=\\([^&]+\\)" id) (setq id (match-string 1 id)))
  (let ((default-directory "/tmp"))
    (call-process "youtube-dl" nil nil nil "--write-auto-sub" "--write-sub" "--no-warnings" "--sub-lang" "en" "--skip-download" "--sub-format" "srv2"
                  (concat "https://youtu.be/" id))
    (my/caption-load-word-data (my/latest-file "/tmp" "\\.srv2\\'"))))

(defvar-local my/caption-cache nil "Word-level timing in the form ((start . ms) (end . ms) (text . ms))")
(defun my/caption-json-time-to-ms (json)
  (+ (* 1000 (string-to-number (alist-get 'seconds json)))
     (/ (alist-get 'nanos json) 1000000)))

(defun my/caption-extract-words-from-json3 ()
  (let* ((data (progn (goto-char (point-min)) (json-read)))
         (json3-p (alist-get 'events data))
         (reversed (reverse
                    (or (alist-get 'events data)
                        (cl-loop for seg in (car (alist-get 'results data))
                                 nconc (alist-get 'words (car (alist-get 'alternatives seg)))))))
         (last-event (seq-first reversed))
         (last-ms (if json3-p
                      (+ (alist-get 'tStartMs last-event)
                         (alist-get 'dDurationMs last-event)))))
    (reverse
     (cl-loop for e across reversed append
              (if json3-p
                  (mapcar
                   (lambda (seg)
                     (let ((rec
                            `((start ,(+ (alist-get 'tStartMs e)
                                         (or (alist-get 'tOffsetMs seg) 0)))
                              (end ,(min last-ms
                                         (+ (alist-get 'tStartMs e)
                                            (or (alist-get 'dDurationMs e) 0))))
                              (text ,(alist-get 'utf8 seg)))))
                       (setq last-ms (alist-get 'start rec))
                       rec))
                   (reverse (alist-get 'segs e)))
                `((start ,(my/caption-json-time-to-ms (alist-get 'startTime seg)))
                  (end ,(my/caption-json-time-to-ms (alist-get 'endTime seg)))
                  (text ,(alist-get 'word seg))))))))

(defun my/caption-extract-words-from-srv2 ()
  (let* ((data (xml-parse-region))
         (text-elements (reverse (dom-by-tag data 'text)))
         (last-start (+ (string-to-number
                         (alist-get 't (xml-node-attributes (car text-elements))))
                        (string-to-number (alist-get 'd (xml-node-attributes (car text-elements)))))))
    (reverse
     (mapcar #'(lambda (element)
                 (let ((rec (list (cons 'start (string-to-number (alist-get 't (xml-node-attributes element))))
                                  (cons 'end last-start)
                                  (cons 'text (car (xml-node-children element))))))
                   (setq last-start (alist-get 'start rec))
                   rec))
             text-elements))))

(defun my/caption-fix-common-errors (data)
  (mapc (lambda (o)
          (mapc (lambda (e)
                  (when (string-match (concat "\\<" (car e) "\\>") (alist-get 'text o))
                    (map-put! o 'text (replace-match (cadr e) t t (alist-get 'text o)))))
                my/subed-common-edits))
        data))

(defun my/caption-load-word-data (file)
  "Load word-level timing from FILE."
  (interactive "fFile: ")
  (let (data)
    (with-current-buffer (find-file-noselect file)
      (cond
       ((string-match "\\.json" file)
        (setq data (my/caption-extract-words-from-json3)))
       ((string-match "\\.srv2\\'" file)
        (setq data (my/caption-extract-words-from-srv2)))
       (t (error "Unknown format."))))
    (setq-local my/caption-cache
                (mapcar (lambda (entry)
                          (setf (alist-get 'text entry)
                                (replace-regexp-in-string "&#39;" "'" (alist-get 'text entry)))
                          entry)
                        (my/caption-fix-common-errors data)))))

(defun my/caption-look-up-word ()
  (save-excursion
    (let* ((end (subed-subtitle-msecs-stop))
           (start (subed-subtitle-msecs-start))
           (remaining-words (split-string (buffer-substring (point) (or (subed-jump-to-subtitle-end) (point)))))
           (words (if remaining-words
                      (reverse (seq-filter (lambda (o)
                                             (and (<= (alist-get 'end o) end)
                                                  (>= (alist-get 'start o) start)
                                                  (not (string-match "^\n*$" (alist-get 'text o)))))
                                           my/caption-cache))))
           (offset 0)
           (done (null remaining-words))
           candidate)
      (while (not done)
        (setq candidate (elt words (+ (1- (length remaining-words)) offset)))
        (cond
         ((and candidate (string-match (concat "\\<" (car remaining-words) "\\>") (alist-get 'text candidate)))
          (setq done t))
         ((> offset (length words)) (setq done t))
         ((> offset 0) (setq offset (- offset)))
         (t (setq offset (1+ (- offset))))))
      candidate)))

(defun my/caption-unwrap ()
  (interactive)
  (subed-jump-to-subtitle-text)
  (let ((limit (save-excursion (or (subed-jump-to-subtitle-end) (point)))))
         (while (re-search-forward "\n" limit t)
           (replace-match " "))))
(defun my/caption-split ()
  "Split the current subtitle based on word-level timing if available."
  (interactive)
  (save-excursion
    (let ((data (my/caption-look-up-word)))
      (prin1 data)
      (subed-split-subtitle (and data (- (alist-get 'start data) (subed-subtitle-msecs-start)))))))
(defun my/caption-split-and-merge-with-next ()
  (interactive)
  (my/caption-split)
  (my/caption-unwrap)
  (subed-forward-subtitle-id)
  (subed-merge-with-next)
  (my/caption-unwrap))
(defun my/caption-split-and-merge-with-previous ()
  (interactive)
  (my/caption-split)
  (subed-merge-with-previous)
  (my/caption-unwrap))
(use-package subed
  :if my/laptop-p
  :load-path "~/vendor/subed/subed"
  :bind
  (:map subed-mode-map
        ("M-'" . my/caption-split)
        ("M-," . my/caption-split-and-merge-with-previous)
        ("M-q" . my/caption-unwrap)
        ("M-." . my/caption-split-and-merge-with-next)))

(defvar my/caption-breaks
  '("the" "this" "we" "we're" "I" "finally" "but" "and" "when")
  "List of words to try to break at.")
(defun my/caption-make-groups (list &optional threshold)
  (let (result
        current-item
        done
        (current-length 0)
        (limit (or threshold 70))
        (lower-limit 30)
        (break-regexp (concat "\\<" (regexp-opt my/caption-breaks) "\\>")))
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

(defun my/caption-format-as-subtitle (list &optional word-timing)
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

(defun my/caption-to-vtt (&optional data)
  (interactive)
  (with-temp-file "captions.vtt"
    (insert "WEBVTT\n\n"
            (mapconcat
             (lambda (entry) (my/caption-format-as-subtitle entry))
             (my/caption-make-groups
              (or data (my/caption-fix-common-errors my/caption-cache)))
             ""))))

(defun my/caption-show (url)
  (interactive (list
                (let ((link (and (derived-mode-p 'org-mode)
                                 (org-element-context))))
                  (if (and link 
                           (eq (org-element-type link) 'link))
                      (read-string (format "URL (%s): " (org-element-property :raw-link link)) nil nil
                                   (org-element-property :raw-link link))
                    (read-string "URL: ")))))
  (when (and (listp url) (org-element-property :raw-link url)) (setq url (org-element-property :raw-link url)))
  (with-current-buffer (get-buffer-create "*Captions*")
    (erase-buffer)
    (org-mode)
    (my/org-insert-youtube-video-with-transcript url)
    (goto-char (point-min))
    (display-buffer (current-buffer))))

(defvar my/subed-common-edits '(("i" "I")
                                ("i've" "I've")
                                ("i'm" "I'm")
                                ("gonna" "going to")
                                ("wanna" "want to")
                                ("transit" "transient")
                                ("uh" "")
                                ("um" "")
                                ("maggot" "Magit")
                                ("e-max" "Emacs")
                                ("emex" "Emacs")
                                ("emax" "Emacs")
                                ("emacs news" "Emacs News")
                                ("iv" "ivy")
                                ("ui" "UI")
                                ("tico" "TECO")
                                ("orgrim" "org-roam")
                                ("imax" "Emacs")
                                ("non-nail" "non-nil")
                                ("comets" "commits")
                                ("sql" "SQL")
                                ("imaxconf" "EmacsConf")
                                ("svg" "SVG")
                                ("maggit" "magit")
                                ("axwm" "EXWM")
                                ("bmx" "Emacs"))
  "List of words and replacements.")

(defun my/subed-find-next-fix-point ()
  (when (re-search-forward
         (format "\\<%s\\>"
                 (regexp-opt (mapcar 'car my/subed-common-edits)))
         nil t)
    (goto-char (match-beginning 0))))

(defun my/subed-fix-common-errors ()
  (interactive)
  (let (done)
    (while (and
            (not done)
            (my/subed-find-next-fix-point))
      (let* ((entry (cdr (assoc (match-string 0) my/subed-common-edits)))
             (c (if (elt entry 1)
                    (and entry (read-char (format "%s (yn.): " (car entry))))
                  ?y)))
        (cond
         ((null entry) (goto-char (match-end 0)))
         ((= c ?y) (replace-match (car entry) t t))
         ((= c ?n) (goto-char (match-end 0)))
         ((= c ?j) (subed-mpv-jump-to-current-subtitle))
         ((= c ?.) (setq done t)))
      ))))

(require 'dash)

(defun my/msecs-to-timestamp (msecs)
  "Convert MSECS to string in the format HH:MM:SS.MS."
  (concat (format-seconds "%02h:%02m:%02s" (/ msecs 1000))
          "." (format "%03d" (mod msecs 1000))))

(defun my/org-insert-youtube-video-with-transcript (url)
  (interactive "MURL: ")
  (let* ((id (if (string-match "\\(?:v=\\|youtu\\.be/\\)\\([^&]+\\)" url) (match-string 1 url) url))
         (temp-file (make-temp-name "org-youtube-"))
         (temp-file-name (concat temp-file ".en.srv1"))
         data)
    (when (and (call-process "youtube-dl" nil nil nil
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
                            (my/msecs-to-timestamp (* 1000 (string-to-number (dom-attr o 'start))))
                            (->> (dom-text o)
                                 (replace-regexp-in-string "[ \n]+" " ")
                                 (replace-regexp-in-string "&#39;" "'")
                                 (replace-regexp-in-string "&quot;" "\""))))
                  (dom-by-tag (xml-parse-file temp-file-name) 'text)
                  ""))
      (delete-file temp-file-name))))

(use-package plover-websocket
  :load-path "~/code/plover-websocket-el"
  :if my/laptop-p
  :config (setq plover-websocket-plover-command "cd ~/vendor/plover; tox -e launch")
  :hydra
  (my/plover (:exit t)
             ("<f1>" plover-websocket-connect "Open websocket")
             ("<f2>" plover-websocket-add-translation "Add translation")
             ("<f3>" plover-websocket-lookup "Lookup")
             ("<f4>" plover-websocket-configure "Configure")
             ("<f5>" plover-websocket-focus "Focus")
             ("<f6>" plover-websocket-toggle-plover "Toggle Plover")
             ("<f7>" plover-websocket-quit "Quit")
             ("<f8>" my/plover-drilling-time "Drill"))
  :bind
  ("<f6>" . #'my/plover/body))

(defvar my/plover-drills
   (mapcar (lambda (desc)
             (cons desc (concat "https://joshuagrams.github.io/steno-jig/learn-keyboard.html?drill=" (url-encode-url desc))))
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
             "Columns: D, B, L, -N")))

(defvar my/plover-drill-history nil "Previous drills")
(defvar my/plover-drill-file "~/code/plover-notes/README.org")

(defun my/plover-stenojig-custom-drill (words)
  (interactive "MWords: ")
  (plover-websocket-resume-plover)
  (unwind-protect
    (progn
    (browse-url-chrome (concat "file:///home/sacha/vendor/steno-jig/from-url.html?go=true&type=randomly&timeLimit=2&name=test&hints=true&drillItems=" (url-encode-url words)))
    (read-string "Ignore this: "))
  (plover-websocket-suspend-plover)))

(defun my/plover-drill (drill)
  "Run a single Plover keyboard drill and capture stats in an Org table."
  (interactive (list (consult--read my/plover-drills :prompt "Drill: " :sort nil
                                    :history my/plover-drill-history
                                    :default (car my/plover-drill-history))))
  (plover-websocket-resume-plover)
  (plover-websocket-send :translation "{PLOVER:TOGGLE_DICT:-main.json}")
  (switch-to-buffer (find-file my/plover-drill-file))
  (goto-char (point-min))
  (re-search-forward "#\\+NAME: drill\n")
  (insert (format "| %s | %s |  |\n"
                  (org-link-make-string (assoc-default drill my/plover-drills) drill)
                  (format-time-string "[%Y-%m-%d %a %H:%M]")))
  (backward-char 3)
  (browse-url (assoc-default drill my/plover-drills))
  (read-string "Ignore this:")
  (plover-websocket-send :translation "{PLOVER:TOGGLE_DICT:+main.json}")
  (plover-websocket-suspend-plover)
  (insert (read-string (format "Time (%s): " (string-join (reverse (my/plover-recent-stats drill)) ", "))))
  (end-of-line)
  (forward-char 1))

(defun my/plover-recent-stats (drill-name)
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

 (defun my/plover-drilling-time ()
   "Keep drilling Plover.
Restore main dictionary and turn off Plover when done."
   (interactive)
   (quantified-track "Steno")
   (call-process "wmctrl" nil 0 nil "-i" "-a" (number-to-string (my/wmctl-get-id "emacs")))
   (unwind-protect (while t (my/plover-drill (consult--read my/plover-drills :prompt "Drill: " :sort nil
                                                            :history 'my/plover-drill-history
                                                            :default (car my/plover-drill-history))))
     (plover-websocket-send :translation "{PLOVER:TOGGLE_DICT:+main.json}")
     (plover-websocket-suspend-plover)))
