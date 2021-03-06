;; This sets up the load path so that we can override it
(package-initialize)
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp")
(add-to-list 'load-path "~/vendor/org-mode/lisp")
(add-to-list 'load-path "~/vendor/org-mode/contrib/lisp")
(require 'org)
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
(unless (assoc-default "org" package-archives)
  (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t))

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

(use-package selectrum :init (selectrum-mode +1)) 
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
              ("C-i" . marginalia-cycle-annotators)))
(use-package embark 
  :after selectrum 
  :config
  (setq embark-prompter 'embark-keymap-prompter) 
  (add-to-list 'embark-target-finders 'my/embark-org-element) 
  (add-to-list 'embark-allow-edit-commands #'my/stream-message)
  (add-to-list 'embark-allow-edit-commands #'my/journal-post)
  (embark-define-keymap embark-sketch-actions
    ("o" (lambda (f) (interactive) (insert (org-link-make-string "sketch:" (file-name-nondirectory f)))))
    ("v" my/geeqie-view))
  (embark-define-keymap embark-journal-actions
    ("e" my/journal-edit))
  :bind (("C-c e" . embark-act) 
         ("C-;" . embark-act)
         :map embark-general-map
         (("j" . my/journal-post)
          ("m" . my/stream-message))
         :map embark-variable-map ("l" . edit-list)))

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
                                                   (directory-files dir t "\\.\\(jpe?g\\|png\\)$"))
                                                 my/sketch-directories))
                          (lambda (a b)
                            (string< (concat (or (my/date-from-filename b) "0") (file-name-nondirectory b))
                                     (concat (or (my/date-from-filename a) "0") (file-name-nondirectory a)) )))))

(defun my/complete-sketch-filename ()
  (consult--read (or my/sketches (my/update-sketch-cache))
   :sort nil
   :prompt "Sketch: " :category 'sketch))

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

(fset 'org-refile-get-location 'my/org-refile-get-location)

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
(defun my/capture-screenshot (time &optional note)
  "Capture screenshot and save it to a file labeled with TIME and NOTE.
       Return the filename."
  (interactive (list (current-time) (read-string "Note: ")))
  (let* ((filename (expand-file-name
                    (concat "Screenshot_"
                            (format-time-string "%Y%0m%d_%H%M%S" time)
                            (if note (concat " " note) "")
                            ".png")
                    "~/Pictures"))
         (cmd (concat "spectacle -b -o "
                      (shell-quote-argument filename))))
    (shell-command cmd)
    filename))
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
  (interactive (list (current-time) (read-string "Note: ")))
  (kill-new (my/capture-screenshot time note))
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

(use-package guide-key
  :defer t
  :diminish guide-key-mode
  :config
  (progn
    (setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "C-c"))
    (guide-key-mode 1)))  ; Enable guide-key-mode

(prefer-coding-system 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

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

(let* ((file "~/code/emacs-calendar/README.org")
       (record `((filename . ,file) (position . 1) (rear-context-string) (front-context-string))))
  (bookmark-store file record nil))
     
(my/defshortcut ?C "~/code/emacs-calendar/README.org")
(my/defshortcut ?e "~/code/.emacs.d/Sacha.org")
(my/defshortcut ?E "~/sync/emacs-news/index.org")
(my/defshortcut ?f "~/code/font/README.org")
(my/defshortcut ?i "~/orgzly/computer-inbox.org")
(my/defshortcut ?I "~/orgzly/Inbox.org")
(my/defshortcut ?o "~/orgzly/organizer.org")
(my/defshortcut ?s "~/code/stream/notes.org")
(my/defshortcut ?b "~/personal/business.org")
(my/defshortcut ?p "/ssh:web:/mnt/prev/home/sacha/planet/en.ini")
(my/defshortcut ?B "~/Dropbox/books")
(my/defshortcut ?n "~/sync/notes")
(my/defshortcut ?N "~/sync/notes/QuickNote.md")
(my/defshortcut ?w "~/Dropbox/public/sharing/index.org")
(my/defshortcut ?W "~/Dropbox/public/sharing/blog.org")
(my/defshortcut ?r "~/personal/reviews.org")
(my/defshortcut ?j "~/personal/journal.org")
(my/defshortcut ?J "~/cloud/a/Journal.csv")
(my/defshortcut ?g "~/code/sachac.github.io/evil-plans/index.org")
(my/defshortcut ?c "~/code/dev/elisp-course.org")
(my/defshortcut ?C "~/personal/calendar.org")
(my/defshortcut ?l "~/orgzly/learning.org")
(my/defshortcut ?L "~/orgzly/stories.org")
(my/defshortcut ?q "~/sync/notes/QuickNote.md")
(my/defshortcut ?Q "~/personal/questions.org")

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

(use-package hydra
  :config
  (defhydra my/goto-random-char-hydra ()
    ("r" my/goto-random-char))

  (defhydra my/window-movement ()
    ("<left>" windmove-left)
    ("<right>" windmove-right)
    ("<down>" windmove-down)
    ("<up>" windmove-up)
    ("y" other-window "other")
    ("h" switch-window "switch-window")
    ("f" find-file "file")
    ("F" find-file-other-window "other file")
    ("v" (progn (split-window-right) (windmove-right)))
    ("o" delete-other-windows :color blue)
    ("a" ace-window)
    ("s" ace-swap-window)
    ("d" delete-window "delete")
    ("D" ace-delete-window "ace delete")
    ("i" ace-maximize-window "maximize")
    ("b" helm-buffers-list)
    ("q" nil))
  (defhydra join-lines ()
    ("<up>" join-line)
    ("<down>" (join-line 1))
    ("t" join-line)
    ("n" (join-line 1)))
  (defhydra my/org (:color blue)
    "Convenient Org stuff."
    ("p" my/org-show-active-projects "Active projects")
    ("a" (org-agenda nil "a") "Agenda"))
  (defhydra my/engine-mode-hydra (:color blue)
    "Engine mode"
    ("b" engine/search-my-blog "blog")
    ("f" engine/search-my-photos "flickr")
    ("m" engine/search-mail "mail")
    ("g" engine/search-google "google")
    ("e" engine/search-emacswiki "emacswiki"))
  ;; From https://github.com/abo-abo/hydra
  (defhydra hydra-buffer-menu (:color pink
                                      :hint nil)
    "
      ^Mark^             ^Unmark^           ^Actions^          ^Search
      ^^^^^^^^-----------------------------------------------------------------
      _m_: mark          _u_: unmark        _x_: execute       _R_: re-isearch
      _s_: save          _U_: unmark up     _b_: bury          _I_: isearch
      _d_: delete        ^ ^                _g_: refresh       _O_: multi-occur
      _D_: delete up     ^ ^                _T_: files only: % -28`Buffer-menu-files-only
      _~_: modified
      "
    ("m" Buffer-menu-mark)
    ("u" Buffer-menu-unmark)
    ("U" Buffer-menu-backup-unmark)
    ("d" Buffer-menu-delete)
    ("D" Buffer-menu-delete-backwards)
    ("s" Buffer-menu-save)
    ("~" Buffer-menu-not-modified)
    ("x" Buffer-menu-execute)
    ("b" Buffer-menu-bury)
    ("g" revert-buffer)
    ("T" Buffer-menu-toggle-files-only)
    ("O" Buffer-menu-multi-occur :color blue)
    ("I" Buffer-menu-isearch-buffers :color blue)
    ("R" Buffer-menu-isearch-buffers-regexp :color blue)
    ("c" nil "cancel")
    ("v" Buffer-menu-select "select" :color blue)
    ("o" Buffer-menu-other-window "other-window" :color blue)
    ("q" quit-window "quit" :color blue))

  (define-key Buffer-menu-mode-map "." 'hydra-buffer-menu/body)

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
  
  (defhydra my/shortcuts (:exit t)
    "Shortcuts"
    ("f" (helm :sources '(helm-source-projectile-files-list
                          helm-source-files-in-current-dir
                          helm-source-projectile-projects
                          helm-source-recentf
                          helm-source-bookmarks
                          helm-source-ls-git
                          helm-source-locate)
               :buffer "*helm-find-files*") "Find")
    ("j" my/helm-journal "Journal")
    ("n" my/capture-timestamped-note)
    ("l" (my/toggle-or-create "*scratch*" (lambda () (switch-to-buffer (startup--get-buffer-create-scratch)))) "Lisp")
    ("d" my/emacs-news-check-duplicates "Dupe")
    ("c" my/org-categorize-emacs-news/body "Categorize")
    ("h" (lambda () (interactive) (my/org-update-link-description "HN")) "Link HN")
    ("i" (lambda () (interactive) (my/org-update-link-description "Irreal")) "Link Irreal")
    ("s" save-buffer "Save")
    ("m" my/share-emacs-news "Mail"))
  (global-set-key (kbd "<f5>") 'my/shortcuts/body))

(use-package pretty-hydra)
(defun my/org-insert-link ()
  (interactive)
  (when (org-in-regexp org-bracket-link-regexp 1)
    (goto-char (match-end 0))
    (insert "\n"))
  (call-interactively 'org-insert-link))

(defhydra my/key-chord-commands ()
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
  ("L" my/org-insert-link)
  ("+" text-scale-increase)
  ("-" text-scale-decrease))

(use-package key-chord
  :if my/laptop-p
  :init
  (setq key-chord-one-key-delay 0.16)
  (setq key-chord-two-keys-delay 0.002)
  (key-chord-define-global "uu"     'undo)
  (key-chord-define-global "jr"     'my/goto-random-char-hydra/my/goto-random-char)
  (key-chord-define-global "kk"     'kill-whole-line)
  (key-chord-define-global "et" 'my/stream-message)
  (key-chord-define-global "em" 'embark-act)
  (key-chord-define-global ".t" 'my/stream/body)
  (key-chord-define-global "jj"     'avy-goto-word-1)
  (key-chord-define-global "yy"    'my/window-movement/body)
  (key-chord-define-global "jw"     'switch-window)
  (key-chord-define-global "jl"     'avy-goto-line)
  (key-chord-define-global "j."     'join-lines/body)
  (key-chord-define-global "FF"     'find-file)
  (key-chord-define-global "qq"     'my/quantified-hydra/body)
  (key-chord-define-global "hh"     'my/key-chord-commands/body)
  (key-chord-define-global "xx"     'er/expand-region)
  (key-chord-define-global "  "     'my/insert-space-or-expand)
  (key-chord-define-global "vv" 'god-mode-all)
  (key-chord-define-global "JJ"     'my/switch-to-previous-buffer)
  (key-chord-mode 1))

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

(defun my/subed-write-adjusted-subtitles (beg end name)
  (interactive "r\nMName: ")
  (let ((s (buffer-substring-no-properties (save-excursion (goto-char (min beg end)) (subed-jump-to-subtitle-id))
                                           (save-excursion (goto-char (max beg end)) (or (subed-jump-to-subtitle-end) (point)))))
        (original (buffer-file-name))
        (offset (- (save-excursion (goto-char (min beg end)) (subed-subtitle-msecs-start)))))
    (with-current-buffer (find-file-noselect (my/extend-file-name original name))
      (erase-buffer)
      (insert s)
      (subed-for-each-subtitle (point-min) (point-max) nil
        (subed-adjust-subtitle-time-start offset)
        (subed-adjust-subtitle-time-stop offset))
      (subed-regenerate-ids)
      (save-buffer)
      (buffer-file-name))))

(defun my/subed-make-animated-gif (beg end name)
  (interactive "r\nMName: ")
  (let* ((video-file (subed-guess-video-file))
         (time-format "%.2h:%.2m:%.2,3s")
         (msecs (my/subed-get-region-start-stop beg end))
         (new-file (my/extend-file-name video-file name "gif"))
         cmd)
    (when (> (length name) 0)
      (setq cmd
            (format "ffmpeg -y -i %s -ss %s -t %s -vf subtitles=%s -r 10 -c:a copy -shortest -async 1 %s"
                    (shell-quote-argument video-file)
                    (format-seconds time-format (/ (car msecs) 1000.0))
                    (format-seconds time-format (/ (- (cdr msecs) (car msecs)) 1000.0))
                    (shell-quote-argument (my/subed-write-adjusted-subtitles beg end name))                
                    (shell-quote-argument new-file)))
      (message "%s" cmd)
      (kill-new cmd)
      (shell-command cmd))))

(defun my/subed-cut-video (beg end name)
  (interactive "r\nMName: ")
  (let* ((video-file (subed-guess-video-file))
         (time-format "%.2h:%.2m:%.2,3s")
         (msecs (my/subed-get-region-start-stop beg end))
         (new-file (my/extend-file-name video-file name))
         cmd)
    (when (> (length name) 0)
      (setq cmd
            (format "ffmpeg -y -i %s -ss %s -t %s -i %s -c:a copy -c:v copy -c:s mov_text -shortest -async 1 %s"
                      (shell-quote-argument video-file)
                      (format-seconds time-format (/ (car msecs) 1000.0))
                      (format-seconds time-format (/ (- (cdr msecs) (car msecs)) 1000.0))
                      (shell-quote-argument (my/subed-write-adjusted-subtitles beg end name))                
                      (shell-quote-argument new-file)))
      (message "%s" cmd)
      (kill-new cmd)
      (shell-command cmd))))

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

(defvar my/subed-common-edits nil "List of words and replacements.")
(setq my/subed-common-edits
      '(("i" "I" t)
        ("i've" "I've" t)
        ("i'm" "I'm" t)
        ("gonna" "going to" t)
        ("wanna" "want to" t)
        ("e-max" "Emacs" t)
        ("emacs news" "Emacs News" t)
        ("imax" "Emacs" t)))
(defun my/subed-find-next-fix-point ()
  (when (re-search-forward
         (format "\\<%s\\>"
                 (regexp-opt (mapcar 'car my/subed-common-edits)))
         nil t)
    (goto-char (match-beginning 0))))
(defun my/subed-common-fixes ()
  (interactive)
  (let (done)
    (while (and
            (not done)
            (my/subed-find-next-fix-point))
      (let* ((entry (cdr (assoc (match-string 0) my/subed-common-edits)))
             (c (if (elt entry 1)
                    ?y
                  (and entry (read-char (format "%s (yn.): " (car entry)))))))
        (cond
         ((null entry) (goto-char (match-end 0)))
         ((= c ?y) (replace-match (car entry) t t))
         ((= c ?n) (goto-char (match-end 0)))
         ((= c ?j) (subed-mpv-jump-to-current-subtitle))
         ((= c ?.) (setq done t)))
      ))))

(defvar my/scan-directory "~/sync/scans")
(defvar my/portfolio-directory "~/sync/portfolio")
(defvar my/camera-directory "~/sync/camera")
(defvar my/private-sketches-directory "~/cloud/private-sketches")
(defvar my/sketches-directory "~/sync/sketches")

(defun my/geeqie-next ()
  (interactive)
  (shell-command "geeqie --remote -n"))
(defun my/geeqie-previous ()
  (interactive)
  (shell-command "geeqie --remote -b"))
(defun my/geeqie-filename ()
  (string-trim (shell-command-to-string "geeqie --remote --tell")))
(defun my/geeqie-view (filenames)
  (interactive "f")
  (shell-command
   (concat "geeqie --remote "
           (mapconcat (lambda (f)
                        (concat "file:" (shell-quote-argument f)))
                      (cond
                       ((listp filenames) filenames)
                       ((file-directory-p filenames)
                        (list (car (seq-filter #'file-regular-p (directory-files filenames t)))))
                       (t (list filenames)))
                      " ")
           " &")))

(defvar my/rotate-jpeg-using-exiftran nil)

(defun my/rotate-image-clockwise (filename)
  (if (and my/rotate-jpeg-using-exiftran
           (string-match "jpe?g" (file-name-extension filename)))
      (call-process "exiftran" nil nil nil "-i" "-9" filename)
    (call-process "mogrify" nil nil nil "-rotate" "90" filename)))

(defun my/rotate-image-counterclockwise (filename)
  (if (and my/rotate-jpeg-using-exiftran
           (string-match "jpe?g" (file-name-extension filename)))
      (call-process "exiftran" nil nil nil "-i" "-2" filename)
    (call-process "mogrify" nil nil nil "-rotate" "270" filename)))

(defun my/geeqie-rotate-clockwise ()
  (interactive)
  (my/rotate-image-clockwise (my/geeqie-filename))
  (my/geeqie-view (my/geeqie-filename)))

(defun my/geeqie-rotate-counterclockwise ()
  (interactive)
  (my/rotate-image-counterclockwise (my/geeqie-filename))
  (my/geeqie-view (my/geeqie-filename)))

(defun my/rename-file-based-on-modification-time (filename)
  "Rename files to their modification time."
  (rename-file filename
               (expand-file-name
                (concat
                 (format-time-string "%Y-%m-%d_%H%M%S"
                                     (file-attribute-modification-time (file-attributes filename)))
                 "."
                 (file-name-extension filename))
                (file-name-directory filename))))

(defun my/geeqie-change-date (filename new-time)
  (interactive (list (my/geeqie-filename)
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
    (my/geeqie-view new-file)))

(defun my/geeqie-rename-current (old-filename new-filename)
  (interactive
   (list (my/geeqie-filename)
         (read-string "Filename: " (concat (file-name-base (my/geeqie-filename)) " "))))
  (rename-file old-filename
               (expand-file-name
                (concat new-filename "." (file-name-extension old-filename))
                (file-name-directory old-filename))))

(defun my/geeqie-crop-to-rectangle ()
  (interactive)
  (call-process
   "mogrify" nil nil nil "-crop"
   (string-trim (shell-command-to-string "geeqie --remote --get-rectangle"))
   (my/geeqie-filename))
  (my/geeqie-view (my/geeqie-filename)))

(defun my/geeqie-scans ()
  "Rename files and open the first one."
  (interactive)
  (mapc 'my/rename-file-based-on-modification-time (directory-files my/scan-directory t "^scan"))
  (call-process "geeqie" nil nil nil "--remote" (concat "file:" (shell-quote-argument (seq-find 'file-regular-p (directory-files "~/sync/scans" t "^[0-9].*\\(gif\\|png\\|jpg\\)"))))))

(defun my/geeqie-delete-and-next ()
  (interactive)
  (let ((file (my/geeqie-filename)))
    (my/geeqie-next)
    (delete-file file t)))

(defhydra my/geeqie ()
  "Manage images with geeqie."
  ("op" (my/geeqie-view my/portfolio-directory) "Open portfolio")
  ("oc" (my/geeqie-view my/camera-directory) "Open camera")
  ("os" my/geeqie-scans "Open scans")
  ("[" my/geeqie-rotate-counterclockwise "CCW")
  ("]" my/geeqie-rotate-clockwise "CW")
  ("r" my/geeqie-rename-current "Rename")
  ("c" my/geeqie-crop-to-rectangle "Crop")
  ("k" (start-process "krita" nil "krita" (my/geeqie-filename)) "krita")
  ("g" (start-process "gimp" nil "gimp" (my/geeqie-filename)) "gimp")
  ("n" my/geeqie-next "Next")
  ("p" my/geeqie-previous "Previous")
  ("d" my/geeqie-change-date "Change date")
  ("x" my/geeqie-delete-and-next "Delete")
  ("m" my/move-portfolio-files "Move portfolio files")
  ("s" (rename-file (my/geeqie-filename)
                    (expand-file-name (file-name-nondirectory (my/geeqie-filename)) my/sketches-directory))
   "Save to sketch directory")
  ("O" (shell-command (format "mogrify -auto-orient %s" (shell-quote-argument (my/geeqie-filename)))) "Rotate based on EXIF")
  ("<up>" (forward-line -1) :hint nil)
  ("<down>" forward-line :hint nil)
  ("im" (insert (format "{{<photo nas=\"1\" src=\"%s\">}}" (my/geeqie-filename))))
  ("if" (insert (my/geeqie-filename) "\n")
   "Insert filename")
  ("v" (my/geeqie-view (string-trim (thing-at-point 'line))) "View")
  ("il" (insert "- " (my/geeqie-filename) "\n") "Insert filename as list item"))

(defun my/move-portfolio-files ()
  (interactive)
  (mapc (lambda (f)
          (let ((new-dir
                 (cond
                  ((string-match "#private" f) my/private-sketches-directory)
                  ((string-match "#me\\>" f) my/sketches-directory)
                  (t my/portfolio-directory))))
            (when new-dir (rename-file f (expand-file-name (file-name-nondirectory f) new-dir)))))
        (seq-filter
         'file-regular-p
         (directory-files my/scan-directory t "^[0-9]+.*#")))
  (shell-command-to-string "make-sketch-thumbnails"))

(setq org-export-with-sub-superscripts nil)

(use-package org
  :load-path ("~/vendor/org-mode/lisp" "~/vendor/org-mode/contrib/lisp")
  :config
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
(bind-key "<f9> <f9>" 'org-agenda-list)
(bind-key "<f9> <f8>" (lambda () (interactive) (org-capture nil "r")))

(with-eval-after-load 'org
  (bind-key "C-M-w" 'append-next-kill org-mode-map)
  (bind-key "C-TAB" 'org-cycle org-mode-map)
  (bind-key "C-c v" 'org-show-todo-tree org-mode-map)
  (bind-key "C-c C-r" 'org-refile org-mode-map)
  (bind-key "C-c R" 'org-reveal org-mode-map)
  (bind-key "C-c o" 'my/org-follow-entry-link org-mode-map)
  (bind-key "C-c d" 'my/org-move-line-to-destination org-mode-map)
  (bind-key "C-c f" 'my/org-file-blog-index-entries org-mode-map)
  (bind-key "C-c t s"  'my/split-sentence-and-capitalize org-mode-map)
  (bind-key "C-c t -"  'my/split-sentence-delete-word-and-capitalize org-mode-map)
  (bind-key "C-c t d"  'my/delete-word-and-capitalize org-mode-map)

  (bind-key "C-c C-p C-p" 'my/org-publish-maybe org-mode-map)
  (bind-key "C-c C-r" 'my/org-refile-and-jump org-mode-map))

(with-eval-after-load 'org-agenda
  (bind-key "i" 'org-agenda-clock-in org-agenda-mode-map))

(setq org-use-effective-time t)

(defun my/org-use-speed-commands-for-headings-and-lists ()
  "Activate speed commands on list items too."
  (or (and (looking-at org-outline-regexp) (looking-back "^\**" nil))
      (save-excursion (and (looking-at (org-item-re)) (looking-back "^[ \t]*" nil)))))
(setq org-use-speed-commands 'my/org-use-speed-commands-for-headings-and-lists)

(defun my/org-mark-done-and-add-to-journal (&optional note)
  (interactive (list (read-string "Note: " (org-get-heading t t t t))))
  (my/org-with-current-task
   (org-todo "DONE")
   (org-entry-put (point) "ZIDSTRING" (plist-get (my/journal-post (or note (org-get-heading t t t t))) :ZIDString))))

(use-package org
  :config
  (progn
    (add-to-list 'org-speed-commands-user '("x" org-todo "DONE"))
    (add-to-list 'org-speed-commands-user '("X" call-interactively 'my/org-mark-done-and-add-to-journal))
    (add-to-list 'org-speed-commands-user '("y" org-todo-yesterday "DONE"))
    (add-to-list 'org-speed-commands-user '("!" my/org-clock-in-and-track))
    (add-to-list 'org-speed-commands-user '("s" call-interactively 'org-schedule))
    (add-to-list 'org-speed-commands-user '("d" my/org-move-line-to-destination))
    (add-to-list 'org-speed-commands-user '("i" call-interactively 'org-clock-in))
    (add-to-list 'org-speed-commands-user '("P" call-interactively 'org2blog/wp-post-subtree))
    (add-to-list 'org-speed-commands-user '("o" call-interactively 'org-clock-out))
    (add-to-list 'org-speed-commands-user '("$" call-interactively 'org-archive-subtree))
    (bind-key "!" 'my/org-clock-in-and-track org-agenda-mode-map)))

(setq org-goto-interface 'outline
      org-goto-max-level 10)
(require 'imenu)
(setq org-startup-folded nil)
(bind-key "C-c j" 'org-clock-goto) ;; jump to current task from anywhere
(bind-key "C-c C-w" 'org-refile)
(setq org-cycle-include-plain-lists 'integrate)
(setq org-catch-invisible-edits 'show-and-error)

(defun my/org-follow-entry-link ()
  "Follow the defined link for this entry."
  (interactive)
  (if (org-entry-get (point) "LINK")
      (org-open-link-from-string (org-entry-get (point) "LINK"))
    (org-open-at-point)))

(defun my/org-link-projects (location)
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

(defun my/org-back-to-heading ()
  (interactive)
  (org-back-to-heading))

(use-package org
  :bind (:map org-mode-map 
              ("C-c b" . my/org-back-to-heading)
              ("C-c p" . org-display-outline-path)))

(defun my/org-show-row-and-column (point)
  (interactive "d")
  (save-excursion
    (goto-char point)
    (let ((row (s-trim (org-table-get nil 1)))
          (col (s-trim (org-table-get 1 nil)))
          (message-log-max nil))
      (message "%s - %s" row col))))

(setq org-directory "~/personal")
(setq org-default-notes-file "~/orgzly/organizer.org")

(defun my/yank-more ()
  (interactive)
  (insert "[[")
  (yank)
  (insert "][more]]"))
(global-set-key (kbd "<f6>") 'my/yank-more)

(defun my/org-insert-heading-for-next-day ()
  "Insert a same-level heading for the following day."
  (interactive)
  (let ((new-date
         (seconds-to-time
          (+ 86400.0
             (float-time
              (org-read-date nil 'to-time (elt (org-heading-components) 4)))))))
    (org-insert-heading-after-current)
    (insert (format-time-string "%Y-%m-%d\n\n" new-date))))

(defun my/org-contacts-template-email (&optional return-value)
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

(defvar my/org-basic-task-template "* TODO %^{Task}
         :PROPERTIES:
         :Effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}
         :END:
         Captured %<%Y-%m-%d %H:%M>
         %?

         %i
         " "Basic task data")
(defvar my/org-inbox-file "~/orgzly/Inbox.org")
(defvar my/ledger-file "~/cloud/ledger/current.ledger")
(setq org-capture-templates
      `(("t" "Quick task" entry
         (file ,my/org-inbox-file)
         "* TODO %^{Task}\n"
         :immediate-finish t)
        ("p" "Podcast log - timestamped" item
         (file+olp+datetree "~/orgzly/timestamped.org")
         "%<%H:%M:%S,%3N> %^{Note}"
         :immediate-finish t)
        ("T" "Task" entry
         (file ,my/org-inbox-file)
         "* TODO %^{Task}\n")
        ("." "Today" entry
         (file ,my/org-inbox-file)
         "* TODO %^{Task}\nSCHEDULED: %t\n"
         :immediate-finish t)
        ("v" "Video" entry
         (file ,my/org-inbox-file)
         "* TODO %^{Task}  :video:\nSCHEDULED: %t\n"
         :immediate-finish t)
        ("e" "Errand" entry
         (file ,my/org-inbox-file)
         "* TODO %^{Task}  :errands:\n"
         :immediate-finish t)
        ("n" "Note" entry
         (file ,my/org-inbox-file)
         "* %^{Note}\n"
         :immediate-finish t)
        ("r" "Note" entry
         (file ,my/org-inbox-file)
         "* %?\n%U - %a")
        ("N" "Note" entry
         (file ,my/org-inbox-file)
         "* %^{Note}\n")
        ("i" "Interrupting task" entry
         (file ,my/org-inbox-file)
         "* STARTED %^{Task}"
         :clock-in :clock-resume)
        ("b" "Business task" entry
         (file+headline "~/personal/business.org" "Tasks")
         ,my/org-basic-task-template)
        ("j" "Journal entry" plain
         (file+olp+datetree "~/orgzly/journal.org")
         "%K - %a\n%i\n%?\n"
         :unnarrowed t)
        ("c" "Protocol Link" entry (file+headline ,org-default-notes-file "Inbox")
         "* [[%:link][%:description]] \n\n#+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n%?\n\nCaptured: %U")
        ("db" "Done - Business" entry
         (file+headline "~/personal/business.org" "Tasks")
         "* DONE %^{Task}\nSCHEDULED: %^t\n%?")
        ("dp" "Done - People" entry
         (file+headline "~/personal/people.org" "Tasks")
         "* DONE %^{Task}\nSCHEDULED: %^t\n%?")
        ("dt" "Done - Task" entry
         (file+headline "~/orgzly/organizer.org" "Inbox")
         "* DONE %^{Task}\nSCHEDULED: %^t\n%?")
        ("q" "Quick note" item
         (file+headline "~/orgzly/organizer.org" "Quick notes"))
        ("l" "Ledger")
        ("lc" "Cash expense" plain
         (file ,my/ledger-file)
         "%(ledger-read-date \"Date: \") * %^{Payee}
             Expenses:Cash
             Expenses:%^{Account}  %^{Amount}
           ")
        ("lb" "BDO CAD" plain
         (file ,my/ledger-file)
         "%(ledger-read-date \"Date: \") * %^{Payee}
             Expenses:Play    $ %^{Amount}
             Assets:BDO
           ")
        ("lp" "BDO PHP" plain
         (file ,my/ledger-file)
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
        ("C" "Contact" entry (file "~/personal/contacts.org")
         "* %(org-contacts-template-name)
           :PROPERTIES:
           :EMAIL: %(my/org-contacts-template-email)
           :END:")))
(bind-key "C-M-r" 'org-capture)



;;(bind-key (kbd "<f5>") 'org-capture)

(defun my/org-refile-and-jump ()
  (interactive)
  (if (derived-mode-p 'org-capture-mode)
      (org-capture-refile)
    (call-interactively 'org-refile))
  (org-refile-goto-last-stored))
(eval-after-load 'org-capture
  '(bind-key "C-c C-r" 'my/org-refile-and-jump org-capture-mode-map))

(setq org-reverse-note-order nil)
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)
(setq org-refile-use-cache nil)
(setq org-refile-targets '((("~/orgzly/organizer.org"
                             "~/code/stream/notes.org"
                             "~/code/.emacs.d/Sacha.org"
                             "~/orgzly/routines.org") . (:maxlevel . 5))))
(setq org-blank-before-new-entry nil)

;; Example: (org-refile 4 nil (my/org-refile-get-location-by-substring "Other Emacs"))
(defun my/org-refile-get-location-by-substring (regexp &optional file)
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
(defun my/org-refile-subtree-to (name)
  (org-refile nil nil (my/org-refile-get-location-exact name)))

(defun my/org-refile-get-location-exact (name &optional file)
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
;; Example: (my/org-clock-in-refile "Off my computer")
(defun my/org-clock-in-refile (location &optional file)
  "Clocks into LOCATION.
        LOCATION and FILE can also be regular expressions for `my/org-refile-get-location-by-substring'."
  (interactive (list (my/org-refile-get-location)))
  (save-window-excursion
    (save-excursion
      (if (stringp location) (setq location (my/org-refile-get-location-by-substring location file)))
      (org-refile 4 nil location)
      (org-clock-in))))

(defun my/org-finish-previous-task-and-clock-in-new-one (location &optional file)
  (interactive (list (my/org-refile-get-location)))
  (save-window-excursion
    (org-clock-goto)
    (org-todo 'done))
  (my/org-clock-in-and-track-by-name location file))

(defun my/org-clock-in-and-track-by-name (location &optional file)
  (interactive (list (my/org-refile-get-location)))
  (save-window-excursion
    (save-excursion
      (if (stringp location) (setq location (my/org-refile-get-location-exact location file)))
      (org-refile 4 nil location)
      (my/org-clock-in-and-track))))
(defun my/org-off-my-computer (category)
  (interactive "MCategory: ")
  (eval-when-compile (require 'quantified nil t))
  (my/org-clock-in-refile "Off my computer")
  (quantified-track category))

(defun my/org-jump ()
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively 'org-refile)))

(defun my/org-bounce-to-file (file)
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
(defun my/org-entry-wpm ()
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

(setq org-todo-keywords
      '((sequence
         "STARTED(s)"
         "TODO(t)"  ; next action
         "TOBLOG(b)"  ; next action
         "WAITING(w@/!)"
         "SOMEDAY(.)" "|" "DONE(x!)" "CANCELLED(c)")
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

(use-package org
  :config
  (add-to-list 'org-speed-commands-user '("N" org-narrow-to-subtree))
  (add-to-list 'org-speed-commands-user '("W" widen))
  (add-to-list 'org-speed-commands-user '("T" my/org-agenda-for-subtree))
  (add-to-list 'org-speed-commands-user '("b" my/org-bounce-to-file)))

(defun my/org-agenda-for-subtree ()
  (interactive)
  (when (derived-mode-p 'org-agenda-mode) (org-agenda-switch-to))
  (my/org-with-current-task
   (let ((org-agenda-view-columns-initially t))
     (org-agenda nil "t" 'subtree))))

(add-to-list 'org-speed-commands-user '("S" call-interactively 'org-sort))

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
          'my/org-mode-ask-effort)

(defun my/org-mode-ask-effort ()
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

(defun my/org-clean-up-inbox ()
  "Archive all DONE tasks and sort the remainder by TODO order."
  (interactive)
  (with-current-buffer (find-file my/org-inbox-file)
    (my/org-archive-done-tasks 'file)
    (goto-char (point-min))
    (if (org-at-heading-p) (save-excursion (insert "\n")))
    (org-sort-entries nil ?p)
    (goto-char (point-min))
    (org-sort-entries nil ?o)
    (save-buffer)))

(defun my/org-archive-done-tasks (&optional scope)
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

(defun my/org-html-quote2 (block backend info)
  (when (org-export-derived-backend-p backend 'html)
    (when (string-match "\\`<div class=\"quote2\">" block)
      (setq block (replace-match "<blockquote>" t nil block))
      (string-match "</div>\n\\'" block)
      (setq block (replace-match "</blockquote>\n" t nil block))
      block)))
(eval-after-load 'ox
  '(add-to-list 'org-export-filter-special-block-functions 'my/org-html-quote2))

(defun my/org-link-youtube-time (url beg end)
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

(defun my/clean-up-google-hangout-chat ()
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

(defvar my/kid-org-file nil "Defined in secrets")
(setq org-agenda-files
      (delq nil
            (mapcar (lambda (x) (and x (file-exists-p x) x))
                    `("~/orgzly/organizer.org"
                      "~/orgzly/Inbox.org"
                      "~/orgzly/computer-inbox.org"
                      "~/code/stream/notes.org"
                      "~/personal/sewing.org"
                      "~/orgzly/people.org"
                      "~/Dropbox/wsmef/trip.txt"
                      ,my/kid-org-file
                      "~/personal/business.org"
                      "~/personal/calendar.org"
                      "~/Dropbox/tasker/summary.txt"
                      "~/Dropbox/public/sharing/index.org"
                      "~/dropbox/public/sharing/learning.org"
                      "~/code/emacs-notes/tasks.org"
                      "~/code/sachac.github.io/evil-plans/index.org"
                      "~/orgzly/cooking.org"
                      "~/orgzly/routines.org"))))
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

(defun my/org-agenda-project-agenda ()
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

(defun my/org-agenda-projects-and-tasks (match)
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
                    (setq rtn (org-scan-tags 'my/org-agenda-project-agenda matcher todo-only))
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
(defvar my/org-agenda-contexts
  '((tags-todo "phone")
    (tags-todo "work")
    (tags-todo "drawing")
    (tags-todo "coding")
    (tags-todo "writing")
    (tags-todo "computer")
    (tags-todo "home")
    (tags-todo "errands"))
  "Usual list of contexts.")
(defun my/org-agenda-skip-scheduled ()
  (org-agenda-skip-entry-if 'scheduled 'deadline 'regexp "\n]+>"))

(setq org-agenda-custom-commands
      `(("a" "Agenda"
         ((agenda "" ((org-agenda-span 2)))
          ;; Projects
          (tags "+project-someday-TODO=\"DONE\"-TODO=\"SOMEDAY\"-inactive-evilplans"
                ((org-tags-exclude-from-inheritance '("project"))
                 (org-agenda-prefix-format "  ")
                 (org-agenda-overriding-header "Projects: ")
                 (org-agenda-sorting-strategy '(priority-down tag-up category-keep effort-down))))
          ;; Inbox
          (alltodo ""
                   ((org-agenda-files '("~/orgzly/Inbox.org" "~/orgzly/computer-inbox.org"))
                    (org-agenda-prefix-format "%-6e ")
                    (org-agenda-overriding-header "Inbox: ")))
          ;; Unscheduled
          (tags-todo "TODO=\"TODO\"-project-cooking-routine-errands-shopping-video-evilplans" 
                     ((org-agenda-skip-function 'my/org-agenda-skip-scheduled)
                      (org-agenda-prefix-format "%-6e ")
                      (org-agenda-overriding-header "Unscheduled TODO entries: ")
                      (org-agenda-sorting-strategy '(priority-down effort-up tag-up category-keep))))
          ))
        ("e" "Emacs" (tags "emacs"))
        ("i" "Inbox" alltodo ""
         ((org-agenda-files '("~/orgzly/Inbox.org" "~/orgzly/computer-inbox.org"))))
        ("t" tags-todo "-cooking"
         ((org-agenda-sorting-strategy '(todo-state-up priority-down effort-up))))
        ("T" tags-todo "TODO=\"TODO\"-goal-routine-cooking-SCHEDULED={.+}" nil "~/cloud/agenda/nonroutine.html")
        ("f" tags-todo "focus-TODO=\"DONE\"-TODO=\"CANCELLED\"")
        ("b" todo ""
         ((org-agenda-files '("~/personal/business.org"))))
        ("B" todo ""
         ((org-agenda-files '("~/Dropbox/books"))))
        ("x" "Column view" todo ""  ; Column view
         ((org-agenda-prefix-format "")
          (org-agenda-cmp-user-defined 'my/org-sort-agenda-items-todo)
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
         ((org-agenda-files '("~/personal/business.org"))
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
         ,my/org-agenda-contexts
         ((org-agenda-sorting-strategy '(priority-up effort-down))
          (org-agenda-max-entries 3)))
        ("C" "All by context"
         ,my/org-agenda-contexts
         ((org-agenda-sorting-strategy '(priority-down effort-down))
          (org-agenda-max-entries nil)))
        ("9" "Unscheduled top 3 by context"
         ,my/org-agenda-contexts
         ((org-agenda-skip-function 'my/org-agenda-skip-scheduled)
          (org-agenda-sorting-strategy '(priority-down effort-down))
          (org-agenda-max-entries 3)))
        ("(" "All unscheduled by context"
         ,my/org-agenda-contexts
         ((org-agenda-skip-function 'my/org-agenda-skip-scheduled)
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
         ((org-agenda-skip-function 'my/org-agenda-skip-scheduled)
          (org-agenda-view-columns-initially nil)
          (org-tags-exclude-from-inheritance '("project"))
          (org-agenda-overriding-header "Unscheduled TODO entries: ")
          (org-columns-default-format "%50ITEM %TODO %3PRIORITY %Effort{:} %TAGS")
          (org-agenda-sorting-strategy '(todo-state-up priority-down effort-up tag-up category-keep))))
        ("r" "Unscheduled, untagged tasks" tags-todo "-someday-TODO=\"SOMEDAY\"-TODO=\"DELEGATED\"-TODO=\"WAITING\"-project-cooking-routine-evilplans-computer-writing-phone-sewing-home-errands-shopping"
         ((org-agenda-skip-function 'my/org-agenda-skip-scheduled)
          (org-agenda-view-columns-initially nil)
          (org-tags-exclude-from-inheritance '("project"))
          (org-agenda-overriding-header "Unscheduled TODO entries: ")
          (org-columns-default-format "%50ITEM %TODO %3PRIORITY %Effort{:} %TAGS")
          (org-agenda-sorting-strategy '(todo-state-up priority-down effort-up tag-up category-keep))))
        ("s" "Someday" tags-todo "TODO=\"SOMEDAY\""
         ((org-agenda-skip-function 'my/org-agenda-skip-scheduled)
          (org-agenda-view-columns-initially nil)
          (org-tags-exclude-from-inheritance '("project"))
          (org-agenda-overriding-header "Someday: ")
          (org-columns-default-format "%50ITEM %TODO %3PRIORITY %Effort{:} %TAGS")
          (org-agenda-sorting-strategy '(todo-state-up priority-down effort-up tag-up category-keep))))
        ("U" "Unscheduled tasks outside projects" tags-todo "-project-cooking-routine"
         ((org-agenda-skip-function 'my/org-agenda-skip-scheduled)
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
         ((org-agenda-files '("~/orgzly/cooking.org"))
          (org-agenda-view-columns-initially t)
          (org-agenda-sorting-strategy '(scheduled-up time-down todo-state-up)))
         )
        ("8" "List projects with tasks" my/org-agenda-projects-and-tasks
         "+PROJECT"
         ((org-agenda-max-entries 3)))))

(setq org-complete-tags-always-offer-all-agenda-tags t)
(setq org-use-fast-tag-selection nil)

(defun my/org-agenda-done (&optional arg)
  "Mark current TODO as done.
       This changes the line at point, all other lines in the agenda referring to
       the same tree node, and the headline of the tree node in the Org-mode file."
  (interactive "P")
  (org-agenda-todo "DONE"))
;; Override the key definition for org-exit
(define-key org-agenda-mode-map "x" 'my/org-agenda-done)

(defun my/org-agenda-mark-done-and-add-followup ()
  "Mark the current TODO as done and add another task after it.
       Creates it at the same level as the previous task, so it's better to use
       this with to-do items than with projects or headings."
  (interactive)
  (org-agenda-todo "DONE")
  (org-agenda-switch-to)
  (org-capture 0 "t"))
;; Override the key definition
(define-key org-agenda-mode-map "F" 'my/org-agenda-mark-done-and-add-followup)

(defun my/org-agenda-new ()
  "Create a new note or task at the current agenda item.
       Creates it at the same level as the previous task, so it's better to use
       this with to-do items than with projects or headings."
  (interactive)
  (org-agenda-switch-to)
  (org-capture 0))
;; New key assignment
(define-key org-agenda-mode-map "N" 'my/org-agenda-new)

(setq org-agenda-sorting-strategy
      '((agenda time-up priority-down tag-up category-keep effort-up)
        ;; (todo user-defined-up todo-state-up priority-down effort-up)
        (todo todo-state-up priority-down effort-up) 
        (tags user-defined-up)
        (search category-keep)))
(setq org-agenda-cmp-user-defined 'my/org-sort-agenda-items-user-defined)
(require 'cl)
(defun my/org-get-context (txt)
  "Find the context."
  (car (member-if
        (lambda (item) (string-match "@" item))
        (get-text-property 1 'tags txt))))

(defun my/org-compare-dates (a b)
  "Return 1 if A should go after B, -1 if B should go after A, or 0 if a = b."
  (cond
   ((and (= a 0) (= b 0)) nil)
   ((= a 0) 1)
   ((= b 0) -1)
   ((> a b) 1)
   ((< a b) -1)
   (t nil)))

(defun my/org-complete-cmp (a b)
  (let* ((state-a (or (get-text-property 1 'todo-state a) ""))
         (state-b (or (get-text-property 1 'todo-state b) "")))
    (or
     (if (member state-a org-done-keywords-for-agenda) 1)
     (if (member state-b org-done-keywords-for-agenda) -1))))

(defun my/org-date-cmp (a b)
  (let* ((sched-a (or (get-text-property 1 'org-scheduled a) 0))
         (sched-b (or (get-text-property 1 'org-scheduled b) 0))
         (deadline-a (or (get-text-property 1 'org-deadline a) 0))
         (deadline-b (or (get-text-property 1 'org-deadline b) 0)))
    (or
     (my/org-compare-dates
      (my/org-min-date sched-a deadline-a)
      (my/org-min-date sched-b deadline-b)))))

(defun my/org-min-date (a b)
  "Return the smaller of A or B, except for 0."
  (funcall (if (and (> a 0) (> b 0)) 'min 'max) a b))

(defun my/org-sort-agenda-items-user-defined (a b)
  ;; compare by deadline, then scheduled date; done tasks are listed at the very bottom
  (or
   (my/org-complete-cmp a b)
   (my/org-date-cmp a b)))

(defun my/org-context-cmp (a b)
  "Compare CONTEXT-A and CONTEXT-B."
  (let ((context-a (my/org-get-context a))
        (context-b (my/org-get-context b)))
    (cond
     ((null context-a) +1)
     ((null context-b) -1)
     ((string< context-a context-b) -1)
     ((string< context-b context-a) +1)
     (t nil))))

(defun my/org-sort-agenda-items-todo (a b)
  (or
   (org-cmp-time a b)
   (my/org-complete-cmp a b)
   (my/org-context-cmp a b)
   (my/org-date-cmp a b)
   (org-cmp-todo-state a b)
   (org-cmp-priority a b)
   (org-cmp-effort a b)))

(defun my/org-agenda-list-unscheduled (&rest ignore)
  "Create agenda view for tasks that are unscheduled and not done."
  (let* ((org-agenda-todo-ignore-with-date t)
         (org-agenda-overriding-header "List of unscheduled tasks: "))
    (org-agenda-get-todos)))
(setq org-stuck-projects
      '("+PROJECT-MAYBE-DONE"
        ("TODO")
        nil
        "\\<IGNORE\\>"))

(defun my/org-show-active-projects ()
  "Show my current projects."
  (interactive)
  (org-tags-view nil "project-inactive-someday"))

(use-package quantified :ensure nil :load-path "~/sync/cloud/elisp" :unless my/phone-p)
(defvar my/weekly-review-line-regexp
  "^  \\([^:]+\\): +\\(Sched[^:]+: +\\)?TODO \\(.*?\\)\\(?:[      ]+\\(:[[:alnum:]_@#%:]+:\\)\\)?[        ]*$"
  "Regular expression matching lines to include.")
(defvar my/weekly-done-line-regexp
  "^  \\([^:]+\\): +.*?\\(?:Clocked\\|Closed\\):.*?\\(TODO\\|DONE\\) \\(.*?\\)\\(?:[       ]+\\(:[[:alnum:]_@#%:]+:\\)\\)?[        ]*$"
  "Regular expression matching lines to include as completed tasks.")

(defun my/quantified-get-hours (category time-summary)
  "Return the number of hours based on the time summary."
  (if (stringp category)
      (if (assoc category time-summary) (/ (cdr (assoc category time-summary)) 3600.0) 0)
    (apply '+ (mapcar (lambda (x) (my/quantified-get-hours x time-summary)) category))))

(defun _my/extract-tasks-from-agenda (string matchers prefix line-re)
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

(ert-deftest _my/extract-tasks-from-agenda ()
  (let (list-a list-b (line-re "\\([^:]+\\):\\( \\)\\(.*\\)"))
    (_my/extract-tasks-from-agenda
     "listA: Task 1\nother: Task 2\nlistA: Task 3"
     '(("listA" . list-a)
       ("." . list-b))
     "- [ ] "
     line-re)
    (should (equal list-a '("- [ ] Task 1" "- [ ] Task 3")))
    (should (equal list-b '("- [ ] Task 2")))))

(defun _my/get-upcoming-tasks ()
  (save-window-excursion
    (org-agenda nil "W")
    (_my/extract-tasks-from-agenda (buffer-string)
                                   '(("routines" . ignore)
                                     ("business" . business-next)
                                     ("people" . relationships-next)
                                     ("tasks" . emacs-next)
                                     ("." . life-next))
                                   "  - [ ] "
                                   my/weekly-review-line-regexp)))
(defun _my/get-previous-tasks ()
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
      (_my/extract-tasks-from-agenda string
                                     '(("routines" . ignore)
                                       ("business" . business)
                                       ("people" . relationships)
                                       ("tasks" . emacs)
                                       ("." . life))
                                     "  - [X] "
                                     my/weekly-done-line-regexp))))

(defun my/org-summarize-focus-areas (date)
  "Summarize previous and upcoming tasks as a list."
  (interactive (list (org-read-date-analyze (if current-prefix-arg (org-read-date) "-fri") nil '(0 0 0))))
  (let (business relationships life business-next relationships-next life-next string emacs emacs-next
                 start end time-summary biz-time ignore base-date)
    (setq base-date (apply 'encode-time date))
    (setq start (format-time-string "%Y-%m-%d" (days-to-time (- (time-to-number-of-days base-date) 6))))
    (setq end (format-time-string "%Y-%m-%d" (days-to-time (1+ (time-to-number-of-days base-date)))))
    (setq time-summary (quantified-summarize-time start end))
    (setq biz-time (my/quantified-get-hours "Business" time-summary))
    (_my/get-upcoming-tasks)
    (_my/get-previous-tasks)
    (setq string
          (concat
           (format "- *A- (Childcare)* (%.1fh - %d%% of total)\n"
                   (my/quantified-get-hours '("A-") time-summary)
                   (/ (my/quantified-get-hours '("A-") time-summary) 1.68))
           (format "- *Business* (%.1fh - %d%%)\n" biz-time (/ biz-time 1.68))
           (mapconcat 'identity business "\n") "\n"
           (mapconcat 'identity business-next "\n")
           "\n"
           (format "  - *Earn* (%.1fh - %d%% of Business)\n"
                   (my/quantified-get-hours "Business - Earn" time-summary)
                   (/ (my/quantified-get-hours "Business - Earn" time-summary) (* 0.01 biz-time)))
           (format "  - *Build* (%.1fh - %d%% of Business)\n"
                   (my/quantified-get-hours "Business - Build" time-summary)
                   (/ (my/quantified-get-hours "Business - Build" time-summary) (* 0.01 biz-time)))
           (format "  - *Connect* (%.1fh - %d%% of Business)\n"
                   (my/quantified-get-hours "Business - Connect" time-summary)
                   (/ (my/quantified-get-hours "Business - Connect" time-summary) (* 0.01 biz-time)))
           (format "- *Relationships* (%.1fh - %d%%)\n"
                   (my/quantified-get-hours '("Discretionary - Social"
                                              "Discretionary - Family") time-summary)
                   (/ (my/quantified-get-hours '("Discretionary - Social"
                                                 "Discretionary - Family") time-summary) 1.68))
           (mapconcat 'identity relationships "\n") "\n"
           (mapconcat 'identity relationships-next "\n") "\n"
           "\n"
           (format "- *Discretionary - Productive* (%.1fh - %d%%)\n"
                   (my/quantified-get-hours "Discretionary - Productive" time-summary)
                   (/ (my/quantified-get-hours "Discretionary - Productive" time-summary) 1.68))
           (format "  - *Drawing* (%.1fh)\n"
                   (my/quantified-get-hours '("Discretionary - Productive - Drawing")  time-summary))
           (format "  - *Emacs* (%.1fh)\n"
                   (my/quantified-get-hours "Discretionary - Productive - Emacs" time-summary))
           (mapconcat 'identity emacs "\n") "\n"
           (mapconcat 'identity emacs-next "\n") "\n"
           (format "  - *Coding* (%.1fh)\n"
                   (my/quantified-get-hours "Discretionary - Productive - Coding" time-summary))
           (mapconcat 'identity life "\n") "\n"
           (mapconcat 'identity life-next "\n") "\n"
           (format "  - *Sewing* (%.1fh)\n"
                   (my/quantified-get-hours "Discretionary - Productive - Sewing" time-summary))
           (format "  - *Writing* (%.1fh)\n"
                   (my/quantified-get-hours "Discretionary - Productive - Writing" time-summary))
           (format "- *Discretionary - Play* (%.1fh - %d%%)\n"
                   (my/quantified-get-hours "Discretionary - Play" time-summary)
                   (/ (my/quantified-get-hours "Discretionary - Play" time-summary) 1.68))
           (format "- *Personal routines* (%.1fh - %d%%)\n"
                   (my/quantified-get-hours "Personal" time-summary)
                   (/ (my/quantified-get-hours "Personal" time-summary) 1.68))
           (format "- *Unpaid work* (%.1fh - %d%%)\n"
                   (my/quantified-get-hours "Unpaid work" time-summary)
                   (/ (my/quantified-get-hours "Unpaid work" time-summary) 1.68))
           (format "- *Sleep* (%.1fh - %d%% - average of %.1f per day)\n"
                   (my/quantified-get-hours "Sleep" time-summary)
                   (/ (my/quantified-get-hours "Sleep" time-summary) 1.68)
                   (/ (my/quantified-get-hours "Sleep" time-summary) 7)
                   )))
    (if (called-interactively-p 'any)
        (insert string)
      string)))

(defun my/org-add-line-item-task (task)
  (interactive "MTask: ")
  (org-insert-heading)
  (insert "[ ] " task)
  (let ((org-capture-entry '("t" "Tasks" entry
                             (file+headline "~/sync/orgzly/organizer.org" "Tasks")
                             "")))
    (org-capture nil "t")
    (insert "TODO " task "\nSCHEDULED: <" (org-read-date) ">")))
                                        ;(define-key org-mode-map (kbd "C-c t") 'my/org-add-line-item-task)

(defun my/org-list-from-rss (url from-date &optional to-date)
    "Convert URL to an Org list"
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))  
      (re-search-forward "<\\?xml")
      (goto-char (match-beginning 0))
      (let* ((feed (xml-parse-region (point) (point-max)))
            (from-time (org-read-date nil t from-date))
            (to-time (if to-date (org-read-date nil t to-date)))
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
                                        (let ((entry-time (date-to-time (elt (car (xml-get-children entry 'pubDate)) 2))))
                                          (and
                                           (not (time-less-p entry-time from-time))
                                           (or (null to-time) (time-less-p entry-time to-time)))))
                                      (xml-get-children (car (xml-get-children (car feed) 'channel)) 'item))))
                   ""))))

(defun my/org-prepare-weekly-review (&optional date skip-urls)
  "Prepare weekly review template."
  (interactive (list (org-read-date))) 
  (let ((base-date (apply 'encode-time (org-read-date-analyze date nil '(0 0 0))))
        start end links prev)
    (setq start (format-time-string "%Y-%m-%d 0:00" (days-to-time (- (time-to-number-of-days base-date) 6)) (current-time-zone)))
    (setq end (format-time-string "%Y-%m-%d 0:00" (days-to-time (1+ (time-to-number-of-days base-date))) (current-time-zone)))
    (setq prev (format-time-string "%Y-%m-%d 0:00" (days-to-time (- (time-to-number-of-days base-date) 7 6)) (current-time-zone)))
    (outline-next-heading)
    (insert
     "** Weekly review: Week ending " (format-time-string "%B %e, %Y" base-date) "  :weekly:\n"
     (my/org-summarize-journal-csv start end nil my/journal-category-map my/journal-categories)
     "\n\n*Blog posts*\n\n"
     (my/org-list-from-rss "https://sachachua.com/blog/feed" start end)
     "\n\n*Sketches*\n\n"
     (my/sketches-export-and-extract start end) "\n"
     "\n\n*Time*\n\n"
     (orgtbl-to-orgtbl
      (my/quantified-compare prev start start end
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
     "\n\n")))
(defun my/prepare-missing-weekly-reviews ()
  "Prepare missing weekly reviews based on LAST_REVIEW property."
  (interactive)
  (let ((today (substring (org-read-date nil nil ".") 0 10))
        (date (org-entry-get (point) "LAST_REVIEW")))
    (while (string< date today)
      (setq date (substring (org-read-date nil nil "++1w" nil (org-time-string-to-time date)) 0 10))
      (unless (string< today date)
        (save-excursion
          (my/org-prepare-weekly-review date))
        (org-entry-put (point) "LAST_REVIEW" date)))))

(defun _my/clean-up-flickr-list (list)
  (setq list
        (replace-regexp-in-string "\\[\"" "[" list))
  (setq list
        (replace-regexp-in-string "<a href=\"\"\\([^\"]+\\).*?>.*?</a>"
                                  "[[\\1][\\2]]" list))
  (setq list
        (replace-regexp-in-string "\"
        " "" (replace-regexp-in-string "\"\\]" "]" list))))

(defun _my/format-flickr-link-for-org (x)
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


(defun _my/parse-and-filter-flickr-csv-buffer (start end)
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


(defun my/sketches-export-and-extract (start end &optional do-insert update-db filter)
  "Create a list of links to sketches."
  (interactive (list (org-read-date) (org-read-date) t current-prefix-arg (read-string "Filter: ")))
  (let ((value
         (mapconcat
          (lambda (filename)
            (let ((base (file-name-nondirectory filename)))
              (format "- %s\n"
                      (org-link-make-string
                       (replace-regexp-in-string "#" "%23"
                                                 (concat "https://sketches.sachachua.com/"
                                                         (if (string-match "^[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9][a-z]" base)
                                                             (concat "id/" (match-string 0 base))
                                                           (concat "filename/" base))))
                       base))))
          (let ((my/sketch-directories '("~/sync/sketches"))) (my/get-sketch-filenames-between-dates start end filter))
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

(defun my/resolve-urls-in-region (beg end)
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

(defun my/open-urls-in-region (beg end)
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

(defun my/evernote-export-and-extract (start-date end-date)
  "Extract notes created on or after START-DATE and before END-DATE."
  (let ((filename "c:/sacha/tmp/Evernote.enex"))
    (call-process
     "c:/Program Files (x86)/Evernote/Evernote/enscript.exe"
     nil t t
     "exportNotes"
     "/q" (concat
           " tag:roundup"
           " created:" (replace-regexp-in-string "-" "" start-date)
           " -created:" (replace-regexp-in-string "-" "" end-date))
     "/f" filename)
    (my/evernote-extract-links-for-review filename)))

(defun my/evernote-extract-links-for-review (filename)
  "Extract note names and URLs from FILENAME.
             The file should be an ENEX export."
  (interactive (list (read-file-name "File: ")
                     (org-read-date)
                     (org-read-date)))
  (let (list)
    (with-temp-buffer
      (insert-file-contents filename)
      (goto-char (point-min))
      (while (re-search-forward "<title>\\(.+?\\)</title>\\(.*?\n\\)*?via Diigo.*?href=\"\\(.*?\\)\"" nil t)
        (setq list
              (cons
               (cons
                (match-string-no-properties 1)
                (match-string-no-properties 3)) list))))
    (setq list
          (mapconcat (lambda (x)
                       (concat "- [["
                               (kensanata/resolve-redirect (cdr x))
                               "][" (car x) "]]: ")) list "\n"))
    (if (called-interactively-p 'any)
        (insert list)
      list)))

(defun my/evernote-export-and-extract-journal ()
  "Extract and file journal entries."
  (interactive)
  (let ((filename "c:\\sacha\\tmp\\journal.enex")
        (journal-file "~/personal/journal.org"))
    (call-process
     "c:/Program Files (x86)/Evernote/Evernote/enscript.exe"
     nil t t
     "exportNotes"
     "/q" (concat
           " notebook:!Inbox"
           " intitle:Journal")
     "/f" filename)
    (my/evernote-process-journal-entries filename journal-file)))

(defun my/evernote-process-journal-entries (filename journal-file)
  "Insert all the journal entries if they do not yet exist."
  (let ((data (car (xml-parse-file filename))))
    (mapc (lambda (x)
            (if (and  (listp x) (equal (car x) 'note))
                (my/evernote-create-journal-note x journal-file)))
          data)))

(defun my/evernote-get-creation-date (note)
  "Return NOTE's created date as (month day year)."
  (let ((created (cadr (assoc-default 'created note))))
    (list (string-to-number (substring created 4 6)) ; month
          (string-to-number (substring created 6 8)) ; day
          (string-to-number (substring created 0 4))))) ; year

(defun my/evernote-create-journal-note (note journal-file)
  "Save the given NOTE to the JOURNAL-FILE."
  (with-current-buffer (find-file journal-file)
    (org-datetree-find-date-create (my/evernote-get-creation-date note))
    (forward-line 1)
    (when (org-at-heading-p) (save-excursion (insert "\n")))
    (let ((content (my/evernote-convert-content-to-org note)))
      (unless (save-excursion
                (re-search-forward (regexp-quote content)
                                   (max (point) (save-excursion (org-end-of-subtree t))) t))
        (insert content)))))

(defun my/evernote-convert-content-to-org (note)
  "Convert Evernote content for NOTE to HTML"
  (with-temp-buffer
    (insert (cadr (assoc-default 'content note)))
    (goto-char (point-min))
    (while (re-search-forward "div>" nil t)
      (replace-match "p>"))
    (shell-command-on-region (point-min) (point-max) "pandoc -f html -t org" nil t)
    (goto-char (point-min))
    (while (re-search-forward "^\\\\+" nil t)
      (replace-match ""))
    (goto-char (point-min))
    (while (re-search-forward "\\\\+$" nil t)
      (replace-match ""))
    (goto-char (point-min))
    (while (re-search-forward "\n\n\n+" nil t)
      (replace-match "\n\n"))
    (s-trim (buffer-string))))

(defun my/org-review-month (start-date)
  "Review the month's clocked tasks and time."
  (interactive (list (org-read-date)))
  ;; Set to the beginning of the month
  (setq start-date (concat (substring start-date 0 8) "01"))
  (let ((org-agenda-show-log t)
        (org-agenda-start-with-log-mode t)
        (org-agenda-start-with-clockreport-mode t)
        (org-agenda-clockreport-parameter-plist '(:link t :maxlevel 3)))
    (org-agenda-list nil start-date 'month)))

(defun _my/extract-posts-from-webpage (url)
  (with-current-buffer (url-retrieve-synchronously url)
    (goto-char (point-min))
    (re-search-forward "<pre>")
    (buffer-substring
     (point)
     (progn (re-search-forward "</pre>") (match-beginning 0)))))
(defun my/org-get-last-week ()
  "Return dates for filtering last week."
  (if (string= (format-time-string "%u") "6") ;; my week starts on Saturday
      (cons (org-read-date nil nil "-1w") (org-read-date nil nil "."))
    (cons (org-read-date nil nil "-2sat") (org-read-date nil nil "-sat"))))
(defun my/org-get-month (&optional date-string)
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

(defun my/org-prepare-monthly-review (time)
  (interactive (list (org-read-date nil t)))
  (let* ((date (decode-time time))
         (month (elt date 4))
         (year (elt date 5))
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
          title (format-time-string "%B %Y" (encode-time 0 0 0 1 month year))
          posts (_my/extract-posts-from-webpage
                 (format "https://sachachua.com/blog/%4d/%d?org=1"
                         year month))
          sketches (my/sketches-export-and-extract (substring start-date 0 10) (substring end-date 0 10) nil t))
    (calendar-increment-month month year -1)
    (setq previous-date (format "%4d-%02d-01 0:00" year month))
    (setq time (my/quantified-compare previous-date start-date start-date end-date '("Business" "Discretionary - Play" "Unpaid work" "A-" "Discretionary - Family" "Discretionary - Social" "Sleep" "Discretionary - Productive" "Personal") "Previous month %" "This month %"))
    (goto-char (line-end-position))
    (insert
     "\n\n** Monthly review: "
     title
     "  :monthly:review:\n\n"
     (my/org-summarize-journal-csv start-date end-date nil my/journal-category-map my/journal-categories '(zid)) "\n\n"
     "*Blog posts*\n"
     posts "\n\n"
     "*Sketches*\n\n"
     sketches
     "*Time*\n\n"
     (orgtbl-to-orgtbl time nil))))

(defun my/org-move-line-to-destination ()
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

(defun my/org-move-line-to-end-of-list ()
  "Move the current list item to the end of the list."
  (interactive)
  (save-excursion
    (let ((string (buffer-substring-no-properties (line-beginning-position)
                                                  (line-end-position))))
      (delete-region (line-beginning-position) (1+ (line-end-position)))
      (org-end-of-item-list)
      (insert string))))

(defun my/org-file-blog-index-entries ()
  "Keep filing until I press `C-g'."
  (interactive)
  (while t
    (my/org-file-blog-index-entry
     (line-beginning-position) (1+ (line-end-position))
     (let ((org-refile-targets
            '(("~/code/sharing/blog.org" . (:maxlevel . 3)))))
       (save-excursion (org-refile-get-location "Location"))))))

(defun my/org-file-blog-index-entry (beg end location)
  "Copy entries into blog.org."
  (interactive
   (list
    (if (region-active-p) (point) (line-beginning-position))
    (if (region-active-p) (mark) (1+ (line-end-position)))
    (let ((org-refile-targets
           '(("~/code/sharing/blog.org" . (:maxlevel . 3)))))
      (save-excursion (org-refile-get-location "Location")))))
  (let ((s
         (replace-regexp-in-string
          "^[ \t]*- \\(\\[X\\] \\)?"
          "- [X] "
          (buffer-substring-no-properties beg end))))
    ;; if we're already in blog.org, delete the previous entry
    (if (string= buffer-file-name (expand-file-name "~/code/sharing/blog.org"))
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

(defvar my/org-last-refile-marker nil "Marker for last refile")
(defun my/org-refile-in-file (&optional prefix)
  "Refile to a target within the current file."
  (interactive)
  (let ((helm-org-headings-actions
         '(("Refile to this heading" . helm-org-heading-refile))))
    (save-excursion
      (helm-org-in-buffer-headings)
      (org-end-of-subtree t)
      (setq my/org-last-refile-marker (point-marker)))))

(defun my/org-refile-to-previous ()
  "Refile subtree to last position from `my/org-refile-in-file'."
  (interactive)
  (save-selected-window
    (when (eq major-mode 'org-agenda-mode)
      (org-agenda-switch-to))
    (org-cut-subtree)
    (save-excursion
      (let* ((marker my/org-last-refile-marker)
             (target-level
              (with-current-buffer (marker-buffer marker)
                (goto-char (marker-position marker))
                (org-current-level))))
        (helm-org-goto-marker marker)
        (org-end-of-subtree t t)
        (org-paste-subtree target-level)))))

(add-to-list 'org-speed-commands-user '("w" call-interactively 'org-refile))
(add-to-list 'org-speed-commands-user '("W" call-interactively 'my/org-refile-in-file))
(add-to-list 'org-speed-commands-user '("." call-interactively 'my/org-refile-to-previous))

(defun my/org-insert-defun (function)
  "Inserts an Org source block with the definition for FUNCTION."
  (interactive (find-function-read))
  (let* ((buffer-point (condition-case nil (find-definition-noselect function nil) (error nil)))
         (new-buf (car buffer-point))
         (new-point (cdr buffer-point))
         definition)
    (if buffer-point        
        (with-current-buffer new-buf ;; Try to get original definition
          (save-excursion
            (goto-char new-point)
            (setq definition (buffer-substring-no-properties (point) (save-excursion (end-of-defun) (point))))))
      ;; Fallback: Print function definition
      (setq definition (concat (prin1-to-string (symbol-function function)) "\n")))
    (insert "#+begin_src emacs-lisp\n" definition "#+end_src\n")))

(use-package org
  :config
  (org-indent-mode 1)
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
         :base-directory "~/code/stream"
         )
        ("emacs-config"
         :base-directory "~/.config/emacs"
         :publishing-directory "~/.config/emacs"
         :publishing-function my/org-html-publish-to-html-trustingly
         )
        ("book-notes"
         :base-directory "c:/sacha/Dropbox/books"
         :publishing-directory "c:/sacha/Dropbox/books/html"
         :publishing-function my/org-html-publish-to-html-trustingly
         :makeindex t)))
;(load "~/code/dev/emacs-chats/build-site.el" t)
;(load "~/code/dev/emacs-notes/build-site.el" t)

(defun my/org-publish-maybe ()
  (require 'ox-publish)
  (interactive)
  (save-excursion
    (if (org-publish-get-project-from-filename
         (buffer-file-name (buffer-base-buffer)) 'up)
        (org-publish-current-file t)
      (my/org-html-export-trustingly))))

(defun my/org-publish-and-browse ()
  (interactive)
  (save-buffer)
  (my/org-publish-maybe)
  (browse-url (org-export-output-file-name ".html" nil default-directory)))
(bind-key "<apps> b" 'my/org-publish-and-browse)

(use-package org2blog
  :commands 'org2blog/wp-post-subtree
  :config
  (progn
    (setq org2blog/wp-track-posts nil)
    (setq org2blog/wp-use-tags-as-categories t)
    (defadvice org2blog/wp-post-buffer (around sacha activate)
      (let ((org-confirm-babel-evaluate nil)
            (org-html-toplevel-hlevel 3))
        ad-do-it))))

(defun my/org2blog-subtree ()
  "Post to my blog and get files ready."
  (interactive)
  (org2blog/wp-post-subtree)
  (my/org-stage-image-files-in-subtree)
  (shell-command "start c:\\sacha\\dropbox\\inbox\\selection")
  (browse-url "http://sachachua.com/blog/wp-admin/edit.php?page=cal"))

(defun my/org2blog-edit-post ()
  "Browse to the edit page."
  (interactive)
  (browse-url (concat "https://sachachua.com/blog/wp-admin/post.php?action=edit&post=" (org-entry-get (point) "POSTID"))))
(use-package htmlize)

(defun my/org-html-export-trustingly ()
  (interactive)
  (let ((org-confirm-babel-evaluate nil))
    (org-html-export-to-html)))

(defun my/org-html-publish-to-html-trustingly (plist filename pub-dir)
  (let ((org-confirm-babel-evaluate nil))
    (org-html-publish-to-html plist filename pub-dir)))

(setq org-html-head "<link rel=\"stylesheet\" type=\"text/css\"
       href=\"//sachachua.com/blog/wp-content/themes/sacha-v3/foundation/css/foundation.min.css\"></link>
       <link rel=\"stylesheet\" type=\"text/css\" href=\"//sachachua.com/org-export.css\"></link>
       <link rel=\"stylesheet\" type=\"text/css\" href=\"//sachachua.com/blog/wp-content/themes/sacha-v3/style.css\"></link>
       <script src=\"//ajax.googleapis.com/ajax/libs/jquery/1.11.0/jquery.min.js\"></script>")
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

(defun my/org-copy-region-as-html (beg end &optional level)
  "Make it easier to copy code for Wordpress posts and other things."
  (interactive "r\np")
  (let ((org-export-html-preamble nil)
        (org-html-toplevel-hlevel (or level 3)))
    (kill-new
     (org-export-string-as (buffer-substring beg end) 'html t))))

(defun my/org-copy-subtree-as-html ()
  (interactive)
  (my/org-copy-region-as-html
   (org-back-to-heading)
   (org-end-of-subtree)))

(setq org-html-checkbox-type 'unicode)
(setq org-html-checkbox-types
      '((unicode (on . "<span class=\"task-done\">&#x2611;</span>")
                 (off . "<span class=\"task-todo\">&#x2610;</span>")
                 (trans . "<span class=\"task-in-progress\">[-]</span>"))))

(defun my/org-share-emacs ()
  "Share my Emacs configuration."
  (interactive)
  (let* ((destination-dir "~/Dropbox/Public/")
         (destination-filename "sacha-emacs.org"))
    (my/save-new-packages)
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

    (use-package org-roam
      :if my/laptop-p
      :ensure t
      :hook
      (after-init . org-roam-mode)
      :custom
      (org-roam-directory "/home/sacha/sync/org-roam")
      :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n g" . org-roam-graph))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))
              (("C-c n I" . org-roam-insert-immediate))))

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

(defun my/org-protocol-insert-link (info) 
  "Store and insert the link at point based on INFO."
  (org-protocol-store-link info)
  (with-current-buffer (window-buffer (selected-window))
    (insert "- ")
    (org-insert-last-stored-link 1)
    (insert "\n")))
(eval-after-load 'org-protocol
  '(add-to-list 'org-protocol-protocol-alist
                '("insert-link" :protocol "insert-link" :function my/org-protocol-insert-link)))

;; javascript:location.href = 'org-protocol://copy-thumbnail?thumbnail=' + encodeURIComponent(document.querySelector('meta[property=\"og:image\"]') ? document.querySelector('meta[property=\"og:image\"]').getAttribute('content') : '') + '&title=' + encodeURIComponent(document.title) + '&url=' + encodeURIComponent(location.href) + '&videoId=' + ((typeof(videoId) !== 'undefined' ? videoId : (document.querySelector('meta[itemprop=\"videoId\"]') ? document.querySelector('meta[itemprop=\"videoId\"]').getAttribute('content') : '')) || '')

(defun my/get-youtube-info (url)
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
           (format-seconds "%h:%.2m:%.2s%z"
                           (/ (string-to-number (match-string 1)) 1000.0))))
      (kill-buffer))))

(defun my/link-video (list)
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

(defun my/org-protocol-copy-thumbnail (info) 
  "Store and insert the link at point based on INFO."
  (interactive "MURL: ")
  (when (stringp info) (setq info (list :url info)))
  (when (string-match "youtube\\.com" (plist-get info :url))
    (setq info (my/link-video info)))
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
                '("copy-thumbnail" :protocol "copy-thumbnail" :function my/org-protocol-copy-thumbnail)))

(defun my/org-elisp-link-export (link description format)
  (cond
   ((eq format 'html) (format "<span title=\"%s\">%s</span>" (replace-regexp-in-string "\"" "&quot;" link) description))
   ((eq format 'text) description)
   ))
(org-link-set-parameters
 "elisp"
 :export 'my/org-elisp-link-export)

(defun my/org-get-links-in-region (beg end)
  (save-excursion
    (let (results)
      (goto-char (min beg end))
      (while (re-search-forward org-any-link-re (max beg end) t)
        (add-to-list 'results (org-element-context)))
      results)))

(defun my/org-dired-file-links-in-region (beg end)
  "Display a Dired buffer for the file links in the selected region."
  (interactive "r")
  (let ((files
         (-map
          (lambda (x)
            (expand-file-name (org-link-unescape (plist-get (cadr x) :path))))
          (-filter
           (lambda (x)
             (string= (plist-get (cadr x) :type) "file"))
           (my/org-get-links-in-region beg end)))))
    (with-current-buffer (get-buffer-create "*Files*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (apply 'call-process "ls" nil t nil "-lR" files))
      (dired-virtual "/")
      (switch-to-buffer (current-buffer)))))

(defvar my/journal-category-map
  '(("Gross" . "Gross motor")
    ("Fine" . "Fine motor")
    ("8 - Kaizen" . "Kaizen")
    ("9 - Us" . "Us")
    ("Self-care" . "Self-care and independence"))
  "Alist of string replacements for journal categories.")
(defvar my/journal-categories
  '("Kaizen" "Us" "Field trip" "Gross motor" "Fine motor"
    "Sensory" "Language" "Music" "Art"
    "Self-care and independence" "Eating" "Sleep" "Emotion"
    "Household" "Social" "Pretend" "Cognition" "World" "Other" "Oops" "Thoughts" "Uncategorized")
  "List of categories to display. 
      Unknown categories will be added to the end.")

(defun my/journal-date (o) (elt o 3))
(defun my/journal-note (o) (car o))
(defun my/journal-week-highlight (o) (elt o 4))
(defun my/journal-category (o) (elt o 1))
(defun my/journal-pictures (o) (when (string> (elt o 2) "") (split-string (elt o 2) ",")))
(defun my/journal-id (o) (elt o 7))
(defun my/journal-status (o) (elt o 8))
(defun my/journal-other (o) (elt o 9))
(defun my/journal-zidstring (o) (elt o 11))
(defun my/org-group-journal-entries (filtered &optional category-map categories)
  (setq category-map (or category-map (my/journal-category-map)))
  (setq categories (or categories (my/journal-categories)))
  (let* ((grouped (-group-by 'my/journal-category filtered))    
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

(defun my/org-date-to-string (date &optional base-date)
  "Return the Org date specified by DATE.
      This is relative to BASE-DATE if specified."
  (org-read-date nil nil date nil (when base-date (org-read-date nil t base-date))))

(ert-deftest my/org-date-to-string ()
  (should (string= (my/org-date-to-string "++1" "2018-08-01") "2018-08-02")))

(defun my/org-filter-journal-csv (filename &optional from to highlight base-date)
  "Return a list of matching entries."
  (setq from (and from (substring (my/org-date-to-string from base-date) 0 10))
        to (and to (substring (my/org-date-to-string to base-date) 0 10)))
  (let* ((data (pcsv-parse-file filename))
         (filtered
          (-filter
           (lambda (o)
             (let ((date (my/journal-date o)))
               (and (or (null from) (not (string< date from)))
                    (or (null to) (string< date to))
                    (and (not (string= (my/journal-status o) "Deleted")))
                    (not (string-match "^!" (my/journal-note o)))
                    (string-equal
                     "true"
                     (cond
                      ((null highlight) "true")
                      ((string-equal highlight "week") (my/journal-week-highlight o))
                      (t "true"))))))
           data)))
    filtered))

(defun my/journal-read-category (&optional initial)
  (completing-read "Category: " my/journal-categories nil nil initial))

(defun my/journal-post (note &rest plist)
  (interactive (list (read-string "Note: ")
                     :Category (my/journal-read-category)
                     :Other (read-string "Other: ")))
  (setq plist (append `(:Note ,note) plist))
  (let ((url-request-method "POST")
        (url-request-extra-headers '(("Content-Type" . "application/json")))
        (json-object-type 'plist)
        (url-request-data (json-encode-plist plist)))
    (with-current-buffer (url-retrieve-synchronously "https://journal.sachachua.com/api/entries")
      (goto-char (point-min))
      (re-search-forward "^$")
      (json-read))))

(defun my/journal-get-by-zidstring (zidstring)
  (my/json-request (concat "https://journal.sachachua.com/api/entries/" zidstring)))

(defun my/journal-edit (zidstring)
  (interactive (list (my/journal-id-from-string (my/journal-completing-read))))
  (let ((entry (my/journal-get-by-zidstring zidstring)))
    (plist-put entry :Note (read-string "Note: " (plist-get entry :Note)))
    (plist-put entry :Category (my/journal-read-category (plist-get entry :Category)))
    (plist-put entry :Other (read-string "Other: " (plist-get entry :Other)))
    (apply 'my/journal-update entry)))

(defun my/journal-update (&rest plist)
  "Update journal entry using PLIST."
  (let ((url-request-method "PUT")
        (url-request-data (json-encode-plist plist)))
    (my/json-request (concat "https://journal.sachachua.com/api/entries/" (plist-get plist :ZIDString)))))
;; (my/journal-post "Hello, world")
  
(defun my/journal-get-entries (from to &optional search)
  "Return parsed CSV of entries limited by FROM, TO, and SEARCH."
  (with-current-buffer
      (url-retrieve-synchronously (format "https://journal.sachachua.com/api/entries.csv?from=%s&to=%s&q=%s" from to (or search "")))
    (goto-char (point-min))
    (delete-region (point-min) (search-forward "\n\n"))
    (cdr (pcsv-parse-buffer))))

(defun my/journal-get (url) (my/json-request (format "https://journal.sachachua.com/%s" url)))
(defun my/journal-get-entry (zid) (my/journal-get (format "api/entries/zid/%s" zid)))

(defun my/json-request (url)
  (let ((json-object-type 'plist)
        (url-request-extra-headers (cons '("Content-Type" . "application/json") url-request-extra-headers)))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (re-search-forward "^$" nil t)
      (json-read))))

(defvar my/journal-search-cache nil "List of search results.")
(defun my/journal-search-query (query-str)
  (let* ((url-request-method "GET")
         (json-response (my/json-request (format "https://journal.sachachua.com/api/entries?q=%s&limit=50"
                                                 query-str))))
    (setq my/journal-search-cache (mapcar (lambda (o)
              (cons
               (format "%s %s"
                       (plist-get o :ZIDString)
                       (plist-get o :Note))
               o))
            json-response))))

(defun my/journal-completing-read ()
  (completing-read "Journal entry: "
                   (lambda (string predicate flag)
                     (if (eq flag 'metadata)
                         `(metadata
                           (display-sort-function . ,#'identity)
                           (category . journal))
                       (let ((list (my/journal-search-query string)))
                         (mapcar 'car
                                 (if predicate
                                     (seq-filter predicate list)
                                   list)))))))

(defun my/journal-id-from-string (s)
  (when (string-match "^[-0-9]+" s) (match-string 0 s)))

(defun my/journal-view (s)
  (interactive (list (my/journal-completing-read)))
  (my/org-journal-open (my/journal-id-from-string s)))

(defun my/journal-sketch-large (zid)
  "Create a large sketch based on ZID."
  (interactive (list (my/journal-completing-read)))
  (let ((filename (expand-file-name (format "%s.psd"
                                             (my/journal-id-from-string zid))
                                    my/sketch-inbox-directory)))
    (unless (file-exists-p filename)
      (copy-file my/sketch-large-template-file filename))
    (my/org-sketch-open filename)))

(defun my/helm-journal-search (&optional values predicate ignore)
  (if (> (length helm-pattern) 2)
      (my/journal-search-query helm-pattern)))

(defun my/journal-format-entry (type o)
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

(defun my/journal-format-entries (type list)
  (mapconcat
   (lambda (o) (my/journal-format-entry type o))
   (reverse list)
   (cond
    ((eq type 'org-link-zid-only) ", ")
    ((eq type 'list-item-with-zid) "")
    ((eq type 'list-item) "")
    ((eq type 'text) " "))))

(defun my/helm-journal ()
  (interactive)
  (helm :sources
        (list
         (helm-build-dummy-source "Create post"
           :action (lambda (cand) (my/journal-post cand)))
         (helm-build-sync-source "Journal"
           :volatile t
           :candidates 'my/helm-journal-search
           :action
           (append
            (mapcar (lambda (o)
                      (cons (car o)
                            `(lambda (candidate)
                               (insert (my/journal-format-entries ,(cdr o) (helm-marked-candidates))))))
                    '(("Insert as link" . 'org-link-zid-only)
                      ("Insert as list with links" . 'list-item-with-zid)
                      ("Insert as list" . 'list-item)
                      ("Insert as text" . 'text)))
            (helm-make-actions
             "View"
             (lambda (candidate)
               (browse-url (format "https://journal.sachachua.com/zids/%s" 
                                   (mapconcat (lambda (o) (assoc-default 'ZIDString o))
                                              (helm-marked-candidates) ","))))))))))

(defun my/org-journal-open (id &optional arg)
  (browse-url (format "https://journal.sachachua.com/zid/%s" id)))

(defun my/org-journal-export (link description format)
  (let* ((path (concat "https://journal.sachachua.com/zid/" link))
         (image (concat "https://journal.sachacuha.com/zid/" link))
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

(defun my/org-journal-complete (&optional prefix)
  (cdr (assoc 'ZIDString (helm-comp-read "Entry: " 'my/helm-journal-search :volatile t))))

(use-package org
  :config
  (org-link-set-parameters
   "journal"
   :follow 'my/org-journal-open
   :export 'my/org-journal-export
   :complete 'my/org-journal-complete))

(defun my/org-journal-summarize (from to &optional search category-map categories)
  (my/org-group-journal-entries (my/journal-get-entries from to search) category-map categories))

(defun my/org-journal-format-tree (groups &optional include)
  (mapconcat
   (lambda (o)
     (concat "- *" (car o) "*\n"
             (mapconcat
              (lambda (i)
                (concat "  - "
                        (if (member 'date include) (concat (my/journal-date i) " ") "")
                        (replace-regexp-in-string "\\\"" "\"" (my/journal-note i))
                        (if (member 'zid include) (concat " " (my/journal-zidstring i)) "")
                        ;; (if (string= "" (my/journal-category i))
                        ;;     ""
                        ;;   (format " (%s)" (my/journal-category i)))
                        "\n"))
              (reverse (cdr o)) "")))
   groups ""))

(defun my/org-summarize-journal-csv (from to &optional search category-map categories include)
  (interactive
   (list (org-read-date nil nil nil "From: ")
         (org-read-date nil nil nil "To: ")
         (read-string "Search: ")
         my/journal-category-map
         my/journal-categories
         nil))
  (let ((list (my/org-journal-format-tree
               (my/org-group-journal-entries
                (my/journal-get-entries from to search) 
                category-map categories)
               include)))    
    (if (called-interactively-p 'any) (insert list) list)))

(defun my/read-journal-category ()
  (completing-read "Category: " my/journal-categories))

(defun my/update-journal-entry (old-text new-text category)
  (interactive (list (read-string "Old: ")
                     (read-string "New: ")
                     (my/read-journal-category)))
  (my/send-intent "com.sachachua.journal.categorize"
                  (list (cons "text" old-text)
                        (cons "newtext" (or new-text old-text))
                        (cons "category" (or category "Uncategorized")))))

(defun my/create-journal-entry (new-text category)
  (interactive (list (read-string "Text: ")
                     (my/read-journal-category)))
  (my/update-journal-entry new-text new-text category))

(defun my/export-journal-entries ()
  "Trigger task to export. Phone must be unlocked."
  (interactive)
  (my/send-intent "com.sachachua.journal.export" '(("a" . "b"))))

(use-package csv
  :commands csv--read-line)
(defun my/prompt-for-uncategorized-entries ()
  (interactive)
  (let ((key-list '("Note" "Date" "highlight week" "Category" "month" "Time" "Link" "ELECT"))
        x new-text category done)
    (while (and (not (eobp)) (not done))
      (forward-char 1)
      (setq x (csv--read-line key-list))
      (when (string= (assoc-default "Category" x nil "") "")
        (setq text (read-string "Text: " (assoc-default "Note" x nil "")))
        (setq category (completing-read "Category: " (cons "." my/journal-categories)))
        (if (string= category ".")
            (setq done t)
          (my/update-journal-entry (assoc-default "Note" x nil "") text category))))))

(defun my/get-image-caption (file)
  (let ((caption (shell-command-to-string (format "exiftool -s -s -s -ImageDescription %s" (shell-quote-argument file)))))
    (when (> (length caption) 0) (format "#+CAPTION: %s" caption))))

(defun my/insert-image-link-with-caption (file)
  (let ((caption (my/get-image-caption file)))
    (insert (or caption "") (org-link-make-string file) "\n")))

(defun my/caption-current-image ()
  (interactive)
  (let ((link (org-element-link-parser)) caption)
    (when (and link (org-element-property :path link))
      (setq caption (my/get-image-caption (org-element-property :path link)))
      (when caption (insert caption)))))

(defun my/set-image-caption (file caption)
  (interactive (list (if (derived-mode-p 'dired-mode) (dired-get-filename) (buffer-file-name))
                     (read-string "Caption: ")))
  (shell-command (format "exiftool -ImageDescription=\"%s\" %s" (shell-quote-argument caption) (shell-quote-argument file))))

(defvar my/photo-directory "/mnt/nfs/photos/inbox")
(defun my/get-photo-rating (file)
  (let ((rating (shell-command-to-string (concat "exiftool -s -s -s -Rating " (shell-quote-argument file)))))
    (string-to-number rating)))

(defun my/make-photo-list (start end &optional rating require-description)
  (interactive (list (org-read-date "Start: ") (org-read-date "End: ")))
  (-filter
   (lambda (filename)
     (and (string> (file-name-nondirectory filename) start)
          (string> end (file-name-nondirectory filename))
          (if rating (>= (my/get-photo-rating filename) rating) t)
          (if require-description (my/get-image-caption filename) t)))
   (directory-files my/photo-directory t ".*\\.jpg$")))

(defun my/org-get-photo (id)
  "Open the photo identified by ID."
  (car (directory-files my/photo-directory t (concat id ".*\\.jpg"))))

(defun my/org-open-photo (id)
  (find-file (my/org-get-photo id)))

                                        ;(my/make-photo-list "2018-06-10" "2018-06-15" nil t)
                                        ;(my/get-photo-rating  (my/org-get-photo "2018-06-10-18-16-31"))

(defun my/org-significant-moments (start end &optional rating)
  (interactive (list (org-read-date "Start: ") (org-read-date "End: ") 3))
  (let ((result
         (mapconcat (lambda (file)
                      (let ((caption (my/get-image-caption file)))
                        (if caption
                            (concat caption (org-link-make-string file) "\n")
                          (concat (org-link-make-string file) "\n"))))
                    (my/make-photo-list start end 3)
                    "\n")))
    (if (called-interactively-p 'any) (insert result) result)))

(setq org-attach-store-link-p 'attached)
(setq org-attach-auto-tag nil)

(use-package ob-http)

(use-package lilypond-init
  :if my/laptop-p
  :load-path "~/vendor/lilypond/elisp"
  :config
  (setq org-babel-lilypond-arrange-mode t
        org-babel-lilypond-commands '("lilypond" "timidity" "timidity")
        org-babel-lilypond-gen-pdf nil
        org-babel-lilypond-display-pdf-post-tangle nil)
  :mode ("\\.ly\\'" . LilyPond-mode))

(setq org-ditaa-jar-path "c:/sacha/Dropbox/bin/ditaa.jar")
(setq org-startup-with-inline-images t)
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
       (sqlite . t)
       (http . t)
       (ledger . t)
       (shell . t)
       (R . t)))
    (setq org-babel-python-command "python3")
    (setq python-shell-interpreter "python3")
    (add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))))

(defun my/org-summarize-task-status ()
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

(defun my/org-days-between (start end)
  "Number of days between START and END (exclusive).
      This includes START but not END."
  (- (calendar-absolute-from-gregorian (org-date-to-gregorian end))
     (calendar-absolute-from-gregorian (org-date-to-gregorian start))))

(setq org-src-window-setup 'current-window)

(defun my/copy-code-as-org-block-and-gist (beg end)
  (interactive "r")
  (let ((filename (file-name-base))
        (mode (symbol-name major-mode))
        (contents
         (if (use-region-p) (buffer-substring beg end) (buffer-string)))
        (gist (if (use-region-p) (gist-region beg end) (gist-buffer))))
    (kill-new
     (format "\n%s\n#+begin_src %s\n%s\n#+end_src\n"
             (org-link-make-string (oref (oref gist :data) :html-url) filename)
             (replace-regexp-in-string "-mode$" "" mode)
             contents))))

(setq calendar-week-start-day 6) ;; My weeks start on Saturday

(defun my/org-get-invoice-range-based-on-date (date)
  (let* ((invoice-date (org-date-to-gregorian date))
         (start (list (1- (car invoice-date)) 1 (elt invoice-date 2)))
         (end (list (car invoice-date) 1 (elt invoice-date 2))))
    (mapcar (lambda (date)
              (format-time-string "%F %H:%M" (encode-time 0 0 0 1 (elt date 0) (elt date 2))))
            (list start end))))

(defun my/org-quantified-get-hours-based-on-range (category start end)
  "Return the number of hours for the specified category."
  (/ (assoc-default category
                    (quantified-summarize-time start end)) 3600.0))

;; TODO: paginate
(defun my/org-quantified-get-detailed-hours-based-on-range (category start end)
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
          (format-time-string "%F" (my/get-week-end-for-time time))
          (format-time-string "%a" time)
          (gethash "duration" entry))))
     entries)))

(defun my/get-week-end-for-time (time &optional week-ends-on-day)
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

(ert-deftest my/org-get-week-ending-date ()
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
                (my/get-week-end-for-time (org-time-string-to-time (car test))))
               (cdr test)))
      (should (string=
               (format-time-string
                "%F"
                (my/get-week-end-for-time (org-time-string-to-time (car test)) 5))
               (cdr test))))))



(defun my/org-quantified-format-detailed-hours-as-table (list)
  "Return a table with rows for LIST.
        | Week ending ____ | Sat | Sun | Mon | Tue | Wed | Thu | Fri | Total |
        LIST elements should be in the form (date week-end-date dow seconds).
        See `my/org-quantified-get-detailed-hours-based-on-range'."
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


(defun my/org-quantified-hours-table ()
  (my/org-quantified-format-detailed-hours-as-table
   (apply 'my/org-quantified-get-detailed-hours-based-on-range 
          (org-entry-get-with-inheritance "QUANTIFIED_CATEGORY")
          (my/org-get-invoice-range-based-on-date (org-entry-get-with-inheritance "INVOICE_DATE")))))

(ert-deftest my/org-get-invoice-range-based-on-date ()
  "Check if invoice range is sane."
  (should (equal (my/org-get-invoice-range-based-on-date "2015-12-05")
                 '("2015-11-01 00:00" "2015-12-01 00:00"))))

(add-to-list 'org-speed-commands-user '("a" call-interactively 'org-archive-subtree-default))

(use-package ox-reveal :disabled t)

(defun my/org-add-dashes-to-tag-regexps ()
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
(use-package org :hook (org-mode-hook . my/org-add-dashes-to-tag-regexps))

(defun my/read-phone-entries ()
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

(defun my/org-package-open (package-name)
  (interactive "MPackage name: ")
  (describe-package (intern package-name)))

(ert-deftest my/org-package-export ()
  (should
   (string=
    (my/org-package-export "transcribe" "transcribe" 'html)
    "<a target=\"_blank\" href=\"https://elpa.gnu.org/packages/transcribe.html\">transcribe</a>"
    ))
  (should
   (string=
    (my/org-package-export "fireplace" "fireplace" 'html)
    "<a target=\"_blank\" href=\"http://melpa.org/#/fireplace\">fireplace</a>"
    )))
(defun my/org-package-export (link description format)
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
     ((eq format 'html) (format "<a target=\"_blank\" href=\"%s\">%s</a>" path desc))
     ((eq format 'wp) (format "<a target=\"_blank\" href=\"%s\">%s</a>" path desc))
     ((eq format 'latex) (format "\\href{%s}{%s}" path desc))
     ((eq format 'texinfo) (format "@uref{%s,%s}" path desc))
     ((eq format 'ascii) (format "%s <%s>" desc path))
     (t path))))

(org-link-set-parameters "package" :follow 'my/org-package-open :export 'my/org-package-export)

(setq org-ascii-links-to-notes nil)

(defun my/reddit-list-upvoted (date)
  (interactive (list (org-read-date)))
  (let ((threshold (org-read-date nil t (concat (substring date 0 (min (length date) 10)) " 0:00")))
        (url my/reddit-upvoted-json)
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
                    (concat my/reddit-upvoted-json "&after=" after)
                  nil)))))
    results))
;;  (my/reddit-list-upvoted "-mon")

(defun my/org-sort-list-in-custom-order (order)
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

(defun my/org-save-all-org-buffers () (let ((my/unfocusing t)) (org-save-all-org-buffers)))
(use-package org
  :config
  (add-function :after after-focus-change-function 'my/org-save-all-org-buffers))

;; from FAQ at http://web-mode.org/ for smartparens
(defun my/web-mode-hook ()
  (setq web-mode-enable-auto-pairing nil))

(defun my/sp-web-mode-is-code-context (id action context)
  (when (and (eq action 'insert)
             (not (or (get-text-property (point) 'part-side)
                      (get-text-property (point) 'block-side))))
    t))

(use-package web-mode
  :if my/laptop-p
  :mode "\\.html?\\'"
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
  :if my/laptop-p
  :config
  (setq lsp-headerline-breadcrumb-enable t
        gc-cons-threshold (* 100 1024 1024)
        read-process-output-max (* 1024 1024)
        company-idle-delay 0.0
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
  :if my/laptop-p
  :commands lsp-ui-mode
  :after lsp-mode)
(use-package dap-mode
  :if my/laptop-p
  :after lsp-mode)

(setq-default tab-width 2)

(defun sanityinc/kill-back-to-indentation ()
  "Kill from point back to the first non-whitespace character on the line."
  (interactive)
  (let ((prev-pos (point)))
    (back-to-indentation)
    (kill-region (point) prev-pos)))
(bind-key "C-M-<backspace>" 'sanityinc/kill-back-to-indentation)

(when (eq system-type 'windows-nt)
  (setenv "CYGWIN" "nodosfilewarning")
  (setq shell-file-name "C:/emacs/libexec/emacs/24.4/i686-pc-mingw32/cmdproxy.exe")
  (add-hook 'comint-output-filter-functions 'shell-strip-ctrl-m nil t)
  (add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt nil t))

(use-package expand-region
  :defer t
  :bind ("C-=" . er/expand-region)
  ("C-<prior>" . er/expand-region)
  ("C-<next>" . er/contract-region))

(eval-after-load 'python-mode
  '(bind-key "C-c C-c" 'compile python-mode-map))

(use-package "eldoc"
  :if my/laptop-p
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
  :if my/laptop-p
  :defer t
  :bind (:map emacs-lisp-mode-map ("C-c C-v" . erefactor-map)))

(use-package paredit 
  :if my/laptop-p
  :hook (emacs-lisp-mode . paredit-mode))
(use-package redshank
  :if my/laptop-p
  :disabled t
  :defer t
  :init (add-hook 'emacs-lisp-mode-hook 'redshank-mode))

(define-key emacs-lisp-mode-map (kbd "C-c .") 'find-function-at-point)
(bind-key "C-c f" 'find-function)

(defun my/sort-sexps-in-region (beg end)
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

(defun my/stub-elisp-defun ()
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

(bind-key "C-:" #'my/stub-elisp-defun emacs-lisp-mode-map)

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

(defun my/change-cursor-color-when-can-expand (&optional field)
  (interactive)
  (when (eq last-command 'self-insert-command)
    (set-cursor-color (if (my/can-expand)
                          yasnippet-can-fire-cursor-color
                        default-cursor-color))))

(defun my/can-expand ()
  "Return true if right after an expandable thing."
  (or (abbrev--before-point) (yasnippet-can-fire-p)))

                                        ; As pointed out by Dmitri, this will make sure it will update color when needed.
(remove-hook 'post-command-hook 'my/change-cursor-color-when-can-expand)

(defun my/insert-space-or-expand ()
  "For binding to the SPC SPC keychord."
  (interactive)
  (condition-case nil (or (my/hippie-expand-maybe nil) (insert "  "))))

(defun my/hippie-expand-maybe (arg)
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
  :if my/laptop-p
  :mode "\\.js\\'"
  :bind (:map js2-mode-map ("C-c C-c" . projectile-compile-project)))

(use-package coffee-mode
  :if my/laptop-p
  :mode "\\.coffee\\'"
  :bind (:map coffee-mode-map ("C-c C-c" . compile)))

(use-package jasminejs-mode
  :if my/laptop-p
  :after js2-mode
  :hook ((js2-mode . jasminejs-mode)
         (jasminejs-mode-hook . jasminejs-add-snippets-to-yas-snippet-dirs)))

(defvar my/javascript-test-regexp (concat (regexp-quote "/** Testing **/") "\\(.*\n\\)*")
  "Regular expression matching testing-related code to remove.
      See `my/copy-javascript-region-or-buffer'.")

(defun my/copy-javascript-region-or-buffer (beg end)
  "Copy the active region or the buffer, wrapping it in script tags.
      Add a comment with the current filename and skip test-related
      code. See `my/javascript-test-regexp' to change the way
      test-related code is detected."
  (interactive "r")
  (unless (region-active-p)
    (setq beg (point-min) end (point-max)))
  (kill-new
   (concat
    "<script type=\"text/javascript\">\n"
    (if (buffer-file-name) (concat "// " (file-name-nondirectory (buffer-file-name)) "\n") "")
    (replace-regexp-in-string
     my/javascript-test-regexp
     ""
     (buffer-substring (point-min) (point-max))
     nil)
    "\n</script>")))

(defvar my/debug-counter 1)
(defun my/insert-or-flush-debug (&optional reset beg end)
  (interactive "pr")
  (cond
   ((= reset 4)
    (save-excursion
      (flush-lines "console.log('DEBUG: [0-9]+" (point-min) (point-max))
      (setq my/debug-counter 1)))
   ((region-active-p)
    (save-excursion
      (goto-char end)
      (insert ");\n")
      (goto-char beg)
      (insert (format "console.log('DEBUG: %d', " my/debug-counter))
      (setq my/debug-counter (1+ my/debug-counter))
      (js2-indent-line)))
   (t
    ;; Wrap the region in the debug
    (insert (format "console.log('DEBUG: %d');\n" my/debug-counter))
    (setq my/debug-counter (1+ my/debug-counter))
    (backward-char 3)
    (js2-indent-line))))

(use-package js2-mode
  :if my/laptop-p
  :commands js2-mode
  :defer t
  :interpreter "node"
  :init (setq js2-basic-offset 2)
  :bind (:map js2-mode-map
              ("C-x C-e" . js-send-last-sexp)
              ("C-M-x" . js-send-last-sexp-and-go)
              ("C-c b" . js-send-buffer)
              ("C-c d" . my/insert-or-flush-debug)
              ("C-c C-b" . js-send-buffer-and-go)
              ("C-c w" . my/copy-javascript-region-or-buffer))
  :config (js2-imenu-extras-setup))

(use-package coffee-mode
  :if my/laptop-p
  :defer t
  :config (setq-default coffee-js-mode 'js2-mode coffee-tab-width 2))

(use-package rjsx-mode
  :if my/laptop-p
  ;:mode "\\.js\\'"
  :config (setq js2-basic-offset 2))

(defun my/clean-up-spans-in-region (beg end)
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

(defun my/clean-up-spans-in-string (string)
  (with-temp-buffer
    (insert string)
    (my/clean-up-spans-in-region (point-min) (point-max))
    (buffer-string)))

(ert-deftest my/clean-up-spans-in-string ()
  (should (string= (my/clean-up-spans-in-string "<span><span>Hello world</span></span>")
                   "Hello world"))
  (should (string= (my/clean-up-spans-in-string "<span><span><a href=\"http://example.com\">Hello another world</a></span></span>")
                   "<a href=\"http://example.com\">Hello another world</a>"))
  (should (string= (my/clean-up-spans-in-string "<span><h1>Leave alone</h1></span>") "<span><h1>Leave alone</h1></span>"))
  (should (string= (my/clean-up-spans-in-string "<span><a href=\"http://example.com\">Leave</a> alone</span>")
                   "<span><a href=\"http://example.com\">Leave</a> alone</span>")))

;; (ert "my/clean-up-spans-in-string")

(defun my/magit-stage-all-and-commit (message)
  (interactive (list (progn (magit-diff-unstaged) (read-string "Commit Message: "))))
  (magit-stage-modified)
  (magit-commit-create (list "-m" message))
  (call-interactively #'magit-push-current-to-pushremote))
(defvar my/magit-limit-to-directory nil "Limit magit status to a specific directory.")
(defun my/magit-status-in-directory (directory)
  "Displays magit status limited to DIRECTORY.
Uses the current `default-directory', or prompts for a directory
if called with a prefix argument. Sets `my/magit-limit-to-directory'
so that it's still active even after you stage a change. Very experimental."
  (interactive (list (expand-file-name
                        (if current-prefix-arg
                            (read-directory-name "Directory: ")
                          default-directory))))
    (setq my/magit-limit-to-directory directory)
    (magit-status directory))
(use-package magit
  :config
  (setq magit-diff-options '("-b")) ; ignore whitespace
  (defadvice magit-insert-untracked-files (around sacha activate)
    (if my/magit-limit-to-directory
        (magit-with-section (section untracked 'untracked "Untracked files:" t)
                            (let ((files (cl-mapcan
                                          (lambda (f)
                                            (when (eq (aref f 0) ??) (list f)))
                                          (magit-git-lines
                                           "status" "--porcelain" "--" my/magit-limit-to-directory))))
                              (if (not files)
                                  (setq section nil)
                                (dolist (file files)
                                  (setq file (magit-decode-git-path (substring file 3)))
                                  (magit-with-section (section file file)
                                                      (insert "\t" file "\n")))
                                (insert "\n"))))
      ad-do-it))

  (defadvice magit-insert-unstaged-changes (around sacha activate)
    (if my/magit-limit-to-directory
        (let ((magit-current-diff-range (cons 'index 'working))
              (magit-diff-options (copy-sequence magit-diff-options)))
          (magit-git-insert-section (unstaged "Unstaged changes:")
                                    #'magit-wash-raw-diffs
                                    "diff-files"
                                    "--" my/magit-limit-to-directory
                                    ))
      ad-do-it))

  (defadvice magit-insert-staged-changes (around sacha activate)
    "Limit to `my/magit-limit-to-directory' if specified."
    (if my/magit-limit-to-directory
        (let ((no-commit (not (magit-git-success "log" "-1" "HEAD"))))
          (when (or no-commit (magit-anything-staged-p))
            (let ((magit-current-diff-range (cons "HEAD" 'index))
                  (base (if no-commit
                            (magit-git-string "mktree")
                          "HEAD"))
                  (magit-diff-options (append '("--cached") magit-diff-options)))
              (magit-git-insert-section (staged "Staged changes:")
                                        (apply-partially #'magit-wash-raw-diffs t)
                                        "diff-index" "--cached" base "--" my/magit-limit-to-directory))))
      ad-do-it))
  :bind (("C-x v C-d" . my/magit-status-in-directory)
         ("C-c g" . magit-file-dispatch)
         ("C-x g" . magit-status)
         ("C-x v p" . magit-push)
         ("C-x v c" . my/magit-stage-all-and-commit)))

;; ;; From http://endlessparentheses.com/merging-github-pull-requests-from-emacs.html
;; (defun endless/load-gh-pulls-mode ()
;;   "Start `magit-gh-pulls-mode' only after a manual request."
;;   (interactive)
;;   (require 'magit-gh-pulls)
;;   (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)
;;   (magit-gh-pulls-mode 1)
;;   (magit-gh-pulls-reload))

;; (use-package magit-gh-pulls)

(defvar my/git-clone-destination "~/vendor")
(defun my/git-clone-clipboard-url ()
  "Clone git URL in clipboard asynchronously and open in dired when finished."
  (interactive)
  (cl-assert (string-match-p "^\\(http\\|https\\|ssh\\)://" (current-kill 0)) nil "No URL in clipboard")
  (let* ((url (current-kill 0))
         (download-dir (expand-file-name my/git-clone-destination))
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

(defun my/recursive-find-file (file &optional directory)
  "Find the first FILE in DIRECTORY or its parents."
  (setq directory (or directory (file-name-directory (buffer-file-name)) (pwd)))
  (if (file-exists-p (expand-file-name file directory))
      (expand-file-name file directory)
    (unless (string= directory "/")
      (my/recursive-find-file file (expand-file-name ".." directory)))))

(defun my/find-tags ()
  "Set the TAGS file."
  (set (make-variable-buffer-local 'tags-table-list) nil)
  (set (make-variable-buffer-local 'tags-file-name)
       (my/recursive-find-file "TAGS")))

(eval-after-load 'drupal-mode
  '(progn
     (add-hook 'drupal-mode-hook 'my/find-tags)))

(use-package projectile
  :diminish projectile-mode
  :config
  (progn
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
    (projectile-mode +1)
    (setq projectile-completion-system 'default)
    (setq projectile-enable-caching t)
    (setq projectile-indexing-method 'alien)
    (add-to-list 'projectile-globally-ignored-files "node-modules")))
(use-package helm-projectile
  :if my/laptop-p)



(use-package rinari :if my/laptop-p)
(use-package bundler :if my/laptop-p)
(use-package robe
  :if my/laptop-p
  :hook
  ((ruby-mode-hook . robe-mode)
   (robe-mode-hook . ac-robe-setup)
   (ruby-mode-hook . auto-complete-mode)))
(use-package haml-mode
  :if my/laptop-p
  :mode "\\.haml\\'")

(defun my/rspec-verify-single ()
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
  :if my/laptop-p
  :config
  (progn
    (setq rspec-command-options "--fail-fast --format documentation")
    (bind-key "C-c , ," 'rspec-rerun rspec-mode-map)
    (fset 'rspec-verify-single 'my/rspec-verify-single)))

(use-package sass-mode
  :if my/laptop-p
  :hook (sass-mode-hook . (lambda () (setq indent-tabs-mode nil))))
(setq-default indent-tabs-mode nil)

(use-package skewer-mode
  :if my/laptop-p
  :hook 
  ((js2-mode-hook . skewer-mode)
   (css-mode-hook . skewer-css-mode)
   (html-mode-hook . skewer-html-mode)))

(use-package company
  :if my/laptop-p
  :config (add-hook 'prog-mode-hook 'company-mode))

(use-package tern
  :if my/laptop-p
  :bind (:map tern-mode-keymap ("C-c C-c" . compile))
  :hook (js2-mode-hook . tern-mode)
  :config
  (when (eq system-type 'windows-nt) (setq tern-command '("cmd" "/c" "tern"))))

(use-package dockerfile-mode
  :mode ("Dockerfile\\'" . dockerfile-mode))

(use-package erc
  :if my/laptop-p
  :config
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
  )

(defun my/announce-on-irc-and-twitter (channels message host port)
  (call-process "t" nil 0 nil "update" message)
  (with-temp-buffer
    (insert "PASS " erc-password "\n"
            "USER " erc-nick "\n"
            "NICK " erc-nick "\n"
            (mapconcat (lambda (o)
                         (format "PRIVMSG %s :%s\n" o message))
                       channels "")
            "QUIT\n")
    (call-process-region (point-min) (point-max) "ncat" nil 0 nil
                         "--ssl" host port)))

(defun my/schedule-announcement (time message)
  (interactive (list (org-read-date t t) (read-string "Message: ")))
  (run-at-time time nil #'my/announce-on-irc-and-twitter '("#emacs" "#emacsconf") message erc-server erc-port))

(defmacro my/org-with-current-task (&rest body)
  "Execute BODY with the point at the subtree of the current task."
  `(if (derived-mode-p 'org-agenda-mode)
       (save-window-excursion
         (org-agenda-switch-to)
         ,@body)
     ,@body))

(defun my/org-clock-in-and-track ()
  "Start the clock running. Clock into Quantified Awesome."
  (interactive)
  (my/org-with-current-task
   (org-clock-in)
   (call-interactively 'my/org-quantified-track)
   (when obs-websocket-streaming-p (my/stream-message (org-get-heading t t t t)))
   (cond
    ((org-entry-get (point) "AUTO")
     (org-link-open-from-string (org-entry-get (point) "AUTO")))
    (t
     (save-restriction
       (org-narrow-to-subtree)
       (org-next-link)
       (when (looking-at org-link-any-re)
         (org-open-at-point)))))))
(bind-key "!" 'my/org-clock-in-and-track org-agenda-mode-map)

(defmacro my/with-org-task (&rest body)
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

(defun my/org-quantified-track (&optional category note)
  "Create a tracking record using CATEGORY and NOTE.
      Default to the current task in the agenda, the currently-clocked
      entry, or the current subtree in Org."
  (interactive (list nil nil))
  (unless (and category note)
    (my/with-org-task
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

(defun my/org-quick-clock-in-task (location jump)
  "Track and clock in on the specified task.
      If JUMP is non-nil or the function is called with the prefix argument, jump to that location afterwards."
  (interactive (list (save-excursion (my/org-refile-get-location "Location")) current-prefix-arg))
  (when location
    (if jump
        (progn (org-refile 4 nil location) (my/org-clock-in-and-track))
      (save-window-excursion
        (org-refile 4 nil location)
        (my/org-clock-in-and-track)))))
(bind-key "C-c q" 'my/org-quick-clock-in-task)

(require 'quantified nil t)

(defun my/compare-times (clocked estimated)
  (if (and (> (length clocked) 0) estimated)
      (format "%.2f"
              (/ (* 1.0 (org-hh:mm-string-to-minutes clocked))
                 (org-hh:mm-string-to-minutes estimated)))
    ""))

(defvar my/workrave-file (expand-file-name ".\\Workrave\\historystats" (getenv "AppData")))

(defun my/workrave-transform-statistics (&optional file)
  (interactive (list my/workrave-file))
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

(defun my/strip-blog-share ()
  (interactive)
  (let (base)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              "<div class=\"sharedaddy sd-sharing-enabled\">.*?<div class=\"sharing-clear\"></div></div></div></div>" nil t)
        (replace-match "")))))

(defun my/artrage-export-png (directory &optional prefix)
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

(defun my/ssh-refresh ()
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
(my/ssh-refresh)

(defun sanityinc/adjust-opacity (frame incr)
  (let* ((oldalpha (or (frame-parameter frame 'alpha) 100))
         (newalpha (+ incr oldalpha)))
    (when (and (<= frame-alpha-lower-limit newalpha) (>= 100 newalpha))
      (modify-frame-parameters frame (list (cons 'alpha newalpha))))))
(global-set-key (kbd "M-C-8") (lambda () (interactive) (sanityinc/adjust-opacity nil -2)))
(global-set-key (kbd "M-C-9") (lambda () (interactive) (sanityinc/adjust-opacity nil 2)))
(global-set-key (kbd "M-C-0") (lambda () (interactive) (modify-frame-parameters nil `((alpha . 100)))))

(setq browse-url-browser-function 'browse-url-xdg-open)
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
  (when my/phone-p
    (add-to-list 'org-file-apps '("\\.png\\'" . default))
    (add-to-list 'org-file-apps '("\\.jpg\\'" . default))
    (add-to-list 'org-file-apps '("\\.jpeg\\'" . default)))
  )

(defun my/format-intent (intent &optional params)
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

(defun my/send-intent (intent &optional params)
  "Send broadcast INTENT to my phone.
      PARAMS is a plist of :key value pairs."
  (let ((command (my/format-intent intent params)))
    (if my/phone-p
        (shell-command command)
      (shell-command (format "ssh phone %s" (shell-quote-argument command))))))

(use-package clipmon
  :disabled t
  :init (progn (setq clipmon-action 'kill-new clipmon-timeout nil clipmon-sound nil clipmon-cursor-color nil clipmon-suffix nil) (clipmon-mode)))

(use-package xclip :if my/phone-p) ; Turn on with xclip-mode

(use-package engine-mode
  :config
  (progn
    (defengine my-blog "https://www.google.ca/search?q=site:sachachua.com+%s" :keybinding "b")
    (defengine mail "https://mail.google.com/mail/u/0/#search/%s" :keybinding "m")
    (defengine google "http://google.com/search?q=%s" :keybinding "g")
    (defengine emacswiki "http://google.com/search?q=site:emacswiki.org+%s" :keybinding "e")
    (bind-key* "C-c s" 'my/engine-mode-hydra/body)
    (engine-mode)))

(setq gnus-select-method '(nnnil ""))
(setq gnus-secondary-select-methods
      '((nntp "news.gmane.io")
        ;; (nnmaildir "mail"
        ;;            (directory "~/Maildir")
        ;;            (directory-files nnheader-directory-files-safe) 
        ;;            (get-new-mail nil))
        (nnimap "imap.googlemail.com"
                (nnimap-address "imap.googlemail.com")
                (nnimap-server-port 993)
                (nnimap-stream ssl)
                (nnimap-authenticator login))
        ;; (nnimap "localhost" 
        ;;   (nnimap-address "localhost")
        ;;   (nnimap-stream network)
        ;;   (nnimap-user "sacha")
        ;;   (nnimap-authenticator login)
        ;;   (nnimap-authinfo-file "~/.authinfo.gpg"))
        ))
(setq smtpmail-smtp-server "smtp.googlemail.com"
      smtpmail-smtp-service 587
      gnus-check-new-newsgroups nil
      gnus-activate-level 2
      gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials '(("localhost" 25 "sacha@local.sachachua.com" nil))
      smtpmail-auth-credentials '(("localhost" 25 "sacha@local.sachachua.com" nil))
      smtpmail-default-smtp-server "localhost"
      smtpmail-smtp-server "localhost"
      smtpmail-smtp-service 25
      smtpmail-local-domain "local.sachachua.com")
(setq send-mail-function 'smtpmail-send-it)
(setq smtpmail-smtp-server "127.0.0.1")
(setq smtpmail-smtp-service 25)
(setq user-mail-address "sacha@sachachua.com")

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

(use-package ledger-mode
  :load-path "~/vendor/ledger-mode"
  :mode "\\.ledger$" 
  :bind (:map ledger-mode-map
              ("C-c C-n" . my/ledger-change-account)
              ("C-c a" . my/ledger-set-unknown-account)
              ("C-c f" . (lambda () (interactive) (find-file (my/latest-file "~/Downloads"))))))
(defun my/open-latest-download ()
  (interactive)
  (find-file (my/latest-file "~/Downloads")))

(defvar my/ledger-account-list-cache nil)
(make-variable-buffer-local 'my/ledger-account-list-cache)
(defadvice ledger-accounts-list (around sacha activate)
  "Cache"
  (setq ad-return-value (or my/ledger-account-list-cache
                            (setq my/ledger-account-list-cache ad-do-it))))

(defun my/ledger-set-unknown-account (account point)
  (interactive (list (ledger-read-account-with-prompt "Account") (point)))
  (let ((extents (ledger-navigate-find-xact-extents point)))
    (save-excursion
      (goto-char (car extents))
      (if (re-search-forward "Expenses:Unknown" (cadr extents) t)
          (replace-match account t t)
        (goto-char point)
        (beginning-of-line)
        (when (re-search-forward "\\([^ \t]+\\)  " (line-end-position) nil)
          (replace-match account t t nil 1))))))

(defun my/ledger-go-to-beginning-of-entry ()
  "Move to the beginning of the current entry."
  (while (and (not (bobp))
              (eq (ledger-context-line-type (ledger-context-at-point))
                  'acct-transaction))
    (forward-line -1)))

(defun my/ledger-entry-date ()
  "Returns the date of the entry containing point or nil."
  (save-excursion
    (my/ledger-go-to-beginning-of-entry)
    (let ((context-info (ledger-context-other-line 0)))
      (when (eq (ledger-context-line-type context-info) 'entry)
        (goto-char (line-beginning-position))
        (if (looking-at "\\([-0-9\\./]+\\)")
            (match-string-no-properties 1))))))

(defun my/ledger-guess-mbna ()
  "Adds a sub-account for the dates for my credit card transactions."
  (interactive)
  (save-excursion
    (my/ledger-go-to-beginning-of-entry)
    (forward-line 1)
    (let ((amount 0) (date (my/ledger-entry-date)) month)
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

(defun my/latest-file (path &optional filter)
  "Return the newest file in PATH. Optionally filter by FILTER."
  (car (sort (seq-remove #'file-directory-p (directory-files path 'full filter t)) #'file-newer-than-file-p)))
(defun my/ledger-change-account (account)
  (interactive (list (ledger-read-account-with-prompt (concat (ledger-xact-payee) ": "))))
  (beginning-of-line)
  (re-search-forward ledger-account-name-regex)
  (replace-match account t t))
(defun my/ledger-fix-unknown ()
  (interactive)
  (while (re-search-forward "Expenses:Unknown.*$ \\(.+\\)" nil t)
    (my/ledger-change-account (ledger-read-account-with-prompt
                               (format "%s %s: " (s-trim (save-match-data (ledger-xact-payee)))
                                       (match-string 1))))))

(add-hook 'after-make-frame-functions
          (lambda (frame)
            (select-frame frame)
            (my/setup-color-theme)))

(use-package crdt :load-path "~/vendor/crdt.el" :ensure nil :if my/laptop-p)

(define-key-after global-map [menu-bar my-menu] (cons "Shortcuts" (make-sparse-keymap "Custom shortcuts")) 'tools)
(define-key global-map [menu-bar my-menu journal] '("Show journal entries" . my/show-missing-journal-entries))
(define-key global-map [menu-bar my-menu agenda] '("Org agenda" . (lambda () (interactive) (org-agenda nil "a"))))
(define-key global-map [menu-bar my-menu audio] '("Process audio" . (lambda () (interactive) (shell-command "~/bin/process-audio &"))))
(define-key global-map [menu-bar my-menu new-index-card] '("New index card" . (lambda () (interactive)
                                                                                (my/org-sketch-open (my/prepare-index-card-template)))))

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
  :if my/laptop-p)
(use-package avy-zap
  :if my/laptop-p
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

(defun my/test-urls (urls)
  "Given a list of URLs, return a list of any URLS that don't result in an OK value."
  (delq nil
        (mapcar (lambda (url)
                  (let ((url-request-method "HEAD"))
                    (with-current-buffer (url-retrieve-synchronously url)
                      (goto-char (point-min))
                      (unless (looking-at "HTTP/1.1 200 OK") url))))
                urls)))

;; https://emacs.stackexchange.com/questions/19035/finding-frames-by-name
(defun my/get-frame-by-name (fname)
  "If there is a frame named FNAME, return it, else nil."
  (seq-find (lambda (frame)
              (when (equal fname (frame-parameter frame 'name))
             frame))
            (frame-list)))
;; (obs-websocket-send "GetSourceSettings" :sourceName "Command log" :callback (lambda  (frame payload) (prin1 payload)))
(defun my/wmctl-get-id (window-name)
  (string-to-number (replace-regexp-in-string "^0x\\|\n" "" (shell-command-to-string (format "wmctrl -l | grep %s | head -1 | awk '{print $1}'" (shell-quote-argument window-name)))) 16))

(defvar my/stream-ffmpeg-multicast nil "Process for multicasting the stream")
(defun my/stream-ffmpeg-multicast ()
  (interactive)
  (unless (process-live-p my/stream-ffmpeg-multicast)
    (setq my/stream-ffmpeg-multicast (start-process "FFmpeg multicast" "*ffmpeg multicast*" "~/bin/ffmpeg-multicast"))))

(defun my/stream-fix-sources ()
  (interactive)
  (obs-websocket-send
   "SetSourceSettings"
   :sourceName "Gstreamer - command log"
   :sourceSettings
   `(:pipeline ,(format "ximagesrc xid=0x%x use-damage=0 ! queue max-size-buffers=0 max-size-time=0 max-size-bytes=0  min-threshold-time=4000000000 ! video." (my/wmctl-get-id "command-log")) :use_timestamps_audio t :use_timestamps_video t) :sourceType "gstreamer-source")
  (obs-websocket-send "SetSourceSettings" :sourceName "Command log"
                      :sourceSettings
                      `(:capture_window
                        ,(format "%d\n%s\n%s"
                                 (my/wmctl-get-id "command-log")
                                 " *command-log*"
                                 "emacs"))))

(use-package command-log-mode
  :if my/laptop-p
  :commands
  command-log-mode
  clm/open-command-log-buffer
  :defines
  clm/command-log-buffer
  )
(defun my/stream-set-up-frames ()
  (interactive)
  (global-command-log-mode 1)
  (unless (my/get-frame-by-name (buffer-name clm/command-log-buffer))
    (switch-to-buffer-other-frame clm/command-log-buffer))
  (clm/with-command-log-buffer
    (text-scale-set 3))
  (call-process "wmctrl" nil 0 nil "-r" (number-to-string (my/wmctl-get-id "command-log")) "-e" "0,0,100,1366,100"))o

(defun my/stream-set-up ()
  (interactive)
  (my/stream-ffmpeg-multicast)
  (obs-websocket-connect)
  (my/stream-toggle-background-music)
  (selectric-mode 1)
  (my/stream-set-up-frames)
  (my/stream-fix-sources)
  (obs-websocket-minor-mode 1)
  (unless (and (erc-get-buffer "#sachachua")
               (with-current-buffer (erc-get-buffer "#sachachua")
                 (erc-server-process-alive)))
    (my/twitch-irc)))

(defvar my/background-music-process nil "Process for playing background music")
(defun my/stream-toggle-background-music (&optional enable)
  (interactive)
  (if (or my/background-music-process
          (and (numberp enable) (< enable 0)))
      (progn
        (when (process-live-p my/background-music-process)
          (kill-process my/background-music-process))
        (setq my/background-music-process nil))
    (let ((files (directory-files "~/code/music" t "mid\\'")))
      (setq my/background-music-process
            (apply
             'start-process
             "*Music*"
             nil
             (append (list "timidity" "-idlr" "--volume=10") files))))))

(defun my/selectric-type-sound ()
  "Make the sound of typing."
  ;; Someday, randomize this or something
  (selectric-make-sound (expand-file-name "selectric-move.wav" selectric-files-path)))

(use-package selectric-mode
  :if my/laptop-p
  :diminish ""
  :config
  (fset #'selectric-type-sound #'my/selectric-type-sound))

(defun my/pacmd-set-device (regexp status)
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

(defvar my/mic-p nil "Non-nil means microphone is on")
(add-to-list 'mode-line-front-space '(:eval (if my/mic-p "*MIC*" "")))

(defun my/mic-off ()
  (interactive)
  (my/pacmd-set-device "Yeti" 'off)
  (my/pacmd-set-device "Internal Microphone" 'off)
  (setq my/mic-p nil))
(defun my/mic-on ()
  (interactive)
  (my/pacmd-set-device "Yeti" 'on)
  (my/pacmd-set-device "Internal Microphone" 'on)
  (setq my/mic-p t))
(defun my/mic-toggle ()
  (interactive)
  (if my/mic-p (my/mic-off) (my/mic-on)))

(defvar my/push-to-talk-mute-timer nil "Timer to mute things again.")
(defvar my/push-to-talk-last-time nil "Last time my/push-to-talk was run")
(defvar my/push-to-talk-threshold 0.5 "Number of seconds")

(defun my/push-to-talk-mute ()
  (interactive)
  (message "Muting.")
  (my/mic-off)
  (force-mode-line-update)
  (when obs-websocket-recording-p (my/obs-websocket-add-caption "[Microphone off]")))

(defun my/push-to-talk ()
  "Tap to toggle microphone on and off, or repeat the command to make it push to talk."
  (interactive)
  (cond
   ((null my/mic-p) ;; It's off, so turn it on
    (when (timerp my/push-to-talk-mute-timer)
      (cancel-timer my/push-to-talk-mute-timer)) 
    (my/mic-on)
    (when obs-websocket-recording-p (my/obs-websocket-add-caption "[Microphone on]"))
    (setq my/push-to-talk-last-time (current-time)))
   ((timerp my/push-to-talk-mute-timer) ;; Push-to-talk mode
    (cancel-timer my/push-to-talk-mute-timer)
    (setq my/push-to-talk-mute-timer
          (run-at-time my/push-to-talk-threshold nil #'my/push-to-talk-mute)))
   ;; Might be push to talk, if we're within the key repeating time
   ((< (- (time-to-seconds (current-time)) (time-to-seconds my/push-to-talk-last-time)) 
       my/push-to-talk-threshold)
    (setq my/push-to-talk-mute-timer
          (run-at-time my/push-to-talk-threshold nil #'my/push-to-talk-mute)))
   ;; It's been a while since I turned the mic on.
   (t (my/push-to-talk-mute))))

(global-set-key (kbd "<f12>") #'my/push-to-talk)

(defun my/stream-message (text)
  (interactive "MText: ")
  (obs-websocket-send "SetSourceSettings" :sourceName "OBSMessage" :sourceSettings 
                      (list :text
                            (concat "Notes at stream.sachachua.com\n"
                                    (mapconcat 'identity (org-wrap text 80) "\n"))))
  (my/obs-websocket-add-caption text)
  (when obs-websocket-streaming-p
    (with-current-buffer (find-file-noselect "~/code/stream/index.org")
      (org-link-search "Timestamps")
      (forward-line 1)
      (insert (format "- (%s) %s\n"
                      (format-seconds "%h:%.2m:%.2s%z" (my/obs-websocket-stream-time-secs))
                      text))))
  (when (erc-get-buffer "#sachachua")
    (with-current-buffer (erc-get-buffer "#sachachua")
      (erc-send-message text))))

(defvar my/obs-websocket-last-stream-timecode nil "(timecode-string . system-time)")
(defvar my/obs-websocket-last-recording-timecode nil "(timecode-string . system-time)")

(defun my/obs-websocket-message-handler (payload)
  "Save the current streaming timecode."
  (pcase (plist-get payload :update-type)
    ("RecordingStarted" (my/obs-websocket-check-recording-timecode))
    ("StreamStatus"
     (setq my/obs-websocket-last-stream-timecode (cons (plist-get payload :stream-timecode) (current-time))))))

(defun my/obs-websocket-check-recording-timecode ()
  (obs-websocket-send "GetRecordingStatus"
                      :callback
                      (lambda (frame payload)
                        (setq my/obs-websocket-last-recording-timecode
                              (cons (plist-get payload :recordTimecode) (current-time))))))

(defun my/obs-websocket-timecode-to-msecs (time-string)
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

(defun my/obs-websocket-adjust-timecode (timecode-time)
  "Returns the current adjusted time in milliseconds based on TIMECODE-TIME. 
TIMECODE-TIME is an alist of (timecode-string . elisp-time)."
  (when timecode-time
    (+ 
     (my/obs-websocket-timecode-to-msecs (car timecode-time))
     (* 1000.0
        (- (time-to-seconds (current-time)) 
           (time-to-seconds (cdr timecode-time)))))))

(defun my/obs-websocket-stream-time-secs ()
  "Return current stream time in seconds."
  (/ (my/obs-websocket-adjust-timecode my/obs-websocket-last-stream-timecode) 1000.0))

(defun my/obs-websocket-stream-time-msecs ()
  "Return current stream time in milliseconds."
  (my/obs-websocket-adjust-timecode my/obs-websocket-last-stream-timecode))

(defun my/obs-websocket-recording-time-secs ()
  "Return current recording time in seconds."
  (/ (my/obs-websocket-adjust-timecode my/obs-websocket-last-recording-timecode) 1000.0))

(defun my/obs-websocket-recording-time-msecs ()
  "Return current recording time in milliseconds."
  (my/obs-websocket-adjust-timecode my/obs-websocket-last-recording-timecode))

(defun my/obs-websocket-caption-file (&optional filename)
  "Return the caption file for the current video."
  (setq filename (or filename obs-websocket-recording-filename))
  (when filename
    (expand-file-name (concat (file-name-sans-extension filename) ".vtt")
                      (file-name-directory filename))))

(defun my/obs-websocket-add-caption (text &optional ms)
  (interactive (list (read-string "Text: ")))
  (obs-websocket-send "SendCaptions" :text text)
  (setq ms (or ms (my/obs-websocket-recording-time-msecs)))
  (when obs-websocket-recording-filename
    (with-current-buffer (find-file-noselect (my/obs-websocket-caption-file))
      (goto-char (point-max))
      (when (bobp) (insert "WEBVTT\n\n"))
      (subed-append-subtitle nil ms nil text)
      (save-excursion
        (when (subed-backward-subtitle-text)
          (subed-set-subtitle-time-stop ms)))
      (save-buffer))))

(defun my/stream-intermission (text)
  "Start an intermission and prompt me for a message."
  (interactive "MText: ")
  (obs-websocket-send "SetCurrentScene" :scene-name "Intermission")
  (my/stream-message text))

(defun my/show-emacs-tasks ()
  (interactive)
  (org-ql-search (org-agenda-files)
    '(and (todo)
          (parent (and (tags "project") (tags "emacs") (not (tags "inactive")))))
    :title "Emacs-related project tasks"
    :sort '(date priority todo)
    :super-groups '((:auto-parent t))))

(use-package obs-websocket
  :if my/laptop-p
  :after pretty-hydra
  :config
  (add-to-list 'obs-websocket-on-message-payload-functions #'my/obs-websocket-message-handler)
  (defhydra my/stream-recording (:exit t)
           "Recording"
           ("b" (obs-websocket-send "StartRecording") "Begin")
           ("r" (obs-websocket-send "StartStopRecording") "Toggle")
           (" " (obs-websocket-send "PauseRecording") "Pause")
           ("p" (my/play-latest-recording) "Play last")
           ("c" (obs-websocket-send "ResumeRecording") "Continue")
           ("e" (obs-websocket-send "StopRecording") "End"))
  (defvaralias 'my/mic-toggle 'my/mic-p)
  (defun my/stream-toggle-streaming () (interactive)  (obs-websocket-send "StartStopStreaming"))
  (defun my/stream-toggle-recording () (interactive) (obs-websocket-send "StartStopRecording"))
  (defvaralias 'my/stream-toggle-streaming 'obs-websocket-streaming-p)
  (defvaralias 'my/stream-toggle-recording 'obs-websocket-recording-p)
  (pretty-hydra-define my/stream (:quit-key "q")
    ("Setup"
     (("C" obs-websocket-connect "Connect")
      ("f" my/stream-ffmpeg-multicast "FFMpeg Multicast" :toggle t)
      ("bt" selectric-mode "Typing sounds")
      ("bm" my/stream-toggle-background-music "Background music")
      ("a" my/show-emacs-tasks "Agenda")
      ("I" my/stream-captions-insert "Insert caption" :toggle t)
      ("us" (browse-url "https://twitch.tv/sachachua") "View stream")
      ("uv" (browse-url "https://dashboard.twitch.tv/u/sachachua/stream-manager") "View manager")
      ("uy" (browse-url "https://studio.youtube.com/channel/UClT2UAbC6j7TqOWurVhkuHQ/livestreaming/dashboard") "Youtube")
      ("m" my/mic-toggle "Toggle mic" :toggle t))
     "Streaming/recording"
     (("s" my/stream-toggle-streaming "Streaming - toggle" :toggle t :exit t)
      ("r" my/stream-toggle-recording "Recording - toggle" :toggle t :exit t)
      ("v" (my/play-latest-recording) "Play last"))
     "Scenes"
     (("d" (obs-websocket-send "SetCurrentScene" :scene-name "Desktop") "Desktop" :exit t)
      ("e" (obs-websocket-send "SetCurrentScene" :scene-name "Emacs") "Emacs" :exit t)
      ("i" (lambda () (interactive)
             (obs-websocket-send "SetCurrentScene" :scene-name "Intermission")
             (call-interactively #'my/stream-message))
       "Intermission" :exit t))
     "Captions"
     (("n" my/obs-websocket-add-caption "Add caption" :exit t)
      ("c" (find-file (my/obs-websocket-caption-file)) "View captions" :exit t)
      ("t" my/stream-message "Message" :hint nil :exit t)
      ("<f8>" my/stream-message "Message" :hint nil :exit t))))
  (global-set-key (kbd "<f8>") #'my/stream/body)
  (add-to-list 'obs-websocket-on-message-payload-functions #'my/obs-websocket-message-handler)
  :load-path "~/code/obs-websocket-el" :ensure nil)

(use-package mpv :if my/laptop-p)
(defvar my/recordings-dir "~/videos/")
(defun my/play-latest-recording ()
  (interactive)
  (let ((latest (my/latest-file my/recordings-dir)))
    (if (file-exists-p (my/obs-websocket-caption-file latest))
        (with-current-buffer (find-file-noselect (my/obs-websocket-caption-file (my/latest-file my/recordings-dir)))
          (goto-char (point-min))
          (subed-mpv-find-video latest)
          (pop-to-buffer (current-buffer)))
      (mpv-play (my/latest-file my/recordings-dir)))))

 (setq imp-default-user-filters '((org-mode . my/imp-htmlize-filter)
                                  (mhtml-mode . nil)
                                  (html-mode . nil)
                                  (web-mode  . nil)))
  (defun my/imp-htmlize-filter (buffer)
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

(defvar my/stream-captions-websocket nil)
(defvar my/stream-captions-history nil)
(defvar my/stream-captions-last-caption nil)
(defvar my/stream-captions-insert nil "Non-nil means insert into the current buffer.")
(defun my/stream-captions-insert () (interactive) (setq my/stream-captions-insert (not my/stream-captions-insert)))

(define-minor-mode my/stream-captions-minor-mode "Toggle the captions server."
  :lighter "CAP"
  :global t)

(defun my/stream-captions-on-message (websocket frame)
  (let* ((payload (json-parse-string (websocket-frame-payload frame) :object-type 'plist :array-type 'list))
         (caption (string-trim (plist-get (car (plist-get (car (plist-get payload :results)) :alternatives)) :transcript))))
    (setq my/stream-captions-last-caption caption)
    (call-process "notify-send" nil nil nil caption)
    (my/obs-websocket-add-caption caption)
    (when my/stream-captions-insert (insert caption))
    (setq my/stream-captions-history (cons caption my/stream-captions-history))))

(defun my/stream-captions-edit-last (caption)
  (interactive (list (read-string "Caption: " my/stream-captions-last-caption 'my/stream-captions-history my/stream-captions-last-caption)))
  (when (> (length caption) 0)
    (my/obs-websocket-add-caption caption)))
(global-set-key (kbd "<f11>") 'my/stream-captions-edit-last)
    
(defun my/stream-captions-on-close (&rest args)
  (message "Captions websocket closed.")
  (my/stream-captions-minor-mode 0)
  (setq my/stream-captions-websocket nil))
 
(defun my/stream-captions-websocket-connect ()
  (interactive)
  (setq my/stream-captions-history nil)
  (my/stream-captions-minor-mode 1)
  (setq my/stream-captions-websocket (websocket-open "ws://localhost:8085"
                                                     :on-message #'my/stream-captions-on-message
                                                     :on-close #'my/stream-captions-on-close)))

(defvar my/stream-captions-process nil)
(defun my/stream-captions-start ()
  (interactive)
  (let ((default-directory "~/code/speech"))
    (setq my/stream-captions-process (start-process "Stream captions" (get-buffer-create "*stream captions*") "node" "test.js"))
    (sleep-for 2)
    (my/stream-captions-websocket-connect)))

(defun my/stream-captions-sentinel (process event)
  (let ((status (process-status my/stream-captions-process)))
    (if (member status '(stop exit signal))
        (my/stream-captions-minor-mode -1))))
(defun my/stream-captions-stop ()
  (interactive)
  (stop-process my/stream-captions-process))

(use-package smartparens
  :if my/laptop-p
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
    (sp-local-pair 'web-mode "<" nil :when '(my/sp-web-mode-is-code-context))

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

(setq epa-file-encrypt-to '("sacha@sachachua.com"))
(setq epa-pinentry-mode 'loopback)
(setq epg-pinentry-mode 'loopback)

(require 'dash)
(require 'dash-functional)
(defmacro my/convert-shell-scripts-to-interactive-commands (directory)
  "Make the shell scripts in DIRECTORY available as interactive commands."
  (cons 'progn
        (-map
         (lambda (filename)
           (let ((function-name (intern (concat "my/shell/" (file-name-nondirectory filename)))))
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

(my/convert-shell-scripts-to-interactive-commands "~/bin")

(defun my/resolve-orgzly-syncthing ()
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
  (directory-files-recursively directory "\\.sync-conflict-"))


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

(defun my/search-irc-logs ()
  (interactive)
  (let ((helm-rg-default-directory "~/backups/server/home/.znc/users/sachac/moddata/log/freenode"))
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

(use-package paint
  :disabled t
  :if my/laptop-p
  :load-path "~/cloud/elisp"
  :init 
  (progn
    (setq paint-foreground-color "white" paint-background-color "black")
    (defun my/paint () (interactive) (delete-other-windows) (paint 1600 900 nil))))

(use-package oddmuse
  :if my/laptop-p
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

(defun my/describe-random-interactive-function ()
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

(defun my/split-sentence-and-capitalize ()
  (interactive)
  (delete-char 1)
  (insert ".")
  (capitalize-word 1))
(defun my/split-sentence-delete-word-and-capitalize ()
  (interactive)
  (delete-char 1)
  (insert ".")
  (kill-word 1)
  (capitalize-word 1))
(defun my/delete-word-and-capitalize ()
  (interactive)
  (skip-syntax-backward "w")
  (kill-word 1)
  (capitalize-word 1))

(defun my/emms-player-mplayer-set-speed (speed)
  "Depends on mplayer's -slave mode"
  (interactive "MSpeed: ")
  (process-send-string emms-player-simple-process-name
                       (format "speed_set %s\n" speed)))

(defvar my/emms-player-mplayer-speed-increment 0.1)

(defun my/emms-player-mplayer-speed-up ()
  "Depends on mplayer's -slave mode"
  (interactive)
  (process-send-string emms-player-simple-process-name
                       (format "speed_incr %f\n" my/emms-player-mplayer-speed-increment)))
(defun my/emms-player-mplayer-slow-down ()
  "Depends on mplayer's -slave mode"
  (interactive)
  (process-send-string emms-player-simple-process-name
                       (format "speed_incr %f\n" (- 0 my/emms-player-mplayer-speed-increment))))

(use-package rainbow-delimiters :disabled t)

(defvar my/org-quantified-categories
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

(defun my/org-summarize-time-use (&optional start end)
  (interactive (list (org-read-date) (org-read-date)))
  (let ((time-summary (quantified-summarize-time start end))
        (categories my/org-quantified-categories)
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

(defun my/org-summarize-upcoming-week ()
  "Summarize upcoming tasks as a list."
  (interactive)
  (org-agenda nil "w")
  (let ((string (buffer-string))
        business relationships life)
    (with-temp-buffer
      (insert string)
      (goto-char (point-min))
      (while (re-search-forward my/weekly-review-line-regexp nil t)
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

(defun my/org-summarize-previous-week ()
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
        (while (re-search-forward my/weekly-review-line-regexp nil t)
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

(defun my/quantified-compare (start1 end1 start2 end2 &optional categories label1 label2)
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

(defun my/animate-emacs-chat ()
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

(defvar my/sketch-directories
  '("~/sync/sketches"
    "~/cloud/private-sketches"
    "~/Dropbox/Inbox"
    "~/Dropbox/Inbox/To blog"))

(defun my/get-sketch-filenames-between-dates (start end filter)
  "Returns index card filenames between START and END."
  (setq start (replace-regexp-in-string "[^0-9]" "" start))
  (setq end (replace-regexp-in-string "[^0-9]" "" end))
  (my/get-sketch-filenames
   (lambda (filename)
     (let ((f (replace-regexp-in-string "[^0-9]" "" (file-name-nondirectory filename))))
       (and (string> f start)
            (string> end f)
            (or (not filter) (string-match filter filename)))))))

(defun my/get-sketch-filenames (base &optional as-regexp)
  (my/get-image-filenames base as-regexp my/sketch-directories))
(defun my/get-image-filenames (base &optional as-regexp directories)
  "Check several directories for files matching BASE.
           Return the matching filenames, if any.
           If AS-REGEXP is non-nil, treat BASE as a regular expression.
           If BASE is a function, use that to filter."
  (-filter
   (lambda (o) (not (string-match "\\.xmp" o)))
   (sort (-flatten
          (delq nil
                (mapcar
                 (lambda (dir)
                   (and (file-directory-p dir)
                        (if (functionp base)
                            (-filter base (directory-files dir t ".*\\.\\(png\\|psd\\|tiff\\|jpg\\)?$"))
                          (directory-files
                           dir t
                           (concat 
                            "\\("
                            (if as-regexp base (regexp-quote base))
                            "\\)"
                            ".*\\(\\.\\(png\\|psd\\|tiff\\|jpg\\)\\)?$"
                            )))))
                 (or directories my/image-directories))))
         'string<)))

(defun my/get-image-filename (base &optional as-regexp directories)
  "Check several directories for files matching BASE.
           Return the first matching filename, if any.
           If AS-REGEXP is non-nil, treat BASE as a regular expression."
  (if (file-exists-p base)
      base
    (car (my/get-image-filenames base as-regexp directories))))
(defun my/get-sketch-filename (base &optional as-regexp)
  (my/get-image-filename base as-regexp my/sketch-directories))

(defun my/list-sketches (regexp &optional full-filename directories)
  "Return a list of sketch filenames matching REGEXP."
  (interactive (list (read-string "Filter: ")))
  (let ((my/sketch-directories (or directories my/sketch-directories)))
    (funcall (if (called-interactively-p 'interactive)
                 (lambda (x) (insert (mapconcat (lambda (y) (concat "- " (org-link-make-string (concat "sketch:" y)))) x "\n"))) 'identity)
             (sort (-uniq
                    (mapcar (if full-filename 'identity
                              'file-name-nondirectory)
                            (my/get-sketch-filenames regexp t)))
                   'string>))))

(defun my/open-images-in-krita (files)
  (apply 'call-process "krita" nil 0 nil "--nosplash" files))
(defun my/open-images-in-gwenview (files)
  (apply 'call-process "gwenview" nil 0 nil "--slideshow" files))
(defun my/open-images-in-feh (files)
  (apply 'call-process "feh" nil nil nil "-D" "1" "-F" files))
(defun my/org-image-open (id &optional arg directories)
  "Open image named ID.
      If ARG is specified, prompt for application to open it in."
  (interactive (list
                (completing-read "Sketch ID: " (my/list-sketches "."))
                (current-prefix-arg)))
  (let* ((files (mapcar (lambda (o) (my/get-image-filename o (or my/image-directories))) (if (listp id) id (list id))))
         (input (if arg (read-char "(k)rita, (g)wenview, (f)eh: ") ?k)))
    (funcall
     (cond
      ((eq input ?g) 'my/open-images-in-gwenview)
      ((eq input ?f) 'my/open-images-in-feh)
      (t 'my/open-images-in-krita))
     files)))
(defun my/org-sketch-open (id &optional arg)
  (my/org-image-open id arg my/sketch-directories))
(defun my/org-image-export (link description format)
  (let* ((path (concat "https://sketches.sachachua.com/filename/" link))
         (image (concat "https://sketches.sachachua.com/static/" link))
         (desc (or description link)))
    (cond
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

(defun my/org-sketch-complete (&optional prefix)
  (concat "sketch:"
          (my/complete-sketch-filename)))
(defun my/org-image-complete (&optional prefix)
  (concat "image:"
          (completing-read "Image: " (my/list-sketches "." nil my/image-directories))))
;; Based on https://emacs.stackexchange.com/questions/38098/org-mode-custom-youtube-link-syntax
(defun my/org-sketch-preview (start end path bracketp)
  "Include overlays for sketches."
  (when (display-graphic-p)
    (let ((filename (my/get-sketch-filename path))
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
   :follow 'my/org-sketch-open
   :export 'my/org-image-export
   :complete 'my/org-sketch-complete
   :activate-func nil))

(use-package org
  :config
  (setq org-image-actual-width 600)
  (org-link-set-parameters
   "image"
   :follow 'my/org-image-open
   :export 'my/org-image-export
   :complete 'my/org-image-complete))

(use-package org
  :config
  (org-link-set-parameters
   "copy"
   :follow (lambda (link) (kill-new link))
))

(defun my/helm-source-org-sketch-list ()
  (my/list-sketches "."))

(defun my/helm-org-insert-sketch-candidates (&optional candidates)
  (mapc (lambda (o)
          (org-insert-link nil (concat "sketch:" o))
          (insert "\n"))
        (helm-marked-candidates)))

(defun my/helm-open-sketches-in-krita (&optional candidates)
  (my/sketch-open-in-krita (helm-marked-candidates)))

(defun my/helm-open-sketches-in-gwenview (&optional candidates)
  (my/sketch-open-in-gwenview (helm-marked-candidates)))

(defun my/helm-open-sketches-in-feh (&optional candidates)
  (my/sketch-open-in-feh (helm-marked-candidates)))

(defvar my/helm-source-org-sketches
  '((name . "Sketches")
    (candidates . my/helm-source-org-sketch-list)
    (action . (("Insert" . my/helm-org-insert-sketch-candidates)
               ("Open in Krita" . my/helm-open-sketches-in-krita)
               ("Open in Gwenview" . my/helm-open-sketches-in-gwenview)
               ("Open as Feh slideshow" . my/helm-open-sketches-in-feh)))
    (persistent-action . my/helm-open-sketches-in-gwenview)))

(defun my/helm-org-sketches ()
  (interactive)
  (helm :sources '(my/helm-source-org-sketches)
        :buffer "*helm-org-sketches*"))

(defun my/set-up-sketch-buffer ()
  "Populate a widget buffer with a few handy buttons."
  (interactive)
  (with-current-buffer (get-buffer-create "*Done*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (widget-create 'push-button
                     :notify (lambda (&rest ignore)
                               (my/org-clock-in-and-track-by-name "Draw"))
                     "Track: Draw")        
      (widget-create 'push-button
                     :notify (lambda (&rest ignore)
                               (my/org-clock-in-and-track-by-name "Draw journal entries"))
                     "Track: Journal")
      (widget-create 'push-button
                     :notify (lambda (&rest ignore)
                               (my/org-sketch-open (my/prepare-index-card-template)))
                     "New")
      (widget-create 'push-button
                     :notify (lambda (&rest ignore)
                               (my/org-sketch-open (my/prepare-large-template)))
                     "New large")
      (widget-create 'push-button
                     :notify (lambda (&rest ignore)
                               (my/org-sketch-open (my/prepare-index-card-template nil (org-read-date))))
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
                               (my/rotate-screen 0)
                               (kill-buffer)
                               (my/rename-scanned-cards))
                     "Process")
      (widget-create 'push-button
                     :notify (lambda (&rest ignore)
                               (my/rotate-screen 0)
                               (delete-window)
                               (my/rename-scanned-cards))
                     "Rename")        
      (widget-create 'push-button
                     :notify (lambda (&rest ignore)
                               (my/rotate-screen 0)
                               (delete-window)
                               (my/convert-and-upload-cards))
                     "Upload")
      (widget-create 'push-button
                     :notify (lambda (&rest ignore)
                               (my/rotate-screen 0)
                               (org-clock-out)
                               (kill-buffer))
                     "Quit")
      (text-scale-set 10)
      (widget-setup)
      (widget-minor-mode)
      (pop-to-buffer (current-buffer))
      (goto-char (point-min))
      (current-buffer))))

(setq my/sketch-executable "krita"
      my/sketch-inbox-directory "~/Dropbox/Inbox"
      my/index-card-template-file "~/Dropbox/drawings/templates/0 - index.psd"
      my/sketch-large-template-file "/home/sacha/Dropbox/drawings/templates/0 - base.psd")
(defun my/prepare-index-cards (n)
  (interactive (list (or current-prefix-arg 5)))
  (let ((counter 1)
        (directory "~/Dropbox/Inbox")
        (template my/index-card-template-file)
        (date (substring (org-read-date nil nil ".") 0 10))
        temp-file)
    (quantified-track "Drawing")
    (dotimes (i 5) (my/org-sketch-open (my/prepare-index-card-template)))
    (my/rotate-screen 180)
    (my/set-up-sketch-buffer)))

(defvar my/index-card-file-name nil "Most recent index card file name.")
(defun my/rotate-screen (degrees)
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

(defun my/prepare-drawing-template (&optional name date template)
  "Create the image file for NAME. Return the new filename."
  (let* ((date (or date (substring (org-read-date nil nil ".") 0 10)))
         (data (my/journal-post (or name "sketch") :Date date)))
    (setq name (expand-file-name
                (concat (assoc-default 'ZIDString data)
                        (if name
                            (concat " "
                                    (my/convert-sketch-title-to-filename (or name "")))
                          
                              "") 
                            "." (file-name-extension template))
                    "~/Dropbox/Inbox"))
    (copy-file (or template my/index-card-template-file) name)
    name))

(defun my/org-insert-new-index-card-link ()
  (interactive)
  (let ((filename
         (my/prepare-index-card-template)))
    (insert "[[sketch:" filename "]]\n")
    (save-window-excursion
      (my/rotate-screen 180)
      (shell-command
       (concat (shell-quote-argument my/sketch-executable)
               " " (shell-quote-argument filename) " &")))))

(defun my/prepare-index-card-template (&optional name date)
  "Create the image file for NAME. Return the new filename."
  (my/prepare-drawing-template name date my/index-card-template-file))

(defun my/prepare-large-template (&optional name date)
  "Create the image file for NAME. Return the new filename."
  (my/prepare-drawing-template name date my/sketch-large-template-file))


(defun my/prepare-index-card (&optional name date)
  "Prepare the index card for NAME.
              Rotate the screen and show a button to un-rotate the screen."
  (interactive (list (read-string "Name: ")
                     (substring (if current-prefix-arg (org-read-date) (org-read-date nil nil ".")) 0 10)))
  (setq my/index-card-file-name (my/prepare-index-card-template name date))
  (save-window-excursion
    (my/rotate-screen 180)
    (shell-command
     (concat (shell-quote-argument my/sketch-executable)
             " " (shell-quote-argument my/index-card-file-name) " &")))
  (my/set-up-sketch-buffer))

(defun my/prepare-index-card-for-subtree ()
  "Create an index card template for the current subtree."
  (interactive)
  (let* ((heading (elt (org-heading-components) 4)))
    (unless (org-entry-get (point) "Effort") (org-set-property "Effort" "0:15"))
    (if (derived-mode-p 'org-agenda-mode) (org-agenda-clock-in) (org-clock-in))
    (my/org-quantified-track "Drawing")
    (if (org-at-heading-p) (forward-line 1))
    (my/prepare-index-card heading)))

(defun my/helm-org-prepare-index-card-for-subtree (candidate)
  (let ((location (org-refile--get-location candidate my/helm-org-refile-locations)))
    (save-window-excursion
      (save-excursion
        (org-refile 4 nil location)
        (my/prepare-index-card-for-subtree)) t)))

(defun my/draw-journal-entry (date)
  "Creates a blank journal entry for DATE and brings up the log."
  (interactive (list (org-read-date)))
  ;; Open the Quantified Awesome time log for that date
  (let ((filename (my/get-journal-entry date))
        (day (format-time-string "%A" (org-time-string-to-time date))))
    (if filename
        (my/org-sketch-open filename)
      ;; (browse-url (format "http://quantifiedawesome.com/records?start=%s&end=%s"
      ;;                     date
      ;;                     (format-time-string
      ;;                      "%Y-%m-%d"
      ;;                      (seconds-to-time
      ;;                       (+ (org-time-string-to-seconds date) 86400)))))
      (setq filename
            (my/prepare-index-card-template (concat day " #daily #journal") date))
      (my/org-sketch-open filename))))

(defun my/get-journal-entry (date)
  "Returns the filename for the journal sketch for DATE."
  (car
   (-filter (lambda (x) (not (string-match "weekly" x)))
            (my/get-sketch-filenames
             (format "%s.* .*#daily" date)
             t))))

(defun my/get-missing-journal-dates (start-date end-date)
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
      (unless (my/get-journal-entry current-date-string)
        (add-to-list 'missing-list current-date-string))
      (setq current-day (1- current-day)))
    missing-list))

(defun my/show-missing-journal-entries (since)
  (interactive (list (if current-prefix-arg (org-read-date) (org-read-date nil nil "-7"))))
  (let ((missing-dates (my/get-missing-journal-dates since (org-read-date nil nil "."))))
    (with-current-buffer (my/set-up-sketch-buffer)
      (mapc
       (lambda (date)
         (widget-create 'push-button
                        :date date
                        :notify (lambda (widget &rest ignore)
                                  (my/draw-journal-entry (plist-get (cdr widget) :date)))
                        date))
       missing-dates)
      (widget-setup)
      (widget-minor-mode))))

(use-package s)
(defun my/process-tiff (files)
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

(defun my/convert-index-card-to-png (o)
  (lambda (o)
    (call-process "krita" nil nil nil o "--export" "--export-filename"
                  (concat (file-name-sans-extension o) ".png"))
    (rename-file o "~/Dropbox/Inbox/backup/" t)))

(defun my/convert-index-card-tiffs-to-pngs ()
  (interactive)
  (let ((pattern "^\\(IMG\\|[0-9]+-[0-9]+-[0-9]+\\).*.\\(tif\\|psd\\)$"))
    (when (directory-files "~/Dropbox/Inbox/" t pattern)
      ;; Convert the TIFFs first
      (mapc 'my/convert-index-card-to-png
            (directory-files "~/Dropbox/Inbox/" t pattern)))))

(defun my/convert-and-upload-cards ()
  "Trust in existing filenames, upload without modification."
  (interactive)
  (my/convert-index-card-tiffs-to-pngs)
  (my/upload-scanned-cards))

(defun my/rename-scanned-card (filename)
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
                    (setq old-name (my/get-sketch-filename (match-string 0 new-name)))
                    (and old-name
                         (not (string= old-name filename))
                         (not (string= (file-name-nondirectory old-name)
                                       (concat (s-trim new-name) "." (file-name-extension filename))))))
          (setq new-name
                (read-string (format "Already exists (%s) - new name: " old-name)
                             new-name)))
        (when (string-match new-name "^\\(.*?\\) *| *\\(.*\\)")
          (with-current-buffer (find-file "~/Dropbox/orgzly/Inbox.org")
            (goto-char (point-max))
            (insert "\n* " (match-string 1 new-name) "\n" (match-string 2 new-name))
            (save-buffer))
          (setq new-name (match-string 1 new-name)))
        (when (> (length new-name) 0)
          (revert-buffer t t)
          (rename-file filename (concat (s-trim new-name) "." (file-name-extension filename)) t)
          (kill-buffer))))))

(defun my/rename-scanned-cards ()
  "Display and rename the scanned or saved files."
  (interactive)
  (my/convert-index-card-tiffs-to-pngs)
  (mapc (lambda (o)
          (when (string= (file-name-extension o) "psd")
            (my/convert-index-card-to-png o)
            (setq o (concat (file-name-sans-extension o) ".png")))
          (my/rename-scanned-card o))
        (reverse (directory-files "~/Dropbox/Inbox/" t "^\\(IMG\\|[0-9]+-[0-9]+-[0-9]+\\).*.\\(psd\\|png\\|jpg\\)")))
  (my/upload-scanned-cards))

(defun my/clean-index-card-directory ()
  "Remove files marked for deletion and move private files."
  (shell-command "mv ~/Dropbox/Inbox/*delete* ~/Dropbox/Inbox/backup")
  (shell-command "mv ~/Dropbox/Inbox/*private* ~/cloud/private-sketches/"))

(defun my/upload-scanned-cards ()
  (interactive)
  (my/clean-index-card-directory)
  (with-current-buffer (get-buffer-create "*Files to be uploaded*")
    (erase-buffer)
    (insert (mapconcat 'identity (directory-files "~/Dropbox/Inbox" nil "^[0-9]+-[0-9]+-[0-9]+[^ ]? .*.\\(png\\|jpg\\)") "\n"))
    (goto-char (point-min))
    (switch-to-buffer (current-buffer))
    (delete-other-windows))
  (shell-command "~/bin/copy-sketches"))

(use-package image+
  :if my/laptop-p
  ;;    :load-path "~/elisp/Emacs-imagex"
  :commands (imagex-global-sticky-mode imagex-auto-adjust-mode)
  :init (progn (imagex-global-sticky-mode) (imagex-auto-adjust-mode)))

(defun my/prepare-sketchnote-file ()
  (interactive)
  (let* ((base-name (org-entry-get-with-inheritance  "BASENAME")))
    (unless base-name (error "Missing basename property"))
    (my/org-sketch-open (my/prepare-large-template base-name))))

(defun my/follow-up-on-sketch (filename)
  "Prompt for FILENAME to follow up on.
      Create an index card with it as a layer, and add the ref to the filename."
  (interactive (list (helm-read-file-name "Image: " :initial-input "~/sketches/")))
  ;; Allow the specification of a short identifier
  (unless (file-exists-p filename) 
    (setq filename (car (directory-files "~/sketches" t (concat "^" filename)))))
  (let ((async-shell-command-buffer 'new-buffer)
        (index-card (my/prepare-index-card-template      
                     (format "-- index card ref %s"
                             (and (string-match "^[^ \\.]+" (file-name-nondirectory filename))
                                  (match-string 0 (file-name-nondirectory filename)))))))
    (shell-command (format "convert %s %s -colorspace cmyk %s"
                           (shell-quote-argument (expand-file-name my/index-card-template-file))
                           (shell-quote-argument (expand-file-name filename))
                           (shell-quote-argument (expand-file-name index-card))))
    (shell-command (format "%s %s &"
                           (shell-quote-argument my/sketch-executable)
                           (shell-quote-argument (expand-file-name index-card))))
    (my/rotate-screen 180)
    (my/set-up-sketch-buffer)))

(defun my/org-stage-image-files-in-subtree ()
  "Move corresponding linked images to staging directory."
  (interactive)
  (save-excursion
    (save-restriction
      (org-narrow-to-subtree)
      (goto-char (point-min))
      (while (re-search-forward org-bracket-link-regexp nil t)
        (let ((filename (file-name-nondirectory (or (match-string 3) (match-string 1)))))
          (when (and (string-match "\\.png$" filename)
                     (file-exists-p (expand-file-name filename "~/Dropbox/Inbox/To blog")))
            (rename-file
             (expand-file-name filename "~/Dropbox/Inbox/To blog")
             "~/Dropbox/Inbox/Selection")))))))

(defun my/org-get-list-categories ()
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

(defvar my/helm-org-list-candidates nil)
(defun my/helm-org-list-categories-init-candidates ()
  "Return a list of categories from this list in a form ready for Helm."
  (setq my/helm-org-list-candidates
        (mapcar (lambda (x)
                  (cons (if (elt x 3)
                            (format "%s - %s" (car x) (elt x 3))
                          (car x))
                        x))
                (my/org-get-list-categories))))

(defun my/org-guess-list-category (&optional categories)
  (interactive)
  (require 'cl-lib)
  (unless categories
    (setq categories
          (my/helm-org-list-categories-init-candidates)))
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
      (my/org-move-current-item-to-category
       (cdr (car found)))
      t)))

(defvar my/org-browse-link-while-categorizing 'eww-readable
  "Set to nil to skip browsing.")

(defun my/org-guess-uncategorized ()
  "Interactively move linked list items to categories from the list.
        Try to guess categories based on substring matches."
  (interactive)
                                        ;(my/helm-org-list-categories-init-candidates)
  (let ((categories (my/org-get-list-categories))
        category)
    (while (and (looking-at "^[-+] \\[\\[\\([^]]+\\)\\]\\[\\([^]]+*\\)")
                (not (string= "done" category)))
      (save-excursion
        ;; (when (eq my/org-browse-link-while-categorizing 'eww-readable)
        ;;   (save-excursion (save-match-data (my/eww-browse-readable (match-string 1)))))
        (setq category (completing-read (match-string 2) categories))
        (unless (string= category "done")
          (my/org-move-current-item-to-category category))))))

;; From https://emacs.stackexchange.com/questions/36284/how-to-open-eww-in-readable-mode/47757
(defun my/eww-readable-nonce ()
  "Once-off call to `eww-readable' after EWW is done rendering."
  (unwind-protect
      (eww-readable)
    (remove-hook 'eww-after-render-hook #'my/eww-readable-nonce)))

(defun my/eww-browse-readable (url)
  (when (looking-at "^[-+] \\[\\[\\([^]]+\\)")
    (add-hook 'eww-after-render-hook #'my/eww-readable-nonce)
    (eww (match-string 1))))

(defun my/org-sort-list-by-regexp (regexp)
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
     nil ?f sort-func 'string<)))

(defun my/refile-sketches-to-questions ()
  (interactive)
  (while (looking-at "^  \\+ \\[\\[.*?\\]\\[\\(.*?\\) -- \\(.*?\\)\\]\\]\n")
    (let ((link (match-string 0))
          (title (match-string 1)))
      (save-excursion
        (if (save-match-data (search-forward (concat "* " title) nil t))
            (progn (forward-line) (insert (match-string 0)) (replace-match ""))
          (forward-line 1))))))

(setq yas-indent-line 'fixed)
(defun my/convert-sketch-title-to-filename (text)
  (setq text (replace-regexp-in-string "[?!]$" "" text))
  (setq text (replace-regexp-in-string "[?!:] " " - " text)))
(ert-deftest my/convert-sketch-title-to-filename ()
  (should (string= (my/convert-sketch-title-to-filename "Test") "Test"))
  (should (string= (my/convert-sketch-title-to-filename "Another Test!") "Another Test"))
  (should (string= (my/convert-sketch-title-to-filename "Does this work? Yes") "Does this work - Yes"))
  (should (string= (my/convert-sketch-title-to-filename "Title: Subtitle") "Title - Subtitle"))
  )

(defun my/convert-sketched-book-to-png ()
  "Convert TIFF to PNG."
  (interactive)
  (let ((basename (org-entry-get-with-inheritance "BASENAME")))
    (shell-command (format "convert \"c:/sacha/dropbox/inbox/%s.tif\" \"c:/sacha/dropbox/inbox/%s.png\""
                           basename
                           basename))))

(defun my/index-sketched-book ()
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

(defun my/package-sketched-book ()
  "Add the latest sketch and package the collection."
  (interactive)
  (shell-command
   (format "plink -A vagrant@127.0.0.1 -P 2222 \"cd ~/Dropbox/Packaging/sketched-books; git add '%s.png'; git commit -m 'Added %s - %s' -a; git push; make all\" &"
           (org-entry-get-with-inheritance "BASENAME")
           (org-entry-get-with-inheritance "SHORT_TITLE")
           (org-entry-get-with-inheritance "AUTHOR"))))

(defun my/get-tile-dimensions (num-items orig-width orig-height target-aspect-ratio)
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
(ert-deftest my/get-tile-dimensions ()
  (should (equal (my/get-tile-dimensions 2 2 1 1) (cons 1 2)))
  (should (equal (my/get-tile-dimensions 4 2 1 0.5) (cons 1 4)))
  (should (equal (my/get-tile-dimensions 12 1 1 (/ 4.0 3.0)) (cons 4 3)))
  (should (equal (my/get-tile-dimensions 11 1 1 (/ 4.0 3.0)) (cons 4 3)))
  (should (equal (my/get-tile-dimensions 13 1 1 (/ 4.0 3.0)) (cons 4 4))))

(defun my/extract-image-filenames (beg end)
  "Return the filenames from the links in this region."
  (let (files)
    (save-excursion
      (goto-char (min beg end))
      (while (re-search-forward "sketch:" (max beg end) t)
        (let ((link (org-element-context)))
          (add-to-list 'files (org-element-property :path link))))
      files)))

(defun my/create-sketch-montage (files &optional tiles)
  "Combine the sketches in the region."
  (interactive
   (list
    (if (derived-mode-p 'dired-mode)
        (dired-get-marked-files)
      (mapcar 'my/get-sketch-filename
              (my/extract-image-filenames (min (point) (mark)) (max (point) (mark)))))
    (if current-prefix-arg (read-string "Tiling: "))))
  ;; Extract the links
  (let ((output-file "~/Dropbox/Inbox/output.png"))
    (unless tiles
      (setq tiles
            (format "%dx"
                    (car (my/get-tile-dimensions (length files) 1500 900 (/ 4.0 3))))))
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

(defun my/create-week-montage (beg end)
  (interactive "r")
  (let* ((date (org-read-date nil nil (unless current-prefix-arg "-fri")))
         (filename (format "Week ending %s #journal #weekly" date))
         (full-filename (my/get-sketch-filename filename)))
    (if full-filename
        (my/org-sketch-open full-filename)
      (my/create-index-card-montage 
       (mapcar 'my/get-sketch-filename
               (my/extract-image-filenames (min (point) (mark)) (max (point) (mark)))) 
       "2x"
       (my/prepare-index-card-template filename)))))

(defun my/create-index-card-montage (files &optional tiling filename)
  "Prepare an index card with a montage of the selected sketches as a layer."
  (interactive
   (list
    (if (derived-mode-p 'dired-mode)
        (dired-get-marked-files)
      (mapcar 'my/get-sketch-filename
              (my/extract-image-filenames (min (point) (mark)) (max (point) (mark)))))))
  (let ((async-shell-command-buffer 'new-buffer)
        (index-card (or filename (my/prepare-index-card-template))))
    (my/create-sketch-montage files tiling)
    (shell-command
     (format "convert %s \\( %s -resize 1500x900 \\) -colorspace cmyk %s"
             (shell-quote-argument (expand-file-name my/index-card-template-file))
             (shell-quote-argument (expand-file-name "~/Dropbox/Inbox/output.png"))
             (shell-quote-argument (expand-file-name index-card))))
    (shell-command (format "%s %s &"
                           (shell-quote-argument my/sketch-executable)
                           (shell-quote-argument (expand-file-name index-card))))
    (my/rotate-screen 180)
    (my/set-up-sketch-buffer)))

(defun my/show-sketches-as-slideshow (list &optional shuffle)
  "Display a quick slideshow of sketches in LIST.
          If LIST is a string, look up those sketch filenames in my Flickr copy."
  (interactive "MFilter: \nP")
  (apply 'call-process "feh" nil nil nil "-D" "1" "-F" (if shuffle "-z" """") 
         (-filter (lambda (x) (string-match "photostream" x))
                  (if (stringp list)
                      (my/list-sketches list t)
                    list))))

(defvar my/org-index-card-source nil)
(defun my/org-prompt-index-cards ()
  "Display a buffer for easy selection of questions to work on."
  (interactive)
  (find-file "~/personal/questions.org")
  (let ((questions
         (cl-sort (org-map-entries 'org-heading-components "TODO=\"DRAW\"")
                  '< :key (lambda (x) (or (elt x 3) 100)))))
    (setq my/org-index-card-source (current-buffer))
    (my/rotate-screen 180)
    (my/set-up-sketch-buffer)
    (mapc (lambda (q)
            (widget-create 'push-button
                           :notify (lambda (widget &rest ignore)
                                     (my/org-sketch-open
                                      (my/prepare-index-card-template
                                       (widget-value widget)))
                                     (with-current-buffer my/org-index-card-source
                                       (save-excursion
                                         (goto-char (org-find-exact-headline-in-buffer (widget-value widget) my/org-index-card-source t))
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

(defun my/prepare-index-card-for-journal ()
  "Create an index card for my process journal."
  (interactive)
  (quantified-track "Drawing")
  (my/prepare-index-card "Journal"))

(add-to-list 'org-speed-commands-user '("d" call-interactively 'my/prepare-index-card-for-subtree))

(defun my/rename-bank-statements ()
  (interactive)
  (let ((months '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")))
    (cl-loop for i from 1 to 12 do
             (message "%d" i)
             (goto-char (point-min))
             (while (re-search-forward (elt months (1- i)) nil t)
               (ignore-errors
                 (replace-match (format "%02d" i))
                 )))))

(defun my/rename-scanned-receipts ()
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

(defvar my/espeak-command "c:/program files (x86)/espeak/command_line/espeak.exe")
(defun my/say (string &optional speed)
  (interactive "MString: ")
  (setq speed (or speed 175))
  (call-process my/espeak-command nil nil nil string "-s" speed))

(when (eq system-type 'windows-nt)
  (setenv "PATH" (concat "\"c:/program files/postgresql/9.3/bin;\"" (getenv "PATH"))))

(defvar my/unfocusing nil)
(defun my/org-babel-tangle-if-saved-in-focus ()
  (unless my/unfocusing
    (org-babel-tangle)))
