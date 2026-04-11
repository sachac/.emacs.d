;;;###autoload
(defun my-reload-emacs-configuration ()
  (interactive)
  (load-file "~/proj/.emacs.d/Sacha.el"))

;;;###autoload
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

;;;###autoload
(defun my-reset-transients ()
  (interactive)
  (setq overriding-terminal-local-map nil))

;;;###autoload
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

;;;###autoload
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
      (while (re-search-forward "^ *(\\(cl-defun\\|defun\\|define-minor-mode\\|define-derived-mode\\) " nil t)
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

(defvar my-dotemacs-url "https://sachachua.com/dotemacs/")

;;;###autoload
(defun my-dotemacs-link-for-section-at-point (&optional combined)
  "Return the link for the current section."
  (let* ((custom-id (org-entry-get-with-inheritance "CUSTOM_ID"))
         (title (org-entry-get (point) "ITEM"))
         (url (if custom-id
                  (concat "dotemacs:" custom-id)
                (concat my-dotemacs-url ":-:text=" (url-hexify-string title)))))
    (if combined
        (org-link-make-string
         url
         title)
      (cons url title))))

(eval-and-compile
  (require 'org-core nil t)
  (require 'org-macs nil t)
  (require 'org-src nil t))
(declare-function 'org-babel-tangle--compute-targets "ob-tangle")
(defun my-org-collect-links-for-tangled-files ()
  "Return a list of ((filename (link link link link)) ...)."
  (let* ((file (buffer-file-name))
         results)
    (org-babel-map-src-blocks (buffer-file-name)
      (let* ((info (org-babel-get-src-block-info))
             (link (my-dotemacs-link-for-section-at-point)))
        (mapc
         (lambda (target)
           (let ((list (assoc target results #'string=)))
             (if list
                 (cl-pushnew link (cdr list) :test 'equal)
               (push (list target link) results))))
         (org-babel-tangle--compute-targets file info))))
    ;; Put it back in source order
    (nreverse
     (mapcar (lambda (o)
               (cons (car o)
                     (nreverse (cdr o))))
             results))))
(defvar my-emacs-config-module-links nil "Cache for links from tangled files.")

;;;###autoload
(defun my-emacs-config-update-module-info ()
  "Update the list of links."
  (interactive)
  (setq my-emacs-config-module-links
        (seq-filter
         (lambda (o)
           (string-match "my-" (car o)))
         (my-org-collect-links-for-tangled-files)))
  (setq my-emacs-config-modules-info
        (mapcar (lambda (group)
                  `(,(file-name-base (car group))
                    (commentary
                     .
                     ,(replace-regexp-in-string
                       "^"
                       ";; "
                       (concat
                        "Related Emacs config sections:\n\n"
                        (org-export-string-as
                         (mapconcat
                          (lambda (link)
                            (concat "- " (cdr link) "\\\\\n  " (org-link-make-string (car link)) "\n"))
                          (cdr group)
                          "\n")
                         'ascii
                         t))))))
                my-emacs-config-module-links)))

;;;###autoload
(defun my-emacs-config-prepare-to-tangle ()
  "Update module info if tangling my config."
  (when (string-match "Sacha.org" (buffer-file-name))
    (my-emacs-config-update-module-info)))

;;;###autoload
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
