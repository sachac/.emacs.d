;;;###autoload
(defun my-emacsconf-prepare-for-screenshots ()
	(interactive)
	(shell-command "xrandr --output LVDS-1 --mode 1280x720")
	(modus-themes-load-theme 'modus-operandi-tinted)
	(my-hl-sexp-update-overlay)
	(set-face-attribute 'default nil :height 170)
	(keycast-header-line-mode))

;;;###autoload
(defun my-emacsconf-back-to-normal ()
	(interactive)
	(shell-command "xrandr --output LVDS-1 --mode 1366x768")
	(modus-themes-load-theme (car modus-themes-to-toggle))
	(my-hl-sexp-update-overlay)
	(set-face-attribute 'default nil :height 115)
	(keycast-header-line-mode -1))

;;;###autoload
(defun my-emacsconf-search-mail (talk)
	(interactive (list (emacsconf-complete-talk)))
	(emacsconf-with-talk-heading talk
		(notmuch-search (format "from:%s or to:%s" (org-entry-get (point) "EMAIL")
														(org-entry-get (point) "EMAIL")))))
