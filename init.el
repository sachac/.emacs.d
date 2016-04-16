;; This sets up the load path so that we can override it
(package-initialize nil)
;; Override the packages with the git version of Org and other packages
(add-to-list 'load-path "~/elisp/org-mode/lisp")
(add-to-list 'load-path "~/elisp/org-mode/contrib/lisp")
(add-to-list 'load-path "~/code/org2blog")
(add-to-list 'load-path "~/Dropbox/2014/presentations/org-reveal")
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp")

;; Prepare stuff for org-export-backends
(setq org-export-backends '(org latex icalendar html ascii))

;; Load the rest of the packages
(package-initialize nil)

(setq package-enable-at-startup nil)
(org-babel-load-file "~/.emacs.d/Sacha.org")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (ob-http peep-dired sauron goto-last-change dired-hacks-utils avy-zap apu org-alert alert flycheck forecast sunshine magit paradox evil-magit pcsv ledger-mode org-gcal elnode yaoddmuse notmuch helm-recoll metaweblog swoop paredit image+ yasnippet web-mode use-package undo-tree switch-window smartparens smart-mode-line skewer-mode rspec-mode robe rinari redshank phi-search-mc org-fstree miniedit mc-extras magit-gh-pulls key-chord jasminejs-mode hydra htmlize helm-projectile helm-descbinds git-messenger expand-region ess erefactor engine-mode emms edit-list company-tern color-theme-solarized coffee-mode bundler auto-compile ace-jump-zap ace-isearch)))
 '(paradox-github-token t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(erc-input-face ((t (:foreground "antique white"))))
 '(helm-selection ((t (:background "ForestGreen" :foreground "black"))))
 '(org-agenda-clocking ((t (:inherit secondary-selection :foreground "black"))))
 '(org-agenda-done ((t (:foreground "dim gray" :strike-through nil))))
 '(org-clock-overlay ((t (:background "SkyBlue4" :foreground "black"))))
 '(org-done ((t (:foreground "PaleGreen" :weight normal :strike-through t))))
 '(org-headline-done ((((class color) (min-colors 16) (background dark)) (:foreground "LightSalmon" :strike-through t))))
 '(outline-1 ((t (:inherit font-lock-function-name-face :foreground "cornflower blue"))))
 '(smerge-refined-added ((t (:inherit smerge-refined-change :background "darkgreen")))))
(put 'narrow-to-region 'disabled nil)
