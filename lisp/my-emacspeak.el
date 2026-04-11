;;;###autoload
  (defun my-emacspeak ()
    (interactive)
    (load-file "/home/sacha/vendor/emacspeak/lisp/emacspeak-setup.el")
    (setq emacspeak-use-auditory-icons t)
    (setq-default emacspeak-use-auditory-icons t)
    (setq-default dtk-quiet nil)
    (setq dtk-quiet nil))

;;;###autoload
  (defun my-emacspeak-quiet ()
    (interactive)
    (setq emacspeak-use-auditory-icons nil)
    (setq-default emacspeak-use-auditory-icons nil)
    (setq-default dtk-quiet t)
    (setq dtk-quiet t)
		(dtk-interp-sync)
		(ad-disable-regexp "emacspeak"))
