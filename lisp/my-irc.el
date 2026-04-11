;;;###autoload
  (defun erc-cmd-OPME ()
    "Request chanserv to op me."
    (erc-message "PRIVMSG"
                 (format "chanserv op %s %s"
                         (erc-default-target)
                         (erc-current-nick)) nil))

;;;###autoload
  (defun erc-cmd-DEOPME ()
    "Deop myself from current channel."
    (erc-cmd-DEOP (format "%s" (erc-current-nick))))
;;;###autoload
  (defun erc-cmd-BAN (nick)
    (let* ((chan (erc-default-target))
           (who (erc-get-server-user nick))
           (host (erc-server-user-host who))
           (user (erc-server-user-login who)))
      (erc-server-send (format "MODE %s +b *!%s@%s" chan user host))))

;;;###autoload
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
;;;###autoload
  (defun my-erc-clean-up ()
    "Clean up dead ERC buffers."
    (interactive)
    (mapc #'kill-buffer (erc-buffer-list (lambda () (null (erc-server-process-alive)))))
    (erc-update-mode-line))

;;;###autoload
(defun my-search-irc-logs ()
  (interactive)
  (let ((default-directory "~/backups/server/home/.znc/users/sachac/moddata/log"))
		(consult-ripgrep default-directory)))
;;;###autoload
(defun my-irc-log-dired ()
  (interactive)
	(dired "~/backups/server/home/.znc/users/sachac/moddata/log"))
