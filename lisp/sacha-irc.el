;;; sacha-irc.el ---  -*- lexical-binding: t -*-

;; Author: Sacha Chua <sacha@sachachua.com>
;; URL: https://sachachua.com/dotemacs

;;; License:
;;
;; This file is not part of GNU Emacs.
;;
;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;

;;; Code:



;; [[file:../Sacha.org::#internet-relay-chat][Internet Relay Chat:2]]
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
  (defun sacha-erc-clean-up ()
    "Clean up dead ERC buffers."
    (interactive)
    (mapc #'kill-buffer (erc-buffer-list (lambda () (null (erc-server-process-alive)))))
    (erc-update-mode-line))
;; Internet Relay Chat:2 ends here

;; [[file:../Sacha.org::#search-logs][Search logs:1]]
;;;###autoload
(defun sacha-search-irc-logs ()
  (interactive)
  (let ((default-directory "~/backups/server/home/.znc/users/sachac/moddata/log"))
		(consult-ripgrep default-directory)))
;;;###autoload
(defun sacha-irc-log-dired ()
  (interactive)
	(dired "~/backups/server/home/.znc/users/sachac/moddata/log"))
;; Search logs:1 ends here

(provide 'sacha-irc)
;;; sacha-irc.el ends here
