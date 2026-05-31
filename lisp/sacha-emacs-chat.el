;;; sacha-emacs-chat.el ---  -*- lexical-binding: t -*-

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
;; Related Emacs config sections:
;;
;; - Create a Google Calendar event from an Org Mode timestamp
;;   https://sachachua.com/dotemacs#streaming-create-a-google-calendar-event-from-an-org-mode-timestamp
;;
;;; Code:



;; [[file:../Sacha.org::#streaming-create-a-google-calendar-event-from-an-org-mode-timestamp][Create a Google Calendar event from an Org Mode timestamp:2]]
(defvar sacha-time-zone "America/Toronto" "Full name of time zone.")

;;;###autoload
(defun sacha-emacs-chat-schedule (&optional time)
  "Create a Google Calendar invite based on TIME or the Org timestamp at point."
  (interactive (list (sacha-org-time-at-point)))
	(browse-url
	 (format
		"https://calendar.google.com/calendar/render?action=TEMPLATE&text=%s&details=%s&dates=%s&ctz=%s"
		(url-hexify-string sacha-emacs-chat-title)
		(url-hexify-string sacha-emacs-chat-description)
		(format-time-string
		 "%Y%m%dT%H%M%S" time)
		sacha-time-zone)))

(defvar sacha-emacs-chat-title "Emacs Chat" "Title of calendar entry.")
(defvar sacha-emacs-chat-description
	"All right, let's try this! =) See the calendar invite for the Google Meet link.

Objective: Share cool stuff about Emacs workflows that's not obvious from reading configs, and have fun chatting about Emacs

Some ideas for things to talk about:
- Which keyboard shortcuts or combinations of functions work really well for you?
- What's something you love about your setup?
- What are you looking forward to tweaking next?

Let me know if you want to do it on stream (more people can ask questions) or off stream (we can clean up the video in case there are hiccups). Also, please feel free to send me links to things you'd like me to read ahead of time, like your config!"
	"Description.")
;; Create a Google Calendar event from an Org Mode timestamp:2 ends here

(provide 'sacha-emacs-chat)
;;; sacha-emacs-chat.el ends here
