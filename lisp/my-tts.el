(defun my-speechd-speak-sentence-and-advance ()
  "Speak the current sentence and move forward."
  (interactive)
  (call-interactively #'speechd-speak-read-sentence)
  (forward-sentence))

(defun my-speechd-repeat-sentence ()
  "Speak the current sentence and move forward."
  (interactive)
  (backward-sentence)
  (call-interactively #'speechd-speak-read-sentence)
  (forward-sentence))

(defvar my-espeak-command "c:/program files (x86)/espeak/command_line/espeak.exe")
;;;###autoload
(defun my-say (string &optional speed)
  (interactive "MString: ")
  (setq speed (or speed 175))
  (call-process my-espeak-command nil nil nil string "-s" speed))
