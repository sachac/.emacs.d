;;; sacha-steno.el ---  -*- lexical-binding: t -*-

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



;; [[file:../Sacha.org::#adding-steno-hints-as-i-type][Adding steno hints as I type:1]]
(defvar sacha-steno-hint-dict nil)
(defvar sacha-steno-hint-dictionaries
	'("~/.config/plover/user.json"
		"~/vendor/steno-dictionaries/dictionaries/dict.json"))
(defvar sacha-steno-hint-buffer " *steno hint*")

;;;###autoload
(defun sacha-steno-hint-load-dictionary ()
	(interactive)
	(setq sacha-steno-hint-dict
				(seq-mapcat
				 (lambda (filename)
					 (with-temp-buffer
						 (insert-file-contents filename)
						 (goto-char (point-min))
						 (json-parse-buffer :object-type 'alist)))
				 sacha-steno-hint-dictionaries)))

;;;###autoload
(defun sacha-steno-hint-lookup (search)
	(let ((search-list (list search (downcase search))))
		(seq-group-by
		 'cdr
		 (seq-filter
			(lambda (entry)
				(member (cdr entry) search-list))
			sacha-steno-hint-dict))))

;;;###autoload
(defun sacha-steno-hint-find (&optional buffer)
	"Return a steno hint for the last 1-4 words, if any."
	(setq buffer (or buffer (current-buffer)))
	(when (buffer-live-p buffer)
		(with-current-buffer buffer
			(let ((pos (point)) result hint)
				(save-excursion
					(dotimes (i 4)
						(backward-word)
						(setq result
									(cons
									 (sacha-steno-hint-lookup
										(string-trim (buffer-substring-no-properties (point) pos)))
									 result)))
					(delq nil result))))))

(defvar sacha-steno-hint-display-functions '(sacha-steno-hint-show-posframe))

;;;###autoload
(defun sacha-steno-hint-show-posframe (result &optional command)
	(if (and result (or (null command)
											(member command '(self-insert-command org-self-insert-command))))
			(progn
				(with-current-buffer (get-buffer-create sacha-steno-hint-buffer)
					(erase-buffer)
					(insert
					 (propertize
						(mapconcat
						 (lambda (entries)
							 (mapconcat
								(lambda (entry)
									(concat
									 (car entry) ": "
									 (mapconcat (lambda (stroke)
																(symbol-name (car stroke)))
															(cdr entry) ", ")))
								entries "\n"))
						 result "\n")
						'face 'lispy-face-hint)
					 "\n"
					 (mapconcat 'sacha-steno-hint-propertized-layout
											 (split-string (symbol-name (car (cadar (car result)))) "/")
											 "\n\n")))
				(posframe-show sacha-steno-hint-buffer :position (point) :border-width 1))
		(posframe-hide sacha-steno-hint-buffer)))

(defvar sacha-steno-hint--timer nil)

;;;###autoload
(defun sacha-steno-hint-recent-when-idle ()
	(interactive)
	(when (timerp sacha-steno-hint--timer)
		(cancel-timer sacha-steno-hint--timer))
	(setq sacha-steno-hint--timer
				(run-with-idle-timer 0.1 nil #'sacha-steno-hint-recent (current-buffer) this-command)))

;;;###autoload
(defun sacha-steno-hint-recent (buffer command)
	(interactive)
	(setq sacha-steno-hint--timer nil)
	(run-hook-with-args 'sacha-steno-hint-display-functions (sacha-steno-hint-find buffer) command))

;;;###autoload
(defun sacha-steno-split-keys (s)
	"Return a list of individual steno keys for RTFCRE."
	(when (string-match "\\([STKPWHR]*\\)\\(-\\|\\([AOEU*]+\\)\\)\\([FRPBLGTSDZ]*\\)" s)
		(append
		 (mapcar (lambda (ch) (format "%s-" (char-to-string ch))) (match-string 1 s))
		 (mapcar 'char-to-string (match-string 3 s))
		 (mapcar (lambda (ch) (format "-%s" (char-to-string ch))) (match-string 4 s)))))
;; (sacha-steno-split-keys "HR-")
;; (sacha-steno-split-keys "HRAEUT")
;; (sacha-steno-split-keys "HR*T")

;;;###autoload
(defun sacha-steno-hint-propertized-layout (s)
	(let ((keys (sacha-steno-split-keys s))
				(steno-layout "STPH*FPLTD\nSKWR*RBGSZ\n  AO EU")
				after-mid)
		(mapconcat
		 (lambda (ch)
			 (setq ch (char-to-string ch))
			 (pcase ch
				 ("\n" (setq after-mid nil) "\n")
				 (" " "  ")
				 (_
					(let (found)
						(if (string-match "[AEOU*]" ch)
								(setq after-mid t
											found (member ch keys))
							(setq found
										(member
										 (if after-mid (concat "-" ch)
											 (concat ch "-"))
										 keys)))
						(if found
							(concat (propertize ch 'face '(:inverse-video t)) " ")
							(concat ch " "))))))
		 steno-layout
		 "")))

;;;###autoload
(defun sacha-steno-hint-window-change ()
	(when (posframe-workable-p)
		(unless (string= (buffer-name)
										 sacha-steno-hint-buffer)
			(when (and sacha-steno-hint-buffer
								 (get-buffer sacha-steno-hint-buffer))
				(posframe-hide sacha-steno-hint-buffer)))))

;;;###autoload
(define-minor-mode sacha-steno-hint-minor-mode
	"Show hints for recent words."
	:init-value nil
	:lighter "Hint"
	(if sacha-steno-hint-minor-mode
			(progn
				(unless sacha-steno-hint-dict (sacha-steno-hint-load-dictionary))
				(add-hook 'post-command-hook #'sacha-steno-hint-recent-when-idle nil t)
				(add-hook 'window-configuration-change-hook #'sacha-steno-hint-window-change))

		(remove-hook 'post-command-hook #'sacha-steno-hint-recent-when-idle t)
		(remove-hook 'window-configuration-change-hook #'sacha-steno-hint-window-change)
		(when (timerp sacha-steno-hint--timer)
			(cancel-timer sacha-steno-hint--timer))
		(when (and sacha-steno-hint-buffer
							 (get-buffer sacha-steno-hint-buffer))
			(posframe-delete sacha-steno-hint-buffer))))
;; Adding steno hints as I type:1 ends here

;; [[file:../Sacha.org::#practising-within-emacs][Practising within Emacs:1]]
;;;###autoload
(defun sacha-practise-steno-interleave (base item)
  "Interleave BASE words with item."
  (cons item
        (-interleave base (make-list (length base) item))))
;; Copied from elfeed--shuffle
;;;###autoload
(defun sacha-practise-steno-shuffle (seq)
  "Destructively shuffle SEQ."
  (let ((n (length seq)))
    (prog1 seq
      (dotimes (i n)
        (cl-rotatef (elt seq i) (elt seq (+ i (cl-random (- n i)))))))))
;;;###autoload
(defun sacha-practise-steno-repeat (seq times)
  (funcall 'append (make-list times seq)))
(defface sacha-practise-steno-correct '((t :foreground "green")) "Correct.")
(defface sacha-practise-steno-wrong '((t :foreground "red")) "Wrong.")
(defface sacha-practise-steno-highlight '((t :background "white" :foreground "black")) "Focus.")
(defface sacha-practise-steno-base '((t :height 150)) "Base.")
(defvar sacha-practise-steno-items nil)
(defvar sacha-practise-steno-index 0)
(defvar sacha-practise-steno-buffer-name "*Steno practice*")
(defvar sacha-practise-steno-start-of-input nil)
(defvar sacha-practise-steno-current-overlay nil)
(defvar sacha-practise-steno-previous-overlay nil)
(defvar sacha-practise-steno-highlight-overlay nil)
(defvar sacha-practise-steno-stroke-buffer nil)
(defvar sacha-practise-steno-for-review nil)

;; From https://stackoverflow.com/questions/1249497/command-to-center-screen-horizontally-around-cursor-on-emacs
;;;###autoload
(defun sacha-horizontal-recenter ()
  "Make the point horizontally centered in the window."
  (interactive)
  (let ((mid (/ (window-width) 2))
        (pixel-pos (car (window-absolute-pixel-position)))
        (pixel-mid (/ (window-pixel-width) 2))
        (line-len (save-excursion (end-of-line) (current-column)))
        (cur (current-column)))
    (while (< pixel-mid pixel-pos)
      (set-window-hscroll (selected-window)
                          (1+ (window-hscroll)))
      (setq pixel-pos (car (window-absolute-pixel-position))))))

;;;###autoload
(defun sacha-practise-steno--handle-correct ()
  (if sacha-practise-steno-previous-overlay
      (move-overlay sacha-practise-steno-previous-overlay (overlay-start sacha-practise-steno-previous-overlay)
                    (+ (overlay-end sacha-practise-steno-previous-overlay) (match-end 0)))
    (setq sacha-practise-steno-previous-overlay
          (make-overlay (overlay-end sacha-practise-steno-previous-overlay)
                        (+ (overlay-end sacha-practise-steno-previous-overlay) (match-end 0))))
    (overlay-put sacha-practise-steno-previous-overlay 'evaporate t)
    (overlay-put sacha-practise-steno-previous-overlay 'face 'sacha-practise-steno-correct)))

;;;###autoload
(defun sacha-practise-steno--mark-incorrect-and-fixed ()
  (let ((ov (make-overlay (overlay-end sacha-practise-steno-previous-overlay)
                          (+ (overlay-end sacha-practise-steno-previous-overlay) (match-beginning 0)))))
    (overlay-put ov 'face 'sacha-practise-steno-wrong)
    (overlay-put ov 'evaporate t))
  ;; make a new overlay
  (setq sacha-practise-steno-previous-overlay (copy-overlay sacha-practise-steno-previous-overlay))
  (move-overlay sacha-practise-steno-previous-overlay
                (+ (overlay-end sacha-practise-steno-previous-overlay) (match-beginning 0))
                (+ (overlay-end sacha-practise-steno-previous-overlay) (match-end 0)))
  (setq sacha-practise-steno-for-review (append sacha-practise-steno-for-review (list (elt sacha-practise-steno-items sacha-practise-steno-index))))
  ;; highlight the sample as incorrect, too
  (let ((incorrect-sample (copy-overlay sacha-practise-steno-highlight-overlay)))
    (overlay-put incorrect-sample 'face 'sacha-practise-steno-wrong)
    (save-excursion
      (goto-char (overlay-start sacha-practise-steno-highlight-overlay))
      (insert (make-string
               (+
                (if (bolp) 1 0)
                (match-beginning 0))
               ?\ )))))

;;;###autoload
(defun sacha-practise-steno--move-to-next-item ()
  (setq sacha-practise-steno-stroke-buffer nil)
  (setq sacha-practise-steno-index (1+ sacha-practise-steno-index))
  (move-overlay sacha-practise-steno-current-overlay (overlay-end sacha-practise-steno-previous-overlay) (point))
  (if (elt sacha-practise-steno-items sacha-practise-steno-index)
      (move-overlay sacha-practise-steno-highlight-overlay
                    (1+ (overlay-end sacha-practise-steno-highlight-overlay))
                    (+ (overlay-end sacha-practise-steno-highlight-overlay)
                       1 (length (car (elt sacha-practise-steno-items sacha-practise-steno-index)))))
    (when sacha-practise-steno-for-review
      (goto-char (point-max))
      (kill-new (mapconcat 'car sacha-practise-steno-for-review " "))
      (insert "\nFor review: " (mapconcat 'car sacha-practise-steno-for-review " ")))))

;;;###autoload
(defun sacha-practise-steno--handle-completed-item ()
  ;; extend the feedback overlay to the current point
  (if (= (match-beginning 0) 0)
      (sacha-practise-steno--handle-correct)
    ;; mark incorrect area
    (sacha-practise-steno--mark-incorrect-and-fixed))
  (sacha-practise-steno--move-to-next-item))

;;;###autoload
(defun sacha-practise-steno-check (&rest _)
  (interactive)
  (let* ((sample (car (elt sacha-practise-steno-items sacha-practise-steno-index)))
         (input (and (< (overlay-end sacha-practise-steno-previous-overlay) (point))
                     (buffer-substring-no-properties (overlay-end sacha-practise-steno-previous-overlay) (point)))))
    (when (and sample input)
      (if (string-match (concat " *" (regexp-quote sample) " *") input)
          (sacha-practise-steno--handle-completed-item)
        ;; still in progress
        (move-overlay sacha-practise-steno-current-overlay
                      (overlay-start sacha-practise-steno-current-overlay)
                      (1+ (point))))
      (sacha-horizontal-recenter))))

;;;###autoload
(defun sacha-practise-steno-store-strokes (payload)
  (when (and (plist-get payload :stroked) (string= (buffer-name) sacha-practise-steno-buffer-name))
    (let ((current-item (elt sacha-practise-steno-items sacha-practise-steno-index))
          (rtfcre (plist-get (plist-get payload :stroked) :rtfcre)))
      (save-excursion
        (goto-char (point-max))
        (insert (if (bolp) "" " ") rtfcre))
      (when (and (cadr current-item)
               (> (- (overlay-end sacha-practise-steno-current-overlay)
                     (overlay-start sacha-practise-steno-current-overlay))
                  (length (car current-item))))
        (setq sacha-practise-steno-stroke-buffer (append sacha-practise-steno-stroke-buffer (list rtfcre)))
        (momentary-string-display (format " (%s -> %s)"
                                          (string-join sacha-practise-steno-stroke-buffer " ")
                                          (cadr current-item))
                                  (point)
                                  ?\0
                                  "")))))

;;;###autoload
(defun sacha-practise-steno (items)
  "Display ITEMS for practicing.
ITEMS should be a list like ((word) (word) (word))."
  (interactive (list (let ((table (org-table-to-lisp)))
                       (if table
                           (if current-prefix-arg
                               (subseq table
                                       (1- (org-table-current-line))
                                       (min (length table) (+ (org-table-current-line) current-prefix-arg -1)))
                             table)
                         sacha-practise-steno-items))))
  (with-current-buffer (get-buffer-create sacha-practise-steno-buffer-name)
    (erase-buffer)
    (insert "\n" (mapconcat 'car items " ") "\n")
    (save-excursion (insert "\n\n"))
    (toggle-truncate-lines 1)
    (setq sacha-practise-steno-items items
          sacha-practise-steno-index 0
          sacha-practise-steno-start-of-input (point)
          sacha-practise-steno-for-review nil
          sacha-practise-steno-current-overlay (make-overlay (point) (1+ (point)))
          sacha-practise-steno-previous-overlay (make-overlay (point) (point))
          sacha-practise-steno-stroke-buffer nil
          sacha-practise-steno-highlight-overlay (make-overlay (1+ (point-min)) (+ 1 (point-min) (length (car (car items))))))
    (buffer-face-set "sacha-practise-steno-base")
    (overlay-put sacha-practise-steno-previous-overlay 'face 'sacha-practise-steno-correct)
    (overlay-put sacha-practise-steno-previous-overlay 'evaporate t)
    (overlay-put sacha-practise-steno-highlight-overlay 'face 'sacha-practise-steno-highlight)
    (overlay-put sacha-practise-steno-highlight-overlay 'evaporate t)
    (overlay-put sacha-practise-steno-current-overlay 'modification-hooks '(sacha-practise-steno-check))
    (overlay-put sacha-practise-steno-current-overlay 'insert-in-front-hooks '(sacha-practise-steno-check))
    (overlay-put sacha-practise-steno-current-overlay 'face 'sacha-practise-steno-wrong)
    (overlay-put sacha-practise-steno-current-overlay 'evaporate t)
    ;; (add-hook 'after-change-functions 'sacha-practise-steno-check nil t)
    (add-hook 'plover-websocket-on-message-payload-functions 'sacha-practise-steno-store-strokes)
    (switch-to-buffer (current-buffer))))

;;;###autoload
(defun sacha-practise-steno-word-list (words)
  (interactive (list (mapcar 'list (split-string (read-string "Words: ")))))
  (sacha-practise-steno words))


;; (call-interactively 'sacha-practise-steno)
;; Practising within Emacs:1 ends here

;; [[file:../Sacha.org::#cheat-sheets][Cheat sheets:1]]
;;;###autoload
(defun sacha-steno-quick-help ()
	(interactive)
	(with-selected-window
			(display-buffer-at-bottom
			 (find-file-noselect "~/proj/plover-notes/cheat-sheet.txt")'())
    ;; ... mark it as dedicated to prevent focus from being stolen
    (set-window-dedicated-p (selected-window) t)
    ;; ... and shrink it immediately.
    (fit-window-to-buffer)))

(defhydra sacha-hydra/cheatsheet/plover ()
  "SKHW- symbols -LTZ modifiers TWR- journal phrases
newparSKWRAURBGS bsPW-FP capKPA !space!capTK-LS cap!spaceKPA rmspcTK-FPS*
number: dupeD, revEU, 00/#OD, 00Z, $DZ, timeK- or -BG
`KH-FG  ^KR-RT ~T*LD <AEPBGT =QA*LS >A*EPBGT |PAO*EUP \\_R*UND
-H-N --TK-RB ,KW-BG ;SKWR*RBGS :capSTPH-FPLT :KL-N !SKHRAPL
?H-F /OI .nspP-P ...SKWR-RBGS 'A*E,AE \"KW-GS,KR-GS
(PREN,* [PWR-BGT,* {TPR-BGT,* @KWRAT $TK-PL *STA*R
\\SPWHRAERB \\&SP-PBD #HAERB percPERS +PHR*US
retro KA*PD cap last *UPD cap all HRO*ERD lowered #* star AFPS add space TK-FPS del space
next HRO*ER lower KPA*L cap all
mode SPH-: RL lower R reset T Title -FPLT _RBGS")

(defhydra sacha-hydra/cheatsheet/jade-plover-phrasing ()
  "S: SWR I, KPWR you, KWHR he, SKWHR she, TWH they, TWR we, KPWH it, STKPWHR nothing
M: OE don't (AOE really don't OEU don't really)
AU didn't, E doesn't, O can't, A or U really, AOEU don't even
E: PB know, P want, RPL remember, BL believe, FG forget, R are
BG can, BGD could, BGT can't, BLG like, BLGT like to, BLGTS likes to
BLT believe that, BS said, BT be the, BTS be said to, BTZ say to
D had, F have, FGT forgot, FLG feel like, FLGT felt like, FLT felt
FPLT must, FR ever, FRB wish, FRBT wish to, FS was, FT have to, FTS has to, FZ has, GT get, L will, LG love, PBD need, PBG think, PBL mean,
PLD mind, PLG imagine, PLT might
"
  )

(defhydra sacha-hydra/cheatsheet/emily-symbols ()
  "SKHW+ A (spc before) O (spc after) * (cap)
        v   E         U     EU
FG ws   Tab Backspace Del   Esc
RPBG    Up  Left      Right Down
FPBL    ↑   ←         →     ↓
FRPBG   PgU Home      End   PgD
blank   ''  {*!}      {*?}  spc
FPL     (   [         <     {
RBG     )   ]         >     }
'F *L +G &FBG \"FP #FRLG $RPBL percFRPB
,B -PL .R /RP :LG ;RB =PBLG @FRPBLG \\FB \\^RPG
_BG `P |PB ~FPBG
-S 2x -T 3x -ST 4x"
  )
(defhydra sacha-hydra/cheatsheet/emily-modifiers ()
  "-LTZ F (C-) R (S-) P(s-) B(M-)
Z is STKPW
AO makes SKWR binary 0-9
Symbols with *, AO variants
TR tab delete backspace esc
KPWR up left down right
KPWHR pgup end home pgdown
blank esc tab return spc
TPH ( < [ {
KWR ) > ] }
P `
H '
!HR \"PH #TKHR $KPWH percPWHR &SKP *T +K ,W -TP .R /WH :TK ;WR
=TKPW ?TPW @TKPWHR \\PR ^KPR |PW ~TPWR")
;; Cheat sheets:1 ends here

;; [[file:../Sacha.org::#displaying-frequency-sorted-completions-with-stroke-hints][Displaying frequency-sorted completions with stroke hints:1]]
(defvar sacha-company-strokedict--grep-executable "grep")

;;;###autoload
(defun sacha-company-strokedict--candidates (prefix)
  "Fetches the candidates matching PREFIX."
  (mapcar (lambda (o)
            (let ((data (split-string o "\t")))
              (propertize (car data) 'meta (cadr data))))
          (split-string
           (shell-command-to-string (concat
                                     sacha-company-strokedict--grep-executable
                                     " -i "
                                     (shell-quote-argument (concat "^" prefix))
                                     " "
                                     "~/.config/plover/annotated.txt -m 10"))
           "\n")))

;;;###autoload
(defun sacha-company-strokedict--annotation (candidate)
  (let ((stroke (get-text-property 0 'meta candidate)))
    (if stroke
        (format " (%s)" stroke)
      "")))

;;;###autoload
(defun sacha-company-strokedict (command &optional arg &rest ignored)
  "`company-mode' backend for user-provided dictionaries. Dictionary files are lazy
loaded."
  (interactive (list 'interactive))
  (cl-case command
    (interactive     (company-begin-backend 'sacha-company-strokedict))
    (candidates      (sacha-company-strokedict--candidates arg))
    (prefix  (when-let ((prefix (company-grab-word))) (substring-no-properties prefix)))
    (annotation (sacha-company-strokedict--annotation arg))
    (sorted          t)
    (duplicates      t)
    (no-cache        t)))
;; Displaying frequency-sorted completions with stroke hints:1 ends here

(provide 'sacha-steno)
;;; sacha-steno.el ends here
