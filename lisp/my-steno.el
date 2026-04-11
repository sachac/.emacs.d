(defvar my-steno-hint-dict nil)
(defvar my-steno-hint-dictionaries
	'("~/.config/plover/user.json"
		"~/vendor/steno-dictionaries/dictionaries/dict.json"))
(defvar my-steno-hint-buffer " *steno hint*")

;;;###autoload
(defun my-steno-hint-load-dictionary ()
	(interactive)
	(setq my-steno-hint-dict
				(seq-mapcat
				 (lambda (filename)
					 (with-temp-buffer
						 (insert-file-contents filename)
						 (goto-char (point-min))
						 (json-parse-buffer :object-type 'alist)))
				 my-steno-hint-dictionaries)))

;;;###autoload
(defun my-steno-hint-lookup (search)
	(let ((search-list (list search (downcase search))))
		(seq-group-by
		 'cdr
		 (seq-filter
			(lambda (entry)
				(member (cdr entry) search-list))
			my-steno-hint-dict))))

;;;###autoload
(defun my-steno-hint-find (&optional buffer)
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
									 (my-steno-hint-lookup
										(string-trim (buffer-substring-no-properties (point) pos)))
									 result)))
					(delq nil result))))))

(defvar my-steno-hint-display-functions '(my-steno-hint-show-posframe))

;;;###autoload
(defun my-steno-hint-show-posframe (result &optional command)
	(if (and result (or (null command)
											(member command '(self-insert-command org-self-insert-command))))
			(progn
				(with-current-buffer (get-buffer-create my-steno-hint-buffer)
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
					 (mapconcat 'my-steno-hint-propertized-layout
											 (split-string (symbol-name (car (cadar (car result)))) "/")
											 "\n\n")))
				(posframe-show my-steno-hint-buffer :position (point) :border-width 1))
		(posframe-hide my-steno-hint-buffer)))

(defvar my-steno-hint--timer nil)

;;;###autoload
(defun my-steno-hint-recent-when-idle ()
	(interactive)
	(when (timerp my-steno-hint--timer)
		(cancel-timer my-steno-hint--timer))
	(setq my-steno-hint--timer
				(run-with-idle-timer 0.1 nil #'my-steno-hint-recent (current-buffer) this-command)))

;;;###autoload
(defun my-steno-hint-recent (buffer command)
	(interactive)
	(setq my-steno-hint--timer nil)
	(run-hook-with-args 'my-steno-hint-display-functions (my-steno-hint-find buffer) command))

;;;###autoload
(defun my-steno-split-keys (s)
	"Return a list of individual steno keys for RTFCRE."
	(when (string-match "\\([STKPWHR]*\\)\\(-\\|\\([AOEU*]+\\)\\)\\([FRPBLGTSDZ]*\\)" s)
		(append
		 (mapcar (lambda (ch) (format "%s-" (char-to-string ch))) (match-string 1 s))
		 (mapcar 'char-to-string (match-string 3 s))
		 (mapcar (lambda (ch) (format "-%s" (char-to-string ch))) (match-string 4 s)))))
;; (my-steno-split-keys "HR-")
;; (my-steno-split-keys "HRAEUT")
;; (my-steno-split-keys "HR*T")

;;;###autoload
(defun my-steno-hint-propertized-layout (s)
	(let ((keys (my-steno-split-keys s))
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
(defun my-steno-hint-window-change ()
	(when (posframe-workable-p)
		(unless (string= (buffer-name)
										 my-steno-hint-buffer)
			(when (and my-steno-hint-buffer
								 (get-buffer my-steno-hint-buffer))
				(posframe-hide my-steno-hint-buffer)))))

;;;###autoload
(define-minor-mode my-steno-hint-minor-mode
	"Show hints for recent words."
	:init-value nil
	:lighter "Hint"
	(if my-steno-hint-minor-mode
			(progn
				(unless my-steno-hint-dict (my-steno-hint-load-dictionary))
				(add-hook 'post-command-hook #'my-steno-hint-recent-when-idle nil t)
				(add-hook 'window-configuration-change-hook #'my-steno-hint-window-change))

		(remove-hook 'post-command-hook #'my-steno-hint-recent-when-idle t)
		(remove-hook 'window-configuration-change-hook #'my-steno-hint-window-change)
		(when (timerp my-steno-hint--timer)
			(cancel-timer my-steno-hint--timer))
		(when (and my-steno-hint-buffer
							 (get-buffer my-steno-hint-buffer))
			(posframe-delete my-steno-hint-buffer))))

;;;###autoload
(defun my-practise-steno-interleave (base item)
  "Interleave BASE words with item."
  (cons item
        (-interleave base (make-list (length base) item))))
;; Copied from elfeed--shuffle
;;;###autoload
(defun my-practise-steno-shuffle (seq)
  "Destructively shuffle SEQ."
  (let ((n (length seq)))
    (prog1 seq
      (dotimes (i n)
        (cl-rotatef (elt seq i) (elt seq (+ i (cl-random (- n i)))))))))
;;;###autoload
(defun my-practise-steno-repeat (seq times)
  (funcall 'append (make-list times seq)))
(defface my-practise-steno-correct '((t :foreground "green")) "Correct.")
(defface my-practise-steno-wrong '((t :foreground "red")) "Wrong.")
(defface my-practise-steno-highlight '((t :background "white" :foreground "black")) "Focus.")
(defface my-practise-steno-base '((t :height 150)) "Base.")
(defvar my-practise-steno-items nil)
(defvar my-practise-steno-index 0)
(defvar my-practise-steno-buffer-name "*Steno practice*")
(defvar my-practise-steno-start-of-input nil)
(defvar my-practise-steno-current-overlay nil)
(defvar my-practise-steno-previous-overlay nil)
(defvar my-practise-steno-highlight-overlay nil)
(defvar my-practise-steno-stroke-buffer nil)
(defvar my-practise-steno-for-review nil)

;; From https://stackoverflow.com/questions/1249497/command-to-center-screen-horizontally-around-cursor-on-emacs
;;;###autoload
(defun my-horizontal-recenter ()
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
(defun my-practise-steno--handle-correct ()
  (if my-practise-steno-previous-overlay
      (move-overlay my-practise-steno-previous-overlay (overlay-start my-practise-steno-previous-overlay)
                    (+ (overlay-end my-practise-steno-previous-overlay) (match-end 0)))
    (setq my-practise-steno-previous-overlay
          (make-overlay (overlay-end my-practise-steno-previous-overlay)
                        (+ (overlay-end my-practise-steno-previous-overlay) (match-end 0))))
    (overlay-put my-practise-steno-previous-overlay 'evaporate t)
    (overlay-put my-practise-steno-previous-overlay 'face 'my-practise-steno-correct)))

;;;###autoload
(defun my-practise-steno--mark-incorrect-and-fixed ()
  (let ((ov (make-overlay (overlay-end my-practise-steno-previous-overlay)
                          (+ (overlay-end my-practise-steno-previous-overlay) (match-beginning 0)))))
    (overlay-put ov 'face 'my-practise-steno-wrong)
    (overlay-put ov 'evaporate t))
  ;; make a new overlay
  (setq my-practise-steno-previous-overlay (copy-overlay my-practise-steno-previous-overlay))
  (move-overlay my-practise-steno-previous-overlay
                (+ (overlay-end my-practise-steno-previous-overlay) (match-beginning 0))
                (+ (overlay-end my-practise-steno-previous-overlay) (match-end 0)))
  (setq my-practise-steno-for-review (append my-practise-steno-for-review (list (elt my-practise-steno-items my-practise-steno-index))))
  ;; highlight the sample as incorrect, too
  (let ((incorrect-sample (copy-overlay my-practise-steno-highlight-overlay)))
    (overlay-put incorrect-sample 'face 'my-practise-steno-wrong)
    (save-excursion
      (goto-char (overlay-start my-practise-steno-highlight-overlay))
      (insert (make-string
               (+
                (if (bolp) 1 0)
                (match-beginning 0))
               ?\ )))))

;;;###autoload
(defun my-practise-steno--move-to-next-item ()
  (setq my-practise-steno-stroke-buffer nil)
  (setq my-practise-steno-index (1+ my-practise-steno-index))
  (move-overlay my-practise-steno-current-overlay (overlay-end my-practise-steno-previous-overlay) (point))
  (if (elt my-practise-steno-items my-practise-steno-index)
      (move-overlay my-practise-steno-highlight-overlay
                    (1+ (overlay-end my-practise-steno-highlight-overlay))
                    (+ (overlay-end my-practise-steno-highlight-overlay)
                       1 (length (car (elt my-practise-steno-items my-practise-steno-index)))))
    (when my-practise-steno-for-review
      (goto-char (point-max))
      (kill-new (mapconcat 'car my-practise-steno-for-review " "))
      (insert "\nFor review: " (mapconcat 'car my-practise-steno-for-review " ")))))

;;;###autoload
(defun my-practise-steno--handle-completed-item ()
  ;; extend the feedback overlay to the current point
  (if (= (match-beginning 0) 0)
      (my-practise-steno--handle-correct)
    ;; mark incorrect area
    (my-practise-steno--mark-incorrect-and-fixed))
  (my-practise-steno--move-to-next-item))

;;;###autoload
(defun my-practise-steno-check (&rest _)
  (interactive)
  (let* ((sample (car (elt my-practise-steno-items my-practise-steno-index)))
         (input (and (< (overlay-end my-practise-steno-previous-overlay) (point))
                     (buffer-substring-no-properties (overlay-end my-practise-steno-previous-overlay) (point)))))
    (when (and sample input)
      (if (string-match (concat " *" (regexp-quote sample) " *") input)
          (my-practise-steno--handle-completed-item)
        ;; still in progress
        (move-overlay my-practise-steno-current-overlay
                      (overlay-start my-practise-steno-current-overlay)
                      (1+ (point))))
      (my-horizontal-recenter))))

;;;###autoload
(defun my-practise-steno-store-strokes (payload)
  (when (and (plist-get payload :stroked) (string= (buffer-name) my-practise-steno-buffer-name))
    (let ((current-item (elt my-practise-steno-items my-practise-steno-index))
          (rtfcre (plist-get (plist-get payload :stroked) :rtfcre)))
      (save-excursion
        (goto-char (point-max))
        (insert (if (bolp) "" " ") rtfcre))
      (when (and (cadr current-item)
               (> (- (overlay-end my-practise-steno-current-overlay)
                     (overlay-start my-practise-steno-current-overlay))
                  (length (car current-item))))
        (setq my-practise-steno-stroke-buffer (append my-practise-steno-stroke-buffer (list rtfcre)))
        (momentary-string-display (format " (%s -> %s)"
                                          (string-join my-practise-steno-stroke-buffer " ")
                                          (cadr current-item))
                                  (point)
                                  ?\0
                                  "")))))

;;;###autoload
(defun my-practise-steno (items)
  "Display ITEMS for practicing.
ITEMS should be a list like ((word) (word) (word))."
  (interactive (list (let ((table (org-table-to-lisp)))
                       (if table
                           (if current-prefix-arg
                               (subseq table
                                       (1- (org-table-current-line))
                                       (min (length table) (+ (org-table-current-line) current-prefix-arg -1)))
                             table)
                         my-practise-steno-items))))
  (with-current-buffer (get-buffer-create my-practise-steno-buffer-name)
    (erase-buffer)
    (insert "\n" (mapconcat 'car items " ") "\n")
    (save-excursion (insert "\n\n"))
    (toggle-truncate-lines 1)
    (setq my-practise-steno-items items
          my-practise-steno-index 0
          my-practise-steno-start-of-input (point)
          my-practise-steno-for-review nil
          my-practise-steno-current-overlay (make-overlay (point) (1+ (point)))
          my-practise-steno-previous-overlay (make-overlay (point) (point))
          my-practise-steno-stroke-buffer nil
          my-practise-steno-highlight-overlay (make-overlay (1+ (point-min)) (+ 1 (point-min) (length (car (car items))))))
    (buffer-face-set "my-practise-steno-base")
    (overlay-put my-practise-steno-previous-overlay 'face 'my-practise-steno-correct)
    (overlay-put my-practise-steno-previous-overlay 'evaporate t)
    (overlay-put my-practise-steno-highlight-overlay 'face 'my-practise-steno-highlight)
    (overlay-put my-practise-steno-highlight-overlay 'evaporate t)
    (overlay-put my-practise-steno-current-overlay 'modification-hooks '(my-practise-steno-check))
    (overlay-put my-practise-steno-current-overlay 'insert-in-front-hooks '(my-practise-steno-check))
    (overlay-put my-practise-steno-current-overlay 'face 'my-practise-steno-wrong)
    (overlay-put my-practise-steno-current-overlay 'evaporate t)
    ;; (add-hook 'after-change-functions 'my-practise-steno-check nil t)
    (add-hook 'plover-websocket-on-message-payload-functions 'my-practise-steno-store-strokes)
    (switch-to-buffer (current-buffer))))

;;;###autoload
(defun my-practise-steno-word-list (words)
  (interactive (list (mapcar 'list (split-string (read-string "Words: ")))))
  (my-practise-steno words))


;; (call-interactively 'my-practise-steno)

;;;###autoload
(defun my-steno-quick-help ()
	(interactive)
	(with-selected-window
			(display-buffer-at-bottom
			 (find-file-noselect "~/proj/plover-notes/cheat-sheet.txt")'())
    ;; ... mark it as dedicated to prevent focus from being stolen
    (set-window-dedicated-p (selected-window) t)
    ;; ... and shrink it immediately.
    (fit-window-to-buffer)))

(defhydra my-hydra/cheatsheet/plover ()
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

(defhydra my-hydra/cheatsheet/jade-plover-phrasing ()
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

(defhydra my-hydra/cheatsheet/emily-symbols ()
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
(defhydra my-hydra/cheatsheet/emily-modifiers ()
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

(defvar my-company-strokedict--grep-executable "grep")

;;;###autoload
(defun my-company-strokedict--candidates (prefix)
  "Fetches the candidates matching PREFIX."
  (mapcar (lambda (o)
            (let ((data (split-string o "\t")))
              (propertize (car data) 'meta (cadr data))))
          (split-string
           (shell-command-to-string (concat
                                     my-company-strokedict--grep-executable
                                     " -i "
                                     (shell-quote-argument (concat "^" prefix))
                                     " "
                                     "~/.config/plover/annotated.txt -m 10"))
           "\n")))

;;;###autoload
(defun my-company-strokedict--annotation (candidate)
  (let ((stroke (get-text-property 0 'meta candidate)))
    (if stroke
        (format " (%s)" stroke)
      "")))

;;;###autoload
(defun my-company-strokedict (command &optional arg &rest ignored)
  "`company-mode' backend for user-provided dictionaries. Dictionary files are lazy
loaded."
  (interactive (list 'interactive))
  (cl-case command
    (interactive     (company-begin-backend 'my-company-strokedict))
    (candidates      (my-company-strokedict--candidates arg))
    (prefix  (when-let ((prefix (company-grab-word))) (substring-no-properties prefix)))
    (annotation (my-company-strokedict--annotation arg))
    (sorted          t)
    (duplicates      t)
    (no-cache        t)))
