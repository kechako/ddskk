;;; skk-nicola.el -- SKK $B$K?F;X%7%U%HF~NO%$%s%?%U%'%$%9$rDs6!(B
;; Copyright (C) 1996 - 2000 Itsushi Minoura <minoura@eva.hi-ho.ne.jp>

;; Author: Itsushi Minoura <minoura@eva.hi-ho.ne.jp>
;;      Tetsuo Tsukamoto <czkmt@remus.dti.ne.jp>
;; Keywords: hardware, japanese

;; This file is part of Daredevil SKK.

;; Daredevil SKK is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either versions 2, or (at your option) any later
;; version.

;; Daredevil SKK is distributed in the hope that it will be useful but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License along
;; with Daredevil SKK, see the file COPYING.  If not, write to the Free
;; Software Foundation Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Commentary:

;; $B$3$N%W%m%0%i%`$OL'1:0o;K$5$s:n$N(B NICOLA-SKK 0.39 $B$r4p$K!"(B Daredevil SKK $B$K(B
;; $BBP1~$5$;$?$b$N$G$9!#86:n$N%"%$%G%"$K4p$$$F<BAu$7$F$$$/M=Dj$G$9!#(B
;;
;; $B%-!<G[Ns$N%k!<%k$OJL%U%!%$%k$K:YJ,2=$7$F$$$^$9!#$3$l$i$O!"F1$8$/L'1:$5$s:n(B
;; $B$N(B omelet ($B$?$^$4MQ$N?F;X%7%U%HF~NO%$%s%?!<%U%'%$%9(B) $B$*$h$SF1;a$N(B web site
;; $B$NJ8>O$r4p$K$D$/$j$^$7$?!#(B
;;
;; $BF1;a$N%"%$%G%"$H?F;X%7%U%HF~NO$K4X$9$k$4?TNO$K7I0U$rI=$7!"$^$?46<U$$$?$7$^(B
;; $B$9!#(B

;; $B>\:Y$K$D$$$F$O!"F1:-$N(B README.NICOLA.ja $B$r$4Mw2<$5$$!#(B

;;; Code:

(eval-when-compile
  (require 'cl)
  (require 'skk-kanagaki-util)
  (require 'skk-macs)
  (require 'skk-vars)
  (require 'static))

(eval-and-compile
  (require 'skk-kanagaki))

(static-when (eq skk-emacs-type 'mule5)
  (eval-and-compile
    (require 'skk-e21)))

(eval-and-compile
  (autoload 'skk-dcomp-face-off "skk-dcomp")
  (autoload 'skk-dcomp-face-on "skk-dcomp"))

(defgroup skk-nicola nil "SKK NICOLA related customization."
  :prefix "skk-nicola-"
  :group 'skk-custom-by-filename
  :group 'skk-kanagaki)

;; Variables.

(defcustom skk-nicola-interval (if (featurep 'lisp-float-type)
				   (/ (float 1) (float 10))
				 1) "\
*$B$3$N;~4V0JFb$KBG80$5$l$?$b$N$rF1;~BG80$HH=Dj$9$k!#(B
$BC10L$OIC!#%G%U%)%k%H$O(B 0.1 $BIC!#(B"
  :type 'number
  :group 'skk-nicola)

(defcustom skk-nicola-latin-interval (if (featurep 'lisp-float-type)
					 (/ (float 1) (float 10))
				       1) "\
*$B$3$N;~4V0JFb$KBG80$5$l$?$b$N$rF1;~BG80$HH=Dj$9$k!#(B
$BC10L$OIC!#%G%U%)%k%H$O(B 0.1 $BIC!#(B"
  :type 'number
  :group 'skk-nicola)

(defcustom skk-nicola-lshift-keys
  (list (cond
	 ((eq system-type 'windows-nt)
	  [noconvert])
	 ((memq skk-emacs-type '(mule3))
	  ;; Don't know what is good..
	  "\\")
	 (t
	  ;; XEmacs, Emacs 19 or later (except Emacs 20.1 & 20.2)
	  [muhenkan]))) "\
*$B:8?F;X%-!<$H$7$F;H$&%-!<!#(B"
  :type '(repeat sexp)
  :group 'skk-nicola)

(defcustom skk-nicola-rshift-keys
  (if (memq skk-emacs-type '(mule3))
      '(" ")
    (append '(" ")
	    (list (cond
		   ((eq system-type 'windows-nt)
		    [convert])
		   ((eq skk-emacs-type 'xemacs)
		    [henkan-mode])
		   ((string-match "^19\\.\\(29\\|3[0-4]\\)" emacs-version)
		    [numbersign])
		   ((string-match "^19\\.2" emacs-version)
		    ;; Mule 2.3@19.28 or earlier (?)
		    [key-35])
		   (t
		    ;; Emacs 20.3 or later
		    [henkan]))))) "\
*$B1&?F;X%-!<$H$7$F;H$&%-!<!#(B"
  :type '(repeat sexp)
  :group 'skk-nicola)

(defcustom skk-nicola-use-lshift-as-space nil "\
*Non-nil $B$G$"$l$P:8?F;X%-!<$b%9%Z!<%9%-!<$H$7$FMxMQ$9$k!#(B"
  :type 'boolean
  :group 'skk-nicola)

(defcustom skk-nicola-set-henkan-point-chars
  (cond ((memq skk-kanagaki-keyboard-type '(nicola-dvorak omelet-dvorak))
	 '(?u ?h))
	(t
	 '(?f ?j)))
  "\
*$BJQ493+;O0LCV$b$7$/$OAw$j3+;O0LCV$N;XDj$r$9$kJ8;z!#(B
$B$3$l$i$NJ8;z$KEv$?$k%-!<$NF1;~BG80$r8!=P$9$k$H!"(B $B<B9T$5$l$k!#(B"
  :type '(repeat character)
  :group 'skk-nicola)

(defcustom skk-nicola-toggle-kana-chars
  (cond ((memq skk-kanagaki-keyboard-type '(nicola-dvorak omelet-dvorak))
	 '(?e ?t))
	(t
	 '(?d ?k)))
  "\
*$B%+%JJQ49$^$?$O(B $B%+%J(B $B"N(B $B$+$J(B $B@Z$jBX$($r$9$kJ8;z!#(B
$B$3$l$i$NJ8;z$KEv$?$k%-!<$NF1;~BG80$r8!=P$9$k$H!"(B $B<B9T$5$l$k!#(B"
  :type '(repeat character)
  :group 'skk-nicola)

(defcustom skk-nicola-use-koyubi-functions
  (cond ((eq skk-kanagaki-keyboard-type 'oasys)
	 t)
	(t
	 nil))
  "\
*Non-nil $B$J$i(B OASYS $BIw$N(B BS $B%-!<$H<h$j>C$7%-!<$rMQ0U$9$k!#(B
$B$3$l$O!"(BJIS $B%-!<%\!<%I$G$O(B \":\" $B$H(B \"]\" $B$N0LCV$KAjEv$9$k!#(B"
  :type 'boolean
  :group 'skk-nicola)

(defcustom skk-nicola-prefix-suffix-abbrev-chars
  (cond ((memq skk-kanagaki-keyboard-type '(nicola-dvorak omelet-dvorak))
	 '(?i ?d))
	(t
	 '(?g ?h)))
  "\
*$B@\F,!&@\Hx8lF~NO$r$7$?$j!"(B abbrev $B%b!<%I$KF~$kJ8;z!#(B
$B$3$l$i$NJ8;z$KEv$?$k%-!<$NF1;~BG80$r8!=P$9$k$H!"(B $B<B9T$5$l$k!#(B"
  :type '(repeat character)
  :group 'skk-nicola)

(defcustom skk-nicola-okuri-style 'nicola-skk "\
*$BAw$j2>L>$N%9%?%$%k!#(B
`nicola-skk' $B$rA*$V$H!"!V"&$7(B*$B$C(B $B"M(B $B"'CN$C!W$N$h$&$KJQ49$9$k!#(B
`skk' $B$rA*$V$H!"!V"&$7(B*$B$C$F(B $B"M(B $B"'CN$C$F!W$N$h$&$KJQ49$9$k!#(B"
  :type '(choice (const nicola-skk)
		 (const skk))
  :group 'skk-nicola)

(defcustom skk-nicola-help-key "2" "\
* \\[help] $B$K$*$$$F%X%k%W$rI=<($9$k%-!<!#(B"
  :type 'sexp
  :group 'skk-nicola)

(defcustom skk-nicola-2nd-help-key "3" "\
* \\[help] $B$K$*$$$F$b$&$R$H$D$N%X%k%W$rI=<($9$k%-!<!#(B"
  :type 'sexp
  :group 'skk-nicola)

(defcustom skk-nicola-hiragana-mode-string
  (cond ((eq skk-status-indicator 'left) "$B$K$3$i(B:")
	(t " $B$K$3$i(B"))
  "\
*$B$R$i$,$J%b!<%I$N%$%s%8%1!<%?!#(B"
  :type 'string
  :group 'skk-nicola)

(defcustom skk-nicola-katakana-mode-string
  (cond ((eq skk-status-indicator 'left) "$B%K%3%i(B:")
	(t " $B%K%3%i(B"))
  "\
*$B%+%?%+%J%b!<%I$N%$%s%8%1!<%?!#(B"
  :type 'string
  :group 'skk-nicola)

;; Internal constants and variables.

(defconst skk-nicola-hiragana-rom-string skk-hiragana-mode-string)
(defconst skk-nicola-katakana-rom-string skk-katakana-mode-string)

(defvar skk-nicola-plain-rule nil)
(defvar skk-nicola-lshift-rule nil)
(defvar skk-nicola-rshift-rule nil)

(defvar skk-nicola-temp-data nil)

(skk-deflocalvar skk-nicola-okuri-flag nil)

(static-when (memq skk-emacs-type '(mule3))
  (case skk-kanagaki-jidou-keymap-kakikae-service
    ;;
    (nicola-jis
     ;; Emacs 20.2 $B$G$O(B Muhenkan, Henkan $B$O;H$($J$$$h$&$J$N$G!";H$($k%-!<$K=q(B
     ;; $B49$($k!#(B
     (skk-kanagaki-call-xmodmap
	 (case skk-emacs-type
	   (mule3
	    "keycode 129 = F19 Mode_switch
keycode 131 = F18\n")
	   (t
	    "keycode 129 = space Mode_switch
keycode 131 = underscore\n"))
       (case skk-emacs-type
	 (mule3
	  (setq skk-nicola-lshift-keys (nconc skk-nicola-lshift-keys
					      '([f18]))
		skk-nicola-rshift-keys (nconc skk-nicola-rshift-keys
					      '([f19]))))
	 (t
	  (setq skk-nicola-lshift-keys (nconc skk-nicola-lshift-keys
					      '("_"))
		skk-nicola-rshift-keys (nconc skk-nicola-rshift-keys
					      '(" ")))))))))

;; Shut up compiler.
(defvar skktut-j-mode-map)
(defvar skktut-latin-mode-map)

;; Hooks.

(add-hook 'skk-mode-hook 'skk-nicola-setup)

(add-hook
 'skk-mode-hook
 (function
  (lambda ()
    ;;
    (case skk-kanagaki-state
      (kana
       (setq skk-hiragana-mode-string skk-nicola-hiragana-mode-string
	     skk-katakana-mode-string skk-nicola-katakana-mode-string))
      (rom
       (setq skk-hiragana-mode-string skk-nicola-hiragana-rom-string
	     skk-katakana-mode-string skk-nicola-katakana-rom-string)))
    ;;
    (skk-modify-indicator-alist 'katakana skk-katakana-mode-string)
    (skk-modify-indicator-alist 'hiragana skk-hiragana-mode-string)
    ;;
    (skk-update-modeline (if skk-katakana
			     'katakana
			   'hiragana)))))

;; Functions.

(defun skk-nicola-setup ()
  ;; SKK $B$N=i2s5/F0;~$N$_<B9T$5$l$k$Y$-$b$N$O$3$N4X?t$KF~$l$k!#(B
  (dolist (key skk-nicola-lshift-keys)
    (define-key skk-j-mode-map key 'skk-nicola-self-insert-lshift)
    (define-key skk-latin-mode-map key 'skk-nicola-turn-on-j-mode))
  (dolist (key skk-nicola-rshift-keys)
    (define-key skk-j-mode-map key 'skk-nicola-self-insert-rshift)
    (define-key skk-latin-mode-map key 'skk-nicola-turn-on-j-mode))
  ;;
  (when skk-nicola-help-key
    (define-key help-map skk-nicola-help-key 'skk-nicola-help))
  (when skk-nicola-2nd-help-key
    (define-key help-map skk-nicola-2nd-help-key 'skk-nicola-2nd-help))
  ;;
  (unless skk-nicola-plain-rule
    (setq skk-nicola-plain-rule
	  (symbol-value
	   (intern
	    (format "skk-%s-plain-rule-list" skk-kanagaki-keyboard-type)))))
  (unless skk-nicola-lshift-rule
    (setq skk-nicola-lshift-rule
	  (symbol-value
	   (intern
	    (format "skk-%s-lshift-rule-list" skk-kanagaki-keyboard-type)))))
  (unless skk-nicola-rshift-rule
    (setq skk-nicola-rshift-rule
	  (symbol-value
	   (intern
	    (format "skk-%s-rshift-rule-list" skk-kanagaki-keyboard-type)))))
  ;;
  (remove-hook 'skk-mode-hook 'skk-nicola-setup))

(defun skk-nicola-setup-tutorial ()
  (dolist (key skk-nicola-lshift-keys)
    (define-key skktut-j-mode-map key 'skk-nicola-self-insert-lshift)
    (define-key skktut-latin-mode-map key 'skk-nicola-turn-on-j-mode))
  (dolist (key skk-nicola-rshift-keys)
    (define-key skktut-j-mode-map key 'skk-nicola-self-insert-rshift)
    (define-key skktut-latin-mode-map key 'skk-nicola-turn-on-j-mode)))

;;;###autoload
(defun skk-nicola-help (&optional arg)
  ;; $B%-!<G[Ns$rI=<($9$k!#(B
  (interactive "p")
  (describe-variable
   (intern (format "skk-%s-keymap-display" skk-kanagaki-keyboard-type))))

;;;###autoload
(defun skk-nicola-2nd-help ()
  ;; skk-nicola.el $BFH<+$N%-!<Dj5A0lMw$rI=<($9$k!#(B
  (interactive)
  (skk-kanagaki-help-1
   "* SKK $B?F;X%7%U%HF~NO(B $B%X%k%W(B*"
   "$B?F;X%7%U%HF~NO%b!<%I$NFH<+%-!<Dj5A(B:"
   (nconc
    ;;
    (mapcar (function
	     (lambda (key)
	       (cons (key-description key) "$B:8?F;X%7%U%H%-!<(B")))
	    skk-nicola-lshift-keys)
    ;;
    (mapcar (function
	     (lambda (key)
	       (cons (key-description key) "$B1&?F;X%7%U%H%-!<(B")))
	    skk-nicola-rshift-keys)
    ;;
    (list (cons "SPC" "$BAw$j$J$7JQ493+;O(B"))
    ;;
    (list
     (do ((spec (nth 4 skk-kanagaki-rule-tree) (cdr spec))
	  (list nil (car spec))
	  (str nil (when (memq
			  (nth 3 list)
			  '(skk-input-by-code-or-menu))
		     (nth 1 list))))
	 ((or str (null spec))
	  (when (stringp str)
	    (cons str "$B%3!<%I$^$?$O%a%K%e!<$K$h$kF~NO(B")))))
    ;;
    (list
     (do ((spec (nth 4 skk-kanagaki-rule-tree) (cdr spec))
	  (list nil (car spec))
	  (str nil (when (memq
			  (nth 3 list)
			  '(skk-today))
		     (nth 1 list))))
	 ((or str (null spec))
	  (when (stringp str)
	    (cons str "$B:#F|$NF|IU$1$rA^F~(B")))))
    ;;
    (list
     (do ((spec (nth 4 skk-kanagaki-rule-tree) (cdr spec))
	  (list nil (car spec))
	  (str nil (when (memq
			  (nth 3 list)
			  '(skk-jisx0208-latin-mode))
		     (nth 1 list))))
	 ((or str (null spec))
	  (when (stringp str)
	    (cons str "$BA41Q%b!<%I(B")))))
    ;;
    (list
     (cons (format "%c + %c"
		   (car skk-nicola-set-henkan-point-chars)
		   (cadr skk-nicola-set-henkan-point-chars))
	   "$BJQ493+;OE@$r%;%C%H!"Aw$j3+;OE@;XDj(B")
     (cons (format "%c + %c"
		   (car skk-nicola-prefix-suffix-abbrev-chars)
		   (cadr skk-nicola-prefix-suffix-abbrev-chars))
	   "$B@\F,<-(B or $B@\Hx<-JQ49(B ($B"&%b!<%I(B or $B"'%b!<%I(B)$B!"(Babbrev $B%b!<%I(B")
     (cons (format "%c + %c"
		   (car skk-nicola-toggle-kana-chars)
		   (cadr skk-nicola-toggle-kana-chars))
	   "$B%+%J%b!<%I(B or $B%+%JJQ49(B")
     (cons "$B:8?F;X%7%U%H(B + $B1&?F;X%7%U%H(B" "latin $B%b!<%I(B $B"N(B $B$+$J%b!<%I@Z$jBX$((B")
     (cons (format "M-x help %s" skk-nicola-help-key)
	   "$B8=:_$NF~NOJ}<0$N%-!<G[Ns$rI=<((B")
     (cons (format "M-x help %s" skk-nicola-2nd-help-key)
	   "$B$3$N%X%k%W$rI=<((B")))))

;;;###autoload
(defun skk-nicola-self-insert-rshift (&optional arg)
  "$B1&%7%U%H$K3d$jIU$1$k4X?t!#(B"
  (interactive "p")
  (skk-nicola-self-insert-lshift arg))

;;;###autoload
(defun skk-nicola-self-insert-lshift (&optional arg)
  "$B:8%7%U%H$K3d$jIU$1$k4X?t!#(B"
  (interactive "p")
  ;;
  (when (or (and (markerp skk-nicola-okuri-flag)
		 (<= (point) (marker-position skk-nicola-okuri-flag)))
	    (or (not skk-henkan-on) skk-henkan-active))
    (setq skk-nicola-okuri-flag nil))
  ;;
  (cond
   ((and (eq skk-kanagaki-state 'kana)
	 (not skk-jisx0201-mode))
    (skk-nicola-insert arg))
   (t
    (unless last-command-char
      (setq last-command-char ?\ ))
    (call-interactively 'skk-insert))))

;;;###autoload
(defun skk-nicola-turn-on-j-mode (&optional arg)
  ;; `skk-latin-mode' $B$K$*$$$F!"(B $B:81&?F;X%-!<$NF1;~BG80$K$h$C$F(B 'skk-j-mode'
  ;; $B$KF~$k!#(B
  (interactive "*p")
  (if (skk-sit-for skk-nicola-latin-interval t)
      ;; then
      (let ((last-command-char
	     (if (characterp (event-to-character last-command-event))
		 (event-to-character last-command-event)
	       ?\ )))
	(call-interactively 'self-insert-command t))
    ;; else
    (let ((last (static-cond
		 ((eq skk-emacs-type 'xemacs)
		  (event-key last-command-event))
		 (t last-command-event)))
	  (next (static-cond
		 ((eq skk-emacs-type 'xemacs)
		  (event-key (next-command-event)))
		 (t (next-command-event))))
	  char)
      (if (eq last next)
	  ;; then
	  (let ((last-command-char
		 (if (characterp (event-to-character last-command-event))
		     (event-to-character last-command-event)
		   ?\ )))
	    (call-interactively 'self-insert-command t)
	    (call-interactively 'self-insert-command t))
	;; else
	(when (characterp next)
	  (setq char next)
	  (setq next (key-description (char-to-string char))))
	(when (eq next 'space)
	  (setq next (key-description " ")))
	(when (symbolp next)
	  (setq next (key-description (vector next))))
	;;
	(unless (stringp next)
	  (setq next (format "%s" next)))
	;;
	(cond ((member next
		       (mapcar (function
				(lambda (key)
				  (key-description key)))
			       (append
				skk-nicola-rshift-keys
				skk-nicola-lshift-keys)))
	       (skk-j-mode-on)
	       (when (and skk-use-color-cursor (skk-color-display-p))
		 ;; $B?7$7$$(B skk-cursor $BBP:v(B
		 (static-cond
		  ((eq skk-emacs-type 'xemacs)
		   (set-face-property
		    'text-cursor 'background skk-cursor-hiragana-color
		    (current-buffer)))
		  (t
		   (set-buffer-local-cursor-color
		    skk-cursor-hiragana-color)))))
	      (char
	       (let ((last-command-char
		      (if (characterp (event-to-character last-command-event))
			  (event-to-character last-command-event)
			?\ )))
		 (call-interactively 'self-insert-command t))
	       (let ((last-command-char char))
		 (call-interactively 'self-insert-command t))))))))

;;;###autoload
(defun skk-nicola-insert (&optional arg)
  ;; $BF1;~BG80$rG'<1$7$F!"(BNICOLA $B$+$JF~NO$r$9$k!#(B
  (interactive "*p")
  (let (time1 time2 next-event next)
    ;;
    (setq time1 (skk-nicola-format-time (current-time)))
    ;;
    (unless (and skk-henkan-on (not skk-henkan-active))
      (setq skk-nicola-okuri-flag nil))
    ;;
    (cond ((skk-sit-for skk-nicola-interval t)
	   ;; No input in the interval.
	   (skk-nicola-insert-single this-command arg))
	  (t
	   ;; Some key's pressed.
	   (setq time2 (skk-nicola-format-time (current-time)))
	   ;;
	   (setq next-event (next-command-event)
		 next (skk-nicola-event-to-key next-event))
	   (cond
	    ((skk-nicola-maybe-double-p this-command next)
	     (skk-nicola-treat-triple this-command next time1 time2 arg))
	    (t
	     ;; $B:G=i$NF~NO$OC1FHBG80$G$7$+$"$j$($J$$$H3NDj!#(B
	     (skk-nicola-insert-single this-command arg)
	     (skk-unread-event next-event)))))
    ;; $BE}7WE*2ACM$,$"$k$+$J(B...$B!)(B
;    (setq skk-nicola-temp-data
;	  (cons
;	   (list (or last-command-char this-command)
;		 period1
;		 next
;		 period2
;		 third)
;	   skk-nicola-temp-data))
    )
  ;; `skk-kana-input' $B$,2?$bF~NO$7$J$$$h$&$K!"(Bnil $B$rJV$7$F$*$/!#(B
  nil)

(defun skk-nicola-format-time (time)
  (let ((time1 (* (float 100000) (car time)))
	(time2 (cadr time))
	(time3 (/ (caddr time) (float 1000000))))
    (+ time1 time2 time3)))

(defun skk-nicola-event-to-key (event)
  (static-cond
   ((eq skk-emacs-type 'xemacs)
    (let ((char (event-to-character event)))
      (cond ((characterp char) char)
	    (t (event-key event)))))
   (t
    (if (symbolp event)
	(vector event)
      event))))

;;; $B!A(B NICOLA $B5,3J=q$h$j(B $B!A(B
;;; 7.4.2$B!!BG80=g=x$@$1$G$O7hDj$G$-$J$$F1;~BG80(B
;;;
;;;        $BJ8;z%-!<(Ba$B!"?F;X%-!<(Bs$B!"J8;z%-!<(Bb$B$N#3$D$N%-!<$,!"H=Dj;~4V0JFb$N4V(B
;;;        $B3V$G=EJ#$7$F2!$5$l$?>l9g$O!"Cf1{$K64$^$l$??F;X%-!<(Bs$B$,J8;z%-!<(Ba$B$r(B
;;;        $B=$>~$9$k$b$N$+!"J8;z%-!<(Bb$B$r=$>~$9$k$b$N$+$r7hDj$7$J$1$l$P$J$i$J(B
;;;        $B$$!#!J?^#6!K(B
;;;
;;;        $B4pK\E*$K$O!"2!2<;~9o$,!"$h$j?F;X%-!<$K6a$$J8;z%-!<$H$N4V$KF1;~BG(B
;;;        $B80$,@.N)$9$k$HH=CG$9$k!#(B
;;;
;;;              $B?^(B6$B!!!!!!!VJ8;z%-!<(BON$B"*?F;X%-!<(BON$B"*J8;z%-!<(BON$B!W$NNc(B
;;;
;;;              $B!!!!J8;z%-!<(Ba $B!!!!!!!!!!!!!!(B|$B!1!1!1(B|
;;;              $B!!!!!!!!!!!!!!!!!!!!!D!D!D!D!!!!!!!!!D!D!D!D!D!D!D!D(B
;;;
;;;              $B!!!!?F;X%-!<(Bs $B!!!!!!!!!!!!!!!!!!(B|$B!1!1!1(B|
;;;              $B!!!!!!!!!!!!!!!!!!!!!D!D!D!D!D!D!!!!!!!!!D!D!D!D!D!D(B
;;;
;;;              $B!!!!J8;z%-!<(Bb $B!!!!!!!!!!!!!!!!!!!!!!!!(B|$B!1!1!1(B|
;;;              $B!!!!!!!!!!!!!!!!!!!!!D!D!D!D!D!D!D!D!D!!!!!!!!!D!D!D(B
;;;
;;;             $B!!!!!!!!!!!!!!!!!!!!!!!!!!!!(B|-t1-|-t2-|
;;;                                         (t1$B!"(Bt2$B$O6&$KH=Dj;~4V0JFb(B)
;;;
;;;       t1=t2$B$J$i$P!"J8;z%-!<(Ba$B$H?F;X%-!<(Bs$B$,F1;~BG80!"J8;z%-!<(Bb$B$OC1FHBG80!#(B
(defun skk-nicola-treat-triple (first next time1 time2 arg)
  (let ((period1 (- time2 time1))
	time3 period2 str third-event third)
  (cond
   ((skk-sit-for period1 t)
    ;; $BF1;~BG80$H3NDj!#(B(< t1 t2)
    (skk-nicola-insert-double first next arg))
   (t
    ;; $B99$K<!$N(B event $B$rD4$Y$k!#(B
    (setq period2 (- (setq time3 (skk-nicola-format-time (current-time)))
		     time2)
	  str (if (characterp next) (char-to-string next))
	  third-event (next-command-event)
	  third (skk-nicola-event-to-key third-event))
    (cond
     ((and
       (skk-nicola-maybe-double-p next third)
       ;; ($BMW$i$J$$$+$bCN$i$J$$$,!"B?>/(B `sit-for' $B$NJV$C$F$/$k;~(B
       ;; $B4V$H(B `current-time' $B$,JV$9;~4V$H$N4V$K%:%l$,@8$8$k$3$H(B
       ;; $B$b$"$k$N$G!"0l1~Hf3S$7$F$*$/(B)
       (> period1 period2))
      ;; $BA0$N(B2$BBG80$OF1;~BG80$G$O$J$$$H3NDj!#(B
      ;; $B8e$N(B2$BBG80$,F1;~BG80$+$I$&$+$O!"99$K<!$NF~NO$rD4$Y$J$$$H(B
      ;; $B3NDj$7$J$$!#(B
      (skk-nicola-insert-single this-command arg)
      (skk-nicola-treat-triple
       (lookup-key skk-j-mode-map (or str next)) third time2 time3 arg))
     (t
      ;; $BA0$N(B2$BBG80$,F1;~BG80$H3NDj!#(B
      (skk-nicola-insert-double this-command next arg)
      (skk-unread-event third-event)))))))

(defun skk-nicola-insert-single (command arg)
  (let ((char last-command-char))
    (case command
      (skk-nicola-self-insert-rshift
       ;; ($BJQ49!&%9%Z!<%9(B)
       (skk-nicola-space-function arg))
      (skk-nicola-self-insert-lshift
       ;; $B:8%7%U%H(B
       (skk-nicola-lshift-function arg))
      (t
       ;; $BJ8;z(B
       (skk-nicola-insert-kana char skk-nicola-plain-rule arg)))))

(defun skk-nicola-insert-double (first next arg)
  (let ((command (cond
		  ((commandp first)
		   first)
		  ((characterp first)
		   (lookup-key skk-j-mode-map (char-to-string first)))
		  (t
		   (lookup-key skk-j-mode-map first))))
	(char (if (characterp first) first last-command-char))
	(str (if (characterp next) (char-to-string next))))
    ;;
    (case (lookup-key skk-j-mode-map (or str next))
      (skk-nicola-self-insert-rshift
       ;; $B1&%7%U%H(B
       (case command
	 (skk-nicola-self-insert-rshift
	  ;; [$B1&(B $B1&(B]
	  (let ((last-command-char ?\ ))
	    (cond ((or skk-henkan-on skk-henkan-active)
		   (skk-kanagaki-insert arg)
		   (unless (>= skk-nicola-interval 1)
		     ;; $BC1FHBG80$rF10l%-!<O"B3BG80$GBeMQ$G$-$k$h$&$K!#(B
		     (skk-kanagaki-insert arg)))
		  (t
		   (self-insert-command
		    (if (>= skk-nicola-interval 1)
			;; $BC1FHBG80$rF10l%-!<O"B3BG80$GBeMQ$G$-$k$h$&$K!#(B
			arg
		      (1+ arg)))))))
	 (skk-nicola-self-insert-lshift
	  ;; [$B:8(B $B1&(B]
	  (cond ((and skk-j-mode (not skk-katakana))
		 (skk-latin-mode 1))
		(t
		 (skk-toggle-kana 1))))
	 (t
	  ;; [$BJ8;z(B $B1&(B]
	  (skk-nicola-insert-kana char
				  skk-nicola-rshift-rule arg))))
      (skk-nicola-self-insert-lshift
       ;; $B:8%7%U%H(B
       (case command
	 (skk-nicola-self-insert-lshift
	  ;;[$B:8(B $B:8(B]
	  (cond ((skk-in-minibuffer-p)
		 (exit-minibuffer))
		(t
		 (skk-nicola-lshift-function arg)
		 (unless (>= skk-nicola-interval 1)
		   ;; $BC1FHBG80$rF10l%-!<O"B3BG80$GBeMQ$G$-$k$h$&$K!#(B
		   (skk-nicola-lshift-function 1)))))
	 (skk-nicola-self-insert-rshift
	  ;; [$B1&(B $B:8(B]
	  (if (and skk-j-mode (not skk-katakana))
	      (skk-latin-mode 1)
	    (skk-toggle-kana 1)))
	 (t
	  ;; [$BJ8;z(B $B:8(B]
	  (skk-nicola-insert-kana char
				  skk-nicola-lshift-rule arg))))
      (t
       ;; $BJ8;z(B
       (cond
	((eq command 'skk-nicola-self-insert-rshift)
	 ;;  [$B1&(B $BJ8;z(B]
	 (skk-nicola-insert-kana next skk-nicola-rshift-rule arg))
	((eq command 'skk-nicola-self-insert-lshift)
	 ;; [$B:8(B $BJ8;z(B]
	 (skk-nicola-insert-kana next skk-nicola-lshift-rule arg))
	((and
	  (not (eq char next))
	  (memq last-command-char skk-nicola-set-henkan-point-chars)
	  (memq next skk-nicola-set-henkan-point-chars))
	 ;; [fj]
	 (cond ((and skk-henkan-on (not skk-henkan-active))
		(if (memq skk-kanagaki-keyboard-type '(106-jis))
		    (skk-kanagaki-set-okurigana-no-sokuon arg)
		  (skk-nicola-set-okuri-flag)))
	       (t
		(skk-set-henkan-point-subr 1))))
	((and
	  (not (eq char next))
	  (memq char skk-nicola-prefix-suffix-abbrev-chars)
	  (memq next skk-nicola-prefix-suffix-abbrev-chars))
	 ;; [gh] suffix $B$N(B $BF~NO(B
	 (cond (skk-henkan-active
		;; $B@\Hx8l$N=hM}(B
		(skk-kakutei)
		(skk-set-henkan-point-subr)
		(insert-and-inherit ?>))
	       (skk-henkan-on
		;; $B@\F,8l$N=hM}(B
		(skk-kana-cleanup 'force)
		(insert-and-inherit ?>)
		(skk-set-marker skk-henkan-end-point (point))
		(setq skk-henkan-count 0
		      skk-henkan-key (buffer-substring-no-properties
				      skk-henkan-start-point (point))
		      skk-prefix "")
		(skk-henkan))
	       (t
		;;
		(skk-abbrev-mode 1))))
	((and
	  (not (eq char next))
	  (memq char skk-nicola-toggle-kana-chars)
	  (memq next skk-nicola-toggle-kana-chars))
	 ;; [dk]
	 (skk-toggle-kana 1))
	(t
	 ;; [$BJ8;z(B $BJ8;z(B]
	 (let ((str (skk-nicola-insert-kana
		     char skk-nicola-plain-rule arg)))
	   (when (and skk-isearch-switch
		      (not (or skk-henkan-on skk-henkan-active)))
	     (setq isearch-cmds
		   (cons
		    (nconc
		     (list (concat (caar isearch-cmds) str)
			   (concat (cadar isearch-cmds) str))
		     (cddar isearch-cmds))
		    isearch-cmds))))
	 (unless (and (>= skk-nicola-interval 1)
		      (eq next char))
	   ;; $BC1FHBG80$rF10l%-!<O"B3BG80$GBeMQ$G$-$k$h$&$K!#(B
	   (skk-nicola-insert-kana next skk-nicola-plain-rule))))))))

(defun skk-nicola-maybe-double-p (first next)
  (let ((command (cond
		  ((commandp first)
		   first)
		  ((characterp first)
		   (lookup-key skk-j-mode-map (char-to-string first)))
		  (t
		   (lookup-key skk-j-mode-map first))))
	(char (if (characterp first) first last-command-char))
	(str (if (characterp next) (char-to-string next)))
	(shifts '(skk-nicola-self-insert-lshift
		  skk-nicola-self-insert-rshift)))
  (or
   ;; * $B$I$A$i$+0lJ}$,?F;X(B
   (or (memq command shifts)
       (memq (lookup-key skk-j-mode-map (or str next)) shifts))
   ;; * skk-nicola $B$K1w$1$kFC<lF1;~BG80%-!<(B
   (and (not (eq char next))
	(or
	 ;; [fj]
	 (and (memq last-command-char skk-nicola-set-henkan-point-chars)
	      (memq next skk-nicola-set-henkan-point-chars))
	 ;; [gh]
	 (and (memq char skk-nicola-prefix-suffix-abbrev-chars)
	      (memq next skk-nicola-prefix-suffix-abbrev-chars))
	 ;; [dk]
	 (and  (memq char skk-nicola-toggle-kana-chars)
	       (memq next skk-nicola-toggle-kana-chars)))))))

(defun skk-nicola-insert-kana (char rule &optional arg)
  ;; CHAR $B$r(B RULE $B$NCf$+$iC5$7$FF~NO$9$Y$-J8;zNs$r7hDj$9$k!#(B
  ;; ARG $B$rM?$($i$l$?>l9g$O$=$N?t$@$1J8;zNs$rO"7k$7$FF~NO$9$k!#(B
  (let* ((el (cadr (assq char rule)))
	 (str (when el (cond ((stringp el) el)
			     ((not (listp el)) nil)
			     (skk-katakana (car el))
			     (t (cdr el)))))
	 (fun (when (and el (symbolp el)) el))
	 (arg (prefix-numeric-value arg)))
    ;;
    (when str
      (if (symbolp str)
	  (setq fun str
		str nil)
	(skk-insert-str (setq str (skk-kanagaki-make-string arg str)))))
    ;;
    (when fun
      (funcall fun arg))
    ;;
    (cond (skk-nicola-okuri-flag
	   (skk-nicola-process-okuri))
	  (t
	   ;;
	   (when skk-henkan-active (skk-kakutei))))
    ;; $B2?$+$K;H$&$3$H$,$"$k$+$b$7$l$J$$$N$G!"(BSTR $B$rJV$7$F$*$/!#(B
    str))

(defun skk-nicola-process-okuri ()
  ;; $BAw$j3+;O$NI8<1$K$h$jAw$j3+;OE@$rG'<1$7!"Aw$j$"$jJQ49$r3+;O$9$k!#(B
  (let ((okuri (buffer-substring-no-properties
		(1+ skk-nicola-okuri-flag) (point)))
	tag)
    (unless (and (not (eq skk-nicola-okuri-style 'nicola-skk))
		 (member okuri '("$B$C(B" "$B%C(B")))
      (save-excursion
	(goto-char skk-nicola-okuri-flag)
	(when (eq (following-char) ?*)
	  (delete-char 1))
	(backward-char 1)
	(if (member
	     (buffer-substring-no-properties
	      (point) (marker-position skk-nicola-okuri-flag))
	     '("$B$C(B" "$B%C(B"))
	    (setq tag 'no-sokuon)))
      (skk-kanagaki-set-okurigana tag))))

(defun skk-nicola-set-okuri-flag ()
  ;; $BAw$j3+;OE@$r(B marker $B$GI8<1$9$k$H$H$b$K!"(B`*' $B$rA^F~$9$k$3$H$GAw$j$"$jJQ49(B
  ;; $B$NBT$A>uBV$G$"$k$3$H$rL@<($9$k!#(B
  (interactive)
  (when (and skk-henkan-on (not skk-henkan-active))
    ;; $B"&%b!<%I$N$H$-$@$15!G=$9$k!#(B
    (let ((pt (point)))
      (unless (and (string= "*" (buffer-substring-no-properties (1- pt) pt))
		   (markerp skk-nicola-okuri-flag))
	;; $B4{$KI8<1:Q$_$J$i2?$b$7$J$$!#(B
	(skk-set-marker skk-nicola-okuri-flag pt)
	(insert-and-inherit "*")))))

(defun skk-nicola-space-function (&optional arg)
  (let ((last-command-char ?\ ))
    (cond
     (skk-henkan-active
      (call-interactively 'skk-insert))
     (skk-henkan-on
      (skk-kanagaki-insert arg))
     (t
      (self-insert-command arg)))))

(defun skk-nicola-lshift-function (&optional arg)
  (cond ((or skk-henkan-active skk-henkan-on)
	 ;; $B3NDj$K;H$&!#(B
	 (skk-kakutei))
	(skk-nicola-use-lshift-as-space
	 ;;
	 (skk-nicola-space-function arg))
	(t
	 ;; $B2~9T$K;H$&!#(B
	 (if (skk-in-minibuffer-p)
	     (exit-minibuffer)
	   (newline arg)))))

;; Pieces of Advice.

(defadvice skk-insert (before skk-nicola-ad activate compile)
  (when (or (and (markerp skk-nicola-okuri-flag)
		 (<= (point) (marker-position skk-nicola-okuri-flag)))
	    (or (not skk-henkan-on) skk-henkan-active))
    (setq skk-nicola-okuri-flag nil)))

(defadvice skk-kakutei (before skk-nicola-ad activate compile)
  ;;
  (when (and skk-j-mode skk-henkan-on (not skk-henkan-active)
	     (markerp skk-nicola-okuri-flag))
    ;; $B3NDj$9$k$H$-$OAw$j3+;O$NI8<1$r>C$9!#(B
    (save-excursion
      (goto-char skk-nicola-okuri-flag)
      (when (eq (following-char) ?*)
	(delete-char 1))))
  ;;
  (setq skk-nicola-okuri-flag nil))

(defadvice skk-previous-candidate (before skk-nicola-ad activate compile)
  (when (or (and (markerp skk-nicola-okuri-flag)
		 (<= (point) (marker-position skk-nicola-okuri-flag)))
	    (or (not skk-henkan-on) skk-henkan-active))
    (setq skk-nicola-okuri-flag nil)))

(defadvice skk-nicola-self-insert-lshift (around skk-nicola-ad-for-dcomp
						 activate compile)
  (cond
   (skk-dcomp-activate
    (if (or skk-henkan-active (not skk-henkan-on))
	ad-do-it
      (let (pos)
	(if (or skk-henkan-active (skk-get-prefix skk-current-rule-tree)
		(not skk-comp-stack))
	    (progn
	      (skk-set-marker skk-dcomp-start-point nil)
	      (skk-set-marker skk-dcomp-end-point nil))
	  (when (and (marker-position skk-dcomp-start-point)
		     (marker-position skk-dcomp-end-point))
	    (skk-dcomp-face-off)
	    (or (member (this-command-keys) skk-dcomp-keep-completion-keys)
		;;
		(if (eq this-command 'skk-nicola-self-insert-rshift)
		    (setq pos (point))
		  ;;
		  (condition-case nil
		      (delete-region skk-dcomp-start-point skk-dcomp-end-point)
		    (error))))))
	ad-do-it
	;;
	(when (and (eq this-command 'skk-nicola-self-insert-rshift)
		   skk-henkan-on
		   (not skk-henkan-active))
	  (when (and (markerp skk-dcomp-start-point)
		     (marker-position skk-dcomp-start-point)
		     (< (marker-position skk-dcomp-start-point) pos))
	    (delete-region skk-dcomp-start-point pos))
	  (when (and (markerp skk-dcomp-end-point)
		     (marker-position skk-dcomp-end-point)
		     (< (point) (marker-position skk-dcomp-end-point)))
	    (delete-region skk-dcomp-end-point (point))))
	;;
	(if (and (not (skk-get-prefix skk-current-rule-tree))
		 (not skk-okurigana)
		 (not skk-henkan-active))
	    (let ((pos (point)))
	      (condition-case nil
		  (progn
		    (skk-comp-do 'first 'silent)
		    (skk-set-marker skk-dcomp-start-point pos)
		    (skk-set-marker skk-dcomp-end-point (point))
		    (skk-dcomp-face-on skk-dcomp-start-point
				       skk-dcomp-end-point)
		    (goto-char skk-dcomp-start-point))
		(error
		 (setq skk-comp-stack nil)
		 (message nil))))))))
   (t
    ad-do-it)))

(defadvice skk-isearch-setup-keymap (before skk-nicola-ad activate compile)
  ;; $B?F;X%-!<$G%5!<%A$,=*N;$7$F$7$^$o$J$$$h$&$K!#(B
  (let ((keys (append skk-nicola-lshift-keys skk-nicola-rshift-keys)))
    (while keys
      (define-key (ad-get-arg 0) (car keys) 'skk-isearch-wrapper)
      (setq keys (cdr keys)))))

(defadvice isearch-char-to-string (around skk-nicola-ad activate compile)
  ;; $B%(%i!<$,=P$k$H8!:w$,CfCG$7$F;H$$?I$$$N$G!"L[$i$;$k!#(B
  (cond ((and skk-use-kana-keyboard (featurep 'skk-isearch)
	      (with-current-buffer
		  (get-buffer-create skk-isearch-working-buffer)
		skk-mode))
	 (condition-case nil
	     ad-do-it
	   (error)))
	(t
	 ad-do-it)))

(defadvice isearch-text-char-description (around skk-nicola-ad activate
						 compile)
  ;; $B%(%i!<$,=P$k$H8!:w$,CfCG$7$F;H$$?I$$$N$G!"L[$i$;$k!#(B
  (cond ((and skk-use-kana-keyboard (featurep 'skk-isearch)
	      (with-current-buffer
		  (get-buffer-create skk-isearch-working-buffer)
		skk-mode))
	 (condition-case nil
	     ad-do-it
	   (error)))
	(t
	 ad-do-it)))

(static-when (eq skk-emacs-type 'mule2)
  ;;
  (defadvice isearch-char-to-string (after skk-nicola-ad activate compile)
    ;; $B$3$N4X?t$,F|K\8l$r$A$c$s$H07$($J$$$3$H$KBP:v!#(B
    (when (integerp (ad-get-arg 0))
      (setq ad-return-value (skk-char-to-string (ad-get-arg 0))))))

;;

(put 'skk-nicola-insert 'isearch-command t)
(put 'skk-nicola-self-insert-lshift 'isearch-command t)
(put 'skk-nicola-self-insert-rshift 'isearch-command t)

;;

(require 'product)
(product-provide (provide 'skk-nicola) (require 'skk-version))

;; skk-nicola.el ends here
