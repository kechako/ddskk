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

;; $B$3$N%W%m%0%i%`$OL'1:0o;K$5$s:n$N(B  NICOLA-SKK version 0.39 $B$r4p$K!"(B Daredevil
;; SKK $B$KBP1~$5$;$?$b$N$G$9!#86:n$N%"%$%G%"$K4p$$$F<BAu$7$F$$$/M=Dj$G$9!#(B
;;
;; $B%-!<G[Ns$N%k!<%k$OJL%U%!%$%k$K:YJ,2=$7$F$$$^$9!#$3$l$i$O!"F1$8$/L'1:$5$s:n$N(B
;; omelet ($B$?$^$4MQ$N?F;X%7%U%HF~NO%$%s%?!<%U%'%$%9(B) $B$*$h$SF1;a$N(B web site $B$NJ8(B
;; $B>O$r4p$K$D$/$j$^$7$?!#(B
;;
;; $BF1;a$N%"%$%G%"$H?F;X%7%U%HF~NO$K4X$9$k$4?TNO$K7I0U$rI=$7$^$9!#(B

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

(defgroup skk-nicola nil "SKK NICOLA related customization."
  :prefix "skk-nicola-"
  :group 'skk
  :group 'skk-kanagaki)

;; Variables.

(defcustom skk-nicola-interval
  (if (featurep 'lisp-float-type) (/ (float 1) (float 10)) 1) "\
*$B$3$N;~4V0JFb$KBG80$5$l$?$b$N$rF1;~BG80$HH=Dj$9$k!#(B
$BC10L$OIC!#%G%U%)%k%H$O(B 0.1 $BIC!#$?$@$7!"(B Emacs 18 $B$N>l9g$OIbF0>.?tE@?t$r07$($J$$(B
$B$?$a!"2>$K(B 1 $BIC$H$7$F$*$/$,!"<BMQE*$K$O87$7$$$H;W$o$l$k!#(B"
  :type 'number
  :group 'skk-nicola)

(defcustom skk-nicola-latin-interval
  (if (featurep 'lisp-float-type) (/ (float 1) (float 10)) 1) "\
*$B$3$N;~4V0JFb$KBG80$5$l$?$b$N$rF1;~BG80$HH=Dj$9$k!#(B
$BC10L$OIC!#%G%U%)%k%H$O(B 0.1 $BIC!#$?$@$7!"(B Emacs 18 $B$N>l9g$OIbF0>.?tE@?t$r07$($J$$(B
$B$?$a!"2>$K(B 1 $BIC$H$7$F$*$/$,!"<BMQE*$K$O87$7$$$H;W$o$l$k!#(B"
  :type 'number
  :group 'skk-nicola)

(defcustom skk-nicola-lshift-key
  (cond ((eq system-type 'windows-nt) [noconvert])
	((memq skk-emacs-type '(nemacs mule1 mule3)) "\\")
	(t [muhenkan])) "\
*$B:8?F;X%-!<$H$7$F;H$&%-!<!#(B"
  :type 'sexp
  :group 'skk-nicola)

(defcustom skk-nicola-rshift-key
  (cond ((eq system-type 'windows-nt) [convert])
	((eq skk-emacs-type 'xemacs) [henkan-mode])
	((string-match "^19.28" emacs-version) [key-35])
	((string-match "^19.\\(29\\|3[0-4]\\)" emacs-version) [numbersign])
	((memq skk-emacs-type '(nemacs mule1 mule3)) " ")
	(t [henkan])) "\
*$B1&?F;X%-!<$H$7$F;H$&%-!<!#(B"
  :type 'sexp
  :group 'skk-nicola)

(defcustom skk-nicola-use-lshift-as-space nil "\
*Non-nil $B$G$"$l$P:8?F;X%-!<$b%9%Z!<%9%-!<$H$7$FMxMQ$9$k!#(B"
  :type 'boolean
  :group 'skk-nicola)

(defcustom skk-nicola-use-space-as-rshift t "\
*Non-nil $B$G$"$l$P%9%Z!<%9%-!<$b1&?F;X%-!<$H$7$FMxMQ$9$k!#(B"
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

(skk-deflocalvar skk-nicola-lshift-original nil "\
skk-mode $B$KF~$k:]$K!":8?F;X%-!<$NK\Mh$NDj5A$rD4$Y$k$?$a$NJQ?t!#(B")

(skk-deflocalvar skk-nicola-okuri-flag nil)

(skk-deflocalvar skk-current-local-map nil "\
Emacs 18 $BMQ$NJQ?t!#(B")

(static-when (memq skk-emacs-type '(nemacs mule1 mule3))
  (case skk-kanagaki-jidou-key-symbol-kakikae-service
    ;;
    (nicola-jis
     ;; Emacs 18 $B$G$O(B Muhenkan, Henkan $B$O;H$($J$$$h$&$J$N$G!";H$($k%-!<$K=q49$((B
     ;; $B$k!#(B
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
	  (setq skk-nicola-lshift-key [f18]
		skk-nicola-rshift-key [f19]))
	 (t
	  (setq skk-nicola-lshift-key "_"
		skk-nicola-rshift-key " ")))))))

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
    (setq skk-nicola-lshift-original
	  (static-cond
	   ((memq skk-emacs-type '(nemacs mule1))
	    (or (lookup-key (or skk-current-local-map
				(make-sparse-keymap))
			    skk-nicola-lshift-key)
		(lookup-key (current-global-map)
			    skk-nicola-lshift-key)))
	   (t
	    (let (skk-mode skk-j-mode)
	      (key-binding skk-nicola-lshift-key)))))
    ;;
    (case skk-kanagaki-state
      (kana
       (setq skk-hiragana-mode-string skk-nicola-hiragana-mode-string
	     skk-katakana-mode-string skk-nicola-katakana-mode-string))
      (rom
       (setq skk-hiragana-mode-string skk-nicola-hiragana-rom-string
	     skk-katakana-mode-string skk-nicola-katakana-rom-string)))
    (setq skk-hiragana-mode-indicator
	  (skk-mode-string-to-indicator 'hiragana
					skk-hiragana-mode-string))
    (setq skk-katakana-mode-indicator
	  (skk-mode-string-to-indicator 'katakana
					skk-katakana-mode-string))
    (skk-update-modeline (if skk-katakana
			     skk-katakana-mode-indicator
			   skk-hiragana-mode-indicator)))))

;; Functions.

(defun skk-nicola-setup ()
  ;; SKK $B$N=i2s5/F0;~$N$_<B9T$5$l$k$Y$-$b$N$O$3$N4X?t$KF~$l$k!#(B
  (when skk-nicola-use-space-as-rshift
    (define-key skk-j-mode-map " " 'skk-nicola-self-insert-rshift))
  ;;
  (define-key skk-j-mode-map skk-nicola-lshift-key
    'skk-nicola-self-insert-lshift)
  (define-key skk-j-mode-map skk-nicola-rshift-key
    'skk-nicola-self-insert-rshift)
  ;;
  (when skk-nicola-use-space-as-rshift
    (define-key skk-latin-mode-map " "
      'skk-nicola-turn-on-j-mode))
  ;;
  (define-key skk-latin-mode-map skk-nicola-lshift-key
    'skk-nicola-turn-on-j-mode)
  (define-key skk-latin-mode-map skk-nicola-rshift-key
    'skk-nicola-turn-on-j-mode)
  ;;
  (when skk-nicola-help-key
    (define-key help-map skk-nicola-help-key
      'skk-nicola-help))
  (when skk-nicola-2nd-help-key
    (define-key help-map skk-nicola-2nd-help-key
      'skk-nicola-2nd-help))
  ;;
  (unless skk-nicola-plain-rule
    (setq skk-nicola-plain-rule
	  (symbol-value
	   (intern
	    (format "skk-%s-plain-rule-list"
		    skk-kanagaki-keyboard-type)))))
  (unless skk-nicola-lshift-rule
    (setq skk-nicola-lshift-rule
	  (symbol-value
	   (intern
	    (format "skk-%s-lshift-rule-list"
		    skk-kanagaki-keyboard-type)))))
  (unless skk-nicola-rshift-rule
    (setq skk-nicola-rshift-rule
	  (symbol-value
	   (intern
	    (format "skk-%s-rshift-rule-list"
		    skk-kanagaki-keyboard-type)))))
  ;;
  (remove-hook 'skk-mode-hook 'skk-niola-setup))

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
   (append
    '((skk-nicola-lshift-key . "$B:8?F;X%7%U%H%-!<(B")
      (skk-nicola-rshift-key . "$B1&?F;X%7%U%H%-!<(B"))
    ;;
    (list
     (when (and skk-nicola-use-space-as-rshift
		(not (member (key-description skk-nicola-rshift-key)
			     '("SPC" "space"))))
       '("space" . "$B1&?F;X%7%U%H%-!<!"Aw$j$J$7JQ493+;O(B")))
    ;;
    (list
     (do ((spec (nth 4 skk-kanagaki-rule-tree) (cdr spec))
	  (list nil (car spec))
	  (str nil (when (memq
			  (nth 3 list)
			  '(skk-kanagaki-set-okurigana
			    skk-kanagaki-set-okurigana-no-sokuon))
		     (nth 1 list))))
	 ((or str (null spec))
	  (when (stringp str)
	    (cons str "$BAw$j$"$jJQ493+;O(B")))))
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
  (skk-nicola-insert arg))

;;;###autoload
(defun skk-nicola-turn-on-j-mode (&optional arg)
  ;; `skk-latin-mode' $B$K$*$$$F!"(B $B:81&?F;X%-!<$NF1;~BG80$K$h$C$F(B 'skk-j-mode' $B$K(B
  ;; $BF~$k!#(B
  (interactive "*p")
  (if (skk-sit-for skk-nicola-latin-interval t)
      ;; then
      (let ((last-command-char ?\ ))
	(call-interactively 'self-insert-command t))
    ;; else
    (let ((last (static-cond
		 ((eq skk-emacs-type 'xemacs)
		  (event-key last-command-event))
		 ((memq skk-emacs-type '(nemacs mule1))
		  last-command-char)
		 (t last-command-event)))
	  (next (static-cond
		 ((eq skk-emacs-type 'xemacs)
		  (event-key (next-command-event)))
		 (t (next-command-event))))
	  char)
      (if (eq last next)
	  ;; then
	  (let ((last-command-char ?\ ))
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
				(list skk-nicola-rshift-key
				      skk-nicola-lshift-key)
				(when skk-nicola-use-space-as-rshift
				  (list " ")))))
	       (skk-j-mode-on)
	       (when (and skk-use-color-cursor (skk-color-display-p))
		 ;; $B?7$7$$(B skk-cursor $BBP:v(B
		 (static-cond
		  ((eq skk-emacs-type 'xemacs)
		   (set-face-property
		    'text-cursor 'background skk-cursor-hiragana-color
		    (current-buffer)))
		  (t
		   (set-buffer-local-cursor-color skk-cursor-hiragana-color)))))
	      (char
	       (let ((last-command-char ?\ ))
		 (call-interactively 'self-insert-command t))
	       (let ((last-command-char char))
		 (call-interactively 'self-insert-command t))))))))

;;;###autoload
(defun skk-nicola-insert (&optional arg)
  ;; $BF1;~BG80$rG'<1$7$F!"(BNICOLA $B$+$JF~NO$r$9$k!#(B
  (interactive "*p")
  (unless (and skk-henkan-on (not skk-henkan-active))
    (setq skk-nicola-okuri-flag nil))
  ;;
  (cond ((skk-sit-for skk-nicola-interval t)
	 ;; No input in the interval.
	 (case this-command
	   (skk-nicola-self-insert-rshift
	    ;; ($BJQ49!&%9%Z!<%9(B)
	    (skk-nicola-space-function arg))
	   (skk-nicola-self-insert-lshift
	    ;; $B:8%7%U%H(B
	    (skk-nicola-lshift-function arg))
	   (t
	    ;; $BJ8;z(B
	    (skk-nicola-insert-kana last-command-char skk-nicola-plain-rule
				    arg))))
	(t
	 ;; Some key's pressed.
	 (let ((next (next-command-event))
	       nextasstr)
	   ;;
	   (static-cond
	    ((eq skk-emacs-type 'xemacs)
	     (let ((char (event-to-character next)))
	       (setq next (cond ((characterp char) char)
				(t (event-key next))))))
	    (t
	     (cond ((symbolp next)
		    (setq next (vector next)))
		   ((characterp next)
		    (setq nextasstr (char-to-string next))))))
	   ;;
	   (case (lookup-key skk-j-mode-map (or nextasstr next))
	     (skk-nicola-self-insert-rshift
	      ;; $B1&%7%U%H(B
	      (case this-command
		(skk-nicola-self-insert-rshift
		 ;; [$B1&(B $B1&(B]
		 (let ((last-command-char ?\ ))
		   (cond ((or skk-henkan-on skk-henkan-active)
			  (skk-kanagaki-insert arg)
			  (unless (>= skk-nicola-interval 1)
			    ;; Emacs 18  $B$GC1FHBG80$rF10l%-!<O"B3BG80$GBeMQ$G$-(B
			    ;; $B$k$h$&$K!#(B
			    (skk-kanagaki-insert arg)))
			 (t
			  (self-insert-command
			   (if (>= skk-nicola-interval 1)
			       ;; Emacs 18 $B$GC1FHBG80$rF10l%-!<O"B3BG80$GBeMQ$G(B
			       ;; $B$-$k$h$&$K!#(B
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
		 (skk-nicola-insert-kana last-command-char
					 skk-nicola-rshift-rule arg))))
	     (skk-nicola-self-insert-lshift
	      ;; $B:8%7%U%H(B
	      (case this-command
		(skk-nicola-self-insert-lshift
		 ;;[$B:8(B $B:8(B]
		 (cond ((skk-in-minibuffer-p)
			(exit-minibuffer))
		       (t
			(skk-nicola-lshift-function arg)
			(unless (>= skk-nicola-interval 1)
			  ;; Emacs 18  $B$GC1FHBG80$rF10l%-!<O"B3BG80$GBeMQ$G$-$k(B
			  ;; $B$h$&$K!#(B
			  (skk-nicola-lshift-function 1)))))
		(skk-nicola-self-insert-rshift
		 ;; [$B1&(B $B:8(B]
		 (if (and skk-j-mode (not skk-katakana))
		     (skk-latin-mode 1)
		   (skk-toggle-kana 1)))
		(t
		 ;; [$BJ8;z(B $B:8(B]
		 (skk-nicola-insert-kana last-command-char
					 skk-nicola-lshift-rule arg))))
	     (t
	      ;; $BJ8;z(B
	      (cond
	       ((eq this-command 'skk-nicola-self-insert-rshift)
		;;  [$B1&(B $BJ8;z(B]
		(skk-nicola-insert-kana next skk-nicola-rshift-rule arg))
	       ((eq this-command 'skk-nicola-self-insert-lshift)
		;; [$B:8(B $BJ8;z(B]
		(skk-nicola-insert-kana next skk-nicola-lshift-rule arg))
	       ((and
		 (not (eq last-command-char next))
		 (memq last-command-char skk-nicola-set-henkan-point-chars)
		 (memq next skk-nicola-set-henkan-point-chars))
		;; [fj]
		(cond ((and skk-henkan-on (not skk-henkan-active))
		       (skk-nicola-set-okuri-flag))
		      (t
		       (skk-set-henkan-point-subr 1))))
	       ((and
		 (not (eq last-command-char next))
		 (memq last-command-char skk-nicola-prefix-suffix-abbrev-chars)
		 (memq next skk-nicola-prefix-suffix-abbrev-chars))
		;; [gh] suffix $B$N(B $BF~NO(B
		(cond (skk-henkan-active
		       ;; $B@\Hx8l$N=hM}(B
		       (skk-kakutei)
		       (skk-set-henkan-point-subr)	
		       (insert ?>))
		      ((and skk-henkan-on (not skk-henkan-active))
		       ;; $B@\F,8l$N=hM}(B
		       (insert ?>)
		       (skk-set-marker skk-henkan-end-point (point))
		       (setq skk-henkan-count 0
			     skk-henkan-key (buffer-substring
					     skk-henkan-start-point (point)))
		       (skk-henkan))
		      (t
		       ;;
		       (skk-abbrev-mode 1))))
	       ((and
		 (not (eq last-command-char next))
		 (memq last-command-char skk-nicola-toggle-kana-chars)
		 (memq next skk-nicola-toggle-kana-chars))
		;; [dk]
		(skk-toggle-kana 1))
	       (t
		;; [$BJ8;z(B $BJ8;z(B]
		(let ((str (skk-nicola-insert-kana
			    last-command-char skk-nicola-plain-rule arg)))
		  (when (and skk-isearch-switch
			     (not (or skk-henkan-on skk-henkan-active)))
		    (setq isearch-cmds
			  (cons
			   (append
			    (list (concat (caar isearch-cmds) str)
				  (concat (cadar isearch-cmds) str))
			    (cddar isearch-cmds))
			   isearch-cmds))))
		(unless (and (>= skk-nicola-interval 1)
			     (eq next last-command-char))
		  ;; Emacs 18 $B$GC1FHBG80$rF10l%-!<O"B3BG80$GBeMQ$G$-$k$h$&$K!#(B
		  (skk-nicola-insert-kana next skk-nicola-plain-rule)))))))))
  ;; `skk-kana-input' $B$,2?$bF~NO$7$J$$$h$&$K!"(Bnil $B$rJV$7$F$*$/!#(B
  nil)

(defun skk-nicola-insert-kana (char rule &optional arg)
  ;; CHAR $B$r(B RULE $B$NCf$+$iC5$7$FF~NO$9$Y$-J8;zNs$r7hDj$9$k!#(B
  ;; ARG $B$rM?$($i$l$?>l9g$O$=$N?t$@$1J8;zNs$rO"7k$7$FF~NO$9$k!#(B
  (let* ((el (cadr (assq char rule)))
	 (str (when el (cond ((stringp el) el)
			     (skk-katakana (car el))
			     (t (cdr el)))))
	 (arg (prefix-numeric-value arg)))
    ;;
    (when str
      (skk-insert-str (setq str (skk-kanagaki-make-string arg str))))
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
	(len (if (eq skk-emacs-type 'nemacs) 2 1)) tag)
    (cond ((and (not (eq skk-nicola-okuri-style 'nicola-skk))
		(member okuri '("$B$C(B" "$B%C(B")))
	   ;; $B2?$b$7$J$$!#(B
	   )
	  (t
	   (save-excursion
	    (goto-char skk-nicola-okuri-flag)
	    (when (eq (following-char) ?*)
	      (delete-char 1))
	    (backward-char (* len 1))
	    (if (member
		 (buffer-substring-no-properties
		  (point) (marker-position skk-nicola-okuri-flag))
		 '("$B$C(B" "$B%C(B"))
		(setq tag 'no-sokuon)))
	   (static-cond
	    ((memq skk-emacs-type '(nemacs mule1))
	     ;; $BM}M3$,$h$/J,$+$i$J$$$,!"(Bpoint $B$,%:%l$F$7$^$&!#(B`skk-insert' $B$rH4(B
	     ;; $B$1$F$+$iJQ49$9$k$H$&$^$/$$$/!#(B(??)
	     (throw 'okuri (or tag 'ari)))
	    (t
	     (skk-kanagaki-set-okurigana tag)))))))

(defun skk-nicola-set-okuri-flag ()
  ;; $BAw$j3+;OE@$r(B marker $B$GI8<1$9$k$H$H$b$K!"(B`*' $B$rA^F~$9$k$3$H$GAw$j$"$jJQ49$N(B
  ;; $BBT$A>uBV$G$"$k$3$H$rL@<($9$k!#(B
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
    (if (or skk-henkan-on skk-henkan-active)
	;; $BJQ49$9$k!#(B
	(skk-kanagaki-insert arg)
      (self-insert-command arg))))

(defun skk-nicola-lshift-function (&optional arg)
  (cond ((or skk-henkan-active skk-henkan-on)
	 ;; $B3NDj$K;H$&!#(B
	 (let ((skk-egg-like-newline t))
	   (newline arg)))
	(skk-nicola-use-lshift-as-space
	 ;;
	 (skk-nicola-space-function arg))
	(t
	 ;; $B2~9T$K;H$&!#(B
	 (if (skk-in-minibuffer-p)
	     (call-interactively 'exit-minibuffer)
	   (newline arg)))))

(defun skk-nicola-setup-tutorial ()
  (when skk-nicola-use-space-as-rshift
    (define-key skktut-j-mode-map " " 'skk-nicola-self-insert-rshift))
  ;;
  (define-key skktut-j-mode-map skk-nicola-lshift-key
    'skk-nicola-self-insert-lshift)
  (define-key skktut-j-mode-map skk-nicola-rshift-key
    'skk-nicola-self-insert-rshift)
  ;;
  (when skk-nicola-use-space-as-rshift
    (define-key skktut-latin-mode-map " "
      'skk-nicola-turn-on-j-mode))
  ;;
  (define-key skktut-latin-mode-map skk-nicola-lshift-key
    'skk-nicola-turn-on-j-mode)
  (define-key skktut-latin-mode-map skk-nicola-rshift-key
    'skk-nicola-turn-on-j-mode))

;; Pieces of Advice.

(defadvice skk-insert (before skk-nicola-ad activate compile)
  (when (or (and (markerp skk-nicola-okuri-flag)
		 (<= (point) (marker-position skk-nicola-okuri-flag)))
	    (or (not skk-henkan-on) skk-henkan-active))
    (setq skk-nicola-okuri-flag nil)))

(defadvice skk-latin-mode (before skk-nicola-ad activate compile)
  (setq skk-nicola-okuri-flag nil))

(defadvice skk-toggle-kana (before skk-nicola-ad activate compile)
  (setq skk-nicola-okuri-flag nil))

(defadvice skk-abbrev-mode (before skk-nicola-ad activate compile)
  (setq skk-nicola-okuri-flag nil))

(defadvice skk-jisx0208-latin-mode (before skk-nicola-ad activate compile)
  (setq skk-nicola-okuri-flag nil))

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

(defadvice skk-kanagaki-toggle-rom-kana (around skk-nicola-ad activate
						preactivate)
  (setq skk-nicola-okuri-flag nil)
  ad-do-it
  ;; $B%b!<%I9T$NI=<($ND4@a!#(B
  (case skk-kanagaki-state
    (kana
     (setq skk-hiragana-mode-string skk-nicola-hiragana-mode-string
	   skk-katakana-mode-string skk-nicola-katakana-mode-string))
    (rom
     (setq skk-hiragana-mode-string skk-nicola-hiragana-rom-string
	   skk-katakana-mode-string skk-nicola-katakana-rom-string)))
  (setq skk-hiragana-mode-indicator
	(skk-mode-string-to-indicator 'hiragana
				      skk-hiragana-mode-string))
  (setq skk-katakana-mode-indicator
	(skk-mode-string-to-indicator 'katakana
				      skk-katakana-mode-string))
  (let ((list (buffer-list))
	buf)
    (while list
      (when (buffer-live-p (setq buf (car list)))
	(with-current-buffer buf
	  (when skk-j-mode
	    (skk-update-modeline (if skk-katakana
				     skk-katakana-mode-indicator
				   skk-hiragana-mode-indicator)))))
      (setq list (cdr list))))
  (force-mode-line-update t))

(defadvice skk-kanagaki-start-henkn-okuriari (before skk-nicola-ad activate
						     compile)
  (setq skk-nicola-okuri-flag nil))

(defadvice skk-previous-candidate (before skk-nicola-ad activate compile)
  (when (or (and (markerp skk-nicola-okuri-flag)
		 (<= (point) (marker-position skk-nicola-okuri-flag)))
	    (or (not skk-henkan-on) skk-henkan-active))
    (setq skk-nicola-okuri-flag nil)))

(static-unless (memq skk-emacs-type '(nemacs mule1))
  ;;
  (defadvice skk-isearch-setup-keymap (before skk-nicola-ad activate compile)
    ;; $B?F;X%-!<$G%5!<%A$,=*N;$7$F$7$^$o$J$$$h$&$K!#(B
    (define-key (ad-get-arg 0) skk-nicola-lshift-key 'skk-isearch-wrapper)
    (define-key (ad-get-arg 0) skk-nicola-rshift-key 'skk-isearch-wrapper))
  ;;
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
  ;;
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
	   ad-do-it))))

(static-when (eq skk-emacs-type 'mule2)
  ;;
  (defadvice isearch-char-to-string (after skk-nicola-ad activate compile)
    ;; $B$3$N4X?t$,F|K\8l$r$A$c$s$H07$($J$$$3$H$KBP:v!#(B
    (when (integerp (ad-get-arg 0))
      (setq ad-return-value (skk-char-to-string (ad-get-arg 0))))))

(static-when (memq skk-emacs-type '(nemacs mule1))
  ;;
  (defadvice skk-insert (around skk-nicola-ad-e18 activate)
    ;; $B%P%0$N860x$,L@$i$+$K$J$k$^$G$N(B work around$B!#(B
    (cond (skk-nicola-okuri-flag
	   (let ((tag (catch 'okuri (skk-nicola-insert (ad-get-arg 0)))))
	     (when (memq tag '(ari no-sokuon))
	       (skk-kanagaki-set-okurigana (eq tag 'no-sokuon)))))
	  (t
	   ad-do-it)))
  ;;
  (defadvice skk-nicola-self-insert-lshift (around skk-nicola-ad-e18 activate)
    ;; $B%P%0$N860x$,L@$i$+$K$J$k$^$G$N(B work around$B!#(B
    (cond (skk-nicola-okuri-flag
	   (let ((tag (catch 'okuri (skk-nicola-insert (ad-get-arg 0)))))
	     (when (memq tag '(ari no-sokuon))
	       (skk-kanagaki-set-okurigana (eq tag 'no-sokuon)))))
	  (t
	   ad-do-it))))

(if (featurep 'skk-tut)
    (skk-nicola-setup-tutorial)
  ;;
  (defadvice skk-tutorial (after skk-nicola-advice-to-skk-tutorial activate)
    (skk-nicola-setup-tutorial)
    (ad-deactivate-regexp "^skk-nicola-advice-for skk-tutorial$")))

;;

(put 'skk-nicola-insert 'isearch-command t)
(put 'skk-nicola-self-insert-lshift 'isearch-command t)
(put 'skk-nicola-self-insert-rshift 'isearch-command t)

;;

(require 'product)
(product-provide (provide 'skk-nicola) (require 'skk-version))

;; skk-nicola.el ends here
