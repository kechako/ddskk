;;; skk-kanagaki.el --- SKK $B$N2>L>F~NO%5%]!<%H(B
;; Copyright (C) 2000 Tetsuo Tsukamoto <czkmt@remus.dti.ne.jp>

;; Author: Tetsuo Tsukamoto <czkmt@remus.dti.ne.jp>
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

;; {$B$F$C$H$jAa$$;H$$$+$?(B ($B;CDj%P!<%8%g%s(B)}
;;
;; ~/.skk $B$K(B
;;
;; (setq skk-use-kana-keyboard)
;; (setq skk-kanagaki-keyboard-type '106-jis)
;;
;; $B$H=q$/!#(B
;;
;;
;; {$B@bL@(B}
;;
;; $B$3$N%W%m%0%i%`$O(B SKK $B$K$*$$$F%m!<%^;zF~NO$J$i$L2>L>F~NO$r%5%]!<%H$9$k$3$H(B
;; $B$rL\E*$H$7$^$9!#(B NICOLA $B$d5l(B JIS $BG[Ns$KBP1~$7$^$9!#(B
;;
;; $B$J$*!"0J2<$O!V?F;X%7%U%HF~NO!W0J30$NNc$G$9!#?F;X%7%U%HF~NO$NNc$K$D$$$F$O!"(B
;; README.NICOLA.ja $B$H(B skk-nicola.el $B$r8fMw$/$@$5$$!#(B
;;
;;  -*- $BLdBjE@(B -*-
;;
;; 1. Emacs Lisp $B$N%l%Y%k$G$NLdBj(B
;;
;; $B2>L>F~NO$K$*$$$F$O(B SHIFT $B%-!<$rMxMQ$7$FF~NO$5$l$k2>L>$b$"$k$?$a!"(B SKK $BK\Mh(B
;; $B$N(BSHIFT $B$N;H$$J}$,$G$-$^$;$s!#$=$NB>$$$m$$$m(B SKK $B$i$7$/$J$$$N$G$9$,!"(B $B$H$j(B
;; $B$"$($:!"(B
;;
;;   o $BJQ493+;OE@$N;XDj$O2>L>F~NO$H$OJL$K9T$&!#(B
;;   o $BJQ49$N3+;O$ODL>oDL$j!"(B [SPC] $B$G;X<($9$k!#(B $B$?$@$7!"Aw$j$"$j$NJQ49$N$H$-(B
;;     $B$O(B $BAw$j3+;OE@$r;XDj$9$k$?$a$NFC<l$JA`:n$r$9$k!#(B
;;
;; $B$7$F$"$j$^$9!#Nc$($P!"!V4r$7$$!W$rF~NO$9$k$?$a$K$O(B
;;
;; [fj] $B$&$l$7(B [fj] $B$$(B
;;
;; $B$N$h$&$KF~NO$7$^$9!#(B[fj] $B$H$O(B f $B$H(B j $B$r(B $BF1;~$KBG80$9$k$3$H$G$9!#>\$7$/$O(B
;;
;; 2. $B%7%9%F%`%l%Y%k$G$NLdBj(B
;;
;; $BBh(B 2 $B$NLdBjE@$H$7$F!"(B $B%-!<G[Ns$N@_Dj$K$h$j9o0uDL$j$NF~NO$,$G$-$J$$>l9g$,$"(B
;; $B$j$^$9!#Nc$($PF|K\8l(B 106 $B%-!<%\!<%I;HMQ;~!"(BXFree86 $B>e$G$O(B
;;
;; o $B!V!o!W%-!<(B ($B2>A[%-!<%3!<%I(B 133)
;; o $B!V!@!W%-!<(B ($B2>A[%-!<%3!<%I(B 123)
;;
;; $B$O$$$:$l$b(B backslash $B$H$7$F07$o$l$^$9!#$7$+$72>L>F~NO$K$*$$$FA0<T$O(B $B!V!<!W!"(B
;; $B8e<T$O!V$m!W(B $B$H$J$k$3$H$,K>$^$l$^$9!#$3$N>l9g$NBP1~:v$H$7$F!"Nc$($P(B
;;
;; % cat >> ~/.Xmodmap
;;
;; keycode 123 = underscore underscore
;; % xmodmap ~/.Xmodmap
;;
;; $B$J$I$H$7$F$*$$$F$+$i!"(B~/.skk $B$K(B
;;
;; (setq skk-kanagaki-rule-list
;;       '(("\\" nil "$B!<(B")))
;;
;; $B$H=q$/$3$H$J$I$,9M$($i$l$^$9!#(B
;; ($BF1MM$N%"%$%G%"$O(B Canna $B$G2>L>F~NO$9$k:]$K$bM-8z$G$"$k$h$&$G$9!#(B)
;;
;; $B$b$7$"$J$?$,(B XEmacs $B$N%Y!<%?%F%9%?!<$J$i$P(B
;;
;; keycode 123 = kana_RO underscore
;; keycode 19 = 0 kana_WO
;;
;; $B$J$s$F@_Dj$G$H$F$b9,$;$K$J$l$k$+$b$7$l$^$;$s!#(B (Mr. XEmacs $B$N$7$o$6$+$J(B?)
;;
;; $B$5$i$K!"$b$7$"$J$?$,(B PC-98 $B%f!<%6(B $B$G(B XEmacs $B$N%Y!<%?%F%9%?!<$J$i$P!"$*$b$`(B
;; $B$m$K!V$+$J!W%-!<$r%m%C%/$7$F$_$F$/$@$5$$!#(B ;')
;;
;;  -*- $B;H$$J}(B -*-
;;
;; 1. $BJQ493+;OE@$N;XDj(B
;;
;; $BDL>o$N(B SKK $B$K$*$$$F$O!"(B SHIFT $B$r2!$7$J$,$iF~NO$9$k$3$H$GJQ493+;O0LCV$rL@<((B
;; $B$7$F$$$^$7$?$,!"2>L>F~NO$G$O$3$l$,$G$-$^$;$s!#$=$3$G!"JL$NJ}K!$GJQ493+;OE@(B
;; $B$r;XDj$7$J$1$l$P$J$j$^$;$s!#$=$3$G!"!V(Bf $B$H(B j $B$rF1;~$K2!$9!W$H$$$&<jK!$r;H$$(B
;; $B$^$9!#0J2<$N(B [fj] $B$O!"F1;~BG80$r0UL#$7$^$9!#(B
;;
;; [fj] $B$O$k(B $B"M(B $B"&$O$k(B [SPC] $B"M(B $B"'=U(B
;;
;; $B$^$?$O(B
;;
;; $B$O$k(B ^B^B [fj] $B"M(B $B"&$O$k(B ^F^F [SPC] $B"M(B $B"'=U(B
;;
;; 2. $BAw$j$"$j$NJQ49$N$7$+$?(B
;;
;; $BDL>o$N(B SKK $B$K$*$$$F$O!"(B SHIFT $B$r2!$7$J$,$iF~NO$9$k$3$H$GAw$j2>L>$N0LCV$rL@(B
;; $B<($7$F$$$^$7$?!#2>L>F~NO(B SKK $B$K$*$$$F$O$=$l$O$G$-$^$;$s!#$=$3$G(B
;;
;; o [fj] $B$,2!$5$l$?$H$-$K!"(B $BD>A0$N(B 1 $BJ8;z$rAw$j2>L>$H8+Jo$7$FJQ49$r3+;O$9$k!#(B
;;
;; $B$H$$$&<jK!$r;H$$$^$9!#(B $BNc$($P!"!VC#$9!W$HF~NO$7$?$$>l9g$O(B
;;
;; $B"&$?$C$9(B [fj]  $B"M(B $B"'C#$9(B
;;
;; $B$N$h$&$K$J$j$^$9!#!VBT$C$F!W$HF~NO$7$?$$>l9g$O(B
;;
;; $B"&$^$C(B [fj] $B"M(B $B"'BT$C(B
;;
;; $B$H$7$F$+$i!V$F!W$rF~NO$7$^$9!#(B
;;
;; 3. $B$$$/$D$+$N=EMW$J%-!<Dj5A$K$D$$$F(B
;;
;; $B%+%JF~NO$,(B $B!V(Bq$B!W!"(B abbrev $B%b!<%I$,(B $B!V(B/$B!W!"(Blatin $B%b!<%I$,(B $B!V(Bl$B!W$J$I$ODjHV$G(B
;; $B$9$,!"2>L>F~NO$G$O$3$l$b;H$($^$;$s!#$=$N$?$a!"$3$l$i$N$&$A=EMW$H;W$o$l$k$b(B
;; $B$N$rJL$N%-!<Dj5A$K$7$F$"$j$^$9!#(BC-h 3 $B$HF~NO$9$k$H!"8=:_$N%b!<%I$K$*$1$kFC(B
;; $B<l$J%-!<Dj5A$r3NG'$G$-$^$9!#(B
;; $B$J$*!"$3$l$i$OF1;~$K%U%!%s%/%7%g%s%-!<$KB`Hr$7$F$"$j$^$9!#(B
;;
;; [f2]  $B!D(B $BJQ493+;OE@$N;XDj(B
;; [f3]  $B!D(B $B@\F,<-$^$?$O@\Hx<-JQ49(B
;; [f5]  $B!D(B $B%3!<%IF~NO(B
;; [f6]  $B!D(B abbrev $B%b!<%I(B
;; [f7]  $B!D(B $B%+%J%b!<%I$^$?$O%+%JJQ49(B
;; [f8]  $B!D(B $BA41Q%b!<%I(B
;; [f9]  $B!D(B $BH>3Q%+%J%b!<%I$^$?$OH>3Q%+%JJQ49(B
;; [f10] $B!D(B latin $B%b!<%I(B
;; [f12] $B!D(B $B%m!<%^;zF~NO(B $B"N(B $B2>L>F~NO(B $B$N@Z$jBX$((B

;;; Code:

(eval-when-compile
  (require 'cl)
  (require 'skk-kanagaki-util)
  (require 'skk-macs)
  (require 'skk-vars)
  (require 'static))

(require 'nicola-ddskk-autoloads)

(static-unless (memq skk-emacs-type '(nemacs mule1))
  (when window-system
    (require 'skk-kanagaki-menu)))

(defgroup skk-kanagaki nil "SKK kanagaki related customization."
  :prefix "skk-kanagaki-"
  :group 'skk)

;; Variables.

(defcustom skk-use-kana-keyboard t "\
*Non-nil $B$J$i2>L>F~NOMQ$N@_Dj$rFI$_9~$`!#(B
SKK $B;HMQCf$K$3$NJQ?t$NCM$r@Z$jBX$($k$3$H$G(B  $B%m!<%^;zF~NO(B $B"N(B $B2>L>F~NO(B $B$N@Z$jBX(B
$B$($,$G$-$k!#(B"
  :type 'boolean
  :group 'skk
  :group 'skk-kanagaki)

(defcustom skk-kanagaki-keyboard-type '106-jis "\
*$B2>L>F~NO$K;HMQ$9$k%-!<%\!<%I$N<oJL!#(B
$BCM$OG$0U$N%7%s%\%k!#(B $B$?$@$7(B `skk-kanagaki-{$B%7%s%\%kL>(B}-base-rule-list' $B$H$$$&(B
$BJQ?t$rMQ0U$7$J$1$l$P$J$i$J$$!#2?$b@_Dj$7$J$1$l$PF|K\8l(B 106 $B%-!<%\!<%IMQ$N@_Dj(B
$B$rMQ0U$7!"$3$l$r;HMQ$9$k!#(B"
  :type '(choice (const 106-jis)
		 (const nicola-jis)
		 (const nicola-us)
		 (const nicola-dvorak)
		 (const omelet-jis)
		 (const omelet-us)
		 (const omelet-dvorak)
		 (const oasys)
		 (symbol :tag "Another Keyboard Type"))
  :group 'skk-kanagaki)

(defcustom skk-kanagaki-set-henkan-point-key
  (cond ((memq skk-emacs-type '(nemacs mule1))
	 "\e[12~")
	(t
	 [f2])) "\
*$B$3$N%-!<$r2!$9$3$H$GJQ493+;O0LCV$r@_Dj$9$k!#(B
$BJQ493+;O0LCV$N@_Dj$O2>L>$rF~NO$9$kA0$K$*$3$J$C$F$b!"(B $BF~NO$7=*$o$C$?8e$G$*$3$J$C(B
$B$F$b9=$o$J$$!#(B"
  :type 'sexp
  :group 'skk-kanagaki)

(defcustom skk-kanagaki-abbrev-mode-key
  (cond ((memq skk-emacs-type '(nemacs mule1))
	 "\e[17~")
	(t
	 [f6])) "\
*$B$3$N%-!<$r2!$9$3$H$G(B abbrev $B%b!<%I$KF~$k!#(B"
  :type 'sexp
  :group 'skk-kanagaki)

(defcustom skk-kanagaki-katakana-mode-key
  (cond ((memq skk-emacs-type '(nemacs mule1))
	 "\e[18~")
	(t
	 [f7])) "\
*$B$3$N%-!<$r2!$9$3$H$G%+%J%b!<%I$H$+$J%b!<%I$r@Z$j$+$($k!#(B
$BJQ493+;O0LCV$N@_Dj8e$K2!$9$3$H$GBP>]J8;zNs$r%+%J$KJQ49$9$k$3$H$b$G$-$k!#(B"
  :type 'sexp
  :group 'skk-kanagaki)

(defcustom skk-kanagaki-latin-jisx0208-mode-key
  (cond ((memq skk-emacs-type '(nemacs mule1))
	 "\e[19~")
	(t
	 [f8])) "\
*$B$3$N%-!<$r2!$9$3$H$GA43Q1Q?t%b!<%I$KF~$k!#(B"
  :type 'sexp
  :group 'skk-kanagaki)

(defcustom skk-kanagaki-hankaku-mode-key
  (cond ((memq skk-emacs-type '(nemacs mule1))
	 "\e[20~")
	(t
	 [f9])) "\
*$B$3$N%-!<$r2!$9$3$H$GH>3Q%+%J%b!<%I$K@Z$j$+$($k!#(B
$BJQ493+;O0LCV$N@_Dj8e$K2!$9$3$H$GBP>]J8;zNs$rH>3Q%+%J$KJQ49$9$k$3$H$b$G$-$k!#(B"
  :type 'sexp
  :group 'skk-kanagaki)

(defcustom skk-kanagaki-latin-mode-key
  (cond ((memq skk-emacs-type '(nemacs mule1))
	 "\e[21~")
	(t
	 [f10])) "\
*$B$3$N%-!<$r2!$9$3$H$G(B latin $B%b!<%I$KF~$k!#(B"
  :type 'sexp
  :group 'skk-kanagaki)

(defcustom skk-kanagaki-toggle-rom-kana-key
  (cond ((memq skk-emacs-type '(nemacs mule1))
	 "\e[24~")
	(t
	 [f12])) "\
*$B$3$N%-!<$r2!$9$3$H$G(B $B%m!<%^;zF~NO(B $B"N(B $B2>L>F~NO$N@Z$jBX$($,$G$-$k!#(B"
  :type 'sexp
  :group 'skk-kanagaki)

(defcustom skk-kanagaki-code-input-key
  (cond ((memq skk-emacs-type '(nemacs mule1))
	 "\e[15~")
	(t
	 [f5])) "\
*$B$3$N%-!<$r2!$9$3$H$G%3!<%IF~NO$,$G$-$k!#(B"
  :type 'sexp
  :group 'skk-kanagaki)

(defcustom skk-kanagaki-midashi-henkan-key
  (cond ((memq skk-emacs-type '(nemacs mule1))
	 "\e[13~")
	(t
	 [f3])) "\
*$B$3$N%-!<$r2!$9$3$H$G@\F,<-$^$?$O@\Hx<-JQ49$r$9$k!#(B"
  :type 'sexp
  :group 'skk-kanagaki)

(defcustom skk-kanagaki-help-key "1" "\
*`help' $B$K$*$$$F%X%k%W$rI=<($9$k%-!<!#(B"
  :type 'sexp
  :group 'skk-kanagaki)

(defcustom skk-kanagaki-previous-candidate-key "\C-p" "\
*$BA08uJd$rI=<($9$k$?$a$N%-!<!#(B
XFree86 $B>e$G;HMQ$9$k>l9g!"(B $BNc$($P$3$NCM$r(B [henkan]  (XEmacs $B$G$O(B
[henkan-mode]) $B$K$9$l$P!"F|K\8l%-!<%\!<%I$N(B [$BA08uJd(B] $B%-!<$K3d$jEv$F$k$3$H$,$G(B
$B$-$k!#(B $BF1%-!<$O!"(BMule2.3@19.28 $B$G$O(B [key-35]$B!"(BMule2.3@19.34 $B$G$O(B [numbersign]
($B$J$<(B ??) $B$H$J$k$i$7$$!#(B"
  :type 'sexp
  :group 'skk-kanagaki)

(defcustom skk-kanagaki-start-henkan-key " " "\
*$BJQ49$r3+;O$9$k$?$a$N%-!<!#(B"
  :type 'sexp
  :group 'skk-kanagaki)

(defcustom skk-kanagaki-rule-list nil "\
*$B%-!<F~NO$KBP$9$kJQ49J8;z$N5,B'$G!";HMQ<T$NDI2C$N@_Dj$r9T$J$&$b$N!#(B
$BNc$($P!"(B $B%-!<G[Ns$rFH<+$K@_Dj$7$F$$$k>l9g$J$I$O!"$3$NJQ?t$rMQ$$$F$=$l$KBP1~$7(B
$B$?@_Dj$r$9$k$3$H$,$G$-$k!#(B"
  :type '(repeat
	  (list :tag "Rule"
		(string :tag "1 (keyboard input)")
		(choice :tag "2 (choose string if sokuon)"
			string
			(const nil))
		(choice :tag "3 (choice)"
			(symbol :tag "Function")
			(string :tag "String (common)")
			(cons :tag "Strings (katakana & hiragana)"
			 (string :tag "3-1 (katakana string)")
			 (string :tag "3-2 (hiragana string)")))))
  :group 'skk-kanagaki)

(defcustom skk-kanagaki-jidou-keymap-kakikae-service nil "\
*Non-nil $B$J$i2>L>F~NO$N$?$a$K>!<j$K%-!<G[Ns$r=q49$($k!#(B
X $B>e$G(B xmodmap $B$,<B9T2DG=$J>l9g$@$1M-8z!#F0:n$,2~A1$5$l$kBe$o$j$K!"B>$N%b!<%I(B
$B$d%"%W%j%1!<%7%g%s$K$b(B $B%-!<G[Ns$NJQ99$,1F6A$9$k$H$$$&I{:nMQ$,$"$k$N$G!"==J,Cm(B
$B0U$7$F;H$C$F$/$@$5$$!#(B"
  :type '(choice (const 106-jis)
		 (const 106-jis-kodawari)
		 (const nicola-jis)
		 (const oasys)
		 (const nil))
  :group 'skk-kanagaki)

;; Internal constants and variables.

(defvar skk-kanagaki-base-rule-list nil)
(defvar skk-kanagaki-rule-tree nil)
(defvar skk-kanagaki-rom-kana-rule-tree nil)

(defvar skk-kanagaki-state 'kana)

;; Functions.

(defalias-maybe 'help-mode 'fundamental-mode)

;;;###autoload
(defun skk-kanagaki-midashi-henkan (&optional arg)
  "$B@\F,<-$^$?$O@\Hx<-JQ49$r$9$k!#(B"
  (interactive "*p")
  (cond (skk-henkan-active
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
	 (skk-henkan))))

;;;###autoload
(defun skk-kanagaki-help ()
  (interactive)
  (skk-kanagaki-help-1
   "* SKK $B2>L>F~NO(B $B%X%k%W(B*"
   "$B8=:_$N2>L>F~NO%b!<%I$N<g$J%-!<Dj5A(B:"
   (append
    '((skk-kanagaki-set-henkan-point-key . "$BJQ493+;OE@$r%;%C%H(B")
      (skk-kanagaki-midashi-henkan-key . "$B@\F,<-(B or $B@\Hx<-JQ49(B")
      (skk-kanagaki-code-input-key . "$B%3!<%IF~NO(B")
      (skk-kanagaki-abbrev-mode-key . "abbrev $B%b!<%I(B")
      (skk-kanagaki-katakana-mode-key . "$B%+%J%b!<%I(B or $B%+%JJQ49(B")
      (skk-kanagaki-latin-jisx0208-mode-key . "$BA41Q%b!<%I(B")
      (skk-kanagaki-hankaku-mode-key . "$BH>3Q%+%J%b!<%I(B or $BH>3Q%+%JJQ49(B")
      (skk-kanagaki-latin-mode-key . "latin $B%b!<%I(B")
      (skk-kanagaki-toggle-rom-kana-key . "$B%m!<%^;zF~NO(B $B"N(B $B2>L>F~NO(B")
      (skk-kanagaki-previous-candidate-key . "$BA08uJdI=<((B")
      (skk-kanagaki-start-henkan-key . "$BJQ49!&<!8uJdI=<((B"))
    (list
     (cons (format "M-x help %s" skk-kanagaki-help-key) "$B$3$N%X%k%W$rI=<((B"))
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
	    (cons str "$BAw$j$"$jJQ493+;O(B"))))))))

;;;###autoload
(defun skk-kanagaki-insert (&optional arg)
  "SPC $B%-!<$@$1$3$l$r(B `skk-insert' $B$NBe$o$j$K;H$&!#(B"
  (interactive "*p")
  (cond ((eq arg 1)
	 (let ((last-command-char ?\ ))
	   (skk-insert arg)))
	(t
	 ;; C-u [SPC] $B$GAw$j$"$jJQ49$r$9$k!#(B
	 (skk-kanagaki-set-okurigana-no-sokuon t))))

;;;###autoload
(defun skk-kanagaki-set-okurigana (&optional no-sokuon)
  "$B%]%$%s%H$ND>A0$NJ8;z$rAw$j2>L>$H8+Jo$7$F!"JQ49$r3+;O$9$k!#(B
$B$?$@$7!"(B $B$b$&$R$H$DA0$NJ8;z$,B%2;$@$C$?>l9g$K$O!"(B $B$=$l0J9_$rAw$j2>L>$H8+Jo$9!#(B"
  (interactive)
  (let ((pt1 (point))
	(len (if (eq skk-emacs-type 'nemacs) 2 1))
	pt2 okuri sokuon)
    (setq okuri
	  (save-excursion
	    ;; $B$&$&!"$3$s$J$3$H$r$7$J$1$l$P$J$i$J$$$N$+(B...
	    (backward-char (* len 1))
	    (buffer-substring-no-properties (setq pt2 (point)) pt1)))
    (when okuri
      (unless no-sokuon
	(setq sokuon
	      (save-excursion
		(backward-char (* len 2))
		(buffer-substring-no-properties (point) pt2)))
	(unless (member sokuon '("$B$C(B" "$B%C(B"))
	  (setq sokuon nil)))
      ;;
      (save-excursion
	(backward-char (* len (if sokuon 2 1)))
	(skk-set-marker skk-okurigana-start-point (point)))
      (setq skk-okuri-char (skk-okurigana-prefix okuri))
      (skk-set-okurigana))))

;;;###autoload
(defun skk-kanagaki-set-okurigana-no-sokuon (&optional arg)
  "$B%]%$%s%H$ND>A0$NJ8;z$rAw$j2>L>$H8+Jo$7$F!"JQ49$r3+;O$9$k!#(B"
  (interactive "*p")
  (skk-kanagaki-set-okurigana (if (eq (prefix-numeric-value arg) 4) nil t)))

(defun skk-kanagaki-initialize ()
  "SKK $B5/F0;~$NE,Ev$J%?%$%_%s%0$G2>L>F~NOMQ$N@_Dj$r9T$&!#(B"
  ;; $B<B:]$K$O(B `skk-regularize' $B$N<B9T8e!"(BSKK $B$N4pK\%k!<%k$,(B compile $B$5$l$?8e(B
  ;; $B$K8F$P$l$k!#(B
  (static-when (memq skk-emacs-type '(nemacs mule1))
    ;; Nemacs $B$N(B canna.el $B$h$j0zMQ!#(B
    (if (not (keymapp (global-key-binding "\e[")))
	(global-unset-key "\e[")))
  ;; $BI,MW$J%b%8%e!<%k$r%m!<%I!#(B
  (when skk-kanagaki-keyboard-type
    (require (intern (format "skk-%s" skk-kanagaki-keyboard-type))))
  ;; $B%-!<%P%$%s%I!#$?$@$7$3$l$O!"$h$jE,@Z$J%-!<Dj5A$r8+$D$1$k$^$G$N;CDjE*=hCV!#(B
  ;; $B$3$3$G8@$&!V$h$jE,@Z$J%-!<Dj5A!W$H$O!"F~NOJ}<0$K0MB8$9$k$?$a!"(BSKK $B$N=EMW(B
  ;; $B$J%-!<Dj5A$r%U%!%s%/%7%g%s%-!<$K;D$7$F$*$/$3$H$O!"<BMQ$N$?$a$h$j$b$`$7$m(B
  ;; $B;29M$N$?$a!#(B
  (dolist
      (cell
       '((skk-kanagaki-set-henkan-point-key . skk-set-henkan-point-subr)
	 (skk-kanagaki-abbrev-mode-key . skk-abbrev-mode)
	 (skk-kanagaki-katakana-mode-key . skk-toggle-kana)
	 (skk-kanagaki-latin-jisx0208-mode-key . skk-jisx0208-latin-mode)
	 (skk-kanagaki-hankaku-mode-key . skk-toggle-katakana)
	 (skk-kanagaki-latin-mode-key . skk-latin-mode)
	 (skk-kanagaki-code-input-key . skk-input-by-code-or-menu)
	 (skk-kanagaki-toggle-rom-kana-key . skk-kanagaki-toggle-rom-kana)
	 (skk-kanagaki-midashi-henkan-key . skk-kanagaki-midashi-henkan)
	 (skk-kanagaki-previous-candidate-key . skk-previous-candidate)))
    (when (and (symbol-value (car cell)) (commandp (cdr cell)))
      (define-key skk-j-mode-map
	(symbol-value (car cell)) (cdr cell))))
  ;;
  (let ((char (and (stringp skk-kanagaki-previous-candidate-key)
		   (string-to-char skk-kanagaki-previous-candidate-key))))
    (when (eq skk-previous-candidate-char ?x)
      ;; $B4{DjCM$N$^$^$G$"$k$H$-!"E,@Z$K@_Dj$9$k!#(B
      (setq skk-previous-candidate-char (or char
					    ;; C-p
					    (int-char 16)))))
  ;;
  (define-key help-map skk-kanagaki-help-key 'skk-kanagaki-help)
  ;;
  (static-unless (memq skk-emacs-type '(nemacs mule1))
    (eval-after-load "skk-jisx0201"
      '(when skk-kanagaki-hankaku-mode-key
	 (define-key skk-jisx0201-mode-map skk-kanagaki-hankaku-mode-key
	   'skk-toggle-katakana))))
  ;;
  (define-key skk-j-mode-map skk-kanagaki-start-henkan-key
    'skk-kanagaki-insert)
  ;;
  (unless skk-kanagaki-base-rule-list
    (setq skk-kanagaki-base-rule-list
	  (symbol-value (intern (format "skk-kanagaki-%s-base-rule-list"
				       skk-kanagaki-keyboard-type)))))
  (setq skk-kanagaki-rule-tree
	(skk-compile-rule-list
	 skk-kanagaki-base-rule-list skk-kanagaki-rule-list))
  (setq skk-kanagaki-rom-kana-rule-tree skk-rule-tree)
  ;;
  (add-hook 'skk-mode-hook
	    (function
	     (lambda ()
	       (skk-kanagaki-adjust-rule-tree))))
  ;; $B6gFIE@F~NO;~$NLdBj$r2sHr!#(B $BF|K\8l(B 106 $B%-!<%\!<%I$G$O(B "<" $B$H(B ">" $B$K$h$k@\(B
  ;; $BHx<-$NF~NO$O$G$-$J$/$J$k!#(B "?" $B$K$h$k@\Hx<-$NF~NO$O$G$-$k!#(B
  (dolist (char skk-special-midashi-char-list)
    (when (and skk-use-kana-keyboard
	       (memq
		(nth 2 (assoc (skk-char-to-string char)
			      (symbol-value
			       (intern (format "skk-kanagaki-%s-base-rule-list"
					       skk-kanagaki-keyboard-type)))))
		'(skk-current-kuten skk-current-touten)))
      (setq skk-special-midashi-char-list
	    (delq char skk-special-midashi-char-list)))))

;; Pieces of advice.

(defadvice skk-insert (around skk-kanagaki-ad activate compile)
  "$B2>L>F~NOMQ$N(B work around $B!#(B"
  ;;
  (when (and skk-process-okuri-early
	     (eq skk-kanagaki-state 'kana))
    ;; `skk-process-okuri-early' $B$,I{:nMQ$r;}$D$+$bCN$l$J$$!#2>L>F~NO$G$O$=$b(B
    ;; $B$=$b0UL#$N$J$$%*%W%7%g%s$J$N$G6/@)E*$K(B off $B$K$9$k!#(B
    (setq skk-process-okuri-early nil))
  ;;
  (if (eq  skk-kanagaki-state 'kana)
      (let (skk-set-henkan-point-key)
	ad-do-it)
    ad-do-it))

(defadvice skk-compute-henkan-lists (around skk-kanagaki-ad activate)
  (let ((okurigana (ad-get-arg 0)))
    (if (not okurigana)
	(setq ad-return-value
	      (list (split-string (buffer-substring-no-properties
				   (point) (progn (end-of-line) (1- (point))))
				  "/") nil nil nil))
      (save-match-data
	(let ((stage 1) (q1 (queue-create)) (q2 (queue-create))
	      (q3 (queue-create)) (q4 (queue-create))
	      (okuri-key (concat "\[" okurigana)) item headchar)
	  (catch 'exit
	    (while (not (eolp))
	      (setq item (buffer-substring-no-properties
			  (point)
			  (1- (search-forward "/")))
		    headchar (if (string= item "")
				 (int-char 0)
			       (skk-str-ref item 0)))
	      (cond ((and (eq headchar ?\[) (<= stage 2))
		     ;;
		     (when (and skk-use-kana-keyboard
				skk-henkan-okuri-strictly)
		       ;; $B2>L>F~NOMQ$NFC<l=hM}(B
		       (cond
			((eq skk-kanagaki-state 'kana)
			 ;; okuri-key $B$,(B "$B$C(B" $B$G(B item $B$,(B "$B$C$F(B" $B$J$I$@$C$?(B
			 ;; $B>l9g!"(Bitem $B$r(B okuri-key $B$KCV$-49$($k!#(B
			 (when (and
				(not (string= okuri-key item))
				(string-match
				 (concat "^" (regexp-quote okuri-key)) item))
			   (setq item okuri-key)))
			((eq skk-kanagaki-state 'rom)
			 ;; okuri-key $B$,(B "$B$C$F(B" $B$G(B item $B$,(B "$B$C(B" $B$J$I$@$C$?(B
			 ;; $B>l9g!"(Bitem $B$r(B okuri-key $B$KCV$-49$($k!#(B
			 (when (and
				(not (string= okuri-key item))
				(string-match
				 (concat "^" (regexp-quote item)) okuri-key))
			   (setq item okuri-key)))))
		     ;;
		     (if (string= item okuri-key)
			 (progn (queue-enqueue q2 item)
				(setq stage 3))
		       (setq stage 2)
		       (queue-enqueue q2 item)))
		    ((= stage 1)
		     (queue-enqueue q1 item))
		    ((= stage 2)
		     (queue-enqueue q2 item))
		    ((= stage 3)
		     (if (eq headchar ?\]) ; ?\]
			 (progn (setq stage 4)
				(queue-enqueue q4 item))
		       (queue-enqueue q3 item)))
		    ((= stage 4)
		     (queue-enqueue q4 item)))))
	  (setq ad-return-value
		(list (queue-all q1)
		      (queue-all q2)
		      (queue-all q3)
		      (queue-all q4))))))))

;;

(require 'product)
(product-provide (provide 'skk-kanagaki) (require 'skk-version))

;;; skk-kanagaki.el ends here
