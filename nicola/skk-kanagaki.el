;;; skk-kanagaki.el --- SKK $B$N2>L>F~NO%5%]!<%H(B
;; Copyright (C) 2000 Tetsuo Tsukamoto <czkmt@remus.dti.ne.jp>

;; Author: Tetsuo Tsukamoto <czkmt@remus.dti.ne.jp>
;; Keywords: hardware, japanese

;; This file is part of SKK (Simple Kana to Kanji conversion program).

;; SKK  is free software;  you  can redistribute it  and/or modify it under the
;; terms  of the GNU General Public License  as published  by the Free Software
;; Foundation;  either versions  2,  or  (at your option)  any  later version.

;; SKK  is distributed  in the hope  that  it will  be useful  but  WITHOUT ANY
;; WARRANTY;  without even the implied  warranty  of MERCHANTABILITY or FITNESS
;; FOR  A  PARTICULAR  PURPOSE.  See  the  GNU General Public License  for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; SKK,  see the file COPYING.  If not,  write  to the Free Software Foundation
;; Inc., 59 Temple Place - Suite 330, Boston,  MA 02111-1307, USA.

;;; Commentary:
;;
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
;; $B$3$N%W%m%0%i%`$O(B  SKK $B$K$*$$$F%m!<%^;zF~NO$J$i$L2>L>F~NO$r%5%]!<%H$9$k$3$H$r(B
;; $BL\E*$H$7$^$9!#(B AT $B8_495!MQ$NF|K\8l(B 106 $B%-!<%\!<%I$OIaDL(B JIS $BG[Ns$N9o0u$,$"$j(B
;; $B$^$9$,!"$^$:$O$3$l$KBP1~$9$kM=Dj$G$9!#(BPC-98 $B$X$NBP1~$O$3$l$r>/$7JQ99$9$l$P$G(B
;; $B$-$k$H;W$$$^$9!#(B
;;
;;  -*- $BLdBjE@(B -*-
;;
;; 1. Emacs Lisp $B$N%l%Y%k$G$NLdBj(B
;;
;; $B2>L>F~NO$K$*$$$F$O(B SHIFT $B%-!<$rMxMQ$7$FF~NO$5$l$k2>L>$b$"$k$?$a!"(B SKK $BK\Mh$N(B
;; SHIFT $B$N;H$$J}$,$G$-$^$;$s!#$=$NB>$$$m$$$m(B SKK $B$i$7$/$J$$$N$G$9$,!"(B $B$H$j$"$((B
;; $B$:!"(B
;;
;;   o $BJQ493+;OE@$N;XDj$O2>L>F~NO$H$OJL$K9T$&!#(B
;;   o $BJQ49$N3+;O$ODL>oDL$j!"(B [SPC] $B$G;X<($9$k!#(B $B$?$@$7!"Aw$j$"$j$NJQ49$N$H$-$O(B
;;     C-u [SPC] $B$H$7$FL@<($9$k!#(B
;;
;; $B$N$h$&$K$7$F$"$j$^$9!#Nc$($P!"!V4r$7$$!W$rF~NO$9$k$?$a$K$O!"(B
;;
;; `Q' $B$&$l$7(B C-u [SPC] $B$$(B
;;
;; $B$N$h$&$KBG$A$^$9!#(B
;; ($B2~A1$NM>CO$,$"$k$H;W$$$^$9$,!"$H$j$"$($:%"%$%G%"$,$3$3$G?T$-$F$$$^$9!#(B)
;;
;; 2. $B%7%9%F%`%l%Y%k$G$NLdBj(B
;;
;; $BBh(B 2 $B$NLdBjE@$H$7$F!"(B $B%-!<G[Ns$N@_Dj$K$h$j9o0uDL$j$NF~NO$,$G$-$J$$>l9g$,$"$j(B
;; $B$^$9!#Nc$($PF|K\8l(B 106 $B%-!<%\!<%I;HMQ;~!"(BXFree86 $B>e$G$O(B
;;
;; o $B!V!o!W%-!<(B ($B2>A[%-!<%3!<%I(B 133)
;; o $B!V!@!W%-!<(B ($B2>A[%-!<%3!<%I(B 123)
;;
;; $B$O$$$:$l$b(B backslash $B$H$7$F07$o$l$^$9!#(B $B$7$+$72>L>F~NO$K$*$$$FA0<T$O(B $B!V!<!W!"(B
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
;; $B$5$i$K!"$b$7$"$J$?$,(B PC-98 $B%f!<%6(B $B$G(B XEmacs $B$N%Y!<%?%F%9%?!<$J$i$P!"$*$b$`$m(B
;; $B$K!V$+$J!W%-!<$r%m%C%/$7$F$_$F$/$@$5$$!#(B ;')
;;
;;  -*- $B;H$$J}(B -*-
;;
;; $BAw$j$J$7$NJQ49$K$D$$$F$O!"DL>o$N(B SKK $B$HF1MM$J$N$G>JN,$7$^$9!#(B
;;
;; 1. $BJQ493+;OE@$N;XDj(B
;;
;; $BDL>o$N(B SKK $B$K$*$$$F$O!"(B SHIFT $B$r2!$7$J$,$iF~NO$9$k$3$H$GJQ493+;O0LCV$rL@<($7(B
;; $B$F$$$^$7$?$,!"2>L>F~NO$G$O$3$l$,$G$-$^$;$s!#$=$3$G!"(B"Q" $B$r2!$9$3$H$GJQ493+;O(B
;; $B0LCV$r;XDj$9$kI,MW$,$"$j$^$9!#(B $BMW$O!"(Babbrev $B%b!<%I$HF1MM$N$d$jJ}$GJQ49$r3+;O(B
;; $B$9$k$3$H$K$J$j$^$9!#Nc$($P!V=U!W$NF~NO$O(B
;;
;; `Q' $B$O$k(B $B"M(B $B"&$O$k(B [SPC] $B"M(B $B"'=U(B
;;
;; $B$^$?$O(B
;;
;; $B$O$k(B ^B^B `Q' $B"M(B $B"&$O$k(B ^F^F [SPC] $B"M(B $B"'=U(B
;;
;; $B$N$$$:$l$+$G$G$-$^$9!#(B
;;
;; 2. $BAw$j$"$j$NJQ49$N$7$+$?(B
;;
;; $BDL>o$N(B SKK $B$K$*$$$F$O!"(B SHIFT $B$r2!$7$J$,$iF~NO$9$k$3$H$GAw$j2>L>$N0LCV$rL@<((B
;; $B$7$F$$$^$7$?!#2>L>F~NO(B SKK $B$K$*$$$F$O$=$l$O$G$-$^$;$s!#$=$3$G(B
;;
;; o `S' $B$,2!$5$l$?$H$-$K!"(B $BD>A0$N(B 1 $BJ8;z$rAw$j2>L>$H8+Jo$7$FJQ49$r3+;O$9$k!#(B
;;
;; $B$H$$$&Iw$K$7$F$_$^$7$?!#(B $BNc$($P!"!VC#$9!W$HF~NO$7$?$$>l9g$O(B
;;
;; $B"&$?$C$9(B `S'  $B"M(B $B"'C#$9(B
;;
;; $B$N$h$&$K$J$j$^$9!#!VBT$C$F!W$HF~NO$7$?$$>l9g$O(B
;;
;; $B"&$^$C(B `S' $B"M(B $B"'BT$C(B
;;
;; $B$H$7$F$+$i!V$F!W$rF~NO$7$^$9!#(B
;;
;; ($BDI5-(B)
;;
;; `S' $B$NBe$o$j$K(B C-u [SPC] $B$G$bAw$j$"$jJQ49$,$G$-$^$9!#(B
;;
;; 3. $B$$$/$D$+$N=EMW$J%-!<Dj5A$K$D$$$F(B
;;
;; $B%+%JF~NO$,(B $B!V(Bq$B!W!"(B abbrev $B%b!<%I$,(B $B!V(B/$B!W!"(Blatin $B%b!<%I$,(B $B!V(Bl$B!W$J$I$ODjHV$G$9(B
;; $B$,!"2>L>F~NO$G$O$3$l$b;H$($^$;$s!#2>L>F~NO$G$-$k%-!<%\!<%I$G%U%!%s%/%7%g%s%-(B
;; $B!<$,;H$($J$$$3$H$O$"$^$jL5$$$@$m$&$H;W$$!"%G%U%)%k%H$G$O$3$l$i$r%U%!%s%/%7%g(B
;; $B%s%-!<$KB`Hr$7$F$_$^$7$?!#%G%U%)%k%H$G$O0J2<$N$h$&$K$J$C$F$$$^$9!#%f!<%6%*%W(B
;; $B%7%g%s$J$N$G<+M3$KJQ99$G$-$^$9!#(B
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
;;
;; $B$3$l$i$N%-!<Dj5A$O0l;~E*$J$b$N$G$9!#$h$j$h$$%-!<Dj5A$r9M$($F=$@5$9$kM=Dj$G$"(B
;; $B$j!"%f!<%6$N$40U8+$K4|BT$7$F$$$^$9!#(B
;;
;; {TODO}
;;
;; o $B8DJL$N%-!<%\!<%IG[Ns$X$NBP1~%3!<%I$O$=$l$>$lJL%U%!%$%k$K$7$F!"%b%8%e!<%kE*(B
;;   $B$K%m!<%I$9$k$3$H$K$7$?!#$=$3$G!":#8e$O$G$-$k$@$1%b%8%e!<%k$rDI2C$9$k!#(B
;; o $B$H$j$"$($:!"(BNICOLA $BG[Ns$H(B omelet $BFH<+G[Ns$X$NBP1~$r40N;$7$?$$!#(B

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
*Non-nil $B$J$i2>L>F~NOMQ$N@_Dj$r%m!<%I$9$k!#(B
SKK $B;HMQCf$K$3$NJQ?t$NCM$r@Z$jBX$($k$3$H$G(B  $B%m!<%^;zF~NO(B $B"N(B $B2>L>F~NO(B $B$N%H%0%k$,(B
$B$G$-$k!#(B"
  :type 'boolean
  :group 'skk
  :group 'skk-kanagaki)

(defcustom skk-kanagaki-keyboard-type '106-jis "\
*$B2>L>F~NO$K;HMQ$9$k%-!<%\!<%I$N%?%$%W!#(B
$BCM$OG$0U$N%7%s%\%k!#(B $B$?$@$7(B  `skk-kanagaki-{$B%7%s%\%kL>(B}-base-rule-list'  $B$H$$$&(B
$BJQ?t$rMQ0U$7$J$1$l$P$J$i$J$$!#%G%U%)%k%H$G$OF|K\8l(B 106 $B%-!<%\!<%IMQ$N@_Dj$rMQ0U(B
$B$7!"$3$l$r;HMQ$9$k!#(B"
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
*$B$3$N%-!<$r2!$9$3$H$G(B $B%m!<%^;zF~NO(B $B"N(B $B2>L>F~NO$N%H%0%k$,$G$-$k!#(B"
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

(defcustom skk-kanagaki-previous-candidate-key "x" "\
*$BA08uJd$rI=<($9$k$?$a$N%-!<!#(B
XFree86 $B>e$G;HMQ$9$k>l9g!"(B $BNc$($P$3$NCM$r(B [henkan]  (XEmacs $B$G$O(B [henkan-mode])
$B$K$9$l$P!"F|K\8l%-!<%\!<%I$N(B [$BA08uJd(B] $B%-!<$K3d$jEv$F$k$3$H$,$G$-$k!#(B $BF1%-!<$O!"(B
Mule2.3@19.28 $B$G$O(B [key-35]$B!"(BMule2.3@19.34 $B$G$O(B [numbersign] ($B$J$<(B ??) $B$H$J$k$i(B
$B$7$$!#(B"
  :type 'sexp
  :group 'skk-kanagaki)

(defcustom skk-kanagaki-start-henkan-key " " "\
*$BJQ49$r3+;O$9$k$?$a$N%-!<!#(B"
  :type 'sexp
  :group 'skk-kanagaki)

(defcustom skk-kanagaki-rule-list nil "\
*$B%-!<F~NO$KBP$9$kJQ49J8;z$N5,B'$G!"%f!<%6!<$NDI2C$N@_Dj$r9T$J$&$b$N!#(B
$BNc$($P!"(B $B2>A[%-!<%3!<%I$KBP$9$k%7%s%\%k$rFH<+$K@_Dj$7$F$$$k>l9g$J$I$O!"$3$NJQ?t(B
$B$rMQ$$$F$=$l$KBP1~$7$?@_Dj$r$9$k$3$H$,$G$-$k!#(B"
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

(defcustom skk-kanagaki-jidou-key-symbol-kakikae-service nil "\
*Non-nil $B$J$i2>L>F~NO$N$?$a$K>!<j$K%-!<%F!<%V%k$r=q49$($k!#(B
X $B>e$G(B xmodmap $B$,%$%s%9%H!<%k$5$l$F$$$k>l9g$@$1M-8z!#F0:n$,2~A1$5$l$kBe$o$j$K!"(B
$BB>$N%b!<%I$d%"%W%j%1!<%7%g%s$K$b(B $B%-!<%F!<%V%k$NJQ99$,1F6A$9$k$H$$$&I{:nMQ$,$"$k(B
$B$N$G!"==J,Cm0U$7$F;H$C$F$/$@$5$$!#(B"
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

(defvar skk-kanagaki-temp-dir
  (static-cond
   ((fboundp 'temp-directory)
    (temp-directory))
   ((and (boundp 'temporary-file-directory) temporary-file-directory)
    temporary-file-directory)
   (t
    (or (getenv "TMP") "/tmp"))))

(defvar skk-kanagaki-isearch-buffer nil)

(skk-deflocalvar skk-kanagaki-state 'kana)

;; Functions.

(defalias-maybe 'help-mode 'fundamental-mode)

;;;###autoload
(defun skk-kanagaki-toggle-rom-kana (&optional arg)
  "$B%m!<%^;zF~NO(B $B"N(B $B2>L>F~NO(B $B$r@Z$jBX$($k!#(B"
  (interactive)
  (setq skk-kanagaki-state
	(if (memq arg '(kana rom))
	    arg
	  (case skk-kanagaki-state
	    (kana 'rom)
	    (rom 'kana)
	    ;; $B$H$j$"$($:!#(B
	    (t 'kana)))))

;;;###autoload
(defun skk-kanagaki-midashi-henkan (&optional arg)
  "$B@\F,<-$^$?$O@\Hx<-JQ49$r$9$k!#(B"
  (interactive "*p")
  (cond (skk-henkan-active
	 (skk-kakutei)
	 (skk-set-henkan-point-subr)	
	 (insert ?>))
	((and skk-henkan-on (not skk-henkan-active))
	 (insert ?>)
	 (skk-set-marker skk-henkan-end-point (point))
	 (setq skk-henkan-count 0
	       skk-henkan-key (buffer-substring
			       skk-henkan-start-point (point)))
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
  (cond ((eq arg 1) (skk-insert arg))
	;; C-u [SPC] $B$GAw$j$"$jJQ49$r$9$k!#(B
	(t (skk-kanagaki-set-okurigana-no-sokuon t))))

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
      (setq skk-okuri-char
	    (cond ((or sokuon (member okuri '("$B$C(B" "$B%C(B")))
		   "t")
		  (t
		   (let ((skk-henkan-okurigana okuri))
		     (skk-okurigana-prefix okuri)))))
      (skk-set-okurigana))))

;;;###autoload
(defun skk-kanagaki-set-okurigana-no-sokuon (&optional arg)
  "$B%]%$%s%H$ND>A0$NJ8;z$rAw$j2>L>$H8+Jo$7$F!"JQ49$r3+;O$9$k!#(B"
  (interactive "*p")
  (skk-kanagaki-set-okurigana (if (eq (prefix-numeric-value arg) 4) nil t)))

;; Pieces of advice.

(defadvice skk-adjust-user-option (before skk-kanagaki-ad activate compile)
  "SKK $B5/F0;~$NE,Ev$J%?%$%_%s%0$G2>L>F~NOMQ$N@_Dj$r9T$&!#(B"
  ;;
  (static-when (memq skk-emacs-type '(nemacs mule1))
    ;; Nemacs $B$N(B canna.el $B$h$j0zMQ!#(B
    (if (not (keymapp (global-key-binding "\e[")))
	(global-unset-key "\e[")))
  ;; $BI,MW$J%b%8%e!<%k$r%m!<%I!#(B
  (when skk-kanagaki-keyboard-type
    (require (intern (format "skk-%s" skk-kanagaki-keyboard-type))))
  ;; $B%-!<%P%$%s%I!#$?$@$7$3$l$O!"$h$jE,@Z$J%-!<Dj5A$r8+$D$1$k$^$G$N;CDjE*=hCV!#(B
  (let ((list
	 '((skk-kanagaki-set-henkan-point-key . skk-set-henkan-point-subr)
	   (skk-kanagaki-abbrev-mode-key . skk-abbrev-mode)
	   (skk-kanagaki-katakana-mode-key . skk-toggle-kana)
	   (skk-kanagaki-latin-jisx0208-mode-key . skk-jisx0208-latin-mode)
	   (skk-kanagaki-hankaku-mode-key . skk-toggle-katakana)
	   (skk-kanagaki-latin-mode-key . skk-latin-mode)
	   (skk-kanagaki-code-input-key . skk-input-by-code-or-menu)
	   (skk-kanagaki-toggle-rom-kana-key . skk-kanagaki-toggle-rom-kana)
	   (skk-kanagaki-midashi-henkan-key . skk-kanagaki-midashi-henkan)
	   (skk-kanagaki-previous-candidate-key . skk-previous-candidate))))
    (while list
      (let ((cons (car list)))
	(when (and (symbol-value (car cons)) (commandp (cdr cons)))
	  (define-key skk-j-mode-map
	    (symbol-value (car cons)) (cdr cons))))
      (setq list (cdr list))))
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
  (setq skk-kanagaki-rom-kana-rule-tree skk-rule-tree))

(defadvice skk-insert (around skk-kanagaki-ad activate compile)
  "$B2>L>F~NOMQ$N(B work around $B!#(B"
  (when (and (skk-local-variable-p 'skk-jisyo (current-buffer))
	     (equal skk-jisyo "~/skk-tut-jisyo")
	     (not (eq skk-kanagaki-state 'rom)))
    (skk-kanagaki-toggle-rom-kana 'rom))
  (let* ((list (copy-sequence skk-special-midashi-char-list))
	 (skk-special-midashi-char-list
	  ;; $B6gFIE@F~NO;~$NLdBj$r2sHr!#(B $BF|K\8l(B 106 $B%-!<%\!<%I$G$O(B "<" $B$H(B ">" $B$K(B
	  ;; $B$h$k@\Hx<-$NF~NO$O$G$-$J$/$J$k!#(B "?" $B$K$h$k@\Hx<-$NF~NO$O$G$-$k!#(B
	  (cond
	   ((and
	     skk-use-kana-keyboard
	     (memq last-command-char list)
	     (memq
	      (nth 2 (assoc (skk-char-to-string last-command-char)
			    (symbol-value
			     (intern (format "skk-kanagaki-%s-base-rule-list"
					     skk-kanagaki-keyboard-type)))))
	      '(skk-current-kuten skk-current-touten)))
	    (delq last-command-char list))
	   (t
	    list))))
    (case skk-kanagaki-state
      (kana
       (unless (equal skk-rule-tree skk-kanagaki-rule-tree)
	 (make-local-variable 'skk-rule-tree)
	 (setq skk-rule-tree skk-kanagaki-rule-tree))
       (let (skk-set-henkan-point-key)
	 ad-do-it))
      (rom
       (unless (equal skk-rule-tree skk-kanagaki-rom-kana-rule-tree)
	 (make-local-variable 'skk-rule-tree)
	 (setq skk-rule-tree skk-kanagaki-rom-kana-rule-tree))
       ad-do-it)
      (t nil))))

(defadvice skk-okurigana-prefix (around skk-knagaki-ad activate compile)
  (if (eq skk-kanagaki-state 'kana)
      (if (member (ad-get-arg 0) '("$B$C(B" "$B%C(B"))
	  "t"
	(let ((skk-henkan-okurigana (ad-get-arg 0)))
	  ad-do-it))
    ad-do-it))

(defadvice skk-isearch-wrapper (around skk-kanagaki-ad activate)
  (setq skk-kanagaki-isearch-buffer (current-buffer))
  ad-do-it
  (setq skk-kanagaki-isearch-buffer nil))

;;

(require 'product)
(product-provide (provide 'skk-kanagaki) (require 'skk-version))

;;; skk-kanagaki.el ends here
