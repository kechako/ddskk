;;; skk-kanagaki.el --- SKK $B$N2>L>F~NO%5%]!<%H(B
;; Copyright (C) 2000 Tetsuo Tsukamoto <czkmt@remus.dti.ne.jp>

;; Author: Tetsuo Tsukamoto <czkmt@remus.dti.ne.jp>
;; Version: $Id: skk-kanagaki.el,v 1.1.2.1 2000/08/07 12:58:55 czkmt Exp $
;; Keywords: japanese
;; Last Modified: $Date: 2000/08/07 12:58:55 $

;; This file is not yet part of Daredevil SKK.

;; Daredevil SKK  is free software;  you  can redistribute it  and/or modify it
;; under the terms  of the GNU General Public License  as published by the Free
;; Software  Foundation;  either versions  2,  or  (at your option)  any  later
;; version.

;; Daredevil SKK is distributed in the hope that it will be useful  but WITHOUT
;; ANY  WARRANTY;  without  even  the implied  warranty  of MERCHANTABILITY  or
;; FITNESS  FOR  A PARTICULAR PURPOSE.  See the GNU General Public License  for
;; more details.

;; You should have received a copy of the GNU General Public License along with
;; Daredevil SKK,  see the file COPYING.  If not,  write  to  the Free Software
;; Foundation Inc., 59 Temple Place - Suite 330, Boston,  MA 02111-1307, USA.

;;; Commentary:
;;
;; {$B;H$$$+$?(B ($B;CDj%P!<%8%g%s(B)}
;;
;; ~/.skk $B$K(B
;;
;; (require 'skk-kanagaki)
;;
;; $B$H=q$/!#(B
;;
;; {$B@bL@(B}
;;
;; $B$3$N%W%m%0%i%`$O(B  SKK $B$K$*$$$F%m!<%^;zF~NO$J$i$L2>L>F~NO$r%5%]!<%H$9$k$3$H$r(B
;; $BL\E*$H$7$^$9!#(B AT $B8_495!MQ$NF|K\8l(B 106 $B%-!<%\!<%I$OIaDL(B JIS $BG[Ns$N9o0u$,$"$j(B
;; $B$^$9$,!"$^$:$O$3$l$KBP1~$9$kM=Dj$G$9!#(B
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
;; [f2] $B$&$l$7(B C-u [SPC] $B$$(B
;;
;; $B$N$h$&$KBG$A$^$9!#(B
;; ($B2~A1$NM>CO$,$"$k$H;W$$$^$9$,!"$H$j$"$($:%"%$%G%"$,$3$3$G?T$-$F$$$^$9!#(B)
;;
;; $BBh(B 2 $B$NLdBjE@$H$7$F!"(B $B%-!<%7%s%\%k$N@_Dj$K$h$j9o0uDL$j$NF~NO$,$G$-$J$$>l9g$,(B
;; $B$"$j$^$9!#Nc$($PF|K\8l(B 106 $B%-!<%\!<%I;HMQ;~!"(BXFree86 $B>e$G$O(B
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

;;; Code:

(eval-when-compile (require 'skk-macs) (require 'skk-vars) (require 'static))

(defgroup skk-kanagaki nil "SKK kanagaki related customization."
  :prefix "skk-kanagaki-"
  :group 'skk)

;; Variables.
(defcustom skk-use-kana-keyboard t "\
*Non-nil $B$J$i2>L>F~NOMQ$N@_Dj$r%m!<%I$9$k!#(B
SKK $B;HMQCf$K$3$NJQ?t$NCM$r@Z$jBX$($k$3$H$G(B  $B%m!<%^;zF~NO(B $B"+"*(B $B2>L>F~NO(B $B$N%H%0%k(B
$B$,$G$-$k!#(B"
  :type 'boolean
  :group 'skk
  :group 'skk-kanagaki)

(defcustom skk-kanagaki-keyboard-type '106-jis "\
*$B2>L>F~NO$K;HMQ$9$k%-!<%\!<%I$N%?%$%W!#(B
$BCM$OG$0U$N%7%s%\%k!#(B $B$?$@$7(B  `skk-kanagaki-{$B%7%s%\%kL>(B}-base-rule-list'  $B$H$$$&(B
$BJQ?t$rMQ0U$7$J$1$l$P$J$i$J$$!#%G%U%)%k%H$G$OF|K\8l(B 106 $B%-!<%\!<%IMQ$N@_Dj$rMQ0U(B
$B$7!"$3$l$r;HMQ$9$k!#(B"
  :type 'symbol
  :group 'skk-kanagaki)

(defcustom skk-kanagaki-set-henkan-point-key [f2] "\
*$B$3$N%-!<$r2!$9$3$H$GJQ493+;O0LCV$r@_Dj$9$k!#(B
$BJQ493+;O0LCV$N@_Dj$O2>L>$rF~NO$9$kA0$K$*$3$J$C$F$b!"(B $BF~NO$7=*$o$C$?8e$G$*$3$J$C(B
$B$F$b9=$o$J$$!#(B"
  :type 'sexp
  :group 'skk-kanagaki)

(defcustom skk-kanagaki-abbrev-mode-key [f6] "\
*$B$3$N%-!<$r2!$9$3$H$G(B abbrev $B%b!<%I$KF~$k!#(B"
  :type 'sexp
  :group 'skk-kanagaki)

(defcustom skk-kanagaki-katakana-mode-key [f7] "\
*$B$3$N%-!<$r2!$9$3$H$G%+%J%b!<%I$H$+$J%b!<%I$r@Z$j$+$($k!#(B
$BJQ493+;O0LCV$N@_Dj8e$K2!$9$3$H$GBP>]J8;zNs$r%+%J$KJQ49$9$k$3$H$b$G$-$k!#(B"
  :type 'sexp
  :group 'skk-kanagaki)

(defcustom skk-kanagaki-latin-jisx0208-mode-key [f8] "\
*$B$3$N%-!<$r2!$9$3$H$GA43Q1Q?t%b!<%I$KF~$k!#(B"
  :type 'sexp
  :group 'skk-kanagaki)

(defcustom skk-kanagaki-hankaku-mode-key [f9] "\
*$B$3$N%-!<$r2!$9$3$H$GH>3Q%+%J%b!<%I$K@Z$j$+$($k!#(B
$BJQ493+;O0LCV$N@_Dj8e$K2!$9$3$H$GBP>]J8;zNs$rH>3Q%+%J$KJQ49$9$k$3$H$b$G$-$k!#(B"
  :type 'sexp
  :group 'skk-kanagaki)

(defcustom skk-kanagaki-latin-mode-key [f10] "\
*$B$3$N%-!<$r2!$9$3$H$G(B latin $B%b!<%I$KF~$k!#(B"
  :type 'sexp
  :group 'skk-kanagaki)

(defcustom skk-kanagaki-code-input-key [f5] "\
*$B$3$N%-!<$r2!$9$3$H$G%3!<%IF~NO$,$G$-$k!#(B"
  :type 'sexp
  :group 'skk-kanagaki)

(defcustom skk-kanagaki-previous-candidate-key "x" "\
*$BA08uJd$rI=<($9$k$?$a$N%-!<!#(B
XFree86 $B>e$G;HMQ$9$k>l9g!"(B $BNc$($P$3$NCM$r(B [henkan]  (XEmacs $B$G$O(B [henkan-mode])
$B$K$9$l$P!"F|K\8l%-!<%\!<%I$N(B [$BA08uJd(B] $B%-!<$K3d$jEv$F$k$3$H$,$G$-$k!#(B $BF1%-!<$O!"(B
Mule2.3@19.28 $B$G$O(B [key-35]$B!"(BMule2.3@19.34 $B$G$O(B [numbersign] $B$H$J$k$i$7$$!#(B"
  :type 'sexp
  :group 'skk-kanagaki)

(defcustom skk-kanagaki-rule-list nil "\
*$B%-!<F~NO$KBP$9$kJQ49J8;z$N5,B'$G!"%f!<%6!<$NDI2C$N@_Dj$r9T$J$&$b$N!#(B
$BNc$($P!"2>A[%-!<%3!<%I$KBP$9$k%7%s%\%k$rFH<+$K@_Dj$7$F$$$k>l9g$J$I$O!"(B $B$3$NJQ?t(B
$B$rMQ$$$F$=$l$KBP1~$7$?@_Dj$r$9$k$3$H$,$G$-$k!#(B"
  :type '(repeat
	  (list :tag "Rule"
		(string :tag "1 (string)")
		(choice :tag "2 (choice)"
			string
			(const nil))
		(choice :tag "3 (choice)"
			(symbol :tag "Function")
			string
			(cons (string :tag "3-1 (string)")
			      (string :tag "3-2 (string)")))))
  :group 'skk-kanagaki)

(defcustom skk-kanagaki-jidou-key-symbol-kakikae-service nil "\
*Non-nil $B$J$i9o0u$I$*$j$N2>L>F~NO$N$?$a$K>!<j$K%-!<%7%s%\%k$r=q49$($k!#(B
$BI{:nMQ$K$4Cm0U$/$@$5$$!#(B :)"
  :type '(choice (const 106-jis)
		 (const 106-jis-kodawari)
		 (const nil))
  :group 'skk-kanagaki)

;; Internal constants and variables.
(defconst skk-kanagaki-106-jis-base-rule-list
  '(("1" nil ("$B%L(B" . "$B$L(B")) ("2" nil ("$B%U(B" . "$B$U(B")) ("3" nil ("$B%"(B" . "$B$"(B"))
    ("4" nil ("$B%&(B" . "$B$&(B")) ("5" nil ("$B%((B" . "$B$((B")) ("6" nil ("$B%*(B" . "$B$*(B"))
    ("7" nil ("$B%d(B" . "$B$d(B")) ("8" nil ("$B%f(B" . "$B$f(B")) ("9" nil ("$B%h(B" . "$B$h(B"))
    ("0" nil ("$B%o(B" . "$B$o(B")) ("-" nil ("$B%[(B" . "$B$[(B")) ("^" nil ("$B%X(B" . "$B$X(B"))
    ("q" nil ("$B%?(B" . "$B$?(B")) ("w" nil ("$B%F(B" . "$B$F(B")) ("e" nil ("$B%$(B" . "$B$$(B"))
    ("r" nil ("$B%9(B" . "$B$9(B")) ("t" nil ("$B%+(B" . "$B$+(B")) ("y" nil ("$B%s(B" . "$B$s(B"))
    ("u" nil ("$B%J(B" . "$B$J(B")) ("i" nil ("$B%K(B" . "$B$K(B")) ("o" nil ("$B%i(B" . "$B$i(B"))
    ("p" nil ("$B%;(B" . "$B$;(B"))
    ("@" nil skk-kanagaki-dakuten)
    ("[" nil skk-kanagaki-handakuten)
    ("a" nil ("$B%A(B" . "$B$A(B")) ("s" nil ("$B%H(B" . "$B$H(B"))  ("d" nil ("$B%7(B" . "$B$7(B"))
    ("f" nil ("$B%O(B" . "$B$O(B")) ("g" nil ("$B%-(B" . "$B$-(B"))  ("h" nil ("$B%/(B" . "$B$/(B"))
    ("j" nil ("$B%^(B" . "$B$^(B")) ("k" nil ("$B%N(B" . "$B$N(B"))  ("l" nil ("$B%j(B" . "$B$j(B"))
    (";" nil ("$B%l(B" . "$B$l(B")) (":" nil ("$B%1(B" . "$B$1(B"))  ("]" nil ("$B%`(B" . "$B$`(B"))
    ("z" nil ("$B%D(B" . "$B$D(B")) ("x" nil ("$B%5(B" . "$B$5(B"))  ("c" nil ("$B%=(B" . "$B$=(B"))
    ("v" nil ("$B%R(B" . "$B$R(B")) ("b" nil ("$B%3(B" . "$B$3(B"))  ("n" nil ("$B%_(B" . "$B$_(B"))
    ("m" nil ("$B%b(B" . "$B$b(B")) ("," nil ("$B%M(B" . "$B$M(B"))  ("." nil ("$B%k(B" . "$B$k(B"))
    ("/" nil ("$B%a(B" . "$B$a(B")) ("\\" nil ("$B%m(B" . "$B$m(B"))
    ;;
    ("!" nil ("$B%L(B" . "$B$L(B")) ("\"" nil ("$B%U(B" . "$B$U(B")) ("#" nil ("$B%!(B" . "$B$!(B"))
    ("$" nil ("$B%%(B" . "$B$%(B")) ("%" nil ("$B%'(B" . "$B$'(B"))  ("&" nil ("$B%)(B" . "$B$)(B"))
    ("'" nil ("$B%c(B" . "$B$c(B")) ("(" nil ("$B%e(B" . "$B$e(B"))  (")" nil ("$B%g(B" . "$B$g(B"))
    ("~" nil ("$B%r(B" . "$B$r(B")) ("=" nil "$B!r(B")
    ("|" nil "$B!<(B") ;; $B$3$l$,0lHV$NLdBj!#(B
    ("Q" nil ("$B%?(B" . "$B$?(B")) ("W" nil ("$B%F(B" . "$B$F(B"))  ("E" nil ("$B%#(B" . "$B$#(B"))
    ("R" nil ("$B%9(B" . "$B$9(B")) ("T" nil ("$B%u(B" . "$B%u(B"))  ("Y" nil ("$B%s(B" . "$B$s(B"))
    ("U" nil ("$B%J(B" . "$B$J(B")) ("I" nil ("$B%K(B" . "$B$K(B"))  ("O" nil ("$B%i(B" . "$B$i(B"))
    ("P" nil "$B!X(B")
    ("`" nil "$B!q(B")
    ("{" nil "$B!V(B")
    ("A" nil ("$B%A(B" . "$B$A(B")) ("S" nil ("$B%H(B" . "$B$H(B"))  ("D" nil ("$B%7(B" . "$B$7(B"))
    ("F" nil ("$B%O(B" . "$B$O(B")) ("G" nil ("$B%-(B" . "$B$-(B"))  ("H" nil ("$B%/(B" . "$B$/(B"))
    ("J" nil ("$B%^(B" . "$B$^(B")) ("K" nil ("$B%N(B" . "$B$N(B"))  ("L" nil ("$B%j(B" . "$B$j(B"))
    ("+" nil "$B!Y(B")          ("*" nil ("$B%v(B" . "$B%v(B"))  ("}" nil "$B!W(B")
    ("Z" nil ("$B%C(B" . "$B$C(B")) ("X" nil ("$B%5(B" . "$B$5(B"))  ("C" nil ("$B%=(B" . "$B$=(B"))
    ("V" nil ("$B%R(B" . "$B$R(B")) ("B" nil ("$B%3(B" . "$B$3(B"))  ("N" nil ("$B%_(B" . "$B$_(B"))
    ("M" nil ("$B%b(B" . "$B$b(B"))
    ("<" nil skk-current-touten)
    (">" nil skk-current-kuten)
    ("?" nil "$B!&(B")          ("_" nil ("$B%m(B" . "$B$m(B"))
    ;;
    ) "\
$BF|K\8l(B 106 $B%-!<%\!<%I$G2>L>F~NO$9$k$?$a$N4pK\%k!<%k!#$3$N@_Dj$G$O(B \"$B!<(B\" $B$NF~NO(B
$B$,9o0u$I$*$j$K$G$-$J$$$,!"(B SHIFT $B%-!<$r2!$9$3$H$G$G$-$k!#9o0u$I$*$j$KF~NO$G$-$k(B
$B$h$&$K$9$k$?$a$K$O!"2>A[%-!<%3!<%I$N%l%Y%k$G@)8f$9$kI,MW$,$"$k!#(B")

(defconst skk-kanagaki-kana-to-rom-alist
  '(;; Nemacs $B$G$OF|K\8l$O(B string $B$H$7$F07$&$Y$-!#(B
    ("$B$"(B" ?a)    ("$B$$(B" ?i)    ("$B$&(B" ?u)    ("$B$((B" ?e)    ("$B$*(B" ?o)
    ("$B$+(B" ?k ?a) ("$B$-(B" ?k ?i) ("$B$/(B" ?k ?u) ("$B$1(B" ?k ?e) ("$B$3(B" ?k ?o)
    ("$B$5(B" ?s ?a) ("$B$7(B" ?s ?i) ("$B$9(B" ?s ?u) ("$B$;(B" ?s ?e) ("$B$=(B" ?s ?o)
    ("$B$?(B" ?t ?a) ("$B$A(B" ?t ?i) ("$B$D(B" ?t ?u) ("$B$F(B" ?t ?e) ("$B$H(B" ?t ?o)
    ("$B$J(B" ?n ?a) ("$B$K(B" ?n ?i) ("$B$L(B" ?n ?u) ("$B$M(B" ?n ?e) ("$B$N(B" ?n ?o)
    ("$B$O(B" ?h ?a) ("$B$R(B" ?h ?i) ("$B$U(B" ?h ?u) ("$B$X(B" ?h ?e) ("$B$[(B" ?h ?o)
    ("$B$^(B" ?m ?a) ("$B$_(B" ?m ?i) ("$B$`(B" ?m ?u) ("$B$a(B" ?m ?e) ("$B$b(B" ?m ?o)
    ("$B$d(B" ?y ?a)              ("$B$f(B" ?y ?u)              ("$B$h(B" ?y ?o)
    ("$B$i(B" ?r ?a) ("$B$j(B" ?r ?i) ("$B$k(B" ?r ?u) ("$B$l(B" ?r ?e) ("$B$m(B" ?r ?o)
    ("$B$o(B" ?w ?a)                                        ("$B$r(B" ?w ?o)
    ("$B$s(B" ?n ?n)
    ("$B$,(B" ?g ?a) ("$B$.(B" ?g ?i) ("$B$0(B" ?g ?u) ("$B$2(B" ?g ?e) ("$B$4(B" ?g ?o)
    ("$B$6(B" ?z ?a) ("$B$8(B" ?z ?i) ("$B$:(B" ?z ?u) ("$B$<(B" ?z ?e) ("$B$>(B" ?z ?o)
    ("$B$@(B" ?d ?a) ("$B$B(B" ?d ?i) ("$B$E(B" ?d ?u) ("$B$G(B" ?d ?e) ("$B$I(B" ?d ?o)
    ("$B$P(B" ?b ?a) ("$B$S(B" ?b ?i) ("$B$V(B" ?b ?u) ("$B$Y(B" ?b ?e) ("$B$\(B" ?b ?o)
    ("$B$Q(B" ?p ?a) ("$B$T(B" ?p ?i) ("$B$W(B" ?p ?u) ("$B$Z(B" ?p ?e) ("$B$](B" ?p ?o)
    ("$B%"(B" ?a)    ("$B%$(B" ?i)    ("$B%&(B" ?u)    ("$B%((B" ?e)    ("$B%*(B" ?o)
    ("$B%+(B" ?k ?a) ("$B%-(B" ?k ?i) ("$B%/(B" ?k ?u) ("$B%1(B" ?k ?e) ("$B%3(B" ?k ?o)
    ("$B%5(B" ?s ?a) ("$B%7(B" ?s ?i) ("$B%9(B" ?s ?u) ("$B%;(B" ?s ?e) ("$B%=(B" ?s ?o)
    ("$B%?(B" ?t ?a) ("$B%A(B" ?t ?i) ("$B%D(B" ?t ?u) ("$B%F(B" ?t ?e) ("$B%H(B" ?t ?o)
    ("$B%J(B" ?n ?a) ("$B%K(B" ?n ?i) ("$B%L(B" ?n ?u) ("$B%M(B" ?n ?e) ("$B%N(B" ?n ?o)
    ("$B%O(B" ?h ?a) ("$B%R(B" ?h ?i) ("$B%U(B" ?h ?u) ("$B%X(B" ?h ?e) ("$B%[(B" ?h ?o)
    ("$B%^(B" ?m ?a) ("$B%_(B" ?m ?i) ("$B%`(B" ?m ?u) ("$B%a(B" ?m ?e) ("$B%b(B" ?m ?o)
    ("$B%d(B" ?y ?a)              ("$B%f(B" ?y ?u)              ("$B%h(B" ?y ?o)
    ("$B%i(B" ?r ?a) ("$B%j(B" ?r ?i) ("$B%k(B" ?r ?u) ("$B%l(B" ?r ?e) ("$B%m(B" ?r ?o)
    ("$B%o(B" ?w ?a)                                        ("$B%r(B" ?w ?o)
    ("$B%s(B" ?n ?n)
                              ("$B%t(B" ?v ?u)
    ("$B%,(B" ?g ?a) ("$B%.(B" ?g ?i) ("$B%0(B" ?g ?u) ("$B%2(B" ?g ?e) ("$B%4(B" ?g ?o)
    ("$B%6(B" ?z ?a) ("$B%8(B" ?z ?i) ("$B%:(B" ?z ?u) ("$B%<(B" ?z ?e) ("$B%>(B" ?z ?o)
    ("$B%@(B" ?d ?a) ("$B%B(B" ?d ?i) ("$B%E(B" ?d ?u) ("$B%G(B" ?d ?e) ("$B%I(B" ?d ?o)
    ("$B%P(B" ?b ?a) ("$B%S(B" ?b ?i) ("$B%V(B" ?b ?u) ("$B%Y(B" ?b ?e) ("$B%\(B" ?b ?o)
    ("$B%Q(B" ?p ?a) ("$B%T(B" ?p ?i) ("$B%W(B" ?p ?u) ("$B%Z(B" ?p ?e) ("$B%](B" ?p ?o)
    ;;
    ) "\
$BAw$j$"$jJQ49$N:]$J$I!"2>L>$r%m!<%^;z$KK]Lu$9$k$?$a$N%F!<%V%k!#(B")

(defconst skk-kanagaki-dakuten-alist
  '(("$B$+(B" "$B$,(B") ("$B$-(B" "$B$.(B") ("$B$/(B" "$B$0(B") ("$B$1(B" "$B$2(B") ("$B$3(B" "$B$4(B")
    ("$B$5(B" "$B$6(B") ("$B$7(B" "$B$8(B") ("$B$9(B" "$B$:(B") ("$B$;(B" "$B$<(B") ("$B$=(B" "$B$>(B")
    ("$B$?(B" "$B$@(B") ("$B$A(B" "$B$B(B") ("$B$D(B" "$B$E(B") ("$B$F(B" "$B$G(B") ("$B$H(B" "$B$I(B")
    ("$B$O(B" "$B$P(B" "$B$Q(B") ("$B$R(B" "$B$S(B" "$B$T(B") ("$B$U(B" "$B$V(B" "$B$W(B") ("$B$X(B" "$B$Y(B" "$B$Z(B")
    ("$B$[(B" "$B$\(B" "$B$](B")
                            ("$B%&(B" "$B%t(B")
    ("$B%+(B" "$B%,(B") ("$B%-(B" "$B%.(B") ("$B%/(B" "$B%0(B") ("$B%1(B" "$B%2(B") ("$B%3(B" "$B%4(B")
    ("$B%5(B" "$B%6(B") ("$B%7(B" "$B%8(B") ("$B%9(B" "$B%:(B") ("$B%;(B" "$B%<(B") ("$B%=(B" "$B%>(B")
    ("$B%?(B" "$B%@(B") ("$B%A(B" "$B%B(B") ("$B%D(B" "$B%E(B") ("$B%F(B" "$B%G(B") ("$B%H(B" "$B%I(B")
    ("$B%O(B" "$B%P(B" "$B%Q(B") ("$B%R(B" "$B%S(B" "$B%T(B") ("$B%U(B" "$B%V(B" "$B%W(B") ("$B%X(B" "$B%Y(B" "$B%Z(B")
    ("$B%[(B" "$B%\(B" "$B%](B")
    ;;
    ) "\
$BByE@$HH>ByE@$rF~NO$9$k$?$a$N%k!<%k!#(B")

(defvar skk-kanagaki-rule-tree nil)
(defvar skk-kanagaki-rom-kana-rule-tree nil)

(defvar skk-kanagaki-temp-dir (or (getenv "TMP") "/tmp"))

;; Advice.
(defadvice skk-regularize (before skk-kanagaki-ad activate compile)
  "SKK $B5/F0;~$NE,Ev$J%?%$%_%s%0$G2>L>F~NOMQ$N@_Dj$r9T$&!#(B"
  (mapcar (function
	   (lambda (cons)
	     (and (symbol-value (car cons))
		  (define-key skk-j-mode-map
		    (symbol-value (car cons)) (cdr cons)))))
	  '((skk-kanagaki-set-henkan-point-key . skk-set-henkan-point-subr)
	    (skk-kanagaki-abbrev-mode-key . skk-abbrev-mode)
	    (skk-kanagaki-katakana-mode-key . skk-toggle-kana)
	    (skk-kanagaki-latin-jisx0208-mode-key . skk-jisx0208-latin-mode)
	    (skk-kanagaki-hankaku-mode-key . skk-toggle-katakana)
	    (skk-kanagaki-latin-mode-key . skk-latin-mode)
	    (skk-kanagaki-code-input-key . skk-input-by-code-or-menu)
	    (skk-kanagaki-previous-candidate-key . skk-previous-candidate)))
  (unless (memq skk-emacs-type '(nemacs mule1))
    (eval-after-load "skk-jisx0201"
      '(and skk-kanagaki-hankaku-mode-key
	    (define-key skk-jisx0201-mode-map skk-kanagaki-hankaku-mode-key
	      'skk-toggle-katakana))))
  ;;
  (case skk-kanagaki-jidou-key-symbol-kakikae-service
    ;;
    (106-jis
     (cond
      ((eq window-system 'x)
       (let ((prog (exec-installed-p "xmodmap"))
	     (tmp (make-temp-name
		   (expand-file-name "kanagaki" skk-kanagaki-temp-dir))))
	 (cond ((and prog
		     (message "xmodmap $B$r8F$s$G$$$^$9(B...")
		     (save-excursion
		       (set-buffer (get-buffer-create " *kanagaki*"))
		       (erase-buffer)
		       (insert "keycode 123 = underscore underscore\n")
		       (write-region (point-min) (point-max) tmp)
		       (eq 0 (call-process prog nil nil nil tmp))))
		;;
		(setq skk-kanagaki-rule-list
		      (nconc skk-kanagaki-rule-list
			     '(("\\" nil "$B!<(B"))))
		(delete-file tmp)
		(message "xmodmap $B$r8F$s$G$$$^$9(B...$B40N;(B"))
	       (t
		(message "xmodmap $B$N8F$S=P$7$K<:GT$7$^$7$?(B")))))
      (t
       nil)))
    ;;
    (106-jis-kodawari
     (cond
      ((eq window-system 'x)
       (let ((prog (exec-installed-p "xmodmap"))
	     (tmp (make-temp-name
		   (expand-file-name "kanagaki" skk-kanagaki-temp-dir))))
	 (cond ((and prog
		     (message "xmodmap $B$r8F$s$G$$$^$9(B...")
		     (save-excursion
		       (set-buffer (get-buffer-create " *kanagaki*"))
		       (erase-buffer)
		       (insert "keycode 123 = quotedbl underscore
keycode 19 = 0 exclam
keycode 21 = asciicircum asciitilde
keycode 34 = at grave\n")
		       (write-region (point-min) (point-max) tmp)
		       (eq 0 (call-process prog nil nil nil tmp))))
		;;
		(setq skk-kanagaki-rule-list
		      (nconc skk-kanagaki-rule-list
			     '(("~" nil "$B!9(B")
			       ("\\" nil "$B!<(B")
			       ("|" nil "$B"L(B")
			       ("`" nil "$B!q(B")
			       ("!" nil ("$B%r(B" . "$B$r(B"))
			       ("\"" nil ("$B%m(B" . "$B$m(B"))
			       ("_" nil "$B!C(B"))))
		(delete-file tmp)
		(message "xmodmap $B$r8F$s$G$$$^$9(B...$B40N;(B"))
	       (t
		(message "xmodmap $B$N8F$S=P$7$K<:GT$7$^$7$?(B")))))
      (t
       nil)))
    ;;
    (t
     nil))
  ;;
  (define-key skk-j-mode-map " " 'skk-kanagaki-insert)
  ;;
  (setq skk-kanagaki-rule-tree
	(skk-compile-rule-list
	 (symbol-value (intern (format "skk-kanagaki-%s-base-rule-list"
				       skk-kanagaki-keyboard-type)))
	 skk-kanagaki-rule-list))
  (unless skk-kanagaki-rom-kana-rule-tree
    (setq skk-kanagaki-rom-kana-rule-tree
	  (skk-compile-rule-list skk-rom-kana-base-rule-list
				 skk-rom-kana-rule-list))))

(defadvice skk-insert (around skk-kanagaki-ad activate compile)
  "$B2>L>F~NOMQ$N(B work around $B!#(B"
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
    (cond (skk-use-kana-keyboard
	   (or (equal skk-rule-tree skk-kanagaki-rule-tree)
	       (setq skk-rule-tree skk-kanagaki-rule-tree))
	   (let (skk-set-henkan-point-key)
	     ad-do-it))
	  (t
	   (or (equal skk-rule-tree skk-kanagaki-rom-kana-rule-tree)
	       (setq skk-rule-tree skk-kanagaki-rom-kana-rule-tree))
	   ad-do-it))))

;; Functions.
(defun skk-kanagaki-insert (&optional arg)
  "SPC $B%-!<$@$1$3$l$r(B `skk-insert' $B$NBe$o$j$K;H$&!#(B"
  (interactive "*p")
  (cond ((eq arg 1) (skk-insert arg))
	;; C-u [SPC] $B$GAw$j$"$jJQ49$r$9$k!#(B
	(t (skk-kanagaki-start-henkan-okuriari))))

(defun skk-kanagaki-dakuten (&optional arg)
  "$BD>A0$NJ8;z$r8+$F2DG=$J$iByE@$rIU2C$7!"$5$b$J$1$l$P(B \"$B!+(B\" $B$rF~NO$9$k!#(B"
  (interactive "*p")
  (let ((list skk-kanagaki-dakuten-alist)
	(pt1 (point))
	(len (if (eq skk-emacs-type 'nemacs) 2 1))
	char1 char2)
    (setq char1
	  (save-excursion
	    (backward-char (* len 1))
	    (buffer-substring (point) pt1)))
    (cond ((setq char2 (cadr (assoc char1 list)))
	   (delete-char -1)
	   (insert char2))
	  (t
	   (insert "$B!+(B")))))

(defun skk-kanagaki-handakuten (&optional arg)
  "$BD>A0$NJ8;z$r8+$F2DG=$J$iH>ByE@$rIU2C$7!"$5$b$J$1$l$P(B \"$B!,(B\" $B$rF~NO$9$k!#(B"
  (interactive "*p")
  (let ((list skk-kanagaki-dakuten-alist)
	(pt1 (point))
	(len (if (eq skk-emacs-type 'nemacs) 2 1))
	char1 char2)
    (setq char1
	  (save-excursion
	    (backward-char (* len 1))
	    (buffer-substring (point) pt1)))
    (cond ((setq char2 (caddr (assoc char1 list)))
	   (delete-char -1)
	   (insert char2))
	  (t
	   (insert "$B!,(B")))))

(defun skk-kanagaki-start-henkan-okuriari (&optional no-sokuon)
  ;; $BD>A0$NJ8;z$r%m!<%^;z$N%j%9%H$KK]Lu$7!"$"$?$+$b$=$N=gHV$K%-!<F~NO$5$l$?$+$N(B
  ;; $B$h$&$K(B SKK $B$K;W$o$;$k!#(B
  (let ((list skk-kanagaki-kana-to-rom-alist)
	(i 0)
	(pt1 (point))
	(len (if (eq skk-emacs-type 'nemacs) 2 1))
	pt2 okuri-char rom char sokuon)
    (setq okuri-char
	  (save-excursion
	    (backward-char (* len 1))
	    (buffer-substring (setq pt2 (point)) pt1)))
    (when okuri-char
      (setq rom (copy-sequence (cdr (assoc okuri-char list))))
      (unless no-sokuon
	(setq sokuon
	      (save-excursion
		(backward-char (* len 2))
		(buffer-substring (point) pt2)))
	(if (member sokuon '("$B$C(B" "$B%C(B"))
	    ;; $BB%2;$r8+$D$1$?$H$-$O!"%m!<%^;z%j%9%H$N@hF,$N;z$r7+$jJV$9!#(B
	    (setcdr rom (copy-sequence rom))
	  (setq sokuon nil))))
    (when rom
      (delete-char (* len (if sokuon -2 -1)))
      ;; $B%m!<%^;z%j%9%H$N@hF,$rBgJ8;z$K$9$k$3$H$GAw$j$"$j$NJQ49$r$5$;$k!#(B
      (setcar rom (upcase (car rom)))
      (while (setq char (nth i rom))
	(let ((last-command-char char)
	      (skk-rule-tree skk-kanagaki-rom-kana-rule-tree))
	  (ad-Orig-skk-insert 1))
	(setq i (1+ i))))))

(provide 'skk-kanagaki)
;;; Local Variables:
;;; End:
;;; skk-kanagaki.el ends here
