;;; skk-106-jis.el --- $BF|K\8l(B 106 $B%-!<%\!<%I$K$h$k2>L>F~NO%5%]!<%H(B

;; Copyright (C) 2000 Tetsuo Tsukamoto <czkmt@remus.dti.ne.jp>

;; Author: Tetsuo Tsukamoto <czkmt@remus.dti.ne.jp>
;; Keywords: hardware, japanese, mule, input method

;; This file is part of Daredevil SKK.

;; Daredevil SKK is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either versions 2, or
;; (at your option) any later version.

;; Daredevil SKK is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Daredevil SKK, see the file COPYING.  If not, write to
;; the Free Software Foundation Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; $B$3$N%U%!%$%k$O!"F|K\8l(B 106 $B%-!<%\!<%I(B ($B5l(B JIS $BG[Ns(B) $B$K$h$k2>L>F~NO$N$?$a$N%k(B
;; $B!<%k$rDs6!$7$^$9!#(B

;;; Code:

(eval-when-compile
  (require 'cl)
  (require 'skk-macs)
  (require 'skk-vars))

(eval-when-compile
  (require 'skk-kanagaki-util))

(require 'skk-kanagaki)


;; $BF|K\8l(B 106 $B%-!<%\!<%I(B ($B5l(B JIS $BG[Ns(B) $B$N%k!<%k(B

(defvar skk-kanagaki-106-jis-base-rule-list
  '(("1" nil skk-nicola-insert)  ("2" nil skk-nicola-insert)
    ("3" nil skk-nicola-insert)  ("4" nil skk-nicola-insert)
    ("5" nil skk-nicola-insert)
    ;;
    ("6" nil skk-nicola-insert) ("7" nil skk-nicola-insert)
    ("8" nil skk-nicola-insert) ("9" nil skk-nicola-insert)
    ("0" nil skk-nicola-insert) ("-" nil skk-nicola-insert)
    ("^" nil skk-nicola-insert) ("\\" nil skk-nicola-insert)
    ;;
    ("q" nil skk-nicola-insert) ("w" nil skk-nicola-insert)
    ("e" nil skk-nicola-insert) ("r" nil skk-nicola-insert)
    ("t" nil skk-nicola-insert)
    ;;
    ("y" nil skk-nicola-insert)  ("u" nil skk-nicola-insert)
    ("i" nil skk-nicola-insert)  ("o" nil skk-nicola-insert)
    ("p" nil skk-nicola-insert)
    ("@" nil skk-kanagaki-dakuten)
    ("[" nil skk-kanagaki-handakuten)
    ;;
    ("a" nil skk-nicola-insert) ("s" nil skk-nicola-insert)
    ("d" nil skk-nicola-insert) ("f" nil skk-nicola-insert)
    ("g" nil skk-nicola-insert)
    ;;
    ("h" nil skk-nicola-insert)  ("j" nil skk-nicola-insert)
    ("k" nil skk-nicola-insert)  ("l" nil skk-nicola-insert)
    (";" nil skk-nicola-insert)  (":" nil skk-nicola-insert)
    ("]" nil skk-nicola-insert)
    ;;
    ("z" nil skk-nicola-insert) ("x" nil skk-nicola-insert)
    ("c" nil skk-nicola-insert) ("v" nil skk-nicola-insert)
    ("b" nil skk-nicola-insert)
    ;;
    ("n" nil skk-nicola-insert)  ("m" nil skk-nicola-insert)
    ("," nil skk-nicola-insert)  ("." nil skk-nicola-insert)
    ("/" nil skk-nicola-insert)
    ;;
    ("#" Nil ("$B%!(B" . "$B$!(B"))
    ("$" nil ("$B%%(B" . "$B$%(B")) ("%" nil ("$B%'(B" . "$B$'(B"))  ("&" nil ("$B%)(B" . "$B$)(B"))
    ("'" nil ("$B%c(B" . "$B$c(B")) ("(" nil ("$B%e(B" . "$B$e(B"))  (")" nil ("$B%g(B" . "$B$g(B"))
    ("~" nil ("$B%r(B" . "$B$r(B")) ("=" nil "$B!r(B")
    ("|" nil "$B!<(B") ;; $B$3$l$,0lHV$NLdBj!#(B
    ("Q" nil skk-set-henkan-point-subr)
    ("E" nil ("$B%#(B" . "$B$#(B"))
    ("T" nil ("$B%u(B" . "$B%u(B"))  ("Y" nil ("$B%s(B" . "$B$s(B"))
    ("P" nil "$B!X(B")
    ("`" nil "$B!q(B")
    ("{" nil "$B!V(B")
    ("A" nil skk-latin-mode)
    ("S" nil skk-kanagaki-set-okurigana-no-sokuon)
    ("D" nil skk-today)
    ("F" nil skk-display-code-for-char-at-point)
    ("J" nil skk-abbrev-mdoe)
    ("K" nil skk-toggle-kana)
    ("L" nil skk-jisx0208-latin-mode)
    ("+" nil "$B!Y(B") ("*" nil ("$B%v(B" . "$B%v(B"))  ("}" nil "$B!W(B")
    ("Z" nil ("$B%C(B" . "$B$C(B"))
    ("X" nil skk-purge-from-jisyo)
    ("C" nil skk-input-by-code-or-menu)
    ("M" nil skk-kanagaki-midashi-henkan)
    ("<" nil skk-current-touten)
    (">" nil skk-current-kuten)
    ("?" nil "$B!&(B")
    ;; $B>e5-$N!V!<!W$NLdBj$r$R$-$:$C$F$$$k!#(B
    ("_" nil ("$B%m(B" . "$B$m(B"))) "\
$BF|K\8l(B 106 $B%-!<%\!<%I$G2>L>F~NO$9$k$?$a$N4pK\%k!<%k!#(B
$B$3$N@_Dj$G$O(B \"$B!<(B\" $B$NF~NO$,9o0u$I$*$j$K$G$-$J$$$,!"(B SHIFT $B%-!<$r2!$9$3$H$G$G$-(B
$B$k!#(B $B9o0u$I$*$j$KF~NO$G$-$k$h$&$K$9$k$?$a$K$O!"2>A[%-!<%3!<%I$N%l%Y%k$G@)8f$9$k(B
$BI,MW$,$"$k!#(B")

(defvar skk-106-jis-plain-rule-list
  '((?1 ("$B%L(B" . "$B$L(B")) (?2 ("$B%U(B" . "$B$U(B")) (?3 ("$B%"(B" . "$B$"(B"))
    (?4 ("$B%&(B" . "$B$&(B")) (?5 ("$B%((B" . "$B$((B")) (?6 ("$B%*(B" . "$B$*(B"))
    (?7 ("$B%d(B" . "$B$d(B")) (?8 ("$B%f(B" . "$B$f(B")) (?9 ("$B%h(B" . "$B$h(B"))
    (?0 ("$B%o(B" . "$B$o(B")) (?- ("$B%[(B" . "$B$[(B")) (?^ ("$B%X(B" . "$B$X(B"))
    (?q ("$B%?(B" . "$B$?(B")) (?w ("$B%F(B" . "$B$F(B")) (?e ("$B%$(B" . "$B$$(B"))
    (?r ("$B%9(B" . "$B$9(B")) (?t ("$B%+(B" . "$B$+(B")) (?y ("$B%s(B" . "$B$s(B"))
    (?u ("$B%J(B" . "$B$J(B")) (?i ("$B%K(B" . "$B$K(B")) (?o ("$B%i(B" . "$B$i(B"))
    (?p ("$B%;(B" . "$B$;(B"))
    (?a ("$B%A(B" . "$B$A(B")) (?s ("$B%H(B" . "$B$H(B"))  (?d ("$B%7(B" . "$B$7(B"))
    (?f ("$B%O(B" . "$B$O(B")) (?g ("$B%-(B" . "$B$-(B"))  (?h ("$B%/(B" . "$B$/(B"))
    (?j ("$B%^(B" . "$B$^(B")) (?k ("$B%N(B" . "$B$N(B"))  (?l ("$B%j(B" . "$B$j(B"))
    (?\; ("$B%l(B" . "$B$l(B")) (?: ("$B%1(B" . "$B$1(B"))  (?\] ("$B%`(B" . "$B$`(B"))
    (?z ("$B%D(B" . "$B$D(B")) (?x ("$B%5(B" . "$B$5(B"))  (?c ("$B%=(B" . "$B$=(B"))
    (?v ("$B%R(B" . "$B$R(B")) (?b ("$B%3(B" . "$B$3(B"))  (?n ("$B%_(B" . "$B$_(B"))
    (?m ("$B%b(B" . "$B$b(B")) (?\, ("$B%M(B" . "$B$M(B"))  (?\. ("$B%k(B" . "$B$k(B"))
    (?/ ("$B%a(B" . "$B$a(B")) (?\\ ("$B%m(B" . "$B$m(B"))))

(defvar skk-106-jis-lshift-rule-list nil)
(defvar skk-106-jis-rshift-rule-list nil)

(require 'skk-nicola)

(case skk-kanagaki-jidou-keymap-kakikae-service
  ;;
  (106-jis
   (skk-kanagaki-call-xmodmap
       "keycode 123 = underscore underscore\n"
     (setq skk-kanagaki-rule-list
	   (nconc skk-kanagaki-rule-list
		  '(("\\" nil "$B!<(B"))))))
  ;;
  (106-jis-kodawari
   (skk-kanagaki-call-xmodmap
       "keycode 123 = quotedbl underscore
keycode 19 = 0 exclam
keycode 21 = asciicircum asciitilde
keycode 34 = at grave\n"
     (setq skk-kanagaki-rule-list
	   (nconc skk-kanagaki-rule-list
		  '(("~" nil "$B!9(B")
		    ("\\" nil "$B!<(B")
		    ("|" nil "$B"L(B")
		    ("!" nil ("$B%r(B" . "$B$r(B"))
		    ("\"" nil ("$B%m(B" . "$B$m(B"))
		    ("_" nil "$B!C(B")))))))

(require 'product)
(product-provide
    (provide 'skk-106-jis)
  (require 'skk-version))

;;; skk-106-jis.el ends here
