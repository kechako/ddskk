;;; skk-def.el --- SKK default definition.
;; Copyright (C) 1999, 2000 NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>

;; Author: NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Version: $Id: skk-def.el,v 1.2 2001/02/03 00:23:00 minakaji Exp $
;; Keywords: japanese
;; Last Modified: $Date: 2001/02/03 00:23:00 $

;; This file is part of Daredevil SKK.

;; Daredevil SKK is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either versions 2, or (at your option)
;; any later version.

;; Daredevil SKK is distributed in the hope that it will be useful
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Daredevil SKK, see the file COPYING.  If not, write to the Free
;; Software Foundation Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary:
;; this file is to restore default setting for SKK.

;; Following people contributed modifications to skk.el (Alphabetical order):

;;; Code:
(setq skk-start-henkan-char ?\040)

(setq skk-rom-kana-base-rule-list
      '(("a" nil ("$B%"(B" . "$B$"(B"))
	("bb" "b" ("$B%C(B" . "$B$C(B"))
	("ba" nil ("$B%P(B" . "$B$P(B"))
	("be" nil ("$B%Y(B" . "$B$Y(B"))
	("bi" nil ("$B%S(B" . "$B$S(B"))
	("bo" nil ("$B%\(B" . "$B$\(B"))
	("bu" nil ("$B%V(B" . "$B$V(B"))
	("bya" nil ("$B%S%c(B" . "$B$S$c(B"))
	("bye" nil ("$B%S%'(B" . "$B$S$'(B"))
	("byi" nil ("$B%S%#(B" . "$B$S$#(B"))
	("byo" nil ("$B%S%g(B" . "$B$S$g(B"))
	("byu" nil ("$B%S%e(B" . "$B$S$e(B"))
	("cc" "c" ("$B%C(B" . "$B$C(B"))
	("cha" nil ("$B%A%c(B" . "$B$A$c(B"))
	("che" nil ("$B%A%'(B" . "$B$A$'(B"))
	("chi" nil ("$B%A(B" . "$B$A(B"))
	("cho" nil ("$B%A%g(B" . "$B$A$g(B"))
	("chu" nil ("$B%A%e(B" . "$B$A$e(B"))
	("cya" nil ("$B%A%c(B" . "$B$A$c(B"))
	("cye" nil ("$B%A%'(B" . "$B$A$'(B"))
	("cyi" nil ("$B%A%#(B" . "$B$A$#(B"))
	("cyo" nil ("$B%A%g(B" . "$B$A$g(B"))
	("cyu" nil ("$B%A%e(B" . "$B$A$e(B"))
	("dd" "d" ("$B%C(B" . "$B$C(B"))
	("da" nil ("$B%@(B" . "$B$@(B"))
	("de" nil ("$B%G(B" . "$B$G(B"))
	("dha" nil ("$B%G%c(B" . "$B$G$c(B"))
	("dhe" nil ("$B%G%'(B" . "$B$G$'(B"))
	("dhi" nil ("$B%G%#(B" . "$B$G$#(B"))
	("dho" nil ("$B%G%g(B" . "$B$G$g(B"))
	("dhu" nil ("$B%G%e(B" . "$B$G$e(B"))
	("di" nil ("$B%B(B" . "$B$B(B"))
	("do" nil ("$B%I(B" . "$B$I(B"))
	("du" nil ("$B%E(B" . "$B$E(B"))
	("dya" nil ("$B%B%c(B" . "$B$B$c(B"))
	("dye" nil ("$B%B%'(B" . "$B$B$'(B"))
	("dyi" nil ("$B%B%#(B" . "$B$B$#(B"))
	("dyo" nil ("$B%B%g(B" . "$B$B$g(B"))
	("dyu" nil ("$B%B%e(B" . "$B$B$e(B"))
	("e" nil ("$B%((B" . "$B$((B"))
	("ff" "f" ("$B%C(B" . "$B$C(B"))
	("fa" nil ("$B%U%!(B" . "$B$U$!(B"))
	("fe" nil ("$B%U%'(B" . "$B$U$'(B"))
	("fi" nil ("$B%U%#(B" . "$B$U$#(B"))
	("fo" nil ("$B%U%)(B" . "$B$U$)(B"))
	("fu" nil ("$B%U(B" . "$B$U(B"))
	("fya" nil ("$B%U%c(B" . "$B$U$c(B"))
	("fye" nil ("$B%U%'(B" . "$B$U$'(B"))
	("fyi" nil ("$B%U%#(B" . "$B$U$#(B"))
	("fyo" nil ("$B%U%g(B" . "$B$U$g(B"))
	("fyu" nil ("$B%U%e(B" . "$B$U$e(B"))
	("gg" "g" ("$B%C(B" . "$B$C(B"))
	("ga" nil ("$B%,(B" . "$B$,(B"))
	("ge" nil ("$B%2(B" . "$B$2(B"))
	("gi" nil ("$B%.(B" . "$B$.(B"))
	("go" nil ("$B%4(B" . "$B$4(B"))
	("gu" nil ("$B%0(B" . "$B$0(B"))
	("gya" nil ("$B%.%c(B" . "$B$.$c(B"))
	("gye" nil ("$B%.%'(B" . "$B$.$'(B"))
	("gyi" nil ("$B%.%#(B" . "$B$.$#(B"))
	("gyo" nil ("$B%.%g(B" . "$B$.$g(B"))
	("gyu" nil ("$B%.%e(B" . "$B$.$e(B"))
	;;("h" "" ("$B%*(B" . "$B$*(B"))
	("ha" nil ("$B%O(B" . "$B$O(B"))
	("he" nil ("$B%X(B" . "$B$X(B"))
	("hi" nil ("$B%R(B" . "$B$R(B"))
	("ho" nil ("$B%[(B" . "$B$[(B"))
	("hu" nil ("$B%U(B" . "$B$U(B"))
	("hya" nil ("$B%R%c(B" . "$B$R$c(B"))
	("hye" nil ("$B%R%'(B" . "$B$R$'(B"))
	("hyi" nil ("$B%R%#(B" . "$B$R$#(B"))
	("hyo" nil ("$B%R%g(B" . "$B$R$g(B"))
	("hyu" nil ("$B%R%e(B" . "$B$R$e(B"))
	("i" nil ("$B%$(B" . "$B$$(B"))
	("jj" "j" ("$B%C(B" . "$B$C(B"))
	("ja" nil ("$B%8%c(B" . "$B$8$c(B"))
	("je" nil ("$B%8%'(B" . "$B$8$'(B"))
	("ji" nil ("$B%8(B" . "$B$8(B"))
	("jo" nil ("$B%8%g(B" . "$B$8$g(B"))
	("ju" nil ("$B%8%e(B" . "$B$8$e(B"))
	("jya" nil ("$B%8%c(B" . "$B$8$c(B"))
	("jye" nil ("$B%8%'(B" . "$B$8$'(B"))
	("jyi" nil ("$B%8%#(B" . "$B$8$#(B"))
	("jyo" nil ("$B%8%g(B" . "$B$8$g(B"))
	("jyu" nil ("$B%8%e(B" . "$B$8$e(B"))
	("kk" "k" ("$B%C(B" . "$B$C(B"))
	("ka" nil ("$B%+(B" . "$B$+(B"))
	("ke" nil ("$B%1(B" . "$B$1(B"))
	("ki" nil ("$B%-(B" . "$B$-(B"))
	("ko" nil ("$B%3(B" . "$B$3(B"))
	("ku" nil ("$B%/(B" . "$B$/(B"))
	("kya" nil ("$B%-%c(B" . "$B$-$c(B"))
	("kye" nil ("$B%-%'(B" . "$B$-$'(B"))
	("kyi" nil ("$B%-%#(B" . "$B$-$#(B"))
	("kyo" nil ("$B%-%g(B" . "$B$-$g(B"))
	("kyu" nil ("$B%-%e(B" . "$B$-$e(B"))
	("ma" nil ("$B%^(B" . "$B$^(B"))
	("me" nil ("$B%a(B" . "$B$a(B"))
	("mi" nil ("$B%_(B" . "$B$_(B"))
	("mo" nil ("$B%b(B" . "$B$b(B"))
	("mu" nil ("$B%`(B" . "$B$`(B"))
	("mya" nil ("$B%_%c(B" . "$B$_$c(B"))
	("mye" nil ("$B%_%'(B" . "$B$_$'(B"))
	("myi" nil ("$B%_%#(B" . "$B$_$#(B"))
	("myo" nil ("$B%_%g(B" . "$B$_$g(B"))
	("myu" nil ("$B%_%e(B" . "$B$_$e(B"))
	("n" nil ("$B%s(B" . "$B$s(B"))
	("n'" nil ("$B%s(B" . "$B$s(B"))
	("na" nil ("$B%J(B" . "$B$J(B"))
	("ne" nil ("$B%M(B" . "$B$M(B"))
	("ni" nil ("$B%K(B" . "$B$K(B"))
	("nn" nil ("$B%s(B" . "$B$s(B"))
	("no" nil ("$B%N(B" . "$B$N(B"))
	("nu" nil ("$B%L(B" . "$B$L(B"))
	("nya" nil ("$B%K%c(B" . "$B$K$c(B"))
	("nye" nil ("$B%K%'(B" . "$B$K$'(B"))
	("nyi" nil ("$B%K%#(B" . "$B$K$#(B"))
	("nyo" nil ("$B%K%g(B" . "$B$K$g(B"))
	("nyu" nil ("$B%K%e(B" . "$B$K$e(B"))
	("o" nil ("$B%*(B" . "$B$*(B"))
	("pp" "p" ("$B%C(B" . "$B$C(B"))
	("pa" nil ("$B%Q(B" . "$B$Q(B"))
	("pe" nil ("$B%Z(B" . "$B$Z(B"))
	("pi" nil ("$B%T(B" . "$B$T(B"))
	("po" nil ("$B%](B" . "$B$](B"))
	("pu" nil ("$B%W(B" . "$B$W(B"))
	("pya" nil ("$B%T%c(B" . "$B$T$c(B"))
	("pye" nil ("$B%T%'(B" . "$B$T$'(B"))
	("pyi" nil ("$B%T%#(B" . "$B$T$#(B"))
	("pyo" nil ("$B%T%g(B" . "$B$T$g(B"))
	("pyu" nil ("$B%T%e(B" . "$B$T$e(B"))
	("rr" "r" ("$B%C(B" . "$B$C(B"))
	("ra" nil ("$B%i(B" . "$B$i(B"))
	("re" nil ("$B%l(B" . "$B$l(B"))
	("ri" nil ("$B%j(B" . "$B$j(B"))
	("ro" nil ("$B%m(B" . "$B$m(B"))
	("ru" nil ("$B%k(B" . "$B$k(B"))
	("rya" nil ("$B%j%c(B" . "$B$j$c(B"))
	("rye" nil ("$B%j%'(B" . "$B$j$'(B"))
	("ryi" nil ("$B%j%#(B" . "$B$j$#(B"))
	("ryo" nil ("$B%j%g(B" . "$B$j$g(B"))
	("ryu" nil ("$B%j%e(B" . "$B$j$e(B"))
	("ss" "s" ("$B%C(B" . "$B$C(B"))
	("sa" nil ("$B%5(B" . "$B$5(B"))
	("se" nil ("$B%;(B" . "$B$;(B"))
	("sha" nil ("$B%7%c(B" . "$B$7$c(B"))
	("she" nil ("$B%7%'(B" . "$B$7$'(B"))
	("shi" nil ("$B%7(B" . "$B$7(B"))
	("sho" nil ("$B%7%g(B" . "$B$7$g(B"))
	("shu" nil ("$B%7%e(B" . "$B$7$e(B"))
	("si" nil ("$B%7(B" . "$B$7(B"))
	("so" nil ("$B%=(B" . "$B$=(B"))
	("su" nil ("$B%9(B" . "$B$9(B"))
	("sya" nil ("$B%7%c(B" . "$B$7$c(B"))
	("sye" nil ("$B%7%'(B" . "$B$7$'(B"))
	("syi" nil ("$B%7%#(B" . "$B$7$#(B"))
	("syo" nil ("$B%7%g(B" . "$B$7$g(B"))
	("syu" nil ("$B%7%e(B" . "$B$7$e(B"))
	("tt" "t" ("$B%C(B" . "$B$C(B"))
	("ta" nil ("$B%?(B" . "$B$?(B"))
	("te" nil ("$B%F(B" . "$B$F(B"))
	("tha" nil ("$B%F%!(B" . "$B$F$!(B"))
	("the" nil ("$B%F%'(B" . "$B$F$'(B"))
	("thi" nil ("$B%F%#(B" . "$B$F$#(B"))
	("tho" nil ("$B%F%g(B" . "$B$F$g(B"))
	("thu" nil ("$B%F%e(B" . "$B$F$e(B"))
	("ti" nil ("$B%A(B" . "$B$A(B"))
	("to" nil ("$B%H(B" . "$B$H(B"))
	("tsu" nil ("$B%D(B" . "$B$D(B"))
	("tu" nil ("$B%D(B" . "$B$D(B"))
	("tya" nil ("$B%A%c(B" . "$B$A$c(B"))
	("tye" nil ("$B%A%'(B" . "$B$A$'(B"))
	("tyi" nil ("$B%A%#(B" . "$B$A$#(B"))
	("tyo" nil ("$B%A%g(B" . "$B$A$g(B"))
	("tyu" nil ("$B%A%e(B" . "$B$A$e(B"))
	("u" nil ("$B%&(B" . "$B$&(B"))
	("vv" "v" ("$B%C(B" . "$B$C(B"))
	("va" nil ("$B%t%!(B" . "$B$&!+$!(B"))
	("ve" nil ("$B%t%'(B" . "$B$&!+$'(B"))
	("vi" nil ("$B%t%#(B" . "$B$&!+$#(B"))
	("vo" nil ("$B%t%)(B" . "$B$&!+$)(B"))
	("vu" nil ("$B%t(B" . "$B$&!+(B"))
	("ww" "w" ("$B%C(B" . "$B$C(B"))
	("wa" nil ("$B%o(B" . "$B$o(B"))
	("we" nil ("$B%&%'(B" . "$B$&$'(B"))
	("wi" nil ("$B%&%#(B" . "$B$&$#(B"))
	("wo" nil ("$B%r(B" . "$B$r(B"))
	("wu" nil ("$B%&(B" . "$B$&(B"))
	("xx" "x" ("$B%C(B" . "$B$C(B"))
	("xa" nil ("$B%!(B" . "$B$!(B"))
	("xe" nil ("$B%'(B" . "$B$'(B"))
	("xi" nil ("$B%#(B" . "$B$#(B"))
	("xka" nil ("$B%u(B" . "$B$+(B"))
	("xke" nil ("$B%v(B" . "$B$1(B"))
	("xo" nil ("$B%)(B" . "$B$)(B"))
	("xtsu" nil ("$B%C(B" . "$B$C(B"))
	("xtu" nil ("$B%C(B" . "$B$C(B"))
	("xu" nil ("$B%%(B" . "$B$%(B"))
	("xwa" nil ("$B%n(B" . "$B$n(B"))
	("xwe" nil ("$B%q(B" . "$B$q(B"))
	("xwi" nil ("$B%p(B" . "$B$p(B"))
	("xya" nil ("$B%c(B" . "$B$c(B"))
	("xyo" nil ("$B%g(B" . "$B$g(B"))
	("xyu" nil ("$B%e(B" . "$B$e(B"))
	("yy" "y" ("$B%C(B" . "$B$C(B"))
	("ya" nil ("$B%d(B" . "$B$d(B"))
	("ye" nil ("$B%$%'(B" . "$B$$$'(B"))
	("yo" nil ("$B%h(B" . "$B$h(B"))
	("yu" nil ("$B%f(B" . "$B$f(B"))
	("zz" "z" ("$B%C(B" . "$B$C(B"))
	("z," nil "$B!E(B")
	("z-" nil "$B!A(B")
	("z." nil "$B!D(B")
	("z/" nil "$B!&(B")
	("z[" nil "$B!X(B")
	("z]" nil "$B!Y(B")
	("za" nil ("$B%6(B" . "$B$6(B"))
	("ze" nil ("$B%<(B" . "$B$<(B"))
	("zh" nil "$B"+(B")
	("zi" nil ("$B%8(B" . "$B$8(B"))
	("zj" nil "$B"-(B")
	("zk" nil "$B",(B")
	("zl" nil "$B"*(B")
	("zo" nil ("$B%>(B" . "$B$>(B"))
	("zu" nil ("$B%:(B" . "$B$:(B"))
	("zya" nil ("$B%8%c(B" . "$B$8$c(B"))
	("zye" nil ("$B%8%'(B" . "$B$8$'(B"))
	("zyi" nil ("$B%8%#(B" . "$B$8$#(B"))
	("zyo" nil ("$B%8%g(B" . "$B$8$g(B"))
	("zyu" nil ("$B%8%e(B" . "$B$8$e(B"))
	("." nil skk-current-kuten)
	("," nil skk-current-touten)
	("-" nil "$B!<(B")
	(":" nil "$B!'(B")
	(";" nil "$B!((B")
	("?" nil "$B!)(B")
	("[" nil "$B!V(B")
	("]" nil "$B!W(B")
	("l" nil skk-latin-mode)
	("q" nil skk-toggle-kana)
	("L" nil skk-jisx0208-latin-mode)
	("Q" nil skk-set-henkan-point-subr)
	("X" nil skk-purge-from-jisyo)
	("/" nil skk-abbrev-mode)
	("$" nil skk-display-code-for-char-at-point)
	("@" nil skk-today)
	("\\" nil skk-input-by-code-or-menu)
	))

(setq skk-rom-kana-rule-list
      '(
	("hh" "h" ("$B%C(B" . "$B$C(B"))
	("mm" "m" ("$B%s(B" . "$B$s(B"))
	))

(setq skk-try-completion-char ?\011)
(provide 'skk-def)
;;; skk-def.el ends here
