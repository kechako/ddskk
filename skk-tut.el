; SKK tutorial for SKK version 10.46 and later versions
;; Copyright (C) 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997,
;;               1998, 1999, 2000
;; Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>

;; Author: Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>
;; Maintainer: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Version: $Id: skk-tut.el,v 1.11 2000/09/21 10:49:43 akiho Exp $
;; Keywords: japanese
;; Last Modified: $Date: 2000/09/21 10:49:43 $

;; This file is part of SKK.

;; SKK is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either versions 2, or (at your option)
;; any later version.

;; SKK is distributed in the hope that it will be useful
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with SKK, see the file COPYING.  If not, write to the Free
;; Software Foundation Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary:
;;; Code:
(require 'skk)

;;;###autoload
(defgroup skk-tut nil "SKK tutorial conversion related customization."
  :prefix "skk-tut-"
  :group 'skk )

;; User variables.  prefix should be `skk-tut-'.
(defcustom skk-tut-file 
  (static-cond ((eq skk-emacs-type 'xemacs) (locate-data-file "SKK.tut"))
	       (t "/usr/local/share/skk/SKK.tut"))
  "*SKK $B%A%e!<%H%j%"%k$N%U%!%$%kL>!#(B
The English version is SKK.tut.E."
  :type 'file
  :group 'skk-tut )

(defvar skk-tut-file-alist
  (` (("Japanese" . (, skk-tut-file))
      ("English" . (, (concat skk-tut-file ".E"))) ))
  "*Alist of `(LANGUAGE . TUTORIAL-FILE)' pairs." )

(defcustom skk-tut-use-face t
  "*Non-nil $B$G$"$l$P!"%A%e!<%H%j%"%k$G(B face $B$rMxMQ$7$?I=<($r9T$J$&!#(B" 
  :type 'boolean
  :group 'skk-tut )

(defface skk-tut-section-face
  '((((class color) (background light))
     (:foreground "yellow" :background "dodgerblue"))
    (((class color) (background dark))
     (:foreground "yellow" :background "slateblue"))
    (((class grayscale)) (:bold t) (:italic t)) )
  "*$B%A%e!<%H%j%"%kCf$N%;%/%7%g%s$NI=<(ItJ,$N(B face$B!#(B" 
  :group 'skk-faces )

(defface skk-tut-do-it-face
  '((((class color) (background light)) (:foreground "DarkGoldenrod"))
    (((class color) (background dark)) (:foreground "LightGoldenrod"))
    (((class grayscale)) (:bold t)) )
  "*$B%A%e!<%H%j%"%kCf$N;X<(9`L\$NI=<(ItJ,$N(B face$B!#(B"
  :group 'skk-faces )

(defface skk-tut-question-face
  '((((class color) (background light)) (:foreground "Blue"))
    (((class color) (background dark)) (:foreground "LightSkyBlue"))
    (((class grayscale)) (:underline t)) )
  "*$B%A%e!<%H%j%"%kCf$NLdBj$NI=<(ItJ,$N(B face$B!#(B"
  :group 'skk-faces )

(defface skk-tut-key-bind-face
  '((((class color) (background light)) (:foreground "Firebrick"))
    (((class color) (background dark)) (:foreground "OrangeRed"))
    (((class grayscale)) (:bold t)) )
  "*$B%A%e!<%H%j%"%kCf$N%-!<%P%$%s%I$NI=<(ItJ,$N(B face$B!#(B"
  :group 'skk-faces )

(defface skk-tut-hint-face
  '((((class color) (background light)) (:foreground "CadetBlue"))
    (((class color) (background dark)) (:foreground "Aquamarine"))
    (((class grayscale)) (:italic t)) )
  "*$B%A%e!<%H%j%"%kCf$N%R%s%H$NI=<(ItJ,$N(B face$B!#(B
$B8=:_$N$H$3$m!"(BSKK.tut.E $B$G$7$+;HMQ$5$l$F$$$J$$!#(B"
  :group 'skk-faces )

;; internal variables and constants.
;; prefix should be `skktut-'.
(defvar skk-tut-section-face 'skk-tut-section-face)
(defvar skk-tut-do-it-face 'skk-tut-do-it-face)
(defvar skk-tut-question-face 'skk-tut-question-face)
(defvar skk-tut-key-bind-face 'skk-tut-key-bind-face)
(defvar skk-tut-hint-face 'skk-tut-hint-face)

(defconst skktut-adviced-alist
  '((skk-abbrev-mode . before) (skk-insert . before)
    (skk-kakutei . before) (skk-mode . before)
    (kill-buffer . around) (other-frame . before)
    (save-buffers-kill-emacs . around)
    ;;(select-frame . before)
    )
  "SKK $B%A%e!<%H%j%"%k$G(B advice $B$,IU$1$i$l$k4X?t$H(B advice class $B$N%(!<%j%9%H!#(B" )

(defconst skktut-question-numbers 37 "SKK $B%A%e!<%H%j%"%k$NLdBj?t!#(B")

(defconst skktut-tut-jisyo "~/skk-tut-jisyo"
  "SKK $B%A%e!<%H%j%"%kMQ$N%@%_!<<-=q!#(B" )

(defconst skktut-init-variables-alist
  '((skk-abbrev-cursor-color . "royalblue")
    (skk-abbrev-mode-string . " a$B$"(B")
    (skk-allow-spaces-newlines-and-tabs . t)
    (skk-auto-fill-mode-hook . nil)
    (skk-auto-insert-paren . nil)
    (skk-auto-okuri-process . nil)
    (skk-auto-start-henkan . nil)
    (skk-byte-compile-init-file . nil)
    (skk-comp-load-hook . nil)
    (skk-compare-jisyo-size-when-saving . nil)
    (skk-completion-function . 'skk-completion-original)
    (skk-convert-okurigana-into-katakana . nil)
    (skk-count-jisyo-candidates-function . 'skk-count-jisyo-candidates-original)
    (skk-count-private-jisyo-candidates-exactly . nil)
    (skk-dabbrev-like-completion . nil)
    (skk-date-ad . 1)
    (skk-default-cursor-color . (if (eq skk-emacs-type 'xemacs)
				    (frame-property (selected-frame) 'cursor-color)
				  (cdr (assq 'cursor-color (frame-parameters (selected-frame)))) ))
    (skk-delete-implies-kakutei . t)
    (skk-delete-okuri-when-quit . nil)
    (skk-downcase-alist . nil)
    (skk-echo . t)
    (skk-egg-like-newline . nil)
    (skk-gadget-load-hook . nil)
    (skk-henkan-face . 'highlight)
    (skk-henkan-okuri-strictly . nil)
    (skk-henkan-overlay-priority . 600)
    (skk-henkan-show-candidates-keys . '(?a ?s ?d ?f ?j ?k ?l))
    (skk-henkan-strict-okuri-precedence . nil)
    (skk-hiragana-mode-string . " $B$+$J(B")
    (skk-init-file . "")
    (skk-input-by-code-menu-keys1 . '(?a ?s ?d ?f ?g ?h ?q ?w ?e ?r ?t ?y))
    (skk-input-by-code-menu-keys2 . '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?q ?w ?e ?r ?t ?y ?u))
    (skk-japanese-message-and-error . nil)
    (skk-jisx0208-latin-cursor-color . "gold")
    (skk-jisx0208-latin-mode-string . " $BA41Q(B")
    (skk-jisx0208-latin-vector . skk-default-jisx0208-latin-vector)
    (skk-jisyo . "~/skk-tut-jisyo")
    (skk-jisyo-save-count . nil)
    (skk-kakutei-early . t)
    (skk-kakutei-key . "\C-j")
    (skk-kana-input-search-function . (function
				       (lambda ()
					 (save-match-data
					   (and (string-match
						 "^h\\([bcdfghjklmnpqrstvwxz]\\)$"
						 skk-prefix )
						(member (char-to-string (preceding-char))
							'("$B$*(B" "$B%*(B") )
						(cons '("$B%*(B" . "$B$*(B") (match-string 1 skk-prefix)) )))))
    (skk-katakana-mode-string . " $B%+%J(B")
    (skk-kcode-load-hook . nil)
    (skk-keep-record . nil)
    (skk-kuten-touten-alist . '((jp . ("$B!#(B" . "$B!"(B"))))
    (skk-kutouten-type . 'jp)
    (skk-latin-cursor-color . "ivory4")
    (skk-latin-mode-string . " SKK")
    (skk-load-hook . nil)
    (skk-mode-hook . nil)
    (skk-next-completion-key . ".")
    (skk-number-style . 1)
    (skk-okuri-char-alist . nil)
    (skk-previous-completion-function . 'skk-previous-completion-original)
    (skk-previous-completion-key . ",")
    (skk-process-okuri-early . nil)
    (skk-public-jisyo-has-entry-p-function . 'skk-public-jisyo-has-entry-p-original)
    (skk-report-set-cursor-error . t)
    (skk-rom-kana-base-rule-list .
				 '(("a" nil ("$B%"(B" . "$B$"(B")) ("bb" "b" ("$B%C(B" . "$B$C(B"))
				   ("ba" nil ("$B%P(B" . "$B$P(B")) ("be" nil ("$B%Y(B" . "$B$Y(B"))
				   ("bi" nil ("$B%S(B" . "$B$S(B")) ("bo" nil ("$B%\(B" . "$B$\(B"))
				   ("bu" nil ("$B%V(B" . "$B$V(B")) ("bya" nil ("$B%S%c(B" . "$B$S$c(B"))
				   ("bye" nil ("$B%S%'(B" . "$B$S$'(B")) ("byi" nil ("$B%S%#(B" . "$B$S$#(B"))
				   ("byo" nil ("$B%S%g(B" . "$B$S$g(B")) ("byu" nil ("$B%S%e(B" . "$B$S$e(B"))
				   ("cc" "c" ("$B%C(B" . "$B$C(B")) ("cha" nil ("$B%A%c(B" . "$B$A$c(B"))
				   ("che" nil ("$B%A%'(B" . "$B$A$'(B")) ("chi" nil ("$B%A(B" . "$B$A(B"))
				   ("cho" nil ("$B%A%g(B" . "$B$A$g(B")) ("chu" nil ("$B%A%e(B" . "$B$A$e(B"))
				   ("cya" nil ("$B%A%c(B" . "$B$A$c(B")) ("cye" nil ("$B%A%'(B" . "$B$A$'(B"))
				   ("cyi" nil ("$B%A%#(B" . "$B$A$#(B")) ("cyo" nil ("$B%A%g(B" . "$B$A$g(B"))
				   ("cyu" nil ("$B%A%e(B" . "$B$A$e(B")) ("dd" "d" ("$B%C(B" . "$B$C(B"))
				   ("da" nil ("$B%@(B" . "$B$@(B")) ("de" nil ("$B%G(B" . "$B$G(B"))
				   ("dha" nil ("$B%G%c(B" . "$B$G$c(B")) ("dhe" nil ("$B%G%'(B" . "$B$G$'(B"))
				   ("dhi" nil ("$B%G%#(B" . "$B$G$#(B")) ("dho" nil ("$B%G%g(B" . "$B$G$g(B"))
				   ("dhu" nil ("$B%G%e(B" . "$B$G$e(B")) ("di" nil ("$B%B(B" . "$B$B(B"))
				   ("do" nil ("$B%I(B" . "$B$I(B")) ("du" nil ("$B%E(B" . "$B$E(B"))
				   ("dya" nil ("$B%B%c(B" . "$B$B$c(B")) ("dye" nil ("$B%B%'(B" . "$B$B$'(B"))
				   ("dyi" nil ("$B%B%#(B" . "$B$B$#(B")) ("dyo" nil ("$B%B%g(B" . "$B$B$g(B"))
				   ("dyu" nil ("$B%B%e(B" . "$B$B$e(B")) ("e" nil ("$B%((B" . "$B$((B"))
				   ("ff" "f" ("$B%C(B" . "$B$C(B")) ("fa" nil ("$B%U%!(B" . "$B$U$!(B"))
				   ("fe" nil ("$B%U%'(B" . "$B$U$'(B")) ("fi" nil ("$B%U%#(B" . "$B$U$#(B"))
				   ("fo" nil ("$B%U%)(B" . "$B$U$)(B")) ("fu" nil ("$B%U(B" . "$B$U(B"))
				   ("fya" nil ("$B%U%c(B" . "$B$U$c(B")) ("fye" nil ("$B%U%'(B" . "$B$U$'(B"))
				   ("fyi" nil ("$B%U%#(B" . "$B$U$#(B")) ("fyo" nil ("$B%U%g(B" . "$B$U$g(B"))
				   ("fyu" nil ("$B%U%e(B" . "$B$U$e(B")) ("gg" "g" ("$B%C(B" . "$B$C(B"))
				   ("ga" nil ("$B%,(B" . "$B$,(B")) ("ge" nil ("$B%2(B" . "$B$2(B"))
				   ("gi" nil ("$B%.(B" . "$B$.(B")) ("go" nil ("$B%4(B" . "$B$4(B"))
				   ("gu" nil ("$B%0(B" . "$B$0(B")) ("gya" nil ("$B%.%c(B" . "$B$.$c(B"))
				   ("gye" nil ("$B%.%'(B" . "$B$.$'(B")) ("gyi" nil ("$B%.%#(B" . "$B$.$#(B"))
				   ("gyo" nil ("$B%.%g(B" . "$B$.$g(B")) ("gyu" nil ("$B%.%e(B" . "$B$.$e(B"))
				   ;;("h" "" ("$B%*(B" . "$B$*(B"))
				   ("ha" nil ("$B%O(B" . "$B$O(B")) ("he" nil ("$B%X(B" . "$B$X(B"))
				   ("hi" nil ("$B%R(B" . "$B$R(B")) ("ho" nil ("$B%[(B" . "$B$[(B"))
				   ("hu" nil ("$B%U(B" . "$B$U(B")) ("hya" nil ("$B%R%c(B" . "$B$R$c(B"))
				   ("hye" nil ("$B%R%'(B" . "$B$R$'(B")) ("hyi" nil ("$B%R%#(B" . "$B$R$#(B"))
				   ("hyo" nil ("$B%R%g(B" . "$B$R$g(B")) ("hyu" nil ("$B%R%e(B" . "$B$R$e(B"))
				   ("i" nil ("$B%$(B" . "$B$$(B")) ("jj" "j" ("$B%C(B" . "$B$C(B"))
				   ("ja" nil ("$B%8%c(B" . "$B$8$c(B")) ("je" nil ("$B%8%'(B" . "$B$8$'(B"))
				   ("ji" nil ("$B%8(B" . "$B$8(B")) ("jo" nil ("$B%8%g(B" . "$B$8$g(B"))
				   ("ju" nil ("$B%8%e(B" . "$B$8$e(B")) ("jya" nil ("$B%8%c(B" . "$B$8$c(B"))
				   ("jye" nil ("$B%8%'(B" . "$B$8$'(B")) ("jyi" nil ("$B%8%#(B" . "$B$8$#(B"))
				   ("jyo" nil ("$B%8%g(B" . "$B$8$g(B")) ("jyu" nil ("$B%8%e(B" . "$B$8$e(B"))
				   ("kk" "k" ("$B%C(B" . "$B$C(B")) ("ka" nil ("$B%+(B" . "$B$+(B"))
				   ("ke" nil ("$B%1(B" . "$B$1(B")) ("ki" nil ("$B%-(B" . "$B$-(B"))
				   ("ko" nil ("$B%3(B" . "$B$3(B")) ("ku" nil ("$B%/(B" . "$B$/(B"))
				   ("kya" nil ("$B%-%c(B" . "$B$-$c(B")) ("kye" nil ("$B%-%'(B" . "$B$-$'(B"))
				   ("kyi" nil ("$B%-%#(B" . "$B$-$#(B")) ("kyo" nil ("$B%-%g(B" . "$B$-$g(B"))
				   ("kyu" nil ("$B%-%e(B" . "$B$-$e(B")) ("mm" "c" ("$B%C(B" . "$B$C(B"))
				   ("ma" nil ("$B%^(B" . "$B$^(B")) ("me" nil ("$B%a(B" . "$B$a(B"))
				   ("mi" nil ("$B%_(B" . "$B$_(B")) ("mo" nil ("$B%b(B" . "$B$b(B"))
				   ("mu" nil ("$B%`(B" . "$B$`(B")) ("mya" nil ("$B%_%c(B" . "$B$_$c(B"))
				   ("mye" nil ("$B%_%'(B" . "$B$_$'(B")) ("myi" nil ("$B%_%#(B" . "$B$_$#(B"))
				   ("myo" nil ("$B%_%g(B" . "$B$_$g(B")) ("myu" nil ("$B%_%e(B" . "$B$_$e(B"))
				   ("n" nil ("$B%s(B" . "$B$s(B")) ("n'" nil ("$B%s(B" . "$B$s(B"))
				   ("na" nil ("$B%J(B" . "$B$J(B")) ("ne" nil ("$B%M(B" . "$B$M(B"))
				   ("ni" nil ("$B%K(B" . "$B$K(B")) ("nn" nil ("$B%s(B" . "$B$s(B"))
				   ("no" nil ("$B%N(B" . "$B$N(B")) ("nu" nil ("$B%L(B" . "$B$L(B"))
				   ("nya" nil ("$B%K%c(B" . "$B$K$c(B")) ("nye" nil ("$B%K%'(B" . "$B$K$'(B"))
				   ("nyi" nil ("$B%K%#(B" . "$B$K$#(B")) ("nyo" nil ("$B%K%g(B" . "$B$K$g(B"))
				   ("nyu" nil ("$B%K%e(B" . "$B$K$e(B")) ("o" nil ("$B%*(B" . "$B$*(B"))
				   ("pp" "p" ("$B%C(B" . "$B$C(B")) ("pa" nil ("$B%Q(B" . "$B$Q(B"))
				   ("pe" nil ("$B%Z(B" . "$B$Z(B")) ("pi" nil ("$B%T(B" . "$B$T(B"))
				   ("po" nil ("$B%](B" . "$B$](B")) ("pu" nil ("$B%W(B" . "$B$W(B"))
				   ("pya" nil ("$B%T%c(B" . "$B$T$c(B")) ("pye" nil ("$B%T%'(B" . "$B$T$'(B"))
				   ("pyi" nil ("$B%T%#(B" . "$B$T$#(B")) ("pyo" nil ("$B%T%g(B" . "$B$T$g(B"))
				   ("pyu" nil ("$B%T%e(B" . "$B$T$e(B")) ("rr" "r" ("$B%C(B" . "$B$C(B"))
				   ("ra" nil ("$B%i(B" . "$B$i(B")) ("re" nil ("$B%l(B" . "$B$l(B"))
				   ("ri" nil ("$B%j(B" . "$B$j(B")) ("ro" nil ("$B%m(B" . "$B$m(B"))
				   ("ru" nil ("$B%k(B" . "$B$k(B")) ("rya" nil ("$B%j%c(B" . "$B$j$c(B"))
				   ("rye" nil ("$B%j%'(B" . "$B$j$'(B")) ("ryi" nil ("$B%j%#(B" . "$B$j$#(B"))
				   ("ryo" nil ("$B%j%g(B" . "$B$j$g(B")) ("ryu" nil ("$B%j%e(B" . "$B$j$e(B"))
				   ("ss" "s" ("$B%C(B" . "$B$C(B")) ("sa" nil ("$B%5(B" . "$B$5(B"))
				   ("se" nil ("$B%;(B" . "$B$;(B")) ("sha" nil ("$B%7%c(B" . "$B$7$c(B"))
				   ("she" nil ("$B%7%'(B" . "$B$7$'(B")) ("shi" nil ("$B%7(B" . "$B$7(B"))
				   ("sho" nil ("$B%7%g(B" . "$B$7$g(B")) ("shu" nil ("$B%7%e(B" . "$B$7$e(B"))
				   ("si" nil ("$B%7(B" . "$B$7(B")) ("so" nil ("$B%=(B" . "$B$=(B"))
				   ("su" nil ("$B%9(B" . "$B$9(B")) ("sya" nil ("$B%7%c(B" . "$B$7$c(B"))
				   ("sye" nil ("$B%7%'(B" . "$B$7$'(B")) ("syi" nil ("$B%7%#(B" . "$B$7$#(B"))
				   ("syo" nil ("$B%7%g(B" . "$B$7$g(B")) ("syu" nil ("$B%7%e(B" . "$B$7$e(B"))
				   ("tt" "t" ("$B%C(B" . "$B$C(B")) ("ta" nil ("$B%?(B" . "$B$?(B"))
				   ("te" nil ("$B%F(B" . "$B$F(B")) ("tha" nil ("$B%F%!(B" . "$B$F$!(B"))
				   ("the" nil ("$B%F%'(B" . "$B$F$'(B")) ("thi" nil ("$B%F%#(B" . "$B$F$#(B"))
				   ("tho" nil ("$B%F%g(B" . "$B$F$g(B")) ("thu" nil ("$B%F%e(B" . "$B$F$e(B"))
				   ("ti" nil ("$B%A(B" . "$B$A(B")) ("to" nil ("$B%H(B" . "$B$H(B"))
				   ("tsu" nil ("$B%D(B" . "$B$D(B")) ("tu" nil ("$B%D(B" . "$B$D(B"))
				   ("tya" nil ("$B%A%c(B" . "$B$A$c(B")) ("tye" nil ("$B%A%'(B" . "$B$A$'(B"))
				   ("tyi" nil ("$B%A%#(B" . "$B$A$#(B")) ("tyo" nil ("$B%A%g(B" . "$B$A$g(B"))
				   ("tyu" nil ("$B%A%e(B" . "$B$A$e(B")) ("u" nil ("$B%&(B" . "$B$&(B"))
				   ("vv" "v" ("$B%C(B" . "$B$C(B")) ("va" nil ("$B%t%!(B" . "$B$&!+$!(B"))
				   ("ve" nil ("$B%t%'(B" . "$B$&!+$'(B")) ("vi" nil ("$B%t%#(B" . "$B$&!+$#(B"))
				   ("vo" nil ("$B%t%)(B" . "$B$&!+$)(B")) ("vu" nil ("$B%t(B" . "$B$&!+(B"))
				   ("ww" "w" ("$B%C(B" . "$B$C(B")) ("wa" nil ("$B%o(B" . "$B$o(B"))
				   ("we" nil ("$B%&%'(B" . "$B$&$'(B")) ("wi" nil ("$B%&%#(B" . "$B$&$#(B"))
				   ("wo" nil ("$B%r(B" . "$B$r(B")) ("wu" nil ("$B%&(B" . "$B$&(B"))
				   ("xx" "x" ("$B%C(B" . "$B$C(B")) ("xa" nil ("$B%!(B" . "$B$!(B"))
				   ("xe" nil ("$B%'(B" . "$B$'(B")) ("xi" nil ("$B%#(B" . "$B$#(B"))
				   ("xka" nil ("$B%u(B" . "$B$+(B")) ("xke" nil ("$B%v(B" . "$B$1(B"))
				   ("xo" nil ("$B%)(B" . "$B$)(B")) ("xtsu" nil ("$B%C(B" . "$B$C(B"))
				   ("xtu" nil ("$B%C(B" . "$B$C(B")) ("xu" nil ("$B%%(B" . "$B$%(B"))
				   ("xwa" nil ("$B%n(B" . "$B$n(B")) ("xwe" nil ("$B%q(B" . "$B$q(B"))
				   ("xwi" nil ("$B%p(B" . "$B$p(B")) ("xya" nil ("$B%c(B" . "$B$c(B"))
				   ("xyo" nil ("$B%g(B" . "$B$g(B")) ("xyu" nil ("$B%e(B" . "$B$e(B"))
				   ("yy" "y" ("$B%C(B" . "$B$C(B")) ("ya" nil ("$B%d(B" . "$B$d(B"))
				   ("ye" nil ("$B%$%'(B" . "$B$$$'(B")) ("yo" nil ("$B%h(B" . "$B$h(B"))
				   ("yu" nil ("$B%f(B" . "$B$f(B")) ("zz" "z" ("$B%C(B" . "$B$C(B"))
				   ("z," nil "$B!E(B") ("z-" nil "$B!A(B") ("z." nil "$B!D(B")
				   ("z/" nil "$B!&(B") ("z[" nil "$B!X(B") ("z]" nil "$B!Y(B")
				   ("za" nil ("$B%6(B" . "$B$6(B")) ("ze" nil ("$B%<(B" . "$B$<(B"))
				   ("zh" nil "$B"+(B") ("zi" nil ("$B%8(B" . "$B$8(B"))
				   ("zj" nil "$B"-(B") ("zk" nil "$B",(B") ("zl" nil "$B"*(B")
				   ("zo" nil ("$B%>(B" . "$B$>(B")) ("zu" nil ("$B%:(B" . "$B$:(B"))
				   ("zya" nil ("$B%8%c(B" . "$B$8$c(B")) ("zye" nil ("$B%8%'(B" . "$B$8$'(B"))
				   ("zyi" nil ("$B%8%#(B" . "$B$8$#(B")) ("zyo" nil ("$B%8%g(B" . "$B$8$g(B"))
				   ("zyu" nil ("$B%8%e(B" . "$B$8$e(B")) ("." nil skk-current-kuten)
				   ("," nil skk-current-touten) ("-" nil "$B!<(B")
				   (":" nil "$B!'(B") (";" nil "$B!((B") ("?" nil "$B!)(B")
				   ("[" nil "$B!V(B") ("]" nil "$B!W(B") ("l" nil skk-latin-mode)
				   ("q" nil skk-toggle-kana) ("L" nil skk-jisx0208-latin-mode)
				   ("Q" nil skk-set-henkan-point-subr)
				   ("X" nil skk-purge-from-jisyo) ("/" nil skk-abbrev-mode)
				   ("$" nil skk-display-code-for-char-at-point)
				   ("@" nil skk-today) ("\\" nil skk-input-by-code-or-menu) ))
    (skk-rom-kana-rule-list . '(("hh" "h" ("$B%C(B" . "$B$C(B"))))
    (skk-save-jisyo-function . 'skk-save-jisyo-original)
    (skk-search-excluding-word-pattern-function . nil)
    (skk-search-prog-list . '((skk-search-jisyo-file skktut-tut-jisyo 0 t)))
    (skk-set-henkan-point-key . '(?A ?B ?C ?D ?E ?F ?G ?H ?I ?J ?K ?M ?N ?O ?P ?R ?S ?T ?U ?V ?W ?Y ?Z))
    (skk-special-midashi-char-list . '(?> ?< ??))
    (skk-start-henkan-key . " ")
    (skk-try-completion-key . "\t")
    (skk-update-jisyo-function . 'skk-update-jisyo-original)
    (skk-use-color-cursor . (and window-system (fboundp 'x-display-color-p) (x-display-color-p)))
    (skk-use-cursor-change . t)
    (skk-use-face . (or window-system (skk-terminal-face-p)))
    (skk-use-look . nil)
    (skk-use-numeric-conversion . t)
    (skk-use-rdbms . nil)
    (skk-use-relation . nil)
    (skk-use-viper . nil)

    ;; not user variables but to be localized.
    (skk-insert-new-word-function . nil)
    (skk-mode-invoked . t)
    (skk-rule-tree
     .
     (skk-compile-rule-list skk-rom-kana-base-rule-list skk-rom-kana-rule-list) ))
  "skk.el $B$N%f!<%6!<JQ?t$N%j%9%H!#(B" )

(defvar skktut-japanese-tut nil
  "Non-nil $B$G$"$l$P!"%A%e!<%H%j%"%k$,F|K\8l$G$"$k$3$H$r<($9!#(B" )
(defvar skktut-right-answer nil "$B@52r$NJ8;zNs!#(B")
(defvar skktut-question-count 1 "$B%A%e!<%H%j%"%k$N8=:_$NLdBjHV9f!#(B")
(defvar skktut-tutorial-end nil "$B%A%e!<%H%j%"%k$N=*N;$r<($9%U%i%0!#(B")
(defvar skktut-working-buffer " *skk-tutorial*")
(defvar skktut-question-buffer "*$BLd(B*")
(defvar skktut-answer-buffer "*$BEz(B*")
(defvar skktut-jisyo-buffer " *skk-tut-jisyo*")
(defvar skktut-original-window-configuration nil)
(defvar skktut-working-window-configuration nil)
(defvar skktut-skk-mode-on nil
  "Non-nil $B$G$"$l$P!"(Bskk-tutorial $B$r5/F0$7$?$H$-$K(B SKK $B$,4{$K5/F0$5$l$F$$$?$3$H$r<($9!#(B" )

(defvar skktut-latin-mode-map nil
  "SKK $B%A%e!<%H%j%"%k(B ASCII $B%b!<%I%-!<%^%C%W!#(B" )

(or skktut-latin-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map "\C-j" 'skk-kakutei)
      (setq skktut-latin-mode-map map) ))

(defvar skktut-j-mode-map nil
  "SKK $B%A%e!<%H%j%"%k$+$J(B/$B%+%J%b!<%I%-!<%^%C%W!#(B" )

(or skktut-j-mode-map
    (let ((map (make-sparse-keymap)))
      (substitute-key-definition 'self-insert-command 'skk-insert map
				 global-map)
      (substitute-key-definition 'egg-self-insert-command 'skk-insert map
				 global-map)
      (substitute-key-definition 'canna-self-insert-command 'skk-insert map
				 global-map)
      (substitute-key-definition 'can-n-egg-self-insert-command 'skk-insert map
				 global-map)
      (define-key map "x" 'skk-previous-candidate)
      (define-key map "\C-j" 'skk-kakutei)
      (define-key map "\t" 'skk-insert)
      (setq skktut-j-mode-map map) ))

(defvar skktut-jisx0208-latin-mode-map nil
  "SKK $B%A%e!<%H%j%"%kA43Q1Q?t;z%b!<%I%-!<%^%C%W!#(B" )

(or skktut-jisx0208-latin-mode-map
    (let ((map (make-sparse-keymap))
	  (i 0) )
      (while (< i 128)
	(if (aref skk-jisx0208-latin-vector i)
	    (define-key map (char-to-string i) 'skk-jisx0208-latin-insert) )
	(setq i (1+ i)) )
      (define-key map "\C-j" 'skk-kakutei)
      (setq skktut-jisx0208-latin-mode-map map) ))

(defvar skktut-abbrev-mode-map nil
  "SKK $B%A%e!<%H%j%"%k(B Abbrev $B%b!<%I%-!<%^%C%W!#(B" )

(or skktut-abbrev-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map "," 'skk-abbrev-comma)
      (define-key map "." 'skk-abbrev-period)
      (define-key map "\C-q" 'skk-jisx0208-latin-henkan)
      (define-key map "\C-j" 'skk-kakutei)
      (define-key map " " 'skk-start-henkan)
      (define-key map "\t" 'skk-try-completion)
      (setq skktut-abbrev-mode-map map) ))

;; -- macros
(defmacro skktut-message (japanese english &rest arg)
  ;; skktut-japanese-tut $B$,(B non-nil $B$@$C$?$i(B JAPANESE $B$r(B nil $B$G$"$l$P(B ENGLISH
  ;; $B$r%(%3!<%(%j%"$KI=<($9$k!#(B
  ;; ARG $B$O(B message $B4X?t$NBh#20z?t0J9_$N0z?t$H$7$FEO$5$l$k!#(B
  (append (list 'message (list 'if 'skktut-japanese-tut japanese english))
          arg ))

(defmacro skktut-error (japanese english &rest arg)
  ;; skktut-japanese-tut $B$,(B non-nil $B$@$C$?$i(B JAPANESE $B$r(B nil $B$G$"$l$P(B ENGLISH
  ;; $B$r%(%3!<%(%j%"$KI=<($7!"%(%i!<$rH/@8$5$;$k!#(B
  ;; ARG $B$O(B error $B4X?t$NBh#20z?t0J9_$N0z?t$H$7$FEO$5$l$k!#(B
  (append (list 'error (list 'if 'skktut-japanese-tut japanese english))
          arg ))

(defmacro skktut-yes-or-no-p (japanese english)
  (list 'yes-or-no-p (list 'if 'skktut-japanese-tut japanese english)) )

;; advices.
(defadvice skk-abbrev-mode (before skktut-ad disable)
  "SKK $B%A%e!<%H%j%"%kMQ%"%I%P%$%9IU!#(B"
  (and (> 12 skktut-question-count)
       (skktut-error "$B$3$N%-!<$O$^$@;H$($^$;$s(B" "Cannot use this key yet" ) ))

(defadvice skk-insert (before skktut-ad disable)
  "SKK $B%A%e!<%H%j%"%kMQ%"%I%P%$%9IU!#(B"
  (and (memq last-command-char skk-set-henkan-point-key)
       (> 12 skktut-question-count)
       (skktut-error "$B$+$J(B/$B%+%J%b!<%I$G$O!"1QBgJ8;z$O$^$@;H$($^$;$s(B"
		     "Cannot use upper case character in kana/katakana mode" ) ))

(defadvice skk-kakutei (before skktut-ad disable)
  "SKK $B%A%e!<%H%j%"%kMQ%"%I%P%$%9IU!#(B"
  (and (interactive-p)
       (= skktut-question-count 1)
       (skktut-error "$B$3$N%-!<$O$^$@;H$($^$;$s(B" "Cannot use this key yet" ) ))

(defadvice skk-mode (before skktut-ad disable)
  "SKK $B%A%e!<%H%j%"%kMQ%"%I%P%$%9IU!#(B"
  (and (interactive-p)
       (= skktut-question-count 1)
       (skktut-error "$B$3$N%-!<$O$^$@;H$($^$;$s(B" "Cannot use this key yet" ) ))

(defadvice kill-buffer (around skktut-ad disable)
  "SKK $B%A%e!<%H%j%"%kMQ%"%I%P%$%9IU!#(B"
  (cond ((or (not (interactive-p))
	     (null (member (ad-get-arg 0) (list skktut-working-buffer
						skktut-question-buffer
						skktut-answer-buffer
						skktut-jisyo-buffer ))))
	 ad-do-it )
	((skktut-yes-or-no-p "$B%A%e!<%H%j%"%k$r$d$a$^$9$+(B? "
			     "Quit tutorial? " )
	 (skk-tutorial-quit 'now)
	 ;; already killed.
	 ;;ad-do-it
	 )))

(defadvice other-frame (before skktut-ad disable)
  "SKK $B%A%e!<%H%j%"%kMQ%"%I%P%$%9IU!#(B"
  (skktut-before-move-to-other-frame) )

;;(defadvice select-frame (before skktut-ad disable)
;;(defadvice select-frame (before skktut-ad activate)
;;  "SKK $B%A%e!<%H%j%"%kMQ%"%I%P%$%9IU!#(B"
;;  (skktut-before-move-to-other-frame) )

(defadvice save-buffers-kill-emacs (around skktut-ad disable)
  "SKK $B%A%e!<%H%j%"%kMQ%"%I%P%$%9IU!#(B"
  (if (skktut-yes-or-no-p "Tutorial $B$b(B Emacs $B$b=*N;$7$^$9!#$h$m$7$$$G$9$M!)(B "
			  "Quit tutorial and kill emacs? " )
      (progn (skk-tutorial-quit 'now)
	     ad-do-it )))

;; interactive commands. prefix should be `skk-tutorial'.
;;;###autoload
(defun skk-tutorial (&optional query-language)
  "SKK $B%A%e!<%H%j%"%k$r5/F0$9$k!#(B
C-u M-x skk-tutorial $B$9$k$H!"%A%e!<%H%j%"%k%U%!%$%k$NA*Br$,2DG=!#(B"
  (interactive "P")
  (if query-language
      (let* ((lang (completing-read "Language: " skk-tut-file-alist))
	     (file (cdr (assoc lang skk-tut-file-alist))) )
	(if (not (file-exists-p (expand-file-name file)))
	    (error "No file found as %s" file)
	  (setq skk-tut-file file)
	  (message "SKK tutorial language set to %s until you exit Emacs"
		   lang ))))
  (let ((inhibit-quit t))
    (if (not (and (boundp 'skk-major-version) (boundp 'skk-minor-version)
		  (>= skk-major-version 10) (>= skk-minor-version 46) ))
        (error "skk.el version 10.46 or later is required")
      (skktut-pre-setup-tutorial)
      (skktut-setup-jisyo-buffer)
      (skktut-setup-working-buffer)
      (skktut-setup-question-buffer)
      (skktut-setup-answer-buffer)
      (skktut-enable-advice)
      (skktut-enable-tutmap)
      (add-hook 'before-make-frame-hook 'skktut-before-move-to-other-frame)
      (add-hook 'minibuffer-setup-hook 'skktut-localize-and-init-variables)
      (skktut-make-windows) )))

(defun skk-tutorial-again (&optional now)
  "SKK $B%A%e!<%H%j%"%k$r:G=i$+$i$d$jD>$9!#(B
C-u M-x skk-tutorial-again $B$9$k$H!"(Byes-or-no-p $B$G?R$M$i$l$k$3$H$J$/D>$A$K$d$jD>$9!#(B"
 (interactive "P")
  (if (or now
	  (skktut-yes-or-no-p "$B:G=i$+$i(B Tutorial $B$r$d$jD>$7$^$9!#$h$m$7$$$G$9$M!)(B "
			      "Quit tutorial and start from question 1 again? " ))
      (progn (skk-tutorial-quit 'now)
             (skk-tutorial) )))

(defun skk-tutorial-quit (&optional now)
  "SKK $B%A%e!<%H%j%"%k$r$d$a$k!#(B
C-u M-x skk-tutorial-quit $B$9$k$H!"(Byes-or-no-p $B$G?R$M$i$l$k$3$H$J$/D>$A$K$d$a$k!#(B"
  (interactive "P")
  (if (or now (skktut-yes-or-no-p "$BK\Ev$K%A%e!<%H%j%"%k$r$d$a$^$9$+(B? "
                                  "Really quit tutorial? " ))
      (let ((inhibit-quit t))
        (delete-other-windows)
        ;; $B:FEY%A%e!<%H%j%"%k$r;H$($k$h$&$K!"FbItJQ?t$r=i4|2=$7$F$*$/!#(B
        (setq skktut-japanese-tut nil
              skktut-question-count 1
              skktut-right-answer nil
              skktut-tutorial-end nil )
        (remove-hook 'minibuffer-setup-hook 'skktut-localize-and-init-variables)
        (remove-hook 'before-make-frame-hook 'skktut-before-move-to-other-frame)
	(skktut-disable-tutmap)
	(skktut-disable-advice)
	(save-excursion
	  (set-buffer skktut-jisyo-buffer)
	  (set-buffer-modified-p nil)
	  (kill-buffer skktut-jisyo-buffer) )
        (kill-buffer skktut-working-buffer)
        (kill-buffer skktut-answer-buffer)
        (kill-buffer skktut-question-buffer)
	(set-window-configuration skktut-original-window-configuration)
        ;; SKK $B$r5/F0$;$:$K$$$-$J$j(B
        ;; skk-tutorial $B$r<B9T$7$?$H$-$K(B skk-jisyo $B%P%C%U%!$,:n$i$l$J$$$N$G(B
        ;; skk-setup-jisyo-buffer $B$G%(%i!<$H$J$j!"(BEmacs $B$N=*N;$,$G$-$J$/(B
        ;; $B$J$k$N$G(B SKK $B%b!<%I$r0lEY5/$3$7$F$*$/!#(B
        (skk-mode 1)
        ;; $B%A%e!<%H%j%"%k5/F0D>A0$K3+$$$F$$$?%P%C%U%!$G!"(Bskk-mode $B$r5/F0$7$F(B
        ;; $B$$$?$i!"$=$N>uBV$K$7$F!"%A%e!<%H%j%"%k$r=*N;$9$k!#(B
        (or skktut-skk-mode-on
            (skk-mode -1) ))))

;; the following commands are also interactive, but users may not call
;; them by name.  So prefix should be `skktut-'.
(defun skktut-next-question ()
  (interactive)
  (set-window-configuration skktut-working-window-configuration)
  ;; called in skktut-answer-buffer.
  (save-match-data
    (let (user-ans)
      (goto-char (point-min))
      (end-of-line)
      (skip-chars-backward " \t")
      (setq user-ans (buffer-substring-no-properties (point-min) (point)))
      (if (string-match "^>* *" user-ans)
	  (setq user-ans (substring user-ans (match-end 0))) )
      (if (not (string= skktut-right-answer user-ans))
	  (progn
	    (skktut-message "$BEz$,0c$$$^$9!#$b$&0lEY$d$C$F$_$F2<$5$$(B"
			    "Wrong.  Try again")
	    (ding) )
	(setq skktut-question-count (1+ skktut-question-count))
	;; buffer independent.
	(skktut-get-question-page skktut-question-count)
	(if (>= skktut-question-count (1+ skktut-question-numbers))
	    (skk-tutorial-quit 'now)
	  (skktut-next-answer-buffer) )))))

(defun skktut-skip-question (arg)
  (interactive "p")
  (set-window-configuration skktut-working-window-configuration)
  ;; called in skktut-answer-buffer.
  (skktut-erase-buffer)
  (setq skktut-question-count (+ skktut-question-count arg))
  (cond ((> 1 skktut-question-count)
	 (setq skktut-question-count 1) )
	;; overrun
	((> skktut-question-count skktut-question-numbers)
	 (setq skktut-question-count skktut-question-numbers) )
	((and (>= skktut-question-count 3) (not skk-j-mode))
	 (skk-mode 1) ))
  ;; buffer independent.
  (skktut-get-question-page skktut-question-count)
  (if skktut-tutorial-end
      (skk-tutorial-quit 'now)
    (skktut-next-answer-buffer) ))

;; internal functions.  prefix should be `skktut-'.
(defun skktut-make-windows ()
  ;; Make window fill its frame.
  (delete-other-windows)
  (split-window-vertically)
  (other-window 1)
  ;; make it selected window and current buffer.
  (switch-to-buffer skktut-answer-buffer)
  (enlarge-window (- (window-height (selected-window)) 20))
  ;; not make it current buffer but visible.
  (display-buffer skktut-question-buffer)
  (setq skktut-working-window-configuration (current-window-configuration)) )

(defun skktut-enable-advice ()
  (let ((alist skktut-adviced-alist)
	 e )
    (while alist
      (setq e (car alist) )
      (ad-enable-advice (car e) (cdr e) 'skktut-ad)
      (ad-activate (car e))
      (setq alist (cdr alist)) )))

(defun skktut-disable-advice ()
  (let ((alist skktut-adviced-alist)
	 e )
    (while alist
      (setq e (car alist) )
      (ad-disable-advice (car e) (cdr e) 'skktut-ad)
      (ad-activate (car e))
      (setq alist (cdr alist)) )))

(defun skktut-enable-tutmap ()
  (let ((inhibit-quit t))
    (set-modified-alist
     'minor-mode-map-alist
     ;; tut map
     (list (cons 'skk-latin-mode skktut-latin-mode-map)
	   (cons 'skk-abbrev-mode skktut-abbrev-mode-map)
	   (cons 'skk-j-mode skktut-j-mode-map)
	   (cons 'skk-jisx0208-latin-mode skktut-jisx0208-latin-mode-map) ))
    ;; for minor-mode-map-alist localized by Viper.
    (if (not (featurep 'viper))
	nil
      (if (if (eq skk-emacs-type 'xemacs)
	      (local-variable-p 'minor-mode-map-alist nil t)
	    (local-variable-p 'minor-mode-map-alist) )
	  (setq-default minor-mode-map-alist minor-mode-map-alist) ))))

(defun skktut-disable-tutmap ()
  (let ((inhibit-quit t)
	(minor-mode-list
	 '(skk-abbrev-mode skk-latin-mode skk-j-mode skk-jisx0208-latin-mode) )
	minor-mode e )
    (while minor-mode-list
      (setq minor-mode (car minor-mode-list)
	    minor-mode-list (cdr minor-mode-list) )
      ;; fail safe.
      (while (setq e (assq minor-mode minor-mode-map-alist))
	(setq minor-mode-map-alist (delq e minor-mode-map-alist)) ))
    (set-modified-alist
     'minor-mode-map-alist
     (list (cons 'skk-latin-mode skk-latin-mode-map)
	   (cons 'skk-abbrev-mode skk-abbrev-mode-map)
	   (cons 'skk-j-mode skk-j-mode-map)
	   (cons 'skk-jisx0208-latin-mode skk-jisx0208-latin-mode-map) )))
  ;; for minor-mode-map-alist localized by Viper.
  (and (default-value skk-use-viper) (skk-viper-normalize-map)) )

(defun skktut-pre-setup-tutorial ()
  (setq skktut-original-window-configuration (current-window-configuration)
	skktut-skk-mode-on skk-mode
	skktut-question-count 1 ))

(defun skktut-setup-jisyo-buffer ()
  ;; setup skktut-tut-jisyo buffer.
  (save-excursion
    (set-buffer (get-buffer-create skktut-jisyo-buffer))
    (buffer-disable-undo (current-buffer))
    (skktut-localize-and-init-variables)
    (setq case-fold-search nil
	  buffer-file-name (expand-file-name skktut-tut-jisyo) )
    (insert (concat ";; okuri-ari entries.\n"
		    "$B$[$C(Bs /$BM_(B/\n"
		    "$B$D$+(Bt /$B;H(B/\n"
		    "$B$?$C(Bs /$BC#(B/\n"
		    "$B$7(Bt /$BCN(B/\n"
		    "$B$&$4(Bk /$BF0(B/\n"
		    ";; okuri-nasi entries.\n"
		    "Greek /$B&!(B/$B&"(B/$B&#(B/$B&$(B/$B&%(B/$B&&(B/$B&'(B/$B&((B/$B&)(B/$B&*(B/$B&+(B/$B&,(B/$B&-(B/$B&.(B/$B&/(B/$B&0(B/"
		    "$B&1(B/$B&2(B/$B&3(B/$B&4(B/$B&5(B/$B&6(B/$B&7(B/$B&8(B/\n"
		    "Russia /$B'!(B/$B'"(B/$B'#(B/$B'$(B/$B'%(B/$B'&(B/$B''(B/$B'((B/$B')(B/$B'*(B/$B'+(B/$B',(B/$B'-(B/$B'.(B/$B'/(B/$B'0(B/"
		    "$B'1(B/$B'2(B/$B'3(B/$B'4(B/$B'5(B/$B'6(B/$B'7(B/$B'8(B/$B'9(B/$B':(B/$B';(B/$B'<(B/$B'=(B/$B'>(B/$B'?(B/$B'@(B/$B'A(B/\n"
		    "greek /$B&A(B/$B&B(B/$B&C(B/$B&D(B/$B&E(B/$B&F(B/$B&G(B/$B&H(B/$B&I(B/$B&J(B/$B&K(B/$B&L(B/$B&M(B/$B&N(B/$B&O(B/$B&P(B/"
		    "$B&Q(B/$B&R(B/$B&S(B/$B&T(B/$B&U(B/$B&V(B/$B&W(B/$B&X(B/\n"
		    "russia /$B'Q(B/$B'R(B/$B'S(B/$B'T(B/$B'U(B/$B'V(B/$B'W(B/$B'X(B/$B'Y(B/$B'Z(B/$B'[(B/$B'\(B/$B'](B/$B'^(B/$B'_(B/$B'`(B/"
		    "$B'a(B/$B'b(B/$B'c(B/$B'd(B/$B'e(B/$B'f(B/$B'g(B/$B'h(B/$B'i(B/$B'j(B/$B'k(B/$B'l(B/$B'm(B/$B'n(B/$B'o(B/$B'p(B/$B'q(B/\n"
		    "$B$$$A$*$/(B /$B0l2/(B/\n"
		    "$B$*$*$5$+(B /$BBg:e(B/\n"
		    "$B$+$J(B /$B2>L>(B/\n"
		    "$B$+$s$8(B /$B4A;z(B/$B44;v(B/$B4F;v(B/\n"
		    "$B$,$/$7$e$&(B /$B3X=,(B/\n"
		    "$B$-(B /$B4p(B/$B5-(B/$B5$(B/$BLZ(B/$B5"(B/\n"
		    "$B$-$4$&(B /$B5-9f(B/$B!"(B/$B!#(B/$B!$(B/$B!%(B/$B!&(B/$B!'(B/$B!((B/$B!)(B/$B!*(B/$B!+(B/$B!,(B/$B!-(B/$B!.(B/$B!/(B/"
		    "$B!0(B/$B!1(B/$B!2(B/$B!3(B/$B!4(B/$B!5(B/$B!6(B/$B!7(B/$B!8(B/$B!9(B/$B!:(B/$B!;(B/$B!<(B/$B!=(B/$B!>(B/$B!?(B/$B!@(B/$B!A(B/"
		    "$B!B(B/$B!C(B/$B!D(B/$B!E(B/$B!F(B/$B!G(B/$B!H(B/$B!I(B/$B!J(B/$B!K(B/$B!L(B/$B!M(B/$B!N(B/$B!O(B/$B!P(B/$B!Q(B/$B!R(B/$B!S(B/"
		    "$B!T(B/$B!U(B/$B!V(B/$B![(B/$B!X(B/$B!Y(B/$B!Z(B/$B![(B/$B!\(B/$B!](B/$B!^(B/$B!_(B/$B!`(B/$B!a(B/$B!b(B/$B!c(B/$B!d(B/$B!e(B/$B!f(B/"
		    "$B!g(B/$B!h(B/$B!i(B/$B!j(B/$B!k(B/$B!l(B/$B!m(B/$B!n(B/$B!o(B/$B!p(B/$B!q(B/$B!r(B/$B!s(B/$B!t(B/$B!u(B/$B!v(B/$B!w(B/$B!x(B/$B!y(B/"
		    "$B!z(B/$B!{(B/$B!|(B/$B!}(B/$B!~(B/$B"!(B/$B""(B/$B"#(B/$B"$(B/$B"%(B/$B"&(B/$B"'(B/$B"((B/$B")(B/$B"*(B/$B"+(B/$B",(B/$B"-(B/"
		    "$B".(B/\n"
		    "$B$-$g$&$H(B /$B5~ET(B/\n"
		    "$B$3$&$Y(B /$B?@8M(B/\n"
		    "$B$4(B /$B8^(B/$B8_(B/$B8`(B/$B8a(B/$B8b(B/$B8c(B/$B8d(B/$B8e(B/$B8f(B/$B8g(B/$B8h(B/$B8i(B/$B8j(B/$B8k(B/$B8l(B/$B8m(B/$B8n(B/"
		    "$B8o(B/\n"
		    "$B$5$$(B /$B:Y(B/$B:G(B/$B:F(B/\n"
		    "$B$5$$$7$g(B /$B:G=i(B/\n"
		    "$B$5$$$H$&(B /$B:XF#(B/\n"
		    "$B$5$H$&(B /$B:4F#(B/\n"
		    "$B$7$e$&$j$g$&(B /$B=*N;(B/\n"
		    "$B$8$7$g(B /$B<-=q(B/$BCO=j(B/\n"
		    "$B$8$s$3$&(B /$B?M8}(B/\n"
		    "$B$;$s$?$/(B /$BA*Br(B/$B@vBu(B/\n"
		    "$B$=$&(B /$BAv(B/\n"
		    "$B$@$$(B /$BBg(B/$BBh(B/$BBe(B/\n"
		    "$B$F$-(B /$BE*(B/$BE((B/$BE)(B/$BE,(B/$BE&(B/\n"
		    "$B$H$&(B /$BEl(B/\n"
		    "$B$H$&$[$/(B /$BElKL(B/\n"
		    "$B$H$&$m$/(B /$BEPO?(B/\n"
		    "$B$H$&$m$/(B /$BEPO?(B/\n"
		    "$B$I$&(B /$BF0(B/\n"
		    "$B$K$e$&$j$g$/(B /$BF~NO(B/\n"
		    "$B$R$3$&$-(B /$BHt9T5!(B/\n"
		    "$B$X$s$+$s(B /$BJQ49(B/\n"
		    "$B$[$/(B /$BKL(B/\n"
		    "$B$_$g$&$8(B /$BL>;z(B/\n"
		    "$B$h$&$$(B /$BMF0W(B/$BMQ0U(B/\n" ))
    (skk-setup-jisyo-buffer) ))

(defun skktut-setup-working-buffer ()
  (save-match-data
    (let (sexp)
      (set-buffer (get-buffer-create skktut-working-buffer))
      (buffer-disable-undo (current-buffer))
      (skktut-localize-and-init-variables)
      (skktut-erase-buffer) ; fail safe.
      (insert-file-contents skk-tut-file)
      (goto-char (point-min))
      (setq skktut-japanese-tut (looking-at ";; SKK Japanese"))
      (while (re-search-forward "^>> \\((.+)\\)$" nil t nil)
        (setq sexp (buffer-substring-no-properties (match-beginning 1)
						   (match-end 1) ))
        (delete-region (match-beginning 1) (match-end 1))
	;; insert evaluated string instead of lisp program.
        (insert (eval (car (read-from-string sexp)))) )
      (goto-char (point-min))
      (if skk-tut-use-face (skktut-colored)) )))

(defun skktut-setup-question-buffer ()
  (save-excursion
    (set-buffer (get-buffer-create skktut-question-buffer))
    (buffer-disable-undo (current-buffer))
    (skktut-erase-buffer) ; fail safe.
    (setq buffer-read-only t)
    (skktut-get-question-page skktut-question-count)
    (local-set-key "\C-xq" 'skk-tutorial-quit)
    (local-set-key "\C-xt" 'skk-tutorial-again)
    (local-set-key "\C-xj" 'skktut-error-command)
    (local-set-key "\C-xn" 'skktut-next-question)
    (local-set-key "\C-xs" 'skktut-skip-question) ))

(defun skktut-setup-answer-buffer ()
  (save-excursion
    (set-buffer (get-buffer-create skktut-answer-buffer))
    ;; users may use undo.
    ;; (buffer-disable-undo (current-buffer))
    ;; skktut-answer-buffer $B$N(B skk.el $B$NJQ?t$r%P%C%U%!%m!<%+%k2=$7!"=i4|2=$9$k!#(B
    (skktut-localize-and-init-variables)
    (local-set-key "\C-xq" 'skk-tutorial-quit)
    (local-set-key "\C-xt" 'skk-tutorial-again)
    (local-set-key "\C-xj" 'skktut-error-command)
    (local-set-key "\C-xn" 'skktut-next-question)
    (local-set-key "\C-xs" 'skktut-skip-question)
    (auto-fill-mode -1)
    (skktut-next-answer-buffer) ))

(defun skktut-localize-and-init-variables ()
  ;; $B%f!<%6!<$,(B skk.el $B$NJQ?t$r%+%9%?%^%$%:$7$F$$$k2DG=@-$,$"$k$N$G!"%+%l%s%H(B
  ;; $B%P%C%U%!$N(B skk.el $B$NJQ?t$r%P%C%U%!%m!<%+%k2=$7!"=i4|2=$9$k!#(B
  (let ((alist skktut-init-variables-alist)
	v )
    (while alist
      (setq v (car (car alist)))
      (make-local-variable v)
      (set v (eval (cdr (car alist))))
      (setq alist (cdr alist)) )))

(defun skktut-erase-buffer ()
  (let ((inhibit-read-only t)
	buffer-read-only )
    (set-text-properties (point-min) (point-max) nil)
    (erase-buffer) ))

(defun skktut-before-move-to-other-frame ()
  (if (skktut-yes-or-no-p "Tutorial $B$r=*N;$7$^$9!#$h$m$7$$$G$9$M!)(B "
			  "Quit tutorial?" )
      (skk-tutorial-quit 'now)
    (skktut-error "Tutorial $B$r=*N;$;$:$KB>$N%U%l!<%`$K0\$k$3$H$O$G$-$^$;$s!#(B"
                  "Quit tutorial or you cannot move to other frame" )))

(defun skktut-colored ()
  ;; face $B$r(B Text Property $B$K$7$F$*$/$H%F%-%9%H$r%3%T!<$7$?$H$-$K0l=o$K%3%T!<$G(B
  ;; $B$-$k$N$G9%ET9g!#(B
  (while (re-search-forward "$B"'(B\\([^$B![(B $B$!(B-$B$s%!(B-$B%s(B]+\\)" nil t nil)
    (put-text-property (match-beginning 1) (match-end 1) 'face
                       'highlight ))
  (goto-char (point-min))
  (while (re-search-forward "^==.+==$" nil t nil)
    (put-text-property (match-beginning 0) (match-end 0)
                       'face skk-tut-section-face ))
  (goto-char (point-min))
  (while (re-search-forward "^!!.+" nil t nil)
    (put-text-property (match-beginning 0) (match-end 0)
                       'face skk-tut-do-it-face ))
  (goto-char (point-min))
  (while (re-search-forward "^>> \\(.+\\)$" nil t nil)
    (put-text-property (match-beginning 1) (match-end 1)
                       'face skk-tut-question-face ))
  (if skktut-japanese-tut
      nil
    (goto-char (point-min))
    (while (re-search-forward "Hint: .*$" nil t nil)
      (put-text-property (match-beginning 0) (match-end 0)
                         'face skk-tut-hint-face ))))

(defun skktut-next-answer-buffer ()
  (save-match-data
    (save-excursion
      ;; first get right answer in `skktut-question-buffer'.
      (set-buffer skktut-question-buffer)
      (goto-char (point-max))
      (search-backward "\n>>")
      (forward-char 1)
      (setq skktut-right-answer
	    (buffer-substring-no-properties
	     (+ 3 (point))
	     (skk-save-point (end-of-line) (point)) )))
    ;; not to save point.
    (let ((cbuf (current-buffer))
	  p )
      (unwind-protect
	  (let ((plist (cons (if (eq skk-emacs-type 'xemacs)
				 'end-open
			       'rear-nonsticky )
			     '(t intangible t read-only t) )))
	    ;; secondary make a new answer buffer.
	    (set-buffer skktut-answer-buffer)
	    (skktut-erase-buffer)
	    (insert ">> \n\n")
	    (add-text-properties (point-min) (- (point) 2) plist)
	    (setq p (point))
	    (insert
	     (if skktut-japanese-tut
		 (concat "* $BEz$,$G$-$?$i(B `C-x n'; $BESCf$G$d$a$k$K$O(B `C-x q'"
			 (if (= skktut-question-count 37) " *"
			   "; $B%9%-%C%W$9$k$K$O(B`C-x s' *" ))
	       (concat "* For next question `C-x n'; to quit `C-x q'"
		       (if (= skktut-question-count 37) " *"
			 "; to skip this question `C-x s' *" ))))
	    (if skk-tut-use-face
		(put-text-property p (point) 'face skk-tut-key-bind-face) )
	    (add-text-properties p (point) plist)
	    (goto-char (+ (point-min) 3)))
	(set-buffer cbuf) ))))

(defun skktut-get-question-page (page)
  (save-excursion
    (save-match-data
      (set-buffer skktut-working-buffer)
      (let (pos str)
        (goto-char (point-min))
        (search-forward "--\n" nil t page)
        (if (looking-at ";") ; lisp program exists.
            (progn
	      (forward-char 3)
	      (setq pos (point))
	      (end-of-line)
	      (save-excursion
		(eval-region pos (point) nil)
		(forward-char 1) )))
        (if (not skktut-tutorial-end)
            (progn
              (setq pos (point))
              (search-forward "\n>>")
              (end-of-line)
              (setq str (buffer-substring pos (point)))
	      (set-buffer skktut-question-buffer)
	      (skktut-erase-buffer)
	      (let (buffer-read-only)
		(insert str)
		(setq mode-line-buffer-identification
		      (concat "$B#S#K#K%A%e!<%H%j%"%k(B: $B!NLd(B "
			      (number-to-string page)
			      "$B!O(B $B!J;D$j(B "
			      (number-to-string (- skktut-question-numbers page))
			      "$BLd!K(B"))
		(set-buffer-modified-p nil)
		(force-mode-line-update 'all) )))))))

;; The following two functions are tricky, since they are executed by
;; `eval-region' in skktut-working-buffer.
(defun skktut-today ()
  (save-restriction
    (save-match-data
      (let (p)
	(widen)
        (search-forward "\n>> ")
	(if (re-search-forward "$B!V(B.*$B!W(B" (skk-save-point (end-of-line) (point)) t)
	    (delete-region (match-beginning 0) (match-end 0)) )
	(setq p (point))
	(insert (concat "$B!V$-$g$&$O!"(B" (skk-current-date) "$B$G$9!#!W(B"))
	(narrow-to-region (point-min) (point))
	(if skk-tut-use-face
	    (put-text-property p (point) 'face skk-tut-question-face) )))))

(defun skktut-end-tutorial ()
  (switch-to-buffer skktut-question-buffer)
  (delete-other-windows)
  (skktut-erase-buffer)
  (let (buffer-read-only)
    (goto-char (point-min))
    (insert
     (if skktut-japanese-tut
	 (concat "SKK $B%A%e!<%H%j%"%k$O$3$l$G=*$j$G$9!#(B\n\n"
		 "SKK 10.x $B$K4X$9$k<ALd!"%3%a%s%H!"(Bbug report $BEy$O(B\n\n"
		 "\tskk@ring.gr.jp\n\n"
		 "$BKx$*Aw$j2<$5$$!#$J$*!"$3$N%"%I%l%9$O(B SKK Ring Server Openlab $B%a%$%j%s%0(B"
 		 "$B%j%9%H$N%"%I%l%9$G$9!#(B\n"
		 "$B2sEz$ODL>o$3$N%"%I%l%9$KBP$7$F$J$5$l$k$N$G!"%a%s%P!<$G$J$$(B"
		 "$BJ}$O$=$N;]$rL@5-$7$F(B\n"
		 "$B%a!<%k$r$*Aw$j$/$@$5$$!#(BSKK Ring Server Openlab $B%a%$%j%s%0%j%9%H$X;2(B"
		 "$B2C4uK>$N>l9g$O(B\n\n"
		 "\tskk-request@ring.gr.jp\n\n"
		 "$B$X%a!<%k$r$*Aw$j$/$@$5$$(B\n\n"
		 "!! $B:G8e$K(B <return> $B%-!<$r2!$7$F$/$@$5$$!#(B" )
       (concat "Now we end the SKK tutorial.\n\n"
	       "Please send comments, questions and bug reports on SKK "
	       "version 10.x to:\n\n"
	       "\tskk@ring.gr.jp\n\n"
	       "This is the address of the SKK Ring Server Openlab mailing list, and "
	       "normally the responces\n"
	       "will be sent only to the ML members.  So, if you are not a ML "
	       "member, please say so \n"
	       "in your mail. If you are interested in joining the SKK Ring Server "
	       "Openlab ML, send a mail to:\n\n"
	       "\tskk-request@ring.gr.jp\n\n"
	       "!! Hit <return> key when you are ready." )))
    (if skk-tut-use-face
	(save-match-data
	  (goto-char (point-min))
	  (re-search-forward "^!!.+" nil t nil)
	  (put-text-property (match-beginning 0) (match-end 0)
			     'face skk-tut-do-it-face )))
    (while (not skktut-tutorial-end)
      (condition-case nil
	  (let* ((event (skk-read-event))
		 (char (event-to-character event)) )
	    (skktut-message "<return> $B%-!<$r2!$7$F$/$@$5$$(B" "Hit <return> key")
	    (if (and char (eq ?\C-m char))
		(setq skktut-tutorial-end t)
	      ;;(skk-unread-event event)
	      ))
	(error nil) ))))

(provide 'skk-tut)
;;; skk-tut.el ends here
