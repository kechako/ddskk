;;; skk-jisx0201.el --- SKK $BMQ(B JISX 0201 $B%3!<%IJ8;zF~NO%W%m%0%i%`(B
;; Copyright (C) 1999, 2000 Tsukamoto Tetsuo <czkmt@remus.dti.ne.jp>

;; Author: Tsukamoto Tetsuo <czkmt@remus.dti.ne.jp>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Version: $Id: skk-jisx0201.el,v 1.10 2001/06/16 07:24:31 minakaji Exp $
;; Keywords: japanese
;; Created: Oct. 30, 1999.
;; Last Modified: $Date: 2001/06/16 07:24:31 $

;; This file is part of Daredevil SKK.

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
;; <$B4JC1$J@bL@(B>
;;
;; ~/.skk $B$K(B
;;
;; (require 'skk-jisx0201)
;;
;; $B$H=q$/$3$H$G%$%s%9%H!<%k$5$l$^$9!#;H$$J}$O0J2<$N$h$&$K$J$j$^$9!#(B
;;
;; $B!}%+%?%+%J%b!<%I$K$*$$$F!"(B
;;   $B!&(B"C-q" $B$GA43Q%+%J%b!<%I$HH>3Q%+%J%b!<%I$r@Z$j$+$($^$9!#(B
;;
;; $B!}$R$i$,$J(B/$B%+%?%+%JN>%b!<%IFb$G$N"&%b!<%I$K$*$$$F!"(B
;;   $B!&(B"C-q" $B$r2!$9$H(I$$B8+=P$78l$H$7$FF~NO$5$l$?$R$i$,$J(B/$B%+%?%+%J$r(IJ]686@6E$B$KJQ49(B
;;     $B$7$^$9!#(B
;;
;; skk-jisx0201-roman-rule-list $B$K(B JISX0201.1976 Japanese Roman (latin-jisx0201)
;; $B$NJ8;zNs$rDj5A$7$F$$$^$9!#$?$@$7(B JISX0201.1976 Japanese Roman $BF~NO$O:#$N$H$3(B
;; $B$m(B Emacs 20.3 $B0J9_$H(BXEmacs 21 $B0J9_$G$7$+=PMh$F$$$^$;$s!#(I6E(B $B$H(B (Jroman(B $B$r@Z$jBX(B
;; $B$($k(B key $B$O$H$j$"$($:(B C-c C-q $B$K$7$F$"$j$^$9!#(B
;;
;;
;; <$B6HL3O"Mm(B>
;;
;; $B$3$N%U%!%$%k$rJT=8$9$k$H$-$O!"$G$-$l$P(B XEmacs $B$r;H$C$F$/$@$5$$!#(B Emacs 20 $B$O(B
;; $B%G%U%)%k%H$G$O(B JISX0201.1976 Japanese Roman $B$r<+F0E*$K(B US-ASCII $B$KJQ49$9$k$h(B
;; $B$&$K$J$C$F$$$k$+$i$G$9!#(B Emacs 20.3 $B0J9_$G$3$N%U%!%$%k$rJT=8$9$k>l9g$O!"$3$N(B
;; $B%U%!%$%k$r3+$/A0$K(B
;;
;; (setq standard-translation-table-for-decode (make-translation-table nil))
;;
;; $B$rI>2A$7$F$/$@$5$$!#(B $BF1MM$K(B Emacs 20.2 $B$G$3$N%U%!%$%k$rJT=8$9$k>l9g$O!"(B $B$3$N(B
;; $B%U%!%$%k$r3+$/A0$K(B
;;
;; (setq standard-character-unification-table-for-decode (make-unification-table nil))
;;
;; $B$rI>2A$7$F$/$@$5$$(B (Mule 2.3 $B$K$D$$$F$OJ,$+$j$^$;$s(B)$B!#(B

;;; Code:
(eval-when-compile
  (require 'skk-macs)
  (require 'skk-vars)
  (require 'static))

(static-cond ((eq skk-emacs-type 'mule2)
	      (eval-and-compile
		(defvar fence-mode-map (make-keymap))
		(require 'jisx0201)))
	     (t
	      (require 'japan-util)))

(eval-and-compile
  (unless (eq skk-emacs-type 'xemacs)
    (autoload 'set-buffer-local-cursor-color "ccc"))
  (autoload 'skk-cursor-current-color "skk-cursor")
  (autoload 'skk-isearch-message "skk-isearch"))

;; $B=tHL$N;v>p$K$h$j(B skk-vars.el $B$KF~$l$k$Y$-$G$J$$JQ?t(B
(defvar skk-jisx0201-base-rule-list
  '(("a" nil "(I1(B")
    ("bb" "b" "(I/(B") ("ba" nil "(IJ^(B") ("be" nil "(IM^(B")
    ("bi" nil "(IK^(B") ("bo" nil "(IN^(B") ("bu" nil "(IL^(B") ("bya" nil "(IK^,(B")
    ("bye" nil "(IK^*(B") ("byi" nil "(IK^((B") ("byo" nil "(IK^.(B") ("byu" nil "(IK^-(B")
    ("cc" "c" "(I/(B") ("cha" nil "(IA,(B") ("che" nil "(IA*(B") ("chi" nil "(IA(B")
    ("cho" nil "(IA.(B") ("chu" nil "(IA-(B") ("cya" nil "(IA,(B") ("cye" nil "(IA*(B")
    ("cyi" nil "(IA((B") ("cyo" nil "(IA.(B") ("cyu" nil "(IA-(B")
    ("dd" "d" "(I/(B") ("da" nil "(I@^(B") ("de" nil "(IC^(B") ("dha" nil "(IC^,(B")
    ("dhe" nil "(IC^*(B") ("dhi" nil "(IC^((B") ("dho" nil "(IC^.(B") ("dhu" nil "(IC^-(B")
    ("di" nil "(IA^(B") ("do" nil "(ID^(B") ("du" nil "(IB^(B") ("dya" nil "(IA^,(B")
    ("dye" nil "(IA^*(B") ("dyi" nil "(IA^((B") ("dyo" nil "(IA^.(B") ("dyu" nil "(IA^-(B")
    ("e" nil "(I4(B")
    ("ff" "f" "(I/(B") ("fa" nil "(IL'(B") ("fe" nil "(IL*(B") ("fi" nil "(IL((B")
    ("fo" nil "(IL+(B") ("fu" nil "(IL(B") ("fya" nil "(IL,(B") ("fye" nil "(IL*(B")
    ("fyi" nil "(IL((B") ("fyo" nil "(IL.(B") ("fyu" nil "(IL-(B") ("gg" "g" "(I/(B")
    ("ga" nil "(I6^(B") ("ge" nil "(I9^(B") ("gi" nil "(I7^(B") ("go" nil "(I:^(B")
    ("gu" nil "(I8^(B") ("gya" nil "(I7^,(B") ("gye" nil "(I7^*(B") ("gyi" nil "(I7^((B")
    ("gyo" nil "(I7^.(B") ("gyu" nil "(I7^-(B")
    ("ha" nil "(IJ(B") ("he" nil "(IM(B") ("hi" nil "(IK(B") ("ho" nil "(IN(B")
    ("hu" nil "(IL(B") ("hya" nil "(IK,(B") ("hye" nil "(IK*(B") ("hyi" nil "(IK((B")
    ("hyo" nil "(IK.(B") ("hyu" nil "(IK-(B") ("i" nil "(I2(B")
    ("jj" "j" "(I/(B") ("ja" nil "(I<^,(B") ("je" nil "(I<^*(B") ("ji" nil "(I<^(B")
    ("jo" nil "(I<^.(B") ("ju" nil "(I<^-(B") ("jya" nil "(I<^,(B") ("jye" nil "(I<^*(B")
    ("jyi" nil "(I<^((B") ("jyo" nil "(I<^.(B") ("jyu" nil "(I<^-(B")
    ("kk" "k" "(I/(B") ("ka" nil "(I6(B") ("ke" nil "(I9(B") ("ki" nil "(I7(B")
    ("ko" nil "(I:(B") ("ku" nil "(I8(B") ("kya" nil "(I7,(B") ("kye" nil "(I7*(B")
    ("kyi" nil "(I7((B") ("kyo" nil "(I7.(B") ("kyu" nil "(I7-(B")
    ("mm" "c" "(I/(B") ("ma" nil "(IO(B") ("me" nil "(IR(B") ("mi" nil "(IP(B")
    ("mo" nil "(IS(B") ("mu" nil "(IQ(B") ("mya" nil "(IP,(B") ("mye" nil "(IP*(B")
    ("myi" nil "(IP((B") ("myo" nil "(IP.(B") ("myu" nil "(IP-(B")
    ("n" nil "(I](B") ("n'" nil "(I](B") ("na" nil "(IE(B") ("ne" nil "(IH(B")
    ("ni" nil "(IF(B") ("nn" nil "(I](B") ("no" nil "(II(B") ("nu" nil "(IG(B")
    ("nya" nil "(IF,(B") ("nye" nil "(IF*(B") ("nyi" nil "(IF((B") ("nyo" nil "(IF.(B")
    ("nyu" nil "(IF-(B")
    ("o" nil "(I5(B")
    ("pp" "p" "(I/(B") ("pa" nil "(IJ_(B") ("pe" nil "(IM_(B") ("pi" nil "(IK_(B")
    ("po" nil "(IN_(B") ("pu" nil "(IL_(B") ("pya" nil "(IK_,(B") ("pye" nil "(IK_*(B")
    ("pyi" nil "(IK_((B") ("pyo" nil "(IK_.(B") ("pyu" nil "(IK_-(B")
    ("rr" "r" "(I/(B") ("ra" nil "(IW(B") ("re" nil "(IZ(B") ("ri" nil "(IX(B")
    ("ro" nil "(I[(B") ("ru" nil "(IY(B") ("rya" nil "(IX,(B") ("rye" nil "(IX*(B")
    ("ryi" nil "(IX((B") ("ryo" nil "(IX.(B") ("ryu" nil "(IX-(B")
    ("ss" "s" "(I/(B") ("sa" nil "(I;(B") ("se" nil "(I>(B") ("sha" nil "(I<,(B")
    ("she" nil "(I<*(B") ("shi" nil "(I<(B") ("sho" nil "(I<.(B") ("shu" nil "(I<-(B")
    ("si" nil "(I<(B") ("so" nil "(I?(B") ("su" nil "(I=(B") ("sya" nil "(I<,(B")
    ("sye" nil "(I<*(B") ("syi" nil "(I<((B") ("syo" nil "(I<.(B") ("syu" nil "(I<-(B")
    ("tt" "t" "(I/(B") ("ta" nil "(I@(B") ("te" nil "(IC(B") ("tha" nil "(IC'(B")
    ("the" nil "(IC*(B") ("thi" nil "(IC((B") ("tho" nil "(IC.(B") ("thu" nil "(IC-(B")
    ("ti" nil "(IA(B") ("to" nil "(ID(B") ("tsu" nil "(IB(B") ("tu" nil "(IB(B")
    ("tya" nil "(IA,(B") ("tye" nil "(IA*(B") ("tyi" nil "(IA((B") ("tyo" nil "(IA.(B")
    ("tyu" nil "(IA-(B")
    ("u" nil "(I3(B")
    ("vv" "v" "(I/(B") ("va" nil "(I3^'(B") ("ve" nil "(I3^*(B") ("vi" nil "(I3^((B")
    ("vo" nil "(I3^+(B") ("vu" nil "(I3^(B")
    ("ww" "w" "(I/(B") ("wa" nil "(I\(B") ("we" nil "(I3*(B") ("wi" nil "(I3((B")
    ("wo" nil "(I&(B") ("wu" nil "(I3(B")
    ("xx" "x" "(I/(B") ("xa" nil "(I'(B") ("xe" nil "(I*(B") ("xi" nil "(I((B")
    ("xka" nil "(I6(B") ("xke" nil "(I9(B") ("xo" nil "(I+(B") ("xtsu" nil "(I/(B")
    ("xtu" nil "(I/(B") ("xu" nil "(I)(B") ("xwa" nil "(I\(B") ("xwe" nil "(I*(B")
    ("xwi" nil "(I((B") ("xya" nil "(I,(B") ("xyo" nil "(I.(B") ("xyu" nil "(I-(B")
    ("yy" "y" "(I/(B") ("ya" nil "(IT(B") ("ye" nil "(I2*(B") ("yo" nil "(IV(B")
    ("yu" nil "(IU(B")
    ("zz" "z" "(I/(B") ("z," nil "$B!E(B") ("z-" nil "$B!A(B") ("z." nil "$B!D(B")
    ("z/" nil "(I%(B") ("z[" nil "$B!X(B") ("z]" nil "$B!Y(B") ("za" nil "(I;^(B")
    ("ze" nil "(I>^(B") ("zh" nil "$B"+(B") ("zi" nil "(I<^(B") ("zj" nil "$B"-(B")
    ("zk" nil "$B",(B") ("zl" nil "$B"*(B") ("zo" nil "(I?^(B") ("zu" nil "(I=^(B")
    ("zya" nil "(I<^,(B") ("zye" nil "(I<^*(B") ("zyi" nil "(I<^((B") ("zyo" nil "(I<^.(B")
    ("zyu" nil "(I<^-(B")
    ("," nil "(I$(B") ("." nil "(I!(B") ("-" nil "(I0(B") (":" nil ":") (";" nil ";")
    ("?" nil "?") ("[" nil "(I"(B") ("]" nil "(I#(B")
    ("l" nil skk-latin-mode)
    ("q" nil skk-toggle-katakana)
    ("L" nil skk-jisx0208-latin-mode)
    ("Q" nil skk-set-henkan-point-subr)
    ("X" nil skk-purge-from-jisyo)
    ("/" nil skk-abbrev-mode)
    ("$" nil skk-display-code-for-char-at-point)
    ("@" nil skk-today)
    ("\\" nil skk-input-by-code-or-menu))
  "*SKK JISX0201 $B%b!<%I$N%Y!<%9$N%k!<%k!#(B")

(defvar skk-jisx0201-roman-rule-list
  '(("!" nil "(J!(B") ("\"" nil "\(J"(B") ("#" nil "(J#(B") ("$" nil "(J$(B") ("%" nil "(J%(B")
    ("&" nil "(J&(B") ("'" nil "(J'(B") ("\(" nil "(J((B") ("\)" nil "(J)(B") ("*" nil "(J*(B")
    ("+" nil "(J+(B") ("," nil "(J,(B") ("-" nil "(J-(B") ("." nil "(J.(B") ("/" nil "(J/(B")
    ("0" nil "(J0(B") ("1" nil "(J1(B") ("2" nil "(J2(B") ("3" nil "(J3(B") ("4" nil "(J4(B")
    ("5" nil "(J5(B") ("6" nil "(J6(B") ("7" nil "(J7(B") ("8" nil "(J8(B") ("9" nil "(J9(B")
    (":" nil "(J:(B") (";" nil "(J;(B") ("<" nil "(J<(B") ("=" nil "(J=(B") (">" nil "(J>(B")
    ("?" nil "(J?(B") ("@" nil "(J@(B")
    ("A" nil "(JA(B") ("B" nil "(JB(B") ("C" nil "(JC(B") ("D" nil "(JD(B") ("E" nil "(JE(B")
    ("F" nil "(JF(B") ("G" nil "(JG(B") ("H" nil "(JH(B") ("I" nil "(JI(B") ("J" nil "(JJ(B")
    ("K" nil "(JK(B") ("L" nil "(JL(B") ("M" nil "(JM(B") ("N" nil "(JN(B") ("O" nil "(JO(B")
    ("P" nil "(JP(B") ("Q" nil "(JQ(B") ("R" nil "(JR(B") ("S" nil "(JS(B") ("T" nil "(JT(B")
    ("U" nil "(JU(B") ("V" nil "(JV(B") ("W" nil "(JW(B") ("X" nil "(JX(B") ("Y" nil "(JY(B")
    ("Z" nil "(JZ(B")
    ("[" nil "(J[(B") ("\\" nil "\(J\(B") ("]" nil "(J](B") ("^" nil "(J^(B") ("_" nil "(J_(B")
    ("`" nil "(J`(B")
    ("a" nil "(Ja(B") ("b" nil "(Jb(B") ("c" nil "(Jc(B") ("d" nil "(Jd(B") ("e" nil "(Je(B")
    ("f" nil "(Jf(B") ("g" nil "(Jg(B") ("h" nil "(Jh(B") ("i" nil "(Ji(B") ("j" nil "(Jj(B")
    ("k" nil "(Jk(B") ("l" nil "(Jl(B") ("m" nil "(Jm(B") ("n" nil "(Jn(B") ("o" nil "(Jo(B")
    ("p" nil "(Jp(B") ("q" nil "(Jq(B") ("r" nil "(Jr(B") ("s" nil "(Js(B") ("t" nil "(Jt(B")
    ("u" nil "(Ju(B") ("v" nil "(Jv(B") ("w" nil "(Jw(B") ("x" nil "(Jx(B") ("y" nil "(Jy(B")
    ("z" nil "(Jz(B")
    ("{" nil "(J{(B") ("|" nil "(J|(B") ("}" nil "(J}(B") ("~" nil "(J~(B") (" " nil " "))
  "*SKK JISX0201 $B%b!<%I$N(B Roman $B$N%k!<%k!#(B")

(defvar skk-jisx0201-rule-list
  '(("!" nil "!") ("\"" nil "\"") ("#" nil "#") ("%" nil "%") ("&" nil "&")
    ("'" nil "'") ("\(" nil "(") ("\)" nil ")") ("*" nil "*") ("+" nil "+")
    ("-" nil "-")
    ("1" nil "1") ("2" nil "2") ("3" nil "3") ("4" nil "4") ("5" nil "5")
    ("6" nil "6") ("7" nil "7") ("8" nil "8") ("9" nil "9")
    (":" nil ":") (";" nil ";") ("<" nil "<") ("=" nil "=") (">" nil ">")
    ("?" nil "?") ("@" nil "@")
    ("A" nil "A") ("B" nil "B") ("C" nil "C") ("D" nil "D") ("E" nil "E")
    ("F" nil "F") ("G" nil "G") ("H" nil "H") ("I" nil "I") ("J" nil "J")
    ("K" nil "K") ("L" nil "L") ("M" nil "M") ("N" nil "N") ("O" nil "O")
    ("P" nil "P") ("Q" nil "Q") ("R" nil "R") ("S" nil "S") ("T" nil "T")
    ("U" nil "U") ("V" nil "V") ("W" nil "W") ("X" nil "X") ("Y" nil "Y")
    ("Z" nil "Z")
    ("^" nil "^") ("_" nil "_") ("`" nil "`") ("{" nil "{") ("|" nil "|")
    ("}" nil "}") ("~" nil "~") (" " nil " "))
"*SKK JISX0201 $B%b!<%I$NDI2C$N%k!<%k!#(B")

(or skk-jisx0201-mode-map
    (let ((map (make-sparse-keymap)))
      (substitute-key-definition 'self-insert-command 'skk-jisx0201-insert map
				 global-map)
      ;; for Mule-2.x
      (substitute-key-definition 'egg-self-insert-command 'skk-jisx0201-insert
				 map global-map)
      (substitute-key-definition 'canna-self-insert-command
				 'skk-jisx0201-insert map global-map)
      (substitute-key-definition 'canna-henkan-region-or-self-insert 'skk-insert
				 map global-map)
      (substitute-key-definition 'can-n-egg-self-insert-command
				 'skk-jisx0201-insert map global-map)
      ;;(define-key map "\C-q" 'skk-jisx0201-henkan)
      (skk-define-menu-bar-map map)
      (setq skk-jisx0201-mode-map map)))

(set-modified-alist
 'minor-mode-map-alist
 (list (cons 'skk-jisx0201-mode skk-jisx0201-mode-map)))

(setq skk-jisx0201-base-rule-tree
      (skk-compile-rule-list skk-jisx0201-base-rule-list skk-jisx0201-rule-list))
(setq skk-jisx0201-roman-rule-tree
      (skk-compile-rule-list skk-jisx0201-roman-rule-list))

;; inline functions.
(defsubst skk-jisx0201-mode-on (&optional arg)
  (setq skk-mode t
        skk-jisx0201-mode t
	skk-jisx0201-roman arg
	skk-jisx0201-rule-tree (if arg
				   skk-jisx0201-roman-rule-tree
				 skk-jisx0201-base-rule-tree)
        skk-abbrev-mode nil
        skk-latin-mode nil
        skk-j-mode nil
        skk-jisx0208-latin-mode nil
        skk-katakana nil)
  (skk-update-modeline 'jisx0201))

;; Pieces of advice.
(defadvice skk-mode (after skk-jisx0201-ad activate)
  (define-key skk-jisx0201-mode-map skk-kakutei-key 'skk-kakutei)
  (setq skk-jisx0201-mode nil))

(defadvice skk-kakutei (after skk-jisx0201-ad activate)
  (and skk-jisx0201-mode (skk-jisx0201-mode-on skk-jisx0201-roman)))

(defadvice skk-latin-mode (after skk-jisx0201-ad activate)
  (setq skk-jisx0201-mode nil))

(defadvice skk-jisx0208-latin-mode (after skk-jisx0201-ad activate)
  (setq skk-jisx0201-mode nil))

(defadvice skk-abbrev-mode (after skk-jisx0201-ad activate)
  (setq skk-jisx0201-mode nil))

(skk-defadvice newline (around skk-jisx0201-ad activate)
  "skk-egg-like-newline $B$,(B non-nil $B$@$C$?$i!"JQ49Cf$N(B newline $B$G3NDj$N$_9T$$!"2~9T$7$J$$!#(B"
  (interactive "*P")
  (if (not (or skk-jisx0201-mode skk-abbrev-mode))
      ad-do-it
    (let (
	  ;;(arg (ad-get-arg 0))
          ;; skk-kakutei $B$r<B9T$9$k$H(B skk-henkan-on $B$NCM$,L5>r7o$K(B nil $B$K$J$k(B
          ;; $B$N$G!"J]B8$7$F$*$/I,MW$,$"$k!#(B
          (no-newline (and skk-egg-like-newline skk-henkan-on))
	  (auto-fill-function (and (interactive-p) auto-fill-function)))
      ;; fill $B$5$l$F$b(B nil $B$,5"$C$F$/$k(B :-<
      ;;(if (skk-kakutei)
      ;;    (setq arg (1- arg)))
      ;;(if skk-mode
      ;;    (let ((opos (point)))
      ;;      ;; skk-kakutei (skk-do-auto-fill) $B$K$h$C$F9T$,@^$jJV$5$l$?$i(B arg $B$r(B 1 $B$D8:$i$9!#(B
      ;;      (skk-kakutei)
      ;;      (if (and (not (= opos (point))) (integerp arg))
      ;;          (ad-set-arg 0 (1- arg)))))
      (and skk-mode (skk-kakutei))
      (if (not no-newline)
	  ad-do-it))))

(skk-defadvice newline-and-indent (around skk-jisx0201-ad activate)
  "skk-egg-like-newline $B$,(B non-nil $B$@$C$?$i!"JQ49Cf$N(B newline-and-indent $B$G3NDj$N$_9T$$!"2~9T$7$J$$!#(B"
  (if (not (or skk-jisx0201-mode skk-abbrev-mode))
      ad-do-it
    (let ((no-newline (and skk-egg-like-newline skk-henkan-on))
	  (auto-fill-function (and (interactive-p) auto-fill-function)))
      (and skk-mode (skk-kakutei))
      (or no-newline ad-do-it))))

(skk-defadvice exit-minibuffer (around skk-jisx0201-ad activate)
  "skk-egg-like-newline $B$,(B non-nil $B$@$C$?$i!"JQ49Cf$N(B exit-minibuffer $B$G3NDj$N$_9T$&!#(B"
  (skk-remove-minibuffer-setup-hook
   'skk-jisx0201-mode-on 'skk-setup-minibuffer
   (function (lambda ()
	       (add-hook 'pre-command-hook 'skk-pre-command nil 'local))))
  (if (not (or skk-jisx0201-mode skk-abbrev-mode))
      ad-do-it
    (let ((no-newline (and skk-egg-like-newline skk-henkan-on)))
      (and skk-mode (skk-kakutei))
      (or no-newline ad-do-it))))

;; functions.
;;;###autoload
(defun skk-jisx0201-mode (arg)
  "SKK $B$N%b!<%I$r(B JISX0201 $B%b!<%I$KJQ99$9$k!#(B"
  (interactive "P")
  (skk-kakutei)
  (skk-jisx0201-mode-on))

(defun skk-toggle-jisx0201 (arg)
  "$BH>3Q%+%J%b!<%I$H%m!<%^;z%b!<%I$r@Z$jBX$($k!#(B"
  (interactive "P")
  (cond ((and skk-henkan-on (not skk-henkan-active))
	 (skk-jisx0201-henkan arg))
	(t
	 (cond (skk-jisx0201-roman
		(setq skk-jisx0201-rule-tree skk-jisx0201-base-rule-tree)
		(setq skk-jisx0201-roman nil))
	       (t
		(or skk-jisx0201-base-rule-tree
		    (setq skk-jisx0201-base-rule-tree skk-jisx0201-rule-tree))
		(setq skk-jisx0201-rule-tree skk-jisx0201-roman-rule-tree)
		(setq skk-jisx0201-roman t))))))

(defun skk-jisx0201-string-conversion (str func)
  (let ((buf (get-buffer-create " *SKK JIS X 0201 work*")))
    (save-excursion
      (set-buffer buf)
      (erase-buffer)
      (insert str)
      (funcall func 1 (point))
      (buffer-string))))

(defun skk-jisx0201-zenkaku (str)
  "STR $B$N(B JIS X 0201 $B%+%J$KB0$9$kJ8;zNs$rBP1~$9$k(B JIS X 0208 $B$NJ8;zNs$GCV$-49$((B
$B$k!#(B"
  (skk-jisx0201-string-conversion
   str
   (static-cond ((eq skk-emacs-type 'mule2)
		 'zenkaku-katakana-region)
		(t
		 'japanese-zenkaku-region))))

(defun skk-jisx0201-hankaku (str)
  "STR $B$N(B JIS X 0208 $B$KB0$9$kJ8;zNs$rBP1~$9$k(B JIS X 0201 $B%+%J$NJ8;zNs$GCV$-49$((B
$B$k!#(B"
  (skk-jisx0201-string-conversion
   str
   (static-cond ((eq skk-emacs-type 'mule2)
		 'hankaku-katakana-region)
		(t
		 'japanese-hankaku-region))))

(defun skk-jisx0201-insert (&optional arg)
  "SKK JISX0201 $B%b!<%I$NJ8;zF~NO$r9T$J$&!#(B"
  (interactive "*p")
  (skk-with-point-move
   (let ((ch last-command-char))
     (cond (
	    ;; start writing a midasi key.
	    (or (and (not skk-jisx0201-roman)
		     (memq ch skk-set-henkan-point-key)
		     (or skk-okurigana
			 (not (skk-get-prefix skk-jisx0201-current-rule-tree))
			 (not (skk-select-branch skk-jisx0201-current-rule-tree ch))))
		(and skk-henkan-on (memq ch skk-special-midashi-char-list)))
	    ;; normal pattern
	    ;; skk-set-henkan-point -> skk-jisx0201-kana-input.
	    (skk-jisx0201-set-henkan-point arg))
	   ;; start conversion.
	   ((and skk-henkan-on (eq ch skk-start-henkan-char))
	    (let ((jisx0201 (buffer-substring-no-properties
			     skk-henkan-start-point (point)))
		  jisx0208 )
	      (if (and jisx0201 (setq jisx0208 (skk-jisx0201-zenkaku jisx0201)))
		  (progn
		    (insert-before-markers jisx0208)
		    (delete-region skk-henkan-start-point
				   (- (point) (length jisx0208)))))
	      (let ((skk-katakana t)) (skk-start-henkan arg)))
	    (when skk-use-color-cursor
		 (static-cond
		  ((eq skk-emacs-type 'xemacs)
		   (set-face-property
		    'text-cursor 'background (skk-cursor-current-color)
		    (current-buffer)))
		  (t
		   (set-buffer-local-cursor-color (skk-cursor-current-color))))))
	   ;; for completion.
	   ((and skk-henkan-on (not skk-henkan-active))
	    (cond ((eq ch skk-try-completion-char)
		   (setq this-command 'skk-comp-do)
		   (skk-comp (not (eq last-command 'skk-comp-do))))
		  ((and (eq last-command 'skk-comp-do)
			(memq ch (list skk-next-completion-char
				       skk-previous-completion-char)))
		   (skk-comp-previous/next ch))
		  (t (skk-jisx0201-kana-input arg))))
	   ;; just imput JISX0201 Kana.
	   (t (skk-jisx0201-kana-input arg))))))

(defun skk-jisx0201-kana-input (&optional arg)
  ;;"JISX0201 $B%b!<%I$NJ8;z$NF~NO$r9T$&%k!<%A%s!#(B"
  (let ((echo-keystrokes 0)
	(queue (list last-command-char)))
    (while queue
      (if (not (skk-get-prefix skk-jisx0201-current-rule-tree))
	  (progn
	    (skk-set-marker skk-kana-start-point (point))
	    (setq skk-jisx0201-current-rule-tree skk-jisx0201-rule-tree))
	(skk-erase-prefix))
      (setq skk-prefix (concat (skk-get-prefix skk-jisx0201-current-rule-tree)
			       (char-to-string last-command-char)))
      (let ((next (skk-select-branch skk-jisx0201-current-rule-tree (car queue)))
	    data)
	(if next
	    ;; can go down SKK-JISX0201-CURRENT-RULE-TREE
	    (if (skk-get-branch-list next)
		;; NEXT have at least one branch
		(progn
		  (and skk-henkan-active
		       skk-kakutei-early
		       (not skk-process-okuri-early)
		       (skk-kakutei))
		  (setq queue (cdr queue)
			skk-jisx0201-current-rule-tree next))
	      ;; NEXT does not have any branch (i.e. NEXT is a leaf)
	      (setq data (skk-get-kana next)
		    queue (nconc (string-to-char-list (skk-get-nextstate next))
				 (cdr queue))
		    skk-jisx0201-current-rule-tree nil))
	  ;; can not go down SKK-JISX0201-CURRENT-RULE-TREE
	  (let ((d (skk-get-kana skk-jisx0201-current-rule-tree)))
	    (if d
		;; SKK-JISX0201-CURRENT-RULE-TREE have a roma->kana rule
		(setq data d
		      queue
		      (nconc (string-to-char-list
			      (skk-get-nextstate skk-jisx0201-current-rule-tree))
			     queue )
		      skk-jisx0201-current-rule-tree nil)
	      ;; SKK-JISX0201-CURRENT-RULE-TREE does not have any roma->kana rule
	      (let ((dd (and skk-kana-input-search-function
			     (funcall skk-kana-input-search-function))))
		(if dd
		    (setq data (car dd)
			  queue (nconc (string-to-char-list (cdr dd))
				       (cdr queue))
			  skk-jisx0201-current-rule-tree nil)
		  (if (eq skk-jisx0201-current-rule-tree skk-jisx0201-rule-tree)
		      ;; typo on the root of tree
		      (setq queue nil
			    skk-jisx0201-current-rule-tree nil)
		    ;; otherwise move to root of the tree, and redo
		    (setq skk-jisx0201-current-rule-tree nil)))))))
	(if (not data)
	    (if skk-jisx0201-current-rule-tree
		(progn
		  (or skk-isearch-message (setq prefix-arg arg))
		  (setq skk-prefix (skk-get-prefix skk-jisx0201-current-rule-tree))
		  (skk-insert-prefix skk-prefix))
	      (and skk-henkan-active (skk-kakutei))
	      (setq skk-prefix "")
	      (or queue
		  (skk-emulate-original-map (skk-make-raw-arg arg))))
	  (skk-cancel-undo-boundary)
	  (setq skk-prefix "")
	  (and (functionp data)
	       (setq data (funcall data (skk-make-raw-arg arg))))
	  (if (not (stringp (if (consp data) (car data) data)))
	      nil
	    (let ((pair (and skk-auto-insert-paren
			     (cdr (assoc data skk-auto-paren-string-alist))))
		  (count0 arg) (count1 arg) (inserted 0))
	      (and skk-henkan-active
		   skk-kakutei-early (not skk-process-okuri-early)
		   (skk-kakutei))
	      (while (> count0 0)
		(skk-insert-str data)
		(setq count0 (1- count0)))
	      (if (not pair)
		  nil
		(while (> count1 0)
		  (if (not (string= pair (char-to-string (following-char))))
		      (progn
			(setq inserted (1+ inserted))
			(skk-insert-str pair)))
		  (setq count1 (1- count1)))
		(or (= inserted 0) (backward-char inserted)))
	      (and skk-okurigana (null queue) (skk-set-okurigana))))))
      (and skk-isearch-message (skk-isearch-message)))))

(defun skk-jisx0201-set-henkan-point (&optional arg)
  ;;"$BJQ49$r3+;O$9$k%]%$%s%H$r%^!<%/$7!"BP1~$9$k(B skk-prefix $B$+!"Jl2;$rF~NO$9$k!#(B"
  (let* ((last-char (skk-downcase last-command-char))
	 (normal (not (eq last-char last-command-char)))
	 (sokuon (and (string= skk-prefix (char-to-string last-char))
		      (/= last-char ?o)))
	 (henkan-active skk-henkan-active))
    (if (or (not skk-henkan-on) skk-henkan-active)
	(if normal
	    (skk-jisx0201-set-henkan-point-subr)
	  (and skk-henkan-on (skk-jisx0201-set-henkan-point-subr))
	  (if henkan-active
	      (skk-emulate-original-map arg)
	    ;; What's to be here?
	    ;;(skk-self-insert arg)
	    ))
      (if (not normal)
	  (progn			; special char
	    (insert-and-inherit last-char)
	    (skk-set-marker skk-henkan-end-point (point))
	    (setq skk-henkan-count 0
		  skk-henkan-key (buffer-substring-no-properties
				  skk-henkan-start-point (point))
		  skk-prefix "")
	    (skk-henkan))
	;; prepare for the processing of okurigana if not skk-okurigana
	;; and the preceding character is not a numeric character.
	;; if the previous char is a special midashi char or a
	;; numeric character, we assume that the user intended to type the
	;; last-command-char in lower case.
	(if (and (or (not (skk-get-prefix skk-jisx0201-current-rule-tree))
		     ;; for KAnji, KanJIru
		     (and
		      (not (= skk-henkan-start-point skk-kana-start-point))
		      (or sokuon	; for TaSSi or TasSi
			  (skk-kana-cleanup)))) ; for NEko
		 (not skk-okurigana)
		 (or (= skk-henkan-start-point (point))
		     (let ((p (char-before)))
		       (not
			(or
			 ;; previous char is a special midashi char
			 (memq p skk-special-midashi-char-list)
			 ;; previous char is an ascii numeric char
			 (and (<= ?0 p) (<= p ?9))
			 ;; previous char is a JIS X 0208 numeric char
			 (and (skk-jisx0208-p p)
			      (= (skk-char-octet p 0) 35) ;?#
			      (<= 48 (skk-char-octet p 1)) ; ?0
			      (<= (skk-char-octet p 1) 57)) ; ?9
			 )))))
	    (if skk-process-okuri-early
		(progn
		  (skk-set-marker skk-henkan-end-point (point))
		  (setq skk-okuri-char (char-to-string last-char))
		  (if sokuon
		      (progn
			(setq skk-henkan-key
			      (concat (buffer-substring-no-properties
				       skk-henkan-start-point
				       skk-kana-start-point)
				      "(IB(B"
				      skk-henkan-okurigana))
			(skk-erase-prefix)
			(insert-and-inherit "(IB(B")
			(setq skk-prefix ""
			      skk-henkan-count 0)
			(skk-henkan)
			(delete-backward-char 2))
		    (setq skk-henkan-key (concat
					  (buffer-substring-no-properties
					   skk-henkan-start-point
					   (point))
					  skk-okuri-char))
		    (insert-and-inherit " ")
		    (setq skk-prefix ""
			  skk-henkan-count 0)
		    (skk-henkan)
		    (delete-backward-char 1))
		  ;; we set skk-kana-start-point here, since the marker may no
		  ;; longer point at the correct position after skk-henkan.
		  (skk-set-marker skk-kana-start-point (point)))
	      (if (= skk-henkan-start-point (point))
		  nil
		(if sokuon
		    (progn
		      (skk-erase-prefix 'clean)
		      (insert-and-inherit "(IB(B")))
		(skk-set-marker skk-okurigana-start-point (point))
		(insert-and-inherit "*")
		(skk-set-marker skk-kana-start-point (point))
		(setq skk-okuri-char (char-to-string last-char)
		      skk-okurigana t))))))
    (if normal
	(progn
	  (setq last-command-char last-char)
	  (skk-jisx0201-kana-input arg)))))

(defun skk-jisx0201-set-henkan-point-subr (&optional arg)
  "$B$+$J$rF~NO$7$?8e$G!"%]%$%s%H$KJQ493+;O$N%^!<%/(B \($B"&(B\) $B$rIU$1$k!#(B
$B85!9$O$3$N4X?t$O(B skk-set-henkan-point $B$NFbIt4X?t$G$"$k!#(B"
  (interactive "*P")
  (skk-with-point-move
   (cancel-undo-boundary)
   (if skk-henkan-on (skk-kakutei)
     (skk-kana-cleanup));; XXX
   (if (not (skk-get-prefix skk-jisx0201-current-rule-tree))
       (insert-and-inherit "$B"&(B")
     (skk-erase-prefix)
     (insert-and-inherit "$B"&(B")
     (skk-set-marker skk-kana-start-point (point))
     (skk-insert-prefix))
   (setq skk-henkan-on t)
   (skk-set-marker skk-henkan-start-point (point))))

;;;###autoload
(defun skk-toggle-katakana (arg)
  (interactive "P")
  (if (and skk-henkan-on (not skk-henkan-active))
      (skk-jisx0201-henkan arg)
    (if skk-jisx0201-mode
	(progn
	  (setq skk-jisx0201-mode nil)
	  (skk-j-mode-on 'katakana)
	  (skk-update-modeline 'katakana))
      (skk-jisx0201-mode-on)
      (skk-update-modeline 'jisx0201)))
  (when skk-use-color-cursor
    (static-cond
     ((eq skk-emacs-type 'xemacs)
      (set-face-property
       'text-cursor 'background (skk-cursor-current-color)
       (current-buffer)))
     (t
      (set-buffer-local-cursor-color (skk-cursor-current-color))))))

(defun skk-jisx0201-henkan (arg)
  "$B"&%b!<%I$G$"$l$P!"%j!<%8%g%s$N$R$i$,$J(B/$B%+%?%+%J$r(IJ]686@6E$B$KJQ49$9$k!#(B
$B"'%b!<%I$G$O2?$b$7$J$$!#(B
$B$=$NB>$N%b!<%I$G$O!"%*%j%8%J%k$N%-!<3d$jIU$1$G%P%$%s%I$5$l$F$$$k%3%^%s%I$r<B9T(B
$B$9$k!#(B"
  (interactive "*P")
  (skk-with-point-move
   (if skk-henkan-on
       (if skk-henkan-active
	   nil
	 (skk-set-marker skk-henkan-end-point (point))
	 (skk-*-henkan-1 'skk-jisx0201-region skk-henkan-start-point
			 skk-henkan-end-point 'vcontract))
     (skk-emulate-original-map arg))))

(defun skk-jisx0201-region (start end &optional vcontract)
  "$B%j!<%8%g%s$N$R$i$,$J(B/$B%+%?%+%J$r(IJ]686@6E$B$KJQ49$9$k!#(B
$B%*%W%7%g%J%k0z?t$N(B VCONTRACT $B$,(B non-nil $B$G$"$l$P!"(B\"$B$&!+(B\" $B$r(B \"(I3^(B\" $B$KJQ49$9(B
$B$k!#(B
$B0z?t$N(B START $B$H(B END $B$O?t;z$G$b%^!<%+!<$G$bNI$$!#(B"
  (interactive "*r\nP")
  (setq end (set-marker (make-marker) end))
  (skk-hiragana-to-jisx0201-region start end vcontract)
  (skk-katakana-to-jisx0201-region start end vcontract)
  (set-marker end nil))

(defun skk-hiragana-to-jisx0201-region
  (start end &optional vcontract latin-jisx0201)
  (skk-search-and-replace
   start end "[$B$!(B-$B$s(B]+"
   (lambda (matched) (save-match-data (skk-jisx0201-hankaku matched))))
  (if vcontract
      (skk-search-and-replace
       start end "$B$&!+(B" (lambda (matched) "(I3^(B")))
  (if latin-jisx0201
      nil
      ;; not yet
      ))

(defun skk-katakana-to-jisx0201-region
  (start end &optional vcontract latin-jisx0201)
  (skk-search-and-replace
   start end "[$B%!(B-$B%s(B]+"
   (lambda (matched) (save-match-data (skk-jisx0201-hankaku matched))))
  (if vcontract
      (skk-search-and-replace
       start end "$B%t(B" (lambda (matched) "(I3^(B")))
  (if latin-jisx0201
      nil
      ;; not yet
      ))

;;

(define-key skk-jisx0201-mode-map skk-kakutei-key 'skk-kakutei)
(define-key skk-jisx0201-mode-map "\C-q" 'skk-toggle-katakana)
(define-key skk-jisx0201-mode-map "\C-c\C-q" 'skk-toggle-jisx0201)
(define-key skk-j-mode-map "\C-q" 'skk-toggle-katakana)

(require 'product)
(product-provide (provide 'skk-jisx0201) (require 'skk-version))
;;; Local Variables:
;;; eval: (cond ((boundp 'standard-translation-table-for-decode) (setq standard-translation-table-for-decode (make-translation-table nil)))((boundp 'standard-character-unification-table-for-decode) (setq standard-character-unification-table-for-decode (make-unification-table nil))))
;;; End:
;;; skk-jisx0201.el ends here
