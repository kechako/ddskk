;;; skk-lookup.el --- SKK lookup gateway
;; Copyright (C) 1999, 2000 Mikio Nakajima <minakaji@osaka.email.ne.jp>

;; Author: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Maintainer: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Version: $Id: skk-lookup.el,v 1.2 2000/09/10 01:07:41 minakaji Exp $
;; Keywords: japanese
;; Created: Sep. 23, 1999
;; Last Modified: $Date: 2000/09/10 01:07:41 $
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

;;; Commentary
;;
;; Keisuke Nishida <kxn30@po.cwru.edu> $B$5$s$N:n$i$l$?<-=q8!:w%D!<%k(B
;; Lookup $B$H(B SKK $B$H$N(B gateway $B$r9T$J$$!"(BLookup $B$G8!:w$G$-$k<-=q$r;H$C(B
;; $B$F8uJd$r=PNO$9$k%W%m%0%i%`$G$9!#(B
;;
;; <HOW TO INSTALL>
;; make $B$r<B9T$9$k:]$K!"(Blookup.el $B$K%Q%9$,DL$C$F$$$F(B require $B$G$-$k(B
;; $B$H$-$O!"K\%W%m%0%i%`$b<+F0E*$K%$%s%9%H!<%k$5$l$^$9!#(Blookup.el $B$,(B
;; $B%$%s%9%H!<%k$5$l$F$$$k$N$K(B Emacs $B$,8!=P$7$F$/$l$J$$$H$-$O!"(B
;; SKK-CFG $B$rJT=8$7$F(B VERSION_SPECIFIC_LISPDIR $B$K$=$N%Q%9$r=q$/$HNI(B
;; $B$$$G$7$g$&!#(B
;;
;; <HOW TO USE>
;; $BEvA3$G$9$,!"(BLookup $B$,%$%s%9%H!<%k$5$l$F$$$F!"$+$D!"BP1~$9$k<-=q$,(B
;; $B%^%&%s%H$5$l$F$$$J$$$H;H$($^$;$s!#(B
;;
;; $B<!$N$h$&$K(B skk-search-prog-list $B$K2C$($F;XDj$7;HMQ$7$^$9!#(B
;; SKK $B$,MQ0U$7$F$$$k8!:w%W%m%0%i%`$NCf$G:G$b=E$$$N$G!"(B
;; skk-seach-server $B$N8!:w$N8e$K;}$C$F$/$k$N$,%;%*%j!<$G$9!#(B
;;
;;  (setq skk-search-prog-list
;;        '((skk-search-jisyo-file skk-jisyo 0 t)
;;          (skk-search-server skk-aux-large-jisyo 10000)
;;          (skk-lookup-search)))
;;
;; $B%G%#%U%)%k%H$N@_Dj$G$O!"(Blookup $B$NJQ?t$G$"$k(B `lookup-search-agents'
;; $B$r%3%T!<$7$F(B ndkks, ndcookie, ndnmz $B$r<h$j5n$j!"(B
;; `skk-lookup-search-agents' $B$K%;%C%H$7$F$3$l$r8!:w$9$k$h$&$K$7$F$$$^$9!#(B
;; $B$b$A$m$s(B lookup $B$N8!:w$H$O0[$J$k@_Dj$r(B `skk-lookup-search-agents' $B$KL@(B
;; $B<($9$k$3$H$b2DG=$G$9!#(B
;;
;; $B8=:_BP1~$7$F$$$k<-=q$O(B
;;
;;   ispell, jedict, CHIEZO, CHUJITEN, COLLOC, GENIUS, GN99EP01,
;;   GN99EP02, IWAKOKU, KANJIGEN, KANWA, KOJIEN, KOKUGO, KOUJIEN,
;;   MYPAEDIA, NEWANC, PLUS, RIKAGAKU, WAEI
;;
;; $B$G$9(B (lookup-dictionary-name $B$,JV$9CM$GI85-$7$F$$$^$9(B)$B!#(B
;; kakasi (KAKASI $B$rMxMQ$9$k$J$i(B skk-kakasi.el $B$r;H$$$^$7$g$&(B),
;; ndcookie, ndnmz $B$K$OBP1~$7$F$$$^$;$s$7!"BP1~$NI,MW$O$J$$$H9M$($F$$(B
;; $B$^$9(B ($B%a%j%C%H$,$"$l$P65$($F2<$5$$(B)$B!#(B
;;
;; $B$4<+J,$G;HMQ$7$F$$$k<-=q$N=PNO$,>e<j$/<h$j9~$a$J$$$H$-$O!"(B
;; `skk-lookup-pickup-headings' $B$r;HMQ$7$FNc$($P!"(B
;;
;;   (skk-lookup-pickup-headings "$B$3$7$g$&(B" 'exact)
;;
;; $B$J$I$HI>2A$7$F(B ("$B$3$7$g$&(B" $B$NJ8;zNsItJ,$OLdBj$H$J$C$F$$$k8!:wBP>]$H(B
;; $BF~$lBX$($^$7$g$&(B) `lookup-dictionary-name' $B$H(B
;; `lookup-entry-heading' $B$,JV$9CM$r;29M$K!"(B`'skk-lookup-option-alist'
;; $B$KI,MW$J%j%9%H$r2C$($^$7$g$&!#?7$?$J%j%9%H$r2C$($i$l$?$i@'Hs:n<T$K(B
;; $B$bCN$;$F2<$5$$!#(Bdefault value $B$K<h$j9~$_$?$$$H;W$$$^$9!#$h$m$7$/$*(B
;; $B4j$$$$$?$7$^$9!#(B
;;
;; $BKvHx$J$,$i!"(BLookup $B$r:n$i$l$?(B Lookup Development Team $B$N3'MM!"(B
;; Lookup $B$N(B $B86:n<T$G$"$j!"K\%W%m%0%i%`$N3+H/$K$b$$$/$D$+5.=E$J$40U8+$r$$$?$@(B
;; $B$-$^$7$?(B Keisuke Nishida <kxn30@po.cwru.edu> $B$5$s!"3+H/$N=i4|$+$i%G(B
;; $B%P%C%0$r<jEA$C$F$$$?$@$$$?!"(BNEMOTO Takashi <tnemoto@mvi.biglobe.ne.jp>
;; $B$5$s!"(Bsphere <sphere@pop12.odn.ne.jp> $B$5$s$K?<$/46<U$$$?$7$^$9!#(B

;;; Code:
(eval-when-compile (require 'skk) (require 'skk-num) (require 'cl))

(require 'poe)
(require 'lookup)

;;;###autoload
(defgroup skk-lookup nil "SKK lookup related customization."
  :prefix "skk-lookup-"
  :group 'skk)

(defcustom skk-lookup-search-agents
  ;; copy-list is a C primitive of XEmacs, but FSFmacs has it
  ;; in cl.el.
  (let ((agents (copy-sequence lookup-search-agents))
	e )
    ;; use `skk-kakasi.el' instead of ndkks.
    (setq agents (delete '(ndkks) agents))
    (while (setq e (assq 'ndcookie agents))
      (setq agents (delq e agents)) )
    (while (setq e (assq 'ndnmz agents))
      (setq agents (delq e agents)) )
    agents )
  "*$B8!:w%(!<%8%'%s%H$N@_Dj$N%j%9%H!#(B
$B%j%9%H$N3FMWAG$O<!$N7A<0$r<h$k(B:

  \(CLASS LOCATION [KEY1 VALUE1 \[KEY2 VALUE2 \[...\]\]\]\)

CLASS $B$K$O!"%(!<%8%'%s%H$N<oN`$r%7%s%\%k$G;XDj$9$k!#(B
LOCATION $B$K$O!"%(!<%8%'%s%H$N=j:_$rJ8;zNs$G;XDj$9$k!#(B
KEY $B5Z$S(B VALUE $B$O>JN,2DG=$G!"%(!<%8%'%s%H$KBP$9$k%*%W%7%g%s$r;XDj$9$k!#(B

$BNc(B: (setq skk-lookup-search-agents
          '((ndtp \"dserver\" :port 2010)
            (ndeb \"/cdrom\" :enable (\"EIWA\")))))"
  :type '(repeat (sexp :tag "Agent"))	; type $B$O$A$g$C$H$d$d$3$7$9$.!&!&(B
  :group 'skk-lookup
  :require 'lookup-vars)

(defcustom skk-lookup-option-alist
  '(
    ;; "[spla -> splat]"
    ("ispell" exact nil nil (not skk-okuri-char) "-> \\([^ ]+\\)]$" nil)
    ;; what's this?
    ("jedict" exact nil nil (not skk-okuri-char) nil nil)
    ;; $B!V<-!&E5!&HW!W(B "$B$"$+#3(B $B^@(B", "ethanol"
    ("CHUJITEN" exact exact prefix t "[$B#0(B-$B#9(B]* *\\([^ ]+\\)$" nil)
    ;; "($BHiIf$J$I$N(B)$B$"$+(B <grime>", "$B!T1Q!U(B ($B%Q%$%W$J$I$N(B)$B$"$+(B <fur>"
    ("COLLOC" exact exact prefix t "\\([^ $B!T!U(B]+\\) <[a-z]+>$" nil)
    ;; $B%8!<%K%"%91QOB(B, "$B$"$+(B[$B^@(B]"
    ;; $B%8!<%K%"%91QOB!&OB1Q<-E5(B $B$$$l$+$((B[$BF~$lBX$((B,$BF~$l49$((B]
    ("GENIUS" exact exact prefix t "\\[\\(.+\\)\\]" ",")
    ;; Super$BE}9g<-=q(B99 Disk1, 2/$B8=BeMQ8l$N4pACCN<1(B
    ;; "$B!&(B" $B$,6h@Z$jJ8;z$G$"$k$H$-$H$=$&$G$J$$$H$-$,$"$k$J$!(B...$B!#(B
    ;; "$B"!<k!&3t!&<l!&<n!L;w$?$b$N4A;z!M(B" "$B"!@V%o%$%s!&%V!<%`!L7r9/LdBj!M(B"
    ("GN99EP01" exact exact prefix t "^$B"!(B\\([^$B!L!M(B]+\\)$B!L(B.+$B!M(B$" nil)
    ("GN99EP02" exact exact prefix t "^$B"!(B\\([^$B!L!M(B]+\\)$B!L(B.+$B!M(B$" nil)
    ;; IWAKOKU: $B!V<-!&E5!&HW!W(B
    ;; "$B$7$?$$!Z;`BN!&;SBN![(B", "$B$7$?$$!Z;YBb![!Z;^Bb![(B",
    ;; "$B$"$$!Z0&![(B", "$B$"$$(B($B$"$p(B)$B!ZMu![(B"
    ;; "$B$"$$(B<gaiji=za52a>$B0%(B<gaiji=za52b>"
    ("IWAKOKU" exact exact prefix t "$B!Z(B\\(.+\\)$B![(B" "$B![!Z(B\\|$B!&(B")
    ;; "$B9$(B", "$B@V(B"
    ("KANWA" exact exact prefix t nil nil)
    ;; $B!V<-!&E5!&HW!W(B "$B9$(B"
    ("MYPAEDIA" exact exact prefix t nil nil)
    ;; $B%K%e!<%"%s%+!<1QOB(B "$B$"$+#2(B $B9$(B"
    ("NEWANC" exact exact prefix t "[$B#0(B-$B#9(B]* *\\([^ ]+\\)$" nil)
    ;; "$B!!$"$+(B <scud$B#2(B>", "$B!!!V$"$+!W(B <rust>"
    ("PLUS" exact exact prefix t "^$B!!(B\\(.+\\) <[a-z$B#0(B-$B#9(B]+>$" nil)
   )
  "*$B<-=qKh$N8!:w!"J8;z@Z$j=P$7%*%W%7%g%s!#(B
$B%j%9%H$N3FMWAG$O2<5-$NDL$j!#(B

  0th: lookup-dictionary-name $B$,JV$9J8;zNs(B \($B<-=q<oJL$rI=$o$9(B\)$B!#!#(B
  1th: $BAw$j$J$7JQ49$N:]$N(B search method $B$r<($9%7%s%\%k!#(Bregexp $B8=:_$N$H$3$m;XDj(B
       $BIT2D!#(B
  2th: $BAw$j$"$jJQ49$G!"$+$D(B skk-process-okuri-early $B%*%W%7%g%s$r;XDj$7$F$$$J$$$H(B
       $B$-(B \($BAw$j2>L>7hDj$N8e$K8!:w$r3+;O$9$k$N$G!"Aw$j2>L>$,FCDj$G$-$k(B\) $B$N(B
       search method $B$r<($9%7%s%\%k!#(Bregexp $B8=:_$N$H$3$m;XDjIT2D!#(Bnil $B$r;XDj$9$k(B
       $B$H!"Aw$j$"$jJQ49$N:]$O$=$N<-=q$r8!:w$7$J$$!#(B
  3th: $BAw$j$"$jJQ49$G!"$+$D(B skk-process-okuri-early $B$G$"$k$H$-(B \($BAw$j2>L>7hDj$N(B
       $BA0$K8!:w$r3+;O$9$k$N$G!"Aw$j2>L>$,FCDj$G$-$J$$$N$G!"Aw$j2>L>$N$+$J(B prefix
       $B$r=|$$$?ItJ,$r8!:w%-!<$H$7$F(B lookup $B$KEO$7$F$$$k(B\) $B$N(B search method $B$r<($9(B
       $B%7%s%\%k!#(Bregexp $B8=:_$N$H$3$m;XDjIT2D!#(Bnil $B$r;XDj$9$k$HAw$j$"$jJQ49$N:]$O(B
       $B$=$N<-=q$r8!:w$7$J$$!#(B
  4th: S $B<0!#$3$N(B S $B<0$rI>2A$7$F(B nil $B$K$J$k$H$-$O8!:w$7$J$$!#$"$k0lDj$N>r7o$rK~(B
       $B$7$?>l9g$K8!:w$7$J$$$h$&$K;XDj$G$-$k!#(B
  5th: $B8uJd$r@Z$j=P$9$?$a$N(B regexp \(\(match-string 1\) $B$G8uJd$r<h$j=P$9$3$H$,(B
       $B$G$-$k$h$&;XDj$9$k(B\)$B!#@Z$j=P$5$:$KJ8;zNsA4BN$rBP>]$K$9$k$H$-$O!"(Bnil $B$r;XDj(B
       $B$9$k!#(B
  6th: $B@Z$j=P$5$l$?J8;zNs$NCf$K99$KJ#?t$N8uJd$r4^$`>l9g$N6h@Z$j$rI=$o$9(B regexp$B!#(B
       $BJ#?t$N8uJd$,F10l(B heading $B$NCf$K=PNO$5$l$J$$$H$-$O!"(Bnil $B$r;XDj$9$k!#(B

$B8=:_BP1~$7$F$$$k<-=qL>$O!"(B\"CHUJITEN\", \"COLLOC\", \"GENIUS\", \"GN99EP01\",
\"GN99EP02\", \"IWAKOKU\", \"KANWA\", \"MYPAEDIA\", \"NEWANC\", \"PLUS\".

`lookup-entry-heading' $B$,<+J,$N;HMQ$9$k<-=q$+$i$I$N$h$&$JJ8;zNs$r<h$j=P$9$N$+(B
$B3N$+$a$?$$$H$-$O!"(B`skk-lookup-pickup-headings' $B$r;HMQ$9$k!#Nc$($P!"(B

 \(skk-lookup-pickup-headings \"$B$3$7$g$&(B\" 'exact\)"
  :type '(repeat
	  (list (string :tag "Dictionary name")
		(choice :tag "Search method for okuri nasi"
			(const exact) (const prefix)
			(const suffix) (const substring)
			(const keyword) (const text)
			(const nil))
		(choice :tag "Search method for okuri ari (not process okuri early)"
			(const exact) (const prefix)
			(const suffix) (const substring)
			(const keyword) (const text)
			(const nil))
		(choice :tag "Search method for okuri ari (process okuri early)"
			(const exact) (const prefix)
			(const suffix) (const substring)
			(const keyword) (const text)
			(const nil))
		(sexp :tag "S expression to search")
		(choice :tag "Regexp to substring candidate from heading"
			regexp (const nil))
		(choice :tag "Regexp to split candidates"
		       regexp (const nil))))
  :group 'skk-lookup)

(defcustom skk-lookup-default-option-list
  '(exact exact prefix t "$B!Z(B\\([^$B!Z![(B]+\\)$B![(B" "$B!&(B")
  ;; CHIEZO: $B!V<-!&E5!&HW!W(B
  ;; KANJIGEN: Super$BE}9g<-=q(B99 Disk2/$B4A;z8;(B : EPWING
  ;; KOUJIEN: $B9-<-1q(B $BBh(B4$BHG(B($B4dGH(B,EPWING) $B%^%k%A%a%G%#%"HG(B
  ;; KOJIEN: $B9-<-1qBh(B5$BHG(B($B4dGH(B,EPWING)
  ;; KOKUGO: what's this?
  ;; RIKAGAKU: $BM}2=3X<-E5(B
  ;; WAEI: what's this?
  "*$B%G%#%U%)%k%H$N<-=q8!:w!"J8;z@Z$j=P$7%*%W%7%g%s!#(B
$B$^$:<-=qL>$r%-!<$K$7$F(B `skk-lookup-option-alist' $B$r0z$-!"$=$3$K<-=q8!:w!"J8;z@Z(B
$B$j=P$7$N%*%W%7%g%s$,8+$D$+$l$P$=$l$r;HMQ$7!"8+$D$+$i$J$+$C$?>l9g$K$3$NJQ?t$G;XDj(B
$B$5$l$k<-=q8!:w!"J8;z@Z$j=P$7$N%*%W%7%g%s$r;HMQ$9$k!#(B

$B%j%9%H$N3FMWAG$O2<5-$NDL$j!#(B

  0th: $BAw$j$J$7JQ49$N:]$N(B search method $B$r<($9%7%s%\%k!#(Bregexp $B8=:_$N$H$3$m;XDj(B
       $BIT2D!#(B
  1th: $BAw$j$"$jJQ49$G!"$+$D(B skk-process-okuri-early $B%*%W%7%g%s$r;XDj$7$F$$$J$$$H(B
       $B$-(B \($BAw$j2>L>7hDj$N8e$K8!:w$r3+;O$9$k$N$G!"Aw$j2>L>$,FCDj$G$-$k(B\) $B$N(B
       search method $B$r<($9%7%s%\%k!#(Bregexp $B8=:_$N$H$3$m;XDjIT2D!#(Bnil $B$r;XDj$9$k(B
       $B$H!"Aw$j$"$jJQ49$N:]$O$=$N<-=q$r8!:w$7$J$$!#(B
  2th: $BAw$j$"$jJQ49$G!"$+$D(B skk-process-okuri-early $B$G$"$k(B \($BAw$j2>L>7hDj$NA0$K(B
       $B8!:w$r3+;O$9$k$N$G!"Aw$j2>L>$,FCDj$G$-$J$$$N$G!"Aw$j2>L>$N$+$J(B prefix $B$r=|(B
       $B$$$?ItJ,$r8!:w%-!<$H$7$F(B lookup $B$KEO$7$F$$$k(B\) $B$H$-$N(B search method $B$r<($9(B
       $B%7%s%\%k!#(Bregexp $B8=:_$N$H$3$m;XDjIT2D!#(Bnil $B$r;XDj$9$k$HAw$j$"$jJQ49$N:]$O(B
       $B$=$N<-=q$r8!:w$7$J$$!#(B
  3th: S $B<0!#$3$N(B S $B<0$rI>2A$7$F(B nil $B$K$J$k$H$-$O8!:w$7$J$$!#$"$k0lDj$N>r7o$rK~(B
       $B$7$?>l9g$K8!:w$7$J$$$h$&$K;XDj$G$-$k!#(B
  4th: $B8uJd$r@Z$j=P$9$?$a$N(B regexp \(\(match-string 1\) $B$G8uJd$r<h$j=P$9$3$H(B
       $B$,$G$-$k$h$&;XDj$9$k(B\)$B!#@Z$j=P$5$:$KJ8;zNsA4BN$rBP>]$K$9$k$H$-$O!"(Bnil $B$r;XDj(B
       $B$9$k!#(B
  5th: $B@Z$j=P$5$l$?J8;zNs$NCf$K99$KJ#?t$N8uJd$r4^$`>l9g$N6h@Z$j$rI=$o$9(B regexp$B!#(B
       $BJ#?t$N8uJd$,F10l(B heading $B$NCf$K=PNO$5$l$J$$$H$-$O!"(Bnil $B$r;XDj$9$k!#(B

$B$3$N%*%W%7%g%s$GBP1~$7$F$$$k<-=qL>$O!"(B\"CHIEZO\", \"KANJIGEN\", \"KOJIEN\",
\"KOUJIEN\", \"KOKUGO, \"RIKAGAKU\", \"WAEI\".
`lookup-entry-heading' $B$G<h$j=P$7$?J8;zNs$,2<5-$N$h$&$K$J$k$3$H$rA0Ds$K$7$F$$$k!#(B

  \"$B$"!>$+!Z0!2J![!E%/%o(B\"
  \"$B$"$+!Zod2@![(B\"
  \"$B$3!>$7$g$&!Z>.@+!&>.@-![!E%7%d%&(B\"

`lookup-entry-heading' $B$,<+J,$N;HMQ$9$k<-=q$+$i$I$N$h$&$JJ8;zNs$r<h$j=P$9$N$+(B
$B3N$+$a$?$$$H$-$O!"(B`skk-lookup-pickup-headings' $B$r;HMQ$9$k!#Nc$($P!"(B

 \(skk-lookup-pickup-headings \"$B$3$7$g$&(B\" 'exact\)"
  :type '(list (choice :tag "Search method for okuri nasi"
		       (const exact) (const prefix)
		       (const suffix) (const substring)
		       (const keyword) (const text)
		       (const nil))
	       (choice :tag "Search method for okuri ari (not process okuri early)"
		       (const exact) (const prefix)
		       (const suffix) (const substring)
		       (const keyword) (const text)
		       (const nil))
	       (choice :tag "Search method for okuri ari (process okuri early)"
		       (const exact) (const prefix)
		       (const suffix) (const substring)
		       (const keyword) (const text)
		       (const nil))
	       (sexp :tag "S expression to search")
	       (choice :tag "Regexp to substring candidate from heading"
		       regexp (const nil))
	       (choice :tag "Regexp to split candidates"
		       regexp (const nil)))
  :group 'skk-lookup)

(defcustom skk-lookup-search-modules nil
  "*$B8!:w%b%8%e!<%k$N@_Dj$N%j%9%H!#(B"
  :type '(repeat (cons :tag "Module" (string :tag "Name")
		       (repeat :tag "Dictionary" (string :tag "ID"))))
  :group 'skk-lookup)

;; internal variables.
(defvar skk-lookup-agent-list nil)
(defvar skk-lookup-default-module nil)
(defvar skk-lookup-module-list nil)

;; aliases.
(defalias-maybe 'skk-okurigana-prefix 'skk-auto-okurigana-prefix)

;;;; inline functions.
(defsubst skk-lookup-get-method (name okuri-process)
  (save-match-data
    (let ((list (assoc name skk-lookup-option-alist))
	  sex)
      ;; If you search via ndtpd, book's name and slash are attached to NAME
      ;; as prefix, like `IWANAMI/KOJIEN'.  The following forms will truncate
      ;; it to `KOJIEN'.
      (if (and (null list) (string-match "/\\(.+\\)$" name))
	  (setq list (assoc (match-string 1 name) skk-lookup-option-alist)))
      (setq sex (nth okuri-process (if list (cdr list) skk-lookup-default-option-list)))
      (cond ((symbolp sex) sex)
	    (t (eval sex))))))

(defsubst skk-lookup-get-nonsearch-sex (name)
  (save-match-data
    (let ((list (assoc name skk-lookup-option-alist)))
      (if (and (null list) (string-match "/\\(.+\\)$" name))
	  (setq list (assoc (match-string 1 name) skk-lookup-option-alist)))
      (nth 3 (if list (cdr list) skk-lookup-default-option-list)))))

(defsubst skk-lookup-get-pickup-regexp (name)
  (save-match-data
    (let ((list (assoc name skk-lookup-option-alist)))
      (if (and (null list) (string-match "/\\(.+\\)$" name))
	  (setq list (assoc (match-string 1 name) skk-lookup-option-alist)))
      (nth 4 (if list (cdr list) skk-lookup-default-option-list)))))

(defsubst skk-lookup-get-split-regexp (name)
  (save-match-data
    (let ((list (assoc name skk-lookup-option-alist)))
      (if (and (null list) (string-match "/\\(.+\\)$" name))
	  (setq list (assoc (match-string 1 name) skk-lookup-option-alist)))
      (nth 5 (if list (cdr list) skk-lookup-default-option-list)))))

;;;; funcitions.
;;;###autoload
(defun skk-lookup-search ()
  (if (and (boundp 'skk-num-list) (or skk-num-list skk-num-recompute-key))
      ;; $B?tCMJQ49$N$H$-$OJQ49%-!<$,(B `#' $B$r4^$`$b$N$J$N$G!"(Blookup $B$G8!:w$7$J$$!#(B
      nil
    (save-excursion
      (let ((module (skk-lookup-default-module))
	    ;; if `lookup-enable-gaiji' is nil, gaiji tag like
	    ;; `<gaiji=za52a>' is put out.
	    ;;(lookup-enable-gaiji nil)
	    (lookup-gaiji-alternate "")
	    (henkan-key skk-henkan-key)
	    okuri-process)
	(cond ((not (or skk-henkan-okurigana skk-okuri-char))
	       ;; okuri-nasi
	       (setq okuri-process 0))
	      ;; okuri-ari and (not skk-process-okuri-early)
	      (skk-henkan-okurigana
	       ;; search method $B$K(B regexp $B$r5v$9$J$i$P$3$3$G(B henkan-key $B$r7h$aBG$A$;$:(B
	       ;; $B$K0l9)IW$$$k$M(B...$B!#(B
	       (setq henkan-key (concat (substring henkan-key 0 (1- (length henkan-key)))
					skk-henkan-okurigana)
		     okuri-process 1))
	      ;; okuri-ari and skk-process-okuri-early
	      (skk-okuri-char
	       ;; $BAw$j2>L>$N$+$J(B prefix $B$r<N$F$F(B lookup $B$KEO$9!#(B
	       (setq henkan-key (substring henkan-key 0 (1- (length henkan-key)))
		     okuri-process 2)))
	(skk-lookup-search-1 module henkan-key okuri-process)))))

(defun skk-lookup-search-1 (module key okuri-process)
  ;; search pattern.
  (let (name method entries pickup-regexp split-regexp
	     candidates-string candidates-list)
    (setq lookup-search-pattern key)
    ;; setup modules.
    (lookup-module-setup module)
    (lookup-foreach
     (lambda (dictionary)
       (when (and (lookup-dictionary-selected-p dictionary)
		  (setq name (lookup-dictionary-name dictionary))
		  (eval (skk-lookup-get-nonsearch-sex name))
		  (setq method (skk-lookup-get-method name okuri-process))
		  ;; valid method or not?
		  (memq method (lookup-dictionary-methods dictionary))
		  ;; actual search.
		  (setq entries (lookup-vse-search-query
				 dictionary
				 (lookup-make-query method lookup-search-pattern))))
	 (setq pickup-regexp (skk-lookup-get-pickup-regexp name)
	       split-regexp (skk-lookup-get-split-regexp name))
	 (lookup-foreach
	  (lambda (entry)
	    ;; pickup necessary string for SKK.
	    (setq candidates-string (lookup-entry-heading entry))
	    (if (not (or pickup-regexp split-regexp))
		(progn
		  (setq candidates-string (skk-lookup-process-okurigana
					   candidates-string
					   okuri-process))
		  (if (and candidates-string
			   (not (string= lookup-search-pattern candidates-string)))
		      (setq candidates-list (cons candidates-string
						  candidates-list))))
	      (setq candidates-list
		    (nconc (skk-lookup-process-heading
			    candidates-string pickup-regexp split-regexp
			    okuri-process)
			   candidates-list))))
	  entries)))
     ;; dictionaries to be searched.
     (lookup-module-dictionaries module))
    (nreverse candidates-list)))

(defun skk-lookup-process-okurigana (string process-type)
  (cond ((string= string "")
	 ;; KOUJIEN has a heading like `$B$^!>$-!Z??LZ!&(B(GAIJI)$B!&Kj![(B'
	 ;; As GAIJI cannot be processed by skk-lookup.el, the heading
	 ;; is equal to `$B$^!>$-!Z??LZ!&!&Kj![(B' for skk-lookup.el.
	 ;; It causes to produce a null string candidate. 
	 ;;   (split-string "$B??LZ!&!&Kj(B" "$B!&(B") -> ("$B??LZ(B" "" "$BKj(B")
	 ;; So return nil if STRING is a null string.
	 nil)
	((= process-type 0) string)
	(t
	 (let ((okuri-length
		(cond ((= process-type 1) (length skk-henkan-okurigana))
		      ((= process-type 2)
		       ;; don't know exactly how long okurigana is.
		       ;; truncate length of one character anyway.
		       skk-kanji-len))))
	   (cond ((= process-type 2)
		  (cond ((> okuri-length (length string))
			 string)
			((string= (skk-okurigana-prefix (substring string -1))
				  skk-okuri-char)
			 (substring string 0 (- okuri-length)))))
		 ((not (string= skk-henkan-okurigana
				(substring string (- okuri-length))))
		  nil)
		 ((> okuri-length (length string)) string)
		 (t (substring string 0 (- okuri-length))))))))

(defun skk-lookup-process-heading
  (heading pickup-regexp split-regexp okuri-process-type)
  ;; heading $B$7$+<h$j=P$5$J$$$N$O$b$C$?$$$J$$!)(B  $BB>$K$b>pJs$r<h$j=P$7(B
  ;; $B$F$*$$$F!"I,MW$K1~$8$F;2>H$9$k$+!)(B
  (save-match-data
    (do (candidates-string candidates-list)
	((or (string= heading "")
	     (and pickup-regexp (not (string-match pickup-regexp heading))))
	 candidates-list)
      (if pickup-regexp
	  (setq candidates-string (match-string 1 heading)
		heading (substring heading (min (+ (match-end 1) skk-kanji-len)
						(length heading))))
	(setq candidates-string heading
	      heading ""))
      (if split-regexp
	  (lookup-foreach
	   (lambda (c)
	     (if (string= lookup-search-pattern c)
		 nil
	       (setq c (skk-lookup-process-okurigana c okuri-process-type))
	       (if c
		   (setq candidates-list (cons c (delete c candidates-list))))))
	   (split-string candidates-string split-regexp))
	(if (string= lookup-search-pattern candidates-string)
	    nil
	  (setq candidates-string (skk-lookup-process-okurigana
				   candidates-string okuri-process-type))
	  (if candidates-string
	      (setq candidates-list
		    (cons candidates-string
			  (delete candidates-string candidates-list)))))))))

;; The following four functions were imported from lookup.el and
;; lookup-types.el.
(defun skk-lookup-default-module ()
  (or skk-lookup-default-module
      (setq skk-lookup-default-module (car (skk-lookup-module-list)))))

(defun skk-lookup-module-list ()
  (or skk-lookup-module-list
      (setq skk-lookup-module-list
	    (mapcar 'skk-lookup-new-module (or skk-lookup-search-modules
					       '(("%SKK-EVERY" "")))))))
(defun skk-lookup-new-module (spec)
  (let ((name (car spec))
	(id-list (cdr spec))
	module agents match start)
    ;; get agent list
    (lookup-foreach (lambda (id)
		      ;; get the list of agents matched with ID
		      (setq match (concat "^" (regexp-quote id))
			    start agents)
		      (lookup-foreach
		       (lambda (e)
			 (when (string-match match (lookup-agent-id e))
			   (setq agents (cons e agents))))
		       (skk-lookup-agent-list))
		      (when (eq start agents)
			(error "No match agent: %s" id)))
		    ;; get a list of agent-IDs
		    (lookup-nunique
		     (mapcar (lambda (id)
			       (string-match "^[^:]*" id)
			       (substring id 0 (match-end 0)))
			     id-list)))
    (setq agents (nreverse (lookup-nunique agents 'eq)))
    ;; construct module
    (setq module (lookup-make-module name nil))
    (lookup-module-put-property module 'agents agents)
    (lookup-module-put-property module 'id-list id-list)
    (lookup-module-init module)))

(defun skk-lookup-agent-list ()
  (or skk-lookup-agent-list
      (setq skk-lookup-agent-list
	    (mapcar 'lookup-new-agent skk-lookup-search-agents))))

;; the following two are to check dictionary output of heading for
;; creating new regexp.
(defun skk-lookup-test-regexp (regexp place string)
  "Search STRING by REGEXP and pick up a part of STRING in PLACE."
  (string-match regexp string)
  (match-string place string))

(defun skk-lookup-pickup-headings (pattern method)
  "Search PATTERN by METHOD."
  (let ((module (skk-lookup-default-module))
	(lookup-gaiji-alternate "")
	;;lookup-enable-gaiji ;  not to put out gaiji.
	var)
    (lookup-module-setup module)
    (lookup-foreach
     (lambda (dictionary)
       (lookup-foreach
	(lambda (entry)
	  (setq var (nconc (list
			    (list (lookup-dictionary-name dictionary)
				  (lookup-dictionary-id dictionary)
				  (lookup-entry-heading entry)
				  ;;(lookup-dictionary-command dictionary 'content entry)
				 ))
			   var)))
	(lookup-vse-search-query
	 dictionary (lookup-make-query method pattern))))
     (lookup-module-dictionaries module))
    var))

(provide 'skk-lookup)
;;; Local Variables:
;;; End:
;;; skk-lookup.el ends here
