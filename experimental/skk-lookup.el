;;; skk-lookup.el --- SKK lookup gateway
;; Copyright (C) 1999 Mikio Nakajima <minakaji@osaka.email.ne.jp>

;; Author: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Maintainer: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Version: $Id: skk-lookup.el,v 1.20 1999/10/17 14:21:04 minakaji Exp $
;; Keywords: japanese
;; Created: Sep. 23, 1999
;; Last Modified: $Date: 1999/10/17 14:21:04 $

;; This file is not part of SKK yet.

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

;;; Commentary
;;
;; Keisuke Nishida <kxn30@po.cwru.edu> $B$5$s$N:n$i$l$?<-=q8!:w%D!<%k(B
;; Lookup $B$H(B SKK $B$H$N(B gateway $B$r9T$J$$!"(BLookup $B$G8!:w$G$-$k<-=q$r;H$C(B
;; $B$F8uJd$r=PNO$9$k%W%m%0%i%`$G$9!#EvA3$G$9$,!"(BLookup $B$,%$%s%9%H!<%k$5$l$F$$$F!"(B
;; $B$+$D!"BP1~$9$k<-=q$,%^%s%H$5$l$F$$$J$$$H;H$($^$;$s!#(B
;;
;; skk.el $B$K$"$k(B kill-buffer $B$N(B advice $B$r<!$N$b$N$HF~$lBX$(%$%s%9%H!<(B
;; $B%k$7D>$9I,MW$,$"$j$^$9(B (SKK 10.55 $B$O4{$KD>$C$F$$$^$9(B)$B!#(B
;;
;; (defadvice kill-buffer (around skk-ad activate)
;;   "SKK $B$N"'%b!<%I$@$C$?$i!"3NDj$7$F$+$i%P%C%U%!$r%-%k$9$k!#(B
;;   $B%P%C%U%!$N%-%k8e!"(BSKK $B$N%b!<%I$K=>$$%+!<%=%k$N?'$rJQ$($k!#(B"
;;   (and skk-mode skk-henkan-on (interactive-p) (skk-kakutei))
;;   ad-do-it
;;   ;; $BJL$N%P%C%U%!$XHt$V%3%^%s%I$O(B skk-mode $B$,(B nil $B$G$b%+!<%=%k?'$rD4@0$9$kI,MW(B
;;   ;; $B$,$"$k!#(B
;;   (skk-set-cursor-properly) )
;;
;; $B<!$N$h$&$K(B skk-search-prog-list $B$K2C$($F;XDj$7;HMQ$7$^$9!#(B
;; SKK $B$,MQ0U$7$F$$$k8!:w%W%m%0%i%`$NCf$G:G$b=E$$$N$G!"(B
;; skk-seach-server $B$N8!:w$N8e$K;}$C$F$/$k$N$,%;%*%j!<$G$9!#(B
;; 
;;  (setq skk-search-prog-list
;;        '((skk-search-jisyo-file skk-jisyo 0 t)
;;          (skk-search-server skk-aux-large-jisyo 10000)
;;          (skk-lookup-search) ))
;;
;; $B8=:_BP1~$7$F$$$k<-=q$O(B
;; 
;;   ispell, CHIEZO, CHUJITEN, COLLOC, GENIUS, GN99EP01, GN99EP02,
;;   IWAKOKU, KANWA, KOJIEN, KOKUGO, KOUJIEN, MYPAEDIA, NEWANC, PLUS,
;;   RIKAGAKU, WAEI
;;
;; $B$G$9(B (lookup-dictionary-name $B$,JV$9CM$GI85-$7$F$$$^$9(B)$B!#(B
;; kakasi (KAKASI $B$rMxMQ$9$k$J$i(B skk-kakasi.el $B$r;H$$$^$7$g$&(B),
;; ndcookie, ndnmz $B$K$OBP1~$$$F$$$^$;$s$7!"BP1~$NI,MW$O$J$$$H9M$($F$$(B
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
;; $B$-$^$7$?(B Keisuke Nishida $B$5$s!"3+H/$N=i4|$+$i%G%P%C%0$r<jEA$C$F$$$?$@$$$?!"(B
;; NEMOTO Takashi <tnemoto@mvi.biglobe.ne.jp> $B$5$s!"(B
;; sphere <sphere@pop12.odn.ne.jp> $B$5$s$K?<$/46<U$$$?$7$^$9!#(B

;;; Code:
(eval-when-compile (require 'skk) (require 'skk-num) (require 'cl))
(require 'lookup)

;;;###autoload
(defgroup skk-lookup nil "SKK lookup related customization."
  :prefix "skk-lookup-"
  :group 'skk )

;;;; user variables.
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
  :require 'lookup-vars )

(defcustom skk-lookup-option-alist
  '(
    ;; "[spla -> splat]"
    ("ispell" exact nil nil "-> \\([^ ]+\\)]$" nil)
    ;; what's this?
    ("jedict" exact nil nil nil nil)
    ;; $B!V<-!&E5!&HW!W(B "$B$"$+#3(B $B^@(B", "ethanol"
    ("CHUJITEN" exact prefix nil "[$B#0(B-$B#9(B]* *\\([^ ]+\\)$" nil)
    ;; "($BHiIf$J$I$N(B)$B$"$+(B <grime>", "$B!T1Q!U(B ($B%Q%$%W$J$I$N(B)$B$"$+(B <fur>"
    ("COLLOC" exact prefix nil "\\([^ $B!T!U(B]+\\) <[a-z]+>$" nil)
    ;; $B%8!<%K%"%91QOB(B, "$B$"$+(B[$B^@(B]"
    ("GENIUS" exact prefix nil "\\[\\(.+\\)\\]" nil)
    ;; Super$BE}9g<-=q(B99 Disk1, 2/$B8=BeMQ8l$N4pACCN<1(B
    ;; "$B!&(B" $B$,6h@Z$jJ8;z$G$"$k$H$-$H$=$&$G$J$$$H$-$,$"$k$J$!(B...$B!#(B
    ;; "$B"!<k!&3t!&<l!&<n!L;w$?$b$N4A;z!M(B" "$B"!@V%o%$%s!&%V!<%`!L7r9/LdBj!M(B"
    ("GN99EP01" exact prefix nil "^$B"!(B\\([^$B!L!M(B]+\\)$B!L(B.+$B!M(B$" nil)
    ("GN99EP02" exact prefix nil "^$B"!(B\\([^$B!L!M(B]+\\)$B!L(B.+$B!M(B$" nil)
    ;; IWAKOKU: $B!V<-!&E5!&HW!W(B
    ;; "$B$7$?$$!Z;`BN!&;SBN![(B", "$B$7$?$$!Z;YBb![!Z;^Bb![(B",
    ;; "$B$"$$!Z0&![(B", "$B$"$$(B($B$"$p(B)$B!ZMu![(B"
    ;; "$B$"$$(B<gaiji=za52a>$B0%(B<gaiji=za52b>"
    ("IWAKOKU" exact prefix nil "$B!Z(B\\(.+\\)$B![(B" "$B![!Z(B\\|$B!&(B")
    ;; "$B9$(B", "$B@V(B"
    ("KANWA" exact prefix nil nil nil)
    ;; $B!V<-!&E5!&HW!W(B "$B9$(B"
    ("MYPAEDIA" exact prefix nil nil nil)
    ;; $B%K%e!<%"%s%+!<1QOB(B "$B$"$+#2(B $B9$(B"
    ("NEWANC" exact prefix nil "[$B#0(B-$B#9(B]* *\\([^ ]+\\)$" nil)
    ;; "$B!!$"$+(B <scud$B#2(B>", "$B!!!V$"$+!W(B <rust>"
    ("PLUS" exact prefix nil "^$B!!(B\\(.+\\) <[a-z$B#0(B-$B#9(B]+>$" nil)
    )
  "*$B<-=qKh$N8!:w!"J8;z@Z$j=P$7%*%W%7%g%s!#(B
$B%j%9%H$N3FMWAG$O2<5-$NDL$j!#(B

  0th: lookup-dictionary-name $B$,JV$9J8;zNs!#(B
  1th: $BAw$j$J$7JQ49$N:]$N(B search method $B$r<($9%7%s%\%k!#(B
  2th: $BAw$j$"$jJQ49$N:]$N(B search method $B$r<($9%7%s%\%k!#(Bnil $B$r;XDj$9$k$HAw$j$"$j(B
       $BJQ49$N:]$O$=$N<-=q$r8!:w$7$J$$!#(B
  3th: S $B<0!#$3$N>r7o$rK~$7$?$H$-$O8!:w$7$J$$!#(B
  4th: $B8uJd$r@Z$j=P$9$?$a$N(B regexp \(\(match-string 1\) $B$G8uJd$r<h$j=P$9$3$H$,(B
       $B$G$-$k$h$&;XDj$9$k(B\)$B!#@Z$j=P$5$:$KJ8;zNsA4BN$rBP>]$K$9$k$H$-$O!"(Bnil $B$r;XDj(B
       $B$9$k!#(B
  5th: $B@Z$j=P$5$l$?J8;zNs$NCf$K99$KJ#?t$N8uJd$r4^$`>l9g$N6h@Z$j$rI=$o$9(B regexp$B!#(B
       $BJ#?t$N8uJd$,F10l(B heading $B$NCf$K=PNO$5$l$J$$$H$-$O!"(Bnil $B$r;XDj$9$k!#(B

$B8=:_BP1~$7$F$$$k<-=qL>$O!"(B\"CHUJITEN\", \"COLLOC\", \"KANWA\", \"MYPAEDIA\",
\"PLUS\".

`lookup-entry-heading' $B$,<+J,$N;HMQ$9$k<-=q$+$i$I$N$h$&$JJ8;zNs$r<h$j=P$9$N$+(B
$B3N$+$a$?$$$H$-$O!"(B`skk-lookup-pickup-headings' $B$r;HMQ$9$k!#Nc$($P!"(B

 \(skk-lookup-pickup-headings \"$B$3$7$g$&(B\" 'exact\)"
  :type '(repeat
	  (list (string :tag "Dictionary name")
		(choice :tag "Search method for okuri nasi"
			(const exact) (const prefix)
			(const suffix) (const substring)
			(const regexp) (const keyword)
			(const text) )
		(choice :tag "Search method for okuri ari"
			(const exact) (const prefix)
			(const suffix) (const substring)
			(const regexp) (const keyword)
			(const text) )
		(sexp :tag "S expression not to search")
		(choice :tag "Regexp to substring candidate from heading"
			regexp (const nil) )
		(choice :tag "Regexp to split candidates"
		       regexp (const nil) )))
  :group 'skk-lookup )

(defcustom skk-lookup-default-option-list
  '(exact prefix nil "$B!Z(B\\([^$B!Z![(B]+\\)$B![(B" "$B!&(B")
  ;; CHIEZO: $B!V<-!&E5!&HW!W(B
  ;; KANJIGEN: Super$BE}9g<-=q(B99 Disk2/$B4A;z8;(B : EPWING
  ;; KOUJIEN: $B9-<-1q(B $BBh(B4$BHG(B($B4dGH(B,EPWING) $B%^%k%A%a%G%#%"HG(B
  ;; KOJIEN: $B9-<-1qBh(B5$BHG(B($B4dGH(B,EPWING)
  ;; RIKAGAKU: $BM}2=3X<-E5(B
  "*$B<-=q$N8!:w!"J8;z@Z$j=P$7%*%W%7%g%s$N%G%#%U%)%k%H!#(B
$B%j%9%H$N3FMWAG$O2<5-$NDL$j!#(B

  0th: $BAw$j$J$7JQ49$N:]$N(B search method $B$r<($9%7%s%\%k!#(B
  1th: $BAw$j$"$jJQ49$N:]$N(B search method $B$r<($9%7%s%\%k!#(Bnil $B$r;XDj$9$k$HAw$j$"$j(B
       $BJQ49$N:]$O$=$N<-=q$r8!:w$7$J$$!#(B
  2th: S $B<0!#$3$N>r7o$rK~$7$?$H$-$O8!:w$7$J$$!#(B
  3th: $B8uJd$r@Z$j=P$9$?$a$N(B regexp \(\(match-string 1\) $B$G8uJd$r<h$j=P$9$3$H(B
       $B$,$G$-$k$h$&;XDj$9$k(B\)$B!#@Z$j=P$5$:$KJ8;zNsA4BN$rBP>]$K$9$k$H$-$O!"(Bnil $B$r;XDj(B
       $B$9$k!#(B
  4th: $B@Z$j=P$5$l$?J8;zNs$NCf$K99$KJ#?t$N8uJd$r4^$`>l9g$N6h@Z$j$rI=$o$9(B regexp$B!#(B
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
		       (const regexp) (const keyword)
		       (const text) )
	       (choice :tag "Search method for okuri ari"
		       (const exact) (const prefix)
		       (const suffix) (const substring)
		       (const regexp) (const keyword)
		       (const text) )
	       (sexp :tag "S expression not to search")
	       (choice :tag "Regexp to substring candidate from heading"
		       regexp (const nil) )
	       (choice :tag "Regexp to split candidates"
		       regexp (const nil) ))
  :group 'skk-lookup )

(defcustom skk-lookup-search-modules nil
  "*$B8!:w%b%8%e!<%k$N@_Dj$N%j%9%H!#(B"
  :type '(repeat (cons :tag "Module" (string :tag "Name")
		       (repeat :tag "Dictionary" (string :tag "ID"))))
  :group 'skk-lookup )

;;;; internal variables.
(defvar skk-lookup-agent-list nil)
(defvar skk-lookup-default-module nil)
(defvar skk-lookup-module-list nil)

;;;; inline functions.
(defsubst skk-lookup-get-method (name)
  (save-match-data
    (let ((list (assoc name skk-lookup-option-alist)))
      ;; If you search via ndtpd, book's name and slash are attached to NAME
      ;; as prefix, like `IWANAMI/KOJIEN'.  The following forms will truncate
      ;; it to `KOJIEN'.
      (if (and (null list) (string-match "/\\(.+\\)$" name))
	  (setq list (assoc (match-string 1 name) skk-lookup-option-alist)) )
      (nth (if (or skk-henkan-okurigana skk-okuri-char)
	       1 0)
	   (if list (cdr list) skk-lookup-default-option-list) ))))

(defsubst skk-lookup-get-nonsearch-sex (name)
  (save-match-data
    (let ((list (assoc name skk-lookup-option-alist)))
      (if (and (null list) (string-match "/\\(.+\\)$" name))
	  (setq list (assoc (match-string 1 name) skk-lookup-option-alist)) )
      (nth 2 (if list (cdr list) skk-lookup-default-option-list)) )))

(defsubst skk-lookup-get-pickup-regexp (name)
  (save-match-data
    (let ((list (assoc name skk-lookup-option-alist)))
      (if (and (null list) (string-match "/\\(.+\\)$" name))
	  (setq list (assoc (match-string 1 name) skk-lookup-option-alist)) )
      (nth 3 (if list (cdr list) skk-lookup-default-option-list)) )))

(defsubst skk-lookup-get-split-regexp (name)
  (save-match-data
    (let ((list (assoc name skk-lookup-option-alist)))
      (if (and (null list) (string-match "/\\(.+\\)$" name))
	  (setq list (assoc (match-string 1 name) skk-lookup-option-alist)) )
      (nth 4 (if list (cdr list) skk-lookup-default-option-list)) )))

;;;; funcitions.
;;;###autoload
(defun skk-lookup-search ()
  (save-excursion
    (let ((module (skk-lookup-default-module))
	  (lookup-gaiji-alternate "")
	  (henkan-key (if skk-use-numeric-conversion
			  (skk-num-compute-henkan-key skk-henkan-key)
			skk-henkan-key ))
	  ;; if `lookup-enable-gaiji' is nil, gaiji tag like
	  ;; `<gaiji=za52a>' is put out.
	  ;; lookup-enable-gaiji
	  sex name method entries pickup-regexp split-regexp
	  candidates-string candidates-list )
      (if (or skk-henkan-okurigana skk-okuri-char)
	  (setq henkan-key (substring henkan-key 0 (1- (length henkan-key)))) )
      ;; search pattern.
      (setq lookup-search-pattern henkan-key)
      ;; setup modules.
      (lookup-module-setup module)
      (lookup-foreach
       (lambda (dictionary)
	 (when (and (lookup-dictionary-selected-p dictionary)
		    (setq name (lookup-dictionary-name dictionary))
		    (progn
		      (setq sex (skk-lookup-get-nonsearch-sex name))
		      (if (not sex) t (not (eval sex))) )
		    (setq method (skk-lookup-get-method name))
		    ;; valid method or not?
		    (memq method (lookup-dictionary-methods dictionary))
		    ;; actual search.
		    (setq entries (lookup-vse-search-query
				   dictionary
				   (lookup-make-query method lookup-search-pattern) )))
	   (setq pickup-regexp (skk-lookup-get-pickup-regexp name)
		 split-regexp (skk-lookup-get-split-regexp name) )
	   (lookup-foreach
	    (lambda (entry)
	      (setq candidates-string (lookup-entry-heading entry))
	      (if (not (string= lookup-search-pattern candidates-string))
		  (setq candidates-list
			;; pickup necessary string for SKK.
			(if (not (or pickup-regexp split-regexp))
			    (cons candidates-string candidates-list)
			  (nconc
			   (skk-lookup-process-heading
			    candidates-string pickup-regexp split-regexp )
			   candidates-list )))))
	    entries )))
       ;; dictionaries to be searched.
       (lookup-module-dictionaries module) )
      (nreverse candidates-list) )))

(defun skk-lookup-process-heading (heading pickup-regexp split-regexp)
  ;; heading $B$7$+<h$j=P$5$J$$$N$O$b$C$?$$$J$$!)(B  $BB>$K$b>pJs$r<h$j=P$7(B
  ;; $B$F$*$$$F!"I,MW$K1~$8$F;2>H$9$k$+!)(B
  (save-match-data
    (do (candidates-string candidates-list)
	((or (string= heading "")
	     (and pickup-regexp (not (string-match pickup-regexp heading))) )
	 candidates-list )
      (if pickup-regexp
	  (setq candidates-string (match-string 1 heading)
		heading (substring heading (min (+ (match-end 1) skk-kanji-len)
						(length heading) )))
	(setq candidates-string heading
	      heading "" ))
      (if split-regexp
	  (lookup-foreach
	   (lambda (c)
	     (if (and (or skk-henkan-okurigana skk-okuri-char)
		      (> (length c) skk-kanji-len) )
		 (setq c (substring c 0 (- (length c) skk-kanji-len))) )
	     (setq candidates-list (cons c (delete c candidates-list))) )
	   (split-string candidates-string split-regexp) )
	(if (not (string= lookup-search-pattern candidates-string))
	    (setq candidates-list
		  (cons candidates-string
			(delete candidates-string candidates-list) )))))))

;; The following four functions were imported from lookup.el and
;; lookup-types.el.
(defun skk-lookup-default-module ()
  (or skk-lookup-default-module
      (setq skk-lookup-default-module (car (skk-lookup-module-list))) ))

(defun skk-lookup-module-list ()
  (or skk-lookup-module-list
      (setq skk-lookup-module-list
	    (mapcar 'skk-lookup-new-module (or skk-lookup-search-modules
					       '(("%SKK-EVERY" "")) )))))
(defun skk-lookup-new-module (spec)
  (let ((name (car spec))
	(id-list (cdr spec))
	module agents match start )
    ;; get agent list
    (lookup-foreach (lambda (id)
		      ;; get the list of agents matched with ID
		      (setq match (concat "^" (regexp-quote id))
			    start agents )
		      (lookup-foreach
		       (lambda (e)
			 (when (string-match match (lookup-agent-id e))
			   (setq agents (cons e agents)) ))
		       (skk-lookup-agent-list) )
		      (when (eq start agents)
			(error "No match agent: %s" id) ))
		    ;; get a list of agent-IDs
		    (lookup-nunique
		     (mapcar (lambda (id)
			       (string-match "^[^:]*" id)
			       (substring id 0 (match-end 0)) )
			     id-list )))
    (setq agents (nreverse (lookup-nunique agents 'eq)))
    ;; construct module
    (setq module (lookup-make-module name nil))
    (lookup-module-put-property module 'agents agents)
    (lookup-module-put-property module 'id-list id-list)
    (lookup-module-init module) ))

(defun skk-lookup-agent-list ()
  (or skk-lookup-agent-list
      (setq skk-lookup-agent-list
	    (mapcar 'lookup-new-agent skk-lookup-search-agents))))

;; the following two are to check dictionary output of heading for 
;; creating new regexp.
(defun skk-lookup-test-regexp (regexp place string)
  "Search STRING by REGEXP and pick up a part of STRING in PLACE."
  (string-match regexp string)
  (match-string place string) )

(defun skk-lookup-pickup-headings (pattern method)
  "Search PATTERN by METHOD."
  (let ((module (skk-lookup-default-module))
	(lookup-gaiji-alternate "")
	;;lookup-enable-gaiji ;  not to put out gaiji.
	var )
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
			   var )))
	(lookup-vse-search-query
	 dictionary (lookup-make-query method pattern) )))
     (lookup-module-dictionaries module) )
    var ))

(provide 'skk-lookup)
;;; Local Variables:
;;; End:
;;; skk-lookup.el ends here
