;;; skk-num.el --- $B?tCMJQ49$N$?$a$N%W%m%0%i%`(B
;; Copyright (C) 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997,
;;               1998, 1999
;; Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>

;; Author: Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>
;; Maintainer: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Version: $Id: skk-num.el,v 1.2 1999/08/29 13:28:01 minakaji Exp $
;; Keywords: japanese
;; Last Modified: $Date: 1999/08/29 13:28:01 $

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

;;; Change log:

;; Following people contributed modifications to skk.el (Alphabetical order):
;;      Hideki Sakurada <sakurada@kuis.kyoto-u.ac.jp>
;;      Manabu Kawashima <kaw@lp.nm.fujitsu.co.jp>

;;; Code:
(eval-when-compile (require 'skk))
(require 'skk-foreword)
(require 'cl)

(defgroup skk-num nil "SKK number conversion related customization."
  :prefix "skk-num-"
  :group 'skk )

;; user variables.
;;;###autoload
(defcustom skk-num-type-alist
  '((0 . identity)
    (1 . skk-num-jisx0208-latin)
    (2 . skk-num-type2-kanji)
    (3 . skk-num-type3-kanji)
    (4 . skk-num-recompute)
    (5 . skk-num-type5-kanji)
    (9 . skk-num-shogi) )
  "*$B?tCM$NJQ49$N$?$a$N!"%$%s%G%/%9$HJQ49$K;HMQ$9$k4X?t$H$N%(!<%j%9%H!#(B
$B3FMWAG$O!"(B`($B%$%s%G%/%9(B . $B4X?tL>(B)' $B$H$$$&9=@.$K$J$C$F$$$k!#(B
car $BItJ,$O!"Nc$($P!"8+=P$78l$,(B \"$BJ?@.(B#1$BG/(B\" $B$N$H$-!"(B`#' $B5-9f$ND>8e$KI=<($5$l$k?t(B
$B;z(B `1' $B$rBeF~$9$k!#(B

$B%$%s%G%/%9$H4X?t$N4X78$O2<5-$NDL$j!#(B
    0 -> $BL5JQ49(B
    1 -> $BA43Q?t;z$XJQ49(B
    2 -> $B4A?t;z$XJQ49(B \($B0L<h$j$J$7(B\)
    3 -> $B4A?t;z$XJQ49(B \($B0L<h$j$r$9$k(B\)
    4 -> $B$=$N?t;z$=$N$b$N$r%-!<$K$7$F<-=q$r:F8!:w(B
    5 -> $B4A?t;z(B ($B<j7A$J$I$G;HMQ$9$kJ8;z$r;HMQ(B) $B$XJQ49(B ($B0L<h$j$r$9$k(B)
    9 -> $B>-4}$G;HMQ$9$k?t;z(B \(\"$B#3;M(B\" $B$J$I(B\) $B$KJQ49(B" 
  :type '(repeat (cons
		  (choice (integer 0 :tag "Muhenkan")
			  (integer 1 :tag "Zenkaku Henkan")
			  (integer 2 :tag "Kansuuji Henkan (Kuraidori ari)")
			  (integer 3 :tag "Kansuuji Henkan (Kuraidori nasi)")
			  (integer 4 :tag "Saikensaku")
			  (integer 5 :tag "Kansuuji Henkan (old Kanji)")
			  (integer 9 :tag "Shogi Moji") )
		  function ))
  :group 'skk-num )

(defcustom skk-num-convert-float nil
  "*Non-nil $B$G$"$l$P!"IbF0>.?tE@?t$r;H$C$?8+=P$78l$KBP1~$7$FJQ49$r9T$J$&!#(B
$B$3$NCM$r(B non-nil $B$K$9$k$3$H$G!"(B\"#.# /#1$B!%(B#1/#0$B7n(B#0$BF|(B/\" $B$J$I$N<-=q8+=P$7$,;HMQ(B
$B$G$-$J$/$J$k$N$G!"Cm0U!#(B"
  :type 'boolean
  :group 'skk-num )

;;;###autoload
(defcustom skk-num-uniq (or (assq 4 skk-num-type-alist)
			    (and (assq 2 skk-num-type-alist)
				 (assq 3 skk-num-type-alist) ))
  "*Non-nil $B$G$"$l$P!"0[$J$k?tCMI=8=$G$bJQ497k2L$,F1$8?tCM$r=EJ#$7$F=PNO$7$J$$!#(B"
  :type 'boolean
  :group 'skk-num )

(defcustom skk-num-load-hook nil
  "*skk-num.el $B$r%m!<%I$7$?8e$K%3!<%k$5$l$k%U%C%/!#(B"
  :type 'hook
  :group 'skk-num )

;; internal constants and variables
;;;###autoload
(defconst skk-num-alist-type1
  '((?0 . "$B#0(B") (?1 . "$B#1(B") (?2 . "$B#2(B") (?3 . "$B#3(B")
    (?4 . "$B#4(B") (?5 . "$B#5(B") (?6 . "$B#6(B") (?7 . "$B#7(B")
    (?8 . "$B#8(B") (?9 . "$B#9(B")
    (?. . "$B!%(B")				; $B>.?tE@!#(B(?. . ".") $B$NJ}$,NI$$?M$b$$$k$+$b(B...$B!#(B
    (?  . "") )
  "ascii $B?t;z$N(B char type $B$HA43Q?t;z$N(B string type $B$NO"A[%j%9%H!#(B
\"1995\" -> \"$B#1#9#9#5(B\" $B$N$h$&$JJ8;zNs$NJQ49$r9T$&:]$KMxMQ$9$k!#(B" )

(defconst skk-num-alist-type2
  '((?0 . "$B!;(B") (?1 . "$B0l(B") (?2 . "$BFs(B") (?3 . "$B;0(B")
    (?4 . "$B;M(B") (?5 . "$B8^(B") (?6 . "$BO;(B") (?7 . "$B<7(B")
    (?8 . "$BH,(B") (?9 . "$B6e(B") (?  . "") )
  "ascii $B?t;z$N(B char type $B$H4A?t;z$N(B string type $B$NO"A[%j%9%H!#(B
\"1995\" -> \"$B0l6e6e8^(B\" $B$N$h$&$JJ8;zNs$NJQ49$r9T$&:]$KMxMQ$9$k!#(B" )

(defconst skk-num-alist-type5
  '((?1 . "$B0m(B") (?2 . "$BFu(B") (?3 . "$B;2(B")
    (?4 . "$B;M(B") (?5 . "$B8`(B") (?6 . "$BO;(B") (?7 . "$B<7(B")
    (?8 . "$BH,(B") (?9 . "$B6e(B") (?  . "") )
  "ascii $B?t;z$N(B char type $B$H4A?t;z$N(B string type $B$NO"A[%j%9%H!#(B
\"1995\" -> \"$B0mot6eI46e=&8`(B\" $B$N$h$&$JJ8;zNs$NJQ49$r9T$&:]$KMxMQ$9$k!#(B" )

;;;###autoload
(skk-deflocalvar skk-num-list nil
  "skk-henkan-key $B$NCf$K4^$^$l$k?t;z$rI=$9J8;zNs$N%j%9%H!#(B
$BNc$($P!"(B\"$B"&$X$$$;$$(B7$B$M$s(B10$B$,$D(B\" $B$NJQ49$r9T$&$H$-!"(Bskk-henkan-key $B$O(B
\"$B$X$$$;$$(B7$B$M$s(B10$B$,$D(B\" $B$G$"$j!"(Bskk-num-list $B$O(B \(\"7\" \"10\"\) $B$H$J$k!#(B" )

;;;###autoload
(skk-deflocalvar skk-num-recompute-key nil
  "#4 $B%?%$%W$N%-!<$K$h$j?tCM$N:F7W;;$r9T$J$C$?$H$-$N8!:w%-!<!#(B" )

;;;###autoload
(defun skk-num-compute-henkan-key (key)
  ;; KEY $B$NCf$NO"B3$9$k?t;z$r8=$o$9J8;zNs$r(B "#" $B$KCV$-49$($?J8;zNs$rJV$9!#(B"12"
  ;; $B$d(B "$B#0#9(B" $B$J$IO"B3$9$k?t;z$r(B 1 $B$D$N(B "#" $B$KCV$-49$($k$3$H$KCm0U!#(B
  ;; $BCV$-49$($??t;z$r(B skk-num-list $B$NCf$K%j%9%H$N7A$GJ]B8$9$k!#(B
  ;; $BNc$($P!"(BKEY $B$,(B "$B$X$$$;$$(B7$BG/(B12$B$,$D(B" $B$G$"$l$P!"(B"$B$X$$$;$$(B#$B$M$s(B#$B$,$D(B"
  ;; $B$HJQ49$7!"(Bskk-num-list $B$K(B ("7" "12") $B$H$$$&%j%9%H$rBeF~$9$k!#(B
  ;; $B<-=q$N8+=P$78l$N8!:w$K;HMQ$9$k!#(B
  (let ((numexp (if skk-num-convert-float
		    "[.0-9]+" "[0-9]+" )))
    ;;(setq skk-noconv-henkan-key key)
    (save-match-data
      ;; $B0L<h$j$N(B "," $B$r=|5n$9$k!#(B
      (while (string-match "," key)
	(setq key (concat (substring key 0 (match-beginning 0))
			  (substring key (match-end 0)) )))
      ;; $BA43Q?t;z$r(B ascii $B?t;z$KJQ49$9$k!#(B
      (while (string-match "[$B#0(B-$B#9(B]" key)
        (let ((zen-num (match-string 0 key)))
          (setq key (concat (substring key 0 (match-beginning 0))
                            (skk-jisx0208-to-ascii zen-num)
                            (substring key (match-end 0)) ))))
      ;; ascii $B?t;z$r(B "#" $B$KCV$-49$(!"$=$N?t;z$r(B skk-num-list $B$NCf$KJ]B8!#(B
      (while (string-match numexp key)
        (setq skk-num-list (nconc skk-num-list (list (match-string 0 key)))
              key (concat (substring key 0 (match-beginning 0))
                          "#"
                          (substring key (match-end 0)) )))))
  key )

;;;###autoload
(defun skk-num-convert (key)
  ;; KEY $B$H(B skk-num-list $B$+$i?tCMJQ498e$NJ8;zNs$rJV$9!#(B
  ;; skk-henkan-count $B$,;X$7$F$$$k?tCMJQ49%-!<$N8uJd$rJQ49$7!"(B
  ;; skk-henkan-list $B$r(B
  ;;   ("#2" ... ) -> (("#2" ."$B0l(B") ...)
  ;; $B$N$h$&$KJQ7A$9$k!#(B
  (if (not key)
      nil
    (let ((numexp (if skk-num-convert-float
                      "#[.0-9]+" "#[0-9]+" ))
          (n 0)
          (workkey key)
          num convnum string convlist current )
      (save-match-data
        (while (and (setq num (nth n skk-num-list))
                    (string-match numexp workkey) )
          (setq convnum (skk-num-exp num (string-to-number
                                          (substring workkey
                                                     (1+ (match-beginning 0))
                                                     (match-end 0) )))
                string (substring workkey 0 (match-beginning 0))
                workkey (substring workkey (match-end 0))
                n (1+ n) )
          (if (not (and (stringp convnum) (string= convnum "")
                        (string= string "") ))
              (setq convlist (nconc convlist (list string convnum))) ))
        (setq convlist (nconc convlist (list workkey)))
        (cond ((null convlist) nil)
              ((and (null (cdr convlist)) (stringp (car convlist)))
               (setq current (car convlist)) )
              ;; RAW-LIST $B$NA4MWAG$,J8;zNs!#(B
              ((null (memq t (mapcar 'listp convlist)))
               (setq current (mapconcat 'identity convlist ""))
               (if (and (> skk-henkan-count -1)
                        (nth skk-henkan-count skk-henkan-list) )
                   ;; ("A" "#2" "C") -> ("A" ("#2" ."$B0l(B") "C")
                   (setf (nth skk-henkan-count skk-henkan-list)
                         (cons key current) )
                 (setq skk-henkan-list
                       (nconc skk-henkan-list (list (cons key current))) )))
              ;; #4
              (t (let ((l (mapcar (function (lambda (e) (cons key e)))
                                  (skk-num-flatten-list (delete "" convlist)) )))
                   (setq current (cdr (car l)))
                   (if (and (> skk-henkan-count -1)
                            (nth skk-henkan-count skk-henkan-list) )
                       (progn
                         (setf (nth skk-henkan-count skk-henkan-list) (car l))
                         (setq skk-henkan-list (skk-splice-in
                                                skk-henkan-list
                                                (1+ skk-henkan-count)
                                                (cdr l) )))
                     (setq skk-henkan-list (nconc skk-henkan-list l)) ))))
        current ))))

;;;###autoload
(defun skk-num-convert*7 ()
  (let ((skk-henkan-count skk-henkan-count)
        (n 7) )
    (while (and (> n 0) (nth skk-henkan-count skk-henkan-list))
      (skk-num-convert (skk-get-current-candidate))
      (setq skk-henkan-count (1+ skk-henkan-count)
            n (1- n) ))
    (and skk-num-recompute-key (skk-num-uniq)) ))

(defun skk-num-rawnum-exp (string)
  (setq string (skk-num-rawnum-exp-1
                string "[$B#0(B-$B#9(B][$B!;0l6e8^;0;M<7FsH,O;(B]" "#9" 0 ))
  (setq string (skk-num-rawnum-exp-1
                string "\\(^\\|[^#0-9]\\)\\([0-9]+\\)" "#0" 2 ))
  (setq string (skk-num-rawnum-exp-1
                string "[$B#0(B-$B#9(B]+" "#1" 0 ))
  (setq string (skk-num-rawnum-exp-1
                string "\\([$B!;0l6e8^;0;M<7FsH,O;==(B][$B==I4@iK|2/C{5~(B]\\)+" "#3" 0 ))
  ;; (mapcar 'char-to-string
  ;;         (sort
  ;;          '(?$B0l(B ?$BFs(B ?$B;0(B ?$B;M(B ?$B8^(B ?$BO;(B ?$B<7(B ?$BH,(B ?$B6e(B ?$B!;(B) '<))
  ;;   --> ("$B!;(B" "$B0l(B" "$B6e(B" "$B8^(B" "$B;0(B" "$B;M(B" "$B<7(B" "$BFs(B" "$BH,(B" "$BO;(B")
  ;;
  ;; [$B!;(B-$B6e(B] $B$H$$$&@55,I=8=$,;H$($J$$$N$G!"@8$N$^$^$D$C$3$s$G$*$/!#(B
  (skk-num-rawnum-exp-1 string "[$B!;0l6e8^;0;M<7FsH,O;(B]+" "#2" 0))

(defun skk-num-rawnum-exp-1 (string key type place)
  (save-match-data
    (while (string-match key string)
      (setq string (concat (substring string 0 (match-beginning place))
			   type
			   (substring string (match-end place)) )))
    string ))

(defun skk-num-flatten-list (list)
  ;; $BM?$($i$l$?%j%9%H$N3FMWAG$+$iAH$_9g$;2DG=$JJ8;zNs$NO"@\$r:n$j!"%j%9%H$GJV(B
  ;; $B$9!#(B
  ;; (("A" "B") "1" ("X" "Y")) -> ("A1X" "A1Y" "B1X" "B1Y")
  (do ((result
        (if (atom (car list)) (list (car list)) (car list))
        (mapcan (function
                 (lambda (a)
                   (mapcar (function (lambda (b) (concat a b)))
                           (if (atom (car tail)) (list (car tail))
                             (car tail) ))))
                result ))
       (tail (cdr list) (cdr tail)) )
      ((null tail) result) ))

(defun skk-num-exp (num type)
  ;; ascii $B?t;z$N(B NUM $B$r(B TYPE $B$K=>$$JQ49$7!"JQ498e$NJ8;zNs$rJV$9!#(B
  ;; TYPE $B$O2<5-$NDL$j!#(B
  ;; 0 -> $BL5JQ49(B
  ;; 1 -> $BA43Q?t;z$XJQ49(B
  ;; 2 -> $B4A?t;z$XJQ49(B ($B0L<h$j$J$7(B)
  ;; 3 -> $B4A?t;z$XJQ49(B ($B0L<h$j$r$9$k(B)
  ;; 4 -> $B$=$N?t;z$=$N$b$N$r%-!<$K$7$F<-=q$r:F8!:w(B
  ;; 5 -> $B4A?t;z(B ($B<j7A$J$I$G;HMQ$9$kJ8;z$r;HMQ(B) $B$XJQ49(B ($B0L<h$j$r$9$k(B)
  ;; 9 -> $B>-4}$G;HMQ$9$k?t;z(B ("$B#3;M(B" $B$J$I(B) $B$KJQ49(B
  (let ((fun (cdr (assq type skk-num-type-alist))))
    (if fun (funcall fun num)) ))

(defun skk-num-jisx0208-latin (num)
  ;; ascii $B?t;z$N(B NUM $B$rA43Q?t;z$NJ8;zNs$KJQ49$7!"JQ498e$NJ8;zNs$rJV$9!#(B
  ;; $BNc$($P(B "45" $B$r(B "$B#4#5(B" $B$KJQ49$9$k!#(B
  (let ((candidate
         (mapconcat (function (lambda (c) (cdr (assq c skk-num-alist-type1))))
                    num "" )))
    (if (not (string= candidate ""))
        candidate )))

(defun skk-num-type2-kanji (num)
  ;; ascii $B?t;z(B NUM $B$r4A?t;z$NJ8;zNs$KJQ49$7!"JQ498e$NJ8;zNs$rJV$9!#(B
  ;; $BNc$($P!"(B"45" $B$r(B "$B;M8^(B" $B$KJQ49$9$k!#(B
  (save-match-data
    (if (not (string-match "\\.[0-9]" num))
        (let ((candidate
               (mapconcat (function (lambda (c)
                                      (cdr (assq c skk-num-alist-type2)) ))
                          num "" )))
          (if (not (string= candidate ""))
              candidate )))))

(defun skk-num-type3-kanji (num)
  ;; ascii $B?t;z(B NUM $B$r4A?t;z$NJ8;zNs$KJQ49$7(B ($B0L<h$j$r$9$k(B)$B!"JQ498e$NJ8;zNs$r(B
  ;; $BJV$9!#Nc$($P(B "1021" $B$r(B "$B@iFs==0l(B" $B$KJQ49$9$k!#(B
  (save-match-data
    (if (not (string-match "\\.[0-9]" num))
	;; $B>.?tE@$r4^$^$J$$?t(B
        (let ((str (skk-num-type3-kanji-1 num)))
          (if (string= "" str) "$B!;(B" str) ))))

(defun skk-num-type3-kanji-1 (num)
  ;; skk-num-type3-kanji $B$N%5%V%k!<%A%s!#(B
  (let ((len (length num))
        modulo char prevchar v )
    ;; $B!V@i5~!W$^$G$O=PNO$9$k!#(B
    (when (> len 20) (skk-error "$B0L$,Bg$-$9$.$^$9!*(B" "Too big number!"))
    (setq num (append num nil))
    (while (setq char (car num))
      ;; $B0L(B:     $B0l(B    $B==(B     $BI4(B    $B@i(B    $BK|(B   $B==K|(B   $BI4K|(B    $B@iK|(B     $B2/(B
      ;; modulo: 1 --> 2 --> 3 --> 0 -> 1 --> 2 ---> 3 ---> 0 ---> 1
      ;; len:    1     2     3     4    5     6      7      8      9
      (setq modulo (mod len 4))
      (if (= len 1)
	  ;; $B0l$N0L$G(B 0 $B$G$J$$?t!#(B
	  (unless (eq char ?0)
	    ;; $B0L$rI=$o$94A?t;z0J30$N4A?t;z!#(B
	    (setq v (concat v (cdr (assq char skk-num-alist-type2)))) )
	;; $B0L$rI=$o$94A?t;z0J30$N4A?t;z!#(B
	(when (or
	       ;; $B==$N0L0J>e$G!"$+$D(B 0, 1 $B0J30$N?t;z!#(B
	       (null (memq char '(?0 ?1)))
	       ;; $B==$N0L0J>e$N(B 1 $B$G!"$3$N0L$,!"0L$rI=$o$94A?t;z$K(B "$B0l(B" $B$r(B
	       ;; $BJ;5-$9$Y$-(B ($BNc$($P!"(B"$B0l2/(B" $B$J$I!#(B"$B2/(B" $B$G$O$*$+$7$$(B) $B$H$-!#(B
	       (and (eq char ?1) (= modulo 1)) )
	  (setq v (concat v (cdr (assq char skk-num-alist-type2)))) )
	;; $B0L$rI=$o$94A?t;z!#(B
	(if (and (eq char ?0) (not (= modulo 1)))
	    nil
	  (when (memq modulo '(2 3 0))
	    (setq v (concat v (cdr (assq modulo '((2 . "$B==(B") (3 . "$BI4(B") (0 . "$B@i(B")))))) )
	  ;; $B!V==K|!W0J>e$N0L$G$=$N8e$b(B 0 $B$,B3$/$H$-!#(B
	  (when (> len 5)
	    (cond ((and (= modulo 2) (eq (nth 1 num) ?0))
		   (setq num (cdr num) len (1- len) char (nth 1 num)) )
		  ((and (= modulo 3) (eq (nth 1 num) ?0) (eq (nth 2 num) ?0))
		   (setq num (nthcdr 2 num) len (- len 2) char (nth 2 num)) )
		  ((and (= modulo 0) (eq (nth 1 num) ?0) (eq (nth 2 num) ?0)
			(eq (nth 3 num) ?0) )
		   (setq num (nthcdr 3 num) len (- len 3) char (nth 3 num)) )))
	  (when (and (memq len '(5 9 13 17)) (not (eq prevchar ?0)))
	    (setq v (concat
		     v
		     (cdr (assq len '((5 . "$BK|(B") (9 . "$B2/(B") (13 . "$BC{(B") (17 . "$B5~(B")))) )))))
      (setq len (1- len) prevchar char num (cdr num)) )
    v ))

(defun skk-num-type5-kanji (num)
  ;; ascii $B?t;z(B NUM $B$r4A?t;z$NJ8;zNs$KJQ49$7(B ($B0L<h$j$r$9$k(B)$B!"JQ498e$NJ8;zNs$r(B
  ;; $BJV$9!#Nc$($P(B "1021" $B$r(B "$B0motFu=&0m(B" $B$KJQ49$9$k!#(B
  (save-match-data
    (if (not (string-match "\\.[0-9]" num))
	;; $B>.?tE@$r4^$^$J$$?t(B
        (let ((str (skk-num-type5-kanji-subr num)))
          (if (string= "" str) "$BNm(B" str) ))))

(defun skk-num-type5-kanji-1 (num)
  ;; skk-num-type5-kanji $B$N%5%V%k!<%A%s!#(B
  (let ((len (length num))
        modulo char prevchar v )
    ;; $B!V@i5~!W$^$G$O=PNO$9$k!#(B
    (when (> len 20) (skk-error "$B0L$,Bg$-$9$.$^$9!*(B" "Too big number!"))
    (setq num (append num nil))
    (while (setq char (car num))
      (setq modulo (mod len 4))
      (if (= len 1)
	  (unless (eq char ?0)
	    (setq v (concat v (cdr (assq char skk-num-alist-type5)))) )
	;; $B0L$rI=$o$94A?t;z0J30$N4A?t;z!#(B
	(setq v (concat v (cdr (assq char skk-num-alist-type5))))
	;; $B0L$rI=$o$94A?t;z!#(B
	(if (and (eq char ?0) (not (= modulo 1)))
	    nil
	  (when (memq modulo '(2 3 0))
	    (setq v (concat v (cdr (assq modulo '((2 . "$B=&(B") (3 . "$BI4(B") (0 . "$Bot(B")))))) )
	  ;; $B!V==K|!W0J>e$N0L$G$=$N8e$b(B 0 $B$,B3$/$H$-!#(B
	  (when (> len 5)
	    (cond ((and (= modulo 2) (eq (nth 1 num) ?0))
		   (setq num (cdr num) len (1- len) char (nth 1 num)) )
		  ((and (= modulo 3) (eq (nth 1 num) ?0) (eq (nth 2 num) ?0))
		   (setq num (nthcdr 2 num) len (- len 2) char (nth 2 num)) )
		  ((and (= modulo 0) (eq (nth 1 num) ?0) (eq (nth 2 num) ?0)
			(eq (nth 3 num) ?0) )
		   (setq num (nthcdr 3 num) len (- len 3) char (nth 3 num)) )))
	  (when (and (memq len '(5 9 13 17)) (not (eq prevchar ?0)))
	    (setq v (concat
		     v
		     (cdr (assq len '((5 . "$Bh_(B") (9 . "$B2/(B") (13 . "$BC{(B") (17 . "$B5~(B")))) )))))
      (setq len (1- len) prevchar char num (cdr num)) )
    v ))

(defun skk-num-shogi (num)
  ;; ascii $B?t;z$N(B NUM $B$r>-4}$G;HMQ$5$l$k?t;zI=5-$KJQ49$9$k!#(B
  ;; $BNc$($P(B "34" $B$r(B "$B#3;M(B" $B$KJQ49$9$k!#(B
  (save-match-data
    (if (and (= (length num) 2)
             (not (string-match "\\.[0-9]" num)) )
        (let ((candidate
               (concat (cdr (assq (aref num 0) skk-num-alist-type1))
                       (cdr (assq (aref num 1) skk-num-alist-type2)) )))
          (if (not (string= candidate ""))
              candidate )))))

(defun skk-num-recompute (num)
  ;; #4 $B$N8+=P$7$KBP$7!"(Bskk-henkan-key $B$KBeF~$5$l$??t;z$=$N$b$N$r:FEY8!:w$9$k!#(B
  (let (result)
    ;; with-temp-buffer $B$@$H2?8N>e<j$/$f$+$J$$(B...$B!)(B $B3NDj$5$l$F$7$^$&!#(B
    ;;(with-temp-buffer
    (save-excursion
      (set-buffer (get-buffer-create " *skk-work*"))
      ;; $B%+%l%s%H%P%C%U%!$N%P%C%U%!%m!<%+%kJQ?t$K1F6A$r5Z$\$5$J$$$h$&!"%o!<%-(B
      ;; $B%s%0%P%C%U%!$X0lC6F($2$k(B
      (let ((skk-current-search-prog-list skk-search-prog-list)
            (skk-henkan-key num)
	    ;; $B%+%l%s%H$NJQ49$OAw$j$J$7(B (skk-henkan-okurigana $B$H(B skk-okuri-char $B$O(B
	    ;; $B$$$:$l$b(B nil) $B$@$,!"JL%P%C%U%!(B (work $B%P%C%U%!(B) $B$KF~$C$F$$$k$N$G!"G0(B
	    ;; $B$N$?$a!"(Bnil $B$rF~$l$F$*$/!#(B
            skk-henkan-okurigana skk-okuri-char skk-use-numeric-conversion )
        (while skk-current-search-prog-list
          (setq result (skk-nunion result (skk-search))) )))
    ;; $B$3$3$G(B *skk-work* $B$r=P$FJQ49$r9T$J$C$F$$$k%+%l%s%H%P%C%U%!$KLa$k(B
    ;; ($B%P%C%U%!%m!<%+%kCM$G$"$k(B skk-henkan-list $B$rA`:n$7$?$$$?$a(B)$B!#(B
    (setq skk-num-recompute-key num)
    (if result
        (if (null (cdr result));;(= (length result) 1)
            (car result)
          result )
      ;; $BJQ49$G$-$J$+$C$?$i85$N?t;z$r$=$N$^$^JV$7$F$*$/!#(B
      num )))

;;;###autoload
(defun skk-num-uniq ()
  (if (or (not skk-num-uniq) (null skk-henkan-list))
      nil
    (save-match-data
      (let ((n1 -1) n2 e1 e2 e3
            ;; 1 $B$D$G$b(B 2 $B7e0J>e$N?t;z$,$"$l$P!"(B#2 $B$H(B #3 $B$G$O(B uniq $B$7$J$$!#(B
            (type2and3 (> 2 (apply 'max (mapcar 'length skk-num-list))))
            type2 type3 index2 index3 head2 head3 tail2 tail3
            case-fold-search )
        (while (setq n1 (1+ n1) e1 (nth n1 skk-henkan-list))
          ;; cons cell $B$G$J$1$l$P(B skk-nunion $B$G=hM}:Q$_$J$N$G!"=EJ#$O$J$$!#(B
          (if (consp e1)
              ;; (car e1) $B$H(B equal $B$N$b$N$,>C$($k$N$@$+$i(B e1 $B<+?H$,>C$($k$3(B
              ;; $B$H$O$J$$!#(B
              (setq skk-henkan-list (delete (car e1) skk-henkan-list)
                    skk-henkan-list (delete (cdr e1) skk-henkan-list) ))
          (if (not (and skk-num-recompute-key (consp e1)))
              nil
            ;; ("#4" . "xxx") $B$r4^$`8uJd$,(B skk-henkan-list $B$NCf$K$"$k!#(B
            (setq n2 -1)
            (while (setq n2 (1+ n2) e2 (nth n2 skk-henkan-list))
              (if (and (not (= n1 n2)) (consp e2)
                       ;; $BNc$($P(B ("#4" . "$B0l(B") $B$H(B ("#2" . "$B0l(B") $B$,JBB8$7$F$$(B
                       ;; $B$k>l9g!#(B
                       (string= (cdr e1) (cdr e2)) )
                  (setq skk-henkan-list (delq e2 skk-henkan-list)) )))
          (if (not type2and3)
              nil
            ;; 1 $B7e$N?t;z$rJQ49$9$k:]$K!"(Bskk-henkan-list $B$K(B #2 $B%(%s%H%j$H(B #3
            ;; $B%(%s%H%j$,$"$l$P!"(B#2 $B$b$7$/$O(B #3 $B%(%s%H%j$N$&$A!"$h$j8eJ}$K$"$k(B
            ;; $B$b$N$r>C$9!#(B
            (setq e3 (if (consp e1) (car e1) e1))
            ;; e3 $B$O(B "#2" $B$N$h$&$K?tCMJQ49$r<($9J8;zNs$N$_$H$O8B$i$J$$$N$G!"(B
            ;; member $B$O;H$($J$$!#(B
            (cond ((string-match "#2" e3)
                   (setq type2 e1
                         index2 n1
                         head2 (substring e3 0 (match-beginning 0))
                         tail2 (substring e3 (match-end 0)) ))
                  ((string-match "#3" e3)
                   (setq type3 e1
                         index3 n1
                         head3 (substring e3 0 (match-beginning 0))
                         tail3 (substring e3 (match-end 0)) )))))
        (if (and type2and3 type2 type3
                 ;; $B?tCMJQ49$r<($9J8;zNs(B "#[23]" $B$NA08e$NJ8;zNs$bF10l$N$H(B
                 ;; $B$-$N$_(B uniq $B$r9T$J$&!#(B
                 (string= head2 head3) (string= tail2 tail3))
            (if (> index2 index3)
                ;; "#3" $B$NJ}$,A0$K$"$k!#(B
                (setq skk-henkan-list (delq type2 skk-henkan-list))
              ;; $BJQ?t(B type[23] $B$NCM$O!"(Bskk-henkan-list $B$+$iD>@\Cj=P$7$?$b(B
              ;; $B$N$@$+$i(B delete $B$G$J$/!"(Bdelq $B$G==J,!#(B
              (setq skk-henkan-list (delq type3 skk-henkan-list)) ))))))

;;;###autoload
(defun skk-num-process-user-minibuf-input (key)
  (let (numexp orglen val)
    (if (or (and (string-match "#[012349]" key)
                 (setq numexp key) )
            (and (setq numexp (skk-num-rawnum-exp key))
                 (not (string= key numexp)) ))
        (progn
          (setq orglen (length skk-henkan-list)
                ;; skk-henkan-list $B$ND4@0$O!"(Bskk-num-convert $B$NCf$G9T$J$C(B
                ;; $B$F$/$l$k!#(B
                val (skk-num-convert numexp) )
          (if (= (length skk-henkan-list) (1+ orglen))
              ;; #4 $B$GJ#?t$N8uJd$KJQ49$G$-$?>l9g$O3NDj$7$J$$!#(B
              (setq skk-kakutei-flag t) ))
      (setq skk-henkan-list (nconc skk-henkan-list (list key))
            skk-kakutei-flag t
            val key ))
    val ))

;;;###autoload
(defun skk-num-initialize ()
  ;; skk-use-numeric-convert $B4XO"$NJQ?t$r=i4|2=$9$k!#(B
  (setq skk-last-henkan-data
	(put-alist 'num-list skk-num-list skk-last-henkan-data)
	skk-num-list nil
        skk-num-recompute-key nil ))

;;;###autoload
(defun skk-num-henkan-key ()
  ;; type4 $B$N?tCM:FJQ49$,9T$J$o$l$?$H$-$O!"?tCM<+?H$rJV$7!"$=$l0J30$N?tCMJQ49(B
  ;; $B$G$O!"(Bskk-henkan-key $B$rJV$9!#(B
  (or skk-num-recompute-key skk-henkan-key) )

;;;###autoload
(defun skk-num-update-jisyo (noconvword word &optional purge)
  ;; $B?t;z<+?H$r8+=P$78l$H$7$F<-=q$N%"%C%W%G!<%H$r9T$J$&!#(B
  (if (and skk-num-recompute-key
           (save-match-data (string-match "#4" noconvword)) )
      (let ((skk-henkan-key skk-num-recompute-key))
	(message "%S" skk-num-recompute-key)
        (skk-update-jisyo word purge) )))

;;;###autoload
(defun skk-num (str)
  ;; $B?t;z$r(B skk-number-style $B$NCM$K=>$$JQ49$9$k!#(B
  ;; skk-current-date $B$N%5%V%k!<%A%s!#(B
  (mapconcat (function
	      (lambda (c)
		(cond ((or (not skk-number-style)
			   (and (numberp skk-number-style)
				(= skk-number-style 0) ))
		       (char-to-string c) )
		      ((or (eq skk-number-style t)
			   (and (numberp skk-number-style)
				(= skk-number-style 1) ))
		       (cdr (assq c skk-num-alist-type1)) )
		      (t (cdr (assq c skk-num-alist-type2))) )))
	     str "" ))

(run-hooks 'skk-num-load-hook)

(provide 'skk-num)
;;; Local Variables:
;;; End:
;;; skk-num.el ends here
