;;; skk-dcomp.el --- SKK dynamic completion
;; Copyright (C) 1999, 2000 Mikio Nakajima <minakaji@osaka.email.ne.jp>

;; Author: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Version: $Id: skk-dcomp.el,v 1.3 2001/01/25 10:16:46 minakaji Exp $
;; Keywords: japanese
;; Last Modified: $Date: 2001/01/25 10:16:46 $

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
;; along with Daredevil SKK, see the file COPYING.  If not, write to the
;; Free Software Foundation Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary
;;
;; $B$3$l$O"&%b!<%I$K$*$1$k8+=P$78l$NF~NO$r!"<+F0E*$K%@%$%J%_%C%/$K%3%s%W(B
;; $B%j!<%7%g%s$9$k%W%m%0%i%`$G$9!#(B
;;
;; MS Excel $B$N%;%kF~NO$N<+F0Jd40(B ($BF1$8Ns$K4{$KF~NO$7$F$$$kJ8;zNs$,$"$C(B
;; $B$?$H$-$K$=$l$r;2>H$7$FJd40$7$h$&$H$9$k5!G=(B) $B$r8+$F$$$F!"$3$lJXMx$@$J$!(B
;; $B$H;W$C$?$N$,!"3+H/$N$-$C$+$1$G$9!#(B
;;
;; $B$=$N8e!"A}0f=SG7(B $B$5$s$,3+H/$7$F$$$k(B POBox $B$r8+$F!"(BMS Excel $B$r8+$?:]$K(B
;; $B;W$C$?$3$H$r;W$$=P$7!"(BSKK $B$N(B skk-comp.el $B$GDs6!$5$l$F$$$k%3%s%W%j!<%7(B
;; $B%g%s$N5!G=$r<+F0E*$KDs6!$9$kJ}8~$G<BAu$7$F$_$?$N$,(B skk-dcomp.el $B$N%3!<(B
;; $B%G%#%s%0;O$^$j$G$9!#(B
;;
;; POBox $B$OBt;38uJd$r=P$7$^$9$,!">/$7F0:n$,CY$$$N$,FqE@$G$9!#(Bskk-dcomp.el
;; $B$O0l$D$7$+8uJd$r=P$7$^$;$s$,!"%f!<%6$N8+=P$78l$NF~NO$KDI=>$7%@%$%J%_%C(B
;; $B%/$K%3%s%W%j!<%7%g%s$9$k5!G=$O(B POBox $BF1MM;}$C$F$$$^$9$7!"$^$?F0:n$O$+$J(B
;; $B$j9bB.$G!"(Bskk-dcomp.el $B$r;H$&$3$H$K$h$k%*!<%P!<%X%C%I$rBN46$9$k$3$H$O$J(B
;; $B$$$H;W$$$^$9!#(B
;;
;;
;; <INSTALL>
;;
;; skk-11/experimental/skk-dcomp.el $B$r(B skk-11/skk-dcomp.el $B$K%3%T!<$7(B
;; $B$F8e$OIaDL$K(B make $B$7$F2<$5$$!#(Bskk-dcomp.el $B$,%$%s%9%H!<%k$5$l!"(B
;; autoload $B$N@_Dj$,<+F0E*$K@8@.$5$l$^$9!#8e$O(B .emacs $B$b$7$/$O(B .skk $B$K(B
;; (require 'skk-dcomp) $B$H=q$-$^$7$g$&!#(B
;; $B0lC6(B (require 'skk-dcomp) $B$7$?8e$K!"%@%$%J%_%C%/%3%s%W%j!<%7%g%s$N(B
;; $B5!G=$r;_$a$?$+$C$?$i!"(B(setq skk-dcomp-activate nil) $B$rI>2A$7$^$7$g$&!#(B
;;
;;
;; <HOW TO WORK>
;;
;; $B"&%b!<%I$KF~$j8+=P$78l$rF~NO$9$k$H!"8D?M<-=q$r<+F0E*$K8!:w$7!"8+=P(B
;; $B$78l$r(B $B%3%s%W%j!<%7%g%s$7$^$9!#2<5-$N$h$&$KF0:n$7$^$9(B ($B%+%C%3Fb$O%-!<(B
;; $BF~NO$r!"(B-!- $B$O%]%$%s%H0LCV$rI=$7$^$9(B)$B!#(B
;; 
;;   (Ho) $B"&$[(B -> $B"&$[(B-!-$B$s$H$&(B
;;
;;   * SKK $B$N%3%s%W%j!<%7%g%s$O!"85Mh8D?M<-=q$N$_$r;2>H$7$F9T$J$o$l$k(B
;;     $B;EMM$K$J$C$F$$$^$9$N$G!"8D?M<-=q$K$J$$8+=P$78l$N%3%s%W%j!<%7%g%s(B
;;     $B$O9T$J$o$l$^$;$s!#(B
;;   * $B%3%s%W%j!<%7%g%s$O!"Aw$j$J$7JQ49$N>l9g$7$+9T$J$o$l$^$;$s!#(B
;;   * Ho $B$NF~NO$KBP$7!"!V$[$s$H$&!W$,%3%s%W%j!<%7%g%s$5$l$k$+$I$&$+$O8D(B
;;     $B?M<-=q$N%(%s%H%j$N=gHV<!Bh(B ($BJQ49=g$K9_=g$KJB$s$G$$$k(B) $B$G$9$N$G!"?M(B
;;     $B$=$l$>$l0c$&$O$:$G$9!#(B
;; 
;; $B<+F0E*$K%3%s%W%j!<%7%g%s$5$l$?8+=P$78l$,!"<+J,$N0U?^$7$?$b$N$G$"$l$P(B TAB
;; $B$r2!$9$3$H$G%]%$%s%H0LCV$rF0$+$7!"%3%s%W%j!<%7%g%s$5$l$?8+=P$78l$rA*Br$9(B
;; $B$k$3$H$,$G$-$^$9!#$=$N$^$^(B SPC $B$r2!$7$FJQ49$9$k$J$j!"(Bq $B$r2!$7$F%+%?%+%J(B
;; $B$K$9$k$J$j(B SKK $BK\Mh$NF0:n$r2?$G$b9T$J$&$3$H$,$G$-$^$9!#(B
;;
;;   (Ho) $B"&$[(B -> $B"&$[(B-!-$B$s$H$&(B (TAB) -> $B"&$[$s$H$&(B-!- (TAB) 
;;
;; $B%3%s%W%j!<%7%g%s$5$l$?8+=P$78l$,<+J,$N0U?^$7$?$b$N$G$J$$>l9g$O!"$+$^(B
;; $B$o$:<!$NF~NO$r$7$F2<$5$$!#%3%s%W%j!<%7%g%s$5$l$?ItJ,$rL5;k$7$?$+$N$h$&$K(B
;; $BF0:n$7$^$9!#(B
;; 
;;   (Ho) $B"&$[(B -> $B"&$[(B-!-$B$s$H$&(B (ka) -> $B"&$[$+(B-!-$B$s(B 
;; 
;; $B%3%s%W%j!<%7%g%s$5$l$J$$>uBV$,<+J,$N0U?^$7$?$b$N$G$"$k>l9g$b!"%3%s%W%j!<(B
;; $B%7%g%s$5$l$?ItJ,$rC1$KL5;k$9$k$@$1$G(B OK $B$G$9!#(B
;; 
;;   (Ho) $B"&$[(B -> $B"&$[(B-!-$B$s$H$&(B (C-j) -> $B$[(B
;;   (Ho) $B"&$[(B -> $B"&$[(B-!-$B$s$H$&(B (SPC) -> $B"'J](B ($B!V$[!W$r8+=P$78l$H$7$?JQ49$,9T$J$o$l$k(B)
;;   (Ho) $B"&$[(B -> $B"&$[(B-!-$B$s$H$&(B (q) -> $B%[(B
;; 
;; $B%3%s%W%j!<%7%g%s$5$l$?>uBV$+$i(B BS $B$r2!$9$H!">C$5$l$?%3%s%W%j!<%7%g%sA0$N(B
;; $B8+=P$78l$+$i:FEY%3%s%W%j!<%7%g%s$r9T$J$$$^$9!#(B
;; 
;;   (Ho) $B"&$[(B -> $B"&$[(B-!-$B$s$H$&(B (ka) -> $B"&$[$+(B-!-$B$s(B (BS) -> $B"&$[(B-!-$B$s$H$&(B 
;; 
;;; Code:
(eval-when-compile (require 'skk))
(require 'skk-comp)

;;; functions.
;; (defsubst skk-extentp (object)
;;   (static-cond
;;    ((eq skk-emacs-type 'xemacs) (extentp object))
;;    (t (overlayp object))))

(defsubst skk-dcomp-face-on (start end)
  (skk-face-on skk-dcomp-extent start end skk-dcomp-face
	       skk-dcomp-face-priority))

(defsubst skk-dcomp-face-off ()
  (skk-detach-extent skk-dcomp-extent))

(defun skk-dcomp-after-delete-backward-char ()
  (if (and skk-henkan-on (not skk-henkan-active)
	   (marker-position skk-dcomp-start-point)
	   (marker-position skk-dcomp-end-point))
      (let (pos)
	(skk-dcomp-face-off)
	(condition-case nil
	    (delete-region skk-dcomp-start-point skk-dcomp-end-point)
	  (error))
	(setq pos (point))
	(condition-case nil
	    (progn
	      (skk-comp-do 'first 'silent)
	      (skk-set-marker skk-dcomp-start-point pos)
	      (skk-set-marker skk-dcomp-end-point (point))
	      (skk-dcomp-face-on skk-dcomp-start-point skk-dcomp-end-point)
	      (goto-char skk-dcomp-start-point))
	  (error
	   (setq skk-comp-stack nil)
	   (message nil))))))

;;; advices.
;; main dynamic completion engine.
(defadvice skk-kana-input (around skk-dcomp-ad activate)
  (if (not skk-dcomp-activate)
      nil
    (if (not skk-henkan-on)
	ad-do-it
      (if (or skk-henkan-active (skk-get-prefix skk-current-rule-tree)
	      (not skk-comp-stack))
	  (progn
	    (skk-set-marker skk-dcomp-start-point nil)
	    (skk-set-marker skk-dcomp-end-point nil))
	(when (and (marker-position skk-dcomp-start-point)
		   (marker-position skk-dcomp-end-point))
	  (skk-dcomp-face-off)
	  (or (member (this-command-keys) skk-dcomp-keep-completion-keys)
	      (condition-case nil
		  (delete-region skk-dcomp-start-point skk-dcomp-end-point)
		(error)))))
      ad-do-it
      (if (and (not (skk-get-prefix skk-current-rule-tree))
	       (not skk-okurigana))
	  (let ((pos (point)))
	    (condition-case nil
		(progn
		  (skk-comp-do 'first 'silent)
		  (skk-set-marker skk-dcomp-start-point pos)
		  (skk-set-marker skk-dcomp-end-point (point))
		  (skk-dcomp-face-on skk-dcomp-start-point skk-dcomp-end-point)
		  (goto-char skk-dcomp-start-point))
	      (error
	       (setq skk-comp-stack nil)
	       (message nil))))))))

(defadvice skk-kakutei (around skk-dcomp-ad activate)
  (if (not skk-dcomp-activate)
      nil
    (if (and skk-henkan-on (not skk-henkan-active)
	     (markerp skk-dcomp-start-point)
	     (markerp skk-dcomp-end-point)
	     (marker-position skk-dcomp-start-point)
	     (marker-position skk-dcomp-end-point))
	(progn
	  (skk-dcomp-face-off)
	  (condition-case nil
	      (delete-region skk-dcomp-start-point skk-dcomp-end-point)
	    (error))))
    ad-do-it
    (skk-set-marker skk-dcomp-start-point nil)
    (skk-set-marker skk-dcomp-end-point nil)
    (setq skk-comp-stack nil)))

(defadvice skk-henkan (before skk-dcomp-ad activate)
  (if (not skk-dcomp-activate)
      nil
    (if (and (markerp skk-dcomp-start-point)
	     (markerp skk-dcomp-end-point)
	     (marker-position skk-dcomp-start-point)
	     (marker-position skk-dcomp-end-point))
	(progn
	  (skk-dcomp-face-off)
	  (delete-region skk-dcomp-end-point (point))
	  (skk-set-marker skk-dcomp-end-point (point))))))

(skk-defadvice keyboard-quit (around skk-dcomp-ad activate)
  (if (not skk-dcomp-activate)
      nil
    (if (and skk-henkan-on (not skk-henkan-active)
	     (marker-position skk-dcomp-start-point)
	     (marker-position skk-dcomp-end-point))
	(progn
	  (skk-dcomp-face-off)
	  (condition-case nil
	      (delete-region skk-dcomp-start-point skk-dcomp-end-point)
	    (error))))
    ad-do-it
    (skk-set-marker skk-dcomp-start-point nil)
    (skk-set-marker skk-dcomp-end-point nil)
    (setq skk-comp-stack nil)))

(defadvice skk-comp (around skk-dcomp-ad activate)
  (if (not skk-dcomp-activate)
      nil
    (if (and (marker-position skk-dcomp-start-point)
	     (marker-position skk-dcomp-end-point))
	(progn
	  (goto-char skk-dcomp-end-point)
	  (setq this-command 'skk-comp-do)
	  (skk-dcomp-face-off)
	  (skk-set-marker skk-dcomp-start-point nil)
	  (skk-set-marker skk-dcomp-end-point nil))
      ad-do-it)))

(defadvice skk-delete-backward-char (after skk-dcomp-ad activate)
  (if skk-dcomp-activate
      (skk-dcomp-after-delete-backward-char)))

(defadvice viper-del-backward-char-in-insert (after skk-dcomp-ad activate)
  (if (and skk-mode skk-dcomp-activate)
      (skk-dcomp-after-delete-backward-char)))

(defadvice vip-del-backward-char-in-insert (after skk-dcomp-ad activate)
  (if (and skk-mode skk-dcomp-activate)
      (skk-dcomp-after-delete-backward-char)))

(require 'product)
(product-provide (provide 'skk-dcomp) (require 'skk-version))
;;; Local Variables:
;;; End:
;;; skk-dcomp.el ends here
