;; skk-gadget.el -- $B<B9TJQ49$N$?$a$N%W%m%0%i%`(B
;; Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000
;; Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>

;; Author: Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Version: $Id: skk-gadget.el,v 1.6 2000/10/30 22:10:15 minakaji Exp $
;; Keywords: japanese
;; Last Modified: $Date: 2000/10/30 22:10:15 $

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
;;
;; $B%W%m%0%i%`<B9TJQ49$H$O(B
;; ======================
;; $BAw$j2>L>$N$J$$<-=q$NJQ49$N8uJd$K(B Emacs Lisp $B$N%3!<%I$,=q$$$F$"$l$P!"(BSKK
;; $B$O$=$N%3!<%I$r(B Lisp $B$N%W%m%0%i%`$H$7$F<B9T$7!"$=$N7k2L$NJ8;zNs$r2hLL$KA^(B
;; $BF~$9$k!#Nc$($P!"<-=q$K(B
;;
;;         now /(current-time-string)/
;;
;; $B$H$$$&9T$,$"$k$H$-!"(B`/now ' $B$H%?%$%W$9$l$P2hLL$K$O8=:_$N;~9o$,I=<($5$l!"(B
;; `$B"'(BFri Apr 10 11:41:43 1992' $B$N$h$&$K$J$k!#(B
;;
;; $B$3$3$G;H$($k(B Lisp $B$N%3!<%I$O2~9T$r4^$s$G$$$J$$$b$N$K8B$i$l$k!#$^$?$3$N%3!<(B
;; $B%I$O7k2L$H$7$FJ8;zNs$rJV$9$b$N$G$J$1$l$P$J$i$J$$!#(B
;;
;; $B$3$N%U%!%$%k$O<B9TJQ49%W%m%0%i%`$r=8$a$?$b$N$G$"$k!#(B
;;
;; skk-gadget.el $B$N(B `gadget' $B$O!V>e<j$/9)IW$7$?F;6q!W$N0UL#!#!V?'!9Ht$S=P$9(B
;; $B5$$N$-$$$?$*$b$A$cH"!W$H$$$&$h$&$J0UL#$GL>IU$1$i$l$?!#(B
;; $BM>CL$@$,!"(BX Window $B$G;HMQ$5$l$k(B `Widget' $B$H$$$&8@MU$O!"(B`window'+`gadget'
;; $B$+$i:n$i$l$?B$8l$i$7$$!#(B

;;; Code:
(eval-when-compile (require 'skk-macs) (require 'skk-vars) (require 'static))

;; -- programs
;;;###autoload
(defun skk-current-date (&optional and-time)
  ;; $B8=:_$NF|;~$rF|K\8l$GJV$9!#(Bskk-today $B$H(B skk-clock $B$N%5%V%k!<%A%s!#(B
  ;; $B%*%W%7%g%J%k0z?t$N(B AND-TIME $B$r;XDj$9$k$H!";~4V$bJV$9!#(B
  (let* ((str (current-time-string))
         (year (if skk-date-ad
                   (skk-num (substring str 20 24))
                 (let ((y (- (string-to-number (substring str 20 24)) 1988)))
                   (if (= y 1) "$B85(B" (skk-num (number-to-string y))))))
         (month (skk-num (cdr (assoc (substring str 4 7) skk-month-alist))))
         (day (substring str 8 10))
         (day-of-week (cdr (assoc (substring str 0 3) skk-week-alist)))
         hour minute second)
    (if (eq (aref day 0) ?\040) ; SPC
	(setq day (substring day 1)))
    (setq day (skk-num day))
    (concat (if skk-date-ad "" "$BJ?@.(B") year "$BG/(B"
            month "$B7n(B" day "$BF|(B" "\(" day-of-week "\)"
            (if and-time
                (progn
                  (setq hour (skk-num (substring str 11 13))
                        minute (skk-num (substring str 14 16))
                        second (skk-num (substring str 17 19)))
                  (concat " " hour "$B;~(B" minute "$BJ,(B" second "$BIC(B"))))))

;;;###autoload
(defun skk-today (&optional and-time)
  "$B8=:_$NF|;~$rF|K\8lI=5-$GJV$9!#(B
$B%*%W%7%g%J%k0z?t$N(B AND-TIME $B$r;XDj$9$k$H!"F|IU$K;~4V$r2C$($k!#(B
skk-date-ad $B$H(B skk-number-style $B$K$h$C$FI=<(J}K!$N%+%9%?%^%$%:$,2DG=!#(B"
  (interactive "*P")
  (if (interactive-p)
      (insert (skk-today and-time))
    (skk-current-date and-time)))

;;;###autoload
(defun skk-clock (&optional kakutei-when-quit time-signal)
  "$B%G%8%?%k;~7W$r%_%K%P%C%U%!$KI=<($9$k!#(B
quit $B$9$k$H$=$N;~E@$NF|;~$r8uJd$H$7$FA^F~$9$k!#(B
quit $B$7$?$H$-$K5/F0$7$F$+$i$N7P2a;~4V$r%_%K%P%C%U%!$KI=<($9$k!#(B
interactive $B$K5/F0$9$kB>!"(B\"clock /(skk-clock)/\" $B$J$I$N%(%s%H%j$r(B SKK $B$N<-=q(B
$B$K2C$(!"(B\"/clock\"+ SPC $B$GJQ49$9$k$3$H$K$h$C$F$b5/F02D!#(BC-g $B$G;_$^$k!#(B
$B<B9TJQ49$G5/F0$7$?>l9g$O!"(BC-g $B$7$?;~E@$N;~E@$NF|;~$rA^F~$9$k!#(B
$B%*%W%7%g%J%k0z?t$N(B KAKUTEI-WHEN-QUIT $B$,(B non-nil $B$G$"$l$P(B C-g $B$7$?$H$-$K3N(B
$BDj$9$k!#(B
$B%*%W%7%g%J%k0z?t$N(B TIME-SIGNAL $B$,(B non-nil $B$G$"$l$P!"(BNTT $B$N;~JsIw$K(B ding $B$9$k!#(B
$B$=$l$>$l!"(B\"clock /(skk-clock nil t)/\" $B$N$h$&$J%(%s%H%j$r<-=q$KA^F~$9$l$PNI$$!#(B
skk-date-ad $B$H(B skk-number-style $B$K$h$C$FI=<(J}K!$N%+%9%?%^%$%:$,2DG=!#(B"
  (interactive "*")
  (let ((start (current-time))
        end mes expr1 expr2 sec snd)
    (cond ((or (not skk-number-style)
               (eq skk-number-style 0))
           (setq expr1 "[789]$BIC(B"
                 expr2 "0$BIC(B"))
          ((or (eq skk-number-style t)
               ;; skk-number-style $B$K(B $B?t;z$H(B t $B0J30$N(B non-nil $BCM$rF~$l$F$$$k>l(B
               ;; $B9g!"(B= $B$r;H$&$H(B Wrong type argument: number-or-marker-p, xxxx
               ;; $B$K$J$C$F$7$^$&!#(B
               (eq skk-number-style 1))
           (setq expr1 "[$B#7#8#9(B]$BIC(B"
                 expr2 "$B#0IC(B"))
          (t
           (setq expr1 "[$B<7H,6e(B]$BIC(B"
                 expr2 "$B!;IC(B")))
    ;;
    (static-when (eq skk-emacs-type 'xemacs)
      ;; XEmacs $B$G(B sound $B$,%m!<%I$5$l$F$$$k$+$I$&$+!#(B
      (when (setq snd (and (boundp 'sound-alist)
			   (eq t (catch 'tag
				   (mapc
				    (function
				     (lambda (list)
				       (and
					(eq 'drum
					    (cadr (memq :sound list)))
					(throw 'tag t))))
				    sound-alist)))))
	;;
	(unless (assq 'clink sound-alist)
	  (load-sound-file "clink" 'clink))))
    ;;
    (save-match-data
      (condition-case nil
          (let (case-fold-search
                inhibit-quit visible-bell
                skk-mode skk-latin-mode skk-j-mode skk-abbrev-mode
		skk-jisx0208-latin-mode)
            (while (not quit-flag)
              (setq mes (skk-current-date t)
		    sec 0)
	      (message "%s    Hit any key to quit" mes)
              (if time-signal
                  (if (string-match expr1 mes)
                      ;; [7890] $B$N$h$&$K@55,I=8=$r;H$o$:!"(B7 $B$@$1$GA4$F$N%^%7%s$,(B
                      ;; $BCe$$$F$f$1$PNI$$$N$@$,(B...$B!#CzEY$3$N4X?t<B9T;~$K(B Garbage
                      ;; collection $B$,8F$P$l$F$bI=<($5$l$k?t;z$,Ht$V>l9g$,$"$k!#(B
		      (static-if (eq skk-emacs-type 'xemacs)
			  ;; $B$$$$2;$,$J$$$J$!(B...
			  (ding nil 'drum)
			(ding))
                    (if (string-match expr2 mes)
                        ;; 0 $B$@$1!V%]!A%s!W$H$$$-$?$$$H$3$m$G$9$,!"%^%7%s$K$h$C(B
                        ;; $B$F:9$,$"$k!#(B
                        ;; 386SX 25Mhz + Mule-2.x $B$@$H!V%T%C!"%T%C!W$H$$$&46$8!#(B
                        ;; $BIU$$$F$f$/$N$,Hs>o$K?I$$!#(B68LC040 33Mhz + NEmacs $B$@$H(B
                        ;; $B!V%T%T%C!W$H$J$j!"2;$N%?%$%_%s%0$ONI$$$N$@$,!"$H$-(B
                        ;; $B$I$-(B 1 $BICJ,$D$$$F$$$1$J$/$J$k!#(BPentium 90Mhz +
                        ;; Mule-2.x$B$@$H!V%T%C!W$H$$$&C12;$K$J$C$F$7$^$&(B... (;_;)$B!#(B
			(static-cond
			 ((eq skk-emacs-type 'xemacs)
			  (if snd
			      ;; $B$A$g$C$H$b$?$D$/(B ?
			      (ding nil 'clink)
			    (ding)
			    (unless (sit-for (setq sec
						   (+ sec
						      (/ (float 1) (float 6))))
					     'nodisplay)
			      (next-command-event)
			      (signal 'quit nil))
			    (ding)))
			 ((featurep 'lisp-float-type)
			  (ding)
			  (unless (sit-for (setq sec
						 (+ sec
						    (/ (float 1) (float 6))))
					   nil
					   'nodisplay)
			    (next-command-event)
			    (signal 'quit nil))
			  (ding))
			 (t
			  ;; Emacs 18
			  (ding)
			  (ding))))))
	      (unless (static-cond
		       ((memq skk-emacs-type '(nemacs mule1 xemacs))
			(sit-for (- 1 sec) 'nodisplay))
		       (t
			(sit-for (- 1 sec) nil 'nodisplay)))
		(next-command-event)
		(signal 'quit nil))))
        (quit
         (prog2
             (setq end (current-time))
             (skk-current-date t)
           (if kakutei-when-quit
               (setq skk-kakutei-flag t))
           (message "$B7P2a;~4V(B: %s $BIC(B" (skk-time-difference start end))))))))

;;;###autoload
(defun skk-ad-to-gengo (&optional fstr lstr)
  ;; $B@>Nq$r859f$KJQ49$9$k!#%*%W%7%g%s0z?t$N(B fstr $B$,;XDj$5$l$F$$$l$P!"G/9f$H(B
  ;; $B?t;z$N4V$K!"(Blstr $B$,;XDj$5$l$F$$$l$P!"?t;z$NKvHx$K!"$=$l$>$l$NJ8;zNs$rO"7k(B
  ;; $B$9$k!#(B
  ;; $B<-=q8+=P$7Nc(B;
  ;; $B$;$$$l$-(B#$B$M$s(B /(skk-ad-to-gengo nil "$BG/(B")/(skk-ad-to-gengo " " " $BG/(B")/
  (let ((ad (string-to-number (car skk-num-list))))
    (concat (cond ((>= 1866 ad)
                   (skk-error "$BJ,$j$^$;$s(B" "Unkown year"))
                  ((>= 1911 ad)
                   (concat "$BL@<#(B" fstr (number-to-string (- ad 1867))))
                  ((>= 1925 ad)
                   (concat "$BBg@5(B" fstr (number-to-string (- ad 1911))))
                  ((>= 1988 ad)
                   (concat "$B><OB(B" fstr (number-to-string (- ad 1925))))
                  (t (concat "$BJ?@.(B" fstr (number-to-string (- ad 1988)))))
            lstr)))

;;;###autoload
(defun skk-gengo-to-ad (&optional string)
  ;; $B859f$r@>Nq$KJQ49$9$k!#%*%W%7%g%s0z?t$N(B string $B$,;XDj$5$l$F$$$l$P!"(B
  ;; $B$=$NJ8;zNs$rKvHx$KO"7k$9$k!#(B
  ;; $B<-=q8+=P$7Nc(B;
  ;; $B$7$g$&$o(B#$B$M$s(B /(skk-gengo-to-ad "$BG/(B")/(skk-gengo-to-ad " $BG/(B")/
  (save-match-data
    (let ((num (car skk-num-list))
          gengo)
      (string-match num skk-henkan-key)
      (setq gengo (substring skk-henkan-key 0 (match-beginning 0))
            num (string-to-number num))
      (concat (number-to-string
               (+ num
                  (cond ((eq num 0)
                         (skk-error "0 $BG/$O$"$jF@$J$$(B"
                                    "Cannot convert 0 year"))
                        ((string= gengo "$B$X$$$;$$(B") 1988)
                        ((string= gengo "$B$7$g$&$o(B")
                         (if (> 64 num)
                             1925
                           (skk-error "$B><OB$O(B 63 $BG/$^$G$G$9(B"
                                      "The last year of Showa is 63")))
                        ((string= gengo "$B$?$$$7$g$&(B")
                         (if (> 15 num)
                             1911
                           (skk-error "$BBg@5$O!"(B14 $BG/$^$G$G$9(B"
                                      "The last year of Taisyo is 14")))
                        ((string= gengo "$B$a$$$8(B")
                         (if (> 45 num)
                             1867
                           (skk-error "$BL@<#$O!"(B44 $BG/$^$G$G$9(B"
                                      "The last year of Meiji is 44")))
                        (t (skk-error "$BH=JLITG=$J859f$G$9!*(B"
                                      "Unknown Gengo!")))))
              string))))

;(defun skk-calc (operator)
;  ;; 2 $B$D$N0z?t$r<h$C$F(B operator $B$N7W;;$r$9$k!#(B
;  ;; $BCm0U(B: '/ $B$O0z?t$H$7$FEO$;$J$$$N$G(B (defalias 'div '/) $B$J$I$H$7!"JL$N7A$G(B
;  ;; skk-calc $B$KEO$9!#(B
;  ;; $B<-=q8+=P$7Nc(B; #*# /(skk-calc '*)/
;  (number-to-string
;   (funcall operator (string-to-number (car skk-num-list))
;            (string-to-number (nth 1 skk-num-list)))))

;;;###autoload
(defun skk-calc (operator)
  ;; 2 $B$D$N0z?t$r<h$C$F(B operator $B$N7W;;$r$9$k!#(B
  ;; $BCm0U(B: '/ $B$O0z?t$H$7$FEO$;$J$$$N$G(B (defalias 'div '/) $B$J$I$H$7!"JL$N7A$G(B
  ;; skk-calc $B$KEO$9!#(B
  ;; $B<-=q8+=P$7Nc(B; #*# /(skk-calc '*)/
  (number-to-string (apply operator (mapcar 'string-to-number skk-num-list))))

;;;###autoload
(defun skk-plus ()
  ;; $B<-=q8+=P$7Nc(B; #+#+# /(skk-plus)/
  (number-to-string
   (apply '+ (mapcar 'string-to-number skk-num-list))))

;;;###autoload
(defun skk-minus ()
  (number-to-string
   (apply '- (mapcar 'string-to-number skk-num-list))))

;;;###autoload
(defun skk-times ()
  (number-to-string
   (apply '* (mapcar 'string-to-number skk-num-list))))

;;;###autoload
(defun skk-ignore-dic-word (&rest no-show-list)
  ;; $B6&MQ<-=q$KEPO?$5$l$F$$$k!"0c$C$F$$$k(B/$B5$$KF~$i$J$$JQ49$r=P$5$J$$$h$&$K$9(B
  ;; $B$k!#(B
  ;; $B<-=q8+=P$7Nc(B;
  ;;   $B$k$9$P$s(B /$BN1<iHV(B/(skk-ignore-dic-word "$BN1<iEE(B")/
  ;;   $B$+$/$F$$(B /(skk-ignore-dic-word "$B3NDj(B")/
  (let (new-word save-okurigana)
    ;; skk-ignore-dic-word $B<+?H$N%(%s%H%j$r>C$9!#>C$9$Y$-8uJd$O(B
    ;; skk-henkan-list $B$+$iD>@\Cj=P$7$F$$$k$N$G(B delete $B$G$O$J$/(B delq $B$G==J,!#(B
    (setq skk-henkan-list (delq (nth skk-henkan-count skk-henkan-list)
                                skk-henkan-list))
    ;; $BA48uJd$r(B skk-henkan-list $B$KF~$l$k!#(B
    (while skk-current-search-prog-list
      (setq skk-henkan-list (skk-nunion skk-henkan-list (skk-search))))
    ;; $BITMW$J8uJd$r<N$F$k!#(B
    (while no-show-list
      (setq skk-henkan-list (delete (car no-show-list) skk-henkan-list)
            no-show-list (cdr no-show-list)))
    ;; $B%+%l%s%H$N8uJd(B (skk-ignore-dic-word $B<+?H$N%(%s%H%j(B) $B$r>C$7$?$N$G!"(B
    ;; skk-henkan-count $B$O<!$N8uJd$r;X$7$F$$$k!#(B
    (setq new-word (or (nth skk-henkan-count skk-henkan-list)
                       (progn (setq save-okurigana skk-okuri-char)
                              (skk-henkan-in-minibuff))))
    ;; $B8uJd$,$J$$$H$-!#(B
    (if (not new-word)
        ;; $B6uJ8;zNs$,EPO?$5$l$?$i<-=qEPO?$NA0$N>uBV$KLa$9!#(B
        ;; (nth -1 '(A B C)) $B$O!"(BA $B$rJV$9$N$G!"(Bn $B$,Ii$N?t$G$J$$$3$H$r%A%'%C%/(B
        ;; $B$7$F$*$/I,MW$,$"$k!#(B
        (if (> skk-henkan-count 0)
            (setq skk-henkan-count (- skk-henkan-count 1)
                  new-word (nth skk-henkan-count skk-henkan-list))
          ;; (1- skk-henkan-count) == -1 $B$K$J$k!#"&%b!<%I$KLa$9!#(B
          (setq new-word (if save-okurigana
                             (substring skk-henkan-key 0
                                        (1- (length skk-henkan-key)))
                             skk-henkan-key)
                skk-henkan-count -1
                ;; $B2<5-$NJQ?t$O!"(Bskk-henkan-in-minibuff $B$NCf$GD4@0$5$l$k!#(B
                ;; skk-henkan-active nil
                ;; skk-okuri-char nil
                ;; skk-henkan-okurigana nil
                 )
          (if skk-use-face
              (setq skk-insert-new-word-function
                    'skk-henkan-face-off-and-remove-itself))))
    new-word))

;;;###autoload
(defun skk-henkan-face-off-and-remove-itself ()
  ;; skk-insert-new-word-function $B$K%;%C%H$9$k$?$a$N4X?t!#%+%l%s%H%P%C%U%!$N(B
  ;; $BJQ49ItJ,$,(B Overlay $B$N(B face $BB0@-$K$h$C$FI=<($,JQ99$5$l$F$$$k$N$rLa$7!"$=$N(B
  ;; $B8e<+J,<+?H$r(B skk-insert-new-word-function $B$+$i<h$j=|$/<+Gz4X?t!#(B
  (skk-henkan-face-off)
  (setq skk-insert-new-word-function nil))

(run-hooks 'skk-gadget-load-hook)

(require 'product)
(product-provide (provide 'skk-gadget) (require 'skk-version))
;;; skk-gadget.el ends here
