;;; skk-look.el --- UNIX look command interface for SKK
;; Copyright (C) 1998, 1999 Mikio Nakajima <minakaji@osaka.email.ne.jp>

;; Author: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Maintainer: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Version: $Id: skk-look.el,v 1.5.2.4.2.2 1999/12/05 05:59:26 minakaji Exp $
;; Keywords: japanese
;; Last Modified: $Date: 1999/12/05 05:59:26 $

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
;; <How to work>
;; .skk $B$+(B .emacs $B$G(B `skk-use-look' $B$r(B t $B$K%;%C%H$7$F$3$l$rI>2A$7$F2<$5$$!#$=$N(B
;; $B8e(B skk-mode $B$rN)$A>e$2$k$+!"(BM-x skk-restart $B$9$k$H!"(BSKK abbrev $B%b!<%I$G!"(B
;; UNIX look $B%3%^%s%I$rMxMQ$7$?1QJ8;z(B + $B%"%9%?%j%9%/$NJQ49$,$G$-$k$h$&$K$J$j$^(B
;; $B$9!#$3$s$J46$8$G$9!#(B
;;
;; $B"&(Bconfere* (SPC)
;; ---> $B"'(Bconference
;;
;; $B3NDj$9$k$H!"(B`confere*' $B$r8+=P$78l!"(B`conference' $B$r8uJd$H$9$k%(%s%H%j$,8D?M<-(B
;; $B=q$KDI2C$5$l$^$9!#$3$N$h$&$J%(%s%H%j$rDI2C$7$?$/$J$$>l9g$O!"(B
;; skk.el $B$N%f!<%6!<JQ?t!"(B`skk-search-excluding-word-pattern-function' $B$rE,@Z$K(B
;; $B@_Dj$9$k$3$H$G!"$3$l$r<B8=$9$k$3$H$,$G$-$^$9!#>\$7$/$O!"(B
;; `skk-search-excluding-word-pattern-function' $B$N%I%-%e%a%s%H$r$4Mw2<$5$$!#(B
;;
;; `skk-look-recursive-search' $B$NCM$r(B non-nil $B$K$9$k$H!"(Blook $B$,8+$D$1$?1QC18l$r(B
;; $B8+=P$78l$K$7$F!":F5"E*$K(B SKK $B<-=qFb$r8!:w$9$k$3$H$,$G$-$^$9!#Nc$($P!"$$$:$l$+(B
;; $B$N(B SKK $B<-=q$K(B
;;
;;  abstract /$B%"%V%9%H%i%/%H(B/$BCj>](B/
;;  abstraction /$B%"%V%9%H%i%/%7%g%s(B/
;;
;; $B$H$$$&%(%s%H%j$,$"$k>l9g!"(B
;;
;;  $B"&(Babs* (SPC)
;;
;;  ---> $B"'(Babstract (SPC) -> $B"'%"%V%9%H%i%/%H(B (SPC) -> $B"'Cj>](B (SPC)
;;       -> $B"'(Babstraction (SPC) -> $B"'%"%V%9%H%i%/%7%g%s(B
;;
;; $B$N$h$&$K1QC18l(B + $B$=$N1QC18l$r8+=P$78l$K$7$?8uJd$N!V%;%C%H!W$rJQ497k2L$H$7$F=P(B
;; $BNO$9$k$3$H$,$G$-$^$9!#$3$N:]!"(B`skk-look-expanded-word-only' $B$NCM$,(B non-nil $B$G(B
;; $B$"$l$P!":F5"8!:w$K@.8y$7$?1QC18l$N!V%;%C%H!W$@$1(B ($B:F5"8!:w$G8!=P$5$l$J$+$C$?(B
;; $B1QC18l$OL5;k$9$k(B) $B$r=PNO$9$k$3$H$,$G$-$^$9!#(B
;;
;; abbrev $B%b!<%I$GJd40$r9T$J$&$H!"8D?M<-=q$r8!:w$7?T$7$?8e$G!"(Blook $B%3%^%s%I$K$h$k(B
;; $B1QC18lJd40$r9T$J$$$^$9!#Nc$($P!"$3$s$J46$8$KF0:n$7$^$9!#(B
;;
;;  $B"&(Bconfe (TAB)
;;  ---> $B"&(Bconference
;;
;; $BF0:n3NG'$r9T$J$C$?(B look $B$O!"(BSlackware 3.5 $B$KF~$C$F$$$?!"(Bman page $B$K(B
;; `BSD Experimental June 14, 1993' $B$H5-:\$N$"$k$b$N(B ($B%P!<%8%g%s>pJs$,$J$$(B) $B$K$F(B
;; $B9T$J$C$F$$$^$9!#%*%W%7%g%s$N;XDj$J$I$,0[$J$k(B look $B$,$"$l$P!"$40lJs2<$5$$!#$h$m(B
;; $B$7$/$*4j$$$$$?$7$^$9!#(B

;; <Dictionary>
;; ftp://ftp.u-aizu.ac.jp:/pub/SciEng/nihongo/ftp.cc.monash.edu.au/
;; $B$KCV$$$F$"$k(B edict $B$rMxMQ$9$k$H<j7Z$K1QOB<-=q$,$G$-$^$9!#(B
;; 
;;   % jgawk -f skk-10/lisp/look/edict2skk.awk edict > temp
;;   % skkdic-expr temp | skkdic-sort > SKK-JISYO.E2J
;;   % rm temp
;;
;; $B$G$-$?(B SKK-JISYO.E2J $B$NMxMQJ}K!$O?'!9$"$j$^$9$,!"(B
;;
;;   % skkdic-expr SKK-JISYO.E2J + /usr/local/share/skk/SKK-JISYO.L | skkdic-sort > SKK-JISYO.L
;;
;; $B$J$I$H$7$F!"(BSKK-JISYO.L $B$H%^!<%8$7$F;H$&$N$,<j7Z$G$9!#(B

;; <Motivation>
;; $B$3$N%W%m%0%i%`$O!"(BeWnn for Linux/FreeBSD $B$N9-9p$KN`;w$N5!G=>R2p$,$"$C$?$N$r8+(B
;; $B$F!"!V$3$s$J5!G=$J$i(B SKK $B>e$K$9$0%$%s%W%j%a%s%H$G$-$k$5!W$H;W$&$H$?$^$i$/$J$C(B
;; $B$F=q$$$F$7$^$$$^$7$?!#(BeWnn $B$KIi$1$k$J!"(BSKK!
;;
;; $B@N!"(BSeiichi Namba <sn@asahi-net.email.ne.jp> $B$5$s$H0l=o$K(B Emacs Lisp $B$G(B
;; look interface $B$r=q$$$?$3$H$,$"$k$N$G$9$,!":#2s$O$=$N:]$N7P83$r@8$+$9$3$H$,$G$-(B
;; $B$^$7$?!#FqGH$5$s$K46<U$$$?$7$^$9!#(B

;;; Code:
(eval-when-compile (require 'skk-macs) (require 'skk-vars))

(and skk-look-command
     (null (member '(skk-look) skk-search-prog-list))
     (let ((pl skk-search-prog-list)
	   (n 0) dic mark )
       (while pl
	 (setq dic (car pl))
	 (if (memq (nth 1 dic) '(skk-jisyo skk-rdbms-private-jisyo-table))
	     (setq mark n
		   pl nil)
	   (setq pl (cdr pl)
		 n (1+ n) )))
       (skk-splice-in skk-search-prog-list (1+ mark)
		      '((skk-look)) )))

;; program
;;;###autoload
(defun skk-look ()
  ;; UNIX look $B%3%^%s%I$rMxMQ$7$?JQ49$r9T$J$&!#(B
  ;; SKK abbrev $B%b!<%I$K$F!"1QJ8;z(B + $B%"%9%?%j%9%/$G(B uncompleted spelling $B$r;XDj(B
  ;; $B$9$k!#(B
  (and skk-abbrev-mode
       (eq (skk-str-ref skk-henkan-key (1- (length skk-henkan-key))) ?*)
       (let ((args (substring skk-henkan-key 0 (1- (length skk-henkan-key))))
	     v )
	 (setq v (skk-look-1 args))
	 (if (not skk-look-recursive-search)
	     v
	   (let (skk-henkan-key v2 v3)
	     (while v
	       (let ((skk-current-search-prog-list
		      (delete '(skk-look) (copy-sequence skk-search-prog-list)) ))
		 (setq skk-henkan-key (car v))
		 (while skk-current-search-prog-list
		   (setq v3 (skk-search)
			 v2 (if (not skk-look-expanded-word-only)
				(skk-nunion v2 (cons (car v) v3))
			      (if v3
				  (skk-nunion v2 (cons (car v) v3))
				v2 )))))
	       (setq v (cdr v)) )
	     v2 )))))

(defun skk-look-1 (args)
  ;; core search engine
  (with-temp-buffer 
    (let (opt)
      (setq args (list args))
      (and skk-look-dictionary (nconc args (list skk-look-dictionary)))
      (and skk-look-dictionary-order (setq opt "d"))
      (and skk-look-ignore-case (setq opt (concat "f" opt)))
      (and skk-look-use-alternate-dictionary
	   (setq opt (concat "a" opt)) )
      (and opt (setq args (cons (concat "-" opt) args)))
      (and skk-look-termination-character
	   (setq args
		 (cons (list "-t" skk-look-termination-character) args) ))
      (and
       (= 0 (apply 'call-process skk-look-command nil t nil args))
       (> (buffer-size) 0)
       (split-string (buffer-substring-no-properties (point-min) (1- (point-max)))
		     "\n" )))))

;;;###autoload
(defun skk-look-completion ()
  (or skk-look-completion-words
      (let ((stacked skk-completion-stack))
	;; look $B$OJ#?t$N8uJd$rEG$/$N$G!"0lC6Cy$a$F$*$$$F!"0l$D$E$D(B complete $B$9$k!#(B
	(setq skk-look-completion-words
	      (delete skk-completion-word (skk-look-1 skk-completion-word)) )
	(while stacked
	  (setq skk-look-completion-words
		(delete (car stacked) skk-look-completion-words)
		stacked (cdr stacked) ))))
  (prog1
      (car skk-look-completion-words)
    (setq skk-look-completion-words (cdr skk-look-completion-words)) ))

(defadvice skk-kakutei-initialize (after skk-look-ad activate)
  (setq skk-look-completion-words nil) )

(provide 'skk-look)
;;; skk-look.el ends here
