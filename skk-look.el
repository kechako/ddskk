;;; skk-look.el --- UNIX look command interface for SKK

;; Copyright (C) 1998, 1999, 2000 NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>

;; Author: NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Version: $Id: skk-look.el,v 1.20 2001/11/18 16:28:11 czkmt Exp $
;; Keywords: japanese, mule, input method
;; Last Modified: $Date: 2001/11/18 16:28:11 $

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

;; <How to work>
;; .skk $B$+(B .emacs $B$G(B `skk-use-look' $B$r(B t $B$K%;%C%H$7$F$3$l$rI>2A$7$F2<$5$$!#$=$N(B
;; $B8e(B skk-mode $B$rN)$A>e$2$k$+!"(BM-x skk-restart $B$9$k$H!"(B $B2<5-$N$h$&$J7]Ev$,2DG=(B
;; $B$K$J$j$^$9!#(B
;;
;; (1)$B1QC18l$rJd40$,$G$-$^$9!#(B
;;
;;    $B"&(Babstr(TAB) ---> $B"&(Babstract
;;
;;    $BDL>o$NJd405!G=F1MM!"(B`.' $B$G<!$NJd408uJd!"(B`,' $B$G$R$H$DA0$NJd408uJd$K(B
;;    $B0\F0$G$-$^$9!#(B
;;
;;    SKK $B7A<0$N1QOB<-=q$,$"$l$P!"$3$3$+$i(B SPC $B$r2!$7$F1QOBJQ49$,$G$-$^$9(B
;;    $B$M!#(B
;;
;; (2)$B1QC18l$r$"$$$^$$$KJQ49$7$F<h$j=P$9$3$H$,$G$-$^$9!#(B
;;
;;    $B"&(Babstr* (SPC) ---> $B"'(Babstract
;;
;;    $B8+=P$78l$K%"%9%?%j%9%/(B (`*') $B$rF~$l$k$N$r$*K:$l$J$/!#(B
;;
;;    $B3NDj$9$k$H!"(B`abstr*' $B$r8+=P$78l!"(B`abstract' $B$r8uJd$H$9$k%(%s%H%j$,8D?M<-(B
;;    $B=q$KDI2C$5$l$^$9!#$3$N$h$&$J%(%s%H%j$rDI2C$7$?$/$J$$>l9g$O!"(B
;;    $B%f!<%6!<JQ?t!"(B`skk-search-excluding-word-pattern-function' $B$rE,@Z$K(B
;;    $B@_Dj$9$k$3$H$G!"$3$l$r<B8=$9$k$3$H$,$G$-$^$9!#>\$7$/$O!"(B
;;    `skk-search-excluding-word-pattern-function' $B$N%I%-%e%a%s%H$r$4Mw2<$5$$!#(B
;;
;; (3)(2)$B$GJQ49$7$?8e!"99$K:F5"E*$J1QOBJQ49$r9T$J$&$3$H$,$G$-$^$9!#(B
;;
;;    $B$^$:!"(B`skk-look-recursive-search' $B$NCM$r(B non-nil $B$K%;%C%H$7$F2<$5(B
;;    $B$$!#(BEmacs/SKK $B$r:F5/F0$9$kI,MW$O$"$j$^$;$s!#(B
;;
;;    $B$9$k$H!"Nc$($P!"(B
;;
;;    $B"&(Babstr* (SPC)
;;
;;      ---> $B"'(Babstract (SPC) -> $B"'%"%V%9%H%i%/%H(B (SPC) -> $B"'Cj>](B (SPC)
;;        -> $B"'(Babstraction (SPC) -> $B"'%"%V%9%H%i%/%7%g%s(B
;;
;;    $B$3$N$h$&$K1QC18l(B + $B$=$N1QC18l$r8+=P$78l$K$7$?8uJd$N!V%;%C%H!W$rJQ49(B
;;    $B7k2L$H$7$F=PNO$9$k$3$H$,$G$-$^$9!#(B
;;
;;    $B$3$N:]!"(B`skk-look-expanded-word-only' $B$NCM$,(B non-nil $B$G$"$l$P!":F5"(B
;;    $B8!:w$K@.8y$7$?1QC18l$N!V%;%C%H!W$@$1$r=PNO$9$k$3$H$,$G$-$^$9(B ($B:F5"(B
;;    $B8!:w$G8!=P$5$l$J$+$C$?1QC18l$OL5;k$7$F=PNO$7$^$;$s(B) $B!#(B
;;
;;    $B$b$A$m$s!"(BSKK $B<-=q$K(B
;;
;;       abstract /$B%"%V%9%H%i%/%H(B/$BCj>](B/
;;       abstraction /$B%"%V%9%H%i%/%7%g%s(B/
;;
;;    $B$H$$$&%(%s%H%j$,$"$k$3$H$rA0Ds$H$7$F$$$^$9!#(Bedict $B$r(B SKK $B<-=q7A<0$K(B
;;    $BJQ49$9$k$HNI$$$G$9$M!#(B
;;
;; $BF0:n3NG'$r9T$J$C$?(B look $B$O!"(BSlackware 3.5 $B$KF~$C$F$$$?!"(Bman page $B$K(B
;; `BSD Experimental June 14, 1993' $B$H5-:\$N$"$k$b$N(B ($B%P!<%8%g%s>pJs$,$J$$(B)
;; $B$K$F9T$J$C$F$$$^$9!#%*%W%7%g%s$N;XDj$J$I$,0[$J$k(B look $B$,$"$l$P!"$40lJs2<$5(B
;; $B$$!#$h$m$7$/$*4j$$$$$?$7$^$9!#(B

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
;;   % skkdic-expr SKK-JISYO.E2J + /usr/local/share/skk/SKK-JISYO.L\
;;     | skkdic-sort > SKK-JISYO.L
;;
;; $B$J$I$H$7$F!"(BSKK-JISYO.L $B$H%^!<%8$7$F;H$&$N$,<j7Z$G$9!#(B

;; <Motivation>
;; $B$3$N%W%m%0%i%`$O!"(BeWnn for Linux/FreeBSD $B$N9-9p$KN`;w$N5!G=>R2p$,$"$C$?$N$r(B
;; $B8+$F!"!V$3$s$J5!G=$J$i(B SKK $B>e$K$9$0%$%s%W%j%a%s%H$G$-$k$5!W$H;W$&$H$?$^$i$/(B
;; $B$J$C$F=q$$$F$7$^$$$^$7$?!#(BeWnn $B$KIi$1$k$J!"(BSKK!
;;
;; $B@N!"(BSeiichi Namba <sn@asahi-net.email.ne.jp> $B$5$s$H0l=o$K(B Emacs Lisp $B$G(B
;; look interface $B$r=q$$$?$3$H$,$"$k$N$G$9$,!":#2s$O$=$N:]$N7P83$r@8$+$9$3$H$,(B
;; $B$G$-$^$7$?!#FqGH$5$s$K46<U$$$?$7$^$9!#(B

;;; Code:

(eval-when-compile
  (require 'skk-macs)
  (require 'skk-vars)
  ;; shut up compiler warnings.
  (defvar ispell-process)
  (defvar ispell-filter)
  (defvar ispell-filter))

(eval-and-compile
  (autoload 'ispell-accept-buffer-local-defs "ispell")
  (autoload 'ispell-parse-output "ispell"))

(when (and skk-look-command
	   (null (member '(skk-look)
			 (default-value 'skk-search-prog-list))))
  (let ((pl (default-value 'skk-search-prog-list))
	(n 0)
	dic mark)
    (while pl
      (setq dic (car pl))
      (if (memq (nth 1 dic) '(skk-jisyo skk-rdbms-private-jisyo-table))
	  (setq mark n
		pl nil)
	(setq pl (cdr pl)
	      n (1+ n))))
    (cond
     (mark
      (skk-splice-in (default-value 'skk-search-prog-list)
		     (1+ mark)
		     '((skk-look))))
     (t
      (setq-default skk-search-prog-list
		    (skk-nunion
		     (default-value 'skk-search-prog-list)
		     '((skk-look))))))))

;; program
;;;###autoload
(defun skk-look ()
  ;; UNIX look $B%3%^%s%I$rMxMQ$7$?JQ49$r9T$J$&!#(B
  ;; SKK abbrev $B%b!<%I$K$F!"1QJ8;z(B + $B%"%9%?%j%9%/$G(B uncompleted spelling $B$r;XDj(B
  ;; $B$9$k!#(B
  (when (and skk-use-look
	     skk-abbrev-mode
	     (eq (skk-str-ref skk-henkan-key (1- (length skk-henkan-key)))
		 ?*))
    (let* ((args (substring skk-henkan-key 0 (1- (length skk-henkan-key))))
	   (v (if skk-look-use-ispell
		  (skk-look-ispell args)
		(skk-look-1 args)))
	   skk-henkan-key
	   skk-use-look
	   v2 v3)
      (cond
       ((not skk-look-recursive-search)
	v)
       (t
	(dolist (key v)
	  (let ((skk-current-search-prog-list
		 (copy-sequence skk-search-prog-list)))
	    (setq skk-henkan-key key)
	    (while skk-current-search-prog-list
	      (setq v3 (let (skk-use-numeric-conversion)
			 (skk-search))
		    v2 (if (or (not skk-look-expanded-word-only)
			       v3)
			   (skk-nunion v2 (cons key v3))
			 v2)))))
	v2)))))

(defun skk-look-1 (args)
  ;; core search engine
  (with-temp-buffer
    (let ((word args)
	  opt)
      (setq args (list args))
      (when skk-look-dictionary
	(nconc args (list skk-look-dictionary)))
      (when skk-look-dictionary-order
	(setq opt "d"))
      (when skk-look-ignore-case
	(setq opt (concat "f" opt)))
      (when skk-look-use-alternate-dictionary
	(setq opt (concat "a" opt)))
      (when opt
	(setq args (cons (concat "-" opt) args)))
      (when skk-look-termination-character
	(setq args
	      (cons (list "-t" skk-look-termination-character) args)))
      (when (and (= 0 (apply 'call-process skk-look-command nil t nil args))
		 (> (buffer-size) 0))
	(delete word (split-string (buffer-substring-no-properties
				    (point-min) (1- (point-max)))
				   "\n"))))))

;;;###autoload
(defun skk-look-completion ()
  (unless skk-look-completion-words
    (let ((stacked skk-comp-stack)) ; $BB>$N5!G=$K$h$kJd408uJd!#(B
      ;; look $B$OJ#?t$N8uJd$rEG$/$N$G!"0lC6Cy$a$F$*$$$F!"(B
      ;; $B0l$D$:$D(B complete $B$9$k!#(B
      (setq skk-look-completion-words
	    (if skk-look-use-ispell
		(skk-look-ispell skk-comp-key)
	      (skk-look-1 skk-comp-key)))
      (dolist (word stacked)
	(setq skk-look-completion-words
	      (delete word skk-look-completion-words)))
      ;;skk-look-completion-words $B$N3FMWAG$O!"<B:]$KJd40$r9T$J$C$?CJ3,$G(B
      ;; `skk-completion' $B$K$h$j(B skk-comp-stack $B$KF~$l$i$l$k!#(B
      ))
  (pop skk-look-completion-words))

(defadvice skk-kakutei-initialize (after skk-look-ad activate)
  (setq skk-look-completion-words nil))


;;;###autoload
(defun skk-look-ispell (word)
  (require 'ispell)
  (ispell-accept-buffer-local-defs)
  (message "")
  (process-send-string ispell-process "%\n") ;put in verbose mode
  (process-send-string ispell-process (concat "^" word "\n"))
  (while (progn
	   (accept-process-output ispell-process)
	   (not (string= "" (car ispell-filter)))))
  (setq ispell-filter (cdr ispell-filter)) ; remove extra \n
  (let ((poss (when (and ispell-filter
			 (listp ispell-filter))
		;; 1: t for an exact match.
		;; 2: A string containing the root word matched via suffix
		;;    removal.
		;; 3: A list of possible correct spellings of the format:
		;;    (ORIGINAL-WORD OFFSET MISS-LIST GUESS-LIST)
		;;    ORIGINAL-WORD is a string of the possibly misspelled
		;;    word.
		;;    OFFSET is an integer giving the line offset of the word.
		;;    MISS-LIST and GUESS-LIST are possibly null lists of
		;;    guesses and misses.
		;; 4: Nil when an error has occurred."
		(or (ispell-parse-output (car ispell-filter))
		    'error)))
	ret var)
    (setq ispell-filter nil)
    (cond
     ((eq poss 'error)
      (skk-message "ispell process $B$G%(%i!<$,H/@8$7$^$7$?(B"
		   "error in ispell process")
      (sit-for 1)
      (message "")
      nil)
     ((or (eq poss t)
	  ;; root word $B$KBP$7$F(B skk-look-1 $B$+$1$A$c$*$&$+!)(B
	  ;; $B$G$b$A$C$H$bJd40$B$c$J$/$J$C$A$^$$$^$9$M(B... (^^;;$B!#(B
	  (stringp poss)
	  (null (or (nth 2 poss) (nth 3 poss))))
      (skk-look-1 word))
     (t
      (setq var (nconc (nth 2 poss) (nth 3 poss)))
      (dolist (key var)
	;; call look command by each candidate put out by ispell.
	(setq ret (skk-nunion ret (cons key (skk-look-1 key)))))
      (delete word (skk-nunion (skk-look-1 word) ret))))))

(require 'product)
(product-provide
    (provide 'skk-look)
  (require 'skk-version))

;;; skk-look.el ends here
