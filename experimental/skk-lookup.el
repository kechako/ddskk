;;; skk-lookup.el --- SKK lookup gateway
;; Copyright (C) 1999 Mikio Nakajima <minakaji@osaka.email.ne.jp>

;; Author: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Maintainer: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Version: $Id: skk-lookup.el,v 1.13 1999/10/03 11:30:49 minakaji Exp $
;; Keywords: japanese
;; Created: Sep. 23, 1999
;; Last Modified: $Date: 1999/10/03 11:30:49 $

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
;; $B$F8uJd$r=PNO$9$k%W%m%0%i%`$G$9!#EvA3$G$9$,!"(BLookup $B5Z$SBP1~$9$k<-=q(B
;; $B$,%$%s%9%H!<%k$5$l$F$$$J$$$H;H$($^$;$s!#(B
;;
;; skk.el $B$K$"$k(B kill-buffer $B$N(B advice $B$r<!$N$b$N$HF~$lBX$(%$%s%9%H!<(B
;; $B%k$7D>$9I,MW$,$"$j$^$9!#(B
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
;; $B8=:_BP1~$7$F$$$k<-=q$O!"(Bispell, CHIEZO, CHUJITEN, COLLOC, KANWA,
;; KOJIEN, KOKUGO, KOUJIEN, MYPAEDIA, PLUS, RIKAGAKU, WAEI $B$G$9(B
;; (lookup-dictionary-name $B$,JV$9CM$GI85-$7$F$$$^$9(B)$B!#(Bkakasi
;; (KAKASI $B$rMxMQ$9$k$J$i(B skk-kakasi.el $B$r;H$$$^$7$g$&(B), ndcookie,
;; ndnmz $B$K$OBP1~$$$F$$$^$;$s$7!"BP1~$NI,MW$O$J$$$H9M$($F$$$^$9(B ($B%a%j%C(B
;; $B%H$,$"$l$P65$($F2<$5$$(B)$B!#(B
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
;; $BKvHx$J$,$i!"(BLookup $B$r:n$i$l$?(B Keisuke Nishida $B$5$s5Z$S(B Lookup
;; Development Team $B$N3'MM!"3+H/$N=i4|$+$i%G%P%C%0$r<jEA$C$F$$$?$@$$$?!"(B
;; NEMOTO Takashi <tnemoto@mvi.biglobe.ne.jp> $B$5$s$K?<$/46<U$$$?$7$^$9!#(B

;;; Code:
(eval-when-compile (require 'skk) (require 'skk-num) (require 'cl))
(require 'lookup)

;;;###autoload
(defgroup skk-lookup nil "SKK lookup related customization."
  :prefix "skk-lookup-"
  :group 'skk )

;;;; user variables.
(defcustom skk-lookup-search-agents
  (let ((agents (copy-list lookup-search-agents))
	e )
    ;; use `skk-kakasi.el'.
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
  :type '(repeat (sexp :tag "agent"))	; type $B$O$A$g$C$H$d$d$3$7$9$.!&!&(B
  :group 'skk-lookup
  :require 'lookup-vars )

(defcustom skk-lookup-option-alist
  '(
    ;; "[spla -> splat]"
    ("ispell" exact "-> \\([^ ]+\\)]$" nil)
    ;; "$B$"$+#3(B $B^@(B", "ethanol"
    ("CHUJITEN" exact "[$B#0(B-$B#9(B]* *\\([^ ]+\\)$" nil)
    ;; "($BHiIf$J$I$N(B)$B$"$+(B <grime>", "$B!T1Q!U(B ($B%Q%$%W$J$I$N(B)$B$"$+(B <fur>"
    ("COLLOC" exact "\\([^ $B!T!U(B]+\\) <[a-z]+>$" nil)
    ;; "$B9$(B", "$B@V(B" 
    ("KANWA" exact nil nil)
    ;; "$B9$(B"
    ("MYPAEDIA" exact nil nil)
    ;; "$B!!$"$+(B <scud$B#2(B>", "$B!!!V$"$+!W(B <rust>"
    ("PLUS" exact "^$B!!(B\\(.+\\) <[a-z$B#0(B-$B#9(B]+>$" nil)
    )
  "*$B<-=qKh$N8!:w!"J8;z@Z$j=P$7%*%W%7%g%s!#(B
$B%j%9%H$N3FMWAG$O2<5-$NDL$j!#(B

  0th: lookup-dictionary-name $B$,JV$9J8;zNs!#(B 
  1th: search methods $B$r<($9%7%s%\%k!#(B
  2th: $B8uJd$r@Z$j=P$9$?$a$N(B regexp \(\(match-string 1\) $B$G8uJd$r<h$j=P$9$3$H$,(B
       $B$G$-$k$h$&;XDj$9$k(B\)$B!#(B
  3th: $B@Z$j=P$5$l$?J8;zNs$NCf$K99$KJ#?t$N8uJd$r4^$`>l9g$N6h@Z$j$rI=$o$9(B regexp$B!#(B

$B8=:_BP1~$7$F$$$k<-=qL>$O!"(B\"CHUJITEN\", \"COLLOC\", \"KANWA\",
\"MYPAEDIA\", \"PLUS\".

`lookup-entry-heading' $B$,<+J,$N;HMQ$9$k<-=q$+$i$I$N$h$&$JJ8;zNs$r<h$j=P$9$N$+(B
$B3N$+$a$?$$$H$-$O!"(B`skk-lookup-pickup-headings' $B$r;HMQ$9$k!#Nc$($P!"(B

 \(skk-lookup-pickup-headings \"$B$3$7$g$&(B\" 'exact\)"
  :type '(repeat
	  (list (string :tag "Dictionary name")
		(choice :tag "Search method"
			(const exact) (const prefix)
			(const suffix) (const substring)
			(const regexp) (const keyword)
			(const text) )
		(choice :tag "regexp to substring candidate from heading"
			regexp (const nil) )
		(choice :tag "regexp to split candidates"
		       regexp (const nil) )))
  :group 'skk-lookup )
    
(defcustom skk-lookup-default-option-list
  '(exact "$B!Z(B\\([^$B!Z![(B]+\\)$B![(B" "$B!&(B")
  "*$B<-=q$N8!:w!"J8;z@Z$j=P$7%*%W%7%g%s$N%G%#%U%)%k%H!#(B
$B%j%9%H$N3FMWAG$O2<5-$NDL$j!#(B

  0th: search methods $B$r<($9%7%s%\%k!#(B
  1th: $B8uJd$r@Z$j=P$9$?$a$N(B regexp \(\(match-string 1\) $B$G8uJd$r<h$j=P$9$3$H(B
       $B$,$G$-$k$h$&;XDj$9$k(B\)$B!#(B
  2th: $B@Z$j=P$5$l$?J8;zNs$NCf$K99$KJ#?t$N8uJd$r4^$`>l9g$N6h@Z$j$rI=$o$9(B regexp$B!#(B

$B$3$N%*%W%7%g%s$GBP1~$7$F$$$k<-=qL>$O!"(B\"CHIEZO\", \"KOJIEN\", \"KOUJIEN\",
\"KOKUGO, \"RIKAGAKU\", \"WAEI\".
`lookup-entry-heading' $B$G<h$j=P$7$?J8;zNs$,2<5-$N$h$&$K$J$k$3$H$rA0Ds$K$7$F$$$k!#(B

  \"$B$"!>$+!Z0!2J![!E%/%o(B\"
  \"$B$"$+!Zod2@![(B\"
  \"$B$3!>$7$g$&!Z>.@+!&>.@-![!E%7%d%&(B\"

`lookup-entry-heading' $B$,<+J,$N;HMQ$9$k<-=q$+$i$I$N$h$&$JJ8;zNs$r<h$j=P$9$N$+(B
$B3N$+$a$?$$$H$-$O!"(B`skk-lookup-pickup-headings' $B$r;HMQ$9$k!#Nc$($P!"(B

 \(skk-lookup-pickup-headings \"$B$3$7$g$&(B\" 'exact\)"
  :type '(list (choice :tag "Search method"
		       (const exact) (const prefix)
		       (const suffix) (const substring)
		       (const regexp) (const keyword)
		       (const text) )
	       (choice :tag "regexp to substring candidate from heading"
		       regexp (const nil) )
	       (choice :tag "regexp to split candidates"
		       regexp (const nil) ))
  :group 'skk-lookup )

(defcustom skk-lookup-search-modules nil
  "*$B8!:w%b%8%e!<%k$N@_Dj$N%j%9%H!#(B"
  :type '(repeat (cons :tag "Module" (string :tag "name")
		       (repeat :tag "Dictionary" (string :tag "ID"))))
  :group 'skk-lookup )

;;;; internal variables.
(defvar skk-lookup-agent-list nil)
(defvar skk-lookup-default-module nil)
(defvar skk-lookup-module-list nil)

;;;; inline functions. 
(defsubst skk-lookup-get-method (name)
  (let ((list (assoc name skk-lookup-option-alist)))
    (car (if list (cdr list) skk-lookup-default-option-list)) ))

(defsubst skk-lookup-get-pickup-regexp (name)
  (let ((list (assoc name skk-lookup-option-alist)))
    (nth 1 (if list (cdr list) skk-lookup-default-option-list)) ))

(defsubst skk-lookup-get-split-regexp (name)
  (let ((list (assoc name skk-lookup-option-alist)))
    (nth 2 (if list (cdr list) skk-lookup-default-option-list)) ))

;;;; funcitions.
;;;###autoload
(defun skk-lookup-search ()
  (save-excursion
    ;; search pattern.
    (setq lookup-search-pattern 
	  (if skk-use-numeric-conversion
	      (skk-num-compute-henkan-key skk-henkan-key)
	    skk-henkan-key ))
    (let ((module (skk-lookup-default-module))
	  lookup-enable-gaiji ; not to put out gaiji.
	  name method entries candidates-string candidates-list )
      ;; setup modules.
      (lookup-module-setup module)
      (lookup-foreach
       (lambda (dictionary)
	 (when (and (lookup-dictionary-selected-p dictionary)
		    (setq name (lookup-dictionary-name dictionary))
		    (setq method (skk-lookup-get-method name))
		    ;; valid method or not?
		    (memq method (lookup-dictionary-methods dictionary))
		    ;; actual search.
		    (setq entries (lookup-vse-search-query
				   dictionary
				   (lookup-make-query method skk-henkan-key) )))
	   (lookup-foreach
	    (lambda (entry)
	      (setq candidates-string (lookup-entry-heading entry))
	      (if (not (string= lookup-search-pattern candidates-string))
		  (setq candidates-list
			(nconc candidates-list 
			       ;; pickup necessary string for SKK.
			       (skk-lookup-process-heading name candidates-string) ))))
	    entries )))
       ;; dictionaries to be searched.
       (lookup-module-dictionaries module) )
      candidates-list )))

(defun skk-lookup-process-heading (name heading)
  ;; heading $B$7$+<h$j=P$5$J$$$N$O$b$C$?$$$J$$!)(B  $BB>$K$b>pJs$r<h$j=P$7(B
  ;; $B$F$*$$$F!"I,MW$K1~$8$F;2>H$9$k$+!)(B
  (save-match-data
    (do ((pickup-pattern (skk-lookup-get-pickup-regexp name))
	 (split-pattern (skk-lookup-get-split-regexp name))
	 candidates-string candidates-list )
	((or (and (not pickup-pattern) (setq candidates-list (list heading)))
	     (string= heading "")
	     (not (string-match pickup-pattern heading)) )
	 candidates-list )
      (setq candidates-string (match-string 1 heading)
	    heading (substring heading (min (+ (match-end 1) skk-kanji-len)
					    (length heading) )))
      (if (not split-pattern)
	  (progn
	    (if (not (string= lookup-search-pattern candidates-string))
		(setq candidates-list
		      (cons candidates-string
			    (delete candidates-string candidates-list) ))))
	(setq candidates-string
	      (lookup-foreach
	       (lambda (k)
		 (if (not (string= lookup-search-pattern candidates-string))
		     (setq candidates-list (cons k (delete k candidates-list))) ))
	       (split-string candidates-string split-pattern) )))
      candidates-list )))

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

;; to check dictionary output of heading for creating new regexp.
(defun skk-lookup-pickup-headings (pattern method)
  (let ((module (skk-lookup-default-module))
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
