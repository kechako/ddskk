;;; skk-lookup.el --- SKK lookup interface
;; Copyright (C) 1999 Mikio Nakajima <minakaji@osaka.email.ne.jp>

;; Author: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Maintainer: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Version: $Id: skk-lookup.el,v 1.8 1999/10/02 09:31:18 minakaji Exp $
;; Keywords: japanese
;; Created: Sep. 23, 1999
;; Last Modified: $Date: 1999/10/02 09:31:18 $

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
;; skk.el $B$K$"$k(B kill-buffer $B$N(B advice $B$r<!$N$b$N$HF~$lBX$(%$%s%9%H!<(B
;; $B%k$7D>$9I,MW$,$"$k!#(B
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
;; $B<!$N$h$&$K(B skk-search-prog-list $B$K2C$($F;XDj$7;HMQ$9$k!#(B
;; skk-seach-server $B$N8!:w$N8e$K;}$C$F$/$k$N$,%;%*%j!<!#(B
;; 
;;  (setq skk-search-prog-list
;;        '((skk-search-jisyo-file skk-jisyo 0 t)
;;          (skk-search-server skk-aux-large-jisyo 10000)
;;          (skk-lookup-search) ))
;;
;;
;;; Code:
(eval-when-compile (require 'skk) (require 'cl))
(require 'lookup)

;;;###autoload
(defgroup skk-lookup nil "SKK lookup related customization."
  :prefix "skk-lookup-"
  :group 'skk )

;;;; user variables.
;; not used yet.
;;;###autoload
(defcustom skk-search-agents lookup-search-agents 
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
  :group 'skk-lookup )

;;;###autoload
(defcustom skk-lookup-option-alist
  '(
    ;; "$B$"$+#3(B $B^@(B", "ethanol"
    ("CHUJITEN" exact "[$B#0(B-$B#9(B]* *\\([^ ]+\\)$" nil nil nil)
    ;; "($BHiIf$J$I$N(B)$B$"$+(B <grime>", "$B!T1Q!U(B ($B%Q%$%W$J$I$N(B)$B$"$+(B <fur>"
    ("COLLOC" exact "\\([^ $B!T!U(B]+\\) <[a-z]+>$" nil nil nil)
    ;; like default but should process gaiji.
    ("IWAKOKU" exact "$B!Z(B\\([^$B!Z![(B]+\\)$B![(B" "$B!&(B" t "_")
    ;; "$B9$(B", "$B@V(B" 
    ("KANWA" exact "^\\(.+\\)$" nil nil nil)
    ;; "$B9$(B"
    ("MYPAEDIA" exact "^\\(.+\\)$" nil nil nil)
    ;; "$B!!$"$+(B <scud$B#2(B>", "$B!!!V$"$+!W(B <rust>"
    ("PLUS" exact "^$B!!(B\\(.+\\) <[a-z$B#0(B-$B#9(B]+>$" nil nil nil)
    )
  "*$B<-=qKh$N8!:w!"J8;z@Z$j=P$7%*%W%7%g%s!#(B
$B%j%9%H$N3FMWAG$O2<5-$NDL$j!#(B

  0th: lookup-dictionary-name $B$,JV$9J8;zNs!"(B 
  1th: search methods $B$r<($9%7%s%\%k!"(B
  2th: $B8uJd$r@Z$j=P$9$?$a$N(B regexp$B!"(B
  3th: $B@Z$j=P$5$l$?J8;zNs$NCf$K99$KJ#?t$N8uJd$r4^$`>l9g$N6h@Z$j$rI=$o$9(B regexp$B!"(B
  4th: $B30;z%G!<%?$r@Z$j<N$F$k$+$I$&$+!"(B
  5th: $B30;z%G!<%?$r<($9(B regexp$B!#(B

$B8=:_BP1~$7$F$$$k<-=qL>$O!"(B\"CHUJITEN\", \"COLLOC\", \"IWAKOKU\", \"KANWA\",
\"MYPAEDIA\", \"PLUS\"."
  :type '(repeat (sexp :tag "Dictionary options alist"))
  :group 'skk-lookup )
    
;;;###autoload
(defcustom skk-lookup-default-option-list
  '(exact "$B!Z(B\\([^$B!Z![(B]+\\)$B![(B" "$B!&(B" nil nil)
  "*$B<-=q$N8!:w!"J8;z@Z$j=P$7%*%W%7%g%s$N%G%#%U%)%k%H!#(B
$B%j%9%H$N3FMWAG$O2<5-$NDL$j!#(B

  0th: search methods $B$r<($9%7%s%\%k!"(B
  1th: $B8uJd$r@Z$j=P$9$?$a$N(B regexp$B!"(B
  2th: $B@Z$j=P$5$l$?J8;zNs$NCf$K99$KJ#?t$N8uJd$r4^$`>l9g$N6h@Z$j$rI=$o$9(B regexp$B!"(B
  3th: $B30;z%G!<%?$r@Z$j<N$F$k$+$I$&$+!"(B
  4th: $B30;z%G!<%?$r<($9(B regexp$B!#(B

$B$3$N%*%W%7%g%s$GBP1~$7$F$$$k<-=qL>$O!"(B\"CHIEZO\", \"KOJIEN\", \"KOUJIEN\",
\"KOKUGO, \"RIKAGAKU\", \"WAEI\".
`lookup-entry-heading' $B$G<h$j=P$7$?J8;zNs$,2<5-$N$h$&$K$J$k$3$H$rA0Ds$K$7$F$$$k!#(B

  \"$B$"!>$+!Z0!2J![!E%/%o(B\"
  \"$B$"$+!Zod2@![(B\"
  \"$B$3!>$7$g$&!Z>.@+!&>.@-![!E%7%d%&(B\""
  :type '(repeat (sexp :tag "Default dictionary options"))
  :group 'skk-lookup )

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

(defsubst skk-lookup-get-process-gaiji-flag (name)
  (let ((list (assoc name skk-lookup-option-alist)))
    (nth 3 (if list (cdr list) skk-lookup-default-option-list)) ))

;;;; funcitions.
;;;###autoload
(defun skk-lookup-search ()
  (save-excursion
    ;; search pattern.
    (setq lookup-search-pattern 
	  (if skk-use-numeric-conversion
	      (skk-num-compute-henkan-key skk-henkan-key)
	    skk-henkan-key ))
    (let ((module (lookup-default-module))
	  name method query entries candidates-string candidates-list )
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
	 (process-gaiji (skk-lookup-get-process-gaiji-flag name))
	 gaji-regexp candidates-string candidates-list )
	((or (string= heading "")
	     (not (string-match pickup-pattern heading)) )
	 candidates-list )
      (setq candidates-string (match-string 1 heading)
	    heading (substring heading (min (+ (match-end 1) skk-kanji-len)
					    (length heading) )))
      (if (not split-pattern)
	  (progn
	    (when process-gaiji 
	      (setq gaji-regexp (skk-lookup-get-gaiji-pattern name))
	      (setq candidates-list (skk-lookup-process-gaiji
				     gaiji-regexp candidates-string )))
	    (if (not (string= lookup-search-pattern candidates-string))
		(setq candidates-string
		      (cons candidates-string
			    (delete candidates-string candidates-list) ))))
	(when process-gaiji 
	  (setq gaji-regexp (skk-lookup-get-gaiji-pattern name)) )
	(setq candidates-string
	      (lookup-foreach
	       (lambda (k)
		 (if process-gaiji
		     (setq k (skk-lookup-process-gaiji gaiji-regexp k)) )
		 (if (not (string= lookup-search-pattern candidates-string))
		     (setq candidates-list (cons k (delete k candidates-list))) ))
	       (split-string candidates-string split-pattern) )))
      candidates-string )))

(defun skk-lookup-process-gaiji (gaiji-regexp tring)
  (save-match-data
    (while (string-match gaiji-regexp  string)
      (setq string (concat (substring string 0 (match-beginning 0))
			   (substring string (min (1+ (match-beginning 0))
						  (length string) )))))
    string ))

;; for creating new regexp.
(defun skk-lookup-pickup-headings (pattern method)
  (let ((module (lookup-default-module))
	var )
    (lookup-module-setup module)
    (lookup-foreach 
     (lambda (dictionary)
       (lookup-foreach 
	(lambda (entry)
	  (setq var (nconc (list (list (lookup-dictionary-name dictionary)
				       (lookup-dictionary-id dictionary)
				       (lookup-entry-heading entry) ))
			   var )))
	(lookup-vse-search-query
	 dictionary (lookup-make-query method pattern) )))
     (lookup-module-dictionaries module) )
    var ))

(provide 'skk-lookup)
;;; Local Variables:
;;; End:
;;; skk-lookup.el ends here
