;;; skk-lookup.el --- SKK lookup interface
;; Copyright (C) 1999 Mikio Nakajima <minakaji@osaka.email.ne.jp>

;; Author: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Maintainer: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Version: $Id: skk-lookup.el,v 1.5 1999/09/29 12:29:21 minakaji Exp $
;; Keywords: japanese
;; Created: Sep. 23, 1999
;; Last Modified: $Date: 1999/09/29 12:29:21 $

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
;; EPWING $B4dGH9-<-1qBh;MHG$r;HMQ$7$F3+H/$7$?!#(BEntry $B%P%C%U%!$,<!$N$h$&(B
;; $B$J=PNO$K$J$k$3$H$rA0Ds$K3F%f!<%6!<JQ?t$N%G%#%U%)%k%HCM$r7h$a$F$$$k!#(B
;;
;;    $B9-<-1q!!Bh;MHG(B             $B$+$a!ZIS!&a1![(B
;;    $B9-<-1q!!Bh;MHG(B             $B$+$a!Z55![(B
;;    $B9-<-1q!!Bh;MHG(B             $B%+%a(B
;; 
;; $B$D$^$jA0Ds$H$7$F$$$k>r7o$O<!$N(B 2 $BE@!#(B
;;
;;   (1)$B8+=P$78l$KBP$9$k8uJd$O(B`$B!Z(B'$B$H(B`$B![(B'$B$G$/$/$i$l$F$$$k!#(B
;;   (2)$BJ#?t$N8uJd$,$"$C$?$H$-$O!"(B`$B!&(B' $B$GO"@\$5$l$F$$$k!#(B
;;
;;; Code:
(eval-when-compile (require 'skk))
(require 'lookup)

(defgroup skk-lookup nil "SKK lookup related customization."
  :prefix "skk-lookup-"
  :group 'skk )

(defcustom skk-lookup-pickup-pattern "$B!Z(B\\(.+\\)$B![(B$"
  "*$B8uJdCj=P$N$?$a$N(B regexp$B!#(B
\(match-string 1\) $B$G8uJd$,<h$j=P$;$k$h$&$K;XDj$9$k!#(B"
  :type 'regexp
  :group 'skk-lookup )

(defcustom skk-lookup-split-pattern "$B!&(B"
  "*$BJ#?t$N8uJd$,$"$k>l9g$N8uJd$N6h@Z$j$N(B regexp$B!#(B"
  :type 'regexp
  :group 'skk-lookup )

;;;###autoload
(defun skk-lookup-search ()
  (save-excursion
    (save-match-data
      (setq lookup-search-pattern 
	    (if skk-use-numeric-conversion
		(skk-num-compute-henkan-key skk-henkan-key)
	      skk-henkan-key ))
      (let ((module (lookup-default-module))
	    (query (lookup-make-query 'exact skk-henkan-key))
	    entries heading candidates-string candidates-list )
	(lookup-module-setup module)
	(lookup-foreach
	 (lambda (dictionary)
	   (when (and (lookup-dictionary-selected-p dictionary)
		      (memq 'exact (lookup-dictionary-methods dictionary))
		      (setq entries (lookup-vse-search-query dictionary query)) )
	     (lookup-foreach
	      (lambda (entry)
		;; heading $B$7$+<h$j=P$5$J$$$N$O$b$C$?$$$J$$!)(B  $BB>$K$b(B
		;; $B>pJs$r<h$j=P$7$F$*$$$F!"I,MW$K1~$8$F;2>H$9$k$+!)(B
		(setq heading (lookup-entry-heading entry))
		(when (string-match skk-lookup-pickup-pattern heading)
		  (setq candidates-string (match-string 1 heading))
		  (if (not skk-lookup-split-pattern)
		      (setq candidates-list
			    (cons candidates-string
				  (delete candidates-string candidates-list) ))
		    (lookup-foreach
		     (lambda (k)
		       (setq candidates-list
			     (cons k (delete k candidates-list)) ))
		     (split-string candidates-string skk-lookup-split-pattern) ))))
	      entries )))
	 (lookup-module-dictionaries module) )
	candidates-list ))))

(provide 'skk-lookup)
;;; Local Variables:
;;; End:
;;; skk-lookup.el ends here
