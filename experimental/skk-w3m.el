;;; skk-w3m.el --- SKK w3m gateway
;; Copyright (C) 2001 NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>

;; Author: NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Version: $Id: skk-w3m.el,v 1.1 2001/04/12 14:37:39 minakaji Exp $
;; Keywords: japanese
;; Created: Apr. 12, 2001 (oh, its my brother's birthday!)
;; Last Modified: $Date: 2001/04/12 14:37:39 $

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
;; <HOW TO INSTALL>
;; emacs-w3m (http://www.namazu.org/~tsuchiya/emacs-w3m) $B$,(B load $B$G(B
;; $B$-$k4D6-$,I,?\$G$9!#$3$N%U%!%$%k$r(B SKK-MK $B$,$"$k%G%#%l%/%H%j$K%3(B
;; $B%T!<$7!"8e$OIaDL$K(B make install $B$9$k$@$1$G$9!#(B
;;
;; <HOW TO WORK>
;; skk-search-prog-list $B$K(B (skk-w3m-search "goo-daijirin") $B$N$h$&$J(B
;; $BMWAG$rDI2C$7$^$9!#DL>o!"B>$N$I$N(B skk search engine $B$h$j$b:G$bCY$$(B
;; $B$N$G!":G$b:G8e$,NI$$$G$7$g$&!#$3$s$J46$8$K$J$j$^$9!#(B
;;
;; (setq skk-search-prog-list
;;       '((skk-search-kakutei-jisyo-file skk-kakutei-jisyo 10000 t)
;;         (skk-search-jisyo-file skk-initial-search-jisyo 10000 t)
;;         (skk-search-jisyo-file skk-jisyo 0 t)
;;         (skk-search-server skk-aux-large-jisyo 10000)
;;         (skk-w3m-search "goo-daijirin")))
;;
;; $B0[$J$k8!:w%(%s%8%s$r;XDj$9$k$K$O(B "goo-daijirin" $B$KBe$($FB>$N$b$N(B
;; $B$r;XDj$7$^$9!#C"$7!"(Bskk-w3m-search-engine-alist $B$KBP1~$9$k%(%s%H(B
;; $B%j$,I,MW$G$9!#(Bw3m-search.el $B$NI8=`$N(B w3m-search-engine-alist $B$O(B
;; $B8+$^$;$s$N$GCm0U$,I,MW$G$9!#(B
;; 
;; <TODO>
;; o $B$H$j$"$($:(B skk-w3m-get-candidates-from-goo-exceed-waei,
;;   skk-w3m-get-candidates-from-goo-exceed-eiwa,
;;   skk-w3m-get-candidates-from-goo-daily-shingo $B$r40@.$5$;$k!#(B
;; o $B8!:w%(%s%8%s$NA}2C!#(B
;; o w3m $B$NBe$o$j$K(B wget $B$,;H$($J$$$+$H;n$7$?$,!"5U$KB.EYCY$7(B...$B!#(B
;; o lookup $B$O(B w3m-search.el $B$r;H$C$?(B Web search $B$rE}9g$7$J$$$N$@$m$&(B
;;   $B$+(B...$B!#E}9g$9$l$P(B skk-lookup.el $B$G0l854IM}$G$-$k!)(B
;;
;; (setq skk-search-prog-list '((skk-search-jisyo-file skk-jisyo 0 t)
;;			        (skk-w3m-search "goo-daijirin")))
;;; Code
(eval-when-compile (require 'skk-macs) (require 'skk-vars))
(require 'w3m-search)

(defgroup skk-w3m nil "SKK w3m related customization."
  :prefix "skk-w3m-"
  :group 'skk)

;;; user variables.
(defvar skk-w3m-search-engine-alist
  '(("goo-daijirin"
     "http://dictionary.goo.ne.jp/cgi-bin/dict_search.cgi?MT=%s&sw=2" euc-japan
     skk-w3m-get-candidates-from-goo-daijirin)
    ("goo-exceed-waei"
     "http://dictionary.goo.ne.jp/cgi-bin/dict_search.cgi?MT=%s&sw=1" euc-japan
     skk-w3m-get-candidates-from-goo-exceed-waei)
    ("goo-exceed-eiwa"
     "http://dictionary.goo.ne.jp/cgi-bin/dict_search.cgi?MT=%s&sw=0" euc-japan
     skk-w3m-get-candidates-from-goo-exceed-eiwa)
    ("goo-daily-shingo"
     "http://dictionary.goo.ne.jp/cgi-bin/dict_search.cgi?MT=%s&sw=3" euc-japan
     skk-w3m-get-candidates-from-goo-daily-shingo))
  "*")

;;; system internal variables and constants.
;; constants.
(defconst skk-w3m-working-buffer " *skk-w3m*")

;; global variable

;;;###autoload
(defun skk-w3m-search (search-engine)
  (let* ((w3m-search-engine-alist skk-w3m-search-engine-alist)
	 (info (cdr (assoc search-engine w3m-search-engine-alist)))
	 (post-process (nth 2 info))
	 (henkan-key skk-henkan-key))
    (if info
	(save-excursion
	  (save-window-excursion
	    (set-buffer (get-buffer-create skk-w3m-working-buffer))
	    (w3m-search search-engine henkan-key)
	    (if post-process (funcall post-process henkan-key)))))))

(defun skk-w3m-get-candidates (header0 header1)
  (if (re-search-forward header0 nil t nil)
      (let (v)
	(while (re-search-forward header1 nil t nil)
	  (setq v (cons (match-string-no-properties 1) v)))
	(nreverse v))))

(defun skk-w3m-get-candidates-from-goo-daijirin (key)
  (skk-w3m-get-candidates
   (concat "$B"#!N(B" (regexp-quote key) "$B!O$NBg<-NSBhFsHG$+$i$N8!:w7k2L!!(B [0-9]+$B7o(B")
   (concat "[0-9]+ +$B?75,$G3+$/(B +" (regexp-quote key) "$B!Z(B\\([^$B!Z![(B]+\\)$B![(B +$")))

(defun skk-w3m-get-candidates-from-goo-exceed-waei (key)
  ;; not yet.
  )

(defun skk-w3m-get-candidates-from-goo-exceed-eiwa (key)
  ;; not yet.
  )

(defun skk-w3m-get-candidates-from-goo-daily-shingo (key)
  ;; not yet.
  )

;; 15:$B"#!N$3$&$3$&!O$NBg<-NSBhFsHG$+$i$N8!:w7k2L!!(B 39$B7o(B                              
;; 16:*                                                                              
;; 17:                                                                               
;; 18:  1   $B?75,$G3+$/(B  $B$3$&$3$&!Z8}9P![(B                                             
;; 19:                                                                               
;; 20:  2   $B?75,$G3+$/(B  $B$3$&$3$&!Z9)9b![(B                                             
;; 21:                                                                               
;; 22:  3   $B?75,$G3+$/(B  $B$3$&$3$&!Z8x9T![(B                                             
;; 23:                                                                               
;; 24:  4   $B?75,$G3+$/(B  $B$3$&$3$&!Z8x8t![(B                                             
;; 25:                                                                               
;; 26:  5   $B?75,$G3+$/(B  $B$3$&$3$&!Z9C9a![(B                                             
;; 27:                                                                               
;; 28:  6   $B?75,$G3+$/(B  $B$3$&$3$&!Z8rU=![(B                                             
;; 29:                                                                               
;; 30:  7   $B?75,$G3+$/(B  $B$3$&$3$&!Z9#8}![(B                                             
;; 31:                                                                               
;; 32:  8   $B?75,$G3+$/(B  $B$3$&$3$&!Z9'9T![(B                                             
;; 33:                                                                               
;; 34:  9   $B?75,$G3+$/(B  $B$3$&$3$&!Z8e9T![(B                                             
;; 35:                                                                               
;; 36:  10  $B?75,$G3+$/(B  $B$3$&$3$&!Z8e9M![(B                                             
;; 37:                                                                               
;; 38:  11  $B?75,$G3+$/(B  $B$3$&$3$&!Z8e96![(B                                             
;; 39:                                                                               
;; 40:  12  $B?75,$G3+$/(B  $B$3$&$3$&!Z8e9`![(B                                             
;; 41:                                                                               
;; 42:  13  $B?75,$G3+$/(B  $B$3$&$3$&!Z9D9M![(B                                             
;; 43:                                                                               
;; 44:  14  $B?75,$G3+$/(B  $B$3$&$3$&!Z9a9a![(B                                             
;; 45:                                                                               
;; 46:  15  $B?75,$G3+$/(B  $B$3$&$3$&!Z9R9T![(B                                             
;; 47:                                                                               
;; 48:  16  $B?75,$G3+$/(B  $B$3$&$3$&!Z9_9H![(B                                             
;; 49:                                                                               
;; 50:  17  $B?75,$G3+$/(B  $B$3$&$3$&!Z9b9;![(B                                             
;; 51:                                                                               
;; 52:  18  $B?75,$G3+$/(B  $B$3$&$3$&!Z2+8}![(B                                             
;; 53:                                                                               
;; 54:  19  $B?75,$G3+$/(B  $B$3$&$3$&!Z9A8}![(B                                             
;; 55:                                                                               
;; 56:  20  $B?75,$G3+$/(B  $B$3$&$3$&!Z9E9Q![(B                                             
;; 57:                                                                               
;; 58:  21  $B?75,$G3+$/(B  $B$3$&$3$&!Z9E9]![(B                                             
;; 59:                                                                               
;; 60:  22  $B?75,$G3+$/(B  $B$3$&$3$&!Z9[9#![(B                                             
;; 61:                                                                               
;; 62:  23  $B?75,$G3+$/(B  $B$3$&$3$&!Z9=7e![(B                                             
;; 63:                                                                               
;; 64:  24  $B?75,$G3+$/(B  $B$3$&$3$&!Z9Qfk![(B                                             
;; 65:                                                                               
;; 66:  25  $B?75,$G3+$/(B  $B$3$&$3$&!Z!|!|![(B                                             
;; 67:                                                                               
;; 68:  26  $B?75,$G3+$/(B  $B$3$&$3$&!Z9c9B![(B                                             
;; 69:                                                                               
;; 70:  27  $B?75,$G3+$/(B  $B$3$&$3$&!Z9bV>![(B                                             
;; 71:                                                                               
;; 72:  28  $B?75,$G3+$/(B  $B$3$&$3$&!Z2+6=![(B                                             
;; 73:                                                                               
;; 74:  29  $B?75,$G3+$/(B  $B$3$&$3$&!Z9@9@![(B                                             
;; 75:                                                                               
;; 76:  30  $B?75,$G3+$/(B  $B$3$&$3$&!ZfVfV![(B                                             
;; 77:                                                                               
;; 78:  31  $B?75,$G3+$/(B  $B$3$&$3$&!Zb+b+!&b)b)![(B                                       
;; 79:                                                                               
;; 80:  32  $B?75,$G3+$/(B  $B$3$&$3$&!Z_j_j!&9898![(B                                       
;; 81:                                                                               
;; 82:  33  $B?75,$G3+$/(B  $B$3$&$3$&!Zn#n#![(B                                             
;; 83:                                                                               
;; 84:  34  $B?75,$G3+$/(B  $B$3$&$3$&!Z[%[%!&9-9-![(B                                       
;; 85:                                                                               
;; 86:  35  $B?75,$G3+$/(B  $B$3$&$3$&!Z9T9T![(B                                             
;; 87:                                                                               
;; 88:  36  $B?75,$G3+$/(B  $B$3$&$3$&!ZZ^Z^![(B                                             
;; 89:                                                                               
;; 90:  37  $B?75,$G3+$/(B  $B$3$&$3$&(B                                                     
;; 91:                                                                               
;; 92:  38  $B?75,$G3+$/(B  $B$3$&$3$&!Z;[$&;[$&![(B                                         
;; 93:                                                                               
;; 94:  39  $B?75,$G3+$/(B  $B$3$&$3$&!Zo3o3![(B                                             
;; 95:                                                                               
;; 96:                                                                               
;; 97:*                                                                              
;; 98:$B"#!N$3$&$3$&!O$NBg<-NSBhFsHG$+$i$N8!:w7k2L!!(B 39$B7o(B                              

;; 15:$B"#!N$M$C$7$s!O$N(BEXCEED$BOB1Q<-E5$+$i$N8!:w7k2L!!(B                                 
;; 16:*                                                                              
;; 17:                                                                               
;; 18:$B$M$C$7$s(B                                                                       
;; 19:[clear] $BG.?4(B                                                                   
;; 20:[clear] zeal$B!(!!(Bardor$B!(!!(Beagerness$B!(!!(Benthusiasm$B!%!!!A$J!!(B                     
;; 21:        eager$B!(!!(Bardent$B!(!!(Bkeen$B!%!!!A$K!!(Beagerly$B!(!!(B                           
;; 22:        earnestly$B!(!!(Bintently$B!%!!(B                                              
;; 23:                                                                               
;; 24:*                                                                              
;; 25:$B"#!N$M$C$7$s!O$N(BEXCEED$BOB1Q<-E5$+$i$N8!:w7k2L!!(B                                 

;; 15:$B"#!N(BSPA$B!O$N%G%$%j!<?78l<-E5$+$i$N8!:w7k2L!!(B                                    
;; 16:*                                                                              
;; 17:                                                                               
;; 18:SPA                                                                            
;; 19:                                                                               
;; 20:  $B!N(Bspeciality store retailer of private label apparel$B!O(B                       
;; 21:  $B<+<R%V%i%s%I$N0aNAIJ$rGd$kD>1DE9$N$3$H!#$^$?!$$=$N$h$&$J;v6H7ABV!#0aNAIJ$N4k(B 
;; 22:  $B2h!&3+H/$+$i@=B$!&N.DL!&HNGd$K;j$k$^$G$r0l3g$7$F<h$j07$$!$8\5R$N%K!<%:$K8zN((B 
;; 23:  $BE*$KBP1~$9$k!#(B                                                               
;; 24:  $B"*%W%i%$%Y!<%H(B-$B%V%i%s%I(B                                                      
;; 25:  $B!LFH<+%V%i%s%I0aNA$N@lLgE9HNGd6H<T$NN,!#%"%a%j%+$N0aNA>.GdE9$K$h$kB$8l$,5/8;(B 
;; 26:  $B!M(B                                                                           
;; 27:                                                                               
;; 28:                                                                               
;; 29:*                                                                              
;; 30:$B"#!N(BSPA$B!O$N%G%$%j!<?78l<-E5$+$i$N8!:w7k2L!!(B                                    

;; 15:$B"#!N(Belaborate$B!O$N(BEXCEED$B1QOB<-E5$+$i$N8!:w7k2L!!(B                                
;; 16:*                                                                              
;; 17:                                                                               
;; 18:e$B!&(Blab$B!&(Bo$B!&(Brate$B!!(B $B!!(B                                                           
;; 19:[clear] $B!|!|!|!|!|!|!|!|!|!|!!(B                                                 
;; 20:[clear] a.$B!!G0F~$j$J!$!!LJL)!N@:9*!O$J!$!!6E$C$?!%!!(B                           
;; 21:        $B!]!!(B                                                                   
;; 22:[clear] $B!|!|!|!|!|!|!|!!(B                                                       
;; 23:[clear] vt.$B!!6l?4$7$F:n$k!N:n$j=P$9!O!$!!?dZJ!J$9$$$3$&!K(B                      
;; 24:        $B!NI_1d!J$U$($s!K!O$9$k!%!!(B                                             
;; 25:[clear] elaborately$B!!(B $B!!(B                                                       
;; 26:[clear] ad.$B!!G0F~$j$K!$!!LJL)$K!%!!(B                                            
;; 27:[clear] elaborateness$B!!(B                                                        
;; 28:[clear] n.$B!!(B                                                                   
;; 29:[clear] elaboration$B!!(B $B!!(B                                                       
;; 30:[clear] n.$B!!LJL)$J;E>e$2!(!!?dZJ!(!!NO:n!(!!DI2C$7$?>\:Y!%(B                     
;; 31:        $B!!(B                                                                     
;; 32:[clear] elaborative$B!!(B                                                          
;; 33:[clear] a.$B!!F~G0$J!%!!(B                                                         
;; 34:                                                                               
;; 35:*                                                                              
;; 36:$B"#!N(Belaborate$B!O$N(BEXCEED$B1QOB<-E5$+$i$N8!:w7k2L!!(B                                

(require 'product)
(product-provide (provide 'skk-w3m) (require 'skk-version))
;;; Local Variables:
;;; End:
;;; skk-w3m.el ends here
