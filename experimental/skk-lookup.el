;;; skk-lookup.el --- SKK lookup interface
;; Copyright (C) 1999 Mikio Nakajima <minakaji@osaka.email.ne.jp>

;; Author: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Maintainer: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Version: $Id: skk-lookup.el,v 1.1 1999/09/28 12:30:51 minakaji Exp $
;; Keywords: japanese
;; Last Modified: $Date: 1999/09/28 12:30:51 $

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
;; $B<!$N$h$&$K(B skk-search-prog-list $B$K2C$($F;XDj$7;HMQ$9$k!#(Bskkserv $B$r(B
;; $B;HMQ$7$?8!:w$N8e$K;}$C$F$/$k$N$,%;%*%j!<!#(B
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
;;; Code:
(eval-when-compile (require 'skk))
(require 'lookup)

(defgroup skk-lookup nil "SKK lookup related customization."
  :prefix "skk-lookup-"
  :group 'skk )

(defcustom skk-lookup-pickup-pattern "$B!Z(B\\(.+\\)$B![(B$"
  "*$B8uJdCj=P$N$?$a$N(B regexp$B!#(B"
  :type 'regexp
  :group skk-lookup )

(defcustom skk-lookup-split-pattern "$B!&(B"
  "*$BJ#?t$N8uJd$,$"$k>l9g$N8uJd$N6h@Z$j$N(B regexp$B!#(B"
  :type 'regexp
  :group skk-lookup  )

(defcustom skk-lookup-query [:query exact pattern]
  "*Lookup $B$KBP$7H/9T$9$k(B query $B$N7A<0!#(B
pattern $BItJ,$K8+=P$78l$,A^F~$5$l$k!#(B"
  :type 'vector
  :group skk-lookup  )

;;;###autoload
(defun skk-lookup-search ()
  (let* ((d (lookup-module-dictionaries lookup-default-module))
	 (query-base (copy-sequence skk-lookup-query))
	 (query (progn (aset query-base 2 skk-henkan-key) query-base))
	v r s )
    (while d
      (setq v (lookup-vse-search-query (car d) query))
      (while v
	(setq r (lookup-entry-heading (car v))
	      v (cdr v) )
	(when (string-match skk-lookup-pickup-pattern r)
	  (if (not skk-lookup-split-pattern)
	      (setq s (cons (match-string 1 r) s))
	    (setq s (nconc
		     (split-string (match-string 1 r) skk-lookup-split-pattern)
		     s )))))
      (setq d (cdr d)) )
    s ))


(provide 'skk-lookup)
;;; Local Variables:
;;; End:
;;; skk-lookup.el ends here
