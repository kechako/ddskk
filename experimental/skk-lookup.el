;;; skk-lookup.el --- SKK lookup interface
;; Copyright (C) 1999 Mikio Nakajima <minakaji@osaka.email.ne.jp>

;; Author: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Maintainer: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Version: $Id: skk-lookup.el,v 1.2 1999/09/28 14:48:17 minakaji Exp $
;; Keywords: japanese
;; Last Modified: $Date: 1999/09/28 14:48:17 $

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
  :group 'skk-lookup )

(defcustom skk-lookup-split-pattern "$B!&(B"
  "*$BJ#?t$N8uJd$,$"$k>l9g$N8uJd$N6h@Z$j$N(B regexp$B!#(B"
  :type 'regexp
  :group 'skk-lookup )

(put 'skk-lookup-start-session 'lisp-indent-function 2)
(eval-after-load "edebug" '(def-edebug-spec skk-lookup-start-session t))

(defmacro skk-lookup-start-session (module type &rest body)
  ;; like skk-lookup-start-session but returns of body's value.
  (` (unwind-protect
	 (let ((lookup-current-session (lookup-make-session (, module) (, type))))
	   (prog2
	       (lookup-module-setup (, module))
	       (,@ body)
	     (unless (eq lookup-last-session lookup-current-session)
	       (lookup-open-session) )))
       ;; $B%;%C%7%g%s$NESCf$G%(%i!<$,H/@8$7$?$H$-$O:G8e$N%;%C%7%g%s$KLa$9!#(B
       (setq lookup-current-session lookup-last-session))))

;;;###autoload
(defun skk-lookup-search ()
  (save-excursion
    (save-window-excursion
      (let ((module (lookup-default-module))
	    v )	
	;; Is it really necessary only to get headings?
	(setq lookup-search-pattern skk-henkan-key)
	(skk-lookup-start-session module 'lookup-search-session
	  (let ((query (lookup-make-query 'exact skk-henkan-key))
		(lookup-search-found)
		entry heading kouho )
	    (lookup-foreach
	     (lambda (dictionary)
	       (when (and (lookup-dictionary-selected-p dictionary)
			  (setq entries (lookup-vse-search-query dictionary query)) )
		 ;; Is it really necessary only to get headings?
		 (if lookup-search-found
		     (lookup-entry-append lookup-current-session entries)
		   (setq lookup-search-found t)
		   (lookup-session-set-query lookup-current-session query)
		   (lookup-session-set-entries lookup-current-session entries)
		   (lookup-open-session) )
		 (lookup-foreach
		  (lambda (entry)
		    (setq heading (lookup-entry-heading entry))
		    (when (string-match skk-lookup-pickup-pattern heading)
		      (setq kouho (match-string 1 heading))
		      (if (not skk-lookup-split-pattern)
			  (setq v (cons kouho (delete kouho v)))
			(lookup-foreach
			 (lambda (k) (setq v (cons k (delete k v))))
			 (split-string kouho skk-lookup-split-pattern) ))))
		  entries )))
	     (lookup-module-dictionaries module) )
	    v ))))))

(provide 'skk-lookup)
;;; Local Variables:
;;; End:
;;; skk-lookup.el ends here
