;;; skk-abbrev.el --- SKK/Emacs abbrev mode interface.
;; Copyright (C) 1999 Mikio Nakajima <minakaji@osaka.email.ne.jp>

;; Author: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Maintainer: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Version: $Id: skk-abbrev.el,v 1.2 1999/11/14 15:27:44 minakaji Exp $
;; Keywords: japanese
;; Created: Oct. 23, 1999
;; Last Modified: $Date: 1999/11/14 15:27:44 $

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

;;; Commentary
;;
;; <how to install>
;;   $B2<5-$N%U%)!<%`$r(B .emacs $B$+(B .skk $B$K=q$$$F2<$5$$!#(Bskk-search-prog-list $B$K(B
;; $B$D$$$F$O!"A4It$r<L$9I,MW$O$"$j$^$;$s!#(B(skk-abbrev-search) $B$r4^$`$h$&$K;XDj$7(B
;; $B$F2<$5$$!#(B
;;
;; (autoload 'skk-abbrev-search "skk-abbrev" nil nil nil)
;;
;; (setq skk-search-prog-list
;;       '((skk-search-jisyo-file skk-jisyo 0 t)
;;         ;; ADD NEXT LINE.
;;         (skk-abbrev-search)     
;;         (skk-search-server skk-aux-large-jisyo 10000) ))
;;
;;
;; <how to work>
;;   skk-abbrev-mode $B$GJQ49$7$?$H$-!"Jd40$7$?$H$-$K(B Emacs original $B$N(B
;; abbrev mode $B$r;H$C$?JQ49$r9T$J$$$^$9!#(B
;;
;; skk-look $B$H$NM%@h=g0L$O!"(B
;;  a.$BJQ49$K$D$$$F$O!"(Bskk-search-prog-list $B$NCf$G(B (skk-abbrev-search) $B$r(B
;;    $B@h=g0L$K=q$-;XDj$7$F2<$5$$!#(Bskk-abbrev-search $B$NJ}$,(B skk-look $B$h$j7Z$$(B
;;    $B$G$9!#(B
;;  b.$BJd40$K$D$$$F$O!"(Bskk-use-look $B$r;XDj$7$F$$$F$b!">o$K(B skk-abbrev-search
;;    $B$NJ}$,@h$K8!:w$5$l$^$9!#(B

;;; Code:
(eval-when-compile (require 'skk-foreword) (require 'skk-comp) )

;; Elib.
(require 'stack-m)

;;; ;;;###autoload
;;(defgroup skk-abbrev nil "SKK abbrev related customization."
;;  :prefix "skk-abbrev-"
;;  :group 'skk )

;;;###autoload
(defun skk-abbrev-search ()
  (let ((var (and skk-abbrev-mode (abbrev-expansion skk-henkan-key))))
    (and var (list var)) ))

(defadvice skk-completion-original (around skk-abbrev-ad activate)
  (let ((first (ad-get-arg 0))
	c-word )
    (condition-case nil
	;; not to search by look in ad-do-it.
	(let (skk-use-look)
	  ad-do-it )
      ;; no word to be completed.
      (error
       (if (not skk-abbrev-mode)
	   nil
	 (setq c-word (and (abbrev-expansion skk-completion-word)))
	 (if (and skk-use-look
		  (or (not c-word)
		      (member c-word (stack-all skk-completion-stack)) ))
	     ;; more searching by look when abbreviating is not enough.
	     (while (or (not c-word)
			(member c-word (stack-all skk-completion-stack)) )
	       (setq c-word (skk-look-completion)) )))
       (if (not c-word)
	   (if skk-japanese-message-and-error
	       (error "\"%s\" $B$GJd40$9$Y$-8+=P$78l$O(B%s$B$"$j$^$;$s(B"
		      skk-completion-word (if first "" "$BB>$K(B") )
	     (error "No %scompletions for \"%s\""
		    (if first "" "more ") skk-completion-word ))
	 (stack-push skk-completion-stack c-word)
	 (delete-region skk-henkan-start-point (point))
	 (insert c-word) )))))

;; end of skk-abbrev.el
