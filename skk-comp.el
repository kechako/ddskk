;;; skk-comp.el --- $BJd40$N$?$a$N%W%m%0%i%`(B
;; Copyright (C) 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997
;;               1999
;; Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>

;; Author: Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>
;; Maintainer: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Version: $Id: skk-comp.el,v 1.4.2.4 1999/11/10 13:02:38 minakaji Exp $
;; Keywords: japanese
;; Last Modified: $Date: 1999/11/10 13:02:38 $

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

;;; Code:
(eval-when-compile (require 'skk-macs) (require 'skk-vars))

;; Elib version 1.0 required.
(require 'stack-m)

;;;###autoload
(defun skk-start-henkan-with-completion (arg)
  "$B"&%b!<%I$GFI$_$NJd40$r9T$J$C$?8e!"JQ49$9$k!#(B
$B$=$l0J30$N%b!<%I$G$O%*%j%8%J%k$N%-!<%^%C%W$K3d$jIU$1$i$l$?%3%^%s%I$r%(%_%e%l!<(B
$B%H$9$k!#(B"
  (interactive "*P")
  (if (and skk-henkan-on (not skk-henkan-active))
      (progn
        (skk-completion (not (eq last-command 'skk-completion)))
        (skk-start-henkan arg) )
    (skk-emulate-original-map arg) ))

;;;###autoload
(defun skk-completion (first)
  ;; skk-try-completion $B$N%5%V%k!<%A%s!#(B
  (funcall skk-completion-function first) )

(defun skk-completion-original (first)
  (let ((inhibit-quit t)
        skk-num-list
        completion-word c-word )
    (skk-kana-cleanup 'force)
    (and first (setq skk-completion-stack (stack-create)))
    (and (or first skk-dabbrev-like-completion)
	 (setq skk-completion-word
	       (buffer-substring-no-properties skk-henkan-start-point (point)) ))
    (and (string= skk-completion-word "")
	 (skk-error "$B6uJ8;z$+$iJd40$9$k$3$H$O$G$-$^$;$s!*(B"
		    "Cannot complete an empty string!" ))
    ;; skk-completion-word $B$O%P%C%U%!%m!<%+%kCM$J$N$G!"<-=q%P%C%U%!$K0\$kA0$K(B
    ;; $B0l;~JQ?t$K0\$7JQ$($F$*$/!#(B
    (setq completion-word skk-completion-word)
    (with-current-buffer (skk-get-jisyo-buffer skk-jisyo)
      (if first (goto-char skk-okuri-nasi-min))
      (save-match-data
        ;; case-fold-search $B$O!"<-=q%P%C%U%!$G$O>o$K(B nil$B!#(B
	(while
	    (and (not c-word)
		 (search-forward
		  (concat "\n"
			  (if skk-use-numeric-conversion
			      (skk-num-compute-henkan-key completion-word)
			    completion-word ))
		  nil t ))
	  (if (eq (following-char) ?\040) ;SPC
	      nil
	    (setq c-word (concat completion-word
				 (buffer-substring-no-properties
				  ;; $B8+=P$78l$K6uGr$O4^$^$l$J$$!#(B" /" $B$r%5!<(B
				  ;; $B%A$9$kI,MW$O$J$$!#(B
				  (point) (1- (search-forward " ")) )))))))
    (and (not c-word) skk-abbrev-mode skk-use-look
	 (setq c-word (skk-look-completion)) )
    ;; $B<-=q%P%C%U%!$N30!#(B
    (if (not c-word)
	(if skk-japanese-message-and-error
	    (error "\"%s\" $B$GJd40$9$Y$-8+=P$78l$O(B%s$B$"$j$^$;$s(B"
		   skk-completion-word (if first "" "$BB>$K(B") )
	  (error "No %scompletions for \"%s\""
		 (if first "" "more ") skk-completion-word ))
      (stack-push skk-completion-stack c-word)
      (delete-region skk-henkan-start-point (point))
      (insert c-word) )))

;;;###autoload
(defun skk-previous-completion ()
  ;; skk-abbrev-comma, skk-insert-comma $B$N%5%V%k!<%A%s!#D>A0$KJd40$r9T$C$?8+(B
  ;; $B=P$7$rA^F~$9$k!#(B
  (funcall skk-previous-completion-function) )

(defun skk-previous-completion-original ()
  (let ((inhibit-quit t)
        c-word )
    (setq c-word (stack-pop skk-completion-stack))
    (if (string= c-word
                 (buffer-substring-no-properties skk-henkan-start-point (point)) )
        ;; $B%]%C%W$7$?8l$,%P%C%U%!$N%]%$%s%HD>A0$K$"$kJ8;zNs$HF1$8$@$C$?$i(B 1 $B$D(B
        ;; $B<N$F$k!#(B
        (setq c-word (stack-pop skk-completion-stack)) )
    (if c-word
	(progn
	  (delete-region skk-henkan-start-point (point))
	  (insert c-word) )
      ;;(insert skk-completion-word)
      (skk-error "\"%s\"$B$GJd40$9$Y$-8+=P$78l$OB>$K$"$j$^$;$s(B"
                 "No more previous completions for \"%s\""
                 skk-completion-word ))
    (setq this-command 'skk-completion) ))

(run-hooks 'skk-comp-load-hook)

(provide 'skk-comp)
;;; Local Variables:
;;; End:
;;; skk-comp.el ends here
