;;; skk-comp.el --- $BJd40$N$?$a$N%W%m%0%i%`(B
;; Copyright (C) 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997
;;               1999
;; Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>

;; Author: Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Version: $Id: skk-comp.el,v 1.7 2000/10/30 22:10:14 minakaji Exp $
;; Keywords: japanese
;; Last Modified: $Date: 2000/10/30 22:10:14 $

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
;; $B"&$5(B (TAB) -> $B"&$5$H$&(B (.) -> $B"&$5$$$H$&(B (,) -> $B"&$5$H$&(B(.) -> $B"&$5$$$H$&(B

;;; Code:
(eval-when-compile (require 'skk-macs) (require 'skk-vars))

;;;###autoload
(defun skk-start-henkan-with-completion (arg)
  "$B"&%b!<%I$GFI$_$NJd40$r9T$J$C$?8e!"JQ49$9$k!#(B
$B$=$l0J30$N%b!<%I$G$O%*%j%8%J%k$N%-!<%^%C%W$K3d$jIU$1$i$l$?%3%^%s%I$r%(%_%e%l!<(B
$B%H$9$k!#(B"
  (interactive "*P")
  (if (and skk-henkan-on (not skk-henkan-active))
      (progn
        (skk-completion (not (eq last-command 'skk-completion)))
        (skk-start-henkan arg))
    (skk-emulate-original-map arg)))

;;;###autoload
(defun skk-completion (first)
  ;; skk-try-completion $B$N%5%V%k!<%A%s!#(B
  (let ((inhibit-quit t)
	;; skk-num $B$,(B require $B$5$l$F$J$$$H(B buffer-local $BCM$r2u$962$l$"$j!#(B
        skk-num-list c-word)
    (skk-kana-cleanup 'force)
    (and first (setq skk-completion-stack nil skk-completion-depth 0))
    (and (or first skk-dabbrev-like-completion)
	 (setq skk-completion-word
	       (buffer-substring-no-properties skk-henkan-start-point (point))))
    (and (string= skk-completion-word "")
	 (skk-error "$B6uJ8;z$+$iJd40$9$k$3$H$O$G$-$^$;$s!*(B"
		    "Cannot complete an empty string!"))
    (if (> skk-completion-depth 0)
	;; ($B2a5n$KC5:w:Q$_$NFI$_$r%"%/%;%9Cf(B)
	(setq skk-completion-depth (1- skk-completion-depth)
	      c-word (nth skk-completion-depth skk-completion-stack))
      ;; ($B?75,$NFI$_$r<-=q%P%C%U%!$+$iC5:w(B)
      ;; skk-completion-word $B$O%P%C%U%!%m!<%+%kCM$J$N$G!"<-=q%P%C%U%!$K0\$kA0$K(B
      ;; $B0l;~JQ?t$K0\$7JQ$($F$*$/!#(B
      (and (setq c-word
		 (or (skk-completion-1 skk-completion-word first)
		     (and skk-abbrev-mode skk-use-look (skk-look-completion))))
	   ;; $B?75,$K8+$D$1$?$H$-$@$1(B cons $B$9$k!#(B
	   (setq skk-completion-stack (cons c-word skk-completion-stack))))
    ;; $B<-=q%P%C%U%!$N30!#(B
    (if (not c-word)
	(progn
	  (setq skk-completion-depth (1+ skk-completion-depth))
	  (if skk-japanese-message-and-error
	      (error "\"%s\" $B$GJd40$9$Y$-8+=P$78l$O(B%s$B$"$j$^$;$s(B"
		     skk-completion-word (if first "" "$BB>$K(B"))
	    (error "No %scompletions for \"%s\""
		   (if first "" "more ") skk-completion-word)))
      (delete-region skk-henkan-start-point (point))
      (insert c-word))))

(defun skk-completion-1 (key first)
  (let (c-word)
    (with-current-buffer (skk-get-jisyo-buffer skk-jisyo)
      (if first (goto-char skk-okuri-nasi-min))
      (save-match-data
	;; case-fold-search $B$O!"<-=q%P%C%U%!$G$O>o$K(B nil$B!#(B
	(while (and (not c-word)
		    (search-forward (concat "\n"
					    (if skk-use-numeric-conversion
						(skk-num-compute-henkan-key key)
					      key))
				    nil t))
	  (or (eq (following-char) ?\040) ;SPC
	      (setq c-word (concat key
				   (buffer-substring-no-properties
				    ;; $B8+=P$78l$K6uGr$O4^$^$l$J$$!#(B" /" $B$r%5!<(B
				    ;; $B%A$9$kI,MW$O$J$$!#(B
				    (point) (1- (search-forward " ")))))))
	c-word))))

;;;###autoload
(defun skk-previous-completion ()
  ;; skk-abbrev-comma, skk-insert-comma $B$N%5%V%k!<%A%s!#D>A0$KJd40$r9T$C$?8+(B
  ;; $B=P$7$rA^F~$9$k!#(B
  (let ((inhibit-quit t)
        (c-word 
	 (progn
	   (setq skk-completion-depth (1+ skk-completion-depth))
	   (nth skk-completion-depth skk-completion-stack))))
    (if c-word
	(progn
	  (delete-region skk-henkan-start-point (point))
	  (insert c-word))
      (setq skk-completion-depth (1- skk-completion-depth))
      (skk-error "\"%s\"$B$GJd40$9$Y$-8+=P$78l$OB>$K$"$j$^$;$s(B"
                 "No more previous completions for \"%s\""
                 skk-completion-word))))

(run-hooks 'skk-comp-load-hook)

(require 'product)
(product-provide (provide 'skk-comp) (require 'skk-version))
;;; Local Variables:
;;; End:
;;; skk-comp.el ends here
