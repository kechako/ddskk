;;; skk-comp.el --- $BJd40$N$?$a$N%W%m%0%i%`(B
;; Copyright (C) 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997
;;               1999, 2000
;; Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>

;; Author: Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Version: $Id: skk-comp.el,v 1.18 2001/09/15 01:12:04 czkmt Exp $
;; Keywords: japanese
;; Last Modified: $Date: 2001/09/15 01:12:04 $

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
(defun skk-comp-start-henkan (arg)
  "$B"&%b!<%I$GFI$_$NJd40$r9T$J$C$?8e!"JQ49$9$k!#(B
$B$=$l0J30$N%b!<%I$G$O%*%j%8%J%k$N%-!<%^%C%W$K3d$jIU$1$i$l$?%3%^%s%I$r%(%_%e%l!<(B
$B%H$9$k!#(B"
  (interactive "*P")
  (if (and skk-henkan-on (not skk-henkan-active))
      (progn
	(skk-comp-do (not (eq last-command 'skk-comp-do)))
	(skk-start-henkan arg))
    (skk-emulate-original-map arg)))

;;;###autoload
(defun skk-comp (first &optional silent)
  (setq this-command 'skk-comp-do)
  (skk-comp-do first silent))

;;;###autoload
(defun skk-comp-do (first &optional silent)
  ;; main completion engine.
  (let ((inhibit-quit t)
	;; skk-num $B$,(B require $B$5$l$F$J$$$H(B buffer-local $BCM$r2u$962$l$"$j!#(B
	skk-num-list c-word)
    (skk-kana-cleanup 'force)
    (and first (setq skk-comp-stack nil skk-comp-depth 0))
    (and (or first skk-dabbrev-like-completion)
	 (setq skk-comp-key
	       (buffer-substring-no-properties skk-henkan-start-point (point))))
    (if (> skk-comp-depth 0)
	;; ($B2a5n$KC5:w:Q$_$NFI$_$r%"%/%;%9Cf(B)
	(setq skk-comp-depth (1- skk-comp-depth)
	      c-word (nth skk-comp-depth skk-comp-stack))
      ;; ($B?75,$NFI$_$r<-=q%P%C%U%!$+$iC5:w(B)
      ;; skk-comp-key $B$O%P%C%U%!%m!<%+%kCM$J$N$G!"<-=q%P%C%U%!$K0\$kA0$K(B
      ;; $B0l;~JQ?t$K0\$7JQ$($F$*$/!#(B
      (and (setq c-word
		 (or (let ((word (skk-comp-do-1 skk-comp-key first)))
		       (if (member word skk-comp-stack)
			   (skk-comp-do-1 skk-comp-key first)
			 word))
		     (and skk-abbrev-mode skk-use-look (skk-look-completion))))
	   ;; $B?75,$K8+$D$1$?$H$-$@$1(B cons $B$9$k!#(B
	   (setq skk-comp-stack (cons c-word skk-comp-stack))))
    ;; $B<-=q%P%C%U%!$N30!#(B
    (if (not c-word)
	(progn
	  (setq skk-comp-depth (1+ skk-comp-depth))
	  (if silent
	      nil
	  (ding)
	  (if (string= skk-comp-key "")
	      (skk-message
	       "$B$3$l0J>e$NMzNr$O$"$j$^$;$s(B"
	       "No more words on history")
	    (if skk-japanese-message-and-error
		(message "\"%s\" $B$GJd40$9$Y$-8+=P$78l$O(B%s$B$"$j$^$;$s(B"
			 skk-comp-key (if first "" "$BB>$K(B"))
	      (message "No %scompletions for \"%s\""
		       (if first "" "more ") skk-comp-key)))))
      (delete-region skk-henkan-start-point (point))
      (insert c-word))))

(defun skk-comp-do-1 (key first)
  ;; skk-comp-1 $B$N%5%V%k!<%A%s!#(B
  (when first
    (setq skk-dic-comp-first t))
  (cond
   ((string= key "")
    (skk-comp-by-history))
   (t
    (or (skk-comp-do-1-in-buf (skk-get-jisyo-buffer skk-jisyo)
			      key
			      first)
	(prog1
	    (skk-comp-do-1-in-buf (skk-dic-setup-buffer)
				  key
				  skk-dic-comp-first)
	  (setq skk-dic-comp-first nil))))))

(defun skk-comp-do-1-in-buf (buffer key first)
  (when (buffer-live-p buffer)
    (let (c-word)
      (with-current-buffer buffer
	(if first
	    (goto-char skk-okuri-nasi-min))
	(save-match-data
	  ;; case-fold-search $B$O!"<-=q%P%C%U%!$G$O>o$K(B nil$B!#(B
	  (while (and (not c-word)
		      (search-forward
		       (concat "\n"
			       (if skk-use-numeric-conversion
				   (skk-num-compute-henkan-key key)
				 key))
		       nil t))
	    (unless (eq (following-char)
			?\040) ;SPC
	      (setq c-word
		    (concat key
			    (buffer-substring-no-properties
			     ;; $B8+=P$78l$K6uGr$O4^$^$l$J$$!#(B
			     ;; " /" $B$r%5!<%A$9$kI,MW$O$J$$!#(B
			     (point)
			     (1- (search-forward " ")))))))
	  c-word)))))

;;;###autoload
(defun skk-comp-previous ()
  ;; skk-abbrev-comma, skk-insert-comma $B$N%5%V%k!<%A%s!#D>A0$KJd40$r9T$C$?8+(B
  ;; $B=P$7$rA^F~$9$k!#(B
  (let ((inhibit-quit t)
	(c-word
	 (progn
	   (setq skk-comp-depth (1+ skk-comp-depth))
	   (nth skk-comp-depth skk-comp-stack))))
    (if c-word
	(progn
	  (delete-region skk-henkan-start-point (point))
	  (insert c-word))
      (setq skk-comp-depth (1- skk-comp-depth))
      (ding)
      (skk-message "\"%s\"$B$GJd40$9$Y$-8+=P$78l$OB>$K$"$j$^$;$s(B"
		   "No more previous completions for \"%s\""
		   skk-comp-key))))

;;;###autoload
(defun skk-comp-previous/next (ch)
  (setq this-command 'skk-comp-do)
  (cond ((eq ch skk-next-completion-char)
	 (skk-comp-do nil))
	((eq ch skk-previous-completion-char)
	 (skk-previous-completion))))

(defun skk-comp-by-history ()
  (unless skk-comp-stack
    (let ((hist skk-kakutei-history)
	  list el)
      (while hist
	(setq el (caar hist))
	(unless (member el list)
	  (setq list (cons el list)))
	(setq hist (cdr hist)))
      (setq skk-comp-kakutei-midasi-list (nreverse list))))
  (prog1
      (car skk-comp-kakutei-midasi-list)
    (setq skk-comp-kakutei-midasi-list (cdr skk-comp-kakutei-midasi-list))))

(defalias 'skk-previous-completion 'skk-comp-previous)
(defalias 'skk-start-henkan-with-completion 'skk-comp-start-henkan)

(run-hooks 'skk-comp-load-hook)
(require 'product)
(product-provide (provide 'skk-comp) (require 'skk-version))
;;; Local Variables:
;;; End:
;;; skk-comp.el ends here
