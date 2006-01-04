;;; skk-comp.el --- $BJd40$N$?$a$N%W%m%0%i%`(B

;; Copyright (C) 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997
;;               1999, 2000
;;   Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>

;; Author: Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Version: $Id: skk-comp.el,v 1.47 2006/01/04 10:10:45 skk-cvs Exp $
;; Keywords: japanese, mule, input method
;; Last Modified: $Date: 2006/01/04 10:10:45 $

;; This file is part of Daredevil SKK.

;; Daredevil SKK is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.

;; Daredevil SKK is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Daredevil SKK, see the file COPYING.  If not, write to
;; the Free Software Foundation Inc., 51 Franklin St, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; $B"&$5(B (TAB) -> $B"&$5$H$&(B (.) -> $B"&$5$$$H$&(B (,) -> $B"&$5$H$&(B(.) -> $B"&$5$$$H$&(B

;;; Code:

(eval-when-compile
  (require 'skk-macs)
  (require 'skk-vars))

;;;###autoload
(defun skk-comp-start-henkan (arg)
  "$B"&%b!<%I$GFI$_$NJd40$r9T$J$C$?8e!"JQ49$9$k!#(B
$B$=$l0J30$N%b!<%I$G$O%*%j%8%J%k$N%-!<%^%C%W$K3d$jIU$1$i$l$?%3%^%s%I$r%(%_%e%l!<(B
$B%H$9$k!#(B"
  (interactive "*P")
  (cond
   ((eq skk-henkan-mode 'on)
    (skk-comp-do (not (eq last-command 'skk-comp-do)))
    (skk-start-henkan arg))
   (t
    (skk-emulate-original-map arg))))

;;;###autoload
(defun skk-comp (first &optional silent)
  (setq this-command 'skk-comp-do)
  (skk-comp-do first silent))

;;;###autoload
(defun skk-comp-do (first &optional silent)
  ;; main completion engine.
  (let ((inhibit-quit t)
	;; skk-num $B$,(B require $B$5$l$F$J$$$H(B
	;; buffer-local $BCM$r2u$962$l$"$j!#(B
	skk-num-list
	c-word)
    (skk-kana-cleanup 'force)
    (when first
      (setq skk-comp-search-done nil
	    skk-comp-stack nil
	    skk-comp-depth 0))
    (when first
      (setq skk-comp-key (buffer-substring-no-properties
			  skk-henkan-start-point (point))))
    (when (and (not (memq skk-use-look '(nil conversion)))
	       skk-abbrev-mode
	       (not (memq skk-look-ignore-case '(nil conversion))))
      (setq skk-comp-key (downcase skk-comp-key)))
    (cond
     ;; ($B2a5n$KC5:w:Q$_$NFI$_$r%"%/%;%9Cf(B)
     (skk-comp-search-done
      (if (= skk-comp-depth 0)
	  ;; circulate $B$J$i$P(B c-word = skk-comp-key $B$J$N$G(B c-word = nil
	  ;; non-circulate $B$J$i$P(B $B$3$l0J>e8uJd$,$J$$$N$G(B c-word = nil
	  (if skk-comp-circulate
	      (setq skk-comp-depth (length skk-comp-stack)))
	(setq skk-comp-depth (1- skk-comp-depth))
	(setq c-word (nth skk-comp-depth skk-comp-stack))))
     ;; ($B?75,$NFI$_$r<-=q%P%C%U%!$+$iC5:w(B)
     ;; skk-comp-key $B$O%P%C%U%!%m!<%+%kCM$J$N$G!"<-=q%P%C%U%!$K0\$kA0$K(B
     ;; $B0l;~JQ?t$K0\$7JQ$($F$*$/!#(B
     (t
      (setq c-word
	    (or (let ((word (skk-comp-do-1 skk-comp-key first)))
		  (while (member word skk-comp-stack)
		    (setq word (skk-comp-do-1 skk-comp-key first)))
		  word)
		;;
		(when (and skk-abbrev-mode
			   (not (memq skk-use-look '(nil conversion))))
		  (skk-look-completion))))
      (if c-word
	  ;; $B?75,$K8+$D$1$?$H$-$@$1(B push $B$9$k!#(B
	  (push c-word skk-comp-stack)
	(setq skk-comp-search-done t)
	(if skk-comp-circulate
	    (setq skk-comp-depth (length skk-comp-stack))))))
    ;; $B<-=q%P%C%U%!$N30!#(B
    (cond
     (c-word
      (delete-region skk-henkan-start-point (point))
      (insert c-word))
     (t
      ;; When skk-comp-circulate, return to the keyword.
      (when (or skk-comp-circulate
		(and (not (memq skk-use-look '(nil conversion)))
		     skk-abbrev-mode
		     (not (memq skk-look-ignore-case '(nil conversion)))))
	(delete-region skk-henkan-start-point (point))
	(insert skk-comp-key))
      (unless silent
	(ding)
	(cond
	 ((string= skk-comp-key "")
	  (skk-message "$B$3$l0J>e$NMzNr$O$"$j$^$;$s(B"
		       "No more words in history"))
	 (t
	  (if skk-japanese-message-and-error
	      (message "\"%s\" $B$GJd40$9$Y$-8+=P$78l$O(B%s$B$"$j$^$;$s(B"
		       skk-comp-key
		       (if first "" "$BB>$K(B"))
	    (message "No %scompletions for \"%s\""
		     (if first "" "more ")
		     skk-comp-key)))))))))

;;;###autoload
(defun skk-comp-do-1 (key first)
  ;; skk-comp-1 $B$N%5%V%k!<%A%s!#(B
  (cond
   ((string= key "")
    (skk-comp-by-history))
   (t
    (let ((buffers (list (skk-get-jisyo-buffer skk-jisyo 'nomsg)
			 (skk-dic-setup-buffer)))
	  (abbrev skk-abbrev-mode)
	  word)
      (when first
	(skk-loop-for-buffers buffers
	  (goto-char skk-okuri-nasi-min)))
      (catch 'word
	(skk-loop-for-buffers buffers
	  (setq word (skk-comp-search-current-buffer key abbrev))
	  (if word
	      (throw 'word word)
	    nil)))))))

;;;###autoload
(defun skk-comp-search-current-buffer (key &optional abbrev)
  (let (c-word)
    (save-match-data
      ;; `case-fold-search' $B$O!"<-=q%P%C%U%!$G$O>o$K(B nil$B!#(B
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
			 (1- (search-forward " ")))))
	  (when (and abbrev
		     (string-match "\\Ca" c-word))
	    ;; abbrev $B%b!<%I$G!V(B3$B$M$s!W$J$I$NJd40$O$7$J$$(B
	    (setq c-word nil))))
      c-word)))

;;;###autoload
(defun skk-comp-previous ()
  ;; skk-abbrev-comma, skk-insert-comma $B$N%5%V%k!<%A%s!#(B
  ;; $BD>A0$KJd40$r9T$C$?8+=P$7$rA^F~$9$k!#(B
  (let ((inhibit-quit t)
	(stack-length (length skk-comp-stack))
	c-word)
    (if (and skk-comp-circulate (= skk-comp-depth stack-length))
	(setq skk-comp-depth 0)
      (setq skk-comp-depth (1+ skk-comp-depth)))
    (setq c-word (nth skk-comp-depth skk-comp-stack))
    (cond
     (c-word
      (delete-region skk-henkan-start-point (point))
      (insert c-word))
     (t
      (if (null skk-comp-circulate)
	  ;; non-circulate $B$J$i$P(B skk-comp-depth $B$,HO0O30$J$N$G(B 1 $BLa$9(B
	  (setq skk-comp-depth (1- skk-comp-depth))
	(delete-region skk-henkan-start-point (point))
	(insert skk-comp-key))
      ;;(setq skk-comp-depth (1- skk-comp-depth))
      (ding)
      (skk-message "\"%s\"$B$GJd40$9$Y$-8+=P$78l$OB>$K$"$j$^$;$s(B"
		   "No more previous completions for \"%s\""
		   skk-comp-key)))))

;;;###autoload
(defun skk-comp-previous/next (ch)
  (setq this-command 'skk-comp-do)
  (cond ((eq ch skk-next-completion-char)
	 (skk-comp-do nil))
	((eq ch skk-previous-completion-char)
	 (skk-previous-completion))))

;;;###autoload
(defun skk-comp-by-history ()
  (unless skk-comp-stack
    (let (list
	  el)
      (dolist (cell skk-kakutei-history)
	(setq el (car cell))
	(unless (member el list)
	  (push el list)))
      (setq skk-comp-kakutei-midasi-list
	    (nreverse list))))
  (pop skk-comp-kakutei-midasi-list))

(defalias 'skk-previous-completion 'skk-comp-previous)
(defalias 'skk-start-henkan-with-completion 'skk-comp-start-henkan)

(run-hooks 'skk-comp-load-hook)

(require 'product)
(product-provide
    (provide 'skk-comp)
  (require 'skk-version))

;;; skk-comp.el ends here
