;;; context-skk.el --- turning off skk when the point leaves "string" or ; comments
;;
;; Copyright (C) 2003 Masatake YAMATO
;;
;; Author: Masatake YAMATO <jet@gyve.org>
;; Created: Tue May 13 19:12:23 2003
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;;
;;; Commentary:
;; $B$3$N%W%m%0%i%`$OJT=8$NJ8L.$K1~$8$F<+F0E*$K(Bskk$B$N%b!<%I$r(Blatin$B$K@Z$jBX$((B
;; $B$^$9!#(B
;;
;; $B$"$k%W%m%0%i%_%s%08@8l$G%W%m%0%i%`$r=q$$$F$$$k$H$-!"F|K\8lF~NO$NI,MW$,(B
;; $B$"$k$N$O0lHL$K!"$=$N%W%m%0%i%_%s%08@8l$NJ8;zNsCf$+%3%a%s%HCf$K8B$i$l$^(B
;; $B$9!#J8;zNs!"%3%a%s%H$N!V30!W$rJT=8$9$k$H$-$O!"B?$/$N>l9gF|K\8lF~NO$OI,(B
;; $BMW$"$j$^$;$s!#(B
;; $B$?$H$($P(B $B!V$((B ^H l$B!W(B emacs lisp$B$G$O!"(B
;; "$B!A(B" $B$d(B ;; $B!A(B
;; $B$H$$$C$?8D=j$G$@$1F|K\8lF~NO$,I,MW$H$J$j$^$9!#(B
;; 
;; $B$3$N%W%m%0%i%`$G$O!"8=:_$NJ8;zNs$H%3%a%s%H$N!V30!W$rJT=83+;O$HF1;~$K(B
;; (skk$B$,%*%s$G$"$l$P(B)skk$B$NF~NO%b!<%I$r(Blatin$B$K@Z$jBX$($^$9!#!V30!W$NJT=8(B
;; $B$r3+;O$9$k$K$"$?$C$F!"F|K\8lF~NO$,(B $B!V$*(B ^H l$B!W(B on$B$K$J$C$F$$$?$?$a$KH/@8(B
;; $B$7$?F~NO8m$j$H$=$N=$@5A`:n$r2sHr$9$k$3$H$,$G$-$^$9!#(B
;;
;; $B<+F0@Z$jBX$($O(Bcontext-skk$B$H$$$&%^%$%J!<%b!<%I$H$7$F<BAu$7$F$"$j$^$9!#(B
;; M-x context-skk
;; $B$G(B $B<+F0@Z$jBX$($N(Bon/off$B$r$G$-$^$9!#%b!<%I%i%$%s$K(B ";" $B$,I=<($5$l$F$$$k>l9g!"(B
;; $B%^%$%J!<%b!<%I$,(Bon$B$K$J$C$F$$$k$3$H$r0UL#$7$^$9!#(B
;;
;; - $B%$%s%9%H!<%k(B
;; (add-hook 'skk-load-hook
;;           '(require 'context-skk))
;;
;; - todo 
;; prefix arg
;; context-context-skk.el

;;; Code: 
;;(require 'skk)

;;
;; Options
;;
(defvar context-skk-context-check-hook ()
  "*$BF|K\8lF~NO$r<+F0E*$K(Boff$B$K$7$?$$!V%3%s%F%-%9%H!W$K$$$l$P(Bt$B$rJV$94X?t$rEPO?$9$k!#(B")
(add-hook ' context-skk-context-check-hook
	    'context-skk-out-of-string-or-comment-in-programming-mode-p)
(add-hook ' context-skk-context-check-hook
	    'context-skk-in-mew-draft-attachments-region-p)

(defvar context-skk-programming-mode
  '(ada-mode antlr-mode asm-mode autoconf-mode awk-mode
    c-mode objc-mode java-mode idl-mode pike-mode cperl-mode
    ;;?? dcl-mode
    delphi-mode f90-mode fortran-mode
    icon-mode idlwave-mode inferior-lisp-mode m4-mode makefile-mode
    metafont-mode modula-2-mode octave-mode pascal-mode perl-mode
    prolog-mode ps-mode postscript-mode scheme-mode sh-mode simula-mode
    ;; sql-mode
    tcl-mode vhdl-mode emacs-lisp-mode lisp-interaction-mode)
  "*context-skk$B$K$F!V%W%m%0%i%_%s%0%b!<%I!W$H$_$J$9%b!<%I$N%j%9%H(B")


(define-minor-mode context-skk
  "$BJ8L.$K1~$8$F<+F0E*$K(Bskk$B$NF~NO%b!<%I$r(Blatin$B$K@Z$j49$($k%^%$%J!<%b!<%I!#(B"
  t " \";\"")

;;
;; Advices
;;
(defadvice skk-insert (around skk-insert-ctx-switch activate)
  "$BJ8L.$K1~$8$F<+F0E*$K(Bskk$B$NF~NO%b!<%I$r(Blatin$B$K$9$k!#(B"
  (if (and context-skk (context-skk-context-check))
      (context-skk-insert) 
    ad-do-it))

(defadvice skk-jisx0208-latin-insert (around skk-jisx0208-latin-insert-ctx-switch activate)
  "$BJ8L.$K1~$8$F<+F0E*$K(Bskk$B$NF~NO%b!<%I$r(Blatin$B$K$9$k!#(B"
  (if (and context-skk (context-skk-context-check))
      (context-skk-insert) 
    ad-do-it))

;;
;; Helper
;;
(defun context-skk-context-check ()
  "$BF|K\8lF~NO$r<+F0E*$K(Boff$B$K$7$?$$!V%3%s%F%-%9%H!W$K$$$l$P(Bt$B$rJV$9(B"
  (run-hook-with-args-until-success 'context-skk-context-check-hook))

(defun context-skk-insert ()
  "skk-latin-mode$B$r(Bon$B$K$7$?>e(B`this-command-keys'$B$KBP$9$k4X?t$r8F$S=P$7D>$9!#(B"
  (skk-latin-mode t)
  (call-interactively (key-binding (this-command-keys))))

(defun context-skk-out-of-string-or-comment-in-programming-mode-p ()
  "$B%W%m%0%i%_%s%0%b!<%I$K$"$C$FJ8;zNs$"$k$$$O%3%a%s%H$N30$K$$$l$P(Bnon-nil$B$rJV$9!#(B
$B%W%m%0%i%_%s%0%b!<%I$K$$$J$$>l9g$O(Bnil$B$rJV$9!#(B
$B%W%m%0%i%_%s%0%b!<%I$K$"$C$FJ8;zNs$"$k$$$O%3%a%s%H$NCf$K$$$k>l9g(Bnil$B$rJV$9!#(B"
  (and (context-skk-in-programming-mode-p) 
       (not (or (context-skk-in-string-p)
		(context-skk-in-comment-p)))))

(defun context-skk-in-mew-draft-attachments-region-p ()
  (and (eq major-mode 'mew-draft-mode)
       (fboundp 'mew-attach-begin)
       (<= (or (mew-attach-begin) (1+ (point-max))) (1- (point)))))
;;
;; Sub predicators
;;
(defun context-skk-in-string-p ()
  (nth 3 (parse-partial-sexp (point) (point-min))))
(defun context-skk-in-comment-p ()
  (nth 4 (parse-partial-sexp (point) (point-min))))

(defun context-skk-in-programming-mode-p ()
  (memq major-mode
	context-skk-programming-mode))

(provide 'context-skk)
;; context-skk.el ends here
