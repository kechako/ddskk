;;; skk-kanagaki-util.el --- SKK $B$N2>L>F~NO%5%]!<%H$N$?$a$NF;6qH"(B
;; Copyright (C) 2000 Tetsuo Tsukamoto <czkmt@remus.dti.ne.jp>

;; Author: Tetsuo Tsukamoto <czkmt@remus.dti.ne.jp>
;; Keywords: hardware, japanese

;; This file is part of Daredevil SKK.

;; Daredevil SKK is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either versions 2, or (at your option) any later
;; version.

;; Daredevil SKK is distributed in the hope that it will be useful but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License along
;; with Daredevil SKK, see the file COPYING.  If not, write to the Free
;; Software Foundation Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Commentary:

;; NICOLA-DDSKK $B$K$H$C$FI,$:$7$b=EMWEY$N9b$/$J$$$b$N!"(Bmacro$B!"(Binline function $B$O(B
;; $B$3$3$KCV$-$^$9!#(B  $BI,MW$J>l9g$O3F%b%8%e!<%k$NCf$+$i$3$N%W%m%0%i%`$r%m!<%I$7$^(B
;; $B$9!#(B

;;; Code:

(eval-when-compile
  (require 'cl)
  (require 'static))

;; Variables.

(defconst skk-kanagaki-dakuten-alist
  '(("$B$+(B" "$B$,(B") ("$B$-(B" "$B$.(B") ("$B$/(B" "$B$0(B") ("$B$1(B" "$B$2(B") ("$B$3(B" "$B$4(B")
    ("$B$5(B" "$B$6(B") ("$B$7(B" "$B$8(B") ("$B$9(B" "$B$:(B") ("$B$;(B" "$B$<(B") ("$B$=(B" "$B$>(B")
    ("$B$?(B" "$B$@(B") ("$B$A(B" "$B$B(B") ("$B$D(B" "$B$E(B") ("$B$F(B" "$B$G(B") ("$B$H(B" "$B$I(B")
    ("$B$O(B" "$B$P(B" "$B$Q(B") ("$B$R(B" "$B$S(B" "$B$T(B") ("$B$U(B" "$B$V(B" "$B$W(B") ("$B$X(B" "$B$Y(B" "$B$Z(B")
    ("$B$[(B" "$B$\(B" "$B$](B")
                            ("$B%&(B" "$B%t(B")
    ("$B%+(B" "$B%,(B") ("$B%-(B" "$B%.(B") ("$B%/(B" "$B%0(B") ("$B%1(B" "$B%2(B") ("$B%3(B" "$B%4(B")
    ("$B%5(B" "$B%6(B") ("$B%7(B" "$B%8(B") ("$B%9(B" "$B%:(B") ("$B%;(B" "$B%<(B") ("$B%=(B" "$B%>(B")
    ("$B%?(B" "$B%@(B") ("$B%A(B" "$B%B(B") ("$B%D(B" "$B%E(B") ("$B%F(B" "$B%G(B") ("$B%H(B" "$B%I(B")
    ("$B%O(B" "$B%P(B" "$B%Q(B") ("$B%R(B" "$B%S(B" "$B%T(B") ("$B%U(B" "$B%V(B" "$B%W(B") ("$B%X(B" "$B%Y(B" "$B%Z(B")
    ("$B%[(B" "$B%\(B" "$B%](B")
    ;;
    ) "\
$BByE@$HH>ByE@$rF~NO$9$k$?$a$N%k!<%k!#(B")


;; Macros

;;;###autoload
(defmacro skk-kanagaki-help-1 (bufname title list)
  (`
   (let ((buf (get-buffer-create (, bufname))))
     (save-excursion
       (set-buffer buf)
       (setq buffer-read-only nil)
       (erase-buffer)
       (insert
	(concat
	 (format "%s\n\n" (, title))
	 (mapconcat
	  (function
	   (lambda (cons)
	     (cond
	      ((and (symbolp (car cons))
		    (symbol-value (car cons)))
	       (format "%s $B!D(B %s\n"
		       (key-description (symbol-value (car cons))) (cdr cons)))
	      (t
	       (format "%s $B!D(B %s\n" (car cons) (cdr cons))))))
	  ;;
	  (delq nil (, list)) "")))
       ;;
       (setq buffer-read-only t)
       (set-buffer-modified-p nil)
       (goto-char (point-min))
       (help-mode))
     (let ((standard-output buf))
       (print-help-return-message))
     (display-buffer buf))))

;;;###autoload
(put 'skk-kanagaki-call-xmodmap 'lisp-indent-function 1)

;;;###autoload
(defmacro skk-kanagaki-call-xmodmap (string &rest form)
  ;; STRING $B$NFbMF$r(B xmodmap $B$KEO$9!#@.8y$7$?$i(B FORM $B$r<B9T$9$k!#(B
  (list
   'let '((x (eq window-system 'x))
	  (prog (exec-installed-p "xmodmap"))
	  (tmp (make-temp-name
		(expand-file-name "kanagaki" skk-kanagaki-temp-dir))))
   (list
    'cond
    (list
     (list 'and 'x 'prog
	   '(message "xmodmap $B$r8F$s$G$$$^$9(B...")
	   (list
	    'save-excursion
	    '(set-buffer (get-buffer-create " *kanagaki*"))
	    '(erase-buffer)
	    (list 'insert string)
	    '(write-region (point-min) (point-max) tmp)
	    '(eq 0 (call-process prog nil nil nil tmp))))
     ;;
     (` (progn (,@ form)))
     '(delete-file tmp)
     '(message "xmodmap $B$r8F$s$G$$$^$9(B...$B40N;(B"))
    '(t
      (message "xmodmap $B$N8F$S=P$7$K<:GT$7$^$7$?(B")))))

;;;###autoload
(defmacro skk-kanagaki-make-string (n str)
  (case skk-emacs-type
   ((xemacs mule4 mule5)
    (` (make-string (, n) (string-to-char (, str)))))
   (t
    (` (mapconcat 'identity (make-vector (, n) (, str)) "")))))

;; Functions.

;;;###autoload
(defun skk-kanagaki-dakuten (&optional arg)
  "$BD>A0$NJ8;z$r8+$F2DG=$J$iByE@$rIU2C$7!"$5$b$J$1$l$P(B \"$B!+(B\" $B$rF~NO$9$k!#(B"
  (interactive "*p")
  (let ((list skk-kanagaki-dakuten-alist)
	(pt1 (point))
	(len (if (eq skk-emacs-type 'nemacs) 2 1))
	char1 char2)
    (setq char1
	  (save-excursion
	    (backward-char (* len 1))
	    (buffer-substring-no-properties (point) pt1)))
    (cond ((setq char2 (cadr (assoc char1 list)))
	   (delete-char -1)
	   (skk-insert-str char2))
	  (t
	   (skk-insert-str "$B!+(B")))))

;;;###autoload
(defun skk-kanagaki-handakuten (&optional arg)
  "$BD>A0$NJ8;z$r8+$F2DG=$J$iH>ByE@$rIU2C$7!"$5$b$J$1$l$P(B \"$B!,(B\" $B$rF~NO$9$k!#(B"
  (interactive "*p")
  (let ((list skk-kanagaki-dakuten-alist)
	(pt1 (point))
	(len (if (eq skk-emacs-type 'nemacs) 2 1))
	char1 char2)
    (setq char1
	  (save-excursion
	    (backward-char (* len 1))
	    (buffer-substring-no-properties (point) pt1)))
    (cond ((setq char2 (caddr (assoc char1 list)))
	   (delete-char -1)
	   (skk-insert-str char2))
	  (t
	   (skk-insert-str "$B!,(B")))))

;;;###autoload
(defun skk-kanagaki-bs (arg)
  ;; OASYS $B$K$*$1$k(B BS $B%-!<$N5!G=$NBe$o$j!#$I$N$h$&$J5sF0$r$5$;$k$Y$-$+$^$@7h$^(B
  ;; $B$C$F$$$J$$!#8=:_$N$H$3$m(B
  ;;
  ;; o $B"'%b!<%I$G$O(B `skk-kanagaki-esc' $B$HF1$85sF0(B
  ;; o $B"&%b!<%I$G$O(B `skk-delete-backward-char' $B$HF1$85sF0(B
  ;; o $B"#%b!<%I$G$O(B `delete-backward-char' $B$HF1$85sF0(B
  ;;
  ;; $B$H$$$&$U$&$K9M$($F$$$k!#(B
  (interactive "*p")
  (cond (skk-henkan-active
	 (call-interactively 'keyboard-quit))
	(skk-henkan-on
	 (if (= (point) (marker-position skk-henkan-start-point))
	     (skk-kakutei arg)
	   (forward-char -1)
	   (delete-char 1)))
	((and skk-isearch-switch
	      (buffer-live-p skk-kanagaki-isearch-buffer))
	 (with-current-buffer skk-kanagaki-isearch-buffer
	   (skk-isearch-delete-char arg)))
	(t
	 (delete-backward-char arg))))

;;;###autoload
(defun skk-kanagaki-esc (&optional arg)
  ;; OASYS $B$K$*$1$k<h$j>C$75!G=$NBe$o$j!#(B $B$H$j$"$($:(B keyboard-quit $B$N>l9g$HF1MM(B
  ;; $B$NF0:n$r$9$k$h$&$K$K$7$F$*$/!#(BOAK $B&BHG$@$H(B
  ;;
  ;; o 1 $B2sL\$N<h$j>C$7$G!"JQ49A0$N>uBV$KLa$7$?>e$GJQ493+;OE@$K%]%$%s%H$r0\F0(B
  ;; o 2 $B2sL\$N<h$j>C$7$GJQ49BP>]$NJ8;zNsA4BN$r>C5n(B
  ;;
  ;; $B$9$k$h$&$K$J$C$F$$$k$,!"(BSKK $B$K$*$1$kJQ49BP>]$NJ8;zNs$O(B $B"&(B $B$H%]%$%s%H$N4V$N(B
  ;; $BJ8;zNs$G$"$j!"%]%$%s%H$r0\F0$9$k$HJQ49BP>]$,JQ$o$C$F$7$^$&!#$=$N$?$a!"%]%$(B
  ;; $B%s%H$O0\F0$7$J$$$3$H$H$9$k!#(B
  (interactive "*P")
  (cond ((skk-in-minibuffer-p)
	 (call-interactively
	  (if (fboundp 'minibuffer-keyboard-quit)
	      'minibuffer-keyboard-quit
	    'abort-recursive-edit)))
	 ((or skk-henkan-on skk-henkan-active)
	  (call-interactively 'keyboard-quit))
	 (t
	  nil)))

;;;###autoload
(defun skk-nicola-visit-nicola-website ()
  (interactive)
  (let ((func (cond ((fboundp 'browse-url)
		     'browse-url)
		    (t
		     'browse-url-netscape))))
    (funcall func "http://nicola.sunicom.co.jp/")))


;;

(require 'product)
(product-provide (provide 'skk-kanagaki-util) (require 'skk-version))

;;; skk-kanagaki-util.el ends here
