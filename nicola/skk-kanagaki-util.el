;;; skk-kanagaki-util.el --- SKK $B$N2>L>F~NO%5%]!<%H$N$?$a$NF;6qH"(B
;; Copyright (C) 2000 Tetsuo Tsukamoto <czkmt@remus.dti.ne.jp>

;; Author: Tetsuo Tsukamoto <czkmt@remus.dti.ne.jp>
;; Keywords: hardware, japanese

;; This file is part of SKK (Simple Kana to Kanji conversion program).

;; SKK  is free software;  you  can redistribute it  and/or modify it under the
;; terms  of the GNU General Public License  as published  by the Free Software
;; Foundation;  either versions  2,  or  (at your option)  any  later version.

;; SKK  is distributed  in the hope  that  it will  be useful  but  WITHOUT ANY
;; WARRANTY;  without even the implied  warranty  of MERCHANTABILITY or FITNESS
;; FOR  A  PARTICULAR  PURPOSE.  See  the  GNU General Public License  for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; SKK,  see the file COPYING.  If not,  write  to the Free Software Foundation
;; Inc., 59 Temple Place - Suite 330, Boston,  MA 02111-1307, USA.

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
(defun skk-kanagaki-delete-backward-char (arg)
  ;; skk-delete-backward-char $B$r$A$g$C$H$@$1JQ99!#(B
  (interactive "*P")
  (skk-with-point-move
   (let ((count (prefix-numeric-value arg)))
     (cond (skk-henkan-active
	    (if (and (not skk-delete-implies-kakutei)
		     (= skk-henkan-end-point (point)))
		(skk-previous-candidate)
	      (if overwrite-mode
		  (progn
		    (backward-char count)
		    (delete-char count arg))
		(delete-backward-char count))
	      (if (> (length skk-prefix) count)
		  (setq skk-prefix (substring skk-prefix 0
					      (- (length skk-prefix) count)))
		(setq skk-prefix ""))
	      (and (>= skk-henkan-end-point (point)) (skk-kakutei))))
	   ((and skk-henkan-on (>= skk-henkan-start-point (point)))
	    (setq skk-henkan-count 0)
	    (skk-kakutei))
	   ((and skk-henkan-on overwrite-mode)
	    (backward-char count)
	    (delete-char count arg))
	   (t
	    (skk-delete-okuri-mark)
	    (if (skk-get-prefix skk-current-rule-tree)
		(skk-erase-prefix 'clean)
	      (delete-backward-char count)))))))

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
