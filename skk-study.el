;;; skk-study.el --- SKK $B3X=,8z2LDs6!%W%m%0%i%`(B
;; Copyright (C) 1999, 2000 NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>

;; Author: NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Version: $Id: skk-study.el,v 1.26 2001/12/16 05:03:11 czkmt Exp $
;; Keywords: japanese
;; Created: Apr. 11, 1999
;; Last Modified: $Date: 2001/12/16 05:03:11 $

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
;; the Free Software Foundation Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; $B$"$kC18l$rJQ49$7$?>l9g$K!"D>A0$KJQ49$7$?8l$r4XO"8l$H$7$FEPO?$7$F$*$-!":FEY$=(B
;; $B$NC18l$NJQ49$r9T$J$C$?$H$-$KEPO?$7$?4XO"8l$,<-=q$K$"$l$P$=$l$rM%@h$7$F=PNO$9(B
;; $B$kC1=c$J3X=,8z2L$rDs6!$9$k%W%m%0%i%`$G$9!#(B
;;
;; $B@N(B SKK ML $B$GOCBj$K$J$C$?C18l$NB0@-$NJ]B8$N$?$a$K!"(Bskk-attr.el $B$r:n$j$^$7$?(B
;; $B$,!"5!G=$rM_D%$j$9$.$F$b$N$K$J$j$^$;$s$G$7$?!#D>A0$NJQ49$H$N4XO"@-$rJ]B8$9$k(B
;; $B$?$a$@$1$K5!G=$r9J$C$F:F9=@.$7$?$N$,$3$N%W%m%0%i%`$G$9!#(B

;; <How to work>
;;
;; XEmacs $B$G(B SKK $B$r%Q%C%1!<%8%$%s%9%H!<%k$7$?>l9g$O!"(B.emacs $B$K(B
;;
;;   (setq skk-search-end-function 'skk-study-search)
;;   (setq skk-update-end-function 'skk-study-update)
;;
;; $B$H=q$/$@$1$G==J,$G$9!#$=$l0J30$NJ}$O!"(B
;;
;;   (add-hook 'skk-load-hook (function (lambda () (require 'skk-study))))
;;
;; $B$J$I$H=q$$$F2<$5$$!#(B

;; <DATA STRUCTURE (SKK-STUDY-ALIST)>
;;
;; ((okuri-ari . (("$B8=:_$N(B HENKAN-KEY" . ((("$BD>A0$N(B HENKAN-KEY" . "$BD>A0$N4A;z(B") . ("HENKAN-KEY $B$r8+=P$78l$H$9$k4XO"8l(B" ...))
;;                                        ...))))
;;  (okuri-nasi . (("$B8=:_$N(B HENKAN-KEY" . ((("$BD>A0$N(B HENKAN-KEY" . "$BD>A0$N4A;z(B") . ("HENKAN-KEY $B$r8+=P$78l$H$9$k4XO"8l(B" ...))
;;                                         ...)))))
;;
;;  o examples
;;
;; ((okuri-ari .
;;           (("$B$-(Br" . ((("$B$U$/(B" . "$BI~(B") . ("$BCe(B"))
;;                      (("$B$-(B" . "$BLZ(B") . ("$B@Z(B"))
;;                      (("$B$($s(B" . "$B1o(B") . ("$B@Z(B"))))
;;            ("$B$J(Bk" . ((("$B$3$I$b(B" . "$B;R6!(B") . ("$B5c(B"))
;;                      (("$B$3$H$j(B" . "$B>.D;(B") . ("$BLD(B"))))
;;            ("$B$+(Bk" . ((("$B$+$_(B" . "$B;f(B") . ("$B=q(B")) (("$B$R$s$+$/(B" . "$BIJ3J(B") . ("$B7g(B")))))
;;           ...)
;;  (okuri-nasi .
;;            (("$B$+$_(B" . ((("$B$-(Br" . "$B@Z(B") . ("$B;f(B"))))
;;             ...)))
;;
;; <TODO>
;;

;;; Code:

(eval-when-compile
  (require 'cl)
  (require 'ring)
  (require 'skk-macs)
  (require 'skk-vars))

;;;###autoload
(defgroup skk-study nil "SKK study related customization."
  :prefix "skk-study-"
  :group 'skk)

;;; user variables.
(defcustom skk-study-file (convert-standard-filename
			   (cond ((eq system-type 'ms-dos)
				  "~/_skkst")
				 (t
				  "~/.skk-study")))
  "*$B3X=,7k2L$rJ]B8$9$k%U%!%$%k!#(B"
  :type 'file
  :group 'skk-study)

(defcustom skk-study-backup-file (convert-standard-filename
				  (cond ((eq system-type 'ms-dos)
					 "~/_skkstbk")
					(t
					 "~/.skk-study.BAK")))
  "*$B3X=,7k2L$rJ]B8$9$k%P%C%/%"%C%W%U%!%$%k!#(B"
  :type 'file
  :group 'skk-study)

(defcustom skk-study-associates-number 3
  "*$BJ]B8$9$k4XO"8l$N?t!#(B"
  :type 'integer
  :group 'skk-study)

(defcustom skk-study-sort-saving t
  "*Non-nil $B$G$"$l$P3X=,7k2L$r%=!<%H$7$F%;!<%V$9$k!#(B"
  :type 'boolean
  :group 'skk-study)

(defcustom skk-study-check-alist-format t
  "*Non-nil $B$G$"$l$P!"3X=,7k2L$NFI$_9~$_;~$KO"A[%j%9%H$N%U%)!<%^%C%H$r%A%'%C%/$9$k!#(B"
  :type 'boolean
  :group 'skk-study)
	 
(defcustom skk-study-search-times 3
  "*$B8=:_$NJQ49%-!<$KBP$9$k4XO"JQ49%-!<$r$$$/$D$^$GAL$C$F8!:w$9$k$+!#(B"
  :type 'integer
  :group 'skk-study)

;;; system internal variables and constants.
;; global variable
(defconst skk-search-end-function 'skk-study-search)
(defconst skk-update-end-function 'skk-study-update)

(defconst skk-study-file-format-version "0.2")
(defvar skk-kakutei-end-function nil)
(defvar skk-study-alist nil)
(defvar skk-study-data-ring nil
  "ring.el $B$r;H$C$?D>A0$N(B skk-study-search-times $B8DJ,$NJQ49%-!<$H3NDj8l%G!<%?!#(B
$B6qBNE*$K$O!"2<5-$N$h$&$J9=B$$K$J$C$F$$$k!#(B

\(2 3 . [\(\"$B$3$&$>$&(B\" . \"$B9=B$(B\"\) \(\"$B$0$?$$$F$-(B\" . \"$B6qBNE*(B\"\) \(\"$B$+$-(B\" . \"$B2<5-(B\"\)]\)")

(defvar skk-study-last-save nil)
(defvar skk-study-last-read nil)

;;;; inline functions.
(defsubst skk-study-get-last-henkan-data (index)
  (and (> (ring-length skk-study-data-ring) index)
       (ring-ref skk-study-data-ring index)))

;;;###autoload
(defun skk-study-search (henkan-buffer midasi okurigana entry)
  (or skk-study-data-ring 
      (setq skk-study-data-ring (make-ring skk-study-search-times)))
  (if (or (null entry)
	  ;; list of single element.
	  (null (cdr entry)))
      nil
    (with-current-buffer henkan-buffer
      (if (or skk-study-alist (skk-study-read))
	  ;; (("$B$-(Br" . ((("$B$U$/(B" . "$BI~(B") . ("$BCe(B")) (("$B$-(B" . "$BLZ(B") . ("$B@Z(B"))))
	  ;;  ("$B$J(Bk" . ((("$B$3$I$b(B" . "$B;R6!(B") . ("$B5c(B")))))
	    
	  (let ((target-alist
		 (cdr (assoc midasi
			     (cdr (assq (cond ((or skk-okuri-char skk-henkan-okurigana)
					       'okuri-ari)
					      (t 'okuri-nasi))
					skk-study-alist))))))
	    (if target-alist
		(setq entry (skk-study-search-1 target-alist midasi okurigana entry)))))))
  entry)

(defun skk-study-search-1 (target-alist midasi okurigana entry)
  (do ((index 0 (1+ index))
       (times skk-study-search-times (1- times))
       last-data associates e modified)
      ((or modified (= times 0)) entry)
    (and 
     (setq last-data (skk-study-get-last-henkan-data index))
     ;; ((("$B$U$/(B" . "$BI~(B") . ("$BCe(B")) (("$B$-(B" . "$BLZ(B") . ("$B@Z(B")))
     ;; ("$BCe(B")
     (setq associates (cdr (assoc last-data target-alist)))
     (setq associates (reverse associates))
     (setq modified t)
     (while (setq e (car associates))
       (setq entry (cons e (delete e entry))
	     associates (cdr associates))))))

;;;###autoload
(defun skk-study-update (henkan-buffer midasi okurigana word purge)
  (or skk-study-data-ring 
      (setq skk-study-data-ring (make-ring skk-study-search-times)))
  (with-current-buffer henkan-buffer
    (let ((inhibit-quit t)
	  (last-data (if (not (ring-empty-p skk-study-data-ring))
			 (ring-ref skk-study-data-ring 0)))
	  grandpa papa baby)
      (if (and (or skk-study-alist (skk-study-read))
	       midasi word last-data
	       (not (or (string= midasi "") (string= word "")
			(and (string= midasi (car last-data))
			     (string= word (cdr last-data))))))
	  (progn
	    (setq grandpa (assq (cond ((or skk-okuri-char skk-henkan-okurigana)
				       'okuri-ari)
				      (t 'okuri-nasi))
				skk-study-alist)
		  ;; ((("$B$U$/(B" . "$BI~(B") . ("$BCe(B")) (("$B$-(B" . "$BLZ(B") . ("$B@Z(B")))
		  papa (assoc midasi (cdr grandpa)))
	    (cond (
		   ;; car $B$K8+=P$78l$r;}$D(B cell $B$,$J$$(B
		   (not (or papa purge))
		   (setcdr grandpa
			   (nconc
			    (list (cons midasi (list (cons last-data (list word)))))
			    (cdr grandpa))))
		  ;; $B8+=P$78l$+$i;O$^$k(B cell $B$O$"$k$,!"(Bcdr $B$K(B (last-key . last-word) $B$r(B
		  ;; $B%-!<$K$7$?(B cell $B$,$J$$!#(B
		  ((not (or
			 ;; (("$B$U$/(B" . "$BI~(B") . ("$BCe(B"))
			 (setq baby (assoc last-data (cdr papa)))
			 purge))
		   (setcdr papa (cons (cons last-data (list word)) (cdr papa))))
		  ;; $B8+=P$78l$r%-!<$H$7$?4{B8$N(B cell $B9=B$$,$G$-$"$,$C$F$$$k$N$G!"4XO"8l$@$1(B
		  ;; $B%"%C%W%G!<%H$9$k!#(B
		  ((not purge)
		   ;; ring $B%G!<%?$NJ}$,$b$C$H8zN(E*$+!)(B  $B$G$b$3$3$NItJ,$N%G!<%?$N%"%C%W%G!<%H(B
		   ;; $B$,8zN(NI$/$G$-$J$$!#(B
		   (setcdr baby (cons word (delete word (cdr baby))))
		   (if (> (1- (length (cdr baby))) skk-study-associates-number)
		       (skk-study-chomp (cdr baby) (1- skk-study-associates-number))))
		  (t (setcdr grandpa (delq baby (cdr grandpa))))))))))

;;;###autoload
(defun skk-study-save (&optional nomsg)
  "skk-study-file $B$K3X=,7k2L$rJ]B8$9$k(B."
  (interactive "P")
  (let ((inhibit-quit t)
	(last-time
	 (nth 5 (file-attributes (expand-file-name skk-study-file))))
	e)
    (if (or (and (null skk-study-alist) (not nomsg))
	    (not skk-study-last-read)
	    (and skk-study-last-save 
		 (skk-study-time-lessp
		  skk-study-last-read skk-study-last-save)))
	(progn
	  (skk-message "SKK $B$N3X=,7k2L$r%;!<%V$9$kI,MW$O$"$j$^$;$s(B"
		       "No SKK study need saving")
	  (sit-for 1))
      (if (not nomsg)
	  (skk-message "%s $B$K(B SKK $B$N3X=,7k2L$r%;!<%V$7$F$$$^$9(B..."
		       "Saving SKK study to %s..." skk-study-file))
      (and skk-study-backup-file
	   (file-exists-p (expand-file-name skk-study-file))
	   (cond ((eq system-type 'ms-dos)
		  (with-temp-file skk-study-backup-file
		    (erase-buffer)
		    (insert-file-contents skk-study-file)))
		 (t
		  (copy-file (expand-file-name skk-study-file)
			     (expand-file-name skk-study-backup-file)
			     'ok-if-already-exists 'keep-date))))
      (with-temp-buffer
	(insert
	 (format ";;; skk-study-file format version %s\n"
		 skk-study-file-format-version))
	(if (not skk-study-sort-saving)
	    nil
	  ;; sort is not necessary, but make an alist rather readable.
	  (setq e (assq 'okuri-ari skk-study-alist))
	  (setcdr e (sort (cdr e) (function (lambda (a b) (string< (car a) (car b))))))
	  (setq e (assq 'okuri-nasi skk-study-alist))
	  (setcdr e (sort (cdr e) (function (lambda (a b) (string< (car a) (car b)))))))
	(skk-study-prin1 skk-study-alist (current-buffer))
	(write-region-as-coding-system
	 (skk-find-coding-system skk-jisyo-code)
	 (point-min) (point-max) skk-study-file))
      (setq skk-study-last-save (current-time))
      (if (not nomsg)
	  (progn
	    (skk-message "%s $B$K(B SKK $B3X=,7k2L$r%;!<%V$7$F$$$^$9(B...$B40N;!*(B"
			 "Saving SKK study to %s...done" skk-study-file)
	    (sit-for 1)
	    (message ""))))))

;;;###autoload
(defun skk-study-read (&optional nomsg force)
  "skk-study-file $B$+$i3X=,7k2L$rFI$_9~$`!#(B
$B%*%W%7%g%J%k0z?t$N(B FORCE $B$,(B non-nil $B$G$"$l$P!"GK4~$N3NG'$r$7$J$$!#(B"
  (interactive "P")
  (skk-create-file
   skk-study-file
   (if (not nomsg)
       (if skk-japanese-message-and-error
	   "SKK $B$N3X=,7k2L%U%!%$%k$r:n$j$^$7$?(B"
	 "I have created an SKK study file for you")))
  (if (or (null skk-study-alist)
	  force
	  (skk-yes-or-no-p (format "%s $B$r:FFI$_9~$_$7$^$9$+!)(B" skk-study-file)
			   (format "Reread %s?" skk-study-file)))
      (progn
	(or nomsg
	    (skk-message "%s $B$N(B SKK $B3X=,7k2L$rE83+$7$F$$$^$9(B..."
			 "Expanding SKK study of %s ..."
			 (file-name-nondirectory skk-study-file)))
	;; $B0BDj$7$?$i%G%#%U%)%k%H$r(B nil $B$K$9$k$M!#(B
	(if skk-study-check-alist-format
	    (skk-study-check-alist-format skk-study-file))
	(setq skk-study-alist (skk-study-read-1 skk-study-file))
	(setq skk-study-last-read (current-time))
	(if (null skk-study-alist)
	    nil
	  (or nomsg
	      (progn
		(skk-message
		 "%s $B$N(B SKK $B3X=,7k2L$rE83+$7$F$$$^$9(B...$B40N;!*(B"
		 "Expanding SKK study of %s ...done"
		 (file-name-nondirectory skk-study-file))
		(sit-for 1)
		(message "")))))))

(defun skk-study-read-1 (file)
  ;; read FILE and return alist.
  (with-temp-buffer
    (let ((version-string
	   (format ";;; skk-study-file format version %s\n"
		   skk-study-file-format-version)))
      (insert-file-contents-as-coding-system (skk-find-coding-system skk-jisyo-code) file)
      (if (= (buffer-size) 0)
	  ;; bare alist
	  (insert version-string "((okuri-ari) (okuri-nasi))"))
      (goto-char (point-min))
      (if (looking-at (regexp-quote version-string))
	  (read (current-buffer))
	(skk-error
	 "skk-study-file $B%U%)!<%^%C%H$N%P!<%8%g%s$,0lCW$7$^$;$s(B"
	 "skk-study-file format version is inconsistent")))))

(defun skk-study-check-alist-format (alist-file)
  "ALIST-FILE $B$NO"A[%j%9%H$N%U%)!<%^%C%H$r%A%'%C%/$9$k!#(B"
  (interactive
   (list (read-file-name
	  (format "Alist file to check: (default: %s) " skk-study-file)
	  default-directory skk-study-file)))
  (skk-message "%s $B%U%!%$%k$NO"A[%j%9%H$N%U%)!<%^%C%H%A%'%C%/$r9T$J$C$F$$$^$9(B..."
	       "Checking %s file alist format..." alist-file)
  (or (skk-study-check-alist-format-1 (skk-study-read-1 alist-file))
      (skk-error "%s $B$NO"A[%j%9%H$N%U%)!<%^%C%H$O2u$l$F$$$^$9(B"
		 "%s alist format is corrupt" alist-file))
  (skk-message
   "%s $B%U%!%$%k$NO"A[%j%9%H$N%U%)!<%^%C%H%A%'%C%/$r9T$J$C$F$$$^$9(B...$B40N;(B!"
   "Checking %s file alist format... done" alist-file)
  (sit-for 1)
  (message ""))

(defun skk-study-check-alist-format-1 (alist)
  (if (not (and (= (length alist) 2) (assq 'okuri-ari alist)
		(assq 'okuri-nasi alist)))
      nil
    (catch 'exit
      (let ((index '(okuri-ari okuri-nasi))
	    (func (function
		   (lambda (str)
		     (let ((len (skk-str-length str)))
		       (and
			(> len 1)
			(skk-ascii-char-p (skk-str-ref str (1- len))))))))
	    alist2 e f)
	(while index
	  (and (eq (car index) 'okuri-nasi)
	       (setq func
		     (function
		      (lambda (str)
			(let ((len (skk-str-length str)))
			  (cond ((= len 1))
				((not (skk-ascii-char-p (skk-str-ref str (1- len)))))
				((skk-ascii-char-p (skk-str-ref str (- len 2))))))))))
	  (setq alist2 (cdr (assq (car index) alist)))
	  (while alist2
	    (setq e (car alist2))
	    (or (funcall func (car e))
		;; $B8+=P$78l$N%A%'%C%/(B
		(throw 'exit nil))
	    (setq f (cdr e))
	    (while f
	      (if (not (and
			;; $BD>A0$NJQ49$N>pJs(B
			(consp (car (car f)))
			;; $B4XO"8l%j%9%H(B
			(listp (cdr (car f)))))
		  (throw 'exit nil))
	      (setq f (cdr f)))
	    (setq alist2 (cdr alist2)))
	  (setq index (cdr index)))
	t))))

(defun skk-study-prin1 (form &optional stream)
  (let ((print-readably t)
	print-level print-length print-quoted)
    (prin1 form stream)))

(defun skk-study-chomp (nth list)
  ;; LIST := '(A B C D), NTH := 1
  ;; -> '(A B)
  (and (> nth -1) (setcdr (nthcdr nth list) nil))
  list)

(defadvice skk-kakutei-initialize (before skk-study-ad activate)
  (let ((kakutei-word (ad-get-arg 0))
	(count 0) data max vector)
    (when kakutei-word
      (setq data (cons skk-henkan-key kakutei-word))
      (setq vector (nthcdr 2 skk-study-data-ring))
      (setq max (length vector))
      (catch 'exit
	(while (> max count)
	  (and (equal (aref vector count) data)
	       (throw 'exit nil))
	  (setq count (1+ count)))
	(ring-insert skk-study-data-ring data)))))
   
(defadvice skk-undo-kakutei (after skk-study-ad activate)
  (let ((last (ring-ref skk-study-data-ring 0))
	(last2 (ring-ref skk-study-data-ring 1))
	target)
    (if (and last last2)
	(progn
	  (setq target (assoc (car last)
			      (assq (cond ((skk-get-last-henkan-datum 'okuri-char)
					   'okuri-ari)
					  (t 'okuri-nasi))
				    skk-study-alist)))
	  (setq target (delq (assoc last2 (cdr target)) target))))))

;; time utilities...
;;  from ls-lisp.el.  Welcome!
(defun skk-study-time-lessp (time0 time1)
  (let ((hi0 (car time0))
	(hi1 (car time1))
	(lo0 (nth 1 time0))
	(lo1 (nth 1 time1)))
    (or (< hi0 hi1) (and (= hi0 hi1) (< lo0 lo1)))))

(add-hook 'skk-before-kill-emacs-hook 'skk-study-save)

(require 'product)
(product-provide (provide 'skk-study) (require 'skk-version))
;;; Local Variables:
;;; End:
;;; skk-study.el ends here
