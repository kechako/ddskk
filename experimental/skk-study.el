;; -*-byte-compile-dynamic: t;-*-
;; -*- byte-compile-dynamic-docstring: t;-*-
;;; skk-study.el --- SKK $B3X=,8z2LDs6!%W%m%0%i%`(B
;; Copyright (C) 1999 Mikio Nakajima <minakaji@osaka.email.ne.jp>

;; Author: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Maintainer: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Version: $Id: skk-study.el,v 1.10 1999/10/10 02:24:36 minakaji Exp $
;; Keywords: japanese
;; Created: Apr. 11, 1999
;; Last Modified: $Date: 1999/10/10 02:24:36 $

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

;;; Commentary:
;;
;; $B$"$kC18l$rJQ49$7$?>l9g$K!"D>A0$KJQ49$7$?8l$r4XO"8l$H$7$FEPO?$7$F$*$-!":FEY$=(B
;; $B$NC18l$NJQ49$r9T$J$C$?$H$-$KEPO?$7$?4XO"8l$,<-=q$K$"$l$P$=$l$rM%@h$7$F=PNO$9(B
;; $B$kC1=c$J3X=,8z2L$rDs6!$9$k%W%m%0%i%`$G$9!#(B
;;
;; $B@N(B SKK ML $B$GOCBj$K$J$C$?C18l$NB0@-$NJ]B8$N$?$a$K!"(Bskk-attr.el $B$r:n$j$^$7$?(B
;; $B$,!"5!G=$rM_D%$j$9$.$F$b$N$K$J$j$^$;$s$G$7$?!#D>A0$NJQ49$H$N4XO"@-$rJ]B8$9$k$?(B
;; $B$a$@$1$K5!G=$r9J$C$F:F9=@.$7$?$N$,$3$N%W%m%0%i%`$G$9!#(B

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
;;                                        ... ))))
;;  (okuri-nasi . (("$B8=:_$N(B HENKAN-KEY" . ((("$BD>A0$N(B HENKAN-KEY" . "$BD>A0$N4A;z(B") . ("HENKAN-KEY $B$r8+=P$78l$H$9$k4XO"8l(B" ...))
;;                                         ... )))))
;;
;;  o examples
;;
;; ((okuri-ari .
;;           (("$B$-(Br" . ((("$B$U$/(B" . "$BI~(B") . ("$BCe(B"))
;;                      (("$B$-(B" . "$BLZ(B") . ("$B@Z(B"))
;;                      (("$B$($s(B" . "$B1o(B") . ("$B@Z(B")) ))
;;            ("$B$J(Bk" . ((("$B$3$I$b(B" . "$B;R6!(B") . ("$B5c(B"))
;;                      (("$B$3$H$j(B" . "$B>.D;(B") . ("$BLD(B")) ))
;;            ("$B$+(Bk" . ((("$B$+$_(B" . "$B;f(B") . ("$B=q(B")) (("$B$R$s$+$/(B" . "$BIJ3J(B") . ("$B7g(B")))) )
;;           ... )
;;  (okuri-nasi .
;;            (("$B$+$_(B" . ((("$B$-(Br" . "$B@Z(B") . ("$B;f(B"))))
;;             ... )))
;;
;; <TODO>
;;

;;; Code:
(eval-when-compile (require 'cl) (require 'ring) (require 'skk))
(require 'skk-foreword)

;;;###autoload
(defgroup skk-study nil "SKK study related customization."
  :prefix "skk-study-"
  :group 'skk )

;;; user variables.
(defcustom skk-study-file (convert-standard-filename "~/.skk-study")
  "*$B3X=,7k2L$rJ]B8$9$k%U%!%$%k!#(B"
  :type 'file
  :group 'skk-study )

(defcustom skk-study-backup-file (convert-standard-filename "~/.skk-study.BAK" )
  "*$B3X=,7k2L$rJ]B8$9$k%P%C%/%"%C%W%U%!%$%k!#(B"
  :type 'file
  :group 'skk-study )

(defcustom skk-study-associates-number 3
  "*$BJ]B8$9$k4XO"8l$N?t!#(B"
  :type 'integer
  :group 'skk-study )

(defcustom skk-study-sort-saving t
  "*Non-nil $B$G$"$l$P3X=,7k2L$r%=!<%H$7$F%;!<%V$9$k!#(B"
  :type 'boolean
  :group 'skk-study )

(defcustom skk-study-check-alist-format t
  "*Non-nil $B$G$"$l$P!"3X=,7k2L$NFI$_9~$_;~$KO"A[%j%9%H$N%U%)!<%^%C%H$r%A%'%C%/$9$k!#(B"
  :type 'boolean
  :group 'skk-study )
	 
(defcustom skk-study-search-times 3
  "*"
  :type 'integer
  :group 'skk-study )

;;; system internal variables and constants.
;; global variable
(defconst skk-study-file-format-version 0.2)

(defvar skk-kakutei-end-function nil)
(defvar skk-study-data-ring (make-ring skk-study-search-times))

(defvar skk-study-alist nil)
(defvar skk-search-end-function 'skk-study-search)
(defvar skk-update-end-function 'skk-study-update)

;;;; inline functions.
(defsubst skk-study-get-last-henkan-data (index)
  (and (> (ring-length skk-study-data-ring) index)
       (ring-ref skk-study-data-ring index) ))

;;;###autoload
(defun skk-study-search (henkan-buffer midasi okurigana entry)
  (if entry
      (with-current-buffer henkan-buffer
	(if (or skk-study-alist (skk-study-read))
	    ;; (("$B$-(Br" . ((("$B$U$/(B" . "$BI~(B") . ("$BCe(B")) (("$B$-(B" . "$BLZ(B") . ("$B@Z(B"))))
	    ;;  ("$B$J(Bk" . ((("$B$3$I$b(B" . "$B;R6!(B") . ("$B5c(B")))) )
	    
	    (let ((target-alist
		   (cdr (assoc
			 midasi
			 (cdr (assq
			       (cond ((or skk-okuri-char
					  ;; skk-okuri-char is nil, but 
					  ;; skk-henkan-okurigana is non-nil
					  ;; when skk-auto.el has processed.
					  skk-henkan-okurigana )
				      'okuri-ari )
				     (t 'okuri-nasi) )
			       skk-study-alist ))))))
	      (if target-alist
		  (skk-study-search-1 target-alist midasi okurigana entry) ))))))

(defun skk-study-search-1 (target-alist midasi okurigana entry)
  (do* ((orgentry (copy-list entry))
	(index 0 (1+ index))
	(last-data (skk-study-get-last-henkan-data index)
		   (skk-study-get-last-henkan-data index) )
	(last-key (car last-data))
	(last-word (cdr last-data))
	(times skk-study-search-times (1- times))
	associates e )
      ((or (not last-data) (= times 0) (not (equal orgentry entry)))
       entry )
    (and 
     ;; ((("$B$U$/(B" . "$BI~(B") . ("$BCe(B")) (("$B$-(B" . "$BLZ(B") . ("$B@Z(B")))
     ;; ("$BCe(B")
     (setq associates (assoc (cons last-key last-word) target-alist))
     (setq associates (nreverse associates))
     (while (setq e (car associates))
       (setq entry (cons e (delete e entry))
	     associates (cdr associates) )))))

;;;###autoload
(defun skk-study-update (henkan-buffer midasi okurigana word purge)
  (with-current-buffer henkan-buffer
    (let ((inhibit-quit t)
	  (last-key (skk-get-last-henkan-data 'henkan-key))
	  (last-word (car (skk-get-last-henkan-data 'henkan-list)))
	  grandpa papa)
      (if (and (or skk-study-alist (skk-study-read))
	       last-key last-word 
	       (not (and (string= midasi last-key) (string= word last-word))) )
	  (progn
	    (setq grandpa (assq (cond ((or skk-okuri-char
					   ;; skk-okuri-char is nil, but 
					   ;; skk-henkan-okurigana is non-nil when
					   ;; skk-auto.el has processed.
					   skk-henkan-okurigana )
				       'okuri-ari )
				      (t 'okuri-nasi) )
				skk-study-alist )
		  ;; ((("$B$U$/(B" . "$BI~(B") . ("$BCe(B")) (("$B$-(B" . "$BLZ(B") . ("$B@Z(B")))
		  papa (assoc midasi (cdr grandpa)) )
	    (cond (
		   ;; car $B$K8+=P$78l$r;}$D(B cell $B$,$J$$(B
		   (not (or papa purge))
		   (setcdr grandpa
			   (nconc
			    (list (cons midasi (list (cons (cons last-key last-word)
							   (list word) ))))
			    (cdr grandpa) )))
		  ;; $B8+=P$78l$+$i;O$^$k(B cell $B$O$"$k$,!"(Bcdr $B$K(B (last-key . last-word) $B$r(B
		  ;; $B%-!<$K$7$?(B cell $B$,$J$$!#(B
		  ((not (or
			 ;; (("$B$U$/(B" . "$BI~(B") . ("$BCe(B"))
			 (setq baby (assoc (cons last-key last-word) (cdr papa)))
			 purge ))
		   (setcdr papa (cons (cons (cons last-key last-word) (list word))
				      (cdr papa) )))
		  ;; $B8+=P$78l$r%-!<$H$7$?4{B8$N(B cell $B9=B$$,$G$-$"$,$C$F$$$k$N$G!"4XO"8l$@$1(B
		  ;; $B%"%C%W%G!<%H$9$k!#(B
		  ((not purge)
		   (setcdr baby (cons word (delete word (cdr baby))))
		   (if (> (1- (length (cdr baby))) skk-study-associates-number)
		       (skk-study-chomp (cdr baby) (1- skk-study-associates-number)) ))
		  (t (setcdr grandpa (delq baby (cdr grandpa)))) ))))))

;;;###autoload
(defun skk-study-save (&optional nomsg)
  "skk-study-file $B$K3X=,7k2L$rJ]B8$9$k(B."
  (interactive "P")
  (let ((inhibit-quit t)
	e )
    (if (and (null skk-study-alist) (not nomsg))
	(progn
	  (skk-message "SKK $B$N3X=,7k2L$r%;!<%V$9$kI,MW$O$"$j$^$;$s(B"
		       "No SKK study need saving" )
	  (sit-for 1) )
      (if (not nomsg)
	  (skk-message "%s $B$K(B SKK $B$N3X=,7k2L$r%;!<%V$7$F$$$^$9(B..."
		       "Saving SKK study to %s..." skk-study-file ))
      (and skk-study-backup-file
	   (file-exists-p (expand-file-name skk-study-file))
	   (copy-file (expand-file-name skk-study-file)
		      (expand-file-name skk-study-backup-file)
		      'ok-if-already-exists 'keep-date ))
      (with-temp-buffer
	(insert
	 (format ";;; skk-study-file format version %s\n"
		 skk-study-file-format-version ))
	(if (not skk-study-sort-saving)
	    nil
	  ;; sort is not necessary, but make an alist rather readable.
	  (setq e (assq 'okuri-ari skk-study-alist))
	  (setcdr e (sort (cdr e) (function (lambda (a b) (string< (car a) (car b))))))
	  (setq e (assq 'okuri-nasi skk-study-alist))
	  (setcdr e (sort (cdr e) (function (lambda (a b) (string< (car a) (car b)))))) )
	(skk-study-prin1 skk-study-alist (current-buffer))
	(write-region-as-coding-system
	 (cond ((and skk-jisyo-code (coding-system-p skk-jisyo-code))
		skk-jisyo-code )
	       ((and skk-jisyo-code (stringp skk-jisyo-code))
		(cdr (assoc skk-jisyo-code skk-coding-system-alist)) )
	       (t (cdr (assoc "euc" skk-coding-system-alist))) )
	 (point-min) (point-max) skk-study-file ))
      (if (not nomsg)
	  (progn
	    (skk-message "%s $B$K(B SKK $B3X=,7k2L$r%;!<%V$7$F$$$^$9(B...$B40N;!*(B"
			 "Saving SKK study to %s...done" skk-study-file )
	    (sit-for 1)
	    (message "") )))))

;;;###autoload
(defun skk-study-read (&optional nomsg)
  "skk-study-file $B$+$i3X=,7k2L$rFI$_9~$`!#(B"
  (interactive "P")
  (skk-create-file
   skk-study-file
   (if (not nomsg)
       (if skk-japanese-message-and-error
	   "SKK $B$N3X=,7k2L%U%!%$%k$r:n$j$^$7$?(B"
	 "I have created an SKK study file for you" )))
  (if (or (null skk-study-alist)
	  (skk-yes-or-no-p (format "%s $B$r:FFI$_9~$_$7$^$9$+!)(B" skk-study-file)
			   (format "Reread %s?" skk-study-file) ))
      (progn
	(or nomsg
	    (skk-message "%s $B$N(B SKK $B3X=,7k2L$rE83+$7$F$$$^$9(B..."
			 "Expanding SKK study of %s ..."
			 (file-name-nondirectory skk-study-file) ))
	;; $B0BDj$7$?$i%G%#%U%)%k%H$r(B nil $B$K$9$k$M!#(B
	(if skk-study-check-alist-format
	    (skk-study-check-alist-format skk-study-file) )
	(setq skk-study-alist (skk-study-read-1 skk-study-file))
	(if (null skk-study-alist)
	    nil
	  (or nomsg
	      (progn
		(skk-message
		 "%s $B$N(B SKK $B3X=,7k2L$rE83+$7$F$$$^$9(B...$B40N;!*(B"
		 "Expanding SKK study of %s ...done"
		 (file-name-nondirectory skk-study-file) )
		(sit-for 1)
		(message "") ))))))

(defun skk-study-read-1 (file)
  ;; read FILE and return alist.
  (with-temp-buffer
    (let ((version-string
	   (format ";;; skk-study-file format version %s\n"
		   skk-study-file-format-version )))
      (insert-file-contents-as-coding-system
       (cond ((and skk-jisyo-code
		   (or (coding-system-p skk-jisyo-code)
		       (and (fboundp 'find-coding-system)
			    (find-coding-system skk-jisyo-code) )))
	      skk-jisyo-code )
	     ((and skk-jisyo-code (stringp skk-jisyo-code))
	      (cdr (assoc skk-jisyo-code skk-coding-system-alist)) )
	     (t (cdr (assoc "euc" skk-coding-system-alist))) )
       file )
      (if (= (buffer-size) 0)
	  ;; bare alist
	  (insert version-string "((okuri-ari) (okuri-nasi))") )
      (goto-char (point-min))
      (if (looking-at (regexp-quote version-string))
	  (read (current-buffer))
	(let ((old-version-string
	       (format
		";;; skk-study-file format version %s\n"
		(- skk-study-file-format-version 0.1) ))
	      (skk-study-sort-saving t) )
	  (cond ((and (looking-at (regexp-quote old-version-string))
		      (skk-yes-or-no-p
		       "skk-study-file $B%U%)!<%^%C%H$N%P!<%8%g%s%"%C%W$r9T$J$$$^$9$+!)(B "
		       "Do you want to make skk-study-file format version up? " ))
		 (prog1
		     (setq skk-study-alist
			   (skk-study-convert-alist-format (read (current-buffer))) )
		   (skk-study-save 'nomsg) ))
		((skk-error
		  "skk-study-file $B%U%)!<%^%C%H$N%P!<%8%g%s$,0lCW$7$^$;$s(B"
		  "skk-study-file format version is inconsistent" ))))))))

;;;###autoload
(defun skk-study-check-alist-format (alist-file)
  "ALIST-FILE $B$NO"A[%j%9%H$N%U%)!<%^%C%H$r%A%'%C%/$9$k!#(B"
  (interactive
   (list (read-file-name
	  (format "Alist file to check: (default: %s) " skk-study-file)
	  default-directory skk-study-file )))
  (skk-message "%s $B%U%!%$%k$NO"A[%j%9%H$N%U%)!<%^%C%H%A%'%C%/$r9T$J$C$F$$$^$9(B..."
	       "Checking %s file alist format..." alist-file )
  (or (skk-study-check-alist-format-1 (skk-study-read-1 alist-file))
      (skk-error "%s $B$NO"A[%j%9%H$N%U%)!<%^%C%H$O2u$l$F$$$^$9(B"
		 "%s alist format is corrupt" alist-file ))
  (skk-message
   "%s $B%U%!%$%k$NO"A[%j%9%H$N%U%)!<%^%C%H%A%'%C%/$r9T$J$C$F$$$^$9(B...$B40N;(B!"
   "Checking %s file alist format... done" alist-file )
  (sit-for 1)
  (message "") )

(defun skk-study-check-alist-format-1 (alist)
  (if (not (and (= (length alist) 2) (assq 'okuri-ari alist)
		(assq 'okuri-nasi alist) ))
      nil
    (catch 'exit
      (let ((index '(okuri-ari okuri-nasi))
	    (func (function
		   (lambda (str)
		     (let ((len (length str)))
		       (and
			(> len 1)
			(skk-ascii-char-p (skk-str-ref str (1- len))) )))))
	    alist2 e f )
	(while index
	  (and (eq (car index) 'okuri-nasi)
	       ;;(setcdr (nthcdr 1 (nth 2 func))
	       ;;        (list (cons 'not (cdr (nthcdr 1 (nth 2 func))))) )))
	       (setq func
		     (function
		      (lambda (str)
			(let ((len (length str)))
			  (or (= len 1)
			      (not (skk-ascii-char-p (skk-str-ref str (1- len)))) ))))))
	  (setq alist2 (cdr (assq (car index) alist)))
	  (while alist2
	    (setq e (car alist2))
	    (or (funcall func (car e))
		;; $B8+=P$78l$N%A%'%C%/(B
		(throw 'exit nil) )
	    (setq f (cdr e))
	    (while f
	      (if (not (and
			;; $BD>A0$NJQ49$N>pJs(B
			(consp (car (car f)))
			;; $B4XO"8l%j%9%H(B
			(listp (cdr (car f))) ))
		  (throw 'exit nil) )
	      (setq f (cdr f)) )
	    (setq alist2 (cdr alist2)) )
	  (setq index (cdr index)) )
	t ))))

(defun skk-study-convert-alist-format (alist)
  ;; convert format version 0.1 to 0.2.
  (let ((inhibit-quit t)
	(base '((okuri-ari) (okuri-nasi)))
	e len )
    (while alist
      (setq e (car alist)
	    len (length (car e)) )
      (if (and (> len 1) (skk-ascii-char-p (skk-str-ref (car e) (1- len))))
	  ;; okuri-ari
	  (setcdr (car base) (cons e (cdr (car base))))
	;; okuri-nasi
	(setcdr (car (cdr base)) (cons e (cdr (car (cdr base))))) )
      (setq alist (cdr alist)) )
    (and (skk-study-check-alist-format-1 base)
	 base )))

(defun skk-study-prin1 (form &optional stream)
  (let ((print-readably t)
	print-level print-length print-quoted )
    (prin1 form stream) ))

(defun skk-study-chomp (nth list)
  ;; LIST := '(A B C D), NTH := 1
  ;; -> '(A B)
  (and (> nth -1) (setcdr (nthcdr nth list) nil))
  list )

(defadvice skk-kakutei-initialize (before skk-study-ad activate)
  (let ((kakutei-word (ad-get-arg 0))
	(count 0) data max vector )
    (when kakutei-word
      (setq data (cons skk-henkan-key kakutei-word))
      (setq vector (nthcdr 2 skk-study-data-ring))
      (setq max (length vector))
      (catch 'exit
	(while (> max count)
	  (and (equal (aref vector count) data)
	       (throw 'exit nil) )
	  (setq count (1+ count)) )
	(ring-insert skk-study-data-ring data) ))))
   
(add-hook 'skk-before-kill-emacs-hook 'skk-study-save)
(provide 'skk-study)
;;; Local Variables:
;;; End:
;;; skk-study.el ends here
