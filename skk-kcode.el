;;; skk-kcode.el --- $B4A;z%3!<%I$r;H$C$?JQ49$N$?$a$N%W%m%0%i%`(B
;; Copyright (C) 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998,
;;               1999
;; Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>

;; Author: Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>
;; Maintainer: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Version: $Id: skk-kcode.el,v 1.4 1999/09/24 22:43:26 minakaji Exp $
;; Keywords: japanese
;; Last Modified: $Date: 1999/09/24 22:43:26 $

;; This file is part of SKK.

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

;; Following people contributed modifications to skk.el (Alphabetical order):
;;       Kenji Yamashita <kenji@cs.titech.ac.jp>
;;       Murata Shuuichirou  <mrt@astec.co.jp>
;;       Shuhei KOBAYASHI <shuhei-k@jaist.ac.jp>
;;       $BDMK\(B $BE0M:(B <czkmt@remus.dti.ne.jp>


;;; Code:
(eval-when-compile (require 'skk))
(require 'skk-foreword)

(defgroup skk-kcode nil "SKK Kanji Code related customization."
  :prefix "skk-"
  :group 'skk )

(defcustom skk-input-by-code-menu-keys1 '(?a ?s ?d ?f ?g ?h ?q ?w ?e ?r ?t ?y)
  "*$B%a%K%e!<7A<0$G(B JIS $BJ8;z$rF~NO$9$k$H$-$K;HMQ$9$kA*Br%-!<$N%j%9%H!#(B
$BBh(B 1 $BCJ3,$N%a%K%e!<$G;HMQ$9$k!#(B
12 $B8D$N%-!<(B (char type) $B$r4^$`I,MW$,$"$k!#(B"
  :type '(repeat character)
  :group 'skk-kcode )

(defcustom skk-input-by-code-menu-keys2
  '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?q ?w ?e ?r ?t ?y ?u)
  "*$B%a%K%e!<7A<0$G(B JIS $BJ8;z$rF~NO$9$k$H$-$K;HMQ$9$kA*Br%-!<$N%j%9%H!#(B
$BBh(B 2 $BCJ3,$N%a%K%e!<$G;HMQ$9$k!#(B
16 $B8D$N%-!<(B (char type) $B$r4^$`I,MW$,$"$k!#(B"
  :type '(repeat character)
  :group 'skk-kcode )

(defcustom skk-kcode-charset
  (if (memq skk-emacs-type '(xemacs mule4 mule3))
      'japanese-jisx0208
    lc-jp )
  "*skk-input-by-code-or-menu $B$G;H$o$l$kJ8;z%;%C%H!#(B"
  :type 'symbol
  :group 'skk-kcode )

(defcustom skk-kcode-load-hook nil
  "*skk-kcode.el $B$r%m!<%I$7$?8e$K%3!<%k$5$l$k%U%C%/!#(B"
  :type 'hook
  :group 'skk-kcode )

;; internal constants and variables.
(defconst skk-code-n1-min 161)
(defconst skk-code-n1-max 244)
(defconst skk-code-n2-min 161)
(defconst skk-code-n2-max 254)
(defconst skk-code-null 128)
(defconst skk-kcode-charset-list
  (if (memq skk-emacs-type '(xemacs mule4 mule3))
      (mapcar '(lambda (x) (list (symbol-name x))) (charset-list)) ))
(defvar skk-input-by-code-or-menu-jump-default skk-code-n1-min)

;;;###autoload
(defun skk-input-by-code-or-menu (&optional arg)
  "7bit $B$b$7$/$O(B 8bit $B$b$7$/$O(B $B6hE@%3!<%I$KBP1~$9$k(B 2byte $BJ8;z$rA^F~$9$k!#(B"
  (interactive "*P")
  (if arg
      (let ((charset
	     (intern (completing-read (format "CHARSET(%s): " skk-kcode-charset)
				      skk-kcode-charset-list nil t ))))
	(cond ((eq charset (intern "")))
	      ((not (skk-charsetp charset))
	       (skk-error "$BL58z$J%-%c%i%/%?!<%;%C%H$G$9(B" "Invalid charset"))
	      (t (setq skk-kcode-charset charset)) )))
  (let ((str
	 (read-string
	  (format
	   "7/8 bits or KUTEN code for %s (00nn or CR for Jump Menu): "
	   skk-kcode-charset )))
	(enable-recursive-minibuffer t)
	n1 n2 )
    (if (string-match "\\(.+\\)-\\(.+\\)" str)
	(setq n1 (+ (string-to-number (match-string 1 str)) 32 128)
	      n2 (+ (string-to-number (match-string 2 str)) 32 128) )
      (setq n1 (if (string= str "") 128
		 (+ (* 16 (skk-char-to-hex (aref str 0) 'jis))
		    (skk-char-to-hex (aref str 1)) ))
	    n2 (if (string= str "") 128
		 (+ (* 16 (skk-char-to-hex (aref str 2) 'jis))
		    (skk-char-to-hex (aref str 3)) ))))
    (if (or (> n1 256) (> n2 256))
	(skk-error "$BL58z$J%3!<%I$G$9(B" "Invalid code") )
    (insert (if (> n1 160)
		(skk-make-string n1 n2)
	      (skk-input-by-code-or-menu-0 n1 n2) ))
    (if skk-henkan-active (skk-kakutei)) ))

(defun skk-char-to-hex (char &optional jischar)
  (cond ((> char 96) (- char 87)) ; a-f
        ((> char 64) (- char 55)) ; A-F
        ((> char 47) ; 0-9
	 (cond (jischar (- char 40)) (t (- char 48)) ))
        (t (skk-error "%c $B$r(B 6 $B?J?t$KJQ49$G$-$^$;$s(B"
		      "Cannot convert %c to hexadecimal number" ))))

(defun skk-make-string (n1 n2)
  (char-to-string (skk-make-char skk-kcode-charset n1 n2)) )

(defun skk-next-n2-code (n)
  (if (<= (setq n (1+ n)) skk-code-n2-max) n skk-code-n2-min))

(defun skk-previous-n2-code (n)
  (if (<= skk-code-n2-min (setq n (1- n))) n skk-code-n2-max))

(defun skk-next-n1-code (n)
  (if (<= (setq n (1+ n)) skk-code-n1-max) n skk-code-n1-min))

(defun skk-previous-n1-code (n)
  (if (<= skk-code-n1-min (setq n (1- n))) n skk-code-n1-max))

(defun skk-input-by-code-or-menu-0 (n1 n2)
  (if (= n1 skk-code-null)
      (skk-input-by-code-or-menu-jump n2)
    (skk-input-by-code-or-menu-1 n1 n2)))

(defun skk-input-by-code-or-menu-jump (n)
  (let ((menu-keys1 ; $BI=<(MQ$N%-!<%j%9%H$rAH$_N)$F$k!#(B
         (mapcar (function (lambda (char) (char-to-string (upcase char))))
                 skk-input-by-code-menu-keys1 ))
        kanji-char )
    (if (< n skk-code-n1-min) (setq n skk-input-by-code-or-menu-jump-default))
    (while (not kanji-char)
      (let ((n-org n)
            (chars
             (list
              (list (skk-make-string n skk-code-n1-min) n skk-code-n1-min)
              (list (skk-make-string n 177) n 177)
              (list (skk-make-string n 193) n 193)
              (list (skk-make-string n 209) n 209)
              (list (skk-make-string n 225) n 225)
              (list (skk-make-string n 241) n 241)
              (progn
                (setq n (skk-next-n1-code n))
                (list (skk-make-string n skk-code-n1-min) n
                      skk-code-n1-min ))
              (list (skk-make-string n 177) n 177)
              (list (skk-make-string n 193) n 193)
              (list (skk-make-string n 209) n 209)
              (list (skk-make-string n 225) n 225)
              (list (skk-make-string n 241) n 241))))
        (skk-save-point
          (let ((i 0) message-log-max str )
            (while (< i 12)
              (setq str (concat str (nth i menu-keys1) ":" (car (nth i chars))
                                "  " ))
              (setq i (1+ i)) )
            (message str) )
          (let ((char (event-to-character (skk-read-event)))
                rest ch )
            (if (not (characterp char))
                (progn
                  (skk-message "\"%s\" $B$OM-8z$J%-!<$G$O$"$j$^$;$s!*(B"
                               "\"%s\" is not valid here!" (prin1 char) )
                  (sit-for 1)
                  (message "")
                  (setq n n-org) )
              (setq rest (or (memq char skk-input-by-code-menu-keys1)
                             (if (skk-lower-case-p char)
                                 (memq (upcase char) skk-input-by-code-menu-keys1)
                               (memq (downcase char) skk-input-by-code-menu-keys1) ))
                    ch (if rest
                           ;; 12 == (length skk-input-by-code-menu-keys1)
                           (nth (- 12 (length rest)) chars)
                         nil )
                    kanji-char
                    (cond
                     (ch)
                     ((eq char ?x)
                      (if (< (setq n (- n-org 2)) skk-code-n1-min)
                          (setq n skk-code-n1-max))
                      nil)
                     ((eq char ?\040)
                      (setq n (skk-next-n1-code n))
                      nil)
                     ((eq char ?\?)
                      (skk-message
                       (concat "$B!X(B%s$B!Y(B  EUC: %2x%2x (%3d, %3d), JIS: %2x%2x (%3d, %3d)  "
                               "[$B2?$+%-!<$r2!$7$F$/$@$5$$(B]" )
                       (concat "$B!X(B%s$B!Y(B  EUC: %2x%2x (%3d, %3d), JIS: %2x%2x (%3d, %3d)  "
                               "[Hit any key to continue]" )
                       (car (car chars))
                       n-org skk-code-n1-min n-org skk-code-n1-min
                       (- n-org 128) (- skk-code-n1-min 128)
                       (- n-org 128) (- skk-code-n1-min 128) )
                      (skk-read-event)
                      (setq n n-org)
                      nil)
                     (t
                      (skk-message "\"%c\" $B$OM-8z$J%-!<$G$O$"$j$^$;$s!*(B"
                                   "\"%c\" is not valid here!" char )
                      (sit-for 1)
                      (message "")
                      (setq n n-org)
                      nil ))))))))
    (setq skk-input-by-code-or-menu-jump-default (car (cdr kanji-char)))
    (skk-input-by-code-or-menu-1
     (car (cdr kanji-char)) (car (cdr (cdr kanji-char))) )))

(defun skk-input-by-code-or-menu-1 (n1 n2)
  (let ((menu-keys2 ; $BI=<(MQ$N%-!<%j%9%H$rAH$_N)$F$k!#(B
         (mapcar (function (lambda (char) (char-to-string (upcase char))))
                 skk-input-by-code-menu-keys2 ))
        kanji-char )
    (while (not kanji-char)
      (let ((n1-org n1) (n2-org n2) (i 0)
            (chars (list (skk-make-string n1 n2))))
        ;; 16 == (length skk-input-by-code-menu-keys2)
        (while (< i 16)
          (nconc chars (list
                        (progn (setq n2 (skk-next-n2-code n2))
                               (if (= n2 skk-code-n2-min)
                                   (setq n1 (skk-next-n1-code n1)))
                               (skk-make-string n1 n2))))
          (setq i (1+ i)))
        (skk-save-point
          (let ((i 0) message-log-max str )
            (while (< i 16)
              (setq str (concat str (nth i menu-keys2) ":" (nth i chars) " "))
              (setq i (1+ i)) )
            (message str) )
          (let ((char (event-to-character (skk-read-event))))
            (if (not (characterp char))
                (progn
                  (skk-message "\"%s\" $B$OM-8z$J%-!<$G$O$"$j$^$;$s!*(B"
                               "\"%s\" is not valid here!" (prin1 char) )
                  (sit-for 1)
                  (message "")
                  (setq n1 n1-org n2 n2-org) )
              (setq rest
                    (or (memq char skk-input-by-code-menu-keys2)
                        (if (skk-lower-case-p char)
                            (memq (upcase char) skk-input-by-code-menu-keys2)
                          (memq (downcase char) skk-input-by-code-menu-keys2) ))
                    ch (if rest
                           ;; 16 == (length skk-input-by-code-menu-keys2)
                           (nth (- 16 (length rest)) chars) )
                    kanji-char
                    (cond
                     (ch)
                     ((eq char ?x)
                      (if (< (setq n2 (- n2 31)) skk-code-n2-min)
                          (setq n2 (+ n2 94)
                                n1 (skk-previous-n1-code n1)))
                      nil )
                     ((eq char ?\040) ; space
                      (if (= (setq n2 (skk-next-n2-code n2))
                             skk-code-n2-min)
                          (setq n1 (skk-next-n1-code n1)))
                      nil )
                     ((eq char ?\?)
                      (skk-message
                       (concat "$B!X(B%s$B!Y(B  EUC: %2x%2x (%3d, %3d), JIS: %2x%2x (%3d, %3d)  "
                               "[$B2?$+%-!<$r2!$7$F$/$@$5$$(B]" )
                       (concat "$B!X(B%s$B!Y(B  EUC: %2x%2x (%3d, %3d), JIS: %2x%2x (%3d, %3d)  "
                               "[Hit any key to continue]" )
                       (car chars) n1-org n2-org n1-org n2-org
                       (- n1-org 128) (- n2-org 128)
                       (- n1-org 128) (- n2-org 128) )
                      (skk-read-event)
                      (setq n1 n1-org n2 n2-org)
                      nil )
                     ((eq char ?>)
                      (if (= (setq n2 (skk-next-n2-code n2-org))
                             skk-code-n2-min)
                          (setq n1 (skk-next-n1-code n1-org))
                        (setq n1 n1-org))
                      nil )
                     ((eq char ?<)
                      (if (= (setq n2 (skk-previous-n2-code n2-org))
                             skk-code-n2-max)
                          (setq n1 (skk-previous-n1-code n1-org))
                        (setq n1 n1-org))
                      nil )
                     (t
                      (skk-message "\"%c\" $B$OM-8z$J%-!<$G$O$"$j$^$;$s!*(B"
                                   "\"%c\" is not valid here!" char )
                      (sit-for 1)
                      (message "")
                      (setq n1 n1-org n2 n2-org)
                      nil ))))))))
    kanji-char ))

;;;###autoload
(defun skk-display-code-for-char-at-point (&optional arg)
  "$B%]%$%s%H$K$"$kJ8;z$N(B EUC $B%3!<%I$H(B JIS $B%3!<%I$rI=<($9$k!#(B"
  (interactive "P")
  (if (eobp)
      (skk-error "$B%+!<%=%k$,%P%C%U%!$N=*C<$K$"$j$^$9(B"
                 "Cursor is at the end of the buffer" )
    (skk-display-code (buffer-substring-no-properties
		       (point) (skk-save-point (forward-char 1) (point)) ))
    ;; $B%(%3!<$7$?J8;zNs$r%+%l%s%H%P%C%U%!$KA^F~$7$J$$$h$&$K!#(B
    t ))

(skk-defun-cond skk-display-code (str)
  ((memq skk-emacs-type '(xemacs mule4 mule3))
   (let* ((char (string-to-char str))
	  (charset (char-charset char)))
     (cond
      ((memq charset '(japanese-jisx0208 japanese-jisx0208-1978))
       (let* ((char1-j (skk-char-octet char 0))
	      (char1-k (- char1-j 32))
	      (char1-e (+ char1-j 128))
	      (char2-j (skk-char-octet char 1))
	      (char2-k (- char2-j 32))
	      (char2-e (+ char2-j 128)))
	 (message
	  "$B!X(B%s$B!Y(B  EUC: %2x%2x (%3d, %3d), JIS: %2x%2x (%3d, %3d), KUTEN: (%2d, %2d)"
	  str char1-e char2-e char1-e char2-e
	  char1-j char2-j char1-j char2-j char1-k char2-k)))
      ((memq charset '(ascii latin-jisx0201))
       (message "\"%s\"  %2x (%3d)"
		str (skk-char-octet char 0)  (skk-char-octet char 0)))
      (t
       (skk-error "$BH=JL$G$-$J$$J8;z$G$9(B"
		  "Cannot understand this character" )))))
  ;; 'mule2
  (t
   (let (;; $BJ8;zNs$r(B char $B$KJ,2r!#(B
	 ;; (mapcar '+ str) == (append str nil)
	 (char-list (mapcar (function +) str)))
     (cond
      ((and (= (length char-list) 3)
	    (memq (car char-list) (list lc-jp lc-jpold)))
       (let* ((char1-e (car (cdr char-list)))
	      (char1-j (- char1-e 128))
	      (char1-k (- char1-j 32))
	      (char2-e (car (cdr (cdr char-list))))
	      (char2-j (- char2-e 128))
	      (char2-k (- char2-j 32)))
	 (message
	  "$B!X(B%s$B!Y(B  EUC: %2x%2x (%3d, %3d), JIS: %2x%2x (%3d, %3d), KUTEN: (%2d, %2d)"
	  str char1-e char2-e char1-e char2-e
	  char1-j char2-j char1-j char2-j char1-k char2-k)))
      ((or (= (length char-list) 1)	; ascii character
	   (memq (car char-list) (list lc-ascii lc-roman)))
       (let ((char (car char-list)))
	 (message "\"%c\"  %2x (%3d)" char char char) ))
      (t
       (skk-error "$BH=JL$G$-$J$$J8;z$G$9(B"
		  "Cannot understand this character" ))))))

(run-hooks 'skk-kcode-load-hook)

(provide 'skk-kcode)
;;; skk-kcode.el ends here
