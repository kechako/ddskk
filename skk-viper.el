;; skk-viper.el --- SKK related code for Viper
;; Copyright (C) 1996, 1997, 1998, 1999
;; Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>, Murata Shuuichirou <mrt@astec.co.jp>
;;
;; Author: Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>,
;;         Murata Shuuichirou <mrt@notwork.org>
;; Maintainer: Murata Shuuichirou <mrt@notwork.org>
;;             Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Version: $Id: skk-viper.el,v 1.7 2000/03/11 02:14:53 minakaji Exp $
;; Keywords: japanese
;; Last Modified: $Date: 2000/03/11 02:14:53 $

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

;;; Code:
(eval-when-compile (require 'skk))
(require 'skk-foreword)
(require 'viper)

;;(defgroup skk-viper nil "SKK/Viper related customization."
;;  :prefix "skk-"
;;  :group 'skk )

;; internal constant.
;;;###autoload
(defconst skk-viper-use-vip-prefix
  (not (fboundp 'viper-normalize-minor-mode-map-alist)))

;;;###autoload
(defconst skk-viper-normalize-map-function
  (if skk-viper-use-vip-prefix 
      'vip-normalize-minor-mode-map-alist 
    'viper-normalize-minor-mode-map-alist )
  "Viper $B$,(B minor-mode-map-alist $B$rD4@0$9$k$?$a$N4X?t!#(B" )

;; macros and inline functions.
(defmacro skk-viper-advice-select (viper vip arg body)
  (` (if skk-viper-use-vip-prefix
	 (defadvice (, vip) (, arg) (,@ body))
       (defadvice (, viper) (, arg) (,@ body)))))

(setq skk-kana-cleanup-command-list
      (cons 
       (if skk-viper-use-vip-prefix
	   'vip-del-backward-char-in-insert
	 'viper-del-backward-char-in-insert )
       skk-kana-cleanup-command-list ))

(setq skk-use-viper t)
(save-match-data
  (or (string-match sentence-end "$B!#!)!*(B")
      (setq sentence-end (concat "[$B!#!)!*(B]\\|" sentence-end))))

;; cursor color support.
(if (and (boundp 'viper-insert-state-cursor-color)
	 viper-insert-state-cursor-color
	 (fboundp 'viper-color-defined-p)
	 (viper-color-defined-p viper-insert-state-cursor-color))
    (setq skk-use-color-cursor nil))

;; advices.
(defadvice skk-cursor-set-properly (before skk-viper-ad activate)
  "vi-state $B$N$H$-$O!"(BSKK $B%b!<%I$K$J$C$F$$$F$b%+!<%=%k$r%G%#%U%)%k%H$K$7$F$*$/!#(B"
  (if (or (and (boundp 'viper-current-state)
	       (eq viper-current-state 'vi-state))
	  (and (boundp 'vip-current-state)
	       (eq vip-current-state 'vi-state)))
      (ad-set-arg 0 skk-default-cursor-color)))

(skk-viper-advice-select
 viper-forward-word-kernel vip-forward-word-kernel
 (around skk-ad activate)
 ("SKK $B%b!<%I$,%*%s$G!"%]%$%s%H$ND>8e$NJ8;z$,(B JISX0208 $B$@$C$?$i(B forward-word $B$9$k!#(B"
  (if (and skk-mode (skk-jisx0208-p (following-char)))
      (forward-word val)
    ad-do-it )))

(skk-viper-advice-select
 viper-backward-word-kernel vip-backward-word-kernel
 (around skk-ad activate)
 ("SKK $B%b!<%I$,%*%s$G!"%]%$%s%H$ND>A0$NJ8;z$,(B JISX0208 $B$@$C$?$i(B backward-word $B$9$k!#(B"
  (if (and skk-mode (skk-jisx0208-p (preceding-char)))
      (backward-word val)
    ad-do-it )))

;; please sync with advice to delete-backward-char
(skk-viper-advice-select
 viper-del-backward-char-in-insert vip-del-backward-char-in-insert
 (around skk-ad activate)
 ("$B"'%b!<%I$G(B skk-delete-implies-kakutei $B$,(B non-nil $B$@$C$?$iD>A0$NJ8;z$r>C$7$F3NDj$9$k!#(B
$B"'%b!<%I$G(B skk-delete-implies-kakutei $B$,(B nil $B$@$C$?$iA08uJd$rI=<($9$k!#(B
$B"&%b!<%I$@$C$?$i3NDj$9$k!#(B
$B3NDjF~NO%b!<%I$G!"$+$J%W%l%U%#%C%/%9$NF~NOCf$J$i$P!"$+$J%W%l%U%#%C%/%9$r>C$9!#(B"
  (let ((count (or (prefix-numeric-value (ad-get-arg 0)) 1)))
    (cond (skk-henkan-active
	   (if (and (not skk-delete-implies-kakutei)
		    (= skk-henkan-end-point (point)))
	       (skk-previous-candidate)
	     ;;(if skk-use-face (skk-henkan-face-off))
 	     ;; overwrite-mode $B$G!"%]%$%s%H$,A43QJ8;z$K0O$^$l$F$$$k$H(B
	     ;; $B$-$K(B delete-backward-char $B$r;H$&$H!"A43QJ8;z$O>C$9$,H>(B
	     ;; $B3QJ8;zJ,$7$+(B backward $BJ}8~$K%]%$%s%H$,La$i$J$$(B (Emacs
	     ;; 19.31 $B$K$F3NG'(B)$B!#JQ49Cf$N8uJd$KBP$7$F$O(B
	     ;; delete-backward-char $B$GI,$:A43QJ8;z(B 1 $BJ8;zJ,(B backward
	     ;; $BJ}8~$KLa$C$?J}$,NI$$!#(B
	     (if overwrite-mode
		 (progn
		   (backward-char count)
		   (delete-char count))
	       ad-do-it )
	     ;; XXX assume skk-prefix has no multibyte chars.
	     (if (> (length skk-prefix) count)
		 (setq skk-prefix (substring skk-prefix 0 (- (length skk-prefix) count)))
	       (setq skk-prefix ""))
	     (if (>= skk-henkan-end-point (point)) (skk-kakutei))))
	  ((and skk-henkan-on (>= skk-henkan-start-point (point)))
	   (setq skk-henkan-count 0)
	   (skk-kakutei))
	  ;; $BF~NOCf$N8+=P$78l$KBP$7$F$O(B delete-backward-char $B$GI,$:A43QJ8;z(B 1
	  ;; $BJ8;zJ,(B backward $BJ}8~$KLa$C$?J}$,NI$$!#(B
	  ((and skk-henkan-on overwrite-mode)
	   (backward-char count)
	   (delete-char count))
	  (t
	   (if (string= skk-prefix "")
	       ad-do-it
	     (skk-erase-prefix 'clean)))))))

(skk-viper-advice-select
 viper-intercept-ESC-key vip-intercept-ESC-key
 (before skk-add activate)
 ("$B"&%b!<%I!""'%b!<%I$@$C$?$i3NDj$9$k!#(B"
  (and skk-mode skk-henkan-on (skk-kakutei))))

(skk-viper-advice-select
 viper-join-lines vip-join-lines
 (after skk-ad activate)
 ("$B%9%Z!<%9$NN>B&$NJ8;z%;%C%H$,(B JISX0208 $B$@$C$?$i%9%Z!<%9$r<h$j=|$/!#(B" ;
  (save-match-data
    (and (skk-jisx0208-p
	  (char-after (progn (skip-chars-forward " ") (point))))
	 (skk-jisx0208-p
	  (char-before (progn (skip-chars-backward " ") (point))))
	 (while (looking-at " ")
	   (delete-char 1))))))

;;; Functions.
;;;###autoload
(defun skk-viper-normalize-map ()
  (let ((other-buffer
	 (if (eq skk-emacs-type 'xemacs)
	     (local-variable-p 'minor-mode-map-alist nil t)
	   (local-variable-p 'minor-mode-map-alist))))
    ;; for current buffer and buffers to be created in the future.
    ;; substantially the same job as viper-harness-minor-mode does.
    (funcall skk-viper-normalize-map-function)
    (setq-default minor-mode-map-alist minor-mode-map-alist)
    (if (not other-buffer)
	nil
      ;; for buffers which are already created and have the minor-mode-map-alist
      ;; localized by Viper.
      (save-current-buffer
	(let ((buf (buffer-list)))
	  (while buf
	    (set-buffer (car buf))
	    (if (null (assq 'skk-j-mode minor-mode-map-alist))
		(progn
		  (set-modified-alist
		   'minor-mode-map-alist
		   (list (cons 'skk-latin-mode skk-latin-mode-map)
			 (cons 'skk-abbrev-mode skk-abbrev-mode-map)
			 (cons 'skk-j-mode skk-j-mode-map)
			 (cons 'skk-jisx0208-latin-mode skk-jisx0208-latin-mode-map)))
		  (funcall skk-viper-normalize-map-function)))
	    (setq buf (cdr buf))))))))

(eval-after-load "viper-cmd"
  '(defun viper-toggle-case (arg)
     "Toggle character case."
     (interactive "P")
     (let ((val (viper-p-val arg)) (c))
       (viper-set-destructive-command
	(list 'viper-toggle-case val nil nil nil nil))
       (while (> val 0)
	 (setq c (following-char))
	 (delete-char 1 nil)
	 (cond ((skk-ascii-char-p c)
		(if (eq c (upcase c))
		    (insert-char (downcase c) 1)
		  (insert-char (upcase c) 1)))
	       ((and (<= ?$B$!(B c) (>= ?$B$s(B c))
		(insert-string
		 (skk-hiragana-to-katakana (char-to-string c))))
	       ((and (<= ?$B%!(B c) (>= ?$B%s(B c))
		(insert-string
		 (skk-katakana-to-hiragana (char-to-string c))))
	       (t (insert-char c 1)))
	 (if (eolp) (backward-char 1))
	 (setq val (1- val))))))

(skk-viper-normalize-map)

(provide 'skk-viper)
;;; skk-viper.el ends here
