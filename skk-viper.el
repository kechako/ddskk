;; skk-viper.el --- SKK related code for Viper
;; Copyright (C) 1996, 1997, 1998, 1999
;; Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>, Murata Shuuichirou <mrt@astec.co.jp>
;;
;; Author: Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>,
;;         Murata Shuuichirou <mrt@astec.co.jp>
;; Maintainer: Murata Shuuichirou <mrt@astec.co.jp>
;;             Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Version: $Id: skk-viper.el,v 1.5.2.3 1999/11/08 23:26:59 minakaji Exp $
;; Keywords: japanese
;; Last Modified: $Date: 1999/11/08 23:26:59 $

;; This file is part of Aloha SKK.

;; Aloha SKK is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either versions 2, or (at your option)
;; any later version.

;; Aloha SKK is distributed in the hope that it will be useful
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Aloha SKK, see the file COPYING.  If not, write to the Free
;; Software Foundation Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary:

;;; Code:
(eval-when-compile (require 'skk-macs) (require 'skk-vars))
(require 'viper)

;; macros and inline functions.
(defmacro skk-viper-advice-select (viper vip arg body)
  (` (if skk-viper-use-vip-prefix
	 (defadvice (, vip) (, arg) (,@ body))
       (defadvice (, viper) (, arg) (,@ body)) )))

(setq skk-kana-cleanup-command-list
      (cons 
       (if skk-viper-use-vip-prefix
	   'vip-del-backward-char-in-insert
	 'viper-del-backward-char-in-insert )
       skk-kana-cleanup-command-list ))

(setq skk-use-viper t)
(save-match-data
  (or (string-match sentence-end "$B!#!)!*(B")
      (setq sentence-end (concat "[$B!#!)!*(B]\\|" sentence-end)) ))

;; cursor color support.
(if (and (boundp 'viper-insert-state-cursor-color)
	 viper-insert-state-cursor-color
	 (fboundp 'viper-color-defined-p)
	 (viper-color-defined-p viper-insert-state-cursor-color)
	 ;;(member (x-color-values viper-insert-state-cursor-color)
         ;;        (list (x-color-values skk-cursor-hiragana-color)
         ;;              (x-color-values skk-cursor-katakana-color)
         ;;              (x-color-values skk-cursor-jisx0208-latin-color)
         ;;              (x-color-values skk-cursor-latin-color) )))
	 )
    (setq skk-use-color-cursor nil) )
;; $B$H$j$"$($:!"30$=$&$M!"7k9==E$$$b$s$M!#(B
;;(add-hook 'viper-post-command-hooks
;; 	    (function (lambda () (and skk-mode (skk-cursor-set-properly)))) ))

;; advices.
(or skk-viper-use-vip-prefix
    ;; vip-hide-replace-overlay $B$O%$%s%i%$%s4X?t(B
    (defadvice viper-hide-replace-overlay (after skk-ad activate)
      "SKK $B$N%b!<%I$K=>$$%+!<%=%k$N?'$rJQ$($k!#(B"
      (and skk-mode (skk-cursor-set-properly)) ))

(skk-viper-advice-select
 viper-insert vip-insert
 (after skk-ad activate)
 ("SKK $B$N%b!<%I$K=>$$%+!<%=%k$N?'$rJQ$($k!#(B"
  (and skk-mode (skk-cursor-set-properly)) ))

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
		    (= skk-henkan-end-point (point)) )
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
		   (delete-char count) )
	       ad-do-it )
	     ;; XXX assume skk-prefix has no multibyte chars.
	     (if (> (length skk-prefix) count)
		 (setq skk-prefix (substring skk-prefix 0 (- (length skk-prefix) count)))
	       (setq skk-prefix "") )
	     (if (>= skk-henkan-end-point (point)) (skk-kakutei)) ))
	  ((and skk-henkan-on (>= skk-henkan-start-point (point)))
	   (setq skk-henkan-count 0)
	   (skk-kakutei) )
	  ;; $BF~NOCf$N8+=P$78l$KBP$7$F$O(B delete-backward-char $B$GI,$:A43QJ8;z(B 1
	  ;; $BJ8;zJ,(B backward $BJ}8~$KLa$C$?J}$,NI$$!#(B
	  ((and skk-henkan-on overwrite-mode)
	   (backward-char count)
	   (delete-char count) )
	  (t
	   (if (string= skk-prefix "")
	       ad-do-it
	     (skk-erase-prefix 'clean) ))))))

(skk-viper-advice-select
 viper-intercept-ESC-key vip-intercept-ESC-key
 (before skk-add activate)
 ("$B"&%b!<%I!""'%b!<%I$@$C$?$i3NDj$9$k!#3NDj8e!"(BSKK $B$N%b!<%I$K=>$$%+!<%=%k$N?'$rJQ$($k!#(B"
  (and skk-mode skk-henkan-on (skk-kakutei)) ))

(skk-viper-advice-select
 viper-join-lines vip-join-lines
 (after skk-ad activate)
 ("$B%9%Z!<%9$NN>B&$NJ8;z%;%C%H$,(B JISX0208 $B$@$C$?$i%9%Z!<%9$r<h$j=|$/!#(B" ;
  (save-match-data
    (and (skk-jisx0208-p
	  (char-after (progn (skip-chars-forward " ") (point))) )
	 (skk-jisx0208-p
	  (char-before (progn (skip-chars-backward " ") (point))) )
	 (while (looking-at " ")
	   (delete-char 1) )))))

;;;###autoload
(defun skk-viper-normalize-map ()
  (let ((other-buffer
	 (if (eq skk-emacs-type 'xemacs)
	     (local-variable-p 'minor-mode-map-alist nil t)
	   (local-variable-p 'minor-mode-map-alist) )))
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
			 (cons 'skk-jisx0208-latin-mode skk-jisx0208-latin-mode-map) ))
		  (funcall skk-viper-normalize-map-function) ))
	    (setq buf (cdr buf)) ))))))

(defun skk-cursor-set-properly ()
  ;; $B%+%l%s%H%P%C%U%!$N(B SKK $B$N%b!<%I$K=>$$!"%+!<%=%k$N?'$rJQ99$9$k!#(B
  (if (and skk-use-color-cursor
	   ;;(x-color-defined-p viper-insert-state-cursor-color)
	   (get-buffer-window (current-buffer)) )
      (cond
       ((or (not skk-mode)
	    ;; vi-state $B$N$H$-$O!"(BSKK $B%b!<%I$K$J$C$F$$$F$b%+!<%=%k$r%G%#(B
	    ;; $B%U%)%k%H$K$7$F$*$/!#(B
	    (or (and (boundp 'viper-current-state)
		     (eq viper-current-state 'vi-state) )
		(and (boundp 'vip-current-state)
		     (eq vip-current-state 'vi-state) )))
	(skk-cursor-set-color skk-cursor-default-color) )
       (t
	(skk-cursor-set-color (cond (skk-jisx0208-latin-mode
				     skk-cursor-jisx0208-latin-color )
				    (skk-katakana skk-cursor-katakana-color)
				    (skk-j-mode skk-cursor-hiragana-color)
				    (t skk-cursor-latin-color) )))))
  (if skk-cursor-change-width
      (skk-change-cursor-when-ovwrt) ))

(skk-viper-normalize-map)

(provide 'skk-viper)
;;; skk-viper.el ends here
