;;; skk-cursor.el --- SKK cursor control.
;; Copyright (C) 1996, 1997, 1998, 1999
;; Masatake YAMATO <jet@airlab.cs.ritsumei.ac.jp>

;; Author: Masatake YAMATO <jet@airlab.cs.ritsumei.ac.jp>
;; Maintainer: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Version: $Id: skk-cursor.el,v 1.1.2.5.2.20 1999/12/18 08:05:45 czkmt Exp $
;; Keywords: japanese
;; Last Modified: $Date: 1999/12/18 08:05:45 $

;; This file is part of Daredevil SKK.

;; Daredevil SKK is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either versions 2, or (at your option) any later
;; version.

;; Daredevil SKK is distributed in the hope that it will be useful but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Daredevil SKK, see the file COPYING.  If not, write to the Free
;; Software Foundation Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary:

;;; Code:
(eval-when-compile (require 'static)
		   (require 'skk-macs) (require 'skk-vars) )

;; functions.
(defun skk-cursor-set-color (color)
  ;; $B%+!<%=%k$N?'$r(B COLOR $B$KJQ99$9$k!#(B
  (and skk-use-color-cursor
       (condition-case nil
	   (set-cursor-color color)
	 (error
	  (set-cursor-color skk-cursor-default-color)
	  (and skk-cursor-report-set-error
	       (skk-message
		"$B%+%i!<%^%C%W@Z$l$G$9!#%G%#%U%)%k%H$N%+%i!<$r;H$$$^$9!#(B"
		"Color map is exhausting, use default cursor color" ))))))

(defun skk-cursor-change-when-ovwrt ()
  (static-cond
   ((eq skk-emacs-type 'xemacs) (setq bar-cursor overwrite-mode))
   (t (if overwrite-mode
	  (modify-frame-parameters (selected-frame) '((cursor-type bar . 3)))
	(modify-frame-parameters (selected-frame) '((cursor-type . box))) ))))

(defun skk-cursor-current-color ()
  ;; $B%+%l%s%H%P%C%U%!$N(B SKK $B$N%b!<%I$+$i!"%+!<%=%k$N?'$r<hF@$9$k!#(B
  (if (not skk-mode)
      skk-cursor-default-color
    (cond (skk-jisx0208-latin-mode
	   skk-cursor-jisx0208-latin-color )
	  (skk-abbrev-mode skk-cursor-abbrev-color)
	  (skk-katakana skk-cursor-katakana-color)
	  (skk-j-mode skk-cursor-hiragana-color)
	  ((and (boundp 'skk-jisx0201-mode)
		skk-jisx0201-mode)
	   skk-cursor-jisx0201-color)
	  (t skk-cursor-latin-color) )))

;; Overwrite by skk-viper.el
(defun skk-cursor-set-properly ()
  ;; $B%+%l%s%H%P%C%U%!$N(B SKK $B$N%b!<%I$K=>$$!"%+!<%=%k$N?'$rJQ99$9$k!#(B
  (if (not (get-buffer-window (current-buffer)))
      nil
    (and skk-use-color-cursor
	 (skk-cursor-set-color (skk-cursor-current-color)) )
    (and skk-cursor-change-width (skk-cursor-change-when-ovwrt)) ))

;;; advices.
;; cover to original Emacs functions.
(defadvice overwrite-mode (after skk-cursor-ad activate)
  "skk-cursor-change-width $B$,(B non-nil $B$@$C$?$i!"%+!<%=%k$NI}$r=L$a$k!#(B"
  (and skk-cursor-change-width (skk-cursor-change-when-ovwrt)) )

(static-when (featurep 'xemacs)
  (defadvice abort-recursive-edit (before skk-cursor-ad activate preactivate)
    "$BF~NO%b!<%I$K1~$8%+!<%=%k?'$rJQ2=$5$;$k!#(BOvwrt $B%b!<%I$N$H$-$K%+!<%=%kI}$r>.$5$/$9$k!#(B"
    (with-current-buffer (skk-minibuffer-origin) (skk-cursor-set-properly))))

;; $BJL$N%P%C%U%!$XHt$V%3%^%s%I$O(B skk-mode $B$,(B nil $B$G$b%+!<%=%k?'$rD4@0$9$kI,MW$,(B
;; $B$"$k!#(B
(let ((funcs '(kill-buffer
	       bury-buffer
	       switch-to-buffer
	       pop-to-buffer
	       other-window
	       delete-window
	       select-window
	       select-frame
	       delete-frame)))
  (while funcs
    (eval
     (`
      (defadvice (, (intern (symbol-name (car funcs))))
	(after skk-cursor-ad activate)
	"$BF~NO%b!<%I$K1~$8%+!<%=%k?'$rJQ2=$5$;$k!#(BOvwrt $B%b!<%I$N$H$-$K%+!<%=%kI}$r>.$5$/$9$k!#(B"
	(skk-cursor-set-properly) ) ) )
    (setq funcs (cdr funcs)) ) )

;;(defadvice goto-line (after skk-cursor-ad activate)
;;  "$BF~NO%b!<%I$K1~$8%+!<%=%k?'$rJQ2=$5$;$k!#(BOvwrt $B%b!<%I$N$H$-$K%+!<%=%kI}$r>.$5$/$9$k!#(B"
;;  (and skk-mode (skk-cursor-set-properly)) )

(defadvice yank (after skk-cursor-ad activate)
  "$BF~NO%b!<%I$K1~$8%+!<%=%k?'$rJQ2=$5$;$k!#(BOvwrt $B%b!<%I$N$H$-$K%+!<%=%kI}$r>.$5$/$9$k!#(B"
  (and skk-mode (skk-cursor-set-properly)) )

(defadvice yank-pop (after skk-cursor-ad activate)
  "$BF~NO%b!<%I$K1~$8%+!<%=%k?'$rJQ2=$5$;$k!#(BOvwrt $B%b!<%I$N$H$-$K%+!<%=%kI}$r>.$5$/$9$k!#(B"
  (and skk-mode (skk-cursor-set-properly)) )

(defadvice recenter (after skk-cursor-ad activate)
  "$BF~NO%b!<%I$K1~$8%+!<%=%k?'$rJQ2=$5$;$k!#(BOvwrt $B%b!<%I$N$H$-$K%+!<%=%kI}$r>.$5$/$9$k!#(B"
  (and skk-mode (skk-cursor-set-properly)) )

;;(defadvice insert-file (after skk-cursor-ad activate)
;;  "$BF~NO%b!<%I$K1~$8%+!<%=%k?'$rJQ2=$5$;$k!#(BOvwrt $B%b!<%I$N$H$-$K%+!<%=%kI}$r>.$5$/$9$k!#(B"
;;  (and skk-mode (skk-cursor-set-properly)) )

(defadvice newline (around skk-cursor-ad activate preactivate)
  "$BF~NO%b!<%I$K1~$8%+!<%=%k?'$rJQ2=$5$;$k!#(BOvwrt $B%b!<%I$N$H$-$K%+!<%=%kI}$r>.$5$/$9$k!#(B"
  (if skk-abbrev-mode
      (progn ad-do-it (skk-cursor-set-properly))
    ad-do-it))

;; cover to hilit19 functions.
(defadvice hilit-yank (after skk-cursor-ad activate)
  "$BF~NO%b!<%I$K1~$8%+!<%=%k?'$rJQ2=$5$;$k!#(BOvwrt $B%b!<%I$N$H$-$K%+!<%=%kI}$r>.$5$/$9$k!#(B"
  (and skk-mode (skk-cursor-set-properly)) )

(defadvice hilit-yank-pop (after skk-cursor-ad activate)
  "$BF~NO%b!<%I$K1~$8%+!<%=%k?'$rJQ2=$5$;$k!#(BOvwrt $B%b!<%I$N$H$-$K%+!<%=%kI}$r>.$5$/$9$k!#(B"
  (and skk-mode (skk-cursor-set-properly)) )

(defadvice hilit-recenter (after skk-cursor-ad activate)
  "$BF~NO%b!<%I$K1~$8%+!<%=%k?'$rJQ2=$5$;$k!#(BOvwrt $B%b!<%I$N$H$-$K%+!<%=%kI}$r>.$5$/$9$k!#(B"
  (and skk-mode (skk-cursor-set-properly)) )

(defadvice keyboard-quit (after skk-cursor-ad activate)
  "$BF~NO%b!<%I$K1~$8%+!<%=%k?'$rJQ2=$5$;$k!#(BOvwrt $B%b!<%I$N$H$-$K%+!<%=%kI}$r>.$5$/$9$k!#(B"
  (and skk-mode
       (not skk-henkan-on)
       (not skk-henkan-active)
       (skk-cursor-set-properly) ) )

;; cover to VIP/Viper functions.
(defadvice viper-intercept-ESC-key (after skk-cursor-ad activate)
  (and skk-mode (skk-cursor-set-properly)) )

(defadvice vip-intercept-ESC-key (after skk-cursor-ad activate)
  (and skk-mode (skk-cursor-set-properly)) )

;; cover to SKK functions.
;; skk-mode $B$H(B skk-auto-fill-mode $B$@$1$O!"(Bskk-cursor-set-properly $B$r8F$P$J$$!#(B
(defadvice skk-mode (after skk-cursor-ad activate)
  (skk-cursor-set-properly))

(defadvice skk-latin-mode (after skk-cursor-ad activate)
  (skk-cursor-set-properly) )

(defadvice skk-jisx0208-latin-mode (after skk-cursor-ad activate)
  (skk-cursor-set-properly) )

(defadvice skk-abbrev-mode (after skk-cursor-ad activate)
  (skk-cursor-set-properly) )

(defadvice skk-toggle-kana (after skk-cursor-ad activate)
  "$BF~NO%b!<%I$K1~$8%+!<%=%k?'$rJQ2=$5$;$k!#(BOvwrt $B%b!<%I$N$H$-$K%+!<%=%kI}$r>.$5$/$9$k!#(B"
  (if skk-katakana
      (skk-cursor-set-color skk-cursor-katakana-color)
    (skk-cursor-set-color skk-cursor-hiragana-color) ))

(defadvice skk-kakutei (after skk-cursor-ad activate)
  "$BF~NO%b!<%I$K1~$8%+!<%=%k?'$rJQ2=$5$;$k!#(BOvwrt $B%b!<%I$N$H$-$K%+!<%=%kI}$r>.$5$/$9$k!#(B"
  (if (interactive-p)
      (skk-cursor-set-color (if skk-katakana skk-cursor-katakana-color
			      skk-cursor-hiragana-color ))))

(defadvice skk-save-jisyo (after skk-cursor-ad activate)
  "$BF~NO%b!<%I$K1~$8%+!<%=%k?'$rJQ2=$5$;$k!#(BOvwrt $B%b!<%I$N$H$-$K%+!<%=%kI}$r>.$5$/$9$k!#(B"
  (skk-cursor-set-properly) )

(defadvice skk-katakana-region (after skk-cursor-ad activate)
  "$BF~NO%b!<%I$K1~$8%+!<%=%k?'$rJQ2=$5$;$k!#(BOvwrt $B%b!<%I$N$H$-$K%+!<%=%kI}$r>.$5$/$9$k!#(B"
  (skk-cursor-set-properly) )

(defadvice skk-hiragana-region (after skk-cursor-ad activate)
  "$BF~NO%b!<%I$K1~$8%+!<%=%k?'$rJQ2=$5$;$k!#(BOvwrt $B%b!<%I$N$H$-$K%+!<%=%kI}$r>.$5$/$9$k!#(B"
  (skk-cursor-set-properly) )

(defadvice skk-jisx0208-latin-region (after skk-cursor-ad activate)
  "$BF~NO%b!<%I$K1~$8%+!<%=%k?'$rJQ2=$5$;$k!#(BOvwrt $B%b!<%I$N$H$-$K%+!<%=%kI}$r>.$5$/$9$k!#(B"
  (skk-cursor-set-properly) )

(defadvice skk-latin-region (after skk-cursor-ad activate)
  "$BF~NO%b!<%I$K1~$8%+!<%=%k?'$rJQ2=$5$;$k!#(BOvwrt $B%b!<%I$N$H$-$K%+!<%=%kI}$r>.$5$/$9$k!#(B"
  (skk-cursor-set-properly) )

(defadvice skk-jisx0201-region (after skk-cursor-ad activate)
  "$BF~NO%b!<%I$K1~$8%+!<%=%k?'$rJQ2=$5$;$k!#(BOvwrt $B%b!<%I$N$H$-$K%+!<%=%kI}$r>.$5$/$9$k!#(B"
  (skk-cursor-set-properly) )

(defadvice skk-gyakubiki-message (after skk-cursor-ad activate)
  "$BF~NO%b!<%I$K1~$8%+!<%=%k?'$rJQ2=$5$;$k!#(BOvwrt $B%b!<%I$N$H$-$K%+!<%=%kI}$r>.$5$/$9$k!#(B"
  (skk-cursor-set-properly) )

(defadvice skk-gyakubiki-katakana-region (after skk-cursor-ad activate)
  "$BF~NO%b!<%I$K1~$8%+!<%=%k?'$rJQ2=$5$;$k!#(BOvwrt $B%b!<%I$N$H$-$K%+!<%=%kI}$r>.$5$/$9$k!#(B"
  (skk-cursor-set-properly) )

(defadvice skk-gyakubiki-katakana-message (after skk-cursor-ad activate)
  "$BF~NO%b!<%I$K1~$8%+!<%=%k?'$rJQ2=$5$;$k!#(BOvwrt $B%b!<%I$N$H$-$K%+!<%=%kI}$r>.$5$/$9$k!#(B"
  (skk-cursor-set-properly) )

(defadvice skk-hurigana-region (after skk-cursor-ad activate)
  "$BF~NO%b!<%I$K1~$8%+!<%=%k?'$rJQ2=$5$;$k!#(BOvwrt $B%b!<%I$N$H$-$K%+!<%=%kI}$r>.$5$/$9$k!#(B"
  (skk-cursor-set-properly) )

(defadvice skk-hurigana-message (after skk-cursor-ad activate)
  "$BF~NO%b!<%I$K1~$8%+!<%=%k?'$rJQ2=$5$;$k!#(BOvwrt $B%b!<%I$N$H$-$K%+!<%=%kI}$r>.$5$/$9$k!#(B"
  (skk-cursor-set-properly) )

(defadvice skk-hurigana-katakana-region (after skk-cursor-ad activate)
  "$BF~NO%b!<%I$K1~$8%+!<%=%k?'$rJQ2=$5$;$k!#(BOvwrt $B%b!<%I$N$H$-$K%+!<%=%kI}$r>.$5$/$9$k!#(B"
  (skk-cursor-set-properly) )

(defadvice skk-romaji-region (after skk-cursor-ad activate)
  "$BF~NO%b!<%I$K1~$8%+!<%=%k?'$rJQ2=$5$;$k!#(BOvwrt $B%b!<%I$N$H$-$K%+!<%=%kI}$r>.$5$/$9$k!#(B"
  (skk-cursor-set-properly) )

(defadvice skk-romaji-message (after skk-cursor-ad activate)
  "$BF~NO%b!<%I$K1~$8%+!<%=%k?'$rJQ2=$5$;$k!#(BOvwrt $B%b!<%I$N$H$-$K%+!<%=%kI}$r>.$5$/$9$k!#(B"
  (skk-cursor-set-properly) )

(defadvice skk-delete-backward-char (around skk-cursor-ad activate preactivate)
  "$BF~NO%b!<%I$K1~$8%+!<%=%k?'$rJQ2=$5$;$k!#(BOvwrt $B%b!<%I$N$H$-$K%+!<%=%kI}$r>.$5$/$9$k!#(B"
  (if skk-abbrev-mode
      (progn ad-do-it (skk-cursor-set-properly))
    ad-do-it))

(defadvice skk-start-henkan (around skk-cursor-ad activate preactivate)
  "$BF~NO%b!<%I$K1~$8%+!<%=%k?'$rJQ2=$5$;$k!#(BOvwrt $B%b!<%I$N$H$-$K%+!<%=%kI}$r>.$5$/$9$k!#(B"
  (if skk-abbrev-mode
      (progn ad-do-it (skk-cursor-set-properly))
    ad-do-it))

(defadvice skk-insert (around skk-cursor-ad activate preactivate)
  "$BF~NO%b!<%I$K1~$8%+!<%=%k?'$rJQ2=$5$;$k!#(BOvwrt $B%b!<%I$N$H$-$K%+!<%=%kI}$r>.$5$/$9$k!#(B"
  (if skk-abbrev-mode
      (progn ad-do-it (skk-cursor-set-properly))
    ad-do-it))

;;(add-hook 'after-make-frame-hook 'skk-cursor-set-properly)

(add-hook 'minibuffer-setup-hook
	  (function
	   (lambda ()
	     (setq skk-cursor-color-before-entering-minibuffer
		   (with-current-buffer
		       (skk-minibuffer-origin) (skk-cursor-current-color)))
	     (skk-cursor-set-properly)
	     (static-when (eq skk-emacs-type 'xemacs)
	       (cond ((and (memq this-command '(skk-insert))
			   (not (memq last-command-char
				      '(?\\))))
		      nil)
		     ((and (memq this-command '(skk-start-henkan))
			   (with-current-buffer
			       (skk-minibuffer-origin) skk-abbrev-mode))
		      (skk-cursor-set-color skk-cursor-hiragana-color))
		     (t
		      (skk-cursor-set-color skk-cursor-default-color)))))))

(add-hook 'minibuffer-exit-hook
	  (function
	   (lambda ()
	     (skk-cursor-set-color
	      skk-cursor-color-before-entering-minibuffer)
	     (and skk-cursor-change-width (skk-cursor-change-when-ovwrt)) ))
	  'append )

;; $B:G=i$K(B load $B$5$l$?$H$-$O!"(Bskk-cursor adviced function $B$K$J$kA0$N4X?t$K$h$C$F(B
;; $B8F$P$l$F$*$j!"(Badvice $B$,8z$$$F$J$$$N$G!"%H%C%W%l%Y%k$G%+!<%=%k$r9g$o$;$F$*$/!#(B
(and (get-buffer-window (current-buffer))
     ;; only first time when this file loaded.
     (not skk-mode-invoked)
     (let ((skk-j-mode t)
	   skk-latin-mode skk-abbrev-mode skk-jisx0208-latin-mode
	   skk-katakana )
       (and (memq this-command '(skk-mode skk-auto-fill-mode
					  select-input-method
					  toggle-input-method))
	    (skk-cursor-set-properly) )))

(provide 'skk-cursor)
;;; Local Variables:
;;; End:
;;; skk-cursor.el ends here
