;;; skk-cursor.el --- SKK cursor control.
;; Copyright (C) 1996, 1997, 1998, 1999, 2000
;; Masatake YAMATO <masata-y@is.aist-nara.ac.jp> 

;; Author: Masatake YAMATO <masata-y@is.aist-nara.ac.jp>
;; Maintainer: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Version: $Id: skk-cursor.el,v 1.1.2.5.2.27 2000/08/16 12:46:03 czkmt Exp $
;; Keywords: japanese
;; Last Modified: $Date: 2000/08/16 12:46:03 $

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
(or (skk-color-display-p) (error "SKK-CURSOR requires color display"))
(eval-when-compile (require 'static)
		   (require 'skk-macs) (require 'skk-vars))

;; functions.
(defun skk-cursor-set-color (color)
  ;; $B%+!<%=%k$N?'$r(B COLOR $B$KJQ99$9$k!#(B
  (if (not color)
      nil
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
	(modify-frame-parameters (selected-frame) '((cursor-type . box)))))))

(defun skk-cursor-current-color ()
  ;; $B%+%l%s%H%P%C%U%!$N(B SKK $B$N%b!<%I$+$i!"%+!<%=%k$N?'$r<hF@$9$k!#(B
  (cond ((not skk-mode) skk-cursor-default-color)
	;; skk-start-henkan $B$NCf$G$O!"(Bskk-j-mode $B%U%i%0$rN)$F$J$,$i!"(B
	;; skk-abbrev-mode $B%U%i%0$bN)$F$F$$$k(B ($BJQ498e!"D>8e$KF~NO$9$kJ8;z$,(B
	;; $B85$NF~NO%b!<%I$K$F9T$J$o$l$k$h$&$K(B)$B!#=>$$!"(Bskk-abbrev-mode $B%U%i(B
	;; $B%0$N%A%'%C%/$NM%@hEY$r>e$2$k!#(B
	(skk-abbrev-mode skk-cursor-abbrev-color)
	(skk-jisx0208-latin-mode
	 skk-cursor-jisx0208-latin-color )
	(skk-katakana skk-cursor-katakana-color)
	(skk-j-mode skk-cursor-hiragana-color)
	(skk-jisx0201-mode skk-cursor-jisx0201-color)
	(t skk-cursor-latin-color)))

(defun skk-cursor-set-properly (&optional color)
  ;; $B%+%l%s%H%P%C%U%!$N(B SKK $B$N%b!<%I$K=>$$!"%+!<%=%k$N?'$rJQ99$9$k!#(B
  ;; $B%*%W%7%g%J%k0z?t$N(B COLOR $B$,;XDj$5$l$?$H$-$O!"$=$N%+!<%=%k?'$r;H$&!#(B
  ;; OVWRT $B%b!<%I$N$H$-$O%+!<%=%k$NI}$r>.$5$/$9$k!#(B
   (if (not (get-buffer-window (current-buffer)))
      nil
    (and skk-use-color-cursor
	 (skk-cursor-set-color (cond (color)
				     (t (skk-cursor-current-color)))))
    (and skk-cursor-change-width (skk-cursor-change-when-ovwrt))))

(defun skk-cursor-setup-minibuffer ()
  (setq skk-cursor-color-before-entering-minibuffer
	(with-current-buffer
	    (skk-minibuffer-origin) (skk-cursor-current-color)))
  (skk-cursor-set-properly)
  (static-when (eq skk-emacs-type 'xemacs)
    (cond ((and (memq this-command '(skk-start-henkan skk-insert))
		(with-current-buffer
		    (skk-minibuffer-origin) skk-abbrev-mode))
	   (skk-cursor-set-color skk-cursor-hiragana-color))
	  ((and (memq this-command '(skk-insert))
		(not (memq last-command-char
			   '(?\\))))
	   nil)
	  (t
	   (skk-cursor-set-color skk-cursor-default-color)))))

;;; advices.
;;$BF~NO%b!<%I$K1~$8%+!<%=%k?'$rJQ2=$5$;$k!#(BOvwrt $B%b!<%I$N$H$-$K%+!<%=%kI}$r>.$5$/$9$k!#(B
;; $BJL$N%P%C%U%!$XHt$V%3%^%s%I$O(B skk-mode $B$,(B nil $B$G$b%+!<%=%k?'$rD4@0$9$kI,MW$,$"$k!#(B
;; CLASS $B$O(B after.
(let ((funcs '(
	       ;; cover to original Emacs functions.
	       bury-buffer
	       delete-frame
	       delete-window
	       ;;execute-extended-command
	       kill-buffer
	       other-window
	       overwrite-mode
	       pop-to-buffer
	       select-frame
	       select-window
	       switch-to-buffer
	       ;; cover to SKK functions.
	       skk-auto-fill-mode
	       skk-gyakubiki-katakana-message
	       skk-gyakubiki-katakana-region
	       skk-gyakubiki-message
	       skk-hiragana-region
	       skk-hurigana-katakana-region
	       skk-hurigana-message
	       skk-hurigana-region
	       skk-jisx0201-region
	       skk-jisx0208-latin-region
	       skk-katakana-region
	       skk-latin-region
	       skk-mode
	       skk-romaji-message
	       skk-romaji-region
	       skk-save-jisyo
	       skk-toggle-kana)))
  (while funcs
    (eval
     (`
      (defadvice (, (intern (symbol-name (car funcs))))
	(after skk-cursor-ad activate)
	"$BF~NO%b!<%I$K1~$8%+!<%=%k?'$rJQ2=$5$;$k!#(BOvwrt $B%b!<%I$N$H$-$K%+!<%=%kI}$r>.$5$/$9$k!#(B"
	(skk-cursor-set-properly))))
    (setq funcs (cdr funcs))))

;;$BF~NO%b!<%I$K1~$8%+!<%=%k?'$rJQ2=$5$;$k!#(BOvwrt $B%b!<%I$N$H$-$K%+!<%=%kI}$r>.$5$/$9$k!#(B
;; CLASS $B$O(B after.
;; skk-mode $B$,(B nil $B$+(B non-nil $B$+$NH=DjIU$-!#(B
(let ((funcs '(goto-line
	       insert-file
	       keyboard-quit
	       recenter
	       yank
	       yank-pop
	       ;; cover to hilit functions.
	       hilit-recenter
	       hilit-yank
	       hilit-yank-pop)))
  (while funcs
    (eval
     (`
      (defadvice (, (intern (symbol-name (car funcs))))
	(after skk-cursor-ad activate)
	"$BF~NO%b!<%I$K1~$8%+!<%=%k?'$rJQ2=$5$;$k!#(BOvwrt $B%b!<%I$N$H$-$K%+!<%=%kI}$r>.$5$/$9$k!#(B"
	(and skk-mode (skk-cursor-set-properly)))))
    (setq funcs (cdr funcs))))

;;$BF~NO%b!<%I$K1~$8%+!<%=%k?'$rJQ2=$5$;$k!#(BOvwrt $B%b!<%I$N$H$-$K%+!<%=%kI}$r>.$5$/$9$k!#(B
;; CLASS $B$O(B before.
;; $B%_%K%P%C%U%!$+$i85$N%+%l%s%H%P%C%U%!$rC5$7=P$7!"%+!<%=%k$r%;%C%H!#(B
(let ((funcs '(exit-minibuffer)))
  (static-if (eq skk-emacs-type 'xemacs)
      (setq funcs (cons 'minibuffer-keyboard-quit funcs)))
  (while funcs
    (eval
     (`
      (defadvice (, (intern (symbol-name (car funcs))))
	(before skk-cursor-ad activate)
	"$BF~NO%b!<%I$K1~$8%+!<%=%k?'$rJQ2=$5$;$k!#(BOvwrt $B%b!<%I$N$H$-$K%+!<%=%kI}$r>.$5$/$9$k!#(B"
	(with-current-buffer (skk-minibuffer-origin) (skk-cursor-set-properly)))))
    (setq funcs (cdr funcs))))

;; $BF~NO%b!<%I$K1~$8%+!<%=%k?'$rJQ2=$5$;$k!#(BOvwrt $B%b!<%I$N$H$-$K%+!<%=%kI}$r>.$5$/$9$k!#(B
;; CLASS $B$O(B around.
;; skk-abbrev-mode $B$N$H$-$@$1%+!<%=%k$r%;%C%H!#(B
(let ((funcs '(
	       ;; cover to original Emacs functions.
	       newline
	       ;; cover to SKK functions.
	       skk-delete-backward-char
	       skk-insert
	       skk-start-henkan
	       )))
  (while funcs
    (eval
     (`
      (defadvice (, (intern (symbol-name (car funcs))))
	(around skk-cursor-ad activate preactivate)
	"$BF~NO%b!<%I$K1~$8%+!<%=%k?'$rJQ2=$5$;$k!#(BOvwrt $B%b!<%I$N$H$-$K%+!<%=%kI}$r>.$5$/$9$k!#(B"
	;; CLASS $B$O(B around.
	;; skk-abbrev-mode $B$N$H$-$@$1%+!<%=%k$r%;%C%H!#(B
	(if skk-abbrev-mode
	    (progn ad-do-it (skk-cursor-set-properly))
	  ad-do-it))))
    (setq funcs (cdr funcs))))

;;(defadvice execute-extended-command (around skk-cursor-ad activate preactivate)
;;  "$BF~NO%b!<%I$K1~$8%+!<%=%k?'$rJQ2=$5$;$k!#(BOvwrt $B%b!<%I$N$H$-$K%+!<%=%kI}$r>.$5$/$9$k!#(B"
;;  (if skk-mode
;;		(unwind-protect ad-do-it (skk-cursor-set-properly))
;;	      ad-do-it ))
;;
;;(static-unless (eq skk-emacs-type 'xemacs)
;;  (defadvice completing-read (around skk-cursor-ad activate preactivate)
;;	 "$BF~NO%b!<%I$K1~$8%+!<%=%k?'$rJQ2=$5$;$k!#(BOvwrt $B%b!<%I$N$H$-$K%+!<%=%kI}$r>.$5$/$9$k!#(B"
;;	 (if skk-mode
;;	     (condition-case err
;;		 (progn ad-do-it (skk-cursor-set-properly))
;;	       (error
;;		(skk-cursor-set-properly)
;;		(signal (car err) (cdr err)))
;;	       (quit
;;		(skk-cursor-set-properly)
;;		(signal 'quit nil)))
;;	   ad-do-it ))
;;  (defadvice read-from-minibuffer (around skk-cursor-ad activate preactivate)
;;	 "$BF~NO%b!<%I$K1~$8%+!<%=%k?'$rJQ2=$5$;$k!#(BOvwrt $B%b!<%I$N$H$-$K%+!<%=%kI}$r>.$5$/$9$k!#(B"
;;	 (if skk-mode
;;	     (condition-case err
;;		 (progn ad-do-it (skk-cursor-set-properly))
;;	       (error
;;		(skk-cursor-set-properly)
;;		(signal (car err) (cdr err)))
;;	       (quit
;;		(skk-cursor-set-properly)
;;		(signal 'quit nil)))
;;	   ad-do-it )))
;;
;;(defadvice insert-file (after skk-cursor-ad activate)
;;  "$BF~NO%b!<%I$K1~$8%+!<%=%k?'$rJQ2=$5$;$k!#(BOvwrt $B%b!<%I$N$H$-$K%+!<%=%kI}$r>.$5$/$9$k!#(B"
;;  (and skk-mode (skk-cursor-set-properly)) )

(static-when (featurep 'xemacs)
  (defadvice abort-recursive-edit (before skk-cursor-ad activate preactivate)
    "$BF~NO%b!<%I$K1~$8%+!<%=%k?'$rJQ2=$5$;$k!#(BOvwrt $B%b!<%I$N$H$-$K%+!<%=%kI}$r>.$5$/$9$k!#(B"
    (with-current-buffer (skk-minibuffer-origin) (skk-cursor-set-properly))))

;; cover to SKK functions.
(defadvice skk-latin-mode (after skk-cursor-ad activate)
  "$B%+!<%=%k?'$r(B skk-cursor-latin-color $B$KJQ2=$5$;$k!#(BOvwrt $B%b!<%I$N$H$-$K%+!<%=%kI}$r>.$5$/$9$k!#(B"
  (skk-cursor-set-properly skk-cursor-latin-color))

(defadvice skk-jisx0208-latin-mode (after skk-cursor-ad activate)
  "$B%+!<%=%k?'$r(B skk-cursor-jisx0208-latin-color $B$KJQ2=$5$;$k!#(BOvwrt $B%b!<%I$N$H$-$K%+!<%=%kI}$r>.$5$/$9$k!#(B"
  (skk-cursor-set-properly skk-cursor-jisx0208-latin-color))

(defadvice skk-abbrev-mode (after skk-cursor-ad activate)
  "$B1~$8%+!<%=%k?'$r(B skk-cursor-abbrev-color $B$KJQ2=$5$;$k!#(BOvwrt $B%b!<%I$N$H$-$K%+!<%=%kI}$r>.$5$/$9$k!#(B"
  (skk-cursor-set-properly skk-cursor-abbrev-color))

(defadvice skk-kakutei (after skk-cursor-ad activate)
  "$BF~NO%b!<%I$K1~$8%+!<%=%k?'$rJQ2=$5$;$k!#(BOvwrt $B%b!<%I$N$H$-$K%+!<%=%kI}$r>.$5$/$9$k!#(B"
  (if (interactive-p) (skk-cursor-set-properly)))

(add-hook 'minibuffer-setup-hook 'skk-cursor-setup-minibuffer)
(add-hook 'minibuffer-exit-hook
	  (function
	   (lambda ()
	     (with-current-buffer (skk-minibuffer-origin)
	       (skk-cursor-set-color
		skk-cursor-color-before-entering-minibuffer)
	       (and skk-cursor-change-width (skk-cursor-change-when-ovwrt)))))
	  'append )

;;; Hooks
(add-hook 'isearch-mode-end-hook 'skk-cursor-set-properly 'append)

(defalias 'skk-set-cursor-color 'skk-cursor-set-color)
(defalias 'skk-change-cursor-when-ovwrt 'skk-cursor-change-when-ovwrt)
(defalias 'skk-set-cursor-properly 'skk-cursor-set-properly)

(add-hook 'skk-mode-hook 'skk-mode-once-again)

(provide 'skk-cursor)
;;; Local Variables:
;;; End:
;;; skk-cursor.el ends here
