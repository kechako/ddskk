;;; skk-cursor.el --- SKK cursor control

;; Copyright (C) 1996, 1997, 1998, 1999, 2000
;;   Masatake YAMATO <masata-y@is.aist-nara.ac.jp>

;; Author: Masatake YAMATO <masata-y@is.aist-nara.ac.jp>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Version: $Id: skk-cursor.el,v 1.32 2001/12/16 05:03:09 czkmt Exp $
;; Keywords: japanese, mule, input method
;; Last Modified: $Date: 2001/12/16 05:03:09 $

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

;;; Code:

(if (not (skk-color-display-p))
    (error "%s" "SKK-CURSOR requires color display"))

(eval-when-compile
  (require 'static)
  (require 'skk-macs)
  (require 'skk-vars))

(static-unless
    (eq skk-emacs-type 'xemacs)
  (require 'ccc))

;; Functions.

(defun skk-cursor-default-color ()
  (static-cond
   ((featurep 'xemacs)
    (frame-property (selected-frame) 'cursor-color))
   (t
    frame-cursor-color)))

;;;###autoload
(defun skk-cursor-current-color ()
  ;; $B%+%l%s%H%P%C%U%!$N(B SKK $B$N%b!<%I$+$i!"%+!<%=%k$N?'$r<hF@$9$k!#(B
  (cond
   ((not (and skk-use-color-cursor
	      skk-mode))
    (skk-cursor-default-color))
   ;; `skk-start-henkan' $B$NCf$G$O!"(Bskk-j-mode $B%U%i%0$rN)$F$J$,$i!"(B
   ;; skk-abbrev-mode $B%U%i%0$bN)$F$F$$$k(B ($BJQ498e!"D>8e$KF~NO$9$kJ8(B
   ;; $B;z$,85$NF~NO%b!<%I$K$F9T$J$o$l$k$h$&$K(B)$B!#=>$$!"(Bskk-abbrev-mode
   ;; $B%U%i%0$N%A%'%C%/$NM%@hEY$r>e$2$k!#(B
   (skk-abbrev-mode
    skk-cursor-abbrev-color)
   (skk-jisx0208-latin-mode
    skk-cursor-jisx0208-latin-color)
   (skk-katakana
    skk-cursor-katakana-color)
   (skk-j-mode
    skk-cursor-hiragana-color)
   (skk-jisx0201-mode
    skk-cursor-jisx0201-color)
   (t
    skk-cursor-latin-color)))

;;;###autoload
(defun skk-cursor-set-1 (color)
  (static-cond
   ((eq skk-emacs-type 'xemacs)
    ;;At 10 Jul 2000 16:37:49 +0900,
    ;;Yoshiki Hayashi <t90553@mail.ecc.u-tokyo.ac.jp> wrote:
    ;;> foreground $B$r(B background $B$KJQ$($kI,MW$,$"$k$3$H0J30$O!":#$N(B
    ;;> $B$H$3$m$=$N$^$^$GF0$$$F$$$k$h$&$G$9!#$7$P$i$/(B test $B$7$F$_$^$9!#(B
    ;;> $B$I$&$b!"(Btext-cursor $B$bIaDL$N(B face $B$N$h$&$G!"(Bforeground $B$,J8(B
    ;;> $B;z$N?'$r!"(Bbackground $B$,J8;z$NGX7J$N?'$rI=$7$F$$$k$h$&$G$9!#(B
    (set-face-property 'text-cursor
		       'background
		       (or color
			   (skk-cursor-current-color))
		       (current-buffer)))
   (t
    (set-buffer-local-cursor-color
     (or color
	 (skk-cursor-current-color))))))

;;;###autoload
(defun skk-cursor-off-1 ()
  (static-cond
   ((eq skk-emacs-type 'xemacs)
    (skk-cursor-set))
   (t
    (set-cursor-color-buffer-local nil))))

;; advices.
(static-when (eq skk-emacs-type 'xemacs)
  (skk-defadvice minibuffer-keyboard-quit (before skk-cursor-ad activate)
    (unless skk-henkan-mode
      (skk-cursor-set (skk-cursor-default-color)))))

;; Hooks
(static-when (eq skk-emacs-type 'xemacs)
  (add-hook 'isearch-mode-end-hook
	    'skk-cursor-set
	    'append)

  (add-hook 'minibuffer-setup-hook
	    'skk-cursor-set
	    'append)

  (add-hook 'minibuffer-exit-hook
	    (lambda ()
	      (with-current-buffer (skk-minibuffer-origin)
		(skk-cursor-set))
	      (skk-cursor-set skk-cursor-default-color 'force))
	    'append))

(require 'product)
(product-provide
    (provide 'skk-cursor)
  (require 'skk-version))

;;; skk-cursor.el ends here
