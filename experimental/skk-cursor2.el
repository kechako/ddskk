;;; skk-cursor2.el --- yet another SKK cursor control.
;; Copyright (C) 1996, 1997, 1998, 1999, 2000
;; Masatake YAMATO <masata-y@is.aist-nara.ac.jp> 

;; Author: Masatake YAMATO <masata-y@is.aist-nara.ac.jp>
;; Maintainer: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Version: $Id: skk-cursor2.el,v 1.1.2.1 2000/07/02 13:30:42 minakaji Exp $
;; Keywords: japanese
;; Last Modified: $Date: 2000/07/02 13:30:42 $

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
(eval-when-compile (require 'static) (require 'skk-macs) (require 'skk-vars))
(require 'ccc)

(setq ccc-before-update-function
      (function (lambda () (setq buffer-local-cursor-color (skk-cursor-current-color)))))

;;; functions.
(defun skk-cursor-current-color ()
  ;; $B%+%l%s%H%P%C%U%!$N(B SKK $B$N%b!<%I$+$i!"%+!<%=%k$N?'$r<hF@$9$k!#(B
  (cond ((not skk-mode) skk-cursor-default-color)
	;; skk-start-henkan $B$NCf$G$O!"(Bskk-j-mode $B%U%i%0$rN)$F$J$,$i!"(B
	;; skk-abbrev-mode $B%U%i%0$bN)$F$F$$$k(B ($BJQ498e!"D>8e$KF~NO$9$kJ8;z$,(B
	;; $B85$NF~NO%b!<%I$K$F9T$J$o$l$k$h$&$K(B)$B!#=>$$!"(Bskk-abbrev-mode $B%U%i(B
	;; $B%0$N%A%'%C%/$NM%@hEY$r>e$2$k!#(B
	(skk-abbrev-mode skk-cursor-abbrev-color)
	(skk-jisx0208-latin-mode skk-cursor-jisx0208-latin-color)
	(skk-katakana skk-cursor-katakana-color)
	(skk-j-mode skk-cursor-hiragana-color)
	((and (boundp 'skk-jisx0201-mode) skk-jisx0201-mode)
	 skk-cursor-jisx0201-color)
	(t skk-cursor-latin-color)))

;;; advices.
(defvar skk-cursor2-buffer-local-frame-params-ad-targets
  '(
    ;; cover to SKK functions.
    skk-abbrev-mode
    skk-auto-fill-mode
    skk-jisx0201-mode
    skk-jisx0208-latin-mode
    skk-kakutei
    skk-latin-mode
    skk-mode
    skk-toggle-kana
    ))

(let ((funcs skk-cursor2-buffer-local-frame-params-ad-targets))
  (while funcs
    (eval
     (`
      (defadvice (, (intern (symbol-name (car funcs))))
	(after skk-cursor2-ad activate)
	"Update frame frame parameters if `buffer-local-*-color' given."
	(update-buffer-local-frame-params)
	)))
    (setq funcs (cdr funcs))))

;;; Hooks
;;(add-hook 'isearch-mode-end-hook 'update-buffer-local-frame-params 'append)

(provide 'skk-cursor2)
;;; Local Variables:
;;; End:
;;; skk-cursor2.el ends here
