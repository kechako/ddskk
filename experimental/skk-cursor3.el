;;; skk-cursor3.el --- yet another SKK cursor control.
;; Copyright (C) 2000 Mikio Nakajima <minakaji@osaka.email.ne.jp>

;; Author: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Maintainer: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Version: $Id: skk-cursor3.el,v 1.1.2.1 2000/07/07 23:55:46 minakaji Exp $
;; Keywords: japanese
;; Last Modified: $Date: 2000/07/07 23:55:46 $

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
;; for XEmacs only.

;;; Code:
(or (skk-color-display-p) (error "SKK-CURSOR requires color display"))
(eval-when-compile (require 'skk-macs) (require 'skk-vars))

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
(defadvice skk-kakutei (after skk-cursor3-ad activate)
  (set-face-property 'text-cursor 'foreground (skk-cursor-current-color)
		     (current-buffer)))

(defadvice skk-mode (after skk-cursor3-ad activate)
  (set-face-property 'text-cursor 'foreground
		     (cond ((not skk-mode) skk-cursor-default-color)
			   (skk-katakana skk-cursor-katakana-color)
			   (skk-j-mode skk-cursor-hiragana-color)
			   (t skk-cursor-latin-color))
		     (current-buffer)))

(defadvice skk-auto-fill-mode (after skk-cursor3-ad activate)
  (set-face-property 'text-cursor 'foreground
		     (cond ((not skk-mode) skk-cursor-default-color)
			   (skk-katakana skk-cursor-katakana-color)
			   (skk-j-mode skk-cursor-hiragana-color)
			   (t skk-cursor-latin-color))
		     (current-buffer)))

(defadvice skk-abbrev-mode (after skk-cursor3-ad activate)
  (set-face-property 'text-cursor 'foreground skk-cursor-abbrev-color
		     (current-buffer)))

(defadvice skk-jisx0201-mode (after skk-cursor3-ad activate)
  (set-face-property 'text-cursor 'foreground skk-cursor-jisx0201-color
		     (current-buffer)))

(defadvice skk-jisx0208-latin-mode (after skk-cursor3-ad activate)
  (set-face-property 'text-cursor 'foreground skk-cursor-jisx0208-latin-color
		     (current-buffer)))

(defadvice skk-latin-mode (after skk-cursor3-ad activate)
  (set-face-property 'text-cursor 'foreground skk-cursor-latin-color
		     (current-buffer)))

(defadvice skk-toggle-kana (after skk-cursor3-ad activate)
  (set-face-property 'text-cursor 'foreground
		     (cond (skk-katakana skk-cursor-katakana-color)
			   (skk-j-mode skk-cursor-hiragana-color))
		     (current-buffer)))

;;; Hooks
(add-hook 'isearch-mode-end-hook
	  (lambda ()
	    (set-face-property 'text-cursor 'foreground
			       (skk-cursor-current-color) (current-buffer)))
	  'append)

(add-hook 'minibuffer-setup-hook 
	  (lambda ()
	    (set-face-property 'text-cursor 'foreground
			       (skk-cursor-current-color) (current-buffer)))
	  'append)

(add-hook 'minibuffer-exit-hook
	  (lambda ()
	    (with-current-buffer (nth 1 (buffer-list))
	      (set-face-property 'text-cursor 'foreground
				 (skk-cursor-current-color) (current-buffer))))
	      'append)

(provide 'skk-cursor3)
;;; Local Variables:
;;; End:
;;; skk-cursor3.el ends here
