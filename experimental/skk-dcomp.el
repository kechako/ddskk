;;; skk-dcomp.el --- SKK dynamic completion
;; Copyright (C) 1999, 2000 Mikio Nakajima <minakaji@osaka.email.ne.jp>

;; Author: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Version: $Id: skk-dcomp.el,v 1.13 2000/12/03 23:35:15 minakaji Exp $
;; Keywords: japanese
;; Last Modified: $Date: 2000/12/03 23:35:15 $

;; This file is part of Daredevil SKK.

;; Daredevil SKK is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either versions 2, or (at your option)
;; any later version.

;; Daredevil SKK is distributed in the hope that it will be useful
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Daredevil SKK, see the file COPYING.  If not, write to the
;; Free Software Foundation Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary
;;
;; $B$3$l$O8+=P$78l$NF~NO$r!"<+F0E*$K%@%$%J%_%C%/$K%3%s%W%j!<%7%g%s$9$k(B
;; $B%W%m%0%i%`$G$9!#A}0f=SG7(B $B$5$s$,3+H/$7$F$$$k(B POBox $B$d(B MS Excel $B$N%;(B
;; $B%k$G$NJ8;zF~NO$K1F6A$r<u$1$F$$$^$9(B (POBox $B$H$O%$%s%?%U%'%$%9<+?H$O(B
;; $B>/$70[$J$j$^$9$,!"F0:n$O$3$A$i$NJ}$,$+$J$j9bB.$J$O$:$G$9(B)$B!#(B
;;
;; <INSTALL>
;; skk-11/experimental/skk-dcomp.el $B$r(B skk-11/skk-dcomp.el $B$K%3%T!<$7(B
;; $B$F8e$OIaDL$K(B make $B$7$F2<$5$$!#(Bskk-dcomp.el $B$,%$%s%9%H!<%k$5$l!"(B
;; autoload $B$N@_Dj$,<+F0E*$K@8@.$5$l$^$9!#(B
;;
;; <HOW TO USE>
;; .emacs $B$b$7$/$O(B .skk $B$K(B (require 'skk-dcomp) $B$H=q$-$^$7$g$&!#$=$l$@(B
;; $B$1$G$9!#(B
;;
;; <HOW TO WORK>
;; SKK $B$rIaDL$K;H$$;O$a$F$_$F2<$5$$!#$-$C$H:G=i$O6C$/$O$:$G$9$,!"%$%s(B
;; $B%?%U%'%$%9$O8@MU$G@bL@$9$k$h$j$b!"BN46$7$?J}$,Aa$$$H;W$$$^$9(B ;-)$B!#(B
;; 
;;; Code:
(eval-when-compile (require 'skk))
(require 'skk-comp)

(defgroup skk-dcomp nil "SKK dynamic completion related customization."
  :prefix "skk-dcomp-"
  :group 'skk)

(defface skk-dcomp-face
  '((((class color)) (:foreground "DarkKhaki"))
    (((class grayscale) (background light)) (:foreground "DimGray" :italic t))
    (((class grayscale) (background dark)) (:foreground "LightGray" :italic t)))
  "*Face used to highlight region dynamically completed."
  :group 'skk-dcomp
  :group 'skk-faces)

(defcustom skk-dcomp-face-priority 700
  "*Overlay/extent priority of `skk-dcomp-face'."
  :type 'integer
  :group 'skk-dcomp)

(defcustom skk-dcomp-keep-completion-keys nil
  ;;   (delq
  ;;    nil
  ;;    (list
  ;;     (car (rassoc (list nil 'skk-toggle-kana) skk-rom-kana-rule-list))
  ;;     (car (rassoc (list nil 'skk-toggle-characters) skk-rom-kana-rule-list))
  ;;     (car (rassoc (list nil 'skk-toggle-kana) skk-rom-kana-base-rule-list))
  ;;     (car (rassoc (list nil 'skk-toggle-characters) skk-rom-kana-base-rule-list))))
  "*$B<+F0Jd40$5$l$?8+=P$78l$r>C$5$J$$%-!<$N%j%9%H!#(B
$BDL>o$O8+=P$78l$NJd408e!"<!$N%-!<F~NO$r$9$k$H!"<+F0Jd40$5$l$?%-!<F~NO$,>C$($F(B
$B$7$^$&$,!"$3$N%j%9%H$K;XDj$5$l$?%-!<F~NO$,$"$C$?$H$-$O<+F0Jd40$5$l$?8+=P$78l(B
$B$r>C$5$J$$!#(B"
  :type '(choice (repeat string) (const nil))
  :group 'skk-dcomp
  :group 'skk-filenames)

(skk-deflocalvar skk-dcomp-start-point nil)
(skk-deflocalvar skk-dcomp-end-point nil)
(skk-deflocalvar skk-dcomp-extent nil)
(defvar skk-dcomp-face 'skk-dcomp-face)

;; functions.
(defsubst skk-extentp (object)
  (static-cond
   ((eq skk-emacs-type 'xemacs) (extentp object))
   (t (overlayp object))))

(defun skk-dcomp-face-on (start end)
  (skk-face-on skk-dcomp-extent start end skk-dcomp-face
	       skk-dcomp-face-priority))

(defun skk-dcomp-face-off ()
  (skk-detach-extent skk-dcomp-extent))

;; main dynamic completion engine.
(defadvice skk-kana-input (around skk-dcomp-ad activate)
  (if (not skk-henkan-on)
      ad-do-it
    (if (or skk-henkan-active (skk-get-prefix skk-current-rule-tree)
	    (not skk-comp-stack))
	(progn
	  (skk-set-marker skk-dcomp-start-point nil)
	  (skk-set-marker skk-dcomp-end-point nil))
      (when (and (marker-position skk-dcomp-start-point)
		 (marker-position skk-dcomp-end-point))
	(skk-dcomp-face-off)
	(or (member (this-command-keys) skk-dcomp-keep-completion-keys)
	    (condition-case nil
		(delete-region skk-dcomp-start-point skk-dcomp-end-point)
	      (error)))))
    ad-do-it
    (if (and (not (skk-get-prefix skk-current-rule-tree))
	     (not skk-okurigana))
	(let ((pos (point)))
	  (condition-case nil
	      (progn
		(skk-comp-do 'first 'silent)
		(skk-set-marker skk-dcomp-start-point pos)
		(skk-set-marker skk-dcomp-end-point (point))
		(skk-dcomp-face-on skk-dcomp-start-point skk-dcomp-end-point)
		(goto-char skk-dcomp-start-point))
	    (error
	     (setq skk-comp-stack nil)
	     (message nil)))))))

(defadvice skk-kakutei (around skk-dcomp-ad activate)
  (if (and skk-henkan-on (not skk-henkan-active)
	   (markerp skk-dcomp-start-point)
	   (markerp skk-dcomp-end-point)
	   (marker-position skk-dcomp-start-point)
	   (marker-position skk-dcomp-end-point))
      (progn
	(skk-dcomp-face-off)
	(condition-case nil
	    (delete-region skk-dcomp-start-point skk-dcomp-end-point)
	  (error))))
  ad-do-it
  (skk-set-marker skk-dcomp-start-point nil)
  (skk-set-marker skk-dcomp-end-point nil)
  (setq skk-comp-stack nil))

(defadvice skk-start-henkan (before skk-dcomp-ad activate)
  (if (and (markerp skk-dcomp-start-point)
	   (markerp skk-dcomp-end-point)
	   (marker-position skk-dcomp-start-point)
	   (marker-position skk-dcomp-end-point))
      (progn
	(skk-dcomp-face-off)
	(delete-region skk-dcomp-end-point (point))
	(skk-set-marker skk-dcomp-end-point (point)))))

(skk-defadvice keyboard-quit (around skk-dcomp-ad activate)
  (if (and skk-henkan-on (not skk-henkan-active)
	   (marker-position skk-dcomp-start-point)
	   (marker-position skk-dcomp-end-point))
      (progn
	(skk-dcomp-face-off)
	(condition-case nil
	    (delete-region skk-dcomp-start-point skk-dcomp-end-point)
	  (error))))
  ad-do-it
  (skk-set-marker skk-dcomp-start-point nil)
  (skk-set-marker skk-dcomp-end-point nil)
  (setq skk-comp-stack nil))

(defadvice skk-comp (around skk-dcomp-ad activate)
  (if (and (marker-position skk-dcomp-start-point)
	   (marker-position skk-dcomp-end-point))
      (progn
	(goto-char skk-dcomp-end-point)
	(setq this-command 'skk-comp-do)
	(skk-dcomp-face-off)
	(skk-set-marker skk-dcomp-start-point nil)
	(skk-set-marker skk-dcomp-end-point nil))
    ad-do-it))

(require 'product)
(product-provide (provide 'skk-dcomp) (require 'skk-version))
;;; Local Variables:
;;; End:
;;; skk-dcomp.el ends here
