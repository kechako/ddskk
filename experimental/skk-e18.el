;; skk-e18.el --- emacs 18 specific functions for skk.el
;; Copyright (C) 2000 Tsukamoto Tetsuo

;; Author: Tsukamoto Tetsuo <czkmt@remus.dti.ne.jp>
;; Version: $Id: skk-e18.el,v 1.1.2.24 2000/09/12 09:32:37 czkmt Exp $
;; Keywords: japanese
;; Last Modified: $Date: 2000/09/12 09:32:37 $

;; This file is not yet part of Daredevil SKK.

;; Daredevil SKK  is free software;  you  can redistribute it  and/or modify it
;; under the terms  of the GNU General Public License  as published by the Free
;; Software  Foundation;  either versions  2,  or  (at your option)  any  later
;; version.

;; Daredevil SKK is distributed in the hope that it will be useful  but WITHOUT
;; ANY  WARRANTY;  without  even  the implied  warranty  of MERCHANTABILITY  or
;; FITNESS  FOR  A PARTICULAR PURPOSE.  See the GNU General Public License  for
;; more details.

;; You should have received a copy of the GNU General Public License along with
;; Daredevil SKK,  see the file COPYING.  If not,  write  to  the Free Software
;; Foundation Inc., 59 Temple Place - Suite 330, Boston,  MA 02111-1307, USA.

;;; Commentary:

;; Daredevil SKK $B$r(B Emacs 18 $B%Y!<%9$GMxMQ$9$k$?$a$N(B work around $B$G$9!#(B
;; $B4pK\E*$J5!G=$7$+F0$-$^$;$s!#8=:_F0:n3NG'$G$-$k4D6-$O(B
;;
;;     o Nemacs 3.3.2 based on Emacs 18.59
;;
;; $B$K8B$i$l$F$$$^$9!#(B
;;
;; Daredevil SKK  $B$O(B advice.el $B$rI,MW$H$7$^$9!#(B Emacs 18 $B$GMxMQ$G$-$k(B advice.el
;; $B$O(B Daredevil SKK $B$N%"!<%+%$%V$N(B  patch/e18/ $B$H$$$&%G%#%l%/%H%j$K<}O?$5$l$F$$(B
;; $B$^$9!#(B

;;; Code:
(condition-case nil
    (require 'advice)
  (error
   (error "advice.el is required for this version of SKK.
Install patch/e18/advice.el in load-path and try again.")))

;; skk-vars.el $B$G(B default variable $B$r(B nil $B$K$7$F$*$-$^$7$?$,!"G0$N$?(B
;; $B$a!"(Bdefconst $B$7$F$*$-$^$7$g$&!#(B
(defconst skk-use-color-cursor nil)
(defconst skk-cursor-change-width nil)

(require 'skk-macs)
(require 'skk-vars)

;; Variables.
(defvar auto-fill-function nil)

;; Macros.
(defmacro-maybe save-match-data (&rest body)
  "Execute the BODY forms, restoring the global value of the match data."
  (let ((original (make-symbol "match-data")))
    (list 'let (list (list original '(match-data)))
          (list 'unwind-protect
                (cons 'progn body)
                (list 'store-match-data original)))))

;; Inline functions.
;; Pieces of advice.
(defadvice byte-code-function-p (around skk-e18-ad activate)
  (cond ((and (consp (ad-get-arg 0)) (consp (cdr (ad-get-arg 0))))
	 ad-do-it)
	(t
	 nil)))

(defadvice search-forward (after skk-e18-ad activate)
  (when ad-return-value
    (setq ad-return-value (match-end 0))))

(defadvice search-backward (after skk-e18-ad activate)
  (when ad-return-value
    (setq ad-return-value (match-beginning 0))))

(defadvice re-search-forward (after skk-e18-ad activate)
  (when ad-return-value
    (setq ad-return-value (match-end 0))))

(defadvice re-search-backward (after skk-e18-ad activate)
  (when ad-return-value
    (setq ad-return-value (match-beginning 0))))

(when (< emacs-minor-version 59)
  (defadvice call-process (after skk-e18-ad activate)
    (when (and (not (eq 0 (ad-get-arg 2)))
	       (null ad-return-value))
      (setq ad-return-value 0))))

;; Other functions.
(defun-maybe window-minibuffer-p (&optional window)
"Return non-nil if WINDOW is a minibuffer window."
  (eq (or window (selected-window)) (minibuffer-window)))

(defun-maybe overlayp (object))

(defun-maybe float (arg)
  arg)

(defun-maybe frame-width (&optional frame)
  "Return number of columns available for display on FRAME.
If FRAME is omitted, describe the currently selected frame."
  (screen-width))

(defalias-maybe 'insert-and-inherit 'insert)
(defalias-maybe 'number-to-string 'int-to-string)

(when (eq skk-emacs-type 'mule1)
  (defun insert-file-contents-as-coding-system
    (coding-system filename &optional visit beg end replace)
    "Like `insert-file-contents', q.v., but CODING-SYSTEM the first arg will
be applied to `file-coding-system-for-read'."
    (let ((file-coding-system-for-read coding-system))
      (insert-file-contents filename visit))))

;; Hooks.

;;(add-hook 'skk-load-hook
;;	  (function
;;	   (lambda ()
;;
;;	       ;; end case nemacs
;;	       ))))

(provide 'skk-e18)

;;; skk-e18.el ends here
