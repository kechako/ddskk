;;; skk-macs.el --- Macros for SKK.
;; Copyright (C) 1999 Mikio Nakajima <minakaji@osaka.email.ne.jp>

;; Author: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Maintainer: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Version: $Id: skk-macs.el,v 1.1.2.1 1999/11/07 14:45:13 minakaji Exp $
;; Keywords: japanese
;; Last Modified: $Date: 1999/11/07 14:45:13 $

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
;;
;;; Code:

;;;; macros
(eval-when-compile (require 'static))

(defmacro skk-defun-cond (name args &optional doc &rest everything-else)
  (or (stringp doc)
      (setq everything-else (cons doc everything-else)
	    doc nil))
  (` (prog1
	 (static-cond
	  (,@ (mapcar
	       (function
		(lambda (case)
		  (list (car case)
			(if doc
			    (` (defun (, name) (, args)
				 (, doc)
				 (,@ (cdr case))))
			  (` (defun (, name) (, args)
			       (,@ (cdr case))))))))
	       everything-else)))
       (setq current-load-list
	     (cons (quote (, name)) current-load-list))
       )))

(defmacro skk-defsubst-cond (name args &optional doc &rest everything-else)
  (or (stringp doc)
      (setq everything-else (cons doc everything-else)
	    doc nil))
  (` (prog1
	 (static-cond
	  (,@ (mapcar
	       (function
		(lambda (case)
		  (list (car case)
			(if doc
			    (` (defsubst (, name) (, args)
				 (, doc)
				 (,@ (cdr case))))
			  (` (defsubst (, name) (, args)
			       (,@ (cdr case))))))))
	       everything-else)))
       (setq current-load-list
	     (cons (quote (, name)) current-load-list))
       )))

(defmacro skk-defmacro-cond (name args &optional doc &rest everything-else)
  (or (stringp doc)
      (setq everything-else (cons doc everything-else)
	    doc nil))
  (` (prog1
	 (static-cond
	  (,@ (mapcar
	       (function
		(lambda (case)
		  (list (car case)
			(if doc
			    (` (defmacro (, name) (, args)
				 (, doc)
				 (,@ (cdr case))))
			  (` (defmacro (, name) (, args)
			       (,@ (cdr case))))))))
	       everything-else)))
       (setq current-load-list
	     (cons (quote (, name)) current-load-list)))))

;; Why I use non-intern temporary variable in the macro --- see comment in
;; save-match-data of subr.el of GNU Emacs. And should we use the same manner
;; in the save-current-buffer, with-temp-buffer and with-temp-file macro
;; definition?
(defmacro skk-save-point (&rest body)
  (` (let ((skk-save-point (point-marker)))
       (unwind-protect
	   (progn (,@ body))
	 (goto-char skk-save-point)
         (skk-set-marker skk-save-point nil) ))))

(defmacro skk-message (japanese english &rest arg)
  ;; skk-japanese-message-and-error $B$,(B non-nil $B$@$C$?$i(B JAPANESE $B$r(B nil $B$G$"$l(B
  ;; $B$P(B ENGLISH $B$r%(%3!<%(%j%"$KI=<($9$k!#(B
  ;; ARG $B$O(B message $B4X?t$NBh#20z?t0J9_$N0z?t$H$7$FEO$5$l$k!#(B
  (append (list 'message (list 'if 'skk-japanese-message-and-error
			       japanese english ))
	  arg ))

(defmacro skk-error (japanese english &rest arg)
  ;; skk-japanese-message-and-error $B$,(B non-nil $B$@$C$?$i(B JAPANESE $B$r(B nil $B$G$"$l(B
  ;; $B$P(B ENGLISH $B$r%(%3!<%(%j%"$KI=<($7!"%(%i!<$rH/@8$5$;$k!#(B
  ;; ARG $B$O(B error $B4X?t$NBh#20z?t0J9_$N0z?t$H$7$FEO$5$l$k!#(B
  (append (list 'error (list 'if 'skk-japanese-message-and-error
			     japanese english ))
	  arg ))

(defmacro skk-yes-or-no-p (japanese english)
  ;; skk-japanese-message-and-error $B$,(B non-nil $B$G$"$l$P!"(Bjapanese $B$r(B nil $B$G$"(B
  ;; $B$l$P(B english $B$r%W%m%s%W%H$H$7$F(B yes-or-no-p $B$r<B9T$9$k!#(B
  ;; yes-or-no-p $B$N0z?t$N%W%m%s%W%H$,J#;($KF~$l9~$s$G$$$k>l9g$O$3$N%^%/%m$r;H(B
  ;; $B$&$h$j%*%j%8%J%k$N(B yes-or-no-p $B$r;HMQ$7$?J}$,%3!<%I$,J#;($K$J$i$J$$>l9g$,(B
  ;; $B$"$k!#(B
  (list 'yes-or-no-p (list 'if 'skk-japanese-message-and-error
				   japanese english )))

(defmacro skk-y-or-n-p (japanese english)
  ;; skk-japanese-message-and-error $B$,(B non-nil $B$G$"$l$P!"(Bjapanese $B$r(B nil $B$G$"(B
  ;; $B$l$P(B english $B$r%W%m%s%W%H$H$7$F(B y-or-n-p $B$r<B9T$9$k!#(B
  (list 'y-or-n-p (list 'if 'skk-japanese-message-and-error
				japanese english )))

(defmacro skk-set-marker (marker position &optional buffer)
  ;; $B%P%C%U%!%m!<%+%kCM$G$"$k(B skk-henkan-start-point, skk-henkan-end-point,
  ;; skk-kana-start-point, $B$"$k$$$O(B skk-okurigana-start-point $B$,(B nil $B$@$C$?$i!"(B
  ;; $B?75,%^!<%+!<$r:n$C$FBeF~$9$k!#(B
  (list 'progn
        (list 'if (list 'not marker)
              (list 'setq marker (list 'make-marker)) )
        (list 'set-marker marker position buffer) ))

;; From viper-util.el.  Welcome!
(defmacro skk-deflocalvar (var default-value &optional documentation)
  (` (progn
       (defvar (, var) (, default-value)
	       (, (format "%s\n\(buffer local\)" documentation)))
       (make-variable-buffer-local '(, var))
     )))

(defmacro skk-with-point-move (&rest form)
  ;; $B%]%$%s%H$r0\F0$9$k$,%U%C%/$r<B9T$7$F$[$7$/$J$$>l9g$K;H$&!#(B
  (` (unwind-protect
	 (progn (,@ form))
       (setq skk-previous-point (point)) )))

(skk-defmacro-cond skk-face-on (object start end face &optional priority)
  ((eq skk-emacs-type 'xemacs)
   (` (let ((inhibit-quit t))
	(if (not (extentp (, object)))
	    (progn
	      (setq (, object) (make-extent (, start) (, end)))
	      (if (not (, priority))
		  (set-extent-face (, object) (, face))
		(set-extent-properties
		 (, object) (list 'face (, face) 'priority (, priority)) )))
	  (set-extent-endpoints (, object) (, start) (, end))  ))))
  (t
   (` (let ((inhibit-quit t))
	(if (not (overlayp (, object)))
	    (progn
	      (setq (, object) (make-overlay (, start) (, end)))
	      (and (, priority) (overlay-put (, object) 'priority (, priority)))
	      (overlay-put (, object) 'face (, face)) )
	  (move-overlay (, object) (, start) (, end)) )))))

(put 'skk-deflocalvar 'lisp-indent-function 'defun)
(put 'skk-defmacro-cond 'lisp-indent-function 'defun)
(put 'skk-defsubst-cond 'lisp-indent-function 'defun)
(put 'skk-defun-cond  'lisp-indent-function 'defun)

;;(defun-maybe mapvector (function sequence)
;;  "Apply FUNCTION to each element of SEQUENCE, making a vector of the results.
;;The result is a vector of the same length as SEQUENCE.
;;SEQUENCE may be a list, a vector or a string."
;;  (vconcat (mapcar function sequence) nil) )

;;(defun-maybe mapc (function sequence)
;;  "Apply FUNCTION to each element of SEQUENCE.
;;SEQUENCE may be a list, a vector, a bit vector, or a string.
;;--- NOT emulated enough, just discard newly constructed list made by mapcar ---
;;This function is like `mapcar' but does not accumulate the results,
;;which is more efficient if you do not use the results."
;;  (mapcar function sequence)
;;  sequence )

(provide 'skk-macs)
;;; end of skk-macs.el.

