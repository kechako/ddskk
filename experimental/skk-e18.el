;; skk-e18.el --- emacs 18 specific functions for skk.el
;; Copyright (C) 2000 Tsukamoto Tetsuo

;; Author: Tsukamoto Tetsuo <czkmt@remus.dti.ne.jp>
;; Keywords: japanese

;; This file is not part of Daredevil SKK yet.

;; Daredevil SKK is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either versions 2, or (at your option)
;; any later version.

;; Daredevil SKK is distributed in the hope that it will be useful
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with SKK, see the file COPYING.  If not, write to the Free
;; Software Foundation Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary:

;; Daredevil SKK $B$r(B Emacs 18 $B%Y!<%9$GMxMQ$9$k$?$a$N(B work around $B$G$9!#(B
;; $B$^$@$[$H$s$IF0$$$F$$$J$$>u67$G$9!#8=:_F0:n3NG'$G$-$k4D6-$O(B
;;
;;     o Nemacs 3.3.2 based on Emacs 18.59
;;
;; $B$K8B$i$l$F$$$^$9!#(B
;; Daredevil SKK $B$O(B advice.el $B$rI,MW$H$7$^$9!#(BEmacs 18 $B$GMxMQ$G$-$k(B
;; advice.el $B$O(B
;;
;;     ftp://ftp.cs.buffalo.edu/pub/Emacs/
;;
;; $B$K$*$$$FG[I[$5$l$F$$$^$9!#(B

;;; Code:
(condition-case nil
    (require 'advice)
  (error
   (error "advice.el is required for This version of SKK.")))

(defconst skk-use-color-cursor nil)
(defconst skk-cursor-change-width nil)

(require 'skk-macs)
(require 'skk-vars)

;; Variables.
(defvar auto-fill-function nil)
(defvar unread-command-events nil)

(defconst skk-hankaku-alist
  '((161 . 32)	; ?\ 
    (170 . 33)	;?\!
    (201 . 34)	;?\"
    (244 . 35)	;?\#
    (240 . 36)	;?\$
    (243 . 37)	;?\%
    (245 . 38)	;?\&
    (199 . 39)	;?\'
    (202 . 40)	;?\(
    (203 . 41)	;?\)
    (246 . 42)	;?\*
    (220 . 43)	;?\+
    (164 . 44)	;?\,
    (221 . 45)	;?\-
    (165 . 46)	;?\.
    (191 . 47)	;?\/
    (167 . 58)	;?\:
    (168 . 59)	;?\;
    (227 . 60)	;?\<
    (225 . 61)	;?\=
    (228 . 62)	;?\>
    (169 . 63)	;?\?
    (247 . 64)	;?\@
    (206 . 91)	;?\[
    (239 . 92)	;?\\
    (207 . 93)	;?\]
    (176 . 94)	;?^ 
    (178 . 95)	;?\_
    (208 . 123)	;?\{
    (195 . 124)	;?\|
    (209 . 125)	;?\}
    (177 . 126)	;?\~
    (198 . 96))	;?` 
  "$BJ8;z%3!<%I$N(B 2 $BHVL\$N%P%$%H$H$=$NJ8;z$KBP1~$9$k(B ascii $BJ8;z(B \(char\) $B$H$NO"A[%j%9%H!#(B
Mule l $B$b$7$/$O(B  Mule 2 $B$r;HMQ$9$k>l9g$K(B skk-latin-region $B$G;2>H$9$k!#(B
Mule-2.3 $BE:IU$N(B egg.el $B$h$j%3%T!<$7$?!#(B")

(skk-deflocalvar skk-current-local-map nil)

(defvar skk-e18-self-insert-keys
  (append (where-is-internal 'self-insert-command global-map)
	  (where-is-internal 'canna-self-insert-command global-map)
	  (where-is-internal 'egg-self-insert-command global-map)
	  '("\t")))

;; Macros.
(defmacro save-match-data (&rest body)
  "Execute the BODY forms, restoring the global value of the match data."
  (let ((original (make-symbol "match-data")))
    (list 'let (list (list original '(match-data)))
          (list 'unwind-protect
                (cons 'progn body)
                (list 'store-match-data original)))))

;; Inline functions.
(defsubst skk-mode-off ()
  (setq skk-mode nil
	skk-abbrev-mode nil
	skk-latin-mode nil
	skk-j-mode nil
	skk-jisx0208-latin-mode nil
	;; j's sub mode.
	skk-katakana nil)
  ;; initialize
  (setq skk-input-mode-string skk-hiragana-mode-string)
  (use-local-map skk-current-local-map)
  (force-mode-line-update)
  (remove-hook 'pre-command-hook 'skk-pre-command 'local))

(defsubst skk-j-mode-on (&optional katakana)
  (setq skk-mode t
	skk-abbrev-mode nil
	skk-latin-mode nil
	skk-j-mode t
	skk-jisx0208-latin-mode nil
	;; j's sub mode.
	skk-katakana katakana)
  (use-local-map skk-j-mode-map)
  (force-mode-line-update))

(defsubst skk-latin-mode-on ()
  (setq skk-mode t
	skk-abbrev-mode nil
	skk-latin-mode t
	skk-j-mode nil
	skk-jisx0208-latin-mode nil
	;; j's sub mode.
	skk-katakana nil
	skk-input-mode-string skk-latin-mode-string)
  (use-local-map skk-latin-mode-map)
  (force-mode-line-update))

(defsubst skk-jisx0208-latin-mode-on ()
  (setq skk-mode t
	skk-abbrev-mode nil
	skk-latin-mode nil
	skk-j-mode nil
	skk-jisx0208-latin-mode t
	;; j's sub mode.
	skk-katakana nil
	skk-input-mode-string skk-jisx0208-latin-mode-string)
  (use-local-map skk-jisx0208-latin-mode-map)
  (force-mode-line-update))

(defsubst skk-abbrev-mode-on ()
  (setq skk-mode t
	skk-abbrev-mode t
	skk-latin-mode nil
	skk-j-mode nil
	skk-jisx0208-latin-mode nil
	;; j's sub mode.
	skk-katakana nil
	skk-input-mode-string skk-abbrev-mode-string)
  (use-local-map skk-abbrev-mode-map)
  (force-mode-line-update))

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

;; Other functions.
(defun-maybe window-minibuffer-p (&optional window)
"Returns non-nil if WINDOW is a minibuffer window."
  (eq (selected-window) (minibuffer-window)))

(defun-maybe float (arg)
  arg)

(defun-maybe insert-and-inherit (&rest args)
  (apply 'insert args))

(defun-maybe number-to-string (num)
  (format "%d" num))

(when (eq skk-emacs-type 'mule1)
  (defun insert-file-contents-as-coding-system
    (coding-system filename &optional visit beg end replace)
    "Like `insert-file-contents', q.v., but CODING-SYSTEM the first arg will
be applied to `file-coding-system-for-read'."
    (insert-file-contents filename visit coding-system)))

(defun skk-e18-setup ()
  (let ((map (if (skk-in-minibuffer-p) minibuffer-local-map
	       (current-local-map)))
	(i 0))
    ;;
    (mapcar (function
	     (lambda (skk-map)
	       (set skk-map (if map (copy-keymap map) (make-sparse-keymap)))))
	    '(skk-current-local-map
	      skk-latin-mode-map
	      skk-j-mode-map
	      skk-jisx0208-latin-mode-map))
    ;;
    (define-key skk-latin-mode-map skk-kakutei-key 'skk-kakutei)
    ;;
    (mapcar (function
	     (lambda (key)
	       (define-key skk-j-mode-map key 'skk-insert)))
	    skk-e18-self-insert-keys)
    ;;
    (define-key skk-jisx0208-latin-mode-map skk-kakutei-key 'skk-kakutei)
    (while (< i 128)
      (and (aref skk-jisx0208-latin-vector i)
	   (define-key skk-jisx0208-latin-mode-map
	     (char-to-string i) 'skk-jisx0208-latin-insert))
      (setq i (1+ i)))
    (define-key skk-jisx0208-latin-mode-map "\C-q" 'skk-latin-henkan)))

(or skk-mode (skk-e18-setup))

;; Hooks.
(add-hook
 'skk-load-hook
 (function
  (lambda ()
    ;; Pieces of advice.
    (defadvice skk-mode (before skk-nemacs-ad activate)
      (or skk-mode (skk-e18-setup)))

    ;; Functions.
    (defun skk-emulate-original-map (arg)
      ;; $B%-!<F~NO$KBP$7$F!"(BSKK $B$N%b!<%I$G$O$J$/!"(BEmacs $B$N%*%j%8%J%k$N%-!<3d$jIU$1$G(B
      ;; $B%3%^%s%I$r<B9T$9$k!#(B
      (let ((prefix-arg arg)
	    (keys (skk-command-key-sequence (this-command-keys) this-command)))
	(if (not keys)
	    ;; no alternative commands.  may be invoked by M-x.
	    nil
	  (let (command local-map buf)
	    (unwind-protect
		(progn
		  (setq buf (current-buffer)
			local-map (current-local-map))
		  (use-local-map skk-current-local-map)
		  (setq command (key-binding keys))
		  (if (eq command this-command)
		      ;; avoid recursive calling of skk-emulate-original-map.
		      nil
		    ;; if no bindings are found, call `undefined'.  it's
		    ;; original behaviour.
		    (skk-cancel-undo-boundary)
		    (command-execute (or command (function undefined)))))
	      ;; restore skk keymap.
	      (save-excursion
		(set-buffer buf)
		(use-local-map local-map)))))))
    (defun skk-jisx0208-to-ascii (string)
      (let ((char
	     (cond ((eq skk-emacs-type 'mule1)
		    (let* ((ch (string-to-char string))
			   (ch1 (char-component ch 1)))
		      (cond ((eq 161 ch1)	; ?\241
			     (cdr (assq (char-component ch 2) skk-hankaku-alist)))
			    ((eq 163 ch1)	; ?\243
			     (- (char-component ch 2) 128) ; ?\200
			     ))))
		   ((eq skk-emacs-type 'nemacs)
		    (let* ((ch1 (aref string 0)))
		      (cond ((eq 161 ch1)	; ?\241
			     (cdr (assq (aref string 1) skk-hankaku-alist)))
			    ((eq 163 ch1)	; ?\243
			     (- (aref string 1) 128) ; ?\200
			     )))))))
	(and char (char-to-string char))))
    ;;
    )))

;;
(provide 'skk-e18)

;;; skk-e18.el ends here
