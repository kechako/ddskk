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
;; daredevil SKK $B$O(B advice.el $B$rI,MW$H$7$^$9!#(BEmacs 18 $B$GMxMQ$G$-$k(B
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
(require 'skk-macs)
(require 'skk-vars)

;; Variables.
(defvar auto-fill-function nil)
(defvar unread-command-events nil)

(skk-deflocalvar skk-current-local-map nil)
(skk-deflocalvar skk-emacs-local-map nil)

;; Macros.
(defmacro save-match-data (&rest body)
  "Execute the BODY forms, restoring the global value of the match data."
  (let ((original (make-symbol "match-data")))
    (list 'let (list (list original '(match-data)))
          (list 'unwind-protect
                (cons 'progn body)
                (list 'store-match-data original)))))

;; Inline functions.
(defsubst skk-lisp-prog-p (word)
  ;; word $B$,(B Lisp $B%W%m%0%i%`$G$"$l$P!"(Bt $B$rJV$9!#(B
  (let ((l (length word)))
    (and (> l 2)
	 (eq (aref word 0) 40)		; ?\(
	 (< (aref word 1) 128)
	 (eq (aref word (1- l)) 41))))	; ?\)

(defsubst skk-in-minibuffer-p ()
  ;; $B%+%l%s%H%P%C%U%!$,%_%K%P%C%U%!$+$I$&$+$r%A%'%C%/$9$k!#(B
  (eq (selected-window) (minibuffer-window)))

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
  (cond ((and (consp object) (consp (cdr object)))
	 ad-do-it)
	(t
	 nil)))

(defadvice search-forward (after skk-e18-ad activate)
  (when ad-return-value
    (setq ad-return-value (match-end 0))))

(defadvice re-search-forward (after skk-e18-ad activate)
  (when ad-return-value
    (setq ad-return-value (match-end 0))))

;; Other functions.
(defun-maybe float (arg)
  arg)

(defun-maybe insert-and-inherit (&rest args)
  (apply 'insert args))

(defun-maybe read-event (&rest args)
  (read-char))

(defun-maybe number-to-string (num)
  (format "%d" num))

(defun skk-e18-define-j-mode-map (map)
  (mapcar (function
	   (lambda (key)
	     (define-key map key 'skk-insert)))
	  '("!" "#" "$" "%" "&" "'" "*" "+" "," "-" "." "/"
	    "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"
	    ":" ";" "<" "=" ">" "?" "@"
	    "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M"
	    "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"
	    "\ " "\"" "\(" "\)" "\[" "\\" "\]" "\{" "\}" "\t" "^" "_" "`"
	    "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m"
	    "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"
	    "|" "~")))

(defun skk-e18-setup ()
  (let ((map (if (skk-in-minibuffer-p) minibuffer-local-map
	       (current-local-map)))
	(i 0))
    ;;
    (setq skk-current-local-map (if map (copy-keymap map)))
    ;;
    (setq skk-latin-mode-map (if map (copy-keymap map) (make-sparse-keymap)))
    (define-key skk-latin-mode-map skk-kakutei-key 'skk-kakutei)
    ;;
    (setq skk-j-mode-map (if map (copy-keymap map) (make-sparse-keymap)))
    (skk-e18-define-j-mode-map skk-j-mode-map)
    ;;
    (setq skk-jisx0208-latin-mode-map (if map (copy-keymap map) (make-sparse-keymap)))
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

    (defun skk-make-temp-file (prefix)
      (let ((dir
	     (cond ((skk-file-exists-and-writable-p temporary-file-directory)
		    (expand-file-name temporary-file-directory))
		   (t (or (file-exists-p "~/tmp") (make-directory "~/tmp"))
		      (or (file-writable-p "~/tmp") (set-file-modes "~/tmp" 1023))
		      "~/tmp"))))
	(make-temp-name
	 (expand-file-name prefix dir))))
    ;;
    )))

(add-hook
 'skk-kcode-load-hook
 (function
  (lambda ()
    (defun skk-make-string (n1 n2)
      (concat (char-to-string n1) (char-to-string n2))))))

;;
(provide 'skk-e18)

;;; skk-e18.el ends here
