;;; skk-macs.el --- Macros and inline functions commonly use in 
;;                  Daredevil SKK package programs.
;; Copyright (C) 1999 Mikio Nakajima <minakaji@osaka.email.ne.jp>

;; Author: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Maintainer: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Version: $Id: skk-macs.el,v 1.1.2.4.2.1 1999/11/29 12:51:44 mrt Exp $
;; Keywords: japanese
;; Last Modified: $Date: 1999/11/29 12:51:44 $

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
;; along with Daredevil SKK, see the file COPYING.  If not, write to the Free
;; Software Foundation Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary:

;;; Code:
;;;; macros
(eval-when-compile
  (require 'static) (require 'skk-vars)
  (defconst skk-emacs-type
    (cond ((string-match "XEmacs" emacs-version) 'xemacs)
	  ((and (boundp 'mule-version)
		(string< "4.0" mule-version) 'mule4 ))
	  ((and (boundp 'mule-version)
		(string< "3.0" mule-version) 'mule3 ))
	  ((and (boundp 'mule-version)
		(string< "2.0" mule-version) 'mule2 )))))

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

(defmacro skk-face-on (object start end face &optional priority)
  (static-cond
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
	   (move-overlay (, object) (, start) (, end)) ))))))

(put 'skk-deflocalvar 'lisp-indent-function 'defun)

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

;; $B%D%j!<$K%"%/%;%9$9$k$?$a$N%$%s%?!<%U%'!<%9(B
(defsubst skk-make-rule-tree (char prefix nextstate kana branch-list)
  (list char
	prefix
	(if (string= nextstate "") nil nextstate)
	kana
	branch-list ))

(defsubst skk-get-char (tree)
  (car tree) )

(defsubst skk-set-char (tree char)
  (setcar tree char) )

(defsubst skk-set-prefix (tree prefix)
  (setcar (nthcdr 1 tree) prefix) )

(defsubst skk-get-prefix (tree)
  (nth 1 tree) )

(defsubst skk-get-nextstate (tree)
  (nth 2 tree) )

(defsubst skk-set-nextstate (tree nextstate)
  (if (string= nextstate "") (setq nextstate nil))
  (setcar (nthcdr 2 tree) nextstate) )

(defsubst skk-get-kana (tree)
  (nth 3 tree) )

(defsubst skk-set-kana (tree kana)
  (setcar (nthcdr 3 tree) kana) )

(defsubst skk-get-branch-list (tree)
  (nth 4 tree) )

(defsubst skk-set-branch-list (tree branch-list)
  (setcar (nthcdr 4 tree) branch-list) )

;; tree procedure for skk-kana-input.
(defsubst skk-add-branch (tree branch)
  (skk-set-branch-list tree (cons branch (skk-get-branch-list tree))) )

(defsubst skk-select-branch (tree char)
  (assq char (skk-get-branch-list tree)) )

(defsubst skk-erase-prefix (&optional clean)
  ;; skk-echo $B$,(B non-nil $B$G$"$l$P%+%l%s%H%P%C%U%!$KA^F~$5$l$?(B skk-prefix $B$r>C(B
  ;; $B$9!#%*%W%7%g%J%k0z?t$N(B CLEAN $B$,;XDj$5$l$k$H!"JQ?t$H$7$F$N(B skk-prefix $B$r(B
  ;; null $BJ8;z$K!"(Bskk-current-rule-tree $B$r(B nil $B=i4|2=$9$k!#(B
  ;;
  ;; $B$+$JJ8;z$NF~NO$,$^$@40@.$7$F$$$J$$>l9g$K$3$N4X?t$,8F$P$l$?$H$-$J$I$O!"%P%C(B
  ;; $B%U%!$KA^F~$5$l$F$$$k(B skk-prefix $B$O:o=|$7$?$$$,!"JQ?t$H$7$F$N(B skk-prefix $B$O(B
  ;; null $BJ8;z$K$7$?$/$J$$!#(B
  (and skk-echo skk-kana-start-point
       (not (string= skk-prefix ""))	; fail safe.
       ;; skk-prefix $B$N>C5n$r%"%s%I%%$NBP>]$H$7$J$$!#(B
       (let ((buffer-undo-list t)
	     (start (marker-position skk-kana-start-point)) )
	 (and start
	      (condition-case nil
		  (delete-region start (+ start (length skk-prefix)))
		(error
		 (skk-set-marker skk-kana-start-point nil) 
		 (setq skk-prefix ""
		       skk-current-rule-tree nil ))))))
  (and clean (setq skk-prefix ""
		   skk-current-rule-tree nil ))) ; fail safe

(defsubst skk-kana-cleanup (&optional force)
  (let ((data (or
	       (and skk-current-rule-tree
		    (null (skk-get-nextstate skk-current-rule-tree))
		    (skk-get-kana skk-current-rule-tree) )
	       (and skk-kana-input-search-function
		    (car (funcall skk-kana-input-search-function)) )))
	kana )
	(if (or force data)
	    (progn
	      (skk-erase-prefix 'clean)
	      (setq kana (if (functionp data) (funcall data nil) data))
	      (if (consp kana)
		  (setq kana (if skk-katakana (car kana) (cdr kana))) )
	      (if (stringp kana) (skk-insert-str kana))
	      (skk-set-marker skk-kana-start-point nil)
	      t ))))

(defsubst skk-numeric-p ()
  (and skk-use-numeric-conversion (require 'skk-num) skk-num-list) )

(defsubst skk-file-exists-and-writable-p (file)
  (and (setq file (expand-file-name file))
       (file-exists-p file) (file-writable-p file) ))

(defsubst skk-lower-case-p (char)
  ;; CHAR $B$,>.J8;z$N%"%k%U%!%Y%C%H$G$"$l$P!"(Bt $B$rJV$9!#(B
  (and (<= ?a char) (>= ?z char) ))

(defsubst skk-downcase (char)
  (or (cdr (assq char skk-downcase-alist)) (downcase char)) )

(defsubst skk-mode-off ()
  (setq skk-mode nil
        skk-abbrev-mode nil
        skk-latin-mode nil
        skk-j-mode nil
        skk-jisx0208-latin-mode nil
        ;; j's sub mode.
        skk-katakana nil )
  ;; initialize
  (setq skk-input-mode-string skk-hiragana-mode-string)
  (force-mode-line-update)
  (remove-hook 'pre-command-hook 'skk-pre-command 'local) )

(defsubst skk-j-mode-on (&optional katakana)
  (setq skk-mode t
        skk-abbrev-mode nil
        skk-latin-mode nil
        skk-j-mode t
        skk-jisx0208-latin-mode nil
        ;; j's sub mode.
        skk-katakana katakana )
  (force-mode-line-update) )

(defsubst skk-latin-mode-on ()
  (setq skk-mode t
        skk-abbrev-mode nil
        skk-latin-mode t
        skk-j-mode nil
        skk-jisx0208-latin-mode nil
        ;; j's sub mode.
        skk-katakana nil
        skk-input-mode-string skk-latin-mode-string )
  (force-mode-line-update) )

(defsubst skk-jisx0208-latin-mode-on ()
  (setq skk-mode t
        skk-abbrev-mode nil
        skk-latin-mode nil
        skk-j-mode nil
        skk-jisx0208-latin-mode t
        ;; j's sub mode.
        skk-katakana nil
        skk-input-mode-string skk-jisx0208-latin-mode-string )
  (force-mode-line-update) )

(defsubst skk-abbrev-mode-on ()
  (setq skk-mode t
        skk-abbrev-mode t
        skk-latin-mode nil
        skk-j-mode nil
        skk-jisx0208-latin-mode nil
        ;; j's sub mode.
        skk-katakana nil
        skk-input-mode-string skk-abbrev-mode-string )
  (force-mode-line-update) )

(defsubst skk-in-minibuffer-p ()
  ;; $B%+%l%s%H%P%C%U%!$,%_%K%P%C%U%!$+$I$&$+$r%A%'%C%/$9$k!#(B
  (window-minibuffer-p (selected-window)) )

(defsubst skk-insert-prefix (&optional char)
  ;; skk-echo $B$,(B non-nil $B$G$"$l$P%+%l%s%H%P%C%U%!$K(B skk-prefix $B$rA^F~$9$k!#(B
  (and skk-echo
       ;; skk-prefix $B$NA^F~$r%"%s%I%%$NBP>]$H$7$J$$!#A^F~$7$?%W%l%U%#%C%/%9$O!"(B
       ;; $B$+$JJ8;z$rA^F~$9$kA0$KA4$F>C5n$9$k$N$G!"$=$N4V!"(Bbuffer-undo-list $B$r(B
       ;; t $B$K$7$F%"%s%I%%>pJs$rC_$($J$/$H$bLdBj$,$J$$!#(B
       (let ((buffer-undo-list t))
         (insert-and-inherit (or char skk-prefix)) )))

(defsubst skk-string<= (str1 str2)
  ;; STR1 $B$H(B STR2 $B$H$rHf3S$7$F!"(Bstring< $B$+(B string= $B$G$"$l$P!"(Bt $B$rJV$9!#(B
  (or (string< str1 str2) (string= str1 str2)) )

(defsubst skk-do-auto-fill ()
  ;; auto-fill-function $B$KCM$,BeF~$5$l$F$*$l$P!"(Bdo-auto-fill $B$r%3!<%k$9$k!#(B
  (and auto-fill-function (funcall auto-fill-function)) )

(defsubst skk-current-insert-mode ()
  (cond (skk-abbrev-mode 'abbrev)
	(skk-latin-mode 'latin)
	(skk-jisx0208-latin-mode 'jisx0208-latin)
	(skk-katakana 'katakana)
	(skk-j-mode 'hiragana) ))

(defsubst skk-substring-head-character (string)
  (char-to-string (string-to-char string)) )

(defsubst skk-get-current-candidate-simply (&optional noconv)
  (if (> 0 skk-henkan-count)
      (skk-error "$B8uJd$r<h$j=P$9$3$H$,$G$-$^$;$s(B"
		 "Cannot get current candidate" )
    ;; (nth -1 '(A B C)) $B$O!"(BA $B$rJV$9$N$G!"Ii$G$J$$$+$I$&$+%A%'%C%/$9$k!#(B
    (let ((word (nth skk-henkan-count skk-henkan-list)))
      (and word
	   (if (and (skk-numeric-p) (consp word))
	       (if noconv (car word) (cdr word))
	     word )))))

;; convert skk-rom-kana-rule-list to skk-rule-tree.
;; The rule tree follows the following syntax:
;; <branch-list>    ::= nil | (<tree> . <branch-list>)
;; <tree>         ::= (<char> <prefix> <nextstate> <kana> <branch-list>)
;; <kana>         ::= (<$B$R$i$,$JJ8;zNs(B> . <$B%+%?%+%JJ8;zNs(B>) | nil
;; <char>         ::= <$B1Q>.J8;z(B>
;; <nextstate>    ::= <$B1Q>.J8;zJ8;zNs(B> | nil

(defsubst skk-make-raw-arg (arg)
  (cond ((= arg 1) nil)
	((= arg -1) '-)
	((numberp arg) (list arg)) ))

(defsubst skk-unread-event (event)
  ;; Unread single EVENT.
  (setq unread-command-events (nconc unread-command-events (list event))) )

;;(defsubst skk-get-current-henkan-data (key)
;;  (cdr (assq key skk-current-henkan-data)) )

;;(defsubst skk-put-current-henkan-data (key val)
;;  (setq skk-current-henkan-data (put-alist key val skk-current-henkan-data)) )

(defsubst skk-get-last-henkan-data (key)
  (cdr (assq key skk-last-henkan-data)) )

(defsubst skk-put-last-henkan-data (key val)
  (setq skk-last-henkan-data (put-alist key val skk-last-henkan-data)) )

(defsubst skk-find-coding-system (code)
  (cond ((and code
	      (or (coding-system-p code)
		  (and (fboundp 'find-coding-system)
		       (find-coding-system code) )))
	 code )
	((and code (stringp code))
	 (cdr (assoc code skk-coding-system-alist)) )
	(t (cdr (assoc "euc" skk-coding-system-alist))) ))

;;;; from dabbrev.el.  Welcome!
;; $BH=Dj4V0c$$$rHH$9>l9g$"$j!#MW2~NI!#(B
(defsubst skk-minibuffer-origin ()
  (nth 1 (buffer-list)) )

;;;; inline functions
;;;; version specific matter.
;;; inline functions.
(defsubst skk-str-length (str)
  (static-cond
   ((memq skk-emacs-type '(xemacs mule4))
    (length str) )
   ((eq skk-emacs-type 'mule3)
    (length (string-to-vector str)) )
   ((eq skk-emacs-type 'mule2)
    (length (string-to-char-list str)) )))

(defsubst skk-substring (str pos1 pos2)
  (static-cond
   ((memq skk-emacs-type '(xemacs mule4))
    (substring str pos1 pos2) )
   ((eq skk-emacs-type 'mule3)
    (if (< pos1 0)
	(setq pos1 (+ (skk-str-length str) pos1)) )
    (if (< pos2 0)
	(setq pos2 (+ (skk-str-length str) pos2)) )
    (if (>= pos1 pos2)
	""
      (let ((sl (nthcdr pos1 (string-to-char-list str))))
	(setcdr (nthcdr (- pos2 pos1 1) sl) nil)
	(concat sl) )))
   ((eq skk-emacs-type 'mule2)
    (if (< pos1 0)
	(setq pos1 (+ (skk-str-length str) pos1)) )
    (if (< pos2 0)
	(setq pos2 (+ (skk-str-length str) pos2)) )
    (if (>= pos1 pos2)
	""
      (let ((sl (nthcdr pos1 (string-to-char-list str))))
	(setcdr (nthcdr (- pos2 pos1 1) sl) nil)
	(mapconcat 'char-to-string sl "") )))))

;; no argument use only in SKK.
(defsubst skk-read-event ()
  (static-cond
   ((eq skk-emacs-type 'xemacs)
    (next-command-event) )
   (t (read-event)) ))

(defsubst skk-char-to-string (char)
  (static-cond
   ((eq skk-emacs-type 'xemacs)
    (char-to-string char) )
   ((string< "20" emacs-version)
    (condition-case nil (char-to-string char) (error)) )
   (t (char-to-string char)) ))

(defsubst skk-ascii-char-p (char)
  ;; CHAR $B$,(B ascii $BJ8;z$@$C$?$i(B t $B$rJV$9!#(B
  (static-cond
   ((memq skk-emacs-type '(xemacs mule4 mule3))
    (eq (char-charset char) 'ascii) )
   ((eq skk-emacs-type 'mule2)
    (= (char-leading-char char) 0) )))
 
(defsubst skk-str-ref (str pos)
  (static-cond
   ((memq skk-emacs-type '(xemacs mule4))
    (aref str pos) )
   ((eq skk-emacs-type 'mule3)
    (aref (string-to-vector str) pos ) )
   ((eq skk-emacs-type 'mule2)
    (nth pos (string-to-char-list str)) )))

(defsubst skk-jisx0208-p (char)
  (static-cond
   ((memq skk-emacs-type '(xemacs mule4 mule3))
    (eq (char-charset char) 'japanese-jisx0208) )
   ((eq skk-emacs-type 'mule2)
    (= (char-leading-char char) lc-jp) )))

(defsubst skk-char-octet (ch &optional n)
  (static-cond
   ((eq skk-emacs-type 'xemacs)
    (or (nth (if n (1+ n) 1) (split-char ch)) 0) )
   (t (char-octet ch n)) ))

;; this one is called once in skk-kcode.el, too.
(defsubst skk-charsetp (object)
  (static-cond
   ((and (eq skk-emacs-type 'xemacs) (fboundp 'charsetp))
    (charsetp object) )
   ((eq skk-emacs-type 'xemacs)
    ;; Is there XEmacs that doesn't have `charsetp'?
    (find-charset object) )
   ((memq skk-emacs-type '(mule4 mule3))
    (charsetp object) )
   ((eq skk-emacs-type 'mule2)
    (character-set object) )))

(provide 'skk-macs)
;;; end of skk-macs.el.
