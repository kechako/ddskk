;;; skk-foreword.el --- $BA0=q$-(B
;; Copyright (C) 1997, 1998, 1999 Mikio Nakajima <minakaji@osaka.email.ne.jp>

;; Author: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Maintainer: Hideki Sakurada <sakurada@kuis.kyoto-u.ac.jp>
;;             Murata Shuuichirou  <mrt@astec.co.jp>
;;             Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Version: $Id: skk-foreword.el,v 1.11 1999/09/25 11:12:46 minakaji Exp $
;; Keywords: japanese
;; Last Modified: $Date: 1999/09/25 11:12:46 $

;; This file is not part of SKK yet.

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

;; $B$3$N%U%!%$%k$O!"%f!<%6!<JQ?t$N@k8@<+BN$K;HMQ$9$k%^%/%m!"(Bskk-*.el $B$G(B
;; $B;HMQ$9$k%^%/%m$J$I!"JQ?t$N@k8@0JA0!"(Bskk-*.el $B$N:G=i$KDj5A$7$F$*$+$J(B
;; $B$1$l$P$J$i$J$$$b$N$r$^$H$a$?$b$N$G$9!#%f!<%6!<JQ?t$NDj5A$NA0$K!"$4(B
;; $B$A$c$4$A$c$H%f!<%6!<$K6=L#$,$J$$$b$N$,JB$s$G$$$?$N$G$O!"%f!<%6!<%U(B
;; $B%l%s%I%j!<$G$O$J$$$H9M$($k$+$i$G$9!#(B
;;
;; Following people contributed to skk-foreword.el (Alphabetical order):
;;       $B>.Ln(B $B9'CK(B <takao@hirata.nuee.nagoya-u.ac.jp>
;;       Hideki Sakurada <sakurada@kuis.kyoto-u.ac.jp>
;;       Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>
;;       Rei FURUKAWA <furukawa@tcp-ip.or.jp>
;;       Shuhei KOBAYASHI <shuhei-k@jaist.ac.jp>
;;       TSUMURA Tomoaki <tsumura@kuis.kyoto-u.ac.jp>

;;; Change log:

;;; Code:
(cond ((or (and (boundp 'epoch::version) epoch::version)
	   (string< (substring emacs-version 0 2) "18") )
       (error "THIS SKK requires Emacs 19 or later") )
      ((not (featurep 'mule))
       (error "THIS SKK requires MULE features") ))

(eval-when-compile
  (defvar skk-abbrev-cursor-color)
  (defvar skk-abbrev-mode)
  (defvar skk-abbrev-mode-string)
  (defvar skk-current-rule-tree)
  (defvar skk-default-cursor-color)
  (defvar skk-downcase-alist)
  (defvar skk-echo)
  (defvar skk-hankaku-alist)
  (defvar skk-henkan-count)
  (defvar skk-henkan-list)
  (defvar skk-hiragana-cursor-color)
  (defvar skk-hiragana-mode-string)
  (defvar skk-input-mode-string)
  (defvar skk-j-mode)
  (defvar skk-jisx0208-latin-cursor-color)
  (defvar skk-jisx0208-latin-mode)
  (defvar skk-jisx0208-latin-mode-string)
  (defvar skk-kana-cleanup-command-list)
  (defvar skk-kana-input-search-function)
  (defvar skk-kana-start-point)
  (defvar skk-katakana)
  (defvar skk-katakana-cursor-color)
  (defvar skk-katakana-mode-string)
  (defvar skk-last-henkan-data)
  (defvar skk-latin-cursor-color)
  (defvar skk-latin-mode)
  (defvar skk-latin-mode-string)
  (defvar skk-look-completion-words)
  (defvar skk-mode)
  (defvar skk-prefix)
  (defvar skk-previous-point)
  (defvar skk-use-numeric-conversion) )

(require 'advice)
(require 'easymenu)
;; APEL 9.22 or later required.
(eval-when-compile (require 'static))
(require 'poe)
(require 'poem) ; requires pces.
(require 'pcustom)
(require 'alist)
;; Elib 1.0 is required.
(require 'queue-m)

;;;###autoload
(eval-and-compile
  (defconst skk-emacs-type (cond ((string-match "XEmacs" emacs-version) 'xemacs)
				 ((and (boundp 'mule-version)
				       (string< "4.0" mule-version) 'mule4 ))
				 ((and (boundp 'mule-version)
				       (string< "3.0" mule-version) 'mule3 ))
				 ((and (boundp 'mule-version)
				       (string< "2.0" mule-version) 'mule2 )))))

(defconst skk-package-data-directory
  (if (boundp 'early-packages)
      (let ((dirs (append (if early-package-load-path early-packages)
			  (if late-package-load-path late-packages)
			  (if last-package-load-path last-packages) ))
	    dir )
	(while (not (file-exists-p (setq dir (car dirs))))
	  (setq dirs (cdr dirs)) )
	(and dir
	     (expand-file-name "skk" (expand-file-name "etc" dir)) ))))

;;(princ (format "SKK-PACKAGE-DATA-DIRECTORY is %s\n"
;;	       skk-package-data-directory ))

;; necessary macro and functions to be declared before user variable declarations.

;;;; macros

;; Who uses SKK without compilaition?
;;(eval-when-compile

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

;;;;) ;eval-when-compile
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

;;;; inline functions
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
  (skk-set-cursor-color skk-default-cursor-color)
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
  ;; mode line
  (if katakana
      (progn
        (setq skk-input-mode-string skk-katakana-mode-string)
        (skk-set-cursor-color skk-katakana-cursor-color) )
    (setq skk-input-mode-string skk-hiragana-mode-string)
    (skk-set-cursor-color skk-hiragana-cursor-color) )
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
  (skk-set-cursor-color skk-latin-cursor-color)
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
  (skk-set-cursor-color skk-jisx0208-latin-cursor-color)
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
  (skk-set-cursor-color skk-abbrev-cursor-color)
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

(defsubst skk-string<= (str1 str2)
  ;; STR1 $B$H(B STR2 $B$H$rHf3S$7$F!"(Bstring< $B$+(B string= $B$G$"$l$P!"(Bt $B$rJV$9!#(B
  (or (string< str1 str2) (string= str1 str2)) )

(defsubst skk-do-auto-fill ()
  ;; auto-fill-function $B$KCM$,BeF~$5$l$F$*$l$P!"(Bdo-auto-fill $B$r%3!<%k$9$k!#(B
  (and auto-fill-function (funcall auto-fill-function)) )

;;;; from dabbrev.el.  Welcome!
;; $BH=Dj4V0c$$$rHH$9>l9g$"$j!#MW2~NI!#(B
(defsubst skk-minibuffer-origin ()
  (nth 1 (buffer-list)) )

(defsubst skk-current-insert-mode ()
  (cond (skk-abbrev-mode 'abbrev)
	(skk-latin-mode 'latin)
	(skk-jisx0208-latin-mode 'jisx0208-latin)
	(skk-katakana 'katakana)
	(skk-j-mode 'hiragana) ))

(defsubst skk-numeric-p ()
  (and skk-use-numeric-conversion (require 'skk-num) skk-num-list) )

(defsubst skk-substring-head-character (string)
  (char-to-string (string-to-char string)) )

(defsubst skk-get-current-candidate-simply (&optional noconv)
  (if (> skk-henkan-count -1)
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

(defsubst skk-pre-command ()
  (and (memq last-command '(skk-insert skk-previous-candidate))
       (null (memq this-command skk-kana-cleanup-command-list))
       (skk-kana-cleanup t) ))

(defsubst skk-make-raw-arg (arg)
  (cond ((= arg 1) nil)
	((= arg -1) '-)
	((numberp arg) (list arg)) ))

(defsubst skk-unread-event (event)
  ;; Unread single EVENT.
  (setq unread-command-events (nconc unread-command-events (list event))) )

(defsubst skk-after-point-move ()
  (and (or (not skk-previous-point) (not (= skk-previous-point (point))))
       (skk-get-prefix skk-current-rule-tree)
       (skk-with-point-move (skk-erase-prefix 'clean)) ))

;;(defsubst skk-get-current-henkan-data (key)
;;  (cdr (assq key skk-current-henkan-data)) )

;;(defsubst skk-put-current-henkan-data (key val)
;;  (setq skk-current-henkan-data (put-alist key val skk-current-henkan-data)) )

(defsubst skk-get-last-henkan-data (key)
  (cdr (assq key skk-last-henkan-data)) )

(defsubst skk-put-last-henkan-data (key val)
  (setq skk-last-henkan-data (put-alist key val skk-last-henkan-data)) )

(defun skk-terminal-face-p ()
  (and (not window-system)
       (fboundp 'frame-face-alist) ;; $BJQ?tL>$_$?$$$J4X?t$@$J(B...$B!#(B
       (fboundp 'selected-frame) ))

;;;; aliases
;; for backward compatibility.
(define-obsolete-function-alias 'skk-zenkaku-mode 'skk-jisx0208-latin-mode)
(define-obsolete-function-alias 'skk-zenkaku-mode-on 'skk-jisx0208-latin-mode-on)
(define-obsolete-function-alias 'skk-zenkaku-insert 'skk-jisx0208-latin-insert)
(define-obsolete-function-alias 'skk-zenkaku-region 'skk-jisx0208-latin-region)
(define-obsolete-function-alias 'skk-zenkaku-henkan 'skk-jisx0208-latin-henkan)
(define-obsolete-function-alias 'skk-ascii-mode-on 'skk-latin-mode-on)
(define-obsolete-function-alias 'skk-ascii-mode 'skk-latin-mode)
(define-obsolete-function-alias 'skk-ascii-region 'skk-latin-region)
(define-obsolete-function-alias 'skk-ascii-henkan 'skk-latin-henkan)
(define-obsolete-function-alias 'skk-convert-ad-to-gengo 'skk-ad-to-gengo)
(define-obsolete-function-alias 'skk-convert-gengo-to-ad 'skk-gengo-to-ad)
(define-obsolete-function-alias 'skk-isearch-forward 'isearch-forward)
(define-obsolete-function-alias 'skk-isearch-forward-regexp 'isearch-forward-regexp)
(define-obsolete-function-alias 'skk-isearch-backward 'isearch-backward)
(define-obsolete-function-alias 'skk-isearch-backward-regexp 'isearch-backward-regexp)

(defconst skk-background-mode
  ;; from font-lock-make-faces of font-lock.el  Welcome!
  (cond
   ((eq skk-emacs-type 'xemacs)
    (if (< (apply '+ (color-rgb-components
                      (face-property 'default 'background) ))
           (/ (apply '+ (color-rgb-components
                         (make-color-specifier "white"))) 3))
        'dark
      'light ))
   ((and window-system (x-display-color-p))
    (let ((bg-resource (x-get-resource ".backgroundMode"
                                       "BackgroundMode"))
          params )
      (if bg-resource
          (intern (downcase bg-resource))
        (setq params (frame-parameters))
        (cond ((cdr (assq 'background-mode params)));; Emacs20.x (Meadow)
	      ((and (eq system-type 'windows-nt);; Mule for Win32
                    (fboundp 'win32-color-values) )
               (< (apply '+ (win32-color-values
                             (cdr (assq 'background-color params)) ))
                  (/ (apply '+ (win32-color-values "white")) 3) )
               'dark )
              ((and (memq system-type '(ms-dos windows-nt))
                    (fboundp 'x-color-values) )
               (if (string-match "light"
                                 (cdr (assq 'background-color params)) )
                   'light
                 'dark ))
              ((< (apply '+ (x-color-values
                             (cdr (assq 'background-color params)) ))
                  (/ (apply '+ (x-color-values "white")) 3) )
               'dark )
              (t 'light) ))))
   (t 'mono) ))

;;;; version specific matter.
;;; inline functions.
(skk-defsubst-cond skk-str-length (str)
  ((memq skk-emacs-type '(xemacs mule4))
   (length str) )
  ((eq skk-emacs-type 'mule3)
   (length (string-to-vector str)) )
  ((eq skk-emacs-type 'mule2)
   (length (string-to-char-list str)) ))

(skk-defsubst-cond skk-substring (str pos1 pos2)
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
       (mapconcat 'char-to-string sl "") ))))

;; no argument use only in SKK.
(skk-defsubst-cond skk-read-event ()
  ((eq skk-emacs-type 'xemacs)
   (next-command-event) )
  (t (read-event)) )

(skk-defsubst-cond skk-char-to-string (char)
  ((eq skk-emacs-type 'xemacs)
   (char-to-string char) )
  ((string< "20" emacs-version)
   (condition-case nil (char-to-string char) (error)) )
  (t (char-to-string char)) )

(skk-defsubst-cond skk-ascii-char-p (char)
  ;; CHAR $B$,(B ascii $BJ8;z$@$C$?$i(B t $B$rJV$9!#(B
  ((memq skk-emacs-type '(xemacs mule4 mule3))
   (eq (char-charset char) 'ascii) )
  ((eq skk-emacs-type 'mule2)
   (= (char-leading-char char) 0) ))
 
(skk-defsubst-cond skk-str-ref (str pos)
  ((memq skk-emacs-type '(xemacs mule4))
   (aref str pos) )
  ((eq skk-emacs-type 'mule3)
   (aref (string-to-vector str) pos ) )
  ((eq skk-emacs-type 'mule2)
   (nth pos (string-to-char-list str)) ))

(skk-defsubst-cond skk-jisx0208-p (char)
  ((memq skk-emacs-type '(xemacs mule4 mule3))
   (eq (char-charset char) 'japanese-jisx0208) )
  ((eq skk-emacs-type 'mule2)
   (= (char-leading-char char) lc-jp) ))

(skk-defsubst-cond skk-char-octet (ch &optional n)
  ((eq skk-emacs-type 'xemacs)
   (or (nth (if n (1+ n) 1) (split-char ch)) 0) )
  (t (char-octet ch n)) )

;;; normal functions.
;; tiny function, but called once in skk-kcode.el.  So not make it inline.
;; or should I think to move to skk-kcode.el?
(skk-defun-cond skk-make-char (charset n1 n2)
  ((eq skk-emacs-type 'xemacs)
   (make-char charset (logand (lognot 128) n1) (logand (lognot 128) n2)) )
  ((memq skk-emacs-type '(mule4 mule3))
   (make-char charset n1 n2) )
  ((eq skk-emacs-type 'mule2)
   (make-character charset n1 n2) ))

;; this one is called once in skk-kcode.el, too.
(skk-defsubst-cond skk-charsetp (object)
  ((and (eq skk-emacs-type 'xemacs) (fboundp 'charsetp))
   (charsetp object) )
  ((eq skk-emacs-type 'xemacs)
   ;; Is there XEmacs that doesn't have `charsetp'?
   (find-charset object) )
  ((memq skk-emacs-type '(mule4 mule3))
   (charsetp object) )
  ((eq skk-emacs-type 'mule2)
   (character-set object) ))

(skk-defun-cond skk-jisx0208-to-ascii (string)
  ((memq skk-emacs-type '(xemacs mule4 mule3))
   (require 'japan-util)
   (let ((char
	  (get-char-code-property (string-to-char string) 'ascii) ))
     (and char (char-to-string char)) ))
  ((eq skk-emacs-type 'mule2)
   (let ((char
	  (let* ((ch (string-to-char string))
		 (ch1 (char-component ch 1)) )
	    (cond ((eq 161 ch1)		; ?\241
		   (cdr (assq (char-component ch 2) skk-hankaku-alist)) )
		  ((eq 163 ch1)		; ?\243
		   (- (char-component ch 2) 128) ; ?\200
		   )))))
     (and char (char-to-string char)) )))

(defun skk-define-menu-bar-map (map)
  ;; SKK $B%a%K%e!<$N%H%C%W$K=P8=$9$k%3%^%s%I$N%a%K%e!<$X$NDj5A$r9T$J$&!#(B
  (easy-menu-define
   skk-menu map
   "Menu used in SKK mode."
   '("SKK"
     ("Convert Region and Echo"
      ("Gyakubiki"
       ["to Hiragana" skk-gyakubiki-message
        (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0)) ]
       ["to Hiragana, All Candidates"
        ;; $B$"$l$l!"(Blambda $B4X?t$ODj5A$G$-$J$$$N$+!)!)!)(B  $BF0$+$J$$$>(B...$B!#(B
        (function (lambda (start end) (interactive "r")
                    (skk-gyakubiki-message start end 'all-candidates) ))
        (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0)) ]
       ["to Katakana" skk-gyakubiki-katakana-message
        (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0)) ]
       ["to Katakana, All Candidates"
        (function (lambda (start end) (interactive "r")
                    (skk-gyakubiki-katakana-message
                     start end 'all-candidates ) ))
        (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0)) ]
       )
      ("Hurigana"
       ["to Hiragana" skk-hurigana-message
        (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0)) ]
       ["to Hiragana, All Candidates"
        (function (lambda (start end) (interactive "r")
                    (skk-hurigana-message start end 'all-candidates) ))
        (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0)) ]
       ["to Katakana" skk-hurigana-katakana-message
        (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0)) ]
       ["to Katakana, All Candidates"
        (function (lambda (start end) (interactive "r")
                    (skk-hurigana-katakana-message
                     start end 'all-candidates) ))
        (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0)) ]
       )
      )
     ("Convert Region and Replace"
      ["Ascii" skk-ascii-region
       (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0)) ]
      ("Gyakubiki"
       ["to Hiragana" skk-gyakubiki-region
        (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0)) ]
       ["to Hiragana, All Candidates"
        (function (lambda (start end) (interactive "r")
                    (skk-gyakubiki-region start end 'all-candidates) ))
        (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0)) ]
       ["to Katakana" skk-gyakubiki-katakana-region
        (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0)) ]
       ["to Katakana, All Candidates"
        (function (lambda (start end) (interactive "r")
                    (skk-gyakubiki-katakana-region
                     start end 'all-candidates ) ))
        (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0)) ]
       )
      ["Hiragana" skk-hiragana-region
       (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0)) ]
      ("Hurigana"
       ["to Hiragana" skk-hurigana-region
        (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0)) ]
       ["to Hiragana, All Candidates"
        (function (lambda (start end) (interactive "r")
                    (skk-hurigana-region start end 'all-candidates) ))
        (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0)) ]
       ["to Katakana" skk-hurigana-katakana-region
        (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0)) ]
       ["to Katakana, All Candidates" (function
                                       (lambda (start end) (interactive "r")
                                         (skk-hurigana-katakana-region
                                          start end 'all-candidates) ))
        (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0)) ]
       )
      ["Katakana" skk-katakana-region
       (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0)) ]
      ["Romaji" skk-romaji-region
       (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0)) ]
      ["Zenkaku" skk-jisx0208-latin-region
       (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0)) ]
      )
     ["Count Jisyo Candidates" skk-count-jisyo-candidates
      (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0)) ]
     ["Save Jisyo" skk-save-jisyo
      (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0)) ]
     ["Undo Kakutei" skk-undo-kakutei
      (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0)) ]
     ["Version" skk-version
      (or (not (boundp 'skktut-problem-count))
          (eq skktut-problem-count 0)) ]
     )))

(provide 'skk-foreword)
;;; Local Variables:
;;; End:
;;; skk-forwords.el ends here
