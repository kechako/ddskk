;; skk-e18.el --- emacs 18 specific functions for skk.el
;; Copyright (C) 2000 Tsukamoto Tetsuo

;; Author: Tsukamoto Tetsuo <czkmt@remus.dti.ne.jp>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Keywords: japanese

;; This file is part of Daredevil SKK.

;; Daredevil SKK is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either versions 2, or (at your option) any later
;; version.

;; Daredevil SKK is distributed in the hope that it will be useful but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License along
;; with Daredevil SKK, see the file COPYING.  If not, write to the Free
;; Software Foundation Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

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

;; Although v18 original compiler cannot expand APEL specific macro such as
;; `defmacro-maybe' or `defun-maybe', but jwz's bytecompiler can do.
;; so require pces to expand such macros.
(require 'pces)
(require
 (condition-case nil
     (require 'advice)
   (error
    (error "%s"
	   "advice.el is required for this version of SKK.
Install patch/e18/advice.el in load-path and try again."))))

;; for safety.
(defconst skk-use-color-cursor nil)
(defconst skk-cursor-change-width nil)
(defconst skk-use-face nil)

(require 'skk-macs)
(require 'skk-vars)

;; Variables.
(defvar skk-e18-self-insert-keys
  (append (where-is-internal 'self-insert-command global-map)
	  (where-is-internal 'canna-self-insert-command global-map)
	  (where-is-internal 'canna-henkan-region-or-self-insert global-map)
	  (where-is-internal 'egg-self-insert-command global-map)
	  '("\t")))

;; Can v18 original compiler expand `skk-deflocalvar'?
;; I'm not sure...
(defvar skk-current-local-map nil)
(make-variable-buffer-local 'skk-current-local-map)

(defvar-maybe pre-command-hook nil)
(defvar-maybe post-command-hook nil)
(defvar-maybe minibuffer-setup-hook nil)
(defvar-maybe minibuffer-exit-hook nil)
(defvar-maybe minor-mode-map-alist nil)

(let ((i 0) e list)
  (setq list '(skk-latin-mode-map skk-j-mode-map skk-jisx0208-latin-mode-map
				  skk-abbrev-mode-map))
  (while (setq e (nth i list))
    (set e (make-sparse-keymap))
    (setq i (1+ i)))
  ;; Defined in skk-mode.
  ;; (define-key skk-latin-mode-map skk-kakutei-key 'skk-kakutei)
  (setq i 0 list skk-e18-self-insert-keys)
  (while (setq e (nth i list))
    (define-key skk-j-mode-map e 'skk-insert)
    (setq i (1+ i)))
  ;; Defined in skk-mode.
  ;; (define-key skk-jisx0208-latin-mode-map skk-kakutei-key 'skk-kakutei)
  (setq i 0)
  (while (< i 128)
    (and (aref skk-jisx0208-latin-vector i)
	 (define-key skk-jisx0208-latin-mode-map
	   (char-to-string i) 'skk-jisx0208-latin-insert))
    (setq i (1+ i)))
  (define-key skk-jisx0208-latin-mode-map "\C-q" 'skk-latin-henkan))

;; Pieces of advice.
(defadvice exit-minibuffer (around skk-e18-ad activate)
  (let ((no-nl (and skk-egg-like-newline skk-henkan-on)))
    (when skk-henkan-on
      (unless skk-mode
	(skk-mode 1))
      (skk-kakutei))
    (if no-nl
	nil
      (setq skk-mode nil)
      ad-do-it)))

(defadvice byte-code-function-p (around skk-e18-ad activate)
  ;; $B$3$l$O0l;~$N(B APEL $B$N%P%0$KBP$7$F(B work around $B$7$?$b$N$@$+$i!":G?7$N(B
  ;; APEL $B$KBP$7$F$OITMW!#(B
  (cond ((and (consp (ad-get-arg 0)) (consp (cdr (ad-get-arg 0))))
	 ad-do-it)
	(t
	 nil)))

;; $B;~@^!"8!:w7O$N4X?t$,?tCM$rJV$9$3$H$r4|BT$7$F$$$k%3!<%I$,$"$k$?$a!"(B
;; $B$=$l$i$,F0$/$h$&$K0J2<$N(B 4 $B4X?t$X$N(B advice $B$r$9$k!#(B
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
    ;; Emacs 18.55 $B$G$O%W%m%;%9<~$j$N%3!<%I$,(B Emacs 18.59 $B$H0c$$!"(B
    ;; `call-process' $B$O>o$K(B nil $B$rJV$9$?$a!"?tCM(B (0 $B$^$?$O(B 1) $B$rJV$9$3$H$r4|(B
    ;; $BBT$7$F$$$k%3!<%I$O$&$^$/F0:n$7$J$$!#0J2<$NBP=h$O@5$7$/$J$$$,!"$I$&$7$h(B
    ;; $B$&$b$J$$!#(B
    (when (and (not (eq 0 (ad-get-arg 2)))
	       (null ad-return-value))
      (setq ad-return-value 0))))

;; Emacs 18 $B$K$*$$$F!"(B`defadvice' $B$O4X?t$NDj5A8e$K8F$P$l$kI,MW$,$"$k$i$7$$$N(B
;; $B$G!"0J2<$N4X?t$rDj5A$7$F$*$$$F!"$3$l$r(B `skk-mode-invoke' $B$+$i8F$S=P$9!#(B
(defun skk-e18-advise-skk-functions ()
  ;; It is impossible to take advantage of `pre-command-hook' and
  ;; `post-command-hook'.
  (defadvice skk-insert (after skk-e18-ad activate compile)
    (skk-e18-pre-command))

  (defadvice skk-previous-candidate (after skk-e18-ad activate compile)
    (skk-e18-pre-command))

  (defadvice skk-kana-input (before skk-e18-ad activate compile)
    ;; $B%G%P%C%0$7$F$$$J$$%P%0$N$?$a$K(B work around $B$9$k!#(B
    (when (and skk-henkan-active
	       skk-kakutei-early
	       (not skk-process-okuri-early))
      (skk-kakutei)))

  (defadvice skk-kakutei (around skk-e18-ad activate compile)
    ;; skk-tut $B$rMxMQ$7$F$$$k$H$-$J$I!"(B`skk-kakutei' $B$NA08e$G(B `skk-jisyo' $B$N(B
    ;; $BCM$,JQ$o$C$F$7$^$&$3$H$,$"$k!#$^$@%G%P%C%0$7$F$$$J$$$?$a!"(Bwork around
    ;; $B$9$k!#(B
    (let ((skk-jisyo skk-jisyo))
      (when skk-henkan-on
	(unless skk-mode
	  (skk-mode 1)))
      ad-do-it)
    (skk-after-point-move)))

;; Other functions.
(defun-maybe frame-width (&optional frame)
  "Return number of columns available for display on FRAME.
If FRAME is omitted, describe the currently selected frame."
  ;; Note that this function will be defined in APEL 10.3.
  (screen-width))

(defun read-from-minibuffer (prompt &optional
				    initial-contents keymap read hist)
  "Read a string from the minibuffer, prompting with string PROMPT.
If optional second arg INITIAL-CONTENTS is non-nil, it is a string
  to be inserted into the minibuffer before reading input.
  If INITIAL-CONTENTS is (STRING . POSITION), the initial input
  is STRING, but point is placed at position POSITION in the minibuffer.
Third arg KEYMAP is a keymap to use whilst reading;
  if omitted or nil, the default is `minibuffer-local-map'.
If fourth arg READ is non-nil, then interpret the result as a lisp object
  and return that object:
  in other words, do `(car (read-from-string INPUT-STRING))'
Fifth arg HIST is ignored in this implementatin."
  ;; This re-definition of `read-from-minibuffer' is intended to enable
  ;; `minibuffer-setup-hook' and `minibuffer-exit-hook'.  Not well tested.
  (let ((minibuf (get-minibuffer (minibuffer-depth)))
	map)
    (with-current-buffer minibuf
      ;; Note that `current-local-map' inside `minibuffer-setup-hook' should
      ;; return the 3rd arg KEYMAP.
      (use-local-map (or keymap minibuffer-local-map))
      ;;
      (when minibuffer-setup-hook
	(save-window-excursion
	  ;; Note that `(window-buffer (minibuffer-window))' should return
	  ;; the new minibuffer.
	  (set-window-buffer (minibuffer-window) (current-buffer))
	  (run-hooks 'minibuffer-setup-hook)))
      ;; The local keymap here will be passed to `si:read-from-minibuffer'.
      ;; if the 3rd arg KEYMAP is nil.
      (setq map (current-local-map)))
    ;; `minibuffer-exit-hook' should be called even on abnormai exits.
    (unwind-protect
	(si:read-from-minibuffer prompt
				 initial-contents
				 (or keymap map)
				 read)
      ;;
      (when minibuffer-exit-hook
	(with-current-buffer (if (buffer-live-p minibuf)
				 minibuf
			       (get-minibuffer (minibuffer-depth)))
	  (save-window-excursion
	    ;; Note that `(window-buffer (minibuffer-window))' should return
	    ;; the new minibuffer.
	    (set-window-buffer (minibuffer-window) (current-buffer))
	    (safe-run-hooks 'minibuffer-exit-hook)))))))

(defun safe-run-hooks (hook)
  ;; /* If we get an error while running the hook, cause the hook variable
  ;;    to be nil.  Also inhibit quits, so that C-g won't cause the hook
  ;;    to mysteriously evaporate. */
  (let ((inhibit-quit hook))
    (condition-case nil
	(run-hooks hook)
      (error
       (when (symbolp hook)
	 (set hook nil))))))

(defun get-minibuffer (depth)
  ;; /* Return a buffer to be used as the minibuffer at depth `depth'.
  ;;  depth = 0 is the lowest allowed argument, and that is the value
  ;;  used for nonrecursive minibuffer invocations */
  (let* ((name (format " *Minibuf-%d*" depth))
	 (buf (get-buffer name)))
    (cond
     ((not (buffer-live-p buf))
      (setq buf (get-buffer-create name))
      ;; Emulate Emacs 19.28.
      ;; /* Although the buffer's name starts with a space, undo should be
      ;;    enabled in it.  */
      (buffer-enable-undo buf))
     (t
      ;; reset_buffer() is called in get_minibuffer() also under Emacs 18.
      (save-current-buffer
	;; Emulate Emacs 19.34.
	(set-buffer buf)
	(kill-all-local-variables))))
    buf))

(defun skk-e18-make-local-map (map1 map2)
  ;; MAP1 $B$H(B MAP2 $B$NN>J}$N%-!<Dj5A$,;H$($k%-!<%^%C%W$rJV$9!#(B
  ;; `set-keymap-parent' $B$HF1MM$N<jK!$G$O$&$^$/$$$+$J$$$?$a!"$3$N$h$&$J>.:Y9)(B
  ;; $B$rO.$7$F$$$k!#(B
  (let ((alist1 (cdr (copy-sequence map1)))
	(alist2 (cdr (copy-sequence map2)))
	alist cell1 cell2 cell)
    (while alist1
      (setq cell nil)
      (setq cell1 (car alist1))
      (cond ((and (keymapp (cdr cell1))
		  (setq cell2 (assq (car cell1) alist2))
		  (keymapp (cdr cell2)))
	     (setq cell (cons (car cell1)
			      (skk-e18-make-local-map
			       (cdr cell1)
			       (cdr cell2))))
	     (setq alist2 (delete cell2 alist2)))
	    (t
	     (setq cell cell1)))
      (when cell
	(setq alist (nconc alist (list cell))))
      (setq alist1 (cdr alist1)))
    (while alist2
      (setq alist (nconc alist (list (car alist2))))
      (setq alist2 (cdr alist2)))
    (cons 'keymap alist)))

(defun skk-e18-pre-command ()
  ;; $B$3$N4X?t$O(B SKK 8.6 $B$N(B `j-kana-input' $B$N%3!<%I$rN.MQ$7$F$$$k!#(B
  ;;
  ;; Emacs 18 $B$K$*$$$F$O(B `pre-command-hook' $B$rMxMQ$9$k<jN)$F$,L5$$$?$a!"5lMh(B
  ;; $B$N<jK!(B (`read-char' $B$G<!$NF~NO$rJa$^$($k(B) $B$K$h$i$6$k$rF@$J$$!#(B
  (condition-case nil
      (let ((char (if (and (setq char (read-char))
			   skk-henkan-on
			   (not skk-henkan-active)
			   ;; we must distinguish the two cases where
			   ;; SKK-ECHO is on and off
			   (= skk-henkan-start-point
			      (if skk-echo (1- (point)) (point)))
			   (< 64 char) (< char 91))
		      ;; this case takes care of the rare case where
		      ;; one types two characters in upper case
		      ;; consequtively.  For example, one sometimes
		      ;; types "TE" when one should type "Te"
		      (+ 32 char)
		    char)))
	(unless (memq (key-binding (char-to-string char))
		      skk-kana-cleanup-command-list)
	  (skk-kana-cleanup t))
	(setq unread-command-char char))
    (quit
     (if (skk-in-minibuffer-p)
	 (abort-recursive-edit)
       (keyboard-quit)))))

(defun skk-e18-setup ()
  (let ((keymap (if (skk-in-minibuffer-p)
		    minibuffer-local-map
		  (current-local-map))))
    (if (and keymap (eq (lookup-key keymap "a") 'skk-insert))
	nil
      (setq skk-current-local-map keymap))))

;; Hooks.

;;(add-hook 'skk-load-hook
;;	  (function
;;	   (lambda ()
;;
;;	       ;; end case nemacs
;;	       ))))

(require 'product)
(product-provide (provide 'skk-e18) (require 'skk-version))
;;; skk-e18.el ends here
