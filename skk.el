;;; skk.el --- SKK (Simple Kana to Kanji conversion program) Daredevil branch
;; Copyright (C) 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997,
;;               1998, 1999, 2000
;; Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>

;; Author: Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Version: $Id: skk.el,v 1.57 2000/11/15 09:41:00 czkmt Exp $
;; Keywords: japanese
;; Last Modified: $Date: 2000/11/15 09:41:00 $

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
;;
;; SKK-MODE is a mode for inputting Japanese to a current buffer which is
;; composed of four minor modes described below.
;;
;;      +----------------------+-------- skk-mode -----+----------------------+
;;      |                      |                       |                      |
;;      |                      |                       |                      |
;;  skk-j-mode           skk-latin-mode      skk-jisx0208-latin-mode   skk-abbrev-mode
;;                           ASCII               JISX0208 LATIN         ABBREVIATION
;;                  (C-j wakes up skk-j-mode)   (ZEN'KAKU EIMOJI)
;;
;; skk-j-mode-map     skk-latin-mode-map  skk-jisx0208-latin-mode-map skk-abbrev-mode-map
;; skk-katakana: nil
;;   HIRAKANA
;;
;;  skk-j-mode-map
;; skk-katakana: t
;;   KATAKANA

;;; Code:
(eval-when-compile ; shut up compiler warning.
  (defvar epoch::version)
  (defvar self-insert-after-hook)
  (defvar skk-rdbms-private-jisyo-table))

(cond ((or (and (boundp 'epoch::version) epoch::version)
	   (string< (substring emacs-version 0 2) "19"))
      (message "This version of SKK may not work on Emacs 18..."))
     ((not (or (featurep 'mule) (boundp 'NEMACS)))
      (error "This version of SKK requires MULE features")))
;; APEL 10.2 or later required.
(eval-when-compile (require 'static))
(require 'poe)
(require 'poem) ; requires pces.
(require 'pces)
(require 'pcustom)
(require 'alist)
(condition-case nil
    (require 'product)
  (error (error "This version of Daredevil SKK requires APEL/10.2 or later")))
(or (product-version>= 'apel-ver '(10 2))
    (error "This version of Daredevil SKK requires APEL/10.2 or later"))
;; Elib 1.0 is required.
(require 'queue-m)
;; Emacs 18.
(static-when (= 18 emacs-major-version) (require 'skk-e18))
;; Emacs standard library.
(require 'advice)
(condition-case nil
    (require 'easymenu)
  (error
   (defalias 'easy-menu-define 'ignore)))
(eval-and-compile (require 'skk-vars) (require 'skk-macs))

;; aliases.
(defalias 'skk-toggle-kana 'skk-toggle-characters)

;; inline functions to hook.
(defsubst skk-after-point-move ()
  (and (or (not skk-previous-point) (not (= skk-previous-point (point))))
       (skk-get-prefix skk-current-rule-tree)
       (skk-with-point-move (skk-erase-prefix 'clean))))

;; inline functions to hook.
(defsubst skk-pre-command ()
  (and (memq last-command '(skk-insert skk-previous-candidate))
       (null (memq this-command skk-kana-cleanup-command-list))
       (skk-kana-cleanup t)))

;;; normal functions.
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
(static-unless (memq skk-emacs-type '(nemacs mule1))
  (define-obsolete-function-alias 'skk-isearch-forward 'isearch-forward)
  (define-obsolete-function-alias 'skk-isearch-forward-regexp 'isearch-forward-regexp)
  (define-obsolete-function-alias 'skk-isearch-backward 'isearch-backward)
  (define-obsolete-function-alias 'skk-isearch-backward-regexp 'isearch-backward-regexp))

(static-cond
 ((memq skk-emacs-type '(nemacs mule1))
  (skk-deflocalvar skk-current-local-map nil)

  (defvar skk-e18-self-insert-keys
    (append (where-is-internal 'self-insert-command global-map)
	    (where-is-internal 'canna-self-insert-command global-map)
	    (where-is-internal 'canna-henkan-region-or-self-insert global-map)
	    (where-is-internal 'egg-self-insert-command global-map)
	    '("\t")))

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

  (defun skk-e18-setup ()
    (let ((keymap (if (skk-in-minibuffer-p)
		      minibuffer-local-map
		    (current-local-map))))
      (if (and keymap (eq (lookup-key keymap "a") 'skk-insert))
	  nil
	(setq skk-current-local-map keymap)))))
 (t
  (defun skk-define-menu-bar-map (map)
    ;; SKK $B%a%K%e!<$N%H%C%W$K=P8=$9$k%3%^%s%I$N%a%K%e!<$X$NDj5A$r9T$J$&!#(B
    (easy-menu-define
     skk-menu map
     "Menu used in SKK mode."
     '("SKK"
       ("Convert Region and Echo"
	("Gyakubiki"
	 ["to Hiragana" skk-gyakubiki-message
	  (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0))]
	 ["to Hiragana, All Candidates"
	  ;; $B$"$l$l!"(Blambda $B4X?t$ODj5A$G$-$J$$$N$+!)!)!)(B  $BF0$+$J$$$>(B...$B!#(B
	  (call-interactively
	   (function (lambda (start end) (interactive "r")
		       (skk-gyakubiki-message start end 'all-candidates))))
	  (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0))]
	 ["to Katakana" skk-gyakubiki-katakana-message
	  (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0))]
	 ["to Katakana, All Candidates"
	  (call-interactively
	   (function (lambda (start end) (interactive "r")
		       (skk-gyakubiki-katakana-message
			start end 'all-candidates))))
	  (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0))])
	("Hurigana"
	 ["to Hiragana" skk-hurigana-message
	  (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0))]
	 ["to Hiragana, All Candidates"
	  (call-interactively
	   (function (lambda (start end) (interactive "r")
		       (skk-hurigana-message start end 'all-candidates))))
	  (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0))]
	 ["to Katakana" skk-hurigana-katakana-message
	  (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0))]
	 ["to Katakana, All Candidates"
	  (call-interactively
	   (function (lambda (start end) (interactive "r")
		       (skk-hurigana-katakana-message
			start end 'all-candidates))))
	  (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0))]))
       ("Convert Region and Replace"
	["Ascii" skk-ascii-region
	 (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0))]
	("Gyakubiki"
	 ["to Hiragana" skk-gyakubiki-region
	  (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0))]
	 ["to Hiragana, All Candidates"
	  (call-interactively
	   (function (lambda (start end) (interactive "r")
		       (skk-gyakubiki-region start end 'all-candidates))))
	  (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0))]
	 ["to Katakana" skk-gyakubiki-katakana-region
	  (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0))]
	 ["to Katakana, All Candidates"
	  (call-interactively
	   (function (lambda (start end) (interactive "r")
		       (skk-gyakubiki-katakana-region
			start end 'all-candidates))))
	  (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0))])
	["Hiragana" skk-hiragana-region
	 (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0))]
	("Hurigana"
	 ["to Hiragana" skk-hurigana-region
	  (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0))]
	 ["to Hiragana, All Candidates"
	  (call-interactively
	   (function (lambda (start end) (interactive "r")
		       (skk-hurigana-region start end 'all-candidates))))
	  (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0))]
	 ["to Katakana" skk-hurigana-katakana-region
	  (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0))]
	 ["to Katakana, All Candidates" (function
					 (lambda (start end) (interactive "r")
					   (skk-hurigana-katakana-region
					    start end 'all-candidates)))
	  (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0))])
	["Katakana" skk-katakana-region
	 (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0))]
	["Romaji" skk-romaji-region
	 (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0))]
	["Zenkaku" skk-jisx0208-latin-region
	 (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0))])
       ["Count Jisyo Candidates" skk-count-jisyo-candidates
	(or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0))]
       ["Save Jisyo" skk-save-jisyo
	(or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0))]
       ["Undo Kakutei" skk-undo-kakutei
	(or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0))]
       ["Version" skk-version
	(or (not (boundp 'skktut-problem-count))
	    (eq skktut-problem-count 0))])))

  (or skk-latin-mode-map
      (let ((map (make-sparse-keymap)))
	;; .skk $B$G(B skk-kakutei-key $B$NJQ99$,2DG=$K$J$k$h$&$K!#(B
	;;(define-key map skk-kakutei-key 'skk-kakutei)
	(skk-define-menu-bar-map map)
	(setq skk-latin-mode-map map)))

  (or skk-j-mode-map
      (let ((map (make-sparse-keymap)))
	(substitute-key-definition 'self-insert-command 'skk-insert map
				   global-map)
	;; for Mule-2.x
	(substitute-key-definition 'egg-self-insert-command 'skk-insert map
				   global-map)
	(substitute-key-definition 'canna-self-insert-command 'skk-insert map
				   global-map)
	(substitute-key-definition 'canna-henkan-region-or-self-insert
				   'skk-insert map global-map)
	(substitute-key-definition 'can-n-egg-self-insert-command 'skk-insert map
				   global-map)
	;; .skk $B$G(B skk-kakutei-key $B$NJQ99$,2DG=$K$J$k$h$&$K!#(B
	;;(define-key map skk-kakutei-key 'skk-kakutei)
	(skk-define-menu-bar-map map)
	(setq skk-j-mode-map map)))

  (or skk-jisx0208-latin-mode-map
      (let ((map (make-sparse-keymap))
	    (i 0))
	(while (< i 128)
	  (and (aref skk-jisx0208-latin-vector i)
	       (define-key map (char-to-string i) 'skk-jisx0208-latin-insert))
	  (setq i (1+ i)))
	(define-key map "\C-q" 'skk-latin-henkan)
	(skk-define-menu-bar-map map)
	(setq skk-jisx0208-latin-mode-map map)))

  (or skk-abbrev-mode-map
      (let ((map (make-sparse-keymap)))
	(define-key map "," 'skk-abbrev-comma)
	(define-key map "." 'skk-abbrev-period)
	(define-key map "\C-q" 'skk-jisx0208-latin-henkan)
	;; .skk $B$G(B skk-kakutei-key $B$NJQ99$,2DG=$K$J$k$h$&$K!#(B
	;;(define-key map skk-kakutei-key 'skk-kakutei)
	(skk-define-menu-bar-map map)
	(setq skk-abbrev-mode-map map)))

  (set-modified-alist
   'minor-mode-map-alist
   (list (cons 'skk-latin-mode skk-latin-mode-map)
	 (cons 'skk-abbrev-mode skk-abbrev-mode-map)
	 (cons 'skk-j-mode skk-j-mode-map)
	 (cons 'skk-jisx0208-latin-mode skk-jisx0208-latin-mode-map)))))

;; VERSION SPECIFIC MATTERS.
(defun skk-jisx0208-to-ascii (string)
  (let ((char
	 (static-cond
	  ((memq skk-emacs-type '(xemacs mule5 mule4 mule3))
	   (require 'japan-util)
	   (get-char-code-property (string-to-char string) 'ascii))
	  ((memq skk-emacs-type '(mule2 mule1))
	   (let* ((ch (string-to-char string))
		  (ch1 (char-component ch 1)))
	     (cond ((eq ch1 ?\241)
		    (cdr (assq (char-component ch 2) skk-hankaku-alist)))
		   ((eq ch1 ?\243)
		    (- (char-component ch 2) ?\200)))))
	  ((eq skk-emacs-type 'nemacs)
	   (let ((ch1 (aref string 0)))
	     (cond ((eq ch1 ?\241)
		    (cdr (assq (aref string 1) skk-hankaku-alist)))
		   ((eq ch1 ?\243)
		    (- (aref string 1) ?\200))))))))
    (and char (char-to-string char))))

;;;###autoload
(defun skk-mode (&optional arg)
  "$BF|K\8lF~NO%b!<%I!#(B
$B%^%$%J!<%b!<%I$N0l<o$G!"%*%j%8%J%k$N%b!<%I$K$O1F6A$rM?$($J$$!#(B
$BIi$N0z?t$rM?$($k$H(B SKK $B%b!<%I$+$iH4$1$k!#(B

An input mode for Japanese, converting romanized phonetic strings to kanji.

A minor mode, it should not affect the use of any major mode or
orthogonal minor modes.

In the initial SKK mode, hiragana submode, the mode line indicator is
\"$B$+$J(B\".  Lowercase romaji entry is automatically converted to
hiragana where possible.  The lowercase characters `q' and `l' change
submodes of SKK, and `x' is used as a prefix indicating a small kana.

`q' is used to toggle between hiragana and katakana \(mode line
indicator \"$B%+%J(B\"\) entry submodes.

`l' is used to enter ASCII submode \(mode line indicator \"SKK\"\).
Uppercase `L' enters JISX0208 latin \(wide ASCII\) submode \(mode line
indicator \"$BA41Q(B\"\).  `\C-j' returns to hiragana submode from either
ASCII submode.

Kanji conversion is complex, but the basic principle is that the user
signals the appropriate stem to be matched against dictionary keys by
the use of uppercase letters.  Because SKK does not use grammatical
information, both the beginning and the end of the stem must be marked.

For non-inflected words \(eg, nouns\) consisting entirely of kanji, the
simplest way to invoke conversion is to enter the reading of the kanji,
the first character only in uppercase.  A leading \"$B"&(B\" indicates that
kanji conversion is in progress.  After entering the reading, press
space.  This invokes dictionary lookup, and the hiragana reading will be
redisplayed in kanji as the first candidate.  Pressing space again gives
the next candidate.  Further presses of space produce further candidates,
as well as a list of the next few candidates in the minibuffer.  Eg,
\"Benri\" => \"$B"&$Y$s$j(B\", and pressing space produces \"$B"'JXMx(B\" \(the solid
triangle indicates that conversion is in progress\).  Backspace steps
through the candidate list in reverse.

A candidate can be accepted by pressing `\C-j', or by entering a
self-inserting character.  \(Unlike other common Japanese input methods,
RET not only accepts the current candidate, but also inserts a line
break.\)

Inflected words \(verbs and adjectives\), like non-inflected words, begin
entry with a capital letter.  However, for these words the end of the
kanji string is signaled by capitalizing the next mora.  Eg, \"TuyoI\"
=> \"$B"'6/$$(B\".  If no candidate is available at that point, the inflection
point will be indicated with an asterisk \"*\", and trailing characters
will be displayed until a candidate is recognized.  It will be
immediately displayed \(pressing space is not necessary\).  Space and
backspace are used to step forward and backward through the list of
candidates.

For more information, see the `skk' topic in Info.  \(Japanese only.\)

A tutorial is available in Japanese or English via \"M-x skk-tutorial\".
Use a prefix argument to choose the language.  The default is system-
dependent."
  (interactive "P")
  (setq skk-mode (cond ((null arg) (not skk-mode))
                       ;; - $B$O(B -1 $B$KJQ49$5$l$k!#(B
                       ((> (prefix-numeric-value arg) 0) t)))
  (if (not skk-mode)
      ;; exit skk-mode
      (progn
        (let ((skk-mode t)) (skk-kakutei))
        (skk-mode-off)
	(remove-hook 'pre-command-hook 'skk-pre-command 'local)
	(remove-hook 'post-command-hook 'skk-after-point-move 'local)
	;; $BESCf$G@Z$jBX$($?$H$-$N$?$a$K!#(B
	(or (eq skk-status-indicator 'minor-mode)
	    (setq skk-input-mode-string ""))
	(static-if (eq skk-emacs-type 'xemacs) (easy-menu-remove skk-menu)))
    ;; enter skk-mode
    (if (not skk-mode-invoked)
        ;; enter skk-mode for the first time in this session
        (progn
	  (static-if (and (eq skk-emacs-type 'xemacs)
			  (boundp 'preloaded-file-list)
			  (member "skk-leim" preloaded-file-list))
	      ;; require dummy file.
	      (require 'skk-xm20_4))
          (skk-setup-init-file)
          (load skk-init-file t)
	  ;;
	  (static-when (eq skk-emacs-type 'xemacs)
	    (defconst skk-xmas-hiragana-extent (make-extent nil nil))
	    (make-face 'skk-xmas-hiragana-face)
	    (set-face-parent 'skk-xmas-hiragana-face
			     'modeline nil '(default))
	    (when (featurep 'window-system)
	      (set-face-foreground 'skk-xmas-hiragana-face
				   skk-cursor-hiragana-color nil
				   '(default color win))
	      (set-face-font 'skk-xmas-hiragana-face [bold] nil
			     '(default mono win))
	      (set-face-font 'skk-xmas-hiragana-face [bold] nil
			     '(default grayscale win)))
	    (set-extent-face skk-xmas-hiragana-extent 'skk-xmas-hiragana-face)
	    ;;
	    (defconst skk-xmas-katakana-extent (make-extent nil nil))
	    (make-face 'skk-xmas-katakana-face)
	    (set-face-parent 'skk-xmas-katakana-face 'modeline nil '(default))
	    (when (featurep 'window-system)
	      (set-face-foreground 'skk-xmas-katakana-face
				   skk-cursor-katakana-color nil
				   '(default color win))
	      (set-face-font 'skk-xmas-katakana-face [bold] nil
			     '(default mono win))
	      (set-face-font 'skk-xmas-katakana-face [bold] nil
			     '(default grayscale win)))
	    (set-extent-face skk-xmas-katakana-extent 'skk-xmas-katakana-face)
	    ;;
	    (defconst skk-xmas-jisx0208-latin-extent (make-extent nil nil))
	    (make-face 'skk-xmas-jisx0208-latin-face)
	    (set-face-parent 'skk-xmas-jisx0208-latin-face 'modeline
			     nil '(default))
	    (when (featurep 'window-system)
	      (set-face-foreground 'skk-xmas-jisx0208-latin-face
				   skk-cursor-jisx0208-latin-color nil
				   '(default color win))
	      (set-face-font 'skk-xmas-jisx0208-latin-face [bold] nil
			     '(default mono win))
	      (set-face-font 'skk-xmas-jisx0208-latin-face [bold] nil
			     '(default grayscale win)))
	    (set-extent-face skk-xmas-jisx0208-latin-extent
			     'skk-xmas-jisx0208-latin-face)
	    ;;
	    (defconst skk-xmas-latin-extent (make-extent nil nil))
	    (make-face 'skk-xmas-latin-face)
	    (set-face-parent 'skk-xmas-latin-face 'modeline nil '(default))
	    (when (featurep 'window-system)
	      (set-face-foreground 'skk-xmas-latin-face
				   skk-cursor-latin-color nil
				   '(default color win))
	      (set-face-font 'skk-xmas-latin-face [bold] nil
			     '(default mono win))
	      (set-face-font 'skk-xmas-latin-face [bold] nil
			     '(default grayscale win)))
	    (set-extent-face skk-xmas-latin-extent 'skk-xmas-latin-face)
	    ;;
	    (defconst skk-xmas-abbrev-extent (make-extent nil nil))
	    (make-face 'skk-xmas-abbrev-face)
	    (set-face-parent 'skk-xmas-abbrev-face 'modeline nil '(default))
	    (when (featurep 'window-system)
	      (set-face-foreground 'skk-xmas-abbrev-face
				   skk-cursor-abbrev-color nil
				   '(default color win))
	      (set-face-font 'skk-xmas-abbrev-face [bold] nil
			     '(default mono win))
	      (set-face-font 'skk-xmas-abbrev-face [bold] nil
			     '(default grayscale win)))
	    (set-extent-face skk-xmas-abbrev-extent 'skk-xmas-abbrev-face)
	    ;;
	    (defconst skk-xmas-jisx0201-extent (make-extent nil nil))
	    (make-face 'skk-xmas-jisx0201-face)
	    (set-face-parent 'skk-xmas-jisx0201-face 'modeline nil '(default))
	    (when (featurep 'window-system)
	      (set-face-foreground 'skk-xmas-jisx0201-face
				   skk-cursor-jisx0201-color nil
				   '(default color win))
	      (set-face-font 'skk-xmas-jisx0201-face [bold] nil
			     '(default mono win))
	      (set-face-font 'skk-xmas-jisx0201-face [bold] nil
			     '(default grayscale win)))
	    (set-extent-face skk-xmas-jisx0201-extent 'skk-xmas-jisx0201-face))
	  ;; end when (eq skk-emacs-type 'xemacs)
	  (skk-setup-modeline)
	  (require 'skk-autoloads)
	  (static-if (or (memq skk-emacs-type '(mule3 mule4 mule5))
			 (and (eq skk-emacs-type 'xemacs)
			      (or
			       ;; XEmacs 21 or later.
			       (> emacs-major-version 20)
			       ;; XEmacs 20.4 or later.
			       (> emacs-minor-version 2))))
	      (require 'skk-leim))
	  (if skk-share-private-jisyo
	      (progn
		(skk-create-file skk-emacs-id-file)
		(setq skk-emacs-id
		      (make-temp-name
		       (concat (system-name) ":"
			       (mapconcat 'int-to-string (current-time) "")
			       ":")))
		(setq skk-jisyo-update-vector
		      (make-vector skk-jisyo-save-count nil))
		(with-temp-buffer
		  (insert-file-contents skk-emacs-id-file)
		  (insert skk-emacs-id "\n")
		  (write-region 1 (point-max) skk-emacs-id-file nil 'nomsg))))
          (if skk-keep-record
	      (skk-create-file skk-record-file
			       "SKK $B$N5-O?MQ%U%!%$%k$r:n$j$^$7$?(B"
			       "I have created an SKK record file for you"))
	  (skk-regularize)
          (setq skk-mode-invoked t)))
    ;; $B0J2<$O(B skk-mode $B$KF~$k$?$S$KKhEY%3!<%k$5$l$k%3!<%I!#(B
    (unless (and (skk-local-variable-p 'skk-jisyo (current-buffer))
		 (equal skk-jisyo "~/skk-tut-jisyo"))
      (skk-create-file skk-jisyo
		       "SKK $B$N6u<-=q$r:n$j$^$7$?(B"
		       "I have created an empty SKK Jisyo file for you"))
    (static-if (memq skk-emacs-type '(nemacs mule1)) (skk-e18-setup))
    (and (or skk-use-color-cursor skk-cursor-change-width)
	 (require 'skk-cursor))
    (and skk-use-viper (require 'skk-viper))
    ;; .skk $B$G(B skk-kakutei-key $B$NJQ99$,2DG=$K$J$k$h$&$K!#(B
    (define-key skk-abbrev-mode-map skk-kakutei-key 'skk-kakutei)
    (define-key skk-abbrev-mode-map (char-to-string skk-start-henkan-char)
      'skk-start-henkan)
    (define-key skk-abbrev-mode-map (char-to-string skk-try-completion-char)
      'skk-try-completion)
    (define-key skk-latin-mode-map skk-kakutei-key 'skk-kakutei)
    (define-key skk-j-mode-map skk-kakutei-key 'skk-kakutei)
    (define-key skk-j-mode-map (char-to-string skk-try-completion-char)
      'skk-insert)
    (unless (featurep 'skk-kanagaki)
      (define-key skk-j-mode-map (char-to-string skk-previous-candidate-char)
	'skk-previous-candidate))
    (define-key skk-jisx0208-latin-mode-map skk-kakutei-key 'skk-kakutei)
    (define-key minibuffer-local-map skk-kakutei-key 'skk-kakutei)
    (define-key minibuffer-local-completion-map skk-kakutei-key 'skk-kakutei)
    (if skk-use-viper
	()
      (define-key skk-j-mode-map
	(char-to-string skk-start-henkan-with-completion-char)
	'skk-start-henkan-with-completion)
      (define-key skk-abbrev-mode-map
	(char-to-string skk-start-henkan-with-completion-char)
	'skk-start-henkan-with-completion)
      (define-key skk-j-mode-map
	(char-to-string skk-backward-and-set-henkan-point-char)
	'skk-backward-and-set-henkan-point)
      (define-key skk-jisx0208-latin-mode-map
	(char-to-string skk-backward-and-set-henkan-point-char)
	'skk-backward-and-set-henkan-point))
    (skk-setup-delete-backward-char)
    (skk-setup-undo)
    ;; XEmacs doesn't have minibuffer-local-ns-map
    (and (boundp 'minibuffer-local-ns-map)
	 (define-key minibuffer-local-ns-map skk-kakutei-key 'skk-kakutei))
    ;; To terminate kana input.
    (make-local-hook 'pre-command-hook)
    (add-hook 'pre-command-hook 'skk-pre-command nil 'local)
    (make-local-hook 'post-command-hook)
    (add-hook 'post-command-hook 'skk-after-point-move nil 'local)
    ;; $BESCf$G@Z$jBX$($?$H$-$N$?$a$K!#(B
    (and (eq skk-status-indicator 'left)
         (setq skk-input-mode-string skk-hiragana-mode-string))
    (skk-j-mode-on)
    (static-if (eq skk-emacs-type 'xemacs) (easy-menu-add skk-menu))
    (run-hooks 'skk-mode-hook)))

;;;###autoload
(defun skk-auto-fill-mode (&optional arg)
  "$BF|K\8lF~NO%b!<%I!#<+F0@^$jJV$75!G=IU$-!#(B
$B%^%$%J!<%b!<%I$N0l<o$G!"%*%j%8%J%k$N%b!<%I$K$O1F6A$rM?$($J$$!#(B
$B@5$N0z?t$rM?$($k$H!"6/@)E*$K(B auto-fill-mode $B5Z$S(B SKK $B%b!<%I$KF~$k!#(B
$BIi$N0z?t$rM?$($k$H(B auto-fill-mode $B5Z$S(B SKK $B%b!<%I$+$iH4$1$k!#(B"
  (interactive "P")
  (let ((auto-fill
         (cond ((null arg)
		(static-if (string< (substring emacs-version 0 2) "19")
		    (not auto-fill-hook) (not auto-fill-function)))
               ((> (prefix-numeric-value arg) 0) t))))
    (auto-fill-mode (if auto-fill 1 -1))
    (skk-mode arg)
    (run-hooks 'skk-auto-fill-mode-hook)))

(defun skk-kill-emacs-without-saving-jisyo (&optional query)
  "SKK $B<-=q$r%;!<%V$7$J$$$G!"(BEmacs $B$r=*N;$9$k!#(B"
  (interactive "P")
  ;; format $B$r0z?t$K;}$?$;$?>l9g$O!"(Bskk-yes-or-no-p $B$r;H$&$H$+$($C$F>iD9$K$J$k!#(B
  (if (yes-or-no-p
       (format (if skk-japanese-message-and-error
                   "$B<-=q$NJ]B8$r$;$:$K(B %s $B$r=*N;$7$^$9!#NI$$$G$9$+!)(B"
                 "Do you really wish to kill %s without saving Jisyo? ")
               (static-if (eq skk-emacs-type 'xemacs) "XEmacs" "Mule")))
      (let ((buff (skk-get-jisyo-buffer skk-jisyo 'nomsg)))
	(ad-disable-advice 'save-buffers-kill-emacs 'before 'skk-ad)
	(ad-activate 'save-buffers-kill-emacs)
	(remove-hook 'skk-before-kill-emacs-hook 'skk-save-jisyo) ; fail safe.
	(if buff
	    (progn (set-buffer buff)
		   (set-buffer-modified-p nil)
		   (kill-buffer buff)))
	(save-buffers-kill-emacs query))))

(defun skk-restart ()
  "skk-init-file $B$N:F%m!<%I5Z$S3F<o:F%;%C%H%"%C%W$N8e!"(BSKK $B%b!<%I$r5/F0$9$k!#(B"
  (interactive)
  (let (skk-mode-invoked) (skk-mode 1)))

(defun skk-regularize ()
  ;; SKK $B$NF0:n$N@55,2=$r?^$k$?$a!"FbItJQ?t$d%f!<%6!<JQ?t$ND4@0$r9T$J$&!#(B
  (skk-setup-auto-paren)
  (setq skk-rule-tree
	(skk-compile-rule-list skk-rom-kana-base-rule-list skk-rom-kana-rule-list))
  (and (not (featurep 'skk-server))
       (or (and (boundp 'skk-servers-list) skk-servers-list)
	   (or (and (boundp 'skk-server-host) skk-server-host)
	       (getenv "SKKSERVER")))
       (require 'skk-server))
  (and (featurep 'skk-server)
       ;; skk-search-server $B$O%5!<%P!<$,Mn$A$F$b;H$($k$N$G!"30$5$J$$!#(B
       (skk-adjust-search-prog-list-for-server-search 'non-del))
  (and skk-auto-okuri-process (skk-adjust-search-prog-list-for-auto-okuri))
  (and skk-use-look (require 'skk-look))
  (and skk-use-jisx0201-input-method (require 'skk-jisx0201))
  (and skk-use-kana-keyboard (require 'skk-kanagaki))
  (skk-setup-delete-selection-mode)
  (skk-adjust-user-option))

(defun skk-setup-delete-backward-char ()
  (let ((commands '(backward-delete-char-untabify
		    backward-delete-char
		    backward-or-forward-delete-char
		    delete-backward-char
		    picture-backward-clear-column
		    ;; following two are SKK adviced.
		    ;;viper-del-backward-char-in-insert
		    ;;vip-del-backward-char-in-insert
		  ))
	(map (if (and (boundp 'overriding-local-map)
		      (keymapp 'overriding-local-map))
		 overriding-local-map
	       (current-global-map)))
	keys)
    (while commands
      (setq keys (where-is-internal (car commands) map)
	    commands (cdr commands))
      (while keys
	(define-key skk-abbrev-mode-map (car keys) 'skk-delete-backward-char)
	(define-key skk-j-mode-map (car keys) 'skk-delete-backward-char)
	(setq keys (cdr keys))))))

(defun skk-setup-undo ()
  (let ((commands '(undo advertised-undo))
	(map (if (and (boundp 'overriding-local-map)
		      (keymapp 'overriding-local-map))
		 overriding-local-map
	       (current-global-map)))
	keys)
    (while commands
      (setq keys (where-is-internal (car commands) map)
	    commands (cdr commands))
      (while keys
	(define-key skk-abbrev-mode-map (car keys) 'skk-undo)
	(define-key skk-j-mode-map (car keys) 'skk-undo)
	(setq keys (cdr keys))))))

(defun skk-setup-init-file ()
  ;; skk-byte-compile-init-file $B$,(B non-nil $B$N>l9g$G!"(Bskk-init-file $B$r%P%$%H%3(B
  ;; $B%s%Q%$%k$7$?%U%!%$%k$,B8:_$7$J$$$+!"$=$N%P%$%H%3%s%Q%$%k:Q%U%!%$%k$h$j(B
  ;; skk-init-file $B$NJ}$,?7$7$$$H$-$O!"(Bskk-init-file $B$r%P%$%H%3%s%Q%$%k$9$k!#(B
  ;;
  ;; skk-byte-compile-init-file $B$,(B nil $B$N>l9g$G!"(Bskk-init-file $B$r%P%$%H%3%s%Q(B
  ;; $B%$%k$7$?%U%!%$%k$h$j(B skk-init-file $B$NJ}$,?7$7$$$H$-$O!"$=$N%P%$%H%3%s%Q%$(B
  ;; $B%k:Q%U%!%$%k$r>C$9!#(B
  (save-match-data
    (let* ((init-file (expand-file-name skk-init-file))
           (elc (concat init-file
                        (if (string-match "\\.el$" init-file)
                            "c"
                          ".elc"))))
      (if skk-byte-compile-init-file
          (and (file-exists-p init-file)
	       (or (not (file-exists-p elc))
		   (file-newer-than-file-p init-file elc))
	       (save-window-excursion ; for keep window configuration.
		 (skk-message "%s $B$r%P%$%H%3%s%Q%$%k$7$^$9!#(B" "Byte-compile %s"
			      skk-init-file)
		 (sit-for 2)
		 (byte-compile-file init-file)))
        (and (file-exists-p init-file)
	     (file-exists-p elc)
	     (file-newer-than-file-p init-file elc)
	     (delete-file elc))))))

(defun skk-emulate-original-map (arg)
  ;; $B%-!<F~NO$KBP$7$F!"(BSKK $B$N%b!<%I$G$O$J$/!"(BEmacs $B$N%*%j%8%J%k$N%-!<3d$jIU$1$G(B
  ;; $B%3%^%s%I$r<B9T$9$k!#(B
  (let ((prefix-arg arg)
        (keys (skk-command-key-sequence (this-command-keys) this-command)))
    (if (not keys)
        ;; no alternative commands.  may be invoked by M-x.
        nil
      (static-if (not (memq skk-emacs-type '(nemacs mule1)))
	  (let (skk-mode skk-latin-mode skk-j-mode skk-abbrev-mode
			 skk-jisx0208-latin-mode command)
	    ;; have to search key binding after binding 4 minor mode flags to nil.
	    (setq command (key-binding keys))
	    (if (eq command this-command)
		;; avoid recursive calling of skk-emulate-original-map.
		nil
	      ;; if no bindings are found, call `undefined'.  it's
	      ;; original behaviour.
	      (skk-cancel-undo-boundary)
	      (command-execute (or command (function undefined)))))
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
	      (use-local-map local-map))))))))

(defun skk-command-key-sequence (key command)
  ;; KEY $B$+$i(B universal arguments $B$r<h$j=|$-!"(BCOMMAND $B$r<B9T$9$k%-!<$rJV$9!#(B
  ;; `execute-extended-command' $B$K$h$C$F%3%^%s%I$,<B9T$5$l$?>l9g$O!"(Bnil $B$rJV$9!#(B
  (while (not (or (zerop (length key))
                  (eq command (key-binding key))))
    (setq key (vconcat (cdr (append key nil)))))
  (and (not (zerop (length key))) key))

(defun skk-setup-delete-selection-mode ()
  ;; Delete Selection $B%b!<%I$,(B SKK $B$r;H$C$?F|K\8lF~NO$KBP$7$F$b5!G=$9$k$h$&$K(B
  ;; $B%;%C%H%"%C%W$9$k!#(B
  (static-cond
   ((eq skk-emacs-type 'xemacs)
    (and (featurep 'pending-del)
	 (not (get 'skk-insert 'pending-delete))
	 (mapcar (function (lambda (func) (put func 'pending-delete t)))
		 '(skk-current-kuten
		   skk-current-touten
		   skk-input-by-code-or-menu
		   skk-insert
		   skk-today))))
   (t
    (and (featurep 'delsel)
	 (not (get 'skk-insert 'delete-selection))
	 (mapcar (function (lambda (func) (put func 'delete-selection t)))
		 '(skk-current-kuten
		   skk-current-touten
		   skk-input-by-code-or-menu
		   skk-insert
		   skk-today))))))

(defun skk-setup-auto-paren ()
  (if (and skk-auto-insert-paren skk-auto-paren-string-alist)
      (let ((strlst (mapcar 'char-to-string skk-special-midashi-char-list))
	    rulealst str alist)
	(while strlst
	  ;; skk-auto-paren-string-alist $B$NCf$+$i!"(Bskk-special-midashi-char-list
	  ;; $B$NMWAG$K4XO"$9$k$b$N$r<h$j=|$/!#(B
	  (remove-alist 'skk-auto-paren-string-alist (car strlst))
	  (setq strlst (cdr strlst)))
	(if (null (memq t (mapcar (function
				   (lambda (e)
				     (skk-ascii-char-p (string-to-char (car e)))))
				  skk-auto-paren-string-alist)))
	    nil
	  (setq alist skk-auto-paren-string-alist
		rulealst (nconc (mapcar (function (lambda (e) (nth 2 e)))
					skk-rom-kana-rule-list)
				(mapcar (function (lambda (e) (nth 2 e)))
					skk-rom-kana-base-rule-list)))
	  (while alist
	    (setq str (car (car alist)))
	    (and (skk-ascii-char-p (string-to-char str))
		 ;; $B=PNOJ8;z$,F~$C$F$$$k%;%k$rD4$Y$F!"$$$:$l$+$N%-!<$K%P%$%s%I(B
		 ;; $B$5$l$F$$$J$$$+$I$&$+$r3NG'$9$k!#(B
		 (null (assoc str rulealst))
		 (null (rassoc str rulealst))
		 ;; $B3d$jIU$1$h$&$H$7$F$$$k%-!<$,!"2?$+B>$N=PNOJ8;z$K%P%$%s%I$5(B
		 ;; $B$l$F$$$J$$$+$I$&$+$r3NG'$9$k!#(B
		 (null (assoc str skk-rom-kana-base-rule-list))
		 (null (assoc str skk-rom-kana-rule-list))
		 ;; skk-auto-paren-string-alist $B$N3FMWAG$N(B car $B$NJ8;z$,(B
		 ;; ascii char $B$G$"$k>l9g$O!"(Bskk-rom-kana-rule-list,
		 ;; skk-rom-kana-base-rule-list $B$K$=$NJ8;z$r=q$-9~$`(B ($BK\(B
		 ;; $BMh$O(B ascii char $B$O(B skk-rom-kana-rule-list,
		 ;; skk-rom-kana-base-rule-list $B$K=q$/I,MW$,$J$$(B ---
		 ;; skk-emulate-original-map$B$K$h$kF~NO$,9T$J$o$l$k(B ---
		 ;; $B$,!"(Bskk-auto-paren-string-alist $B$K;XDj$5$l$?BP$K$J$k(B
		 ;; $BJ8;z$NA^F~$N$?$a$K$O!"%-!<$H$J$kJ8;z$r=q$$$F$*$/I,MW$,(B
		 ;; $B$"$k(B)$B!#(B
		 (setq skk-rom-kana-rule-list (cons (list str nil str)
						    skk-rom-kana-rule-list)))
	    (setq alist (cdr alist)))))))

(defun skk-adjust-user-option ()
  ;; $BN>N)$G$-$J$$%*%W%7%g%s$ND4@0$r9T$J$&!#(B
  (and skk-process-okuri-early
       ;; skk-process-okuri-early $B$NCM$,(B non-nil $B$G$"$k$H$-$K2<5-$NCM$,(B non-nil
       ;; $B$G$"$l$P@5>o$KF0$+$J$$$N$G$3$NJQ?t$NM%@h=g0L$r9b$/$7$?!#(B
       (setq skk-kakutei-early nil
	     skk-auto-okuri-process nil
	     skk-henkan-okuri-strictly nil
	     skk-henkan-strict-okuri-precedence nil)))

(defun skk-try-completion (arg)
  "$B"&%b!<%I$G8+=P$78l$NJd40$r9T$&!#(B
$B$=$l0J30$N%b!<%I$G$O!"%*%j%8%J%k$N%-!<3d$jIU$1$N%3%^%s%I$r%(%_%e%l!<%H$9$k!#(B"
  (interactive "P")
  (skk-with-point-move
   (if (and skk-henkan-on (not skk-henkan-active))
       (progn
	 (setq this-command 'skk-completion)
	 (skk-completion (not (eq last-command 'skk-completion))))
     (skk-emulate-original-map arg))))

(defun skk-latin-mode (arg)
  "SKK $B$N%b!<%I$r(B latin (ascii) $B%b!<%I$KJQ99$9$k!#(B"
  (interactive "P")
  (skk-kakutei)
  (skk-latin-mode-on))

(defun skk-jisx0208-latin-mode (arg)
  "SKK $B$N%b!<%I$rA43Q1Q;zF~NO%b!<%I$KJQ99$9$k!#(B"
  (interactive "P")
  (skk-kakutei)
  (skk-jisx0208-latin-mode-on))

(defun skk-abbrev-mode (arg)
  "ascii $BJ8;z$r%-!<$K$7$?JQ49$r9T$&$?$a$NF~NO%b!<%I!#(B"
  (interactive "*P")
  (cond (skk-henkan-active
	 (skk-kakutei))
	;;((and skk-henkan-on (not skk-henkan-active))
	(skk-henkan-on
	 (skk-error "$B4{$K"&%b!<%I$KF~$C$F$$$^$9(B" "Already in $B"&(B mode")))
  (skk-set-henkan-point-subr)
  (skk-abbrev-mode-on))

(defun skk-toggle-characters (arg)
  "$B"#%b!<%I$G!"$R$i$,$J%b!<%I$H%+%?%+%J%b!<%I$r%H%0%k$G@Z$jBX$($k!#(B
$B"'%b!<%I!""&%b!<%I$G$O!"(Bskk-henkan-start-point ($B"&$ND>8e(B) $B$H%+!<%=%k$N4V$NJ8;zNs$r(B

    $B$R$i$,$J(B <=> $B%+%?%+%J(B
    $BA43Q1Q?t;z(B <=> ascii

$B$N$h$&$KJQ49$9$k!#(B"
  (interactive "P")
  (cond (skk-henkan-on
         (let (char)
           (skk-set-marker skk-henkan-end-point (point))
           (skk-save-point
	    (goto-char skk-henkan-start-point)
	    
	    (while (or 
		    ;; "$B!<(B" $B$G$OJ8;z<oJL$,H=JL$G$-$J$$$N$G!"%]%$%s%H$r?J$a$k!#(B
		    (looking-at "$B!<(B")
		    (eq 'unknown (setq char (skk-what-char-type))))
	      (forward-char 1)))
           (cond ((eq char 'hiragana)
                  (skk-katakana-region
		   skk-henkan-start-point skk-henkan-end-point
		   'vcontract))
                 ((eq char 'katakana)
                  (skk-hiragana-region
		   skk-henkan-start-point skk-henkan-end-point))
                 ((eq char 'jisx0208-latin)
                  (skk-latin-region
		   skk-henkan-start-point skk-henkan-end-point))
                 ((eq char 'ascii)
                  (skk-jisx0208-latin-region
		   skk-henkan-start-point skk-henkan-end-point)))))
        ((and (skk-in-minibuffer-p) (not skk-j-mode))
         ;; $B%_%K%P%C%U%!$X$N=iFMF~;~!#(B
         (skk-j-mode-on)
	 ;; $B$3$3$G(B skk-katakana $B%U%i%0$rN)$F$F$*$+$J$/$FNI$$$N$+!)(B
	 )
        (t (setq skk-katakana (not skk-katakana))))
  (skk-kakutei))

(defun skk-misc-for-picture ()
  ;; picture-mode $B$XF~$C$?$H$-$K(B SKK $B5/F0A0$N>uBV$KLa$9!#(B
  ;; edit-picture-hook $B$K(B add-hook $B$7$F;HMQ$9$k!#(B
  ;;
  ;; $B4{B8$N%P%C%U%!$r(B picture mode $B$K$7$?$H$-!"(Bpicture-mode $B4X?t$O(B
  ;; kill-all-local-variables $B4X?t$r8F$P$J$$$N$G!"(BSKK $B4XO"$N%P%C%U%!%m!<%+%k(B
  ;; $BJQ?t$,85$N%P%C%U%!$NCM$N$^$^$K$J$C$F$7$^$&!#$=$3$G!"(Bpicture mode $B$KF~$C$?(B
  ;; $B$H$-$K%U%C%/$rMxMQ$7$F$3$l$i$N%P%C%U%!%m!<%+%kJQ?t$r(B kill $B$9$k!#(B
  ;; RMS $B$O(B picture-mode $B$G(B kill-all-local-variables $B4X?t$r8F$P$J$$$N$O!"%P%0(B
  ;; $B$G$O$J$$!"$H8@$C$F$$$?!#(B
  ;;
  ;; picture-mode $B$G(B SKK $B$r;HMQ$74A;zF~NO$r$7$?>l9g$K!"(BBS $B$GA43QJ8;z$,>C$;$J$$(B
  ;; $B$N$O!"(BSKK $B$NIT6q9g$G$O$J$/!"(Bpicture.el $B$NLdBj(B (move-to-column-force $B4X?t(B
  ;; $B$NCf$G;HMQ$7$F$$$k(B move-to-column $B$GA43QJ8;z$rL5;k$7$?%+%i%`?t$,M?$($i$l(B
  ;; $B$?$H$-$K%+!<%=%k0\F0$,$G$-$J$$$+$i(B) $B$G$"$k!#>C$7$?$$J8;z$K%]%$%s%H$r9g$o(B
  ;; $B$;!"(BC-c C-d $B$G0lJ8;z$E$D>C$9$7$+J}K!$O$J$$!#(B
  (and skk-mode (skk-kill-local-variables)))

(defun skk-kill-local-variables ()
  ;; SKK $B4XO"$N%P%C%U%!%m!<%+%kJQ?t$rL58z$K$9$k!#(B
  (skk-mode -1)
  (let ((lv (buffer-local-variables))
        v vstr)
    (while lv
      (setq v (car (car lv))
            lv (cdr lv)
            vstr (prin1-to-string v))
      (and (> (length vstr) 3) (string= "skk-" (substring vstr 0 4))
	   (kill-local-variable v)))))

;;;; kana inputting functions

(defun skk-insert (&optional arg)
  "SKK $B$NJ8;zF~NO$r9T$J$&!#(B"
  ;; skk-rom-kana-\\(base-\\)*rule-list $B$N(B caddr $B$K4X?t$r=q$-!"$=$N4X?tFb$G!"0l(B
  ;; $BDj>r7o$rK~$7$?>l9g$K(B ($BJ8;zA^F~0J30$N(B) $B$"$kFCDj$NF0:n$r$5$;!"$=$&$G$J$$>l9g$O$"$kFC(B
  ;; $BDjJ8;z$NA^F~$r9T$J$&$3$H$N%a%j%C%H!"%G%a%j%C%H$K$D$$$F!#(B
  ;;
  ;; $B%a%j%C%H(B; $BI,$:(B skk-kana-input $B$rDL$k$N$G!"(Bunfixed prefix + $B%H%j%,!<%-!<$N(B
  ;; $BJ8;z=hM}$r9T$J$C$F$+$i;XDj$N4X?t8F$S=P$7$KF~$k$3$H$,$G$-$k!#(B
  ;;
  ;; $B%G%a%j%C%H(B; $B%3!<%k$5$l$?4X?tFb$G!"FH<+$KA^F~J8;z$r7hDj$9$k$3$H$O$G$-$k$,!"(B
  ;; skk-rom-kana-\\(base-\\)*rule-list $BFb$GDj5A$,9T$J$($J$$(B ($B4{$KJ8;z$NBe$o$j(B
  ;; $B$K4X?tL>$,;XDj$5$l$F$$$k$+$i!#3:Ev4X?tFb$G!"(Bskk-kana-input $B$r%3!<%k$9$k$H!"(B
  ;; $BL58B%k!<%W$K4Y$$$C$F$7$^$&(B)$B!#4X?tFb$G%*%j%8%J%k$N%+%l%s%H%^%C%W$NF0:n$r%((B
  ;; $B%_%e%l!<%H$9$k$H!"%f!<%6!<$,A^F~J8;z$NDj5A$rJQ99$G$-$J$$!#(B
  ;;
  ;; $B$^$?!"(Bskk-input-vector $B$rGQ$7!"(Bskk-rom-kana-\\(base-\\)*rule-list $B$KA^F~(B
  ;; $B$9$Y$-J8;zDj5A$r=8Cf$5$;$?$3$H$+$i!"2DG=$J8B$j$3$l$rJx$7$?$/$J$$!#(B
  ;;
  ;; $B>e5-$N9M;!$+$i!"2<5-$N$h$&$KJ}?K$r7h$a$?!#(B
  ;;
  ;; (1)$BA^F~J8;z$NDj5A$O!"(Bskk-rom-kana-\\(base-\\)*rule-list $B0J30$G$O9T$J$o$J(B
  ;;    $B$$!#(B
  ;; (2)$B%H%j%,!<%-!<$r%f!<%6!<JQ?t$H$7!"$3$N%-!<$,2!$5$l$?>l9g$+$I$&$+$NH=Dj$O!"(B
  ;;    skk-insert $BFb$G9T$J$$!"E,Ev$J4X?t$r%3!<%k$9$k!#(B
  ;; (3)(2)$B$N%f!<%6!<JQ?t$O!"(Bskk-abbrev-mode-map $B$N%-!<Dj5A$J$I$G$b;2>H$9$k$3(B
  ;;    $B$H$H$7!"2DG=$J8B$jF0:n$NE}0l$r?^$k!#(B
  ;; (4)unfixed prefix + $B%H%j%,!<%-!<$N=hM}$OI,MW$K1~$8$F3:Ev4X?t$NCf$KKd$a9~(B
  ;;    $B$`!#(B
  (interactive "*p")
  (skk-with-point-move
   (let ((ch last-command-char))
     (cond (
	    ;; start writing a midasi key.
	    (or (and (memq ch skk-set-henkan-point-key)
		     (or skk-okurigana
			 (not (skk-get-prefix skk-current-rule-tree))
			 (not (skk-select-branch skk-current-rule-tree ch))))
		(and skk-henkan-on (memq ch skk-special-midashi-char-list)))
	    ;; normal pattern
	    ;; skk-set-henkan-point -> skk-kana-input.
	    (skk-set-henkan-point arg))
	   ;; start conversion.
	   ((and skk-henkan-on (eq ch skk-start-henkan-char))
	    (skk-start-henkan arg))
	   ;; for completion.
	   ((and skk-henkan-on (not skk-henkan-active))
	    (cond ((eq ch skk-try-completion-char)
		   (setq this-command 'skk-completion)
		   (skk-completion (not (eq last-command 'skk-completion))))
		  ((eq last-command 'skk-completion)
		   (cond ((eq ch skk-next-completion-char)
			  (setq this-command 'skk-completion)
			  (skk-completion nil))
			 ((eq ch skk-previous-completion-char)
			  (setq this-command 'skk-completion)
			  (skk-previous-completion))
			 (t (skk-kana-input arg))))
		  (t (skk-kana-input arg))))
	   ;; just imput Kana.
	   (t (skk-kana-input arg))))))

(defun skk-kana-input (&optional arg)
  ;;"$B$+$JJ8;z$NF~NO$r9T$&%k!<%A%s!#(B"
  ;; Message-Id: <19980610190611B.sakurada@kuis.kyoto-u.ac.jp>
  ;; From: Hideki Sakurada <sakurada@kuis.kyoto-u.ac.jp>
  ;; Date: Wed, 10 Jun 1998 19:06:11 +0900 (JST)
  ;;
  ;; $B?7$7$$(B skk-kana-input $B$O(B, $B4JC1$K@bL@$9$k$H(B,
  ;; $B$"$i$+$8$a%k!<%k$rLZ$N7A$KI=8=$7$F$*$$$F(B, $BF~NO$r8+(B
  ;; $B$FLZ$rC)$j(B, $B$=$l0J>eC)$l$J$/$J$C$?$i$=$N@a$KEPO?$5(B
  ;; $B$l$F$$$k2>L>$rF~NO$9$k(B. $B$H$$$&$7$/$_$G$9(B.
  ;;
  ;; $BNc$($P(B, n a t $B$N$_$+$i$J$k0J2<$N%k!<%k(B
  ;;
  ;;     a  $B"*(B $B$"(B
  ;;     n  $B"*(B $B$s(B
  ;;     nn $B"*(B $B$s(B
  ;;     na $B"*(B $B$J(B
  ;;     ta $B"*(B $B$?(B
  ;;     tt $B"*(B $B$C(B ($B<!>uBV(B t)
  ;;
  ;; $B$r%k!<%kLZ$KJQ49$9$k$H(B,
  ;;
  ;;                 $B!?(B/$B!@(B
  ;;               $B!?(B /   $B!@(B
  ;;           a $B!?(B  / t    $B!@(B n
  ;;           $B!?(B   /         $B!@(B
  ;;          $B$"(B   $B!&(B           $B$s(B
  ;;             $B!?(B|           / $B!@(B
  ;;         a $B!?(B  | t      a /    $B!@(B n
  ;;         $B!?(B    |         /       $B!@(B
  ;;       $B$?(B     $B$C(B        $B$J(B        $B$s(B
  ;;          ($B<!>uBV(B "t")
  ;;
  ;; $B$H$$$&7A$K$J$j$^$9(B.
  ;;
  ;; $B=i4|>uBV(B($BLZ$N:,(B)$B$G(B `a' $B$rF~NO$9$k$H(B, $BLZ$N:,$+$i(B
  ;; $B!V$"!W$K0\F0$7$^$9(B. $B<!$K$I$N$h$&$JF~NO$,Mh$F$b(B,
  ;; $B$=$l$h$j2<$K$?$I$l$J$$$N$G(B, $B!V$"!W$r=PNO$7$F:,$KLa$j$^$9(B.
  ;; $B%k!<%k$K<!>uBV$,@_Dj$5$l$F$$$k>l9g$O(B, $B@_Dj$5$l$F$$(B
  ;; $B$kJ8;zNs$r%-%e!<$KLa$7$F$+$i:,$KLa$j$^$9(B.
  ;;
  ;; $B=i4|>uBV$G(B `n' $B$rF~NO$9$k$H(B, $B!V$s!W$K0\F0$7$^$9(B.
  ;; $B<!$K(B `a' $B$^$?$O(B `n' $B$,F~NO$5$l$l$P$=$l$h$j2<$K$?$I$l$k(B
  ;; $B$N$G<!$NF~NO$r8+$k$^$G$^$@=PNO$7$^$;$s(B.
  ;; $B<!$K(B `t' $B$,F~NO$5$l$?>l9g$O(B, `t' $B$G$O2<$K$?$I$l$J$$$N$G(B,
  ;; $B!V$s!W$r=PNO$7$F(B `t' $B$r%-%e!<$KLa$7$^$9(B.
  ;;
  ;; $B$3$3$G(B, $B=i4|>uBV(B, $B8=>uBV$r$=$l$>$l(B skk-rule-tree,
  ;; skk-current-rule-tree $B$GI=$7(B.
  ;; $BLZ$r2<$K$?$I$k(B, $B$H$$$&A`:n$O(B, skk-select-branch $B$r(B
  ;; $BMQ$$$F(B,
  ;;
  ;;   (skk-select-branch rule-tree ?a)
  ;;
  ;; $B$N$h$&$K$7$^$9(B. $B8=>uBV$K@_Dj$5$l$F$$$k$+$J(B(("$B%"(B". "$B$"(B")$B$J$I(B),
  ;; $B<!>uBV(B("t" $B$J$I(B)$B$O(B, $B$=$l$>$l(B skk-get-kana,
  ;; skk-get-nextstate $B$G<h$j$^$9(B.
  ;; don't echo key strokes in the minibuffer.
  (let ((echo-keystrokes 0)
	(queue (list last-command-char)))
    (while queue
      (if (not (skk-get-prefix skk-current-rule-tree))
	  (progn
	    (skk-set-marker skk-kana-start-point (point))
	    (setq skk-current-rule-tree skk-rule-tree))
	(skk-erase-prefix))
      (setq skk-prefix (concat (skk-get-prefix skk-current-rule-tree)
			       (char-to-string last-command-char)))
      (let ((next (skk-select-branch skk-current-rule-tree (car queue)))
	    data)
	(if next
	    ;; can go down SKK-CURRENT-RULE-TREE
	    (if (skk-get-branch-list next)
		;; NEXT have at least one branch
		(progn
		  (and skk-henkan-active
		       skk-kakutei-early
		       (not skk-process-okuri-early)
		       (skk-kakutei))
		  (setq queue (cdr queue)
			skk-current-rule-tree next))
	      ;; NEXT does not have any branch (i.e. NEXT is a leaf)
	      (setq data (skk-get-kana next)
		    queue (nconc (string-to-char-list (skk-get-nextstate next))
				 (cdr queue))
		    skk-current-rule-tree nil))
	  ;; can not go down SKK-CURRENT-RULE-TREE
	  (let ((d (skk-get-kana skk-current-rule-tree)))
	    (if d
		;; SKK-CURRENT-RULE-TREE have a roma->kana rule
		(setq data d
		      queue
		      (nconc (string-to-char-list
			      (skk-get-nextstate skk-current-rule-tree))
			     queue)
		      skk-current-rule-tree nil)
	      ;; SKK-CURRENT-RULE-TREE does not have any roma->kana rule
	      (let ((dd (and skk-kana-input-search-function
			     (funcall skk-kana-input-search-function))))
		(if dd
		    (setq data (car dd)
			  queue (nconc (string-to-char-list (cdr dd))
				       (cdr queue))
			  skk-current-rule-tree nil)
		  (if (eq skk-current-rule-tree skk-rule-tree)
		      ;; typo on the root of tree
		      (setq queue nil
			    skk-current-rule-tree nil)
		    ;; otherwise move to root of the tree, and redo
		    (setq skk-current-rule-tree nil)))))))
	(if (not data)
	    (if skk-current-rule-tree
		(progn
		  ;;(digit-argument arg)
		  ;; $B$&!A$s!"$h$&J,$+$i$s!#$H$j$"$($:!#(B
		  (or skk-isearch-message (setq prefix-arg arg))
		  (setq skk-prefix (skk-get-prefix skk-current-rule-tree))
		  (skk-insert-prefix skk-prefix))
	      ;;(skk-kana-cleanup 'force)
	      (and skk-henkan-active (skk-kakutei))
	      (setq skk-prefix "")
	      (or queue
		  (skk-emulate-original-map (skk-make-raw-arg arg))))
	  (skk-cancel-undo-boundary)
	  (setq skk-prefix "")
	  (and (functionp data)
	       (setq data (funcall data (skk-make-raw-arg arg))))
	  (if (not (stringp (if (consp data) (car data) data)))
	      nil
	    (let* ((str (if (consp data) (if skk-katakana (car data) (cdr data))
			  data))
		   (pair (and skk-auto-insert-paren
			      (cdr (assoc str skk-auto-paren-string-alist))))
		   (count0 arg) (count1 arg) (inserted 0))
	      (and skk-henkan-active
		   skk-kakutei-early (not skk-process-okuri-early)
		   (skk-kakutei))
	      ;; arg $B$OJ]B8$7$F$*$+$J$$$H!"(B0 $B$K$J$C$F$7$^$$!"(Bqueue
	      ;; $B$,$?$^$C$F$$$F:FEY$3$3$X$d$C$FMh$?$H$-$KJ8;zF~NO$,(B
	      ;; $B$G$-$J$/$J$k!#(B
	      (while (> count0 0)
		(skk-insert-str str)
		(setq count0 (1- count0)))
	      (if (not pair)
		  nil
		(while (> count1 0)
		  (if (not (string= pair (char-to-string (following-char))))
		      (progn
			(setq inserted (1+ inserted))
			(skk-insert-str pair)))
		  (setq count1 (1- count1)))
		(or (= inserted 0) (backward-char inserted)))
	      (and skk-okurigana (null queue) (skk-set-okurigana))))))
      ;; XXX I don't know how skk-isearch-message works....
      (and skk-isearch-message (skk-isearch-message)))))

;; tree procedure ($B%D%j!<$K%"%/%;%9$9$k$?$a$N%$%s%?!<%U%'!<%9(B)
(defun skk-search-tree (tree char-list)
  ;; TREE $B$N:,$+$i@hC<$X(B CHAR-LIST $B$K=>$C$F$?$I$k!#(B
  ;; $B@.8y$7$?>l9g$O(B nil $B$H(B $B7k2L$NLZ$NAH$rJV$7(B,
  ;; $B<:GT$7$?>l9g$O$?$I$l$J$+$C$?(B CHAR-LIST $B$N;D$j$H$?$I$l$J$/$J$C$?(B
  ;; $B@aE@$NLZ$NAH$rJV$9!#(B
  (catch 'return
    (let (next char rest)
      (while char-list
	(setq char (car char-list)
	      rest (cdr char-list)
	      next (skk-select-branch tree char))
	(if next
	    (setq tree next
		  char-list rest)
	  (throw 'return (cons char-list tree))))
      (cons nil tree))))

(defun skk-add-rule (tree rule)
  (let* ((prefix (nth 0 rule))
	 (l (length prefix))
	 (result (skk-search-tree tree (string-to-char-list prefix)))
	 (rest (car result))
	 (addpoint (cdr result)))
    (while rest
      (let ((addtree
	     (skk-make-rule-tree (car rest)
				 (substring prefix 0 (1+ (- l (length rest))))
				 nil nil nil)))
	(skk-add-branch addpoint addtree)
	(setq addpoint addtree
	      rest (cdr rest))))
    (skk-set-nextstate addpoint (nth 1 rule))
    (skk-set-kana addpoint (nth 2 rule))))

(defun skk-delete-rule (tree string)
  ;; $BF~NO(B STRING $B$KBP$9$k%k!<%k$r%k!<%kLZ(B TREE $B$+$i:o=|(B
  (catch 'return
    (let ((char-list (string-to-char-list string))
	  (cutpoint tree)
	  (cuttree (car (skk-get-branch-list tree)))
					; TREE $B$N:,$+$i=P$k;^$,(B1$BK\$7$+$J$$>l9g(B
					; $B$N$?$a$K0l1~=i4|2=$7$F$*$/(B
	  next)
      (while char-list
	(setq next (skk-select-branch tree (car char-list))
	      char-list (cdr char-list))
	(if next
	    (if (> (length (skk-get-branch-list tree)) 1)
		(setq cutpoint tree	; $B;^$,(B2$BK\0J>e$N;~(B cutpoint cuttree
		      cuttree next	; $B$r(B update
		      tree next)
	      (setq tree next))
	  (throw 'return nil)))
      (skk-set-branch-list cutpoint
			   (delq cuttree (skk-get-branch-list cutpoint))))))

;; convert skk-rom-kana-rule-list to skk-rule-tree.
;; The rule tree follows the following syntax:
;; <branch-list>  := nil | (<tree> . <branch-list>)
;; <tree>         := (<char> <prefix> <nextstate> <kana> <branch-list>)
;; <kana>         := (<$B$R$i$,$JJ8;zNs(B> . <$B%+%?%+%JJ8;zNs(B>) | nil
;; <char>         := <$B1Q>.J8;z(B>
;; <nextstate>    := <$B1Q>.J8;zJ8;zNs(B> | nil
;;;###autoload
(defun skk-compile-rule-list (&rest l)
  ;; rule-list $B$rLZ$N7A$K%3%s%Q%$%k$9$k!#(B
  (let ((tree (skk-make-rule-tree nil "" nil nil nil))
	rule ll)
    (while l
      (setq ll (car l)
	    l (cdr l))
      (while ll
	(setq rule (car ll)
	      ll (cdr ll))
	(skk-add-rule tree rule)))
    tree))

(defun skk-insert-str (str)
  ;; STR $B$rA^F~$9$k!#I,MW$G$"$l$P(B self-insert-after-hook $B$r%3(B
  ;; $B!<%k$9$k!#(Boverwrite-mode $B$G$"$l$P!"E,@Z$K>e=q$-$r9T$&!#(B
  (insert-and-inherit str)
  (if (and skk-henkan-on (not skk-henkan-active))
      (and skk-auto-start-henkan (not skk-okurigana) (skk-auto-start-henkan str))
    (and (boundp 'self-insert-after-hook) self-insert-after-hook
	 (funcall self-insert-after-hook (- (point) (length str)) (point)))
    (and overwrite-mode
	 (skk-del-char-with-pad (skk-ovwrt-len (string-width str)))))
  ;; SKK 9.6 $B$G$O$3$N%?%$%_%s%0$G(B fill $B$,9T$o$l$F$$$?$,!"(BSKK 10 $B$G$O9T$o$l$F$$(B
  ;; $B$J$+$C$?!#(B
  (when (and skk-j-mode (not skk-henkan-on))
    (skk-do-auto-fill)))

(defun skk-ovwrt-len (len)
  ;; $B>e=q$-$7$FNI$$D9$5$rJV$9!#(B
  (min (string-width
	(buffer-substring-no-properties
	 (point) (skk-save-point (end-of-line) (point))))
       len))

(defun skk-del-char-with-pad (length)
  ;; $BD9$5(B LENGTH $B$NJ8;z$r>C5n$9$k!#D4@0$N$?$a!"I,MW$G$"$l$P!"KvHx$K%9%Z!<%9$r(B
  ;; $BA^F~$9$k!#(B
  (let ((p (point)) (len 0))
    (while (< len length)
      (forward-char 1)
      (setq len (string-width (buffer-substring-no-properties (point) p))))
    (delete-region p (point))
    (or (= length len)
        (progn
	  (insert-and-inherit " ")
          (backward-char 1)))))

(defun skk-cancel-undo-boundary ()
  ;; skk-insert, skk-jisx0208-latin-insert $B$GO"B3$7$FF~NO$5(B
  ;; $B$l$?(B 20 $BJ8;z$r(B 1 $B2s$N%"%s%I%%$NBP>]$H$9$k!#(B`20' $B$O(B
  ;; keyboard.c $B$KDj$a$i$l$?%^%8%C%/%J%s%P!<!#(BMule-2.3 $BE:IU(B
  ;; $B$N(B egg.el $B$r;29M$K$7$?!#(B
  (if (and (< skk-self-insert-non-undo-count 20)
           (memq last-command
                 '(skk-insert
                   skk-jisx0208-latin-insert
                   ;; SKK abbrev $B%b!<%I$G$O!"%"%9%-!<J8;zF~NO$,(B Emacs $B%*%j%8%J(B
                   ;; $B%k$N(B self-insert-command $B$K$h$j9T$J$o$l$F$$$k$N$G!"(B
                   ;; skk-self-insert-non-undo-count $B$r%$%s%/%j%a%s%H$9$k$3$H(B
                   ;; $B$,$G$-$J$$$N$G!"%"%s%I%%$r%(%_%e%l!<%H$G$-$J$$!#(B
                   ;; $B$7$+$b!"%+%s%^$d%T%j%*%I$rA^F~$7$?;~E@$G!"(B
                   ;; skk-abbrev-comma $B$d(B skk-abbrev-period $B$r;H$&$3$H$K$J$k$N(B
                   ;; $B$G(B (self-insert-command $B0J30$N%3%^%s%I$r;H$C$F$7$^$&$N$G(B)$B!"(B
		   ;; $B%*%j%8%J%k$N%"%s%I%%$N5!G=$bB;$J$C$F$7$^$&!#(B
		   ;; $B$7$+$78=<BLdBj$H$7$F$O!"(BSKK abbrev $B%b!<%I$O>JN,7A$H$7$F$N(B
		   ;; $B8+=P$78l$rA^F~$9$k$?$a$N%b!<%I$G$"$k$N$G!"D9$$8+=P$78l$r(B
		   ;; $BA^F~$9$k$3$H$O$"$^$j$J$/!"LdBj$b>.$5$$$H9M$($i$l$k!#(B
                   ;;skk-abbrev-comma
                   ;;skk-abbrev-period
		 )))
      (progn
        (cancel-undo-boundary)
	(if (null skk-current-rule-tree)
	    ;; $B$^$@$+$JJ8;z$,40@.$7$F$$$J$$$H$-$O!"(Bundo count $B$r%$%s%/%j%a%s%H(B
	    ;; $B$7$J$$!#(B
	    (setq skk-self-insert-non-undo-count
		  (1+ skk-self-insert-non-undo-count))))
    (setq skk-self-insert-non-undo-count 1)))

(defun skk-translate-okuri-char (okurigana)
  (and skk-okuri-char-alist
       (cdr (assoc (skk-substring-head-character okurigana) skk-okuri-char-alist))))

(defun skk-set-okurigana ()
  ;; $B8+=P$78l$+$i(B skk-henkan-okurigana, skk-henkan-key $B$N3FCM$r%;%C%H$9$k!#(B
  (cancel-undo-boundary)
  ;;(and skk-katakana (skk-hiragana-region skk-henkan-start-point (point)))
  (skk-set-marker skk-henkan-end-point skk-okurigana-start-point)
  ;; just in case
  (skk-save-point
    (goto-char skk-okurigana-start-point)
    (or (eq (following-char) ?*) (insert-and-inherit "*")))
  (setq skk-henkan-okurigana (buffer-substring-no-properties
                              (1+ skk-okurigana-start-point)
                              (point)))
  (setq skk-henkan-key (concat (buffer-substring-no-properties
				skk-henkan-start-point
				skk-henkan-end-point)
			       (or (skk-translate-okuri-char
				    skk-henkan-okurigana)
				   skk-okuri-char))
        skk-prefix "")
  (if skk-katakana
      (setq skk-henkan-key (skk-katakana-to-hiragana skk-henkan-key)
	    skk-henkan-okurigana
	    (skk-katakana-to-hiragana skk-henkan-okurigana)))
  (delete-region skk-okurigana-start-point (1+ skk-okurigana-start-point))
  (setq skk-henkan-count 0)
  (skk-henkan)
  (setq skk-okurigana nil))

;;;; other inputting functions

(defun skk-toggle-kutouten ()
  "$B6gFIE@$N<oN`$r%H%0%k$GJQ99$9$k!#(B"
  (interactive)
  (setq skk-kutouten-type (if (eq skk-kutouten-type 'jp) 'en 'jp))
  (and (interactive-p)
       (skk-message "$B6gE@(B: `%s'  $BFIE@(B: `%s'"
		    "Kuten: `%s'  Touten: `%s'"
		    (skk-current-kuten nil) (skk-current-touten nil))))

(defun skk-current-kuten (arg)
  ;; just ignore arg.
  (car (cdr (assq skk-kutouten-type skk-kuten-touten-alist))))

(defun skk-current-touten (arg)
  ;; just ignore arg.
  (cdr (cdr (assq skk-kutouten-type skk-kuten-touten-alist))))

(defun skk-abbrev-period (arg)
  "SKK abbrev $B%b!<%I$G8+=P$7$NJd40$r9T$C$F$$$k:GCf$G$"$l$P!"<!$N8uJd$rI=<($9$k!#(B
$BJd40$ND>8e$G$J$1$l$P!"%*%j%8%J%k$N%-!<3d$jIU$1$N%3%^%s%I$r%(%_%e%l!<%H$9$k!#(B
SKK abbrev $B%b!<%I0J30$G$O!"(Bskk-insert-period $B4X?t$r;HMQ$9$k$3$H!#(B"
  (interactive "*P")
  (skk-with-point-move
   (if (eq last-command 'skk-completion)
       (progn
	 (setq this-command 'skk-completion)
	 (skk-completion nil))
     (skk-emulate-original-map arg))))

(defun skk-abbrev-comma (arg)
  "SKK abbrev $B%b!<%I$G8+=P$7$NJd40$r9T$C$F$$$k:GCf$G$"$l$P!"D>A0$N8uJd$rI=<($9$k!#(B
$BJd40$ND>8e$G$J$1$l$P!"%*%j%8%J%k$N%-!<3d$jIU$1$N%3%^%s%I$r%(%_%e%l!<%H$9$k!#(B
SKK abbrev $B%b!<%I0J30$G$O!"(Bskk-insert-comma $B4X?t$r;HMQ$9$k$3$H!#(B"
  (interactive "*P")
  (skk-with-point-move
   (if (eq last-command 'skk-completion)
       (progn
	 (setq this-command 'skk-completion)
	 (skk-previous-completion))
     (skk-emulate-original-map arg))))

(defun skk-jisx0208-latin-insert (arg)
  "$BA41QJ8;z$r%+%l%s%H%P%C%U%!$KA^F~$9$k!#(B
skk-jisx0208-latin-vector $B$r%F!<%V%k$H$7$F!":G8e$KF~NO$5$l$?%-!<$KBP1~$9$kJ8(B
$B;z$rA^F~$9$k!#(B
skk-auto-insert-paren $B$NCM$,(B non-nil $B$N>l9g$G!"(Bskk-auto-paren-string-alist $B$K(B
$BBP1~$9$kJ8;zNs$,$"$k$H$-$O!"$=$NBP1~$9$kJ8;zNs(B ($B$+$C$3N`(B) $B$r<+F0E*$KA^F~$9$k!#(B"
  (interactive "*p")
  (skk-with-point-move
   (let* ((str (aref skk-jisx0208-latin-vector last-command-char))
	  (arg2 arg)
	  (pair-str
	   (and skk-auto-insert-paren
		(cdr (assoc str skk-auto-paren-string-alist))))
	  (pair-str-inserted 0))
     (if (not str)
	 (skk-emulate-original-map arg)
       (skk-cancel-undo-boundary)
       (while (> arg 0)
	 (skk-insert-str str)
	 (setq arg (1- arg)))
       (if (not pair-str)
	   nil
	 (while (> arg2 0)
	   (if (not (string= pair-str (char-to-string (following-char))))
	       (progn
		 (setq pair-str-inserted (1+ pair-str-inserted))
		 (skk-insert-str pair-str)))
	   (setq arg2 (1- arg2)))
	 (or (= pair-str-inserted 0) (backward-char pair-str-inserted)))))))

(defun skk-delete-backward-char (arg)
  "$B"'%b!<%I$G(B skk-delete-implies-kakutei $B$,(B non-nil $B$@$C$?$iD>A0$NJ8;z$r>C$7$F3NDj$9$k!#(B
$B"'%b!<%I$G(B skk-delete-implies-kakutei $B$,(B nil $B$@$C$?$iA08uJd$rI=<($9$k!#(B
$B"&%b!<%I$G(B`$B"&(B'$B$h$j$bA0$N%]%$%s%H$G<B9T$9$k$H3NDj$9$k!#(B
$B3NDjF~NO%b!<%I$G!"$+$J%W%l%U%#%C%/%9$NF~NOCf$J$i$P!"$+$J%W%l%U%#%C%/%9$r>C$9!#(B"
  (interactive "*P")
  (skk-with-point-move
   (let ((count (prefix-numeric-value arg)))
     (cond (skk-henkan-active
	    (if (and (not skk-delete-implies-kakutei)
		     (= skk-henkan-end-point (point)))
		(skk-previous-candidate)
	      ;; overwrite-mode $B$G!"%]%$%s%H$,A43QJ8;z$K0O$^$l$F$$$k$H(B
	      ;; $B$-$K(B delete-backward-char $B$r;H$&$H!"A43QJ8;z$O>C$9$,H>(B
	      ;; $B3QJ8;zJ,$7$+(B backward $BJ}8~$K%]%$%s%H$,La$i$J$$(B (Emacs
	      ;; 19.31 $B$K$F3NG'(B)$B!#JQ49Cf$N8uJd$KBP$7$F$O(B
	      ;; delete-backward-char $B$GI,$:A43QJ8;z(B 1 $BJ8;zJ,(B backward
	      ;; $BJ}8~$KLa$C$?J}$,NI$$!#(B
	      (if overwrite-mode
		  (progn
		    (backward-char count)
		    (delete-char count arg))
		(skk-emulate-original-map arg))
	      ;; XXX assume skk-prefix has no multibyte chars.
	      (if (> (length skk-prefix) count)
		  (setq skk-prefix (substring skk-prefix 0 (- (length skk-prefix) count)))
		(setq skk-prefix ""))
	      (and (>= skk-henkan-end-point (point)) (skk-kakutei))))
	   ((and skk-henkan-on (>= skk-henkan-start-point (point)))
	    (setq skk-henkan-count 0)
	    (skk-kakutei))
	   ;; $BF~NOCf$N8+=P$78l$KBP$7$F$O(B delete-backward-char $B$GI,$:A43QJ8;z(B 1
	   ;; $BJ8;zJ,(B backward $BJ}8~$KLa$C$?J}$,NI$$!#(B
	   ((and skk-henkan-on overwrite-mode)
	    (backward-char count)
	    (delete-char count arg))
	   (t
	    (skk-delete-okuri-mark)
	    (if (skk-get-prefix skk-current-rule-tree)
		(skk-erase-prefix 'clean)
	      (skk-emulate-original-map arg)))))))

;;;; henkan routines
(defun skk-henkan ()
  ;; $B%+%J$r4A;zJQ49$9$k%a%$%s%k!<%A%s!#(B
  (let (mark new-word kakutei-henkan)
    (if (string= skk-henkan-key "")
        (skk-kakutei)
      ;; we use mark to go back to the correct position after henkan
      (or (eobp) (setq mark (skk-save-point (forward-char 1) (point-marker))))
      (if (not skk-henkan-active)
          (progn
            (skk-change-marker)
            (setq skk-current-search-prog-list skk-search-prog-list)))
      ;; skk-henkan-1 $B$NCf$+$i%3!<%k$5$l$k(B skk-henkan-show-candidate $B$+$i(B throw
      ;; $B$5$l$k!#$3$3$G%-%c%C%A$7$?>l9g$O!"(B?x $B$,%9%H%j!<%`$KLa$5$l$F$$$k$N$G!"(B
      ;; $B$3$N4X?t$r=P$F!"(Bskk-previous-candidates $B$X$f$/!#(B
      (catch 'unread
        (setq new-word (or (skk-henkan-1) (skk-henkan-in-minibuff))
              kakutei-henkan skk-kakutei-flag)
        (and new-word (skk-insert-new-word new-word)))
      (if mark
          (progn
            (goto-char mark)
            ;; $B;2>H$5$l$F$$$J$$%^!<%+!<$O!"(BGarbage Collection $B$,%3!<%k$5$l$?$H(B
            ;; $B$-$K2s<}$5$l$k$,!"$=$l$^$G$N4V!"%F%-%9%H$N$I$3$+$r;X$7$F$$$k$H!"(B
            ;; $B%F%-%9%H$N%"%C%W%G!<%H$N:]$K$=$N%^!<%+!<CM$r99?7$9$kI,MW$,$"$k(B
            ;; $B$N$G!"$I$3$b;X$5$J$$$h$&$K$9$k!#(B
            (skk-set-marker mark nil)
	    (backward-char 1))
        (goto-char (point-max)))
      (and kakutei-henkan (skk-kakutei new-word)))))

(defun skk-henkan-1 ()
  ;; skk-henkan $B$N%5%V%k!<%A%s!#(B
  (let (new-word)
    (if (= skk-henkan-count 0)
        (progn
          (and (eq last-command 'skk-undo-kakutei-henkan)
	       (eq (car (car skk-current-search-prog-list))
		   'skk-search-kakutei-jisyo-file)
	       ;; in this case, we should not search kakutei jisyo.
	       (setq skk-current-search-prog-list
		     (cdr skk-current-search-prog-list)))
          (while (and skk-current-search-prog-list (not new-word))
            (setq skk-henkan-list (skk-nunion skk-henkan-list (skk-search)))
	    (skk-henkan-list-filter)
	    (setq new-word (skk-get-current-candidate)))
          (if (and new-word skk-kakutei-flag)
	      ;; found the unique candidate in kakutei jisyo
	      (setq this-command 'skk-kakutei-henkan)))
      ;; $BJQ492s?t$,(B 1 $B0J>e$N$H$-!#(B
      (setq new-word (skk-get-current-candidate))
      (or new-word
          ;; $B?7$7$$8uJd$r8+$D$1$k$+!"(Bskk-current-search-prog-list $B$,6u$K$J(B
          ;; $B$k$^$G(B skk-search $B$rO"B3$7$F%3!<%k$9$k!#(B
          (while (and skk-current-search-prog-list (not new-word))
            (setq skk-henkan-list (skk-nunion skk-henkan-list (skk-search)))
	    (skk-henkan-list-filter)
	    (setq new-word (skk-get-current-candidate))))
      (and new-word (> skk-henkan-count 3)
	   ;; show candidates in minibuffer
	   (setq new-word (skk-henkan-show-candidates))))
    new-word))

(defun skk-get-current-candidate (&optional noconv)
  (if (skk-numeric-p)
      (if noconv
	  (car (skk-get-current-candidate-1))
	(cdr (skk-get-current-candidate-1)))
    (skk-get-current-candidate-1)))

(defun skk-henkan-list-filter ()
  (if (skk-numeric-p)
      (progn (skk-num-uniq) (skk-num-multiple-convert)))
  (if (and (featurep 'jisx0213) skk-jisx0213-prohibit)
      (skk-jisx0213-henkan-list-filter)))

(defun skk-henkan-show-candidates ()
  ;; $B%_%K%P%C%U%!$GJQ49$7$?8uJd72$rI=<($9$k!#(B
  (skk-save-point
   (let* ((candidate-keys		; $BI=<(MQ$N%-!<%j%9%H(B
           (mapcar
	    (function (lambda (c)
			(and (memq c '(?\C-g ?\040 ?x)) ; ?\040 is SPC.
			     (skk-error "`%s' $B$KL58z$J%-!<$,;XDj$5$l$F$$$^$9!#(B"
					"Illegal key in `%s'"
					"skk-henkan-show-candidates-keys"))
			(char-to-string (upcase c))))
	    skk-henkan-show-candidates-keys))
          key-num-alist			; $B8uJdA*BrMQ$NO"A[%j%9%H(B
          (key-num-alist1		; key-num-alist $B$rAH$_N)$F$k$?$a$N:n6HMQO"A[%j%9%H!#(B
           (let ((count 6))
             (mapcar (function (lambda (key) (prog1 (cons key count)
                                               (setq count (1- count)))))
                     ;; $B5U$5$^$K$7$F$*$$$F!"I=<($9$k8uJd$N?t$,>/$J$+$C$?$i@h(B
                     ;; $BF,$+$i4v$D$+:o$k!#(B
                     (reverse skk-henkan-show-candidates-keys))))
          (loop 0)
          inhibit-quit
          henkan-list new-one reverse n)
     ;; Emacs 19.28 $B$@$H(B Overlay $B$r>C$7$F$*$+$J$$$H!"<!$K(B insert $B$5$l$k(B
     ;; skk-henkan-key $B$K2?8N$+(B Overlay $B$,$+$+$C$F$7$^$&!#(B
     (and skk-use-face (skk-henkan-face-off))
     (delete-region skk-henkan-start-point skk-henkan-end-point)
     (while loop
       (cond (reverse
	      (setq loop (1- loop)
		    henkan-list (nthcdr (+ 4 (* loop 7)) skk-henkan-list)
		    reverse nil))
	     (skk-exit-show-candidates
	      ;; $B8uJd$,?T$-$F$7$^$C$F!"(Bskk-henkan-show-candidates ->
	      ;; skk-henkan-in-minibuff -> skk-henkan
	      ;; -> skk-henkan-show-candidates $B$N=g$G!":F$S$3$N4X?t$,8F$P$l(B
	      ;; $B$?$H$-$O!"$3$3$G(B henkan-list $B$H(B loop $B$r7W;;$9$k!#(B
	      (setq henkan-list (nthcdr skk-henkan-count skk-henkan-list)
		    loop (car skk-exit-show-candidates)
		    skk-exit-show-candidates nil))
	     (t
	      ;; skk-henkan-show-candidates-keys $B$N:G=*$N%-!<$KBP1~$9$k8uJd(B
	      ;; $B$,=P$F$/$k$^$G%5!<%A$rB3$1$k!#(B
	      (skk-henkan-list-filter)
	      (while (and skk-current-search-prog-list
			  (null (nthcdr (+ 11 (* loop 7)) skk-henkan-list)))
		(setq skk-henkan-list
		      (skk-nunion skk-henkan-list (skk-search)))
		(skk-henkan-list-filter))
	      (setq henkan-list (nthcdr (+ 4 (* loop 7)) skk-henkan-list))))
       (save-window-excursion
	 (setq n (skk-henkan-show-candidate-subr candidate-keys henkan-list))
	 (if (> n 0)
	     (condition-case nil
		 (let* ((event (next-command-event))
			(char (event-to-character event))
			(key (static-cond
			      ((eq skk-emacs-type 'xemacs)
			       (let ((tmp (event-key event)))
				 (if (symbolp tmp)
				     (vector tmp)
				   event)))
			      (t
			       (if char
				   (vector char)
				 (let ((keys (recent-keys)))
				   (vector (aref keys (1- (length keys)))))))))
			num)
		   (if (eq skk-emacs-type 'xemacs)
		       (message ""))	; clear out candidates in echo area
		   (if (and (null char) (null key))
		       (skk-unread-event event)
		     (setq key-num-alist (nthcdr (- 7 n) key-num-alist1))
		     (and key-num-alist char
			  (setq num (cdr (or (assq char key-num-alist)
					     (if (skk-lower-case-p char)
						 (assq (upcase char) key-num-alist)
					       (assq (downcase char) key-num-alist))))))
		     (cond (num
			    (setq new-one (nth num henkan-list)
				  skk-henkan-count (+ 4 (* loop 7) num)
				  skk-kakutei-flag t
				  loop nil))
			   ((eq char ?\040) ; SPC
			    (if (or skk-current-search-prog-list
				    (nthcdr 7 henkan-list))
				(setq loop (1+ loop))
			      ;; $B8uJd$,?T$-$?!#$3$N4X?t$+$iH4$1$k!#(B
			      (let ((last-showed-index (+ 4 (* loop 7))))
				(setq skk-exit-show-candidates
				      ;; cdr $BIt$O!"<-=qEPO?$KF~$kA0$K:G8e$KI=<($7(B
				      ;; $B$?8uJd72$NCf$G:G=i$N8uJd$r;X$9%$%s%G%/%9(B
				      (cons loop last-showed-index))
				;; $B<-=qEPO?$KF~$k!#(Bskk-henkan-count $B$O(B
				;; skk-henkan-list $B$N:G8e$N8uJd$N<!(B ($BB8:_$7$J$$(B
				;; --- nil)$B$r;X$9!#(B
				(setq skk-henkan-count (+ last-showed-index n)
				      loop nil))))
			   ((or (eq char skk-previous-candidate-char) ; ?x
				(member
				 (key-description key)
				 (mapcar
				  (function
				   (lambda (key)
				     (key-description key)))
				  (append
				   (where-is-internal 'skk-previous-candidate
						      skk-j-mode-map)
				   (where-is-internal 'skk-delete-backward-char
						      skk-j-mode-map)
				   (where-is-internal 'skk-undo
						      skk-j-mode-map)))))
			    (if (= loop 0)
				;; skk-henkan-show-candidates $B$r8F$VA0$N>uBV$KLa(B
				;; $B$9!#(B
				(progn
				  (setq skk-henkan-count 4)
				  (skk-unread-event
				   (character-to-event
				    (aref
				     (car (where-is-internal
					   'skk-previous-candidate
					   skk-j-mode-map))
				     0)))
				  ;; skk-henkan $B$^$G0l5$$K(B throw $B$9$k!#(B
				  (throw 'unread nil))
			      ;; $B0l$DA0$N8uJd72$r%(%3!<%(%j%"$KI=<($9$k!#(B
			      (setq reverse t)))
			   ((member
			     (key-description key)
			     (mapcar
			      (function
			       (lambda (key)
				 (key-description key)))
			      (append
			       (where-is-internal 'keyboard-quit
						  skk-j-mode-map)
			       (where-is-internal 'skk-kanagaki-bs
						  skk-j-mode-map)
			       (where-is-internal 'skk-kanagaki-esc
						  skk-j-mode-map))))
			    (signal 'quit nil))
			   (t
			    (skk-message "`%s' $B$OL58z$J%-!<$G$9!*(B"
					 "`%s' is not valid here!"
					 (or (key-description key)
					     (key-description char)))
			    (sit-for 1)))))
	       (quit
		;; skk-previous-candidate $B$X(B
		(setq skk-henkan-count 0)
		(skk-unread-event
		 (character-to-event
		  (aref
		   (car (where-is-internal 'skk-previous-candidate skk-j-mode-map))
		   0)))
		;; skk-henkan $B$^$G0l5$$K(B throw $B$9$k!#(B
		(throw 'unread nil)))))) ; end of while loop
     (if (consp new-one)
         (cdr new-one)
       new-one))))

(defun skk-henkan-show-candidate-subr (keys candidates)
  ;; key $B$H(B candidates $B$rAH$_9g$o$;$F(B 7 $B$D$N8uJd72(B ($B8uJd?t$,(B 7 $B$KK~$?$J$+$C(B
  ;; $B$?$i$=$3$GBG$A@Z$k(B) $B$NJ8;zNs$r:n$j!"%_%K%P%C%U%!$KI=<($9$k!#(B
  (let ((workinglst
	 ;; CANDIDATES $B$N@hF,$N(B 7 $B$D$N$_$N%j%9%H!#(B
	 (let ((count 0) e v)
	   (while (> 7 count)
	     (setq e (nth count candidates))
	     (if e
		 (setq v (cons (cond ((and (skk-numeric-p) (consp e))
				      (cdr e))
				     ((not (skk-lisp-prog-p e))
				      e)
				     ((skk-eval-string e))
				     (t e))
			       v)
		       count (1+ count))
	       (setq count 7)))
	   (nreverse v)))
	(string-width-function
	 (static-cond ((memq skk-emacs-type '(mule4 mule5))
		       'string-bytes)
		      (t
		       'string-width)))
	(n 0) str cand message-log-max)
    (if (not (car workinglst))
        nil
      ;;(setq workinglst (skk-truncate-message workinglst))
      (setq n 1
            ;; $B:G=i$N8uJd$NA0$K6uGr$r$/$C$D$1$J$$$h$&$K:G=i$N8uJd$@$1@h$K<h$j(B
            ;; $B=P$9!#(B
            str (concat (car keys) ":" (if (consp (car workinglst))
					   (cdr (car workinglst))
					 (car workinglst))))
      ;; $B;D$j$N(B 6 $B$D$r<h$j=P$9!#8uJd$H8uJd$N4V$r6uGr$G$D$J$0!#(B
      (while (and (< n 7) (setq cand (nth n workinglst)))
        (setq cand (if (consp cand) (cdr cand) cand)
              str (concat str "  " (nth n keys) ":" cand)
              n (1+ n)))
      (setq str (format
		 "%s  [$B;D$j(B %d%s]"
		 str (length (nthcdr n candidates))
		 (make-string (length skk-current-search-prog-list) ?+)))
      (if (> (frame-width) (funcall string-width-function str))
	  (message "%s" str)
	(let ((buff (get-buffer-create "*$B8uJd(B*"))
	      (case-fold-search t))
	  (save-excursion
	    (set-buffer buff)
	    (erase-buffer)
	    (insert str)
	    (goto-char (point-min))
	    ;; 1 $B8uJd$K(B 1 $B9T$r$o$j$"$F$k!#(B
	    (forward-char 2)
	    (while (re-search-forward
		    (concat "  "
			    (mapconcat 'identity keys ":\\|  ") ":\\|"
			    "  \\[$B;D$j(B [0-9]+\\(\\++\\)?\\]") nil t)
	      (goto-char (match-beginning 0))
	      (delete-char 2)
	      (insert "\n"))
	    (goto-char (point-min))
	    (while (and (move-to-column (- (frame-width) 2))
			(not (eobp))
			(>= (frame-width) (current-column)))
	      (when (not (eolp))
		(backward-char 1)
		(insert "\n  "))
	      (forward-line 1)))
	  (unless (eq (next-window) (selected-window))
	    ;; *$B8uJd(B* $B%P%C%U%!$r8+0W$/$9$k!#(B
	    ;; (save-window-excursion $B$NCf$J$N$GBg>fIW$J$O$:(B)
	    (delete-other-windows))
	  (display-buffer buff)
	  (or (pos-visible-in-window-p)
	      (recenter '(1))))))
    ;; $BI=<($9$k8uJd?t$rJV$9!#(B
    n))

(defun skk-henkan-in-minibuff ()
  ;; $B<-=qEPO?%b!<%I$KF~$j!"EPO?$7$?C18l$NJ8;zNs$rJV$9!#(B
  (save-match-data
    (let ((enable-recursive-minibuffers t)
          ;; $BJQ49Cf$K(B isearch message $B$,=P$J$$$h$&$K$9$k!#(B
          skk-isearch-message orglen new-one)
      (static-unless (memq skk-emacs-type '(nemacs mule1))
	(add-hook 'minibuffer-setup-hook 'skk-j-mode-on)
	(add-hook
	 'minibuffer-setup-hook
	 (function (lambda ()
		     (add-hook 'pre-command-hook 'skk-pre-command nil 'local)))))
      (condition-case nil
          (setq new-one
                (read-from-minibuffer
                 (concat (or (and (skk-numeric-p) (skk-num-henkan-key))
                             (if skk-okuri-char
                                 (skk-compute-henkan-key2)
                               skk-henkan-key))
                         " ")
		 (if (and (not skk-okuri-char)
			  skk-read-from-minibuffer-function)
		     (funcall skk-read-from-minibuffer-function))
		 (static-when (memq skk-emacs-type '(nemacs mule1))
		   ;; Emacs 18 $B$G$O(B minibuffer-setup-hook $B$,8z$+$J$$$N$G!"D>@\(B
		   ;; skk-mode $B$r5/F0$9$k!#(Bkeymap $B$bE,@Z$KM?$($kI,MW$,$"$k!#(B
		   (with-current-buffer
		       (get-buffer-create
			(format " *Minibuf-%d*" (minibuffer-depth)))
		     (skk-j-mode-on))
		   (append skk-j-mode-map (cdr minibuffer-local-map)))))
        (quit
         (setq new-one "")))
      (if (and skk-check-okurigana-on-touroku
	       ;; $BAw$j$"$jJQ49$G$b(B skk-okuri-char $B$@$1$@$HH=CG$G$-$J$$!#(B
	       skk-henkan-okurigana new-one)
	  (setq new-one (skk-remove-redundant-okurgana new-one)))
      (if (string= new-one "")
          (if skk-exit-show-candidates
              ;; $B%_%K%P%C%U%!$KI=<($7$?8uJd$,?T$-$F<-=qEPO?$KF~$C$?$,!"6uJ8;z(B
              ;; $BNs$,EPO?$5$l$?>l9g!#:G8e$K%_%K%P%C%U%!$KI=<($7$?8uJd72$r:FI=(B
              ;; $B<($9$k!#(B
              (progn
                (setq skk-henkan-count (cdr skk-exit-show-candidates))
                (skk-henkan))
            ;; skk-henkan-show-candidates $B$KF~$kA0$K8uJd$,?T$-$?>l9g(B
            (setq skk-henkan-count (1- skk-henkan-count))
            (if (= skk-henkan-count -1)
                (progn
                  ;; $BAw$j$"$j$NJQ49$G<-=qEPO?$KF~$j!"6uJ8;z$rEPO?$7$?8e!"$=$N(B
                  ;; $B$^$^:FEYAw$j$J$7$H$7$FJQ49$7$?>l9g$O(B
                  ;; skk-henkan-okurigana, skk-okuri-char $B$NCM$r(B nil $B$K$7$J$1(B
                  ;; $B$l$P!"$=$l$>$l$NCM$K8E$$Aw$j2>L>$,F~$C$?$^$^$G8!:w$K<:GT(B
                  ;; $B$9$k!#(B
                  (setq skk-henkan-okurigana nil
                        skk-okurigana nil
                        skk-okuri-char nil)
                  (skk-change-marker-to-white))
              ;; skk-henkan-count $B$,(B -1 $B$G$J$1$l$P!"%+%l%s%H%P%C%U%!$G$O:G8e$N(B
              ;; $B8uJd$rI=<($7$?$^$^$J$N$G(B ($BI=<(4XO"$G$O2?$b$7$J$/$F$b!"$b$&4{(B
              ;; $B$KK>$_$N>uBV$K$J$C$F$$$k(B) $B2?$b$7$J$$!#(B
	      ))
        (and (string-match "[ $B!!(B]+$" new-one)
	     (setq new-one (substring new-one 0 (match-beginning 0))))
	(setq skk-henkan-list (nconc skk-henkan-list (list new-one)))
	(if (skk-numeric-p)
	    (progn
	      (setq orglen (length skk-henkan-list))
	      (skk-num-convert)
	      (setq new-one (cdr (skk-get-current-candidate-1)))))
	(if (or (not orglen) (= orglen (length skk-henkan-list)))
	    (setq skk-kakutei-flag t))
        (setq skk-henkan-in-minibuff-flag t
              skk-touroku-count (1+ skk-touroku-count)))
      ;; (nth skk-henkan-count skk-henkan-list) $B$,(B nil $B$@$+$i<-=qEPO?$K(B
      ;; $BF~$C$F$$$k!#(Bskk-henkan-count $B$r%$%s%/%j%a%s%H$9$kI,MW$O$J$$!#(B
      ;; new-one $B$,6uJ8;zNs$@$C$?$i(B nil $B$rJV$9!#(B
      (if (string= new-one "")
	  nil
	(if (string-match ";" new-one)
	    (skk-quote-semicolon new-one)
	  new-one)))))

(defun skk-compute-henkan-key2 ()
  ;; skk-henkan-okurigana $B$,(B non-nil $B$J$i(B skk-henkan-key $B$+$i!"$+$D$F(B
  ;; skk-henkan-key2 $B$H8F$P$l$F$$$?$b$N$r:n$k!#(B
  ;; skk-henkan-key2 $B$H$O!"!V4A;zItJ,$NFI$_(B + "*" + $BAw$j2>L>!W$N7A<0$NJ8;zNs$r(B
  ;; $B8@$&!#(B
  (if skk-henkan-okurigana
      (save-match-data
	(string-match "[a-z]+$" skk-henkan-key)
	(concat (substring skk-henkan-key 0 (match-beginning 0))
		"*" skk-henkan-okurigana))))

(defun skk-remove-redundant-okurgana (word)
  ;; $BAw$j$"$j$NEPO?$r$9$k$H$-!"Aw$j2>L>$r>C$7$F$+$i(B [RET] $B$r2!$5$J$1$l$P@5$7$/(B
  ;; $BEPO?$G$-$J$$!#$=$3$G!"%f!<%6$,4V0c$($FAw$j2>L>$r>C$7K:$l$F$$$J$$$+$I$&$+!"(B
  ;; SKK $B$NB&$G%A%'%C%/$G$-$kHO0O$K$D$$$F$O%f!<%6$N3NG'$r<h$k!#$3$NItJ,$O(B
  ;; `skk-check-okurigana-on-touroku' $B$r(B non-nil $B$K@_Dj$7$F$$$k>l9g$N$_M-8z!#(B
  ;; $BJQ49$,9T$J$o$l$?%P%C%U%!$G%3!<%k$5$l$k(B ($B%_%K%P%C%U%!!"<-=q%P%C%U%!$G$O$J$$(B)$B!#(B
  (save-match-data
    (let* ((len (skk-str-length word))
	   (str1 (when (< 0 len)
		   (skk-substring word (1- len) len)))
	   (str2 (when (< 1 len)
		   (skk-substring word (- len 2) (1- len))))
	   (str (if (string-match "^[$B$!(B-$B$s(B]$" str2)
		    (concat str2 str1)
		  str1)))
      (when (and str
		 (string-match "^[$B$!(B-$B$s(B]$" str1)
		 (or
		  (eq skk-check-okurigana-on-touroku 'auto)
		  (skk-y-or-n-p
		   (format "$B<-=qEPO?$GF~NO$7$?(B `%s' $B$N(B `%s' $B$OAw$j2>L>$G$9$+!)(B"
			   word str)
		   (format "You mean `%s' in `%s' you've registered is okurigana?"
			   str word))))
	;; $B%f!<%6$N;X<($K=>$$Aw$j2>L>$r<h$j=|$/!#(B
	(message "")
	(setq word (skk-substring word 0
				  (if (string-match "^[$B$!(B-$B$s(B]$" str2)
				      (- len 2)
				    (1- len)))))))
  word)

(defun skk-setup-minibuffer ()
  ;; $B%+%l%s%H%P%C%U%!$NF~NO%b!<%I$K=>$$%_%K%P%C%U%!$NF~NO%b!<%I$r@_Dj$9$k!#(B
  (cond ((eq skk-minibuffer-origin-mode 'hiragana)
	 (skk-j-mode-on))
	((eq skk-minibuffer-origin-mode 'katakana)
	 (skk-j-mode-on t))
	((eq skk-minibuffer-origin-mode 'abbrev)
	 (skk-abbrev-mode-on))
	((eq skk-minibuffer-origin-mode 'latin)
	 (skk-latin-mode-on))
	((eq skk-minibuffer-origin-mode 'jisx0208-latin)
	 (skk-jisx0208-latin-mode-on))))

(defun skk-previous-candidate (&optional arg)
  "$B"'%b!<%I$G$"$l$P!"0l$DA0$N8uJd$rI=<($9$k!#(B
$B"'%b!<%I0J30$G$O%+%l%s%H%P%C%U%!$K(B \"x\" $B$rA^F~$9$k!#(B
$B3NDj<-=q$K$h$k3NDj$ND>8e$K8F$V$H3NDj$,%"%s%I%%$5$l$F!"3NDjA0$N>uBV$G(B
$BD>A0$N8+=P$78l$,%+%l%s%H%P%C%U%!$KA^F~$5$l$k!#(B"
  (interactive "*p")
  (skk-with-point-move
   (if (not skk-henkan-active)
       (if (not (eq last-command 'skk-kakutei-henkan))
	   (and last-command-char (characterp last-command-char)
		(skk-kana-input arg))
	 ;; restore the state just before the last kakutei henkan.
	 (delete-region skk-henkan-start-point (point))
	 (skk-set-henkan-point-subr)
	 (insert-and-inherit 
	  (if (not skk-katakana)
	      (skk-get-last-henkan-datum 'henkan-key)
	    (skk-hiragana-to-katakana (skk-get-last-henkan-datum 'henkan-key))))
	 (setq this-command 'skk-undo-kakutei-henkan))
     (if (string= skk-henkan-key "")
	 nil
       (let ((mark
	      (if (not (eobp))
		  (skk-save-point (forward-char 1) (point-marker)))))
	 (skk-save-point
	  (if (= skk-henkan-count 0)
	      (progn
		(and skk-okuri-char
		     ;; roman prefix for okurigana should be removed.
		     (setq skk-henkan-key (substring skk-henkan-key 0 -1)))
		(if skk-katakana
		    (setq skk-henkan-key
			  (skk-hiragana-to-katakana skk-henkan-key)))
		(setq skk-henkan-count -1
		      skk-henkan-in-minibuff-flag nil
		      skk-henkan-list nil
		      skk-henkan-okurigana nil
		      skk-okuri-char nil
		      skk-okuri-index-min -1
		      skk-okuri-index-max -1
		      skk-okurigana nil
		      skk-prefix "")
		(and (skk-numeric-p) (skk-num-initialize))
		;; Emacs 19.28 $B$@$H(B Overlay $B$r>C$7$F$*$+$J$$$H!"<!$K(B insert $B$5$l(B
		;; $B$k(B skk-henkan-key $B$K2?8N$+(B Overlay $B$,$+$+$C$F$7$^$&!#(B
		(and skk-use-face (skk-henkan-face-off))
		(delete-region skk-henkan-start-point skk-henkan-end-point)
		(goto-char skk-henkan-end-point)
		(insert-and-inherit skk-henkan-key)
		(skk-change-marker-to-white))
	    (setq skk-henkan-count (1- skk-henkan-count))
	    (skk-insert-new-word (skk-get-current-candidate))))
	 (if mark
	     (progn
	       (goto-char mark)
	       (skk-set-marker mark nil)
	       (backward-char 1))
	   (goto-char (point-max)))
	 (and skk-abbrev-mode (= skk-henkan-count -1) (skk-abbrev-mode-on)))))))

(defun skk-undo (&optional arg)
  (interactive "*P")
  (cond (skk-henkan-active
	 (skk-previous-candidate))
	(skk-henkan-on
	 (if (= (point) (marker-position skk-henkan-start-point))
	     (skk-kakutei arg)
	   (forward-char -1)
	   (delete-char 1)))
	(t
	 (skk-emulate-original-map arg))))

(defun skk-insert-new-word (word)
  ;; $B8+=P$78l$r>C$7!"$=$N>l=j$XJQ497k2L$NJ8;zNs$rA^F~$9$k!#(B
  ;; Emacs 19.28 $B$@$H(B Overlay $B$r>C$7$F$*$+$J$$$H!"<!$K(B insert $B$5$l$k(B
  ;; skk-henkan-key $B$K2?8N$+(B Overlay $B$,$+$+$C$F$7$^$&!#(B
  (save-match-data
    (let (annotation)
      (if (string-match ";" word)
	  (setq annotation (substring word (match-end 0))
		word (substring word 0 (match-beginning 0))))
      (setq word (if (skk-lisp-prog-p word) (skk-eval-string word) word))
      (and skk-use-face (skk-henkan-face-off))
      (delete-region skk-henkan-start-point skk-henkan-end-point)
      (goto-char skk-henkan-start-point)
      (insert-and-inherit word)
      (skk-set-marker skk-henkan-end-point (point))
      (and skk-use-face (skk-henkan-face-on))
      (if (and skk-show-annotation annotation)
	  (skk-annotation-show annotation))
      (and skk-insert-new-word-function
	   (funcall skk-insert-new-word-function)))))

(defun skk-kakutei (&optional word)
  "$B8=:_I=<($5$l$F$$$k8l$G3NDj$7!"<-=q$N99?7$r9T$&!#(B
$B%+%l%s%H%P%C%U%!$G(B SKK $B%b!<%I$K$J$C$F$$$J$+$C$?$i(B SKK $B%b!<%I$KF~$k!#(B
$B%*%W%7%g%J%k0z?t$N(B WORD $B$rEO$9$H!"8=:_I=<($5$l$F$$$k8uJd$H$OL54X78$K(B WORD $B$G3N(B
$BDj$9$k!#(B"
  ;; read only $B$G%(%i!<$K$J$k$h$&$K$9$k$H(B read only $B%P%C%U%!$G(B SKK $B$,5/F0$G$-(B
  ;; $B$J$/$J$k!#(B
  (interactive)
  (let ((inhibit-quit t)
	converted kakutei-word var)
    (if (not skk-henkan-on)
	nil
      (if skk-henkan-active
	  (progn
	    (setq kakutei-word
		  ;; $B3NDj<-=q$N8l$G3NDj$7$?$H$-$O!"<-=q$K$=$N8l$r=q$-9~$`I,MW$b$J(B
		  ;; $B$$$7!"99?7$9$kI,MW$b$J$$$H;W$C$F$$$?$,!"Jd40$r9T$J$&$H$-$O!"(B
		  ;; $B8D?M<-=q$r;2>H$9$k(B ($B3NDj<-=q$O;2>H$7$J$$(B) $B$N$G!"B?>/;q8;$H;~(B
		  ;; $B4V$rL5BL$K$7$F$b!"8D?M<-=q$K3NDj<-=q$N%(%s%H%j$r=q$-9~$s$G99(B
		  ;; $B?7$b$7$F$*$/!#(B
		  (or word (skk-get-current-candidate 'noconv)))
	    (if (or
		 (and (not skk-search-excluding-word-pattern-function) kakutei-word)
		 (and
		  kakutei-word skk-search-excluding-word-pattern-function
		  (not
		   (funcall skk-search-excluding-word-pattern-function kakutei-word))))
		(progn
		  (skk-update-jisyo kakutei-word)
		  (if (skk-numeric-p)
		      (progn
			(setq converted (skk-get-current-candidate))
			(skk-num-update-jisyo kakutei-word converted)))))))
      (if skk-mode
	  (progn
	    (skk-kakutei-cleanup-buffer)
	    ;; KAKUTEI-WORD $B$J$I$N>pJs$,I,MW$G$"$l$P!"(Bskk-last-henkan-data
	    ;; $B$+$iF@$i$l$k!#I,MW$J%G!<%?$,$=$l$i$NJQ?t$K8BDj$5$l$J$$$N$G!"(B
	    ;; $B0z?t$K$7$J$$!#(B
	    (and skk-kakutei-end-function (funcall skk-kakutei-end-function))
	    (skk-kakutei-initialize
	     (if (skk-numeric-p) (cons kakutei-word converted) kakutei-word)))))
    (skk-do-auto-fill)
    (if skk-mode
	(skk-j-mode-on skk-katakana)
      ;; $B%+%l%s%H%P%C%U%!$G$^$@(B skk-mode $B$,%3!<%k$5$l$F$$$J$+$C$?$i!"%3!<%k$9$k!#(B
      (skk-mode 1))))

(defun skk-kakutei-cleanup-buffer ()
  ;; $B3NDjD>8e$N%P%C%U%!$N@07A$r9T$J$&!#(B
  (if skk-okurigana
      (progn
        (skk-delete-okuri-mark)))
  ;;(and skk-katakana skk-convert-okurigana-into-katakana
  ;;     (< skk-henkan-end-point (point))
  ;;     (skk-katakana-region skk-henkan-end-point (point)))
  (skk-delete-henkan-markers)
  (and (boundp 'self-insert-after-hook) self-insert-after-hook
       (funcall self-insert-after-hook skk-henkan-start-point (point)))
  (and overwrite-mode
       (skk-del-char-with-pad
	(skk-ovwrt-len
	 (string-width
	  (buffer-substring-no-properties skk-henkan-start-point (point)))))))

(defun skk-kakutei-initialize (&optional kakutei-word)
  ;; $B3NDj;~$KJQ?t$N=i4|2=$H%"%s%I%%$N$?$a$NJQ?t$NJ]B8$r9T$J$&!#(B
  (if (and kakutei-word (or (consp kakutei-word)
                            (not (string= kakutei-word ""))))
      (progn
	(setq skk-kakutei-count (1+ skk-kakutei-count))
        ;; skk-undo-kakutei $B$N$?$a$K:G8e$NJQ49$N%G!<%?$rJ]B8$9$k!#(B
	(skk-put-last-henkan-data
	 (list (cons 'henkan-key skk-henkan-key)
	       (cons 'okuri-char skk-okuri-char)
	       (cons 'henkan-okurigana skk-henkan-okurigana)
	       (cons 'henkan-list
		     ;; $B3NDj$7$?8l$r@hF,$K$9$k!#(B
		     (cons kakutei-word
			   (delete kakutei-word skk-henkan-list)))
	       ;; (eq last-command 'skk-kakutei-henkan) $B$G%]!<%?%V%k$K3NG'$G$-(B
	       ;; $B$k$N$G$"$($F$$$i$J$$$+!#(B
	       ;; (cons 'kakutei-henkan (eq this-command 'skk-kakutei-henkan))
	       ;; $B>e5-0J30$N(B henkan data $B$r(B skk-last-henkan-data $B$K;D$7$?$+$C$?$i!"(B
	       ;; skk-kakutei-end-function $B$rMxMQ$9$k!#(B
	      ))))
  (setq skk-abbrev-mode nil
        skk-exit-show-candidates nil
        skk-henkan-active nil
        skk-henkan-count -1
	skk-henkan-in-minibuff-flag nil
        skk-henkan-key nil
        skk-henkan-list nil
        skk-henkan-okurigana nil
        skk-henkan-on nil
        skk-kakutei-flag nil
        skk-okuri-char nil
	skk-okuri-index-min -1
	skk-okuri-index-max -1
	;; skk-prefix ""
	))

(defun skk-undo-kakutei ()
  "$B0lHV:G8e$N3NDj$r%"%s%I%%$7!"8+=P$7$KBP$9$k8uJd$rI=<($9$k!#(B
$B:G8e$K3NDj$7$?$H$-$N8uJd$O%9%-%C%W$5$l$k!#(B
$B8uJd$,B>$K$J$$$H$-$O!"%_%K%P%C%U%!$G$N<-=qEPO?$KF~$k!#(B"
  (interactive)
  (skk-with-point-move
   (cond ((eq last-command 'skk-undo-kakutei)
	  (skk-error "$B3NDj%"%s%I%%$OO"B3;HMQ$G$-$^$;$s(B"
		     "Cannot undo kakutei repeatedly"))
	 (skk-henkan-active
	  (skk-error "$B"'%b!<%I$G$O3NDj%"%s%I%%$G$-$^$;$s(B"
		     "Cannot undo kakutei in $B"'(B mode"))
	 ( ; skk-henkan-key may be nil or "".
	  (or (not (skk-get-last-henkan-datum 'henkan-key))
	      (string= (skk-get-last-henkan-datum 'henkan-key) ""))
	  (skk-error "$B%"%s%I%%%G!<%?$,$"$j$^$;$s(B" "Lost undo data")))
   (condition-case nil
       (let ((end
	      (if (skk-get-last-henkan-datum 'henkan-okurigana)
		  (+ (length (skk-get-last-henkan-datum 'henkan-okurigana))
		     skk-henkan-end-point)
		skk-henkan-end-point)))
	 (setq skk-henkan-active t
	       skk-henkan-on t
	       skk-current-search-prog-list
	       (if (eq (car (car skk-search-prog-list))
		       'skk-search-kakutei-jisyo-file)
		   ;; $B3NDj<-=q$OC5$7$F$bL50UL#!#(B
		   (cdr skk-search-prog-list)
		 skk-search-prog-list))
	 ;; get henkan data back from skk-last-henkan-data.
	 (setq skk-henkan-key (skk-get-last-henkan-datum 'henkan-key)
	       skk-henkan-list (skk-get-last-henkan-datum 'henkan-list)
	       skk-henkan-okurigana (skk-get-last-henkan-datum 'henkan-okurigana)
	       skk-okuri-char (skk-get-last-henkan-datum 'okuri-char))
	 (and skk-use-numeric-conversion
	      (setq skk-num-list (skk-get-last-henkan-datum 'skk-num-list)))
	 (and (>= (point-max) end)
	      ;; $B:G8e$NJQ49ItJ,$N%F%-%9%H$r>C$9!#Aw$j2>L>$rGD0.$7$F$$$k$N$J$i(B
	      ;; (skk-process-okuri-early $B$,(B non-nil $B$J$iAw$j2>L>$rGD0.$G$-$J$$(B)$B!"(B
	      ;; $BAw$j2>L>$r4^$a$?ItJ,$^$G$r>C$9!#(B
	      (delete-region skk-henkan-start-point end))
	 (goto-char skk-henkan-start-point)
	 (insert-and-inherit "$B"'(B")
	 (skk-set-marker skk-henkan-start-point (point))
	 (if skk-okuri-char
	     (progn			; $BAw$j$"$j(B
	       (insert-and-inherit (substring skk-henkan-key 0
					      (1- (length skk-henkan-key))))
	       (skk-set-marker skk-henkan-end-point (point))
	       (and skk-henkan-okurigana (insert-and-inherit skk-henkan-okurigana)))
	   (insert-and-inherit skk-henkan-key)
	   (skk-set-marker skk-henkan-end-point (point)))
	 (skk-message "$B3NDj%"%s%I%%!*(B" "Undo kakutei!")
	 (setq skk-henkan-count 1)
	 (skk-henkan))
     ;; skk-kakutei-undo $B$+$iESCf$GH4$1$?>l9g$O!"3F<o%U%i%0$r=i4|2=$7$F$*$+$J$$(B
     ;; $B$H<!$NF0:n$r$7$h$&$H$7$?$H$-$K%(%i!<$K$J$k!#(B
     ((error quit) (skk-kakutei)))))

(defun skk-set-henkan-point (&optional arg)
  ;;"$BJQ49$r3+;O$9$k%]%$%s%H$r%^!<%/$7!"BP1~$9$k(B skk-prefix $B$+!"Jl2;$rF~NO$9$k!#(B"
  (let* ((last-char (skk-downcase last-command-char))
	 (normal (not (eq last-char last-command-char)))
	 (sokuon (and (string= skk-prefix (char-to-string last-char))
		      (/= last-char ?o)))
	 (henkan-active skk-henkan-active))
    (if (or (not skk-henkan-on) skk-henkan-active)
	(if normal
	    (skk-set-henkan-point-subr)
	  (and skk-henkan-on (skk-set-henkan-point-subr))
	  (if henkan-active
	      (skk-emulate-original-map arg)
	    ;; What's to be here?
	    ;;(skk-self-insert arg)
	  ))
      (if (not normal)
	  (progn			; special char
	    (insert-and-inherit last-char)
	    (skk-set-marker skk-henkan-end-point (point))
	    (setq skk-henkan-count 0
		  skk-henkan-key (buffer-substring-no-properties
				  skk-henkan-start-point (point))
		  skk-prefix "")
	    (skk-henkan))
	;; prepare for the processing of okurigana if not skk-okurigana
	;; and the preceding character is not a numeric character.
	;; if the previous char is a special midashi char or a
	;; numeric character, we assume that the user intended to type the
	;; last-command-char in lower case.
	(if (and (or (not (skk-get-prefix skk-current-rule-tree)) ; for KAnji, KanJIru
		     (and
		      (not (= skk-henkan-start-point skk-kana-start-point))
		      (or sokuon	; for TaSSi or TasSi
			  (skk-kana-cleanup)))) ; for NEko
		 (not skk-okurigana)
		 (or (= skk-henkan-start-point (point))
		     (let ((p (char-before)))
		       (not
			(or
			 ;; previous char is a special midashi char
			 (memq p skk-special-midashi-char-list)
			 ;; previous char is an ascii numeric char
			 (and (<= ?0 p) (<= p ?9))
			 ;; previous char is a JIS X 0208 numeric char
			  (and (skk-jisx0208-p p)
			       (= (skk-char-octet p 0) 35) ;?#
			       (<= 48 (skk-char-octet p 1)) ; ?0
			       (<= (skk-char-octet p 1) 57))  ; ?9
			)))))
	    (if skk-process-okuri-early
		(progn
		  (skk-set-marker skk-henkan-end-point (point))
		  (setq skk-okuri-char (char-to-string last-char))
		  (if sokuon
		      (progn
			(setq skk-henkan-key
			      (concat (buffer-substring-no-properties
				       skk-henkan-start-point
				       skk-kana-start-point)
				      (if skk-katakana "$B%C(B" "$B$C(B")
				      skk-henkan-okurigana))
			(skk-erase-prefix)
			(insert-and-inherit (if skk-katakana "$B%C(B " "$B$C(B "))
			(setq skk-prefix ""
			      skk-henkan-count 0)
			(skk-henkan)
			(delete-backward-char 2))
		    (setq skk-henkan-key (concat
					  (buffer-substring-no-properties
					   skk-henkan-start-point
					   (point))
					  skk-okuri-char))
		    (insert-and-inherit " ")
		    (setq skk-prefix ""
			  skk-henkan-count 0)
		    (skk-henkan)
		    (delete-backward-char 1))
		  ;; we set skk-kana-start-point here, since the marker may no
		  ;; longer point at the correct position after skk-henkan.
		  (skk-set-marker skk-kana-start-point (point)))
	      (if (= skk-henkan-start-point (point))
		  nil
		(if sokuon
		    (progn
		      (skk-erase-prefix 'clean)
		      (insert-and-inherit (if skk-katakana "$B%C(B" "$B$C(B"))))
		(skk-set-marker skk-okurigana-start-point (point))
		(insert-and-inherit "*")
		(skk-set-marker skk-kana-start-point (point))
		(setq skk-okuri-char (char-to-string last-char)
		      skk-okurigana t))))))
    (if normal
	(progn
	  (setq last-command-char last-char)
	  (skk-kana-input arg)))))

(defun skk-start-henkan (arg)
  "$B"&%b!<%I$G$O4A;zJQ49$r3+;O$9$k!#"'%b!<%I$G$O<!$N8uJd$rI=<($9$k!#(B
$B"&%b!<%I$G!"%+%?%+%J%b!<%I$N$^$^4A;zJQ49$r3+;O$9$k$H!"8+=P$78l$rJ?2>L>$K(B
$BJQ498e!"4A;zJQ49$r3+;O$9$k!#(B
$B8+=P$78l$NJQ49$;$:$K$=$N$^$^4A;zJQ49$r9T$J$$$?$1$l$P!"(BC-u SPC \(arg $B$,(B 4
$B$K$J$k(B\) $B$H%?%$%W$9$k!#(B"
  (interactive "*p")
  (skk-with-point-move
   (cancel-undo-boundary)
   (if skk-henkan-active
       (progn
	 (setq skk-henkan-count (1+ skk-henkan-count))
	 (skk-henkan))
     (save-match-data
       (let (pos)
	 (skk-kana-cleanup 'force)
	 (and (skk-get-prefix skk-current-rule-tree)
	      ;; Never.  `skk-erase-prefix' called by `skk-kana-cleanup'
	      ;; initializes `skk-prefix'.
	      (skk-error "$B%U%#%C%/%9$5$l$F$$$J$$(B skk-prefix $B$,$"$j$^$9(B"
			 "Have unfixed skk-prefix"))
	 (setq pos (point))
	 (and (< pos skk-henkan-start-point)
	      (skk-error
	       "$B%+!<%=%k$,JQ493+;OCOE@$h$jA0$K$"$j$^$9(B"
	       "Henkan end point must be after henkan start point"))
	 (setq skk-henkan-key (buffer-substring-no-properties
			       skk-henkan-start-point pos))
	 (and skk-katakana (= arg 1)
	      (setq skk-henkan-key (skk-katakana-to-hiragana skk-henkan-key)))
	 (and skk-okurigana (string-match "\\* *$" skk-henkan-key)
	      (skk-error
	       "$B6u$NAw$j2>L>$G4A;z$rEPO?$7$h$&$H$7$F$$$^$9(B"
	       "No okurigana!"))
	 (if skk-allow-spaces-newlines-and-tabs
	     ;; skk-henkan-key $B$NCf$N(B "[ \n\t]+" $B$r40A4$K<h$j=|$/!#(B
	     (while (string-match "[ \n\t]+" skk-henkan-key)
	       (setq skk-henkan-key
		     (concat (substring skk-henkan-key 0 (match-beginning 0))
			     (substring skk-henkan-key (match-end 0)))))
	   (skk-save-point
	    (beginning-of-line)
	    (and (> (point) skk-henkan-start-point)
		 (skk-error
		  "$BJQ49%-!<$K2~9T$,4^$^$l$F$$$^$9(B"
		  "Henkan key may not contain a new line character")))
	   ;; $B:G=i$N%9%Z!<%9$G(B skk-henkan-key $B$r$A$g$s@Z$k$@$1!#(B
	   (setq skk-henkan-key (substring skk-henkan-key 0
					   (string-match " "
							 skk-henkan-key))))
	 (skk-set-marker skk-henkan-end-point pos)
	 (setq skk-henkan-count 0)
	 (skk-henkan)
	 (if (and skk-abbrev-mode skk-henkan-active)
	     (progn
	       ;; $B$3$&$7$F$*$+$J$$$HJQ498e!"<!$KF~NO$5$l$kJ8;z$b$^$?(B
	       ;; SKK abbrev-mode $BF~NO$K$J$C$F$7$^$&!#(B
	       (skk-j-mode-on skk-katakana)
	       (setq skk-abbrev-mode t))))))))

(defun skk-auto-start-henkan (str)
  ;; skk-auto-start-henkan-keyword-list $B$NMWAG$NJ8;zNs$rA^F~$7$?$H$-$K<+F0E*$K(B
  ;; ($B%9%Z!<%9$rBG80$7$J$/$H$b(B) $BJQ49$r3+;O$9$k!#%(!<!_%$%=%U%H<R$N(B MSDOS $BMQ(B $B$N(B
  ;; FEP$B!"(BWX2+ $BIw!#(B
  (and (member str skk-auto-start-henkan-keyword-list)
       (skk-save-point
        (backward-char 1)
        (and (> (point) skk-henkan-start-point)
	     (let ((skk-prefix ""))
	       (skk-start-henkan (prefix-numeric-value current-prefix-arg)))))))

(defun skk-backward-and-set-henkan-point (arg)
  "$B%]%$%s%H$ND>A0$K$"$kJ8;zNs$N@hF,$KJQ493+;O%]%$%s%H$r<($9(B \"$B"&(B\" $B$rIU$1$k!#(B
$B%+!<%=%k$ND>A0$K$"$kJ8;z(B \($B%9%Z!<%9J8;z!"%?%VJ8;z!"D92;$rI=$o$9!V!<!W(B $B$OL5>r7o(B
$B$K%9%-%C%W$5$l$k(B\) $B$r(B skk-what-char-type $B$K$FH=JL$7!"F1<o$NJ8;zNs$r$R$H$+$?$^(B
$B$j$H$7$F8eJ}$X%9%-%C%W$9$k!#(B
$BC"$7!"$R$i$+$J$N>l9g$O!V$r!W$ND>A0$G!"%+%?%+%J$N>l9g$O!V%r!W$ND>A0$G;_$^$k!#(B
C-u ARG $B$G(B ARG $B$rM?$($k$H!"$=$NJ8;zJ,$@$1La$C$FF1$8F0:n$r9T$J$&!#(B"
  (interactive "*P")
  (if (not skk-mode)
      (skk-emulate-original-map arg)
    (catch 'exit1
      (skk-save-point
       ;; $B$H$j$"$($::G=i$N(B SPC, TAB, $BA43Q(B SPC $B$@$1%8%c%s%W$9$k!#(B
       (skip-chars-backward " \t$B!!(B")
       ;; $B0z?t$"$j!#(B
       (if arg
	   (if (not skk-allow-spaces-newlines-and-tabs)
	       (backward-char (prefix-numeric-value arg))
	     (setq arg (prefix-numeric-value arg))
	     (while (> arg 0)
	       (skip-chars-backward " \t$B!!(B")
	       (if (bolp)
		   ;; $B9TF,$@$C$?$i0l9TA0$N9TKv$^$GLa$k$,!"(Barg $B$O8:$i$5$J$$!#(B
		   (backward-char 1)
		 (backward-char 1)
		 (setq arg (1- arg)))))
	 ;; $B0z?t$J$7!#(B
	 (let ((limit
		(if (not skk-allow-spaces-newlines-and-tabs)
		    (skk-save-point (beginning-of-line) (point))
		  (point-min)))
	       ;; $B!2!1!0!/!.!-!,!+!*!)!(!'!&!%!$!#(B
	       (unknown-chars-regexp
		(if skk-allow-spaces-newlines-and-tabs
		    "[ $B!!(B\n\t$B!<!7!6!5!4!3(B]"
		  "[$B!!!<!7!6!5!4!3(B]"))
	       type p)
	   (save-match-data
	     (skk-save-point
	      (backward-char 1)
	      (while (and (> (point) limit)
			  ;; unknown-chars-regexp $B$G$OJ8;z<oJL$,H=JL$G$-$J$$$N(B
			  ;; $B$G!"$=$NJ8;zNs$,B3$/8B$j%]%$%s%H$r%P%C%U%!$N@hF,(B
			  ;; $BJ}8~$XLa$9!#(B
			  (looking-at unknown-chars-regexp))
		(backward-char 1))
	      (setq type (skk-what-char-type))
	      (if (eq type 'unknown)
		  (throw 'exit1 nil)
		(skk-backward-and-set-henkan-point-1 type)
		(setq p (point))
		(if skk-allow-spaces-newlines-and-tabs
		    (while (and (> (point) limit) (bolp))
		      ;; 1 $B9T>e$N9TKv$X!#(B
		      (backward-char 1)
		      ;; $B%]%$%s%H$,H=JL$G$-$J$$J8;z<oJL$N>e$K$"$k4V$O(B
		      ;; backward $BJ}8~$X%]%$%s%H$rLa$9!#(B
		      ;;(while (and (> (point) limit)
		      ;;            (looking-at unknown-chars-regexp))
		      ;;  (backward-char 1))
		      (if;;(or
			  (> 0 (skk-backward-and-set-henkan-point-1 type))
			  ;;(eq (skk-what-char-type) type))
			  (setq p (point))))))))
	   (goto-char p)
	   (skip-chars-forward unknown-chars-regexp)))
       (skk-set-henkan-point-subr)))))

(defun skk-backward-and-set-henkan-point-1 (type)
  ;; skk-backward-and-set-henkan-point $B$N%5%V%k!<%A%s!#(BCHAR $B$N<oN`$K1~$8$?J8;z(B
  ;; $B$r%9%-%C%W$7$F%P%C%U%!$N@hF,J}8~$XLa$k!#(B
  (cond ((eq type 'hiragana)
         ;; "$B$r(B" $B$NA0$G;_$^$C$?J}$,JXMx!)(B
         (skip-chars-backward "$B!3!4!5!6!7!<$s$!(B-$B$q(B"))
        ((eq type 'katakana)
         ;; "$B%r(B" $B$NA0$G;_$^$C$?J}$,JXMx!)(B
         (skip-chars-backward "$B!3!4!5!6!7!<%s%!(B-$B%q(B"))
        ((eq type 'jisx0208-latin)
         (skip-chars-backward "$B!!(B-$B#z(B"))
        ((eq type 'ascii)
         (skip-chars-backward " -~"))))

(defun skk-what-char-type ()
  ;; $B8=:_$N%]%$%s%H$K$"$kJ8;z$,$I$s$J<oN`$+$rH=JL$9$k!#(B
  (save-match-data
    (cond ((looking-at "[$B$!(B-$B$s(B]") 'hiragana)
          ((looking-at "[$B%!(B-$B%s(B]") 'katakana)
          ;; "$B!<(B" $B$r=|30$7$F$$$k(B ("$B!<(B" $B$O(B "$B!;(B" $B$H(B "$B!=(B" $B$N4V$KF~$C$F$$$k(B)$B!#(B
          ((looking-at "[$B!!(B-$B!;!=(B-$B#z(B]") 'jisx0208-latin)
          ((looking-at "[ -~]") 'ascii)
          (t 'unknown))))

(defun skk-set-henkan-point-subr (&optional arg)
  "$B$+$J$rF~NO$7$?8e$G!"%]%$%s%H$KJQ493+;O$N%^!<%/(B \($B"&(B\) $B$rIU$1$k!#(B
$B85!9$O$3$N4X?t$O(B skk-set-henkan-point $B$NFbIt4X?t$G$"$k!#(B"
  (interactive "*P")
  (skk-with-point-move
   (cancel-undo-boundary)
   (if skk-henkan-on (skk-kakutei)
     (skk-kana-cleanup));; XXX
   (if (not (skk-get-prefix skk-current-rule-tree))
       (insert-and-inherit "$B"&(B")
     (skk-erase-prefix)
     (insert-and-inherit "$B"&(B")
     (skk-set-marker skk-kana-start-point (point))
     (skk-insert-prefix))
   (setq skk-henkan-on t)
   (skk-set-marker skk-henkan-start-point (point))))

(defun skk-change-marker ()
  ;; "$B"&(B"$B$r(B"$B"'(B"$B$KJQ$($k!#(Bskk-henkan-active $B%U%i%0$r(B t $B$K$9$k!#(B
  (skk-save-point
   (goto-char (- skk-henkan-start-point skk-kanji-len))
   (if (looking-at "$B"&(B")
       (progn
	 (cancel-undo-boundary)
	 (let ((buffer-undo-list t))
	     (insert-and-inherit "$B"'(B")
	     (delete-char 1))
	 (setq skk-henkan-active t))
     (skk-kakutei)
     (skk-error "$B"&$,$"$j$^$;$s(B" "It seems that you have deleted $B"&(B"))))

(defun skk-change-marker-to-white ()
  ;; "$B"'(B"$B$r(B"$B"&(B"$B$KJQ$($k!#(Bskk-henkan-active $B%U%i%0$r(B nil $B$K$9$k!#(B
  (skk-save-point
   (goto-char (- skk-henkan-start-point skk-kanji-len))
   (cancel-undo-boundary)
   (if (looking-at "$B"'(B")
       (let ((buffer-undo-list t))
	 (insert-and-inherit "$B"&(B")
	 (delete-char 1))
     (goto-char skk-henkan-start-point)
     (insert-and-inherit "$B"&(B")
     (skk-set-marker skk-henkan-start-point (point))
     (skk-message "$B"'$,$"$j$^$;$s(B" "It seems that you have deleted $B"'(B"))
   (setq skk-henkan-active nil)))

(defun skk-delete-henkan-markers (&optional nomesg)
  ;; $BJQ49;~$K%+%l%s%H%P%C%U%!$KI=$o$l$k(B `$B"&(B', `$B"'(B' $B%^!<%/$r>C$9!#(B
  (if (not (marker-position skk-henkan-start-point))
      nil
    (save-match-data
      (skk-save-point
       (goto-char (- skk-henkan-start-point skk-kanji-len))
       (if skk-henkan-active
	   (progn
	     (and skk-use-face (skk-henkan-face-off))
	     (if (looking-at "$B"'(B")
		 (delete-char 1)
	       (or nomesg
		   (skk-message "$B"'$,$"$j$^$;$s(B"
				"It seems that you have deleted $B"'(B"))))
	 (if (looking-at "$B"&(B")
	     (delete-char 1)
	   (or nomesg
	       (skk-message "$B"&$,$"$j$^$;$s(B"
			    "It seems that you have deleted $B"&(B"))))))))

(defun skk-delete-okuri-mark ()
  ;; $BAw$j2>L>F~NOCf$K%+%l%s%H%P%C%U%!$KI=$o$l$k(B `*' $B%^!<%/$r>C$7!"Aw$j2>L>4XO"(B
  ;; $B%U%i%0$r(B nil $B$K%;%C%H$9$k!#(B
  (if (or (not skk-okurigana)
	  (not skk-okurigana-start-point)
	  (not (markerp skk-okurigana-start-point))
	  (not (marker-position skk-okurigana-start-point)))
      nil
    (skk-save-point
      (and (eq (char-after skk-okurigana-start-point) ?*) ; ?*
	   (delete-region skk-okurigana-start-point
			  (1+ skk-okurigana-start-point)))
      (setq skk-okurigana nil
            skk-okuri-char nil
            skk-henkan-okurigana nil))))

;;;; jisyo related functions
(defun skk-purge-from-jisyo (&optional arg)
  "$B"'%b!<%I$G8=:_$N8uJd$r<-=q%P%C%U%!$+$i>C5n$9$k!#(B"
  (interactive "*P")
  (skk-with-point-move
   (if (and skk-henkan-active (not (string= skk-henkan-key "")))
       (if (not
	    (yes-or-no-p (format
			  (if skk-japanese-message-and-error
			      "%s /%s/%s$B$r<-=q$+$i:o=|$7$^$9!#NI$$$G$9$+!)(B"
			    "Really purge \"%s /%s/%s\"?")
			  skk-henkan-key (skk-get-current-candidate)
			  (if (and skk-henkan-okurigana
				   (or skk-henkan-okuri-strictly
				       skk-henkan-strict-okuri-precedence))
			      (concat
			       (if skk-japanese-message-and-error
				   " ($BAw$j2>L>(B: "
				 "(okurigana: ")
			       skk-henkan-okurigana
			       ") ")
			    " "))))
	   nil
	 ;; skk-henkan-start-point $B$+$i(B point $B$^$G:o=|$7$F$7$^$C$F$b!"JQ49D>8e(B
	 ;; $B$K(B ($B%+!<%=%k$rF0$+$9$3$H$J$/(B) skk-purge-from-jisyo $B$r8F$Y$PLdBj$J$$(B
	 ;; $B$,!"%+!<%=%k$,0c$&>l=j$X0\F0$7$F$$$?>l9g$O!":o=|$9$Y$-$G$J$$$b$N$^(B
	 ;; $B$G:o=|$7$F$7$^$&2DG=@-$,$"$k!#$=$3$G!"Aw$j2>L>$,$"$l$P$=$ND9$5$r4^(B
	 ;; $B$a$?(B end $B$r5a$a!":#2s$NJQ49$K4XO"$7$?8D=j$@$1$r@53N$K@Z$j<h$k$h$&$K(B
	 ;; $B$9$k!#(B
	 (let ((end (if skk-henkan-okurigana (+ (length skk-henkan-okurigana)
						skk-henkan-end-point)
		      skk-henkan-end-point))
	       (word (skk-get-current-candidate)))
	   (skk-update-jisyo word 'purge)
	   ;; Emacs 19.28 $B$@$H(B Overlay $B$r>C$7$F$*$+$J$$$H!"<!$K(B insert $B$5$l$k(B
	   ;; skk-henkan-key $B$K2?8N$+(B Overlay $B$,$+$+$C$F$7$^$&!#(B
	   (and skk-use-face (skk-henkan-face-off))
	   (delete-region skk-henkan-start-point end)
	   (skk-change-marker-to-white)
	   (skk-kakutei))))))

(defun skk-save-jisyo (&optional quiet)
  "SKK $B$N<-=q%P%C%U%!$r%;!<%V$9$k!#(B
  $B%*%W%7%g%J%k0z?t$N(B QUIET $B$,(B non-nil $B$G$"$l$P!"<-=q%;!<%V;~$N%a%C%;!<%8$r=P$5$J(B
  $B$$!#(B"
  (interactive "P")
  ;; skk.el $B0J30$GDs6!$5$l$k<-=q%;!<%V5!G=$rMxMQ$G$-$k$h$&$K4X?t$r(B funcall $B$9$k(B
  ;; $B7A$K$7$F$*$/!#(B
  (funcall skk-save-jisyo-function quiet))

(defun skk-save-jisyo-original (&optional quiet)
  ;;"SKK $B$N<-=q%P%C%U%!$r%;!<%V$9$k!#(B
  ;;$B%*%W%7%g%J%k0z?t$N(B QUIET $B$,(B non-nil $B$G$"$l$P!"<-=q%;!<%V;~$N%a%C%;!<%8$r=P$5$J(B
  ;;$B$$!#(B"
  (let ((skk-jisyo
	 (if (and (skk-local-variable-p 'skk-jisyo (current-buffer))
		  (equal skk-jisyo "~/skk-tut-jisyo"))
	     (default-value 'skk-jisyo)
	   skk-jisyo)))
    (let ((jisyo-buffer (skk-get-jisyo-buffer skk-jisyo 'nomsg)))
      (if (or (not jisyo-buffer) (not (buffer-modified-p jisyo-buffer)))
	  (if (not quiet)
	      (progn
		(skk-message "SKK $B<-=q$rJ]B8$9$kI,MW$O$"$j$^$;$s(B"
			     "No need to save SKK jisyo")
		(sit-for 1)))
	(with-current-buffer jisyo-buffer
	  (if (and skk-share-private-jisyo
		   (file-exists-p skk-emacs-id-file)
		   ;; $B8D?M<-=q$,B>$N(B emacs $B>e$N(B skk $B$K$h$j99?7$5$l$?$+$r%A%'%C%/(B
		   (with-temp-buffer
		     (insert-file-contents skk-emacs-id-file)
		     (goto-char (point-min))
		     (not (search-forward skk-emacs-id nil t))))
	      (progn
		(lock-buffer skk-jisyo)
		;; $B8=:_$N(B jisyo-buffer $B$NFbMF$r>C5n$7$F!"B>$N(B emacs $B>e$N(B skk $B$,(B
		;; $B99?7$7$?(B skk-jisyo $B$rFI$_9~$`!#(B
		(erase-buffer)
		(insert-file-contents skk-jisyo)
		(skk-setup-jisyo-buffer)
		;; skk-jisyo-update-vector $B$K$7$?$,$C$F%P%C%U%!$r99?7$9$k!#(B
		(let ((index 0) list skk-henkan-key)
		  (while (and (< index skk-jisyo-save-count)
			      (setq list (aref skk-jisyo-update-vector index)))
		    ;; skk-update-jisyo-1, skk-search-jisyo-file-1
		    ;; $B$G;2>H$5$l$k(B skk-henkan-key $B$r%;%C%H$9$k(B
		    (setq skk-henkan-key (car list))
		    (skk-update-jisyo-1
		     ;; okurigana    word
		     (nth 1 list) (nth 2 list)
		     (skk-search-jisyo-file-1 (nth 1 list) 0 'delete)
		     ;; purge
		     (nth 3 list))
		    (setq index (1+ index))))))
	  (let ((inhibit-quit t)
		(tempo-file (skk-make-temp-jisyo)))
	    (if (not quiet)
		(skk-message "SKK $B<-=q$rJ]B8$7$F$$$^$9(B..."
			     "Saving SKK jisyo..."))
	    (skk-save-jisyo-1 tempo-file)
	    (skk-check-size-and-do-save-jisyo tempo-file)
	    ;; $B<-=q$N%;!<%V$K@.8y$7$F=i$a$F(B modified $B%U%i%C%0$r(B nil $B$K$9$k!#(B
	    (set-buffer-modified-p nil)
	    (setq skk-update-jisyo-count 0)
	    (if (not quiet)
		(progn
		  (skk-message "SKK $B<-=q$rJ]B8$7$F$$$^$9(B...$B40N;!*(B"
			       "Saving SKK jisyo...done")
		  (sit-for 1))))
	  (if skk-share-private-jisyo
	      (with-temp-buffer
		(fillarray skk-jisyo-update-vector nil)
		(insert skk-emacs-id "\n")
		(write-region 1 (point-max) skk-emacs-id-file nil 'nomsg)
		(unlock-buffer))))))))

(defun skk-save-jisyo-1 (file)
  (save-match-data
    (let (buffer-read-only)
      (goto-char (point-min))
      (if (re-search-forward "^;; okuri-ari entries.$" nil 'noerror)
          nil
        (skk-error
         "$BAw$j$"$j%(%s%H%j$N%X%C%@!<$,$"$j$^$;$s!*(B SKK $B<-=q$N%;!<%V$rCf;_$7$^$9(B"
         "Header line for okuri-ari entries is missing!  Stop saving SKK jisyo"))
      ;; $B$*$C!"%3%a%s%H%U%'%$%9$,(B $ $B$G=*$o$i$J$$$>(B > hilit19.el
      (if (re-search-forward "^;; okuri-nasi entries.$" nil 'noerror)
          nil
        (skk-error
         "$BAw$j$J$7%(%s%H%j$N%X%C%@!<$,$"$j$^$;$s(B $B!*(B SKK $B<-=q$N%;!<%V$rCf;_$7$^$9(B"
         "Header line for okuri-nasi entries is missing!  Stop saving SKK jisyo")))
    (write-region-as-coding-system
     (skk-find-coding-system skk-jisyo-code)
     1 (point-max) file nil 'nomsg)))

(defun skk-check-size-and-do-save-jisyo (new-file)
  (let ((new-size (nth 7 (file-attributes new-file)))
        old-size)
    (if (= new-size 0)
        (progn
          (delete-file new-file)
          (skk-error "SKK $B<-=q$,6u$K$J$C$F$$$^$9!*(B $B<-=q$N%;!<%V$rCf;_$7$^$9(B"
                     "Null SKK jisyo!  Stop saving jisyo")))
    (if (or (not skk-compare-jisyo-size-when-saving)
            ;; $B5l<-=q$H$N%5%$%:Hf3S$r9T$J$o$J$$!#(B
            (progn
              ;; (1)skk-jisyo $B$,$J$$$+!"(B
              ;; (2)new-file $B$H(B skk-jisyo $B$,F10l$N%5%$%:$+(B
              ;;    (skk-(aux-)large-jisyo $B$+$i?75,$NC18l$rFI$_9~$^$J$+$C$?$j!"(B
              ;;    $B?75,C18l$NEPO?$r9T$J$o$J$+$C$?>l9g$O%5%$%:$,F1$8(B)$B!"(B
              ;; (3)new-file $B$NJ}$,Bg$-$$(B
              ;; $B>l9g(B ($B>e5-$N(B 3 $BDL$j$G$"$l$P$$$:$l$b@5>o(B)$B!#(B
              (setq old-size (nth 7 (file-attributes skk-jisyo)))
              (or (not old-size)
                  (>= new-size old-size))))
        (skk-make-new-jisyo new-file)
      ;; yes-or-no-p $B$K2sEz$7!"(Bnewline $B$9$k$H!"(Bthis-command $B$,JQ$C$F$7$^$&!#(B
      (let (this-command this-command-char last-command last-command-char)
        (if (skk-yes-or-no-p
             (format
              "skk-jisyo $B$,(B %dbytes $B>.$5$/$J$j$^$9$,!"%;!<%V$7$FNI$$$G$9$+!)(B"
              (- old-size new-size))
             (format
              "New %s will be %dbytes smaller.  Save anyway?"
              skk-jisyo (- old-size new-size)))
            ;; $B$H$K$+$/%;!<%V!#(B
            (skk-make-new-jisyo new-file)
          ;; $B%;!<%V$H$j;_$a!#(B
          (delete-file new-file)
          (with-output-to-temp-buffer "*SKK warning*"
            (if skk-japanese-message-and-error
                (progn
                  (princ "$B%;!<%V$7$h$&$H$9$k<-=q$N%5%$%:$,85$N$b$N$h$j$b>.$5$J$C$F$7$^$&$N$G!"(B")
                  (terpri)
                  (princ "$B%;!<%V$rESCf$GCf;_$7$^$7$?!#<-=q$N%5%$%:$,>.$5$/$J$C$?860x$K$ONc$((B")
                  (terpri)
                  (princ "$B$P!"(B")
                  (terpri)
                  (terpri)
                  (princ "    $B!&(BM-x skk-purge-from-jisyo $B$r<B9T$7$?!#(B")
                  (terpri)
                  (terpri)
                  (princ "    $B!&(B.skk-jisyo $B$N4A;z%3!<%I$H!"(B\" *.skk-jisyo*\" $B%P%C%U%!$N4A;z%3!<%I(B")
                  (terpri)
                  (princ "      $B$,0[$J$C$F$$$k!#(B")
                  (terpri)
                  (terpri)
                  (princ "    $B!&(B\" *.skk-jisyo*\" $B%P%C%U%!$r<+J,$GJT=8$7$?!#(B")
                  (terpri)
                  (terpri)
                  (princ "$B$J$I$,9M$($i$l$^$9(B ($B:G=i$N(B 2 $B$D$,860x$G$"$l$P!"0[>o$G$O$"$j$^$;$s!#(B")
                  (terpri)
                  (princ "$B:G8e$N>l9g$O!"$"$J$?$,$I$N$h$&$JJT=8$r$7$?$+$K$h$j$^$9(B)$B!#860x$r3NG'(B")
                  (terpri)
                  (princ "$B8e!"?5=E$K<-=q$N%;!<%V$r9T$J$&$3$H$r$*4+$a$7$^$9!#(B")
                  (terpri)
                  (terpri)
                  (princ "$B85$N<-=q$r:FEYFI$_9~$`$K$O!"(B")
                  (terpri)
                  (terpri)
                  (princ "    M-x skk-reread-private-jisyo")
                  (terpri)
                  (terpri)
                  (princ "$B$r<B9T$7$F2<$5$$!#(B"))
              (princ "As size of your private JISYO to be saved is smaller than the")
              (terpri)
              (princ "original, we have stopped saving JISYO.  For example, the following")
              (terpri)
              (princ "condition makes a smaller private JISYO;")
              (terpri)
              (terpri)
              (princ "    (a)You executed M-x skk-purge-from-jisyo,")
              (terpri)
              (terpri)
              (princ "    (b)Kanji code of .skk-jisyo is different from the one of")
              (terpri)
              (princ "       \" *.skk-jisyo*\" buffer, or")
              (terpri)
              (terpri)
              (princ "    (c)You edited \" *.skk-jisyo*\" buffer manually.")
              (terpri)
              (terpri)
              (princ "The first two conditions are not strange, but the last one depends on")
              (terpri)
              (princ "how you edited JISYO.  We strongly recommend to save JISYO")
              (terpri)
              (princ "carefully after checking what causes this.")
              (terpri)
              (princ "If you want to reread your original private JISYO, type")
              (terpri)
              (terpri)
              (princ "    M-x skk-reread-private-jisyo")
              (terpri)))
          (skk-error "SKK $B<-=q$N%;!<%V$rCf;_$7$^$7$?!*(B"
                     "Stop saving SKK jisyo!"))))))

(defun skk-make-temp-jisyo ()
  ;; SKK $B8D?M<-=qJ]B8$N$?$a$N:n6HMQ$N%U%!%$%k$r:n$j!"%U%!%$%k$N%b!<%I$r(B
  ;; skk-jisyo $B$N$b$N$HF1$8$K@_Dj$9$k!#:n$C$?:n6HMQ%U%!%$%k$NL>A0$rJV$9!#(B
  (let ((tempo-name
	 (skk-make-temp-file (if (featurep 'skk-dos)
				 "sk"
			       "skkdic"))))
    (skk-create-file tempo-name)
    ;; temporary file $B$K(B remote file $B$r;XDj$9$k$3$H$J$IM-$jF@$J$$!)(B
    ;;(if (or
    ;;     ;; XEmacs has efs.el
    ;;     (eq skk-emacs-type 'xemacs)
    ;;     ;; ange-ftp.el does not have a wrapper to set-file-modes.
    ;;     (not (and (featurep 'ange-ftp) (boundp 'ange-ftp-name-format)
    ;;               (string-match (car ange-ftp-name-format) tempo-name))))
    (set-file-modes tempo-name (file-modes skk-jisyo))
    ;;)
    tempo-name))

(defun skk-make-temp-file (prefix)
  (let ((dir
	 (cond ((skk-file-exists-and-writable-p temporary-file-directory)
		temporary-file-directory)
	       (t (or (file-exists-p "~/tmp") (make-directory "~/tmp"))
		  (or (file-writable-p "~/tmp") (set-file-modes "~/tmp" 1023))
		  "~/tmp/"))))
    (make-temp-name (expand-file-name prefix (expand-file-name dir)))))

(defun skk-make-new-jisyo (tempo-file)
  ;; TEMPO-FILE $B$r?75,$N(B skk-jisyo $B$K$9$k!#(Bskk-backup-jisyo $B$,(B non-nil $B$@$C$?(B
  ;; $B$i%P%C%/%"%C%W<-=q$r:n$k!#(B
  (let ((skk-jisyo
	 (if (and (skk-local-variable-p 'skk-jisyo (current-buffer))
		  (equal skk-jisyo "~/skk-tut-jisyo"))
	     (default-value 'skk-jisyo)
	   skk-jisyo)))
    (if skk-backup-jisyo
	(progn
	  (if (file-exists-p skk-backup-jisyo)
	      (delete-file skk-backup-jisyo))
	  (rename-file skk-jisyo skk-backup-jisyo))
      (delete-file skk-jisyo))
    (rename-file tempo-file skk-jisyo 'ok-if-already-exists)))

(defun skk-reread-private-jisyo (&optional force)
  "$B%P%C%U%!$KFI$_9~$s$@8D?M<-=q$rGK4~$7!"%U%!%$%k$+$i%P%C%U%!$X:FFI$_9~$_$9$k!#(B
$B%*%W%7%g%J%k0z?t$N(B FORCE $B$,(B non-nil $B$G$"$l$P!"GK4~$N3NG'$r$7$J$$!#(B"
  (interactive "P")
  (let ((buf (skk-get-jisyo-buffer skk-jisyo 'nomsg)))
    (if (and buf
             (or force
                 (skk-yes-or-no-p "$BJT=8Cf$N8D?M<-=q$rGK4~$7$^$9$+!)(B"
                                  "Discard your editing private JISYO?")))
        (progn
          (with-current-buffer buf
            (set-buffer-modified-p nil)
            (kill-buffer buf))
          (or
           (skk-get-jisyo-buffer skk-jisyo 'nomsg)
           (skk-error "$B8D?M<-=q$r:FFI$_9~$_$9$k$3$H$,$G$-$^$;$s!*(B"
                      "Cannot reread private JISYO!"))))))

(defun skk-record-jisyo-data ()
  "$B<-=q%G!<%?$r(B skk-record-file $B$K%;!<%V$9$k!#(B"
  (if (or (not skk-keep-record) (> 1 skk-kakutei-count))
      nil
    (with-temp-file skk-record-file
      (insert-file-contents skk-record-file)
      (goto-char (point-min))
      (insert
       (format
        "%s  $BEPO?(B: %3d  $B3NDj(B: %4d  $B3NDjN((B: %3d%%  $B8l?t(B:%6d\n"
        (current-time-string)
        skk-touroku-count skk-kakutei-count
        (/ (* 100 (- skk-kakutei-count skk-touroku-count))
           skk-kakutei-count)
        (cond ((featurep 'skk-rdbms)
	       ;; RDBMS $B$r;H$($P$b$C$H6=L#?<$$E}7W$,<h$l$k$+$b$7$l$J$$(B
	       ;; $B$,!"$H$j$"$($:8l?t$@$1?t$($FF~$l$F$*$/!#(B
	       (skk-rdbms-count-jisyo-candidates skk-rdbms-private-jisyo-table))
	      (skk-count-private-jisyo-candidates-exactly
	       (skk-count-jisyo-candidates (expand-file-name (if (consp skk-jisyo) (car skk-jisyo) skk-jisyo))))
	       ;; 1 $B9T(B 1 $B8uJd$H$_$J$9!#(B
	      (t (with-current-buffer (skk-get-jisyo-buffer skk-jisyo 'nomsg)
		   (- (count-lines (point-min) (point-max)) 2))))))
      (if (integerp skk-keep-record)
          (progn
            (setq selective-display nil)
            (widen)
            (goto-char (point-min))
            (forward-line skk-keep-record)
            (delete-region (point) (point-max)))))
    (setq skk-touroku-count 0 skk-kakutei-count 0)))

(defun skk-count-jisyo-candidates (file-or-table)
  "SKK $B<-=q$N8uJd?t$r?t$($k!#(B"
  (interactive
   (list (cond ((eq skk-count-jisyo-candidates-function
		    'skk-count-jisyo-candidates-original)
		(read-file-name
		 (format "Jisyo file: (default: %s) " skk-jisyo)
		 default-directory skk-jisyo 'confirm))
	       ((eq skk-count-jisyo-candidates-function
		    'skk-rdbms-count-jisyo-candidates)
		;; $B%G!<%?%Y!<%9%U%!%$%k$rD>@\%U%!%$%kL>$G;XDj$G$-$k(B
		;; permission $B$,$J$$>l9g$,B?$$$h$M(B...$B!#(B
		;;(read-file-name
		;; (format "Jisyo table: (default: %s) "
		;;	 skk-rdbms-private-jisyo-table))
		skk-rdbms-private-jisyo-table))))
  ;; mule@emacs19.31 $B$@$H2<5-$N$h$&$K$9$k$H(B (`$B%!(B' $B$,860x$N$h$&(B) $B2?8N$+(B
  ;; default-directory $B$NKvHx$K2~9T$,IU$/!#(B
  ;; $BDL>o$O5$$,IU$+$J$$$,!"(Brsz-mini.el $B$r;H$C$F(B resize-minibuffer-mode $B$r(B
  ;; non-nil $B$K$7$F$$$k$HITMW$J(B 2 $B9TL\$,=P8=$9$k!#(B
  ;; (interactive "f$B<-=q%U%!%$%k(B: ")
  (let ((count (funcall skk-count-jisyo-candidates-function file-or-table)))
    (if (interactive-p)
	(message (if (= count 1) "%d candidate" "%d candidates") count)
      count)))

(defun skk-count-jisyo-candidates-original (file)
  ;;"SKK $B<-=q$N8uJd?t$r?t$($k!#(B
  ;;`[' $B$H(B `]' $B$K0O$^$l$?Aw$j2>L>Kh$N%V%m%C%/Fb$O?t$($J$$!#(B"
  (with-current-buffer (find-file-noselect file)
    (save-match-data
      (let ((count 0)
            (min (point-min))
            (max (and (interactive-p) (point-max)))
            (interactive-p (interactive-p)))
        (goto-char min)
        (if (or
             ;; $B$3$A$i$O(B skk-save-point $B$r;H$o$:!"%]%$%s%H$r0\F0$5$;$k!#(B
             (not (re-search-forward "^;; okuri-ari entries.$" nil t nil))
             (not
              (skk-save-point
                (re-search-forward "^;; okuri-nasi entries.$" nil t nil))))
            (skk-error "$B$3$N%U%!%$%k$O(B SKK $B<-=q$G$O$"$j$^$;$s(B"
                       "This file is not a SKK dictionary"))
	(beginning-of-line)
	(while (looking-at ";")
	  (forward-line 1)
	  (beginning-of-line))
	(search-forward " " nil t)
        (while (search-forward "/" nil t)
          (cond ((or (eolp) (looking-at "\\["))
                 (forward-line 1)
                 (beginning-of-line)
		 (while (looking-at ";")
		   (forward-line 1)
		   (beginning-of-line))
		 (search-forward " " nil t))
                (t
                 (setq count (1+ count))))
          (if interactive-p
              (message "Counting jisyo candidates...%3d%% done"
                       (/ (* 100 (- (point) min)) max))))
	count))))

(defun skk-create-file (file &optional japanese english)
  ;; FILE $B$,$J$1$l$P!"(BFILE $B$H$$$&L>A0$N6u%U%!%$%k$r:n$k!#(B
  ;; $B%*%W%7%g%J%k0z?t$N(B JAPANESE/ENGLISH $B$r;XDj$9$k$H!"%U%!%$%k:n@.8e$=$N%a%C%;(B
  ;; $B!<%8$r%_%K%P%C%U%!$KI=<($9$k!#(B
  (let ((file (expand-file-name file)))
    (or (file-exists-p file)
	(progn
	  (write-region 1 1 file nil 0)
	  (if (or japanese english)
	      (progn
		(message (if skk-japanese-message-and-error
			     japanese english))
		(sit-for 3)))))))

(defun skk-get-jisyo-buffer (file &optional nomsg)
  ;; FILE $B$r3+$$$F(B SKK $B<-=q%P%C%U%!$r:n$j!"%P%C%U%!$rJV$9!#(B
  ;; $B%*%W%7%g%J%k0z?t$N(B NOMSG $B$r;XDj$9$k$H%U%!%$%kFI$_9~$_$N:]$N%a%C%;!<%8$r(B
  ;; $BI=<($7$J$$!#(B
  (if file
      (let* ((inhibit-quit t)
	     (code (skk-find-coding-system (if (consp file) (cdr file) skk-jisyo-code)))
	     (file (if (consp file) (car file) file))
             (jisyo-buf (concat " *" (file-name-nondirectory file)
                               "*")))
        ;; $B<-=q%P%C%U%!$H$7$F%*!<%W%s$5$l$F$$$k$J$i!"2?$b$7$J$$!#(B
        (or (get-buffer jisyo-buf)
            (with-current-buffer (setq jisyo-buf (get-buffer-create jisyo-buf))
	      (setq file (expand-file-name file))
              (buffer-disable-undo jisyo-buf)
              (auto-save-mode -1)
              ;; $B%o!<%-%s%0%P%C%U%!$N%b!<%I%i%$%s$O%"%C%W%G!<%H$5$l$J$$!)(B
              ;;(make-local-variable 'line-number-mode)
              ;;(make-local-variable 'column-number-mode)
              ;;(setq column-number-mode nil
              ;;      line-number-mode nil)
              (setq buffer-read-only nil
                    case-fold-search nil
                    ;; buffer-file-name $B$r(B nil $B$K$7$F$*$/$H(B M-x compile $B$J$I(B
		    ;; $BFbIt$G(B save-some-buffers $B$r%3!<%k$7$F$$$k%3%^%s%I$r(B
		    ;; $B;H$C$?$H$-$G$b%;!<%V$9$k$+$I$&$+$r?R$M$F$3$J$/$J$k!#(B
                    ;; buffer-file-name file
                    ;; cache-long-line-scans nil
                    ;; dabbrev $B$N%5!<%A$H$J$k%P%C%U%!$K$J$i$J$$$h$&$KB8:_$7$J(B
                    ;; $B$$%b!<%IL>$K$7$F$*$/!#<B32$N$"$kI{:nMQ$O$J$$$O$:!#(B
                    major-mode 'skk-jisyo-mode
                    mode-name "SKK dic")
              (or nomsg
                  (skk-message "SKK $B<-=q(B %s $B$r%P%C%U%!$KFI$_9~$s$G$$$^$9(B..."
                               "Inserting contents of %s ..."
                               (file-name-nondirectory file)))
	      (let (enable-character-translation enable-character-unification)
		(insert-file-contents-as-coding-system code file))
              (or nomsg
                  (skk-message
                   "SKK $B<-=q(B %s $B$r%P%C%U%!$KFI$_9~$s$G$$$^$9(B...$B40N;!*(B"
                   "Inserting contents of %s ...done"
                   (file-name-nondirectory file)))
              (skk-setup-jisyo-buffer)
              (set-buffer-modified-p nil)
              jisyo-buf)))))

(defun skk-setup-jisyo-buffer ()
  ;; skk-jisyo $B$N<-=q%P%C%U%!$G!"(B
  ;; (1)$B6u%P%C%U%!$G$"$l$P!"?7$7$/%X%C%@!<$r:n$j!"(B
  ;; (2)$B<-=q%(%s%H%j$,$"$k4{B8$N<-=q%P%C%U%!$J$i$P!"%X%C%@!<$,@5$7$$$+$I$&$+$r(B
  ;;    $B%A%'%C%/$9$k!#(B
  ;;
  ;; skk-okuri-ari-min $B$H(B skk-okuri-nasi-min $B$N0LCV$rJQ99$7$?!#(B
  ;;                       $B"-(B $B?7$7$$(B skk-okuri-ari-min
  ;;   ;; okuri-ari entries.
  ;;   $B"+(B $B0JA0$N(B skk-okuri-ari-min
  ;;
  ;;   $B"-(B skk-okuri-ari-max $B"-(B $B?7$7$$(B skk-okuri-nasi-min
  ;;   ;; okuri-nasi entries.
  ;;   $B"+(B $B0JA0$N(B skk-okuri-nasi-min
  ;;
  ;;
  ;; $BJQ99A0$N0LCV$G$"$l$P!"2<5-$N$h$&$J6u<-=q$N>l9g!"(B
  ;;
  ;;   ;; okuri-ari entries.
  ;;   ;; okuri-nasi entries.
  ;;
  ;; skk-okuri-ari-min $B$H(B skk-okuri-ari-max $B$N%^!<%+!<$,=E$J$C$F$7$^$$!"(B
  ;; skk-okuri-ari-min $B$N0LCV$KA^F~$7$?%(%s%H%j$,(B skk-okuri-ari-max $B$N%^!<%+!<(B
  ;; $B$r8eJ}$K2!$7$d$i$J$$!#(B
  ;;
  ;; $B$3$N4X?t$N%*%j%8%J%k$NL>>N$O!"(Bj-check-jisyo $B$@$C$?$,!"(Bskk-check-jisyo $B$H(B
  ;; $B$$$&L>A0$K$9$k$H(B skk-tools.el $BFb$N4X?tL>$H=EJ#$9$k!#(B
  ;; case-fold-search $B$O!"<-=q%P%C%U%!$G$O>o$K(B nil$B!#(B
  (save-match-data
    (if (= (buffer-size) 0)
	;; $B6u%P%C%U%!$@$C$?$i!"%X%C%@!<$N$_A^F~!#(B
	(insert ";; okuri-ari entries.\n" ";; okuri-nasi entries.\n"))
    (goto-char (point-min))
    (if (re-search-forward "^;; okuri-ari entries.$" nil 'noerror)
	;; $B8GDj%]%$%s%H$J$N$G!"(B(point) $B$G==J,!#(B
	(setq skk-okuri-ari-min (point))
      (skk-error "$BAw$j$"$j%(%s%H%j$N%X%C%@!<$,$"$j$^$;$s!*(B"
		 "Header line for okuri-ari entries is missing!"))
    (if (re-search-forward "^;; okuri-nasi entries.$" nil 'noerror)
	(progn
	  (beginning-of-line)
	  ;; $B6&M-<-=q$J$i8GDj%]%$%s%H$G$bNI$$$N$@$,!"<-=q%P%C%U%!$GJT=8$r9T(B
	  ;; $B$J$C$?$H$-$N$3$H$rG[N8$7$F%^!<%+!<$K$7$F$*$/!#(B
	  (setq skk-okuri-ari-max (point-marker))
	  (forward-line 1)
	  (backward-char 1)
	  (setq skk-okuri-nasi-min (point-marker)))
      (skk-error "$BAw$j$J$7%(%s%H%j$N%X%C%@!<$,$"$j$^$;$s!*(B"
		 "Header line for okuri-nasi entries is missing!"))))

(defun skk-search ()
  ;; skk-current-search-prog-list $B$NMWAG$K$J$C$F$$$k%W%m%0%i%`$rI>2A$7$F!"(B
  ;; skk-henkan-key$B$r%-!<$K$7$F8!:w$r9T$&!#(B
  (let (l)
    (while (and (null l) skk-current-search-prog-list)
      (setq l (eval (car skk-current-search-prog-list))
	    skk-current-search-prog-list (cdr skk-current-search-prog-list)))
    l))

(defun skk-search-jisyo-file (file limit &optional nomsg)
  ;; SKK $B<-=q%U%)!<%^%C%H$N(B FILE $B$G(B skk-henkan-key $B$r%-!<$K$7$F8!:w$r9T$&!#(B
  ;; $B8!:w%j!<%8%g%s$,(B LIMIT $B0J2<$K$J$k$^$G%P%$%J%j%5!<%A$r9T$$!"$=$N8e%j%K%"(B
  ;; $B%5!<%A$r9T$&!#(B
  ;; LIMIT $B$,(B 0 $B$G$"$l$P!"%j%K%"%5!<%A$N$_$r9T$&!#(B
  ;; $B<-=q$,%=!<%H$5$l$F$$$J$$$N$G$"$l$P!"(BLIMIT $B$r(B 0 $B$9$kI,MW$,$"$k!#(B
  ;; $B%*%W%7%g%J%k0z?t$N(B NOMSG $B$,(B non-nil $B$G$"$l$P(B skk-get-jisyo-buffer $B$N%a%C(B
  ;; $B%;!<%8$r=PNO$7$J$$$h$&$K$9$k!#(B
  (let ((jisyo-buffer (skk-get-jisyo-buffer file nomsg)))
    (if jisyo-buffer
        ;; skk-henkan-key $B$H(B skk-henkan-okurigana $B$O%+%l%s%H%P%C%U%!$N%m!<%+%k(B
        ;; $BCM!#(B
        (let ((okurigana (or skk-henkan-okurigana skk-okuri-char))
              (midasi
               (if skk-use-numeric-conversion
		   ;; skk-henkan-key $B$,(B nil $B$N$3$H$,$"$k!#2?8N(B?
                   (skk-num-compute-henkan-key skk-henkan-key)
                 skk-henkan-key))
	      (henkan-buffer (current-buffer))
              entry-list entry)
          (with-current-buffer jisyo-buffer
            (setq skk-henkan-key midasi
                  entry-list (skk-search-jisyo-file-1 okurigana limit))
            (if entry-list
                (progn
                  (setq entry
                        (cond ((and okurigana skk-henkan-okuri-strictly)
                               ;; $BAw$j2>L>$,F10l$N%(%s%H%j$N$_$rJV$9!#(B
                               (nth 2 entry-list))
                              ((and okurigana skk-henkan-strict-okuri-precedence)
                               ;; $BAw$j2>L>$,F10l$N%(%s%H%j$N$&$7$m$K!"(B
                               ;; $B$=$NB>$N%(%s%H%j$r$D$1$F$+$($9!#(B
                               (skk-nunion (nth 2 entry-list) (car entry-list)))
                              (t (car entry-list))))
		  (and skk-search-end-function
		       (setq entry (funcall skk-search-end-function
					    henkan-buffer midasi okurigana entry)))
		  entry)))))))

(defun skk-search-jisyo-file-1 (okurigana limit &optional delete)
  ;; skk-search-jisyo-file $B$N%5%V%k!<%A%s!#(Bskk-compute-henkan-lists $B$r;HMQ$7!"(B
  ;; $B8+=P$78l$K$D$$$F$N%(%s%H%j$N>pJs$rJV$9!#(B
  ;; DELETE $B$,(B non-nil $B$G$"$l$P!"(BMIDASI $B$K%^%C%A$9$k%(%s%H%j$r:o=|$9$k!#(B
  (let ((key (concat "\n" skk-henkan-key " /"))
        min max size p)
    (save-match-data
      ;; skk-okuri-ari-min $B$H(B skk-okuri-ari-max $B$O<-=q%P%C%U%!$N%m!<%+%kCM!#(B
      (if okurigana
          (setq min skk-okuri-ari-min
                max skk-okuri-ari-max)
        (setq min skk-okuri-nasi-min
              max (point-max)))
      (if (> limit 0)
          (while (progn (setq size (- max min)) (> size limit))
            (goto-char (+ min (/ size 2)))
            (beginning-of-line)
            (setq p (point))
            ;; $BAw$j$"$j$J$i5U=g$KHf3S$r9T$J$&!#(B
            (if
                (if okurigana
                    (string< (buffer-substring-no-properties
			      p (1- (search-forward  " ")))
                             skk-henkan-key)
                  (string< skk-henkan-key
                           (buffer-substring-no-properties
			    p (1- (search-forward " ")))))
                (setq max p)
              (setq min p))))
      (goto-char min)
      ;; key $B$,8!:w3+;OCOE@$K$"$C$?>l9g$G$b8!:w2DG=$J$h$&$K0lJ8;zLa$k!#(Bkey $B$,(B
      ;; $B$=$N@hF,ItJ,$K(B "\n" $B$r4^$s$G$$$k$3$H$KCm0U!#(B
      (or (bobp) (backward-char 1))
      ;; case-fold-search $B$O!"<-=q%P%C%U%!$G$O>o$K(B nil$B!#(B
      (if (search-forward key max 'noerror)
	  (prog1
	      (skk-compute-henkan-lists okurigana)
	    (if delete
		(progn
		  (beginning-of-line)
		  (delete-region (point)
				 (progn (forward-line 1) (point))))))))))


(defun skk-compute-henkan-lists (okurigana)
  ;; $B<-=q%(%s%H%j$r(B 4 $B$D$N%j%9%H$KJ,2r$9$k!#(B
  ;;
  ;; $BAw$j$J$7(B ($BNc$($P!"<-=q%(%s%H%j(B "$B$F$s$5$$(B /$BE>:\(B/$BE7:R(B/$BE7:M(B/" $B$N=hM}(B)
  ;; entry1 := ("$BE>:\(B" "$BE7:R(B" "$BE7:M(B") == $BA4%(%s%H%j(B
  ;; entry2 := nil
  ;; entry3 := nil
  ;; entry4 := nil
  ;;
  ;; $BAw$j$"$j(B ($BNc$($P!"!V5c$/!W$NJQ49$r9T$C$?>l9g$N!"<-=q%(%s%H%j(B
  ;;           "$B$J(Bk /$BK4(B/$BL5(B/$BLD(B/$B5c(B/[$B$/(B/$BL5(B/$BLD(B/$B5c(B/]/[$B$-(B/$BK4(B/]/" $B$N=hM}(B)
  ;; entry1 := ("$BK4(B" "$BL5(B" "$BLD(B" "$B5c(B")  == $B4A;zItJ,$NA4%(%s%H%j(B
  ;; entry2 := ("[$B$/(B")                == $BB>$NAw$j2>L>$r;H$&4A;z%(%s%H%j(B ($B$"$l(B
  ;;                                     $B$P(B) + $B:#2s$NJQ49$NAw$j2>L>ItJ,(B
  ;; entry3 := ("$BL5(B" "$BLD(B" "$B5c(B")       == $B:#2s$NJQ49$NAw$j2>L>$r;H$&2DG=@-$N(B
  ;;                                     $B$"$kA44A;z%(%s%H%j(B
  ;; entry4 := ("]" "[$B$-(B" "$BK4(B" "]")   == $BB>$NAw$j2>L>$r;H$&4A;z%(%s%H%j(B ($B;D(B
  ;;                                     $B$j!#$"$l$P(B)
  ;;
  ;;   * "[" $B$OD>8e$KB3$/$R$i$,$J$rAw$j2>L>$K;}$D4A;z$N%(%s%H%j$N=i$^$j$rI=$7!"(B
  ;;     "]" $B$O!"3:Ev$NAw$j2>L>%0%k!<%W$N=*$j$r<($9!#(B
  ;;
  ;; $B$3$N4X?t$O!"JQ49;~$H!"3NDjD>8e$N<-=q$N%"%C%W%G!<%H;~$N(B 2 $BEY8F$P$l$k(B
  ;; ($BJQ49;~$K8!:w$r9T$C$?<-=q$,!"(Bskk-jisyo $B$H$O8B$i$J$$$N$G!"(B2 $BEY7W;;$;$6$k(B
  ;; $B$rF@$J$$(B)$B!#(B
  ;;
  ;; $BJQ49;~$O!"(Bskk-henkan-okuri-strictly $B$,(B non-nil $B$G$"$l$P!"(B
  ;; $B7W;;7k2L$N(B entry3$B$r!"(Bskk-henkan-okuri-strictly $B$,(B nil $B$G$"$C$F(B
  ;; $B$+$D(B skk-henkan-strict-okuri-precedence $B$,(B non-nil $B$"$l$P(B
  ;; (skk-nunion entry3 entry1) $B$r<h$j=P$9!#(B
  ;; $B$U$?$D$NJQ?t$,$H$b$K(B nil $B$N>l9g$O(B entry1 $B$r<h$j=P$9!#(B
  (if (not okurigana)
      (list (split-string (buffer-substring-no-properties
			   (point) (progn (end-of-line) (1- (point))))
			  "/") nil nil nil)
    (save-match-data
      (let ((stage 1) (q1 (queue-create)) (q2 (queue-create))
            (q3 (queue-create)) (q4 (queue-create))
            (okuri-key (concat "\[" okurigana)) item headchar)
        (catch 'exit
          (while (not (eolp))
            (setq item (buffer-substring-no-properties
			(point)
			(1- (search-forward "/")))
                  headchar (if (string= item "") (int-char 0) (skk-str-ref item 0)))
            (cond ((and (eq headchar ?\[) (<= stage 2))
                   (if (string= item okuri-key)
                       (progn (queue-enqueue q2 item)
                              (setq stage 3))
                     (setq stage 2)
                     (queue-enqueue q2 item)))
                  ((= stage 1)
                   (queue-enqueue q1 item))
                  ((= stage 2)
                   (queue-enqueue q2 item))
                  ((= stage 3)
                   (if (eq headchar ?\]) ; ?\]
                       (progn (setq stage 4)
                              (queue-enqueue q4 item))
                     (queue-enqueue q3 item)))
                  ((= stage 4)
                   (queue-enqueue q4 item)))))
        ;;        entry1          entry2        entry3          entry4
        (list (queue-all q1) (queue-all q2) (queue-all q3) (queue-all q4))))))

(defun skk-nunion (x y)
  ;; X $B$H(B Y $B$NOB=89g$r:n$k!#Ey$7$$$+$I$&$+$NHf3S$O!"(Bequal $B$G9T$o$l$k!#(BX $B$K(B Y
  ;; $B$rGK2uE*$KO"@\$9$k!#(B
  (if (null x)
      y
    (if (null y)
	x
      (save-match-data
	(let ((list2 y) list1 origlist1 e1 e2)
	  (while list2
	    (setq list1 (cons nil x)
		  e2 (car list2)
		  origlist1 list1)
	    (catch 'found
	      (while (setq e1 (car (cdr list1)))
		(if (equal e1 e2)
		    (throw 'found nil)
		  (if (not (string-match ";" e1))
		      nil
		    (setq e1 (substring e1 0 (match-beginning 0)))
		    (if (or (equal e1 e2)
			    (and
			     (string-match ";" e2)
			     (equal (substring e2 0 (match-beginning 0)) e1)))
			(throw 'found nil))))
		(setq list1 (cdr list1)))
	      (setcdr list1 (list e2))
	      (setq x (cdr origlist1)))
	    (setq list2 (cdr list2)))
	  x)))))

(defun skk-search-kakutei-jisyo-file (file limit &optional nomsg)
  ;; $B<-=q%U%!%$%k$rC5$7!"8uJd$r%j%9%H$GJV$9!#(B
  ;; $B8uJd$r8+$D$1$?>l9g$O!"Bg0hJQ?t(B skk-kakutei-flag $B$K(B non-nil $B$rBeF~$9$k!#(B
  ;; $B8uJd$,8+$D$+$i$J$+$C$?>l9g$O!"(Bnil $B$rJV$9!#(B
  (setq skk-kakutei-flag (skk-search-jisyo-file file limit nomsg)))

(defun skk-update-jisyo (word &optional purge)
  (funcall skk-update-jisyo-function word purge))

(defun skk-update-jisyo-original (word &optional purge)
  ;; WORD $B$,<!$NJQ49;~$K:G=i$N8uJd$K$J$k$h$&$K!"%W%i%$%Y!<%H<-=q$r99?7$9$k!#(B
  ;; PURGE $B$,(B non-nil $B$G(B WORD $B$,6&M-<-=q$K$"$k%(%s%H%j$J$i(B skk-ignore-dic-word
  ;; $B4X?t$G%/%)!<%H$7$?%(%s%H%j$r%W%i%$%Y!<%H<-=q$K:n$j!"<!$NJQ49$+$i=PNO$7$J(B
  ;; $B$$$h$&$K$9$k!#(B
  ;; WORD $B$,6&M-<-=q$K$J$1$l$P!"%W%i%$%Y!<%H<-=q$N<-=q%(%s%H%j$+$i:o=|$9$k!#(B
  ;;
  ;; SKK 9.x $B$h$j!"%W%i%$%Y!<%H<-=q$N%(%s%H%j$NA^F~$NJ}K!$rJQ99$7$?(B (9.3 $B$N$_(B
  ;; $B$ONc30(B)$B!#(B
  ;;
  ;; $B!ZJQ99A0![(B
  ;;         ;; okuri-ari entries.
  ;;  $B8+%-(B   $B$o$k(Bk /$B0-(B/[$B$+(B/$B0-(B/]/[$B$/(B/$B0-(B/]/
  ;;  $B=P!<(B   $B$o$k(Bi /$B0-(B/[$B$$(B/$B0-(B/]/
  ;;  $B$7$K(B   $B$o$?(Bs /$BEO(B/[$B$5(B/$BEO(B/]/[$B$;(B/$BEO(B/]/
  ;;  $B8l9_(B   $B$o$9(Br /$BK:(B/[$B$l(B/$BK:(B/]/
  ;;  $B$r=g(B   $B$o$+(Bt /$BJ,(B/$BH=(B/[$B$C$?(B/$BJ,(B/$BH=(B/]/[$B$C$F(B/$BJ,(B/]/
  ;;   $B"-(B     .....
  ;;         $B$"(Bi /$B9g(B/[$B$$(B/$B9g(B/]/
  ;;         ;; okuri-nasi entries.
  ;;  $BJQ$G(B   $B$8$g$&$?$$(B /$B>uBV(B/
  ;;  $B49>:(B   $B$=$&$K$e$&(B /$BA^F~(B/
  ;;  $B=g=g(B   $B$+$J(B /$B2>L>(B/
  ;;   $B"-(B    ...
  ;;         ...
  ;;
  ;; $B!ZJQ998e![(B
  ;;         ;; okuri-ari entries.
  ;;  $BJQ$G(B   $B$G(Bt /$B=P(B/[$B$F(B/$B=P(B/]/[$B$?(B/$B=P(B/]/
  ;;  $B49>:(B   $B$D(Bi /$BIU(B/[$B$$(B/$BIU(B/]/
  ;;  $B=g=g(B   $B$1(Bs /$B>C(B/[$B$9(B/$B>C(B/]/[$B$7(B/$B>C(B/]/[$B$;(B/$B>C(B/]/[$B$5(B/$B>C(B/]/
  ;;   $B"-(B    $B$+$((Bs /$BJV(B/[$B$7(B/$BJV(B/]/[$B$9(B/$BJV(B/]/[$B$5(B/$BJV(B/]/[$B$;(B/$BJV(B/]/
  ;;         ...
  ;;         ...
  ;;         $B$J$,(Bs /$BD9(B/$BN.(B/[$B$7(B/$BN.(B/]/[$B$5(B/$BD9(B/]/[$B$=(B/$BN.(B/]/
  ;;         ;; okuri-nasi entries.
  ;;  $BJQ$G(B   $B$8$g$&$?$$(B /$B>uBV(B/
  ;;  $B49>:(B   $B$=$&$K$e$&(B /$BA^F~(B/
  ;;  $B=g=g(B   $B$+$J(B /$B2>L>(B/
  ;;   $B"-(B    ...
  ;;         ...
  ;;
  ;; skk-auto-okuri-process $B$,(B non-nil $B$N$H$-$K!"(B(j-okuri-search $B2~$a(B)
  ;; skk-okuri-search $B$O8+=P$78l$ND9$$=g$K8uJd$rJV$9I,MW$,$"$k!#(B
  ;; SKK 8.6 $B$^$G$O!"(Bskk-okuri-search $B$,(B j-okuri-ari-min $B$+$i(B j-okuri-ari-max
  ;; $B$^$G$r=g$KC5$7!"8+$D$1$?$b$N=g$K8uJd$rJV$9$?$a$K%W%i%$%Y!<%H<-=q$,8+=P$7(B
  ;; $B8l$r%-!<$H$7$F9_=g$K%=!<%H$5$l$F$$$kI,MW$,$"$C$?!#(B
  ;; SKK 9.x $B$G$O!"(Bskk-okuri-search $B$,!"8+IU$1$?8uJd$r8+=P$78l$r%-!<$H$7$F>:=g(B
  ;; $B$K%=!<%H$7$FJV$9$?$a!"%W%i%$%Y!<%H<-=q$N%=!<%H$OI,MW$G$J$$!#$h$C$F!":G8e(B
  ;; $B$KJQ49$7$?$b$N$r(B (j-okuri-ari-min $B2~$a(B) skk-okuri-ari-min $B$N0LCV$KA^F~$9(B
  ;; $B$k!#(B
  ;;
  (let ((jisyo-buffer (skk-get-jisyo-buffer skk-jisyo 'nomsg))
	(midasi
	 (if skk-use-numeric-conversion
	     (skk-num-compute-henkan-key skk-henkan-key)
	   skk-henkan-key))
	(henkan-buffer (and skk-update-end-function (current-buffer))))
    (if jisyo-buffer
	(let ((inhibit-quit t) buffer-read-only old-entry okurigana)
	  (if (> skk-okuri-index-min -1)
	      (setq word (skk-remove-common word)
		    ;; skk-henkan-key $B$O(B skk-remove-common $B$K$h$C$FJQ99$5$l$F$$(B
		    ;; $B$k2DG=@-$,$"$k!#(B
		    midasi skk-henkan-key))
	  (setq okurigana (or skk-henkan-okurigana skk-okuri-char))
	  (with-current-buffer jisyo-buffer
	    ;; $B4{B8%(%s%H%j$r8!:w8e>C5n$9$k!#A^F~$9$Y$-%(%s%H%j$,(B entry1 $B$K(B 1
	    ;; $B$D$7$+$J$/!"(Bword $B$HF1$8J8;z$G$"$C$F$b!"$$$C$?$s>C$7$F$=$N%(%s%H(B
	    ;; $B%j$r(B min $B%]%$%s%H$K0\F0$5$;$J$1$l$P$J$i$J$$(B ($BFI$_$NJd40$r9T$&$H(B
	    ;; $B$-$O!"(Bmin $B%]%$%s%H$+$i8+=P$7$rC5$9$?$a!"?7$7$$8+=P$7$[$I!"(Bmin
	    ;; $B%]%$%s%H$K6a$$$H$3$m$K$J$1$l$P$J$i$J$$(B)$B!#(B
	    (setq skk-henkan-key midasi
		  old-entry (skk-search-jisyo-file-1 okurigana 0 'delete))
	    (skk-update-jisyo-1 okurigana word old-entry purge)
	    ;; $BJ#?t$N(B emacs $B$G(B SKK $B$,5/F0$5$l$F$$$k$H$-$K8D?M<-=q$r@09gE*$K(B
	    ;; $B99?7$9$k$?$a$K3NDj$NF0:n$r5-O?$9$k!#(B
	    (if skk-share-private-jisyo
		(aset skk-jisyo-update-vector skk-update-jisyo-count
		      (list midasi okurigana word purge)))
	    (and skk-update-end-function
		 (funcall skk-update-end-function
			  henkan-buffer midasi okurigana word purge))
	    (setq skk-update-jisyo-count (1+ skk-update-jisyo-count))
	    (if (and skk-jisyo-save-count
		     (= skk-jisyo-save-count skk-update-jisyo-count))
		;; auto save.
		(skk-save-jisyo 'quiet)))))))

(defun skk-update-jisyo-1 (okurigana word old-entry-list purge)
  ;; $B4{B8%(%s%H%j$+$i7W;;$7$?(B entry[1-4] $B$NCM$H!":#2s$NJQ49$N7k2L(B word $B$H$r%^!<(B
  ;; $B%8$7$F!"?7$?$J%(%s%H%j$r7W;;$7!"A^F~$9$k!#(B
  (let ((entry1 (car old-entry-list)) (entry2 (nth 1 old-entry-list))
        (entry3 (nth 2 old-entry-list)) (entry4 (nth 3 old-entry-list)))
    (if (not purge)
        ;; entry1 $B$N@hF,$N%(%s%H%j$r(B word $B$K$9$k!#(B
        (setq entry1 (cons word (delete word entry1)))
      ;; $BAw$j$J$7!"$b$7$/$O(B skk-henkan-okuri-strictly $B$H(B
      ;; skk-henkan-strict-okuri-precedence $B$,(B nil $B$N>l9g!#(B
      (if (or (not okurigana) (not (or skk-henkan-okuri-strictly
				       skk-henkan-strict-okuri-precedence)))
          ;; entry1 $B$r(B purge$B!#6&MQ<-=q$K$"$k%(%s%H%j$@$C$?$i!"(B
          ;; skk-ignore-dic-word $B$G%/%)!<%H$7$F<!$NJQ49$+$i=PNO$7$J$$$h$&$K$9(B
          ;; $B$k!#6&MQ<-=q$K$J$$J8;zNs$O(B word $B$r>C$9!#(B
          (if (skk-public-jisyo-has-entry-p okurigana word)
              (setq entry1 (skk-compose-ignore-entry entry1 word))
            (setq entry1 (delete word entry1)))
        ;; $BAw$j$"$j$G!"$+$D(B skk-henkan-okuri-strictly $B$+(B
	;; skk-henkan-strict-okuri-precedence $B$,(B non-nil $B$N>l9g$G!"$+$D(B
        ;; $B$3$N(B word $B$H%Z%"$K$J$kAw$j2>L>$,(B okurigana $B$7$+$J$$$H$-!#(B
        (if (and okurigana (or skk-henkan-okuri-strictly
			       skk-henkan-strict-okuri-precedence)
                 (null (member word entry2)) (null (member word entry4)))
            (setq entry1 (delete word entry1))
          ;; $B$=$NB>$N>l9g$O2?$b$7$J$$!#(B
        )))
    (if (null entry1)
        ;; entry1 $B$,(B null $B$G$"$l$P!"$b$&2?$b$9$k$3$H$O$J$$!#(B
        nil
      (goto-char (if okurigana skk-okuri-ari-min skk-okuri-nasi-min))
      (insert "\n" skk-henkan-key " /")
      ;; entry1 -- $BA4%(%s%H%j(B ($BAw$j$J$7$N>l9g(B) or $B4A;zItJ,$NA4%(%s%H%j(B ($BAw$j$"(B
      ;; $B$j$N>l9g(B)
      (insert (mapconcat 'skk-quote-char entry1 "/") "/")
      (if (not okurigana)
          nil
        ;; entry2 $B0J9_$N%(%s%H%j$r=hM}$9$k$N$O!"Aw$j$"$j$N>l9g$N$_!#(B
        ;; $B@h$KA^F~$9$Y$-%(%s%H%j$r7W;;!"D4@0$9$k!#(B
        (if entry3
            (if (not purge)
                (setq entry3 (cons word (delete word entry3)))
              (setq entry3 (delete word entry3))
              (if (null entry3)
                  ;; entry3 $B$H$7$FA^F~$9$k$b$N$,A4$/$J$1$l$P!"(B"/[$B$/(B/]/" $B$N$h(B
                  ;; $B$&$JAw$j2>L>$N$_$N%(%s%H%j$r:n$i$J$$$h$&$K$9$k(B ($BI,MW$G(B
                  ;; $B$"$l$P!"(Bentry2 $B$N:G8eJ}$H(B) entry4 $B$N@hF,$N%(%s%H%j(B "]"
                  ;; $B$r:o=|!#(B
                  (let ((last2 (nthcdr (- (length entry2) 2) entry2)))
                    ;; entry2 $B$N:G8eJ}$O>o$K(B "[$BAw$j2>L>(B" $B$H$O8B$i$J$$!#(B
                    (and (string= (nth 1 last2) (concat "[" okurigana))
			 (setcdr last2 nil))
                    ;; entry4 $B$N@hF,$O>o$K(B "]"$B!#(B
                    (setq entry4 (cdr entry4)))))
          ;; entry3 $B$,(B null $B$G$"$l$P(B
          (if (or skk-process-okuri-early purge)
              ;; skk-process-okuri-early $B$,(B non-nil $B$J$iAw$j2>L>$,J,$i$J$$$N$G(B
              ;; $B2?$b$7$J$$!#(B-- $B:#2s;HMQ$7$?Aw$j2>L>$,$o$+$i$J$$$^$^JQ49$7$F$$(B
              ;; $B$k$N$G!"A4$F$N%(%s%H%j$,(B entry2 $B$KF~$C$F$$$k(B -- entry3,
              ;; entry4 $B$O(B null$B!#(B
              ;; entry3 $B$H$7$FA^F~$9$k$b$N$,A4$/$J$1$l$P!"2?$b$7$J$$(B -- entry3
              ;; $B$,(B purge $BA0$+$i(B null $B$J$i!"(Bentry2 $B$NKvHx$O(B "[" $B$G$J$$$7!"(B
              ;; entry4 $B$O(B null $B$@$+$i(B entry[234] $B$NA`:n$OITMW!#(B
              nil
            (setq entry2 (nconc entry2 (list (concat "[" okurigana)))
                  entry3 (list word)
                  ;; purge $BA0$+$i(B entry3 $B$,(B null $B$@$C$?$N$@$+$i(B entry4 $B$b(B null$B!#(B
                  entry4 (list "]")))))
      (if entry2
          ;; entry2 -- $B:#2s;HMQ$7$J$+$C$?Aw$j2>L>$r;H$&4A;z$N8uJd72(B + "[" + $B:#(B
          ;; $B2s;HMQ$7$?Aw$j2>L>(B ($BAw$j2>L>$N$_!#$=$NAw$j2>L>$r;HMQ$9$k4A;z$N8u(B
          ;; $BJd72$O!"(Bentry3 $B$K4^$^$l$k(B)$B!#(B
          (progn
            (insert (mapconcat 'skk-quote-char entry2 "/") "/")
            ;; entry2 $B$,(B null $B$J$i(B entry3 $B$b(B null$B!#(B
            (and entry3
		 ;; entry3 -- $B:#2s;HMQ$7$?Aw$j2>L>$r;H$&A44A;z%(%s%H%j(B
		 (insert (mapconcat 'skk-quote-char entry3 "/") "/"))
            ;; purge $B$G(B entry3 $B$,(B null $B$K$J$C$?>l9g$O(B entry4 $B$,;D$C$F$$$k$H$-(B
            ;; $B$,$"$k!#(B
            (and entry4
		 ;; entry4 -- "]" + $BB>$NAw$j2>L>$r;H$&A44A;z%(%s%H%j(B (entry2 $B$N(B
		 ;; $B;D$j(B)$B!#(B
		 (insert (mapconcat 'skk-quote-char entry4 "/") "/")))))))

(defun skk-quote-char (word)
  ;; $B<-=q$N@)8B$+$i<-=q%(%s%H%jFb$K4^$a$F$O$J$i$J$$J8;z$,(B WORD $B$NCf$K$"$l$P!"(B
  ;; $BI>2A$7$?$H$-$K$=$NJ8;z$H$J$k$h$&$J(B Lisp $B%3!<%I$rJV$9!#(B
  (save-match-data
    (if (and word
             (string-match "[/\n\r\"]" word)
             ;; we should not quote WORD if it is a symbolic expression
             (not (skk-lisp-prog-p word))
	     (not (string-match ";" word))) ; has an annotation
	(format "(concat \"%s\")"
                (mapconcat (function (lambda (c)
                                       (cond ((eq c ?/) "\\057")
                                             ((eq c ?\n) "\\n")
                                             ((eq c ?\r) "\\r")
                                             ((eq c ?\") "\\\"")
                                             ((eq c ?\\) "\\\\")
                                             (t (char-to-string c)))))
                           ;; $BJ8;zNs$rBP1~$9$k(B char $B$N%j%9%H$KJ,2r$9$k!#(B
                           (append word nil) ""))
      word)))

(defun skk-public-jisyo-has-entry-p (okurigana word)
  ;; $B6&M-<-=q$,(B MIDASHI $B5Z$S$=$l$KBP1~$9$k(B WORDS $B%(%s%H%j$r;}$C$F$$$l$P!"(B
  ;; non-nil $B$rJV$9!#%W%i%$%Y!<%H<-=q$N%P%C%U%!$G%3!<%k$5$l$k!#(B
  (let (fn skk-henkan-okuri-strictly skk-henkan-strict-okuri-precedence)
    (if okurigana
        (setq skk-henkan-okurigana okurigana))
    ;; skkserv $B$r;H$&@_Dj$K$J$C$F$$$?$i!"(Bskk-server.el $B$r%m!<%I$9$k!#(B
    (and (not (featurep 'skk-server))
	 (or (and (boundp 'skk-servers-list) skk-servers-list)
	     (or (and (boundp 'skk-server-host) skk-server-host)
		 (getenv "SKKSERVER")))
	 (require 'skk-server))
    (setq fn (funcall skk-public-jisyo-to-be-searched-function))
    (and fn (member word (eval fn)))))

(defun skk-public-jisyo-to-be-searched-original ()
  ;; skk-search-prog-list $B$NCf$+$i!"0lHVBg$-$J6&M-<-=q$G%5!<%A$9$k%W%m(B
  ;; $B%0%i%`$rJV$9!#(B
  (let (fn)
    (and (featurep 'skk-server) (or skk-servers-list skk-server-host)
	 (setq fn (assq 'skk-search-server skk-search-prog-list)))
    ;; skk-search-server $B$+$i;O$^$k%j%9%H$,$J$1$l$P!"$H$K$+$/Bg$-$$<-=q$r0z?t(B
    ;; $B$K$7$F$$$k(B skk-search-jisyo-file $B%W%m%0%i%`$rC5$9!#(B
    (if (and (not fn) (or skk-aux-large-jisyo skk-large-jisyo))
	(let ((spl skk-search-prog-list)
	      cell)
	  (while (setq cell (car spl))
	    (if (and (eq (car cell) 'skk-search-jisyo-file)
		     (memq (nth 1 cell) '(skk-aux-large-jisyo skk-large-jisyo)))
		(setq fn cell
		      spl nil)
	      (setq spl (cdr spl))))))
    fn))

(defun skk-compose-ignore-entry (entry &optional add)
  ;; ENTRY $B$NCf$K(B skk-ignore-dic-word $B4X?t$G%/%)!<%H$7$?%(%s%H%j$,$"$l(B
  ;; $B$P!"0l$D$N%(%s%H%j$K$^$H$a$k!#(B
  ;; $B%*%W%7%g%J%k0z?t$N(B ADD $B$,;XDj$5$l$F$$$?$i!"(BADD $B$r4^$a$?(B
  ;; skk-ignore-dic-word $B%(%s%H%j$r:n$k!#(B
  ;; $B?7$7$$(B skk-ignore-dic-word $B%(%s%H%j$r(B car $B$K!"$=$l0J30$N%(%s%H%j(B cdr $B$K$7(B
  ;; $B$?%j%9%H$rJV$9!#(B
  (let (l arg e)
    (and add (setq entry (delete add entry)))
    (setq l entry)
    (save-match-data
      (while l
        (setq e (car l)
              l (cdr l))
        (and (string-match "(skk-ignore-dic-word +\\([^\)]+\\))" e)
	     (setq arg (concat arg
			       (substring e (1+ (match-beginning 1))
					  (1- (match-end 1)))
			       "\" \"")
		   entry (delq e entry))))
      (if add
          (setq arg (if arg (concat arg add) add))
        ;; $BKvHx$N(B " \"" $B$r@Z$jMn$H$9!#(B
        (setq arg (substring arg 0 -2)))
      (cons (concat "(skk-ignore-dic-word \"" arg "\")") entry))))

(defun skk-katakana-region (start end &optional vcontract)
  "$B%j!<%8%g%s$N$R$i$,$J$r%+%?%+%J$KJQ49$9$k!#(B
$B%*%W%7%g%J%k0z?t$N(B VCONTRACT $B$,(B non-nil $B$G$"$l$P!"(B\"$B$&!+(B\" $B$r(B \"$B%t(B\" $B$KJQ49$9(B
$B$k!#(B
$B0z?t$N(B START $B$H(B END $B$O?t;z$G$b%^!<%+!<$G$bNI$$!#(B"
  (interactive "*r\nP")
  (if vcontract
      (skk-search-and-replace
       start end "$B$&!+(B" (lambda (matched) nil "$B%t(B")))
  (skk-search-and-replace
   start end "[$B$!(B-$B$s(B]+"
   (lambda (matched) (skk-hiragana-to-katakana matched))))

(defun skk-hiragana-region (start end &optional vexpand)
  "$B%j!<%8%g%s$N%+%?%+%J$r$R$i$,$J$KJQ49$9$k!#(B
$B%*%W%7%g%J%k0z?t$N(B VEXPAND $B$,(B non-nil $B$G$"$l$P!"(B\"$B%t(B\" $B$r(B \"$B$&!+(B\" $B$KJQ49$9$k!#(B
$B0z?t$N(B START $B$H(B END $B$O?t;z$G$b%^!<%+!<$G$bNI$$!#(B
\"$B%u(B\" $B$H(B \"$B%v(B\" $B$OJQ99$5$l$J$$!#$3$N(B 2 $B$D$NJ8;z$OBP1~$9$k$R$i$,$J$,$J$$$N$G!"%+(B
$B%?%+%J$H$7$F$O07$o$l$J$$!#(B"
  (interactive "*r\nP")
  (if vexpand
      (skk-search-and-replace
       start end "$B%t(B" (lambda (matched) nil "$B$&!+(B")))
  (skk-search-and-replace
   start end "[$B%!(B-$B%s(B]+"
   (lambda (matched) (skk-katakana-to-hiragana matched))))

(defun skk-jisx0208-latin-region (start end)
  "$B%j!<%8%g%s$N(B ascii $BJ8;z$rBP1~$9$kA43Q1QJ8;z$KJQ49$9$k!#(B"
  (interactive "*r")
  (skk-search-and-replace
   start end "[ -~]"
   (lambda (matched)
     (aref skk-default-jisx0208-latin-vector (string-to-char matched)))))

(defun skk-latin-region (start end)
  ;; $B%j!<%8%g%s$NA43Q1Q?t;z$rBP1~$9$k(B ascii $BJ8;z$KJQ49$9$k!#(B
  (interactive "*r")
  (skk-search-and-replace
   start end "\\cS\\|\\cA" ; "$B!+(B" $B$K%^%C%A$7$A$c$&(B...
   (lambda (matched)
     (let ((ascii (skk-jisx0208-to-ascii matched)))
       (or ascii matched)))))

(defun skk-search-and-replace (start end regexp func)
  (let (matched replace)
    (save-match-data
      (skk-save-point
       ;; END may be changed when length of MATCHED and one of REPLACE
       ;; are different.
       (setq end (set-marker (make-marker) end))
       (goto-char start)
       (while (re-search-forward regexp end 'noerror)
	 (setq matched (buffer-substring-no-properties
			(match-beginning 0) (match-end 0))
	       replace (funcall func matched))
	 (goto-char (match-beginning 0))
	 ;; firstly insert a new string, secondly delete an old string to save
	 ;; the cursor position.
	 (insert-and-inherit replace)
	 (delete-region (+ (match-beginning 0) (length replace))
			(+ (match-end 0) (length replace))))
       (set-marker end nil)))))

(defun skk-katakana-henkan (arg)
  "$B"&%b!<%I$G$"$l$P!"%j!<%8%g%s$N$R$i$,$J$r%+%?%+%J$KJQ49$9$k!#(B
$B"'%b!<%I$G$O2?$b$7$J$$!#(B
$B$=$NB>$N%b!<%I$G$O!"%*%j%8%J%k$N%-!<3d$jIU$1$G%P%$%s%I$5$l$F$$$k%3%^%s%I$r<B9T(B
$B$9$k!#(B"
  (interactive "*P")
  (skk-with-point-move
   (if skk-henkan-on
       (if skk-henkan-active
	   nil
	 (skk-set-marker skk-henkan-end-point (point))
	 (skk-*-henkan-1 'skk-katakana-region skk-henkan-start-point
			 skk-henkan-end-point 'vcontract))
     (skk-emulate-original-map arg))))

(defun skk-hiragana-henkan (arg)
  "$B"&%b!<%I$G$"$l$P!"%j!<%8%g%s$N%+%?%+%J$r$R$i$,$J$KJQ49$9$k!#(B
$B"'%b!<%I$G$O2?$b$7$J$$!#(B
$B$=$NB>$N%b!<%I$G$O!"%*%j%8%J%k$N%-!<3d$jIU$1$G%P%$%s%I$5$l$F$$$k%3%^%s%I$r<B9T(B
$B$9$k!#(B"
  (interactive "*P")
  (skk-with-point-move
   (if skk-henkan-on
       (if skk-henkan-active
	   nil
	 (skk-set-marker skk-henkan-end-point (point))
	 (skk-*-henkan-1 'skk-hiragana-region skk-henkan-start-point
			 skk-henkan-end-point 'vexpand))
     (skk-emulate-original-map arg))))

(defun skk-jisx0208-latin-henkan (arg)
  "$B"&%b!<%I$G$"$l$P!"(Bascii $BJ8;z$rBP1~$9$kA43Q1QJ8;z$KJQ49$9$k!#(B
$B"'%b!<%I$G$O2?$b$7$J$$!#(B
$B$=$NB>$N%b!<%I$G$O!"%*%j%8%J%k$N%-!<3d$jIU$1$G%P%$%s%I$5$l$F$$$k%3%^%s%I$r<B9T(B
$B$9$k!#(B"
  (interactive "*P")
  (skk-with-point-move
   (if skk-henkan-on
       (if skk-henkan-active
	   nil
	 (skk-set-marker skk-henkan-end-point (point))
	 (skk-*-henkan-1 'skk-jisx0208-latin-region skk-henkan-start-point
			 skk-henkan-end-point))
     (skk-emulate-original-map arg))))

(defun skk-latin-henkan (arg)
  "$B"&%b!<%I$G$"$l$P!"(Bascii $BJ8;z$rBP1~$9$kA43QJ8;z$KJQ49$9$k!#(B
$B"'%b!<%I$G$O2?$b$7$J$$!#(B
$B$=$NB>$N%b!<%I$G$O!"%*%j%8%J%k$N%-!<3d$jIU$1$G%P%$%s%I$5$l$F$$$k%3%^%s%I$r<B9T(B
$B$9$k!#(B"
  (interactive "*P")
  (skk-with-point-move
   (if skk-henkan-on
       (if skk-henkan-active
	   nil
	 (skk-set-marker skk-henkan-end-point (point))
	 (skk-*-henkan-1 'skk-latin-region skk-henkan-start-point
			 skk-henkan-end-point))
     (skk-emulate-original-map arg))))

(defun skk-*-henkan-1 (func &rest args)
  ;; $BJQ492DG=$+$I$&$+$N%A%'%C%/$r$7$?8e$K(B ARGS $B$r0z?t$H$7$F(B FUNC $B$rE,MQ$7!"(B
  ;; skk-henkan-start-point $B$H(B skk-henkan-end-point $B$N4V$NJ8;zNs$rJQ49$9$k!#(B
  (cond ((skk-get-prefix skk-current-rule-tree)
	 (skk-error "$B%U%#%C%/%9$5$l$F$$$J$$(B skk-prefix $B$,$"$j$^$9(B"
		    "Have unfixed skk-prefix"))
	((< (point) skk-henkan-start-point)
	 (skk-error "$B%+!<%=%k$,JQ493+;OCOE@$h$jA0$K$"$j$^$9(B"
		    "Henkan end point must be after henkan start point"))
	((and (not skk-allow-spaces-newlines-and-tabs)
	      (skk-save-point (beginning-of-line)
			      (> (point) skk-henkan-start-point)))
	 (skk-error "$BJQ49%-!<$K2~9T$,4^$^$l$F$$$^$9(B"
		    "Henkan key may not contain a new line character")))
  (apply func args)
  (skk-kakutei))

(defun skk-hiragana-to-katakana (hiragana)
  (static-cond
   ((not (eq skk-emacs-type 'nemacs))
    (let ((diff (- ?$B%"(B ?$B$"(B)))
      (mapconcat
       (function (lambda (e)
		   (if (and (<= ?$B$!(B e) (>= ?$B$s(B e))
		       (char-to-string (+ e diff))
		     (char-to-string e))))
       (string-to-int-list hiragana) "")))
   (t (save-match-data
	(let ((start 0))
	  (while (string-match "[$B$!(B-$B$s(B]" hiragana start)
	    (aset hiragana (match-beginning 0) ?\245)
	    (setq start (match-end 0)))
	  hiragana)))))

(defun skk-katakana-to-hiragana (katakana)
  (static-cond
   ((not (eq skk-emacs-type 'nemacs))
    (let ((diff (- ?$B%"(B ?$B$"(B)))
      (mapconcat
       (function (lambda (e)
		   (if (and (<= ?$B%!(B e) (>= ?$B%s(B e))
		       (char-to-string (- e diff))
		     (char-to-string e))))
       (string-to-int-list katakana) "")))
   (t (save-match-data
	(let ((start 0))
	  (while (string-match "[$B%!(B-$B%s(B]" katakana start)
	    (aset katakana (match-beginning 0) ?\244)
	    (setq start (match-end 0)))
	  katakana)))))

(defun skk-splice-in (org offset spliced)
  ;; ORG := '(A B C), SPLICED := '(X Y), OFFSET := 1
  ;; -> '(A B X Y C)
  (let (tmp tail)
    (or (> offset 0) (error "Cannot splice in!"))
    (setq tmp (nthcdr (1- offset) org)
          tail (cdr tmp))
    (setcdr tmp nil) ;cut off
    (setcdr tmp (if tail (nconc spliced tail) spliced))
    org))

;; (defun skk-chomp (nth list)
;;   ;; LIST := '(A B C D), NTH := 1
;;   ;; -> '(A B)
;;   (and (> nth -1) (setcdr (nthcdr nth list) nil))
;;   list)

(defun skk-henkan-face-on ()
  ;; skk-use-face $B$,(B non-nil $B$N>l9g!"(Bskk-henkan-start-point $B$H(B
  ;; skk-henkan-end-point $B$N4V$N(B face $BB0@-$r(B skk-henkan-face $B$NCM$KJQ99$9$k!#(B
  ;;
  ;; SKK 9.4 $B$h$j(B Text Properties $B$r;HMQ$9$k$N$r;_$a$F!"(BOverlays $B$r;HMQ$9$k$h(B
  ;; $B$&$K$7$?(B (egg.el, canna.el, wnn-egg.el $B$r;29M$K$7$?(B)$B!#(B
  ;; Overlays $B$O!"%F%-%9%H$N0lIt$G$O$J$$$N$G!"%P%C%U%!$+$iJ8;z$r@Z$j=P$7$F$b%3(B
  ;; $B%T!<$NBP>]$K$J$i$J$$$7!"%"%s%I%%;~$bL5;k$5$l$k$N$G!"JQ49$5$l$?8uJd$NI=<((B
  ;; $B$r0l;~E*$KJQ99$9$k$K$O(B Text Properties $B$h$j$b9%ET9g$G$"$k!#(B
  (if (and skk-henkan-face
	   (marker-position skk-henkan-start-point)
	   (marker-position skk-henkan-end-point))
      (skk-face-on skk-henkan-overlay
		   skk-henkan-start-point skk-henkan-end-point
		   skk-henkan-face skk-henkan-overlay-priority)))

(defun skk-henkan-face-off ()
  ;; skk-henkan-start-point $B$H(B skk-henkan-end-point $B$N4V$NI=<($rJQ99$7$F$$$k(B
  ;; skk-henkan-overlay $B$r>C$9!#(B
  (and skk-henkan-face (skk-detach-extent skk-henkan-overlay)))

(defun skk-detach-extent (object)
  (static-cond
   ((eq skk-emacs-type 'xemacs)
    (and (extentp object) (detach-extent object)))
   (t
    (and (overlayp object) (delete-overlay object)))))

(defun skk-make-face (face)
  ;; hilit-lookup-face-create $B$N%5%V%;%C%H!#(Btutorial $B$G?'IU$1$r9T$J$&>l9g$G$b(B
  ;; hilit19 $B$K0MB8$;$:$H$j$"$($:(B face $B$r<+A0$G:n$k$3$H$,$G$-$k$h$&$K!"$H$$$&(B
  ;; $BL\E*$G:n$C$?$b$N$G!"4JC1$J?'IU$1$7$+$G$-$J$$!#$"$^$j8-$/$O$J$$!#J#;($J(B
  ;; face $B$r:n$j$?$$?M$O(B hilit-lookup-face-create $BEy$r;H$C$F2<$5$$!#(B
  (or (car (memq face (face-list)))
      (let ((face-name (symbol-name face)))
        (setq face (make-face face))
        (save-match-data
          (if (not (string-match "/" face-name))
              (set-face-foreground face face-name)
            (set-face-foreground
             face
             (substring face-name 0 (match-beginning 0)))
            (set-face-background
             face
             (substring face-name (1+ (match-beginning 0)))))
          face))))

;; skk-auto.el, skk-rdbms.el $B$NN>J}$G;H$&$N$G!"(Bskk-auto.el $B$h$j0\F0$7$?!#(B
(defun skk-remove-common (word)
  ;; skk-henkan-key $B$H(B word $B$N4V$K6&DL$NAw$j2>L>$r<h$j=|$-!"Aw$j2>L>0J30$NItJ,(B
  ;; $B$NJ8;zNs$rJV$9!#(Bskk-henkan-key $B$H(B skk-henkan-okurigana $B$NCM$r%;%C%H$9$k!#(B
  ;; $BNc$($P!"(Bword == $B;}$C$F$-$?(B $B$G$"$l$P!"(Bskk-henkan-key := "$B$b(Bt",
  ;; skk-henkan-okurigana := "$B$C$F(B", word := "$B;}(B" $B$N$h$&$KJ,2r$7!"(Bword $B$rJV$9!#(B
  ;; skk-auto-okuri-process $B$NCM$,(B non-nil $B$G$"$k$H$-$K$3$N4X?t$r;HMQ$9$k!#(B
  ;; $BJQ49$,9T$J$o$l$?%P%C%U%!$G%3!<%k$5$l$k(B ($B<-=q%P%C%U%!$G$O$J$$(B)$B!#(B
  (if (and (not (skk-numeric-p)) (not skk-abbrev-mode)
           (or skk-henkan-in-minibuff-flag
               (and (<= skk-okuri-index-min skk-henkan-count)
                    (<= skk-henkan-count skk-okuri-index-max))))
      (let ((midasi skk-henkan-key)
            (midasi-len (skk-str-length skk-henkan-key))
            (word-len (skk-str-length word))
            (cont t)
            char pos pos2 midasi-tail word-tail new-word okuri-first
            new-skk-okuri-char new-skk-henkan-key)
        (if (not (and (>= midasi-len 2) (>= word-len 2)))
            nil
          ;; check if both midasi and word end with the same ascii char.
          (if (and (skk-ascii-char-p (skk-str-ref midasi (1- midasi-len)))
		   (eq (skk-str-ref midasi (1- midasi-len))
		       (skk-str-ref word (1- word-len))))
              ;; if so chop off the char from midasi and word.
	      ;; assume size of an ASCII char is always 1.
              (setq midasi (substring midasi 0 -1)
                    midasi-len (1- midasi-len)
                    word (substring word 0 -1)
                    word-len (1- word-len)))
          (setq midasi-tail (skk-substring midasi (1- midasi-len)
					   midasi-len)
		word-tail (skk-substring word (1- word-len)
					 word-len))
          (if (not (and (string= midasi-tail word-tail)
			(or (and (skk-string<= "$B$!(B" midasi-tail)
				 (skk-string<= midasi-tail "$B$s(B"))
			    (member midasi-tail '("$B!"(B" "$B!#(B" "$B!$(B" "$B!%(B")))))
	      nil
	    ;; $B8+=P$78l$HC18l$H$NKvHx$,F10l$N$+$JJ8;z$N>l9g!#(B
	    ;; $BAw$j$J$7$rAw$j$"$j$X(B
	    (setq pos (1- word-len)
		  new-word new-skk-henkan-key)
	    (while (and cont (> pos 0))
	      (setq char (skk-substring word (1- pos) pos))
	      (if (and (skk-string<= "$B0!(B" char) (skk-string<= char "$Bt$(B"))
		  ;; char is the right-most Kanji
		  (setq cont nil)
		(setq pos (1- pos))))
	    (setq pos2 (- midasi-len (- word-len pos)))
	    ;; check if midasi and word has the same tail of length
	    (if (not (string= (skk-substring midasi pos2 midasi-len)
			      (skk-substring word pos word-len)))
		nil
	      (setq okuri-first (skk-substring word pos (1+ pos)))
	      (setq skk-henkan-okurigana
		    (if (and (string= okuri-first "$B$C(B")
			     (<= (+ pos 2) word-len))
			;; in this case okuriga consits of two
			;; characters, e.g., $B!V;D$C$?!W(B
			(skk-substring word pos (+ pos 2))
		      okuri-first))
	      (setq new-word (skk-substring word 0 pos)
		    new-skk-okuri-char (skk-okurigana-prefix okuri-first)
		    new-skk-henkan-key (concat
					(skk-substring midasi 0 pos2)
					new-skk-okuri-char))
	      (if (not skk-henkan-in-minibuff-flag)
		  (setq word new-word
			skk-henkan-key new-skk-henkan-key)
		;; $B<-=qEPO?%b!<%I$GEPO?$5$l$?>l9g!#(B
		;; ask if register as okuri-ari word.
		(let (inhibit-quit)	; allow keyboard quit
		  (if (y-or-n-p
		       (format
			(if skk-japanese-message-and-error
			    "%s /%s/ $B$rAw$j$"$j%(%s%H%j$H$7$FEPO?$7$^$9$+!)(B"
			  "Shall I register this as okuri-ari entry: %s /%s/ ? ")
			new-skk-henkan-key new-word))
		      (setq word new-word
			    skk-okuri-char new-skk-okuri-char
			    skk-henkan-key new-skk-henkan-key)
		    (setq skk-henkan-okurigana nil
			  skk-okuri-char nil)
		    (message "")))))))))
  ;; $BJ,2r$7$?(B word ($BAw$j2>L>ItJ,$r=|$$$?$b$N(B) $B$rJV$9!#(B
  word)

(defun skk-okurigana-prefix (okurigana)
  (cond ((not (and (skk-string<= "$B$!(B" okurigana) (skk-string<= okurigana "$B$s(B")))
	 nil)
	((string= okurigana "$B$s(B")
	 "n")
	((and (string= okurigana "$B$C(B")
	      (not (string= skk-henkan-okurigana "$B$C(B")))
	 (aref skk-kana-rom-vector
	       ;; assume the character is hiragana of JIS X 0208.
	       (static-cond
		((eq skk-emacs-type 'nemacs)
		 (- (string-to-char
		     (substring skk-henkan-okurigana 3 4)) 161))
		(t
		 (- (skk-char-octet
		     (string-to-char (skk-substring skk-henkan-okurigana 1 2))
		     1)
		    33)))))
	(t (aref skk-kana-rom-vector
		 (static-cond
		  ((eq skk-emacs-type 'nemacs)
		   (- (string-to-char
		       (substring skk-henkan-okurigana 1 2)) 161))
		 (t
		  (- (skk-char-octet
		     (string-to-char (skk-substring skk-henkan-okurigana 0 1))
		     1)
		    33)))))))

;; from type-break.el.  Welcome!
(defun skk-time-difference (a b)
  ;; Compute the difference, in seconds, between a and b, two structures
  ;; similar to those returned by `current-time'.
  ;; Use addition rather than logand since that is more robust; the low 16
  ;; bits of the seconds might have been incremented, making it more than 16
  ;; bits wide.
  (+ (lsh (- (car b) (car a)) 16)
     (- (nth 1 b) (nth 1 a))))

(defun skk-remove-minibuffer-setup-hook (&rest args)
  ;; Remove all args from minibuffer-setup-hook.
  (while args
    (remove-hook 'minibuffer-setup-hook (car args))
    (setq args (cdr args))))

(add-hook 'edit-picture-hook 'skk-misc-for-picture 'append)
(add-hook 'skk-before-kill-emacs-hook 'skk-record-jisyo-data)
;; add 'skk-save-jisyo only to remove easily.
(add-hook 'skk-before-kill-emacs-hook 'skk-save-jisyo)
(add-hook 'minibuffer-exit-hook
          (function
           (lambda ()
	     (remove-hook 'pre-command-hook 'skk-pre-command 'local)
	     (skk-remove-minibuffer-setup-hook
	      'skk-j-mode-on 'skk-setup-minibuffer
	      (function (lambda ()
			  (add-hook 'pre-command-hook 'skk-pre-command nil 'local)))))))

(defun skk-setup-modeline ()
  "$B%b!<%I9T$X$N%9%F!<%?%9I=<($r=`Hw$9$k!#(B"
  (cond ((eq skk-status-indicator 'left)
	 (let ((list
		(cond
		 ((and (fboundp 'face-proportional-p)
		       (face-proportional-p 'modeline))
		  '((skk-latin-mode-string . ("--SKK:" . " SKK"))
		    (skk-hiragana-mode-string . ("--$B$+$J(B:" . " $B$+$J(B"))
		    (skk-katakana-mode-string . ("--$B%+%J(B:" . " $B%+%J(B"))
		    (skk-jisx0208-latin-mode-string . ("--$BA41Q(B:" . " $BA41Q(B"))
		    (skk-abbrev-mode-string . ("--a$B$"(B:" . " a$B$"(B"))
		    (skk-jisx0201-mode-string . ("--jisx0201" . " jisx0201"))))
		 (t
		  '((skk-latin-mode-string . ("--SKK::" . " SKK"))
		    (skk-hiragana-mode-string . ("--$B$+$J(B:" . " $B$+$J(B"))
		    (skk-katakana-mode-string . ("--$B%+%J(B:" . " $B%+%J(B"))
		    (skk-jisx0208-latin-mode-string . ("--$BA41Q(B:" . " $BA41Q(B"))
		    (skk-abbrev-mode-string . ("--a$B$"(B::" . " a$B$"(B"))
		    (skk-jisx0201-mode-string . ("--jisx0201" . " jisx0201")))))))
	   (while list
	     (let ((sym (caar list))
		   (strs (cdar list)))
	       (if (string= (symbol-value sym) (cdr strs))
		   (set sym (car strs))))
	     (setq list (cdr list))))
	 ;;
	 (static-cond
	  ((eq skk-emacs-type 'xemacs)
	   (let ((extent (make-extent nil nil)))
	     (unless (rassq 'skk-input-mode-string default-modeline-format)
	       (setq-default default-modeline-format
			     (append (list
				      ""
				      (cons extent 'skk-input-mode-string)
				      default-modeline-format))))
	     (mapc
	      (function
	       (lambda (buf)
		 (when (buffer-live-p buf)
		   (save-excursion
		     (set-buffer buf)
		     (when (and (listp modeline-format)
				(not (rassq 'skk-input-mode-string modeline-format)))
		       (setq modeline-format
			     (append (list
				      ""
				      (cons extent 'skk-input-mode-string))
				     modeline-format)))))))
	      (buffer-list))))
	  ;;
	  (t
	   (unless (memq 'skk-input-mode-string (default-value 'mode-line-format))
	     (setq-default mode-line-format
			   (append '("" skk-input-mode-string)
				   (default-value 'mode-line-format))))
	   (let ((list (buffer-list)))
	     (while list
	       (let ((buf (car list)))
		 (when (buffer-live-p buf)
		   (save-excursion
		     (set-buffer buf)
		     (when (and (listp mode-line-format)
				(or (assq 'mode-line-format (buffer-local-variables))
				    (memq 'mode-line-format (buffer-local-variables)))
				(not (memq 'skk-input-mode-string mode-line-format)))
		       (setq mode-line-format
			     (append '("" skk-input-mode-string)
				     mode-line-format))))))
	       (setq list (cdr list))))))
	 (setq-default skk-input-mode-string "")
	 (force-mode-line-update t))
	;;
	((eq skk-status-indicator 'minor-mode)
	 (static-if (eq skk-emacs-type 'xemacs)
	     (add-minor-mode 'skk-mode 'skk-input-mode-string)
	   (setq minor-mode-alist
		 ;; each element of minor-mode-alist is not cons cell.
		 (put-alist 'skk-mode
			    '(skk-input-mode-string) minor-mode-alist))))))

;; cover to original functions.

(skk-defadvice keyboard-quit (around skk-ad activate)
  "$B"'%b!<%I$G$"$l$P!"8uJd$NI=<($r$d$a$F"&%b!<%I$KLa$9(B ($B8+=P$78l$O;D$9(B)$B!#(B
$B"&%b!<%I$G$"$l$P!"8+=P$78l$r:o=|$9$k!#(B
$B>e5-$N$I$A$i$N%b!<%I$G$b$J$1$l$P(B keyboard-quit $B$HF1$8F0:n$r$9$k!#(B"
  (cond
   ;; SKK is not invoked in the current buffer.
   ((not skk-mode) ad-do-it)
   ;; $B"#(B mode (Kakutei input mode).
   ((not skk-henkan-on)
    (cond ((skk-get-prefix skk-current-rule-tree)
	   (skk-erase-prefix 'clean))
	  (t ad-do-it)))
   ;; $B"'(B mode (Conversion mode).
   (skk-henkan-active
    (setq skk-henkan-count 0)
    (if (and skk-delete-okuri-when-quit skk-henkan-okurigana)
	(let ((count (/ (length skk-henkan-okurigana) skk-kanji-len)))
	  (skk-previous-candidate)
	  ;; $B$3$3$G$O(B delete-backward-char $B$KBhFs0z?t$rEO$5$J$$J}$,%Y%?!<!)(B
	  (delete-backward-char count))
      (skk-previous-candidate)))
   ;; $B"&(B mode (Midashi input mode).
   (t (skk-erase-prefix 'clean)
      (and (> (point) skk-henkan-start-point)
	   (delete-region (point) skk-henkan-start-point))
      (skk-kakutei))))

(skk-defadvice abort-recursive-edit (around skk-ad activate)
  "$B"'%b!<%I$G$"$l$P!"8uJd$NI=<($r$d$a$F"&%b!<%I$KLa$9(B ($B8+=P$78l$O;D$9(B)$B!#(B
$B"&%b!<%I$G$"$l$P!"8+=P$78l$r:o=|$9$k!#(B
$B>e5-$N$I$A$i$N%b!<%I$G$b$J$1$l$P(B abort-recursive-edit $B$HF1$8F0:n$r$9$k!#(B"
  ;; subr command but no arg.
  (skk-remove-minibuffer-setup-hook
   'skk-j-mode-on 'skk-setup-minibuffer
   (function (lambda () (add-hook 'pre-command-hook 'skk-pre-command nil 'local))))
  (cond ((not skk-mode) ad-do-it)
	((not skk-henkan-on)
	 (cond ((skk-get-prefix skk-current-rule-tree)
		(skk-erase-prefix 'clean))
	       (t ad-do-it)))
        (skk-henkan-active
         (setq skk-henkan-count 0)
         (if (and skk-delete-okuri-when-quit skk-henkan-okurigana)
             (let ((count (/ (length skk-henkan-okurigana) skk-kanji-len)))
               (skk-previous-candidate)
               ;; $B$3$3$G$O(B delete-backward-char $B$KBhFs0z?t$rEO$5$J$$J}$,%Y%?!<!)(B
               (delete-backward-char count))
           (skk-previous-candidate)))
	(t (skk-erase-prefix 'clean)
	   (and (> (point) skk-henkan-start-point)
		(delete-region (point) skk-henkan-start-point))
	   (skk-kakutei))))
	
(skk-defadvice newline (around skk-ad activate)
  "skk-egg-like-newline $B$,(B non-nil $B$@$C$?$i!"JQ49Cf$N(B newline $B$G3NDj$N$_9T$$!"2~9T$7$J$$!#(B"
  (if (not (or skk-j-mode skk-abbrev-mode))
      ad-do-it
    (let (
	  ;;(arg (ad-get-arg 0))
          ;; skk-kakutei $B$r<B9T$9$k$H(B skk-henkan-on $B$NCM$,L5>r7o$K(B nil $B$K$J$k(B
          ;; $B$N$G!"J]B8$7$F$*$/I,MW$,$"$k!#(B
          (no-newline (and skk-egg-like-newline skk-henkan-on))
	  (auto-fill-function (and (interactive-p) auto-fill-function)))
      ;; fill $B$5$l$F$b(B nil $B$,5"$C$F$/$k(B :-<
      ;;(if (skk-kakutei)
      ;;    (setq arg (1- arg)))
      ;;(if skk-mode
      ;;    (let ((opos (point)))
      ;;      ;; skk-kakutei (skk-do-auto-fill) $B$K$h$C$F9T$,@^$jJV$5$l$?$i(B arg $B$r(B 1 $B$D8:$i$9!#(B
      ;;      (skk-kakutei)
      ;;      (if (and (not (= opos (point))) (integerp arg))
      ;;          (ad-set-arg 0 (1- arg)))))
      (and skk-mode (skk-kakutei))
      (if (not no-newline)
	  ad-do-it))))

(skk-defadvice newline-and-indent (around skk-ad activate)
  "skk-egg-like-newline $B$,(B non-nil $B$@$C$?$i!"JQ49Cf$N(B newline-and-indent $B$G3NDj$N$_9T$$!"2~9T$7$J$$!#(B"
  (if (not (or skk-j-mode skk-abbrev-mode))
      ad-do-it
    (let ((no-newline (and skk-egg-like-newline skk-henkan-on))
	  (auto-fill-function (and (interactive-p) auto-fill-function)))
      (and skk-mode (skk-kakutei))
      (or no-newline ad-do-it))))

(skk-defadvice exit-minibuffer (around skk-ad activate)
  "skk-egg-like-newline $B$,(B non-nil $B$@$C$?$i!"JQ49Cf$N(B exit-minibuffer $B$G3NDj$N$_9T$&!#(B"
  ;; subr command but no arg.
  (skk-remove-minibuffer-setup-hook
   'skk-j-mode-on 'skk-setup-minibuffer
   (function (lambda ()
	       (add-hook 'pre-command-hook 'skk-pre-command nil 'local))))
  (if (not (or skk-j-mode skk-abbrev-mode))
      ad-do-it
    (let ((no-newline (and skk-egg-like-newline skk-henkan-on)))
      (and skk-mode (skk-kakutei))
      (or no-newline ad-do-it))))

(defadvice picture-mode-exit (before skk-ad activate)
  "SKK $B$N%P%C%U%!%m!<%+%kJQ?t$rL58z$K$7!"(Bpicture-mode-exit $B$r%3!<%k$9$k!#(B
picture-mode $B$+$i=P$?$H$-$K$=$N%P%C%U%!$G(B SKK $B$r@5>o$KF0$+$9$?$a$N=hM}!#(B"
  (and skk-mode (skk-kill-local-variables)))

(skk-defadvice undo (before skk-ad activate)
  "SKK $B%b!<%I$,(B on $B$J$i(B skk-self-insert-non-undo-count $B$r=i4|2=$9$k!#(B"
  (and skk-mode (setq skk-self-insert-non-undo-count 0)))

(skk-defadvice kill-buffer (before skk-ad activate)
  "SKK $B$N"'%b!<%I$@$C$?$i!"3NDj$7$F$+$i%P%C%U%!$r%-%k$9$k!#(B"
  (interactive "bKill buffer: ") ; subr command with arg.
  (and skk-mode skk-henkan-on (interactive-p) (skk-kakutei)))

(skk-defadvice save-buffers-kill-emacs (before skk-ad activate)
  (run-hooks 'skk-before-kill-emacs-hook))

(defadvice comint-send-input (around skk-ad activate compile)
  (cond ((or skk-henkan-on skk-henkan-active)
	 (skk-kakutei)
	 (unless skk-egg-like-newline ad-do-it))
	(t ad-do-it)))

(static-if (eq skk-emacs-type 'xemacs)
    (skk-defadvice minibuffer-keyboard-quit (around skk-ad activate)
      ;; XEmacs has minibuffer-keyboard-quit that has nothing to do with delsel.
      (skk-remove-minibuffer-setup-hook
       'skk-j-mode-on 'skk-setup-minibuffer
       (function (lambda ()
		   (add-hook 'pre-command-hook 'skk-pre-command nil 'local))))
      (cond ((not skk-mode) ad-do-it)
	    ((not skk-henkan-on)
	     (cond ((skk-get-prefix skk-current-rule-tree)
		    (skk-erase-prefix 'clean))
		   (t ad-do-it)))
	    (skk-henkan-active
	     (setq skk-henkan-count 0)
	     (if (and skk-delete-okuri-when-quit skk-henkan-okurigana)
		 (let ((count (/ (length skk-henkan-okurigana) skk-kanji-len)))
		   (skk-previous-candidate)
		   ;; $B$3$3$G$O(B delete-backward-char $B$KBhFs0z?t$rEO$5$J$$J}$,%Y%?!<!)(B
		   (delete-backward-char count))
	       (skk-previous-candidate)))
	    (t (skk-erase-prefix 'clean)
	       (and (> (point) skk-henkan-start-point)
		    (delete-region (point) skk-henkan-start-point))
	       (skk-kakutei)))))

(defun skk-mode-once-again ()
  ;; skk-mode $B$N5/F0Cf$K(B skk-mode $B$K(B advice $B$rD%$C$?>l9g!":G=i$N(B 1 $B2s$@$1$=$N(B
  ;; $B%"%I%P%$%9$,@8$-$J$$$N$G!"(Bskk-mode-hook $B$r;H$C$F:FEY(B adviced skk-mode $B$r(B
  ;; $B5/F0$9$k!#%U%C%/<+LG5!G=IU$-!#(B
  (remove-hook 'skk-mode-hook 'skk-mode-once-again)
  (let ((off (not skk-mode)))
    (skk-mode 1)
    (if off (skk-mode -1))))

(run-hooks 'skk-load-hook)

(require 'product)
(product-provide (provide 'skk) (require 'skk-version))
;;; Local Variables:
;;; End:
;;; skk.el ends here
