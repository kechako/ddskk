;;; skk-isearch.el --- isearch mode for skk with Emacs 19 or later and XEmacs.
;; Copyright (C) 1994, 1995, 1996, 1997, 1998, 1999
;; Enami Tsugutomo <enami@ba2.so-net.or.jp>

;; Author: Enami Tsugutomo <enami@ba2.so-net.or.jp>
;; Maintainer: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Version: $Id: skk-isearch.el,v 1.5.2.4.2.18 1999/12/30 09:05:50 czkmt Exp $
;; Keywords: japanese
;; Last Modified: $Date: 1999/12/30 09:05:50 $

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
;; functions for hooks.
;;
;; 1. always invoke skk isearch.
;; (add-hook 'isearch-mode-hook 'skk-isearch-mode-setup)
;; (add-hook 'isearch-mode-end-hook 'skk-isearch-mode-cleanup)
;;
;; 2. invoke only if skk-mode is on.
;; (add-hook 'isearch-mode-hook
;;           (function (lambda ()
;;			 (and (boundp 'skk-mode) skk-mode
;;			      (skk-isearch-mode-setup)))))
;;
;; (add-hook 'isearch-mode-end-hook
;;           (function
;;            (lambda ()
;;              (and (boundp 'skk-mode) skk-mode (skk-isearch-mode-cleanup)))))
;; 
;; 3. invoke if current buffer has japanese characters.
;; ...
;;
;; skk-isearch-initial-mode examine the variable of skk before calling
;; skk-mode.

;;; Code:
(eval-when-compile (require 'skk-macs) (require 'static))
(require 'skk)
(require 'skk-vars)

;; interface to skk.el
;;
(defsubst skk-isearch-turn-off-skk-mode ()
  "Turn off skk mode."
  (skk-mode 0))

(defsubst skk-isearch-turn-on-skk-mode ()
  "Turn on skk mode."
  (skk-mode 1))

(defsubst skk-isearch-conversion-active-p ()
  "Non-nil if skk conversion is active."
  skk-henkan-on)

(defsubst skk-isearch-conversion-start ()
  "Point where conversion is start.  Includes skk marker."
  (- skk-henkan-start-point skk-kanji-len))

(defsubst skk-isearch-skk-kakutei ()
  "Perform kakutei."
  (skk-kakutei))

(defsubst skk-isearch-skk-hiragana-mode-p ()
  "Non-nil if skk is hiragana input mode."
  (and (not skk-katakana) skk-j-mode))

(defsubst skk-isearch-skk-turn-on-hiragana-mode ()
  "Set current skk mode to hiragana input mode."
  (skk-j-mode-on))

(defsubst skk-isearch-skk-katakana-mode-p ()
  "Non-nil if skk is katakana input mode."
  (and skk-j-mode skk-katakana))

(defsubst skk-isearch-skk-turn-on-katakana-mode ()
  "Set current skk mode to katakana input mode."
  (skk-j-mode-on 'katakana))

(defsubst skk-isearch-skk-jisx0208-latin-mode-p ()
  "Non-nil if skk is jisx0208 latin (zenkaku) input mode."
  skk-jisx0208-latin-mode)

(defsubst skk-isearch-skk-abbrev-mode-p ()
  "Non-nil if skk is Abbrev mode."
  skk-abbrev-mode)

(defsubst skk-isearch-skk-turn-on-jix0208-latin-mode ()
  "Set current skk mode to jisx0208 latin (zenkaku) input mode."
  (skk-jisx0208-latin-mode-on))

(defsubst skk-isearch-skk-turn-on-latin-mode ()
  "Set current skk mode to normal latin input mode."
  (skk-latin-mode-on))

(defun skk-isearch-message ()
  "Show isearch message."
  (skk-isearch-incomplete-message
   (if (string= skk-prefix "") (char-to-string last-command-char) skk-prefix)))

(defun skk-isearch-current-mode ()
  "Return the symbolic current mode of skk for skk-isearch."
  (cond ((not skk-mode) nil)
	((skk-isearch-skk-katakana-mode-p) 'katakana)
	((skk-isearch-skk-hiragana-mode-p) 'hiragana)
	((skk-isearch-skk-jisx0208-latin-mode-p) 'jisx0208-latin)
	((skk-isearch-skk-abbrev-mode-p) 'abbrev)
	(t 'latin)))

(defun skk-isearch-set-initial-mode (mode)
  "Set up the initial condition according to given symbolic MODE.
The MODE should be canonical."
  ;; following code is highly depends on internal of skk.
  ;; (skk-isearch-turn-on-skk-mode)
  ;; (skk-isearch-skk-kakutei)
  (cond ((eq mode 'hiragana) (skk-isearch-skk-turn-on-hiragana-mode))
	((eq mode 'katakana) (skk-isearch-skk-turn-on-katakana-mode))
	((eq mode 'jisx0208-latin)
	 (skk-isearch-skk-turn-on-jix0208-latin-mode))
	((eq mode 'latin) (skk-isearch-skk-turn-on-latin-mode))
	((not mode) (skk-isearch-turn-off-skk-mode))
	;; shouldn't happen.
	(t (error "unknown skk-isearch-mode %s" mode))))

(defun skk-isearch-symbolic-mode (mode)
  "Return symbolic skk isearch mode for given numerical MODE."
  (car (rassq mode skk-isearch-mode-canonical-alist)))

(defun skk-isearch-numerical-mode (mode)
  "Return numerical skk isearch mode for given symbolic MODE."
  (cdr (assq mode skk-isearch-mode-canonical-alist)))

(defun skk-isearch-mode-string ()
  "Return the current skk mode string for prompting."
  (with-current-buffer (get-buffer-create skk-isearch-working-buffer)
    (cdr (assq (skk-isearch-current-mode) skk-isearch-mode-string-alist))))

(defun skk-isearch-current-numerical-mode ()
  "Return the symbolic skk isearch mode according to the current skk
internal condition."
  (skk-isearch-numerical-mode (or (skk-isearch-current-mode) 'latin)))

(defun skk-isearch-canonical-start-mode (mode)
  "Canonicalize the symbolic skk isearch MODE."
  ;; alias, canonical, or error.
  (cond ((cdr (rassq mode skk-isearch-mode-alias-alist)))
	((cdr (assq mode skk-isearch-mode-alias-alist)))
	((skk-isearch-numerical-mode mode) mode)
	(t (error "Unknown skk-isearch-start-mode: %s" mode))))

(defun skk-isearch-initial-mode ()
  "Return the symbolic mode name of skk-isearch used to initialize working
buffer."
  (cond ((and skk-isearch-use-previous-mode skk-isearch-mode)
	 ;; use the mode when last isearch is done.  note that the
	 ;; `skk-isearch-mode' is numerical, so convert it to symbolic
	 ;; mode.
	 (skk-isearch-symbolic-mode skk-isearch-mode))
	(skk-isearch-start-mode
	 ;; always start with the specified mode.
	 ;; `skk-isearch-start-mode' is symbolic.
	 (skk-isearch-canonical-start-mode skk-isearch-start-mode))
	;; guess the current buffer.  note that if skk-mode is off,
	;; skk-isearch-current-mode returns symbol `nil' and control
	;; falls through to next cond clause.
	((skk-isearch-current-mode))
	;; skk-mode is off in this buffer.
	(t skk-isearch-initial-mode-when-skk-mode-disabled)))

(defun skk-isearch-initialize-working-buffer ()
  "Initialize the current buffer as working buffer for skk isearch.
More precicely, turn on skk-mode, put into kana mode, make sure
kakutei'ed and erase the buffer contents."
  (skk-isearch-turn-on-skk-mode)
  (skk-isearch-skk-kakutei)
  (erase-buffer))

;;;###autoload
(defun skk-isearch-mode-setup ()
  "hook function called when skk isearch begin."
  ;; setup working buffer.  initial skk mode for isearch should be
  ;; determined in the original buffer and set in working buffer.
  (require 'skk)
  (let ((initial (skk-isearch-initial-mode)))
    (with-current-buffer (get-buffer-create skk-isearch-working-buffer)
      (skk-erase-prefix 'clean)
      (skk-isearch-initialize-working-buffer)
      (skk-isearch-set-initial-mode initial)))
  ;; setup variables and keymap
  (or (keymapp skk-isearch-mode-map)
      (static-cond
       ((eq skk-emacs-type 'xemacs)
	(setq skk-isearch-mode-map (skk-isearch-setup-keymap (make-keymap)))
	(set-keymap-parents skk-isearch-mode-map isearch-mode-map))
       (t
	(setq skk-isearch-mode-map
	      (skk-isearch-setup-keymap (cons 'keymap isearch-mode-map))))))
  (set skk-isearch-overriding-local-map skk-isearch-mode-map)
  (setq skk-isearch-incomplete-message ""
	;; set skk-isearch-message non-nil to call skk-isearch-message.
	skk-isearch-message "")
  (skk-isearch-mode-message)
  (skk-isearch-incomplete-message))

;;;###autoload
(defun skk-isearch-mode-cleanup ()
  "Hook function called when skk isearch is done."
  ;; remember the current skk mode for next use.
  (let ((mode (skk-current-insert-mode)))
    (and skk-isearch-use-previous-mode
	 (setq skk-isearch-mode
	       (with-current-buffer (get-buffer-create skk-isearch-working-buffer)
		 (skk-isearch-current-numerical-mode))))
    ;; reset the overrinding-local-map.
    (set skk-isearch-overriding-local-map nil)
    (setq skk-isearch-message nil
	  skk-isearch-last-mode-string ""
	  skk-isearch-last-mode-regexp "")
    ;; $B%5!<%ACf$KF~NO%b!<%I$rJQ99$7$?$i!"%b!<%I%i%$%s$NI=<($b$=$l$K=>$$(B
    ;; $BJQ99$5$l$k$N$G!"%+%l%s%H%P%C%U%!$NF~NO%b!<%I$H%b!<%I%i%$%s$NI=<((B
    ;; $B$H$,(B sync $B$7$J$/$J$k!#=>$$!"%5!<%A$,=*N;$7$?:]!"%b!<%I%i%$%s$r%+(B
    ;; $B%l%s%H%P%C%U%!$NF~NO%b!<%I$H(B sync $B$5$;$k!#(B
    (cond ((eq mode 'hiragana) (skk-j-mode-on))
	  ((eq mode 'katakana) (skk-j-mode-on t))
	  ((eq mode 'abbrev) (skk-abbrev-mode-on))
	  ((eq mode 'latin) (skk-latin-mode-on))
	  ((eq mode 'jisx0208-latin) (skk-jisx0208-latin-mode-on))))
  (remove-hook 'pre-command-hook 'skk-pre-command 'local)
  (skk-remove-minibuffer-setup-hook
   'skk-j-mode-on 'skk-setup-minibuffer
   (function (lambda ()
	       (add-hook 'pre-command-hook 'skk-pre-command nil 'local)))))

(defun skk-isearch-incomplete-message (&optional prefix)
  "Show message when kana kanji convertion is in progress.
Optional argument PREFIX is apppended if given."
  (let ((isearch-message (concat isearch-message
				 skk-isearch-incomplete-message prefix)))
    (isearch-message)))

;;
;; define keymap
;;

;; XXX should be more generic
(defun skk-isearch-setup-keymap (map)
  ;; printable chars.
  (let ((c ?\040))
    (while (< c ?\177)
      (define-key map (char-to-string c) 'skk-isearch-wrapper)
      (setq c (1+ c))))

  ;; control chars for skk.
  (define-key map "\C-g" 'skk-isearch-keyboard-quit)
  (define-key map skk-kakutei-key 'skk-isearch-newline)
  (define-key map "\C-m" 'skk-isearch-exit)

  ;; Keys for `skk-isearch-delete-char'.
  (let ((commands '(backward-delete-char-untabify
		    backward-delete-char
		    backward-or-forward-delete-char
		    delete-backward-char))
	keys)
    (while commands
      (setq keys (where-is-internal (car commands) (current-global-map))
	    commands (cdr commands))
      (while keys
	(define-key map (car keys) 'skk-isearch-delete-char)
	(setq keys (cdr keys)))))

  ;; C-x map for skk.
  (define-key map "\C-x" (make-sparse-keymap))
  (define-key map [?\C-x t] 'isearch-other-control-char)
  (define-key map "\C-x\C-j" 'skk-isearch-skk-mode)
  map)


;;
;; wrapper functions
;;

(defun skk-isearch-redo-function ()
  "Execute the command of given key sequence in skk environment."
  ;; with saving value of old binding.
  (let ((local-map (symbol-value skk-isearch-overriding-local-map)))
    (unwind-protect
	(progn
	  ;; temporarily disable the overriding-local-map.  this
	  ;; should be done in ther buffer isearch is performed, i.e.,
	  ;; before entering skk-isearch-working-buffer.
	  (set skk-isearch-overriding-local-map nil)
	  ;; don't change the current buffer during save/restore the
	  ;; overriding-local-map, because it is buffer local in some
	  ;; version of emacs.
	  (with-current-buffer (get-buffer-create skk-isearch-working-buffer)
	    ;; listify this-command-keys.  this works only if it is
	    ;; string.
	    (setq unread-command-events
                  (append (if (= (length (this-command-keys)) 0)
                              (list last-command-event)
                            (this-command-keys))
                          nil ))
	    (condition-case error
		;; setup last-command-event and this-command because
		;; some command refers them.
		(let* ((keys (read-key-sequence nil))
		       (this-command (key-binding keys)))
		  (setq last-command-event (aref keys (1- (length keys))))
		  (command-execute this-command))
	      ((quit error) (signal (car error) (cdr error)))))
	  (skk-isearch-mode-message))
      (set skk-isearch-overriding-local-map local-map))))

(defun skk-isearch-search-string ()
  "Return the string to be searched.
If the conversion is in progress and no string is fixed, just return nil."
    (with-current-buffer (get-buffer-create skk-isearch-working-buffer)
      (prog1
	  (cond ((skk-isearch-conversion-active-p)
		 (let ((start (skk-isearch-conversion-start)))
		   ;; is there fixed string?
		   (if (> start 1)
		       (prog1
			   (buffer-substring 1 start)
			 (delete-region 1 start)))))
		;; a conversion is in progress
		((not (string= skk-prefix "")) nil)
		;;(skk-current-rule-tree nil)
		;; whole string in the buffer is fixed.
		((not (zerop (buffer-size)))
		 (prog1
		     (buffer-string)
		   (erase-buffer))))
	;; update incomplete-message with contents of working buffer.
	(setq skk-isearch-incomplete-message (buffer-string))
	;; update echo area.
	(skk-isearch-incomplete-message))))


;;
;; regexp search supports.
;;
(defun skk-isearch-last-char (string)
  (and (string-match ".\\'" string)
       (string-to-char (substring string (match-beginning 0)))))

(defun skk-isearch-breakable-p (char)
  (and char
       (funcall skk-isearch-breakable-character-p-function char)))

(defun skk-isearch-search-string-regexp (string)
  (if isearch-regexp
      (let ((chars (string-to-char-list string))
	    (prev (skk-isearch-last-char isearch-string))
	    (result ""))
	(while chars
	  (if (and (skk-isearch-breakable-p prev)
		   (skk-isearch-breakable-p (car chars)))
	      (setq result (concat result skk-isearch-whitespace-regexp)))
	  (setq result (concat result (char-to-string (car chars)))
		prev (car chars)
		chars (cdr chars)))
	result)
    string))

(defun skk-isearch-mode-message ()
  "Prepend the skk isearch mode string to `isearch-message'.
If the current mode is different from previous, remove it first."
  (let ((mode-string (skk-isearch-mode-string)))
    (if (string= mode-string skk-isearch-last-mode-string)
	nil
      (if (string-match skk-isearch-last-mode-regexp isearch-message)
	  (setq isearch-message (substring isearch-message (match-end 0))))
      (setq skk-isearch-last-mode-string mode-string
	    skk-isearch-last-mode-regexp (concat "^"
						 (regexp-quote mode-string)))
      (setq isearch-message (concat mode-string isearch-message)))))

(defun skk-isearch-process-search-string (string)
  (isearch-process-search-string (skk-isearch-search-string-regexp string)
				 string))


;;
;; interactive functions.
;;
(defun skk-isearch-delete-char (&rest args)
  (interactive "P")
  (or (with-current-buffer (get-buffer-create skk-isearch-working-buffer)
	;; following code is highly depends on internal of skk.
	(if (skk-isearch-conversion-active-p)
	    (prog1
		t
	      (if (string= skk-prefix "")
		  ;; now, we can't pass the universal argument within the
		  ;; isearch-mode.  so hard code the value `1'.
		  (delete-backward-char 1)
		(skk-erase-prefix 'clean))
	      (setq skk-isearch-incomplete-message (buffer-string))
	      (skk-isearch-incomplete-message))))
      (let ((str (skk-isearch-mode-string)))
	(mapcar
	 (function (lambda (cmd)
		     (or (string-match (concat "^" (regexp-quote str))
				       (car (cdr cmd)))
			 (setcdr cmd (cons (concat str (car cmd))
					   (cdr (cdr cmd)))))))
	 isearch-cmds)
	(isearch-delete-char))))

(defun skk-isearch-kakutei (isearch-function)
  "Special wrapper for skk-kakutei or newline."
  (if (with-current-buffer (get-buffer-create skk-isearch-working-buffer)
	;; following code is highly depends on internal of skk.
	(if (skk-isearch-conversion-active-p)
	    (prog1
		t
	      (skk-isearch-skk-kakutei))))
      (skk-isearch-process-search-string (skk-isearch-search-string))
    (funcall isearch-function)))

(defun skk-isearch-exit (&rest args)
  (interactive "P")
  (skk-isearch-kakutei (function isearch-exit)))

(defun skk-isearch-newline (&rest args)
  (interactive "P")
  ;; following code is highly depends on internal of skk.
  (if (with-current-buffer (get-buffer-create skk-isearch-working-buffer)
	(if (memq (skk-isearch-current-mode) '(latin jisx0208-latin))
	    (prog1
		t
	      ;; if the working buffer is latin or jisx0208-latin
	      ;; mode, default behaviour of C-j is set current mode
	      ;; to kana mode.
	      (skk-isearch-turn-on-skk-mode)
	      (skk-isearch-mode-message))))
      (isearch-message)
    (if (event-to-character last-command-event)
	(skk-isearch-kakutei (function isearch-printing-char))
      (skk-isearch-mode-message)
      (isearch-message))))

(defun skk-isearch-skk-mode (&rest args)
  (interactive "P")
  (skk-isearch-redo-function)
  (isearch-message))

(defun skk-isearch-keyboard-quit (&rest args)
  (interactive "P")
  (condition-case ()
      (progn
	(skk-isearch-redo-function)
	;; update echo area message.
	(skk-isearch-search-string))
    (quit (isearch-abort))))

(defun skk-isearch-wrapper (&rest args)
  (interactive "P")
  (skk-isearch-redo-function)
  (let ((string (skk-isearch-search-string)))
    (if (null string)
	;; on the way to converting to kanji.
	nil
      ;; with saving value of old binding...
      (let ((local-map (symbol-value skk-isearch-overriding-local-map))
	    (current-buffer (current-buffer)))
	;; because the overrinding local map may be buffer local, keep the
	;; current buffer, but we can't use save-excursion. ...
	(unwind-protect
	    (progn
	      (set skk-isearch-overriding-local-map isearch-mode-map)
	      (let ((command (key-binding string)))
		(cond ((not (commandp command))
		       ;; just search literally.
		       (skk-isearch-process-search-string string))
		      ;; internationalized isearch.el
		      ((fboundp 'isearch-process-search-multibyte-characters)
		       ;; internationalized isearch.el binds all
		       ;; multibyte characters to `isearch-printing-char'.
		       (cond (
			      ;; STRING is not a multibyte character.
			      (> skk-kanji-len (length string))
			      ;; process a special character, such as *, |, ...
			      (command-execute command))
			     (t (skk-isearch-process-search-string string))))
		      ;; non internationalized isearch.el
		      (t (command-execute command)))))
	  ;; restore the overriding local map.
	  (set-buffer current-buffer)
	  (set skk-isearch-overriding-local-map local-map))))))

;;
;; Advice.
;;

(defadvice isearch-repeat (after skk-isearch-ad activate preactivate)
  "`isearch-message' $B$rE,@Z$K@_Dj$9$k!#(B"
  (unless (string-match (concat "^" (regexp-quote (skk-isearch-mode-string)))
			isearch-message)
    (setq isearch-message
	  (concat
	   (skk-isearch-mode-string)
	   (mapconcat 'isearch-text-char-description isearch-string "")))
    (setq isearch-cmds (cdr isearch-cmds))
    (isearch-push-state)
    (isearch-update)))

(defadvice isearch-edit-string (before skk-isearch-ad activate preactivate)
  "`isearch-message' $B$rE,@Z$K@_Dj$9$k!#(B"
  (when (string-match (concat "^" (regexp-quote (skk-isearch-mode-string)))
		      isearch-message)
    (setq isearch-message (substring isearch-message (match-end 0)))))

(defadvice isearch-search (before skk-isearch-ad activate preactivate)
  "`isearch-message' $B$rE,@Z$K@_Dj$9$k!#(B"
  (unless (or isearch-nonincremental
	      (string-match (concat "^" (regexp-quote (skk-isearch-mode-string)))
			    isearch-message))
    (setq isearch-message
	  (concat
	   (skk-isearch-mode-string)
	   (mapconcat 'isearch-text-char-description isearch-string "")))))

(put 'skk-isearch-wrapper 'isearch-command t)
(put 'skk-isearch-keyboard-quit 'isearch-command t)
(put 'skk-isearch-newline 'isearch-command t)
(put 'skk-isearch-exit 'isearch-command t)
(put 'skk-isearch-delete-char 'isearch-command t)
(put 'isearch-other-control-char 'isearch-command t)
(put 'skk-isearch-skk-mode 'isearch-command t)

(provide 'skk-isearch)

;;; skk-isearch.el ends here
