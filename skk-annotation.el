;;; skk-annotation.el --- SKK annotation $B4XO"%W%m%0%i%`(B

;; Copyright (C) 2000, 2001 NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>

;; Author: NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Version: $Id: skk-annotation.el,v 1.40 2006/01/16 15:23:38 skk-cvs Exp $
;; Keywords: japanese, mule, input method
;; Created: Oct. 27, 2000.
;; Last Modified: $Date: 2006/01/16 15:23:38 $

;; This file is part of Daredevil SKK.

;; Daredevil SKK is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.

;; Daredevil SKK is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Daredevil SKK, see the file COPYING.  If not, write to
;; the Free Software Foundation Inc., 51 Franklin St, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; $B$3$l$O!"(BSKK $B8D?M<-=q$KIU$1$?%"%N%F!<%7%g%s(B ($BCp<a(B) $B$r3hMQ$9$k%W%m%0(B
;; $B%i%`$G$9!#(B
;;
;; <INSTALL>
;;
;; SKK $B$rIaDL$K(B make $B$7$F2<$5$$!#FC$K:n6H$OITMW$G$9!#(B
;;
;;
;; <HOW TO USE>
;;
;;   (setq skk-show-annotation t)
;;
;; $B$H(B ~/.emacs $B$K=q$-$^$7$g$&!#<-=q$N8uJd$K(B `;' $B$+$i;O$^$kJ8;zNs$,$"$l(B
;; $B$P!"$=$N3:Ev$N8uJd$,JQ49$5$l$F%P%C%U%!$K=PNO$5$l$?:]!"(B`;' $B0J9_$r$=(B
;; $B$N8uJd$N%"%N%F!<%7%g%s$H$7$F%(%3!<%(%j%"$KI=<($7$^$9!#(B
;;
;;   (setq skk-annotation-show-as-message nil)
;;
;; $B$H(B ~/.emacs $B$K=q$$$?>l9g$O!"(Bother-window $B$r0l;~E*$K3+$$$F%"%N%F!<%7%g(B
;; $B%s$rI=<($7$^$9!#(Bother-window $B$O$=$N8uJd$K$D$$$F3NDj$9$k$+!"$=$N8uJd(B
;; $B$NA*Br$r;_$a$k(B ($B<!$N8uJd$rA*Br$7$?$j!"(Bquit $B$7$?$j(B) $B$9$k$H<+F0E*$KJD(B
;; $B$8$i$l$^$9!#(B
;;
;; SKK $B$G$O(B 5 $BHVL\$N8uJd0J9_$r%(%3!<%(%j%"$r;H$C$FI=<($7$^$9!#=>$$!"(B5
;; $BHVL\0J9_$N8uJd$K$D$$$F$O!"(Bskk-annotation-show-as-message $B$,(B t $B$G$b(B
;; $B%&%#%s%I%&$r3+$$$F%"%N%F!<%7%g%s$rI=<($7$^$9!#(B
;;
;; $B$"$kC18l$K$D$$$F!"%"%N%F!<%7%g%s$rIU$1$?$$$H$-$O!"3NDj$7$?D>8e$KF1(B
;; $B$8%P%C%U%!$G(B
;;
;;   M-x skk-annotation-add
;;
;; $B$7$^$7$g$&!#%"%N%F!<%7%g%s$rJT=8$9$k%P%C%U%!$,3+$$$F!"%+%l%s%H%P%C(B
;; $B%U%!$K$J$j$^$9$N$G!"$=$3$X%"%N%F!<%7%g%s$rIU$1$^$7$g$&!#(B
;; 1 $B9T$G$"$kI,MW$O$"$j$^$;$s$,!"J#?t9T$N%"%N%F!<%7%g%s$rIU$1$k$H(B echo
;; area $B$XI=<($5$l$?$H$-$KA4BN$,8+$($J$/$J$j$^$9!#(B
;; $B$^$?!"(B`;' $B$NJ8;z<+BN$OF~$l$kI,MW$O$"$j$^$;$s!#(B
;; $B:#$^$G$K4{$KIU$1$F$$$?%"%N%F!<%7%g%s$,$"$l$PJT=8%P%C%U%!$,I=<($5$l(B
;; $B$?$H$-$K$=$N%"%N%F!<%7%g%s$,(B prefix $BE*$K=PNO$5$l$^$9!#4{B8$N%"%N%F!<(B
;; $B%7%g%s$b4^$a$FJT=8$7$F2<$5$$!#%P%C%U%!$N@hF,9T$r=|$$$FA4$F$N9T$,?7(B
;; $B$7$$%"%N%F!<%7%g%s$H$7$F>e=q$-$5$l$^$9!#(B
;; $BJT=8$,=*$o$C$?$i(B C-c C-c $B$7$^$7$g$&!#(B
;;
;; $B>e5-$NF0:n$G%f!<%6$,IU$1$?%"%N%F!<%7%g%s$r!V%f!<%6%"%N%F!<%7%g%s!W(B
;; $B$H8F$S$^$9!#%f!<%6%"%N%F!<%7%g%s$O!"(B
;;
;;   $B!V$-$+$s(B /$B4|4V(B/$B5!4X(B;*$B5!4XEj;q2H(B/$B4p44(B;*$B4p446HL3(B/$B!W(B
;;
;; $B$N$h$&$K(B `;' $B$ND>8e$K(B `*' $B$NJ8;z$,<+F0E*$K?6$i$l$^$9!#$3$l$O%f!<%6(B
;; $B$,FH<+$KIU$1$?%"%N%F!<%7%g%s$G$"$k$3$H$r<($7$^$9(B (`*' $B$NJ8;z$OJQ49(B
;; $B;~$K$OI=<($5$l$^$;$s(B)$B!#(B
;;
;; $B0lJ}!"6&M-<-=q$K85!9IU$1$i$l$F$$$k%"%N%F!<%7%g%s$r!V%7%9%F%`%"%N%F!<(B
;; $B%7%g%s!W$H8F$S!"$3$l$O(B `;' $B$ND>8e$K(B `*' $B$NJ8;z$rH<$J$$$^$;$s!#(B
;; <$BNc(B>
;;    $B!V$$$<$s(B /$B0JA0(B;previous/$B0MA3(B;still/$B!W(B
;;
;; $B%f!<%6%"%N%F!<%7%g%s$H%7%9%F%`%"%N%F!<%7%g%s$r6hJL$9$k$3$H$G!"%f!<(B
;; $B%6%"%N%F!<%7%g%s$@$1$rI=<($7$?$j!"$"$k$$$O$=$N5U$r9T$J$&$3$H$,2DG=(B
;; $B$G$9!#(B`skk-annotation-function' $B$KI=<($7$?$$%"%N%F!<%7%g%s$r(B
;; non-nil $B$HH=Dj$9$k4X?t$r=q$-$^$7$g$&!#$3$s$J46$8$G$9!#(B
;;
;;   (setq skk-annotation-function
;;         (lambda (annotation) (eq (aref annotation 0) ?*)))
;;
;; $B>e5-$NNc$G$O!"%"%N%F!<%7%g%s$N@hF,$,(B `*' $B$G;O$^$k!V%f!<%6%"%N%F!<%7%g(B
;; $B%s!W$N>l9g$K(B t $B$rJV$7$^$9$N$G!"%f!<%6%"%N%F!<%7%g%s$@$1$rI=<($7$^$9!#(B
;;
;; M-x skk-annotation-add $B$7$?$b$N$N!"7k6I%"%N%F!<%7%g%s$rIU$1$:$KCV$-(B
;; $B$?$$$H$-$O!"(B
;;
;;   M-x skk-annotation-kill
;;
;; $B$7$F2<$5$$!#(B
;;
;; $B$^$?!":G8e$K3NDj$7$?8uJd$K$D$$$F$N%"%N%F!<%7%g%s$r<h$j5n$j$?$$$H$-(B
;; $B$O!"(B
;;
;;   M-x skk-annotation-remove
;;
;; $B$7$F2<$5$$!#(B
;; `;' $B$NJ8;z$r4^$s$@8uJd$O!"(Beval $B$9$k$H(B `;' $B$K$J$k(B Lisp $B<0$H$7$F(B
;; quote $B$5$l$F<-=q8uJd$H$7$F<}$a$i$l$J$1$l$P$J$j$^$;$s!#4{B8$N<-=q$K(B
;; $B$D$$$F$O!"<-=q$rFI$_9~$s$@%P%C%U%!$G(B
;;
;;   M-x skk-annotation-update-jisyo-format
;;
;; $B$9$k$3$H$G$3$N:n6H$r9T$J$&$3$H$,$G$-$^$9!#8D?M<-=q!"(BSKK-JISYO.L $B$K(B
;; $B$D$$$F$O@'Hs9T$J$C$F$*$$$?J}$,NI$$$G$7$g$&!#(B
;; SKK Openlab $B$G:#8eG[I[$9$k<-=q$O(B `;' $B$OM=$a(B quote $B$5$l$F$$$k>uBV$K(B
;; $B$7$^$9!#(B
;; $BC"$7!"4{$K%"%N%F!<%7%g%s$,IU$1$i$l$F$$$k>l9g$O!"$3$N%"%N%F!<%7%g%s(B
;; $B<+BN$b8uJd$H6hJL$G$-$:$K(B quote $B$5$l$F$7$^$$$^$9$N$G!"$4Cm0U2<$5$$(B
;; ($B:#$N$H$3$m<j:n6H$G(B quote $B$5$l$J$$$h$&$KB`Hr$9$k$J$I$7$+J}K!$O$"$j(B
;; $B$^$;$s(B)$B!#(B
;;
;; Viper $BBP:v$O$^$@9T$J$C$F$$$^$;$s!#(B~/.viper $B$K<!$N$h$&$K=q$$$F2<$5$$!#(B
;; (viper-harness-minor-mode "skk-annotation")

;;; Code:

(eval-when-compile
  (require 'skk-macs)
  (require 'skk-vars)
  (require 'static))

(unless skk-annotation-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" 'skk-annotation-save-and-quit)
    (define-key map "\C-c\C-k" 'skk-annotation-kill)
    (setq skk-annotation-mode-map map)))

(unless (assq 'skk-annotation-mode minor-mode-alist)
  (setq minor-mode-alist (cons '(skk-annotation-mode " annotation")
			       minor-mode-alist)))

(when (and (boundp 'minor-mode-map-alist)
	   (not (assq 'skk-annotation-mode-map minor-mode-map-alist)))
  (setq minor-mode-map-alist
	(cons (cons 'skk-annotation-mode skk-annotation-mode-map)
	      minor-mode-map-alist)))

;; inline functions.
(defsubst skk-annotation-erase-buffer ()
  (let ((inhibit-read-only t)
	buffer-read-only)
    (static-when
	(fboundp 'set-text-properties)
      (set-text-properties (point-min) (point-max) nil))
    (erase-buffer)))

(defsubst skk-annotation-insert (annotation)
  (with-current-buffer (get-buffer-create skk-annotation-buffer)
    (skk-annotation-erase-buffer)
    (insert annotation)))

(defsubst skk-annotation-get (annotation)
  (or (string= annotation "")
      (if (eq (aref annotation 0) ?*)
	  (substring annotation 1)
	annotation)))

;; advices.

;; functions.
;;;###autoload
(defun skk-annotation-show (annotation)
  (unless skk-kakutei-flag
    (when (or (not skk-annotation-function)
	      (funcall skk-annotation-function annotation))
      (skk-annotation-show-1 (skk-annotation-get annotation)))))

(defun skk-annotation-show-1 (annotation)
  (setq annotation (skk-eval-string annotation))
  (cond
   ((and (eval-when-compile (eq skk-emacs-type 'mule5))
	 window-system
	 skk-show-tooltip)
    (skk-tooltip-show-at-point annotation))
   ((and skk-annotation-show-as-message
	 (not (skk-in-minibuffer-p)))
    (skk-annotation-show-as-message annotation))
   ((and (not (skk-annotation-display-p 'minibuf))
	 (skk-in-minibuffer-p))
    ;; do nothing
    nil)
   (t
    (skk-annotation-show-buffer annotation)))
  ;; $BCm<a$NI=<($O$3$3$^$G$@$,!"$3$3$G%f!<%6$,Cm<a$NFbMF$r%3%T!<$7$?$j(B
  ;; $B$7$FMxMQ$G$-$k$h$&$K$9$k!#(B
  (let* ((copy-command (key-binding skk-annotation-copy-key))
	 (browse-command (key-binding skk-annotation-browse-key))
	 (list (list copy-command browse-command))
	 event command)
    (while (and list
		(condition-case nil
		    (progn
		      (setq event (next-command-event)
			    command (key-binding (skk-event-key event)))
		      ;; Return value of the following expression is important.
		      (memq command list))
		  (quit
		   nil)))
      (cond ((eq command copy-command)
	     (setq list (delq copy-command list))
	     (kill-new (substring-no-properties annotation))
	     (skk-message "$B8=:_$NCm<a$r%3%T!<$7$^$7$?(B"
			  "Copying the current note...done")
	     (setq event nil))
	    ((eq command browse-command)
	     (setq list (delq browse-command list))
	     (browse-url annotation)
	     (skk-message "$B8=:_$NCm<a$r(B URL $B$H$7$F%V%i%&%:$7$F$$$^$9(B..."
			  "Browsing the current note as URL...")
	     (setq event nil))
	    (t
	     (setq list nil))))
    (when event
      (skk-unread-event event))))

(defun skk-annotation-show-buffer (annotation)
  (condition-case nil
      (save-window-excursion
	(let ((minibuf-p (skk-in-minibuffer-p))
	      event window)
	  (skk-annotation-insert annotation)
	  (cond
	   (minibuf-p
	    (if (setq window (get-buffer-window (skk-minibuffer-origin)))
		(select-window window)
	      (other-window 1))
	    (unless (eq (next-window) (selected-window))
	      (delete-other-windows)))
	   (t
	    (split-window-vertically)))
	  (display-buffer skk-annotation-buffer)
	  (when minibuf-p
	    (select-window (minibuffer-window)))
	  (setq event (next-command-event))
	  (when (skk-key-binding-member
		 (skk-event-key event)
		 '(key-board-quit
		   skk-kanagaki-bs
		   skk-kanagaki-esc)
		 skk-j-mode-map)
	    (signal 'quit nil))
	  (skk-unread-event event)))
    (quit
     ;; skk-previous-candidate $B$X(B
     (setq skk-henkan-count 0)
     (skk-unread-event
      (character-to-event
       (aref
	(car (where-is-internal 'skk-previous-candidate
				skk-j-mode-map))
	0))))))

(defun skk-annotation-show-as-message (annotation)
  (message "%s" annotation))

(defun skk-annotation-setup ()
  (let ((skk-henkan-key (skk-get-last-henkan-datum 'henkan-key))
	(skk-okuri-char (skk-get-last-henkan-datum 'okuri-char))
	(cand (car (skk-get-last-henkan-datum 'henkan-list)))
	word)
    (unless cand
      (setq skk-henkan-key
	    (read-from-minibuffer "Midasi: "))
      (when (string= skk-henkan-key "")
	(skk-error "$B%"%N%F!<%7%g%s$9$kC18l$,$"$j$^$;$s(B"
		   "No word to be annotated"))
      (when (string-match "\\cj\\([a-z]+\\)$"
			  skk-henkan-key)
	(setq skk-okuri-char (match-string 1 skk-henkan-key)
	      ;; $BAw$j$"$jJQ49$r;XDj$9$k$H(B
	      ;; skk-henkan-okurigana $B$N;XDj$K:$$k!#(B
	      skk-henkan-okurigana ""))
      (setq cand
	    (prog1
		(skk-henkan-in-minibuff)
	      (setq skk-kakutei-flag nil))))
    ;; $B$3$N;~E@$G$O(B skk-num-list $B$O4{$K(B nil
    ;; $B%_%K%P%C%U%!$+$iBP>]$r;XDj$7$?>l9g$K$O(B consp $B$K$J$i$J$$(B
    (when (consp cand)
      (setq cand (car cand)))
    (setq word (car (skk-treat-strip-note-from-word cand)))
    (when (and (string-match "[0-9]" skk-henkan-key)
	       (or (string-match "#[0-9]" word)
		   (skk-lisp-prog-p word)))
      (setq skk-henkan-key
	    (skk-num-compute-henkan-key skk-henkan-key)))
    (setq skk-annotation-target-data
	  (list skk-henkan-key
		skk-okuri-char
		cand))
    ;; $B0U?^$rM}2r$7$F$J$$$,!"(Bskk-kakutei-initialize $B$N$[$&$,E,@Z$J5$$b(B
    (skk-kakutei)))

;;;###autoload
(defun skk-annotation-add (&optional no-previous-annotation)
  "$B:G8e$K3NDj$7$?8l$K(B annotation $B$rIU$1$k!#(B
$B4{$KIU$1$i$l$F$$$k(B annotation $B$,$"$l$P$=$l$rJT=8%P%C%U%!$K=PNO$9$k!#(B
no-previous-annotation $B$r;XDj$9$k$H(B \(C-u M-x skk-annotation-add $B$G;XDj2D(B\)
$B4{$KIU$1$i$l$F$$$k(B annotation $B$rJT=8%P%C%U%!$K=PNO$7$J$$!#(B"
  (interactive "P")
  (save-match-data
    (skk-kakutei)
    (skk-annotation-setup)
    (let* ((plist (append
		   '(intangible t read-only t)
		   (static-if (eq skk-emacs-type 'xemacs)
		       '(start-closed t end-open t)
		     '(front-sticky t rear-nonsticky t))))
	   (wholestring (nth 2 skk-annotation-target-data))
	   (realword (if (and wholestring
			      (string-match ";\\*?" wholestring))
			 (substring wholestring 0 (match-beginning 0))
		       wholestring))
	   (annotation (if (and realword
				(string-match ";\\*?" wholestring))
			   (substring wholestring (match-end 0))
			 nil)))
      (setq skk-annotation-original-window-configuration
	    (current-window-configuration))
      (delete-other-windows)
      (split-window-vertically)
      (other-window 1)
      (switch-to-buffer (get-buffer-create skk-annotation-buffer))
      (setq buffer-read-only nil
	    skk-annotation-mode t)
      (skk-annotation-erase-buffer)
      (insert
       (format "\
;; Add a note to word `%s' (this line will not be added to the note.)
"
	       realword))
      (static-if (fboundp 'set-text-properties)
	  (add-text-properties (point-min) (1- (point)) plist))
      (when (and (not no-previous-annotation)
		 annotation)
	(insert annotation))
      (run-hooks 'skk-annotation-mode-hook)
      (message "%s to save edits, %s to just kill this buffer"
	       (mapconcat 'key-description
			  (where-is-internal 'skk-annotation-save-and-quit
					     skk-annotation-mode-map)
			  ", ")

	       (mapconcat 'key-description
			  (where-is-internal 'skk-annotation-kill
					     skk-annotation-mode-map)
			  ", ")))))

(defun skk-annotation-save-and-quit (&optional quiet)
  "$B:G8e$K3NDj$7$?8l$K(B annotation $B$rIU$1$F(B annotation $B%P%C%U%!$rJD$8$k!#(B"
  ;; called in the annotation buffer.
  (interactive "P")
  (let (annotation)
    (save-match-data
      (with-current-buffer (get-buffer-create skk-annotation-buffer)
	(goto-char (point-min))
	(when (looking-at ";; Add a note to word") ; $BCfESH>C<(B
	  (forward-line 1)
	  (beginning-of-line))
	(setq annotation (buffer-substring-no-properties
			  (point) (point-max)))
	(when (string-match "^[\t\n $B!!(B]+" annotation)
	  (setq annotation (substring annotation (match-end 0))))
	(when (string-match "[\t\n $B!!(B]+$" annotation)
	  (setq annotation (substring annotation 0 (match-beginning 0))))
	(when (string= annotation "")
	  (setq annotation nil))
	(setq annotation (skk-quote-char annotation))))
    (if annotation
	(skk-annotation-last-word-1
	 (lambda (beg end)
	   (goto-char beg)
	   (when (re-search-forward ";[^/]*" end t)
	     (delete-region (match-beginning 0) (match-end 0)))
	   (goto-char end)
	   (insert ";*" annotation)))
      ;; $B:o=|$7$?;~(B
      (let ((old-annotation
	     (cdr (skk-treat-strip-note-from-word
		   (nth 2 skk-annotation-target-data)))))
	(when (and old-annotation
		   (yes-or-no-p
		    (format (if skk-japanese-message-and-error
				"$B4{B8$N%"%N%F!<%7%g%s(B `%s' $B$r:o=|$7$^$9$+!)(B "
			      "Delete old annotation `%s' ? ")
			    (skk-annotation-get old-annotation))))
	  (skk-annotation-last-word-1
	   (lambda (beg end)
	     (goto-char beg)
	     (when (re-search-forward ";[^/]*" end t)
	       (delete-region (match-beginning 0) (match-end 0))))))))
    (skk-annotation-erase-buffer)
    (kill-buffer (current-buffer))
    (set-window-configuration
     skk-annotation-original-window-configuration)
    (when annotation
      (unless quiet
	(message "%s" "Added annotation")))))

(defun skk-annotation-kill ()
  "annotation $B$rIU$1$:$K(B annotation $B%P%C%U%!$r(B kill $B$9$k!#(B"
  ;; called in the annotation buffer.
  (interactive)
  (skk-annotation-erase-buffer)
  (kill-buffer (current-buffer))
  (set-window-configuration
   skk-annotation-original-window-configuration))

;;;###autoload
(defun skk-annotation-remove ()
  "$B:G8e$K3NDj$7$?8l$+$i(B annotation $B$r<h$j5n$k!#(B"
  (interactive)
  (save-match-data
    (skk-kakutei)
    (skk-annotation-setup)
    (when (yes-or-no-p
	   (format (if skk-japanese-message-and-error
		       "%s $B$K$D$$$F$N%"%N%F!<%7%g%s$r:o=|$7$^$9$+!)(B "
		     "Really delete annotation for %s? ")
		   (nth 2 skk-annotation-target-data)))
      (skk-annotation-last-word-1
       (lambda (beg end)
	 (goto-char beg)
	 (when (re-search-forward ";[^/]*" end t)
	   (delete-region (match-beginning 0) (match-end 0))))))))

;;;###autoload
(defun skk-annotation-display-p (test)
  (cond ((eq skk-show-annotation nil)
	 nil)
	((and (listp skk-show-annotation)
	      (eq (car skk-show-annotation) 'not)
	      ;; (not ...)
	      (memq test skk-show-annotation))
	 ;; (not list), (not minibuf) or (not list minibuf)
	 nil)
	(t
	 ;; non-nil
	 t)))

;;;###autoload
(defun skk-annotation-toggle-display-p ()
  (interactive)
  (cond ((eq skk-show-annotation nil)
	 ;; do nothing
	 nil)
	((and (listp skk-show-annotation)
	      (eq (car skk-show-annotation) 'not))
	 ;; (not ...)
	 (cond ((memq 'list skk-show-annotation)
		(if (eq (length skk-show-annotation) 2)
		    ;; (not list) -> t  i.e. turn on
		    (setq skk-show-annotation t)
		  ;; (not list minibuf) -> (not minibuf)
		  (setq skk-show-annotation '(not minibuf))))
	       (t
		;; (not minibuf) -> (not list minibuf)  i.e. turn off
		(setq skk-show-annotation '(not list minibuf)))))
	(t
	 ;; non-nil -> (not list)  i.e. turn off
	 (setq skk-show-annotation '(not list)))))

(defun skk-annotation-last-word-1 (function)
  ;; funcall FUNCTION with BEG and END where BEG and END are markers.
  (let ((inhibit-quit t)
	(jisyo-buffer (skk-get-jisyo-buffer skk-jisyo 'nomsg))
	(word (nth 2 skk-annotation-target-data))
	(beg (make-marker))
	(end (make-marker))
	(eol (make-marker))
	pattern)
    (when (buffer-live-p jisyo-buffer)
      (save-match-data
	(with-current-buffer jisyo-buffer
	  (goto-char (if (nth 1 skk-annotation-target-data)
			 skk-okuri-ari-min
		       skk-okuri-nasi-min))
	  (when (re-search-forward
		    (concat "^\\("
			    (regexp-quote (car skk-annotation-target-data))
			    "\\) /")
		    (if (nth 1 skk-annotation-target-data)
			skk-okuri-ari-max nil)
		    t nil)
	    (goto-char (match-beginning 1))
	    (set-marker eol (skk-save-point (end-of-line) (point)))
	    (when (string-match ";" word)
	      (setq word (substring word 0 (match-beginning 0))))
	    (when (re-search-forward
		   (concat "/\\(" word "\\)\\(;[^/]*\\)*/")
		   eol t nil)
	      (set-marker beg (match-beginning 1))
	      (set-marker end (or (match-end 2) (match-end 1)))
	      (funcall function beg end)
	      (when (nth 1 skk-annotation-target-data)
		(goto-char end)
		;; skip other candidates that has not a okuirigana.
		(search-forward "/[" eol t nil)
		(setq pattern (concat "/\\(" word "\\)\\(;[^/]*\\)*/"))
		(while (re-search-forward pattern eol t nil)
		  (set-marker beg (match-beginning 1))
		  (set-marker end (or (match-end 2)
				      (match-end 1)))
		  (funcall function beg end)))
	      (set-marker beg nil)
	      (set-marker end nil)
	      (set-marker eol nil))))))))

;;;###autoload
(defun skk-annotation-quote (&optional quiet)
  "$B:G8e$K3NDj$7$?8l$K4^$^$l$k(B `;' $B$r8uJd$N0lIt$H$7$F(B quote $B$9$k!#(B"
  (interactive "P")
  (skk-kakutei)
  (skk-annotation-setup)
  (let (candidate)
    (skk-annotation-last-word-1
     (lambda (beg end)
       (goto-char beg)
       (setq candidate (buffer-substring-no-properties beg end))
       (when (string-match ";" candidate)
	 (delete-region beg end)
	 (insert (skk-quote-semicolon candidate))
	 (unless quiet
	   (message "%s" "Quoted")))))))

;;;###autoload
(defun skk-annotation-update-jisyo-format ()
  (interactive)
  (skk-setup-jisyo-buffer)
  (let ((min skk-okuri-ari-min) (max skk-okuri-ari-max))
    (skk-annotation-update-jisyo-format-1 min max)
    (setq min skk-okuri-nasi-min
	  max (point-max))
    (skk-annotation-update-jisyo-format-1 min max)))

(defun skk-annotation-update-jisyo-format-1 (min max)
  (let (candidate)
    (goto-char min)
    (while (re-search-forward "\\/\\([^\n/]*;[^\n/]*\\)\\/" max t nil)
      (setq candidate (buffer-substring-no-properties
		       (match-beginning 1) (match-end 1)))
      (delete-region (match-beginning 1) (match-end 1))
      (goto-char (match-beginning 1))
      (insert
       (concat "(concat \""
	       (mapconcat
		(function
		 (lambda (c)
		   (if (eq c ?\;)
		       "\\073"
		     (char-to-string c))))
		(append candidate nil) "")
	       "\")")))))

(require 'product)
(product-provide
    (provide 'skk-annotation)
  (require 'skk-version))

;;; skk-annotation.el ends here
