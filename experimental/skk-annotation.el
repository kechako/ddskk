;;; skk-annotation.el --- SKK annotation $B4XO"%W%m%0%i%`(B
;; Copyright (C) 2000 Mikio Nakajima <minakaji@osaka.email.ne.jp>

;; Author: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Version: $Id: skk-annotation.el,v 1.2 2000/10/30 22:18:14 minakaji Exp $
;; Keywords: japanese
;; Created: Oct. 27, 2000.
;; Last Modified: $Date: 2000/10/30 22:18:14 $

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
;; along with Daredevil SKK, see the file COPYING.  If not, write to the
;; Free Software Foundation Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.
;;
;;; Commentary:
;;
;; $B!V%f!<%6!<%"%N%F!<%7%g%s!W$H$O(B `;' $B$ND>8e$K(B `*' $B$NJ8;z$rH<$&%"%N%F!<%7%g%s(B
;; $B$G!"%f!<%6$,FH<+$KIU$1$?$b$N$G$"$k$3$H$r<($7$^$9!#(B
;; <$BNc(B>
;;   $B!V$-$+$s(B /$B4|4V(B/$B5!4X(B;*$B5!4XEj;q2H(B/$B4p44(B;*$B4p446HL3(B/$B!W(B
;;
;;
;; $B!V%7%9%F%`%"%N%F!<%7%g%s!W$H$O(B `;' $B$ND>8e$K(B `*' $B$NJ8;z$rH<$o$J$$%"%N%F!<%7(B
;; $B%g%s$G!"%7%9%F%`$,85!9IU$7$F$$$k$b$N$G$"$k$3$H$r<($7$^$9!#(B
;; <$BNc(B>
;;    $B!V$$$<$s(B /$B0JA0(B;previous/$B0MA3(B;still/$B!W(B
;;
;; Viper $BBP:v$O$^$@!#(B.viper $B$K<!$N$h$&$K=q$$$F2<$5$$!#(B
;; (viper-harness-minor-mode "skk-annotation")
;;
;;; Code:
(eval-when-compile (require 'skk-macs) (require 'skk-vars))

(if skk-annotation-mode-map
    nil
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" 'skk-annotation-save-and-quit)
    (define-key map "\C-c\C-k" 'skk-annotation-quit)
    (setq skk-annotation-mode-map map)))

(or (assq 'skk-annotation-mode minor-mode-alist)
    (setq minor-mode-alist (cons '(skk-annotation-mode " annotation")
				 minor-mode-alist)))
(or (assq 'skk-annotation-mode-map minor-mode-map-alist)
    (setq minor-mode-map-alist
	  (cons (cons 'skk-annotation-mode skk-annotation-mode-map)
		minor-mode-map-alist)))

;; inline functions.
(defsubst skk-annotation-insert (annotation)
  (with-current-buffer (get-buffer-create skk-annotation-buffer)
    (let (buffer-read-only)
      (erase-buffer)
      (insert (eval annotation)))))

(defsubst skk-annotation-quote-1 (word)
  (concat "(concat \""
	  (mapconcat
	   (function (lambda (c) (if (eq c ?\;) "\\073" (char-to-string c))))
	   (append word nil) "")
	  "\")"))

;; advices.
(defadvice skk-nunion (around skk-annotation-ad activate)
  (save-match-data
    (let* ((var ad-do-it) (tmp var))
      (while tmp
	(if (string-match ";" (car tmp))
	    (setq tmp (delete (substring (car tmp) 0 (match-beginning 0)) tmp)))
	(setq tmp (cdr tmp)))
      var)))

;; functions.
;;;###autoload
(defun skk-annotation-show (annotation)
  (cond ((not skk-annotation-function)
	 (skk-annotation-show-1 annotation))
	((funcall skk-annotation-function)
	 (skk-annotation-show-1 annotation))))
    
(defun skk-annotation-show-1 (annotation)
  (if (eq (aref annotation 0) ?*)
      (setq annotation (substring annotation 1)))
   (if skk-annotation-show-message
      (skk-annotation-show-message annotation)
    (skk-annotation-show-buffer annotation)))

(defun skk-annotation-show-buffer (annotation)
  (save-window-excursion
    (let (event)
      (skk-annotation-insert annotation)
      (split-window-vertically)
      (display-buffer skk-annotation-buffer)
      (setq event (skk-read-event))
      (skk-unread-event event))))

(defun skk-annotation-show-message (annotation)
  (if (> skk-henkan-count 3)
      nil
    (message annotation)))

(defun skk-annotation-setup ()
  (if (skk-get-last-henkan-datum 'henkan-list)
      (setq skk-annotation-annotated-word
	    (list 
	     (skk-get-last-henkan-datum 'henkan-key)
	     (skk-get-last-henkan-datum 'okuri-char)
	     (skk-get-last-henkan-datum 'henkan-list)))
    (setq skk-henkan-key
	  (read-from-minibuffer
	   "Midasi: " nil
	   (static-when (memq skk-emacs-type '(nemacs mule1))
	     (with-current-buffer
		 (get-buffer-create
		  (format " *Minibuf-%d*" (minibuffer-depth)))
	       (skk-j-mode-on))
	     (append skk-j-mode-map (cdr minibuffer-local-map)))))
    (if (not skk-henkan-key)
	(skk-error "$B%"%N%F!<%7%g%s$rIU$1$kC18l$,$"$j$^$;$s(B"
		   "No annotated word")
      (setq skk-annotation-annotated-word
	    (list skk-henkan-key
		  (if (string-match "^[^a-zA-Z]+\\([a-z]+\\)$" skk-henkan-key)
		      (setq skk-okuri-char
			    (substring skk-henkan-key (match-beginning 1))
			    ;; $BAw$j$"$jJQ49$r;XDj$9$k$H(B skk-henkan-okurigana $B$N;XDj$K:$$k!#(B
			    skk-henkan-okurigana ""))
		  (list (skk-henkan-in-minibuff))))
      (skk-kakutei))))

;;;###autoload
(defun skk-annotation-add (&optional no-previous-annotation)
  "$B:G8e$K3NDj$7$?8l$K(B annotation $B$rIU$1$k!#(B
$B4{$KIU$1$i$l$F$$$k(B annotation $B$,$"$l$P$=$l$rJT=8%P%C%U%!$K=PNO$9$k!#(B
no-previous-annotation $B$r;XDj$9$k$H(B \(C-u M-x skk-annotation-add $B$G;XDj2D(B\)
$B4{$KIU$1$i$l$F$$$k(B annotation $B$rJT=8%P%C%U%!$K=PNO$7$J$$!#(B"
  (interactive "P")
  (save-match-data
    (skk-kakutei)
    (let ((last-henkan-data skk-last-henkan-data))
      (skk-annotation-setup)
      (setq skk-annotation-original-window-configuration
	    (current-window-configuration))
      (delete-other-windows)
      (split-window-vertically)
      (other-window 1)
      (switch-to-buffer (get-buffer-create skk-annotation-buffer))
      (setq buffer-read-only nil
	    skk-annotation-mode t
	    ;; copy buffer local variable of current buffer to annotation buffer.
	    ;; annotation buffer $B$GJL$NJQ49!"3NDj$r$9$k$H>e=q$-$5$l$F$7$^$&(B...$B!#(B
	    ;;skk-last-henkan-data last-henkan-data
	    )
      (erase-buffer)
      (if (and (not no-previous-annotation)
	       (string-match
		";\\**"
		(car (nth 2 skk-annotation-annotated-word))))
	  (insert (substring (car (nth 2 skk-annotation-annotated-word))
			     (match-end 0))))
      (run-hooks 'skk-annotation-mode-hook)
      (message "%s to save edits, %s to just kill this buffer"
	       (mapconcat 'key-description
			  (where-is-internal 'skk-annotation-save-and-quit
					     skk-annotation-mode-map)
			  ", ")

	       (mapconcat 'key-description
			  (where-is-internal 'skk-annotation-quit
					     skk-annotation-mode-map)
			  ", ")))))

(defun skk-annotation-save-and-quit (&optional quiet)
  "$B:G8e$K3NDj$7$?8l$K(B annotation $B$rIU$1$F(B annotation $B%P%C%U%!$rJD$8$k!#(B"
  ;; called in the annotation buffer.
  (interactive "P")
  (let (annotation)
    (save-match-data
      (with-current-buffer (get-buffer-create skk-annotation-buffer)
	(setq annotation (buffer-substring-no-properties
			  (point-min) (point-max)))
	(if (string-match "[\t\n ]+$" annotation)
	    (setq annotation (substring annotation 0 (match-beginning 0))))
	(setq annotation (skk-quote-char annotation))))
    (if annotation
	(skk-annotation-last-word-1 
	 (lambda (beg end)
	   (goto-char beg)
	   (if (re-search-forward ";[^/]*" end t)
	       (delete-region (match-beginning 0) (match-end 0)))
	   (goto-char end)
	   (insert ";*" annotation))))
    (set-window-configuration
     skk-annotation-original-window-configuration)
    (or quiet (message "Added annotation"))))

(defun skk-annotation-kill ()
  "annotation $B$rIU$1$:$K(B annotation $B%P%C%U%!$r(B kill $B$9$k!#(B"
  ;; called in the annotation buffer.
  (interactive)
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
    (skk-annotation-last-word-1 
     (lambda (beg end)
       (goto-char beg)
       (if (re-search-forward ";[^/]*" end t)
	   (delete-region (match-beginning 0) (match-end 0)))))))

(defun skk-annotation-last-word-1 (function)
  ;; funcall FUNCTION with BEG and END where BEG and END are markers.
  (let ((inhibit-quit t)
	(jisyo-buffer (skk-get-jisyo-buffer skk-jisyo 'nomsg))
	(word (car (nth 2 skk-annotation-annotated-word)))
	(beg (make-marker)) (end (make-marker))
	(eol (make-marker))
	candidate pattern)
    (if (not jisyo-buffer)
	nil
      (save-match-data
	(with-current-buffer jisyo-buffer
	  (goto-char (if (nth 1 skk-annotation-annotated-word)
			 skk-okuri-ari-min skk-okuri-nasi-min))
	  (if (not (re-search-forward 
		    (concat "^\\("
			    (regexp-quote (car skk-annotation-annotated-word))
			    "\\) /")
		    (if (nth 1 skk-annotation-annotated-word)
			skk-okuri-ari-max nil)
		    t nil))
	      nil 
	    (goto-char (match-beginning 1))
	    (set-marker eol (skk-save-point (end-of-line) (point)))
	    (if (string-match ";" word)
		(setq word (substring word 0 (match-beginning 0))))
	    (if (not (re-search-forward
		      (concat "/\\(" word "\\)\\(;[^/]*\\)*/")
		      eol t nil))
		nil
	      (set-marker beg (match-beginning 1))
	      (set-marker end (or (match-end 2) (match-end 1)))
	      (funcall function beg end)
	      (if (not (nth 1 skk-annotation-annotated-word))
		  nil
		(goto-char end)
		;; skip other candidates that has not a okuirigana.
		(search-forward "/[" eol t nil)
		(setq pattern (concat "/\\(" word "\\)\\(;[^/]*\\)*/"))
		(while (re-search-forward pattern eol t nil)
		  (set-marker beg (match-beginning 1))
		  (set-marker end (or (match-end 2) (match-end 1)))
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
       (if (string-match ";" candidate)
	   (progn
	     (delete-region beg end)
	     (insert (skk-annotation-quote-1 candidate))
	     (or quiet
		 (message "Quoted"))))))))

(require 'product)
(product-provide (provide 'skk-annotation) (require 'skk-version))
;;; end of skk-annotation.el.
