;;; skk-annotation.el --- SKK annotation $B4XO"%W%m%0%i%`(B
;; Copyright (C) 2000 Mikio Nakajima <minakaji@osaka.email.ne.jp>

;; Author: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Version: $Id: skk-annotation.el,v 1.1.2.2 2000/10/28 01:48:37 minakaji Exp $
;; Keywords: japanese
;; Created: Oct. 27, 2000.
;; Last Modified: $Date: 2000/10/28 01:48:37 $

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
    (skk-annotation-insert annotation)
    (message annotation)
    (sit-for 1)))

;;;###autoload
(defun skk-annotation-add (&optional no-previous-annotation)
  "$B:G8e$K3NDj$7$?8l$K(B annotation $B$rIU$1$k!#(B"
  (interactive "P")
  (save-match-data
    (skk-kakutei)
    (let ((word (skk-get-last-henkan-datum 'henkan-list)))
      (or word (skk-error "$B3NDj$7$?>pJs$,$"$j$^$;$s(B" "No kakutei information"))
      (setq skk-annotation-original-window-configuration
	    (current-window-configuration))
      (delete-other-windows)
      (split-window-vertically)
      (other-window 1)
      (switch-to-buffer (get-buffer-create skk-annotation-buffer))
      (setq buffer-read-only nil
	    skk-annotation-mode t)
      (erase-buffer)
      (if (and (not no-previous-annotation)
	       (string-match ";\\**" (car word)))
	  (insert (substring (car word) (match-end 0))))
      (run-hooks 'skk-annotation-mode-hook)
      (message "%s to save edits"
	       (mapconcat 'key-description
			  (where-is-internal 'skk-annotation-save-and-quit
					     skk-annotation-mode-map)
			  ", ")))))

(defun skk-annotation-save-and-quit (&optional quiet)
  "$B:G8e$K3NDj$7$?8l$K(B annotation $B$rIU$1$F(B annotation $B%P%C%U%!$rJD$8$k!#(B"
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
	   (setq end (set-marker (make-marker) end))
	   (goto-char beg)
	   (if (re-search-forward ";[^/]*" end t)
	       (delete-region (match-beginning 0) (match-end 0)))
	   (goto-char end)
	   (set-marker end nil)
	   (insert ";*" annotation))))
    (set-window-configuration
     skk-annotation-original-window-configuration)
    (or quiet (message "Added annotation"))))

;;;###autoload
(defun skk-annotation-remove (&optional no-previous-annotation)
  "$B:G8e$K3NDj$7$?8l$+$i(B annotation $B$r<h$j5n$k!#(B"
  (interactive "P")
  (save-match-data
    (skk-kakutei)
    (let ((word (skk-get-last-henkan-datum 'henkan-list)))
      (or word (skk-error "$B3NDj$7$?>pJs$,$"$j$^$;$s(B" "No kakutei information"))
	(skk-annotation-last-word-1 
	 (lambda (beg end)
	   (setq end (set-marker (make-marker) end))
	   (goto-char beg)
	   (if (re-search-forward ";[^/]*" end t)
	       (delete-region (match-beginning 0) (match-end 0)))
	   (goto-char end)
	   (set-marker end nil))))))

(defun skk-annotation-last-word-1 (function)
  (let ((jisyo-buffer (skk-get-jisyo-buffer skk-jisyo 'nomsg))
	(inhibit-quit t)
	candidate beg end)
    (if (not jisyo-buffer)
	nil
      (save-match-data
	(with-current-buffer jisyo-buffer
	  (goto-char (if (skk-get-last-henkan-datum 'okuri-char)
			 skk-okuri-ari-min skk-okuri-nasi-min))
	  (if (not (search-forward " /" nil t nil))
	      nil 
	    (setq beg (point)
		  end (progn (search-forward "/") (1- (point))))
	    (funcall function beg end)))))))

;;;###autoload
(defun skk-annotation-quote (&optional quiet)
  "$B:G8e$K3NDj$7$?8l$K4^$^$l$k(B `;' $B$r8uJd$N0lIt$H$7$F(B quote $B$9$k!#(B"
  (interactive "P")
  (skk-kakutei)
  (if (skk-get-last-henkan-datum 'henkan-list)
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
		     (message "Quoted")))))))))

(require 'product)
(product-provide (provide 'skk-annotation) (require 'skk-version))
;;; end of skk-annotation.el.
