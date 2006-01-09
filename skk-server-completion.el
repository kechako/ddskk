;;; skk-server-completion.el --- server completion $B$N%/%i%$%"%s%H(B
;;
;; Copyright (C) 2005 Fumihiko MACHIDA <machida@users.sourceforge.jp>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
;; 02110-1301, USA

;;; Commentary:

;; Server completion $B$KBP1~$7$?<-=q%5!<%P$rMQ$$8+=P$78l$+$i;O$^$kA4$F$N(B
;; $B8l6g$N8!:w$r9T$J$$$^$9!#(B

;; $B$3$N%W%m%0%i%`$O0J2<$N(B 2 $B$D$N5!G=$rDs6!$7$^$9!#(B
;;
;; * skk-look $B$NF|K\8lHG!#FI$_$N:G8e$K(B `~' $B$rIU$1$FJQ49$9$k$H!"$=$NFI$_$+(B
;;   $B$i;O$^$kA4$F$N8uJd$rI=<($7$^$9!#(B
;;
;; $BNc!'(B
;;
;; $B"&$^$A$@(B~
;; ==> "$B$^$A$@(B" "$BD.ED(B" "$B$^$A$@$($-(B" "$BD.ED1X(B" "$B$^$A$@$*$@$-$e$&(B" "$BD.ED>.ED5^(B" ..
;;
;; * skk-comp $B$G!"(Bserver completion $B$r;HMQ(B
;;
;; $BNc!'(B
;;
;; $B"&$^$A$@(B-!- $B$G(B Tab $B$r2!$9$H!""&$^$A$@$($-(B $B"*(B $B"&$^$A$@$*$@$-$e$&(B $B!D!D(B
;; $B$H$J$j$^$9!#(B

;; [$B@_DjJ}K!(B]
;;
;; .skk $B$K!"0J2<$rDI2C$7$^$9!#(B
;;
;; (require 'skk-server-completion)
;; (add-to-list 'skk-search-prog-list
;;	     '(skk-server-completion-search) t)
;;
;; $B$^$?!"(B`~' $B$rIU$1$?JQ497k2L$r8D?M<-=q$K3X=,$7$F$7$^$&$N$r$d$a$k$?$a$K$O(B
;; $B0J2<$rDI2C$7$F$/$@$5$$!#(B
;;
;; (add-hook 'skk-search-excluding-word-pattern-function
;;	  #'(lambda (kakutei-word)
;;	      (string-match (format "%s$"
;;				    (regexp-quote
;;				     (char-to-string
;;				      skk-server-completion-search-char)))
;;			    skk-henkan-key)))

;;; Code:

(require 'skk)
(require 'skk-comp)


;;;###autoload
(defun skk-server-completion-search ()
  "$B%5!<%P!<%3%s%W%j!<%7%g%s$r9T$$!"F@$i$l$?3F8+=P$7$G$5$i$K8!:w$9$k!#(B
$BAw$jM-$jJQ49$K$OHsBP1~!#(B"
  (when (and (eq (aref skk-henkan-key (1- (length skk-henkan-key)))
		 skk-server-completion-search-char)
	     (not (or skk-henkan-okurigana
		      skk-okuri-char)))
    (let ((key (substring skk-henkan-key
			  0 (1- (length skk-henkan-key))))
	  midasi-list)
      (when skk-use-numeric-conversion
	(setq key (skk-num-compute-henkan-key key)))
      (setq midasi-list (skk-server-completion-search-midasi key))
      (skk-server-completion-search-recursive midasi-list))))

(defun skk-server-completion-search-midasi (key)
  "server completion $B$rMxMQ$7$F!"(Bkey $B$+$i;O$^$k$9$Y$F$N8+=P$78l$N%j%9%H$rJV5Q$9$k!#(B"
  (when (skk-server-live-p (skk-open-server))
    (with-current-buffer skkserv-working-buffer
      (let ((cont t)
	    (count 0)
	    l)
	(erase-buffer)
	(process-send-string skkserv-process (concat "4" key " "))
	(while (and cont (skk-server-live-p))
	  (accept-process-output)
	  (setq count (1+ count))
	  (when (> (buffer-size) 0)
	    (if (eq (char-after 1) ?1)	;?1
		;; found key successfully, so check if a whole line
		;; is received.
		(when (eq (char-after (1- (point-max)))
			  ?\n)		;?\n
		  (setq cont nil))
	      ;; not found or error, so exit
	      (setq cont nil))))
	(goto-char (point-min))
	(when skk-server-report-response
	  (skk-message "%d $B2s(B SKK $B%5!<%P!<$N1~EzBT$A$r$7$^$7$?(B"
		       "Waited for server response %d times"
		       count))
	(when (eq (following-char) ?1)	;?1
	  (forward-char 2)
	  (car (skk-compute-henkan-lists nil)))))))

(defun skk-server-completion-search-recursive (midasi-list)
  "`midasi-list' $B$N8+=P$7$r:FJQ49$9$k(B"
  (let (result-list kouho-list)
    (dolist (skk-henkan-key midasi-list)
      (setq kouho-list (cons skk-henkan-key (skk-search-server-1 nil nil))
	    result-list (nconc result-list kouho-list)))
    result-list))

;;;###autoload
(defun skk-comp-by-server-completion ()
  ;; skk-comp-prefix $B$O;H$($J$$(B
  ;; $B:#$N$H$3$mHs?tCMJQ49$N$_(B
  (when skk-comp-first
    (setq skk-server-completion-words
	  (skk-server-completion-search-midasi skk-comp-key))
    (when (string= skk-comp-key
		   (car skk-server-completion-words))
      (pop skk-server-completion-words)))
  (pop skk-server-completion-words))

(provide 'skk-server-completion)


;;; skk-server-completion.el ends here
