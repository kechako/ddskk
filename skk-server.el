;;; skk-server.el --- SKK $B%5!<%P!<$N$?$a$N%W%m%0%i%`(B

;; Copyright (C) 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996,
;;               1997, 1998, 1999, 2000
;;   Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>

;; Author: Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Version: $Id: skk-server.el,v 1.15 2001/11/17 05:40:16 czkmt Exp $
;; Keywords: japanese, mule, input method
;; Last Modified: $Date: 2001/11/17 05:40:16 $

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
;;
;;; Code:
(eval-when-compile (require 'skk-macs) (require 'skk-vars) (require 'static))

;;;###autoload
(defun skk-server-version ()
  (interactive)
  (cond
   ((interactive-p)
    (message "%s" (skk-server-version)))
   (t
    (let (status)
      (unless (or skk-server-host
		  skk-servers-list)
	(skk-error "Lack of host information of SKK server"
		   "SKK $B%5!<%P!<$N%[%9%H>pJs$,$"$j$^$;$s(B"))
      (setq status (process-status "skkservd"))
      (unless (eq status skk-network-open-status)
	(setq status (skk-open-server)))
      (when (eq status skk-network-open-status)
	(let (v)
	  (save-match-data
	    (with-current-buffer skkserv-working-buffer
	      (erase-buffer)
	      ;; $B%5!<%P!<%P!<%8%g%s$rF@$k!#(B
	      (process-send-string "skkservd" "2")
	      (while (eq (buffer-size) 0)
		(accept-process-output))
	      (setq v (buffer-string))
	      (erase-buffer)
	      ;; $B%[%9%HL>$rF@$k!#(B
	      (process-send-string "skkservd" "3")
	      (while (eq (buffer-size) 0)
		(accept-process-output))
	      (goto-char (point-min))
	      (format (concat "SKK SERVER version %s"
			      (if skk-japanese-message-and-error
				  "($B%[%9%HL>(B %s)"
				"running on HOST %s"))
		      v
		      (prog1
			  (buffer-string)
			(erase-buffer)))))))))))

;;;###autoload
(defun skk-search-server-1 (file limit)
  ;; skk-search-server $B$N%5%V%k!<%A%s!#(B
  (let ((key
	 (if skk-use-numeric-conversion
	     (skk-num-compute-henkan-key skk-henkan-key)
	   skk-henkan-key))
	;; $B%P%C%U%!%m!<%+%kCM$N<u$1EO$7$N$?$a!"JLL>$N0l;~JQ?t$K<h$k!#(B
	(okurigana (or skk-henkan-okurigana
		       skk-okuri-char))
	(status (process-status "skkservd")))
    (unless (eq status skk-network-open-status)
      (setq status (skk-open-server)))
    (cond
     ((eq status skk-network-open-status)
      (with-current-buffer skkserv-working-buffer
	(let ((cont t)
	      (count 0)
	      l)
	  (erase-buffer)
	  (process-send-string "skkservd" (concat "1" key " "))
	  (while (and cont
		      (eq (process-status "skkservd")
			  skk-network-open-status))
	    (accept-process-output)
	    (setq count (1+ count))
	    (when (> (buffer-size) 0)
	      (if (eq (char-after 1) ?1) ;?1
		  ;; found key successfully, so check if a whole line
		  ;; is received.
		  (when (eq (char-after (1- (point-max)))
			    ?\n) ;?\n
		    (setq cont nil))
		;; not found or error, so exit
		(setq cont nil))))
	  (goto-char (point-min))
	  (when skk-server-report-response
	    (skk-message "%d $B2s(B SKK $B%5!<%P!<$N1~EzBT$A$r$7$^$7$?(B"
			 "Waited for server response %d times"
			 count))
	  (when (eq (following-char) ?1) ;?1
	    (forward-char 2)
	    (setq l (skk-compute-henkan-lists okurigana))
	    (when l
	      (cond ((and okurigana
			  skk-henkan-okuri-strictly)
		     ;; $BAw$j2>L>$,F10l$N%(%s%H%j$N$_$rJV$9!#(B
		     (nth 2 l))
		    ((and okurigana
			  skk-henkan-strict-okuri-precedence)
		     (skk-nunion (nth 2 l) (car l)))
		    (t
		     (car l))))))))
     (t
      ;; server is not active, so search file instead
      (skk-search-jisyo-file file limit)))))

(defun skk-open-server ()
  ;; SKK $B%5!<%P!<$H@\B3$9$k!#%5!<%P!<%W%m%;%9$N(B status $B$rJV$9!#(B
  (let (status code proc)
    (when (or (skk-open-network-stream)
	      (skk-open-server-1))
      (setq status (process-status "skkservd"))
      (when (eq status skk-network-open-status)
	(setq code (cdr (assoc "euc" skk-coding-system-alist))
	      proc (get-process "skkservd"))
	(set-process-coding-system proc code code)))
    status))

(defun skk-open-server-1 ()
  ;; skk-open-server $B$N%5%V%k!<%A%s!#(B
  ;; skkserv $B%5!<%S%9$r%*!<%W%s$G$-$?$i(B t $B$rJV$9!#(B
  ;; skkserv $B$O0z?t$K<-=q$,;XDj$5$l$F$$$J$1$l$P!"(BDEFAULT_JISYO $B$r;2>H$9$k!#(B
  (unless skk-servers-list
    ;; Emacs $B5/F08e$K4D6-JQ?t$r@_Dj$7$?>l9g!#(B
    (unless skk-server-host
      (setq skk-server-host (getenv "SKKSERVER")))
    (unless skk-server-prog
      (setq skk-server-prog (getenv "SKKSERV")))
    (unless skk-server-jisyo
      (setq skk-server-jisyo (getenv "SKK_JISYO")))
    (if skk-server-host
	(setq skk-servers-list (list (list skk-server-host
					   skk-server-prog
					   skk-server-jisyo
					   skk-server-portnum)))
      (setq skk-server-prog nil)))
  (while (and (not (eq (process-status "skkservd")
		       skk-network-open-status))
	      skk-servers-list)
    (let ((elt (car skk-servers-list))
	  arg)
      (setq skk-server-host (car elt)
	    skk-server-prog (nth 1 elt)
	    skk-server-jisyo (nth 2 elt)
	    skk-server-portnum (nth 3 elt)
	    skk-servers-list (cdr skk-servers-list))
      ;; skkserv $B$N5/F0%*%W%7%g%s$O2<5-$NDL$j!#(B
      ;;     skkserv [-d] [-p NNNN] [JISHO]
      ;;     `-d'     $B%G%#%P%C%0!&%b!<%I(B
      ;;     `-p NNNN'     $BDL?.MQ$N%]!<%HHV9f$H$7$F(BNNNN$B$r;H$&(B.
      ;;     `~/JISYO'     ~/JISYO$B$r<-=q$H$7$FMxMQ(B.
      (if skk-server-jisyo
	  (setq arg (list skk-server-jisyo))
	;; skkserv $B$O0z?t$K<-=q$,;XDj$5$l$F$$$J$1$l$P!"(BDEFAULT_JISYO $B$r(B
	;; $B;2>H$9$k!#(B
	)
      ;;(if skk-server-debug
      ;;    (setq arg (cons "-d" arg)))
      (when (and skk-server-portnum
		 (not (= skk-server-portnum 1178)))
	(setq arg
	      (nconc (list "-p" (number-to-string skk-server-portnum))
		     arg)))
      (when (and skk-server-host
		 (not (skk-open-network-stream))
		 skk-server-prog)
	;; skk-startup-server $B$G%5!<%P!<$r5/F0$9$k$K$O!"(Bskk-server-host $B$H(B
	;; skk-server-prog $B$,@_Dj$5$l$F$$$k$3$H$,I,MW!#(B
	(skk-startup-server arg))))
  (if (not (eq (process-status "skkservd")
	       skk-network-open-status))
      ;; reset SKK-SERVER-HOST so as not to use server in this session
      (setq skk-server-host nil
	    skk-server-prog nil
	    skk-servers-list nil)
    t))

(defun skk-open-network-stream ()
  ;; skk-server-host $B$K$*$1$k(B skkserv $B%5!<%S%9$N(B TCP $B@\B3$r%*!<%W%s$7!"%W%m%;(B
  ;; $B%9$rJV$9!#(B
  (ignore-errors
    (setq skkserv-process
	  (open-network-stream "skkservd"
			       skkserv-working-buffer
			       skk-server-host
			       (or skk-server-portnum
				   "skkserv")))
    (process-kill-without-query skkserv-process)))

(defun skk-startup-server (arg)
  ;; skkserv $B$r5/F0$G$-$?$i(B t $B$rJV$9!#(B
  (let (
	;;(msgbuff (get-buffer-create " *skkserv-msg*"))
	(count 7))
    (while (> count 0)
      (skk-message
       "%s $B$N(B SKK $B%5!<%P!<$,5/F0$7$F$$$^$;$s!#5/F0$7$^$9(B%s"
       "SKK SERVER on %s is not active, I will activate it%s"
       skk-server-host (make-string count ?.))
      (if (or (string= skk-server-host (system-name))
	      (string= skk-server-host "localhost"))
	  ;; server host is local machine
	  (apply 'call-process skk-server-prog nil
		 ;;msgbuff
		 0 nil arg)
	(apply 'call-process
	       skk-server-remote-shell-program nil
	       ;; 0 $B$K$7$F%5%V%W%m%;%9$N=*N;$rBT$C$F$O$$$1$J$$M}M3$,$"$k!)(B
	       ;; $B$J$1$l$P(B msgbuf $B$K%(%i!<=PNO$r<h$C$?J}$,7z@_E*$G$O!)(B  $B$^$?$=(B
	       ;; $B$N>l9g$O$3$N(B while $B%k!<%W<+?H$,$$$i$J$$!)(B
	       ;; msgbuff
	       0 nil skk-server-host skk-server-prog arg))
      (sit-for 3)
      (if (and (skk-open-network-stream)
	       (eq (process-status "skkservd") skk-network-open-status))
	  (setq count 0)
	(setq count (1- count))))
    (if (eq (process-status "skkservd") skk-network-open-status)
	(progn
	  (skk-message "$B%[%9%H(B %s $B$N(B SKK $B%5!<%P!<$,5/F0$7$^$7$?(B"
		       "SKK SERVER on %s is active now"
		       skk-server-host)
	  (sit-for 1) ; return t
	  t) ; $B$G$bG0$N$?$a(B
      (skk-message "%s $B$N(B SKK $B%5!<%P!<$r5/F0$9$k$3$H$,$G$-$^$;$s$G$7$?(B"
		   "Could not activate SKK SERVER on %s"
		   skk-server-host)
      (sit-for 1)
      (ding) ;return nil
      nil))) ; $B$G$bG0$N$?$a(B

;;;###autoload
(defun skk-adjust-search-prog-list-for-server-search (&optional non-del)
  ;; skk-server-host $B$b$7$/$O(B skk-servers-list $B$,(B nil $B$G$"$l$P!"(B
  ;; skk-search-prog-list $B$+$i(B skk-search-server $B$r(B car $B$K;}$D%j%9%H$r>C$9!#(B
  ;; non-nil $B$G$"$l$P!"2C$($k!#(B
   (when (and (or skk-server-host
		  skk-servers-list)
	      (not (assq 'skk-search-server
			 (default-value 'skk-search-prog-list))))
     ;; skk-search-prog-list $B$,(B nil $B$H$$$&$3$H$O$^$:$J$$$@$m$&$,!"G0$N$?(B
     ;; $B$a!"(Bsetq $B$7$F$*$/!#(B
     (setq-default
      skk-search-prog-list
      ;; $BKvHx$KIU$1$k!#KvHx$K$O(B (skk-okuri-search) $B$r;}$C$F$-$?$$?M(B
      ;; $B$b$$$k$+$b!#%*%W%7%g%s$GIU$1$k>l=j$rJQ99$9$k$h$&$K$7$?J}$,(B
      ;; $BNI$$!)(B
      (nconc (default-value 'skk-search-prog-list)
	     (list
	      '(skk-search-server skk-aux-large-jisyo 10000))))))

(defun skk-disconnect-server ()
  ;; $B%5!<%P!<$r@Z$jN%$9!#(B
  (when (and skk-server-host
	     (eq (process-status "skkservd")
		 skk-network-open-status))
    (process-send-string "skkservd" "0") ; disconnect server
    (accept-process-output (get-process "skkservd"))))

;;(add-hook 'skk-mode-hook 'skk-adjust-search-prog-list-for-server-search)
(add-hook 'skk-before-kill-emacs-hook 'skk-disconnect-server)

(run-hooks 'skk-server-load-hook)

(require 'product)
(product-provide
    (provide 'skk-server)
  (require 'skk-version))

;;; skk-server.el ends here
