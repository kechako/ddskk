;;; skk-w3m.el --- SKK search using w3m-search
;; Copyright (C) 2001 NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>

;; Author: NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Version: $Id: skk-w3m.el,v 1.14 2001/05/31 02:55:32 minakaji Exp $
;; Keywords: japanese
;; Created: Apr. 12, 2001 (oh, its my brother's birthday!)
;; Last Modified: $Date: 2001/05/31 02:55:32 $

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
;; w3m (http://ei5nazha.yz.yamagata-u.ac.jp/~aito/w3m/) $B$rMxMQ$7!"(B
;; Emacs $B$NCf$+$i(B Web $B8!:w%(%s%8%s$K$h$k8!:w$r$7!"8!:w7k2L$NCf$+$i(B
;; SKK $B$N8uJd$H$7$F<h$j=P$7$?$$$b$N$r@Z$j=P$7$FMxMQ$9$k%W%m%0%i%`$G(B
;; $B$9!#(B
;; 
;; skk-w3m-use-w3m-backend $B$,(B non-nil $B$G$"$l$P!"(Bw3m $B$r(B backend $B%*%W(B
;; $B%7%g%sIU$-$G5/F0$7$F(B w3m $B$HD>@\8r?.$7$^$9!#(Bnil $B$G$"$l$P(B emacs-w3m
;; (http://www-nagao.kuee.kyoto-u.ac.jp/member/tsuchiya/w3m/) $B$r7PM3(B
;; $B$7$F(B w3m $B$rMxMQ$7$^$9!#8=:_$N(B emacs-w3m $B$G$O(B w3m $B$r(B backend $B$GF0$+(B
;; $B$7$F$$$^$;$s!#(Bw3m backend $B$rMxMQ$9$k$3$H$G!"8!:w$NEYKh$K(B w3m $B$r5/(B
;; $BF0$9$kI,MW$,$J$/$J$j!"%W%m%;%9$N5/F0!"=*N;$KH<$J$&%*!<%P!<%X%C%I$r(B
;; $B8:$i$9$3$H$,$G$-$^$9$,!"(Bw3m backend $B$O3+H/Cf$G$"$j!":#8eBgI}$J;EMM(B
;; $BJQ99$,9T$J$o$l$k2DG=@-$b$"$j!"M=CG$r5v$7$^$;$s!#(B
;; 
;; <HOW TO INSTALL>
;; .emacs $B$rFI$_9~$^$:$K(B emacs-w3m $B$,(B load $B$G$-$k4D6-$,I,?\$G$9!#$=(B
;; $B$N>e$G$3$N%U%!%$%k$r(B SKK-MK $B$,$"$k%G%#%l%/%H%j$K%3%T!<$7(B ($B%j%s%/(B
;; $B$,;H$($k%U%!%$%k%7%9%F%`$G$O(B SKK-MK $B$N$"$k%G%#%l%/%H%j$G(B
;;   ln -s ./experimental/skk-w3m.el .
;; $B$7$?J}$,NI$$$+$b$7$l$^$;$s(B)$B!"8e$OIaDL$K(B make install $B$9$k$@$1$G$9!#(B
;;
;; <HOW TO WORK>
;; skk-search-prog-list $B$K(B (skk-w3m-search "goo-daijirin") $B$N$h$&$J(B
;; $BMWAG$rDI2C$7$^$9!#DL>o!"B>$N$I$N(B skk search engine $B$h$j$b:G$bCY$$(B
;; $B$N$G!":G$b:G8e$,NI$$$G$7$g$&!#$3$s$J46$8$K$J$j$^$9!#(B
;;
;; (setq skk-search-prog-list
;;       '((skk-search-jisyo-file skk-jisyo 0 t)
;;         (skk-search-server skk-aux-large-jisyo 10000)
;;         (skk-w3m-search "goo-daijirin")
;;         (skk-w3m-search "goo-exceed-eiwa")))
;;
;; skk-w3m-search $B$N0z?t$O8!:w%(%s%8%s$N<oN`$rJ8;zNs$G;XDj$7$^$9!#(B
;; $BC"$7!"(Bskk-w3m-search-engine-alist $B$KBP1~$9$k%(%s%H%j$,I,MW$G$9!#(B
;;
;; skk-w3m.el $B$G$O(B search-engine $BKh$K8!:w7k2L$r(B cache $B$7$^$9!#(B
;; (skk-w3m-search "goo-daijirin" t) $B$N$h$&$K(B `skk-w3m-search' $B$NBh(B
;; $BFs0z?t$K(B non-nil argument $B$r;XDj$9$k$H(B cache $B$r9T$J$o$:!"Kh2s(B w3m
;; $B$K8!:w$r$5$;$^$9!#(B
;;
;; <TODO>
;; o $B$H$j$"$($:(B skk-w3m-get-candidates-from-goo-exceed-waei,
;;   skk-w3m-get-candidates-from-goo-exceed-eiwa,
;;   skk-w3m-get-candidates-from-goo-daily-shingo $B$r40@.$5$;$k!#(B
;; o $B8!:w%(%s%8%s$NA}2C!#(B
;; o lookup $B$O(B w3m-search.el $B$r;H$C$?(B Web search $B$rE}9g$7$J$$$N$@$m$&(B
;;   $B$+(B...$B!#E}9g$9$l$P(B skk-lookup.el $B$G0l854IM}$G$-$k!)(B
;; o w3m backend $B$N2~NI$KDI=>!#(B
;; 
;;; Code
(eval-when-compile (require 'skk-macs) (require 'skk-vars))

(defgroup skk-w3m nil "SKK w3m related customization."
  :prefix "skk-w3m-"
  :group 'skk)

;;;; user variables.
(defvar skk-w3m-search-engine-alist
  '(("goo-daijirin"
     "http://dictionary.goo.ne.jp/cgi-bin/dict_search.cgi?MT=%s&sw=2" euc-japan
     skk-w3m-get-candidates-from-goo-daijirin
     (or
      ;; cannot search a key which contains okuri prefix.
      skk-okuri-char
      ;; cannot search by Web engine a string which containing SKK special `#' character.
      skk-num-list skk-num-recompute-key
      ;; this engine does not contain English entries.
      skk-abbrev-mode))
    ("goo-exceed-waei"
     "http://dictionary.goo.ne.jp/cgi-bin/dict_search.cgi?MT=%s&sw=1" euc-japan
     ;;skk-w3m-get-candidates-from-goo-exceed-waei ; not yet finished.
     nil
     (or skk-okuri-char skk-num-list skk-num-recompute-key skk-abbrev-mode))
    ("goo-exceed-eiwa"
     "http://dictionary.goo.ne.jp/cgi-bin/dict_search.cgi?MT=%s&sw=0" euc-japan
     ;;skk-w3m-get-candidates-from-goo-exceed-eiwa ; not yet finished.
     nil
     (not skk-abbrev-mode))
    ("goo-daily-shingo"
     "http://dictionary.goo.ne.jp/cgi-bin/dict_search.cgi?MT=%s&sw=3" euc-japan
     ;;skk-w3m-get-candidates-from-goo-daily-shingo ; not yet finished.
     nil
     (or skk-okuri-char skk-num-list skk-num-recompute-key)))
  "*$B8!:w%(%s%8%sKh$N8!:w%*%W%7%g%s$r;XDj$9$k%(!<%j%9%H!#(B
car $B$O8!:w%(%s%8%s$rI=$o$9J8;zNs!"(B
cdr $B$O(B URL ($B8!:wJ8;zNs$r(B %s $B$GI=$o$9(B),
2th $B$O(B Web page $B$N(B coding-system,
3th $B$O8uJd@Z$j=P$7$K;HMQ$9$k4X?t$rI=$o$9%7%s%\%k!#(B
4th (optional) $B$O(B S $B<0$r;XDj$7!"I>2A$7$F(B non-nil $B$K$J$k>uBV$N$H$-$O(B w3m
    $B$K8!:w=hM}$r$5$;$J$$!#(B
5th $B$O(B `skk-henkan-key' $B$r2C9)$9$k4X?t!#(B")

(defvar skk-w3m-use-w3m-backend t
  "*Non-nil $B$G$"$l$P!"(Bw3m $B$r(B backend $B%*%W%7%g%sIU$-$G5/F0$7$F8!:w$r9T$J$&!#(B
`start-process' $B$,;H$($J$$(B Emacs $B$G$OMxMQIT2D!#(B
nil $B$G$"$l$P!"(Bemacs-w3m $B$r7PM3$7$F(B w3m $B$rMxMQ$9$k(B ($B8=:_$N(B emacs-w3m $B$G$O(B
w3m $B$r(B backend $B$GF0$+$7$F$$$J$$(B)$B!#(B")

(defvar skk-w3m-command (or (and (boundp 'w3m-command) w3m-command) "w3m")
  "*w3m $B%3%^%s%IL>!#(B")
 
(defvar skk-w3m-command-args "-backend"
  "*w3m $B$N(B backend $B%*%W%7%g%s!#(B")

(defvar skk-w3m-backend-command-prompt "w3m>"
  "*w3m backend $B$N%3%^%s%I%W%m%s%W%H!#(B")

(defvar skk-w3m-default-process-coding-system 'euc-japan
  "*w3m backend $B%W%m%;%9$N%G%#%U%)%k%H$N(B coding-system$B!#(B")

(defvar skk-w3m-kill-command "quit"
  "*w3m backend $B$N=*N;%3%^%s%I!#(B")

(defvar skk-w3m-no-wait nil
  "*Non-nil $B$G$"$l$P!"(Bw3m backend $B%W%m%;%9$,2?$+=PNO$9$k$^$GBT$?$J$$!#(B")

;;;; system internal variables and constants.
;;; constants.
(defconst skk-w3m-working-buffer " *skk-w3m-work*")

;;; global variables
(defvar skk-w3m-process nil)
(defvar skk-w3m-last-process-point (make-marker))
(defvar skk-w3m-cache nil)

;;;; inline functions
(defsubst skk-w3m-process-alive ()
  (and skk-w3m-process
       (memq (process-status skk-w3m-process) '(run stop))))

;;;; functions
;;; common tools
;;;###autoload
(defun skk-w3m-search (search-engine &optional no-cache)
  (let* ((dbase (assoc search-engine skk-w3m-search-engine-alist))
	 (sex (nth 4 dbase))
	 cache v)
    (if (and dbase
	     (or (not sex)	       ; always search this engine, or
		 (not (eval sex))))	; search this time.
	(if (and (not no-cache)
		 (setq cache (cdr (assoc search-engine skk-w3m-cache))
		       cache (cdr (assoc skk-henkan-key cache))))
	    cache
	  (condition-case nil
	      (prog1
		  (setq v (if skk-w3m-use-w3m-backend
			      (skk-w3m-search-by-backend dbase skk-henkan-key)
			    (skk-w3m-search-by-emcas-w3m dbase skk-henkan-key)))
		(skk-w3m-cache search-engine skk-henkan-key v))
	    (error)))))) ; catch network unreachable error or something like that.

(defun skk-w3m-cache (search-engine key list)
  (let ((cache (assoc search-engine skk-w3m-cache))
	l)
    (cond
     ((and cache (setq l (assoc key cache)))
      (setcdr l list))
     (cache
      (setcdr cache (cons (cons key list) (cdr cache))))
     (t
      (setq skk-w3m-cache (cons (cons search-engine (list (cons key list)))
				skk-w3m-cache))))))
	   
(defun skk-w3m-filter-string (string filters)
  (while filters
    (while (string-match (car filters) string)
      (setq string (concat (substring string 0 (match-beginning 0))
			   (substring string (match-end 0)))))
    (setq filters (cdr filters)))
  string)

;;; emacs-w3m dependent
(defun skk-w3m-search-by-emcas-w3m (dbase key)
  (require 'w3m)
  (require 'w3m-search)
  (save-excursion
    (let ((post-process (nth 3 dbase))
	  (process-key (nth 5 dbase))
	  (w3m-async-exec nil)
	  (w3m-work-buffer-name " *skk-w3m-work*"))
      (if process-key (setq key (funcall process-key key)))
      (if post-process 
	  (w3m-with-work-buffer
	    (or (w3m-w3m-retrieve
		 (format (nth 1 dbase)
			 (w3m-search-escape-query-string key (nth 2 dbase))))
		(error "")
		(funcall post-process key)))))))

;;; w3m backend dependent
(defun skk-w3m-search-by-backend (dbase key)
  (let (pos)
    (with-current-buffer (get-buffer-create skk-w3m-working-buffer)
      (or (skk-w3m-process-alive) (skk-w3m-init-w3m-backend))
      (let ((process-key (nth 5 dbase))
	    (post-process (nth 3 dbase)))
	(if (not post-process)
	    nil
	  (if process-key
	      (setq key (funcall process-key key)))
	  (if (nth 2 dbase)
	      (skk-w3m-set-process-coding-system (nth 2 dbase)))
	  (message "Reading...")
	  (setq pos (skk-w3m-run-command
		     (concat "get " (format
				     (nth 1 dbase)
				     (skk-w3m-search-escape-query-string
				      key (nth 2 dbase))))))
	  (message "Reading...done")
	  (if (and pos (markerp pos))
	      (progn
		(goto-char pos)
		;; not to enlarge working buffer
		(delete-region (point-min) (progn (beginning-of-line) (point)))
		(setq key (funcall post-process key)))))))))

(defun skk-w3m-set-process-coding-system (coding-system)
  (static-cond
   ((eq skk-emacs-type 'xemacs)
    (set-process-input-coding-system skk-w3m-process coding-system)
    (set-process-output-coding-system skk-w3m-process coding-system))
   (t
    (set-process-coding-system skk-w3m-process coding-system coding-system))))

(defun skk-w3m-init-w3m-backend ()
  (let ((process-connection-type t))
    (buffer-disable-undo)
    ;;(insert "\nStarting w3m backend...\n\n")
    (skk-message "skk $B$N$?$a$K(B w3m backend $B$r5/F0$7$F$$$^$9(B..."
		 "Starting w3m backend for skk...")
    (condition-case nil
	(progn
	  (setq skk-w3m-process
		(start-process "skk w3m" (current-buffer) skk-w3m-command
			       skk-w3m-command-args))
	  (process-kill-without-query skk-w3m-process)
	  (skk-w3m-set-process-coding-system
	   skk-w3m-default-process-coding-system))
      (file-error (skk-error "$B%7%9%F%`>e$K(B \"%s\" $B$,8+$D$+$j$^$;$s(B"
			     "Sorry, can't find \"%s\" on your system"
			     skk-w3m-command))
      (error (skk-w3m-kill 'nomsg)))
    (if (eq (process-status skk-w3m-process) 'exit)
	(progn
	  (skk-w3m-kill 'nomsg)
	  (skk-error "%s $B%W%m%;%9$,0[>o=*N;$7$^$7$?!#(B"
		     "Process %s exited abnormally with code 1"
		     skk-w3m-process)))
    (while (and (memq (process-status skk-w3m-process) '(run stop))
		(goto-char (point-min))
		(not (re-search-forward skk-w3m-backend-command-prompt nil t)))
      (accept-process-output skk-w3m-process))
    ;;(or (memq (process-status skk-w3m-process) '(run stop))
    ;;    (skk-error "w3m backend $B%W%m%;%9$r%9%?!<%H$9$k$3$H$,$G$-$^$;$s(B"
    ;;               "Unable to start w3m backend process"))
    (goto-char (process-mark skk-w3m-process))
    (set-marker skk-w3m-last-process-point (point))
    (skk-message "skk $B$N$?$a$K(B w3m backend $B$r5/F0$7$F$$$^$9(B...$B40N;(B!"
		 "Starting w3m backend for skk...done")))

(defun skk-w3m-kill (&optional nomsg)
  "w3m backend $B%W%m%;%9$r;&$9!#(B"
  (interactive "P")
  (if (not (skk-w3m-process-alive))
      ;; $BKLEM?@7}$N@$3&$G$9$J(B...$B!#(B
      (or nomsg
	  (skk-message "w3m backend $B%W%m%;%9$O4{$K;`$s$G$^$9(B"
		       "w3m backend process has already died"))
    (with-current-buffer (get-buffer skk-w3m-working-buffer)
      (unwind-protect
	  (let ((skk-w3m-no-wait t))
	    (skk-w3m-run-command skk-w3m-kill-command)
	    ;;(sit-for 1)
	    (and (process-status skk-w3m-process)
		 (delete-process skk-w3m-process))
	    ;;(setq skk-w3m-process nil)
	    (or nomsg
		(skk-message "w3m backend $B%W%m%;%9$,;`$K$^$7$?(B"
			     "w3m backend process died")))
	(kill-buffer (current-buffer))))))

(defun skk-w3m-run-command (command)
  ;; return last point where last command issued.
  (save-match-data
    (setq command (concat command " \n"))
    (let ((pmark (process-mark skk-w3m-process)))
      (accept-process-output)
      ;; $BF0$$$?%]%$%s%H$rJ]B8$9$k$?$a(B save-excursion $B$O;H$o$J$$!#(B
      (goto-char pmark)
      (set-marker skk-w3m-last-process-point (point))
      (insert command)
      (set-marker pmark (point))
      (process-send-string skk-w3m-process command)
      (accept-process-output (and (not skk-w3m-no-wait) skk-w3m-process))
      (goto-char skk-w3m-last-process-point)
      (while (and (not (re-search-forward
			skk-w3m-backend-command-prompt pmark t))
		  ;; quit $B%3%^%s%I$rAw$C$?$i%W%m%s%W%H$O5"$C$F$3$J$$!#(B
		  (not (eq (process-status skk-w3m-process) 'exit)))
	(accept-process-output))
      ;;(skk-w3m-check-errors)
      skk-w3m-last-process-point)))

;; just a copy of w3m-url-encode-string of w3m.el
(defun skk-w3m-url-encode-string (str &optional coding)
  (apply (function concat)
	 (mapcar
	  (lambda (ch)
	    (cond
	     ((string-match "[-a-zA-Z0-9_:/]" (char-to-string ch)) ; xxx?
	      (char-to-string ch))	; printable
	     (t
	      (format "%%%02X" ch))))	; escape
	  ;; Coerce a string to a list of chars.
	  (append (encode-coding-string str (or coding 'iso-2022-jp))
		  nil))))

;; just a copy of w3m-search-escape-query-string of w3m-search.el
(defun skk-w3m-search-escape-query-string (str &optional coding)
  (mapconcat (lambda (s)
	       (skk-w3m-url-encode-string
		s (or coding skk-w3m-default-process-coding-system)))
	     (split-string str)
	     "+"))

;;; process functions for each databases.
(defun skk-w3m-get-candidates-from-goo-daijirin (key)
  ;; <!-- RESULT_BLOCK -->
  ;; <table width="100%" border="0" cellspacing="0" cellpadding="0"><tr><td>
  ;; <!-- ej_res1 -->
  ;; <table width="100%" border="0" cellspacing="0" cellpadding="0">
  ;;   <tr>
  ;;     <td>
  ;;       $B"#!N(B<font color="#993333">$B$3$&$3$&(B</font>$B!O$NBg<-NSBhFsHG$+$i$N8!:w7k2L!!(B
  ;;      <font size="+1" color="#993333"><b>39$B7o(B</b></font>
  ;;     </td>
  ;;   </tr>
  ;;   <tr>
  ;;     <td bgcolor="#993333"><img src="/Common/clear.gif" width="1" height="1" alt=""></td>
  ;;   </tr>
  ;;   <tr>
  ;;     <td>
  ;;       <br>
  ;;       <table border="0" cellspacing="4" cellpadding="4">
  ;;         <tr>
  ;;           <td><br></td>
  ;;           <td><b>1</b></td>
  ;;           <td>
  ;;             <a href="/cgi-bin/jp-more_print.cgi?MT=%A4%B3%A4%A6%A4%B3%A4%A6&ID=a4b3/06660300.txt&sw=2" target="_blank">
  ;;             <img src="/Common/icon01.gif" width="12" height="12" border="0" alt="$B?75,$G3+$/(B"></a>
  ;;             </td>
  ;;           <td nowrap>
;;             <a href="/cgi-bin/jp-more_print.cgi?MT=%A4%B3%A4%A6%A4%B3%A4%A6&ID=a4b3/06660300.txt&sw=2">$B$3$&$3$&(B $B!Z8}9P![(B</a>
  ;;           </td>
  ;;         </tr>
  ;;         ...
  ;;         <tr>
  ;;           <td><br></td>
  ;;           <td><b>25</b></td>
  ;;           <td>
  ;;             <a href="/cgi-bin/jp-more_print.cgi?MT=%A4%B3%A4%A6%A4%B3%A4%A6&ID=a4b3/06663300.txt&sw=2" target="_blank">
  ;;             <img src="/Common/icon01.gif" width="12" height="12" border="0" alt="$B?75,$G3+$/(B"></a>
  ;;             </td>
  ;;           <td nowrap>
  ;;             <a href="/cgi-bin/jp-more_print.cgi?MT=%A4%B3%A4%A6%A4%B3%A4%A6&ID=a4b3/06663300.txt&sw=2">$B$3$&$3$&(B $B!Z(B<img src="/jp/image/G149A.gif" width="14" HEIGHT="19" align="absmiddle" hspace="2"><img src="/jp/image/G149A.gif" width="14" HEIGHT="19" align="absmiddle" hspace="2">$B![(B</a>
  ;;           </td>
  ;;         </tr>
  ;;         ...
  ;;       </table>
  ;;       <br>
  ;;     </td>
  ;;   </tr>
  ;;   <tr>
  ;;     <td bgcolor="#993333"><img src="/Common/clear.gif" width="1" height="1" alt=""></td>
  ;;   </tr>
  ;;   <tr>
  ;;     <td>
  ;;       $B"#!N(B<font color="#993333">$B$3$&$3$&(B</font>$B!O$NBg<-NSBhFsHG$+$i$N8!:w7k2L!!(B
  ;;      <font size="+1" color="#993333"><b>39$B7o(B</b></font>
  ;;     </td>
  ;;   </tr>
  ;; </table>
  ;; <!-- ej_res1 -->
  ;; </td></tr></table>
  ;; <!-- RESULT_BLOCK -->
  (save-match-data
    (let ((startregexp
	   (if skk-w3m-use-w3m-backend 
	       nil
	       ;;(format
		;;"$B"#!N(B%s$B!O$NBg<-NSBhFsHG$+$i$N8!:w7k2L!!(B <b>[0-9]+$B7o(B</b>" key)
	     "<!-- RESULT_BLOCK -->"))
	  (endregexp
	   (if skk-w3m-use-w3m-backend 
	       nil
	       ;;(format
		;;"$B"#!N(B%s$B!O$NBg<-NSBhFsHG$+$i$N8!:w7k2L!!(B <b>[0-9]+$B7o(B</b>" key)
	     "<!-- RESULT_BLOCK -->"))
	  (start (if skk-w3m-use-w3m-backend skk-w3m-last-process-point))
	  (end (if skk-w3m-use-w3m-backend (process-mark skk-w3m-process)))
	  temp v)
      (if startregexp 
	  (progn
	    (re-search-forward startregexp nil t nil)
	    (setq start (point))))
      (if endregexp
	  (progn
	    (re-search-forward endregexp nil t nil)
	    (setq end (point))))
      (if (not (and start end))
	  nil
	(goto-char start)
	(setq key (format "<a href=\".+\">%s *$B!Z(B\\([^<>$B!Z![(B]+\\)$B![(B</a>" key))
	(while (re-search-forward key end t nil)
	  (setq temp (skk-w3m-filter-string
		      ;; $B!R2?;~!S(B
		      (match-string-no-properties 1) '("$B!R(B" "$B!S(B")))
	  (setq v (nconc (split-string temp "$B!&(B") v)))
	(nreverse v)))))

(defun skk-w3m-get-candidates-from-goo-exceed-waei (key)
  ;; SORRY, NOT YET.
  ;;   ;; 15:$B"#!N$M$C$7$s!O$N(BEXCEED$BOB1Q<-E5$+$i$N8!:w7k2L!!(B
  ;;   ;; 16:*
  ;;   ;; 17:
  ;;   ;; 18:$B$M$C$7$s(B
  ;;   ;; 19:[clear] $BG.?4(B
  ;;   ;; 20:[clear] zeal$B!(!!(Bardor$B!(!!(Beagerness$B!(!!(Benthusiasm$B!%!!!A$J!!(B
  ;;   ;; 21:        eager$B!(!!(Bardent$B!(!!(Bkeen$B!%!!!A$K!!(Beagerly$B!(!!(B
  ;;   ;; 22:        earnestly$B!(!!(Bintently$B!%!!(B
  ;;   ;; 23:
  ;;   ;; 24:*
  ;;   ;; 25:$B"#!N$M$C$7$s!O$N(BEXCEED$BOB1Q<-E5$+$i$N8!:w7k2L!!(B
  ;;   (let (temp v)
  ;;     (save-match-data
  ;;       (if (not (re-search-forward
  ;; 		(concat "$B"#(B\\$B!N(B" (regexp-quote key) "\\$B!O$N(BEXCEED$BOB1Q<-E5$+$i$N8!:w7k2L(B")
  ;; 		nil t nil))
  ;; 	  nil
  ;; 	(while (re-search-forward "\\[clear\\] [a-z]+\\.$B!!(B\\([^ a-zA-Z][^$B!%(B]+\\)$B!%(B" nil t nil)
  ;; 	  (setq temp (match-string-no-properties 1))
  ;; 	  (setq temp (skk-w3m-filter-string
  ;; 		      ;; [[$BJFOC(B]]
  ;; 		      temp '("\n" "[0-9]+: +" "[$B!!(B ]+" "$B!J(B[$B$!(B-$B$s(B]+$B!K(B" "([, a-z]+)"
  ;; 			     "\\[\\[[^a-zA-Z]+\\]\\]")))
  ;; 	  (while (string-match "\\([^$B!$!((B]+\\)$B!N(B\\([^$B!$!((B]+\\)$B!O(B\\([^$B!$!((B]+\\)*" temp)
  ;; 	    (setq temp (concat (substring temp 0 (match-beginning 0))
  ;; 			       (match-string-no-properties 1 temp)
  ;; 			       (match-string-no-properties 3 temp)
  ;; 			       "$B!$(B"
  ;; 			       (match-string-no-properties 2 temp)
  ;; 			       (match-string-no-properties 3 temp)
  ;; 			       (substring temp (match-end 0)))))
  ;;
  ;; 	  (setq v (nconc v (split-string temp "[$B!$!((B]"))))
  ;; 	v))))
  )

(defun skk-w3m-get-candidates-from-goo-exceed-eiwa (key)
  ;; SORRY, NOT YET.
  ;;
  ;; <!-- RESULT_BLOCK -->
  ;; <table width="100%" border="0" cellspacing="0" cellpadding="0"><tr><td>
  ;; <!-- ej_res1 -->
  ;; <table width="100%" border="0" cellspacing="0" cellpadding="0">
  ;;   <tr>
  ;;     <td>
  ;;       $B"#!N(B<font color="#993333">collaborate</font>$B!O$N(BEXCEED$B1QOB<-E5$+$i$N8!:w7k2L!!(B
  ;;     </td>
  ;;   </tr>
  ;;   <tr>
  ;;     <td bgcolor="#993333"><img src="/Common/clear.gif" width="1" height="1" alt=""></td>
  ;;   </tr>
  ;;   <tr>
  ;;     <td>
  ;;       <br>
  ;; <TABLE WIDTH="468" BORDER="0" CELLSPACING="0" CELLPADDING="0">
  ;;   <TR ALIGN="LEFT" VALIGN="MIDDLE">
  ;;     <TD>
  ;;       <SPAN CLASS="css4g">
  ;;         <B>col$B!&(Blab$B!&(Bo$B!&(Brate</B>$B!!(B<A HREF="http://dictionary2.goo.ne.jp/ej/voice/C/01010419.wav"><IMG LOWSRC="/ej/image/voice.gif" WIDTH="23" HEIGHT="12" BORDER="0" ALIGN="absmiddle"></A>$B!!(B
  ;;       </SPAN>
  ;;     </TD>
  ;;   </TR>
  ;; </TABLE>
  ;; <TABLE WIDTH="468" BORDER="0" CELLSPACING="0" CELLPADDING="0">
  ;;   <TR>
  ;;     <TD ALIGN="LEFT" VALIGN="TOP"><IMG SRC="/Common/clear.gif" WIDTH="68" HEIGHT="2"></TD>
  ;;     <TD WIDTH="400" ALIGN="LEFT" VALIGN="TOP">
  ;;       <SPAN CLASS="css3g">
  ;;         <IMG SRC="/ej/image/e1073.gif" WIDTH="8" HEIGHT="16" ALT=""><IMG SRC="/ej/image/e1015.gif" WIDTH="8" HEIGHT="16" ALT=""><IMG SRC="/ej/image/e1009.gif" WIDTH="8" HEIGHT="16" ALT=""><IMG SRC="/ej/image/e1016.gif" WIDTH="8" HEIGHT="16" ALT=""><IMG SRC="/ej/image/e1022.gif" WIDTH="8" HEIGHT="16" ALT=""><IMG SRC="/ej/image/e1001.gif" WIDTH="8" HEIGHT="16" ALT=""><IMG SRC="/ej/image/e1009.gif" WIDTH="8" HEIGHT="16" ALT=""><IMG SRC="/ej/image/e101b.gif" WIDTH="8" HEIGHT="16" ALT=""><IMG SRC="/ej/image/e1054.gif" WIDTH="8" HEIGHT="16" ALT=""><IMG SRC="/ej/image/e1013.gif" WIDTH="8" HEIGHT="16" ALT=""><IMG SRC="/ej/image/e1003.gif" WIDTH="8" HEIGHT="16" ALT=""><IMG SRC="/ej/image/e1074.gif" WIDTH="8" HEIGHT="16" ALT="">$B!!(B
  ;;       </SPAN>
  ;;     </TD>
  ;;   </TR>
  ;; </TABLE>
  ;; <TABLE WIDTH="468" BORDER="0" CELLSPACING="0" CELLPADDING="0">
  ;;   <TR>
  ;;     <TD ALIGN="LEFT" VALIGN="TOP"><IMG SRC="/Common/clear.gif" WIDTH="68" HEIGHT="2"></TD>
  ;;     <TD WIDTH="400" ALIGN="LEFT" VALIGN="TOP">
  ;;       <SPAN CLASS="css3g">
  ;;         <i>vi.</i>$B!!6&$KF/$/!(!!6&F18&5f$9$k!!(B<i>(with, on, in)$B!((B</i>$B!!E(B&!N@jNN73!O$K6(NO$9$k!%!!(B
  ;;       </SPAN>
  ;;     </TD>
  ;;   </TR>
  ;; </TABLE>
  ;; <TABLE WIDTH="468" BORDER="0" CELLSPACING="0" CELLPADDING="0">
  ;;   <TR>
  ;;     <TD ALIGN="LEFT" VALIGN="TOP"><IMG SRC="/Common/clear.gif" WIDTH="48" HEIGHT="2"></TD>
  ;;     <TD WIDTH="420" ALIGN="LEFT" VALIGN="TOP">
  ;;       <SPAN CLASS="css3g">
  ;;         <FONT COLOR="FF0000">collaboration</FONT>$B!!(B<A HREF="http://dictionary2.goo.ne.jp/ej/voice/C/02020773.wav"><IMG LOWSRC="/ej/image/voice.gif" WIDTH="23" HEIGHT="12" BORDER="0" ALIGN="absmiddle"></A>$B!!(B
  ;;       </SPAN>
  ;;     </TD>
  ;;   </TR>
  ;; </TABLE>
  ;; <TABLE WIDTH="468" BORDER="0" CELLSPACING="0" CELLPADDING="0">
  ;;   <TR>
  ;;     <TD ALIGN="LEFT" VALIGN="TOP"><IMG SRC="/Common/clear.gif" WIDTH="68" HEIGHT="2"></TD>
  ;;     <TD WIDTH="400" ALIGN="LEFT" VALIGN="TOP">
  ;;       <SPAN CLASS="css3g">
  ;;         <i>n.</i>$B!!(B<FONT COLOR="FF0000">collaborationism</FONT>$B!!(B<i>n.</i>$B!!(B<FONT COLOR="FF0000">collaborationist</FONT>$B!!(B<i>n.</i>$B!!!JE(B&$X$N!K6(NO<T!%!!(B
  ;;       </SPAN>
  ;;     </TD>
  ;;   </TR>
  ;; </TABLE>
  ;; <TABLE WIDTH="468" BORDER="0" CELLSPACING="0" CELLPADDING="0">
  ;;   <TR>
  ;;     <TD ALIGN="LEFT" VALIGN="TOP"><IMG SRC="/Common/clear.gif" WIDTH="48" HEIGHT="2"></TD>
  ;;     <TD WIDTH="420" ALIGN="LEFT" VALIGN="TOP">
  ;;       <SPAN CLASS="css3g">
  ;;         <FONT COLOR="FF0000">collaborative</FONT>$B!!(B
  ;;       </SPAN>
  ;;     </TD>
  ;;   </TR>
  ;; </TABLE>
  ;; <TABLE WIDTH="468" BORDER="0" CELLSPACING="0" CELLPADDING="0">
  ;;   <TR>
  ;;     <TD ALIGN="LEFT" VALIGN="TOP"><IMG SRC="/Common/clear.gif" WIDTH="68" HEIGHT="2"></TD>
  ;;     <TD WIDTH="400" ALIGN="LEFT" VALIGN="TOP">
  ;;       <SPAN CLASS="css3g">
  ;;         <IMG SRC="/ej/image/e1073.gif" WIDTH="8" HEIGHT="16" ALT=""><IMG SRC="/ej/image/e1015.gif" WIDTH="8" HEIGHT="16" ALT=""><IMG SRC="/ej/image/e1009.gif" WIDTH="8" HEIGHT="16" ALT=""><IMG SRC="/ej/image/e1016.gif" WIDTH="8" HEIGHT="16" ALT=""><IMG SRC="/ej/image/e1022.gif" WIDTH="8" HEIGHT="16" ALT=""><IMG SRC="/ej/image/e1001.gif" WIDTH="8" HEIGHT="16" ALT=""><IMG SRC="/ej/image/e1009.gif" WIDTH="8" HEIGHT="16" ALT=""><IMG SRC="/ej/image/e101b.gif" WIDTH="8" HEIGHT="16" ALT=""><IMG SRC="/ej/image/e1054.gif" WIDTH="8" HEIGHT="16" ALT=""><IMG SRC="/ej/image/e1013.gif" WIDTH="8" HEIGHT="16" ALT=""><IMG SRC="/ej/image/e1003.gif" WIDTH="8" HEIGHT="16" ALT=""><IMG SRC="/ej/image/e1013.gif" WIDTH="8" HEIGHT="16" ALT=""><IMG SRC="/ej/image/e101d.gif" WIDTH="8" HEIGHT="16" ALT=""><IMG SRC="/ej/image/e1074.gif" WIDTH="8" HEIGHT="16" ALT="">$B!!(B
  ;;       </SPAN>
  ;;     </TD>
  ;;   </TR>
  ;; </TABLE>
  ;; <TABLE WIDTH="468" BORDER="0" CELLSPACING="0" CELLPADDING="0">
  ;;   <TR>
  ;;     <TD ALIGN="LEFT" VALIGN="TOP"><IMG SRC="/Common/clear.gif" WIDTH="68" HEIGHT="2"></TD>
  ;;     <TD WIDTH="400" ALIGN="LEFT" VALIGN="TOP">
  ;;       <SPAN CLASS="css3g">
  ;;         <i>a.</i>$B!!6&F1@):n$N!%!!(B
  ;;       </SPAN>
  ;;     </TD>
  ;;   </TR>
  ;; </TABLE>
  ;; <TABLE WIDTH="468" BORDER="0" CELLSPACING="0" CELLPADDING="0">
  ;;   <TR>
  ;;     <TD ALIGN="LEFT" VALIGN="TOP"><IMG SRC="/Common/clear.gif" WIDTH="48" HEIGHT="2"></TD>
  ;;     <TD WIDTH="420" ALIGN="LEFT" VALIGN="TOP">
  ;;       <SPAN CLASS="css3g">
  ;;         <FONT COLOR="FF0000">collaborator</FONT>$B!!(B<A HREF="http://dictionary2.goo.ne.jp/ej/voice/C/02020774.wav"><IMG LOWSRC="/ej/image/voice.gif" WIDTH="23" HEIGHT="12" BORDER="0" ALIGN="absmiddle"></A>$B!!(B
  ;;       </SPAN>
  ;;     </TD>
  ;;   </TR>
  ;; </TABLE>
  ;; <TABLE WIDTH="468" BORDER="0" CELLSPACING="0" CELLPADDING="0">
  ;;   <TR>
  ;;     <TD ALIGN="LEFT" VALIGN="TOP"><IMG SRC="/Common/clear.gif" WIDTH="68" HEIGHT="2"></TD>
  ;;     <TD WIDTH="400" ALIGN="LEFT" VALIGN="TOP">
  ;;       <SPAN CLASS="css3g">
  ;;         <i>n.</i>$B!!(B
  ;;       </SPAN>
  ;;     </TD>
  ;;   </TR>
  ;; </TABLE>
  ;;       <br>
  ;;     </td>
  ;;   </tr>
  ;;   <tr>
  ;;     <td bgcolor="#993333"><img src="/Common/clear.gif" width="1" height="1" alt=""></td>
  ;;   </tr>
  ;;   <tr>
  ;;     <td>
  ;;       $B"#!N(B<font color="#993333">collaborate</font>$B!O$N(BEXCEED$B1QOB<-E5$+$i$N8!:w7k2L!!(B
  ;;     </td>
  ;;   </tr>
  ;; </table>
  ;; <!-- ej_res1 -->
  ;; </td></tr></table>
  ;; <!-- RESULT_BLOCK -->
  ;;
  ;; con$B!&(Btem$B!&(Bpo$B!&(Bra$B!&(Bry
  ;; [clear] $B!|!|!|!|!|!|!|!|!|!|!|!|!|!|(B
  ;; [clear] a., n.$B!!F1;~Be$N!J?M!$;(;o!K!!(B(with)$B!(!!F1G/Np$N!J(B
  ;; $B?M!K!(!!8=Be$N!J?M!K!%!!(B
  ;;
  ;; *
  ;; $B"#!N(Bcontemporary$B!O$N(BEXCEED$B1QOB<-E5$+$i$N8!:w7k2L(B
  ;;
  ;; 14:$B"#!N(Bcollaborate$B!O$N(BEXCEED$B1QOB<-E5$+$i$N8!:w7k2L!!(B
  ;; 15:*
  ;; 16:
  ;; 17:col$B!&(Blab$B!&(Bo$B!&(Brate$B!!(B $B!!(B
  ;; 18:[clear] $B!|!|!|!|!|!|!|!|!|!|!|!|!!(B
  ;; 19:[clear] vi.$B!!6&$KF/$/!(!!6&F18&5f$9$k!!(B(with, on, in)$B!(!!(B
  ;; 20:        $BE(B&!N@jNN73!O$K6(NO$9$k!%!!(B
  ;; 21:[clear] collaboration$B!!(B $B!!(B
  ;; 22:[clear] n.$B!!(Bcollaborationism$B!!(Bn.$B!!(Bcollaborationist$B!!(Bn.$B!!!J(B
  ;; 23:        $BE(B&$X$N!K6(NO<T!%!!(B
  ;; 24:[clear] collaborative$B!!(B
  ;; 25:[clear] $B!|!|!|!|!|!|!|!|!|!|!|!|!|!|!!(B
  ;; 26:[clear] a.$B!!6&F1@):n$N!%!!(B
  ;; 27:[clear] collaborator$B!!(B $B!!(B
  ;; 28:[clear] n.$B!!(B
  ;; 29:
  ;; 30:*
  ;; 31:$B"#!N(Bcollaborate$B!O$N(BEXCEED$B1QOB<-E5$+$i$N8!:w7k2L!!(B
  ;;
  ;; $B"#!N(Bvery$B!O$N(BEXCEED$B1QOB<-E5$+$i$N8!:w7k2L!!(B 2$B7o(B
  ;; *
  ;;
  ;; 1  $B?75,$G3+$/(B  very
  ;;
  ;; 2  $B?75,$G3+$/(B  Very light
  ;; 
  ;; *
  ;; $B"#!N(Bvery$B!O$N(BEXCEED$B1QOB<-E5$+$i$N8!:w7k2L!!(B 2$B7o(B
  ;;
  ;; $B"#!N(Bcontemporary$B!O$N(BEXCEED$B1QOB<-E5$+$i$N8!:w7k2L(B
  ;; *
  ;;   (save-match-data
  ;;     (let (temp v start end)
  ;;       (if (not (search-forward "<!-- RESULT_BLOCK -->" nil t nil))
  ;; 	  nil
  ;; 	(setq start (point))
  ;; 	(if (search-forward "<!-- RESULT_BLOCK -->" nil t nil)
  ;; 	    (setq end (point)))
  ;; 	(goto-char start)
  ;; 	(setq key (concat "<a href=\".+\">" (regexp-quote key) " +$B!Z(B\\([^$B!Z![(B]+\\)$B![(B</a>"))
  ;; 	(while (re-search-forward key end t nil)
  ;; 	  (setq temp (skk-w3m-filter-string
  ;; 		      ;; $B!R2?;~!S(B
  ;; 		      (match-string-no-properties 1) '("$B!R(B" "$B!S(B")))
  ;; 	  (setq v (nconc (split-string temp "$B!&(B") v)))
  ;; 	(nreverse v)))))
  ;;   (save-match-data
  ;;     (let (v)
  ;;       (if (not (re-search-forward "[0-9]+  $B?75,$G3+$/(B" nil t nil))
  ;; 	  (if (re-search-forward
  ;; 	       (concat "$B"#(B\\$B!N(B" (regexp-quote key) "\\$B!O$N(BEXCEED$B1QOB<-E5$+$i$N8!:w7k2L(B")
  ;; 	       nil t nil)
  ;; 	      (setq v (skk-w3m-get-candidates-from-goo-exceed-eiwa-1)))
  ;; 	(beginning-of-line)
  ;; 	(while (re-search-forward "[0-9]+  $B?75,$G3+$/(B" nil t nil)
  ;; 	  (backward-char)
  ;; 	  (w3m-view-this-url)
  ;; 	  (goto-char (point-min))
  ;; 	  (if (re-search-forward
  ;; 	       (concat "$B"#(B\\$B!N(B" (regexp-quote key) "\\$B!O$N(BEXCEED$B1QOB<-E5$+$i$N8!:w7k2L(B")
  ;; 	       nil t nil)
  ;; 	      (setq v (nconc v (skk-w3m-get-candidates-from-goo-exceed-eiwa-1))))
  ;; 	  (w3m-view-previous-page)))
  ;;       v))
  )

;; (defun skk-w3m-get-candidates-from-goo-exceed-eiwa-1 ()
;;   (save-match-data
;;     (let (temp temp1 temp2 temp3 tail v)
;;       (while (re-search-forward
;; 	      "\\[clear\\] [a-z]+\\.\\(, [a-z]+\\.\\)*$B!!(B\\([^ a-zA-Z][^$B!%(B]+\\)$B!%(B"
;; 	      nil t nil)
;; 	(setq temp (match-string-no-properties 2))
;; 	(setq temp (skk-w3m-filter-string
;; 		  ;; e.x. `$BFh@w!J$J$D$;$s!K9)(B', `(on, in)', `$B!Z7P1D![(B'
;; 		    temp '("\n" "[0-9]+: +" "[$B!!(B ]+" "$B!J(B[$B$!(B-$B$s(B]+$B!K(B" "([, a-z]+)"
;; 			   "$B!D$N(B" "$B!Z(B[^$B!Z![(B]+$B![(B" "($B6/0U(B)")))
;; 	(while (string-match
;; 		;; ((...)) $B$O0UL#$rI=$o$9$h$&$@!#(B
;; 		;; e.x. $B%$%s%8%1!<%?!!(B(($B5!4o$N:nF0>uBV$rI=<($9$k5!G=(B))
;; 		;; $B3g8LFb$r$"$($F%U%#%k%?%j%s%0$7$J$$$G=PNO$9$k!#(B
;; 		"\\([^$B!$!((B]+\\)\\($B!N(B\\|((\\)\\([^$B!$!((B]+\\)\\($B!O(B\\|))\\)\\([^$B!$!((B]+\\)*"
;; 		temp)
;; 	  (setq temp (concat (substring temp 0 (match-beginning 0))
;; 			     (match-string-no-properties 1 temp)
;; 			     (match-string-no-properties 5 temp)
;; 			     "$B!$(B"
;; 			     (match-string-no-properties 3 temp)
;; 			     (match-string-no-properties 5 temp)
;; 			     (substring temp (match-end 0)))))
;; 	;; $BEvOG!J$N860x!K(B $B"*(B $BEvOG!$EvOG$N860x(B
;; 	;; $BF1;~Be$N!J?M!$;(;o!K"*(B  $BF1;~Be$N!$F1;~Be$N?M!$F1;~Be$N;(;o(B
;; 	(while (string-match "\\([^$B!$!((B]+\\)$B!J(B\\([^$B!((B]+\\)$B!K(B\\([^$B!$!((B]+\\)*" temp)
;; 	  (setq temp1 (match-string-no-properties 1 temp)
;; 		temp2 (match-string-no-properties 2 temp)
;; 		temp3 (match-string-no-properties 3 temp)
;; 		tail (substring temp (match-end 0)))
;; 	  (setq temp (concat (substring temp 0 (match-beginning 0))
;; 			     temp1 "$B!$(B"
;; 			     (mapconcat 'identity
;; 					(mapcar
;; 					 (function (lambda (e) (concat temp1 e temp3)))
;; 					 (split-string temp2 "$B!$(B"))
;; 					"$B!$(B")
;; 			     tail)))
;; 	;; $B!JLdBj$r!KJ65j$5$;$k(B $B"*(B $BJ65j$5$;$k!$LdBj$rJ65j$5$;$k(B
;; 	(while (string-match "$B!J(B\\([^$B!((B]+\\)$B!K(B\\([^$B!$!((B]+\\)" temp)
;; 	  (setq temp1 (match-string-no-properties 1 temp)
;; 		temp2 (match-string-no-properties 2 temp)
;; 		tail (substring temp (match-end 0)))
;; 	  (setq temp (concat (substring temp 0 (match-beginning 0))
;; 			     temp2 "$B!$(B"
;; 			     (mapconcat 'identity
;; 					(mapcar
;; 					 (function (lambda (e) (concat e temp2)))
;; 					 (split-string temp1 "$B!$(B"))
;; 					"$B!$(B")
;; 			     tail)))
;; 	(setq v (nconc v (split-string temp "[$B!$!((B]")))
;; 	;; skip to next candidate.
;; 	(or (re-search-forward "\\[clear\\] $B!|(B+" nil t nil)
;; 	    (goto-char (point-max))))
;;       v)))

(defun skk-w3m-get-candidates-from-goo-daily-shingo (key)
  ;; not yet.
  ;; 15:$B"#!N(BSPA$B!O$N%G%$%j!<?78l<-E5$+$i$N8!:w7k2L!!(B
  ;; 16:*
  ;; 17:
  ;; 18:SPA
  ;; 19:
  ;; 20:  $B!N(Bspeciality store retailer of private label apparel$B!O(B
  ;; 21:  $B<+<R%V%i%s%I$N0aNAIJ$rGd$kD>1DE9$N$3$H!#$^$?!$$=$N$h$&$J;v6H7ABV!#0aNAIJ$N4k(B
  ;; 22:  $B2h!&3+H/$+$i@=B$!&N.DL!&HNGd$K;j$k$^$G$r0l3g$7$F<h$j07$$!$8\5R$N%K!<%:$K8zN((B
  ;; 23:  $BE*$KBP1~$9$k!#(B
  ;; 24:  $B"*%W%i%$%Y!<%H(B-$B%V%i%s%I(B
  ;; 25:  $B!LFH<+%V%i%s%I0aNA$N@lLgE9HNGd6H<T$NN,!#%"%a%j%+$N0aNA>.GdE9$K$h$kB$8l$,5/8;(B
  ;; 26:  $B!M(B
  ;; 27:
  ;; 28:
  ;; 29:*
  ;; 30:$B"#!N(BSPA$B!O$N%G%$%j!<?78l<-E5$+$i$N8!:w7k2L!!(B
  )

(require 'product)
(product-provide (provide 'skk-w3m) (require 'skk-version))
;;; Local Variables:
;;; End:
;;; skk-w3m.el ends here
