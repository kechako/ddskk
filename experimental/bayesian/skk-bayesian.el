;; skk-bayesian.el -- Bayesian estimation for SKK
;; Copyright (C) 2004 Kenichi Kurihara <kenichi_kurihara@nifty.com>

;; Author: Kenichi Kurihara <kenichi_kurihara@nifty.com>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Version: $Id: skk-bayesian.el,v 1.2 2004/02/29 01:05:10 minakaji Exp $
;; Keywords: japanese
;; Last Modified: $Date: 2004/02/29 01:05:10 $

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
;; the Free Software Foundation Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; skk-study $B$,D>A0$NMzNr$N$_$r;HMQ$9$k$N$G!"$3$l$r3HD%$7$?$$$H;W$C$?$N(B
;; $B$,A4$F$NF05!$G$9!#(BSKK $B$H$=$N%3%_%e%K%F%#$K46<U$7$^$9!#(B
;;
;;
;; <$BF0:n(B>
;; $BNc(B: (skk-bayesian-prefix-len = 5 $B$N;~(B)
;; $B!V$=$NI~$r!"!W$N8e$K!"$-(Br $B$rJQ49$9$k>u67$K$*$$$F!"(B
;; entry $B$,!"(B("$B@Z(B" "$BCe(B" "$B;B(B") $B$G$"$k>u67$r9M$($k!#(B
;; $B$3$N(B enrty $B$r0J2<$N3NN($r7W;;$9$k$3$H$G!"%=!<%H$9$k!#(B
;;
;; Prob( word="$B@Z(B" | p_1="$B!"(B", p_2="$B$r(B", p_3="$BI~(B", p_4="$B$N(B", p_5="$B$=(B" )
;; Prob( word="$BCe(B" | p_1="$B!"(B", p_2="$B$r(B", p_3="$BI~(B", p_4="$B$N(B", p_5="$B$=(B" )
;; Prob( word="$B;B(B" | p_1="$B!"(B", p_2="$B$r(B", p_3="$BI~(B", p_4="$B$N(B", p_5="$B$=(B" )
;;
;; $B3X=,$9$Y$-%Q%i%a!<%?$N?t$r8:$i$9$?$a!"$3$N3NN(%b%G%k$r0J2<$N$h$&$J(B
;; $B:.9gJ,I[$G$"$k$H2>Dj$9$k!#(B
;;
;; Prob( word="$B@Z(B" | p_1="$B!"(B", p_2="$B$r(B", p_3="$BI~(B", p_4="$B$N(B", p_5="$B$=(B" )
;;   ~= \sum_{i=1}^5 w_i * Prob( word="$B@Z(B" | p_i )
;;
;; $B$?$@$7!"(Bw_i $B$O:.9gJ,I[$N=E$_$G$"$k!#(B
;;
;;
;; <$B2]Bj(B>
;; 1. bskk $B$,C1=c$K:n$i$l$F$$$k$N$G!"JQ49$NMzNr$,Bg$-$/$J$C$?;~$K!"F0:n(B
;;    $BB.EY$HI,MW$J%a%b%j$NNL$,?4G[!#(B
;; 2. $B:.9gJ,I[$N=E$_(B w_i $B$O8=:_!"(Bw_1, w_2, ..., w_n $B$KBP$7$F!"(B
;;    w_i : w_j = (n-i) : (n-j)
;;    $B$H$J$k$h$&$KCM$r7h$a$F$$$k!#K\Mh!"$$$:$l$b1#$lJQ?t$H$7$F!"(BEM$B%"%k(B
;;    $B%4%j%:%`(B, VBA $BEy$K$h$j3X=,$9$Y$-$+$b$7$l$J$$!#(B
;; 3. skk-bayesian-prefix-len $B$OJQ?t$K$7$F$$$k$N$G!"%f!<%6$,7hDj$G$-$k(B
;;    $B$,!"M}A[E*$K$O%b%G%k$N?dDjLdBj$H$H$i$($F!"3X=,%G!<%?$+$i7hDj$9$Y(B
;;    $B$-$@$m$&!#$^$?!"$"$kDxEY!"3X=,$7$?8e$K(B skk-bayesian-prefix-len$B$r(B
;;    $BBg$-$$CM$KJQ99$9$k$N$O!"?dDj$K0-1F6A$rM?$($=$&!#(B
;; 4. 2$B$H(B3$B$K=E$J$k$,!"Cx:n8"$N?4G[$r$7$J$/$F$b$h$$%3!<%Q%9$+$i!"3X=,$r9T$$(B
;;    skk-bayesian-prefix-len $B$H(B $B:.9gJ,I[$N=E$_$r7hDj$7$?$$!#(B
;; 5. bskk $B$H$N%W%m%H%3%k$,AG?M=-$$!#(B
;;
;;
;; <$B;H$$J}(B>
;; ~/.skk $B$K!"(B(require 'skk-bayesian) $B$H=q$$$F2<$5$$!#(B
;; skk-study $B$H$NJ;MQ$O5!G=$,=E$J$k$N$G!"$*4+$a$G$-$^$;$s!#(B
;;
;; $B$^$?!"(Bbskk $B$O!"%5!<%P$+%5%V%W%m%;%9$H$7$F;HMQ$7$^$9!#(B
;; *$B%5%V%W%m%;%9(B
;; $B%5%V%W%m%;%9$H$7$F;HMQ$9$k$K$O!"(Bbskk $B$r%Q%9$NDL$C$?>l=j$KCV$/$@$1$G$9!#(B
;; $BLdBj$O!"$$$/$D$b(B emacs $B$r5/F0$9$k$H(B ~/.skk-bayesian $B$O:G8e$K99?7$7$?(B
;; emacs $B$K0M$k$N$G!"B>$N(B emacs $B$G$N3X=,%G!<%?$OJ]B8$5$l$^$;$s!#(B
;; *$B%5!<%P(B
;; bskk $B$r%5!<%P$H$7$F;HMQ$9$k$K$O!"(Bskk-bayesian.el $B$,(B emacs $B$+$iFI$_9~(B
;; $B$^$l$kA0$K!"(B
;; % bskk -f ~/.skk-bayesian -s
;; $B$H$7$F!"N)$A>e$2$F$*$/I,MW$,$"$j$^$9!#(B

;;; Code:

(require 'skk-vars)
(require 'skk-macs)

(defvar skk-bayesian-prefer-server nil
  "*non-nil $B$J$i$P!"(B`skk-bayesian-server-port'$B$N(B`skk-bayesian-port'$B$K@\B3$9$k!#(B
$B$=$&$G$J$1$l$P!"(Bbskk $B$r%5%V%W%m%;%9$H$7$FN)$A>e$2$k!#(B")
(defvar skk-bayesian-port 51178
  "*`skk-bayesian-prefer-server'$B$,(B non-nil $B$N;~$K(B`skk-bayesian-host'$B$K@\B3$9$k%]!<%HHV9f(B")
(defvar skk-bayesian-host "localhost"
  "*`skk-bayesian-prefer-server'$B$,(B non-nil $B$N;~$K@\B3$9$k%[%9%H(B")
(defvar skk-bayesian-coding-system 'euc-jp)
(defvar skk-bayesian-prefix-len 20 "*$B3X=,$dM=B,$K;HMQ$9$k!"JQ498l$ND>A0$NJ8;z?t(B")
(defvar skk-bayesian-last-prefix-str nil "*$B3NDj8l$ND>A0$NJ8;zNs(B")
(defvar skk-bayesian-history-file "~/.skk-bayesian" "*history file")
(defvar skk-bayesian-debug nil "*$B%G%P%C%0MQ$N%a%C%;!<%8$rI=<((B")

(defconst skk-bayesian-command-sort "#sort\n")
(defconst skk-bayesian-command-add "#add\n")
(defconst skk-bayesian-command-save "#save\n")
(defvar skk-bayesian-process nil)

(defsubst skk-bayesian-debug-message (STRING &rest ARGS)
  (if skk-bayesian-debug
      (apply 'message STRING ARGS)))

(defsubst skk-bayesian-process-live-p ()
  "`skk-bayesian-process' $B$,(B non-nil $B$+$D$=$N%W%m%;%9$,<B9TCf$J$i(B t $B$rJV$9!#(B "
  (if skk-bayesian-process
      (let ((status (process-status skk-bayesian-process)))
        ;; $B%M%C%H%o!<%/%W%m%;%9$J$i!"(Bopen, $BDL>o$N%5%V%W%m%;%9$J$i!"(Brun$B!#(B
        ;; $B$3$l$i$O!"GSB>E*!#(B
        (or (eq status 'open)
            (eq status 'run)))))

(defun skk-bayesian-search (henkan-buffer midasi okurigana entry)
  ;; $B0z?t$NNc(B
  ;; entry : ("$B;B(B" "$B@Z(B" "$BCe(B")
  ;; midasi: $B$-(Br
  ;; okurigana: $B$k(B
  (setq skk-bayesian-last-prefix-str nil)
  (when (< 1 (length entry))
    (skk-bayesian-init)
    (let ((prefix-str "")
          (entry-str "")
          new-entry)
      ;; make entry-str
      (let ((e entry))
        (while e
          (setq entry-str (concat entry-str " " (car e)))
          (setq e (cdr e))))
      (skk-bayesian-debug-message (concat "entry-str=" entry-str))
      ;; make prefix-str
      (with-current-buffer henkan-buffer
	(let* ((just-before-point (- (point) (length midasi) 2))
	       (prefix-str-len 0)
	       char)
	  (while (and (<= (point-min) just-before-point)
		      (<= prefix-str-len skk-bayesian-prefix-len))
	    (setq char (buffer-substring-no-properties
			just-before-point (1+ just-before-point)))
	    (when (not (string-match "[[:cntrl:][:blank:]]" char))
	      (setq prefix-str (concat char " " prefix-str))
	      (setq prefix-str-len (1+ prefix-str-len)))
	    (setq just-before-point (1- just-before-point)))
	  ))
      (skk-bayesian-debug-message (concat "prefix-str=" prefix-str))
      ;; send prefix-str to skk-bayesian-process
      (with-current-buffer (process-buffer skk-bayesian-process)
	(delete-region (point-min) (point-max))
	(process-send-string skk-bayesian-process skk-bayesian-command-sort)
	(process-send-string skk-bayesian-process (concat entry-str "\n"))
	(process-send-string skk-bayesian-process (concat prefix-str "\n"))
	(while (not (and (> (point-max) 1)
			 (eq (char-after (1- (point-max))) ?\n)))
	  (accept-process-output skk-bayesian-process 0 5))
	(goto-char (point-min))
	(setq new-entry
	      (condition-case err
		  (read (current-buffer))
		(error (skk-message "Error while reading the out put of bskk; %s"
				    "bskk $B$N=PNO$NFI$_9~$_Cf$K%(%i!<(B; %s"
				    (error-message-string err))
		       nil))))
      (skk-bayesian-debug-message (concat "new-entry=" (prin1-to-string new-entry)))
      (if (and new-entry
               (listp new-entry))
          (progn
            (setq skk-bayesian-last-prefix-str prefix-str)
            new-entry)
        entry))))

(defun skk-bayesian-update (henkan-buffer midasi okurigana word purge)
  (when skk-bayesian-last-prefix-str
    (skk-bayesian-init)
    (skk-bayesian-debug-message (concat "kakutei-word=" word))
    (skk-bayesian-debug-message (concat "prefix=" skk-bayesian-last-prefix-str))
    (with-current-buffer (process-buffer skk-bayesian-process)
      (delete-region (point-min) (point-max))
      (process-send-string skk-bayesian-process skk-bayesian-command-add)
      (process-send-string skk-bayesian-process
			   (concat word "\n"))
      (process-send-string skk-bayesian-process
			   (concat skk-bayesian-last-prefix-str "\n"))
      ;; wait for a message to synchronize skk-bayesian-process
      (skk-bayesian-debug-message "add history...")
      (while (not (and (> (point-max) 1)
		       (eq (char-after (1- (point-max))) ?\n)))
	(accept-process-output skk-bayesian-process 0 5))
      (skk-bayesian-debug-message "add history...done.")
      )))

(defun skk-bayesian-save-history ()
  "Save skk-bayesian history to `skk-bayesian-history-file'."
  (interactive)
  (when (skk-bayesian-process-live-p)
    (with-current-buffer (process-buffer skk-bayesian-process)
      (delete-region (point-min) (point-max))
      (process-send-string skk-bayesian-process skk-bayesian-command-save)
      ;; wait for a message to synchronize skk-bayesian-process
      (skk-message "skk-bayesian $B$NMzNr$rJ]B8$7$F$$$^$9(B..." "saving history...")
      (while (not (and (> (point-max) 1)
		       (eq (char-after (1- (point-max))) ?\n)))
	(accept-process-output skk-bayesian-process 0 5))
      (skk-message "skk-bayesian $B$NMzNr$rJ]B8$7$F$$$^$9(B...$B40N;(B" "saving history...done")
      )))

(defun skk-bayesian-restart-process ()
  (if (skk-bayesian-process-live-p) (skk-bayesian-kill-process))
  (let  ((proc-buf (get-buffer-create (if skk-bayesian-debug
                                          "*skk-bayesian*"
                                        " *skk-bayesian*")))
         (proc-name "skk-bayesian"))
    (setq skk-bayesian-process
          (or (and skk-bayesian-prefer-server
                   (condition-case err
                       (open-network-stream proc-name
                                            proc-buf
                                            skk-bayesian-host skk-bayesian-port)
                     (error (skk-bayesian-debug-message "Error: %s\n%s"
                                                        (error-message-string err)
                                                        "run bskk as a sub process.")
                            nil)))
              (start-process proc-name
                             proc-buf
                             "ruby" "-S" "bskk" "-f" skk-bayesian-history-file
                             (if skk-bayesian-debug "-v" "")))))
  (set-process-coding-system skk-bayesian-process
                             skk-bayesian-coding-system
                             skk-bayesian-coding-system)
  (process-kill-without-query skk-bayesian-process))

(defun skk-bayesian-kill-process ()
  "Kill skk-bayesian process."
  (interactive)
  (when (skk-bayesian-process-live-p)
    (with-current-buffer (process-buffer skk-bayesian-process)
      (delete-region (point-min) (point-max))
      ;; send EOF
      (process-send-eof skk-bayesian-process)
      ;; wait for a skk-bayesian-debug-message to synchronize skk-bayesian-process
      (while (not (and (> (point-max) 1)
		       (eq (char-after (1- (point-max))) ?\n)))
	(accept-process-output skk-bayesian-process 0 5))
      (when (skk-bayesian-process-live-p)
	(skk-bayesian-debug-message "sent EOF, but the process still live.")
	;; send SIGKILL or close the connection
	(delete-process skk-bayesian-process))
      (setq skk-bayesian-process nil))))

(defun skk-bayesian-init ()
  "Set up skk-bayesian process."
  (interactive)
  (when (not (skk-bayesian-process-live-p))
    (skk-bayesian-restart-process)))

(provide 'skk-bayesian)

(add-to-list 'skk-search-end-function 'skk-bayesian-search)
(add-to-list 'skk-update-end-function 'skk-bayesian-update)
(add-hook 'skk-before-kill-emacs-hook 'skk-bayesian-save-history)
(skk-bayesian-init)

;;; skk-bayesian.el ends here
