;;; skk.el --- SKK (Simple Kana to Kanji conversion program)
;; Copyright (C) 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997,
;;               1998, 1999
;; Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>

;; Author: Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>
;; Maintainer: Hideki Sakurada <sakurada@kuis.kyoto-u.ac.jp>
;;             Murata Shuuichirou <mrt@astec.co.jp>
;;             Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Version: $Id: skk.el,v 1.25 2000/01/17 04:05:23 furue Exp $
;; Keywords: japanese
;; Last Modified: $Date: 2000/01/17 04:05:23 $

;; SKK is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either versions 2, or (at your option) any later
;; version.

;; SKK is distributed in the hope that it will be useful but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with SKK, see the file COPYING.  If not, write to the Free
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
(require 'skk-foreword)

(defconst skk-version "10.58")
(defconst skk-major-version (string-to-int (substring skk-version 0 2)))
(defconst skk-minor-version (string-to-int (substring skk-version 3)))

;;;###autoload
(defun skk-version ()
  (interactive)
  (if (not (interactive-p))
      skk-version
    (save-match-data
      (let* ((raw-date "$Date: 2000/01/17 04:05:23 $")
             (year (substring raw-date 7 11))
             (month (substring raw-date 12 14))
             (date (substring raw-date 15 17)) )
        (if (string-match "^0" month)
            (setq month (substring month (match-end 0))) )
        (if (string-match "^0" date)
            (setq date (substring date (match-end 0))) )
        (message "SKK version %s of %s, APEL inside"
                 skk-version
                 (concat (car (rassoc month skk-month-alist))
                         " " date ", " year ))))))

;;;; variables declaration
;;; user variables

(defvar skk-init-file (convert-standard-filename "~/.skk")
  "*SKK $B$N=i4|@_Dj%U%!%$%kL>!#(B
skk.el 9.x $B$h$j(B ~/.emacs $B$G$N%+%9%?%^%$%:$b2DG=$H$J$C$?!#(B"
;  "*Name of the SKK initialization file.
;From skk.el 9.x on all customization may be done in ~/.emacs."
)

;;;###autoload
(defgroup skk nil "SKK basic customization."
  :prefix "skk-"
  :group 'japanese
  :group 'input-method )

(defgroup skk-faces nil
  "Faces used by SKK."
  :group 'skk
  :group 'faces)

(defcustom skk-special-midashi-char-list '(?> ?< ??)
  "*$B@\F,<-!"@\Hx<-$NF~NO$N$?$a$N%W%l%U%#%C%/%9%-!<!"%5%U%#%C%/%9%-!<$N%j%9%H!#(B"
  ;;  "*List of prefix and suffix keys for entering `settoji' and `setsubiji'."
  :type '(repeat character)
  :group 'skk )

(defcustom skk-mode-hook nil
  "*SKK $B$r5/F0$7$?$H$-$N%U%C%/!#(B
$BB>$K!"(Bskk-auto-fill-mode-hook$B!"(Bskk-load-hook, skk-init-file $B$G$b%+%9%?(B
$B%^%$%:$,2DG=!#(B"
  ;; "*Hook run at SKK startup.  This hook is also run
  ;;in skk-auto-fill-mode after skk-auto-fill-mode-hook.
  ;;skk-auto-fill-mode-hook, skk-load-hook, skk-init-file may also be used
  ;;for customization."
  :type 'hook
  :group 'skk )

(defcustom skk-auto-fill-mode-hook nil
  "*skk-auto-fill-mode $B$r5/F0$7$?$H$-$N%U%C%/!#(B
$BB>$K!"(Bskk-mode-hook, skk-load-hook, skk-init-file $B$G$b%+%9%?%^%$%:$,2D(B
$BG=!#(B"
  ;;  "*Hook run at startup of skk-auto-fill-mode.
  ;;skk-mode-hook$B!"(Bskk-load-hook, skk-init-file may also be used for
  ;;customization."
  :type 'hook
  :group 'skk )

(defcustom skk-load-hook nil
  "*skk.el $B$r%m!<%I$7$?$H$-$N%U%C%/!#(B
$BB>$K!"(Bskk-mode-hook, skk-auto-fill-mode-hook, skk-init-file $B$G$b%+%9%?(B
$B%^%$%:$,2DG=!#(B"
  ;;  "*Hook run when SKK is loaded.
  ;;skk-auto-fill-mode-hook$B!"(Bskk-mode-hook, skk-init-file may also be used
  ;;for customization."
  :type 'hook
  :group 'skk )

(defcustom skk-search-end-function nil
  "*$BC18l8!:w=*N;;~$K%3!<%k$5$l$k4X?t!#(B
$B$3$N4X?t$rMxMQ$7$F8!:w$7$?C18l$NM%@h=g0L$rJQ99$9$k$J$I$N:n6H$,2DG=!#(B
HENKAN-BUFFER, MIDASI, OKURIGANA, ENTRY $B$N(B 4 $B0z?t$rH<$J$C$F%3!<%k$5$l$k!#(B
$B2C9)$7$?(B ENTRY $B$rJV$9$3$H!#(B
$B$3$N4X?t$O!"<-=q%P%C%U%!$G%3!<%k$5$l$k$N$G!"JQ49$r9T$J$C$?%P%C%U%!%m!<%+%k$J>pJs$r(B
$B<h$j=P$7$?$$$H$-$O!"(BHENKAN-BUFFER $B$rMxMQ$9$k!#(B"
  :type '(choice function (const nil))
  :group 'skk )
 
(defcustom skk-update-end-function nil
  "*$B8D?M<-=q$N99?7=*N;;~$K%3!<%k$5$l$k4X?t!#(B
HENKAN-BUFFER, MIDASI, OKURIGANA, WORD, PURGE $B$N(B 5 $B0z?t$rH<$J$C$F%3!<%k$5$l$k!#(B
$B$3$N4X?t$O!"<-=q%P%C%U%!$G%3!<%k$5$l$k$N$G!"JQ49$r9T$J$C$?%P%C%U%!%m!<%+%k$J>pJs$r<h$j(B
$B=P$7$?$$$H$-$O!"(BHENKAN-BUFFER $B$rMxMQ$9$k!#(B
skk-kakutei-initialize $B$,%3!<%k$5$l$kA0$K$3$N4X?t$,%3!<%k$5$l$k$N$G!":G8e$N3NDj(B
$B$K4X$9$k%U%i%0N`$O!"$3$N4X?t$NCf$+$i;2>H$9$k$3$H$,$G$-$k!#(B"
  :type '(choice function (const nil))
  :group 'skk )
  
(defcustom skk-kakutei-end-function nil
  "*$B3NDj;~$K%3!<%k$5$l$k4X?t!#(B
KAKUTEI-WORD $B0z?t$rH<$J$C$F!"JQ49$r9T$J$C$?%P%C%U%!$G%3!<%k$5$l$k!#(B
skk-kakutei-initialize $B$,%3!<%k$5$l$kA0$K$3$N4X?t$,%3!<%k$5$l$k$N$G!":G8e$N3NDj(B
$B$K4X$9$k%U%i%0N`$O!"$3$N4X?t$NCf$+$i;2>H$9$k$3$H$,$G$-$k!#(B" 
  :type '(choice function (const nil))
  :group 'skk )

(defcustom skk-kakutei-jisyo nil
  "*$B:G=i$K8!:w$9$k<-=q!#(B
Non-nil $B$G!"$+$D(B skk-search-prog-list $B$NMWAG$NCf$K$3$NJQ?t$,;HMQ$5$l$F$$$l$P!"(B
$B;XDj$5$l$?<-=q$r8!:w$N$?$a%P%C%U%!$KFI$_9~$_!"8!:w$r9T$J$&!#(B
$B8+=P$78l$O!"%=!<%H$5$l$F$$$J$1$l$P$J$i$J$$!#(B
$B3F8+=P$78l$N:G=i$N%(%s%H%j$7$+8!:w$7$J$$(B ($BJ#?t$N%(%s%H%j$,$"$C$F$b(B 2 $BHVL\0J9_$N(B
$B%(%s%H%j$OL5;k$5$l$k(B)$B!#(B
skk-search-prog-list $B$NCM$r@_Dj$9$k$3$H$K$h$j!"8!:wBP>]$N<-=q$NJQ99!"8!:w$N=g(B
$B=x$NJQ99$,2DG=!#(B"
  ;;  "*The first dictionary to be searched.
  ;;If non-nil, and this variable is used as a component of
  ;;`skk-search-prog-list', the indicated dictionary is read into a
  ;;buffer and searched.
  ;;The keys must be sorted.
  ;;Only the first entry in each key is checked; if several entries are
  ;;present the second and following entries are ignored.
  ;;By setting the value of `skk-search-prog-list' the dictionaries
  ;;searched and the order of search can be changed."
  :type '(choice file (const nil))
  :group 'skk )

(defcustom skk-initial-search-jisyo nil
  "*$B%f!<%6!<<-=q$N8!:w$NA0$K8!:w$9$k<-=q!#(B
$B8+=P$78l$O!"%=!<%H$5$l$F$$$J$1$l$P$J$i$J$$!#(B
Non-nil $B$G!"$+$D(B skk-search-prog-list $B$NMWAG$NCf$K$3$NJQ?t$,;HMQ$5$l$F$$$l$P!"(B
$B;XDj$5$l$?<-=q$r8!:w$N$?$a%P%C%U%!$KFI$_9~$_!"8!:w$r9T$J$&!#(B
skk-search-prog-list $B$NCM$r@_Dj$9$k$3$H$K$h$j!"8!:wBP>]$N<-=q$NJQ99!"8!:w$N=g(B
$B=x$NJQ99$,2DG=!#(B"
  ;;  "*This dictionary is searched before the user's personal dictionary.
  ;;The keys must be sorted.
  ;;If non-nil, and this variable is used as a component of
  ;;`skk-search-prog-list', the indicated dictionary is read into a
  ;;buffer and searched.
  ;;By setting the value of `skk-search-prog-list' the dictionaries
  ;;searched and the order of search can be changed."
  :type '(choice file (const nil))
  :group 'skk )

(defcustom skk-large-jisyo nil
  "*$B%f!<%6!<<-=q$N8!:w$N8e$K8!:w$9$k<-=q!#(B
$B8+=P$78l$O!"%=!<%H$5$l$F$$$J$1$l$P$J$i$J$$!#(B
Non-nil $B$G!"$+$D(B skk-search-prog-list $B$NMWAG$NCf$K$3$NJQ?t$,;HMQ$5$l$F$$$l$P!"(B
$B;XDj$5$l$?<-=q$r8!:w$N$?$a%P%C%U%!$KFI$_9~$_!"8!:w$r9T$J$&!#(B
skk-search-prog-list $B$NCM$r@_Dj$9$k$3$H$K$h$j!"8!:wBP>]$N<-=q$NJQ99!"8!:w$N=g(B
$B=x$NJQ99$,2DG=!#(B" 
  :type '(choice file (const nil))
  :group 'skk )

(defcustom skk-aux-large-jisyo nil
  "*SKK $B%5!<%P!<$G:G8e$K8!:w$9$k<-=q!#(B
$B8+=P$78l$O!"%=!<%H$5$l$F$$$J$1$l$P$J$i$J$$!#(B
Non-nil $B$G!"$+$D(B skk-search-prog-list $B$NMWAG$NCf$K$3$NJQ?t$,;HMQ$5$l$F$$$l$P!"(B
SKK $B%5!<%P!<$r;H$$8!:w$r9T$&!#(B
SKK $B%5!<%P!<$,(B active $B$G$J$1$l$P!";XDj$5$l$?<-=q$r%P%C%U%!$KFI$_9~$`!#(B
skk-search-prog-list $B$NCM$r@_Dj$9$k$3$H$K$h$j!"8!:wBP>]$N<-=q$NJQ99!"8!:w$N=g(B
$B=x$NJQ99$,2DG=!#(B
$B$3$NCM$r@_Dj$9$k$3$H$K$h$j!"(Bskk-server.el $B$,(B autoload $B$5$l$k!#(B" 
  :type '(choice file (const nil))
  :group 'skk )

(defcustom skk-search-prog-list
  '((skk-search-kakutei-jisyo-file skk-kakutei-jisyo 10000 t)
    (skk-search-jisyo-file skk-initial-search-jisyo 10000 t)
    (skk-search-jisyo-file skk-jisyo 0 t)
    ;; skk-auto.el $B$r%m!<%I$9$k$H2<5-$NMWAG$,%W%i%9$5$l$k!#(B
    ;;(skk-okuri-search)
    (skk-search-jisyo-file skk-large-jisyo 10000)
    ;; skk-server.el $B$r%m!<%I$9$k$H2<5-$NMWAG$,%W%i%9$5$l$k!#(B
    ;;(skk-search-server skk-aux-large-jisyo 10000)
    ;; skk-server-host $B$b$7$/$O(B skk-servers-list $B$r;XDj$9$k$H!"(Bskk-server.el 
    ;; $B$,(B autoload $B$5$l$k!#(B
    )
  "*$B8!:w4X?t!"8!:wBP>]$N<-=q$r7hDj$9$k$?$a$N%j%9%H!#(B
$BJQ49$7$?8uJd$rJV$9(B S $B<0$r%j%9%H$N7A$KI=5-$7$?$b$N!#(B
skk-search $B4X?t$,(B skk-search-prog-list $B$N(B car $B$+$i8eJ}8~$X=gHV$K(B S $B<0$NI>2A$r(B
$B9T$$JQ49$r9T$J$&!#(B" 
  :type '(repeat
	  (list (function :tag "Search funcition")
		(choice :tag "Dictionary" file (const nil))
		(choice :tag "Minimum region size to be binary-searched"
			integer (const nil) )
		(choice :tag "Quietly reading dictionary to Emacs buffer"
			(const t) (const nil) )))
  :group 'skk )

(defcustom skk-jisyo (convert-standard-filename "~/.skk-jisyo")
  "*SKK $B$N%f!<%6!<<-=q!#(B" 
  :type 'file
  :group 'skk )

(defcustom skk-backup-jisyo (convert-standard-filename "~/.skk-jisyo.BAK")
  "*SKK $B$N%f!<%6!<<-=q$N%P%C%/%"%C%W%U%!%$%k!#(B" 
  :type 'file
  :group 'skk )

(defcustom skk-jisyo-code nil
  "*Non-nil $B$G$"$l$P!"$=$NCM$G<-=q%P%C%U%!$N4A;z%3!<%I$r@_Dj$9$k!#(B
Mule $B$G$O!"(B*euc-japan*, *sjis*, *junet*$B!#(B
$B$^$?!"(B\"euc\", \"ujis\", \"sjis\", \"jis\" $B$J$I$NJ8;zNs$K$h$C$F$b;XDj$,2DG=!#(B" 
  :type '(choice symbol string)
  :group 'skk )

(defcustom skk-keep-record t
  "*Non-nil $B$G$"$l$P!"JQ49$K4X$9$k5-O?$r(B skk-record-file $B$K<h$k!#(B"
  :type 'boolean
  :group 'skk )

(defcustom skk-record-file (convert-standard-filename "~/.skk-record")
  "*$B%f!<%6!<<-=q$NE}7W$r<h$k%U%!%$%k!#(B
$B<-=q%;!<%V$N;~9o!"C18l$NEPO??t!"3NDj$r9T$C$?2s?t!"3NDjN(!"A4BN$N8l?t$N(B
$B>pJs$r<}$a$k!#(B" 
  :type 'file
  :group 'skk )

(defcustom skk-kakutei-key "\C-j"
  "*$B4A;zJQ49$N3NDjF0:n$r9T$&%-!<!#(B"
  :type 'string
  :group 'skk )

(defcustom skk-previous-candidate-char ?x
  "*skk-previous-candidate $B$r3dEv$F$?%-!<%-%c%i%/%?!#(B" 
  :type 'character
  :group 'skk )

(defcustom skk-try-completion-char ?\011 ; TAB 
  "*$B8+=P$78l$NJd40F0:n$r9T$J$&%-!<%-%c%i%/%?!#(B" 
  :type 'character
  :group 'skk )

(defcustom skk-next-completion-char ?.
  "*$B8+=P$78l$NJd40F0:n$G!"<!$N8uJd$r=PNO$9$k%-!<%-%c%i%/%?!#(B" 
  :type 'character
  :group 'skk )

(defcustom skk-previous-completion-char ?,
  "*$B8+=P$78l$NJd40F0:n$G!"A0$N8uJd$r=PNO$9$k%-!<%-%c%i%/%?!#(B" 
  :type 'character
  :group 'skk )

(defcustom skk-start-henkan-char ?\040	; SPC
  "*$B4A;zJQ49$r3+;O$9$k%-!<%-%c%i%/%?!#(B" 
  :type 'character
  :group 'skk )

(defcustom skk-start-henkan-with-completion-char ?\240 ; M-SPC
  "*$B8+=P$78l$rJd40$7$J$,$i"'%b!<%I$KF~$k%-!<%-%c%i%/%?!#(B" 
  :type 'character
  :group 'skk )

(defcustom skk-backward-and-set-henkan-point-char ?\321 ; M-Q
  "*$B%]%$%s%H$rLa$7$F"&%b!<%I$KF~$k%-!<%-%c%i%/%?!#(B" 
  :type 'character
  :group 'skk )

(defcustom skk-use-viper nil
  "*Non-nil $B$G$"$l$P!"(BVIPER $B$KBP1~$9$k!#(B" 
  :type 'boolean
  :group 'skk )

(defcustom skk-henkan-okuri-strictly nil
  "*Non-nil $B$G$"$l$P!"8+=P$78l$HAw$j2>L>$,0lCW$7$?$H$-$@$18uJd$H$7$F=PNO$9$k!#(B
$BNc$($P!"2<5-$N$h$&$J<-=q%(%s%H%j$,!"(Bskk-jisyo \($B%W%i%$%Y!<%H<-=q(B\) $B$K$"$C$?>l9g$K(B

  \"$B$*$*(Bk /$BBg(B/$BB?(B/[$B$/(B/$BB?(B/]/[$B$-(B/$BBg(B/]/\"

\"$B"&$*$*(B*$B$/(B\" $B$rJQ49$7$?$H$-!"(B\"$BB?$/(B\" $B$N$_$r=PNO$7!"(B\"$BBg$/(B\" $B$r=PNO$7$J$$!#(B

SKK-JISYO.[SML] $B$NAw$j2>L>%(%s%H%j$O>e5-$N7A<0$K$J$C$F$$$J$$$N$G!"(Bskk-jisyo $B$N(B
$BAw$j$"$j$N<-=q%(%s%H%j$,$3$N7A<0$N$b$N$r$"$^$j4^$s$G$$$J$$>l9g$O!"$3$N%*%W%7%g(B
$B%s$r(B on $B$K$9$k$3$H$G!"$9$0$KC18lEPO?$KF~$C$F$7$^$&$N$GCm0U$9$k$3$H!#(B

skk-process-okuri-early $B$NCM$,(B nil $B$J$i$P>e5-$N7A<0$G(B skk-jisyo $B$,:n$i$l$k!#(B

Emacs 19 $B%Y!<%9$N(B Mule $B$J$i$P!"2<5-$N%U%)!<%`$rI>2A$9$k$3$H$G!"C18lEPO?$KF~$C(B
$B$?$H$-$@$10l;~E*$K$3$N%*%W%7%g%s$r(B nil $B$K$9$k$3$H$,$G$-$k!#(B

    \(add-hook 'minibuffer-setup-hook
              \(function
               \(lambda \(\)
                 \(if \(and \(boundp 'skk-henkan-okuri-strictly\)
                          skk-henkan-okuri-strictly
                          \(not \(eq last-command 'skk-purge-from-jisyo\)\) \)
                     \(progn
                       \(setq skk-henkan-okuri-strictly nil\)
                       \(put 'skk-henkan-okuri-strictly 'temporary-nil t\) \)\)\)\)\)

    \(add-hook 'minibuffer-exit-hook
              \(function
               \(lambda \(\)
                 \(if \(get 'skk-henkan-okuri-strictly 'temporary-nil\)
                     \(progn
                       \(put 'skk-henkan-okuri-strictly 'temporary-nil nil\)
                       \(setq skk-henkan-okuri-strictly t\) \)\)\)\)\)

$B$3$N%*%W%7%g%sMxMQ;~$O!"(Bskk-process-okuri-early $B$NCM$O(B nil $B$G$J$1$l$P$J$i$J$$(B
\($B%a%K%e!<%P!<$rMxMQ$7$F%+%9%?%^%$%:$7$?>l9g$O<+F0E*$KD4@0$5$l$k(B\)$B!#(B" 
  :type 'boolean
  :group 'skk )

(defcustom skk-henkan-strict-okuri-precedence nil
  "*Non-nil $B$G$"$l$P!"8+=P$78l$HAw$j2>L>$,0lCW$7$?8uJd$rM%@h$7$FI=<($9$k!#(B
$BNc$($P!"2<5-$N$h$&$J<-=q%(%s%H%j$,!"(Bskk-jisyo \($B%W%i%$%Y!<%H<-=q(B\) $B$K$"$C$?>l9g$K(B

  \"$B$*$*(Bk /$BBg(B/$BB?(B/[$B$/(B/$BB?(B/]/[$B$-(B/$BBg(B/]/\"

\"$B"&$*$*(B*$B$/(B\" $B$rJQ49$7$?$H$-!"$^$:(B\"$BB?$/(B\" $B$r=PNO$7!"(B
$B<!$K(B \"$BBg$/(B\" $B$r=PNO$9$k!#(B

\"$BBg$/(B\"$B$J$I$N8uJd$O$&$C$H$&$7$$$,!"$9$0$KC18lEPO?$K$O$$$C$F$7$^$&$N$b(B
$B7y$J$R$H$K$*$9$9$a!#(B

$B$3$N%*%W%7%g%sMxMQ;~$O!"(Bskk-process-okuri-early $B$NCM$O(B nil $B$G$J$1$l$P$J$i$J$$!#(B
$B$^$?(B skk-henkan-okuri-strictly $B$,(B non-nil $B$N$H$-$O!"$3$NJQ?t$OL5;k$5$l$k!#(B
\($B%a%K%e!<%P!<$rMxMQ$7$F%+%9%?%^%$%:$7$?>l9g$O<+F0E*$KD4@0$5$l$k(B\)$B!#(B"
  :type 'boolean
  :group 'skk )
 
(defcustom skk-auto-okuri-process nil
  "*Non-nil $B$G$"$l$P!"Aw$j2>L>ItJ,$r<+F0G'<1$7$FJQ49$r9T$&!#(B
$BNc$($P!"(B

    \"Uresii (\"UreSii\" $B$G$O$J$/(B) -> $B4r$7$$(B\"

$B$N$h$&$KJQ49$5$l$k!#C"$7!"(Bskk-jisyo $B<-=q(B \($B%W%i%$%Y!<%H<-=q(B\) $B$,!"(B

    \"$B$&$l(Bs /$B4r(B/[$B$7(B/$B4r(B/]/\"

$B$N$h$&$J7A<0$K$J$C$F$$$k$3$H$,I,MW$G$"$k(B \(SKK-JISYO.[SML] $B$O$3$N7A<0$KBP1~$7(B
$B$F$$$J$$$N$G!"(Bskk-jisyo $B$K$3$N%(%s%H%j$,$J$1$l$P$J$i$J$$(B\)$B!#(B

$B$3$N%*%W%7%g%sMxMQ;~$O!"(Bskk-process-okuri-early $B$NCM$O(B nil $B$G$J$1$l$P$J$i$J$$(B
\($B%a%K%e!<%P!<$rMxMQ$7$F%+%9%?%^%$%:$7$?>l9g$O<+F0E*$KD4@0$5$l$k(B\)$B!#(B" 
  :type 'boolean
  :group 'skk )

(defcustom skk-process-okuri-early nil
  "*Non-nil $B$G$"$l$P!"Aw$j2>L>$N%m!<%^;z%W%l%U%#%C%/%9$NF~NO;~E@$GJQ49$r3+;O$9$k!#(B
$BNc$($P!"(B

    \"UgoK -> $B"'F0(Bk\"$B!#(B

$BAw$j2>L>$,J,$i$J$$$^$^JQ49$7$F$$$k$3$H$K$J$k$N$G!"(Bskk-jisyo $B$,Aw$j2>L>$KBP1~$7(B
$B$?7A$K@.D9$7$J$$!#$D$^$j(B

    \"$B$&$4(Bk /$BF0(B/\"

$B$N$h$&$J7ABV$N$^$^$H$J$k!#$?$@$7!"4{$K(B

    \"$B$&$4(Bk /$BF0(B/[$B$/(B/$BF0(B/]/[$B$+(B/$BF0(B/]/[$B$1(B/$BF0(B/]/[$B$-(B/$BF0(B/]/[$B$3(B/$BF0(B/]/\"

$B$N$h$&$J%(%s%H%j$,(B skk-jisyo $B$K$"$l$P!"$=$l$rGK2u$7$J$$!#(B

nil $B$G$"$l$P!"Aw$j2>L>$NF~NO$,40N;$7$?;~E@$GJQ49$,3+;O$9$k!#Nc$($P!"(B

    \"UgoK -> $B"&$&$4(B*k\", \"UgoKu -> $B"'F0$/(B\"

$B$3$N%*%W%7%g%s$r(B on $B$K$7$F(B skk-mode $B$r5/F0$9$k$H!"N>N)$G$-$J$$%*%W%7%g%s$G$"$k(B
skk-kakutei-early, skk-auto-okuri-process, skk-henkan-okuri-strictly $B$O(B nil $B$K(B
$B%;%C%H$5$l$k!#(B" 
  :type 'boolean
  :group 'skk )

(defcustom skk-egg-like-newline nil
  "*Non-nil $B$G$"$l$P!""'%b!<%I$G2~9T$r%?%$%W$7$F$b3NDj$9$k$N$_$G2~9T$7$J$$!#(B" 
  :type 'boolean
  :group 'skk )

(defcustom skk-kakutei-early t
  "*Non-nil $B$G$"$l$P(B skk-insert $B$,8F$P$l$?$H$-$K8=:_$N8uJd$r3NDj$9$k!#(B
$BNc$($P!"(B

    \"$B"&$+$/$F$$(B -> $B"'3NDj(B -> $B3NDj(Bs -> $B3NDj$9(B\"

$B$N$h$&$KJQ498e!"!V$9!W$N(B prefix $B$G$"$k(B \"s\" $B$rF~NO$7$?;~E@$G3NDj$9$k!#(B
nil $B$G$"$l$P!"Nc$($P(B

    \"$B"&$+$/$F$$(B -> $B"'3NDj(B -> $B"'3NDj(Bs -> $B"'3NDj$9$k(B -> $B3NDj$9$k!#(B\"

$B$N$h$&$K(B skk-kakutei $B$rD>@\!"4V@\$K%3!<%k$9$k$^$G(B \($B6gFIE@$rF~NO$7$?$j!"?7$?$J(B
$B"&%b!<%I$KF~$C$?$j$9$k$H4V@\E*$K(B skk-kakutei $B$r%3!<%k$9$k(B\) $B$O!"3NDj$7$J$$$N$G!"(B
$B$=$N4V$O!"JQ498uJd$rA*$S$J$*$9$3$H$J$I$,2DG=!#(B

$B$3$N%*%W%7%g%sMxMQ;~$O!"(Bskk-process-okuri-early $B$NCM$O(B nil $B$G$J$1$l$P$J$i$J$$(B
\($B%a%K%e!<%P!<$rMxMQ$7$F%+%9%?%^%$%:$7$?>l9g$O<+F0E*$KD4@0$5$l$k(B\)$B!#(B" 
  :type 'boolean
  :group 'skk )

(defcustom skk-delete-implies-kakutei t
  "*Non-nil $B$G$"$l$P!""'%b!<%I$G(B BS $B$r2!$9$H!"A0$N0lJ8;z$r:o=|$73NDj$9$k!#(B
nil $B$G$"$l$P!"0l$DA0$N8uJd$rI=<($9$k!#(B"
  :type 'boolean
  :group 'skk )

(defcustom skk-allow-spaces-newlines-and-tabs t
  "*Non-nil $B$G$"$l$P!"8+=P$78l$NCf$K%9%Z!<%9!"%?%V!"2~9T$,$"$C$F$b$=$l$r<h$j=|$$$FJQ49$9$k$3$H$,2DG=!#(B
$BNc$($P!"2<5-$N$h$&$K(B $BCf$K2~9T$,F~$C$F$$$F$bJQ49$,2DG=$G$"$k!#(B

     \"$B"&$+(B
  $B$J(B\"
   -> \"$B2>L>(B\"

$B$3$NCM$,(B nil $B$G$"$l$P!":G=i$N%9%Z!<%9$G8+=P$78l$r@Z$j5M$a$F$7$^$$!"0J9_$N%9%Z!<(B
$B%9!"%?%V!"2~9T$OL5;k$5$l$k!#(B
$B$3$NCM$O!"(Bskk-start-henkan, skk-latin-henkan, skk-katakana-henkan,
skk-hiragana-henkan, skk-jisx0208-latin-henkan $B5Z$S(B
skk-backward-and-set-henkan-point $B$NF0:n$K1F6A$9$k!#(B"
  :type 'boolean
  :group 'skk )

(defcustom skk-convert-okurigana-into-katakana nil
  "*Non-nil $B$G$"$l$P!"%+%?%+%J%b!<%I$GJQ49$7$?$H$-$KAw$j2>L>$b%+%?%+%J$KJQ49$9$k!#(B" 
  :type 'boolean
  :group 'skk )

(defcustom skk-delete-okuri-when-quit nil
  "*Non-nil $B$G$"$l$P!"Aw$j$"$j$NJQ49Cf$K(B \"C-g\" $B$r2!$9$HAw$j2>L>$r>C$7"&%b!<%I$KF~$k!#(B
$BNc$($P!"(B

    \"$B"&$J(B*$B$/(B -> $B"'5c$/(B -> \"C-g\" ->$B"&$J(B\"

nil $B$G$"$l$P!"Aw$j2>L>$r4^$a$?8+=P$78l$r$=$N$^$^;D$7!""#%b!<%I$KF~$k!#Nc$($P!"(B

    \"$B"&$J(B*$B$/(B -> $B"'5c$/(B -> \"C-g\" -> $B$J$/(B\"" 
  :type 'boolean
  :group 'skk )

(defcustom skk-henkan-show-candidates-keys '(?a ?s ?d ?f ?j ?k ?l)
  "*$B%a%K%e!<7A<0$G8uJd$rA*Br$9$k$H$-$NA*Br%-!<$N%j%9%H!#(B
\"x\", \" \" $B5Z$S(B \"C-g\" $B0J30$N(B 7 $B$D$N%-!<(B (char type) $B$r4^$`I,MW$,$"(B
$B$k!#(B\"x\", \" \" $B5Z$S(B \"C-g\" $B$O8uJdA*Br;~$K$=$l$>$lFCJL$J;E;v$K3d$jEv(B
$B$F$i$l$F$$$k$N$G!"$3$N%j%9%H$NCf$K$O4^$a$J$$$3$H!#(B"
  :type '(repeat character)
  :group 'skk )

(defcustom skk-status-indicator 'minor-mode
  "*SKK $B$N>uBV$r%b!<%I9T$N$I$3$KI=<($9$k$+$r7h$a$k!#(B
left $B$G$"$l$P:8C<$KI=<($9$k!#(B
$B$5$b$J$1$l$P%^%$%J!<%b!<%I$H$7$F$NI=<(K!$r<h$k!#(B"
  :type '(choice (const minor-mode)
		 (const left))
  :group 'skk )

(defcustom skk-latin-mode-string " SKK"
  "*SKK $B$,(B latin (ascii) $B%b!<%I$G$"$k$H$-$K%b!<%I%i%$%s$KI=<($5$l$kJ8;zNs!#(B" 
  :type 'string
  :group 'skk )

(defcustom skk-hiragana-mode-string " $B$+$J(B"
  "*$B$R$i$,$J%b!<%I$G$"$k$H$-$K%b!<%I%i%$%s$KI=<($5$l$kJ8;zNs!#(B"
  :type 'string
  :group 'skk )

(defcustom skk-katakana-mode-string " $B%+%J(B"
  "*$B%+%?%+%J%b!<%I$G$"$k$H$-$K%b!<%I%i%$%s$KI=<($5$l$kJ8;zNs!#(B"
  :type 'string
  :group 'skk )

(defcustom skk-jisx0208-latin-mode-string " $BA41Q(B"
  "*$BA41Q%b!<%I$G$"$k$H$-$K%b!<%I%i%$%s$KI=<($5$l$kJ8;zNs!#(B"
  :type 'string
  :group 'skk )

(defcustom skk-abbrev-mode-string " a$B$"(B"
  "*SKK abbrev $B%b!<%I$G$"$k$H$-$K%b!<%I%i%$%s$KI=<($5$l$kJ8;zNs!#(B"
  :type 'string
  :group 'skk )

(defcustom skk-echo t
  "*Non-nil $B$G$"$l$P!"2>L>J8;z$N%W%l%U%#%C%/%9$rI=<($9$k!#(B" 
  :type 'boolean
  :group 'skk )

(defcustom skk-use-numeric-conversion t
  "*Non-nil $B$G$"$l$P!"?tCMJQ49$r9T$&!#(B" 
  :type 'boolean
  :group 'skk )

(defcustom skk-rom-kana-base-rule-list
  '(("a" nil ("$B%"(B" . "$B$"(B"))
    ("bb" "b" ("$B%C(B" . "$B$C(B"))
    ("ba" nil ("$B%P(B" . "$B$P(B"))
    ("be" nil ("$B%Y(B" . "$B$Y(B"))
    ("bi" nil ("$B%S(B" . "$B$S(B"))
    ("bo" nil ("$B%\(B" . "$B$\(B"))
    ("bu" nil ("$B%V(B" . "$B$V(B"))
    ("bya" nil ("$B%S%c(B" . "$B$S$c(B"))
    ("bye" nil ("$B%S%'(B" . "$B$S$'(B"))
    ("byi" nil ("$B%S%#(B" . "$B$S$#(B"))
    ("byo" nil ("$B%S%g(B" . "$B$S$g(B"))
    ("byu" nil ("$B%S%e(B" . "$B$S$e(B"))
    ("cc" "c" ("$B%C(B" . "$B$C(B"))
    ("cha" nil ("$B%A%c(B" . "$B$A$c(B"))
    ("che" nil ("$B%A%'(B" . "$B$A$'(B"))
    ("chi" nil ("$B%A(B" . "$B$A(B"))
    ("cho" nil ("$B%A%g(B" . "$B$A$g(B"))
    ("chu" nil ("$B%A%e(B" . "$B$A$e(B"))
    ("cya" nil ("$B%A%c(B" . "$B$A$c(B"))
    ("cye" nil ("$B%A%'(B" . "$B$A$'(B"))
    ("cyi" nil ("$B%A%#(B" . "$B$A$#(B"))
    ("cyo" nil ("$B%A%g(B" . "$B$A$g(B"))
    ("cyu" nil ("$B%A%e(B" . "$B$A$e(B"))
    ("dd" "d" ("$B%C(B" . "$B$C(B"))
    ("da" nil ("$B%@(B" . "$B$@(B"))
    ("de" nil ("$B%G(B" . "$B$G(B"))
    ("dha" nil ("$B%G%c(B" . "$B$G$c(B"))
    ("dhe" nil ("$B%G%'(B" . "$B$G$'(B"))
    ("dhi" nil ("$B%G%#(B" . "$B$G$#(B"))
    ("dho" nil ("$B%G%g(B" . "$B$G$g(B"))
    ("dhu" nil ("$B%G%e(B" . "$B$G$e(B"))
    ("di" nil ("$B%B(B" . "$B$B(B"))
    ("do" nil ("$B%I(B" . "$B$I(B"))
    ("du" nil ("$B%E(B" . "$B$E(B"))
    ("dya" nil ("$B%B%c(B" . "$B$B$c(B"))
    ("dye" nil ("$B%B%'(B" . "$B$B$'(B"))
    ("dyi" nil ("$B%B%#(B" . "$B$B$#(B"))
    ("dyo" nil ("$B%B%g(B" . "$B$B$g(B"))
    ("dyu" nil ("$B%B%e(B" . "$B$B$e(B"))
    ("e" nil ("$B%((B" . "$B$((B"))
    ("ff" "f" ("$B%C(B" . "$B$C(B"))
    ("fa" nil ("$B%U%!(B" . "$B$U$!(B"))
    ("fe" nil ("$B%U%'(B" . "$B$U$'(B"))
    ("fi" nil ("$B%U%#(B" . "$B$U$#(B"))
    ("fo" nil ("$B%U%)(B" . "$B$U$)(B"))
    ("fu" nil ("$B%U(B" . "$B$U(B"))
    ("fya" nil ("$B%U%c(B" . "$B$U$c(B"))
    ("fye" nil ("$B%U%'(B" . "$B$U$'(B"))
    ("fyi" nil ("$B%U%#(B" . "$B$U$#(B"))
    ("fyo" nil ("$B%U%g(B" . "$B$U$g(B"))
    ("fyu" nil ("$B%U%e(B" . "$B$U$e(B"))
    ("gg" "g" ("$B%C(B" . "$B$C(B"))
    ("ga" nil ("$B%,(B" . "$B$,(B"))
    ("ge" nil ("$B%2(B" . "$B$2(B"))
    ("gi" nil ("$B%.(B" . "$B$.(B"))
    ("go" nil ("$B%4(B" . "$B$4(B"))
    ("gu" nil ("$B%0(B" . "$B$0(B"))
    ("gya" nil ("$B%.%c(B" . "$B$.$c(B"))
    ("gye" nil ("$B%.%'(B" . "$B$.$'(B"))
    ("gyi" nil ("$B%.%#(B" . "$B$.$#(B"))
    ("gyo" nil ("$B%.%g(B" . "$B$.$g(B"))
    ("gyu" nil ("$B%.%e(B" . "$B$.$e(B"))
    ;;("h" "" ("$B%*(B" . "$B$*(B"))
    ("ha" nil ("$B%O(B" . "$B$O(B"))
    ("he" nil ("$B%X(B" . "$B$X(B"))
    ("hi" nil ("$B%R(B" . "$B$R(B"))
    ("ho" nil ("$B%[(B" . "$B$[(B"))
    ("hu" nil ("$B%U(B" . "$B$U(B"))
    ("hya" nil ("$B%R%c(B" . "$B$R$c(B"))
    ("hye" nil ("$B%R%'(B" . "$B$R$'(B"))
    ("hyi" nil ("$B%R%#(B" . "$B$R$#(B"))
    ("hyo" nil ("$B%R%g(B" . "$B$R$g(B"))
    ("hyu" nil ("$B%R%e(B" . "$B$R$e(B"))
    ("i" nil ("$B%$(B" . "$B$$(B"))
    ("jj" "j" ("$B%C(B" . "$B$C(B"))
    ("ja" nil ("$B%8%c(B" . "$B$8$c(B"))
    ("je" nil ("$B%8%'(B" . "$B$8$'(B"))
    ("ji" nil ("$B%8(B" . "$B$8(B"))
    ("jo" nil ("$B%8%g(B" . "$B$8$g(B"))
    ("ju" nil ("$B%8%e(B" . "$B$8$e(B"))
    ("jya" nil ("$B%8%c(B" . "$B$8$c(B"))
    ("jye" nil ("$B%8%'(B" . "$B$8$'(B"))
    ("jyi" nil ("$B%8%#(B" . "$B$8$#(B"))
    ("jyo" nil ("$B%8%g(B" . "$B$8$g(B"))
    ("jyu" nil ("$B%8%e(B" . "$B$8$e(B"))
    ("kk" "k" ("$B%C(B" . "$B$C(B"))
    ("ka" nil ("$B%+(B" . "$B$+(B"))
    ("ke" nil ("$B%1(B" . "$B$1(B"))
    ("ki" nil ("$B%-(B" . "$B$-(B"))
    ("ko" nil ("$B%3(B" . "$B$3(B"))
    ("ku" nil ("$B%/(B" . "$B$/(B"))
    ("kya" nil ("$B%-%c(B" . "$B$-$c(B"))
    ("kye" nil ("$B%-%'(B" . "$B$-$'(B"))
    ("kyi" nil ("$B%-%#(B" . "$B$-$#(B"))
    ("kyo" nil ("$B%-%g(B" . "$B$-$g(B"))
    ("kyu" nil ("$B%-%e(B" . "$B$-$e(B"))
    ("ma" nil ("$B%^(B" . "$B$^(B"))
    ("me" nil ("$B%a(B" . "$B$a(B"))
    ("mi" nil ("$B%_(B" . "$B$_(B"))
    ("mo" nil ("$B%b(B" . "$B$b(B"))
    ("mu" nil ("$B%`(B" . "$B$`(B"))
    ("mya" nil ("$B%_%c(B" . "$B$_$c(B"))
    ("mye" nil ("$B%_%'(B" . "$B$_$'(B"))
    ("myi" nil ("$B%_%#(B" . "$B$_$#(B"))
    ("myo" nil ("$B%_%g(B" . "$B$_$g(B"))
    ("myu" nil ("$B%_%e(B" . "$B$_$e(B"))
    ("n" nil ("$B%s(B" . "$B$s(B"))
    ("n'" nil ("$B%s(B" . "$B$s(B"))
    ("na" nil ("$B%J(B" . "$B$J(B"))
    ("ne" nil ("$B%M(B" . "$B$M(B"))
    ("ni" nil ("$B%K(B" . "$B$K(B"))
    ("nn" nil ("$B%s(B" . "$B$s(B"))
    ("no" nil ("$B%N(B" . "$B$N(B"))
    ("nu" nil ("$B%L(B" . "$B$L(B"))
    ("nya" nil ("$B%K%c(B" . "$B$K$c(B"))
    ("nye" nil ("$B%K%'(B" . "$B$K$'(B"))
    ("nyi" nil ("$B%K%#(B" . "$B$K$#(B"))
    ("nyo" nil ("$B%K%g(B" . "$B$K$g(B"))
    ("nyu" nil ("$B%K%e(B" . "$B$K$e(B"))
    ("o" nil ("$B%*(B" . "$B$*(B"))
    ("pp" "p" ("$B%C(B" . "$B$C(B"))
    ("pa" nil ("$B%Q(B" . "$B$Q(B"))
    ("pe" nil ("$B%Z(B" . "$B$Z(B"))
    ("pi" nil ("$B%T(B" . "$B$T(B"))
    ("po" nil ("$B%](B" . "$B$](B"))
    ("pu" nil ("$B%W(B" . "$B$W(B"))
    ("pya" nil ("$B%T%c(B" . "$B$T$c(B"))
    ("pye" nil ("$B%T%'(B" . "$B$T$'(B"))
    ("pyi" nil ("$B%T%#(B" . "$B$T$#(B"))
    ("pyo" nil ("$B%T%g(B" . "$B$T$g(B"))
    ("pyu" nil ("$B%T%e(B" . "$B$T$e(B"))
    ("rr" "r" ("$B%C(B" . "$B$C(B"))
    ("ra" nil ("$B%i(B" . "$B$i(B"))
    ("re" nil ("$B%l(B" . "$B$l(B"))
    ("ri" nil ("$B%j(B" . "$B$j(B"))
    ("ro" nil ("$B%m(B" . "$B$m(B"))
    ("ru" nil ("$B%k(B" . "$B$k(B"))
    ("rya" nil ("$B%j%c(B" . "$B$j$c(B"))
    ("rye" nil ("$B%j%'(B" . "$B$j$'(B"))
    ("ryi" nil ("$B%j%#(B" . "$B$j$#(B"))
    ("ryo" nil ("$B%j%g(B" . "$B$j$g(B"))
    ("ryu" nil ("$B%j%e(B" . "$B$j$e(B"))
    ("ss" "s" ("$B%C(B" . "$B$C(B"))
    ("sa" nil ("$B%5(B" . "$B$5(B"))
    ("se" nil ("$B%;(B" . "$B$;(B"))
    ("sha" nil ("$B%7%c(B" . "$B$7$c(B"))
    ("she" nil ("$B%7%'(B" . "$B$7$'(B"))
    ("shi" nil ("$B%7(B" . "$B$7(B"))
    ("sho" nil ("$B%7%g(B" . "$B$7$g(B"))
    ("shu" nil ("$B%7%e(B" . "$B$7$e(B"))
    ("si" nil ("$B%7(B" . "$B$7(B"))
    ("so" nil ("$B%=(B" . "$B$=(B"))
    ("su" nil ("$B%9(B" . "$B$9(B"))
    ("sya" nil ("$B%7%c(B" . "$B$7$c(B"))
    ("sye" nil ("$B%7%'(B" . "$B$7$'(B"))
    ("syi" nil ("$B%7%#(B" . "$B$7$#(B"))
    ("syo" nil ("$B%7%g(B" . "$B$7$g(B"))
    ("syu" nil ("$B%7%e(B" . "$B$7$e(B"))
    ("tt" "t" ("$B%C(B" . "$B$C(B"))
    ("ta" nil ("$B%?(B" . "$B$?(B"))
    ("te" nil ("$B%F(B" . "$B$F(B"))
    ("tha" nil ("$B%F%!(B" . "$B$F$!(B"))
    ("the" nil ("$B%F%'(B" . "$B$F$'(B"))
    ("thi" nil ("$B%F%#(B" . "$B$F$#(B"))
    ("tho" nil ("$B%F%g(B" . "$B$F$g(B"))
    ("thu" nil ("$B%F%e(B" . "$B$F$e(B"))
    ("ti" nil ("$B%A(B" . "$B$A(B"))
    ("to" nil ("$B%H(B" . "$B$H(B"))
    ("tsu" nil ("$B%D(B" . "$B$D(B"))
    ("tu" nil ("$B%D(B" . "$B$D(B"))
    ("tya" nil ("$B%A%c(B" . "$B$A$c(B"))
    ("tye" nil ("$B%A%'(B" . "$B$A$'(B"))
    ("tyi" nil ("$B%A%#(B" . "$B$A$#(B"))
    ("tyo" nil ("$B%A%g(B" . "$B$A$g(B"))
    ("tyu" nil ("$B%A%e(B" . "$B$A$e(B"))
    ("u" nil ("$B%&(B" . "$B$&(B"))
    ("vv" "v" ("$B%C(B" . "$B$C(B"))
    ("va" nil ("$B%t%!(B" . "$B$&!+$!(B"))
    ("ve" nil ("$B%t%'(B" . "$B$&!+$'(B"))
    ("vi" nil ("$B%t%#(B" . "$B$&!+$#(B"))
    ("vo" nil ("$B%t%)(B" . "$B$&!+$)(B"))
    ("vu" nil ("$B%t(B" . "$B$&!+(B"))
    ("ww" "w" ("$B%C(B" . "$B$C(B"))
    ("wa" nil ("$B%o(B" . "$B$o(B"))
    ("we" nil ("$B%&%'(B" . "$B$&$'(B"))
    ("wi" nil ("$B%&%#(B" . "$B$&$#(B"))
    ("wo" nil ("$B%r(B" . "$B$r(B"))
    ("wu" nil ("$B%&(B" . "$B$&(B"))
    ("xx" "x" ("$B%C(B" . "$B$C(B"))
    ("xa" nil ("$B%!(B" . "$B$!(B"))
    ("xe" nil ("$B%'(B" . "$B$'(B"))
    ("xi" nil ("$B%#(B" . "$B$#(B"))
    ("xka" nil ("$B%u(B" . "$B$+(B"))
    ("xke" nil ("$B%v(B" . "$B$1(B"))
    ("xo" nil ("$B%)(B" . "$B$)(B"))
    ("xtsu" nil ("$B%C(B" . "$B$C(B"))
    ("xtu" nil ("$B%C(B" . "$B$C(B"))
    ("xu" nil ("$B%%(B" . "$B$%(B"))
    ("xwa" nil ("$B%n(B" . "$B$n(B"))
    ("xwe" nil ("$B%q(B" . "$B$q(B"))
    ("xwi" nil ("$B%p(B" . "$B$p(B"))
    ("xya" nil ("$B%c(B" . "$B$c(B"))
    ("xyo" nil ("$B%g(B" . "$B$g(B"))
    ("xyu" nil ("$B%e(B" . "$B$e(B"))
    ("yy" "y" ("$B%C(B" . "$B$C(B"))
    ("ya" nil ("$B%d(B" . "$B$d(B"))
    ("ye" nil ("$B%$%'(B" . "$B$$$'(B"))
    ("yo" nil ("$B%h(B" . "$B$h(B"))
    ("yu" nil ("$B%f(B" . "$B$f(B"))
    ("zz" "z" ("$B%C(B" . "$B$C(B"))
    ("z," nil "$B!E(B")
    ("z-" nil "$B!A(B")
    ("z." nil "$B!D(B")
    ("z/" nil "$B!&(B")
    ("z[" nil "$B!X(B")
    ("z]" nil "$B!Y(B")
    ("za" nil ("$B%6(B" . "$B$6(B"))
    ("ze" nil ("$B%<(B" . "$B$<(B"))
    ("zh" nil "$B"+(B")
    ("zi" nil ("$B%8(B" . "$B$8(B"))
    ("zj" nil "$B"-(B")
    ("zk" nil "$B",(B")
    ("zl" nil "$B"*(B")
    ("zo" nil ("$B%>(B" . "$B$>(B"))
    ("zu" nil ("$B%:(B" . "$B$:(B"))
    ("zya" nil ("$B%8%c(B" . "$B$8$c(B"))
    ("zye" nil ("$B%8%'(B" . "$B$8$'(B"))
    ("zyi" nil ("$B%8%#(B" . "$B$8$#(B"))
    ("zyo" nil ("$B%8%g(B" . "$B$8$g(B"))
    ("zyu" nil ("$B%8%e(B" . "$B$8$e(B"))
    ("." nil skk-current-kuten)
    ("," nil skk-current-touten)
    ("-" nil "$B!<(B")
    (":" nil "$B!'(B")
    (";" nil "$B!((B")
    ("?" nil "$B!)(B")
    ("[" nil "$B!V(B")
    ("]" nil "$B!W(B")
    ("l" nil skk-latin-mode)
    ("q" nil skk-toggle-kana)
    ("L" nil skk-jisx0208-latin-mode)
    ("Q" nil skk-set-henkan-point-subr)
    ("X" nil skk-purge-from-jisyo)
    ("/" nil skk-abbrev-mode)
    ("$" nil skk-display-code-for-char-at-point)
    ("@" nil skk-today)
    ("\\" nil skk-input-by-code-or-menu)
    )
  ;; $B%3%s%9%?%s%H$K$7$F$7$^$o$J$$$N$O!"%m!<%^;zF~NO$H$OA4$/JL$N@_Dj$r(B
  ;; $B$9$k?M$b$$$k$+$i$G$9!#(B
  "*$B%-!<F~NO$KBP$9$kJQ49J8;z$r8=$o$9%*!<%H%^%H%s>uBVA+0\5,B'!#(B
$B%j%9%H$N3FMWAG$O!"2<5-$N%j%9%H7A<0$rK~$?$7$F$$$J$1$l$P$J$i$J$$!#(B

\($B8=:_$N%-!<F~NO>uBV(B[@$B<!%-!<F~NO(B0][@$B<!%-!<F~NO(B1]...[@$B<!%-!<F~NO(Bn] $B:G=*$N%-!<F~NO>uBV(B $B=PNO(B\)

\($BC"$7!"(B\"@\" $B$OO"@\(B\) $B$r0UL#$9$k!#(B

$B=PNO$K;XDj$G$-$k$b$N$O!"J8;zNs!"J8;zNs$r(B car, cdr $B$K;}$D(B dot pair$B!"(B
$B4X?tL>%7%s%\%k$N$$$:$l$+!#(Bdot pair $B$O!"%+%J%b!<%I$N$H$-$O(B car $B$NJ8(B
$B;zNs!"$+$J%b!<%I$N$H$-$O(B cdr $B$NJ8;zNs$,A^F~$5$l$k!#J8;zNs$N$_;XDj$5$l(B
$B$F$$$k>l9g$O!"F~NO%b!<%I$K$+$+$o$i$:$=$NJ8;z$,A^F~$5$l$k!#(B
$BJ8;zNs$rA^F~$9$k4X?t$K$D$$$F$O!"(Binsert $B$rL@<(E*$K8F$VI,MW$O$J$/!"J8;z(B
$BNs$rJV$;$PNI$$!#J8;zNs$rA^F~$7$J$$4X?t$K$D$$$F$b;XDj$O2D!#(B

$B$3$NJQ?t$NDj5A$r%Y!<%9$K(B skk-rom-kana-rule-list $B$,DI2C$5$l!"(Bskk-mode
$B5/F0;~$K(B skk-rule-tree $B$H$$$&LZ$N7A$K%3%s%Q%$%k$5$l$k!#(B
2 $B$D$N%k!<%k%j%9%H$K=EJ#$9$k%-!<$N@_Dj$,$"$k>l9g$O!"(B
skk-rom-kana-rule-list $B$NDj5A$,M%@h$5$l$k!#(B" 
  :type '(repeat
	  (list string string
		(choice function string (cons string string)) ))
  :group 'skk )

(defcustom skk-rom-kana-rule-list
  '(
    ;; $B%f!<%6!<$N9%$_$G@_Dj$,J,$l$=$&$JMWAG$O!"(B
    ;; skk-rom-kana-base-rule-list $B$+$i$3$A$i$X0\$7$^$7$g$&(B...$B!#(B
    ("hh" "h" ("$B%C(B" . "$B$C(B"))
    ;; when you may want to insert $B!V$,$s$^!W(Bby "gamma"...
    ("mm" "m" ("$B%s(B" . "$B$s(B"))
    )
  "*$B%-!<F~NO$KBP$9$kJQ49J8;z$r8=$o$9%*!<%H%^%H%s>uBVA+0\5,B'$G!"%f!<%6!<$NDI2C$N@_Dj$r9T$J$&$b$N!#(B
$B%Y!<%9$H$J$k(B skk-rom-kana-base-rule-list $B$K$3$NJQ?t$NDj5A$,DI2C$5$l!"(B
skk-mode $B5/F0;~$K(B skk-rule-tree $B$H$$$&LZ$N7A$K%3%s%Q%$%k$5$l$k!#(B
2 $B$D$N%k!<%k%j%9%H$K=EJ#$9$k%-!<$N@_Dj$,$"$k>l9g$O!"$3$NJQ?t$NDj5A$,M%(B
$B@h$5$l$k!#(B

$B%j%9%H$N3FMWAG$O!"2<5-$N%j%9%H7A<0$rK~$?$7$F$$$J$1$l$P$J$i$J$$!#(B

\($B8=:_$N%-!<F~NO>uBV(B[@$B<!%-!<F~NO(B0][@$B<!%-!<F~NO(B1]...[@$B<!%-!<F~NO(Bn] $B:G=*$N%-!<F~NO>uBV(B $B=PNO(B\)

\($BC"$7!"(B\"@\" $B$OO"@\(B\) $B$r0UL#$9$k!#(B

$B=PNO$N<oN`$K$D$$$F$O!"(Bskk-rom-kana-base-rule-list $B$r;2>H$N$3$H!#(B
$B%f!<%6!<$,DI2C$7$?$$%k!<%k$r(B

    \(setq skk-rom-kana-rule-list
      '\(
        \(\"hh\" \"h\" \(\"$B%C(B\" . \"$B$C(B\"\)\)
        \(\"@\" nil \"$B!w(B\"\)
        ...
        \)

$B$N$h$&$K(B .emacs $B$d(B skk-init-file $B$KD>@\=q$/$N$,<j7Z!#(B

$B%G%#%U%)%k%H$G$O!"(B\(\"hh\" \"h\" \(\"$B%C(B\" . \"$B$C(B\"\)\) $B$H$$$&MWAG$,@_(B
$BDj$5$l$F$$$k$,!"(B\"ohhira\" -> \"$B$*$*$R$i(B\" $B$N$h$&$K(B \"hh\" $B$rB%2;=hM}(B
$B$7$?$/$J$1$l$P!"(Bskk-rom-kana-rule-list $B$+$i(B

    \(\"hh\" \"h\" \(\"$B%C(B\" . \"$B$C(B\"\)\) 

$B$H$$$&MWAG$r>C$9!#(B
$B$^$?!"(B`@' $B$G(B skk-today ($BEvF|$NF|IU$NF~NO(B) $B$r5/F0$9$kBe$j$K(B `$B!w(B' $B$rF~(B
$BNO$7$?$$>l9g$O!"(Bskk-rom-kana-rule-list $B$K(B

    \(\"@\" nil \"$B!w(B\"\)

$B$H$$$&MWAG$r2C$($k!#(Bskk-mode $B$N5/F08e(B skk-rom-kana-rule-list $B$NJQ99$r(B
$B9T$J$C$?>l9g!"$=$N@_Dj$rH?1G$5$;$k$K$O(B M-x skk-restart $B$r<B9T$9$kI,MW(B
$B$,$"$k!#(B" 
  :type '(repeat
	  (list string string
		(choice function string (cons string string)) ))
  :group 'skk )

(defcustom skk-kana-input-search-function
  (function
   (lambda ()
     (save-match-data
       (and (string-match "^h\\([bcdfghjklmnpqrstvwxz]\\)$" skk-prefix)
	    (member (char-to-string (preceding-char)) '("$B$*(B" "$B%*(B"))
	    (cons '("$B%*(B" . "$B$*(B") (match-string 1 skk-prefix)) ))))
  "*$B%k!<%k%j%9%H$NCf$K5-$;$J$$JQ49%k!<%k$r=hM}$9$k4X?t!#(B
skk-rom-kana-base-rule-list $B$H(B skk-rom-kana-rule-list $B$NMWAG$rA4$F8!:w(B
$B$7$?8e$K%3!<%k$5$l$k!#0z?t$O$J$$!#(B

\($B8=:_$NF~NO$KBP$9$k=PNO(B . \"$BB3$/(B unfixed prefix\"\)

$B$H$$$&%;%k$rJV$9!#=PNO$N<oN`$K$D$$$F$O!"(Bskk-rom-kana-base-rule-list $B$r(B
$B;2>H$N$3$H!#(B

$B%G%#%U%)%k%H$G$O!"(B\"$B$*(B\" $B$N8e$N(B \"h\" + $B;R2;$NF~NO$r(B \"$B$*$*(B\" + $BB3$/;R(B
$B2;=hM}MQ$N(B unfixed prefix $B$KJQ49$7$F$$$k!#(B" 
  :type 'function
  :group 'skk )

(defcustom skk-okuri-char-alist nil
  "*$B$"$kAw$j2>L>$rJL$NAw$j2>L>$KJQ49$9$k%k!<%k$r5-=R$9$k%(!<%j%9%H!#(B" 
  :type '(repeat (cons string string))
  :group 'skk )

(defcustom skk-downcase-alist nil
  "*$BJQ49%-!<(B ($BBgJ8;z%m!<%^;z(B) $B$N>.J8;z$X$NJQ495,B'$rI=$o$9%(!<%j%9%H!#(B
$BJQ49%-!<$NF~NO$r3+;O$9$k:]!"(BSKK $B$G$OBgJ8;z$GF~NO$r9T$J$&$N$G!"(B
skk-set-henkan-point $B$NCf$G$3$l$r>.J8;z$KJQ49$9$k:n6H$r9T$J$&!#$3$N%(!<(B
$B%j%9%H$KBgJ8;z(B -> $B>.J8;z$NJQ49%k!<%k$r=q$$$F$*$/$3$H$G!"%-!<F~NO$N%+%9(B
$B%?%^%$%:$r9T$J$&$3$H$,$G$-$k!#$3$N%(!<%j%9%H$,(B null $B$N>l9g$O!"C1$K(B
downcase $B$5$l$k!#(B" 
  :type '(repeat (cons character character))
  :group 'skk )

(defcustom skk-jisx0208-latin-vector
  [nil  nil  nil  nil  nil  nil  nil  nil
   nil  nil  nil  nil  nil  nil  nil  nil
   nil  nil  nil  nil  nil  nil  nil  nil
   nil  nil  nil  nil  nil  nil  nil  nil
   "$B!!(B"  "$B!*(B" "$B!I(B" "$B!t(B" "$B!p(B" "$B!s(B" "$B!u(B" "$B!G(B"
   "$B!J(B" "$B!K(B" "$B!v(B" "$B!\(B" "$B!$(B" "$B!](B" "$B!%(B" "$B!?(B"
   "$B#0(B" "$B#1(B" "$B#2(B" "$B#3(B" "$B#4(B" "$B#5(B" "$B#6(B" "$B#7(B"
   "$B#8(B" "$B#9(B" "$B!'(B" "$B!((B" "$B!c(B" "$B!a(B" "$B!d(B" "$B!)(B"
   "$B!w(B" "$B#A(B" "$B#B(B" "$B#C(B" "$B#D(B" "$B#E(B" "$B#F(B" "$B#G(B"
   "$B#H(B" "$B#I(B" "$B#J(B" "$B#K(B" "$B#L(B" "$B#M(B" "$B#N(B" "$B#O(B"
   "$B#P(B" "$B#Q(B" "$B#R(B" "$B#S(B" "$B#T(B" "$B#U(B" "$B#V(B" "$B#W(B"
   "$B#X(B" "$B#Y(B" "$B#Z(B" "$B!N(B" "$B!@(B" "$B!O(B" "$B!0(B" "$B!2(B"
   "$B!F(B" "$B#a(B" "$B#b(B" "$B#c(B" "$B#d(B" "$B#e(B" "$B#f(B" "$B#g(B"
   "$B#h(B" "$B#i(B" "$B#j(B" "$B#k(B" "$B#l(B" "$B#m(B" "$B#n(B" "$B#o(B"
   "$B#p(B" "$B#q(B" "$B#r(B" "$B#s(B" "$B#t(B" "$B#u(B" "$B#v(B" "$B#w(B"
   "$B#x(B" "$B#y(B" "$B#z(B" "$B!P(B" "$B!C(B" "$B!Q(B" "$B!A(B" nil]
  "*skk-jisx0208-latin-insert $B$G;2>H$5$l$kJ8;z%F!<%V%k!#(B
$B%-!<$KBP1~$9$k0LCV$KJ8;zNs$,$"$l$P!"A41Q%b!<%I$G3:Ev$N%-!<$r2!$9$3$H$G!"BP1~$9(B
$B$kJ8;z$,A^F~$5$l$k!#(B
$BNc$($P!"%9%Z!<%9%-!<$KBP1~$7$F!"H>3Q%9%Z!<%9$rA^F~$5$;$k$h$&$KJQ99$7$?$1$l$P!"(B
skk.el $B$N%m!<%I8e(B ($B$b$7$/$O(B skk-load-hook $B$rMxMQ$7$F(B)$B!"(B

     \(aset skk-jisx0208-latin-vector 32 \" \"\)

$B$H$9$k$+!"$b$7$/$O!"(Bskk-jisx0208-latin-vector $B$N(B 32 $BHVL\(B (0 $BHV$+$i?t$($F(B) $B$NCM$r(B \" \"
$B$H$9$k$h$&$J(B skk-jisx0208-latin-vector $B$rD>@\=q$-!"(Bsetq $B$GBeF~$9$k!#(B32 $B$O!"(B? ($BH>3Q%9(B
$B%Z!<%9$N(B char type) $B$rI>2A$7$?$H$-$NCM!#(B" 
  :type 'vector
  :group 'skk )

(defcustom skk-use-face (or window-system (skk-terminal-face-p))
  "*Non-nil $B$G$"$l$P!"(BEmacs $B$N(B face $B$N5!G=$r;HMQ$7$FJQ49I=<($r9T$J$&!#(B" 
  :type 'boolean
  :group 'skk )

(defcustom skk-henkan-face 'highlight
  "*$BJQ498uJd$N(B face $BB0@-!#(Bskk-use-face $B$,(B non-nil $B$N$H$-$N$_M-8z!#(B
Emacs $BI8=`%U%'%$%9$N(B default, modeline, region, secondary-selection,
highlight, underline, bold, italic, bold-italic $B$NB>!"?7$?$K(B face $B$r:n(B
$B$j;XDj$9$k$3$H$b2DG=!#(B
$B?7$?$J(B face $B$r:n$j;XDj$9$k$K$O(B skk-make-face $B$rMxMQ$7$F!"(B

      \(skk-make-face 'DimGray/PeachPuff1\)
      \(setq skk-henkan-face 'DimGray/PeachPuff1\)

$B$N$h$&$K$9$k$N$,<j7Z!#(Bforeground $B$H(B background $B$N?';XDj$@$1$G$J$$6E$C$?(B face
$B$r:n$k>l9g$O!"(Bskk-make-face $B$G$OBP1~$G$-$J$$$N$G!"(BEmacs $B$N(B hilit19.el $B$N(B
hilit-lookup-face-create $B$J$I$rMxMQ$9$k!#?'$rIU$1$k>l9g$NG[?'$O!"(Bcanna.el $B$N(B
canna:attribute-alist $B$,NI$$Nc$+$b$7$l$J$$!#(B" 
  :type 'face
  :group 'skk )

(defcustom skk-use-color-cursor (and window-system (fboundp 'x-display-color-p)
				     (x-display-color-p) )
  "*Non-nil $B$G$"$l$P!"(BSKK $B%b!<%I$NF~NO%b!<%I$K1~$8$F%+!<%=%k$K?'$rIU$1$k!#(B"
  :type 'boolean
  :group 'skk )

(defcustom skk-default-cursor-color
  (if (eq skk-emacs-type 'xemacs)
      (frame-property (selected-frame) 'cursor-color)
    (cdr (assq 'cursor-color (frame-parameters (selected-frame)))))
  "*SKK $B$N%*%U$r<($9%+!<%=%k?'!#(B
skk-use-color-cursor $B$,(B non-nil $B$N$H$-$K;HMQ$5$l$k!#(B" 
  :group 'skk )

(defcustom skk-hiragana-cursor-color (if (eq skk-background-mode 'light)
					 "coral4"
				       "pink" )
  "*$B$+$J%b!<%I$r<($9%+!<%=%k?'!#(B
skk-use-color-cursor $B$,(B non-nil $B$N$H$-$K;HMQ$5$l$k!#(B" 
  :type 'string
  :group 'skk )

(defcustom skk-katakana-cursor-color (if (eq skk-background-mode 'light)
					 "forestgreen"
				       "green" )
  "*$B%+%?%+%J%b!<%I$r<($9%+!<%=%k?'!#(B
skk-use-color-cursor $B$,(B non-nil $B$N$H$-$K;HMQ$5$l$k!#(B" 
  :type 'string
  :group 'skk )

(defcustom skk-jisx0208-latin-cursor-color "gold"
  "*$BA43Q1Q;z%b!<%I$r<($9%+!<%=%k?'!#(B
skk-use-color-cursor $B$,(B non-nil $B$N$H$-$K;HMQ$5$l$k!#(B" 
  :type 'string
  :group 'skk )

(defcustom skk-latin-cursor-color (if (eq skk-background-mode 'light)
				      "ivory4"
				    "gray" )
  "*$B%"%9%-!<%b!<%I$r<($9%+!<%=%k?'!#(B
skk-use-color-cursor $B$,(B non-nil $B$N$H$-$K;HMQ$5$l$k!#(B" 
  :type 'string
  :group 'skk )

(defcustom skk-abbrev-cursor-color "royalblue"
  "*abbrev $B%b!<%I$r<($9%+!<%=%k?'!#(B
skk-use-color-cursor $B$,(B non-nil $B$N$H$-$K;HMQ$5$l$k!#(B" 
  :type 'string
  :group 'skk )

(defcustom skk-report-set-cursor-error t
  "*Non-nil $B$G$"$l$P!"%+%i!<%^%C%W@Z$l$,5/$-$?>l9g!"%(%i!<%a%C%;!<%8$rI=<($9$k!#(B
nil $B$G$"$l$P!"I=<($7$J$$!#(B" 
  :type 'boolean
  :group 'skk )

(defcustom skk-use-cursor-change t
  "*Non-nil $B$G$"$l$P!"(BOvwrt $B%^%$%J!<%b!<%I;~$K%+!<%=%k$NI}$r=L$a$k!#(B" 
  :type 'boolean
  :group 'skk )

(defcustom skk-auto-insert-paren nil
  "*Non-nil $B$G$"$l$P!"(B2 $B$D$NJ8;zNs$r$^$H$a$FA^F~$7!"$=$NJ8;zNs$N4V$K%+!<%=%k$r0\F0$9$k!#(B
$BNc$($P!"(B\"$B!V(B\" $B$rF~NO$7$?$H$-$K(B \"$B!W(B\" $B$r<+F0E*$KA^F~$7!"N>$+$.$+$C$3$N4V$K(B
$B%+!<%=%k$r0\F0$9$k!#(B
$BA^F~$9$kJ8;zNs$O!"(Bskk-auto-paren-string-alist $B$G;XDj$9$k!#(B" 
  :type 'boolean
  :group 'skk )

(defcustom skk-auto-paren-string-alist
  '(("$B!V(B" . "$B!W(B") ("$B!X(B" . "$B!Y(B") ("(" . ")") ("$B!J(B" . "$B!K(B")
    ("{" . "}")("$B!P(B" . "$B!Q(B") ("$B!R(B" . "$B!S(B") ("$B!T(B" . "$B!U(B")
    ("[" . "]") ("$B!N(B" . "$B!O(B") ("$B!L(B" . "$B!M(B") ("$B!Z(B" . "$B![(B")
    ("\"" . "\"")("$B!H(B" . "$B!I(B") ("`" . "'")
    ;;("<" . ">") ;; skk-special-midashi-char-list $B$NCf$K$"$kJ8;z!#(B
    )
  "*$B<+F0E*$KBP$K$J$kJ8;zNs$rF~NO$9$k$?$a$NO"A[%j%9%H!#(B
 skk-auto-insert-paren $B$,(B non-nil $B$N>l9g!"(Bcar $B$NJ8;zNs$,A^F~$5$l$?$H$-(B
$B$K(B cdr $B$NJ8;zNs$r<+F0E*$KA^F~$5$l!"%+!<%=%k$O$=$N(B 2 $B$D$NJ8;zNs$N4V$K0\(B
$BF0$9$k!#(B
skk-special-midashi-char-list $B$NMWAG$K$J$C$F$$$kJ8;z$O!"(B
skk-auto-paren-string-alist $B$K4^$a$F$b:o=|$5$l$k!#(B " 
  :type '(repeat (cons string string))
  :group 'skk ) 

(defcustom skk-japanese-message-and-error nil
  "*Non-nil $B$G$"$l$P!"(BSKK $B$N%a%C%;!<%8$H%(%i!<$rF|K\8l$GI=<($9$k!#(B
nil $B$G$"$l$P!"1Q8l$GI=<($9$k!#(B" 
  :type 'boolean
  :group 'skk )

(defcustom skk-set-henkan-point-key
  '(?A ?B ?C ?D ?E ?F ?G ?H ?I ?J ?K ?M ?N ?O ?P ?R ?S ?T ?U ?V ?W ?Y ?Z)
  "*$BJQ49$N3+;OCOE@$r7h$a$k%-!<$N%j%9%H!#(B"
  :type '(repeat character)
  :group 'skk )

(defcustom skk-jisyo-save-count 50
  "*$B?tCM$G$"$l$P!"$=$N2s?t<-=q$,99?7$5$l$?$H$-$K<-=q$r<+F0E*$K%;!<%V$9$k!#(B
nil $B$G$"$l$P!"<-=q$N%*!<%H%;!<%V$r9T$J$o$J$$!#(B" 
  :type '(choice integer (const nil))
  :group 'skk )

(defcustom skk-byte-compile-init-file nil
  "*Non-nil $B$G$"$l$P!"(Bskk-mode $B5/F0;~$K(B skk-init-file $B$r%P%$%H%3%s%Q%$%k$9$k!#(B
$B@53N$K8@$&$H!"(B

  (1)skk-init-file $B$r%P%$%H%3%s%Q%$%k$7$?%U%!%$%k$,$J$$$+!"(B
  (2)skk-init-file $B$H$=$N%P%$%H%3%s%Q%$%k:Q%U%!%$%k$rHf3S$7$F!"A0<T$NJ}$,?7$7(B
     $B$$$H$-(B

$B$K(B skk-init-file $B$r%P%$%H%3%s%Q%$%k$9$k!#(B
nil $B$G$"$l$P!"(Bskk-init-file $B$H$=$N%P%$%H%3%s%Q%$%k:Q$_%U%!%$%k$rHf3S$7$F(B 
skk-init-file $B$NJ}$,?7$7$$$H$-$O!"$=$N%P%$%H%3%s%Q%$%k:Q%U%!%$%k$r>C$9!#(B" 
  :type 'boolean
  :group 'skk )

(defcustom skk-count-private-jisyo-candidates-exactly nil
  "*Non-nil $B$G$"$l$P!"(BEmacs $B$r=*N;$9$k$H$-$K@53N$K8D?M<-=q$N8uJd?t$r?t$($k!#(B
nil $B$G$"$l$P!"(B1 $B9T$KJ#?t$N8uJd$,$"$C$F$b(B 1 $B8uJd$H$7$F?t$($k!#(B
$B7W;;7k2L$O!"(Bskk-record-file $B$KJ]B8$5$l$k!#(B" 
  :type 'boolean
  :group 'skk )

(defcustom skk-compare-jisyo-size-when-saving t
  "*Non-nil $B$G$"$l$P!"(Bskk-jisyo $B$N%;!<%V;~$K%U%!%$%k%5%$%:$N%A%'%C%/$r9T$J$&!#(B
$BA02s%;!<%V$7$?(B skk-jisyo $B$H:#2s%;!<%V$7$h$&$H$9$k<-=q$H$N%5%$%:Hf3S$r9T$J$$!"(B
$B8e<T$NJ}$,Bg$-$$$H$-$K%f!<%6!<$K%;!<%V$rB3$1$k$+$I$&$+$N3NG'$r5a$a$k!#(B" 
  :type 'boolean
  :group 'skk )

(defcustom skk-auto-start-henkan t
  "*$BC18l$dJ8@a$N6h@Z$j$r<($9J8;z$NBG80$K$h$j<+F0E*$KJQ49$r3+;O$9$k!#(B
skk-auto-start-henkan-keyword-list $B$K$h$jC18l$dJ8@a$N6h@Z$j$r<($9J8;z$r;XDj$9$k!#(B" 
  :type 'boolean
  :group 'skk )

(defcustom skk-auto-start-henkan-keyword-list
  '("$B$r(B" "$B!"(B" "$B!#(B" "$B!%(B" "$B!$(B" "$B!)(B" "$B!W(B" "$B!*(B" "$B!((B" "$B!'(B" ")" ";" ":"
    "$B!K(B" "$B!I(B" "$B![(B" "$B!Y(B" "$B!U(B" "$B!S(B" "$B!Q(B" "$B!O(B" "$B!M(B" "}" "]" "?" "."
    "," "!" )
  ;; $B$"$^$j%-!<%o!<%I$,B?$/$J$k$H!"DL>o$NJQ49$r:$Fq$K$9$k!)(B
  "*$B<+F0JQ49$r3+;O$9$k%-!<%o!<%I!#(B
skk-auto-start-henkan $B$,(B non-nil $B$N$H$-!"$3$N%j%9%H$NMWAG$NJ8;z$rA^F~(B
$B$9$k$H!"(BSPC $B$r2!$9$3$H$J$/<+F0E*$KJQ49$r3+;O$9$k!#(B" 
  :type '(repeat string)
  :group 'skk )

(defcustom skk-search-excluding-word-pattern-function nil
  "*$B8D?M<-=q$K<h$j9~$^$J$$J8;zNs$N%Q%?!<%s$r8!:w$9$k4X?t$r;XDj$9$k!#(B
$B3NDj$7$?J8;zNs$r0z?t$KEO$7$F(B funcall $B$5$l$k!#(B

SKK $B$G$OJQ49!"3NDj$r9T$J$C$?J8;zNs$OA4$F8D?M<-=q$K<h$j9~$^$l$k$,!"$3$N(B
$BJQ?t$G;XDj$5$l$?4X?t$,(B non-nil $B$rJV$9$H$=$NJ8;zNs$O8D?M<-=q$K<h$j9~$^(B
$B$l$J$$!#(B

$BNc$($P!"$3$NJQ?t$K2<5-$N$h$&$J;XDj$9$k$H!"JQ49$K$h$j(B (SKK abbrev mode
$B$G$NJQ49$r=|$/(B) $B%+%?%+%J$N$_$+$i$J$kJ8;zNs$rF@$F3NDj$7$F$b!"$=$l$r8D?M(B
$B<-=q$K<h$j9~$^$J$$!#(B

  \(setq skk-search-excluding-word-pattern-function
        \(function
         \(lambda \(kakutei-word\)
         ;; $B$3$N4X?t$,(B t $B$rJV$7$?$H$-$O!"$=$NJ8;zNs$O8D?M<-=q$K<h$j9~$^$l$J$$!#(B
           \(save-match-data
             \(and
            ;; $BAw$j$J$7JQ49$G!"(B
              \(not skk-okuri-char\)
            ;; $B3NDj8l$,%+%?%+%J$N$_$+$i9=@.$5$l$F$$$F!"(B
              \(string-match \"^[$B!<%!(B-$B%s(B]+$\" kakutei-word\)
            ;; SKK abbrev mode $B0J30$G$NJQ49$+!"(B
              \(or \(not skk-abbrev-mode\)
                ;; $B8+=P$78l$,%+%?%+%J!"$R$i$,$J0J30$N$H$-!#(B
                ;; \($B8e$G"&%^!<%/$rIU$1$?$H$-$O!"8+=P$78l$,1QJ8;z$G$b!"(B
                ;; skk-abbrev-mode$B$,(B t $B$K$J$C$F$$$J$$(B\)$B!#(B
                  \(not \(string-match \"^[^$B!<%!(B-$B%s$!(B-$B$s(B]+$\" skk-henkan-key\)\) \)\)\)\)\)\)

$B%+%?%+%J$rJQ49$K$h$j5a$a$?$$$,!"8D?M<-=q$K$O%+%?%+%J$N$_$N8uJd$r<h$j9~$_$?(B
$B$/$J$$!"$J$I!"8D?M<-=q$,I,MW0J>e$KKD$l$k$N$rM^$($kL\E*$K;HMQ$G$-$k!#(B

$B$J$*!"8D?M<-=q$K<h$j9~$^$J$$8+=P$78l$K$D$$$F$OJd40$,8z$+$J$$$N$G!"Cm0U$9$k$3$H!#(B"
  :type 'function
  :group 'skk )

(defcustom skk-update-jisyo-function 'skk-update-jisyo-original
  "*skk-update-jisyo $B$G;HMQ$9$k4X?t!#(B" 
  :type 'function
  :group 'skk )

(defcustom skk-save-jisyo-function 'skk-save-jisyo-original
  "*skk-save-jisyo $B$G;HMQ$9$k4X?t!#(B" 
  :type 'function
  :group 'skk )

(defcustom skk-count-jisyo-candidates-function
  'skk-count-jisyo-candidates-original
  "*skk-count-jisyo-candidates $B$G;HMQ$9$k4X?t!#(B" 
  :type 'function
  :group 'skk )

(defcustom skk-public-jisyo-to-be-searched-function
  'skk-public-jisyo-to-be-searched-original
  "*skk-public-jisyo-has-entry-p $B$G;HMQ$9$k4X?t!#(B" 
  :type 'function
  :group 'skk )

(defcustom skk-use-look nil
  "*Non-nil $B$G$"$l$P!"(BUNIX look $B%3%^%s%I$rMxMQ$7$?Jd40!&JQ49$r9T$J$&!#(B
SKK abbrev $B%b!<%I$GJd40$r9T$J$&$H!"8D?M<-=q$r8!:w$7?T$7$?8e$G!"(BUNIX look $B%3%^%s(B
$B%I$K$h$k1QC18lJd40$r9T$J$&!#Nc$($P!"(B 

  $B"&(Bconfe \(TAB\)
  ---> $B"&(Bconference

SKK abbrev $B%b!<%I$G!"!V1QJ8;z(B + $B%"%9%?%j%9%/!W$K$FJQ49$r9T$J$&$H!"(Blook $B%3%^%s%I(B
$B$K$h$k$"$$$^$$8!:w$r9T$J$&$3$H$,$G$-$k!#Nc$($P!"(B

 $B"&(Bconfere* \(SPC\)
  ---> $B"'(Bconference

$B$3$N>uBV$G3NDj$9$k$H!"(B`confere*' $B$r8+=P$78l!"(B`conference' $B$r8uJd$H$9$k%(%s%H%j(B
$B$,8D?M<-=q$KDI2C$5$l$k!#(B`skk-search-excluding-word-pattern-function' $B$K$h(B
$B$j!"3NDj$7$F$b$3$N$h$&$J%(%s%H%j$rDI2C$7$J$$$h$&$K@_Dj$9$k$3$H$,$G$-$k!#(B" 
  :type 'boolean
  :group 'skk )

(defcustom skk-kana-rom-vector
  ["x" "a" "x" "i" "x" "u" "x" "e" "x" "o" "k" "g" "k" "g" "k" "g"
   "k" "g" "k" "g" "s" "z" "s" "j" "s" "z" "s" "z" "s" "z" "t" "d"
   "t" "d" "x" "t" "d" "t" "d" "t" "d" "n" "n" "n" "n" "n" "h" "b"
   "p" "h" "b" "p" "h" "b" "p" "h" "b" "p" "h" "b" "p" "m" "m" "m"
   "m" "m" "x" "y" "x" "y" "x" "y" "r" "r" "r" "r" "r" "x" "w" "x"
   "x" "w" "n"]
  "*skk-remove-common $B$G;HMQ$9$k$+$JJ8;z$+$i%m!<%^;z$X$NJQ49%k!<%k!#(B
$B2<5-$N3:Ev$9$k$+$JJ8;z$r$=$NJ8;z$N%m!<%^;z%W%l%U%#%C%/%9$G8=$o$7$?$b$N!#(B
    $B$!(B  $B$"(B  $B$#(B  $B$$(B  $B$%(B  $B$&(B  $B$'(B  $B$((B  $B$)(B  $B$*(B  $B$+(B  $B$,(B  $B$-(B  $B$.(B  $B$/(B  $B$0(B
    $B$1(B  $B$2(B  $B$3(B  $B$4(B  $B$5(B  $B$6(B  $B$7(B  $B$8(B  $B$9(B  $B$:(B  $B$;(B  $B$<(B  $B$=(B  $B$>(B  $B$?(B  $B$@(B
    $B$A(B  $B$B(B  $B$C(B  $B$D(B  $B$E(B  $B$F(B  $B$G(B  $B$H(B  $B$I(B  $B$J(B  $B$K(B  $B$L(B  $B$M(B  $B$N(B  $B$O(B  $B$P(B
    $B$Q(B  $B$R(B  $B$S(B  $B$T(B  $B$U(B  $B$V(B  $B$W(B  $B$X(B  $B$Y(B  $B$Z(B  $B$[(B  $B$\(B  $B$](B  $B$^(B  $B$_(B  $B$`(B
    $B$a(B  $B$b(B  $B$c(B  $B$d(B  $B$e(B  $B$f(B  $B$g(B  $B$h(B  $B$i(B  $B$j(B  $B$k(B  $B$l(B  $B$m(B  $B$n(B  $B$o(B  $B$p(B
    $B$q(B  $B$r(B  $B$s(B
$B$=$l$>$l$N$+$JJ8;z$,Aw$j2>L>$G$"$k>l9g$K$I$N%m!<%^;z%W%l%U%#%C%/%9$rBP1~$5$;$k(B
$B$N$+$r;XDj$9$k$3$H$,$G$-$k!#!V$8!W!"!V$A!W!"!V$U!W$NJ8;z$K$D$$$F!"BP1~$9$k%m!<(B
$B%^;z%W%l%U%#%C%/%9$r(B \"z\", \"c\",\"f\" $B$KJQ99$r4uK>$9$k>l9g$b$"$k$G$"$m$&!#(B
skk-auto-okuri-process $B$NCM$,(B non-nil $B$N$H$-!"$"$k$$$O%5JQJd=uJQ49$,9T$J$o$l$k(B
$B$H$-;2>H$5$l$k!#(B" 
  :type 'vector
  :group 'skk )

(defcustom skk-henkan-overlay-priority 600
  "*$BJQ49$7$?8uJd$K=E$M$k(B overlay $B$N(B priority$B!#(B
$BNc$($P!"(BViper $B$G(B R $B%3%^%s%I$K$h$j(B replace $B$r9T$J$&$H$-$K!"(B
viper-replace-overlay $B$H$$$&(B priority 400 $B$N(B overlay $B$r=E$M$i$l$k$,!"(B
skk-henkan-overlay-priority $B$N%G%#%U%)%k%HCM$O$3$N(B overlay $B$h$j(B
priority $B$,9b$$$N$G!"M%@h$7$FI=<($5$l$k!#(B" 
  :type 'integer
  :group 'skk )

(defcustom skk-kuten-touten-alist '((jp . ("$B!#(B" . "$B!"(B" )) (en . ("$B!%(B" . "$B!$(B")))
  "*$B6gE@$HFIE@$N%(!<%j%9%H!#(B
$B3FMWAG$N7A<0$O!"(B

   \($B%7%s%\%k(B . \($B6gE@$rI=$o$9J8;zNs(B . $BFIE@$rI=$o$9J8;zNs(B\)\)

$B$H$$$&(B cons cell$B!#%7%s%\%k$NItJ,$O!"(B`jp' $B$b$7$/$O(B `en' $B$H$7!"(B
skk-toggle-kutouten $B$O$3$l$r%H%0%k$G@Z$j49$($k!#(B
$B%G%#%U%)%k%H$N6gFIE@$N%?%$%W$O!"(B`skk-kutouten-type' $B$G;XDj$9$k!#(B" 
  :type '(repeat (cons (choice (const jp) (const en))
		       (cons string string)  ))
  :group 'skk )

(skk-deflocalvar skk-kutouten-type 'jp
  "*$B%G%#%U%)%k%H$N6gFIE@$N%?%$%W!#(B`jp' $B$b$7$/$O(B `en' $B$H$$$&%7%s%\%k!#(B" )

(defcustom skk-read-from-minibuffer-function nil
  "*$BC18lEPO?%b!<%I$G(B read-from-minibuffer $B$N(B INITIAL-CONTENTS $B$rDs6!$9$k(B funcition$B!#(B
$B$3$N(B function $B$OJ8;zNs$rJV$5$J$1$l$P$J$i$J$$!#(B
$BNc$($P!"(Bskk-henkan-key $B$r$=$N$^$^(B initial-contents $B$H$7$FMxMQ$7$?$$$H$-(B
$B$O!"(B
  \(setq skk-read-from-minibuffer-function 
        \(function \(lambda \(\) skk-henkan-key\)\) \)
$B$H;XDj$9$k!#(B"
  :type 'function
  :group 'skk )

(defvar skk-latin-mode-map nil "*ASCII $B%b!<%I$N%-!<%^%C%W!#(B")
(or skk-latin-mode-map 
    (let ((map (make-sparse-keymap)))
      ;; .skk $B$G(B skk-kakutei-key $B$NJQ99$,2DG=$K$J$k$h$&$K!#(B
      ;;(define-key map skk-kakutei-key 'skk-kakutei)
      (skk-define-menu-bar-map map)
      (setq skk-latin-mode-map map) ))

(defvar skk-j-mode-map nil "*$B$+$J%b!<%I$N%-!<%^%C%W!#(B")
(or skk-j-mode-map
    (let ((map (make-sparse-keymap)))
      (substitute-key-definition 'self-insert-command 'skk-insert map
				 global-map)
      ;; for Mule-2.x
      (substitute-key-definition 'egg-self-insert-command 'skk-insert map
				 global-map)
      (substitute-key-definition 'canna-self-insert-command 'skk-insert map
				 global-map)
      (substitute-key-definition 'can-n-egg-self-insert-command 'skk-insert map
				 global-map)
      ;; .skk $B$G(B skk-kakutei-key $B$NJQ99$,2DG=$K$J$k$h$&$K!#(B
      ;;(define-key map skk-kakutei-key 'skk-kakutei)
      (skk-define-menu-bar-map map)
      (setq skk-j-mode-map map) ))

(defvar skk-jisx0208-latin-mode-map nil "*$BA43Q%b!<%I$N%-!<%^%C%W!#(B")
(or skk-jisx0208-latin-mode-map 
    (let ((map (make-sparse-keymap))
	  (i 0) )
      (while (< i 128)
	(and (aref skk-jisx0208-latin-vector i)
	     (define-key map (char-to-string i) 'skk-jisx0208-latin-insert) )
	(setq i (1+ i)) )
      (define-key map "\C-q" 'skk-latin-henkan)
      (skk-define-menu-bar-map map)
      (setq skk-jisx0208-latin-mode-map map) ))

(defvar skk-abbrev-mode-map nil "*SKK abbrev $B%b!<%I$N%-!<%^%C%W!#(B")
(or skk-abbrev-mode-map 
    (let ((map (make-sparse-keymap)))
      (define-key map "," 'skk-abbrev-comma)
      (define-key map "." 'skk-abbrev-period)
      (define-key map "\C-q" 'skk-jisx0208-latin-henkan)
      ;; .skk $B$G(B skk-kakutei-key $B$NJQ99$,2DG=$K$J$k$h$&$K!#(B
      ;;(define-key map skk-kakutei-key 'skk-kakutei)
      (skk-define-menu-bar-map map)
      (setq skk-abbrev-mode-map map) ))

;;; -- internal constants and variables
;; ---- global ones.
;;(defvar skk-henkan-face 'skk-henkan-face)
(defconst skk-month-alist
  '(("Jan" . "1") ("Feb" . "2") ("Mar" . "3") ("Apr" . "4") ("May" . "5")
    ("Jun" . "6") ("Jul" . "7") ("Aug" . "8") ("Sep" . "9") ("Oct" . "10")
    ("Nov" . "11") ("Dec" . "12") )
  "$B1Q8l$N7nL>$H;;MQ?t;z$NO"A[%j%9%H!#(B

$B;;MQ?t;z$+$i1Q8l$N7nL>$N$_$r=PNO$9$k$N$G$"$l$P!"%Y%/%?!<$r;H$C$?J}$,9bB.$@$,!"(B
$B1Q8l$N7nL>$+$i;;MQ?t;z$r=PNO$9$k$N$G$"$l$PO"A[%j%9%H$G$J$1$l$PL5M}$J$N$G!"B?(B
$BL\E*$K;HMQ$G$-$k$h$&O"A[%j%9%H$N7ABV$r<h$k!#(B"
  ;;  "Alist of English month abbreviations and numerical values.
  ;;
  ;;Although it is faster to use a vector if we only want to output
  ;;month abbreviations given the ordinal, without the alist it's
  ;;unreasonable [sic] to output the ordinal given the abbreviation,
  ;;so for multi-purpose utility we use the alist form."
  )

(defconst skk-coding-system-alist
  (if (memq skk-emacs-type '(xemacs mule4 mule3))
      '(("euc" . euc-japan)
        ("ujis" . euc-japan)
        ("sjis". sjis)
        ("jis" . junet) )
    '(("euc" . *euc-japan*)
      ("ujis" . *euc-japan*)
      ("sjis". *sjis*)
      ("jis" . *junet*) ))
  "coding-system $B$NJ8;zNsI=8=$H!"%7%s%\%kI=8=$NO"A[%j%9%H!#(B" )

(defconst skk-default-jisx0208-latin-vector
  ;; note that skk-jisx0208-latin-vector is a user variable.
  ;; skk.el $B%m!<%IA0$K(B .emacs $B$J$I$G!"(Bskk-jisx0208-latin-vector $B$NJL$NCM$r%f!<(B
  ;; $B%6!<$,D>@\=q$$$?$j!"(Bskk.el $B%m!<%I8e$K$3$NCM$r(B aset $B$GD>@\$$$8$C$?$j$7$J(B
  ;; $B$1$l$P(B default-value $B$G(B skk-jisx0208-latin-vector $B$K%"%/%;%9$9$k$3$H$G(B
  ;; skk-default-jisx0208-latin-vector $B$NCM$rJ];}$9$k$3$H$b$G$-$h$&$,!"$=$l$O(B
  ;; $BK>$a$J$$(B...$B!#(B
  [nil  nil  nil  nil  nil  nil  nil  nil
   nil  nil  nil  nil  nil  nil  nil  nil
   nil  nil  nil  nil  nil  nil  nil  nil
   nil  nil  nil  nil  nil  nil  nil  nil
   "$B!!(B"  "$B!*(B" "$B!I(B" "$B!t(B" "$B!p(B" "$B!s(B" "$B!u(B" "$B!G(B"
   "$B!J(B" "$B!K(B" "$B!v(B" "$B!\(B" "$B!$(B" "$B!](B" "$B!%(B" "$B!?(B"
   "$B#0(B" "$B#1(B" "$B#2(B" "$B#3(B" "$B#4(B" "$B#5(B" "$B#6(B" "$B#7(B"
   "$B#8(B" "$B#9(B" "$B!'(B" "$B!((B" "$B!c(B" "$B!a(B" "$B!d(B" "$B!)(B"
   "$B!w(B" "$B#A(B" "$B#B(B" "$B#C(B" "$B#D(B" "$B#E(B" "$B#F(B" "$B#G(B"
   "$B#H(B" "$B#I(B" "$B#J(B" "$B#K(B" "$B#L(B" "$B#M(B" "$B#N(B" "$B#O(B"
   "$B#P(B" "$B#Q(B" "$B#R(B" "$B#S(B" "$B#T(B" "$B#U(B" "$B#V(B" "$B#W(B"
   "$B#X(B" "$B#Y(B" "$B#Z(B" "$B!N(B" "$B!@(B" "$B!O(B" "$B!0(B" "$B!2(B"
   "$B!F(B" "$B#a(B" "$B#b(B" "$B#c(B" "$B#d(B" "$B#e(B" "$B#f(B" "$B#g(B"
   "$B#h(B" "$B#i(B" "$B#j(B" "$B#k(B" "$B#l(B" "$B#m(B" "$B#n(B" "$B#o(B"
   "$B#p(B" "$B#q(B" "$B#r(B" "$B#s(B" "$B#t(B" "$B#u(B" "$B#v(B" "$B#w(B"
   "$B#x(B" "$B#y(B" "$B#z(B" "$B!P(B" "$B!C(B" "$B!Q(B" "$B!A(B" nil]
  "skk-jisx0208-latin-region $B$G;2>H$9$kJ8;z%F!<%V%k!#(B
\"ascii\" -> \"$B#a#s#c#i#i(B\" $B$N$h$&$JA43QJ8;z$X$NJQ49$r9T$&:]$KMxMQ$9$k!#(B" )

(defconst skk-kanji-len (length "$B$"(B")
  "$B4A;z0lJ8;z$ND9$5!#(BMule[1-3] $B$G$O(B 3 $B$K$J$k!#(BMule4, XEmacs $B$G$O(B 1$B!#(B" )

(defconst skk-hankaku-alist
  (if (eq skk-emacs-type 'mule2)
      '((161 . 32)	; ?\ 
	(170 . 33)	;?\!
	(201 . 34)	;?\"
	(244 . 35)	;?\#
	(240 . 36)	;?\$
	(243 . 37)	;?\%
	(245 . 38)	;?\&
	(199 . 39)	;?\'
	(202 . 40)	;?\(
	(203 . 41)	;?\)
	(246 . 42)	;?\*
	(220 . 43)	;?\+
	(164 . 44)	;?\,
	(221 . 45)	;?\-
	(165 . 46)	;?\.
	(191 . 47)	;?\/
	(167 . 58)	;?\:
	(168 . 59)	;?\;
	(227 . 60)	;?\<
	(225 . 61)	;?\=
	(228 . 62)	;?\>
	(169 . 63)	;?\?
	(247 . 64)	;?\@
	(206 . 91)	;?\[
	(239 . 92)	;?\\
	(207 . 93)	;?\]
	(176 . 94)	;?^ 
	(178 . 95)	;?\_
	(208 . 123)	;?\{
	(195 . 124)	;?\|
	(209 . 125)	;?\}
	(177 . 126)	;?\~
	(198 . 96) ))	;?` 
  "$BJ8;z%3!<%I$N(B 2 $BHVL\$N%P%$%H$H$=$NJ8;z$KBP1~$9$k(B ascii $BJ8;z(B \(char\) $B$H$NO"A[%j%9%H!#(B
Mule l $B$b$7$/$O(B  Mule 2 $B$r;HMQ$9$k>l9g$K(B skk-latin-region $B$G;2>H$9$k!#(B
Mule-2.3 $BE:IU$N(B egg.el $B$h$j%3%T!<$7$?!#(B" )

(defconst skk-kana-cleanup-command-list
  '(skk-delete-backward-char skk-insert skk-previous-candidate) )

(defvar skk-rule-tree nil
  "$B%m!<%^;z(B -> $B$+$JJQ49$N>uBVA+0\5,B'$rI=$9%D%j!<$N=i4|>uBV!#(B
skk-mode $B$N5/F0;~$KKh2s(B skk-rom-kana-base-rule-list $B$H(B
skk-rom-kana-rule-list $B$+$iLZ$N7A$K%3%s%Q%$%k$5$l$k!#(B" )

(defvar skk-insert-new-word-function nil
  "$B8uJd$rA^F~$7$?$H$-$K(B funcall $B$5$l$k4X?t$rJ]B8$9$kJQ?t!#(B" )

(skk-deflocalvar skk-input-mode-string skk-hiragana-mode-string
  "SKK $B$NF~NO%b!<%I$r<($9J8;zNs!#(Bskk-mode $B5/F0;~$O!"(Bskk-hiragana-mode-string$B!#(B" )

(defvar skk-isearch-message nil
  "skk-isearch $B4X?t$r%3!<%k$9$k$?$a$N%U%i%0!#(B
Non-nil $B$G$"$l$P!"(Bskk-isearch-message $B4X?t$r%3!<%k$9$k!#(B" )

(defvar skk-mode-invoked nil
  "Non-nil $B$G$"$l$P!"(BEmacs $B$r5/F08e4{$K(B skk-mode $B$r5/F0$7$?$3$H$r<($9!#(B" )

(defvar skk-kakutei-count 0
  "$BJQ498uJd$r3NDj$7$?%+%&%s%H$rJ];}$9$kJQ?t!#(B
skk-record-file $B$N(B \"$B3NDj(B:\" $B9`L\$N%+%&%s%?!<!#(B" )

(defvar skk-touroku-count 0
  "$B<-=qEPO?$7$?%+%&%s%H$rJ];}$9$kJQ?t!#(B
skk-record-file $B$N(B \"$BEPO?(B:\" $B9`L\$N%+%&%s%?!<!#(B" )

(defvar skk-update-jisyo-count 0
  "$B<-=q$r99?7$7$?2s?t!#(B
$B$3$N%+%&%s%?!<$N?t;z$,(B skk-jisyo-save-count $B0J>e$H$J$C$?$H$-$K%f!<%6!<<-=q$N%*!<(B
$B%H%;!<%V$,9T$J$o$l$k!#(B
$B<-=q$N%;!<%V$,9T$J$o$l$k$H%$%K%7%c%i%$%:$5$l$k!#(B" )

(defvar skk-minibuffer-origin-mode nil
  "$BF~NO%b!<%I$rI=$o$9%7%s%\%k!#(B
$BM-8z$JCM$O!"(B`hiragana', `katakana', `abbrev', `latin', `jisx0208-latin' $B$b$7$/$O(B
nil $B$N$$$:$l$+!#(B" )

;; ---- buffer local variables
;; <$B%U%i%0N`(B>
;;(skk-deflocalvar skk-current-henkan-data
;;  '(
;;    ;; global variables
;;    ;; $B%P%C%U%!%m!<%+%kJQ?t$N%G%#%U%)%k%HCM$r@_Dj$9$k$H!"$3$l$rD>@\=q49$($7$?$H(B
;;    ;; $B$-$KB>$N%P%C%U%!$+$i8+$($kCM$bJQ$o$C$F$7$^$&!#(Bglobal $B$J%U%i%0$O$3$l$rMx(B
;;    ;; $BMQ$7$F%G%#%U%)%k%HCMM?$($F$*$/!#(B
;;    (invoked . nil) ; Emacs $B$r5/F08e4{$K(B skk-mode $B$r5/F0$7$?$3$H$r<($9(B
;;    (isearch-message . nil) ; skk-isearch $B4X?t$r%3!<%k$9$k$?$a$N%U%i%0(B
;;    (kakutei-count . 0) ; $BJQ498uJd$r3NDj$7$?%+%&%s%H$rJ];}$9$kJQ?t(B
;;    (minibuffer-origin-mode . nil) ;$BF~NO%b!<%I$rI=$o$9%7%s%\%k(B
;;    (touroku-count . 0) ; $B<-=qEPO?$7$?%+%&%s%H$rJ];}$9$kJQ?t(B
;;    (update-jisyo-count . 0) ; $B<-=q$r99?7$7$?2s?t(B
;;    ;; buffer-local variables.
;;    ;;(current-search-prog-list . nil) ;skk-search-prog-list $B$N8=:_$NCM$rJ]B8$9$k%j%9%H(B
;;    ;;(exit-show-candidates . nil) ;$B%_%K%P%C%U%!$G8uJd$r<!!9$KI=<($7$F!"8uJd$,?T$-$?$3$H$r<($9(B
;;    ;;(henkan-active . nil) ; $B"'%b!<%I(B ($BJQ49Cf(B) $B$G$"$k$3$H$r<($9(B
;;    ;;(henkan-count . -1) ;skk-henkan-list $B$N%j%9%H$N%$%s%G%/%9$G8=:_$N8uJd$r:9$9$b$N(B
;;    ;;(henkan-end-point . nil ) ; $BJQ49=*N;%]%$%s%H$r<($9%^!<%+!<(B
;;    ;;(henkan-in-minibuff-flag . nil) ;$B%_%K%P%C%U%!$G<-=qEPO?$r9T$C$?$H$-$K$3$N%U%i%0$,N)$D(B
;;    ;;(henkan-key . nil) ;$BJQ49$9$Y$-8+=P$78l(B
;;    ;;(henkan-list . nil) ; $BJQ497k2L$N8uJd$N%j%9%H(B
;;    ;;(henkan-okurigana . nil) ;$B8=:_$NJQ49$NAw$j2>L>ItJ,(B
;;    ;;(henkan-on . nil) ; $B"&%b!<%I(B ($BJQ49BP>]$NJ8;zNs7hDj$N$?$a$N%b!<%I(B) $B$G$"$k$3$H$r<($9(B
;;    ;;(henkan-start-point . nil) ; $BJQ493+;O%]%$%s%H$r<($9%^!<%+!<(B
;;    ;;(kakutei-flag . nil) ; $B3NDj$7$FNI$$8uJd$r8+$D$1$?>uBV$G$"$k$3$H$r;X$9(B
;;    ;;(kana-start-point . nil) ;$B$+$JJ8;z$N3+;O%]%$%s%H$r<($9%^!<%+!<(B
;;    ;;(katakana . nil) ; $BF~NO%b!<%I$,%+%J%b!<%I$G$"$k$3$H$r<($9(B
;;    ;;(okuri-ari-max . nil) ; $B<-=q$NAw$jM-$j%(%s%H%j$N=*N;E@$r<($9%P%C%U%!%]%$%s%H(B
;;    ;;(okuri-ari-min . nil) ; $B<-=q$NAw$jM-$j%(%s%H%j$N3+;OE@$r<($9%P%C%U%!%]%$%s%H(B
;;    ;;(okuri-char . nil) ;$BJQ49$9$Y$-8l$NAw$j2>L>$NItJ,$N%W%l%U%#%C%/%9(B
;;    ;;(okuri-index-max . -1) ;skk-henkan-list $B$N%$%s%G%/%9$G<+F0Aw$j=hM}!"$b$7$/$O%5JQ8!:w$G8!:w$7$?:G8e$N8uJd$r;X$9$b$N(B
;;    ;;(okuri-index-min . -1) ;skk-henkan-list $B$N%$%s%G%/%9$G<+F0Aw$j=hM}!"$b$7$/$O%5JQ8!:w$G8!:w$7$?:G=i$N8uJd$r;X$9$b$N(B
;;    ;;(okuri-nasi-min . nil) ; $B<-=q$NAw$j$J$7%(%s%H%j$N3+;OE@$r<($9%P%C%U%!%]%$%s%H(B
;;    ;;(okurigana . nil) ; $BAw$j2>L>ItJ,$,F~NOCf$G$"$k$3$H$r<($9(B
;;    ;;(okurigana-start-point . nil) ; $BAw$j2>L>$N3+;O%]%$%s%H$r<($9%^!<%+!<(B
;;    ;;(prefix . "") ; $BF~NO$9$k$+$J$r7hDj$9$k$?$a$N%W%l%U%#%C%/%9(B
;;    ;;(previous-point . nil) ;$B$3$NJQ?t$KJ];}$5$l$k%]%$%s%H$,8=:_$N%]%$%s%H$H0[$J$k>l9g!"(Bskk-with-point-move $B$,;H$o$l$F$$$J$$%3%^%s%I$rF0:n$5$;$k$H!"(Bskk-after-point-move $B$,:nF0$9$k(B
;;    ;;(self-insert-non-undo-count . 1) ;skk-insert $B$b$7$/$O(B skk-jisx0208-latin-insert $B$GO"B3F~NO$7$?J8;z?t$rI=$o$9%+%&%s%?!<(B
;;    ))

(skk-deflocalvar skk-mode nil
  "Non-nil $B$G$"$l$P!"%+%l%s%H%P%C%U%!$G8=:_(B skk-mode $B$r5/F0$7$F$$$k$3$H$r<($9!#(B" )

(skk-deflocalvar skk-latin-mode nil
  "Non-nil $B$G$"$l$P!"F~NO%b!<%I$,(B ASCII $B%b!<%I$G$"$k$3$H$r<($9!#(B" )

(skk-deflocalvar skk-j-mode nil
  "Non-nil $B$G$"$l$P!"F~NO%b!<%I$,$+$J!&%+%J%b!<%I$G$"$k$3$H$r<($9!#(B" )

(skk-deflocalvar skk-katakana nil
  "Non-nil $B$G$"$l$P!"F~NO%b!<%I$,%+%J%b!<%I$G$"$k$3$H$r<($9!#(B
\"(and (not skk-katakana) skk-j-mode))\" $B$,(B t $B$G$"$l$P!"$+$J%b!<%I$G$"$k$3$H$r(B
$B<($9!#(B" )

(skk-deflocalvar skk-jisx0208-latin-mode nil
  "Non-nil $B$G$"$l$P!"F~NO%b!<%I$,A41Q%b!<%I$G$"$k$3$H$r<($9!#(B" )

(skk-deflocalvar skk-abbrev-mode nil
  "Non-nil $B$G$"$l$P!"F~NO%b!<%I$,(B SKK abbrev $B%b!<%I$G$"$k$3$H$r<($9!#(B" )

(skk-deflocalvar skk-okurigana nil
  "Non-nil $B$G$"$l$P!"Aw$j2>L>ItJ,$,F~NOCf$G$"$k$3$H$r<($9!#(B" )

(skk-deflocalvar skk-henkan-on nil
  "Non-nil $B$G$"$l$P!""&%b!<%I(B \($BJQ49BP>]$NJ8;zNs7hDj$N$?$a$N%b!<%I(B\) $B$G$"$k$3$H$r<($9!#(B" )

(skk-deflocalvar skk-henkan-active nil
  "Non-nil $B$G$"$l$P!""'%b!<%I(B \($BJQ49Cf(B\) $B$G$"$k$3$H$r<($9!#(B" )

(skk-deflocalvar skk-kakutei-flag nil
  "Non-nil $B$J$i3NDj$7$FNI$$8uJd$r8+$D$1$?>uBV$G$"$k$3$H$r;X$9!#(B
skk-henkan, skk-search-kakutei-jisyo-file, skk-henkan-show-candidates,
skk-henkan-in-minibuff $B$H(B skk-kakutei-save-and-init-variables $B$GJQ99!";2>H$5$l(B
$B$k!#(B" )

(skk-deflocalvar skk-exit-show-candidates nil
  "$B%_%K%P%C%U%!$G8uJd$r<!!9$KI=<($7$F!"8uJd$,?T$-$?$H$-$K(B non-nil $B$H$J$k!#(B
$B$=$NCM$O%j%9%H$G!"(Bcar $B$K(B skk-henkan-show-candidate $B4X?t$G(B while $B%k!<%W$r2s$C(B
$B$?2s?t$r<($90l;~JQ?t(B loop $B$NCM$r!"(Bcdr $BIt$K:G8e$K%_%K%P%C%U%!$KI=<($7$?(B 1 $B$DA0(B
$B$N8uJd72$N:G8e$NMWAG$r;X$9%$%s%G%/%9$,BeF~$5$l$k!#(B
skk-henkan-show-candidates, skk-henkan-in-minibuff $B$H(B
skk-kakutei-save-and-init-variables $B$GJQ99!";2>H$5$l$k!#(B" )

;; <$B%-!<%^%C%W4XO"(B>
(skk-deflocalvar skk-current-rule-tree nil
  "$B%m!<%^;z(B -> $B$+$JJQ49$N>uBVA+0\5,B'$rI=$o$9%D%j!<$N8=;~E@$N>uBV!#(B
$B%m!<%^;zF~NO$N=i4|$G$O(B skk-rule-tree $B$HF10l$N>uBV$G!"J8;zF~NO$,?J$`$K(B
$B$D$l!"LZ$r$?$I$C$F$f$/>uBV$NA+0\$rI=$9!#(B" )

;; <$B<-=q4XO"$NJQ?t(B>
(skk-deflocalvar skk-okuri-ari-min nil
  "SKK $B<-=q$NAw$jM-$j%(%s%H%j$N3+;OE@$r<($9%P%C%U%!%]%$%s%H!#(B")

(skk-deflocalvar skk-okuri-ari-max nil
  "SKK $B<-=q$NAw$jM-$j%(%s%H%j$N=*N;E@$r<($9%P%C%U%!%]%$%s%H!#(B
skk-jisyo $B$N%P%C%U%!$G$O<-=q$N99?7$NI,MW$,$"$k$?$a$K%^!<%+!<$,BeF~$5$l$k!#(B" )

(skk-deflocalvar skk-okuri-nasi-min nil
  "SKK $B<-=q$NAw$j$J$7%(%s%H%j$N3+;OE@$r<($9%P%C%U%!%]%$%s%H!#(B
skk-jisyo $B$N%P%C%U%!$G$O<-=q$N99?7$NI,MW$,$"$k$?$a$K%^!<%+!<$,BeF~$5$l$k!#(B" )

;; <$B$=$NB>(B>
(skk-deflocalvar skk-mode-line nil
  "SKK $B$N%b!<%I$r<($9%b!<%I%i%$%s$NJ8;zNs!#(B
skk-mode-string, skk-hiragana-mode-string, skk-katakana-mode-string
and skk-jisx0208-latin-mode-string $B$N$$$:$l$+$,BeF~$5$l$k!#(B" )

(skk-deflocalvar skk-previous-point nil
  "skk-with-point-move $B4XO"JQ?t!#(B
$B$3$NJQ?t$KJ];}$5$l$k%]%$%s%H$,8=:_$N%]%$%s%H$H0[$J$k>l9g!"(Bskk-with-point-move $B$,(B
$B;H$o$l$F$$$J$$%3%^%s%I$rF0:n$5$;$k$H!"(Bskk-after-point-move $B$,:nF0$9$k!#(B" )

;; "" $B$KBP1~$7$?%(%s%H%j$,(B skk-roma-kana-[aiue] $B$K$"$k$?$a!"(B"" $B$r(B nil $B$GBeMQ(B
;; $B$G$-$J$$!#(B
(skk-deflocalvar skk-prefix ""
  "$BF~NO$9$k$+$J$r7hDj$9$k$?$a$N%W%l%U%#%C%/%9!#(B" )

(skk-deflocalvar skk-henkan-start-point nil
  "$BJQ493+;O%]%$%s%H$r<($9%^!<%+!<!#(B" )

(skk-deflocalvar skk-henkan-end-point nil
  "$BJQ49=*N;%]%$%s%H$r<($9%^!<%+!<!#(B" )

(skk-deflocalvar skk-kana-start-point nil
  "$B$+$JJ8;z$N3+;O%]%$%s%H$r<($9%^!<%+!<!#(B" )

(skk-deflocalvar skk-okurigana-start-point nil
  "$BAw$j2>L>$N3+;O%]%$%s%H$r<($9%^!<%+!<!#(B" )

(skk-deflocalvar skk-henkan-key nil
  "$BJQ49$9$Y$-8+=P$78l!#(B
$BNc$($P!"(B\"$B"&$+$J(B\" $B$rJQ49$9$l$P!"(Bskk-henkan-key $B$K$O(B \"$B$+$J(B\" $B$,BeF~$5$l$k!#(B
\"$B"&$o$i(B*$B$&(B\" $B$N$h$&$JAw$j$"$j$NJQ49$N>l9g$K$O!"(B\"$B$o$i(Bu\" $B$N$h$&$K!"4A;zItJ,$N(B
$BFI$_$,$J(B + $BAw$j2>L>$N:G=i$NJ8;z$N%m!<%^;z$N%W%l%U%#%C%/%9$,BeF~$5$l$k!#(B" )

(skk-deflocalvar skk-okuri-char nil
  "$BJQ49$9$Y$-8l$NAw$j2>L>$NItJ,$N%W%l%U%#%C%/%9!#(B
$BNc$($P!"(B\"$B$*$/(B*$B$j(B\" $B$rJQ49$9$k$H$-$O!"(Bskk-okuri-char $B$O(B \"r\"$B!#(B
skk-okuri-char $B$,(B non-nil $B$G$"$l$P!"Aw$j$"$j$NJQ49$G$"$k$3$H$r<($9!#(B" )

(skk-deflocalvar skk-henkan-okurigana nil
  "$B8=:_$NJQ49$NAw$j2>L>ItJ,!#(B
$BNc$($P!"(B\"$B"&$&$^$l(B*$B$k(B\" $B$rJQ49$9$l$P!"(Bskk-henkan-okurigana $B$K$O(B \"$B$k(B\" $B$,BeF~(B
$B$5$l$k!#(B" )

(skk-deflocalvar skk-last-kakutei-henkan-key nil
  "$B3NDj<-=q$K$h$j:G8e$K3NDj$7$?$H$-$N8+=P$78l!#(B
$B3NDj<-=q$K$h$k3NDj$ND>8e$K(B x $B%-!<$r2!$9$H3NDj$,%"%s%I%%$5$l$F!"3NDjA0$N>uBV$G(B
$B$3$N8+=P$78l$,%+%l%s%H%P%C%U%!$KA^F~$5$l$k!#(B" )

(skk-deflocalvar skk-henkan-list nil
  "$BJQ497k2L$N8uJd$N%j%9%H!#(B
$BNc$($P!"(B\"$B"&$J(B*$B$/(B\" $B$H$$$&JQ49$9$l$P!"(Bskk-henkan-list $B$O(B
(\"$BLD(B\" \"$B5c(B\" \"$BL5(B\" \"$BK4(B\") $B$N$h$&$K$J$k!#(B" )

(skk-deflocalvar skk-henkan-count -1
  "skk-henkan-list $B$N%j%9%H$N%$%s%G%/%9$G8=:_$N8uJd$r:9$9$b$N!#(B" )

(skk-deflocalvar skk-self-insert-non-undo-count 1
  "skk-insert $B$b$7$/$O(B skk-jisx0208-latin-insert $B$GO"B3F~NO$7$?J8;z?t$rI=$o$9%+%&%s%?!<!#(B
Emacs $B$N%*%j%8%J%k$NF0:n$G$O!"(Bself-insert-command $B$K%P%$%s%I$5$l$?%-!<F~NO$O!"(B
$BO"B3(B 20 $B2s$^$G$,(B 1 $B$D$N%"%s%I%%$NBP>]$H$J$k!#$3$NF0:n$r%(%_%e%l!<%H$9$k$?$a$N(B
$B%+%&%s%?!<!#$3$N%+%&%s%?!<$,!"(B20 $B0J2<$G$"$k$H$-$O!"F~NO$N$?$S$K(B 
cancel-undo-boundary $B$,%3!<%k$5$l$k!#(B" )

(skk-deflocalvar skk-current-search-prog-list nil
  "skk-search-prog-list $B$N8=:_$NCM$rJ]B8$9$k%j%9%H!#(B
$B:G=i$NJQ49;~$O(B skk-search-prog-list $B$NA4$F$NCM$rJ];}$7!"JQ49$r7+$jJV$9$?$S$K(B 1
$B$D$E$DC;$/$J$C$F$f$/!#(B" )
  
;; for skk-undo-kakutei
(skk-deflocalvar skk-last-henkan-data nil
  "$B:G8e$K9T$J$C$?JQ49$K4X$9$k%G!<%?$N%(!<%j%9%H!#(B
$B%G%#%U%)%k%H$N%-!<$O!"(Bhenkan-key, henkan-okurigana,
okuri-char, henkan-list $B$N3F%7%s%\%k!#(B
\(skk-num $B$r(B require $B$7$F$$$k$H$-$O!"(Bnum-list $B$,DI2C$5$l$k(B\)$B!#(B" )

(skk-deflocalvar skk-henkan-overlay nil
  "$B8uJd$rI=<($9$k$H$-$K;HMQ$9$k(B Overlay$B!#(B" )

(skk-deflocalvar skk-henkan-in-minibuff-flag nil
  "$B%_%K%P%C%U%!$G<-=qEPO?$r9T$C$?$H$-$K$3$N%U%i%0$,N)$D!#(B
skk-remove-common $B$G;2>H$5$l$k!#(B" )

(skk-deflocalvar skk-okuri-index-min -1
  "skk-henkan-list $B$N%$%s%G%/%9$G<+F0Aw$j=hM}!"$b$7$/$O%5JQ8!:w$G8!:w$7$?:G=i$N8uJd$r;X$9$b$N!#(B" )

(skk-deflocalvar skk-okuri-index-max -1
  "skk-henkan-list $B$N%$%s%G%/%9$G<+F0Aw$j=hM}!"$b$7$/$O%5JQ8!:w$G8!:w$7$?:G8e$N8uJd$r;X$9$b$N!#(B" )

(set-modified-alist
 'minor-mode-map-alist
 (list (cons 'skk-latin-mode skk-latin-mode-map)
       (cons 'skk-abbrev-mode skk-abbrev-mode-map)
       (cons 'skk-j-mode skk-j-mode-map)
       (cons 'skk-jisx0208-latin-mode skk-jisx0208-latin-mode-map) ))

;;;; defadvices.
;; defadvice $B$GDj5A$9$k$H!"8e$G%f!<%6!<$,?75,$N5!G=$rIU$1$F99$K(B defadvice $B$7$F(B
;; $B$b$A$c$s$HF0$/!#(B

;; cover to original functions.

(defadvice keyboard-quit (around skk-ad activate)
  "$B"'%b!<%I$G$"$l$P!"8uJd$NI=<($r$d$a$F"&%b!<%I$KLa$9(B ($B8+=P$78l$O;D$9(B)$B!#(B
$B"&%b!<%I$G$"$l$P!"8+=P$78l$r:o=|$9$k!#(B
$B>e5-$N$I$A$i$N%b!<%I$G$b$J$1$l$P(B keyboard-quit $B$HF1$8F0:n$r$9$k!#(B"
  (cond 
   ;; SKK is not invoked in the current buffer.
   ((not skk-mode) ad-do-it)
   ;; $B"#(B mode (Kakutei input mode).
   ((not skk-henkan-on)
    (cond ((skk-get-prefix skk-current-rule-tree)
	   (skk-erase-prefix 'clean) )
	  (t ad-do-it) ))
   ;; $B"'(B mode (Conversion mode).
   (skk-henkan-active
    (setq skk-henkan-count 0)
    (if (and skk-delete-okuri-when-quit skk-henkan-okurigana)
	(let ((count (/ (length skk-henkan-okurigana) skk-kanji-len)))
	  (skk-previous-candidate)
	  ;; $B$3$3$G$O(B delete-backward-char $B$KBhFs0z?t$rEO$5$J$$J}$,%Y%?!<!)(B
	  (delete-backward-char count) )
      (skk-previous-candidate) ))
   ;; $B"&(B mode (Midashi imput mode).
   (t (skk-erase-prefix 'clean)
      (and (> (point) skk-henkan-start-point)
	   (delete-region (point) skk-henkan-start-point) )
      (skk-kakutei) )))

(defadvice abort-recursive-edit (around skk-ad activate)
  "$B"'%b!<%I$G$"$l$P!"8uJd$NI=<($r$d$a$F"&%b!<%I$KLa$9(B ($B8+=P$78l$O;D$9(B)$B!#(B
$B"&%b!<%I$G$"$l$P!"8+=P$78l$r:o=|$9$k!#(B
$B>e5-$N$I$A$i$N%b!<%I$G$b$J$1$l$P(B abort-recursive-edit $B$HF1$8F0:n$r$9$k!#(B"
  (skk-remove-minibuffer-setup-hook
   'skk-j-mode-on 'skk-setup-minibuffer
   (function (lambda () (add-hook 'pre-command-hook 'skk-pre-command nil 'local))) )
  (cond ((not skk-mode) ad-do-it)
	((not skk-henkan-on)
	 (cond ((skk-get-prefix skk-current-rule-tree)
		(skk-erase-prefix 'clean) )
	       (t ad-do-it) ))
        (skk-henkan-active
         (setq skk-henkan-count 0)
         (if (and skk-delete-okuri-when-quit skk-henkan-okurigana)
             (let ((count (/ (length skk-henkan-okurigana) skk-kanji-len)))
               (skk-previous-candidate)
               ;; $B$3$3$G$O(B delete-backward-char $B$KBhFs0z?t$rEO$5$J$$J}$,%Y%?!<!)(B
               (delete-backward-char count) )
           (skk-previous-candidate) ))
	(t (skk-erase-prefix 'clean)
	   (and (> (point) skk-henkan-start-point)
		(delete-region (point) skk-henkan-start-point) )
	   (skk-kakutei) )))
	
(defadvice newline (around skk-ad activate)
  "skk-egg-like-newline $B$,(B non-nil $B$@$C$?$i!"JQ49Cf$N(B newline $B$G3NDj$N$_9T$$!"2~9T$7$J$$!#(B"
  (if (not (or skk-j-mode skk-abbrev-mode))
      ad-do-it
    (let (
	  ;;(arg (ad-get-arg 0))
          ;; skk-kakutei $B$r<B9T$9$k$H(B skk-henkan-on $B$NCM$,L5>r7o$K(B nil $B$K$J$k(B
          ;; $B$N$G!"J]B8$7$F$*$/I,MW$,$"$k!#(B
          (no-newline (and skk-egg-like-newline skk-henkan-on))
	  (auto-fill-function (and (interactive-p) auto-fill-function)) )
      ;; fill $B$5$l$F$b(B nil $B$,5"$C$F$/$k(B :-<
      ;;(if (skk-kakutei)
      ;;    (setq arg (1- arg)) )
      ;;(if skk-mode
      ;;    (let ((opos (point)))
      ;;      ;; skk-kakutei (skk-do-auto-fill) $B$K$h$C$F9T$,@^$jJV$5$l$?$i(B arg $B$r(B 1 $B$D8:$i$9!#(B
      ;;      (skk-kakutei)
      ;;      (if (and (not (= opos (point))) (integerp arg))
      ;;          (ad-set-arg 0 (1- arg)) )))
      (and skk-mode (skk-kakutei))
      (if (not no-newline)
	  ad-do-it ))))

(defadvice newline-and-indent (around skk-ad activate)
  "skk-egg-like-newline $B$,(B non-nil $B$@$C$?$i!"JQ49Cf$N(B newline-and-indent $B$G3NDj$N$_9T$$!"2~9T$7$J$$!#(B"
  (if (not (or skk-j-mode skk-abbrev-mode))
      ad-do-it
    (let ((no-newline (and skk-egg-like-newline skk-henkan-on))
	  (auto-fill-function (and (interactive-p) auto-fill-function)) )
      (and skk-mode (skk-kakutei))
      (or no-newline ad-do-it) )))

(defadvice exit-minibuffer (around skk-ad activate)
  "skk-egg-like-newline $B$,(B non-nil $B$@$C$?$i!"JQ49Cf$N(B exit-minibuffer $B$G3NDj$N$_9T$&!#(B"
  (skk-remove-minibuffer-setup-hook
   'skk-j-mode-on 'skk-setup-minibuffer
   (function (lambda ()
	       (add-hook 'pre-command-hook 'skk-pre-command nil 'local) )))
  (if (not (or skk-j-mode skk-abbrev-mode))
      ad-do-it
    (let ((no-newline (and skk-egg-like-newline skk-henkan-on)))
      (and skk-mode (skk-kakutei))
      (or no-newline ad-do-it) )))

(defadvice picture-mode-exit (before skk-ad activate)
  "SKK $B$N%P%C%U%!%m!<%+%kJQ?t$rL58z$K$7!"(Bpicture-mode-exit $B$r%3!<%k$9$k!#(B
picture-mode $B$+$i=P$?$H$-$K$=$N%P%C%U%!$G(B SKK $B$r@5>o$KF0$+$9$?$a$N=hM}!#(B"
  (and skk-mode (skk-kill-local-variables)) )

(defadvice undo (before skk-ad activate)
  "SKK $B%b!<%I$,(B on $B$J$i(B skk-self-insert-non-undo-count $B$r=i4|2=$9$k!#(B"
  (and skk-mode (setq skk-self-insert-non-undo-count 0)) )

(defadvice kill-buffer (before skk-ad activate)
  "SKK $B$N"'%b!<%I$@$C$?$i!"3NDj$7$F$+$i%P%C%U%!$r%-%k$9$k!#(B
  $B%P%C%U%!$N%-%k8e!"(BSKK $B$N%b!<%I$K=>$$%+!<%=%k$N?'$rJQ$($k!#(B"
  (and skk-mode skk-henkan-on (interactive-p) (skk-kakutei)) )

(defadvice save-buffers-kill-emacs (before skk-ad activate)
  (run-hooks 'skk-before-kill-emacs-hook) )

(if (eq skk-emacs-type 'xemacs)
    ;; XEmacs has minibuffer-keyboard-quit that has nothing to do with delsel.
    (defadvice minibuffer-keyboard-quit (around skk-ad activate)
      (skk-remove-minibuffer-setup-hook
       'skk-j-mode-on 'skk-setup-minibuffer
       (function (lambda ()
		   (add-hook 'pre-command-hook 'skk-pre-command nil 'local) )))
      (cond ((not skk-mode) ad-do-it)
	    ((not skk-henkan-on)
	     (cond ((skk-get-prefix skk-current-rule-tree)
		    (skk-erase-prefix 'clean) )
		    (t ad-do-it) ))
            (skk-henkan-active
             (setq skk-henkan-count 0)
             (if (and skk-delete-okuri-when-quit skk-henkan-okurigana)
                 (let ((count (/ (length skk-henkan-okurigana) skk-kanji-len)))
                   (skk-previous-candidate)
                   ;; $B$3$3$G$O(B delete-backward-char $B$KBhFs0z?t$rEO$5$J$$J}$,%Y%?!<!)(B
                   (delete-backward-char count) )
               (skk-previous-candidate) ))
            (t (skk-erase-prefix 'clean)
	       (and (> (point) skk-henkan-start-point)
		    (delete-region (point) skk-henkan-start-point) )
               (skk-kakutei) )))
  (defadvice minibuffer-keyboard-quit (around skk-ad activate)
    ;; for delsel.el
    (if (and skk-mode
	     (not (and delete-selection-mode transient-mark-mode mark-active)) )
	(keyboard-quit)
      ad-do-it )))

;;;; mode setup

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
                       ((> (prefix-numeric-value arg) 0) t) ))
  (if (not skk-mode)
      ;; exit skk-mode
      (progn
        (let ((skk-mode t)) (skk-kakutei))
        (skk-mode-off) 
	(and (eq skk-status-indicator 'left)
	     (setq skk-input-mode-string "") )
	(and (eq skk-emacs-type 'xemacs) (easy-menu-remove skk-menu)) )
    ;; enter skk-mode
    (if (not skk-mode-invoked)
        ;; enter skk-mode for the first time in this session
        (progn
	  (and (eq skk-emacs-type 'xemacs)
	       (boundp 'preloaded-file-list)
	       (member "skk-leim" preloaded-file-list)
	       ;; require dummy file.
	       (require 'skk-vars) )
          (skk-setup-init-file)
          (load skk-init-file t)
	  (skk-setup-modeline)
	  (require 'skk-autoloads)
	  (if (or (memq skk-emacs-type '(mule3 mule4))
		  (and (eq skk-emacs-type 'xemacs)
		       (or 
			;; XEmacs 21 or later.
			(> emacs-major-version 20)
			;; XEmacs 20.4 or later.
			(> emacs-minor-version 2) )))
	      (require 'skk-leim) )
	  (if skk-use-numeric-conversion (require 'skk-num))
          (if skk-keep-record
	      (skk-create-file skk-record-file
			       "SKK $B$N5-O?MQ%U%!%$%k$r:n$j$^$7$?(B"
			       "I have created an SKK record file for you" ))
	  (skk-create-file skk-jisyo
			   "SKK $B$N6u<-=q$r:n$j$^$7$?(B"
			   "I have created an empty SKK Jisyo file for you" )
	  (skk-regularize)
          (setq skk-mode-invoked t) ))
    ;; $B0J2<$O(B skk-mode $B$KF~$k$?$S$KKhEY%3!<%k$5$l$k%3!<%I!#(B
    (and skk-use-viper (require 'skk-viper))
    (and (or skk-use-color-cursor skk-use-cursor-change)
	 (require 'skk-cursor) )
    ;; .skk $B$G(B skk-kakutei-key $B$NJQ99$,2DG=$K$J$k$h$&$K!#(B
    (define-key skk-abbrev-mode-map skk-kakutei-key 'skk-kakutei)
    (define-key skk-abbrev-mode-map (char-to-string skk-start-henkan-char)
      'skk-start-henkan )
    (define-key skk-abbrev-mode-map (char-to-string skk-try-completion-char)
      'skk-try-completion )
    (define-key skk-latin-mode-map skk-kakutei-key 'skk-kakutei)
    (define-key skk-j-mode-map skk-kakutei-key 'skk-kakutei)
    (define-key skk-j-mode-map (char-to-string skk-try-completion-char)
      'skk-insert )
    (define-key skk-j-mode-map (char-to-string skk-previous-candidate-char)
      'skk-previous-candidate )
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
 	'skk-backward-and-set-henkan-point) 
      )
    (skk-setup-delete-backward-char)
    ;; XEmacs doesn't have minibuffer-local-ns-map
    (and (boundp 'minibuffer-local-ns-map)
	 (define-key minibuffer-local-ns-map skk-kakutei-key 'skk-kakutei) )
    ;; To terminate kana input.
    (make-local-hook 'pre-command-hook)
    (add-hook 'pre-command-hook 'skk-pre-command nil 'local)
    (make-local-hook 'post-command-hook)
    (add-hook 'post-command-hook 'skk-after-point-move nil 'local)
    (and (eq skk-status-indicator 'left)
	 (setq skk-input-mode-string skk-hiragana-mode-string) )
    (skk-j-mode-on)
    (and (eq skk-emacs-type 'xemacs) (easy-menu-add skk-menu))
    (run-hooks 'skk-mode-hook) ))

;;;###autoload
(defun skk-auto-fill-mode (&optional arg)
  "$BF|K\8lF~NO%b!<%I!#<+F0@^$jJV$75!G=IU$-!#(B
$B%^%$%J!<%b!<%I$N0l<o$G!"%*%j%8%J%k$N%b!<%I$K$O1F6A$rM?$($J$$!#(B
$B@5$N0z?t$rM?$($k$H!"6/@)E*$K(B auto-fill-mode $B5Z$S(B SKK $B%b!<%I$KF~$k!#(B
$BIi$N0z?t$rM?$($k$H(B auto-fill-mode $B5Z$S(B SKK $B%b!<%I$+$iH4$1$k!#(B"
  (interactive "P")
  (let ((auto-fill
         (cond ((null arg) (not auto-fill-function))
               ((> (prefix-numeric-value arg) 0) t) )))
    (auto-fill-mode (if auto-fill 1 -1))
    (skk-mode arg)
    (run-hooks 'skk-auto-fill-mode-hook) ))

(defun skk-kill-emacs-without-saving-jisyo (&optional query)
  "SKK $B<-=q$r%;!<%V$7$J$$$G!"(BEmacs $B$r=*N;$9$k!#(B"
  (interactive "P")
  ;; format $B$r0z?t$K;}$?$;$?>l9g$O!"(Bskk-yes-or-no-p $B$r;H$&$H$+$($C$F>iD9$K$J$k!#(B
  (if (yes-or-no-p
       (format (if skk-japanese-message-and-error
                   "$B<-=q$NJ]B8$r$;$:$K(B %s $B$r=*N;$7$^$9!#NI$$$G$9$+!)(B"
                 "Do you really wish to kill %s without saving Jisyo? " )
               (cond ((eq skk-emacs-type 'xemacs) "XEmacs")
		     (t "Mule") )))
      (let ((buff (skk-get-jisyo-buffer skk-jisyo 'nomsg)))
	(ad-disable-advice 'save-buffers-kill-emacs 'before 'skk-ad)
	(ad-activate 'save-buffers-kill-emacs)
	(remove-hook 'skk-before-kill-emacs-hook 'skk-save-jisyo) ; fail safe.
	(if buff
	    (progn (set-buffer buff)
		   (set-buffer-modified-p nil)
		   (kill-buffer buff) ))
	(save-buffers-kill-emacs query) )))

(defun skk-restart ()
  "skk-init-file $B$N:F%m!<%I5Z$S3F<o:F%;%C%H%"%C%W$N8e!"(BSKK $B%b!<%I$r5/F0$9$k!#(B"
  (interactive)
  (let (skk-mode-invoked) (skk-mode 1)) )

(defun skk-regularize ()
  ;; SKK $B$NF0:n$N@55,2=$r?^$k$?$a!"FbItJQ?t$d%f!<%6!<JQ?t$ND4@0$r9T$J$&!#(B
  (skk-setup-auto-paren)
  (setq skk-rule-tree
	(skk-compile-rule-list skk-rom-kana-base-rule-list skk-rom-kana-rule-list) )
  (and (not (featurep 'skk-server))
       (or (and (boundp 'skk-servers-list) skk-servers-list)
	   (or (and (boundp 'skk-server-host) skk-server-host)
	       (getenv "SKKSERVER") ))
       (require 'skk-server) )
  (and (featurep 'skk-server)
       ;; skk-search-server $B$O%5!<%P!<$,Mn$A$F$b;H$($k$N$G!"30$5$J$$!#(B
       (skk-adjust-search-prog-list-for-server-search 'non-del) )
  (and skk-auto-okuri-process (skk-adjust-search-prog-list-for-auto-okuri))
  (and skk-use-look (require 'skk-look))
  (skk-setup-delete-selection-mode)
  (skk-adjust-user-option) )

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
	keys )
    (while commands
      (setq keys (where-is-internal (car commands) overriding-local-map)
	    commands (cdr commands) )
      (while keys
	(define-key skk-abbrev-mode-map (car keys) 'skk-delete-backward-char)
	(define-key skk-j-mode-map (car keys) 'skk-delete-backward-char)
	(setq keys (cdr keys)) ))))

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
                          ".elc" ))))
      (if skk-byte-compile-init-file
          (and (file-exists-p init-file)
	       (or (not (file-exists-p elc))
		   (file-newer-than-file-p init-file elc) )
	       (save-window-excursion ; for keep window configuration.
		 (skk-message "%s $B$r%P%$%H%3%s%Q%$%k$7$^$9!#(B" "Byte-compile %s"
			      skk-init-file )
		 (sit-for 2)
		 (byte-compile-file init-file) ))
        (and (file-exists-p init-file)
	     (file-exists-p elc)
	     (file-newer-than-file-p init-file elc)
	     (delete-file elc) )))))

(defun skk-emulate-original-map (arg)
  ;; $B%-!<F~NO$KBP$7$F!"(BSKK $B$N%b!<%I$G$O$J$/!"(BEmacs $B$N%*%j%8%J%k$N%-!<3d$jIU$1$G(B
  ;; $B%3%^%s%I$r<B9T$9$k!#(B
  (let ((prefix-arg arg)
        (keys (skk-command-key-sequence (this-command-keys) this-command)) )
    (if (not keys)
        ;; no alternative commands.  may be invoked by M-x.
        nil
      (let (skk-mode skk-latin-mode skk-j-mode skk-abbrev-mode skk-jisx0208-latin-mode
                     command )
        (setq command (key-binding keys))
        (if (eq command this-command)
            ;; avoid recursive calling of skk-emulate-original-map.
            nil
          ;; if no bindings are found, call `undefined'.  it's
          ;; original behaviour.
          (skk-cancel-undo-boundary)
          (command-execute (or command (function undefined))))))))

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
  (and (featurep 'delsel)
       (not (get 'skk-insert 'delete-selection))
       (mapcar (function (lambda (func) (put func 'delete-selection t)))
	       '(skk-current-kuten
		 skk-current-touten
		 skk-input-by-code-or-menu
		 skk-insert
		 skk-today ))))

(defun skk-setup-auto-paren ()
  (if (and skk-auto-insert-paren skk-auto-paren-string-alist)
      (let ((strlst (mapcar 'char-to-string skk-special-midashi-char-list))
	    rulealst str alist )
	(while strlst
	  ;; skk-auto-paren-string-alist $B$NCf$+$i!"(Bskk-special-midashi-char-list
	  ;; $B$NMWAG$K4XO"$9$k$b$N$r<h$j=|$/!#(B
	  (remove-alist 'skk-auto-paren-string-alist (car strlst))
	  (setq strlst (cdr strlst)) )
	(if (null (memq t (mapcar (function
				   (lambda (e)
				     (skk-ascii-char-p (string-to-char (car e))) ))
				  skk-auto-paren-string-alist )))
	    nil
	  (setq alist skk-auto-paren-string-alist
		rulealst (nconc (mapcar (function (lambda (e) (nth 2 e)))
					skk-rom-kana-rule-list )
				(mapcar (function (lambda (e) (nth 2 e)))
					skk-rom-kana-base-rule-list )))
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
						    skk-rom-kana-rule-list )))
	    (setq alist (cdr alist)) )))))

(defun skk-adjust-user-option ()
  ;; $BN>N)$G$-$J$$%*%W%7%g%s$ND4@0$r9T$J$&!#(B
  (and skk-process-okuri-early
       ;; skk-process-okuri-early $B$NCM$,(B non-nil $B$G$"$k$H$-$K2<5-$NCM$,(B non-nil
       ;; $B$G$"$l$P@5>o$KF0$+$J$$$N$G$3$NJQ?t$NM%@h=g0L$r9b$/$7$?!#(B
       (setq skk-kakutei-early nil
	     skk-auto-okuri-process nil
	     skk-henkan-okuri-strictly nil
	     skk-henkan-strict-okuri-precedence nil )))

(defun skk-try-completion (arg)
  "$B"&%b!<%I$G8+=P$78l$NJd40$r9T$&!#(B
$B$=$l0J30$N%b!<%I$G$O!"%*%j%8%J%k$N%-!<3d$jIU$1$N%3%^%s%I$r%(%_%e%l!<%H$9$k!#(B"
  (interactive "P")
  (skk-with-point-move
   (if (and skk-henkan-on (not skk-henkan-active))
       (progn
	 (setq this-command 'skk-completion)
	 (skk-completion (not (eq last-command 'skk-completion))) )
     (skk-emulate-original-map arg) )))

(defun skk-latin-mode (arg)
  "SKK $B$N%b!<%I$r(B latin (ascii) $B%b!<%I$KJQ99$9$k!#(B"
  (interactive "P")
  (skk-kakutei)
  (skk-latin-mode-on) )

(defun skk-jisx0208-latin-mode (arg)
  "SKK $B$N%b!<%I$rA43Q1Q;zF~NO%b!<%I$KJQ99$9$k!#(B"
  (interactive "P")
  (skk-kakutei)
  (skk-jisx0208-latin-mode-on) )

(defun skk-abbrev-mode (arg)
  "ascii $BJ8;z$r%-!<$K$7$?JQ49$r9T$&$?$a$NF~NO%b!<%I!#(B"
  (interactive "*P")
  (and skk-henkan-on (not skk-henkan-active)
       (skk-error "$B4{$K"&%b!<%I$KF~$C$F$$$^$9(B" "Already in $B"&(B mode") )
  (skk-kakutei)
  (skk-set-henkan-point-subr)
  (skk-abbrev-mode-on) )

(defun skk-toggle-kana (arg)
  "$B$R$i$,$J%b!<%I$H%+%?%+%J%b!<%I$r%H%0%k$G@Z$jBX$($k!#(B
$B%+%?%+%J%b!<%I$GJQ49$r9T$J$&$H$-$K!"Aw$j2>L>$r%+%?%+%J$KJQ49$7$?$/$J$$$H$-$O!"(B
skk-convert-okurigana-into-katakana $B$NCM$r(B non-nil $B$K$9$k!#(B

$B"&%b!<%I$G$O!"(Bskk-henkan-start-point ($B"&$ND>8e(B) $B$H%+!<%=%k$N4V$NJ8;zNs$r(B

    $B$R$i$,$J(B <=> $B%+%?%+%J(B
    $BA43Q1Q?t;z(B <=> ascii

$B$N$h$&$KJQ49$9$k!#(B"
  (interactive "P")
  (cond ((and skk-henkan-on (not skk-henkan-active))
         (let (char)
           (skk-save-point
             (goto-char skk-henkan-start-point)
             ;; "$B!<(B" $B$G$OJ8;z<oJL$,H=JL$G$-$J$$$N$G!"%]%$%s%H$r?J$a$k!#(B
             (while (looking-at "$B!<(B")
               (forward-char 1) )
             (setq char (skk-what-char-type)) )
           (skk-set-marker skk-henkan-end-point (point))
           (cond ((eq char 'hiragana)
                  (skk-katakana-henkan arg) )
                 ((eq char 'katakana)
                  (skk-hiragana-henkan arg) )
                 ((eq char 'jisx0208-latin)
                  (skk-jisx0208-latin-henkan arg) )
                 ((eq char 'ascii)
                  (skk-latin-henkan arg) ))))
        ((and (skk-in-minibuffer-p) (not skk-j-mode))
         ;; $B%_%K%P%C%U%!$X$N=iFMF~;~!#(B
         (skk-j-mode-on) )
        (t (setq skk-katakana (not skk-katakana))) )
  (skk-kakutei)
  (setq skk-input-mode-string (if skk-katakana skk-katakana-mode-string
				skk-hiragana-mode-string ))
  (force-mode-line-update) )

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
  (and skk-mode (skk-kill-local-variables)) )

(defun skk-kill-local-variables ()
  ;; SKK $B4XO"$N%P%C%U%!%m!<%+%kJQ?t$rL58z$K$9$k!#(B
  (skk-mode -1)
  (let ((lv (buffer-local-variables))
        v vstr )
    (while lv
      (setq v (car (car lv))
            lv (cdr lv)
            vstr (prin1-to-string v) )
      (and (> (length vstr) 3) (string= "skk-" (substring vstr 0 4))
	   (kill-local-variable v) ))))

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
  (interactive "p*")
  (skk-with-point-move
   (let ((ch last-command-char))
     (cond (
	    ;; start writing a midasi key.
	    (or (and (memq ch skk-set-henkan-point-key)
		     (or skk-okurigana
			 (not (skk-get-prefix skk-current-rule-tree))
			 (not (skk-select-branch skk-current-rule-tree ch)) ))
		(and skk-henkan-on (memq ch skk-special-midashi-char-list)) )
	    ;; normal pattern
	    ;; skk-set-henkan-point -> skk-kana-input.
	    (skk-set-henkan-point arg) )
	   ;; start conversion.
	   ((and skk-henkan-on (eq ch skk-start-henkan-char))
	    (skk-start-henkan arg) ) 
	   ;; for completion.
	   ((and skk-henkan-on (not skk-henkan-active))
	    (cond ((eq ch skk-try-completion-char)
		   (setq this-command 'skk-completion)
		   (skk-completion (not (eq last-command 'skk-completion))) )
		  ((eq last-command 'skk-completion)
		   (cond ((eq ch skk-next-completion-char)
			  (setq this-command 'skk-completion)
			  (skk-completion nil) )
			 ((eq ch skk-previous-completion-char)
			  (setq this-command 'skk-completion)
			  (skk-previous-completion) )
			 (t (skk-kana-input arg)) ))
		  (t (skk-kana-input arg)) ))
	   ;; just imput Kana.
	   (t (skk-kana-input arg)) ))))

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
	(queue (list last-command-char)) )
    (while queue
      (if (not (skk-get-prefix skk-current-rule-tree))
	  (progn
	    (skk-set-marker skk-kana-start-point (point))
	    (setq skk-current-rule-tree skk-rule-tree) )
	(skk-erase-prefix) )
      (setq skk-prefix (concat (skk-get-prefix skk-current-rule-tree)
			       (char-to-string last-command-char)))
      (let ((next (skk-select-branch skk-current-rule-tree (car queue)))
	    data )
	(if next
	    ;; can go down SKK-CURRENT-RULE-TREE
	    (if (skk-get-branch-list next)
		;; NEXT have at least one branch
		(progn
		  (and skk-henkan-active
		       skk-kakutei-early
		       (not skk-process-okuri-early) 
		       (skk-kakutei) )
		  (setq queue (cdr queue)
			skk-current-rule-tree next ))
	      ;; NEXT does not have any branch (i.e. NEXT is a leaf)
	      (setq data (skk-get-kana next)
		    queue (nconc (string-to-char-list (skk-get-nextstate next))
				 (cdr queue) )
		    skk-current-rule-tree nil ))
	  ;; can not go down SKK-CURRENT-RULE-TREE
	  (let ((d (skk-get-kana skk-current-rule-tree)))
	    (if d
		;; SKK-CURRENT-RULE-TREE have a roma->kana rule
		(setq data d
		      queue
		      (nconc (string-to-char-list
			      (skk-get-nextstate skk-current-rule-tree) )
			     queue )
		      skk-current-rule-tree nil )
	      ;; SKK-CURRENT-RULE-TREE does not have any roma->kana rule
	      (let ((dd (and skk-kana-input-search-function
			     (funcall skk-kana-input-search-function) )))
		(if dd
		    (setq data (car dd)
			  queue (nconc (string-to-char-list (cdr dd))
				       (cdr queue) )
			  skk-current-rule-tree nil )
		  (if (eq skk-current-rule-tree skk-rule-tree)
		      ;; typo on the root of tree
		      (setq queue nil
			    skk-current-rule-tree nil )
		    ;; otherwise move to root of the tree, and redo
		    (setq skk-current-rule-tree nil) ))))))
	(if (not data)
	    (if skk-current-rule-tree
		(progn
		  ;;(digit-argument arg)
		  ;; $B$&!A$s!"$h$&J,$+$i$s!#$H$j$"$($:!#(B
		  (or skk-isearch-message (setq prefix-arg arg))
		  (setq skk-prefix (skk-get-prefix skk-current-rule-tree))
		  (skk-insert-prefix skk-prefix) )
	      ;;(skk-kana-cleanup 'force)
	      (and skk-henkan-active (skk-kakutei))
	      (setq skk-prefix "")
	      (or queue
		  (skk-emulate-original-map (skk-make-raw-arg arg)) ))
	  (skk-cancel-undo-boundary)
	  (setq skk-prefix "")
	  (and (functionp data)
	       (setq data (funcall data (skk-make-raw-arg arg))) )
	  (if (not (stringp (if (consp data) (car data) data)))
	      nil
	    (let* ((str (if (consp data) (if skk-katakana (car data) (cdr data))
			  data ))
		   (pair (and skk-auto-insert-paren
			      (cdr (assoc str skk-auto-paren-string-alist)) ))
		   (count0 arg) (count1 arg) (inserted 0) )
	      (and skk-henkan-active
		   skk-kakutei-early (not skk-process-okuri-early)
		   (skk-kakutei) )
	      ;; arg $B$OJ]B8$7$F$*$+$J$$$H!"(B0 $B$K$J$C$F$7$^$$!"(Bqueue
	      ;; $B$,$?$^$C$F$$$F:FEY$3$3$X$d$C$FMh$?$H$-$KJ8;zF~NO$,(B
	      ;; $B$G$-$J$/$J$k!#(B
	      (while (> count0 0)
		(skk-insert-str str)
		(setq count0 (1- count0)) )
	      (if (not pair)
		  nil
		(while (> count1 0)
		  (if (not (string= pair (char-to-string (following-char))))
		      (progn
			(setq inserted (1+ inserted))
			(skk-insert-str pair) ))
		  (setq count1 (1- count1)) )
		(or (= inserted 0) (backward-char inserted)) )
	      (and skk-okurigana (null queue) (skk-set-okurigana)) ))))
      ;; XXX I don't know how skk-isearch-message works....
      (and skk-isearch-message (skk-isearch-message)) )))

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
	 (addpoint (cdr result)) )
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
(defun skk-compile-rule-list (&rest l)
  ;; rule-list $B$rLZ$N7A$K%3%s%Q%$%k$9$k!#(B
  (let ((tree (skk-make-rule-tree nil "" nil nil nil))
	rule ll )
    (while l
      (setq ll (car l)
	    l (cdr l) )
      (while ll
	(setq rule (car ll)
	      ll (cdr ll) )
	(skk-add-rule tree rule) ))
    tree ))

(defun skk-insert-str (str)
  ;; STR $B$rA^F~$9$k!#I,MW$G$"$l$P(B self-insert-after-hook $B$r%3(B
  ;; $B!<%k$9$k!#(Boverwrite-mode $B$G$"$l$P!"E,@Z$K>e=q$-$r9T$&!#(B
  (insert-and-inherit str)
  (if (and skk-henkan-on (not skk-henkan-active))
      (and skk-auto-start-henkan (not skk-okurigana) (skk-auto-start-henkan str))
    (and (boundp 'self-insert-after-hook) self-insert-after-hook
	 (funcall self-insert-after-hook (- (point) (length str)) (point)) )
    (and overwrite-mode
	 (skk-del-char-with-pad (skk-ovwrt-len (string-width str))) )))

(defun skk-ovwrt-len (len)
  ;; $B>e=q$-$7$FNI$$D9$5$rJV$9!#(B
  (min (string-width
	(buffer-substring-no-properties
	 (point) (skk-save-point (end-of-line) (point)) ))
       len ))

(defun skk-del-char-with-pad (length)
  ;; $BD9$5(B LENGTH $B$NJ8;z$r>C5n$9$k!#D4@0$N$?$a!"I,MW$G$"$l$P!"KvHx$K%9%Z!<%9$r(B
  ;; $BA^F~$9$k!#(B
  (let ((p (point)) (len 0))
    (while (< len length)
      (forward-char 1)
      (setq len (string-width (buffer-substring-no-properties (point) p))) )
    (delete-region p (point))
    (or (= length len)
        (progn
	  (insert-and-inherit " ")
          (backward-char 1) ))))

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
		  (1+ skk-self-insert-non-undo-count) )))
    (setq skk-self-insert-non-undo-count 1) ))

(defun skk-translate-okuri-char (okurigana)
  (and skk-okuri-char-alist
       (cdr (assoc (skk-substring-head-character okurigana) skk-okuri-char-alist)) ))

(defun skk-set-okurigana ()
  ;; $B8+=P$78l$+$i(B skk-henkan-okurigana, skk-henkan-key $B$N3FCM$r%;%C%H$9$k!#(B
  (cancel-undo-boundary)
  (and skk-katakana (skk-hiragana-region skk-henkan-start-point (point)))
  (skk-set-marker skk-henkan-end-point skk-okurigana-start-point)
  ;; just in case
  (skk-save-point
    (goto-char skk-okurigana-start-point)
    (or (eq (following-char) ?*) (insert-and-inherit "*")) )
  (setq skk-henkan-okurigana (buffer-substring-no-properties
                              (1+ skk-okurigana-start-point)
                              (point) ))
  (setq skk-henkan-key (concat (buffer-substring-no-properties
				skk-henkan-start-point
				skk-henkan-end-point )
			       (or (skk-translate-okuri-char
				    skk-henkan-okurigana)
				   skk-okuri-char ))
        skk-prefix "" )
  (delete-region skk-okurigana-start-point (1+ skk-okurigana-start-point))
  (setq skk-henkan-count 0)
  (skk-henkan)
  (setq skk-okurigana nil) )

;;;; other inputting functions

(defun skk-toggle-kutouten ()
  "$B6gFIE@$N<oN`$r%H%0%k$GJQ99$9$k!#(B"
  (interactive)
  (setq skk-kutouten-type (if (eq skk-kutouten-type 'jp) 'en 'jp))
  (and (interactive-p)
       (skk-message "$B6gE@(B: `%s'  $BFIE@(B: `%s'"
		    "Kuten: `%s'  Touten: `%s'"
		    (skk-current-kuten nil) (skk-current-touten nil) )))

(defun skk-current-kuten (arg)
  ;; just ignore arg.
  (car (cdr (assq skk-kutouten-type skk-kuten-touten-alist))) )

(defun skk-current-touten (arg)
  ;; just ignore arg.
  (cdr (cdr (assq skk-kutouten-type skk-kuten-touten-alist))) )

(defun skk-abbrev-period (arg)
  "SKK abbrev $B%b!<%I$G8+=P$7$NJd40$r9T$C$F$$$k:GCf$G$"$l$P!"<!$N8uJd$rI=<($9$k!#(B
$BJd40$ND>8e$G$J$1$l$P!"%*%j%8%J%k$N%-!<3d$jIU$1$N%3%^%s%I$r%(%_%e%l!<%H$9$k!#(B
SKK abbrev $B%b!<%I0J30$G$O!"(Bskk-insert-period $B4X?t$r;HMQ$9$k$3$H!#(B"
  (interactive "*P")
  (skk-with-point-move
   (if (eq last-command 'skk-completion)
       (progn
	 (setq this-command 'skk-completion)
	 (skk-completion nil) )
     (skk-emulate-original-map arg) )))

(defun skk-abbrev-comma (arg)
  "SKK abbrev $B%b!<%I$G8+=P$7$NJd40$r9T$C$F$$$k:GCf$G$"$l$P!"D>A0$N8uJd$rI=<($9$k!#(B
$BJd40$ND>8e$G$J$1$l$P!"%*%j%8%J%k$N%-!<3d$jIU$1$N%3%^%s%I$r%(%_%e%l!<%H$9$k!#(B
SKK abbrev $B%b!<%I0J30$G$O!"(Bskk-insert-comma $B4X?t$r;HMQ$9$k$3$H!#(B"
  (interactive "*P")
  (skk-with-point-move
   (if (eq last-command 'skk-completion)
       (skk-previous-completion)
     (skk-emulate-original-map arg) )))

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
		(cdr (assoc str skk-auto-paren-string-alist)) ))
	  (pair-str-inserted 0) )
     (if (not str)
	 (skk-emulate-original-map arg)
       (skk-cancel-undo-boundary)
       (while (> arg 0)
	 (skk-insert-str str)
	 (setq arg (1- arg)) )
       (if (not pair-str)
	   nil
	 (while (> arg2 0)
	   (if (not (string= pair-str (char-to-string (following-char))))
	       (progn
		 (setq pair-str-inserted (1+ pair-str-inserted))
		 (skk-insert-str pair-str) ))
	   (setq arg2 (1- arg2)) )
	 (or (= pair-str-inserted 0) (backward-char pair-str-inserted)) )))))

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
		     (= skk-henkan-end-point (point)) )
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
		    (delete-char count arg) )
		(skk-emulate-original-map arg) )
	      ;; XXX assume skk-prefix has no multibyte chars.
	      (if (> (length skk-prefix) count)
		  (setq skk-prefix (substring skk-prefix 0 (- (length skk-prefix) count)))
		(setq skk-prefix "") )
	      (and (>= skk-henkan-end-point (point)) (skk-kakutei)) ))
	   ((and skk-henkan-on (>= skk-henkan-start-point (point)))
	    (setq skk-henkan-count 0)
	    (skk-kakutei) )
	   ;; $BF~NOCf$N8+=P$78l$KBP$7$F$O(B delete-backward-char $B$GI,$:A43QJ8;z(B 1
	   ;; $BJ8;zJ,(B backward $BJ}8~$KLa$C$?J}$,NI$$!#(B
	   ((and skk-henkan-on overwrite-mode)
	    (backward-char count)
	    (delete-char count arg) )
	   (t 
	    (skk-delete-okuri-mark)
	    (if (skk-get-prefix skk-current-rule-tree)
		(skk-erase-prefix 'clean)
	      (skk-emulate-original-map arg) ))))))

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
            (setq skk-current-search-prog-list skk-search-prog-list) ))
      ;; skk-henkan-1 $B$NCf$+$i%3!<%k$5$l$k(B skk-henkan-show-candidate $B$+$i(B throw
      ;; $B$5$l$k!#$3$3$G%-%c%C%A$7$?>l9g$O!"(B?x $B$,%9%H%j!<%`$KLa$5$l$F$$$k$N$G!"(B
      ;; $B$3$N4X?t$r=P$F!"(Bskk-previous-candidates $B$X$f$/!#(B
      (catch 'unread
        (setq new-word (or (skk-henkan-1) (skk-henkan-in-minibuff))
              kakutei-henkan skk-kakutei-flag )
        (and new-word (skk-insert-new-word new-word)) )
      (if mark
          (progn
            (goto-char mark)
            ;; $B;2>H$5$l$F$$$J$$%^!<%+!<$O!"(BGarbage Collection $B$,%3!<%k$5$l$?$H(B
            ;; $B$-$K2s<}$5$l$k$,!"$=$l$^$G$N4V!"%F%-%9%H$N$I$3$+$r;X$7$F$$$k$H!"(B
            ;; $B%F%-%9%H$N%"%C%W%G!<%H$N:]$K$=$N%^!<%+!<CM$r99?7$9$kI,MW$,$"$k(B
            ;; $B$N$G!"$I$3$b;X$5$J$$$h$&$K$9$k!#(B
            (skk-set-marker mark nil)
	    (backward-char 1) )
        (goto-char (point-max)) )
      (and kakutei-henkan
	   (skk-kakutei (if (skk-numeric-p)
			    (skk-get-current-candidate-simply 'noconv)
			  new-word ))))))

(defun skk-henkan-1 ()
  ;; skk-henkan $B$N%5%V%k!<%A%s!#(B
  (let (new-word)
    (if (= skk-henkan-count 0)
        (progn
          (and (eq last-command 'skk-undo-kakutei-henkan)
	       (eq (car (car skk-current-search-prog-list))
		   'skk-search-kakutei-jisyo-file )
	       ;; in this case, we should not search kakutei jisyo.
	       (setq skk-current-search-prog-list
		     (cdr skk-current-search-prog-list) ))
          (setq skk-henkan-list (skk-search))
          (if (null skk-henkan-list)
              nil
            (setq new-word (skk-get-current-candidate))
            (and skk-kakutei-flag
		 ;; found the unique candidate in kakutei jisyo
		 (setq this-command 'skk-kakutei-henkan) )))
      ;; $BJQ492s?t$,(B 1 $B0J>e$N$H$-!#(B
      (setq new-word (skk-get-current-candidate))
      (or new-word
          ;; $B?7$7$$8uJd$r8+$D$1$k$+!"(Bskk-current-search-prog-list $B$,6u$K$J(B
          ;; $B$k$^$G(B skk-search $B$rO"B3$7$F%3!<%k$9$k!#(B
          (while (and skk-current-search-prog-list (not new-word))
            (setq skk-henkan-list (skk-nunion skk-henkan-list (skk-search))
                  new-word (skk-get-current-candidate) )))
      (and new-word (> skk-henkan-count 3)
	   ;; show candidates in minibuffer
	   (setq new-word (skk-henkan-show-candidates) )))
    new-word ))

(defun skk-get-current-candidate ()
  (if (skk-numeric-p)
      (let (val)
        (skk-num-uniq)
        (setq val (skk-num-convert (skk-get-current-candidate-simply)))
        (if (not skk-num-recompute-key)
            val
          (skk-num-uniq)
          (skk-num-convert (skk-get-current-candidate-simply)) ))
    (skk-get-current-candidate-simply) ))

(defun skk-henkan-show-candidates ()
  ;; $B%_%K%P%C%U%!$GJQ49$7$?8uJd72$rI=<($9$k!#(B
  (skk-save-point
   (let* ((candidate-keys ; $BI=<(MQ$N%-!<%j%9%H(B
           (mapcar
	    (function (lambda (c)
			(and (memq c '(?\C-g ?\040 ?x)) ; ?\040 is SPC.
			     (skk-error "`%s' $B$KL58z$J%-!<$,;XDj$5$l$F$$$^$9!#(B"
					"Illegal key in `%s'"
					"skk-henkan-show-candidates-keys" ))
			(char-to-string (upcase c)) ))
	    skk-henkan-show-candidates-keys ))
          key-num-alist ; $B8uJdA*BrMQ$NO"A[%j%9%H(B
          (key-num-alist1 ; key-num-alist $B$rAH$_N)$F$k$?$a$N:n6HMQO"A[%j%9%H!#(B
           (let ((count 6))
             (mapcar (function (lambda (key) (prog1 (cons key count)
                                               (setq count (1- count)) )))
                     ;; $B5U$5$^$K$7$F$*$$$F!"I=<($9$k8uJd$N?t$,>/$J$+$C$?$i@h(B
                     ;; $BF,$+$i4v$D$+:o$k!#(B
                     (reverse skk-henkan-show-candidates-keys) )))
          (loop 0)
          inhibit-quit
          henkan-list new-one str reverse n )
     ;; Emacs 19.28 $B$@$H(B Overlay $B$r>C$7$F$*$+$J$$$H!"<!$K(B insert $B$5$l$k(B
     ;; skk-henkan-key $B$K2?8N$+(B Overlay $B$,$+$+$C$F$7$^$&!#(B
     (and skk-use-face (skk-henkan-face-off))
     (delete-region skk-henkan-start-point skk-henkan-end-point)
     (while loop
       (if str
           (let (message-log-max)
             (message str) )
         (cond (reverse
                (setq loop (1- loop)
                      henkan-list (nthcdr (+ 4 (* loop 7)) skk-henkan-list)
                      reverse nil ))
               (skk-exit-show-candidates
                ;; $B8uJd$,?T$-$F$7$^$C$F!"(Bskk-henkan-show-candidates ->
                ;; skk-henkan-in-minibuff -> skk-henkan
                ;; -> skk-henkan-show-candidates $B$N=g$G!":F$S$3$N4X?t$,8F$P$l(B
                ;; $B$?$H$-$O!"$3$3$G(B henkan-list $B$H(B loop $B$r7W;;$9$k!#(B
                (setq henkan-list (nthcdr skk-henkan-count skk-henkan-list)
                      loop (car skk-exit-show-candidates)
                      skk-exit-show-candidates nil ))
               (t
                ;; skk-henkan-show-candidates-keys $B$N:G=*$N%-!<$KBP1~$9$k8uJd(B
                ;; $B$,=P$F$/$k$^$G%5!<%A$rB3$1$k!#(B
                (and (skk-numeric-p) (skk-num-uniq))
                (while (and skk-current-search-prog-list
                            (null (nthcdr (+ 11 (* loop 7)) skk-henkan-list)) )
                  (setq skk-henkan-list
                        (skk-nunion skk-henkan-list (skk-search)) )
                  (and (skk-numeric-p) (skk-num-uniq)) )
                (and (skk-numeric-p) (skk-num-convert*7))
                (setq henkan-list (nthcdr (+ 4 (* loop 7)) skk-henkan-list)) ))
         (setq n (skk-henkan-show-candidate-subr candidate-keys henkan-list)) )
       (if (> n 0)
           (condition-case nil
               (let* ((event (skk-read-event))
                      (char (event-to-character event))
                      num )
		 (if (eq skk-emacs-type 'xemacs)
		     (message "")) ; clear out candidates in echo area
                 (if (null char)
                     (skk-unread-event event)
                   (setq key-num-alist (nthcdr (- 7 n) key-num-alist1))
                   (and key-num-alist
			(setq num (cdr (or (assq char key-num-alist)
					   (if (skk-lower-case-p char)
					       (assq (upcase char) key-num-alist)
					     (assq (downcase char) key-num-alist) )))))
                   (cond (num
                          (setq new-one (nth num henkan-list)
                                skk-henkan-count (+ 4 (* loop 7) num)
                                skk-kakutei-flag t
                                loop nil
                                str nil ))
                         ((eq char ?\040) ; SPC
                          (if (or skk-current-search-prog-list
                                  (nthcdr 7 henkan-list) )
                              (setq loop (1+ loop)
                                    str nil )
                            ;; $B8uJd$,?T$-$?!#$3$N4X?t$+$iH4$1$k!#(B
                            (let ((last-showed-index (+ 4 (* loop 7))))
                              (setq skk-exit-show-candidates
                                    ;; cdr $BIt$O!"<-=qEPO?$KF~$kA0$K:G8e$KI=<($7(B
                                    ;; $B$?8uJd72$NCf$G:G=i$N8uJd$r;X$9%$%s%G%/%9(B
                                    (cons loop last-showed-index) )
                              ;; $B<-=qEPO?$KF~$k!#(Bskk-henkan-count $B$O(B
                              ;; skk-henkan-list $B$N:G8e$N8uJd$N<!(B ($BB8:_$7$J$$(B
                              ;; --- nil )$B$r;X$9!#(B
                              (setq skk-henkan-count (+ last-showed-index n)
                                    loop nil
                                    str nil ))))
                         ((eq char skk-previous-candidate-char)	; ?x
                          (if (= loop 0)
                              ;; skk-henkan-show-candidates $B$r8F$VA0$N>uBV$KLa(B
                              ;; $B$9!#(B
                              (progn
                                (setq skk-henkan-count 4)
                                (skk-unread-event (character-to-event
						   skk-previous-candidate-char))
                                ;; skk-henkan $B$^$G0l5$$K(B throw $B$9$k!#(B
                                (throw 'unread nil) )
                            ;; $B0l$DA0$N8uJd72$r%(%3!<%(%j%"$KI=<($9$k!#(B
                            (setq reverse t
                                  str nil )))
			 ;; $B$3$l$,$J$$$H(B quit $B$G$-$J$$!#2?8N!)(B
			 ((and (eq skk-emacs-type 'xemacs)
			       (eq char (quit-char)))
			  (signal 'quit nil))
                         (t (skk-message "\"%c\" $B$OM-8z$J%-!<$G$O$"$j$^$;$s!*(B"
                                         "\"%c\" is not valid here!"
                                         char )
                            (sit-for 1) ))))
             (quit
              ;; skk-previous-candidate $B$X(B
              (setq skk-henkan-count 0)
              (skk-unread-event (character-to-event skk-previous-candidate-char))
              ;; skk-henkan $B$^$G0l5$$K(B throw $B$9$k!#(B
              (throw 'unread nil) ))))  ; end of while loop
     (if (consp new-one)
         (cdr new-one)
       new-one ))))

(defun skk-henkan-show-candidate-subr (keys candidates)
  ;; key $B$H(B candidates $B$rAH$_9g$o$;$F(B 7 $B$D$N8uJd72(B ($B8uJd?t$,(B 7 $B$KK~$?$J$+$C(B
  ;; $B$?$i$=$3$GBG$A@Z$k(B) $B$NJ8;zNs$r:n$j!"%_%K%P%C%U%!$KI=<($9$k!#(B
  (let ((workinglst
	 ;; CANDIDATES $B$N@hF,$N(B 7 $B$D$N$_$N%j%9%H!#(B
	 (let ((count 0) e v)
	   (while (> 7 count)
	     (setq e (nth count candidates))
	     (if e
		 (setq v (cons e v)
		       count (1+ count) )
	       (setq count 7) ))
	   (nreverse v) ))
	(n 0) str cand message-log-max )
    (if (not (car workinglst))
        nil
      (setq workinglst (skk-truncate-message workinglst))
      (setq n 1
            ;; $B:G=i$N8uJd$NA0$K6uGr$r$/$C$D$1$J$$$h$&$K:G=i$N8uJd$@$1@h$K<h$j(B
            ;; $B=P$9!#(B
            str (concat (car keys) ":" (if (consp (car workinglst))
                                            (cdr (car workinglst))
                                          (car workinglst) )))
      ;; $B;D$j$N(B 6 $B$D$r<h$j=P$9!#8uJd$H8uJd$N4V$r6uGr$G$D$J$0!#(B
      (while (and (< n 7) (setq cand (nth n workinglst)))
        (setq cand (if (consp cand) (cdr cand) cand)
              str (concat str "  " (nth n keys) ":" cand)
              n (1+ n) ))
      (message "%s  [$B;D$j(B %d%s]"
               str (length (nthcdr n candidates))
               (make-string (length skk-current-search-prog-list) ?+) ))
    ;; $BI=<($9$k8uJd?t$rJV$9!#(B
    n ))

(defun skk-truncate-message (l)
  (let* (
	 ;; L $B$KF~$C$F$$$k$=$l$>$l$NMWAG(B ($B8uJd(B) $B$NJ8;zNs$NI}$N%j%9%H!#(B
	 (width-list
	  (mapcar
	   (function (lambda (e) (string-width (if (consp e) (cdr e) e))))
	   l ))
	 ;; $B8uJd?t!#(B
	 (candidates-num (length l))
	 ;; $B8uJd0J30$K%(%3!<%(%j%"$KI=<($5$l$kItIJ$NJ8;zNs$NI}!#(B
	 ;; (string-width "  [$B;D$j(B 100+]") -> 13
	 ;; ` F:'$B$J$I$N8uJd$NA*Br$N$?$a$KI=<($5$l$k(B width 3 $B$NJ8;zNs$,8uJd?tJ,$"$k!#(B
	 ;; $B%(%3!<%(%j%"$N:G=i$N8uJd$O6uGr$,A0$KIU$$$F$$$J$$$N$G(B 1-$B!#(B
	 (parts-len (+ 13 (1- (* 3 candidates-num))))
	 ;; $B$G!"%H!<%?%k$G$I$l$@$1$NI}$K$J$k$+!#(B
	 (message-width (apply '+ parts-len width-list))
	 (diff (- (window-width) message-width))
	 (count 0) (plus 0) max )
    (if (> diff 0)
	;; window-width $B$K<}$^$C$F$$$l$P2?$b$7$J$$!#(B
	l
      ;; $B$=$l$>$l$N8uJd$N:GBgI}$r2>7h$a$9$k!#(B
      (setq max (/ (float (- (window-width) parts-len)) candidates-num))
      (while width-list
	(if (> (car width-list) max)
	    (setq count (1+ count))
	  (setq plus (+ (- max (car width-list)) plus)) )
	(setq width-list (cdr width-list)) )
      ;; $B:GBgI}$KK~$?$J$$D9$5$r=8$a$F:GBgI}$r=$@5!#(B
      (setq max (truncate (/ (+ plus (- (window-width) parts-len))
			     candidates-num )))
      (mapcar
       (function
	(lambda (e)
	  ;; $B:GBgI}0J>e$NJ8;zNs$r(B
	  (cond ((and (stringp e) (> (string-width e) max))
		 ;; $B:GBgI}$K<}$^$k$h$&$KC;$+$/$9$k!#(B
		 (concat (truncate-string-to-width e (- max 3)) "...") )
		((and (consp e) (> (string-width (cdr e)) max))
		 (cons (car e) 
		       (concat (truncate-string-to-width (cdr e) (- max 3))
			       "..." )))
		(t e) )))
       l ))))

(defun skk-henkan-in-minibuff ()
  ;; $B%_%K%P%C%U%!$G<-=qEPO?$r$7!"EPO?$7$?%(%s%H%j$NJ8;zNs$rJV$9!#(B
  (save-match-data
    (let ((enable-recursive-minibuffers t)
          ;; $BJQ49Cf$K(B isearch message $B$,=P$J$$$h$&$K$9$k!#(B
          skk-isearch-message new-one )
      (add-hook 'minibuffer-setup-hook 'skk-j-mode-on)
      (add-hook
       'minibuffer-setup-hook
       (function (lambda ()
		   (add-hook 'pre-command-hook 'skk-pre-command nil 'local) )))
      (condition-case nil
          (setq new-one
                (read-from-minibuffer
                 (concat (or (and (skk-numeric-p) (skk-num-henkan-key))
                             (if skk-okuri-char
                                 (skk-compute-henkan-key2)
                               skk-henkan-key ))
                         " " )
		 (if (and (not skk-okuri-char)
			  skk-read-from-minibuffer-function )
		     (funcall skk-read-from-minibuffer-function) )))
        (quit
         (setq new-one "") ))
      (if (string= new-one "")
          (if skk-exit-show-candidates
              ;; $B%_%K%P%C%U%!$KI=<($7$?8uJd$,?T$-$F<-=qEPO?$KF~$C$?$,!"6uJ8;z(B
              ;; $BNs$,EPO?$5$l$?>l9g!#:G8e$K%_%K%P%C%U%!$KI=<($7$?8uJd72$r:FI=(B
              ;; $B<($9$k!#(B
              (progn
                (setq skk-henkan-count (cdr skk-exit-show-candidates))
                (skk-henkan) )
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
                        skk-okuri-char nil )
                  (skk-change-marker-to-white) )
              ;; skk-henkan-count $B$,(B -1 $B$G$J$1$l$P!"%+%l%s%H%P%C%U%!$G$O:G8e$N(B
              ;; $B8uJd$rI=<($7$?$^$^$J$N$G(B ($BI=<(4XO"$G$O2?$b$7$J$/$F$b!"$b$&4{(B
              ;; $B$KK>$_$N>uBV$K$J$C$F$$$k(B) $B2?$b$7$J$$!#(B
              ))
        ;; $B%_%K%P%C%U%!$GJQ49$7$?J8;zNs$,$"$k(B ($B6uJ8;zNs$G$J$$(B) $B$H$-!#(B
        ;; $BKvHx$N6uGr$r<h$j=|$/!#(B
        (and (string-match "[ $B!!(B]+$" new-one)
	     (setq new-one (substring new-one 0 (match-beginning 0))) )
        (if (skk-numeric-p)
            (setq new-one (skk-num-process-user-minibuf-input new-one))
          ;; $B$9$4$/$?$/$5$s$N8uJd$,$"$k>l9g$K!"$=$N:G8e$K?7$7$$8uJd$r2C$($k$N$O(B
          ;; $B$1$C$3$&9|$@$,!#(B
          (setq skk-henkan-list (nconc skk-henkan-list (list new-one))
                ;; $B%U%i%0$r%*%s$K$9$k!#(B
                skk-kakutei-flag t ))
        (setq skk-henkan-in-minibuff-flag t
              skk-touroku-count (1+ skk-touroku-count) ))
      ;; (nth skk-henkan-count skk-henkan-list) $B$,(B nil $B$@$+$i<-=qEPO?$K(B
      ;; $BF~$C$F$$$k!#(Bskk-henkan-count $B$r%$%s%/%j%a%s%H$9$kI,MW$O$J$$!#(B
      ;; (setq skk-henkan-count (1+ skk-henkan-count))
      ;; new-one $B$,6uJ8;zNs$@$C$?$i(B nil $B$rJV$9!#(B
      (if (not (string= new-one "")) new-one) )))

(defun skk-compute-henkan-key2 ()
  ;; skk-henkan-okurigana $B$,(B non-nil $B$J$i(B skk-henkan-key $B$+$i!"$+$D$F(B 
  ;; skk-henkan-key2 $B$H8F$P$l$F$$$?$b$N$r:n$k!#(B
  ;; skk-henkan-key2 $B$H$O!"!V4A;zItJ,$NFI$_(B + "*" + $BAw$j2>L>!W$N7A<0$NJ8;zNs$r(B
  ;; $B8@$&!#(B
  (if skk-henkan-okurigana
      (save-match-data
 	(string-match "[a-z]+$" skk-henkan-key)
 	(concat (substring skk-henkan-key 0 (match-beginning 0))
 		"*" skk-henkan-okurigana ))))

(defun skk-setup-minibuffer ()
  ;; $B%+%l%s%H%P%C%U%!$NF~NO%b!<%I$K=>$$%_%K%P%C%U%!$NF~NO%b!<%I$r@_Dj$9$k!#(B
  (cond ((eq skk-minibuffer-origin-mode 'hiragana)
	 (skk-j-mode-on) )
	((eq skk-minibuffer-origin-mode 'katakana)
	 (skk-j-mode-on t) )
	((eq skk-minibuffer-origin-mode 'abbrev)
	 (skk-abbrev-mode-on) )
	((eq skk-minibuffer-origin-mode 'latin)
	 (skk-latin-mode-on) )
	((eq skk-minibuffer-origin-mode 'jisx0208-latin)
	 (skk-jisx0208-latin-mode-on) )))

(defun skk-previous-candidate (&optional arg)
  "$B"'%b!<%I$G$"$l$P!"0l$DA0$N8uJd$rI=<($9$k!#(B
$B"'%b!<%I0J30$G$O%+%l%s%H%P%C%U%!$K(B \"x\" $B$rA^F~$9$k!#(B
$B3NDj<-=q$K$h$k3NDj$ND>8e$K8F$V$H3NDj$,%"%s%I%%$5$l$F!"3NDjA0$N>uBV$G(B
$BD>A0$N8+=P$78l$,%+%l%s%H%P%C%U%!$KA^F~$5$l$k!#(B"
  (interactive "p*")
  (skk-with-point-move
   (if (not skk-henkan-active)
       (if (not (eq last-command 'skk-kakutei-henkan))
	   (skk-kana-input arg)
	 ;; restore the state just before the last kakutei henkan.
	 (delete-region skk-henkan-start-point (point))
	 (skk-set-henkan-point-subr)
	 (insert-and-inherit (skk-get-last-henkan-data 'henkan-key))
	 (setq this-command 'skk-undo-kakutei-henkan) )
     (if (string= skk-henkan-key "")
	 nil
       (let ((mark
	      (if (not (eobp))
		  (skk-save-point (forward-char 1) (point-marker)) )))
	 (skk-save-point
	  (if (= skk-henkan-count 0)
	      (progn
		(and skk-okuri-char
		     ;; roman prefix for okurigana should be removed.
		     (setq skk-henkan-key (substring skk-henkan-key 0 -1)) )
		(setq skk-henkan-count -1
		      skk-henkan-in-minibuff-flag nil
		      skk-henkan-list nil
		      skk-henkan-okurigana nil
		      skk-okuri-char nil
		      skk-okuri-index-min -1
		      skk-okuri-index-max -1
		      skk-okurigana nil
		      skk-prefix "" )
		(and (skk-numeric-p) (skk-num-initialize))
		;; Emacs 19.28 $B$@$H(B Overlay $B$r>C$7$F$*$+$J$$$H!"<!$K(B insert $B$5$l(B
		;; $B$k(B skk-henkan-key $B$K2?8N$+(B Overlay $B$,$+$+$C$F$7$^$&!#(B
		(and skk-use-face (skk-henkan-face-off))
		(delete-region skk-henkan-start-point skk-henkan-end-point)
		(goto-char skk-henkan-end-point)
		(insert-and-inherit skk-henkan-key)
		(skk-change-marker-to-white) )
	    (setq skk-henkan-count (1- skk-henkan-count))
	    (skk-insert-new-word (skk-get-current-candidate-simply)) ))
	 (if mark
	     (progn
	       (goto-char mark)
	       (skk-set-marker mark nil)
	       (backward-char 1) )
	   (goto-char (point-max)) )
	 (and skk-abbrev-mode (= skk-henkan-count -1) (skk-abbrev-mode-on) ))))))

(defun skk-insert-new-word (word)
  ;; $B8+=P$78l$r>C$7!"$=$N>l=j$XJQ497k2L$NJ8;zNs$rA^F~$9$k!#(B
  (let (func)
    ;; Emacs 19.28 $B$@$H(B Overlay $B$r>C$7$F$*$+$J$$$H!"<!$K(B insert $B$5$l$k(B
    ;; skk-henkan-key $B$K2?8N$+(B Overlay $B$,$+$+$C$F$7$^$&!#(B
    (and skk-use-face (skk-henkan-face-off))
    (delete-region skk-henkan-start-point skk-henkan-end-point)
    (goto-char skk-henkan-start-point)
    ;; (^_^;) $B$N$h$&$J8+=P$78l$KBP$7!"(Bread-from-string $B$r8F$V$H%(%i!<$K$J$k$N(B
    ;; $B$G!"(Bcondition-case $B$G$=$N%(%i!<$rJa$^$($k!#(B
    (condition-case nil
	(setq func (car (read-from-string word)))
      (error (setq func word)))
    (condition-case nil
	(insert-and-inherit (if (and (listp func)
				     (functionp (car func)) )
				(eval func) word ))
      ;; $BJ8;zNs$rJV$5$J$$(B Lisp $B%W%m%0%i%`$rI>2A$7$F$b%(%i!<$K$J$i$J$$J}$,JXMx!)(B
      (error nil) )
    (skk-set-marker skk-henkan-end-point (point))
    (and skk-use-face (skk-henkan-face-on))
    (and skk-insert-new-word-function
	 (funcall skk-insert-new-word-function) )))

(defun skk-kakutei (&optional word)
  "$B8=:_I=<($5$l$F$$$k8l$G3NDj$7!"<-=q$N99?7$r9T$&!#(B
$B%*%W%7%g%J%k0z?t$N(B WORD $B$rEO$9$H!"8=:_I=<($5$l$F$$$k8uJd$H$OL54X78$K(B WORD $B$G3N(B
$BDj$9$k!#(B"
  ;; read only $B$G%(%i!<$K$J$k$h$&$K$9$k$H(B read only $B%P%C%U%!$G(B SKK $B$,5/F0$G$-(B
  ;; $B$J$/$J$k!#(B
  (interactive)
  (let ((inhibit-quit t)
	converted kakutei-word )
    (if skk-mode
	(skk-j-mode-on skk-katakana)
      ;; $B%+%l%s%H%P%C%U%!$G$^$@(B skk-mode $B$,%3!<%k$5$l$F$$$J$+$C$?$i!"%3!<%k$9(B
      ;; $B$k!#(B
      (skk-mode 1) )
    (if (not skk-henkan-on)
	nil
      (if (not skk-henkan-active)
	  nil
	(setq kakutei-word
	      ;; $B3NDj<-=q$N8l$G3NDj$7$?$H$-$O!"<-=q$K$=$N8l$r=q$-9~$`I,MW$b$J(B
	      ;; $B$$$7!"99?7$9$kI,MW$b$J$$$H;W$C$F$$$?$,!"Jd40$r9T$J$&$H$-$O!"(B
	      ;; $B8D?M<-=q$r;2>H$9$k(B ($B3NDj<-=q$O;2>H$7$J$$(B) $B$N$G!"B?>/;q8;$H;~(B
	      ;; $B4V$rL5BL$K$7$F$b!"8D?M<-=q$K3NDj<-=q$N%(%s%H%j$r=q$-9~$s$G99(B
	      ;; $B?7$b$7$F$*$/!#(B
	      (or word (skk-get-current-candidate-simply (skk-numeric-p))) )
	(if (or
	     (and (not skk-search-excluding-word-pattern-function) kakutei-word)
	     (and
	      kakutei-word skk-search-excluding-word-pattern-function
	      (not
	       (funcall skk-search-excluding-word-pattern-function kakutei-word) )))
	    (progn
	      (skk-update-jisyo kakutei-word)
	      (if (skk-numeric-p)
		  (progn
		    (setq converted (skk-get-current-candidate-simply))
		    (skk-num-update-jisyo kakutei-word converted) )))))
      (skk-kakutei-cleanup-buffer) )
    ;; KAKUTEI-WORD $B$J$I$N>pJs$,I,MW$G$"$l$P!"(Bskk-last-henkan-data $B$+$iF@$i$l(B
    ;; $B$k!#I,MW$J%G!<%?$,$=$l$i$NJQ?t$K8BDj$5$l$J$$$N$G!"0z?t$K$7$J$$!#(B
    (and skk-kakutei-end-function (funcall skk-kakutei-end-function))
    (skk-kakutei-initialize (if (skk-numeric-p) (cons kakutei-word converted)
			      kakutei-word ))
    (skk-do-auto-fill) ))

(defun skk-kakutei-cleanup-buffer ()
  ;; $B3NDjD>8e$N%P%C%U%!$N@07A$r9T$J$&!#(B
  (if skk-okurigana
      (progn
        (skk-delete-okuri-mark)
        (and skk-katakana skk-convert-okurigana-into-katakana
	     (skk-katakana-region skk-henkan-end-point (point)) )))
  (skk-delete-henkan-markers)
  (and (boundp 'self-insert-after-hook) self-insert-after-hook
       (funcall self-insert-after-hook skk-henkan-start-point (point)) )
  (and overwrite-mode
       (skk-del-char-with-pad
	(skk-ovwrt-len
	 (string-width
	  (buffer-substring-no-properties skk-henkan-start-point (point)) )))))

(defun skk-kakutei-initialize (&optional kakutei-word)
  ;; $B3NDj;~$KJQ?t$N=i4|2=$H%"%s%I%%$N$?$a$NJQ?t$NJ]B8$r9T$J$&!#(B
  (if (and kakutei-word (or (consp kakutei-word)
                            (not (string= kakutei-word "")) ))
      (progn
	(setq skk-kakutei-count (1+ skk-kakutei-count))
        ;; skk-undo-kakutei $B$N$?$a$K:G8e$NJQ49$N%G!<%?$rJ]B8$9$k!#(B
	(skk-put-last-henkan-data 'henkan-key skk-henkan-key)
	(skk-put-last-henkan-data 'okuri-char skk-okuri-char)
	(skk-put-last-henkan-data 'henkan-okurigana skk-henkan-okurigana)
	(skk-put-last-henkan-data
	 'henkan-list
	 ;; $B3NDj$7$?8l$r@hF,$K$9$k!#(B
	 (cons kakutei-word (delete kakutei-word skk-henkan-list)) )
	;; (eq last-command 'skk-kakutei-henkan) $B$G%]!<%?%V%k$K3NG'$G$-$k$N$G(B
	;; $B$"$($F$$$i$J$$$+!#(B
	;;(skk-put-last-henkan-data
	;; 'kakutei-henkan
	;; (eq this-command 'skk-kakutei-henkan) )
	;;
	;; $B>e5-0J30$N(B henkan data $B$r(B skk-last-henkan-data $B$K;D$7$?$+$C$?$i!"(B
	;; skk-kakutei-end-function $B$rMxMQ$9$k!#(B
	))
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
	)
  (and (skk-numeric-p) (skk-num-initialize))
  (and skk-use-look (setq skk-look-completion-words nil)) )

(defun skk-undo-kakutei ()
  "$B0lHV:G8e$N3NDj$r%"%s%I%%$7!"8+=P$7$KBP$9$k8uJd$rI=<($9$k!#(B
$B:G8e$K3NDj$7$?$H$-$N8uJd$O%9%-%C%W$5$l$k!#(B
$B8uJd$,B>$K$J$$$H$-$O!"%_%K%P%C%U%!$G$N<-=qEPO?$KF~$k!#(B"
  (interactive) 
  (skk-with-point-move
   (cond ((eq last-command 'skk-undo-kakutei)
	  (skk-error "$B3NDj%"%s%I%%$OO"B3;HMQ$G$-$^$;$s(B"
		     "Cannot undo kakutei repeatedly" ))
	 (skk-henkan-active
	  (skk-error "$B"'%b!<%I$G$O3NDj%"%s%I%%$G$-$^$;$s(B"
		     "Cannot undo kakutei in $B"'(B mode" ))
	 ( ; skk-henkan-key may be nil or "".
	  (or (not (skk-get-last-henkan-data 'henkan-key))
	      (string= (skk-get-last-henkan-data 'henkan-key) "") )
	  (skk-error "$B%"%s%I%%%G!<%?$,$"$j$^$;$s(B" "Lost undo data") ))
   (condition-case nil
       (let ((end
	      (if (skk-get-last-henkan-data 'henkan-okurigana)
		  (+ (length (skk-get-last-henkan-data 'henkan-okurigana))
		     skk-henkan-end-point )
		skk-henkan-end-point )))
	 (setq skk-henkan-active t
	       skk-henkan-on t
	       skk-current-search-prog-list
	       (if (eq (car (car skk-search-prog-list))
		       'skk-search-kakutei-jisyo-file )
		   ;; $B3NDj<-=q$OC5$7$F$bL50UL#!#(B
		   (cdr skk-search-prog-list)
		 skk-search-prog-list ))
	 ;; get henkan data back from skk-last-henkan-data.
	 (setq skk-henkan-key (skk-get-last-henkan-data 'henkan-key)
	       skk-henkan-list (skk-get-last-henkan-data 'henkan-list)
	       skk-henkan-okurigana (skk-get-last-henkan-data 'henkan-okurigana)
	       skk-okuri-char (skk-get-last-henkan-data 'okuri-char) )
	 (and skk-use-numeric-conversion
	      (setq skk-num-list (skk-get-last-henkan-data 'skk-num-list)) )
	 (and (>= (point-max) end)
	      ;; $B:G8e$NJQ49ItJ,$N%F%-%9%H$r>C$9!#Aw$j2>L>$rGD0.$7$F$$$k$N$J$i(B
	      ;; (skk-process-okuri-early $B$,(B non-nil $B$J$iAw$j2>L>$rGD0.$G$-$J$$(B)$B!"(B
	      ;; $BAw$j2>L>$r4^$a$?ItJ,$^$G$r>C$9!#(B
	      (delete-region skk-henkan-start-point end) )
	 (goto-char skk-henkan-start-point)
	 (insert-and-inherit "$B"'(B")
	 (skk-set-marker skk-henkan-start-point (point))
	 (if skk-okuri-char
	     (progn			; $BAw$j$"$j(B
	       (insert-and-inherit (substring skk-henkan-key 0
					      (1- (length skk-henkan-key)) ))
	       (skk-set-marker skk-henkan-end-point (point))
	       (and skk-henkan-okurigana (insert-and-inherit skk-henkan-okurigana)) )
	   (insert-and-inherit skk-henkan-key)
	   (skk-set-marker skk-henkan-end-point (point)) )
	 (skk-message "$B3NDj%"%s%I%%!*(B" "Undo kakutei!")
	 (setq skk-henkan-count 1)
	 (skk-henkan) )
     ;; skk-kakutei-undo $B$+$iESCf$GH4$1$?>l9g$O!"3F<o%U%i%0$r=i4|2=$7$F$*$+$J$$(B
     ;; $B$H<!$NF0:n$r$7$h$&$H$7$?$H$-$K%(%i!<$K$J$k!#(B
     (error (skk-kakutei))
     (quit (skk-kakutei)) )))
     
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
				  skk-henkan-start-point (point) )
		  skk-prefix "" )
	    (skk-henkan) )
	;; prepare for the processing of okurigana if not skk-okurigana
	;; and the preceding character is not a numeric character.
	;; if the previous char is a special midashi char or a
	;; numeric character, we assume that the user intended to type the
	;; last-command-char in lower case.
	(if (and (or (not (skk-get-prefix skk-current-rule-tree)) ; for KAnji, KanJIru
		     (and
		      (not (= skk-henkan-start-point skk-kana-start-point))
		      (or sokuon	; for TaSSi or TasSi
			  (skk-kana-cleanup)) )) ; for NEko
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
			       (<= (skk-char-octet p 1) 57) )  ; ?9
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
				       skk-kana-start-point )
				      (if skk-katakana "$B%C(B" "$B$C(B")
				      skk-henkan-okurigana ))
			(skk-erase-prefix)
			(insert-and-inherit (if skk-katakana "$B%C(B " "$B$C(B "))
			(setq skk-prefix ""
			      skk-henkan-count 0 )
			(skk-henkan)
			(delete-backward-char 2) )
		    (setq skk-henkan-key (concat
					  (buffer-substring-no-properties
					   skk-henkan-start-point
					   (point) )
					  skk-okuri-char ))
		    (insert-and-inherit " ")
		    (setq skk-prefix ""
			  skk-henkan-count 0 )
		    (skk-henkan)
		    (delete-backward-char 1) )
		  ;; we set skk-kana-start-point here, since the marker may no
		  ;; longer point at the correct position after skk-henkan.
		  (skk-set-marker skk-kana-start-point (point)) )
	      (if (= skk-henkan-start-point (point))
		  nil
		(if sokuon
		    (progn
		      (skk-erase-prefix 'clean)
		      (insert-and-inherit (if skk-katakana "$B%C(B" "$B$C(B")) ))
		(skk-set-marker skk-okurigana-start-point (point))
		(insert-and-inherit "*")
		(skk-set-marker skk-kana-start-point (point))
		(setq skk-okuri-char (char-to-string last-char)
		      skk-okurigana t ))))))
    (if normal
	(progn
	  (setq last-command-char last-char)
	  (skk-kana-input arg) ))))

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
	 (skk-henkan) )
     (save-match-data
       (let (pos)
	 (skk-kana-cleanup 'force)
	 (and (skk-get-prefix skk-current-rule-tree)
	      ;; Never.  `skk-erase-prefix' called by `skk-kana-cleanup'
	      ;; initializes `skk-prefix'.
	      (skk-error "$B%U%#%C%/%9$5$l$F$$$J$$(B skk-prefix $B$,$"$j$^$9(B"
			 "Have unfixed skk-prefix" ))
	 (setq pos (point))
	 (and (< pos skk-henkan-start-point)
	      (skk-error
	       "$B%+!<%=%k$,JQ493+;OCOE@$h$jA0$K$"$j$^$9(B"
	       "Henkan end point must be after henkan start point" ))
	 (and skk-katakana (= arg 1)
	      (skk-hiragana-region skk-henkan-start-point pos) )
	 (setq skk-henkan-key (buffer-substring-no-properties
			       skk-henkan-start-point pos ))
	 (and skk-okurigana (string-match "\\* *$" skk-henkan-key)
	      (skk-error
	       "$B6u$NAw$j2>L>$G4A;z$rEPO?$7$h$&$H$7$F$$$^$9(B"
	       "No okurigana!" ))
	 (if skk-allow-spaces-newlines-and-tabs
	     ;; skk-henkan-key $B$NCf$N(B "[ \n\t]+" $B$r40A4$K<h$j=|$/!#(B
	     (while (string-match "[ \n\t]+" skk-henkan-key)
	       (setq skk-henkan-key
		     (concat (substring skk-henkan-key 0 (match-beginning 0))
			     (substring skk-henkan-key (match-end 0)) )))
	   (skk-save-point
	    (beginning-of-line)
	    (and (> (point) skk-henkan-start-point)
		 (skk-error
		  "$BJQ49%-!<$K2~9T$,4^$^$l$F$$$^$9(B"
		  "Henkan key may not contain a new line character" )))
	   ;; $B:G=i$N%9%Z!<%9$G(B skk-henkan-key $B$r$A$g$s@Z$k$@$1!#(B
	   (setq skk-henkan-key (substring skk-henkan-key 0
					   (string-match " "
							 skk-henkan-key ))))
	 (skk-set-marker skk-henkan-end-point pos)
	 (setq skk-henkan-count 0)
	 (skk-henkan)
	 (if (and skk-abbrev-mode skk-henkan-active)
	     (progn
	       (skk-j-mode-on)
	       (setq skk-abbrev-mode t) )))))))

(defun skk-auto-start-henkan (str)
  ;; skk-auto-start-henkan-keyword-list $B$NMWAG$NJ8;zNs$rA^F~$7$?$H$-$K<+F0E*$K(B 
  ;; ($B%9%Z!<%9$rBG80$7$J$/$H$b(B) $BJQ49$r3+;O$9$k!#%(!<!_%$%=%U%H<R$N(B MSDOS $BMQ(B $B$N(B 
  ;; FEP$B!"(BWX2+ $BIw!#(B
  (and (member str skk-auto-start-henkan-keyword-list)
       (skk-save-point
        (backward-char 1)
        (and (> (point) skk-henkan-start-point)
	     (let ((skk-prefix ""))
	       (skk-start-henkan (prefix-numeric-value current-prefix-arg)) )))))

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
		 (setq arg (1- arg)) )))
	 ;; $B0z?t$J$7!#(B
	 (let ((limit
		(if (not skk-allow-spaces-newlines-and-tabs)
		    (skk-save-point (beginning-of-line) (point))
		  (point-min) ))
	       ;; $B!2!1!0!/!.!-!,!+!*!)!(!'!&!%!$!#(B
	       (unknown-chars-regexp
		(if skk-allow-spaces-newlines-and-tabs
		    "[ $B!!(B\n\t$B!<!7!6!5!4!3(B]"
		  "[$B!!!<!7!6!5!4!3(B]" ))
	       type p )
	   (save-match-data
	     (skk-save-point
	      (backward-char 1)
	      (while (and (> (point) limit)
			  ;; unknown-chars-regexp $B$G$OJ8;z<oJL$,H=JL$G$-$J$$$N(B
			  ;; $B$G!"$=$NJ8;zNs$,B3$/8B$j%]%$%s%H$r%P%C%U%!$N@hF,(B
			  ;; $BJ}8~$XLa$9!#(B
			  (looking-at unknown-chars-regexp) )
		(backward-char 1) )
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
		      ;;            (looking-at unknown-chars-regexp) )
		      ;;  (backward-char 1) )
		      (if;;(or
			  (> 0 (skk-backward-and-set-henkan-point-1 type))
			  ;;(eq (skk-what-char-type) type))
			  (setq p (point)) ))))))
	   (goto-char p)
	   (skip-chars-forward unknown-chars-regexp) ))
       (skk-set-henkan-point-subr) ))))

(defun skk-backward-and-set-henkan-point-1 (type)
  ;; skk-backward-and-set-henkan-point $B$N%5%V%k!<%A%s!#(BCHAR $B$N<oN`$K1~$8$?J8;z(B
  ;; $B$r%9%-%C%W$7$F%P%C%U%!$N@hF,J}8~$XLa$k!#(B
  (cond ((eq type 'hiragana)
         ;; "$B$r(B" $B$NA0$G;_$^$C$?J}$,JXMx!)(B
         (skip-chars-backward "$B!3!4!5!6!7!<$s$!(B-$B$q(B") )
        ((eq type 'katakana)
         ;; "$B%r(B" $B$NA0$G;_$^$C$?J}$,JXMx!)(B
         (skip-chars-backward "$B!3!4!5!6!7!<%s%!(B-$B%q(B") )
        ((eq type 'jisx0208-latin)
         (skip-chars-backward "$B!!(B-$B#z(B") )
        ((eq type 'ascii)
         (skip-chars-backward " -~") )))

(defun skk-what-char-type ()
  ;; $B8=:_$N%]%$%s%H$K$"$kJ8;z$,$I$s$J<oN`$+$rH=JL$9$k!#(B
  (save-match-data
    (cond ((looking-at "[$B$!(B-$B$s(B]") 'hiragana)
          ((looking-at "[$B%!(B-$B%s(B]") 'katakana)
          ;; "$B!<(B" $B$r=|30$7$F$$$k(B ("$B!<(B" $B$O(B "$B!;(B" $B$H(B "$B!=(B" $B$N4V$KF~$C$F$$$k(B)$B!#(B
          ((looking-at "[$B!!(B-$B!;!=(B-$B#z(B]") 'jisx0208-latin)
          ((looking-at "[ -~]") 'ascii)
          (t 'unknown) )))

(defun skk-set-henkan-point-subr (&optional arg)
  "$B$+$J$rF~NO$7$?8e$G!"%]%$%s%H$KJQ493+;O$N%^!<%/(B \($B"&(B\) $B$rIU$1$k!#(B
$B85!9$O$3$N4X?t$O(B skk-set-henkan-point $B$NFbIt4X?t$G$"$k!#(B"
  (interactive "*P")
  (skk-with-point-move
   (cancel-undo-boundary)
   (if skk-henkan-on (skk-kakutei)
     (skk-kana-cleanup) );; XXX
   (if (not (skk-get-prefix skk-current-rule-tree))
       (insert-and-inherit "$B"&(B")
     (skk-erase-prefix)
     (insert-and-inherit "$B"&(B")
     (skk-set-marker skk-kana-start-point (point))
     (skk-insert-prefix) )
   (setq skk-henkan-on t)
   (skk-set-marker skk-henkan-start-point (point)) ))

(defun skk-change-marker ()
  ;; "$B"&(B"$B$r(B"$B"'(B"$B$KJQ$($k!#(Bskk-henkan-active $B%U%i%0$r(B t $B$K$9$k!#(B
  (skk-save-point
   (goto-char (- skk-henkan-start-point skk-kanji-len))
   (if (looking-at "$B"&(B")
       (progn
	 (cancel-undo-boundary)
	 (let ((buffer-undo-list t))
	     (insert-and-inherit "$B"'(B")
	     (delete-char 1) )
	 (setq skk-henkan-active t) )
     (skk-kakutei)
     (skk-error "$B"&$,$"$j$^$;$s(B" "It seems that you have deleted $B"&(B") )))

(defun skk-change-marker-to-white ()
  ;; "$B"'(B"$B$r(B"$B"&(B"$B$KJQ$($k!#(Bskk-henkan-active $B%U%i%0$r(B nil $B$K$9$k!#(B
  (skk-save-point
   (goto-char (- skk-henkan-start-point skk-kanji-len))
   (cancel-undo-boundary)
   (if (looking-at "$B"'(B")
       (let ((buffer-undo-list t))
	 (insert-and-inherit "$B"&(B")
	 (delete-char 1) )
     (goto-char skk-henkan-start-point)
     (insert-and-inherit "$B"&(B")
     (skk-set-marker skk-henkan-start-point (point))
     (skk-message "$B"'$,$"$j$^$;$s(B" "It seems that you have deleted $B"'(B") )
   (setq skk-henkan-active nil) ))

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
				"It seems that you have deleted $B"'(B" ))))
	 (if (looking-at "$B"&(B")
	     (delete-char 1)
	   (or nomesg
	       (skk-message "$B"&$,$"$j$^$;$s(B"
			    "It seems that you have deleted $B"&(B" ))))))))

(defun skk-delete-okuri-mark ()
  ;; $BAw$j2>L>F~NOCf$K%+%l%s%H%P%C%U%!$KI=$o$l$k(B `*' $B%^!<%/$r>C$7!"Aw$j2>L>4XO"(B
  ;; $B%U%i%0$r(B nil $B$K%;%C%H$9$k!#(B
  (if (or (not skk-okurigana)
	  (not skk-okurigana-start-point)
	  (not (markerp skk-okurigana-start-point))
	  (not (marker-position skk-okurigana-start-point)) )
      nil
    (skk-save-point
      (and (eq (char-after skk-okurigana-start-point) ?*) ; ?*
	   (delete-region skk-okurigana-start-point
			  (1+ skk-okurigana-start-point) ))
      (setq skk-okurigana nil
            skk-okuri-char nil
            skk-henkan-okurigana nil ))))
            
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
			    "Really purge \"%s /%s/%s\"?" )
			  skk-henkan-key (skk-get-current-candidate-simply)
			  (if (and skk-henkan-okurigana
				   (or skk-henkan-okuri-strictly
				       skk-henkan-strict-okuri-precedence ))
			      (concat
			       (if skk-japanese-message-and-error
				   " ($BAw$j2>L>(B: "
				 "(okurigana: " )
			       skk-henkan-okurigana
			       ") " )
			    " " ))))
	   nil
	 ;; skk-henkan-start-point $B$+$i(B point $B$^$G:o=|$7$F$7$^$C$F$b!"JQ49D>8e(B
	 ;; $B$K(B ($B%+!<%=%k$rF0$+$9$3$H$J$/(B) skk-purge-from-jisyo $B$r8F$Y$PLdBj$J$$(B
	 ;; $B$,!"%+!<%=%k$,0c$&>l=j$X0\F0$7$F$$$?>l9g$O!":o=|$9$Y$-$G$J$$$b$N$^(B
	 ;; $B$G:o=|$7$F$7$^$&2DG=@-$,$"$k!#$=$3$G!"Aw$j2>L>$,$"$l$P$=$ND9$5$r4^(B
	 ;; $B$a$?(B end $B$r5a$a!":#2s$NJQ49$K4XO"$7$?8D=j$@$1$r@53N$K@Z$j<h$k$h$&$K(B
	 ;; $B$9$k!#(B
	 (let ((end (if skk-henkan-okurigana (+ (length skk-henkan-okurigana)
						skk-henkan-end-point )
		      skk-henkan-end-point ))
	       (word (skk-get-current-candidate-simply (skk-numeric-p))) )
	   (skk-update-jisyo word 'purge)
	   ;; Emacs 19.28 $B$@$H(B Overlay $B$r>C$7$F$*$+$J$$$H!"<!$K(B insert $B$5$l$k(B
	   ;; skk-henkan-key $B$K2?8N$+(B Overlay $B$,$+$+$C$F$7$^$&!#(B
	   (and skk-use-face (skk-henkan-face-off))
	   (delete-region skk-henkan-start-point end)
	   (skk-change-marker-to-white)
	   (skk-kakutei) )))))

(defun skk-save-jisyo (&optional quiet)
  "SKK $B$N<-=q%P%C%U%!$r%;!<%V$9$k!#(B
  $B%*%W%7%g%J%k0z?t$N(B QUIET $B$,(B non-nil $B$G$"$l$P!"<-=q%;!<%V;~$N%a%C%;!<%8$r=P$5$J(B
  $B$$!#(B"
  (interactive "P")
  (funcall skk-save-jisyo-function quiet) )

(defun skk-save-jisyo-original (&optional quiet)
  ;;"SKK $B$N<-=q%P%C%U%!$r%;!<%V$9$k!#(B
  ;;$B%*%W%7%g%J%k0z?t$N(B QUIET $B$,(B non-nil $B$G$"$l$P!"<-=q%;!<%V;~$N%a%C%;!<%8$r=P$5$J(B
  ;;$B$$!#(B"
  (let* ((skk-jisyo (expand-file-name skk-jisyo))
         (jisyo-buffer (skk-get-jisyo-buffer skk-jisyo 'nomsg)))
    (if (or (not jisyo-buffer) (not (buffer-modified-p jisyo-buffer)))
        (if (not quiet) 
            (progn
	      (skk-message "SKK $B<-=q$rJ]B8$9$kI,MW$O$"$j$^$;$s(B"
                           "No need to save SKK jisyo" )
              (sit-for 1) ))
      (with-current-buffer jisyo-buffer
        (let ((inhibit-quit t)
              (tempo-file (skk-make-temp-jisyo)) )
          (if (not quiet)
              (skk-message "SKK $B<-=q$rJ]B8$7$F$$$^$9(B..."
                           "Saving SKK jisyo..." ))
          (skk-save-jisyo-1 tempo-file)
          (skk-check-size-and-do-save-jisyo tempo-file)
          ;; $B<-=q$N%;!<%V$K@.8y$7$F=i$a$F(B modified $B%U%i%C%0$r(B nil $B$K$9$k!#(B
          (set-buffer-modified-p nil)
	  (setq skk-update-jisyo-count 0)
          (if (not quiet)
              (progn
                (skk-message "SKK $B<-=q$rJ]B8$7$F$$$^$9(B...$B40N;!*(B"
                             "Saving SKK jisyo...done" )
                (sit-for 1) ))
          (and (eq this-command 'save-buffers-kill-emacs)
	       (skk-record-jisyo-data) ))))))

(defun skk-save-jisyo-1 (file)
  (save-match-data
    (let (buffer-read-only)
      (goto-char (point-min))
      (if (re-search-forward "^;; okuri-ari entries.$" nil 'noerror)
          nil
        (skk-error
         "$BAw$j$"$j%(%s%H%j$N%X%C%@!<$,$"$j$^$;$s!*(B SKK $B<-=q$N%;!<%V$rCf;_$7$^$9(B"
         "Header line for okuri-ari entries is missing!  Stop saving SKK jisyo" ))
      ;; $B$*$C!"%3%a%s%H%U%'%$%9$,(B $ $B$G=*$o$i$J$$$>(B > hilit19.el
      (if (re-search-forward "^;; okuri-nasi entries.$" nil 'noerror)
          nil
        (skk-error
         "$BAw$j$J$7%(%s%H%j$N%X%C%@!<$,$"$j$^$;$s(B $B!*(B SKK $B<-=q$N%;!<%V$rCf;_$7$^$9(B"
         "Header line for okuri-nasi entries is missing!  Stop saving SKK jisyo" )))
    (write-region-as-coding-system
     (cond ((and skk-jisyo-code
		 (or (coding-system-p skk-jisyo-code)
		     (and (fboundp 'find-coding-system)
			  (find-coding-system skk-jisyo-code) )))
	    skk-jisyo-code )
	   ((and skk-jisyo-code (stringp skk-jisyo-code))
	    (cdr (assoc skk-jisyo-code skk-coding-system-alist)) )
	   (t (cdr (assoc "euc" skk-coding-system-alist))) )
     1 (point-max) file nil 'nomsg )))

(defun skk-check-size-and-do-save-jisyo (new-file)
  (let ((new-size (nth 7 (file-attributes new-file)))
        old-size )
    (if (= new-size 0)
        (progn
          (delete-file new-file)
          (skk-error "SKK $B<-=q$,6u$K$J$C$F$$$^$9!*(B $B<-=q$N%;!<%V$rCf;_$7$^$9(B"
                     "Null SKK jisyo!  Stop saving jisyo" )))
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
                  (>= new-size old-size) )))
        (skk-make-new-jisyo new-file)
      ;; yes-or-no-p $B$K2sEz$7!"(Bnewline $B$9$k$H!"(Bthis-command $B$,JQ$C$F$7$^$&!#(B
      (let (this-command this-command-char last-command last-command-char)
        (if (skk-yes-or-no-p
             (format
              "skk-jisyo $B$,(B %dbytes $B>.$5$/$J$j$^$9$,!"%;!<%V$7$FNI$$$G$9$+!)(B"
              (- old-size new-size) )
             (format
              "New %s will be %dbytes smaller.  Save anyway?"
              skk-jisyo (- old-size new-size) ))
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
                  (princ "$B$r<B9T$7$F2<$5$$!#(B") )
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
              (terpri) ))
          (skk-error "SKK $B<-=q$N%;!<%V$rCf;_$7$^$7$?!*(B"
                     "Stop saving SKK jisyo!" ))))))

(defun skk-make-temp-jisyo ()
  ;; SKK $B8D?M<-=qJ]B8$N$?$a$N:n6HMQ$N%U%!%$%k$r:n$j!"%U%!%$%k$N%b!<%I$r(B
  ;; skk-jisyo $B$N$b$N$HF1$8$K@_Dj$9$k!#:n$C$?:n6HMQ%U%!%$%k$NL>A0$rJV$9!#(B
  (let ((tempo-name (skk-make-temp-file "skkdic")))
    (skk-create-file tempo-name)
    ;; temporary file $B$K(B remote file $B$r;XDj$9$k$3$H$J$IM-$jF@$J$$!)(B
    ;;(if (or 
    ;;     ;; XEmacs has efs.el
    ;;     (eq skk-emacs-type 'xemacs)
    ;;     ;; ange-ftp.el does not have a wrapper to set-file-modes.
    ;;     (not (and (featurep 'ange-ftp) (boundp 'ange-ftp-name-format)
    ;;               (string-match (car ange-ftp-name-format) tempo-name) )))
    (set-file-modes tempo-name  (file-modes skk-jisyo))
    ;;)
    tempo-name ))

(defun skk-make-temp-file (prefix)
  (let ((dir
	 (cond ((skk-file-exists-and-writable-p temporary-file-directory)
		(expand-file-name temporary-file-directory) )
	       ((and (memq system-type '(ms-dos windows-nt))
		     (skk-file-exists-and-writable-p "a:/temp") )
		;; NEC PC-9800 series.
		"a:/temp" )
	       (t (or (file-exists-p "~/tmp") (make-directory "~/tmp"))
		  (or (file-writable-p "~/tmp") (set-file-modes "~/tmp" 1023))
		  "~/tmp" ))))
    (make-temp-name
     (concat dir
	     (if (memq (skk-str-ref dir (1- (length dir)) ) '(?/ ?\\))
		 "" "/" )
	     prefix ))))

(defun skk-make-new-jisyo (tempo-file)
  ;; TEMPO-FILE $B$r?75,$N(B skk-jisyo $B$K$9$k!#(Bskk-backup-jisyo $B$,(B non-nil $B$@$C$?(B
  ;; $B$i%P%C%/%"%C%W<-=q$r:n$k!#(B
  (if skk-backup-jisyo
      (progn
        (if (file-exists-p skk-backup-jisyo)
            (delete-file skk-backup-jisyo) )
        (rename-file skk-jisyo skk-backup-jisyo) )
    (delete-file skk-jisyo) )
  (rename-file tempo-file skk-jisyo 'ok-if-already-exists) )

(defun skk-reread-private-jisyo (&optional force)
  "$B%P%C%U%!$KFI$_9~$s$@8D?M<-=q$rGK4~$7!"%U%!%$%k$+$i%P%C%U%!$X:FFI$_9~$_$9$k!#(B
$B%*%W%7%g%J%k0z?t$N(B FORCE $B$,(B non-nil $B$G$"$l$P!"GK4~$N3NG'$r$7$J$$!#(B"
  (interactive "P")
  (let ((buf (skk-get-jisyo-buffer skk-jisyo 'nomsg)))
    (if (and buf
             (or force
                 (skk-yes-or-no-p "$BJT=8Cf$N8D?M<-=q$rGK4~$7$^$9$+!)(B"
                                  "Discard your editing private JISYO?" )))
        (progn
          (with-current-buffer buf
            (set-buffer-modified-p nil)
            (kill-buffer buf) )
          (or
           (skk-get-jisyo-buffer skk-jisyo 'nomsg)
           (skk-error "$B8D?M<-=q$r:FFI$_9~$_$9$k$3$H$,$G$-$^$;$s!*(B"
                      "Cannot reread private JISYO!" ))))))

(defun skk-record-jisyo-data ()
  ;; $B<-=q%G!<%?$r<h$j!"(BEmacs $B$N=*N;$N:]$G$"$l$P!"$=$N%G!<%?$r(B 
  ;; skk-record-file $B$KJ]B8$7!"$=$l0J30$G$"$l$P!"$=$l$r%(%3!<$9$k!#(B
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
           skk-kakutei-count )
        (cond ((featurep 'skk-rdbms)
	       ;; RDBMS $B$r;H$($P$b$C$H6=L#?<$$E}7W$,<h$l$k$+$b$7$l$J$$(B
	       ;; $B$,!"$H$j$"$($:8l?t$@$1?t$($FF~$l$F$*$/!#(B
	       (skk-rdbms-count-jisyo-candidates skk-rdbms-private-jisyo-table) )
	      (skk-count-private-jisyo-candidates-exactly
	       (skk-count-jisyo-candidates (expand-file-name skk-jisyo)) )
	       ;; 1 $B9T(B 1 $B8uJd$H$_$J$9!#(B
	      (t (with-current-buffer (skk-get-jisyo-buffer skk-jisyo 'nomsg)
		   (- (count-lines (point-min) (point-max)) 2) ))))))
    (setq skk-touroku-count 0 skk-kakutei-count 0) ))

(defun skk-count-jisyo-candidates (file-or-table)
  "SKK $B<-=q$N8uJd?t$r?t$($k!#(B"
  (interactive
   (list (cond ((eq skk-count-jisyo-candidates-function
		    'skk-count-jisyo-candidates-original )
		(read-file-name
		 (format "Jisyo file: (default: %s) " skk-jisyo)
		 "~/" skk-jisyo 'confirm ))
	       ((eq skk-count-jisyo-candidates-function
		    'skk-rdbms-count-jisyo-candidates )
		;; $B%G!<%?%Y!<%9%U%!%$%k$rD>@\%U%!%$%kL>$G;XDj$G$-$k(B
		;; permission $B$,$J$$>l9g$,B?$$$h$M(B...$B!#(B
		;;(read-file-name
		;; (format "Jisyo table: (default: %s) "
		;;	 skk-rdbms-private-jisyo-table ))
		skk-rdbms-private-jisyo-table ))))
  ;; mule@emacs19.31 $B$@$H2<5-$N$h$&$K$9$k$H(B (`$B%!(B' $B$,860x$N$h$&(B) $B2?8N$+(B 
  ;; default-directory $B$NKvHx$K2~9T$,IU$/!#(B
  ;; $BDL>o$O5$$,IU$+$J$$$,!"(Brsz-mini.el $B$r;H$C$F(B resize-minibuffer-mode $B$r(B 
  ;; non-nil $B$K$7$F$$$k$HITMW$J(B 2 $B9TL\$,=P8=$9$k!#(B
  ;; (interactive "f$B<-=q%U%!%$%k(B: ")
  (let ((count (funcall skk-count-jisyo-candidates-function file-or-table)))
    (if (interactive-p)
	(message "%d entries" count)
      count )))

(defun skk-count-jisyo-candidates-original (file)
  ;;"SKK $B<-=q$N8uJd?t$r?t$($k!#(B
  ;;`[' $B$H(B `]' $B$K0O$^$l$?Aw$j2>L>Kh$N%V%m%C%/Fb$O?t$($J$$!#(B"
  (with-current-buffer (find-file-noselect file)
    (save-match-data
      (let ((count 0)
            (min (point-min))
            (max (and (interactive-p) (point-max)))
            (interactive-p (interactive-p)) )
        (goto-char min)
        (if (or
             ;; $B$3$A$i$O(B skk-save-point $B$r;H$o$:!"%]%$%s%H$r0\F0$5$;$k!#(B
             (not (re-search-forward "^;; okuri-ari entries.$" nil t nil))
             (not
              (skk-save-point
                (re-search-forward "^;; okuri-nasi entries.$" nil t nil) )))
            (skk-error "$B$3$N%U%!%$%k$O(B SKK $B<-=q$G$O$"$j$^$;$s(B"
                       "This file is not a SKK dictionary") )
        (while (search-forward "/" nil t)
          (cond ((looking-at "\\[")
                 (forward-line 1)
                 (beginning-of-line) )
                ((not (eolp))
                 (setq count (1+ count)) ))
          (if interactive-p
              (message "Counting jisyo candidates...%3d%% done"
                       (/ (* 100 (- (point) min)) max) )))
	count ))))

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
 			     japanese english ))
		(sit-for 3) ))))))

(defun skk-get-jisyo-buffer (file &optional nomsg)
  ;; FILE $B$r3+$$$F(B SKK $B<-=q%P%C%U%!$r:n$j!"%P%C%U%!$rJV$9!#(B
  ;; $B%*%W%7%g%J%k0z?t$N(B NOMSG $B$r;XDj$9$k$H%U%!%$%kFI$_9~$_$N:]$N%a%C%;!<%8$r(B
  ;; $BI=<($7$J$$!#(B
  (if file
      (let ((inhibit-quit t)
            (jisyo-buf (concat " *" (file-name-nondirectory file)
                               "*" )))
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
              ;;      line-number-mode nil )
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
                    mode-name "SKK dic" )
              (or nomsg
                  (skk-message "SKK $B<-=q(B %s $B$r%P%C%U%!$KFI$_9~$s$G$$$^$9(B..."
                               "Inserting contents of %s ..."
                               (file-name-nondirectory file) ))
	      (let (enable-character-translation enable-character-unification)
		(insert-file-contents-as-coding-system
		 (cond ((and skk-jisyo-code
			     (or (coding-system-p skk-jisyo-code)
				 (and (fboundp 'find-coding-system)
				      (find-coding-system skk-jisyo-code) )))
			skk-jisyo-code )
		       ((and skk-jisyo-code (stringp skk-jisyo-code))
			(cdr (assoc skk-jisyo-code skk-coding-system-alist)) )
		       (t (cdr (assoc "euc" skk-coding-system-alist))) )
		 file ))
              (or nomsg
                  (skk-message
                   "SKK $B<-=q(B %s $B$r%P%C%U%!$KFI$_9~$s$G$$$^$9(B...$B40N;!*(B"
                   "Inserting contents of %s ...done"
                   (file-name-nondirectory file) ))
              (skk-setup-jisyo-buffer)
              (set-buffer-modified-p nil)
              jisyo-buf )))))

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
	(insert ";; okuri-ari entries.\n" ";; okuri-nasi entries.\n") )
    (goto-char (point-min))
    (if (re-search-forward "^;; okuri-ari entries.$" nil 'noerror)
	;; $B8GDj%]%$%s%H$J$N$G!"(B(point) $B$G==J,!#(B
	(setq skk-okuri-ari-min (point))
      (skk-error "$BAw$j$"$j%(%s%H%j$N%X%C%@!<$,$"$j$^$;$s!*(B"
		 "Header line for okuri-ari entries is missing!" ))
    (if (re-search-forward "^;; okuri-nasi entries.$" nil 'noerror)
	(progn
	  (beginning-of-line)
	  ;; $B6&M-<-=q$J$i8GDj%]%$%s%H$G$bNI$$$N$@$,!"<-=q%P%C%U%!$GJT=8$r9T(B
	  ;; $B$J$C$?$H$-$N$3$H$rG[N8$7$F%^!<%+!<$K$7$F$*$/!#(B
	  (setq skk-okuri-ari-max (point-marker))
	  (forward-line 1)
	  (backward-char 1)
	  (setq skk-okuri-nasi-min (point-marker)) )
      (skk-error "$BAw$j$J$7%(%s%H%j$N%X%C%@!<$,$"$j$^$;$s!*(B"
		 "Header line for okuri-nasi entries is missing!" ))))

(defun skk-search ()
  ;; skk-current-search-prog-list $B$NMWAG$K$J$C$F$$$k%W%m%0%i%`$rI>2A$7$F!"(B
  ;; skk-henkan-key$B$r%-!<$K$7$F8!:w$r9T$&!#(B
  (let (l)
    (while (and (null l) skk-current-search-prog-list)
      (setq l (eval (car skk-current-search-prog-list))
	    skk-current-search-prog-list (cdr skk-current-search-prog-list) ))
    l ))

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
                 skk-henkan-key ))
	      (henkan-buffer (current-buffer))
              entry-list entry )
          (with-current-buffer jisyo-buffer
            (setq skk-henkan-key midasi
                  entry-list (skk-search-jisyo-file-1 okurigana limit) )
            (if entry-list
                (progn
                  (setq entry
                        (cond ((and okurigana skk-henkan-okuri-strictly)
                               ;; $BAw$j2>L>$,F10l$N%(%s%H%j$N$_$rJV$9!#(B
                               (nth 2 entry-list) )
                              ((and okurigana skk-henkan-strict-okuri-precedence)
                               ;; $BAw$j2>L>$,F10l$N%(%s%H%j$N$&$7$m$K!"(B
                               ;; $B$=$NB>$N%(%s%H%j$r$D$1$F$+$($9!#(B
                               (skk-nunion (nth 2 entry-list) (car entry-list)))
                              (t (car entry-list)) ))
		  (and skk-search-end-function
		       (setq entry (funcall skk-search-end-function
					    henkan-buffer midasi okurigana entry )) )
		  entry )))))))

(defun skk-search-jisyo-file-1 (okurigana limit &optional delete)
  ;; skk-search-jisyo-file $B$N%5%V%k!<%A%s!#(Bskk-compute-henkan-lists $B$r;HMQ$7!"(B
  ;; $B8+=P$78l$K$D$$$F$N%(%s%H%j$N>pJs$rJV$9!#(B
  ;; DELETE $B$,(B non-nil $B$G$"$l$P!"(BMIDASI $B$K%^%C%A$9$k%(%s%H%j$r:o=|$9$k!#(B
  (let ((key (concat "\n" skk-henkan-key " /"))
        min max size p )
    (save-match-data
      ;; skk-okuri-ari-min $B$H(B skk-okuri-ari-max $B$O<-=q%P%C%U%!$N%m!<%+%kCM!#(B
      (if okurigana
          (setq min skk-okuri-ari-min
                max skk-okuri-ari-max )
        (setq min skk-okuri-nasi-min
              max (point-max) ))
      (if (> limit 0)
          (while (progn (setq size (- max min)) (> size limit))
            (goto-char (+ min (/ size 2)))
            (beginning-of-line)
            (setq p (point))
            ;; $BAw$j$"$j$J$i5U=g$KHf3S$r9T$J$&!#(B
            (if
                (if okurigana
                    (string< (buffer-substring-no-properties
			      p (1- (search-forward  " ")) )
                             skk-henkan-key )
                  (string< skk-henkan-key
                           (buffer-substring-no-properties
			    p (1- (search-forward " "))) ))
                (setq max p)
              (setq min p) )))
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
				 (progn (forward-line 1) (point)) ))))))))


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
			   (point) (progn (end-of-line) (1- (point))) )
			  "/" ) nil nil nil )
    (save-match-data
      (let ((stage 1) (q1 (queue-create)) (q2 (queue-create))
            (q3 (queue-create)) (q4 (queue-create))
            (okuri-key (concat "\[" okurigana)) item headchar )
        (catch 'exit
          (while (not (eolp))
            (setq item (buffer-substring-no-properties
			(point)
			(1- (search-forward "/")) )
                  headchar (if (string= item "") (int-char 0) (skk-str-ref item 0)) )
            (cond ((and (eq headchar ?\[) (<= stage 2))
                   (if (string= item okuri-key)
                       (progn (queue-enqueue q2 item)
                              (setq stage 3) )
                     (setq stage 2)
                     (queue-enqueue q2 item) ))
                  ((= stage 1)
                   (queue-enqueue q1 item) )
                  ((= stage 2)
                   (queue-enqueue q2 item) )
                  ((= stage 3)
                   (if (eq headchar ?\]) ; ?\]
                       (progn (setq stage 4)
                              (queue-enqueue q4 item) )
                     (queue-enqueue q3 item) ))
                  ((= stage 4)
                   (queue-enqueue q4 item) ))))
        ;;        entry1          entry2        entry3          entry4
        (list (queue-all q1) (queue-all q2) (queue-all q3) (queue-all q4)) ))))

(defun skk-nunion (x y)
  ;; X $B$H(B Y $B$NOB=89g$r:n$k!#Ey$7$$$+$I$&$+$NHf3S$O!"(Bequal $B$G9T$o$l$k!#(BX $B$K(B Y
  ;; $B$rGK2uE*$KO"@\$9$k!#(B
  (cond ((null x) y)
        ((null y) x)
        (t (let ((list2 y))
	     (while list2
	       (let* ((list1 (cons nil x))
		      (oldlist1 list1) )
		 (catch 'found
		   (while (cdr list1)
		     (if (equal (car (cdr list1)) (car list2))
			 (throw 'found nil)
		       (setq list1 (cdr list1)) ))
		   (setcdr list1 (list (car list2)))
		   (setq x (cdr oldlist1) ))
	       (setq list2 (cdr list2)) )
	       ))
	   x )))

(defun skk-search-kakutei-jisyo-file (file limit &optional nomsg)
  ;; $B<-=q%U%!%$%k$rC5$7!"8uJd$r%j%9%H$GJV$9!#(B
  ;; $B8uJd$r8+$D$1$?>l9g$O!"Bg0hJQ?t(B skk-kakutei-flag $B$K(B non-nil $B$rBeF~$9$k!#(B
  ;; $B8uJd$,8+$D$+$i$J$+$C$?>l9g$O!"(Bnil $B$rJV$9!#(B
  (setq skk-kakutei-flag (skk-search-jisyo-file file limit nomsg)) )

(defun skk-update-jisyo (word &optional purge)
  (funcall skk-update-jisyo-function word purge) )

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
	   skk-henkan-key ))
	(henkan-buffer (current-buffer)) )
    (if jisyo-buffer
	(let ((inhibit-quit t) buffer-read-only old-entry okurigana)
	  (if (> skk-okuri-index-min -1)
	      (setq word (skk-remove-common word)
		    midasi skk-henkan-key ))
	  (setq okurigana (or skk-henkan-okurigana skk-okuri-char))
	  (with-current-buffer jisyo-buffer
	    ;; $B4{B8%(%s%H%j$r8!:w8e>C5n$9$k!#A^F~$9$Y$-%(%s%H%j$,(B entry1 $B$K(B 1
	    ;; $B$D$7$+$J$/!"(Bword $B$HF1$8J8;z$G$"$C$F$b!"$$$C$?$s>C$7$F$=$N%(%s%H(B
	    ;; $B%j$r(B min $B%]%$%s%H$K0\F0$5$;$J$1$l$P$J$i$J$$(B ($BFI$_$NJd40$r9T$&$H(B
	    ;; $B$-$O!"(Bmin $B%]%$%s%H$+$i8+=P$7$rC5$9$?$a!"?7$7$$8+=P$7$[$I!"(Bmin
	    ;; $B%]%$%s%H$K6a$$$H$3$m$K$J$1$l$P$J$i$J$$(B)$B!#(B
	    (setq skk-henkan-key midasi
		  old-entry (skk-search-jisyo-file-1 okurigana 0 'delete) )
	    (skk-update-jisyo-1 okurigana word old-entry purge)
	    (and skk-update-end-function
		 (funcall skk-update-end-function
			  henkan-buffer midasi okurigana word purge ))
	    (setq skk-update-jisyo-count (1+ skk-update-jisyo-count))
	    (if (and skk-jisyo-save-count
		     (= skk-jisyo-save-count skk-update-jisyo-count) )
		;; auto save.
		(skk-save-jisyo 'quiet) ))))))

(defun skk-update-jisyo-1 (okurigana word old-entry-list purge)
  ;; $B4{B8%(%s%H%j$+$i7W;;$7$?(B entry[1-4] $B$NCM$H!":#2s$NJQ49$N7k2L(B word $B$H$r%^!<(B
  ;; $B%8$7$F!"?7$?$J%(%s%H%j$r7W;;$7!"A^F~$9$k!#(B
  (let ((entry1 (car old-entry-list)) (entry2 (nth 1 old-entry-list))
        (entry3 (nth 2 old-entry-list)) (entry4 (nth 3 old-entry-list)) )
    (if (not purge)
        ;; entry1 $B$N@hF,$N%(%s%H%j$r(B word $B$K$9$k!#(B
        (setq entry1 (cons word (delete word entry1)))
      ;; $BAw$j$J$7!"$b$7$/$O(B skk-henkan-okuri-strictly $B$H(B
      ;; skk-henkan-strict-okuri-precedence $B$,(B nil $B$N>l9g!#(B
      (if (or (not okurigana) (not (or skk-henkan-okuri-strictly
				       skk-henkan-strict-okuri-precedence )))
          ;; entry1 $B$r(B purge$B!#6&MQ<-=q$K$"$k%(%s%H%j$@$C$?$i!"(B
          ;; skk-ignore-dic-word $B$G%/%)!<%H$7$F<!$NJQ49$+$i=PNO$7$J$$$h$&$K$9(B
          ;; $B$k!#6&MQ<-=q$K$J$$J8;zNs$O(B word $B$r>C$9!#(B
          (if (skk-public-jisyo-has-entry-p okurigana word)
              (setq entry1 (skk-compose-ignore-entry entry1 word))
            (setq entry1 (delete word entry1)) )
        ;; $BAw$j$"$j$G!"$+$D(B skk-henkan-okuri-strictly $B$+(B
	;; skk-henkan-strict-okuri-precedence $B$,(B non-nil $B$N>l9g$G!"$+$D(B
        ;; $B$3$N(B word $B$H%Z%"$K$J$kAw$j2>L>$,(B okurigana $B$7$+$J$$$H$-!#(B
        (if (and okurigana (or skk-henkan-okuri-strictly
			       skk-henkan-strict-okuri-precedence )
                 (null (member word entry2)) (null (member word entry4)) )
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
			 (setcdr last2 nil) )
                    ;; entry4 $B$N@hF,$O>o$K(B "]"$B!#(B
                    (setq entry4 (cdr entry4)) )))
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
                  entry4 (list "]") ))))
      (if entry2
          ;; entry2 -- $B:#2s;HMQ$7$J$+$C$?Aw$j2>L>$r;H$&4A;z$N8uJd72(B + "[" + $B:#(B
          ;; $B2s;HMQ$7$?Aw$j2>L>(B ($BAw$j2>L>$N$_!#$=$NAw$j2>L>$r;HMQ$9$k4A;z$N8u(B
          ;; $BJd72$O!"(Bentry3 $B$K4^$^$l$k(B)$B!#(B
          (progn
            (insert (mapconcat 'skk-quote-char entry2 "/" ) "/")
            ;; entry2 $B$,(B null $B$J$i(B entry3 $B$b(B null$B!#(B
            (and entry3
		 ;; entry3 -- $B:#2s;HMQ$7$?Aw$j2>L>$r;H$&A44A;z%(%s%H%j(B
		 (insert (mapconcat 'skk-quote-char entry3 "/") "/") )
            ;; purge $B$G(B entry3 $B$,(B null $B$K$J$C$?>l9g$O(B entry4 $B$,;D$C$F$$$k$H$-(B
            ;; $B$,$"$k!#(B
            (and entry4
		 ;; entry4 -- "]" + $BB>$NAw$j2>L>$r;H$&A44A;z%(%s%H%j(B (entry2 $B$N(B
		 ;; $B;D$j(B)$B!#(B
		 (insert (mapconcat 'skk-quote-char entry4 "/") "/") ))))))

(defun skk-quote-char (word)
  ;; $B<-=q$N@)8B$+$i<-=q%(%s%H%jFb$K4^$a$F$O$J$i$J$$J8;z$,(B WORD $B$NCf$K$"$l$P!"(B
  ;; $BI>2A$7$?$H$-$K$=$NJ8;z$H$J$k$h$&$J(B Lisp $B%3!<%I$rJV$9!#(B
  (save-match-data
    (if (and word
             (string-match "[/\n\r\"]" word)
             ;; we should not quote WORD if it is a symbolic expression
             (not (skk-lisp-prog-p word)) )
        (concat "(concat \""
                (mapconcat (function (lambda (c)
                                       (cond ((eq c ?/) "\\057" )
                                             ((eq c ?\n) "\\n" )
                                             ((eq c ?\r) "\\r" )
                                             ((eq c ?\") "\\\"" )
                                             ((eq c ?\\) "\\\\" )
                                             (t (char-to-string c)))))
                           ;; $BJ8;zNs$rBP1~$9$k(B char $B$N%j%9%H$KJ,2r$9$k!#(B
                           (append word nil) "")
                "\")")
      word )))

(defun skk-lisp-prog-p (word)
  ;; word $B$,(B Lisp $B%W%m%0%i%`$G$"$l$P!"(Bt $B$rJV$9!#(B
  (let ((l (skk-str-length word)))
    (and (> l 2) (eq (skk-str-ref word 0) ?\() (< (aref word 1) 128)
         (eq (skk-str-ref word (1- l)) ?\)) )))

(defun skk-public-jisyo-has-entry-p (okurigana word)
  ;; $B6&M-<-=q$,(B MIDASHI $B5Z$S$=$l$KBP1~$9$k(B WORDS $B%(%s%H%j$r;}$C$F$$$l$P!"(B
  ;; non-nil $B$rJV$9!#%W%i%$%Y!<%H<-=q$N%P%C%U%!$G%3!<%k$5$l$k!#(B
  (let (fn skk-henkan-okuri-strictly skk-henkan-strict-okuri-precedence)
    (if okurigana
        (setq skk-henkan-okurigana okurigana) )
    ;; skkserv $B$r;H$&@_Dj$K$J$C$F$$$?$i!"(Bskk-server.el $B$r%m!<%I$9$k!#(B
    (and (not (featurep 'skk-server))
	 (or (and (boundp 'skk-servers-list) skk-servers-list)
	     (or (and (boundp 'skk-server-host) skk-server-host)
		 (getenv "SKKSERVER") ))
	 (require 'skk-server) )
    (setq fn (funcall skk-public-jisyo-to-be-searched-function))
    (and fn (member word (eval fn))) ))

(defun skk-public-jisyo-to-be-searched-original ()
  ;; skk-search-prog-list $B$NCf$+$i!"0lHVBg$-$J6&M-<-=q$G%5!<%A$9$k%W%m(B
  ;; $B%0%i%`$rJV$9!#(B
  (let (fn)
    (and (featurep 'skk-server) (or skk-servers-list skk-server-host)
	 (setq fn (assq 'skk-search-server skk-search-prog-list)) )
    ;; skk-search-server $B$+$i;O$^$k%j%9%H$,$J$1$l$P!"$H$K$+$/Bg$-$$<-=q$r0z?t(B
    ;; $B$K$7$F$$$k(B skk-search-jisyo-file $B%W%m%0%i%`$rC5$9!#(B
    (if (and (not fn) (or skk-aux-large-jisyo skk-large-jisyo))
	(let ((spl skk-search-prog-list)
	      cell )
	  (while (setq cell (car spl))
	    (if (and (eq (car cell) 'skk-search-jisyo-file)
		     (memq (nth 1 cell) '(skk-aux-large-jisyo skk-large-jisyo)) )
		(setq fn cell
		      spl nil )
	      (setq spl (cdr spl)) ))))
    fn ))

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
              l (cdr l) )
        (and (string-match "(skk-ignore-dic-word +\\([^\)]+\\))" e)
	     (setq arg (concat arg
			       (substring e (1+ (match-beginning 1))
					  (1- (match-end 1)) )
			       "\" \"" )
		   entry (delq e entry) )))
      (if add
          (setq arg (if arg (concat arg add) add))
        ;; $BKvHx$N(B " \"" $B$r@Z$jMn$H$9!#(B
        (setq arg (substring arg 0 -2)) )
      (cons (concat "(skk-ignore-dic-word \"" arg "\")") entry) )))


(defun skk-katakana-region (start end &optional vcontract)
  "$B%j!<%8%g%s$N$R$i$,$J$r%+%?%+%J$KJQ49$9$k!#(B
$B%*%W%7%g%J%k0z?t$N(B VCONTRACT $B$,(B non-nil $B$G$"$l$P!"(B\"$B$&!+(B\" $B$r(B \"$B%t(B\" $B$KJQ49$9(B
$B$k!#(B
$B0z?t$N(B START $B$H(B END $B$O?t;z$G$b%^!<%+!<$G$bNI$$!#(B"
  (interactive "*r\nP")
  (skk-save-point
   (let (katakana)
     (save-match-data
       (goto-char start)
       (while (re-search-forward  "[$B$!(B-$B$s(B]+" end 'noerror)
	 (setq katakana
	       (skk-hiragana-to-katakana
		(buffer-substring-no-properties (match-beginning 0)
						(match-end 0) )))
	 (backward-char (skk-str-length katakana))
	 ;; firstly insert a new string, secondly delete an old string to save
	 ;; the cursor position.
	 (insert-and-inherit katakana)
	 (delete-region (+ (match-beginning 0) (length katakana))
			(+ (match-end 0) (length katakana)) ))
       (if vcontract
	   (progn
	     (goto-char start)
	     (while (re-search-forward  "$B%&!+(B" end 'noerror)
	       (backward-char (skk-str-length "$B%&!+(B"))
	       (let ((vu-len (length "$B%t(B")))
		 (insert-and-inherit "$B%t(B")
		 (delete-region (+ (match-beginning 0) vu-len)
				(+ (match-end 0) vu-len) )))))))))

(defun skk-hiragana-region (start end &optional vexpand)
  "$B%j!<%8%g%s$N%+%?%+%J$r$R$i$,$J$KJQ49$9$k!#(B
$B%*%W%7%g%J%k0z?t$N(B VEXPAND $B$,(B non-nil $B$G$"$l$P!"(B\"$B%t(B\" $B$r(B \"$B$&!+(B\" $B$KJQ49$9$k!#(B
$B0z?t$N(B START $B$H(B END $B$O?t;z$G$b%^!<%+!<$G$bNI$$!#(B
\"$B%u(B\" $B$H(B \"$B%v(B\" $B$OJQ99$5$l$J$$!#$3$N(B 2 $B$D$NJ8;z$OBP1~$9$k$R$i$,$J$,$J$$$N$G!"%+(B
$B%?%+%J$H$7$F$O07$o$l$J$$!#(B"
  (interactive "*r\nP")
  (skk-save-point
   (let (hiragana)
     (save-match-data
       (goto-char start)
       (while (re-search-forward  "[$B%!(B-$B%s(B]+" end 'noerror)
	 (setq hiragana
	       (skk-katakana-to-hiragana
		(buffer-substring-no-properties (match-beginning 0)
						(match-end 0) )))
	 (backward-char (skk-str-length hiragana))
	 ;; firstly insert a new string, secondly delete an old string to save
	 ;; the cursor position.
	 (insert-and-inherit hiragana)
	 (delete-region (+ (match-beginning 0) (length hiragana))
			(+ (match-end 0) (length hiragana)) ))
       (if vexpand
	   (progn
	     (goto-char start)
	     (while (re-search-forward  "$B%t(B" end 'noerror)
	       (backward-char (skk-str-length "$B%t(B"))
	       (insert-and-inherit "$B$&!+(B")
	       (let ((vu-len (length "$B$&!+(B")))
		 (delete-region (+ (match-beginning 0) vu-len)
				(+ (match-end 0) vu-len) )))))))))

(defun skk-jisx0208-latin-region (start end)
  "$B%j!<%8%g%s$N(B ascii $BJ8;z$rBP1~$9$kA43Q1QJ8;z$KJQ49$9$k!#(B"
  (interactive "*r")
  (skk-save-point
   (save-match-data
     (goto-char end)
     (while (re-search-backward "[ -~]" start 'noerror)
       ;; firstly insert a new char, secondly delete an old char to save
       ;; the cursor position.
       (let* ((c (aref skk-default-jisx0208-latin-vector (following-char)))
	      (c-len (length c)) )
	 (insert-and-inherit c)
	 (delete-region (+ (match-beginning 0) c-len)
			(+ (match-end 0) c-len) ))))))

(defun skk-latin-region (start end)
  ;; $B%j!<%8%g%s$NA43Q1Q?t;z$rBP1~$9$k(B ascii $BJ8;z$KJQ49$9$k!#(B
  ;; egg.el 3.09 $B$N(B hankaku-region $B$r;29M$K$7$?!#(B
  (interactive "*r")
  (skk-save-point
   (save-match-data
     (let (val)
       (goto-char end)
       (while (re-search-backward "\\cS\\|\\cA" start 'noerror)
	 (setq val (skk-jisx0208-to-ascii (char-to-string (following-char))))
	 (if val
	     (progn
	       (insert-and-inherit val)
	       (delete-region (+ (match-beginning 0) 1)
			      (+ (match-end 0) 1) ))))))))

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
			 skk-henkan-end-point 'vcontract ))
     (skk-emulate-original-map arg) )))

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
			 skk-henkan-end-point 'vexpand ))
     (skk-emulate-original-map arg) )))

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
			 skk-henkan-end-point ))
     (skk-emulate-original-map arg) )))

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
			 skk-henkan-end-point ))
     (skk-emulate-original-map arg) )))

(defun skk-*-henkan-1 (func &rest args)
  ;; $BJQ492DG=$+$I$&$+$N%A%'%C%/$r$7$?8e$K(B ARGS $B$r0z?t$H$7$F(B FUNC $B$rE,MQ$7!"(B
  ;; skk-henkan-start-point $B$H(B skk-henkan-end-point $B$N4V$NJ8;zNs$rJQ49$9$k!#(B
  (cond ((skk-get-prefix skk-current-rule-tree)
	 (skk-error "$B%U%#%C%/%9$5$l$F$$$J$$(B skk-prefix $B$,$"$j$^$9(B"
		    "Have unfixed skk-prefix" ))
	((< (point) skk-henkan-start-point)
	 (skk-error "$B%+!<%=%k$,JQ493+;OCOE@$h$jA0$K$"$j$^$9(B"
		    "Henkan end point must be after henkan start point" ))
	((and (not skk-allow-spaces-newlines-and-tabs)
	      (skk-save-point (beginning-of-line)
			      (> (point) skk-henkan-start-point) ))
	 (skk-error "$BJQ49%-!<$K2~9T$,4^$^$l$F$$$^$9(B"
		    "Henkan key may not contain a new line character" )))
  (apply func args)
  (skk-kakutei) )

(defun skk-hiragana-to-katakana (hiragana)
  (let ((diff (- ?$B%"(B ?$B$"(B)))
    (mapconcat (function (lambda (e) (char-to-string (+ e diff))))
	       (string-to-int-list hiragana) "" )))

(defun skk-katakana-to-hiragana (katakana)
  (let ((diff (- ?$B%"(B ?$B$"(B)))
    (mapconcat (function (lambda (e) (char-to-string (- e diff))))
	       (string-to-int-list katakana) "" )))

(defun skk-splice-in (org offset spliced)
  ;; ORG := '(A B C), SPLICED := '(X Y), OFFSET := 1
  ;; -> '(A B X Y C)
  (let (tmp tail)
    (or (> offset 0) (error "Cannot splice in!"))
    (setq tmp (nthcdr (1- offset) org)
          tail (cdr tmp) )
    (setcdr tmp nil) ;cut off
    (setcdr tmp (if tail (nconc spliced tail) spliced))
    org ))

;; (defun skk-chomp (nth list)
;;   ;; LIST := '(A B C D), NTH := 1
;;   ;; -> '(A B)
;;   (and (> nth -1) (setcdr (nthcdr nth list) nil))
;;   list )

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
	   (marker-position skk-henkan-end-point) )
      (skk-face-on skk-henkan-overlay
		   skk-henkan-start-point skk-henkan-end-point
		   skk-henkan-face skk-henkan-overlay-priority )))

(defun skk-henkan-face-off ()
  ;; skk-henkan-start-point $B$H(B skk-henkan-end-point $B$N4V$NI=<($rJQ99$7$F$$$k(B
  ;; skk-henkan-overlay $B$r>C$9!#(B
  (and skk-henkan-face (skk-detach-extent skk-henkan-overlay)) )

(defun skk-detach-extent (object)
  (static-cond
   ((eq skk-emacs-type 'xemacs)
    (and (extentp object) (detach-extent object)) )
   (t
    (and (overlayp object) (delete-overlay object)) )))

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
             (substring face-name 0 (match-beginning 0)) )
            (set-face-background
             face
             (substring face-name (1+ (match-beginning 0))) ))
          face ))))
                        
;; skk-auto.el, skk-rdbms.el $B$NN>J}$G;H$&$N$G!"(Bskk-auto.el $B$h$j0\F0$7$?!#(B
(defun skk-remove-common (word)
  ;; skk-henkan-key $B$H(B word $B$N4V$K6&DL$NAw$j2>L>$r<h$j=|$-!"Aw$j2>L>0J30$NItJ,(B
  ;; $B$NJ8;zNs$rJV$9!#(Bskk-henkan-key $B$H(B skk-henkan-okurigana $B$NCM$r%;%C%H$9$k!#(B
  ;; $BNc$($P!"(Bword == $B;}$C$F$-$?(B $B$G$"$l$P!"(Bskk-henkan-key := "$B$b(Bt",
  ;; skk-henkan-okurigana := "$B$C$F(B", word := "$B;}(B" $B$N$h$&$KJ,2r$7!"(Bword $B$rJV$9!#(B
  ;; skk-auto-okuri-process $B$NCM$,(B non-nil $B$G$"$k$H$-$K$3$N4X?t$r;HMQ$9$k!#(B
  (if (and (not (skk-numeric-p)) (not skk-abbrev-mode)
           (or skk-henkan-in-minibuff-flag
               (and (<= skk-okuri-index-min skk-henkan-count)
                    (<= skk-henkan-count skk-okuri-index-max) )))
      (let ((midasi skk-henkan-key)
            (midasi-len (skk-str-length skk-henkan-key))
            (word-len (skk-str-length word))
            (cont t)
            char pos pos2 midasi-tail word-tail new-word okuri-first
            new-skk-okuri-char new-skk-henkan-key )
        (if (not (and (>= midasi-len 2) (>= word-len 2)))
            nil
          ;; check if both midasi and word end with the same ascii char.
          (if (and (eq (skk-str-ref midasi (1- midasi-len))
		       (skk-str-ref word (1- word-len)))
                   (skk-ascii-char-p (skk-str-ref midasi (1- midasi-len))) )
              ;; if so chop off the char from midasi and word.
	      ;; assume size of an ASCII char is always 1.
              (setq midasi (substring midasi 0 -1)
                    midasi-len (1- midasi-len)
                    word (substring word 0 -1)
                    word-len (1- word-len) ))
          (setq midasi-tail (skk-substring midasi (1- midasi-len)
					   midasi-len )
		word-tail (skk-substring word (1- word-len)
					 word-len ))
          ;; $B$b$&>/$7E83+$G$-$=$&$@$,!"%P%$%H%3%s%Q%$%i!<$,%*%W%F%#%^%$%:$7$d(B
          ;; $B$9$$$h$&$K(B not $B$rIU$1$k$@$1$K$7$F$*$/!#(B
          (if (not (and (string= midasi-tail word-tail)
                        (or (and (skk-string<= "$B$!(B" midasi-tail)
                                 (skk-string<= midasi-tail "$B$s(B") )
                            (member midasi-tail '("$B!"(B" "$B!#(B" "$B!$(B" "$B!%(B")) )))
              nil
            (setq pos (1- word-len)
                  new-word new-skk-henkan-key )
            (while (and cont (> pos 0))
              (setq char (skk-substring word (1- pos) pos))
              (if (and (skk-string<= "$B0!(B" char) (skk-string<= char "$Bt$(B"))
                  ;; char is the right-most Kanji
                  (setq cont nil)
                (setq pos (1- pos)) ))
            (setq pos2 (- midasi-len (- word-len pos)))
            ;; check if midasi and word has the same tail of length
            (if (not (string= (skk-substring midasi pos2 midasi-len)
                              (skk-substring word pos word-len) ))
                nil
              (setq okuri-first (skk-substring word pos (1+ pos)))
              (setq skk-henkan-okurigana
                    (if (and (string= okuri-first "$B$C(B")
                             (<= (+ pos 2) word-len) )
                        ;; in this case okuriga consits of two
                        ;; characters, e.g., $B!V;D$C$?!W(B
                        (skk-substring word pos (+ pos 2))
                      okuri-first ))
              (setq new-word (skk-substring word 0 pos)
		    new-skk-okuri-char (skk-okurigana-prefix okuri-first)
		    new-skk-henkan-key (concat
					(skk-substring midasi 0 pos2)
					new-skk-okuri-char ))
              (if (not skk-henkan-in-minibuff-flag)
                  (setq word new-word
                        skk-henkan-key new-skk-henkan-key )
                ;; ask if register as okuri-ari word.
                (let (inhibit-quit)	; allow keyboard quit
                  (if (y-or-n-p
                       (format
                        (if skk-japanese-message-and-error
                            "%s /%s/ $B$rAw$j$"$j%(%s%H%j$H$7$FEPO?$7$^$9$+!)(B"
                          "Shall I register this as okuri-ari entry: %s /%s/ ? " )
                        new-skk-henkan-key new-word ))
                      (setq word new-word
			    skk-okuri-char new-skk-okuri-char
                            skk-henkan-key new-skk-henkan-key )
                    (setq skk-henkan-okurigana nil
                          skk-okuri-char nil )
                    (message "") ))))))))
  ;; $BJ,2r$7$?(B word ($BAw$j2>L>ItJ,$r=|$$$?$b$N(B) $B$rJV$9!#(B
  word )

(defun skk-okurigana-prefix (okurigana)
  (cond ((string= okurigana "$B$s(B")
	 "n" )
	((string= okurigana "$B$C(B")
	 (aref skk-kana-rom-vector
	       ;; assume the character is hiragana of JIS X 0208.
	       (- (skk-char-octet
		   (string-to-char (skk-substring skk-henkan-okurigana 1 2))
		   1 )
		  33 )))
	(t (aref skk-kana-rom-vector
		 (- (skk-char-octet
		     (string-to-char (skk-substring skk-henkan-okurigana 0 1))
		     1 )
		    33 )))))

;; from type-break.el.  Welcome!
(defun skk-time-difference (a b)
  ;; Compute the difference, in seconds, between a and b, two structures
  ;; similar to those returned by `current-time'.
  ;; Use addition rather than logand since that is more robust; the low 16
  ;; bits of the seconds might have been incremented, making it more than 16
  ;; bits wide.
  (+ (lsh (- (car b) (car a)) 16)
     (- (nth 1 b) (nth 1 a)) ))

(defun skk-remove-minibuffer-setup-hook (&rest args)
  ;; Remove all args from minibuffer-setup-hook.
  (while args
    (remove-hook 'minibuffer-setup-hook (car args))
    (setq args (cdr args)) ))

(add-hook 'edit-picture-hook 'skk-misc-for-picture 'append)
;; add 'skk-save-jisyo only to remove easily.
(add-hook 'skk-before-kill-emacs-hook 'skk-save-jisyo)
(add-hook 'minibuffer-exit-hook
          (function
           (lambda ()
	     (remove-hook 'pre-command-hook 'skk-pre-command 'local)
	     (skk-remove-minibuffer-setup-hook
	      'skk-j-mode-on 'skk-setup-minibuffer
	      (function (lambda ()
			  (add-hook 'pre-command-hook 'skk-pre-command nil 'local) ))))))

(defun skk-setup-modeline ()
  "$B%b!<%I9T$X$N%9%F!<%?%9I=<($r=`Hw$9$k!#(B"
  (cond ((eq skk-status-indicator 'left)
	 (mapcar (function
		  (lambda (el)
		    (let ((sym (car el))
			  (strs (cdr el)))
		      (if (string= (symbol-value sym) (cdr strs))
			  (set sym (car strs)) ))))
		 (cond
		  ((and (fboundp 'face-proportional-p)
			(face-proportional-p 'modeline))
		   '((skk-latin-mode-string . ("--SKK:" . " SKK"))
		     (skk-hiragana-mode-string . ("--$B$+$J(B:" . " $B$+$J(B"))
		     (skk-katakana-mode-string . ("--$B%+%J(B:" . " $B%+%J(B"))
		     (skk-jisx0208-latin-mode-string . ("--$BA41Q(B:" . " $BA41Q(B"))
		     (skk-abbrev-mode-string . ("--a$B$"(B:" . " a$B$"(B"))))
		  (t
		   '((skk-latin-mode-string . ("--SKK::" . " SKK"))
		     (skk-hiragana-mode-string . ("--$B$+$J(B:" . " $B$+$J(B"))
		     (skk-katakana-mode-string . ("--$B%+%J(B:" . " $B%+%J(B"))
		     (skk-jisx0208-latin-mode-string . ("--$BA41Q(B:" . " $BA41Q(B"))
		     (skk-abbrev-mode-string . ("--a$B$"(B::" . " a$B$"(B"))))))
	 (cond ((eq skk-emacs-type 'xemacs)
		(or (memq 'skk-input-mode-string default-mode-line-format)
		    (setq-default default-modeline-format
				  (append '("" skk-input-mode-string)
					  default-modeline-format) ))
		(mapc
		 (function
		  (lambda (buf)
		    (if (buffer-live-p buf)
			(save-excursion
			  (set-buffer buf)
			  (or (memq 'skk-input-mode-string modeline-format)
			      (setq modeline-format
				    (append '("" skk-input-mode-string)
					    modeline-format) ))))))
		 (buffer-list) ))
	       (t
		(or (memq 'skk-input-mode-string mode-line-format)
		    (setq-default
		     mode-line-format
		     (append '("" skk-input-mode-string)
			     mode-line-format) ))))
	 (setq-default skk-input-mode-string "")
	 (force-mode-line-update t) )
	(t
	 (setq minor-mode-alist
	       (put-alist 'skk-mode
			  ;; each element of minor-mode-alist is not cons cell.
			  '(skk-input-mode-string) minor-mode-alist) ))))

(run-hooks 'skk-load-hook)

(provide 'skk)
;;; Local Variables:
;;; End:
;;; skk.el ends here
