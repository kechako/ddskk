;;; skk-lookup.el --- SKK lookup gateway
;; Copyright (C) 1999, 2000 Mikio Nakajima <minakaji@osaka.email.ne.jp>

;; Author: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Version: $Id: skk-lookup.el,v 1.1.2.3.2.6 2000/09/27 13:42:07 minakaji Exp $
;; Keywords: japanese
;; Created: Sep. 23, 1999
;; Last Modified: $Date: 2000/09/27 13:42:07 $

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

;;; Commentary
;;
;; Keisuke Nishida <kxn30@po.cwru.edu> $B$5$s$N:n$i$l$?<-=q8!:w%D!<%k(B
;; Lookup $B$H(B SKK $B$H$N(B gateway $B$r9T$J$$!"(BLookup $B$G8!:w$G$-$k<-=q$r;H$C(B
;; $B$F8uJd$r=PNO$9$k%W%m%0%i%`$G$9!#(B
;;
;; <HOW TO INSTALL>
;; make $B$r<B9T$9$k:]$K!"(Blookup.el $B$K%Q%9$,DL$C$F$$$F(B require $B$G$-$k(B
;; $B$H$-$O!"K\%W%m%0%i%`$b<+F0E*$K%$%s%9%H!<%k$5$l$^$9!#(Blookup.el $B$,(B
;; $B%$%s%9%H!<%k$5$l$F$$$k$N$K(B Emacs $B$,8!=P$7$F$/$l$J$$$H$-$O!"(B
;; SKK-CFG $B$rJT=8$7$F(B VERSION_SPECIFIC_LISPDIR $B$K$=$N%Q%9$r=q$/$HNI(B
;; $B$$$G$7$g$&!#(B
;;
;; <HOW TO USE>
;; $BEvA3$G$9$,!"(BLookup $B$,%$%s%9%H!<%k$5$l$F$$$F!"$+$D!"BP1~$9$k<-=q$,(B
;; $B%^%&%s%H$5$l$F$$$J$$$H;H$($^$;$s!#(B
;;
;; $B<!$N$h$&$K(B skk-search-prog-list $B$K2C$($F;XDj$7;HMQ$7$^$9!#(B
;; SKK $B$,MQ0U$7$F$$$k8!:w%W%m%0%i%`$NCf$G:G$b=E$$$N$G!"(B
;; skk-seach-server $B$N8!:w$N8e$K;}$C$F$/$k$N$,%;%*%j!<$G$9!#(B
;;
;;  (setq skk-search-prog-list
;;        '((skk-search-jisyo-file skk-jisyo 0 t)
;;          (skk-search-server skk-aux-large-jisyo 10000)
;;          (skk-lookup-search)))
;;
;; $B%G%#%U%)%k%H$N@_Dj$G$O!"(Blookup $B$NJQ?t$G$"$k(B `lookup-search-agents'
;; $B$r%3%T!<$7$F(B ndkks, ndcookie, ndnmz $B$r<h$j5n$j!"(B
;; `skk-lookup-search-agents' $B$K%;%C%H$7$F$3$l$r8!:w$9$k$h$&$K$7$F$$$^$9!#(B
;; $B$b$A$m$s(B lookup $B$N8!:w$H$O0[$J$k@_Dj$r(B `skk-lookup-search-agents' $B$KL@(B
;; $B<($9$k$3$H$b2DG=$G$9!#(B
;;
;; $B8=:_BP1~$7$F$$$k<-=q$O(B
;;
;;   ispell, jedict, CHIEZO, CHUJITEN, COLLOC, GENIUS, GN99EP01,
;;   GN99EP02, IWAKOKU, KANJIGEN, KANWA, KOJIEN, KOKUGO, KOUJIEN,
;;   MYPAEDIA, NEWANC, PLUS, RIKAGAKU, WAEI
;;
;; $B$G$9(B (lookup-dictionary-name $B$,JV$9CM$GI85-$7$F$$$^$9(B)$B!#(B
;; kakasi (KAKASI $B$rMxMQ$9$k$J$i(B skk-kakasi.el $B$r;H$$$^$7$g$&(B),
;; ndcookie, ndnmz $B$K$OBP1~$7$F$$$^$;$s$7!"BP1~$NI,MW$O$J$$$H9M$($F$$(B
;; $B$^$9(B ($B%a%j%C%H$,$"$l$P65$($F2<$5$$(B)$B!#(B
;;
;; $B$4<+J,$G;HMQ$7$F$$$k<-=q$N=PNO$,>e<j$/<h$j9~$a$J$$$H$-$O!"(B
;; `skk-lookup-pickup-headings' $B$r;HMQ$7$FNc$($P!"(B
;;
;;   (skk-lookup-pickup-headings "$B$3$7$g$&(B" 'exact)
;;
;; $B$J$I$HI>2A$7$F(B ("$B$3$7$g$&(B" $B$NJ8;zNsItJ,$OLdBj$H$J$C$F$$$k8!:wBP>]$H(B
;; $BF~$lBX$($^$7$g$&(B) `lookup-dictionary-name' $B$H(B
;; `lookup-entry-heading' $B$,JV$9CM$r;29M$K!"(B`'skk-lookup-option-alist'
;; $B$KI,MW$J%j%9%H$r2C$($^$7$g$&!#?7$?$J%j%9%H$r2C$($i$l$?$i@'Hs:n<T$K(B
;; $B$bCN$;$F2<$5$$!#(Bdefault value $B$K<h$j9~$_$?$$$H;W$$$^$9!#$h$m$7$/$*(B
;; $B4j$$$$$?$7$^$9!#(B
;;
;; $BKvHx$J$,$i!"(BLookup $B$r:n$i$l$?(B Lookup Development Team $B$N3'MM!"(B
;; Lookup $B$N(B $B86:n<T$G$"$j!"K\%W%m%0%i%`$N3+H/$K$b$$$/$D$+5.=E$J$40U8+$r$$$?$@(B
;; $B$-$^$7$?(B Keisuke Nishida <kxn30@po.cwru.edu> $B$5$s!"3+H/$N=i4|$+$i%G(B
;; $B%P%C%0$r<jEA$C$F$$$?$@$$$?!"(BNEMOTO Takashi <tnemoto@mvi.biglobe.ne.jp>
;; $B$5$s!"(Bsphere <sphere@pop12.odn.ne.jp> $B$5$s$K?<$/46<U$$$?$7$^$9!#(B

;;; Code:
(eval-when-compile (require 'cl) (require 'skk-macs) (require 'skk-vars)
		   (require 'skk-num))

(require 'poe)
(require 'lookup)

(defalias-maybe 'skk-okurigana-prefix 'skk-auto-okurigana-prefix)

;;;; inline functions.
(defsubst skk-lookup-get-method (name okuri-process)
  (save-match-data
    (let ((list (assoc name skk-lookup-option-alist))
	  sex)
      ;; If you search via ndtpd, book's name and slash are attached to NAME
      ;; as prefix, like `IWANAMI/KOJIEN'.  The following forms will truncate
      ;; it to `KOJIEN'.
      (if (and (null list) (string-match "/\\(.+\\)$" name))
	  (setq list (assoc (match-string 1 name) skk-lookup-option-alist)))
      (setq sex (nth okuri-process (if list (cdr list) skk-lookup-default-option-list)))
      (cond ((symbolp sex) sex)
	    (t (eval sex))))))

(defsubst skk-lookup-get-nonsearch-sex (name)
  (save-match-data
    (let ((list (assoc name skk-lookup-option-alist)))
      (if (and (null list) (string-match "/\\(.+\\)$" name))
	  (setq list (assoc (match-string 1 name) skk-lookup-option-alist)))
      (nth 3 (if list (cdr list) skk-lookup-default-option-list)))))

(defsubst skk-lookup-get-pickup-regexp (name)
  (save-match-data
    (let ((list (assoc name skk-lookup-option-alist)))
      (if (and (null list) (string-match "/\\(.+\\)$" name))
	  (setq list (assoc (match-string 1 name) skk-lookup-option-alist)))
      (nth 4 (if list (cdr list) skk-lookup-default-option-list)))))

(defsubst skk-lookup-get-split-regexp (name)
  (save-match-data
    (let ((list (assoc name skk-lookup-option-alist)))
      (if (and (null list) (string-match "/\\(.+\\)$" name))
	  (setq list (assoc (match-string 1 name) skk-lookup-option-alist)))
      (nth 5 (if list (cdr list) skk-lookup-default-option-list)))))

;;;; funcitions.
;;;###autoload
(defun skk-lookup-search ()
  (if (and (boundp 'skk-num-list) (or skk-num-list skk-num-recompute-key))
      ;; $B?tCMJQ49$N$H$-$OJQ49%-!<$,(B `#' $B$r4^$`$b$N$J$N$G!"(Blookup $B$G8!:w$7$J$$!#(B
      nil
    (save-excursion
      (let ((module (skk-lookup-default-module))
	    ;; if `lookup-enable-gaiji' is nil, gaiji tag like
	    ;; `<gaiji=za52a>' is put out.
	    ;;(lookup-enable-gaiji nil)
	    (lookup-gaiji-alternate "")
	    (henkan-key skk-henkan-key)
	    okuri-process v)
	(cond ((not (or skk-henkan-okurigana skk-okuri-char))
	       ;; okuri-nasi
	       (setq okuri-process 0))
	      ;; okuri-ari and `skk-lookup-process-henkan-key-function' is non-nil.
	      (skk-lookup-process-henkan-key-function
	       (setq v (funcall skk-lookup-process-henkan-key-function
				henkan-key)
		     henkan-key (car v)
		     okuri-process (cdr v)))
	      ;; okuri-ari and (not skk-process-okuri-early)
	      (skk-henkan-okurigana
	       ;; $BAw$j2>L>$N$+$J(B prefix $B$r<N$F!"Aw$j2>L>$rB-$7$F(B lookup $B$KEO$9!#(B
	       (setq henkan-key (concat (substring henkan-key 0 (1- (length henkan-key)))
					skk-henkan-okurigana)
		     okuri-process 1))
	      ;; okuri-ari and skk-process-okuri-early
	      (skk-okuri-char
	       ;; $BAw$j2>L>$N$+$J(B prefix $B$r<N$F$F(B lookup $B$KEO$9!#(B
	       (setq henkan-key (substring henkan-key 0 (1- (length henkan-key)))
		     okuri-process 2)))
	(skk-lookup-search-1 module henkan-key okuri-process)))))

(defun skk-lookup-search-1 (module key okuri-process)
  ;; search pattern.
  (let (name method entries pickup-regexp split-regexp
	     candidates-string candidates-list)
    (setq lookup-search-pattern key)
    ;; setup modules.
    (lookup-module-setup module)
    (lookup-foreach
     (lambda (dictionary)
       (when (and (lookup-dictionary-selected-p dictionary)
		  (setq name (lookup-dictionary-name dictionary))
		  (eval (skk-lookup-get-nonsearch-sex name))
		  (setq method (skk-lookup-get-method name okuri-process))
		  ;; valid method or not?
		  (memq method (lookup-dictionary-methods dictionary))
		  ;; actual search.
		  (setq entries (lookup-vse-search-query
				 dictionary
				 (lookup-make-query method lookup-search-pattern))))
	 (setq pickup-regexp (skk-lookup-get-pickup-regexp name)
	       split-regexp (skk-lookup-get-split-regexp name))
	 (lookup-foreach
	  (lambda (entry)
	    ;; pickup necessary string for SKK.
	    (setq candidates-string (lookup-entry-heading entry))
	    (if (not (or pickup-regexp split-regexp))
		(progn
		  (setq candidates-string (skk-lookup-process-okurigana
					   candidates-string
					   okuri-process))
		  (if (and candidates-string
			   (not (string= lookup-search-pattern candidates-string)))
		      (setq candidates-list (cons candidates-string
						  candidates-list))))
	      (setq candidates-list
		    (nconc (skk-lookup-process-heading
			    candidates-string pickup-regexp split-regexp
			    okuri-process)
			   candidates-list))))
	  entries)))
     ;; dictionaries to be searched.
     (lookup-module-dictionaries module))
    (nreverse candidates-list)))

(defun skk-lookup-process-okurigana (string process-type)
  (cond ((string= string "")
	 ;; KOUJIEN has a heading like `$B$^!>$-!Z??LZ!&(B(GAIJI)$B!&Kj![(B'
	 ;; As GAIJI cannot be processed by skk-lookup.el, the heading
	 ;; is equal to `$B$^!>$-!Z??LZ!&!&Kj![(B' for skk-lookup.el.
	 ;; It causes to produce a null string candidate.
	 ;;   (split-string "$B??LZ!&!&Kj(B" "$B!&(B") -> ("$B??LZ(B" "" "$BKj(B")
	 ;; So return nil if STRING is a null string.
	 nil)
	;; okuri-nasi
	((= process-type 0) string)
	;; okuri-ari
	(t
	 (let* ((okuri-length
		 (cond
		  ;; has `skk-henkan-okurigana'.
		  ((= process-type 1) (length skk-henkan-okurigana))
		  ;; `skk-process-okuri-early' is non-nil.
		  ((= process-type 2)
		   ;; don't know exactly how long okurigana is.
		   ;; truncate length of one character anyway.
		   skk-kanji-len)))
		(okurigana (and (> (length string) okuri-length)
				(substring string (- okuri-length)))))
	   (cond (
		  ;; cannot detect okurigana in STRING.
		  (not okurigana) nil)
		 (skk-henkan-okuri-strictly
		  (and (string= skk-henkan-okurigana okurigana)
		       ;; cut okurigana off.
		       (substring string 0 (- okuri-length))))
		 ;; `skk-process-okuri-early' or not `skk-henkan-okuri-strictly'.
		 ((string= (skk-okurigana-prefix okurigana) skk-okuri-char)
		  ;; cut okurigana off.
		  (substring string 0 (- okuri-length))))))))

(defun skk-lookup-process-heading
  (heading pickup-regexp split-regexp okuri-process-type)
  ;; heading $B$7$+<h$j=P$5$J$$$N$O$b$C$?$$$J$$!)(B  $BB>$K$b>pJs$r<h$j=P$7(B
  ;; $B$F$*$$$F!"I,MW$K1~$8$F;2>H$9$k$+!)(B
  (save-match-data
    (do (candidates-string candidates-list)
	((or (string= heading "")
	     (and pickup-regexp (not (string-match pickup-regexp heading))))
	 candidates-list)
      (if pickup-regexp
	  (setq candidates-string (match-string 1 heading)
		heading (substring heading (min (+ (match-end 1) skk-kanji-len)
						(length heading))))
	(setq candidates-string heading
	      heading ""))
      (if split-regexp
	  (lookup-foreach
	   (lambda (c)
	     (if (string= lookup-search-pattern c)
		 nil
	       (setq c (skk-lookup-process-okurigana c okuri-process-type))
	       (if c
		   (setq candidates-list (cons c (delete c candidates-list))))))
	   (split-string candidates-string split-regexp))
	(if (string= lookup-search-pattern candidates-string)
	    nil
	  (setq candidates-string (skk-lookup-process-okurigana
				   candidates-string okuri-process-type))
	  (if candidates-string
	      (setq candidates-list
		    (cons candidates-string
			  (delete candidates-string candidates-list)))))))))

;; The following four functions were imported from lookup.el and
;; lookup-types.el.
(defun skk-lookup-default-module ()
  (or skk-lookup-default-module
      (setq skk-lookup-default-module (car (skk-lookup-module-list)))))

(defun skk-lookup-module-list ()
  (or skk-lookup-module-list
      (setq skk-lookup-module-list
	    (mapcar 'skk-lookup-new-module (or skk-lookup-search-modules
					       '(("%SKK-EVERY" "")))))))
(defun skk-lookup-new-module (spec)
  (let ((name (car spec))
	(id-list (cdr spec))
	module agents match start)
    ;; get agent list
    (lookup-foreach (lambda (id)
		      ;; get the list of agents matched with ID
		      (setq match (concat "^" (regexp-quote id))
			    start agents)
		      (lookup-foreach
		       (lambda (e)
			 (when (string-match match (lookup-agent-id e))
			   (setq agents (cons e agents))))
		       (skk-lookup-agent-list))
		      (when (eq start agents)
			(error "No match agent: %s" id)))
		    ;; get a list of agent-IDs
		    (lookup-nunique
		     (mapcar (lambda (id)
			       (string-match "^[^:]*" id)
			       (substring id 0 (match-end 0)))
			     id-list)))
    (setq agents (nreverse (lookup-nunique agents 'eq)))
    ;; construct module
    (setq module (lookup-make-module name nil))
    (lookup-module-put-property module 'agents agents)
    (lookup-module-put-property module 'id-list id-list)
    (lookup-module-init module)))

(defun skk-lookup-agent-list ()
  (or skk-lookup-agent-list
      (progn
	(if (null skk-lookup-search-agents)
	    ;; copy-list is a C primitive of XEmacs, but FSFmacs has it
	    ;; in cl.el.
	    (setq skk-lookup-search-agents
		  (let ((agents (copy-sequence lookup-search-agents))
			e)
		    ;; use `skk-kakasi.el' instead of ndkks.
		    (setq agents (delete '(ndkks) agents))
		    (while (setq e (assq 'ndcookie agents))
		      (setq agents (delq e agents)))
		    (while (setq e (assq 'ndnmz agents))
		      (setq agents (delq e agents)))
		    agents)))
	(setq skk-lookup-agent-list
	      (mapcar 'lookup-new-agent skk-lookup-search-agents)))))

;; the following two are to check dictionary output of heading for
;; creating new regexp.
(defun skk-lookup-test-regexp (regexp place string)
  "Search STRING by REGEXP and pick up a part of STRING in PLACE."
  (string-match regexp string)
  (match-string place string))

(defun skk-lookup-pickup-headings (pattern method)
  "Search PATTERN by METHOD."
  (let ((module (skk-lookup-default-module))
	(lookup-gaiji-alternate "")
	;;lookup-enable-gaiji ;  not to put out gaiji.
	var)
    (lookup-module-setup module)
    (lookup-foreach
     (lambda (dictionary)
       (lookup-foreach
	(lambda (entry)
	  (setq var (nconc (list
			    (list (lookup-dictionary-name dictionary)
				  (lookup-dictionary-id dictionary)
				  (lookup-entry-heading entry)
				  ;;(lookup-dictionary-command dictionary 'content entry)
				))
			   var)))
	(lookup-vse-search-query
	 dictionary (lookup-make-query method pattern))))
     (lookup-module-dictionaries module))
    var))

(defun skk-lookup-map-prefix-and-kana ()
  (let ((lenv (length skk-lookup-kana-vector))
	(n 0) kana prefix prefix-kana alist)
    (while (> lenv n)
      (setq kana (aref skk-lookup-kana-vector n)
	    prefix (aref skk-kana-rom-vector n)
	    prefix-kana (assoc prefix alist)
	    n (1+ n))
      (if prefix-kana
	  (setcdr prefix-kana (cons kana (cdr prefix-kana)))
	(setq alist (cons (cons prefix (list kana)) alist))))
    alist))

(provide 'skk-lookup)
;;; Local Variables:
;;; End:
;;; skk-lookup.el ends here
