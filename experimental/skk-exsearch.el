;;; skk-exsearch.el --- 外部検索プログラム共用 interface
;; Copyright (C) 2000 Mikio Nakajima <minakaji@osaka.email.ne.jp>

;; Author: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Maintainer: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Version: $Id: skk-exsearch.el,v 1.1.2.1 2000/03/19 14:03:16 minakaji Exp $
;; Keywords: japanese
;; Last Modified: $Date: 2000/03/19 14:03:16 $

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
;; This file needs eieio (which provides CLOS like OO programming) 
;; package that can be found at;
;;
;;    ftp://ftp.ultranet.com/pub/zappo
;;
;; This program may be (or may not be) core engine of external 
;; searching program.
;;
;;; Code:
(eval-when-compile (require 'skk-macs) (require 'skk-vars))

(require 'eieio)

(defclass synchronous-search-engine ()
  ((program :initarg :program
	    :initform nil
	    :documentation "Program file.")
   (infile :initarg :infile
	   :initform nil
	   :documentation
	   "This is where the program's input comes from. (nil means `/dev/null').")
   (stderr :initarg :stderr
	   :initform nil
	   :documentation
	   "What to do with standard error in the child.  nil (discard standard error output), t (mix it with ordinary output), or a file name string.")
   ;;(argument :initarg :argument :initform nil :documentation "")
   (dictionary :initarg :dictionary
	       :initform nil
	       :documentation "Dictionary file to be searched."))
  "External synchronous search engine superclass.")

(defclass asynchronous-search-engine ()
  ((process initarg :process
	    :initform nil
	    :documentation "Process object that belongs to program."))
  "External asynchronous search engine superclass.")

(defclass regular-engine (synchronous-search-engine)
  ((coding-system :initarg :coding-system
		  :initform (lambda () (skk-find-coding-system skk-jisyo-code))))
  "Regular search engine type.
Call program synchronously in separate process.
This type returns a line that contains candidates that are delimited by slash.")

(defclass look-engine (synchronous-search-engine)
  (())
  "look type.
Call program synchronously in separate process.
This type returns multiple lines.  Each line contains a candidate.")

(defclass server-engine (asynchronous-search-engine)
  ((found :initform 1
	  :documentation "A magic number that indicates the server found a candidate.")
   (not-found :initform 4
	      :documentation
	      "A magic number that indicates the server did not find a candidates.")
   (coding-system :initarg :coding-system
		  :initform (lambda () (skk-find-coding-system skk-jisyo-code))))
  "Server search engine type.
Call program asynchronously in separate process.
This type returns a line that contains a magic number and candidates that are
delimited by slash.")

(defvar cdbget (make-instance regular-engine
			      :program "/usr/local/bin/cdbget"
			      :infile "/usr/local/share/skk/SKK-JISYO.L.cdb")
  "*cdbget search engine object.")

(defvar look (make-instance look-engine :program "/usr/bin/look")
  "*look search engine object.")
			       
(defmethod core-engine ((engine synchronous-search-engine) argument)
  (save-excursion
    (and (= 0 (apply 'call-process (oref engine program)
		     (oref engine infile)
		     t			; output to current buffer.
		     (cons t (oref engine stderr))
		     argument))
	 (> (buffer-size) 0))))
		       
(defmethod search-engine ((engine regular-engine) &rest argument)
  ;; core search engine
  (let ((okurigana (or skk-henkan-okurigana skk-okuri-char))
	l)
    (with-temp-buffer 
      (and (core-engine engine argument)
	   (setq l (skk-compute-henkan-lists okurigana))
	   (cond ((and okurigana skk-henkan-okuri-strictly)
		  (nth 2 l))
		 ((and okurigana skk-henkan-strict-okuri-precedence)
		  (skk-nunion (nth 2 l) (car l)))
		 (t (car l)))))))

(defmethod search-engine ((engine look-engine) &rest argument)
  (with-temp-buffer 
    (let (opt)
      (and (oref engine dictionary) 
	   (nconc argument (list (oref engine dictionary))))
      (and skk-look-dictionary-order (setq opt "d"))
      (and skk-look-ignore-case (setq opt (concat "f" opt)))
      (and skk-look-use-alternate-dictionary
	   (setq opt (concat "a" opt)) )
      (and opt (setq argument (cons (concat "-" opt) argument)))
      (and skk-look-termination-character
	   (setq argument
		 (cons (list "-t" skk-look-termination-character) argument) ))
      (and (core-engine engine argument)
	   (split-string (buffer-substring-no-properties (point-min) (1- (point-max)))
			 "\n" )))))

(if (fboundp 'modify-coding-system-alist)
    (modify-coding-system-alist
     'process (oref cdbget program)
     (cons (oref cdbget coding-system) (oref cdbget coding-system))))

;;;###autoload
(defun skk-cdbget-search ()
  (search-engine cdbget skk-henkan-key))

;;;###autoload
(defun skk-look-search ()
  (and skk-abbrev-mode
       (eq (skk-str-ref skk-henkan-key (1- (length skk-henkan-key))) ?*)
       (let ((args (substring skk-henkan-key 0 (1- (length skk-henkan-key))))
	     v )
	 (setq v (search-engine look args))
	 (if (not skk-look-recursive-search)
	     v
	   (let (skk-henkan-key v2 v3)
	     (while v
	       (let ((skk-current-search-prog-list
		      (delete '(skk-look) (copy-sequence skk-search-prog-list)) ))
		 (setq skk-henkan-key (car v))
		 (while skk-current-search-prog-list
		   (setq v3 (skk-search)
			 v2 (if (not skk-look-expanded-word-only)
				(skk-nunion v2 (cons (car v) v3))
			      (if v3
				  (skk-nunion v2 (cons (car v) v3))
				v2 )))))
	       (setq v (cdr v)) )
	     v2 )))))

(provide 'skk-exsearch)
;;; skk-exsearch.el ends here
