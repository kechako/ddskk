;;; skk-mkmgk.el --- Make Mazegaki dictionary from SKK-JISYO.*
;; Copyright (C) 2001 NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>

;; Author: NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Version: $Id: skk-mkmgk.el,v 1.4 2001/07/28 10:51:00 minakaji Exp $
;; Keywords: japanese
;; Last Modified: $Date: 2001/07/28 10:51:00 $

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

;;; Commentary
;;
;; M-x skk-make-mazegaki-dic ���ƽ��Ϥ��줿���� (��������Ǥ� TMP) ��
;;
;;   $ skkdic-expr TMP | skkdic-sort > NEWDICT
;;
;; �Ȳù�����Ⱥ����� SKK ���� (NEWDICT) ������夬��ޤ���

;;; Code:
(eval-when-compile (require 'skk-macs))
(require 'skk)

(defvar skk-mkmgk-region-limit 10000
  "*�����꡼�����Υ����������ο���꾮�������ˡ��Х��ʥꥵ������ߤ��˥��������˰ܹԤ��롣
�Ŀͼ����ù�������ϡ�0 �����ꤹ�롣")

;;;###autoload
(defun skk-make-mazegaki-dic (dic &optional nomsg)
  "SKK �����μ��񤫤� TUT-Code �ʤɤǻȤ���뺮���񤭼�����롣
���Ϥ��줿���� (��������Ǥ� TMP) ��

  $ skkdic-expr TMP | skkdic-sort > NEWDICT

�Ȥ��Ʋù�����ɬ�פ��ꡣ"
  (interactive "f����ե�����:\nP ")
  (let ((cont t)
	(workbuf (get-buffer-create " *skkmkmgk working*"))
	max output)
    (while cont
      (setq output (read-file-name
		    "�ɤ��˽��Ϥ��ޤ���? "
		    nil
		    (convert-standard-filename "~/.skk-jisyo.tmp")))
      (if (or (not (file-exists-p output))
	      (yes-or-no-p
	       (format "%s �˾�񤭤��Ƥ�����Ǥ���? " output)))
	  (setq cont nil)))
    (with-current-buffer workbuf (erase-buffer))
    (with-temp-buffer
      (insert-file-contents-as-coding-system
       (cdr (assoc "euc" skk-coding-system-alist))
       (expand-file-name dic))
      (re-search-forward "^;; okuri-nasi entries\\.$")
      ;; abbrev ����� skip
      (delete-region (point-min) (progn (re-search-forward "^��")
					(beginning-of-line)
					;;(forward-char -1)
					(point)))
      (skk-make-mazegaki-dic-1 workbuf nomsg))
    (with-current-buffer workbuf
      (if (not (> (buffer-size) 0))
	  (or nomsg
	      (message "No entries of Mazegaki dictionary"))
	(write-region-as-coding-system
	 (cdr (assoc "euc" skk-coding-system-alist))
	 ;; only Emacs 21's write-region has 6th arg MUSTBENEW.
	 1 (point-max) output nil t nil)
	(or nomsg
	    (message "Making Mazegaki dictionary...100%% done"))))
    (kill-buffer workbuf)))

;;;###autoload
(defun skk-make-mazegaki-dic-region (min max &optional nomsg)
  "SKK �����μ��񤫤� TUT-Code �ʤɤǻȤ���뺮���񤭼�����롣
�ù�����������ȥХåե��˽񤭽Ф� `pop-to-buffer' ���롣
���Ϥ��줿�Хåե���ե����� (��������Ǥ� TMP) ����¸��

  $ skkdic-expr TMP | skkdic-sort > NEWDICT

�Ȥ��Ʋù�����ɬ�פ��ꡣ"
  (interactive "r\nP")
  (let ((outbuf (get-buffer-create " *skkmkmgk working*")))
    (save-excursion
      (save-restriction
	(narrow-to-region min max)
	(goto-char min)
	(skk-make-mazegaki-dic-1 outbuf nomsg)))
    (pop-to-buffer outbuf)
    (goto-char (point-min))))
 
(defun skk-make-mazegaki-dic-1 (outbuf nomsg)
  (let ((max (point-max))
	(cont t)
	header0 header-list candidates0 candidates1)
    (while (and cont (not (eobp)))
      (or nomsg
	  (message (format "Making Mazegaki dictionary...%d%%%% done"
			   (* (/ (* (point) 100.00) (* max 100.00))
			      100.0))))
      (beginning-of-line)
      ;; �Ҥ餬�ʸ��Ф� e.x. "��������"
      (setq header0 (buffer-substring-no-properties
		     (point) (save-excursion (search-forward " ")
					     (backward-char 1) (point))))
      (if (= (skk-str-length header0) 1)
	  nil
	(search-forward " /")
	;; �Ҥ餬�ʸ��Ф��򥭡��ˤ�������ꥹ�� (2 ʸ���ʾ�) e.x. "����"
	(setq candidates0 (skk-mkmgk-filter
			   (car (skk-compute-henkan-lists nil))))
	(if (null candidates0)
	    nil
	  ;; �Ҥ餬�ʸ��Ф���ʬ��
	  (setq header-list (string-to-char-list header0))
	  (while header-list
	    (let* ((header-list1 header-list)
		   (header1 (char-to-string (car header-list1)))
		   (n 1))
	      (while (and header1 (> (length header0) (length header1)))
		(save-excursion
		  (goto-char (point-min))
		  ;; ʬ�򤷤��Ҥ餬�ʸ��Ф��� 1 ʸ���Ťĸ��Ф��ˤ��ƺƸ��� e.x. "����"
		  (if (not (setq candidates1 (skk-mkmgk-binary-search
					      header1 (point-min) max skk-mkmgk-region-limit)))
		      nil
		    (let ((can0 candidates0))
		      (while (and candidates1 can0)
			(if (not (string-match (car candidates1) (car can0)))
			    (or (setq candidates1 (cdr candidates1))
				(setq can0 (cdr can0)))
			  ;; XXX
			  ;; "��������" �Τ褦����Ǥϡ�"����" �� "����" ��
			  ;; 2 �� match ����Τǡ�Ʊ����Τ���Ϥ��Ƥ��ޤ���
			  ;; ������ˤ��� skkdic-expr & skkdic-sort ���ʤ�
			  ;; ��� SKK ����Ȥ��ƻȤ��ʤ��Τǡ��ޤ����ä�...��
			  (with-current-buffer outbuf
			    ;; ʬ�򥭡� (e.x. ����) �ˤ����� e.x. "��" ��
			    ;; ���ꥸ�ʥ륭�� (e.x. ��������) �θ��� e.x.
			    ;; "����" �� match ������
			    (goto-char (point-max))
			    ;; ������ /����/
			    (insert (substring (car can0) 0 (match-beginning 0))
				    header1
				    (substring (car can0) (match-end 0))
				    " /" (car can0) "/\n")
			    (if (string-match header1 header0)
				(insert
				 ;; �꤫�� /����/
				 (substring header0 0 (match-beginning 0))
				 (car candidates1)
				 (substring header0 (match-end 0))
				 " /" (car can0) "/\n")))
			  (setq candidates1 nil)))))
		  (if (> (skk-str-length
			  (mapconcat 'char-to-string header-list1 nil)) n)
		      ;; HEADER1 �򿭤Ф��ƺƸ���
		      ;; e.x. "��" �� "����" �� "������" �� "��������"
		      ;;      "��" �� "����" �� "������"
		      ;;      "��" �� "����"
		      ;;      "��"
		      (setq header1 (concat
				     header1
				     (char-to-string (nth n header-list1)))
			    n (1+ n))
		    ;; ���� char ��Ƭ�ˤ�����������
		    ;; e.x. "��" �� "��" �� "��" �� "��"
		    (setq header1 nil))))
	      (setq header-list (cdr header-list1))))))
      (setq cont (= (forward-line 1) 0)))
    (or nomsg
	(message "Making Mazegaki dictionary...100%% done"))))

(defun skk-mkmgk-filter (list)
  (delq nil (mapcar
	     (function
	      (lambda (word)
		(if (and
		     ;; ���� 1 ʸ���򺮤��񤭤��뤳�ȤϤʤ��Ǥ��礦...
		     (> (skk-str-length word) 1)
		     ;; �������ʸ�� skip
		     (not (string-match "^[����-��]+$" word))
		     ;; �Ѹ�� skip
		     (not (string-match "^[a-zA-Z]+$" word)))
		    (if (string-match ";" word)
			(substring word 0 (match-beginning 0))
		      word))))
	     list)))

(defun skk-mkmgk-binary-search (key min max limit)
  (let ((case-fold-search nil)
        size p)
    (if (> limit 0)
	(while (progn (setq size (- max min)) (> size limit))
	  (goto-char (+ min (/ size 2)))
	  (beginning-of-line)
	  (setq p (point))
	  (if (string< key (buffer-substring-no-properties
			    p (1- (search-forward " "))))
	      (setq max p)
	    (setq min p))))
    (goto-char min)
    (beginning-of-line)
    (if (re-search-forward (concat "^" key " /") max 'noerror)
	(skk-mkmgk-filter (car (skk-compute-henkan-lists nil))))))

(require 'product)
(product-provide (provide 'skk-mkmgk) (require 'skk-version))
;; end of skk-mkmgk.el
