;;; skk-hankaku-mode.el --- SKK �� JISX 0201 �������ϥץ����
;; Copyright (C) 1999 Tsukamoto Tetsuo <czkmt@remus.dti.ne.jp>

;; Author: Tsukamoto Tetsuo <czkmt@remus.dti.ne.jp>
;; Version: $Id: skk-hankaku-mode.el,v 1.2 1999/10/31 08:33:50 minakaji Exp $
;; Keywords: japanese
;; Last Modified: $Date: 1999/10/31 08:33:50 $

;; This file is not part of SKK yet.

;; SKK is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either versions 2, or (at your option)
;; any later version.

;; SKK is distributed in the hope that it will be useful
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with SKK, see the file COPYING.  If not, write to the Free
;; Software Foundation Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary:
;;
;; ~/.skk ��
;;
;; (require 'skk-hankaku-mode)
;;
;; �Ƚ񤯤��Ȥǥ��󥹥ȡ��뤵��ޤ����Ȥ����ϰʲ��Τ褦�ˤʤ�ޤ���
;;
;; ���Ҥ餬�ʥ⡼�ɤˤ����ơ�
;;   ��"qq" �����ѥ��ʥ⡼�ɤˤʤ�ޤ���
;;   ��"qa" ��Ⱦ�ѥ��ʥ⡼�ɤˤʤ�ޤ���
;; ���������ʥ⡼�ɤˤ����ơ�
;;   ��"qq" �ǤҤ餬�ʥ⡼�ɤˤʤ�ޤ���
;;   ��"qs" �����ѥ��ʥ⡼�ɤ�Ⱦ�ѥ��ʥ⡼�ɤ��ڤ꤫���ޤ���
;; ���Ҥ餬��/��������ξ�⡼����Ǥ΢��⡼�ɤˤ����ơ�
;;   ���Ҥ餬��/�������ʤΥȥ����Ѵ��� "q" �ǤϤʤ� "qq" �ǹԤ��ޤ���
;;   ��"qs" �򲡤��Ȏ����Ф���Ȥ������Ϥ��줿�Ҥ餬��/�������ʤ�ʎݎ����������Ť��Ѵ����ޤ���
;;
;; ����¾�Ͻ����̤�Ȥ��ޤ���

;;; Code:
(require 'skk)

(defvar skk-hankaku-rule-list
  '(("a" nil ("��" . "��"))
    ("bb" "b" ("��" . "��"))
    ("ba" nil ("�ʎ�" . "��"))
    ("be" nil ("�͎�" . "��"))
    ("bi" nil ("�ˎ�" . "��"))
    ("bo" nil ("�Ύ�" . "��"))
    ("bu" nil ("�̎�" . "��"))
    ("bya" nil ("�ˎގ�" . "�Ӥ�"))
    ("bye" nil ("�ˎގ�" . "�Ӥ�"))
    ("byi" nil ("�ˎގ�" . "�Ӥ�"))
    ("byo" nil ("�ˎގ�" . "�Ӥ�"))
    ("byu" nil ("�ˎގ�" . "�Ӥ�"))
    ("cc" "c" ("��" . "��"))
    ("cha" nil ("����" . "����"))
    ("che" nil ("����" . "����"))
    ("chi" nil ("��" . "��"))
    ("cho" nil ("����" . "����"))
    ("chu" nil ("����" . "����"))
    ("cya" nil ("����" . "����"))
    ("cye" nil ("����" . "����"))
    ("cyi" nil ("����" . "����"))
    ("cyo" nil ("����" . "����"))
    ("cyu" nil ("����" . "����"))
    ("dd" "d" ("��" . "��"))
    ("da" nil ("����" . "��"))
    ("de" nil ("�Î�" . "��"))
    ("dha" nil ("�Îގ�" . "�Ǥ�"))
    ("dhe" nil ("�Îގ�" . "�Ǥ�"))
    ("dhi" nil ("�Îގ�" . "�Ǥ�"))
    ("dho" nil ("�Îގ�" . "�Ǥ�"))
    ("dhu" nil ("�Îގ�" . "�Ǥ�"))
    ("di" nil ("����" . "��"))
    ("do" nil ("�Ď�" . "��"))
    ("du" nil ("��" . "��"))
    ("dya" nil ("���ގ�" . "�¤�"))
    ("dye" nil ("���ގ�" . "�¤�"))
    ("dyi" nil ("���ގ�" . "�¤�"))
    ("dyo" nil ("���ގ�" . "�¤�"))
    ("dyu" nil ("���ގ�" . "�¤�"))
    ("e" nil ("��" . "��"))
    ("ff" "f" ("��" . "��"))
    ("fa" nil ("�̎�" . "�դ�"))
    ("fe" nil ("�̎�" . "�դ�"))
    ("fi" nil ("�̎�" . "�դ�"))
    ("fo" nil ("�̎�" . "�դ�"))
    ("fu" nil ("��" . "��"))
    ("fya" nil ("�̎�" . "�դ�"))
    ("fye" nil ("�̎�" . "�դ�"))
    ("fyi" nil ("�̎�" . "�դ�"))
    ("fyo" nil ("�̎�" . "�դ�"))
    ("fyu" nil ("�̎�" . "�դ�"))
    ("gg" "g" ("��" . "��"))
    ("ga" nil ("����" . "��"))
    ("ge" nil ("����" . "��"))
    ("gi" nil ("����" . "��"))
    ("go" nil ("����" . "��"))
    ("gu" nil ("����" . "��"))
    ("gya" nil ("���ގ�" . "����"))
    ("gye" nil ("���ގ�" . "����"))
    ("gyi" nil ("���ގ�" . "����"))
    ("gyo" nil ("���ގ�" . "����"))
    ("gyu" nil ("���ގ�" . "����"))
    ;;("h" "" ("��" . "��"))
    ("ha" nil ("��" . "��"))
    ("he" nil ("��" . "��"))
    ("hi" nil ("��" . "��"))
    ("ho" nil ("��" . "��"))
    ("hu" nil ("��" . "��"))
    ("hya" nil ("�ˎ�" . "�Ҥ�"))
    ("hye" nil ("�ˎ�" . "�Ҥ�"))
    ("hyi" nil ("�ˎ�" . "�Ҥ�"))
    ("hyo" nil ("�ˎ�" . "�Ҥ�"))
    ("hyu" nil ("�ˎ�" . "�Ҥ�"))
    ("i" nil ("��" . "��"))
    ("jj" "j" ("��" . "��"))
    ("ja" nil ("���ގ�" . "����"))
    ("je" nil ("���ގ�" . "����"))
    ("ji" nil ("����" . "��"))
    ("jo" nil ("���ގ�" . "����"))
    ("ju" nil ("���ގ�" . "����"))
    ("jya" nil ("���ގ�" . "����"))
    ("jye" nil ("���ގ�" . "����"))
    ("jyi" nil ("���ގ�" . "����"))
    ("jyo" nil ("���ގ�" . "����"))
    ("jyu" nil ("���ގ�" . "����"))
    ("kk" "k" ("��" . "��"))
    ("ka" nil ("��" . "��"))
    ("ke" nil ("��" . "��"))
    ("ki" nil ("��" . "��"))
    ("ko" nil ("��" . "��"))
    ("ku" nil ("��" . "��"))
    ("kya" nil ("����" . "����"))
    ("kye" nil ("����" . "����"))
    ("kyi" nil ("����" . "����"))
    ("kyo" nil ("����" . "����"))
    ("kyu" nil ("����" . "����"))
    ("mm" "c" ("��" . "��"))
    ("ma" nil ("��" . "��"))
    ("me" nil ("��" . "��"))
    ("mi" nil ("��" . "��"))
    ("mo" nil ("��" . "��"))
    ("mu" nil ("��" . "��"))
    ("mya" nil ("�Ў�" . "�ߤ�"))
    ("mye" nil ("�Ў�" . "�ߤ�"))
    ("myi" nil ("�Ў�" . "�ߤ�"))
    ("myo" nil ("�Ў�" . "�ߤ�"))
    ("myu" nil ("�Ў�" . "�ߤ�"))
    ("n" nil ("��" . "��"))
    ("n'" nil ("��" . "��"))
    ("na" nil ("��" . "��"))
    ("ne" nil ("��" . "��"))
    ("ni" nil ("��" . "��"))
    ("nn" nil ("��" . "��"))
    ("no" nil ("��" . "��"))
    ("nu" nil ("��" . "��"))
    ("nya" nil ("�Ǝ�" . "�ˤ�"))
    ("nye" nil ("�Ǝ�" . "�ˤ�"))
    ("nyi" nil ("�Ǝ�" . "�ˤ�"))
    ("nyo" nil ("�Ǝ�" . "�ˤ�"))
    ("nyu" nil ("�Ǝ�" . "�ˤ�"))
    ("o" nil ("��" . "��"))
    ("pp" "p" ("��" . "��"))
    ("pa" nil ("�ʎ�" . "��"))
    ("pe" nil ("�͎�" . "��"))
    ("pi" nil ("�ˎ�" . "��"))
    ("po" nil ("�Ύ�" . "��"))
    ("pu" nil ("�̎�" . "��"))
    ("pya" nil ("�ˎߎ�" . "�Ԥ�"))
    ("pye" nil ("�ˎߎ�" . "�Ԥ�"))
    ("pyi" nil ("�ˎߎ�" . "�Ԥ�"))
    ("pyo" nil ("�ˎߎ�" . "�Ԥ�"))
    ("pyu" nil ("�ˎߎ�" . "�Ԥ�"))
    ("rr" "r" ("��" . "��"))
    ("ra" nil ("��" . "��"))
    ("re" nil ("��" . "��"))
    ("ri" nil ("��" . "��"))
    ("ro" nil ("��" . "��"))
    ("ru" nil ("��" . "��"))
    ("rya" nil ("�؎�" . "���"))
    ("rye" nil ("�؎�" . "�ꤧ"))
    ("ryi" nil ("�؎�" . "�ꤣ"))
    ("ryo" nil ("�؎�" . "���"))
    ("ryu" nil ("�؎�" . "���"))
    ("ss" "s" ("��" . "��"))
    ("sa" nil ("��" . "��"))
    ("se" nil ("��" . "��"))
    ("sha" nil ("����" . "����"))
    ("she" nil ("����" . "����"))
    ("shi" nil ("��" . "��"))
    ("sho" nil ("����" . "����"))
    ("shu" nil ("����" . "����"))
    ("si" nil ("��" . "��"))
    ("so" nil ("��" . "��"))
    ("su" nil ("��" . "��"))
    ("sya" nil ("����" . "����"))
    ("sye" nil ("����" . "����"))
    ("syi" nil ("����" . "����"))
    ("syo" nil ("����" . "����"))
    ("syu" nil ("����" . "����"))
    ("tt" "t" ("��" . "��"))
    ("ta" nil ("��" . "��"))
    ("te" nil ("��" . "��"))
    ("tha" nil ("�Î�" . "�Ƥ�"))
    ("the" nil ("�Î�" . "�Ƥ�"))
    ("thi" nil ("�Î�" . "�Ƥ�"))
    ("tho" nil ("�Î�" . "�Ƥ�"))
    ("thu" nil ("�Î�" . "�Ƥ�"))
    ("ti" nil ("��" . "��"))
    ("to" nil ("��" . "��"))
    ("tsu" nil ("��" . "��"))
    ("tu" nil ("��" . "��"))
    ("tya" nil ("����" . "����"))
    ("tye" nil ("����" . "����"))
    ("tyi" nil ("����" . "����"))
    ("tyo" nil ("����" . "����"))
    ("tyu" nil ("����" . "����"))
    ("u" nil ("��" . "��"))
    ("vv" "v" ("��" . "��"))
    ("va" nil ("���ގ�" . "���ޤ�"))
    ("ve" nil ("���ގ�" . "���ޤ�"))
    ("vi" nil ("���ގ�" . "���ޤ�"))
    ("vo" nil ("���ގ�" . "���ޤ�"))
    ("vu" nil ("����" . "����"))
    ("ww" "w" ("��" . "��"))
    ("wa" nil ("��" . "��"))
    ("we" nil ("����" . "����"))
    ("wi" nil ("����" . "����"))
    ("wo" nil ("��" . "��"))
    ("wu" nil ("��" . "��"))
    ("xx" "x" ("��" . "��"))
    ("xa" nil ("��" . "��"))
    ("xe" nil ("��" . "��"))
    ("xi" nil ("��" . "��"))
    ("xka" nil ("��" . "��"))
    ("xke" nil ("��" . "��"))
    ("xo" nil ("��" . "��"))
    ("xtsu" nil ("��" . "��"))
    ("xtu" nil ("��" . "��"))
    ("xu" nil ("��" . "��"))
    ("xwa" nil ("��" . "��"))
    ("xwe" nil ("��" . "��"))
    ("xwi" nil ("��" . "��"))
    ("xya" nil ("��" . "��"))
    ("xyo" nil ("��" . "��"))
    ("xyu" nil ("��" . "��"))
    ("yy" "y" ("��" . "��"))
    ("ya" nil ("��" . "��"))
    ("ye" nil ("����" . "����"))
    ("yo" nil ("��" . "��"))
    ("yu" nil ("��" . "��"))
    ("zz" "z" ("��" . "��"))
    ("z," nil "��")
    ("z-" nil "��")
    ("z." nil "��")
    ("z/" nil "��")
    ("z[" nil "��")
    ("z]" nil "��")
    ("za" nil ("����" . "��"))
    ("ze" nil ("����" . "��"))
    ("zh" nil "��")
    ("zi" nil ("����" . "��"))
    ("zj" nil "��")
    ("zk" nil "��")
    ("zl" nil "��")
    ("zo" nil ("����" . "��"))
    ("zu" nil ("����" . "��"))
    ("zya" nil ("���ގ�" . "����"))
    ("zye" nil ("���ގ�" . "����"))
    ("zyi" nil ("���ގ�" . "����"))
    ("zyo" nil ("���ގ�" . "����"))
    ("zyu" nil ("���ގ�" . "����"))
    ("," nil "��")
    ("." nil "��")
    ("-" nil "��")
    (":" nil ":")
    (";" nil ";")
    ("?" nil "?")
    ("[" nil "��")
    ("]" nil "��")
    ("l" nil skk-latin-mode)
;;    ("q" nil skk-toggle-kana)
    ("L" nil skk-jisx0208-latin-mode)
    ("Q" nil skk-set-henkan-point-subr)
    ("X" nil skk-purge-from-jisyo)
    ("/" nil skk-abbrev-mode)
    ("$" nil skk-display-code-for-char-at-point)
    ("@" nil skk-today)
    ("\\" nil skk-input-by-code-or-menu)
    )
  "*SKK Ⱦ�ѥ⡼�ɤΥ롼�롣")

(defvar skk-hankaku-added-base-rule-list
  '(("qq" nil skk-toggle-kana-zenkaku)
    ("qa" nil skk-toggle-kana-hankaku)
    ("qs" nil skk-toggle-zenkaku-hankaku)
    )
  "SKK Ⱦ�ѥ⡼�ɤΤ���� skk-j-mode ���̤Υ��������")

(defvar skk-original-katakana-mode-string nil)
(defvar skk-hankaku-mode-string " ����")

(add-hook 'skk-mode-hook
	  (function
	   (lambda ()
	     (or skk-original-katakana-mode-string
		 (setq skk-original-katakana-mode-string
		       skk-katakana-mode-string))
	     (if (and (string= skk-hankaku-mode-string " ����")
		      (string= skk-original-katakana-mode-string "--����:"))
		 (setq skk-hankaku-mode-string "--����:")))))

(and (assoc "q" skk-rom-kana-base-rule-list)
     (delete (assoc "q" skk-rom-kana-base-rule-list) 
	     skk-rom-kana-base-rule-list))

(add-hook 'skk-mode-hook
	  (function
	   (lambda ()
	     (setq skk-rule-tree
		   (skk-compile-rule-list
		    skk-rom-kana-base-rule-list skk-rom-kana-rule-list
		    skk-hankaku-added-base-rule-list))))
	  t)

(defvar skk-hankaku-stat nil)

(defun skk-toggle-kana-zenkaku (arg)
  (interactive)
  (setq skk-rule-tree
	(skk-compile-rule-list
	 skk-rom-kana-base-rule-list skk-rom-kana-rule-list
	 skk-hankaku-added-base-rule-list) )
  (setq skk-katakana-mode-string skk-original-katakana-mode-string)
  (skk-toggle-kana arg)
  (setq skk-hankaku-stat nil))

(defun skk-toggle-kana-hankaku (arg)
  (interactive)
  (setq skk-rule-tree
	(skk-compile-rule-list
	 skk-hankaku-rule-list skk-rom-kana-rule-list
	 skk-hankaku-added-base-rule-list))
  (setq skk-katakana-mode-string skk-hankaku-mode-string)
  (skk-toggle-kana arg)
  (setq skk-hankaku-stat t))

(defun skk-toggle-zenkaku-hankaku (&optional arg)
  (interactive)
  (if skk-hankaku-stat
      (progn
	(setq skk-rule-tree
	      (skk-compile-rule-list
	       skk-rom-kana-base-rule-list skk-rom-kana-rule-list
	       skk-hankaku-added-base-rule-list))
	(setq skk-katakana-mode-string skk-original-katakana-mode-string)
	(setq skk-hankaku-stat nil))
    (if (and skk-henkan-on (not skk-henkan-active))
	(skk-hankaku-henkan arg) )
    (setq skk-rule-tree
	  (skk-compile-rule-list
	   skk-hankaku-rule-list skk-rom-kana-rule-list
	   skk-hankaku-added-base-rule-list))
    (setq skk-katakana-mode-string skk-hankaku-mode-string)
    (setq skk-hankaku-stat t))
  (and skk-katakana (setq skk-input-mode-string skk-katakana-mode-string))
  (force-mode-line-update)
  nil)

(defun skk-hankaku-henkan (arg)
  "���⡼�ɤǤ���С��꡼�����ΤҤ餬��/�������ʤ�ʎݎ����������Ť��Ѵ����롣
���⡼�ɤǤϲ��⤷�ʤ���
����¾�Υ⡼�ɤǤϡ����ꥸ�ʥ�Υ�������դ��ǥХ���ɤ���Ƥ��륳�ޥ�ɤ�¹�
���롣"
  (interactive "*P")
  (skk-with-point-move
   (if skk-henkan-on
       (if skk-henkan-active
	   nil
	 (skk-set-marker skk-henkan-end-point (point))
	 (skk-*-henkan-1 'skk-hankaku-region skk-henkan-start-point
			 skk-henkan-end-point 'vcontract ))
     (skk-emulate-original-map arg) )))

(defun skk-hankaku-region (start end &optional vcontract)
  "�꡼�����ΤҤ餬��/�������ʤ�ʎݎ����������Ť��Ѵ����롣
���ץ���ʥ������ VCONTRACT �� non-nil �Ǥ���С�\"����\" �� \"����\" ���Ѵ���
�롣
������ START �� END �Ͽ����Ǥ�ޡ������Ǥ��ɤ���"
  (interactive "*r\nP")
  (setq end (set-marker (make-marker) end))
  (skk-hiragana-to-hankaku-region start end vcontract)
  (skk-katakana-to-hankaku-region start end vcontract)
  (set-marker end nil)
  (skk-set-cursor-properly) )

(defun skk-hiragana-to-hankaku-region (start end &optional vcontract)
  (save-match-data
    (let (object hankaku)
      (skk-save-point
       (goto-char start)
       (while (re-search-forward  "[��-��]+" end 'noerror)
	 (setq object (buffer-substring-no-properties
		       (match-beginning 0) (match-end 0) )
	       hankaku (save-match-data (japanese-hankaku object)) )
	 (backward-char (skk-str-length object))
	 ;; firstly insert a new string, secondly delete an old string to save
	 ;; the cursor position.
	 (insert-and-inherit hankaku)
	 (delete-region (+ (match-beginning 0) (length hankaku))
			(+ (match-end 0) (length hankaku)) ))
       (if vcontract
	   (progn
	     (goto-char start)
	     (while (re-search-forward  "����" end 'noerror)
	       (backward-char (skk-str-length "����"))
	       (let ((vu-len (length "����")))
		 (insert-and-inherit "����")
		 (delete-region (+ (match-beginning 0) vu-len)
				(+ (match-end 0) vu-len) )))))))))
       
(defun skk-katakana-to-hankaku-region (start end &optional vcontract)
  (save-match-data
    (let (object hankaku)
      (skk-save-point
       (goto-char start)
       (while (re-search-forward  "[��-��]+" end 'noerror)
	 (setq object (buffer-substring-no-properties
		       (match-beginning 0) (match-end 0) )
	       hankaku (save-match-data (japanese-hankaku object)) )
	 (backward-char (skk-str-length object))
	 ;; firstly insert a new string, secondly delete an old string to save
	 ;; the cursor position.
	 (insert-and-inherit hankaku)
	 (delete-region (+ (match-beginning 0) (length hankaku))
			(+ (match-end 0) (length hankaku)) ))
       (if vcontract
	   (progn
	     (goto-char start)
	     (while (re-search-forward  "����" end 'noerror)
	       (backward-char (skk-str-length "����"))
	       (let ((vu-len (length "����")))
		 (insert-and-inherit "����")
		 (delete-region (+ (match-beginning 0) vu-len)
				(+ (match-end 0) vu-len) )))))))))
       
(provide 'skk-hankaku-mode)
;;; Local Variables:
;;; End:
;;; skk-hankaku-mode.el ends here
