;;; skk-kanagaki.el --- SKK の仮名入力サポート
;; Copyright (C) 2000 Tetsuo Tsukamoto <czkmt@remus.dti.ne.jp>

;; Author: Tetsuo Tsukamoto <czkmt@remus.dti.ne.jp>
;; Version: $Id: skk-kanagaki.el,v 1.1.2.4 2000/08/09 09:41:38 czkmt Exp $
;; Keywords: japanese
;; Last Modified: $Date: 2000/08/09 09:41:38 $

;; This file is not yet part of Daredevil SKK.

;; Daredevil SKK  is free software;  you  can redistribute it  and/or modify it
;; under the terms  of the GNU General Public License  as published by the Free
;; Software  Foundation;  either versions  2,  or  (at your option)  any  later
;; version.

;; Daredevil SKK is distributed in the hope that it will be useful  but WITHOUT
;; ANY  WARRANTY;  without  even  the implied  warranty  of MERCHANTABILITY  or
;; FITNESS  FOR  A PARTICULAR PURPOSE.  See the GNU General Public License  for
;; more details.

;; You should have received a copy of the GNU General Public License along with
;; Daredevil SKK,  see the file COPYING.  If not,  write  to  the Free Software
;; Foundation Inc., 59 Temple Place - Suite 330, Boston,  MA 02111-1307, USA.

;;; Commentary:
;;
;; {てっとり早い使いかた (暫定バージョン)}
;;
;; ~/.skk に
;;
;; (require 'skk-kanagaki)
;;
;; と書く (将来的には require する必要はなくなる、と思う)。
;;
;;
;; {説明}
;;
;; このプログラムは  SKK においてローマ字入力ならぬ仮名入力をサポートすることを
;; 目的とします。 AT 互換機用の日本語 106 キーボードは普通 JIS 配列の刻印があり
;; ますが、まずはこれに対応する予定です。PC-98 への対応はこれを少し変更すればで
;; きると思います。
;;
;;  -*- 問題点 -*-
;;
;; 1. Emacs Lisp のレベルでの問題
;;
;; 仮名入力においては SHIFT キーを利用して入力される仮名もあるため、 SKK 本来の
;; SHIFT の使い方ができません。その他いろいろ SKK らしくないのですが、 とりあえ
;; ず、
;;
;;   o 変換開始点の指定は仮名入力とは別に行う。
;;   o 変換の開始は通常通り、 [SPC] で指示する。 ただし、送りありの変換のときは
;;     C-u [SPC] として明示する。
;;
;; のようにしてあります。例えば、「嬉しい」を入力するためには、
;;
;; [f2] うれし C-u [SPC] い
;;
;; のように打ちます。
;; (改善の余地があると思いますが、とりあえずアイデアがここで尽きています。)
;;
;; 2. システムレベルでの問題
;;
;; 第 2 の問題点として、 キーシンボルの設定により刻印通りの入力ができない場合が
;; あります。例えば日本語 106 キーボード使用時、XFree86 上では
;;
;; o 「￥」キー (仮想キーコード 133)
;; o 「＼」キー (仮想キーコード 123)
;;
;; はいずれも backslash として扱われます。 しかし仮名入力において前者は 「ー」、
;; 後者は「ろ」 となることが望まれます。この場合の対応策として、例えば
;;
;; % cat >> ~/.Xmodmap
;;
;; keycode 123 = underscore underscore
;; % xmodmap ~/.Xmodmap
;;
;; などとしておいてから、~/.skk に
;; 
;; (setq skk-kanagaki-rule-list
;;       '(("\\" nil "ー")))
;;
;; と書くことなどが考えられます。
;; (同様のアイデアは Canna で仮名入力する際にも有効であるようです。)
;;
;; もしあなたが XEmacs のベータテスターならば
;;
;; keycode 123 = kana_RO underscore
;; keycode 19 = 0 kana_WO
;;
;; なんて設定でとても幸せになれるかもしれません。 (Mr. XEmacs のしわざかな?)
;;
;; さらに、もしあなたが PC-98 ユーザ で XEmacs のベータテスターならば、おもむろ
;; に「かな」キーをロックしてみてください。 ;')
;;
;;  -*- 使い方 -*-
;;
;; 送りなしの変換については、通常の SKK と同様なので省略します。
;;
;; 1. 変換開始点の指定
;;
;; 通常の SKK においては、 SHIFT を押しながら入力することで変換開始位置を明示し
;; ていましたが、仮名入力ではこれができません。そこで、コマンド
;; skk-set-henkan-point-subr をキーにバインド (デフォルトは [f2]) し、 これを押
;; すことで変換開始位置を指定するようにしました。 要は、abbrev モードと同様のや
;; り方で変換を開始することになります。例えば「春」の入力は
;;
;; [f2] はる ⇒ ▽はる [SPC] ⇒ ▼春
;;
;; または
;;
;; はる ^B^B [f2] ⇒ ▽はる ^F^F [SPC] ⇒ ▼春
;;
;; のいずれかでできます。
;;
;; 2. 送りありの変換のしかた
;;
;; 通常の SKK においては、 SHIFT を押しながら入力することで送り仮名の位置を明示
;; していました。仮名入力 SKK においてはそれはできません。そこで
;;
;; o C-u [SPC] が押されたときに、 直前の 1 文字を送り仮名と見倣して変換を開始す
;;   る (C-u が押されなければ送りなしと見倣す)。
;;
;; という風にしてみました。 但し、促音があった場合には促音も含めて 2 文字を送り
;; 仮名と見倣します。例えば、「待って」と入力したい場合は
;;
;; ▽まって C-u [SPC]  ⇒ ▼待って
;;
;; のようになります。
;;
;; 3. いくつかの重要なキー定義について
;;
;; カナ入力が 「q」、 abbrev モードが 「/」、latin モードが 「l」などは定番です
;; が、仮名入力ではこれも使えません。仮名入力できるキーボードでファンクションキ
;; ーが使えないことはあまり無いだろうと思い、デフォルトではこれらをファンクショ
;; ンキーに任せてみました。デフォルトでは以下のようになっています。ユーザオプシ
;; ョンなので自由に変更できます。
;;
;; [f2]  … 変換開始点の指定
;; [f5]  … コード入力
;; [f6]  … abbrev モード
;; [f7]  … カナモードまたはカナ変換
;; [f8]  … 全英モード
;; [f9]  … 半角カナモードまたは半角カナ変換
;; [f10] … latin モード
;;
;; {TODO}
;;
;; o このプログラムでサポート可能な入力方法 (キーボード) については、これをサポ
;;   ートする。
;; o このプログラムでサポート不可能なものは別のプログラムを書いてサポートする。

;;; Code:

(eval-when-compile (require 'skk-macs) (require 'skk-vars) (require 'static))

(defgroup skk-kanagaki nil "SKK kanagaki related customization."
  :prefix "skk-kanagaki-"
  :group 'skk)

;; Variables.

(defcustom skk-use-kana-keyboard t "\
*Non-nil なら仮名入力用の設定をロードする。
SKK 使用中にこの変数の値を切り替えることで  ローマ字入力 ←→ 仮名入力 のトグル
ができる。"
  :type 'boolean
  :group 'skk
  :group 'skk-kanagaki)

(defcustom skk-kanagaki-keyboard-type '106-jis "\
*仮名入力に使用するキーボードのタイプ。
値は任意のシンボル。 ただし  `skk-kanagaki-{シンボル名}-base-rule-list'  という
変数を用意しなければならない。デフォルトでは日本語 106 キーボード用の設定を用意
し、これを使用する。"
  :type 'symbol
  :group 'skk-kanagaki)

(defcustom skk-kanagaki-set-henkan-point-key
  (cond ((memq skk-emacs-type '(nemacs mule1))
	 "\e[12~")
	(t
	 [f2])) "\
*このキーを押すことで変換開始位置を設定する。
変換開始位置の設定は仮名を入力する前におこなっても、 入力し終わった後でおこなっ
ても構わない。"
  :type 'sexp
  :group 'skk-kanagaki)

(defcustom skk-kanagaki-abbrev-mode-key
  (cond ((memq skk-emacs-type '(nemacs mule1))
	 "\e[17~")
	(t
	 [f6])) "\
*このキーを押すことで abbrev モードに入る。"
  :type 'sexp
  :group 'skk-kanagaki)

(defcustom skk-kanagaki-katakana-mode-key
  (cond ((memq skk-emacs-type '(nemacs mule1))
	 "\e[18~")
	(t
	 [f7])) "\
*このキーを押すことでカナモードとかなモードを切りかえる。
変換開始位置の設定後に押すことで対象文字列をカナに変換することもできる。"
  :type 'sexp
  :group 'skk-kanagaki)

(defcustom skk-kanagaki-latin-jisx0208-mode-key
  (cond ((memq skk-emacs-type '(nemacs mule1))
	 "\e[19~")
	(t
	 [f8])) "\
*このキーを押すことで全角英数モードに入る。"
  :type 'sexp
  :group 'skk-kanagaki)

(defcustom skk-kanagaki-hankaku-mode-key
  (cond ((memq skk-emacs-type '(nemacs mule1))
	 "\e[20~")
	(t
	 [f9])) "\
*このキーを押すことで半角カナモードに切りかえる。
変換開始位置の設定後に押すことで対象文字列を半角カナに変換することもできる。"
  :type 'sexp
  :group 'skk-kanagaki)

(defcustom skk-kanagaki-latin-mode-key
  (cond ((memq skk-emacs-type '(nemacs mule1))
	 "\e[21~")
	(t
	 [f10])) "\
*このキーを押すことで latin モードに入る。"
  :type 'sexp
  :group 'skk-kanagaki)

(defcustom skk-kanagaki-code-input-key
  (cond ((memq skk-emacs-type '(nemacs mule1))
	 "\e[15~")
	(t
	 [f5])) "\
*このキーを押すことでコード入力ができる。"
  :type 'sexp
  :group 'skk-kanagaki)

(defcustom skk-kanagaki-previous-candidate-key "x" "\
*前候補を表示するためのキー。
XFree86 上で使用する場合、 例えばこの値を [henkan]  (XEmacs では [henkan-mode])
にすれば、日本語キーボードの [前候補] キーに割り当てることができる。 同キーは、
Mule2.3@19.28 では  [key-35]、 Mule2.3@19.34 では  [numbersign] (??) となるらし
い。"
  :type 'sexp
  :group 'skk-kanagaki)

(defcustom skk-kanagaki-rule-list nil "\
*キー入力に対する変換文字の規則で、ユーザーの追加の設定を行なうもの。
例えば、仮想キーコードに対するシンボルを独自に設定している場合などは、 この変数
を用いてそれに対応した設定をすることができる。"
  :type '(repeat
	  (list :tag "Rule"
		(string :tag "1 (keyboard input)")
		(choice :tag "2 (choose string if sokuon)"
			string
			(const nil))
		(choice :tag "3 (choice)"
			(symbol :tag "Function")
			(string :tag "String (common)")
			(cons :tag "Strings (katakana & hiragana)"
			 (string :tag "3-1 (katakana string)")
			 (string :tag "3-2 (hiragana string)")))))
  :group 'skk-kanagaki)

(defcustom skk-kanagaki-jidou-key-symbol-kakikae-service nil "\
*Non-nil なら刻印どおりの仮名入力のために勝手にキーシンボルを書換える。
副作用にご注意ください。 :)"
  :type '(choice (const 106-jis)
		 (const 106-jis-kodawari)
		 (const nil))
  :group 'skk-kanagaki)


;; Internal constants and variables.

(defconst skk-kanagaki-106-jis-base-rule-list
  '(("1" nil ("ヌ" . "ぬ")) ("2" nil ("フ" . "ふ")) ("3" nil ("ア" . "あ"))
    ("4" nil ("ウ" . "う")) ("5" nil ("エ" . "え")) ("6" nil ("オ" . "お"))
    ("7" nil ("ヤ" . "や")) ("8" nil ("ユ" . "ゆ")) ("9" nil ("ヨ" . "よ"))
    ("0" nil ("ワ" . "わ")) ("-" nil ("ホ" . "ほ")) ("^" nil ("ヘ" . "へ"))
    ("q" nil ("タ" . "た")) ("w" nil ("テ" . "て")) ("e" nil ("イ" . "い"))
    ("r" nil ("ス" . "す")) ("t" nil ("カ" . "か")) ("y" nil ("ン" . "ん"))
    ("u" nil ("ナ" . "な")) ("i" nil ("ニ" . "に")) ("o" nil ("ラ" . "ら"))
    ("p" nil ("セ" . "せ"))
    ("@" nil skk-kanagaki-dakuten)
    ("[" nil skk-kanagaki-handakuten)
    ("a" nil ("チ" . "ち")) ("s" nil ("ト" . "と"))  ("d" nil ("シ" . "し"))
    ("f" nil ("ハ" . "は")) ("g" nil ("キ" . "き"))  ("h" nil ("ク" . "く"))
    ("j" nil ("マ" . "ま")) ("k" nil ("ノ" . "の"))  ("l" nil ("リ" . "り"))
    (";" nil ("レ" . "れ")) (":" nil ("ケ" . "け"))  ("]" nil ("ム" . "む"))
    ("z" nil ("ツ" . "つ")) ("x" nil ("サ" . "さ"))  ("c" nil ("ソ" . "そ"))
    ("v" nil ("ヒ" . "ひ")) ("b" nil ("コ" . "こ"))  ("n" nil ("ミ" . "み"))
    ("m" nil ("モ" . "も")) ("," nil ("ネ" . "ね"))  ("." nil ("ル" . "る"))
    ("/" nil ("メ" . "め")) ("\\" nil ("ロ" . "ろ"))
    ;;
    ("!" nil ("ヌ" . "ぬ")) ("\"" nil ("フ" . "ふ")) ("#" nil ("ァ" . "ぁ"))
    ("$" nil ("ゥ" . "ぅ")) ("%" nil ("ェ" . "ぇ"))  ("&" nil ("ォ" . "ぉ"))
    ("'" nil ("ャ" . "ゃ")) ("(" nil ("ュ" . "ゅ"))  (")" nil ("ョ" . "ょ"))
    ("~" nil ("ヲ" . "を")) ("=" nil "£")
    ("|" nil "ー") ;; これが一番の問題。
    ("Q" nil ("タ" . "た")) ("W" nil ("テ" . "て"))  ("E" nil ("ィ" . "ぃ"))
    ("R" nil ("ス" . "す")) ("T" nil ("ヵ" . "ヵ"))  ("Y" nil ("ン" . "ん"))
    ("U" nil ("ナ" . "な")) ("I" nil ("ニ" . "に"))  ("O" nil ("ラ" . "ら"))
    ("P" nil "『")
    ("`" nil "¢")
    ("{" nil "「")
    ("A" nil ("チ" . "ち")) ("S" nil ("ト" . "と"))  ("D" nil ("シ" . "し"))
    ("F" nil ("ハ" . "は")) ("G" nil ("キ" . "き"))  ("H" nil ("ク" . "く"))
    ("J" nil ("マ" . "ま")) ("K" nil ("ノ" . "の"))  ("L" nil ("リ" . "り"))
    ("+" nil "』")          ("*" nil ("ヶ" . "ヶ"))  ("}" nil "」")
    ("Z" nil ("ッ" . "っ")) ("X" nil ("サ" . "さ"))  ("C" nil ("ソ" . "そ"))
    ("V" nil ("ヒ" . "ひ")) ("B" nil ("コ" . "こ"))  ("N" nil ("ミ" . "み"))
    ("M" nil ("モ" . "も"))
    ("<" nil skk-current-touten)
    (">" nil skk-current-kuten)
    ("?" nil "・")          ("_" nil ("ロ" . "ろ"))
    ;;
    ) "\
日本語 106 キーボードで仮名入力するための基本ルール。この設定では \"ー\" の入力
が刻印どおりにできないが、 SHIFT キーを押すことでできる。刻印どおりに入力できる
ようにするためには、仮想キーコードのレベルで制御する必要がある。")

(defconst skk-kanagaki-kana-to-rom-alist
  '(;; Nemacs では日本語は string として扱うべき。
    ("あ" ?a)    ("い" ?i)    ("う" ?u)    ("え" ?e)    ("お" ?o)
    ("か" ?k ?a) ("き" ?k ?i) ("く" ?k ?u) ("け" ?k ?e) ("こ" ?k ?o)
    ("さ" ?s ?a) ("し" ?s ?i) ("す" ?s ?u) ("せ" ?s ?e) ("そ" ?s ?o)
    ("た" ?t ?a) ("ち" ?t ?i) ("つ" ?t ?u) ("て" ?t ?e) ("と" ?t ?o)
    ("な" ?n ?a) ("に" ?n ?i) ("ぬ" ?n ?u) ("ね" ?n ?e) ("の" ?n ?o)
    ("は" ?h ?a) ("ひ" ?h ?i) ("ふ" ?h ?u) ("へ" ?h ?e) ("ほ" ?h ?o)
    ("ま" ?m ?a) ("み" ?m ?i) ("む" ?m ?u) ("め" ?m ?e) ("も" ?m ?o)
    ("や" ?y ?a)              ("ゆ" ?y ?u)              ("よ" ?y ?o)
    ("ら" ?r ?a) ("り" ?r ?i) ("る" ?r ?u) ("れ" ?r ?e) ("ろ" ?r ?o)
    ("わ" ?w ?a)                                        ("を" ?w ?o)
    ("ん" ?n ?n)
    ("が" ?g ?a) ("ぎ" ?g ?i) ("ぐ" ?g ?u) ("げ" ?g ?e) ("ご" ?g ?o)
    ("ざ" ?z ?a) ("じ" ?z ?i) ("ず" ?z ?u) ("ぜ" ?z ?e) ("ぞ" ?z ?o)
    ("だ" ?d ?a) ("ぢ" ?d ?i) ("づ" ?d ?u) ("で" ?d ?e) ("ど" ?d ?o)
    ("ば" ?b ?a) ("び" ?b ?i) ("ぶ" ?b ?u) ("べ" ?b ?e) ("ぼ" ?b ?o)
    ("ぱ" ?p ?a) ("ぴ" ?p ?i) ("ぷ" ?p ?u) ("ぺ" ?p ?e) ("ぽ" ?p ?o)
    ("ア" ?a)    ("イ" ?i)    ("ウ" ?u)    ("エ" ?e)    ("オ" ?o)
    ("カ" ?k ?a) ("キ" ?k ?i) ("ク" ?k ?u) ("ケ" ?k ?e) ("コ" ?k ?o)
    ("サ" ?s ?a) ("シ" ?s ?i) ("ス" ?s ?u) ("セ" ?s ?e) ("ソ" ?s ?o)
    ("タ" ?t ?a) ("チ" ?t ?i) ("ツ" ?t ?u) ("テ" ?t ?e) ("ト" ?t ?o)
    ("ナ" ?n ?a) ("ニ" ?n ?i) ("ヌ" ?n ?u) ("ネ" ?n ?e) ("ノ" ?n ?o)
    ("ハ" ?h ?a) ("ヒ" ?h ?i) ("フ" ?h ?u) ("ヘ" ?h ?e) ("ホ" ?h ?o)
    ("マ" ?m ?a) ("ミ" ?m ?i) ("ム" ?m ?u) ("メ" ?m ?e) ("モ" ?m ?o)
    ("ヤ" ?y ?a)              ("ユ" ?y ?u)              ("ヨ" ?y ?o)
    ("ラ" ?r ?a) ("リ" ?r ?i) ("ル" ?r ?u) ("レ" ?r ?e) ("ロ" ?r ?o)
    ("ワ" ?w ?a)                                        ("ヲ" ?w ?o)
    ("ン" ?n ?n)
                              ("ヴ" ?v ?u)
    ("ガ" ?g ?a) ("ギ" ?g ?i) ("グ" ?g ?u) ("ゲ" ?g ?e) ("ゴ" ?g ?o)
    ("ザ" ?z ?a) ("ジ" ?z ?i) ("ズ" ?z ?u) ("ゼ" ?z ?e) ("ゾ" ?z ?o)
    ("ダ" ?d ?a) ("ヂ" ?d ?i) ("ヅ" ?d ?u) ("デ" ?d ?e) ("ド" ?d ?o)
    ("バ" ?b ?a) ("ビ" ?b ?i) ("ブ" ?b ?u) ("ベ" ?b ?e) ("ボ" ?b ?o)
    ("パ" ?p ?a) ("ピ" ?p ?i) ("プ" ?p ?u) ("ペ" ?p ?e) ("ポ" ?p ?o)
    ;;
    ) "\
送りあり変換の際など、仮名をローマ字に翻訳するためのテーブル。")

(defconst skk-kanagaki-dakuten-alist
  '(("か" "が") ("き" "ぎ") ("く" "ぐ") ("け" "げ") ("こ" "ご")
    ("さ" "ざ") ("し" "じ") ("す" "ず") ("せ" "ぜ") ("そ" "ぞ")
    ("た" "だ") ("ち" "ぢ") ("つ" "づ") ("て" "で") ("と" "ど")
    ("は" "ば" "ぱ") ("ひ" "び" "ぴ") ("ふ" "ぶ" "ぷ") ("へ" "べ" "ぺ")
    ("ほ" "ぼ" "ぽ")
                            ("ウ" "ヴ")
    ("カ" "ガ") ("キ" "ギ") ("ク" "グ") ("ケ" "ゲ") ("コ" "ゴ")
    ("サ" "ザ") ("シ" "ジ") ("ス" "ズ") ("セ" "ゼ") ("ソ" "ゾ")
    ("タ" "ダ") ("チ" "ヂ") ("ツ" "ヅ") ("テ" "デ") ("ト" "ド")
    ("ハ" "バ" "パ") ("ヒ" "ビ" "ピ") ("フ" "ブ" "プ") ("ヘ" "ベ" "ペ")
    ("ホ" "ボ" "ポ")
    ;;
    ) "\
濁点と半濁点を入力するためのルール。")

(defvar skk-kanagaki-rule-tree nil)
(defvar skk-kanagaki-rom-kana-rule-tree nil)

(defvar skk-kanagaki-temp-dir (or (getenv "TMP") "/tmp"))


;; Pieces of advice.

(defadvice skk-regularize (before skk-kanagaki-ad activate compile)
  "SKK 起動時の適当なタイミングで仮名入力用の設定を行う。"
  (when (memq skk-emacs-type '(nemacs mule1))
    (if (not (keymapp (global-key-binding "\e[")))
	(global-unset-key "\e[")))
  (mapcar (function
	   (lambda (cons)
	     (and (symbol-value (car cons))
		  (define-key skk-j-mode-map
		    (symbol-value (car cons)) (cdr cons)))))
	  '((skk-kanagaki-set-henkan-point-key . skk-set-henkan-point-subr)
	    (skk-kanagaki-abbrev-mode-key . skk-abbrev-mode)
	    (skk-kanagaki-katakana-mode-key . skk-toggle-kana)
	    (skk-kanagaki-latin-jisx0208-mode-key . skk-jisx0208-latin-mode)
	    (skk-kanagaki-hankaku-mode-key . skk-toggle-katakana)
	    (skk-kanagaki-latin-mode-key . skk-latin-mode)
	    (skk-kanagaki-code-input-key . skk-input-by-code-or-menu)
	    (skk-kanagaki-previous-candidate-key . skk-previous-candidate)))
  (unless (memq skk-emacs-type '(nemacs mule1))
    (eval-after-load "skk-jisx0201"
      '(and skk-kanagaki-hankaku-mode-key
	    (define-key skk-jisx0201-mode-map skk-kanagaki-hankaku-mode-key
	      'skk-toggle-katakana))))
  ;;
  (case skk-kanagaki-jidou-key-symbol-kakikae-service
    ;;
    (106-jis
     (cond
      ((eq window-system 'x)
       (let ((prog (exec-installed-p "xmodmap"))
	     (tmp (make-temp-name
		   (expand-file-name "kanagaki" skk-kanagaki-temp-dir))))
	 (cond ((and prog
		     (message "xmodmap を呼んでいます...")
		     (save-excursion
		       (set-buffer (get-buffer-create " *kanagaki*"))
		       (erase-buffer)
		       (insert "keycode 123 = underscore underscore\n")
		       (write-region (point-min) (point-max) tmp)
		       (eq 0 (call-process prog nil nil nil tmp))))
		;;
		(setq skk-kanagaki-rule-list
		      (nconc skk-kanagaki-rule-list
			     '(("\\" nil "ー"))))
		(delete-file tmp)
		(message "xmodmap を呼んでいます...完了"))
	       (t
		(message "xmodmap の呼び出しに失敗しました")))))
      (t
       nil)))
    ;;
    (106-jis-kodawari
     (cond
      ((eq window-system 'x)
       (let ((prog (exec-installed-p "xmodmap"))
	     (tmp (make-temp-name
		   (expand-file-name "kanagaki" skk-kanagaki-temp-dir))))
	 (cond ((and prog
		     (message "xmodmap を呼んでいます...")
		     (save-excursion
		       (set-buffer (get-buffer-create " *kanagaki*"))
		       (erase-buffer)
		       (insert "keycode 123 = quotedbl underscore
keycode 19 = 0 exclam
keycode 21 = asciicircum asciitilde
keycode 34 = at grave\n")
		       (write-region (point-min) (point-max) tmp)
		       (eq 0 (call-process prog nil nil nil tmp))))
		;;
		(setq skk-kanagaki-rule-list
		      (nconc skk-kanagaki-rule-list
			     '(("~" nil "々")
			       ("\\" nil "ー")
			       ("|" nil "¬")
			       ("!" nil ("ヲ" . "を"))
			       ("\"" nil ("ロ" . "ろ"))
			       ("_" nil "｜"))))
		(delete-file tmp)
		(message "xmodmap を呼んでいます...完了"))
	       (t
		(message "xmodmap の呼び出しに失敗しました")))))
      (t
       nil)))
    ;;
    (t
     nil))
  ;;
  (define-key skk-j-mode-map " " 'skk-kanagaki-insert)
  ;;
  (setq skk-kanagaki-rule-tree
	(skk-compile-rule-list
	 (symbol-value (intern (format "skk-kanagaki-%s-base-rule-list"
				       skk-kanagaki-keyboard-type)))
	 skk-kanagaki-rule-list))
  (unless skk-kanagaki-rom-kana-rule-tree
    (setq skk-kanagaki-rom-kana-rule-tree
	  (skk-compile-rule-list skk-rom-kana-base-rule-list
				 skk-rom-kana-rule-list))))

(defadvice skk-insert (around skk-kanagaki-ad activate compile)
  "仮名入力用の work around 。"
  (let* ((list (copy-sequence skk-special-midashi-char-list))
	 (skk-special-midashi-char-list
	  ;; 句読点入力時の問題を回避。 日本語 106 キーボードでは "<" と ">" に
	  ;; よる接尾辞の入力はできなくなる。 "?" による接尾辞の入力はできる。
	  (cond
	   ((and
	     skk-use-kana-keyboard
	     (memq last-command-char list)
	     (memq
	      (nth 2 (assoc (skk-char-to-string last-command-char)
			    (symbol-value
			     (intern (format "skk-kanagaki-%s-base-rule-list"
					     skk-kanagaki-keyboard-type)))))
	      '(skk-current-kuten skk-current-touten)))
	    (delq last-command-char list))
	   (t
	    list))))
    (cond (skk-use-kana-keyboard
	   (or (equal skk-rule-tree skk-kanagaki-rule-tree)
	       (setq skk-rule-tree skk-kanagaki-rule-tree))
	   (let (skk-set-henkan-point-key)
	     ad-do-it))
	  (t
	   (or (equal skk-rule-tree skk-kanagaki-rom-kana-rule-tree)
	       (setq skk-rule-tree skk-kanagaki-rom-kana-rule-tree))
	   ad-do-it))))


;; Functions.

(defun skk-kanagaki-insert (&optional arg)
  "SPC キーだけこれを `skk-insert' の代わりに使う。"
  (interactive "*p")
  (cond ((eq arg 1) (skk-insert arg))
	;; C-u [SPC] で送りあり変換をする。
	(t (skk-kanagaki-start-henkan-okuriari))))

(defun skk-kanagaki-dakuten (&optional arg)
  "直前の文字を見て可能なら濁点を付加し、さもなければ \"゛\" を入力する。"
  (interactive "*p")
  (let ((list skk-kanagaki-dakuten-alist)
	(pt1 (point))
	(len (if (eq skk-emacs-type 'nemacs) 2 1))
	char1 char2)
    (setq char1
	  (save-excursion
	    (backward-char (* len 1))
	    (buffer-substring-no-properties (point) pt1)))
    (cond ((setq char2 (cadr (assoc char1 list)))
	   (delete-char -1)
	   (insert char2))
	  (t
	   (insert "゛")))))

(defun skk-kanagaki-handakuten (&optional arg)
  "直前の文字を見て可能なら半濁点を付加し、さもなければ \"゜\" を入力する。"
  (interactive "*p")
  (let ((list skk-kanagaki-dakuten-alist)
	(pt1 (point))
	(len (if (eq skk-emacs-type 'nemacs) 2 1))
	char1 char2)
    (setq char1
	  (save-excursion
	    (backward-char (* len 1))
	    (buffer-substring-no-properties (point) pt1)))
    (cond ((setq char2 (caddr (assoc char1 list)))
	   (delete-char -1)
	   (insert char2))
	  (t
	   (insert "゜")))))

(defun skk-kanagaki-start-henkan-okuriari (&optional no-sokuon)
  ;; 直前の文字をローマ字のリストに翻訳し、あたかもその順番にキー入力されたかの
  ;; ように SKK に思わせる。
  (let ((list skk-kanagaki-kana-to-rom-alist)
	(i 0)
	(pt1 (point))
	(len (if (eq skk-emacs-type 'nemacs) 2 1))
	pt2 okuri-char rom char sokuon)
    (setq okuri-char
	  (save-excursion
	    (backward-char (* len 1))
	    (buffer-substring-no-properties (setq pt2 (point)) pt1)))
    (when okuri-char
      (setq rom (copy-sequence (cdr (assoc okuri-char list))))
      (unless no-sokuon
	(setq sokuon
	      (save-excursion
		(backward-char (* len 2))
		(buffer-substring-no-properties (point) pt2)))
	(if (member sokuon '("っ" "ッ"))
	    ;; 促音を見つけたときは、ローマ字リストの先頭の字を繰り返す。
	    (setcdr rom (copy-sequence rom))
	  (setq sokuon nil))))
    (when rom
      (delete-char (* len (if sokuon -2 -1)))
      ;; ローマ字リストの先頭を大文字にすることで送りありの変換をさせる。
      (setcar rom (upcase (car rom)))
      (while (setq char (nth i rom))
	(let ((last-command-char char)
	      (skk-rule-tree skk-kanagaki-rom-kana-rule-tree))
	  (ad-Orig-skk-insert 1))
	(setq i (1+ i))))))

(provide 'skk-kanagaki)
;;; Local Variables:
;;; End:
;;; skk-kanagaki.el ends here
