;;; skk-annotation.el --- SKK annotation 関連プログラム
;; Copyright (C) 2000 Mikio Nakajima <minakaji@osaka.email.ne.jp>

;; Author: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Version: $Id: skk-annotation.el,v 1.1.2.1 2000/10/27 09:44:20 minakaji Exp $
;; Keywords: japanese
;; Created: Oct. 27, 2000.
;; Last Modified: $Date: 2000/10/27 09:44:20 $

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
;;
;;; Commentary:
;;
;;; Code:

;;;###autoload
(defun skk-annotation-show (annotation)
  (let (event)
    (save-window-excursion
      (save-excursion
	(set-buffer (get-buffer-create "*SKK annotation*"))
	(erase-buffer)
	(insert annotation))
      ;;(delete-other-windows)
      (split-window-vertically)
      (display-buffer "*SKK annotation*")
      (setq event (skk-read-event))
      (skk-unread-event event))))

(require 'product)
(product-provide (provide 'skk-annotation) (require 'skk-version))
;;; end of skk-annotation.el.
