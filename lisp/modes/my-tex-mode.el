;;; my-tex-mode.el --- TeX config -*- lexical-binding: t; -*-

;; Copyright (C) 2025 David R. Connell

;; Author: David R. Connell <david32@dcon.addy.io>
;; Created: December 13, 2025

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;; Configuration for TeX buffers.

;;; Code:

(autoload 'tex-mode "tex-mode")

(with-eval-after-load 'tex-mode
  (add-to-list 'tex-compile-commands
	       '((concat "latexmk -xelatex" " %f") t "%r.pdf"))
  (add-to-list 'tex-compile-commands
	       '((concat "latexmk -lualatex" " %f") t "%r.pdf")))

(provide 'my-tex-mode)
;;; my-tex-mode.el ends here
