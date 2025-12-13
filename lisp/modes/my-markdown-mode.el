;;; my-markdown-mode.el --- Markdown editing -*- lexical-binding: t; -*-

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
;; Configuration for editing markdown buffers.

;;; Code:

(use-package markdown-mode
  :hook (markdown-mode . visual-line-mode)
  :mode (("\\.md\\'" . gfm-mode)
	 ("\\.markdown\\'" . gfm-mode)
	 ("\\.Rmd\\'" . gfm-mode)
	 ("README\\.md\\'" . gfm-mode)
	 ("qutebrowser-editor" . gfm-mode))
  :config
  (my-local-leader-def
    :keymaps '(markdowm-mode-map gfm-mode-map)
    :infix "l"
    "l" #'markdown-insert-link)
  (general-nmap
    :keymaps '(markdown-mode-map gfm-mode-map)
    "gj" #'markdown-outline-next
    "gk" #'markdown-outline-previous
    "M-j" #'markdown-move-down
    "M-k" #'markdown-move-up
    "C-i" #'markdown-demote
    "C-S-i" #'markdown-promote))

(use-package poly-markdown
  :mode (("\\.md\\'" . poly-markdown-mode)
	 ("\\.Rmd\\'" . poly-markdown-mode)))

(provide 'my-markdown-mode)
;;; my-markdown-mode.el ends here
