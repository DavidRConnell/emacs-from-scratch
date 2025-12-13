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

(require 'my-keybindings)

(autoload 'gfm-mode "markdown-mode")

(dolist (ext '(md markdown Rmd))
  (add-to-list 'auto-mode-alist `(,(format "\\.%s\\'" ext) . gfm-mode)))
(add-to-list 'auto-mode-alist '("qutebrowser-editor" . gfm-mode))

(defvar my-md-link-map (make-sparse-keymap)
  "Poor man's org-link mimic.")

(my-local-leader-def
  :keymaps '(markdowm-mode-map gfm-mode-map)
  "p" 'markdown-preview
  "l" '(:keymap my-md-link-map :which-key "links"))

(general-def
  :keymaps 'my-md-link-map
  "l" 'markdown-insert-link)

(general-nmap
  :keymaps '(markdown-mode-map gfm-mode-map)
  "gj" 'markdown-outline-next
  "gk" 'markdown-outline-previous
  "M-j" 'markdown-move-down
  "M-k" 'markdown-move-up
  "C-i" 'markdown-demote
  "C-S-i" 'markdown-promote)

;; REVIEW: Not sure if I use this.
;; (with-eval-after-load 'markdown-mode
;;   (require 'poly-markdown)
;;   (add-hook 'gfm-mode-hook 'poly-markdown-mode)
;;   (add-hook 'markdown-mode-hook 'poly-markdown-mode))

(provide 'my-markdown-mode)
;;; my-markdown-mode.el ends here
