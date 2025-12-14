;;; my-c-mode.el --- C and C++ configuration -*- lexical-binding: t; -*-

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
;; Configuration for C and C++ modes.

;;; Code:

(require 'my-keybindings)
(require 'my-prog-mode "modes/my-prog-mode")

(add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
(add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
(add-to-list 'major-mode-remap-alist '(c-or-c++-mode . c-or-c++-ts-mode))

(with-eval-after-load 'c-ts-base-mode
  (general-nmmap
    :keymaps 'c-ts-base-mode-map
    "gK" 'my-man-at-point)

  (my-local-leader-def
    :keymaps 'c-ts-base-mode-map
    "d" '(:keymap my-gud-map :which-key "debug")))

(add-hook 'c-ts-base-mode-hook #'eglot-ensure)
(add-hook 'c-ts-base-mode-hook
	  (lambda ()
	    (setq-local format-all-formatters
			'(("C" (clang-format "--style=file"))
			  ("C++" (clang-format "--style=file"))))))

(add-hook 'c-ts-base-mode-hook #'format-all-mode)

(provide 'my-c-mode)
;;; my-c-mode.el ends here
