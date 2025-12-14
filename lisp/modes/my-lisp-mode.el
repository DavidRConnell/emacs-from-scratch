;;; my-lisp-mode.el --- General lisp mode configuration -*- lexical-binding: t; -*-

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
;; Configuration for general Lisp modes. Specific Lisps may have additional
;; configuration files.

;;; Code:

(require 'my-keybindings)
(require 'my-ui)

(require 'lispy)
(require 'lispyville)

(add-hook 'emacs-lisp-mode-hook #'lispy-mode)
(add-hook 'lisp-mode-hook #'lispy-mode)
(add-hook 'lispy-mode-hook #'lispyville-mode)

(my-popper-add-reference "\\*lispy-message\\*")

(lispyville-set-key-theme
 '(operators c-w slurp/barf-lispy commentary prettify c-u))

(with-eval-after-load 'scheme
  (autoload 'geiser-mode "geiser-mode")
  (add-hook 'scheme-mode-hook #'geiser-mode)

  (general-nmap
    :keymaps 'geiser-mode-map
    "K" 'evil-scroll-line-up))

(provide 'my-lisp-mode)
;;; my-lisp-mode.el ends here
