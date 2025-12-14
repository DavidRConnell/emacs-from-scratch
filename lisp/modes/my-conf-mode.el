;;; my-conf-mode.el --- Set up for Configuration modes -*- lexical-binding: t; -*-

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
;; Set up for various configuration file types.

;;; Code:

(add-to-list 'major-mode-remap-alist '(yaml-mode . yaml-ts-mode))
(add-to-list 'major-mode-remap-alist '(toml-mode . toml-ts-mode))
(add-to-list 'major-mode-remap-alist '(conf-toml-mode . toml-ts-mode))
(add-to-list 'major-mode-remap-alist '(js-mode . js-ts-mode))

(autoload 'toml-ts-mode "toml-ts-mode")
(autoload 'yaml-ts-mode "yaml-ts-mode")
(autoload 'js-ts-mode "js-ts-mode")

(add-to-list 'auto-mode-alist '("\\.toml\\'" . toml-mode))
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.clang-format\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\flake.lock\\'" . js-mode))

(with-eval-after-load 'yaml-ts-mode
  (add-hook 'yaml-ts-mode-hook #'format-all-mode)

  (with-eval-after-load 'format-all
    (add-hook 'yaml-ts-mode-hook
	      (lambda ()
		(setq-local format-all-formatters '(("YAML" prettierd)))))))

(provide 'my-conf-mode)
;;; my-conf-mode.el ends here
