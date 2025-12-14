;;; my-scad-mode.el --- SCAD 3d modeling support -*- lexical-binding: t; -*-

;; Copyright (C) 2025 David R. Connell

;; Author: David R. Connell <david32@dcon.addy.io>
;; Created: December 14, 2025

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
;; Configuration for working on OpenSCAD files.

;;; Code:

(require 'my-keybindings)

(autoload 'scad-mode "scad-mode")
(add-to-list 'auto-mode-alist '("\\.scad\\'" . scad-mode))

(with-eval-after-load 'scad-mode
  (straight-use-package
   '(scad-dbus :host github :repo "Lenbok/scad-dbus" :branch "master"))

  (require 'scad-dbus)

  (add-hook 'scad-mode-hook #'eglot-ensure)
  (with-eval-after-load 'eglot
   (add-to-list 'eglot-server-programs
		 '(scad-mode "openscad-lsp" "--port" :autoport)))

  (my-local-leader-def
    :keymaps 'scad-mode-map
    "-" 'scad-dbus-view-zoom-out
    "+" 'scad-dbus-view-zoom-in
    "=" 'scad-dbus-view-reset
    "p" 'scad-dbus-preview
    "c" 'scad-dbus-export-stl))

(provide 'my-scad-mode)
;;; my-scad-mode.el ends here
