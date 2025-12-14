;;; my-nix-mode.el --- Nix configuration -*- lexical-binding: t; -*-

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
;; Configuration for editing Nix.

;;; Code:

(require 'my-projects)
(require 'my-keybindings)

(autoload 'nix-mode "nix-mode")
(autoload 'nix-flake "nix-flake")

(add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode))

(my-leader-def
  "h" 'my-find-home-manager
  "x" 'nix-flake)

(defun my-find-home-manager ()
  "Find file in my home-manager configuration flake."
  (interactive)
  (projectile-find-file-in-directory
   (expand-file-name "nixpkgs" (getenv "XDG_CONFIG_HOME"))))

(with-eval-after-load 'nix-mode
  (add-hook 'nix-mode-hook #'eglot-ensure)

  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '(nix-mode "nil")))

  (add-hook 'nix-mode-hook #'format-all-mode)
  (with-eval-after-load 'format-all
    (add-hook 'nix-mode-hook (lambda ()
			       (setq-local format-all-formatters
					   '(("Nix" nixfmt)))))))

(provide 'my-nix-mode)
;;; my-nix-mode.el ends here
