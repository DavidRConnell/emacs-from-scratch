;;; early-init.el --- Early init config file -*- lexical-binding: t; -*-

;; Copyright (C) 2020 David R. Connell
;;
;; Author: David R. Connell <david32@dcon.addy.io>
;; Created: October 13, 2020

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
;; Override UI defaults early to prevent flashes at start up.
;; Modifies garbage collection values to improve initialization times.

;;; Code:

(setq gc-cons-threshold most-positive-fixnum)
(setq load-prefer-newer t
      native-comp-jit-compilation t)

(setq frame-resize-pixelwise t)
(setq default-frame-alist
      '((font . "Hack:style=Light:size=14")
        (min-height . 1) '(height . 45)
        (min-width . 40) '(width . 81)
        (vertical-scroll-bars . nil)
        (internal-border-width . 24)
        (tool-bar-lines . 0)
        (menu-bar-lines . 0)))

(setq native-comp-async-report-warnings-errors nil)

(scroll-bar-mode 0)
(tool-bar-mode 0)
(tooltip-mode 0)
(global-hl-line-mode 1)

(setq x-underline-at-descent-line t
      ring-bell-function 'ignore)

;; Vertical window divider
(setq window-divider-default-right-width 24
      window-divider-default-places 'right-only)
(window-divider-mode 1)

;; No ugly button for checkboxes
(setq widget-image-enable nil)

(setq frame-inhibit-implied-resize t)

(setq package-enable-at-startup nil)

(provide 'early-init)
;;; early-init.el ends here
