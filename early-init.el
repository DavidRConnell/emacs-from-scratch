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
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
	  (lambda ()
	    (setq file-name-handler-alist default-file-name-handler-alist)))

(setq load-prefer-newer t
      native-comp-jit-compilation t
      native-comp-async-report-warnings-errors 'silent)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)

(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t)

(add-to-list 'default-frame-alist '(font . "Hack:style=Light:size=14"))
(add-to-list 'default-frame-alist '(min-height . 1))
(add-to-list 'default-frame-alist '(height . 45))
(add-to-list 'default-frame-alist '(min-width . 40))
(add-to-list 'default-frame-alist '(width . 81))
(add-to-list 'default-frame-alist '(internal-border-width . 24))

;; No ugly button for checkboxes
(setq widget-image-enable nil)

(setq x-underline-at-descent-line t
     ring-bell-function 'ignore)

;; Vertical window divider
(setq window-divider-default-right-width 24
     window-divider-default-places 'right-only)
(window-divider-mode 1)

(setq package-enable-at-startup nil)

(provide 'early-init)
;;; early-init.el ends here
