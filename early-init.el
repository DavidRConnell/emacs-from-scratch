;;; early-init.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 David R. Connell
;;
;; Author: David R. Connell <http://github/voidee>
;; Maintainer: David R. Connell <voidee@TheVoid>
;; Created: October 13, 2020
;; Modified: October 13, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/voidee/early-init
;; Package-Requires: ((emacs 27.1) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(setq gc-cons-threshold most-positive-fixnum)

(setq frame-resize-pixelwise t)
(setq default-frame-alist
      '((font . "Hack:style=Light:size=14")
        (min-height . 1)  '(height     . 45)
        (min-width  . 40) '(width      . 81)
        (vertical-scroll-bars . nil)
        (internal-border-width . 24)
        (tool-bar-lines . 0)
        (menu-bar-lines . 0)))

;; Fall back font for glyph missing in Roboto
(defface fallback '((t :family "Fira Code"
                       :inherit 'nano-face-faded)) "Fallback")
(set-display-table-slot standard-display-table 'truncation
                        (make-glyph-code ?… 'fallback))
(set-display-table-slot standard-display-table 'wrap
                         (make-glyph-code ?↩ 'fallback))

(setq inhibit-startup-screen nil
      initial-buffer-choice "~/notes/zettle/todo.org"
      inhibit-startup-message t
      inhibit-startup-echo-area-message t
      initial-scratch-message nil)

(scroll-bar-mode 0)
(tool-bar-mode 0)
(tooltip-mode 0)
(global-hl-line-mode 1)

(setq x-underline-at-descent-line t
      ring-bell-function 'ignore)

;; Vertical window divider
(setq window-divider-default-right-width 24)
(setq window-divider-default-places 'right-only)
(window-divider-mode 1)

;; No ugly button for checkboxes
(setq widget-image-enable nil)

(setq frame-inhibit-implied-resize t)

(setq package-enable-at-startup nil)

(provide 'early-init)
;;; early-init.el ends here
