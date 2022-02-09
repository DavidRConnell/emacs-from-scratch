;;; appearance.el --- Tweak the appearance of emacs -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package modus-operandi-theme)

(load-theme 'modus-operandi t)
(setq-default mode-line-format nil)
(blink-cursor-mode -1)
(global-display-fill-column-indicator-mode t)
(show-paren-mode t)

(use-package visual-fill-column
  :hook (visual-line-mode . visual-fill-column-mode))

(require 'mode-line)

(provide 'appearance)
