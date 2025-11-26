;;; appearance.el --- Tweak the appearance of emacs -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(load-theme 'modus-operandi t)
(setq-default mode-line-format nil)
(blink-cursor-mode -1)
(setq-default fill-column 79)
(global-display-fill-column-indicator-mode t)
(show-paren-mode t)

(use-package visual-fill-column
  :hook (visual-line-mode . visual-fill-column-mode))

(require 'mode-line)
(set-face-attribute 'modus-themes-completion-selected nil
		    :foreground 'unspecified :background nano-color-highlight
		    :family 'unspecified :slant 'unspecified
		    :weight 'unspecified :height 'unspecified
		    :underline 'unspecified :overline 'unspecified
		    :box 'unspecified :inherit 'bold)

(provide 'appearance)
