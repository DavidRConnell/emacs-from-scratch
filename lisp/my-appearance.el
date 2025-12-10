;;; my-appearance.el --- Tweak the appearance of emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2020 David R. Connell
;;
;; Author: David R. Connell <david32@dcon.addy.io>
;; Created: October 26, 2020

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
;; Set the theme and appearance variables.

;;; Code:

;; Load modus operandi early to use as a fallback. Nano does not provide fonts
;; for nearly as many packages as modus operandi so anything not in nano will
;; leave colors to the original theme.
(load-theme 'modus-operandi t)

(blink-cursor-mode -1)
(show-paren-mode)
(global-hl-line-mode)

(setq frame-title-format "%b")

(global-display-fill-column-indicator-mode)
(customize-set-variable 'mode-line-format "%-")
(customize-set-variable 'fill-column 79)

(autoload 'visual-fill-column-mode "visual-fill-column")
(add-hook 'visual-line-mode-hook #'visual-fill-column-mode)

(let ((load-path (append (list (expand-file-name "vendor/nano-emacs"
						 user-emacs-directory))
			 load-path)))
  (require 'nano-faces)
  (require 'nano-theme-light)
  (require 'nano-theme)
  (require 'nano-modeline))

(nano-refresh-theme)
(set-face-attribute 'modus-themes-completion-selected nil
		    :foreground 'unspecified :background nano-color-highlight
		    :family 'unspecified :slant 'unspecified
		    :weight 'unspecified :height 'unspecified
		    :underline 'unspecified :overline 'unspecified
		    :box 'unspecified :inherit 'bold)

(provide 'my-appearance)
;;; my-appearance.el ends here
