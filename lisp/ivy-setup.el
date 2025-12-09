;;; ivy-setup.el --- Setup ivy completion -*- lexical-binding: t -*-

;; Copyright (C) 2020 David R. Connell
;;
;; Author: David R. Connell <david32@dcon.addy.io>

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
;; Set up Ivy completion menu.

;;; Code:

(use-package ivy
  :config
  (ivy-mode 1)
  (general-define-key
   :keymaps 'override
   "C-c C-r" #'ivy-resume)
  (general-define-key
   :keymaps 'ivy-minibuffer-map
   "C-i" #'ivy-dispatching-done
   "C-w" #'ivy-backward-kill-word
   "C-SPC" #'ivy-mark)

  (use-package ivy-posframe
    :config
    (setq ivy-posframe-display-functions-alist
	  '((t . ivy-posframe-display-at-frame-center)))
    (setq ivy-posframe-parameters '((internal-border . 2)))
    (ivy-posframe-mode 1))

  (use-package ivy-avy
    :general
    (general-define-key
     :keymaps 'ivy-minibuffer-map
     "C-s" #'ivy-avy))

  (use-package ivy-rich
    :config
    (ivy-rich-mode 1))

  (use-package counsel
    :config
    (my-leader-def
      "O" #'counsel-recentf)
    (counsel-mode 1)
    (setq counsel-describe-function-function #'helpful-callable
	  counsel-describe-variable-function #'helpful-variable))

  (use-package orderless
    :config
    (setq ivy-re-builders-alist '((t . orderless-ivy-re-builder))
	  completion-styles '(orderless)))
  (savehist-mode)

  (use-package ivy-prescient
    :disabled
    :config
    (ivy-prescient-mode 1))

  (use-package swiper
    :general
    (general-nmmap
      "/" #'swiper
      "?" #'swiper-backward
      "gn" #'swiper-thing-at-point)
    :config
    (general-define-key
     :keymaps 'swiper-map
     "C-s" #'swiper-avy))

  (setq ivy-sort-max-size 7500))

(use-package amx
  :commands amx
  :init
  (setq amx-backend 'ivy)
  :config (amx-mode 1))

(provide 'ivy-setup)
;;; ivy-setup.el ends here
