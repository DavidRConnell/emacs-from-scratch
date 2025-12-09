;;; corfu-setup.el --- Set up corfu completion -*- lexical-binding: t -*-
;;
;; Copyright (C) 2024 David R. Connell
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
;; Packages related to corfu.

;;; Code:

(use-package corfu
  :config
  (general-define-key
   :keymaps corfu-map
   "C-SPC" corfu-complete)
  (general-imap
    "C-SPC" #'completion-at-point)
  (setq corfu-cylce t
	corfu-auto t
	corfu-echo-documentation t
	corfu-quit-no-match t)

  (corfu-global-mode 1))

(use-package orderless
  :init
  (setq completion-styles '(orderless)
	completion-category-defaults nil
	completion-category-overrides '((file (styles . (partial-completion))))))

(provide 'corfu-setup)
;;; corfu-setup.el ends here
