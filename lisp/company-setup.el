;;; company-setup.el --- Company completion -*- lexical-binding: t -*-
;;
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
;; Company text completion set up.

;;; Code:

(use-package company
  :config
  (general-define-key
   :keymaps 'company-active-map
   "C-w" nil
   "C-n" #'company-select-next
   "C-p" #'company-select-previous
   "C-h" #'company-show-doc-buffer
   "C-s" #'company-filter-candidates
   "C-i" #'company-complete-selection
   "C-SPC" #'company-complete-common-or-cycle)

  (general-imap
    "C-SPC" #'company-complete
    "C-n" #'company-dabbrev
    "C-f" #'company-files
    "C-s" #'company-ispell)

  (general-define-key
   :keymaps 'company-search-map
   "C-n" #'company-select-next-or-abort
   "C-p" #'company-select-previous-or-abort)

  (global-company-mode 1)
  (add-hook 'text-mode-hook (defun my-set-text-mode-backends ()
			      (setq-local company-backends
					  '(company-dabbrev))))

  (setq company-idle-delay nil
	company-minimum-prefix-length 2
	company-selection-wrap-around t)

  (use-package company-box
    :disabled
    :hook (company-mode . company-box-mode)
    :config (setq company-box-scrollbar nil))

  (use-package company-prescient
    :hook (company-mode . company-prescient-mode))

  (use-package company-math
    :general
    (general-imap
      "C-\\" #'company-math-symbols-unicode)))

(provide 'company-setup)
;;; company-setup.el ends here
