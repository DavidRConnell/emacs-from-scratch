;;; my-completion.el --- Setup completion menus -*- lexical-binding: t -*-
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
;; Completion menu setup. Provides general completion menu settings, requiring
;; more specific initialization files to make switching between completion
;; flexible.

;;; Code:

(require 'prescient)
(require 'orderless)

(customize-set-variable 'prescient-aggressive-file-save t)
(customize-set-variable 'prescient-sort-length-enable nil)
(customize-set-variable 'prescient-sort-full-matches-first t)
(customize-set-variable 'prescient-history-length 200)
(customize-set-variable 'prescient-frequency-decay 0.997)
(customize-set-variable 'prescient-frequency-threshold 0.05)

(customize-set-variable 'completion-styles '(orderless basic))
(customize-set-variable 'completion-category-defaults nil)
(customize-set-variable 'completion-category-overrides
			'((file (styles partial-completion))
			  (eglot (styles orderless))
			  (eglot-capf (styles orderless))))

(prescient-persist-mode)

(let ((load-path (append
		  (list (expand-file-name "lisp/completion"
					  user-emacs-directory))
		  load-path)))
  (require 'my-vertico)
  (require 'my-corfu))

(provide 'my-completion)
;;; my-completion.el ends here
