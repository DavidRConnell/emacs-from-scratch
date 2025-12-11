;;;; init.el --- Main initialization file -*- lexical-binding: t; -*-

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
;; Dispatcher for requiring all Emacs initialization files.

;;; Code:

(add-to-list 'load-path
             (expand-file-name "lisp/" user-emacs-directory))

(defconst my-extra-evil-jump-commands
  '()
  "A list of commands that should (but currently don't) set evil's jump list.")

(require 'my-variables)
(require 'my-tidy-files)
(require 'my-straight-bootstrap)
(require 'my-appearance)
(require 'pass)
(require 'my-keybindings)
(require 'my-ui)
(require 'projects)
(require 'git)
(require 'development)
(require 'writting)
(require 'notes)
(require 'references)
(require 'file-templates)
(require 'terminal)
(require 'rss)
(require 'completion)

(let ((inhibit-message t))
  (message (format "Initialization time: %s"
                   (emacs-init-time))))

(provide 'init)
;;; init.el ends here
