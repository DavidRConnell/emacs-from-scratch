;;;; file-templates.el --- Autoinsert templates in empty files -*- lexical-binding: t; -*-

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
;; Add autoinsert for new files.

;;; Code:

(use-package yatemplate
  :after yasnippet
  :config
  (auto-insert-mode t)
  (setq yatemplate-dir (expand-file-name "file-templates"
					 user-emacs-directory))
  (yatemplate-fill-alist))

(provide 'file-templates)
;;; file-templates.el ends here
