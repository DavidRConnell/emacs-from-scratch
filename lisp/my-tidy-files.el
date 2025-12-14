;;; my-tidy-files.el --- Clean up the user directory -*- lexical-binding: t; -*-

;; Copyright (C) 2020 David R. Connell
;;
;; Author: David R. Connell <david32@dcon.addy.io>
;; Created: July 28, 2020

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
;; Prevents Emacs from filling the git controlled user directory with generated
;; files. Try to respect XDG directory types as much as possible.

;;; Code:

(require 'my-variables)

(eval-and-compile
  (setq no-littering-etc-directory my-cache-dir)
  (setq no-littering-var-directory my-var-dir)
  (require 'no-littering))

(setq my-auto-save-directory
      (expand-file-name "auto-save/" my-var-dir)
      my-backup-directory
      (expand-file-name "backup/" my-var-dir))

(setq auto-save-file-name-transforms
      `((".*" ,(concat my-auto-save-directory "\\1") t))
      backup-directory-alist
      `(("" . ,my-backup-directory)))

(if (not (file-directory-p my-auto-save-directory))
    (make-directory my-auto-save-directory))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq custom-file (expand-file-name "custom.el" my-var-dir))
(if (file-exists-p custom-file)
    (load custom-file))

(provide 'my-tidy-files)
;;; my-tidy-files.el ends here
