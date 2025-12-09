;;; variables.el --- Set global variables -*- lexical-binding: t; -*-

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
;; Defines user specific global variables that will be used throughout the
;; config.

;;; Code:

(require 'cl-lib)

(setq user-full-name "David R. Connell"
      user-mail-address "david32@dcon.addy.io")

(defvar my-cache-dir (expand-file-name "emacs/" (getenv "XDG_CACHE_HOME"))
  "Location to store reproducibly generated files.")

(defvar my-var-dir (expand-file-name "emacs/" (getenv "XDG_DATA_HOME"))
  "Location to store persistent, non-reproducible data.")

(defvar my-config-dir (expand-file-name "emacs/" (getenv "XDG_CONFIG_HOME"))
  "Location to store persistent, non-reproducible data.")

(dolist (dir (list my-cache-dir my-var-dir my-config-dir))
  (unless (file-exists-p dir)
    (make-directory dir)))

(defvar my-notes-dir "~/notes/")
(defvar my-zettle-dir (expand-file-name "zettle/" my-notes-dir))
(defvar my-refs-notes-dir (expand-file-name "references/" my-zettle-dir))
(defvar my-refs-pdfs-dir "~/References/")
(defvar my-refs-books-dir "~/books/")
(defvar my-refs-bib (expand-file-name "master.bib" my-refs-notes-dir))

(defvar my-personal-dictionary
  (expand-file-name ".aspell.en.pws" (getenv "HOME")))

(defvar my-alternate-dictionary
  (expand-file-name "dicts/en-common.wl" my-var-dir))

(setenv "PATH" (format "%s:%s" (getenv "PATH") "~/bin"))

(provide 'variables)
;;; variables.el ends here
