;;; package --- variables.el - Set global variables -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 David R. Connell
;;
;; Author: David R. Connell <http://github/voidee>
;; Maintainer: David R. Connell <voidee@TheVoid>
;; Created: October 26, 2020
;; Modified: October 26, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/voidee/variables
;; Package-Requires: ((emacs 27.1) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
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

(cl-dolist (dir (list my-cache-dir my-var-dir my-config-dir))
  (unless (file-exists-p dir)
    (make-directory dir)))

(defvar my-notes-dir "~/notes/")
(defvar my-zettle-dir (expand-file-name "zettle/" my-notes-dir))
(defvar my-refs-notes-dir (expand-file-name "references/" my-zettle-dir))
(defvar my-refs-pdfs-dir "~/References/")
(defvar my-refs-books-dir "~/books/")
(defvar my-refs-bib (expand-file-name "master.bib" my-refs-notes-dir))

(setenv "PATH" (format "%s:%s" (getenv "PATH") "~/bin"))

(provide 'variables)
;;; variables.el ends here
