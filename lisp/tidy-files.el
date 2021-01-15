;;; tidy-files.el --- description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 David R. Connell
;;
;; Author: David R. Connell <http://github/voidee>
;; Maintainer: David R. Connell <voidee@TheVoid>
;; Created: July 28, 2020
;; Modified: July 28, 2020
;; Version: 0.0.1
;; Keywords:
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  description
;;
;;; Code:

(use-package no-littering
  :init
  (setq no-littering-etc-directory my-cache-dir
	no-littering-var-directory my-var-dir))

(setq my-auto-save-directory
      (expand-file-name "auto-save/" my-var-dir))

(setq auto-save-file-name-transforms
      `((".*" ,(concat my-auto-save-directory "\\1") t)))

(if (not (file-directory-p my-auto-save-directory))
    (make-directory my-auto-save-directory))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(provide 'tidy-files)
;;; tidy-files.el ends here
