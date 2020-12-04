;;; settings.el --- description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 David R. Connell
;;
;; Author: David R. Connell <http://github/voidee>
;; Maintainer: David R. Connell <voidee@TheVoid>
;; Created: July 28, 2020
;; Modified: July 28, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/voidee/settings
;; Package-Requires: ((emacs 26.3) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  description
;;
;;; Code:

(setq backup-directory-alist
      `((".*" . ,(expand-file-name "backup/" my-var-dir))))
(setq auto-save-file-name-transforms
      `((".*"
	 ,(expand-file-name "auto-save/\\1" my-var-dir) t)))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(provide 'settings)
;;; settings.el ends here
