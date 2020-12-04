;;; rss.el --- description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 David R. Connell
;;
;; Author: David R. Connell <http://github/voidee>
;; Maintainer: David R. Connell <voidee@TheVoid>
;; Created: July 28, 2020
;; Modified: July 28, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/voidee/rss
;; Package-Requires: ((emacs 26.3) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  description
;;
;;; Code:

;; elfeed

(use-package elfeed
  :general
  (my-leader-def
    "e" #'elfeed)
  (general-define-key
   :keymaps 'elfeed-show-mode-map
   "gr" #'elfeed-update)
  :config
  (setq elfeed-db-directory
	(expand-file-name "elfeed/db/" my-cache-dir)
	elfeed-enclusure-default-dir
	(expand-file-name "elfeed/enclusures/" my-cache-dir)
	elfeed-search-filter "@2-week-ago")
  (use-package elfeed-org
    :preface
    (setq rmh-elfeed-org-files (list "~/org/elfeed.org"))))

(provide 'rss)
;;; rss.el ends here
