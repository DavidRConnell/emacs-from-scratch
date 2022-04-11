;;; terminal.el --- description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 David R. Connell
;;
;; Author: David R. Connell <http://github/voidee>
;; Maintainer: David R. Connell <voidee@TheVoid>
;; Created: July 28, 2020
;; Modified: July 28, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/voidee/terminal
;; Package-Requires: ((emacs 26.3) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  description
;;
;;; Code:

(defun my-term (&optional working-directory)
  "Open a new st window in `default-directory' or WORKING-DIRECTORY."

  (interactive)
  (if working-directory
      (let* ((dir-len (length (split-string working-directory "/")))
	     (last-dir (- dir-len 2)))
	(message "%d\n%d" dir-len last-dir)
	(start-process "st" nil "st"
		       (concat "-cst:" (nth last-dir (split-string working-directory "/")))
		       (concat  "-e cd " working-directory)))
    (start-process "st" nil "st")))

(use-package vterm
  :disabled
  :config
  (general-imap
    :keymaps 'vterm-mode-map
    "C-SPC" #'vterm-send-tab
    "C-y" #'vterm-yank))

(provide 'terminal)
;;; terminal.el ends here
