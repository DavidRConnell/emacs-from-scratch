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

(require 'f)

(defun my-term (&optional working-directory)
  "Open a new alacritty window in `default-directory' or WORKING-DIRECTORY."

  (interactive)
  (if working-directory
      (let* ((project-name (car (last (f-split working-directory))))
	     (proc-name (format "alacritty-%s" project-name))
	     (alacritty-title (format "*project-%s*" project-name)))
	(if (get-process proc-name)
	    (shell-command (format "stumpish select-window-by-name \"%s\"" alacritty-title))
	  (start-process  proc-name nil "alacritty"
			  (format "--title=%s" alacritty-title)
			  (format "--working-directory=%s" working-directory)
			  "--command=tmux")))
    (start-process "alacritty" nil "alacritty")))
  :disabled
  :config

(provide 'terminal)
;;; terminal.el ends here
