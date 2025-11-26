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

(defun my--working-directory-name (working-directory)
  (let* ((parts (f-split working-directory))
	 (ssh-parts (if (length> parts 1)
			(string-split (cl-second parts) ":")
		      (list "nil")))
	 (ssh-p (string= (cl-first ssh-parts) "ssh"))
	 (ssh-remote (if ssh-p (cl-second ssh-parts) nil))
	 (project-name (concat (car (last parts)) (if ssh-remote "-ssh" "")))
	 (stripped-working-directory
	  (if ssh-remote (mapconcat 'identity
				    (cl-loop for n from 2 to (- (length parts) 1)
					     collect (concat "/" (nth n parts))) "")
	    working-directory)))
    (list stripped-working-directory project-name ssh-remote)))

(defun my-term (&optional working-directory)
  "Open a new alacritty window in `default-directory' or WORKING-DIRECTORY."

  (interactive)
  (if (not working-directory)
      (setq working-directory default-directory))

  (let* ((working-directory-parts (my--working-directory-name
				   working-directory))
	 (working-directory (cl-first working-directory-parts))
	 (project-name (cl-second working-directory-parts))
	 (remote-name (cl-third working-directory-parts))
	 (proc-name (format "alacritty-%s" project-name))
	 (alacritty-title (format "*project-%s*" project-name)))
    (message "%s %s" alacritty-title remote-name)
    (if (get-process proc-name)
	(shell-command (format "stumpish select-window-by-name \"%s\"" alacritty-title))
      (start-process  proc-name nil "alacritty"
		      (format "--title=%s" alacritty-title)
		      (if remote-name
			  (format "--command=ssh %s" remote-name)
			(format "--working-directory=%s" working-directory))))))

(provide 'terminal)
;;; terminal.el ends here
