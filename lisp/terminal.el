;;; terminal.el --- Manage external terminals -*- lexical-binding: t; -*-

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
;; Integrate with StumpWM to manage external terminals in a project knowing
;; manner.

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
