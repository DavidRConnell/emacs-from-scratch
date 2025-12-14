;;; my-terminal.el --- Manage external terminals -*- lexical-binding: t; -*-

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
  "Split WORKING-DIRECTORY into a path project name and remote."
  (let* ((parts (cdr (f-split working-directory)))
	 (ssh-parts (string-split (car parts) ":"))
	 (ssh-p (string= (car ssh-parts) "ssh"))
	 (ssh-remote (if ssh-p (cadr ssh-parts) nil))
	 (project-name (concat (if ssh-p (concat ssh-remote ":") "")
			       (car (last parts))))
	 (stripped-working-directory
	  (if ssh-p
	      (mapconcat (lambda (p) (concat (f-path-separator) p))
			 (cdr parts))
	    working-directory)))
    (list stripped-working-directory project-name ssh-remote)))

(defun my-term (&optional working-directory)
  "Open a new alacritty window in `default-directory' or WORKING-DIRECTORY."
  (interactive)

  (let* ((working-directory (or working-directory default-directory))
	 (working-directory-parts (my--working-directory-name
				   working-directory))
	 (working-directory (car working-directory-parts))
	 (project (cadr working-directory-parts))
	 (remote-name (caddr working-directory-parts))
	 (proc-name (format "alacritty-%s" project))
	 (alacritty-title (format "*project-%s*" project))
	 (common-args (list proc-name nil "alacritty"
			    (format "--title=%s" alacritty-title)))
	 (args (append common-args
		       (if remote-name
			   (list "--command" "ssh"
				 "-t" remote-name
				 "cd" working-directory ";" "zsh" "--login")
			 (list "--working-directory" working-directory)))))

    (if (get-process proc-name)
	(shell-command (format "stumpish select-window-by-name \"%s\"" alacritty-title))
      (apply 'start-process args))))

(provide 'my-terminal)
;;; my-terminal.el ends here
