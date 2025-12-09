;;; mail.el --- Mailbox -*- lexical-binding: t; -*-

;; Copyright (C) 2020 David R. Connell
;;
;; Author: David R. Connell <david32@dcon.addy.io>

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
;; Set up for email reading and composing.

;;; Code:

(use-package mu4e
  :load-path "~/.nix-profile/share/emacs/site-lisp/mu4e"
  :commands mu4e mu4e-compose-new mu4e-update-index
  :general
  (my-leader-def
    "m" #'mu4e
    "M" #'mu4e-compose-new)
  :config
  (require 'org-mu4e)
  (require 'mu4e-contrib)
  (require 'smtpmail)

  (setq smtpmail-debug-info t
	smtpmail-stream-type 'starttls)
  (setq mail-user-agent 'mu4e-user-agent)
  (setq mu4e-hide-index-messages t)

  (setq message-kill-buffer-on-exit t
	message-send-mail-function #'smtpmail-send-it)

  (add-hook 'mu4e-view-mode-hook #'visual-line-mode)
  (general-nmap
    :keymaps 'mu4e-view-mode-map
    "J" #'evil-scroll-down
    "p" #'mu4e-view-save-attachments)

  (general-nmap
    :keymaps 'mu4e-main-mode-map
    :prefix "C-c"
    "C-j" #'mu4e~headers-jump-to-maildir)

  (setq mu4e-attachment-dir "/tmp/"
	mu4e-compose-complete-addresses t
	mu4e-compose-dont-reply-to-self t
	mu4e-get-mail-command "mbsync -a"
	mu4e-completing-read-function #'completing-read
	mu4e-compose-signature "Thank you\nDavid R. Connell\n"
	mu4e-change-filenames-when-moving t
	mu4e-sent-messages-behavior 'delete
	mu4e-view-show-images t
	mu4e-compose-format-flowed t)

  (add-to-list 'mu4e-view-actions '("View in browser" . mu4e-action-view-in-browser))

  (setq mu4e-contexts
	`(,(make-mu4e-context
	    :name "gmail"
	    :enter-func (lambda () (mu4e-message "Entering gmail context"))
	    :leave-func (lambda () (mu4e-message "Leaving gmail context"))
	    :match-func (lambda (msg)
			  (when msg
			    (string-prefix-p "/gmail"
					     (mu4e-message-field msg :maildir))))
	    :vars '((user-mail-address . "davidconnell12@gmail.com")
		    (mu4e-trash-folder . "/gmail/Trash")
		    (mu4e-sent-folder . "/gmail/[Gmail]/Sent Mail")
		    (mu4e-drafts-folder . "/gmail/Drafts")
		    (mu4e-refile-folder . "/gmail/Archive")
		    (smtpmail-smtp-user . "davidconnell12@gmail.com")
		    (smtpmail-smtp-service . 587)
		    (smtpmail-smtp-server . "smtp.gmail.com")))

	  ,(make-mu4e-context
	    :name "rush"
	    :enter-func (lambda () (mu4e-message "Entering rush context"))
	    :leave-func (lambda () (mu4e-message "Leaving rush context"))
	    :match-func (lambda (msg)
			  (when msg
			    (string-prefix-p "/rush"
					     (mu4e-message-field msg :maildir))))
	    :vars '((user-mail-address . "david_r_connell@rush.edu")
		    (mu4e-trash-folder . "/rush/Deleted Items")
		    (mu4e-sent-folder . "/rush/Outbox")
		    (mu4e-drafts-folder . "/rush/Drafts")
		    (mu4e-refile-folder . "/rush/Archive")
		    (smtpmail-smtp-user . "david_r_connell@rush.edu")
		    (smtpmail-smtp-server . "smtp.office365.com")
		    (smtpmail-smtp-service . 587)))))

  (defvar my-mu4e-account-alist)
  ;; (advice-add 'mu4e :before (window-configuration-to-register :pre-mu4e-config))
  ;; (general-define-key
  ;;  [remap mu4e-quit] #'(lambda () (interactive)
  ;; 			 (mu4e-quit)))
  )

(use-package org-msg
  ;; :straight t
  :hook (mu4e-compose-pre . org-msg-mode)
  :config
  (setq org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil \\n:t"
	org-msg-default-alternatives '(html text))
  (general-define-key
   :keymaps 'org-msg-edit-mode-map
   :state 'override
   :prefix "C-c"
   "C-e" #'org-msg-preview
   "C-k" #'org-msg-kill-buffer
   "C-a" #'org-msg-attach
   "C-c" #'message-send-and-exit))

(use-package ecomplete
  :after mu4e
  :hook (message-sent . message-put-addressess-in-ecomplete)
  :config
  (ecomplete-setup)
  (setq message-mail-alias-type nil
	message-expand-name-standard-ui t))

(provide 'mail)
;;; mail.el ends here
