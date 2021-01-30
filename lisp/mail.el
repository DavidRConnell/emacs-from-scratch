;;; -*- lexical-binding: t; -*-

(use-package mu4e
  :general
  (my-leader-def
    "m" #'mu4e)
  :config
  (require 'org-mu4e)
  ;; (require 'mu4e-contrib)
  ;; (require 'smtpmail)

  (general-nmap
    :keymaps 'mu4e-view-mode-map
    "J" #'evil-scroll-down)

  (setq mu4e-attachment-dir "/tmp/"
	mu4e-compose-complete-addresses t
	mu4e-compose-dont-reply-to-self t
	mu4e-get-mail-command "mbsync -a"
	mu4e-completing-read-function #'completing-read
	mu4e-compose-signature "Thank you\nDavid R. Connell\n")

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
		    (mu4e-sent-folder . "/gmail/[Gmail].Sent Mail")
		    (mu4e-drafts-folder . "/gmail/Drafts")
		    (mu4e-refile-folder . "/gmail/Archive")))
	  ,(make-mu4e-context
	    :name "rush"
	    :enter-func (lambda () (mu4e-message "Entering rush context"))
	    :leave-func (lambda () (mu4e-message "Leaving rush context"))
	    :match-func (lambda (msg)
			  (when msg
			    (string-prefix-p "/rush"
					     (mu4e-message-field msg :maildir))))
	    :vars '((user-mail-address . "david_r_connell@rush.edu")
		    (mu4e-trash-folder . "/rush/Trash")
		    (mu4e-sent-folder . "/rush/Sent")
		    (mu4e-drafts-folder . "/rush/Drafts")
		    (mu4e-refile-folder . "/rush/Archive")))))

  (defvar my-mu4e-account-alist))

(use-package org-msg)

(provide 'mail)
;;; mail.el ends here
