;;; -*- lexical-binding: t; -*-
;; mu4e

(use-package mu4e
  :general
  (my-leader-def
    "m" #'mu4e)
  :config
  (require 'org-mu4e)
  (require 'mu4e-contrib)
  (require 'smtpmail)

  (setq mu4e-attachment-dir "/tmp/"
	mu4e-compose-complete-addresses t
	mu4e-compose-dont-reply-to-self t
	mu4e-get-mail-command "mbsync -a"
	mu4e-sent-folder "/Sent/"
	mu4e-drafts-folder "/Drafts/"
	mu4e-completing-read-function #'completing-read)

  (defvar my-mu4e-account-alist))

(use-package org-msg)

(provide 'mail)
;;; mail.el ends here
