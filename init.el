;;;; init.el --- Load modules -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(add-to-list 'load-path
             (expand-file-name "lisp/" user-emacs-directory))

(require 'variables)
(require 'packages)
(require 'tidy-files)
(require 'keybindings)
(require 'ui)
(require 'pass)
(require 'projects)
(require 'appearance)
(require 'git)
(require 'development)
(require 'writting)
(require 'notes)
(require 'references)
(require 'file-templates)
(require 'mail)
(require 'terminal)
(require 'rss)
(require 'chat)
(require 'completion)
(require 'speed)

(let ((inhibit-message t))
  (message (format "Initialization time: %s"
                   (emacs-init-time))))

(provide 'init)
;;; init.el ends here
