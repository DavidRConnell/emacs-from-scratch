;;;; init.el --- Load modules -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'package)
(setq package-archives nil
      package-enable-at-startup nil)
(package-initialize)

(add-to-list 'load-path
             (expand-file-name "lisp/" user-emacs-directory))

(require 'variables)
(require 'tidy-files)
(require 'keybindings)
(require 'ui)
(require 'git)
(require 'projects)
(require 'development)
(require 'debugging)
(require 'writting)
(require 'mail)
(require 'terminal)
(require 'completion)
(require 'rss)
(require 'appearance)  ;; take care of general emacs stuff.
(require 'speed)

(let ((inhibit-message t))
  (message (format "Initialization time: %s"
                 (emacs-init-time))))

(provide 'init)
;;; init.el ends here
