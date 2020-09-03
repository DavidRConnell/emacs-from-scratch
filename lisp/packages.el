;;; Declare packages installed packages here.

(require 'package)
(add-to-list 'package-archives
             ' ("melpa" . "https://melpa.org/packages/"))

(package-initialize)
(package-refresh-contents)

(defmacro my-add-package (name)
  "Install package NAME if it does hasn't already been installed.
Is there a way to parse config to look for file names? Install those
names if not installed and delete installed package that are installed
but not in config?"
  `(unless (package-installed-p ',name)
     (package-install ',name)))

(my-add-package use-package)
(my-add-package evil)
(my-add-package magit)
(my-add-package evil-magit)
(my-add-package git-gutter)
(my-add-package which-key)
;; (my-add-package helpful)
;; (my-add-package elisp-demos)
;; (my-add-package elisp-def)
;; (my-add-package macrostep)
;; (my-add-package sly)
;; (my-add-package lispy)
;; (my-add-package lispyville)
;; (my-add-package overseer)
;; (my-add-package buttercup)
(my-add-package gcmh)

(provide 'packages)
