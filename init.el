(add-to-list 'load-path
             (expand-file-name "lisp/" user-emacs-directory))

(require 'packages)    ;; declare packages
(require 'keybindings)
(require 'appearance)  ;; take care of general emacs stuff.
(require 'ui)
(require 'git)
(require 'development)
(require 'completion)
(require 'speed)

(provide 'init)
;;; init.el ends here
