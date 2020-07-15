(require 'package)
(add-to-list 'package-archives
             ' ("melpa" . "https://melpa.org/packages/"))

(package-initialize)
(package-refresh-contents)

(defmacro my-add-package (name)
  "Install package NAME if it does hasn't already been installed"
  `(unless (package-installed-p ',name)
     (package-install ',name)))

(my-add-package use-package)
(my-add-package evil)
(my-add-package magit)
(my-add-package evil-magit)
(my-add-package git-gutter)
(my-add-package gcmh)

(provide 'my-packages)
