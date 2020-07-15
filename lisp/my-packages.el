(provide 'my-packages)

(require 'package)
(add-to-list 'package-archives
             ' ("melpa" . "https://melpa.org/packages/"))

(package-initialize)
(package-refresh-contents)

(defmacro dc-package (name)
  "Install package NAME if it does hasn't already been installed"
  `(unless (package-installed-p ',name)
     (package-install ',name)))

(dc-package use-package)
(dc-package evil)
(dc-package magit)
(dc-package evil-magit)
(dc-package git-gutter)
