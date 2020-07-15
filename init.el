(require 'package)
(add-to-list 'package-archives
             ' ("melpa" . "https://melpa.org/packages/"))

(package-initialize)
(package-refresh-contents)

;; use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; evil
(unless (package-installed-p 'evil)
  (package-install 'evil))

(use-package evil
  :config
  (evil-mode 1))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (evil))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
