;;; Corfu --- code for seting up corfu completion -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package corfu
  :init
  (corfu-global-mode 1)
  :config
  (general-define-key
    :keymaps 'corfu-map
    "C-n" #'corfu-next
    "C-p" #'corfu-previous
    "C-SPC" #'corfu-complete)
  (general-imap
    "C-SPC" #'completion-at-point)
  (setq  corfu-cylce t
	 corfu-auto t
	 corfu-echo-documentation t))
text-mode-

(use-package orderless
  :init
  (setq completion-styles '(orderless)
	completion-category-defaults nil
	completion-category-overrides '((file (styles . (pratial-completion))))))

(provide 'corfu-setup)
;;; corfu-setup.el ends here
