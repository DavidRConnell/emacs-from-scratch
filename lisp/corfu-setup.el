;;; Corfu --- code for seting up corfu completion -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package corfu
  :config
  (general-define-key
   :keymaps corfu-map
   "C-SPC" corfu-complete)
  (general-imap
    "C-SPC" #'completion-at-point)
  (setq corfu-cylce t
	corfu-auto t
	corfu-echo-documentation t
	corfu-quit-no-match t)

  (corfu-global-mode 1))

(use-package orderless
  :init
  (setq completion-styles '(orderless)
	completion-category-defaults nil
	completion-category-overrides '((file (styles . (pratial-completion))))))

(provide 'corfu-setup)
;;; corfu-setup.el ends here
