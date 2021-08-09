;;; vertico --- Setup vertico completion -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package vertico
  :init
  (vertico-mode 1)
  :config
  (general-def
    :keymaps 'vertico-map
    "C-n" #'vertico-next
    "C-p" #'vertico-previous
    "C-SPC" #'minibuffer-complete
    "C-w" #'backward-kill-word)
  (my-leader-def
    "o" #'find-file))

(use-package orderless
  :init
  (setq completion-styles '(orderless)
	completion-category-defaults nil
	completion-category-overrides '((file (styles partial-completion)))))

(use-package savehist
  :config
  (savehist-mode 1))

(use-package embark
  :general
  (general-define-key
    :keymaps 'vertico-map
    "C-i" #'embark-act
    "C-j" #'embark-dwim)
  (general-define-key
    "C-h b" #'embark-bindings)
  (my-leader-def
    :infix "p"
    "g" #'consult-ripgrep)

  ;; (use-package embark-consult)
  )

(use-package marginalia
  :config
  (marginalia-mode 1)
  (general-define-key
    :keymaps 'minibuffer-local-map
    "M-a" #'marginalia-cycle))

(use-package mini-frame
  :config
  (setq mini-frame-show-parameters
	'((top . 0.4)
	  (width . 0.75)
	  (left . 0.5)))
  (mini-frame-mode 1))

(use-package consult
  :config
  (my-leader-def
    "b" #'consult-buffer
    "O" #'consult-locate
    "l" #'consult-imenu
    "L" #'consult-imenu-multi)
  (general-nmmap
    "/" #'consult-line))

(use-package flyspell-correct
  :general
  (general-nmap
    "z=" #'flyspell-correct-wrapper))

(provide 'vertico-setup)
;;; vertico-setup.el ends here
