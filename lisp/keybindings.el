;;; package --- for general key bindings
;;; Commentary:
;; also look into use-package's method might not be needed.
;; evil
;;; Code

(use-package general
  :config (general-evil-setup)
  (general-create-definer my-leader-def
    :states '(normal visual motion)
    :keymaps 'override
    :prefix "SPC")
  (general-create-definer my-local-leader-def
    :states '(normal visual motion)
    :keymaps 'override
    :prefix "C-SPC"))

(winner-mode t)
(use-package evil
  :preface (setq evil-undo-system 'undo-tree)
  :init
  (setq evil-want-keybinding nil
	evil-respect-visual-line-mode t)
  :config
  (general-imap
    "C-u" #'evil-delete-back-to-indentation)
  (general-nmap
    "J" #'evil-scroll-line-down
    "K" #'evil-scroll-line-up
    "H" #'evil-beginning-of-visual-line
    "L" #'evil-end-of-line-or-visual-line
    "gj" #'evil-join)
  (general-nmap
    :prefix "C-w"
    "u" #'winner-undo
    "C-r" #'winner-redo)
  (my-leader-def
    "b" #'switch-to-buffer
    "w" #'save-buffer
    "q" #'evil-delete-buffer
    "o" #'find-file
    ";" #'eval-expression)
  (general-omap
    "j" #'avy-goto-line-below
    "k" #'avy-goto-line-above)
  (evil-mode 1)
  (setq evil-echo-area-message nil
        evil-echo-state nil)

  (setq evil-ex-search-vim-style-regexp t)

  (use-package evil-collection
    :config (evil-collection-init))

  (use-package evil-nerd-commenter
    :commands (evilnc-comment-or-uncomment-lines evilnc-comment-operator)
    :general
    (general-nmap
      :prefix "g"
      "c" (general-key-dispatch
	      #'evilnc-comment-operator
	    "c" #'evilnc-comment-or-uncomment-lines)))

  (use-package evil-goggles
    :init (setq evil-goggles-duration 0.1
                evil-goggles-pulse nil
                evil-goggles-enable-change nil
                evil-goggles-enable-delete nil)
    :config (evil-goggles-mode))

  (use-package evil-surround
    :config (global-evil-surround-mode 1)))

(provide 'keybindings)
