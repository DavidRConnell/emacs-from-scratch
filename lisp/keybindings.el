;;; keybindings --- for general key bindings -*- lexical-binding: t; -*-
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
    :prefix "C-SPC")
  (general-create-definer general-nmmap
    :states '(normal motion))
  (general-create-definer general-nmvmap
    :states '(normal motion visual)))

(fset #'yes-or-no-p #'y-or-n-p)

(winner-mode t)
(use-package evil
  :init
  (setq evil-want-keybinding nil
	evil-search-module 'evil-search
	;; evil-undo-system 'undo-fu
	)
  :config
  (general-nmvmap
    :keymaps 'override
    "J" #'evil-scroll-line-down
    "K" #'evil-scroll-line-up
    "H" #'evil-beginning-of-visual-line
    "L" #'evil-end-of-line-or-visual-line
    "M" #'evil-goto-mark)
  (general-imap
    "C-u" #'evil-delete-back-to-indentation)
  (general-nmvmap
    "gj" #'evil-join
    "j" #'evil-next-visual-line
    "k" #'evil-previous-visual-line
    "-" #'dired-jump
    "C-m" #'evil-goto-mark
    "C-q" #'evil-execute-macro)
  (general-nmmap
    :prefix "C-w"
    "u" #'winner-undo
    "C-r" #'winner-redo)
  (general-nmap
    :keymaps 'emacs-lisp-mode-map
    :prefix "C-c"
    "C-b" #'eval-buffer)
  (my-leader-def
    "b" #'switch-to-buffer
    "w" #'save-buffer
    "q" (lambda () (interactive) (kill-buffer))
    "Q" #'evil-delete-buffer
    "o" #'find-file
    "l" #'imenu
    ";" #'eval-expression)

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
