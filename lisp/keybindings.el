;;   general   for binding keys also look into use-package's method might not be needed.
;;   evil

(use-package general
  :config (general-evil-setup)
  (general-create-definer my-leader-def
    :wrapping general-nvmap
    :prefix "SPC")
  (general-create-definer my-local-leader-def
    :wrapping general-nvmap
    :prefix "C-SPC"))

(winner-mode t)
(use-package evil
  :general
  (general-nmap "J" #'evil-scroll-line-down
                "K" #'evil-scroll-line-up
                "H" #'evil-beginning-of-visual-line
                "L" #'evil-end-of-line-or-visual-line
                "gj" #'evil-join
                :prefix "C-w"
                "u" #'winner-undo
                "C-r" #'winner-redo)
  :config
  (evil-mode 1))

(provide 'keybindings)
