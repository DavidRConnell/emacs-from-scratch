;;   general   for binding keys also look into use-package's method might not be needed.
;;   evil

(use-package general
  :config
  (general-evil-setup)
  (defconst dc-leader "SPC")
  (defconst dc-local-leader ","))

(use-package evil
  :general
  (general-nmap "J" #'evil-scroll-line-down
                "K" #'evil-scroll-line-up
                "H" #'evil-beginning-of-visual-line
                "L" #'evil-end-of-line-or-visual-line
                "gj" #'evil-join)
  :config
  (evil-mode 1))

(provide 'keybindings)
