(use-package which-key
  :config (which-key-mode))

(use-package evil-easymotion
  :general
  (general-omap
    "j" #'evilem-motion-next-line
    "k" #'evilem-motion-previous-line))

(use-package avy
  :general
  (general-nmap
    "C-f" #'evil-avy-goto-char-in-line
    "C-/" #'evil-avy-goto-char-timer)
  :config
  (setq avy-keys-alist
        '((avy-goto-char . (?u ?h ?e ?t ?o ?n ?a ?s))))
  (setq avy-keys '(?u ?h ?e ?t ?o ?n ?a ?s))
  (setq avy-enter-times-out t)
  (setq avy-timeout-seconds 1)
  (setq aw-keys '(?u ?h ?e ?t ?o ?n ?a ?s))
  (setq avy-flyspell-correct-function #'flyspell-correct-at-point)
  (defun dc-avy-action-kill-move (pt)
    "Kill sexp at PT and move there."
    (goto-char pt)
    (avy-forward-item)
    (kill-region pt (point))
    (message "Killed: %s" (current-kill 0))
    (point)
    (evil-insert-state))

  (setq avy-dispatch-alist
        '((?c . dc-avy-action-kill-move)
          (?d . avy-action-kill-stay)
          (?g . avy-action-teleport)
          (?m . avy-action-mark)
          (?n . avy-action-copy)
          (?y . avy-action-yank)
          (?i . avy-action-ispell)
          (?z . avy-action-zap-to-char))))

(use-package ace-window
  :general
  (general-nmap
    :prefix "C-w"
    "C-w" #'ace-window)
  :config
  (setq aw-keys '(?a ?o ?e ?u ?h ?t ?n ?s)))

(provide 'ui)
