(use-package modus-operandi-theme)
(use-package modus-vivendi-theme)

(defvar my-light-theme 'modus-operandi
  "Default light theme.")
(defvar my-dark-theme 'modus-vivendi
  "Default dark theme.")

(defun my-set-theme (&optional type)
  "Load a theme. Select a TYPE, light or dark."
  (interactive)
  (if (not type)
      (setq type (completing-read "Type: " '(light dark))))
  (if (eq type 'light)
      (load-theme my-light-theme t)
    (load-theme my-dark-theme t)))

(my-set-theme 'light)

(use-package delight)

;; Fonts
;; Mode line

(provide 'appearance)
