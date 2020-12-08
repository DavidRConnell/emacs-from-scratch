(use-package modus-operandi-theme)
(use-package modus-vivendi-theme)

(defvar my-light-theme 'modus-operandi "Default light theme.")
(defvar my-dark-theme 'modus-vivendi "Default dark theme.")
(defvar my-current-theme-light-p nil)

(defun my-set-theme (&optional type)
  "Load a theme. Select a TYPE, light or dark."
  (interactive)
  (if (not type)
      (setq type (completing-read "Type: " '(light dark))))
  (if (string-equal (format "%s" type) "light")
      (progn
	(load-theme my-light-theme t)
	(setq my-current-theme-light-p t))
    (progn
      (load-theme my-dark-theme t)
      (setq my-current-theme-light-p nil))))

(defun my-toggle-theme ()
  "Toggle between my light and dark themes."
  (interactive)
  (if my-current-theme-light-p
      (my-set-theme 'dark)
    (my-set-theme 'light)))

(my-set-theme 'light)
(general-nmap :prefix "C-h" "t" #'my-toggle-theme)

(setq-default mode-line-format nil)
(blink-cursor-mode -1)
(global-display-fill-column-indicator-mode t)
(show-paren-mode t)

(require 'mode-line)

(provide 'appearance)
