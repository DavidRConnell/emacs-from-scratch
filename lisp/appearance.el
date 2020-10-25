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

(defun my-mode-line-fill ()
  "Do stuff"
  (let* ((line-num-len (+ 4 1 3))
	 (perc-len 6)
	 (spaces (- (window-total-width)
		 (+ 2 (length (buffer-name))
		    (length (format "%s" mode-name))
		    1 line-num-len 1 perc-len))))
    (make-string spaces ? )))

(setq-default mode-line-format
	      '((:eval
		 (propertize "  %b" 'face
				   (if (buffer-modified-p)
				       'success
				     'nil)))
		(:eval (propertize (my-mode-line-fill)))
		(:propertize "%m")
		(:propertize " %4l:")
		(:eval (propertize "%3c" 'face
				   (if (>= (current-column) fill-column)
				       'warning
				     'nil)))
		(:propertize " %p")))

(blink-cursor-mode -1)
(global-display-fill-column-indicator-mode t)

(provide 'appearance)
