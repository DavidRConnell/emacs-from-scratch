(dolist (mode '(tool-bar-mode blink-cursor-mode scroll-bar-mode))
  (when (fboundp mode)
    (funcall mode -1)))

(defun my-set-theme (&optional type)
  "Load a theme. Select a TYPE, light or dark."
  (interactive)
  (let ((light  'modus-operandi)
        (dark 'modus-vivendi))
  (if (not type)
      (setq type (completing-read "Type: " '(light dark))))
  (if (eq type 'light)
      (load-theme light t)
    (load-theme dark t))))

(my-set-theme 'light t)

;; Fonts
;; Mode line

(provide 'appearance)
