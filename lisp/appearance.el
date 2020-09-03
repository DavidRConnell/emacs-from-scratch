(dolist (mode '(tool-bar-mode blink-cursor-mode scroll-bar-mode))
  (when (fboundp mode)
    (funcall mode -1)))

(defun dc-set-theme (&optional type)
  "Load a theme. Select a TYPE, light or dark."
  (interactive)
  (let ((light  'modus-operandi)
        (dark 'modus-vivendi))
  (if (not type)
      (setq type (completing-read "Type: " '(light dark))))
  (if (eq type 'light)
      (load-theme light)
    (load-theme dark))))

(dc-set-theme 'light)

;; Fonts
;; Mode line

(provide 'appearance)
