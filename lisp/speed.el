;; speed up startup (it's already slow).

(use-package gcmh
  :config
  (setq gcmh-idle-delay 5
      gcmh-high-cons-threshold (* 16 1024 1024)))  ; 16mb

(provide 'speed)
