;;; development --- Setup development enviornment. -*- lexical-binding: t; -*-
;;; Commentary:
;;   elisp development tools.  Watch JWiegley's videos
;;   some sort of eval program
;;   sly/lisp editing set-up.
;;   undo-tree
;;; Code:

(add-hook 'emacs-lisp-mode-hook #'eldoc-mode)

(use-package smartparens
  :config
  (setq smartparens-strict-mode t)
  (smartparens-global-mode 1)
  (require 'smartparens-config)

  (use-package evil-smartparens
    :config
    (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)))

(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  :config
  (setq flycheck-indication-mode 'right-fringe)
  (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
    [16 48 112 240 112 48 16] nil nil 'center))

(use-package emr
  :general
  (general-nmap
    "M-SPC" #'emr-show-refactor-menu)
  :config
  (my-local-leader-def
    :infix "r"
    :keymaps 'emacs-lisp-mode-map
    "v" #'emr-el-extract-variable
    "f" #'emr-el-extract-function))

(use-package elisp-def
  :hook (emacs-lisp-mode . elisp-def-mode))

;; :general (:keymaps 'elisp-def-mode-map "gd"))

;; (use-package elfmt
;;   :straight (elfmt :host github :repo "riscy/elfmt")
;;   :hook (emacs-lisp-mode . elfmt-mode))

(use-package highlight-function-calls
  :hook (emacs-lisp-mode . highlight-function-calls-mode)
  :config (use-package rainbow-identifiers
            :disabled :hook
            (prog-mode . rainbow-identifiers-mode)))

(use-package highlight-defined
  :hook (emacs-lisp-mode . highlight-defined-mode))

;; (use-package elsa
;;   :mode "\\.el\\'"
;;   :config (use-package flycheck-elsa
;;   :config (add-hook 'emacs-lisp-mode-hook #'flycheck-elsa-setup)))

(use-package helpful
;; (use-package macrostep)
  :general
  (general-nmap
    :prefix "C-h"
    "o" #'helpful-symbol
    "k" #'helpful-key
    "f" #'helpful-callable
    "v" #'helpful-variable))

(use-package elisp-demos
  :config
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))
;; (use-package sly)

(use-package lispy
  :hook ((emacs-lisp-mode lisp-mode)
         . lispy-mode)
  :config
  (use-package
    lispyville :hook (lispy-mode . lispyville-mode)
    :config
    (lispyville-set-key-theme
     '(operators
       c-w
       slurp/barf-lispy
       commentary
       prettify
       c-u))))

(use-package polymode
  :after org
  :config
  (use-package poly-org
    :hook (org-mode . poly-org-mode)))

(use-package format-all
  :config
  (defun my-format-all-setup ()
    (format-all-ensure-formatter)
    (format-all-buffer)
    (format-all-mode)))

(use-package lsp-mode
  :commands lsp-mode lsp-deferred)

(use-package ess
  :mode ("\\.r\\'" . ess-r-mode)
  :hook (ess-r-mode . lsp-deferred)
  :hook (ess-r-mode . my-format-all-setup)
  :config
  (general-define-key
   :keymaps 'ess-r-mode-map
   :prefix "C-c"
   "C-c" #'ess-eval-region-or-function-or-paragraph
   "C-a" (lambda () (interactive)
	   (insert "<- "))
   "C-p" (lambda () (interactive)
	   (end-of-line)
	   (insert " %>%")
	   (evil-normal-state)))

  (add-hook 'ess-help-mode-hook #'evil-motion-state)

  (general-nmap
   :keymaps 'ess-r-mode-map
   :prefix "g"
   "K" #'ess-help
   "V" #'ess-display-vignettes
   "o" #'ess-display-help-apropos)

  (general-mmap
   :keymaps 'ess-help-mode-map
   "q" #'kill-current-buffer
   "gK" #'ess-help
   "gV" #'ess-display-vignettes
   "go" #'ess-display-help-apropos)

  (add-hook 'ess-r-mode-hook
	    (lambda () (interactive)
	      (setq-local company-backends '(company-R-args
					     company-R-objects
					     company-dabbrev-code))))
  (advice-add #'ess-eval-region-or-function-or-paragraph
	      :before #'evil-set-jump)

  (advice-add #'ess-eval-region
	      :after #'my--goto-bottom-of-r-repl)

  (defun my--goto-bottom-of-r-repl (&rest _)
    "Scroll to the bottom of the repl after runnning a function.
This ensures the results are visible."
    (if (eq major-mode 'ess-r-mode)
	(let ((win (get-buffer-window (current-buffer))))
	  (ess-switch-to-end-of-ESS)
	  (select-window win))))

  (setq ess-offset-continued 'straight
	ess-nuke-trailing-whitespace-p t
	ess-style 'DEFAULT
	ess-history-directory (expand-file-name "ess-history/" my-var-dir))
  (use-package ess-R-data-view))

(use-package python
  :hook (python-mode . lsp-deferred)
  :hook (python-mode . my-format-all-setup)
  :config
  (defun my-set-python-library-directories ()
    "Determine library directories for lsp based on python shell interpreter."
    (let ((version-str (shell-command-to-string
			(concat python-shell-interpreter
				" --version")))
	  (major-version)
	  (minor-version)
	  (py-package-dir))

      (string-match
       "\\([0-9]\\)\\.\\([0-9]\\)"
       version-str)

      (setq major-version (match-string 1 version-str)
	    minor-version (match-string 2 version-str)
	    py-package-dir (format "python%s.%s/site-packages/"
				   major-version
				   minor-version))

      (setq lsp-clients-python-library-directories
	    `(,(expand-file-name py-package-dir "/usr/lib/")
	      ,(expand-file-name py-package-dir "~/.local/")))))

  (my-set-python-library-directories)

  (add-hook 'python-mode-hook (lambda ()
				(setq-local company-backends
					    '(company-capf
					      company-dabbrev))))

  (setq python-indent-guess-indent-offset-verbose nil
	org-babel-python-command python-shell-interpreter)

  (defun my-python-help (thing)
    "Open a help buffer for python THING."

    (interactive
     (list (let ((thing-candidate (python-eldoc--get-symbol-at-point)))
	     (read-string (concat
			   "Help on"
			   (if thing-candidate
			       (concat " (" thing-candidate ")")
			     "")
			   ": ")
			  nil
			  nil
			  thing-candidate))))
    (let* ((buff-name (format "*Python help for: %s*" thing))
	   (buff (get-buffer-create buff-name)))
      (with-current-buffer buff
	(insert
	 (python-shell-send-string-no-output (concat "help(" thing ")")))
	(python-mode)
	(flycheck-mode -1)
	(goto-line 1))
      (switch-to-buffer-other-window buff)))

  (general-nmap
    :keymaps 'python-mode-map
    :prefix "g"
    "K" #'my-python-help))

(use-package pdf-tools
  :straight nil
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (general-define-key
   :keymaps 'pdf-view-mode-map
   "r" #'org-ref-pdf-to-bibtex)
  (use-package saveplace-pdf-view))

;; (use-package overseer)
;; (use-package buttercup)

(provide 'development)
;;; development.el ends here
