;;; development --- Setup development enviornment. -*- lexical-binding: t; -*-
;;; Commentary:
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

(use-package ess
  :mode ("\\.r\\'" . ess-r-mode)
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
  :hook (python-mode . my-format-all-setup)
  :config

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
  (general-nmap
   :keymaps 'pdf-view-mode-map
   "r" #'org-ref-pdf-to-bibtex)
  (use-package saveplace-pdf-view))

(use-package sh-script
  :straight nil
  :hook (sh-mode . my-format-all-setup)
  :mode ("\\.bats\\'" . sh-mode)
  :config
  (setq sh-indent-after-continuation 'always)
  (add-hook 'sh-mode-hook
	    (defun my-set-shell-formatter ()
	      (setq-local format-all-formatters
			  '(("Shell" beautysh))))
	    -10)

  (use-package company-shell
    :config
    (setq company-shell-delete-duplicates t)
    (add-hook 'sh-mode-hook
	      #'(lambda ()
		  (setq-local company-backends
			    '(company-shell
			      company-capf
			      company-yasnippet
			      company-files))))))

(use-package c-mode
  :straight nil
  :hook (c-mode . my-format-all-setup))

;; Testing might at hooks later.
(use-package tree-sitter
  :commands tree-sitter-hl-mode
  :general
  (my-leader-def
    :infix "h"
    "t" (defun my-tree-sitter-toggle ()
	  (interactive)
	  (prism-whitespace-mode -1)
	  (call-interactively #'tree-sitter-hl-mode)))
  :config
  (use-package tree-sitter-langs))

(use-package prism
  :straight (prism :host github :repo "alphapapa/prism.el")
  :commands (prism-whitespace-mode prism-mode)
(general-define-key
 :keymaps 'Info-mode-map
 "C-c C-o" #'Info-follow-nearest-node)

(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (add-to-list 'imenu-generic-expression
			 '("Package"
			   "^\\s-*(use-package \\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)[[:space:]\n]+[^)]"
			   1))))

(use-package hl-todo
    :hook (prog-mode . hl-todo-mode)
    :config
    (setq hl-todo-highlight-punctuation ":"
          hl-todo-keyword-faces
          `(("TODO"       warning bold)
            ("FIXME"      error bold)
            ("HACK"       font-lock-constant-face bold)
            ("REVIEW"     font-lock-keyword-face bold)
            ("NOTE"       success bold)
            ("DEPRECATED" font-lock-doc-face bold))))

(use-package nix-mode
  :hook format-all-mode
  :mode "\\.nix\\'"
  :general
  (my-leader-def
    :infix "h"
    "p" (defun my-prism-toggle ()
    "h" (defun my-find-home-manager ()
	  (interactive)
	  (if (string-match-p "lisp" mode-name)
	      (call-interactively #'prism-mode)
	    (progn
	      (tree-sitter-hl-mode -1)
	      (call-interactively #'prism-whitespace-mode))))))
	  (projectile-find-file-in-directory
	   (expand-file-name "nixpkgs"
			     (getenv "XDG_CONFIG_HOME")))))
  :config
  (my-local-leader-def
    :keymaps 'nix-mode-map
    "u" (defun my-update-home-manager ()
	  (interactive)
	  (async-shell-command "home-manager switch"))))

;; (use-package overseer)
;; (use-package buttercup)

(use-package matlab
  :mode ("\\.m\\'" . matlab-mode)
  :hook (matlab-mode . flymake-mode)
  :config
  (require 'matlab-shell)
  (require 'matlab-shell-gud)
  (require 'matlab-topic)
  ;; (add-hook 'matlab-shell-mode-hook (lambda () (load-library "matlab-load")))

  (cl-loop for hk in prog-mode-hook
	   do (add-hook 'matlab-mode-hook hk))

  (defun my-wrap-matlab-shell (fun &rest args)
    "Open matlab in project root instead of `default-directory' and return to
calling window."

    (let ((old-dir default-directory)
	  (win (selected-window))
	  (buff (buffer-name)))
      (cd (projectile-project-root))
      (apply fun args)
      (switch-to-buffer buff)
      (pop-to-buffer "*MATLAB*")
      (select-window win)
      (cd old-dir)))

  (advice-add #'matlab-shell :around #'my-wrap-matlab-shell)

  (require 'flymake-codeissues)
  (setq matlab-shell-command-switches
	'("-nodesktop" "-nosplash" "-softwareopengl")
	matlab-shell-tab-use-company nil)
  (setq matlab-indent-function-body t
	matlab-functions-have-end t
	matlab-fill-code t
	matlab-return-add-semicolon t
	matlab-highlight-cross-function-variables t)
  (defvar font-lock-reference-face nil
    "Empty var to stop error in matlab (treats as a var instead of font)")

  (require 'matlab-testing)
  (general-nmap
    :keymaps 'matlab-mode-map
    "gK" (defun my-matlab-help-at-point ()
	   (interactive)
	   (matlab-shell-describe-command
	    (matlab-read-word-at-point))))

  (general-nvmap
    :keymaps 'matlab-mode-map
    :prefix "C-c"
    "C-c" #'matlab-shell-run-cell
    "C-l" #'matlab-shell-save-and-go
    "C-j" #'mt-shell-run-line
    "C-r" #'mt-shell-run-region)

  (general-imap
    :keymaps 'matlab-shell-mode-map
    "C-SPC" #'matlab-shell-c-tab)

  (my-local-leader-def
    :keymaps 'matlab-mode-map
    :infix "t"
    "g" #'mt-toggle-test-file
    "G" #'(lambda () (interactive)
	    (counsel-find-file (mt-get-test-dir)))
    "t" #'(lambda () (interactive)
	    (mt-shell-run-tests "file" "Unit"))
    "T" #'(lambda () (interactive)
	    (mt-shell-run-tests "project" "Unit"))
    "i" #'(lambda () (interactive)
	    (mt-shell-run-tests "project" "Integration"))
    "f" #'(lambda () (interactive)
	    (mt-shell-run-tests "project" "Functional"))
    "p" #'(lambda () (interactive)
	    (mt-shell-run-performance-tests "file"))
    "P" #'(lambda () (interactive)
	    (mt-shell-run-performance-tests "project"))
    "r" #'(lambda () (interactive)
	    (mt-shell-run-tests "rerun")))

  (my-local-leader-def
    :keymaps 'matlab-mode-map
    "," #'matlab-shell
    "r" (lambda (arg) (interactive "P")
	  (if arg
	      (call-interactively #'mt-run-command)
	    (call-interactively #'mt-run-last-command)))
    "?" (lambda ()
	  (interactive)
	  (let ((mathworks-ref-prefix "https://www.mathworks.com/help/matlab/ref/"))
	    (eww (concat mathworks-ref-prefix (matlab-read-word-at-point) ".html")))))

  (defun my-matlab-insert-function-snippet ()
    "Add snippet for matlab function when opening a new .m file."
    (if (and (equal 0 (buffer-size))
	     (not (string-match-p "^\s*\\*.*\\*\s*$" (buffer-name))))
	(insert (concat "function " (file-name-sans-extension (buffer-name)) "\nend"))))

  (defun my-matlab-fix-imenu-generic-expression ()
    (setq imenu-generic-expression
	  '(("Function" "^\\s-*\\(function\\)\\s-*\\([^\\.\n]*\\)" 2)
	    ("Function" "^\\s-*\\(function\\)\\s-*\\([^\\.\n]*\\.\\.\\.\\s-*\n.*\\)" 2)
	    ("Class" "^\\s-*\\(classdef\\)\\s-*\\(.*\\)" 2)
	    ("Methods" "^\\s-*\\(methods\\)\\s-*\\(.*\\)?" 2)
	    ("Properties" "^\\s-*\\(properties\\)\\s-*\\(.*\\)?" 2)
	    ("Section" "^\\s-*%%\\s-*\\(.*\\)$" 1))))

  (add-hook 'matlab-mode-hook #'my-matlab-insert-function-snippet)
  (add-hook 'matlab-mode-hook #'my-matlab-fix-imenu-generic-expression))

(use-package cc-mode
  :config
  (use-package lsp-mode
    :hook (c-mode . lsp))
  (use-package ccls
    :hook (c-mode . (lambda () (require 'ccls) (lsp)))))

(use-package cypher-mode
  :straight (cypher-mode :host github :repo "fxbois/cypher-mode")
  :mode ("\\.cypher\\'" . cypher-mode)
  :config
  (set-face-attribute 'cypher-variable-face nil
		      :foreground 'unspecified :background 'unspecified
		      :family 'unspecified :slant 'unspecified
		      :weight 'normal :height 'unspecified
		      :underline 'unspecified :overline 'unspecified
		      :box 'unspecified :inherit font-lock-variable-name-face)
  (set-face-attribute 'cypher-pattern-face nil
		      :foreground 'unspecified :background 'unspecified
		      :family 'unspecified :slant 'unspecified
		      :weight 'bold :height 'unspecified
		      :underline 'unspecified :overline 'unspecified
		      :box 'unspecified :inherit font-lock-variable-name-face)
(defun cypher-send-buffer (&optional output-file)
  (interactive)
  (let ((file (buffer-name (current-buffer)))
	(database "neo4j")
	(command "cypher-shell --database=%s --file=%s"))
    (setq command (format command database file))
    (if output-file
	(setq command (concat command " > " output-file)))
    (async-shell-command command)))

  (general-def
    :keymaps 'cypher-mode-map
    :prefix "C-c"
    "C-c" #'cypher-send-buffer
    "C-e" (defun cypher-results-to-buffer (file) (interactive "F")
	    (cypher-send-buffer file))))

(provide 'development)
;;; development.el ends here
