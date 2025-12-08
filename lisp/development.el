;;; development --- Setup development enviornment. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(add-hook 'emacs-lisp-mode-hook #'eldoc-mode)
(add-hook 'emacs-lisp-mode-hook #'format-all-mode)
(add-hook 'emacs-lisp-mode-hook #'flymake-mode)

(setq major-mode-remap-alist
      '((yaml-mode . yaml-ts-mode)
	(toml-mode . toml-ts-mode)
	(sh-mode . bash-ts-mode)
	(python-mode . python-ts-mode)
	(c-mode . c-ts-mode)))

(add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-ts-mode))

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

;; (use-package elfmt
;;   :straight (elfmt :host github :repo "riscy/elfmt")
;;   :hook (emacs-lisp-mode . elfmt-mode))

(use-package highlight-function-calls
  :hook (emacs-lisp-mode . highlight-function-calls-mode))

(use-package highlight-defined
  :hook (emacs-lisp-mode . highlight-defined-mode))

(use-package helpful
  :general
  (general-define-key
   [remap describe-symbol] #'helpful-symbol
   [remap describe-key] #'helpful-key
   [remap describe-function] #'helpful-callable
   [remap describe-variable] #'helpful-variable))

(use-package elisp-demos
  :config
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))
;; (use-package macrostep)

(use-package geiser
  :hook (scheme-mode . geiser-mode)
  :config
  ;; (general-define-key
  ;;  :keymaps 'lispyville-mode-map
  ;;  "e" #'geiser-eval-definition)
  (general-nmap
    :keymaps 'geiser-mode-map
    "K" #'evil-scroll-line-up)
  (use-package emr
    :general
    (general-nmap
      :keymaps 'geiser-mode-map
      "M-SPC" #'emr-show-refactor-menu)
    :config
    (my-local-leader-def
      :keymaps 'geiser-mode-map
      :infix "r"
      "v" #'emr-scm-extract-variable
      "f" #'emr-scm-extract-function)))

(use-package lispy
  :hook ((emacs-lisp-mode lisp-mode geiser-mode)
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

(use-package format-all
  :hook (format-all-mode . format-all-ensure-formatter)
  :commands format-all-buffer format-all-mode
  :general
  (my-leader-def
    "f" #'format-all-buffer))

(use-package cmake-ts-mode
  :hook (cmake-ts-mode . format-all-mode)
  :hook (cmake-ts-mode . eglot-ensure))

(use-package ess
  :mode ("\\.r\\'" . ess-r-mode)
  :hook (ess-r-mode . format-all-mode)
  :config
  (require 'ess-r-mode)
  (general-define-key
   :keymaps '(ess-r-mode-map inferior-ess-r-mode-map)
   :prefix "C-c"
   "C-a" (lambda () (interactive)
	   (fixup-whitespace)
	   (insert " <- "))
   "C-p" (lambda (arg)
	   (interactive "P")
	   (if (not arg)
	       (end-of-line))
	   (delete-horizontal-space)
	   (insert " |>")))

  (general-define-key
   :keymaps 'ess-r-mode-map
   :prefix "C-c"
   "C-c" #'ess-eval-region-or-function-or-paragraph
   "c" #'ess-eval-region-or-function-or-paragraph-and-step
   "j" #'ess-eval-line-and-step)

  (my-local-leader-def
    :keymaps 'ess-r-mode-map
    "," #'R)

  (my-local-leader-def
    :keymaps 'ess-r-mode-map
    "t" (lambda ()
	  (interactive)
	  (ess-send-string (ess-get-process) (format "use_test(\"%s\")" (buffer-name))))
    "T" #'ess-r-devtools-test-package
    "c" #'ess-r-devtools-check-package
    "l" #'ess-r-devtools-load-package
    "u" #'ess-r-devtools-unload-package
    "b" #'ess-r-devtools-build
    "d" #'ess-r-devtools-document-package)

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
  :mode ("\\.py\\'" . python-mode)
  :hook (python-base-mode . eglot-ensure)
  :hook (python-base-mode . format-all-mode)
  :hook (python-base-mode . (lambda () (setq-local format-all-formatters '(("Python" isort black)))))
  :config
  (setq python-indent-guess-indent-offset-verbose nil
	python-shell-interpreter "ipython"
	python-shell-interpreter-args "-i --simple-prompt --InteractiveShell.display_page=True"
	python-shell-completion-native-enable nil
	org-babel-python-command python-shell-interpreter)

  (defun my-wrap-run-python (fun &rest args)
    "Open python in project root instead of `default-directory' and return to
calling window."

    (let ((old-dir default-directory)
	  (win (selected-window)))
      (cd (projectile-project-root))
      (apply fun args)
      (select-window win)
      (cd old-dir)))

  (advice-add #'run-python :around #'my-wrap-run-python)

  (my-local-leader-def
    :keymaps 'python-base-mode-map
    "," #'run-python
    "i" #'py-isort-buffer)

  (general-nmap
    :keymaps 'python-base-mode-map
    :prefix "C-c"
    "C-j" #'python-shell-send-statement
    "C-c" #'python-shell-send-region
    "C-f" #'python-shell-send-defun
    "C-b" #'python-shell-send-buffer)

  (use-package python-pytest
    :general
    (my-local-leader-def
      :keymaps 'python-base-mode-map
      "t" #'python-pytest-dispatch)
    :config
    (setq projectile-test-prefix-function
	  (defun projectile-test-prefix-with-fallback (project-type)
	    "Find default test files prefix based on PROJECT-TYPE."
	    (projectile-project-type-attribute project-type 'test-prefix "test_"))))

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
    :keymaps 'python-base-mode-map
    :prefix "g"
    "?" #'my-python-help
    "K" #'python-describe-at-point))

(use-package py-vterm-interaction
  :disabled
  :hook (python-mode . py-vterm-interaction-mode)
  :commands py-vterm-interaction-repl
  :config
  (setq-default py-vterm-interaction-repl-program python-shell-interpreter
		py-vterm-interaction-silent-cells t)

  (defun my-wrap-run-python (fun &rest args)
    "Open python in project root instead of `default-directory' and return to
calling window."

    (let ((old-dir default-directory)
	  (win (selected-window)))
      (cd (projectile-project-root))
      (apply fun args)
      (select-window win)
      (cd old-dir)))

  (advice-add #'py-vterm-interaction-repl :around #'my-wrap-run-python)
  (defalias 'py-vterm-interaction-repl 'run-python))

(use-package cython-mode)

(use-package sh-script
  :magic ("#!/usr/bin/env bash" . sh-mode)
  :hook ((bash-ts-mode sh-mode) . format-all-mode)
  :hook ((bash-ts-mode sh-mode) . eglot-ensure)
  :config
  (setq sh-indent-after-continuation 'always)
  (require 'man)
  (general-nmmap
    :keymaps '(sh-mode-map bash-ts-mode Man-mode-map)
    "gK" #'(lambda (arg)
	     (interactive "P")
	     (my-man-at-point arg 1)))
  (add-hook 'sh-mode-hook
	    (defun my-set-shell-formatter ()
	      (setq-local format-all-formatters
			  '(("Shell" shfmt))))
	    -10))

(use-package direnv
  :config
  (add-to-list 'auto-mode-alist '("\\.envrc'" . direnv-envrc-mode))
  (add-to-list 'auto-mode-alist '("\\direnvrc'" . direnv-envrc-mode))
  (direnv-mode))

;; Testing might add hooks later.
(use-package prism
  :disabled
  ;; :straight (prism :host github :repo "alphapapa/prism.el")
  :commands (prism-whitespace-mode prism-mode)
  ;; :general
  ;; (my-leader-def
  ;;   :infix "h"
  ;;   "p" (defun my-prism-toggle ()
  ;; 	  (interactive)
  ;; 	  (if (string-match-p "lisp" mode-name)
  ;; 	      (call-interactively #'prism-mode)
  ;; 	    (progn
  ;; 	      (tree-sitter-hl-mode -1)
  ;; 	      (call-interactively #'prism-whitespace-mode)))))
  )

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
  :hook (text-mode . hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        `(("TODO"       warning bold)
          ("FIXME"      next-error bold)
          ("HACK"       font-lock-constant-face bold)
          ("REVIEW"     hydra-face-red bold)
          ("NOTE"       success bold)
	  ("WARNING"    hydra-face-red bold)
          ("DEPRECATED" font-lock-doc-face bold)
	  ("TEMP"       modus-themes-prominent-warning bold))))

(use-package nix-mode
  :hook (nix-mode . eglot-ensure)
  :hook (nix-mode . format-all-mode)
  :hook (nix-mode . (lambda () (setq-local format-all-formatters '(("Nix" nixfmt)))))
  :mode "\\.nix\\'"
  :general
  (my-leader-def
    "h" (defun my-find-home-manager ()
	  (interactive)
	  (projectile-find-file-in-directory
	   (expand-file-name "nixpkgs"
			     (getenv "XDG_CONFIG_HOME"))))
    "x" #'nix-flake)
  :config
  (require 'nix-flake))

(use-package matlab
  :mode ("\\.m\\'" . matlab-mode)
  :hook (matlab-mode . flymake-mode)
  :config
  (require 'matlab-shell)
  (require 'matlab-shell-gud)
  (require 'matlab-topic)
  (require 'mlint)

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

  (general-nmmap
    :keymaps '(matlab-mode-map matlab-ts-mode-map matlab-shell-mode-map matlab-shell-help-mode-map)
    "gK" (defun my-matlab-help-at-point ()
	   (interactive)
	   (matlab-shell-describe-command
	    (matlab-read-word-at-point)))
    "gD" #'matlab-shell-locate-fcn
    "gr" #'xref-find-references)

  (general-nvmap
    :keymaps '(matlab-mode-map matlab-ts-mode-map)
    :prefix "C-c"
    "C-c" #'matlab-shell-run-cell
    "C-b" #'matlab-shell-save-and-go
    "C-j" #'mt-shell-run-line
    "C-r" #'mt-shell-run-region)

  (defun my-matlab-capfs ()
    (setq-local completion-at-point-functions
		(list
		 ;; (cape-company-to-capf #'company-matlab-shell)
		 (cape-company-to-capf #'company-semantic)
		 (cape-company-to-capf #'company-yasnippet)
		 #'cape-keyword
		 #'cape-dabbrev
		 #'cape-file)
		cape-keyword-list '((matlab-mode
				     "break" "case" "catch" "classdef" "continue"
				     "else" "elseif" "end" "for" "function" "global"
				     "if" "otherwise" "parfor" "persistent" "return"
				     "spmd" "switch" "try" "while"))))

  (add-hook 'matlab-mode-hook #'my-matlab-capfs 100)
  (add-hook 'matlab-mode-hook #'semantic-mode)
  (add-hook 'matlab-shell-mode-hook
	    (lambda ()
	      (setq-local completion-at-point-functions
			  (list (cape-company-to-capf #'company-matlab-shell)
				#'cape-dabbrev
				#'cape-history
				#'cape-file))))

  (my-local-leader-def
    :keymaps '(matlab-mode-map matlab-ts-mode-map)
    :infix "t"
    "g" #'mt-toggle-test-file
    "G" #'(lambda () (interactive)
	    (find-file (mt-get-test-dir)))
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
	    (mt-shell-run-tests "rerun" "")))

  (my-local-leader-def
    :keymaps '(matlab-mode-map matlab-ts-mode-map)
    :infix "p"
    "o" #'(lambda () (interactive)
	    (mt-run-command "profile clear; profile on"))
    "s" #'(lambda () (interactive)
	    (mt-run-command "profile off; profsave(profile('info'), '/tmp/matlab/profile_results')"))
    "v" #'(lambda () (interactive)
	    (consult-file-externally "/tmp/matlab/profile_results/file0.html")))

  (my-local-leader-def
    :keymaps '(matlab-mode-map matlab-shell-mode-map matlab-ts-mode-map)
    :infix "d"
    "b" #'mlgud-break
    "r" #'mlgud-remove
    "f" #'mlgud-up
    "d" #'mlgud-down
    "s" #'mlgud-step
    "n" #'mlgud-next
    "u" #'mlgud-until
    "c" #'mlgud-cont
    "q" #'mlgud-finish)

  (my-local-leader-def
    :keymaps '(matlab-mode-map matlab-ts-mode-map)
    "," #'matlab-shell
    "r" (lambda (arg) (interactive "P")
	  (if arg
	      (call-interactively #'mt-run-command)
	    (call-interactively #'mt-run-last-command)))
    "?" (lambda ()
	  (interactive)
	  (let ((mathworks-ref-prefix "https://www.mathworks.com/help/matlab/ref/"))
	    (mozilla-readable (concat mathworks-ref-prefix (matlab-read-word-at-point) ".html")))))

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
	    ("Arguments" "^\\s-*\\(arguments\\)\\s-*\\(.*\\)" 2)
	    ("Methods" "^\\s-*\\(methods\\)\\s-*\\(.*\\)?" 2)
	    ("Properties" "^\\s-*\\(properties\\)\\s-*\\(.*\\)?" 2)
	    ("Section" "^\\s-*%%\\s-*\\(.*\\)$" 1))))

  (add-hook 'matlab-mode-hook #'my-matlab-insert-function-snippet)
  (add-hook 'matlab-mode-hook #'my-matlab-fix-imenu-generic-expression)

  (cl-dolist (fn matlab-mode-hook)
    (add-hook 'matlab-ts-mode-hook fn)))

(use-package aggressive-indent-mode
  :straight t
  :hook ((matlab-mode . aggressive-indent-mode)
	 (emacs-lisp-mode . aggressive-indent-mode)
	 (cmake-ts-mode . aggressive-indent-mode)))

(mapc (lambda (x) (require-with-check x nil 'reload))
      '(project xref))
(require 'eglot)

(use-package eglot
  :commands eglot eglot-ensure
  :config
  (general-nmap
    :keymaps 'eglot-mode-map
    :prefix "C-e"
    "r" 'eglot-rename
    "d" 'eldoc
    "a" 'eglot-code-actions
    "f" 'eglot-format
    "n" 'flymake-goto-next-error
    "p" 'flymake-goto-prev-error
    "q" 'eglot-code-action-quickfix)

  (setq flymake-fringe-indicator-position 'right-fringe)
  (add-to-list 'eglot-server-programs '(nix-mode "nil"))
  (add-to-list 'eglot-server-programs '((matlab-mode matlab-ts-mode) "matlab-language-server" "--stdio"))
  (add-to-list 'eglot-server-programs '(java-mode "jdt-language-server"))
  (add-to-list 'eglot-server-programs '((python-mode python-ts-mode) "pylsp"))
  (add-to-list 'eglot-server-programs '(scad-mode "openscad-lsp" "--port" :autoport))

  (setq completion-category-overrides '((eglot (styles orderless))))
  (with-eval-after-load 'eglot
    (setq completion-category-defaults nil)))

(use-package eglot-booster
  :straight (eglot-booster :type git :host github :repo "jdtsmith/eglot-booster")
  :after eglot
  :config (eglot-booster-mode))

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

(use-package gnuplot-mode
  :mode ("\\.gnu\\'" . gnuplot-mode))

(use-package toml-mode
  :mode ("\\.toml\\'" . toml-mode))

(use-package yaml-ts-mode
  :mode (("\\.yml\\'" . yaml-ts-mode)
	 ("\\.yaml\\'" . yaml-ts-mode)
	 ("\\.clang-format\\'" . yaml-ts-mode))
  :hook (yaml-ts-mode
	 . (lambda ()
	     (setq-local format-all-formatters '(("YAML" prettierd)))))
  :hook (yaml-ts-mode . format-all-mode)
  :hook (yaml-ts-mode . highlight-indent-guides-mode))

(use-package mhtml-mode
  :hook (mhtml-mode
	 . (lambda ()
	     (setq-local format-all-formatters '(("HTML" prettierd)))))
  :hook (mhtml-mode . format-all-mode))

(use-package mermaid-mode
  :mode (("\\.mmd\\'" . mermaid-mode)))

(use-package dockerfile-ts-mode)

(setq compilation-scroll-output 'first-error
      compilation-auto-jump-to-first-error nil)

(use-package scad-mode
  :mode "\\.scad\\'"
  :hook (scad-mode . eglot-ensure))

(use-package scad-dbus
  :after scad-mode
  :straight (:host github :repo "Lenbok/scad-dbus" :branch "master")
  :config
  (my-local-leader-def
    :keymaps 'scad-mode-map
    "-" #'scad-dbus-view-zoom-out
    "+" #'scad-dbus-view-zoom-in
    "=" #'scad-dbus-view-reset
    "p" #'scad-dbus-preview
    "c" #'scad-dbus-export-stl))

(provide 'development)
;;; development.el ends here
