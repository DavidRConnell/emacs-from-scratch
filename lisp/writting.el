;;; writting.el --- packages for writting -*- lexical-binding: t; -*-
;;; Code:

(use-package org
  :demand
  :init
  (setq org-modules '(org-id ol-info org-protocol ol-doi))
  :config
  (my-leader-def
    :infix "n"
    "l" #'org-store-link)
  (general-imap
    :keymaps 'org-mode-map
    "C-i" #'org-do-demote
    "C-S-i" #'org-do-promote)
  (add-hook 'org-mode-hook #'org-indent-mode)
  (add-hook 'org-mode-hook #'turn-on-visual-line-mode)
  (add-hook 'org-mode-hook
	    (lambda () (interactive)
	      (setq-local company-backends '(company-capf
					     company-dabbrev))))
  (setq org-startup-folded t
	org-hide-emphasis-markers t
	org-catch-invisible-edits 'smart)

  (defun my-org-toggle-emphasis-markers-display ()
    (interactive)
    (setq-local org-hide-emphasis-markers (not org-hide-emphasis-markers))
    (font-lock-fontify-buffer))

  (defun my-org-toggle-markup-display ()
    (interactive)
    (org-toggle-link-display)
    (my-org-toggle-emphasis-markers-display))

  (setq org-directory my-zettle-dir
	org-agenda-files '("todo.org"))

  (setq org-tag-alist '(("ignore")
			("noexport")
			("export"))
	org-tags-column (- 3 fill-column))

  (setq org-confirm-babel-evaluate nil
	org-link-elisp-confirm-function nil)

  (add-to-list 'org-file-apps '("\\.pdf\\'" . "xdg-open %s"))

  (general-define-key
   :keymaps 'org-mode-map
   :prefix "C-c"
   "C-q" #'counsel-org-tag
   "r" #'org-refile)

  (general-define-key
   :keymaps 'org-src-mode-map
   "C-c C-'" #'org-edit-src-exit)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((R . t)
     (python . t)
     (matlab . t)
     (shell . t)
     (emacs-lisp . t)
     (latex . t)))

  (require 'ob-octave-fix)
  (setq org-babel-default-header-args:matlab '((:session . "*MATLAB*")))

  (use-package ob-async)

  (use-package org-superstar
    :hook (org-mode . org-superstar-mode)
    :config
    (setq org-superstar-leading-bullet ?\s
	  org-superstar-leading-fallback ?\s
	  org-superstar-hide-leading-stars nil
	  org-superstar-todo-bullet-alist
	  '(("TODO" . 9744)
	    ("[ ]" . 9744)
	    ("DONE" . 9745)
	    ("[X]" . 9745))))

  (use-package evil-org
    :hook (org-mode . evil-org-mode)
    :init
    (defvar evil-org-retain-visual-state-on-shift t)
    (defvar evil-org-special-o/O '(table-row))
    (defvar evil-org-use-additional-insert t)
    :config
    (add-hook 'evil-org-mode-hook #'evil-normalize-keymaps)
    (evil-org-set-key-theme)

    (require 'org-tweaks)
    (general-define-key
     :states '(normal insert)
     :keymaps 'org-mode-map
     [C-return] #'+org/insert-item-below
     [C-S-return] #'+org/insert-item-above
     [C-M-return] #'org-insert-subheading))

  (general-nmap
    :keymaps 'org-mode-map
    "zn" #'org-toggle-narrow-to-subtree)

  (my-local-leader-def
    :keymaps 'org-mode-map
    "t" #'org-todo
    "h" #'org-toggle-heading
    "i" #'org-toggle-item
    "p" #'org-priority
    "P" #'org-set-property)

  (my-local-leader-def
    :keymaps 'org-mode-map
    :infix "l"
    :which-key "links"
    "i" #'org-id-store-link
    "l" #'org-insert-link
    "L" #'org-insert-all-links
    "s" #'org-store-link
    "S" #'org-insert-last-stored-link
    "t" #'my-org-toggle-markup-display)

  (my-local-leader-def
    :keymaps 'org-mode-map
    :infix "s"
    "n" #'org-babel-next-src-block
    "p" #'org-babel-previous-src-block
    "g" #'org-babel-goto-named-src-block)

  (use-package ox-clip)

  (use-package org-cliplink
    :general
    (my-local-leader-def
      :keymaps 'org-mode-map
      :infix "l"
      "c" #'org-cliplink))

  (use-package ox-hugo
    :after ox
    :config
    (setq org-hugo-use-code-for-kbd t
	  org-blackfriday--org-element-string '((src-block . "Listing")
						(table . "Table")
						(figure . "Figure")))

    (defun org-ref-cref-export (keyword desc format)
      "cref link export function.
See https://www.ctan.org/tex-archive/macros/latex/contrib/cleveref"
      (cond
       ((eq format 'latex) (format "\\cref{%s}" keyword))
       ;; considering the fact that latex's the standard of math formulas, just use
       ;;mathjax to render the html customize the variable
       ;;'org-html-mathjax-template' and 'org-html-mathjax-options' referring to
       ;;'autonumber'
       ((or (eq format 'md) (eq format 'html))
	(let (type)
	  (when (string-match "\\(.*\\):.*" keyword)
	    (setq type (match-string 1 keyword))
	    (cond
	     ((string= type "eq")
	      (format "eq \\ref{%s}" keyword))))))))

    (use-package citeproc-org
      :config
      (citeproc-org-setup)
      (setq citeproc-org-org-bib-header "** References\n")))

  (use-package ox
    :straight nil
    :config
    (setq org-latex-pdf-process '("tectonic %f")
	  org-latex-packages-alist '(
				     ("" "xcolor" nil)
				     ("capitalise, nameinlink, noabbrev" "cleveref" nil))
	  org-latex-classes `(
			      ("article"
			       ,(concat
				 "\\documentclass[11pt]{article}\n"
				 "[DEFAULT-PACKAGES]\n"
				 "[PACKAGES]\n"
				 "\\definecolor{defaultcolor}{HTML}{436092}\n"
				 "\\crefformat{equation}{#2eq~#1#3}\n"
				 "\\crefmultiformat{equation}{#2eq~#1#3}{ and #2#1#3}{, #2#1#3}{ and #2#1#3}\n"
				 "\\crefmultiformat{figure}{#2Figures~#1#3}{ and #2#1#3}{, #2#1#3}{ and #2#1#3}\n"
				 "\\hypersetup{%\n"
				 "colorlinks,\n"
				 "linkcolor=defaultcolor,\n"
				 "urlcolor=defaultcolor,\n"
				 "citecolor=defaultcolor}")
			       ("\\section{%s}" . "\\section*{%s}")
			       ("\\subsection{%s}" . "\\subsection*{%s}")
			       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
			       ("\\paragraph{%s}" . "\\paragraph*{%s}")
			       ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
	  org-export-with-sub-superscripts '{})
    (require 'ox-extra)
    (ox-extras-activate '(ignore-headlines)))

  (use-package ox-word
    :straight nil
    :load-path "~/projects/ox-word")

  (use-package sdcv-mode
    :straight '(sdcv-mode :type git :host github :repo "gucong/emacs-sdcv")
    :general
    (my-leader-def
      :infix "d"
      "d" #'sdcv-search)
    :config

  (use-package wiki-summary
    :general
    (my-leader-def
      :infix "d"
      "k" #'wiki-summary))

  (use-package wordnut
    :general
    (my-leader-def
      :infix "d"
      "w" #'wordnut-search)
    :config
    (add-hook 'wordnut-mode-hook #'org-mode)))

  (require 'notes)
  (require 'references))


(use-package helm
  :general
  (my-leader-def
    ";" #'helm-eval-expression-with-eldoc))

;; (use-package org-journal)
;; (use-package org-drill)

(use-package flyspell
  :straight nil
  :hook (text-mode . flyspell-mode)
  :config
  (setq ispell-program-name (executable-find "aspell")
	ispell-personal-dictionary "~/.aspell.en.pws")
  (use-package flyspell-correct
    :general
    (general-nmap
      "z=" #'flyspell-correct-wrapper)
    :config
    (use-package flyspell-correct-ivy)))

(use-package nov
  :mode ("\\.epub\\'" . nov-mode)
  :config
  (setq nov-text-width fill-column))

(use-package markdown-mode
  :hook (markdown-mode . visual-line-mode)
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdowm-mode)
	 ("qutebrowser-editor" . gfm-mode)))

(provide 'writting)
;;; writting ends here
