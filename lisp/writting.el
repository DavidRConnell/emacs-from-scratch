;;; writting.el --- packages for writting -*- lexical-binding: t; -*-
;;   spell-checker. Should work with magit.
;;   Deft
;;; Code:

(use-package org
  :defer 2
  :commands org-mode
  :straight '(org-mode :host github
           :repo "emacs-straight/org-mode"
           :files ("*.el" "lisp/*.el" "contrib/lisp/*.el")
           :build (with-temp-file (expand-file-name "org-version.el"
						    (straight--repos-dir "org-mode"))
                    (insert "(fset 'org-release (lambda () \"9.5\"))\n"
                            "(fset 'org-git-version #'ignore)\n"
                            "(provide 'org-version)\n")))
  :general
  (my-leader-def
    :infix "n"
    "l" #'org-store-link
    "f" #'my-org-roam-find-file
    "x" #'org-roam-capture)

  :config
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

  (setq org-tag-alist '(("ignore")
			("noexport")))

  (setq org-confirm-babel-evaluate nil
	org-link-elisp-confirm-function nil)

  (general-define-key
   :keymaps 'org-mode-map
   "C-c C-q" #'counsel-org-tag)

  (general-define-key
   :keymaps 'org-src-mode-map
   "C-c C-c" #'org-edit-src-exit)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((R . t)
     (python . t)
     (shell . t)
     (emacs-lisp . t)))

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
    :straight '(evil-org :host github :repo "hlissner/evil-org-mode")
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
     :keymap 'org-mode-map
     [C-return] #'+org/insert-item-below
     [C-S-return] #'+org/insert-item-above
     [C-M-return] #'org-insert-subheading))

  (my-local-leader-def
    :keymaps 'org-mode-map
    "t" #'org-todo
    "h" #'org-toggle-heading
    "i" #'org-toggle-item)

  (my-local-leader-def
    :keymaps 'org-mode-map
    :infix "l"
    :which-key "links"
    "i" #'org-id-store-link
    "l" #'org-insert-link
    "L" #'org-insert-all-links
    "s" #'org-store-link
    "S" #'org-insert-last-stored-link
    "t" #'org-toggle-link-display)

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
    (setq org-latex-pdf-process
      '("pdflatex -interaction nonstopmode -output-directory %o %f"
	"bibtex %b"
	"pdflatex -interaction nonstopmode -output-directory %o %f"
	"pdflatex -interaction nonstopmode -output-directory %o %f"))
    (require 'ox-extra)
    (ox-extras-activate '(ignore-headlines)))

  (use-package org-roam
    :init (setq org-roam-directory my-zettle-dir
		org-roam-db-location (expand-file-name "org-roam-db"
						       my-cache-dir))
    :hook (org-mode . org-roam-mode)
    :general
    (my-leader-def
      :infix "n"
      "f" #'my-org-roam-find-file
      "x" #'org-roam-capture)
    (my-local-leader-def
      :keymaps 'org-mode-map
      :infix "m"
      "x" #'org-roam-capture
      "b" #'org-roam-switch-to-buffer
      "f" #'my-org-roam-find-file
      "i" #'org-roam-insert
      "I" #'org-roam-insert-immediate
      "m" #'org-roam)

    :config
    (add-hook 'find-file-hook
	      (lambda ()
		(if (and
		     (memq 'org-roam-buffer--update-maybe
			   post-command-hook)
		     (not (eq 'visible (org-roam-buffer--visibility))))
		    (with-current-buffer (window-buffer)
		      (org-roam-buffer--get-create)))))

    (defun my-org-roam-find-file-action (x)
      "From https://github.com/abo-abo/oremacs/blob/15e6a33d314121ea0b3f1659dbc3ee8181dce854/modes/ora-org-roam.el"
      (if (consp x)
	  (let ((file-path (plist-get (cdr x) :path)))
	    (org-roam--find-file file-path))
	(let* ((title-with-tags x)
	       (org-roam-capture--info
		`((title . ,title-with-tags)
		  (slug . ,(funcall org-roam-title-to-slug-function title-with-tags))))
	       (org-roam-capture--context 'title))
	  (setq org-roam-capture-additional-template-props (list :finalize 'find-file))
	  (org-roam-capture--capture))))

    (defun my-org-roam-find-file ()
      (interactive)
      (unless org-roam-mode (org-roam-mode))
      (ivy-read "File: " (org-roam--get-title-path-completions)
		:action #'my-org-roam-find-file-action
		:caller 'my-org-roam-find-file))

;;; Need to find an appropriate hook
    ;; (add-hook 'change-major-mode-hook
    ;; 	      (defun my-toggle-org-roam-buffer ()
    ;; 		(cond
    ;; 		 ((and (eq 'visible (org-roam-buffer--visibility))
    ;; 		       (not (org-roam-db-has-file-p
    ;; 			     (buffer-file-name (window-buffer)))))
    ;; 		  (org-roam-buffer-deactivate))
    ;; 		 ((and (not (eq 'visible
    ;; 				(org-roam-buffer--visibility)))
    ;; 		       (org-roam-db-has-file-p
    ;; 			(buffer-file-name (window-buffer))))
    ;; 		  (org-roam-buffer-activate)))))

    (setq org-roam-graph-viewer "/usr/bin/qutebrowser"
	  org-roam-completion-everywhere t

	  org-roam-capture-templates
	  '(("d" "default" plain #'org-roam-capture--get-point
	     :file-name "%<%Y%m%d%H%M%S>"
	     :head "#+TITLE: ${title}\n"
	     :unnarrowed t))

	  org-roam-capture-immediate-template
	  '("d" "default" plain #'org-roam-capture--get-point
	    :file-name "%<%Y%m%d%H%M%S>"
	    :head "#+TITLE: ${title}\n"
	    :unnarrowed t
	    :immediate-finish t))

    (use-package org-roam-protocol
      :straight nil
      :config
      (use-package org-protocol
	:straight nil)
      (setq org-roam-capture-ref-templates
		    '(("r" "ref" plain #'org-roam-capture--get-point
		       :file-name "%<%Y%m%d%H%M%S>"
		       :head "#+TITLE: ${title}\n#+ROAM_KEY: ${ref}\n"
		       :unnarrowed t))))

    (use-package org-roam-bibtex
      :hook (org-roam-mode . org-roam-bibtex-mode)
      :general
      (my-local-leader-def
	:keymaps 'org-mode-map
	:infix "m"
	"i" #'orb-insert-non-ref
	"r" #'orb-insert
	"n" #'orb-note-actions)
      :config
      (setq orb-templates
	    '(("r" "ref" plain #'org-roam-capture--get-point
	       ""
	       :file-name "references/${citekey}"
	       :head "#+TITLE: ${title}\n#+AUTHOR: ${author}\n#+ROAM_KEY: ${ref}\n"
	       :unnarrowed t))
	    orb-insert-link-description 'citation))

    (use-package org-roam-server
      :general
      (my-local-leader-def
	:keymaps 'org-mode-map
	:infix "m"
	"s" (defun my-org-roam-start-and-open-server ()
	      "Start org-roam-server and open network in browser."
	      (interactive)
	      (org-roam-server-mode 1)
	      (call-interactively
	       (org-link-open-from-string "http://localhost:8080/"))))
      :config
      (setq org-roam-server-host "127.0.0.1"
	    org-roam-server-port 8080
	    org-roam-server-authenticate nil
	    org-roam-server-export-inline-images t
	    org-roam-server-serve-files nil
	    org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
	    org-roam-server-network-poll t
	    org-roam-server-network-arrows nil
	    org-roam-server-network-label-truncate t
	    org-roam-server-network-label-truncate-length 60
	    org-roam-server-network-label-wrap-length 20)))

  ;; TODO: move into it's own package.
  (use-package +ox-word
    :straight nil
    :load-path "~/.doom.d/extras/")

  (use-package sdcv-mode
    :disabled
    :straight '(sdcv-mode :type git :host github :repo "gucong/emacs-sdcv")
    :general
    (my-leader-def
      :infix "d"
      "d" #'sdcv-search))

  (use-package lexic
    :straight '(lexic :host github :repo "tecosaur/lexic")
    :general
    (my-leader-def
      :infix "d"
      "d" #'lexic-search)
    :init
    (setq lexic-dictionary-specs
	  '(("Webster's Revised Unabridged Dictionary (1913)" :formatter lexic-format-webster :priority 1)
	    ("Element\s database" :short "Element" :formatter lexic-format-element :priority 3)
	    ("Online Etymology Dictionary" :short "Etymology" :formatter lexic-format-online-etym :priority 4)
	    ("Soule's Dictionary of English Synonyms" :short "Synonyms" :formatter lexic-format-soule :priority 5)))
    :config
    (general-define-key
     :keymaps 'lexic-mode-map
     :states 'normal
     "q" #'lexic-return-from-lexic
     "o" #'counsel-outline
     "RET" #'lexic-search-word-at-point
     "C-c C-o" #'lexic-search-word-at-point
     "a" #'outline-show-all
     "h" (lambda () (interactive) (outline-hide-sublevels 3))
     "H" #'outline-hide-sublevels
     "C-i" #'lexic-toggle-entry
     "n" #'lexic-next-entry
     "N" (lambda () (interactive) (lexic-next-entry t))
     "p" #'lexic-previous-entry
     "P" (lambda () (interactive) (lexic-previous-entry t))
     "E" (lambda () (interactive) (lexic-return-from-lexic) ; expand
	       (switch-to-buffer (lexic-get-buffer)))
     "M" (lambda () (interactive) (lexic-return-from-lexic) ; minimise
	       (lexic-goto-lexic))
     "C-p" #'lexic-search-history-backwards
     "C-n" #'lexic-search-history-forwards
     "/" (lambda () (interactive) (call-interactively #'lexic-search))))

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

(use-package org-ref
  :commands org-mode
  :general
  (general-imap
    :keymaps 'org-mode-map
    "C-]" #'org-ref-insert-link)
  (my-local-leader-def
    :keymaps 'org-mode-map
    :infix "e"
    "c" #'org-ref-insert-link
    "r" #'org-ref-insert-ref-link
    "b" #'org-ref-insert-bibliography-link
    "s" #'org-ref-insert-bibliographystyle-link
    "n" #'org-ref-open-notes-at-point
    "p" #'org-ref-open-pdf-at-point
    "u" #'org-ref-open-url-at-point)
  (my-leader-def
    :infix "r"
    "c" #'helm-bibtex
    "m" (defun my-open-master-bib
	    () (interactive) (find-file my-refs-bib)))

  :config
  (setq org-ref-default-bibliography (list my-refs-bib)
	org-ref-default-ref-type "cref"
	org-ref-show-broken-links t
	org-latex-prefer-user-labels t
	org-ref-bibliography-notes my-refs-notes-dir
	org-ref-notes-function (lambda (key)
				 (interactive)
				 (org-ref-notes-function-many-files
				  key))
	org-ref-pdf-directory my-refs-pdfs-dir
	org-ref-get-pdf-filename-function
	(lambda (key)
	  (let ((files (directory-files-recursively
			org-ref-pdf-directory
			(concat key ".pdf"))))
	    (if (= 1 (length files))
		(car files)
	      (completing-read "Choose: " files))))))

(use-package helm
  :general
  (my-leader-def
    ";" #'helm-eval-expression-with-eldoc))

(use-package bibtex
  :after org-ref
  :config
  (require 'find-lisp)
  (setq bibtex-completion-bibliography my-refs-bib
	bibtex-completion-additional-search-fields '(keywords)
	bibtex-completion-library-path
	(cl-remove-if-not
	 (lambda (f)
	   (find-lisp-file-predicate-is-directory f my-refs-pdfs-dir))
	 (directory-files-recursively my-refs-pdfs-dir "." 'dirs))

	bibtex-completion-notes-path my-refs-notes-dir
	bibtex-completion-pdf-open-function
	(lambda (fpath) (call-process "xdg-open" nil 0 nil fpath))

	bibtex-completion-notes-template-multiple-files
	"#+TITLE: ${title}\n#+AUTHOR: ${author-or-editor}\n#+ROAM_KEY: cite:${=key=}")

  (general-nmap
    :keymaps 'bibtex-mode-map
    :prefix "C-c"
    "C-c" #'org-ref-clean-bibtex-entry))

(use-package ebib
  :general
  (my-leader-def
    :infix "r"
    "e" (defun my-ebib-open-with-file ()
	  "Open with bib file in PWD if exists otherwise use master.

If there is more than one local bib file ask."
	  (interactive)
	  (let* ((potential-bibs (org-ref-find-bibliography))
		 (local-bib-file
		  (cond
		   ((not potential-bibs)
		    nil)
		   ((= (length potential-bibs) 1)
		    (first potential-bibs))
		   ((> (length potential-bibs) 1)
		    (completing-read "Select bib" potential-bibs)))))
	    (if local-bib-file
		(ebib (expand-file-name local-bib-file))
	      (ebib my-refs-bib)))))
  :config
  (setq ebib-notes-directory my-refs-notes-dir
	ebib-reading-list-file (concat my-refs-notes-dir "readinglist.org")
	ebib-default-directory my-refs-notes-dir
	ebib-keywords-file (concat my-refs-notes-dir ".keywords.txt")
	ebib-notes-show-note-method nil
	ebib-file-associations '(("pdf" . "xdg-open")))

  (require 'find-lisp)
  (setq ebib-file-search-dirs
	(cl-remove-if-not
	 (lambda (f) (find-lisp-file-predicate-is-directory f my-refs-pdfs-dir))
	 (directory-files-recursively my-refs-pdfs-dir "." 'dirs)))

  (general-define-key
   :keymaps 'ebib-index-mode-map
   "/" #'swiper-isearch)

  (defun my-ebib-create-org-title (key db)
    (replace-regexp-in-string "[\t\n ]+"
			      " "
			      (or (ebib-get-field-value
				   "title" key db 'noerror 'unbraced 'xref)
				  "(No Title)")))

  (defun my-ebib-create-org-author (key db)
    (replace-regexp-in-string "[\t\n ]+ "
			      " "
			      (or (ebib-get-field-value
				   "author" key db 'noerror 'unbraced 'xref)
				  (ebib-get-field-value
				   "editor" key db 'noerror 'unbraced 'xref)
				  "(No Author)")))

  (defun my-ebib-create-org-identifier (key _)
    key)

  (setq ebib-notes-template-specifiers
	'((?K . ebib-create-org-identifier)
	  (?k . my-ebib-create-org-identifier)
	  (?T . ebib-create-org-title)
	  (?t . my-ebib-create-org-title)
	  (?A . my-ebib-create-org-author)
	  (?L . ebib-create-org-link)
	  (?F . ebib-create-org-file-link)
	  (?D . ebib-create-org-doi-link)
	  (?U . ebib-create-org-url-link)
	  (?M . ebib-reading-list-todo-marker)))

  (setq ebib-reading-list-template-specifiers ebib-notes-template-specifiers)

  (setq ebib-reading-list-template "* %M [[file:%k.org][%t]]\n:PROPERTIES:\n:AUTHOR: %A\n:END:\ncite:%k\n\n")
  (setq ebib-notes-template "#+TITLE: %t\n#+AUTHOR: %A\ncite:%k\n\n>|<"))

;; (use-package org-journal)
;; (use-package org-drill)

(use-package deft
  :after org-roam
  :general
  (my-leader-def
    :infix "n"
    "d" #'deft)
  :config
  (setq deft-extensions '("org")
	deft-new-file-format "%Y%m%d%H%M%S"
	deft-use-filter-string-for-filename nil
	deft-directory org-roam-directory)
  (general-imap
    :keymaps 'deft-mode-map
    "C-o" #'deft-open-file-other-window
    "C-w" #'deft-filter-decrement-word
    "C-n" #'next-line
    "C-p" #'previous-line))

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

(provide 'writting)
;;; writting ends here
