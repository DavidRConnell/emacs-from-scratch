;;   spell-checker. Should work with magit.
;;   grammar?
;;   babel
;;   ox
;;   ox-hugo
;;   ox-word
;;   Set up new notes (org?) directory with zettle and reference subdirs.
;;   Use referenece subdir for reference notes not actual papers.
;;   Add elfeed org
;;   Deft
;;; Code:

(use-package org
  :config
  (add-hook 'org-mode-hook #'org-indent-mode)
  (add-hook 'org-mode-hook #'turn-on-visual-line-mode)
  (add-hook 'org-mode-hook
	    (lambda () (interactive)
	      (setq-local company-backends '(company-capf
					     company-dabbrev))))
  (setq org-startup-folded t
	org-hide-emphasis-markers t
	org-catch-invisible-edits 'smart)

  (setq org-confirm-babel-evaluate nil
	org-link-elisp-confirm-function nil)

  (general-define-key
   :keymaps 'org-src-mode-map
   "C-c C-c" #'org-edit-src-exit)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((R . t)
     (python . t)))

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
    :config
    (add-hook 'evil-org-mode-hook #'evil-org-set-key-theme))

  (use-package org-roam
    :init (setq org-roam-directory my-zettle-dir
                org-roam-db-location (expand-file-name "org-roam-db"
                                                       my-cache-dir))
    :hook (org-mode . org-roam-mode)
    :general
    (my-leader-def
     :infix "z"
     "g" #'org-roam-find-file
     "x" #'org-roam-capture)
    (my-local-leader-def
      :keymaps 'org-mode-map
      :infix "m"
      "x" #'org-roam-capture
      "f" #'org-roam-find-file
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
      :after org-protocol
      :config (setq org-roam-capture-ref-templates
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
	"r" #'orb-insert)
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
	    org-roam-server-network-label-wrap-length 20))))

  (use-package org-ref
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
      "s" #'org-ref-insert-bibliographystyle-link)
    :config
    (setq org-ref-default-bibliography (list my-refs-bib)
	  org-ref-default-ref-type "cref"
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

  (use-package bibtex
    :config
    (require 'find-lisp)
    (setq bibtex-completion-bibliography my-refs-bib)
    (setq bibtex-completion-additional-search-fields '(keywords))
    (setq bibtex-completion-library-path
	  (cl-remove-if-not
	   (lambda (f) (find-lisp-file-predicate-is-directory
			f
			my-refs-pdfs-dir))
	   (directory-files-recursively my-refs-pdfs-dir "." 'dirs)))
    (setq bibtex-completion-notes-path my-refs-notes-dir)
    (setq bibtex-completion-pdf-open-function
	  (lambda (fpath) (call-process "xdg-open" nil 0 nil fpath)))
    (setq bibtex-completion-notes-template-multiple-files
	  "${title}\n#+AUTHOR: ${author-or-editor}\ncite:${=key=}")

    (use-package sdcv-mode
      :straight '(sdcv-mode :type git :host github :repo "gucong/emacs-sdcv")
      :general
      (my-leader-def
	:infix "d"
	"d" #'sdcv-search))

    (use-package wiki-summary
      :general
      (my-leader-def
	:infix "d"
	"k" #'wiki-summary))

    (use-package wordnut
      :general
      (my-leader-def
	:infix "d"
	"w" #'wordnut-search)))
  ;; (use-package org-journal)
  ;; (use-package org-drill)
  ;; (use-package deft)
  ;; (use-package ebib))

(provide 'writting)
;;; writting ends here
