;;; writting.el --- packages for writting -*- lexical-binding: t; -*-
;;; Code:

(setq sentence-end-double-space nil)

(use-package emacs
  :mode ("\\.tsv\\'" . whitespace-mode))

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
	org-catch-invisible-edits 'smart
	org-list-allow-alphabetical t)

  (defun my-org-toggle-emphasis-markers-display ()
    (interactive)
    (setq-local org-hide-emphasis-markers (not org-hide-emphasis-markers))
    (font-lock-fontify-buffer))

  (defun my-org-toggle-markup-display ()
    (interactive)
    (org-toggle-link-display)
    (my-org-toggle-emphasis-markers-display))

  (setq org-directory my-zettle-dir
	org-agenda-files '("todo.org" "habits.org" "meetings.org"))
  (my-leader-def
    "A" #'(lambda (arg) (interactive "P") (org-agenda arg "a")))

  (setq org-tag-alist '(("ignore")
			("noexport")
			("export"))
	;; org-tags-column (- 3 fill-column)
	)

  ;; Code for generating links for hugo
  (defun org-hugo-link-complete ()
    "Create link with Hugo ref shortcode"
    (concat "{{% ref \"" (file-relative-name (read-file-name "File: ")) "\" %}}"))

  (defun org-hugo-follow (link)
    (find-file (expand-file-name link)))

  (org-link-set-parameters "hugo"
			   :complete 'org-hugo-link-complete
			   :follow 'org-hugo-follow)

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

  (use-package ob-mermaid
    :config
    (setq ob-mermaid-cli-path "/etc/profiles/per-user/voidee/bin/mmdc"))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((R . t)
     (python . t)
     (matlab . t)
     (makefile . t)
     (shell . t)
     (emacs-lisp . t)
     (latex . t)
     (gnuplot . t)
     (mermaid . t)
     (dot . t)))

  (require 'ob-octave-fix)
  (setq org-babel-default-header-args:matlab '((:session . "*MATLAB*")))

  (use-package ob-async)

  (use-package org-modern
    :disabled
    :config
    (setq org-auto-align-tags nil
	  org-tags-column 0
	  org-insert-heading-respect-content t
	  org-pretty-entities nil
	  org-ellipsis "…")
    (setq org-modern-fold-stars
	  '(("▶" . "▼") ("▷" . "▽") ("▶" . "▼") ("▹" . "▿") ("▸" . "▾")))
    (global-org-modern-mode))

  (use-package org-superstar
    :hook (org-mode . org-superstar-mode)
    :config
    (set-face-attribute 'org-level-3 nil :foreground "black")
    (setq org-superstar-leading-bullet ?\s
	  org-superstar-leading-fallback ?\s
	  org-superstar-hide-leading-stars nil
	  org-superstar-headline-bullets-list '(?◉ ?○ ?◈ ?▷)
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
    "a" #'org-priority
    "p" #'org-set-property)

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
    :which-key "src"
    "n" #'org-babel-next-src-block
    "p" #'org-babel-previous-src-block
    "g" #'org-babel-goto-named-src-block)

  (my-local-leader-def
    :keymaps 'org-mode-map
    :infix "f"
    :which-key "footnotes"
    "f" #'org-footnote-new
    "d" #'org-footnote-delete
    "a" #'org-footnote-action)

  (use-package ox-clip)
  (use-package ox-reveal
    :config
    (setq org-reveal-root "file:///home/voidee/Downloads/reveal/reveal.js-master")
    (defun org-reveal-plugin-scripts-4 (plugins info in-single-file)
      "Return scripts for initializing reveal.js 4.x builtin scripts."
      ;; Return a pair whose first value is the HTML contents for the
      ;; plugin scripts, the second value is a list of import statements
      ;; to be embedded in the Reveal.initialize call
      (if (not (null plugins))
	  ;; Generate plugin scripts
	  (let* ((plugins (mapcar
			   (lambda (p)
                             ;; Convert legacy
                             ;; plugin names into
                             ;; reveal.js 4.0 ones
                             (cond
                              ((eq p 'highlight) 'RevealHighlight)
                              ((eq p 'markdown) 'RevealMarkdown)
                              ((eq p 'search) 'RevealSearch)
                              ((eq p 'notes) 'RevealNotes)
                              ((eq p 'math) 'RevealMath.KaTeX)
                              ((eq p 'zoom) 'RevealZoom)
                              (t p)))
			   plugins))
		 (available-plugins
		  (append '((RevealHighlight . "%splugin/highlight/highlight.js")
                            (RevealMarkdown . "%splugin/markdown/markdown.js")
                            (RevealSearch . "%splugin/search/search.js")
                            (RevealNotes . "%splugin/notes/notes.js")
                            (RevealMath.KaTeX . "%splugin/math/math.js")
                            (RevealZoom . "%splugin/zoom/zoom.js"))
			  org-reveal-external-plugins
			  ;; Buffer local plugins
			  (let ((local-plugins (plist-get info :reveal-external-plugins)))
                            (and local-plugins
				 (org-reveal--read-sexps-from-string local-plugins)))))
		 (plugin-js (seq-filter 'identity ;; Filter out nil
					(mapcar (lambda (p)
						  (cdr (assoc p available-plugins)))
						plugins))))
            (if (not (null plugin-js))
		(cons
		 ;; First value of the pair, a list of script file names
		 (let ((root-path (org-reveal-root-path info)))
		   (org-reveal--multi-level-mapconcat
                    (lambda (p)
                      (org-reveal--script-tag-by-file-name (org-reveal--replace-first-%s p root-path)
							   in-single-file))
                    plugin-js ""))
		 ;; Second value of the tuple, a list of Reveal plugin
		 ;; initialization statements
		 (format "plugins: [%s]"
			 (mapconcat 'symbol-name
                                    ;; Remove multiplex from plugins, as
                                    ;; the multiplex plugin has been moved
                                    ;; out of reveal.js.
                                    (seq-filter (lambda (p) (not (eq p 'multiplex))) plugins) ", ")))
              ;; No available plugin info found. Perhaps wrong plugin
              ;; names are given
              (cons nil nil)))
	;; No plugins, return empty string
	(cons nil nil)))
    (defun org-reveal-template (contents info)
      "Return complete document string after HTML conversion.
contents is the transcoded contents string.
info is a plist holding export options."
      (concat
       (format "<!DOCTYPE html>\n<html%s>\n<head>\n"
               (if-format " lang=\"%s\"" (plist-get info :language)))
       "<meta charset=\"utf-8\"/>\n"
       (if-format "<title>%s</title>\n" (org-export-data (plist-get info :title) info))
       (if-format "<meta name=\"author\" content=\"%s\"/>\n" (org-export-data (plist-get info :author) info))
       (if-format "<meta name=\"description\" content=\"%s\"/>\n" (org-export-data (plist-get info :description) info))
       (if-format "<meta name=\"keywords\" content=\"%s\"/>\n" (org-export-data (plist-get info :keywords) info))
       (org-reveal-stylesheets info)
       (org-reveal--build-pre/postamble 'head-preamble info)
       (org-element-normalize-string (plist-get info :html-head))
       (org-element-normalize-string (plist-get info :html-head-extra))
       "</head>
<body>\n"
       (org-reveal--build-pre/postamble 'preamble info)
       "<div class=\"reveal\">\n"
       (org-reveal--build-pre/postamble 'prologue info)
       "<div class=\"slides\">\n"
       ;; Title slides
       (let ((title-slide (plist-get info :reveal-title-slide)))
	 (when (and title-slide (not (plist-get info :reveal-subtree)))
	   (let ((title-slide-background (plist-get info :reveal-title-slide-background))
		 (title-slide-background-size (plist-get info :reveal-title-slide-background-size))
		 (title-slide-background-position (plist-get info :reveal-title-slide-background-position))
		 (title-slide-background-repeat (plist-get info :reveal-title-slide-background-repeat))
		 (title-slide-background-transition (plist-get info :reveal-title-slide-background-transition))
		 (title-slide-background-opacity (plist-get info :reveal-title-slide-background-opacity))
		 (title-slide-state (plist-get info :reveal-title-slide-state))
		 (title-slide-with-header (plist-get info :reveal-slide-global-header))
		 (title-slide-with-footer (plist-get info :reveal-slide-global-footer)))
             (concat "<section id=\"sec-title-slide\""
                     (when title-slide-background
                       (concat " data-background=\"" title-slide-background "\""))
                     (when title-slide-background-size
                       (concat " data-background-size=\"" title-slide-background-size "\""))
                     (when title-slide-background-position
                       (concat " data-background-position=\"" title-slide-background-position "\""))
                     (when title-slide-background-repeat
                       (concat " data-background-repeat=\"" title-slide-background-repeat "\""))
                     (when title-slide-background-transition
                       (concat " data-background-transition=\"" title-slide-background-transition "\""))
		     (when title-slide-background-opacity
		       (concat " data-background-opacity=\"" title-slide-background-opacity "\""))
                     (when title-slide-state
		       (concat " data-state=\"" title-slide-state "\""))
                     ">"
                     (when title-slide-with-header
                       (let ((header (plist-get info :reveal-slide-header)))
			 (when header (format "<div class=\"slide-header\">%s</div>\n" header))))
                     (cond ((eq title-slide nil) nil)
			   ((stringp title-slide) (format-spec title-slide (org-html-format-spec info)))
			   ((eq title-slide 'auto) (org-reveal--auto-title-slide-template info)))
                     "\n"
                     (when title-slide-with-footer
                       (let ((footer (plist-get info :reveal-slide-footer)))
			 (when footer (format "<div class=\"slide-footer\">%s</div>\n" footer))))
                     "</section>\n"))))
       contents
       "</div>\n"
       (org-reveal--build-pre/postamble 'epilogue info)
       "</div>\n"
       (org-reveal--build-pre/postamble 'postamble info)
       (org-reveal-scripts info)
       "</body>
</html>\n"))
    )

  (use-package org-cliplink
    :general
    (my-local-leader-def
      :keymaps 'org-mode-map
      :infix "l"
      "c" #'org-cliplink))

  (use-package org-appear
    :hook (org-mode . org-appear-mode)
    :config
    (setq org-appear-trigger 'always
	  org-appear-autoentities t
	  org-appear-autoemphasis t)
    (add-hook 'evil-insert-state-entry-hook #'org-appear-manual-start nil t)
    (add-hook 'evil-insert-state-exit-hook #'org-appear-manual-stop nil t))

  (use-package ox-hugo
    :defer
    :after ox
    :config
    (setq org-hugo-use-code-for-kbd t
	  org-blackfriday--org-element-string '((src-block . "Listing")
						(table . "Table")
						(figure . "Figure"))))

  (use-package ox
    :defer
    :config
    (require 'ox-pandoc)
    (setq org-latex-pdf-process '("[ -d %o/build ] || mkdir %o/build"
				  "latexmk -g -%latex -outdir=%o/build %f"
				  "mv %o/build/*.pdf %o")
	  ;; org-latex-default-packages-alist '(("AUTO" "inputenc" t ("pdflatex"))
	  ;; 				     ("T1" "fontenc" t ("pdflatex"))
	  ;; 				     ("" "fontspec" t ("lualatex" "xelatex"))
	  ;; 				     ("tracking=true, letterspace=30" "microtype" t ("lualatex"))
	  ;; 				     ("" "microtype" t ("xelatex")))

	  ;; org-latex-packages-alist '(("" "xcolor" nil)
	  ;; 			     ("" "amsmath" nil)
	  ;; 			     ("" "unicode-math" nil)
	  ;; 			     ("" "hyperref" nil)
	  ;; 			     ("capitalise, nameinlink, noabbrev" "cleveref" nil))
	  org-latex-default-packages-alist '()
	  org-latex-packages-alist '()
	  org-latex-classes `(("article"
			       ,(concat
				 "\\documentclass[11pt,headings=big,numbers=noenddot]{scrartcl}\n"
				 "[DEFAULT-PACKAGES]\n"
				 "[PACKAGES]\n"
				 "\\usepackage{amsmath}\n"
				 "\\usepackage{hyperref}\n"
				 "\\usepackage{unicode-math}\n"
				 "\\usepackage{xcolor}\n"
				 "\\usepackage{fontspec}\n"
				 "\\usepackage[capitalise,nameinlink,noabbrev]{cleveref}\n"
				 "\\usepackage[tracking=true]{microtype}\n"
				 "\\setmainfont[Ligatures=TeX]{TeX Gyre Pagella}\n"
				 "\\setsansfont[Ligatures=TeX]{TeX Gyre Pagella}\n"
				 "\\setmathfont{TeX Gyre Pagella Math}\n"
				 "\\definecolor{defaultcolor}{HTML}{436092}\n"
				 "\\crefformat{equation}{#2eq~#1#3}\n"
				 "\\crefmultiformat{equation}{#2eq~#1#3}{ and #2#1#3}{, #2#1#3}{ and #2#1#3}\n"
				 "\\crefmultiformat{figure}{#2Figures~#1#3}{ and #2#1#3}{, #2#1#3}{ and #2#1#3}\n"
				 "\\hypersetup{%\n"
				 "  colorlinks,\n"
				 "  linkcolor=defaultcolor,\n"
				 "  urlcolor=defaultcolor,\n"
				 "  citecolor=defaultcolor"
				 "}")
			       ("\\section{%s}" . "\\section*{%s}")
			       ("\\subsection{%s}" . "\\subsection*{%s}")
			       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
			       ("\\paragraph{%s}" . "\\paragraph*{%s}")
			       ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
			      ("simple"
			       "\\documentclass{scrartcl}"
			       ("\\section{%s}" . "\\section*{%s}")
			       ("\\subsection{%s}" . "\\subsection*{%s}")
			       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
			       ("\\paragraph{%s}" . "\\paragraph*{%s}")
			       ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
			      ("book"
			       "\\documentclass{srcbook}"
			       ("\\chapter{%s}" . "\\chapter{%s}")
			       ("\\section{%s}" . "\\section*{%s}")
			       ("\\subsection{%s}" . "\\subsection*{%s}")
			       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
			       ("\\paragraph{%s}" . "\\paragraph*{%s}"))
			      ("letter"
			       "\\documentclass[11pt]{scrlttr2}"
			       ("\\section{%s}" . "\\section*{%s}"))
			      ("rushdoc"
			       "\\documentclass{rushdoc}"
			       ("\\section{%s}" . "\\section*{%s}")
			       ("\\subsection{%s}" . "\\subsection*{%s}")
			       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
			       ("\\paragraph{%s}" . "\\paragraph*{%s}")
			       ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
			      ("rushpres"
			       "\\documentclass{rushpresentation}"
			       ("\\section{%s}" . "\\section*{%s}")
			       ("\\subsection{%s}" . "\\subsection*{%s}")
			       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
	  org-export-with-sub-superscripts '{}
	  org-odt-preferred-output-format "odt"
	  org-html-validation-link nil
	  org-latex-prefer-user-labels t
	  org-export-in-background nil ;; Would like but is currently failing
	  org-html-head "
<style>
  pre.src {
    background-color: #FAFAFA;
  }

  div, p {
    margin: auto;
    max-width: 48em;
    line-height: 1.4;
  }

  p {
    font-size 11pt;
    font-weight: normal;
    color: #222222;
    text-indent: 4ex;
  }

  /* Treat underline as bold italic. Bit of a hack, bold and
  italicized is being ignored by pandoc when exporting to docx so
  using underline to identify text to manually change style to
  emphasize in word. But the intended effect is actually italic and
  bold so using this. */
  b, span.underline {
    font-family: sans-serif;
    font-weight: bold;
    font-style: italic;
    color: black;
    text-decoration: none;
  }

  code {
    color: black;
    font-weight: 600;
  }

  h1, h2, h3, h4, h5, h6 {
    color: black;
    font-family: \"Noto Sans\", sans-serif;
    margin-bottom: 0.5ex;
    margin-top: 3ex;
  }

  h2 {
    font-size: 24pt;
  }

  h3 {
    font-size: 14pt;
  }

  a:link {
    color: #0031a9;
  }

  a:visited {
    color: #673AB7;
  }

  pre.src:before {
    font-family: sans-serif;
    font-weight: bold;
    font-size: 12;
  }
</style>
")

    (defun org-latex-ref-to-cref (text backend _)
      "Use \\cref instead of \\ref in latex export."
      (when (org-export-derived-backend-p backend 'latex)
	(replace-regexp-in-string "\\\\ref{" "\\\\cref{" text)))

    (add-to-list 'org-export-filter-final-output-functions
                 'org-latex-ref-to-cref)


    (require 'ox-extra)
    (ox-extras-activate '(ignore-headlines))

    (my-local-leader-def
      :keymaps 'org-mode-map
      "v" #'org-view-output-file)

    ;;; Taken straight from tecosaur's config
    (defvar org-view-output-file-extensions '("pdf" "html" "org" "md" "rst" "txt" "tex")
      "Search for output files with these extensions, in order, viewing the first that matches")
    (defvar org-view-external-file-extensions '("html" "pdf")
      "File formats that should be opened externally.")

    (defun org-view-output-file (&optional org-file-path)
      "Visit buffer open on the first output file (if any) found, using `org-view-output-file-extensions'"
      (interactive)
      (let* ((org-file-path (or org-file-path (buffer-file-name) ""))
	     (dir (file-name-directory org-file-path))
	     (basename (file-name-base org-file-path))
	     (output-file nil))
	(dolist (ext org-view-output-file-extensions)
	  (unless output-file
	    (when (file-exists-p
		   (concat dir basename "." ext))
	      (setq output-file (concat dir basename "." ext)))))
	(if output-file
	    (if (member (file-name-extension output-file) org-view-external-file-extensions)
		(browse-url-xdg-open output-file)
	      (pop-to-buffer (or (find-buffer-visiting output-file)
				 (find-file-noselect output-file))))))))

  (use-package ox-gemini
    :disabled
    ;; :straight t
    )

  (use-package engrave-faces
    :config
    (require 'ox-latex)
    (require 'engrave-faces-html)
    (require 'engrave-faces-latex)
    (setq org-latex-src-block-backend 'engraved))

  (use-package ox-word
    :disabled
    :after org
    :load-path "~/packages/emacs/ox-word")

  (use-package sdcv-mode
    :straight '(sdcv-mode :type git :host github :repo "gucong/emacs-sdcv")
    :general
    (my-leader-def
      :infix "d"
      "d" #'sdcv-search)
    :config
    (general-nmap
      :keymaps 'sdcv-mode-map
      "q" #'evil-delete-buffer
      "C-SPC" #'sdcv-toggle-entry))

  (use-package wiki-summary
    :general
    (my-leader-def
      :infix "d"
      "k" #'wiki-summary))

  (use-package wordnut
    :general
    (my-leader-def
      :infix "d"
      "" '(:ignore t :which-key "Dictionary")
      "w" #'wordnut-search)
    :config
    (general-nmap
      :keymaps 'wordnut-mode-map
      "q" #'evil-delete-buffer)
    ))

(use-package org-pomodoro
  :general
  (my-local-leader-def
    :keymaps 'org-mode-map
    "c" #'org-pomodoro))

(use-package jinx
  :hook (emacs-startup . global-jinx-mode)
  :init
  ;; HACK not sure why ASPELL_CONF is not set inside emacs env.
  ;; (setenv "ASPELL_CONF"
  ;; 	  "dict-dir /nix/store/2n182xpayvhckd2nxjzr1j97yy6pxp69-aspell-env/lib/aspell")

  (require 'ispell)
  (setq ispell-program-name (executable-find "aspell")
	ispell-personal-dictionary (expand-file-name ".aspell.en.pws"
						     (getenv "HOME"))
	ispell-alternate-dictionary (expand-file-name "dicts/en-common.wl"
						      my-var-dir))
  :config
  (general-nmap
    "z=" #'jinx-correct)

  (set-face-attribute 'jinx-misspelled nil :underline t :inherit 'nano-face-popout))


(use-package flyspell
  :disabled
  :config
  (add-hook 'text-mode-hook #'flyspell-mode)
  (add-hook 'prog-mode-hook #'flyspell-prog-mode)
  (remove-hook 'org-mode-hook #'flyspell-mode)
  (setq ispell-program-name (executable-find "aspell")
	ispell-personal-dictionary "~/.aspell.en.pws"
	ispell-alternate-dictionary (expand-file-name "dicts/en-common.wl"
						      my-var-dir)))
(use-package flyspell-correct
  :after flyspell
  :general
  (general-nmap
    "z=" #'flyspell-correct-wrapper))

(use-package nov
  :mode ("\\.epub\\'" . nov-mode)
  :config
  (setq nov-text-width fill-column))

(use-package markdown-mode
  :hook (markdown-mode . visual-line-mode)
  :mode (("\\.md\\'" . gfm-mode)
	 ("\\.markdown\\'" . gfm-mode)
	 ("\\.Rmd\\'" . gfm-mode)
	 ("README\\.md\\'" . gfm-mode)
	 ("qutebrowser-editor" . gfm-mode))
  :config
  (my-local-leader-def
    :keymaps '(markdowm-mode-map gfm-mode-map)
    :infix "l"
    "l" #'markdown-insert-link)
  (general-nmap
    :keymaps '(markdown-mode-map gfm-mode-map)
    "gj" #'markdown-outline-next
    "gk" #'markdown-outline-previous
    "M-j" #'markdown-move-down
    "M-k" #'markdown-move-up
    "C-i" #'markdown-demote
    "C-S-i" #'markdown-promote))

(use-package poly-markdown
  :mode (("\\.md\\'" . poly-markdown-mode)
	 ("\\.Rmd\\'" . poly-markdown-mode)))

(use-package flymake-proselint
  :hook ((text-mode . flymake-proselint-setup)))

(use-package ox-pandoc
  :defer
  :init
  (setq org-pandoc-menu-entry
	'((?h "As HTML5" org-pandoc-export-to-html5)
	  (?5 "As HTML5-pdf" org-pandoc-export-to-html5-pdf)
	  (?l "As LaTeX-pdf" org-pandoc-export-to-latex-pdf)
	  (?b "As beamer-pdf" org-pandoc-export-to-beamer-pdf)
	  (?e "As epub" org-pandoc-export-to-epub3)
	  (?r "As rst" org-pandoc-export-to-rst)
	  (?m "As man" org-pandoc-export-to-man)
	  (?d "As docx" org-pandoc-export-to-docx)
	  (?o "As odt" org-pandoc-export-to-odt)))
  :config
  (setq org-pandoc-options '((data-dir . "~/.local/share/pandoc/")
			     (standalone . t))
	org-pandoc-options-for-docx '((reference-doc . "~/.local/share/pandoc/reference.docx"))))

(use-package tex-mode
  :config
  (add-to-list 'tex-compile-commands
	       '((concat "latexmk -xelatex" " %f") t "%r.pdf"))
  (add-to-list 'tex-compile-commands
	       '((concat "latexmk -lualatex" " %f") t "%r.pdf")))

(provide 'writting)
;;; writting ends here
