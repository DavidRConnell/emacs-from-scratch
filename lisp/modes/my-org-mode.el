;;; writting.el --- packages for writing -*- lexical-binding: t; -*-

;; Copyright (C) 2020 David R. Connell
;;
;; Author: David R. Connell <david32@dcon.addy.io>

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;; Configuration related to writing. Primarily based on `org-mode'.

;;; Code:

(customize-set-variable 'sentence-end-double-space nil)
(customize-set-variable 'org-modules '(org-id))

(require 'my-keybindings)
(require 'my-variables)
(require 'org)

(defvar my-ol-map (make-sparse-keymap))
(defvar my-ob-src-map (make-sparse-keymap))
(defvar my-org-fn-map (make-sparse-keymap))

(general-def
  :keymaps 'my-notes-map
  "l" 'org-store-link)

(general-imap
  :keymaps 'org-mode-map
  "C-i" 'org-do-demote
  "C-S-i" 'org-do-promote)

(general-nmap
  :keymaps 'org-mode-map
  "zn" 'org-toggle-narrow-to-subtree)

(my-local-leader-def
  :keymaps 'org-mode-map
  "t" 'org-todo
  "h" 'org-toggle-heading
  "i" 'org-toggle-item
  "a" 'org-priority
  "p" 'org-set-property
  "l" '(:keymap my-ol-map :which-key "links")
  "s" '(:keymap my-ob-src-map :which-key "src-blocks")
  "f" '(:keymap my-org-fn-map :which-key "footnotes"))

(general-def
  :keymaps 'my-ol-map
  "i" 'org-id-store-link
  "l" 'org-insert-link
  "L" 'org-insert-all-links
  "s" 'org-store-link
  "S" 'org-insert-last-stored-link
  "t" 'my-org-toggle-markup-display)

(general-def
  :keymaps 'my-ob-src-map
  "n" 'org-babel-next-src-block
  "p" 'org-babel-previous-src-block
  "g" 'org-babel-goto-named-src-block)

(general-def
  :keymaps 'my-org-fn-map
  "f" 'org-footnote-new
  "d" 'org-footnote-delete
  "a" 'org-footnote-action)

(add-hook 'org-mode-hook #'org-indent-mode)

(customize-set-variable 'org-directory my-zettle-dir)
(customize-set-variable 'org-startup-folded t)
(customize-set-variable 'org-hide-emphasis-markers t)
(customize-set-variable 'org-catch-invisible-edits 'smart)
(customize-set-variable 'org-list-allow-alphabetical t)
(customize-set-variable 'org-agenda-files '("todo.org" "habits.org" "meetings.org"))
(customize-set-variable 'org-tag-alist
			'(("ignore") ("noexport") ("export")))
(customize-set-variable 'org-ellipsis "…")
(customize-set-variable 'org-hide-leading-stars t)

(require 'org-superstar)
(add-hook 'org-mode-hook #'org-superstar-mode)
(set-face-attribute 'org-level-3 nil :foreground "black")

(customize-set-variable 'org-superstar-leading-bullet ?\s)
(customize-set-variable 'org-superstar-leading-fallback ?\s)
(customize-set-variable 'org-superstar-headline-bullets-list '(?▶ ?▷ ?◉ ?○))
(customize-set-variable 'org-superstar-todo-bullet-alist
			'(("TODO" . 9744) ("[ ]" . 9744)
			  ("DONE" . 9745) ("[X]" . 9745)))

(add-to-list 'org-file-apps '("\\.pdf\\'" . "xdg-open %s"))

(add-hook 'org-after-todo-state-change-hook
	  #'(lambda ()
	      (if (org-entry-is-todo-p)
		  (org-reset-checkbox-state-subtree))))

(defun my-org-toggle-emphasis-markers-display ()
  (interactive)
  (setq-local org-hide-emphasis-markers (not org-hide-emphasis-markers))
  (font-lock-fontify-buffer))

(defun my-org-toggle-markup-display ()
  (interactive)
  (org-toggle-link-display)
  (my-org-toggle-emphasis-markers-display))

(my-leader-def
  "A" '(lambda (arg) (interactive "P") (org-agenda arg "a")))

(general-def
  :keymaps 'org-mode-map
  :prefix "C-c"
  "C-q" 'counsel-org-tag
  "r" 'org-refile)

(general-def
  :keymaps 'org-src-mode-map
  "C-c C-'" 'org-edit-src-exit)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (python . t)
   (shell . t)
   (emacs-lisp . t)
   (latex . t)
   (gnuplot . t)))

(with-eval-after-load 'evil
  (customize-set-variable 'evil-org-retain-visual-state-on-shift t)
  (customize-set-variable 'evil-org-special-o/O '(table-row))
  (customize-set-variable 'evil-org-use-additional-insert t)

  (require 'evil-org)
  (require 'org-tweaks)

  (add-hook 'org-mode-hook #'evil-org-mode)

  (add-hook 'evil-org-mode-hook #'evil-normalize-keymaps)
  (evil-org-set-key-theme '(navigation insert textobjects additional))

  (general-def
    :states '(normal insert)
    :keymaps 'org-mode-map
    [C-return] '+org/insert-item-below
    [C-S-return] '+org/insert-item-above
    [C-M-return] 'org-insert-subheading))

(autoload 'org-cliplink "org-cliplink")
(general-def
  :keymaps 'my-ol-map
  "c" 'org-cliplink)

(require 'org-appear)
(add-hook 'org-mode-hook #'org-appear-mode)
(add-hook 'evil-insert-state-entry-hook #'org-appear-manual-start nil t)
(add-hook 'evil-insert-state-exit-hook #'org-appear-manual-stop nil t)

(customize-set-variable 'org-appear-trigger 'always)
(customize-set-variable 'org-appear-autoentities t)
(customize-set-variable 'org-appear-autoemphasis t)

(autoload 'org-pomodoro "org-pomodoro")
(my-local-leader-def
  :keymaps 'org-mode-map
  "c" 'org-pomodoro)

(autoload 'org-export-dispatch "ox" nil t)
(with-eval-after-load 'ox
  (require 'ox-pandoc)
  (require 'ox-latex)
  (require 'ox-html)

  (customize-set-variable 'org-export-with-sub-superscripts '{})
  (customize-set-variable 'org-export-in-background nil)
  (customize-set-variable 'org-odt-preferred-output-format "odt")

  (require 'ox-extra)
  (ox-extras-activate '(ignore-headlines))

  (my-local-leader-def
    :keymaps 'org-mode-map
    "v" 'org-view-output-file)

  ;; Taken straight from tecosaur's config
  (defvar org-view-output-file-extensions
    '("pdf" "html" "org" "md" "rst" "txt" "tex")
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
			       (find-file-noselect output-file)))))))

  (require 'engrave-faces))

(with-eval-after-load 'ox-pandoc
  (customize-set-variable 'org-pandoc-menu-entry
			  '((?h "As HTML5" org-pandoc-export-to-html5)
			    (?5 "As HTML5-pdf" org-pandoc-export-to-html5-pdf)
			    (?l "As LaTeX-pdf" org-pandoc-export-to-latex-pdf)
			    (?b "As beamer-pdf" org-pandoc-export-to-beamer-pdf)
			    (?e "As epub" org-pandoc-export-to-epub3)
			    (?r "As rst" org-pandoc-export-to-rst)
			    (?m "As man" org-pandoc-export-to-man)
			    (?d "As docx" org-pandoc-export-to-docx)
			    (?o "As odt" org-pandoc-export-to-odt)))

  (let ((pandoc-dir (expand-file-name "pandoc" (getenv "XDG_DATA_DIR"))))
    (customize-set-variable 'org-pandoc-options
			    `((data-dir . ,pandoc-dir)
			      (standalone . t)))
    (customize-set-variable
     'org-pandoc-options-for-docx
     `((reference-doc . ,(expand-file-name "reference.docx" pandoc-dir))))))

;; Note this should load with ox. Using this to break up into smaller chunks.
(with-eval-after-load 'ox-latex
  (require 'engrave-faces-latex)
  (customize-set-variable 'org-latex-src-block-backend 'engraved)
  (customize-set-variable 'org-latex-prefer-user-labels t)
  (customize-set-variable 'org-latex-pdf-process
			  '("[ -d %o/build ] || mkdir %o/build"
			    "latexmk -g -%latex -outdir=%o/build %f"
			    "mv %o/build/*.pdf %o"))
  (customize-set-variable 'org-latex-default-packages-alist
			  '(("AUTO" "inputenc" t ("pdflatex"))
			    ("T1" "fontenc" t ("pdflatex"))
			    ("" "fontspec" t ("lualatex" "xelatex"))
			    ("tracking=true, letterspace=30" "microtype" t ("lualatex"))
			    ("" "microtype" t ("xelatex"))))

  (customize-set-variable 'org-latex-packages-alist
			  '(("" "xcolor" nil)
			    ("" "amsmath" nil)
			    ("" "unicode-math" nil)
			    ("" "hyperref" nil)
			    ("capitalise, nameinlink, noabbrev" "cleveref" nil)))
  (customize-set-variable 'org-latex-classes
			  `(("article"
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
			     ("\\section{%s}" . "\\section*{%s}")))))

(with-eval-after-load 'ox-html
  (require 'engrave-faces-html)

  (customize-set-variable 'org-html-validation-link nil)
  (customize-set-variable 'org-html-head "
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
"))

(provide 'my-org-mode)
;;; my-org-mode.el ends here
