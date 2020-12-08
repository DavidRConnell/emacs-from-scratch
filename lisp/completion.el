;;; completion --- Setup completion types -*- lexical-binding: t -*-
;;   counsel/ivy
;;   company
;;   yas
;;   file-snippets or whatever it is. Let's make sure keeping good formed files.
;;; Commentary:
;;; Code:

(use-package prescient
  :init (setq prescient-save-file (expand-file-name
                                   "prescient-save.el" my-var-dir))
  :config
  (prescient-persist-mode +1))

(use-package selectrum
  :config
  (general-define-key
   :keymaps 'global
   "C-c C-r" #'selectrum-repeat)
  (general-define-key
   :keymaps 'selectrum-minibuffer-map
   "C-w" #'evil-delete-backward-word
   "C-u" #'evil-delete-back-to-indentation)

  (setq selectrum-num-candidates-displayed 10)
  (selectrum-mode +1)

  (use-package selectrum-prescient
    :after prescient
    :config
    (selectrum-prescient-mode +1))

  (use-package consult
    :straight '(consult :type git :host github :repo "minad/consult")
    :general
    (general-define-key
     :states '(normal visual motion)
     :jump t
     "/" #'consult-line
     "gn" #'consult-line-symbol-at-point)
    (my-leader-def
      :jump t
      "b" #'consult-buffer
      "C-b" #'consult-buffer-other-window
      "M-b" #'consult-buffer-other-frame)))

(use-package mini-frame
  :init (setq mini-frame-show-parameters
	      '((top . 0.33) (width . 0.7) (left . 0.5)))
  :config
  (mini-frame-mode +1)
  (define-advice fit-frame-to-buffer (:around (f &rest args) dont-skip-ws-for-mini-frame)
    (cl-letf* ((orig (symbol-function #'window-text-pixel-size))
	       ((symbol-function #'window-text-pixel-size)
		(lambda (win from to &rest args)
		  (apply orig
			 (append (list win from
				       (if (and (window-minibuffer-p win)
						(frame-root-window-p win)
						(eq t to))
					   nil
					 to))
				 args)))))
      (apply f args))))

(use-package ctrlf
  :disabled
  :general (general-nmap
	     "/" #'ctrlf-forward-fuzzy-regexp
	     "?" #'ctrlf-backward-fuzzy-regexp
	     "g*" #'ctrlf-forward-symbol-at-point)
  :config
  (ctrlf-mode +1))

(use-package ivy
  :disabled
  :config
  (use-package ivy-posframe
    :hook (ivy-mode . ivy-posframe-mode))
  (use-package counsel)
  (setq ivy-mode 1)
  (setq ivy-sort-max-size 7500)
  (require 'counsel nil t))

(use-package amx
  :commands amx
  :init
  (setq amx-save-file (expand-file-name "amx-items" my-var-dir)
	amx-backend 'selectrum)
  :config (amx-mode 1))

(use-package company
  :config
  (general-imap
    "C-SPC" #'company-complete
    "C-n"   #'company-dabbrev
    "C-f"   #'company-files
    "C-s"   #'company-ispell)
  (general-define-key
   :keymaps 'company-active-map
   "C-w" nil
   "C-n" #'company-select-next
   "C-p" #'company-select-previous
   "C-h" #'company-show-doc-buffer
   "C-s" #'company-filter-candidates
   "C-i" #'company-complete-selection
   "C-SPC" #'company-complete-common-or-cycle)
  (general-define-key
   :keymaps 'company-search-map
   "C-n" #'company-select-next-or-abort
   "C-p" #'company-select-previous-or-abort)
  (global-company-mode)
  (setq company-idle-delay 0
	company-minimum-prefix-length 2)
  (use-package company-quickhelp
    :disabled
    :hook (company-mode . company-quickhelp-mode))
  (use-package company-box
    :hook (company-mode . company-box-mode)
    :config (setq company-box-scrollbar nil))
  (use-package company-prescient
    :hook (company-mode . company-prescient-mode))
  (use-package company-math
    :general
    (general-imap
      "C-\\" #'company-math-symbols-unicode)))

(use-package yasnippet
  :hook ((text-mode prog-mode snippet-mode) .
	 yas-minor-mode-on)
  :general
  (general-imap
    "C-e" #'company-yasnippet)
  :config
  ;; (use-package yasnippet-snippets)

  (use-package doom-snippets
    :straight '(doom-snippets :host github :repo "hlissner/doom-snippets")
    :config
    (setq doom-snippets-dir (expand-file-name
			     "straight/repos/doom-snippets/"
			     my-cache-dir))
    (yas-reload-all))

  (general-define-key
   :keymaps 'yas-keymap
   "C-SPC" #'yas-next-field-or-maybe-expand)
  (defun my-add-yasnippet-backend ()
    (add-to-list 'company-backends #'company-yasnippet 'append))

  (add-hook 'yas-minor-mode-hook #'my-add-yasnippet-backend))



(provide 'completion)
