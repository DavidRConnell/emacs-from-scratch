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
  :general
  (general-define-key
   :keymaps 'global
   "C-c C-r" #'selectrum-repeat)
  (general-define-key
   :keymaps 'selectrum-minibuffer-map
   "C-w" #'evil-delete-backward-word)
  :config
  (setq selectrum-num-candidates-displayed 10)
  (selectrum-mode +1)

  (use-package selectrum-prescient
    :after prescient
    :config
    (selectrum-prescient-mode +1))

  (use-package consult
    :straight '(consult :type git :host github :repo "minad/consult")
    :general
    ;; (general-define-key
    ;;  :states '(normal visual motion)
    ;;  "/" #'consult-line
    ;;  "gn" #'consult-line-symbol-at-point)
    (my-leader-def
      "b" #'consult-buffer)
    :config
    (setf (alist-get 'execute-extended-command consult-annotate-alist)
	  #'consult-annotate-command-full)))

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
  :config (amx-mode 1))

(use-package company
  :general
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
  :config
  (global-company-mode)
  (setq company-idle-delay 0
	company-minimum-prefix-length 2)
  (use-package company-quickhelp
    :disabled
    :hook (company-mode . company-quickhelp-mode))
  (use-package company-prescient
    :hook (company-mode . company-prescient-mode)))


(provide 'completion)
