;;; Company --- code for seting up company completion -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package company
  :config
  (general-define-key
   :keymaps 'company-active-map
   "C-w" nil
   "C-n" #'company-select-next
   "C-p" #'company-select-previous
   "C-h" #'company-show-doc-buffer
   "C-s" #'company-filter-candidates
   "C-i" #'company-complete-selection
   "C-SPC" #'company-complete-common-or-cycle)

  (general-imap
    "C-SPC" #'company-complete
    "C-n" #'company-dabbrev
    "C-f" #'company-files
    "C-s" #'company-ispell)

  (general-define-key
   :keymaps 'company-search-map
   "C-n" #'company-select-next-or-abort
   "C-p" #'company-select-previous-or-abort)

  (global-company-mode 1)
  (add-hook 'text-mode-hook (defun my-set-text-mode-backends ()
			      (setq-local company-backends
					  '(company-dabbrev))))

  (setq company-idle-delay nil
	company-minimum-prefix-length 2
	company-selection-wrap-around t)

  (use-package company-box
    :disabled
    :hook (company-mode . company-box-mode)
    :config (setq company-box-scrollbar nil))

  (use-package company-prescient
    :hook (company-mode . company-prescient-mode))

  (use-package company-math
    :general
    (general-imap
      "C-\\" #'company-math-symbols-unicode)))

(provide 'company-setup)
;;; company-setup.el ends here
