;;; appearance.el --- Tweak the appearance of emacs -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(load-theme 'modus-operandi t)
(setq-default mode-line-format nil)
(blink-cursor-mode -1)
(setq-default fill-column 79)
(global-display-fill-column-indicator-mode t)
(show-paren-mode t)
(global-hl-line-mode 1)

(use-package visual-fill-column
  :hook (visual-line-mode . visual-fill-column-mode))

(require 'mode-line)
(set-face-attribute 'modus-themes-completion-selected nil
		    :foreground 'unspecified :background nano-color-highlight
		    :family 'unspecified :slant 'unspecified
		    :weight 'unspecified :height 'unspecified
		    :underline 'unspecified :overline 'unspecified
		    :box 'unspecified :inherit 'bold)

;; Fall back font for glyph missing in Roboto.
(defface fallback '((t :family "Fira Code" :inherit 'nano-face-faded))
  "Fallback font.")
(set-display-table-slot standard-display-table 'truncation
                        (make-glyph-code ?… 'fallback))
(set-display-table-slot standard-display-table 'wrap
                        (make-glyph-code ?↩ 'fallback))

(use-package eww
  :config
  (defun mozilla-readable (url)
    (let ((buff (format "*readable-%s*" url)))
      (with-current-buffer (get-buffer-create buff)
	(pop-to-buffer (current-buffer))
	(shell-command (format "readable %s" url) (current-buffer))
	(eww-display-html 'utf-8 (buffer-name) nil (point-min) (current-buffer)))))

  (defun eww-readable ()
    "View the main \"readable\" parts of the current web page.
This command uses heuristics to find the parts of the web page that
contains the main textual portion, leaving out navigation menus and
the like."
    (interactive nil eww-mode)
    (let* ((old-data eww-data)
	   (dom (with-temp-buffer
		  (insert (plist-get old-data :source))
		  (condition-case nil
		      (decode-coding-region (point-min) (point-max) 'utf-8)
		    (coding-system-error nil))
		  (shell-command-on-region (point-min) (point-max)
					   "readable" nil t)
                  (eww--preprocess-html (point-min) (point-max))
		  (libxml-parse-html-region (point-min) (point-max))))
           (base (plist-get eww-data :url)))
      (eww-score-readability dom)
      (eww-save-history)
      (eww-display-html nil nil
			(list 'base (list (cons 'href base))
                              (eww-highest-readability dom))
			nil (current-buffer))
      (dolist (elem '(:source :url :title :next :previous :up :peer))
	(plist-put eww-data elem (plist-get old-data elem)))
      (eww--after-page-change))))

(provide 'appearance)
