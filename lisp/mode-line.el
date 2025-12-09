;;; mode-line.el --- clean nano mode-line -*- lexical-binding: t; -*-

;;; Commentary:
;; From rougier's nano-emacs
;; https://github.com/rougier/nano-emacs/blob/master/nano-modeline.el.
;; With cleanup this will should be integrated back into the vendored nano
;; package.

;;; Code:

(require 'subr-x)
(require 'nano-theme-light)

;; -------------------------------------------------------------------
(defun vc-branch ()
  (if vc-mode
      (let ((backend (vc-backend buffer-file-name)))
        (concat "#" (substring-no-properties vc-mode
                                 (+ (if (eq backend 'Hg) 2 3) 2))))  nil))


;; From https://amitp.blogspot.com/2011/08/emacs-custom-mode-line.html
;; ---------------------------------------------------------------------
(defun shorten-directory (dir max-length)
  "Show up to MAX-LENGTH characters of a directory name DIR."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat "…/" output)))
    output))

;; ---------------------------------------------------------------------

(defun nano-modeline-compose (status name primary secondary)
  "Compose a string with provided information."
  (let* ((char-width    (window-font-width nil 'default))
	 (filler " ")
         (space-up       +0.15)
         (space-down     -0.20)
	 (gui            (display-graphic-p))
	 (prefix (cond ((string= status "RO")
			(propertize " RO "
				    'face 'nano-face-header-popout))
                       ((string= status "**")
			(propertize " ** "
				    'face 'nano-face-header-critical))
                       ((string= status "RW")
			(propertize " RW "
				    'face 'nano-face-header-faded))
                       (t (propertize status
				      'face 'nano-face-header-popout))))
         (left (concat
                (propertize " "  'face 'nano-face-header-default
			         'display `(raise ,space-up))
                (propertize name 'face 'nano-face-header-strong)
                (propertize " "  'face 'nano-face-header-default
			         'display `(raise ,space-down))
		(propertize primary 'face 'nano-face-header-default)))
         (right (concat secondary " "))
         (available-width (- (window-body-width) 1
			     (length prefix) (length left) (length right)))
	 (available-width (max 1 available-width)))
    (concat prefix
	    (if gui
		(propertize " " 'face 'nano-face-header-separator))
	    left
	    (propertize (make-string available-width ?\ ) 'face 'nano-face-header-default)
	    (if gui
		(propertize filler 'face 'nano-face-header-filler)
	      (propertize " " 'face 'nano-face-header-filler))
	    right
	    (if gui
		(propertize " "   'face 'nano-face-header-separator)))))

;; ---------------------------------------------------------------------
(set-face-attribute 'eyebrowse-mode-line-active nil
		    :foreground nano-color-salient
		    :weight 'bold
		    :inherit 'nano-face-header-default)
(set-face 'eyebrowse-mode-line-inactive 'nano-face-header-default)
(set-face 'eyebrowse-mode-line-delimiters 'nano-face-header-default)
(set-face 'eyebrowse-mode-line-separator 'nano-face-header-default)

(defun nano-modeline-right-side ()
  "Provide general information for the right side of the modeline."
  (let* ((line-info (format-mode-line "%l:%c %p%%  "))
	 (eyebrowse-workspaces (eyebrowse-mode-line-indicator))
	 (popup (if (member popper-popup-status '(popup user-popup))
		    (propertize " POP " 'face 'nano-face-header-popout)
		  ""))
	 (buffer (max 1 (- 7 (length line-info)))))
    (concat eyebrowse-workspaces
	    (propertize (make-string buffer ?\ ) 'face 'nano-face-header-default)
	    (propertize line-info 'face 'nano-face-header-default)
	    (propertize " " 'face 'nano-face-header-separator)
	    popup)))

;; ---------------------------------------------------------------------
(defun my-org-roam-db-has-file-p (file)
  (not (null (org-roam-db-query
	      [:select * :from titles :where (= file $s1)] file))))

(defun nano-modeline-org-roam-mode-p ()
  (and  (fboundp 'org-roam-file-p)
	(derived-mode-p 'org-mode)
	(org-roam-file-p (buffer-file-name (window-buffer)))))

(defun nano-modeline-org-roam-mode ()
  (nano-modeline-compose (nano-modeline-status)
			 (org-roam-node-title
			  (org-roam-node-from-id (org-entry-get nil "ID" 'inherit)))
			 "(Org-roam)"
			 ""))

;; ---------------------------------------------------------------------
(defun nano-modeline-mu4e-dashboard-mode-p ()
  (bound-and-true-p mu4e-dashboard-mode))

(defun nano-modeline-mu4e-dashboard-mode ()
  (nano-modeline-compose (nano-modeline-status)
                         "Mail"
                         (nano-modeline-mu4e-context)
                         ""))

;; ---------------------------------------------------------------------
(defun nano-modeline-elfeed-search-mode-p ()
  (derived-mode-p 'elfeed-search-mode))

(defun nano-modeline-elfeed-search-mode ()
  (nano-modeline-compose (nano-modeline-status)
                         "Elfeed"
                         (concat "(" (elfeed-search--header)  ")")
                         ""))

;; Elfeed (regular header)
(with-eval-after-load 'elfeed
  (defun elfeed-setup-header ()
    (setq header-line-format (default-value 'header-line-format)))
  (setq elfeed-search-header-function #'elfeed-setup-header))

;; ---------------------------------------------------------------------
(defun nano-modeline-elfeed-show-mode-p ()
  (derived-mode-p 'elfeed-show-mode))

(defun nano-modeline-elfeed-show-mode ()
  (let* ((title        (elfeed-entry-title elfeed-show-entry))
         (feed         (elfeed-entry-feed elfeed-show-entry))
         (feed-title   (plist-get (elfeed-feed-meta feed) :title)))
    (nano-modeline-compose (nano-modeline-status)
                           (s-truncate 40 title "…")
                           ""
                           "")))

;; ---------------------------------------------------------------------
(defun nano-modeline-calendar-mode-p ()
  (derived-mode-p 'calendar-mode))

(defun nano-modeline-calendar-mode () "")

;; Calendar (no header, only overline)
(with-eval-after-load 'calendar
  (defun calendar-setup-header ()
    (setq header-line-format "")
    (face-remap-add-relative
     'header-line `(:overline ,(face-foreground 'default)
                    :height 0.5
                    :background ,(face-background 'default))))
  (add-hook 'calendar-initial-window-hook #'calendar-setup-header)

  ;; From https://emacs.stackexchange.com/questions/45650
  (add-to-list 'display-buffer-alist
               `(,(rx string-start "*Calendar*" string-end)
                 (display-buffer-below-selected))))

;; ---------------------------------------------------------------------
(defun nano-modeline-org-capture-mode-p ()
  (bound-and-true-p org-capture-mode))

(defun nano-modeline-org-capture-mode ()
  (nano-modeline-compose (nano-modeline-status)
                         "Capture"
                         "(org)"
                         ""))

(with-eval-after-load 'org-capture
  (defun org-capture-turn-off-header-line ()
    (setq-local header-line-format (default-value 'header-line-format))
    ;; (fit-window-to-buffer nil nil 8)
    ;; (face-remap-add-relative 'header-line '(:background "#ffffff"))
    (message nil))
  (add-hook 'org-capture-mode-hook
            #'org-capture-turn-off-header-line))

;; ---------------------------------------------------------------------
(defun nano-modeline-org-agenda-mode-p ()
  (derived-mode-p 'org-agenda-mode))

(defun nano-modeline-org-agenda-mode ()
  (nano-modeline-compose (nano-modeline-status)
                         "Agenda"
                         ""
                         (format-time-string "%H:%M")
                         ))

;; ---------------------------------------------------------------------
(defun nano-modeline-term-mode-p ()
  (derived-mode-p 'term-mode))

(defun nano-modeline-term-mode ()
  (nano-modeline-compose " >_ "
                         "Terminal"
                         (concat "(" shell-file-name ")")
                         (shorten-directory default-directory 32)))

;; ---------------------------------------------------------------------
(defun nano-modeline-mu4e-main-mode-p ()
  (derived-mode-p 'mu4e-main-mode))

(defun nano-modeline-mu4e-main-mode ()
  (nano-modeline-compose (nano-modeline-status)
                         "Mail"
                         (nano-modeline-mu4e-context)
                         (format-time-string "%A %d %B %Y, %H:%M")))

;; ---------------------------------------------------------------------
(defun nano-modeline-mu4e-headers-mode-p ()
  (derived-mode-p 'mu4e-headers-mode))

(defun nano-modeline-mu4e-headers-mode ()
  (nano-modeline-compose (nano-modeline-status)
                         (mu4e~quote-for-modeline mu4e~headers-last-query)
                         ""
                         ""))

(with-eval-after-load 'mu4e
  (defun mu4e~header-line-format () (nano-modeline)))

;; ---------------------------------------------------------------------
(setq mu4e-modeline-max-width 72)

(defun nano-modeline-mu4e-view-mode-p ()
  (derived-mode-p 'mu4e-view-mode))

(defun nano-modeline-mu4e-view-mode ()
  (let* ((msg     (mu4e-message-at-point))
         (subject (mu4e-message-field msg :subject))
         (from    (mu4e~headers-contact-str (mu4e-message-field msg :from)))
         (date     (mu4e-message-field msg :date)))
    (nano-modeline-compose (nano-modeline-status)
                           (s-truncate 40 subject "…")
                           ""
                           from)))

(add-hook 'mu4e-view-mode-hook
          (lambda () (setq header-line-format "%-")
                     (face-remap-add-relative 'header-line
                                              '(:background "#ffffff" :height 1.0))))


;; ---------------------------------------------------------------------
(defun nano-modeline-message-mode-p ()
  (derived-mode-p 'message-mode))

(defun nano-modeline-mssage-mode ()
  (nano-modeline-compose (nano-modeline-status)
                         "Message" "(draft)" ""
			 '(( "SEND" . message-send))))


;; ---------------------------------------------------------------------
(setq org-mode-line-string nil)
(with-eval-after-load 'org-clock
  (add-hook 'org-clock-out-hook
            #'(lambda () (setq org-mode-line-string nil)
                        (force-mode-line-update))))

(defun nano-modeline-org-clock-mode-p ()
  org-mode-line-string)

(defun nano-modeline-org-clock-mode ()
    (let ((buffer-name (format-mode-line "%b"))
          (mode-name   (format-mode-line "%m"))
          (branch      (vc-branch)))
      (nano-modeline-compose (nano-modeline-status)
                             buffer-name
                             (concat "(" mode-name
                                     (if branch (concat ", "
                                             (propertize branch 'face 'italic)))
                                     ")" )
                             org-mode-line-string)))

;; ---------------------------------------------------------------------
(defun nano-modeline-docview-mode-p ()
  (derived-mode-p 'doc-view-mode))

(defun nano-modeline-docview-mode ()
  (let ((buffer-name (format-mode-line "%b"))
	(mode-name   (format-mode-line "%m"))
	(branch      (vc-branch))
	(page-number (concat
		      (number-to-string (doc-view-current-page)) "/"
		      (or (ignore-errors
			    (number-to-string (doc-view-last-page-number)))
			  "???"))))
    (nano-modeline-compose
     (nano-modeline-status)
     buffer-name
     (concat "(" mode-name
	     (if branch (concat ", "
				(propertize branch 'face 'italic)))
	     ")" )
     page-number)))

;; ---------------------------------------------------------------------
(defun nano-modeline-pdf-view-mode-p ()
  (derived-mode-p 'pdf-view-mode))

(defun nano-modeline-pdf-view-mode ()
  (let ((buffer-name (format-mode-line "%b"))
	(mode-name   (format-mode-line "%m"))
	(branch      (vc-branch))
	(page-number (concat
		      (number-to-string (pdf-view-current-page)) "/"
		      (or (ignore-errors
			    (number-to-string (pdf-cache-number-of-pages)))
			  "???"))))
    (nano-modeline-compose
     "RW"
     buffer-name
     (concat "(" mode-name
	     (if branch (concat ", "
				(propertize branch 'face 'italic)))
	     ")" )
     page-number)))

;; ---------------------------------------------------------------------
(defun buffer-menu-mode-header-line ()
  (face-remap-add-relative
   'header-line `(:background ,(face-background 'nano-face-subtle))))
(add-hook 'Buffer-menu-mode-hook
          #'buffer-menu-mode-header-line)

;; ---------------------------------------------------------------------
(defun nano-modeline-completion-list-mode-p ()
  (derived-mode-p 'completion-list-mode))

(defun nano-modeline-completion-list-mode ()
    (let ((buffer-name (format-mode-line "%b"))
          (mode-name   (format-mode-line "%m")))
      (nano-modeline-compose (nano-modeline-status)
                             buffer-name "" (nano-modeline-right-side))))

;; ---------------------------------------------------------------------

(defun nano-modeline-default-mode ()
    (let ((buffer-name (format-mode-line "%b"))
          (mode-name   (format-mode-line "%m"))
          (branch      (vc-branch)))
      (nano-modeline-compose (nano-modeline-status)
                             buffer-name
                             (concat "(" mode-name
                                     (if branch (concat ", "
                                            (propertize branch 'face 'italic)))
                                     ")" )
                             (nano-modeline-right-side))))

;; ---------------------------------------------------------------------
(defun nano-modeline-status ()
  "Return buffer status: read-only (RO), modified (**) or read-write (RW)."

  (let ((read-only   buffer-read-only)
        (modified    (and buffer-file-name (buffer-modified-p))))
    (cond (modified  "**") (read-only "RO") (t "RW"))))

;; ---------------------------------------------------------------------
(defun nano-modeline-mu4e-context ()
  "Return the current mu4e context as a non propertized string."

  (if (> (length (mu4e-context-label)) 0)
      (concat "(" (substring-no-properties (mu4e-context-label) 1 -1) ")")
    "(none)"))


;; ---------------------------------------------------------------------
(defun nano-modeline ()
  "Install a header line whose content is dependend on the major mode"
  (interactive)
  (setq-default header-line-format
  '((:eval
     (cond ((nano-modeline-elfeed-search-mode-p)   (nano-modeline-elfeed-search-mode))
           ((nano-modeline-elfeed-show-mode-p)     (nano-modeline-elfeed-show-mode))
           ((nano-modeline-calendar-mode-p)        (nano-modeline-calendar-mode))
           ((nano-modeline-org-capture-mode-p)     (nano-modeline-org-capture-mode))
           ((nano-modeline-org-agenda-mode-p)      (nano-modeline-org-agenda-mode))
           ((nano-modeline-org-clock-mode-p)       (nano-modeline-org-clock-mode))
           ((nano-modeline-term-mode-p)            (nano-modeline-term-mode))
           ((nano-modeline-mu4e-dashboard-mode-p)  (nano-modeline-mu4e-dashboard-mode))
           ((nano-modeline-mu4e-main-mode-p)       (nano-modeline-mu4e-main-mode))
           ((nano-modeline-mu4e-headers-mode-p)    (nano-modeline-mu4e-headers-mode))
           ((nano-modeline-pdf-view-mode-p)        (nano-modeline-pdf-view-mode))
	   ((nano-modeline-docview-mode-p)         (nano-modeline-docview-mode))
	   ((nano-modeline-completion-list-mode-p) (nano-modeline-completion-list-mode))
	   ((nano-modeline-message-mode-p)         (nano-modeline-mssage-mode))
	   ((nano-modeline-org-roam-mode-p)        (nano-modeline-org-roam-mode))
	   (t                                      (nano-modeline-default-mode)))))))

;; ---------------------------------------------------------------------
(defun nano-modeline-update-windows ()
  "Modify the mode line depending on the presence of a window below."

  (dolist (window (window-list))
    (with-selected-window window
      (if (or (one-window-p t)
	      (eq (window-in-direction 'below) (minibuffer-window))
	      (not (window-in-direction 'below)))
	      (with-current-buffer (window-buffer window)
	        (setq mode-line-format "%-"))
	    (with-current-buffer (window-buffer window)
 	      (setq mode-line-format nil)))
;;      (if (window-in-direction 'above)
;;	      (face-remap-add-relative 'header-line '(:overline "#777777"))
;;	    (face-remap-add-relative 'header-line '(:overline nil)))
      )))
(add-hook 'window-configuration-change-hook 'nano-modeline-update-windows)

(setq eshell-status-in-modeline nil)
(setq-default mode-line-format "%-")
(nano-modeline)

(provide 'mode-line)
;;; mode-line.el ends here
