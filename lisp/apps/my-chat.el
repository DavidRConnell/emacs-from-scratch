;;; my-chat.el --- Chat room interfaces -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 David R. Connell
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
;; Sets up packages for interacting with locally running LLMs and regular human
;; chat rooms.

;;; Code:

(require 'my-ui)
(require 'my-keybindings)

(setq gptel-default-mode 'org-mode
      gptel-org-branching-context t
      gptel-use-header-line nil)

(autoload 'gptel-menu "gptel-transient")
(autoload 'gptel "gptel")
(autoload 'gptel-system-prompt "gptel")

(defvar my-chat-map (make-sparse-keymap))

(my-leader-def
  "a" '(:keymap my-chat-map :which-key "chat"))

(defun my-gptel-scratch ()
  (interactive)
  (let* ((project (project-name (project-current)))
	 (scratch-buffer-name (format "*GPTel %s scratch*" project)))
    (with-current-buffer (get-buffer-create scratch-buffer-name)
      (org-mode)
      (when (equal 0 (buffer-size))
	(insert (format "* Scratch\n%s"
			(alist-get 'org-mode gptel-prompt-prefix-alist)))
	(gptel-org-set-properties 0 nil)))
    (gptel scratch-buffer-name nil nil t)))

(defun my-gptel-open-workspace ()
  (interactive)
  (let* ((ws-dir (expand-file-name "chatgpt" "~/notes"))
	 (ws (completing-read "File: "
			      (directory-files ws-dir
					       nil
					       (rx ".org" eos)))))
    (gptel (find-file (expand-file-name ws ws-dir)))))

(general-def
  :keymaps 'my-chat-map
  "s" 'gptel-menu
  "c" 'my-gptel-scratch
  "p" 'gptel-system-prompt
  "w" (lambda () (interactive) (my-gptel-open-workspace)
	(popper-lower-to-popup))
  "W" 'my-gptel-open-workspace)

(with-eval-after-load 'gptel
  (require 'gptel-org)
  (require 'gptel-openai-oauth)
  (require 'gptel-context)

  (setf (alist-get 'org-mode gptel-prompt-prefix-alist) "@User: ")
  (setf (alist-get 'org-mode gptel-response-prefix-alist) "@Assistant: ")

  (my-popper-add-reference "\\*GPTel .*\\*")

  (customize-set-variable 'gptel-model 'gpt-6-astra)
  (customize-set-variable 'gptel-backend
			  (gptel-make-openai-oauth "OpenAI"
			    :stream t
			    :models '(gpt-6-astra
				      gpt-5.6-sol))))

(provide 'my-chat)
;;; my-chat.el ends here
