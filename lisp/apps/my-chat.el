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

(use-package ellama
  :general
  (my-leader-def
    "a" #'ellama-transient-main-menu)
  :config
  (require 'llm-ollama)
  (setopt ellama-coding-provider
	  (make-llm-ollama
	   :chat-model "qwen2.5-coder:14b"
	   :embedding-model "nomic-embed-text"
	   :default-chat-non-standard-params '(("num_ctx" . 32768))))
  (setq ellama-provider "qwen2.5:14b"))

(use-package gptel
  :disabled
  :general
  (my-leader-def
    :infix "a"
    "s" #'gptel-menu
    "c" #'gptel
    "p" #'gptel-system-prompt)
  :config
  (require 'gptel-ollama)
  (require 'gptel-org)
  (require 'gptel-context)
  (require 'gptel-curl)
  (setq gptel-model 'qwen2.5:14b
	gptel-backend (gptel-make-ollama "Ollama"
					 :host "localhost:11434"
					 :stream t
					 :models '(qwen2.5:14b
						   phi4:latest
						   qwen2.5-coder:14b
						   starcoder2:15b
						   codellama:13b))))

(use-package ement
  :disabled
  :general
  (my-leader-def
    :infix "c"
    "c" #'ement-connect
    "C" #'ement-disconnect
    "l" #'ement-list-rooms
    "v" #'ement-view-room
    "j" #'ement-join-room)
  :config
  (my-local-leader-def
    :keymaps 'ement-room-mode-map
    "/" #'ement-room-occur
    "e" #'ement-room-edit-message
    "f" #'ement-room-send-file
    "i" #'ement-room-send-image
    "l" #'ement-leave-room
    "m" #'ement-list-members
    "r" #'ement-room-retro))

(provide 'my-chat)
;;; my-chat.el ends here
