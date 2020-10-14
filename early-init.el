;;; early-init.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 David R. Connell ;; ;; Author: David R. Connell <http://github/voidee> ;; Maintainer: David R. Connell <voidee@TheVoid> ;; Created: October 13, 2020 ;; Modified: October 13, 2020 ;; Version: 0.0.1 ;; Keywords: ;; Homepage: https://github.com/voidee/early-init ;; Package-Requires: ((emacs 27.1) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(push '(tool-bar-lines . 0) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(font . "Hack Nerd Font Mono-14") default-frame-alist)

(blink-cursor-mode -1)
(scroll-bar-mode -1)


(setq package-enable-at-startup nil)

(provide 'early-init)
;;; early-init.el ends here
