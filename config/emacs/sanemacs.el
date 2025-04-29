;;; -*- lexical-binding: t -*-

;; Customize Emacs
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'which-key-mode) (which-key-mode +1))
(column-number-mode +1)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(when (fboundp 'completion-preview-mode)
  (add-hook 'prog-mode-hook #'completion-preview-mode))
(fset 'yes-or-no-p 'y-or-n-p)
(setq
 inhibit-startup-screen t
 initial-major-mode 'fundamental-mode
 initial-scratch-message ""
 sentence-end-double-space nil)
(setq-default
 cursor-type 'bar
 indent-tabs-mode nil
 tab-width 4)
(load-theme 'modus-vivendi)
(defun sanemacs/show-trailing-whitespace ()
  "Shows trailing whitespace for the current buffer."
  (setq show-trailing-whitespace t))
(add-hook 'prog-mode-hook #'sanemacs/show-trailing-whitespace)

;; Configure features to use `.cache` directory
(setq
 auto-save-list-file-prefix "~/.cache/emacs/auto-save/"
 auto-save-file-name-transforms '((".*" "~/.cache/emacs/auto-save/" t))
 backup-by-copying t
 backup-directory-alist '((".*" . "~/.cache/emacs/backup/"))
 bookmark-default-file "~/.cache/emacs/bookmarks"
 create-lockfiles nil
 eshell-directory-name "~/.cache/emacs/eshell"
 treesit-extra-load-path '("~/.cache/emacs/tree-sitter"))
(with-eval-after-load 'tramp-cache
  (setq tramp-persistency-file-name "~/.cache/emacs/tramp"))

;; Configure package management
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(setq package-archive-priorities '(("gnu" . 1) ("nongnu" . 1) ("melpa" . 0)))
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; Install standard packages
(use-package ace-window
  :bind
  (("C-x o" . ace-window)))
(use-package avy
  :bind
  (("C-:" . avy-goto-char)
   ("M-g f" . avy-goto-line)))
(use-package undo-tree
  :custom
  (undo-tree-auto-save-history nil)
  :config
  (global-undo-tree-mode +1))

;; Custom keybinds
(keymap-global-set "C-x k" #'kill-current-buffer)

;; Use separate file for custom variables
(setq custom-file "~/.cache/emacs/custom.el")
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(load custom-file nil t)
