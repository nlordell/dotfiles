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
 indent-tab-mode nil
 show-trailing-whitespace t
 tab-width 4)
(load-theme 'modus-vivendi)

;; Write auto-saves, backups and bookmarks to `.cache` directory
(setq
 auto-save-list-file-prefix "~/.cache/emacs/auto-save/"
 auto-save-file-name-transforms '((".*" "~/.cache/emacs/auto-save/" t))
 backup-by-copying t
 backup-directory-alist '((".*" . "~/.cache/emacs/backup/"))
 bookmark-default-file "~/.cache/emacs/bookmarks"
 create-lockfiles nil)

;; Configure TRAMP
(with-eval-after-load 'tramp-cache
  (setq tramp-persistency-file-name "~/.cache/emacs/tramp"))

;; Use separate file for custom variables
(setq custom-file "~/.cache/emacs/custom.el")
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(load custom-file nil t)

;; Configure package management
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(setq package-archive-priorities '(("gnu" . 1) ("nongnu" . 1) ("melpa" . 0)))
(require 'use-package)
(setq use-package-always-ensure t)
