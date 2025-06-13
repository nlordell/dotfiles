;;; init.el --- Emacs initialization -*- lexical-binding: t -*-

;;; Commentary:

;; Emacs initialization file. Contains my personal and evolving Emacs
;; configuration - what a rabbit hole!

;;; TODO:

;; 1. Setup LLM coding: gptel, aidermacs
;; 2. Try out neocaml - tree-sitter based OCaml mode
;; 3. Take some stuff from Emacs NANO
;;    <https://github.com/rougier/nano-emacs>

;;; Code:

;;; -- Customizations --

(defcustom init/system-flavour 'plain
  "The flavour of system.

   Allows initialization and customization differentiation with a
   shared .emacs."
  :tag "System Flavour"
  :type '(choice (const :tag "Plain" plain)
                 (const :tag "Development" development))
  :group 'convenience)

(defcustom init/journal-file "~/Documents/Journal.org"
  "The path to my journal file."
  :tag "Journal File"
  :type '(string)
  :group 'convenience)

;;; -- Utility Functions --

(defun init/expand-file-name (name)
  "Expands file NAME relative to the `user-emacs-directory'."
  (expand-file-name name user-emacs-directory))

(defun init/show-trailing-whitespace ()
  "Show trailing whitespace for the current buffer."
  (setq show-trailing-whitespace t))

(defun init/auto-save-directory ()
  "The auto-save directory where the save file lives."
  (file-name-directory (concat auto-save-list-file-prefix "1-localhost")))

(defun init/open-init ()
  "Opens the Emacs init.el configuration file."
  (interactive)
  (find-file (init/expand-file-name "init.el")))

(defun init/open-local-init ()
  "Opens the system-specific configuration file."
  (interactive)
  (find-file custom-file))

(defun init/open-journal ()
  "Opens my journal."
  (interactive)
  (find-file init/journal-file))

(defun init/dev ()
  "Returns whether or not this configuration is for the `development' flavour."
  (eq init/system-flavour 'development))

;;; -- Emacs Configuration --

(use-package emacs
  :hook ((prog-mode . completion-preview-mode)
         (prog-mode . display-line-numbers-mode)
         (prog-mode . init/show-trailing-whitespace)
         (before-save . delete-trailing-whitespace))
  :bind (("C-c ," . init/open-local-init)
         ("C-c C-," . init/open-init)
         ("C-c C-j" . init/open-journal)
         ("C-x k" . kill-current-buffer)
         ("M-/" . hippie-expand))
  :init
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (which-key-mode +1)
  (column-number-mode +1)
  (delete-selection-mode +1)
  (global-auto-revert-mode +1)
  (load (init/expand-file-name "local-init.el") t t)
  :custom
  (use-short-answers t)
  (inhibit-startup-screen t)
  (initial-scratch-message "")
  (frame-title-format '("%b"))
  (ring-bell-function 'ignore)
  (sentence-end-double-space nil)
  (cursor-type 'bar)
  (indent-tabs-mode nil)
  (tab-width 2)
  (backup-by-copying t)
  (backup-directory-alist `((".*" . ,(init/expand-file-name "backup"))))
  (auto-save-file-name-transforms `((".*" ,(init/auto-save-directory) t)))
  (create-lockfiles nil)
  (tab-width 4)
  (custom-file (init/expand-file-name "local-init.el")))

;;; -- Built-ins --

(use-package c-ts-mode
  :mode "\\.[ch]\\'")

(use-package eglot
  :bind (:map eglot-mode-map
         ("M-q" . eglot-format)))

(use-package js-ts-mode
  :mode "\\.m?js\\'"
  :custom
  (js-indent-level 2))

(use-package modus-themes
  :config
  (load-theme 'modus-vivendi t))

(use-package rust-ts-mode
  :mode "\\.rs\\'"
  :hook
  (rust-ts-mode . eglot-ensure))

(use-package typescript-ts-mode
  :mode "\\.tsx?\\'"
  :custom
  (typescript-ts-mode-indent-offset 2))

(use-package xref
  :config
  (when (executable-find "rg")
    (setq xref-search-program 'ripgrep)))

;;; -- Packages --

;; Enable MELPA, but make it a lower priority to GNU and non-GNU ELPA
;; repositories. This makes it so we fallback to MELPA when there are
;; no stable versions on the official ELPA repositories.

(use-package package
  :custom
  (package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                      ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                      ("melpa" . "https://melpa.org/packages/")))
  (package-archive-priorities '(("gnu" . 1)
                                ("nongnu" . 1)
                                ("melpa" . 0))))

;;; -- Prelude --

(use-package ace-window
  :ensure t
  :bind (("C-x o" . ace-window)))

(use-package avy
  :ensure t
  :bind (("C-:" . avy-goto-char)
         ("M-g f" . avy-goto-line)))

(use-package exec-path-from-shell
  :if (eq system-type 'darwin)
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package undo-tree
  :ensure t
  :custom
  (undo-tree-auto-save-history nil)
  :config
  (global-undo-tree-mode +1))

;;; -- LLM --

(when (init/dev)
  (use-package copilot
    :ensure t
    :bind (("C-<tab>" . copilot-accept-completion)
           ("C-M-<tab>" . copilot-accept-completion-by-word)
           ("C-c a a" . copilot-mode)
           ("C-c a n" . copilot-next-completion)
           ("C-c a p" . copilot-previous-completion))))

;;; -- General Development --

(when (init/dev)
  (use-package ligature
    :ensure t
    :hook
    (prog-mode . ligature-mode)
    :config
    (ligature-set-ligatures
     'prog-mode
     '(".." "..." "::" ":=" ";;" ";;;" "??" "**" "/*" "*/" "/**" "<-"
       "->" "-->" "<!--" "<=" "=>" ">=" "<<" ">>" "<>" "<|" "|>" "</"
       "/>" "</>" "#(" "#{" "#[" "#!" "##" "###" "####" "[|" "|]" "[<"
       ">]" "{|" "|}" "{{" "}}" "//" "///" "&&" "++" "||" "==" "==="
       "=~" "~-" "__" "!=" "!==" "--" "---")))

  (use-package magit
    :ensure t
    :bind (("C-x g" . magit-status))))

;;; -- OCaml --

(when (init/dev)
  (use-package tuareg
    :ensure t
    :mode (("\\.mli?\\'" . tuareg-mode)
           ("\\.opam\\'" . tuareg-opam-mode)
           ("\\.mly\\'" . tuareg-menhir-mode)))

  (use-package ocaml-eglot
    :ensure t
    :after (tuareg)
    :hook
    (tuareg-mode . ocaml-eglot)
    (ocaml-eglot . eglot-ensure)))

;;; -- Solidity --

(when (init/dev)
  (if (file-directory-p "~/Developer/nlordell/sol-mode")
      (use-package sol-mode
        :load-path "~/Developer/nlordell/sol-mode"
        :mode "\\.sol\\'")
    (use-package sol-mode
      :ensure t
      :mode "\\.sol\\'")))

;;; -- Miscellaneous --

(when (init/dev)
  (use-package dockerfile-mode
    :ensure t
    :mode "[/\\]Dockerfile")

  (use-package markdown-mode
    :ensure t
    :mode ("\\.md\\'" . gfm-mode)
    :hook
    (gfm-mode . flyspell-mode)
    (gfm-mode . visual-line-mode)
    (gfm-mode . init/show-trailing-whitespace)))

(provide 'init)
;;; init.el ends here.
