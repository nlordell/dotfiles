;;; init.el --- Emacs initialization -*- lexical-binding: t -*-

;;; Commentary:

;; Emacs initialization file. Contains my personal and evolving Emacs
;; configuration - what a rabbit hole!

;;; TODO:

;; 1. Emacs over TRAMP with `flatpak-spawn --host podman`
;;    <https://happihacking.com/blog/posts/2023/dev-containers-emacs/>
;; 2. Setup LLM coding: gptel, aidermacs
;; 3. Try out neocaml - tree-sitter based OCaml mode
;; 4. Take some stuff from Emacs NANO
;;    <https://github.com/rougier/nano-emacs>

;;; Code:

;;; -- Customizations --

(defcustom init/system-flavour 'plain
  "The flavour of system.

   Allows initialization and customization differentiation with a
   shared .emacs."
  :type '(choice (const :tag "Plain" plain)
                 (const :tag "Development" development))
  :group 'convenience)

;;; -- Utility Functions --

(defun init/expand-file-name (name)
  "Expands file NAME relative to the `user-emacs-directory'."
  (expand-file-name name user-emacs-directory))

(defun init/show-trailing-whitespace ()
  "Show trailing whitespace for the current buffer."
  (setq show-trailing-whitespace t))

(defun init/open-init ()
  "Opens the Emacs init.el configuration file."
  (interactive)
  (find-file (init/expand-file-name "init.el")))

(defun init/open-local-init ()
  "Opens the system-specific configuration file."
  (interactive)
  (find-file custom-file))

(defun init/dev ()
  "Returns whether or not this configuration is for the `development' flavour."
  (eq init/system-flavour 'development))

;;; -- Emacs Configuration --

;; Mostly adapted from `sanemacs.el', and ported to a `use-package'
;; section.

(use-package emacs
  :hook ((prog-mode . completion-preview-mode)
         (prog-mode . display-line-numbers-mode)
         (prog-mode . init/show-trailing-whitespace)
         (before-safe . delete-trailing-whitespace))
  :bind (("C-c ," . init/open-local-init)
         ("C-c C-," . init/open-init)
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
  (load-theme 'modus-vivendi)
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
  (create-lockfiles nil)
  (tab-width 4)
  (custom-file (init/expand-file-name "local-init.el")))

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

;; Some useful packages taken from `prelude.el' that just generally
;; enhance Emacs editing.

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
  :init
  (exec-path-from-shell-initialize))

(use-package undo-tree
  :ensure t
  :init
  (global-undo-tree-mode +1)
  :custom
  (undo-tree-auto-save-history nil))

;;; -- LLM --

(use-package copilot
  :ensure t
  :bind (("C-<tab>" . copilot-accept-completion)
         ("C-M-<tab>" . copilot-accept-completion-by-word)
         ("C-c a a" . copilot-mode)
         ("C-c a n" . copilot-next-completion)
         ("C-c a p" . copilot-previous-completion)))

;;; -- General Development --

(use-package ligature
  :if (init/dev)
  :ensure t
  :hook
  (prog-mode . ligature-mode)
  :config
  (ligature-set-ligatures
   'prog-mode
   '(".." "..." "::" ":=" ";;" ";;;" "??" "**" "/*" "*/" "/**" "<-" "->" "-->"
     "<!--" "<=" "=>" ">=" "<<" ">>" "<>" "<|" "|>" "</" "/>" "</>" "#(" "#{"
     "#[" "#!" "##" "###" "####" "[|" "|]" "[<" ">]" "{|" "|}" "{{" "}}" "//"
     "///" "&&" "++" "||" "==" "===" "=~" "~-" "__" "!=" "!==" "--" "---")))

(use-package magit
  :if (init/dev)
  :ensure t
  :bind (("C-x g" . magit-status)))

(use-package projectile
  :if (init/dev)
  :ensure t
  :bind-keymap ("C-c p" . projectile-command-map)
  :custom
  (projectile-project-search-path '(("~/Developer/" . 2)))
  :config
  (projectile-mode +1))

(use-package rg
  :if (init/dev)
  :ensure t)

;;; -- Miscellaneous --

(use-package markdown-mode
  :if (init/dev)
  :ensure t
  :hook
  (gfm-mode . flyspell-mode)
  (gfm-mode . visual-line-mode)
  :init
  (add-to-list 'major-mode-remap-alist '(markdown-mode . gfm-mode)))

(use-package dockerfile-mode
  :if (init/dev)
  :ensure t)

;;; -- OCaml --

(use-package tuareg
  :if (init/dev)
  :ensure t)

(use-package ocaml-eglot
  :if (init/dev)
  :ensure t
  :after (tuareg)
  :bind (("M-q" . eglot-format))
  :hook
  (tuareg-mode . ocaml-eglot)
  (ocaml-eglot . eglot-ensure))

;;; -- Rust --

(use-package rust-ts-mode
  :if (init/dev)
  :mode "\\.rs\\'"
  :bind (("M-q" . eglot-format))
  :hook
  (rust-ts-mode . eglot-ensure))

(provide 'init)
;;; init.el ends here.
