;;; init.el --- Emacs initialization -*- lexical-binding: t -*-

;;; Commentary:

;; Emacs initialization file. Contains my personal and evolving Emacs
;; configuration - what a rabbit hole!

;;; Code:

;;; -- Customizations --

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
  (setq-local show-trailing-whitespace t))

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

(defun devbox/magit-status ()
  "Opens the local Git status for a devbox directory.

If the current buffer is not open in the devbox, then this function just runs
`magit-status' normally."
  (interactive)
  (if (equal (file-remote-p default-directory 'host) "devbox")
      (let ((local-dir (tramp-file-name-localname
                        (tramp-dissect-file-name default-directory))))
        (magit-status-setup-buffer local-dir))
    (magit-status)))

;;; -- Emacs Configuration --

(use-package emacs
  :hook ((prog-mode . display-line-numbers-mode)
         (prog-mode . init/show-trailing-whitespace)
         (before-save . delete-trailing-whitespace))
  :bind (("C-c ," . init/open-local-init)
         ("C-c C-," . init/open-init)
         ("C-c C-j" . init/open-journal)
         ("C-x k" . kill-current-buffer)
         ("M-/" . hippie-expand))
  :custom
  (use-short-answers t)
  (inhibit-startup-screen t)
  (initial-scratch-message "")
  (initial-major-mode 'fundamental-mode)
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
  (tab-always-indent 'complete)
  (completion-styles '(basic initials substring))
  (auto-revert-avoid-polling t)
  (auto-revert-remote-files t)
  (major-mode-remap-alist
   '((c-mode . c-ts-mode)
     (javascript-mode . js-ts-mode)
     (typescript-mode . typescript-ts-mode)))
  (custom-file (init/expand-file-name "local-init.el"))
  :init
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (when (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))
  (which-key-mode +1)
  (savehist-mode +1)
  (column-number-mode +1)
  (delete-selection-mode +1)
  (global-auto-revert-mode +1)
  (load-theme 'modus-vivendi t)
  (load (init/expand-file-name "local-init.el") t t)
  (setq gc-cons-threshold (or init/gc-cons-threshold 800001)))

;;; -- Built-ins --

(use-package completion-preview
  :hook ((prog-mode . completion-preview-mode))
  :bind (:map completion-preview-active-mode-map
         ("M-n" . completion-preview-next-candidate)
         ("M-p" . completion-preview-prev-candidate)))

(use-package eglot
  :bind (:map eglot-mode-map
         ("M-q" . eglot-format))
  :config
  (fset #'jsonrpc--log-event #'ignore))

(use-package project
  :custom
  (project-mode-line t)
  :config
  (add-to-list 'project-switch-commands
               '(devbox/magit-status "Magit" "m") t))

(use-package tramp
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

(use-package xref
  :config
  (when (executable-find "grep")
    (setopt xref-search-program 'ripgrep)))

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
         ("C-'" . avy-goto-char-timer)
         ("M-g f" . avy-goto-line)))

(use-package corfu
  :ensure t
  :hook ((prog-mode . corfu-mode)
         (corfu-mode . corfu-popupinfo-mode))
  :bind (:map corfu-map
         ("SPC" . corfu-insert-separator)
         ("C-n" . corfu-next)
         ("C-p" . corfu-previous))
  :custom
  (corfu-cycle t)
  (corfu-popupinfo-delay '(0.25 . 0.1))
  (corfu-popupinfo-hide nil))

(use-package exec-path-from-shell
  :if (eq system-type 'darwin)
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode +1))

(use-package undo-tree
  :ensure t
  :custom
  (undo-tree-auto-save-history nil)
  :config
  (global-undo-tree-mode +1))

(use-package spacious-padding
  :ensure t
  :custom
  (spacious-padding-subtle-mode-line t)
  :config
  (spacious-padding-mode +1))

(use-package vertico
  :ensure t
  :config
  (vertico-mode +1))

;;; -- General Development --

(use-package ligature
  :ensure t
  :hook ((prog-mode . ligature-mode))
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
  :bind (("C-x g" . devbox/magit-status)))

;;; -- OCaml --

(use-package tuareg
  :ensure t
  :mode (("\\.mli?\\'" . tuareg-mode)
         ("\\.opam\\'" . tuareg-opam-mode)
         ("\\.mly\\'" . tuareg-menhir-mode)))

(use-package ocaml-eglot
  :ensure t
  :after (tuareg)
  :hook ((tuareg-mode . ocaml-eglot)
         (ocaml-eglot . eglot-ensure)))

;;; -- Rust --

(use-package rust-ts-mode
  :mode "\\.rs\\'"
  :hook ((rust-ts-mode . eglot-ensure)))

;;; -- Solidity --

(if (file-directory-p "~/Developer/nlordell/sol-mode")
    (use-package sol-mode
      :load-path "~/Developer/nlordell/sol-mode"
      :mode "\\.sol\\'")
  (use-package sol-mode
    :ensure t
    :mode "\\.sol\\'"))

;;; -- Miscellaneous --

(use-package dockerfile-mode
  :ensure t
  :mode "[/\\]Dockerfile")

(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . gfm-mode)
  :hook ((gfm-mode . flyspell-mode)
         (gfm-mode . visual-line-mode)
         (gfm-mode . init/show-trailing-whitespace)))

(provide 'init)
;;; init.el ends here.
