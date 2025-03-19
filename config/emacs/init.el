;;; -*- lexical-binding: t -*-

;;; TODO(nlordell):
;; 1. Update remaining init scripts:
;;        (load "~/.config/emacs/gtd.el" nil t)
;;        (load "~/.config/emacs/tree-sitter.el" nil t)
;; 2. Emacs over devcontainers/docker with TRAMP
;;    <https://happihacking.com/blog/posts/2023/dev-containers-emacs/>
;; 3. Investigate other potential sane Emacs defaults
;;    <https://idiomdrottning.org/bad-emacs-defaults>
;; 4. Setup GPTel - should be possible without FastGPT credits

(load "~/.config/emacs/sanemacs.el" nil t)
(load "~/.config/emacs/ligatures.el" nil t)

;;;
;;; General
;;;

(use-package ace-window
  :bind
  (("C-x o" . ace-window)))

(use-package avy
  :bind
  (("C-:" . avy-goto-char)
   ("M-g f" . avy-goto-line)))

(use-package ligature
  :config
  (ligature-set-ligatures 'prog-mode ligatures-berkeley-mono)
  (global-ligature-mode +1))

(use-package magit
  :custom
  (transient-levels-file "~/.cache/emacs/transient/levels.el")
  (transient-values-file "~/.cache/emacs/transient/values.el")
  (transient-history-file "~/.cache/emacs/transient/history.el"))

(use-package projectile
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :custom
  (projectile-known-projects-file "~/.cache/emacs/projectile-bookmarks.eld")
  (projectile-project-search-path '(("~/Developer/" . 2)))
  :config
  (projectile-mode +1))

(use-package undo-tree
  :custom
  (undo-tree-auto-save-history nil)
  :config
  (global-undo-tree-mode +1))

(use-package which-key
  :config
  (which-key-mode +1))

;;;
;;; OCaml
;;;

(use-package tuareg)

(use-package ocaml-eglot
  :after (tuareg)
  :hook
  (tuareg-mode . ocaml-eglot)
  (ocaml-eglot . eglot-ensure))

;;;
;;; macOS
;;;

(when (memq window-system '(mac ns))
  ;; Make the shell environement available when started on macOS Spotlight.
  (use-package exec-path-from-shell
    :config
    (exec-path-from-shell-initialize)))
