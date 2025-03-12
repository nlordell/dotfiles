(load "~/.config/emacs/sanemacs.el" nil t)
(load "~/.config/emacs/ligatures.el" nil t)
(load "~/.config/emacs/meow.el" nil t)

(use-package ligature
  :ensure t
  :config
  (ligature-set-ligatures 'prog-mode ligatures-berkeley-mono)
  (global-ligature-mode t))

(use-package magit
  :ensure t)

(use-package meow
  :ensure t
  :config
  (meow-setup-qwerty)
  (meow-global-mode 1))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;;; OCaml
(use-package tuareg
  :ensure t)
(use-package ocaml-eglot
  :ensure t
  :after tuareg
  :hook
  (tuareg-mode . ocaml-eglot)
  (ocaml-eglot . eglot-ensure))

;;; TODO(nlordell):
;;; 1. Update remaining init scripts:
;;;        (load "~/.config/emacs/gtd.el" nil t)
;;; 2. Emacs over devcontainers/docker with TRAMP
;;;    <https://happihacking.com/blog/posts/2023/dev-containers-emacs/>
;;; 3. Investigate other potential sane Emacs defaults
;;;    <https://idiomdrottning.org/bad-emacs-defaults>

;;; Make the shell environement available when started on macOS Spotlight.
(when (memq window-system '(mac ns))
  (use-package exec-path-from-shell
    :config
    (exec-path-from-shell-initialize)))
