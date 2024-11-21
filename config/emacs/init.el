(load "~/.config/emacs/sanemacs.el" nil t)
(load "~/.config/emacs/meow.el" nil t)
;;(load "~/.config/emacs/gtd.el" nil t)
;;(load "~/.config/emacs/ligatures.el" nil t)

(use-package magit
  :ensure t)

(use-package meow
  :ensure t
  :config
  (meow-setup)
  (meow-global-mode 1))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;;; TODO(nlordell):
;;; 1. Use built-in tree-sitter support (v29)
;;; 2. Simplify GTD.el to match what I do in Journal.md
;;; 3. Emacs over devcontainers/docker with TRAMP
;;;    <https://happihacking.com/blog/posts/2023/dev-containers-emacs/>
;;; 4. Investigate other potential sane Emacs defaults
;;;    <https://idiomdrottning.org/bad-emacs-defaults>
;;; 5. Matering Emacs <https://www.masteringemacs.org/>

;;; Enable ligatures on macOS
(if (fboundp 'mac-auto-operator-composition-mode)
  (mac-auto-operator-composition-mode))
