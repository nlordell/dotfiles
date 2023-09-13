(load "~/.config/emacs/sanemacs.el" nil t)
(load "~/.config/emacs/gtd.el" nil t)
(load "~/.config/emacs/macos.el" nil t)

(use-package which-key
  :ensure t
  :config (which-key-mode))

;;; TODO(nlordell):
;;; 1. Try out `meow` modal editing mode
;;; 2. Use built-in tree-sitter support (v29)
;;; 3. Simplify GTD.el to match what I do in ~/WEEK.md
;;; 4. Emacs over devcontainers/docker with TRAMP
;;;    <https://happihacking.com/blog/posts/2023/dev-containers-emacs/>
