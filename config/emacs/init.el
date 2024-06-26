(load "~/.config/emacs/sanemacs.el" nil t)
;;(load "~/.config/emacs/gtd.el" nil t)

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;;; TODO(nlordell):
;;; 1. Try out `meow` modal editing mode
;;; 2. Use built-in tree-sitter support (v29)
;;; 3. Simplify GTD.el to match what I do in Journal.md
;;; 4. Emacs over devcontainers/docker with TRAMP
;;;    <https://happihacking.com/blog/posts/2023/dev-containers-emacs/>
;;; 5. Investigate other potential sane Emacs defaults
;;;    <https://idiomdrottning.org/bad-emacs-defaults>
;;; 6. Install and learn magit
;;;    <https://www.youtube.com/watch?app=desktop&v=2-0OwGTt0dI>
;;; 7. Matering Emacs <https://www.masteringemacs.org/>

;;; Enable ligatures on macOS
(if (fboundp 'mac-auto-operator-composition-mode)
  (mac-auto-operator-composition-mode))
