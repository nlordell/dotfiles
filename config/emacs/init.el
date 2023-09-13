(load "~/.config/emacs/sanemacs.el" nil t)
(load "~/.config/emacs/gtd.el" nil t)

;;; TODO(nlordell):
;;; 1. Setup `which-keys` for command discoverability
;;; 2. Move `eln-cache` using `startup-redirect-eln-cache` (v29)
;;; 3. Try out `meow` modal editing mode
;;; 4. Move bookmarks to `~/.cache` (v29)
;;; 5. Use built-in tree-sitter support (v29)
;;; 6. use built-in use-package package (v29)
;;; 7. Simplify GTD.el to match what I do in ~/WEEK.md
;;; 8. Setup dev-container support for Emacs
;;; 9. Emacs over devcontainers with TRAMP
;;;    <https://happihacking.com/blog/posts/2023/dev-containers-emacs/>

;;; Enable ligatures on macOS
(if (fboundp 'mac-auto-operator-composition-mode)
  (mac-auto-operator-composition-mode))
