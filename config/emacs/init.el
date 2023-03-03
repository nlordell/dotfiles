(load "~/.config/emacs/sanemacs.el" nil t)
(load "~/.config/emacs/gtd.el" nil t)

;;; TODO(nlordell):
;;; 1. Setup `which-keys` for command discoverability
;;; 2. Move `eln-cache`, this requires `startup-redirect-eln-cache` which will
;;;    only become available in Emacs v29+
;;; 3. Try out `meow` modal editing mode
;;; 4. Move bookmarks to `~/.cache`
;;; 5. Use built-in tree-sitter support (requires Emacs v29+)

;;; Enable ligatures on macOS
(if (fboundp 'mac-auto-operator-composition-mode)
  (mac-auto-operator-composition-mode))
