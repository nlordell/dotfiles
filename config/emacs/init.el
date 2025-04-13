;;; -*- lexical-binding: t -*-

;;; TODO(nlordell):
;; 1. Emacs over devcontainers/docker with TRAMP
;;    <https://happihacking.com/blog/posts/2023/dev-containers-emacs/>
;; 2. Setup LLM coding: gptel, aidermacs
;; 3. Try out neocaml - tree-sitter based OCaml mode
;; 4. Take some stuff from Emacs NANO
;;    <https://github.com/rougier/nano-emacs>

(load "~/.config/emacs/sanemacs.el" nil t)

(defcustom system-flavour 'plain
  "The flavour of system.

   Allows initialization and customization differentiation with a
   shared .emacs."
  :type '(choice (const :tag "Plain" plain)
                 (const :tag "Development" development))
  :group 'convenience)

(when (eq system-flavour 'development)
  (load "~/.config/emacs/flavours/development.el" nil t))
(when (eq system-type 'darwin)
  (load "~/.config/emacs/flavours/macos.el" nil t))

(let ((local-init "~/.config/emacs/local-init.el"))
  (when (file-exists-p local-init)
    (load local-init nil t)))
