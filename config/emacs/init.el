;;; -*- lexical-binding: t -*-

;;; TODO(nlordell):
;; 1. Emacs over TRAMP with `flatpak-spawn --host podman`
;;    <https://happihacking.com/blog/posts/2023/dev-containers-emacs/>
;; 2. Setup LLM coding: gptel, aidermacs
;; 3. Try out neocaml - tree-sitter based OCaml mode
;; 4. Take some stuff from Emacs NANO
;;    <https://github.com/rougier/nano-emacs>

(defcustom system-flavour 'plain
  "The flavour of system.

   Allows initialization and customization differentiation with a
   shared .emacs."
  :type '(choice (const :tag "Plain" plain)
                 (const :tag "Development" development))
  :group 'convenience)

(defun init/load (name &rest kargs)
  "Loads an init file."
  (let* ((init-directory (file-name-directory (or load-file-name (buffer-file-name))))
         (file-name (file-name-concat init-directory name))
         (is-optional (plist-get kargs :optional))
         (should-load (or (not is-optional) (file-exists-p file-name))))
    (when should-load
      (load file-name nil t))))

(init/load "sanemacs.el")
(when (eq system-flavour 'development)
  (init/load "flavours/development.el"))
(when (eq system-type 'darwin)
  (init/load "flavours/macos.el"))
(init/load "local-init.el" :optional t)
