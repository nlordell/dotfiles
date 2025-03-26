;;; -*- lexical-binding: t -*-

;;; TODO(nlordell):
;; 1. Emacs over devcontainers/docker with TRAMP
;;    <https://happihacking.com/blog/posts/2023/dev-containers-emacs/>
;; 2. Setup LLM coding: gptel, aidermacs
;; 3. Try out neocaml - tree-sitter based

(load "~/.config/emacs/sanemacs.el" nil t)

(defcustom system-category 'plain
  "The category of system.

   Allows initialization and customization differentiation with a
   shared .emacs."
  :type '(choice (const :tag "Plain" plain)
				 (const :tag "Development" development))
  :group 'convenience)

(when (eq system-category 'development)
  (load "~/.config/emacs/config/development.el" nil t))
(when (eq system-type 'darwin)
  (load "~/.config/emacs/config/macos.el" nil t))
