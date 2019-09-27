;;; init.el --- Emacs init file
;;; Commentary:
;;; Personal init file for Evil Rust development with some goodies

;;; Locals
(defun expand-data-file-name (name)
  "Convert filename NAME to an absolute and canonicalized path rooted in $XDG_DATA_HOME/emacs"
  ;; TODO: fix for Windows
  (let* ((xdg-data-home        (or (getenv "XDG_DATA_HOME") "~/.local/share"))
         (emacs-data-directory (expand-file-name "emacs" xdg-data-home)))
    (expand-file-name name emacs-data-directory)))

;;; Packaging
(setq
 package-user-dir           (expand-data-file-name "elpa/")
 package-archives           '(("melpa" . "https://stable.melpa.org/packages/")
                              ("gnu"   . "https://elpa.gnu.org/packages/"))
 package-archive-priorities '(("melpa" . 1)
                              ("gnu"   . 0)))
(package-initialize)

;;; Customization
(prefer-coding-system 'utf-8)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(show-paren-mode 1)

(setq-default
 indent-tabs-mode     nil
 tab-width            4
 display-line-numbers 'relative)

(setq
 mouse-yank-at-point     t
 inhibit-startup-message t
 initial-scratch-message nil)

;;; Runtime Files
(setq
 create-lockfiles                nil
 backup-directory-alist         `((".*" . ,(expand-data-file-name "backup/")))
 auto-save-list-file-prefix     (expand-data-file-name "autosave/.saves-")
 auto-save-file-name-transforms `((".*" ,(expand-data-file-name "autosave/") t)))

(setq custom-file (expand-data-file-name "custom.el"))
(load custom-file 'noerror)

;;; Packages
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; Interface
(use-package all-the-icons)

(use-package evil
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (use-package evil-collection
    :config
    (evil-collection-init)))

(use-package magit
  :bind ("C-x g" . 'magit-status))

(use-package neotree
  :bind ([f8] . 'neotree-toggle)
  :init
  (setq neo-theme 'icons))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :config
  (which-key-mode))

;; Theming
(use-package solarized-theme
  :defer t)
(use-package zenburn-theme
  :defer t)

(use-package circadian
  :init
  (setq
   calendar-latitude  52.5
   calendar-longitude 13.4
   circadian-themes   '((:sunrise . solarized-light)
                        (:sunset  . zenburn)))
  :config
  (circadian-setup))

;; Miscellaneous
(use-package so-long
  :disabled ; seems to not be on ELPA yet
  :config
  (global-so-long-mode 1))

;; TODO:
;;;;
;; - rust-mode
;; - ivy
;;   - counsel
;;   - swipper
;;   - avy
;; - org-*
;;   - org-agenda
;; - flycheck
;; - dashboard
;; - avy
;;;;
;; - fzf
;; - projectile
;; - ripgrep
;; - mode line
;; - evil goodies
;; - split init.el
