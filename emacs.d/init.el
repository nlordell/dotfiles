;;; package --- Init
;;; Commentary:
;;; Personal init file mostly tailored towards Rust development

;;; Locals
(defun expand-data-file-name (name)
  "Convert filename NAME to an absolute and canonicalized path rooted in $XDG_DATA_HOME/emacs"
  ;; TODO: fix for Windows
  (let* ((xdg-data-home        (or (getenv "XDG_DATA_HOME") "~/.local/share"))
         (emacs-data-directory (expand-file-name "emacs" xdg-data-home)))
    (expand-file-name name emacs-data-directory)))

;;; Customization
(prefer-coding-system 'utf-8)
(menu-bar-mode -1)
(tool-bar-mode -1)
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

;;; Packaging
(setq
 package-user-dir           (expand-data-file-name "elpa/")
 package-archives           '(("melpa" . "https://stable.melpa.org/packages/")
                              ("gnu"   . "https://elpa.gnu.org/packages/"))
 package-archive-priorities '(("melpa" . 1)
                              ("gnu"   . 0)))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;;; Packages
(use-package evil
  :config
  (evil-mode 1))

(use-package which-key
  :config
  (which-key-mode))

;;; Theming
(use-package doom-themes
  :defer t)
(use-package circadian
  :config
  (setq
   calendar-latitude  52.5
   calendar-longitude 13.4
   circadian-themes   '((:sunrise . doom-nord-light)
                        (:sunset  . doom-one)))
  (circadian-setup))

;; TODO:
;; - all-the-icons
;; - hide-mode-line
;; - neotree
;; - dtrt-indent
;; - smartparens
;; - so-long
;;;;
;; - rust-mode
;; - ivy
;;   - counsel
;;   - swipper
;;   - avy
;; - magit
;; - org-*
;;   - org-agenda
;; - flycheck
;; - dashboard
;; - avy
;;;;
;; - fzf
;; - ripgrep
;; - mode line
;; - evil goodies
;; - split init.el
