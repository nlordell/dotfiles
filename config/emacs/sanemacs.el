;;;
;;; Sanemacs <https://sanemacs.com>
;;;

;;; Customize Emacs
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(column-number-mode)
(fset 'yes-or-no-p 'y-or-n-p)
(setq
  inhibit-startup-screen t
  initial-scratch-message ""
  sentence-end-double-space nil)
(setq-default
  cursor-type 'bar
  show-trailing-whitespace t
  tab-width 4)

;;; Write auto-saves and backups to `.cache` directory
(setq
   auto-save-list-file-prefix "~/.cache/emacs/auto-save/"
   auto-save-file-name-transforms '((".*" "~/.cache/emacs/auto-save/" t))
   backup-by-copying t
   backup-directory-alist '((".*" . "~/.cache/emacs/backup/"))
   create-lockfiles nil)

;;; Setup `package.el`
(require 'package)
(setq
  package-enable-at-startup nil
  package-user-dir "~/.cache/emacs/elpa/"
  package-gnupghome-dir "~/.cache/emacs/elpa/gnupg")
(add-to-list 'package-archives
  '("melpa" . "https://melpa.org/packages/"))
(unless package--initialized (package-initialize))

;;; Setup `use-package`
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))
(setq use-package-always-ensure t)

;;; Setup Modus theme
(load-theme 'modus-vivendi)

;;; Use separate file for custom variables
(setq custom-file "~/.cache/emacs/custom.el")
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(load custom-file nil t)
