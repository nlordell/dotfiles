;;;
;;; Customized Sanemacs <https://sanemacs.com>
;;;

;;; Customize Emacs
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(column-number-mode)
(fset 'yes-or-no-p 'y-or-n-p)
(setq
  inhibit-startup-echo-area-message (user-login-name)
  inhibit-startup-screen t
  initial-major-mode 'fundamental-mode
  initial-scratch-message ""
  sentence-end-double-space nil)
(setq-default
  cursor-type 'bar
  show-trailing-whitespace t
  tab-width 4)

;;; Write auto-saves, backups and bookmarks to `.cache` directory
(setq
   auto-save-list-file-prefix "~/.cache/emacs/auto-save/"
   auto-save-file-name-transforms '((".*" "~/.cache/emacs/auto-save/" t))
   backup-by-copying t
   backup-directory-alist '((".*" . "~/.cache/emacs/backup/"))
   bookmark-default-file "~/.cache/emacs/bookmarks"
   create-lockfiles nil)

;;; Configure TRAMP
(with-eval-after-load 'tramp-cache
  (setq tramp-persistency-file-name "~/.cache/emacs/tramp"))

;;; Setup Modus theme
(load-theme 'modus-vivendi)

;;; Use separate file for custom variables
(setq custom-file "~/.cache/emacs/custom.el")
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(load custom-file nil t)
