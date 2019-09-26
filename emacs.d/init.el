;;; package --- Init
;;; Commentary:
;;; Personal init file mostly tailored towards Rust development

;;; Locals
(defun expand-data-file-name (name)
  "Convert filename NAME to an absolute and canonicalized path rooted in $XDG_DATA_HOME/emacs"
  ;; TODO(nlordell): fix for Windows
  (let* ((xdg-data-home        (or (getenv "XDG_DATA_HOME")
                                   (expand-file-name ".local/share" (getenv "HOME"))))
         (emacs-data-directory (expand-file-name "emacs" xdg-data-home)))
    (expand-file-name name emacs-data-directory)))

;;; Customization
(menu-bar-mode -1)
(tool-bar-mode -1)
(show-paren-mode 1)

(setq-default indent-tabs-mode nil)

(setq
 create-lockfiles                   nil
 backup-directory-alist            `((".*" . ,(expand-data-file-name "backup/")))
 auto-save-list-file-prefix        (expand-data-file-name "autosave/.saves-")
 auto-save-file-name-transforms    `((".*" ,(expand-data-file-name "autosave/") t)))

(setq custom-file (expand-data-file-name "custom.el"))
(load custom-file 'noerror)

(setq
 inhibit-startup-message    t
 initial-scratch-message    nil)

;;; Packages
(setq
 package-user-dir    (expand-data-file-name "elpa/")
 package-archives    '(("gnu" . "https://elpa.gnu.org/packages/")
                       ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

(use-package evil
  :config
  (evil-mode 1))
