;;; Locals
(defconst user-data-directory (expand-file-name "data" user-emacs-directory)
  "Root directory for untracked user Emacs data")
(defun expand-data-file-name (name)
  "Convert filename NAME to an absolute and canonicalized path rooted in the variable `user-data-directory'"
  (expand-file-name name user-data-directory))

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
