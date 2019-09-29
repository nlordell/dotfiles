;;; init.el --- Emacs init file
;;; Commentary:
;;; Personal init file for Evil Rust development with some goodies

;;; Locals
;; TODO: fix these for Windows
(defun expand-data-file-name (name)
  "Convert filename NAME to an absolute and canonicalized path rooted in $XDG_DATA_HOME/emacs"
  (let* ((xdg-data-home        (or (getenv "XDG_DATA_HOME") "~/.local/share"))
         (emacs-data-directory (expand-file-name "emacs" xdg-data-home)))
    (expand-file-name name emacs-data-directory)))
(defun expand-cache-file-name (name)
  "Convert the filename NAME to an absolte and canonicalized path rooted in $XDG_CACHE_DIR/emacs"
  (let* ((xdg-cache-home       (or (getenv "XDG_CACHE_HOME") "~/.cache"))
         (emacs-data-directory (expand-file-name "emacs" xdg-cache-home)))
    (expand-file-name name emacs-data-directory)))

;;; Packaging
(setq
 package-user-dir           (expand-data-file-name "elpa/")
 package-archives           '(("melpa"          . "https://stable.melpa.org/packages/")
                              ("gnu"            . "https://elpa.gnu.org/packages/")
                              ("melpa-unstable" . "https://melpa.org/packages/"))
 package-archive-priorities '(("melpa"          . 2)
                              ("gnu"            . 1)
                              ("melpa-unstable" . 0)))
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
 auto-save-file-name-transforms `((".*" ,(expand-data-file-name "autosave/") t))
 recentf-save-file              (expand-cache-file-name "recentf"))

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

(use-package avy
  :bind ("C-:" . avy-goto-char))

(use-package evil
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  ;; TODO: right now this is causing use-package to load a bunch of stuff
  ;; prematurely which is sub-optimal
  (use-package evil-collection
    :config
    (evil-collection-init))
  (use-package evil-magit
    :after magit))

(use-package ivy
  :init
  (setq
   ivy-use-virtual-buffers      t
   enable-recursive-minibuffers t
   ivy-re-builders-alist        '((counsel-M-x . ivy--regex-fuzzy)
                                  (t           . ivy--regex-plus)))
  :config
  (ivy-mode 1)
  (use-package counsel
    :bind (("M-x"     . counsel-M-x)
           ("C-c c p" . counsel-fzf)
           ("C-c c r" . counsel-rg))
    :config
    (assq-delete-all 'counsel-M-x ivy-initial-inputs-alist)
    (use-package amx
      :init
      (setq amx-save-file (expand-cache-file-name "amx-items"))))
  (use-package swiper
    :bind ("C-s" . 'swiper)))

(use-package magit
  :bind (("C-x g s" . 'magit-status)
         ("C-x g l" . 'magit-log-all)))

(use-package neotree
  :bind ([f8] . 'neotree-toggle)
  :init
  (setq
   neo-autorefresh       nil
   neo-smart-open        nil
   neo-theme             'icons
   neo-window-fixed-size nil)
  :config
  (add-to-list 'window-size-change-functions
               (lambda (frame)
                 (let ((neo-window (neo-global--get-window)))
                   (unless (null neo-window)
                     (setq neo-window-width (window-width neo-window)))))))

(use-package projectile
  :bind-keymap ("C-c p" . projectile-command-map)
  :init
  (setq
   projectile-cache-file            (expand-cache-file-name "projectile.cache")
   projectile-completion-system     'ivy
   projectile-known-projects-file   (expand-cache-file-name "projectile-bookmarks.eld")
   projectile-switch-project-action 'neotree-projectile-action)
  :config
  (projectile-mode +1))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :config
  (which-key-mode))

;; Development
(use-package flycheck
  :pin melpa-unstable
  :hook (rust-mode . flycheck-mode)
  :config
  (use-package flycheck-pos-tip
    :pin melpa-unstable
    :hook (flycheck-mode . flycheck-pos-tip-mode)))

(use-package rust-mode
  :mode "\\.rs\\'"
  :init
  :config
  (use-package flycheck-rust
    :hook (flycheck-mode . flycheck-rust-setup)))

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
   circadian-themes   '((:sunrise . zenburn)
                        (:sunset  . zenburn)))
  :config
  (circadian-setup))

;; Miscellaneous
(use-package so-long
  :disabled ; seems to not be on ELPA yet
  :config
  (global-so-long-mode 1))

(use-package dashboard
  :init
  (setq
   dashboard-startup-banner 'logo
   dashboard-items          '((projects  . 5)
                              (recents   . 5)
                              (bookmarks . 5)
                              (agenda    . 5)))
  :config
  (evil-define-key 'normal dashboard-mode-map (kbd "j") 'dashboard-next-line)
  (evil-define-key 'normal dashboard-mode-map (kbd "k") 'dashboard-previous-line)
  (evil-define-key 'normal dashboard-mode-map (kbd "}") 'dashboard-next-section)
  (evil-define-key 'normal dashboard-mode-map (kbd "{") 'dashboard-previous-section)
  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))))

;; TODO:
;;;;
;; - org-*
;;   - org-agenda
;; - rust-mode
;;   - racer/cargo/ minor modes
;;   - company/eldoc integration
;;;;
;; - mode line
;; - evil goodies
;; - split init.el
