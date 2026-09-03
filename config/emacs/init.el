;;; init.el --- Emacs initialization -*- lexical-binding: t -*-

;;; Commentary:

;; Emacs initialization file. Contains my personal and evolving Emacs
;; configuration - what a rabbit hole!

;;; Code:

;;; -- Customizations --

(defcustom my/journal-file "~/Documents/Journal.org"
  "The path to my journal file."
  :tag "Journal File"
  :type '(string)
  :group 'convenience)

;;; -- Utility Functions --

(defun my/expand-file-name (name)
  "Expands file NAME relative to the `user-emacs-directory'."
  (expand-file-name name user-emacs-directory))

(defun my/show-trailing-whitespace ()
  "Show trailing whitespace for the current buffer."
  (setq-local show-trailing-whitespace t))

(defun my/auto-save-directory ()
  "The auto-save directory where the save file lives."
  (file-name-directory (concat auto-save-list-file-prefix "1-localhost")))

(defun my/open-init ()
  "Opens the Emacs init.el configuration file."
  (interactive)
  (find-file (my/expand-file-name "init.el")))

(defun my/open-local-init ()
  "Opens the system-specific configuration file."
  (interactive)
  (find-file custom-file))

(defun my/open-journal ()
  "Opens my journal."
  (interactive)
  (find-file my/journal-file))

(defun my/vc-spin-off-branch (new-branch)
  "Create and switch to a new branch from the current branch."
  (interactive "sSpin-off new branch: ")
  (let* ((default-directory (vc-root-dir))
         (current-branch (string-trim
                          (vc-git--run-command-string
                           nil "branch" "--show-current")))
         (upstream (or (ignore-errors
                         (string-trim
                          (vc-git--run-command-string
                           nil "rev-parse" "--abbrev-ref" "@{upstream}")))
                       "")))
    (when (string-empty-p new-branch)
      (user-error "Branch name cannot be empty"))
    (when (string-prefix-p "-" new-branch)
      (user-error "Branch name cannot start with '-'"))
    (vc-git-command nil 0 nil "checkout" "-b" new-branch)
    (if (or (string-empty-p current-branch) (string-empty-p upstream))
        (message "Spun off %s from %s" new-branch
                 (if (string-empty-p current-branch) "HEAD" current-branch))
      (condition-case err
          (let ((merge-base (string-trim
                             (vc-git--run-command-string
                              nil "merge-base" current-branch upstream))))
            (when (not (string-empty-p merge-base))
              (vc-git-command nil 0 nil "branch" "--force"
                              current-branch merge-base)
              (message "Spun off %s from %s; reset %s to merge-base with %s"
                       new-branch current-branch current-branch upstream)))
        (error (message "Spun off %s from %s (could not reset %s: %s)"
                        new-branch current-branch current-branch
                        (error-message-string err)))))
    (revert-buffer)))

;(defun my/vc-push-set-upstream ()
;  "Push the current branch to origin and set it as upstream."
;  (interactive)
;  (let* ((default-directory (vc-root-dir))
;         (branch (string-trim
;                  (vc-git--run-command-string
;                   nil "branch" "--show-current"))))
;    (when (string-empty-p branch)
;      (user-error "Not on a branch"))
;    (vc-git-command "*vc-git: push*" 'async nil "push" "-u" "origin" branch)
;    (pop-to-buffer "*vc-git: push*")
;    (message "Pushed %s to upstream origin/%s" branch branch)
;    (revert-buffer)))

(defun my/vc-push-set-upstream ()
  "Push the current branch to origin and set it as upstream, asynchronously."
  (interactive)
  (let* ((default-directory (vc-root-dir))
         (branch (string-trim
                  (vc-git--run-command-string
                   nil "branch" "--show-current")))
         (buffer-name "*vc-git: push*")
         (vc-dir-buffer (current-buffer)))
    (when (string-empty-p branch)
      (user-error "Not on a branch"))
    (let ((proc (vc-git-command
                 buffer-name 'async nil "push" "-u" "origin" branch)))
      (when (processp proc)
        (process-put proc 'my-branch branch)
        (process-put proc 'my-vc-dir-buffer vc-dir-buffer)
        (set-process-sentinel
         proc
         (lambda (proc event)
           (when (memq (process-status proc) '(exit signal))
             (let ((status (process-exit-status proc))
                   (branch (process-get proc 'my-branch))
                   (vc-dir-buffer (process-get proc 'my-vc-dir-buffer)))
               (if (eq status 0)
                   (progn
                     (message "Pushed %s to upstream origin/%s" branch branch)
                     (when (buffer-live-p vc-dir-buffer)
                       (with-current-buffer vc-dir-buffer
                         (revert-buffer t t))))
                 (message "Push failed for %s (see %s)"
                          branch (process-buffer proc))))))))
      (pop-to-buffer buffer-name))))

;;; -- Emacs Configuration --

(use-package emacs
  :hook ((before-save . delete-trailing-whitespace)
         (prog-mode . display-line-numbers-mode)
         (prog-mode . my/show-trailing-whitespace))
  :bind (("C-c ," . my/open-local-init)
         ("C-c C-," . my/open-init)
         ("C-c C-j" . my/open-journal)
         ("C-." . completion-at-point)
         ("C-x k" . kill-current-buffer)
         ("M-/" . hippie-expand))
  :custom
  (use-short-answers t)
  (inhibit-startup-screen t)
  (initial-scratch-message "")
  (initial-major-mode 'fundamental-mode)
  (frame-title-format '("%b"))
  (ring-bell-function 'ignore)
  (sentence-end-double-space nil)
  (cursor-type 'bar)
  (indent-tabs-mode nil)
  (tab-width 2)
  (backup-by-copying t)
  (backup-directory-alist `((".*" . ,(my/expand-file-name "backup"))))
  (auto-save-file-name-transforms `((".*" ,(my/auto-save-directory) t)))
  (create-lockfiles nil)
  (tab-width 4)
  (tab-always-indent 'complete)
  (auto-revert-avoid-polling t)
  (remote-file-name-inhibit-locks t)
  (major-mode-remap-alist
   '((c-mode . c-ts-mode)
     (javascript-mode . js-ts-mode)
     (typescript-mode . typescript-ts-mode)))
  (custom-file (my/expand-file-name "local-init.el"))
  :init
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (when (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))
  (which-key-mode +1)
  (savehist-mode +1)
  (column-number-mode +1)
  (delete-selection-mode +1)
  (global-auto-revert-mode +1)
  (load-theme 'modus-vivendi t)
  (load (my/expand-file-name "local-init.el") t t)
  (setq gc-cons-threshold (or my/gc-cons-threshold 800001)))

;;; -- Packages --

(use-package completion-preview
  :hook ((prog-mode . completion-preview-mode))
  :bind (:map completion-preview-active-mode-map
         ("M-n" . completion-preview-next-candidate)
         ("M-p" . completion-preview-prev-candidate)))

(use-package eglot
  :bind (:map eglot-mode-map
         ("M-q" . eglot-format))
  :config
  (fset #'jsonrpc--log-event #'ignore)
  ;; Some language servers monitor their parent's process ID and will
  ;; automatically kill themselve if the process ID is no longer there. Since
  ;; we run tools in containerized environments, we don't want this behaviour:
  ;; Emacs is running on the host, and its PID is not visible to the container.
  ;; Tell Eglot to **not** send a process ID.
  (setq eglot-withhold-process-id t))

(use-package flymake
  :bind (:map flymake-mode-map
         ("M-n" . flymake-goto-next-error)
         ("M-p" . flymake-goto-prev-error))
  :custom
  (flymake-indicator-type 'margins)
  (flymake-margin-indicators-string
   `((error " " compilation-error)
     (warning " " compilation-warning)
     (note " " compilation-info))))

(use-package project
  :custom
  (project-mode-line t))

(use-package rust-ts-mode
  :mode "\\.rs\\'")

(use-package vc
  :custom
  (vc-handled-backends '(Git)))
(use-package vc-dir
  :bind (:map vc-dir-mode-map
              ("C-c s" . my/vc-spin-off-branch)
              ("C-c P" . my/vc-push-set-upstream)))

(use-package xref
  :config
  (when (executable-find "rg")
    (setopt xref-search-program 'ripgrep)))

(provide 'init)
;;; init.el ends here.
