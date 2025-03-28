;;; -*- lexical-binding: t -*-

;; Go VROOM-VROOM
(setq gc-cons-threshold 10000000)

;; Setup the cache directory nice and early; we use it everywhere
(make-directory "~/.cache/emacs" t)

;; Configure package.el directories
(setq
 package-user-dir "~/.cache/emacs/elpa/"
 package-gnupghome-dir "~/.cache/emacs/elpa/gnupg")

;; Configure native-comp cache direactory
(when (boundp 'native-comp-eln-load-path)
  (startup-redirect-eln-cache "~/.cache/emacs/eln-cache"))

;; Suppress native-comp warnings on startup
(setq warning-suppress-log-types '((comp) (bytecomp)))
