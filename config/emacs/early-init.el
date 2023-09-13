;;; Go VROOM-VROOM
(setq gc-cons-threshold 10000000)

;;; Configure package.el directories
(setq
  package-user-dir "~/.cache/emacs/elpa/"
  package-gnupghome-dir "~/.cache/emacs/elpa/gnupg")

;;; Configure native-comp cache direactory
(when (boundp 'native-comp-eln-load-path)
  (startup-redirect-eln-cache "~/.cache/emacs/eln-cache"))
