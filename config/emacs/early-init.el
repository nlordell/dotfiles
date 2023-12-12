;;; Go VROOM-VROOM
(setq gc-cons-threshold 10000000)

;;; Configure package.el directories
(setq
  package-user-dir "~/.cache/emacs/elpa/"
  package-gnupghome-dir "~/.cache/emacs/elpa/gnupg")

;;; Configure native-comp cache direactory
(when (boundp 'native-comp-eln-load-path)
  (startup-redirect-eln-cache "~/.cache/emacs/eln-cache"))

;;; Suppress native-comp warnings on startup
(setq warning-suppress-log-types '((comp) (bytecomp)))

;;; HACK: Work around native compilation errors on macOS...
(if (eq system-type 'darwin)
  (setenv "LIBRARY_PATH" "/opt/local/lib/gcc12/gcc/arm64-apple-darwin23/12.3.0")
)
