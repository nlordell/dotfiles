;;; early-init.el --- Emacs early initialization -*- lexical-binding: t -*-

;;; Commentary:

;; Early initialization configuration. Right now, it just increases
;; startup performance and reduces noise.

;;; Code:

(setq init/gc-cons-threshold gc-cons-threshold)
(setq gc-cons-threshold 104857600)

(setq byte-compile-warnings '(not obsolete))
(setq warning-suppress-log-types '((comp) (bytecomp)))
(setq native-comp-async-report-warnings-errors 'silent)
(setq inhibit-startup-echo-area-message "nlordell")

(provide 'early-init)
;;; early-init.el ends here
