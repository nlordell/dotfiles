;;; early-init.el --- Emacs early initialization -*- lexical-binding: t -*-

;;; Commentary:

;; Early initialization configuration. Right now, it just increases
;; the `gc-cons-threshold' in order to speed up initialization.

;;; Code:

(setq gc-cons-threshold 104857600)

(provide 'early-init)
;;; early-init.el ends here
