;;; -*- lexical-binding: t -*-

;;;
;;; General
;;;

(use-package copilot
  :custom
  (copilot-install-dir "~/.cache/emacs/copilot")
  :bind (("C-<tab>" . copilot-accept-completion)
         ("C-M-<tab>" . copilot-accept-completion-by-word)
         ("C-c a a" . copilot-mode)
         ("C-c a n" . copilot-next-completion)
         ("C-c a p" . copilot-previous-completion)))

(use-package eglot
  :ensure nil
  :bind
  (:map eglot-mode-map
        ("M-q" . eglot-format)))

(use-package ligature
  :hook
  (prog-mode . ligature-mode)
  :config
  (ligature-set-ligatures
   'prog-mode
   '(;; Group A
     ".." ".=" "..." "..<" "::" ":::" ":=" "::=" ";;" ";;;" "??" "???" ".?" "?."
     ":?" "?:" "?=" "**" "***" "/*" "*/" "/**"
     ;; Group B
     "<-" "->" "-<" ">-" "<--" "-->" "<<-" "->>" "-<<" ">>-" "<-<" ">->" "<-|"
     "|->" "-|" "|-" "||-" "<!--" "<#--" "<=" "=>" ">=" "<==" "==>" "<<=" "=>>"
     "=<<" ">>=" "<=<" ">=>" "<=|" "|=>" "<=>" "<==>" "||=" "|=" "//=" "/="
     "/=="
     ;; Group C
     "<<" ">>" "<<<" ">>>" "<>" "<$" "$>" "<$>" "<+" "+>" "<+>" "<:" ":<" "<:<"
     ">:" ":>" "<~" "~>" "<~>" "<<~" "<~~" "~~>" "~~" "<|" "|>" "<|>" "<||"
     "||>" "<|||" "|||>" "</" "/>" "</>" "<*" "*>" "<*>" ":?>"
     ;; Group D
     "#(" "#{" "#[" "]#" "#!" "#?" "#=" "#_" "#_(" "##" "###" "####"
     ;; Group E
     "[|" "|]" "[<" ">]" "{!!" "!!}" "{|" "|}" "{{" "}}" "{{--" "--}}" "{!--"
     "//" "///" "!!"
     ;; Group F
     "www" "@_" "&&" "&&&" "&=" "~@" "++" "+++" "/\\" "\\/" "_|_" "||"
     ;; Group G
     "=:" "=:=" "=!=" "==" "===" "=/=" "=~" "~-" "^=" "__" "!=" "!==" "-~" "--"
     "---")))

(use-package magit
  :custom
  (transient-levels-file "~/.cache/emacs/transient/levels.el")
  (transient-values-file "~/.cache/emacs/transient/values.el")
  (transient-history-file "~/.cache/emacs/transient/history.el"))

(use-package markdown-mode
  :hook
  (gfm-mode . flyspell-mode)
  (gfm-mode . visual-line-mode)
  :init
  (add-to-list 'major-mode-remap-alist '(markdown-mode . gfm-mode)))

(use-package projectile
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :custom
  (projectile-known-projects-file "~/.cache/emacs/projectile-bookmarks.eld")
  (projectile-project-search-path '(("~/Developer/" . 2)))
  :config
  (projectile-mode +1))

(use-package rg)

;;;
;;; Ocaml
;;;

(use-package tuareg)

(use-package ocaml-eglot
  :after (tuareg)
  :hook
  (tuareg-mode . ocaml-eglot)
  (ocaml-eglot . eglot-ensure))
