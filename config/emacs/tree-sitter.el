(require 'treesit)

(setq
 tree-sitter-install-dir "~/.cache/emacs/tree-sitter"
 tree-sitter-languages '((rust :version "v0.23.1")
                         (ocaml :version "v0.23.2"
                                :grammars ((ocaml :src "grammars/ocaml/src")
                                           (ocaml_interface :src "grammars/interface/src")
                                           (ocaml_type :src "grammars/type/src")))))

(defun tree-sitter--install-language (lang)
  "Installs tree-sitter language grammars for LANG."
  (let* ((lconf (or (alist-get lang tree-sitter-languages)
                    (error "unknown language %s" lang)))
         (url (or (plist-get lconf :url)
                  (format "https://github.com/tree-sitter/tree-sitter-%s" lang)))
         (version (plist-get lconf :version))
         (grammars (or (plist-get lconf :grammars)
                       `((,lang)))))
    (cl-loop for (grammar . gconf) in grammars do
             (let* ((src (or (plist-get gconf :src) "src"))
                    (recipe (list "~/.cache/emacs/tree-sitter"
                                  grammar url version src)))
               (apply #'treesit--install-language-grammar-1 recipe)))))

(defun tree-sitter--major-mode-remaps (lang)
  "Gets the remap configuration for LANG or `nil' when not available"
  (let* ((lconf (or (alist-get lang tree-sitter-languages)
                    (error "unknown language %s" lang)))
         (remaps (plist-get lconf :remaps)))
    (if (treesit-language-available-p lang) remaps nil)))

(defun use-tree-sitter-language (lang)
  "Installs and configures tree-sitter language grammars for LANG."
  (unless (treesit-language-available-p lang)
    (tree-sitter--install-language lang)
    (let (remaps (tree-sitter--major-mode-remaps lang))
      (append major-mode-remap-alist remaps))))

(setq
 treesit-extra-load-path `(,tree-sitter-install-dir)
 major-mode-remap-alist (mapcan (lambda (lang)
                                  (tree-sitter--major-mode-remaps (car lang)))
                                tree-sitter-languages))
