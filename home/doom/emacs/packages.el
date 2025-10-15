;;;  -*- no-byte-compile: t; -*-

(unpin! lsp-treemacs)
(package! adoc-mode)
(package! prettier-js)
(package! evil-nerd-commenter)
(package! highlight-indent-guides)
(package! treemacs-persp)
(package! org-journal)
(unpin! org-roam)
(package! org-roam-ui)
(unpin! corfu)
(package! uuidgen)
(unpin! protobuf-mode)
(package! protobuf-mode
  :recipe (:host github
           :repo "protocolbuffers/protobuf"
           :nonrecursive t
           :files ("editors/protobuf-mode.el")))
(package! org-pandoc-import
  :recipe (:host github
           :repo "tecosaur/org-pandoc-import"
           :files ("*.el" "filters" "preprocessors")))
(unpin! flycheck-golangci-lint)
(package! flycheck-golangci-lint
  :recipe (:host github
           :repo "mattsawyer77/flycheck-golangci-lint"
           :branch "main"
           ))
(unpin! makefile-executor)
(package! makefile-executor
  :recipe (:host github
           :repo "mattsawyer77/makefile-executor.el"
           :branch "main"
           ))
(package! rfc-mode)
(package! page-break-lines)
(package! info-colors)
(package! textsize)
(package! org-modern)
(package! ewal)
(package! git-auto-commit-mode)
(package! impatient-showdown)
(package! jsonnet-mode)
(package! mermaid-mode)
(package! ob-mermaid)
(package! evil-ts
  :recipe (:host github
           :repo "foxfriday/evil-ts"
           :branch "main"))
(package! autothemer)
(package! kanagawa-theme
  :recipe (:host github
           :repo "mattsawyer77/kanagawa-theme"
           :branch "main"))
(package! apheleia)
(package! just-mode)
(package! emacs-eat
  :recipe (:host codeberg
           :repo "akib/emacs-eat"
           :files ("*.el" ("term" "term/*.el") "*.texi"
                   "*.ti" ("terminfo/e" "terminfo/e/*")
                   ("terminfo/65" "terminfo/65/*")
                   ("integration" "integration/*")
                   (:exclude ".dir-locals.el" "*-tests.el"))))
(package! envrc)
(package! cov)
(package! lab
  :recipe (:host github
           :repo "isamert/lab.el"
           :branch "main"))
(package! just-mode)
(package! emacs-eat
  :recipe (:host codeberg
           :repo "akib/emacs-eat"
           :files ("*.el" ("term" "term/*.el") "*.texi"
                   "*.ti" ("terminfo/e" "terminfo/e/*")
                   ("terminfo/65" "terminfo/65/*")
                   ("integration" "integration/*")
                   (:exclude ".dir-locals.el" "*-tests.el"))))
(package! tabspaces :recipe (:host github :repo "mclear-tools/tabspaces"))
(package! modern-tab-bar :recipe (:host github :repo "aaronjensen/emacs-modern-tab-bar"))
(package! gptel :recipe (:nonrecursive t))
(package! gptel-quick :recipe (:host github :repo "karthink/gptel-quick"))
(package! gptel-magit)

(package! mcp)

(package! gitlab-lsp :recipe
  (:host github
   :repo "kassick/gitlab-lsp.el"
   :files ("*.el")))
(package! ultra-scroll :recipe (:host github :repo "jdtsmith/ultra-scroll" :branch "main"))
(when (featurep :system 'macos)
  (package! ns-keychain
    :recipe (:host github :repo "mattsawyer77/ns-keychain.el")))

(package! jq-mode :recipe (:host github :repo "ljos/jq-mode"))
(disable-packages! hl-line)
