;;;  -*- no-byte-compile: t; -*-

;; (unpin! lsp-mode)
;; (unpin! lsp-ui)
;; (unpin! rustic)
;; (unpin! consult-lsp)
;; (unpin! treemacs)
;; (unpin! better-jumper)
(package! adoc-mode)
;; (unpin! doom-modeline)
;; (package! doom-modeline)
(package! prettier-js)
(package! evil-nerd-commenter)
(package! highlight-indent-guides)
(package! treemacs-persp)
;; (package! flycheck-posframe)
;; (when EMACS29+
;; (package! evil-textobj-tree-sitter)
;; )
;; (unless EMACS29+
;; (unpin! tree-sitter)
;; (package! tree-sitter)
;;   (unpin! tree-sitter-langs)
;;   (package! tree-sitter-langs)
;;   )
;; (package! treesit-auto)
(package! uuidgen)
(package! dap-mode)
;; protobuf-mode repo contains way more than just the emacs package, including annoying git submodules,
;; and changes frequently, though protobuf-mode.el changes very seldom
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
;; (package! doom-nano-modeline
;;   :recipe (:host github
;;   :repo "ronisbr/doom-nano-modeline"))
(package! impatient-showdown)
(package! jsonnet-mode)
(package! mermaid-mode)
(package! ob-mermaid)
(package! evil-ts
  :recipe (:host github
           :repo "foxfriday/evil-ts"
           :branch "main"))
(package! lilypond-mode
  :recipe (:repo "https://gitlab.com/lilypond/lilypond.git"
           :branch "master"
           :files ("elisp/*.el")
           ))
(package! flycheck-lilypond)
;; (package! vertico-posframe
;;     :recipe (:host github :repo "tumashu/vertico-posframe"))
(package! autothemer)
(package! kanagawa-theme
  :recipe (:host github
           :repo "mattsawyer77/kanagawa-theme"
           :branch "main"))

(disable-packages! hl-line)

(package! just-mode)
(package! emacs-eat
  :recipe (:host codeberg
           :repo "akib/emacs-eat"
           :files ("*.el" ("term" "term/*.el") "*.texi"
                   "*.ti" ("terminfo/e" "terminfo/e/*")
                   ("terminfo/65" "terminfo/65/*")
                   ("integration" "integration/*")
                   (:exclude ".dir-locals.el" "*-tests.el"))))
