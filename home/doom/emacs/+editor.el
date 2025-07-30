;;;  -*- lexical-binding: t; -*-

(setq read-process-output-max (* 2 1024 1024)) ;; 2MB
;; (setq native-comp-async-report-warnings-errors nil)
(setq doom-modeline-vcs-max-length 30)
(setq doom-modeline-height 32)
(setq doom-modeline-persp-name t)
(setq confirm-kill-emacs nil)
(setq mac-command-modifier 'super)
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
(setq-default truncate-lines t)
(setq-default tab-width 2)
(setq-default scroll-margin 3)
(setq-default maximum-scroll-margin 0.15)
(setq-default sh-basic-offset 2)
(setq sh-basic-offset 2)
(setq scroll-margin 3)

(when (and
       (display-graphic-p)
       (featurep :system 'macos))
  (add-hook! after-init #'ultra-scroll-mode))
;; (setq doom-modeline-icon t)
;; (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
;; (add-to-list 'default-frame-alist '(ns-appearance . dark)))

;; (after! doom-nano-modeline
;;   (doom-nano-modeline-mode 1)
;;   (global-hide-mode-line-mode 1))

;; make kops edit automatically use yaml mode
(add-to-list 'auto-mode-alist '("\\kops-edit.+yaml$" . yaml-ts-mode))
(add-to-list 'auto-mode-alist '("\\\.tmpl$" . go-template-mode))
;; make k8s templates automatically use go template mode
(add-to-list 'auto-mode-alist '("\\k8s\/templates" . go-template-mode))
(add-to-list 'auto-mode-alist '("\\kubernetes\/templates" . go-template-mode))
;; make stack files use yaml mode
(add-to-list 'auto-mode-alist '("\\stack.yaml" . yaml-ts-mode))
(add-to-list 'auto-mode-alist '("\\package.yaml" . yaml-ts-mode))
(add-to-list 'auto-mode-alist '("\\\.aws/*" . toml-ts-mode))
(add-to-list 'auto-mode-alist '("\\\.saml2aws" . toml-ts-mode))
(add-to-list 'auto-mode-alist '("\\\.kube/config.*" . yaml-ts-mode))
(add-to-list 'auto-mode-alist '("\\\.cc$" . c++-ts-mode))
(add-to-list 'auto-mode-alist '("\\\.hpp$" . c++-ts-mode))
(add-to-list 'auto-mode-alist '("\\\.h$" . c++-ts-mode))
;; make SSH authorized keys files more readable
(add-to-list 'auto-mode-alist '("\\SConscript". python-mode))
(add-to-list 'auto-mode-alist '("\\SConstruct". python-mode))
(add-to-list 'auto-mode-alist '("\\go\.mod". go-mod-ts-mode))
;; (add-to-list 'auto-mode-alist '("\\\.go". go-ts-mode))
(add-to-list 'auto-mode-alist '("\\\.go". go-mode))
;; use GNU Makefile mode instead of BSD
(add-to-list 'auto-mode-alist '("\\Makefile" . makefile-gmake-mode))
;; jsonnet
(add-to-list 'auto-mode-alist '("\\\.libsonnet$" . jsonnet-mode))
;; mermaid
(add-to-list 'auto-mode-alist '("\\\.mmd$" . mermaid-mode))
(add-to-list 'auto-mode-alist '("\\\.mermaid$" . mermaid-mode))
;; markdown
(add-to-list 'auto-mode-alist '("\\\.md$" . gfm-mode))
;; CODEOWNERS
(add-to-list 'auto-mode-alist '("\\CODEOWNERS$" . conf-mode))
;; json5-ish
(add-to-list 'auto-mode-alist '("\\\.hujson$" . jsonc-mode))
;; custom protobuf mode to provide some missing navigation functionality
(after! protobuf-plus-mode
  (add-to-list 'auto-mode-alist '("\\.proto\\'" . protobuf-plus-mode)))

(after! evil
  ;; prevent paste from its default behavior of replacing the clipboard register with the replaced contents
  (setq-default evil-kill-on-visual-paste nil)
  (setq evil-kill-on-visual-paste nil))

(setq +format-on-save-disabled-modes (add-to-list '+format-on-save-disabled-modes 'conf-toml-mode))
(setq +format-on-save-disabled-modes (add-to-list '+format-on-save-disabled-modes 'toml-ts-mode))
(setq +format-on-save-disabled-modes (add-to-list '+format-on-save-disabled-modes 'yaml-mode))
(setq +format-on-save-disabled-modes (add-to-list '+format-on-save-disabled-modes 'yaml-ts-mode))
(setq +format-on-save-disabled-modes (add-to-list '+format-on-save-disabled-modes 'cpp-mode))
(setq +format-on-save-disabled-modes (add-to-list '+format-on-save-disabled-modes 'c++-ts-mode))
(setq +format-on-save-disabled-modes (add-to-list '+format-on-save-disabled-modes 'c++-mode))
;; since aphelia is handling this now, adding to +format-on-save-disabled-modes doesn't work for protobuf-mode
;; (setq +format-on-save-disabled-modes (add-to-list '+format-on-save-disabled-modes 'protobuf-mode))
(setq +format-on-save-disabled-modes (add-to-list '+format-on-save-disabled-modes 'shell-script-mode))
(setq +format-on-save-disabled-modes (add-to-list '+format-on-save-disabled-modes 'bash-ts-mode))

(after! ws-butler
  ;; prevent ws-butler mode for makefile-mode
  (setq ws-butler-global-exempt-modes (add-to-list 'ws-butler-global-exempt-modes 'makefile-mode t))
  (setq ws-butler-global-exempt-modes (add-to-list 'ws-butler-global-exempt-modes 'makefile-gmake-mode t))
  (setq ws-butler-global-exempt-modes (add-to-list 'ws-butler-global-exempt-modes 'makefile-bsdmake-mode t)))

;; (add-hook! go-mode
;;   (tree-sitter-hl-mode 1))

;; use tree-sitter for syntax highlighting for modes that don't have native tree-sitter support
;; (after! tree-sitter
;;   ;; (setf tree-sitter-major-mode-language-alist
;;   ;;       (cl-remove 'go-mode tree-sitter-major-mode-language-alist :key #'car))
;;   (require 'tree-sitter-langs)
;;   ;; (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
;;   ;; (add-hook! go-mode #'tree-sitter-mode)
;;   ;; (setq-default tree-sitter-hl-use-font-lock-keywords nil)
;;   ;; (add-hook! go-mode
;;   ;; (setq-local tree-sitter-hl-use-font-lock-keywords t)
;;   ;; (tree-sitter-mode 1)
;;   ;; (go-ts-mode)
;;   ;; (add-function :before-while (local 'tree-sitter-hl-face-mapping-function)
;;   ;;               (lambda (capture-name)
;;   ;;                 (string= capture-name "type"))
;;   ;; )
;;   )

(add-hook! go-mode #'+word-wrap-mode)
;; XXX: tree sitter seems to be terribly slow, force it back to legacy go-mode
(add-hook! go-ts-mode #'go-mode)
;; (add-hook! go-ts-mode #'+word-wrap-mode)

;; (global-treesit-auto-mode 1)
;; don't use tree-sitter for go until one or both of the following are fixed:
;; https://github.com/dominikh/go-mode.el/issues/396
;; https://github.com/dominikh/go-mode.el/issues/401
;; (add-hook! go-ts-mode #'go-mode)
;; until then, use native tree-sitter for major modes that work well with it:
;; (add-hook! rustic-mode #'rust-ts-mode)
;; (add-hook! c++-mode #'c++-ts-mode)
;; (add-hook! c-mode #'c-ts-mode)
;; (add-hook! js-mode #'js-ts-mode)
;; (add-hook! css-mode #'css-ts-mode)
;; (add-hook! shell-mode #'bash-ts-mode) ;; ??
;; (add-hook! html-mode #'html-ts-mode)
;; (add-hook! java-mode #'java-ts-mode)
;; (add-hook! json-mode #'json-ts-mode)
;; (add-hook! ruby-mode #'json-ts-mode)
;; (add-hook! conf-toml-mode #'toml-ts-mode)
;; (add-hook! yaml-mode #'yaml-ts-mode)
;; (add-hook! cmake-mode #'cmake-ts-mode)
;; (add-hook! csharp-mode #'csharp-ts-mode)
;; (add-hook! go-mod-mode #'go-mod-ts-mode)
;; (add-hook! python-mode #'python-ts-mode)
;; (add-hook! dockerfile-mode #'dockerfile-ts-mode)
;; (add-hook! typescript-mode #'typescript-ts-mode)

(after! rustic
  (setq rustic-lsp-server 'rust-analyzer
        rustic-format-on-save t
        lsp-rust-server 'rust-analyzer
        lsp-rust-analyzer-display-chaining-hints t
        lsp-rust-analyzer-display-parameter-hints t
        ;; lsp-rust-analyzer-server-display-inlay-hints t
        ;; lsp-rust-analyzer-cargo-watch-command "clippy"
        )
  )

(after! (rustic lsp-mode)
  ;; do not cache the broken result from rust-analyzer
  (advice-add #'lsp-eldoc-function :after (lambda (&rest _) (setq lsp--hover-saved-bounds nil)))
  ;; extract and show short signature for rust-analyzer
  (cl-defmethod lsp-clients-extract-signature-on-hover (contents (_server-id (eql rust-analyzer)))
    (let* ((value (if lsp-use-plists (plist-get contents :value) (gethash "value" contents)))
           (groups (--partition-by (s-blank? it) (s-lines (s-trim value))))
           (sig_group (if (s-equals? "```rust" (car (-third-item groups)))
                          (-third-item groups)
                        (car groups)))
           (sig (--> sig_group
                     (--drop-while (s-equals? "```rust" it) it)
                     (--take-while (not (s-equals? "```" it)) it)
                     (--map (s-trim it) it)
                     (s-join " " it))))
      (lsp--render-element (concat "```rust\n" sig "\n```")))))

;; XXX: not working
;; (after! dap-mode
;;   (add-hook! 'dap-stopped-hook
;;             (lambda (arg) (call-interactively #'dap-hydra)))
;;   )

;; (after! (go-mode lsp-mode)
;; (add-hook! go-ts-mode #'lsp)
(add-hook! go-mode #'lsp)
;; (require 'dap-go)
;; (dap-go-setup)
;; (setq flycheck-golangci-lint-fast t)
;; )

(add-hook! conf-toml-mode #'lsp)
(add-hook! toml-ts-mode #'lsp)

(add-hook! emacs-lisp-mode #'+word-wrap-mode)
(add-hook! emacs-lisp-mode #'rainbow-mode)
(add-hook! emacs-lisp-mode #'flycheck-mode)

;; (add-hook! (go-mode go-ts-mode)
;;   (lsp)
;;   (+word-wrap-mode 1))
;; (add-hook! go-ts-mode
;;   (setq go-ts-mode-indent-offset 2))

;; override LSP's default diagnostic checker and use golangci-lint instead
;; (add-hook! lsp-diagnostics-mode
;;   (when (eq major-mode 'go-mode)
;;     (lsp-diagnostics-flycheck-disable)
;;     (flycheck-golangci-lint-setup)
;;     (setq flycheck-golangci-lint-fast t)
;;     (map! :mode go-mode
;;           :map general-override-mode-map
;;           :rnv "SPC c x" #'flycheck-list-errors
;;           )))

(after! display-line-numbers
  (add-hook! (prog-mode go-template-mode)
    (turn-on-visual-line-mode)
    (display-line-numbers-mode)
    )
  )


(after! undo-tree
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist '(("." . "~/.config/emacs/.local/undo")))
  (global-undo-tree-mode 1))

(after! evil-surround
  (global-evil-surround-mode 1))

(after! projectile
  (setq projectile-project-search-path '("~/workspaces"
                                         "~/workspaces/f5/volterra/ves.io"
                                         "~/workspaces/f5/volterra/ves.io/sre"
                                         )))

(after! persp
  (setq uniquify-buffer-name-style 'forward))

(after! treemacs
  (require 'treemacs-projectile)
  (treemacs-follow-mode 1)
  (treemacs-project-follow-mode 1)
  (treemacs-git-mode 'deferred)
  (setq-default treemacs--width-is-locked nil)
  (setq treemacs-position 'left)
  (setq treemacs-project-follow-cleanup t)
  )

(defun sawyer/handle-frame-resize ()
  "function to execute whenever the frame resizes"
  (let
      ((new-treemacs-width (/ (frame-pixel-width) 70)))
    (setq treemacs--width-is-locked nil)
    (treemacs-set-width new-treemacs-width)
    (setq treemacs--width-is-locked t)
    (message (format "resized treemacs width to %s" treemacs-width)))
  )
;; (if (> (/ (float (car dimensions)) (car (cdr dimensions))) 2)
;;     ;; wide aspect ratio
;;     (treemacs-set-width 80)
;;   ;; normal aspect ratio
;;   (treemacs-set-width 50)
;;   ))
(after! (textsize treemacs)
  (textsize-mode 1)
  (sawyer/handle-frame-resize)
  ;; (advice-add 'set-frame-height :after #'sawyer/handle-frame-resize)
  ;; (advice-add 'set-frame-width :after #'sawyer/handle-frame-resize)
  ;; (advice-add 'set-frame-size :after #'sawyer/handle-frame-resize)
  )

(after! flycheck
  (setq flycheck-indication-mode 'left-fringe)
  (setq-default flycheck-relevant-error-other-file-show nil)
  ;; make flycheck window auto-resize (with a max height of 5 lines)
  (defadvice flycheck-error-list-refresh (around shrink-error-list activate)
    ad-do-it
    (-when-let (window (flycheck-get-error-list-window t))
      (with-selected-window window
        (fit-window-to-buffer window 5))))
  ;; make flycheck columns wider than their absurd defaults
  (add-hook! flycheck-error-list-mode
    (setq-local tabulated-list-format
                '[("File" 30)
                  ("Line" 5 flycheck-error-list-entry-< :right-align t)
                  ("Col" 3 nil :right-align t)
                  ("Level" 8 flycheck-error-list-entry-level-<)
                  ("ID" 12 t)
                  (#("Message (Checker)" 0 7
                     (face flycheck-error-list-error-message)
                     9 16
                     (face flycheck-error-list-checker-name))
                   0 t)])))
(after! flycheck
  (add-hook! prog-mode (flycheck-mode 1)))

(after! (flycheck-posframe lsp-ui)
  (flycheck-posframe-configure-pretty-defaults)
  (setq flycheck-posframe-border-width 8)
  ;; (add-hook! prog-mode
  ;;   (unless (-contains? local-minor-modes 'lsp-ui-mode)
  ;;       (flycheck-posframe-mode 1)
  ;;       )
  ;;   )
  ;; (add-hook! evil-insert-state-entry
  ;;   (unless (-contains? local-minor-modes 'lsp-ui-mode)
  ;;     (flycheck-posframe-mode -1)))
  ;; (add-hook! evil-insert-state-exit
  ;;   (unless (-contains? local-minor-modes 'lsp-ui-mode)
  ;;     (flycheck-posframe-mode 1))
  ;;   )
  )
(unless (display-graphic-p)
  (after! git-gutter
    (setq git-gutter:modified-sign "▕")
    (setq git-gutter:added-sign "▕")
    (setq git-gutter:deleted-sign "▕")
    )
  (require 'evil-terminal-cursor-changer)
  (after! evil-terminal-cursor-changer
    (evil-terminal-cursor-changer-activate) ; or (etcc-on)
    )
  )

(after! (haskell lsp-haskell ormolu lsp-ui)
  (setq lsp-haskell-process-path-hie "hie-wrapper")
  (setq ormolu-reformat-buffer-on-save t)
  )
(add-hook! haskell-mode #'lsp)
(add-hook! haskell-mode
  (ormolu-format-on-save-mode)
  (flycheck-posframe-mode -1)
  )


;; (add-hook! rustic-mode #'tree-sitter-mode)
(add-hook! (rustic-mode rust-ts-mode)
           ;; (flycheck-select-checker 'rustic-clippy)
           (lsp)
           ;; (lsp-toggle-signature-auto-activate)
           (+word-wrap-mode)
           ;; (flycheck-posframe-mode -1)
           ;; (flycheck-mode -1)
           ;; (tree-sitter-hl-mode 1)
           (lsp-inlay-hints-mode -1)
           (setq lsp-enable-symbol-highlighting nil)
           )

(after! highlight-indent-guides
  (setq highlight-indent-guides-method 'fill)
  (setq highlight-indent-guides-auto-enabled nil)
  (add-hook!
    (emacs-lisp-mode
     haskell-mode
     json-mode
     json-ts-mode
     makefile-mode
     makefile-gmake-mode
     makefile-bsdmake-mode
     ponylang-mode
     conf-toml-mode
     toml-ts-mode
     yaml-mode yaml-ts-mode)
    #'highlight-indent-guides-mode)
  )

;; (after! (terraform-mode lsp-mode)
;;   (lsp-register-client
;;    (make-lsp-client :new-connection (lsp-stdio-connection '("terraform-ls" "serve"))
;;                     :major-modes '(terraform-mode)
;;                     :server-id 'terraform-ls)))

;; (after! (terraform lsp)
;;   (add-to-list 'lsp-language-id-configuration '(terraform-mode . "terraform"))
;;   (lsp-register-client
;;   (make-lsp-client :new-connection (lsp-stdio-connection '("~/.local/bin/terraform-lsp" "-enable-log-file"))
;;                     :major-modes '(terraform-mode)
;;                     :server-id 'terraform-ls)))

;; (add-hook! terraform-mode
;;   (lsp)
;;   (terraform-format-on-save-mode)
;;   )

(add-hook! js2-mode
  (prettier-js-mode)
  )

(after! js2-mode
  (setq-default js2-basic-offset 2)
  )

(after! lsp-mode
  ;; lsp-terraform is broken and breaks other lsp clients,
  ;; see https://github.com/emacs-lsp/lsp-mode/issues/3577
  (delete 'lsp-terraform lsp-client-packages)
  (delete 'lsp-copilot lsp-client-packages)
  ;; (setq lsp-golangci-lint-fast t)
  )

(after! (lsp-mode lsp-ui)
  ;; borrowed from https://github.com/emacs-lsp/lsp-ui/issues/184#issuecomment-1162406920
  ;; to fix issues with lsp-ui-sideline alignment caused by having the default font scaled up
  ;; XXX: not working
  ;; (defun lsp-ui-sideline--compute-text-width (text-with-properties &optional window)
  ;;   (let ((window (or window (selected-window)))
  ;;         (remapping-alist face-remapping-alist))
  ;;     (with-temp-buffer
  ;;       (setq-local face-remapping-alist remapping-alist)
  ;;       (set-window-buffer window (current-buffer))
  ;;       (insert text-with-properties)
  ;;       (car (window-text-pixel-size)))))

  ;; borrowed from https://github.com/emacs-lsp/lsp-ui/issues/184#issuecomment-1161554461
  ;; to fix issues with lsp-ui-sideline alignment caused by having the default font scaled up
  ;; NOTE: working but might crash emacs
  ;; (defun lsp-ui-sideline--window-width ()
  ;;   (- (window-max-chars-per-line)
  ;;      (lsp-ui-sideline--margin-width)
  ;;      (or (and (>= emacs-major-version 27)
  ;;               ;; We still need this number when calculating available space
  ;;               ;; even with emacs >= 27
  ;;               (lsp-ui-util-line-number-display-width))
  ;;          0)))

  ;; (defun lsp-ui-sideline--display-all-info (list-infos tag bol eol)
  ;;   (when (and (lsp-ui-sideline--valid-tag-p tag 'line)
  ;;              (not (lsp-ui-sideline--stop-p)))
  ;;     (let ((inhibit-modification-hooks t)
  ;;           (win-width (lsp-ui-sideline--window-width))
  ;;           ;; sort by bounds
  ;;           (list-infos (--sort (< (caadr it) (caadr other)) list-infos)))
  ;;       (lsp-ui-sideline--delete-kind 'info)
  ;;       (--each list-infos
  ;;         (-let (((symbol bounds info) it))
  ;;           (lsp-ui-sideline--push-info win-width symbol bounds info bol eol))))))

  ;; ;; borrowed from https://github.com/emacs-lsp/lsp-ui/issues/441#issue-611772813
  ;; ;; to make lsp-ui-peek appear in a childframe/posframe
  ;; (defun lsp-ui-peek--peek-display (src1 src2)
  ;;   (-let* ((win-width (frame-width))
  ;;           (lsp-ui-peek-list-width (/ (frame-width) 2))
  ;;           (string (-some--> (-zip-fill "" src1 src2)
  ;;                     (--map (lsp-ui-peek--adjust win-width it) it)
  ;;                     (-map-indexed 'lsp-ui-peek--make-line it)
  ;;                     (-concat it (lsp-ui-peek--make-footer))))
  ;;           )
  ;;     (setq lsp-ui-peek--buffer (get-buffer-create " *lsp-peek--buffer*"))
  ;;     (posframe-show lsp-ui-peek--buffer
  ;;                    :string (mapconcat 'identity string "")
  ;;                    :min-width (frame-width)
  ;;                    :poshandler #'posframe-poshandler-frame-center)))

  ;; (defun lsp-ui-peek--peek-destroy ()
  ;;   (when (bufferp lsp-ui-peek--buffer)
  ;;     (posframe-delete lsp-ui-peek--buffer))
  ;;   (setq lsp-ui-peek--buffer nil
  ;;         lsp-ui-peek--last-xref nil)
  ;;   (set-window-start (get-buffer-window) lsp-ui-peek--win-start))

  ;; (advice-add #'lsp-ui-peek--peek-new :override #'lsp-ui-peek--peek-display)
  ;; (advice-add #'lsp-ui-peek--peek-hide :override #'lsp-ui-peek--peek-destroy)

  ;; (defun lsp-ui-sideline--align (&rest lengths)
  ;;   (list (* (window-font-width)
  ;;            (+ (apply '+ lengths) (if (display-graphic-p) 1 2)))))
  ;; end of lsp-ui-sideline patches

  (setq lsp-response-timeout 30)
  (setq lsp-file-watch-ignored-directories
        ;; NOTE: [/\\\\]  is a custom token defined by lsp-mode to represent a path separator
        ;;   and [^/\\\\] is a custom token defined by lsp-mode to represent a non-path-separator
        ;;   and \\ is something I don't understand but it causes lsp to break if followed by an underscore
        (cl-union lsp-file-watch-ignored-directories
                  '("[/\\\\]\.cache[/\\\\]?"
                    "\.cache\.go"
                    "[/\\\\]extschema[/\\\\]?"
                    "[/\\\\]_extschema[/\\\\]?"
                    "[/\\\\]_protoschema[/\\\\]?"
                    "pbvesenv"
                    "pbts"
                    "pbswagger"
                    "pbsnippet"
                    "pbplantuml"
                    "pbgo"
                    "pbcpp"
                    "vendor"
                    "[/\\\\]target[/\\\\]?"
                    "\.vscode"
                    "\.idea"
                    "\.direnv"
                    "\.devenv"
                    ))
        )
  (setq lsp-modeline-diagnostics-scope :file)
  (setq lsp-file-watch-threshold 8000)
  (setq lsp-headerline-breadcrumb-enable 't)
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project symbols))
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-ui-peek-enable nil)
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-position 'at-point)
  (setq lsp-ui-doc-border (doom-lighten 'bg 0.1))
  (add-to-list 'lsp-ui-doc-frame-parameters '(internal-border-width . 8))
  (setq lsp-ui-doc-show-with-cursor nil)
  (setq lsp-ui-doc-show-with-mouse t)
  (setq lsp-lens-enable t)
  (setq lsp-headerline-breadcrumb-enable t)
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-ui-sideline-show-hover nil)
  (setq lsp-ui-sideline-show-code-actions nil)
  (setq lsp-ui-sideline-show-diagnostics nil)
  (setq lsp-modeline-code-actions-enable t)
  (setq lsp-modeline-diagnostics-enable t)
  (setq lsp-signature-auto-activate t) ;; you could manually request them via `lsp-signature-activate`
  (setq lsp-signature-render-documentation t)
  (setq lsp-completion-show-detail t)
  (setq lsp-completion-show-kind t)
  ;; fix for https://github.com/emacs-lsp/lsp-mode/issues/2701
  (setq lsp-enable-links t) ;; is this working?
  (setq lsp-ui-imenu-buffer-position 'left)
  ;; (setq lsp-diagnostics-provider :flymake)
  ;; (setq lsp-diagnostics-provider :flycheck)
  (setq lsp-nix-nil-formatter ["nixpkgs-fmt"])
  (setq nix-nixfmt-bin "nixpkgs-fmt")
  )

(after! (flycheck rustic)
  (push 'rustic-clippy flycheck-checkers)
  )

(after! org
  (setq org-export-body-only t)
  (cond ((equal (system-name) "SEA-ML-00059144")
         (setq org-agenda-files '("/Users/sawyer/Documents/OneDrive - F5 Networks/notes")))
        ((equal (system-name) "KD21QWDKW7")
         (progn
           (setq org-directory "~/onedrive/notes")
           (setq org-agenda-files '("/Users/m.sawyer/Library/CloudStorage/OneDrive-F5,Inc/notes"))
           ))
        (t
         (setq org-agenda-files '("/Users/sawyer/Library/Mobile Documents/com~apple~CloudDocs/notes"))))
  ;; (setq
  ;;  org-hide-emphasis-markers t
  ;;  org-hide-block-startup nil
  ;;  org-hide-leading-stars t
  ;;  org-hide-macro-markers t
  ;;  org-auto-align-tags nil
  ;;  org-tags-column 0
  ;;  org-fold-catch-invisible-edits 'show-and-error
  ;;  org-special-ctrl-a/e t
  ;;  org-insert-heading-respect-content t

  ;;  ;; Org styling, hide markup etc.
  ;;  org-pretty-entities t
  ;;  org-ellipsis "…"

  ;;  ;; Agenda styling
  ;;  org-agenda-block-separator ?─
  ;;  org-agenda-time-grid
  ;;  '((daily today require-timed)
  ;;    (800 1000 1200 1400 1600 1800 2000)
  ;;    " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
  ;;  org-agenda-current-time-string
  ;;  "⭠ now ─────────────────────────────────────────────────")
  ;; )
  )
;; (after! (org org-roam)
;;   (setq org-roam-directory (file-truename (format! "%s/%s" org-directory "roam")))
;;   )
;; (after! (org org-journal)
;;   (setq +org-capture-journal-file (file-truename (format! "%s/%s" org-directory "journal")))
;;   (setq org-journal-dir (file-truename (format! "%s/%s" org-directory "journal")))
;;   )

;; (when-let (dims (doom-store-get 'last-frame-size))
;;   (cl-destructuring-bind ((left . top) width height fullscreen) dims
;;     (setq initial-frame-alist
;;           (append initial-frame-alist
;;                   `((left . ,left)
;;                     (top . ,top)
;;                     (width . ,width)
;;                     (height . ,height)
;;                     (fullscreen . ,fullscreen))))))

;; (defun save-frame-dimensions ()
;;   (doom-store-put 'last-frame-size
;;                   (list (frame-position)
;;                         (frame-width)
;;                         (frame-height)
;;                         (frame-parameter nil 'fullscreen))))

;; (add-hook 'kill-emacs-hook #'save-frame-dimensions)

;; (add-hook! ponylang-mode
;;   (setq create-lockfiles nil)
;;   (require 'flycheck-pony)
;;   (require 'pony-snippets)
;;   (flycheck-select-checker 'pony)
;;   (whitespace-mode -1)
;;   )

(add-hook! makefile-mode #'+word-wrap-mode)

;; (after! (lsp-mode tramp ccls)
;;   (lsp-register-client
;;    (make-lsp-client
;;     ;; :new-connection (lsp-stdio-connection (lambda () (cons ccls-executable ccls-args)))
;;     :new-connection (lsp-tramp-connection (lambda () (cons ccls-executable ccls-args)))
;;     :remote? t
;;     :major-modes '(c++-ts-mode cpp-mode c-mode c++-mode cuda-mode objc-mode)
;;     :server-id 'ccls
;;     :multi-root nil
;;     :notification-handlers
;;     (lsp-ht ("$ccls/publishSkippedRanges" #'ccls--publish-skipped-ranges)
;;             ("$ccls/publishSemanticHighlight" #'ccls--publish-semantic-highlight))
;;     :initialization-options (lambda () ccls-initialization-options)
;;     :library-folders-fn ccls-library-folders-fn))
;;   (setq ccls-initialization-options '(:index (:comments 2) :completion (:detailedLabel t))))

;; (after! ccls
;;   (setq ccls-initialization-options '(:index (:comments 2) :completion (:detailedLabel t)))
;;   (set-lsp-priority! 'ccls 2)) ; optional as ccls is the default in Doom
;; (add-hook! ccls #'tree-sitter-mode)

(add-hook! protobuf-mode #'display-line-numbers-mode)
(add-hook! protobuf-mode #'flycheck-mode)
(add-hook! protobuf-mode #'+word-wrap-mode)
(add-hook! protobuf-plus-proto-root
  (setq-local proto-search-path (vc-git-root
                                 (file-name-as-directory
                                  (or (buffer-file-name) default-directory)))))

;; disable autoformat for protobuf-mode
(add-hook! protobuf-mode (apheleia-mode -1))

;; buf format only seems to work when run from the project root dir
;; see https://github.com/radian-software/apheleia/issues/30#issuecomment-778150037
;; (defun sawyer/fix-apheleia-project-dir (orig-fn &rest args)
;;   (let ((project (project-current)))
;;     ;; if protobuf mode, change working dir to root before running buf format
;;     (if (and (eq major-mode 'protobuf-mode) (not (null project)))
;;         (let ((default-directory (project-root project))) (apply orig-fn args))
;;       (apply orig-fn args))))

;; (after! apheleia-mode
;;   (add-to-list `apheleia-formatters '(buf . ("buf" "format" "-w" "--debug" "--path" "file")))
;;   (advice-add 'apheleia-format-buffer :around #'sawyer/fix-apheleia-project-dir))

;; (add-hook! protobuf-mode
;;   (setq-local apheleia-formatter 'buf))

(put 'flycheck-protoc-import-path 'safe-local-variable #'listp)

;; (after! (yaml-mode lsp-mode)
;;     (setq lsp-yaml-format-enable nil)
;;     ;; (setq lsp-yaml-prose-wrap nil)
;;     ;; (setq lsp-yaml-print-width nil)
;;     ;; (setq lsp-yaml-bracket-spacing nil)
;;     ;; (setq lsp-yamlls-after-open-hook nil)
;;     (setq lsp-yaml-completion nil)
;;     (setq lsp-yaml-validate nil)
;;     ;; (setq lsp-yaml--schema-store-schemas-alist nil)
;;     ;; (setq lsp-yaml-single-quote nil)
;;     ;; (setq lsp-yaml-schema-store-enable nil)
;;     ;; (setq lsp-yaml-schemas nil)
;;     ;; (setq lsp-yaml-schema-store-local-db nil)
;;     (setq lsp-yaml--built-in-kubernetes-schema nil)
;;     ;; (setq lsp-yaml-server-command nil)
;;     ;; (setq lsp-yaml-schema-store-uri nil)
;;     ;; (setq lsp-yaml-hover nil)
;;     ;; (setq lsp-yaml-custom-tags nil)
;;     )
(add-hook! (yaml-mode yaml-ts-mode) (+lsp-optimization-mode -1))

;; (after! org-pandoc-import
;;   ;; automatically convert markdown to org (and back) on-the-fly
;;   (org-pandoc-import-transient-mode 1)
;;   (add-hook! markdown-mode
;;     (org-pandoc-import-transient-mode 1))
;;   )

(after! magit
  (setq auto-revert-check-vc-info t)
  (setq auto-revert-interval 30)
  (setq magit-refresh-status-buffer nil)
  (setq magit-diff-highlight-indentation nil)
  (setq magit-diff-highlight-trailing nil)
  (setq magit-diff-paint-whitespace nil)
  (setq magit-diff-highlight-hunk-body nil)
  (setq magit-diff-refine-hunk nil)
  (setq-default git-commit-summary-max-length 100)
  (setq git-commit-summary-max-length 100)
  ;; (remove-hook 'magit-status-sections-hook 'magit-insert-tags-header)
  ;; (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-pushremote)
  ;; (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-pushremote)
  ;; (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-upstream)
  ;; (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-upstream-or-recent)
  ;; (remove-hook 'magit-status-sections-hook 'magit-insert-status-headers)
  )
;; (add-hook! magit (lambda () (magit-delta-mode +1)))

;; (use-package! makefile-executor
;;   :defer
;;   :config
;;   ;; ".PHONY: emacs--makefile--list\n
;;   ;; emacs--makefile--list:
;;   ;; @$(MAKE) -pRrq -f $(lastword $(MAKEFILE_LIST)) : 2>/dev/null | awk -v RS= -F: '/^# File/,/^# Finished Make data base/ {if ($$1 !~ \"^[#.]\") {print $$1}}' | sort | egrep -v -e '^[^[:alnum:]]' -e '^$@$$'\n"
;;   (setq-default makefile-executor-list-target-code)
;;   )

;; (use-package! vertico-posframe
;;   :config
;;   (setq vertico-posframe-min-height 20)
;;   (vertico-posframe-mode 1))

(after! nim-mode #'lsp)

(add-hook! rfc-mode-hook #'page-break-lines-mode)
(add-hook! rfc-mode-hook #'writeroom-mode)
;; (add-hook! magit-mode
;;   (magit-delta-mode 1)
;;   )

;; Configure directory extension.
;; (use-package! vertico-buffer
;;   :after vertico
;;   :ensure nil
;;   ;; ;; More convenient directory navigation commands
;;   ;; :bind (:map vertico-map
;;   ;;        ("RET" . vertico-directory-enter)
;;   ;;        ("DEL" . vertico-directory-delete-char)
;;   ;;        ("M-DEL" . vertico-directory-delete-word))
;;   ;; ;; Tidy shadowed file names
;;   ;; :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
;;   :config
;;   (setq vertico-buffer-display-action
;;         '(display-buffer-in-side-window
;;           (window-height . 12)
;;           (side . top)))
;;   (defun my-vertico-buffer-setup ()
;;     (dolist (win (get-buffer-window-list))
;;       (set-window-parameter win 'my-mini-window (window-minibuffer-p win)))
;;     (face-remap-add-relative 'default '(:filtered (:window my-mini-window nil) (:background "gray98")))
;;     (face-remap-add-relative 'fringe '(:filtered (:window my-mini-window nil) (:background "gray90")))
;;     (face-remap-add-relative 'mode-line-active :height 1.0 :box nil :background "white" :overline "gray90")
;;     (face-remap-add-relative 'mode-line-inactive :height 1.0 :box nil :background "white" :overline "gray90")
;;     (face-remap-add-relative 'header-line :height 1 :box nil :background "white" :underline "gray90")
;;     (setq-local
;;      left-fringe-width 1
;;      right-fringe-width 1
;;      left-margin-width 1
;;      right-margin-width 1
;;      fringes-outside-margins t
;;      mode-line-format ""
;;      header-line-format "")
;;     (dolist (win (get-buffer-window-list))
;;       (set-window-buffer win (current-buffer))))
;;   (advice-add #'vertico-buffer--setup :after #'my-vertico-buffer-setup)
;;   )

(after! vterm
  (setq vterm-shell "/bin/zsh")
  (setq vterm-tramp-shells nil)
  ;; (set-popup-rule! "^\\*doom:vterm.*"
  ;;   :side 'right
  ;;   :slot 5
  ;;   :vslot 5
  ;;   :size 0.40
  ;;   :select t
  ;;   :quit t
  ;;   :autosave t)
  (set-popup-rule! "^\\*doom:vterm.*" :ignore t))

(set-popup-rule! "^\\*eww\\*"
  :side 'right
  :slot 5
  :vslot 5
  :size 0.50
  )

(set-popup-rule! "^\\*jq-json\\*"
  :side 'right
  :slot 5
  :vslot 5
  :size 0.50
  )

(after! go-guru)
(set-popup-rule! "^\\*go-guru-output.*"
  :side 'bottom
  :size 5
  )

(set-popup-rule! "\\*gpt:*"
  :side 'right
  :slot 5
  :vslot 5
  :size 0.40
  )

(after! centaur-tabs
  ;; override stock centaur-tabs-hide-tab
  ;; (defun centaur-tabs-hide-tab (buffer-name)
  ;; "conditionally hide the tab based on the buffer name"
  ;; (let ((name (format "%s" buffer-name)))
  ;; (or
  ;; ;; Current window is not dedicated window.
  ;; (window-dedicated-p (selected-window))
  ;;
  ;; ;; Buffer name not match below blacklist.
  ;; (string-prefix-p " *" name)
  ;; (string-prefix-p "*" name))))

  ;;   (defun centaur-tabs-hide-tab-cached (buf)
  ;;     "Cached vesion of `centaur-tabs-hide-tab' to improve performance.
  ;; Operates over buffer BUF"
  ;;     ;; (let ((hide (gethash buf centaur-tabs-hide-hash 'not-found)))
  ;;     ;;   (when (eq hide 'not-found)
  ;;     ;;     (setq hide (funcall centaur-tabs-hide-tab-function buf))
  ;;     ;;     (puthash buf hide centaur-tabs-hide-hash))
  ;;     ;;   hide))
  ;;     (centaur-tabs-hide-tab-function buf))

  ;; same as upstream, but don't create arbitrary groups for hidden buffers
  ;; (defun centaur-tabs-projectile-buffer-groups ()
  ;;   "Return the list of group names BUFFER belongs to."
  ;;   (if centaur-tabs-projectile-buffer-group-calc
  ;;       (symbol-value 'centaur-tabs-projectile-buffer-group-calc)
  ;;     (set (make-local-variable 'centaur-tabs-projectile-buffer-group-calc)
  ;;          (cond
  ;;           ;; ((or (get-buffer-process (current-buffer)) (memq major-mode '(comint-mode compilation-mode))) '("Term"))
  ;;           ;; ((string-equal "*" (substring (buffer-name) 0 1)) '("Misc"))
  ;;           ((condition-case _err
  ;;                (projectile-project-root)
  ;;              (error nil))
  ;;            (list (projectile-project-name)))
  ;;           ;; ((memq major-mode '(emacs-lisp-mode python-mode emacs-lisp-mode c-mode
  ;;           ;;                     c++-mode javascript-mode js-mode
  ;;           ;;                     js2-mode makefile-mode
  ;;           ;;                     lua-mode vala-mode))
  ;;           ;;  '("Coding"))
  ;;           ;; ((memq major-mode '( nxhtml-mode html-mode
  ;;           ;;                      mhtml-mode css-mode))
  ;;           ;;  '("HTML"))
  ;;           ;; ((memq major-mode '(org-mode calendar-mode diary-mode)) '("Org"))
  ;;           ;; ((memq major-mode '(dired-mode)) '("Dir"))
  ;;           (t '())))
  ;;     (symbol-value 'centaur-tabs-projectile-buffer-group-calc)))

  (setq
   centaur-tabs--buffer-show-groups t
   ;; centaur-tabs-style "alternate"
   ;; centaur-tabs-style "bar"
   centaur-tabs-style "box"
   ;; centaur-tabs-style "chamfer"
   ;; centaur-tabs-style "rounded"
   ;; centaur-tabs-style "slant"
   ;; centaur-tabs-style "wave"
   ;; centaur-tabs-style "zigzag"
   centaur-tabs-height 32
   centaur-tabs-set-icons nil
   centaur-tabs-gray-out-icons 'buffer
   centaur-tabs-set-bar nil
   ;; centaur-tabs-set-bar 'over
   centaur-tabs-excluded-prefixes '("*" " *"))
  (centaur-tabs-group-by-projectile-project)
  )

;;(add-hook! after-init #'centaur-tabs-mode)

;; XXX: the following runs but something else re-enables ws-butler...
(add-hook! makefile-mode
  (message "in makefile-mode hook")
  (indent-tabs-mode 1)
  (ws-butler-mode -1)
  (message "ws-butler should be disabled...")
  (if (-contains? local-minor-modes 'ws-butler-mode)
      (message "dammit...ws-butler is still active")
    (message "hooray...ws-butler is dead")
    )
  )
;; (add-hook! makefile-bsdmake-mode
;;            (message "in makefile-bsdmake-mode hook")
;;            (indent-tabs-mode 1)
;;            (ws-butler-mode -1)
;;            )
;; (add-hook! makefile-gmake-mode
;;            (message "in makefile-gmake-mode hook")
;;            (indent-tabs-mode 1)
;;            (ws-butler-mode -1)
;;            )

(after! impatient-showdown
  ;; Display markdown depends on your flavor.
  (setq impatient-showdown-flavor 'github)
  ;; For display body background color.
  (setq impatient-showdown-background-color "#f0f0f0")
  ;; Preview HTML template.
  ;; (setq impatient-showdown-preview-template )
  ;; For display markdown border color.
  (setq impatient-showdown-markdown-border-color "#f8f8f8")
  ;; Default preview HTML template.
  ;; (setq impatient-showdown--default-preview-template )
  ;; display markdown background color.
  (setq impatient-showdown-markdown-background-color "#f8f8f8")
  (setq impatient-mode-delay 1.0) ;; seconds
  )

(after! consult
  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file
   +default/search-project
   +default/search-other-project
   +default/search-project-for-symbol-at-point
   +default/search-cwd
   +default/search-other-cwd
   +default/search-notes-for-symbol-at-point
   +default/search-emacsd
   consult--source-recent-file
   consult--source-project-recent-file
   consult--source-bookmark
   :preview-key (list :debounce 0.1 'any))
  )

(after! vertico
  (vertico-multiform-mode 1)
  (setq vertico-multiform-commands
        '((consult-imenu grid)
          (consult-line buffer)
          ))
  )

;; XXX: ob-mermaid not working with docker-based mermaid, for now, have to do `npm install -g '@mermaid-js/mermaid-cli'`
;; mermaid-cli:local tagged from "ghcr.io/mermaid-js/mermaid-cli/mermaid-cli:10.8.1-beta.3@sha256:d05922a4f8949eee96d90b7d6dc8c32f8843c02ab244f694d68c23a419e91b77"
;; (defcustom mermaid-docker-image "mermaid-cli:local"
;;   "docker image used to execute mermaid CLI (mmdc)"
;;   :type 'string)
;; (after! mermaid-mode
;;   (setq mermaid-mmdc-location "docker")
;;   (setq mermaid-flags
;;         (format "run -u %d -v /tmp:/tmp %s" (user-uid) mermaid-docker-image)))

(after! git-auto-commit-mode (setq gac-automatically-push-p t))

;; from emacs-lsp-booster
;; https://github.com/blahgeek/emacs-lsp-booster
(defun lsp-booster--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json."
  (or
   (when (equal (following-char) ?#)
     (let ((bytecode (read (current-buffer))))
       (when (byte-code-function-p bytecode)
         (funcall bytecode))))
   (apply old-fn args)))
(advice-add (if (progn (require 'json)
                       (fboundp 'json-parse-buffer))
                'json-parse-buffer
              'json-read)
            :around
            #'lsp-booster--advice-json-parse)

(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Prepend emacs-lsp-booster command to lsp CMD."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?)                             ;; for check lsp-server-present?
             (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
             lsp-use-plists
             (not (functionp 'json-rpc-connection))  ;; native json-rpc
             (executable-find "emacs-lsp-booster"))
        (progn
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))

(after! lsp-mode
  (let ((emacs-lsp-booster-cmd (executable-find "emacs-lsp-booster")))
    (if emacs-lsp-booster-cmd
        (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)
      (message "could not find emacs-lsp-booster on path, falling back to default (non-boosted) lsp-mode")))
  )
;; end of emacs-lsp-booster

(after! lsp-treemacs
  (setq lsp-treemacs-error-list-expand-depth 3)
  )

(after! emacs-eat
  (setq eat-term-name "xterm-256color"))

(after! envrc
  (envrc-global-mode))

(after! lsp-clangd
  (setq lsp-clients-clangd-args
        '("-j=3"
          "--background-index"
          ;; "--clang-tidy"
          "--completion-style=detailed"
          "--header-insertion=never"
          "--header-insertion-decorators=0"
          "--log=verbose"
          ))
  (set-lsp-priority! 'clangd 2))

(defvar sawyer/org-mode-left-margin-width 2
  "The `left-margin-width' to be used in `org-mode' buffers.")
(defvar sawyer/org-mode-right-margin-width 2
  "The `right-margin-width' to be used in `org-mode' buffers.")

;; (defun sawyer/setup-org-mode-margins ()
;;   (when (and (derived-mode-p 'org-mode)
;;              (eq (current-buffer) ; Check current buffer is active.
;;                  (window-buffer (frame-selected-window))))
;;     (setq left-margin-width (if display-line-numbers
;;                                 0 sawyer/org-mode-left-margin-width))
;;     (setq right-margin-width (if display-line-numbers
;;                                  0 sawyer/org-mode-right-margin-width))
;;     (set-window-buffer (get-buffer-window (current-buffer))
;;                        (current-buffer))))
;; (add-hook! window-configuration-change #'sawyer/setup-org-mode-margins)
;; (add-hook! display-line-numbers-mode #'sawyer/setup-org-mode-margins)
;; (add-hook! org-mode #'sawyer/setup-org-mode-margins)

;; (after! spacious-padding
;;   (setq spacious-padding-widths '(:internal-border-width 0 :header-line-width 4 :mode-line-width 6 :tab-width 4 :right-divider-width 30 :scroll-bar-width 8 :fringe-width 30)))
;; (add-hook! org-mode #'spacious-padding-mode)

(add-hook! c++-mode 'lsp)
(add-hook! c++-ts-mode 'lsp)

(after! lab
  ;; Required.
  (setq lab-host "https://gitlab.com")

  ;; Required.
  ;; See the following link to learn how you can gather one for yourself:
  ;; https://docs.gitlab.com/ee/user/profile/personal_access_tokens.html#create-a-personal-access-token
  (setq lab-token (getenv "GITLAB_API_TOKEN"))

  ;; Optional, but useful. See the variable documentation.
  (setq lab-group "12645129") ;; F5/volterra
  )

(when (featurep :system 'macos)
  (require 'ns-keychain))

(defun sawyer/get-creds (hostname account)
  "get credentials securely"
  (if (fboundp 'ns-keychain-get-internet-password)
      (ns-keychain-get-internet-password hostname account)
    (error "TODO: implement sawyer/get-creds for non-macOS")))

(after! gptel
  (setq gptel-model '04-mini
        gptel-default-mode 'org-mode
        gptel-track-media t
        gptel-org-set-properties t
        gptel-prompt-prefix-alist '((markdown-mode . "# ")
                                    (org-mode . "* ")
                                    (text-mode . "### "))
        gptel-response-prefix-alist
        '((markdown-mode . "")
          (org-mode . "** 🤖:\n")
          (text-mode . ""))
        gptel-backend (gptel-make-openai "gpt: Local OpenWebUI"
                        :host "127.0.0.1:3000"
                        :protocol "http"
                        :endpoint "/api/chat/completions"
                        :key (sawyer/get-creds "127.0.0.1" "matt.sawyer@gmail.com")
                        :stream t
                        :models '(o4-mini gpt-4.1)))

  ;; (setq gptel-model   '04-mini
  ;;       gptel-backend (gptel-make-openai "gpt: F5GPT"
  ;;                       :host "f5ai.pd.f5net.com"
  ;;                       :endpoint "/api/chat/completions"
  ;;                       :stream t
  ;;                       :key (sawyer/get-creds "f5ai.pd.f5net.com" "sawyer")
  ;;                       :models '(o4-mini gpt-4.1)))
  )

;; (after! aider
;;   (aider-doom-enable)
;;   ;; For latest claude sonnet model
;;   ;; (setq aider-args '("--model" "sonnet"))
;;   ;; (setq aider-args `(
;;   ;;                    "--model-settings-file" ,(expand-file-name "~/.aider.model.settings.yml")
;;   ;;                    ;; "--model" "ollama_chat/deepseek-r1:14"
;;   ;;                    "--model" "ollama_chat/mistral-small:24b"
;;   ;;                    ;; "--model" "ollama_chat/gemma3:1b"
;;   ;;                    ;; "--model" "ollama_chat/qwen2.5-coder:latest"
;;   ;;                    ))
;;   (setenv "OPENAI_API_BASE" "https://f5ai.pd.f5net.com/api")
;;   (setenv "AIDER_OPENAI_API_KEY" (getenv "OPENAI_API_KEY"))
;;   ;; (setenv "ANTHROPIC_API_KEY" anthropic-api-key)
;;   ;; Or chatgpt model
;;   ;; (setq aider-args '("--model" "o3-mini"))
;;   ;; (setq aider-args '("--model" "openai/gpt-4.5-preview"))
;;   (setq aider-args '("--model" "gpt-4o"))
;;   ;; (setq aider-args '("--model" "openai/gpt-4.5-preview-2025-02-27"))
;;   ;; (setenv "OPENAI_API_KEY" <your-openai-api-key>)
;;   (setenv "OLLAMA_API_BASE" "http://haystack-ts:11434")
;;   (setenv "AIDER_OPENAI_API_BASE" "https://f5ai.pd.f5net.com/api")

;;   ;; Or use your personal config file
;;   ;; ;;
;;   ;; Optional: Set a key binding for the transient menu
;;   )

;; (after! gitlab-lsp
;;   ;; speed up completions
;;   (setq gitlab-lsp-show-completions-with-other-clients nil)
;;   (setq gitlab-lsp-token (getenv "GITLAB_LSP_TOKEN"))

;;   (add-hook 'gitlab-lsp-complete-before-complete-hook
;;             (lambda ()
;;               ;; scroll to top so preview can show the snippet
;;               (recenter-top-bottom 4)

;;               ;; Show something, since we can not spin ...
;;               (message "Asking for suggestions ...")))

;;   (define-key global-map
;;               (kbd "C-*") '("Complete with Gitlab Duo" . gitlab-lsp-complete))
;;   )

(after! treesit
  (setq treesit-extra-load-path '("~/.config/emacs/.local/cache/tree-sitter"))
  (setq treesit-language-source-alist '(
                                        (bash "https://github.com/tree-sitter/tree-sitter-bash")
                                        (cmake "https://github.com/uyha/tree-sitter-cmake")
                                        (c "https://github.com/tree-sitter/tree-sitter-c")
                                        (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
                                        (css "https://github.com/tree-sitter/tree-sitter-css")
                                        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
                                        (go "https://github.com/tree-sitter/tree-sitter-go" "v0.23.4")
                                        (gomod "https://github.com/camdencheek/tree-sitter-go-mod" "v1.1.0")
                                        (html "https://github.com/tree-sitter/tree-sitter-html")
                                        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
                                        (json "https://github.com/tree-sitter/tree-sitter-json")
                                        (make "https://github.com/alemuller/tree-sitter-make")
                                        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
                                        (python "https://github.com/tree-sitter/tree-sitter-python")
                                        (rust "https://github.com/tree-sitter/tree-sitter-rust")
                                        (toml "https://github.com/tree-sitter/tree-sitter-toml")
                                        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
                                        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
                                        (yaml "https://github.com/ikatyang/tree-sitter-yaml")))
  (setq treesit-font-lock-level 4))
;; (add-hook! go-ts-mode
;;   (setq-local treesit-font-lock-level 3))

(after! modern-tab-bar
  (setq modern-tab-bar-separator "")
  )
(after! tabspaces
  (setq
   tabspaces-use-filtered-buffers-as-default t
   ;; tabspaces-default-tab "Default"
   tabspaces-remove-to-default nil
   tabspaces-exclude-buffers '("^\\*.*" "^ \\*.*")
   tabspaces-include-buffers '("*scratch*")
   tabspaces-initialize-project-with-todo nil
   ;; sessions
   ;; tabspaces-session t
   ;; tabspaces-session-auto-restore t
   )
  (setq tab-bar-new-tab-choice "*scratch*"))

(add-hook! after-init #'modern-tab-bar-mode)
(add-hook! after-init #'tabspaces-mode)
