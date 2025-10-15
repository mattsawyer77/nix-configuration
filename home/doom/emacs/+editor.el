;;; +editor.el --- Editor configuration -*- lexical-binding: t; -*-

;;; Basic Settings
(modify-syntax-entry ?_ "w") ;; treat _ as non-word-boundary
(setq read-process-output-max (* 2 1024 1024)  ; For LSP performance
      doom-modeline-vcs-max-length 30
      doom-modeline-height 32
      doom-modeline-persp-name t
      confirm-kill-emacs nil
      mac-command-modifier 'super
      visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow)
      scroll-margin 3)

(setq-default truncate-lines t
              tab-width 2
              scroll-margin 3
              maximum-scroll-margin 0.15
              sh-basic-offset 2)

;; Frame, UI Enhancements
(when (and (display-graphic-p) (featurep :system 'macos))
  (add-hook! after-init #'ultra-scroll-mode))

 ;;; File Mode Associations
(dolist (spec
         '(("\\kops-edit.+yaml$"      . yaml-ts-mode)
           ("\\.tmpl$"                . go-template-mode)
           ("k8s/templates"           . go-template-mode)
           ("kubernetes/templates"    . go-template-mode)
           ("stack.yaml"              . yaml-ts-mode)
           ("package.yaml"            . yaml-ts-mode)
           ("\\.aws/.*"               . toml-ts-mode)
           ("\\.saml2aws"             . toml-ts-mode)
           ("\\.kube/config.*"        . yaml-ts-mode)
           ("\\.cc$"                  . c++-ts-mode)
           ("\\.hpp$"                 . c++-ts-mode)
           ("\\.h$"                   . c++-ts-mode)
           ("SConscript"              . python-mode)
           ("SConstruct"              . python-mode)
           ("go.mod"                  . go-mod-ts-mode)
           ("\\.go$"                  . go-mode)
           ("Makefile"                . makefile-gmake-mode)
           ("\\.libsonnet$"           . jsonnet-mode)
           ("\\.mmd$"                 . mermaid-mode)
           ("\\.mermaid$"             . mermaid-mode)
           ("\\.md$"                  . gfm-mode)
           ("CODEOWNERS$"             . conf-mode)
           ("\\.js$"                  . js-ts-mode)
           ("\\.hujson$"              . jsonc-mode)))
  (add-to-list 'auto-mode-alist spec))

(after! proto-nav-mode
  (add-to-list 'auto-mode-alist '("\\.proto\\'" . proto-nav-mode)))

;; TODO: do we need this or does set-formatter! handle it
;; (delete 'apheleia-mode-alist '(protobuf-mode . clang-format))
(add-hook! protobuf-mode
  (message "setting up buf formatter")
  (set-formatter! 'buf '("buf" "format" "-w" (or buffer-file-name mode-result)) :modes '(protobuf-mode proto-nav-mode)))

 ;;; Format-on-Save Disabled Modes
(dolist (mode '(conf-toml-mode toml-ts-mode yaml-mode yaml-ts-mode cpp-mode c++-ts-mode c++-mode shell-script-mode bash-ts-mode))
  (add-to-list '+format-on-save-disabled-modes mode))

 ;;; ws-butler Exempt Modes (makefiles)
(after! ws-butler
  (dolist (mode '(makefile-mode makefile-gmake-mode makefile-bsdmake-mode))
    (add-to-list 'ws-butler-global-exempt-modes mode t)))

 ;;; Major Mode Hooks
(after! go-mode
  (setq gofmt-command "goimports"))
(add-hook! go-mode     #'+word-wrap-mode #'lsp)
;; (add-hook! go-ts-mode  #'go-mode)
(add-hook! go-mode  #'go-ts-mode)
(add-hook! go-ts-mode  #'lsp)
(add-hook! js-mode #'js-ts-mode)
(add-hook! conf-toml-mode #'lsp)
(add-hook! toml-ts-mode #'lsp)
(add-hook! emacs-lisp-mode #'+word-wrap-mode #'rainbow-mode #'flycheck-mode)
(add-hook! haskell-mode #'lsp #'ormolu-format-on-save-mode (lambda () (flycheck-posframe-mode -1)))
(add-hook! (yaml-mode yaml-ts-mode) (+lsp-optimization-mode -1))
(add-hook! makefile-mode #'+word-wrap-mode
  (lambda ()
    (message "in makefile-mode hook")
    (indent-tabs-mode 1)
    (ws-butler-mode -1)
    (message (if (-contains? local-minor-modes 'ws-butler-mode)
                 "dammit...ws-butler is still active"
               "hooray...ws-butler is dead"))))
(add-hook! js2-mode #'prettier-js-mode)
(add-hook! (protobuf-mode)
           #'display-line-numbers-mode #'flycheck-mode #'+word-wrap-mode)
(add-hook! (rustic-mode rust-ts-mode)
           #'lsp #'+word-wrap-mode (lambda () (lsp-inlay-hints-mode -1))
           (lambda () (setq lsp-enable-symbol-highlighting nil)))
(add-hook! c++-mode 'lsp)
(add-hook! c++-ts-mode 'lsp)

 ;;; Display Line Numbers and Visual Line for Programming Modes
(after! display-line-numbers
  (add-hook! (prog-mode go-template-mode)
    (turn-on-visual-line-mode)
    (display-line-numbers-mode)))

 ;;; Undo, Surround, Projectile, Perspective, Tabs
(after! undo-tree
  (setq undo-tree-auto-save-history t
        undo-tree-history-directory-alist '(("." . "~/.config/emacs/.local/undo")))
  (global-undo-tree-mode 1))

(after! evil-surround (global-evil-surround-mode 1))

(after! projectile
  (setq projectile-project-search-path '("~/workspaces"
                                         "~/workspaces/f5/volterra/ves.io"
                                         "~/workspaces/f5/volterra/ves.io/sre")))

(after! persp
  (setq uniquify-buffer-name-style 'forward))

 ;;; Treemacs
(after! treemacs
  (require 'treemacs-projectile)
  (treemacs-follow-mode 1)
  (treemacs-project-follow-mode 1)
  (treemacs-git-mode 'deferred)
  (setq-default treemacs--width-is-locked nil)
  (setq treemacs-position 'left
        treemacs-project-follow-cleanup t))

(defun sawyer/handle-frame-resize ()
  "Function to execute whenever the frame resizes."
  (let ((new-treemacs-width (/ (frame-pixel-width) 70)))
    (setq treemacs--width-is-locked nil)
    (treemacs-set-width new-treemacs-width)
    (setq treemacs--width-is-locked t)
    (message (format "resized treemacs width to %s" treemacs-width))))

(after! (textsize treemacs)
  (textsize-mode 1)
  (sawyer/handle-frame-resize))

 ;;; Flycheck and Related
(after! flycheck
  (setq flycheck-indication-mode 'left-fringe
        flycheck-relevant-error-other-file-show nil)
  (advice-add 'flycheck-error-list-refresh :around
              (lambda (orig-fun &rest args)
                (apply orig-fun args)
                (when-let ((window (flycheck-get-error-list-window t)))
                  (with-selected-window window
                    (fit-window-to-buffer window 5)))))
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
                   0 t)]))
  (add-hook! prog-mode (flycheck-mode 1)))

(after! (flycheck-posframe lsp-ui)
  (flycheck-posframe-configure-pretty-defaults)
  (setq flycheck-posframe-border-width 8))

 ;;; LSP Settings
(after! lsp-mode
  (delete 'lsp-terraform lsp-client-packages)
  (delete 'lsp-copilot lsp-client-packages)

  (setq lsp-response-timeout 30
        lsp-file-watch-threshold 8000
        lsp-enable-symbol-highlighting nil
        lsp-modeline-diagnostics-scope :file
        lsp-headerline-breadcrumb-enable t
        lsp-headerline-breadcrumb-segments '(path-up-to-project symbols)
        lsp-ui-peek-enable nil
        lsp-ui-doc-enable t
        lsp-ui-doc-position 'at-point
        lsp-ui-doc-border (doom-lighten 'bg 0.1)
        lsp-lens-enable t
        lsp-ui-sideline-enable nil
        lsp-signature-auto-activate t
        lsp-signature-render-documentation t
        lsp-completion-show-detail t
        lsp-completion-show-kind t
        lsp-enable-links t ; fix links
        lsp-ui-imenu-buffer-position 'left
        lsp-nix-nil-formatter ["nixpkgs-fmt"]
        nix-nixfmt-bin "nixpkgs-fmt")

  (setq lsp-file-watch-ignored-directories
        (cl-union lsp-file-watch-ignored-directories
                  '("[/\\\\]\\.?cache[/\\\\]?"
                    "\\.cache.go"
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
                    ;; "vendor"
                    "[/\\\\]target[/\\\\]?"
                    "\\.vscode" "\\.idea" "\\.direnv" "\\.devenv"))))

 ;;; LSP: Rust
(after! rustic
  (setq rustic-lsp-server 'rust-analyzer
        rustic-format-on-save t
        lsp-rust-server 'rust-analyzer
        lsp-rust-analyzer-display-chaining-hints t
        lsp-rust-analyzer-display-parameter-hints t))

(after! (rustic lsp-mode)
  (advice-add #'lsp-eldoc-function :after (lambda (&rest _) (setq lsp--hover-saved-bounds nil)))
  (cl-defmethod lsp-clients-extract-signature-on-hover (contents (_server-id (eql rust-analyzer)))
    (let* ((value (if lsp-use-plists (plist-get contents :value) (gethash "value" contents)))
           (groups (--partition-by (s-blank? it) (s-lines (s-trim value))))
           (sig_group (if (s-equals? "```rust" (car (-third-item groups))) (-third-item groups) (car groups)))
           (sig (--> sig_group
                     (--drop-while (s-equals? "```rust" it) it)
                     (--take-while (not (s-equals? "```" it)) it)
                     (--map (s-trim it) it)
                     (s-join " " it))))
      (lsp--render-element (concat "```rust\n" sig "\n```")))))

(after! (flycheck rustic) (push 'rustic-clippy flycheck-checkers))

 ;;; Org-Mode and Notes
(after! org
  (setq org-export-body-only t)
  (cond
   ((equal (system-name) "SEA-ML-00059144")
    (setq org-agenda-files '("/Users/sawyer/Documents/OneDrive - F5 Networks/notes")))
   ((equal (system-name) "KD21QWDKW7")
    (progn
      (setq org-directory "~/onedrive/notes")
      (setq org-agenda-files '("/Users/m.sawyer/Library/CloudStorage/OneDrive-F5,Inc/notes"))))
   (t
    (setq org-agenda-files '("/Users/sawyer/Library/Mobile Documents/com~apple~CloudDocs/notes")))))

 ;;; Popup Rules
(dolist (rule
         '(("^\\*eww\\*"      :side right :slot 5 :vslot 5 :size 0.5)
           ("^\\*jq-json\\*"  :side right :slot 5 :vslot 5 :size 0.5)
           ("^\\*go-guru-output.*" :side bottom :size 5)
           ;; ("^\\*gpt:\\*"        :side right :slot 5 :vslot 5 :size 0.4))
           (apply #'set-popup-rule! rule))))

 ;;; Terminal, VTerm, EAT
(after! vterm  (setq vterm-shell "/bin/zsh" vterm-tramp-shells nil)
  (set-popup-rule! "^\\*doom:vterm.*" :ignore t))

(after! emacs-eat (setq eat-term-name "xterm-256color"))

 ;;; Language Customization
(after! haskell (setq lsp-haskell-process-path-hie "hie-wrapper"))
(after! ormolu (setq ormolu-reformat-buffer-on-save t))

(after! js2-mode (setq-default js2-basic-offset 2))

 ;;; Highlight Indent Guides
(after! highlight-indent-guides
  (setq highlight-indent-guides-method 'fill
        highlight-indent-guides-auto-enabled nil)
  (add-hook!
    (emacs-lisp-mode haskell-mode json-mode json-ts-mode makefile-mode makefile-gmake-mode makefile-bsdmake-mode ponylang-mode conf-toml-mode toml-ts-mode yaml-mode yaml-ts-mode)
    #'highlight-indent-guides-mode))

 ;;; LSP Booster integration (if present)
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
    (if (and (not test?)
             (not (file-remote-p default-directory))
             lsp-use-plists
             (not (functionp 'json-rpc-connection))
             (executable-find "emacs-lsp-booster"))
        (progn
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))
(after! lsp-mode
  (let ((emacs-lsp-booster-cmd (executable-find "emacs-lsp-booster")))
    (if emacs-lsp-booster-cmd
        (advice-add 'lsp-resolve-final-command :around
                    #'lsp-booster--advice-final-command)
      (message "could not find emacs-lsp-booster on path, falling back to default (non-boosted) lsp-mode"))))

 ;;; Tab Bar & Spaces
(after! modern-tab-bar  (setq modern-tab-bar-separator ""))
(after! tabspaces
  (setq tabspaces-use-filtered-buffers-as-default t
        tabspaces-remove-to-default nil
        tabspaces-exclude-buffers '("^\\*.*" "^ \\*.*")
        tabspaces-include-buffers '("*scratch*")
        tabspaces-initialize-project-with-todo nil
        tab-bar-new-tab-choice "*scratch*"))

(add-hook! after-init #'modern-tab-bar-mode)
(add-hook! after-init #'tabspaces-mode)

 ;;; Other Package Configurations
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
   :preview-key (list :debounce 0.1 'any)))

(after! vertico
  (vertico-multiform-mode 1)
  (setq vertico-multiform-commands
        '((consult-imenu grid)
          (consult-line buffer))))

(after! envrc (envrc-global-mode))

(after! lab
  (setq lab-host "https://gitlab.com"
        lab-token (getenv "GITLAB_API_TOKEN")
        lab-group "12645129")) ; F5/volterra

(when (featurep :system 'macos) (require 'ns-keychain))
(defun sawyer/get-creds (hostname account)
  "get credentials securely"
  (if (fboundp 'ns-keychain-get-generic-password)
      (ns-keychain-get-generic-password hostname account)
    (error "TODO: implement sawyer/get-creds for non-macOS")))

(defun sawyer/add-repomix-to-gptel-context ()
  "If repomix-output.xml exists in the project root, add it to GPTel context using `gptel-context-add-file`."
  (interactive)
  (let* ((project-root
          (cond
           ((and (featurep 'projectile) (fboundp 'projectile-project-root))
            (projectile-project-root))
           ((fboundp 'project-root)
            (when-let ((pr (project-current)))
              (project-root pr)))
           (t
            (locate-dominating-file default-directory ".git"))))
         (repomix-path (and project-root
                            (expand-file-name "repomix-output.xml" project-root))))
    (when (and repomix-path (file-exists-p repomix-path))
      (gptel-context-add-file repomix-path)
      (message "Added %s to GPTel context." repomix-path))))

(defun sawyer/gptel-mode-buffer-local-variables ()
  "Set buffer-local variables for `gptel-mode'."
  (setq-local default-directory
              (expand-file-name "chat" org-directory))
  (set-visited-file-name (concat (file-name-sans-extension
                                  (expand-file-name (buffer-name)))
                                 ".org"))
  (visual-line-mode 1))

(defun sawyer/gptel-mode-in-chat-directory-p ()
  "Return non-nil if buffer is visiting a file in the chat directory."
  (when-let* ((buffer-file-name)
              (chat-dir (expand-file-name "llm-sessions" org-directory)))
    (file-in-directory-p buffer-file-name chat-dir)))

(defun sawyer/gptel-mode ()
  "Enable `gptel-mode' automatically if file is stored in chat directory."
  (when (sawyer/gptel-mode-in-chat-directory-p)
    (gptel-mode 1)
    ;; gptel-mode marks buffer as modified, but doesn't actually modify anything
    (set-buffer-modified-p nil)))

(defun sawyer/gptel-mode-after-response (start end)
  "Save chat buffer and move to next org heading.

                                        START and END indicates the starting and ending position of the LLM response."
  (when (sawyer/gptel-mode-in-chat-directory-p)
    (save-buffer))
  (call-interactively 'gptel-end-of-response))

(after! gptel
  (require 'gptel-integrations)
  (require 'gptel-org)
  (gptel-make-openai "gpt: F5 openwebui"
    :host "f5ai.pd.f5net.com"
    :protocol "https"
    :endpoint "/api/chat/completions"
    :key (sawyer/get-creds "f5gpt-open-webui" "default")
    :stream t
    :models '(o4-mini
              gpt-4.1
              gpt-5))
  (gptel-make-openai "gpt: local openwebui"
    :host "127.0.0.1:3000"
    :protocol "http"
    :endpoint "/api/chat/completions"
    :key (sawyer/get-creds "local-open-webui" "default")
    :stream t
    :models '(o4-mini ;; f5gpt
              gpt-4.1 ;; f5gpt
              gpt-5   ;; f5gpt
              ;; devstral-small-2507-gguf:q3_k_xl ;; llama.cpp
              ;; deepseek-r1-distill-qwen-1.5b-gguf:q8_0 ;; llama.cpp -- useless though?
              ))
  (gptel-make-gh-copilot "Copilot")
  (setq gptel-model 'gpt-4.1
        gptel-use-tools t
        gptel-confirm-tool-calls 'auto
        gptel-include-tool-results 'auto
        gptel-default-mode 'org-mode
        gptel-track-media t
        gptel-org-set-properties t
        gptel-prompt-prefix-alist '((markdown-mode . "# ")
                                    (org-mode . "* ")
                                    (text-mode . "### "))
        gptel-response-prefix-alist '((markdown-mode . "")
                                      (org-mode . "")
                                      (text-mode . ""))
        gptel--system-message "You are a large language model living in Emacs and a helpful assistant. Respond concisely in markdown (github-flavored markdown if including code)."
        gptel-directives '((default
                            . "You are a large language model living in Emacs and a helpful assistant. Respond concisely in markdown (github-flavored markdown if including code).")
                           (programming
                            . "You are a large language model and a careful programmer. Provide code and only code as output without any additional text, prompt or note.")
                           (writing
                            . "You are a large language model and a writing assistant. Respond concisely in markdown format.")
                           (architecture
                            . "You are a large language model and a distributed systems architect. When feasible, respond to questions with discussions of tradeoffs or pros/cons of any architectural points to be debated. Respondi in markdown format (github-flavored markdown if including code."))
        ;; gptel-response-separator "\n\n* "
        ;; gptel-backend (gptel-make-openai "gpt: F5 openwebui"
        ;;                 :host "f5ai.pd.f5net.com"
        ;;                 :protocol "https"
        ;;                 :endpoint "/api/chat/completions"
        ;;                 :key (sawyer/get-creds "f5gpt-open-webui" "default")
        ;;                 :stream t
        ;;                 :models '(o4-mini
        ;;                           gpt-4.1
        ;;                           gpt-5))
        ;; gptel-backend (gptel-make-gh-copilot "Copilot")
        ;; gptel-backend (gptel-make-openai "gpt: local openwebui"
        ;;                 :host "127.0.0.1:3000"
        ;;                 :protocol "http"
        ;;                 :endpoint "/api/chat/completions"
        ;;                 :key (sawyer/get-creds "local-open-webui" "default")
        ;;                 :stream t
        ;;                 :models '(o4-mini ;; f5gpt
        ;;                           gpt-4.1 ;; f5gpt
        ;;                           gpt-5   ;; f5gpt
        ;;                           ;; devstral-small-2507-gguf:q3_k_xl ;; llama.cpp
        ;;                           ;; deepseek-r1-distill-qwen-1.5b-gguf:q8_0 ;; llama.cpp -- useless though?
        ;;                           ))
        )
  ;; (add-hook! gptel-post-stream #'gptel-auto-scroll) ;; might be incompatible with gptel-default-mode: org-mode
  ) ;; gptel

(add-hook! gptel-mode-hook #'sawyer/add-repomix-to-gptel-context)
(add-hook! gptel-mode-hook #'sawyer/gptel-mode-buffer-local-variables)
(add-hook! gptel-post-response #'sawyer/gptel-mode-after-response)
(add-hook! org-mode #'sawyer/gptel-mode)

(after! mcp
  (require 'mcp-hub)
  (add-hook! gptel-mode-hook #'gptel-mcp-connect)
  (setq mcp-hub-servers `(("github" . (:command "github-mcp-server"
                                       :args ("stdio" "--read-only")
                                       :env (:GITHUB_PERSONAL_ACCESS_TOKEN ,(sawyer/get-creds "github-mcp-server" "default"))))
                          ("tree-sitter" . (:command "mcp-server-tree-sitter" :args ("--debug")))
                          ("duckduckgo" . (:command "duckduckgo-mcp-server"))
                          ("nixos" . (:command "mcp-nixos"))
                          ;; ("repomix" . (:command "repomix" :args ("--mcp")))
                          ;; ("filesystem" . (:command "npx" :args ("-y" "@modelcontextprotocol/server-filesystem" ,(getenv "home"))))
                          ;; ("sequential-thinking" . (:command "npx" :args ("-y" "@modelcontextprotocol/server-sequential-thinking")))
                          ;; ("context7" . (:command "npx" :args ("-y" "@upstash/context7-mcp") :env (:default_minimum_tokens "6000")))
                          ))
  )

(after! magit
  (setq auto-revert-check-vc-info t
        auto-revert-interval 30
        magit-refresh-status-buffer nil
        magit-diff-highlight-indentation nil
        magit-diff-highlight-trailing nil
        magit-diff-paint-whitespace nil
        magit-diff-highlight-hunk-body nil
        magit-diff-refine-hunk nil
        git-commit-summary-max-length 100))

(after! (gptel magit)
  (add-hook! magit-mode #'gptel-magit-install))

(after! lsp-treemacs (setq lsp-treemacs-error-list-expand-depth 3))

(after! treesit
  (setq treesit-extra-load-path '("~/.config/emacs/.local/cache/tree-sitter"))
  (setq treesit-language-source-alist
        '((bash        "https://github.com/tree-sitter/tree-sitter-bash")
          (cmake       "https://github.com/uyha/tree-sitter-cmake")
          (c           "https://github.com/tree-sitter/tree-sitter-c")
          (cpp         "https://github.com/tree-sitter/tree-sitter-cpp")
          (css         "https://github.com/tree-sitter/tree-sitter-css")
          (elisp       "https://github.com/Wilfred/tree-sitter-elisp")
          (go          "https://github.com/tree-sitter/tree-sitter-go" "v0.23.4")
          (gomod       "https://github.com/camdencheek/tree-sitter-go-mod" "v1.1.0")
          (html        "https://github.com/tree-sitter/tree-sitter-html")
          (javascript  "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
          (jsdoc       "https://github.com/tree-sitter/tree-sitter-jsdoc" "v0.25.0" src)
          (json        "https://github.com/tree-sitter/tree-sitter-json")
          (make        "https://github.com/alemuller/tree-sitter-make")
          (markdown    "https://github.com/ikatyang/tree-sitter-markdown")
          (nix         "https://github.com/nix-community/tree-sitter-nix" "v0.3.0")
          (protobuf    "https://github.com/coder3101/tree-sitter-proto" "main" "src")
          (python      "https://github.com/tree-sitter/tree-sitter-python")
          (rust        "https://github.com/tree-sitter/tree-sitter-rust" "v0.23.3")
          (toml        "https://github.com/tree-sitter/tree-sitter-toml")
          (tsx         "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (typescript  "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          (yaml        "https://github.com/ikatyang/tree-sitter-yaml")))
  (setq treesit-font-lock-level 4))

 ;;; Example for additional hooks
(add-hook! nim-mode #'lsp)
(add-hook! rfc-mode-hook #'page-break-lines-mode #'writeroom-mode)

 ;;; Package: highlight-indent-guides
(after! highlight-indent-guides
  (setq highlight-indent-guides-method 'fill
        highlight-indent-guides-auto-enabled nil)
  (add-hook!
    (emacs-lisp-mode haskell-mode json-mode json-ts-mode makefile-mode makefile-gmake-mode makefile-bsdmake-mode ponylang-mode conf-toml-mode toml-ts-mode yaml-mode yaml-ts-mode)
    #'highlight-indent-guides-mode))

(require 'vc-git)
(require 'cl-lib)
(defun sawyer/normalize-dirs (dirs)
  "Given a list DIRS of directories (absolute or relative),
 return a sorted, deduplicated list of their absolute paths.
 Non-existent or non-directory entries are dropped."
  (let ((canon
         ;; Step 1: expand each to an absolute path and drop if not a directory
         (delq nil
               (mapcar (lambda (d)
                         (let ((abs (expand-file-name d)))
                           (when (file-directory-p abs)
                             ;; normalize by removing any trailing slash
                             (directory-file-name abs))))
                       dirs))))
    ;; Step 2: remove duplicates (string=) and sort by string<
    (sort (cl-delete-duplicates canon :test #'string=) #'string<)))

;; (defun sawyer/proto-root-dirs ()
;;   "get a list of candidate directories to search for proto files based on
;; the buffer's directory"
;;   (if (and (boundp 'flycheck-protoc-import-path)
;;            (not (eq flycheck-protoc-import-path nil)))
;;       flycheck-protoc-import-path
;;     (sawyer/normalize-dirs (list (vc-git-root (file-name-as-directory
;;                                                (or (buffer-file-name) default-directory)))))))

;; (after! lsp-mode
;;   (add-to-list 'lsp-language-id-configuration '(protobuf-mode . "protobuf"))
;;   ;; protols --include-paths=/path/to/protos,/another/path/to/protos
;;   (lsp-register-client (make-lsp-client
;;                         :new-connection (lsp-stdio-connection (append '("protols") (list (format "--include-paths=%s" (string-join (sawyer/proto-root-dirs) ",")))))
;;                         :activation-fn (lsp-activate-on "protobuf")
;;                         :server-id 'protols)))

(after! jq-mode
  ;; (add-to-list 'load-path "/path/to/jq-mode-dir")
  ;; (autoload 'jq-mode "jq-mode.el"
  ;;     "Major mode for editing jq files" t)
  (add-to-list 'auto-mode-alist '("\\.jq$" . jq-mode))
  )

(defun sawyer/go-generate-test-coverage (&optional callback)
  "Asynchronously generate test coverage for the current Go package and notify on completion.
if CALLBACK is non-nil, call it after coverage is successfully generated."
  (interactive)
  (unless (memq major-mode '(go-mode go-ts-mode))
    (error "Not in a Go buffer"))
  (let* ((file (buffer-file-name))
         (dir (file-name-directory file))
         (default-directory dir)
         (pkg (string-trim
               (with-temp-buffer
                 (call-process "go" nil (current-buffer) nil "list" "-f" "{{.ImportPath}}")
                 (buffer-string))))
         (cov-file (expand-file-name "coverage.out" dir))
         (lcov-file (expand-file-name "lcov.info" dir))
         (cov-cmd (format "go test -coverprofile=%s %s" cov-file pkg))
         (lcov-cmd (format "gcov2lcov --use-absolute-source-path -infile=%s -outfile %s" cov-file lcov-file)))
    ;; Asynchronously run `go test`
    (message "generating test coverage for package: %s" pkg)
    (make-process
     :name "go-test-coverage"
     :buffer "*go-test-coverage*"
     :command (list "sh" "-c" cov-cmd)
     :noquery t
     :sentinel
     (lambda (proc event)
       (when (string= event "finished\n")
         ;; Now run gcov2lcov, also async
         ;; (message "converting go coverage format to lcov")
         (make-process
          :name "gcov2lcov"
          :buffer "*gcov2lcov*"
          :command (list "sh" "-c" lcov-cmd)
          :noquery t
          :sentinel
          (lambda (p e)
            (if (string= e "finished\n")
                (progn
                  (message "lcov generated for package: %s" pkg)
                  (when (fboundp 'notifications-notify)
                    (notifications-notify
                     :title "Go Coverage"
                     :body (format "Generated: %s" lcov-file)))
                  (when callback (funcall callback)))
              (message "gcov2lcov failed: %s (see *gcov2lcov*)" e)))))))))

(defun sawyer/go-generate-test-coverage-and-show ()
  "Enable `cov-mode` if lcov.info is newer than all Go files in the current buffer's directory.
Otherwise, call `sawyer/go-generate-test-coverage` with a callback to enable cov-mode
in the original buffer when complete."
  (interactive)
  (let* ((dir (file-name-directory (or buffer-file-name default-directory)))
         (lcov-file (expand-file-name "lcov.info" dir))
         (go-files (directory-files dir t "\\.go\\'"))
         (origin-buffer (current-buffer)))
    (cl-labels
        ((enable-cov-in-origin ()
           (when (buffer-live-p origin-buffer)
             (with-current-buffer origin-buffer
               (when (fboundp 'cov-mode)
                 (cov-mode 1))))))
      (if (not (file-exists-p lcov-file))
          (sawyer/go-generate-test-coverage #'enable-cov-in-origin)
        (let* ((lcov-mod (nth 5 (file-attributes lcov-file)))
               (newer-go-file
                (cl-some (lambda (file)
                           (let ((mod-time (nth 5 (file-attributes file))))
                             (when (time-less-p lcov-mod mod-time) file)))
                         go-files)))
          (if newer-go-file
              (sawyer/go-generate-test-coverage #'enable-cov-in-origin)
            (enable-cov-in-origin)))))))

;; TRAMP
(setq remote-file-name-inhibit-locks t
      tramp-use-scp-direct-remote-copying t
      remote-file-name-inhibit-auto-save-visited t
      tramp-copy-size-limit (* 1024 1024) ;; 1MB
      tramp-verbose 2)
