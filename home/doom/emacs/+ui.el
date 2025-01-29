;;;  -*- lexical-binding: t; -*-

(setq default-frame-alist
      (append (list
               '(min-height . 1)
               '(height . 45)
               '(min-width . 1)
               '(width . 81)
               '(vertical-scroll-bars . nil)
               '(internal-border-width . 10)
               '(left-fringe . 0)
               '(right-fringe . 0)
               '(tool-bar-lines . 0)
               '(menu-bar-lines . 0)
               )))

;; only supported for emacs-plus (i.e. homebrew patches)
(when (and (featurep :system 'macos) (fboundp 'display-graphic-p))
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))

  ;;; XXX: titlebar shenanigans
  (defun ns-auto-titlebar-set-frame (frame &rest _)
    "Set ns-appearance frame parameter for FRAME to match its background-mode parameter."
    (let ((mode (frame-parameter frame 'background-mode)))
      (modify-frame-parameters frame
                               `((ns-transparent-titlebar . t)
                                 (ns-appearance . ,mode)
                                 ))))

  (defun ns-auto-titlebar-set-all-frames (&rest _)
    "Set ns-appearance frame parameter for all frames to match their background-mode parameter."
    (mapc 'ns-auto-titlebar-set-frame (frame-list)))

  ;; (defun toggle-undecorated-frame ()
  ;;   "Toggle the 'undecorated' parameter and frame maximized state for the current frame."
  ;;   (interactive)
  ;;   (let* ((undecorated-value (frame-parameter nil 'undecorated))
  ;;          (new-value (not undecorated-value)))
  ;;     (set-frame-parameter nil 'undecorated new-value)
  ;;     (toggle-frame-maximized))
  ;;   (message "Undecorated frame setting and maximized state toggled."))
  ;; (toggle-undecorated-frame)
  ;; (map! (:leader :nv :desc "toggle title bar" "t t" #'toggle-undecorated-frame))
  )

(defface sawyer/mono-face
  `((t :inherit default
     ;; :family "Input" :weight normal
     ;; :family "PragmataPro Liga" :weight normal
     :family "PragmataPro Liga 1.1" :weight normal
     ;; :family "Roboto Mono" :weight normal
     ;; :family "JetBrains Mono 1.1" :weight thin
     ;; :family "Victor Mono" :weight thin
     ;; :family "Berkeley Mono"
     ;; :family "Iosevka" :weight extra-light
     ;; :weight thin
     ;; :weight extra-light ;; (a.k.a. ultra-light)
     ;; :weight semi-light ;; (a.k.a. demi-light)
     ;; :weight light
     ;; :weight normal ;; (a.k.a. regular a.k.a. book)
     ;; :weight medium
     ;; :weight semi-bold ;; (a.k.a. demi-bold)
     ;; :weight bold
     ;; :weight ultra-bold ;; (a.k.a. extra-bold)
     ;; :weight heavy ;; (a.k.a. black),
     ;; :weight ultra-heavy
     ))
  "font spec for all monospace text")

(defface sawyer/variable-face
  `((t :inherit default
     ;; :family "Lexend"
     :family "IBM Plex Sans"
     ;; :family "Roboto"
     ;; :family "Fira Sans"
     ;; :family "Avenir Next"
     ;; :weight thin
     ;; :weight extra-light ;; (a.k.a. ultra-light)
     ;; :weight semi-light ;; (a.k.a. demi-light)
     ;; :weight light
     ;; :weight normal ;; (a.k.a. regular a.k.a. book)
     ;; :weight medium
     ;; :weight semi-bold ;; (a.k.a. demi-bold)
     ;; :weight bold
     ;; :weight ultra-bold ;; (a.k.a. extra-bold)
     ;; :weight heavy ;; (a.k.a. black),
     ;; :weight ultra-heavy
     :size 20
     ))
  "font spec for all variable-width text")

;; TODO can height be defined with a var?
(defface code-face
  `((t :inherit sawyer/mono-face
     :height 1.2 ;; for pragmata
     ;; :height 1.1 ;; for jetbrains mono
     ;; :weight extra-light
     ))
  "code-face")
;; TODO: figure out how to make the family dynamic
(defface org-face
  `((t :inherit sawyer/variable-face
     :height 1.1
     :weight normal
     ))
  "org-face")
(defface org-code-face
  `((t :inherit code-face
     :height 0.8
     :weight normal
     ))
  "org-code-face")

;; make minibuffer font smaller
;; (thanks to https://protesilaos.com/codelog/2024-10-17-emacs-remap-minibuffer-face/)
;; XXX: causes too many other weird issues (flycheck, treemacs, etc.)
;; (defvar-local sawyer/minibuffer-font-remap-cookie nil
;;   "The current face remap of `my-minibuffer-set-font'.")
;; (defface sawyer/minibuffer-default
;;   '((t :height 0.8))
;;   "Face for the minibuffer and the Completions.")
;; (defun sawyer/minibuffer-set-font ()
;;   (setq-local sawyer/minibuffer-font-remap-cookie
;;               (face-remap-add-relative 'default 'sawyer/minibuffer-default)))
;; (add-hook 'minibuffer-mode-hook #'sawyer/minibuffer-set-font)
;; end of minibuffer font setup

(defcustom sawyer/baseline-font-size 17.0 "baseline font point size for doom-font and textsize" :type 'number)
(defcustom sawyer/dark-theme 'kanagawa "dark theme used for toggling" :type 'string)
(defcustom sawyer/light-theme 'doom-nord-light "light theme used for toggling" :type 'string)

(setq doom-tokyo-night-brighter-comments nil)
(setq doom-city-lights-brighter-comments t)
(setq doom-feather-brighter-comments t)
(setq doom-challenger-deep-brighter-comments nil)

(defcustom sawyer/kanagawa-colors/default-bg "#17171E" "kanagawa: default background color" :type 'string)
(defcustom sawyer/kanagawa-colors/default-fg "#dcd78a" "kanagawa: default foreground color" :type 'string)
(defcustom sawyer/kanagawa-colors/red "#ff6c6b" "kanagawa: red color" :type 'string)
(defcustom sawyer/kanagawa-colors/green "#98bb6c" "kanagawa: green color" :type 'string)
(defcustom sawyer/kanagawa-colors/brown "#c0a36e" "kanagawa: brown color" :type 'string)
(defcustom sawyer/kanagawa-colors/blue "#7fb4ca" "kanagawa: blue color" :type 'string)
(defcustom sawyer/kanagawa-colors/teal "#7aa89f" "kanagawa: teal color" :type 'string)
(defcustom sawyer/kanagawa-colors/yellow "#e6c384" "kanagawa: yellow color" :type 'string)
(defcustom sawyer/kanagawa-colors/purple "#957fb8" "kanagawa: purple color" :type 'string)
(defcustom sawyer/kanagawa-colors/orange "#da8548" "kanagawa: orange color" :type 'string)
(defcustom sawyer/kanagawa-colors/pink "#d27e99" "kanagawa: pink color" :type 'string)
(defcustom sawyer/kanagawa-colors/gray "#727169" "kanagawa: gray color" :type 'string)
(defface sawyer/kanagawa/org-todo-face       `((t :inherit org-code-face :weight bold :background ,sawyer/kanagawa-colors/blue :foreground "#223249")) "font spec for org todo labels")
(defface sawyer/kanagawa/org-todo-start-face `((t :inherit org-code-face :weight bold :background ,sawyer/kanagawa-colors/green :foreground "#223249")) "font spec for org todo start labels")
(defface sawyer/kanagawa/org-todo-wait-face  `((t :inherit org-code-face :weight bold :background ,sawyer/kanagawa-colors/yellow :foreground "#49443C")) "font spec for org todo wait labels")
(defface sawyer/kanagawa/org-todo-hold-face  `((t :inherit org-code-face :weight bold :background ,sawyer/kanagawa-colors/yellow :foreground "#49443C")) "font spec for org todo hold labels")
(defface sawyer/kanagawa/org-todo-idea-face  `((t :inherit org-code-face :weight bold :background ,sawyer/kanagawa-colors/teal :foreground "#223249")) "font spec for org todo idea labels")
(defface sawyer/kanagawa/org-todo-done-face  `((t :inherit org-code-face :weight bold :background ,sawyer/kanagawa-colors/gray :foreground "#223249")) "font spec for org todo done labels")
(defface sawyer/kanagawa/org-todo-yes-face   `((t :inherit org-code-face :weight bold :background ,sawyer/kanagawa-colors/green :foreground "#223249")) "font spec for org todo yes labels")
(defface sawyer/kanagawa/org-todo-no-face    `((t :inherit org-code-face :weight bold :background ,sawyer/kanagawa-colors/red :foreground "#43242B")) "font spec for org todo no labels")
(defface sawyer/kanagawa/org-todo-kill-face  `((t :inherit org-code-face :weight bold :background ,sawyer/kanagawa-colors/orange :foreground "#43242B")) "font spec for org todo kill labels")

(defun sawyer/kanagawa-setup ()
  "setup faces for kanagawa, since it is not a doom theme"
  (when (-contains? custom-enabled-themes 'kanagawa)
    (message "setting up kanagawa theme...")
    ;; (autothemer-colorize)
    ;; (setq doom-themes--colors '(
    ;;                             (grey katanaGray "black" "black")
    ;;                             (red samuraiRed "red" "red")
    ;;                             (orange surimiOrange "orange" "orange")
    ;;                             (green springGreen "green" "green")
    ;;                             (teal waveDusk1 "teal" "green")
    ;;                             (yellow carpYellow "yellow" "yellow")
    ;;                             (blue springBlue "blue" "blue")
    ;;                             (dark-blue dragonBlue "blue" "blue")
    ;;                             (magenta sakuraPink "magenta" "purple")
    ;;                             (violet oniViolet "magenta" "purple")
    ;;                             (cyan lightBlue "cyan" "blue")
    ;;                             (dark-cyan waveBlue-2 "grey" "grey")))
    ;; `(set-face-attribute 'border nil :background 'sawyer/kanagawa-colors/default-bg)
    ;; `(set-face-attribute 'internal-border nil :background 'sawyer/kanagawa-colors/default-bg)
    ;; `(set-face-attribute 'vertical-border nil :foreground 'sawyer/kanagawa-colors/default-bg)
    ;; `(set-face-attribute 'window-divider nil :foreground 'sawyer/kanagawa-colors/default-bg)
    ;; `(set-face-attribute 'tab-line nil :background (doom-darken 'sawyer/kanagawa-colors/default-bg 0.1))
    ;; (after! centaur-tabs #'centaur-tabs-headline-match)
    ;; (after! tree-sitter
    ;;   `(set-face-attribute 'font-lock-string-face nil :background (doom-lighten 'sawyer/kanagawa-colors/default-bg 0.07))
    ;;   `(set-face-attribute 'tree-sitter-hl-face:function.builtin nil :background 'sawyer/kanagawa-colors/default-bg)
    ;;   `(set-face-attribute 'tree-sitter-hl-face:function.call nil :background 'sawyer/kanagawa-colors/default-bg)
    ;;   `(set-face-attribute 'tree-sitter-hl-face:variable nil :background 'sawyer/kanagawa-colors/default-bg))
    ;; (after! (lsp flycheck)
    ;;   `(set-face-attribute 'flycheck-warning nil :underline '(:style wave :color 'sawyer/kanagawa-colors/orange) :background 'sawyer/kanagawa-colors/default-bg)
    ;;   `(set-face-attribute 'flycheck-error nil :underline '(:style wave :color 'sawyer/kanagawa-colors/red) :background 'sawyer/kanagawa-colors/default-bg)
    ;;   )
    ;; (after! org
    ;;   `(set-face-attribute 'org-document-title nil :foreground 'sawyer/kanagawa-colors/red)
    ;;   `(set-face-attribute 'org-document-info nil :foreground 'sawyer/kanagawa-colors/red)
    ;;   `(set-face-attribute 'org-document-info-keyword nil :background (doom-lighten 'sawyer/kanagawa-colors/default-bg 0.1) :foreground 'sawyer/kanagawa-colors/blue :height 0.8)
    ;;   )

    (setq org-modern-todo-faces
          '(
            ("TODO" sawyer/kanagawa/org-todo-face)
            ("STRT" sawyer/kanagawa/org-todo-start-face)
            ("WAIT" sawyer/kanagawa/org-todo-wait-face)
            ("HOLD" sawyer/kanagawa/org-todo-hold-face)
            ("IDEA" sawyer/kanagawa/org-todo-idea-face)
            ("DONE" sawyer/kanagawa/org-todo-done-face)
            ("YES"  sawyer/kanagawa/org-todo-yes-face)
            ("NO"   sawyer/kanagawa/org-todo-no-face)
            ("KILL" sawyer/kanagawa/org-todo-kill-face)))

    (message "kanagawa theme setup.")
    ))
(after! kanagawa-theme
  ;; (require 'autothemer)
  ;; TODO: query colors from the theme somehow
  (sawyer/kanagawa-setup)
  (add-hook! doom-load-theme #'sawyer/kanagawa-setup))

(defun sawyer/light-switch ()
  "toggle between light and dark themes"
  (interactive)
  (if (eq doom-theme sawyer/light-theme)
      (load-theme sawyer/dark-theme 't)
    (load-theme sawyer/light-theme 't))
  )
(if (and (featurep :system 'macos) (fboundp 'mac-application-state))
    ;; only works on macport emacs
    (progn
      (defun sawyer/macos-appearance-sync ()
        "automatically set the theme to dark or light
     based on the macOS appearance settings (default to dark theme)"
        (let ((appearance (plist-get (mac-application-state) :appearance)))
          ;; check for light mode
          (if (string-equal appearance "NSAppearanceNameAqua")
              (progn
                (message "setting theme to light-theme")
                (load-theme sawyer/light-theme 't))
            (progn
              (message "setting theme to dark-theme")
              (load-theme sawyer/dark-theme't)))))
      (add-hook! doom-after-init #'sawyer/macos-appearance-sync)
      (add-hook! mac-effective-appearance-change #'sawyer/macos-appearance-sync))
  (progn
    (map! :after general
          :map general-override-mode-map
          :nv "<f7>" #'sawyer/light-switch))
  )

(setq doom-font
      (font-spec
       :family (face-attribute 'sawyer/mono-face :family)
       :size sawyer/baseline-font-size
       ;; :weight 'thin
       ))
(setq doom-variable-pitch-font
      (font-spec
       :family (face-attribute 'sawyer/variable-face :family)
       :size 18
       :weight 'normal
       ))

;; disable solaire mode on GUI
;; (after! solaire-mode
;;   (when (display-graphic-p)
;;     (solaire-global-mode -1)))

(after! doom-themes
  (setq doom-themes-enable-italic nil)
  )

(after! tree-sitter
  (custom-set-faces!
    `(tree-sitter-hl-face:property :slant normal :inherit font-lock-constant-face)
    `(tree-sitter-hl-face:punctuation :inherit font-lock-)
    )
  )

(after! lsp-ui
  (custom-set-faces!
    `(lsp-flycheck-warning-unnecessary-category :inherit default :background ,(doom-color 'bg) :foreground warning)
    `(lsp-lens-face :foreground ,(doom-darken 'fg 0.1) :height 0.8)
    `(lsp-ui-doc-header :foreground ,(doom-color 'fg) :background ,(doom-color 'bg))
    )
  )

(after! consult
  (custom-set-faces!
    `(consult-grep-context :inherit font-lock-comment-face)
    `(consult-help :inherit font-lock-doc-face)
    )
  )

(after! doom-modeline
  (setq doom-modeline-buffer-file-name-style 'relative-to-project)
  (setq doom-challenger-deep-padded-modeline t)
  (setq doom-modeline-major-mode-icon t)
  )

(after! hl-todo
  (setq hl-todo-keyword-faces
        `(("TODO"  . ,(face-foreground 'warning))
          ("FIXME" . ,(face-foreground 'error))
          ("XXX" . ,(face-foreground 'error))
          ("NOTE"  . ,(face-foreground 'success))))
  )

(after! treemacs
  (setq treemacs-user-mode-line-format 'none)
  (setq doom-themes-treemacs-enable-variable-pitch t)
  (custom-set-faces!
    `(treemacs-window-background-face :height 1.0)
    `(treemacs-root-face
      :inherit treemacs-window-background-face
      :height 1.2
      :weight medium
      :slant normal
      :family ,(face-attribute 'sawyer/variable-face :family)
      )
    `(treemacs-directory-collapsed-face :weight medium :slant normal :height 1.0 :family ,(face-attribute 'sawyer/variable-face :family))
    `(treemacs-directory-face :foreground ,(doom-darken 'fg 0.2) :weight medium :slant normal :height 1.0 :family ,(face-attribute 'sawyer/variable-face :family))
    `(treemacs-file-face :foreground ,(doom-darken 'fg 0.2) :weight medium :slant normal :height 1.0 :family ,(face-attribute 'sawyer/variable-face :family))
    `(treemacs-fringe-indicator-face :weight medium :slant normal :height 1.0 :family ,(face-attribute 'sawyer/variable-face :family))
    `(treemacs-git-added-face :weight medium :slant normal :height 1.0 :family ,(face-attribute 'sawyer/variable-face :family))
    `(treemacs-git-commit-diff-face :family ,(face-attribute 'sawyer/variable-face :family))
    `(treemacs-git-conflict-face :weight medium :slant normal :height 1.0 :family ,(face-attribute 'sawyer/variable-face :family))
    `(treemacs-git-ignored-face :weight medium :slant normal :height 1.0 :family ,(face-attribute 'sawyer/variable-face :family))
    `(treemacs-git-modified-face :weight medium :slant normal :height 1.0 :family ,(face-attribute 'sawyer/variable-face :family))
    `(treemacs-git-renamed-face :weight medium :slant normal :height 1.0 :family ,(face-attribute 'sawyer/variable-face :family))
    `(treemacs-git-unmodified-face :foreground ,(doom-darken 'fg 0.2) :weight medium :slant normal :height 1.0 :family ,(face-attribute 'sawyer/variable-face :family))
    `(treemacs-git-untracked-face :weight medium :slant normal :height 1.0 :family ,(face-attribute 'sawyer/variable-face :family))
    `(treemacs-header-button-face :family ,(face-attribute 'sawyer/variable-face :family))
    `(treemacs-help-column-face :weight medium :slant normal :height 1.0 :family ,(face-attribute 'sawyer/variable-face :family))
    `(treemacs-help-title-face :weight medium :slant normal :height 1.0 :family ,(face-attribute 'sawyer/variable-face :family))
    `(treemacs-marked-file-face :family ,(face-attribute 'sawyer/variable-face :family))
    `(treemacs-on-failure-pulse-face :weight medium :slant normal :height 1.0 :family ,(face-attribute 'sawyer/variable-face :family))
    `(treemacs-on-success-pulse-face :weight medium :slant normal :height 1.0 :family ,(face-attribute 'sawyer/variable-face :family))
    `(treemacs-peek-mode-indicator-face :family ,(face-attribute 'sawyer/variable-face :family))
    `(treemacs-tags-face :weight medium :slant normal :height 1.0 :family ,(face-attribute 'sawyer/variable-face :family))
    `(treemacs-term-node-face :weight medium :slant normal :height 1.0 :family ,(face-attribute 'sawyer/variable-face :family))
    `(treemacs-nerd-icons-file-face :height 1.0)
    `(doom-themes-treemacs-root-face :foreground ,(face-attribute 'treemacs-root-face :foreground) :background ,(face-attribute 'default :background))
    )
  )

(add-hook! (prog-mode
            text-mode
            conf-toml-mode
            conf-space-mode
            gitignore-mode
            go-template-mode
            protobuf-mode
            go-mode
            )
  (face-remap-add-relative 'default 'code-face)
  ;; (setq-local default-text-properties '(line-spacing 0.3 line-height 1.3))
  (display-line-numbers-mode 1)
  )

;; XXX: not working
;; (add-hook! treemacs-mode
;;   (set-face-attribute 'button nil
;;                       :family '(face-attribute 'sawyer/variable-face :family)
;;                       :background ,(doom-color 'bg)
;;                       :underline nil
;;                       :box
;;                       ))

;; (add-hook! (prog-mode
;;             text-mode
;;             conf-toml-mode
;;             conf-space-mode
;;             gitignore-mode
;;             haskell-cabal-mode
;;             )
;;   (face-remap-add-relative 'default 'code-face)
;;   (face-remap-add-relative 'solaire-default-face 'code-face)
;;   ;; (setq-local default-text-properties '(line-spacing 0.2 line-height 1.1))
;;   (hl-line-mode 0)
;;   )

(after! flycheck-posframe
  (custom-set-faces!
    `(flycheck-posframe-background-face :height 0.9 :background ,(doom-lighten 'bg 0.2))
    `(flycheck-posframe-face :height 0.9 :background ,(doom-lighten 'bg 0.1) :family ,(face-attribute 'sawyer/variable-face :family))
    `(flycheck-posframe-info-face :height 0.9 :family ,(face-attribute 'sawyer/variable-face :family))
    `(flycheck-posframe-error-face :height 0.9 :family ,(face-attribute 'sawyer/variable-face :family))
    `(flycheck-posframe-border-face :height 0.9 :family ,(face-attribute 'sawyer/variable-face :family))
    `(flycheck-posframe-warning-face :height 0.9 :family ,(face-attribute 'sawyer/variable-face :family))
    `(flycheck-posframe-background-face :height 0.9 :family ,(face-attribute 'sawyer/variable-face :family))
    )
  )

;; (after! flycheck
;;   (custom-set-faces!
;;     ;; XXX: not working well
;;     ;; `(flycheck-error-list-error-message :height 0.8)
;;     ;; `(flycheck-error :height 0.8)
;;     ;; `(flycheck-info :height 0.8)
;;     ;; `(flycheck-warning :height 0.8)
;;     ;; `(flycheck-overlay :height 1.0)
;;     ;; `(lsp-flycheck-error-unnecessary-category :height 1.0)
;;     ;; `(flycheck-delimited-error :height 0.8)
;;     ;; `(flycheck-error-delimiter :height 0.8)
;;     ;; `(flycheck-error-list-checker-name :height 0.8)
;;     ;; `(flycheck-error-list-column-number :height 0.8)
;;     ;; `(flycheck-error-list-error :height 0.8)
;;     ;; `(flycheck-error-list-error-message :height 0.8)
;;     ;; `(flycheck-error-list-filename :height 0.8)
;;     ;; `(flycheck-error-list-highlight :background ,(doom-lighten 'bg 0.1) :height 0.8)
;;     ;; `(flycheck-error-list-id :height 0.8)
;;     ;; `(flycheck-error-list-id-with-explainer :height 0.8)
;;     ;; `(flycheck-error-list-info :height 0.8)
;;     ;; `(flycheck-error-list-line-number :height 0.8)
;;     ;; `(flycheck-error-list-warning :height 0.8)
;;     ;; `(flycheck-fringe-error :height 0.8)
;;     ;; `(flycheck-fringe-info :height 0.8)
;;     ;; `(flycheck-fringe-warning :height 0.8)
;;     ;; `(flycheck-verify-select-checker :height 1.0)
;;     ))

;; (add-hook! flycheck-mode
;;   (setq-local default-text-properties '(line-spacing 0.0 line-height 0.7))
;;   (when (cl-find-if (lambda (theme) (eq (string-match-p "doom-" (symbol-name theme)) 0)) custom-enabled-themes)
;;     (custom-set-faces!
;;       `(flycheck-error :background ,(doom-darken (doom-color 'red) 0.3) :foreground "white")
;;       `(flycheck-info :background ,(doom-darken (doom-color 'green) 0.3) :foreground "green")
;;       `(flycheck-warning :background ,(doom-darken (doom-color 'yellow) 0.5) :foreground "white")
;;       )))

(after! org
  (custom-set-faces!
    `(org-table :family ,(face-attribute 'sawyer/mono-face :family) :height 1.0)
    `(org-block :inherit 'org-code-face :background ,(doom-lighten 'bg 0.1))
    `(org-block-begin-line :inherit 'org-face :background ,(doom-lighten 'bg 0.05) :foreground ,(doom-lighten 'bg 0.3) :height 0.8)
    `(org-block-end-line :inherit 'org-face :background ,(doom-lighten 'bg 0.05) :foreground ,(doom-lighten 'bg 0.3) :height 0.8)
    `(org-formula :inherit 'org-code-face :background ,(doom-color 'bg))
    `(org-quote :inherit 'org-face :background ,(doom-lighten 'bg 0.1) :slant italic)
    `(org-tag :foreground ,(doom-blend (doom-color 'magenta) (doom-color 'grey) 0.4))
    `(org-modern-symbol :foreground ,(doom-color 'red))
    `(org-level-8 :family ,(face-attribute 'sawyer/variable-face :family) :height 0.9)
    `(org-level-7 :family ,(face-attribute 'sawyer/variable-face :family) :height 0.9)
    `(org-level-6 :family ,(face-attribute 'sawyer/variable-face :family) :height 0.9)
    `(org-level-5 :family ,(face-attribute 'sawyer/variable-face :family) :height 0.9)
    `(org-level-4 :family ,(face-attribute 'sawyer/variable-face :family) :height 0.9 :weight bold)
    `(org-level-3 :family ,(face-attribute 'sawyer/variable-face :family) :height 1.0 :weight normal)
    `(org-level-2 :family ,(face-attribute 'sawyer/variable-face :family) :height 1.05 :weight bold)
    `(org-level-1 :family ,(face-attribute 'sawyer/variable-face :family) :height 1.2 :weight normal)
    `(org-document-title :inherit 'org-face :height 1.1 :weight bold)
    ;; make leading hidden indentation consistent
    `(org-hide :family ,(face-attribute 'sawyer/mono-face :family))
    `(org-checkbox :family "Fira Code" :height 1.2 :weight normal)
    )
  )

(add-hook! org-mode
  (set-face-attribute 'default nil :family '(face-attribute 'sawyer/variable-face :family))
  ;;   (setq-local default-text-properties '(line-spacing 0.4 line-height 1.0))
  (face-remap-add-relative 'solaire-default-face 'org-face)
  (face-remap-add-relative 'default 'org-face)
  (face-remap-add-relative 'org-code 'org-code-face)
  ;;   (setq-local left-margin-width 5)
  ;;   (setq-local right-margin-width 5)
  (setq-local display-line-numbers-type nil)
  (display-line-numbers-mode -1)
  ;;   (setq-local header-line-format " ")
  ;;   (setq-local internal-border-width 32)
  ;;   (set-window-buffer nil (current-buffer))
  (auto-save-mode t)
  ;;   ;; (when (fboundp '+zen/toggle)
  ;;   ;;   (+zen/toggle 1))
  ;;   ;; (setq
  ;;   ;;  ;; Edit settings
  ;;   ;;  org-auto-align-tags nil
  ;;   ;;  org-tags-column 0
  ;;   ;;  org-catch-invisible-edits 'show-and-error
  ;;   ;;  org-special-ctrl-a/e t
  ;;   ;;  org-insert-heading-respect-content t

  ;;   ;;  ;; Org styling, hide markup etc.
  ;;   ;;  org-hide-emphasis-markers t
  ;;   ;;  org-pretty-entities t
  ;;   ;;  org-ellipsis "…"

  ;;   ;;  ;; Agenda styling
  ;;   ;;  org-agenda-block-separator ?─
  ;;   ;;  org-agenda-time-grid
  ;;   ;;  '((daily today require-timed)
  ;;   ;;    (800 1000 1200 1400 1600 1800 2000)
  ;;   ;;    " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
  ;;   ;;  org-agenda-current-time-string
  ;;   ;;  "⭠ now ─────────────────────────────────────────────────")
  )

(after! tree-sitter
  (custom-theme-set-faces! 'doom-horizon
    `(tree-sitter-hl-face:property :foreground ,(doom-darken (doom-color 'fg) 0.1))
    `(tree-sitter-hl-face:operator :foreground ,(doom-darken (doom-color 'red) 0.1))
    `(tree-sitter-hl-face:keyword :inherit font-lock-keyword-face)
    `(tree-sitter-hl-face:string :inherit font-lock-string-face)
    `(tree-sitter-hl-face:constant.builtin :weight bold)
    `(tree-sitter-hl-face:comment :inherit font-lock-comment-face)
    `(tree-sitter-hl-face:variable :inherit font-lock-variable-use-face)
    `(tree-sitter-hl-face:function :weight bold :foreground ,(doom-color 'cyan))
    `(tree-sitter-hl-face:function.call :foreground ,(doom-color 'cyan))
    )
  (custom-theme-set-faces! 'doom-tomorrow-night
    `(tree-sitter-hl-face:punctuation :foreground ,(doom-darken (doom-color 'red) 0.2) :weight bold)
    `(tree-sitter-hl-face:punctuation.bracket :foreground ,(doom-darken (doom-color 'red) 0.2) :weight bold)
    `(tree-sitter-hl-face:punctuation.special :foreground ,(doom-darken (doom-color 'red) 0.2) :weight bold)
    `(tree-sitter-hl-face:punctuation.delimiter :foreground ,(doom-darken (doom-color 'red) 0.2) :weight bold)
    `(tree-sitter-hl-face:constant :foreground ,(doom-color 'blue))
    ;; `(tree-sitter-hl-face:comment :inherit font-lock-comment-face)
    )
  (custom-theme-set-faces! 'doom-ayu-mirage
    `(tree-sitter-hl-face:type :foreground ,(doom-color 'red) :weight bold)
    `(tree-sitter-hl-face:constructor :foreground ,(doom-lighten 'red 0.3) :weight bold)
    `(tree-sitter-hl-face:method.call :weight bold)
    `(tree-sitter-hl-face:keyword :foreground ,(doom-darken (doom-color 'cyan) 0.1) :weight bold)
    `(tree-sitter-hl-face:function :foreground ,(doom-lighten (doom-color 'cyan) 0.1) :weight bold)
    `(tree-sitter-hl-face:punctuation :foreground ,(doom-darken (doom-color 'cyan) 0.2) :weight bold)
    `(tree-sitter-hl-face:punctuation.bracket :foreground ,(doom-darken (doom-color 'cyan) 0.2) :weight bold)
    `(tree-sitter-hl-face:punctuation.special :foreground ,(doom-darken (doom-color 'cyan) 0.2) :weight bold)
    `(tree-sitter-hl-face:punctuation.delimiter :foreground ,(doom-darken (doom-color 'cyan) 0.2) :weight bold)
    `(tree-sitter-hl-face:operator :foreground ,(doom-darken (doom-color 'cyan) 0.2) :weight bold)
    `(tree-sitter-hl-face:constant :foreground ,(doom-color 'blue) :weight bold)
    `(tree-sitter-hl-face:comment :foreground ,(doom-blend (doom-color 'blue) "#888888" 0.4))
    `(tree-sitter-hl-face:variable :foreground ,(doom-blend (doom-color 'blue) "#aaaaaa" 0.2) :weight bold)
    `(tree-sitter-hl-face:string :background ,(doom-lighten (doom-color 'bg) 0.1) :foreground ,(doom-blend (doom-color 'blue) "#888888" 0.5))
    )
  (custom-theme-set-faces! 'doom-snazzy
    `(tree-sitter-hl-face:punctuation :foreground ,(doom-darken (doom-color 'red) 0.2))
    `(tree-sitter-hl-face:punctuation.bracket :foreground ,(doom-darken (doom-color 'red) 0.2))
    `(tree-sitter-hl-face:punctuation.special :foreground ,(doom-darken (doom-color 'red) 0.2))
    `(tree-sitter-hl-face:punctuation.delimiter :foreground ,(doom-darken (doom-color 'red) 0.2))
    `(tree-sitter-hl-face:operator :foreground ,(doom-darken (doom-color 'red) 0.2))
    ;; `(tree-sitter-hl-face:variable :foreground ,(doom-blend 'red 'white 0.6))
    `(tree-sitter-hl-face:function.method :weight bold)
    `(tree-sitter-hl-face:function.call :weight bold)
    `(tree-sitter-hl-face:function :weight bold)
    `(tree-sitter-hl-face:property :weight bold)
    `(tree-sitter-hl-face:doc :foreground ,(doom-color 'blue))
    )
  (custom-theme-set-faces! '(doom-monokai-octagon doom-monokai-spectrum)
    ;; `(tree-sitter-hl-face:comment :inherit font-lock-comment-face)
    `(tree-sitter-hl-face:punctuation :foreground ,(doom-darken (doom-color 'red) 0.2))
    `(tree-sitter-hl-face:punctuation.bracket :foreground ,(doom-darken (doom-color 'red) 0.2))
    `(tree-sitter-hl-face:punctuation.special :foreground ,(doom-darken (doom-color 'red) 0.2))
    `(tree-sitter-hl-face:punctuation.delimiter :foreground ,(doom-darken (doom-color 'red) 0.2))
    `(tree-sitter-hl-face:operator :foreground ,(doom-darken (doom-color 'red) 0.2))
    `(tree-sitter-hl-face:variable :foreground ,(doom-blend 'cyan 'white 0.6))

    )
  (custom-theme-set-faces! 'doom-tokyo-night
    `(tree-sitter-hl-face:comment :inherit font-lock-comment-face)
    `(tree-sitter-hl-face:doc :inherit font-lock-comment-face :foreground ,(doom-blend (doom-color 'blue) "#888888" 0.8))
    `(tree-sitter-hl-face:keyword :inherit font-lock-keyword-face :weight bold)
    `(tree-sitter-hl-face:punctuation :foreground ,(doom-darken (doom-color 'red) 0.2))
    `(tree-sitter-hl-face:punctuation.bracket :foreground ,(doom-darken (doom-color 'red) 0.2))
    `(tree-sitter-hl-face:punctuation.special :foreground ,(doom-darken (doom-color 'red) 0.2))
    `(tree-sitter-hl-face:punctuation.delimiter :foreground ,(doom-darken (doom-color 'red) 0.2))
    `(tree-sitter-hl-face:operator :foreground ,(doom-darken (doom-color 'red) 0.2))
    `(tree-sitter-hl-face:function.method :inherit font-lock-function-name-face :weight bold)
    `(tree-sitter-hl-face:function.call :inherit font-lock-function-name-face :weight bold)
    `(tree-sitter-hl-face:function :inherit font-lock-function-name-face :weight bold)
    ;; `(tree-sitter-hl-face:variable :foreground ,(doom-blend 'red 'white 0.6))
    ;; `(tree-sitter-hl-face:variable.parameter :foreground ,(doom-blend 'red 'white 0.6))
    ;; `(tree-sitter-hl-face:property :foreground ,(doom-blend 'red 'white 0.6))
    `(tree-sitter-hl-face:property.definition :weight bold)
    `(tree-sitter-hl-face:number :inherit highlight-numbers-number)
    `(tree-sitter-hl-face:type :inherit font-lock-type-face :weight bold)
    )
  (custom-theme-set-faces! 'doom-nord-light
    ;; `(tree-sitter-hl-face:comment :inherit font-lock-comment-face)
    `(tree-sitter-hl-face:keyword :weight bold :foreground ,(doom-color 'orange))
    ;; `(tree-sitter-hl-face:property :inherit font-lock-variable-name-face)
    `(tree-sitter-hl-face:punctuation :foreground ,(doom-darken (doom-color 'red) 0.2))
    `(tree-sitter-hl-face:punctuation.bracket :foreground ,(doom-darken (doom-color 'red) 0.2))
    `(tree-sitter-hl-face:punctuation.special :foreground ,(doom-darken (doom-color 'red) 0.2))
    `(tree-sitter-hl-face:punctuation.delimiter :foreground ,(doom-darken (doom-color 'red) 0.2))
    `(tree-sitter-hl-face:operator :foreground ,(doom-darken (doom-color 'red) 0.2))
    `(tree-sitter-hl-face:number :inherit highlight-numbers-number)
    ;; `(tree-sitter-hl-face:type :inherit font-lock-type-face)
    )
  (custom-theme-set-faces! 'doom-monokai-pro
    `(tree-sitter-hl-face:variable :foreground ,(doom-lighten (doom-color 'cyan) 0.5))
    `(tree-sitter-hl-face:punctuation :foreground ,(doom-darken (doom-color 'orange) 0.2))
    `(tree-sitter-hl-face:punctuation.bracket :foreground ,(doom-darken (doom-color 'orange) 0.2))
    `(tree-sitter-hl-face:punctuation.special :foreground ,(doom-darken (doom-color 'orange) 0.2))
    `(tree-sitter-hl-face:punctuation.delimiter :foreground ,(doom-darken (doom-color 'orange) 0.2))
    `(treemacs-git-ignored-face :foreground ,(doom-color 'magenta))
    `(treemacs-git-untracked-face :foreground ,(doom-color 'grey))
    `(treemacs-git-modified-face :foreground ,(doom-color 'cyan))
    )
  )

;; (after! ccls
;;   (custom-set-faces!
;;     `(ccls-skipped-range-face :inherit 'font-lock-comment-face)
;;     )
;;   )

(add-hook! elisp #'rainbow-mode 1)

(use-package! info-colors
  :commands (info-colors-fontify-node))
(add-hook 'Info-selection-hook 'info-colors-fontify-node)

;; (modify-all-frames-parameters
;;  '((right-divider-width . 16)
;;    (internal-border-width . 16)))
;; (dolist (face '(window-divider
;;                 window-divider-first-pixel
;;                 window-divider-last-pixel))
;;   (face-spec-reset-face face)
;;   (set-face-foreground face (face-attribute 'default :background)))
;; (set-face-background 'fringe (face-attribute 'default :background))
(setq
 ;;  ;; Edit settings
 org-auto-align-tags nil
 org-tags-column 0
 org-fold-catch-invisible-edits 'show-and-error
 org-special-ctrl-a/e t
 org-insert-heading-respect-content t

 ;;  ;; Org styling, hide markup etc.
 org-hide-emphasis-markers t
 org-pretty-entities t
 org-ellipsis "…"

 ;;  ;; Agenda styling
 ;;  org-agenda-block-separator ?─
 ;;  org-agenda-time-grid
 ;;  '((daily today require-timed)
 ;;    (800 1000 1200 1400 1600 1800 2000)
 ;;    " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
 ;;  org-agenda-current-time-string
 ;;  "⭠ now ─────────────────────────────────────────────────"
 )
(add-hook 'org-mode-hook #'org-modern-mode)
;; (add-hook 'org-agenda-finalize-hook #'org-modern-agenda)

;; (after! rustic
;;   (custom-set-faces!
;;     `(lsp-rust-analyzer-inlay-face :inherit font-lock-comment-face
;;                                    :weight light
;;                                    :height 0.8)
;;     )
;;   )

(add-hook! highlight-indent-guides-mode
  (custom-set-faces!
    `(highlight-indent-guides-odd-face :inherit default
      :background ,(doom-lighten 'bg 0.05))
    `(highlight-indent-guides-even-face :inherit default
      :background ,(doom-lighten 'bg 0.08))
    )
  )

(add-hook! elisp #'rainbow-mode 1)

(after! vertico-posframe
  (setq vertico-posframe-border-width 10)
  (custom-set-faces!
    `(vertico-posframe-border :background ,(doom-color 'bg))
    )
  )

(add-hook! writeroom-mode
  (setq-default +zen-text-scale 1)
  (setq-default writeroom-width 100))

;; NOTE: to get the current monitor's dimensions:
;; (textsize--monitor-size-mm (selected-frame))
(when (display-graphic-p)
  (after! textsize
    (setq textsize-default-points sawyer/baseline-font-size)
    ;; use monitor size instead of pixel pitch
    (setq textsize-pixel-pitch-thresholds nil)
    ;; NOTE: the following alist must be sorted by the key (mm size)
    (setq textsize-monitor-size-thresholds
          '(
            (286 . -2)    ;; 14" MBP
            (345 . 0.0)    ;; 16" MBP
            (531 . -0.1)   ;; 24" 4K (full-res)
            (802 . 0)    ;; 34" superwide
            ))
    )
  (add-hook! after-init #'textsize-mode)
  )

;; org-modern-todo-faces seems to only work by inheriting faces, so define these as custom faces
;; (after! (doom-themes org-modern)
;;   (defface sawyer/org-todo-face       `((t :inherit org-code-face :weight bold :background ,(doom-color 'blue) :foreground ,(doom-color 'bg))) "font spec for org todo labels")
;;   (defface sawyer/org-todo-start-face `((t :inherit org-code-face :weight bold :background ,(doom-color 'green) :foreground ,(doom-color 'bg))) "font spec for org todo start labels")
;;   (defface sawyer/org-todo-wait-face  `((t :inherit org-code-face :weight bold :background ,(doom-color 'yellow) :foreground ,(doom-color 'bg))) "font spec for org todo wait labels")
;;   (defface sawyer/org-todo-hold-face  `((t :inherit org-code-face :weight bold :background ,(doom-color 'yellow) :foreground ,(doom-color 'bg))) "font spec for org todo hold labels")
;;   (defface sawyer/org-todo-idea-face  `((t :inherit org-code-face :weight bold :background ,(doom-color 'cyan) :foreground ,(doom-color 'bg))) "font spec for org todo idea labels")
;;   (defface sawyer/org-todo-done-face  `((t :inherit org-code-face :weight bold :background ,(doom-color 'grey) :foreground ,(doom-color 'bg))) "font spec for org todo done labels")
;;   (defface sawyer/org-todo-yes-face   `((t :inherit org-code-face :weight bold :background ,(doom-color 'green) :foreground ,(doom-color 'bg))) "font spec for org todo yes labels")
;;   (defface sawyer/org-todo-no-face    `((t :inherit org-code-face :weight bold :background ,(doom-color 'red) :foreground ,(doom-color 'bg))) "font spec for org todo no labels")
;;   (defface sawyer/org-todo-kill-face  `((t :inherit org-code-face :weight bold :background ,(doom-color 'grey) :foreground ,(doom-color 'bg))) "font spec for org todo kill labels")
;;   (defun sawyer/doom-org-modern-setup ()
;;     "setup org-modern-todo faces for doom-themes"
;;     ;; if we have loaded a doom-* theme, we can use doom-color to set org-todo colors
;;     (when (cl-find-if (lambda (theme) (eq (string-match-p "doom-" (symbol-name theme)) 0)) custom-enabled-themes)
;;       (setq org-modern-todo-faces
;;             '(
;;               ("TODO" sawyer/org-todo-face)
;;               ("STRT" sawyer/org-todo-start-face)
;;               ("WAIT" sawyer/org-todo-wait-face)
;;               ("HOLD" sawyer/org-todo-hold-face)
;;               ("IDEA" sawyer/org-todo-idea-face)
;;               ("DONE" sawyer/org-todo-done-face)
;;               ("YES" sawyer/org-todo-yes-face)
;;               ("NO" sawyer/org-todo-no-face)
;;               ("KILL" sawyer/org-todo-kill-face)))))
;;   (sawyer/doom-org-modern-setup)
;;   (add-hook! doom-load-theme #'sawyer/doom-org-modern-setup))

;; (after! org-modern
;;   ;; table styles are still kinda messed up: https://github.com/minad/org-modern/issues/5
;;   (setq org-modern-table nil)
;;   (custom-set-faces!
;;     `(org-modern-date-active :inherit org-modern-done :family ,(face-attribute 'sawyer/mono-face :family))
;;     `(org-modern-date-inactive :inherit org-modern-done :family ,(face-attribute 'sawyer/mono-face :family)))
;;   (add-hook! org-mode #'org-modern-mode)
;;   (add-hook! org-agenda-finalize #'org-modern-agenda)
;;   )

;; borrow org styles for markdown
;; (after! (org markdown-mode)
;;     ;; '(markdown-header-delimiter-face :foreground "#616161" :height 0.9)
;;   (custom-set-faces!
;;     `(markdown-header-face-1 :family ,(face-attribute 'org-level-1 :family) :height ,(face-attribute 'org-level-1 :height))
;;     `(markdown-header-face-2 :family ,(face-attribute 'org-level-2 :family) :height ,(face-attribute 'org-level-2 :height))
;;     `(markdown-header-face-3 :family ,(face-attribute 'org-level-3 :family) :height ,(face-attribute 'org-level-3 :height))
;;     `(markdown-header-face-4 :family ,(face-attribute 'org-level-4 :family) :height ,(face-attribute 'org-level-4 :height))
;;     `(markdown-header-face-5 :family ,(face-attribute 'org-level-5 :family) :height ,(face-attribute 'org-level-5 :height))
;;     `(markdown-header-face-6 :family ,(face-attribute 'org-level-6 :family) :height ,(face-attribute 'org-level-6 :height))
;;     `(markdown-pre-face :family ,(face-attribute 'sawyer/mono-face :family))
;;     `(markdown-markup-face :family ,(face-attribute 'sawyer/mono-face :family)))
;;   )

;; (add-hook! markdown-mode
;;   (setq-local default-text-properties '(line-spacing 0.4 line-height 1.0))
;;   (face-remap-add-relative 'default 'org-face)
;;   ;; (face-remap-add-relative 'markdown-pre-face 'org-code-face)
;;   ;; (face-remap-add-relative 'markdown-markup-face 'org-code-face)
;;   ;; (setq-local display-line-numbers-type nil)
;;   ;; (display-line-numbers-mode -1)
;;   ;; (setq-local header-line-format " ")
;;   )

(after! centaur-tabs
  (custom-set-faces!
    `(centaur-tabs-default
      :family ,(face-attribute 'sawyer/variable-face :family)
      :height 0.9
      :weight medium)
    `(centaur-tabs-selected
      :family ,(face-attribute 'sawyer/variable-face :family)
      :foreground ,(doom-color 'fg)
      :background ,(doom-color 'bg)
      :height 0.9
      :weight medium)
    `(centaur-tabs-unselected
      :family ,(face-attribute 'sawyer/variable-face :family)
      :foreground ,(face-attribute font-lock-comment-face :foreground)
      :height 0.9
      :weight medium)
    `(centaur-tabs-unselected-modified
      :family ,(face-attribute 'sawyer/variable-face :family)
      :foreground ,(doom-blend (doom-color 'red) (doom-color 'grey) 0.5)
      :height 0.9
      :weight medium)
    `(centaur-tabs-selected-modified
      :family ,(face-attribute 'sawyer/variable-face :family)
      :foreground ,(doom-color 'red)
      :height 0.9
      :weight medium))
  (centaur-tabs-change-fonts (face-attribute 'sawyer/variable-face :family) 0.9)
  (centaur-tabs-headline-match)
  )

;; (after! dap-mode
;;   (custom-theme-set-faces!
;;     `(dap-ui-pending-breakpoint-face :background ,(doom-color 'yellow) :foreground ,(doom-color 'bg)))
;;   )

(after! markup-faces
  (custom-set-faces!
    `(markup-meta-face :height 1.0)
    )
  )

;; (after! adoc-mode
;;   (add-hook! adoc-mode
;;     (message "in adoc-mode hook")
;;     (face-remap-add-relative 'default 'sawyer/variable-face)
;;     ;; (setq-local default-text-properties '(line-spacing 0.1 line-height 1.2))
;;     )
;;   (custom-set-faces!
;;     `(adoc-align-face :family ,(face-attribute 'sawyer/mono-face :family))
;;     `(adoc-gen-face :foreground ,(face-attribute 'adoc-gen-face :foreground) :family ,(face-attribute 'sawyer/variable-face :family)))
;;   )
(after! doom-themes
  (custom-set-faces!
    `(doom-dashboard-banner :inherit font-lock-builtin-face :height 1.0)
    `(doom-dashboard-menu-title :inherit font-lock-keyword-face :height 1.0)
    `(doom-dashboard-menu-desc :inherit font-lock-builtin-face :height 1.0)
    `(doom-dashboard-loaded :inherit font-lock-docface :height 1.0)
    ;; `(tooltip :background ,(doom-lighten 'bg 0.1) :family ,(face-attribute 'sawyer/variable-face :family) :weight normal)
    `(font-lock-string-face :background ,(doom-lighten 'bg 0.05))
    `(line-number :slant normal)
    `(line-number-current-line :slant normal)
    `(mode-line :family ,(face-attribute 'sawyer/variable-face :family) :weight bold)
    `(mode-line-inactive :family ,(face-attribute 'sawyer/variable-face :family) :weight bold)
    `(doom-modeline-bar :family ,(face-attribute 'sawyer/variable-face :family) :background ,(doom-lighten 'bg 0.1) :weight bold)
    `(doom-modeline-buffer-file :family ,(face-attribute 'sawyer/variable-face :family) :weight bold)
    `(doom-modeline-buffer-path :family ,(face-attribute 'sawyer/variable-face :family) :weight bold)
    `(doom-modeline-buffer-project-root :family ,(face-attribute 'sawyer/variable-face :family) :weight bold)
    `(line-number :slant normal)
    `(line-number-current-line :slant normal)
    ;; `(border :background ,(doom-darken 'bg 0.1))
    ;; `(internal-border :background ,(doom-darken 'bg 0.2))
    ;; `(fringe :background ,(doom-darken 'bg 0.0))
    ;; `(window-divider :background ,(doom-darken 'bg 0.1))
    ;; `(header-line :background ,(doom-color 'bg) :family ,(face-attribute 'sawyer/variable-face :family))
    `(ediff-fine-diff-A :background ,(doom-blend 'red 'bg 0.3) :weight normal)
    `(ediff-even-diff-A :background ,(doom-blend 'red 'bg 0.2) :weight normal)
    `(ediff-odd-diff-A :background ,(doom-blend 'red 'bg 0.2) :weight normal)
    `(ediff-fine-diff-B :background ,(doom-blend 'green 'bg 0.3) :weight normal)
    `(ediff-even-diff-B :background ,(doom-blend 'green 'bg 0.2) :weight normal)
    `(ediff-odd-diff-B :background ,(doom-blend 'green 'bg 0.2) :weight normal)
    ;; `(nav-flash-face :foreground "#ffffff" :background ,(doom-lighten 'bg 0.2))
    )

  (custom-theme-set-faces! 'doom-spacegrey
    `(default :foreground "#b1bbcb" :background ,(doom-darken 'bg 0.25))
    `(window-divider :foreground ,(doom-darken 'bg 0.3) :background ,(doom-darken 'bg 0.1))
    `(fringe :background ,(doom-darken 'bg 0.25))
    `(header-line :background ,(doom-darken 'bg 0.2))
    `(line-number :foreground ,(doom-lighten (doom-color 'bg) 0.1)
      :background ,(doom-darken (doom-color 'bg) 0.2)
      :height 1.0)
    `(line-number-current-line :background ,(doom-lighten (doom-color 'bg) 0.2)
      :height 1.0)
    `(region :background ,(doom-blend (doom-color 'green) "#000000" 0.4))
    `(font-lock-keyword-face :weight medium
      :foreground ,(doom-blend (doom-color 'magenta) (doom-color 'grey) 0.4))
    `(font-lock-constant-face :weight medium
      :foreground ,(doom-lighten (doom-color 'blue) 0.3))
    `(font-lock-builtin-face :weight medium)
    `(font-lock-variable-name-face :foreground ,(doom-color 'blue))
    `(font-lock-preprocessor-face :foreground "tomato")
    `(font-lock-string-face :foreground ,(doom-blend (doom-color 'blue) "#aaaaaa" 0.7) :background ,(doom-darken (doom-color 'bg) 0.05))
    `(font-lock-type-face :foreground ,(doom-darken (doom-color 'red) 0.1))
    `(font-lock-function-name-face :foreground ,(doom-blend (doom-color 'cyan) "#88aa88" 0.4))
    `(font-lock-doc-face :foreground ,(doom-color 'orange))
    `(font-lock-comment-face :foreground "#506487")
    `(tree-sitter-hl-face:punctuation :foreground ,(doom-darken (doom-color 'red) 0.2) :weight bold)
    `(tree-sitter-hl-face:punctuation.bracket :foreground ,(doom-darken (doom-color 'red) 0.2) :weight bold)
    `(tree-sitter-hl-face:punctuation.special :foreground ,(doom-darken (doom-color 'red) 0.2) :weight bold)
    `(tree-sitter-hl-face:punctuation.delimiter :foreground ,(doom-darken (doom-color 'red) 0.2) :weight bold)
    `(tree-sitter-hl-face:constant :foreground ,(doom-color 'blue))
    `(treemacs-git-modified-face :foreground ,(doom-color 'blue))
    `(treemacs-git-untracked-face :foreground ,(doom-color 'yellow))
    `(region :background "#405477")
    `(vertico-current :background "#405477")
    `(corfu-current :background "#405477")
    )

  (custom-theme-set-faces! 'doom-ayu-mirage
    `(line-number :inherit default
      :foreground ,(doom-lighten 'bg 0.2)
      :background ,(doom-darken 'bg 0.1))
    `(font-lock-keyword-face :foreground ,(doom-color 'cyan) :weight bold)
    `(font-lock-builtin-face :foreground ,(doom-color 'blue) :weight bold)
    `(font-lock-constant-face :foreground ,(doom-color 'blue) :weight bold)
    `(font-lock-string-face :background ,(doom-lighten (doom-color 'bg) 0.1) :foreground ,(doom-blend (doom-color 'red) "#aaaaaa" 0.2))
    `(font-lock-function-name-face :weight bold)
    `(font-lock-type-face :foreground ,(doom-color 'red) :weight bold)
    `(font-lock-variable-name-face :foreground ,(doom-blend (doom-color 'blue) "#aaaaaa" 0.2) :weight bold)
    `(font-lock-doc-face :foreground ,(doom-color 'blue))
    `(font-lock-comment-face :foreground ,(doom-blend (doom-color 'blue) "#888888" 0.4))
    `(ediff-fine-diff-A :background ,(doom-blend 'red 'bg 0.3) :weight bold))

  (custom-theme-set-faces! 'doom-ayu-dark
    `(line-number :inherit default
      :foreground ,(doom-lighten 'bg 0.2)
      :background ,(doom-darken 'bg 0.1))
    `(font-lock-keyword-face :foreground ,(doom-color 'cyan) :weight thin)
    `(font-lock-builtin-face :foreground ,(doom-color 'blue) :weight thin)
    `(font-lock-constant-face :foreground ,(doom-color 'blue) :weight thin)
    `(font-lock-string-face :background ,(doom-lighten (doom-color 'bg) 0.1) :foreground ,(doom-blend (doom-color 'red) "#aaaaaa" 0.2))
    `(font-lock-function-name-face :weight thin)
    `(font-lock-type-face :foreground ,(doom-color 'red) :weight thin)
    `(font-lock-variable-name-face :foreground ,(doom-blend (doom-color 'blue) "#aaaaaa" 0.2) :weight thin)
    `(font-lock-doc-face :foreground ,(doom-color 'blue))
    `(font-lock-comment-face :foreground ,(doom-blend (doom-color 'blue) "#888888" 0.4))
    `(ediff-fine-diff-A :background ,(doom-blend 'red 'bg 0.3) :weight thin)
    `(vertico-current :background ,(doom-blend (doom-color 'cyan) (doom-color 'bg) 0.4))
    `(vertico-group-title :foreground ,(doom-blend (doom-color 'cyan) (doom-color 'bg) 0.4))
    `(nav-flash-face :foreground "#ffffff" :background ,(doom-lighten 'bg 0.2))
    `(doom-modeline-buffer-modified :foreground ,(doom-color 'red))
    `(treemacs-hl-line-face :inherit hl-line :background ,(doom-lighten 'bg 0.3))
    `(tree-sitter-hl-face:function.method.call :weight thin :foreground ,(doom-lighten (doom-color 'blue) 0.3))
    `(tree-sitter-hl-face:property :weight thin)
    )
  (setq
   evil-insert-state-cursor '(bar "green")
   evil-replace-state-cursor '(box "red")
   evil-visual-state-cursor '(hollow "yellow"))

  (custom-theme-set-faces! 'doom-monokai-pro
    `(line-number
      :foreground ,(doom-lighten (doom-color 'bg) 0.2)
      :height 1.0)
    `(line-number-current-line
      :foreground ,(doom-darken (doom-color 'cyan) 0.3)
      :background ,(doom-darken (doom-color 'cyan) 0.6)
      :height 1.0)
    `(treemacs-root-face :inherit nil :foreground ,(doom-color 'fg))
    `(doom-themes-treemacs-file-face :inherit nil :foreground ,(doom-color 'red))
    `(treemacs-git-untracked-face :foreground ,(doom-color 'grey))
    `(treemacs-git-modified-face :foreground ,(doom-color 'blue))
    `(window-divider :foreground ,(doom-lighten 'bg 0.05))
    `(region :background ,(doom-blend (doom-color 'blue) "#111111" 0.2))
    `(evil-ex-lazy-highlight
      :background ,(doom-darken (doom-color 'blue) 0.1)
      :foreground "black")
    `(evil-ex-search
      :background ,(doom-lighten (doom-color 'blue) 0.2)
      :foreground "black")
    `(font-lock-comment-face :foreground ,(doom-blend (doom-color 'cyan) "#555555" 0.3))
    )

  (custom-theme-set-faces! '(doom-monokai-octagon doom-monokai-spectrum)
    `(default :foreground ,(doom-color 'fg) :background ,(doom-darken (doom-color 'bg) 0.2))
    `(vertical-border :foreground ,(doom-darken (doom-color 'bg) 0.3))
    `(window-divider :foreground ,(doom-darken (doom-color 'bg) 0.3))
    `(header-line
      :background ,(doom-darken (doom-color 'bg) 0.2)
      ;; :family ,(face-attribute 'sawyer/variable-face :family)
      :height 1.0
      :weight normal)
    `(region :background ,(doom-blend (doom-color 'blue) "#111111" 0.2))
    `(line-number
      :foreground ,(doom-lighten (doom-color 'bg) 0.1)
      :height 1.0)
    `(line-number-current-line
      :foreground ,(doom-darken (doom-color 'cyan) 0.3)
      :height 1.0)
    `(evil-ex-lazy-highlight
      :background ,(doom-darken (doom-color 'yellow) 0.1)
      :foreground "black")
    `(evil-ex-search
      :background ,(doom-lighten (doom-color 'yellow) 0.2)
      :foreground "black")
    `(font-lock-comment-face :foreground ,(doom-darken (doom-color 'cyan) 0.4))
    `(font-lock-variable-name-face :foreground ,(doom-blend (doom-color 'cyan) "#ffffff" 0.5))
    `(font-lock-keyword-face :weight normal)
    `(font-lock-builtin-face :weight normal)
    ;; `(org-face
    ;;   :family ,(face-attribute 'sawyer/variable-face :family)
    ;;   :weight normal
    ;;   :foreground ,(doom-blend (doom-color 'fg) (doom-color 'cyan) 0.7))
    ;; `(org-code-face
    ;;   :family ,(face-attribute 'sawyer/mono-face :family)
    ;;   :weight normal
    ;;   :foreground ,(doom-color 'cyan))
    ;; `(org-document-title :foreground ,(doom-color 'fg))
    ;; `(org-modern-symbol :foreground ,(doom-color 'red))
    ;; `(org-list-dt :foreground ,(doom-color 'red))
    ;; `(lsp-face-highlight-textual :background ,(doom-lighten (doom-color 'bg) 0.2))
    )

  (custom-theme-set-faces! 'doom-tokyo-night
    `(line-number
      :foreground ,(doom-lighten (doom-color 'bg) 0.2))
    `(line-number-current-line
      :foreground ,(doom-lighten (doom-color 'bg) 0.4))
    `(font-lock-string-face :background ,(doom-lighten (doom-color 'bg) 0.05))
    `(treemacs-root-face :foreground ,(doom-color 'red))
    `(treemacs-header-button-face :foreground ,(doom-color 'red))
    `(font-lock-keyword-face :foreground "#bb9af7" :weight bold)
    `(font-lock-builtin-face :foreground "#bb9af7" :weight bold)
    `(font-lock-constant-face :foreground "#ff9e64" :weight bold)
    `(font-lock-string-face :background "#2f303f" :foreground ,(doom-blend (doom-color 'blue) "#aaaaaa" 0.2))
    `(font-lock-function-name-face :foreground "#7aa2f7" :weight bold)
    `(font-lock-type-face :foreground ,(doom-color 'red) :weight bold)
    `(font-lock-variable-name-face :foreground ,(doom-blend (doom-color 'blue) "#aaaaaa" 0.6) :weight normal)
    `(font-lock-comment-face :foreground ,(doom-blend (doom-color 'blue) "#888888" 0.4))
    `(font-lock-doc-face :inherit font-lock-comment-face)
    `(vertico-current :background ,(doom-blend (doom-color 'cyan) (doom-color 'bg) 0.2))
    `(nav-flash-face :foreground "#ffffff" :background ,(doom-lighten 'bg 0.2))
    `(doom-modeline-buffer-modified :foreground ,(doom-color 'red))
    `(ediff-fine-diff-A :background ,(doom-blend 'red 'bg 0.3) :weight bold)
    `(ediff-even-diff-A :background ,(doom-blend 'red 'bg 0.2) :weight bold)
    `(ediff-odd-diff-A :background ,(doom-blend 'red 'bg 0.2) :weight bold)
    `(ediff-fine-diff-B :background ,(doom-blend 'green 'bg 0.3) :weight bold)
    `(ediff-even-diff-B :background ,(doom-blend 'green 'bg 0.2) :weight bold)
    `(ediff-odd-diff-B :background ,(doom-blend 'green 'bg 0.2) :weight bold)
    `(treemacs-hl-line-face :inherit hl-line :background ,(doom-lighten 'bg 0.2))
    `(header-line :background ,(doom-darken 'bg 0.2), :foreground ,(doom-color 'fg))
    `(border :foreground ,(doom-color 'bg) :background ,(doom-darken 'bg 0.2))
    `(internal-border :foreground ,(doom-color 'bg) :background ,(doom-darken 'bg 0.3))
    `(fringe :foreground ,(doom-color 'bg) :background ,(doom-color 'bg))
    `(window-divider :foreground ,(doom-darken 'bg 0.5) :background ,(doom-darken 'bg 0.5))
    `(font-lock-punctuation-face :foreground ,(doom-color 'red))
    )

  (custom-theme-set-faces! 'doom-challenger-deep
    `(default :background "#171a2c")
    `(line-number
      :foreground ,(doom-lighten (doom-color 'bg) 0.2)
      :height 1.0)
    `(line-number-current-line
      :foreground ,(doom-lighten (doom-color 'bg) 0.4)
      :height 1.0)
    ;; `(font-lock-keyword-face :foreground ,(doom-color 'red) :weight normal)
    ;; `(font-lock-builtin-face :foreground ,(doom-color 'magenta) :weight normal)
    ;; `(font-lock-constant-face :foreground ,(doom-color 'cyan) :weight normal)
    `(font-lock-function-name-face :foreground ,(doom-blend (doom-color 'cyan) "#dddddd" 0.5) :weight normal)
    `(font-lock-type-face :foreground "#91DDFF" :weight normal)
    `(font-lock-string-face :foreground ,(doom-color 'base7) :background ,(doom-color 'base1))
    `(font-lock-comment-face :inherit nil :foreground "#2590dd")
    `(font-lock-bracket-face :foreground ,(doom-darken (doom-color 'red) 0.2))
    `(font-lock-delimiter-face :foreground ,(doom-darken (doom-color 'red) 0.2))
    `(font-lock-operator-face :foreground ,(doom-darken (doom-color 'red) 0.2))
    `(treemacs-window-background-face :background ,(doom-darken 'bg 0.1))
    `(treemacs-hl-line-face :inherit hl-line :background ,(doom-lighten 'bg 0.2))
    `(treemacs-git-modified-face :foreground ,(doom-color 'dark-blue))
    `(border :foreground ,(doom-color 'bg) :background ,(doom-darken 'bg 0.3))
    `(internal-border :foreground ,(doom-color 'bg) :background ,(doom-darken 'bg 0.3))
    `(window-divider :foreground ,(doom-darken 'bg 0.05) :background ,(doom-darken 'bg 0.5))
    `(fringe :foreground ,(doom-lighten 'bg 0.2) :background ,(doom-darken 'bg 0.1))
    `(mode-line :background ,(doom-darken 'bg 0.1))
    `(doom-modeline-buffer-modified :foreground ,(doom-color 'red))
    `(doom-modeline-buffer-project-root :foreground ,(doom-color 'blue))
    `(doom-modeline-buffer-path :foreground ,(doom-color 'blue))
    )

  (custom-theme-set-faces! 'doom-horizon
    `(default :background ,(doom-darken 'bg 0.1))
    `(region :background ,(doom-darken 'grey 0.2))
    `(line-number :foreground "#335566")
    `(line-number-current-line :foreground "#002255" :background "#447799")
    `(evil-ex-lazy-highlight
      :background ,(doom-darken (doom-color 'yellow) 0.1)
      :foreground "black")
    `(evil-ex-search
      :background ,(doom-lighten (doom-color 'yellow) 0.2)
      :foreground "black")
    `(doom-modeline-buffer-modified :foreground ,(doom-color 'red))
    `(doom-modeline-buffer-project-root :foreground ,(doom-color 'blue))
    `(doom-modeline-buffer-path :foreground ,(doom-color 'blue))
    `(font-lock-string-face :background ,(doom-lighten 'bg 0.1) :foreground ,(doom-color 'cyan))
    `(font-lock-type-face :weight bold :foreground ,(doom-color 'red))
    `(font-lock-doc-face :background ,(doom-darken (doom-color 'cyan) 0.8) :foreground ,(doom-color 'cyan))
    `(font-lock-keyword-face :weight bold :foreground ,(doom-color 'magenta))
    `(font-lock-builtin-face :weight bold :foreground ,(doom-color 'magenta))
    `(font-lock-comment-face :foreground "#2f79a2")
    ;; `(org-document-title :foreground ,(doom-color 'cyan))
    `(wgrep-face :background ,(doom-darken 'green 0.6) :weight bold)
    `(treemacs-window-background-face :background ,(doom-darken 'bg 0.2))
    `(treemacs-root-face :background ,(doom-darken 'bg 0.05))
    `(treemacs-git-added-face :foreground ,(doom-color 'green))
    `(treemacs-git-modified-face :foreground ,(doom-color 'blue))
    `(treemacs-git-untracked-face :foreground ,(doom-color 'magenta))
    `(window-divider :foreground ,(doom-color 'bg))
    `(fringe :foreground ,(doom-color 'bg))
    `(font-lock-variable-name-face :foreground ,(doom-color 'fg))
    ;; `(border :background "#0e0e0f")
    ;; `(internal-border :background "#0e0e0f")
    `(org-document-title :inherit 'org-face :height 1.1 :weight bold :foreground ,(doom-color 'blue))
    `(org-table :foreground ,(doom-color 'cyan))
    `(org-meta-line :inherit font-lock-comment-face)
    `(org-document-info-keyword :foreground ,(doom-color 'orange))
    `(lsp-headerline-breadcrumb-path-face :background ,(doom-darken 'bg 0.05) :foreground ,(doom-color 'yellow))
    `(lsp-headerline-breadcrumb-symbols-face :background ,(doom-darken 'bg 0.05) :foreground ,(doom-color 'cyan))
    )

  (custom-theme-set-faces! 'doom-nord-light
    `(line-number
      :foreground ,(doom-darken (doom-color 'bg) 0.2)
      :height 1.0)
    `(line-number-current-line
      :foreground ,(doom-darken (doom-color 'bg) 0.4)
      :height 1.0)
    ;; `(font-lock-keyword-face :weight bold)
    ;; `(font-lock-builtin-face :weight bold)
    `(org-modern-symbol :foreground ,(doom-color 'red))
    `(org-list-dt :foreground ,(doom-color 'red))
    ;; `(treemacs-root-face :foreground ,(doom-color 'red))
    ;; `(doom-themes-treemacs-root-face :foreground ,(doom-color 'red))
    ;; `(treemacs-header-button-face :foreground ,(doom-color 'red))
    ;; `(lsp-face-highlight-textual
    ;;   :foreground ,(doom-color 'fg)
    ;;   :background ,(doom-color 'bg))
    ;; `(font-lock-keyword-face :weight bold)
    ;; `(font-lock-type-face :weight bold)
    `(font-lock-string-face :background "#e8eeff")
    `(dockerfile-image-name :foreground ,(doom-color 'cyan))
    `(highlight-numbers-number :foreground ,(doom-color 'red))
    )

  (custom-theme-set-faces! 'doom-feather-dark
    `(default :background "#22222f")
    `(font-lock-comment-face :foreground "#5699af" :background "#2f2839" :slant normal))

  (custom-theme-set-faces! 'doom-moonlight
    `(font-lock-comment-face :foreground ,(doom-blend (doom-color 'blue) "#888888" 0.3))
    `(font-lock-string-face :background ,(doom-lighten 'bg 0.05) :foreground ,(doom-blend (doom-color 'blue) "#888888" 0.7))
    `(line-number :inherit 'default :foreground "#666688")
    `(line-number-current-line :inherit 'default :foreground "#aaaacc" :slant normal)
    `(vertico-current :background ,(doom-blend (doom-color 'cyan) (doom-color 'bg) 0.2))
    `(nav-flash-face :foreground "#ffffff" :background ,(doom-lighten 'bg 0.2))
    `(doom-modeline-buffer-modified :foreground ,(doom-color 'red))
    `(ediff-fine-diff-A :background ,(doom-blend 'red 'bg 0.3) :weight medium)
    `(ediff-even-diff-A :background ,(doom-blend 'red 'bg 0.2) :weight medium)
    `(ediff-odd-diff-A :background ,(doom-blend 'red 'bg 0.2) :weight medium)
    `(ediff-fine-diff-B :background ,(doom-blend 'green 'bg 0.3) :weight medium)
    `(ediff-even-diff-B :background ,(doom-blend 'green 'bg 0.2) :weight medium)
    `(ediff-odd-diff-B :background ,(doom-blend 'green 'bg 0.2) :weight medium)
    `(treemacs-hl-line-face :inherit hl-line :background ,(doom-lighten 'bg 0.1))
    `(org-document-title :inherit 'org-face :height 1.1 :weight bold :foreground ,(doom-color 'blue))
    `(org-table :foreground ,(doom-color 'cyan))
    )
  (custom-theme-set-faces! 'doom-city-lights
    `(default :foreground ,(doom-blend (doom-color 'grey) "#CCDDFF" 0.4))
    `(font-lock-variable-name-face :inherit 'default)
    `(font-lock-comment-face :foreground ,(doom-blend (doom-color 'grey) (doom-color 'blue) 0.6))
    `(font-lock-string-face :foreground ,(doom-color 'blue), :background ,(doom-darken (doom-color 'blue) 0.7))
    `(region :background ,(doom-blend (doom-color 'bg) (doom-color 'blue) 0.7))
    `(vertico-current :inherit 'region)
    `(corfu-current :inherit 'region)
    `(font-lock-type-face :foreground ,(doom-blend (doom-color 'grey) (doom-color 'red) 0.4) :weight bold)
    `(font-lock-constant-face :foreground ,(doom-blend (doom-color 'grey) (doom-color 'blue) 0.4) :weight bold)
    `(font-lock-function-name-face :foreground ,(doom-blend (doom-color 'grey) (doom-color 'blue) 0.4) :weight bold)
    `(font-lock-builtin-face :foreground ,(doom-color 'cyan) :weight bold)
    `(font-lock-keyword-face :foreground ,(doom-color 'cyan) :weight bold)
    `(treemacs-git-modified-face :foreground ,(doom-color 'cyan))
    )
  (custom-theme-set-faces! 'modus-vivendi
    `(default :background "#0e0e0f" :foreground "#aabbff")
    `(window-divider :foreground "#0f0f1a")
    `(fringe :foreground "#0e0e0f")
    `(border :background "#0e0e0f")
    `(internal-border :background "#0e0e0f")
    `(line-number :background "#000000" :foreground "#777777" :height 0.9)
    `(line-number-current-line :background "#333333" :foreground "#999999" :height 0.9)
    `(region :inherit 'default :background "#223355")
    `(mode-line :box nil :background "#333333")
    `(modus-themes-tab-backdrop :background "#222222")
    `(treemacs-root-face :underline nil)
    `(centaur-tabs-default :family ,(face-attribute 'sawyer/variable-face :family) :height 0.9 :background "#222222" :slant normal :weight medium)
    `(centaur-tabs-selected :family ,(face-attribute 'sawyer/variable-face :family) :foreground "#aaabbff" :background "#223355" :height 0.9 :slant normal :weight medium)
    `(centaur-tabs-unselected :family ,(face-attribute 'sawyer/variable-face :family) :foreground ,(face-attribute font-lock-comment-face :foreground) :background "#222222" :height 0.9 :slant normal :weight medium)
    `(centaur-tabs-unselected-modified :family ,(face-attribute 'sawyer/variable-face :family) :height 0.9 :background "#222222" :foreground "#cc3333" :slant normal :weight medium)
    `(centaur-tabs-selected-modified :family ,(face-attribute 'sawyer/variable-face :family) :height 0.9 :foreground "#ff3333" :slant normal :weight medium)))

(defun sawyer/modus-themes-custom-faces (&rest _)
  (modus-themes-with-colors
   (custom-set-faces
    ;; Make foreground the same as background for a uniform bar on
    ;; Doom Emacs.
    ;;
    ;; Doom should not be implementing such hacks because themes
    ;; cannot support them:
    ;; <https://protesilaos.com/codelog/2022-08-04-doom-git-gutter-modus-themes/>.
    `(git-gutter-fr:added ((,c :foreground ,bg-added-fringe)))
    `(git-gutter-fr:deleted ((,c :foreground ,bg-removed-fringe)))
    `(git-gutter-fr:modified ((,c :foreground ,bg-changed-fringe))))))

(add-hook! modus-themes-after-load-theme #'sawyer/modus-themes-custom-faces)

(after! cov
  (custom-set-faces!
    ;; Fringe indicator face used for lines which were not run.
    `(cov-none-face :foreground ,(doom-color 'red))
    ;; Fringe indicator face used for rarely-run lines.
    `(cov-light-face :foreground ,(doom-color 'green))
    ;; Fringe indicator face used for commonly-run lines See ‘cov-med-threshold’.
    `(cov-med-face :foreground ,(doom-lighten (doom-color 'green) 0.1))
    ;; Fringe indicator face used for heavily-run lines See ‘cov-high-threshold’.
    `(cov-heavy-face :foreground ,(doom-lighten (doom-color 'green) 0.2))
    ;; Fringe indicator face used in coverage mode for lines which were run.
    ;; `(cov-coverage-run-face)
    ;; Fringe indicator face used in coverage mode for lines which were not run.
    ;; `(cov-coverage-not-run-face)
    )
  )

(load-theme sawyer/dark-theme 't)

;; only works on macos with homebrew patches
;; TODO not sure how to detect whether this is from homebrew vs vanilla emacs-macport
(when (featurep :system 'macos)
  ;; (message "setting up transparent title bar...")
  ;; (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  ;; (setq ns-use-proxy-icon nil)
  ;; (setq frame-title-format "%e")
  ;; (message "setting up transparent title bar done")
  )
