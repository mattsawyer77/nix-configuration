;;; +ui.el --- UI, theme, font, and faces configuration -*- lexical-binding: t; -*-

;;; Frame defaults

(setq default-frame-alist
      (append
       '((min-height . 1) (height . 45)
         (min-width . 1) (width . 81)
         (vertical-scroll-bars . nil)
         (internal-border-width . 10)
         (left-fringe . 0) (right-fringe . 0)
         (tool-bar-lines . 0) (menu-bar-lines . 0))))

(when (and (featurep :system 'macos) (fboundp 'display-graphic-p))
  (setq ns-use-proxy-icon nil)
  (setq frame-title-format nil)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark)))

;;; Faces and Font Setup

(defface sawyer/mono-face
  '((t :inherit default :family "PragmataPro Liga 1.1" :weight normal))
  ;; '((t :inherit default :family "JetBrains Mono 1.1" :weight normal))
  ;; '((t :inherit default :family "JetBrains Mono"))
  ;; '((t :inherit default :family "Iosevka Custom" :weight normal))
  "Font spec for all monospace text.")

(defface sawyer/variable-face
  ;; '((t :inherit default :family "Galvji" :size 20 :weight light))
  '((t :inherit default :family "Fira Sans" :size 20 :weight normal))
  "Font spec for all variable-width text.")

(defface code-face
  '((t :inherit sawyer/mono-face :height 1.2 :weight normal))
  "Monospace code face.")

(defface org-face
  '((t :inherit sawyer/variable-face :height 1.2))
  "Variable-pitch face for prose/org.")

(defface org-code-face
  '((t :inherit code-face :height 0.8))
  "Face for inline code in org.")

(defcustom sawyer/baseline-font-size 18.0 "Baseline font pt size." :type 'number)
(defcustom sawyer/dark-theme 'kanagawa "Theme for dark mode." :type 'symbol)
(defcustom sawyer/light-theme 'doom-nord-light "Theme for light mode." :type 'symbol)

(setq doom-font (font-spec
                 :family (face-attribute 'sawyer/mono-face :family)
                 :size sawyer/baseline-font-size))
(setq doom-variable-pitch-font (font-spec
                                :family (face-attribute 'sawyer/variable-face :family)
                                :size 18 :weight 'normal))

;;; Org-mode UI/face tweaks

(setq
 org-auto-align-tags nil
 org-tags-column 0
 org-fold-catch-invisible-edits 'show-and-error
 org-special-ctrl-a/e t
 org-insert-heading-respect-content t
 org-hide-emphasis-markers t
 org-pretty-entities nil
 org-ellipsis "…")

(add-hook 'org-mode-hook #'org-modern-mode)

(add-hook! org-mode
           ;; Set variable-pitch for org main text, and code font for org-code
           (face-remap-add-relative 'solaire-default-face 'org-face)
           (face-remap-add-relative 'default 'org-face)
           (face-remap-add-relative 'org-code 'org-code-face)
           (setq-local display-line-numbers-type nil)
           (display-line-numbers-mode -1)
           (auto-save-mode t)
           (mixed-pitch-mode t))

(after! org
  (custom-set-faces!
    `(org-default :inherit 'org-face)
    `(org-table :family ,(face-attribute 'sawyer/mono-face :family) :height 1.0)
    `(org-level-1 :family ,(face-attribute 'sawyer/variable-face :family) :height 1.2 :weight normal)
    `(org-level-2 :family ,(face-attribute 'sawyer/variable-face :family) :height 1.05 :weight bold)
    `(org-level-3 :family ,(face-attribute 'sawyer/variable-face :family) :height 1.0)
    `(org-level-4 :family ,(face-attribute 'sawyer/variable-face :family) :height 0.9 :weight bold)
    `(org-level-5 :family ,(face-attribute 'sawyer/variable-face :family) :height 0.9)
    `(org-level-6 :family ,(face-attribute 'sawyer/variable-face :family) :height 0.9)
    `(org-level-7 :family ,(face-attribute 'sawyer/variable-face :family) :height 0.9)
    `(org-level-8 :family ,(face-attribute 'sawyer/variable-face :family) :height 0.9)
    `(org-document-title :inherit 'org-face :height 1.1 :weight bold)
    `(org-hide :inherit default :family ,(face-attribute 'sawyer/mono-face :family))
    `(org-checkbox :family "Fira Code" :height 1.2 :weight normal)
    `(org-quote :inherit org-block :family ,(face-attribute 'sawyer/variable-face :family))))

;;; Prog & Text Modes: Code font and line numbers

(add-hook! (prog-mode text-mode conf-toml-mode conf-space-mode gitignore-mode go-template-mode protobuf-mode go-mode)
  (face-remap-add-relative 'default 'code-face)
  (setq-local default-text-properties '(line-spacing 0.2 line-height 1.2))
  (display-line-numbers-mode 1))

;;; Theme Switching (Light/Dark)

(defun sawyer/light-switch ()
  "Toggle between light and dark themes."
  (interactive)
  (load-theme (if (eq doom-theme sawyer/light-theme)
                  sawyer/dark-theme
                sawyer/light-theme)
              t))

;; On auto-switch theme with system setting, or dark-theme if undetectable
(defun sawyer/auto-set-theme (&optional system-appearance-mode)
  "Set theme based on system appearance to system-appearance-mode, which can be either `light` or `dark`. nil will result in automatic theme detection."
  (cond ((string-equal system-appearance-mode "dark") (load-theme sawyer/dark-theme t))
        ((string-equal system-appearance-mode "light") (load-theme sawyer/light-theme t))
        (ns-system-appearance ;; (emacs-plus or jimeh emacs builds)
         (progn
           (message "ns-system-appearance is set to %s" ns-system-appearance)
           (progn
             (cond ((string-equal ns-system-appearance "dark")
                    (progn (message "setting theme to %s" sawyer/dark-theme)
                           ;; pre-load a doom theme to see if it makes the switch work better?
                           (load-theme 'doom-ayu-dark t)
                           (load-theme sawyer/dark-theme t)))
                   ((string-equal ns-system-appearance "light")
                    (progn (message "setting theme to %s" sawyer/light-theme)
                           ;; pre-load a doom theme to see if it makes the switch work better?
                           (load-theme 'doom-ayu-light t)
                           (load-theme sawyer/light-theme t)))
                   (t (progn
                        (error "unknown system appearance mode %s, falling back to the dark-theme: %s" ns-system-appearance sawyer/dark-theme)
                        ;; pre-load a doom theme to see if it makes the switch work better?
                        (load-theme 'doom-ayu-dark t)
                        (load-theme sawyer/dark-theme t)))))))
        ((fboundp 'mac-application-state) ;; (emacs-mac only)
         (progn
           "Set theme based on macOS appearance."
           (let ((appearance (plist-get (mac-application-state) :appearance)))
             (if (string-equal appearance "NSAppearanceNameAqua")
                 (load-theme sawyer/light-theme t)
               (load-theme sawyer/dark-theme t)))
           (add-hook! doom-after-init #'sawyer/auto-set-theme)
           (add-hook! mac-effective-appearance-change #'sawyer/auto-set-theme)))
        (t
         (progn
           (message "this version of emacs does not support detection of system light/dark mode, falling back to dark-theme %s" sawyer/dark-theme)
           ;; pre-load a doom theme to see if it makes the switch work better?
           (load-theme 'doom-ayu-dark t)
           (load-theme sawyer/dark-theme t)))))

(map! :after general
      :map general-override-mode-map
      :nv "<f7>" #'sawyer/light-switch)

;;; Theme / Package Customizations

(after! doom-themes
  (setq doom-themes-enable-italic nil)
  (custom-set-faces!
    `(mode-line :family ,(face-attribute 'sawyer/variable-face :family) :weight bold)
    `(mode-line-inactive :family ,(face-attribute 'sawyer/variable-face :family) :weight bold)
    `(line-number :slant normal)
    `(line-number-current-line :slant normal))
  (custom-theme-set-faces! 'doom-nord-light
    `(vertical-border :background ,(doom-darken 'bg 0.02))
    `(border :background ,(doom-darken 'bg 0.02))
    `(window-divider :foreground ,(doom-darken 'bg 0.1))
    `(font-lock-string-face :background ,(doom-darken 'bg 0.05) :foreground ,(doom-color 'blue))
    `(line-number-current-line :background "#dld9e0")
    `(doom-modeline :background "#clc9d0")
    `(doom-modeline-info :background "#clc9d0")
    `(doom-modeline-buffer-file :background "#clc9d0")
    `(doom-modeline-buffer-path :background "#clc9d0")
    `(doom-modeline-buffer-modified :background "#clc9d0")
    `(doom-modeline-buffer-major-mode :background "#clc9d0")
    `(doom-modeline-project-dir :background "#clc9d0")
    `(modern-tab-bar-separator :foreground "#clc9d0" :background "#clc9d0")
    `(modern-tab-bar-tab :foreground "#clc9d0" :background "#clc9d0")
    `(modern-tab-bar-tab-highlight :foreground "#clc9d0" :background "#clc9d0")
    `(tab-bar-tab-group-inactive :foreground "#clc9d0" :background "#clc9d0")
    `(tab-bar-tab-ungrouped :foreground "#clc9d0" :background "#clc9d0")
    `(vertico-current :foreground ,(doom-color 'cyan))
    )
  (custom-theme-set-faces! 'doom-ayu-light
    `(font-lock-comment-face :foreground "#a4a7a9")
    `(doom-modeline-buffer-modified :foreground ,(doom-color 'red))
    `(org-block :family ,(face-attribute 'sawyer/mono-face :family))
    `(org-level-1 :foreground ,(doom-color 'orange))
    `(org-level-2 :foreground ,(doom-color 'blue))
    `(org-level-3 :foreground ,(doom-color 'cyan))
    `(org-level-4 :foreground ,(doom-color 'yellow))
    )
  (custom-theme-set-faces! 'doom-tokyo-night
    `(line-number :foreground "#62597b" :height 0.9)
    `(line-number-current-line :height 0.9)
    `(font-lock-bracket-face :foreground ,(doom-darken 'blue 0.1))
    `(font-lock-delimiter-face :foreground ,(doom-color 'red))
    `(font-lock-type-face :foreground ,(doom-color 'red) :weight bold)
    `(font-lock-function-name-face :foreground ,(doom-color 'blue) :weight bold)
    `(font-lock-operator-face :foreground ,(doom-color 'orange))
    `(font-lock-comment-face :foreground "#62597b")
    `(font-lock-keyword-face :foreground ,(doom-color 'magenta) :weight bold)
    `(font-lock-variable-name-face :foreground ,(doom-darken 'fg 0.1))
    `(font-lock-string-face :foreground ,(doom-color 'green) :background ,(doom-lighten 'bg 0.05))
    )
  )

(after! tree-sitter
  (custom-set-faces!
    `(tree-sitter-hl-face:property :slant normal :inherit font-lock-constant-face)))

(after! lsp-ui
  (custom-set-faces!
    `(lsp-flycheck-warning-unnecessary-category :inherit default :background ,(doom-color 'bg) :foreground warning)
    `(lsp-lens-face :foreground ,(doom-darken 'fg 0.1) :height 0.8)
    `(lsp-ui-doc-header :foreground ,(doom-color 'fg) :background ,(doom-color 'bg))
    `(lsp-inlay-hint-face
      :height 0.8
      :foreground ,(face-attribute 'font-lock-type-face :foreground)
      :family ,(face-attribute 'sawyer/variable-face :family))))

(after! treemacs
  (setq treemacs-user-mode-line-format 'none
        doom-themes-treemacs-enable-variable-pitch t)
  (custom-set-faces!
    `(treemacs-root-face :inherit treemacs-window-background-face :height 1.2 :weight normal :family ,(face-attribute 'sawyer/variable-face :family))
    `(treemacs-directory-face :foreground ,(doom-darken 'fg 0.2) :family ,(face-attribute 'sawyer/variable-face :family))
    `(treemacs-file-face :foreground ,(doom-darken 'fg 0.2) :family ,(face-attribute 'sawyer/variable-face :family))
    `(treemacs-directory-collapsed-face :weight normal :slant normal :height 1.0 :family ,(face-attribute 'sawyer/variable-face :family))
    `(treemacs-directory-face :foreground ,(doom-darken 'fg 0.2) :weight normal :slant normal :height 1.0 :family ,(face-attribute 'sawyer/variable-face :family))
    `(treemacs-file-face :foreground ,(doom-darken 'fg 0.2) :weight normal :slant normal :height 1.0 :family ,(face-attribute 'sawyer/variable-face :family))
    `(treemacs-fringe-indicator-face :weight normal :slant normal :height 1.0 :family ,(face-attribute 'sawyer/variable-face :family))
    `(treemacs-git-added-face :weight normal :slant normal :height 1.0 :family ,(face-attribute 'sawyer/variable-face :family))
    `(treemacs-git-commit-diff-face :family ,(face-attribute 'sawyer/variable-face :family))
    `(treemacs-git-conflict-face :weight normal :slant normal :height 1.0 :family ,(face-attribute 'sawyer/variable-face :family))
    `(treemacs-git-ignored-face :weight normal :slant normal :height 1.0 :family ,(face-attribute 'sawyer/variable-face :family))
    `(treemacs-git-modified-face :weight normal :slant normal :height 1.0 :family ,(face-attribute 'sawyer/variable-face :family))
    `(treemacs-git-renamed-face :weight normal :slant normal :height 1.0 :family ,(face-attribute 'sawyer/variable-face :family))
    `(treemacs-git-unmodified-face :foreground ,(doom-darken 'fg 0.2) :weight normal :slant normal :height 1.0 :family ,(face-attribute 'sawyer/variable-face :family))
    `(treemacs-git-untracked-face :weight normal :slant normal :height 1.0 :family ,(face-attribute 'sawyer/variable-face :family))
    `(treemacs-header-button-face :family ,(face-attribute 'sawyer/variable-face :family))
    `(treemacs-help-column-face :weight normal :slant normal :height 1.0 :family ,(face-attribute 'sawyer/variable-face :family))
    `(treemacs-help-title-face :weight normal :slant normal :height 1.0 :family ,(face-attribute 'sawyer/variable-face :family))
    `(treemacs-marked-file-face :family ,(face-attribute 'sawyer/variable-face :family))
    `(treemacs-on-failure-pulse-face :weight normal :slant normal :height 1.0 :family ,(face-attribute 'sawyer/variable-face :family))
    `(treemacs-on-success-pulse-face :weight normal :slant normal :height 1.0 :family ,(face-attribute 'sawyer/variable-face :family))
    `(treemacs-peek-mode-indicator-face :family ,(face-attribute 'sawyer/variable-face :family))
    `(treemacs-tags-face :weight normal :slant normal :height 1.0 :family ,(face-attribute 'sawyer/variable-face :family))
    `(treemacs-term-node-face :weight normal :slant normal :height 1.0 :family ,(face-attribute 'sawyer/variable-face :family))))

(after! consult
  (custom-set-faces!
    `(consult-grep-context :inherit font-lock-comment-face)
    `(consult-help :inherit font-lock-doc-face)))

(after! doom-modeline
  (setq doom-modeline-buffer-file-name-style 'relative-to-project
        doom-challenger-deep-padded-modeline t
        doom-modeline-major-mode-icon t))

(after! hl-todo
  (setq hl-todo-keyword-faces
        `(("TODO"  . ,(face-foreground 'warning))
          ("FIXME" . ,(face-foreground 'error))
          ("XXX" . ,(face-foreground 'error))
          ("NOTE"  . ,(face-foreground 'success)))))

;;; Text scaling

(when (display-graphic-p)
  (after! textsize
    (setq textsize-default-points sawyer/baseline-font-size
          textsize-pixel-pitch-thresholds nil
          ;; NOTE: to get the current monitor's dimensions:
          ;; (textsize--monitor-size-mm (selected-frame))
          textsize-monitor-size-thresholds
          '(
            (286 . -2)    ;; 14" MBP
            (344 . -1)    ;; 16" MBP
            (530 . -1)   ;; 24" 4K (full-res)
            ;;(801 . -1)    ;; 34" superwide
            ))))

(add-hook! after-init #'textsize-mode)

;;; Extra minor-mode/appearance tweaks

(use-package! info-colors :commands (info-colors-fontify-node))
(add-hook 'Info-selection-hook 'info-colors-fontify-node)

(after! vertico-posframe
  (setq vertico-posframe-border-width 10)
  (custom-set-faces! `(vertico-posframe-border :background ,(doom-color 'bg))))

(add-hook! highlight-indent-guides-mode
  (custom-set-faces!
    `(highlight-indent-guides-odd-face :inherit default :background ,(doom-lighten 'bg 0.05))
    `(highlight-indent-guides-even-face :inherit default :background ,(doom-lighten 'bg 0.08))))

(after! kanagawa-theme
  (setq kanagawa-theme-normal-weight 'light)
  (setq kanagawa-theme-heavier-weight 'medium))

(defun sawyer/bold-to-medium ()
  "Iterate over all faces and set those with bold weight to medium."
  (interactive)
  (dolist (face (face-list))
    (let ((current-weight (face-attribute face :weight nil t)))
      (when (eq current-weight 'bold)
        (set-face-attribute face nil :weight 'medium)))))

;;; Load default theme at startup
(load-theme sawyer/dark-theme t)
(add-hook! after-init
  (when ns-system-appearance
    (setq ns-system-appearance-change-functions '(sawyer/auto-set-theme))
    (message "setup auto-theme switching via ns-system-appearance-change-functions"))
  (sawyer/auto-set-theme))
