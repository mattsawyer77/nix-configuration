;;; argonaut-theme.el --- derived from Gogh Argonaut/Doom Vibrant -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Added: March 28, 2024
;; Author: <https://github.com/mattswyer77>
;; Maintainer:
;;
;;; Commentary:
;;; Code:

(require 'doom-themes)

;; AaBbMmYyZz - #232323
;; AaBbMmYyZz - #FF000F
;; AaBbMmYyZz - #8CE10B
;; AaBbMmYyZz - #FFB900
;; AaBbMmYyZz - #008DF8
;; AaBbMmYyZz - #6D43A6
;; AaBbMmYyZz - #00D8EB
;; AaBbMmYyZz - #FFFFFF
;; AaBbMmYyZz - #444444
;; AaBbMmYyZz - #FF2740
;; AaBbMmYyZz - #ABE15B
;; AaBbMmYyZz - #FFD242
;; AaBbMmYyZz - #0092FF
;; AaBbMmYyZz - #9A5FEB
;; AaBbMmYyZz - #67FFF0
;; AaBbMmYyZz - #FFFFFF


;;
;;; Variables

(defgroup doom-argonaut-theme nil
  "Options for the `doom-argonaut' theme."
  :group 'doom-themes)

(defcustom doom-argonaut-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-argonaut-theme
  :type 'boolean)

(defcustom doom-argonaut-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-argonaut-theme
  :type 'boolean)

(defcustom doom-argonaut-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'doom-argonaut-theme
  :type '(choice integer boolean))


;;
;;; Theme definition

(def-doom-theme doom-argonaut
                "A dark theme based off of doom-one with more vibrant colors."

                ;; name        gui       256           16
                ((bg         '("#242730" "black"       "black" ))
                 (fg         '("#bbc2cf" "#bfbfbf"     "brightwhite" ))

                 ;; These are off-color variants of bg/fg, used primarily for `solaire-mode',
                 ;; but can also be useful as a basis for subtle highlights (e.g. for hl-line
                 ;; or region), especially when paired with the `doom-darken', `doom-lighten',
                 ;; and `doom-blend' helper functions.
                 (bg-alt     '("#2a2e38" "black"       "black"       ))
                 (fg-alt     '("#5D656B" "#5d5d5d"     "white"       ))

                 ;; These should represent a spectrum from bg to fg, where base0 is a starker
                 ;; bg and base8 is a starker fg. For example, if bg is light grey and fg is
                 ;; dark grey, base0 should be white and base8 should be black.
                 (base0      '("#1c1f24" "#101010"     "black"       ))
                 (base1      '("#1c1f24" "#1e1e1e"     "brightblack" ))
                 (base2      '("#21272d" "#21212d"     "brightblack" ))
                 (base3      '("#23272e" "#262626"     "brightblack" ))
                 (base4      '("#484854" "#5e5e5e"     "brightblack" ))
                 (base5      '("#62686E" "#666666"     "brightblack" ))
                 (base6      '("#757B80" "#7b7b7b"     "brightblack" ))
                 (base7      '("#9ca0a4" "#979797"     "brightblack" ))
                 (base8      '("#DFDFDF" "#dfdfdf"     "white"       ))

                 (grey       base4)
                 (red        '("#FF2740" "#ff6655" "red"             ))
                 (orange     '("#FFB900" "#dd8844" "brightred"       ))
                 (green      '("#8CE10B" "#99bb66" "green"           ))
                 (teal       '("#00D8EB" "#44b9b1" "brightgreen"     ))
                 (yellow     '("#FFD242" "#ECBE7B" "yellow"          ))
                 (blue       '("#0092FF" "#51afef" "brightblue"      ))
                 (dark-blue  '("#008DF8" "#2257A0" "blue"            ))
                 (magenta    '("#9A5FEB" "#c678dd" "brightmagenta"   ))
                 (violet     '("#6D43A6" "#a9a1e1" "magenta"         )) ;a9a1e1
                 (cyan       '("#67FFF0" "#46D9FF" "brightcyan"      ))
                 (dark-cyan  '("#00D8EB" "#5699AF" "cyan"            ))

                 ;; These are the "universal syntax classes" that doom-themes establishes.
                 ;; These *must* be included in every doom themes, or your theme will throw an
                 ;; error, as they are used in the base theme defined in doom-themes-base.
                 (highlight      blue)
                 (vertical-bar   base0)
                 (selection      dark-blue)
                 (builtin        magenta)
                 (comments       (if doom-argonaut-brighter-comments dark-cyan base5))
                 (doc-comments   (if doom-argonaut-brighter-comments (doom-lighten dark-cyan 0.15) (doom-lighten base4 0.3)))
                 (constants      violet)
                 (functions      cyan)
                 (keywords       blue)
                 (methods        violet)
                 (operators      magenta)
                 (type           yellow)
                 (strings        green)
                 (variables      (doom-lighten magenta 0.4))
                 (numbers        orange)
                 (region         "#3d4451")
                 (error          red)
                 (warning        yellow)
                 (success        green)
                 (vc-modified    yellow)
                 (vc-added       green)
                 (vc-deleted     red)

                 ;; These are extra color variables used only in this theme; i.e. they aren't
                 ;; mandatory for derived themes.
                 (modeline-fg             fg)
                 (modeline-fg-inactive    (doom-blend blue grey (if doom-argonaut-brighter-modeline 0.9 0.2)))
                 (modeline-bg             (if doom-argonaut-brighter-modeline
                                              `("#383f58" ,@(cdr base1))
                                            `(,(doom-darken (car bg) 0.15) ,@(cdr base1))))
                 (modeline-bg-alt         (if doom-argonaut-brighter-modeline
                                              modeline-bg
                                            `(,(car bg-alt) ,@(cdr base0))))
                 (modeline-bg-inactive     `(,(doom-darken (car bg-alt) 0.2) ,@(cdr base0)))
                 (modeline-bg-alt-inactive (doom-darken bg 0.25))

                 (-modeline-pad
                  (when doom-argonaut-padded-modeline
                    (if (integerp doom-argonaut-padded-modeline) doom-argonaut-padded-modeline 4))))


  ;;;; Base theme face overrides
                (((font-lock-comment-face &override)
                  :background (if doom-argonaut-brighter-comments (doom-darken bg-alt 0.095) 'unspecified))
                 ((line-number &override) :foreground base4)
                 ((line-number-current-line &override) :foreground blue :bold bold)
                 (mode-line
                  :background modeline-bg :foreground modeline-fg
                  :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
                 (mode-line-inactive
                  :background modeline-bg-inactive :foreground modeline-fg-inactive
                  :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
                 (mode-line-emphasis :foreground (if doom-argonaut-brighter-modeline base8 highlight))
                 (org-block :background (doom-darken base3 0.1))

   ;;;; all-the-icons
                 ((all-the-icons-dblue &override) :foreground dark-cyan)
   ;;;; centaur-tabs
                 (centaur-tabs-unselected :background bg-alt :foreground base6)
   ;;;; css-mode <built-in> / scss-mode
                 (css-proprietary-property :foreground orange)
                 (css-property             :foreground green)
                 (css-selector             :foreground blue)
   ;;;; doom-modeline
                 (doom-modeline-bar
                  :background (if doom-argonaut-brighter-modeline modeline-bg highlight))
                 (doom-modeline-buffer-path
                  :foreground (if doom-argonaut-brighter-modeline base8 blue) :bold bold)
   ;;;; elscreen
                 (elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")
   ;;;; markdown-mode
                 (markdown-header-face :inherit 'bold :foreground red)
   ;;;; solaire-mode
                 (solaire-mode-line-face
                  :inherit 'mode-line
                  :background modeline-bg-alt
                  :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-alt)))
                 (solaire-mode-line-inactive-face
                  :inherit 'mode-line-inactive
                  :background modeline-bg-alt-inactive
                  :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-alt-inactive)))
   ;;;; whitespace <built-in>
                 (whitespace-empty :background base2))

  ;;;; Base theme variable overrides
                ;; ()
                )

;;; doom-argonaut-theme.el ends here
