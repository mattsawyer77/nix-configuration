;;;  -*- lexical-binding: t; -*-

(when (fboundp 'up-slightly) (map! "<mouse-4>" 'up-slightly))
(when (fboundp 'down-slightly) (map! "<mouse-5>" 'down-slightly))

;; TODO: unmap +/- keys in view-mode-map
(map!
 :map general-override-mode-map
 :g "C-s" #'basic-save-buffer
 :g "s-t" #'eat-project
 :g "<f3>" #'rotate-layout
 :n "C-," (lambda ()
            (interactive)
            (dired-other-window "~/workspaces/nix-configuration/home/doom/emacs"))
 (:after lsp-ui
  :nv "<f1>" (lambda ()
               (interactive)
               (lsp-ui-doc-show))
  (:leader
   (:prefix ("o" . "open")
    :desc "lsp-ui-imenu" "i" #'lsp-ui-imenu))
  )
 (:after tabspaces
  :nv "C-SPC" #'tabspaces-switch-or-create-workspace
  :nv "C-t" #'tabspaces-open-or-create-project-and-workspace
  :nv "s-}" #'tab-bar-switch-to-next-tab
  :nv "s-{" #'tab-bar-switch-to-prev-tab
  (:leader
   (:prefix ("TAB" . "Tab")
    :desc "Switch Tab" "TAB" #'tabspaces-switch-or-create-workspace
    :desc "Close Tab" "x" #'tabspaces-kill-buffers-close-workspace)
   (:prefix ("p" . "+project")
    :desc "Switch project" "p" #'tabspaces-open-or-create-project-and-workspace
    :desc "Add new project" "a" #'tabspaces-open-or-create-project-and-workspace)))
 (:after emacs-eat
  :nv "<f8>" #'eat-project
  (:leader
   (:prefix ("o" . "+open")
    :desc "Open EAT" "e" #'eat-project
    )))
 ;; (:after flycheck
 ;; :leader
 ;; (:prefix ("e" . "errors")
 ;; :desc "go to next error" "n" #'flycheck-next-error
 ;; :desc "go to previous error" "p" #'flycheck-previous-error
 ;; :desc "verify Flycheck setup" "v" #'flycheck-verify-setup
 ;; :desc "list errors" "l" #'flycheck-list-errors))
 (:leader
  (:after projectile
   :desc "Switch to last project buffer" "`" #'projectile-project-buffers-other-buffer)
  (:prefix ("f" . "file")
   :desc "Ediff buffers" "c" #'ediff-buffers)
  (:after vertico
   :leader
   :desc "Repeat last Vertico search" "\"" #'vertico-repeat-select))
 )

;;; XXX: not working
;; (after! (evil-nerd-commenter evil-commands)
(add-hook! prog-mode
  (map! :map general-override-mode-map
        :mode prog-mode
        :nv "#" #'evilnc-comment-or-uncomment-lines
        :nv "TAB" #'evil-jump-item))

(after! general
  (map! :map general-override-mode-map
        :nv "g d" #'+lookup/definition
        :nv "g t" #'+lookup/type-definition
        :nv "g r" #'+lookup/references
        :nv "g I" #'+lookup/implementations
        :nv "g a" #'projectile-project-buffers-other-buffer
        :nv "g b" #'consult-buffer)
  )

;; (map! :after flycheck
;; (:leader
;; (:prefix ("e" . "errors")
;; :desc "go to next error" "n" #'flycheck-next-error
;; :desc "go to previous error" "p" #'flycheck-previous-error
;; :desc "verify Flycheck setup" "v" #'flycheck-verify-setup
;; :desc "list errors" "l" #'flycheck-list-errors)))

(map! :after impatient-showdown-mode
      :mode markdown-mode
      (:localleader
       :desc "Impatient Showdown live preview" "p" #'impatient-showdown-mode))

(map! :after rustic
      :map rustic-mode-map
      (:localleader
       (:prefix ("c" . "check")
        :desc "cargo check" "c" (cmd! (compile "cargo check"))
        )))

(map! :after evil-org
      :map evil-org-mode-map
      :mode org-mode
      :i "TAB" #'evil-org->
      :i "S-TAB" #'evil-org-<
      :nv "C-j" #'org-move-subtree-down
      :nv "C-k" #'org-move-subtree-up
      :nv "C-n" #'org-move-item-down
      :nv "C-p" #'org-move-item-up
      :i "RET" #'evil-org-return
      :n "z O" #'evil-open-fold-rec
      (:localleader
       :nv "-" #'org-cycle-list-bullet))

(map! :after expand-region
      :v "v" #'er/expand-region
      :v "V" #'er/contract-region)

(map! :after smerge-mode
      :map smerge-mode-map
      (:leader
       (:prefix ("g" . "smerge")
        :desc "next hunk" "n" #'smerge-next
        :desc "prev hunk" "p" #'smerge-prev
        :desc "keep my version" "M" #'smerge-keep-upper
        :desc "keep my version" "m" #'smerge-keep-upper
        :desc "keep their version" "T" #'smerge-keep-lower
        )))

(map! :mode rfc-mode
      :map rfc-mode-map
      :g "C-f" #'rfc-mode-forward-page
      :g "C-b" #'rfc-mode-backward-page
      :g "<next>" #'rfc-mode-forward-page
      :g "<prior>" #'rfc-mode-backward-page
      )

(map! :after magit
      :mode magit-blame-mode
      (:leader
       :nv "g v" #'magit-visit-ref))

(map! :after dap-mode
      (:map dap-ui-session-mode-map
       :nv "<f5>" #'dap-continue
       :nv "<f6>" #'dap-next
       :nv "<f7>" #'dap-step-out
       :nv "<f8>" #'dap-step-in)
      :leader
      (:prefix ("d" . "debug")
       :desc "DAP Debug" "d" #'dap-debug
       :desc "DAP Hydra" "h" #'dap-hydra
       :desc "DAP Disconnect" "Q" #'dap-disconnect
       (:prefix ("b" . "breakpoint")
        :desc "DAP Breakpoint Add" "a" #'dap-breakpoint-add
        :desc "DAP Breakpoint Delete" "d" #'dap-breakpoint-delete
        :desc "DAP Breakpoint Delete All" "D" #'dap-breakpoint-delete
        )))

(map! :after textsize
      (:map global-map
       :nv "s-0" #'textsize-reset
       :nv "s--" #'textsize-decrement
       :nv "s-=" #'textsize-increment))
(map! :after cov
      :leader (:map global-map :nv "c v" #'cov-mode))
(map! :after general
      (:nv "<f5>" #'gptel
       :nv "C-<f5>" #'gptel-menu))

(set-lookup-handlers! 'proto-nav-mode
  :definition #'proto-nav-project-find-message-def-at-point
  :references #'proto-nav-project-find-message-refs-at-point)
