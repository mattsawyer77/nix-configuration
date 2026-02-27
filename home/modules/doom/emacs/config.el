;;;  -*- lexical-binding: t; -*-

;; suppress annoying elisp compiler warnings
(setq warning-minimum-level :emergency)
(after! comp
  ;; HACK Disable native-compilation for some troublesome packages
  (mapc (doom-partial #'add-to-list 'native-comp-deferred-compilation-deny-list)
        (list "/emacs-jupyter.*\\.el\\'"
              "/evil-collection-vterm\\.el\\'"
              ;; "/with-editor\\.el\\'"
              )))
(setq comp-speed 2
      comp-deferred-compilation t)
(load! "+go-template-mode")
(load! "+proto-search")
(load! "+editor")
(load! "+ui")
(load! "+keybindings")
