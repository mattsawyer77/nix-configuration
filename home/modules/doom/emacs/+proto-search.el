;;;  -*- lexical-binding: t; -*-
;;; Commentary: emacs package to provide finding proto definitions, since there is no lsp server that really works

(define-derived-mode proto-nav-mode protobuf-mode
  "proto-nav-mode"
  "minor mode for finding the definition of a protobuf message.")

(add-to-list 'auto-mode-alist '("\\.proto\\'" . proto-nav-mode))

(defcustom proto-nav-proto-roots nil
  "path(s) to search for proto files
-- defaults to the current buffer's git root directory"
  :type '(list))

(require 'cl-lib)
(require 'vc-git)
(require 'consult)
(require 'vertico)

(defun proto-nav--root-dirs ()
  "get a list of candidate directories to search for proto files based on
the buffer's directory"
  (if (and (boundp 'flycheck-protoc-import-path)
           (not (eq flycheck-protoc-import-path nil)))
      flycheck-protoc-import-path
    (list (vc-git-root (file-name-as-directory
                        (or (buffer-file-name) default-directory))))))

(defun proto-nav--normalize-dirs (dirs)
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

(defun proto-nav-project-find-message-def (&optional initial-query directory)
  "Performs a live project search for the proto message definition from the
project root dir (and any proto root dirs defined in
flycheck-protoc-import-path) using ripgrep."
  (interactive "s")
  (let* ((extra-args '("--type-add=proto:*.proto" "--type=proto" "--only-matching"))
         (extra-dirs (proto-nav--root-dirs))
         (final-dirs (proto-nav--normalize-dirs extra-dirs))
         (all-args (append extra-args final-dirs))
         (query (format "^message\\s+%s\\b" (regexp-quote initial-query))))
    (+vertico-file-search
      :query query
      :in directory
      :args all-args)))

(defun proto-nav-project-find-message-refs (&optional initial-query directory)
  "Performs a live project search for references to the proto message
definition from the project root using ripgrep."
  (interactive "s")
  (let* ((extra-args '("--type-add=proto:*.proto" "--type=proto" "--only-matching"))
         (extra-dirs (proto-nav--root-dirs))
         (final-dirs (proto-nav--normalize-dirs extra-dirs))
         (all-args (append extra-args final-dirs))
         (query (format "^\\s*[\\w\\._]*\\b%s\\b.*=\\s*\\d+" (regexp-quote initial-query))))
    (+vertico-file-search
      :query query
      :in directory
      :args all-args)))

(defun proto-nav-project-find-message-def-at-point ()
  "Performs a live project search for the definition of the proto message at point."
  (interactive)
  (proto-nav-project-find-message-def (thing-at-point 'symbol)))

(defun proto-nav-project-find-message-refs-at-point ()
  "Performs a live project search for refs to the proto message at point."
  (interactive)
  (proto-nav-project-find-message-refs (thing-at-point 'symbol)))

(defun proto-nav-project-find-message-def-at-point-in-buffer ()
  "Performs a live project search for the definition of the proto message at point."
  (interactive)
  (let* ((query (format "^message[[:space:]]+%s\\b" (regexp-quote (thing-at-point 'symbol)))))
    (consult-line query)))
