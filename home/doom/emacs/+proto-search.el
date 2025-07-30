;;;  -*- lexical-binding: t; -*-
;;; Commentary: emacs package to provide finding proto definitions, since there is no lsp server that really works

(define-derived-mode protobuf-plus-mode protobuf-mode
  "protobuf-plus-mode"
  "minor mode for finding the definition of a protobuf message.")

(add-to-list 'auto-mode-alist '("\\.proto\\'" . protobuf-plus-mode))
(require 'vc-git)
(defcustom protobuf-plus-proto-root nil
  "path to search for proto files
-- defaults to the current buffer's git root directory"
  :type '(string))

(defun protobuf-plus-find-message-def (name)
  "Find the .proto file that defines message NAME.
If exactly one hit, jump there.  If multiple, use completing-read (Vertico)
so you can pick one."
  (interactive "sProto message name: ")
  (let* ((search-dir (if (eq protobuf-plus-proto-root nil)
                         (vc-git-root (file-name-as-directory (or (buffer-file-name) default-directory)))
                       protobuf-plus-proto-root
                       ))
         ;; (_ (message "resolved search-dir: %s" search-dir))
         (cmd
          (if (executable-find "rg")
              (format "rg --color=never --glob '*.proto' --no-heading --line-number --sort=path --only-matching '^message\\s+%s\\b' %s"
                      (shell-quote-argument name)
                      search-dir)
            (format "grep -R -n -w --include='*.proto' -E '^message[[:space:]]+%s\\b' %s"
                    (shell-quote-argument name) search-dir)))
         ;; (_ (message "about to search using command: %s" cmd))
         (raw (shell-command-to-string cmd))
         ;; (_ (message "search output: %s" raw))
         (lines (split-string raw "\n" t))
         ;; build (display-string . (file . line-num))
         (cands
          (mapcar
           (lambda (ln)
             (let* ((parts    (split-string ln ":" t))
                    (file     (car parts))
                    (lnum     (string-to-number (cadr parts)))
                    (content  (string-trim (mapconcat #'identity (cddr parts) ":")))
                    (disp     (format "%s:%d: %s" file lnum content)))
               (cons disp (cons file lnum))))
           lines)))
    (cond
     ((null cands)
      (user-error "No message '%s' found under %s" name search-dir))
     ((= 1 (length cands))
      (let* ((target (cdar cands)))
        (find-file (car target))
        (goto-char (point-min))
        (forward-line (1- (cdr target)))))
     (t
      (let* ((choice (completing-read "Pick proto: " (mapcar #'car cands) nil t))
             (target (cdr (assoc choice cands))))
        (find-file (car target))
        (goto-char (point-min))
        (forward-line (1- (cdr target))))))))

(defun protobuf-plus-find-message-def-at-point ()
  "Call `protobuf-plus-find-message-def` on the symbol at point."
  (interactive)
  (let ((sym (thing-at-point 'symbol t)))
    (if (and sym (not (string-empty-p sym)))
        (protobuf-plus-find-message-def sym)
      (user-error "No symbol at point"))))

;;; Search for *references* to a proto message, not its definition.
(defun protobuf-plus-find-message-references (name)
  "Find references to proto message NAME in all .proto files under
`protobuf-plus-proto-root`. Uses `rg` (ripgrep) if available,
otherwise falls back to `grep -R`. On a single hit, jump there.
On multiple hits, prompt with `completing-read`."
  (interactive "sProto message to search for: ")
  (let* ((quoted-name (shell-quote-argument name))
         (search-dir (if (eq protobuf-plus-proto-root nil)
                         (vc-git-root (file-name-as-directory (or (buffer-file-name) default-directory)))
                       protobuf-plus-proto-root
                       ))
         (cmd
          (if (executable-find "rg")
              (format "rg --line-number --no-heading --color never --glob '*.proto' --sort=path --only-matching '^\s*[\\w\\._]*\\b%s\\b.*=\\s*\\d+' %s"
                      quoted-name search-dir)
            (format "grep -R -n -w --include='*.proto' '^[[:space:]]*[\\w\\._]*\\b%s\\b.*=[[:space:]]*\\d+' %s"
                    quoted-name search-dir)))
         (_ (message "about to execute command: %s" cmd))
         )
    (let* ((raw-lines (split-string (shell-command-to-string cmd) "\n" t))
           (cands
            (mapcar
             (lambda (ln)
               (let* ((parts   (split-string ln ":" t))
                      (file    (car parts))
                      (lnum    (string-to-number (cadr parts)))
                      (txt     (mapconcat #'identity (cddr parts) ":"))
                      (summary (string-trim txt))
                      (disp    (format "%s:%d: %s" file lnum summary)))
                 ;; each candidate is (DISPLAY . (FILE . LINENUM))
                 (cons disp (cons file lnum))))
             raw-lines)))
      (cond
       ((null cands)
        (user-error "No references to '%s' found under %s" name protobuf-plus-proto-root))
       ((= (length cands) 1)
        (let* ((target (cdar cands)))
          (find-file (car target))
          (goto-char (point-min))
          (forward-line (1- (cdr target)))))
       (t
        (let* ((choice (completing-read "Choose reference: "
                                        (mapcar #'car cands) nil t))
               (target (cdr (assoc choice cands))))
          (find-file (car target))
          (goto-char (point-min))
          (forward-line (1- (cdr target)))))))))

(defun protobuf-plus-find-message-references-at-point ()
  "Call `protobuf-plus-find-message-references` on the symbol at point."
  (interactive)
  (let ((sym (thing-at-point 'symbol t)))
    (if (and sym (not (string-empty-p sym)))
        (protobuf-plus-find-message-references sym)
      (user-error "No symbol at point"))))
