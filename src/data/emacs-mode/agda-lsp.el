;;; agda-lsp.el --- Major mode for Agda using LSP

;;; Commentary:

;; A major mode for editing Agda using the Language Server Protocol

;;; Code:

(require 'agda-input)
(require 'agda2-abbrevs)

(defgroup agda-lsp-mode nil
  "Major mode for interactively developing Agda programs."
  :group 'languages)

(defcustom agda-lsp-mode-program-name "agda"
  "The path to the Agda executable.

This should be a string containing the name or path of the
executable.  This defaults to \"agda\"."
  :type '(choice (const :tag "Default executable" "agda")
                  (file :tag "Name or path"))
  :tag "Agda executable"
  :group 'agda-lsp-mode
  :risky t)

(defconst agda-lsp-mode--keywords
  '("abstract" "codata" "coinductive" "constructor" "data" "do"
    "eta-equality" "field" "forall" "import" "in" "inductive"
    "infix" "infixl" "infixr" "instance" "interleaved" "let"
    "macro" "module" "mutual" "no-eta-equality" "open" "overlap"
    "pattern" "postulate" "primitive" "private" "quote"
    "quoteTerm" "record" "rewrite" "syntax" "tactic" "unquote"
    "unquoteDecl" "unquoteDef" "variable" "where" "with" "opaque"
    "unfolding" "using" "hiding" "renaming" "public")
  "All Agda keywords.")

(defconst agda-lsp-mode--font-lock
  (list
   ;; Keywords
   `(,(regexp-opt agda-lsp-mode--keywords 'words) . font-lock-keyword-face)))

(defvar agda-lsp-mode--syntax-table
  (let ((tbl (make-syntax-table)))
    ;; Set the syntax of every char to "w" except for those whose default
    ;; syntax in `standard-syntax-table' is `paren' or `whitespace'.
    (map-char-table (lambda (keys val)
                      ;; `keys' here can be a normal char, a generic char
                      ;; (Emacs<23), or a char range (Emacs>=23).
                      (unless (memq (car val)
                                    (eval-when-compile
                                      (mapcar 'car
                                              (list (string-to-syntax "(")
                                                    (string-to-syntax ")")
                                                    (string-to-syntax " ")))))
                        (modify-syntax-entry keys "w" tbl)))
                    (standard-syntax-table))
    ;; Then override the remaining special cases.
    (dolist (cs '((?- . "w 12") (?\n . ">")
                  (?. . ".") (?\; . ".") (?! . ".")))
      (modify-syntax-entry (car cs) (cdr cs) tbl))
    tbl)
  "Syntax table used by the Agda mode:

-   | Comment character, word constituent.
\n  | Comment ender.
.;! | Punctuation.

Remaining characters inherit their syntax classes from the
standard syntax table if that table treats them as matching
parentheses or whitespace.  Otherwise they are treated as word
constituents.")

;;;###autoload
(define-derived-mode agda-lsp-mode prog-mode "Agda"
  "Major mode for editing Agda code."

  ;; Empty lines delimit paragraphs
  (setq-local paragraph-start "\\s-*$")
  (setq-local paragraph-separate paragraph-start)

  (setq-local comment-start "-- ")
  ;; Use the syntax table for locating comments
  (setq-local comment-use-syntax t)

  (setq-local eglot-confirm-server-initiated-edits nil)

  ;; Syntax highlighting
  (set-syntax-table agda-lsp-mode--syntax-table)
  (setq-local font-lock-defaults '(agda-lsp-mode--font-lock))

  ;; Templates
  (setq local-abbrev-table agda2-mode-abbrev-table)

  ;; Agda input
  ;; Protect global value of default-input-method from set-input-method.
  (make-local-variable 'default-input-method)
  ;; Don't take script into account when determining word boundaries
  (set (make-local-variable 'word-combining-categories) (cons '(nil . nil) word-combining-categories))
  (set-input-method "Agda")

  ; Enable lsp-mode or eglot if available
  (cond
    ((fboundp 'lsp-mode) (lsp-mode t) (lsp-semantic-tokens-mode t))
    ((fboundp 'eglot) (eglot t))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.l?agda\\'" . agda-lsp-mode))

;;;###autoload
(modify-coding-system-alist 'file "\\.l?agda\\'" 'utf-8)

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
    '(agda-lsp-mode . ("agda" "--lsp"))))

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration '(agda-lsp-mode . "agda"))

  (lsp-register-client
    (make-lsp-client
      :new-connection
      (lsp-stdio-connection (lambda () `(,agda-lsp-mode-program-name "--lsp")))
      :activation-fn (lsp-activate-on "agda")
      :priority -1
      :server-id 'agda)))

(provide 'agda-lsp)
;;; agda-lsp.el ends here
