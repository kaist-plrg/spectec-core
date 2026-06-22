;;; spectec-ts-mode.el --- Major mode for SpecTecX specs, using tree-sitter -*- lexical-binding: t; -*-

;; Copyright (C) 2026  kaist-plrg

;; Package-Requires: ((emacs "29.1"))
;; URL: https://github.com/kaist-plrg/spectecx

;; This file is distributed under the Apache License, Version 2.0.

;;; Commentary:

;; Tree-sitter major mode for SpecTecX specs (`.spectec').  Font-lock mirrors
;; the grammar's `highlights.scm'; meta-notation sits at font-lock level 4 and
;; recedes until then.

;;; Code:

(require 'treesit)

(defgroup spectec-ts nil
  "Major mode for SpecTecX specifications."
  :group 'languages
  :prefix "spectec-ts-")

(defface spectec-ts-tag-face
  '((t :inherit font-lock-constant-face))
  "Face for SpecTecX object syntax (atoms, tags, operators, brackets)."
  :group 'spectec-ts)

;;;###autoload
(add-to-list 'treesit-language-source-alist
             '(spectec "https://github.com/KunJeong/tree-sitter-spectec")
             t)

(defvar spectec-ts-mode--syntax-table
  (let ((table (make-syntax-table)))
    ;; `;;' begins a line comment; quote/backtick are atom markers, not strings.
    (modify-syntax-entry ?\; ". 12b" table)
    (modify-syntax-entry ?\n ">   b" table)
    (modify-syntax-entry ?\' "." table)
    (modify-syntax-entry ?\` "." table)
    (modify-syntax-entry ?\" "\"" table)
    table)
  "Syntax table for `spectec-ts-mode'.")

(defvar spectec-ts-mode--font-lock-settings
  (treesit-font-lock-rules
   :language 'spectec
   :feature 'comment
   '((comment) @font-lock-comment-face
     (separator) @font-lock-comment-face
     ["#"] @font-lock-comment-face)

   :language 'spectec
   :feature 'string
   '((text_literal) @font-lock-string-face
     (hint_text) @font-lock-string-face
     (hint_operator) @font-lock-string-face
     (hint_function_id) @font-lock-string-face
     (hint_latex) @font-lock-doc-face
     (hint_placeholder) @font-lock-escape-face)

   :language 'spectec
   :feature 'keyword
   '(["syntax" "var" "relation" "rule" "dec" "def" "if" "hint"]
     @font-lock-keyword-face
     "--" @font-lock-keyword-face
     (else_premise) @font-lock-keyword-face
     (if_premise "if" @font-lock-keyword-face))

   :language 'spectec
   :feature 'type
   '([(bool_type) (text_type) (tuple_type) (iterated_type)] @font-lock-type-face
     (plain_type) @font-lock-type-face
     (syntax_definition (syntax_id) @font-lock-type-face)
     (syntax_declaration (syntax_id) @font-lock-type-face)
     (type_parameters (lowercase_id) @font-lock-type-face)
     (type) @font-lock-type-face)

   :language 'spectec
   :feature 'constant
   '((boolean_literal) @font-lock-constant-face
     (epsilon_literal) @font-lock-constant-face)

   :language 'spectec
   :feature 'number
   '((number_literal) @font-lock-number-face)

   :language 'spectec
   :feature 'function
   '((call_expression (function_id) @font-lock-function-call-face)
     (function_declaration name: (function_id) @font-lock-function-name-face)
     (function_definition name: (function_id) @font-lock-function-name-face)
     (rule_definition rule_name: (rule_id) @font-lock-function-name-face)
     (relation_declaration name: (relation_id) @font-lock-function-name-face)
     (rule_definition relation_name: (relation_id) @font-lock-function-name-face)
     (rule_premise relation_name: (relation_id) @font-lock-function-name-face)
     (hint_name) @font-lock-builtin-face)

   ;; A coarser binding/use split than the grammar's highlights.scm, as
   ;; Emacs 29.1 treesit currently lacks support for `#has-ancestor?` predicate.
   :language 'spectec
   :feature 'variable
   '((regular_id) @font-lock-variable-use-face
     (variable_definition name: (syntax_id) @font-lock-variable-name-face)
     (value_pattern (regular_id) @font-lock-variable-name-face)
     (constructor_pattern_arg (regular_id) @font-lock-variable-name-face))

   :language 'spectec
   :feature 'tag
   '((constructor_id) @spectec-ts-tag-face
     (operator) @spectec-ts-tag-face
     ["`(" "`)" "`[" "`]" "`{" "`}" "`<" "`>"] @spectec-ts-tag-face)

   :language 'spectec
   :feature 'operator
   '((atom_infix) @font-lock-operator-face
     (atom_relational) @font-lock-operator-face
     (notation_rel operator: (_) @font-lock-operator-face)
     (notation_bin operator: (_) @font-lock-operator-face)
     ["?" "*"] @font-lock-operator-face)

   :language 'spectec
   :feature 'bracket
   '(["(" "[" ")" "]"] @font-lock-bracket-face
     (type_parameters ["<" ">"] @font-lock-bracket-face))

   :language 'spectec
   :feature 'delimiter
   '([":" "," "." "|" "/"] @font-lock-delimiter-face))
  "Tree-sitter font-lock settings for `spectec-ts-mode'.")

(defun spectec-ts-mode--ensure-grammar ()
  "Offer to build the SpecTecX tree-sitter grammar when it is missing.
Skipped in batch sessions and once the grammar is available."
  (when (and (not noninteractive)
             (treesit-available-p)
             (not (treesit-ready-p 'spectec t))
             (y-or-n-p "SpecTecX tree-sitter grammar is not installed.  Build it now? "))
    (treesit-install-language-grammar 'spectec)))

;;;###autoload
(define-derived-mode spectec-ts-mode prog-mode "SpecTecX"
  "Major mode for editing SpecTecX specifications, powered by tree-sitter."
  :syntax-table spectec-ts-mode--syntax-table
  :group 'spectec-ts
  (setq-local comment-start ";; ")
  (setq-local comment-end "")
  (setq-local comment-start-skip ";;+[ \t]*")
  (spectec-ts-mode--ensure-grammar)
  (when (treesit-ready-p 'spectec)
    (treesit-parser-create 'spectec)
    (setq-local treesit-font-lock-settings spectec-ts-mode--font-lock-settings)
    (setq-local treesit-font-lock-feature-list
                '((comment string)
                  (keyword type constant number)
                  (function variable tag)
                  (operator bracket delimiter)))
    (treesit-major-mode-setup)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.spectec\\'" . spectec-ts-mode))

;; Use eglot, if present, for diagnostics; finds `spectecx-lsp' on PATH.
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(spectec-ts-mode . ("spectecx-lsp"))))

(provide 'spectec-ts-mode)
;;; spectec-ts-mode.el ends here
