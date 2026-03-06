;;; combobulate-rust.el --- Rust support for combobulate -*- lexical-binding: t; -*-
(require 'combobulate-settings)
(require 'combobulate-navigation)
(require 'combobulate-setup)
(require 'combobulate-manipulation)
(require 'combobulate-rust-rules)

(defgroup combobulate-rust nil
  "Configuration switches for Rust."
  :group 'combobulate
  :prefix "combobulate-rust-")

(defun combobulate-rust-pretty-print-node-name (node default-name)
  "Pretty printer for Rust nodes."
  (combobulate-string-truncate
   (replace-regexp-in-string
    (rx (| (>= 2 " ") "\n")) ""
    (pcase (combobulate-node-type node)
      ("function_item"
       (concat "fn "
               (combobulate-node-text (combobulate-node-child-by-field node "name"))))
      ("struct_item"
       (concat "struct "
               (combobulate-node-text (combobulate-node-child-by-field node "name"))))
      ("enum_item"
       (concat "enum "
               (combobulate-node-text (combobulate-node-child-by-field node "name"))))
      ("impl_item"
       (concat "impl "
               (combobulate-node-text (combobulate-node-child-by-field node "type"))))
      ("trait_item"
       (concat "trait "
               (combobulate-node-text (combobulate-node-child-by-field node "name"))))
      ("identifier" (combobulate-node-text node))
      ("type_identifier" (combobulate-node-text node))
      (_ default-name)))
   40))

(eval-and-compile
  (defvar combobulate-rust-definitions
    '((context-nodes
       '("identifier" "type_identifier" "field_identifier"
         "integer_literal" "float_literal" "string_literal"
         "char_literal" "boolean_literal" "self" "primitive_type"))
      (indent-after-edit nil)
      (envelope-indent-region-function #'indent-region)
      (pretty-print-node-name-function #'combobulate-rust-pretty-print-node-name)
      (procedures-edit nil)
      (procedures-sexp nil)
      (plausible-separators '("," ";" "\n"))
      (procedures-defun
       '((:activation-nodes
          ((:nodes ("function_item" "struct_item" "enum_item"
                    "impl_item" "trait_item" "mod_item"))))))
      (procedures-logical
       '((:activation-nodes ((:nodes (all))))))
      (procedures-sibling
       `(;; Top-level items and statements (source_file, block, declaration_list)
         (:activation-nodes
          ((:nodes ((rule "_declaration_statement") "expression_statement")
                   :has-parent ("source_file" "block" "declaration_list")))
          :selector (:choose parent :match-children t))
         ;; Match arms
         (:activation-nodes
          ((:nodes ("match_arm") :has-parent ("match_block")))
          :selector (:choose parent :match-children (:match-rules ("match_arm"))))
         ;; Function parameters
         (:activation-nodes
          ((:nodes ("parameter" "self_parameter")
                   :has-parent ("parameters")))
          :selector (:choose parent :match-children t))
         ;; Arguments in function calls
         (:activation-nodes
          ((:nodes ("arguments") :has-parent ("call_expression" "macro_invocation")))
          :selector (:choose node :match-children t))
         ;; Struct fields
         (:activation-nodes
          ((:nodes ("field_declaration")
                   :has-parent ("field_declaration_list")))
          :selector (:choose parent :match-children (:match-rules ("field_declaration"))))
         ;; Enum variants
         (:activation-nodes
          ((:nodes ("enum_variant")
                   :has-parent ("enum_variant_list")))
          :selector (:choose parent :match-children (:match-rules ("enum_variant"))))
         ;; Struct expression initializers
         (:activation-nodes
          ((:nodes ("field_initializer" "shorthand_field_initializer")
                   :has-parent ("field_initializer_list")))
          :selector (:choose parent :match-children t))
))
      (procedures-hierarchy
       `((:activation-nodes
          ((:nodes "block" :position at))
          :selector (:choose node :match-children t))
         (:activation-nodes
          ((:nodes "match_block" :position at))
          :selector (:choose node :match-children (:match-rules ("match_arm"))))
         (:activation-nodes
          ((:nodes ("function_item" "if_expression" "for_expression"
                    "while_expression" "loop_expression" "match_expression"
                    "impl_item" "struct_item" "enum_item" "trait_item"
                    "mod_item" "closure_expression")
                   :position at))
          :selector (:choose node :match-children
                             (:match-rules ("block" "match_block"
                                            "field_declaration_list"
                                            "declaration_list"
                                            "enum_variant_list"))))
         (:activation-nodes
          ((:nodes ((all))))
          :selector (:choose node :match-children t))))
      (envelope-procedure-shorthand-alist
       '((general-statement
          . ((:activation-nodes
              ((:nodes ("block" "source_file" "declaration_list")
                       :has-parent ("block" "source_file" "declaration_list"))))))))
      (envelope-list
       '((:description
          "if ... { ... } [else { ... }]"
          :key "i"
          :mark-node t
          :shorthand general-statement
          :name "if-expression"
          :template
          ("if " @ (p true "Condition") " {" n>
           (choice* :missing nil :rest (r> n>) :name "if-block")
           "}" >
           (choice* :missing nil
                    :rest (" else {" n> @ r> n> "}" > n>)
                    :name "else-block")))
         (:description
          "match ... { ... }"
          :key "m"
          :mark-node t
          :shorthand general-statement
          :name "match-expression"
          :template
          ("match " @ (p expr "Expression") " {" n>
           (p pattern "Pattern") " => " @ "," n>
           "}" > n>))
         (:description
          "for ... in ... { ... }"
          :key "f"
          :mark-node t
          :shorthand general-statement
          :name "for-loop"
          :template
          ("for " (p var "Variable") " in " @ (p iter "Iterator") " {" n>
           r> n> "}" > n>))
         (:description
          "let ... = ...;"
          :key "l"
          :mark-node t
          :shorthand general-statement
          :name "let-binding"
          :template
          ("let "
           (choice* :name "mutable" :rest ("mut "))
           (p name "Name") " = " @ r> ";" n>))
         (:description
          "impl ... { ... }"
          :key "I"
          :mark-node t
          :shorthand general-statement
          :name "impl-block"
          :template
          ("impl " @ (p type "Type") " {" n>
           r> n> "}" > n>)))))))

(define-combobulate-language
 :name rust
 :language rust
 :major-modes (rust-mode rust-ts-mode)
 :custom combobulate-rust-definitions
 :setup-fn combobulate-rust-setup)

(defun combobulate-rust-setup (_))

(provide 'combobulate-rust)
;;; combobulate-rust.el ends here
