# First Class Stage Interpreter

## Tokenizer

Separators are
- `+`, `*`, `-`, `/`
- spaces
- `(*`, `*)`
- `(`, `)`

## Grammar

```
expr ::= "if" expr "then" expr "else" expr
       | "fun" pattern "->" expr
       | "let" pattern = expr "in" expr
       | boolean_expr
       | list_expr
       | "quote" variable expr
       | "unquote" variable expr
       | "match" expr "with" inside_match

inside_match ::= pattern "->" expr
               | pattern "->" expr "|" inside_match

list_expr ::= "[]"
            | expr "::" list_expr

inside_list ::= expr ";" inside_list
              | expr

pattern ::= variable
          | tuple_pattern 
          | list_pattern

list_pattern ::= variable "::" list_pattern
               | tuple_pattern :: list_pattern

tuple_pattern ::= "(" inside_tuple_pattern ")"

inside_tuple_pattern ::= pattern
                       | pattern "," inside_tuple_pattern

boolean_constant ::= "true" | "false"

boolean_expr ::= boolean_term_expr "&&" boolean_expr
               | boolean_term_expr "||" boolean_expr
               | boolean_term_expr

boolean_term_expr ::= numeral_expr "<" numeral_expr
                    | numeral_expr ">" numeral_expr
                    | numeral_expr "<=" numeral_expr
                    | numeral_expr ">=" numeral_expr
                    | numeral_expr "==" numeral_expr
                    | numeral_expr

numeral_expr ::= numeral_expr "+" term
               | numeral_expr "-" term
               | term

term ::= term "*" factor
       | term "/" factor
       | application

application ::= application factor
              | factor

factor ::= number
         | variable
         | "(" inside_tuple ")"
         | "epsilon"
         | boolean_constant

inside_tuple ::= expr "," inside_tuple
```
