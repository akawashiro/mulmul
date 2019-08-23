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
       | "fun" variable "->" expr
       | "let" variable = expr "in" expr
       | boolean_expr
       | "quote" variable expr
       | "unquote" variable expr

boolean ::= "true" | "false"

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
         | boolean

inside_tuple ::= expr "," inside_tuple
```
