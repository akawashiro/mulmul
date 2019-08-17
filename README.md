# mulmul
A multistage programming language

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
       | numeral_expr
       | "quote" variable expr
       | "unquote" variable expr

boolean ::= "true" | "false"

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
         | "(" expr ")"
         | "epsilon"
         | boolean
```
