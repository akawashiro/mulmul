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
expr ::= boolean
       | "if" expr "then" expr "else" expr
       | "fun" variable "->" expr
       | numeral_expr
       | "epsilon"
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
```
