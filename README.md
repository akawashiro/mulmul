# mulmul
A multistage programming language

## Grammar

```
expr ::= boolean
       | "if" expr "then" expr "else" expr
       | "fun" variable "->" expr
       | numeral_expr
       | "epsilon"
       | "quote" variable expr
       | "unquote" variable expr
       | expr expr

boolean ::= "true" | "false"

numeral_expr ::= numeral_expr "+" term
               | numeral_expr "-" term

term ::= term "*" factor
       | term "/" factor

factor ::= number
         | variable
         | "(" expr ")"
```
