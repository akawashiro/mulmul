# mulmul
Multistage programming language

## Example

### Lisp-like
```lisp
(letrec vadd1 n v1 v2
  (sfun a 
        (if (= n 0)
          (quote a nil)
          (quote a
                 (let ((t1 (tail (unquote a v1)))
                       (t2 (tail (unquote a v2))))
                  (cons
                   (+ (head (unquote a v1)) (head (unquote a v2)))
                   (unquote a (vadd1 (- n 1) (unquote a v1) (unquote a v2)))))))))
```

### OCaml-like
```ocaml
let rec vadd1 n v1 v2 =
  sfun a ->
    if n = 0
    then 
      |>_a []
    else
      |>_a ( 
      let t1 = List.tl (<|_a v1) in
      let t2 = List.tl (<|_a v2) in
      List.hd (<|_a v1) + List.hd (<|_a v2)
      :: <|_a (vadd1 (n-1) (|>_a t1) (|>_a t2)))

let rec vadd n =
  |>_a (fun v1 -> fun v2 -> <|_a(vadd1 n (|>_a v1) (|>_a v2)))
```

```ocaml
let rec vadd1 n v1 v2 =
  sfun a ->
    if n = 0
    then 
      quote a []
    else
      quote a ( 
      let t1 = List.tl (unquote a v1) in
      let t2 = List.tl (unquote a v2) in
      List.hd (unquote a v1) + List.hd (unquote a v2)
      :: unquote a (vadd1 (n-1) (quote a t1) (quote a t2)))

let rec vadd n =
  quote a (fun v1 -> fun v2 -> unquote a (vadd1 n (quote a v1) (quote a v2)))
```

## Tokenizer

Separators are
- spaces
- `->`
- `+`, `*`, `-`, `/`, `<`, `>`, `<=`, `>=`, `==`
- `(*`, `*)`
- `(`, `)`, `,`
- `::`, `::`

## Parser

```
expr ::= "if" expr "then" expr "else" expr
       | "fun" pattern "->" expr
       | "let" pattern = expr "in" expr
       | list_expr
       | "quote" variable expr
       | "unquote" variable expr
       | "match" expr "with" inside_match

inside_match ::= pattern "->" expr
               | pattern "->" expr "|" inside_match

list_expr ::= boolean_expr "::" list_expr
            | boolean_expr

pattern ::= list_pattern

list_pattern ::= tuple_pattern "::" list_pattern
                | tuple_pattern

tuple_pattern ::= "(" inside_tuple_pattern ")"
                | variable
                | "[]"

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
         | "[]"

inside_tuple ::= expr "," inside_tuple
```
