#lang pl 03

;; PART A: 

#| BNF for the MUWAE language:
    <MUWAE> ::= (list <num>)
            | { + <MUWAE> <MUWAE> }
            | { - <MUWAE> <MUWAE> }
            | { * <MUWAE> <MUWAE> }
            | { / <MUWAE> <MUWAE> }
            | { with { <id> <MUWAE> } <MUWAE> }
            | { sqrt <MUWAE> }
            | <id>
|#

;; MUWAE abstract syntax trees
(define-type MUWAE
    [Num  (Listof Number)]
    [Add  MUWAE MUWAE]
    [Sub  MUWAE MUWAE]
    [Mul  MUWAE MUWAE]
    [Div  MUWAE MUWAE]
    [Sqrt MUWAE]
    [Id   Symbol]
    [With Symbol MUWAE MUWAE])

(: parse-sexpr : Sexpr -> MUWAE)
;; to convert s-expressions into MUWAEs
(define (parse-sexpr sexpr)
(match sexpr
    [(number: n) (Num (list n))]
    [(symbol: name) (Id name)]
    [(list 'sqrt expr) (Sqrt (parse-sexpr expr))]
    [(cons 'with more)
    (match sexpr
        [(list 'with (list (symbol: name) named) body)
        (With name (parse-sexpr named) (parse-sexpr body))]
        [else (error 'parse-sexpr "bad `with' syntax in ~s" sexpr)])]
    [(list '+ lhs rhs) (Add (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '- lhs rhs) (Sub (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '* lhs rhs) (Mul (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '/ lhs rhs) (Div (parse-sexpr lhs) (parse-sexpr rhs))]
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))

(: parse : String -> MUWAE)
;; parses a string containing a MUWAE expression to a MUWAE AST(sqrt (+ 16 (* (+ 1 (sqrt 1)) (/ 9 2))))
(define (parse str)
    (parse-sexpr (string->sexpr str)))

#| Formal specs for `subst':
    (`N' is a <num>, `E1', `E2' are <MUWAE>s, `x' is some <id>, `y' is a
    *different* <id>)
    N[v/x]                = N
    {+ E1 E2}[v/x]        = {+ E1[v/x] E2[v/x]}
    {- E1 E2}[v/x]        = {- E1[v/x] E2[v/x]}
    {* E1 E2}[v/x]        = {* E1[v/x] E2[v/x]}
    {/ E1 E2}[v/x]        = {/ E1[v/x] E2[v/x]}
    {sqrt E}[v/x]         = {sqrt E[v/x]}
    y[v/x]                = y
    x[v/x]                = v
    {with {y E1} E2}[v/x] = {with {y E1[v/x]} E2[v/x]}
    {with {x E1} E2}[v/x] = {with {x E1[v/x]} E2}
|#

(: subst : MUWAE Symbol MUWAE -> MUWAE)
;; substitutes the second argument with the third argument in the
;; first argument, as per the rules of substitution; the resulting
;; expression contains no free instances of the second argument
(define (subst expr from to)
(cases expr
    [(Num n) expr]
    [(Sqrt n)  (Sqrt (subst n from to))]
    [(Add l r) (Add (subst l from to) (subst r from to))]
    [(Sub l r) (Sub (subst l from to) (subst r from to))]
    [(Mul l r) (Mul (subst l from to) (subst r from to))]
    [(Div l r) (Div (subst l from to) (subst r from to))]
    [(Id name) (if (eq? name from) to expr)]
    [(With bound-id named-expr bound-body)
    (With bound-id
            (subst named-expr from to)
            (if (eq? bound-id from)
            bound-body
            (subst bound-body from to)))]))

#| Formal specs for `eval':
    eval(N)         = N
    eval({+ E1 E2}) = eval(E1) + eval(E2)
    eval({- E1 E2}) = eval(E1) - eval(E2)
    eval({* E1 E2}) = eval(E1) * eval(E2)
    eval({/ E1 E2}) = eval(E1) / eval(E2)
    eval({sqrt E})  = (sqrt+ (eval E))
    eval(id)        = error!
    eval({with {x E1} E2}) = eval(E2[eval(E1)/x])
|#

(: eval : MUWAE -> (Listof Number))
;; evaluates MUWAE expressions by reducing them to numbers
(define (eval expr)
(cases expr
    [(Num n)    n]
    [(Sqrt n)  (sqrt+ (eval n))]
    [(Add l r) (bin-op + (eval l) (eval r))]
    [(Sub l r) (bin-op - (eval l) (eval r))]
    [(Mul l r) (bin-op * (eval l) (eval r))]
    [(Div l r) (bin-op / (eval l) (eval r))]
    [(With bound-id named-expr bound-body)
        (eval (subst bound-body
                bound-id
                (Num (eval named-expr))))]
    [(Id name) (error 'eval "free identifier: ~s" name)]))

(: run : String -> (Listof Number))
;; evaluate a MUWAE program contained in a string
(define (run str) (eval (parse str)))

(: sqrt+ : (Listof Number) -> (Listof Number))
;; a version of `sqrt' that takes a list of numbers, and return a list 
;; with twice the elements, holding the two roots of each of the inputs; 
;; throws an error if any input is negative. 
(define (sqrt+ ns) 
    (cond [(null? ns) '()] 
        [(< (first ns) 0) (error 'sqrt' "`sqrt' requires a nonnegative input, got: ~s" ns)] 
        [else (let ([num (sqrt (first ns))])
            (cons num (cons (- 0 num) (sqrt+ (rest ns)))))]
    )
)

(: bin-op : (Number Number -> Number) (Listof Number) (Listof Number)-> (Listof Number))
;; applies a binary numeric function on all combinations of numbers from
;; the two input lists, and return the list of all of the results
(define (bin-op op ls rs)

    (: helper : Number (Listof Number) -> (Listof Number))
    (define (helper l rs) 
            (: f : Number -> Number)
            (define (f num) (op l num))
        (map f rs))
    
(if (null? ls) null 
    (append (helper (first ls) rs) (bin-op op (rest ls) rs))))

;; tests
(test (run "5") => '(5))
(test (run "{+ 5 5}") => '(10))
(test (run "{with {x {+ 5 5}} {+ x x}}") => '(20))
(test (run "{with {x 5} {+ x x}}") => '(10))
(test (run "{with {x {+ 5 5}} {with {y {- x 3}} {+ y y}}}") => '(14))
(test (run "{with {x 5} {with {y {- x 3}} {+ y y}}}") => '(4))
(test (run "{with {x 5} {+ x {with {x 3} 10}}}") => '(15))
(test (run "{with {x 5} {+ x {with {x 3} x}}}") => '(8))
(test (run "{with {x 5} {+ x {with {y 3} x}}}") => '(10))
(test (run "{with {x 5} {with {y x} y}}") => '(5))
(test (run "{with {x 5} {with {x x} x}}") => '(5))
(test (run "{with {x 1} y}") =error> "free identifier")  

(test (run "{sqrt 9}") => '(3 -3)) 
(test (run "{sqrt 1}") => '(1 -1)) 
(test (run "{sqrt 0}") => '(0 0)) 
(test (run "{+ {sqrt 1} 3}") => '(4 2)) 
(test (run "{+ {/ {+ {sqrt 1} 3} 2} {sqrt 100}}") => '(12 -8 11 -9))
(test (run "{ * {sqrt 1} {sqrt 1}}") => '(1 -1 -1 1))
(test (run "{ + {sqrt 1} {sqrt 1}}") => '(2 0 0 -2))
(test (run "{sqrt {+ 16 {* {+ 1 {sqrt 1}} {/ 9 2}}}}") => '(5 -5 4 -4))
(test (run "{with {x {sqrt 9}} {* x x}}") =>'(9 -9 -9 9))
(test (run "{with {x {sqrt 9}} {/ x 3}}") =>'(1 -1))
(test (run "{with {x 4} {sqrt x}}") => '(2 -2))

(test (run "{sqrt -1}") =error> "`sqrt' requires a nonnegative input")
(test (run "{with {x -4} {sqrt x}}") =error> "`sqrt' requires a nonnegative input") 
(test (run "{with {x} {sqrt x}}") =error> "bad `with' syntax")
(test (run "{with {x 5} {Add x 1}}") =error> "bad syntax in (Add x 1)")


;; PART B: 

#| BNF for the WAE language:
      <WAE> ::= <num>
              | { + <WAE> <WAE> }
              | { - <WAE> <WAE> }
              | { * <WAE> <WAE> }
              | { / <WAE> <WAE> }
              | { with { <id> <WAE> } <WAE> }
              | <id>
|#

;; WAE abstract syntax trees
(define-type WAE
  [NumW  Number]
  [AddW  WAE WAE]
  [SubW  WAE WAE]
  [MulW  WAE WAE]
  [DivW  WAE WAE]
  [IdW   Symbol]
  [WithW Symbol WAE WAE])

(: parse-sexprW : Sexpr -> WAE)
;; to convert s-expressions into WAEs
(define (parse-sexprW sexpr)
  (match sexpr
    [(number: n)    (NumW n)]
    [(symbol: name) (IdW name)]
    [(cons 'with more)
      (match sexpr
      [(list 'with (list (symbol: name) named) body)
        (WithW name (parse-sexprW named) (parse-sexprW body))]
        [else (error 'parse-sexprW "bad `with' syntax in ~s" sexpr)])]
    [(list '+ lhs rhs) (AddW (parse-sexprW lhs) (parse-sexprW rhs))]
    [(list '- lhs rhs) (SubW (parse-sexprW lhs) (parse-sexprW rhs))]
    [(list '* lhs rhs) (MulW (parse-sexprW lhs) (parse-sexprW rhs))]
    [(list '/ lhs rhs) (DivW (parse-sexprW lhs) (parse-sexprW rhs))]
    [else (error 'parse-sexprW "bad syntax in ~s" sexpr)]))

(: parseW : String -> WAE)
;; parses a string containing a WAE expression to a WAE AST
(define (parseW str) (parse-sexprW (string->sexpr str)))

#| Formal specs for `substW':
    (`N' is a <num>, `E1', `E2' are <WAE>s, `x' is some <id>, `y' is a
    *different* <id>)
      N[v/x]                = N
      {+ E1 E2}[v/x]       W = {+ E1[v/x] E2[v/x]}
      {- E1 E2}[v/x]        = {- E1[v/x] E2[v/x]}
      {* E1 E2}[v/x]        = {* E1[v/x] E2[v/x]}
      {/ E1 E2}[v/x]        = {/ E1[v/x] E2[v/x]}
      y[v/x]                = y
      x[v/x]                = v
      {with {y E1} E2}[v/x] = {with {y E1[v/x]} E2[v/x]}
      {with {x E1} E2}[v/x] = {with {x E1[v/x]} E2}
|#

(: substW : WAE Symbol WAE -> WAE)
;; substitutes the second argument with the third argument in the
;; first argument, as per the rules of substitution; the resulting
;; expression contains no free instances of the second argument
(define (substW expr from to)
  (cases expr
    [(NumW n) expr]
    [(AddW l r) (AddW (substW l from to) (substW r from to))]
    [(SubW l r) (SubW (substW l from to) (substW r from to))]
    [(MulW l r) (MulW (substW l from to) (substW r from to))]
    [(DivW l r) (DivW (substW l from to) (substW r from to))]
    [(IdW name) (if (eq? name from) to expr)]
    [(WithW bound-id named-expr bound-body)
      (WithW bound-id
            (substW named-expr from to)
            (if (eq? bound-id from)
              bound-body
              (substW bound-body from to)))]))


(: freeInstanceList : WAE -> (Listof Symbol))
;; freeInstanceList consumes an abstract syntax tree (WAE) and returns null 
;; if theWre are no Wfree instance, and a list of all the free instances otherwise
(define (freeInstanceList wae) 
  (cases wae
      [(NumW n) '()]
      [(AddW l r) (removeDuplicatedIfNecessary l r)]
      [(SubW l r) (removeDuplicatedIfNecessary l r)]
      [(MulW l r) (removeDuplicatedIfNecessary l r)]
      [(DivW l r) (removeDuplicatedIfNecessary l r)]
      [(WithW bound-id named-expr bound-body)
        (freeInstanceList (substW bound-body bound-id named-expr))]
      [(IdW name) (list name)]
  )
)

;; removeDuplicatedIfNecessary - Help method
;; When we have the same expression on both sides, it's the same free instance
;; so don't add it twice
(: removeDuplicatedIfNecessary : WAE WAE -> (Listof Symbol))
(define (removeDuplicatedIfNecessary l r) 
  (let ([x (freeInstanceList l)] [y (freeInstanceList r)])
                    (if (equal? x y) x (append x y)))
)

;; tests
(test (freeInstanceList (parseW "w")) => '(w))
(test (freeInstanceList (parseW "{with {xxx 2} {with {yyy 3} {+ {- xx y} z}}}")) => '(xx y z))
(test (freeInstanceList (WithW 'x (NumW 2) (AddW (IdW 'x) (NumW 3)))) => '()) 
(test (freeInstanceList (parseW "{+ z {+ x z}}")) => '(z x z))
(test (freeInstanceList (parseW "{with {y {/ z 3}} {+ y y}}")) => '(z))
(test (freeInstanceList (parseW "{with {a 3} {* a b}}"))=> '(b))
(test (freeInstanceList (parseW "{with {x 5} {with {x x} {/ x 5}}}")) => '())
(test (freeInstanceList (parseW "{with {x} {+ 5 x}}")) =error> "bad `with' syntax")
(test (freeInstanceList (parseW "{with {x 5} {AddW x 1}}")) =error> "bad syntax in (AddW x 1)")