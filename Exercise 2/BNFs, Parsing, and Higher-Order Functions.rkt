#lang pl

;; Q1
;; Defining two new types 
(define-type BIT = (U 0 1))
(define-type Bit-List = (Listof BIT))

;; The actual interpreter 
#| BNF for the ROL language:
<ROL>  ::= "{ reg-len =  <PositiveNumber> <RegE>}"
<RegE> ::= Reg <Bits> 
        | And <RegE1> <RegE2> 
        | Or <RegE1> <RegE2> 
        | Shl <RegE>
<Bits> ::= (Listof (U 0 1)) ;; BNF allows wrong length

Examples of ROL:
1. "{ reg-len =  4  {1 0 0 0}}" => <ROL> => <RegE> => Reg <Bits> => (Reg '(1 0 0 0))
2. "{ reg-len = 4  {shl {1 0 1 0}}}" => <ROL> => <RegE> => Shl <RegE> => Shl (Reg <Bits>) => 
    (Shl (Reg '(1 0 0 0)))
3. "{ reg-len = 2  {and {shl {1 0}} {1 0}}}" => <ROL> => <RegE> 
    => And <RegE1> <RegE2> => And <RegE1> (Reg <Bits>) => And <RegE1> (Reg '(1 0)) => 
        And Shl <RegE> (Reg '(1 0)) => And Shl <RegE> (Reg '(1 0)) => 
        And (Shl (Reg '(1 0))) (Reg '(1 0)) => (And (Shl (Reg '(1 0))) (Reg '(1 0)))
|#

;; RegE abstract syntax trees
(define-type RegE
    [Reg  Bit-List]
    [And  RegE RegE]
    [Or   RegE RegE] 
    [Shl  RegE])
    
;; Next is a technical function that converts (casts) 
;; (any) list into a bit-list. We use it in parse-sexpr. 
(: list->bit-list : (Listof Any) -> Bit-List)
;; to cast a list of bits as a bit-list
(define (list->bit-list lst)
    (cond [(null? lst) null]
        [(eq? (first lst) 1)(cons 1 (list->bit-list (rest lst)))]
        [else (cons 0 (list->bit-list (rest lst)))]))
    

(: parse-sexpr : Sexpr -> RegE)
;; to convert the main s-expression into ROL
(define (parse-sexpr sexpr)
    (match sexpr
    [(list reg-len-sym eq-sign (number: reg-len) RegE) 
        (if (> reg-len 0)
        (parse-sexpr-RegL RegE reg-len) 
        (error 'parse-sexpr "bad syntax in reg-len: ~s" reg-len))] 
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))

(: parse-sexpr-RegL : Sexpr Number -> RegE)
;; to convert s-expressions into RegEs
(define (parse-sexpr-RegL sexpr reg-len)
    (match sexpr
    [(list (and l (or 1 0)) ...) 
    (if (and (list? l) (eq? (length l) reg-len)) [Reg (list->bit-list l)] 
        (error 'parse-sexpr "wrong number of bits in ~s" l))]
    [(list 'or rege1 rege2) [Or (parse-sexpr-RegL rege1 reg-len) (parse-sexpr-RegL rege2 reg-len)]]
    [(list 'and rege1 rege2) [And (parse-sexpr-RegL rege1 reg-len) (parse-sexpr-RegL rege2 reg-len)]]
    [(list 'shl rege) [Shl (parse-sexpr-RegL rege reg-len)]]
    [else (error 'parse-sexpr-RegL "bad syntax in ~s" sexpr)]))

(: parse : String -> RegE) 
;; parses a string containing a RegE expression to a RegE AST
(define (parse str)(parse-sexpr (string->sexpr str)))

;; Understand the list->bit-list function
(test (list->bit-list '(1 2 3 1 0)) => '(1 0 0 1 0))

;; Check string->sexpr output
(test (string->sexpr "{1 0 0 0}") => '(1 0 0 0))
(test (string->sexpr "{shl {1 0 0 0}}") => '(shl (1 0 0 0)))
(test (string->sexpr "{ reg-len = 4  {shl {1 0 0 0}}}") => '(reg-len = 4 (shl (1 0 0 0))))

;; Tests - valid cases
(test(parse "{ reg-len =  4  {1 0 0 0}}") => (Reg '(1 0 0 0))) 
(test (parse "{ reg-len = 4  {shl {1 0 0 0}}}") => (Shl (Reg '(1 0 0 0))))
(test (parse "{ reg-len = 4  {and {shl {1 0 1 0}} {shl {1 0 1 0}}}}") => 
    (And (Shl (Reg '(1 0 1 0))) (Shl (Reg '(1 0 1 0)))))
(test (parse "{ reg-len = 4  { or {and {shl {1 0 1 0}} {shl {1 0 0 1}}} {1 0 1 0}}}") => 
    (Or (And (Shl (Reg '(1 0 1 0))) (Shl (Reg '(1 0 0 1)))) (Reg '(1 0 1 0))))
(test (parse "{ reg-len = 2  { or {and {shl {1 0}} {1 0}} {1 0}}}") => 
    (Or (And(Shl (Reg '(1 0))) (Reg '(1 0))) (Reg '(1 0))))
(test (parse "{reg-len = 3 {shl {and {1 0 0} {0 1 1}}}}") => 
    (Shl (And (Reg '(1 0 0)) (Reg '(0 1 1)))))

;; Tests - invalid cases
(test (parse "{}") =error> "bad syntax in")
(test (parse "{ reg-len =  4  {1 2 3 4}}") =error> "bad syntax in")
(test (parse "{ reg-len =  0  {}}") =error> "bad syntax in reg-len")
(test (parse "{ reg-len =  10  {}}") =error> "wrong number of bits in")
(test (parse "{1 0 0 0}") =error> "bad syntax in")
(test (parse "{1 2 3 4}") =error> "bad syntax in")
(test (parse "{ reg-len = 3 {and {2 2 1} {0 1 1}}}") =error> "bad syntax in")
(test (parse "{ reg-len = 3 {+ {1 1 1} {0 1 1}}}") =error> "bad syntax in")
(test (parse "{ reg-len = 3 {{1 1 1} {0 1 1}}}") =error> "bad syntax in") ;; No Symbol at all
(test (parse "{ reg-len = 4  {shl {1 1 1 1} {0 1 1 1}}}") =error> "bad syntax in")
(test (parse "{ reg-len = 4  {or {1 1 1 1} {0 1 1}}}") =error> 
    "wrong number of bits in (0 1 1)")
(test (parse "{ reg-len = 0  {or {and {shl {1 0}} {1 0}} {1 0}}}") =error> 
    "bad syntax in reg-len")

;; Q2
#|
Q2.1
The problem in the following expression is that you can derive a double 'set operation':
    {* {+ {set 1} {set 2}} get}
            ^       ^
This will override the saved value and will result "bad syntax".
My solution:
Define a new type X:
<X> ::= <num> | get
<MAE> ::= <num>
    | { + <X> <MAE> }
    | { - <X> <MAE> }
    | { * <X> <MAE> }
    | { / <X> <MAE> }
    | { set <MAE> }
    | get
|#

#|
Q2.2
<AE> ::= <num>
        | { + <AE> <AE> }
        | { - <AE> <AE> }
        | { * <AE> <AE> }
        | { / <AE> <AE> }

;; Same as <AE> but with a `get' operation
<GAE> ::= <num>
        | { + <GAE> <GAE> }
        | { - <GAE> <GAE> }
        | { * <GAE> <GAE> }
        | { / <GAE> <GAE> }
        | get

;; Gives the ability to multiply <GAE> as much as we want
<MUL_GAE> ::= { set <GAE> } | <MUL_GAE> <MUL_GAE>

<MAE> ::= { seq <AE> } ;; valid sequence by example
        | { seq { set <AE> } <GAE> }
        | { seq { set <AE> } <MUL_GAE> <GAE> } 

Derivation process (for each one of <MAE>'s roles):
1. { seq { - 308 21 } } => { seq <AE> } => <AE> -> { - <AE> <AE> } => { seq { - <AE> <AE> } }
    => <AE> -> <num> => { seq { - <num> <num> } } => { - 308 21 } => 287
2. { seq { set { + 30 82 } } { + get 13 }} 
        => { seq { set <AE> } <GAE> } 
        1. { set <AE> } => <AE> -> { + <AE> <AE> } => { set { + <AE> <AE> } }
            => <AE> -> <num> => { set { + <num> <num> } } => { set { + 30 82 } } => 112
        2. <GAE> -> { + <GAE> <GAE> } => { + <GAE> <GAE> } => <GAE> -> get, <GAE> -> <num>
            => { + get <num> } => { + 112 13 } => 125
3. { seq { set 198 }
        { set { * get 21 } } 
        get } => { seq { set <AE> } <MUL_GAE> <GAE> }
        1. { set <AE> } => <AE> -> <num> => { set 198 }
        2. <MUL_GAE> => { set <GAE> } => <GAE> -> { * <GAE> <GAE> } => { set { * <GAE> <GAE> } }
            => <GAE> -> get, <GAE> -> <num> => { set { * get <num> } } => { set { * 198 21 } }
            => 4158
        3. <GAE> -> get => 4158
|#
;; Q3
(: square : Number -> Number)
;; take a number and return its squared value
(define (square x) (* x x))

(: sum-of-squares : (Listof Number) -> Number)
;; take a list of numbers and return the sum of their squared value
(define (sum-of-squares list) 
    (foldl (lambda ([a : Number] [b : Number]) (+ (square a) b)) 0 list))

;; Tests for square function
(test (square 1) => 1)
(test (square -1) => 1)
(test (square 1.5) => 2.25)
(test (square -2.5) => 6.25)
(test (square 11) => 121)
(test (square -11) => 121)

;; Tests for sum-of-squares function
(test (sum-of-squares '()) => 0)
(test (sum-of-squares '(1)) => 1)
(test (sum-of-squares '(1 2 3)) => 14)
(test (sum-of-squares '(1 -2 3)) => 14)
(test (sum-of-squares '(1.5 -2 3)) => 15.25)
(test (sum-of-squares '(1.5 -2.5 3)) => 17.5)

;; Q4 
;; BINTREE abstract syntax
(define-type BINTREE
    [Node BINTREE BINTREE]
    [Leaf Number])

;; Test BINTREE correctness
(test (BINTREE? (Node (Leaf 2) (Node (Leaf 3) (Leaf 4)))))

(: tree-map :  (Number -> Number) BINTREE -> BINTREE)
;; to use a numeric function (a function of a single number argument) on each one of 
;; the leaves in the binary tree
(define (tree-map func binTree) 
    (cases binTree
        [(Node binTree1 binTree2) (Node (tree-map func binTree1) (tree-map func binTree2))]
        [(Leaf x) (Leaf (func x))]
    )
)

;; Tests for tree-map
(test (tree-map add1 (Leaf 1)) => (Leaf 2))
(test (tree-map add1 (Node (Leaf 1) (Node (Leaf 2) (Leaf 3)))) => 
    (Node (Leaf 2) (Node (Leaf 3) (Leaf 4))))
(test (tree-map add1 (Node (Leaf 1.5) (Node (Leaf -1) (Leaf 3)))) => 
    (Node (Leaf 2.5) (Node (Leaf 0) (Leaf 4))))
(test (tree-map / (Leaf 1)) => (Leaf 1))

(: tree-fold : (All (A) (A A -> A) (Number -> A) BINTREE -> A))
;; the equivalent of the swiss-army-knife tool that foldl is for lists.
(define (tree-fold combiner func binTree) 
    (cases binTree
        [(Node binTree1 binTree2) 
            (combiner 
                (tree-fold combiner func binTree1)
                (tree-fold combiner func binTree2))]
        [(Leaf x) (func x)]
    )
)

(: tree-flatten : BINTREE -> (Listof Number))
;; flattens a binary tree to a list of its values in
;; left-to-right order
(define (tree-flatten tree)
    (tree-fold (inst append Number) (inst list Number) tree))

;; Tests for tree-flatten
(test (tree-flatten (Leaf 1)) => '(1))
(test (tree-flatten (Node (Leaf 1) (Node (Leaf 2) (Leaf 3)))) => '(1 2 3))
(test (tree-flatten (Node (Leaf 1.5) (Node (Leaf -1) (Leaf 3)))) => '(1.5 -1 3))

(: tree-reverse : BINTREE -> BINTREE)
;; consumes a tree and returns a tree that is its mirror image
(define (tree-reverse tree)
    (tree-fold (lambda ([a : BINTREE] [b : BINTREE]) (Node b a)) 
        (lambda ([x : Number]) (Leaf x)) tree))

;; Tests for tree-reverse
(test (tree-reverse (Leaf 0)) => (Leaf 0))
(test (tree-reverse (Node (Leaf 1) (Node (Leaf 2) (Leaf 3)))) => 
    (Node (Node (Leaf 3) (Leaf 2)) (Leaf 1)))
(test (reverse (tree-flatten (tree-reverse (Node (Leaf 1) (Node (Leaf 2) (Leaf 3)))))) => '(1 2 3))
(test (equal? 
        (reverse (tree-flatten (Leaf 0)))
        (tree-flatten (tree-reverse (Leaf 0)))))
(test (equal? 
        (reverse (tree-flatten (Node (Leaf 1.5) (Node (Leaf -1) (Leaf 3)))))
        (tree-flatten (tree-reverse (Node (Leaf 1.5) (Node (Leaf -1) (Leaf 3))))))) 
