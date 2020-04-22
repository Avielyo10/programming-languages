#lang pl

;; Q1
#|
Help method, does String ends with "pl"?
returns #true iff a String ends with "pl".
|#
(: endsWithPl? : String -> Boolean)
(define (endsWithPl? str)
    (let ([l (string-length str)])
        (and 
            (> l 1)
            (eq? (string-ref str (- l 1)) #\l)
            (eq? (string-ref str (- l 2)) #\p)
        )
    )
)

#| 
plSuffixContained – consumes a list of strings
and returns the first string that contains the string "pl" as a suffix – if one such
exists, and returns #f otherwise.
|#
(: plSuffixContained : (Listof String) -> (U String #f))
(define (plSuffixContained stringList)
    (if (and (list? stringList) (not (null? stringList))) ;; Check if it's non-empty list
        ;; Iteration with recursion
        (if (endsWithPl? (first stringList))
            (first stringList)
            (plSuffixContained (rest stringList))
        )
    #f)
)
    
;; Unit tests for endsWithPl?
(test (endsWithPl? "l") => #f)
(test (endsWithPl? "Pl") => #f)
(test (endsWithPl? "ppl") => #t)

;; Unit tests for plSuffixContained
(test (plSuffixContained '()) => #f)
(test (plSuffixContained '("22" "a" "t")) => #f)
(test (plSuffixContained '("sps" "ppl" "pl")) => "ppl")

;; Q2.1
#|
Implementing tail-recursion as help method for write-poly.
Takes Listof numbers, it's size (as index) to determine the power of 𝑥 and 
the polynom as string that we are building.
With each iteration we write the first number in the list in its polynomial form to the string
and using tail-recursion to handle the rest of the list till the list is empty.
|#
(: tail-recursion-write-poly : (Listof Number) Number String -> String)
(define (tail-recursion-write-poly numberList index str)
    (let ([currNum (first numberList)] [currIndex (- index 1)] [isCurrNumZero? (eq? (first numberList) 0)] [isStrNotEmpty? (not (zero? (string-length str)))])
        (if (and isCurrNumZero? (> currIndex 0))
            (tail-recursion-write-poly (rest numberList) currIndex (format "~a~a" str (if (and isStrNotEmpty? (> (second numberList) 0)) "+" "")))
            (cond 
            [(eq? currIndex 0) (format "~a~a" str (if (eq? currNum 0) "" currNum))]
            [(eq? currIndex 1) 
                (tail-recursion-write-poly (rest numberList) currIndex 
                    (format "~a~ax~a" str currNum (if (> (second numberList) 0) "+" "")))]
            [else 
                (tail-recursion-write-poly (rest numberList) currIndex 
                    (format "~a~ax^~a~a" str currNum currIndex (if (> (second numberList) 0) "+" "")))])
        )
    )
)

#|
write-poly - consumes a list of coefficients
(numbers) 𝑎1, 𝑎2, … , 𝑎𝑛 and returns the polynomial (in a reversed order of
coefficients) "𝑎_1𝑥^𝑛 + 𝑎_2𝑥^{𝑛−1} + ⋯ + 𝑎_𝑛".
|#
(: write-poly : (Listof Number) -> String)
(define (write-poly numberList) 
    (if (and (list? numberList) (not (null? numberList)))
        (tail-recursion-write-poly numberList (length numberList) "")
        ""
    )
)

;; Unit tests for write-poly
(test (write-poly '()) => "")
(test (write-poly '(3)) => "3")
(test (write-poly '(-3)) => "-3")
(test (write-poly '(3 2)) => "3x+2")
(test (write-poly '(3 -2)) => "3x-2")
(test (write-poly '(3 2 6)) => "3x^2+2x+6")
(test (write-poly '(3 -2 6)) => "3x^2-2x+6")
(test (write-poly '(7 8 9 10)) => "7x^3+8x^2+9x+10")
(test (write-poly '(-7 8 9 -10)) => "-7x^3+8x^2+9x-10")
(test (write-poly '(1 0 1 0)) => "1x^3+1x")
(test (write-poly '(0 0 0 1 0)) => "1x")

;; Q2.2
#|
Implementing tail-recursion as help method for compute-poly.
Takes Listof numbers, it's size (as index) to determine the power of 𝑥, 
the value we computed till now and 𝑥 itself.
With each iteration we compute the first number in the list and add it to ans
then we are using tail-recursion to handle the rest of the list till the list is empty.
|#
(: tail-recursion-compute-poly : (Listof Number) Integer Number Number -> Number)
(define (tail-recursion-compute-poly numberList index ans x)
    (let ([currNum (first numberList)] [currIndex (- index 1)])
        (cond [(eq? currIndex 0) (+ ans currNum)]
        [else (tail-recursion-compute-poly (rest numberList) currIndex (+ ans (* currNum (expt x currIndex))) x)])
    )
)

#|
compute-poly - consumes a number 𝑥 and a list of
coefficients (numbers) 𝑎1, 𝑎2, … , 𝑎𝑛 and returns the result of the
polynomial "𝑎_1𝑥^𝑛 + 𝑎_2𝑥^{𝑛−1} + ⋯ + 𝑎_𝑛".
|#
(: compute-poly : Number (Listof Number) -> Number)
(define (compute-poly x numberList) 
    (if (and (list? numberList) (not (null? numberList)))
        (tail-recursion-compute-poly numberList (length numberList) 0 x)
        0
    )
)

;; Unit tests for compute-poly
(test (compute-poly 2 '()) => 0)
(test (compute-poly 2 '(0)) => 0)
(test (compute-poly 2 '(3 2 6)) => 22)
(test (compute-poly 3 '(4 3 -2 0)) => 129)

;; Q3 
#| 
A keyed-stack data structure. 
Each element in the stack will be indexed with a symbol. 
|#
(define-type KeyStack
    [EmptyKS]
    [Push Symbol String KeyStack])

#|
search-stack – the search operation takes as input a symbol (key) and
a keyed-stack and return the first (LIFO, last in first out) value that 
is keyed accordingly.
If the key does not appear in the original stack, it should return #false.
|#
(: search-stack : Symbol KeyStack -> (U String #f))
(define (search-stack symbol keyStack)
    (cases keyStack
        [(Push sym str stack) (if (eq? sym symbol) str (search-stack symbol stack))]
        [EmptyKS #f])
)

#|
pop-stack – the pop operation takes as input a keyed-stack and return 
the keyed-stack without its first (keyed) value.
If the original stack was empty, it should return #false.
|#
(: pop-stack : KeyStack -> (U KeyStack #f))
(define (pop-stack keyStack)
    (cases keyStack
        [(Push sym str stack) stack]
        [EmptyKS #f])
)

;; Unit tests for KeyStack type
(test (KeyStack? (EmptyKS)) => #t)
(test (KeyStack? (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => #t)

;; Constructor tests
(test (EmptyKS) => (EmptyKS))
(test (Push 'b "B" (Push 'a "A" (EmptyKS))) => 
    (Push 'b "B" (Push 'a "A" (EmptyKS))))
(test (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS)))) => 
    (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS)))))

;; Unit tests for search-stack
(test (search-stack 'a (EmptyKS)) => #f)
(test (search-stack 'a (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) 
    => "AAA")
(test (search-stack 'c (Push 'a "AAA" (Push 'b "B" (Push 'c "A" (EmptyKS))))) 
    => "A")
(test (search-stack 'c (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS)))))
     => #f)

;; Unit tests for pop-stack
(test (pop-stack (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => 
    (Push 'b "B" (Push 'a "A" (EmptyKS))))
(test (pop-stack (EmptyKS)) => #f)

;; Q4
#|
This function takes a natural number and determine 
whether this is an odd number or not, returns #true iff this is an odd number.
This function depends on is-even? function, asking if the number is 0, if #true then return #false
else - use is-even? with the number minus 1.
|#
(: is-odd? : Natural -> Boolean)
(define (is-odd? x)
 (if (zero? x)
    false
    (is-even? (- x 1))))

#|
This function takes a natural number and determine 
whether this is an even number or not, returns #true iff this is an even number.
This function depends on is-odd? function, asking if the number is 0, if #true then return #true
else - use is-odd? with the number minus 1.
|#
(: is-even? : Natural -> Boolean)
(define (is-even? x)
 (if (zero? x)
    true
    (is-odd? (- x 1))))

;; Unit tests for is-odd?/is-even?
(test (not (is-odd? 12)))
(test (is-even? 12))
(test (not (is-odd? 0)))
(test (is-even? 0))
(test (is-odd? 1))
(test (not (is-even? 1)))

#|
Template function, takes:
    1. function that takes some generic type A and returns a Boolean. (e.g. is-even?).
    2. Listof some generic type A. 
returns #true iff the function from section 1 returns true for every value in the list from section 2.
|#
(: every? : (All (A) (A -> Boolean) (Listof A) -> Boolean))
(define (every? pred lst)
    (or (null? lst)
        (and 
            (pred (first lst))
            (every? pred (rest lst))
        )
    )
)
;; An example for the usefulness of this polymorphic function
#|
all-even? - takes a Listof natural numbers and 
returns #true iff all of the numbers within the list are even.
|#
(: all-even? : (Listof Natural) -> Boolean)
(define (all-even? lst)
    (every? is-even? lst))

;; Unit tests for all-even?
(test (all-even? null))
(test (all-even? (list 0)))
(test (all-even? (list 2 4 6 8)))
(test (not (all-even? (list 1 3 5 7))))
(test (not (all-even? (list 1))))
(test (not (all-even? (list 2 4 1 6))))

#|
Template function, takes:
    1. function that takes some generic type A and returns a Boolean. (e.g. is-even?).
    2. function that takes some generic type B and returns a Boolean. (e.g. is-even?).
    3. Listof some generic type A.
    4. Listof some generic type B.
returns #true iff:
    1. The function from section 1 returns #true for every value in the list from section 3.
    AND
    2. The function from section 2 returns #true for every value in the list from section 4.
|#
(: every2? : (All (A B) (A -> Boolean) (B -> Boolean) (Listof A) (Listof B) ->
    Boolean))
(define (every2? pred1 pred2 lst1 lst2)
    (or (null? lst1) ;; both lists assumed to be of same length
        (and 
            (pred1 (first lst1))
            (pred2 (first lst2))
            (every2? pred1 pred2 (rest lst1) (rest lst2)))))

#|
all-even-odd? - takes two lists of natural numbers and 
returns #true iff:
    1. All of the numbers within the first list are even.
    AND
    2. All of the numbers within the second list are odd.
|#
(: all-even-odd? : (Listof Natural) (Listof Natural) -> Boolean)
(define (all-even-odd? lst1 lst2)
    (every2? is-even? is-odd? lst1 lst2))

;; Unit tests for all-even-odd?
(test (all-even-odd? null null))
(test (all-even-odd? (list 0) (list 1)))
(test (all-even-odd? (list 2 4 6 8) (list 1 3 5 7)))
(test (not (all-even-odd? (list 1 3 5 7) (list 2 4 6 8))))
(test (not (all-even-odd? (list 1) (list 1))))
(test (not (all-even-odd? (list 2 4 1 6) (list 1 3 5 7))))
