#lang racket #| CSC324 2017 Fall Assignment 2 : Due Wednesday November 29 at 6PM. |#

#| The Maybe Monad.

 In this assignment you implement the functional API for computation that propagates false as failure,
  and use that to implement the associated Do notation. |#

(provide >> >>=
         ÷ √ ln
         E1 Do E2 E3)

(module+ test (require rackunit)
  
  ; Implement ‘>>’, called “then”, that takes two *expressions* and produces an expression that:
  ;   1. Evaluates the first expression.
  ;   2. If that produces false then that is the result.
  ;   3. Otherwise, evaluates the second expression and that is the result.
  
  #;(check-false (>> (not (zero? 0)) (/ 324 0)))
  #;(check-equal? (>> (not (zero? 324)) (/ 324 324))
                  1)
  #;(check-false (>> (number? "324") (>> (not (zero? "324")) (/ 324 "324"))))

  ; Implement functions ÷, √, and ln, that produce false if dividing by zero, taking the square root
  ;  of a negative number, or taking the logarithm of a non-positive number.
  ; Use ‘>>’ appropriately in the implementations.
  ; Implement ÷ curried: taking a number, and returning a unary function ready to divide a number.
  #;(check-false (√ -1))
  #;(check-equal? (√ 324) 18)
  #;(check-false ((÷ 1) 0))
  #;(check-equal? ((÷ 324) 18) 18)
  #;(check-false (ln 0))
  #;(check-equal? (ln 324) (log 324))
  
  ; Implement *function* ‘>>=’, called “bind”, that takes two arguments and:
  ;  1. If the first argument is false then that is the result.
  ;  2. Otherwise, calls the second argument on the first.
  ; Use ‘>>’ appropriately in the implementation.
  #;(check-false (>>= -1 √))
  #;(check-false (>>= (>>= -1 √) ln))
  #;(check-equal? (>>= (>>= (>>= 324 √) (÷ 1)) ln)
                  (log (/ (sqrt 324)))))

(define >> (local [(define (>>> e1 e2)
                     (cond [e1 e2]
                           [else #f]))]
             >>>))
(define ÷ (local [(define (div lower)
                    (local [(define (divv upper)
                              (>> (not (zero? lower)) (/ upper lower)))]divv))]div))
(define √ (local [(define (squareroot num)
                    (>> (not (negative? num)) (sqrt num)))]
            squareroot))
(define ln (local [(define (loga num)
                    (>> (not (negative? num)) (log num)))]
            loga))
(define >>= (local [(define (f e1 e2)
                      (>> (not e1) (e2 e1)))]
              f))

; Consider this language of arithmetic expressions:
;   <numeric-literal>
;      - represented by a number
;   (√ <ae>)
;      - represented by a list with the symbol √  and an arithemtic expression
;   (ln <ae>)
;      - represented by a list with the symbol ln and an arithemtic expression
;   (<ae> ÷ <ae>)
;      - represented by a list with an arithmetic expression, symbol ÷, and arithemtic expression
  
; Implement function ‘E1’ to evaluate such expressions, producing false if any of the computations
;  are invalid according to the earlier restrictions for square root, logarithm, and division.
; Use pattern matching appropriately, along with ‘>>’ and ‘>>=’ for propagating false.
; In particular, do not use any other conditionals, nor boolean operations or literals.
; Use quasiquotation as appropriate for the patterns, but nothing else from match's pattern language
; [e.g. don't use ‘?’, nor #:when].
(define E1 (local [(define (f e)
                    (match e
                      [`(√ ,ae) `,(>> (not `(,negative? ,ae)) `(,sqrt ,ae))]
                      [`(ln ,ae) `,(>> (not `,(negative? ae)) `,(log ae))]
                      [`(,ae1 ÷ ,ae2) `,(>> (not (zero? ae2)) (/ ae1 ae2))]
                      [`,num `,num]))]
             f))


; Implement ‘Do’, using ‘>>’ and ‘>>=’ appropriately.
;
; It takes a sequence of clauses to be evaluated in order, short-circuiting to produce false if any
;  of the clauses produces false, producing the value of the last clause.
;
; Except for the last clause, a clause can be of the form
#;(identifier ← expression)
;  in which case its meaning is: evaluate the expression, and make the identifier refer to the
;  value in subsequent clauses.
;
; Don't use any local naming [local, let, match, define, etc] except for λ parameters:
;  recall that ‘let’ is just a notation for a particular lambda calculus “design pattern”.
  
(module+ test
  #;(check-equal? (Do 324)
                  324)
  #;(check-false (Do #false
                     (/ 1 0)))
  #;(check-false (Do (r1 ← (√ -1))
                     (r2 ← (ln (+ 1 r1)))
                     ((÷ r1) r2)))
  #;(check-false (Do (r1 ← (√ -1))
                     (r2 ← (ln (+ 1 r1)))
                     ((÷ r1) r2))))

(define Do (λ (x) (begin
                    x
                    )))

; Implement ‘E2’, behaving the same way as ‘E1’, but using ‘Do’ notation instead of ‘>>’ and ‘>>=’.

(define E2 (void))

; Implement ‘E3’, behaving the same way as ‘E2’, by expanding each use of ‘Do’ notation in ‘E2’,
;  and also replacing ‘E2’ with ‘E3’. The result will be similar to your ‘E1’, but likely a bit
;  less elegant.

(define E3 (void))
