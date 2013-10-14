#lang racket

(define (square x)
  (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (<= x y)
  (or (< x y) (= x y)))

(define (large-sum x y z)
  (if (and (<= x y) (<= x z)) ;x is smallest
      (sum-of-squares y z)
      (if (<= y z) ; y is smallest
          (sum-of-squares x z)
          (sum-of-squares x y))))

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

; checks if guess sqared is close enough to x
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) .00001))

; averages two numbers
(define (average x y)
  (/ (+ x y) 2))

; procedure to improve guess by averaging guess with x/guess
(define (improve guess x)
  (average guess (/ x guess)))

; recursive iterator to find sqrt by improving upon its previous guess
(define (sqrt-iter guess x)
  (if (good-enough? guess x) 
      guess 
      (sqrt-iter (improve guess x) x)))

; sqrt procedure
(define (sqrt x)
  (sqrt-iter 1 x))

; sqrt with proper scoping
(define (sqrt-scope x)
  (define (good-enough-scope? guess)
    (< (abs (- (square guess) x)) .00001))
  (define (improve-scope guess)
    (average guess (/ x guess)))
  (define (sqrt-iter-scope guess)
    (if (good-enough-scope? guess) 
      guess 
      (sqrt-iter-scope (improve-scope guess))))
  (sqrt-iter-scope 1))


