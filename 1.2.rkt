#lang racket

(define (slow-factorial n)
  (if (= n 1) 1
      (* n (slow-factorial (- n 1)))))

(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product) 
              (+ counter 1))))
  (iter 1 1))

; Ackerman function
(define (A x y) 
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1) (A x (- y 1))))))


(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                  (fib (- n 2))))))

(define (fib2 n)
  (fib-iter 1 0 n))

(define (fib-iter a b count)
  (if (= count 0)
    b
    (fib-iter (+ a b) a (- count 1))))

; Counting Change problem
(define (first-denomination kind-of-coins)
  (cond ((= kind-of-coins 1) 1)
        ((= kind-of-coins 2) 5)
        ((= kind-of-coins 3) 10) 
        ((= kind-of-coins 4) 25)
        ((= kind-of-coins 5) 50)))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount (- kinds-of-coins 1))
                 (cc (- amount (first-denomination kinds-of-coins)) 
                     kinds-of-coins)))))
(define (count-change amount)
  (cc amount 5))

(define (recurse-f n)
  (if (< n 3) n 
    (+ (recurse-f (- n 1)) 
       (* 2 (recurse-f (- n 2)))
       (* 3 (recurse-f (- n 3))))))

(define (iter-f n)
  (define (iter a b c n)
    (if (< n 3) 
      a
      (iter (+ a (* 2 b) (* 3 c)) a b (- n 1))))
  (if (< n 3)
    n
    (iter 2 1 0 n)))


(define (pascal r c)
  (cond ((= r c) 1)
        ((= r 1) 1)
        ((+ (pascal (- r 1) (- c 1))
            (pascal r (- c 1))))))

(define (cube x) (* x x x))

(define (p x) (- (* 3 x) (* 4 (cube x))))

(define (sine angle)
  (if (not (> (abs angle) 0.1))
    angle
    (p (sine (/ angle 3.0)))))

(define (expt b n)
  (if (= n 0)
    1
    (* b (expt b (- n 1)))))

(define (expt-iter b product n)
  (if (= n 0)
    product
    (expt-iter b 
                (* product b) 
                (- n 1))))

(define (expt-i b n)
  (expt-iter b 1 n))

(define (even? n)
  (= (remainder n 2) 0))

(define (square n)
  (* n n))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (double a)
  (+ a a))

(define (half a)
  (/ a 2))

; 2 * 4 = 4*2 = 8 *1
(define (fast-mult a b)
  (cond ((= b 0) 0)
        ((= b 1) a)
        ((even? b) (fast-mult (double a) (half b)))
        (else (+ a (fast-mult a (- b 1))))))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= (smallest-divisor n) n))

