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


(A 2 4)
