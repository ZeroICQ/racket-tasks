#lang racket
(require math)


(define (repeated f n)
  (if (= n 1) (lambda (x) (f x))  (lambda (x) (f ((repeated f (- n 1))x)) ) ))

((repeated sqr 1) 2)
((repeated sqr 2) 2)
((repeated sqr 3) 2)
((repeated sqr 4) 2)


; λx.f((λx.fx)x)

;λx.f ((λx.f((λx.fx)x)x))
;2^2=4 4^2 = 16 16^2 256