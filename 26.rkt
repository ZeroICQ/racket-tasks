#lang racket
    
(define nil '())

(define (iter head items acc)
    (if (null? items)
        acc
        (iter head (cdr items)
              (if (even? (+ (car items) head))
                  (append acc (list (car items)))
                  acc))))

(define (same-parity head . tail)
  (cons head (iter head tail nil)))

;(define a (list 2 2 3 4 5 6))
(same-parity 1 2 3 4 5 6 7)

(same-parity 6 2 3 4 5 6)
