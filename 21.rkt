#lang racket
; заданное число
(define a (read))
(define i (list 1 5 10 25 50))

(define (call as is)
  (cond
    [(empty? is) 0]
    [(= as 0)    1]
    [(< as 0)    0]
    [else (+ (call as (cdr is)) (call (- as (first is)) is))]))

(call a i)