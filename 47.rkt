#lang racket
(require rnrs/mutable-pairs-6)
(require compatibility/mlist)

(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (mcdr x)))
          (set-mcdr! x y)
          (loop temp x))))
  (loop x '()))

(define v (mlist 'a 'b 'c))
(define w (mystery v))
w
; список в обратном порядке
