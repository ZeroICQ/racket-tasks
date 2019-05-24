#lang racket
; (define (append .  ll)
; (последний список не должен переписываться)???

(define (prnt l) l)

(define (iter items)
  (if (null? items)
      null
      (append (car items) (iter (cdr items)))))

(define (append-ll .  ll) (iter ll))

(append-ll (list 1 2 3 4 5 6) (list 7 8 9) (list 2 (list 10)))