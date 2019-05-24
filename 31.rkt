#lang racket

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))


(define (append l1 l2)
  (accumulate cons l2 l1))

(append (list (list 10 12) 1 2 3) (list 4 5 (list 7 8) ))

