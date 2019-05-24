#lang racket
; (define (length l)

(define (inc a b) (+ b 1))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (length l)
  (accumulate inc 0 l))

(length (list 1 2 3 4 5 6))

(length (list 1 2 3 4 5 6 7 8 9 10 11 12))
(length '())