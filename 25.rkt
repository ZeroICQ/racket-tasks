#lang racket
(require math)

(define (cons a b)
  (* (expt 2 a)
     (expt 3 b)))

;  (define (divides? a b)
;    (= (remainder b a) 0))

(define (iter a n acc)
  (if (divides? a n)
      (iter a (/ n a) (+ 1 acc))
      acc))

(define (extract-expt a n)
  (iter a n 0))

(define (car z)
  (extract-expt 2 z))

(define (cdr z)
  (extract-expt 3 z))

(car (cons 2 3))
