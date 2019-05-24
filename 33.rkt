#lang racket

;(define (map  l)
; (accumulate (lambda (x y) <??>) nil l))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (map proc seq)
  (accumulate (lambda (a b) (cons (proc a) b)) '() seq))

(define mymap (list 1 2 3 4 5 6 7 9))

(map (lambda (element) (+ element 1)) mymap)
(map (lambda (element) (* element 2)) mymap)