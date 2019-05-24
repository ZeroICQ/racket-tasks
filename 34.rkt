#lang racket
; брать первые
;Дополнить определение accumulate-n, которая осуществляет accumulate
;по соответствующим элементам входного списка списков:
; (define (accumulate-n op init seqs)

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))


(define (accumulate-n op init x)
  (if (null? (car x))
      '()
      (cons (accumulate op init (map (lambda (x) (car x)) x))
            (accumulate-n op init (map (lambda (x) (cdr x)) x)))))

(define lol (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))
(accumulate-n (lambda (x y) (+ x y)) 0 lol)
(accumulate-n (lambda (x y) (* x y)) 1 lol)