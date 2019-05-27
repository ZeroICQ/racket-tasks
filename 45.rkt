#lang racket

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

(define str1 '(foo bar baz))
str1
(count-pairs str1)
е
(define x '(foo)) 
(define y (cons x x)) 
(define str2 (list y))
str2
(count-pairs str2)

(define x1 '(foo)) 
(define y1 (cons x1 x1)) 
(define str3 (cons y1 y1))
str3
(count-pairs str3)
