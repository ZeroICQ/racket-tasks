#lang racket

(define (count-pairs x)
  (let ((visited '()))
    (define (count x)
      (cond [(not (pair? x)) 0]
            [(memq x visited) 0]
            [else
             (set! visited (cons x visited))
             (+ (count (car x))
                (count (cdr x))
                1)]))
    (count x)))

(define str1 '(foo bar baz))
str1
(count-pairs str1)

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
