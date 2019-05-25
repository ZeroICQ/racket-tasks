#lang racket

(define (inc x) (+ x 1))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (occurances seq el)
  (accumulate (lambda (i counter)
                (cond [(and (number? i) (number? el)) (if (= i el) (inc counter) counter)]
                      [(and (symbol? i) (symbol? el)) (if (eq? i el) (inc counter) counter)]
                      [else (if (equal? i el) (inc counter) counter)])) 0 seq))

(define test-list (list 1 1 1 2 3 '4a '4a '4a '5 '6 (list 9 0 4)))

(occurances test-list '4a)
(occurances test-list 4)
(occurances test-list (list 9 0 4))
(occurances test-list (list 9 1 4))