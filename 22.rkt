#lang racket
(define (deriv f dx)
                (lambda (x)
                  (/ (- (f (+ x dx)) (f x)) dx)))

(define nth_deriv (lambda (n f dx)
                    (if (= n 0) f (nth_deriv (- n 1) (deriv f dx) dx))))


((nth_deriv 0 (lambda (x) (* x x)) 0.01) 1)
((nth_deriv 1 (lambda (x) (* x x)) 0.01) 1)
((nth_deriv 2 (lambda (x) (* x x)) 0.01) 1)
((nth_deriv 3 (lambda (x) (* x x)) 0.01) 1)
((nth_deriv 4 (lambda (x) (* x x)) 0.01) 1)
;((deriv (lambda (x) (* x x)) 0.01) 1)

