#lang racket

; memq находится ли элемент в списке. eq?


(define (cycle? x)
  (let ((visited '()))
    (define (cycle-helper x)
      (cond [(not (pair? x)) false]
            [(memq x visited) true]
            [else (set! visited (cons x visited))
             (or (cycle-helper (car x))
                 (cycle-helper (cdr x)))]))
    (cycle-helper x)))

(define t1 (cons 'a 'b)) 
(define t2 (cons t1 t1))
  
(cycle? t2)
(cycle? '(1 2 3))