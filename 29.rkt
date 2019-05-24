#lang racket
; deep-reverse

; let баундит переменные в боди 
(define (iter items acc)
  (if (null? items)
      acc
      (let ((head (car items))
            (tail (cdr items)))
        (if (list? head)
            (iter tail (append (list (deep-reverse head)) acc))
            (iter tail (append (list head) acc))))))

(define (deep-reverse items)
  (iter items '()))

(define x (list (list 1 2) (list 3 4) 5))
(deep-reverse x)