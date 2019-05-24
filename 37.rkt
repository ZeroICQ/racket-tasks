#lang racket

;fold-right=accumulate 
(define (accumulate op init x)
  (if (null? x)
      init
      (op (car x)
          (accumulate op init (cdr x)))))

(define fold-right accumulate)
;Таким образом, при оценке 
;(fold-right op init x)
; для x=(x1,x2,...,xn) формируется и затем оценивается выражение
;(op x1 (op x2 (op ...(op xn init)...)))

(define (fold-left op init x)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))  (cdr rest))))
  (iter init x))
;Таким образом, при оценке 
;(fold-left op init x)
; для x=(x1,x2,...,xn) формируется и затем оценивается выражение
;(op (...  (op (op init x1) x2)...) xn)

(fold-right / 1 (list 1 2 3 4))
; ( 1 / (2 / (3 /(4/1) )))
; 1) 4/1=4
; 2) 3/4
; 3) 3/8

(fold-left / 1 (list 1 2 3 4))
; (((1/1) / 2) / 3) / 4

(fold-right list null (list 1 2 3 4))
;(list 1 (list 2 (list 3 (list 4 null ))))

; (list (list (list (list null 1) 2) 3) 4)
(fold-left list null (list 1 2 3 4))

; ассоциативность