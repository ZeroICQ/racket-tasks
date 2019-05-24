#lang racket

;Сформировать структуры с помощью cons, которые приводят к печати 
;(1 . 2 3)
;(1 2 . 3);
;(1 . 2 . 3)

(define (print-list-structure x)
  (define (print-contents y)
    (print-list-structure (car y))
    (cond
      ((null? (cdr y)) '())
      ((not (pair? (cdr y)))
       (display " . ")
       (print-list-structure (cdr y)))
      (else
       (display " ")
       (print-contents (cdr y)))))
  (cond
    ((null? x)
     (display "()"))
    ((not (pair? x))
     (display x))
    (else
     (display "(")
     (print-contents x)
     (display ")"))))

;(cons (list 1) (list 2 3))
;(cons 2 (cons 3 '()))
;(cons (cons 1 2) (cons 3 null))

;(cons 1 (cons 2 3))
;c
;(print-list-structure (cons (cons 1 null) (cons 2 3)))
;(display "\n")

(print-list-structure (cons 1 (cons 2 3)))
(display "\n")
(print-list-structure (cons (cons 1 2) (cons 3 '())))
; нельзя, первые скобки не уйдут,
; т.к. для точки нужен второй элемент пары, не пара, а у нас ещё 3 есть
; без вложенной пары нельзя, получаются скобки
(display "\n")