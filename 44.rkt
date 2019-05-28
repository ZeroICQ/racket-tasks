#lang racket
;Изобразить диаграммы среды после оценивания 1-6:

(define (make-account b)
  (define (withdraw a)
    (if (>= b a)
        (begin (set! b (- b a))
               b)
        "Insufficient funds"))
  (define (deposit a)
    (set! b (+ b a))
    b)
  (define (dispatch m)
    (cond ((eq? m 'w) withdraw)
          ((eq? m 'd) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  dispatch)

(define a1 (make-account 100))
(define a2 (make-account 50))
((a1 'd) 25)
; 125
((a1 'd) 25)
; 150

((a2 'd) 25)
;25