#lang racket

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch pass m)
    (if (not (eq? pass password)) (lambda . (display "Wrong password"))
    (cond [(eq? m 'withdraw) withdraw]
          [(eq? m 'deposit) deposit]
          [else (error "Unknown request: "
                       m)])))
  dispatch)

(define acc (make-account 100 'kek))


((acc 'kekos 'withdraw) 50)
