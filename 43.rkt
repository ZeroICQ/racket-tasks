#lang racket

(define (make-account balance password)
  (let ((pass-input-counter 0))
    (define (check-password pass . a)
      (begin
        (cond [(eq? pass password) (begin (set! pass-input-counter 0) true)]
              [else (begin (set! pass-input-counter (+ pass-input-counter 1))
                           (if (>= pass-input-counter 3)
                               (display "Ваш аккаунт заблокирован\n")
                               (begin (display "Осталось ") (display (- 3 pass-input-counter)) (display " попыток\n")))
                           false)])))
  
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (dispatch pass m)
      (if (not (check-password pass)) (lambda . (display "В доступе отказано" ))
          (cond [(eq? m 'withdraw) withdraw]
                [(eq? m 'deposit) deposit]
                [else (error "Unknown request: "
                             m)])))

    dispatch))

(define acc (make-account 100 'kek))
(define block-acc (make-account 100 '123))

((block-acc '321 'withdraw) 50)
((block-acc '321 'withdraw) 50)
((block-acc '321 'withdraw) 50)
((block-acc '321 'withdraw) 50)

((acc 'kekos 'withdraw) 50)
((acc 'kekos 'withdraw) 50)
((acc 'kek 'withdraw) 50)
((acc 'kek 'withdraw) 10)
((acc 'kek 'withdraw) 100)
((acc 'kek 'deposit) 100)
