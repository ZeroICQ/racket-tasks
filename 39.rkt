#lang racket

(define (deriv exp var)
  (cond [(number? exp) 0]
        [(variable? exp) (if (same-variable? exp var) 1 0)]
        [(sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var))]
        [(product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp)))]
        [(exponentiation? exp) (make-product
                          (exponent exp)
                          (make-product
                           (make-exponentiation (base exp) (make-sum (exponent exp) -1))
                           (deriv (base exp) var)))]
        [else
         (error "unknown expression type" exp)]))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-product m1 m2)
  (cond [(or (=number? m1 0) (=number? m2 0)) 0]
        [(=number? m1 1) m2]
        [(=number? m2 1) m1]
        [(and (number? m1) (number? m2)) (* m1 m2)]
        [else (list '* m1 m2)]))

(define (sum? x) (and (pair? x) (eq? (car x) '+)))

(define (make-sum a1 a2)
  (cond [(=number? a1 0) a2]
        [(=number? a2 0) a1]
        [(and (number? a1) (number? a2))
         (+ a1 a2)]
        [else (list '+ a1 a2)]))

; cadr := (car (cdr x))
; addend= first element in sum
(define (addend s) (cadr s))

; caddr := (car (cdr (cdr v))).
; augend = second element in sum
(define (augend s) (caddr s))

(define (product? x) (and (pair? x) (eq? (car x) '*)))
(define (exponentiation? x) (and (pair? x) (eq? (car x) '**)))

(define (make-exponentiation b e)
  (cond [(=number? e 0) 1]
        [(=number? e 1) b]
        [(and (number? b) (number? e)) (expt b e)]
        [else (list '** b e)]))

(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

(define (base e) (cadr e))
(define (exponent e) (caddr e))

(define (=number? exp num) (and (number? exp) (= exp num)))



;(deriv '(+ x 3) 'x) ;1
;(deriv '(* x y) 'x) ;y
;(deriv '(* (* x y) (+ x 3)) 'x) ;(+ (* x y) (* y (+ x 3)))

; exponent:

(deriv '(+ x (* 4 x)) 'x)
(deriv '(** x 8) 'x)
(deriv '(** x 7) 'x)

(make-exponentiation 'x 0)
(make-exponentiation 1 'f)
(make-exponentiation 2 3)
;(deriv '(** 2 3) 'x)

(define test-exponent (make-exponentiation 'a 3))
(define (test-sum) (make-sum 'a 'b))
(display "\n")
test-exponent
(exponentiation? test-exponent)
(exponentiation? test-sum)

(base test-exponent)
(exponent test-exponent)


