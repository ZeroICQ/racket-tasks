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
        [else
          (error "unknown expression type" exp)]))

(define (variable? x) (symbol? x))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

; возвращает true, если в списке есть оператор
(define (find-op op seq)
  (accumulate (lambda (a b) (or b (eq? a op))) false seq))

(define (inc a b) (+ b 1))

(define (length l)
  (accumulate inc 0 l))

;(define (flatten x) (if (and (pair? x) (null? (cdr x)) (= (length x) 1)) (car x)
;                        (accumulate (lambda (a b) (if (pair? a) (append a b) (append (list a) b))) '() x)))


(define (flatten x) (if (and (pair? x) (null? (cdr x))) (car x) x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-product m1 m2)
  (cond [(or (=number? m1 0) (=number? m2 0)) 0]
        [(=number? m1 1) m2]
        [(=number? m2 1) m1]
        [(and (number? m1) (number? m2)) (* m1 m2)]
        [else (list m1 '* m2)]))

(define (sum? x) (and (pair? x) (find-op '+ x)))

(define (make-sum a1 a2)
  (cond [(=number? a1 0) a2]
        [(=number? a2 0) a1]
        [(and (number? a1) (number? a2))
         (+ a1 a2)]
        [else (list a1 '+ a2)]))

; variant of fold-left
(define (get-pre element seq)
  (define (iter result rest)
     (cond [(or (null? rest) (equal? (car rest) element)) result]
           [else (iter (append result (list (car rest))) (cdr rest))]))
   (iter '() seq))

(define (get-post element seq)
  (define (iter rest)
     (if (equal? (car rest) element) (cdr rest) (iter (cdr rest))))
   (iter seq))

; cadr := (car (cdr x))
;; addend= first element in sum
(define (addend s) (flatten (get-pre '+ s)))

; caddr := (car (cdr (cdr v))).
; augend = second element in sum
(define (augend s) (flatten (get-post '+ s)))

(define (product? x) (and (pair? x) (find-op '* x) (not (find-op '+ x))))

(define (multiplier p) (flatten (get-pre '* p)))
(define (multiplicand p) (flatten (get-post '* p)))

(define (=number? exp num) (and (number? exp) (= exp num)))

;(deriv '(x + x + (x* 6)) 'x)
;(define a '(x + x + (x * 6)))
;(sum? a);t
;(product? a);f
;(define b '(x * x * (x + 6)))
;(sum? b);f
;(product? b);t

;(define c '(x * (x + b) * x + (x + 6)))
;(get-pre '+ c)
;(get-post '+ c)
;(addend c)
;(augend c)

;(define d '(x + 3 * x * (x + y) + z))

(deriv d 'x)
(deriv d 'y)
(deriv d 'z)

(deriv 'y 'x)
