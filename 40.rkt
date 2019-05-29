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


(define (flat-one x) (if (and (pair? x) (null? (cdr x))) (car x) x))
(define (flat x) (accumulate (lambda (a b) (if (pair? a) (append a b) (append (list a) b))) '() x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-product m1 m2)
  (cond [(or (=number? m1 0) (=number? m2 0)) 0]
        [(=number? m1 1) m2]
        [(=number? m2 1) m1]
        [(and (number? m1) (number? m2)) (* m1 m2)]
        [else (remove-brackets-product m1 m2)]))

(define (sum? x) (and (pair? x) (find-op '+ x)))
(define (remove-brackets-product m1 m2)
  (cond [(and (product? m1) (product? m2)) (append m1 '* m2)]
        [(and (product? m1) (not (product? m2))) (append m1 (list '* ) (list m2))]
        [(and (not (product? m1)) (product? m2)) (append (list m1) (list '*) m2)]
        [else (append (list m1) (list '*) (list m2))]))

(define (make-sum a1 a2)
  (cond [(=number? a1 0) a2]
        [(=number? a2 0) a1]
        [(and (number? a1) (number? a2))
         (+ a1 a2)]
        [else (flat (list a1 '+ a2))]))

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
(define (addend s) (flat-one (get-pre '+ s)))

; caddr := (car (cdr (cdr v))).
; augend = second element in sum
(define (augend s) (flat-one (get-post '+ s)))

(define (product? x) (and (pair? x) (find-op '* x) (not (find-op '+ x))))

(define (multiplier p) (flat-one (get-pre '* p)))
(define (multiplicand p) (flat-one (get-post '* p)))

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

(define d '(x + 3 * x * (x + y) + z))

(deriv d 'x)
(deriv d 'y)
(deriv d 'z)

(deriv 'y 'x)

(define e '(x * x * x))
(deriv e 'x)

(deriv '(x * (x + y * z)) 'x)
(deriv '(x * (x + y * z)) 'y)
(deriv '(x * (x + y * z)) 'z)

(deriv '(x + (x + x + x)) 'x)
(deriv '(x + (x + x * x + x)) 'x)
(deriv '((x * x) * (x * x)) 'x)

