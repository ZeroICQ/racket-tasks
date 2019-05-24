#lang racket
;определить операции на векторах и матрицах, представленных в виде списков или списков списков (по строкам):
; (define (dot-product v w)
;    (accumulate + 0 (append-n <??> <??> (list v w) )))

;(define (matrix-*-vector m v)
;(map <??> m))

;(define (transpose mat)
;(accumulate-n <??> <??> mat))

;(define (matrix-*-matrix m n)
;    (let ((cols (transpose n)))
;        (map <??> m)))


;(define
;  (dot-product v w)
;  (if (or (null? v)  (null? w))
;      null
;      (append (list (* (car v) (car w))) (dot-product (cdr v) (cdr w)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init x)
  (if (null? (car x))
      '()
      (cons (accumulate op init (map (lambda (x) (car x)) x))
            (accumulate-n op init (map (lambda (x) (cdr x)) x)))))


(define (dot-product x y)
  (accumulate + 0 (accumulate-n * 1 (list x y))))

(define a (list 1 2 3 4 5))
(define b (list 1 2 3 4 5))
(dot-product a b)

; (define (matrix-*-vector m v)
; (map <??> m))

(define (matrix-*-vector m v)
  (map (lambda (y) (dot-product y v)) m))

; 1 2 3    1   14
; 4 5 6  * 2 = 32
; 7 8 9    3   50
(matrix-*-vector '((1 2 3) (4 5 6) (7 8 9)) '(1 2 3))

(define (transpose mat)
  (accumulate-n (lambda (x y) (append (list x) y)) '() mat))
(transpose '((1 2 3) (4 5 6) (7 8 9)))

;(define (matrix-*-matrix m n)
;    (let ((cols (transpose n)))
;        (map <??> m)))

(define (matrix-*-matrix m1 m2)
  (map (lambda (x) (matrix-*-vector (transpose m1) x)) m2))
(matrix-*-matrix '((1 2 3) (4 5 6) (7 8 9)) '((1 2 3) (4 5 6) (7 8 9)))

