#lang racket
; 2.41: n, s -заданные натуральные числа.
;Сформировать последовательность троек (i,j,k), 1<=k<j<i<=n, таких, что
;i+j+k=s.

(define (inc x) (+ x 1))

(define (check-conditions n s k j i)
  (cond
    [(> (+ k j i) s) false]
    [(>= k j) false]
    [(>= j i) false]
    [(> i n) false]
    [else true]))


(define (iter-i result n s k j i)
  (if (not (check-conditions n s k j i))
      result
      (if (= (+ k j i) s)
          (cons (list i j k) result)
          (iter-i (iter-j result n s k j i) n s k j (inc i)))))

(define (iter-j result n s k j i)
  (if (not (check-conditions n s k j i))
      result
      (if (= (+ k j i) s)
          (cons (list i j k) result)
          (iter-j (iter-k result n s k j i) n s k (inc j) i))))

  
(define (iter-k result n s k j i)
  (if (not (check-conditions n s k j i))
      result
      (if (= (+ k j i) s)
          (cons (list i j k) result)
          (iter-k result n s (inc k) j i))))


;(define (iter n s result k j i)
;  (if (> (+ k j s) s) result)
;  (if (or (>= k j) (>= j i)) (result))
;  (if (= (+ k j i) s) (result))
;  (iter n s )

(define (build n s) (iter-i null n s  1 2 3))

(build 5 8)
(build 7 10)
; flatmap


