#lang racket
;tree-map
(define (map proc items)
  (if (null? items)
      null
      (cons (proc (car items))
            (map proc (cdr items)))))

;(map abs (list -10 2.5 -11.6 17))

;(map (lambda (x) (* x x)) (list 1 2 3 4))


(define (tree-map f tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map f sub-tree)
             (f sub-tree)))
       tree))

(define (square-tree tree)
  (tree-map sqr tree))

(define tree (list 1 (list 2 (list 10 11 12) 3 4) 5))
(square-tree tree)

(tree-map (lambda (x) (+ x 1)) tree)

