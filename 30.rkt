#lang racket

; fringe принимает дерево в виде списка, возвращает список, элементы которого - листья, в порядке слева направо

(define (iter items acc)
  (if (null? items)
      acc
      (let ((head (car items))
            (tail (cdr items)))
        (if (list? head)
            (iter tail (append acc (fringe head)))
            (iter tail (append acc (list head)))))))

(define (fringe items)  
  (iter items '()))

(define x (list (list 1 2) 5 (list 3 4)))

(fringe x)