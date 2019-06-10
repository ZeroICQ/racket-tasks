#lang racket
(require rnrs/mutable-pairs-6)
(require compatibility/mlist)
(require math/number-theory)

(define (make-table)
  (define (make-record key value) (cons key value))
  (define (record-key record) (car record))
  (define (record-value record) (cdr record))
  (define (entry tree) (car tree))
  (define (left-branch tree) (cadr tree))
  (define (right-brach tree) (caddr tree))
  (define (make-tree entry left right)
    (list entry left right))

  (define (adjoin-set x set)
    (cond ((null? set) (make-tree x '() '()))
          ((= (record-key x) (record-key (entry set)))
           set)
          ((< (record-key x) (record-key (entry set)))
           (make-tree (entry set)
                      (adjoin-set x (left-branch set))
                      (right-brach set)))
          (else
           (make-tree (entry set)
                      (left-branch set)
                      (adjoin-set x (right-brach set))))))

  (let ((local-table '()))
    (define (lookup key)
      (define (recur key tree)
        (if (null? tree) 
            false
            (let ((current-key (record-key (entry tree))))
              (cond ((= key current-key)
                     (entry tree))
                    ((< key current-key)
                     (recur key (left-branch tree)))
                    (else
                     (recur key (right-brach tree)))))))
      (recur key local-table))
    (define (insert! key value)
      (set! local-table 
            (adjoin-set (make-record key value)
                        local-table)))
    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
            ((eq? m 'insert!) insert!)
            (else (error "Unkown operation -- TABLE" m))))
    dispatch))

(define (lookup key table)
  ((table 'lookup) key))
(define (insert! key value table)
  ((table 'insert!) key value))


(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((previously-computed-result
             (lookup x table)))
        (or previously-computed-result
            (let ((result (f x)))
              (insert! x result table)
              result))))))

(define memo-fib
  (memoize
   (lambda (n)
     (cond [(= n 0) 0]
           [(= n 1) 1]
           [else (+ (memo-fib (- n 1))
                    (memo-fib (- n 2)))]))))

;1) почему (memoize fib)  не сокращает число шагов оценивания fib
;((let ((table (make-table)))
;   (lambda (x)
;     (let ((previously-computed-result (lookup x table)))
;       (or previously-computed-result
;           (let ((result (f x)))  ; <== here the naive (fib 3) will be called
;             (insert! x result table)
;             result))))) 
; 3)

