#lang racket/base

(provide all-duplicates)

;; all-duplicates
;; Returns a list of duplicate items in lst. More precisely, it returns all the x such that
;; there was a previous y where (same? (extract-key x) (extract-key y)). If an item is present
;; in the list N times, it will be present (N-1) times in the duplicates list.
(define (all-duplicates items
                        [same? equal?]
                        #:key [key values])

  (unless (list? items)
    (raise-argument-error 'check-duplicates "list?" 0 items))
  (unless (and (procedure? key)
               (procedure-arity-includes? key 1))
    (raise-argument-error 'check-duplicates "(-> any/c any/c)" key))

  (let ((table (cond [(eq? same? equal?)  (make-hash)]
                     [(eq? same? eq?)  (make-hasheq)]
                     [(eq? same? eqv?) (make-hasheqv)]
                     [else (raise-argument-error 'all-duplicates
                                                 "one of equal?, eq?, eqv?"
                                                 same?) ])))
    (let loop ([items items]
               [dups '()])
      (if (pair? items)
          (let ([key-item (key (car items))])
            (if (hash-ref table key-item #f)
                (loop (cdr items) (cons (car items) dups))
                (begin (hash-set! table key-item #t)
                       (loop (cdr items) dups))))
          (reverse dups)))))
