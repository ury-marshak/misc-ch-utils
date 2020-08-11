#lang racket/base

(require racket/list)
;; (require  racket/string)

(require "rth-tsv.rkt")
(require "utils.rkt")

(define IN-FILENAME "Remembering Traditional Hanzi 1+2.txt")
(define OUT-FILENAME "RTH-fix1.csv")




(define (process-file process-row (fname-in IN-FILENAME) (fname-out OUT-FILENAME) (take-num 18))
  (let ((data (read-RTH IN-FILENAME)))
    (let ((newdata (map process-row data)))
      (write-RTH-csv OUT-FILENAME newdata)
       (take newdata take-num))))



;;; macro define-processor that expands to this:
;; (define (remove-entities-from-keyword)
;;   (define (process-row row)
;;     (let ([char (list-ref row CHARACTER-FIELD-NUM)]
;;           [keyword (list-ref row KEYWORD-FIELD-NUM)])
;;       (let ([newkeyword (remove-entities keyword)])
;;         (unless (string=? keyword newkeyword)
;;           (printf "~a\n" keyword))
;;         (list char newkeyword))))
;;   (process-file process-row IN-FILENAME OUT-FILENAME 18))

(define-syntax define-processor
  (syntax-rules ()
    [(_ (name row-param param ...) body1 body ...)
     (define (name param ...)
       (define (process-row row-param)
         body1 body ...)
       (process-file process-row IN-FILENAME OUT-FILENAME))]))


;; Remove HTML entities such as &nbsp; from the Keyword field
(define (remove-entities s)
  ;; (set! s (regexp-replace* #rx"&quot;" s "\""))
  (set! s (regexp-replace* #rx"&nbsp;" s " "))
  s)

(define-processor (remove-entities-from-keyword row)
  (let ([char (list-ref row CHARACTER-FIELD-NUM)]
        [keyword (list-ref row KEYWORD-FIELD-NUM)])
    (let ([newkeyword (remove-entities keyword)])
      (unless (string=? keyword newkeyword)
        (printf "~a\n" keyword))
      (list char newkeyword)))  )



;; check specified fields for possible duplicates,
;; print first duplicate if found

(define (check-uniqueness)
  (let ((data (read-RTH IN-FILENAME)))
    (define (check-field fld-num)
      (define (get-field row)
        (list-ref row fld-num))
      (let ([dups (all-duplicates data
                                  #:key get-field)])
        (when dups
          (printf "~a" (map get-field dups)))))

    (for-each check-field
              (list RTH-ID-FIELD-NUM CHARACTER-FIELD-NUM KEYWORD-FIELD-NUM STUDY-ORDER-FIELD-NUM))
    (newline)))
