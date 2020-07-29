#lang racket/base

(require racket/list racket/match)
;; (require (only-in srfi/48
;;                     [format format/48]))
(require (only-in srfi/54
                    [cat cat/54]))

(require csv-reading)
(require csv-writing)


#|
From https://www.reddit.com/r/Anki/comments/736sx4/how_do_i_change_the_card_order_of_a_deck/

You have to first add the field KanjiOrder and put 5-digit numbers there.
( i am surprised your deck does not have it ).
After adding a few cards, sort by the field KanjiOrder and Reposition - to save the order.


Reposition means sort the cards by the sort field, select all cards and then
Menu / Cards / Reposition / From 0, Step 1

And then order the filtered deck by "Due"

|#



(define IN-FILENAME (make-parameter "Remembering Traditional Hanzi 1+2.txt"))
(define OUT-FILENAME "RTH-order.txt")
(define CHARACTER-FIELD-NUM 0)
(define RTH-ID-FIELD-NUM 5)
(define STUDY-ORDER-FIELD-NUM 14)



(define (make-reader [delimiter #\tab])
  (make-csv-reader-maker
   `((separator-chars              ,delimiter)
     (strip-leading-whitespace?  . #f)
     (strip-trailing-whitespace? . #f))))

(define (all-rows port)
  (define read-row ((make-reader) port))
  (for/list ([row (in-producer read-row '())])
      row))

(define (read-file infilename)
  (with-input-from-file infilename
    (lambda () (all-rows (current-input-port)))))

(define (write-file outfilename data)
  (let ((printing-params (make-csv-printing-params
                          #:quotes-only-when-needed? #f)))
   (with-output-to-file outfilename
     (lambda () (display-table data (current-output-port) #:printing-params printing-params ))
     #:exists 'replace)))


(define (read-RTH)
  ;; "~/work/rth-sort/Remembering Traditional Hanzi 1+2.txt"
  (read-file (IN-FILENAME)))


;; (regexp-match #px"([^\\d]*)([\\d]*)([^\\d]*)" "N002P")

(define rth-id-regex #px"^([^\\d]*)([\\d]*)([^\\d]*)$")

(define (calc-study-order-field rth-id)
  (unless (string? rth-id)
    (raise-argument-error 'calc-study-order-field
                          "string"
                          rth-id))
  (match rth-id
    [(pregexp rth-id-regex (list whole prefix num-s suffix))
     (let* ((num-of-rth (string->number num-s))
            (study-order (* num-of-rth 100)))
       (cond [(string=? suffix "P") ; primitive, should be before the character
              (set! study-order (- study-order 50))]

             [(string=? suffix "N") ; "new", insert after
              (set! study-order (+ study-order 20))]

             [(string=? suffix "")] ;; standard, do nothing

             [else (raise-argument-error 'calc-study-order-field
                                         "P or empty string"
                                         (format "~a in ~a" suffix whole))])
       ;; (unless (string=? prefix "")
       ;;   (error (format "Prefix not empty in ~a. This check may be commented out and the prefix will be prepended to StudyOrder." rth-id)))

       (format "~a~a" prefix (cat/54 study-order 6 #\0))) ]
    [_ (raise-argument-error 'calc-study-order-field
                          "like x000x"
                          rth-id)]))

(define (process-row row)
  (list-update row STUDY-ORDER-FIELD-NUM
               (lambda (v) (calc-study-order-field (list-ref row RTH-ID-FIELD-NUM)))))


(define (process (fname #f))
  (parameterize ([IN-FILENAME (or fname (IN-FILENAME))])
   (let ((data (read-RTH)))
     (let* ((newdata (map process-row data))
            (orderdata (map (lambda (row)
                              (list (list-ref row CHARACTER-FIELD-NUM)
                                    (list-ref row STUDY-ORDER-FIELD-NUM)))
                            newdata)))
       (write-file OUT-FILENAME orderdata)
       (take orderdata 5))
     )))


#|

(process)

(process "RT1-8.txt")

|#
