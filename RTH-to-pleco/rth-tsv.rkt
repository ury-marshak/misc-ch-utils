#lang racket/base

(require csv-reading)
(require csv-writing)

(provide read-RTH write-RTH
         CHARACTER-FIELD-NUM KEYWORD-FIELD-NUM KEYWORD-INFO-FIELD-NUM
         PRIMITIVE-FIELD-NUM PRIMITIVE-STORY-FIELD-NUM
         RTH-ID-FIELD-NUM
         ELEMENTS-FIELD-NUM STORY-FIELD-NUM SIMPLIFIED-ID-FIELD-NUM STUDY-ORDER-FIELD-NUM
         TAGS-FIELD-NUM)


(define IN-FILENAME "Flt. RTH Study.txt")
;; (define OUT-FILENAME "RTH-el-pleco.txt")

(define CHARACTER-FIELD-NUM 0)
(define KEYWORD-FIELD-NUM 1)
(define KEYWORD-INFO-FIELD-NUM 2)
(define PRIMITIVE-FIELD-NUM 3)
(define PRIMITIVE-STORY-FIELD-NUM 4)
(define RTH-ID-FIELD-NUM 6)
(define ELEMENTS-FIELD-NUM 8)
(define STORY-FIELD-NUM 9)
(define SIMPLIFIED-ID-FIELD-NUM 12)
(define STUDY-ORDER-FIELD-NUM 16)
(define TAGS-FIELD-NUM 17)



;;; --- Reading the file
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


(define (read-RTH infilename)
  (read-file infilename))


;;; --- Writing

(define (write-RTH outfilename data)
  (let ((printing-params (make-csv-printing-params
                          #:quotes-only-when-needed? #f)))
    (with-output-to-file outfilename
      (lambda () (display-table data (current-output-port) #:printing-params printing-params ))
      #:exists 'replace)))
