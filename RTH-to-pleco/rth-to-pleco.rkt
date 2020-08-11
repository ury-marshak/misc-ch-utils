#lang racket/base

(require racket/list)
(require  racket/string)
(require "rth-tsv-reader.rkt")
(require "pleco-format.rkt")

;;(require csv-writing)


(define IN-FILENAME "Remembering Traditional Hanzi 1+2.txt")
(define OUT-FILENAME "RTH-el-pleco.txt")

;; (define CHARACTER-FIELD-NUM 0)
;; (define KEYWORD-FIELD-NUM 1)
;; (define KEYWORD-INFO-FIELD-NUM 2)
;; (define PRIMITIVE-FIELD-NUM 3)
;; (define RTH-ID-FIELD-NUM 5)
;; (define ELEMENTS-FIELD-NUM 7)
;; (define STORY-FIELD-NUM 8)
;; (define SIMPLIFIED-ID-FIELD-NUM 11)
;; (define STUDY-ORDER-FIELD-NUM 15)


(define USED-ELEMENTS-FIELD-NUM STORY-FIELD-NUM) ;; Change to ELEMENTS-FIELD-NUM when it's ready


;;; ---

(define (choose-tag tags)
  (let ([result (findf (lambda (t)
                         (member t tags))
                       '("RTH1" "RTH2" "RT1-8"))])
    (unless result
      (error "No useful tag in " tags))
    result))

(define (format-article row)
  (let ([elements (list-ref row USED-ELEMENTS-FIELD-NUM)]
        [rth-index (list-ref row RTH-ID-FIELD-NUM)]
        [tags (string-split (list-ref row TAGS-FIELD-NUM))]
        [primitive (list-ref row PRIMITIVE-FIELD-NUM)]
        [keyword (list-ref row KEYWORD-FIELD-NUM)]
        [lines '()])
    (define (push val) (set! lines (cons val lines)))


    (push (format "~a ~a (~a)" rth-index (pleco:bold keyword) (choose-tag tags)) )

    ;;(push "")

    (unless (string=? primitive "")
      (push (format "~a~a~a" (pleco:italic "(prim: ") (pleco:from-html primitive) (pleco:italic ")" ) ) ))

    (push "")

    (push (pleco:from-html elements))

    ;;
    (string-join (reverse lines) (pleco:newline))))


(define (process-row row)
  (let* ([trad (string-trim (list-ref row CHARACTER-FIELD-NUM))]
         [simp (string-trim (list-ref row SIMPLIFIED-ID-FIELD-NUM))]
         [heading (if (or (string=? simp "") (string=? simp trad))
                      trad
                      (format "~a[~a]" simp trad))]
         [pinyin ""]
         [article (format-article row)])
    (format "~a\t~a\t~a" heading pinyin article)))


(define (process-file (fname IN-FILENAME))
  (let ((data (read-RTH IN-FILENAME)))
    (let* ((pleco-strings (map process-row data)))
      (call-with-output-file OUT-FILENAME
        (lambda (out)
          (for ([s pleco-strings])
            (write-string s out)
            (write-string "\n" out)))
        #:exists 'replace)
      (take pleco-strings 18))
    ))
