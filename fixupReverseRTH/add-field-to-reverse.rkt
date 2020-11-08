#lang racket/base

(require racket/list)

(require "../RTH-to-pleco/rth-tsv.rkt")
(require "../RTH-to-pleco/enum.rkt")


(enum
 R.CHARACTER-FIELD-NUM
 R.KEYWORD-FIELD-NUM
 R.KEYWORD-INFO-FIELD-NUM
 R.PRIMITIVE-FIELD-NUM
 R.PRIMITIVE-STORY-FIELD-NUM
 R.PINYIN-FIELD-NUM
 R.RTH-ID-FIELD-NUM
 R.STROKES-FIELD-NUM
 R.ELEMENTS-FIELD-NUM
 R.STORY-FIELD-NUM
 R.HINT-FIELD-NUM
 R.COMMENTS-FIELD-NUM
 R.SIMPLIFIED-ID-FIELD-NUM
 R.MEASURE-WORD-FIELD-NUM
 R.MEANING-FIELD-NUM
 R.CANGJIE5-FIELD-NUM
 R.STUDY-ORDER-FIELD-NUM
 R.OTHER-STORIES-FIELD-NUM
 R.TAGS-FIELD-NUM
)


(define RTH-FILENAME "Remembering Traditional Hanzi 1+2.txt")
(define IN-FILENAME "Reverse RTH.txt")
(define OUT-FILENAME "RevRTH-fix1.csv")


(define FIELDS-CORRESPONDENCE `( ;(,R.CHARACTER-FIELD-NUM . ,CHARACTER-FIELD-NUM)
                                 (,R.PRIMITIVE-FIELD-NUM . ,PRIMITIVE-FIELD-NUM)
                                 (,R.PRIMITIVE-STORY-FIELD-NUM . ,PRIMITIVE-STORY-FIELD-NUM)
                                 (,R.PINYIN-FIELD-NUM . ,PINYIN-FIELD-NUM)
                                 (,R.STROKES-FIELD-NUM . ,STROKES-FIELD-NUM)
                                 (,R.ELEMENTS-FIELD-NUM . ,ELEMENTS-FIELD-NUM)
                                 (,R.STORY-FIELD-NUM . ,STORY-FIELD-NUM)
                                 (,R.HINT-FIELD-NUM . ,HINT-FIELD-NUM)
                                 (,R.COMMENTS-FIELD-NUM . ,COMMENTS-FIELD-NUM)
                                 (,R.MEASURE-WORD-FIELD-NUM . ,MEASURE-WORD-FIELD-NUM)
                                 (,R.MEANING-FIELD-NUM . ,MEANING-FIELD-NUM)
                                 (,R.CANGJIE5-FIELD-NUM . ,CANGJIE5-FIELD-NUM)
                                 (,R.OTHER-STORIES-FIELD-NUM . ,OTHER-STORIES-FIELD-NUM)
                                 ))
(define FIELDS-TO-CHANGE (map car FIELDS-CORRESPONDENCE))


(define (get-corresponding-field field-num)
  (let ([field-pair (assoc field-num FIELDS-CORRESPONDENCE)])
    (if field-pair
        (cdr field-pair)
        #f)))

(define rth-ht (make-parameter #f))


(define (load-RTH (fname-in RTH-FILENAME))
  (let ((data (read-RTH fname-in)))
    (for-each (lambda (row)
                (let ([rth-index (list-ref row RTH-ID-FIELD-NUM)])
                  (hash-set! (rth-ht) rth-index row)))
              data))
  #f)


(define (missing-reverse (fname-in IN-FILENAME))
  (let ((data (read-RTH fname-in)))
    (let ((newdata (filter (lambda (x) x) (map (lambda (row)
                                                 (let* ([rth-index (list-ref row R.RTH-ID-FIELD-NUM)]
                                                        [rth-row (hash-ref (rth-ht) rth-index #f)])
                                                   (if rth-row #f rth-index)
                                                   ))
                                               data))))
      newdata
      )))


(define (process-reverse (fname-in IN-FILENAME) (fname-out OUT-FILENAME) (take-num 18))
  (let ((data (read-RTH fname-in)))
    (let ((newdata (filter (lambda (x) x) (map process-row data))))
      (write-RTH-csv fname-out newdata)
      (take newdata take-num))))


(define (process-row row)
  (let* ([rth-index (list-ref row R.RTH-ID-FIELD-NUM)]
         [rth-row (hash-ref (rth-ht) rth-index #f)])

    (for/list ([i (in-naturals)]
               [value (in-list row)])
      ;; (println rth-index)
      (let ([rth-field (get-corresponding-field i)])
        (if (and rth-field rth-row)
            (list-ref rth-row rth-field)
            value))
      )) )



(define (run-fix)
  (parameterize ([rth-ht (make-hash)])
    (load-RTH)
    (process-reverse)
    ))

(define (missing)
  (parameterize ([rth-ht (make-hash)])
    (load-RTH)
    (missing-reverse)
    ))
