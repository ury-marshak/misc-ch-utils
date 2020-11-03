#lang racket/base

(require racket/path)
(require racket/list)
(require racket/string)

(require html-parsing)
(require sxml/sxpath)
(require html-writing)



(define (read-from-port port)
  (html->xexp port))

(define (read-file infilename)
  (with-input-from-file infilename
    (lambda () (read-from-port (current-input-port)))))

(define (parse-file infilename)
  (let ((doc (read-file infilename)))
    (define username (car ((sxpath "//div[@id='main_container']/h2/text()" )
                       doc)))
    (define stories ((sxpath "//div[@class='sharedstory rtkframe']" )  ;; "//div[contains(@class, 'rtkframe')]"
      doc))

    (cons username stories)))


(define (get-hanzi story-sxml)
  (let ((hanzis ((sxpath "//div[@class='mystories-kanji']/span/text()") story-sxml)))
    (unless (= (length hanzis) 1)
      (raise-user-error "Bad number of hanzis:  " hanzis))
    (car hanzis)))

(define (get-story-body story-sxml)
  (let ((story-body ((sxpath "//div[@class='story']/node()") story-sxml)))
    ;; (unless (= (length stories) 1)
    ;;   (raise-user-error "Bad length of stories  " stories))
    ;; (car stories)
    story-body))

(define (list-of-xexp-->html nodes)
  (for/fold ((html-string ""))
              ((node nodes))
      (string-append-immutable html-string
                               (xexp->html node))))


(define (story-sxml->html story-sxml)
  (let ((nodes ((sxpath "//node()") story-sxml)))
    ;; (unless (= (length stories) 1)
    ;;   (raise-user-error "Bad length of stories  " stories))
    ;; (car stories)
    (list-of-xexp-->html nodes)))



(struct story (user hanzi sxml-nodes)
  #:transparent) ;;#:mutable

(define (parse-story story-sxml username)
  (let* ((hanzi (get-hanzi story-sxml))
         (story-body (get-story-body story-sxml)))
    (story username hanzi story-body)))

;; (define (process-story story-sxml username)
;;   (define parsed-story (parse-story story-sxml username))

;;   parsed-story)



(define (process-file html-file fn)
  (let* ((file-results (parse-file html-file))
         (username (car file-results))
         (stories-results (cdr file-results))
         (stories (for/list ((story-sxml stories-results)) (parse-story story-sxml username))))
    (when fn
      (for-each fn stories))
    ;; (println stories)
    ;; (println username)
    ;; (printf "results: ~a\n" (length stories-results))
    stories))


(define (test-1)
  (process-file "/Users/ury/work/misc-ch-utils/koohii/koohii/aphasiac2_01.html" #f))


;; (define (trim-if-string s)
;;   (if (string? s)
;;       (string-trim s)
;;       s))

;; (define (trim-equal? a b)
;;   (equal?/recur (trim-if-string a) (trim-if-string b) trim-equal?))

(define (same-story s1 s2)
  (define (trim-if-string s)
    (if (string? s)
        (string-trim s)
        s))
  (define (trimmed-equal? a b)
    (equal?/recur (trim-if-string a) (trim-if-string b) trimmed-equal?))

  (and (string=? (story-hanzi s1) (story-hanzi s2))
       (trimmed-equal? (story-sxml-nodes s1) (story-sxml-nodes s2))))


(define (is-new-story stry stories-list)
  (for/and ((old-story stories-list))
    (not (same-story stry old-story))))


(define (add-to-hash curr-story ht)
  (hash-update! ht (story-hanzi curr-story)
                (lambda (existing-stories)
                  ;; (printf "upd: ~a" existing-stories)
                  (if (is-new-story curr-story existing-stories)
                      (cons curr-story existing-stories)
                      existing-stories))
                (lambda ()  '() ) ))



(define (get-html-files dirpath)
  (filter (lambda (fp)
            (define ext (path-get-extension fp))
            (and ext (bytes=? ext #".html")))
          (directory-list dirpath #:build? #t)))


(define stories-ht (make-parameter #f))


(define (process-dir dirpath)
  (define html-files (get-html-files dirpath))

  ;; (set! html-files (take html-files 1 ))

  (let ((ht (make-hash)))
    (for ((html-file html-files))
      (printf "processing: ~a\n" html-file)
      (process-file html-file
                    (lambda (story) (add-to-hash story ht))))
    ht))

(define (add-stories-to-tsv dirpath)
  (define ht (process-dir dirpath))
  (parameterize ([stories-ht ht])
    (process-RTH-tsv process-tsv-row)))


(define (test-2)
  (process-dir "/Users/ury/work/misc-ch-utils/koohii/koohii/"))

(define (test-3)
  (add-stories-to-tsv "/Users/ury/work/misc-ch-utils/koohii/koohii/")
  (println "done"))


;;

(require "../RTH-to-pleco/rth-tsv.rkt")

(define IN-FILENAME "Remembering Traditional Hanzi 1+2.txt")
(define OUT-FILENAME "RTH-fix1.csv")

(define (process-RTH-tsv process-row (fname-in IN-FILENAME) (fname-out OUT-FILENAME) (take-num 18))
  (let ((data (read-RTH fname-in)))
    (let ((newdata (filter (lambda (x) x) (map process-row data))))
      (write-RTH-csv fname-out newdata)
      (take newdata take-num))))


(define (get-html-stories-for-char char)
  (define stories (hash-ref (stories-ht) char #f))
  (if stories
      (make-html-for-stories stories)
      #f))


(define (process-tsv-row row)
  (let ([char (list-ref row CHARACTER-FIELD-NUM)])
    (define other-stories (get-html-stories-for-char char))
    ;; (unless o-s
    ;;   (set! o-s ""))
    (if other-stories
        (list char other-stories)
        #f)) )


(define (make-html-for-stories stories)
  (define wrapped-stories (for/fold ([acc '()]
                                     #:result (reverse acc))
                                    ([st (in-list stories)])
                            (cons `(dd ,@(story-sxml-nodes st))
                                  (cons `(dt ,(string-append-immutable (story-user st) ":"))
                                        acc))  ))


  (xexp->html `(dl ,@wrapped-stories)))
