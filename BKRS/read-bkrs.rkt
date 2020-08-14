#lang racket/base

(require racket/port racket/match
         racket/string)

(require db/base db/sqlite3)


(define IN-FILENAME "/Users/ury/work/misc-ch-utils/BKRS/BKRS.dsl")

;; Comments {{ ... }} in the .DSL need to be removed beforehand, for example:
;; $ sed '/{{.*}}/ d; /{{/,/}}/ d' BKRS.utf8 >BKRS-nocomments.utf8

(define (do-read-dsl infilename process-article #:encoding (encoding #f) #:limit (limit #f))
  (with-input-from-file infilename
    (lambda ()
      (when encoding
        (current-input-port (reencode-input-port (current-input-port) encoding)))

      (let ([curr-headings #f]
            [curr-article-lines #f]
            [reading-file-headings #t])

        (define (add-file-heading file-heading)
          (unless reading-file-headings
            (raise-user-error "File heading out of place: " file-heading)) )


        (define (maybe-close-current-article)
          (when curr-article-lines
            (when process-article
              (process-article (reverse curr-headings)
                               (reverse curr-article-lines)))
            (set! curr-article-lines #f)
            (set! curr-headings #f)))

        (define (add-heading heading)
          (set! reading-file-headings #f)
          (maybe-close-current-article)
          (unless curr-headings
            (set! curr-headings '()))
          (set! curr-headings (cons heading curr-headings)))

        (define (add-article-line a-line)
          (set! reading-file-headings #f)
          (unless curr-headings
            (error "article without heading"))
          (unless curr-article-lines
            (set! curr-article-lines '()))
          (set! curr-article-lines (cons a-line curr-article-lines)))

        (for ([line (in-lines)]
              [linenum (in-naturals)]
              #:break (and limit (> linenum limit))
              )

          (match line
            [(pregexp #px"^\uFEFF?#.*$" (list file-heading))
             (add-file-heading file-heading)]
            [(pregexp #px"^\\S+.*$" (list heading))
             (add-heading heading)]
            [(pregexp #px"^\\s+(.*)$"  (list _ bodyline))
             (add-article-line bodyline)]
            ["" #f ;(printf "Empty line at ~a\n" linenum)
                ]
            ))

        (maybe-close-current-article))
      (newline )
      )))



(define (%just-read-dsl infilename  #:encoding (encoding #f) #:limit (limit #f))
  (with-input-from-file infilename
    (lambda ()
      (when encoding
        (current-input-port (reencode-input-port (current-input-port) encoding)))

      (let ([curr-headings #f]
            [curr-article-lines #f]
            [reading-file-headings #t])


        (for ([line (in-lines)]
              [linenum (in-naturals)]
              #:break (and limit (> linenum limit))
              )

          #f))
      (newline ))))



;; (define (read-std-dsl infilename process-article)
;;   (do-read-dsl infilename process-article "UTF-16"))

(define (check-BKRS-article1 article-lines)
  (unless (and (>= (length article-lines) 1)
               (regexp-match #px"^\\s*(\\[m1\\].*)$" (car article-lines))
               (for/and ([line (in-list (cdr article-lines))])
                 (regexp-match #px"^\\s*(\\[m2\\].*)$" line)))
    (error "Bad: ~a" article-lines)))

(define (article-has-russian? article-lines)
  (define (line-has-russian? s)
    (regexp-match #rx"[А-Яа-я]" s))
  (for/or ([line (in-list article-lines)])
    (line-has-russian? line)))


(define (read-dsl (infilename IN-FILENAME)
                  #:encoding (encoding #f) #:limit (limit #f))
  (let ([n 0]
        [n-non-rus 0])
    (do-read-dsl infilename
                 (lambda (headings article-lines)
                   (set! n (add1 n))
                   ;; (check-BKRS-article article-lines)

                   (unless (article-has-russian? article-lines)
                     (set! n-non-rus (add1 n-non-rus))
                     ;(print headings) (print article-lines) (newline)
                     )

                   ;(print headings) (print article-lines) (newline)
                   )
                 #:encoding encoding
                 #:limit limit)

    (printf "Total ~a articles, ~a non-rus\n" n n-non-rus)))






(define (call-with-sqlite-connection proc #:database path #:mode (mode 'read/write))
  (let ([dbc (sqlite3-connect #:database path
                              #:mode mode)])
    (dynamic-wind
      void
      (lambda () (proc dbc))
      (lambda () (disconnect dbc)))))


(define (read-dict-to-db dbpath dictpath  #:encoding (encoding #f) #:limit (limit #f))
  (call-with-sqlite-connection
   #:database dbpath #:mode 'create
   (lambda (dbc)
     (query-exec dbc "CREATE TABLE IF NOT EXISTS articles (
                      article_id INTEGER PRIMARY KEY,
                      headings TEXT,
                      body TEXT
                      );")
     (query-exec dbc "DELETE  FROM articles;")

     (define (store-article headings article-lines)
       (let ([headings-string (string-join headings "\n")]
             [article-string (string-join article-lines "\n")])
         (query-exec dbc "INSERT INTO articles VALUES (NULL, ?, ?)"
                     headings-string
                     article-string)
         ))
     (let ([n 0]
           [n-non-rus 0])
       (do-read-dsl dictpath #:encoding encoding #:limit limit
                    (lambda (headings article-lines)
                      (set! n (add1 n))
                      ;; (check-BKRS-article article-lines)

                      (if (article-has-russian? article-lines)
                          (store-article headings article-lines)
                          (set! n-non-rus (add1 n-non-rus))

                        ;(print headings) (print article-lines) (newline)
                        )

                      ;(print headings) (print article-lines) (newline)
                      )
                    )

       (printf "Total ~a articles, ~a non-rus\n" n n-non-rus)))
   ))


(define (%just-read-db path #:limit (limit #f))
  (call-with-sqlite-connection
   #:database path  #:mode 'read/write
   (lambda (dbc)
     (let ([articlenum 0])
       (for ([(article-id headings-s article-s) (in-query dbc "SELECT * FROM articles" #:fetch 2000)]
             #:break (and limit (> articlenum limit)))

         (set! articlenum (add1 articlenum))
         ;; (printf "~a\n" articlenum)
         (let ([headings (string-split headings-s "\n")]
               [article-lines (string-split article-s "\n")])
           (when (= 0 (string-length article-s))
             (raise-user-error "Empty article " article-id)))
         ;; (printf "~a ~a ~a\n" article-id headings-s article-s)
         )))
   ))


(define (short-non-russian #:limit (limit #f))
  (let ([n 0]
        [n-non-rus 0]
        [n-short-nr 0])
    (with-output-to-file "short.txt" #:exists 'replace
      (lambda ()
        (do-read-dsl "/Users/ury/Downloads/dabkrs_200814"  #:limit limit
                     (lambda (headings article-lines)
                       (set! n (add1 n))
                       (when (= 0 (remainder n 100000))
                         (eprintf "Line ~a\n" n))
                       (unless (article-has-russian? article-lines)
                         ;; (store-article headings article-lines)
                         (set! n-non-rus (add1 n-non-rus))
                         (when (< (string-length (apply string-append-immutable article-lines))
                                  15)
                           (for ([line (in-list headings)])
                             (printf "~a\n" line))
                           (for ([line (in-list article-lines)])
                             (printf "~a\n" line))
                           (printf "\n----------\n\n")
                           ))


                       )

                     ;(print headings) (print article-lines) (newline)
                     )
        ))))

(define (do-read-db-with-dbc dbc process-article
                             #:limit (limit #f)  #:progress (progress-n 100000)
                             #:split-strings (split-strings #t))
  (let ([articlenum 0])
    (for ([(article-id headings-s article-s) (in-query dbc "SELECT * FROM articles" #:fetch 2000)]
          #:break (and limit (> articlenum limit)))

      (set! articlenum (add1 articlenum))
      (when progress-n
        (when (= 0 (remainder articlenum progress-n))
          (eprintf "Article ~a\n" articlenum)))

      ;; (printf "~a\n" articlenum)

      (let ([headings (if split-strings
                          (string-split headings-s "\n")
                          headings-s)]
            [article-lines (if split-strings
                               (string-split article-s "\n")
                               article-s)])
        (when process-article
          (process-article dbc
                           article-id
                           headings
                           article-lines)))
      )))


(define (do-read-db dbpath init-proc process-article
                    #:limit (limit #f)  #:progress (progress-n 100000)
                    #:split-strings (split-strings #t))
  (call-with-sqlite-connection
   #:database dbpath  #:mode 'read/write
   (lambda (dbc)
     (when init-proc
       (init-proc dbc))
     (do-read-db-with-dbc dbc process-article #:limit limit  #:progress progress-n
                          #:split-strings split-strings))
   ))


(define (create-headings-index dbpath #:limit (limit #f))
  (do-read-db dbpath #:limit limit
              (lambda (dbc)
                (query-exec dbc "CREATE TABLE IF NOT EXISTS headings (
                      heading_id INTEGER PRIMARY KEY,
                      heading TEXT,
                      article_id INTEGER
                      );")
                (query-exec dbc "DELETE  FROM headings;")
                (query-exec dbc "CREATE UNIQUE INDEX headings_idx ON headings (heading)")
                )

              (lambda (dbc article-id headings article-lines)
                (for ([hdng (in-list headings)])
                  (query-exec dbc "INSERT INTO headings (heading, article_id) VALUES (?, ?)"
                              hdng article-id) )                )))

(define (extract-missing-1 #:limit (limit #f) #:progress (progress-n 100000))
  (call-with-sqlite-connection
   #:database "dab.db"  #:mode 'read-only
   (lambda (dab-dbc)
     (call-with-sqlite-connection
      #:database "bkrs.db"  #:mode 'read-only
      (lambda (bkrs-dbc)
        (let ([articlenum 0]
              [missing-num 0])
          (for ([(heading dab-article-id) (in-query dab-dbc "SELECT heading,article_id FROM headings" #:fetch 2000)]
                #:break (and limit (> articlenum limit)))
            (set! articlenum (add1 articlenum))
            (when progress-n
              (when (= 0 (remainder articlenum progress-n))
                (eprintf "Article ~a\n" articlenum)))

            (let ([article-id (query-maybe-value bkrs-dbc
                                                 "SELECT article_id FROM headings WHERE heading=?"
                                                 heading)])
              (unless article-id
                (set! missing-num (add1 missing-num)))

              ))
          (printf "Articles: ~a, missing from BKRS: ~a\n" articlenum missing-num))

        )))))


(define (extract-missing #:limit (limit #f) #:progress (progress-n 20000))
  (call-with-sqlite-connection
   #:database "bkrs.db"  #:mode 'read-only
   (lambda (bkrs-dbc)
     (define bkrs-headings-table (make-hash))

     (for ([(heading bkrs-article-id)
            (in-query bkrs-dbc "SELECT heading,article_id FROM headings" #:fetch 20000)])
       (hash-set! bkrs-headings-table heading bkrs-article-id))
     (printf "BKRS headings loaded.\n")

     (let ([articlenum 0]
           [missing-num 0])

       (with-output-to-file "missing.txt" #:exists 'replace
         (lambda ()
           (do-read-db
            "dab.db" #:limit limit #:split-strings #f
            void ;; no need for init-proc
            (lambda (dbc dab-article-id headings-s article-lines-s)

              (set! articlenum (add1 articlenum))
              (when progress-n
                (when (= 0 (remainder articlenum progress-n))
                  (eprintf "Article ~a\n" articlenum)))

              (let ([headings (string-split headings-s "\n")])
                (for ([heading (in-list headings)])
                  (let ([bkrs-article-id (hash-ref bkrs-headings-table heading #f)])
                    (if bkrs-article-id
                      (query-exec dbc "UPDATE headings SET present_in_bkrs = 1 WHERE heading=?;" heading)
                      (begin
                        (set! missing-num (add1 missing-num))
                        (printf "~a\n" headings-s)
                        (printf "~a\n" article-lines-s)
                        (printf "\n----------\n\n"))
                      )

                    ))))
            )))
       (printf "Articles: ~a, missing from BKRS: ~a\n" articlenum missing-num))

     )))
