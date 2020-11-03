#lang racket/base

(require html-parsing)
(require sxml/sxpath)
(require html-writing)
(require racket/path)



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



(define (get-html-files dirpath)
  (filter (lambda (fp)
            (define ext (path-get-extension fp))
            (and ext (bytes=? ext #".html")))
          (directory-list dirpath #:build? #t)))

(define (process-dir dirpath)
  (define html-files (get-html-files dirpath))
  (for ((html-file html-files))
    (process-file html-file)))


(define (test-2)
  (process-dir "/Users/ury/work/misc-ch-utils/koohii/koohii/"))
