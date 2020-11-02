#lang racket/base

(require html-parsing)
(require sxml/sxpath)
(require html-writing)


(define (read-from-port port)
  (html->xexp port))

(define (read-file infilename)
  (with-input-from-file infilename
    (lambda () (read-from-port (current-input-port)))))

(define (process-file infilename)
  (let ((doc (read-file infilename)))
    ((sxpath "//div[@class='sharedstory rtkframe']" )  ;; "//div[contains(@class, 'rtkframe')]"
     doc)))

(define (get-hanzi story-sxml)
  (let ((hanzis ((sxpath "//div[@class='mystories-kanji']/span/text()") story-sxml)))
    (unless (= (length hanzis) 1)
      (raise-user-error "Bad number of hanzis:  " hanzis))
    (car hanzis)))

(define (get-story story-sxml)
  (let ((stories ((sxpath "//div[@class='story']/node()") story-sxml)))
    ;; (unless (= (length stories) 1)
    ;;   (raise-user-error "Bad length of stories  " stories))
    ;; (car stories)
    stories
    (for/fold ((html-string ""))
              ((node stories))
      (string-append-immutable html-string
                               " "
                               (xexp->html node)))))

(define (parse-story story-sxml)
  (let* ((hanzi (get-hanzi story-sxml))
         (story (get-story story-sxml)))
    (cons hanzi story)))

(define (process-story story-sxml)
  (define parsed-story (parse-story story-sxml))

  parsed-story)


(define (test-1)
  (let* ((results (process-file "/Users/ury/work/koohii/koohii/aphasiac2_01.html"))
         (stories (for/list ((story-sxml results)) (process-story story-sxml))))
    (println stories)
    (printf "results: ~a\n" (length results))))
