#lang racket/base


(provide (prefix-out pleco:
                     (combine-out
                      bold
                      newline
                      italic
                      link
                      from-html))
         strip-HTML)

(define +pleco-newline+   "\uEAB1")
(define +pleco-bold+      "\uEAB2")
(define +pleco-/bold+     "\uEAB3")
(define +pleco-italic+    "\uEAB4")
(define +pleco-/italic+   "\uEAB5")
(define +pleco-link+      "\uEAB8")
(define +pleco-/link+     "\uEABB")


(define (bold s)
  (string-append-immutable +pleco-bold+ s +pleco-/bold+))

(define (newline)
  (string-append-immutable +pleco-newline+))

(define (italic s)
  (string-append-immutable +pleco-italic+ s +pleco-/italic+))

(define (link s)
  (string-append-immutable +pleco-link+ s +pleco-/link+))


(define (strip-HTML s)
  (regexp-replace* #rx"<.*?>" s ""))

(define (from-html s)
  (set! s (regexp-replace* #rx"<i>" s +pleco-italic+))
  (set! s (regexp-replace* #rx"</i>" s +pleco-/italic+))
  (set! s (regexp-replace* #rx"<b>" s +pleco-bold+))
  (set! s (regexp-replace* #rx"</b>" s +pleco-/bold+))

  (set! s (regexp-replace* #rx"&quot;" s "\""))
  (set! s (regexp-replace* #rx"&nbsp;" s " "))

  ;; remove all other tags:
  (strip-HTML s))
