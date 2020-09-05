#lang racket/base

(require ffi/unsafe
         ffi/unsafe/define
         ffi/unsafe/alloc)

(define-ffi-definer define-expat (ffi-lib "libexpat"))

(define-expat XML_ExpatVersion (_fun -> _string))


(define _XML_Parser (_cpointer 'XMLPARSER))
(define _XML_Char_pointer (_cpointer 'XMLCHAR))


;;(define-expat XML_ParserCreate (_fun _XML_Char_pointer -> _XML_Parser) )
(define-expat XML_ParserFree (_fun _XML_Parser -> _void)
  #:wrap (deallocator))
(define-expat XML_ParserCreate (_fun _string -> _XML_Parser)
  #:wrap (allocator XML_ParserFree))


(define-expat XML_ErrorString (_fun _int -> _string/utf-8))
(define-expat XML_GetErrorCode (_fun _XML_Parser -> _int ))
(define-expat XML_GetCurrentLineNumber (_fun _XML_Parser -> _int) )
(define-expat XML_GetCurrentColumnNumber (_fun _XML_Parser -> _int) )
(define-expat XML_GetCurrentByteIndex (_fun _XML_Parser -> _int) )
;; (define-expat XML_GetInputContex (_fun _XML_Parser (_int o offset) (_int o size) -> _byte) )

(define-expat XML_GetCurrentByteCount (_fun _XML_Parser -> _int) )


(define _XML_StartElementHandler (_fun _pointer _string _pointer -> _void))
(define-expat XML_SetStartElementHandler (_fun _XML_Parser _XML_StartElementHandler -> _void))

(define _XML_EndElementHandler (_fun _pointer _string -> _void))
(define-expat XML_SetEndElementHandler (_fun _XML_Parser _XML_EndElementHandler -> _void))

(define-expat XML_SetElementHandler (_fun _XML_Parser _XML_StartElementHandler _XML_EndElementHandler -> _void))


(define _XML_ParamEntityParsing (_enum '(XML_PARAM_ENTITY_PARSING_NEVER
                                         XML_PARAM_ENTITY_PARSING_UNLESS_STANDALONE
                                         XML_PARAM_ENTITY_PARSING_ALWAYS)))

;; int XML_SetParamEntityParsing (XML_Parser p, enum XML_ParamEntityParsing code)
(define-expat XML_SetParamEntityParsing (_fun _XML_Parser _XML_ParamEntityParsing -> _int))

(define _XML_Status (_enum '(XML_STATUS_ERROR
                             XML_STATUS_OK
                             XML_STATUS_SUSPENDED)))

(define-expat XML_Parse (_fun _XML_Parser _string _int _bool -> _XML_Status))

(define _XML_CharacterDataHandler (_fun _pointer _bytes (len : _int) -> _void))
(define-expat XML_SetCharacterDataHandler (_fun _XML_Parser _XML_CharacterDataHandler -> _void))





(struct expat-parser
  (XML_Parser))

(define (make-parser
         #:start-element-handler (start-element-handler #f)
         #:end-element-handler (end-element-handler #f)
         #:character-data-handler (character-data-handler #f)
         )
  (define xml-parser (XML_ParserCreate "UTF-8"))
  (when start-element-handler
    (XML_SetStartElementHandler
     xml-parser
     (lambda (userdata name args)
       (let ((attrs (for/list ([i (in-naturals)])
                      (define s1 (ptr-ref args _string (* 2 i)))
                      #:break (not s1)
                      (define s2 (ptr-ref args _string (add1 (* 2 i))))
                      (cons s1 s2)) ))
         ;;(printf "attrs: ~a\n" attrs)
         (start-element-handler name attrs)))))

  (when end-element-handler
    (XML_SetEndElementHandler
     xml-parser
     (lambda (userdata name)
       (end-element-handler name))))

  (when character-data-handler
    (XML_SetCharacterDataHandler
     xml-parser
     (lambda (userdata raw-character-data len)
       (define char-data (cast raw-character-data _bytes (_bytes/nul-terminated o len)))
       (character-data-handler char-data))))

  (expat-parser xml-parser))


(define (get-error-string xml-parser)
  (XML_ErrorString (XML_GetErrorCode xml-parser)))


(define (do-parse-string parser xml-string is-final)
  (let* ([xml-parser (expat-parser-XML_Parser parser)]
         [status (XML_Parse xml-parser xml-string (string-length xml-string) is-final)])
    ;; XML_ErrorString(XML_GetErrorCode(p))
    (when (eq? status 'XML_STATUS_ERROR)
      (raise-user-error "Expat error: " (get-error-string xml-parser)
                        " line:" (XML_GetCurrentLineNumber xml-parser)
                        " column:" (XML_GetCurrentColumnNumber xml-parser)
                        " byte:" (XML_GetCurrentByteIndex xml-parser)))
    status
  ))


(define (parse-string parser xml-string)
  (do-parse-string parser xml-string  #t))

(define (parse-string-partial parser xml-string)
  (do-parse-string parser xml-string  #f))

(define (parse-string-final parser (xml-string ""))
  (parse-string parser xml-string))


(define (parse-port parser port)
  (for ([line (in-lines port)])
    (eprintf "line: ~a\n" line)
    (parse-string-partial parser (string-append-immutable line "\n")))
  (parse-string-final parser))


(define (parse-file parser path)
  (with-input-from-file path
    (lambda ()
      (parse-port parser (current-input-port)))))


(define +XML+ "<?xml version=\"1.0\"?>
   <book id=\"bk101\">
      <author>Gambardella, Matthew</author>
      <title>XML Developer's Guide</title>
      <genre>Computer</genre>
      <price>44.95</price>
      <publish_date>2000-10-01</publish_date>
      <description>An in-depth look at creating applications
      with XML.</description>
   </book>
")



(define (test-parse)
  (define (start-element-handler name attrs)
    (printf "start: ~a  attrs: ~a\n" name attrs))
  (define (end-element-handler name)
    (printf "end: ~a\n" name))
  (define (character-data-handler data)
    (printf "char: ~a\n" data))

  (define p (make-parser
             #:start-element-handler start-element-handler
             #:end-element-handler end-element-handler
             #:character-data-handler character-data-handler
             ))
  (parse-string p +XML+))


(define (test-parse2)
  (define (start-element-handler name attrs)
    (printf "start: ~a  attrs: ~a\n" name attrs))
  (define (end-element-handler name)
    (printf "end: ~a\n" name))
  (define (character-data-handler data)
    (printf "char: ~a\n" data))

  (define p (make-parser
             #:start-element-handler start-element-handler
             #:end-element-handler end-element-handler
             #:character-data-handler character-data-handler
             ))
  (printf "p: ~a\n" p)
  ;; (let ([res (XML_SetParamEntityParsing p 'XML_PARAM_ENTITY_PARSING_UNLESS_STANDALONE)])
  ;;   (printf "XML_SetParamEntityParsing result: ~a\n" res))

  (parse-file p "BKRS.xdxf.noent")
  )
