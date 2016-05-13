;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10 -*-
;;
;                                        ;

(cl:in-package #:cl)

(defpackage #:xdxf-pleco
    (:use :cl))


(eval-when (:compile-toplevel :load-toplevel :execute)
 (asdf:load-system 'quicklisp)
 (quicklisp:quickload :cxml)

 )


#|

 (asdf:load-system 'quicklisp)
 (quicklisp:quickload :fare-csv)
 (asdf:load-system 'fare-csv)

 |#


(in-package :xdxf-pleco)
(declaim (optimize (safety 3) (debug 3) (speed 0) (space 0)))



(defparameter +variants+ "裏爲僞臺衆箇麽麫廕鑒游板唲")


(defun is-variant (word)
  (loop for c across word
          thereis (position c +variants+)))


(defparameter +modern-variants+ '((#\裏 . #\裡)))
(defun select-trad-and-simp-headwords (headwords)
  (cond ( (= 1 (length headwords))
          (values (first headwords) nil '()))

        ( (= 2 (length headwords))
          (values (first headwords) (second headwords) '()))

        ( t
          (loop for (var-char . modern-char) in +modern-variants+
                when (position var-char (first headwords))
                  do (loop for prev = headwords then (cdr prev)
                           for curr = (cdr headwords) then (cdr curr)
                           when (null curr)
                             do (error "Problem reordering ~A" headwords)
                           when (position modern-char (car curr))
                             do (let ((new-head (car curr)))
                                  (setf (cdr prev) (cdr curr))
                                  (setf headwords (cons new-head headwords))
                                  (return)))
                     (return))
          (values (first headwords) (second headwords) (cddr headwords)))))


(defparameter *home* "/Users/ury")
(defparameter *input-file-name* (concatenate 'string *home* "/work/CH/BKRS_17_03_08/BKRS.xdxf"))
(defparameter *dtd-file-name* (concatenate 'string *home* "/work/CH/BKRS_17_03_08/xdxf_strict.dtd"))

(defparameter *output-file-name* (concatenate 'string *home* "/work/CH/BKRS_17_03_08/BKRS-pleco.txt"))

(defun read-xdxf-to-dom (fname-in fname-out)
  (declare (ignorable fname-out))
  (let ((uri "https://raw.github.com/soshial/xdxf_makedict/master/format_standard/xdxf_strict.dtd")
        (pathname *dtd-file-name*))
    (flet ((resolver (pubid sysid)
             (declare (ignore pubid))
             (when (puri:uri= sysid (puri:parse-uri uri))
               (open pathname :element-type '(unsigned-byte 8)))))

      (let* ((dom-doc (cxml:parse-file (pathname fname-in)
                                       (cxml-dom:make-dom-builder)
                                       :entity-resolver #'resolver))
         (root (dom:document-element dom-doc)))

    root
    )))
  )


(defun replace-all (string part replacement &key (test #'char=))
"Returns a new string in which all the occurences of the part 
is replaced with replacement."
    (with-output-to-string (out)
      (loop with part-length = (length part)
            for old-pos = 0 then (+ pos part-length)
            for pos = (search part string
                              :start2 old-pos
                              :test test)
            do (write-string string out
                             :start old-pos
                             :end (or pos (length string)))
            when pos do (write-string replacement out)
              while pos)))




(defparameter +indent-step+ 2)
(defparameter +indent-string+ "  ")

(defparameter +pleco-newline+ #. (string (code-char #xEAB1)))
(defparameter +pleco-bold+ #. (string (code-char #xEAB2)))
(defparameter +pleco-/bold+ #. (string (code-char #xEAB3)))
(defparameter +pleco-italic+ #. (string (code-char #xEAB4)))
(defparameter +pleco-/italic+ #. (string (code-char #xEAB5)))
(defparameter +pleco-link+ #. (string (code-char #xEAB8)))
(defparameter +pleco-/link+ #. (string (code-char #xEABB)))
(defparameter +pleco-blue+ #. (coerce (list (code-char #xEAC1)
                                            (code-char #x8000)(code-char #x8000)
                                            (code-char #x8000)
                                            (code-char #x80FF))
                                      'string))

(defparameter +pleco-red+ #. (coerce (list (code-char #xEAC1)
                                           (code-char #x8000)
                                           (code-char #x80FF)
                                           (code-char #x8000)
                                           (code-char #x8000))
                                     'string))
(defparameter +pleco-gray+ #. (coerce (list (code-char #xEAC1)
                                            (code-char #x8000)
                                            (code-char #x8077)
                                           (code-char #x8077)
                                           (code-char #x8077))
                                     'string))
(defparameter +pleco-/color+ #. (string (code-char #xEAC2)))




(defun process-ar (source outstream)
  ;; here we are positioned on the ar tag
  ;;(klacks:consume source)

  (let ((headwords)
        (indent-level 0)
        (at-bol t)
        (article-pieces)
        (article))

    (loop for evt = (klacks:peek-next source)
          with level = 1
          while (> level 0)
          do ;;(print evt)
             (flet ((new-line ()
                      (push +pleco-newline+  article-pieces)
                      (setf at-bol t))

                    (replace-newlines (s)
                      (replace-all s (string #\Newline) +pleco-newline+))
                    
                    (start-el-p (name)
                      (and (eq evt :start-element)
                           (string= (klacks:current-lname source) name)))
                    (end-el-p (name)
                      (and (eq evt :end-element)
                           (string= (klacks:current-lname source) name))))

               (when (eq evt :start-element)
                 (incf level))
               (when (eq evt :end-element)
                 (decf level))

               (cond ((start-el-p "k")

                      (let ((kw))
                        (setf evt (klacks:peek-next source))
                        (klacks:expect source :characters)
                        (setf kw (klacks:current-characters source))
                        ;; (format t "k: ~A" kw)
                        (push kw headwords)
                        ))

                     ((end-el-p "k")

                      )


                     ((start-el-p "blockquote")

                      (incf indent-level)
                      (push +indent-string+ article-pieces)
                      ;; (unless at-bol
                      ;;   (new-line))
                      )

                     ((end-el-p "blockquote")

                      (decf indent-level)
                      ;; (unless at-bol
                      ;;   (new-line))
                      )

                     ;;
                     ((start-el-p "c")
                      
                      (let ((red nil))
                        (klacks:map-attributes #'(lambda (ns loc qual value specified)
                                                   (declare (ignore ns qual specified))
                                                   (assert (string= loc "c"))
                                                   (assert (string= value "red"))
                                                   (setf red t))
                                               source)
                        (if red
                            (push +pleco-red+ article-pieces)
                            (push +pleco-blue+ article-pieces))))

                     ((end-el-p "c")                      
                      (push +pleco-/color+ article-pieces))
                     ;;

                     ((start-el-p "i")                      
                      (push +pleco-italic+ article-pieces))

                     ((end-el-p "i")
                      (push +pleco-/italic+ article-pieces))

                     ;;
                     ((start-el-p "b")
                      (push +pleco-bold+ article-pieces))

                     ((end-el-p "b")
                      (push +pleco-/bold+ article-pieces))

                     ;;
                     ((start-el-p "ex")
                      (push +pleco-gray+ article-pieces))

                     ((end-el-p "ex")
                      (push +pleco-/color+ article-pieces))

                     ;;
                     ((eq evt :characters)
                      (let ((s (klacks:current-characters source)))
                        ;; (when at-bol
                        ;;   (loop repeat)
                        ;;   (setf at-bol nil))
                        (push (replace-newlines s) article-pieces)
                        ))

                     ;;
                     ((start-el-p "kref")
                      (push +pleco-link+ article-pieces))

                     ((end-el-p "kref")
                      (push +pleco-/link+ article-pieces))

                     ;;
                     ((end-el-p "ar")

                      )

                     ;;
                     (t (error "unknown ~A" evt))
                     
                     ))
             
             
          )
    (setf article
          (apply #'concatenate 'string
                 (nreverse article-pieces)))

    (let ((pinyin ""))
      (multiple-value-bind (trad simp variants)
          (select-trad-and-simp-headwords headwords)
        (let ((heading (if simp
                           (format nil "~A[~A]" simp trad)
                           trad)))

          (format outstream "~A~c~A~c~A~%" heading #\Tab pinyin #\Tab article)
          (let ((ref-article (concatenate 'string
                                      "Variant of "
                                      +pleco-link+
                                      trad
                                      +pleco-/link+)))
            (loop for var in variants
                  do (format outstream "~A~c~A~c~A~%" var #\Tab pinyin #\Tab ref-article)))
          )))


    ;; (loop for hw in headwords
    ;;       when (string= hw "為")
    ;;         do (break))
    ;; (print heading)    (print article)


    )
  
  )


;; (defparameter +convert-from-n+ 102930)
;; (defparameter +convert-to-n+ 102950)

(defparameter +convert-from-n+ nil)
(defparameter +convert-to-n+ nil)


(defun convert-xdxf (fname-in fname-out)
  (let ((uri "https://raw.github.com/soshial/xdxf_makedict/master/format_standard/xdxf_strict.dtd")
        (pathname *dtd-file-name*))
    (flet ((resolver (pubid sysid)
             (declare (ignore pubid))
             (when (puri:uri= sysid (puri:parse-uri uri))
               (open pathname :element-type '(unsigned-byte 8)))))

      (let ((articles 0))
        (with-open-file (f-out fname-out
                               :external-format :utf-8
                               :direction :output
                               :if-does-not-exist :create
                               :if-exists :supersede)

          (klacks:with-open-source (source (cxml:make-source (pathname fname-in)
                                                             :entity-resolver #'resolver))

          
            (loop for ar-el = (klacks:find-element source "ar")
                  for n from 0
                  while ar-el
                  while (or (not +convert-to-n+)
                            (< n +convert-to-n+))
                  do (if (or (not +convert-from-n+)
                             (>= n +convert-from-n+))
                         (progn ;;(print n)
                           (process-ar source f-out)
                           (incf articles))
                         (progn
                           (klacks:consume source)))
                  )))
        (format t "~%Articles: ~A~%" articles))))
  )

#|
  (convert-xdxf *input-file-name* *output-file-name*)
|#
