;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10 -*-
;;
;                                        ;

(cl:in-package #:cl)

(defpackage #:rth-add
    (:use :cl))


(eval-when (:compile-toplevel :load-toplevel :execute)
 (asdf:load-system 'quicklisp)
 (quicklisp:quickload :fare-csv)
 (quicklisp:quickload :split-sequence)
 (quicklisp:quickload :cl-ppcre)
 )


;;;
;;; Load moedict.lisp first
;;;

(in-package :rth-add)
(declaim (optimize (safety 3) (debug 3) (speed 0) (space 0)))


(defparameter +rth-tsv-path+ "~/work/CH/AddCangjieToRTH/Remembering Traditional Hanzi 1+2.txt")
(defparameter +rth-tsv-new-path+ "~/work/CH/AddCangjieToRTH/RTH1+2-new.tsv")

(defun load-source-tsv (fname-in)
  (with-open-file (infile fname-in :direction :input
                                   :external-format :utf-8)
    (let ((data (loop for line = (read-line infile nil nil)
                      ;; repeat 100
                      while line
                      for seq = (split-sequence:split-sequence #\Tab line)
                      collect seq)))
      data)))



(defun remove-formatting (s)
  (setf s (ppcre:regex-replace-all "^\\\"(.*)\\\"" s "\\1"))
  (ppcre:regex-replace-all "<[^>]*>" s ""))




(defvar *cj5-hash* nil)

(defparameter +cj5-path+ "~/work/CH/AddCangjieToRTH/cj5-70000.txt")

(defun load-cj5-data ()
  (with-open-file (infile +cj5-path+ :direction :input
                                     :external-format :utf-8)

    ;; Skip the header
    (loop for line = (read-line infile nil nil)
          repeat 7
          while line
          collect line)

    (let ((data (loop for line = (read-line infile nil nil)
                      for n from 1
                      ;; repeat 20
                      while line
                      for seq = (ppcre:split "\\s+" line)
                      collect seq)))

      (setf *cj5-hash* (make-hash-table :test 'equal))
      
      (loop for (cj5 char) in data
            do  ;; (break "~A ~A" cj5 (length char))
               (let ((codes (gethash char *cj5-hash*)))
                 ;; (assert (null (gethash char *cj5-hash*)))
                 (setf codes (append codes (list cj5)))
                 (setf (gethash char *cj5-hash*) codes))))))


(defun make-cj5-line (char)
  (unless *cj5-hash*
    (load-cj5-data))

  (let ((cj5-list (gethash char *cj5-hash*)))
    (if cj5-list
        (string-upcase
         (moedict::join ", " cj5-list))
        (progn
          (error "CJ5 not found: ~A" char)
          "?"))))



(defun find-cangjie5 (char)
  (if (= (length char) 1)
      (make-cj5-line char)
      (let* ((first-char-str (subseq char 0 1))
             (first-char (char first-char-str 0))
             (first-char-code (char-code first-char)))
        (if (or (member first-char '(#\( #\" #\Space ) :test 'equalp)
                (and (>= first-char-code (char-code #\a))
                     (<= first-char-code (char-code #\z)))
                (and (>= first-char-code (char-code #\A))
                     (<= first-char-code (char-code #\Z))))
            (progn
              ;; (break "----skip ~A ~A" char first-char-code)
              "")
            (progn
              ;;(break "prob ~A ~A" char first-char-code)
              (let ((cj5 (make-cj5-line first-char-str)))
                ;; (break "~A ~A" char cj5)
                (or cj5
                    "")))))))


(defun add-data-cangjie5 (fname-in fname-out)
  (let ((data (load-source-tsv fname-in))
        (newdata))
    (setf newdata (loop for row in data
                        ;; repeat 20
                        for index = (first row)
                        for cangjie5 = (find-cangjie5 index)
                        do ;;(setf row (nconc row (list index)))
                           ;;(setf (nth 1 row) "")

                           (setf row (list index cangjie5))
                        collect row))
    (with-open-file (f-out fname-out
                           :external-format :utf-8
                           :direction :output
                           :if-does-not-exist :create
                           :if-exists :supersede)
      
      (loop for row in newdata
            do (format f-out "~A~%"
                       (moedict::join (string #\Tab) row))))))



#|


  (add-data-cangjie5 +rth-tsv-path+ +rth-tsv-new-path+)

  (add-data-cangjie5 "~/work/CH/AddCangjieToRTH/RT1-8.txt" "~/work/CH/AddCangjieToRTH/RT1-8-new.tsv")

|#
