;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10 -*-
;;
;                                        ;

(cl:in-package #:cl)

(defpackage #:tocfl-add
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

(in-package :tocfl-add)
(declaim (optimize (safety 3) (debug 3) (speed 0) (space 0)))


(defparameter +tocfl-tsv-path+ "/home/ury/work/CH/TOCFL-vocabulary/TOCFL.tsv")
(defparameter +tocfl-tsv-new-path+ "/home/ury/work/CH/TOCFL-vocabulary/TOCFL-new.tsv")

(defun load-tocfl-tsv (fname-in)
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




(defvar *rth-hash* nil)

(defparameter +rth-path+ "/home/ury/work/CH/RTHplusGrades1-8.tsv")

(defun load-rth-data ()
  (with-open-file (infile +rth-path+ :direction :input
                                     :external-format :utf-8)
    (let ((data (loop for line = (read-line infile nil nil)
                      ;; repeat 100
                      while line
                      for seq = (split-sequence:split-sequence #\Tab line)
                      collect seq)))

      (setf *rth-hash* (make-hash-table :test 'equalp))
      
      (loop for (trad keyword PoS . rest) in data
            do (assert (null (gethash trad *rth-hash*)))
               (setf (gethash trad *rth-hash*) (list :keyword keyword
                                                     :PoS (remove-formatting PoS)))))))


(defun get-rth-name (trad)
  (unless *rth-hash*
    (load-rth-data))

  (let ((rth (gethash trad *rth-hash*)))
    (if rth
        (let ((result (getf rth :keyword))
              (PoS (getf rth :PoS)))
          (when (plusp (length PoS))
            (setf result
                  (concatenate 'string result "[" PoS "]")))
          result)
        "?")))


(defun make-rth-line (expr)
  (let ((line-list)
        (total-strokes 0))
    (loop for char across expr
          for trad = (string char)
          for stroke-count = (moedict::get-stroke-count trad)
          for rth-name = (get-rth-name trad)
          do (when line-list
               (push " + " line-list))
             (push (format nil "~A(~A)" rth-name stroke-count) line-list)
             (incf total-strokes stroke-count)
             
          )
    ;; (when (> (length expr) 1)
    ;;   )
    (push (format nil " = ~A" total-strokes) line-list)
    (setf line-list (nreverse line-list))

    (cons total-strokes
          (moedict::join "" line-list))))

(defun add-data-tocfl1 ()
  (let ((data (load-tocfl-tsv +tocfl-tsv-path+))
        (newdata))
    (setf newdata (loop for row in data
                        ;; repeat 20
                        for trad = (second row)
                        for (total-strokes . rth-line) = (make-rth-line trad)
                        do (setf row (nconc row (list total-strokes rth-line)))
                           (setf (nth 5 row) (remove-formatting (nth 5 row)))
                        collect row))
    (with-open-file (f-out +tocfl-tsv-new-path+
                           :external-format :utf-8
                           :direction :output
                           :if-does-not-exist :create
                           :if-exists :supersede)
      
      (loop for row in newdata
            do (format f-out "~A~%"
                       (moedict::join (string #\Tab) row))))))


(defun add-data-tocfl ()
  (let ((data (load-tocfl-tsv +tocfl-tsv-path+))
        (newdata))
    (setf newdata (loop for row in data
                        ;; repeat 20
                        for index = (first row)
                        do ;;(setf row (nconc row (list index)))
                           ;;(setf (nth 1 row) "")
                           (setf row (list index index))
                        collect row))
    (with-open-file (f-out +tocfl-tsv-new-path+
                           :external-format :utf-8
                           :direction :output
                           :if-does-not-exist :create
                           :if-exists :supersede)
      
      (loop for row in newdata
            do (format f-out "~A~%"
                       (moedict::join (string #\Tab) row))))))
