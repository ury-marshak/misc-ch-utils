;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10 -*-
;;
;                                        ;

(cl:in-package #:cl)

(defpackage #:moedict
    (:use :cl))


(eval-when (:compile-toplevel :load-toplevel :execute)
 (asdf:load-system 'quicklisp)
 (quicklisp:quickload :sqlite)
 (quicklisp:quickload :split-sequence)
 ;; (quicklisp:quickload :cl-ppcre)
 )



(in-package :moedict)
(declaim (optimize (safety 3) (debug 3) (speed 0) (space 0)))


(defvar *moedict-db* nil)  ;; (setf *moedict-db* nil)

;; Downloaded from https://github.com/kuanyui/moedict.el, uncompressed and renamed
(defparameter +moedict-path+ "~/work/CH/moedict.sqlite3")

(defun maybe-open-db ()
  (unless *moedict-db*
    (setf *moedict-db* (sqlite:connect +moedict-path+))))


(defun get-entry (headword)
  (maybe-open-db)
  (sqlite:execute-to-list *moedict-db* "select * from entries where title = ?" headword))

(defun get-stroke-count (headword)
  (let ((result (get-entry headword)))
    (assert (= (length result) 1))
    ;; stroke count is the fourth item
    (nth 3 (first result))))


(defparameter +rt-1-8-path+ "~/work/CH/RT1-8.txt")
(defparameter +rt-1-8-new-path+ "~/work/CH/RT1-8-new.txt")

(defun load-rt-1-8-data (fname-in)
  (with-open-file (infile fname-in :direction :input
                                   :external-format :utf-8)
    (let ((data (loop for line = (read-line infile nil nil)
                      ;; repeat 100
                      while line
                      for seq = (split-sequence:split-sequence #\Tab line)
                      collect seq)))
      data)))


;; Edi Weitz
(defun join (separator list)
  (with-output-to-string (out)
    (loop for (element . more) on list
          do (princ element out)
          when more
            do (princ separator out))))


(defun process-rt-1-8 ()
  (let ((data (load-rt-1-8-data +rt-1-8-path+))
        (newdata))
    (setf newdata (loop for row in data
                        ;; repeat 10
                        for trad = (first row)
                        for stroke-count = (get-stroke-count trad)
                        do (setf (nth 7 row) stroke-count)
                        collect row))
    (with-open-file (f-out +rt-1-8-new-path+
                           :external-format :utf-8
                           :direction :output
                           :if-does-not-exist :create
                           :if-exists :supersede)
      
      (loop for row in newdata
            do (format f-out "~A~%"
                       (join (string #\Tab) row))))))
