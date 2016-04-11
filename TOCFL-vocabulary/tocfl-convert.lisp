;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10 -*-
;;
;                                        ;

(cl:in-package #:cl)

(defpackage #:tocfl
    (:use :cl))


(eval-when (:compile-toplevel :load-toplevel :execute)
 (asdf:load-system 'quicklisp)
 (quicklisp:quickload :fare-csv)
 (quicklisp:quickload :split-sequence)
 (quicklisp:quickload :cl-ppcre)
 ;;(quicklisp:quickload :cl-ppcre-unicode)
 )


#|

 (asdf:load-system 'quicklisp)
 (quicklisp:quickload :fare-csv)
 (asdf:load-system 'fare-csv)

 |#

;;;
;;; Load moedict.lisp and tocfl-add-info.lisp first
;;;


(in-package :tocfl)
(declaim (optimize (safety 3) (debug 3) (speed 0) (space 0)))

(defun load-data (fname-in fields)
  (loop for row in (fare-csv:read-csv-file fname-in
                                           :external-format :utf-8)
        for first-line = t then nil
        unless first-line
          collect (loop for v in row
                        for k in fields
                        collect k
                        collect v)))

(defun in-bopomofo-ranges (c)
  (let ((cc (char-code c)))
    (or (and (>= cc #x00003100) (<= cc #x0000312F))
        (and (>= cc #x000031A0) (<= cc #x000031BF))
        (char= c #\MODIFIER_LETTER_MACRON)
        (char= c #\MODIFIER_LETTER_ACUTE_ACCENT)
        (char= c #\CARON)
        (char= c #\MODIFIER_LETTER_GRAVE_ACCENT)
        (char= c #\DOT_ABOVE))))


(defun bopomofo-only (word)
  (loop for c across word
        unless (in-bopomofo-ranges c)
          do (return-from bopomofo-only nil)
        )
  t)



(defvar *current-category* "")

(defparameter +fields-with-category+
  '(:category :word :pinyin :pos
    ))

(defparameter +fields-without-category+
  '(:word :pinyin :pos
    ))

(defun expand-parentheses (word)
  (flet ((split-parentheses (s)
               (split-sequence:split-sequence-if
                #'(lambda (c)
                    (member c '( #\( #\) )))
                s
                :remove-empty-subseqs t)))

    (let ((words (split-parentheses word )))

      (assert (<= (length words) 3 )))

    (let ((result))
      (push word result)
      (cl-ppcre:register-groups-bind (a b c)
          ("^(.*)\\((.*)\\)(.*)$" word)
        (setf result '())
        (if (bopomofo-only b)
            (push (concatenate 'string a c) result)
            (progn
              (push (concatenate 'string a c) result)
              (push (concatenate 'string a b c) result))))
      (nreverse result))
    ))

(defvar *running-number* 1)

(defvar *max-variants* 0)
;; (defvar *num-large-variants* 0)

(defun expand-row-separate-lines (row)
  "Return a list of output lines"

  (let ((output))
    (destructuring-bind  (&key (category "") word pinyin PoS &allow-other-keys)
        row
      (if (and category
               (plusp (length category)))
          (setf *current-category* category)
          (setf category *current-category*))

      (flet ((split/ (s)
               (split-sequence:split-sequence-if
                #'(lambda (c)
                    (member c '(#\/  )))
                s
                :remove-empty-subseqs t)))
        (let ((words (split/ word ))
              (pinyins (split/ pinyin )))

          (setf words (loop for str in words
                            append (expand-parentheses str)))
          (setf pinyins (loop for str in pinyins
                              append (expand-parentheses str)))

          (when (and (= (length words) 2) (= (length pinyins) 1))
            (setf pinyins (append pinyins pinyins)))

          (assert (= (length words) (length pinyins)))
          (setf *max-variants* (max *max-variants* (length words)))
          ;; (when (> (length words) 2)
          ;;   (incf *num-large-variants*))

          (let ((variants-list))
            (setf variants-list (loop for w in words
                                      for p in pinyins
                                      for first = t then nil
                                      unless first
                                        collect (cons w p)))

            (push (list category
                      (first words)
                      (first pinyins)
                      PoS
                      (format nil "TW~4,'0d" *running-number*)
                      variants-list)
                  output)))))
    (incf *running-number*)
    (nreverse output)))

(defun join-variants-with-pinyin (variants)
  (let ((variants-list (loop for (w . p) in variants
                             collect (format nil "~A [~A]" w p))))
    (moedict::join ", " variants-list)))

(defun join-variants-without-pinyin (variants)
  (let ((variants-list (loop for (w . p) in variants
                             collect w)))
    (moedict::join " " variants-list)))

(defvar *output-category* "")

(defun process-row-pleco (row)
  (let ((output))
    (destructuring-bind  (category word pinyin PoS index variants)
        row
      (declare (ignore PoS index variants))
      (when (and category
                 (string/= category *output-category*))
        (setf *output-category* category)
        (push (concatenate 'string "// " category) output ))

      (let ((line (format nil "~A~c~A" word #\Tab pinyin)))
        (push line output))

      )
    (nreverse output)))

(defparameter *import-file-name* "~/work/CH/TOCFL-vocabulary/vocabulary-L1.csv")
(defparameter *output-file-name* "~/work/CH/TOCFL-vocabulary/L1-tabbed.txt")


(defun convert-files (fname-in fname-out &key has-categories
                                              (expand-row #'expand-row-separate-lines)
                                              (process-row #'process-row-pleco))
  (let* ((*current-category* "")
         (*output-category* "")
         (data (load-data fname-in (if has-categories +fields-with-category+ +fields-without-category+)))
         res
         (debug nil))

    (format t "~%Source rows: ~A" (length data))

    (setf data (loop for row in data
                     for n from 0
                     with new-data = nil
                     for process = (or (not debug)
                                       (and (>= n 0) (<= n 100)))
                     when process
                       do (setf new-data (funcall expand-row row))
                     when process
                       append new-data))

    (format t "~%Expanded rows: ~A" (length data))

    ;; (print data)
    (setf res (loop for row in data
                    for result = (funcall process-row row)
                    for n from 0
                    when result
                      append result))

    (format t "~%Rows: ~A" (length res))
    ;;(print res)
    (with-open-file (f-out fname-out
                           :external-format :utf-8
                           :direction :output
                           :if-does-not-exist :create
                           :if-exists :supersede)
      (loop for line in res
            do (format f-out "~A~%" line)))
    ))

(defun convert-L1 ()
  (convert-files "~/work/CH/TOCFL-vocabulary/vocabulary-L1.csv"
                 "~/work/CH/TOCFL-vocabulary/L1-tabbed.txt" :has-categories t))

(defun convert-L2 ()
  (convert-files "~/work/CH/TOCFL-vocabulary/vocabulary-L2.csv"
                 "~/work/CH/TOCFL-vocabulary/L2-tabbed.txt" :has-categories t))


(defun cedict-to-line (word)
  (let* ((entries (cc-cedict word))
         (res nil)
         (more-than-one (> (length entries) 1)))
    (loop for entry in entries
          do (when res
               (push " ~~~ " res))
             (when more-than-one
               (push (format nil "[~A]" (getf entry :pinyin)) res))
             (push (getf entry :translation) res))
    (setf res (nreverse res))
    (apply #'concatenate 'string res)))

(defun simp-or-empty (word)
  (let* ((entries (cc-cedict word))
         ;; (more-than-one (> (length entries) 1))
         (res nil))
    (loop for entry in entries
          do (let ((newstr (getf entry :simp)))
               (unless (member newstr res :test #'string=)
                 (when res
                   (push ", " res))
                 (push newstr res))))
    (setf res (nreverse res))
    (when (and (= (length res) 1)
               (string= (first res) word))
      (setf res nil))
    (apply #'concatenate 'string res)))


(defparameter *skip-complex-fields* nil)

(defun process-row-tsv (row)
  (let ((output))
    (destructuring-bind  (category word pinyin PoS index variants)
        row

      (let* ((hint "")
             (keyword "")
             (comments "")
             (produce "")
             (variants-without-pinyin (join-variants-without-pinyin variants))
             (variants-with-pinyin (join-variants-with-pinyin variants))
             (strokes-and-rth-line (if *skip-complex-fields*
                                       (cons "" "")
                                       (tocfl-add::make-rth-line word)))
             (strokes (car strokes-and-rth-line))
             (rth-line (cdr strokes-and-rth-line))
             (cedict-line (if *skip-complex-fields*
                              ""
                              (cedict-to-line word)))
             (out-row (list index word pinyin variants-without-pinyin
                            PoS hint keyword comments strokes
                            rth-line cedict-line category
                            variants-with-pinyin
                            (simp-or-empty word) produce
                            ))
             (line (moedict::join (string #\Tab) out-row)))
        (push line output))

      )
    (nreverse output)))


;; (defparameter +L2-starting-number+ 538)
;; (defparameter +L3-starting-number+ 1050)
(defparameter +L2-new-number+ 501)
(defparameter +L3-new-number+ 999)
(defparameter +L4-new-number+ 2495)
(defparameter +L5-new-number+ 4990)

(defun convert-L1-tsv ()
  (let ((*running-number* 1))
    (convert-files "~/work/CH/TOCFL-vocabulary/vocabulary-L1.csv"
                   "~/work/CH/TOCFL-vocabulary/L1-vocabulary.tsv"
                   :has-categories t
                   :process-row #'process-row-tsv)))

(defun convert-L2-tsv ()
  (let ((*running-number* +L2-new-number+))
    (convert-files "~/work/CH/TOCFL-vocabulary/vocabulary-L2.csv"
                   "~/work/CH/TOCFL-vocabulary/L2-vocabulary.tsv"
                   :has-categories t
                   :process-row #'process-row-tsv)))


(defun convert-L3-tsv ()
  (let ((*running-number* +L3-new-number+))
    (convert-files "~/work/CH/TOCFL-vocabulary/vocabulary-L3.csv"
                   "~/work/CH/TOCFL-vocabulary/L3-vocabulary.tsv"
                   :has-categories nil
                   :process-row #'process-row-tsv)))
(defun convert-L4-tsv ()
  (let ((*running-number* +L4-new-number+))
    (convert-files "~/work/CH/TOCFL-vocabulary/vocabulary-L4.csv"
                   "~/work/CH/TOCFL-vocabulary/L4-vocabulary.tsv"
                   :has-categories nil
                   :process-row #'process-row-tsv)))
(defun convert-L5-tsv ()
  (let ((*running-number* +L5-new-number+))
    (convert-files "~/work/CH/TOCFL-vocabulary/vocabulary-L5.csv"
                   "~/work/CH/TOCFL-vocabulary/L5-vocabulary.tsv"
                   :has-categories nil
                   :process-row #'process-row-tsv)))

;; (res (loop for row in (fare-csv:read-csv-file *import-file-name*
;;                                                        :external-format :utf-8)
;;                     for first-line = t then nil
;;                     for result  = (unless first-line
;;                                     (process-row row))
;;                     do (incf n-rows)
;;                     when result
;;                       collect result ))



(defun load-cc-cedict (filename)
  (let ((wordhash (make-hash-table :test 'equalp)))
    (with-open-file (f filename
                       :direction :input
                       :external-format :utf-8)
      (loop for line = (read-line f nil nil)
            while line
            do (cl-ppcre:register-groups-bind (trad simp pinyin translation)
                   ("^(\\w+)\\s+(\\w+)\\s+\\[([^]]*)\\]\\s+([^\\r]*)\\r?$" line )
                 ;;(break "~A ~A p:~A tr:~A" trad simp pinyin translation)
                 ;; (when (gethash trad wordhash)
                 ;;   (error "duplicate: ~A" trad))
                 (push (list :trad trad :simp simp
                             :pinyin pinyin
                             :translation translation)
                       (gethash trad wordhash) )
                 )))
    wordhash))

(defvar *cc-cedict* nil)
(defparameter +cedict-path+ "~/work/CH/cedict_1_0_ts_utf-8_mdbg.txt")

(defun cc-cedict (word)
  (unless *cc-cedict*
    (setf *cc-cedict* (load-cc-cedict +cedict-path+)))
  (gethash word *cc-cedict*))


(defun expand-row-original-lines (row)
  (destructuring-bind  (&key (category "") word pinyin PoS &allow-other-keys)
      row

    (let ((index (format nil "TW~4,'0d" *running-number*))
          (variants '()))
      (prog1
          (list (list category word pinyin PoS index variants))
              (incf *running-number*)))))


(defun match-bopomofo-filter (pos)
  (let ((n (loop for p from pos below (length cl-ppcre::*string*)
               while (in-bopomofo-ranges (char cl-ppcre::*string* p))
                 count t)))
    (when (plusp n)
      (+ pos n))))

(defun remove-bopomofo-in-parentheses (s)
  (ppcre:regex-replace-all '(:sequence #\(
                             (:greedy-repetition 1 nil (:filter match-bopomofo-filter))
                             #\))
                           s
                           ""
                           ))


(defvar *latex-row* 0)

(defun process-row-latex (row)
  (let ((output)
        (line)
        (w))
    (destructuring-bind  (category word pinyin pos index variants)
        row

      (declare (ignore category variants))

      (setf w (remove-bopomofo-in-parentheses word))
      (setf line (format nil "\\outputword{~A}{~A}{~A}{~A}" index w pos pinyin)))


    (push line output)
    (incf *latex-row*)
    (let ((cyclic (mod *latex-row* 21)))
      (when (or (= cyclic 7)
                (= cyclic 14))
        (push "\\skipwordspace" output))
      (when (= cyclic 0)
        (push "\\nextwordpage" output)))

    (nreverse output)))

(defun convert-L1-latex ()
  (let ((*running-number* 1)
        (*latex-row* 0))
    (convert-files "~/work/CH/TOCFL-vocabulary/vocabulary-L1.csv"
                   "~/work/CH/TOCFL-vocabulary/L1-vocabulary.tex"
                   :has-categories t
                   :expand-row #'expand-row-original-lines
                   :process-row #'process-row-latex)))

(defun convert-L2-latex ()
  (let ((*running-number* +L2-new-number+)
        (*latex-row* 0))
    (convert-files "~/work/CH/TOCFL-vocabulary/vocabulary-L2.csv"
                   "~/work/CH/TOCFL-vocabulary/L2-vocabulary.tex"
                   :has-categories t
                   :expand-row #'expand-row-original-lines
                   :process-row #'process-row-latex)))

(defun convert-L3-latex ()
  (let ((*running-number* +L3-new-number+)
        (*latex-row* 0))
    (convert-files "~/work/CH/TOCFL-vocabulary/vocabulary-L3.csv"
                   "~/work/CH/TOCFL-vocabulary/L3-vocabulary.tex"
                   :has-categories nil
                   :expand-row #'expand-row-original-lines
                   :process-row #'process-row-latex)))
