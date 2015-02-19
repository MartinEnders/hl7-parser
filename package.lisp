;;;; package.lisp

(defpackage :hl7-parser
  (:use #:cl)
  (:export decode encode))


(defpackage :hl7-parser-test
  (:use #:cl #:hl7-parser)
  (:export test-parser))


