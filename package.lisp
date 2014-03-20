;;;; package.lisp

(defpackage #:hl7-parser
  (:use #:cl)
  (:export parse-message parse-messages))

(defpackage #:hl7-fast-parser
  (:use #:cl)
  (:export parse))
