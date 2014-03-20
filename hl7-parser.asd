;;;; hl7-parser.asd

(asdf:defsystem #:hl7-parser
  :serial t
  :description "Describe hl7-parser here"
  :author "Martin R. Enders"
  :license "Specify license here"
  :components ((:file "package")
               (:file "hl7-parser")
	       (:file "hl7-fast-parser")))

