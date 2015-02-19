;;;; hl7-parser-test.lisp

(in-package #:hl7-parser-test)

(defun test-parser (in-file out-file &key (external-format :iso-8859-1) (output-delimiters "|^~\\&"))
  (with-open-file (in in-file :direction :input :external-format external-format)
    (with-open-file (out out-file :direction :output :external-format external-format :if-does-not-exist :create :if-exists :supersede)
      (loop for x = (read-line in nil nil)
	 while x
 	 do (format out "~A" (car (encode (decode x) :delimiter output-delimiters)))))))
				
  
