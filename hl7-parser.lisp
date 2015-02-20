;;;; hl7-parser.lisp

(in-package #:hl7-parser)

;;; "hl7-parser" goes here. Hacks and glory await!

(defun decode (string-or-stream)
  "Returns List of HL7-Messages from String"
  (if (stringp string-or-stream)
      (with-input-from-string (s string-or-stream)
	(mapcar #'parse-2 (lexer s)))
      (mapcar #'parse-2 (lexer string-or-stream))))

(defun encode (message-list &key (delimiter "|^~\\&") (message-delimiter nil))
  "Requires list of Messages, Returns list of HL7-Messages in Strings"
  (hl7-writer message-list :delimiter-string delimiter :message-delimiter message-delimiter))
      



;;  _                       
;; | |    _____  _____ _ __ 
;; | |   / _ \ \/ / _ \ '__|
;; | |__|  __/>  <  __/ |   
;; |_____\___/_/\_\___|_|   
                         
(defun lexer (stream)
  (loop while (peek-char nil stream nil nil)
     collect
       (let* ((msh            (get-string-from-stream stream 3))
	      (delimiter-raw  (get-string-from-stream stream 5))
	      (delimiter      `( ,(cons (char delimiter-raw 0) :field)
				  ,(cons (char delimiter-raw 1) :component)
				  ,(cons (char delimiter-raw 2) :repetition)
				  ,(cons (char delimiter-raw 3) :escape) ;; evtl. einfach ignorieren
				  ,(cons (char delimiter-raw 4) :subcomponent)
				  ,(cons #\Return               :segment)
				  ,(cons #\Newline              :message))))
	 
	 
	 (append (list msh :field delimiter-raw)
		 (loop for last = nil then x
		    for x = (get-next-token stream delimiter)
		    while x
		    when (and last (symbolp last) (symbolp x)) collect (if (and (eq last :segment) (eq x :message)) nil "") into result
		    collect x into result
		    until (eq x :message)
		    finally (if (eq x :message)
				(return result)
				(return (append result '(nil :message)))))))))
	    


(defun get-next-token (stream delimiter)
  (or (string-to-next-delimiter stream delimiter)
      (get-next-delimiter stream delimiter)))


(defun get-next-delimiter (stream delimiter)
  (let ((input-char (peek-char nil stream nil nil)))
    (if (delimiter-p input-char delimiter)
	(progn
	  (read-char stream nil nil)
	  (delimiter-to-keyword input-char delimiter))
	nil)))

(defun string-to-next-delimiter (stream delimiter)
  (let ((output (with-output-to-string(s)
		  (loop for x = (peek-char nil stream nil nil) while (and x (or (escape-delimiter-p x delimiter) (not (delimiter-p x delimiter))))
		     do (progn
			  (when (escape-delimiter-p x delimiter)
			    (princ (read-char stream) s))
			  (princ (read-char stream) s))))))
    (if (and output (equal output ""))
	nil
	output)))


(defun delimiter-p (char delimiter)
  (if char
      (assoc char delimiter :test #'char=)
      nil))

(defun escape-delimiter-p (char delimiter)
  (if char
      (eq (cdr (assoc char delimiter :test #'char=)) :escape)
      nil))

(defun delimiter-to-keyword (char delimiter)
  (cdr (assoc char delimiter :test #'char=)))
     
(defun get-string-from-stream (stream number-of-characters)
  (let* ((result (make-string number-of-characters))
	 (number-of-read-characters (read-sequence result stream :start 0 :end number-of-characters)))
    (if (= number-of-read-characters number-of-characters)
	result
	(error "Stream not long enough. String: ~A Number of Requested Characters: ~A~%" result number-of-characters))))



;;  ____                          
;; |  _ \ __ _ _ __ ___  ___ _ __ 
;; | |_) / _` | '__/ __|/ _ \ '__|
;; |  __/ (_| | |  \__ \  __/ |   
;; |_|   \__,_|_|  |___/\___|_|   
                               


(defun get-precedence (token)
  (let ((tokens '(:escape :subcomponent :component :repetition :field :segment :message)))
    (position token tokens :test #'eq)))

(defun operand>= (a b)
  (let ((a-p (get-precedence a))
	(b-p (get-precedence b)))
    (and a b (>= a-p b-p))))

(defun operand> (a b)
  (let ((a-p (get-precedence a))
	(b-p (get-precedence b)))
    (and a b (> a-p b-p))))

(defun operand= (a b)
  (let ((a-p (get-precedence a))
	(b-p (get-precedence b)))
    (and a b (= a-p b-p))))


(defun logging (&rest rest)
  (declare (ignore rest))
  (values))
;  (apply #'format rest))


(defun parse-2 (tokens)
  (let ((operand-stack nil)
	(operator-stack nil))
    (dolist (x tokens)
      (logging t "Operand-stack:~%~S~%Operator-stack~%~S~%Token~%~S~%----~%" operand-stack operator-stack x)
      
      (cond ((or (stringp x) (numberp x) (null x))
	     (push x operand-stack))
	    ((symbolp x)
	     (loop while (operand>= x (car operator-stack) )
		  do
		    (let ((a  (pop operand-stack))
			  (b  (pop operand-stack))
			  (op (pop operator-stack)))
		      (logging t "    A:~S,~%    B:~S,~%    OP:~S~%" a b op)

		      (cond ((and (listp b) (eq (car b) op))
			     (logging t "1.1~%")
			     (push (append (list op)  (cdr b) (if a (list a) a) ) operand-stack)) ;;if a=nil don't append
			    ((and (listp a) (eq (car a) op))
			     (logging t "1.2~%")
			     (push (append (list op) (if b (list b) b) (cdr a) ) operand-stack))
			    (t
			     (logging t "1.3~%")
			     (push (append (list op) (if b (list b) b) (if a (list a) a)) operand-stack)))))

		     
	     (push x operator-stack))))

    (logging t " ------ Aufraeumen ---------- ~%")
    (logging t "~S~%~S~%----~%" operand-stack operator-stack)
    
    (dolist (x operator-stack)
      (logging t "Operand-Stack:~%~S~%Operator-Stack:~%~S~%----~%" operand-stack operator-stack)
      (let ((a  (pop operand-stack))
	    (b  (pop operand-stack))
	    (op x))
	(logging t "    A: ~S,~%    B: ~S,~%    OP: ~S~%" a b op)
	(cond ((and (listp b) (eq (car b) op))
	       (logging t "1~%")
	       (push (append (list op) (cdr b) (if a (list a) a) ) operand-stack))
	      ((and (listp a) (eq (car a) op))
	       (logging t "2~%")
	       (push (append (list op) (if b (list b) b) (cdr a) ) operand-stack))
	      (t
	       (logging t "3~%")
	       (push (append (list op) (if b (list b) b) (if a (list a) a)) operand-stack)))))
    
    (car operand-stack)))

		    

  
;; __        __    _ _            
;; \ \      / / __(_) |_ ___ _ __ 
;;  \ \ /\ / / '__| | __/ _ \ '__|
;;   \ V  V /| |  | | ||  __/ |   
;;    \_/\_/ |_|  |_|\__\___|_|   
                               
(defun hl7-writer (hl7-messages &key (delimiter-string "|^~\\&") (message-delimiter nil))
  (let ((delimiter `(,(cons :field            (char delimiter-string 0))
		      ,(cons :component       (char delimiter-string 1))
		      ,(cons :repetition      (char delimiter-string 2))
		      ,(cons :subcomponent    (char delimiter-string 4))
		      ,(cons :segment         #\Return)           
		      ,(cons :message         #\Newline))))
    (loop for x in hl7-messages
       collect (concatenate 'string
			    (correct-msh (hl7-output x delimiter) delimiter-string)
			    (if message-delimiter
				(format nil "~c" (cdr (assoc :message delimiter :test #'eq)))
				"")))))

(defun correct-msh (string delimiter-string)
  (if (and (> (length string) 3)
	   (string= (subseq string 0 3) "MSH"))
      (concatenate 'string (subseq string 0 3) delimiter-string (subseq string 9))
      string))
      

(defun hl7-output (message delimiter)
  (if (listp message)
      (let* ((typ              (car message))
	     (delimiter-char   (cdr (assoc typ delimiter :test #'eq)))
	     (delimiter-string (if (char= delimiter-char #\~)
				   "~~" ;; escape for format sequence
				   (format nil "~c" delimiter-char))))

	
	(format nil (concatenate 'string "~{~A~^" delimiter-string "~}" (if (eq typ :segment) delimiter-string "")) 
		(loop for x in (cdr message)
		   collect (format nil "~A" (hl7-output x delimiter)))))

      (format nil "~A" message)))


;;; EOF
