;;;; hl7-parser.lisp

(in-package #:hl7-parser)

;;; "hl7-parser" goes here. Hacks and glory await!

(defparameter *log-stream* *standard-output*)

;;; HL7 default seperators
(defparameter  *seperators* 
  '(:field #\|
    :component #\^
    :subcomponent #\& 
    :repetition #\~ 
    :escape #\\ 
    :segment #\Return
    :message #\Newline)
  "Default Values for initialization of delimiter objects.")

(defvar *parser-context* nil)



(defclass delimiters ()
  ((field        :initarg :field        :initform (getf *seperators* :field)        :accessor field)
   (component    :initarg :component    :initform (getf *seperators* :component)    :accessor component)
   (subcomponent :initarg :subcomponent :initform (getf *seperators* :subcomponent) :accessor subcomponent)
   (repetition   :initarg :repetition   :initform (getf *seperators* :repetition)   :accessor repetition)
   (escape       :initarg :escape       :initform (getf *seperators* :escape)       :accessor escape)
   (segment      :initarg :segment      :initform (getf *seperators* :segment)      :accessor segment)
   (message      :initarg :message      :initform (getf *seperators* :message)      :accessor message)))

(defclass hl7-message ()
  ((delimiters   :initarg :delimiters     :initform (make-instance 'delimiters) :accessor delimiters)
   (msg-stream   :initarg :msg-stream     :initform nil                         :accessor msg-stream)
   (token-list   :initarg :token-list     :initform nil                         :accessor token-list)
   (message      :initarg :message        :initform nil                         :accessor message)))



;;; Entry Methods

(defmethod parse-messages ((message string))
  "Parse Messages from String"
  (with-input-from-string (s message)
    (parse-messages s)))


(defmethod parse-messages ((message stream))
  "Parse Messages from Stream"
  (loop for x = (read-line message nil nil)
       while x
       collect (parse-message x)))


(defmethod parse-message ((message string))
  "Parse single Message from String"
  (with-input-from-string (s message)
    (parse-message s)))

(defmethod parse-message ((message stream))
  "Parse single Message from Stream"
  (let ((*parser-context* (make-instance 'hl7-message :msg-stream message)))
    (parse-message *parser-context*)))


;;; STRUCTURE
;(message segment field repetable component subcomponent)
;(message segment field component subcomponent)


  


(defmethod parse-message ((m hl7-message))
  "parse-message parses only one message and stops at the :message-delimiter.
To parse multiple Messages call parse message several times"
  (logging "parse-message")
  (setf (token-list m) (tokenize-message m)) ; set token-list in hl7-message object and set token-list in let
  (cons :message (parse-segments (loop for x = (pop (token-list m))
				      while (and x
						 (not (eq x :message-delimiter)))
				      collect x))))


(defun parse-segments (list)
  "Returs Segments, assume that the next token is the first element of an Segment"
  (logging "parse-segments")
  (loop while list
     collect (cons :segment (parse-fields (loop for x = (pop list)
					     while (and x 
							(not (or (eq x :segment-delimiter)
								 (eq x :message-delimiter)))) ;; Segment ends when nil, segment-delimiter or message-delimiter occours
					     collect x)))))
  
(defun parse-fields (list)
  (logging "parse-fields")
  (let ((result nil))
    ;; Special treatment for MSH and the Delimiters
    (when (and (> (length list) 2)
	       (equal (first list) "MSH"))
      (setf result (list (list :field (pop list))
			 (list :field (pop list))))
      (pop list)) ; Remove Field Delimiter after the MSH Delimiters
    (append result
	    (loop while list collect
		 (cons :field (parse-repetition (parse-escape (loop for x = (pop list) while (and x
												  (not (eq x :field-delimiter)))
								 collect x))))))))


(defun parse-repetition(list)
  (logging "parse-repetition")
  (if (find :repetition-delimiter list :test #'eq)
      (list
       (cons :repetition
	     (parse-components (loop while list collect
				    (loop for x = (pop list) while (and x 
									(not (eq x :repetition-delimiter)))
				       collect x)))))
      (parse-components list)))

(defun parse-components (list)
  (logging "parse-components")
  (if (find :component-delimiter list :test #'eq)
      (loop while list collect
	   (cons :component (parse-subcomponents (parse-escape (loop for x = (pop list) while (and x
												   (not (eq x :component-delimiter)))
								  collect x)))))
      list))

(defun parse-subcomponents (list)
  (logging "parse-subcomponents")
  (if (find :subcomponent-delimiter list :test #'eq)
      (loop while list collect
	   (cons :subcomponent (parse-escape (loop for x = (pop list) while (and x
										 (not (eq x :subcomponent-delimiter)))
						collect x))))
      list))


(defun parse-escape (list)
  (logging "parse-escape")
  (if (evenp (count :escape-delimiter list))
      (let ((result nil))
	(loop while list collect
	     (loop for x = (pop list) 
		while x
		do (cond ((and (eq x :escape-delimiter)
			       (>= (length list) 2)
			       (stringp (first list))
			       (eq (second list) :escape-delimiter))
			  (push (list :escape (pop list)) result)
			  (pop list)) ; remove trailing :escape-delimiter
			 (t (push x result)))))
	(reverse result))
      (break "Uneven Number of Escape Characters")))
			  


  





	 
;;; LEXER

(defmethod tokenize-message ((m hl7-message))
  "Returns flat list with Strings and Characters.
Delimiters are repalced with generic keywords.
After tokenization there is no need to pay attention to Message-Specific delimiters"
  (logging "tokenize-message")
    (let ((msh-name    (get-string-from-stream (msg-stream m) 3))
	  (delimiters  (get-string-from-stream (msg-stream m) 5)))
      ;; Check if Message starts with MSH and set the Delimiters
      (if (and (equal msh-name "MSH")
	       (set-delimiters delimiters (delimiters m)))
	  ;; Start Message tokenization
	  (progn
	    (append (list msh-name)
		    (list delimiters)
		    (loop for x = (get-next-token m) while x collect x)) ;; TODO: Stop when token is the end-message-token default: #\Newline
	    )
	  nil)))
	  
(defmethod get-next-token ((m hl7-message))
  "Returns on every call the next token from the stream in hl7-message object"
  (logging "get-next-token")
  (with-slots (msg-stream delimiters) m
    (or (string-to-next-delimiter msg-stream delimiters)
	(get-next-delimiter msg-stream delimiters))))
    
(defmethod get-next-delimiter (stream (d delimiters))
  "Read next delimiter from stream. If the next char is not a delimiter the method returns nil"
  (logging "get-next-delimiter")
  (let ((input-char (peek-char nil stream nil nil)))
    (if (delimiter-p input-char d)
	(progn 
	  (read-char stream nil nil) ; read character from stream
	  (delimiter-to-keyword input-char d))
	nil)))
	

(defmethod string-to-next-delimiter (stream (d delimiters))
  "Return all non delimiter chars as string. Leave delimiter on stream"
  (logging "string-to-next-delimiter")
  (let ((output (with-output-to-string (s)
		  (loop for x = (peek-char nil stream nil nil) while (and x (not (delimiter-p x d)))
		     do (princ (read-char stream) s)))))
    (if (equal output "")
	nil 
	output)))





;;; Delimiter Methods
(defmethod delimiter-p(char (d delimiters))
  (logging "delimiter-p")
  (with-slots (field component repetition escape subcomponent segment message) d
    (let ((delimiter-list (list field component repetition escape subcomponent segment message)))
      (and char (characterp char) (find char delimiter-list :test #'char=)))))


(defmethod delimiter-to-keyword(char (d delimiters))
  (logging "delimiter-to-keyword")
  (with-slots (field component repetition escape subcomponent segment message) d
    (cond ((equal char field)         :field-delimiter)
	  ((equal char component)     :component-delimiter)
	  ((equal char repetition)    :repetition-delimiter)
	  ((equal char escape)        :escape-delimiter)
	  ((equal char subcomponent)  :subcomponent-delimiter)
	  ((equal char segment)       :segment-delimiter)
	  ((equal char message)       :message-delimiter)
	  (t :error))))
    



(defmethod set-delimiters (delimiters (d delimiters))
  "delimiters: HL7 delimiters as String"
  (logging "set-delimiters")
  (if (= 5 (length delimiters))
      (progn 
	(setf (field d)        (char delimiters 0)
	      (component d)    (char delimiters 1)
	      (repetition d)   (char delimiters 2)
	      (escape d)       (char delimiters 3)
	      (subcomponent d) (char delimiters 4))
	d)
      nil))
	
;;; UTILS
(defun get-string-from-stream (stream number-of-characters)
  (logging "get-string-from-stream")
  (let* ((result                    (make-string number-of-characters))
	 (number-of-read-characters (read-sequence result stream :start 0 :end number-of-characters)))
    (if (= number-of-read-characters number-of-characters)
	result
	nil)))
    


;;; Logging
;(defun logging (text)
;    (print text *log-stream*))

;; Empty logging function.
(defun logging (text) (declare (ignore text)))

;;; TEST

(defun run-test (&optional (path))
  (with-open-file (s path :external-format :iso-8859-1) 
;    (let ((puffer (with-output-to-string (output)
;		    (loop for x = (read-line s nil nil) while x do (progn (princ x output)
;									  (princ #\Newline output))))))
	(time (parse-messages s)) (values)))
  

  
			   
