;;;; parser-test.lisp

(in-package #:hl7-fast-parser)

;;; "hl7-parser" goes here. Hacks and glory await!


;;  ____        __ _       _ _   _                 
;; |  _ \  ___ / _(_)_ __ (_) |_(_) ___  _ __  ___ 
;; | | | |/ _ \ |_| | '_ \| | __| |/ _ \| '_ \/ __|
;; | |_| |  __/  _| | | | | | |_| | (_) | | | \__ \
;; |____/ \___|_| |_|_| |_|_|\__|_|\___/|_| |_|___/
                                                


(defparameter *linear-structure* '(:message :segment :field :repetition :component :subcomponent)
  "HL7-Message Structure elements")

(defparameter  *seperators* '(#\Newline #\Return #\| #\~ #\^ #\&)
  "HL7-Default Delimiters for Message, Segment, Field, Repetition, Component and Subcomponent")


(defparameter *structure-bu* (loop for x in *linear-structure*
		   for y in (cdr *linear-structure*)
		  collect y 
		  collect x)
"Bottom up structure, plist e.g. (:segment :message :field :segment) you can look up the next higher hirarchy token") 

;;  _   _ _   _ _     
;; | | | | |_(_) |___ 
;; | | | | __| | / __|
;; | |_| | |_| | \__ \
;;  \___/ \__|_|_|___/
                   

(defun structure> (a b &optional (structure *linear-structure*))
  (< (position a structure) (position b structure)))

(defun structure< (a b &optional (structure *linear-structure*))
  (> (position a structure) (position b structure)))

(defun structure<= (a b &optional (structure *linear-structure*))
  (let ((pos-a (position a structure))
	(pos-b (position b structure)))
    (and pos-a pos-b (>= pos-a pos-b))))

(defun structure>= (a b &optional (structure *linear-structure*))
  (let ((pos-a (position a structure))
	(pos-b (position b structure)))
    (and pos-a pos-b (<= pos-a pos-b))))

;;  _   _ _    _____     ____                          
;; | | | | |  |___  |   |  _ \ __ _ _ __ ___  ___ _ __ 
;; | |_| | |     / /____| |_) / _` | '__/ __|/ _ \ '__|
;; |  _  | |___ / /_____|  __/ (_| | |  \__ \  __/ |   
;; |_| |_|_____/_/      |_|   \__,_|_|  |___/\___|_|   
                                                    


; PID|1^hahaaa&bla|Martin#\Return
; (:segment (:field "PID") (:field (:component "1") (:component (:subcomponent "hahaaa") (:subcomponent "bla"))) (:field "Martin"))
; "PID" :field "1" :component "hahaaa" :subcomponent "blaa" :field "Martin" :segment
;				  "PV1" :field "2" :field "301423" :message  



(defun parse (stream &optional (tokenizer (get-tokenizer stream)) )
  (let ((collector nil)
	(last-token nil)
	(last-control nil)) ;; last delimiter
    (loop for token-with-property = (multiple-value-list (funcall tokenizer)) while (cadr token-with-property)
       do (let ((x (car token-with-property)))
	    (cond ((or (stringp x) (null x))
		   (push x collector) ;; collect non delimiter tokens
		   (setf last-token x))
		  
		  ((keywordp x) 
		   (when (and last-token (keywordp last-token))
		     (push nil collector))

		   (if (or (not last-control) (eq x last-control) (not (structure< last-control x)) )
		       (progn
			 (push (list x (pop collector)) collector)) ;  At first delimiter occurance or when current delimitier equals last delimiter
		       
		       (let ((pop-collector (pop collector)))
			 (push (list last-control pop-collector) collector) ; push last delimiter and last data to collector
			 (progn
			   (let ((local-last-control last-control))
			     ;; When the Parser finds a Delimiter which is in hirarchy at least two steps higher then the last one - then close all the opened hirarchys from current untill the last layer under the current delimiter
			     (loop while (structure<= (getf *structure-bu* local-last-control) x)
				do (let ((local-result nil))

				     ;; hard coded optionality of :repetition
				     (let ((2-controls (list (caar collector) (caadr collector))))
				       (when (and (eq (car  2-controls) :repetition)
						  (not (apply #'eq 2-controls)))
					 ;; If Repetition occurs only once
					 (push (cons :field (cdr (pop collector))) collector)
					 (setf local-last-control (getf *structure-bu* local-last-control))))

				     ;; collect all elements for the last delimiter in local-result
				     (loop for element in collector while (eq (car element) local-last-control)
					do (push (pop collector) local-result))

				     ;; go one step up in hierarchy
				     (setf local-last-control (getf *structure-bu* local-last-control)) 

				     ;; push list of keyword of enclosing structure e.g. component for subcomponent -> all subcomponents are pushed to collector in the form (:component (:subcomponent ...)(:subcomponet ...)...)
				     (push (cons local-last-control local-result) collector)  ))))))


		   (setf last-token x)
		   (setf last-control x)))))
	 
	 (reverse collector)))


;;  _   _ _    _____   _____     _              _              
;; | | | | |  |___  | |_   _|__ | | _____ _ __ (_)_______ _ __ 
;; | |_| | |     / /____| |/ _ \| |/ / _ \ '_ \| |_  / _ \ '__|
;; |  _  | |___ / /_____| | (_) |   <  __/ | | | |/ /  __/ |   
;; |_| |_|_____/_/      |_|\___/|_|\_\___|_| |_|_/___\___|_|   
                                            
(defun tokenize (string-or-stream)
  (cond ((stringp string-or-stream)
	 (with-input-from-string (stream string-or-stream)
	   (tokenize-to-list stream)))
	((streamp string-or-stream)
	 (tokenize-to-list string-or-stream))
	(t (error "string-or-stream ~S is not of type String or Stream" string-or-stream))))
	 
	       
(defun tokenize-to-list (stream)
  (let ((tokenizer (get-tokenizer stream)))
    (loop for result = (multiple-value-list (funcall tokenizer))
       while (cadr result)
       collect (car result))))
	         


(defun get-tokenizer (stream)
  "Returns a tokenizerfunction which returns two values at each call.
First value is the token, Second value is the end of tokens indicator.
When second value is nil then the end of token-stream is reached"
  (let* ((token-hash  nil)
	 (start-list  nil)) ;; start list is ('MSH' :field 'delimiter')

    ;(format t "~%Delimiters: ~S" delimiters)

    ;; Check if Message starts with MSH and set the Delimiters
    (multiple-value-bind (th sl) (initialize-delimiters stream)
      (setf token-hash th
	    start-list sl))
    
    #'(lambda ()
	(if start-list
	    (values (pop start-list) t)
	    (multiple-value-bind (token token-p) (get-next-token stream token-hash)
	      
	      (when (and token-p (eq token :message)) ; When new Message starts then re-initialize the delimiters
		(multiple-value-bind (th sl) (initialize-delimiters stream)
		  (when (and th sl)
		    (setf token-hash th
			  start-list sl))))
	      
	      (values token token-p))))))


(defun initialize-delimiters (stream)
  (let* ((msh-name     (get-string-from-stream stream 3))
	 (delimiters   (get-string-from-stream stream 5))
	 (token-hash   nil)
	 (start-list   nil))

    (if (and (equal msh-name "MSH") delimiters)
	(progn 
	  (setf token-hash (set-delimiters delimiters))
	  (setf start-list (list msh-name 
				 (delimiter (char delimiters 0) token-hash)
				 delimiters))
	  (values token-hash start-list))
	(values nil nil))))



(defun get-next-token (stream token-hash)
  "Returns on every call the next token from the stream in hl7-message object"
  ;(format t "~%get-next-token")
  (multiple-value-bind (delimiter delimiter-p) (string-to-next-delimiter stream token-hash)
    (if delimiter-p 
	(progn
	  ;(format t "~%  delimiter: ~S p: ~S" delimiter delimiter-p)
	  (values delimiter delimiter-p))
	(multiple-value-bind (value value-p) (get-next-delimiter stream token-hash)
	  (values value value-p)))))
	      


    
(defun get-next-delimiter (stream token-hash)
  "Read next delimiter from stream. If the next char is not a delimiter the method returns nil"
  ;(format t "~%get-next-delimiter")
  (let ((input-char (peek-char nil stream nil nil)))
    (if (delimiter input-char token-hash)
	(progn (read-char stream nil nil) ; read character from stream
	       ;(format t "~%  input-char: ~S, delimiter-key: ~S" input-char (delimiter input-char token-hash))
	       (values (delimiter input-char token-hash) input-char))
	(values nil input-char))))
	
(defun string-to-next-delimiter (stream token-hash)
  "Return all non delimiter chars as string. Leave delimiter on stream"
  ;(format t "~%get-next-string")
  (let ((peek-char-p (peek-char nil stream nil nil)))
    ;(format t "~%  peek-char-p: ~S" peek-char-p)
    (if (and peek-char-p (not (delimiter peek-char-p token-hash)))
	(let ((output (with-output-to-string (s)
			(loop for x = (peek-char nil stream nil nil) while (and x (not (delimiter x token-hash)))
			 do (princ (read-char stream) s)))))
	  (if (and (equal output "") peek-char-p)
	      (values nil t)
	      (values output t)))
	(values nil nil))))


;;  ____       _ _           _ _                
;; |  _ \  ___| (_)_ __ ___ (_) |_ ___ _ __ ___ 
;; | | | |/ _ \ | | '_ ` _ \| | __/ _ \ '__/ __|
;; | |_| |  __/ | | | | | | | | ||  __/ |  \__ \
;; |____/ \___|_|_|_| |_| |_|_|\__\___|_|  |___/
                                             


(defun set-delimiters (delimiter-string)
  "Returns Hash with delimiter-characters as key and keywords as values"
  (let ((delimiter-plist (loop 
			    for x in *linear-structure*
			    for b in *seperators*
			    collect x
			    collect b))
	(token-hash (make-hash-table)))
  
    ;(format t "~%set-delimiters: ~S" delimiter-plist)

    ;; Fill p-list with keywords and delimiter-characters
    (if (and (stringp delimiter-string)
	     (= (length delimiter-string) 5))
	(setf (getf delimiter-plist :field)        (char delimiter-string 0)
	      (getf delimiter-plist :component)    (char delimiter-string 1)
	      (getf delimiter-plist :repetition)   (char delimiter-string 2)
	      (getf delimiter-plist :escape)       (char delimiter-string 3)
	      (getf delimiter-plist :subcomponent) (char delimiter-string 4))
	(error "Uncorrect Delimiter-list: ~S" delimiter-string))
    
    ;; Fill Hash with delimiters as keys and keywords as values
    (let ((keywords (remove-if-not #'keywordp delimiter-plist)))
      (loop for k in keywords
	 do (setf (gethash (getf delimiter-plist k) token-hash) k)))
	          
    token-hash))

(defun delimiter (character token-hash)
  "Returns nil when character is not a delimiter otherwise true"
  (gethash character token-hash))

     
	  
;;  _   _ _____ ___ _     ____  
;; | | | |_   _|_ _| |   / ___| 
;; | | | | | |  | || |   \___ \ 
;; | |_| | | |  | || |___ ___) |
;;  \___/  |_| |___|_____|____/ 
                             

(defun get-string-from-stream (stream number-of-characters)
  (let* ((result                    (make-string number-of-characters))
	 (number-of-read-characters (read-sequence result stream :start 0 :end number-of-characters)))
    (if (= number-of-read-characters number-of-characters)
	result
	nil)))
    
                                      
;;  _____ _____ ____ _____ 
;; |_   _| ____/ ___|_   _|
;;   | | |  _| \___ \ | |  
;;   | | | |___ ___) || |  
;;   |_| |_____|____/ |_|  
           

(defun run-test (&optional (path) (return-p nil) )
  (with-open-file (s path :external-format :iso-8859-1) 
    (if return-p
	(time (parse s))
	(progn 
	  (time (parse s))
	  (values)))))

;; hl7-parser
;; Evaluation took:
;;   62.006 seconds of real time
;;   72.340000 seconds of total run time (71.640000 user, 0.700000 system)
;;   [ Run times consist of 10.244 seconds GC time, and 62.096 seconds non-GC time. ]
;;   116.67% CPU
;;   88 lambdas converted
;;   111,341,090,096 processor cycles
;;   7,502,474,576 bytes consed
