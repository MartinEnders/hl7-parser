;;;; hl7-parser.lisp

(in-package #:hl7-parser-v3)

;;; "hl7-parser" goes here. Hacks and glory await!


(defparameter *test-message* "m|a^b^c^d^^^^|e" );; "MSH|^~\\&|a|b^c|e"
(setf *test-message* "m|a^r|t|i|n")

(defparameter *delimiters* '((#\Newline . :message) (#\Return . :segment) (#\| . :field) (#\~ . :repetition) (#\^ . :component) (#\& . :sub-component)))


(defun test-wrapper ()
  (parser (lexer *test-message*)))

(defun lexer (message)
  (with-input-from-string (s message)
    (loop for c = (read-char s nil nil)
	 while c
	 collect c)))



(defun parser (tokens)
  (let ((result nil)
	(token (pop tokens))
	(delimiter (car tokens)))

    (if (and (delimiter-p delimiter)
	     (not (delimiter-p token)))

	(cond ((delimiter= delimiter #\|)
	       (push (list :segment token) result))
	      ((delimiter< delimiter #\|)
	       (print "call next function"))
	      ((delimiter> delimiter #\|)
	       (print "return from function"))
	      (t
	       (error "Parsing error")))

	(error "Parsing error outer"))
  (reverse result)))
    

;; (defun parse-field (token delimiter)
;;   (let ((next-step (next-step token delimiter #\|)))
;;     (case next-step
;;       (stay
;;        (list :field token))
;;       (down
;;        (list :field (list :component token))) ;; list component wird funktionsaufruf zur naechsten ebene
;;       (up
;;        (list :field token)))))



(defun test (tokens)
  (loop for x = (pop tokens)
        for y = (pop tokens)
       while x
       collect (list (get-delimiter-name y) x)))



(defun next-step (token delimiter current-level)
  (if (and (delimiter-p delimiter)
	   (delimiter-p current-level)
	   (not (delimiter-p token)))
      (cond ((delimiter= delimiter current-level)
	     'stay)
	    ((delimiter< delimiter current-level)
	     'down)
	    ((delimiter> delimiter current-level)
	     'up)
	    (t
	     (error "01 next-step error")))
      (error "02 next-step error")))


;; auf der ebene auf der ich mich befinde bis delimiter lesen
;; wenn delimiter mein eigener ist dann zusammenpacken und auf resultat legen
;; wenn delimiter uebergeordnet ist dann komplett beenden
;; wenn delimiter untegeordnet ist dann token und stream an naechste ebene weitergeben
;; bei repetition sonderbehandlung wenn man nicht auf eigens token stoesst.

;; Anname: Der Lexer stellt sicher, dass werte immer ein Token sind. D.h. nach einem nicht-Delimiter kommt sicher immer ein Delimiter



;; Funktion die abhaenging vom Token und des darauffolgenden tokens (peek) und der hierarchieebene entscheidet was gemacht wird

;(defun next-step (token peek layer)
  
  



;; folgends kommt schon mal gut:
(defun parser-test-segment (tokens)
  (let ((result nil))
    (loop for x = (pop tokens)
       while x
       do (if (delimiter-p (car tokens))
	      (cond ((delimiter>= (car tokens) #\Return) (pop tokens) (return))
		    ((delimiter< (car tokens) #\Return) (pop tokens) (push (parser-test-field (cons x tokens)) result)))
	      
	      (push x result)))
    (append '(:segment) (reverse result))))
	      

(defun parser-test-field (tokens)
  (let ((result nil))
    (loop for x = (pop tokens)
       while x
       do (if (delimiter-p (car tokens))
	      (cond ((delimiter>= (car tokens) #\|) (pop tokens) (return))
		    ((delimiter< (car tokens) #\|) (pop tokens) (push (parser-test-component (cons x tokens)) result)))
	      
	      (push x result)))
    (append '(:field) (reverse result))))


;; (defun parser-test-component (tokens)
;;   (cons :component (pop tokens)))


	      
;;        while (and x
;; 		  (or (not (delimiter-p x))
;; 		      (delimiter<= x #\Return))) ;; solange loopen bis ende der hirarchieebene
	
	
      


;;   (loop for x = (car tokens)
;;      while (not (delimiter-p x))
;;      do (pop tokens)
;;      collect x))

  





(defun parse-segment (tokens)
  (append (list :segment)
	  (loop while tokens
	     collect (let* ((last-token nil)
			    (processed-tokens (loop for x = (pop tokens)
						 do (setf last-token x)
						 while (and x (not (char= x #\|)))
						 collect x)))
		       (parse-field processed-tokens)))))
	  
       
       


(defun parse-field (tokens)
  (append (list :field)
	  (if (= (length tokens) 1)
	      tokens
	      (loop while tokens
		 collect (let* ((last-token nil)
				(processed-tokens (loop for x = (pop tokens)
						     do (setf last-token x)
						     while (and x (char/= x #\~))
						     collect x)))
			   (parse-repetition  processed-tokens))))))
  
  
  


(defun parse-repetition (tokens)
  (append (list :repetition)
	  (if (= (length tokens) 1)
	      tokens
	  (loop while tokens
	       collect (let* ((last-token nil)
			      (processed-tokens (loop for x = (pop tokens)
						     do (setf last-token x)
						     while (and x (not (char= x #\^)))
						     collect x)))
			 ;(append '(:component) processed-tokens)))))
			 (parse-component processed-tokens))))))




(defun parse-component (tokens)
  (append (list :component)
	  (if (= (length tokens) 1)
	      tokens
	  (loop while tokens
	       collect (let* ((last-token nil)
			      (processed-tokens (loop for x = (pop tokens)
						     do (setf last-token x)
						     while (and x (not (char= x #\&)))
						     collect x)))
			 (parse-sub-component processed-tokens))))))

(defun parse-sub-component (tokens)
  (append (list :sub-components) tokens))



;;  _   _ _____ ___ _     ____  
;; | | | |_   _|_ _| |   / ___| 
;; | | | | | |  | || |   \___ \ 
;; | |_| | | |  | || |___ ___) |
;;  \___/  |_| |___|_____|____/ 
;;  _____ _____ _____ _____ _____ 
;; |_____|_____|_____|_____|_____|
                               
                               
;; Delimiter Utils
(defun get-delimiter-name (delimiter)
  (cdr (assoc delimiter *delimiters*)))

(defun delimiter-p (token)
  (member token *delimiters* :test #'equal :key #'car))

;; Delimiter comparison (based on hirachy)
(defun delimiter< (token-1 token-2)
  (delimiter-compare-intern token-1 token-2 #'>))

(defun delimiter<= (token-1 token-2)
  (delimiter-compare-intern token-1 token-2 #'>=))

(defun delimiter> (token-1 token-2)
  (delimiter-compare-intern token-1 token-2 #'<))

(defun delimiter>= (token-1 token-2)
  (delimiter-compare-intern token-1 token-2 #'<=))

(defun delimiter= (token-1 token-2)
  (delimiter-compare-intern token-1 token-2 #'=))

(defun delimiter-compare-intern (token-1 token-2 number-compare-function)
  (let ((pos-1 (position token-1 *delimiters* :key #'car :test #'char=))
	(pos-2 (position token-2 *delimiters* :key #'car :test #'char=)))
    (if (and pos-1 pos-2 (numberp pos-1) (numberp pos-2))
	(funcall number-compare-function pos-1 pos-2)
	(error "Parameters are no delimiters"))))




