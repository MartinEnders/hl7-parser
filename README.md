# Common Lisp HL7-Parser

This Project is currently in development.

I'm not sure if I'll do further development in the next time.

(It is just on Github to get not lost)



## Implementation
SBCL on Debian GNU/Linux

## Usage
Please have a look at the packages.lisp file - there are parse-functions exportet they except Strings or Streams.

The fast-parser (it is just faster than the other one) accepts only streams.

## Example
```cl
CL-USER> (with-input-from-string (s (format nil "MSH|^~~\\&|test|test^test|~c" #\Newline ))
	   (hl7-parser:parse-message s))
(:MESSAGE
 (:SEGMENT (:FIELD "MSH") (:FIELD "|^~\\&") (:FIELD "test")
  (:FIELD (:COMPONENT "test") (:COMPONENT "test"))))
CL-USER> (with-input-from-string (s (format nil "MSH|^~~\\&|test|test^test|~c" #\Newline ))
	   (hl7-fast-parser:parse s))
((:MESSAGE
  (:SEGMENT (:FIELD "MSH") (:FIELD "|^~\\&") (:FIELD "test")
   (:FIELD (:COMPONENT "test") (:COMPONENT "test")))
  (:SEGMENT (:FIELD NIL))))
```

As you can see hl7-parser and hl7-fast-parser work slightly different.

e.g. hl7-fast-parser will not recognize HL7-Escape sequences.

