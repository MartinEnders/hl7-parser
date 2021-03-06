# Common Lisp HL7-Parser

HL7-Parser for HL7 Version 2 Messages (decode / encode) for Common Lisp.

> The HL7 Version 2 Messaging Standard — Application Protocol for Electronic
> Data Exchange in Healthcare Environments — is considered to be the workhorse
> of data exchange in healthcare and is the most widely implemented standard
> for healthcare information in the world.

Source: http://www.hl7.org/about/FAQs/index.cfm?ref=nav ("General HL7 FAQ" > "Version 2" > "What is Version 2?")

## License
http://opensource.org/licenses/BSD-3-Clause

## Implementation
SBCL and CCL on Debian GNU/Linux

## Installation

HL7-Parser is 'quickloadable':

```cl
(ql:quickload "hl7-parser")
```

## Usage
```cl
(defun decode (string-or-stream)
```

hl7-parser:decode

 * decode: Accepts a String or a Stream containing HL7-Messages
 * returns a list of parsed messages.

```cl
(defun encode (message-list &key (delimiter "|^~\\&") (message-delimiter nil))
```
hl7-parser:encode
 * `message-list` List of decoded HL7-Messages
 * `delimiter` for encoding of Message
 * `message-delimiter` if true then the encoded Messages ends with a `#\Newline` if nil then no `#\Newline` is appended (default nil).
 * returns a list of encoded messages (Strings)



```cl
(defun test-parser (in-file out-file &key (external-format :iso-8859-1) (output-delimiters "|^~\\&"))
```
hl7-test:test-parser
 * Read `in-file` (with HL7-messages) decode every line and then encode it and write it to `out-file`.
 * Use `diff in-file out-file` to check if the encode and decode of the messages work.
 * returns always `NIL`

## Exeptionhandling and validation
Every String or Stream with at least eight Characters is parsed in a more or less reasonable way.

There are no syntax or HL7-Structure checking mechanisms.

## Escaping

The HL7-Escaping works like in the most programming languages but in this case this is the wrong way.

The following escape sequences are defined in the HL7 2.5 Standard in Chapter 2.7.1:
```
\H\        start highlighting
\N\        normal text (end highlighting)
\F\        field separator
\S\        component separator
\T\        subcomponent separator
\R\        repetition separator
\E\        escape character
\Xdddd...\ hexadecimal data
\Zdddd...\ locally defined escape sequence
```

These escape sequences are not recognized correctly by the parser:
```cl
CL-USER> (hl7-parser:decode (format nil "MSH|^~~\\&|test|te\\F\\st^test|~cEVN||123~c~c" #\Return #\Return #\Newline ))
;; Evaluates to:
((:MESSAGE
  (:SEGMENT (:FIELD "MSH" "|^~\\&" "test" (:COMPONENT "te\\F\\st" "test") "")
   (:FIELD "EVN" "" "123"))))
;; Should evaluate to:
((:MESSAGE
  (:SEGMENT (:FIELD "MSH" "|^~\\&" "test" (:COMPONENT "te|st" "test") "")
   (:FIELD "EVN" "" "123"))))
```
The double-backslashes are needed because they are used within a string.

Issue: https://github.com/MartinEnders/hl7-parser/issues/1

## Example
```cl
CL-USER> (hl7-parser:decode (format nil "MSH|^~~\\&|test|test^test|~cEVN||123~c~c" #\Return #\Return #\Newline ))
((:MESSAGE
  (:SEGMENT (:FIELD "MSH" "|^~\\&" "test" (:COMPONENT "test" "test") "")
   (:FIELD "EVN" "" "123") "")))

CL-USER> (hl7-parser:encode (hl7-parser:decode (format nil "MSH|^~~\\&|test|test^test|~cEVN||123~c~c" #\Return #\Return #\Newline )))
("MSH|^~\\&|test|test^test|^MEVN||123^M") ; ^M -> #\Return

CL-USER> (hl7-parser:encode (hl7-parser:decode (format nil "MSH|^~~\\&|test|test^test|~cEVN||123~c~c" #\Return #\Return #\Newline )) :message-delimiter t)
("MSH|^~\\&|test|test^test|^MEVN||123^M
") ; ^M -> #\Return
```

How to set Delimiters for encoding:
```cl
CL-USER> (hl7-parser:decode (format nil "MSH#^~~\\&#test#test^test#~c" #\Newline ))
((:MESSAGE (:FIELD "MSH" "#^~\\&" "test" (:COMPONENT "test" "test") "")))

CL-USER> (hl7-parser:encode (hl7-parser:decode (format nil "MSH|^~~\\&|test|test^test|~c" #\Newline )) :delimiter "#^~\\&")
("MSH#^~\\&#test#test^test#")
```

Pay attention to the character escaping (~ in format directive and backslashes)