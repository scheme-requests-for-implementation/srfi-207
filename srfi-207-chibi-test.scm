;;; Copyright (C) 2020 Wolfgang Corcoran-Mathe
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the
;;; "Software"), to deal in the Software without restriction, including
;;; without limitation the rights to use, copy, modify, merge, publish,
;;; distribute, sublicense, and/or sell copies of the Software, and to
;;; permit persons to whom the Software is furnished to do so, subject to
;;; the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included
;;; in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
;;; OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Test suite for chibi-scheme (http://synthcode.com/scheme/chibi)

(import (scheme base)
        (srfi 207)
        (only (srfi 1) list-tabulate every)
        (chibi test))

(cond-expand
  ((library (srfi 158))
   (import (only (srfi 158) generator->list)))
  (else
   (begin
    (define (generator->list gen)
      (let rec ((x (gen)))
        (if (eof-object? x)
            '()
            (cons x (rec (gen)))))))))

;;;; Utility

(define-syntax constantly
  (syntax-rules ()
    ((_ obj) (lambda _ obj))))

(define always (constantly #t))
(define never (constantly #f))

;; Returns a list of the values produced by expr.
(define-syntax values~>list
  (syntax-rules ()
    ((_ expr)
     (call-with-values (lambda () expr) list))))

;; If expr causes an exception to be raised, return 'bytestring-error
;; if the raised object satisfies bytestring-error?, and #f otherwise.
(define-syntax catch-bytestring-error
  (syntax-rules ()
   ((_ expr)
    (guard (condition ((bytestring-error? condition) 'bytestring-error)
                      (else #f))
      expr))))

;; Testing shorthand for write-binary-bytestring.
(define (%bytestring/IO . args)
  (call-with-port (open-output-bytevector)
                  (lambda (port)
                    (apply write-binary-bytestring port args)
                    (get-output-bytevector port))))

;; Testing shorthands for SNB I/O.  Coverage library fans, eat your
;; hearts out.
(define (parse-SNB/prefix s)
  (call-with-port (open-input-string s)
                  (lambda (p)
                    (read-textual-bytestring #t p))))

(define (parse-SNB s)
  (call-with-port (open-input-string s)
                  (lambda (p)
                    (read-textual-bytestring #f p))))

(define (%bytestring->SNB bstring)
  (call-with-port (open-output-string)
                  (lambda (port)
                    (write-textual-bytestring bstring port)
                    (get-output-string port))))


(define test-bstring (bytestring "lorem"))

(define homer
  (bytestring "The Man, O Muse, informe, who many a way / \
               Wound in his wisedome to his wished stay;"))

(define homer64
  "VGhlIE1hbiwgTyBNdXNlLCBpbmZvcm1lLCB3aG8gbWFueSBhIHdheSAvIFdvd\
   W5kIGluIGhpcyB3aXNlZG9tZSB0byBoaXMgd2lzaGVkIHN0YXk7")

(define homer64-w
  "VGhlIE1hb iwgTyBNdXNlL CBpbmZvcm1lL\nCB3aG8gbWF\tueSBhIH\rdheSAvIFdvd\
   W5kIGluI   GhpcyB    3aXNlZ\t\t\nG9tZSB0b    yBoaXMgd\t2lzaGVkIHN0YXk7")

;;;; Constructors

(test-group "Constructors"
  (test test-bstring (bytestring "lo" #\r 101 #u8(#x6D)))
  (test (bytevector) (bytestring))

  (test 'bytestring-error (catch-bytestring-error (bytestring 256)))
  (test 'bytestring-error (catch-bytestring-error (bytestring "λ")))
)

(test-group "Conversion"
  (test "6c6f72656d" (bytevector->hex-string test-bstring))
  (test test-bstring (hex-string->bytevector "6c6f72656d"))
  (test 'bytestring-error
        (catch-bytestring-error (hex-string->bytevector "c6f72656d")))
  (test 'bytestring-error
        (catch-bytestring-error (hex-string->bytevector "6czf72656d")))
  (test homer (hex-string->bytevector (bytevector->hex-string homer)))

  (test #u8() (hex-string->bytevector (bytevector->hex-string #u8())))

  (test "bG9yZW0=" (bytevector->base64 test-bstring))
  (test "/+//" (bytevector->base64 #u8(#xFF #xEF #xFF)))
  (test "@*@@" (bytevector->base64 #u8(#xFF #xEF #xFF) "*@"))
  (test homer64 (bytevector->base64 homer))
  (test "AQ==" (bytevector->base64 #u8(#x01)))
  (test "" (bytevector->base64 #u8()))
  (test test-bstring (base64->bytevector "bG9yZW0="))
  (test #u8(#xFF #xEF #xFF) (base64->bytevector "/+//"))
  (test #u8(#xFF #xEF #xFF) (base64->bytevector "@*@@" "*@"))
  (test homer (base64->bytevector homer64))
  (test homer (base64->bytevector homer64-w))
  (test #u8(#x01) (base64->bytevector "AQ=="))
  (test #u8() (base64->bytevector ""))
  (test #u8() (base64->bytevector "\n\n\n==\t\r\n"))
  (test 'bytestring-error
        (catch-bytestring-error (base64->bytevector "bG9@frob")))

  (test-assert (null? (bytestring->list #u8())))
  (test '(#\F #\R 0 #\B) (bytestring->list (bytestring 70 82 0 66)))
  (test '(7 9 9 10 200) (bytestring->list (bytestring "\a\t\t\n" 200)))
  (test test-bstring (make-bytestring (bytestring->list test-bstring)))
  (test (bytestring "rem") (make-bytestring (bytestring->list test-bstring 2)))
  (test (bytestring "or") (make-bytestring (bytestring->list test-bstring 1 3)))

  (let ((bvec (make-bytevector 5)))
    (test test-bstring
          (begin
           (make-bytestring! bvec 0 '(#x6c #x6f #x72 #x65 #x6d))
           bvec)))
  (let ((bvec (make-bytevector 9 #x20)))
    (test (bytestring "  lorem  ")
          (begin (make-bytestring! bvec 2 '("lo" #\r #x65 #u8(#x6d)))
                 bvec)))
  (test 'bytestring-error (catch-bytestring-error (make-bytestring '("λ"))))
  (test 'bytestring-error (catch-bytestring-error (make-bytestring '(#x100))))

  (let ((s (list-tabulate (bytevector-length test-bstring)
                          (lambda (i)
                            (bytevector-u8-ref test-bstring i)))))
    (test s (let ((g (make-bytestring-generator "lo" #\r #x65 #u8(#x6d))))
               (generator->list g))))
  (test 'bytestring-error
        (catch-bytestring-error (make-bytestring-generator "λ" #\m #\u)))
  (test 'bytestring-error
        (catch-bytestring-error (make-bytestring-generator 89 90 300)))
)

(test-group "Selectors"
  (test test-bstring
        (bytestring-pad test-bstring (bytevector-length test-bstring) 122))
  (test "zzzlorem" (utf8->string (bytestring-pad test-bstring 8 122)))
  (test (bytestring-pad test-bstring 8 #\z)
        (bytestring-pad test-bstring 8 (char->integer #\z)))
  (test test-bstring
        (bytestring-pad-right test-bstring
                              (bytevector-length test-bstring)
                              122))
  (test "loremzzz" (utf8->string (bytestring-pad-right test-bstring 8 122)))
  (test (bytestring-pad-right test-bstring 8 #\z)
        (bytestring-pad-right test-bstring 8 (char->integer #\z)))

  (test #u8() (bytestring-trim test-bstring always))
  (test test-bstring (bytestring-trim test-bstring never))
  (test #u8(#x72 #x65 #x6D)
        (bytestring-trim test-bstring (lambda (u8) (< u8 112))))
  (test #u8() (bytestring-trim-right test-bstring always))
  (test test-bstring (bytestring-trim-right test-bstring never))
  (test #u8(#x6C #x6F #x72)
        (bytestring-trim-right test-bstring (lambda (u8) (< u8 112))))
  (test #u8() (bytestring-trim-both test-bstring always))
  (test test-bstring (bytestring-trim-both test-bstring never))
  (test #u8(#x72)
        (bytestring-trim-both test-bstring (lambda (u8) (< u8 112))))
)

(test-group "bytestring-replace"
  (test (bytestring "lists")
        (bytestring-replace test-bstring (bytestring "mists") 1 5 1 5))
  (test (bytestring "loaded")
        (bytestring-replace test-bstring (bytestring "faded") 2 5 1 5))
  (test test-bstring
        (bytestring-replace (make-bytevector 5)
                            test-bstring
                            0
                            (bytevector-length test-bstring)))

  (let ((bv1 (bytestring "food")) (bv2 (bytestring "od fo")))
    (test (bytestring "food food") (bytestring-replace bv1 bv2 2 2 0 5)))
  (let ((bv1 (bytestring "food food")))
    (test (bytestring "food") (bytestring-replace bv1 (bytevector) 2 7 0 0)))
)

(test-group "Comparison"
  (define short-bstring (bytestring "lore"))
  (define long-bstring (bytestring "lorem "))
  (define mixed-case-bstring (bytestring "loreM"))

  (test-not (bytestring<? test-bstring test-bstring))
  (test-assert (bytestring<? short-bstring test-bstring))
  (test-assert (bytestring<? mixed-case-bstring test-bstring))
  (test-not (bytestring>? test-bstring test-bstring))
  (test-assert (bytestring>? test-bstring short-bstring))
  (test-assert (bytestring>? test-bstring mixed-case-bstring))
  (test-assert (bytestring<=? test-bstring test-bstring))
  (test-assert (bytestring<=? short-bstring test-bstring))
  (test-assert (bytestring<=? mixed-case-bstring test-bstring))
  (test-not (bytestring<=? test-bstring mixed-case-bstring))
  (test-not (bytestring<=? long-bstring test-bstring))
  (test-assert (bytestring>=? test-bstring test-bstring))
  (test-assert (bytestring>=? test-bstring short-bstring))
  (test-assert (bytestring>=? test-bstring mixed-case-bstring))
  (test-not (bytestring>=? mixed-case-bstring test-bstring))
  (test-not (bytestring>=? short-bstring test-bstring))
)

(test-group "Searching"
  (define (eq-r? b) (= b #x72))
  (define (lt-r? b) (< b #x72))

  (test 0 (bytestring-index test-bstring always))
  (test-not (bytestring-index test-bstring never))
  (test 3 (bytestring-index test-bstring always 3))
  (test 2 (bytestring-index test-bstring eq-r?))

  (test 4 (bytestring-index-right test-bstring always))
  (test-not (bytestring-index-right test-bstring never))
  (test 4 (bytestring-index-right test-bstring always 3))
  (test 2 (bytestring-index-right test-bstring eq-r?))

  (test (list test-bstring (bytevector))
        (values~>list (bytestring-span test-bstring always)))
  (test (list (bytevector) test-bstring)
        (values~>list (bytestring-span test-bstring never)))
  (test (list (bytestring "lo") (bytestring "rem"))
        (values~>list (bytestring-span test-bstring lt-r?)))

  (test (list (bytevector) test-bstring)
        (values~>list (bytestring-break test-bstring always)))
  (test (list test-bstring (bytevector))
        (values~>list (bytestring-break test-bstring never)))
  (test (list (bytestring "lo") (bytestring "rem"))
        (values~>list (bytestring-break test-bstring eq-r?)))
)

(test-group "Join and split"
  (define test-segments '(#u8(1) #u8(2) #u8(3)))

  (test #u8(#x01 0 #x02 0 #x03) (bytestring-join test-segments #u8(0)))
  (test #u8(0 #x01 0 #x02 0 #x03)
        (bytestring-join test-segments #u8(0) 'prefix))
  (test #u8(#x01 0 #x02 0 #x03 0)
        (bytestring-join test-segments #u8(0) 'suffix))
  (test #u8() (bytestring-join '() #u8(0)))
  (test #u8(#x01 #x20 #x02 #x20 #x03) (bytestring-join test-segments #\space))
  (test #u8(#x01 0 #x02 0 #x03) (bytestring-join test-segments 0))
  (test #u8(#x01 #x41 #x42 #x02 #x41 #x42 #x03)
        (bytestring-join test-segments "AB"))

  (test #u8(1 7 8 2 7 8 3) (bytestring-join test-segments #u8(7 8)))
  (test 'bytestring-error
        (catch-bytestring-error (bytestring-join test-segments 300)))
  (test 'bytestring-error
        (catch-bytestring-error (bytestring-join test-segments "λ")))
  (test 'bytestring-error
        (catch-bytestring-error (bytestring-join '() #u8(0) 'strict-infix)))
  (test 'bytestring-error
        (catch-bytestring-error (bytestring-join '() #u8(0) 'foofix)))

  (test test-segments (bytestring-split #u8(1 0 2 0 3) 0 'infix))
  (test test-segments (bytestring-split #u8(0 #x01 0 #x02 0 #x03) 0 'prefix))
  (test test-segments (bytestring-split #u8(#x01 0 #x02 0 #x03 0) 0 'suffix))
  (test '(#u8() #u8() #u8()) (bytestring-split #u8(0 0) 0))
  (test '() (bytestring-split #u8() 0))
  (test 'bytestring-error
        (catch-bytestring-error (bytestring-split #u8() 0 'foofix)))
)

(test-group "I/O"
  (test test-bstring (%bytestring/IO "lo" #\r 101 #u8(#x6D)))
  (test #u8() (%bytestring/IO))
  (test 'bytestring-error (catch-bytestring-error (%bytestring/IO 256)))
  (test 'bytestring-error (catch-bytestring-error (%bytestring/IO "λ")))

    ;;; read-textual-bytestring

  (test #u8() (parse-SNB/prefix "#u8\"\""))
  (test test-bstring (parse-SNB/prefix "#u8\"lorem\""))
  (test (bytevector 222 173 240 13)
        (parse-SNB/prefix "#u8\"\\xde;\\xad;\\xf0;\\x0d;\""))
  (test (bytestring #\" #\\ #\alarm #\backspace #\tab #\newline #\return #\|)
        (parse-SNB/prefix "#u8\"\\\"\\\\\\a\\b\\t\\n\\r\\|\""))
  (test test-bstring (parse-SNB/prefix "#u8\"lor\\\n\te\\   \r\n\tm\""))
  (test test-bstring (parse-SNB "\"lorem\""))

    ;; Invalid SNB detection.
  (test 'bytestring-error
        (catch-bytestring-error (parse-SNB/prefix "#u\"lorem\"")))
  (test 'bytestring-error
        (catch-bytestring-error (parse-SNB/prefix "#u8lorem\"")))
  (test 'bytestring-error
        (catch-bytestring-error (parse-SNB/prefix "#u8\"lorem")))
  (test 'bytestring-error
        (catch-bytestring-error (parse-SNB/prefix "#u8\"lorem")))
  (test 'bytestring-error
        (catch-bytestring-error (parse-SNB/prefix "#u8\"l\\orem\"")))
  (test 'bytestring-error
        (catch-bytestring-error (parse-SNB/prefix "#u8\"l\\    orem\"")))
  (test 'bytestring-error
        (catch-bytestring-error (parse-SNB/prefix "#u8\"l\\x6frem\"")))
  (test 'bytestring-error
        (catch-bytestring-error (parse-SNB/prefix "#u8\"l\\x6z;rem\"")))
  (test 'bytestring-error
        (catch-bytestring-error (parse-SNB/prefix "#u8\"α equivalence\"")))

    ;;; write-textual-bytestring

  (test "#u8\"\"" (%bytestring->SNB #u8()))
  (test "#u8\"lorem\"" (%bytestring->SNB test-bstring))
  (test "#u8\"\\xde;\\xad;\\xbe;\\xef;\""
        (%bytestring->SNB (bytevector 222 173 190 239)))
  (test "#u8\"\\\"\\\\\\a\\b\\t\\n\\r\\|\""
        (%bytestring->SNB
         (bytestring #\" #\\ #\alarm #\backspace #\tab #\newline #\return #\|)))

  (let ((test-bstrings
         '(#u8(124 199 173 212 209 232 249 16 198 32 123 111 130 92 64 155)
           #u8(50 133 193 27 177 105 10 186 61 149 177 105 96 70 223 190)
           #u8(0 117 226 155 110 0 66 216 27 129 187 81 17 210 71 152)
           #u8(123 31 159 25 100 135 246 47 249 137 243 241 45 241 240 221)
           #u8(207 186 70 110 118 231 79 195 153 253 93 101 126 198 70 235)
           #u8(138 176 92 152 208 107 28 236 198 254 111 37 241 116 191 206)
           #u8(221 254 214 90 0 155 132 92 157 246 199 224 224 142 91 114)
           #u8(228 216 233 80 142 15 158 54 5 85 174 101 111 75 126 209)
           #u8(191 16 83 245 45 98 72 212 148 202 135 19 213 150 141 121)
           #u8(41 169 182 96 47 184 16 116 196 251 243 93 81 162 175 140)
           #u8(85 49 218 138 132 11 27 11 182 27 120 71 254 169 132 166)
           #u8(89 216 175 23 97 10 237 112 208 195 112 80 198 154 241 254)
           #u8(187 54 6 57 250 137 129 89 188 19 225 217 168 178 174 129)
           #u8(88 164 89 40 175 194 108 56 12 124 109 96 148 149 119 109)
           #u8(241 66 32 115 203 71 128 154 240 111 194 137 73 44 146 3)
           #u8(177 185 177 233 18 14 178 106 110 109 222 147 111 157 216 208))))
    (test-assert
     (every (lambda (bvec)
              (equal? bvec (parse-SNB/prefix (%bytestring->SNB bvec))))
            test-bstrings)))
)
