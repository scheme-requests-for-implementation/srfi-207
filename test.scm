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

(import (scheme base))
(import (bytestrings))

(cond-expand
  ((library (srfi 78))
   (import (srfi 78)))
  (else
    (begin
      (define-syntax check
        (syntax-rules (=>)
          ((check expr)
           (check expr => #t))
          ((check expr => expected)
           (if (equal? expr expected)
             (begin
               (display 'expr)
               (display " => ")
               (display expected)
               (display " ; correct")
               (newline))
             (begin
               (display "FAILED: for ")
               (display 'expr)
               (display " expected ")
               (display expected)
               (display " but got ")
               (display expr)
               (newline))))))
      (define (check-report) #t))))

;;;; Utility

;; Returns the value of expr, or, if an exception was raised, the
;; object that was raised.
(define-syntax catch-exceptions
  (syntax-rules ()
   ((_ expr)
    (call-with-current-continuation
     (lambda (k)
       (with-exception-handler
        k
        (lambda () expr)))))))

(define test-bstring (bytestring "lorem"))

;;;; Constructors

(define (check-constructor)
  (check (bytestring "lo" #\r #x65 #u8(#x6d)) => test-bstring)
  (check (bytestring)                         => (bytevector))

  (check (bytestring-error? (catch-exceptions (bytestring #x100))) => #t)
  (check (bytestring-error? (catch-exceptions (bytestring "λ")))   => #t))

(define (check-conversion)
  (check (bytevector->hex-string test-bstring) => "6c6f72656d")
  
  (check (bytevector->base64 test-bstring)             => "bG9yZW0=")
  (check (bytevector->base64 #u8(#xff #xef #xff))      => "/+//")
  (check (bytevector->base64 #u8(#xff #xef #xff) "*@") => "@*@@")
  (check (base64->bytevector "bG9yZW0=")               => test-bstring)
  (check (base64->bytevector "/+//")                   => #u8(#xff #xef #xff))
  (check (base64->bytevector "@*@@" "*@")              => #u8(#xff #xef #xff)))

(define (check-all)
  (check-constructor)
  (check-conversion)
  (check-report))

(check-all)
