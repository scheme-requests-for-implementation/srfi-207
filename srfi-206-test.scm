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
(import (srfi 206))

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

(define (print-header message)
  (newline)
  (display (string-append ";;; " message))
  (newline))

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

(define-syntax with-output-to-bytevector
  (syntax-rules ()
    ((_ thunk)
     (parameterize ((current-output-port (open-output-bytevector)))
       (thunk)
       (get-output-bytevector (current-output-port))))))

(define test-bstring (bytestring "lorem"))

;;;; Constructors

(define (check-constructor)
  (print-header "Running constructor tests...")
  (check (bytestring "lo" #\r #x65 #u8(#x6d)) => test-bstring)
  (check (bytestring)                         => (bytevector))

  (check (catch-bytestring-error (bytestring #x100)) => 'bytestring-error)
  (check (catch-bytestring-error (bytestring "λ"))   => 'bytestring-error))

(define (check-conversion)
  (print-header "Running conversion tests...")
  (check (bytevector->hex-string test-bstring) => "6c6f72656d")
  (check (hex-string->bytevector "6c6f72656d") => test-bstring)

  (check (hex-string->bytevector (bytevector->hex-string #u8())) => #u8())

  (check (bytevector->base64 test-bstring)             => "bG9yZW0=")
  (check (bytevector->base64 #u8(#xff #xef #xff))      => "/+//")
  (check (bytevector->base64 #u8(#xff #xef #xff) "*@") => "@*@@")
  (check (base64->bytevector "bG9yZW0=")               => test-bstring)
  (check (base64->bytevector "/+//")                   => #u8(#xff #xef #xff))
  (check (base64->bytevector "@*@@" "*@")              => #u8(#xff #xef #xff))

  (check (bytestring->list #u8()) => '())
  (check (bytestring->list test-bstring) => '(#x6c #x6f #x72 #x65 #x6d))
  (check (list->bytestring (bytestring->list test-bstring)) => test-bstring))

(define (check-selection)
  (print-header "Running selection tests...")

  (check (bytestring-pad test-bstring (bytevector-length test-bstring) #x7a)
   => test-bstring)
  (check (utf8->string (bytestring-pad test-bstring 8 #x7a))
   => "zzzlorem")
  (check (equal? (bytestring-pad test-bstring 8 #\z)
                 (bytestring-pad test-bstring 8 (char->integer #\z)))
   => #t)
  (check (bytestring-pad-right test-bstring
                               (bytevector-length test-bstring)
                               #x7a)
   => test-bstring)
  (check (utf8->string (bytestring-pad-right test-bstring 8 #x7a))
   => "loremzzz")
  (check (equal? (bytestring-pad-right test-bstring 8 #\z)
                 (bytestring-pad-right test-bstring 8 (char->integer #\z)))
   => #t)

  (check (bytestring-trim test-bstring always) => #u8())
  (check (bytestring-trim test-bstring never)  => test-bstring)
  (check (bytestring-trim test-bstring (lambda (u8) (< u8 #x70)))
   => #u8(#x72 #x65 #x6d))
  (check (bytestring-trim-right test-bstring always) => #u8())
  (check (bytestring-trim-right test-bstring never)  => test-bstring)
  (check (bytestring-trim-right test-bstring (lambda (u8) (< u8 #x70)))
   => #u8(#x6c #x6f #x72))
  (check (bytestring-trim-both test-bstring always) => #u8())
  (check (bytestring-trim-both test-bstring never)  => test-bstring)
  (check (bytestring-trim-both test-bstring (lambda (u8) (< u8 #x70)))
   => #u8(#x72)))

(define (check-replacement)
  (print-header "Running bytestring-replace tests...")

  (check (bytestring-replace test-bstring (bytestring "mists") 1 5)
   => (bytestring "lists"))
  (check (bytestring-replace test-bstring (bytestring "faded") 2 5 1 5)
   => (bytestring "loaded"))
  (check (bytestring-replace (make-bytevector 5)
                             test-bstring
                             0
                             (bytevector-length test-bstring))
   => test-bstring)

  ;; Replacing from the end of the `to' bytestring is equivalent
  ;; to appending the two bytestrings.
  (let ((test-bstring2 (bytestring " ipsum")))
    (check (bytestring-replace test-bstring
                               test-bstring2
                               (bytevector-length test-bstring)
                               (bytevector-length test-bstring)
                               0
                               (bytevector-length test-bstring2))
     => (bytevector-append test-bstring test-bstring2))))

(define (check-comparison)
  (define short-bstring (bytestring "lore"))
  (define long-bstring (bytestring "lorem "))
  (define mixed-case-bstring (bytestring "loreM"))
  (print-header "Runnng comparison tests...")

  (check (bytestring=? test-bstring test-bstring)        => #t)
  (check (bytestring=? test-bstring
                       #u8(#x6c #x6f #x72 #x65 #x6d))
   => #t)
  (check (bytestring=? test-bstring mixed-case-bstring)  => #f)
  (check (bytestring=? test-bstring short-bstring)       => #f)
  (check (bytestring<? test-bstring test-bstring)        => #f)
  (check (bytestring<? short-bstring test-bstring)       => #t)
  (check (bytestring<? mixed-case-bstring test-bstring)  => #t)
  (check (bytestring>? test-bstring test-bstring)        => #f)
  (check (bytestring>? test-bstring short-bstring)       => #t)
  (check (bytestring>? test-bstring mixed-case-bstring)  => #t)
  (check (bytestring<=? test-bstring test-bstring)       => #t)
  (check (bytestring<=? short-bstring test-bstring)      => #t)
  (check (bytestring<=? mixed-case-bstring test-bstring) => #t)
  (check (bytestring<=? test-bstring mixed-case-bstring) => #f)
  (check (bytestring<=? long-bstring test-bstring)       => #f)
  (check (bytestring>=? test-bstring test-bstring)       => #t)
  (check (bytestring>=? test-bstring short-bstring)      => #t)
  (check (bytestring>=? test-bstring mixed-case-bstring) => #t)
  (check (bytestring>=? mixed-case-bstring test-bstring) => #f)
  (check (bytestring>=? short-bstring test-bstring)      => #f)

  (check (bytestring-ci=? test-bstring test-bstring)        => #t)
  (check (bytestring-ci=? test-bstring
                          #u8(#x6c #x6f #x72 #x65 #x6d))
   => #t)
  (check (bytestring-ci=? test-bstring mixed-case-bstring)  => #t)
  (check (bytestring-ci=? test-bstring short-bstring)       => #f)
  (check (bytestring-ci<? test-bstring test-bstring)        => #f)
  (check (bytestring-ci<? short-bstring test-bstring)       => #t)
  (check (bytestring-ci<? mixed-case-bstring test-bstring)  => #f)
  (check (bytestring-ci>? test-bstring test-bstring)        => #f)
  (check (bytestring-ci>? test-bstring short-bstring)       => #t)
  (check (bytestring-ci>? test-bstring mixed-case-bstring)  => #f)
  (check (bytestring-ci<=? test-bstring test-bstring)       => #t)
  (check (bytestring-ci<=? short-bstring test-bstring)      => #t)
  (check (bytestring-ci<=? mixed-case-bstring test-bstring) => #t)
  (check (bytestring-ci<=? test-bstring mixed-case-bstring) => #t)
  (check (bytestring-ci<=? long-bstring test-bstring)       => #f)
  (check (bytestring-ci>=? test-bstring test-bstring)       => #t)
  (check (bytestring-ci>=? test-bstring short-bstring)      => #t)
  (check (bytestring-ci>=? test-bstring mixed-case-bstring) => #t)
  (check (bytestring-ci>=? mixed-case-bstring test-bstring) => #t)
  (check (bytestring-ci>=? short-bstring test-bstring)      => #f))

(define (check-searching)
  (define (eq-r? b) (= b #x72))
  (define (lt-r? b) (< b #x72))
  (print-header "Running search tests...")

  (check (bytestring-index test-bstring always)     => 0)
  (check (bytestring-index test-bstring never)      => #f)
  (check (bytestring-index test-bstring always 3)   => 3)
  (check (bytestring-index test-bstring eq-r?) => 2)

  (check (bytestring-index-right test-bstring always)     => 4)
  (check (bytestring-index-right test-bstring never)      => #f)
  (check (bytestring-index-right test-bstring always 3)   => 4)
  (check (bytestring-index-right test-bstring eq-r?) => 2)

  (check (values~>list (bytestring-span test-bstring always))
   => (list test-bstring (bytevector)))
  (check (values~>list (bytestring-span test-bstring never))
   => (list (bytevector) test-bstring))
  (check (values~>list (bytestring-span test-bstring lt-r?))
   => (list (bytestring "lo") (bytestring "rem")))

  (check (values~>list (bytestring-break test-bstring always))
   => (list (bytevector) test-bstring))
  (check (values~>list (bytestring-break test-bstring never))
   => (list test-bstring (bytevector)))
  (check (values~>list (bytestring-break test-bstring eq-r?))
   => (list (bytestring "lo") (bytestring "rem"))))

(define (check-join-and-split)
  (define test-segments '(#u8(1) #u8(2) #u8(3)))
  (print-header "Running joining and splitting tests...")

  (check (bytestring-join test-segments #u8(0))         => #u8(1 0 2 0 3))
  (check (bytestring-join test-segments #u8(0) 'prefix) => #u8(0 1 0 2 0 3))
  (check (bytestring-join test-segments #u8(0) 'suffix) => #u8(1 0 2 0 3 0))
  (check (bytestring-join '() #u8(0))                   => #u8())
  (check (catch-bytestring-error
           (bytestring-join '() #u8(0) 'strict-infix))  => 'bytestring-error)
  (check (catch-bytestring-error
           (bytestring-join '() #u8(0) 'foofix))        => 'bytestring-error)

  (check (bytestring-split #u8(1 0 2 0 3) 0 'infix)    => test-segments)
  (check (bytestring-split #u8(0 1 0 2 0 3) 0 'prefix) => test-segments)
  (check (bytestring-split #u8(1 0 2 0 3 0) 0 'suffix) => test-segments)
  (check (bytestring-split #u8(0 0) 0)                 => '(#u8() #u8() #u8()))
  (check (bytestring-split #u8() 0)                    => '())
  (check (catch-bytestring-error
           (bytestring-split #u8() 0 'foofix))         => 'bytestring-error))

(define (check-output)
  (print-header "Running output tests...")

  (check (with-output-to-bytevector
          (lambda ()
            (write-bytestring (current-output-port) "lo" #\r #x65 #u8(#x6d))))
   => test-bstring)
  (check (catch-bytestring-error
           (with-output-to-bytevector
            (lambda () (write-bytestring (current-output-port) #x100))))
   => 'bytestring-error))

(define (check-all)
  (check-constructor)
  (check-conversion)
  (check-selection)
  (check-replacement)
  (check-comparison)
  (check-searching)
  (check-join-and-split)
  (check-output)

  (check-report))

(check-all)
