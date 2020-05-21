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

;;;; Utility

(define (exact-natural? x)
  (and (integer? x) (exact? x) (not (negative? x))))

(define (ascii-char-or-integer? obj)
  (let ((int-obj (if (char? obj) (char->integer obj) obj)))
    (and (exact-natural? int-obj) (< int-obj 256))))

;;;; Constructors

(define (bytestring-segment-length obj)
  (cond ((ascii-char-or-integer? obj) 1)
        ((bytevector? obj) (bytevector-length obj))
        ((string? obj) (string-length obj))
        (else (error "invalid bytestring element" obj))))

(define (arguments-byte-length args)
  (fold (lambda (arg total) (+ total (bytestring-segment-length arg)))
        0
        args))

(define (copy-bytestring-segment! to at from)
  (cond ((integer? from) (bytevector-u8-set! to at from))
        ((char? from) (bytevector-u8-set! to at (char->integer from)))
        ((bytevector? from) (bytevector-copy! to at from))
        ((string? from) (bytevector-copy! to at (string->utf8 from)))))

(define (list->bytestring lis)
  (let* ((len (arguments-byte-length lis))
         (bstring (make-bytevector len)))
    (let lp ((i 0) (lis lis))
      (if (null? lis)
          bstring
          (begin
           (copy-bytestring-segment! bstring i (car lis))
           (lp (+ i (bytestring-segment-length (car lis)))
               (cdr lis)))))))

(define (bytestring . args)
  (list->bytestring args))

;;;; Conversion

(define (bytevector-u8-fold-right kons knil bvec)
  (let ((len (bytevector-length bvec)))
    (let rec ((i 0))
      (if (>= i len)
          knil
          (kons (bytevector-u8-ref bvec i)
                (rec (+ i 1)))))))

(define (integer->hex-string n)
  (let ((hex-raw (number->string n 16)))
    (if (even? (string-length hex-raw))
        hex-raw
        (string-append "0" hex-raw))))

(define (bytevector->hex-string bstring)
  (assume (bytevector? bstring))
  (let ((len (bytevector-length bstring)))
    (bytevector-u8-fold-right (lambda (byte hex)
                                (string-append (integer->hex-string byte) hex))
                              (string)
                              bstring)))

(define bytevector->base64
  (case-lambda
    ((bvec) (bytevector->base64 bvec "+/"))
    ((bvec digits)
     (assume (bytevector? bvec))
     (assume (string? digits))
     (base64-encode-bytevector bvec digits))))

(define base64->bytevector
  (case-lambda
    ((base64-string) (bytevector->base64 base64-string "+/"))
    ((base64-string digits)
     (assume (string? base64-string))
     (assume (string? digits))
     ;; FIXME: ensure base64-string is ASCII(?)
     (base64-decode-bytevector (string->utf8 base64-string) digits))))

(cond-expand
  ((library (scheme bytevector))
   (define (bytestring->list bstring)
     (bytevector->u8-list bstring)))
  (else
   (define (bytestring->list bstring)
     (assume (bytevector? bstring))
     (list-tabulate (bytevector-length bstring)
                    (lambda (i) (bytevector-u8-ref bstring i))))))

;;;; Selection

(define (bytestring-pad-left-or-right bstring len char-or-u8 side)
  (assume (bytevector? bstring))
  (assume (exact-natural? len))
  (unless (ascii-char-or-integer? char-or-u8)
    (error "invalid bytestring element" char-or-u8))
  (let ((pad-len (- len (bytevector-length bstring))))
    (if (<= pad-len 0)
        bstring
        (let ((padded (make-bytevector len (if (char? char-or-u8)
                                               (char->integer char-or-u8)
                                               char-or-u8)))
              (offset (if (eqv? side 'right) 0 pad-len)))
          (bytevector-copy! padded offset bstring)
          padded))))

(define (bytestring-pad bstring len char-or-u8)
  (bytestring-pad-left-or-right bstring len char-or-u8 'left))

(define (bytestring-pad-right bstring len char-or-u8)
  (bytestring-pad-left-or-right bstring len char-or-u8 'right))

(define (bytestring-trim bstring pred)
  (assume (bytevector? bstring))
  (assume (procedure? pred))
  (let ((new-start (bytestring-index bstring
                                     (lambda (b) (not (pred b))))))
    (if new-start
        (bytevector-copy bstring new-start)
        (bytevector))))

(define (bytestring-trim-right bstring pred)
  (assume (bytevector? bstring))
  (assume (procedure? pred))
  (let ((new-end (+ 1 (bytestring-index-right bstring
                                              (lambda (b)
                                                (not (pred b)))))))
    (if new-end
        (bytevector-copy bstring 0 new-end)
        (bytevector))))

(define (bytestring-trim-both bstring pred)
  (assume (bytevector? bstring))
  (assume (procedure? pred))
  (let ((new-start (bytestring-index bstring
                                     (lambda (b) (not (pred b)))))
        (new-end (+ 1 (bytestring-index-right bstring
                                              (lambda (b)
                                                (not (pred b)))))))
    (bytevector-copy bstring
                     (or new-start 0)
                     (or new-end (bytevector-length bstring)))))

;;;; Replacement

(define bytestring-replace
  (case-lambda
    ((bstring1 bstring2 start end)
     (bytestring-replace bstring1 bstring2 start end start end))
    ((bstring1 bstring2 start1 end1 start2 end2)
     (assume (bytevector? bstring1))
     (assume (bytevector? bstring2))
     (assume (exact-natural? start1))
     (assume (exact-natural? end1))
     (assume (exact-natural? start2))
     (assume (exact-natural? end2))
     (let* ((b1-len (bytevector-length bstring1))
            (sub-len (- end2 start2))
            (new-len (+ sub-len (- b1-len (- end1 start1))))
            (bs-new (make-bytevector new-len)))
       (bytevector-copy! bs-new 0 bstring1 0 start1)
       (bytevector-copy! bs-new start1 bstring2 start2 end2)
       (bytevector-copy! bs-new (+ start1 sub-len) bstring1 end1 b1-len)
       bs-new))))

;;;; Searching

(define bytestring-index
  (case-lambda
    ((bstring pred) (bytestring-index bstring pred 0))
    ((bstring pred start)
     (bytestring-index bstring pred 0 (bytevector-length bstring)))
    ((bstring pred start end)
     (assume (bytevector? bstring))
     (assume (procedure? pred))
     (assume (exact-natural? start))
     (assume (exact-natural? end))
     (let lp ((i start))
       (cond ((>= i end) #f)
             ((pred (bytevector-u8-ref bstring i)) i)
             (else (lp (+ i 1))))))))

(define bytestring-index-right
  (case-lambda
    ((bstring pred) (bytestring-index-right bstring pred 0))
    ((bstring pred start)
     (bytestring-index bstring pred 0 (bytevector-length bstring)))
    ((bstring pred start end)
     (assume (bytevector? bstring))
     (assume (procedure? pred))
     (assume (exact-natural? start))
     (assume (exact-natural? end))
     (let lp ((i (- end 1)))
       (cond ((< i start) #f)
             ((pred (bytevector-u8-ref bstring i)) i)
             (else (lp (- i 1))))))))

(define (bytestring-break bstring pred)
  (assume (bytevector? bstring))
  (assume (procedure? pred))
  (let ((tail-start (bytestring-index bstring pred)))
    (if tail-start
        (values (bytevector-copy bstring 0 tail-start)
                (bytevector-copy bstring tail-start
                                         (bytevector-length bstring)))
        (values bstring (bytevector)))))

(define (bytestring-span bstring pred)
  (bytestring-break bstring (lambda (x) (not (pred x)))))

;;;; Comparison

(cond-expand
  ((library (scheme bytevector))
   (define (bytestring=? bstring1 bstring2)
     (bytevector=? bstring1 bstring2)))
  (else
   (define (bytestring=? bstring1 bstring2)
     (assume (bytevector? bstring1))
     (assume (bytevector? bstring2))
     (let ((len1 (bytevector-length bstring1)))
       (and (= len1 (bytevector-length bstring2))
            (let lp ((i 0))
              (cond ((= i len1) #t)
                    ((= (bytevector-u8-ref bstring1 i)
                        (bytevector-u8-ref bstring2 i))
                     (lp (+ i 1)))
                    (else #f))))))))
