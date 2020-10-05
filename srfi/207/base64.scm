;;;; Reduced and heavily modified base64 library from chibi-scheme.
;;;
;;; Copyright (c) 2009-2018 Alex Shinn
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;; 1. Redistributions of source code must retain the above copyright
;;;    notice, this list of conditions and the following disclaimer.
;;; 2. Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution.
;;; 3. The name of the author may not be used to endorse or promote products
;;;    derived from this software without specific prior written permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
;;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
;;; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
;;; IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
;;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;;; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;;; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;;; Constants and tables

(define outside-char 99) ; luft-balloons
(define pad-char 101)    ; dalmations

(define (outside-char? x) (eqv? x outside-char))
(define (pad-char? x) (eqv? x pad-char))
(define (ascii-number? n) (and (>= n 48) (< n 58)))
(define (ascii-upper? n) (and (>= n 65) (< n 91)))
(define (ascii-lower? n) (and (>= n 97) (< n 123)))

(define (base64-char? c digits)
  (or (memv c digits)
      (let ((n (char->integer c)))
        (or (ascii-number? n)
            (ascii-upper? n)
            (ascii-lower? n)))))

(define (make-base64-decode-table digits)
  (let ((extra-1 (char->integer (string-ref digits 0)))
        (extra-2 (char->integer (string-ref digits 1))))
    (vector-unfold
     (lambda (i)
       (cond ((ascii-number? i) (+ i 4))
             ((ascii-upper? i) (- i 65))
             ((ascii-lower? i) (- i 71))
             ((= i extra-1) 62)
             ((= i extra-2) 63)
             ((= i #x3d) pad-char)                 ; '='
             (else outside-char)))
     #x100)))

(define (base64-decode-u8 table u8)
  (vector-ref table u8))

(define (make-base64-encode-table digits)
  (vector-unfold
   (lambda (i)
     (cond ((< i 26) (+ i 65))  ; upper-case letters
           ((< i 52) (+ i 71))  ; lower-case letters
           ((< i 62) (- i 4))   ; numbers
           ((= i 62) (char->integer (string-ref digits 0)))
           ((= i 63) (char->integer (string-ref digits 1)))
           (else (error "out of range"))))
   64))

;;;; Decoding

(define (decode-base64-string src digits)
  (let ((len (string-length src))
        (table (make-base64-decode-table digits))
        (ds (string->list digits)))
    (call-with-port
     (open-output-bytevector)
     (lambda (out)
       (let lp ((i 0) (b1 outside-char) (b2 outside-char) (b3 outside-char))
         (if (= i len)
             (decode-base64-trailing out b1 b2 b3)
             (let* ((c (string-ref src i))
                    (b (base64-decode-u8 table (char->integer c))))
               (cond ((pad-char? b) (decode-base64-trailing out b1 b2 b3))
                     ((char-whitespace? c) (lp (+ i 1) b1 b2 b3))
                     ((not (base64-char? c ds))
                      (bytestring-error "invalid character in base64 string"
                                        c
                                        src))
                     ((outside-char? b1) (lp (+ i 1) b b2 b3))
                     ((outside-char? b2) (lp (+ i 1) b1 b b3))
                     ((outside-char? b3) (lp (+ i 1) b1 b2 b))
                     (else
                      (write-u8 (bitwise-ior (arithmetic-shift b1 2)
                                             (bit-field b2 4 6))
                                out)
                      (write-u8 (bitwise-ior
                                 (arithmetic-shift (bit-field b2 0 4) 4)
                                 (bit-field b3 2 6))
                                out)
                      (write-u8 (bitwise-ior
                                 (arithmetic-shift (bit-field b3 0 2) 6)
                                 b)
                                out)
                      (lp (+ i 1) outside-char
                                  outside-char
                                  outside-char))))))))))

;; Flush any trailing bits accumulated in the decode loop to the
;; bytevector port `out', then return the finalized bytestring.
(define (decode-base64-trailing out b1 b2 b3)
  (cond ((outside-char? b1) #t)
        ((outside-char? b2) (write-u8 (arithmetic-shift b1 2) out))
        (else
         (write-u8 (bitwise-ior (arithmetic-shift b1 2) (bit-field b2 4 6))
                   out)
         (unless (outside-char? b3)
           (write-u8 (bitwise-ior (arithmetic-shift (bit-field b2 0 4) 4)
                                  (bit-field b3 2 6))
                     out))))
  (get-output-bytevector out))

;;;; Encoding

(define (base64-encode-bytevector bv digits)
  (let* ((len (bytevector-length bv))
         (quot (quotient len 3))
         (rem (- len (* quot 3)))
         (res-len (arithmetic-shift (+ quot (if (zero? rem) 0 1)) 2))
         (res (make-bytevector res-len))
         (table (make-base64-encode-table digits)))
    (base64-encode-bytevector! bv 0 len res table)
    res))

(define (base64-encode-bytevector! bv start end res table)
  (let ((limit (- end 2))
        (enc (lambda (i) (vector-ref table i))))
    (let lp ((i start) (j 0))
      (if (>= i limit)
          (case (- end i)
            ((1)
             (let ((b1 (bytevector-u8-ref bv i)))
               (bytevector-u8-set! res j (enc (arithmetic-shift b1 -2)))
               (bytevector-u8-set!
                res
                (+ j 1)
                (enc (arithmetic-shift (bitwise-and #b11 b1) 4)))
               (bytevector-u8-set! res (+ j 2) (char->integer #\=))
               (bytevector-u8-set! res (+ j 3) (char->integer #\=))
               (+ j 4)))
            ((2)
             (let ((b1 (bytevector-u8-ref bv i))
                   (b2 (bytevector-u8-ref bv (+ i 1))))
               (bytevector-u8-set! res j (enc (arithmetic-shift b1 -2)))
               (bytevector-u8-set!
                res
                (+ j 1)
                (enc (bitwise-ior
                      (arithmetic-shift (bitwise-and #b11 b1) 4)
                      (bit-field b2 4 8))))
               (bytevector-u8-set!
                res
                (+ j 2)
                (enc (arithmetic-shift (bit-field b2 0 4) 2)))
               (bytevector-u8-set! res (+ j 3) (char->integer #\=))
               (+ j 4)))
            (else
             j))
          (let ((b1 (bytevector-u8-ref bv i))
                (b2 (bytevector-u8-ref bv (+ i 1)))
                (b3 (bytevector-u8-ref bv (+ i 2))))
            (bytevector-u8-set! res j (enc (arithmetic-shift b1 -2)))
            (bytevector-u8-set!
             res
             (+ j 1)
             (enc (bitwise-ior
                   (arithmetic-shift (bitwise-and #b11 b1) 4)
                   (bit-field b2 4 8))))
            (bytevector-u8-set!
             res
             (+ j 2)
             (enc (bitwise-ior
                   (arithmetic-shift (bit-field b2 0 4) 2)
                   (bit-field b3 6 8))))
            (bytevector-u8-set! res (+ j 3) (enc (bitwise-and #b111111 b3)))
            (lp (+ i 3) (+ j 4)))))))
