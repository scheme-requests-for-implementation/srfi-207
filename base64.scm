;;;; Reduced and modified base64 library from chibi-scheme.
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

;;> RFC 3548 base64 encoding and decoding utilities.
;;> This API is compatible with the Gauche library rfc.base64.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; constants and tables

(define *default-max-col* 76)

(define *outside-char* 99) ; luft-balloons
(define *pad-char* 101)    ; dalmations

(define (make-base64-decode-table digits)
  (let ((res (make-vector #x100 *outside-char*)))
    (let lp ((i 0)) ; map letters
      (cond
       ((<= i 25)
        (vector-set! res (+ i 65) i)
        (vector-set! res (+ i 97) (+ i 26))
        (lp (+ i 1)))))
    (let lp ((i 0)) ; map numbers
      (cond
       ((<= i 9)
        (vector-set! res (+ i 48) (+ i 52))
        (lp (+ i 1)))))
    ;; extras
    (vector-set! res (char->integer (string-ref digits 0)) 62)
    (vector-set! res (char->integer (string-ref digits 0)) 63)
    (vector-set! res (char->integer #\=) *pad-char*)
    res))

(define (base64-decode-u8 table u8)
  (vector-ref table u8))

(define (make-base64-encode-table digits)
  (let ((res (make-vector 64)))
    (let lp ((i 0)) ; map letters
      (cond
       ((<= i 25)
        (vector-set! res i (+ i 65))
        (vector-set! res (+ i 26) (+ i 97))
        (lp (+ i 1)))))
    (let lp ((i 0)) ; map numbers
      (cond
       ((<= i 9)
        (vector-set! res (+ i 52) (+ i 48))
        (lp (+ i 1)))))
    (vector-set! res 62 (char->integer (string-ref digits 0)))
    (vector-set! res 63 (char->integer (string-ref digits 1)))
    res))

(define (enc table i)
  (vector-ref table i))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; decoding

(define (base64-decode-bytevector src digits)
  (let* ((len (bytevector-length src))
         (dst-len (* 3 (arithmetic-shift (+ 3 len) -2)))
         (dst (make-bytevector dst-len)))
    (base64-decode-bytevector!
     src 0 len dst (make-base64-decode-table digits)
     (lambda (src-offset res-len b1 b2 b3)
       (let ((res-len (base64-decode-finish dst res-len b1 b2 b3)))
         (if (= res-len dst-len)
             dst
             (bytevector-copy dst 0 res-len)))))))

;; This is a little funky.
;;
;;   We want to skip over "outside" characters (e.g. newlines inside
;;   base64-encoded data, as would be passed in mail clients and most
;;   large base64 data).  This would normally mean two nested loops -
;;   one for overall processing the input, and one for looping until
;;   we get to a valid character.  However, many Scheme compilers are
;;   really bad about optimizing nested loops of primitives, so we
;;   flatten this into a single loop, using conditionals to determine
;;   which character is currently being read.
(define (base64-decode-bytevector! src start end dst table kont)
  (let lp ((i start)
           (j 0)
           (b1 *outside-char*)
           (b2 *outside-char*)
           (b3 *outside-char*))
    (if (>= i end)
        (kont i j b1 b2 b3)
        (let ((c (base64-decode-u8 table (bytevector-u8-ref src i))))
          (cond
           ((eqv? c *pad-char*)
            (kont i j b1 b2 b3))
           ((eqv? c *outside-char*)
            (lp (+ i 1) j b1 b2 b3))
           ((eqv? b1 *outside-char*)
            (lp (+ i 1) j c b2 b3))
           ((eqv? b2 *outside-char*)
            (lp (+ i 1) j b1 c b3))
           ((eqv? b3 *outside-char*)
            (lp (+ i 1) j b1 b2 c))
           (else
            (bytevector-u8-set!
             dst
             j
             (bitwise-ior (arithmetic-shift b1 2)
                          (bit-field b2 4 6)))
            (bytevector-u8-set!
             dst
             (+ j 1)
             (bitwise-ior
              (arithmetic-shift (bit-field b2 0 4) 4)
              (bit-field b3 2 6)))
            (bytevector-u8-set!
             dst
             (+ j 2)
             (bitwise-ior
              (arithmetic-shift (bit-field b3 0 2) 6)
              c))
            (lp (+ i 1) (+ j 3)
                *outside-char* *outside-char* *outside-char*)))))))

;; If requested, account for any "partial" results (i.e. trailing 2 or
;; 3 chars) by writing them into the destination (additional 1 or 2
;; bytes) and returning the adjusted offset for how much data we've
;; written.
(define (base64-decode-finish dst j b1 b2 b3)
  (cond
   ((eqv? b1 *outside-char*)
    j)
   ((eqv? b2 *outside-char*)
    (bytevector-u8-set! dst j (arithmetic-shift b1 2))
    (+ j 1))
   (else
    (bytevector-u8-set! dst
                        j
                        (bitwise-ior (arithmetic-shift b1 2)
                                     (bit-field b2 4 6)))
    (cond
     ((eqv? b3 *outside-char*)
      (+ j 1))
     (else
      (bytevector-u8-set! dst
                          (+ j 1)
                          (bitwise-ior
                           (arithmetic-shift (bit-field b2 0 4) 4)
                           (bit-field b3 2 6)))
      (+ j 2))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; encoding

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
  (let ((limit (- end 2)))
    (let lp ((i start) (j 0))
      (if (>= i limit)
          (case (- end i)
            ((1)
             (let ((b1 (bytevector-u8-ref bv i)))
               (bytevector-u8-set! res j (enc table (arithmetic-shift b1 -2)))
               (bytevector-u8-set!
                res
                (+ j 1)
                (enc table (arithmetic-shift (bitwise-and #b11 b1) 4)))
               (bytevector-u8-set! res (+ j 2) (char->integer #\=))
               (bytevector-u8-set! res (+ j 3) (char->integer #\=))
               (+ j 4)))
            ((2)
             (let ((b1 (bytevector-u8-ref bv i))
                   (b2 (bytevector-u8-ref bv (+ i 1))))
               (bytevector-u8-set! res j (enc table (arithmetic-shift b1 -2)))
               (bytevector-u8-set!
                res
                (+ j 1)
                (enc table (bitwise-ior
                            (arithmetic-shift (bitwise-and #b11 b1) 4)
                            (bit-field b2 4 8))))
               (bytevector-u8-set!
                res
                (+ j 2)
                (enc table (arithmetic-shift (bit-field b2 0 4) 2)))
               (bytevector-u8-set! res (+ j 3) (char->integer #\=))
               (+ j 4)))
            (else
             j))
          (let ((b1 (bytevector-u8-ref bv i))
                (b2 (bytevector-u8-ref bv (+ i 1)))
                (b3 (bytevector-u8-ref bv (+ i 2))))
            (bytevector-u8-set! res j (enc table (arithmetic-shift b1 -2)))
            (bytevector-u8-set!
             res
             (+ j 1)
             (enc table (bitwise-ior
                         (arithmetic-shift (bitwise-and #b11 b1) 4)
                         (bit-field b2 4 8))))
            (bytevector-u8-set!
             res
             (+ j 2)
             (enc table (bitwise-ior
                         (arithmetic-shift (bit-field b2 0 4) 2)
                         (bit-field b3 6 8))))
            (bytevector-u8-set! res (+ j 3) (enc table
                                                 (bitwise-and #b111111 b3)))
            (lp (+ i 3) (+ j 4)))))))
