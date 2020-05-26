;;;; Hex-string <-> bytevector conversion routines from chibi-scheme.
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

;;;; Integer conversion

;; Convert an unsigned integer n to a bytevector representing
;; the base-256 big-endian form (the zero index holds the MSB).
(define (integer->bytevector n)
  (cond
   ((zero? n)
    (make-bytevector 1 0))
   ((negative? n)
    (error "can't convert a negative integer to bytevector" n))
   (else
    (let lp ((n n) (res '()))
      (if (zero? n)
          (let* ((len (length res))
                 (bv (make-bytevector len 0)))
            (do ((i 0 (+ i 1))
                 (ls res (cdr ls)))
                ((= i len) bv)
              (bytevector-u8-set! bv i (car ls))))
          (lp (quotient n 256) (cons (remainder n 256) res)))))))

;; The inverse of integer->bytevector.  Convert a bytevector
;; representing the base-256 big-endian form (the zero index holds
;; the MSB) to the corresponding unsigned integer.
(define (bytevector->integer bv)
  (let ((len (bytevector-length bv)))
    (let lp ((i 0) (n 0))
      (if (>= i len)
          n
          (lp (+ i 1)
              (+ (arithmetic-shift n 8)
                 (bytevector-u8-ref bv i)))))))

;;;; Hex string conversion

;; Big-endian conversion, guaranteed padded to even length.
(define (integer->hex-string n)
  (let* ((res (number->string n 16))
         (len (string-length res)))
    (if (even? len)
        res
        (string-append "0" res))))

(define (hex-string->integer str)
  (string->number str 16))

(define (bytevector->hex-string bv)
  (let ((out (open-output-string))
        (len (bytevector-length bv)))
    (let lp ((i 0))
      (cond
       ((>= i len)
        (get-output-string out))
       (else
        (write-string (integer->hex-string (bytevector-u8-ref bv i)) out)
        (lp (+ i 1)))))))

(define (hex-string->bytevector str)
  (integer->bytevector (hex-string->integer str)))
