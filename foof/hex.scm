;;;; Hex-string <-> bytevector conversion routines from chibi-scheme,
;;;; with some modifications.
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
  (assume (and (integer? n) (not (negative? n))))
  (if (zero? n)
      (make-bytevector 1 0)
      (u8-list->bytevector
       (unfold-right zero?
                     (lambda (n) (truncate-remainder n 256))
                     (lambda (n) (truncate-quotient n 256))
                     n))))

;;;; Hex string conversion

;; Big-endian conversion, guaranteed padded to even length.
(define (integer->hex-string n)
  (let* ((res (number->string n 16))
         (len (string-length res)))
    (if (even? len)
        res
        (string-append "0" res))))

;; Exported.
(define (bytevector->hex-string bv)
  (let ((len (bytevector-length bv)))
    (call-with-port
     (open-output-string)
     (lambda (out)
       (let lp ((i 0))
         (cond ((>= i len) (get-output-string out))
               (else
                (write-string (integer->hex-string (bytevector-u8-ref bv i))
                              out)
                (lp (+ i 1)))))))))
