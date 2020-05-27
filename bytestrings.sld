(define-library (bytestrings)
  (import (scheme base)
          (scheme case-lambda)
          (srfi 1)
          ;; (srfi 151)
          )

  (cond-expand
    ((library (scheme bytevector))
     (import (scheme bytevector)))
    (else #t))

  (cond-expand
    ((library (srfi 145))
     (import (srfi 145)))
    (else
     (begin
      (define (assume _) #t))))

  (cond-expand
    ((library (srfi 151))             ; SRFI 151 is the One True Way
     (import (srfi 151)))
    (chicken                          ; DELETE ME
     (begin
      (import (chicken bitwise))
      (define (mask size) (bitwise-not (arithmetic-shift -1 size)))
      (define (bit-field n start end)
        (bitwise-and (arithmetic-shift n (- start))
                     (mask (- end start)))))))

  (cond-expand
    ((library (srfi 152))
     (import (srfi 152)))
    ((library (srfi 130))
     (import (srfi 130)))
    ((library (srfi 13))
     (import (srfi 13))))

  (export bytestring list->bytestring bytevector->hex-string bytestring->list
          hex-string->bytevector bytevector->base64 base64->bytevector
          bytestring-pad bytestring-pad-right bytestring-trim
          bytestring-trim-right bytestring-trim-both bytestring-replace
          bytestring-index bytestring-index-right bytestring-break
          bytestring-span
          bytestring=? bytestring>? bytestring<? bytestring<=? bytestring>=?
          bytestring-ci=? bytestring-ci>? bytestring-ci<? bytestring-ci<=?
          bytestring-ci>=?
          bytestring-error? bytestring-error-message bytestring-error-irritants
          bytestring-join bytestring-split
          write-bytestring)

  (include "base64.scm")
  (include "hex.scm")
  (include "bytestrings.scm"))
