(define-library (srfi 207)
  (import (scheme base)
          (scheme case-lambda)
          (srfi 1)
          (srfi 151)
          (foof hex)
          (foof base64))

  (cond-expand
    ((library (scheme bytevector))
      (import (only (scheme bytevector) bytevector=? bytevector->u8-list))
      (begin
       (define (bytestring->list bstring)
         (bytevector->u8-list bstring))))
    (else
     (begin
      (define bytevector=? equal?)
      (define (bytestring->list bstring)
        (assume (bytevector? bstring))
        (list-tabulate (bytevector-length bstring)
                       (lambda (i) (bytevector-u8-ref bstring i)))))))

  (cond-expand
    ((library (srfi 145))
     (import (srfi 145)))
    (else
     (begin
      (define (assume _) #t))))

  (cond-expand
    ((library (srfi 152))
     (import (srfi 152)))
    ((library (srfi 130))
     (import (srfi 130)))
    ((library (srfi 13))
     (import (srfi 13)))
    (else
     (error
      "No string library found (need one of SRFIs 152, 130, or 13).")))

  (cond-expand
    (guile       ; u8vectors and bytevectors are distinct in Guile
     (begin
      (define (u8vector-for-each proc bvec)
        (assume (procedure? proc))
        (assume (bytevector? bvec))
        (let ((len (bytevector-length bvec)))
          (let lp ((i 0))
            (cond ((= i len) (if #f #f))
                  (else
                   (proc (bytevector-u8-ref bvec i))
                   (lp (+ i 1)))))))))
    (else (import (only (srfi 160 u8) u8vector-for-each))))

  (export bytestring list->bytestring bytevector->hex-string bytestring->list
          bytevector->string
          string->bytevector
          list->bytestring!
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

  (include "parse.scm")
  (include "207.scm"))
