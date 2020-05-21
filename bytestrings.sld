(define-library (bytestrings)
  (import (scheme base)
          (scheme case-lambda)
          (srfi 1)
          (srfi 145)
          ;; TODO: cond-expand this.
          (srfi 151))

  (export bytestring bytevector->hex-string list->bytestring list->bytestring
          bytevector->base64 base64->bytevector
          bytestring-pad bytestring-pad-right bytestring-trim
          bytestring-trim-right bytestring-trim-both bytestring-replace
          bytestring-index bytestring-index-right bytestring-break
          bytestring-span
          bytestring=? bytestring>? bytestring<? bytestring<=? bytestring>=?
          bytestring-ci=? bytestring-ci>? bytestring-ci<? bytestring-ci<=?
          bytestring-ci>=?)

  (include "base64.scm")
  (include "bytestrings.scm"))
