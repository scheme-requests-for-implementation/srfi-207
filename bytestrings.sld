(define-library (bytestrings)
  (import (scheme base)
          (scheme case-lambda)
          (srfi 1)
          (srfi 145))

  (export bytestring bytevector->hex-string list->bytestring list->bytestring
          bytestring-pad bytestring-pad-right bytestring-trim
          bytestring-trim-right bytestring-trim-both bytestring-replace
          bytestring-index bytestring-index-right bytestring-break
          bytestring-span
          bytestring=?)

  (include "bytestrings.scm"))
