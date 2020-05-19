(define-library (bytestrings)
  (import (scheme base)
          (srfi 1)
          (srfi 145))

  (export bytestring bytevector->hex-string list->bytestring list->bytestring
          bytestring-pad bytestring-pad-right bytestring-trim
          bytestring-trim-right bytestring-trim-both)

  (include "bytestrings.scm"))
