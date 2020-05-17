(define-library (bytestrings)
  (import (scheme base)
          (srfi 1)
          (srfi 145))

  (export bytestring bytevector->hex-string list->bytestring list->bytestring
          bytestring-pad bytestring-pad-right)

  (include "bytestrings.scm"))
