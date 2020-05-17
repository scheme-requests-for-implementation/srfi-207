(define-library (bytestrings)
  (import (srfi 1)
          (srfi 145))

  (export bytestring bytevector->hex-string list->bytestring list->bytestring)

  (include "bytestrings.scm"))
