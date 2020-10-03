(define-library (foof hex)
  (import (scheme base)
          (scheme bytevector)
          (only (srfi 1) unfold-right)
          (srfi 145)
          (srfi 151))

  (export integer->bytevector bytestring->hex-string)

  (include "hex.scm"))
