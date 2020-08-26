(define-library (foof hex)
  (import (scheme base)
          (scheme bytevector)
          (srfi 151))

  (export integer->bytevector bytevector->hex-string)

  (include "hex.scm"))
