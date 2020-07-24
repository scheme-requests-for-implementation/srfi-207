(define-library (lib hex)
  (import (scheme base))

  (export integer->bytevector integer->hex-string)

  (include "hex.scm"))
