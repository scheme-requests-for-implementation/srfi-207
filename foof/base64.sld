(define-library (foof base64)
  (import (scheme base)
          (srfi 151))

  (export base64-encode-bytevector base64-decode-bytevector)

  (include "base64.scm"))
