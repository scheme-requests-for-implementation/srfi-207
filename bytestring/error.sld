(define-library (bytestring error)
  (import (scheme base))

  (export bytestring-error-message
          bytestring-error-irritants
          bytestring-error?
          bytestring-error)

  (begin
   (define-record-type <bytestring-error>
     (raw-bytestring-error message irritants)
     bytestring-error?
     (message bytestring-error-message)
     (irritants bytestring-error-irritants))

   (define (bytestring-error message . irritants)
     (raise (raw-bytestring-error message irritants))))
)
