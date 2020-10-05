(define-library (foof base64)
  (import (scheme base)
          (only (scheme char) char-whitespace?)
          (srfi 151)
          (bytestring error))

  (cond-expand
    ((library (srfi 133))
     (import (only (srfi 133) vector-unfold)))
    (else
     (begin    ; We only need the "seedless" (tabulate) version
      (define (vector-unfold f len)
        (let ((res (make-vector len)))
          (let lp ((i 0))
            (cond ((= i len) res)
                  (else (vector-set! res i (f i))
                        (lp (+ i 1))))))))))

  (export base64-encode-bytevector decode-base64-string)

  (include "base64.scm"))
