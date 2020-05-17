(define (convert-argument x error-location)
  (cond ((and (integer? x) (<= 0 x) (<= x 255))
         (bytevector x))
        ((and (char? x) (char<=? #\null x) (char<=? x #\delete))
         (bytevector (char->integer x)))
        ((bytevector? x) x)
        ((string? x) (string->utf8 x))  ; TODO: ensure ASCII
        (else ; TODO: error type
         (error (string-append error-location ": invalid argument") x))))

;; TODO: Error handling and algorithmic improvement.
(define (bytestring . args)
  (let lp ((bs (bytevector)) (args args))
    (if (null? args)
        bs
        (lp (bytevector-append bs (convert-argument (car args) "bytestring"))
            (cdr args)))))

(define (list->bytestring lis)
  (fold-right (lambda (x bs)
                (bytevector-append (convert-argument x "list->bytestring") bs))
              (bytevector)
              lis))

;; FIXME: Are exact integers the Right Thing here?  Anything which is
;; a valid argument to bytestring is allowed as an element of the list
;; we return.
;;
;; This is just bytevector-u8-ref from (scheme bytevector).  TODO: Use
;; a cond-expand.
(define (bytestring->list bytestring)
  (assume (bytevector? bytestring))
  (list-tabulate (bytevector-length bytestring)
                 (lambda (i) (bytevector-u8-ref bytestring i))))

(define (bytevector-fold-right kons knil bvec)
  (let ((len (bytevector-length bvec)))
    (let rec ((i 0))
      (if (>= i len)
          knil
          (kons (bytevector-u8-ref bvec i)
                (rec (+ i 1)))))))

(define (integer->hex-string n)
  (let ((s (number->string n 16)))
    (if (even? (string-length s)) s (string-append "0" s))))

(define (bytevector->hex-string bytestring)
  (assume (bytevector? bytestring))
  (let ((len (bytevector-length bytestring)))
    (bytevector-fold-right (lambda (b s)
                             (string-append (integer->hex-string b) s))
                           (string)
                           bytestring)))
