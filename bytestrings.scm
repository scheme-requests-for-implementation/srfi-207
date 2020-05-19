;;; Utility

(define (bytevector-index bvec pred)
  (let ((len (bytevector-length bvec)))
    (let lp ((i 0))
      (cond ((= i len) #f)
            ((pred (bytevector-u8-ref bvec i)) i)
            (else (lp (+ i 1)))))))

(define (bytevector-index-right bvec pred)
  (let lp ((i (- (bytevector-length bvec) 1)))
    (cond ((< i 0) #f)
          ((pred (bytevector-u8-ref bvec i)) i)
          (else (lp (- i 1))))))

(define (convert-argument x error-location)
  (cond ((and (integer? x) (<= 0 x) (<= x 255))
         (bytevector x))
        ((and (char? x) (char<=? #\null x) (char<=? x #\delete))
         (bytevector (char->integer x)))
        ((bytevector? x) x)
        ((string? x) (string->utf8 x))  ; TODO: ensure ASCII
        (else ; TODO: error type
         (error (string-append error-location ": invalid argument") x))))

;;; Constructor

;; TODO: Error handling and algorithmic improvement.

(define (bytestring . args)
  (let lp ((bs (bytevector)) (args args))
    (if (null? args)
        bs
        (lp (bytevector-append bs (convert-argument (car args) "bytestring"))
            (cdr args)))))

;;; Conversion

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

(define (bytevector->hex-string bstring)
  (assume (bytevector? bstring))
  (let ((len (bytevector-length bstring)))
    (bytevector-fold-right (lambda (b s)
                             (string-append (integer->hex-string b) s))
                           (string)
                           bstring)))

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
(define (bytestring->list bstring)
  (assume (bytevector? bstring))
  (list-tabulate (bytevector-length bstring)
                 (lambda (i) (bytevector-u8-ref bstring i))))

;;; Selection

(define (bytestring-pad bstring len char-or-u8)
  (assume (bytevector? bstring))  ; TODO: better type checks
  (assume (integer? len))
  (assume (or (char? char-or-u8) (integer? char-or-u8)))
  (let ((old-len (bytevector-length bstring)))
    (if (>= old-len len)
        bstring
        (bytevector-append (make-bytevector (- len old-len) char-or-u8)
                           bstring))))

(define (bytestring-pad-right bstring len char-or-u8)
  (assume (bytevector? bstring))  ; TODO: better type checks
  (assume (integer? len))
  (assume (or (char? char-or-u8) (integer? char-or-u8)))
  (let ((old-len (bytevector-length bstring)))
    (if (>= old-len len)
        bstring
        (bytevector-append bstring
                           (make-bytevector (- len old-len) char-or-u8)))))

(define (bytestring-trim bstring pred)
  (let ((new-start (bytevector-index bstring (lambda (b) (not (pred b))))))
    (if new-start (bytevector-copy bstring new-start) (bytevector))))

(define (bytestring-trim-right bstring pred)
  (let ((new-end (+ 1 (bytevector-index-right bstring
                                              (lambda (b) (not (pred b)))))))
    (if new-end (bytevector-copy bstring 0 new-end) (bytevector))))

(define (bytestring-trim-both bstring pred)
  (let ((new-start (bytevector-index bstring (lambda (b) (not (pred b)))))
        (new-end (+ 1 (bytevector-index-right bstring
                                              (lambda (b) (not (pred b)))))))
    (bytevector-copy bstring
                     (or new-start 0)
                     (or new-end (bytevector-length bstring)))))
