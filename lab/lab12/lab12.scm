
(define-macro (def func args body)
    `(define ,(cons func args) ,body))


(define (map-stream f s)
    (if (null? s)
    	nil
    	(cons-stream (f (car s)) (map-stream f (cdr-stream s)))))

(define (n-begin-ints n) (cons-stream n (n-begin-ints (+ n 1))))
(define positive-ints (n-begin-ints 1))

(define all-three-multiples
  (map-stream (lambda (x) (* 3 x)) positive-ints)
)


(define (compose-all funcs)
  (define (apply remain-funcs value) 
    (if(null? remain-funcs) 
      value
      (apply (cdr remain-funcs) ((car remain-funcs) value))))
  (lambda (x) (apply funcs x))
)


(define (partial-sums stream)
  (define (helper start strm)
    (if(null? strm)
      nil
      (cons-stream (+ start (car strm)) (helper (+ start (car strm)) (cdr-stream strm)))))
  (helper 0 stream)
)

