(define (cddr s)
  (cdr (cdr s)))

(define (cadr s)
  (car (cdr s))
)

(define (caddr s)
  (car (cddr s))
)


(define (sign num)
  (cond
  ((< num 0) -1)
  ((= num 0) 0)
  (else 1)
  )
)


(define (square x) (* x x))

(define (pow x y)
  (cond
  ((= y 0) 1)
  ((= 0 (modulo y 2)) (square (pow x (quotient y 2))))
  (else (* x (square (pow x (quotient (- y 1) 2)))))
  )
)


(define (unique s)
  (if (null? s) 
    nil
    (cons (car s) (filter (lambda (x) (not (eq? x (car s)))) (unique (cdr s))))
  )
)


(define (replicate x n)
  (if(= n 0)
    nil
    (if (= n 1)
      (list x)
      (if (= (modulo n 2) 0)
        (append (replicate x (quotient n 2)) (replicate x (quotient n 2)))
        (append (replicate x (quotient (+ n 1) 2)) (replicate x (quotient (- n 1) 2)))
      )
    )
  )  
)


(define (accumulate combiner start n term)
  (if (= n 0)
    start
    (combiner (term n) (accumulate combiner start (- n 1) term))
  )
)


(define (accumulate-tail combiner start n term)
  (define (accumulate-tree cmb left right term)
    (define mid (quotient (+ left right) 2))
    (cond
      ((= (- right left) 1) (term left))
      ((= (- right mid) 1) (cmb (term left) (term mid)))
      (else (cmb (term mid) (cmb (accumulate-tree cmb left mid term) (accumulate-tree cmb (+ mid 1) right term))))
    )
  )
  (if (= n 0)
    start
    (combiner start (accumulate-tree combiner 1 (+ n 1) term))
  )   
)


(define-macro (list-of map-expr for var in lst if filter-expr)
  `(map (lambda (,var) ,map-expr) (filter (lambda (,var) ,filter-expr) ,lst))
)

