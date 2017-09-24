(#%require rackunit)
;; returns a curried version of 2-parameter function f
(define curry2
  (lambda (f)
    (lambda (x)
      (lambda (y)
        (f x y)))))

;; returns an uncurried version of 2-parameter curried function f
(define uncurry2
  (lambda (f)
    (lambda (x y)
      ((f x) y))))

;; curried version of *
(define mult
  (curry2 *))

;; takes two 1-parameter functions f and g and returns
;; a function that is f composed with g 
(define compose
  (lambda (f g)
    (lambda (x)
      (f (g x)))))

;; takes a 1-parameter predicate predicate and returns
;; a predicate that is the negation of predicate
(define negate
  (lambda (predicate)
    (lambda (x)
      (not (predicate x)))))

;; returns x^2 for testing
(define square
  (lambda (x)
    (* x x)))

;; returns x + 2
(define add-2
  (lambda (x)
    (+ 2 x)))

; curry test
(check-equal? (((curry2 +) 52) 2) 54)

; mult test
(check-equal? ((mult 3) 4) 12)

; uncurry tests
(check-equal? ((uncurry2 mult) 52 2) 104)
(check-equal? ((uncurry2 (curry2 +)) 1000 -15.25) 984.75)

; compose tests
(check-equal? ((compose square square) 2) 16)
(check-equal? ((compose square add-2) 3) 25)
(check-equal? ((compose add-2 square) 3) 11)

; negate test
(check-equal? ((negate null?) '()) #f)