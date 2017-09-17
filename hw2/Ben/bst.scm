(#%require rackunit)

(define null-bst?
  (lambda (bst)
    (if (null? bst)
        #t
        #f)))

(define null-bst
  (lambda ()
     '()))

(define entry
  (lambda (bst)
    (if (bst? bst)
        (car bst)
        #f)))

(define left
  (lambda (bst)
    (if (bst? bst)
        (car (cdr bst))
        #f)))

(define right
  (lambda (bst)
    (if (bst? bst)
        (car (cdr (cdr bst)))
        #f)))

(define bst?
  (lambda (bst)
    ;(display bst)
    ;(newline)
    (if (null-bst? bst)
        #t
        (begin
          (if (equal? (length bst) 3)
              (begin
                (bst? (car (cdr bst)))
                (bst? (car (cdr (cdr bst)))))
              #f)))))

(check-equal? (null-bst? (null-bst)) #t)
(check-equal? (null-bst) '())
(check-equal? (bst? '(5 (4 () ()) (6 () ()))) #t)
(check-equal? (entry '(5 (4 () ()) (6 () ()))) 5)
(check-equal? (left '(5 (4 () ()) (6 () ()))) '(4 () ()))
(check-equal? (right '(5 (4 () ()) (6 () ()))) '(6 () ()))
(check-equal? (entry '(5 (4 () ()) (6 ()))) #f)
