(#%require rackunit)

(define null-bst
  (lambda ()
    '()))

(define entry
  (lambda (bst)
    (if (and (bst? bst)
             (not (null-bst? bst)))
        (car bst)
        #f)))

(define left
  (lambda (bst)
    (if (and (bst? bst)
             (not (null-bst? bst)))
        (cadr bst)
        #f)))

(define right
  (lambda (bst)
    (if (and (bst? bst)
             (not (null-bst? bst)))
        (cadr (cdr bst))
        #f)))

(define make-bst
  (lambda (elt left right)
    (if (bst? (list elt left right))
        (list elt left right)
        #f)))

(define null-bst?
  (lambda (bst)
    (if (null? bst)
        #t
        #f)))

(define bst?
  (lambda (bst)
    (if (bst-helper bst)
        #t
        #f)))

(define bst-helper
  (lambda (bst) 
    (cond ((null? bst) #t)
          ((or (not (pair? bst))
               (not (equal? (length bst) 3))
               (not (integer? (car bst)))
               (not (bst-helper (cadr bst)))
               (not (bst-helper (cadr (cdr bst))))
               (and (integer? (bst-helper (cadr bst)))
                    (>= (bst-helper (cadr bst))
                        (car bst)))
               (and (integer? (bst-helper (cadr (cdr bst))))
                    (<= (bst-helper (cadr (cdr bst)))
                        (car bst))))
           #f)
          (else (car bst)))))

(define member?
  (lambda (v bst)
    (cond ((null? bst) #f)
          ((equal? (car bst)
                   v) #t)
          ((> v
              (car bst)) (member? v
                                  (cadr (cdr bst))))
          (else (member? v
                         (cadr bst))))))

(define is-leaf?
  (lambda (bst)
    (if (and (integer? (car bst))
             (null? (cadr bst))
             (null? (cadr (cdr bst))))
        #t
        #f)))

(define preorder
  (lambda (bst)
    (cond ((is-leaf? bst) (list (car bst)))
          ((null? (cadr bst)) (cons (car bst)
                                    (preorder (cadr (cdr bst)))))
          ((null? (cadr (cdr bst))) (cons (car bst)
                                          (preorder (cadr bst))))
          (else (append (list (car bst))
                        (preorder (cadr bst))
                        (preorder (cadr (cdr bst))))))))

(check-equal? (null-bst? '()) #t)
(check-equal? (null-bst? '(0 () ())) #f)
(check-equal? (null-bst? (null-bst)) #t)

(check-equal? (bst? (null-bst)) #t)
(check-equal? (bst? '(5 () ())) #t)
(check-equal? (bst? '(5 (3 () ()) (6 () ()))) #t)
(check-equal? (bst? '(() ())) #f)
(check-equal? (bst? '(3)) #f)
(check-equal? (bst? '(() () ())) #f)
(check-equal? (bst? '((5) () ())) #f)
(check-equal? (bst? '(5 (6 () ()) ())) #f)
(check-equal? (bst? '(5 () (4 () ()))) #f)
(check-equal? (bst? '(5 (4 () ()) (6 () (7 () ())))) #t)

(check-equal? (entry (null-bst)) #f)
(check-equal? (entry '(5 () ())) 5)
(check-equal? (entry '(5 0 0)) #f)
(check-equal? (entry '(5 (4 () ()) (6 () (7 () ())))) 5)

(check-equal? (left '(5 () ())) '())
(check-equal? (left '(5 (4 (3 () ()) ()) (6 () ()))) '(4 (3 () ()) ()))
(check-equal? (left '(5 4 3)) #f)

(check-equal? (right '(5 () ())) '())
(check-equal? (right '(5 (4 (3 () ()) ()) (6 () ()))) '(6 () ()))
(check-equal? (right '(5 4 3)) #f)

(check-equal? (make-bst 5 '() '()) '(5 () ()))
(check-equal? (make-bst 5 '() '(4 ()())) #f)
(check-equal? (make-bst 5 4 6) #f)
(check-equal? (make-bst 5 '(4 () ()) '(6 () ())) '(5 (4 () ()) (6 () ())))
(check-equal? (make-bst 5 '(4 (q) ()) '(6 () ())) #f)

(check-equal? (member? 4 '(5 () ())) #f)
(check-equal? (member? 4 '(5 (4 () ()) ())) #t)

(check-equal? (preorder '(5 () ())) '(5))
(check-equal? (preorder '(5 (4 () ()) ())) '(5 4))
(check-equal? (preorder '(5 (4 () ()) (6 () ()))) '(5 4 6))
(check-equal? (preorder '(1 (2 (4 () ()) (5 () ())) (3 () ()))) '(1 2 4 5 3))