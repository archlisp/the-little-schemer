;;; Toys

(define (atom? x)
  (and (not (pair? x)) (not (null? x))))

;;; Do it, Do It Again, and Again, and Again ...

(define (lat? l)
  (cond
   ((null? l) #t)
   ((atom? (car l)) (lat? (cdr l)))
   (else #f)))

(define (member? a lat)
  (cond
   ((null? lat) #f)
   (else (or (eq? (car lat) a)
             (member? a (cdr lat))))))

;;; Cons the Magnificent

(define (rember a lat)
  (cond
   ((null? lat) (quote ()))
   ((eq? (car lat) a) (cdr lat))
   (else (cons (car lat)
               (rember a (cdr lat))))))

(define (tco-rember a lat)
  (let loop ((lat lat) (seen '()))
    (cond
     ((null? lat) (quote ()))
     ((eq? (car lat) a) (append (reverse seen) (cdr lat)))
     (else (loop (cdr lat) (cons (car lat) seen))))))
