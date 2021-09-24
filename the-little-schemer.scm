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
     ((eq? (car lat) a) (set-cdr! seen (cdr lat)))
     (else (loop (cdr lat) (cons (car lat) seen))))))

(define (firsts l)
  (cond
   ((null? l) '())
   (else (cons (car (car l))
               (firsts (cdr l))))))

(define (insertR new old lat)
  (cond
   ((null? lat) '())
   ((eq? (car lat) old) (cons (car lat) (cons new (cdr lat))))
   (else (cons (car lat) (insertR new old (cdr lat))))))

(define (insertL new old lat)
  (cond
   ((null? lat) '())
   ((eq? (car lat) old) (cons new lat))
   (else (cons (car lat) (insertL new old (cdr lat))))))

(define (subst new old lat)
  (cond
   ((null? lat) '())
   ((eq? (car lat) old) (cons new (cdr lat)))
   (else (cons (car lat) (subst new old (cdr lat))))))

(define (subst2 new o1 o2 lat)
  (cond
   ((null? lat) '())
   ((or (eq? (car lat) o1)
        (eq? (car lat) o2))
    (cons new (cdr lat)))
   (else (cons (car lat) (subst2 new o1 o2 (cdr lat))))))

(define (multirember a lat)
  (cond
   [(null? lat) '()]
   [(eq? (car lat) a) (multirember a (cdr lat))]
   [else (cons (car lat) (multirember a (cdr lat)))]))

(define (multiinsertR new old lat)
  (cond
   [(null? lat) '()]
   [(eq? (car lat) old)
    (cons
     (car lat)
     (cons new (multiinsertR new old (cdr lat))))]
   [else (cons (car lat)
               (multiinsertR new old (cdr lat)))]))

(define (multiinsertL new old lat)
  (cond
   [(null? lat) '()]
   [(eq? (car lat) old)
    (cons new
          (cons old
                (multiinsertL new old (cdr lat))))]
   [else (cons (car lat)
               (multiinsertL new old (cdr lat)))]))
