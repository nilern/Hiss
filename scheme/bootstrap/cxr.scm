(define caar
  (lambda (ls)
    (car (car ls))))

(define cadr
  (lambda (ls)
    (car (cdr ls))))

(define cddr
  (lambda (ls)
    (cdr (cdr ls))))

(define caddr
  (lambda (ls)
    (car (cdr (cdr ls)))))
