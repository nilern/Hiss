(define make-env
  (lambda ()
    '()))

(define env-ref
  (lambda (env name)
    ((lambda (pair)
       (if pair
         (cdr pair)
         #f))
     (assq name env))))

(define env-set!
  (lambda (env name val)
    ((lambda (pair)
       (if pair
         (set-cdr! pair val)
         (error "unbound variable" name)))
     (assq name env))))

(define env-def
  (lambda (env name val)
    (cons (cons name val) env)))

(define env-dissoc
  (lambda (env name)
    (if (null? env)
      env
      (if (eq? (caar env) name)
        (cdr env)
        (cons (car env) (env-dissoc (cdr env) name))))))
