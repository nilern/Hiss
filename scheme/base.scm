(define values
  (lambda vs
    (call/cc (lambda (k) (apply k vs)))))
