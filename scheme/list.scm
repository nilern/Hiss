(define length
  (lambda (ls)
    (if (pair? ls)
      (+ (length (cdr ls)) 1)
      0)))
