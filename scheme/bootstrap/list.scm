(define length
  (lambda (ls)
    (if (pair? ls)
      (+ (length (cdr ls)) 1)
      0)))

(define assq
  (lambda (obj alist)
    (if (pair? alist)
      (if (eq? obj (caar alist))
        (car alist)
        (assq obj (cdr alist)))
      #f)))
