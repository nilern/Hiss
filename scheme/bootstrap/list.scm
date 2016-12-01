(define list (lambda vs vs))

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

(define dissq*
  (lambda (alist keys)
    (if (pair? alist)
      (if (memq (caar alist) keys)
        (dissq* (cdr alist) keys)
        (cons (car alist) (dissq* (cdr alist) keys)))
      alist)))

(define map
  (lambda (f ls)
    (if (null? ls)
      ls
      (if (pair? ls)
        (cons (f (car ls)) (map f (cdr ls)))
        (error "map: not a list" ls)))))
