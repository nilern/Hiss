;(define-syntax let
;  (lambda (stx)
;    (let* ((sexp (syntax-e stx))
;           (bindings (syntax-e (cadr sexp)))
;           (body (cddr sexp))
;           (formals (map (comp car syntax-e) bindings))
;           (args (map (comp cadr syntax-e) bindings)))
;      (datum->syntax stx
;        (list*
;          (datum->syntax stx
;            (list* (syntax lambda) (datum->syntax (cadr sexp) formals)
;              body))
;          args)))))
(define-syntax let
  (lambda (stx)
    ((lambda (sexp)
       (datum->syntax stx
         (list*
           (datum->syntax stx
             (list* (syntax lambda)
                    (datum->syntax (cadr sexp)
                                   (map (lambda (stx) (car (syntax-e stx)))
                                        (syntax-e (cadr sexp))))
                    (cddr sexp)))
          (map (lambda (stx) (cadr (syntax-e stx)))
               (syntax-e (cadr sexp))))))
       (syntax-e stx))))

(let ((a 3)
      (b 5))
  (list a b))
