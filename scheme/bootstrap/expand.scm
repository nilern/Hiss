(define extract-formals
  (lambda (fstx)
    ((lambda (formals)
       (if (pair? formals)
         (cons (syntax-e (car formals))
               (extract-formals (datum->syntax fstx (cdr formals))))
         (if (null? formals)
           formals
           (if (identifier? formals)
             (syntax-e formals)
             (error "invalid formals" fstx)))))
     (syntax-e fstx))))

;;;;

;(define expand-once
;  (lambda (expanders stx)
;    (let ((sexp (syntax-e stx)))
;      (if (pair? sexp)
;        (let ((expander (env-ref (syntax-e (car sexp)) expanders)))
;          (if expander
;            (expander stx)
;            stx))
;        stx))))
(define expand-once
  (lambda (expanders stx)
    ((lambda (sexp)
       (if (pair? sexp)
         ((lambda (expander)
            (if expander
              (expander stx)
              stx))
          (env-ref expanders (syntax-e (car sexp))))
         stx))
     (syntax-e stx))))

;(define expand
;  (lambda (expanders stx)
;    (let loop ((stx stx))
;      (let ((stx* (expand-once expanders stx)))
;        (if (eq? stx* stx)
;          stx
;          (loop stx*))))))
(define expand
  (lambda (expanders stx)
    ((lambda (loop)
       (begin
         (set! loop (lambda (stx)
                      ((lambda (stx*)
                         (if (eq? stx* stx)
                           stx
                           (loop stx*)))
                       (expand-once expanders stx))))
         (loop stx)))
     (begin))))

;;;;

;(define (expand-lambda expanders stx)
;  (let ((sexp (syntax-e stx)))
;    (values
;      (datum->syntax stx
;        (list (car sexp) (cadr sexp)
;          (expand-all #f
;            (dissq* expanders (extract-formals (cadr sexp)))
;            (caddr sexp))))
;      expanders)))
(define expand-lambda
  (lambda (expanders stx)
    ((lambda (sexp)
       (values
         (datum->syntax stx
           (list (car sexp) (cadr sexp)
             (expand-all #f
               (dissq* expanders (extract-formals (cadr sexp)))
               (caddr sexp))))
         expanders))
     (syntax-e stx))))

;(define (expand-define-syntax toplevel? expanders stx)
;  (let ((sexp (syntax-e stx)))
;    (if toplevel?
;      (let ((expander (##intr#compile (expand-all #f expanders (caddr sexp)))))
;        (values (datum->syntax stx (list (datum->syntax stx 'begin)))
;                (env-def expanders (syntax-e (cadr sexp)) expander)))
;      (error "define-syntax found below toplevel"))))
(define expand-define-syntax
  (lambda (toplevel? expanders stx)
    ((lambda (sexp)
       (if toplevel?
         ((lambda (expander)
           (values (datum->syntax stx (list (datum->syntax stx 'begin)))
                   (env-def expanders (syntax-e (cadr sexp)) expander)))
          (##intr#eval (expand-all #f expanders (caddr sexp))))
         (error "define-syntax found below toplevel")))
     (syntax-e stx))))

;(define expand-all
;  (lambda (toplevel? expanders stx)
;    (let expand-all* ((toplevel? toplevel?) (stx stx))
;      (let* ((stx* (expand expanders stx))
;             (sexp (syntax-e stx*)))
;        (if (pair? sexp)
;          (case (syntax-e (car sexp))
;            ((quote) (values stx* expanders))
;            ((lambda) (expand-lambda expanders stx*))
;            ((define-syntax) (expand-define-syntax toplevel? expanders stx*))
;            (else (values (datum->syntax stx*
;                            (map (lambda (stx) (expand-all* #f stx)) sexp))
;                          expanders)))
;          (values stx* expanders))))))
(define expand-all
  (lambda (toplevel? expanders stx)
    ((lambda (expand-all*)
       (begin
         (set! expand-all*
               (lambda (toplevel? stx)
                 ((lambda (stx*)
                    ((lambda (sexp)
                       (if (pair? sexp)
                         ((lambda (op-sexp)
                            (if (eq? op-sexp 'quote)
                              (values stx* expanders)
                               (if (eq? op-sexp 'lambda)
                                 (expand-lambda expanders stx*)
                                 (if (eq? op-sexp 'define-syntax)
                                   (expand-define-syntax toplevel? expanders stx*)
                                   (values
                                     (datum->syntax stx*
                                       (map (lambda (stx) (expand-all* #f stx)) sexp))
                                     expanders)))))
                           (syntax-e (car sexp)))
                          (values stx* expanders)))
                    (syntax-e stx*)))
                 (expand expanders stx))))
         (expand-all* toplevel? stx)))
     (begin))))

;(define expand-toplevel
;  (lambda (expanders stx)
;    (letrec ((expand-toplevel*
;              (lambda (expanders stxs)
;                (if (null? stxs)
;                  stxs
;                  (let-values (((stx* expanders*)
;                                (expand-all #t expanders (car stxs))))
;                    (cons stx* (expand-toplevel* expanders* (cdr stxs))))))))
;      (datum->syntax stx (expand-toplevel* expanders (syntax-e stx))))))
(define expand-toplevel
  (lambda (expanders stx)
    ((lambda (expand-toplevel*)
       (begin
         (set! expand-toplevel*
               (lambda (expanders stxs)
                 (if (null? stxs)
                   stxs
                   (call-with-values
                     (lambda () (expand-all #t expanders (car stxs)))
                     (lambda (stx* expanders*)
                       (cons stx* (expand-toplevel* expanders* (cdr stxs))))))))
         (datum->syntax stx (expand-toplevel* expanders (syntax-e stx)))))
     (begin))))
