(define call-with-values (lambda (f g) (##intr#call/vs f g)))

(define values (##intr#values))

(define eq? (lambda (a b) (##intr#eq? a b)))

(define cons (lambda (head tail) (##intr#cons head tail)))

(define pair? (lambda (v) (##intr#pair? v)))

(define car (lambda (ls) (##intr#car ls)))

(define cdr (lambda (ls) (##intr#cdr ls)))

(define null? (lambda (v) (##intr#null? v)))

(define syntax-e (lambda (v) (##intr#stx-e v)))

(define datum->syntax (lambda (stx datum) (##intr#mk-stx stx datum)))
