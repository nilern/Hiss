(define eq? (lambda (a b) (##intr#eq? a b)))

(define cons (lambda (head tail) (##intr#cons head tail)))

(define pair? (lambda (v) (##intr#pair? v)))

(define car (lambda (ls) (##intr#car ls)))

(define cdr (lambda (ls) (##intr#cdr ls)))
