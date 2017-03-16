(define apply (lambda (f args) (##intr#apply f args)))

(define call-with-current-continuation (lambda (f) (##intr#call/cc f)))
(define call-with-values (lambda (f g) (##intr#call/vs f g)))
(define values (##intr#values))

(define eq? (lambda (a b) (##intr#eq? a b)))

(define eqv? (lambda (a b) (##intr#eqv? a b)))

(define equal? (lambda (a b) (##intr#equal? a b)))

(define + (lambda (a b) (##intr#add a b)))
(define - (lambda (a b) (##intr#sub a b)))
(define * (lambda (a b) (##intr#mul a b)))

(define < (lambda (a b) (##intr#lt a b)))

(define cons (lambda (head tail) (##intr#cons head tail)))

(define pair? (lambda (v) (##intr#pair? v)))

(define car (lambda (ls) (##intr#car ls)))

(define cdr (lambda (ls) (##intr#cdr ls)))

(define null? (lambda (v) (##intr#null? v)))

(define syntax-e (lambda (v) (##intr#stx-e v)))

(define datum->syntax (lambda (stx datum) (##intr#mk-stx stx datum)))

(define open-input-file (lambda (filename) (##intr#open/if filename)))

(define read-string-all (lambda (port) (##intr#read/s-all port)))

(define write (lambda (v) (##intr#write v)))

(define newline (lambda () (##intr#nl)))

(define command-line (lambda () (##intr#cline)))

(define parse/syntax-all (lambda (desc str) (##intr#parse/stx-all desc str)))
