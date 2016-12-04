(define argl (command-line))

(define expr-string
  (if (equal? (cadr argl) "-e")
    (caddr argl)
    (read-string-all (open-input-file (cadr argl)))))

(define expr-source
  (if (equal? (cadr argl) "-e")
    (caddr argl)
    (cadr argl)))

(define expr-stx
  (parse/syntax-all expr-source expr-string))

(write
  (##intr#eval
    (expand-toplevel (make-env)
      (datum->syntax expr-stx (cons (syntax begin) (syntax-e expr-stx))))))
(newline)
