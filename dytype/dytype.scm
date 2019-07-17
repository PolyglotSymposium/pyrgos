(module dytype (check synthesize repl)
  (import scheme)
  (define check
    (lambda (gamma expr type)
      (equal? type (synthesize gamma expr))
    )
  )
  (define synth-symbol
    (lambda (gamma expr)
      (let ((judgment (assq expr gamma))) (if judgment (cdr judgment) #f))
    )
  )
  (define func-type?
    (lambda (type)
      (if (and type (list? type) (eq? '-> (car type)) (list? (cdr type))) (cdr type) #f)
    )
  )
  (define synth-appl
    (lambda (gamma expr)
      (let* ((type (synthesize gamma (car expr)))
             (i-and-o (func-type? type))
            )
        (if (and i-and-o (check gamma (cdr expr) (cdr i-and-o)))
            (cdr i-and-o)
            #f
        )
      )
    )
  )
  (define synthesize
    (lambda (gamma expr)
      (cond ((number? expr) '*)
            ((symbol? expr) (synth-symbol gamma expr))
            ((pair? expr) (cond ((eq? 'lambda (car expr)) #f)
                                (else (synth-appl gamma expr))
                          )
            ) (else #f)
      )
    )
  )

  (define type+
    (lambda (l r)
      (if (and (eq? 1 l) (integer? r))
          (+ l r)
          (cons l r))))

  (define condense-type
    (lambda (type)
      (cond ((pair? type) (type+ (condense-type (car type))
                                 (condense-type (cdr type))))
            [(eq? '* type) 1])))

  (define repl-with
    (lambda (gamma)
      (display "2-t> ")
      (let [(x [read])]
        (cond [(equal? x ',q) '()]
              (else (display (condense-type (synthesize gamma x)))
                    (newline)
                    (repl-with gamma))
        )
      )
    )
  )

  (define repl
    (lambda ()
      (display ">> dytype\n")
      (repl-with '((+ (* *))))
      (display "dytype >>\n")
    )
  )
)
