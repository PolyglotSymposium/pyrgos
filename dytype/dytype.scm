(import scheme)

(use srfi-1)

(define check
  (lambda (gamma expr type)
    (equal? type (synthesize gamma expr))))

(define synth-symbol
  (lambda (gamma expr)
    (let ((judgment (assq expr gamma))) (if judgment (cdr judgment)))))

(define synth-appl-1
  (lambda (gamma ftype arg)
    (cond [(and (integer? ftype) (> ftype 1)) (if (check gamma arg 1)
                                                  (- ftype 1))]
          [(pair? ftype) (if (check gamma arg (car ftype)) (cdr ftype))])))

(define variadic?
  (lambda (ftype) [and (pair? ftype) (eq? '* (car ftype))]))

(define cons-it-out
  (lambda (args)
    (foldr (lambda (x y) (list 'cons x y)) ''() args)))

(define type+
  (lambda (l r)
    (cond [(and (eq? 1 l) (integer? r)) (+ l r)]
          [(pair? r) (cons l r)])))

(define synth-variadic-appl
  (lambda (gamma ftype args)
    (synth-appl-1 gamma (type+ 1 (cdr ftype)) (cons-it-out args))))

(define synth-partial-appl
  (lambda (gamma ftype arg rest)
    (let [(t1 [synth-appl-1 gamma ftype arg])]
      (cond [(or [eq? (cond) t1] [eq? '() rest]) t1]
            (else [synth-appl- gamma t1 rest])))))

(define synth-appl-
  (lambda (gamma ftype args)
    (cond [(pair? args)
           (cond ([variadic? ftype] [synth-variadic-appl gamma ftype args])
                 (else (synth-partial-appl gamma ftype (car args) (cdr args))))])))

(define synth-appl
  (lambda (gamma func args)
    (synth-appl- gamma (synthesize gamma func) args)))

;; There is exactly one type of non-function, but infinitely may function types.
;; Therefore, our only hope of synthesizing a lambda is to try and see if it
;; will work if we assign the argument type 1 (unless I read up more on H-M and
;; better understand how to do this).
(define synth-lambda-1
  (lambda (gamma arg body)
    (if (symbol? arg)
        (type+ 1 (synthesize (cons (cons arg 1) gamma) body)))))

(define push-args-onto-body
  (lambda (args body)
    (if (eq? '() args)
        body
        (list 'lambda args body))))

(define synth-lambda
  (lambda (gamma args body)
    (if (pair? args)
        (synth-lambda-1 gamma
                        (car args)
                        (push-args-onto-body (cdr args) body)))))

(define synth-pair
  (lambda (gamma kar kdr)
    (cond [(eq? 'lambda kar) (if [and (list? kdr) (eq? 2 (length kdr))]
                                 (synth-lambda gamma (car kdr) (car (cdr kdr))))]
          ;; A quoted expression is pure data
          [(eq? 'quote kar) 1]
          (else (synth-appl gamma kar kdr))
          )))

(define (synth-if gamma cnd csq alt)
  (if (check gamma cnd 1)
    (let [(csq-type (synthesize gamma csq))]
      (if (check gamma alt csq-type)
        csq-type))))

(define safe-eval
  (lambda (expr)
    (condition-case (eval expr) [_ () expr])))

(define synthesize
  (lambda (gamma expr)
    (cond ((number? expr) 1)
          ((string? expr) 1)
          ((symbol? expr) (synth-symbol gamma expr))
	  ([and (pair? expr) (eq? 4 (length expr)) (eq? 'if (car expr))]
	     (synth-if gamma (cadr expr) (caddr expr) (cadddr expr)))
          ((pair? expr) (synth-pair gamma (car expr) (cdr expr))))))

(define repl-with
  (lambda (gamma)
    (display "2-t> ")
    (let [(x [read])]
      (cond [(equal? x ',q) '()]
            (else (let* ((t (synthesize gamma x))
                         (x- (if (eq? (cond) t) x (safe-eval x))))
                    (display (format "~a : ~a" x- t))
                    (newline)
                    (repl-with gamma)))))))

(define prelude
  '(
    (+ . (* . 1))
    (modulo . 3)
    (number->string . 2)
    (cons . 3)
    (list . (* . 1))
    (iota . 4)
    (map . (2 . 2))
    (eq? . 3)
    ))

(define repl
  (lambda ()
    (display ">> dytype\n")
    (repl-with prelude)
    (display "dytype >>\n")
  ))

(repl)
