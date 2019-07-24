(import scheme)

;; TODO
;; * variadic lambdas of types like (1 * . 1)
;; * (define (x args)) syntax
;; * totality checker/primitive (mutual) recursion
;; * total macro system
;; * alternate code backends (e.g. Guile, Chez, Racket)
;; * make the compiler code compile across many Schemes
;; * self-host (type-check this code)
;; * make partial application not just type-check but actually work
;; * how do we want to handle effects?
;; * support `:` in the backend
;; * implement checking for lambdas that cannot be synthesized

(use srfi-1)
(use ports)

(define (check gamma expr type) (equal? type (synthesize gamma expr)))

(define (synth-symbol gamma expr)
  (let ((judgment (assq expr gamma))) (if judgment (cdr judgment))))

(define (synth-appl-1 gamma ftype arg)
  (cond [(and (integer? ftype) (> ftype 1)) (if (check gamma arg 1)
                                                (- ftype 1))]
        [(pair? ftype) (if (check gamma arg (car ftype)) (cdr ftype))]))

(define (variadic? ftype) [and (pair? ftype) (eq? '* (car ftype))])

(define (cons-it-out args) (foldr (lambda (x y) (list 'cons x y)) ''() args))

(define (type+ l r)
  (cond [(pair? r) (cons l r)]
        [(integer? r) (cond [(eq? 1 l) (+ l r)]
                            [(eq? '* l) (cons l r)])]
        ))

(define (synth-variadic-appl gamma ftype args)
  (synth-appl-1 gamma (type+ 1 (cdr ftype)) (cons-it-out args)))

(define (synth-partial-appl gamma ftype arg rest)
  (let [(t1 [synth-appl-1 gamma ftype arg])]
    (cond [(or [eq? (cond) t1] [eq? '() rest]) t1]
          (else [synth-appl- gamma t1 rest]))))

(define (synth-appl- gamma ftype args)
  (cond [(pair? args)
         (cond ([variadic? ftype] [synth-variadic-appl gamma ftype args])
               (else (synth-partial-appl gamma ftype (car args) (cdr args))))]
        ))

(define (synth-appl gamma func args)
  (synth-appl- gamma (synthesize gamma func) args))

;; There is exactly one type of non-function, but infinitely may function types.
;; Therefore, our only hope of synthesizing a lambda is to try and see if it
;; will work if we assign the argument type 1 (unless I read up more on H-M and
;; better understand how to do this); or, recognize it as variadic syntactically.
(define (synth-lambda-1 gamma arg body)
  (if (symbol? arg) (type+ 1 (synthesize (cons (cons arg 1) gamma) body))))

(define (push-args-onto-body args body)
  (if (eq? '() args) body (list 'lambda args body)))

(define (synth-lambda- gamma args body)
  (cond [(pair? args) (synth-lambda-1 gamma
                                      (car args)
                                      (push-args-onto-body (cdr args) body))]
        [(symbol? args) (type+ '* (synthesize (cons (cons args 1) gamma)
                                              body))]
        ))

(define (synth-lambda gamma lexpr)
  (if (and (list? lexpr) (eq? 2 (length lexpr)))
      (synth-lambda- gamma (car lexpr) (cadr lexpr))))

(define (check-ann gamma ann)
  (if (and (list? ann) (eq? 2 (length ann)))
      (let [(expr (car ann))
            (type (cadr ann))]
        (if (check gamma expr type) type))))

;; Putting `if` into the type checker as a "good enough for now" approach to
;; conditionals.
;;
;; We're not really totally sure whether `if` or `cond` is the more appropriate
;; first conditional primitive. Also, recognizing that extending the type system
;; of each special form likely won't scale, we should revisit this when we've
;; gotten a better handle on macros or more special-form patterns have been
;; introduced.
(define (synth-if- gamma cnd csq opt-alt)
  (if (check gamma cnd 1)
      (let [(csq-type (synthesize gamma csq))]
        (cond [(and (null? opt-alt) (eq? 1 csq-type)) 1]
              [(and (eq? 1 (length opt-alt)) (check gamma (car opt-alt) csq-type)) csq-type]))))

(define (synth-if gamma ifexpr)
  (if (<= 2 (length ifexpr))
      (synth-if- gamma (car ifexpr) (cadr ifexpr) (cddr ifexpr))))

(define (synth-list gamma kar kdr)
  (cond
   [(eq? ': kar) (check-ann gamma kdr)]
   [(eq? 'if kar) (synth-if gamma kdr)]
   [(eq? 'lambda kar) (synth-lambda gamma kdr)]
   [(eq? 'quote kar) 1] ;; A quoted expression is pure data
   (else (synth-appl gamma kar kdr))
   ))

(define (safe-eval expr) (condition-case (eval expr) [_ () expr]))

(define (synthesize gamma expr)
  (cond ((number? expr) 1)
        ((string? expr) 1)
        ((boolean? expr) 1)
        ((symbol? expr) (synth-symbol gamma expr))
        ((and (list? expr) (pair? expr)) (synth-list gamma (car expr) (cdr expr)))
        ))

(define (do/define gamma topl)
  (if (and [list? topl] [eq? 2 (length topl)] [symbol? (car topl)])
      (let [[t (synthesize gamma (cadr topl))]]
        [list (cons (cons (car topl) t) gamma) t (car topl)])
      [list gamma (cond)]))

(define (toplevel gamma topl)
  (cond [(and (pair? topl) (eq? 'define (car topl))) (do/define gamma (cdr topl))]
        (else (list gamma (synthesize gamma topl)))))

(define (guarded-eval x t o)
  (let [(x- (if (eq? (cond) t) x (safe-eval x)))]
    (if (eq? 1 (length o)) (car o) x-)))

(define (run-and-print-with gamma x)
  (let* ((result (toplevel gamma x))
         (new-gamma (car result))
         (t (cadr result))
         (x- (guarded-eval x t (cddr result))))
    (display (format "~s : ~s" x- t))
    (newline)
    new-gamma))

(define (repl-with gamma)
  (display "2-t> ")
  (let [(x [read])]
    (cond [(equal? x ',q) '()]
          (else (repl-with (run-and-print-with gamma x))))))

;; Important: if we aren't careful with what we introduce here, we could
;; introduce unsoundness (namely, by including possibly non-terminating
;; functions).
(define prelude
  '((* . (* . 1)) (+ . (* . 1)) (- . (* . 1)) (/ . (* . 1))
    (assq . 3)
    (boolean? . 2)
    (caar . 2) (cadr . 2) (car . 2) (cdar . 2) (cddr . 2) (cdr . 2)
    (cons . 3)
    (eq? . 3) (equal? . 3)
    (integer? . 2)
    (iota . 4)
    (length . 2)
    (list . (* . 1)) (list? . 2)
    (map . (2 . 2))
    (modulo . 3)
    (null? . 1)
    (number->string . 2)
    (number? . 2)
    (pair? . 2)
    (string? . 2)
    (symbol? . 2)
    ))

(define (repl)
  (display ">> dytype\n")
  (repl-with prelude)
  (display "dytype >>\n")
  )

(if (null? (command-line-arguments))
  (repl)
  (run-and-print-with prelude (call-with-input-string (car (command-line-arguments)) read)))
