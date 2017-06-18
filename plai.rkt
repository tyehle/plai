#lang plai-typed
(require plai-typed/s-exp-match)

(define-type ArithS
  [numS (n : number)]
  [plusS (l : ArithS) (r : ArithS)]
  [minusS (l : ArithS) (r : ArithS)]
  [multS (l : ArithS) (r : ArithS)])

(define-type ArithC
  [numC (n : number)]
  [plusC (l : ArithC) (r : ArithC)]
  [multC (l : ArithC) (r : ArithC)])


;;;;; Parser ;;;;;

(define (parse (s : s-expression)) : ArithS
  (cond
    [(s-exp-match? `NUMBER s) (numS (s-exp->number s))]
    [(s-exp-match? '{+ ANY ANY} s)
     (plusS (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? '{- ANY ANY} s)
     (minusS (parse (second (s-exp->list s)))
             (parse (third (s-exp->list s))))]
    [(s-exp-match? '{* ANY ANY} s)
     (multS (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [else (error 'parse "invalid input")]))


;;;;; Desugarer ;;;;;

(define (desugar (exp : ArithS)) : ArithC
  (type-case ArithS exp
    [numS (n) (numC n)]
    [plusS (l r) (plusC (desugar l) (desugar r))]
    [multS (l r) (multC (desugar l) (desugar r))]
    [minusS (l r) (plusC (desugar l) (multC (numC -1) (desugar r)))]))

;;;;; Interpreter ;;;;;

(define (interp (exp : ArithC)) : number
  (type-case ArithC exp
    [numC (n) n]
    [plusC (l r) (+ (interp l) (interp r))]
    [multC (l r) (* (interp l) (interp r))]))