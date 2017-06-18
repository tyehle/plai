#lang plai-typed
(require plai-typed/s-exp-match)

(define-type ArithS
  [boolS (b : boolean)]
  [numS (n : number)]
  [plusS (l : ArithS) (r : ArithS)]
  [minusS (l : ArithS) (r : ArithS)]
  [multS (l : ArithS) (r : ArithS)])

(define-type ArithC
  [boolC (b : boolean)]
  [numC (n : number)]
  [plusC (l : ArithC) (r : ArithC)]
  [multC (l : ArithC) (r : ArithC)])

(define-type Value
  [numV (n : number)]
  [boolV (b : boolean)])


;;;;; Parser ;;;;;

(define (parse (s : s-expression)) : ArithS
  (cond
    [(s-exp-match? '#t s) (boolS #t)]
    [(s-exp-match? '#f s) (boolS #f)]
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
    [boolS (b) (boolC b)]
    [numS (n) (numC n)]
    [plusS (l r) (plusC (desugar l) (desugar r))]
    [multS (l r) (multC (desugar l) (desugar r))]
    [minusS (l r) (plusC (desugar l) (multC (numC -1) (desugar r)))]))

;;;;; Interpreter ;;;;;

(define (interp (exp : ArithC)) : Value
  (type-case ArithC exp
    [boolC (b) (boolV b)]
    [numC (n) (numV n)]
    [plusC (l r) (num-binop + (interp l) (interp r))]
    [multC (l r) (num-binop * (interp l) (interp r))]))

(define (num-binop (op : (number number -> number)) (l : Value) (r : Value)) : Value
  (cond
    [(and (numV? l) (numV? r))
     (numV (op (numV-n l) (numV-n r)))]
    [else (error 'num-binop "one argument not a number")]))