#lang plai-typed
(require plai-typed/s-exp-match)

(define-type ExprC
  [boolC (b : boolean)]
  [numC (n : number)]
  [idC (s : symbol)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [zeroC (n : ExprC)]
  [ifC (c : ExprC) (t : ExprC) (f : ExprC)]
  [consC (head : ExprC) (tail : ExprC)]
  [emptyC]
  [headC (l : ExprC)]
  [tailC (l : ExprC)]
  [appC (fun : ExprC) (arg : ExprC)]
  [lamC (arg : symbol) (body : ExprC)])

(define-type Value
  [numV (n : number)]
  [boolV (b : boolean)]
  [consV (h : Value) (t : Value)]
  [emptyV]
  [closV (arg : symbol) (body : ExprC) (env : Env)])

(define-type Binding
  [bind (name : symbol) (val : Value)])

(define-type-alias Env (listof Binding))
(define mt-env empty)
(define extend-env cons)

(define (run input) (interp (parse input) mt-env))


;;;;; Parser ;;;;;

(define (parse (s : s-expression)) : ExprC
  (cond
    [(s-exp-match? `true s) (boolC #t)]
    [(s-exp-match? `false s) (boolC #f)]
    [(s-exp-match? `NUMBER s) (numC (s-exp->number s))]
    [(s-exp-match? '{+ ANY ANY} s)
     (plusC (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? '{* ANY ANY} s)
     (multC (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? '{zero? ANY} s)
     (zeroC (parse (second (s-exp->list s))))]
    [(s-exp-match? '{if ANY ANY ANY} s)
     (ifC (parse (second (s-exp->list s)))
          (parse (third (s-exp->list s)))
          (parse (fourth (s-exp->list s))))]
    [(s-exp-match? '{cons ANY ANY} s)
     (consC (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? `empty s)
     (emptyC)]
    [(s-exp-match? '{head ANY} s)
     (headC (parse (second (s-exp->list s))))]
    [(s-exp-match? '{tail ANY} s)
     (tailC (parse (second (s-exp->list s))))]
    [(s-exp-match? '{lambda {SYMBOL} ANY} s)
     (lamC (s-exp->symbol (first (s-exp->list (second (s-exp->list s)))))
           (parse (third (s-exp->list s))))]
    [(s-exp-match? '{ANY ANY} s)
     (appC (parse (first (s-exp->list s)))
           (parse (second (s-exp->list s))))]
    [(s-exp-match? `SYMBOL s)
     (idC (s-exp->symbol s))]
    [else (error 'parse "invalid input")]))


;;;;; Interpreter ;;;;;

(define (interp (exp : ExprC) (env : Env)) : Value
  (type-case ExprC exp
    [boolC (b) (boolV b)]
    [numC (n) (numV n)]
    [idC (s) (lookup s env)]
    [plusC (l r) (num-binop + (interp l env) (interp r env))]
    [multC (l r) (num-binop * (interp l env) (interp r env))]
    [zeroC (ne) (let ([nv (interp ne env)])
                  (type-case Value nv
                    [numV (n) (boolV (= 0 n))]
                    [else (error 'interp "not a number")]))]
    [ifC (c t f)
         (let ([ci (interp c env)])
           (cond
             [(boolV? ci) (if (boolV-b ci) (interp t env) (interp f env))]
             [else (error 'interp "can only test booleans")]))]
    [consC (h t)
           (consV (interp h env) (interp t env))]
    [emptyC ()
            (emptyV)]
    [headC (l)
           (type-case Value (interp l env)
             [consV (h t) h]
             [else (error 'interp "not a cons")])]
    [tailC (l)
           (type-case Value (interp l env)
             [consV (h t) t]
             [else (error 'interp "not a cons")])]
    [appC (fun arg)
          (type-case Value (interp fun env)
            [closV (arg-name body clos-env)
                   (interp body
                           (extend-env (bind arg-name (interp arg env)) clos-env))]
            [else (error 'inter "not a function")])]
    [lamC (arg body)
          (closV arg body env)]))

(define (lookup (name : symbol) (env : Env)) : Value
  (cond
    [(empty? env) (error 'lookup "name not found")]
    [(symbol=? name (bind-name (first env))) (bind-val (first env))]
    [else (lookup name (rest env))]))

(define (num-binop (op : (number number -> number)) (l : Value) (r : Value)) : Value
  (cond
    [(and (numV? l) (numV? r))
     (numV (op (numV-n l) (numV-n r)))]
    [else (error 'num-binop "one argument not a number")]))