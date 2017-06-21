#lang plai-typed
(require plai-typed/s-exp-match)

(define-type DefC
  [funDefC (name : symbol) (arg : symbol) (body : ExprC)])

(define-type ExprC
  [boolC (b : boolean)]
  [numC (n : number)]
  [idC (s : symbol)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [zeroC (n : ExprC)]
  [ifC (c : ExprC) (t : ExprC) (f : ExprC)]
  [appC (fun : symbol) (arg : ExprC)])

(define-type Value
  [numV (n : number)]
  [boolV (b : boolean)])

(define-type Binding
  [bind (name : symbol) (val : Value)])

(define-type-alias Env (listof Binding))
(define mt-env empty)
(define extend-env cons)


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
    [(s-exp-match? '{ANY ANY} s)
     (appC (s-exp->symbol (first (s-exp->list s)))
           (parse (second (s-exp->list s))))]
    [(s-exp-match? `ANY s)
     (idC (s-exp->symbol s))]
    [else (error 'parse "invalid input")]))


;;;;; Interpreter ;;;;;

(define (interp (exp : ExprC) (env : Env) (fds : (listof DefC))) : Value
  (type-case ExprC exp
    [boolC (b) (boolV b)]
    [numC (n) (numV n)]
    [idC (s) (lookup s env)]
    [plusC (l r) (num-binop + (interp l env fds) (interp r env fds))]
    [multC (l r) (num-binop * (interp l env fds) (interp r env fds))]
    [zeroC (n) (let ([nv (interp n env fds)])
                 (if (numV? nv)
                     (boolV (= 0 (numV-n nv)))
                     (error 'interp "not a number")))]
    [ifC (c t f)
         (let ([ci (interp c env fds)])
           (cond
             [(boolV? ci) (if (boolV-b ci) (interp t env fds) (interp f env fds))]
             [else (error 'interp "can only test booleans")]))]
    [appC (name arg)
          (type-case DefC (lookup-def name fds)
            [funDefC (_ arg-name body)
                     (interp body
                             (extend-env (bind arg-name (interp arg env fds)) mt-env)
                             fds)])]))

(define (lookup (name : symbol) (env : Env)) : Value
  (cond
    [(empty? env) (error 'lookup "name not found")]
    [(symbol=? name (bind-name (first env))) (bind-val (first env))]
    [else (lookup name (rest env))]))

(define (subst (what : ExprC) (for : symbol) (in : ExprC)) : ExprC
  (type-case ExprC in
    [boolC (_) in]
    [numC (_) in]
    [idC (s) (if (symbol=? s for)
                 what
                 in)]
    [plusC (l r) (plusC (subst what for l) (subst what for r))]
    [multC (l r) (multC (subst what for l) (subst what for r))]
    [zeroC (n) (zeroC (subst what for n))]
    [ifC (c t f) (ifC (subst what for c)
                      (subst what for t)
                      (subst what for f))]
    [appC (fun arg) (appC fun (subst what for arg))]))

(define (num-binop (op : (number number -> number)) (l : Value) (r : Value)) : Value
  (cond
    [(and (numV? l) (numV? r))
     (numV (op (numV-n l) (numV-n r)))]
    [else (error 'num-binop "one argument not a number")]))

(define (lookup-def (s : symbol) (fds : (listof DefC))) : DefC
  (cond
    [(empty? fds) (error 'lookup-def "name not found")]
    [(symbol=? s (funDefC-name (first fds))) (first fds)]
    [else (lookup-def s (rest fds))]))