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

(define (interp (exp : ExprC) (fds : (listof DefC))) : Value
  (type-case ExprC exp
    [boolC (b) (boolV b)]
    [numC (n) (numV n)]
    [idC (s) (error 'interp "unbound identifier")]
    [plusC (l r) (num-binop + (interp l fds) (interp r fds))]
    [multC (l r) (num-binop * (interp l fds) (interp r fds))]
    [zeroC (n) (let ([nv (interp n fds)])
                 (if (numV? nv)
                     (boolV (= 0 (numV-n nv)))
                     (error 'interp "not a number")))]
    [ifC (c t f)
         (let ([ci (interp c fds)])
           (cond
             [(boolV? ci) (if (boolV-b ci) (interp t fds) (interp f fds))]
             [else (error 'interp "can only test booleans")]))]
    [appC (name arg)
          (let ([fun (lookup name fds)])
            (interp (subst arg (funDefC-arg fun) (funDefC-body fun)) fds))]))

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

(define (lookup (s : symbol) (fds : (listof DefC))) : DefC
  (cond
    [(empty? fds) (error 'lookup "name not found")]
    [(symbol=? s (funDefC-name (first fds))) (first fds)]
    [else (lookup s (rest fds))]))