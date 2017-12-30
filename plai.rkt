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
  [appC (fun : ExprC) (arg : ExprC)]
  [lamC (arg : symbol) (body : ExprC)]
  [boxC (arg : ExprC)]
  [unboxC (arg : ExprC)]
  [setboxC (b : ExprC) (v : ExprC)]
  [seqC (b1 : ExprC) (b2 : ExprC)])

(define-type Value
  [numV (n : number)]
  [boolV (b : boolean)]
  [closV (arg : symbol) (body : ExprC) (env : Env)]
  [boxV (loc : Location)])

(define-type-alias Location number)

(define-type Binding
  [bind (name : symbol) (loc : Location)])

(define-type-alias Env (listof Binding))
(define mt-env empty)
(define extend-env cons)

(define-type Storage
  [cell (loc : Location) (val : Value)])

(define-type-alias Store (listof Storage))
(define mt-store empty)
(define override-store cons)

(define new-loc
  (let ([n (box 0)])
    (lambda ()
      (begin
        (set-box! n (+ 1 (unbox n)))
        (unbox n)))))

(define-type Result
  [v*s (val : Value) (sto : Store)])

(define (run input) (interp (parse input) mt-env mt-store))


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
    [(s-exp-match? '{box ANY} s)
     (boxC (parse (second (s-exp->list s))))]
    [(s-exp-match? '{unbox ANY} s)
     (unboxC (parse (second (s-exp->list s))))]
    [(s-exp-match? '{set-box! ANY ANY} s)
     (setboxC (parse (second (s-exp->list s)))
              (parse (third (s-exp->list s))))]
    [(s-exp-match? '{begin ANY ANY} s)
     (seqC (parse (second (s-exp->list s)))
           (parse (third (s-exp->list s))))]
    [(s-exp-match? '{lambda {SYMBOL} ANY} s)
     (lamC (s-exp->symbol (first (s-exp->list (second (s-exp->list s)))))
           (parse (third (s-exp->list s))))]
    [(s-exp-match? '{ANY ANY} s)
     (appC (parse (first (s-exp->list s)))
           (parse (second (s-exp->list s))))]
    [(s-exp-match? `SYMBOL s)
     (idC (s-exp->symbol s))]
    [else (error 'parse (string-append "invalid input" (s-exp->string s)))]))


;;;;; Interpreter ;;;;;

(define (interp (exp : ExprC) (env : Env) (sto : Store)) : Result
  (type-case ExprC exp
    [boolC (b) (v*s (boolV b) sto)]
    [numC (n) (v*s (numV n) sto)]
    [idC (s) (v*s (fetch (lookup s env) sto) sto)]
    [plusC (l r) (type-case Result (interp l env sto)
                   [v*s (lv s1) (type-case Result (interp r env s1)
                                  [v*s (rv s2) (v*s (num-binop + lv rv) s2)])])]
    [multC (l r) (type-case Result (interp l env sto)
                   [v*s (lv s1) (type-case Result (interp r env s1)
                                  [v*s (rv s2) (v*s (num-binop * lv rv) s2)])])]
    [zeroC (ne) (type-case Result (interp ne env sto)
                  [v*s (nv s1) (type-case Value nv
                                 [numV (n) (v*s (boolV (= 0 n)) s1)]
                                 [else (error 'interp "not a number")])])]
    [ifC (c t f)
         (type-case Result (interp c env sto)
           [v*s (cv sc)
                (cond
                  [(boolV? cv) (if (boolV-b cv) (interp t env sc) (interp f env sc))]
                  [else (error 'interp "can only test booleans")])])]
    [appC (fun arg)
          (type-case Result (interp fun env sto)
            [v*s (fun-val fun-sto)
                 (type-case Value fun-val
                   [closV (arg-name body clos-env)
                          (type-case Result (interp arg env fun-sto)
                            [v*s (arg-val arg-sto)
                                 (let* ([where (new-loc)]
                                        [body-env (extend-env
                                                   (bind arg-name where)
                                                   clos-env)]
                                        [body-sto (override-store
                                                   (cell where arg-val)
                                                   arg-sto)])
                                   (interp body body-env body-sto))])]
                   [else (error 'interp "not a function")])])]
    [lamC (arg body)
          (v*s (closV arg body env) sto)]
    [boxC (arg)
          (type-case Result (interp arg env sto)
            [v*s (arg-val arg-sto)
                 (let ([where (new-loc)])
                   (v*s (boxV where) (override-store (cell where arg-val) arg-sto)))])]
    [unboxC (arg)
            (type-case Result (interp arg env sto)
              [v*s (arg-val arg-sto)
                   (type-case Value arg-val
                     [boxV (box-val) (v*s (fetch box-val arg-sto) arg-sto)]
                     [else (error 'interp "not a box")])])]
    [setboxC (box value)
             (type-case Result (interp box env sto)
               [v*s (box-val box-sto)
                    (type-case Result (interp value env box-sto)
                      [v*s (value-val value-sto)
                           (type-case Value box-val
                             [boxV (where)
                                   (v*s value-val
                                        (override-store (cell where value-val)
                                                        value-sto))]
                             [else (error 'interp "not a box")])])])]
    [seqC (b1 b2)
          (type-case Result (interp b1 env sto)
            [v*s (b1-val b1-sto)
                 (interp b2 env b1-sto)])]))

(define (lookup (name : symbol) (env : Env)) : Location
  (cond
    [(empty? env) (error 'lookup "name not found")]
    [(symbol=? name (bind-name (first env))) (bind-loc (first env))]
    [else (lookup name (rest env))]))

(define (fetch (loc : Location) (sto : Store)) : Value
  (cond
    [(empty? sto) (error 'fetch "location not found")]
    [(equal? loc (cell-loc (first sto))) (cell-val (first sto))]
    [else (fetch loc (rest sto))]))

(define (num-binop (op : (number number -> number)) (l : Value) (r : Value)) : Value
  (cond
    [(and (numV? l) (numV? r))
     (numV (op (numV-n l) (numV-n r)))]
    [else (error 'num-binop "one argument not a number")]))