#lang plai-typed
(require plai-typed/s-exp-match)

(define-type ArithC
  [boolC (b : boolean)]
  [numC (n : number)]
  [plusC (l : ArithC) (r : ArithC)]
  [multC (l : ArithC) (r : ArithC)]
  [zeroC (n : ArithC)]
  [ifC (c : ArithC) (t : ArithC) (f : ArithC)])

(define-type Value
  [numV (n : number)]
  [boolV (b : boolean)])


;;;;; Parser ;;;;;

(define (parse (s : s-expression)) : ArithC
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
    [else (error 'parse "invalid input")]))


;;;;; Interpreter ;;;;;

(define (interp (exp : ArithC)) : Value
  (type-case ArithC exp
    [boolC (b) (boolV b)]
    [numC (n) (numV n)]
    [plusC (l r) (num-binop + (interp l) (interp r))]
    [multC (l r) (num-binop * (interp l) (interp r))]
    [zeroC (n) (let ([nv (interp n)])
                 (if (numV? nv)
                     (boolV (= 0 (numV-n nv)))
                     (error 'interp "not a number")))]
    [ifC (c t f)
         (let ([ci (interp c)])
           (cond
             [(boolV? ci) (if (boolV-b ci) (interp t) (interp f))]
             [else (error 'interp "can only test booleans")]))]))

(define (num-binop (op : (number number -> number)) (l : Value) (r : Value)) : Value
  (cond
    [(and (numV? l) (numV? r))
     (numV (op (numV-n l) (numV-n r)))]
    [else (error 'num-binop "one argument not a number")]))