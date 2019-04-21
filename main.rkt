#lang play
(require "machine.rkt")
(print-only-errors #t) 
;;;;;;;;;;;;;;;;;;;;;;;
;; Language definition
;;;;;;;;;;;;;;;;;;;;;;;

#|
<s-expr> ::= <num>
         | <id>
         | {+ <s-expr> <s-expr>}
         | {- <s-expr> <s-expr>}
         | {with {<s-expr> : <type> <s-expr>} <s-expr>}
         | {fun {<id>  : <s-expr>} [: <type>] <expr>}
         | {<expr> <expr>}         
 
<type> ::= Num
         | {<type> -> <type>}}
|#
(deftype Expr
  (num n)
  (add l r)
  (sub l r)
  (id s) 
  (fun id targ body tbody)
  (fun-db body)
  (acc n) ;Se usa para la pregunta 3
  (app fun-id arg-expr))

(deftype Type
  (TNum)
  (TFun arg ret))


;; ************************************************* Pregunta 1 *************************************************

;; parse-type: <type> -> Type
;; 
(define (parse-type s-expr)
  (match s-expr
    ['Num (TNum)]
    [(list type1 -> type2) (TFun (parse-type type1) (parse-type type2))]
    [e (error "¨Parse error")]))

(define (parse s-expr)
  (match s-expr
    [(? number?) (num s-expr)]
    [(? symbol?) (id s-expr)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (add (parse l) (parse r))]
    [(list 'fun (list iden : targ) body)
     (fun iden (parse-type targ) (parse body) #f)]
    [(list 'fun (list iden : targ) : tbody body)
     (fun iden (parse-type targ) (parse body) (parse-type tbody))]
    [(list 'with (list expr1 : type1 expr2) expr3)
     (app (fun expr1 (parse-type type1) (parse expr3) #f) (parse expr2))]
    [(list e1 e2) (app (parse e1) (parse e2))]
))

(define (prettify type)
  (match type
    [(TNum) 'Num]
    [(TFun arg ret) (list (prettify arg) '-> (prettify ret))]))

;; ************************************************* Pregunta 2 *************************************************

(deftype TypeEnv
  (emptyEnv)
  (tEnv id type env))

(define empty-env (emptyEnv))

(define extend-env tEnv)

(define (lookup-env x type-env)
  (match type-env
    [(emptyEnv) (error "Type error: free identifier:" x)]
    [(tEnv id type env) (if (symbol=? id x)
                            type
                            (lookup-env x env))]))

(define (typeof-with-type-env expr env)
  (match expr
    [(num n) (TNum)]
    [(id s) (lookup-env s env)]
    [(add l r) (def tl (typeof-with-type-env l env))
               (def tr (typeof-with-type-env r env))
               (if (not (TNum? tl)) (error
                                     (string-append "Type error in expression fun position 1: expected: Num found "
                                                  (~a (prettify tl))))
                                     (void))
               (if (not (TNum? tr)) (error
                                     (string-append "Type error in expression fun position 2: expected: Num found "
                                                  (~a (prettify tr))))
                                     (void))
               (TNum)
    ]
    [(sub l r) (def tl (typeof-with-type-env l env))
               (def tr (typeof-with-type-env r env))
               (if (not (TNum? tl)) (error
                                     (string-append "Type error in expression fun position 1: expected: Num found "
                                                  (~a (prettify tl))))
                                     (void))
               (if (not (TNum? tr)) (error
                                     (string-append "Type error in expression fun position 2: expected: Num found "
                                                  (~a (prettify tr))))
                                     (void))
               (TNum)
    ]
    [(fun id targ body #f) (def new-env (extend-env id targ env))
                           (def ctbody (typeof-with-type-env body new-env))
                           (TFun targ ctbody)
    ]
    [(fun id targ body tbody) (def new-env (extend-env id targ env))
                              (def ctbody (typeof-with-type-env body new-env))
                              (if (equal? ctbody tbody)
                                  (TFun targ tbody)
                                  (error
                                   (string-append "Type error in expression fun position 1: expected: "
                                                  (~a (prettify tbody)) " found "
                                                  (~a (prettify ctbody)))))
    ]
    [(app fun-id arg-expr) (def rt (typeof-with-type-env fun-id env))
                           (if (not (TFun? rt))
                               (error
                                   (string-append "Type error in expression app position 1: expected (T -> S) found "
                                                  (~a (prettify rt))))
                               (void))
                           (def (TFun art ret) rt)
                           (def bt (typeof-with-type-env arg-expr env))
                           (if (not (equal? art bt))
                                   (error
                                   (string-append "Type error in expression fun position 1: expected: "
                                                  (~a (prettify art)) " found "
                                                  (~a (prettify bt))))
                               (void))
                           ret
    ]       
))

(define (typecheck s-expr)
  (prettify (typeof (parse s-expr))))

(define (typeof expr)
  (typeof-with-type-env expr (emptyEnv)))


;; ************************************************* Pregunta 3 *************************************************

(deftype IndexEnv
  (mtEnv)
  (iEnv id env))

(define empty-index-env (mtEnv))

(define extend-index-env iEnv)

(define (lookup-index-env x val env)
  (match env
    [(mtEnv) (error "Free identifier:" x)]
    [(iEnv id next)
     (if (equal? id x)
         (acc val)
         (lookup-index-env x (add1 val) next))]))


(define (deBruijn-with-index-env expr env)
  (match expr
    [(num n) (num n)]
    [(id s) (lookup-index-env s 0 env)]
    [(add l r) (add (deBruijn-with-index-env l env)
                    (deBruijn-with-index-env r env))]
    [(sub l r) (add (deBruijn-with-index-env l env)
                    (deBruijn-with-index-env r env))]
    [(fun id targ body tbody) (def new-env (extend-index-env id env))
                              (fun-db (deBruijn-with-index-env body new-env))
    ]
    [(app fun-id arg-expr) (app (deBruijn-with-index-env fun-id env)
                                (deBruijn-with-index-env arg-expr env))
    ]))


(define (deBruijn expr)
  (deBruijn-with-index-env expr (mtEnv)))

(define (compile expr)
  (match expr
    [(num n) (cons (INT-CONST n) '())]))

(define (typed-compile s-expr) #f)