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

;################################ Pregunta 1 ################################

;; parse-type :: type -> Type
;; Convierte type en Type
(define (parse-type s-expr)
  (match s-expr
    [(== 'Num) (TNum)]
    [(list t1 -> t2) (TFun (parse-type t1) (parse-type t2))]
    [e (error "Parse error")]))

;; parse :: s-expr -> Expr
;; Convierte s-expr en Expr
(define (parse s-expr)
  (match s-expr
    [(? number?) (num s-expr)]
    [(? symbol?) (id s-expr)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
    [(list 'fun (list x : se) e) (fun 'x (parse-type se) (parse e) #f)]
    [(list 'fun (list x : se) : t e) (fun 'x (parse-type se) (parse e) (parse-type t))]
    [(list 'with (list x : t1 e1) e2) (app (fun 'x (parse-type t1) (parse e2) #f) (parse  e1))]
    [(list fun-id arg-expr) (app (parse fun-id) (parse arg-expr))]
   ))

;; prettify :: Type -> type
;; Convierte un Type en type, como una funcion inversa a parse-type
(define (prettify T-type)
  (match T-type
    [(TNum) 'Num]
    [(TFun arg ret) (list (prettify arg) '-> (prettify ret))]))


;################################ Pregunta 2 ###############################

(deftype TypeEnv
  (emptEnv)
  (tEnv id val env))

(define empty-env (emptEnv))

(define extend-env tEnv)

(define (env-lookup x env)
  (match env
    [(emptEnv) (error "Type error: free identifier:" x)]
    [(tEnv id val env) (if (symbol=? id x) val (env-lookup x env))]))


;; typeof-with-type-env 
(define (typeof-with-type-env expr type-env)
  (match expr
    [(num n) (TNum)]
    [(id s) (env-lookup s type-env)]
    [(add l r)
     (def tl (typeof-with-type-env l type-env))
     (def tr (typeof-with-type-env r type-env)) (TNum)]
    [(sub l r)
     (def tl (typeof-with-type-env l type-env))
     (def tr (typeof-with-type-env r type-env)) (TNum)]
    [(fun id targ body #f)
     (def body-value (typeof-with-type-env body  (extend-env id targ type-env)))
     (TFun targ body-value)]
    [(fun id targ body tbody)
     (def body-value (typeof-with-type-env body  (extend-env id targ type-env)))
     (TFun targ body-value)]
    [(app fun-id arg-expr) #f]
  ))

;; typeof :: Expr -> Type
(define (typeof expr)
  (typeof-with-type-env expr (emptEnv)))

(define (typecheck s-expr)
  (prettify (typeof(parse s-expr))))

(define (deBruijn expr)#f)

(define (compile expr) #f)





(define (typed-compile s-expr) #f)