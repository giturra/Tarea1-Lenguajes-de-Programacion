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

;################################ Pregunta 1 ###############################

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
    [(emptEnv) (error 'env-lookup "free identifier: ~a" x)]
    [(tEnv id val env) (if (symbol=? id x) val (env-lookup x env))]))





;; typeof :: Expr -> Type
(define (typeof expr) #f)

(define (deBruijn expr)#f)

(define (compile expr) #f)



(define (typecheck s-expr) #f)

(define (typed-compile s-expr) #f)