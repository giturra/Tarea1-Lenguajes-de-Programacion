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

;################################ Pregunta 1 ###########################

;; parse-type :: type -> Type
;; Convierte type en Type
(define (parse-type s-expr)
  (match s-expr
    [(== 'Num) (TNum)]
    [(list t1 -> ) (error "Parse error")]
    [(list -> t1) (error "Parse error")]
    [(list t1 -> t2) (TFun (parse-type t1) (parse-type t2))]))

;; parse :: s-expr -> Expr
;; Convierte s-expr en Expr
(define (parse s-expr)
  (match s-expr
    [(? number?) (num s-expr)]
    [(? symbol?) (id s-expr)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]))

(define (deBruijn expr)#f)

(define (compile expr) #f)

(define (typeof expr) #f)

(define (typecheck s-expr) #f)

(define (typed-compile s-expr) #f)