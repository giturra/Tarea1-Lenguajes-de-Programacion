#lang play
(require "main.rkt")
(require "machine.rkt")


;; parse-type
(test (parse-type 'Num) (TNum))
(test/exn (parse-type '{Num ->}) "Parse error")

(test (parse-type '{Num -> Num}) (TFun (TNum) (TNum)))
(test/exn (parse-type '{ -> Num}) "Parse error")


;; parse
(test (parse '{- 3 1}) (sub (num 3) (num 1)))

(test (parse '{+ 1 3}) (add (num 1) (num 3)))
(test (parse '{with {x : Num 5} {+ x 3}}) (app (fun 'x (TNum) (add (id 'x) (num 3)) #f) (num 5)))


;;typeof
(test (typeof (parse '{+ 1 3})) (TNum))


;typecheck
(test (typecheck '3) 'Num)
(test (typecheck  '{fun {f : Num} : Num 10}) '(Num -> Num))


#| Test que venian que aun no funcionan


;; deBruijn
(test (deBruijn (num 3)) (num 3))
(test (deBruijn (parse '{with {x : Num 5}  {with  {y : Num  {+ x 1}} {+ y x}}}))
      (app (fun-db (app (fun-db (add (acc 0) (acc 1))) (add (acc 0) (num 1)))) (num 5)))
(test/exn (deBruijn (parse 'x)) "Free identifier: x")

;;compile
(test (compile (add (num 2) (num 1))) (list  (INT-CONST 1) (INT-CONST 2) (ADD)))


;;typeof
(test (typeof (parse '{+ 1 3})) (TNum))


|#

