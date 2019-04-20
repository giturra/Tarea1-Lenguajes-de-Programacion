#lang play
(require "main.rkt")
(require "machine.rkt")


;; parse-type
(test (parse-type 'Num) (TNum))

;; Dario test

(test (parse-type 'Num) (TNum))
(test/exn (parse-type '->) "Parse error")
(test/exn (parse-type '{Num ->}) "Parse error")
(test (parse-type '{Num -> Num}) (TFun (TNum) (TNum)))
(test/exn (parse-type '{Num -> ->}) "Parse error")

;; end Dario Test

(test (parse-type '{Num -> Num}) (TFun (TNum) (TNum)))
(test/exn (parse-type '{ -> Num}) "Parse error")


;; parse
(test (parse 10) (num 10))
(test (parse 'x) (id 'x))
(test (parse '{+ 1 3}) (add (num 1) (num 3)))
(test (parse '{- 3 1}) (add (num 3) (num 1)))
(test (parse '{fun {x : Num} 20}) (fun 'x (TNum) (num 20) #f))
(test (parse '{fun {x : Num} : Num 20}) (fun 'x (TNum) (num 20) (TNum)))

(test (parse '{with {x : Num 5} {+ x 3}}) (app (fun 'x (TNum) (add (id 'x) (num 3)) #f) (num 5)))


;; Dario Test

(println "= Basic =")
(test (parse 2) (num 2))
(test (parse 'x) (id 'x))

(println "= Arithmetic =")
(test (parse '{+ 1 1}) (add (num 1) (num 1)))
(test (parse '{+ x 1}) (add (id 'x) (num 1)))
(test (parse '{+ x y}) (add (id 'x) (id 'y)))

(println "= Functions =")
(test (parse '{fun {x : Num} x}) (fun 'x (TNum) (id 'x) #f))
;(test (parse '{fun {x : Num} : Num x}) (fun 'x (TNum) (id 'x) (TNum)))
(test (parse '{fun {x : Num} {+ 1 x}}) (fun 'x (TNum) (add (num 1) (id 'x)) #f))
(test (parse '{fun {x : Num} : Num {+ 1 x}}) (fun 'x (TNum) (add (num 1) (id 'x)) (TNum)))
; really elaborated one
(test (parse '{fun {f : {Num -> Num}} {fun {g : Num} : {Num -> Num} {fun {h : Num} 4}}})
      (fun 'f (TFun (TNum) (TNum)) (fun 'g (TNum) (fun 'h (TNum) (num 4) #f) (TFun (TNum) (TNum))) #f) )

(println "= App =")
(test (parse '{{fun {x : Num} x} 2}) (app (fun 'x (TNum) (id 'x) #f) (num 2)))
(test (parse '{{fun {x : Num} : Num x} 2}) (app (fun 'x (TNum) (id 'x) (TNum)) (num 2)))
(test (parse '{{fun {x : {Num -> Num}} : Num {x 1}} {fun {y : Num} : Num y}})
      (app (fun 'x (TFun (TNum) (TNum)) (app (id 'x) (num 1)) (TNum))
           (fun 'y (TNum) (id 'y) (TNum))))

(println "= With =")
(test (parse '{with {x : Num 2} {+ 1 x}}) (app (fun 'x (TNum) (add (num 1) (id 'x)) #f) (num 2)))
(test (parse '{with {x : Num 2} {+ 1
              {with {y : Num x} {+ y x}}}})
      (app (fun 'x (TNum) (add (num 1)
           (app (fun 'y (TNum) (add (id 'y) (id 'x)) #f) (id 'x))) #f) (num 2)))
(test (parse '{with {x : Num 2} {+ 1
              {with {y : Num x} {
                {fun {z : Num} : Num {+ z x}} y                 
                                 }}}})
      (app (fun 'x (TNum) (add (num 1)
           (app (fun 'y (TNum) 
                (app (fun 'z (TNum) (add (id 'z) (id 'x)) (TNum)) (id 'y))                
                                 #f) (id 'x))) #f) (num 2)))


;; end Dario Test

;; Dario Test

;; PRETTIFY

(test (prettify (TNum)) 'Num)
(test (prettify (TFun (TNum) (TNum))) '{Num -> Num})
(test (prettify (TFun (TNum) (TFun (TNum) (TNum))))
                 '{Num -> {Num -> Num}})
(test (prettify (TFun (TFun (TNum) (TNum)) (TFun (TNum) (TNum))))
                 '{{Num -> Num} -> {Num -> Num}})

;; end Dario Test


;;typeof
(test (typeof (parse 10)) (TNum))
(test (typeof (parse '{+ 1 3})) (TNum))
(test (typeof (parse '{- 3 2})) (TNum))
(test (typeof (parse '{fun {x : Num} x})) (TFun (TNum) (TNum)))


;; Dario Test

;; TYPEOF
(println "========== Typeof Tests =========")
(test (typeof (parse '{with { y : {Num -> Num} {fun {x : Num} : Num {+ x x}}}
                            { fun {z : Num} : Num {+ z {y 2} }}}))
      (TFun (TNum) (TNum)))

(test (typeof (parse '{with {n : Num 5}
                  {with {f : {Num -> Num} {fun {x : Num} : Num {+ x n}}}
                        {with {n : Num 10}
                              {f n}}}})) (TNum))

(test (typeof (parse '{{fun {x : {Num -> Num}} : {Num -> Num}
                            {{fun {y : {Num -> Num}} : {Num -> Num} y} x}
                                 } {fun {x : Num} : Num x}})) (TFun (TNum) (TNum)))



;; end Dario Test

;typecheck
(test (typecheck '3) 'Num)



#|

;; deBruijn
(test (deBruijn (num 3)) (num 3))
(test (deBruijn (parse '{with {x : Num 5}  {with  {y : Num  {+ x 1}} {+ y x}}}))
      (app (fun-db (app (fun-db (add (acc 0) (acc 1))) (add (acc 0) (num 1)))) (num 5)))
(test/exn (deBruijn (parse 'x)) "Free identifier: x")

;;compile
(test (compile (add (num 2) (num 1))) (list  (INT-CONST 1) (INT-CONST 2) (ADD)))

|#
