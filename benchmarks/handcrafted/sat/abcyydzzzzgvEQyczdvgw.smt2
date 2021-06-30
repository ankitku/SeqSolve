(set-logic QF_S)
 
(declare-fun x () String)
(declare-fun y () String)
(declare-fun z () String)
(declare-fun v () String)
(declare-fun w () String)

(assert (= (str.++ "abc" y y "d" z z z z "g" v) (str.++ y "c" z "d" v "g" w)))

(check-sat)
(get-model)
