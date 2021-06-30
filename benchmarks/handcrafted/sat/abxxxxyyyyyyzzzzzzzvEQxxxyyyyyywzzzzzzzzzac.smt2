(set-logic QF_S)
 
(declare-fun x () String)
(declare-fun y () String)
(declare-fun z () String)
(declare-fun v () String)
(declare-fun w () String)

(assert (= (str.++ "ab" x x x x y y y y y y z z z z z z z v) (str.++ x x x y y y y y y w z z z z z z z z z "ac")))

(check-sat)
(get-model)
