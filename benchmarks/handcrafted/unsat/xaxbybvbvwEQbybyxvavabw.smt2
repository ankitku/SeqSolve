(set-logic QF_S)
 
(declare-fun x () String)
(declare-fun y () String)
(declare-fun v () String)
(declare-fun w () String)

(assert (= (str.++ x "a" x "b" y "b" v "b" v w) (str.++ "b" y "b" y x
v "a" v "ab" w)))

(check-sat)
(get-model)
