(set-logic QF_S)
 
(declare-fun x () String)
(declare-fun y () String)

(assert (= (str.++ x "a" x "b" y) (str.++ "b" y "b" y x)))

(check-sat)
(get-model)
